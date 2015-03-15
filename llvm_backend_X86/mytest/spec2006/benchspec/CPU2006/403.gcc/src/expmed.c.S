	.file	"expmed.c.bc"
	.text
	.globl	init_expmed
	.align	16, 0x90
	.type	init_expmed,@function
init_expmed:                            # @init_expmed
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
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
	movl	word_mode(%rip), %edi
	movq	$10000, %rsi            # imm = 0x2710
	callq	gen_rtx_REG
	callq	start_sequence
	movl	word_mode(%rip), %edi
	movq	$10000, %rsi            # imm = 0x2710
	callq	gen_rtx_REG
	movq	const_int_rtx+512(%rip), %rdi
	movq	%rax, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	rtx_cost
	movl	word_mode(%rip), %esi
	movl	%eax, zero_cost(%rip)
	movq	$75, %rdi
	movq	%rbx, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	const_int_rtx+512(%rip), %rcx
	movl	word_mode(%rip), %esi
	movl	%eax, add_cost(%rip)
	movq	$87, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+512(%rip), %rcx
	movl	word_mode(%rip), %r12d
	movq	%rax, %r15
	movq	$78, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+512(%rip), %rcx
	movl	word_mode(%rip), %r13d
	movq	%rax, %r12
	movq	$78, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ee
	movq	$76, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	movq	%rax, %r13
	callq	init_recog
	movl	add_cost(%rip), %eax
	movl	%r14d, shift_cost(%rip)
	movl	%eax, shiftsub_cost(%rip)
	movl	%eax, shiftadd_cost(%rip)
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32000, %rax            # imm = 0x7D00
	movl	%eax, shiftsub_cost(,%rbx,4)
	movl	%eax, shiftadd_cost(,%rbx,4)
	movl	%eax, shift_cost(,%rbx,4)
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	32(%r15), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 16(%rcx)
	movq	32(%r15), %rdi
	movq	%r15, %rsi
	leaq	12(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%r15), %rax
	movq	16(%rax), %rdi
	movq	$47, %rsi
	callq	rtx_cost
	movl	%eax, shift_cost(,%rbx,4)
.LBB0_3:                                # %if.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	movq	32(%r12), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 16(%rcx)
	movq	32(%r12), %rdi
	movq	%r12, %rsi
	leaq	12(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_5
# BB#4:                                 # %if.then62
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%r12), %rax
	movq	16(%rax), %rdi
	movq	$47, %rsi
	callq	rtx_cost
	movl	%eax, shiftadd_cost(,%rbx,4)
.LBB0_5:                                # %if.end72
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	movq	32(%r13), %rcx
	movq	16(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 16(%rcx)
	movq	32(%r13), %rdi
	movq	%r13, %rsi
	leaq	12(%rsp), %rdx
	callq	recog
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_7
# BB#6:                                 # %if.then94
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	32(%r13), %rax
	movq	16(%rax), %rdi
	movq	$47, %rsi
	callq	rtx_cost
	movl	%eax, shiftsub_cost(,%rbx,4)
.LBB0_7:                                # %for.inc
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%rbx
	cmpq	$64, %rbx
	jne	.LBB0_1
# BB#8:                                 # %for.end
	movl	word_mode(%rip), %esi
	movq	$77, %rdi
	movq	(%rsp), %r12            # 8-byte Reload
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movl	word_mode(%rip), %ebx
	movl	%eax, negate_cost(%rip)
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$79, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	add_cost(%rip), %edx
	sarq	%cl, %rax
	movq	$33, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movabsq	$0, %rbx
	movabsq	$1, %r15
	movq	%rbx, %rax
	jg	.LBB0_10
# BB#9:                                 # %for.end
	movq	%r15, %rax
.LBB0_10:                               # %for.end
	movl	word_mode(%rip), %r14d
	movl	%eax, sdiv_pow2_cheap(%rip)
	xorq	%rdi, %rdi
	movq	$32, %rsi
	callq	gen_rtx_CONST_INT
	movq	$80, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	add_cost(%rip), %edx
	sarq	%cl, %rax
	movq	$33, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB0_12
# BB#11:                                # %for.end
	movq	%r15, %rbx
.LBB0_12:                               # %for.end
	movl	class_narrowest_mode+4(%rip), %r14d
	movl	%ebx, smod_pow2_cheap(%rip)
	testq	%r14, %r14
	jne	.LBB0_13
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_14:                               # %for.inc156
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	$121, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$121, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movl	%eax, mul_widen_cost(,%r13,4)
	movq	$121, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r12
	movq	$121, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	$78, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	mode_bitsize(%rbx,%rbx), %esi
	movq	%rax, %r15
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$90, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$122, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movl	%eax, mul_highpart_cost(,%rbx,4)
	movq	%r13, %r14
.LBB0_13:                               # %for.body121
                                        # =>This Inner Loop Header: Depth=1
	movq	$10000, %rsi            # imm = 0x2710
	movq	%r14, %rdi
	callq	gen_rtx_REG
	movq	%rax, %r15
	movq	$81, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	%eax, div_cost(,%rbx,4)
	movq	$78, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movzbl	mode_wider_mode(%rbx), %r13d
	movl	%eax, mul_cost(,%rbx,4)
	testq	%r13, %r13
	jne	.LBB0_14
.LBB0_15:                               # %for.end160
	callq	end_sequence
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	init_expmed, .Ltmp17-init_expmed
	.cfi_endproc

	.globl	negate_rtx
	.align	16, 0x90
	.type	negate_rtx,@function
negate_rtx:                             # @negate_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp23:
	.cfi_def_cfa_offset 32
.Ltmp24:
	.cfi_offset %rbx, -24
.Ltmp25:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$77, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	simplify_unary_operation
	testq	%rax, %rax
	je	.LBB1_2
# BB#1:                                 # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB1_2:                                # %if.then
	movq	optab_table+256(%rip), %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_unop  # TAILCALL
.Ltmp26:
	.size	negate_rtx, .Ltmp26-negate_rtx
	.cfi_endproc

	.globl	mode_for_extraction
	.align	16, 0x90
	.type	mode_for_extraction,@function
mode_for_extraction:                    # @mode_for_extraction
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 16
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	je	.LBB2_1
# BB#2:                                 # %entry
	andq	%rcx, %rdi
	cmpq	$2, %rdi
	jne	.LBB2_3
# BB#6:                                 # %sw.bb2
	movabsq	$insn_data+44520, %rdx
	jmp	.LBB2_7
.LBB2_1:
	movabsq	$insn_data+44560, %rdx
	jmp	.LBB2_7
.LBB2_3:                                # %entry
	cmpq	$1, %rdi
	jne	.LBB2_5
# BB#4:                                 # %sw.bb1
	movabsq	$insn_data+44480, %rdx
.LBB2_7:                                # %sw.epilog
	movq	%rsi, %rdi
	andq	%rcx, %rdi
	xorq	%rax, %rax
	cmpq	%rcx, %rdi
	je	.LBB2_10
# BB#8:                                 # %if.end
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	24(%rdx), %rax
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %rcx
	movzbl	16(%rax,%rcx,8), %eax
	testq	%rax, %rax
	jne	.LBB2_10
# BB#9:                                 # %if.then4
	movl	word_mode(%rip), %eax
.LBB2_10:                               # %return
	popq	%rdx
	retq
.LBB2_5:                                # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.mode_for_extraction, %rcx
	movq	$259, %rsi              # imm = 0x103
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp29:
	.size	mode_for_extraction, .Ltmp29-mode_for_extraction
	.cfi_endproc

	.globl	store_bit_field
	.align	16, 0x90
	.type	store_bit_field,@function
store_bit_field:                        # @store_bit_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp38:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp41:
	.cfi_def_cfa_offset 112
.Ltmp42:
	.cfi_offset %rbx, -48
.Ltmp43:
	.cfi_offset %r12, -40
.Ltmp44:
	.cfi_offset %r13, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %r10d
	movq	%rcx, %r11
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	$66, %r10
	movq	$8, %rcx
	je	.LBB3_2
# BB#1:                                 # %cond.false
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
.LBB3_2:                                # %cond.end
	movq	insn_data+44584(%rip), %rdi
	xorq	%rdx, %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	divq	%rcx
	movq	%rax, %r15
	movzbl	88(%rdi), %eax
	testq	%rax, %rax
	je	.LBB3_3
# BB#4:                                 # %cond.end
	movq	%r11, 32(%rsp)          # 8-byte Spill
	jmp	.LBB3_5
.LBB3_3:
	movq	%r11, 32(%rsp)          # 8-byte Spill
	movl	word_mode(%rip), %eax
.LBB3_5:                                # %cond.end
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%r9, %r9
	js	.LBB3_6
# BB#7:                                 # %if.then
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	$3, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movabsq	$2305843009213693936, %rcx # imm = 0x1FFFFFFFFFFFFFF0
	andq	%rax, %rcx
	subq	%rcx, %r9
	jmp	.LBB3_8
.LBB3_6:
	movq	%rsi, 8(%rsp)           # 8-byte Spill
.LBB3_8:                                # %while.cond.preheader
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%rdx, %r14
	cmpq	$63, %r10
	jne	.LBB3_11
# BB#9:                                 # %while.body.lr.ph
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	.align	16, 0x90
.LBB3_10:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	8(%rbx), %rbx
	movzbl	(%rbx), %edx
	addq	%rax, %r15
	cmpq	$63, %rdx
	je	.LBB3_10
.LBB3_11:                               # %while.end
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r8, %rdi
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB3_12
# BB#13:                                # %if.then19
	movl	generating_concat_p(%rip), %r13d
	movl	%r12d, generating_concat_p(%rip)
	movq	%rax, %rdi
	callq	force_not_mem
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movl	%r13d, generating_concat_p(%rip)
	jmp	.LBB3_14
.LBB3_12:
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB3_14:                               # %if.end21
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %r8
	shrq	%cl, %r8
	addq	$32, %r8
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %r8
	movabsq	$34359738367, %rcx      # imm = 0x7FFFFFFFF
	addq	%rcx, %r8
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %r8
	movq	$3, %rcx
	shrq	%cl, %r8
	movq	$23, %rcx
	shrq	%cl, %rax
	movq	%r14, %r12
	testq	%r12, %r12
	jne	.LBB3_32
# BB#15:                                # %land.lhs.true
	movq	$32, %rcx
	movq	32(%rsp), %r14          # 8-byte Reload
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movzbl	mode_bitsize(%r14,%r14), %ecx
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB3_32
# BB#16:                                # %land.lhs.true41
	movl	(%rbx), %edi
	addq	$4, %rax
	movq	%rax, %rcx
	imulq	%r15, %rcx
	addq	%rcx, %r8
	movq	%rdi, %r9
	andq	$65535, %r9             # imm = 0xFFFF
	cmpq	$66, %r9
	je	.LBB3_20
# BB#17:                                # %cond.true46
	movzbl	mode_size(%r14), %r11d
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rax
	cmpq	%rax, %r11
	jae	.LBB3_19
# BB#18:                                # %lor.lhs.false
	movq	$16, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%r11, %rax
	jne	.LBB3_32
.LBB3_19:                               # %land.lhs.true65
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%r11
	testq	%r10, %rdx
	je	.LBB3_20
.LBB3_32:                               # %if.end143
	movzbl	2(%rbx), %edi
	callq	int_mode_for_mode
	movl	(%rbx), %edx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%r8, %rsi
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rsi
	je	.LBB3_38
# BB#33:                                # %if.then153
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	jne	.LBB3_35
# BB#34:                                # %if.then158
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	jmp	.LBB3_37
.LBB3_35:                               # %if.else160
	movq	%rax, %rcx
	andq	%r8, %rcx
	cmpq	$51, %rcx
	je	.LBB3_132
# BB#36:                                # %if.then163
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	gen_lowpart
.LBB3_37:                               # %if.end168
	movq	%rax, %rbx
.LBB3_38:                               # %if.end168
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB3_40
# BB#39:                                # %if.then173
	movq	%rbx, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_alias_set
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	set_mem_expr
	movl	(%rbx), %eax
.LBB3_40:                               # %if.end175
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	je	.LBB3_58
# BB#41:                                # %if.end175
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB3_58
# BB#42:                                # %land.lhs.true189
	movq	$32, %rcx
	movq	32(%rsp), %r13          # 8-byte Reload
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movzbl	mode_bitsize(%r13,%r13), %ecx
	cmpq	%r14, %rcx
	jne	.LBB3_58
# BB#43:                                # %land.lhs.true195
	movq	optab_table+248(%rip), %rsi
	movq	$4, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movslq	8(%rsi,%rdi), %r12
	cmpq	$1317, %r12             # imm = 0x525
	jne	.LBB3_44
.LBB3_58:                               # %if.end291
	movl	target_flags(%rip), %r13d
	movq	%r13, %rsi
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rsi, %r12
	shrq	%cl, %r12
	leaq	32(%r12), %rdi
	cmpq	%r14, %rdi
	jae	.LBB3_68
# BB#59:                                # %if.then298
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	orq	$31, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r15
	andq	%r15, %r12
	addq	%r14, %r12
	movq	%r14, %rbx
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rdi
	movq	%rax, %r14
	imulq	%r14, %rdi
	movq	$1, %rsi
	callq	smallest_mode_for_size
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%r15, %r14
	movq	%r15, %rsi
	je	.LBB3_60
# BB#61:                                # %cond.false347.lr.ph
	movq	$20, %rcx
	shrq	%cl, %r13
	andq	$32, %r13
	addq	$32, %r13
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %r13
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%r13
	xorq	%r12, %r12
	andq	%rsi, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_62:                               # %cond.false347
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %r14d
	movq	$20, %rcx
	shrq	%cl, %r14
	andq	$32, %r14
	addq	$32, %r14
	movq	%r14, %r15
	imulq	%r12, %r15
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %r15
	movq	%rbx, %rax
	subq	%r15, %rax
	cmpq	%rax, %r14
	jb	.LBB3_64
# BB#63:                                # %cond.false347
                                        #   in Loop: Header=BB3_62 Depth=1
	movq	%rax, %r14
.LBB3_64:                               # %cond.false347
                                        #   in Loop: Header=BB3_62 Depth=1
	movq	48(%rsp), %rdi          # 8-byte Reload
	movzbl	2(%rdi), %eax
	movq	56(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %r15
	testq	%rax, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB3_66
# BB#65:                                # %cond.false347
                                        #   in Loop: Header=BB3_62 Depth=1
	movq	%rax, %rdx
.LBB3_66:                               # %cond.false347
                                        #   in Loop: Header=BB3_62 Depth=1
	movl	word_mode(%rip), %r13d
	movq	%r12, %rsi
	callq	operand_subword_force
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	%rax, %r8
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	store_bit_field
	incq	%r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB3_62
# BB#67:
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_131
.LBB3_68:                               # %if.end396
	cmpq	$66, %rdx
	movq	24(%rsp), %r12          # 8-byte Reload
	jne	.LBB3_69
# BB#77:                                # %if.else463
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, %rbx
	jmp	.LBB3_78
.LBB3_69:                               # %if.then401
	testq	%r15, %r15
	jne	.LBB3_72
# BB#70:                                # %lor.lhs.false404
	movq	$16, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %edi
	movabsq	$4286578688, %rcx       # imm = 0xFF800000
	andq	%rcx, %rsi
	movq	$23, %rcx
	shrq	%cl, %rsi
	addq	$4, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	%rsi, %rdi
	jbe	.LBB3_71
.LBB3_72:                               # %if.then416
	cmpq	$61, %rdx
	je	.LBB3_76
# BB#73:                                # %if.then416
	cmpq	$63, %rdx
	jne	.LBB3_75
# BB#74:                                # %land.lhs.true426
	movq	8(%rbx), %rbx
	movq	$16, %rcx
	movzbl	2(%rbx), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB3_75
.LBB3_76:                               # %if.end450
	movq	$20, %rcx
	shrq	%cl, %r13
	andq	$32, %r13
	addq	$32, %r13
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	mode_for_size
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rsi, %rdx
	imulq	%r15, %rdx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rbx
	xorq	%r15, %r15
	jmp	.LBB3_78
.LBB3_60:
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_131
.LBB3_20:                               # %if.then96
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	andq	%r10, %rdx
	cmpq	%rdx, %rdi
	je	.LBB3_31
# BB#21:                                # %if.then102
	cmpq	$63, %r9
	jne	.LBB3_22
# BB#25:                                # %if.then107
	movq	8(%rbx), %rbx
	movl	(%rbx), %eax
	movq	$16, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdx, %rdi
	je	.LBB3_23
# BB#26:                                # %lor.lhs.false116
	movl	mode_class(,%r14,4), %ecx
	orq	$2, %rcx
	andq	%r10, %rcx
	cmpq	$3, %rcx
	je	.LBB3_23
# BB#27:                                # %if.else
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_bit_field, %rcx
	movq	$367, %rsi              # imm = 0x16F
	jmp	.LBB3_28
.LBB3_44:                               # %if.then200
	movq	48(%rsp), %rsi          # 8-byte Reload
	movzbl	(%rsi), %ecx
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB3_49
# BB#45:                                # %if.then200
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24592, %rdx            # imm = 0x6010
	je	.LBB3_47
# BB#46:
	movq	32(%rsp), %r14          # 8-byte Reload
	jmp	.LBB3_51
.LBB3_71:
	xorq	%r15, %r15
.LBB3_78:                               # %if.end465
	movq	48(%rsp), %rsi          # 8-byte Reload
	movl	(%rsi), %edi
	movq	$14, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	andq	$1020, %rdx             # imm = 0x3FC
	movl	mode_class(%rdx), %ecx
	orq	$2, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	jne	.LBB3_80
# BB#79:
	movq	%rsi, %r13
	jmp	.LBB3_81
.LBB3_80:                               # %if.then481
	movl	word_mode(%rip), %edi
	callq	gen_lowpart
	movl	(%rax), %edi
	movq	%rax, %r13
.LBB3_81:                               # %if.end483
	movq	%r14, %rdx
	movq	%rdi, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB3_129
# BB#82:                                # %land.lhs.true489
	cmpq	$1, %rdx
	jne	.LBB3_84
# BB#83:                                # %land.lhs.true489
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$54, %rdi
	je	.LBB3_129
.LBB3_84:                               # %land.lhs.true497
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	%rdx, %rax
	jb	.LBB3_129
# BB#85:                                # %land.lhs.true503
	movl	(%rbx), %ecx
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB3_87
# BB#86:                                # %land.lhs.true513
	leaq	(%r12,%rdx), %rcx
	cmpq	%rax, %rcx
	ja	.LBB3_129
.LBB3_87:                               # %if.then520
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	callq	get_last_insn
	movq	insn_data+44584(%rip), %rcx
	movzbl	88(%rcx), %ecx
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB3_88
# BB#89:                                # %if.then520
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	jmp	.LBB3_90
.LBB3_22:                               # %if.then102.if.end131_crit_edge
	movl	(%rbx), %eax
.LBB3_23:                               # %if.end131
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB3_29
# BB#24:                                # %if.then136
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	movq	%r8, %rdx
	callq	gen_rtx_SUBREG
	jmp	.LBB3_30
.LBB3_29:                               # %if.else138
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	adjust_address_1
.LBB3_30:                               # %if.end141
	movq	%rax, %rbx
.LBB3_31:                               # %if.end141
	movq	%rbx, %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB3_131
.LBB3_47:                               # %if.then200
	shlq	%cl, %rax
	testq	$129, %rax
	je	.LBB3_49
# BB#48:                                # %if.then214
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	gen_lowpart
	jmp	.LBB3_50
.LBB3_49:                               # %if.then231
	xorq	%rdx, %rdx
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	convert_to_mode
.LBB3_50:                               # %if.end234
	movq	%rax, %rsi
.LBB3_51:                               # %if.end234
	leaq	(%r12,%r12,4), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rax
	movq	%rsi, %rdi
	movq	%rsi, %r12
	movq	%r14, %rsi
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_53
# BB#52:                                # %if.then240
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
.LBB3_53:                               # %if.end242
	movzbl	(%rbx), %eax
	cmpq	$63, %rax
	jne	.LBB3_57
# BB#54:                                # %if.then247
	movq	8(%rbx), %rbx
	movzbl	2(%rbx), %eax
	movq	%r14, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB3_57
# BB#55:                                # %lor.lhs.false256
	movl	mode_class(,%r13,4), %eax
	orq	$2, %rax
	andq	%rdx, %rax
	cmpq	$3, %rax
	jne	.LBB3_56
.LBB3_57:                               # %if.end272
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	$32, %rsi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rsi
	movabsq	$34359738367, %rcx      # imm = 0x7FFFFFFFF
	addq	%rcx, %rsi
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$23, %rcx
	shrq	%cl, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rax, %rdx
	imulq	%r15, %rdx
	addq	%rsi, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r12, %rbx
	movq	%rbx, %rsi
	movq	48(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB3_131
.LBB3_88:
	movl	word_mode(%rip), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB3_90:                               # %if.then520
	movl	volatile_ok(%rip), %r14d
	movq	$1, %rax
	movl	%eax, volatile_ok(%rip)
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB3_99
# BB#91:                                # %land.lhs.true528
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	insn_data+44584(%rip), %rax
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB3_92
.LBB3_99:                               # %if.end595
	movl	%r14d, volatile_ok(%rip)
	movl	(%rbx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	%rbx, %rax
	jne	.LBB3_101
# BB#100:                               # %if.then600
	movl	byte_mode(%rip), %esi
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	adjust_address_1
	movl	(%rax), %edx
.LBB3_101:                              # %if.end602
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r14, %r12
	jne	.LBB3_103
# BB#102:                               # %if.then607
	movq	8(%rax), %rsi
	movl	16(%rax), %edx
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	gen_rtx_SUBREG
	movl	(%rax), %edx
.LBB3_103:                              # %if.end615
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB3_104
# BB#105:                               # %land.lhs.true620
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	16(%rsp), %r8           # 8-byte Reload
	movq	%r8, %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rdx
	je	.LBB3_107
# BB#106:                               # %if.then626
	xorq	%rdx, %rdx
	movq	%r8, %rdi
	movq	%rax, %rsi
	movq	%r8, %r14
	callq	gen_rtx_SUBREG
	movq	%r14, %r8
	jmp	.LBB3_107
.LBB3_104:
	movq	16(%rsp), %r8           # 8-byte Reload
.LBB3_107:                              # %if.end628
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movl	(%r13), %eax
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r8, %rdi
	andq	%r12, %rdi
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rdi, %rdx
	je	.LBB3_113
# BB#108:                               # %if.then637
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	movq	8(%rsp), %rdi           # 8-byte Reload
	cmpq	%rdi, %rcx
	jae	.LBB3_109
# BB#119:                               # %if.else677
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	movq	48(%rsp), %rdx          # 8-byte Reload
	jne	.LBB3_121
# BB#120:                               # %if.then682
	movq	8(%rdx), %rdi
	movq	%r8, %rsi
	movq	%r8, %r14
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%r14, %r8
	jmp	.LBB3_112
.LBB3_109:                              # %if.then646
	movzbl	mode_size(%rdx), %eax
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rax
	jae	.LBB3_117
# BB#110:                               # %if.then658
	xorq	%rcx, %rcx
	movq	%r8, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r14
	callq	simplify_subreg
	movq	%r14, %r8
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB3_113
# BB#111:                               # %if.then664
	movq	48(%rsp), %r14          # 8-byte Reload
	movzbl	2(%r14), %edi
	movq	%r14, %rsi
	movq	%r8, %r13
	callq	force_reg
	movzbl	2(%r14), %edx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	simplify_gen_subreg
	movq	%r13, %r8
	jmp	.LBB3_112
.LBB3_92:                               # %if.then533
	movl	(%rbx), %r8d
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$51, %rsi
	je	.LBB3_94
# BB#93:                                # %lor.lhs.false539
	movzbl	mode_size(%rsi), %eax
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB3_97
.LBB3_94:                               # %if.then551
	movq	16(%rbx), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB3_96
# BB#95:                                # %cond.true559
	movl	32(%rax), %edx
.LBB3_96:                               # %cond.end565
	movq	$27, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	get_best_mode
	movq	%rax, %rsi
.LBB3_97:                               # %if.end575
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB3_129
# BB#98:                                # %if.end579
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movzbl	mode_size(%rax), %r9d
	xorq	%rdx, %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	divq	%rcx
	movq	%rdx, %r14
	imulq	%rax, %r9
	movq	$1, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%r9, %rdx
	callq	adjust_address_1
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	copy_to_reg
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r14, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%r13, %rbx
	movq	%rbx, %r8
	movq	40(%rsp), %r9           # 8-byte Reload
	callq	store_bit_field
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	jmp	.LBB3_131
.LBB3_129:                              # %insv_loses
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	movq	%r13, %rbx
.LBB3_130:                              # %return
	movq	%rbx, %r8
	callq	store_fixed_bit_field
.LBB3_131:                              # %return
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_121:                              # %if.else687
	leaq	-67(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	movq	%rdx, %r13
	jb	.LBB3_113
# BB#122:                               # %lor.lhs.false697
	cmpq	$133, %rax
	jg	.LBB3_125
# BB#123:                               # %lor.lhs.false697
	cmpq	$58, %rax
	ja	.LBB3_127
# BB#124:                               # %lor.lhs.false697
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$414331165718085632, %rax # imm = 0x5C0000000000000
	testq	%rax, %rdx
	movq	48(%rsp), %r13          # 8-byte Reload
	jne	.LBB3_113
	jmp	.LBB3_127
.LBB3_117:                              # %if.else674
	movq	%r8, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r14
	callq	gen_lowpart
	movq	%r14, %r8
.LBB3_112:                              # %if.end731
	movq	%rax, %r13
.LBB3_113:                              # %if.end731
	movq	%r13, %rdi
	movq	%r8, %rsi
	movq	insn_data+44584(%rip), %rax
	movq	%r8, %r14
	callq	*72(%rax)
	testq	%r12, %rax
	jne	.LBB3_115
# BB#114:                               # %if.then736
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, %r13
.LBB3_115:                              # %if.end738
	movq	8(%rsp), %rsi           # 8-byte Reload
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	$32, %rcx
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	gen_insv
	testq	%rax, %rax
	je	.LBB3_128
# BB#116:                               # %if.then744
	movq	%rax, %rdi
	callq	emit_insn
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_131
.LBB3_128:                              # %if.else746
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	delete_insns_since
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r12, %rcx
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB3_130
.LBB3_125:                              # %lor.lhs.false697
	cmpq	$134, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	je	.LBB3_113
# BB#126:                               # %lor.lhs.false697
	cmpq	$140, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	je	.LBB3_113
.LBB3_127:                              # %if.then727
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_bit_field, %rcx
	movq	$665, %rsi              # imm = 0x299
	jmp	.LBB3_28
.LBB3_132:                              # %if.else165
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_bit_field, %rcx
	movq	$391, %rsi              # imm = 0x187
	jmp	.LBB3_28
.LBB3_75:                               # %if.else448
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_bit_field, %rcx
	movq	$522, %rsi              # imm = 0x20A
	jmp	.LBB3_28
.LBB3_56:                               # %if.else270
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_bit_field, %rcx
	movq	$444, %rsi              # imm = 0x1BC
.LBB3_28:                               # %if.else
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp47:
	.size	store_bit_field, .Ltmp47-store_bit_field
	.cfi_endproc

	.align	16, 0x90
	.type	store_fixed_bit_field,@function
store_fixed_bit_field:                  # @store_fixed_bit_field
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
	subq	$48, %rsp
.Ltmp59:
	.cfi_def_cfa_offset 96
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
	movq	%rdi, %r9
	movl	(%r9), %ebx
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rbx, %rax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB4_7
# BB#1:                                 # %if.then
	testq	%r14, %r14
	jne	.LBB4_2
# BB#4:                                 # %if.end
	movl	target_flags(%rip), %eax
	leaq	(%r13,%r12), %rdx
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jbe	.LBB4_18
# BB#5:                                 # %if.then11
	movq	%r9, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r8, %rcx
	jmp	.LBB4_6
.LBB4_7:                                # %if.else
	movq	$16, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_bitsize(%rax,%rax), %edx
	movslq	word_mode(%rip), %rcx
	testq	%rdx, %rdx
	je	.LBB4_9
# BB#8:                                 # %lor.lhs.false18
	movzbl	mode_bitsize(%rcx,%rcx), %esi
	cmpq	%rsi, %rdx
	jbe	.LBB4_10
.LBB4_9:                                # %if.then27
	movq	%rcx, %rax
.LBB4_10:                               # %if.end28
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	16(%r9), %rcx
	movq	%r9, 24(%rsp)           # 8-byte Spill
	leaq	(%r13,%r14,8), %r15
	testq	%rcx, %rcx
	movq	$8, %rdx
	je	.LBB4_12
# BB#11:                                # %cond.true
	movl	32(%rcx), %edx
.LBB4_12:                               # %cond.end
	movq	$27, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	get_best_mode
	movq	%rax, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB4_13
# BB#14:                                # %if.end47
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %ebx
	cmpq	%r13, %rbx
	jbe	.LBB4_16
# BB#15:                                # %if.end47.if.end66_crit_edge
	movq	$3, %rcx
	shrq	%cl, %rbx
	jmp	.LBB4_17
.LBB4_13:                               # %if.then44
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
.LBB4_6:                                # %if.then11
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	store_split_bit_field  # TAILCALL
.LBB4_16:                               # %if.then54
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	imulq	%rbx, %rax
	addq	%rax, %r14
	movq	$3, %rcx
	shlq	%cl, %rax
	subq	%rax, %r13
.LBB4_17:                               # %if.end66
	movq	24(%rsp), %rdi          # 8-byte Reload
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rbx
	leaq	(%r13,%rdx,8), %r13
	subq	%rdx, %r14
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r14, %rdx
	callq	adjust_address_1
	movq	%rax, %r9
	movl	(%r9), %ebx
	movq	32(%rsp), %r8           # 8-byte Reload
.LBB4_18:                               # %if.end76
	movl	(%r8), %eax
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$54, %rdx
	jne	.LBB4_47
# BB#19:                                # %if.then84
	movq	8(%r8), %rax
	cmpq	$63, %r12
	movq	%rax, %rdx
	ja	.LBB4_21
# BB#20:                                # %if.then89
	movq	$1, %rdx
	movq	%r12, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	andq	%rax, %rdx
.LBB4_21:                               # %if.end92
	testq	%rdx, %rdx
	je	.LBB4_22
# BB#23:                                # %if.else96
	cmpq	$63, %r12
	movq	%r12, %rdi
	ja	.LBB4_25
# BB#24:                                # %land.lhs.true
	movq	$1, %rsi
	movq	%rdi, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	cmpq	%rsi, %rdx
	je	.LBB4_28
.LBB4_25:                               # %lor.lhs.false103
	xorq	%r8, %r8
	cmpq	$64, %rdi
	jne	.LBB4_26
# BB#27:                                # %lor.lhs.false103
	cmpq	$-1, %rdx
	movabsq	$0, %r12
	jne	.LBB4_29
.LBB4_28:                               # %if.then109
	movq	$1, %r12
	xorq	%r8, %r8
	jmp	.LBB4_29
.LBB4_47:                               # %if.else115
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	xorq	%r15, %r15
	cmpq	%r12, %rcx
	je	.LBB4_51
# BB#48:                                # %land.rhs
	movzbl	mode_bitsize(%rbx,%rbx), %ecx
	leaq	(%r13,%r12), %rsi
	cmpq	%rcx, %rsi
	je	.LBB4_49
# BB#50:                                # %land.rhs
	movabsq	$1, %r15
	jmp	.LBB4_51
.LBB4_22:
	movq	%r12, %rdi
	movq	$1, %r8
	xorq	%r12, %r12
	jmp	.LBB4_29
.LBB4_49:
	movabsq	$0, %r15
.LBB4_51:                               # %land.end
	cmpq	%rbx, %rax
	jne	.LBB4_53
# BB#52:
	movq	%rbx, %r14
	jmp	.LBB4_58
.LBB4_53:                               # %if.then135
	movq	%r9, 24(%rsp)           # 8-byte Spill
	orq	$2, %rdx
	cmpq	$63, %rdx
	jne	.LBB4_56
# BB#54:                                # %land.lhs.true145
	movzbl	mode_size(%rbx), %ecx
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rcx
	jae	.LBB4_56
# BB#55:                                # %if.then157
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%r8, %rsi
	callq	gen_lowpart
	jmp	.LBB4_57
.LBB4_56:                               # %if.else159
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%r8, %rsi
	callq	convert_to_mode
.LBB4_57:                               # %if.end162
	movq	%rax, %r8
	movq	24(%rsp), %r9           # 8-byte Reload
.LBB4_58:                               # %if.end162
	testq	%r15, %r15
	je	.LBB4_59
# BB#60:                                # %if.then164
	movq	optab_table+144(%rip), %rdx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$63, %rax
	jg	.LBB4_62
# BB#61:                                # %if.end5.i314.thread
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	$64, %rax
	subq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdi
	shrq	%cl, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	jmp	.LBB4_65
.LBB4_59:
	movq	%r9, 24(%rsp)           # 8-byte Spill
	jmp	.LBB4_66
.LBB4_62:                               # %if.end5.i314
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$65, %rax
	movq	%r14, %rdx
	jl	.LBB4_63
# BB#64:                                # %if.then15.i322
	movq	$128, %rax
	subq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdi
	movq	$-1, %rsi
	shrq	%cl, %rsi
	jmp	.LBB4_65
.LBB4_63:
	movq	$-1, %rdi
	xorq	%rsi, %rsi
.LBB4_65:                               # %mask_rtx.exit325
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%rdx, %rbx
	callq	immed_double_const
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movq	%rax, %r8
.LBB4_66:                               # %if.end168
	testq	%r13, %r13
	movq	%r13, %rdi
	je	.LBB4_67
# BB#68:                                # %if.then171
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB4_69
# BB#70:
	movabsq	$0, %r12
	.align	16, 0x90
.LBB4_71:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	movabsq	$0, %rax
	movq	%r14, %rdi
	je	.LBB4_75
# BB#72:                                # %for.body.i
                                        #   in Loop: Header=BB4_71 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB4_74
# BB#73:                                # %if.then13.i
                                        #   in Loop: Header=BB4_71 Depth=1
	movq	$2, %rax
	jmp	.LBB4_75
	.align	16, 0x90
.LBB4_74:                               # %if.else14.i
                                        #   in Loop: Header=BB4_71 Depth=1
	movq	$3, %rax
.LBB4_75:                               # %if.else79.i
                                        #   in Loop: Header=BB4_71 Depth=1
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rax, (%rsp)
	movq	%rdi, %r14
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB4_39
# BB#76:                                # %land.lhs.true94.i
                                        #   in Loop: Header=BB4_71 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB4_71
# BB#77:                                # %for.end.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	jmp	.LBB4_3
.LBB4_67:
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r8, %r15
	xorq	%r12, %r12
	jmp	.LBB4_39
.LBB4_69:
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	xorq	%r12, %r12
	jmp	.LBB4_39
.LBB4_26:
	xorq	%r12, %r12
.LBB4_29:                               # %if.end111
	movq	$32, %rcx
	movq	%rdi, %rsi
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$63, %rdx
	jg	.LBB4_30
# BB#31:                                # %if.then.i331
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	$-1, %rdx
	movq	%rsi, %rcx
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rax
	jmp	.LBB4_32
.LBB4_30:
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
.LBB4_32:                               # %if.end.i333
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$63, %rdx
	jg	.LBB4_36
# BB#33:                                # %if.then2.i334
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, %rdi
	movq	%r13, %rcx
	shlq	%cl, %rdi
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	jle	.LBB4_34
# BB#35:                                # %cond.true.i
	movq	$64, %rdx
	subq	%r13, %rdx
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	shrq	%cl, %rax
	jmp	.LBB4_37
.LBB4_36:                               # %if.else.i
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	addq	%r13, %rcx
	movq	%r13, 8(%rsp)           # 8-byte Spill
	shlq	%cl, %rax
	xorq	%rdi, %rdi
.LBB4_37:                               # %lshift_value.exit
	movq	%rax, %rsi
	jmp	.LBB4_38
.LBB4_34:
	movq	%r13, 8(%rsp)           # 8-byte Spill
.LBB4_38:                               # %lshift_value.exit
	movq	%rbx, %rdx
	movq	%rbx, %r14
	callq	immed_double_const
	movq	%rax, %r15
.LBB4_39:                               # %if.end175
	movq	24(%rsp), %r13          # 8-byte Reload
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	movabsq	$0, %rdi
	movabsq	$1, %rdx
	movq	%rdi, %rcx
	je	.LBB4_41
# BB#40:                                # %if.end175
	movq	%rdx, %rcx
.LBB4_41:                               # %if.end175
	movl	flag_force_mem(%rip), %esi
	testq	%rsi, %rsi
	je	.LBB4_43
# BB#42:                                # %if.end175
	movq	%rdx, %rdi
.LBB4_43:                               # %if.end175
	xorq	%rax, %rax
	testq	%rdi, %rcx
	jne	.LBB4_45
# BB#44:                                # %if.end175
	movq	%r13, %rax
.LBB4_45:                               # %if.end175
	testq	%r12, %r12
	je	.LBB4_78
# BB#46:
	movq	%r13, %rcx
	jmp	.LBB4_87
.LBB4_78:                               # %if.then187
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	optab_table+144(%rip), %r12
	movq	$32, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	cmpq	$63, %rax
	movq	%r14, %rdx
	jg	.LBB4_80
# BB#79:                                # %if.then.i
	movq	$-1, %rdi
	movq	%rsi, %rcx
	shlq	%cl, %rdi
.LBB4_80:                               # %if.end.i
	movq	16(%rsp), %r9           # 8-byte Reload
	leaq	(%rsi,%r9), %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$63, %rax
	jg	.LBB4_82
# BB#81:                                # %if.then2.i
	movq	$64, %rax
	subq	%r9, %rax
	subq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rax
	shrq	%cl, %rax
	andq	%rax, %rdi
.LBB4_82:                               # %if.end5.i
	movq	$32, %rcx
	movq	%rsi, %rax
	movq	%rsi, %r8
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$65, %rax
	movq	$-1, %rsi
	movq	$-1, %rbx
	jl	.LBB4_84
# BB#83:                                # %if.else8.i
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	addq	%r8, %rcx
	movq	$-1, %rbx
	shlq	%cl, %rbx
.LBB4_84:                               # %if.end12.i
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	cmpq	$65, %r10
	jl	.LBB4_86
# BB#85:                                # %if.then15.i
	movq	$128, %rax
	subq	%r9, %rax
	subq	%r8, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rax
	shrq	%cl, %rax
	andq	%rax, %rbx
	notq	%rbx
	movq	%rbx, %rsi
.LBB4_86:                               # %mask_rtx.exit
	notq	%rdi
	movq	%rdx, %rbx
	callq	immed_double_const
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	movq	32(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	movq	%rax, %rcx
.LBB4_87:                               # %if.end193
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB4_89
# BB#88:                                # %if.then195
	movq	optab_table+152(%rip), %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%rcx, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	expand_binop
	movq	%rax, %rcx
.LBB4_89:                               # %if.end197
	cmpq	%rcx, %r13
	je	.LBB4_90
# BB#91:                                # %if.then200
	movq	%r13, %rdi
	movq	%rcx, %rsi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.LBB4_90:                               # %if.end202
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_2:                                # %if.then5
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.store_fixed_bit_field, %rcx
	movq	$723, %rsi              # imm = 0x2D3
.LBB4_3:                                # %if.then5
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp65:
	.size	store_fixed_bit_field, .Ltmp65-store_fixed_bit_field
	.cfi_endproc

	.globl	extract_bit_field
	.align	16, 0x90
	.type	extract_bit_field,@function
extract_bit_field:                      # @extract_bit_field
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
	subq	$144, %rsp
.Ltmp77:
	.cfi_def_cfa_offset 192
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
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movq	%r9, %r12
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	$8, %rcx
	je	.LBB5_2
# BB#1:                                 # %cond.false
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
.LBB5_2:                                # %cond.end
	movq	insn_data+44504(%rip), %rsi
	xorq	%rdx, %rdx
	movq	128(%rsp), %rax         # 8-byte Reload
	divq	%rcx
	movq	%rax, %r13
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movl	word_mode(%rip), %ecx
	movzbl	16(%rsi), %eax
	testq	%rax, %rax
	movq	%rcx, %rdx
	je	.LBB5_4
# BB#3:                                 # %cond.end
	movq	%rax, %rdx
.LBB5_4:                                # %cond.end
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r8, 136(%rsp)          # 8-byte Spill
	movq	insn_data+44544(%rip), %rax
	movzbl	16(%rax), %eax
	movq	200(%rsp), %rsi
	testq	%rax, %rax
	je	.LBB5_6
# BB#5:                                 # %cond.end
	movq	%rax, %rcx
.LBB5_6:                                # %cond.end
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	192(%rsp), %rax
	testq	%rsi, %rsi
	js	.LBB5_8
# BB#7:                                 # %if.then
	movq	$3, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	shrq	%cl, %rdx
	movabsq	$2305843009213693936, %rcx # imm = 0x1FFFFFFFFFFFFFF0
	andq	%rdx, %rcx
	subq	%rcx, %rsi
.LBB5_8:                                # %if.end
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r12, 96(%rsp)          # 8-byte Spill
	je	.LBB5_10
# BB#9:                                 # %if.end
	movq	%rax, 96(%rsp)          # 8-byte Spill
.LBB5_10:                               # %if.end
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%rbx, %r15
	jmp	.LBB5_11
	.align	16, 0x90
.LBB5_13:                               # %while.body
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	16(%r15), %eax
	xorq	%rdx, %rdx
	divq	%rsi
	movq	8(%r15), %r15
	movl	(%r15), %edi
	addq	%rax, %r13
.LBB5_11:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	je	.LBB5_14
# BB#12:                                # %while.cond
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$63, %rax
	je	.LBB5_13
# BB#18:                                # %if.end69.loopexit
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	jmp	.LBB5_19
.LBB5_14:                               # %land.lhs.true
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rdi
	jne	.LBB5_15
# BB#16:                                # %land.lhs.true
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	cmpq	$0, 128(%rsp)           # 8-byte Folded Reload
	jne	.LBB5_19
# BB#17:                                # %land.lhs.true59
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	%r14, %rax
	movq	%r12, %rdi
	jne	.LBB5_19
	jmp	.LBB5_86
.LBB5_15:
	movq	%rbx, 56(%rsp)          # 8-byte Spill
.LBB5_19:                               # %if.end69
	callq	int_mode_for_mode
	movl	(%r15), %edx
	movq	%rax, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rsi
	je	.LBB5_25
# BB#20:                                # %if.then79
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	jne	.LBB5_22
# BB#21:                                # %if.then84
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
	jmp	.LBB5_24
.LBB5_22:                               # %if.else
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$51, %rcx
	je	.LBB5_43
# BB#23:                                # %if.then88
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	gen_lowpart
.LBB5_24:                               # %if.end93
	movq	%rax, %r15
.LBB5_25:                               # %if.end93
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB5_27
# BB#26:                                # %if.then98
	movq	%r15, %rdi
	callq	shallow_copy_rtx
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	set_mem_alias_set
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	set_mem_expr
.LBB5_27:                               # %if.end100
	movq	%r14, %rdi
	movl	target_flags(%rip), %ebx
	andq	$33554432, %rbx         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	movabsq	$34359738367, %r14      # imm = 0x7FFFFFFFF
	addq	%rax, %r14
	movq	128(%rsp), %rax         # 8-byte Reload
	andq	%rax, %r14
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	$23, %rcx
	shrq	%cl, %rbx
	addq	$4, %rbx
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movl	mode_class(,%rax,4), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rbx
	imulq	%r13, %rbx
	leaq	-7(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	movq	%r12, 120(%rsp)         # 8-byte Spill
	jb	.LBB5_29
# BB#28:                                # %cond.false123
	xorq	%rdx, %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rdi, %r12
	callq	mode_for_size
	movq	%r12, %rdi
	movq	%rax, %r12
.LBB5_29:                               # %cond.end128
	movl	(%r15), %esi
	addq	%rbx, %r14
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	je	.LBB5_32
# BB#30:                                # %land.lhs.true134
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB5_52
# BB#31:                                # %land.lhs.true140
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB5_52
.LBB5_32:                               # %land.lhs.true152
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	cmpq	%rdi, %rax
	ja	.LBB5_35
# BB#33:                                # %land.lhs.true159
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rdi, %rcx
	jne	.LBB5_35
# BB#34:                                # %land.lhs.true165
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	addq	%rcx, %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB5_38
.LBB5_35:                               # %lor.lhs.false173
	xorq	%rdx, %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rdi, %rbx
	callq	mode_for_size
	movq	%rbx, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$51, %rax
	je	.LBB5_52
# BB#36:                                # %lor.lhs.false173
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB5_52
# BB#37:                                # %lor.lhs.false173.if.then192_crit_edge
	movl	(%r15), %esi
.LBB5_38:                               # %if.then192
	movq	$16, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r12, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	%rax, %rbx
	je	.LBB5_49
# BB#39:                                # %if.then198
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB5_45
# BB#40:                                # %if.then203
	movq	8(%r15), %r15
	movl	(%r15), %esi
	movq	$16, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	%rbx, %rax
	je	.LBB5_45
# BB#41:                                # %lor.lhs.false212
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	orq	$2, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB5_42
.LBB5_45:                               # %if.end228
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB5_47
# BB#46:                                # %if.then233
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_SUBREG
	jmp	.LBB5_48
.LBB5_52:                               # %if.end244
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	leaq	32(%rax), %rcx
	movabsq	$4294967264, %rdx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	cmpq	%rdi, %rcx
	jae	.LBB5_87
# BB#53:                                # %if.then251
	orq	$31, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	addq	%rdi, %rax
	movq	%rdi, 104(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %r14
	movq	136(%rsp), %rdx         # 8-byte Reload
	testq	%rdx, %rdx
	movq	120(%rsp), %rdi         # 8-byte Reload
	je	.LBB5_55
# BB#54:                                # %lor.lhs.false266
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	je	.LBB5_56
.LBB5_55:                               # %if.then271
	callq	gen_reg_rtx
	movq	%rax, %rdx
.LBB5_56:                               # %if.end273
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	$49, %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB5_64
	.align	16, 0x90
.LBB5_57:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %ebx
	movq	$20, %rcx
	shrq	%cl, %rbx
	andq	$32, %rbx
	addq	$32, %rbx
	imulq	%r13, %rbx
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	operand_subword
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	%rax, %r12
	movq	%rsi, %rcx
	imulq	%r13, %rcx
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	subq	%rcx, %rax
	cmpq	%rax, %rsi
	jb	.LBB5_59
# BB#58:                                # %for.body
                                        #   in Loop: Header=BB5_57 Depth=1
	movq	%rax, %rsi
.LBB5_59:                               # %for.body
                                        #   in Loop: Header=BB5_57 Depth=1
	movl	word_mode(%rip), %eax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rbx
	movq	128(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rbx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rsp)
	movq	$1, %rcx
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	%r12, %r8
	movq	120(%rsp), %r9          # 8-byte Reload
	callq	extract_bit_field
	testq	%r12, %r12
	je	.LBB5_60
# BB#61:                                # %if.end315
                                        #   in Loop: Header=BB5_57 Depth=1
	cmpq	%r12, %rax
	je	.LBB5_63
# BB#62:                                # %if.then318
                                        #   in Loop: Header=BB5_57 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB5_63:                               # %for.inc
                                        #   in Loop: Header=BB5_57 Depth=1
	movq	%r14, %rax
	incq	%r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r13, %rcx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_57
.LBB5_64:                               # %for.end
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB5_68
# BB#65:                                # %if.then322
	movl	target_flags(%rip), %esi
	movq	136(%rsp), %r15         # 8-byte Reload
	movzbl	2(%r15), %eax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	%rsi, %rcx
	imulq	%r14, %rcx
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB5_86
# BB#66:                                # %if.then335
	movq	%r14, %rcx
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %rbx
	andq	%r13, %rcx
	andq	%r13, %rbx
	cmpq	%rbx, %rcx
	jae	.LBB5_86
	.align	16, 0x90
.LBB5_67:                               # %for.body350
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	operand_subword
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	incq	%r14
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	%rbx, %rax
	jne	.LBB5_67
	jmp	.LBB5_86
.LBB5_87:                               # %if.end370
	movq	%rdi, %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	int_mode_for_mode
	movq	%rax, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$51, %rcx
	movq	120(%rsp), %rdi         # 8-byte Reload
	jne	.LBB5_90
# BB#88:                                # %if.end376
	callq	int_mode_for_mode
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$51, %rcx
	je	.LBB5_89
.LBB5_90:                               # %if.end380
	movl	(%r15), %edx
	movq	%rdx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$66, %rdi
	jne	.LBB5_91
# BB#97:                                # %if.else420
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r15
	movq	%r14, %rbx
	jmp	.LBB5_98
.LBB5_91:                               # %if.then385
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%r13, %r13
	movq	%r14, %rbx
	jne	.LBB5_94
# BB#92:                                # %lor.lhs.false388
	movq	$16, %rcx
	shrq	%cl, %rdx
	movl	target_flags(%rip), %esi
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rax, %rsi
	cmpq	%rsi, %rdx
	jbe	.LBB5_93
.LBB5_94:                               # %if.then400
	cmpq	$61, %rdi
	je	.LBB5_96
# BB#95:                                # %if.then405
	movq	%r15, %rdi
	callq	copy_to_reg
	movq	%rax, %r15
.LBB5_96:                               # %if.end407
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rdx
	imulq	%r13, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %r15
	xorq	%r13, %r13
	jmp	.LBB5_98
.LBB5_68:                               # %if.end357
	movq	$32, %rcx
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %r14d
	movq	104(%rsp), %rax         # 8-byte Reload
	subq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	je	.LBB5_69
	.align	16, 0x90
.LBB5_74:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB5_78
# BB#75:                                # %for.body.i
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB5_77
# BB#76:                                # %if.then13.i
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	$2, %rax
	jmp	.LBB5_78
	.align	16, 0x90
.LBB5_77:                               # %if.else14.i
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	$3, %rax
.LBB5_78:                               # %if.else79.i
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB5_80
# BB#79:                                # %if.else79.i
                                        #   in Loop: Header=BB5_74 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB5_74
.LBB5_80:                               # %for.end.i
	testq	%rax, %rax
	movq	%rax, %r15
	jne	.LBB5_70
	jmp	.LBB5_81
.LBB5_47:                               # %if.else235
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	adjust_address_1
.LBB5_48:                               # %if.end238
	movq	%rax, %r15
.LBB5_49:                               # %if.end238
	movq	120(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	je	.LBB5_86
# BB#50:                                # %if.then241
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	jmp	.LBB5_51
.LBB5_69:
	movq	136(%rsp), %r15         # 8-byte Reload
.LBB5_70:                               # %expand_shift.exit
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB5_86
	.align	16, 0x90
.LBB5_71:                               # %for.body.i1179
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB5_83
# BB#72:                                # %for.body.i1179
                                        #   in Loop: Header=BB5_71 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB5_82
# BB#73:                                # %if.then13.i1180
                                        #   in Loop: Header=BB5_71 Depth=1
	movq	$2, %rax
	jmp	.LBB5_83
	.align	16, 0x90
.LBB5_82:                               # %if.else14.i1181
                                        #   in Loop: Header=BB5_71 Depth=1
	movq	$3, %rax
.LBB5_83:                               # %if.else79.i1183
                                        #   in Loop: Header=BB5_71 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB5_85
# BB#84:                                # %if.else79.i1183
                                        #   in Loop: Header=BB5_71 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB5_71
.LBB5_85:                               # %for.end.i1190
	testq	%rax, %rax
	movq	%rax, %r15
	jne	.LBB5_86
.LBB5_81:                               # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	jmp	.LBB5_44
.LBB5_93:
	xorq	%r13, %r13
.LBB5_98:                               # %if.end422
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB5_145
# BB#99:                                # %if.then424
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	%rbx, %rax
	jb	.LBB5_144
# BB#100:                               # %land.lhs.true430
	movl	(%r15), %ecx
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB5_102
# BB#101:                               # %land.lhs.true440
	movq	88(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rbx), %rcx
	cmpq	%rax, %rcx
	ja	.LBB5_144
.LBB5_102:                              # %if.then447
	callq	get_last_insn
	movq	insn_data+44544(%rip), %rcx
	movzbl	16(%rcx), %edi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	jne	.LBB5_104
# BB#103:
	movl	word_mode(%rip), %edi
.LBB5_104:                              # %if.then447
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r12
	jne	.LBB5_115
# BB#105:                               # %if.then454
	movq	%rdi, %r14
	movq	$1, %rax
	movl	%eax, volatile_ok(%rip)
	movzbl	2(%r15), %esi
	movq	%r15, %rdi
	movq	insn_data+44544(%rip), %rax
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_106
# BB#113:                               # %if.else519
	movl	byte_mode(%rip), %esi
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r13, %rdx
	callq	adjust_address_1
	movq	%rax, %r12
	movq	88(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB5_114
.LBB5_145:                              # %if.else613
	movq	$32, %rcx
	movq	48(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	%rbx, %rax
	jb	.LBB5_163
# BB#146:                               # %land.lhs.true619
	movl	(%r15), %ecx
	orq	$2, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB5_148
# BB#147:                               # %land.lhs.true629
	movq	88(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rbx), %rcx
	cmpq	%rax, %rcx
	ja	.LBB5_163
.LBB5_148:                              # %if.then636
	callq	get_last_insn
	movq	insn_data+44504(%rip), %rcx
	movzbl	16(%rcx), %r12d
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	jne	.LBB5_150
# BB#149:
	movl	word_mode(%rip), %r12d
.LBB5_150:                              # %if.then636
	movl	(%r15), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%r15, %r14
	jne	.LBB5_160
# BB#151:                               # %if.then656
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdi
	movq	insn_data+44504(%rip), %rax
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_152
# BB#159:                               # %if.else728
	movq	$32, %rcx
	movq	%r13, %rdx
	movl	byte_mode(%rip), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	callq	adjust_address_1
	movq	%rax, %r14
	movq	88(%rsp), %rax          # 8-byte Reload
	jmp	.LBB5_160
.LBB5_106:                              # %if.then461
	movl	(%r15), %r8d
	movq	$16, %rcx
	movq	%r8, %r12
	shrq	%cl, %r12
	andq	$255, %r12
	cmpq	$51, %r12
	je	.LBB5_108
# BB#107:                               # %lor.lhs.false467
	movzbl	mode_size(%r12), %eax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB5_111
.LBB5_108:                              # %if.then479
	movq	16(%r15), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB5_110
# BB#109:                               # %cond.true486
	movl	32(%rax), %edx
.LBB5_110:                              # %cond.end491
	movq	$27, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	movq	%rbx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rcx
	callq	get_best_mode
	movq	%rax, %r12
.LBB5_111:                              # %if.end501
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB5_144
# BB#112:                               # %if.end505
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movzbl	mode_size(%rax), %r9d
	xorq	%rdx, %rdx
	movq	128(%rsp), %rax         # 8-byte Reload
	divq	%rcx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	imulq	%rax, %r9
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r9, %rdx
	callq	adjust_address_1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, %r12
.LBB5_114:                              # %if.end521
	movl	volatile_ok(%rip), %eax
	movl	%eax, volatile_ok(%rip)
	movl	(%r12), %eax
	movq	%r14, %rdi
.LBB5_115:                              # %if.end522
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB5_116
# BB#118:                               # %land.lhs.true539
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rdi, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_120
# BB#119:
	movq	%rdi, %r14
	jmp	.LBB5_121
.LBB5_116:                              # %if.end522
	cmpq	$63, %rcx
	jne	.LBB5_117
# BB#143:                               # %land.lhs.true527
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rdi, %rcx
	movq	%rdi, %r14
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_121
.LBB5_144:                              # %extzv_loses
	movq	$1, (%rsp)
	jmp	.LBB5_164
.LBB5_120:                              # %if.then545
	xorq	%rdx, %rdx
	movq	%rdi, %r14
	movq	%r12, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %r12
	jmp	.LBB5_121
.LBB5_117:
	movq	%rdi, %r14
.LBB5_121:                              # %if.end547
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB5_124
# BB#122:                               # %lor.lhs.false553
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	movq	136(%rsp), %rax         # 8-byte Reload
	je	.LBB5_125
# BB#123:                               # %land.lhs.true555
	movq	136(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	movq	%rcx, %rax
	jne	.LBB5_125
.LBB5_124:                              # %if.then560
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
.LBB5_125:                              # %if.end562
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movl	(%rax), %edi
	movq	$16, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%r14, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB5_127
# BB#126:
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	movq	%rax, %r12
	movq	%rax, 48(%rsp)          # 8-byte Spill
	jmp	.LBB5_132
.LBB5_127:                              # %if.then568
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB5_131
# BB#128:                               # %if.then573
	movq	%r14, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movzbl	mode_size(%rbx), %r14d
	movzbl	mode_size(%rdx), %ebx
	movq	%rax, %rsi
	movq	%rax, 48(%rsp)          # 8-byte Spill
	callq	gen_lowpart
	movq	%rax, %r12
	cmpq	%rbx, %r14
                                        # kill: RAX<def> R12<kill>
	ja	.LBB5_130
# BB#129:                               # %select.mid
	xorq	%rax, %rax
.LBB5_130:                              # %select.end
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	24(%rsp), %r14          # 8-byte Reload
	jmp	.LBB5_132
.LBB5_131:                              # %if.else589
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB5_132:                              # %if.end592
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	insn_data+44544(%rip), %rax
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_134
# BB#133:                               # %if.then597
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB5_134:                              # %if.end599
	xorq	%rdi, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	%rax, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_extzv
	testq	%rax, %rax
	je	.LBB5_141
# BB#135:                               # %if.then605
	movq	%rax, %rdi
	callq	emit_insn
	movq	48(%rsp), %r15          # 8-byte Reload
	jmp	.LBB5_136
.LBB5_141:                              # %if.else607
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	$1, (%rsp)
	jmp	.LBB5_142
.LBB5_152:                              # %if.then664
	movl	(%r15), %r8d
	movq	$16, %rcx
	movq	%r8, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	cmpq	$51, %r14
	je	.LBB5_154
# BB#153:                               # %lor.lhs.false671
	movzbl	mode_size(%r14), %eax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB5_157
.LBB5_154:                              # %if.then683
	movq	16(%r15), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB5_156
# BB#155:                               # %cond.true691
	movl	32(%rax), %edx
.LBB5_156:                              # %cond.end697
	movq	$27, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	movq	%rbx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rcx
	callq	get_best_mode
	movq	%rax, %r14
.LBB5_157:                              # %if.end707
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB5_163
# BB#158:                               # %if.end711
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movzbl	mode_size(%rax), %r9d
	xorq	%rdx, %rdx
	movq	128(%rsp), %rax         # 8-byte Reload
	divq	%rcx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	imulq	%rax, %r9
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r9, %rdx
	callq	adjust_address_1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%rax, %r14
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB5_160:                              # %if.end732
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB5_161
# BB#166:                               # %land.lhs.true749
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_168
# BB#167:                               # %if.then755
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_SUBREG
	movq	%rax, %r14
	jmp	.LBB5_168
.LBB5_161:                              # %if.end732
	cmpq	$63, %rcx
	jne	.LBB5_168
# BB#162:                               # %land.lhs.true737
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_163
.LBB5_168:                              # %if.end757
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB5_171
# BB#169:                               # %lor.lhs.false763
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	movq	136(%rsp), %rax         # 8-byte Reload
	je	.LBB5_172
# BB#170:                               # %land.lhs.true765
	movq	136(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	movq	%rcx, %rax
	jne	.LBB5_172
.LBB5_171:                              # %if.then770
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
.LBB5_172:                              # %if.end772
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movl	(%rax), %edx
	movq	$16, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r12, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jne	.LBB5_174
# BB#173:
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	movq	%r12, %r14
	movq	%rax, %r12
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB5_179
.LBB5_163:                              # %extv_loses
	movq	$0, (%rsp)
.LBB5_164:                              # %if.end825
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
.LBB5_165:                              # %if.end825
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	136(%rsp), %r15         # 8-byte Reload
	movq	%r15, %r9
	callq	extract_fixed_bit_field
	movq	%rax, %r12
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB5_137
.LBB5_174:                              # %if.then778
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB5_178
# BB#175:                               # %if.then783
	movzbl	mode_size(%rbx), %ecx
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movzbl	mode_size(%rdi), %ebx
	movq	%r12, %rdi
	movq	%r12, %r14
	movq	%rax, %rsi
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	gen_lowpart
	movq	%rax, %r12
	cmpq	%rbx, 128(%rsp)         # 8-byte Folded Reload
                                        # kill: RAX<def> R12<kill>
	ja	.LBB5_177
# BB#176:                               # %select.mid10
	xorq	%rax, %rax
.LBB5_177:                              # %select.end9
	movq	%rax, 128(%rsp)         # 8-byte Spill
	jmp	.LBB5_179
.LBB5_178:                              # %if.else800
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdi
	movq	%r12, %r14
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB5_179:                              # %if.end803
	movq	%r12, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	movq	insn_data+44504(%rip), %rax
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_181
# BB#180:                               # %if.then808
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB5_181:                              # %if.end810
	xorq	%rdi, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	%rax, %r14
	movq	$32, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	%rax, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	gen_extv
	testq	%rax, %rax
	je	.LBB5_183
# BB#182:                               # %if.then817
	movq	%rax, %rdi
	callq	emit_insn
	movq	32(%rsp), %r15          # 8-byte Reload
.LBB5_136:                              # %if.end825
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %rax         # 8-byte Reload
.LBB5_137:                              # %if.end825
	cmpq	%r15, %r12
	je	.LBB5_86
# BB#138:                               # %if.end825
	cmpq	%rax, %r12
	je	.LBB5_86
# BB#139:                               # %if.end833
	movzbl	2(%r12), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_140
# BB#184:                               # %if.end833
	movq	120(%rsp), %rcx         # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB5_185
# BB#186:                               # %if.then845
	movq	80(%rsp), %rax          # 8-byte Reload
	orq	$2, %rax
	andq	%rdx, %rax
	cmpq	$3, %rax
	jne	.LBB5_188
# BB#187:                               # %if.else862
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
.LBB5_51:                               # %if.then241
	movq	72(%rsp), %rdx          # 8-byte Reload
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_to_mode  # TAILCALL
.LBB5_140:
	movq	%r12, %r15
	jmp	.LBB5_86
.LBB5_185:
	movq	%r12, %r15
.LBB5_86:                               # %return
	movq	%r15, %rax
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_188:                              # %if.then855
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	mode_bitsize(%rax,%rax), %edi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	convert_to_mode
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB5_183:                              # %if.else819
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	$0, (%rsp)
.LBB5_142:                              # %if.end825
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB5_165
.LBB5_43:                               # %if.else90
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_bit_field, %rcx
	movq	$1077, %rsi             # imm = 0x435
	jmp	.LBB5_44
.LBB5_42:                               # %if.else226
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_bit_field, %rcx
	movq	$1147, %rsi             # imm = 0x47B
	jmp	.LBB5_44
.LBB5_60:                               # %if.then314
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_bit_field, %rcx
	movq	$1199, %rsi             # imm = 0x4AF
	jmp	.LBB5_44
.LBB5_89:                               # %if.then379
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.extract_bit_field, %rcx
	movq	$1244, %rsi             # imm = 0x4DC
.LBB5_44:                               # %if.else90
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp83:
	.size	extract_bit_field, .Ltmp83-extract_bit_field
	.cfi_endproc

	.globl	expand_shift
	.align	16, 0x90
	.type	expand_shift,@function
expand_shift:                           # @expand_shift
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp90:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp92:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp93:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp95:
	.cfi_def_cfa_offset 128
.Ltmp96:
	.cfi_offset %rbx, -48
.Ltmp97:
	.cfi_offset %r12, -40
.Ltmp98:
	.cfi_offset %r13, -32
.Ltmp99:
	.cfi_offset %r14, -24
.Ltmp100:
	.cfi_offset %r15, -16
	movq	%r9, %r14
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rcx, %r12
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%r13, %rcx
	cmpq	$82, %rcx
	movabsq	$0, %rbx
	movabsq	$1, %rax
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	jne	.LBB6_2
# BB#1:                                 # %entry
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB6_2:                                # %entry
	cmpq	$84, %rcx
	jne	.LBB6_4
# BB#3:                                 # %entry
	movq	%rax, %rbx
.LBB6_4:                                # %entry
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	cmpq	%rcx, %rax
	je	.LBB6_5
# BB#6:                                 # %for.cond.preheader
	movq	%r15, %rdx
	andq	%r13, %rdx
	cmpq	$84, %rdx
	movabsq	$0, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$1, %rcx
	je	.LBB6_8
# BB#7:                                 # %for.cond.preheader
	movq	%rcx, %rax
.LBB6_8:                                # %for.cond.preheader
	movq	%r12, 16(%rsp)          # 8-byte Spill
	cmpq	$82, %rdx
	je	.LBB6_10
# BB#9:                                 # %for.cond.preheader
	movq	%rcx, 32(%rsp)          # 8-byte Spill
.LBB6_10:                               # %for.cond.preheader
	orq	%rbx, 64(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %r15
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	cmpq	$84, %r15
	jne	.LBB6_27
# BB#11:
	movabsq	$0, %r15
	movabsq	$optab_table+200, %rbx
	.align	16, 0x90
.LBB6_12:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	movabsq	$0, %rax
	je	.LBB6_23
# BB#13:                                # %for.body.us
                                        #   in Loop: Header=BB6_12 Depth=1
	testq	%r13, %r12
	jne	.LBB6_31
# BB#14:                                # %if.end39.us
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movabsq	$optab_table+192, %rax
	jne	.LBB6_16
# BB#15:                                # %if.end39.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	%rbx, %rax
.LBB6_16:                               # %if.end39.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	(%rax), %rsi
	movq	$0, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB6_58
# BB#17:                                # %land.lhs.true.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	movabsq	$0, %rax
	jne	.LBB6_23
# BB#18:                                # %land.lhs.true50.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	movabsq	$0, %rax
	jle	.LBB6_23
# BB#19:                                # %land.lhs.true54.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	mode_bitsize(%rax,%rax), %esi
	movq	%rcx, %rax
	andq	%r13, %rax
	cmpq	%rsi, %rax
	movabsq	$0, %rax
	jae	.LBB6_23
# BB#20:                                # %if.then64.us
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movq	%rbx, %rax
	jne	.LBB6_22
# BB#21:                                # %if.then64.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movabsq	$optab_table+192, %rax
.LBB6_22:                               # %if.then64.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	(%rax), %rbx
	subq	%rcx, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	$0, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	movabsq	$optab_table+200, %rbx
	.align	16, 0x90
.LBB6_23:                               # %for.inc.us
                                        #   in Loop: Header=BB6_12 Depth=1
	incq	%r12
	testq	%rax, %rax
	movq	%r15, %rdx
	jne	.LBB6_25
# BB#24:                                # %for.inc.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movabsq	$1, %rdx
.LBB6_25:                               # %for.inc.us
                                        #   in Loop: Header=BB6_12 Depth=1
	jne	.LBB6_57
# BB#26:                                # %for.inc.us
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$3, %rsi
	jl	.LBB6_12
	jmp	.LBB6_57
.LBB6_5:
	movq	48(%rsp), %rax          # 8-byte Reload
	jmp	.LBB6_58
.LBB6_27:
	andq	%rax, 32(%rsp)          # 8-byte Folded Spill
	movabsq	$0, %rbx
	.align	16, 0x90
.LBB6_28:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%r15, %r15
	testq	%r13, %r12
	je	.LBB6_38
# BB#29:                                # %for.body
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB6_37
# BB#30:                                # %if.then13
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	$2, %r15
	jmp	.LBB6_38
	.align	16, 0x90
.LBB6_37:                               # %if.else14
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	$3, %r15
.LBB6_38:                               # %if.else79
                                        #   in Loop: Header=BB6_28 Depth=1
	testq	%r13, %r14
	je	.LBB6_42
# BB#39:                                # %if.end89
                                        #   in Loop: Header=BB6_28 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movabsq	$optab_table+168, %rax
	jne	.LBB6_41
# BB#40:                                # %if.end89
                                        #   in Loop: Header=BB6_28 Depth=1
	movabsq	$optab_table+176, %rax
.LBB6_41:                               # %if.end89
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	(%rax), %rsi
	movq	%r15, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB6_58
.LBB6_42:                               # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	testq	%r13, %r14
	movq	%rbx, %rcx
	jne	.LBB6_44
# BB#43:                                # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	movabsq	$1, %rcx
.LBB6_44:                               # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	%r15, %rax
	andq	%r13, %rax
	cmpq	$2, %rax
	movq	%rbx, %rax
	jne	.LBB6_46
# BB#45:                                # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	movabsq	$1, %rax
.LBB6_46:                               # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	32(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rax
	jne	.LBB6_48
# BB#47:                                # %land.lhs.true94
                                        #   in Loop: Header=BB6_28 Depth=1
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB6_53
.LBB6_48:                               # %if.then100
                                        #   in Loop: Header=BB6_28 Depth=1
	testq	%r13, %r14
	movq	$4, %rax
	jne	.LBB6_50
# BB#49:                                # %if.then100
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	%r15, %rax
.LBB6_50:                               # %if.then100
                                        #   in Loop: Header=BB6_28 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	movabsq	$optab_table+168, %rcx
	jne	.LBB6_52
# BB#51:                                # %if.then100
                                        #   in Loop: Header=BB6_28 Depth=1
	movabsq	$optab_table+184, %rcx
.LBB6_52:                               # %if.then100
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	(%rcx), %rsi
	movq	%rax, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
.LBB6_53:                               # %for.inc
                                        #   in Loop: Header=BB6_28 Depth=1
	incq	%r12
	testq	%rax, %rax
	movq	%rbx, %rdx
	jne	.LBB6_55
# BB#54:                                # %for.inc
                                        #   in Loop: Header=BB6_28 Depth=1
	movabsq	$1, %rdx
.LBB6_55:                               # %for.inc
                                        #   in Loop: Header=BB6_28 Depth=1
	jne	.LBB6_57
# BB#56:                                # %for.inc
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$3, %rsi
	jl	.LBB6_28
.LBB6_57:                               # %for.end
	testq	%rdx, %rdx
	je	.LBB6_58
# BB#59:                                # %if.then113
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB6_31:                               # %if.then22
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB6_33
# BB#32:                                # %if.then22
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB6_33:                               # %if.then22
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %r15
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	make_tree
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	mode_bitsize(%rcx,%rcx), %edi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	force_reg
	movq	%rax, %r13
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB6_34
# BB#35:                                # %if.then22
	movabsq	$1, %rdi
	jmp	.LBB6_36
.LBB6_34:
	movabsq	$0, %rdi
.LBB6_36:                               # %if.then22
	movq	32(%rsp), %r8           # 8-byte Reload
	orq	$82, %rdi
	movq	$1, %r9
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	expand_shift
	movq	%rax, %r15
	movq	64(%rsp), %rdi          # 8-byte Reload
	orq	$82, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	expand_shift
	movq	optab_table+152(%rip), %rsi
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
.LBB6_58:                               # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp101:
	.size	expand_shift, .Ltmp101-expand_shift
	.cfi_endproc

	.align	16, 0x90
	.type	extract_fixed_bit_field,@function
extract_fixed_bit_field:                # @extract_fixed_bit_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp108:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp109:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp110:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp111:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp112:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp113:
	.cfi_def_cfa_offset 96
.Ltmp114:
	.cfi_offset %rbx, -48
.Ltmp115:
	.cfi_offset %r12, -40
.Ltmp116:
	.cfi_offset %r13, -32
.Ltmp117:
	.cfi_offset %r14, -24
.Ltmp118:
	.cfi_offset %r15, -16
	movl	(%rsi), %r15d
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r8, %r12
	movq	%rcx, %r8
	movq	%rdx, %r13
	movq	%rdi, %r14
	movq	96(%rsp), %rbx
	movq	%r15, %rax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB7_4
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %eax
	leaq	(%r12,%r8), %rdx
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jbe	.LBB7_12
# BB#2:                                 # %if.then9
	movq	%rsi, %rdi
	movq	%r8, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	jmp	.LBB7_3
.LBB7_4:                                # %if.else
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	16(%rsi), %rax
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	leaq	(%r12,%r13,8), %r14
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB7_6
# BB#5:                                 # %cond.true
	movl	32(%rax), %edx
.LBB7_6:                                # %cond.end
	movl	word_mode(%rip), %eax
	movq	$27, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	movq	%r8, %rdi
	movq	%r14, %rsi
	movq	%rax, %rcx
	movq	%r8, %rbx
	movq	%r15, %r8
	callq	get_best_mode
	movq	%rax, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB7_7
# BB#8:                                 # %if.end28
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	cmpq	%r12, %rdi
	jbe	.LBB7_10
# BB#9:                                 # %if.end28.if.end46_crit_edge
	movq	$3, %rcx
	shrq	%cl, %rdi
	jmp	.LBB7_11
.LBB7_7:                                # %if.then24
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	96(%rsp), %rcx
.LBB7_3:                                # %if.then9
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	extract_split_bit_field  # TAILCALL
.LBB7_10:                               # %if.then34
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	imulq	%rdi, %rax
	addq	%rax, %r13
	movq	$3, %rcx
	shlq	%cl, %rax
	subq	%rax, %r12
.LBB7_11:                               # %if.end46
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rdi
	leaq	(%r12,%rdx,8), %r12
	subq	%rdx, %r13
	movq	$1, %rcx
	movq	$1, %r8
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rdx
	callq	adjust_address_1
	movq	%rax, %rsi
	movl	(%rsi), %r15d
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	96(%rsp), %rbx
	movq	16(%rsp), %r8           # 8-byte Reload
.LBB7_12:                               # %if.end56
	movq	$16, %rcx
	shrq	%cl, %r15
	andq	$255, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	je	.LBB7_31
# BB#13:                                # %if.then61
	testq	%r12, %r12
	je	.LBB7_14
# BB#15:                                # %if.then63
	movq	%r14, %rbx
	movq	%rdx, %r14
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r8, %r13
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	build_int_2_wide
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %rcx
	je	.LBB7_19
# BB#16:                                # %land.lhs.true
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movabsq	$0, %rcx
	jne	.LBB7_19
# BB#17:                                # %land.lhs.true71
	andq	$1073741824, %rdx       # imm = 0x40000000
	movq	$30, %rcx
	shrq	%cl, %rdx
	xorq	%rcx, %rcx
	testq	%rdx, %rdx
	jne	.LBB7_19
# BB#18:                                # %land.lhs.true71
	movq	24(%rsp), %rcx          # 8-byte Reload
.LBB7_19:                               # %cond.end78
	movq	%rbx, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %r15
	movabsq	$0, %r8
	jne	.LBB7_21
# BB#20:                                # %cond.end78
	movq	%rcx, %r8
.LBB7_21:                               # %cond.end78
	movq	$83, %rdi
	movq	$1, %r9
	movq	%r15, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_shift
	movq	%rax, %rsi
	movq	%r14, %rdx
	movq	%rbx, %r14
	jmp	.LBB7_22
.LBB7_31:                               # %if.end108
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	force_reg
	movq	%rbx, %rdx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	andq	%rdx, %r14
	xorq	%r13, %r13
	cmpq	%r14, %r15
	je	.LBB7_33
# BB#32:                                # %if.end108
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB7_33:                               # %if.end108
	movl	class_narrowest_mode+4(%rip), %eax
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %r12
	testq	%rax, %rax
	je	.LBB7_38
# BB#34:
	movq	%rax, %r13
	.align	16, 0x90
.LBB7_35:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %r15d
	cmpq	%r12, %r15
	jae	.LBB7_36
# BB#37:                                # %for.inc
                                        #   in Loop: Header=BB7_35 Depth=1
	movzbl	mode_wider_mode(%rax), %r13d
	testq	%r13, %r13
	jne	.LBB7_35
.LBB7_38:                               # %for.end.loopexit
	movq	%rdx, %r14
	movzbl	mode_bitsize(%r13,%r13), %r15d
	jmp	.LBB7_39
.LBB7_14:
	movq	%r8, %r13
.LBB7_22:                               # %if.end85
	movq	%r14, %rax
	andq	%rdx, %rax
	cmpq	%rax, %r15
	je	.LBB7_24
# BB#23:                                # %if.then88
	movq	%rdx, %rbx
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	convert_to_mode
	movq	%rbx, %rdx
	movq	%rax, %rsi
.LBB7_24:                               # %if.end90
	movzbl	mode_bitsize(%r15,%r15), %eax
	addq	%r13, %r12
	cmpq	%r12, %rax
	je	.LBB7_63
# BB#25:                                # %if.then97
	movzbl	2(%rsi), %ebx
	movq	optab_table+144(%rip), %r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$63, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%rsi, %r12
	jg	.LBB7_27
# BB#26:                                # %if.end5.i.thread
	movq	$64, %rcx
	subq	%r13, %rcx
	andq	%rdx, %rcx
	movq	$-1, %rdi
	shrq	%cl, %rdi
	xorq	%rsi, %rsi
	jmp	.LBB7_30
.LBB7_27:                               # %if.end5.i
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$65, %rax
	jl	.LBB7_28
# BB#29:                                # %if.then15.i
	movq	$128, %rcx
	subq	%r13, %rcx
	andq	%rdx, %rcx
	movq	$-1, %rdi
	movq	$-1, %rsi
	shrq	%cl, %rsi
	jmp	.LBB7_30
.LBB7_36:                               # %if.then122
	movq	%rdx, %r14
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert_to_mode
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB7_39:                               # %for.end
	andq	$65535, %r15            # imm = 0xFFFF
	movq	%r15, %rdi
	subq	%r12, %rdi
	je	.LBB7_40
# BB#43:                                # %if.then134
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	callq	build_int_2_wide
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	movabsq	$0, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	je	.LBB7_47
# BB#44:                                # %land.lhs.true145
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	movabsq	$0, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	jne	.LBB7_47
# BB#45:                                # %land.lhs.true150
	andq	$1073741824, %rdx       # imm = 0x40000000
	movq	$30, %rcx
	shrq	%cl, %rdx
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	jne	.LBB7_47
# BB#46:                                # %land.lhs.true150
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 32(%rsp)          # 8-byte Spill
.LBB7_47:                               # %cond.end157
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB7_40
	.align	16, 0x90
.LBB7_48:                               # %for.body.i261
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	testq	%r14, %r12
	je	.LBB7_52
# BB#49:                                # %for.body.i261
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	%r12, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB7_51
# BB#50:                                # %if.then13.i262
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	$2, %rax
	jmp	.LBB7_52
	.align	16, 0x90
.LBB7_51:                               # %if.else14.i263
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	$3, %rax
.LBB7_52:                               # %if.else79.i265
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	optab_table+168(%rip), %rsi
	movq	$1, %r9
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	32(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB7_41
# BB#53:                                # %land.lhs.true94.i
                                        #   in Loop: Header=BB7_48 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB7_48
	jmp	.LBB7_54
.LBB7_40:
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB7_41:                               # %if.end160
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r15
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	je	.LBB7_42
# BB#55:
	movq	24(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB7_56:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %rbx
	movabsq	$0, %rax
	je	.LBB7_60
# BB#57:                                # %for.body.i
                                        #   in Loop: Header=BB7_56 Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB7_59
# BB#58:                                # %if.then13.i
                                        #   in Loop: Header=BB7_56 Depth=1
	movq	$2, %rax
	jmp	.LBB7_60
	.align	16, 0x90
.LBB7_59:                               # %if.else14.i
                                        #   in Loop: Header=BB7_56 Depth=1
	movq	$3, %rax
.LBB7_60:                               # %if.else79.i
                                        #   in Loop: Header=BB7_56 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB7_62
# BB#61:                                # %if.else79.i
                                        #   in Loop: Header=BB7_56 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB7_56
.LBB7_62:                               # %for.end.i
	testq	%rax, %rax
	movq	%rax, %rsi
	jne	.LBB7_63
.LBB7_54:                               # %for.end.i271
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB7_42:
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB7_63:                               # %return
	movq	%rsi, %rax
	jmp	.LBB7_64
.LBB7_28:
	movq	$-1, %rdi
	xorq	%rsi, %rsi
.LBB7_30:                               # %mask_rtx.exit
	movq	%rbx, %rdx
	callq	immed_double_const
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	callq	expand_binop
.LBB7_64:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp119:
	.size	extract_fixed_bit_field, .Ltmp119-extract_fixed_bit_field
	.cfi_endproc

	.globl	expand_inc
	.align	16, 0x90
	.type	expand_inc,@function
expand_inc:                             # @expand_inc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp123:
	.cfi_def_cfa_offset 32
.Ltmp124:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	optab_table(%rip), %rax
	movzbl	2(%rbx), %edi
	movq	%rsi, %rcx
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB8_1
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	jmp	emit_move_insn  # TAILCALL
.LBB8_1:                                # %if.end
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp125:
	.size	expand_inc, .Ltmp125-expand_inc
	.cfi_endproc

	.globl	expand_dec
	.align	16, 0x90
	.type	expand_dec,@function
expand_dec:                             # @expand_dec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp128:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp129:
	.cfi_def_cfa_offset 32
.Ltmp130:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	optab_table+16(%rip), %rax
	movzbl	2(%rbx), %edi
	movq	%rsi, %rcx
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB9_1
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	jmp	emit_move_insn  # TAILCALL
.LBB9_1:                                # %if.end
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp131:
	.size	expand_dec, .Ltmp131-expand_dec
	.cfi_endproc

	.globl	make_tree
	.align	16, 0x90
	.type	make_tree,@function
make_tree:                              # @make_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp137:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp138:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp139:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp140:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp141:
	.cfi_def_cfa_offset 64
.Ltmp142:
	.cfi_offset %rbx, -40
.Ltmp143:
	.cfi_offset %r12, -32
.Ltmp144:
	.cfi_offset %r14, -24
.Ltmp145:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movl	(%r15), %ecx
	movq	%rdi, %r14
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$119, %rax
	jg	.LBB10_37
# BB#1:                                 # %entry
	cmpq	$74, %rax
	jg	.LBB10_9
# BB#2:                                 # %entry
	cmpq	$54, %rax
	je	.LBB10_11
# BB#3:                                 # %entry
	cmpq	$55, %rax
	jne	.LBB10_4
# BB#16:                                # %sw.bb17
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB10_17
# BB#18:                                # %do.body
	addq	$16, %r15
	movq	16(%r15), %rax
	movq	%rax, 16(%rsp)
	movq	(%r15), %rax
	movq	8(%r15), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	jmp	.LBB10_43
.LBB10_37:                              # %entry
	leaq	-120(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jae	.LBB10_42
# BB#38:                                # %sw.bb163
	movq	8(%r15), %rcx
	cmpq	$121, %rax
	jne	.LBB10_39
# BB#40:                                # %sw.bb163
	movabsq	$1, %rsi
	jmp	.LBB10_41
.LBB10_9:                               # %entry
	addq	$-75, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$15, %rcx
	ja	.LBB10_42
# BB#10:                                # %entry
	jmpq	*.LJTI10_0(,%rcx,8)
.LBB10_20:                              # %sw.bb51
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$59, %rdi
	jmp	.LBB10_21
.LBB10_39:
	movabsq	$0, %rsi
.LBB10_41:                              # %sw.bb163
	movzbl	2(%rcx), %edi
	callq	type_for_mode
	movq	8(%r15), %rsi
	movq	%rax, %rdi
	callq	make_tree
.LBB10_34:                              # %return
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
.LBB10_35:                              # %return
	movq	%rax, %rdi
	callq	fold
	jmp	.LBB10_43
.LBB10_11:                              # %sw.bb
	movzbl	17(%r14), %eax
	movq	8(%r15), %rdi
	testq	$32, %rax
	je	.LBB10_13
# BB#12:                                # %land.lhs.true
	movl	60(%r14), %eax
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$254, %rax
	movzbl	mode_bitsize(%rax), %eax
	xorq	%rsi, %rsi
	cmpq	$64, %rax
	jb	.LBB10_14
.LBB10_13:                              # %lor.rhs
	movq	$63, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
.LBB10_14:                              # %lor.end
	movq	$63, %rcx
	shlq	%cl, %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	jmp	.LBB10_15
.LBB10_4:                               # %entry
	cmpq	$56, %rax
	jne	.LBB10_42
# BB#5:                                 # %sw.bb37
	movq	8(%r15), %rax
	movslq	(%rax), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	jle	.LBB10_19
# BB#6:                                 # %for.body.lr.ph
	decq	%r12
	xorq	%rbx, %rbx
	jmp	.LBB10_7
	.align	16, 0x90
.LBB10_8:                               # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB10_7 Depth=1
	movq	8(%r15), %rax
	decq	%r12
.LBB10_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%r12,8), %rsi
	movq	%r14, %rdi
	callq	make_tree
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB10_8
.LBB10_19:                              # %for.end
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	build_vector
	jmp	.LBB10_43
.LBB10_42:                              # %sw.default
	movq	$120, %rdi
	callq	make_node
	movq	%r14, 8(%rax)
	movq	%r15, 40(%rax)
	movq	$0, 32(%rax)
	jmp	.LBB10_43
.LBB10_17:                              # %if.then
	movq	16(%r15), %rdi
	movq	24(%r15), %rsi
.LBB10_15:                              # %return
	callq	build_int_2_wide
	movq	%r14, 8(%rax)
.LBB10_43:                              # %return
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB10_22:                              # %sw.bb61
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$60, %rdi
	jmp	.LBB10_21
.LBB10_23:                              # %sw.bb72
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	make_tree
	movq	$77, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build1
	jmp	.LBB10_35
.LBB10_24:                              # %sw.bb79
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$61, %rdi
	jmp	.LBB10_21
.LBB10_29:                              # %sw.bb129
	movzbl	16(%r14), %eax
	cmpq	$7, %rax
	movq	%r14, %rbx
	je	.LBB10_31
# BB#30:                                # %if.then135
	movq	%r14, %rdi
	callq	signed_type
	movq	%rax, %rbx
.LBB10_31:                              # %if.end138
	movq	8(%r15), %rsi
	jmp	.LBB10_32
.LBB10_36:                              # %sw.bb150
	movq	%r14, %rdi
	callq	unsigned_type
	movq	8(%r15), %rsi
	movq	%rax, %rbx
.LBB10_32:                              # %return
	movq	%rbx, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$62, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	jmp	.LBB10_33
.LBB10_25:                              # %sw.bb90
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$82, %rdi
.LBB10_21:                              # %return
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build
	jmp	.LBB10_35
.LBB10_28:                              # %sw.bb115
	movq	%r14, %rdi
	callq	signed_type
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	signed_type
	jmp	.LBB10_27
.LBB10_26:                              # %sw.bb101
	movq	%r14, %rdi
	callq	unsigned_type
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	unsigned_type
.LBB10_27:                              # %return
	movq	8(%r15), %rsi
	movq	%rax, %rdi
	callq	make_tree
	movq	16(%r15), %rsi
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	make_tree
	movq	%rax, %rcx
	movq	$83, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
.LBB10_33:                              # %return
	callq	build
	jmp	.LBB10_34
.Ltmp146:
	.size	make_tree, .Ltmp146-make_tree
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI10_0:
	.quad	.LBB10_20
	.quad	.LBB10_22
	.quad	.LBB10_23
	.quad	.LBB10_24
	.quad	.LBB10_29
	.quad	.LBB10_42
	.quad	.LBB10_36
	.quad	.LBB10_42
	.quad	.LBB10_42
	.quad	.LBB10_42
	.quad	.LBB10_42
	.quad	.LBB10_42
	.quad	.LBB10_25
	.quad	.LBB10_42
	.quad	.LBB10_28
	.quad	.LBB10_26

	.text
	.globl	expand_mult
	.align	16, 0x90
	.type	expand_mult,@function
expand_mult:                            # @expand_mult
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp153:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp154:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp155:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp156:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp157:
	.cfi_def_cfa_offset 48
	subq	$784, %rsp              # imm = 0x310
.Ltmp158:
	.cfi_def_cfa_offset 832
.Ltmp159:
	.cfi_offset %rbx, -48
.Ltmp160:
	.cfi_offset %r12, -40
.Ltmp161:
	.cfi_offset %r13, -32
.Ltmp162:
	.cfi_offset %r14, -24
.Ltmp163:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movl	(%r14), %eax
	movq	%r8, %rbx
	movq	%rcx, %r15
	movq	%rsi, %r8
	movq	%rdi, 120(%rsp)         # 8-byte Spill
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$55, %rdx
	jne	.LBB11_6
# BB#1:                                 # %land.lhs.true
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB11_6
# BB#2:                                 # %land.lhs.true4
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rax, %rcx
	cmpq	$32, %rcx
	ja	.LBB11_6
# BB#3:                                 # %land.lhs.true6
	cmpq	$0, 24(%r14)
	je	.LBB11_4
.LBB11_6:                               # %if.else
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	$33, %rax
	movq	%r14, %rax
	jb	.LBB11_5
# BB#7:                                 # %if.else
	cmpq	$54, %rdx
	movq	%r14, %rax
	jne	.LBB11_5
# BB#8:                                 # %land.lhs.true21
	cmpq	$0, 8(%r14)
	movq	%r14, %rax
	jns	.LBB11_9
	jmp	.LBB11_26
.LBB11_4:                               # %if.then
	movq	16(%r14), %rsi
	xorq	%rdi, %rdi
	movq	%r8, %r12
	callq	gen_rtx_CONST_INT
	movq	%r12, %r8
.LBB11_5:                               # %if.end28
	testq	%rax, %rax
	je	.LBB11_26
.LBB11_9:                               # %land.lhs.true30
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB11_26
# BB#10:                                # %land.lhs.true35
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rbx
	jne	.LBB11_12
# BB#11:                                # %land.lhs.true35
	movl	flag_trapv(%rip), %eax
	testq	%r13, %rax
	jne	.LBB11_26
.LBB11_12:                              # %if.then38
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	8(%r14), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r8, %rsi
	callq	force_reg
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	add_cost(%rip), %esi
	sarq	%cl, %rdx
	movq	$2, %rcx
	shlq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %r12
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jle	.LBB11_14
# BB#13:                                # %if.then38
	movq	%rax, %r12
.LBB11_14:                              # %if.then38
	leaq	456(%rsp), %rdi
	movq	%rdi, 112(%rsp)         # 8-byte Spill
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	synth_mult
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	cmpq	$33, %rax
	jae	.LBB11_15
# BB#16:                                # %if.then54
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rsi
	negq	%rsi
	movswq	456(%rsp), %r15
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r15
	movq	%r15, %rdx
	jle	.LBB11_18
# BB#17:                                # %if.then54
	movq	%r12, %rdx
.LBB11_18:                              # %if.then54
	movl	negate_cost(%rip), %eax
	subq	%rax, %rdx
	leaq	128(%rsp), %rdi
	callq	synth_mult
	movswq	128(%rsp), %rax
	movl	negate_cost(%rip), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	%r15, %rdx
	jge	.LBB11_20
# BB#19:                                # %if.then71
	leaq	456(%rsp), %rdi
	movq	%rdi, 112(%rsp)         # 8-byte Spill
	leaq	128(%rsp), %rsi
	movq	$324, %rdx              # imm = 0x144
	callq	memcpy
	movq	$1, %rax
	jmp	.LBB11_20
.LBB11_15:                              # %if.then38.if.end73_crit_edge
	movq	%r15, 72(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
.LBB11_20:                              # %if.end73
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ebx
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	-1(%rax), %rsi
	movq	$48, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$48, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r15
	movq	%r15, %rdx
	jle	.LBB11_22
# BB#21:                                # %if.end73
	movq	%r12, %rdx
.LBB11_22:                              # %if.end73
	movl	add_cost(%rip), %eax
	subq	%rax, %rdx
	leaq	128(%rsp), %rdi
	callq	synth_mult
	movswq	128(%rsp), %rax
	movl	add_cost(%rip), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r15, %rdx
	jge	.LBB11_24
# BB#23:                                # %if.then93
	leaq	456(%rsp), %rdi
	leaq	128(%rsp), %rsi
	movq	$324, %rdx              # imm = 0x144
	callq	memcpy
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ebx
	movq	$2, 16(%rsp)            # 8-byte Folded Spill
.LBB11_24:                              # %if.end94
	movabsq	$4294901760, %rax       # imm = 0xFFFF0000
	andq	%rbx, %rax
	movq	$16, %rcx
	shrq	%cl, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$48, %rcx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	%r12, %rbx
	jge	.LBB11_25
# BB#30:                                # %if.then99
	xorq	%rsi, %rsi
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rbx
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB11_32
# BB#31:                                # %if.then105
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rbx
.LBB11_32:                              # %if.end107
	movl	460(%rsp), %eax
	cmpq	$1, %rax
	jne	.LBB11_33
# BB#37:                                # %if.then118
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
	movq	$1, %rax
	jmp	.LBB11_35
.LBB11_25:
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	104(%rsp), %r8          # 8-byte Reload
.LBB11_26:                              # %if.end289
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB11_142
# BB#27:                                # %if.end289
	movl	flag_trapv(%rip), %ecx
	testq	%rax, %rcx
	je	.LBB11_142
# BB#28:                                # %land.lhs.true293
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB11_142
# BB#29:
	movabsq	$optab_table+40, %rax
	jmp	.LBB11_143
.LBB11_142:                             # %cond.false299
	movabsq	$optab_table+32, %rax
.LBB11_143:                             # %cond.end300
	movq	(%rax), %rsi
	movq	$3, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r8, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	movq	%rbx, %r9
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB11_144
.LBB11_145:                             # %return
	movq	%r15, %rax
	addq	$784, %rsp              # imm = 0x310
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_33:                              # %if.end107
	testq	%rax, %rax
	jne	.LBB11_38
# BB#34:                                # %if.then111
	movq	const_int_rtx+512(%rip), %rsi
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	copy_to_mode_reg
	movq	%rax, %r15
	xorq	%rax, %rax
.LBB11_35:                              # %for.cond.preheader
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$48, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB11_36
# BB#40:                                # %for.body.lr.ph
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	.align	16, 0x90
.LBB11_41:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_100 Depth 2
                                        #     Child Loop BB11_89 Depth 2
                                        #     Child Loop BB11_122 Depth 2
                                        #     Child Loop BB11_111 Depth 2
                                        #     Child Loop BB11_78 Depth 2
                                        #     Child Loop BB11_67 Depth 2
                                        #     Child Loop BB11_58 Depth 2
	movq	%r15, 64(%rsp)          # 8-byte Spill
	cmpq	$2, 32(%rsp)            # 8-byte Folded Reload
	movq	%rax, %rcx
	jne	.LBB11_43
# BB#42:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movabsq	$1, %r12
	movq	%r12, %rcx
.LBB11_43:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	movq	%rax, %r12
	movq	%rax, %r15
	jne	.LBB11_45
# BB#44:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movabsq	$1, %rbx
	movq	%rbx, %r12
.LBB11_45:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movsbq	716(%rsp,%rsi), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %r14
	callq	preserve_subexpressions_p
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	andq	%r13, %rcx
	movq	%r15, %rsi
	movq	%rsi, %rax
	je	.LBB11_47
# BB#46:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movabsq	$1, %rax
.LBB11_47:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	jne	.LBB11_49
# BB#48:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rdi, 80(%rsp)          # 8-byte Spill
.LBB11_49:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$48, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$48, %rcx
	sarq	%cl, %rdx
	decq	%rdx
	andq	%r13, %rdx
	movq	%r14, %rcx
	andq	%r13, %rcx
	cmpq	%rdx, %rcx
	movq	%rsi, %rcx
	je	.LBB11_51
# BB#50:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movabsq	$1, %rcx
.LBB11_51:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	orq	%r12, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	orq	%rdx, %rcx
	orq	%rcx, %rax
	jne	.LBB11_53
# BB#52:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	72(%rsp), %rbx          # 8-byte Reload
.LBB11_53:                              # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movl	460(%rsp,%r14,4), %eax
	addq	$-2, %rax
	andq	%r13, %rax
	cmpq	$6, %rax
	movq	%rdi, %r15
	ja	.LBB11_146
# BB#54:                                # %for.body
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 56(%rsp)          # 8-byte Spill
	jmpq	*.LJTI11_0(,%rax,8)
.LBB11_55:                              # %sw.bb
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	jne	.LBB11_58
# BB#56:                                #   in Loop: Header=BB11_41 Depth=1
	movq	112(%rsp), %r12         # 8-byte Reload
	jmp	.LBB11_57
	.align	16, 0x90
.LBB11_58:                              # %for.body.i
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_62
# BB#59:                                # %for.body.i
                                        #   in Loop: Header=BB11_58 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_61
# BB#60:                                # %if.then13.i
                                        #   in Loop: Header=BB11_58 Depth=2
	movq	$2, %rax
	jmp	.LBB11_62
	.align	16, 0x90
.LBB11_61:                              # %if.else14.i
                                        #   in Loop: Header=BB11_58 Depth=2
	movq	$3, %rax
.LBB11_62:                              # %if.else79.i
                                        #   in Loop: Header=BB11_58 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_64
# BB#63:                                # %if.else79.i
                                        #   in Loop: Header=BB11_58 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_58
.LBB11_64:                              # %for.end.i
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	movq	%rax, %r15
	movq	112(%rsp), %r12         # 8-byte Reload
	je	.LBB11_65
.LBB11_57:                              # %expand_shift.exit
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	104(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, 96(%rsp)           # 8-byte Folded Spill
	jmp	.LBB11_133
.LBB11_66:                              # %sw.bb164
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	movq	112(%rsp), %rax         # 8-byte Reload
	je	.LBB11_74
	.align	16, 0x90
.LBB11_67:                              # %for.body.i458
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_71
# BB#68:                                # %for.body.i458
                                        #   in Loop: Header=BB11_67 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_70
# BB#69:                                # %if.then13.i459
                                        #   in Loop: Header=BB11_67 Depth=2
	movq	$2, %rax
	jmp	.LBB11_71
	.align	16, 0x90
.LBB11_70:                              # %if.else14.i460
                                        #   in Loop: Header=BB11_67 Depth=2
	movq	$3, %rax
.LBB11_71:                              # %if.else79.i462
                                        #   in Loop: Header=BB11_67 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r12, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_73
# BB#72:                                # %if.else79.i462
                                        #   in Loop: Header=BB11_67 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_67
.LBB11_73:                              # %for.end.i469
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	je	.LBB11_65
.LBB11_74:                              # %expand_shift.exit472
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$75, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	movq	80(%rsp), %rcx          # 8-byte Reload
	jne	.LBB11_76
# BB#75:                                # %expand_shift.exit472
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rcx, %rsi
.LBB11_76:                              # %expand_shift.exit472
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r15
	movq	$1, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rax
	addq	%rax, 96(%rsp)          # 8-byte Folded Spill
	movq	112(%rsp), %r12         # 8-byte Reload
	jmp	.LBB11_133
.LBB11_77:                              # %sw.bb178
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB11_85
	.align	16, 0x90
.LBB11_78:                              # %for.body.i480
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_82
# BB#79:                                # %for.body.i480
                                        #   in Loop: Header=BB11_78 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_81
# BB#80:                                # %if.then13.i481
                                        #   in Loop: Header=BB11_78 Depth=2
	movq	$2, %rax
	jmp	.LBB11_82
	.align	16, 0x90
.LBB11_81:                              # %if.else14.i482
                                        #   in Loop: Header=BB11_78 Depth=2
	movq	$3, %rax
.LBB11_82:                              # %if.else79.i484
                                        #   in Loop: Header=BB11_78 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r12, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_84
# BB#83:                                # %if.else79.i484
                                        #   in Loop: Header=BB11_78 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_78
.LBB11_84:                              # %for.end.i491
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	je	.LBB11_65
.LBB11_85:                              # %expand_shift.exit494
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$76, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	jne	.LBB11_87
# BB#86:                                # %expand_shift.exit494
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%r14, %rsi
.LBB11_87:                              # %expand_shift.exit494
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r15
	movq	$1, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rax
	subq	%rax, 96(%rsp)          # 8-byte Folded Spill
	movq	112(%rsp), %r12         # 8-byte Reload
	jmp	.LBB11_133
.LBB11_110:                             # %sw.bb220
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	movq	%r15, %rax
	je	.LBB11_118
	.align	16, 0x90
.LBB11_111:                             # %for.body.i546
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_115
# BB#112:                               # %for.body.i546
                                        #   in Loop: Header=BB11_111 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_114
# BB#113:                               # %if.then13.i547
                                        #   in Loop: Header=BB11_111 Depth=2
	movq	$2, %rax
	jmp	.LBB11_115
	.align	16, 0x90
.LBB11_114:                             # %if.else14.i548
                                        #   in Loop: Header=BB11_111 Depth=2
	movq	$3, %rax
.LBB11_115:                             # %if.else79.i550
                                        #   in Loop: Header=BB11_111 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_117
# BB#116:                               # %if.else79.i550
                                        #   in Loop: Header=BB11_111 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_111
.LBB11_117:                             # %for.end.i557
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	je	.LBB11_65
.LBB11_118:                             # %expand_shift.exit560
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$75, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	movq	80(%rsp), %rcx          # 8-byte Reload
	jne	.LBB11_120
# BB#119:                               # %expand_shift.exit560
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rcx, %rsi
.LBB11_120:                             # %expand_shift.exit560
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r15
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rax
	addq	%rax, %rdx
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	112(%rsp), %r12         # 8-byte Reload
	jmp	.LBB11_133
.LBB11_121:                             # %sw.bb234
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	%r15, %rbx
	je	.LBB11_129
	.align	16, 0x90
.LBB11_122:                             # %for.body.i568
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r12
	movabsq	$0, %rax
	je	.LBB11_126
# BB#123:                               # %for.body.i568
                                        #   in Loop: Header=BB11_122 Depth=2
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_125
# BB#124:                               # %if.then13.i569
                                        #   in Loop: Header=BB11_122 Depth=2
	movq	$2, %rax
	jmp	.LBB11_126
	.align	16, 0x90
.LBB11_125:                             # %if.else14.i570
                                        #   in Loop: Header=BB11_122 Depth=2
	movq	$3, %rax
.LBB11_126:                             # %if.else79.i572
                                        #   in Loop: Header=BB11_122 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB11_128
# BB#127:                               # %if.else79.i572
                                        #   in Loop: Header=BB11_122 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB11_122
.LBB11_128:                             # %for.end.i579
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rbx, %rbx
	je	.LBB11_65
.LBB11_129:                             # %expand_shift.exit582
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$76, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	movq	112(%rsp), %r12         # 8-byte Reload
	jne	.LBB11_132
# BB#130:                               # %cond.false241
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%r13, %rcx
	jne	.LBB11_132
# BB#131:                               # %cond.false241
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rbx, %rsi
.LBB11_132:                             # %cond.end247
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	shlq	%cl, %rdx
	subq	%rsi, %rdx
	movq	%rax, %r15
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	jmp	.LBB11_133
.LBB11_88:                              # %sw.bb192
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB11_96
	.align	16, 0x90
.LBB11_89:                              # %for.body.i502
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_93
# BB#90:                                # %for.body.i502
                                        #   in Loop: Header=BB11_89 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_92
# BB#91:                                # %if.then13.i503
                                        #   in Loop: Header=BB11_89 Depth=2
	movq	$2, %rax
	jmp	.LBB11_93
	.align	16, 0x90
.LBB11_92:                              # %if.else14.i504
                                        #   in Loop: Header=BB11_89 Depth=2
	movq	$3, %rax
.LBB11_93:                              # %if.else79.i506
                                        #   in Loop: Header=BB11_89 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_95
# BB#94:                                # %if.else79.i506
                                        #   in Loop: Header=BB11_89 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_89
.LBB11_95:                              # %for.end.i513
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	movq	%rax, %r15
	je	.LBB11_65
.LBB11_96:                              # %expand_shift.exit516
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$75, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	jne	.LBB11_98
# BB#97:                                # %expand_shift.exit516
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%r14, %rsi
.LBB11_98:                              # %expand_shift.exit516
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r15
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	incq	%rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	jmp	.LBB11_133
.LBB11_99:                              # %sw.bb206
                                        #   in Loop: Header=BB11_41 Depth=1
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB11_107
	.align	16, 0x90
.LBB11_100:                             # %for.body.i524
                                        #   Parent Loop BB11_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB11_104
# BB#101:                               # %for.body.i524
                                        #   in Loop: Header=BB11_100 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB11_103
# BB#102:                               # %if.then13.i525
                                        #   in Loop: Header=BB11_100 Depth=2
	movq	$2, %rax
	jmp	.LBB11_104
	.align	16, 0x90
.LBB11_103:                             # %if.else14.i526
                                        #   in Loop: Header=BB11_100 Depth=2
	movq	$3, %rax
.LBB11_104:                             # %if.else79.i528
                                        #   in Loop: Header=BB11_100 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB11_106
# BB#105:                               # %if.else79.i528
                                        #   in Loop: Header=BB11_100 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB11_100
.LBB11_106:                             # %for.end.i535
                                        #   in Loop: Header=BB11_41 Depth=1
	testq	%rax, %rax
	movq	%rax, %r15
	je	.LBB11_65
.LBB11_107:                             # %expand_shift.exit538
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	$76, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	48(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	jne	.LBB11_109
# BB#108:                               # %expand_shift.exit538
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%r14, %rsi
.LBB11_109:                             # %expand_shift.exit538
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r15
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	decq	%rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB11_133:                             # %sw.epilog
                                        #   in Loop: Header=BB11_41 Depth=1
	movzbl	(%r15), %eax
	cmpq	$63, %rax
	movq	%r12, %r14
	movq	120(%rsp), %rbx         # 8-byte Reload
	jne	.LBB11_135
# BB#134:                               # %if.then257
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	8(%r15), %rax
	movzbl	2(%rax), %ebx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	gen_lowpart
	movq	%rax, %r14
.LBB11_135:                             # %if.end264
                                        #   in Loop: Header=BB11_41 Depth=1
	movq	%r12, 112(%rsp)         # 8-byte Spill
	callq	get_last_insn
	movq	%rax, %r12
	xorq	%rdi, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	$78, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r12, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	movq	56(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	$48, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movabsq	$0, %rax
	jl	.LBB11_41
	jmp	.LBB11_136
.LBB11_36:
	movq	%rbx, 112(%rsp)         # 8-byte Spill
.LBB11_136:                             # %for.end
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$2, %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	jne	.LBB11_137
# BB#139:                               # %if.then277
	movq	96(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	movq	$75, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB11_140
.LBB11_137:                             # %for.end
	cmpq	$1, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB11_140
# BB#138:                               # %if.then271
	movq	optab_table+256(%rip), %rsi
	negq	%rbx
	xorq	%r8, %r8
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %r15
.LBB11_140:                             # %if.end282
	cmpq	%rbx, %r14
	je	.LBB11_145
# BB#141:                               # %if.then285
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_mult, %rcx
	movq	$2558, %rsi             # imm = 0x9FE
	jmp	.LBB11_39
.LBB11_146:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.expand_mult, %rdx
	movq	$2524, %rsi             # imm = 0x9DC
	callq	fancy_abort
.LBB11_144:                             # %if.then305
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_mult, %rcx
	movq	$2572, %rsi             # imm = 0xA0C
	jmp	.LBB11_39
.LBB11_38:                              # %if.else120
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_mult, %rcx
	movq	$2446, %rsi             # imm = 0x98E
	jmp	.LBB11_39
.LBB11_65:                              # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
.LBB11_39:                              # %if.else120
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp164:
	.size	expand_mult, .Ltmp164-expand_mult
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_55
	.quad	.LBB11_66
	.quad	.LBB11_77
	.quad	.LBB11_110
	.quad	.LBB11_121
	.quad	.LBB11_88
	.quad	.LBB11_99

	.text
	.align	16, 0x90
	.type	synth_mult,@function
synth_mult:                             # @synth_mult
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp168:
	.cfi_def_cfa_offset 16
.Ltmp169:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp170:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
.Ltmp171:
	.cfi_offset %rbx, -56
.Ltmp172:
	.cfi_offset %r12, -48
.Ltmp173:
	.cfi_offset %r13, -40
.Ltmp174:
	.cfi_offset %r14, -32
.Ltmp175:
	.cfi_offset %r15, -24
	movq	%rdx, %rbx
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movw	%bx, (%r14)
	jle	.LBB12_59
# BB#1:                                 # %if.end
	testq	%r12, %r12
	jne	.LBB12_2
# BB#4:                                 # %if.then10
	movslq	zero_cost(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB12_59
# BB#5:                                 # %if.else
	movq	$1, %rcx
	movw	%cx, 2(%r14)
	movw	%ax, (%r14)
	xorq	%rax, %rax
	movl	%eax, 4(%r14)
	jmp	.LBB12_59
.LBB12_2:                               # %if.end
	cmpq	$1, %r12
	jne	.LBB12_6
# BB#3:                                 # %if.then5
	movq	$1, %rax
	movw	%ax, 2(%r14)
	xorq	%rcx, %rcx
	movw	%cx, (%r14)
	movl	%eax, 4(%r14)
	jmp	.LBB12_59
.LBB12_6:                               # %if.end19
	movq	%rsp, %r13
	addq	$-336, %r13             # imm = 0xFFFFFFFFFFFFFEB0
	movq	%r13, %rsp
	movq	%rsp, %rax
	addq	$-336, %rax             # imm = 0xFFFFFFFFFFFFFEB0
	movq	%rax, -56(%rbp)         # 8-byte Spill
	movq	%rax, %rsp
	testq	$1, %r12
	je	.LBB12_14
# BB#7:
	movq	$1, %rcx
	.align	16, 0x90
.LBB12_8:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	leaq	(%rax,%rax), %rcx
	testq	%r12, %rax
	jne	.LBB12_8
# BB#9:                                 # %for.end
	testq	%rax, %rax
	je	.LBB12_12
# BB#10:                                # %lor.lhs.false
	cmpq	$3, %rax
	jb	.LBB12_17
# BB#11:                                # %lor.lhs.false
	cmpq	$3, %r12
	je	.LBB12_17
.LBB12_12:                              # %if.then59
	movl	add_cost(%rip), %eax
	leaq	1(%r12), %rsi
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%rbx, %rdx
	subq	%rax, %rdx
	movq	%r13, %rdi
	callq	synth_mult
	movswq	(%r13), %rdx
	movl	add_cost(%rip), %eax
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	movq	$1, -64(%rbp)           # 8-byte Folded Spill
	jge	.LBB12_18
# BB#13:                                # %if.then67
	movq	%rdx, -48(%rbp)         # 8-byte Spill
	movswq	2(%r13), %rax
	xorq	%rcx, %rcx
	movb	%cl, 260(%r13,%rax)
	movswq	2(%r13), %rax
	movq	$4, %rcx
	jmp	.LBB12_20
.LBB12_14:                              # %if.then22
	movq	%r12, %rdi
	negq	%rdi
	andq	%r12, %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	movl	target_flags(%rip), %edx
	sarq	%cl, %rsi
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movq	$0, -64(%rbp)           # 8-byte Folded Spill
	cmpq	%rdx, %rsi
	jge	.LBB12_18
# BB#15:                                # %if.then27
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%r12, %rsi
	shrq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	%rax, -72(%rbp)         # 8-byte Spill
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	shift_cost(,%rdx,4), %eax
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%rbx, %rdx
	subq	%rax, %rdx
	movq	%r13, %rdi
	callq	synth_mult
	movswq	(%r13), %rdx
	movq	-48(%rbp), %rax         # 8-byte Reload
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	jge	.LBB12_18
# BB#16:                                # %if.end45
	movq	%rdx, -48(%rbp)         # 8-byte Spill
	movswq	2(%r13), %rax
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movb	%cl, 260(%r13,%rax)
	movswq	2(%r13), %rax
	movq	$2, %rcx
	jmp	.LBB12_20
.LBB12_17:                              # %if.else78
	movl	add_cost(%rip), %eax
	leaq	-1(%r12), %rsi
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%rbx, %rdx
	subq	%rax, %rdx
	movq	%r13, %rdi
	callq	synth_mult
	movswq	(%r13), %rdx
	movl	add_cost(%rip), %eax
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rax
	movq	$1, -64(%rbp)           # 8-byte Folded Spill
	jge	.LBB12_18
# BB#19:                                # %if.then86
	movq	%rdx, -48(%rbp)         # 8-byte Spill
	movswq	2(%r13), %rax
	xorq	%rcx, %rcx
	movb	%cl, 260(%r13,%rax)
	movswq	2(%r13), %rax
	movq	$3, %rcx
.LBB12_20:                              # %if.end98
	movl	%ecx, 4(%r13,%rax,4)
	movq	%r13, %r15
	movq	-56(%rbp), %r13         # 8-byte Reload
	jmp	.LBB12_21
.LBB12_18:
	movq	-56(%rbp), %r15         # 8-byte Reload
	movq	%rbx, -48(%rbp)         # 8-byte Spill
.LBB12_21:                              # %if.end98
	leaq	-1(%r12), %rbx
	movq	%rbx, %rdi
	callq	floor_log2_wide
	movq	%rax, %rdi
	movq	$32, %rcx
                                        # kill: RAX<def> RDI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB12_22
# BB#23:                                # %for.body104.lr.ph
	movq	%rbx, -72(%rbp)         # 8-byte Spill
	movq	%r15, -56(%rbp)         # 8-byte Spill
	movl	target_flags(%rip), %r8d
	movq	$20, %rcx
	shrq	%cl, %r8
	andq	$32, %r8
	addq	$32, %r8
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%rdi, %r15
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$2, %rcx
	shlq	%cl, %rsi
	.align	16, 0x90
.LBB12_24:                              # %for.body104
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rbx
	movq	%r15, %rcx
	shlq	%cl, %rbx
	leaq	1(%rbx), %r9
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%r9
	testq	%rdx, %rdx
	jne	.LBB12_32
# BB#25:                                # %for.body104
                                        #   in Loop: Header=BB12_24 Depth=1
	cmpq	%r12, %r9
	jae	.LBB12_32
# BB#26:                                # %for.body104
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jl	.LBB12_27
.LBB12_32:                              # %if.end151
                                        #   in Loop: Header=BB12_24 Depth=1
	decq	%rbx
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rbx
	testq	%rdx, %rdx
	jne	.LBB12_40
# BB#33:                                # %if.end151
                                        #   in Loop: Header=BB12_24 Depth=1
	cmpq	%r12, %rbx
	jae	.LBB12_40
# BB#34:                                # %if.end151
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jl	.LBB12_35
.LBB12_40:                              # %for.inc204
                                        #   in Loop: Header=BB12_24 Depth=1
	decq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r15
	addq	$-4, %rsi
	cmpq	$1, %rax
	jg	.LBB12_24
# BB#41:
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	%r13, %r15
	jmp	.LBB12_42
.LBB12_22:
	movq	%r15, %rax
	movq	%r13, %r15
	movq	-48(%rbp), %r13         # 8-byte Reload
	jmp	.LBB12_44
.LBB12_27:                              # %if.then119
	movl	add_cost(%rip), %ecx
	movl	shift_cost(%rsi), %eax
	movslq	shiftadd_cost(%rsi), %rsi
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jle	.LBB12_29
# BB#28:                                # %if.then119
	movq	%rax, %rsi
.LBB12_29:                              # %if.then119
	movq	%rsi, -80(%rbp)         # 8-byte Spill
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%r9
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	subq	%rsi, %rdx
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	synth_mult
	movq	%r13, %rdx
	movswq	(%rdx), %r13
	movq	-80(%rbp), %rax         # 8-byte Reload
	addq	%rax, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jge	.LBB12_38
# BB#30:                                # %if.then139
	movswq	2(%rdx), %rax
	movb	%r15b, 260(%rdx,%rax)
	movswq	2(%rdx), %rax
	movq	$5, %rcx
	jmp	.LBB12_31
.LBB12_35:                              # %if.then167
	movl	add_cost(%rip), %ecx
	movl	shift_cost(%rsi), %eax
	movslq	shiftsub_cost(%rsi), %rsi
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jle	.LBB12_37
# BB#36:                                # %if.then167
	movq	%rax, %rsi
.LBB12_37:                              # %if.then167
	movq	%rsi, -80(%rbp)         # 8-byte Spill
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rbx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	subq	%rsi, %rdx
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	synth_mult
	movq	%r13, %rdx
	movswq	(%rdx), %r13
	movq	-80(%rbp), %rax         # 8-byte Reload
	addq	%rax, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jge	.LBB12_38
# BB#39:                                # %if.then191
	movswq	2(%rdx), %rax
	movb	%r15b, 260(%rdx,%rax)
	movswq	2(%rdx), %rax
	movq	$6, %rcx
.LBB12_31:                              # %for.end205
	movl	%ecx, 4(%rdx,%rax,4)
	movq	%rdx, %rax
	movq	-56(%rbp), %r15         # 8-byte Reload
	jmp	.LBB12_43
.LBB12_38:
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	%rdx, %r15
.LBB12_42:                              # %for.end205
	movq	-48(%rbp), %r13         # 8-byte Reload
.LBB12_43:                              # %for.end205
	movq	-72(%rbp), %rbx         # 8-byte Reload
.LBB12_44:                              # %for.end205
	cmpq	$0, -64(%rbp)           # 8-byte Folded Reload
	je	.LBB12_45
# BB#46:                                # %if.then209
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movq	$1, %rdi
	subq	%r12, %rdi
	andq	%rbx, %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB12_47
# BB#48:                                # %land.lhs.true216
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jge	.LBB12_47
# BB#49:                                # %if.then222
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	shiftadd_cost(,%rdx,4), %edi
	movq	%rdi, -56(%rbp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rax, -64(%rbp)         # 8-byte Spill
	shrq	%cl, %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r13, %rdx
	subq	%rdi, %rdx
	movq	%r15, %rdi
	callq	synth_mult
	movswq	(%r15), %rdx
	movq	-56(%rbp), %rax         # 8-byte Reload
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jge	.LBB12_47
# BB#50:                                # %if.then234
	movq	%rdx, -56(%rbp)         # 8-byte Spill
	movswq	2(%r15), %rax
	movq	-64(%rbp), %rcx         # 8-byte Reload
	movb	%cl, 260(%r15,%rax)
	movswq	2(%r15), %rax
	movq	$7, %rcx
	movl	%ecx, 4(%r15,%rax,4)
	movq	%r15, -64(%rbp)         # 8-byte Spill
	jmp	.LBB12_51
.LBB12_45:
	movq	%rax, %r15
	jmp	.LBB12_56
.LBB12_47:
	movq	-48(%rbp), %rax         # 8-byte Reload
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movq	%r15, -48(%rbp)         # 8-byte Spill
	movq	%r13, -56(%rbp)         # 8-byte Spill
.LBB12_51:                              # %if.end246
	leaq	1(%r12), %r13
	notq	%r12
	andq	%r13, %r12
	movq	%r12, %rdi
	callq	exact_log2_wide
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB12_52
# BB#53:                                # %land.lhs.true253
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB12_52
# BB#54:                                # %if.then259
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	shiftsub_cost(,%rax,4), %esi
	movq	%rsi, -72(%rbp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	shrq	%cl, %r13
	movq	$32, %rcx
	movq	-56(%rbp), %rax         # 8-byte Reload
	movq	%rax, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%rax, %rdx
	subq	%rsi, %rdx
	movq	-48(%rbp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	synth_mult
	movswq	(%r12), %rax
	movq	-72(%rbp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r15, %rdx
	movq	-56(%rbp), %r13         # 8-byte Reload
	movq	-64(%rbp), %r15         # 8-byte Reload
	jge	.LBB12_56
# BB#55:                                # %if.then271
	movswq	2(%r12), %rcx
	movb	%bl, 260(%r12,%rcx)
	movswq	2(%r12), %rcx
	movq	$8, %rdx
	movl	%edx, 4(%r12,%rcx,4)
	movq	%r12, %r15
	movq	%rax, %r13
	jmp	.LBB12_56
.LBB12_52:
	movq	-64(%rbp), %r15         # 8-byte Reload
	movq	-56(%rbp), %r13         # 8-byte Reload
.LBB12_56:                              # %if.end284
	movswq	(%r14), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	je	.LBB12_59
# BB#57:                                # %if.end290
	movzbl	2(%r15), %ebx
	cmpq	$64, %rbx
	je	.LBB12_59
# BB#58:                                # %if.end296
	incq	%rbx
	movw	%bx, 2(%r14)
	movw	%r13w, (%r14)
	leaq	4(%r14), %rdi
	leaq	4(%r15), %rsi
	movq	$48, %rcx
	shlq	%cl, %rbx
	movq	$48, %rcx
	sarq	%cl, %rbx
	movq	$2, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	callq	memcpy
	addq	$260, %r14              # imm = 0x104
	addq	$260, %r15              # imm = 0x104
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy
.LBB12_59:                              # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp176:
	.size	synth_mult, .Ltmp176-synth_mult
	.cfi_endproc

	.globl	ceil_log2
	.align	16, 0x90
	.type	ceil_log2,@function
ceil_log2:                              # @ceil_log2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp178:
	.cfi_def_cfa_offset 16
	decq	%rdi
	callq	floor_log2_wide
	incq	%rax
	popq	%rdx
	retq
.Ltmp179:
	.size	ceil_log2, .Ltmp179-ceil_log2
	.cfi_endproc

	.globl	expand_mult_highpart_adjust
	.align	16, 0x90
	.type	expand_mult_highpart_adjust,@function
expand_mult_highpart_adjust:            # @expand_mult_highpart_adjust
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp186:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp187:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp188:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp189:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp190:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp191:
	.cfi_def_cfa_offset 112
.Ltmp192:
	.cfi_offset %rbx, -48
.Ltmp193:
	.cfi_offset %r12, -40
.Ltmp194:
	.cfi_offset %r13, -32
.Ltmp195:
	.cfi_offset %r14, -24
.Ltmp196:
	.cfi_offset %r15, -16
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r9
	je	.LBB14_1
# BB#2:                                 # %entry
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB14_3
.LBB14_1:
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
.LBB14_3:                               # %entry
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	decq	%rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	56(%rsp), %r12          # 8-byte Reload
	je	.LBB14_11
	.align	16, 0x90
.LBB14_4:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB14_8
# BB#5:                                 # %for.body.i
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB14_7
# BB#6:                                 # %if.then13.i
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	$2, %rax
	jmp	.LBB14_8
	.align	16, 0x90
.LBB14_7:                               # %if.else14.i
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	$3, %rax
.LBB14_8:                               # %if.else79.i
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_binop
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB14_10
# BB#9:                                 # %if.else79.i
                                        #   in Loop: Header=BB14_4 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB14_4
.LBB14_10:                              # %for.end.i
	testq	%r12, %r12
	je	.LBB14_29
.LBB14_11:                              # %expand_shift.exit
	movzbl	2(%r12), %eax
	movq	48(%rsp), %rbx          # 8-byte Reload
	xorq	$1, %rbx
	addq	$75, %rbx
	testq	%rax, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	jne	.LBB14_14
# BB#12:                                # %land.lhs.true.i
	movzbl	2(%r14), %eax
	testq	%rax, %rax
	jne	.LBB14_14
# BB#13:                                # %if.end.i
	movq	$83, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB14_15
.LBB14_14:                              # %if.then6.i
	movq	optab_table+144(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	expand_binop
.LBB14_15:                              # %expand_and.exit
	movq	%r14, %r12
	movq	%rbx, %rdi
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r15, %rsi
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	force_operand
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB14_16
	.align	16, 0x90
.LBB14_17:                              # %for.body.i40
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB14_21
# BB#18:                                # %for.body.i40
                                        #   in Loop: Header=BB14_17 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB14_20
# BB#19:                                # %if.then13.i41
                                        #   in Loop: Header=BB14_17 Depth=1
	movq	$2, %rax
	jmp	.LBB14_21
	.align	16, 0x90
.LBB14_20:                              # %if.else14.i42
                                        #   in Loop: Header=BB14_17 Depth=1
	movq	$3, %rax
.LBB14_21:                              # %if.else79.i44
                                        #   in Loop: Header=BB14_17 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB14_23
# BB#22:                                # %if.else79.i44
                                        #   in Loop: Header=BB14_17 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB14_17
.LBB14_23:                              # %for.end.i51
	testq	%rax, %rax
	movq	%rax, %rbx
	movq	48(%rsp), %r14          # 8-byte Reload
	jne	.LBB14_24
.LBB14_29:                              # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB14_16:
	movq	48(%rsp), %r14          # 8-byte Reload
	movq	%r12, %rbx
.LBB14_24:                              # %expand_shift.exit54
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB14_27
# BB#25:                                # %land.lhs.true.i61
	movq	56(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	jne	.LBB14_27
# BB#26:                                # %if.end.i64
	movq	$83, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB14_28
.LBB14_27:                              # %if.then6.i66
	movq	optab_table+144(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	expand_binop
.LBB14_28:                              # %expand_and.exit68
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	force_operand  # TAILCALL
.Ltmp197:
	.size	expand_mult_highpart_adjust, .Ltmp197-expand_mult_highpart_adjust
	.cfi_endproc

	.globl	expand_and
	.align	16, 0x90
	.type	expand_and,@function
expand_and:                             # @expand_and
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp205:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp206:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp207:
	.cfi_def_cfa_offset 48
.Ltmp208:
	.cfi_offset %rbx, -40
.Ltmp209:
	.cfi_offset %r12, -32
.Ltmp210:
	.cfi_offset %r14, -24
.Ltmp211:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	2(%r14), %eax
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rdi, %r12
	testq	%rax, %rax
	jne	.LBB15_3
# BB#1:                                 # %land.lhs.true
	movzbl	2(%r15), %eax
	testq	%rax, %rax
	jne	.LBB15_3
# BB#2:                                 # %if.end
	movq	$83, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB15_4
.LBB15_3:                               # %if.then6
	movq	optab_table+144(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	expand_binop
.LBB15_4:                               # %if.end8
	testq	%rbx, %rbx
	je	.LBB15_5
# BB#6:                                 # %if.else
	cmpq	%rbx, %rax
	je	.LBB15_8
# BB#7:                                 # %if.then12
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	jmp	.LBB15_8
.LBB15_5:
	movq	%rax, %rbx
.LBB15_8:                               # %if.end15
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp212:
	.size	expand_and, .Ltmp212-expand_and
	.cfi_endproc

	.globl	expand_mult_highpart
	.align	16, 0x90
	.type	expand_mult_highpart,@function
expand_mult_highpart:                   # @expand_mult_highpart
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp219:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp220:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp221:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp222:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp223:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp224:
	.cfi_def_cfa_offset 128
.Ltmp225:
	.cfi_offset %rbx, -48
.Ltmp226:
	.cfi_offset %r12, -40
.Ltmp227:
	.cfi_offset %r13, -32
.Ltmp228:
	.cfi_offset %r14, -24
.Ltmp229:
	.cfi_offset %r15, -16
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdi, %rax
	movq	$32, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_bitsize(%r12,%r12), %ecx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%r8, %r13
	movq	%rdx, %rbx
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	cmpq	$65, %rcx
	jae	.LBB16_47
# BB#1:                                 # %if.end
	movzbl	mode_wider_mode(%r12), %r15d
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$63, %rcx
	movq	%rbx, %rax
	sarq	%cl, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r13
	movq	%r13, 56(%rsp)          # 8-byte Spill
	jne	.LBB16_3
# BB#2:                                 # %if.end
	movq	%rax, %r14
.LBB16_3:                               # %if.end
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rbx
	movq	%rbx, %rdx
	callq	immed_double_const
	movl	target_flags(%rip), %edx
	movq	%rax, %r14
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %r9        # imm = 0xFFFFFFE0
	andq	%r9, %rdx
	movq	40(%rsp), %r13          # 8-byte Reload
	cmpq	%rdx, %r13
	jae	.LBB16_6
# BB#4:                                 # %land.lhs.true
	movl	mul_cost(,%rbx,4), %eax
	movl	shift_cost-4(,%r13,4), %edx
	addq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB16_6
# BB#5:                                 # %if.then18
	movq	%rbx, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	callq	convert_to_mode
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	expand_mult
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
.LBB16_45:                              # %if.else
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_modes  # TAILCALL
.LBB16_6:                               # %if.end25
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	32(%rsp), %r8           # 8-byte Reload
	testq	%r8, %r8
	jne	.LBB16_8
# BB#7:                                 # %if.then28
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r9, %rbx
	callq	gen_reg_rtx
	movq	%rbx, %r9
	movq	%rax, %r8
.LBB16_8:                               # %if.end30
	movslq	mul_highpart_cost(,%r12,4), %rax
	movq	$32, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB16_9
# BB#10:                                # %if.then35
	movq	%r9, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r9           # 8-byte Reload
	testq	%rax, %r9
	movq	64(%rsp), %rdi          # 8-byte Reload
	jne	.LBB16_11
# BB#12:                                # %if.then35
	movabsq	$optab_table+48, %rax
	jmp	.LBB16_13
.LBB16_9:
	movq	56(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_14
.LBB16_11:
	movabsq	$optab_table+56, %rax
.LBB16_13:                              # %if.then35
	movq	(%rax), %rsi
	movq	$0, (%rsp)
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r9, %rbx
	callq	expand_binop
	movq	%rax, %r15
	xorq	%r8, %r8
	testq	%r15, %r15
	movq	%r14, %r9
	jne	.LBB16_46
.LBB16_14:                              # %if.end45
	movl	target_flags(%rip), %eax
	leaq	-1(%r13), %r10
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	andq	%r9, %rax
	cmpq	%rax, %r13
	ja	.LBB16_15
# BB#24:                                # %land.lhs.true52
	movl	mul_highpart_cost(,%r12,4), %eax
	movl	shift_cost(,%r10,4), %ecx
	movl	add_cost(%rip), %edx
	leaq	(%rax,%rcx,2), %rax
	leaq	(%rax,%rdx,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB16_15
# BB#25:                                # %if.then63
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r10, %r15
	movq	%r9, %r14
	jne	.LBB16_26
# BB#27:                                # %if.then63
	movabsq	$optab_table+56, %rax
	jmp	.LBB16_28
.LBB16_15:
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB16_16:                              # %if.end74
	movq	optab_table+72(%rip), %rax
	movq	optab_table+64(%rip), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	movq	%rax, %r14
	jne	.LBB16_18
# BB#17:                                # %if.end74
	movq	%rdi, %r14
.LBB16_18:                              # %if.end74
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	8(%r14,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB16_31
# BB#19:                                # %land.lhs.true84
	movslq	mul_widen_cost(,%r12,4), %rdx
	movq	$32, %rcx
	movq	72(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB16_31
# BB#20:                                # %if.then89
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	force_reg
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB16_21
.LBB16_31:                              # %if.end91
	movq	optab_table+32(%rip), %r14
	movq	$4, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movl	8(%r14,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB16_34
# BB#32:                                # %land.lhs.true98
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	andq	%r9, %rdx
	cmpq	%rdx, %r13
	ja	.LBB16_34
# BB#33:                                # %land.lhs.true105
	movl	mul_cost(,%r12,4), %ecx
	movl	shift_cost(,%r10,4), %edx
	addq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	72(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB16_34
.LBB16_21:                              # %try
	movq	$2, (%rsp)
	xorq	%r8, %r8
	movq	%rbx, %r9
	xorq	%r15, %r15
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%r9, %r14
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB16_46
# BB#22:                                # %if.end164
	movl	word_mode(%rip), %eax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	%rdx, %rax
	jne	.LBB16_44
# BB#23:                                # %if.then167
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_highpart  # TAILCALL
.LBB16_34:                              # %if.end115
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	jne	.LBB16_36
# BB#35:                                # %if.end115
	movq	%rax, %rdi
.LBB16_36:                              # %if.end115
	movq	$4, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movl	8(%rdi,%rax), %eax
	xorq	%r15, %r15
	cmpq	$1317, %rax             # imm = 0x525
	movq	%r8, %rsi
	je	.LBB16_46
# BB#37:                                # %land.lhs.true127
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	andq	%r9, %rax
	cmpq	%rax, %r13
	ja	.LBB16_46
# BB#38:                                # %land.lhs.true134
	movq	%rdi, %r14
	movl	mul_widen_cost(,%r12,4), %eax
	movl	shift_cost(,%r10,4), %ecx
	movl	add_cost(%rip), %edx
	leaq	(%rax,%rcx,2), %rax
	leaq	(%rax,%rdx,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB16_46
# BB#39:                                # %if.then146
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	force_reg
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	jne	.LBB16_40
# BB#41:                                # %if.then146
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movabsq	$1, %r9
	jmp	.LBB16_42
.LBB16_26:
	movabsq	$optab_table+48, %rax
.LBB16_28:                              # %if.then63
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	(%rax), %rsi
	movq	$0, (%rsp)
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	xorq	%r8, %r8
	testq	%rax, %rax
	movq	%r14, %r9
	movq	%r15, %r10
	je	.LBB16_16
# BB#29:                                # %if.then71
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, %r8
	movq	%rbx, %r9
	jmp	.LBB16_30
.LBB16_44:                              # %if.else
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%rcx, %r15
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	jmp	.LBB16_45
.LBB16_40:
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movabsq	$0, %r9
.LBB16_42:                              # %if.then146
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	$2, (%rsp)
	xorq	%r8, %r8
	xorq	%r15, %r15
	movq	%rdi, %rbx
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_binop
	movq	%rax, %r12
	testq	%r12, %r12
	movq	32(%rsp), %r14          # 8-byte Reload
	je	.LBB16_46
# BB#43:                                # %if.then152
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%r14, %r13
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	callq	convert_modes
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r13, %r8
	movq	%r14, %r9
.LBB16_30:                              # %if.then71
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_mult_highpart_adjust  # TAILCALL
.LBB16_46:                              # %return
	movq	%r15, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_47:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_mult_highpart, %rcx
	movq	$2790, %rsi             # imm = 0xAE6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp230:
	.size	expand_mult_highpart, .Ltmp230-expand_mult_highpart
	.cfi_endproc

	.globl	expand_divmod
	.align	16, 0x90
	.type	expand_divmod,@function
expand_divmod:                          # @expand_divmod
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
	subq	$208, %rsp
.Ltmp242:
	.cfi_def_cfa_offset 256
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
	movq	%r8, %rbx
	movzbl	(%rbx), %r12d
	movq	256(%rsp), %r10
	xorq	%r11, %r11
	cmpq	$54, %r12
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rdi, %r13
	jne	.LBB17_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB17_3
.LBB17_1:
	movabsq	$0, %rax
.LBB17_3:                               # %entry
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	jne	.LBB17_9
# BB#4:                                 # %land.rhs
	movq	8(%rbx), %rax
	leaq	-1(%rax), %rcx
	testq	%rax, %rcx
	movq	$1, %r11
	je	.LBB17_9
# BB#5:                                 # %lor.rhs
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r10
	movabsq	$0, %r11
	jne	.LBB17_9
# BB#6:                                 # %land.rhs7
	movq	%rax, %rcx
	negq	%rcx
	notq	%rax
	testq	%rax, %rcx
	jne	.LBB17_7
# BB#8:                                 # %land.rhs7
	movabsq	$1, %r11
	jmp	.LBB17_9
.LBB17_7:
	movabsq	$0, %r11
.LBB17_9:                               # %land.end20
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB17_10
# BB#11:                                # %if.end
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %r10
	jne	.LBB17_12
# BB#13:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB17_14
.LBB17_10:                              # %if.then
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB17_17
	jmp	.LBB17_18
.LBB17_12:
	movabsq	$0, %rax
.LBB17_14:                              # %if.end
	movq	const_int_rtx+504(%rip), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB17_24
# BB#15:                                # %if.end
	testq	%rax, %rax
	je	.LBB17_24
# BB#16:                                # %if.then27
	testq	%r8, %r13
	je	.LBB17_19
.LBB17_17:                              # %if.then29
	movq	const_int_rtx+512(%rip), %r15
.LBB17_18:                              # %return
	movq	%r15, %rax
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_24:                              # %if.end40
	testq	%r9, %r9
	je	.LBB17_34
# BB#25:                                # %land.lhs.true42
	movl	(%r9), %eax
	andq	$1073807359, %rax       # imm = 0x4000FFFF
	cmpq	$1073741885, %rax       # imm = 0x4000003D
	je	.LBB17_34
# BB#26:                                # %lor.lhs.false
	testq	%r8, %r13
	jne	.LBB17_28
# BB#27:                                # %lor.lhs.false
	cmpq	$54, %r12
	jne	.LBB17_31
.LBB17_28:                              # %land.lhs.true54
	movq	%r9, %rdi
	movq	%r15, %rsi
	movq	%r9, 128(%rsp)          # 8-byte Spill
	movq	%r11, 120(%rsp)         # 8-byte Spill
	callq	reg_mentioned_p
	movq	120(%rsp), %r11         # 8-byte Reload
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	256(%rsp), %r10
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	jne	.LBB17_34
# BB#29:                                # %lor.lhs.false57
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB17_31
# BB#30:                                # %land.lhs.true62
	movzbl	(%r9), %eax
	cmpq	$66, %rax
	je	.LBB17_34
.LBB17_31:                              # %lor.lhs.false67
	movq	%r11, 120(%rsp)         # 8-byte Spill
	movq	%r9, 128(%rsp)          # 8-byte Spill
	movq	%r9, %rdi
	movq	%rbx, %rsi
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	%r10, %rbx
	callq	reg_mentioned_p
	movq	120(%rsp), %r11         # 8-byte Reload
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	%rbx, %r10
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	112(%rsp), %rbx         # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB17_34
# BB#32:                                # %lor.lhs.false70
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	jne	.LBB17_35
# BB#33:                                # %land.lhs.true75
	movzbl	(%r9), %eax
	cmpq	$66, %rax
	jne	.LBB17_35
.LBB17_34:                              # %if.then80
	xorq	%r9, %r9
.LBB17_35:                              # %if.end81
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB17_41
# BB#36:                                # %if.end81
	movq	%r11, %rax
	xorq	$1, %rax
	jne	.LBB17_41
# BB#37:                                # %cond.end98.thread2789
	testq	%r8, %r10
	jne	.LBB17_38
# BB#39:                                # %cond.end98.thread2789
	movabsq	$optab_table+184, %rax
	jmp	.LBB17_40
.LBB17_41:                              # %cond.false105
	movq	%r10, %rax
	andq	%r8, %rax
	jne	.LBB17_42
# BB#43:                                # %cond.false105
	movabsq	$optab_table+96, %rcx
	jmp	.LBB17_44
.LBB17_19:                              # %if.end30
	movl	flag_trapv(%rip), %eax
	testq	%rax, %rax
	je	.LBB17_22
# BB#20:                                # %land.lhs.true32
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB17_22
# BB#21:
	movabsq	$optab_table+264, %rax
	jmp	.LBB17_23
.LBB17_22:                              # %cond.false37
	movabsq	$optab_table+256, %rax
.LBB17_23:                              # %cond.end38
	movq	(%rax), %rsi
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%r9, %rcx
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_unop  # TAILCALL
.LBB17_38:
	movabsq	$optab_table+176, %rax
.LBB17_40:                              # %cond.end98.thread2789
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movq	%rax, %rbx
	jmp	.LBB17_48
.LBB17_42:
	movabsq	$optab_table+112, %rcx
.LBB17_44:                              # %cond.false105
	testq	%rax, %rax
	jne	.LBB17_45
# BB#46:                                # %cond.false105
	movabsq	$optab_table+80, %rax
	jmp	.LBB17_47
.LBB17_45:
	movabsq	$optab_table+104, %rax
.LBB17_47:                              # %cond.false105
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movq	(%rcx), %rbx
.LBB17_48:                              # %cond.end111
	testq	%r8, %r14
	je	.LBB17_58
# BB#49:
	movq	%r14, %rdx
	.align	16, 0x90
.LBB17_50:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rax,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB17_59
# BB#51:                                # %lor.lhs.false119
                                        #   in Loop: Header=BB17_50 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rbx,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB17_59
# BB#52:                                # %for.inc
                                        #   in Loop: Header=BB17_50 Depth=1
	movzbl	mode_wider_mode(%rsi), %edx
	testq	%rdx, %rdx
	jne	.LBB17_50
# BB#53:                                # %for.cond134.preheader
	testq	%r8, %r14
	je	.LBB17_58
# BB#54:
	movq	%r14, %rdx
	.align	16, 0x90
.LBB17_55:                              # %for.body137
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	cmpq	$0, 16(%rax,%rdi)
	jne	.LBB17_59
# BB#56:                                # %lor.lhs.false142
                                        #   in Loop: Header=BB17_55 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	cmpq	$0, 16(%rbx,%rdi)
	jne	.LBB17_59
# BB#57:                                # %for.inc150
                                        #   in Loop: Header=BB17_55 Depth=1
	movzbl	mode_wider_mode(%rsi), %edx
	testq	%rdx, %rdx
	jne	.LBB17_55
.LBB17_58:
	xorq	%rdx, %rdx
.LBB17_59:                              # %if.end155
	movq	%r11, 120(%rsp)         # 8-byte Spill
	movq	%r15, 152(%rsp)         # 8-byte Spill
	testq	%r8, %rdx
	movq	%r14, %rax
	je	.LBB17_61
# BB#60:                                # %if.end155
	movq	%rdx, %rax
.LBB17_61:                              # %if.end155
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r14, 88(%rsp)          # 8-byte Spill
	testq	%r9, %r9
	movq	%rax, %r15
	je	.LBB17_63
# BB#62:                                # %land.lhs.true161
	movzbl	2(%r9), %eax
	movq	%r15, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	movq	%r9, %rax
	je	.LBB17_64
.LBB17_63:                              # %if.else
	movq	%r15, %rdi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	gen_reg_rtx
	movq	%rbx, %r9
	movq	%r14, %r8
.LBB17_64:                              # %if.end169
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movl	div_cost(,%rdx,4), %ecx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	testq	%r8, %r13
	movq	%r13, 144(%rsp)         # 8-byte Spill
	movabsq	$0, %rdx
	movq	112(%rsp), %rdi         # 8-byte Reload
	je	.LBB17_69
# BB#65:                                # %land.lhs.true176
	movq	expand_divmod.last_div_const(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB17_68
# BB#66:                                # %land.lhs.true176
	cmpq	$54, %r12
	jne	.LBB17_68
# BB#67:                                # %land.lhs.true181
	movq	8(%rdi), %rdx
	cmpq	%rcx, %rdx
	movabsq	$0, %rdx
	je	.LBB17_69
.LBB17_68:                              # %cond.true187
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mul_cost(,%rcx,4), %ecx
	movl	add_cost(%rip), %edx
	addq	%rcx, %rdx
.LBB17_69:                              # %cond.end191
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	144(%rsp), %rcx         # 8-byte Reload
	testq	%r8, %rcx
	jne	.LBB17_70
# BB#71:                                # %cond.end191
	cmpq	$54, %r12
	movq	256(%rsp), %rcx
	movq	120(%rsp), %r10         # 8-byte Reload
	jne	.LBB17_73
# BB#72:                                # %cond.true197
	movq	8(%rdi), %rsi
	jmp	.LBB17_73
.LBB17_70:
	movq	256(%rsp), %rcx
	movq	120(%rsp), %r10         # 8-byte Reload
.LBB17_73:                              # %cond.end202
	movq	%rdi, %r12
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rsi, expand_divmod.last_div_const(%rip)
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rax
	andq	%r8, %rax
	movq	%r15, %r13
	movq	%r13, %rdx
	andq	%r8, %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	%rax, %rdx
	je	.LBB17_87
# BB#74:                                # %if.then206
	movq	%r13, %rax
	movq	%r9, 128(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	movq	%r8, %r13
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rcx, %r14
	callq	convert_modes
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	convert_modes
	movzbl	(%rax), %ecx
	xorq	%r10, %r10
	cmpq	$54, %rcx
	jne	.LBB17_75
# BB#76:                                # %if.then206
	movabsq	$1, %rcx
	jmp	.LBB17_77
.LBB17_75:
	movabsq	$0, %rcx
.LBB17_77:                              # %if.then206
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	%r13, %r8
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	%r15, %r13
	movq	%r14, %rcx
	jne	.LBB17_86
# BB#78:                                # %land.rhs214
	movq	8(%rax), %rsi
	leaq	-1(%rsi), %rdx
	andq	%rsi, %rdx
	jne	.LBB17_79
# BB#80:                                # %land.rhs214
	movabsq	$1, %r10
	jmp	.LBB17_81
.LBB17_79:
	movabsq	$0, %r10
.LBB17_81:                              # %land.rhs214
	testq	%rdx, %rdx
	je	.LBB17_86
# BB#82:                                # %land.rhs214
	testq	%r8, %rcx
	jne	.LBB17_86
# BB#83:                                # %land.rhs227
	movq	%rsi, %rdx
	negq	%rdx
	notq	%rsi
	testq	%rsi, %rdx
	jne	.LBB17_84
# BB#85:                                # %land.rhs227
	movabsq	$1, %r10
	jmp	.LBB17_86
.LBB17_84:
	movabsq	$0, %r10
.LBB17_86:                              # %land.end243
	movq	%rax, %r12
.LBB17_87:                              # %if.end245
	movq	152(%rsp), %r15         # 8-byte Reload
	movl	(%r15), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$134217794, %rax        # imm = 0x8000042
	jne	.LBB17_88
# BB#89:                                # %if.then255
	movq	%r10, 120(%rsp)         # 8-byte Spill
	movq	%r9, 128(%rsp)          # 8-byte Spill
	movq	%rcx, %r14
	movq	%r8, %rbx
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
	jmp	.LBB17_90
.LBB17_88:
	movq	%r10, 120(%rsp)         # 8-byte Spill
	movq	%r9, 128(%rsp)          # 8-byte Spill
	movq	%rcx, %r14
	movq	%r8, %rbx
.LBB17_90:                              # %if.end257
	movq	%r12, %rax
	movl	(%rax), %ecx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$134217794, %rcx        # imm = 0x8000042
	jne	.LBB17_92
# BB#91:                                # %if.then267
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	force_reg
.LBB17_92:                              # %if.end269
	movq	%rax, %r12
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	144(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	testq	%rbx, %rax
	je	.LBB17_94
# BB#93:                                # %if.then273
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
.LBB17_94:                              # %if.end275
	callq	get_last_insn
	movq	%rax, %r8
	movq	%r14, %rsi
	testq	%rbx, %rsi
	je	.LBB17_95
# BB#96:                                # %if.then278
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	movq	$62, %rax
	movq	$62, %rdx
	je	.LBB17_98
# BB#97:                                # %if.then278
	movq	%rcx, %rdx
.LBB17_98:                              # %if.then278
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movq	%rdx, %rcx
	andq	%rbx, %rcx
	cmpq	$68, %rcx
	movq	$66, %rcx
	je	.LBB17_100
# BB#99:                                # %if.then278
	movq	%rdx, %rcx
.LBB17_100:                             # %if.then278
	movq	%rcx, %rdx
	andq	%rbx, %rdx
	cmpq	$71, %rdx
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	%r12, %r11
	jne	.LBB17_101
# BB#102:                               # %if.then278
	movabsq	$1, %rdx
	jmp	.LBB17_103
.LBB17_95:
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	%r12, %r11
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB17_106
.LBB17_101:
	movabsq	$0, %rdx
.LBB17_103:                             # %if.then278
	movq	120(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdx
	jne	.LBB17_105
# BB#104:                               # %if.then278
	movq	%rcx, %rax
.LBB17_105:                             # %if.then278
	movq	%rax, %rcx
	movq	72(%rsp), %r12          # 8-byte Reload
.LBB17_106:                             # %if.end293
	movq	const_int_rtx+512(%rip), %rax
	xorq	%r13, %r13
	cmpq	%rax, %r11
	je	.LBB17_107
# BB#108:                               # %if.then296
	addq	$-62, %rcx
	andq	%rbx, %rcx
	cmpq	$9, %rcx
	ja	.LBB17_530
# BB#109:                               # %if.then296
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	mode_bitsize(%rax,%rax), %eax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r12
	jmpq	*.LJTI17_0(,%rcx,8)
.LBB17_110:                             # %sw.bb
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB17_111
# BB#125:                               # %if.then298
	movq	8(%r11), %r14
	testq	%rbx, %rsi
	je	.LBB17_165
# BB#126:                               # %if.then300
	movq	%r15, 152(%rsp)         # 8-byte Spill
	leaq	-1(%r14), %rax
	testq	%r14, %rax
	je	.LBB17_127
# BB#132:                               # %if.else323
	movq	120(%rsp), %rdx         # 8-byte Reload
	cmpq	$64, %rdx
	ja	.LBB17_133
# BB#134:                               # %if.then326
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r10, 128(%rsp)         # 8-byte Spill
	leaq	-1(%rdx), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	cmpq	%rax, %r14
	jae	.LBB17_135
# BB#138:                               # %if.else338
	movq	%rdx, %rsi
	movq	%r11, %r15
	leaq	200(%rsp), %rcx
	leaq	196(%rsp), %r8
	leaq	192(%rsp), %r9
	movq	%r14, %rdi
                                        # kill: RDX<def> RSI<kill>
	movq	%rsi, %r12
	callq	choose_multiplier
	testq	%rax, %rax
	je	.LBB17_143
# BB#139:                               # %if.else338
	movq	%r14, %rcx
	andq	$1, %rcx
	jne	.LBB17_143
# BB#140:                               # %if.then346
	movq	%r14, %rdi
	negq	%rdi
	andq	%r14, %rdi
	callq	floor_log2_wide
	movq	%rax, %rbx
	movq	%r12, %rsi
	movq	%rsi, %rdx
	subq	%rbx, %rdx
	movq	%rbx, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	shrq	%cl, %r14
	leaq	200(%rsp), %rcx
	leaq	196(%rsp), %r8
	leaq	192(%rsp), %r9
	movq	%r14, %rdi
	callq	choose_multiplier
	testq	%rax, %rax
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB17_148
# BB#141:                               # %if.then354
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_divmod, %rcx
	movq	$3200, %rsi             # imm = 0xC80
	jmp	.LBB17_142
.LBB17_383:                             # %sw.bb1014
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	96(%rsp), %r12          # 8-byte Reload
	movq	%r11, 112(%rsp)         # 8-byte Spill
	testq	%rbx, %rsi
	je	.LBB17_416
# BB#384:                               # %if.then1016
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	144(%rsp), %r14         # 8-byte Reload
	je	.LBB17_392
# BB#385:                               # %land.lhs.true1018
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	leaq	-1(%rdi), %r15
	testq	%rdi, %r15
	je	.LBB17_386
.LBB17_392:                             # %if.end1055
	testq	%r10, %r10
	je	.LBB17_394
# BB#393:                               # %lor.lhs.false1058
	movl	(%r10), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rdx
	je	.LBB17_395
.LBB17_394:                             # %if.then1064
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r10
	movl	(%r10), %eax
.LBB17_395:                             # %if.end1066
	andq	$65535, %rax            # imm = 0xFFFF
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB17_399
# BB#396:                               # %if.then1068
	cmpq	$61, %rax
	movq	%r10, %r13
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r12, %rbx
	je	.LBB17_398
# BB#397:                               # %cond.false1074
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB17_398:                             # %cond.end1076
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	jmp	.LBB17_402
.LBB17_292:                             # %sw.bb811
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	%r11, 112(%rsp)         # 8-byte Spill
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	120(%rsp), %rax         # 8-byte Reload
	je	.LBB17_333
# BB#293:                               # %sw.bb811
	cmpq	$64, %rax
	ja	.LBB17_333
# BB#294:                               # %if.then816
	movq	%rbx, %r13
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	jle	.LBB17_321
# BB#295:                               # %if.then828
	movq	%r8, 32(%rsp)           # 8-byte Spill
	leaq	-1(%rdi), %rax
	testq	%rdi, %rax
	je	.LBB17_296
# BB#307:                               # %if.else850
	movq	120(%rsp), %rbx         # 8-byte Reload
	leaq	-1(%rbx), %r14
	leaq	168(%rsp), %rcx
	leaq	160(%rsp), %r8
	leaq	164(%rsp), %r9
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	choose_multiplier
	testq	%rax, %rax
	jne	.LBB17_308
# BB#309:                               # %if.end859
	movl	target_flags(%rip), %eax
	movslq	160(%rsp), %rdx
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	cmpq	%rax, %rdx
	movq	%r13, %rcx
	movq	32(%rsp), %r8           # 8-byte Reload
	jge	.LBB17_333
# BB#310:                               # %if.end859
	andq	%rcx, %rax
	cmpq	%rax, %rbx
	ja	.LBB17_333
# BB#311:                               # %if.then872
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	96(%rsp), %r13          # 8-byte Reload
	movq	%rcx, %r12
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%r14, 120(%rsp)         # 8-byte Spill
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	152(%rsp), %r15         # 8-byte Reload
	je	.LBB17_319
.LBB17_312:                             # %for.body.i2358
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %rbx
	movabsq	$0, %rax
	je	.LBB17_316
# BB#313:                               # %for.body.i2358
                                        #   in Loop: Header=BB17_312 Depth=1
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB17_315
# BB#314:                               # %if.then13.i2359
                                        #   in Loop: Header=BB17_312 Depth=1
	movq	$2, %rax
	jmp	.LBB17_316
.LBB17_315:                             # %if.else14.i2360
                                        #   in Loop: Header=BB17_312 Depth=1
	movq	$3, %rax
.LBB17_316:                             # %if.else79.i2362
                                        #   in Loop: Header=BB17_312 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB17_318
# BB#317:                               # %if.else79.i2362
                                        #   in Loop: Header=BB17_312 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB17_312
.LBB17_318:                             # %for.end.i2369
	testq	%r15, %r15
	je	.LBB17_204
.LBB17_319:                             # %expand_shift.exit2372
	movq	optab_table+160(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	expand_binop
	movq	136(%rsp), %rcx         # 8-byte Reload
	movl	shift_cost(,%rcx,4), %ecx
	movq	120(%rsp), %rdx         # 8-byte Reload
	movl	shift_cost(,%rdx,4), %esi
	movl	add_cost(%rip), %edi
	addq	%rdi, %rdi
	movq	168(%rsp), %rdx
	movq	72(%rsp), %r9           # 8-byte Reload
	subq	%rcx, %r9
	subq	%rsi, %r9
	subq	%rdi, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_mult_highpart
	movq	%rax, %r14
	testq	%r14, %r14
	movq	%r12, %rax
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB17_333
# BB#320:                               # %if.then890
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	optab_table+160(%rip), %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%rbx, %rdi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	jmp	.LBB17_331
.LBB17_489:                             # %sw.bb1299
	movq	%r15, %r13
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movq	%rbx, %r14
	movq	%rsi, %r12
	callq	gen_label_rtx
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	testq	%r14, %r12
	je	.LBB17_500
# BB#490:                               # %if.then1301
	movq	optab_table+112(%rip), %rdi
	movq	$1, %r9
	movq	%r13, %r12
	movq	%r12, %rsi
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	movq	%rax, %r8
	movq	%rax, %rbx
	callq	expand_twoval_binop
	testq	%r14, %rax
	je	.LBB17_492
# BB#491:
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movq	%r14, %r13
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	96(%rsp), %r14          # 8-byte Reload
	jmp	.LBB17_493
.LBB17_477:                             # %sw.bb1268
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB17_107
# BB#478:                               # %sw.bb1268
	movq	120(%rsp), %r12         # 8-byte Reload
	cmpq	$64, %r12
	ja	.LBB17_107
# BB#479:                               # %if.then1273
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	%r11, %r14
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	8(%r14), %r14
	movq	%r14, %rdi
	negq	%rdi
	andq	%r14, %rdi
	callq	floor_log2_wide
	movq	%rax, %rcx
	movq	%rbx, %r8
	andq	%r8, %rcx
	sarq	%cl, %r14
	movq	$-1, %rdx
	cmpq	$64, %r12
	movq	256(%rsp), %r9
	movq	%r12, %r10
	je	.LBB17_483
# BB#480:                               # %cond.end.i
	movq	$1, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdx
	cmpq	$4, %r10
	jb	.LBB17_481
# BB#482:
	decq	%rdx
.LBB17_483:                             # %while.body.lr.ph.i
	movq	$3, %rsi
	movq	%r14, %rcx
	.align	16, 0x90
.LBB17_484:                             # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdi
	imulq	%r14, %rdi
	movq	$2, %rbx
	subq	%rdi, %rbx
	imulq	%rcx, %rbx
	andq	%rdx, %rbx
	movq	$33, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rsi, %rsi
	cmpq	%r10, %rdi
	movq	%rbx, %rcx
	jl	.LBB17_484
	jmp	.LBB17_485
.LBB17_107:
	movq	%r15, 152(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
.LBB17_113:                             # %if.then1359
	testq	%r10, %r10
	je	.LBB17_117
# BB#114:                               # %land.lhs.true1361
	movzbl	2(%r10), %eax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB17_116
# BB#115:                               # %land.lhs.true1361
	xorq	%r10, %r10
.LBB17_116:                             # %land.lhs.true1361
	movq	%r10, %r13
.LBB17_117:                             # %if.end1368
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movq	%r11, %r12
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB17_118
# BB#531:                               # %if.then1370
	movq	optab_table+128(%rip), %rsi
	movq	$4, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	8(%rcx,%rax), %ecx
	movq	%rax, %r14
	movq	optab_table+120(%rip), %rdx
	xorq	%rax, %rax
	cmpq	$1317, %rcx             # imm = 0x525
	movq	%r12, %r8
	jne	.LBB17_533
# BB#532:                               # %select.mid
	movq	$2, %rax
.LBB17_533:                             # %select.end
	movq	%rax, 8(%rsp)
	movq	256(%rsp), %rax
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	%r8, %r12
	movq	%r13, %r9
	callq	sign_expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB17_539
# BB#534:                               # %if.then1381
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	8(%rax,%r14), %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r15
	movq	256(%rsp), %r9
	testq	%rbx, %r9
	jne	.LBB17_535
# BB#536:                               # %if.then1381
	movabsq	$optab_table+96, %rax
	jmp	.LBB17_537
.LBB17_118:                             # %if.end1368.if.end1397_crit_edge
	movq	$4, %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	leaq	8(%rcx,%rax), %r14
.LBB17_119:                             # %if.end1397
	movq	optab_table+104(%rip), %rsi
	movq	optab_table+80(%rip), %rdx
	xorq	%rdi, %rdi
	movq	144(%rsp), %rcx         # 8-byte Reload
	testq	%rbx, %rcx
	movabsq	$0, %r9
	jne	.LBB17_121
# BB#120:                               # %if.end1397
	movq	%r13, %r9
.LBB17_121:                             # %if.end1397
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movl	(%r14), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	256(%rsp), %rax
	jne	.LBB17_123
# BB#122:                               # %select.mid6
	movq	$2, %rdi
.LBB17_123:                             # %select.end5
	movq	%rdi, 8(%rsp)
	movq	%rax, (%rsp)
	movq	96(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%r12, %r15
	movq	%r15, %r8
	movq	%rax, %r12
	callq	sign_expand_binop
	testq	%rax, %rax
	movq	%rax, %r14
	je	.LBB17_540
# BB#124:
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	136(%rsp), %r12         # 8-byte Reload
	movq	%r15, %r11
	jmp	.LBB17_548
.LBB17_540:                             # %if.then1413
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
	testq	%rbx, %r12
	movq	%r12, %r9
	jne	.LBB17_541
# BB#542:                               # %if.then1413
	movabsq	$optab_table+96, %rax
	jmp	.LBB17_543
.LBB17_535:
	movabsq	$optab_table+112, %rax
.LBB17_537:                             # %if.then1381
	movq	(%rax), %rdi
	xorq	%rcx, %rcx
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%r15, %r8
	callq	expand_twoval_binop
	testq	%rbx, %rax
	je	.LBB17_119
# BB#538:                               # %if.then1381
	testq	%r15, %r15
	je	.LBB17_119
.LBB17_539:                             # %if.then1394
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	jmp	.LBB17_130
.LBB17_541:
	movabsq	$optab_table+112, %rax
.LBB17_543:                             # %if.then1413
	movq	(%rax), %rdi
	xorq	%r8, %r8
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	expand_twoval_binop
	movq	%rax, %rdx
	movq	144(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rdx
	andq	%rbx, %rax
	movq	%rax, %r14
	movq	136(%rsp), %r12         # 8-byte Reload
	je	.LBB17_545
# BB#544:                               # %if.then1413
	movq	%r15, %r14
.LBB17_545:                             # %if.then1413
	testq	%rbx, %rdx
	je	.LBB17_547
# BB#546:
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	112(%rsp), %r11         # 8-byte Reload
	jmp	.LBB17_548
.LBB17_111:
	movq	%r11, %r14
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	%r15, 152(%rsp)         # 8-byte Spill
.LBB17_112:                             # %fail1
	movq	%r8, %rdi
	callq	delete_insns_since
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	%r14, %r11
	jmp	.LBB17_113
.LBB17_416:                             # %if.else1124
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	movq	144(%rsp), %r15         # 8-byte Reload
	je	.LBB17_434
# BB#417:                               # %land.lhs.true1126
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	leaq	(%rdi,%rax), %rcx
	incq	%rax
	orq	%rcx, %rax
	testq	%rdi, %rax
	je	.LBB17_418
.LBB17_434:                             # %if.end1170
	testq	%r10, %r10
	je	.LBB17_436
# BB#435:                               # %lor.lhs.false1173
	movl	(%r10), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rdx
	je	.LBB17_437
.LBB17_436:                             # %if.then1179
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r10
	movl	(%r10), %eax
.LBB17_437:                             # %if.end1181
	andq	$65535, %rax            # imm = 0xFFFF
	testq	%rbx, %r15
	je	.LBB17_441
# BB#438:                               # %if.then1183
	cmpq	$61, %rax
	movq	%r10, %rbx
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	152(%rsp), %r15         # 8-byte Reload
	je	.LBB17_440
# BB#439:                               # %cond.false1189
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB17_440:                             # %cond.end1191
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%rbx, %r13
	jmp	.LBB17_444
.LBB17_500:                             # %if.else1320
	movq	optab_table+96(%rip), %rdi
	xorq	%r9, %r9
	xorq	%r12, %r12
	movq	%r13, %rsi
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	movq	%rax, 136(%rsp)         # 8-byte Spill
	callq	expand_twoval_binop
	testq	%r14, %rax
	movq	%r14, %r13
	je	.LBB17_502
# BB#501:
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%rbx, %r14
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB17_503
.LBB17_547:                             # %if.then1424
	movq	optab_table+104(%rip), %rsi
	movq	optab_table+80(%rip), %rdx
	movq	256(%rsp), %rax
	movq	%rax, (%rsp)
	movq	$3, 8(%rsp)
	movq	%r13, %rdi
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %r9
	callq	sign_expand_binop
	movq	%r14, %r11
	movq	%rax, %r14
	movq	%r15, %r10
	jmp	.LBB17_548
.LBB17_530:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.expand_divmod, %rdx
	movq	$3938, %rsi             # imm = 0xF62
	callq	fancy_abort
.LBB17_165:                             # %if.else472
	movq	%r15, 152(%rsp)         # 8-byte Spill
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movq	$63, %rcx
	movq	%r14, %rax
	sarq	%cl, %rax
	leaq	(%r14,%rax), %rdi
	xorq	%rax, %rdi
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%r12, %r13
	testq	%rbx, %rax
	je	.LBB17_166
# BB#167:                               # %if.else472
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r10, 128(%rsp)         # 8-byte Spill
	testq	%r14, %r14
	movq	96(%rsp), %r15          # 8-byte Reload
	jns	.LBB17_169
# BB#168:                               # %if.then490
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	movq	%r15, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rbx, %rdi
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%rdi, %r14
	jmp	.LBB17_169
.LBB17_492:                             # %if.then1310
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movq	%r14, %r13
	movq	optab_table+104(%rip), %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	expand_binop
	movq	%rax, %r15
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	expand_mult
	movq	optab_table+16(%rip), %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	%rax, %rcx
	movq	136(%rsp), %r8          # 8-byte Reload
	callq	expand_binop
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rbx, %r12
.LBB17_493:                             # %if.end1315
	movq	$-1, %rsi
	movq	%r12, %rdi
	callq	plus_constant_wide
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	%rax, %rbx
	cmpq	$1, %rcx
	jne	.LBB17_496
# BB#494:                               # %land.lhs.true.i2638
	movq	$110, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	can_compare_p
	testq	%r13, %rax
	je	.LBB17_495
.LBB17_496:                             # %if.else.i2642
	movq	$110, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_497
.LBB17_321:                             # %if.else898
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	const_int_rtx+504(%rip), %rcx
	movq	$75, %rdi
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	152(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	force_operand
	movq	optab_table+152(%rip), %rsi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movq	%rax, %r15
	movq	120(%rsp), %rdi         # 8-byte Reload
	decq	%rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	je	.LBB17_329
.LBB17_322:                             # %for.body.i2380
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB17_326
# BB#323:                               # %for.body.i2380
                                        #   in Loop: Header=BB17_322 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB17_325
# BB#324:                               # %if.then13.i2381
                                        #   in Loop: Header=BB17_322 Depth=1
	movq	$2, %rax
	jmp	.LBB17_326
.LBB17_325:                             # %if.else14.i2382
                                        #   in Loop: Header=BB17_322 Depth=1
	movq	$3, %rax
.LBB17_326:                             # %if.else79.i2384
                                        #   in Loop: Header=BB17_322 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_328
# BB#327:                               # %if.else79.i2384
                                        #   in Loop: Header=BB17_322 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_322
.LBB17_328:                             # %for.end.i2391
	testq	%rax, %rax
	movq	%rax, %r15
	je	.LBB17_204
.LBB17_329:                             # %expand_shift.exit2394
	movq	$76, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	112(%rsp), %r8          # 8-byte Reload
	callq	expand_divmod
	movq	%rax, %r14
	testq	%r14, %r14
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB17_333
# BB#330:                               # %if.then914
	movq	optab_table+288(%rip), %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	expand_unop
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	jmp	.LBB17_331
.LBB17_399:                             # %if.else1079
	cmpq	$61, %rax
	movq	%r10, %r15
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%r12, %rbx
	je	.LBB17_401
# BB#400:                               # %cond.false1085
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB17_401:                             # %cond.end1087
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB17_402:                             # %if.end1090
	movq	optab_table+112(%rip), %rdi
	movq	$1, %r9
	movq	152(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%r13, %r8
	callq	expand_twoval_binop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_409
# BB#403:                               # %if.then1093
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r14
	movq	$103, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	do_cmp_and_jump
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	movq	%r13, %rbx
	je	.LBB17_405
# BB#404:                               # %if.then.i2492
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_405:                             # %expand_inc.exit2493
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%rbx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_407
# BB#406:                               # %if.then.i2500
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_407:                             # %expand_dec.exit2501
	movq	%r14, %rdi
	callq	emit_label
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_355
# BB#408:                               # %expand_dec.exit2501
	movq	%r15, %rbx
	jmp	.LBB17_355
.LBB17_409:                             # %if.end1103
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	%rbx, %r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rax, %r12
	callq	copy_to_mode_reg
	movq	%rax, %rbx
	callq	gen_label_rtx
	movq	%rax, %r15
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r13
	movq	$102, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	do_cmp_and_jump
	movq	const_int_rtx+512(%rip), %rsi
	movq	%r12, %rdi
	callq	emit_move_insn
	movq	%r13, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r15, %rdi
	callq	emit_label
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%rbx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_411
# BB#410:                               # %if.then.i2508
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_411:                             # %expand_dec.exit2509
	movq	optab_table+104(%rip), %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_413
# BB#412:                               # %if.then1120
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_413:                             # %if.end1122
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r12), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdx
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_415
# BB#414:                               # %if.then.i2516
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_415:                             # %expand_inc.exit2517
	movq	%r13, %rdi
	jmp	.LBB17_390
.LBB17_127:                             # %if.then308
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	%r11, %r13
	movq	%r14, %rdi
	callq	floor_log2_wide
	movq	%rax, %rbx
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_131
# BB#128:                               # %if.then311
	movq	optab_table+144(%rip), %r14
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_129
.LBB17_131:                             # %if.end319
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	movq	$1, %r9
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	152(%rsp), %rdx         # 8-byte Reload
.LBB17_154:                             # %if.end427
	movq	%rax, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_shift
	movq	%rax, %r15
	jmp	.LBB17_155
.LBB17_386:                             # %if.then1029
	movq	%r10, 128(%rsp)         # 8-byte Spill
	callq	floor_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	movq	$1, %r9
	movq	%r12, %rsi
	movq	152(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_shift
	movq	optab_table+144(%rip), %r14
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_binop
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rsi
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	%rax, %rdi
	movq	%r15, %rdx
	movq	%r12, %r8
	callq	emit_store_flag
	testq	%rax, %rax
	je	.LBB17_387
# BB#391:                               # %if.else1051
	movq	$75, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB17_431
.LBB17_502:                             # %if.then1329
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rcx
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	152(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdx
	movq	%r15, %r8
	callq	expand_binop
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdx
	callq	expand_mult
	movq	optab_table+16(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	136(%rsp), %r8          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %rsi
	movq	%r15, %r14
.LBB17_503:                             # %if.end1334
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movq	%r14, 112(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	expand_abs
	movq	%rbx, %rdi
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r14, %rsi
	callq	expand_abs
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB17_504
	.align	16, 0x90
.LBB17_508:                             # %for.body.i2667
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %r12
	movabsq	$0, %rax
	je	.LBB17_512
# BB#509:                               # %for.body.i2667
                                        #   in Loop: Header=BB17_508 Depth=1
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB17_511
# BB#510:                               # %if.then13.i2668
                                        #   in Loop: Header=BB17_508 Depth=1
	movq	$2, %rax
	jmp	.LBB17_512
.LBB17_511:                             # %if.else14.i2669
                                        #   in Loop: Header=BB17_508 Depth=1
	movq	$3, %rax
.LBB17_512:                             # %if.else79.i2671
                                        #   in Loop: Header=BB17_508 Depth=1
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB17_505
# BB#513:                               # %land.lhs.true94.i
                                        #   in Loop: Header=BB17_508 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB17_508
	jmp	.LBB17_204
.LBB17_504:
	movq	%rbx, %r15
.LBB17_505:                             # %expand_shift.exit2680
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	cmpq	$1, %rax
	movq	96(%rsp), %r12          # 8-byte Reload
	jne	.LBB17_514
# BB#506:                               # %land.lhs.true.i2686
	movq	$111, %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	can_compare_p
	testq	%r13, %rax
	je	.LBB17_507
.LBB17_514:                             # %if.else.i2690
	movq	$111, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_515
.LBB17_133:
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB17_113
.LBB17_296:                             # %if.then833
	callq	floor_log2_wide
	movq	%rax, %r15
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%r13, %rax
	je	.LBB17_298
# BB#297:                               # %if.then836
	movq	optab_table+144(%rip), %r14
	movq	$1, %rsi
	movq	%r15, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB17_298
.LBB17_129:                             # %if.then316
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	jmp	.LBB17_130
.LBB17_495:                             # %if.then.i2640
	callq	gen_label_rtx
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%r12, %r15
	movq	%rax, %r12
	movq	$1, %rsi
	movq	%r14, %rdi
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%r12, %r9
	callq	do_jump_by_parts_greater_rtx
	movq	%r12, %rdi
	movq	%r15, %r12
	movq	104(%rsp), %r15         # 8-byte Reload
	callq	emit_label
.LBB17_497:                             # %do_cmp_and_jump.exit2643
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_499
# BB#498:                               # %if.then.i2650
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_499:                             # %expand_inc.exit2651
	movq	optab_table+16(%rip), %rsi
	movq	136(%rsp), %rbx         # 8-byte Reload
	movzbl	2(%rbx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdx
	movq	%r12, %rcx
	jmp	.LBB17_526
.LBB17_441:                             # %if.else1194
	cmpq	$61, %rax
	movq	%r10, %r14
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	152(%rsp), %r15         # 8-byte Reload
	je	.LBB17_443
# BB#442:                               # %cond.false1200
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB17_443:                             # %cond.end1202
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB17_444:                             # %if.end1205
	movq	optab_table+96(%rip), %rdi
	xorq	%r9, %r9
	movq	%r15, %rsi
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%r13, %r8
	callq	expand_twoval_binop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_454
# BB#445:                               # %if.then1208
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	$103, %rdx
	movq	%r13, %rdi
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	%r12, %r13
	movq	%r13, %rcx
	movq	%rax, %r8
	movq	%rax, %r12
	callq	do_cmp_and_jump
	movq	optab_table+160(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r13, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	expand_binop
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	const_int_rtx+512(%rip), %r15
	movq	%rax, %rbx
	cmpq	$1, %rcx
	jne	.LBB17_448
# BB#446:                               # %land.lhs.true.i2553
	movq	$107, %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_447
.LBB17_448:                             # %if.else.i2557
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r12, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_449
.LBB17_454:                             # %if.end1220
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	%rbx, %r13
	movq	%r12, %rdi
	callq	gen_reg_rtx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r15, 152(%rsp)         # 8-byte Spill
	callq	copy_to_mode_reg
	movq	%rax, %r15
	callq	gen_label_rtx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %rbx
	callq	gen_label_rtx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %r14
	movq	%rax, 104(%rsp)         # 8-byte Spill
	cmpq	$1, %rcx
	jne	.LBB17_457
# BB#455:                               # %land.lhs.true.i2580
	movq	$107, %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_456
.LBB17_457:                             # %if.else.i2584
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r12, %r8
	movq	%r12, %rbx
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_458
.LBB17_418:                             # %if.then1143
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	%rbx, %r13
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	callq	floor_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%rax, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	movq	152(%rsp), %r14         # 8-byte Reload
	je	.LBB17_426
.LBB17_419:                             # %for.body.i2525
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB17_423
# BB#420:                               # %for.body.i2525
                                        #   in Loop: Header=BB17_419 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB17_422
# BB#421:                               # %if.then13.i2526
                                        #   in Loop: Header=BB17_419 Depth=1
	movq	$2, %rax
	jmp	.LBB17_423
.LBB17_422:                             # %if.else14.i2527
                                        #   in Loop: Header=BB17_419 Depth=1
	movq	$3, %rax
.LBB17_423:                             # %if.else79.i2529
                                        #   in Loop: Header=BB17_419 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	movq	%rax, %r14
	jne	.LBB17_425
# BB#424:                               # %if.else79.i2529
                                        #   in Loop: Header=BB17_419 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB17_419
.LBB17_425:                             # %for.end.i2536
	testq	%r14, %r14
	je	.LBB17_204
.LBB17_426:                             # %expand_shift.exit2539
	movq	optab_table+144(%rip), %r15
	movq	136(%rsp), %rsi         # 8-byte Reload
	decq	%rsi
	xorq	%rdi, %rdi
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r12, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_binop
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	const_int_rtx+512(%rip), %rcx
	movq	$102, %rsi
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	%rax, %rdi
	movq	%r15, %rdx
	movq	%rbx, %r8
	callq	emit_store_flag
	testq	%rax, %rax
	je	.LBB17_427
# BB#433:                               # %if.else1166
	movq	$75, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	movq	%rax, %r12
	jmp	.LBB17_430
.LBB17_507:                             # %if.then.i2688
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	%rbx, %r8
	movq	128(%rsp), %r9          # 8-byte Reload
	callq	do_jump_by_parts_greater_rtx
	movq	%rbx, %rdi
	callq	emit_label
.LBB17_515:                             # %do_cmp_and_jump.exit2691
	movq	optab_table+160(%rip), %rsi
	movq	$2, (%rsp)
	xorq	%r8, %r8
	xorq	%r9, %r9
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_binop
	movq	%rax, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	decq	%rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	je	.LBB17_523
	.align	16, 0x90
.LBB17_516:                             # %for.body.i2699
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB17_520
# BB#517:                               # %for.body.i2699
                                        #   in Loop: Header=BB17_516 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB17_519
# BB#518:                               # %if.then13.i2700
                                        #   in Loop: Header=BB17_516 Depth=1
	movq	$2, %rax
	jmp	.LBB17_520
.LBB17_519:                             # %if.else14.i2701
                                        #   in Loop: Header=BB17_516 Depth=1
	movq	$3, %rax
.LBB17_520:                             # %if.else79.i2703
                                        #   in Loop: Header=BB17_516 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_522
# BB#521:                               # %if.else79.i2703
                                        #   in Loop: Header=BB17_516 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_516
.LBB17_522:                             # %for.end.i2711
	testq	%rax, %rax
	movq	%rax, %r14
	je	.LBB17_204
.LBB17_523:                             # %expand_shift.exit2714
	movq	optab_table+160(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rdx
	callq	expand_binop
	movq	optab_table+16(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movq	optab_table(%rip), %rsi
	movq	104(%rsp), %r15         # 8-byte Reload
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_525
# BB#524:                               # %if.then.i2721
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_525:                             # %expand_inc.exit2722
	movq	optab_table+160(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%r14, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	expand_binop
	movq	optab_table+16(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movq	optab_table+16(%rip), %rsi
	movq	136(%rsp), %rbx         # 8-byte Reload
	movzbl	2(%rbx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdx
	movq	%rax, %rcx
.LBB17_526:                             # %expand_inc.exit2722
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_528
# BB#527:                               # %if.then.i2729
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_528:                             # %expand_dec.exit2730
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB17_355
# BB#529:                               # %if.end1349
	movq	%r15, %rbx
	jmp	.LBB17_355
.LBB17_298:                             # %if.end846
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, 136(%rsp)         # 8-byte Spill
	cmpq	%rcx, %rax
	movq	152(%rsp), %rax         # 8-byte Reload
	je	.LBB17_331
# BB#299:
	movq	%r13, %rbx
	movq	152(%rsp), %r15         # 8-byte Reload
.LBB17_300:                             # %for.body.i2336
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %r12
	movabsq	$0, %rax
	je	.LBB17_304
# BB#301:                               # %for.body.i2336
                                        #   in Loop: Header=BB17_300 Depth=1
	movq	%r12, %rax
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB17_303
# BB#302:                               # %if.then13.i2337
                                        #   in Loop: Header=BB17_300 Depth=1
	movq	$2, %rax
	jmp	.LBB17_304
.LBB17_303:                             # %if.else14.i2338
                                        #   in Loop: Header=BB17_300 Depth=1
	movq	$3, %rax
.LBB17_304:                             # %if.else79.i2340
                                        #   in Loop: Header=BB17_300 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	movq	%rax, %r14
	jne	.LBB17_306
# BB#305:                               # %if.else79.i2340
                                        #   in Loop: Header=BB17_300 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB17_300
.LBB17_306:                             # %for.end.i2347
	xorq	%r12, %r12
	testq	%r14, %r14
	movq	112(%rsp), %r11         # 8-byte Reload
	movq	128(%rsp), %r10         # 8-byte Reload
	jne	.LBB17_548
	jmp	.LBB17_204
.LBB17_331:                             # %if.end920
	xorq	%r12, %r12
	testq	%rax, %rax
	movq	%rax, %r14
	movq	%r13, %rbx
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB17_333
# BB#332:
	movq	112(%rsp), %r11         # 8-byte Reload
	movq	128(%rsp), %r10         # 8-byte Reload
	jmp	.LBB17_548
.LBB17_333:                             # %if.end924
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r8, %rdi
	callq	delete_insns_since
	movq	128(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB17_335
# BB#334:                               # %lor.lhs.false927
	movl	(%rax), %esi
	movq	$16, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rdx
	je	.LBB17_336
.LBB17_335:                             # %if.then933
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movl	(%rax), %esi
.LBB17_336:                             # %if.end935
	andq	$65535, %rsi            # imm = 0xFFFF
	movq	144(%rsp), %rcx         # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB17_340
# BB#337:                               # %if.then937
	cmpq	$61, %rsi
	movq	%rax, %r12
	movq	%rax, 128(%rsp)         # 8-byte Spill
	je	.LBB17_339
# BB#338:                               # %cond.false943
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB17_339:                             # %cond.end945
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	jmp	.LBB17_343
.LBB17_340:                             # %if.else948
	cmpq	$61, %rsi
	movq	%rax, %r14
	movq	%rax, 128(%rsp)         # 8-byte Spill
	je	.LBB17_342
# BB#341:                               # %cond.false954
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB17_342:                             # %cond.end956
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB17_343:                             # %if.end959
	movq	optab_table+96(%rip), %rdi
	xorq	%r9, %r9
	movq	152(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	callq	expand_twoval_binop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_356
# BB#344:                               # %if.then962
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$103, %rdx
	movq	%r12, %rdi
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movq	%r15, %r12
	movq	%r12, %rcx
	movq	%rax, %r8
	callq	do_cmp_and_jump
	movq	optab_table+160(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	%rbx, %r13
	callq	expand_binop
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	const_int_rtx+512(%rip), %r15
	movq	%rax, %rbx
	cmpq	$1, %rcx
	jne	.LBB17_347
# BB#345:                               # %land.lhs.true.i2400
	movq	$104, %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_346
.LBB17_347:                             # %if.else.i2404
	movq	$104, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_348
.LBB17_356:                             # %if.end973
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
	callq	gen_label_rtx
	movq	%rax, %r14
	callq	gen_label_rtx
	movq	%rax, %r13
	callq	gen_label_rtx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, 72(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %r12
	movq	%rax, 104(%rsp)         # 8-byte Spill
	cmpq	$1, %rcx
	jne	.LBB17_359
# BB#357:                               # %land.lhs.true.i2426
	movq	$107, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_358
.LBB17_359:                             # %if.else.i2430
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r13, (%rsp)
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_360
.LBB17_346:                             # %if.then.i2402
	callq	gen_label_rtx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%rax, %rbx
	movq	%rbx, %r9
	callq	do_jump_by_parts_greater_rtx
	movq	%rbx, %rdi
	callq	emit_label
.LBB17_348:                             # %do_cmp_and_jump.exit2405
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%r14), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdx
	movq	%r14, %r8
	callq	expand_binop
	cmpq	%r14, %rax
	movq	136(%rsp), %rbx         # 8-byte Reload
	je	.LBB17_350
# BB#349:                               # %if.then.i2412
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_350:                             # %expand_dec.exit
	movq	optab_table(%rip), %rsi
	movzbl	2(%rbx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_352
# BB#351:                               # %if.then.i2419
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_352:                             # %expand_inc.exit2420
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB17_353
.LBB17_481:
	movq	%r14, %rbx
.LBB17_485:                             # %invert_mod2n.exit
	testq	%r8, %r9
	je	.LBB17_486
# BB#487:                               # %invert_mod2n.exit
	movabsq	$1, %r15
	jmp	.LBB17_488
.LBB17_486:
	movabsq	$0, %r15
.LBB17_488:                             # %invert_mod2n.exit
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	96(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	152(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdx
	movq	%rax, %rcx
	movq	256(%rsp), %r9
	callq	expand_shift
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	expand_mult
	movq	%rax, %rbx
	callq	get_last_insn
	movq	%rax, %r14
	leaq	79(%r15,%r15), %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%rbx, %r12
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	movq	%rbx, %r11
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB17_432
.LBB17_135:                             # %if.then332
	movq	$108, %rsi
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r11, %rcx
	movq	%r11, %r13
	movq	96(%rsp), %r8           # 8-byte Reload
	callq	emit_store_flag
	testq	%rax, %rax
	movq	%rax, %r15
	je	.LBB17_136
.LBB17_155:                             # %if.end427
	callq	get_last_insn
	movq	%rax, %r14
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB17_156
# BB#157:                               # %land.lhs.true431
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	%r15, %r12
	jne	.LBB17_158
# BB#159:                               # %cond.true439
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB17_161
# BB#160:                               # %cond.false450
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r10, %r15
	callq	single_set_2
	movq	%r15, %r10
.LBB17_161:                             # %cond.end458
	testq	%rax, %rax
	je	.LBB17_158
# BB#162:                               # %land.lhs.true462
	movq	%r10, %r15
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	jne	.LBB17_163
# BB#164:                               # %if.then468
	movq	$81, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
.LBB17_163:
	movq	%r13, %r11
	movq	%r15, %r10
	xorq	%r13, %r13
	jmp	.LBB17_382
.LBB17_158:
	movq	%r13, %r11
	xorq	%r13, %r13
	jmp	.LBB17_382
.LBB17_166:
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%r10, 128(%rsp)         # 8-byte Spill
	movq	96(%rsp), %r15          # 8-byte Reload
.LBB17_169:                             # %if.end493
	cmpq	$1, %r14
	movq	152(%rsp), %r12         # 8-byte Reload
	je	.LBB17_278
# BB#170:                               # %if.end493
	cmpq	$-1, %r14
	jne	.LBB17_173
# BB#171:                               # %if.then500
	movq	optab_table+256(%rip), %rsi
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
.LBB17_277:                             # %if.end763
	movq	%rax, %r12
	jmp	.LBB17_278
.LBB17_173:                             # %if.else502
	movq	120(%rsp), %rax         # 8-byte Reload
	leaq	-1(%rax), %rdx
	movq	$1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	cmpq	%rbx, %rdi
	jne	.LBB17_176
# BB#174:                               # %if.then508
	movq	$103, %rsi
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r15, %r8
	callq	emit_store_flag
	testq	%rax, %rax
	movq	%rax, %r12
	jne	.LBB17_278
# BB#175:
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB17_137
.LBB17_387:                             # %if.then1048
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rax, %r14
	movq	$103, %rdx
	movq	%r15, %rdi
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	do_cmp_and_jump
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r13), %edi
	movq	$3, (%rsp)
	xorq	%r9, %r9
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%r13, %rdx
	movq	%r13, %r8
	callq	expand_binop
	cmpq	%r13, %rax
	movq	%r13, %r12
	je	.LBB17_389
# BB#388:                               # %if.then.i2484
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_389:                             # %expand_inc.exit2485
	movq	%r14, %rdi
.LBB17_390:                             # %if.end1356
	callq	emit_label
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB17_431
.LBB17_358:                             # %if.then.i2428
	callq	gen_label_rtx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rax, %r12
	movq	%r12, %r8
	movq	%r13, %r9
	callq	do_jump_by_parts_greater_rtx
	movq	%r12, %rdi
	callq	emit_label
.LBB17_360:                             # %do_cmp_and_jump.exit2431
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %r12
	cmpq	$1, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB17_363
# BB#361:                               # %land.lhs.true.i2437
	movq	$107, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_362
.LBB17_363:                             # %if.else.i2441
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r14, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_364
.LBB17_362:                             # %if.then.i2439
	callq	gen_label_rtx
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%r13, %r8
	movq	%r14, %r9
	callq	do_jump_by_parts_greater_rtx
	movq	%r13, %rdi
	callq	emit_label
.LBB17_364:                             # %do_cmp_and_jump.exit2442
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%rbx, %r13
	movq	%r15, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_366
# BB#365:                               # %if.then985
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_366:                             # %if.end987
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r14, %rdi
	callq	emit_label
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_368
# BB#367:                               # %if.then.i2449
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_368:                             # %expand_inc.exit2450
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+512(%rip), %r12
	cmpq	$1, 40(%rsp)            # 8-byte Folded Reload
	movq	112(%rsp), %r14         # 8-byte Reload
	jne	.LBB17_371
# BB#369:                               # %land.lhs.true.i2456
	movq	$105, %rdi
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_370
.LBB17_371:                             # %if.else.i2460
	movq	$105, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r13, %rbx
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_372
.LBB17_370:                             # %if.then.i2458
	callq	gen_label_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	do_jump_by_parts_greater_rtx
	movq	%r14, %rdi
	movq	112(%rsp), %r14         # 8-byte Reload
	callq	emit_label
.LBB17_372:                             # %do_cmp_and_jump.exit2461
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	movabsq	$0, %r13
	je	.LBB17_374
# BB#373:                               # %if.then999
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_374:                             # %if.end1001
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_376
# BB#375:                               # %if.then.i2468
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_376:                             # %expand_dec.exit2469
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_378
# BB#377:                               # %if.then1010
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_378:                             # %if.end1012
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table+16(%rip), %rsi
	jmp	.LBB17_379
.LBB17_156:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r13, %r11
	movq	128(%rsp), %r10         # 8-byte Reload
	xorq	%r13, %r13
	movq	%r15, %r12
	jmp	.LBB17_382
.LBB17_447:                             # %if.then.i2555
	callq	gen_label_rtx
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	%rax, %rbx
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	do_jump_by_parts_greater_rtx
	movq	%rbx, %rdi
	callq	emit_label
.LBB17_449:                             # %do_cmp_and_jump.exit2558
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r14), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdx
	movq	%r14, %r8
	callq	expand_binop
	cmpq	%r14, %rax
	je	.LBB17_451
# BB#450:                               # %if.then.i2565
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_451:                             # %expand_inc.exit2566
	movq	optab_table+16(%rip), %rsi
	movq	136(%rsp), %rdx         # 8-byte Reload
	movzbl	2(%rdx), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rdx, %rbx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB17_453
# BB#452:                               # %if.then.i2573
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_453:                             # %expand_dec.exit2574
	movq	%r12, %rdi
.LBB17_353:                             # %expand_inc.exit2420
	callq	emit_label
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_355
# BB#354:                               # %expand_inc.exit2420
	movq	%r14, %rbx
.LBB17_355:                             # %expand_inc.exit2420
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB17_130
.LBB17_427:                             # %if.then1162
	callq	gen_label_rtx
	movq	const_int_rtx+512(%rip), %rsi
	movq	$103, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rcx
	movq	%rax, %rbx
	movq	%rbx, %r8
	callq	do_cmp_and_jump
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movq	%r14, %r12
	movzbl	2(%r12), %edi
	movq	$3, (%rsp)
	xorq	%r9, %r9
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdx
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_429
# BB#428:                               # %if.then.i2546
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_429:                             # %expand_inc.exit2547
	movq	%rbx, %rdi
	callq	emit_label
.LBB17_430:                             # %if.end1356
	movq	%r13, %rbx
	jmp	.LBB17_431
.LBB17_143:                             # %if.end357
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB17_148
# BB#144:                               # %if.then360
	movl	target_flags(%rip), %eax
	movslq	196(%rsp), %rbx
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	cmpq	%rax, %rbx
	jle	.LBB17_146
# BB#145:
	movq	%r15, %r14
	jmp	.LBB17_112
.LBB17_148:                             # %if.else390
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB17_149
# BB#150:                               # %if.else390
	movq	%r15, %r13
	movslq	196(%rsp), %r14
	cmpq	%rax, %r14
	jge	.LBB17_151
# BB#152:                               # %if.end405
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_shift
	movl	shift_cost(,%rbx,4), %ecx
	movl	shift_cost(,%r14,4), %esi
	movq	200(%rsp), %rdx
	movq	72(%rsp), %r9           # 8-byte Reload
	subq	%rcx, %r9
	subq	%rsi, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	expand_mult_highpart
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB17_136
# BB#153:                               # %if.end419
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	movq	$1, %r9
	movq	%r15, %rsi
	movq	%rbx, %rdx
	jmp	.LBB17_154
.LBB17_176:                             # %if.else514
	leaq	-1(%r14), %rax
	testq	%r14, %rax
	jne	.LBB17_186
# BB#177:                               # %land.lhs.true519
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_179
# BB#178:                               # %cond.true521
	movl	smod_pow2_cheap(%rip), %eax
	jmp	.LBB17_180
.LBB17_456:                             # %if.then.i2582
	callq	gen_label_rtx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rbx, %r9
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r12, %rbx
	movq	%r14, %rdx
	movq	%r13, %rcx
	movq	%rax, %r14
	movq	%r14, %r8
	callq	do_jump_by_parts_greater_rtx
	movq	%r14, %rdi
	callq	emit_label
.LBB17_458:                             # %do_cmp_and_jump.exit2585
	movq	const_int_rtx+512(%rip), %r12
	cmpq	$1, 72(%rsp)            # 8-byte Folded Reload
	jne	.LBB17_461
# BB#459:                               # %land.lhs.true.i2591
	movq	$105, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_460
.LBB17_461:                             # %if.else.i2595
	movq	$105, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_462
.LBB17_460:                             # %if.then.i2593
	callq	gen_label_rtx
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	do_jump_by_parts_greater_rtx
	movq	%r14, %rdi
	callq	emit_label
.LBB17_462:                             # %do_cmp_and_jump.exit2596
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	120(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	callq	expand_binop
	cmpq	%r14, %rax
	je	.LBB17_464
# BB#463:                               # %if.then1238
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_464:                             # %if.end1240
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_466
# BB#465:                               # %if.then.i2603
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_466:                             # %expand_dec.exit2604
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+512(%rip), %r12
	cmpq	$1, 72(%rsp)            # 8-byte Folded Reload
	movq	%r13, %r14
	jne	.LBB17_469
# BB#467:                               # %land.lhs.true.i2610
	movq	$107, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_468
.LBB17_469:                             # %if.else.i2614
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %r8
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_470
.LBB17_468:                             # %if.then.i2612
	callq	gen_label_rtx
	movq	%r14, %r13
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%r14, %r8
	movq	32(%rsp), %r9           # 8-byte Reload
	callq	do_jump_by_parts_greater_rtx
	movq	%r14, %rdi
	movq	%r13, %r14
	callq	emit_label
.LBB17_470:                             # %do_cmp_and_jump.exit2615
	xorq	%r13, %r13
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_472
# BB#471:                               # %if.then1252
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_472:                             # %if.end1254
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
	movzbl	2(%r15), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r15, %rdx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB17_474
# BB#473:                               # %if.then.i2622
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_474:                             # %expand_inc.exit2623
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	optab_table+80(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_476
# BB#475:                               # %if.then1263
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_476:                             # %if.end1265
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	const_int_rtx+520(%rip), %rcx
	movq	optab_table(%rip), %rsi
.LBB17_379:                             # %if.end1012
	movzbl	2(%r12), %edi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdx
	movq	%r12, %r8
	callq	expand_binop
	cmpq	%r12, %rax
	je	.LBB17_381
# BB#380:                               # %if.then.i2476
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_381:                             # %expand_dec.exit2477
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %r10         # 8-byte Reload
	movq	%r14, %r11
	jmp	.LBB17_382
.LBB17_146:                             # %if.end368
	movq	%r15, %r13
	decq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	shift_cost(,%rbx,4), %eax
	movl	shift_cost+4(%rip), %ecx
	movl	add_cost(%rip), %esi
	addq	%rsi, %rsi
	movq	200(%rsp), %rdx
	movq	72(%rsp), %r9           # 8-byte Reload
	subq	%rax, %r9
	subq	%rcx, %r9
	subq	%rsi, %r9
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	152(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	callq	expand_mult_highpart
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB17_136
# BB#147:                               # %if.end379
	movq	$76, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%r15, %r14
	movq	%rax, %r15
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r14, %r15
	movq	%rax, %rcx
	callq	expand_shift
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	movq	$1, %r9
	movq	%r15, %rsi
	movq	%r14, %rdx
	jmp	.LBB17_154
.LBB17_136:
	movq	%r13, %r14
.LBB17_137:                             # %fail1
	xorq	%r13, %r13
	movq	32(%rsp), %r8           # 8-byte Reload
	jmp	.LBB17_112
.LBB17_149:
	movq	%r15, %r14
	jmp	.LBB17_112
.LBB17_151:
	movq	%r13, %r14
	xorq	%r13, %r13
	jmp	.LBB17_112
.LBB17_179:                             # %cond.false523
	movl	sdiv_pow2_cheap(%rip), %eax
.LBB17_180:                             # %cond.false523
	testq	%rax, %rax
	je	.LBB17_186
# BB#181:                               # %land.lhs.true525
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	testq	%rcx, %rax
	jne	.LBB17_182
# BB#183:                               # %land.lhs.true525
	movabsq	$optab_table+80, %rax
	jmp	.LBB17_184
.LBB17_182:
	movabsq	$optab_table+120, %rax
.LBB17_184:                             # %land.lhs.true525
	movq	(%rax), %rax
	movq	$4, %rcx
	movq	80(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	xorq	%r12, %r12
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB17_278
# BB#185:                               # %lor.lhs.false537
	movq	optab_table+96(%rip), %rax
	movq	$4, %rcx
	movq	80(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	%rsi, %rdx
	jne	.LBB17_278
.LBB17_186:                             # %if.else545
	movq	%r14, 136(%rsp)         # 8-byte Spill
	movq	%r15, 96(%rsp)          # 8-byte Spill
	leaq	-1(%rdi), %r14
	testq	%rdi, %r14
	je	.LBB17_187
# BB#231:                               # %if.else644
	movq	120(%rsp), %r14         # 8-byte Reload
	cmpq	$64, %r14
	ja	.LBB17_232
# BB#233:                               # %if.then647
	movq	%rdx, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rcx, %r15
	leaq	184(%rsp), %rcx
	leaq	176(%rsp), %r8
	leaq	180(%rsp), %r9
	movq	%r14, %rsi
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	callq	choose_multiplier
	movl	target_flags(%rip), %edx
	movslq	176(%rsp), %r12
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rdx, %rax
	cmpq	%rdx, %r12
	jl	.LBB17_234
# BB#235:                               # %if.then647
	movabsq	$1, %rcx
	jmp	.LBB17_236
.LBB17_187:                             # %if.then550
	movq	%rdx, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	floor_log2_wide
	movslq	ix86_branch_cost(%rip), %rcx
	movq	%rax, %r13
	movl	%r13d, 180(%rsp)
	testq	%rcx, %rcx
	jle	.LBB17_190
# BB#188:                               # %lor.lhs.false554
	cmpq	$2, %rbx
	je	.LBB17_205
# BB#189:                               # %lor.lhs.false554
	cmpq	$2, %rcx
	jg	.LBB17_205
.LBB17_190:                             # %if.then560
	callq	gen_label_rtx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	copy_to_mode_reg
	movq	80(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	movq	const_int_rtx+512(%rip), %r15
	movq	%rax, %r12
	cmpq	$1, %rcx
	jne	.LBB17_193
# BB#191:                               # %land.lhs.true.i
	movq	$104, %rdi
	xorq	%rdx, %rdx
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	can_compare_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_192
.LBB17_193:                             # %if.else.i
	movq	$104, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	96(%rsp), %r8           # 8-byte Reload
	callq	emit_cmp_and_jump_insns
	jmp	.LBB17_194
.LBB17_232:
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	112(%rsp), %r11         # 8-byte Reload
	movq	128(%rsp), %r10         # 8-byte Reload
	xorq	%r13, %r13
	jmp	.LBB17_113
.LBB17_234:
	movabsq	$0, %rcx
.LBB17_236:                             # %if.then647
	cmpq	%r14, %rax
	movq	184(%rsp), %rax
	sbbq	%rdx, %rdx
	andq	$1, %rdx
	orq	%rcx, %rdx
	cmpq	%rbx, %rax
	jae	.LBB17_257
# BB#237:                               # %if.then655
	testq	%rdx, %rdx
	jne	.LBB17_258
# BB#238:                               # %if.end672
	movl	shift_cost(,%r12,4), %ecx
	movq	104(%rsp), %rdx         # 8-byte Reload
	movl	shift_cost(,%rdx,4), %edx
	movl	add_cost(%rip), %esi
	movq	%r13, %rbx
	subq	%rcx, %rbx
	subq	%rdx, %rbx
	subq	%rsi, %rbx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %r9
	callq	expand_mult_highpart
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB17_258
# BB#239:                               # %if.end685
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB17_247
.LBB17_240:                             # %for.body.i2248
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movabsq	$0, %rax
	je	.LBB17_244
# BB#241:                               # %for.body.i2248
                                        #   in Loop: Header=BB17_240 Depth=1
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_243
# BB#242:                               # %if.then13.i2249
                                        #   in Loop: Header=BB17_240 Depth=1
	movq	$2, %rax
	jmp	.LBB17_244
.LBB17_243:                             # %if.else14.i2250
                                        #   in Loop: Header=BB17_240 Depth=1
	movq	$3, %rax
.LBB17_244:                             # %if.else79.i2252
                                        #   in Loop: Header=BB17_240 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_246
# BB#245:                               # %if.else79.i2252
                                        #   in Loop: Header=BB17_240 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_240
.LBB17_246:                             # %for.end.i2259
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB17_204
.LBB17_247:                             # %expand_shift.exit2262
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	152(%rsp), %rax         # 8-byte Reload
	je	.LBB17_255
.LBB17_248:                             # %for.body.i2270
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	movabsq	$0, %rax
	je	.LBB17_252
# BB#249:                               # %for.body.i2270
                                        #   in Loop: Header=BB17_248 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_251
# BB#250:                               # %if.then13.i2271
                                        #   in Loop: Header=BB17_248 Depth=1
	movq	$2, %rax
	jmp	.LBB17_252
.LBB17_251:                             # %if.else14.i2272
                                        #   in Loop: Header=BB17_248 Depth=1
	movq	$3, %rax
.LBB17_252:                             # %if.else79.i2274
                                        #   in Loop: Header=BB17_248 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_254
# BB#253:                               # %if.else79.i2274
                                        #   in Loop: Header=BB17_248 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_248
	jmp	.LBB17_254
.LBB17_257:                             # %if.else702
	testq	%rdx, %rdx
	jne	.LBB17_258
# BB#259:                               # %if.end720
	movq	$-1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rax
	movl	shift_cost(,%r12,4), %ecx
	movq	104(%rsp), %rdx         # 8-byte Reload
	movl	shift_cost(,%rdx,4), %edx
	movl	add_cost(%rip), %esi
	movq	%rax, 184(%rsp)
	addq	%rsi, %rsi
	movq	%r13, %rbx
	subq	%rcx, %rbx
	subq	%rdx, %rbx
	subq	%rsi, %rbx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %r9
	callq	expand_mult_highpart
	testq	%rax, %rax
	je	.LBB17_258
# BB#260:                               # %if.end737
	movq	$75, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	152(%rsp), %rcx         # 8-byte Reload
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %rbx
	je	.LBB17_268
.LBB17_261:                             # %for.body.i2292
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movabsq	$0, %rax
	je	.LBB17_265
# BB#262:                               # %for.body.i2292
                                        #   in Loop: Header=BB17_261 Depth=1
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_264
# BB#263:                               # %if.then13.i2293
                                        #   in Loop: Header=BB17_261 Depth=1
	movq	$2, %rax
	jmp	.LBB17_265
.LBB17_264:                             # %if.else14.i2294
                                        #   in Loop: Header=BB17_261 Depth=1
	movq	$3, %rax
.LBB17_265:                             # %if.else79.i2296
                                        #   in Loop: Header=BB17_261 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_267
# BB#266:                               # %if.else79.i2296
                                        #   in Loop: Header=BB17_261 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_261
.LBB17_267:                             # %for.end.i2303
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB17_204
.LBB17_268:                             # %expand_shift.exit2306
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	152(%rsp), %rax         # 8-byte Reload
	je	.LBB17_255
.LBB17_269:                             # %for.body.i2314
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	movabsq	$0, %rax
	je	.LBB17_273
# BB#270:                               # %for.body.i2314
                                        #   in Loop: Header=BB17_269 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_272
# BB#271:                               # %if.then13.i2315
                                        #   in Loop: Header=BB17_269 Depth=1
	movq	$2, %rax
	jmp	.LBB17_273
.LBB17_272:                             # %if.else14.i2316
                                        #   in Loop: Header=BB17_269 Depth=1
	movq	$3, %rax
.LBB17_273:                             # %if.else79.i2318
                                        #   in Loop: Header=BB17_269 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_254
# BB#274:                               # %if.else79.i2318
                                        #   in Loop: Header=BB17_269 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_269
.LBB17_254:                             # %for.end.i2281
	testq	%rax, %rax
	je	.LBB17_204
.LBB17_255:                             # %expand_shift.exit2284
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	js	.LBB17_256
# BB#275:                               # %if.else752
	movq	$76, %rdi
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	jmp	.LBB17_276
.LBB17_258:
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB17_137
.LBB17_308:                             # %if.then858
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_divmod, %rcx
	movq	$3488, %rsi             # imm = 0xDA0
	jmp	.LBB17_142
.LBB17_205:                             # %if.else571
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rax, 104(%rsp)         # 8-byte Spill
	je	.LBB17_213
.LBB17_206:                             # %for.body.i2204
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	movabsq	$0, %rax
	je	.LBB17_210
# BB#207:                               # %for.body.i2204
                                        #   in Loop: Header=BB17_206 Depth=1
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_209
# BB#208:                               # %if.then13.i2205
                                        #   in Loop: Header=BB17_206 Depth=1
	movq	$2, %rax
	jmp	.LBB17_210
.LBB17_209:                             # %if.else14.i2206
                                        #   in Loop: Header=BB17_206 Depth=1
	movq	$3, %rax
.LBB17_210:                             # %if.else79.i2208
                                        #   in Loop: Header=BB17_206 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	callq	expand_binop
	movq	%rax, 104(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB17_212
# BB#211:                               # %if.else79.i2208
                                        #   in Loop: Header=BB17_206 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB17_206
.LBB17_212:                             # %for.end.i2215
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	je	.LBB17_204
.LBB17_213:                             # %expand_shift.exit2218
	movq	120(%rsp), %rax         # 8-byte Reload
	subq	%r13, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	expand_shift
	movq	$75, %rdi
	movq	%r14, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
	movq	%rax, %r12
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB17_220
.LBB17_214:                             # %for.body.i2226
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	movabsq	$0, %rax
	je	.LBB17_218
# BB#215:                               # %for.body.i2226
                                        #   in Loop: Header=BB17_214 Depth=1
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_217
# BB#216:                               # %if.then13.i2227
                                        #   in Loop: Header=BB17_214 Depth=1
	movq	$2, %rax
	jmp	.LBB17_218
.LBB17_217:                             # %if.else14.i2228
                                        #   in Loop: Header=BB17_214 Depth=1
	movq	$3, %rax
.LBB17_218:                             # %if.else79.i2230
                                        #   in Loop: Header=BB17_214 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_203
# BB#219:                               # %if.else79.i2230
                                        #   in Loop: Header=BB17_214 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_214
	jmp	.LBB17_203
.LBB17_192:                             # %if.then.i
	callq	gen_label_rtx
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	do_jump_by_parts_greater_rtx
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
.LBB17_194:                             # %do_cmp_and_jump.exit
	movq	%r14, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	optab_table(%rip), %rcx
	movq	%r12, %r15
	movzbl	2(%r15), %edx
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	movq	%r15, %r12
	je	.LBB17_196
# BB#195:                               # %if.then.i2195
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB17_196:                             # %expand_inc.exit
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	je	.LBB17_220
.LBB17_197:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movabsq	$0, %rax
	je	.LBB17_201
# BB#198:                               # %for.body.i
                                        #   in Loop: Header=BB17_197 Depth=1
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB17_200
# BB#199:                               # %if.then13.i
                                        #   in Loop: Header=BB17_197 Depth=1
	movq	$2, %rax
	jmp	.LBB17_201
.LBB17_200:                             # %if.else14.i
                                        #   in Loop: Header=BB17_197 Depth=1
	movq	$3, %rax
.LBB17_201:                             # %if.else79.i
                                        #   in Loop: Header=BB17_197 Depth=1
	movq	optab_table+184(%rip), %rcx
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB17_203
# BB#202:                               # %if.else79.i
                                        #   in Loop: Header=BB17_197 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB17_197
.LBB17_203:                             # %for.end.i
	testq	%rax, %rax
	movq	%rax, %r12
	je	.LBB17_204
.LBB17_220:                             # %if.end588
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	js	.LBB17_222
# BB#221:
	movq	96(%rsp), %r15          # 8-byte Reload
.LBB17_278:                             # %if.end763
	callq	get_last_insn
	movq	%rax, %r14
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB17_279
# BB#281:                               # %land.lhs.true767
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %r10         # 8-byte Reload
	jne	.LBB17_282
# BB#283:                               # %cond.true775
	movq	32(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB17_285
# BB#284:
	movq	%r15, 96(%rsp)          # 8-byte Spill
	jmp	.LBB17_286
.LBB17_279:
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB17_431:                             # %if.end1356
	movq	112(%rsp), %r11         # 8-byte Reload
.LBB17_432:                             # %if.end1356
	movq	128(%rsp), %r10         # 8-byte Reload
	xorq	%r13, %r13
.LBB17_382:                             # %if.end1356
	testq	%r12, %r12
	movq	%r12, %r14
	movq	136(%rsp), %r12         # 8-byte Reload
	je	.LBB17_113
.LBB17_548:                             # %if.end1429
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB17_557
# BB#549:                               # %if.then1431
	movq	%r11, %r9
	xorq	%r13, %r13
	testq	%r10, %r10
	movq	256(%rsp), %r8
	je	.LBB17_553
# BB#550:                               # %land.lhs.true1433
	movzbl	2(%r10), %eax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB17_552
# BB#551:                               # %land.lhs.true1433
	xorq	%r10, %r10
.LBB17_552:                             # %land.lhs.true1433
	movq	%r10, %r13
.LBB17_553:                             # %if.end1440
	movq	%r14, %rsi
	testq	%rsi, %rsi
	movq	96(%rsp), %r15          # 8-byte Reload
	je	.LBB17_554
# BB#555:                               # %if.else1445
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%rsi, %r14
	movq	%r9, %rdx
	movq	%r8, %r12
	callq	expand_mult
	movq	optab_table+16(%rip), %rsi
	movq	$3, (%rsp)
	movq	%r15, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movq	%r13, %r8
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB17_556
.LBB17_554:                             # %if.then1443
	movq	%rsi, %r14
	movq	optab_table+128(%rip), %rsi
	movq	optab_table+120(%rip), %rdx
	movq	%r8, (%rsp)
	movq	$3, 8(%rsp)
	movq	%r15, %rdi
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	%r9, %r8
	movq	%r13, %r9
	callq	sign_expand_binop
.LBB17_556:                             # %if.end1449
	movq	%rax, %r12
.LBB17_557:                             # %if.end1449
	movq	144(%rsp), %rax         # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB17_559
# BB#558:                               # %if.end1449
	movq	%r14, %r12
.LBB17_559:                             # %if.end1449
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
.LBB17_130:                             # %if.then316
	addq	$208, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB17_282:
	movq	%r15, 96(%rsp)          # 8-byte Spill
	jmp	.LBB17_287
.LBB17_285:                             # %cond.false787
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r10, %r15
	callq	single_set_2
	movq	%r15, %r10
.LBB17_286:                             # %cond.end795
	testq	%rax, %rax
	je	.LBB17_287
# BB#288:                               # %land.lhs.true799
	movq	%r10, %r13
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	je	.LBB17_291
# BB#289:
	movq	112(%rsp), %r11         # 8-byte Reload
	jmp	.LBB17_290
.LBB17_287:
	movq	112(%rsp), %r11         # 8-byte Reload
	xorq	%r13, %r13
	jmp	.LBB17_382
.LBB17_291:                             # %if.then805
	movq	$79, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	movq	%r15, %r11
.LBB17_290:                             # %if.end1356
	movq	%r13, %r10
	xorq	%r13, %r13
	jmp	.LBB17_382
.LBB17_256:                             # %if.then695
	movq	$76, %rdi
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%r13, %rcx
.LBB17_276:                             # %if.end763
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	force_operand
	jmp	.LBB17_277
.LBB17_204:                             # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
.LBB17_142:                             # %if.then354
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB17_222:                             # %if.then591
	callq	get_last_insn
	movq	%rax, %r15
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	je	.LBB17_230
# BB#223:                               # %land.lhs.true595
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB17_230
# BB#224:                               # %cond.true603
	movq	32(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB17_226
# BB#225:                               # %cond.false615
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB17_226:                             # %cond.end623
	testq	%rax, %rax
	je	.LBB17_230
# BB#227:                               # %land.lhs.true627
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	jne	.LBB17_230
# BB#228:                               # %land.lhs.true627
	testq	%rbx, %rbx
	js	.LBB17_230
# BB#229:                               # %if.then636
	movq	%rbx, %rdi
	movq	96(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$79, %rdi
	movq	%r14, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
.LBB17_230:                             # %if.end641
	movq	optab_table+256(%rip), %rax
	xorq	%r8, %r8
	movq	96(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	%r12, %rax
	movq	%rax, %rdx
	movq	%rax, %rcx
	callq	expand_unop
	jmp	.LBB17_277
.Ltmp248:
	.size	expand_divmod, .Ltmp248-expand_divmod
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_110
	.quad	.LBB17_383
	.quad	.LBB17_292
	.quad	.LBB17_489
	.quad	.LBB17_110
	.quad	.LBB17_383
	.quad	.LBB17_292
	.quad	.LBB17_489
	.quad	.LBB17_530
	.quad	.LBB17_477

	.text
	.globl	emit_store_flag
	.align	16, 0x90
	.type	emit_store_flag,@function
emit_store_flag:                        # @emit_store_flag
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
	subq	$96, %rsp
.Ltmp260:
	.cfi_def_cfa_offset 144
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
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%r8, %r12
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB18_1
	.align	16, 0x90
.LBB18_49:                              # %if.then76
                                        #   in Loop: Header=BB18_1 Depth=1
	movl	word_mode(%rip), %edi
	movq	%rsi, %rbx
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	gen_highpart
	movq	%rbx, %rsi
	movq	%rax, %r13
	movl	word_mode(%rip), %r12d
.LBB18_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	protect_from_queue
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rbx
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB18_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%r14, %rdi
	callq	unsigned_condition
	movq	%rax, %r14
.LBB18_3:                               # %if.end
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	swap_commutative_operands_p
	testq	%r15, %rax
	jne	.LBB18_5
# BB#4:                                 #   in Loop: Header=BB18_1 Depth=1
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r13, %rbx
	movq	%r14, %rsi
	jmp	.LBB18_6
	.align	16, 0x90
.LBB18_5:                               # %if.then6
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%r14, %rdi
	callq	swap_condition
	movq	%rax, %rsi
	movq	%r13, 88(%rsp)          # 8-byte Spill
.LBB18_6:                               # %if.end8
                                        #   in Loop: Header=BB18_1 Depth=1
	testq	%r15, %r12
	jne	.LBB18_8
# BB#7:                                 # %if.then9
                                        #   in Loop: Header=BB18_1 Depth=1
	movzbl	2(%rbx), %r12d
.LBB18_8:                               # %if.end13
                                        #   in Loop: Header=BB18_1 Depth=1
	leaq	-104(%rsi), %rax
	andq	%r15, %rax
	cmpq	$7, %rax
	jbe	.LBB18_10
# BB#9:                                 #   in Loop: Header=BB18_1 Depth=1
	movq	%rbx, 80(%rsp)          # 8-byte Spill
.LBB18_40:                              # %sw.epilog
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	$32, %rcx
	movq	%r12, %r14
	movq	%r12, 40(%rsp)          # 8-byte Spill
	shlq	%cl, %r14
	movq	$32, %rcx
	movl	target_flags(%rip), %eax
	sarq	%cl, %r14
	movzbl	mode_bitsize(%r14,%r14), %edx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$64, %rax
	addq	$64, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	cmpq	%rax, %rdx
	jne	.LBB18_50
# BB#41:                                # %land.lhs.true
                                        #   in Loop: Header=BB18_1 Depth=1
	movl	mode_class(,%r14,4), %eax
	cmpq	$1, %rax
	jne	.LBB18_50
# BB#42:                                # %land.lhs.true
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_50
# BB#43:                                # %land.lhs.true47
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	andq	$134283263, %rax        # imm = 0x800FFFF
	cmpq	$134217794, %rax        # imm = 0x8000042
	je	.LBB18_50
# BB#44:                                # %if.then56
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%rsi, %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rcx, %rax
	cmpq	$102, %rax
	jne	.LBB18_47
# BB#45:                                # %if.then62
                                        #   in Loop: Header=BB18_1 Depth=1
	movl	word_mode(%rip), %edi
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rsi, %r12
	movq	%rbx, %rsi
	callq	gen_highpart
	movl	word_mode(%rip), %edi
	movq	%rax, %r13
	movq	%rbx, %rsi
	callq	gen_lowpart
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%r13, %rdx
	movq	%rax, %rcx
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	expand_binop
	movq	%r12, %rsi
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB18_50
# BB#46:                                # %tailrecurse.backedge
                                        #   in Loop: Header=BB18_1 Depth=1
	movl	word_mode(%rip), %r12d
	jmp	.LBB18_1
	.align	16, 0x90
.LBB18_10:                              # %if.end13
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	jmpq	*.LJTI18_0(,%rax,8)
.LBB18_21:                              # %sw.bb21
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	movabsq	$0, %rsi
	jne	.LBB18_23
# BB#22:                                # %sw.bb21
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$1, %rsi
.LBB18_23:                              # %sw.bb21
                                        #   in Loop: Header=BB18_1 Depth=1
	jne	.LBB18_25
# BB#24:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_25:                              # %sw.bb21
                                        #   in Loop: Header=BB18_1 Depth=1
	orq	$104, %rsi
	jmp	.LBB18_40
	.align	16, 0x90
.LBB18_47:                              # %if.else
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%rsi, %rax
	andq	%r15, %rax
	cmpq	$104, %rax
	je	.LBB18_49
# BB#48:                                # %if.else
                                        #   in Loop: Header=BB18_1 Depth=1
	cmpq	$107, %rax
	je	.LBB18_49
	jmp	.LBB18_50
.LBB18_26:                              # %sw.bb25
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+504(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_28
# BB#27:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_28:                              # %sw.bb25
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$0, %rsi
	je	.LBB18_30
# BB#29:                                # %sw.bb25
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$1, %rsi
.LBB18_30:                              # %sw.bb25
                                        #   in Loop: Header=BB18_1 Depth=1
	orq	$104, %rsi
	jmp	.LBB18_40
.LBB18_16:                              # %sw.bb17
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+504(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	movabsq	$0, %rsi
	jne	.LBB18_18
# BB#17:                                # %sw.bb17
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$1, %rsi
.LBB18_18:                              # %sw.bb17
                                        #   in Loop: Header=BB18_1 Depth=1
	jne	.LBB18_20
# BB#19:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_20:                              # %sw.bb17
                                        #   in Loop: Header=BB18_1 Depth=1
	orq	$106, %rsi
	jmp	.LBB18_40
.LBB18_11:                              # %sw.bb
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_13
# BB#12:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_13:                              # %sw.bb
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$0, %rsi
	je	.LBB18_15
# BB#14:                                # %sw.bb
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$1, %rsi
.LBB18_15:                              # %sw.bb
                                        #   in Loop: Header=BB18_1 Depth=1
	orq	$106, %rsi
	jmp	.LBB18_40
.LBB18_31:                              # %sw.bb29
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_33
# BB#32:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_33:                              # %sw.bb29
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	$102, %rsi
	je	.LBB18_40
# BB#34:                                # %sw.bb29
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	$108, %rsi
	jmp	.LBB18_40
.LBB18_35:                              # %sw.bb33
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+520(%rip), %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_37
# BB#36:                                #   in Loop: Header=BB18_1 Depth=1
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB18_37:                              # %sw.bb33
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$0, %rax
	je	.LBB18_39
# BB#38:                                # %sw.bb33
                                        #   in Loop: Header=BB18_1 Depth=1
	movabsq	$1, %rax
.LBB18_39:                              # %sw.bb33
                                        #   in Loop: Header=BB18_1 Depth=1
	leaq	103(,%rax,8), %rsi
	jmp	.LBB18_40
.LBB18_50:                              # %if.end81
	movq	$16, %rcx
	movq	64(%rsp), %rdx          # 8-byte Reload
	shrq	%cl, %rdx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	const_int_rtx+512(%rip), %rcx
	movslq	setcc_gen_code(,%rax,4), %rbx
	andq	$255, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	cmpq	%rcx, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_67
# BB#51:                                # %land.lhs.true86
	movq	%rsi, %r13
	andq	%r15, %r13
	cmpq	$104, %r13
	je	.LBB18_53
# BB#52:                                # %land.lhs.true86
	cmpq	$107, %r13
	jne	.LBB18_67
.LBB18_53:                              # %land.lhs.true92
	movl	mode_class(,%r14,4), %eax
	cmpq	$1, %rax
	jne	.LBB18_67
# BB#54:                                # %land.lhs.true97
	movq	64(%rsp), %r12          # 8-byte Reload
	movzbl	mode_size(%r12), %eax
	movzbl	mode_size(%r14), %ecx
	cmpq	%rcx, %rax
	jbe	.LBB18_55
# BB#56:                                # %if.then124
	xorq	%rsi, %rsi
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r12, %rbx
	jmp	.LBB18_57
.LBB18_67:                              # %if.end151
	movq	%rsi, %r13
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB18_80
# BB#68:
	movq	56(%rsp), %rdi          # 8-byte Reload
.LBB18_69:                              # %if.end284
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	callq	delete_insns_since
	movl	flag_expensive_optimizations(%rip), %eax
	xorq	%r12, %r12
	testq	%rax, %rax
	jne	.LBB18_72
# BB#70:                                # %land.lhs.true286
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	%r15, %rax
	cmpq	%rax, 64(%rsp)          # 8-byte Folded Reload
	movq	48(%rsp), %r12          # 8-byte Reload
	je	.LBB18_72
# BB#71:                                # %land.lhs.true286
	xorq	%r12, %r12
.LBB18_72:                              # %cond.end291
	movslq	ix86_branch_cost(%rip), %rax
	testq	%rax, %rax
	movq	%r13, %rdx
	jle	.LBB18_121
# BB#73:                                # %land.lhs.true295
	movq	%rdx, %rcx
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%rsi, %rcx
	cmpq	$102, %rcx
	jne	.LBB18_122
# BB#74:                                # %land.lhs.true295
	movl	mode_class(,%r14,4), %ecx
	andq	%r15, %rcx
	cmpq	$1, %rcx
	jne	.LBB18_122
# BB#75:                                # %land.lhs.true295
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, 88(%rsp)          # 8-byte Folded Reload
	je	.LBB18_122
# BB#76:                                # %if.then309
	movq	%rdx, %rbx
	movq	optab_table+160(%rip), %rsi
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r8
	callq	expand_binop
	movq	%rbx, %rsi
	testq	%rax, %rax
	jne	.LBB18_78
# BB#77:                                # %if.end315
	movq	%rsi, %rbx
	movq	optab_table+16(%rip), %rsi
	movq	$1, %r9
	movq	$2, (%rsp)
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r8
	callq	expand_binop
	movq	%rbx, %rsi
	testq	%rax, %rax
	je	.LBB18_79
.LBB18_78:                              # %if.end320
	movq	const_int_rtx+512(%rip), %rcx
	movq	144(%rsp), %rdx
	movq	%rdx, (%rsp)
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rdx
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	emit_store_flag
	movq	%rax, %r10
	testq	%r10, %r10
	jne	.LBB18_181
.LBB18_79:                              # %if.then323
	movq	56(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB18_180
.LBB18_80:                              # %if.then154
	callq	emit_queue
	callq	do_pending_stack_adjust
	callq	get_last_insn
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%r9, %r9
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	compare_from_rtx
	movq	%rax, %r10
	movzbl	(%r10), %r13d
	cmpq	$54, %r13
	jne	.LBB18_88
# BB#81:                                # %if.then161
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, %r10
	je	.LBB18_181
# BB#82:                                # %cond.false
	movq	144(%rsp), %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB18_84
# BB#83:                                # %cond.true166
	movq	const_int_rtx+520(%rip), %r10
	jmp	.LBB18_181
.LBB18_88:                              # %if.end177
	leaq	(%rbx,%rbx,4), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rax
	movzbl	16(%rax), %ebx
	movq	(%rax), %r12
	callq	preserve_subexpressions_p
	testq	%r15, %rax
	jne	.LBB18_90
# BB#89:                                # %lor.lhs.false191
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	*%r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jne	.LBB18_91
.LBB18_90:                              # %if.then194
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rdi
.LBB18_91:                              # %if.end196
	xorq	%rax, %rax
	movq	%rdi, %r12
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	testq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	je	.LBB18_69
# BB#92:                                # %if.then201
	movq	%rax, %rdi
	callq	emit_insn
	movq	64(%rsp), %r13          # 8-byte Reload
	movzbl	mode_size(%r13), %eax
	movq	%rbx, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	movq	%r12, %r14
	jbe	.LBB18_98
# BB#93:                                # %if.then211
	movzbl	mode_bitsize(%rdx,%rdx), %eax
	xorq	%rdx, %rdx
	cmpq	$64, %rax
	ja	.LBB18_97
# BB#94:                                # %land.rhs
	addq	%r15, %rax
	testq	%r15, %rax
	je	.LBB18_95
# BB#96:                                # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB18_97
.LBB18_121:                             # %if.end325
	xorq	%r10, %r10
	testq	%r15, %rax
	je	.LBB18_181
.LBB18_122:                             # %lor.lhs.false328
	movl	mode_class(,%r14,4), %ecx
	xorq	%r10, %r10
	cmpq	$1, %rcx
	jne	.LBB18_181
# BB#123:                               # %lor.lhs.false328
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%rcx, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_181
# BB#124:                               # %lor.lhs.false336
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rdx, %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	cmpq	$102, %rcx
	je	.LBB18_127
# BB#125:                               # %land.lhs.true342
	cmpq	$2, %rax
	jl	.LBB18_181
# BB#126:                               # %land.lhs.true342
	leaq	-105(%rdx), %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	ja	.LBB18_181
.LBB18_127:                             # %if.end352
	movq	144(%rsp), %rax
	testq	%r15, %rax
	movq	%r15, %r13
	movq	$1, %rcx
	je	.LBB18_129
# BB#128:                               # %if.end352
	movq	%rax, %rcx
.LBB18_129:                             # %if.end352
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%r13, %r15
	andq	%r15, %rdx
	cmpq	$105, %rdx
	jne	.LBB18_130
# BB#137:                               # %if.then372
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	xorq	%rbx, %rbx
	testq	%r15, %rax
	movq	%r15, %r13
	je	.LBB18_139
# BB#138:                               # %if.then372
	xorq	%r12, %r12
.LBB18_139:                             # %if.then372
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	32(%rsp), %rdi          # 8-byte Reload
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r15
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	40(%rsp), %r12          # 8-byte Reload
	je	.LBB18_147
	.align	16, 0x90
.LBB18_140:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r13, %rbx
	movabsq	$0, %rax
	je	.LBB18_144
# BB#141:                               # %for.body.i
                                        #   in Loop: Header=BB18_140 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB18_143
# BB#142:                               # %if.then13.i
                                        #   in Loop: Header=BB18_140 Depth=1
	movq	$2, %rax
	jmp	.LBB18_144
	.align	16, 0x90
.LBB18_143:                             # %if.else14.i
                                        #   in Loop: Header=BB18_140 Depth=1
	movq	$3, %rax
.LBB18_144:                             # %if.else79.i
                                        #   in Loop: Header=BB18_140 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB18_146
# BB#145:                               # %if.else79.i
                                        #   in Loop: Header=BB18_140 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB18_140
.LBB18_146:                             # %for.end.i
	testq	%rax, %rax
	je	.LBB18_182
.LBB18_147:                             # %expand_shift.exit
	movq	optab_table+16(%rip), %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	88(%rsp), %r8           # 8-byte Reload
	jmp	.LBB18_148
.LBB18_55:
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB18_57:                              # %if.end127
	movq	%rbx, %r14
	andq	%r15, %r14
	xorq	%rax, %rax
	cmpq	%r14, %r12
	jne	.LBB18_59
# BB#58:                                # %if.end127
	movq	48(%rsp), %rax          # 8-byte Reload
.LBB18_59:                              # %if.end127
	cmpq	$104, %r13
	jne	.LBB18_60
# BB#61:                                # %if.then134
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	optab_table+288(%rip), %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %r13
	jmp	.LBB18_62
.LBB18_60:
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r13          # 8-byte Reload
.LBB18_62:                              # %if.end136
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	144(%rsp), %rcx
	andq	%r15, %rcx
	cmpq	%r15, %rcx
	je	.LBB18_63
# BB#64:                                # %if.end136
	movabsq	$1, %r9
	jmp	.LBB18_65
.LBB18_63:
	movabsq	$0, %r9
.LBB18_65:                              # %if.end136
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	$83, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	%rax, %r10
	cmpq	%r14, %r12
	je	.LBB18_181
# BB#66:                                # %if.then148
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r10, %rdx
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_modes  # TAILCALL
.LBB18_84:                              # %cond.false167
	cmpq	%r15, %rax
	je	.LBB18_85
# BB#86:                                # %cond.false167
	movabsq	$const_true_rtx, %rax
	movq	(%rax), %r10
	jmp	.LBB18_181
.LBB18_130:                             # %if.end352
	movq	%r15, %r13
	xorq	%r10, %r10
	cmpq	$106, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	jne	.LBB18_131
# BB#132:                               # %if.then359
	movq	%r12, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r13, %rax
	movq	%r12, %r15
	movq	40(%rsp), %r12          # 8-byte Reload
	je	.LBB18_134
# BB#133:                               # %if.then359
	xorq	%r15, %r15
.LBB18_134:                             # %if.then359
	movq	optab_table+16(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %r8
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB18_135
# BB#136:                               # %if.then366
	movq	optab_table+152(%rip), %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	movq	%r15, 88(%rsp)          # 8-byte Spill
.LBB18_148:                             # %if.end385
	callq	expand_binop
	movq	%rax, %r10
	jmp	.LBB18_149
.LBB18_85:
	movabsq	$const_int_rtx+504, %rax
	movq	(%rax), %r10
	jmp	.LBB18_181
.LBB18_131:
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	40(%rsp), %r12          # 8-byte Reload
	jmp	.LBB18_149
.LBB18_95:
	movabsq	$0, %rdx
.LBB18_97:                              # %land.end
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	convert_move
	movq	%r13, %rdx
	movq	%rbx, %r14
.LBB18_98:                              # %if.end227
	movq	%rdx, %r13
	callq	preserve_subexpressions_p
	testq	%r15, %rax
	movq	%r12, %rbx
	je	.LBB18_100
# BB#99:                                # %if.end227
	xorq	%rbx, %rbx
.LBB18_100:                             # %if.end227
	movq	144(%rsp), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jae	.LBB18_103
# BB#101:
	movq	%r14, %r10
.LBB18_102:                             # %if.end278
	movq	%r13, %r12
	movq	64(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB18_119
.LBB18_103:                             # %if.else238
	cmpq	%r15, %rcx
	je	.LBB18_104
# BB#106:                               # %if.else244
	movzbl	mode_bitsize(%r13,%r13), %eax
	cmpq	$1, %rax
	jne	.LBB18_111
# BB#107:                               # %if.then259
	cmpq	$1, %rcx
	jne	.LBB18_108
# BB#109:                               # %if.then259
	movabsq	$1, %r15
	jmp	.LBB18_110
.LBB18_104:                             # %if.then242
	movq	optab_table+256(%rip), %rsi
	xorq	%r8, %r8
	movq	%r13, %r12
	movq	%r12, %rdi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	expand_unop
	jmp	.LBB18_105
.LBB18_111:                             # %if.else269
	movzbl	2(%r14), %eax
	movq	const_int_rtx+520(%rip), %r15
	testq	%rax, %rax
	jne	.LBB18_114
# BB#112:                               # %land.lhs.true.i
	movzbl	2(%r15), %eax
	testq	%rax, %rax
	jne	.LBB18_114
# BB#113:                               # %if.end.i
	movq	$83, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB18_115
.LBB18_114:                             # %if.then6.i
	movq	optab_table+144(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r13, %rdi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	expand_binop
.LBB18_115:                             # %if.end8.i
	testq	%rbx, %rbx
	je	.LBB18_116
# BB#117:                               # %if.else.i
	cmpq	%rbx, %rax
	movq	%r12, %r10
	movq	%r13, %r12
	movq	%rbx, %r14
	movq	64(%rsp), %rbx          # 8-byte Reload
	je	.LBB18_119
# BB#118:                               # %if.then12.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r14, %r10
	jmp	.LBB18_119
.LBB18_108:
	movabsq	$0, %r15
.LBB18_110:                             # %if.then259
	movq	%r13, %r12
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$83, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	movq	%r15, %r9
	callq	expand_shift
.LBB18_105:                             # %if.end278
	movq	%rax, %r10
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB18_119:                             # %if.end278
	cmpq	%r12, %rbx
	je	.LBB18_181
	jmp	.LBB18_120
.LBB18_135:
	movq	%r15, 88(%rsp)          # 8-byte Spill
	xorq	%r10, %r10
.LBB18_149:                             # %if.end385
	cmpq	$102, 72(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_150
# BB#151:                               # %if.then391
	movq	optab_table+272(%rip), %rsi
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movl	8(%rsi,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	56(%rsp), %r15          # 8-byte Reload
	jne	.LBB18_152
# BB#153:                               # %if.else398
	movq	optab_table+296(%rip), %rsi
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movl	8(%rsi,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB18_152
# BB#154:                               # %if.else407
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%r14), %edx
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jae	.LBB18_157
# BB#155:                               # %if.then416
	xorq	%rsi, %rsi
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movl	word_mode(%rip), %edi
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$1, %rcx
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	convert_modes
	movl	word_mode(%rip), %r12d
	jmp	.LBB18_156
.LBB18_150:
	movq	56(%rsp), %r15          # 8-byte Reload
	jmp	.LBB18_169
.LBB18_152:                             # %if.then396
	movq	$1, %r8
	movq	%r12, %rdi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
.LBB18_156:                             # %if.end421
	movq	%rax, %r10
.LBB18_157:                             # %if.end421
	movq	16(%rsp), %rbx          # 8-byte Reload
	testq	%r10, %r10
	je	.LBB18_162
# BB#158:                               # %if.then424
	cmpq	$103, %rbx
	jne	.LBB18_160
# BB#159:                               # %if.then427
	movq	optab_table+16(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%r10, %rdx
	movq	88(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
	jmp	.LBB18_161
.LBB18_160:                             # %if.else429
	movq	optab_table+256(%rip), %rsi
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	%r10, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
.LBB18_161:                             # %if.end432
	movq	%rax, %r10
	testq	%r10, %r10
	jne	.LBB18_169
.LBB18_162:                             # %land.lhs.true435
	cmpq	$102, %rbx
	je	.LBB18_164
# BB#163:                               # %land.lhs.true435
	movl	ix86_branch_cost(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB18_179
.LBB18_164:                             # %if.then441
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB18_166
# BB#165:                               # %if.then441
	xorq	%r14, %r14
.LBB18_166:                             # %if.then441
	movq	%r14, 88(%rsp)          # 8-byte Spill
	movq	optab_table+256(%rip), %rsi
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	expand_unop
	movq	optab_table+152(%rip), %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r14, %r8
	callq	expand_binop
	movq	%rax, %r10
	testq	%r10, %r10
	je	.LBB18_169
# BB#167:                               # %if.then441
	cmpq	$103, 16(%rsp)          # 8-byte Folded Reload
	jne	.LBB18_169
# BB#168:                               # %if.then452
	movq	optab_table+288(%rip), %rsi
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	%r10, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %r10
.LBB18_169:                             # %if.end456
	testq	%r10, %r10
	je	.LBB18_175
# BB#170:                               # %if.end456
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%r13, %rbx
	je	.LBB18_175
# BB#171:                               # %if.then460
	movq	%r10, %r14
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	andq	%r13, %rbx
	cmpq	$1, %rbx
	jne	.LBB18_172
# BB#173:                               # %if.then460
	movabsq	$1, %r9
	jmp	.LBB18_174
.LBB18_172:
	movabsq	$0, %r9
.LBB18_174:                             # %if.then460
	movq	%r14, %rdx
	movq	$83, %rdi
	movq	%r12, %rsi
	movq	%rax, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	callq	expand_shift
	movq	%rax, %r10
.LBB18_175:                             # %if.end470
	testq	%r10, %r10
	je	.LBB18_179
# BB#176:                               # %if.then472
	movzbl	2(%r10), %eax
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB18_120
# BB#177:                               # %if.else479
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB18_181
# BB#178:                               # %if.then481
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r10, %rsi
	callq	emit_move_insn
	movq	%rbx, %r10
	jmp	.LBB18_181
.LBB18_120:                             # %if.then281
	xorq	%rdx, %rdx
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r10, %rsi
	callq	convert_move
	movq	%rbx, %r10
	jmp	.LBB18_181
.LBB18_179:                             # %if.else485
	movq	%r15, %rdi
.LBB18_180:                             # %return
	callq	delete_insns_since
	xorq	%r10, %r10
.LBB18_181:                             # %return
	movq	%r10, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_116:
	movq	%rax, %r10
	jmp	.LBB18_102
.LBB18_182:                             # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp266:
	.size	emit_store_flag, .Ltmp266-emit_store_flag
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_21
	.quad	.LBB18_26
	.quad	.LBB18_16
	.quad	.LBB18_11
	.quad	.LBB18_31
	.quad	.LBB18_40
	.quad	.LBB18_40
	.quad	.LBB18_35

	.text
	.align	16, 0x90
	.type	choose_multiplier,@function
choose_multiplier:                      # @choose_multiplier
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
	subq	$144, %rsp
.Ltmp278:
	.cfi_def_cfa_offset 192
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
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-1(%r12), %rdi
	callq	floor_log2_wide
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rbx, %rdx
	jge	.LBB19_1
# BB#3:                                 # %if.end
	movq	%r12, 88(%rsp)          # 8-byte Spill
	leaq	1(%rax), %rbx
	leaq	(%rbx,%r14), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$128, %rcx
	je	.LBB19_4
# BB#5:                                 # %if.end4
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %r13
	subq	%r15, %r13
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$64, %rdx
	jl	.LBB19_7
# BB#6:                                 # %if.then6
	addq	$-64, %rsi
	movq	$1, %r15
	movq	%rsi, %rcx
	shlq	%cl, %r15
	xorq	%r12, %r12
	jmp	.LBB19_8
.LBB19_7:                               # %if.else
	movq	$1, %r12
	movq	%rsi, %rcx
	shlq	%cl, %r12
	xorq	%r15, %r15
.LBB19_8:                               # %if.end10
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	104(%rsp), %rbx
	movq	%rbx, 16(%rsp)
	leaq	128(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, (%rsp)
	movq	$62, %rdi
	movq	$1, %rsi
	xorq	%r9, %r9
	movq	$1, %r14
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	88(%rsp), %r8           # 8-byte Reload
	callq	div_and_round_double
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$64, %rax
	jl	.LBB19_10
# BB#9:                                 # %if.then13
	addq	$-64, %r13
	movq	%r13, %rcx
	shlq	%cl, %r14
	orq	%r14, %r15
	jmp	.LBB19_11
.LBB19_10:                              # %if.else17
	movq	%r13, %rcx
	shlq	%cl, %r14
	orq	%r14, %r12
.LBB19_11:                              # %if.end21
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	%rbx, 16(%rsp)
	leaq	136(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	120(%rsp), %rax
	movq	$62, %rdi
	movq	$1, %rsi
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r8
	callq	div_and_round_double
	movq	136(%rsp), %rax
	testq	%rax, %rax
	je	.LBB19_14
# BB#12:                                # %if.end21
	subq	%r14, %r15
	cmpq	%r14, %r15
	jae	.LBB19_13
.LBB19_14:                              # %if.end26
	cmpq	$1, %rax
	jg	.LBB19_16
# BB#15:                                # %if.end26
	movq	128(%rsp), %rsi
	cmpq	$2, %rsi
	jge	.LBB19_16
# BB#17:                                # %if.end30
	cmpq	%rax, %rsi
	movq	80(%rsp), %r9           # 8-byte Reload
	jl	.LBB19_18
# BB#20:                                # %lor.lhs.false32
	jne	.LBB19_22
# BB#21:                                # %land.lhs.true34
	movq	112(%rsp), %rcx
	movq	120(%rsp), %rdx
	cmpq	%rdx, %rcx
	jae	.LBB19_22
.LBB19_18:                              # %for.cond.preheader
	movq	$32, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	48(%rsp), %r8           # 8-byte Reload
	js	.LBB19_19
# BB#23:                                # %for.body.lr.ph
	movq	112(%rsp), %rdx
	movq	120(%rsp), %rdi
	movq	$63, %rcx
	shlq	%cl, %rsi
	movq	%r9, %rbx
	.align	16, 0x90
.LBB19_24:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rcx
	shrq	%cl, %rdx
	orq	%rsi, %rdx
	movq	$63, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$1, %rcx
	shrq	%cl, %rdi
	orq	%rsi, %rdi
	cmpq	%rdi, %rdx
	jae	.LBB19_26
# BB#25:                                # %if.end46
                                        #   in Loop: Header=BB19_24 Depth=1
	movq	$0, 128(%rsp)
	movq	%rdx, 112(%rsp)
	movq	$0, 136(%rsp)
	movq	%rdi, 120(%rsp)
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movabsq	$0, %rsi
	jg	.LBB19_24
	jmp	.LBB19_26
.LBB19_19:
	movq	%r9, %rbx
.LBB19_26:                              # %for.end
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	%ebx, (%rcx)
	movq	72(%rsp), %rcx          # 8-byte Reload
	movl	%r9d, (%rcx)
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$63, %rdx
	jg	.LBB19_30
# BB#27:                                # %if.then48
	movq	120(%rsp), %rax
	movq	$1, %rdx
	movq	%r8, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	movq	%rax, %rcx
	andq	%rdx, %rcx
	cmpq	%rdx, %rax
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rcx, (%rax)
	jb	.LBB19_28
# BB#29:                                # %if.then48
	movabsq	$1, %rax
	jmp	.LBB19_31
.LBB19_30:                              # %if.else54
	movq	120(%rsp), %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, (%rdx)
	jmp	.LBB19_31
.LBB19_28:
	movabsq	$0, %rax
.LBB19_31:                              # %return
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.choose_multiplier, %rcx
	movq	$2622, %rsi             # imm = 0xA3E
	jmp	.LBB19_2
.LBB19_4:                               # %if.then3
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.choose_multiplier, %rcx
	movq	$2631, %rsi             # imm = 0xA47
	jmp	.LBB19_2
.LBB19_13:                              # %if.then25
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.choose_multiplier, %rcx
	movq	$2657, %rsi             # imm = 0xA61
	jmp	.LBB19_2
.LBB19_16:                              # %if.then29
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.choose_multiplier, %rcx
	movq	$2659, %rsi             # imm = 0xA63
	jmp	.LBB19_2
.LBB19_22:                              # %if.then36
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.choose_multiplier, %rcx
	movq	$2662, %rsi             # imm = 0xA66
.LBB19_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp284:
	.size	choose_multiplier, .Ltmp284-choose_multiplier
	.cfi_endproc

	.align	16, 0x90
	.type	do_cmp_and_jump,@function
do_cmp_and_jump:                        # @do_cmp_and_jump
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
	movq	%rcx, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %eax
	movq	%rdx, %r14
	movq	%rsi, %r13
	cmpq	$1, %rax
	jne	.LBB20_25
# BB#1:                                 # %land.lhs.true
	xorq	%rdx, %rdx
	movq	%rdi, %r15
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r8, %rbx
	callq	can_compare_p
	movq	%r15, %rdi
	movq	%rbx, %r8
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB20_2
.LBB20_25:                              # %if.else
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r8, (%rsp)
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r12, %r8
	callq	emit_cmp_and_jump_insns
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_2:                               # %if.then
	movq	%rdi, %rbx
	movq	%r8, 8(%rsp)            # 8-byte Spill
	callq	gen_label_rtx
	addq	$-102, %r14
	andq	%r15, %r14
	cmpq	$9, %r14
	ja	.LBB20_24
# BB#3:                                 # %if.then
	jmpq	*.LJTI20_0(,%r14,8)
.LBB20_20:                              # %sw.bb10
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%r13, %rcx
	movq	%rbx, %rdi
	jne	.LBB20_22
# BB#21:                                # %lor.lhs.false12
	movzbl	2(%rdi), %ecx
	andq	%r12, %r15
	cmpq	%r15, %rcx
	jne	.LBB20_22
# BB#23:                                # %if.end18
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	jmp	.LBB20_19
.LBB20_14:                              # %sw.bb6
	movq	const_int_rtx+512(%rip), %rcx
	cmpq	%r13, %rcx
	movq	%rbx, %rdi
	jne	.LBB20_16
# BB#15:                                # %lor.lhs.false
	movzbl	2(%rdi), %ecx
	andq	%r12, %r15
	cmpq	%r15, %rcx
	jne	.LBB20_16
# BB#18:                                # %if.end
	movq	%rax, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
.LBB20_19:                              # %sw.epilog
	movq	%rax, %rbx
	callq	do_jump_by_parts_equality_rtx
	jmp	.LBB20_10
.LBB20_13:                              # %sw.bb5
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	jmp	.LBB20_8
.LBB20_12:                              # %sw.bb4
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	jmp	.LBB20_6
.LBB20_11:                              # %sw.bb3
	xorq	%rsi, %rsi
	jmp	.LBB20_5
.LBB20_7:                               # %sw.bb2
	movq	$1, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%r13, %rcx
.LBB20_8:                               # %sw.epilog
	movq	8(%rsp), %r8            # 8-byte Reload
	movq	%rax, %rbx
	movq	%rbx, %r9
	jmp	.LBB20_9
.LBB20_4:                               # %sw.bb
	movq	$1, %rsi
.LBB20_5:                               # %sw.epilog
	movq	%r12, %rdi
	movq	%r13, %rdx
	movq	%rbx, %rcx
.LBB20_6:                               # %sw.epilog
	movq	%rax, %r8
	movq	%rax, %rbx
	movq	8(%rsp), %r9            # 8-byte Reload
.LBB20_9:                               # %sw.epilog
	callq	do_jump_by_parts_greater_rtx
.LBB20_10:                              # %sw.epilog
	movq	%rbx, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_label  # TAILCALL
.LBB20_24:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.do_cmp_and_jump, %rdx
	movq	$4760, %rsi             # imm = 0x1298
	callq	fancy_abort
.LBB20_22:                              # %if.then17
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.do_cmp_and_jump, %rcx
	movq	$4755, %rsi             # imm = 0x1293
	jmp	.LBB20_17
.LBB20_16:                              # %if.then9
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.do_cmp_and_jump, %rcx
	movq	$4749, %rsi             # imm = 0x128D
.LBB20_17:                              # %if.then9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp302:
	.size	do_cmp_and_jump, .Ltmp302-do_cmp_and_jump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI20_0:
	.quad	.LBB20_20
	.quad	.LBB20_14
	.quad	.LBB20_13
	.quad	.LBB20_12
	.quad	.LBB20_24
	.quad	.LBB20_11
	.quad	.LBB20_24
	.quad	.LBB20_24
	.quad	.LBB20_7
	.quad	.LBB20_4

	.text
	.globl	expand_mult_add
	.align	16, 0x90
	.type	expand_mult_add,@function
expand_mult_add:                        # @expand_mult_add
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
	subq	$16, %rsp
.Ltmp314:
	.cfi_def_cfa_offset 64
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
	movq	%r9, %r13
	movq	%rcx, %r15
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r14
	movq	%r8, %rdi
	movq	%r13, %rsi
	callq	type_for_mode
	movzbl	2(%r15), %edi
	movq	%rax, %rbx
	testq	%rdi, %rdi
	movq	%rbx, %r12
	je	.LBB21_2
# BB#1:                                 # %cond.false
	movq	%r13, %rsi
	callq	type_for_mode
	movq	%rax, %r12
.LBB21_2:                               # %cond.end
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	make_tree
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	make_tree
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	make_tree
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_expr  # TAILCALL
.Ltmp320:
	.size	expand_mult_add, .Ltmp320-expand_mult_add
	.cfi_endproc

	.globl	emit_store_flag_force
	.align	16, 0x90
	.type	emit_store_flag_force,@function
emit_store_flag_force:                  # @emit_store_flag_force
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
	subq	$48, %rsp
.Ltmp332:
	.cfi_def_cfa_offset 96
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
	movq	96(%rsp), %rax
	movq	%r9, %r15
	movq	%r8, %rbx
	movq	%rcx, %r14
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %r13
	movq	%rax, (%rsp)
                                        # kill: RDI<def> R13<kill>
                                        # kill: RSI<def> R12<kill>
                                        # kill: RCX<def> R14<kill>
                                        # kill: R8<def> RBX<kill>
                                        # kill: R9<def> R15<kill>
	callq	emit_store_flag
	testq	%rax, %rax
	jne	.LBB22_6
# BB#1:                                 # %if.end
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	%r13, %rbx
	jne	.LBB22_4
# BB#2:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB22_4
# BB#3:                                 # %lor.lhs.false6
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	reg_mentioned_p
	testq	%r15, %rax
	je	.LBB22_5
.LBB22_4:                               # %if.then9
	movzbl	2(%rbx), %edi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB22_5:                               # %if.end13
	movq	const_int_rtx+520(%rip), %rsi
	movq	%rbx, %rdi
	callq	emit_move_insn
	callq	gen_label_rtx
	movq	%rax, %r15
	movq	%r15, 8(%rsp)
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	do_compare_rtx_and_jump
	movq	const_int_rtx+512(%rip), %rsi
	movq	%rbx, %rdi
	callq	emit_move_insn
	movq	%r15, %rdi
	callq	emit_label
	movq	%rbx, %rax
.LBB22_6:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp338:
	.size	emit_store_flag_force, .Ltmp338-emit_store_flag_force
	.cfi_endproc

	.align	16, 0x90
	.type	extract_split_bit_field,@function
extract_split_bit_field:                # @extract_split_bit_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp345:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp346:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp347:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp348:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp349:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp350:
	.cfi_def_cfa_offset 128
.Ltmp351:
	.cfi_offset %rbx, -48
.Ltmp352:
	.cfi_offset %r12, -40
.Ltmp353:
	.cfi_offset %r13, -32
.Ltmp354:
	.cfi_offset %r14, -24
.Ltmp355:
	.cfi_offset %r15, -16
	movq	%rdi, %r11
	movq	%r11, 24(%rsp)          # 8-byte Spill
	movl	(%r11), %r8d
	movq	%rcx, %rbx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%r8, %rax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB23_2
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %r9d
	movq	$20, %rcx
	movq	%r9, %rax
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	jmp	.LBB23_9
.LBB23_2:                               # %if.else
	movq	16(%r11), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB23_4
# BB#3:                                 # %cond.true
	movl	32(%rax), %edx
.LBB23_4:                               # %cond.end
	movl	target_flags(%rip), %r9d
	movq	$20, %rcx
	movq	%r9, %rdi
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdi, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdx, %rdi
	cmpq	%rcx, %rdi
	jae	.LBB23_6
# BB#5:                                 # %cond.end
	movq	$8, 72(%rsp)            # 8-byte Folded Spill
.LBB23_6:                               # %cond.end
	jae	.LBB23_10
# BB#7:                                 # %cond.end
	testq	%rax, %rax
	je	.LBB23_10
# BB#8:                                 # %cond.true18
	movl	32(%rax), %eax
.LBB23_9:                               # %while.cond.preheader
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB23_10:                              # %while.cond.preheader
	xorq	%r13, %r13
	testq	%rsi, %rsi
	je	.LBB23_27
# BB#11:                                # %while.body.lr.ph
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, 72(%rsp)          # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$1, %r10
	xorq	%r12, %r12
	jmp	.LBB23_12
	.align	16, 0x90
.LBB23_38:                              # %while.cond.backedge.while.body_crit_edge
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movl	target_flags(%rip), %r9d
	movl	(%r11), %r8d
	xorq	%r10, %r10
.LBB23_12:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_31 Depth 2
	movq	32(%rsp), %rax          # 8-byte Reload
	leaq	(%r12,%rax), %rax
	xorq	%rdx, %rdx
	movq	$20, %rcx
	movq	%r9, %rdi
	shrq	%cl, %rdi
	divq	72(%rsp)                # 8-byte Folded Reload
	movq	%rax, %rbx
	movq	%rdx, %r14
	movq	%rsi, %r13
	subq	%r12, %r13
	andq	$32, %rdi
	addq	$32, %rdi
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %rdi
	cmpq	%rdi, %r13
	jb	.LBB23_14
# BB#13:                                # %while.body
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	%rdi, %r13
.LBB23_14:                              # %while.body
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	%r10, 64(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	subq	%r14, %rax
	cmpq	%rax, %r13
	jb	.LBB23_16
# BB#15:                                # %while.body
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	%rax, %r13
.LBB23_16:                              # %while.body
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB23_17
# BB#20:                                # %if.then92
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%r11, %rdi
	movq	%rbx, %rsi
	movq	%r8, %rdx
	jmp	.LBB23_21
	.align	16, 0x90
.LBB23_17:                              # %while.body
                                        #   in Loop: Header=BB23_12 Depth=1
	cmpq	$63, %rax
	jne	.LBB23_18
# BB#19:                                # %if.then70
                                        #   in Loop: Header=BB23_12 Depth=1
	movl	16(%r11), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	addq	$4, %r9
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %r9
	xorq	%rdx, %rdx
	divq	%r9
	movq	8(%r11), %rdi
	movzbl	2(%rdi), %edx
	addq	%rax, %rbx
	movq	%rbx, %rsi
.LBB23_21:                              # %if.end100
                                        #   in Loop: Header=BB23_12 Depth=1
	callq	operand_subword_force
	movq	%rax, %rsi
	xorq	%rbx, %rbx
	jmp	.LBB23_22
	.align	16, 0x90
.LBB23_18:                              #   in Loop: Header=BB23_12 Depth=1
	movq	%r11, %rsi
.LBB23_22:                              # %if.end100
                                        #   in Loop: Header=BB23_12 Depth=1
	movl	word_mode(%rip), %edi
	movq	72(%rsp), %rax          # 8-byte Reload
	imulq	%rax, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	xorq	%r9, %r9
	movq	$1, (%rsp)
	movq	%rbx, %rdx
	movq	%r13, %rcx
	movq	%r14, %r8
	callq	extract_fixed_bit_field
	movq	%r12, %rcx
	addq	%r13, %rcx
	andq	%r15, %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	movq	%rcx, %rdi
	subq	%r13, %rdi
	je	.LBB23_23
# BB#30:                                # %if.then110
                                        #   in Loop: Header=BB23_12 Depth=1
	movl	word_mode(%rip), %r14d
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r13
	cmpq	%rcx, %r13
	jne	.LBB23_31
.LBB23_23:                              #   in Loop: Header=BB23_12 Depth=1
	movq	%r12, %rax
	jmp	.LBB23_24
	.align	16, 0x90
.LBB23_31:                              # %for.body.i
                                        #   Parent Loop BB23_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %rbx
	movabsq	$0, %rax
	je	.LBB23_35
# BB#32:                                # %for.body.i
                                        #   in Loop: Header=BB23_31 Depth=2
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB23_34
# BB#33:                                # %if.then13.i
                                        #   in Loop: Header=BB23_31 Depth=2
	movq	$2, %rax
	jmp	.LBB23_35
	.align	16, 0x90
.LBB23_34:                              # %if.else14.i
                                        #   in Loop: Header=BB23_31 Depth=2
	movq	$3, %rax
.LBB23_35:                              # %if.else79.i
                                        #   in Loop: Header=BB23_31 Depth=2
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB23_24
# BB#36:                                # %land.lhs.true94.i
                                        #   in Loop: Header=BB23_31 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB23_31
	jmp	.LBB23_37
	.align	16, 0x90
.LBB23_24:                              # %if.end115
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	movq	24(%rsp), %r11          # 8-byte Reload
	jne	.LBB23_25
# BB#39:                                # %if.else118
                                        #   in Loop: Header=BB23_12 Depth=1
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rax, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%r11, %rbx
	callq	expand_binop
	movq	%rbx, %r11
.LBB23_25:                              #   in Loop: Header=BB23_12 Depth=1
	movq	%rax, %r13
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	56(%rsp), %r12          # 8-byte Reload
	cmpq	%rsi, %r12
	jb	.LBB23_38
# BB#26:
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB23_27:                              # %while.end
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rbx
	jne	.LBB23_55
# BB#28:                                # %if.end123
	movq	%r13, %r12
	movl	target_flags(%rip), %edi
	movl	word_mode(%rip), %r14d
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %rdi
	subq	%rsi, %rdi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r13
	cmpq	%rcx, %r13
	je	.LBB23_29
	.align	16, 0x90
.LBB23_40:                              # %for.body.i189
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %rbx
	movabsq	$0, %rax
	je	.LBB23_44
# BB#41:                                # %for.body.i189
                                        #   in Loop: Header=BB23_40 Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB23_43
# BB#42:                                # %if.then13.i190
                                        #   in Loop: Header=BB23_40 Depth=1
	movq	$2, %rax
	jmp	.LBB23_44
	.align	16, 0x90
.LBB23_43:                              # %if.else14.i191
                                        #   in Loop: Header=BB23_40 Depth=1
	movq	$3, %rax
.LBB23_44:                              # %if.else79.i193
                                        #   in Loop: Header=BB23_40 Depth=1
	movq	optab_table+168(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB23_46
# BB#45:                                # %if.else79.i193
                                        #   in Loop: Header=BB23_40 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB23_40
.LBB23_46:                              # %for.end.i198
	testq	%rax, %rax
	movq	%rax, %r13
	jne	.LBB23_47
	jmp	.LBB23_56
.LBB23_29:
	movq	%r12, %r13
.LBB23_47:                              # %expand_shift.exit200
	movl	target_flags(%rip), %edi
	movl	word_mode(%rip), %r14d
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movabsq	$4294967264, %rax       # imm = 0xFFFFFFE0
	andq	%rax, %rdi
	movq	40(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	build_int_2_wide
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %r12
	cmpq	%rcx, %r12
	je	.LBB23_55
	.align	16, 0x90
.LBB23_48:                              # %for.body.i208
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %rbx
	movabsq	$0, %rax
	je	.LBB23_52
# BB#49:                                # %for.body.i208
                                        #   in Loop: Header=BB23_48 Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB23_51
# BB#50:                                # %if.then13.i209
                                        #   in Loop: Header=BB23_48 Depth=1
	movq	$2, %rax
	jmp	.LBB23_52
	.align	16, 0x90
.LBB23_51:                              # %if.else14.i210
                                        #   in Loop: Header=BB23_48 Depth=1
	movq	$3, %rax
.LBB23_52:                              # %if.else79.i212
                                        #   in Loop: Header=BB23_48 Depth=1
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB23_54
# BB#53:                                # %if.else79.i212
                                        #   in Loop: Header=BB23_48 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB23_48
.LBB23_54:                              # %for.end.i220
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB23_56
.LBB23_55:                              # %return
	movq	%r13, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB23_37:                              # %for.end.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.expand_shift, %rdx
	movq	$2043, %rsi             # imm = 0x7FB
	callq	fancy_abort
.LBB23_56:                              # %if.then113.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_shift, %rcx
	movq	$2043, %rsi             # imm = 0x7FB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp356:
	.size	extract_split_bit_field, .Ltmp356-extract_split_bit_field
	.cfi_endproc

	.align	16, 0x90
	.type	store_split_bit_field,@function
store_split_bit_field:                  # @store_split_bit_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp363:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp364:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp365:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp366:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp367:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp368:
	.cfi_def_cfa_offset 96
.Ltmp369:
	.cfi_offset %rbx, -48
.Ltmp370:
	.cfi_offset %r12, -40
.Ltmp371:
	.cfi_offset %r13, -32
.Ltmp372:
	.cfi_offset %r14, -24
.Ltmp373:
	.cfi_offset %r15, -16
	movl	(%rdi), %eax
	movq	%rcx, %rbx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB24_2
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %r13d
	movq	$20, %rcx
	shrq	%cl, %r13
	andq	$32, %r13
	addq	$32, %r13
	jmp	.LBB24_9
.LBB24_2:                               # %if.else
	movq	16(%rdi), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	je	.LBB24_4
# BB#3:                                 # %cond.true
	movl	32(%rax), %edx
.LBB24_4:                               # %cond.end
	movl	target_flags(%rip), %r13d
	movq	$20, %rcx
	shrq	%cl, %r13
	andq	$32, %r13
	addq	$32, %r13
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%r13, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	%rcx, %rsi
	jae	.LBB24_6
# BB#5:                                 # %cond.end
	movq	$8, %r13
.LBB24_6:                               # %cond.end
	jae	.LBB24_9
# BB#7:                                 # %cond.end
	testq	%rax, %rax
	je	.LBB24_9
# BB#8:                                 # %cond.true18
	movl	32(%rax), %r13d
.LBB24_9:                               # %if.end
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movzbl	(%rbx), %eax
	cmpq	$133, %rax
	jg	.LBB24_15
# BB#10:                                # %if.end
	addq	$-55, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$15, %rcx
	jbe	.LBB24_12
# BB#11:
	movq	%rbx, %rsi
	jmp	.LBB24_25
.LBB24_15:                              # %if.end
	cmpq	$134, %rax
	je	.LBB24_18
# BB#16:                                # %if.end
	cmpq	$140, %rax
	je	.LBB24_18
# BB#17:
	movq	%rbx, %rsi
	jmp	.LBB24_25
.LBB24_12:                              # %if.end
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$12299, %rax            # imm = 0x300B
	je	.LBB24_13
.LBB24_18:                              # %if.then66
	movl	word_mode(%rip), %edi
	movq	%rbx, %rsi
	callq	gen_lowpart_common
	movq	%rax, %rsi
	testq	%rsi, %rsi
	je	.LBB24_20
# BB#19:                                # %if.then66
	cmpq	%rbx, %rsi
	jne	.LBB24_25
.LBB24_20:                              # %if.else71
	movl	word_mode(%rip), %r14d
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	movq	%r14, %rdi
	je	.LBB24_22
# BB#21:                                # %if.else71
	movq	%rax, %rdi
.LBB24_22:                              # %if.else71
	movq	%rbx, %rsi
	callq	force_reg
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_lowpart_common
.LBB24_24:                              # %while.cond.preheader
	movq	%rax, %rsi
.LBB24_25:                              # %while.cond.preheader
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	je	.LBB24_40
# BB#26:                                # %while.body.lr.ph
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	%r13, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB24_27:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	(%rbx,%rax), %rax
	xorq	%rdx, %rdx
	divq	%r13
	movq	%rax, %r12
	movq	%rdx, %r14
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	subq	%rbx, %r15
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rax
	cmpq	%rax, %r15
	jb	.LBB24_29
# BB#28:                                # %while.body
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	%rax, %r15
.LBB24_29:                              # %while.body
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	%r13, %rax
	subq	%r14, %rax
	cmpq	%rax, %r15
	jb	.LBB24_31
# BB#30:                                # %while.body
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	%rax, %r15
.LBB24_31:                              # %while.body
                                        #   in Loop: Header=BB24_27 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$54, %rax
	jne	.LBB24_33
# BB#32:                                # %if.then132
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	8(%rsi), %rax
	movq	%rbx, %rcx
	shrq	%cl, %rax
	movq	$1, %rsi
	movq	%r15, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	andq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB24_34
	.align	16, 0x90
.LBB24_33:                              # %if.else138
                                        #   in Loop: Header=BB24_27 Depth=1
	movl	word_mode(%rip), %edi
	xorq	%rdx, %rdx
	xorq	%r9, %r9
	movq	$1, (%rsp)
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	extract_fixed_bit_field
.LBB24_34:                              # %if.end141
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	%rax, %r13
	movq	24(%rsp), %rdi          # 8-byte Reload
	movl	(%rdi), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB24_35
# BB#37:                                # %if.then170
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	jmp	.LBB24_38
	.align	16, 0x90
.LBB24_35:                              # %if.end141
                                        #   in Loop: Header=BB24_27 Depth=1
	cmpq	$63, %rax
	jne	.LBB24_39
# BB#36:                                # %if.then146
                                        #   in Loop: Header=BB24_27 Depth=1
	movl	16(%rdi), %eax
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rsi
	xorq	%rdx, %rdx
	divq	%rsi
	movq	8(%rdi), %rdi
	movzbl	2(%rdi), %edx
	addq	%rax, %r12
.LBB24_38:                              # %if.end178
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	%r12, %rsi
	callq	operand_subword_force
	movq	%rax, %rdi
	xorq	%r12, %r12
.LBB24_39:                              # %if.end178
                                        #   in Loop: Header=BB24_27 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	imulq	%rax, %r12
	movq	$3, %rcx
	shrq	%cl, %r12
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	%r13, %r8
	movq	%rax, %r13
	callq	store_fixed_bit_field
	addq	%r15, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	32(%rsp), %r15          # 8-byte Reload
	cmpq	%r15, %rbx
	movq	16(%rsp), %rsi          # 8-byte Reload
	jb	.LBB24_27
.LBB24_40:                              # %while.end
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_13:                              # %if.end
	cmpq	$15, %rcx
	jne	.LBB24_14
# BB#23:                                # %if.then89
	movq	%rbx, %rdi
	callq	copy_to_reg
	jmp	.LBB24_24
.LBB24_14:
	movq	%rbx, %rsi
	jmp	.LBB24_25
.Ltmp374:
	.size	store_split_bit_field, .Ltmp374-store_split_bit_field
	.cfi_endproc

	.type	zero_cost,@object       # @zero_cost
	.local	zero_cost
	.comm	zero_cost,4,4
	.type	add_cost,@object        # @add_cost
	.local	add_cost
	.comm	add_cost,4,4
	.type	shift_cost,@object      # @shift_cost
	.local	shift_cost
	.comm	shift_cost,256,16
	.type	shiftsub_cost,@object   # @shiftsub_cost
	.local	shiftsub_cost
	.comm	shiftsub_cost,256,16
	.type	shiftadd_cost,@object   # @shiftadd_cost
	.local	shiftadd_cost
	.comm	shiftadd_cost,256,16
	.type	negate_cost,@object     # @negate_cost
	.local	negate_cost
	.comm	negate_cost,4,4
	.type	sdiv_pow2_cheap,@object # @sdiv_pow2_cheap
	.local	sdiv_pow2_cheap
	.comm	sdiv_pow2_cheap,4,4
	.type	smod_pow2_cheap,@object # @smod_pow2_cheap
	.local	smod_pow2_cheap
	.comm	smod_pow2_cheap,4,4
	.type	div_cost,@object        # @div_cost
	.local	div_cost
	.comm	div_cost,236,16
	.type	mul_cost,@object        # @mul_cost
	.local	mul_cost
	.comm	mul_cost,236,16
	.type	mul_widen_cost,@object  # @mul_widen_cost
	.local	mul_widen_cost
	.comm	mul_widen_cost,236,16
	.type	mul_highpart_cost,@object # @mul_highpart_cost
	.local	mul_highpart_cost
	.comm	mul_highpart_cost,236,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"expmed.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.mode_for_extraction,@object # @__FUNCTION__.mode_for_extraction
.L__FUNCTION__.mode_for_extraction:
	.asciz	"mode_for_extraction"
	.size	.L__FUNCTION__.mode_for_extraction, 20

	.type	.L__FUNCTION__.store_bit_field,@object # @__FUNCTION__.store_bit_field
.L__FUNCTION__.store_bit_field:
	.asciz	"store_bit_field"
	.size	.L__FUNCTION__.store_bit_field, 16

	.type	.L__FUNCTION__.extract_bit_field,@object # @__FUNCTION__.extract_bit_field
.L__FUNCTION__.extract_bit_field:
	.asciz	"extract_bit_field"
	.size	.L__FUNCTION__.extract_bit_field, 18

	.type	.L__FUNCTION__.expand_shift,@object # @__FUNCTION__.expand_shift
.L__FUNCTION__.expand_shift:
	.asciz	"expand_shift"
	.size	.L__FUNCTION__.expand_shift, 13

	.type	.L__FUNCTION__.expand_mult,@object # @__FUNCTION__.expand_mult
.L__FUNCTION__.expand_mult:
	.asciz	"expand_mult"
	.size	.L__FUNCTION__.expand_mult, 12

	.type	.L__FUNCTION__.expand_mult_highpart,@object # @__FUNCTION__.expand_mult_highpart
.L__FUNCTION__.expand_mult_highpart:
	.asciz	"expand_mult_highpart"
	.size	.L__FUNCTION__.expand_mult_highpart, 21

	.type	expand_divmod.last_div_const,@object # @expand_divmod.last_div_const
	.local	expand_divmod.last_div_const
	.comm	expand_divmod.last_div_const,8,8
	.type	.L__FUNCTION__.expand_divmod,@object # @__FUNCTION__.expand_divmod
.L__FUNCTION__.expand_divmod:
	.asciz	"expand_divmod"
	.size	.L__FUNCTION__.expand_divmod, 14

	.type	.L__FUNCTION__.do_cmp_and_jump,@object # @__FUNCTION__.do_cmp_and_jump
.L__FUNCTION__.do_cmp_and_jump:
	.asciz	"do_cmp_and_jump"
	.size	.L__FUNCTION__.do_cmp_and_jump, 16

	.type	.L__FUNCTION__.choose_multiplier,@object # @__FUNCTION__.choose_multiplier
.L__FUNCTION__.choose_multiplier:
	.asciz	"choose_multiplier"
	.size	.L__FUNCTION__.choose_multiplier, 18

	.type	.L__FUNCTION__.store_fixed_bit_field,@object # @__FUNCTION__.store_fixed_bit_field
.L__FUNCTION__.store_fixed_bit_field:
	.asciz	"store_fixed_bit_field"
	.size	.L__FUNCTION__.store_fixed_bit_field, 22


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
