	.file	"caller-save.c.bc"
	.text
	.globl	init_caller_save
	.align	16, 0x90
	.type	init_caller_save,@function
init_caller_save:                       # @init_caller_save
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	xorq	%r9, %r9
	movabsq	$regno_save_mode+4, %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_1:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_8 Depth 2
                                        #     Child Loop BB0_5 Depth 2
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	call_used_regs(%rdx), %eax
	testq	%rax, %rax
	je	.LBB0_14
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB0_1 Depth=1
	movzbl	call_fixed_regs(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB0_14
# BB#3:                                 # %for.cond4.preheader
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_15
# BB#4:                                 # %for.body8.lr.ph
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	-17(%rdi), %rdx
	andq	%r8, %rdx
	movq	$1, %r12
	movq	$1, %rsi
	movq	%rdi, %rcx
	shlq	%cl, %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	cmpq	$1, %rdx
	movq	%rbx, %r15
	ja	.LBB0_8
	.align	16, 0x90
.LBB0_5:                                # %cond.end18.us
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%r9d, -4(%rbx,%r12,4)
	movq	%r12, %rcx
	andq	%r8, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_7
# BB#6:                                 # %if.then33.us
                                        #   in Loop: Header=BB0_5 Depth=2
	movq	$1, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movb	%cl, call_fixed_regs(%rdx)
	movq	(%rsp), %rcx            # 8-byte Reload
	orq	%rcx, call_fixed_reg_set(%rip)
.LBB0_7:                                # %for.inc.us
                                        #   in Loop: Header=BB0_5 Depth=2
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB0_5
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_8:                                # %for.body8
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %r14
	movq	%r8, %r13
	andq	%r13, %r14
	cmpq	$1, %r14
	jne	.LBB0_9
# BB#10:                                # %cond.end18
                                        #   in Loop: Header=BB0_8 Depth=2
	movq	$1, %rsi
	callq	choose_hard_reg_mode
	xorq	%r9, %r9
	testq	%r13, %rax
	movq	%r13, %r8
	movl	%eax, (%r15)
	jne	.LBB0_13
# BB#11:                                # %cond.end18
                                        #   in Loop: Header=BB0_8 Depth=2
	cmpq	$1, %r14
	jne	.LBB0_13
# BB#12:                                # %if.then33
                                        #   in Loop: Header=BB0_8 Depth=2
	movq	$1, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movb	%al, call_fixed_regs(%rcx)
	movq	(%rsp), %rax            # 8-byte Reload
	orq	%rax, call_fixed_reg_set(%rip)
	jmp	.LBB0_13
	.align	16, 0x90
.LBB0_9:                                # %cond.end18.thread
                                        #   in Loop: Header=BB0_8 Depth=2
	movq	%r13, %r8
	movl	%r9d, (%r15)
.LBB0_13:                               # %for.inc
                                        #   in Loop: Header=BB0_8 Depth=2
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	movl	target_flags(%rip), %edi
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	addq	$4, %rdi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4, %r15
	cmpq	%rax, %rsi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	jl	.LBB0_8
	jmp	.LBB0_15
	.align	16, 0x90
.LBB0_14:                               # %if.else
                                        #   in Loop: Header=BB0_1 Depth=1
	leaq	(%rdx,%rdx,4), %rax
	movl	%r9d, regno_save_mode+4(,%rax,4)
.LBB0_15:                               # %for.inc40
                                        #   in Loop: Header=BB0_1 Depth=1
	incq	%rdi
	addq	$20, %rbx
	cmpq	$53, %rdi
	jne	.LBB0_1
# BB#16:                                # %for.cond43.preheader
	movq	reg_class_contents+96(%rip), %rdx
	xorq	%rax, %rax
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB0_18:                               # %for.body46
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	movq	%rax, %rsi
	jne	.LBB0_19
# BB#17:                                # %for.cond43
                                        #   in Loop: Header=BB0_18 Depth=1
	incq	%rax
	incq	%rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$52, %rsi
	movq	%rdi, %rsi
	jle	.LBB0_18
.LBB0_19:                               # %for.end55
	movq	%rsi, %rax
	movq	%r8, %r13
	andq	%r13, %rax
	cmpq	$53, %rax
	je	.LBB0_59
# BB#20:                                # %if.end59
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_rtx_REG
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$65536, %r14            # imm = 0x10000
	.align	16, 0x90
.LBB0_21:                               # %for.body66
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_22 Depth 2
	movq	%r13, %r15
	movl	target_flags(%rip), %r13d
	movq	$25, %rcx
	shrq	%cl, %r13
	andq	$1, %r13
	orq	$4, %r13
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	callq	gen_rtx_CONST_INT
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$1, %r13
	movabsq	$regno_save_mode+4, %rbx
	.align	16, 0x90
.LBB0_22:                               # %for.body76
                                        #   Parent Loop BB0_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rbx), %edi
	testq	%rdi, %rdi
	je	.LBB0_27
# BB#23:                                # %land.lhs.true82
                                        #   in Loop: Header=BB0_22 Depth=2
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	strict_memory_address_p
	testq	%r15, %rax
	je	.LBB0_24
.LBB0_27:                               # %for.inc90
                                        #   in Loop: Header=BB0_22 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r13
	addq	$20, %rbx
	cmpq	$53, %rax
	jl	.LBB0_22
	jmp	.LBB0_28
	.align	16, 0x90
.LBB0_24:                               #   in Loop: Header=BB0_21 Depth=1
	decq	%r13
	movq	%r13, %r12
.LBB0_28:                               # %for.end92
                                        #   in Loop: Header=BB0_21 Depth=1
	movq	%r15, %r13
	andq	%r13, %r12
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$33, %rcx
	sarq	%cl, %r14
	cmpq	$53, %r12
	je	.LBB0_29
# BB#25:                                # %for.cond64
                                        #   in Loop: Header=BB0_21 Depth=1
	testq	%r13, %r14
	jne	.LBB0_21
# BB#26:
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB0_29:                               # %for.end98
	callq	start_sequence
	xorq	%rax, %rax
	xorq	%r14, %r14
	.align	16, 0x90
.LBB0_30:                               # %for.cond107.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_31 Depth 2
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r15
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB0_31:                               # %for.body110
                                        #   Parent Loop BB0_30 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB0_43
# BB#32:                                # %if.then113
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_rtx_MEM
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_REG
	movq	%rax, %r13
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	emit_insn
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	emit_insn
	movslq	40(%r13), %rcx
	movq	%rax, %r12
	testq	%rcx, %rcx
	jns	.LBB0_34
# BB#33:                                # %cond.false127
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	%r13, %rdi
	callq	recog_memoized_1
	movq	%rax, %rcx
.LBB0_34:                               # %cond.end129
                                        #   in Loop: Header=BB0_31 Depth=2
	movl	%ecx, reg_save_code(,%r15,4)
	movslq	40(%r12), %rax
	testq	%rax, %rax
	jns	.LBB0_36
# BB#35:                                # %cond.false144
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	%r12, %rdi
	callq	recog_memoized_1
	movl	reg_save_code(,%r15,4), %ecx
.LBB0_36:                               # %cond.end146
                                        #   in Loop: Header=BB0_31 Depth=2
	movl	%eax, reg_restore_code(,%r15,4)
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rdx, %rax
	movabsq	$0, %rax
	je	.LBB0_38
# BB#37:                                # %cond.end146
                                        #   in Loop: Header=BB0_31 Depth=2
	movabsq	$1, %rax
.LBB0_38:                               # %cond.end146
                                        #   in Loop: Header=BB0_31 Depth=2
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	movq	%rdx, %r14
	movabsq	$0, %rcx
	je	.LBB0_40
# BB#39:                                # %cond.end146
                                        #   in Loop: Header=BB0_31 Depth=2
	movabsq	$1, %rcx
.LBB0_40:                               # %cond.end146
                                        #   in Loop: Header=BB0_31 Depth=2
	andq	%rcx, %rax
	je	.LBB0_42
# BB#41:                                # %if.then165
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	%r13, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	movq	%rax, %r13
	movq	%r12, %rdi
	callq	extract_insn
	movq	$1, %rdi
	callq	constrain_operands
	andq	%r13, %rax
.LBB0_42:                               # %if.end169
                                        #   in Loop: Header=BB0_31 Depth=2
	movq	%r14, %r13
	testq	%r13, %rax
	movq	16(%rsp), %r14          # 8-byte Reload
	jne	.LBB0_44
.LBB0_43:                               # %if.else181
                                        #   in Loop: Header=BB0_31 Depth=2
	movl	%r13d, reg_save_code(,%r15,4)
	movl	%r13d, reg_restore_code(,%r15,4)
.LBB0_44:                               # %for.inc191
                                        #   in Loop: Header=BB0_31 Depth=2
	incq	%rbx
	incq	%r15
	cmpq	$59, %rbx
	jne	.LBB0_31
# BB#45:                                # %for.inc194
                                        #   in Loop: Header=BB0_30 Depth=1
	incq	%r14
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	$59, %rax
	cmpq	$53, %r14
	jne	.LBB0_30
# BB#46:                                # %for.cond197.preheader
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	xorq	%r10, %r10
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB0_51
# BB#47:
	movabsq	$regno_save_mode+4, %r8
	xorq	%r9, %r9
	.align	16, 0x90
.LBB0_48:                               # %for.body208.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_49 Depth 2
	movq	$32, %rcx
	movq	%r10, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$1, %rdi
	movq	$1, %r11
	movq	%r10, %rcx
	shlq	%cl, %r11
	movq	%r8, %rbx
	.align	16, 0x90
.LBB0_49:                               # %for.body208
                                        #   Parent Loop BB0_48 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rbx), %ecx
	imulq	$236, %r14, %rdx
	movl	reg_save_code(%rdx,%rcx,4), %ecx
	cmpq	%r13, %rcx
	jne	.LBB0_50
# BB#54:                                # %if.then219
                                        #   in Loop: Header=BB0_49 Depth=2
	movl	%r9d, (%rbx)
	movq	%rdi, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	movq	%rdi, %rdx
	jne	.LBB0_56
# BB#55:                                # %if.then226
                                        #   in Loop: Header=BB0_49 Depth=2
	movq	$1, %rdx
	movb	%dl, call_fixed_regs(%r14)
	orq	%r11, call_fixed_reg_set(%rip)
	jmp	.LBB0_56
	.align	16, 0x90
.LBB0_50:                               # %for.body208.for.inc234_crit_edge
                                        #   in Loop: Header=BB0_49 Depth=2
	movq	%rdi, %rdx
.LBB0_56:                               # %for.inc234
                                        #   in Loop: Header=BB0_49 Depth=2
	incq	%rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$4, %rbx
	cmpq	%rsi, %rdx
	jl	.LBB0_49
# BB#57:                                # %for.inc237
                                        #   in Loop: Header=BB0_48 Depth=1
	incq	%r10
	addq	$20, %r8
	cmpq	$53, %r10
	jne	.LBB0_48
	jmp	.LBB0_58
	.align	16, 0x90
.LBB0_51:                               # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	addq	$4, %r10
	movq	%r10, %rax
	andq	%r13, %rax
	cmpq	$52, %rax
	jne	.LBB0_51
# BB#52:
	movq	$52, %rax
	.align	16, 0x90
.LBB0_53:                               # %for.inc237.us
                                        # =>This Inner Loop Header: Depth=1
	incq	%rax
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$53, %rcx
	jne	.LBB0_53
.LBB0_58:                               # %for.end239
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	end_sequence  # TAILCALL
.LBB0_59:                               # %if.then58
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.init_caller_save, %rcx
	movq	$160, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	init_caller_save, .Ltmp17-init_caller_save
	.cfi_endproc

	.globl	init_save_areas
	.align	16, 0x90
	.type	init_save_areas,@function
init_save_areas:                        # @init_save_areas
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
	movl	target_flags(%rip), %edi
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	addq	$4, %rdi
	xorq	%rsi, %rsi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB1_1
# BB#5:                                 # %entry.entry.split_crit_edge
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %rdx
	movq	$8, %rbx
	jg	.LBB1_6
# BB#7:                                 # %entry.entry.split_crit_edge
	movq	$8, %r14
	jmp	.LBB1_8
.LBB1_1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_2:                                # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	addq	$4, %rsi
	movq	%rsi, %rcx
	andq	%rax, %rcx
	cmpq	$52, %rcx
	jne	.LBB1_2
# BB#3:
	movq	$52, %rcx
	.align	16, 0x90
.LBB1_4:                                # %for.inc6.us
                                        # =>This Inner Loop Header: Depth=1
	incq	%rcx
	movq	%rcx, %rdx
	andq	%rax, %rdx
	cmpq	$53, %rdx
	jne	.LBB1_4
	jmp	.LBB1_9
.LBB1_6:
	decq	%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	leaq	8(,%rcx,8), %r14
	.align	16, 0x90
.LBB1_8:                                # %for.inc6
                                        # =>This Inner Loop Header: Depth=1
	leaq	regno_save_mem(%rbx), %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	memset
	addq	$40, %rbx
	cmpq	$2128, %rbx             # imm = 0x850
	jne	.LBB1_8
.LBB1_9:                                # %for.end8
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp26:
	.size	init_save_areas, .Ltmp26-init_save_areas
	.cfi_endproc

	.globl	setup_save_areas
	.align	16, 0x90
	.type	setup_save_areas,@function
setup_save_areas:                       # @setup_save_areas
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp33:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp34:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp35:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp36:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp38:
	.cfi_def_cfa_offset 112
.Ltmp39:
	.cfi_offset %rbx, -48
.Ltmp40:
	.cfi_offset %r12, -40
.Ltmp41:
	.cfi_offset %r13, -32
.Ltmp42:
	.cfi_offset %r14, -24
.Ltmp43:
	.cfi_offset %r15, -16
	movslq	max_regno(%rip), %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	cmpq	$54, %rsi
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	jl	.LBB2_20
# BB#1:                                 # %for.body.lr.ph
	movq	reg_renumber(%rip), %rdi
	movq	reg_n_info(%rip), %r15
	movq	cfun(%rip), %r8
	movl	target_flags(%rip), %r9d
	movq	$23, %rcx
	movq	%r9, %r10
	shrq	%cl, %r10
	andq	$4, %r10
	addq	$4, %r10
	movq	$25, %rcx
	movq	%r9, %r11
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	movq	$24, %rcx
	shrq	%cl, %r9
	andq	$2, %r9
	xorq	$6, %r9
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$53, %rbx
	movabsq	$0, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_16 Depth 2
	movswq	(%rdi,%rbx,2), %r12
	testq	%r12, %r12
	js	.LBB2_19
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	32(%r15,%rbx,8), %rax
	movslq	32(%rax), %rax
	testq	%rax, %rax
	jle	.LBB2_19
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	24(%r8), %rax
	movq	80(%rax), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	2(%rax), %ecx
	movq	%r12, %rax
	andq	$65528, %rax            # imm = 0xFFF8
	cmpq	$8, %rax
	je	.LBB2_8
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	leaq	-21(%r12), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB2_8
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	leaq	-45(%r12), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB2_8
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%r12, %rax
	addq	$-29, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$7, %rax
	jbe	.LBB2_8
# BB#12:                                # %cond.false
                                        #   in Loop: Header=BB2_2 Depth=1
	cmpq	$18, %rcx
	movq	%r11, %rax
	je	.LBB2_15
# BB#13:                                # %cond.false
                                        #   in Loop: Header=BB2_2 Depth=1
	cmpq	$24, %rcx
	movq	%r9, %rax
	je	.LBB2_15
# BB#14:                                # %cond.false74
                                        #   in Loop: Header=BB2_2 Depth=1
	movzbl	mode_size(%rcx), %eax
	leaq	-1(%rax,%r10), %rax
	cqto
	idivq	%r10
	jmp	.LBB2_15
.LBB2_8:                                # %cond.true
                                        #   in Loop: Header=BB2_2 Depth=1
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB2_15
# BB#9:                                 # %lor.rhs
                                        #   in Loop: Header=BB2_2 Depth=1
	cmpq	$6, %rcx
	movq	%r14, %rax
	jne	.LBB2_11
# BB#10:                                # %lor.rhs
                                        #   in Loop: Header=BB2_2 Depth=1
	movabsq	$1, %rax
.LBB2_11:                               # %lor.rhs
                                        #   in Loop: Header=BB2_2 Depth=1
	incq	%rax
.LBB2_15:                               # %cond.end94
                                        #   in Loop: Header=BB2_2 Depth=1
	movq	%r12, %rcx
	andq	%r13, %rcx
	addq	%r12, %rax
	andq	%r13, %rax
	cmpq	%rax, %rcx
	jae	.LBB2_19
	.align	16, 0x90
.LBB2_16:                               # %for.body100
                                        #   Parent Loop BB2_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB2_18
# BB#17:                                # %if.then104
                                        #   in Loop: Header=BB2_16 Depth=2
	movq	$1, %rdx
	shlq	%cl, %rdx
	orq	%rdx, 48(%rsp)          # 8-byte Folded Spill
.LBB2_18:                               # %for.inc
                                        #   in Loop: Header=BB2_16 Depth=2
	incq	%r12
	movq	%r12, %rdx
	andq	%r13, %rdx
	incq	%rcx
	cmpq	%rax, %rdx
	jne	.LBB2_16
	.align	16, 0x90
.LBB2_19:                               # %for.inc106
                                        #   in Loop: Header=BB2_2 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB2_2
.LBB2_20:                               # %for.cond109.preheader
	movabsq	$regno_save_mem+8, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$4, 16(%rsp)            # 8-byte Folded Spill
	movabsq	$regno_save_mem, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB2_21:                               # %for.body112
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_23 Depth 2
                                        #       Child Loop BB2_36 Depth 3
                                        #       Child Loop BB2_38 Depth 3
                                        #       Child Loop BB2_40 Depth 3
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %r14
	movq	$32, %rcx
                                        # kill: RAX<def> R14<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_26
# BB#22:                                # %for.body120.lr.ph
                                        #   in Loop: Header=BB2_21 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	leaq	regno_save_mem+8(,%rax,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB2_23:                               # %for.body120
                                        #   Parent Loop BB2_21 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_36 Depth 3
                                        #       Child Loop BB2_38 Depth 3
                                        #       Child Loop BB2_40 Depth 3
	movq	$2, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	regno_save_mode(%rax,%rcx,4), %edi
	movq	$32, %rcx
	movq	%rdi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	testq	%rdi, %rdi
	je	.LBB2_25
# BB#24:                                # %lor.lhs.false127
                                        #   in Loop: Header=BB2_23 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	cmpq	$0, (%rax)
	movq	$1, %rax
	movq	32(%rsp), %rsi          # 8-byte Reload
	jne	.LBB2_25
	.align	16, 0x90
.LBB2_36:                               # %for.body138
                                        #   Parent Loop BB2_21 Depth=1
                                        #     Parent Loop BB2_23 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	$0, (%rsi)
	jne	.LBB2_25
# BB#35:                                # %for.cond135
                                        #   in Loop: Header=BB2_36 Depth=3
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rax
	addq	$40, %rsi
	cmpq	%rbx, %rdx
	movq	$1, %rbx
	jl	.LBB2_36
	.align	16, 0x90
.LBB2_38:                               # %for.body155
                                        #   Parent Loop BB2_21 Depth=1
                                        #     Parent Loop BB2_23 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	56(%rsp), %rax          # 8-byte Reload
	leaq	-1(%rax,%rbx), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB2_25
# BB#37:                                # %for.cond152
                                        #   in Loop: Header=BB2_38 Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB2_38
# BB#39:                                # %for.body187.lr.ph
                                        #   in Loop: Header=BB2_23 Depth=2
	movzbl	mode_size(%r8), %esi
	xorq	%rdx, %rdx
	callq	assign_stack_local
	movq	%rax, %rdi
	movq	$3, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	regno_save_mem(%rax,%rcx,8), %rbx
	movq	%rdi, regno_save_mem(%rax,%rcx,8)
	movq	$1, %r13
	movq	16(%rsp), %r12          # 8-byte Reload
	jmp	.LBB2_40
	.align	16, 0x90
.LBB2_41:                               # %for.body187.for.body187_crit_edge
                                        #   in Loop: Header=BB2_40 Depth=3
	movq	(%rbx), %rdi
	incq	%r13
	addq	$20, %r12
.LBB2_40:                               # %for.body187
                                        #   Parent Loop BB2_21 Depth=1
                                        #     Parent Loop BB2_23 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	leaq	-1(%r13), %rdx
	imulq	%rax, %rdx
	movq	$32, %rcx
	movl	regno_save_mode(%r12), %esi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	callq	adjust_address_1
	movq	%rax, regno_save_mem(%r12,%r12)
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r14, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_41
	.align	16, 0x90
.LBB2_25:                               # %for.inc208
                                        #   in Loop: Header=BB2_23 Depth=2
	decq	%r15
	decq	%r14
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_23
.LBB2_26:                               # %for.inc210
                                        #   in Loop: Header=BB2_21 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	addq	$40, 32(%rsp)           # 8-byte Folded Spill
	addq	$20, 16(%rsp)           # 8-byte Folded Spill
	cmpq	$53, %rax
	jne	.LBB2_21
# BB#27:
	xorq	%r15, %r15
	.align	16, 0x90
.LBB2_28:                               # %for.body216
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_30 Depth 2
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	%rax, %r12
	movq	$32, %rcx
                                        # kill: RAX<def> R12<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_33
# BB#29:                                # %for.body224.lr.ph
                                        #   in Loop: Header=BB2_28 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	(%rax,%r12,8), %rbx
	decq	%r12
	.align	16, 0x90
.LBB2_30:                               # %for.body224
                                        #   Parent Loop BB2_28 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %r14
	testq	%r14, %r14
	je	.LBB2_32
# BB#31:                                # %if.then231
                                        #   in Loop: Header=BB2_30 Depth=2
	callq	get_frame_alias_set
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	set_mem_alias_set
.LBB2_32:                               # %for.inc238
                                        #   in Loop: Header=BB2_30 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r12
	addq	$-8, %rbx
	testq	%rax, %rax
	jg	.LBB2_30
.LBB2_33:                               # %for.inc241
                                        #   in Loop: Header=BB2_28 Depth=1
	incq	%r15
	addq	$40, 8(%rsp)            # 8-byte Folded Spill
	cmpq	$53, %r15
	jne	.LBB2_28
# BB#34:                                # %for.end243
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp44:
	.size	setup_save_areas, .Ltmp44-setup_save_areas
	.cfi_endproc

	.globl	save_call_clobbered_regs
	.align	16, 0x90
	.type	save_call_clobbered_regs,@function
save_call_clobbered_regs:               # @save_call_clobbered_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp51:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp52:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp53:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp54:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp56:
	.cfi_def_cfa_offset 336
.Ltmp57:
	.cfi_offset %rbx, -48
.Ltmp58:
	.cfi_offset %r12, -40
.Ltmp59:
	.cfi_offset %r13, -32
.Ltmp60:
	.cfi_offset %r14, -24
.Ltmp61:
	.cfi_offset %r15, -16
	movq	reload_insn_chain(%rip), %r12
	movq	$0, hard_regs_saved(%rip)
	xorq	%rax, %rax
	movl	%eax, n_regs_saved(%rip)
	testq	%r12, %r12
	je	.LBB3_113
# BB#1:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_11 Depth 2
                                        #     Child Loop BB3_17 Depth 2
                                        #     Child Loop BB3_24 Depth 2
                                        #       Child Loop BB3_25 Depth 3
                                        #         Child Loop BB3_27 Depth 4
                                        #           Child Loop BB3_57 Depth 5
                                        #     Child Loop BB3_64 Depth 2
                                        #       Child Loop BB3_70 Depth 3
                                        #         Child Loop BB3_73 Depth 4
                                        #       Child Loop BB3_95 Depth 3
                                        #     Child Loop BB3_100 Depth 2
                                        #     Child Loop BB3_106 Depth 2
	movq	%r12, %rcx
	movzbl	262(%rcx), %eax
	testq	$8, %rax
	jne	.LBB3_3
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	32(%rcx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movzbl	(%rax), %r14d
	movzbl	rtx_class(%r14), %eax
	movq	(%rcx), %r12
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%r12, 8(%rsp)           # 8-byte Spill
	cmpq	$105, %rax
	jne	.LBB3_103
# BB#6:                                 # %if.then7
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	n_regs_saved(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_15
# BB#7:                                 # %if.then9
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r8, %r13
	cmpq	$33, %r14
	jne	.LBB3_9
# BB#8:                                 # %if.then12
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	hard_regs_saved(%rip), %rax
	jmp	.LBB3_10
.LBB3_9:                                # %if.else
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$0, referenced_regs(%rip)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdi
	callq	mark_referenced_regs
	movq	hard_regs_saved(%rip), %rcx
	movq	referenced_regs(%rip), %rax
	andq	%rcx, %rax
.LBB3_10:                               # %if.end14
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%rax, referenced_regs(%rip)
	xorq	%rbx, %rbx
	jmp	.LBB3_11
	.align	16, 0x90
.LBB3_14:                               # %for.inc.for.body18_crit_edge
                                        #   in Loop: Header=BB3_11 Depth=2
	movq	%r8, %r13
	movq	referenced_regs(%rip), %rax
.LBB3_11:                               # %for.body18
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB3_13
# BB#12:                                # %if.then21
                                        #   in Loop: Header=BB3_11 Depth=2
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	leaq	64(%rsp), %r8
	callq	insert_restore
	addq	%rax, %rbx
.LBB3_13:                               # %for.inc
                                        #   in Loop: Header=BB3_11 Depth=2
	movq	%r13, %r8
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB3_14
.LBB3_15:                               # %if.end25
                                        #   in Loop: Header=BB3_2 Depth=1
	cmpq	$34, %r14
	jne	.LBB3_103
# BB#16:                                # %do.body
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r8, %r14
	movq	$0, 56(%rsp)
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	40(%rax), %rbx
	leaq	56(%rsp), %rdi
	movq	%rbx, %rsi
	callq	reg_set_to_hard_reg_set
	movq	56(%rsp), %rax
	xorq	%rcx, %rcx
	movabsq	$regno_save_mode+4, %rdx
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB3_17:                               # %for.body33
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rax
	jne	.LBB3_18
# BB#19:                                # %if.else44
                                        #   in Loop: Header=BB3_17 Depth=2
	movl	%edi, 64(%rsp,%rcx,4)
	jmp	.LBB3_20
	.align	16, 0x90
.LBB3_18:                               # %if.then38
                                        #   in Loop: Header=BB3_17 Depth=2
	movl	(%rdx), %esi
	movl	%esi, 64(%rsp,%rcx,4)
.LBB3_20:                               # %for.inc48
                                        #   in Loop: Header=BB3_17 Depth=2
	incq	%rcx
	addq	$20, %rdx
	cmpq	$53, %rcx
	jne	.LBB3_17
# BB#21:                                # %do.body51
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	(%rbx), %r10
	testq	%r10, %r10
	movq	%r14, %r9
	je	.LBB3_63
# BB#22:                                # %for.cond69.preheader.lr.ph
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	16(%r10), %eax
	testq	%rax, %rax
	movq	$53, %r13
	je	.LBB3_24
# BB#23:                                # %select.mid
                                        #   in Loop: Header=BB3_2 Depth=1
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_24:                               # %for.cond69.preheader
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_25 Depth 3
                                        #         Child Loop BB3_27 Depth 4
                                        #           Child Loop BB3_57 Depth 5
	movq	%r10, 32(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB3_25:                               # %for.body72
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_24 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_27 Depth 4
                                        #           Child Loop BB3_57 Depth 5
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	24(%r10,%rcx,8), %r12
	testq	%r12, %r12
	je	.LBB3_61
# BB#26:                                # %for.body81.preheader
                                        #   in Loop: Header=BB3_25 Depth=3
	movq	$6, %rcx
	movq	48(%rsp), %r11          # 8-byte Reload
	shlq	%cl, %r11
	movq	%r11, 40(%rsp)          # 8-byte Spill
	movq	%r13, %r14
	andq	%r9, %r14
	.align	16, 0x90
.LBB3_27:                               # %for.body81
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_24 Depth=2
                                        #       Parent Loop BB3_25 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB3_57 Depth 5
	movq	$1, %rax
	movq	%r14, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB3_60
# BB#28:                                # %if.then87
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	16(%r10), %esi
	movq	$7, %rcx
	shlq	%cl, %rsi
	leaq	(%r14,%r11), %rdx
	addq	%rsi, %rdx
	movq	$32, %rcx
	movq	reg_renumber(%rip), %rsi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movswq	(%rsi,%rdx,2), %rbx
	testq	%rbx, %rbx
	js	.LBB3_114
# BB#29:                                # %if.then98
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movl	(%rcx), %r8d
	notq	%rax
	andq	%rax, %r12
	movq	%rbx, %rax
	andq	$65528, %rax            # imm = 0xFFF8
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$8, %rax
	je	.LBB3_33
# BB#30:                                # %if.then98
                                        #   in Loop: Header=BB3_27 Depth=4
	leaq	-21(%rbx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB3_33
# BB#31:                                # %if.then98
                                        #   in Loop: Header=BB3_27 Depth=4
	leaq	-45(%rbx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB3_33
# BB#32:                                # %if.then98
                                        #   in Loop: Header=BB3_27 Depth=4
	leaq	-29(%rbx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$7, %rax
	jbe	.LBB3_33
# BB#37:                                # %cond.false
                                        #   in Loop: Header=BB3_27 Depth=4
	cmpq	$24, %rsi
	jne	.LBB3_38
# BB#40:                                # %cond.true165
                                        #   in Loop: Header=BB3_27 Depth=4
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB3_42
	.align	16, 0x90
.LBB3_33:                               # %cond.true
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	mode_class(,%rsi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB3_42
# BB#34:                                # %lor.rhs
                                        #   in Loop: Header=BB3_27 Depth=4
	cmpq	$6, %rax
	movabsq	$0, %r15
	jne	.LBB3_36
# BB#35:                                # %lor.rhs
                                        #   in Loop: Header=BB3_27 Depth=4
	movabsq	$1, %r15
.LBB3_36:                               # %lor.rhs
                                        #   in Loop: Header=BB3_27 Depth=4
	incq	%r15
	jmp	.LBB3_42
.LBB3_38:                               # %cond.false
                                        #   in Loop: Header=BB3_27 Depth=4
	cmpq	$18, %rsi
	jne	.LBB3_41
# BB#39:                                # %cond.true151
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB3_42
.LBB3_41:                               # %cond.false169
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %r15
	.align	16, 0x90
.LBB3_42:                               # %cond.end191
                                        #   in Loop: Header=BB3_27 Depth=4
	leaq	-17(%rbx), %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$2, %rcx
	jb	.LBB3_53
# BB#43:                                # %cond.false199
                                        #   in Loop: Header=BB3_27 Depth=4
	testq	%rsi, %rsi
	jne	.LBB3_45
# BB#44:                                # %cond.false199
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	%r15, %rcx
	andq	%r9, %rcx
	cmpq	$1, %rcx
	jne	.LBB3_53
.LBB3_45:                               # %cond.false213
                                        #   in Loop: Header=BB3_27 Depth=4
	testq	%rsi, %rsi
	jne	.LBB3_47
# BB#46:                                # %cond.true223
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	choose_hard_reg_mode
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	40(%rsp), %r11          # 8-byte Reload
	jmp	.LBB3_53
.LBB3_47:                               # %cond.false225
                                        #   in Loop: Header=BB3_27 Depth=4
	cmpq	$3, %rsi
	movq	32(%rsp), %r10          # 8-byte Reload
	movq	40(%rsp), %r11          # 8-byte Reload
	jne	.LBB3_49
# BB#48:                                # %land.lhs.true235
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	ix86_cpu(%rip), %ecx
	movq	$1, %rax
	shlq	%cl, %rax
	movl	x86_partial_reg_stall(%rip), %ecx
	andq	%rcx, %rax
	testq	%r9, %rax
	movq	$4, %rax
	je	.LBB3_53
.LBB3_49:                               # %cond.false240
                                        #   in Loop: Header=BB3_27 Depth=4
	andq	$16711680, %r8          # imm = 0xFF0000
	cmpq	$131072, %r8            # imm = 0x20000
	jne	.LBB3_52
# BB#50:                                # %cond.false240
                                        #   in Loop: Header=BB3_27 Depth=4
	cmpq	$4, %rbx
	jl	.LBB3_52
# BB#51:                                # %land.lhs.true253
                                        #   in Loop: Header=BB3_27 Depth=4
	movzbl	target_flags+3(%rip), %ecx
	movq	$4, %rax
	testq	$2, %rcx
	je	.LBB3_53
.LBB3_52:                               # %cond.false257
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	%rsi, %rax
	.align	16, 0x90
.LBB3_53:                               # %cond.end273
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movslq	64(%rsp,%rbx,4), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	movzbl	mode_bitsize(%rsi,%rsi), %edx
	cmpq	%rdx, %rcx
	jbe	.LBB3_55
# BB#54:                                # %if.then285
                                        #   in Loop: Header=BB3_27 Depth=4
	movl	%eax, 64(%rsp,%rbx,4)
.LBB3_55:                               # %while.cond289.preheader
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_59
# BB#56:                                # %while.body292.lr.ph
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	56(%rsp), %rdx
	.align	16, 0x90
.LBB3_57:                               # %while.body292
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_24 Depth=2
                                        #       Parent Loop BB3_25 Depth=3
                                        #         Parent Loop BB3_27 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%rdx, %rsi
	leaq	-1(%r15,%rbx), %rax
	leaq	-1(%r15), %r15
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	orq	%rsi, %rdx
	testq	%rdi, %rdi
	jg	.LBB3_57
# BB#58:                                # %while.cond289.if.end298_crit_edge
                                        #   in Loop: Header=BB3_27 Depth=4
	movq	%rdx, 56(%rsp)
.LBB3_59:                               # %if.end298
                                        #   in Loop: Header=BB3_27 Depth=4
	testq	%r12, %r12
	je	.LBB3_61
.LBB3_60:                               # %for.inc304
                                        #   in Loop: Header=BB3_27 Depth=4
	incq	%r13
	movq	%r13, %rax
	andq	%r9, %rax
	incq	%r14
	cmpq	$64, %rax
	jb	.LBB3_27
.LBB3_61:                               # %if.end307
                                        #   in Loop: Header=BB3_25 Depth=3
	movq	48(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	movq	%rcx, %rax
	andq	%r9, %rax
	xorq	%r13, %r13
	cmpq	$2, %rax
	jb	.LBB3_25
# BB#62:                                # %for.end310
                                        #   in Loop: Header=BB3_24 Depth=2
	movq	(%r10), %r10
	xorq	%r13, %r13
	testq	%r10, %r10
	movq	8(%rsp), %r12           # 8-byte Reload
	jne	.LBB3_24
.LBB3_63:                               # %do.end315
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	$0, this_insn_sets(%rip)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movabsq	$mark_set_regs, %rsi
	callq	note_stores
	movq	call_fixed_reg_set(%rip), %rax
	notq	%rax
	movq	56(%rsp), %rcx
	andq	%rax, %rcx
	movq	this_insn_sets(%rip), %rax
	movq	hard_regs_saved(%rip), %rdx
	notq	%rax
	movq	call_used_reg_set(%rip), %rsi
	andq	%rcx, %rax
	notq	%rdx
	andq	%rax, %rdx
	andq	%rdx, %rsi
	movq	%rsi, 56(%rsp)
	jmp	.LBB3_64
	.align	16, 0x90
.LBB3_98:                               # %for.inc339.for.body329_crit_edge
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	56(%rsp), %rsi
.LBB3_64:                               # %for.body329
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_70 Depth 3
                                        #         Child Loop BB3_73 Depth 4
                                        #       Child Loop BB3_95 Depth 3
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%rax, %rsi
	jne	.LBB3_66
# BB#65:                                #   in Loop: Header=BB3_64 Depth=2
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB3_97
	.align	16, 0x90
.LBB3_66:                               # %if.then334
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	$32, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%r14,%r14,4), %r11
	cmpq	$0, regno_save_mem+8(,%r11,8)
	je	.LBB3_67
# BB#68:                                # %if.end.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movl	target_flags(%rip), %r8d
	movq	$23, %rcx
	movq	%r8, %r10
	shrq	%cl, %r10
	andq	$4, %r10
	leaq	4(%r10), %r9
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%r9
	movq	%rax, %rbx
	movq	$32, %rcx
                                        # kill: RAX<def> RBX<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_75
# BB#69:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
.LBB3_70:                               # %for.body.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_64 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB3_73 Depth 4
	movq	$3, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	cmpq	$0, regno_save_mem(%rax,%r11,8)
	je	.LBB3_74
# BB#71:                                # %for.body12.i.preheader
                                        #   in Loop: Header=BB3_70 Depth=3
	movq	$1, %rdi
	.align	16, 0x90
.LBB3_73:                               # %for.body12.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_64 Depth=2
                                        #       Parent Loop BB3_70 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	leaq	-1(%rdi,%r12), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rsi, %rax
	je	.LBB3_74
# BB#72:                                # %for.cond10.i
                                        #   in Loop: Header=BB3_73 Depth=4
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	cmpq	%rdx, %rax
	jl	.LBB3_73
	jmp	.LBB3_76
	.align	16, 0x90
.LBB3_74:                               # %for.inc20.i
                                        #   in Loop: Header=BB3_70 Depth=3
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB3_70
	.align	16, 0x90
.LBB3_75:                               #   in Loop: Header=BB3_64 Depth=2
	xorq	%rbx, %rbx
.LBB3_76:                               # %for.end21.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	%rbx, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$3, %rcx
	movq	%r13, %rax
	movl	64(%rsp,%r14,4), %esi
	shlq	%cl, %rax
	movq	regno_save_mem(%rax,%r11,8), %r15
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rsi, %rsi
	je	.LBB3_93
# BB#77:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movzbl	2(%r15), %ecx
	cmpq	%rcx, %rsi
	je	.LBB3_93
# BB#78:                                # %land.lhs.true32.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	%r12, %rcx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	je	.LBB3_82
# BB#79:                                # %land.lhs.true32.i
                                        #   in Loop: Header=BB3_64 Depth=2
	leaq	-21(%r12), %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_82
# BB#80:                                # %land.lhs.true32.i
                                        #   in Loop: Header=BB3_64 Depth=2
	leaq	-45(%r12), %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jb	.LBB3_82
# BB#81:                                # %land.lhs.true32.i
                                        #   in Loop: Header=BB3_64 Depth=2
	leaq	-29(%r12), %rcx
	andq	%rdi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB3_82
# BB#86:                                # %cond.false.i
                                        #   in Loop: Header=BB3_64 Depth=2
	cmpq	$24, %rsi
	jne	.LBB3_87
# BB#89:                                # %cond.true69.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	$24, %rcx
	shrq	%cl, %r8
	andq	$2, %r8
	xorq	$6, %r8
	jmp	.LBB3_91
.LBB3_82:                               # %cond.true.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r8
	je	.LBB3_91
# BB#83:                                # %lor.rhs.i
                                        #   in Loop: Header=BB3_64 Depth=2
	cmpq	$6, %rax
	movabsq	$0, %r8
	jne	.LBB3_85
# BB#84:                                # %lor.rhs.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movabsq	$1, %r8
.LBB3_85:                               # %lor.rhs.i
                                        #   in Loop: Header=BB3_64 Depth=2
	incq	%r8
	jmp	.LBB3_91
.LBB3_87:                               # %cond.false.i
                                        #   in Loop: Header=BB3_64 Depth=2
	cmpq	$18, %rsi
	jne	.LBB3_90
# BB#88:                                # %cond.true61.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	$25, %rcx
	shrq	%cl, %r8
	andq	$1, %r8
	xorq	$3, %r8
	jmp	.LBB3_91
.LBB3_90:                               # %cond.false73.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movzbl	mode_size(%rax), %eax
	orq	$3, %r10
	addq	%rax, %r10
	movq	%r10, %rax
	cqto
	idivq	%r9
	movq	%rax, %r8
.LBB3_91:                               # %cond.end89.i
                                        #   in Loop: Header=BB3_64 Depth=2
	andq	%rdi, %r8
	cmpq	%r8, %r13
	jne	.LBB3_93
# BB#92:                                # %if.then93.i
                                        #   in Loop: Header=BB3_64 Depth=2
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	callq	adjust_address_1
	movq	%rax, %r15
	.align	16, 0x90
.LBB3_93:                               # %if.end96.i
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movzbl	2(%r15), %edi
	movq	%r12, %rsi
	callq	gen_rtx_REG
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	2(%r15), %ecx
	imulq	$236, %r14, %rdx
	movl	reg_save_code(%rdx,%rcx,4), %edx
	movq	$1, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rcx
	callq	insert_one_insn
	movq	%rax, %r15
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rbx
	movq	%r13, %r14
	je	.LBB3_96
# BB#94:                                # %for.body113.lr.ph.i
                                        #   in Loop: Header=BB3_64 Depth=2
	addq	$64, %r15
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_95:                               # %for.body113.i
                                        #   Parent Loop BB3_2 Depth=1
                                        #     Parent Loop BB3_64 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx,%r12), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	orq	%rax, hard_regs_saved(%rip)
	movq	%r15, %rdi
	movq	%rcx, %rsi
	movq	%r8, %r13
	callq	bitmap_set_bit
	movq	%r13, %r8
	movl	n_regs_saved(%rip), %eax
	incq	%rax
	movl	%eax, n_regs_saved(%rip)
	movq	%rbx, %rax
	andq	%r8, %rax
	incq	%rbx
	cmpq	%r14, %rax
	jne	.LBB3_95
.LBB3_96:                               # %insert_save.exit
                                        #   in Loop: Header=BB3_64 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	-1(%r12,%rax), %r12
.LBB3_97:                               # %for.inc339
                                        #   in Loop: Header=BB3_64 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jle	.LBB3_98
# BB#99:                                # %for.end341
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	hard_regs_saved(%rip), %rax
	xorq	%rdx, %rdx
	movl	%edx, n_regs_saved(%rip)
	xorq	%rcx, %rcx
	movq	8(%rsp), %r12           # 8-byte Reload
	.align	16, 0x90
.LBB3_100:                              # %for.body345
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rax
	je	.LBB3_102
# BB#101:                               # %if.then350
                                        #   in Loop: Header=BB3_100 Depth=2
	incq	%rdx
	movl	%edx, n_regs_saved(%rip)
.LBB3_102:                              # %for.inc353
                                        #   in Loop: Header=BB3_100 Depth=2
	incq	%rcx
	cmpq	$53, %rcx
	jne	.LBB3_100
	.align	16, 0x90
.LBB3_103:                              # %if.end357
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	testq	%rax, %rax
	leaq	64(%rsp), %r13
	jne	.LBB3_104
# BB#111:                               # %if.then366
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%rcx, %r15
	movl	n_regs_saved(%rip), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB3_106
	jmp	.LBB3_112
	.align	16, 0x90
.LBB3_104:                              # %lor.lhs.false361
                                        #   in Loop: Header=BB3_2 Depth=1
	movslq	24(%rax), %rax
	movq	%rcx, %rdx
	movslq	24(%rdx), %rcx
	movq	%rdx, %r15
	cmpq	%rcx, %rax
	jle	.LBB3_112
# BB#105:                               # %lor.lhs.false361
                                        #   in Loop: Header=BB3_2 Depth=1
	movl	n_regs_saved(%rip), %eax
	xorq	%rbx, %rbx
	testq	%r8, %rax
	je	.LBB3_112
	.align	16, 0x90
.LBB3_106:                              # %for.body373
                                        #   Parent Loop BB3_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	hard_regs_saved(%rip), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB3_110
# BB#107:                               # %if.then378
                                        #   in Loop: Header=BB3_106 Depth=2
	movq	%r8, %r14
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$33, %rax
	movabsq	$0, %rsi
	jne	.LBB3_109
# BB#108:                               # %if.then378
                                        #   in Loop: Header=BB3_106 Depth=2
	movabsq	$1, %rsi
.LBB3_109:                              # %if.then378
                                        #   in Loop: Header=BB3_106 Depth=2
	movl	target_flags(%rip), %edi
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	addq	$4, %rdi
	movq	$16, %rax
	xorq	%rdx, %rdx
	idivq	%rdi
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r13, %r8
	callq	insert_restore
	addq	%rax, %rbx
	movq	%r14, %r8
.LBB3_110:                              # %for.inc391
                                        #   in Loop: Header=BB3_106 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB3_106
.LBB3_112:                              # %for.cond.backedge
                                        #   in Loop: Header=BB3_2 Depth=1
	testq	%r12, %r12
	jne	.LBB3_2
.LBB3_113:                              # %for.end397
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_114:                              # %if.else297
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.save_call_clobbered_regs, %rdx
	movq	$433, %rsi              # imm = 0x1B1
	callq	fancy_abort
.LBB3_3:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.save_call_clobbered_regs, %rcx
	movq	$369, %rsi              # imm = 0x171
	jmp	.LBB3_4
.LBB3_67:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insert_save, %rcx
	movq	$724, %rsi              # imm = 0x2D4
.LBB3_4:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp62:
	.size	save_call_clobbered_regs, .Ltmp62-save_call_clobbered_regs
	.cfi_endproc

	.align	16, 0x90
	.type	mark_referenced_regs,@function
mark_referenced_regs:                   # @mark_referenced_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp69:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp70:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp71:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 48
.Ltmp74:
	.cfi_offset %rbx, -48
.Ltmp75:
	.cfi_offset %r12, -40
.Ltmp76:
	.cfi_offset %r13, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movabsq	$4294967292, %r14       # imm = 0xFFFFFFFC
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_39:                               # %if.then163
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rcx), %r12
.LBB4_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	cmpq	$49, %rax
	je	.LBB4_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$47, %rax
	jne	.LBB4_13
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	16(%r12), %rdi
	callq	mark_referenced_regs
.LBB4_4:                                # %if.then3
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%r12), %r12
	movl	(%r12), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$68, %rax
	jg	.LBB4_12
# BB#5:                                 # %if.then3
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$63, %rax
	jne	.LBB4_6
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%r12), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$61, %rsi
	jne	.LBB4_16
# BB#10:                                # %land.lhs.true22
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rdx
	jae	.LBB4_8
# BB#11:                                # %lor.lhs.false37
                                        #   in Loop: Header=BB4_1 Depth=1
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rax
	ja	.LBB4_15
	jmp	.LBB4_8
	.align	16, 0x90
.LBB4_12:                               # %if.then3
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$69, %rax
	je	.LBB4_8
.LBB4_13:                               # %if.end51
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$66, %rax
	je	.LBB4_15
# BB#14:                                # %if.end51
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$63, %rax
	jne	.LBB4_17
.LBB4_15:                               # %if.end51.if.then57_crit_edge
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%r12), %rcx
.LBB4_16:                               # %if.then57
                                        #   in Loop: Header=BB4_1 Depth=1
	movzbl	(%rcx), %eax
	movq	%rcx, %r12
.LBB4_17:                               # %if.end63
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$61, %rax
	jne	.LBB4_41
# BB#18:                                # %if.then66
                                        #   in Loop: Header=BB4_1 Depth=1
	movslq	8(%r12), %rax
	cmpq	$53, %rax
	movq	%rax, %r8
	jl	.LBB4_20
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %r8
.LBB4_20:                               # %cond.end
                                        #   in Loop: Header=BB4_1 Depth=1
	testq	%r8, %r8
	jns	.LBB4_21
# BB#38:                                # %if.else
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	reg_equiv_mem(%rip), %rcx
	movq	(%rcx,%rax,8), %rcx
	testq	%rcx, %rcx
	jne	.LBB4_39
# BB#40:                                # %if.else169
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	reg_equiv_address(%rip), %rcx
	movq	(%rcx,%rax,8), %r12
	testq	%r12, %r12
	jne	.LBB4_1
	jmp	.LBB4_8
.LBB4_6:                                # %if.then3
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$59, %rax
	je	.LBB4_8
# BB#7:                                 # %if.then3
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	$61, %rax
	jne	.LBB4_13
	jmp	.LBB4_8
.LBB4_41:                               # %if.end180
	movzbl	rtx_length(%rax), %r14d
	testq	%r14, %r14
	je	.LBB4_8
# BB#42:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r15
	.align	16, 0x90
.LBB4_43:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_48 Depth 2
	movq	%r14, %r13
	movzbl	-1(%r15,%r13), %eax
	leaq	-1(%r13), %r14
	cmpq	$69, %rax
	jne	.LBB4_44
# BB#47:                                # %if.then205
                                        #   in Loop: Header=BB4_43 Depth=1
	movq	(%r12,%r13,8), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB4_45
	jmp	.LBB4_48
	.align	16, 0x90
.LBB4_49:                               # %for.body213.for.body213_crit_edge
                                        #   in Loop: Header=BB4_48 Depth=2
	movq	(%r12,%r13,8), %rax
.LBB4_48:                               # %for.body213.lr.ph
                                        #   Parent Loop BB4_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rdi
	callq	mark_referenced_regs
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB4_49
	jmp	.LBB4_45
	.align	16, 0x90
.LBB4_44:                               # %for.body
                                        #   in Loop: Header=BB4_43 Depth=1
	cmpq	$101, %rax
	jne	.LBB4_45
# BB#46:                                # %if.then194
                                        #   in Loop: Header=BB4_43 Depth=1
	movq	(%r12,%r13,8), %rdi
	callq	mark_referenced_regs
.LBB4_45:                               # %for.cond.backedge
                                        #   in Loop: Header=BB4_43 Depth=1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB4_43
	jmp	.LBB4_8
.LBB4_21:                               # %if.then77
	movzbl	2(%r12), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r8, %rcx
	cmpq	$8, %rcx
	je	.LBB4_25
# BB#22:                                # %if.then77
	leaq	-21(%r8), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_25
# BB#23:                                # %if.then77
	leaq	-45(%r8), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_25
# BB#24:                                # %if.then77
	leaq	-29(%r8), %rdx
	andq	%rcx, %rdx
	cmpq	$7, %rdx
	jbe	.LBB4_25
# BB#30:                                # %cond.false117
	cmpq	$24, %rax
	jne	.LBB4_31
# BB#33:                                # %cond.true133
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB4_35
.LBB4_25:                               # %cond.true101
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB4_35
# BB#26:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB4_27
# BB#28:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB4_35
.LBB4_31:                               # %cond.false117
	cmpq	$18, %rax
	jne	.LBB4_34
# BB#32:                                # %cond.true123
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB4_35
.LBB4_27:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB4_35
.LBB4_34:                               # %while.cond.preheader
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB4_8
.LBB4_35:                               # %while.body.lr.ph
	movq	referenced_regs(%rip), %rdi
	.align	16, 0x90
.LBB4_36:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	leaq	-1(%rax,%r8), %rdx
	leaq	-1(%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	orq	%rbx, %rdi
	testq	%rsi, %rsi
	jg	.LBB4_36
# BB#37:                                # %while.cond.for.end225.loopexit_crit_edge
	movq	%rdi, referenced_regs(%rip)
.LBB4_8:                                # %for.end225
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	mark_referenced_regs, .Ltmp79-mark_referenced_regs
	.cfi_endproc

	.align	16, 0x90
	.type	insert_restore,@function
insert_restore:                         # @insert_restore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp88:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp89:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp91:
	.cfi_def_cfa_offset 80
.Ltmp92:
	.cfi_offset %rbx, -48
.Ltmp93:
	.cfi_offset %r12, -40
.Ltmp94:
	.cfi_offset %r13, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	(%r15,%r15,4), %rax
	cmpq	$0, regno_save_mem+8(,%rax,8)
	je	.LBB5_31
# BB#1:                                 # %for.cond.preheader
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB5_7
# BB#2:                                 # %for.body.lr.ph
	movq	hard_regs_saved(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
.LBB5_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_5 Depth 2
	movq	$3, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	cmpq	$0, regno_save_mem(%rsi,%rax,8)
	movq	$1, %rsi
	je	.LBB5_6
	.align	16, 0x90
.LBB5_5:                                # %for.body12
                                        #   Parent Loop BB5_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi,%r14), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %rdx
	je	.LBB5_6
# BB#4:                                 # %for.cond10
                                        #   in Loop: Header=BB5_5 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rsi
	cmpq	%rbx, %rdi
	movq	%r13, %rbx
	jl	.LBB5_5
	jmp	.LBB5_8
	.align	16, 0x90
.LBB5_6:                                # %for.inc18
                                        #   in Loop: Header=BB5_3 Depth=1
	decq	%r13
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB5_3
.LBB5_7:
	xorq	%rbx, %rbx
.LBB5_8:                                # %for.end19
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rbx, %rdi
	andq	%r9, %rdi
	movq	$3, %rcx
	movq	%rdi, %rdx
	movl	(%r8,%r15,4), %esi
	shlq	%cl, %rdx
	movq	regno_save_mem(%rdx,%rax,8), %r12
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rsi, %rsi
	je	.LBB5_9
# BB#10:                                # %land.lhs.true
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movzbl	2(%r12), %ecx
	cmpq	%rcx, %rsi
	je	.LBB5_27
# BB#11:                                # %land.lhs.true30
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB5_15
# BB#12:                                # %land.lhs.true30
	leaq	-21(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB5_15
# BB#13:                                # %land.lhs.true30
	leaq	-45(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB5_15
# BB#14:                                # %land.lhs.true30
	leaq	-29(%r14), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB5_15
# BB#20:                                # %cond.false
	cmpq	$24, %rsi
	jne	.LBB5_21
# BB#23:                                # %cond.true66
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB5_25
.LBB5_9:
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	jmp	.LBB5_27
.LBB5_15:                               # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB5_25
# BB#16:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB5_17
# BB#18:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB5_25
.LBB5_21:                               # %cond.false
	cmpq	$18, %rsi
	jne	.LBB5_24
# BB#22:                                # %cond.true58
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB5_25
.LBB5_17:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB5_25
.LBB5_24:                               # %cond.false70
	movl	target_flags(%rip), %r8d
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r8
	andq	$4, %r8
	leaq	3(%rax,%r8), %rax
	addq	$4, %r8
	cqto
	idivq	%r8
.LBB5_25:                               # %cond.end85
	andq	%r9, %rax
	cmpq	%rax, %rdi
	jne	.LBB5_27
# BB#26:                                # %if.then89
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	callq	adjust_address_1
	movq	%rax, %r12
.LBB5_27:                               # %if.end92
	movzbl	2(%r12), %edi
	movq	%r14, %rsi
	callq	gen_rtx_REG
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movzbl	2(%r12), %ecx
	imulq	$236, %r15, %rdx
	movl	reg_restore_code(%rdx,%rcx,4), %edx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rcx
	callq	insert_one_insn
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jle	.LBB5_30
# BB#28:                                # %for.body109.lr.ph
	addq	$64, %rbx
	movq	$1, %r15
	andq	%rdx, %r13
	.align	16, 0x90
.LBB5_29:                               # %for.body109
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%r15,%r14), %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, hard_regs_saved(%rip)
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%r13, %r12
	movq	%r14, %r13
	movq	%rdx, %r14
	callq	bitmap_set_bit
	movq	%r14, %rdx
	movq	%r13, %r14
	movq	%r12, %r13
	movl	n_regs_saved(%rip), %eax
	decq	%rax
	movl	%eax, n_regs_saved(%rip)
	movq	%r15, %rax
	andq	%rdx, %rax
	incq	%r15
	cmpq	%r13, %rax
	jne	.LBB5_29
.LBB5_30:                               # %for.end118
	movq	8(%rsp), %rax           # 8-byte Reload
	decq	%rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_31:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insert_restore, %rcx
	movq	$647, %rsi              # imm = 0x287
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp97:
	.size	insert_restore, .Ltmp97-insert_restore
	.cfi_endproc

	.align	16, 0x90
	.type	mark_set_regs,@function
mark_set_regs:                          # @mark_set_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 16
.Ltmp101:
	.cfi_offset %rbx, -16
	movl	(%rdi), %eax
	movq	$16, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	andq	$255, %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB6_1
# BB#5:                                 # %land.lhs.true
	movl	8(%rdi), %esi
	cmpq	$52, %rsi
	ja	.LBB6_23
	jmp	.LBB6_6
.LBB6_1:                                # %entry
	cmpq	$63, %rax
	jne	.LBB6_23
# BB#2:                                 # %if.then
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB6_23
# BB#3:                                 # %lor.lhs.false
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB6_23
# BB#4:                                 # %if.end
	movq	$1, %rsi
	callq	subreg_hard_regno
	movq	%rax, %rsi
.LBB6_6:                                # %if.end23
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rsi, %rax
	cmpq	$8, %rax
	je	.LBB6_10
# BB#7:                                 # %if.end23
	leaq	-21(%rsi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_10
# BB#8:                                 # %if.end23
	leaq	-45(%rsi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB6_10
# BB#9:                                 # %if.end23
	leaq	-29(%rsi), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB6_10
# BB#15:                                # %cond.false
	cmpq	$24, %rbx
	jne	.LBB6_16
# BB#18:                                # %cond.true49
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB6_20
.LBB6_10:                               # %cond.true
	movl	mode_class(,%rbx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB6_20
# BB#11:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB6_12
# BB#13:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB6_20
.LBB6_16:                               # %cond.false
	cmpq	$18, %rbx
	jne	.LBB6_19
# BB#17:                                # %cond.true45
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB6_20
.LBB6_12:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB6_20
.LBB6_19:                               # %cond.end65
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rbx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB6_23
.LBB6_20:                               # %for.body.lr.ph
	movq	this_insn_sets(%rip), %rdx
	addq	%rsi, %rax
	.align	16, 0x90
.LBB6_21:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rdi
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdi
	jl	.LBB6_21
# BB#22:                                # %for.cond.for.end.loopexit_crit_edge
	movq	%rdx, this_insn_sets(%rip)
.LBB6_23:                               # %for.end
	popq	%rbx
	retq
.Ltmp102:
	.size	mark_set_regs, .Ltmp102-mark_set_regs
	.cfi_endproc

	.align	16, 0x90
	.type	insert_one_insn,@function
insert_one_insn:                        # @insert_one_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp109:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp110:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp111:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp112:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp114:
	.cfi_def_cfa_offset 80
.Ltmp115:
	.cfi_offset %rbx, -48
.Ltmp116:
	.cfi_offset %r12, -40
.Ltmp117:
	.cfi_offset %r13, -32
.Ltmp118:
	.cfi_offset %r14, -24
.Ltmp119:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	32(%r12), %r13
	movq	%rcx, %r15
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	callq	new_insn_chain
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB7_28
# BB#1:                                 # %if.then
	movq	8(%r12), %rax
	testq	%rax, %rax
	movq	%rax, 8(%rbx)
	je	.LBB7_3
# BB#2:                                 # %if.then4
	movq	%rbx, (%rax)
	jmp	.LBB7_4
.LBB7_28:                               # %if.else131
	movq	(%r12), %rax
	testq	%rax, %rax
	movq	%rax, (%rbx)
	je	.LBB7_30
# BB#29:                                # %if.then137
	movq	%rbx, 8(%rax)
.LBB7_30:                               # %if.end140
	movq	%rbx, (%r12)
	movq	%r12, 8(%rbx)
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	emit_insn_after
	movq	%rax, 32(%rbx)
	leaq	40(%rbx), %r14
	leaq	40(%r12), %rsi
	movq	%r14, %rdi
	callq	bitmap_copy
	movq	32(%r12), %rax
	movq	32(%rax), %rdi
	movabsq	$add_stored_regs, %rsi
	movq	%r14, %rdx
	callq	note_stores
	leaq	64(%rbx), %rdi
	callq	bitmap_clear
	movslq	24(%r12), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	32(%r12), %rdx
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB7_32
# BB#31:                                # %if.then161
	movq	32(%rbx), %rdx
	movq	%rdx, 8(%rcx)
	jmp	.LBB7_32
.LBB7_3:                                # %if.else
	movq	%rbx, reload_insn_chain(%rip)
.LBB7_4:                                # %if.end
	movq	%rbx, 8(%r12)
	movq	%r12, (%rbx)
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	movq	%rax, 32(%rbx)
	movq	%r12, 8(%rsp)           # 8-byte Spill
	leaq	40(%rbx), %r14
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	leaq	40(%r12), %rsi
	movq	%r14, %rdi
	callq	bitmap_copy
	movq	32(%r12), %rax
	movq	56(%rax), %rbx
	jmp	.LBB7_5
	.align	16, 0x90
.LBB7_25:                               # %for.inc112
                                        #   in Loop: Header=BB7_5 Depth=1
	movq	16(%rbx), %rbx
.LBB7_5:                                # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_24 Depth 2
	testq	%rbx, %rbx
	je	.LBB7_26
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB7_5 Depth=1
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB7_25
# BB#7:                                 # %if.then14
                                        #   in Loop: Header=BB7_5 Depth=1
	movq	8(%rbx), %rcx
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB7_33
# BB#8:                                 # %if.end22
                                        #   in Loop: Header=BB7_5 Depth=1
	movslq	8(%rcx), %r13
	cmpq	$53, %r13
	jl	.LBB7_10
# BB#9:                                 # %if.then26
                                        #   in Loop: Header=BB7_5 Depth=1
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%r13,2), %r13
.LBB7_10:                               # %if.end28
                                        #   in Loop: Header=BB7_5 Depth=1
	testq	%r13, %r13
	js	.LBB7_25
# BB#11:                                # %if.end32
                                        #   in Loop: Header=BB7_5 Depth=1
	movq	%r13, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB7_15
# BB#12:                                # %if.end32
                                        #   in Loop: Header=BB7_5 Depth=1
	leaq	-21(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB7_15
# BB#13:                                # %if.end32
                                        #   in Loop: Header=BB7_5 Depth=1
	leaq	-45(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB7_15
# BB#14:                                # %if.end32
                                        #   in Loop: Header=BB7_5 Depth=1
	leaq	-29(%r13), %rcx
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	jbe	.LBB7_15
# BB#19:                                # %cond.false
                                        #   in Loop: Header=BB7_5 Depth=1
	cmpq	$24, %rax
	jne	.LBB7_20
# BB#22:                                # %cond.true82
                                        #   in Loop: Header=BB7_5 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB7_24
.LBB7_15:                               # %cond.true
                                        #   in Loop: Header=BB7_5 Depth=1
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB7_24
# BB#16:                                # %lor.rhs
                                        #   in Loop: Header=BB7_5 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB7_18
# BB#17:                                # %lor.rhs
                                        #   in Loop: Header=BB7_5 Depth=1
	movabsq	$1, %rax
.LBB7_18:                               # %lor.rhs
                                        #   in Loop: Header=BB7_5 Depth=1
	incq	%rax
	jmp	.LBB7_24
.LBB7_20:                               # %cond.false
                                        #   in Loop: Header=BB7_5 Depth=1
	cmpq	$18, %rax
	jne	.LBB7_23
# BB#21:                                # %cond.true73
                                        #   in Loop: Header=BB7_5 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB7_24
.LBB7_23:                               # %for.cond105.preheader
                                        #   in Loop: Header=BB7_5 Depth=1
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
	jle	.LBB7_25
	.align	16, 0x90
.LBB7_24:                               # %for.body108
                                        #   Parent Loop BB7_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rax), %r12
	movq	$32, %rcx
	movq	%r12, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	-1(%rax,%r13), %rsi
	movq	%r14, %rdi
	callq	bitmap_set_bit
	testq	%r15, %r15
	movq	%r12, %rax
	jg	.LBB7_24
	jmp	.LBB7_25
.LBB7_26:                               # %for.end116
	movq	16(%rsp), %rbx          # 8-byte Reload
	leaq	64(%rbx), %rdi
	callq	bitmap_clear
	movq	8(%rsp), %rdx           # 8-byte Reload
	movslq	24(%rdx), %rax
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	32(%rdx), %rdx
	movq	(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB7_32
# BB#27:                                # %if.then122
	movq	32(%rbx), %rdx
	movq	%rdx, (%rcx)
.LBB7_32:                               # %if.end170
	movl	%eax, 24(%rbx)
	movzbl	262(%rbx), %eax
	orq	$8, %rax
	movb	%al, 262(%rbx)
	movq	32(%rbx), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 40(%rax)
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_33:                               # %if.then21
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.insert_one_insn, %rcx
	movq	$824, %rsi              # imm = 0x338
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp120:
	.size	insert_one_insn, .Ltmp120-insert_one_insn
	.cfi_endproc

	.align	16, 0x90
	.type	add_stored_regs,@function
add_stored_regs:                        # @add_stored_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp125:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp126:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp127:
	.cfi_def_cfa_offset 32
.Ltmp128:
	.cfi_offset %rbx, -32
.Ltmp129:
	.cfi_offset %r14, -24
.Ltmp130:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	movzbl	(%rsi), %eax
	movq	%rdx, %r14
	movq	$16, %rcx
	movq	%rdi, %r15
	shrq	%cl, %r15
	cmpq	$49, %rax
	je	.LBB8_22
# BB#1:                                 # %if.end
	andq	$255, %r15
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$63, %rcx
	jne	.LBB8_4
# BB#2:                                 # %land.lhs.true
	movq	8(%rbx), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB8_4
# BB#3:                                 # %if.then9
	movl	16(%rbx), %edx
	movl	8(%rcx), %edi
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	movl	(%rbx), %edi
.LBB8_4:                                # %if.end30
	andq	$65535, %rdi            # imm = 0xFFFF
	cmpq	$61, %rdi
	jne	.LBB8_22
# BB#5:                                 # %lor.lhs.false
	movl	8(%rbx), %ecx
	cmpq	$52, %rcx
	ja	.LBB8_22
# BB#6:                                 # %if.end39
	leaq	(%rcx,%rax), %rbx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rbx, %rdx
	cmpq	$8, %rdx
	je	.LBB8_10
# BB#7:                                 # %if.end39
	leaq	-21(%rcx,%rax), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$8, %rsi
	jb	.LBB8_10
# BB#8:                                 # %if.end39
	leaq	-45(%rcx,%rax), %rsi
	andq	%rdx, %rsi
	cmpq	$8, %rsi
	jb	.LBB8_10
# BB#9:                                 # %if.end39
	leaq	-29(%rcx,%rax), %rax
	andq	%rdx, %rax
	cmpq	$7, %rax
	jbe	.LBB8_10
# BB#15:                                # %cond.false
	cmpq	$24, %r15
	jne	.LBB8_16
# BB#18:                                # %cond.true68
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB8_20
.LBB8_10:                               # %cond.true
	movl	mode_class(,%r15,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB8_20
# BB#11:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB8_12
# BB#13:                                # %lor.rhs
	movabsq	$1, %r15
	incq	%r15
	jmp	.LBB8_20
.LBB8_16:                               # %cond.false
	cmpq	$18, %r15
	jne	.LBB8_19
# BB#17:                                # %cond.true64
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB8_20
.LBB8_12:
	movabsq	$0, %r15
	incq	%r15
	jmp	.LBB8_20
.LBB8_19:                               # %cond.end85
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r15), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB8_22
.LBB8_20:                               # %for.body.lr.ph
	addq	%rbx, %r15
	.align	16, 0x90
.LBB8_21:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jl	.LBB8_21
.LBB8_22:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp131:
	.size	add_stored_regs, .Ltmp131-add_stored_regs
	.cfi_endproc

	.type	regno_save_mode,@object # @regno_save_mode
	.local	regno_save_mode
	.comm	regno_save_mode,1060,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"caller-save.c"
	.size	.L.str, 14

	.type	.L__FUNCTION__.init_caller_save,@object # @__FUNCTION__.init_caller_save
.L__FUNCTION__.init_caller_save:
	.asciz	"init_caller_save"
	.size	.L__FUNCTION__.init_caller_save, 17

	.type	reg_save_code,@object   # @reg_save_code
	.local	reg_save_code
	.comm	reg_save_code,12508,16
	.type	reg_restore_code,@object # @reg_restore_code
	.local	reg_restore_code
	.comm	reg_restore_code,12508,16
	.type	regno_save_mem,@object  # @regno_save_mem
	.local	regno_save_mem
	.comm	regno_save_mem,2120,16
	.type	hard_regs_saved,@object # @hard_regs_saved
	.local	hard_regs_saved
	.comm	hard_regs_saved,8,8
	.type	n_regs_saved,@object    # @n_regs_saved
	.local	n_regs_saved
	.comm	n_regs_saved,4,4
	.type	.L__FUNCTION__.save_call_clobbered_regs,@object # @__FUNCTION__.save_call_clobbered_regs
.L__FUNCTION__.save_call_clobbered_regs:
	.asciz	"save_call_clobbered_regs"
	.size	.L__FUNCTION__.save_call_clobbered_regs, 25

	.type	referenced_regs,@object # @referenced_regs
	.local	referenced_regs
	.comm	referenced_regs,8,8
	.type	this_insn_sets,@object  # @this_insn_sets
	.local	this_insn_sets
	.comm	this_insn_sets,8,8
	.type	.L__FUNCTION__.insert_save,@object # @__FUNCTION__.insert_save
.L__FUNCTION__.insert_save:
	.asciz	"insert_save"
	.size	.L__FUNCTION__.insert_save, 12

	.type	.L__FUNCTION__.insert_one_insn,@object # @__FUNCTION__.insert_one_insn
.L__FUNCTION__.insert_one_insn:
	.asciz	"insert_one_insn"
	.size	.L__FUNCTION__.insert_one_insn, 16

	.type	.L__FUNCTION__.insert_restore,@object # @__FUNCTION__.insert_restore
.L__FUNCTION__.insert_restore:
	.asciz	"insert_restore"
	.size	.L__FUNCTION__.insert_restore, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
