	.file	"optabs.c.bc"
	.text
	.globl	expand_simple_binop
	.align	16, 0x90
	.type	expand_simple_binop,@function
expand_simple_binop:                    # @expand_simple_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	code_to_optab(,%rsi,8), %rsi
	testq	%rsi, %rsi
	je	.LBB0_1
# BB#2:                                 # %if.end
	movq	%rax, %rcx
	popq	%rax
	jmp	expand_binop  # TAILCALL
.LBB0_1:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_simple_binop, %rcx
	movq	$624, %rsi              # imm = 0x270
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp2:
	.size	expand_simple_binop, .Ltmp2-expand_simple_binop
	.cfi_endproc

	.globl	expand_binop
	.align	16, 0x90
	.type	expand_binop,@function
expand_binop:                           # @expand_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp9:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp10:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp11:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp12:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 48
	subq	$256, %rsp              # imm = 0x100
.Ltmp14:
	.cfi_def_cfa_offset 304
.Ltmp15:
	.cfi_offset %rbx, -48
.Ltmp16:
	.cfi_offset %r12, -40
.Ltmp17:
	.cfi_offset %r13, -32
.Ltmp18:
	.cfi_offset %r14, -24
.Ltmp19:
	.cfi_offset %r15, -16
	movq	%r9, 160(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rdi, 232(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB1_1
	.align	16, 0x90
.LBB1_94:                               # %if.then336
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	xorq	%r15, %r15
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
.LBB1_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %eax
	movq	%rax, 216(%rsp)         # 8-byte Spill
	movq	%rsi, %r12
	callq	get_last_insn
	movq	%rax, 144(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r13
	testq	%r15, %r15
	movabsq	$0, %rax
	je	.LBB1_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
.LBB1_3:                                # %if.end
                                        #   in Loop: Header=BB1_1 Depth=1
	movl	flag_force_mem(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB1_5
# BB#4:                                 #   in Loop: Header=BB1_1 Depth=1
	movq	%rax, 224(%rsp)         # 8-byte Spill
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_5:                                # %if.then18
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	force_not_mem
	movq	%rax, %r13
.LBB1_6:                                # %if.end21
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	216(%rsp), %r15         # 8-byte Reload
	movq	optab_table+16(%rip), %rax
	cmpq	%r12, %rax
	jne	.LBB1_9
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%r13), %eax
	cmpq	$54, %rax
	jne	.LBB1_9
# BB#8:                                 # %if.then24
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	negate_rtx
	movq	optab_table(%rip), %r12
	movq	%rax, %r13
.LBB1_9:                                # %if.end26
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rbx), %ecx
	addq	$-87, %r15
	cmpq	$133, %rcx
	jg	.LBB1_14
# BB#10:                                # %if.end26
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, 240(%rsp)         # 8-byte Spill
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	jbe	.LBB1_12
# BB#11:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, 248(%rsp)         # 8-byte Spill
	movq	%r14, %r12
	jmp	.LBB1_20
	.align	16, 0x90
.LBB1_14:                               # %if.end26
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, 240(%rsp)         # 8-byte Spill
	movq	%r12, 248(%rsp)         # 8-byte Spill
	cmpq	$134, %rcx
	je	.LBB1_17
# BB#15:                                # %if.end26
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$140, %rcx
	je	.LBB1_17
# BB#16:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %r12
	jmp	.LBB1_20
	.align	16, 0x90
.LBB1_12:                               # %if.end26
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, 248(%rsp)         # 8-byte Spill
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	je	.LBB1_13
.LBB1_17:                               # %land.lhs.true58
                                        #   in Loop: Header=BB1_1 Depth=1
	callq	preserve_subexpressions_p
	movq	%r14, %r12
	testq	%r12, %rax
	je	.LBB1_20
# BB#18:                                # %land.lhs.true61
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB1_20
# BB#19:                                # %if.then65
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 240(%rsp)         # 8-byte Spill
	jmp	.LBB1_20
.LBB1_13:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %r12
	.align	16, 0x90
.LBB1_20:                               # %if.end67
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%r13), %ecx
	cmpq	$133, %rcx
	jg	.LBB1_25
# BB#21:                                # %if.end67
                                        #   in Loop: Header=BB1_1 Depth=1
	addq	$-54, %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	jbe	.LBB1_23
# BB#22:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	jmp	.LBB1_35
	.align	16, 0x90
.LBB1_25:                               # %if.end67
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB1_28
# BB#26:                                # %if.end67
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$140, %rcx
	je	.LBB1_28
# BB#27:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	jmp	.LBB1_35
	.align	16, 0x90
.LBB1_23:                               # %if.end67
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	je	.LBB1_24
.LBB1_28:                               # %land.lhs.true99
                                        #   in Loop: Header=BB1_1 Depth=1
	callq	preserve_subexpressions_p
	testq	%r12, %rax
	je	.LBB1_29
# BB#30:                                # %land.lhs.true99
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, %rax
	andq	%r12, %rax
	cmpq	$5, %rax
	jb	.LBB1_31
# BB#32:                                # %land.lhs.true104
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %esi
	movq	%r13, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB1_33
# BB#34:                                # %if.then108
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, %r13
	jmp	.LBB1_35
.LBB1_29:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	jmp	.LBB1_35
.LBB1_31:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	jmp	.LBB1_35
.LBB1_33:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	jmp	.LBB1_35
.LBB1_24:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r15, 216(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB1_35:                               # %if.end110
                                        #   in Loop: Header=BB1_1 Depth=1
	callq	get_last_insn
	movq	248(%rsp), %r9          # 8-byte Reload
	movslq	(%r9), %rcx
	movzbl	rtx_class(%rcx), %ecx
	movq	%rax, 184(%rsp)         # 8-byte Spill
	cmpq	$99, %rcx
	je	.LBB1_41
# BB#36:                                # %if.end110
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	optab_table+64(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_41
# BB#37:                                # %if.end110
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	optab_table+72(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_41
# BB#38:                                # %if.end110
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	optab_table+48(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_41
# BB#39:                                # %if.end110
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	optab_table+56(%rip), %rax
	xorq	%r15, %r15
	cmpq	%rax, %r9
	je	.LBB1_41
# BB#40:                                #   in Loop: Header=BB1_1 Depth=1
	movq	240(%rsp), %rbx         # 8-byte Reload
	movq	224(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB1_50
	.align	16, 0x90
.LBB1_41:                               # %if.then129
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	224(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB1_43
# BB#42:                                # %lor.lhs.false132
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB1_47
.LBB1_43:                               # %cond.true137
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	jne	.LBB1_45
# BB#44:                                # %land.lhs.true142
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	240(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB1_46
.LBB1_45:                               # %lor.lhs.false147
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%r13, %rsi
	jne	.LBB1_48
	jmp	.LBB1_46
.LBB1_47:                               # %cond.false150
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rdi
	movq	%r9, %r14
	movq	%rsi, %rbx
	callq	rtx_equal_p
	movq	%rbx, %rsi
	movq	%r14, %r9
	testq	%r12, %rax
	jne	.LBB1_46
.LBB1_48:                               # %lor.lhs.false153
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	240(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	movq	$1, %r15
	jne	.LBB1_49
.LBB1_46:                               # %if.then158
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rbx
	movq	$1, %r15
	movq	240(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_50
.LBB1_49:                               #   in Loop: Header=BB1_1 Depth=1
	movq	240(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB1_50:                               # %if.end160
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	304(%rsp), %rax
	andq	%r12, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	cmpq	$4, %rax
	movq	168(%rsp), %r14         # 8-byte Reload
	je	.LBB1_100
# BB#51:                                # %land.lhs.true163
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movslq	8(%r9,%rax), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB1_100
# BB#52:                                # %if.then168
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r9, 248(%rsp)          # 8-byte Spill
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rax
	movq	%rax, 240(%rsp)         # 8-byte Spill
	movzbl	40(%rax), %r12d
	movzbl	64(%rax), %r14d
	testq	%rsi, %rsi
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	movq	%rsi, 224(%rsp)         # 8-byte Spill
	jne	.LBB1_54
# BB#53:                                # %if.else
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, 200(%rsp)         # 8-byte Spill
.LBB1_54:                               # %if.end188
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%r15, %r15
	jne	.LBB1_56
# BB#55:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rdx
	jmp	.LBB1_69
	.align	16, 0x90
.LBB1_56:                               # %if.then190
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	2(%rbx), %edx
	cmpq	%r12, %rdx
	jne	.LBB1_58
# BB#57:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rdx
	jmp	.LBB1_69
	.align	16, 0x90
.LBB1_58:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	2(%r13), %ecx
	cmpq	%r14, %rcx
	movabsq	$0, %rax
	je	.LBB1_60
# BB#59:                                # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movabsq	$1, %rax
.LBB1_60:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%r14, %rdx
	movabsq	$0, %rdx
	jne	.LBB1_62
# BB#61:                                # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movabsq	$1, %rdx
.LBB1_62:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	andq	%rdx, %rax
	cmpq	%r12, %rcx
	movabsq	$0, %rcx
	jne	.LBB1_64
# BB#63:                                # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movabsq	$1, %rcx
.LBB1_64:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	andq	%rcx, %rax
	movq	%rbx, %rdx
	jne	.LBB1_66
# BB#65:                                # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rdx
.LBB1_66:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%rax, %rax
	jne	.LBB1_68
# BB#67:                                # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, %r13
.LBB1_68:                               # %land.lhs.true195
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rbx
.LBB1_69:                               # %if.end215
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	2(%rbx), %esi
	cmpq	%r12, %rsi
	movq	%rbx, %r15
	je	.LBB1_75
# BB#70:                                # %if.end215
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%r12, %r12
	movq	%rbx, %r15
	je	.LBB1_75
# BB#71:                                # %if.then224
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rdx, %r13
	testq	%rsi, %rsi
	jne	.LBB1_74
# BB#72:                                # %cond.false234
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size(%r12), %ecx
	cmpq	%rcx, %rax
	movq	232(%rsp), %rsi         # 8-byte Reload
	ja	.LBB1_74
# BB#73:                                # %select.mid
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rsi
.LBB1_74:                               # %cond.end247
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	160(%rsp), %rcx         # 8-byte Reload
	callq	convert_modes
	movq	%rax, %r15
	movq	%r13, %rdx
.LBB1_75:                               # %if.end250
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movzbl	2(%rdx), %esi
	cmpq	%r14, %rsi
	movq	%rdx, %r13
	je	.LBB1_83
# BB#76:                                # %if.end250
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%r14, %r14
	movq	%rdx, %r13
	je	.LBB1_83
# BB#77:                                # %if.then259
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%rsi, %rsi
	jne	.LBB1_82
# BB#78:                                # %cond.false269
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	216(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%r14), %esi
	cmpq	$5, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	cmpq	%rsi, %rcx
	movabsq	$0, %rcx
	ja	.LBB1_80
# BB#79:                                # %cond.false269
                                        #   in Loop: Header=BB1_1 Depth=1
	movabsq	$1, %rcx
.LBB1_80:                               # %cond.false269
                                        #   in Loop: Header=BB1_1 Depth=1
	orq	%rax, %rcx
	movq	%r14, %rsi
	jne	.LBB1_82
# BB#81:                                # %cond.false269
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	232(%rsp), %rsi         # 8-byte Reload
.LBB1_82:                               # %cond.end284
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %rdi
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	%rdx, %rbx
	callq	convert_modes
	movq	%rbx, %rdx
	movq	%rax, %r13
.LBB1_83:                               # %if.end287
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rdx, 208(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	240(%rsp), %rax         # 8-byte Reload
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_86
# BB#84:                                # %if.end287
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%r12, %r12
	je	.LBB1_86
# BB#85:                                # %if.then297
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
.LBB1_86:                               # %if.end299
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	200(%rsp), %r12         # 8-byte Reload
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	240(%rsp), %rax         # 8-byte Reload
	callq	*48(%rax)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	232(%rsp), %rcx         # 8-byte Reload
	jne	.LBB1_89
# BB#87:                                # %if.end299
                                        #   in Loop: Header=BB1_1 Depth=1
	testq	%r14, %r14
	je	.LBB1_89
# BB#88:                                # %if.then310
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rcx, %r14
	callq	copy_to_mode_reg
	movq	%r14, %rcx
	movq	%rax, %r13
.LBB1_89:                               # %if.end312
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rdi
	movq	%rcx, %rsi
	movq	240(%rsp), %rax         # 8-byte Reload
	callq	*(%rax)
	testq	%rbx, %rax
	jne	.LBB1_91
# BB#90:                                # %if.then320
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB1_91:                               # %if.end322
                                        #   in Loop: Header=BB1_1 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	176(%rsp), %rcx         # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB1_97
# BB#92:                                # %if.then327
                                        #   in Loop: Header=BB1_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jne	.LBB1_95
# BB#93:                                # %land.lhs.true332
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rcx
	movq	%r13, %r8
	callq	add_equal_note
	testq	%r14, %rax
	je	.LBB1_94
.LBB1_95:                               # %if.end338
	movq	%rbx, %rdi
	callq	emit_insn
	jmp	.LBB1_96
.LBB1_100:                              # %if.end342.loopexit
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movq	304(%rsp), %r15
	andq	%r15, %rax
	cmpq	$1, %rax
	movq	$2, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_102
# BB#101:                               # %if.end342.loopexit
	movq	%r15, 176(%rsp)         # 8-byte Spill
.LBB1_102:                              # %if.end342.loopexit
	movl	mode_class(,%r14,4), %eax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%r13, 208(%rsp)         # 8-byte Spill
	movq	160(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_103
.LBB1_97:                               # %if.else340
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movq	304(%rsp), %r15
	andq	%r15, %rax
	cmpq	$1, %rax
	movq	$2, 176(%rsp)           # 8-byte Folded Spill
	movq	192(%rsp), %rbx         # 8-byte Reload
	je	.LBB1_99
# BB#98:                                # %if.else340
	movq	%r15, 176(%rsp)         # 8-byte Spill
.LBB1_99:                               # %if.else340
	movq	168(%rsp), %r14         # 8-byte Reload
	movl	mode_class(,%r14,4), %eax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	160(%rsp), %r13         # 8-byte Reload
	movq	224(%rsp), %rsi         # 8-byte Reload
.LBB1_103:                              # %if.end342
	movq	%rsi, 224(%rsp)         # 8-byte Spill
	movq	optab_table+32(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_113
# BB#104:                               # %land.lhs.true345
	movzbl	mode_wider_mode(%r14), %edi
	testq	%rdi, %rdi
	je	.LBB1_113
# BB#105:                               # %land.lhs.true351
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB1_106
# BB#107:                               # %land.lhs.true351
	movabsq	$optab_table+64, %rax
	jmp	.LBB1_108
.LBB1_106:
	movabsq	$optab_table+72, %rax
.LBB1_108:                              # %land.lhs.true351
	movq	(%rax), %rsi
	movq	$4, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movl	8(%rsi,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB1_113
# BB#109:                               # %if.then366
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%rbx, %rdx
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%r13, %r12
	movq	%r15, %r13
	movq	%r14, %r15
	movq	%r9, %r14
	movq	%r12, %r9
	callq	expand_binop
	movq	%r14, %r9
	movq	%r15, %r14
	movq	%r13, %r15
	movq	%r12, %r13
	testq	%rax, %rax
	je	.LBB1_113
# BB#110:                               # %if.then378
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	136(%rsp), %rdx         # 8-byte Reload
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	jne	.LBB1_112
# BB#111:                               # %if.then383
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
.LBB1_166:                              # %if.else486
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB1_113:                              # %if.end388
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	136(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r8
	movq	%r8, 240(%rsp)          # 8-byte Spill
	cmpq	$6, %r8
	ja	.LBB1_125
# BB#114:                               # %if.end388
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	$70, %rax
	je	.LBB1_125
# BB#115:                               # %switch.early.test
	cmpq	$2, 152(%rsp)           # 8-byte Folded Reload
	jb	.LBB1_125
# BB#116:                               # %for.cond.preheader
	movzbl	mode_wider_mode(%r14), %ebx
	jmp	.LBB1_117
.LBB1_167:                              # %if.else488
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	304(%rsp), %r15
	movq	240(%rsp), %r8          # 8-byte Reload
	movzbl	mode_wider_mode(%rbx), %ebx
.LBB1_117:                              # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_125
# BB#118:                               # %for.body
                                        #   in Loop: Header=BB1_117 Depth=1
	andq	$255, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	8(%r9,%rax), %ecx
	movq	optab_table+32(%rip), %rax
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB1_149
# BB#119:                               # %lor.lhs.false415
                                        #   in Loop: Header=BB1_117 Depth=1
	cmpq	%rax, %r9
	jne	.LBB1_124
# BB#120:                               # %land.lhs.true418
                                        #   in Loop: Header=BB1_117 Depth=1
	movzbl	mode_wider_mode(%rbx), %eax
	testq	%rax, %rax
	je	.LBB1_124
# BB#121:                               # %land.lhs.true424
                                        #   in Loop: Header=BB1_117 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r13
	movabsq	$optab_table+72, %rcx
	jne	.LBB1_123
# BB#122:                               # %land.lhs.true424
                                        #   in Loop: Header=BB1_117 Depth=1
	movabsq	$optab_table+64, %rcx
.LBB1_123:                              # %land.lhs.true424
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	(%rcx), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	%r9, %rax
	jne	.LBB1_149
.LBB1_124:                              # %for.cond.backedge
                                        #   in Loop: Header=BB1_117 Depth=1
	movzbl	mode_wider_mode(%rbx), %ebx
	jmp	.LBB1_117
.LBB1_149:                              # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table+152(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB1_156
# BB#150:                               # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table+144(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB1_156
# BB#151:                               # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table+160(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB1_156
# BB#152:                               # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB1_156
# BB#153:                               # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table+16(%rip), %rcx
	cmpq	%rcx, %r9
	je	.LBB1_156
# BB#154:                               # %if.then439
                                        #   in Loop: Header=BB1_117 Depth=1
	cmpq	%rax, %r9
	je	.LBB1_156
# BB#155:                               # %lor.lhs.false462
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	optab_table+168(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_157
.LBB1_156:                              # %land.lhs.true465
                                        #   in Loop: Header=BB1_117 Depth=1
	cmpq	$1, %r8
	jne	.LBB1_157
# BB#158:                               # %land.rhs
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	$1, %r8
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	232(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %r14
	movq	%r13, %rcx
	movq	%r9, %r12
	movq	%r12, 248(%rsp)         # 8-byte Spill
	callq	widen_operand
	movq	optab_table+168(%rip), %rcx
	movq	%rax, %r15
	cmpq	%rcx, %r12
	movabsq	$0, %r12
	je	.LBB1_160
# BB#159:                               # %land.rhs
                                        #   in Loop: Header=BB1_117 Depth=1
	movabsq	$1, %r12
	jmp	.LBB1_160
.LBB1_157:                              # %if.end469.thread
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	%r9, 248(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	232(%rsp), %r14         # 8-byte Reload
	xorq	%r12, %r12
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	widen_operand
	movq	%rax, %r15
.LBB1_160:                              # %land.rhs
                                        #   in Loop: Header=BB1_117 Depth=1
	movq	208(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	movq	%r12, %r8
	callq	widen_operand
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r13, %r9
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB1_167
# BB#161:                               # %if.then477
	cmpq	$1, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_162
# BB#165:                               # %if.else486
	movq	%r14, %rdi
	movq	%r15, %rsi
	jmp	.LBB1_166
.LBB1_125:                              # %if.end494
	movq	optab_table+144(%rip), %rax
	movabsq	$4294967293, %rbx       # imm = 0xFFFFFFFD
	andq	%r15, %rbx
	cmpq	%rax, %r9
	je	.LBB1_168
# BB#126:                               # %if.end494
	movq	optab_table+152(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_168
# BB#127:                               # %lor.lhs.false500
	movq	optab_table+160(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_128
.LBB1_168:                              # %lor.lhs.false500
	cmpq	$1, %r8
	movq	192(%rsp), %r13         # 8-byte Reload
	jne	.LBB1_169
# BB#170:                               # %land.lhs.true506
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%r14), %edx
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jbe	.LBB1_169
# BB#171:                               # %land.lhs.true514
	movslq	word_mode(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%r9,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_172
.LBB1_169:
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 200(%rsp)         # 8-byte Spill
.LBB1_129:                              # %if.end586
	movq	optab_table+176(%rip), %rax
	movq	optab_table+168(%rip), %rdx
	cmpq	%rax, %r9
	je	.LBB1_132
# BB#130:                               # %if.end586
	cmpq	%rdx, %r9
	je	.LBB1_132
# BB#131:                               # %lor.lhs.false592
	movq	optab_table+184(%rip), %rcx
	cmpq	%rcx, %r9
	jne	.LBB1_220
.LBB1_132:                              # %land.lhs.true595
	cmpq	$1, %r8
	jne	.LBB1_220
# BB#133:                               # %land.lhs.true598
	movq	208(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB1_220
# BB#134:                               # %land.lhs.true603
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r14), %edi
	movq	$22, %rcx
	shrq	%cl, %rsi
	andq	$8, %rsi
	addq	$8, %rsi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jne	.LBB1_220
# BB#135:                               # %land.lhs.true612
	movslq	word_mode(%rip), %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%r9,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB1_220
# BB#136:                               # %land.lhs.true619
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rdx,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB1_220
# BB#137:                               # %land.lhs.true626
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	8(%rax,%rsi), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB1_220
# BB#138:                               # %if.then633
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	200(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB1_141
# BB#139:                               # %if.then633
	cmpq	%r13, %rcx
	je	.LBB1_141
# BB#140:                               # %if.then633
	movq	208(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB1_142
.LBB1_141:                              # %if.then654
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rcx
.LBB1_142:                              # %if.end656
	movq	%rcx, %r15
	movq	232(%rsp), %r13         # 8-byte Reload
	callq	start_sequence
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %r12
	movq	%r12, 224(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	optab_table+168(%rip), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%rbx, %rcx
	cmpq	%rax, %rcx
	movabsq	$0, %r14
	movabsq	$1, %rax
	movq	%r14, %rbx
	jne	.LBB1_144
# BB#143:                               # %if.end656
	movq	%rax, %rbx
.LBB1_144:                              # %if.end656
	movq	%rcx, 248(%rsp)         # 8-byte Spill
	je	.LBB1_146
# BB#145:                               # %if.end656
	movq	%rax, %r14
.LBB1_146:                              # %if.end656
	movq	$1, %rdx
	movq	%r15, 200(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r13, %rcx
	callq	operand_subword
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rcx
	callq	operand_subword
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	%rbx, %r15
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	operand_subword_force
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	operand_subword_force
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	cmpq	%rsi, %r12
	jge	.LBB1_147
# BB#200:                               # %if.else716
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	optab_table+176(%rip), %rbx
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	optab_table+168(%rip), %r12
	movq	112(%rsp), %r15         # 8-byte Reload
	cmpq	%r15, 248(%rsp)         # 8-byte Folded Reload
	je	.LBB1_202
# BB#201:                               # %if.else716
	movq	%r12, %rbx
.LBB1_202:                              # %if.else716
	movq	224(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	xorq	%r13, %r13
	callq	gen_rtx_CONST_INT
	xorq	%r8, %r8
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movl	word_mode(%rip), %edi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%r14, %rcx
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_218
# BB#203:                               # %if.end742
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	cmpq	%r15, 248(%rsp)         # 8-byte Folded Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%r12, %rsi
	je	.LBB1_205
# BB#204:                               # %if.end742
	movq	96(%rsp), %rsi          # 8-byte Reload
.LBB1_205:                              # %if.end742
	movl	word_mode(%rip), %edi
	movq	176(%rsp), %r15         # 8-byte Reload
	movq	%r15, (%rsp)
	xorq	%r8, %r8
	xorq	%r13, %r13
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_218
# BB#206:                               # %if.end747
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	movq	%r15, (%rsp)
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB1_207
# BB#208:                               # %if.end747
	movabsq	$1, %rax
	jmp	.LBB1_209
.LBB1_172:                              # %if.then521
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movq	224(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB1_175
# BB#173:                               # %if.then521
	cmpq	%r13, %rcx
	je	.LBB1_175
# BB#174:                               # %if.then521
	movq	208(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB1_176
.LBB1_175:                              # %if.then533
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rcx
.LBB1_176:                              # %if.end535
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	movq	%r13, 192(%rsp)         # 8-byte Spill
	callq	start_sequence
	movzbl	mode_bitsize(%r14,%r14), %eax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	xorq	%r13, %r13
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_177
# BB#178:
	movq	232(%rsp), %rcx         # 8-byte Reload
	.align	16, 0x90
.LBB1_179:                              # %for.body545
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rcx, %rbx
	callq	operand_subword
	movl	word_mode(%rip), %r12d
	movq	%rax, %r14
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, %r15
	movq	208(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r14, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_180
# BB#181:                               # %if.end555
                                        #   in Loop: Header=BB1_179 Depth=1
	cmpq	%rax, %r14
	je	.LBB1_183
# BB#182:                               # %if.then558
                                        #   in Loop: Header=BB1_179 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_183:                              # %for.inc561
                                        #   in Loop: Header=BB1_179 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	xorq	%rdx, %rdx
	movq	200(%rsp), %rax         # 8-byte Reload
	idivq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	movq	%rbx, %rcx
	jl	.LBB1_179
	jmp	.LBB1_184
.LBB1_128:
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	192(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_129
.LBB1_177:
	movq	232(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB1_184
.LBB1_180:
	movq	%rbx, %rcx
.LBB1_184:                              # %for.end562
	movq	%rcx, %r15
	callq	get_insns
	movq	%rax, %r12
	callq	end_sequence
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	xorq	%r8, %r8
	xorq	%rdx, %rdx
	movq	200(%rsp), %rax         # 8-byte Reload
	idivq	%rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r13
	cmpq	%rax, %r13
	jne	.LBB1_185
# BB#186:                               # %if.then573
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	movq	192(%rsp), %r14         # 8-byte Reload
	movq	208(%rsp), %r13         # 8-byte Reload
	je	.LBB1_188
# BB#187:                               # %if.then577
	movq	%rax, 248(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	copy_rtx
	movq	248(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r8
.LBB1_188:                              # %if.end583
	movq	%r12, %rdi
	movq	224(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	emit_no_conflict_block
	movq	%rbx, %r12
	jmp	.LBB1_96
.LBB1_185:
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	%r15, 232(%rsp)         # 8-byte Spill
	movq	192(%rsp), %r13         # 8-byte Reload
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB1_129
.LBB1_112:                              # %if.else385
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%r13, %rdx
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert_to_mode  # TAILCALL
.LBB1_147:                              # %if.then671
	movq	224(%rsp), %rax         # 8-byte Reload
	subq	%rsi, %rax
	movq	$32, %rcx
	movl	word_mode(%rip), %ebx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rbx, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	movq	%r14, %r12
	movq	%rax, %rcx
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%rbx, %rdi
	jne	.LBB1_148
# BB#189:                               # %if.then671
	movabsq	$1, %r14
	jmp	.LBB1_190
.LBB1_148:
	movabsq	$0, %r14
.LBB1_190:                              # %if.then671
	cmpq	%rdi, %r15
	je	.LBB1_193
# BB#191:                               # %if.then671
	testq	%r14, %r14
	jne	.LBB1_193
# BB#192:                               # %if.then684
	movq	%r15, %rsi
	callq	emit_move_insn
.LBB1_193:                              # %if.end686
	movq	optab_table+184(%rip), %rax
	movq	248(%rsp), %rcx         # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB1_194
# BB#195:                               # %if.end686
	movabsq	$1, %rax
	jmp	.LBB1_196
.LBB1_194:
	movabsq	$0, %rax
.LBB1_196:                              # %if.end686
	orq	%r14, %rax
	je	.LBB1_197
# BB#198:                               # %if.else695
	xorq	%r13, %r13
	testq	%r15, %r15
	movq	%r12, %rbx
	je	.LBB1_218
# BB#199:                               # %if.then698
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	movl	word_mode(%rip), %r14d
	shrq	%cl, %rsi
	andq	$32, %rsi
	orq	$31, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r14, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	jmp	.LBB1_214
.LBB1_197:                              # %if.then692
	movslq	word_mode(%rip), %rax
	movq	const_tiny_rtx(,%rax,8), %r13
	movq	120(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB1_215
.LBB1_162:                              # %if.then480
	movq	224(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	movq	%r14, %rdi
	jne	.LBB1_164
# BB#163:                               # %if.then483
	callq	gen_reg_rtx
.LBB1_164:                              # %if.end485
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rax, %rbx
	callq	convert_move
	movq	%rbx, %r12
	jmp	.LBB1_96
.LBB1_207:
	movabsq	$0, %rax
.LBB1_209:                              # %if.end747
	movq	128(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rbx
	movq	224(%rsp), %r14         # 8-byte Reload
	je	.LBB1_212
# BB#210:                               # %if.end747
	testq	%rax, %rax
	jne	.LBB1_212
# BB#211:                               # %if.then753
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB1_212:                              # %if.end755
	testq	%rbx, %rbx
	je	.LBB1_218
# BB#213:                               # %if.end760
	movl	word_mode(%rip), %edi
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	movq	208(%rsp), %rcx         # 8-byte Reload
.LBB1_214:                              # %if.end760
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %r13
.LBB1_215:                              # %if.end760
	testq	%r13, %r13
	je	.LBB1_218
# BB#216:                               # %if.end760
	cmpq	%rbx, %r13
	je	.LBB1_218
# BB#217:                               # %if.then766
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
.LBB1_218:                              # %if.end769
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	testq	%r13, %r13
	je	.LBB1_219
# BB#237:                               # %if.then773
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edi
	xorq	%r8, %r8
	testq	%rdi, %rdi
	movq	208(%rsp), %r15         # 8-byte Reload
	movq	192(%rsp), %rbx         # 8-byte Reload
	je	.LBB1_239
# BB#238:                               # %if.then777
	movq	232(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r8
.LBB1_239:                              # %if.end781
	movq	%r14, %rdi
	movq	200(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	emit_no_conflict_block
	jmp	.LBB1_96
.LBB1_219:
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	192(%rsp), %r13         # 8-byte Reload
.LBB1_220:                              # %if.end784
	movq	optab_table+192(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_222
# BB#221:                               # %lor.lhs.false787
	movq	optab_table+200(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_272
.LBB1_222:                              # %land.lhs.true790
	cmpq	$1, %r8
	jne	.LBB1_272
# BB#223:                               # %land.lhs.true793
	movq	208(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB1_272
# BB#224:                               # %land.lhs.true798
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%r14), %edx
	movq	$22, %rcx
	shrq	%cl, %rax
	andq	$8, %rax
	addq	$8, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB1_272
# BB#225:                               # %land.lhs.true808
	movslq	word_mode(%rip), %rax
	movq	optab_table+168(%rip), %rdx
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	8(%rdx,%rsi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB1_272
# BB#226:                               # %land.lhs.true815
	movq	optab_table+176(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB1_272
# BB#227:                               # %if.then822
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	200(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB1_230
# BB#228:                               # %if.then822
	cmpq	%r13, %rcx
	je	.LBB1_230
# BB#229:                               # %if.then822
	movq	208(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rcx
	jne	.LBB1_231
.LBB1_230:                              # %if.then851
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rcx
.LBB1_231:                              # %if.end853
	movq	%rcx, %r12
	movq	232(%rsp), %r13         # 8-byte Reload
	callq	start_sequence
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	optab_table+192(%rip), %rdx
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	cmpq	%rdx, %rbx
	movabsq	$0, %r15
	movabsq	$1, %rax
	movq	%r15, %r14
	jne	.LBB1_233
# BB#232:                               # %if.end853
	movq	%rax, %r14
.LBB1_233:                              # %if.end853
	movq	%rbx, 248(%rsp)         # 8-byte Spill
	je	.LBB1_235
# BB#234:                               # %if.end853
	movq	%rax, %r15
.LBB1_235:                              # %if.end853
	movq	$1, %rdx
	movq	%r12, %rbx
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r13, %r12
	movq	%r12, %rcx
	callq	operand_subword
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rcx
	callq	operand_subword
	movq	%rax, %r13
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	operand_subword_force
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	operand_subword_force
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	movq	%rsi, %rdx
	andq	%rcx, %rdx
	cmpq	%rdx, 224(%rsp)         # 8-byte Folded Reload
	jne	.LBB1_240
# BB#236:                               # %if.then872
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	const_int_rtx+512(%rip), %rbx
	movq	192(%rsp), %r13         # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	200(%rsp), %r12         # 8-byte Reload
	jmp	.LBB1_270
.LBB1_240:                              # %if.else875
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	120(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %rdx
	movabsq	$1, %rbx
	movq	%rdx, %rdi
	jne	.LBB1_242
# BB#241:                               # %if.else875
	movq	%rbx, %rdi
.LBB1_242:                              # %if.else875
	movq	$32, %rcx
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jge	.LBB1_244
# BB#243:                               # %if.else875
	movq	%rbx, %rdx
.LBB1_244:                              # %if.else875
	movq	optab_table+176(%rip), %rcx
	movq	optab_table+168(%rip), %rbx
	movq	%rbx, 120(%rsp)         # 8-byte Spill
	xorq	%rdx, %rdi
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	movq	200(%rsp), %r12         # 8-byte Reload
	jne	.LBB1_246
# BB#245:                               # %if.else875
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 112(%rsp)         # 8-byte Spill
.LBB1_246:                              # %if.else875
	movq	%r15, 104(%rsp)         # 8-byte Spill
	jne	.LBB1_248
# BB#247:                               # %if.else875
	movq	%rcx, 120(%rsp)         # 8-byte Spill
.LBB1_248:                              # %if.else875
	cmpq	%rsi, %rax
	movq	176(%rsp), %r14         # 8-byte Reload
	jle	.LBB1_250
# BB#249:                               # %if.then913
	movq	%r13, %rax
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movl	target_flags(%rip), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$64, %rax
	movq	$64, %rsi
	subq	%r13, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	jmp	.LBB1_251
.LBB1_250:                              # %if.else927
	subq	%r13, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rdi, %rdi
	movq	%r13, %rsi
.LBB1_251:                              # %if.end936
	callq	gen_rtx_CONST_INT
	movq	%rax, %r15
	movq	192(%rsp), %r13         # 8-byte Reload
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movl	word_mode(%rip), %edi
	movq	%r14, (%rsp)
	xorq	%r8, %r8
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	movl	word_mode(%rip), %edi
	movq	%r14, %rcx
	movq	%rax, %r14
	movq	%rcx, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	testq	%r14, %r14
	je	.LBB1_252
# BB#253:                               # %if.end936
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB1_261
# BB#254:                               # %if.end947
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rdx
	je	.LBB1_256
# BB#255:                               # %if.end947
	movq	%rcx, %rdx
.LBB1_256:                              # %if.end947
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	jne	.LBB1_258
# BB#257:                               # %if.end947
	movq	%rcx, %rax
.LBB1_258:                              # %if.end947
	cmpq	%r14, %rbx
	je	.LBB1_261
# BB#259:                               # %if.end947
	testq	%rax, %rax
	jne	.LBB1_261
# BB#260:                               # %if.then953
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB1_261
.LBB1_252:
	xorq	%rbx, %rbx
.LBB1_261:                              # %if.end955
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	movq	176(%rsp), %r14         # 8-byte Reload
	movq	%r14, (%rsp)
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	movl	word_mode(%rip), %edi
	movq	%rax, %r15
	movq	%r14, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB1_262
# BB#263:                               # %if.end955
	testq	%r15, %r15
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	%r15, %rdx
	je	.LBB1_264
# BB#265:                               # %if.end955
	testq	%rax, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
	je	.LBB1_267
# BB#266:                               # %if.then966
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rax, %rcx
	movq	%r15, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %rbx
	jmp	.LBB1_267
.LBB1_262:
	movq	168(%rsp), %r14         # 8-byte Reload
.LBB1_264:
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	72(%rsp), %rbx          # 8-byte Reload
.LBB1_267:                              # %if.end968
	testq	%rbx, %rbx
	je	.LBB1_270
# BB#268:                               # %if.end968
	cmpq	%r15, %rbx
	je	.LBB1_270
# BB#269:                               # %if.then974
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB1_270:                              # %if.end977
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	testq	%rbx, %rbx
	je	.LBB1_271
# BB#275:                               # %if.then981
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %edi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB1_277
# BB#276:                               # %if.then985
	movq	232(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	movq	208(%rsp), %rcx         # 8-byte Reload
	callq	gen_rtx_fmt_ee
.LBB1_277:                              # %if.end989
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rcx, %rdx
	cmpq	%rdx, 224(%rsp)         # 8-byte Folded Reload
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%r15, %rdi
	jne	.LBB1_278
# BB#279:                               # %if.else997
	callq	emit_insns
	jmp	.LBB1_96
.LBB1_271:
	movq	%r12, 200(%rsp)         # 8-byte Spill
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB1_272:                              # %if.end1001
	movq	optab_table(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_282
# BB#273:                               # %lor.lhs.false1004
	movq	optab_table+16(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_274
# BB#280:                               # %lor.lhs.false1004
	cmpq	$1, %r8
	je	.LBB1_283
# BB#281:
	movq	200(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_315
.LBB1_282:                              # %land.lhs.true1007
	cmpq	$1, %r8
	jne	.LBB1_370
.LBB1_283:                              # %land.lhs.true1010
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r14), %eax
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$22, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	$8, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB1_285
# BB#284:
	movq	200(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_315
.LBB1_285:                              # %land.lhs.true1020
	movslq	word_mode(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%r9,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_287
# BB#286:
	movq	200(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_315
.LBB1_274:
	movq	200(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_315
.LBB1_287:                              # %if.then1027
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movzbl	mode_bitsize(%r14,%r14), %r14d
	movabsq	$4293918720, %rax       # imm = 0xFFF00000
	andq	%rax, %rsi
	movq	$20, %rcx
	shrq	%cl, %rsi
	addq	$32, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rax
	idivq	%rsi
	movq	%rax, %r15
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	force_reg
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	force_reg
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	200(%rsp), %r12         # 8-byte Reload
	testq	%r12, %r12
	je	.LBB1_291
# BB#288:                               # %lor.lhs.false1056
	movl	(%r12), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB1_291
# BB#289:                               # %lor.lhs.false1056
	movq	104(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %r12
	je	.LBB1_291
# BB#290:                               # %lor.lhs.false1056
	movq	96(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %r12
	jne	.LBB1_292
.LBB1_291:                              # %if.then1067
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	movl	(%r12), %eax
.LBB1_292:                              # %if.end1069
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	%r12, 200(%rsp)         # 8-byte Spill
	movq	%r13, 192(%rsp)         # 8-byte Spill
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB1_294
# BB#293:                               # %if.then1074
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB1_294:                              # %for.cond1078.preheader
	xorq	%r13, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB1_311
# BB#295:
	andq	%rax, %r15
	movq	%r15, 224(%rsp)         # 8-byte Spill
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB1_296:                              # %for.body1081
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	operand_subword
	movq	%rax, %r14
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, %r15
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movl	word_mode(%rip), %edi
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, (%rsp)
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r14, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB1_297
# BB#298:                               # %if.end1096
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	%r14, 112(%rsp)         # 8-byte Spill
	leaq	1(%r13), %r14
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	224(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rdx
	jae	.LBB1_302
# BB#299:                               # %if.then1099
                                        #   in Loop: Header=BB1_296 Depth=1
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	optab_table(%rip), %rcx
	movq	$107, %rsi
	cmpq	%rcx, 248(%rsp)         # 8-byte Folded Reload
	je	.LBB1_301
# BB#300:                               # %select.mid19
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	$105, %rsi
.LBB1_301:                              # %select.end18
                                        #   in Loop: Header=BB1_296 Depth=1
	movl	word_mode(%rip), %r8d
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	%rax, %rdi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	emit_store_flag_force
	movq	%rax, %rdx
.LBB1_302:                              # %if.end1105
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_303
# BB#304:                               # %if.then1108
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	%rdx, %r15
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rbx, (%rsp)
	movq	248(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	128(%rsp), %rcx         # 8-byte Reload
	callq	expand_binop
	movq	%rax, %rcx
	movq	224(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	jae	.LBB1_305
# BB#306:                               # %if.then1120
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	optab_table(%rip), %rcx
	movq	$107, %rsi
	cmpq	%rcx, %rbx
	je	.LBB1_308
# BB#307:                               # %select.mid21
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	$105, %rsi
.LBB1_308:                              # %select.end20
                                        #   in Loop: Header=BB1_296 Depth=1
	movl	word_mode(%rip), %r8d
	movq	$1, %r9
	movq	$1, (%rsp)
	movq	%rax, %rdi
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	emit_store_flag_force
	movq	optab_table+152(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r9, %r9
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%rdx, %r8
	callq	expand_binop
	movq	%rbx, %rcx
	testq	%rax, %rax
	movq	%rax, %r15
	movq	200(%rsp), %r12         # 8-byte Reload
	jne	.LBB1_309
	jmp	.LBB1_311
	.align	16, 0x90
.LBB1_303:                              #   in Loop: Header=BB1_296 Depth=1
	movq	200(%rsp), %r12         # 8-byte Reload
	jmp	.LBB1_310
.LBB1_305:                              #   in Loop: Header=BB1_296 Depth=1
	movq	200(%rsp), %r12         # 8-byte Reload
.LBB1_309:                              # %if.end1132
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rcx, %rsi
	callq	emit_move_insn
	movq	%r15, %rdx
.LBB1_310:                              # %for.cond1078.backedge
                                        #   in Loop: Header=BB1_296 Depth=1
	movq	224(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	%rdx, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	jb	.LBB1_296
	jmp	.LBB1_311
.LBB1_297:
	movq	200(%rsp), %r12         # 8-byte Reload
.LBB1_311:                              # %for.end1137
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	xorq	%rdx, %rdx
	movq	80(%rsp), %rax          # 8-byte Reload
	idivq	%rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r13
	cmpq	%rax, %r13
	jne	.LBB1_314
# BB#312:                               # %if.then1147
	movq	optab_table+240(%rip), %rax
	movq	$4, %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	232(%rsp), %rbx         # 8-byte Reload
	je	.LBB1_96
# BB#313:                               # %if.then1154
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	emit_move_insn
	movq	248(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %r14d
	movq	%rax, %r15
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movq	%r12, %r13
	movq	%rax, %r12
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r13, %r12
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	jmp	.LBB1_96
.LBB1_314:                              # %if.else1164
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	%r12, %r13
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB1_315:                              # %if.end1166
	movq	optab_table+32(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_316
# BB#317:                               # %if.end1166
	cmpq	$1, %r8
	jne	.LBB1_318
# BB#319:                               # %land.lhs.true1172
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%r14), %edx
	movq	$22, %rcx
	shrq	%cl, %rax
	andq	$8, %rax
	addq	$8, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB1_320
# BB#321:                               # %land.lhs.true1182
	movslq	word_mode(%rip), %rax
	movq	$4, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movl	8(%r9,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB1_323
# BB#322:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_316:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_318:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_320:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_323:                              # %land.lhs.true1189
	movq	optab_table(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_325
# BB#324:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_325:                              # %land.lhs.true1196
	movq	optab_table+72(%rip), %rax
	movq	$4, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_328
# BB#326:                               # %lor.lhs.false1203
	movq	optab_table+64(%rip), %rax
	movq	$4, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_328
# BB#327:
	movq	%r13, 200(%rsp)         # 8-byte Spill
	jmp	.LBB1_370
.LBB1_328:                              # %if.then1210
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	192(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, %r14
	movq	$1, %rsi
	movq	208(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, 224(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%rax, %r15
	cmpq	%r12, %r13
	je	.LBB1_332
# BB#329:                               # %if.then1210
	movq	208(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r13
	je	.LBB1_332
# BB#330:                               # %lor.lhs.false1229
	xorq	%rbx, %rbx
	testq	%r13, %r13
	je	.LBB1_333
# BB#331:                               # %land.lhs.true1232
	movzbl	(%r13), %eax
	cmpq	$61, %rax
	movq	%r13, %rbx
	je	.LBB1_333
.LBB1_332:                              # %if.then1237
	xorq	%rbx, %rbx
.LBB1_333:                              # %if.end1238
	movq	optab_table+72(%rip), %rsi
	movq	$4, %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movl	8(%rsi,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	%r14, %r12
	je	.LBB1_337
# BB#334:                               # %if.then1245
	movq	$1, %r9
	movq	$0, (%rsp)
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_336
# BB#335:
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_347
.LBB1_278:                              # %if.then995
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%rax, %r8
	callq	emit_no_conflict_block
	jmp	.LBB1_96
.LBB1_336:                              # %if.then1249
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	168(%rsp), %rdx         # 8-byte Reload
.LBB1_337:                              # %land.lhs.true1255
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	%r15, %rbx
	movq	optab_table+64(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB1_369
# BB#338:                               # %if.then1262
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	orq	$31, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	optab_table+64(%rip), %rsi
	movq	%rax, %r14
	movq	$1, %r9
	movq	$0, (%rsp)
	movq	232(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%rbx, %r15
	movq	%r15, %rcx
	movq	200(%rsp), %r8          # 8-byte Reload
	callq	expand_binop
	movq	optab_table+176(%rip), %rsi
	movl	word_mode(%rip), %edi
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	$1, %r9
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, (%rsp)
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movl	word_mode(%rip), %edi
	testq	%rax, %rax
	je	.LBB1_340
# BB#339:                               # %if.then1273
	movq	optab_table(%rip), %rsi
	jmp	.LBB1_342
.LBB1_340:                              # %if.else1275
	xorq	%r13, %r13
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	%r12, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_343
# BB#341:                               # %if.then1278
	movq	optab_table+16(%rip), %rsi
	movl	word_mode(%rip), %edi
.LBB1_342:                              # %if.end1281
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	128(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movq	%rax, %r8
	callq	expand_binop
	movq	%rax, %r13
.LBB1_343:                              # %if.end1281
	movq	optab_table+176(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%rbx, (%rsp)
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	movl	word_mode(%rip), %edi
	testq	%rax, %rax
	je	.LBB1_345
# BB#344:                               # %if.then1284
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	optab_table(%rip), %rsi
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movq	%rax, %r8
	callq	expand_binop
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB1_347
.LBB1_345:                              # %if.else1286
	movq	optab_table+184(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	%r15, %rdx
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	%r14, %rcx
	callq	expand_binop
	testq	%rax, %rax
	movq	112(%rsp), %r14         # 8-byte Reload
	je	.LBB1_369
# BB#346:                               # %if.then1289
	movq	optab_table+16(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r9, %r9
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movq	%rax, %r8
	callq	expand_binop
	movq	%rax, 224(%rsp)         # 8-byte Spill
.LBB1_347:                              # %if.end1293
	testq	%r14, %r14
	je	.LBB1_369
# BB#348:                               # %if.end1293
	testq	%r13, %r13
	je	.LBB1_369
# BB#349:                               # %if.end1293
	cmpq	$0, 224(%rsp)           # 8-byte Folded Reload
	je	.LBB1_369
# BB#350:                               # %if.then1299
	movq	$1, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	232(%rsp), %rcx         # 8-byte Reload
	callq	operand_subword
	movl	word_mode(%rip), %edi
	movq	%r12, %rdx
	movq	%rax, %r15
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	224(%rsp), %rcx         # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_369
# BB#351:                               # %if.end1309
	movq	optab_table(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r9, %r9
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r15, %r8
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB1_352
# BB#353:                               # %if.end1309
	movabsq	$1, %rax
	jmp	.LBB1_354
.LBB1_352:
	movabsq	$0, %rax
.LBB1_354:                              # %if.end1309
	cmpq	%r15, %rbx
	je	.LBB1_357
# BB#355:                               # %if.end1309
	testq	%rax, %rax
	jne	.LBB1_357
# BB#356:                               # %if.then1315
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB1_357:                              # %if.end1317
	testq	%rbx, %rbx
	movq	%r13, %rcx
	je	.LBB1_369
# BB#358:                               # %if.end1322
	movl	word_mode(%rip), %edi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_369
# BB#359:                               # %if.end1327
	movq	optab_table(%rip), %rsi
	movl	word_mode(%rip), %edi
	xorq	%r9, %r9
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r15, %r8
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB1_360
# BB#361:                               # %if.end1327
	movabsq	$1, %rax
	jmp	.LBB1_362
.LBB1_360:
	movabsq	$0, %rax
.LBB1_362:                              # %if.end1327
	cmpq	%r15, %rbx
	je	.LBB1_365
# BB#363:                               # %if.end1327
	testq	%rax, %rax
	jne	.LBB1_365
# BB#364:                               # %if.then1333
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB1_365:                              # %if.end1335
	testq	%rbx, %rbx
	je	.LBB1_369
# BB#366:                               # %if.then1338
	movq	optab_table+240(%rip), %rax
	movq	$4, %rcx
	movq	168(%rsp), %r13         # 8-byte Reload
	shlq	%cl, %r13
	movl	8(%rax,%r13), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_368
# BB#367:
	movq	%r14, %r12
	jmp	.LBB1_96
.LBB1_369:                              # %if.end1353
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB1_370:                              # %if.end1354
	movq	136(%rsp), %rcx         # 8-byte Reload
	addq	$-5, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB1_580
# BB#371:                               # %if.then1360
	cmpq	$5, %r8
	jne	.LBB1_372
# BB#373:                               # %if.then1360
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movabsq	$1, %rsi
	jmp	.LBB1_374
.LBB1_372:
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	%r8, 240(%rsp)          # 8-byte Spill
	movq	%r9, 248(%rsp)          # 8-byte Spill
	movabsq	$0, %rsi
.LBB1_374:                              # %if.then1360
	movq	200(%rsp), %r12         # 8-byte Reload
	movzbl	mode_unit_size(%r14), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	xorq	$1, %rsi
	incq	%rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %rbx
                                        # kill: RAX<def> RBX<kill>
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$51, %rax
	je	.LBB1_375
# BB#377:                               # %if.end1384
	movq	%r14, 168(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	jne	.LBB1_379
# BB#378:                               # %if.then1386
	movq	232(%rsp), %rdi         # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB1_379:                              # %if.end1388
	movq	%r12, 200(%rsp)         # 8-byte Spill
	callq	start_sequence
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	gen_realpart
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rbx, 224(%rsp)         # 8-byte Spill
	movq	%r12, %rsi
	callq	gen_imagpart
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	192(%rsp), %rsi         # 8-byte Reload
	movzbl	2(%rsi), %eax
	movq	232(%rsp), %rbx         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	xorq	%r13, %r13
	cmpq	%rbx, %rax
	movq	%rsi, %rdx
	movq	%rsi, %r12
	movabsq	$0, %r8
	jne	.LBB1_381
# BB#380:                               # %if.then1396
	movq	224(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	%r12, %r14
	movq	%r14, %rsi
	callq	gen_realpart
	movq	%rax, %r15
	movq	%r13, %rdi
	xorq	%r13, %r13
	movq	%r14, %rsi
	movq	%r14, %r12
	callq	gen_imagpart
	movq	%r15, %rdx
	movq	%rax, %r8
.LBB1_381:                              # %if.end1400
	movq	208(%rsp), %rcx         # 8-byte Reload
	movzbl	2(%rcx), %eax
	cmpq	%rbx, %rax
	movq	%rcx, %rbx
	jne	.LBB1_383
# BB#382:                               # %if.then1406
	movq	224(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%rcx, %r13
	movq	%r8, 176(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	callq	gen_realpart
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	gen_imagpart
	movq	%r15, %rdx
	movq	%r14, %rbx
	movq	176(%rsp), %r8          # 8-byte Reload
	movq	%r13, %rcx
	movq	%rax, %r13
.LBB1_383:                              # %if.end1410
	testq	%rdx, %rdx
	movq	248(%rsp), %rsi         # 8-byte Reload
	je	.LBB1_386
# BB#384:                               # %if.end1410
	testq	%rbx, %rbx
	je	.LBB1_386
# BB#385:                               # %lor.lhs.false1416
	movq	%r8, %rax
	orq	%r13, %rax
	je	.LBB1_386
# BB#387:                               # %if.end1423
	movl	(%rsi), %eax
	cmpq	$79, %rax
	je	.LBB1_438
# BB#388:                               # %if.end1423
	movq	%r13, %r14
	movq	%r8, %r13
	cmpq	$78, %rax
	movq	160(%rsp), %r9          # 8-byte Reload
	je	.LBB1_405
# BB#389:                               # %if.end1423
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	addq	$-75, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	ja	.LBB1_587
# BB#390:                               # %sw.bb
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%rbx, %rcx
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	callq	expand_binop
	testq	%rax, %rax
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_578
# BB#391:                               # %if.else1429
	cmpq	%rbx, %rax
	je	.LBB1_393
# BB#392:                               # %if.then1432
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_393:                              # %if.end1435
	movq	%r13, %rdx
	testq	%rdx, %rdx
	movq	304(%rsp), %r15
	movq	%r14, %rax
	je	.LBB1_396
# BB#394:                               # %if.end1435
	testq	%rax, %rax
	je	.LBB1_396
# BB#395:                               # %if.then1439
	movq	%r15, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rcx
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
.LBB1_402:                              # %if.end1459
	testq	%rax, %rax
	movq	%rax, %rdx
	jne	.LBB1_403
	jmp	.LBB1_578
.LBB1_438:                              # %sw.bb1565
	movq	%rdx, %r15
	testq	%r13, %r13
	movq	%r13, 120(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	160(%rsp), %r13         # 8-byte Reload
	je	.LBB1_439
# BB#449:                               # %if.else1604
	movq	%r8, 176(%rsp)          # 8-byte Spill
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movl	flag_complex_divide_method(%rip), %eax
	cmpq	$1, %rax
	movq	%r15, %rdx
	jne	.LBB1_450
# BB#493:                               # %sw.bb1607
	movq	optab_table+88(%rip), %rax
	cmpq	%rsi, %rax
	je	.LBB1_495
# BB#494:
	movabsq	$optab_table, %rax
	movabsq	$optab_table+32, %rcx
	movabsq	$optab_table+256, %rdi
	movabsq	$optab_table+16, %rsi
	jmp	.LBB1_496
.LBB1_405:                              # %sw.bb1470
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	%r12, 192(%rsp)         # 8-byte Spill
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%rdx, %rsi
	callq	force_reg
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %rcx
	movq	%r13, %rsi
	testq	%rsi, %rsi
	movq	%r14, %rbx
	je	.LBB1_431
# BB#406:                               # %sw.bb1470
	testq	%rbx, %rbx
	je	.LBB1_431
# BB#407:                               # %if.then1474
	movq	%r12, %rdi
	movq	%rcx, %r13
	movq	%r13, 104(%rsp)         # 8-byte Spill
	callq	force_reg
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, %r15
	movq	%r15, (%rsp)
	movq	%r12, %rdi
	movq	160(%rsp), %r14         # 8-byte Reload
	movq	%r12, %rbx
	movq	248(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, %r13
	xorq	%r8, %r8
	movq	%r15, (%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	testq	%r13, %r13
	je	.LBB1_408
# BB#409:                               # %if.then1474
	testq	%rax, %rax
	je	.LBB1_410
# BB#411:                               # %if.end1489
	movq	optab_table+40(%rip), %rcx
	movq	248(%rsp), %rdx         # 8-byte Reload
	cmpq	%rcx, %rdx
	je	.LBB1_412
# BB#413:                               # %select.mid27
	movq	optab_table+16(%rip), %rsi
	jmp	.LBB1_414
.LBB1_396:                              # %if.else1441
	testq	%rdx, %rdx
	movq	128(%rsp), %r14         # 8-byte Reload
	je	.LBB1_397
.LBB1_403:                              # %if.else1463
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	cmpq	%r14, %rdx
	je	.LBB1_578
# BB#404:                               # %if.then1466
	movq	%r14, %rdi
	movq	%rdx, %rsi
	callq	emit_move_insn
	jmp	.LBB1_578
.LBB1_431:                              # %if.else1533
	movq	%rsi, %r15
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rcx, %r13
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_578
# BB#432:                               # %if.else1540
	cmpq	%r14, %rax
	je	.LBB1_434
# BB#433:                               # %if.then1543
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_434:                              # %if.end1546
	movq	%r15, %rcx
	testq	%rcx, %rcx
	je	.LBB1_436
# BB#435:                               # %if.then1549
	movq	304(%rsp), %r15
	movq	%r15, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	jmp	.LBB1_437
.LBB1_439:                              # %if.then1568
	movq	%r8, 176(%rsp)          # 8-byte Spill
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %r14
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_441
# BB#440:                               # %if.then1572
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	%r13, %r9
	callq	expand_binop
	jmp	.LBB1_442
.LBB1_450:                              # %if.else1604
	testq	%rax, %rax
	jne	.LBB1_586
# BB#451:                               # %sw.bb1605
	movq	optab_table+88(%rip), %rax
	cmpq	%rsi, %rax
	movq	%rdx, %rsi
	je	.LBB1_453
# BB#452:
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	movabsq	$optab_table, %rax
	movabsq	$optab_table+32, %rcx
	movabsq	$optab_table+256, %rdx
	movabsq	$optab_table+16, %rdi
	jmp	.LBB1_454
.LBB1_397:                              # %if.else1444
	movq	248(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %edx
	cmpq	$76, %rdx
	jne	.LBB1_402
# BB#398:                               # %if.then1448
	movq	optab_table+24(%rip), %rdx
	cmpq	%rdx, %rcx
	movq	160(%rsp), %r8          # 8-byte Reload
	je	.LBB1_399
# BB#400:                               # %select.mid25
	movq	optab_table+256(%rip), %rsi
	jmp	.LBB1_401
.LBB1_441:                              # %if.else1574
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r13, (%rsp)
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%r14, %r8
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_divmod
.LBB1_442:                              # %if.end1576
	testq	%rax, %rax
	je	.LBB1_443
# BB#444:                               # %if.else1580
	cmpq	%rbx, %rax
	movq	304(%rsp), %r15
	je	.LBB1_446
# BB#445:                               # %if.then1583
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_446:                              # %if.end1586
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	jne	.LBB1_448
# BB#447:                               # %if.then1589
	movq	%r15, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	movq	%rbx, %r8
	movq	%r13, %r9
	jmp	.LBB1_426
.LBB1_443:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_408:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_410:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_495:                              # %if.then.i2746
	movabsq	$optab_table+40, %rcx
	movabsq	$optab_table+264, %rdi
	movabsq	$optab_table+24, %rsi
	movabsq	$optab_table+8, %rax
.LBB1_496:                              # %if.end.i2758
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	(%rdi), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	(%rsi), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	(%rax), %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rdx, %rsi
	callq	force_reg
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	movq	176(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB1_498
# BB#497:                               # %if.then3.i2760
	movq	%r12, %rdi
	callq	force_reg
.LBB1_498:                              # %if.end5.i2763
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	force_reg
	movq	%rax, %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	movq	112(%rsp), %r14         # 8-byte Reload
	movq	%rcx, %r15
	jne	.LBB1_500
# BB#499:                               # %if.else.i2765
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	expand_abs
	movq	%rax, %r14
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	expand_abs
	movq	%rax, %r15
.LBB1_500:                              # %if.end10.i
	testq	%r14, %r14
	je	.LBB1_501
# BB#502:                               # %if.end10.i
	movq	%r12, 224(%rsp)         # 8-byte Spill
	movq	%r13, %r12
	testq	%r15, %r15
	je	.LBB1_503
# BB#504:                               # %if.end14.i
	movq	232(%rsp), %r13         # 8-byte Reload
	movzbl	2(%r14), %ebx
	callq	gen_label_rtx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$107, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %r8
	movq	%r12, %r14
	movq	%r14, %r9
	callq	emit_cmp_and_jump_insns
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_506
# BB#505:                               # %if.then17.i
	xorq	%r8, %r8
	movq	304(%rsp), %r15
	movq	%r15, %rbx
	movq	%rbx, (%rsp)
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	jmp	.LBB1_507
.LBB1_436:                              # %if.else1551
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
.LBB1_437:                              # %if.end1553
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	160(%rsp), %r9          # 8-byte Reload
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB1_428
	jmp	.LBB1_578
.LBB1_453:                              # %if.then.i
	movq	%rdi, 192(%rsp)         # 8-byte Spill
	movabsq	$optab_table+40, %rcx
	movabsq	$optab_table+264, %rdx
	movabsq	$optab_table+24, %rdi
	movabsq	$optab_table+8, %rax
.LBB1_454:                              # %if.end.i
	movq	(%rcx), %r15
	movq	(%rdx), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	(%rdi), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	224(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	callq	force_reg
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	176(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB1_456
# BB#455:                               # %if.then3.i
	movq	%r13, %rdi
	callq	force_reg
.LBB1_456:                              # %if.end5.i
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	force_reg
	movq	%rax, %r12
	movq	%r12, 120(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, %rbx
	movq	%rbx, (%rsp)
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%rdx, %rcx
	movq	%r15, %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	160(%rsp), %r15         # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	%rbx, (%rsp)
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r12, %rcx
	movq	%r15, %r9
	callq	expand_binop
	movq	176(%rsp), %rdx         # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB1_457
# BB#458:                               # %if.end5.i
	testq	%rax, %rax
	je	.LBB1_459
# BB#460:                               # %if.end12.i
	xorq	%r8, %r8
	movq	%rbx, %r14
	movq	%r14, (%rsp)
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rcx
	movq	%r15, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_461
# BB#462:                               # %if.end16.i
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	%r14, (%rsp)
	movq	%rbx, %rdi
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	movq	104(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdx
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	80(%rsp), %rdx          # 8-byte Reload
	xorq	%r8, %r8
	testq	%rdx, %rdx
	je	.LBB1_463
# BB#467:                               # %if.else.i
	movq	%r14, (%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	je	.LBB1_468
# BB#469:                               # %if.else.i
	movq	%rbx, %rsi
	testq	%rax, %rax
	je	.LBB1_470
# BB#471:                               # %if.end33.i
	xorq	%r8, %r8
	movq	304(%rsp), %rcx
	movq	%rcx, %rbx
	movq	%rbx, (%rsp)
	movq	%rsi, %r13
	movq	%r13, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rcx
	movq	%r15, %r14
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	%rbx, (%rsp)
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	movq	%rbx, %rcx
	movq	%rax, %r15
	xorq	%r8, %r8
	movq	%rcx, (%rsp)
                                        # kill: RBX<def> RCX<kill>
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r14, %r12
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	testq	%r15, %r15
	je	.LBB1_472
# BB#473:                               # %if.end33.i
	testq	%rax, %rax
	je	.LBB1_474
# BB#475:                               # %if.end41.i
	xorq	%r8, %r8
	movq	%rbx, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r12, %r9
	callq	expand_binop
	movq	%rax, %r14
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB1_476
# BB#477:                               # %if.end41.i
	testq	%r14, %r14
	movq	%rbx, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	jne	.LBB1_480
# BB#478:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_448:                              # %if.else1591
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r13, (%rsp)
	movq	%r12, %rdx
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	expand_divmod
	jmp	.LBB1_427
.LBB1_412:
	movq	optab_table+24(%rip), %rsi
.LBB1_414:                              # %select.end26
	movq	%rbx, %r12
	movq	%r15, (%rsp)
	movq	%r12, %rdi
	movq	%r13, %rdx
	movq	%rax, %rcx
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_415
# BB#416:                               # %if.else1500
	cmpq	%rbx, %rax
	je	.LBB1_418
# BB#417:                               # %if.then1503
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_418:                              # %if.end1506
	xorq	%r8, %r8
	movq	%r15, %r13
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	248(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, %r15
	xorq	%r8, %r8
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	testq	%r15, %r15
	je	.LBB1_419
# BB#420:                               # %if.end1506
	testq	%rax, %rax
	je	.LBB1_421
# BB#422:                               # %if.end1515
	movq	optab_table+40(%rip), %rcx
	movq	248(%rsp), %rdx         # 8-byte Reload
	cmpq	%rcx, %rdx
	je	.LBB1_423
# BB#424:                               # %select.mid29
	movq	optab_table(%rip), %rsi
	jmp	.LBB1_425
.LBB1_415:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_501:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_503:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_506:                              # %if.else19.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	%r14, (%rsp)
	movq	224(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdx
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	112(%rsp), %r8          # 8-byte Reload
	callq	expand_divmod
	movq	304(%rsp), %r15
	movq	%r15, %rbx
.LBB1_507:                              # %if.end21.i
	movq	%r13, 232(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	104(%rsp), %r15         # 8-byte Reload
	je	.LBB1_508
# BB#509:                               # %if.end24.i
	xorq	%r8, %r8
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	%rax, %rcx
	movq	%rcx, %r13
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_510
# BB#511:                               # %if.end28.i
	xorq	%r8, %r8
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB1_512
# BB#513:                               # %if.end32.i
	movq	80(%rsp), %rdx          # 8-byte Reload
	testq	%rdx, %rdx
	movq	%r13, %rcx
	je	.LBB1_514
# BB#520:                               # %if.else45.i
	xorq	%r8, %r8
	movq	304(%rsp), %r13
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_521
# BB#522:                               # %if.end49.i
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r15, %rbx
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_523
# BB#524:                               # %if.end54.i2771
	xorq	%r8, %r8
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	%r14, %r9
	movq	%r14, %rbx
	callq	expand_binop
	movq	%rax, %r13
	movq	32(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %rcx
	je	.LBB1_525
# BB#526:                               # %if.end54.i2771
	testq	%r13, %r13
	jne	.LBB1_528
# BB#527:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_457:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_459:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_508:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_461:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_399:
	movq	optab_table+264(%rip), %rsi
.LBB1_401:                              # %select.end24
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	expand_unop
	jmp	.LBB1_402
.LBB1_419:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_421:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_510:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_463:                              # %if.then18.i
	movq	%r14, (%rsp)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	movq	176(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	movq	%r15, %r12
	movq	%rcx, %rbx
	je	.LBB1_464
# BB#465:                               # %if.then18.i
	testq	%rax, %rax
	movq	48(%rsp), %r13          # 8-byte Reload
	je	.LBB1_466
# BB#479:                               # %if.end25.i
	xorq	%rcx, %rcx
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r12, %r8
	callq	expand_unop
	movq	%rbx, %rcx
	movq	%rax, %r14
	movq	304(%rsp), %rax
.LBB1_480:                              # %if.end48.i
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_482
# BB#481:                               # %if.then50.i
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r13, %rcx
	movq	136(%rsp), %r8          # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_483
.LBB1_423:
	movq	optab_table+8(%rip), %rsi
.LBB1_425:                              # %select.end28
	movq	304(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	%r14, %r9
.LBB1_426:                              # %select.end28
	callq	expand_binop
.LBB1_427:                              # %select.end28
	testq	%rax, %rax
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_578
.LBB1_428:                              # %if.else1526
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	cmpq	%rbx, %rax
	je	.LBB1_578
# BB#429:                               # %if.then1529
	movq	%rbx, %rdi
.LBB1_430:                              # %sw.epilog1611
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_578:                              # %sw.epilog1611
	callq	get_insns
	movq	%rax, %r13
	callq	end_sequence
	cmpq	$0, 176(%rsp)           # 8-byte Folded Reload
	je	.LBB1_588
# BB#579:
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	240(%rsp), %r8          # 8-byte Reload
	movq	88(%rsp), %rbx          # 8-byte Reload
.LBB1_580:                              # %if.end1627
	movq	$4, %rcx
	movq	%r14, %r13
	shlq	%cl, %r13
	cmpq	$0, 16(%r9,%r13)
	je	.LBB1_596
# BB#581:                               # %if.end1627
	cmpq	$1, %rbx
	jne	.LBB1_596
# BB#582:                               # %if.then1638
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	216(%rsp), %rbx         # 8-byte Reload
	andq	%rax, %rbx
	callq	start_sequence
	cmpq	$4, %rbx
	movq	208(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rcx
	movq	232(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r14
	ja	.LBB1_584
# BB#583:                               # %if.then1645
	movl	word_mode(%rip), %r14d
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	convert_to_mode
	movq	%rax, %rcx
.LBB1_584:                              # %if.end1647
	movq	%r14, 240(%rsp)         # 8-byte Spill
	movq	192(%rsp), %rax         # 8-byte Reload
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	je	.LBB1_585
# BB#591:                               # %if.end1647
	movq	%rcx, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	je	.LBB1_592
# BB#593:                               # %if.then1659
	movq	%rbx, %rdi
	movq	192(%rsp), %rsi         # 8-byte Reload
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %r14
	callq	convert_to_mode
	movq	%rax, %r15
	jmp	.LBB1_594
.LBB1_596:                              # %if.end1671
	movq	%r8, %r15
	movq	%r9, %rbx
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	304(%rsp), %rax
	movq	%rax, %rcx
	addq	$-2, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	jae	.LBB1_608
# BB#597:                               # %if.end1681
	movq	232(%rsp), %r13         # 8-byte Reload
	cmpq	$3, 152(%rsp)           # 8-byte Folded Reload
	movq	%rbx, %r9
	jne	.LBB1_598
# BB#599:                               # %if.end1681
	movabsq	$1, %rax
	jmp	.LBB1_600
.LBB1_588:                              # %if.then1614
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %r12d
	xorq	%r8, %r8
	testq	%r12, %r12
	movq	208(%rsp), %rbx         # 8-byte Reload
	movq	192(%rsp), %r14         # 8-byte Reload
	je	.LBB1_590
# BB#589:                               # %if.then1618
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	copy_rtx
	movq	%r12, %rdi
	movq	232(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r8
.LBB1_590:                              # %if.end1624
	movq	%r13, %rdi
	movq	200(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	emit_no_conflict_block
	jmp	.LBB1_96
.LBB1_585:
	movq	%rbx, %r14
	movq	192(%rsp), %r15         # 8-byte Reload
	jmp	.LBB1_595
.LBB1_592:
	movq	%rbx, %r14
	movq	192(%rsp), %r15         # 8-byte Reload
.LBB1_594:                              # %if.end1661
	movq	%r12, %rcx
.LBB1_595:                              # %if.end1661
	movq	248(%rsp), %r12         # 8-byte Reload
	leaq	16(%r12,%r13), %rax
	movq	(%rax), %rdi
	movq	240(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	%rcx, 8(%rsp)
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$2, %r8
	xorq	%rax, %rax
	movq	%r14, (%rsp)
	movq	%r14, %rcx
	movq	%r15, %r9
	callq	emit_library_call_value
	movq	%rax, %r13
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movl	(%r12), %edi
	movq	%rax, %r12
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	208(%rsp), %rcx         # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%rax, %rcx
	callq	emit_libcall_block
	jmp	.LBB1_96
.LBB1_598:
	movabsq	$0, %rax
.LBB1_600:                              # %if.end1681
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	%r15, %rdx
	cmpq	$6, %rdx
	ja	.LBB1_608
# BB#601:                               # %if.end1681
	movq	$1, %rax
	movq	%rdx, %rcx
	shlq	%cl, %rax
	testq	$70, %rax
	je	.LBB1_608
# BB#602:                               # %for.cond1697.preheader
	movzbl	mode_wider_mode(%r14), %ebx
	jmp	.LBB1_603
.LBB1_627:                              # %if.else1769
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	movq	248(%rsp), %r9          # 8-byte Reload
	movq	240(%rsp), %rdx         # 8-byte Reload
	movzbl	mode_wider_mode(%rbx), %ebx
.LBB1_603:                              # %for.cond1697.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB1_608
# BB#604:                               # %for.body1700
                                        #   in Loop: Header=BB1_603 Depth=1
	andq	$255, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	8(%r9,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB1_609
# BB#605:                               # %lor.lhs.false1707
                                        #   in Loop: Header=BB1_603 Depth=1
	cmpq	$3, 152(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_607
# BB#606:                               # %land.lhs.true1710
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	cmpq	$0, 16(%r9,%rax)
	jne	.LBB1_609
.LBB1_607:                              # %for.cond1697.backedge
                                        #   in Loop: Header=BB1_603 Depth=1
	movzbl	mode_wider_mode(%rbx), %ebx
	jmp	.LBB1_603
.LBB1_609:                              # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+152(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#610:                               # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+144(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#611:                               # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+160(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#612:                               # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#613:                               # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+16(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#614:                               # %if.then1716
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+32(%rip), %rax
	cmpq	%rax, %r9
	je	.LBB1_616
# BB#615:                               # %lor.lhs.false1740
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	optab_table+168(%rip), %rax
	cmpq	%rax, %r9
	jne	.LBB1_617
.LBB1_616:                              # %land.lhs.true1743
                                        #   in Loop: Header=BB1_603 Depth=1
	cmpq	$1, %rdx
	jne	.LBB1_617
# BB#618:                               # %land.rhs1750
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	%rdx, 240(%rsp)         # 8-byte Spill
	movq	$1, %r8
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	%r9, %r14
	callq	widen_operand
	movq	optab_table+168(%rip), %rcx
	cmpq	%rcx, %r14
	movabsq	$0, %r15
	je	.LBB1_620
# BB#619:                               # %land.rhs1750
                                        #   in Loop: Header=BB1_603 Depth=1
	movabsq	$1, %r15
.LBB1_620:                              # %land.rhs1750
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
	movq	208(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %r12
	jmp	.LBB1_621
.LBB1_617:                              # %if.end1747.thread
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	%rdx, 240(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	xorq	%r15, %r15
	movq	192(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	160(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rcx
	callq	widen_operand
	movq	%rax, %r12
	movq	208(%rsp), %rdi         # 8-byte Reload
.LBB1_621:                              # %land.end1753
                                        #   in Loop: Header=BB1_603 Depth=1
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	%rbx, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	widen_operand
	xorq	%r8, %r8
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rbx, %rdi
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%rax, %rcx
	movq	%r14, %r9
	callq	expand_binop
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB1_627
# BB#622:                               # %if.then1758
	cmpq	$1, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_623
# BB#626:                               # %if.else1767
	movq	%r13, %rdi
	movq	%r14, %rsi
	jmp	.LBB1_166
.LBB1_608:                              # %if.end1777
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	delete_insns_since
	xorq	%r12, %r12
.LBB1_96:                               # %return
	movq	%r12, %rax
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_512:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_468:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_470:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_514:                              # %if.then34.i
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_515
# BB#516:                               # %if.end38.i
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r14, %r8
	callq	expand_unop
	movq	%rax, %r13
	testq	%rbx, %rbx
	movq	%rbx, %rax
	je	.LBB1_517
# BB#518:                               # %if.end38.i
	movq	%r14, %rbx
	testq	%r13, %r13
	movq	%rax, %rcx
	movq	%rax, %r15
	je	.LBB1_519
.LBB1_528:                              # %if.end61.i2774
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_530
# BB#529:                               # %if.then63.i2776
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r8
	movq	%rbx, %r12
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_531
.LBB1_482:                              # %if.else52.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %r8
	movq	136(%rsp), %r9          # 8-byte Reload
	callq	expand_divmod
.LBB1_483:                              # %if.end54.i
	testq	%rax, %rax
	je	.LBB1_484
# BB#485:                               # %if.end57.i
	movq	136(%rsp), %rdi         # 8-byte Reload
	cmpq	%rdi, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	je	.LBB1_487
# BB#486:                               # %if.then59.i
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_487:                              # %if.end61.i
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_489
# BB#488:                               # %if.then63.i
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	movq	%r13, %rcx
	movq	%r15, %r8
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_490
.LBB1_521:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_464:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_466:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_623:                              # %if.then1761
	movq	200(%rsp), %r12         # 8-byte Reload
	testq	%r12, %r12
	movq	%r13, %rdi
	jne	.LBB1_625
# BB#624:                               # %if.then1764
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB1_625:                              # %if.end1766
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	convert_move
	jmp	.LBB1_96
.LBB1_484:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_472:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_474:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_368:                              # %if.then1345
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%r14, %r15
	movq	%rax, %r14
	movq	192(%rsp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movq	%rax, %rbx
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	copy_rtx
	movq	$78, %rdi
	movq	232(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	movq	%r15, %r12
	jmp	.LBB1_96
.LBB1_489:                              # %if.else65.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %rcx
	movq	%r13, %r8
	movq	%r15, %r9
	callq	expand_divmod
.LBB1_490:                              # %if.end67.i
	testq	%rax, %rax
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_578
# BB#491:                               # %if.end70.i
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	cmpq	%r15, %rax
	je	.LBB1_578
# BB#492:                               # %if.then72.i
	movq	%r15, %rdi
	jmp	.LBB1_430
.LBB1_523:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_515:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_530:                              # %if.else65.i2778
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%rbx, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %r12
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	136(%rsp), %r14         # 8-byte Reload
	movq	%r14, %r9
	callq	expand_divmod
.LBB1_531:                              # %if.end67.i2781
	testq	%rax, %rax
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB1_532
# BB#533:                               # %if.end70.i2783
	cmpq	%r14, %rax
	je	.LBB1_535
# BB#534:                               # %if.then72.i2785
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_535:                              # %if.end74.i
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_537
# BB#536:                               # %if.then76.i
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_538
.LBB1_476:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_532:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_525:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_517:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_519:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_537:                              # %if.else78.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	movq	%rbx, %r8
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_divmod
.LBB1_538:                              # %if.end80.i
	testq	%rax, %rax
	je	.LBB1_539
# BB#540:                               # %if.end83.i
	cmpq	%rbx, %rax
	je	.LBB1_542
# BB#541:                               # %if.then85.i
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_542:                              # %if.end87.i
	movq	%rbx, %r13
	callq	gen_label_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	emit_label
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_544
# BB#543:                               # %if.then94.i
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_545
.LBB1_539:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_375:                              # %if.then1383
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_binop, %rcx
	movq	$1512, %rsi             # imm = 0x5E8
	jmp	.LBB1_376
.LBB1_386:                              # %if.then1422
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_binop, %rcx
	movq	$1539, %rsi             # imm = 0x603
	jmp	.LBB1_376
.LBB1_587:                              # %sw.default1610
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_binop, %rcx
	movq	$1719, %rsi             # imm = 0x6B7
	jmp	.LBB1_376
.LBB1_586:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_binop, %rcx
	movq	$1713, %rsi             # imm = 0x6B1
.LBB1_376:                              # %if.then1383
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB1_544:                              # %if.else96.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	xorq	%r9, %r9
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	176(%rsp), %r8          # 8-byte Reload
	callq	expand_divmod
.LBB1_545:                              # %if.end98.i
	movq	%rax, 128(%rsp)         # 8-byte Spill
	cmpq	$0, 128(%rsp)           # 8-byte Folded Reload
	je	.LBB1_546
# BB#547:                               # %if.end101.i
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_548
# BB#549:                               # %if.end105.i
	xorq	%r8, %r8
	movq	304(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	movq	%rax, 176(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB1_550
# BB#551:                               # %if.end109.i
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	movq	%rax, 136(%rsp)         # 8-byte Spill
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB1_552
# BB#556:                               # %if.else119.i
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB1_557
# BB#558:                               # %if.end123.i
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%r8, %r8
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %r9
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB1_559
# BB#560:                               # %if.end128.i
	xorq	%r8, %r8
	movq	304(%rsp), %rcx
	movq	%rcx, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%r12, %r9
	callq	expand_binop
	movq	%rax, %r15
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB1_561
# BB#562:                               # %if.end128.i
	testq	%r15, %r15
	jne	.LBB1_564
# BB#563:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_546:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_548:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_550:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_552:                              # %if.then111.i
	xorq	%rcx, %rcx
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r12, %r8
	callq	expand_unop
	movq	%rax, %r15
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB1_553
# BB#554:                               # %if.then111.i
	testq	%r15, %r15
	je	.LBB1_555
.LBB1_564:                              # %if.end135.i
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_566
# BB#565:                               # %if.then137.i
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r8
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_567
.LBB1_557:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_559:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_561:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_553:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_555:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_566:                              # %if.else139.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	176(%rsp), %r8          # 8-byte Reload
	movq	%r14, %r9
	callq	expand_divmod
.LBB1_567:                              # %if.end141.i
	testq	%rax, %rax
	je	.LBB1_568
# BB#569:                               # %if.end144.i
	cmpq	%r14, %rax
	je	.LBB1_571
# BB#570:                               # %if.then146.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_571:                              # %if.end148.i
	cmpq	$6, 240(%rsp)           # 8-byte Folded Reload
	jne	.LBB1_573
# BB#572:                               # %if.then150.i
	movq	304(%rsp), %rax
	movq	%rax, (%rsp)
	movq	224(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	%r13, %r8
	movq	%r12, %r9
	callq	expand_binop
	jmp	.LBB1_574
.LBB1_568:
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.LBB1_573:                              # %if.else152.i
	xorq	%rdi, %rdi
	movq	$62, %rsi
	movq	%r12, (%rsp)
	movq	224(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	movq	176(%rsp), %r8          # 8-byte Reload
	movq	%r13, %r9
	callq	expand_divmod
.LBB1_574:                              # %if.end154.i
	testq	%rax, %rax
	movq	%r13, %rcx
	movq	$1, 176(%rsp)           # 8-byte Folded Spill
	je	.LBB1_578
# BB#575:                               # %if.end157.i
	cmpq	%rcx, %rax
	je	.LBB1_577
# BB#576:                               # %if.then159.i
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB1_577:                              # %if.end161.i
	movq	%rbx, %rdi
	callq	emit_label
	movq	$0, 176(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_578
.Ltmp20:
	.size	expand_binop, .Ltmp20-expand_binop
	.cfi_endproc

	.align	16, 0x90
	.type	add_equal_note,@function
add_equal_note:                         # @add_equal_note
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp32:
	.cfi_def_cfa_offset 80
.Ltmp33:
	.cfi_offset %rbx, -48
.Ltmp34:
	.cfi_offset %r12, -40
.Ltmp35:
	.cfi_offset %r13, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_class(%rax), %r15d
	movq	%rdi, %r14
	movq	$1, %rax
	leaq	-49(%r15), %rcx
	andq	$255, %rcx
	cmpq	$50, %rcx
	ja	.LBB2_24
# BB#1:                                 # %entry
	movq	%rsi, %r12
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$1125899906844675, %rcx # imm = 0x4000000000803
	testq	%rcx, %rdx
	je	.LBB2_24
# BB#2:                                 # %lor.lhs.false
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movzbl	(%r14), %eax
	cmpq	$24, %rax
	jne	.LBB2_23
# BB#3:                                 # %lor.lhs.false21
	movq	8(%r14), %rax
	movl	(%rax), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rax,%rdx,8), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB2_23
# BB#4:                                 # %cond.true
	movq	32(%rdi), %rbx
	movzbl	(%rbx), %eax
	cmpq	$47, %rax
	je	.LBB2_6
# BB#5:                                 # %cond.false
	movq	%rbx, %rsi
	callq	single_set_2
	movq	%rax, %rbx
.LBB2_6:                                # %cond.end93
	testq	%rbx, %rbx
	je	.LBB2_23
# BB#7:                                 # %lor.lhs.false97
	movzbl	(%r12), %eax
	cmpq	$133, %rax
	je	.LBB2_23
# BB#8:                                 # %lor.lhs.false102
	movq	8(%rbx), %rdi
	movq	%r12, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_11
# BB#9:                                 # %land.lhs.true107
	movq	%r12, %rax
	movq	%rcx, %r12
	movq	%rax, %rdx
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$64, %rcx
	jne	.LBB2_23
# BB#10:                                # %lor.lhs.false115
	movq	8(%rax), %rax
	movq	8(%rax), %rdi
	movq	%rdx, %rbx
	movq	%rbx, %rsi
	callq	rtx_equal_p
	movq	%r12, %rcx
	movq	%rbx, %r12
	testq	%rcx, %rax
	je	.LBB2_23
.LBB2_11:                               # %if.end
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rdi
	movq	%rcx, %rbx
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	reg_overlap_mentioned_p
	movq	%rbx, %rsi
	testq	%rsi, %rax
	jne	.LBB2_14
# BB#12:                                # %lor.lhs.false129
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB2_19
# BB#13:                                # %land.lhs.true131
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rsi, %rbx
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	reg_overlap_mentioned_p
	movq	%rbx, %rsi
	testq	%rsi, %rax
	je	.LBB2_19
.LBB2_14:                               # %if.then134
	movq	8(%r14), %rax
	movl	(%rax), %ebx
	addq	$-2, %rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB2_19
# BB#15:                                # %for.body.lr.ph
	movq	%rsi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	jmp	.LBB2_18
.LBB2_17:                               # %for.cond.for.body_crit_edge
                                        #   in Loop: Header=BB2_18 Depth=1
	movq	8(%r14), %rax
	decq	%rbx
.LBB2_18:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax,%rbx,8), %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	reg_set_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	jne	.LBB2_24
# BB#16:                                # %for.cond
                                        #   in Loop: Header=BB2_18 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_17
.LBB2_19:                               # %if.end152
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	2(%rax), %r12d
	andq	$255, %r15
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	%rax, %rbx
	cmpq	$49, %r15
	jne	.LBB2_21
# BB#20:                                # %if.then158
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	jmp	.LBB2_22
.LBB2_21:                               # %if.else
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
.LBB2_22:                               # %if.end169
	movq	8(%r14), %rdx
	movl	(%rdx), %esi
	decq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	8(%rdx,%rsi,8), %rdi
	movq	$4, %rsi
	movq	%rax, %rdx
	callq	set_unique_reg_note
.LBB2_23:                               # %return
	movq	$1, %rax
.LBB2_24:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	add_equal_note, .Ltmp38-add_equal_note
	.cfi_endproc

	.align	16, 0x90
	.type	widen_operand,@function
widen_operand:                          # @widen_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 32
.Ltmp46:
	.cfi_offset %rbx, -32
.Ltmp47:
	.cfi_offset %r14, -24
.Ltmp48:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	%rsi, %r14
	movq	%rdi, %r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r8
	je	.LBB3_7
# BB#1:                                 # %land.lhs.true
	movl	(%r15), %edi
	testq	$16711680, %rdi         # imm = 0xFF0000
	je	.LBB3_6
# BB#2:                                 # %lor.lhs.false
	movq	%rdi, %rcx
	andq	$268500991, %rcx        # imm = 0x1000FFFF
	cmpq	$268435519, %rcx        # imm = 0x1000003F
	jne	.LBB3_4
# BB#3:                                 # %land.lhs.true10
	movq	$26, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	andq	$1, %rbx
	andq	%rax, %rsi
	cmpq	%rsi, %rbx
	jne	.LBB3_4
.LBB3_7:                                # %if.then15
	movq	%r14, %rdi
	movq	%rdx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	convert_modes  # TAILCALL
.LBB3_4:                                # %if.end16
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB3_8
# BB#5:                                 # %if.end26
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movzbl	2(%r15), %edi
	movq	%r14, %rsi
	callq	gen_lowpart
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	%r14, %r15
.LBB3_6:                                # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB3_8:                                # %if.then20
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r15, %rsi
	callq	force_reg
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	gen_rtx_SUBREG  # TAILCALL
.Ltmp49:
	.size	widen_operand, .Ltmp49-widen_operand
	.cfi_endproc

	.globl	emit_no_conflict_block
	.align	16, 0x90
	.type	emit_no_conflict_block,@function
emit_no_conflict_block:                 # @emit_no_conflict_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp58:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp59:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp61:
	.cfi_def_cfa_offset 80
.Ltmp62:
	.cfi_offset %rbx, -48
.Ltmp63:
	.cfi_offset %r12, -40
.Ltmp64:
	.cfi_offset %r13, -32
.Ltmp65:
	.cfi_offset %r14, -24
.Ltmp66:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movzbl	(%r13), %eax
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	cmpq	$61, %rax
	jne	.LBB4_53
# BB#1:                                 # %entry
	movl	reload_in_progress(%rip), %eax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB4_53
# BB#2:                                 # %for.cond.preheader
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%r14, %r14
	movabsq	$0, %r15
	je	.LBB4_8
# BB#3:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB4_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$32, %rax
	jne	.LBB4_53
# BB#5:                                 # %lor.lhs.false5
                                        #   in Loop: Header=BB4_4 Depth=1
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB4_53
# BB#6:                                 # %for.inc
                                        #   in Loop: Header=BB4_4 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_4
# BB#7:
	movq	%r14, %rbx
	movq	%r14, %r15
	jmp	.LBB4_8
.LBB4_53:                               # %if.then
	movq	%r14, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insns  # TAILCALL
.LBB4_25:                               # %if.end120
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%r14, %rdi
	callq	add_insn
	.align	16, 0x90
.LBB4_8:                                # %for.cond11
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_18 Depth 2
	movq	%r15, %r14
	testq	%r14, %r14
	je	.LBB4_26
# BB#9:                                 # %for.body13
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	24(%r14), %r15
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB4_11
# BB#10:                                # %if.then19
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB4_11:                               # %if.end20
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB4_13
# BB#12:                                # %if.then23
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB4_13:                               # %if.end24
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	32(%r14), %rcx
	movzbl	(%rcx), %eax
	leaq	-47(%rax), %rdx
	andq	%r12, %rdx
	cmpq	$3, %rdx
	jb	.LBB4_19
# BB#14:                                # %if.end24
                                        #   in Loop: Header=BB4_8 Depth=1
	cmpq	$39, %rax
	jne	.LBB4_16
# BB#15:                                # %for.cond57.preheader
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	8(%rcx), %rax
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB4_16
	.align	16, 0x90
.LBB4_18:                               # %for.body64
                                        #   Parent Loop BB4_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rsi,8), %rcx
	movzbl	(%rcx), %edi
	cmpq	$47, %rdi
	je	.LBB4_19
# BB#17:                                # %for.cond57
                                        #   in Loop: Header=BB4_18 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB4_18
	jmp	.LBB4_16
	.align	16, 0x90
.LBB4_19:                               # %if.end89
                                        #   in Loop: Header=BB4_8 Depth=1
	testq	%rcx, %rcx
	je	.LBB4_16
# BB#20:                                # %if.end92
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	8(%rcx), %rsi
	movq	%r13, %rdi
	callq	reg_overlap_mentioned_p
	testq	%r12, %rax
	jne	.LBB4_8
# BB#21:                                # %if.then98
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	16(%r14), %rax
	testq	%rax, %rax
	movq	%r15, %rcx
	je	.LBB4_23
# BB#22:                                # %if.then103
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%r15, 24(%rax)
	movq	%rbx, %rcx
.LBB4_23:                               # %if.end111
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	%rcx, %rbx
	testq	%r15, %r15
	je	.LBB4_25
# BB#24:                                # %if.then113
                                        #   in Loop: Header=BB4_8 Depth=1
	movq	16(%r14), %rax
	movq	%rax, 16(%r15)
	jmp	.LBB4_25
.LBB4_26:                               # %for.end123
	callq	get_last_insn
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	16(%rsp), %r15          # 8-byte Reload
	cmpq	%r15, %r13
	movq	24(%rsp), %r12          # 8-byte Reload
	je	.LBB4_29
# BB#27:                                # %for.end123
	cmpq	%r12, %r13
	je	.LBB4_29
# BB#28:                                # %if.then127
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB4_29:                               # %for.cond131.preheader
	testq	%rbx, %rbx
	je	.LBB4_45
# BB#30:                                # %for.body133.lr.ph
	testq	%r12, %r12
	je	.LBB4_35
# BB#31:                                # %for.body133.lr.ph.for.body133.lr.ph.split_crit_edge
	testq	%r15, %r15
	je	.LBB4_32
	.align	16, 0x90
.LBB4_40:                               # %land.lhs.true138
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	movq	%rbx, %rdi
	callq	add_insn
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB4_42
# BB#41:                                # %if.then142
                                        #   in Loop: Header=BB4_40 Depth=1
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$9, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB4_42:                               # %land.lhs.true152
                                        #   in Loop: Header=BB4_40 Depth=1
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB4_44
# BB#43:                                # %if.then156
                                        #   in Loop: Header=BB4_40 Depth=1
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$9, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB4_44:                               # %for.cond131.backedge
                                        #   in Loop: Header=BB4_40 Depth=1
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB4_40
	jmp	.LBB4_45
	.align	16, 0x90
.LBB4_32:                               # %land.lhs.true138.us285
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	movq	%rbx, %rdi
	callq	add_insn
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	jne	.LBB4_34
# BB#33:                                # %if.then142.us289
                                        #   in Loop: Header=BB4_32 Depth=1
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$9, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB4_34:                               # %for.cond131.backedge.us304
                                        #   in Loop: Header=BB4_32 Depth=1
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB4_32
	jmp	.LBB4_45
.LBB4_35:                               # %for.body133.lr.ph.split.us
	testq	%r15, %r15
	je	.LBB4_36
	.align	16, 0x90
.LBB4_37:                               # %land.lhs.true152.us
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	movq	%rbx, %rdi
	callq	add_insn
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB4_39
# BB#38:                                # %if.then156.us
                                        #   in Loop: Header=BB4_37 Depth=1
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$9, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB4_39:                               # %for.cond131.backedge.us
                                        #   in Loop: Header=BB4_37 Depth=1
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB4_37
	jmp	.LBB4_45
	.align	16, 0x90
.LBB4_36:                               # %for.cond131.backedge.us.us
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	movq	%rbx, %rdi
	callq	add_insn
	testq	%r14, %r14
	movq	%r14, %rbx
	jne	.LBB4_36
.LBB4_45:                               # %for.end166
	movzbl	2(%r13), %eax
	movq	optab_table+240(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB4_46
# BB#48:                                # %if.else178
	callq	get_last_insn
	movq	%rax, %r14
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	find_reg_note
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	remove_note
	jmp	.LBB4_49
.LBB4_46:                               # %if.then172
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
	movq	%rax, %r14
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB4_49
# BB#47:                                # %if.then175
	movq	$4, %rsi
	movq	%r14, %rdi
	callq	set_unique_reg_note
.LBB4_49:                               # %if.end181
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%rax, %rax
	je	.LBB4_50
# BB#51:                                # %if.else185
	movq	24(%rax), %rbx
	jmp	.LBB4_52
.LBB4_50:                               # %if.then183
	callq	get_insns
	movq	%rax, %rbx
.LBB4_52:                               # %return
	movq	56(%rbx), %rcx
	movq	$4, %rdi
	movq	$7, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%rbx)
	movq	56(%r14), %rcx
	movq	$4, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r14)
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_16:                               # %if.then91
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.emit_no_conflict_block, %rdx
	movq	$2768, %rsi             # imm = 0xAD0
	callq	fancy_abort
.Ltmp67:
	.size	emit_no_conflict_block, .Ltmp67-emit_no_conflict_block
	.cfi_endproc

	.globl	expand_unop
	.align	16, 0x90
	.type	expand_unop,@function
expand_unop:                            # @expand_unop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp74:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp75:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp76:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp77:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp79:
	.cfi_def_cfa_offset 128
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
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %rbx
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$4, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	leaq	8(%rsi,%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jmp	.LBB5_1
	.align	16, 0x90
.LBB5_18:                               # %if.then61
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	xorq	%r14, %r14
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB5_1:                                # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	callq	get_last_insn
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	je	.LBB5_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rbx, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
.LBB5_3:                                # %if.end
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB5_5
# BB#4:                                 # %if.then4
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
.LBB5_5:                                # %if.end6
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx), %rcx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB5_21
# BB#6:                                 # %if.then9
                                        #   in Loop: Header=BB5_1 Depth=1
	leaq	(%rcx,%rcx,4), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rcx,8), %r14
	movzbl	40(%r14), %r13d
	testq	%rax, %rax
	movq	%rax, %r15
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jne	.LBB5_8
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB5_8:                                # %if.end21
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	movq	%rbx, %r12
	je	.LBB5_11
# BB#9:                                 # %if.end21
                                        #   in Loop: Header=BB5_1 Depth=1
	cmpq	%r13, %rax
	movq	%rbx, %r12
	je	.LBB5_11
# BB#10:                                # %if.then28
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	convert_to_mode
	movq	%rax, %r12
.LBB5_11:                               # %if.end30
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	*24(%r14)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_13
# BB#12:                                # %if.then37
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
.LBB5_13:                               # %if.end39
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	*(%r14)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_15
# BB#14:                                # %if.then47
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r15
.LBB5_15:                               # %if.end49
                                        #   in Loop: Header=BB5_1 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	48(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB5_20
# BB#16:                                # %if.then54
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB5_19
# BB#17:                                # %land.lhs.true58
                                        #   in Loop: Header=BB5_1 Depth=1
	movl	(%r14), %edx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	add_equal_note
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_18
.LBB5_19:                               # %if.end63
	movq	%rbx, %rdi
	callq	emit_insn
	jmp	.LBB5_97
.LBB5_21:                               # %if.end67.loopexit
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	mode_class(,%rax,4), %ebx
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB5_22
.LBB5_20:                               # %if.else65
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	mode_class(,%rax,4), %ebx
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
.LBB5_22:                               # %if.end67
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$6, %rsi
	ja	.LBB5_28
# BB#23:                                # %if.end67
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	$70, %rax
	je	.LBB5_28
# BB#24:                                # %for.cond.preheader
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	mode_wider_mode(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB5_28
# BB#25:
	movabsq	$0, %r13
	.align	16, 0x90
.LBB5_26:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	8(%r14,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB5_27
# BB#40:                                # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	cmpq	$1, %rsi
	movq	%r13, %rax
	jne	.LBB5_42
# BB#41:                                # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	movabsq	$1, %rax
.LBB5_42:                               # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	optab_table+256(%rip), %rcx
	cmpq	%r14, %rcx
	movq	%r13, %r8
	jne	.LBB5_44
# BB#43:                                # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	movabsq	$1, %r8
.LBB5_44:                               # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	%rsi, %r12
	movq	optab_table+288(%rip), %rcx
	cmpq	%r14, %rcx
	movq	%r13, %rcx
	jne	.LBB5_46
# BB#45:                                # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	movabsq	$1, %rcx
.LBB5_46:                               # %if.then83
                                        #   in Loop: Header=BB5_26 Depth=1
	orq	%rcx, %r8
	andq	%rax, %r8
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	widen_operand
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	movq	%r15, %r8
	callq	expand_unop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB5_47
# BB#53:                                # %if.else106
                                        #   in Loop: Header=BB5_26 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	%r12, %rsi
.LBB5_27:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_26 Depth=1
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB5_26
.LBB5_28:                               # %if.end112
	movq	optab_table+288(%rip), %rax
	cmpq	%r14, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	jne	.LBB5_54
# BB#29:                                # %if.end112
	cmpq	$1, %rsi
	jne	.LBB5_54
# BB#30:                                # %land.lhs.true118
	movl	target_flags(%rip), %eax
	movzbl	mode_size(%rdi), %edx
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	cmpq	%rcx, %rdx
	jbe	.LBB5_54
# BB#31:                                # %land.lhs.true125
	movslq	word_mode(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%r14,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB5_32
.LBB5_54:                               # %if.else166
	movl	(%r14), %eax
	cmpq	$77, %rax
	movq	48(%rsp), %rdx          # 8-byte Reload
	jne	.LBB5_68
# BB#55:                                # %if.else166
	addq	$-5, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	ja	.LBB5_68
# BB#56:                                # %if.then176
	movq	%r14, %r12
	cmpq	$5, %rsi
	jne	.LBB5_57
# BB#58:                                # %if.then176
	movabsq	$1, %rsi
	jmp	.LBB5_59
.LBB5_68:                               # %if.end217
	movq	$4, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	cmpq	$0, 16(%r14,%rdx)
	je	.LBB5_70
# BB#69:                                # %if.then222
	leaq	16(%r14,%rdx), %rbx
	callq	start_sequence
	movq	(%rbx), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, (%rsp)
	movq	%rbx, %rcx
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	%r13, %r9
	callq	emit_library_call_value
	movq	%rax, %r12
	callq	get_insns
	movq	%r14, %r15
	movq	%rax, %r14
	callq	end_sequence
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movl	(%r15), %edi
	movq	%rax, %r15
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	gen_rtx_fmt_e
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	emit_libcall_block
	jmp	.LBB5_97
.LBB5_57:
	movabsq	$0, %rsi
.LBB5_59:                               # %if.then176
	movzbl	mode_unit_size(%rdi), %edi
	movq	$3, %rcx
	shlq	%cl, %rdi
	xorq	$1, %rsi
	incq	%rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$51, %rax
	movq	64(%rsp), %r14          # 8-byte Reload
	je	.LBB5_98
# BB#60:                                # %if.end189
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	jne	.LBB5_62
# BB#61:                                # %if.then192
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB5_62:                               # %if.end194
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	start_sequence
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	gen_imagpart
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	gen_imagpart
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	16(%rsp), %r8           # 8-byte Reload
	callq	expand_unop
	cmpq	%rax, %r15
	je	.LBB5_64
# BB#63:                                # %if.then200
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB5_64:                               # %if.end202
	movq	%r13, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	gen_realpart
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	gen_realpart
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	16(%rsp), %r8           # 8-byte Reload
	callq	expand_unop
	cmpq	%rax, %r15
	je	.LBB5_66
# BB#65:                                # %if.then208
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB5_66:                               # %if.end210
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movl	(%r12), %r15d
	movq	%r14, %rdi
	callq	copy_rtx
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rdx
	jmp	.LBB5_67
.LBB5_70:                               # %if.end233
	cmpq	$6, %rsi
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %r13
	ja	.LBB5_78
# BB#71:                                # %if.end233
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	testq	$70, %rdx
	je	.LBB5_78
# BB#72:                                # %for.cond246.preheader
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	mode_wider_mode(%rcx), %r15d
	testq	%r15, %r15
	je	.LBB5_78
# BB#73:
	movabsq	$0, %r12
	.align	16, 0x90
.LBB5_74:                               # %for.body249
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %r15
	movq	$4, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movl	8(%r13,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB5_81
# BB#75:                                # %lor.lhs.false256
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	$4, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	cmpq	$0, 16(%r13,%rax)
	je	.LBB5_76
.LBB5_81:                               # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	cmpq	$1, %rsi
	movq	%r12, %rax
	jne	.LBB5_83
# BB#82:                                # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	movabsq	$1, %rax
.LBB5_83:                               # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	%rsi, %r14
	movq	optab_table+256(%rip), %rcx
	cmpq	%r13, %rcx
	movq	%r12, %r8
	jne	.LBB5_85
# BB#84:                                # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	movabsq	$1, %r8
.LBB5_85:                               # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	optab_table+288(%rip), %rcx
	cmpq	%r13, %rcx
	movq	%r12, %rcx
	jne	.LBB5_87
# BB#86:                                # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	movabsq	$1, %rcx
.LBB5_87:                               # %if.then262
                                        #   in Loop: Header=BB5_74 Depth=1
	orq	%rcx, %r8
	andq	%rax, %r8
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	widen_operand
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	%rbx, %r8
	callq	expand_unop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB5_88
# BB#93:                                # %if.else288
                                        #   in Loop: Header=BB5_74 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	%r14, %rsi
.LBB5_76:                               # %for.cond246.backedge
                                        #   in Loop: Header=BB5_74 Depth=1
	movzbl	mode_wider_mode(%r15), %r15d
	testq	%r15, %r15
	jne	.LBB5_74
# BB#77:                                # %for.cond246.if.end296.loopexit_crit_edge
	movl	(%r13), %eax
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB5_78:                               # %if.end296
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$77, %rax
	movq	24(%rsp), %r8           # 8-byte Reload
	jne	.LBB5_96
# BB#79:                                # %if.then300
	movq	optab_table+264(%rip), %rax
	cmpq	%r13, %rax
	je	.LBB5_80
# BB#94:                                # %select.mid
	movq	optab_table+16(%rip), %rsi
	jmp	.LBB5_95
.LBB5_32:                               # %if.then132
	movq	%rdi, %r15
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB5_34
# BB#33:                                # %if.then132
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	jne	.LBB5_35
.LBB5_34:                               # %if.then138
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB5_35:                               # %if.end140
	callq	start_sequence
	movzbl	mode_bitsize(%r15,%r15), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	xorq	%r14, %r14
	xorq	%rdx, %rdx
	idivq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB5_39
	.align	16, 0x90
.LBB5_36:                               # %for.body150
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %r15
	movq	%r14, %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	operand_subword
	movl	word_mode(%rip), %r13d
	movq	%rax, %r12
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	operand_subword_force
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r12, %rcx
	movq	16(%rsp), %r8           # 8-byte Reload
	callq	expand_unop
	cmpq	%rax, %r12
	je	.LBB5_38
# BB#37:                                # %if.then156
                                        #   in Loop: Header=BB5_36 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB5_38:                               # %for.inc159
                                        #   in Loop: Header=BB5_36 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	xorq	%rdx, %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	idivq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rsi
	movq	%r15, %rbx
	jl	.LBB5_36
.LBB5_39:                               # %for.end160
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %r15d
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	callq	copy_rtx
	movq	%r15, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
.LBB5_67:                               # %return
	movq	%rax, %r8
	callq	emit_no_conflict_block
	movq	%rbx, %r15
.LBB5_97:                               # %return
	movq	%r15, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_47:                               # %if.then95
	cmpq	$1, %r12
	jne	.LBB5_48
# BB#51:                                # %if.else104
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
.LBB5_52:                               # %if.else104
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB5_48:                               # %if.then98
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	jne	.LBB5_50
# BB#49:                                # %if.then101
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB5_50:                               # %if.end103
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert_move
	movq	%rbx, %r15
	jmp	.LBB5_97
.LBB5_80:
	movq	optab_table+24(%rip), %rsi
.LBB5_95:                               # %select.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	const_tiny_rtx(,%rax,8), %rdx
	movq	$3, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rcx
	movq	16(%rsp), %r9           # 8-byte Reload
	callq	expand_binop
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB5_97
.LBB5_96:                               # %if.end311
	xorq	%r15, %r15
	jmp	.LBB5_97
.LBB5_88:                               # %if.then277
	cmpq	$1, %r14
	jne	.LBB5_89
# BB#92:                                # %if.else286
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB5_52
.LBB5_89:                               # %if.then280
	movq	24(%rsp), %r14          # 8-byte Reload
	testq	%r14, %r14
	jne	.LBB5_91
# BB#90:                                # %if.then283
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB5_91:                               # %if.end285
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert_move
	movq	%r14, %r15
	jmp	.LBB5_97
.LBB5_98:                               # %if.then188
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_unop, %rcx
	movq	$2227, %rsi             # imm = 0x8B3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp85:
	.size	expand_unop, .Ltmp85-expand_unop
	.cfi_endproc

	.globl	emit_libcall_block
	.align	16, 0x90
	.type	emit_libcall_block,@function
emit_libcall_block:                     # @emit_libcall_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp92:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp93:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp94:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp95:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp96:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp97:
	.cfi_def_cfa_offset 80
.Ltmp98:
	.cfi_offset %rbx, -48
.Ltmp99:
	.cfi_offset %r12, -40
.Ltmp100:
	.cfi_offset %r13, -32
.Ltmp101:
	.cfi_offset %r14, -24
.Ltmp102:
	.cfi_offset %r15, -16
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movl	(%rsi), %eax
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r15
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	cmpq	$61, %rcx
	movq	%rsi, %rbx
	je	.LBB6_2
# BB#1:                                 # %if.then
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%rax, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB6_2:                                # %if.end
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_3
# BB#8:                                 # %land.lhs.true
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_3
# BB#9:                                 # %for.cond.preheader
	testq	%r15, %r15
	je	.LBB6_60
# BB#10:
	movq	%rbx, %r12
	movq	%r15, %rbx
	.align	16, 0x90
.LBB6_11:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB6_15
# BB#12:                                # %if.then14
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB6_15
# BB#13:                                # %land.lhs.true17
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	8(%rax), %rcx
	cmpq	$0, 8(%rcx)
	jg	.LBB6_15
# BB#14:                                # %if.then21
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	remove_note
	.align	16, 0x90
.LBB6_15:                               # %for.inc
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_11
	jmp	.LBB6_16
.LBB6_3:                                # %for.cond27.preheader
	testq	%r15, %r15
	je	.LBB6_60
# BB#4:
	movq	%rbx, %r12
	movq	%r15, %rbx
	.align	16, 0x90
.LBB6_5:                                # %for.body29
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB6_35
# BB#6:                                 # %if.then33
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	$23, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	$-1, %rsi
	callq	gen_rtx_CONST_INT
	testq	%r14, %r14
	jne	.LBB6_7
# BB#34:                                # %if.else42
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	jmp	.LBB6_35
	.align	16, 0x90
.LBB6_7:                                # %if.then37
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	%rax, 8(%r14)
.LBB6_35:                               # %for.inc53
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_5
.LBB6_16:                               # %for.cond59.preheader
	testq	%r15, %r15
	movq	%r12, %rbx
	je	.LBB6_60
# BB#17:
	movq	%rbx, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r15, %rbx
	.align	16, 0x90
.LBB6_18:                               # %for.body61
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	movabsq	$0, %r14
	jne	.LBB6_21
# BB#19:                                # %cond.true
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	32(%r12), %r14
	movzbl	(%r14), %eax
	cmpq	$47, %rax
	je	.LBB6_21
# BB#20:                                # %cond.false
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	single_set_2
	movq	%rax, %r14
.LBB6_21:                               # %cond.end83
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	$7, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB6_23
# BB#22:                                # %if.then89
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB6_23:                               # %if.end90
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB6_25
# BB#24:                                # %if.then94
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	remove_note
.LBB6_25:                               # %if.end95
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	24(%r12), %rbx
	testq	%r14, %r14
	je	.LBB6_44
# BB#26:                                # %land.lhs.true101
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB6_44
# BB#27:                                # %land.lhs.true109
                                        #   in Loop: Header=BB6_18 Depth=1
	movl	8(%rdi), %eax
	cmpq	$53, %rax
	jb	.LBB6_44
# BB#28:                                # %land.lhs.true117
                                        #   in Loop: Header=BB6_18 Depth=1
	cmpq	%r15, %r12
	je	.LBB6_39
# BB#29:                                # %lor.lhs.false120
                                        #   in Loop: Header=BB6_18 Depth=1
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB6_32
# BB#30:                                # %lor.lhs.false128
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	32(%r15), %rsi
	callq	reg_mentioned_p
	testq	%r13, %rax
	movq	%r13, %rax
                                        # kill: R13<def> RAX<kill>
	jne	.LBB6_44
# BB#31:                                # %lor.lhs.false128.land.lhs.true137_crit_edge
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	8(%r14), %rdi
.LBB6_32:                               # %land.lhs.true137
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	reg_used_between_p
	testq	%r13, %rax
	movq	%r13, %rax
	jne	.LBB6_33
# BB#36:                                # %land.lhs.true143
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	16(%r14), %rdi
	movq	%r15, %rsi
	movq	%rax, %r13
	callq	modified_in_p
	testq	%r13, %rax
	movq	%r13, %rax
	jne	.LBB6_37
# BB#38:                                # %land.lhs.true149
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	16(%r14), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rax, %r14
	callq	modified_between_p
	testq	%r14, %rax
	movq	%r14, %r13
	jne	.LBB6_44
.LBB6_39:                               # %if.then155
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	16(%r12), %rax
	testq	%rax, %rax
	movq	%rbx, %r14
	je	.LBB6_41
# BB#40:                                # %if.then160
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%rbx, 24(%rax)
	movq	%r15, %r14
.LBB6_41:                               # %if.end168
                                        #   in Loop: Header=BB6_18 Depth=1
	testq	%rbx, %rbx
	je	.LBB6_43
# BB#42:                                # %if.then170
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	16(%r12), %rax
	movq	%rax, 16(%rbx)
.LBB6_43:                               # %if.end177
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r12, %rdi
	callq	add_insn
	movq	%r14, %r15
	jmp	.LBB6_44
.LBB6_33:                               #   in Loop: Header=BB6_18 Depth=1
	movq	%rax, %r13
	jmp	.LBB6_44
.LBB6_37:                               #   in Loop: Header=BB6_18 Depth=1
	movq	%rax, %r13
	.align	16, 0x90
.LBB6_44:                               # %for.cond59.backedge
                                        #   in Loop: Header=BB6_18 Depth=1
	testq	%rbx, %rbx
	jne	.LBB6_18
# BB#45:                                # %for.end180
	callq	get_last_insn
	movq	%rax, %r14
	testq	%r15, %r15
	je	.LBB6_47
	.align	16, 0x90
.LBB6_46:                               # %for.body184
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r15), %rbx
	movq	%r15, %rdi
	callq	add_insn
	testq	%rbx, %rbx
	movq	%rbx, %r15
	jne	.LBB6_46
.LBB6_47:
	movq	(%rsp), %rbx            # 8-byte Reload
	jmp	.LBB6_48
.LBB6_60:                               # %for.end180.thread
	callq	get_last_insn
	movq	%rax, %r14
.LBB6_48:                               # %for.end189
	movq	%rbx, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	emit_move_insn
	movzbl	2(%rbx), %edx
	movq	optab_table+240(%rip), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	8(%rsi,%rdx), %ecx
	movq	%rax, %r15
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB6_49
# BB#50:                                # %if.else201
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	find_reg_note
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	remove_note
	jmp	.LBB6_51
.LBB6_49:                               # %if.then198
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	copy_rtx
	movq	$4, %rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
.LBB6_51:                               # %if.end203
	movq	16(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %rbx
	je	.LBB6_53
# BB#52:                                # %if.then206
	movq	%rbx, %rsi
	callq	emit_move_insn
.LBB6_53:                               # %if.end208
	testq	%r14, %r14
	je	.LBB6_54
# BB#55:                                # %if.else213
	movq	24(%r14), %rbx
	jmp	.LBB6_56
.LBB6_54:                               # %if.then211
	callq	get_insns
	movq	%rax, %rbx
.LBB6_56:                               # %if.end217
	movl	flag_non_call_exceptions(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_58
# BB#57:                                # %lor.lhs.false219
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	may_trap_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_59
.LBB6_58:                               # %if.then222
	movq	56(%rbx), %rcx
	movq	$4, %rdi
	movq	$7, %rsi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%rbx)
	movq	56(%r15), %rcx
	movq	$4, %rdi
	movq	$6, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r15)
.LBB6_59:                               # %if.end237
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp103:
	.size	emit_libcall_block, .Ltmp103-emit_libcall_block
	.cfi_endproc

	.globl	sign_expand_binop
	.align	16, 0x90
	.type	sign_expand_binop,@function
sign_expand_binop:                      # @sign_expand_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp112:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp113:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp114:
	.cfi_def_cfa_offset 48
	subq	$992, %rsp              # imm = 0x3E0
.Ltmp115:
	.cfi_def_cfa_offset 1040
.Ltmp116:
	.cfi_offset %rbx, -48
.Ltmp117:
	.cfi_offset %r12, -40
.Ltmp118:
	.cfi_offset %r13, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movq	1040(%rsp), %r10
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r10
	movq	%rsi, %r13
	jne	.LBB7_2
# BB#1:                                 # %entry
	movq	%r15, %r13
.LBB7_2:                                # %entry
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%r10, %r9
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB7_17
# BB#3:                                 # %entry
	movq	1048(%rsp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB7_17
# BB#4:                                 # %if.end
	leaq	40(%rsp), %rdi
	movq	$952, %rdx              # imm = 0x3B8
	movq	%rdi, %r12
	movq	%r15, %rsi
	callq	memcpy
	movq	$32, %rcx
	movq	%rbx, %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$1317, %rcx             # imm = 0x525
	movl	%ecx, 48(%rsp,%rax)
	movq	$0, 56(%rsp,%rax)
	movq	$2, (%rsp)
	movq	%r12, %rsi
	movq	%r14, %r15
	movq	%r15, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	1040(%rsp), %rbx
	movq	%rbx, %r9
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB7_7
# BB#5:                                 # %if.end
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	je	.LBB7_7
# BB#6:                                 # %if.then8
	movq	$2, (%rsp)
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	callq	expand_binop
.LBB7_7:                                # %if.end10
	testq	%rax, %rax
	jne	.LBB7_17
# BB#8:                                 # %if.end10
	movq	%r15, %rdx
	movq	%rbx, %r15
	movq	1048(%rsp), %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	cmpq	$2, %rbx
	je	.LBB7_17
# BB#9:                                 # %if.end15
	movq	$1, (%rsp)
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rdx, %r12
                                        # kill: RDX<def> R12<kill>
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%r15, %r9
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB7_17
# BB#10:                                # %if.end15
	cmpq	$1, %rbx
	je	.LBB7_17
# BB#11:                                # %if.end21
	leaq	40(%rsp), %rsi
	movq	1048(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%r15, %rbx
	movq	%rbx, %r9
	callq	expand_binop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r14, %rdi
	movq	%r12, %rdx
	movq	%rbx, %r9
	testq	%rcx, %rbx
	jne	.LBB7_12
# BB#13:                                # %if.end21
	movabsq	$1, %rcx
	jmp	.LBB7_14
.LBB7_12:
	movabsq	$0, %rcx
.LBB7_14:                               # %if.end21
	testq	%rax, %rax
	jne	.LBB7_17
# BB#15:                                # %if.end21
	testq	%rcx, %rcx
	jne	.LBB7_17
# BB#16:                                # %if.then27
	movq	1048(%rsp), %rax
	movq	%rax, (%rsp)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rsp), %r8           # 8-byte Reload
	callq	expand_binop
.LBB7_17:                               # %cleanup
	addq	$992, %rsp              # imm = 0x3E0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	sign_expand_binop, .Ltmp121-sign_expand_binop
	.cfi_endproc

	.globl	expand_twoval_binop
	.align	16, 0x90
	.type	expand_twoval_binop,@function
expand_twoval_binop:                    # @expand_twoval_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp128:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp129:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp130:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp131:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp132:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp133:
	.cfi_def_cfa_offset 160
.Ltmp134:
	.cfi_offset %rbx, -48
.Ltmp135:
	.cfi_offset %r12, -40
.Ltmp136:
	.cfi_offset %r13, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	%r15, 64(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	movq	%r14, %rax
	jne	.LBB8_2
# BB#1:                                 # %entry
	movq	%r8, %rax
.LBB8_2:                                # %entry
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movzbl	2(%rax), %r13d
	callq	get_last_insn
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB8_3
# BB#4:                                 # %if.then
	movq	%rbx, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	force_not_mem
	movq	%rax, 104(%rsp)         # 8-byte Spill
	jmp	.LBB8_5
.LBB8_3:
	movq	%r12, 104(%rsp)         # 8-byte Spill
.LBB8_5:                                # %if.end
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movzbl	(%rbx), %eax
	movq	%r13, %r12
	movl	mode_class(,%r12,4), %r13d
	cmpq	$133, %rax
	jg	.LBB8_8
# BB#6:                                 # %if.end
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB8_13
# BB#7:                                 # %if.end
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB8_10
	jmp	.LBB8_13
.LBB8_8:                                # %if.end
	cmpq	$134, %rax
	je	.LBB8_10
# BB#9:                                 # %if.end
	cmpq	$140, %rax
	jne	.LBB8_13
.LBB8_10:                               # %land.lhs.true
	callq	preserve_subexpressions_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_13
# BB#11:                                # %land.lhs.true37
	movl	(%r15), %esi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB8_13
# BB#12:                                # %if.then40
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 96(%rsp)          # 8-byte Spill
.LBB8_13:                               # %if.end42
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$133, %rax
	jg	.LBB8_16
# BB#14:                                # %if.end42
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB8_21
# BB#15:                                # %if.end42
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB8_18
	jmp	.LBB8_21
.LBB8_16:                               # %if.end42
	cmpq	$134, %rax
	je	.LBB8_18
# BB#17:                                # %if.end42
	cmpq	$140, %rax
	jne	.LBB8_21
.LBB8_18:                               # %land.lhs.true74
	callq	preserve_subexpressions_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_21
# BB#19:                                # %land.lhs.true77
	movl	(%r15), %esi
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB8_21
# BB#20:                                # %if.then81
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 104(%rsp)         # 8-byte Spill
.LBB8_21:                               # %if.end83
	testq	%r14, %r14
	je	.LBB8_23
# BB#22:                                # %if.then85
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	jmp	.LBB8_24
.LBB8_23:                               # %if.else
	movq	%r12, %rdi
	callq	gen_reg_rtx
.LBB8_24:                               # %if.end88
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB8_26
# BB#25:                                # %if.then90
	movq	$1, %rsi
	callq	protect_from_queue
	jmp	.LBB8_27
.LBB8_26:                               # %if.else92
	movq	%r12, %rdi
	callq	gen_reg_rtx
.LBB8_27:                               # %if.end94
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	$4, %rcx
	movq	%r12, %rdx
	movq	%r12, 88(%rsp)          # 8-byte Spill
	shlq	%cl, %rdx
	movslq	8(%r15,%rdx), %rcx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB8_46
# BB#28:                                # %if.then99
	movq	%r13, 8(%rsp)           # 8-byte Spill
	leaq	(%rcx,%rcx,4), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %r15
	movzbl	40(%r15), %r12d
	movq	96(%rsp), %rsi          # 8-byte Reload
	movzbl	2(%rsi), %eax
	movzbl	64(%r15), %ecx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%rsi, %rbx
	je	.LBB8_31
# BB#29:                                # %if.then99
	cmpq	%r12, %rax
	movq	%rsi, %rbx
	je	.LBB8_31
# BB#30:                                # %if.then125
	movq	%r12, %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	convert_to_mode
	movq	%rax, %rbx
.LBB8_31:                               # %if.end127
	movq	104(%rsp), %rsi         # 8-byte Reload
	movzbl	2(%rsi), %eax
	testq	%rax, %rax
	movq	%rsi, %r13
	je	.LBB8_34
# BB#32:                                # %if.end127
	movq	80(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rsi, %r13
	je	.LBB8_34
# BB#33:                                # %if.then137
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	convert_to_mode
	movq	%rax, %r13
.LBB8_34:                               # %if.end139
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	*24(%r15)
	movq	%rbx, %rsi
	movq	%r12, %rdi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB8_36
# BB#35:
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	jmp	.LBB8_37
.LBB8_36:                               # %if.then146
	callq	copy_to_mode_reg
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB8_37:                               # %if.end148
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	*48(%r15)
	testq	%r12, %rax
	jne	.LBB8_39
# BB#38:                                # %if.then156
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r13
.LBB8_39:                               # %if.end158
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	*(%r15)
	testq	%r12, %rax
	movq	32(%rsp), %rbx          # 8-byte Reload
	je	.LBB8_56
# BB#40:                                # %lor.lhs.false166
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	*72(%r15)
	testq	%r12, %rax
	je	.LBB8_56
# BB#41:                                # %if.end175
	xorq	%rax, %rax
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	%rbx, %rcx
	movq	24(%rsp), %rbx          # 8-byte Reload
	callq	*insn_data+16(,%rbx,8)
	testq	%rax, %rax
	je	.LBB8_45
# BB#42:                                # %if.then180
	movq	%rax, %rdi
	callq	emit_insn
.LBB8_43:                               # %return
	movq	$1, %rax
	jmp	.LBB8_44
.LBB8_45:                               # %if.else182
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	8(%rsp), %r13           # 8-byte Reload
.LBB8_46:                               # %if.end184
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	cmpq	$6, %r13
	ja	.LBB8_52
# BB#47:                                # %if.end184
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	$70, %rax
	je	.LBB8_52
# BB#48:                                # %for.cond.preheader
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	mode_wider_mode(%rax), %ebx
	jmp	.LBB8_49
	.align	16, 0x90
.LBB8_51:                               # %for.cond.backedge
                                        #   in Loop: Header=BB8_49 Depth=1
	movzbl	mode_wider_mode(%rbx), %ebx
.LBB8_49:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB8_52
# BB#50:                                # %for.body
                                        #   in Loop: Header=BB8_49 Depth=1
	andq	$255, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	8(%r15,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB8_51
# BB#53:                                # %if.then201
                                        #   in Loop: Header=BB8_49 Depth=1
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	convert_modes
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	convert_modes
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %r13
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r12, %rcx
	movq	%r14, %r8
	movq	%r15, %r9
	callq	expand_twoval_binop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_54
# BB#55:                                # %if.else209
                                        #   in Loop: Header=BB8_49 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	%r13, %r15
	movzbl	mode_wider_mode(%rbx), %ebx
	jmp	.LBB8_49
.LBB8_52:                               # %if.end215
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	xorq	%rax, %rax
.LBB8_44:                               # %return
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_54:                               # %if.then208
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	callq	convert_move
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	convert_move
	jmp	.LBB8_43
.LBB8_56:                               # %if.then174
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_twoval_binop, %rcx
	movq	$1998, %rsi             # imm = 0x7CE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp139:
	.size	expand_twoval_binop, .Ltmp139-expand_twoval_binop
	.cfi_endproc

	.globl	expand_simple_unop
	.align	16, 0x90
	.type	expand_simple_unop,@function
expand_simple_unop:                     # @expand_simple_unop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp141:
	.cfi_def_cfa_offset 16
	movq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	code_to_optab(,%rsi,8), %rsi
	testq	%rsi, %rsi
	je	.LBB9_1
# BB#2:                                 # %if.end
	movq	%rax, %rcx
	popq	%rax
	jmp	expand_unop  # TAILCALL
.LBB9_1:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_simple_unop, %rcx
	movq	$2055, %rsi             # imm = 0x807
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp142:
	.size	expand_simple_unop, .Ltmp142-expand_simple_unop
	.cfi_endproc

	.globl	expand_abs
	.align	16, 0x90
	.type	expand_abs,@function
expand_abs:                             # @expand_abs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp149:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp150:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp151:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp152:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp154:
	.cfi_def_cfa_offset 80
.Ltmp155:
	.cfi_offset %rbx, -48
.Ltmp156:
	.cfi_offset %r12, -40
.Ltmp157:
	.cfi_offset %r13, -32
.Ltmp158:
	.cfi_offset %r14, -24
.Ltmp159:
	.cfi_offset %r15, -16
	movl	flag_trapv(%rip), %eax
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %r12
	jne	.LBB10_2
# BB#1:                                 # %entry
	movq	%rdx, %r12
.LBB10_2:                               # %entry
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rcx
	je	.LBB10_4
# BB#3:                                 # %entry
	movq	%rdx, %rax
.LBB10_4:                               # %entry
	orq	%rax, %r12
	jne	.LBB10_5
# BB#6:                                 # %entry
	movabsq	$optab_table+280, %rax
	jmp	.LBB10_7
.LBB10_5:
	movabsq	$optab_table+272, %rax
.LBB10_7:                               # %entry
	movq	(%rax), %rsi
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	expand_unop
	testq	%rax, %rax
	jne	.LBB10_45
# BB#8:                                 # %if.end3
	movq	$32, %rcx
	movq	%r14, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	movq	optab_table+216(%rip), %rax
	sarq	%cl, %r13
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB10_12
# BB#9:                                 # %if.then5
	callq	get_last_insn
	movq	optab_table+256(%rip), %rsi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	expand_unop
	testq	%rax, %rax
	je	.LBB10_11
# BB#10:                                # %if.end11
	movq	optab_table+216(%rip), %rsi
	xorq	%r9, %r9
	movq	$2, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB10_45
.LBB10_11:                              # %if.end14
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
.LBB10_12:                              # %if.end15
	movl	mode_class(,%r13,4), %eax
	cmpq	$1, %rax
	jne	.LBB10_19
# BB#13:                                # %if.end15
	movl	ix86_branch_cost(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB10_19
# BB#14:                                # %if.then20
	movzbl	mode_bitsize(%r13,%r13), %edi
	decq	%rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$83, %rdi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	expand_shift
	movq	optab_table+160(%rip), %rsi
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%rax, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	testq	%rax, %rax
	je	.LBB10_19
# BB#15:                                # %if.end36
	testq	$1, %r12
	jne	.LBB10_16
# BB#17:                                # %if.end36
	movabsq	$optab_table+24, %rcx
	jmp	.LBB10_18
.LBB10_16:
	movabsq	$optab_table+16, %rcx
.LBB10_18:                              # %if.end36
	movq	(%rcx), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%rax, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %r8
	callq	expand_binop
	testq	%rax, %rax
	jne	.LBB10_45
.LBB10_19:                              # %if.end41
	cmpq	%rbx, %r15
	jne	.LBB10_24
# BB#20:                                # %land.lhs.true44
	movzbl	(%r15), %eax
	cmpq	$61, %rax
	jne	.LBB10_24
# BB#21:                                # %land.lhs.true47
	movl	8(%r15), %eax
	cmpq	$52, %rax
	movq	$1, %rax
	ja	.LBB10_23
# BB#22:                                # %select.mid
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB10_23:                              # %select.end
	movq	%rax, 24(%rsp)          # 8-byte Spill
.LBB10_24:                              # %if.end52
	callq	gen_label_rtx
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB10_30
# BB#25:                                # %if.end52
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rsi, %rax
	je	.LBB10_30
# BB#26:                                # %lor.lhs.false57
	movl	(%rbx), %eax
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%r14, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB10_30
# BB#27:                                # %lor.lhs.false57
	movq	%rax, %rcx
	andq	$134283263, %rcx        # imm = 0x800FFFF
	andq	%rsi, %rcx
	cmpq	$134217794, %rcx        # imm = 0x8000042
	je	.LBB10_30
# BB#28:                                # %lor.lhs.false72
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB10_31
# BB#29:                                # %land.lhs.true77
	movl	8(%rbx), %eax
	cmpq	$52, %rax
	ja	.LBB10_31
.LBB10_30:                              # %if.then83
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB10_31:                              # %if.end85
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	mode_class(,%r13,4), %edx
	cmpq	$1, %rdx
	movl	%ecx, 4(%rax)
	jne	.LBB10_38
# BB#32:                                # %land.lhs.true91
	movq	optab_table+328(%rip), %rax
	movq	optab_table+360(%rip), %rdx
	movq	%r14, %rsi
	.align	16, 0x90
.LBB10_33:                              # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rax,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB10_36
# BB#34:                                # %if.end.i
                                        #   in Loop: Header=BB10_33 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rdx,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB10_38
# BB#35:                                # %if.end45.i
                                        #   in Loop: Header=BB10_33 Depth=1
	movzbl	mode_wider_mode(%rsi), %esi
	testq	%rsi, %rsi
	jne	.LBB10_33
	jmp	.LBB10_37
.LBB10_36:                              # %can_compare_p.exit
	cmpq	$0, bcc_gen_fctn+832(%rip)
	je	.LBB10_37
.LBB10_38:                              # %if.else
	movq	const_tiny_rtx(,%r13,8), %rsi
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, 8(%rsp)
	movq	$104, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %r8
	callq	do_compare_rtx_and_jump
	jmp	.LBB10_39
.LBB10_37:                              # %if.then94
	movq	const_int_rtx+512(%rip), %rcx
	xorq	%rsi, %rsi
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r9
	callq	do_jump_by_parts_greater_rtx
.LBB10_39:                              # %if.end97
	testq	$1, %r12
	jne	.LBB10_40
# BB#41:                                # %if.end97
	movabsq	$optab_table+264, %rax
	jmp	.LBB10_42
.LBB10_40:
	movabsq	$optab_table+256, %rax
.LBB10_42:                              # %if.end97
	movq	(%rax), %rsi
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	%rbx, %rcx
	callq	expand_unop
	cmpq	%rbx, %rax
	je	.LBB10_44
# BB#43:                                # %if.then106
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB10_44:                              # %if.end108
	movq	%r15, %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movl	%ecx, 4(%rax)
	movq	%rbx, %rax
.LBB10_45:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp160:
	.size	expand_abs, .Ltmp160-expand_abs
	.cfi_endproc

	.globl	can_compare_p
	.align	16, 0x90
	.type	can_compare_p,@function
can_compare_p:                          # @can_compare_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp163:
	.cfi_def_cfa_offset 16
.Ltmp164:
	.cfi_offset %rbx, -16
	movq	optab_table+328(%rip), %r9
	movq	optab_table+360(%rip), %r10
	movq	optab_table+368(%rip), %r11
	movq	optab_table+376(%rip), %rbx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB11_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	8(%r9,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB11_2
# BB#10:                                # %if.end
                                        #   in Loop: Header=BB11_1 Depth=1
	testq	%r8, %rdx
	jne	.LBB11_12
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	8(%r10,%rax), %eax
	jmp	.LBB11_16
	.align	16, 0x90
.LBB11_12:                              # %if.end23
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	%rdx, %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	jne	.LBB11_13
# BB#15:                                # %land.lhs.true37
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	8(%rbx,%rax), %eax
	jmp	.LBB11_16
	.align	16, 0x90
.LBB11_13:                              # %if.end23
                                        #   in Loop: Header=BB11_1 Depth=1
	cmpq	$1, %rax
	jne	.LBB11_17
# BB#14:                                # %land.lhs.true26
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	8(%r11,%rax), %eax
	.align	16, 0x90
.LBB11_16:                              # %land.lhs.true37
                                        #   in Loop: Header=BB11_1 Depth=1
	cmpq	$1317, %rax             # imm = 0x525
	movq	$1, %rax
	jne	.LBB11_18
.LBB11_17:                              # %if.end45
                                        #   in Loop: Header=BB11_1 Depth=1
	movzbl	mode_wider_mode(%rsi), %esi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	jne	.LBB11_1
	jmp	.LBB11_18
.LBB11_2:                               # %if.then
	testq	%r8, %rdx
	je	.LBB11_3
# BB#6:                                 # %if.else
	andq	%r8, %rdx
	movq	$1, %rax
	cmpq	$2, %rdx
	jne	.LBB11_18
# BB#7:                                 # %if.then8
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	setcc_gen_code(,%rdi,4), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB11_8
# BB#9:                                 # %if.then8
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB11_18:                              # %return
	popq	%rbx
	retq
.LBB11_3:                               # %if.then2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$0, bcc_gen_fctn(,%rdi,8)
	je	.LBB11_4
# BB#5:                                 # %if.then2
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB11_4:
	movabsq	$0, %rax
	popq	%rbx
	retq
.LBB11_8:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp165:
	.size	can_compare_p, .Ltmp165-can_compare_p
	.cfi_endproc

	.globl	expand_complex_abs
	.align	16, 0x90
	.type	expand_complex_abs,@function
expand_complex_abs:                     # @expand_complex_abs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp172:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp173:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp174:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp177:
	.cfi_def_cfa_offset 144
.Ltmp178:
	.cfi_offset %rbx, -48
.Ltmp179:
	.cfi_offset %r12, -40
.Ltmp180:
	.cfi_offset %r13, -32
.Ltmp181:
	.cfi_offset %r14, -24
.Ltmp182:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	mode_class(,%rbx,4), %r15d
	movq	%rdx, %r14
	movq	%rsi, %r13
	callq	get_last_insn
	movzbl	mode_unit_size(%rbx), %edi
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rdi
	cmpq	$5, %r15
	jne	.LBB12_1
# BB#2:                                 # %entry
	movabsq	$1, %rsi
	jmp	.LBB12_3
.LBB12_1:
	movabsq	$0, %rsi
.LBB12_3:                               # %entry
	xorq	$1, %rsi
	incq	%rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$51, %rax
	je	.LBB12_64
# BB#4:                                 # %if.end
	xorq	%rsi, %rsi
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r13, %rdi
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB12_6
# BB#5:                                 # %if.then8
	movq	%rax, %rdi
	callq	force_not_mem
.LBB12_6:                               # %if.end10
	movq	%rax, 88(%rsp)          # 8-byte Spill
	callq	get_last_insn
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	je	.LBB12_8
# BB#7:                                 # %if.then13
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB12_8:                               # %if.end15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB12_10
# BB#9:                                 # %if.end15
	movq	%rdx, %rcx
.LBB12_10:                              # %if.end15
	movl	flag_trapv(%rip), %esi
	testq	%rsi, %rsi
	movq	%rax, %rsi
	je	.LBB12_12
# BB#11:                                # %if.end15
	movq	%rdx, %rsi
.LBB12_12:                              # %if.end15
	andq	%rsi, %rcx
	cmpq	$1, %r15
	jne	.LBB12_14
# BB#13:                                # %if.end15
	movq	%rdx, %rax
.LBB12_14:                              # %if.end15
	testq	%rax, %rcx
	jne	.LBB12_15
# BB#16:                                # %if.end15
	movabsq	$optab_table+272, %rax
	jmp	.LBB12_17
.LBB12_15:
	movabsq	$optab_table+280, %rax
.LBB12_17:                              # %if.end15
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	(%rax), %r15
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movslq	8(%r15,%rax), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB12_32
# BB#18:                                # %if.then28
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movzbl	40(%rax), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %r13
	jne	.LBB12_20
# BB#19:                                # %if.else
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB12_20:                              # %if.end40
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rcx), %eax
	testq	%rax, %rax
	movq	%rcx, %r14
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB12_23
# BB#21:                                # %if.end40
	cmpq	%rbx, %rax
	movq	%rcx, %r14
	je	.LBB12_23
# BB#22:                                # %if.then50
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%rcx, %rbx
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	convert_to_mode
	movq	%rbx, %rcx
	movq	%r14, %rbx
	movq	%rax, %r14
.LBB12_23:                              # %if.end52
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	40(%rsp), %rax          # 8-byte Reload
	callq	*24(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_25
# BB#24:                                # %if.then59
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r14
.LBB12_25:                              # %if.end61
	movq	%r13, %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	40(%rsp), %rax          # 8-byte Reload
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_27
# BB#26:                                # %if.then69
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r13
.LBB12_27:                              # %if.end71
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB12_31
# BB#28:                                # %if.then76
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	jne	.LBB12_30
# BB#29:                                # %land.lhs.true81
	movl	(%r15), %edx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rcx
	callq	add_equal_note
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_65
.LBB12_30:                              # %if.end86
	movq	%rbx, %rdi
	callq	emit_insn
	jmp	.LBB12_63
.LBB12_31:                              # %if.else88
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	movq	48(%rsp), %rbx          # 8-byte Reload
.LBB12_32:                              # %if.end90
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movzbl	mode_wider_mode(%rbx), %r14d
	testq	%r14, %r14
	je	.LBB12_43
# BB#33:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB12_34:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	8(%r15,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB12_42
# BB#35:                                # %if.then102
                                        #   in Loop: Header=BB12_34 Depth=1
	movq	%rbx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	convert_modes
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r12, %rcx
	callq	expand_complex_abs
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB12_36
# BB#41:                                # %if.else118
                                        #   in Loop: Header=BB12_34 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
.LBB12_42:                              # %for.inc
                                        #   in Loop: Header=BB12_34 Depth=1
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	jne	.LBB12_34
.LBB12_43:                              # %for.end
	movq	$32, %rcx
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	optab_table+304(%rip), %rdx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB12_44
# BB#45:                                # %for.end
	movl	flag_trapv(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_46
# BB#47:                                # %if.then132
	movq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	gen_realpart
	movq	%rbx, %r13
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	gen_imagpart
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %rdx
	callq	expand_mult
	movq	%rax, %rbx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	callq	expand_mult
	movq	optab_table(%rip), %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r13, %rdi
	movq	%rbx, %rdx
	movq	%r13, %rbx
	movq	%rax, %rcx
	callq	expand_binop
	movq	optab_table+304(%rip), %rsi
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%rax, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	expand_unop
	movq	%rax, %r13
	testq	%r13, %r13
	jne	.LBB12_63
# BB#48:                                # %if.then141
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%rbx, %r13
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	jmp	.LBB12_49
.LBB12_36:                              # %if.then107
	cmpq	$5, 32(%rsp)            # 8-byte Folded Reload
	movq	64(%rsp), %rbx          # 8-byte Reload
	jne	.LBB12_37
# BB#40:                                # %if.else116
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
.LBB12_59:                              # %if.else192
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_lowpart  # TAILCALL
.LBB12_44:
	movq	%rbx, %r13
	jmp	.LBB12_49
.LBB12_46:
	movq	%rbx, %r13
.LBB12_49:                              # %if.end144
	movq	$4, %rcx
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rax
	shlq	%cl, %rax
	cmpq	$0, 16(%r15,%rax)
	je	.LBB12_50
# BB#66:                                # %if.then149
	callq	start_sequence
	movq	optab_table+272(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %rbx
	movq	16(%rax,%rbx), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	%r14, (%rsp)
	movq	%r13, %rbx
	movq	%rbx, %rcx
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	callq	emit_library_call_value
	movq	%rax, 72(%rsp)          # 8-byte Spill
	callq	get_insns
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	end_sequence
	movq	%rbx, %rdi
	callq	gen_reg_rtx
	movl	(%r15), %edi
	movq	%rax, %r13
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	emit_libcall_block
	jmp	.LBB12_63
	.align	16, 0x90
.LBB12_61:                              # %for.inc197
                                        #   in Loop: Header=BB12_50 Depth=1
	movzbl	mode_wider_mode(%r14), %r14d
.LBB12_50:                              # %for.cond163.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB12_62
# BB#51:                                # %for.body166
                                        #   in Loop: Header=BB12_50 Depth=1
	andq	$255, %r14
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movl	8(%r15,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB12_53
# BB#52:                                # %lor.lhs.false
                                        #   in Loop: Header=BB12_50 Depth=1
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	cmpq	$0, 16(%r15,%rax)
	je	.LBB12_61
.LBB12_53:                              # %if.then178
                                        #   in Loop: Header=BB12_50 Depth=1
	movq	%r14, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	convert_modes
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r12, %rcx
	callq	expand_complex_abs
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB12_54
# BB#60:                                # %if.else194
                                        #   in Loop: Header=BB12_50 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	jmp	.LBB12_61
.LBB12_62:                              # %for.end201
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	xorq	%r13, %r13
	jmp	.LBB12_63
.LBB12_37:                              # %if.then110
	testq	%rbx, %rbx
	movq	56(%rsp), %rdi          # 8-byte Reload
	jne	.LBB12_39
# BB#38:                                # %if.then113
	callq	gen_reg_rtx
	movq	%rax, %rbx
.LBB12_39:                              # %if.end115
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	convert_move
	movq	%rbx, %r13
.LBB12_63:                              # %return
	movq	%r13, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_65:                              # %if.then84
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	delete_insns_since
	xorq	%rcx, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %r8
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_unop  # TAILCALL
.LBB12_54:                              # %if.then183
	cmpq	$5, 32(%rsp)            # 8-byte Folded Reload
	movq	64(%rsp), %r14          # 8-byte Reload
	jne	.LBB12_55
# BB#58:                                # %if.else192
	movq	%r13, %rdi
	movq	%rbx, %rsi
	jmp	.LBB12_59
.LBB12_55:                              # %if.then186
	testq	%r14, %r14
	jne	.LBB12_57
# BB#56:                                # %if.then189
	movq	%r13, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r14
.LBB12_57:                              # %if.end191
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert_move
	movq	%r14, %r13
	jmp	.LBB12_63
.LBB12_64:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_complex_abs, %rcx
	movq	$2471, %rsi             # imm = 0x9A7
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp183:
	.size	expand_complex_abs, .Ltmp183-expand_complex_abs
	.cfi_endproc

	.globl	emit_unop_insn
	.align	16, 0x90
	.type	emit_unop_insn,@function
emit_unop_insn:                         # @emit_unop_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp190:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp191:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp192:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp193:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp194:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp195:
	.cfi_def_cfa_offset 80
.Ltmp196:
	.cfi_offset %rbx, -48
.Ltmp197:
	.cfi_offset %r12, -40
.Ltmp198:
	.cfi_offset %r13, -32
.Ltmp199:
	.cfi_offset %r14, -24
.Ltmp200:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	(%rdi,%rdi,4), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %r14
	movzbl	40(%r14), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, %rax
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	protect_from_queue
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB13_1
# BB#2:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	cmpq	$120, %rax
	je	.LBB13_3
# BB#4:                                 # %entry
	cmpq	$121, %rax
	je	.LBB13_5
# BB#6:                                 # %if.then
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	force_not_mem
	movq	%rax, %r15
	jmp	.LBB13_7
.LBB13_5:
	movq	%r12, 24(%rsp)          # 8-byte Spill
	jmp	.LBB13_7
.LBB13_1:
	movq	%r12, 24(%rsp)          # 8-byte Spill
	jmp	.LBB13_7
.LBB13_3:
	movq	%r12, 24(%rsp)          # 8-byte Spill
.LBB13_7:                               # %if.end
	movq	%r15, %rdi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	*24(%r14)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB13_9
# BB#8:                                 # %if.then12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r15
.LBB13_9:                               # %if.end14
	movzbl	2(%r13), %esi
	movq	%r13, %rdi
	callq	*(%r14)
	movq	%r12, %r14
	testq	%r12, %rax
	je	.LBB13_10
# BB#11:                                # %lor.lhs.false
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	movq	%r13, %r12
	je	.LBB13_14
# BB#12:                                # %land.lhs.true24
	movl	(%r13), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	%r13, %r12
	jne	.LBB13_14
	jmp	.LBB13_13
.LBB13_10:                              # %if.end14.if.then28_crit_edge
	movl	(%r13), %edi
.LBB13_13:                              # %if.then28
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
.LBB13_14:                              # %if.end33
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rbx
	movzbl	(%rbx), %eax
	cmpq	$24, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	jne	.LBB13_17
# BB#15:                                # %if.end33
	testq	%r14, %rdx
	je	.LBB13_17
# BB#16:                                # %if.then42
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rcx
	callq	add_equal_note
.LBB13_17:                              # %if.end44
	movq	%rbx, %rdi
	callq	emit_insn
	cmpq	%r13, %r12
	je	.LBB13_18
# BB#19:                                # %if.then47
	movq	%r13, %rdi
	movq	%r12, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_move_insn  # TAILCALL
.LBB13_18:                              # %if.end49
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp201:
	.size	emit_unop_insn, .Ltmp201-emit_unop_insn
	.cfi_endproc

	.globl	emit_clr_insn
	.align	16, 0x90
	.type	emit_clr_insn,@function
emit_clr_insn:                          # @emit_clr_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	const_int_rtx+512(%rip), %rsi
	jmp	emit_move_insn  # TAILCALL
.Ltmp202:
	.size	emit_clr_insn, .Ltmp202-emit_clr_insn
	.cfi_endproc

	.globl	emit_0_to_1_insn
	.align	16, 0x90
	.type	emit_0_to_1_insn,@function
emit_0_to_1_insn:                       # @emit_0_to_1_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	const_int_rtx+520(%rip), %rsi
	jmp	emit_move_insn  # TAILCALL
.Ltmp203:
	.size	emit_0_to_1_insn, .Ltmp203-emit_0_to_1_insn
	.cfi_endproc

	.globl	prepare_operand
	.align	16, 0x90
	.type	prepare_operand,@function
prepare_operand:                        # @prepare_operand
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp210:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp211:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp212:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp213:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp214:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp215:
	.cfi_def_cfa_offset 80
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
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r8, %r12
	andq	%rdx, %r12
	movq	%rcx, %r13
	andq	%rdx, %r13
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	protect_from_queue
	movq	%rax, %r14
	cmpq	%r12, %r13
	je	.LBB16_2
# BB#1:                                 # %if.then
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rax, %r14
.LBB16_2:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	(%rbx,%rbx,4), %rax
	movq	insn_data+24(,%rax,8), %r12
	leaq	(%r15,%r15,2), %rbx
	movzbl	16(%r12,%rbx,8), %esi
	movq	%r14, %rdi
	callq	*(%r12,%rbx,8)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_4
# BB#3:                                 # %if.end21
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_4:                               # %if.then11
	leaq	16(%r12,%rbx,8), %rax
	movzbl	(%rax), %edi
	movq	%r14, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	copy_to_mode_reg  # TAILCALL
.Ltmp221:
	.size	prepare_operand, .Ltmp221-prepare_operand
	.cfi_endproc

	.globl	emit_cmp_and_jump_insns
	.align	16, 0x90
	.type	emit_cmp_and_jump_insns,@function
emit_cmp_and_jump_insns:                # @emit_cmp_and_jump_insns
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
	subq	$80, %rsp
.Ltmp233:
	.cfi_def_cfa_offset 128
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
	movq	128(%rsp), %r12
	movq	%r9, %r13
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R15<kill>
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB17_1
# BB#2:                                 # %if.then
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB17_3
# BB#5:                                 # %if.end
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	swap_condition
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r15, %r12
	jmp	.LBB17_6
.LBB17_1:
	movq	%rbx, %r12
	movq	%r15, 72(%rsp)          # 8-byte Spill
.LBB17_6:                               # %if.end4
	callq	emit_queue
	testq	%r14, %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB17_7
# BB#8:                                 # %if.then6
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	unsigned_condition
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB17_9
.LBB17_7:
	movq	%r13, 40(%rsp)          # 8-byte Spill
.LBB17_9:                               # %if.end8
	movq	$32, %rcx
	movq	%rbx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%rbx, %r15
	andq	%r14, %r15
	cmpq	$51, %r15
	je	.LBB17_10
# BB#11:                                # %if.end8
	movl	flag_force_mem(%rip), %eax
	testq	%r14, %rax
	je	.LBB17_10
# BB#12:                                # %if.then.i
	movq	%r12, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	force_not_mem
	movq	%rax, 72(%rsp)          # 8-byte Spill
	jmp	.LBB17_13
.LBB17_10:
	movq	%r12, %rbx
.LBB17_13:                              # %if.end.i
	movq	64(%rsp), %r14          # 8-byte Reload
	movzbl	(%rbx), %ecx
	movq	%rbx, %r12
	cmpq	$133, %rcx
	jg	.LBB17_16
# BB#14:                                # %if.end.i
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_22
# BB#15:                                # %if.end.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB17_18
	jmp	.LBB17_22
.LBB17_16:                              # %if.end.i
	cmpq	$134, %rcx
	je	.LBB17_18
# BB#17:                                # %if.end.i
	cmpq	$140, %rcx
	jne	.LBB17_22
.LBB17_18:                              # %land.lhs.true30.i
	callq	preserve_subexpressions_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_22
# BB#19:                                # %land.lhs.true33.i
	movq	$74, %rsi
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB17_20
# BB#21:                                # %if.then36.i
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, %r12
	jmp	.LBB17_22
.LBB17_20:
	movq	%rbx, %r12
.LBB17_22:                              # %if.end38.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB17_25
# BB#23:                                # %if.end38.i
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_30
# BB#24:                                # %if.end38.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB17_27
	jmp	.LBB17_30
.LBB17_25:                              # %if.end38.i
	cmpq	$134, %rcx
	je	.LBB17_27
# BB#26:                                # %if.end38.i
	cmpq	$140, %rcx
	jne	.LBB17_30
.LBB17_27:                              # %land.lhs.true70.i
	callq	preserve_subexpressions_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_30
# BB#28:                                # %land.lhs.true73.i
	movq	$74, %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB17_30
# BB#29:                                # %if.then76.i
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_reg
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB17_30:                              # %if.end78.i
	movzbl	2(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_33
# BB#31:                                # %land.lhs.true82.i
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	jne	.LBB17_33
# BB#32:                                # %if.then87.i
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	force_reg
	movq	%rax, %r12
.LBB17_33:                              # %if.end89.i
	cmpq	$51, %r15
	jne	.LBB17_45
# BB#34:                                # %if.then91.i
	movq	%r12, %r14
	movq	16(%r14), %rax
	testq	%rax, %rax
	movq	$8, %rdx
	movq	$8, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	72(%rsp), %r15          # 8-byte Reload
	je	.LBB17_36
# BB#35:                                # %cond.true.i
	movl	32(%rax), %esi
.LBB17_36:                              # %cond.end.i
	movq	16(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB17_38
# BB#37:                                # %cond.true101.i
	movl	32(%rcx), %edx
.LBB17_38:                              # %cond.end107.i
	andq	%rdi, %rdx
	andq	%rdi, %rsi
	cmpq	%rdx, %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	$8, %rdx
	jae	.LBB17_41
# BB#39:                                # %cond.true110.i
	testq	%rax, %rax
	je	.LBB17_43
# BB#40:                                # %cond.true115.i
	movl	32(%rax), %edx
	jmp	.LBB17_43
.LBB17_45:                              # %if.end153.i
	movq	optab_table+328(%rip), %rax
	movq	optab_table+360(%rip), %rdx
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	.align	16, 0x90
.LBB17_46:                              # %do.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rax,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB17_49
# BB#47:                                # %if.end.i.i
                                        #   in Loop: Header=BB17_46 Depth=1
	movq	$4, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movl	8(%rdx,%rdi), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB17_112
# BB#48:                                # %if.end45.i.i
                                        #   in Loop: Header=BB17_46 Depth=1
	movzbl	mode_wider_mode(%rsi), %esi
	testq	%rsi, %rsi
	jne	.LBB17_46
	jmp	.LBB17_50
.LBB17_41:                              # %cond.false123.i
	testq	%rcx, %rcx
	je	.LBB17_43
# BB#42:                                # %cond.true128.i
	movl	32(%rcx), %edx
.LBB17_43:                              # %cond.end136.i
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	andq	%rdx, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, 56(%rsp)          # 8-byte Spill
	callq	emit_queue
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r14
	testq	%rbx, %rbx
	movq	%rbx, %r13
	je	.LBB17_44
# BB#111:                               # %if.end144.i
	movq	insn_data+48144(%rip), %rax
	movzbl	16(%rax), %r15d
	movq	%r15, %rdi
	callq	gen_reg_rtx
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	protect_from_queue
	movq	$4, %rdi
	movq	$1, %rdx
	movq	%rax, %rsi
	callq	convert_to_mode
	movq	%r12, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	gen_cmpstrsi
	movq	%rax, %rdi
	callq	emit_insn
	movq	const_int_rtx+512(%rip), %rbx
	movq	64(%rsp), %r14          # 8-byte Reload
	jmp	.LBB17_112
.LBB17_49:                              # %can_compare_p.exit.i
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$0, bcc_gen_fctn(,%rdx,8)
	jne	.LBB17_112
.LBB17_50:                              # %if.end157.i
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	16(%rax,%rdx), %rdi
	testq	%rdi, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	mode_class(,%r13,4), %edx
	je	.LBB17_56
# BB#51:                                # %if.end157.i
	movq	%rdx, %rax
	andq	%rcx, %rax
	cmpq	$2, %rax
	je	.LBB17_56
# BB#52:                                # %if.then164.i
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	movq	%r12, %r8
	je	.LBB17_55
# BB#53:                                # %land.lhs.true172.i
	movq	optab_table+336(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	16(%rax,%r13), %rax
	testq	%rax, %rax
	je	.LBB17_55
# BB#54:                                # %land.lhs.true172.i
	movq	%rax, %rdi
.LBB17_55:                              # %if.end183.i
	movl	word_mode(%rip), %edx
	movq	%r15, 8(%rsp)
	movq	$3, %rsi
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	%rbx, (%rsp)
	movq	%r15, %r9
	callq	emit_library_call
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movl	word_mode(%rip), %edi
	movq	%rax, %r12
	callq	hard_libcall_value
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	const_int_rtx+520(%rip), %rbx
	movl	word_mode(%rip), %r15d
	jmp	.LBB17_112
.LBB17_56:                              # %if.end187.i
	movq	%r14, 64(%rsp)          # 8-byte Spill
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	movq	%rcx, %r13
	movq	%r12, %r15
	je	.LBB17_57
# BB#110:                               # %if.else.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.prepare_cmp_insn, %rcx
	movq	$3238, %rsi             # imm = 0xCA6
	jmp	.LBB17_4
	.align	16, 0x90
.LBB17_102:                             # %if.then68.i.i
                                        #   in Loop: Header=BB17_57 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	movq	%rax, %r14
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert_to_mode
	movq	%rax, %r15
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	convert_to_mode
	movq	%rax, %rbx
.LBB17_57:                              # %tailrecurse.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_99 Depth 2
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	protect_from_queue
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movzbl	2(%r15), %ebx
	movq	%rax, %r14
	leaq	-13(%rbx), %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	jbe	.LBB17_58
.LBB17_98:                              # %if.else55.i.i
                                        #   in Loop: Header=BB17_57 Depth=1
	movq	optab_table+328(%rip), %rax
	.align	16, 0x90
.LBB17_99:                              # %for.cond.i.i
                                        #   Parent Loop BB17_57 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	mode_wider_mode(%rbx), %ebx
	testq	%rbx, %rbx
	je	.LBB17_103
# BB#100:                               # %for.body.i.i
                                        #   in Loop: Header=BB17_99 Depth=2
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB17_102
# BB#101:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB17_99 Depth=2
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	cmpq	$0, 16(%rax,%rdx)
	je	.LBB17_99
	jmp	.LBB17_102
.LBB17_58:                              # %tailrecurse.i.i
                                        #   in Loop: Header=BB17_57 Depth=1
	jmpq	*.LJTI17_0(,%rax,8)
.LBB17_59:                              # %if.then.i1.i
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-102(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	ja	.LBB17_152
# BB#60:                                # %if.then.i1.i
	movabsq	$libfunc_table+192, %rax
	movq	$103, %rbx
	movq	$13, %r9
	jmpq	*.LJTI17_5(,%rcx,8)
.LBB17_104:                             # %sw.bb2.i.i
	movabsq	$libfunc_table+200, %rax
	jmp	.LBB17_105
.LBB17_66:                              # %if.then9.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-102(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	ja	.LBB17_152
# BB#67:                                # %if.then9.i.i
	movabsq	$libfunc_table+248, %rax
	movq	$103, %rbx
	movq	$15, %r9
	jmpq	*.LJTI17_4(,%rcx,8)
.LBB17_68:                              # %sw.bb11.i.i
	movabsq	$libfunc_table+256, %rax
	jmp	.LBB17_105
.LBB17_74:                              # %if.then21.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-102(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	ja	.LBB17_152
# BB#75:                                # %if.then21.i.i
	movabsq	$libfunc_table+304, %rax
	movq	$103, %rbx
	movq	$16, %r9
	jmpq	*.LJTI17_3(,%rcx,8)
.LBB17_76:                              # %sw.bb23.i.i
	movabsq	$libfunc_table+312, %rax
	jmp	.LBB17_105
.LBB17_82:                              # %if.then33.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-102(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	ja	.LBB17_152
# BB#83:                                # %if.then33.i.i
	movabsq	$libfunc_table+360, %rax
	movq	$103, %rbx
	movq	$17, %r9
	jmpq	*.LJTI17_2(,%rcx,8)
.LBB17_84:                              # %sw.bb35.i.i
	movabsq	$libfunc_table+368, %rax
	jmp	.LBB17_105
.LBB17_90:                              # %if.then45.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-102(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$10, %rcx
	ja	.LBB17_152
# BB#91:                                # %if.then45.i.i
	movabsq	$libfunc_table+416, %rax
	movq	$103, %rbx
	movq	$18, %r9
	jmpq	*.LJTI17_1(,%rcx,8)
.LBB17_92:                              # %sw.bb47.i.i
	movabsq	$libfunc_table+424, %rax
	jmp	.LBB17_105
.LBB17_62:                              # %sw.bb4.i.i
	movabsq	$libfunc_table+216, %rax
	jmp	.LBB17_105
.LBB17_61:                              # %sw.bb3.i.i
	movabsq	$libfunc_table+208, %rax
	jmp	.LBB17_105
.LBB17_64:                              # %sw.bb6.i.i
	movabsq	$libfunc_table+232, %rax
	jmp	.LBB17_105
.LBB17_63:                              # %sw.bb5.i.i
	movabsq	$libfunc_table+224, %rax
	jmp	.LBB17_105
.LBB17_65:                              # %sw.bb7.i.i
	movabsq	$libfunc_table+240, %rax
	jmp	.LBB17_105
.LBB17_70:                              # %sw.bb13.i.i
	movabsq	$libfunc_table+272, %rax
	jmp	.LBB17_105
.LBB17_69:                              # %sw.bb12.i.i
	movabsq	$libfunc_table+264, %rax
	jmp	.LBB17_105
.LBB17_72:                              # %sw.bb15.i.i
	movabsq	$libfunc_table+288, %rax
	jmp	.LBB17_105
.LBB17_71:                              # %sw.bb14.i.i
	movabsq	$libfunc_table+280, %rax
	jmp	.LBB17_105
.LBB17_73:                              # %sw.bb16.i.i
	movabsq	$libfunc_table+296, %rax
	jmp	.LBB17_105
.LBB17_78:                              # %sw.bb25.i.i
	movabsq	$libfunc_table+328, %rax
	jmp	.LBB17_105
.LBB17_77:                              # %sw.bb24.i.i
	movabsq	$libfunc_table+320, %rax
	jmp	.LBB17_105
.LBB17_80:                              # %sw.bb27.i.i
	movabsq	$libfunc_table+344, %rax
	jmp	.LBB17_105
.LBB17_79:                              # %sw.bb26.i.i
	movabsq	$libfunc_table+336, %rax
	jmp	.LBB17_105
.LBB17_81:                              # %sw.bb28.i.i
	movabsq	$libfunc_table+352, %rax
	jmp	.LBB17_105
.LBB17_86:                              # %sw.bb37.i.i
	movabsq	$libfunc_table+384, %rax
	jmp	.LBB17_105
.LBB17_85:                              # %sw.bb36.i.i
	movabsq	$libfunc_table+376, %rax
	jmp	.LBB17_105
.LBB17_88:                              # %sw.bb39.i.i
	movabsq	$libfunc_table+400, %rax
	jmp	.LBB17_105
.LBB17_87:                              # %sw.bb38.i.i
	movabsq	$libfunc_table+392, %rax
	jmp	.LBB17_105
.LBB17_89:                              # %sw.bb40.i.i
	movabsq	$libfunc_table+408, %rax
	jmp	.LBB17_105
.LBB17_94:                              # %sw.bb49.i.i
	movabsq	$libfunc_table+440, %rax
	jmp	.LBB17_105
.LBB17_93:                              # %sw.bb48.i.i
	movabsq	$libfunc_table+432, %rax
	jmp	.LBB17_105
.LBB17_96:                              # %sw.bb51.i.i
	movabsq	$libfunc_table+456, %rax
	jmp	.LBB17_105
.LBB17_95:                              # %sw.bb50.i.i
	movabsq	$libfunc_table+448, %rax
	jmp	.LBB17_105
.LBB17_97:                              # %sw.bb52.i.i
	movabsq	$libfunc_table+464, %rax
.LBB17_105:                             # %if.end80.i.i
	movq	64(%rsp), %rbx          # 8-byte Reload
.LBB17_106:                             # %if.end80.i.i
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB17_152
# BB#107:                               # %if.end84.i.i
	movl	word_mode(%rip), %edx
	movq	%r9, 8(%rsp)
	movq	%r14, (%rsp)
	movq	$3, %rsi
	movq	$2, %rcx
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%r15, %r8
	callq	emit_library_call
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movl	word_mode(%rip), %edi
	movq	%rax, %r12
	callq	hard_libcall_value
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	const_int_rtx+512(%rip), %rcx
	movl	word_mode(%rip), %r15d
	andq	%r13, %rbx
	cmpq	$112, %rbx
	movq	$102, %rax
	je	.LBB17_109
# BB#108:                               # %if.end84.i.i
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB17_109:                             # %if.end84.i.i
	movq	%rax, %r14
	movq	%rcx, %rbx
.LBB17_112:                             # %prepare_cmp_insn.exit
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movl	mode_class(,%rdi,4), %r8d
	movq	%rax, %r9
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r8
	movq	128(%rsp), %r14
	movq	%r14, %rsi
	.align	16, 0x90
.LBB17_113:                             # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rsi, %rsi
	movb	%r15b, 2(%r9)
	je	.LBB17_126
# BB#114:                               # %if.then.i21
                                        #   in Loop: Header=BB17_113 Depth=1
	movq	optab_table+360(%rip), %rax
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	movq	$32, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB17_126
# BB#115:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB17_113 Depth=1
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rax
	movq	%r9, %r13
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r8, %r14
	callq	*(%rax)
	movq	%r14, %r8
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %r9
	movq	128(%rsp), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_116
.LBB17_126:                             # %if.end15.i
                                        #   in Loop: Header=BB17_113 Depth=1
	movq	const_tiny_rtx(,%rdi,8), %rdx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rdx
	jne	.LBB17_135
# BB#127:                               # %if.end15.i
                                        #   in Loop: Header=BB17_113 Depth=1
	movq	optab_table+344(%rip), %rcx
	movslq	8(%rcx,%rax), %r12
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB17_128
.LBB17_135:                             # %if.end39.i
                                        #   in Loop: Header=BB17_113 Depth=1
	movq	optab_table+328(%rip), %rax
	movq	$4, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movslq	8(%rax,%rdx), %r12
	cmpq	$1317, %r12             # imm = 0x525
	jne	.LBB17_136
# BB#147:                               # %if.end60.i
                                        #   in Loop: Header=BB17_113 Depth=1
	cmpq	$6, %r8
	ja	.LBB17_150
# BB#148:                               # %if.end60.i
                                        #   in Loop: Header=BB17_113 Depth=1
	movq	$1, %rax
	movq	%r8, %rcx
	shlq	%cl, %rax
	testq	$70, %rax
	je	.LBB17_150
# BB#149:                               # %if.end67.i
                                        #   in Loop: Header=BB17_113 Depth=1
	movzbl	mode_wider_mode(%rbx), %r15d
	testq	%r15, %r15
	jne	.LBB17_113
.LBB17_150:                             # %do.end.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.emit_cmp_and_jump_insn_1, %rdx
	movq	$3333, %rsi             # imm = 0xD05
	callq	fancy_abort
.LBB17_128:                             # %if.then25.i
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r14
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	%r15, %rcx
	andq	%rdx, %rcx
	movq	%rdx, %r13
	cmpq	%rax, %rcx
	je	.LBB17_130
# BB#129:                               # %if.then.i153.i
	movq	%r15, %rdi
	movq	%r14, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rax, %r14
.LBB17_130:                             # %if.end.i163.i
	leaq	(%r12,%r12,4), %r15
	movq	insn_data+24(,%r15,8), %rbx
	movzbl	16(%rbx), %esi
	movq	%r14, %rdi
	callq	*(%rbx)
	testq	%r13, %rax
	jne	.LBB17_132
# BB#131:                               # %if.then11.i167.i
	movzbl	16(%rbx), %edi
	movq	%r14, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r14
.LBB17_132:                             # %prepare_operand.exit169.i
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	*insn_data+16(,%r15,8)
	movq	%rax, %rdi
	callq	emit_insn
	movq	128(%rsp), %rdi
	testq	%rdi, %rdi
	movq	64(%rsp), %rdx          # 8-byte Reload
	je	.LBB17_151
# BB#133:                               # %if.then33.i
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	callq	*bcc_gen_fctn(,%rdx,8)
	jmp	.LBB17_134
.LBB17_136:                             # %if.then45.i
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rdx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%r15, %r14
	andq	%rax, %r14
	cmpq	%rcx, %r14
	movq	40(%rsp), %rcx          # 8-byte Reload
	je	.LBB17_138
# BB#137:                               # %if.then.i173.i
	movq	%r15, %rdi
	callq	convert_modes
	movq	%rax, %rdx
.LBB17_138:                             # %if.end.i183.i
	leaq	(%r12,%r12,4), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %r13
	movzbl	16(%r13), %esi
	movq	%rdx, %rdi
	movq	%rdx, %rbx
	callq	*(%r13)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_140
# BB#139:
	movq	%rbx, %r12
	jmp	.LBB17_141
.LBB17_140:                             # %if.then11.i187.i
	movzbl	16(%r13), %edi
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
.LBB17_141:                             # %prepare_operand.exit189.i
	xorq	%rsi, %rsi
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rbx
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB17_143
# BB#142:                               # %if.then.i193.i
	movq	%r15, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rax, %rbx
.LBB17_143:                             # %if.end.i203.i
	movq	64(%rsp), %r14          # 8-byte Reload
	movzbl	40(%r13), %esi
	movq	%rbx, %rdi
	callq	*24(%r13)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB17_145
# BB#144:                               # %if.then11.i207.i
	movzbl	40(%r13), %edi
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB17_145:                             # %prepare_operand.exit209.i
	movq	%r12, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	32(%rsp), %rcx          # 8-byte Reload
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	movq	128(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB17_151
# BB#146:                               # %if.then54.i
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	callq	*bcc_gen_fctn(,%r14,8)
	jmp	.LBB17_134
.LBB17_151:                             # %emit_cmp_and_jump_insn_1.exit
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_116:                             # %if.then8.i
	movq	%r9, 64(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rcx
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	%r15, %r13
	andq	%rax, %r13
	cmpq	%r14, %r13
	je	.LBB17_118
# BB#117:                               # %if.then.i.i
	movq	%r15, %rdi
	movq	%rcx, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rax, %rcx
.LBB17_118:                             # %if.end.i.i26
	leaq	(%r12,%r12,4), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rbx
	movzbl	40(%rbx), %esi
	movq	%rcx, %rdi
	movq	%rcx, %r12
	callq	*24(%rbx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_120
# BB#119:
	movq	%r12, 48(%rsp)          # 8-byte Spill
	jmp	.LBB17_121
.LBB17_120:                             # %if.then11.i.i
	movzbl	40(%rbx), %edi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB17_121:                             # %prepare_operand.exit.i
	xorq	%rsi, %rsi
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r12
	cmpq	%r14, %r13
	je	.LBB17_123
# BB#122:                               # %if.then.i133.i
	movq	%r15, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	%rax, %r12
.LBB17_123:                             # %if.end.i143.i
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movzbl	64(%rbx), %esi
	movq	%r12, %rdi
	callq	*48(%rbx)
	testq	%r14, %rax
	jne	.LBB17_125
# BB#124:                               # %if.then11.i147.i
	movzbl	64(%rbx), %edi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
.LBB17_125:                             # %prepare_operand.exit149.i
	movq	128(%rsp), %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	xorq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	movq	32(%rsp), %rbx          # 8-byte Reload
	callq	*insn_data+16(,%rbx,8)
.LBB17_134:                             # %if.then33.i
	movq	%rax, %rdi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_jump_insn  # TAILCALL
.LBB17_152:                             # %if.then83.i.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.prepare_float_lib_cmp, %rdx
	movq	$3616, %rsi             # imm = 0xE20
	callq	fancy_abort
.LBB17_103:                             # %for.end.i.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.prepare_float_lib_cmp, %rdx
	movq	$3612, %rsi             # imm = 0xE1C
	callq	fancy_abort
.LBB17_3:                               # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.emit_cmp_and_jump_insns, %rcx
	movq	$3370, %rsi             # imm = 0xD2A
	jmp	.LBB17_4
.LBB17_44:                              # %if.then143.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.prepare_cmp_insn, %rcx
	movq	$3132, %rsi             # imm = 0xC3C
.LBB17_4:                               # %if.then2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp239:
	.size	emit_cmp_and_jump_insns, .Ltmp239-emit_cmp_and_jump_insns
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_59
	.quad	.LBB17_98
	.quad	.LBB17_66
	.quad	.LBB17_74
	.quad	.LBB17_82
	.quad	.LBB17_90
.LJTI17_1:
	.quad	.LBB17_92
	.quad	.LBB17_106
	.quad	.LBB17_94
	.quad	.LBB17_93
	.quad	.LBB17_96
	.quad	.LBB17_95
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_97
.LJTI17_2:
	.quad	.LBB17_84
	.quad	.LBB17_106
	.quad	.LBB17_86
	.quad	.LBB17_85
	.quad	.LBB17_88
	.quad	.LBB17_87
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_89
.LJTI17_3:
	.quad	.LBB17_76
	.quad	.LBB17_106
	.quad	.LBB17_78
	.quad	.LBB17_77
	.quad	.LBB17_80
	.quad	.LBB17_79
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_81
.LJTI17_4:
	.quad	.LBB17_68
	.quad	.LBB17_106
	.quad	.LBB17_70
	.quad	.LBB17_69
	.quad	.LBB17_72
	.quad	.LBB17_71
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_73
.LJTI17_5:
	.quad	.LBB17_104
	.quad	.LBB17_106
	.quad	.LBB17_62
	.quad	.LBB17_61
	.quad	.LBB17_64
	.quad	.LBB17_63
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_152
	.quad	.LBB17_65

	.text
	.globl	emit_cmp_insn
	.align	16, 0x90
	.type	emit_cmp_insn,@function
emit_cmp_insn:                          # @emit_cmp_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp241:
	.cfi_def_cfa_offset 16
	movq	$0, (%rsp)
	callq	emit_cmp_and_jump_insns
	popq	%rax
	retq
.Ltmp242:
	.size	emit_cmp_insn, .Ltmp242-emit_cmp_insn
	.cfi_endproc

	.globl	emit_indirect_jump
	.align	16, 0x90
	.type	emit_indirect_jump,@function
emit_indirect_jump:                     # @emit_indirect_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp245:
	.cfi_def_cfa_offset 16
.Ltmp246:
	.cfi_offset %rbx, -16
	movl	target_flags(%rip), %esi
	movq	%rdi, %rbx
	movq	insn_data+46384(%rip), %rax
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	*(%rax)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_2
# BB#1:                                 # %if.then
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	movq	%rbx, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %rbx
.LBB19_2:                               # %if.end
	movq	%rbx, %rdi
	callq	gen_indirect_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	popq	%rbx
	jmp	emit_barrier  # TAILCALL
.Ltmp247:
	.size	emit_indirect_jump, .Ltmp247-emit_indirect_jump
	.cfi_endproc

	.globl	emit_conditional_move
	.align	16, 0x90
	.type	emit_conditional_move,@function
emit_conditional_move:                  # @emit_conditional_move
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp254:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp255:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp256:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp257:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp258:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp259:
	.cfi_def_cfa_offset 112
.Ltmp260:
	.cfi_offset %rbx, -48
.Ltmp261:
	.cfi_offset %r12, -40
.Ltmp262:
	.cfi_offset %r13, -32
.Ltmp263:
	.cfi_offset %r14, -24
.Ltmp264:
	.cfi_offset %r15, -16
	movq	%r9, %r15
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	%rsi, %r13
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	swap_commutative_operands_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB20_1
# BB#2:                                 # %if.then
	movq	%r13, %rdi
	callq	swap_condition
	movq	%rbx, %rdx
	jmp	.LBB20_3
.LBB20_1:
	movq	%r12, %rax
	movq	%rbx, %r12
	movq	%rax, %rdx
	movq	%r13, %rax
.LBB20_3:                               # %if.end
	movq	112(%rsp), %rsi
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$105, %rcx
	movq	%r15, %rdi
	jne	.LBB20_4
# BB#12:                                # %land.lhs.true7
	movq	%rdx, %r15
	movzbl	(%r15), %ecx
	cmpq	$54, %rcx
	jne	.LBB20_13
# BB#14:                                # %land.lhs.true11
	movq	%rax, %rcx
	movq	8(%r15), %rax
	cmpq	$-1, %rax
	jne	.LBB20_16
# BB#15:
	movq	const_int_rtx+512(%rip), %r15
.LBB20_16:                              # %land.lhs.true11
	movq	$104, %rax
	je	.LBB20_18
# BB#17:                                # %land.lhs.true11
	movq	%rcx, %rax
	jmp	.LBB20_18
.LBB20_4:                               # %if.end
	cmpq	$107, %rcx
	movq	%rdx, %r15
	jne	.LBB20_18
# BB#5:                                 # %land.lhs.true
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	movq	$107, %rax
	jne	.LBB20_18
# BB#6:                                 # %land.lhs.true3
	movq	8(%r15), %rax
	movq	%rsi, %rbx
	cmpq	$1, %rax
	jne	.LBB20_8
# BB#7:
	movq	const_int_rtx+512(%rip), %r15
.LBB20_8:                               # %land.lhs.true3
	movq	%rbx, %rsi
	je	.LBB20_9
# BB#10:                                # %land.lhs.true3
	movabsq	$1, %rax
	orq	$106, %rax
	jmp	.LBB20_18
.LBB20_13:
	movq	$105, %rax
	jmp	.LBB20_18
.LBB20_9:
	movabsq	$0, %rax
	orq	$106, %rax
.LBB20_18:                              # %if.end18
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB20_20
# BB#19:                                # %if.then20
	movzbl	2(%r12), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
.LBB20_20:                              # %if.end23
	movq	%rsi, %r13
	movq	%rdi, %rbx
	callq	swap_commutative_operands_p
	testq	%r14, %rax
	je	.LBB20_21
# BB#22:                                # %land.lhs.true26
	xorq	%rcx, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdx
	callq	reversed_comparison_code_parts
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%rbx, %r12
	je	.LBB20_24
# BB#23:                                # %land.lhs.true26
	movq	%r13, %r12
.LBB20_24:                              # %land.lhs.true26
	je	.LBB20_26
# BB#25:                                # %land.lhs.true26
	movq	%rax, 48(%rsp)          # 8-byte Spill
.LBB20_26:                              # %land.lhs.true26
	testq	%rcx, %rcx
	movq	120(%rsp), %rcx
	je	.LBB20_28
# BB#27:                                # %land.lhs.true26
	movq	%rbx, %r13
	jmp	.LBB20_28
.LBB20_21:
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	%rbx, %r12
	movq	120(%rsp), %rcx
.LBB20_28:                              # %if.end30
	movq	%r13, 40(%rsp)          # 8-byte Spill
	testq	%r14, %rcx
	jne	.LBB20_30
# BB#29:                                # %if.then32
	movzbl	2(%r12), %ecx
.LBB20_30:                              # %if.end36
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	%rcx, %rax
	movq	%rcx, %rbx
	andq	%r14, %rax
	movl	movcc_gen_code(,%rax,4), %eax
	movq	$32, %rcx
	movq	%rax, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%r15, %r15
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB20_47
# BB#31:                                # %if.end40
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_33
# BB#32:                                # %if.then42
	movq	%r12, %rdi
	callq	force_not_mem
	movq	%rax, %r12
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	force_not_mem
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB20_33:                              # %if.end45
	movq	16(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB20_35
# BB#34:                                # %if.then47
	movq	$1, %rsi
	callq	protect_from_queue
	jmp	.LBB20_36
.LBB20_35:                              # %if.else49
	movq	%rbx, %rdi
	callq	gen_reg_rtx
.LBB20_36:                              # %if.end51
	movq	%rax, %r15
	callq	emit_queue
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	protect_from_queue
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rcx,%rcx,4), %rcx
	movq	insn_data+24(,%rcx,8), %rbx
	movzbl	16(%rbx), %esi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	*(%rbx)
	testq	%r14, %rax
	movq	%r15, %rax
	jne	.LBB20_38
# BB#37:                                # %if.then65
	movzbl	16(%rbx), %edi
	callq	gen_reg_rtx
.LBB20_38:                              # %if.end74
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movzbl	64(%rbx), %esi
	movq	%r12, %rdi
	callq	*48(%rbx)
	testq	%r14, %rax
	movq	%r12, %rax
	jne	.LBB20_40
# BB#39:                                # %if.then89
	movzbl	64(%rbx), %edi
	movq	%rax, %rsi
	callq	copy_to_mode_reg
.LBB20_40:                              # %if.end98
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movzbl	88(%rbx), %esi
	movq	%r12, %rdi
	callq	*72(%rbx)
	testq	%r14, %rax
	jne	.LBB20_42
# BB#41:                                # %if.then113
	movzbl	88(%rbx), %edi
	movq	%r12, %rsi
	callq	copy_to_mode_reg
	movq	%rax, %r12
.LBB20_42:                              # %if.end122
	movq	24(%rsp), %rsi          # 8-byte Reload
	xorq	%r9, %r9
	xorq	%r15, %r15
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	128(%rsp), %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	compare_from_rtx
	movq	%rax, %rcx
	movzbl	(%rcx), %eax
	andq	%rbx, %r14
	cmpq	%r14, %rax
	jne	.LBB20_47
# BB#43:                                # %if.end128
	leaq	(%r13,%r13,4), %rbx
	xorq	%rax, %rax
	xorq	%r15, %r15
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	*insn_data+16(,%rbx,8)
	testq	%rax, %rax
	je	.LBB20_47
# BB#44:                                # %if.end134
	movq	%rax, %rdi
	callq	emit_insn
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	cmpq	%rbx, %rsi
	je	.LBB20_46
# BB#45:                                # %if.then137
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	convert_move
.LBB20_46:                              # %return
	movq	%rbx, %r15
.LBB20_47:                              # %return
	movq	%r15, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp265:
	.size	emit_conditional_move, .Ltmp265-emit_conditional_move
	.cfi_endproc

	.globl	can_conditionally_move_p
	.align	16, 0x90
	.type	can_conditionally_move_p,@function
can_conditionally_move_p:               # @can_conditionally_move_p
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	movl	movcc_gen_code(,%rax,4), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB21_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB21_1:
	movabsq	$0, %rax
	retq
.Ltmp266:
	.size	can_conditionally_move_p, .Ltmp266-can_conditionally_move_p
	.cfi_endproc

	.globl	gen_add2_insn
	.align	16, 0x90
	.type	gen_add2_insn,@function
gen_add2_insn:                          # @gen_add2_insn
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
.Ltmp278:
	.cfi_offset %rbx, -48
.Ltmp279:
	.cfi_offset %r12, -40
.Ltmp280:
	.cfi_offset %r13, -32
.Ltmp281:
	.cfi_offset %r14, -24
.Ltmp282:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	2(%r12), %eax
	movq	optab_table(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	leaq	(%rax,%rax,4), %r15
	movq	insn_data+24(,%r15,8), %rbx
	movzbl	16(%rbx), %eax
	movq	%rsi, %r14
                                        # kill: RDI<def> R12<kill>
	movq	%rax, %rsi
	callq	*(%rbx)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB22_3
# BB#1:                                 # %lor.lhs.false
	movzbl	40(%rbx), %esi
	movq	%r12, %rdi
	callq	*24(%rbx)
	testq	%r13, %rax
	je	.LBB22_3
# BB#2:                                 # %lor.lhs.false23
	movzbl	64(%rbx), %esi
	movq	%r14, %rdi
	callq	*48(%rbx)
	testq	%r13, %rax
	je	.LBB22_3
# BB#4:                                 # %if.end
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*insn_data+16(,%r15,8)  # TAILCALL
.LBB22_3:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_add2_insn, %rcx
	movq	$3823, %rsi             # imm = 0xEEF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp283:
	.size	gen_add2_insn, .Ltmp283-gen_add2_insn
	.cfi_endproc

	.globl	gen_add3_insn
	.align	16, 0x90
	.type	gen_add3_insn,@function
gen_add3_insn:                          # @gen_add3_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp290:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp291:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp292:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp293:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp294:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp295:
	.cfi_def_cfa_offset 64
.Ltmp296:
	.cfi_offset %rbx, -48
.Ltmp297:
	.cfi_offset %r12, -40
.Ltmp298:
	.cfi_offset %r13, -32
.Ltmp299:
	.cfi_offset %r14, -24
.Ltmp300:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	2(%r13), %eax
	movq	optab_table(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdi,%rax), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB23_4
# BB#1:                                 # %lor.lhs.false
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rbx
	movzbl	16(%rbx), %esi
	movq	%r13, %rdi
	callq	*(%rbx)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB23_4
# BB#2:                                 # %lor.lhs.false9
	movzbl	40(%rbx), %esi
	movq	%r15, %rdi
	callq	*24(%rbx)
	testq	%r12, %rax
	je	.LBB23_4
# BB#3:                                 # %lor.lhs.false24
	movzbl	64(%rbx), %esi
	movq	%r14, %rdi
	callq	*48(%rbx)
	testq	%r12, %rax
	je	.LBB23_4
# BB#5:                                 # %if.end
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*insn_data+16(,%rcx,8)  # TAILCALL
.LBB23_4:                               # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp301:
	.size	gen_add3_insn, .Ltmp301-gen_add3_insn
	.cfi_endproc

	.globl	have_add2_insn
	.align	16, 0x90
	.type	have_add2_insn,@function
have_add2_insn:                         # @have_add2_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp308:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp309:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp310:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp311:
	.cfi_def_cfa_offset 48
.Ltmp312:
	.cfi_offset %rbx, -40
.Ltmp313:
	.cfi_offset %r12, -32
.Ltmp314:
	.cfi_offset %r14, -24
.Ltmp315:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	2(%rbx), %eax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB24_9
# BB#1:                                 # %if.end
	movq	optab_table(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB24_8
# BB#2:                                 # %if.end6
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %r15
	movzbl	16(%r15), %esi
	movq	%rbx, %rdi
	callq	*(%r15)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB24_8
# BB#3:                                 # %lor.lhs.false
	movzbl	40(%r15), %esi
	movq	%rbx, %rdi
	callq	*24(%r15)
	testq	%r12, %rax
	je	.LBB24_8
# BB#4:                                 # %lor.lhs.false29
	movzbl	64(%r15), %esi
	movq	%r14, %rdi
	callq	*48(%r15)
	testq	%r12, %rax
	je	.LBB24_5
# BB#6:                                 # %lor.lhs.false29
	movabsq	$1, %rax
	jmp	.LBB24_7
.LBB24_8:                               # %return
	xorq	%rax, %rax
.LBB24_7:                               # %lor.lhs.false29
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB24_5:
	movabsq	$0, %rax
	jmp	.LBB24_7
.LBB24_9:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.have_add2_insn, %rcx
	movq	$3855, %rsi             # imm = 0xF0F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp316:
	.size	have_add2_insn, .Ltmp316-have_add2_insn
	.cfi_endproc

	.globl	gen_sub2_insn
	.align	16, 0x90
	.type	gen_sub2_insn,@function
gen_sub2_insn:                          # @gen_sub2_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp323:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp324:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp325:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp326:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp327:
	.cfi_def_cfa_offset 48
.Ltmp328:
	.cfi_offset %rbx, -48
.Ltmp329:
	.cfi_offset %r12, -40
.Ltmp330:
	.cfi_offset %r13, -32
.Ltmp331:
	.cfi_offset %r14, -24
.Ltmp332:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	2(%r12), %eax
	movq	optab_table+16(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	leaq	(%rax,%rax,4), %r15
	movq	insn_data+24(,%r15,8), %rbx
	movzbl	16(%rbx), %eax
	movq	%rsi, %r14
                                        # kill: RDI<def> R12<kill>
	movq	%rax, %rsi
	callq	*(%rbx)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB25_3
# BB#1:                                 # %lor.lhs.false
	movzbl	40(%rbx), %esi
	movq	%r12, %rdi
	callq	*24(%rbx)
	testq	%r13, %rax
	je	.LBB25_3
# BB#2:                                 # %lor.lhs.false23
	movzbl	64(%rbx), %esi
	movq	%r14, %rdi
	callq	*48(%rbx)
	testq	%r13, %rax
	je	.LBB25_3
# BB#4:                                 # %if.end
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*insn_data+16(,%r15,8)  # TAILCALL
.LBB25_3:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_sub2_insn, %rcx
	movq	$3887, %rsi             # imm = 0xF2F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp333:
	.size	gen_sub2_insn, .Ltmp333-gen_sub2_insn
	.cfi_endproc

	.globl	gen_sub3_insn
	.align	16, 0x90
	.type	gen_sub3_insn,@function
gen_sub3_insn:                          # @gen_sub3_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp340:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp341:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp342:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp343:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp344:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp345:
	.cfi_def_cfa_offset 64
.Ltmp346:
	.cfi_offset %rbx, -48
.Ltmp347:
	.cfi_offset %r12, -40
.Ltmp348:
	.cfi_offset %r13, -32
.Ltmp349:
	.cfi_offset %r14, -24
.Ltmp350:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	2(%r13), %eax
	movq	optab_table+16(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdi,%rax), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB26_4
# BB#1:                                 # %lor.lhs.false
	leaq	(%rax,%rax,4), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	insn_data+24(,%rax,8), %rbx
	movzbl	16(%rbx), %esi
	movq	%r13, %rdi
	callq	*(%rbx)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB26_4
# BB#2:                                 # %lor.lhs.false9
	movzbl	40(%rbx), %esi
	movq	%r15, %rdi
	callq	*24(%rbx)
	testq	%r12, %rax
	je	.LBB26_4
# BB#3:                                 # %lor.lhs.false24
	movzbl	64(%rbx), %esi
	movq	%r14, %rdi
	callq	*48(%rbx)
	testq	%r12, %rax
	je	.LBB26_4
# BB#5:                                 # %if.end
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmpq	*insn_data+16(,%rcx,8)  # TAILCALL
.LBB26_4:                               # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp351:
	.size	gen_sub3_insn, .Ltmp351-gen_sub3_insn
	.cfi_endproc

	.globl	have_sub2_insn
	.align	16, 0x90
	.type	have_sub2_insn,@function
have_sub2_insn:                         # @have_sub2_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp357:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp358:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp359:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp360:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp361:
	.cfi_def_cfa_offset 48
.Ltmp362:
	.cfi_offset %rbx, -40
.Ltmp363:
	.cfi_offset %r12, -32
.Ltmp364:
	.cfi_offset %r14, -24
.Ltmp365:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	2(%rbx), %eax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB27_9
# BB#1:                                 # %if.end
	movq	optab_table+16(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movslq	8(%rdx,%rax), %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB27_8
# BB#2:                                 # %if.end6
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %r15
	movzbl	16(%r15), %esi
	movq	%rbx, %rdi
	callq	*(%r15)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB27_8
# BB#3:                                 # %lor.lhs.false
	movzbl	40(%r15), %esi
	movq	%rbx, %rdi
	callq	*24(%r15)
	testq	%r12, %rax
	je	.LBB27_8
# BB#4:                                 # %lor.lhs.false29
	movzbl	64(%r15), %esi
	movq	%r14, %rdi
	callq	*48(%r15)
	testq	%r12, %rax
	je	.LBB27_5
# BB#6:                                 # %lor.lhs.false29
	movabsq	$1, %rax
	jmp	.LBB27_7
.LBB27_8:                               # %return
	xorq	%rax, %rax
.LBB27_7:                               # %lor.lhs.false29
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB27_5:
	movabsq	$0, %rax
	jmp	.LBB27_7
.LBB27_9:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.have_sub2_insn, %rcx
	movq	$3919, %rsi             # imm = 0xF4F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp366:
	.size	have_sub2_insn, .Ltmp366-have_sub2_insn
	.cfi_endproc

	.globl	gen_move_insn
	.align	16, 0x90
	.type	gen_move_insn,@function
gen_move_insn:                          # @gen_move_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp372:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp373:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp374:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp375:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp376:
	.cfi_def_cfa_offset 48
.Ltmp377:
	.cfi_offset %rbx, -40
.Ltmp378:
	.cfi_offset %r12, -32
.Ltmp379:
	.cfi_offset %r14, -24
.Ltmp380:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	2(%r15), %edx
	movq	%rsi, %r14
	testq	%rdx, %rdx
	jne	.LBB28_2
# BB#1:                                 # %if.then
	movzbl	2(%r14), %edx
.LBB28_2:                               # %if.end
	movl	mode_class(,%rdx,4), %esi
	movq	$4, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	cmpq	$4, %rsi
	jne	.LBB28_20
# BB#3:                                 # %if.end
	movq	optab_table+240(%rip), %rcx
	movl	8(%rcx,%rax), %esi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$1317, %rsi             # imm = 0x525
	jne	.LBB28_20
# BB#4:                                 # %if.then9
	cmpq	$52, %rdx
	je	.LBB28_6
# BB#5:                                 # %land.lhs.true11
	movl	840(%rcx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	movq	$52, %rbx
	jne	.LBB28_11
.LBB28_6:                               # %for.cond.preheader
	movzbl	mode_size(%rdx), %ecx
	movq	$2, %rbx
	andq	$255, %rcx
	.align	16, 0x90
.LBB28_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	andq	%rax, %rdx
	movzbl	mode_size(%rdx), %esi
	cmpq	%rcx, %rsi
	je	.LBB28_10
# BB#8:                                 # %for.inc
                                        #   in Loop: Header=BB28_7 Depth=1
	movzbl	mode_wider_mode(%rdx), %ebx
	testq	%rbx, %rbx
	jne	.LBB28_7
	jmp	.LBB28_9
.LBB28_20:                              # %if.end68
	callq	start_sequence
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn_1
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB28_10:                              # %if.end30
	testq	%rax, %rbx
	je	.LBB28_9
.LBB28_11:                              # %if.end34
	movl	reload_in_progress(%rip), %eax
	movq	%rbx, %rdi
	testq	%rax, %rax
	je	.LBB28_18
# BB#12:                                # %if.then35
	movq	%r15, %rsi
	callq	gen_lowpart_common
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB28_15
# BB#13:                                # %land.lhs.true38
	movzbl	(%r15), %eax
	xorq	%r12, %r12
	cmpq	$66, %rax
	jne	.LBB28_15
# BB#14:                                # %if.then43
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	adjust_address_1
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	copy_replacements
.LBB28_15:                              # %if.end45
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_lowpart_common
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB28_19
# BB#16:                                # %land.lhs.true49
	movzbl	(%r14), %eax
	xorq	%r15, %r15
	cmpq	$66, %rax
	jne	.LBB28_19
# BB#17:                                # %if.then54
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	adjust_address_1
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	copy_replacements
	jmp	.LBB28_19
.LBB28_18:                              # %if.else57
	movq	%r15, %rsi
	callq	gen_lowpart
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	gen_lowpart
	movq	%rax, %r15
.LBB28_19:                              # %if.end60
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	optab_table+240(%rip), %rax
	sarq	%cl, %rbx
	movq	$4, %rcx
	shlq	%cl, %rbx
	movslq	8(%rax,%rbx), %rax
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmpq	*insn_data+16(,%rcx,8)  # TAILCALL
.LBB28_9:                               # %if.then33
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.gen_move_insn, %rcx
	movq	$3972, %rsi             # imm = 0xF84
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp381:
	.size	gen_move_insn, .Ltmp381-gen_move_insn
	.cfi_endproc

	.globl	can_extend_p
	.align	16, 0x90
	.type	can_extend_p,@function
can_extend_p:                           # @can_extend_p
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB29_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB29_3
.LBB29_1:
	movabsq	$0, %rax
.LBB29_3:                               # %entry
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$472, %rdi, %rcx        # imm = 0x1D8
	leaq	extendtab(%rcx,%rsi,8), %rcx
	movl	(%rcx,%rax,4), %eax
	retq
.Ltmp382:
	.size	can_extend_p, .Ltmp382-can_extend_p
	.cfi_endproc

	.globl	gen_extend_insn
	.align	16, 0x90
	.type	gen_extend_insn,@function
gen_extend_insn:                        # @gen_extend_insn
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r8
	je	.LBB30_1
# BB#2:                                 # %entry
	movabsq	$1, %r8
	jmp	.LBB30_3
.LBB30_1:
	movabsq	$0, %r8
.LBB30_3:                               # %entry
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$472, %rdx, %rcx        # imm = 0x1D8
	leaq	extendtab(%rcx,%rax,8), %rax
	movslq	(%rax,%r8,4), %rax
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	jmpq	*insn_data+16(,%rcx,8)  # TAILCALL
.Ltmp383:
	.size	gen_extend_insn, .Ltmp383-gen_extend_insn
	.cfi_endproc

	.globl	expand_float
	.align	16, 0x90
	.type	expand_float,@function
expand_float:                           # @expand_float
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp390:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp391:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp392:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp393:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp394:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp395:
	.cfi_def_cfa_offset 144
.Ltmp396:
	.cfi_offset %rbx, -48
.Ltmp397:
	.cfi_offset %r12, -40
.Ltmp398:
	.cfi_offset %r13, -32
.Ltmp399:
	.cfi_offset %r14, -24
.Ltmp400:
	.cfi_offset %r15, -16
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movl	(%rsi), %r8d
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	je	.LBB31_11
# BB#1:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rdx
	je	.LBB31_2
# BB#3:                                 # %for.body.lr.ph
	movabsq	$1, %rax
	jmp	.LBB31_4
.LBB31_2:
	movabsq	$0, %rax
.LBB31_4:                               # %for.body.lr.ph
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB31_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB31_6 Depth 2
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movl	(%rdi), %eax
	movq	$16, %rcx
	movq	%rax, %r14
	shrq	%cl, %r14
	movq	%rsi, %r13
	andq	%r9, %r13
	movq	%r13, 56(%rsp)          # 8-byte Spill
	andq	$255, %r14
	je	.LBB31_28
	jmp	.LBB31_6
	.align	16, 0x90
.LBB31_27:                              # %for.inc.for.body10_crit_edge
                                        #   in Loop: Header=BB31_6 Depth=2
	movl	(%rdi), %eax
.LBB31_6:                               # %for.body10
                                        #   Parent Loop BB31_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r14, %rbx
	andq	%r9, %rbx
	cmpq	%rax, %rbx
	je	.LBB31_9
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB31_6 Depth=2
	movq	%rdx, %r13
	movq	%rdi, %r15
	movq	%r14, %rdi
	movq	%r9, %r12
	callq	significand_size
	movq	%r12, %r9
	movq	48(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %r8d
	movq	$15, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	andq	$510, %rdx              # imm = 0x1FE
	movzbl	mode_bitsize(%rdx), %ecx
	andq	%r9, %rax
	movq	%r15, %rdi
	movq	%r13, %rdx
	cmpq	%rcx, %rax
	jae	.LBB31_8
# BB#25:                                # %land.lhs.true.for.inc_crit_edge
                                        #   in Loop: Header=BB31_6 Depth=2
	movq	56(%rsp), %r13          # 8-byte Reload
	jmp	.LBB31_26
	.align	16, 0x90
.LBB31_8:                               #   in Loop: Header=BB31_6 Depth=2
	movq	56(%rsp), %r13          # 8-byte Reload
.LBB31_9:                               # %if.end21
                                        #   in Loop: Header=BB31_6 Depth=2
	movq	%r14, %rbx
	andq	%r9, %rbx
	imulq	$472, %rbx, %rax        # imm = 0x1D8
	leaq	floattab(%rax,%r13,8), %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movl	(%rcx,%rsi,4), %r15d
	cmpq	$1317, %r15             # imm = 0x525
	jne	.LBB31_10
# BB#13:                                # %land.lhs.true25
                                        #   in Loop: Header=BB31_6 Depth=2
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%rsi, %r13
	je	.LBB31_26
# BB#14:                                # %land.lhs.true25
                                        #   in Loop: Header=BB31_6 Depth=2
	testq	%r9, %rdx
	je	.LBB31_26
# BB#15:                                # %if.end34
                                        #   in Loop: Header=BB31_6 Depth=2
	movl	floattab(%rax,%r13,8), %r15d
	xorq	%r12, %r12
	cmpq	$1317, %r15             # imm = 0x525
	jne	.LBB31_16
.LBB31_26:                              # %for.inc
                                        #   in Loop: Header=BB31_6 Depth=2
	movzbl	mode_wider_mode(%rbx), %r14d
	testq	%r14, %r14
	jne	.LBB31_27
.LBB31_28:                              # %for.inc65
                                        #   in Loop: Header=BB31_5 Depth=1
	movzbl	mode_wider_mode(%r13), %esi
	testq	%rsi, %rsi
	jne	.LBB31_5
# BB#29:                                # %for.end69
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	testq	%r9, %rdx
	movq	%r9, %rbx
	je	.LBB31_54
# BB#30:                                # %if.then71
	callq	gen_label_rtx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	callq	emit_queue
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %r13
	testq	%rcx, %rcx
	je	.LBB31_32
# BB#31:                                # %if.then76
	movq	%r13, %rdi
	callq	force_not_mem
	movq	%rax, %r13
.LBB31_32:                              # %if.end78
	movq	%rbx, %r8
	movl	(%r12), %eax
	movq	$16, %rcx
	movq	%rax, %r14
	shrq	%cl, %r14
	andq	$255, %r14
	je	.LBB31_38
# BB#33:                                # %for.body85.lr.ph
	movzbl	2(%r13), %ecx
	movzbl	mode_bitsize(%rcx,%rcx), %edx
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	%r14, %rsi
	.align	16, 0x90
.LBB31_34:                              # %for.body85
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	andq	%r8, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %ebx
	cmpq	%rbx, %rdx
	jae	.LBB31_37
# BB#35:                                # %land.lhs.true97
                                        #   in Loop: Header=BB31_34 Depth=1
	imulq	$472, %rdi, %rbx        # imm = 0x1D8
	movl	floattab(%rbx,%rcx,8), %ebx
	cmpq	$1317, %rbx             # imm = 0x525
	jne	.LBB31_36
.LBB31_37:                              # %for.inc106
                                        #   in Loop: Header=BB31_34 Depth=1
	movzbl	mode_wider_mode(%rdi), %esi
	testq	%rsi, %rsi
	jne	.LBB31_34
.LBB31_38:                              # %if.then113
	movq	%r14, %rdi
	movq	%r8, %rbx
	callq	significand_size
	movq	%rbx, %r8
	movzbl	2(%r13), %ecx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	incq	%rax
	andq	%r8, %rax
	cmpq	%rcx, %rax
	jae	.LBB31_39
# BB#47:                                # %if.then126
	movq	%r8, %r15
	movq	%r12, 48(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	64(%rsp), %rsi          # 8-byte Reload
	movl	(%rsi), %edx
	movq	%rax, %rbx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB31_50
# BB#48:                                # %lor.lhs.false
	movl	8(%rsi), %eax
	cmpq	$53, %rax
	jb	.LBB31_50
# BB#49:                                # %lor.lhs.false135
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	andq	%r14, %r15
	cmpq	%r15, %rdx
	je	.LBB31_51
.LBB31_50:                              # %if.then141
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rsi
.LBB31_51:                              # %if.end143
	movzbl	2(%r13), %r12d
	movq	%rsi, %r15
	callq	do_pending_stack_adjust
	movq	const_int_rtx+512(%rip), %rsi
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	%r13, %rdi
	movq	%r12, %r8
	callq	emit_cmp_and_jump_insns
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	expand_float
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%rbx, %rdi
	callq	emit_label
	movq	optab_table+144(%rip), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	xorq	%r8, %r8
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%r13, %rdx
	callq	expand_binop
	movq	global_trees+96(%rip), %rcx
	movq	%rax, %rbx
	movq	$83, %rdi
	xorq	%r8, %r8
	movq	$1, %r9
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	expand_shift
	movq	optab_table+152(%rip), %rsi
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	expand_float
	movq	optab_table(%rip), %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%r15, %rcx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB31_53
# BB#52:                                # %if.then157
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB31_53:                              # %if.end159
	callq	do_pending_stack_adjust
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movq	48(%rsp), %r12          # 8-byte Reload
	jmp	.LBB31_90
.LBB31_10:
	movq	%rdx, %r12
.LBB31_16:                              # %if.then37
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	$1, %rsi
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	callq	protect_from_queue
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %rdx
	movzbl	2(%rdx), %eax
	cmpq	%rax, %r13
	je	.LBB31_18
# BB#17:                                # %if.then45
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	convert_to_mode
	movq	%rax, %rdx
.LBB31_18:                              # %if.end47
	movq	32(%rsp), %r13          # 8-byte Reload
	movzbl	2(%r13), %eax
	movq	%r14, %rcx
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	movq	64(%rsp), %rsi          # 8-byte Reload
	je	.LBB31_20
# BB#19:                                # %if.then53
	movq	%r14, %rdi
	movq	%rdx, %r14
	callq	gen_reg_rtx
	movq	%r14, %rdx
	movq	%rax, %rsi
.LBB31_20:                              # %if.end55
	testq	%rbx, %r12
	je	.LBB31_21
# BB#22:                                # %if.end55
	movabsq	$1, %rax
	jmp	.LBB31_23
.LBB31_21:
	movabsq	$0, %rax
.LBB31_23:                              # %if.end55
	leaq	125(%rax,%rax), %rcx
	movq	%r15, %rdi
	movq	%rsi, %rbx
	callq	emit_unop_insn
	movq	%rbx, %rsi
	cmpq	%r13, %rsi
	je	.LBB31_95
# BB#24:                                # %if.then59
	xorq	%rdx, %rdx
	movq	%r13, %rdi
.LBB31_94:                              # %if.end375
	callq	convert_move
.LBB31_95:                              # %if.end375
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB31_54:                              # %if.end199
	movq	$1, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r15
	movzbl	2(%r15), %eax
	movzbl	mode_size(%rax), %eax
	movzbl	mode_size+4(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB31_56
# BB#55:                                # %if.then214
	movq	$4, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	convert_to_mode
	movq	%rax, %r15
.LBB31_56:                              # %if.end216
	movl	flag_force_mem(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_58
# BB#57:                                # %if.then218
	movq	%r15, %rdi
	callq	force_not_mem
	movq	%rax, %r15
.LBB31_58:                              # %if.end220
	movzbl	2(%r14), %eax
	addq	$-15, %rax
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB31_87
# BB#59:                                # %if.end220
	jmpq	*.LJTI31_0(,%rax,8)
.LBB31_60:                              # %if.then226
	movzbl	2(%r15), %eax
	cmpq	$4, %rax
	je	.LBB31_61
# BB#62:                                # %if.then226
	cmpq	$6, %rax
	jne	.LBB31_63
# BB#88:                                # %if.then245
	movabsq	$libfunc_table+488, %rax
	jmp	.LBB31_89
.LBB31_39:                              # %if.then113.if.end162_crit_edge
	movl	(%r12), %eax
	jmp	.LBB31_40
.LBB31_36:
	movq	%rsi, %r14
.LBB31_40:                              # %if.end162
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	andq	%r14, %r8
	cmpq	%r8, %rdx
	jne	.LBB31_43
# BB#41:                                # %if.end162
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB31_43
# BB#42:                                # %lor.lhs.false173
	movl	8(%r12), %eax
	cmpq	$52, %rax
	ja	.LBB31_44
.LBB31_43:                              # %if.then179
	movq	%r14, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rdi
.LBB31_44:                              # %if.end181
	xorq	%rdx, %rdx
	movq	%r13, %rsi
	movq	%rdi, %r15
	callq	expand_float
	callq	do_pending_stack_adjust
	movq	const_int_rtx+512(%rip), %rsi
	movzbl	2(%r13), %r8d
	movq	$104, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	callq	emit_cmp_and_jump_insns
	movzbl	2(%r13), %eax
	movzbl	mode_bitsize(%rax,%rax), %esi
	movq	dconst1+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst1(%rip), %rax
	leaq	72(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_ldexp
	movq	optab_table(%rip), %rbx
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	immed_real_const_1
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	movq	%r15, %r8
	callq	expand_binop
	cmpq	%r15, %rax
	je	.LBB31_46
# BB#45:                                # %if.then195
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB31_46:                              # %if.end197
	callq	do_pending_stack_adjust
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	jmp	.LBB31_90
.LBB31_66:                              # %if.then256
	movzbl	2(%r15), %eax
	cmpq	$4, %rax
	je	.LBB31_67
# BB#68:                                # %if.then256
	cmpq	$6, %rax
	jne	.LBB31_69
# BB#71:                                # %if.then276
	movabsq	$libfunc_table+512, %rax
	jmp	.LBB31_89
.LBB31_73:                              # %if.then287
	movzbl	2(%r15), %eax
	cmpq	$4, %rax
	je	.LBB31_74
# BB#75:                                # %if.then287
	cmpq	$6, %rax
	jne	.LBB31_76
# BB#78:                                # %if.then307
	movabsq	$libfunc_table+536, %rax
	jmp	.LBB31_89
.LBB31_80:                              # %if.then318
	movzbl	2(%r15), %eax
	cmpq	$4, %rax
	je	.LBB31_81
# BB#82:                                # %if.then318
	cmpq	$6, %rax
	jne	.LBB31_83
# BB#85:                                # %if.then338
	movabsq	$libfunc_table+560, %rax
	jmp	.LBB31_89
.LBB31_61:
	movabsq	$libfunc_table+472, %rax
	jmp	.LBB31_89
.LBB31_63:                              # %if.then226
	cmpq	$5, %rax
	jne	.LBB31_65
# BB#64:                                # %if.then238
	movabsq	$libfunc_table+480, %rax
	jmp	.LBB31_89
.LBB31_67:
	movabsq	$libfunc_table+496, %rax
	jmp	.LBB31_89
.LBB31_69:                              # %if.then256
	cmpq	$5, %rax
	jne	.LBB31_72
# BB#70:                                # %if.then269
	movabsq	$libfunc_table+504, %rax
	jmp	.LBB31_89
.LBB31_74:
	movabsq	$libfunc_table+520, %rax
	jmp	.LBB31_89
.LBB31_76:                              # %if.then287
	cmpq	$5, %rax
	jne	.LBB31_79
# BB#77:                                # %if.then300
	movabsq	$libfunc_table+528, %rax
	jmp	.LBB31_89
.LBB31_81:
	movabsq	$libfunc_table+544, %rax
	jmp	.LBB31_89
.LBB31_83:                              # %if.then318
	cmpq	$5, %rax
	jne	.LBB31_86
# BB#84:                                # %if.then331
	movabsq	$libfunc_table+552, %rax
.LBB31_89:                              # %if.end347
	movq	(%rax), %rbx
	callq	start_sequence
	movzbl	2(%r14), %ecx
	movzbl	2(%r15), %edi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	%rdi, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %r9
	callq	emit_library_call_value
	movq	%rax, %r12
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movzbl	2(%r14), %esi
	movq	$125, %rdi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_e
	movq	%rbx, %rdi
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r14, %r12
	movq	%rax, %rcx
	callq	emit_libcall_block
.LBB31_90:                              # %done
	cmpq	%r12, %r15
	movq	%r15, %rsi
	je	.LBB31_95
# BB#91:                                # %if.then362
	movl	(%rsi), %eax
	movl	(%r12), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB31_92
# BB#93:                                # %if.else373
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	jmp	.LBB31_94
.LBB31_92:                              # %if.then371
	movq	%r12, %rdi
	callq	emit_move_insn
	jmp	.LBB31_95
.LBB31_87:                              # %if.else343
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.expand_float, %rdx
	movq	$4317, %rsi             # imm = 0x10DD
	callq	fancy_abort
.LBB31_11:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_float, %rcx
	movq	$4096, %rsi             # imm = 0x1000
	jmp	.LBB31_12
.LBB31_65:                              # %if.else246
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_float, %rcx
	movq	$4281, %rsi             # imm = 0x10B9
	jmp	.LBB31_12
.LBB31_72:                              # %if.else277
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_float, %rcx
	movq	$4292, %rsi             # imm = 0x10C4
	jmp	.LBB31_12
.LBB31_79:                              # %if.else308
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_float, %rcx
	movq	$4303, %rsi             # imm = 0x10CF
	jmp	.LBB31_12
.LBB31_86:                              # %if.else339
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_float, %rcx
	movq	$4314, %rsi             # imm = 0x10DA
.LBB31_12:                              # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp401:
	.size	expand_float, .Ltmp401-expand_float
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI31_0:
	.quad	.LBB31_60
	.quad	.LBB31_66
	.quad	.LBB31_73
	.quad	.LBB31_80

	.text
	.globl	expand_fix
	.align	16, 0x90
	.type	expand_fix,@function
expand_fix:                             # @expand_fix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp408:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp409:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp410:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp411:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp412:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp413:
	.cfi_def_cfa_offset 144
.Ltmp414:
	.cfi_offset %rbx, -48
.Ltmp415:
	.cfi_offset %r12, -40
.Ltmp416:
	.cfi_offset %r13, -32
.Ltmp417:
	.cfi_offset %r14, -24
.Ltmp418:
	.cfi_offset %r15, -16
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movzbl	2(%rsi), %r9d
	testq	%r9, %r9
	je	.LBB32_31
# BB#1:                                 # %for.body.lr.ph
	movzbl	2(%rdi), %r10d
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB32_2
# BB#3:                                 # %for.body.lr.ph
	movabsq	$1, %rsi
	jmp	.LBB32_4
.LBB32_2:
	movabsq	$0, %rsi
.LBB32_4:                               # %for.body.lr.ph
	movq	optab_table+136(%rip), %r8
	movq	%r9, %r15
.LBB32_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB32_7 Depth 2
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	testq	%r10, %r10
	je	.LBB32_30
# BB#6:                                 # %for.body6.lr.ph
                                        #   in Loop: Header=BB32_5 Depth=1
	movq	$4, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	leaq	8(%r8,%rax), %r11
	movq	%r10, %r13
	.align	16, 0x90
.LBB32_7:                               # %for.body6
                                        #   Parent Loop BB32_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	imulq	$472, %r15, %rcx        # imm = 0x1D8
	leaq	fixtrunctab(%rcx,%r12,8), %rax
	movl	(%rax,%rsi,4), %ebx
	xorq	%r14, %r14
	cmpq	$1317, %rbx             # imm = 0x525
	jne	.LBB32_8
# BB#9:                                 # %if.end.i
                                        #   in Loop: Header=BB32_7 Depth=2
	movl	(%r11), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB32_12
# BB#10:                                # %can_fix_p.exit
                                        #   in Loop: Header=BB32_7 Depth=2
	leaq	fixtab(%rcx,%r12,8), %rbx
	movl	(%rbx,%rsi,4), %ebx
	cmpq	$1317, %rbx             # imm = 0x525
	movq	$1, %r14
	jne	.LBB32_11
.LBB32_12:                              # %land.lhs.true
                                        #   in Loop: Header=BB32_7 Depth=2
	cmpq	%r10, %r12
	je	.LBB32_29
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB32_7 Depth=2
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rdx
	je	.LBB32_29
# BB#14:                                # %if.then
                                        #   in Loop: Header=BB32_7 Depth=2
	movl	fixtrunctab(%rcx,%r12,8), %ebx
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	cmpq	$1317, %rbx             # imm = 0x525
	jne	.LBB32_15
# BB#16:                                # %if.end.i530
                                        #   in Loop: Header=BB32_7 Depth=2
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB32_29
# BB#17:                                # %if.end
                                        #   in Loop: Header=BB32_7 Depth=2
	movl	fixtab(%rcx,%r12,8), %ebx
	cmpq	$1317, %rbx             # imm = 0x525
	movq	$1, %r14
	jne	.LBB32_18
.LBB32_29:                              # %for.inc
                                        #   in Loop: Header=BB32_7 Depth=2
	movzbl	mode_wider_mode(%r12), %r13d
	testq	%r13, %r13
	jne	.LBB32_7
.LBB32_30:                              # %for.inc41
                                        #   in Loop: Header=BB32_5 Depth=1
	movzbl	mode_wider_mode(%r15), %r15d
	testq	%r15, %r15
	jne	.LBB32_5
.LBB32_31:                              # %for.end45
	movl	(%rdi), %r8d
	movq	%rdi, %r13
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	testq	%r10, %rdx
	je	.LBB32_48
# BB#32:                                # %land.lhs.true47
	movq	$16, %rcx
	movq	%r8, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %r12d
	cmpq	$64, %r12
	ja	.LBB32_48
# BB#33:                                # %land.lhs.true47
	testq	%r9, %r9
	je	.LBB32_48
# BB#34:                                # %for.body63.lr.ph
	movq	optab_table+136(%rip), %rdi
	movq	%r9, %r15
	.align	16, 0x90
.LBB32_35:                              # %for.body63
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %r14
	andq	%r10, %r14
	movzbl	mode_bitsize(%r14,%r14), %eax
	cmpq	%r12, %rax
	jbe	.LBB32_47
# BB#36:                                # %land.lhs.true75
                                        #   in Loop: Header=BB32_35 Depth=1
	imulq	$472, %r14, %rax        # imm = 0x1D8
	movl	fixtrunctab(%rax,%rsi,8), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	jne	.LBB32_39
# BB#37:                                # %if.end.i541
                                        #   in Loop: Header=BB32_35 Depth=1
	movq	$4, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movl	8(%rdi,%rbx), %ecx
	cmpq	$1317, %rcx             # imm = 0x525
	je	.LBB32_47
# BB#38:                                # %can_fix_p.exit545
                                        #   in Loop: Header=BB32_35 Depth=1
	movl	fixtab(%rax,%rsi,8), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB32_39
.LBB32_47:                              # %for.inc150
                                        #   in Loop: Header=BB32_35 Depth=1
	movzbl	mode_wider_mode(%r14), %r15d
	testq	%r15, %r15
	jne	.LBB32_35
.LBB32_48:                              # %if.end155
	movq	%rdx, %r14
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movzbl	mode_size(%r8), %eax
	movzbl	mode_size+4(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB32_50
# BB#49:                                # %if.end352.thread
	movq	$4, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	expand_fix
	movq	%r13, %r12
	jmp	.LBB32_113
.LBB32_8:
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	jmp	.LBB32_18
.LBB32_50:                              # %if.else
	addq	$-15, %r9
	andq	%r10, %r9
	cmpq	$3, %r9
	ja	.LBB32_110
# BB#51:                                # %if.else
	movq	%r13, %rdi
	jmpq	*.LJTI32_0(,%r9,8)
.LBB32_52:                              # %if.then172
	cmpq	$6, %r8
	je	.LBB32_66
# BB#53:                                # %if.then172
	cmpq	$5, %r8
	jne	.LBB32_54
# BB#63:                                # %if.then187
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_64
# BB#65:                                # %if.then187
	movabsq	$libfunc_table+576, %rax
	jmp	.LBB32_58
.LBB32_11:
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	jmp	.LBB32_18
.LBB32_15:
	xorq	%r14, %r14
.LBB32_18:                              # %if.then15
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	callq	protect_from_queue
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movq	%rax, %r8
	movzbl	2(%r8), %eax
	cmpq	%rax, %r15
	je	.LBB32_20
# BB#19:                                # %if.then22
	xorq	%rdx, %rdx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %rsi
	callq	convert_to_mode
	movq	%rax, %r8
.LBB32_20:                              # %if.end24
	testq	%r14, %r14
	je	.LBB32_22
# BB#21:                                # %if.then26
	movzbl	2(%r8), %edi
	movq	%r8, %r14
	callq	gen_reg_rtx
	movq	optab_table+136(%rip), %rsi
	movzbl	2(%r14), %edi
	xorq	%r8, %r8
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	expand_unop
	movq	%rax, %r8
.LBB32_22:                              # %if.end28
	movq	32(%rsp), %r14          # 8-byte Reload
	movzbl	2(%r14), %eax
	cmpq	%rax, %r12
	movq	40(%rsp), %rsi          # 8-byte Reload
	je	.LBB32_24
# BB#23:                                # %if.then33
	movq	%r13, %rdi
	movq	%r8, %r15
	callq	gen_reg_rtx
	movq	%r15, %r8
	movq	%rax, %rsi
.LBB32_24:                              # %if.end35
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB32_25
# BB#26:                                # %if.end35
	movabsq	$1, %rax
	jmp	.LBB32_27
.LBB32_25:
	movabsq	$0, %rax
.LBB32_27:                              # %if.end35
	leaq	126(%rax,%rax), %rcx
	movq	%rbx, %rdi
	movq	%r8, %rdx
	movq	%rsi, %rbx
	callq	emit_unop_insn
	movq	%rbx, %rsi
	cmpq	%r14, %rsi
	je	.LBB32_118
# BB#28:                                # %if.then38
	movq	%r14, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB32_117
.LBB32_39:                              # %if.then82
	decq	%r12
	movq	dconst1+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst1(%rip), %rax
	leaq	72(%rsp), %rdi
	movq	%rax, (%rsp)
	movq	%r12, %rsi
	callq	ereal_ldexp
	movq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	72(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	callq	immed_real_const_1
	movq	%rax, 56(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	callq	emit_queue
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	protect_from_queue
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %r13
	testq	%rcx, %rcx
	je	.LBB32_41
# BB#40:                                # %if.then95
	movq	%r13, %rdi
	callq	force_not_mem
	movq	%rax, %r13
.LBB32_41:                              # %if.end97
	movzbl	2(%r13), %eax
	cmpq	%rax, %r14
	je	.LBB32_43
# BB#42:                                # %if.then103
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	convert_to_mode
	movq	%rax, %r13
.LBB32_43:                              # %if.end105
	callq	do_pending_stack_adjust
	movzbl	2(%r13), %r8d
	movq	$104, %rdx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	%r15, (%rsp)
	movq	%r13, %rdi
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	emit_cmp_and_jump_insns
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	expand_fix
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r15, %rdi
	callq	emit_label
	movq	optab_table+16(%rip), %rsi
	movzbl	2(%r13), %edi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	expand_binop
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_fix
	movq	optab_table+160(%rip), %r14
	movq	%rbx, %r15
	movzbl	2(%r15), %ebx
	movq	$1, %rdi
	movq	%r12, %rcx
	shlq	%cl, %rdi
	movq	%rbx, %rsi
	callq	trunc_int_for_mode
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	$1, %r9
	movq	$3, (%rsp)
	movq	%rbx, %rdi
	movq	%r15, %rbx
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	expand_binop
	cmpq	%rbx, %rax
	je	.LBB32_45
# BB#44:                                # %if.then129
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB32_45:                              # %if.end131
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	emit_label
	movzbl	2(%rbx), %eax
	movq	optab_table+240(%rip), %rdx
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	8(%rdx,%rax), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB32_118
# BB#46:                                # %if.then140
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movzbl	2(%rbx), %ebx
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	copy_rtx
	movq	$128, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$4, %rsi
	movq	%r14, %rdi
	movq	%rax, %rdx
	callq	set_unique_reg_note
	jmp	.LBB32_118
.LBB32_71:                              # %if.then215
	cmpq	$6, %r8
	je	.LBB32_80
# BB#72:                                # %if.then215
	cmpq	$5, %r8
	jne	.LBB32_73
# BB#77:                                # %if.then233
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_78
# BB#79:                                # %if.then233
	movabsq	$libfunc_table+600, %rax
	jmp	.LBB32_58
.LBB32_84:                              # %if.then261
	cmpq	$6, %r8
	je	.LBB32_93
# BB#85:                                # %if.then261
	cmpq	$5, %r8
	jne	.LBB32_86
# BB#90:                                # %if.then279
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_91
# BB#92:                                # %if.then279
	movabsq	$libfunc_table+624, %rax
	jmp	.LBB32_58
.LBB32_97:                              # %if.then307
	cmpq	$6, %r8
	je	.LBB32_106
# BB#98:                                # %if.then307
	cmpq	$5, %r8
	jne	.LBB32_99
# BB#103:                               # %if.then325
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_104
# BB#105:                               # %if.then325
	movabsq	$libfunc_table+648, %rax
	jmp	.LBB32_58
.LBB32_66:                              # %if.then199
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_67
# BB#68:                                # %if.then199
	movabsq	$libfunc_table+584, %rax
	jmp	.LBB32_58
.LBB32_54:                              # %if.then172
	cmpq	$4, %r8
	jne	.LBB32_69
# BB#55:                                # %if.then178
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_56
# BB#57:                                # %if.then178
	movabsq	$libfunc_table+568, %rax
	jmp	.LBB32_58
.LBB32_80:                              # %if.then245
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_81
# BB#82:                                # %if.then245
	movabsq	$libfunc_table+608, %rax
	jmp	.LBB32_58
.LBB32_73:                              # %if.then215
	cmpq	$4, %r8
	jne	.LBB32_83
# BB#74:                                # %if.then221
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_75
# BB#76:                                # %if.then221
	movabsq	$libfunc_table+592, %rax
	jmp	.LBB32_58
.LBB32_93:                              # %if.then291
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_94
# BB#95:                                # %if.then291
	movabsq	$libfunc_table+632, %rax
	jmp	.LBB32_58
.LBB32_86:                              # %if.then261
	cmpq	$4, %r8
	jne	.LBB32_96
# BB#87:                                # %if.then267
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_88
# BB#89:                                # %if.then267
	movabsq	$libfunc_table+616, %rax
	jmp	.LBB32_58
.LBB32_106:                             # %if.then337
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_107
# BB#108:                               # %if.then337
	movabsq	$libfunc_table+656, %rax
	jmp	.LBB32_58
.LBB32_99:                              # %if.then307
	cmpq	$4, %r8
	jne	.LBB32_109
# BB#100:                               # %if.then313
	movq	%r14, %rbx
	testq	%r10, %r14
	jne	.LBB32_101
# BB#102:                               # %if.then313
	movabsq	$libfunc_table+640, %rax
	jmp	.LBB32_58
.LBB32_110:                             # %if.else347
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.expand_fix, %rdx
	movq	$4542, %rsi             # imm = 0x11BE
	callq	fancy_abort
.LBB32_64:
	movabsq	$libfunc_table+672, %rax
	jmp	.LBB32_58
.LBB32_78:
	movabsq	$libfunc_table+696, %rax
	jmp	.LBB32_58
.LBB32_91:
	movabsq	$libfunc_table+720, %rax
	jmp	.LBB32_58
.LBB32_104:
	movabsq	$libfunc_table+744, %rax
	jmp	.LBB32_58
.LBB32_67:
	movabsq	$libfunc_table+680, %rax
	jmp	.LBB32_58
.LBB32_56:
	movabsq	$libfunc_table+664, %rax
	jmp	.LBB32_58
.LBB32_81:
	movabsq	$libfunc_table+704, %rax
	jmp	.LBB32_58
.LBB32_75:
	movabsq	$libfunc_table+688, %rax
	jmp	.LBB32_58
.LBB32_94:
	movabsq	$libfunc_table+728, %rax
	jmp	.LBB32_58
.LBB32_88:
	movabsq	$libfunc_table+712, %rax
	jmp	.LBB32_58
.LBB32_107:
	movabsq	$libfunc_table+752, %rax
	jmp	.LBB32_58
.LBB32_101:
	movabsq	$libfunc_table+736, %rax
.LBB32_58:                              # %if.then178
	movq	%r10, %r13
	movq	(%rax), %r15
	testq	%r15, %r15
	je	.LBB32_118
# BB#59:                                # %if.then354
	movq	$1, %rsi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	callq	protect_from_queue
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	protect_from_queue
	movl	flag_force_mem(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB32_61
# BB#60:                                # %if.then360
	movq	%r14, %rdi
	callq	force_not_mem
	movq	%rax, %r14
.LBB32_61:                              # %if.end362
	testq	%r13, %rbx
	je	.LBB32_62
# BB#111:                               # %if.end362
	movabsq	$1, %r13
	jmp	.LBB32_112
.LBB32_62:
	movabsq	$0, %r13
.LBB32_112:                             # %if.end362
	callq	start_sequence
	movzbl	2(%r12), %ecx
	movzbl	2(%r14), %edi
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %r8
	xorq	%rax, %rax
	movq	%rdi, (%rsp)
	movq	%r15, %rdi
	movq	%r14, %r9
	callq	emit_library_call_value
	movq	%rax, %r15
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movzbl	2(%r12), %esi
	leaq	126(%r13,%r13), %rdi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_e
	movq	%rbx, %rdi
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	emit_libcall_block
.LBB32_113:                             # %if.end377
	cmpq	%r12, %rbx
	je	.LBB32_118
# BB#114:                               # %if.then380
	movl	(%r12), %eax
	movl	(%rbx), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	je	.LBB32_115
# BB#116:                               # %if.else391
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
.LBB32_117:                             # %if.end393
	callq	convert_move
.LBB32_118:                             # %if.end393
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_115:                             # %if.then389
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	jmp	.LBB32_118
.LBB32_69:                              # %if.else205
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_fix, %rcx
	movq	$4506, %rsi             # imm = 0x119A
	jmp	.LBB32_70
.LBB32_83:                              # %if.else251
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_fix, %rcx
	movq	$4517, %rsi             # imm = 0x11A5
	jmp	.LBB32_70
.LBB32_96:                              # %if.else297
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_fix, %rcx
	movq	$4528, %rsi             # imm = 0x11B0
	jmp	.LBB32_70
.LBB32_109:                             # %if.else343
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_fix, %rcx
	movq	$4539, %rsi             # imm = 0x11BB
.LBB32_70:                              # %if.else205
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp419:
	.size	expand_fix, .Ltmp419-expand_fix
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI32_0:
	.quad	.LBB32_52
	.quad	.LBB32_71
	.quad	.LBB32_84
	.quad	.LBB32_97

	.text
	.globl	have_insn_for
	.align	16, 0x90
	.type	have_insn_for,@function
have_insn_for:                          # @have_insn_for
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	code_to_optab(,%rdi,8), %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB33_4
# BB#1:                                 # %land.rhs
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movl	8(%rdx,%rsi), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB33_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB33_4:                               # %land.end
	retq
.LBB33_2:
	movabsq	$0, %rax
	retq
.Ltmp420:
	.size	have_insn_for, .Ltmp420-have_insn_for
	.cfi_endproc

	.globl	init_one_libfunc
	.align	16, 0x90
	.type	init_one_libfunc,@function
init_one_libfunc:                       # @init_one_libfunc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp423:
	.cfi_def_cfa_offset 16
.Ltmp424:
	.cfi_offset %rbx, -16
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB34_2
# BB#1:                                 # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB34_2:                               # %cond.end
	movq	8(%rax), %rax
	popq	%rbx
	retq
.Ltmp425:
	.size	init_one_libfunc, .Ltmp425-init_one_libfunc
	.cfi_endproc

	.globl	mark_optab
	.align	16, 0x90
	.type	mark_optab,@function
mark_optab:                             # @mark_optab
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp431:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp432:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp433:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp434:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp435:
	.cfi_def_cfa_offset 48
.Ltmp436:
	.cfi_offset %rbx, -40
.Ltmp437:
	.cfi_offset %r12, -32
.Ltmp438:
	.cfi_offset %r14, -24
.Ltmp439:
	.cfi_offset %r15, -16
	movq	(%rdi), %r14
	movq	$16, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14,%rbx), %r15
	testq	%r15, %r15
	je	.LBB35_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB35_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB35_4:                               # %for.inc
                                        #   in Loop: Header=BB35_1 Depth=1
	addq	$16, %rbx
	cmpq	$960, %rbx              # imm = 0x3C0
	jne	.LBB35_1
# BB#5:                                 # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp440:
	.size	mark_optab, .Ltmp440-mark_optab
	.cfi_endproc

	.globl	init_optabs
	.align	16, 0x90
	.type	init_optabs,@function
init_optabs:                            # @init_optabs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp443:
	.cfi_def_cfa_offset 16
.Ltmp444:
	.cfi_offset %rbx, -16
	xorq	%rcx, %rcx
	movabsq	$fixtab+4, %rbx
	movq	$1317, %rsi             # imm = 0x525
	movabsq	$fixtrunctab+4, %r8
	.align	16, 0x90
.LBB36_1:                               # %for.cond2.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_2 Depth 2
	movq	$59, %rdi
	movq	%rbx, %rdx
	.align	16, 0x90
.LBB36_2:                               # %for.cond7.preheader
                                        #   Parent Loop BB36_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%esi, -4(%rdx)
	movl	%esi, (%rdx)
	addq	$8, %rdx
	decq	%rdi
	jne	.LBB36_2
# BB#3:                                 # %for.inc19
                                        #   in Loop: Header=BB36_1 Depth=1
	incq	%rcx
	addq	$472, %rbx              # imm = 0x1D8
	cmpq	$59, %rcx
	jne	.LBB36_1
# BB#4:
	xorq	%rdx, %rdx
	movq	$1317, %rsi             # imm = 0x525
	movabsq	$floattab+4, %rbx
	.align	16, 0x90
.LBB36_5:                               # %for.cond27.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_6 Depth 2
	movq	$59, %rdi
	movq	%r8, %rax
	.align	16, 0x90
.LBB36_6:                               # %for.cond32.preheader
                                        #   Parent Loop BB36_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%esi, -4(%rax)
	movl	%esi, (%rax)
	addq	$8, %rax
	decq	%rdi
	jne	.LBB36_6
# BB#7:                                 # %for.inc49
                                        #   in Loop: Header=BB36_5 Depth=1
	incq	%rdx
	addq	$472, %r8               # imm = 0x1D8
	cmpq	$59, %rdx
	jne	.LBB36_5
# BB#8:
	xorq	%rdx, %rdx
	movq	$1317, %rsi             # imm = 0x525
	movabsq	$extendtab+4, %rax
	.align	16, 0x90
.LBB36_9:                               # %for.cond57.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_10 Depth 2
	movq	$59, %rdi
	movq	%rbx, %rcx
	.align	16, 0x90
.LBB36_10:                              # %for.cond62.preheader
                                        #   Parent Loop BB36_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%esi, -4(%rcx)
	movl	%esi, (%rcx)
	addq	$8, %rcx
	decq	%rdi
	jne	.LBB36_10
# BB#11:                                # %for.inc79
                                        #   in Loop: Header=BB36_9 Depth=1
	incq	%rdx
	addq	$472, %rbx              # imm = 0x1D8
	cmpq	$59, %rdx
	jne	.LBB36_9
# BB#12:
	xorq	%rcx, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_13:                              # %for.cond87.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_14 Depth 2
	movq	$59, %rsi
	movq	%rax, %rdi
	.align	16, 0x90
.LBB36_14:                              # %for.cond92.preheader
                                        #   Parent Loop BB36_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	%edx, -4(%rdi)
	movl	%edx, (%rdi)
	addq	$8, %rdi
	decq	%rsi
	jne	.LBB36_14
# BB#15:                                # %for.inc109
                                        #   in Loop: Header=BB36_13 Depth=1
	incq	%rcx
	addq	$472, %rax              # imm = 0x1D8
	cmpq	$59, %rcx
	jne	.LBB36_13
# BB#16:
	movq	$-612, %rax             # imm = 0xFFFFFFFFFFFFFD9C
	movq	$1317, %rcx             # imm = 0x525
	.align	16, 0x90
.LBB36_17:                              # %for.body115
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, setcc_gen_code+612(%rax)
	addq	$4, %rax
	jne	.LBB36_17
# BB#18:
	movq	$-236, %rax
	movq	$1317, %rcx             # imm = 0x525
	.align	16, 0x90
.LBB36_19:                              # %for.body124
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, movcc_gen_code+236(%rax)
	addq	$4, %rax
	jne	.LBB36_19
# BB#20:                                # %for.end129
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_21:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_21
# BB#22:                                # %init_optab.exit
	movq	$75, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+600(%rip)
	movq	%rax, optab_table(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_23:                              # %for.body.i.i349
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_23
# BB#24:                                # %init_optabv.exit
	movq	$75, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+8(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_25:                              # %for.body.i.i358
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_25
# BB#26:                                # %init_optab.exit360
	movq	$76, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+608(%rip)
	movq	%rax, optab_table+16(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_27:                              # %for.body.i.i368
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_27
# BB#28:                                # %init_optabv.exit370
	movq	$76, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+24(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_29:                              # %for.body.i.i378
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_29
# BB#30:                                # %init_optab.exit380
	movq	$78, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+624(%rip)
	movq	%rax, optab_table+32(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_31:                              # %for.body.i.i388
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_31
# BB#32:                                # %init_optabv.exit390
	movq	$78, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+40(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_33:                              # %for.body.i.i398
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_33
# BB#34:                                # %init_optab.exit400
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+48(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_35:                              # %for.body.i.i408
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_35
# BB#36:                                # %init_optab.exit410
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+56(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_37:                              # %for.body.i.i418
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_37
# BB#38:                                # %init_optab.exit420
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+64(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_39:                              # %for.body.i.i428
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_39
# BB#40:                                # %init_optab.exit430
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+72(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_41:                              # %for.body.i.i438
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_41
# BB#42:                                # %init_optab.exit440
	movq	$79, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+632(%rip)
	movq	%rax, optab_table+80(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_43:                              # %for.body.i.i448
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_43
# BB#44:                                # %init_optabv.exit450
	movq	$79, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+88(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_45:                              # %for.body.i.i458
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_45
# BB#46:                                # %init_optab.exit460
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+96(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_47:                              # %for.body.i.i468
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_47
# BB#48:                                # %init_optab.exit470
	movq	$81, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+648(%rip)
	movq	%rax, optab_table+104(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_49:                              # %for.body.i.i478
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_49
# BB#50:                                # %init_optab.exit480
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+112(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_51:                              # %for.body.i.i488
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_51
# BB#52:                                # %init_optab.exit490
	movq	$80, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+640(%rip)
	movq	%rax, optab_table+120(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_53:                              # %for.body.i.i498
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_53
# BB#54:                                # %init_optab.exit500
	movq	$82, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+656(%rip)
	movq	%rax, optab_table+128(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_55:                              # %for.body.i.i508
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_55
# BB#56:                                # %init_optab.exit510
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+136(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_57:                              # %for.body.i.i518
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_57
# BB#58:                                # %init_optab.exit520
	movq	$83, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+664(%rip)
	movq	%rax, optab_table+144(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_59:                              # %for.body.i.i528
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_59
# BB#60:                                # %init_optab.exit530
	movq	$84, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+672(%rip)
	movq	%rax, optab_table+152(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_61:                              # %for.body.i.i538
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_61
# BB#62:                                # %init_optab.exit540
	movq	$85, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+680(%rip)
	movq	%rax, optab_table+160(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_63:                              # %for.body.i.i548
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_63
# BB#64:                                # %init_optab.exit550
	movq	$87, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+696(%rip)
	movq	%rax, optab_table+168(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_65:                              # %for.body.i.i558
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_65
# BB#66:                                # %init_optab.exit560
	movq	$89, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+712(%rip)
	movq	%rax, optab_table+184(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_67:                              # %for.body.i.i568
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_67
# BB#68:                                # %init_optab.exit570
	movq	$90, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+720(%rip)
	movq	%rax, optab_table+176(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_69:                              # %for.body.i.i578
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_69
# BB#70:                                # %init_optab.exit580
	movq	$88, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+704(%rip)
	movq	%rax, optab_table+192(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_71:                              # %for.body.i.i588
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_71
# BB#72:                                # %init_optab.exit590
	movq	$91, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+728(%rip)
	movq	%rax, optab_table+200(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_73:                              # %for.body.i.i598
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_73
# BB#74:                                # %init_optab.exit600
	movq	$92, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+736(%rip)
	movq	%rax, optab_table+208(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_75:                              # %for.body.i.i608
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_75
# BB#76:                                # %init_optab.exit610
	movq	$93, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+744(%rip)
	movq	%rax, optab_table+216(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_77:                              # %for.body.i.i618
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_77
# BB#78:                                # %init_optab.exit620
	movq	$94, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+752(%rip)
	movq	%rax, optab_table+224(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_79:                              # %for.body.i.i628
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_79
# BB#80:                                # %init_optab.exit630
	movq	$95, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+760(%rip)
	movq	%rax, optab_table+232(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_81:                              # %for.body.i.i638
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_81
# BB#82:                                # %init_optab.exit640
	movq	$47, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+376(%rip)
	movq	%rax, optab_table+240(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_83:                              # %for.body.i.i648
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_83
# BB#84:                                # %init_optab.exit650
	movq	$64, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+512(%rip)
	movq	%rax, optab_table+248(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_85:                              # %for.body.i.i658
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_85
# BB#86:                                # %init_optab.exit660
	movq	$74, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+592(%rip)
	movq	%rax, optab_table+328(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_87:                              # %for.body.i.i668
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_87
# BB#88:                                # %init_optab.exit670
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+336(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_89:                              # %for.body.i.i678
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_89
# BB#90:                                # %init_optab.exit680
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+344(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_91:                              # %for.body.i.i688
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_91
# BB#92:                                # %init_optab.exit690
	movq	$77, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+616(%rip)
	movq	%rax, optab_table+256(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_93:                              # %for.body.i.i698
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_93
# BB#94:                                # %init_optabv.exit700
	movq	$77, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+264(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_95:                              # %for.body.i.i708
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_95
# BB#96:                                # %init_optab.exit710
	movq	$129, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+1032(%rip)
	movq	%rax, optab_table+272(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_97:                              # %for.body.i.i718
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_97
# BB#98:                                # %init_optabv.exit720
	movq	$129, %rcx
	movl	%ecx, (%rax)
	movq	%rax, optab_table+280(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_99:                              # %for.body.i.i728
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_99
# BB#100:                               # %init_optab.exit730
	movq	$86, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+688(%rip)
	movq	%rax, optab_table+288(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_101:                             # %for.body.i.i738
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_101
# BB#102:                               # %init_optab.exit740
	movq	$131, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+1048(%rip)
	movq	%rax, optab_table+296(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_103:                             # %for.body.i.i748
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_103
# BB#104:                               # %init_optab.exit750
	movq	$130, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab+1040(%rip)
	movq	%rax, optab_table+304(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_105:                             # %for.body.i.i758
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_105
# BB#106:                               # %init_optab.exit760
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+312(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_107:                             # %for.body.i.i768
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_107
# BB#108:                               # %init_optab.exit770
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+320(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_109:                             # %for.body.i.i778
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_109
# BB#110:                               # %init_optab.exit780
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+352(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_111:                             # %for.body.i.i788
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_111
# BB#112:                               # %init_optab.exit790
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+360(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_113:                             # %for.body.i.i798
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_113
# BB#114:                               # %init_optab.exit800
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+368(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_115:                             # %for.body.i.i808
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_115
# BB#116:                               # %init_optab.exit810
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+376(%rip)
	movq	$952, %rdi              # imm = 0x3B8
	callq	xmalloc
	movq	$16, %rcx
	movq	$1317, %rdx             # imm = 0x525
	.align	16, 0x90
.LBB36_117:                             # %for.body.i.i818
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -8(%rax,%rcx)
	movq	$0, (%rax,%rcx)
	addq	$16, %rcx
	cmpq	$960, %rcx              # imm = 0x3C0
	jne	.LBB36_117
# BB#118:                               # %init_optab.exit820
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	%rax, code_to_optab(%rip)
	movq	%rax, optab_table+384(%rip)
	movq	$-236, %rax
	movq	$1317, %rcx             # imm = 0x525
	.align	16, 0x90
.LBB36_119:                             # %for.body181
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, movstr_optab+236(%rax)
	movl	%ecx, clrstr_optab+236(%rax)
	movl	%ecx, reload_out_optab+236(%rax)
	movl	%ecx, reload_in_optab+236(%rax)
	addq	$4, %rax
	jne	.LBB36_119
# BB#120:                               # %for.end192
	callq	init_all_optabs
	movq	optab_table(%rip), %rdi
	movabsq	$.L.str1, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+8(%rip), %rdi
	movabsq	$.L.str2, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+8(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+16(%rip), %rdi
	movabsq	$.L.str3, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+16(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+24(%rip), %rdi
	movabsq	$.L.str4, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+24(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+32(%rip), %rdi
	movabsq	$.L.str5, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+32(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+40(%rip), %rdi
	movabsq	$.L.str6, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+40(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+80(%rip), %rdi
	movabsq	$.L.str7, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+80(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+88(%rip), %rdi
	movabsq	$.L.str8, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+104(%rip), %rdi
	movabsq	$.L.str9, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+96(%rip), %rdi
	movabsq	$.L.str10, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$52, %r8
	callq	init_libfuncs
	movq	optab_table+112(%rip), %rdi
	movabsq	$.L.str11, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$52, %r8
	callq	init_libfuncs
	movq	optab_table+120(%rip), %rdi
	movabsq	$.L.str12, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+128(%rip), %rdi
	movabsq	$.L.str13, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+136(%rip), %rdi
	movabsq	$.L.str14, %rcx
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$50, %r8
	callq	init_libfuncs
	movq	optab_table+144(%rip), %rdi
	movabsq	$.L.str15, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+152(%rip), %rdi
	movabsq	$.L.str16, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+160(%rip), %rdi
	movabsq	$.L.str17, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+168(%rip), %rdi
	movabsq	$.L.str18, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+184(%rip), %rdi
	movabsq	$.L.str19, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+176(%rip), %rdi
	movabsq	$.L.str20, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+208(%rip), %rdi
	movabsq	$.L.str21, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+208(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+216(%rip), %rdi
	movabsq	$.L.str22, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+216(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$51, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+224(%rip), %rdi
	movabsq	$.L.str23, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+232(%rip), %rdi
	movabsq	$.L.str24, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$51, %r8
	callq	init_libfuncs
	movq	optab_table+256(%rip), %rdi
	movabsq	$.L.str25, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+256(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$50, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+264(%rip), %rdi
	movabsq	$.L.str26, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	callq	init_libfuncs
	movq	optab_table+264(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$50, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+288(%rip), %rdi
	movabsq	$.L.str27, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	callq	init_libfuncs
	movq	optab_table+296(%rip), %rdi
	movabsq	$.L.str28, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	callq	init_libfuncs
	movq	optab_table+328(%rip), %rdi
	movabsq	$.L.str29, %rbx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movq	optab_table+336(%rip), %rdi
	movabsq	$.L.str30, %rcx
	movq	$4, %rsi
	movq	$6, %rdx
	movq	$50, %r8
	callq	init_libfuncs
	movq	optab_table+328(%rip), %rdi
	movq	$15, %rsi
	movq	$18, %rdx
	movq	$50, %r8
	movq	%rbx, %rcx
	callq	init_libfuncs
	movabsq	$.L.str31, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_122
# BB#121:                               # %cond.false.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_122:                             # %init_one_libfunc.exit
	movq	8(%rax), %rax
	movq	optab_table+272(%rip), %rcx
	movq	%rax, 368(%rcx)
	movabsq	$.L.str28, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_124
# BB#123:                               # %cond.false.i832
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_124:                             # %init_one_libfunc.exit836
	movq	8(%rax), %rbx
	movq	$32, %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	callq	mode_for_size
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	optab_table+296(%rip), %rdx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rbx, 16(%rdx,%rax)
	movabsq	$.L.str32, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_126
# BB#125:                               # %cond.false.i848
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_126:                             # %init_one_libfunc.exit852
	movq	8(%rax), %rax
	movq	%rax, libfunc_table(%rip)
	movabsq	$.L.str33, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_128
# BB#127:                               # %cond.false.i864
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_128:                             # %init_one_libfunc.exit868
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+8(%rip)
	movabsq	$.L.str34, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_130
# BB#129:                               # %cond.false.i880
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_130:                             # %init_one_libfunc.exit884
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+16(%rip)
	movabsq	$.L.str35, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_132
# BB#131:                               # %cond.false.i896
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_132:                             # %init_one_libfunc.exit900
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+24(%rip)
	movabsq	$.L.str36, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_134
# BB#133:                               # %cond.false.i912
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_134:                             # %init_one_libfunc.exit916
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+32(%rip)
	movabsq	$.L.str37, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_136
# BB#135:                               # %cond.false.i928
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_136:                             # %init_one_libfunc.exit932
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+40(%rip)
	movabsq	$.L.str38, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_138
# BB#137:                               # %cond.false.i944
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_138:                             # %init_one_libfunc.exit948
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+48(%rip)
	movabsq	$.L.str39, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_140
# BB#139:                               # %cond.false.i960
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_140:                             # %init_one_libfunc.exit964
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+56(%rip)
	movabsq	$.L.str40, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_142
# BB#141:                               # %cond.false.i976
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_142:                             # %init_one_libfunc.exit980
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+64(%rip)
	movabsq	$.L.str41, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_144
# BB#143:                               # %cond.false.i992
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_144:                             # %init_one_libfunc.exit996
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+72(%rip)
	movabsq	$.L.str42, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_146
# BB#145:                               # %cond.false.i1008
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_146:                             # %init_one_libfunc.exit1012
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+80(%rip)
	movabsq	$.L.str43, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_148
# BB#147:                               # %cond.false.i1024
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_148:                             # %init_one_libfunc.exit1028
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+88(%rip)
	movabsq	$.L.str44, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_150
# BB#149:                               # %cond.false.i1040
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_150:                             # %init_one_libfunc.exit1044
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+96(%rip)
	movabsq	$.L.str45, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_152
# BB#151:                               # %cond.false.i1056
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_152:                             # %init_one_libfunc.exit1060
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+104(%rip)
	movabsq	$.L.str46, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_154
# BB#153:                               # %cond.false.i1072
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_154:                             # %init_one_libfunc.exit1076
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+112(%rip)
	movabsq	$.L.str47, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_156
# BB#155:                               # %cond.false.i1088
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_156:                             # %init_one_libfunc.exit1092
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+120(%rip)
	movabsq	$.L.str48, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_158
# BB#157:                               # %cond.false.i1104
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_158:                             # %init_one_libfunc.exit1108
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+128(%rip)
	movabsq	$.L.str49, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_160
# BB#159:                               # %cond.false.i1120
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_160:                             # %init_one_libfunc.exit1124
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+136(%rip)
	movabsq	$.L.str50, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_162
# BB#161:                               # %cond.false.i1136
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_162:                             # %init_one_libfunc.exit1140
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+144(%rip)
	movabsq	$.L.str51, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_164
# BB#163:                               # %cond.false.i1152
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_164:                             # %init_one_libfunc.exit1156
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+160(%rip)
	movabsq	$.L.str52, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_166
# BB#165:                               # %cond.false.i1168
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_166:                             # %init_one_libfunc.exit1172
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+168(%rip)
	movabsq	$.L.str53, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_168
# BB#167:                               # %cond.false.i1184
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_168:                             # %init_one_libfunc.exit1188
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+176(%rip)
	movabsq	$.L.str54, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_170
# BB#169:                               # %cond.false.i1200
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_170:                             # %init_one_libfunc.exit1204
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+184(%rip)
	movabsq	$.L.str55, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_172
# BB#171:                               # %cond.false.i1216
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_172:                             # %init_one_libfunc.exit1220
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+192(%rip)
	movabsq	$.L.str56, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_174
# BB#173:                               # %cond.false.i1232
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_174:                             # %init_one_libfunc.exit1236
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+200(%rip)
	movabsq	$.L.str57, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_176
# BB#175:                               # %cond.false.i1248
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_176:                             # %init_one_libfunc.exit1252
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+208(%rip)
	movabsq	$.L.str58, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_178
# BB#177:                               # %cond.false.i1264
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_178:                             # %init_one_libfunc.exit1268
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+216(%rip)
	movabsq	$.L.str59, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_180
# BB#179:                               # %cond.false.i1280
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_180:                             # %init_one_libfunc.exit1284
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+224(%rip)
	movabsq	$.L.str60, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_182
# BB#181:                               # %cond.false.i1296
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_182:                             # %init_one_libfunc.exit1300
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+232(%rip)
	movabsq	$.L.str61, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_184
# BB#183:                               # %cond.false.i1312
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_184:                             # %init_one_libfunc.exit1316
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+240(%rip)
	movabsq	$.L.str62, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_186
# BB#185:                               # %cond.false.i1328
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_186:                             # %init_one_libfunc.exit1332
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+248(%rip)
	movabsq	$.L.str63, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_188
# BB#187:                               # %cond.false.i1344
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_188:                             # %init_one_libfunc.exit1348
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+256(%rip)
	movabsq	$.L.str64, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_190
# BB#189:                               # %cond.false.i1360
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_190:                             # %init_one_libfunc.exit1364
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+264(%rip)
	movabsq	$.L.str65, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_192
# BB#191:                               # %cond.false.i1376
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_192:                             # %init_one_libfunc.exit1380
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+272(%rip)
	movabsq	$.L.str66, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_194
# BB#193:                               # %cond.false.i1392
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_194:                             # %init_one_libfunc.exit1396
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+280(%rip)
	movabsq	$.L.str67, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_196
# BB#195:                               # %cond.false.i1408
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_196:                             # %init_one_libfunc.exit1412
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+288(%rip)
	movabsq	$.L.str68, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_198
# BB#197:                               # %cond.false.i1424
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_198:                             # %init_one_libfunc.exit1428
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+296(%rip)
	movabsq	$.L.str69, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_200
# BB#199:                               # %cond.false.i1440
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_200:                             # %init_one_libfunc.exit1444
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+304(%rip)
	movabsq	$.L.str70, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_202
# BB#201:                               # %cond.false.i1456
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_202:                             # %init_one_libfunc.exit1460
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+312(%rip)
	movabsq	$.L.str71, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_204
# BB#203:                               # %cond.false.i1472
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_204:                             # %init_one_libfunc.exit1476
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+320(%rip)
	movabsq	$.L.str72, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_206
# BB#205:                               # %cond.false.i1488
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_206:                             # %init_one_libfunc.exit1492
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+328(%rip)
	movabsq	$.L.str73, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_208
# BB#207:                               # %cond.false.i1504
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_208:                             # %init_one_libfunc.exit1508
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+336(%rip)
	movabsq	$.L.str74, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_210
# BB#209:                               # %cond.false.i1520
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_210:                             # %init_one_libfunc.exit1524
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+344(%rip)
	movabsq	$.L.str75, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_212
# BB#211:                               # %cond.false.i1536
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_212:                             # %init_one_libfunc.exit1540
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+352(%rip)
	movabsq	$.L.str76, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_214
# BB#213:                               # %cond.false.i1552
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_214:                             # %init_one_libfunc.exit1556
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+360(%rip)
	movabsq	$.L.str77, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_216
# BB#215:                               # %cond.false.i1568
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_216:                             # %init_one_libfunc.exit1572
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+368(%rip)
	movabsq	$.L.str78, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_218
# BB#217:                               # %cond.false.i1584
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_218:                             # %init_one_libfunc.exit1588
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+376(%rip)
	movabsq	$.L.str79, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_220
# BB#219:                               # %cond.false.i1600
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_220:                             # %init_one_libfunc.exit1604
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+384(%rip)
	movabsq	$.L.str80, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_222
# BB#221:                               # %cond.false.i1616
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_222:                             # %init_one_libfunc.exit1620
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+392(%rip)
	movabsq	$.L.str81, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_224
# BB#223:                               # %cond.false.i1632
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_224:                             # %init_one_libfunc.exit1636
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+400(%rip)
	movabsq	$.L.str82, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_226
# BB#225:                               # %cond.false.i1648
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_226:                             # %init_one_libfunc.exit1652
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+408(%rip)
	movabsq	$.L.str83, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_228
# BB#227:                               # %cond.false.i1664
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_228:                             # %init_one_libfunc.exit1668
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+416(%rip)
	movabsq	$.L.str84, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_230
# BB#229:                               # %cond.false.i1680
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_230:                             # %init_one_libfunc.exit1684
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+424(%rip)
	movabsq	$.L.str85, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_232
# BB#231:                               # %cond.false.i1696
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_232:                             # %init_one_libfunc.exit1700
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+432(%rip)
	movabsq	$.L.str86, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_234
# BB#233:                               # %cond.false.i1712
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_234:                             # %init_one_libfunc.exit1716
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+440(%rip)
	movabsq	$.L.str87, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_236
# BB#235:                               # %cond.false.i1728
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_236:                             # %init_one_libfunc.exit1732
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+448(%rip)
	movabsq	$.L.str88, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_238
# BB#237:                               # %cond.false.i1744
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_238:                             # %init_one_libfunc.exit1748
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+456(%rip)
	movabsq	$.L.str89, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_240
# BB#239:                               # %cond.false.i1760
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_240:                             # %init_one_libfunc.exit1764
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+464(%rip)
	movabsq	$.L.str90, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_242
# BB#241:                               # %cond.false.i1776
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_242:                             # %init_one_libfunc.exit1780
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+472(%rip)
	movabsq	$.L.str91, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_244
# BB#243:                               # %cond.false.i1792
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_244:                             # %init_one_libfunc.exit1796
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+480(%rip)
	movabsq	$.L.str92, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_246
# BB#245:                               # %cond.false.i1808
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_246:                             # %init_one_libfunc.exit1812
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+488(%rip)
	movabsq	$.L.str93, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_248
# BB#247:                               # %cond.false.i1824
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_248:                             # %init_one_libfunc.exit1828
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+496(%rip)
	movabsq	$.L.str94, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_250
# BB#249:                               # %cond.false.i1840
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_250:                             # %init_one_libfunc.exit1844
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+504(%rip)
	movabsq	$.L.str95, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_252
# BB#251:                               # %cond.false.i1856
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_252:                             # %init_one_libfunc.exit1860
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+512(%rip)
	movabsq	$.L.str96, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_254
# BB#253:                               # %cond.false.i1872
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_254:                             # %init_one_libfunc.exit1876
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+520(%rip)
	movabsq	$.L.str97, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_256
# BB#255:                               # %cond.false.i1888
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_256:                             # %init_one_libfunc.exit1892
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+528(%rip)
	movabsq	$.L.str98, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_258
# BB#257:                               # %cond.false.i1904
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_258:                             # %init_one_libfunc.exit1908
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+536(%rip)
	movabsq	$.L.str99, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_260
# BB#259:                               # %cond.false.i1920
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_260:                             # %init_one_libfunc.exit1924
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+544(%rip)
	movabsq	$.L.str100, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_262
# BB#261:                               # %cond.false.i1936
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_262:                             # %init_one_libfunc.exit1940
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+552(%rip)
	movabsq	$.L.str101, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_264
# BB#263:                               # %cond.false.i1952
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_264:                             # %init_one_libfunc.exit1956
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+560(%rip)
	movabsq	$.L.str102, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_266
# BB#265:                               # %cond.false.i1968
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_266:                             # %init_one_libfunc.exit1972
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+568(%rip)
	movabsq	$.L.str103, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_268
# BB#267:                               # %cond.false.i1984
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_268:                             # %init_one_libfunc.exit1988
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+576(%rip)
	movabsq	$.L.str104, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_270
# BB#269:                               # %cond.false.i2000
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_270:                             # %init_one_libfunc.exit2004
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+584(%rip)
	movabsq	$.L.str105, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_272
# BB#271:                               # %cond.false.i2016
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_272:                             # %init_one_libfunc.exit2020
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+592(%rip)
	movabsq	$.L.str106, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_274
# BB#273:                               # %cond.false.i2032
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_274:                             # %init_one_libfunc.exit2036
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+600(%rip)
	movabsq	$.L.str107, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_276
# BB#275:                               # %cond.false.i2048
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_276:                             # %init_one_libfunc.exit2052
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+608(%rip)
	movabsq	$.L.str108, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_278
# BB#277:                               # %cond.false.i2064
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_278:                             # %init_one_libfunc.exit2068
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+616(%rip)
	movabsq	$.L.str109, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_280
# BB#279:                               # %cond.false.i2080
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_280:                             # %init_one_libfunc.exit2084
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+624(%rip)
	movabsq	$.L.str110, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_282
# BB#281:                               # %cond.false.i2096
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_282:                             # %init_one_libfunc.exit2100
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+632(%rip)
	movabsq	$.L.str111, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_284
# BB#283:                               # %cond.false.i2112
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_284:                             # %init_one_libfunc.exit2116
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+640(%rip)
	movabsq	$.L.str112, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_286
# BB#285:                               # %cond.false.i2128
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_286:                             # %init_one_libfunc.exit2132
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+648(%rip)
	movabsq	$.L.str113, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_288
# BB#287:                               # %cond.false.i2144
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_288:                             # %init_one_libfunc.exit2148
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+656(%rip)
	movabsq	$.L.str114, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_290
# BB#289:                               # %cond.false.i2160
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_290:                             # %init_one_libfunc.exit2164
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+664(%rip)
	movabsq	$.L.str115, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_292
# BB#291:                               # %cond.false.i2176
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_292:                             # %init_one_libfunc.exit2180
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+672(%rip)
	movabsq	$.L.str116, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_294
# BB#293:                               # %cond.false.i2192
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_294:                             # %init_one_libfunc.exit2196
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+680(%rip)
	movabsq	$.L.str117, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_296
# BB#295:                               # %cond.false.i2208
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_296:                             # %init_one_libfunc.exit2212
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+688(%rip)
	movabsq	$.L.str118, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_298
# BB#297:                               # %cond.false.i2224
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_298:                             # %init_one_libfunc.exit2228
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+696(%rip)
	movabsq	$.L.str119, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_300
# BB#299:                               # %cond.false.i2240
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_300:                             # %init_one_libfunc.exit2244
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+704(%rip)
	movabsq	$.L.str120, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_302
# BB#301:                               # %cond.false.i2256
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_302:                             # %init_one_libfunc.exit2260
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+712(%rip)
	movabsq	$.L.str121, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_304
# BB#303:                               # %cond.false.i2272
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_304:                             # %init_one_libfunc.exit2276
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+720(%rip)
	movabsq	$.L.str122, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_306
# BB#305:                               # %cond.false.i2288
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_306:                             # %init_one_libfunc.exit2292
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+728(%rip)
	movabsq	$.L.str123, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_308
# BB#307:                               # %cond.false.i2304
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_308:                             # %init_one_libfunc.exit2308
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+736(%rip)
	movabsq	$.L.str124, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_310
# BB#309:                               # %cond.false.i2320
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_310:                             # %init_one_libfunc.exit2324
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+744(%rip)
	movabsq	$.L.str125, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_312
# BB#311:                               # %cond.false.i2336
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_312:                             # %init_one_libfunc.exit2340
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+752(%rip)
	movabsq	$.L.str126, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_314
# BB#313:                               # %cond.false.i2352
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_314:                             # %init_one_libfunc.exit2356
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+760(%rip)
	movabsq	$.L.str127, %rdi
	callq	get_identifier
	movq	integer_types+40(%rip), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	$30, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rbx
	orq	$4194560, 48(%rbx)      # imm = 0x400100
	movl	16(%rbx), %eax
	orq	$524288, %rax           # imm = 0x80000
	movl	%eax, 16(%rbx)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB36_316
# BB#315:                               # %cond.false.i2368
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB36_316:                             # %init_one_libfunc.exit2372
	movq	8(%rax), %rax
	movq	%rax, libfunc_table+768(%rip)
	movabsq	$libfunc_table, %rbx
	movq	$103, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, trap_rtx(%rip)
	movabsq	$trap_rtx, %rdi
	movq	$1, %rsi
	callq	ggc_add_rtx_root
	movabsq	$optab_table, %rdi
	movabsq	$mark_optab, %rcx
	movq	$49, %rsi
	movq	$8, %rdx
	callq	ggc_add_root
	movq	$97, %rsi
	movq	%rbx, %rdi
	popq	%rbx
	jmp	ggc_add_rtx_root  # TAILCALL
.Ltmp445:
	.size	init_optabs, .Ltmp445-init_optabs
	.cfi_endproc

	.globl	gen_cond_trap
	.align	16, 0x90
	.type	gen_cond_trap,@function
gen_cond_trap:                          # @gen_cond_trap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp452:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp453:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp454:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp455:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp456:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp457:
	.cfi_def_cfa_offset 64
.Ltmp458:
	.cfi_offset %rbx, -48
.Ltmp459:
	.cfi_offset %r12, -40
.Ltmp460:
	.cfi_offset %r13, -32
.Ltmp461:
	.cfi_offset %r14, -24
.Ltmp462:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	2(%rbx), %r14d
	movq	%rdx, %r13
	movq	%rdi, %r15
	xorq	%r12, %r12
	testq	%r14, %r14
	je	.LBB37_5
# BB#1:                                 # %if.end
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	optab_table+328(%rip), %rax
	movq	$4, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movl	8(%rax,%rdx), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB37_5
# BB#2:                                 # %if.then2
	callq	start_sequence
	movq	optab_table+328(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %r14
	movslq	8(%rax,%r14), %rax
	leaq	(%rax,%rax,4), %rcx
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	*insn_data+16(,%rcx,8)
	movq	%rax, %rdi
	callq	emit_insn
	movq	trap_rtx(%rip), %rax
	movw	%r15w, (%rax)
	movq	trap_rtx(%rip), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_conditional_trap
	testq	%rax, %rax
	je	.LBB37_4
# BB#3:                                 # %if.then13
	movq	%rax, %rdi
	callq	emit_insn
	callq	gen_sequence
	movq	%rax, %r12
.LBB37_4:                               # %if.end16
	callq	end_sequence
.LBB37_5:                               # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp463:
	.size	gen_cond_trap, .Ltmp463-gen_cond_trap
	.cfi_endproc

	.align	16, 0x90
	.type	init_libfuncs,@function
init_libfuncs:                          # @init_libfuncs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp467:
	.cfi_def_cfa_offset 16
.Ltmp468:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp469:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$40, %rsp
.Ltmp470:
	.cfi_offset %rbx, -56
.Ltmp471:
	.cfi_offset %r12, -48
.Ltmp472:
	.cfi_offset %r13, -40
.Ltmp473:
	.cfi_offset %r14, -32
.Ltmp474:
	.cfi_offset %r15, -24
	movq	%r8, -48(%rbp)          # 8-byte Spill
	movq	%rcx, %rbx
	movq	%rdx, -56(%rbp)         # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jg	.LBB38_7
# BB#1:                                 # %for.body.lr.ph
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r14
	addq	$4, %r14
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	1(%rbx), %rax
	movq	%rax, -72(%rbp)         # 8-byte Spill
	movq	%rbx, %r13
	.align	16, 0x90
.LBB38_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB38_8 Depth 2
                                        #     Child Loop BB38_5 Depth 2
	movq	mode_name(,%r15,8), %r12
	movq	%r12, %rdi
	callq	strlen
	addq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %rcx
	movq	%rcx, %rdi
	subq	%rax, %rdi
	negq	%rax
	movq	%rdi, %rsp
	movq	$24415, %rdx            # imm = 0x5F5F
	movw	%dx, (%rcx,%rax)
	movzbl	(%r13), %edx
	leaq	2(%rdi), %rax
	testq	%rdx, %rdx
	movq	-72(%rbp), %rcx         # 8-byte Reload
	je	.LBB38_3
	.align	16, 0x90
.LBB38_8:                               # %for.body10
                                        #   Parent Loop BB38_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movb	%dl, (%rax)
	movzbl	(%rcx), %edx
	incq	%rax
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB38_8
.LBB38_3:                               # %for.cond13.preheader
                                        #   in Loop: Header=BB38_2 Depth=1
	movzbl	(%r12), %ecx
	testq	%rcx, %rcx
	je	.LBB38_6
# BB#4:                                 # %for.body15.preheader
                                        #   in Loop: Header=BB38_2 Depth=1
	incq	%r12
	.align	16, 0x90
.LBB38_5:                               # %for.body15
                                        #   Parent Loop BB38_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rcx
	movzbl	_sch_tolower(%rcx), %ecx
	movb	%cl, (%rax)
	movzbl	(%r12), %ecx
	incq	%rax
	incq	%r12
	testq	%rcx, %rcx
	jne	.LBB38_5
.LBB38_6:                               # %for.end21
                                        #   in Loop: Header=BB38_2 Depth=1
	movq	-48(%rbp), %rcx         # 8-byte Reload
	movb	%cl, (%rax)
	xorq	%rcx, %rcx
	movb	%cl, 1(%rax)
	movl	target_flags(%rip), %ebx
	leaq	1(%rax), %rsi
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	subq	%rdi, %rsi
	callq	ggc_alloc_string
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	$4, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	-64(%rbp), %rcx         # 8-byte Reload
	movq	%rax, 16(%rcx,%rdx)
	movq	$32, %rcx
	leaq	1(%r15), %rax
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$32, %rcx
	movq	-56(%rbp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %r15
	movq	%rax, %r15
	jl	.LBB38_2
.LBB38_7:                               # %for.end33
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp475:
	.size	init_libfuncs, .Ltmp475-init_libfuncs
	.cfi_endproc

	.type	code_to_optab,@object   # @code_to_optab
	.comm	code_to_optab,1232,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"optabs.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.expand_simple_binop,@object # @__FUNCTION__.expand_simple_binop
.L__FUNCTION__.expand_simple_binop:
	.asciz	"expand_simple_binop"
	.size	.L__FUNCTION__.expand_simple_binop, 20

	.type	optab_table,@object     # @optab_table
	.comm	optab_table,392,16
	.type	.L__FUNCTION__.expand_binop,@object # @__FUNCTION__.expand_binop
.L__FUNCTION__.expand_binop:
	.asciz	"expand_binop"
	.size	.L__FUNCTION__.expand_binop, 13

	.type	.L__FUNCTION__.expand_twoval_binop,@object # @__FUNCTION__.expand_twoval_binop
.L__FUNCTION__.expand_twoval_binop:
	.asciz	"expand_twoval_binop"
	.size	.L__FUNCTION__.expand_twoval_binop, 20

	.type	.L__FUNCTION__.expand_simple_unop,@object # @__FUNCTION__.expand_simple_unop
.L__FUNCTION__.expand_simple_unop:
	.asciz	"expand_simple_unop"
	.size	.L__FUNCTION__.expand_simple_unop, 19

	.type	.L__FUNCTION__.expand_unop,@object # @__FUNCTION__.expand_unop
.L__FUNCTION__.expand_unop:
	.asciz	"expand_unop"
	.size	.L__FUNCTION__.expand_unop, 12

	.type	.L__FUNCTION__.expand_complex_abs,@object # @__FUNCTION__.expand_complex_abs
.L__FUNCTION__.expand_complex_abs:
	.asciz	"expand_complex_abs"
	.size	.L__FUNCTION__.expand_complex_abs, 19

	.type	.L__FUNCTION__.emit_no_conflict_block,@object # @__FUNCTION__.emit_no_conflict_block
.L__FUNCTION__.emit_no_conflict_block:
	.asciz	"emit_no_conflict_block"
	.size	.L__FUNCTION__.emit_no_conflict_block, 23

	.type	bcc_gen_fctn,@object    # @bcc_gen_fctn
	.comm	bcc_gen_fctn,1224,16
	.type	setcc_gen_code,@object  # @setcc_gen_code
	.comm	setcc_gen_code,612,16
	.type	.L__FUNCTION__.emit_cmp_and_jump_insns,@object # @__FUNCTION__.emit_cmp_and_jump_insns
.L__FUNCTION__.emit_cmp_and_jump_insns:
	.asciz	"emit_cmp_and_jump_insns"
	.size	.L__FUNCTION__.emit_cmp_and_jump_insns, 24

	.type	movcc_gen_code,@object  # @movcc_gen_code
	.comm	movcc_gen_code,236,16
	.type	.L__FUNCTION__.gen_add2_insn,@object # @__FUNCTION__.gen_add2_insn
.L__FUNCTION__.gen_add2_insn:
	.asciz	"gen_add2_insn"
	.size	.L__FUNCTION__.gen_add2_insn, 14

	.type	.L__FUNCTION__.have_add2_insn,@object # @__FUNCTION__.have_add2_insn
.L__FUNCTION__.have_add2_insn:
	.asciz	"have_add2_insn"
	.size	.L__FUNCTION__.have_add2_insn, 15

	.type	.L__FUNCTION__.gen_sub2_insn,@object # @__FUNCTION__.gen_sub2_insn
.L__FUNCTION__.gen_sub2_insn:
	.asciz	"gen_sub2_insn"
	.size	.L__FUNCTION__.gen_sub2_insn, 14

	.type	.L__FUNCTION__.have_sub2_insn,@object # @__FUNCTION__.have_sub2_insn
.L__FUNCTION__.have_sub2_insn:
	.asciz	"have_sub2_insn"
	.size	.L__FUNCTION__.have_sub2_insn, 15

	.type	.L__FUNCTION__.gen_move_insn,@object # @__FUNCTION__.gen_move_insn
.L__FUNCTION__.gen_move_insn:
	.asciz	"gen_move_insn"
	.size	.L__FUNCTION__.gen_move_insn, 14

	.type	extendtab,@object       # @extendtab
	.comm	extendtab,27848,16
	.type	.L__FUNCTION__.expand_float,@object # @__FUNCTION__.expand_float
.L__FUNCTION__.expand_float:
	.asciz	"expand_float"
	.size	.L__FUNCTION__.expand_float, 13

	.type	libfunc_table,@object   # @libfunc_table
	.comm	libfunc_table,776,16
	.type	.L__FUNCTION__.expand_fix,@object # @__FUNCTION__.expand_fix
.L__FUNCTION__.expand_fix:
	.asciz	"expand_fix"
	.size	.L__FUNCTION__.expand_fix, 11

	.type	fixtab,@object          # @fixtab
	.comm	fixtab,27848,16
	.type	fixtrunctab,@object     # @fixtrunctab
	.comm	fixtrunctab,27848,16
	.type	floattab,@object        # @floattab
	.comm	floattab,27848,16
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"add"
	.size	.L.str1, 4

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"addv"
	.size	.L.str2, 5

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"sub"
	.size	.L.str3, 4

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"subv"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"mul"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"mulv"
	.size	.L.str6, 5

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"div"
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"divv"
	.size	.L.str8, 5

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"udiv"
	.size	.L.str9, 5

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"divmod"
	.size	.L.str10, 7

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"udivmod"
	.size	.L.str11, 8

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"mod"
	.size	.L.str12, 4

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"umod"
	.size	.L.str13, 5

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"ftrunc"
	.size	.L.str14, 7

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"and"
	.size	.L.str15, 4

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"ior"
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"xor"
	.size	.L.str17, 4

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"ashl"
	.size	.L.str18, 5

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"ashr"
	.size	.L.str19, 5

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"lshr"
	.size	.L.str20, 5

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"min"
	.size	.L.str21, 4

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"max"
	.size	.L.str22, 4

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"umin"
	.size	.L.str23, 5

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"umax"
	.size	.L.str24, 5

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"neg"
	.size	.L.str25, 4

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"negv"
	.size	.L.str26, 5

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"one_cmpl"
	.size	.L.str27, 9

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"ffs"
	.size	.L.str28, 4

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"cmp"
	.size	.L.str29, 4

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"ucmp"
	.size	.L.str30, 5

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"cabs"
	.size	.L.str31, 5

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"__extendsfdf2"
	.size	.L.str32, 14

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"__extendsfxf2"
	.size	.L.str33, 14

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"__extendsftf2"
	.size	.L.str34, 14

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"__extenddfxf2"
	.size	.L.str35, 14

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"__extenddftf2"
	.size	.L.str36, 14

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"__truncdfsf2"
	.size	.L.str37, 13

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"__truncxfsf2"
	.size	.L.str38, 13

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"__trunctfsf2"
	.size	.L.str39, 13

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"__truncxfdf2"
	.size	.L.str40, 13

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"__trunctfdf2"
	.size	.L.str41, 13

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"abort"
	.size	.L.str42, 6

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"memcpy"
	.size	.L.str43, 7

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"memmove"
	.size	.L.str44, 8

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"bcopy"
	.size	.L.str45, 6

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"memcmp"
	.size	.L.str46, 7

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"__gcc_bcmp"
	.size	.L.str47, 11

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"memset"
	.size	.L.str48, 7

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"bzero"
	.size	.L.str49, 6

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"_Unwind_Resume"
	.size	.L.str50, 15

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"__builtin_setjmp"
	.size	.L.str51, 17

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"__builtin_longjmp"
	.size	.L.str52, 18

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"_Unwind_SjLj_Register"
	.size	.L.str53, 22

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"_Unwind_SjLj_Unregister"
	.size	.L.str54, 24

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"__eqhf2"
	.size	.L.str55, 8

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"__nehf2"
	.size	.L.str56, 8

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"__gthf2"
	.size	.L.str57, 8

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"__gehf2"
	.size	.L.str58, 8

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"__lthf2"
	.size	.L.str59, 8

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"__lehf2"
	.size	.L.str60, 8

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"__unordhf2"
	.size	.L.str61, 11

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"__eqsf2"
	.size	.L.str62, 8

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"__nesf2"
	.size	.L.str63, 8

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"__gtsf2"
	.size	.L.str64, 8

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"__gesf2"
	.size	.L.str65, 8

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"__ltsf2"
	.size	.L.str66, 8

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"__lesf2"
	.size	.L.str67, 8

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"__unordsf2"
	.size	.L.str68, 11

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"__eqdf2"
	.size	.L.str69, 8

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"__nedf2"
	.size	.L.str70, 8

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"__gtdf2"
	.size	.L.str71, 8

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"__gedf2"
	.size	.L.str72, 8

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"__ltdf2"
	.size	.L.str73, 8

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"__ledf2"
	.size	.L.str74, 8

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"__unorddf2"
	.size	.L.str75, 11

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"__eqxf2"
	.size	.L.str76, 8

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"__nexf2"
	.size	.L.str77, 8

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"__gtxf2"
	.size	.L.str78, 8

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"__gexf2"
	.size	.L.str79, 8

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"__ltxf2"
	.size	.L.str80, 8

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"__lexf2"
	.size	.L.str81, 8

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"__unordxf2"
	.size	.L.str82, 11

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"__eqtf2"
	.size	.L.str83, 8

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"__netf2"
	.size	.L.str84, 8

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"__gttf2"
	.size	.L.str85, 8

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"__getf2"
	.size	.L.str86, 8

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"__lttf2"
	.size	.L.str87, 8

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"__letf2"
	.size	.L.str88, 8

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"__unordtf2"
	.size	.L.str89, 11

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"__floatsisf"
	.size	.L.str90, 12

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"__floatdisf"
	.size	.L.str91, 12

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"__floattisf"
	.size	.L.str92, 12

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"__floatsidf"
	.size	.L.str93, 12

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"__floatdidf"
	.size	.L.str94, 12

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"__floattidf"
	.size	.L.str95, 12

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"__floatsixf"
	.size	.L.str96, 12

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"__floatdixf"
	.size	.L.str97, 12

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"__floattixf"
	.size	.L.str98, 12

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"__floatsitf"
	.size	.L.str99, 12

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"__floatditf"
	.size	.L.str100, 12

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"__floattitf"
	.size	.L.str101, 12

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"__fixsfsi"
	.size	.L.str102, 10

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"__fixsfdi"
	.size	.L.str103, 10

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"__fixsfti"
	.size	.L.str104, 10

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"__fixdfsi"
	.size	.L.str105, 10

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"__fixdfdi"
	.size	.L.str106, 10

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"__fixdfti"
	.size	.L.str107, 10

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"__fixxfsi"
	.size	.L.str108, 10

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"__fixxfdi"
	.size	.L.str109, 10

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"__fixxfti"
	.size	.L.str110, 10

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"__fixtfsi"
	.size	.L.str111, 10

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"__fixtfdi"
	.size	.L.str112, 10

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"__fixtfti"
	.size	.L.str113, 10

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"__fixunssfsi"
	.size	.L.str114, 13

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"__fixunssfdi"
	.size	.L.str115, 13

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"__fixunssfti"
	.size	.L.str116, 13

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"__fixunsdfsi"
	.size	.L.str117, 13

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"__fixunsdfdi"
	.size	.L.str118, 13

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"__fixunsdfti"
	.size	.L.str119, 13

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"__fixunsxfsi"
	.size	.L.str120, 13

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"__fixunsxfdi"
	.size	.L.str121, 13

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"__fixunsxfti"
	.size	.L.str122, 13

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"__fixunstfsi"
	.size	.L.str123, 13

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"__fixunstfdi"
	.size	.L.str124, 13

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"__fixunstfti"
	.size	.L.str125, 13

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"__cyg_profile_func_enter"
	.size	.L.str126, 25

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"__cyg_profile_func_exit"
	.size	.L.str127, 24

	.type	trap_rtx,@object        # @trap_rtx
	.local	trap_rtx
	.comm	trap_rtx,8,8
	.type	.L__FUNCTION__.emit_cmp_and_jump_insn_1,@object # @__FUNCTION__.emit_cmp_and_jump_insn_1
.L__FUNCTION__.emit_cmp_and_jump_insn_1:
	.asciz	"emit_cmp_and_jump_insn_1"
	.size	.L__FUNCTION__.emit_cmp_and_jump_insn_1, 25

	.type	.L__FUNCTION__.prepare_cmp_insn,@object # @__FUNCTION__.prepare_cmp_insn
.L__FUNCTION__.prepare_cmp_insn:
	.asciz	"prepare_cmp_insn"
	.size	.L__FUNCTION__.prepare_cmp_insn, 17

	.type	.L__FUNCTION__.prepare_float_lib_cmp,@object # @__FUNCTION__.prepare_float_lib_cmp
.L__FUNCTION__.prepare_float_lib_cmp:
	.asciz	"prepare_float_lib_cmp"
	.size	.L__FUNCTION__.prepare_float_lib_cmp, 22


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
