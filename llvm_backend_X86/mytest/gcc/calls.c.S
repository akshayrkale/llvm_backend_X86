	.file	"calls.c.bc"
	.text
	.globl	prepare_call_address
	.align	16, 0x90
	.type	prepare_call_address,@function
prepare_call_address:                   # @prepare_call_address
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %r14
	movq	%rsi, %rbx
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	protect_from_queue
	movq	%rax, %r13
	testq	%rbx, %rbx
	je	.LBB0_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	lookup_static_chain
	movq	%rax, %r15
.LBB0_2:                                # %if.end
	movzbl	(%r13), %eax
	cmpq	$68, %rax
	je	.LBB0_5
# BB#3:                                 # %if.then3
	movq	$2, %rdi
	movq	%r13, %rsi
	callq	memory_address
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB0_5
# BB#4:                                 # %cond.true
	movq	%r13, %rdi
	callq	force_not_mem
	movq	%rax, %r13
.LBB0_5:                                # %if.end10
	testq	%r15, %r15
	je	.LBB0_8
# BB#6:                                 # %if.then12
	movq	static_chain_rtx(%rip), %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	static_chain_rtx(%rip), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB0_8
# BB#7:                                 # %if.then17
	movq	%r14, %rdi
	callq	use_reg
.LBB0_8:                                # %if.end19
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	prepare_call_address, .Ltmp16-prepare_call_address
	.cfi_endproc

	.globl	setjmp_call_p
	.align	16, 0x90
	.type	setjmp_call_p,@function
setjmp_call_p:                          # @setjmp_call_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp18:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	callq	special_function_p
	andq	$32, %rax
	popq	%rdx
	retq
.Ltmp19:
	.size	setjmp_call_p, .Ltmp19-setjmp_call_p
	.cfi_endproc

	.align	16, 0x90
	.type	special_function_p,@function
special_function_p:                     # @special_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp26:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp28:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp31:
	.cfi_def_cfa_offset 64
.Ltmp32:
	.cfi_offset %rbx, -48
.Ltmp33:
	.cfi_offset %r12, -40
.Ltmp34:
	.cfi_offset %r13, -32
.Ltmp35:
	.cfi_offset %r14, -24
.Ltmp36:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	$4, %r15
	jne	.LBB2_42
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB2_42
# BB#2:                                 # %land.lhs.true2
	movq	72(%rbx), %rax
	testq	%rax, %rax
	je	.LBB2_42
# BB#3:                                 # %land.lhs.true4
	movl	24(%rax), %r14d
	cmpq	$17, %r14
	ja	.LBB2_42
# BB#4:                                 # %land.lhs.true7
	cmpq	$0, 80(%rbx)
	jne	.LBB2_42
# BB#5:                                 # %land.lhs.true10
	movzbl	18(%rbx), %ecx
	testq	$8, %rcx
	je	.LBB2_42
# BB#6:                                 # %if.then
	movq	32(%rax), %r12
	cmpq	$16, %r14
	je	.LBB2_12
# BB#7:                                 # %if.then
	cmpq	$6, %r14
	jne	.LBB2_11
# BB#8:                                 # %land.lhs.true23
	movzbl	(%r12), %eax
	cmpq	$97, %rax
	jne	.LBB2_11
# BB#9:                                 # %land.lhs.true26
	movabsq	$.L.str8, %rsi
	movq	%r12, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$97, %r13
	je	.LBB2_15
# BB#10:                                # %lor.lhs.false
	cmpq	$16, %r14
	jne	.LBB2_11
.LBB2_12:                               # %land.lhs.true35
	movzbl	(%r12), %r13d
	cmpq	$95, %r13
	jne	.LBB2_13
# BB#14:                                # %land.lhs.true40
	movabsq	$.L.str9, %rsi
	movq	%r12, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$95, %r13
	jne	.LBB2_17
.LBB2_15:                               # %if.then43
	orq	$8, %r15
	jmp	.LBB2_16
.LBB2_11:                               # %lor.lhs.false.if.end_crit_edge
	movzbl	(%r12), %r13d
.LBB2_16:                               # %if.end
	movq	%r13, %rax
	andq	$255, %rax
	cmpq	$95, %rax
	movq	%r12, %rdi
	jne	.LBB2_23
.LBB2_17:                               # %if.then48
	movzbl	1(%r12), %eax
	leaq	2(%r12), %rdi
	cmpq	$95, %rax
	jne	.LBB2_20
# BB#18:                                # %land.lhs.true53
	movzbl	(%rdi), %ecx
	cmpq	$120, %rcx
	jne	.LBB2_20
# BB#19:                                # %if.then58
	leaq	3(%r12), %rdi
	jmp	.LBB2_22
.LBB2_20:                               # %if.else
	cmpq	$95, %rax
	je	.LBB2_22
# BB#21:                                # %if.else
	leaq	1(%r12), %rdi
.LBB2_22:                               # %if.else
	movq	$95, %r13
	jmp	.LBB2_23
.LBB2_13:
	movq	%r12, %rdi
.LBB2_23:                               # %if.end69
	movsbq	(%rdi), %r14
	cmpq	$107, %r14
	jg	.LBB2_26
# BB#24:                                # %if.end69
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rax
	andq	$255, %rax
	cmpq	$102, %rax
	je	.LBB2_54
# BB#25:
	movq	%r14, %r12
	jmp	.LBB2_56
.LBB2_26:                               # %if.end69
	movq	%r12, 8(%rsp)           # 8-byte Spill
	cmpq	$112, %r14
	jg	.LBB2_28
# BB#27:                                # %if.end69
	movq	%r14, %rax
	andq	$255, %rax
.LBB2_51:                               # %if.else142
	cmpq	$108, %rax
	movq	%r14, %r12
	jne	.LBB2_56
# BB#52:                                # %land.lhs.true147
	movzbl	1(%rdi), %eax
	cmpq	$111, %rax
	movq	$108, %r12
	jne	.LBB2_56
# BB#53:                                # %land.lhs.true152
	movabsq	$.L.str17, %rsi
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	strcmp
	movq	%r14, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_56
	jmp	.LBB2_41
.LBB2_28:                               # %if.end69
	movq	%r14, %rcx
	andq	$255, %rcx
	cmpq	$113, %rcx
	je	.LBB2_45
# BB#29:                                # %if.end69
	cmpq	$118, %rcx
	jne	.LBB2_30
# BB#47:                                # %land.lhs.true132
	movzbl	1(%rdi), %eax
	cmpq	$102, %rax
	movq	$118, %r12
	jne	.LBB2_56
# BB#48:                                # %land.lhs.true137
	movabsq	$.L.str16, %rsi
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	movq	%rcx, (%rsp)            # 8-byte Spill
	callq	strcmp
	movq	%r12, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	(%rsp), %rax            # 8-byte Reload
	je	.LBB2_49
# BB#50:                                # %if.else142
	cmpq	$102, %rax
	jne	.LBB2_51
.LBB2_54:                               # %land.lhs.true162
	movzbl	1(%rdi), %eax
	cmpq	$111, %rax
	movq	$102, %r12
	jne	.LBB2_56
# BB#55:                                # %land.lhs.true167
	movabsq	$.L.str18, %rsi
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	callq	strcmp
	movq	%r12, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r14, %r12
	jne	.LBB2_56
	jmp	.LBB2_69
.LBB2_45:                               # %land.lhs.true119
	movzbl	1(%rdi), %eax
	cmpq	$115, %rax
	movq	$113, %r12
	jne	.LBB2_56
# BB#46:                                # %land.lhs.true124
	movabsq	$.L.str15, %rsi
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	strcmp
	movq	%r14, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_56
.LBB2_49:                               # %if.then140
	orq	$32, %r15
	jmp	.LBB2_42
.LBB2_30:                               # %if.end69
	cmpq	$115, %rcx
	movq	%r14, %r12
	jne	.LBB2_56
# BB#31:                                # %if.then74
	movzbl	1(%rdi), %ebx
	cmpq	$97, %rbx
	je	.LBB2_43
# BB#32:                                # %if.then74
	cmpq	$105, %rbx
	je	.LBB2_38
# BB#33:                                # %if.then74
	cmpq	$101, %rbx
	jne	.LBB2_42
# BB#34:                                # %land.lhs.true79
	movabsq	$.L.str10, %rsi
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	callq	strcmp
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB2_44
# BB#35:                                # %lor.lhs.false82
	movabsq	$.L.str11, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB2_44
# BB#36:                                # %lor.lhs.false85
	cmpq	$97, %rbx
	movq	%r12, %rdi
	jne	.LBB2_37
.LBB2_43:                               # %land.lhs.true98
	movabsq	$.L.str13, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_42
.LBB2_44:                               # %if.end103.thread348
	orq	$32, %r15
	jmp	.LBB2_42
.LBB2_56:                               # %lor.lhs.false170
	andq	$255, %r13
	cmpq	$95, %r13
	jne	.LBB2_59
# BB#57:                                # %land.lhs.true175
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	1(%rax), %eax
	cmpq	$95, %rax
	jne	.LBB2_59
# BB#58:                                # %land.lhs.true180
	movabsq	$.L.str19, %rsi
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	strcmp
	movq	%r14, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_69
.LBB2_59:                               # %lor.lhs.false183
	andq	$255, %r12
	cmpq	$101, %r12
	jne	.LBB2_70
# BB#60:                                # %land.lhs.true188
	movzbl	1(%rdi), %eax
	cmpq	$120, %rax
	jne	.LBB2_70
# BB#61:                                # %land.lhs.true193
	movzbl	2(%rdi), %eax
	cmpq	$101, %rax
	jne	.LBB2_70
# BB#62:                                # %land.lhs.true198
	movzbl	3(%rdi), %eax
	cmpq	$99, %rax
	jne	.LBB2_70
# BB#63:                                # %land.lhs.true203
	movzbl	4(%rdi), %eax
	cmpq	$108, %rax
	je	.LBB2_65
# BB#64:                                # %land.lhs.true203
	cmpq	$118, %rax
	jne	.LBB2_70
.LBB2_65:                               # %land.lhs.true213
	movzbl	5(%rdi), %eax
	testq	%rax, %rax
	je	.LBB2_69
# BB#66:                                # %land.lhs.true213
	cmpq	$101, %rax
	je	.LBB2_68
# BB#67:                                # %land.lhs.true213
	cmpq	$112, %rax
	jne	.LBB2_70
.LBB2_68:                               # %land.lhs.true228
	movzbl	6(%rdi), %eax
	testq	%rax, %rax
	je	.LBB2_69
.LBB2_70:                               # %if.else235
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	movl	target_flags(%rip), %edx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	orq	$4, %rdx
	cmpq	%rdx, %rax
	jne	.LBB2_42
# BB#71:                                # %land.lhs.true247
	movabsq	$.L.str20, %rsi
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	strcmp
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB2_74
# BB#72:                                # %lor.lhs.false250
	movabsq	$.L.str21, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB2_74
# BB#73:                                # %lor.lhs.false253
	movabsq	$.L.str22, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r14, %rax
	jne	.LBB2_42
.LBB2_74:                               # %if.then256
	orq	$4, %r15
	jmp	.LBB2_42
.LBB2_69:                               # %if.then233
	orq	$128, %r15
	jmp	.LBB2_42
.LBB2_37:                               # %lor.lhs.false85
	cmpq	$105, %rbx
	jne	.LBB2_42
.LBB2_38:                               # %land.lhs.true90
	movabsq	$.L.str12, %rsi
	movq	%rdi, %rbx
	callq	strcmp
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB2_40
# BB#39:
	orq	$32, %r15
.LBB2_40:                               # %land.lhs.true90
	movabsq	$.L.str14, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r14, %rax
	jne	.LBB2_42
.LBB2_41:
	orq	$64, %r15
.LBB2_42:                               # %land.lhs.true90
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp37:
	.size	special_function_p, .Ltmp37-special_function_p
	.cfi_endproc

	.globl	expand_call
	.align	16, 0x90
	.type	expand_call,@function
expand_call:                            # @expand_call
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp41:
	.cfi_def_cfa_offset 16
.Ltmp42:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp43:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$456, %rsp              # imm = 0x1C8
.Ltmp44:
	.cfi_offset %rbx, -56
.Ltmp45:
	.cfi_offset %r12, -48
.Ltmp46:
	.cfi_offset %r13, -40
.Ltmp47:
	.cfi_offset %r14, -32
.Ltmp48:
	.cfi_offset %r15, -24
	movl	target_flags(%rip), %eax
	movq	40(%rdi), %rcx
	movq	%rcx, -200(%rbp)        # 8-byte Spill
	movq	%rdx, -392(%rbp)        # 8-byte Spill
	movq	%rsi, -216(%rbp)        # 8-byte Spill
	testq	$2048, %rax             # imm = 0x800
	movq	$1, %rcx
	jne	.LBB3_2
# BB#1:                                 # %land.rhs
	andq	$4096, %rax             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rax
	movq	%rax, %rcx
.LBB3_2:                                # %land.end
	movq	%rcx, -192(%rbp)        # 8-byte Spill
	movq	cfun(%rip), %rax
	movslq	highest_outgoing_arg_in_use(%rip), %rcx
	movq	%rcx, -384(%rbp)        # 8-byte Spill
	movq	$0, -120(%rbp)
	movq	16(%rax), %rax
	movq	stack_usage_map(%rip), %rcx
	movq	%rcx, -400(%rbp)        # 8-byte Spill
	movl	16(%rdi), %r13d
	movl	4(%rax), %eax
	movq	%rax, -360(%rbp)        # 8-byte Spill
	movq	32(%rdi), %rbx
	movq	$13, %rcx
	shrq	%cl, %r13
	andq	$16, %r13
	movq	%rdi, %r14
	callq	get_callee_fndecl
	movq	%rax, -224(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	je	.LBB3_26
# BB#3:                                 # %if.then10
	movl	flag_no_inline(%rip), %eax
	testq	%rax, %rax
	movq	-224(%rbp), %rbx        # 8-byte Reload
	jne	.LBB3_9
# BB#4:                                 # %if.then10
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rbx
	je	.LBB3_9
# BB#5:                                 # %land.lhs.true12
	movzbl	49(%rbx), %ecx
	testq	$8, %rcx
	je	.LBB3_9
# BB#6:                                 # %land.lhs.true17
	movq	160(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB3_9
# BB#7:                                 # %land.lhs.true20
	movl	352(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB3_9
# BB#8:                                 # %land.lhs.true20.if.end49_crit_edge
	leaq	16(%rbx), %r15
	movq	$1, -136(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_19
.LBB3_9:                                # %if.else
	movzbl	17(%rbx), %ecx
	leaq	16(%rbx), %r15
	testq	$4, %rcx
	je	.LBB3_11
# BB#10:
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_19
.LBB3_26:                               # %if.else52
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %r12d
	movq	%r12, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	xorq	%rdx, %rdx
	cmpq	$100, %rcx
	jne	.LBB3_27
# BB#28:                                # %if.end34.i1857
	movq	48(%rax), %rdx
	movq	8(%rax), %rax
	movq	$25, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$4, %rdi
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rdx
	je	.LBB3_30
# BB#29:                                # %if.end34.i1857
	orq	$4608, %rdi             # imm = 0x1200
.LBB3_30:                               # %if.end34.i1857
	movq	$13, %rcx
	movq	%r12, %rdx
	movl	16(%rax), %esi
	shrq	%cl, %rdx
	andq	$16, %rdx
	orq	%rdi, %rdx
	jmp	.LBB3_31
.LBB3_11:                               # %if.then31
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %r8
	movq	%rax, %rdx
	jne	.LBB3_13
# BB#12:                                # %if.then31
	movq	%r8, %rdx
.LBB3_13:                               # %if.then31
	movzbl	49(%rbx), %esi
	movl	warn_inline(%rip), %edi
	movq	$3, %rcx
	shrq	%cl, %rsi
	testq	%rdi, %rdi
	je	.LBB3_15
# BB#14:                                # %if.then31
	movq	%r8, %rax
.LBB3_15:                               # %if.then31
	movslq	optimize(%rip), %rcx
	testq	%rcx, %rcx
	jle	.LBB3_18
# BB#16:                                # %if.then31
	andq	%rsi, %rax
	andq	%rdx, %rax
	je	.LBB3_18
# BB#17:                                # %if.then45
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	warning_with_decl
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB3_18:                               # %if.end46
	movq	%rbx, %rdi
	callq	mark_addressable
.LBB3_19:                               # %if.end49
	movq	%rbx, -224(%rbp)        # 8-byte Spill
	movl	(%r15), %r12d
	movq	%r12, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %eax
	xorq	%rdx, %rdx
	cmpq	$100, %rax
	movq	%r12, %rsi
	movq	%rbx, %rax
	jne	.LBB3_23
# BB#20:                                # %if.end34.i
	movq	-224(%rbp), %rax        # 8-byte Reload
	movq	48(%rax), %rdx
	movq	8(%rax), %rax
	movq	$25, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$4, %rdi
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rdx
	je	.LBB3_22
# BB#21:                                # %if.end34.i
	orq	$4608, %rdi             # imm = 0x1200
.LBB3_22:                               # %if.end34.i
	movq	$13, %rcx
	movq	%r12, %rdx
	movl	16(%rax), %esi
	shrq	%cl, %rdx
	andq	$16, %rdx
	orq	%rdi, %rdx
.LBB3_23:                               # %if.end43.i
	movq	%r12, %rcx
	andq	$6144, %rcx             # imm = 0x1800
	cmpq	$4096, %rcx             # imm = 0x1000
	jne	.LBB3_25
# BB#24:
	orq	$4097, %rdx             # imm = 0x1001
.LBB3_25:                               # %if.end43.i
	movq	$10, %rcx
	shrq	%cl, %r12
	andq	$2, %r12
	orq	%rdx, %r12
	andq	$255, %rsi
	jmp	.LBB3_34
.LBB3_27:
	movq	%r12, %rsi
.LBB3_31:                               # %if.end43.i1866
	movq	%r12, %rcx
	andq	$6144, %rcx             # imm = 0x1800
	cmpq	$4096, %rcx             # imm = 0x1000
	jne	.LBB3_33
# BB#32:
	orq	$4097, %rdx             # imm = 0x1001
.LBB3_33:                               # %if.end43.i1866
	movq	$10, %rcx
	shrq	%cl, %r12
	andq	$2, %r12
	orq	%rdx, %r12
	andq	$255, %rsi
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
.LBB3_34:                               # %if.end43.i1866
	cmpq	$23, %rsi
	jne	.LBB3_37
# BB#35:                                # %land.lhs.true73.i1872
	movzbl	62(%rax), %eax
	testq	$2, %rax
	je	.LBB3_37
# BB#36:                                # %if.then79.i1875
	andq	$-5634, %r12            # imm = 0xFFFFFFFFFFFFE9FE
	orq	$1024, %r12             # imm = 0x400
.LBB3_37:                               # %if.end58
	movl	warn_aggregate_return(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_40
# BB#38:                                # %land.lhs.true69
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	addq	$-18, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$4, %rcx
	ja	.LBB3_40
# BB#39:                                # %if.then107
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB3_40:                               # %if.end108
	orq	%r13, %r12
	movq	%r14, %rbx
	movq	%rbx, %rdi
	callq	aggregate_value_p
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB3_41
# BB#42:                                # %if.then111
	movq	8(%rbx), %rdi
	movq	%rbx, %r14
	movabsq	$4294962686, %rax       # imm = 0xFFFFEDFE
	andq	%rax, %r12
	callq	int_size_in_bytes
	movq	-216(%rbp), %rdx        # 8-byte Reload
	testq	%rdx, %rdx
	je	.LBB3_45
# BB#43:                                # %land.lhs.true117
	movzbl	(%rdx), %ecx
	cmpq	$66, %rcx
	jne	.LBB3_45
# BB#44:                                # %if.then121
	movq	%rax, -368(%rbp)        # 8-byte Spill
	movq	8(%rdx), %rax
	movq	%rax, -208(%rbp)        # 8-byte Spill
	jmp	.LBB3_46
.LBB3_45:                               # %if.else123
	movq	%rax, -368(%rbp)        # 8-byte Spill
	movq	8(%r14), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	callq	assign_temp
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	mark_temp_addr_taken
	movq	8(%rbx), %rax
	movq	%rax, -208(%rbp)        # 8-byte Spill
	movq	$0, -216(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_46
.LBB3_41:
	movq	$0, -368(%rbp)          # 8-byte Folded Spill
	movq	%rbx, %r14
	movq	$0, -208(%rbp)          # 8-byte Folded Spill
.LBB3_46:                               # %if.end131
	movq	-136(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	movq	%r14, %rcx
	je	.LBB3_72
# BB#47:                                # %if.then133
	movq	%r15, %r13
	movq	8(%rcx), %rbx
	movq	%rcx, -272(%rbp)        # 8-byte Spill
	movq	$0, -64(%rbp)
	callq	get_last_insn
	movq	%rax, -136(%rbp)        # 8-byte Spill
	movq	$12, %rdi
	callq	timevar_push
	movq	-224(%rbp), %r15        # 8-byte Reload
	movq	%r15, %rdi
	movq	-200(%rbp), %rsi        # 8-byte Reload
	movq	-216(%rbp), %rdx        # 8-byte Reload
	movq	-392(%rbp), %rcx        # 8-byte Reload
	movq	%rbx, %r8
	movq	-208(%rbp), %r9         # 8-byte Reload
	callq	expand_inline_function
	movq	%rax, %r14
	movq	$12, %rdi
	callq	timevar_pop
	cmpq	$-1, %r14
	je	.LBB3_63
# BB#48:                                # %if.then.i1882
	movzbl	target_flags+1(%rip), %eax
	testq	$16, %rax
	je	.LBB3_59
# BB#49:                                # %if.then.i1882
	movl	stack_arg_under_construction(%rip), %eax
	testq	%r13, %rax
	je	.LBB3_59
# BB#50:                                # %if.then11.i
	movq	-136(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB3_52
# BB#51:                                # %cond.true.i
	movq	24(%rax), %r15
	jmp	.LBB3_53
.LBB3_63:                               # %if.end52.i
	movl	warn_inline(%rip), %edx
	movzbl	49(%r15), %eax
	movq	$3, %rcx
	shrq	%cl, %rax
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	je	.LBB3_65
# BB#64:                                # %if.end52.i
	movq	%rsi, %rdx
.LBB3_65:                               # %if.end52.i
	movl	flag_no_inline(%rip), %edi
	testq	%rdi, %rdi
	jne	.LBB3_67
# BB#66:                                # %if.end52.i
	movq	%rsi, %rcx
.LBB3_67:                               # %if.end52.i
	movslq	optimize(%rip), %rsi
	testq	%rsi, %rsi
	jle	.LBB3_71
# BB#68:                                # %if.end52.i
	andq	%rax, %rdx
	andq	%rcx, %rdx
	je	.LBB3_71
# BB#69:                                # %land.lhs.true64.i
	movq	-224(%rbp), %rax        # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$4, %rax
	jne	.LBB3_71
# BB#70:                                # %if.then69.i
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	-224(%rbp), %rdi        # 8-byte Reload
	callq	warning_with_decl
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB3_71:                               # %try_to_integrate.exit.thread
	movq	-224(%rbp), %rdi        # 8-byte Reload
	callq	mark_addressable
	movq	-272(%rbp), %rcx        # 8-byte Reload
	jmp	.LBB3_72
.LBB3_52:                               # %cond.false.i
	callq	get_insns
	movq	%rax, %r15
.LBB3_53:                               # %cond.end.i
	movq	-224(%rbp), %rax        # 8-byte Reload
	movq	160(%rax), %rax
	movl	76(%rax), %eax
	testq	%rax, %rax
	jne	.LBB3_59
# BB#54:                                # %cond.end.i
	testq	%r15, %r15
	je	.LBB3_59
# BB#55:
	movq	%r15, %rax
	.align	16, 0x90
.LBB3_56:                               # %for.body20.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	je	.LBB3_58
# BB#57:                                # %for.inc25.i
                                        #   in Loop: Header=BB3_56 Depth=1
	movq	24(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_56
	jmp	.LBB3_59
.LBB3_58:                               # %if.then32.i
	callq	start_sequence
	leaq	-64(%rbp), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_save
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_CONST_INT
	xorq	%rsi, %rsi
	movq	$8, %rdx
	movq	%rax, %rdi
	callq	allocate_dynamic_stack_space
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_insns_before
	movq	-64(%rbp), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_restore
.LBB3_59:                               # %if.end44.i
	movq	-216(%rbp), %rbx        # 8-byte Reload
	cmpq	%rbx, %r14
	je	.LBB3_61
# BB#60:                                # %land.lhs.true47.i
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	movq	%rbx, %rax
	jne	.LBB3_62
.LBB3_61:                               # %if.end51.i
	movq	%r14, %rax
.LBB3_62:                               # %try_to_integrate.exit
	cmpq	$-1, %rax
	movq	-272(%rbp), %rcx        # 8-byte Reload
	jne	.LBB3_651
.LBB3_72:                               # %if.end140
	movq	%rcx, -272(%rbp)        # 8-byte Spill
	movq	32(%rcx), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB3_73
# BB#75:                                # %if.end160
	movslq	ix86_preferred_stack_boundary(%rip), %rcx
	movq	%rcx, -416(%rbp)        # 8-byte Spill
	movq	8(%rax), %rax
	movq	%rax, -344(%rbp)        # 8-byte Spill
	movq	-224(%rbp), %rdi        # 8-byte Reload
	movq	%r12, %rsi
	callq	special_function_p
	movq	%rax, %rdx
	orq	%r12, %rdx
	testq	$8, %rdx
	je	.LBB3_77
# BB#76:                                # %if.then167
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$64, %rcx
	movl	%ecx, 424(%rax)
.LBB3_77:                               # %if.end170
	xorq	%r13, %r13
	movq	-208(%rbp), %rdi        # 8-byte Reload
	testq	%rdi, %rdi
	movq	-200(%rbp), %rax        # 8-byte Reload
	movq	-216(%rbp), %r12        # 8-byte Reload
	je	.LBB3_78
# BB#79:                                # %if.end170
	movq	struct_value_rtx(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB3_80
# BB#81:                                # %if.then175
	movq	%rdx, %r12
	movq	%rax, %r15
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB3_85
# BB#82:                                # %lor.lhs.false181
	movl	target_flags(%rip), %eax
	testq	$4096, %rax             # imm = 0x1000
	movq	%rdi, %rbx
	je	.LBB3_86
# BB#83:                                # %lor.lhs.false181
	movl	stack_arg_under_construction(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rdi, %rbx
	je	.LBB3_86
# BB#84:                                # %lor.lhs.false181
	movq	global_rtl+72(%rip), %rax
	cmpq	%rax, %rdi
	movq	%rdi, %rbx
	jne	.LBB3_86
.LBB3_85:                               # %cond.true
	callq	copy_addr_to_reg
	movq	%rax, %rbx
.LBB3_86:                               # %cond.end
	movq	global_trees(%rip), %r14
	movq	-344(%rbp), %rax        # 8-byte Reload
	movq	8(%rax), %rdi
	callq	build_pointer_type
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	make_tree
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	tree_cons
	movq	$1, -376(%rbp)          # 8-byte Folded Spill
	movq	%r12, %rdx
	movq	-216(%rbp), %r12        # 8-byte Reload
	jmp	.LBB3_87
.LBB3_78:
	movq	$0, -376(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_87
.LBB3_80:
	movq	$0, -376(%rbp)          # 8-byte Folded Spill
.LBB3_87:                               # %if.end195
	movq	%rdx, -160(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	je	.LBB3_88
# BB#89:
	movq	%rax, %rcx
	movq	%rax, %r14
	.align	16, 0x90
.LBB3_90:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rcx
	incq	%r13
	testq	%rcx, %rcx
	jne	.LBB3_90
	jmp	.LBB3_91
.LBB3_88:
	movq	%rax, %r14
.LBB3_91:                               # %for.end
	movq	%r13, -136(%rbp)        # 8-byte Spill
	leaq	-112(%rbp), %rdi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	-344(%rbp), %rsi        # 8-byte Reload
	callq	init_cumulative_args
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$168, %rax, %rdx
	leaq	15(%rdx), %rax
	andq	$-16, %rax
	movq	%rsp, %rdi
	subq	%rax, %rdi
	movq	%rdi, -144(%rbp)        # 8-byte Spill
	movq	%rdi, %rsp
	xorq	%rsi, %rsi
	callq	memset
	movq	$0, -72(%rbp)
	movq	$0, -80(%rbp)
	movq	%r14, %rsi
	testq	%rsi, %rsi
	je	.LBB3_92
# BB#93:                                # %for.body.lr.ph.i
	movl	target_flags(%rip), %edx
	andq	$6144, %rdx             # imm = 0x1800
	leaq	-1(%r13), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	$-1, %rcx
	je	.LBB3_95
# BB#94:                                # %for.body.lr.ph.i
	movq	$1, %rcx
.LBB3_95:                               # %for.body.lr.ph.i
	testq	%rdx, %rdx
	je	.LBB3_97
# BB#96:                                # %for.body.lr.ph.i
	xorq	%rax, %rax
.LBB3_97:                               # %for.body.lr.ph.i
	imulq	$168, %rax, %rax
	movq	-144(%rbp), %rdx        # 8-byte Reload
	leaq	152(%rax,%rdx), %r13
	imulq	$168, %rcx, %rax
	movq	%rax, -184(%rbp)        # 8-byte Spill
	xorq	%rbx, %rbx
	movq	$0, -168(%rbp)          # 8-byte Folded Spill
	movq	%rsi, %r14
	movq	%rsi, -200(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB3_98:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rax
	movq	8(%rax), %r15
	movq	global_trees(%rip), %rcx
	movq	%rax, -152(%r13)
	movq	-160(%rbp), %rax        # 8-byte Reload
	movq	%rax, -152(%rbp)        # 8-byte Spill
	cmpq	%rcx, %r15
	je	.LBB3_100
# BB#99:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_98 Depth=1
	cmpq	$0, 32(%r15)
	jne	.LBB3_101
.LBB3_100:                              # %if.then9.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	global_trees+88(%rip), %rax
	movq	integer_types+40(%rip), %r15
	movq	%rax, -152(%r13)
.LBB3_101:                              # %if.end13.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	16(%r15), %eax
	cmpq	$21, %rax
	jne	.LBB3_104
# BB#102:                               # %land.lhs.true16.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	62(%r15), %eax
	testq	$8, %rax
	je	.LBB3_104
# BB#103:                               # %if.then21.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	24(%r15), %rax
	movq	8(%rax), %r15
.LBB3_104:                              # %if.end25.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rbx, -280(%rbp)        # 8-byte Spill
	movq	32(%r15), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	je	.LBB3_106
# BB#105:                               # %land.lhs.true33.i
                                        #   in Loop: Header=BB3_98 Depth=1
	callq	contains_placeholder_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_107
.LBB3_106:                              # %lor.lhs.false37.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	17(%r15), %eax
	testq	$4, %rax
	je	.LBB3_126
.LBB3_107:                              # %if.then43.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	cfun(%rip), %rcx
	movq	-152(%r13), %rax
	movzbl	425(%rcx), %edx
	movzbl	16(%rax), %ecx
	testq	$16, %rdx
	jne	.LBB3_108
# BB#115:                               # %if.else124.i
                                        #   in Loop: Header=BB3_98 Depth=1
	cmpq	$50, %rcx
	je	.LBB3_114
# BB#116:                               # %if.else144.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB3_120
# BB#117:                               # %lor.lhs.false149.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB3_120
# BB#118:                               # %lor.lhs.false158.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	flag_stack_check(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_123
# BB#119:                               # %land.lhs.true160.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	movq	$4092, %rax             # imm = 0xFFC
	subq	%rdx, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
	movq	40(%r15), %rdi
	movabsq	$2951479051793528259, %rcx # imm = 0x28F5C28F5C28F5C3
	mulq	%rcx
	movq	$2, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_123
.LBB3_120:                              # %if.then170.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	32(%r14), %rdi
	callq	expr_size
	cmpq	$0, -120(%rbp)
	movq	%rax, %rbx
	jne	.LBB3_122
# BB#121:                               # %if.then176.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	leaq	-120(%rbp), %rsi
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, -280(%rbp)        # 8-byte Spill
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.LBB3_122:                              # %if.end179.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	64(%r15), %edx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	allocate_dynamic_stack_space
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %rbx
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	set_mem_attributes
	jmp	.LBB3_124
	.align	16, 0x90
.LBB3_108:                              # %if.then48.i
                                        #   in Loop: Header=BB3_98 Depth=1
	cmpq	$50, %rcx
	jne	.LBB3_114
# BB#109:                               # %land.lhs.true57.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	40(%rax), %rdi
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB3_114
# BB#110:                               # %land.lhs.true70.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	144(%rdi), %rax
	testq	%rax, %rax
	jne	.LBB3_112
# BB#111:                               # %cond.false.i1895
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	-152(%r13), %rax
	movq	40(%rax), %rdi
	movq	144(%rdi), %rax
.LBB3_112:                              # %cond.end.i1897
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	je	.LBB3_114
# BB#113:                               # %if.then104.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rdi, -152(%r13)
	.align	16, 0x90
.LBB3_114:                              # %if.end114.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%r15, %rdi
	callq	build_pointer_type
	movq	-152(%r13), %rdx
	movq	$121, %rdi
	movq	%rax, %rsi
	jmp	.LBB3_125
.LBB3_123:                              # %if.else183.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rsi, %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	assign_temp
	movq	%rax, %rbx
.LBB3_124:                              # %if.end185.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	-152(%r13), %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	store_expr
	movabsq	$4294962686, %rax       # imm = 0xFFFFEDFE
	andq	%rax, -152(%rbp)        # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	build_pointer_type
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	make_tree
	movq	$121, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
.LBB3_125:                              # %if.end200.i
                                        #   in Loop: Header=BB3_98 Depth=1
	callq	build1
	movq	%rax, -152(%r13)
	movq	%r15, %rdi
	callq	build_pointer_type
	movq	%rax, %r15
.LBB3_126:                              # %if.end200.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	60(%r15), %r12d
	movq	$9, %rcx
	shrq	%cl, %r12
	andq	$127, %r12
	movl	16(%r15), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, -104(%r13)
	movl	%r12d, -144(%r13)
	movq	$32, %rcx
	movq	-136(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-168(%rbp), %rdx        # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movabsq	$0, %rax
	jge	.LBB3_128
# BB#127:                               # %if.end200.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$1, %rax
.LBB3_128:                              # %if.end200.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rax, -176(%rbp)        # 8-byte Spill
	leaq	-112(%rbp), %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	function_arg
	testq	%rax, %rax
	movq	%rax, -120(%r13)
	movq	%rax, -112(%r13)
	je	.LBB3_130
# BB#129:                               # %if.then230.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rcx, %rcx
	movl	%ecx, -100(%r13)
.LBB3_130:                              # %if.end233.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rbx, %rbx
	testq	%r15, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB3_139
# BB#131:                               # %land.rhs.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	32(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$25, %rcx
	jne	.LBB3_132
# BB#133:                               # %lor.lhs.false244.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	17(%r15), %ecx
	testq	$4, %rcx
	jne	.LBB3_134
# BB#135:                               # %lor.lhs.false244.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%r12, %rcx
	andq	%rdx, %rcx
	cmpq	$6, %rcx
	je	.LBB3_136
# BB#137:                               # %lor.rhs.i
                                        #   in Loop: Header=BB3_98 Depth=1
	cmpq	$51, %rcx
	jne	.LBB3_139
# BB#138:                               # %land.lhs.true268.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%r15, %rdi
	callq	int_size_in_bytes
	movq	-120(%r13), %rax
	jmp	.LBB3_139
	.align	16, 0x90
.LBB3_132:                              #   in Loop: Header=BB3_98 Depth=1
	movq	$1, %rbx
	jmp	.LBB3_139
.LBB3_134:                              #   in Loop: Header=BB3_98 Depth=1
	movq	$1, %rbx
	jmp	.LBB3_139
.LBB3_136:                              #   in Loop: Header=BB3_98 Depth=1
	movq	$1, %rbx
	.align	16, 0x90
.LBB3_139:                              # %land.end278.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	%ebx, -96(%r13)
	testq	%rax, %rax
	je	.LBB3_143
# BB#140:                               # %land.lhs.true285.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$39, %rcx
	jne	.LBB3_143
# BB#141:                               # %land.lhs.true293.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	cmpq	$0, 8(%rcx)
	jne	.LBB3_143
# BB#142:                               # %if.then303.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	$1, %rbx
	movl	%ebx, -96(%r13)
	.align	16, 0x90
.LBB3_143:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	testq	%rax, %rax
	movabsq	$0, %rdx
	jne	.LBB3_145
# BB#144:                               # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$1, %rdx
.LBB3_145:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movzbl	17(%r15), %ecx
	andq	$4, %rcx
	movabsq	$0, %rsi
	jne	.LBB3_147
# BB#146:                               # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$1, %rsi
.LBB3_147:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	testq	%rbx, %rbx
	movabsq	$0, %rdi
	jne	.LBB3_149
# BB#148:                               # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$1, %rdi
.LBB3_149:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	orq	%rdx, %rdi
	testq	%rdi, %rsi
	jne	.LBB3_151
# BB#150:                               # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	$1, -192(%rbp)          # 8-byte Folded Spill
.LBB3_151:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	testq	%rcx, %rcx
	movq	-152(%rbp), %rcx        # 8-byte Reload
	movq	%rcx, %rsi
	je	.LBB3_153
# BB#152:                               # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rcx, %rsi
	andq	$-4097, %rsi            # imm = 0xFFFFFFFFFFFFEFFF
.LBB3_153:                              # %if.end307.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$4294963199, %rdx       # imm = 0xFFFFEFFF
	andq	%rdx, %rcx
	movq	%rcx, -152(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	je	.LBB3_157
# BB#154:                               # %lor.lhs.false341.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	-100(%r13), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_157
# BB#155:                               # %lor.lhs.false341.i
                                        #   in Loop: Header=BB3_98 Depth=1
	testq	%rbx, %rbx
	jne	.LBB3_157
# BB#156:                               # %if.end366.thread.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rsi, -160(%rbp)        # 8-byte Spill
	movq	-80(%rbp), %rax
	movq	-72(%rbp), %rcx
	movq	%rcx, -64(%r13)
	movq	-56(%rbp), %rcx
	movq	-48(%rbp), %rdx
	movq	%rax, -72(%r13)
	movq	%rdx, 8(%r13)
	movq	%rcx, (%r13)
	xorq	%rax, %rax
	jmp	.LBB3_161
	.align	16, 0x90
.LBB3_157:                              # %if.end366.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rsi, -160(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB3_159
# BB#158:                               # %if.end366.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movabsq	$1, %rdx
.LBB3_159:                              # %if.end366.i
                                        #   in Loop: Header=BB3_98 Depth=1
	leaq	-56(%r13), %rax
	leaq	-88(%r13), %r9
	subq	$16, %rsp
	leaq	-56(%rbp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	-224(%rbp), %rcx        # 8-byte Reload
	leaq	-80(%rbp), %r8
	callq	locate_and_pad_parm
	addq	$16, %rsp
	movl	-96(%r13), %eax
	testq	%rax, %rax
	movq	-80(%rbp), %rax
	movq	-72(%rbp), %rcx
	movq	-56(%rbp), %rdx
	movq	-48(%rbp), %rsi
	movq	%rcx, -64(%r13)
	movq	%rax, -72(%r13)
	movq	%rsi, 8(%r13)
	movq	%rdx, (%r13)
	jne	.LBB3_652
# BB#160:                               # %if.end366.i.if.then379.i_crit_edge
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	-100(%r13), %eax
.LBB3_161:                              # %if.then379.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	target_flags(%rip), %esi
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$23, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	addq	$4, %rdi
	imulq	%rax, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$20, %rcx
	shrq	%cl, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	cqto
	idivq	%rsi
	subq	%rdx, %rdi
	movq	$32, %rcx
	movq	-56(%r13), %rax
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	subq	%rdi, %rax
	movq	%rax, -56(%r13)
	jmp	.LBB3_162
	.align	16, 0x90
.LBB3_652:                              # %if.end366.if.end402_crit_edge.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	-56(%r13), %rax
.LBB3_162:                              # %if.end402.i
                                        #   in Loop: Header=BB3_98 Depth=1
	addq	%rax, -80(%rbp)
	movq	-48(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB3_168
# BB#163:                               # %do.body.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_164
# BB#165:                               # %if.else425.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	-72(%rbp), %r12
	movq	sizetype_tab+8(%rip), %rdi
	movq	%rbx, %rsi
	callq	convert
	testq	%r12, %r12
	je	.LBB3_167
# BB#166:                               # %if.else432.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
.LBB3_167:                              # %if.end439.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movq	%rax, -72(%rbp)
	jmp	.LBB3_168
	.align	16, 0x90
.LBB3_164:                              # %if.then421.i
                                        #   in Loop: Header=BB3_98 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	tree_low_cst
	addq	%rax, -80(%rbp)
.LBB3_168:                              # %if.end439.i
                                        #   in Loop: Header=BB3_98 Depth=1
	movl	60(%r15), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	leaq	-112(%rbp), %rdi
	movq	%r15, %rdx
	movq	-176(%rbp), %rcx        # 8-byte Reload
	callq	function_arg_advance
	movq	(%r14), %r14
	incq	-168(%rbp)              # 8-byte Folded Spill
	movq	-184(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r13
	testq	%r14, %r14
	movq	-280(%rbp), %rbx        # 8-byte Reload
	jne	.LBB3_98
# BB#169:                               # %initialize_argument_information.exit
	cmpq	$0, -72(%rbp)
	movq	$1, -448(%rbp)          # 8-byte Folded Spill
	movq	-216(%rbp), %r12        # 8-byte Reload
	movq	-136(%rbp), %r13        # 8-byte Reload
	jne	.LBB3_196
	jmp	.LBB3_170
.LBB3_92:
	movq	%rsi, -200(%rbp)        # 8-byte Spill
.LBB3_170:                              # %if.end204
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rsi, %r14
	movq	-192(%rbp), %rax        # 8-byte Reload
	testq	%r14, %rax
	je	.LBB3_172
# BB#171:
	movq	-160(%rbp), %rcx        # 8-byte Reload
	movq	%rcx, -152(%rbp)        # 8-byte Spill
	movq	%rax, -448(%rbp)        # 8-byte Spill
	jmp	.LBB3_196
.LBB3_172:                              # %for.cond.preheader.i
	movq	%rbx, -280(%rbp)        # 8-byte Spill
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	movq	%r13, -136(%rbp)        # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$0, -168(%rbp)          # 8-byte Folded Spill
	testq	%rax, %rax
	movabsq	$0, %rsi
	movq	-144(%rbp), %r15        # 8-byte Reload
	jle	.LBB3_189
# BB#173:                               # %for.body.i1904.preheader
	leaq	56(%r15), %rbx
	movq	$0, -168(%rbp)          # 8-byte Folded Spill
	movq	$1, %r12
	xorq	%rax, %rax
	.align	16, 0x90
.LBB3_174:                              # %for.body.i1904
                                        # =>This Inner Loop Header: Depth=1
	movslq	-4(%rbx), %rcx
	testq	%rcx, %rcx
	jle	.LBB3_177
# BB#175:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movl	(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_177
# BB#176:                               #   in Loop: Header=BB3_174 Depth=1
	movq	$1, %r13
	xorq	%rsi, %rsi
	jmp	.LBB3_181
	.align	16, 0x90
.LBB3_177:                              # %if.else.i1906
                                        #   in Loop: Header=BB3_174 Depth=1
	xorq	%rsi, %rsi
	testq	%r14, %rax
	movabsq	$0, %r13
	je	.LBB3_181
# BB#178:                               # %land.lhs.true8.i
                                        #   in Loop: Header=BB3_174 Depth=1
	cmpq	$0, -24(%rbx)
	movabsq	$0, %rsi
	jne	.LBB3_180
# BB#179:                               # %land.lhs.true8.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movabsq	$1, %rsi
.LBB3_180:                              # %land.lhs.true8.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	%rax, %r13
.LBB3_181:                              # %if.end13.i1911
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	-56(%rbx), %rax
	movq	8(%rax), %rdi
	movl	60(%rdi), %ecx
	andq	$65024, %rcx            # imm = 0xFE00
	cmpq	$26112, %rcx            # imm = 0x6600
	jne	.LBB3_187
# BB#182:                               # %land.lhs.true18.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	%rsi, -152(%rbp)        # 8-byte Spill
	movzbl	16(%rax), %ecx
	cmpq	$53, %rcx
	ja	.LBB3_184
# BB#183:                               # %land.lhs.true18.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$12384898975268864, %rcx # imm = 0x2C000000000000
	testq	%rcx, %rax
	jne	.LBB3_185
	.align	16, 0x90
.LBB3_184:                              # %lor.lhs.false43.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movzbl	17(%rdi), %eax
	testq	$4, %rax
	je	.LBB3_186
.LBB3_185:                              # %if.then54.i
                                        #   in Loop: Header=BB3_174 Depth=1
	callq	int_size_in_bytes
	movq	-168(%rbp), %rcx        # 8-byte Reload
	andq	%r14, %rcx
	addq	%rax, %rcx
	movq	%rcx, -168(%rbp)        # 8-byte Spill
.LBB3_186:                              # %for.inc.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	-152(%rbp), %rsi        # 8-byte Reload
.LBB3_187:                              # %for.inc.i
                                        #   in Loop: Header=BB3_174 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-136(%rbp), %rdx        # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB3_189
# BB#188:                               # %for.inc.i
                                        #   in Loop: Header=BB3_174 Depth=1
	incq	%r12
	addq	$168, %rbx
	testq	%rsi, %rsi
	movq	%r13, %rax
	je	.LBB3_174
.LBB3_189:                              # %for.end.i
	movq	%r15, -144(%rbp)        # 8-byte Spill
	movq	$33, %rcx
	movq	-168(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	-80(%rbp), %rdi
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jl	.LBB3_191
# BB#190:                               # %for.end.i
	movq	%rdx, %rcx
.LBB3_191:                              # %for.end.i
	testq	%rdi, %rdi
	movq	-216(%rbp), %r12        # 8-byte Reload
	movq	-280(%rbp), %rbx        # 8-byte Reload
	jle	.LBB3_193
# BB#192:                               # %for.end.i
	movq	%rdx, %rax
.LBB3_193:                              # %for.end.i
	testq	%rax, %rcx
	movq	$1, -448(%rbp)          # 8-byte Folded Spill
	movq	-136(%rbp), %r13        # 8-byte Reload
	movq	-160(%rbp), %rax        # 8-byte Reload
	jne	.LBB3_195
# BB#194:                               # %for.end.i
	movq	%rsi, -448(%rbp)        # 8-byte Spill
.LBB3_195:                              # %for.end.i
	movq	%rax, -152(%rbp)        # 8-byte Spill
.LBB3_196:                              # %finalize_must_preallocate.exit
	xorq	%r14, %r14
	movq	-208(%rbp), %rcx        # 8-byte Reload
	testq	%rcx, %rcx
	movabsq	$0, %rax
	movq	%rax, -248(%rbp)        # 8-byte Spill
	je	.LBB3_206
# BB#197:                               # %land.lhs.true207
	movq	global_rtl+64(%rip), %rdi
	movq	%rcx, %r15
	movq	%r15, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_200
# BB#198:                               # %lor.lhs.false210
	movq	global_rtl+72(%rip), %rdi
	movq	%r15, %rsi
	movq	%rbx, %r14
	movq	%rcx, %rbx
	callq	reg_mentioned_p
	testq	%rbx, %rax
	movq	%r14, %rbx
	movabsq	$0, %r14
	je	.LBB3_199
.LBB3_200:                              # %land.lhs.true213
	cmpq	$0, -72(%rbp)
	movq	%r15, %rdi
	jne	.LBB3_205
# BB#201:                               # %lor.lhs.false216
	movzbl	target_flags+1(%rip), %eax
	testq	$16, %rax
	jne	.LBB3_202
# BB#203:                               # %lor.lhs.false216
	movq	-80(%rbp), %rax
	testq	%rax, %rax
	je	.LBB3_204
.LBB3_205:                              # %if.then221
	callq	copy_to_reg
	movq	%rax, -248(%rbp)        # 8-byte Spill
	jmp	.LBB3_206
.LBB3_199:
	movq	%r15, -248(%rbp)        # 8-byte Spill
	jmp	.LBB3_206
.LBB3_202:
	movq	%rdi, -248(%rbp)        # 8-byte Spill
	jmp	.LBB3_206
.LBB3_204:
	movq	%rdi, -248(%rbp)        # 8-byte Spill
.LBB3_206:                              # %if.end223
	movq	%rbx, -280(%rbp)        # 8-byte Spill
	movl	expand_call.currently_expanding_call(%rip), %eax
	testq	%rax, %rax
	leaq	1(%rax), %rax
	movl	%eax, expand_call.currently_expanding_call(%rip)
	movq	$1, %rbx
	jne	.LBB3_207
# BB#208:                               # %if.end223
	movl	flag_optimize_sibling_calls(%rip), %eax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	movq	-272(%rbp), %rsi        # 8-byte Reload
	je	.LBB3_216
# BB#209:                               # %if.end223
	movl	rtx_equal_function_value_matters(%rip), %eax
	testq	%r8, %rax
	je	.LBB3_216
# BB#210:                               # %lor.lhs.false231
	movq	%rsi, %r14
	movq	$1, %rdi
	movq	$1, %rbx
	movq	%r8, %r15
	callq	any_pending_cleanups
	movq	%r15, %r8
	testq	%r8, %rax
	jne	.LBB3_211
# BB#212:                               # %lor.lhs.false231
	movq	-72(%rbp), %rax
	testq	%rax, %rax
	movq	%r14, %rsi
	movabsq	$0, %r14
	jne	.LBB3_216
# BB#213:                               # %lor.lhs.false240
	movq	32(%rsi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB3_214
# BB#215:                               # %lor.lhs.false250
	movq	32(%rax), %rax
	movq	current_function_decl(%rip), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	movq	$1, %r14
	movq	$1, %r9
	jne	.LBB3_216
	jmp	.LBB3_217
.LBB3_207:
	movq	-272(%rbp), %rsi        # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	jmp	.LBB3_216
.LBB3_211:
	movq	%r14, %rsi
	xorq	%r14, %r14
	jmp	.LBB3_216
.LBB3_214:
	movq	$1, %r14
	xorq	%rbx, %rbx
.LBB3_216:                              # %if.then259
	xorq	%r9, %r9
.LBB3_217:                              # %if.end260
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB3_219
# BB#218:                               # %if.end260
	movq	%rdx, %rcx
.LBB3_219:                              # %if.end260
	cmpq	$0, -224(%rbp)          # 8-byte Folded Reload
	je	.LBB3_221
# BB#220:                               # %if.end260
	movq	%rdx, %rax
.LBB3_221:                              # %if.end260
	testq	$96, -152(%rbp)         # 8-byte Folded Reload
	jne	.LBB3_234
# BB#222:                               # %if.end260
	notq	%rbx
	andq	%rcx, %rbx
	andq	%rax, %rbx
	je	.LBB3_234
# BB#223:                               # %lor.lhs.false271
	movq	-224(%rbp), %rax        # 8-byte Reload
	movl	16(%rax), %eax
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB3_234
# BB#224:                               # %lor.lhs.false271
	cmpq	$0, -224(%rbp)          # 8-byte Folded Reload
	je	.LBB3_234
# BB#225:                               # %land.lhs.true279
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB3_227
# BB#226:                               # %land.lhs.true279
	andq	$524288, %rax           # imm = 0x80000
	testq	%r8, %rax
	jne	.LBB3_234
.LBB3_227:                              # %land.lhs.true287
	movzbl	target_flags(%rip), %eax
	testq	$32, %rax
	je	.LBB3_232
# BB#228:                               # %lor.lhs.false290
	movq	-224(%rbp), %rax        # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %ecx
	cmpq	$8, %rcx
	ja	.LBB3_232
# BB#229:                               # %lor.lhs.false290
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$324, %rax              # imm = 0x144
	je	.LBB3_232
# BB#230:                               # %lor.lhs.false330
	movq	cfun(%rip), %rax
	movq	48(%rax), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %ecx
	cmpq	$8, %rcx
	ja	.LBB3_234
# BB#231:                               # %lor.lhs.false330
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$324, %rax              # imm = 0x144
	je	.LBB3_234
.LBB3_232:                              # %lor.lhs.false375
	movq	cfun(%rip), %rax
	movq	-80(%rbp), %rdx
	movslq	68(%rax), %rax
	cmpq	%rax, %rdx
	jg	.LBB3_234
# BB#233:                               # %lor.lhs.false381
	movq	-224(%rbp), %rdi        # 8-byte Reload
	movq	-344(%rbp), %rsi        # 8-byte Reload
	movq	%r14, -352(%rbp)        # 8-byte Spill
	movq	%r8, %r15
	movq	%r9, %rbx
	callq	ix86_return_pops_args
	movq	current_function_decl(%rip), %rdi
	movq	cfun(%rip), %rcx
	movq	8(%rdi), %rsi
	movl	68(%rcx), %edx
	movq	%r12, %r14
	movq	%rax, %r12
	andq	%r15, %r12
	callq	ix86_return_pops_args
	movq	%rbx, %r9
	movq	%r15, %r8
	movq	-352(%rbp), %rbx        # 8-byte Reload
	movq	-272(%rbp), %rsi        # 8-byte Reload
	andq	%r8, %rax
	cmpq	%rax, %r12
	movq	%r14, %r12
	je	.LBB3_235
.LBB3_234:                              # %if.then391
	xorq	%rbx, %rbx
.LBB3_235:                              # %if.end392
	movq	%rbx, %rax
	orq	%r9, %rax
	je	.LBB3_236
# BB#237:                               # %if.then396
	movq	%r9, -184(%rbp)         # 8-byte Spill
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movl	target_flags(%rip), %ecx
	leaq	-1(%r13), %rdx
	xorq	%rax, %rax
	andq	$6144, %rcx             # imm = 0x1800
	movabsq	$0, %rdi
	je	.LBB3_239
# BB#238:                               # %if.then396
	movq	%rdx, %rdi
.LBB3_239:                              # %if.then396
	movq	%rbx, -352(%rbp)        # 8-byte Spill
	movq	%rsi, -272(%rbp)        # 8-byte Spill
	movq	%r13, %rbx
	je	.LBB3_241
# BB#240:                               # %if.then396
	movq	$-1, %rbx
.LBB3_241:                              # %if.then396
	movq	%rdi, %rdx
	andq	%r8, %rdx
	andq	%r8, %rbx
	movq	%rbx, -168(%rbp)        # 8-byte Spill
	cmpq	%rbx, %rdx
	je	.LBB3_260
# BB#242:                               # %for.body409.lr.ph
	andq	%r8, %rcx
	cmpq	$1, %rcx
	sbbq	%rdx, %rdx
	notq	%rdx
	orq	$1, %rdx
	movq	%rdx, -160(%rbp)        # 8-byte Spill
	movq	-184(%rbp), %rax        # 8-byte Reload
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	(%rdx,%rdi), %r15
	testq	%rax, %rax
	je	.LBB3_248
# BB#243:                               # %for.body409.preheader
	imulq	$168, %rdi, %r14
	movq	-144(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r14
	imulq	$168, %rdx, %rax
	movq	%rax, -176(%rbp)        # 8-byte Spill
	xorq	%rax, %rax
	.align	16, 0x90
.LBB3_244:                              # %for.body409
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r12
	movq	(%r14), %rdi
	movq	%r8, %rbx
	callq	unsafe_for_reeval
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB3_259
# BB#245:                               # %for.body409
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	%r13, %rbx
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB3_246
# BB#257:                               # %sw.bb420
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %rdx
	movq	$34, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	(%r14), %rdi
	movq	%rax, %r13
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, 144(%r13)
	movq	%r13, (%r14)
	jmp	.LBB3_258
	.align	16, 0x90
.LBB3_246:                              # %for.body409
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	%rcx, %r13
	cmpq	$1, %rax
	jne	.LBB3_256
# BB#247:                               # %sw.bb
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	(%r14), %rdi
	callq	unsave_expr
	movq	%rax, (%r14)
	movq	%r13, %rcx
.LBB3_258:                              # %for.inc443
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	%rbx, %r13
.LBB3_259:                              # %for.inc443
                                        #   in Loop: Header=BB3_244 Depth=1
	movq	(%r14), %rsi
	xorq	%rdi, %rdi
	movq	%r12, %rdx
	movq	%rcx, %rbx
	callq	tree_cons
	movq	%rbx, %r8
	movq	%r15, %rcx
	andq	%r8, %rcx
	movq	-160(%rbp), %rdx        # 8-byte Reload
	addq	%rdx, %r15
	movq	-176(%rbp), %rdx        # 8-byte Reload
	addq	%rdx, %r14
	movq	-168(%rbp), %rdx        # 8-byte Reload
	cmpq	%rdx, %rcx
	jne	.LBB3_244
	jmp	.LBB3_260
.LBB3_236:
	movq	%r8, %r12
	movq	%rsi, -272(%rbp)        # 8-byte Spill
	movq	-152(%rbp), %r14        # 8-byte Reload
	jmp	.LBB3_263
.LBB3_248:                              # %for.body409.us.preheader
	imulq	$168, %rdi, %r12
	movq	-144(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r12
	imulq	$168, %rdx, %rax
	movq	%rax, -176(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB3_249:                              # %for.body409.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdi
	movq	%r8, %r14
	callq	unsafe_for_reeval
	movq	%r14, %r8
	testq	%r8, %rax
	je	.LBB3_255
# BB#250:                               # %for.body409.us
                                        #   in Loop: Header=BB3_249 Depth=1
	andq	%r8, %rax
	cmpq	$1, %rax
	jne	.LBB3_251
# BB#253:                               # %sw.bb.us
                                        #   in Loop: Header=BB3_249 Depth=1
	movq	(%r12), %rdi
	movq	%r8, %r14
	callq	unsave_expr
	movq	%r14, %r8
	movq	%rax, (%r12)
	jmp	.LBB3_254
	.align	16, 0x90
.LBB3_251:                              # %for.body409.us
                                        #   in Loop: Header=BB3_249 Depth=1
	movq	%r13, %rbx
	movq	%r8, %r13
	cmpq	$2, %rax
	jne	.LBB3_256
# BB#252:                               # %sw.bb420.us
                                        #   in Loop: Header=BB3_249 Depth=1
	movq	(%r12), %rax
	movq	8(%rax), %rdx
	movq	$34, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	(%r12), %rdi
	movq	%rax, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, 144(%r14)
	movq	%r14, (%r12)
	movq	%r13, %r8
	movq	%rbx, %r13
.LBB3_254:                              # %for.inc443.us
                                        #   in Loop: Header=BB3_249 Depth=1
	movq	-168(%rbp), %rbx        # 8-byte Reload
.LBB3_255:                              # %for.inc443.us
                                        #   in Loop: Header=BB3_249 Depth=1
	movq	%r15, %rcx
	andq	%r8, %rcx
	movq	-160(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r15
	movq	-176(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r12
	xorq	%rax, %rax
	cmpq	%rbx, %rcx
	jne	.LBB3_249
.LBB3_260:                              # %for.end444
	movq	%rax, -200(%rbp)        # 8-byte Spill
	movq	$1, %rdi
	movq	%r8, %rbx
	callq	any_pending_cleanups
	movq	$0, -480(%rbp)          # 8-byte Folded Spill
	andq	%rbx, %rax
	movq	%rbx, %r12
	movq	-352(%rbp), %rbx        # 8-byte Reload
	je	.LBB3_262
# BB#261:                               # %for.end444
	xorq	%rbx, %rbx
.LBB3_262:                              # %for.end444
	testq	%rax, %rax
	movq	-152(%rbp), %r14        # 8-byte Reload
	movq	-184(%rbp), %r9         # 8-byte Reload
	jne	.LBB3_271
.LBB3_263:                              # %if.end449
	testq	%r9, %r9
	je	.LBB3_264
# BB#265:                               # %if.then451
	movq	%rbx, %r15
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, -152(%rbp)        # 8-byte Spill
	movl	8(%rax), %eax
	movq	%rax, -160(%rbp)        # 8-byte Spill
	callq	emit_queue
	callq	start_sequence
	callq	expand_start_target_temps
	callq	get_last_insn
	movq	-200(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	optimize_tail_recursion
	testq	%r12, %rax
	je	.LBB3_266
# BB#267:                               # %if.then457
	movq	$1, %rdi
	callq	any_pending_cleanups
	xorq	%rbx, %rbx
	testq	%r12, %rax
	movabsq	$0, %rax
	movq	%rax, -480(%rbp)        # 8-byte Spill
	jne	.LBB3_270
# BB#268:                               # %if.else461
	callq	get_insns
	movq	%rax, -480(%rbp)        # 8-byte Spill
	jmp	.LBB3_269
.LBB3_264:
	movq	$0, -480(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_271
.LBB3_266:
	movq	$0, -480(%rbp)          # 8-byte Folded Spill
.LBB3_269:                              # %if.end464
	movq	%r15, %rbx
.LBB3_270:                              # %if.end464
	callq	expand_end_target_temps
	callq	end_sequence
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	-152(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, (%rax)
	movq	-160(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, 8(%rax)
.LBB3_271:                              # %if.end469
	movl	profile_arc_flag(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_274
# BB#272:                               # %if.end469
	movq	%r14, %rax
	andq	$128, %rax
	je	.LBB3_274
# BB#273:                               # %if.then474
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movabsq	$.L.str4, %rdx
	movq	$68, %rdi
	callq	gen_rtx_fmt_s
	movq	%rax, %rdi
	movq	$7, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	callq	emit_library_call
.LBB3_274:                              # %if.end479
	movq	%rbx, -352(%rbp)        # 8-byte Spill
	movq	%r14, -152(%rbp)        # 8-byte Spill
	movq	cfun(%rip), %rax
	movslq	404(%rax), %rcx
	movq	-416(%rbp), %rdx        # 8-byte Reload
	cmpq	%rdx, %rcx
	jge	.LBB3_277
# BB#275:                               # %if.end479
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, -224(%rbp)        # 8-byte Folded Reload
	je	.LBB3_277
# BB#276:                               # %if.then487
	movq	-416(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, 404(%rax)
.LBB3_277:                              # %if.end490
	movq	$63, %rcx
	movq	-416(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rsi
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	addq	%rdx, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	movq	%rsi, -432(%rbp)        # 8-byte Spill
	movl	152(%rax), %ecx
	incq	%rcx
	movl	%ecx, 152(%rax)
	movq	-144(%rbp), %rax        # 8-byte Reload
	leaq	56(%rax), %rcx
	movq	%rcx, -464(%rbp)        # 8-byte Spill
	leaq	120(%rax), %rcx
	movq	%rcx, -440(%rbp)        # 8-byte Spill
	leaq	24(%rax), %rcx
	movq	%rcx, -472(%rbp)        # 8-byte Spill
	leaq	128(%rax), %rcx
	movq	%rcx, -456(%rbp)        # 8-byte Spill
	leaq	136(%rax), %rax
	movq	%rax, -296(%rbp)        # 8-byte Spill
	movq	$0, -424(%rbp)          # 8-byte Folded Spill
	xorq	%r14, %r14
	xorq	%rdx, %rdx
	movq	$0, -408(%rbp)          # 8-byte Folded Spill
	movq	%r12, %rbx
	.align	16, 0x90
.LBB3_278:                              # %for.body495.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_279 Depth 2
                                        #     Child Loop BB3_320 Depth 2
                                        #     Child Loop BB3_315 Depth 2
                                        #     Child Loop BB3_378 Depth 2
                                        #     Child Loop BB3_386 Depth 2
                                        #     Child Loop BB3_433 Depth 2
                                        #     Child Loop BB3_452 Depth 2
                                        #       Child Loop BB3_460 Depth 3
                                        #       Child Loop BB3_467 Depth 3
                                        #     Child Loop BB3_480 Depth 2
                                        #       Child Loop BB3_488 Depth 3
                                        #       Child Loop BB3_495 Depth 3
                                        #     Child Loop BB3_514 Depth 2
                                        #       Child Loop BB3_528 Depth 3
                                        #     Child Loop BB3_554 Depth 2
                                        #     Child Loop BB3_566 Depth 2
                                        #     Child Loop BB3_619 Depth 2
                                        #     Child Loop BB3_630 Depth 2
                                        #     Child Loop BB3_638 Depth 2
	movq	-352(%rbp), %rax        # 8-byte Reload
	movq	-216(%rbp), %r12        # 8-byte Reload
	jmp	.LBB3_279
	.align	16, 0x90
.LBB3_281:                              # %for.inc1359.thread
                                        #   in Loop: Header=BB3_279 Depth=2
	incq	%rdx
.LBB3_279:                              # %for.body495
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rcx, %rcx
	testq	%rbx, %rdx
	movabsq	$0, %rsi
	jne	.LBB3_300
# BB#280:                               # %if.then500
                                        #   in Loop: Header=BB3_279 Depth=2
	testq	%rbx, %rax
	je	.LBB3_281
# BB#299:                               # %if.end503
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rdx, %r15
	callq	emit_queue
	movq	%r15, %rdx
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movl	8(%rax), %esi
.LBB3_300:                              # %if.end508
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rsi, -320(%rbp)        # 8-byte Spill
	movq	%rcx, -312(%rbp)        # 8-byte Spill
	movq	%r14, -304(%rbp)        # 8-byte Spill
	movq	%rdx, %r14
	movq	%rdx, %r15
	andq	%rbx, %r14
	jne	.LBB3_301
# BB#302:                               # %if.end508
                                        #   in Loop: Header=BB3_278 Depth=1
	orq	$256, -152(%rbp)        # 8-byte Folded Spill
                                        # imm = 0x100
	jmp	.LBB3_303
	.align	16, 0x90
.LBB3_301:                              #   in Loop: Header=BB3_278 Depth=1
	andq	$-257, -152(%rbp)       # 8-byte Folded Spill
                                        # imm = 0xFFFFFFFFFFFFFEFF
.LBB3_303:                              # %if.end508
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$0, -128(%rbp)
	callq	start_sequence
	testq	%r14, %r14
	movq	%r14, -160(%rbp)        # 8-byte Spill
	jne	.LBB3_305
# BB#304:                               # %if.then517
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	expand_start_target_temps
.LBB3_305:                              # %if.end518
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r13, %r14
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movslq	(%rax), %rax
	cmpq	$31, %rax
	jg	.LBB3_309
# BB#306:                               # %lor.lhs.false523
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rax, %rax
	jle	.LBB3_308
# BB#307:                               # %land.lhs.true528
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$1032, -152(%rbp)       # 8-byte Folded Reload
                                        # imm = 0x408
	jne	.LBB3_309
.LBB3_308:                              # %lor.lhs.false531
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rbx, %r15
	jne	.LBB3_310
.LBB3_309:                              # %if.then534
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	do_pending_stack_adjust
.LBB3_310:                              # %if.end535
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rbx, %r15
	je	.LBB3_326
# BB#311:                               # %land.lhs.true537
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, -168(%rbp)        # 8-byte Spill
	movq	%rbx, %r13
	movq	-152(%rbp), %rax        # 8-byte Reload
	andq	$4096, %rax             # imm = 0x1000
	je	.LBB3_313
# BB#312:                               # %if.then540
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	movl	4(%rcx), %edx
	incq	%rdx
	movl	%edx, 4(%rcx)
.LBB3_313:                              # %if.then546
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB3_324
# BB#314:                               # %for.body.lr.ph.i1924
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$1, %r15
	testq	%r13, %rax
	movq	-144(%rbp), %rax        # 8-byte Reload
	movq	%rax, %r12
	movq	%rax, %rbx
	jne	.LBB3_320
	.align	16, 0x90
.LBB3_315:                              # %lor.lhs.false.us.i
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	target_flags(%rip), %esi
	movq	(%r12), %rdi
	movq	$12, %rcx
	shrq	%cl, %rsi
	notq	%rsi
	andq	$1, %rsi
	movq	$0, calls_function_save_exprs(%rip)
	callq	calls_function_1
	testq	%r13, %rax
	movq	$0, calls_function_save_exprs(%rip)
	je	.LBB3_319
# BB#316:                               # %if.then.us.i
                                        #   in Loop: Header=BB3_315 Depth=2
	movq	(%r12), %rdi
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	testq	$4, %rax
	jne	.LBB3_653
# BB#317:                               # %if.end.us.i
                                        #   in Loop: Header=BB3_315 Depth=2
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, 16(%r12)
	callq	emit_queue
	movq	16(%r12), %rdi
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	%rax, 16(%r12)
	movq	%rax, 24(%r12)
	movq	(%r12), %rcx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %esi
	movl	8(%r12), %edi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rdi, %rsi
	je	.LBB3_319
# BB#318:                               # %if.then39.us.i
                                        #   in Loop: Header=BB3_315 Depth=2
	movl	48(%r12), %ecx
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, 16(%r12)
.LBB3_319:                              # %for.inc.us.i
                                        #   in Loop: Header=BB3_315 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$168, %r12
	cmpq	%rdx, %rax
	jl	.LBB3_315
	jmp	.LBB3_324
	.align	16, 0x90
.LBB3_320:                              # %if.then.i1931
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rdi
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	testq	$4, %rax
	jne	.LBB3_653
# BB#321:                               # %if.end.i
                                        #   in Loop: Header=BB3_320 Depth=2
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, 16(%rbx)
	callq	emit_queue
	movq	16(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	%rax, 16(%rbx)
	movq	%rax, 24(%rbx)
	movq	(%rbx), %rcx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %esi
	movl	8(%rbx), %edi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rdi, %rsi
	je	.LBB3_323
# BB#322:                               # %if.then39.i
                                        #   in Loop: Header=BB3_320 Depth=2
	movl	48(%rbx), %ecx
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, 16(%rbx)
.LBB3_323:                              # %for.inc.i1937
                                        #   in Loop: Header=BB3_320 Depth=2
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$168, %rbx
	cmpq	%rdx, %rax
	jl	.LBB3_320
.LBB3_324:                              # %precompute_arguments.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$4100, -152(%rbp)       # 8-byte Folded Reload
                                        # imm = 0x1004
	movq	%r13, %rbx
	movq	-168(%rbp), %r15        # 8-byte Reload
	movq	-216(%rbp), %r12        # 8-byte Reload
	je	.LBB3_326
# BB#325:                               # %if.then552
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	start_sequence
.LBB3_326:                              # %if.end553
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-80(%rbp), %rax
	movq	%rax, -240(%rbp)        # 8-byte Spill
	movq	-72(%rbp), %rsi
	cmpq	$0, -160(%rbp)          # 8-byte Folded Reload
	movq	%r14, %r13
	je	.LBB3_328
# BB#327:                               # %if.end553
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-416(%rbp), %rax        # 8-byte Reload
	movq	%rax, -160(%rbp)        # 8-byte Spill
.LBB3_328:                              # %if.end553
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	target_flags(%rip), %eax
	testq	$4096, %rax             # imm = 0x1000
	jne	.LBB3_330
# BB#329:                               #   in Loop: Header=BB3_278 Depth=1
	movq	-160(%rbp), %r14        # 8-byte Reload
	jmp	.LBB3_333
	.align	16, 0x90
.LBB3_330:                              # %land.lhs.true.i1944
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movq	$32, %rcx
	movq	-160(%rbp), %rdi        # 8-byte Reload
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jle	.LBB3_332
# BB#331:                               # %land.lhs.true.i1944
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rdi, %rax
.LBB3_332:                              # %land.lhs.true.i1944
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, %r14
.LBB3_333:                              # %if.end.i1947
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, -168(%rbp)        # 8-byte Spill
	movq	%rbx, %r15
	testq	%rsi, %rsi
	jne	.LBB3_334
# BB#339:                               # %if.else.i1957
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$60, %rcx
	movq	%rdx, %rax
	movq	cfun(%rip), %rsi
	shrq	%cl, %rax
	andq	$7, %rax
	addq	%r14, %rax
	movq	16(%rsi), %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$35, %rcx
	movslq	8(%rsi), %rsi
	sarq	%cl, %rax
	cmpq	$8, %rdx
	movq	$1, %rcx
	movq	%r15, %rbx
	jl	.LBB3_341
# BB#340:                               # %if.else.i1957
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, %rcx
.LBB3_341:                              # %if.else.i1957
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-240(%rbp), %rax        # 8-byte Reload
	leaq	(%rax,%rcx), %rax
	leaq	-1(%rsi,%rax), %rdi
	movq	%rdi, %rax
	cqto
	idivq	%rcx
	subq	%rdx, %rdi
	xorq	%r9, %r9
	subq	%rsi, %rdi
	movq	%rdi, -232(%rbp)        # 8-byte Spill
	jns	.LBB3_343
# BB#342:                               # %if.else.i1957
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$0, -232(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_343
	.align	16, 0x90
.LBB3_334:                              # %cond.false.i1951
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	-240(%rbp), %rdi        # 8-byte Reload
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %r9
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$60, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$7, %rsi
	addq	%r14, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$35, %rcx
	sarq	%cl, %rsi
	movq	$0, -232(%rbp)          # 8-byte Folded Spill
	cmpq	$16, %rax
	jl	.LBB3_335
# BB#336:                               # %if.then23.i
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	8(%rax), %eax
	leaq	-1(%rsi), %rcx
	andq	%rax, %rcx
	testq	%r15, %rcx
	jne	.LBB3_337
# BB#338:                               # %if.end27.i
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r9, %rdi
	callq	round_up
	movq	%rax, %r9
	movq	%r15, %rbx
	jmp	.LBB3_343
	.align	16, 0x90
.LBB3_335:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %rbx
.LBB3_343:                              # %compute_argument_block_size.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movq	cfun(%rip), %rdx
	movq	16(%rdx), %rdi
	movl	8(%rdi), %eax
	movslq	(%rdi), %r8
	movq	%rax, %rsi
	subq	%r8, %rsi
	movq	-168(%rbp), %rcx        # 8-byte Reload
	testq	%rbx, %rcx
	jne	.LBB3_345
# BB#344:                               # %if.then569
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r9, -328(%rbp)         # 8-byte Spill
	movq	%rsi, -336(%rbp)        # 8-byte Spill
	movq	%rbx, %r15
	movq	global_rtl+48(%rip), %r14
	movq	-80(%rbp), %rdi
	callq	sbitmap_alloc
	movq	%rax, stored_args_map(%rip)
	movq	%rax, %rdi
	callq	sbitmap_zero
	jmp	.LBB3_381
	.align	16, 0x90
.LBB3_345:                              # %if.else573
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rsi, -336(%rbp)        # 8-byte Spill
	testq	%r9, %r9
	movq	-224(%rbp), %r15        # 8-byte Reload
	jne	.LBB3_346
# BB#349:                               # %if.else611
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r9, -328(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	-232(%rbp), %rsi        # 8-byte Reload
	movslq	76(%rdx), %r9
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r9, %rsi
	jle	.LBB3_351
# BB#350:                               # %if.then616
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-232(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, 76(%rdx)
.LBB3_351:                              # %if.end618
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-448(%rbp), %rcx        # 8-byte Reload
	testq	%rbx, %rcx
	jne	.LBB3_353
# BB#352:                               #   in Loop: Header=BB3_278 Depth=1
	movq	$1, -264(%rbp)          # 8-byte Folded Spill
	xorq	%r12, %r12
	jmp	.LBB3_403
	.align	16, 0x90
.LBB3_346:                              # %if.then577
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r15
	cmpq	$0, -120(%rbp)
	jne	.LBB3_348
# BB#347:                               # %if.end585
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	leaq	-120(%rbp), %rsi
	movq	%r9, %rbx
	callq	emit_stack_save
	movq	%rbx, %r9
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, -280(%rbp)        # 8-byte Spill
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movl	stack_arg_under_construction(%rip), %eax
	movq	%rax, -408(%rbp)        # 8-byte Spill
	movl	%ecx, stack_arg_under_construction(%rip)
.LBB3_348:                              # %cond.end608
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r9, -328(%rbp)         # 8-byte Spill
	movq	sizetype_tab+8(%rip), %rdi
	movq	%r9, %rsi
	callq	convert
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	-232(%rbp), %rdi        # 8-byte Reload
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	push_block
.LBB3_380:                              # %if.end707
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, %r14
	jmp	.LBB3_381
.LBB3_353:                              # %if.then620
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	target_flags+1(%rip), %ecx
	testq	$16, %rcx
	jne	.LBB3_354
# BB#361:                               # %if.else644
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	4(%rdi), %ecx
	testq	%rcx, %rcx
	movq	-232(%rbp), %rsi        # 8-byte Reload
	jne	.LBB3_369
# BB#362:                               # %if.then649
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-240(%rbp), %rcx        # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	movq	-432(%rbp), %r9         # 8-byte Reload
	idivq	%r9
	movq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%r8, %rax
	cqto
	idivq	%r9
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %r9
	movq	%r8, %rax
	jl	.LBB3_366
# BB#363:                               # %if.then.i1967
                                        #   in Loop: Header=BB3_278 Depth=1
	subq	%rdx, %rsi
	testq	%rsi, %rsi
	jle	.LBB3_365
# BB#364:                               # %if.then10.i
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r8, %rax
	movq	-432(%rbp), %rcx        # 8-byte Reload
	subq	%rcx, %rax
	addq	%rsi, %rax
	jmp	.LBB3_366
.LBB3_354:                              # %if.then623
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-232(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-384(%rbp), %rcx        # 8-byte Reload
	cmpq	%rax, %rcx
	movq	%rcx, %r14
	movq	%rcx, %rdx
	jge	.LBB3_356
# BB#355:                               # %if.then623
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-232(%rbp), %r14        # 8-byte Reload
.LBB3_356:                              # %if.then623
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	%r14d, highest_outgoing_arg_in_use(%rip)
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %r12
	subq	%rax, %r12
	movq	%r12, %rsp
	testq	%rbx, %rdx
	movq	%r12, stack_usage_map(%rip)
	je	.LBB3_358
# BB#357:                               # %if.then633
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rdi
	movq	-400(%rbp), %rsi        # 8-byte Reload
	movq	%rdx, %r15
	movq	%rbx, %r13
	callq	memcpy
	movq	%r13, %rbx
	movq	-136(%rbp), %r13        # 8-byte Reload
	movq	%r15, %rdx
.LBB3_358:                              # %if.end635
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rdx, -384(%rbp)        # 8-byte Spill
	movq	%rdx, %rax
	andq	%rbx, %rax
	movq	%r14, %rcx
	andq	%rbx, %rcx
	movq	%rbx, %r15
	cmpq	%rax, %rcx
	je	.LBB3_360
# BB#359:                               # %if.then638
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-384(%rbp), %rax        # 8-byte Reload
	addq	%rax, %r12
	subq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r14, %rdx
	callq	memset
.LBB3_360:                              # %if.end643
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	global_rtl+72(%rip), %r14
	jmp	.LBB3_381
.LBB3_365:                              # %if.else.i1968
                                        #   in Loop: Header=BB3_278 Depth=1
	addq	%r8, %rsi
	movq	%rsi, %rax
.LBB3_366:                              # %combine_pending_stack_adjustment_and_call.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-240(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	subq	%rax, %rsi
	addq	%r8, %rsi
	movq	%rsi, -232(%rbp)        # 8-byte Spill
	movq	%rdx, %rsi
	subq	%rax, %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB3_368
# BB#367:                               # %if.end663.thread
                                        #   in Loop: Header=BB3_278 Depth=1
	negq	%rsi
	movl	%esi, (%rdi)
	callq	do_pending_stack_adjust
	jmp	.LBB3_370
.LBB3_368:                              # %if.else659
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rax, %rax
	movl	%eax, (%rdi)
.LBB3_369:                              # %if.end663
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rbx, %rsi
	jne	.LBB3_371
.LBB3_370:                              # %if.then666
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r15
	movq	global_rtl+72(%rip), %rdi
	jmp	.LBB3_372
.LBB3_371:                              # %if.else667
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r15
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	push_block
	movq	%rax, %rdi
.LBB3_372:                              # %if.end671
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	copy_to_reg
	movl	stack_arg_under_construction(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB3_376
# BB#373:                               # %if.then674
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rdi, %rdi
	movq	-232(%rbp), %rsi        # 8-byte Reload
	callq	gen_rtx_CONST_INT
	cmpq	$0, -120(%rbp)
	movq	%rax, %r12
	jne	.LBB3_375
# BB#374:                               # %if.then681
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	leaq	-120(%rbp), %rsi
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	movq	%rcx, -280(%rbp)        # 8-byte Spill
	movabsq	$0, %rbx
	movl	%ebx, (%rax)
	movl	stack_arg_under_construction(%rip), %eax
	movq	%rax, -408(%rbp)        # 8-byte Spill
	movslq	highest_outgoing_arg_in_use(%rip), %rdx
	movl	%ebx, stack_arg_under_construction(%rip)
	movq	%rsp, %rdi
	leaq	15(%rdx), %rax
	andq	$-16, %rax
	subq	%rax, %rdi
	movq	%rdi, %rsp
	movq	%rdi, stack_usage_map(%rip)
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, highest_outgoing_arg_in_use(%rip)
.LBB3_375:                              # %if.end688
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rsi, %rsi
	movq	$8, %rdx
	movq	%r12, %rdi
	callq	allocate_dynamic_stack_space
.LBB3_376:                              # %if.end690
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	movq	-464(%rbp), %rdx        # 8-byte Reload
	jle	.LBB3_381
	.align	16, 0x90
.LBB3_378:                              # %for.body694
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_379
# BB#377:                               # %for.cond691
                                        #   in Loop: Header=BB3_378 Depth=2
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	addq	$168, %rdx
	cmpq	%rdi, %rsi
	jl	.LBB3_378
	.align	16, 0x90
.LBB3_381:                              # %if.end707
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%r14, %r14
	jne	.LBB3_383
# BB#382:                               #   in Loop: Header=BB3_278 Depth=1
	movq	$1, -264(%rbp)          # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	%r15, %rbx
	movq	-224(%rbp), %r15        # 8-byte Reload
	jmp	.LBB3_403
	.align	16, 0x90
.LBB3_383:                              # %if.then.i1974
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	(%r14), %eax
	movq	$0, -264(%rbp)          # 8-byte Folded Spill
	cmpq	$75, %rax
	movabsq	$0, %rax
	movq	%r14, -160(%rbp)        # 8-byte Spill
	movq	%r15, %rbx
	jne	.LBB3_385
# BB#384:                               # %if.then1.i
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	16(%r14), %rax
	movq	8(%r14), %rcx
	movq	%rcx, -160(%rbp)        # 8-byte Spill
	movslq	8(%rax), %rax
.LBB3_385:                              # %if.end.i1977
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, -184(%rbp)        # 8-byte Spill
	movq	%r14, -176(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r13
	testq	%rax, %rax
	movq	-440(%rbp), %r15        # 8-byte Reload
	jle	.LBB3_402
	.align	16, 0x90
.LBB3_386:                              # %for.body.i1981
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-48(%r15), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_388
# BB#387:                               # %cond.true.i1984
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	-56(%r15), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB3_389
	.align	16, 0x90
.LBB3_388:                              # %cond.false28.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	-56(%r15), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB3_389:                              # %cond.end41.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	%rax, %r14
	movq	-32(%r15), %rsi
	testq	%rsi, %rsi
	jne	.LBB3_391
# BB#390:                               # %cond.true49.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	-40(%r15), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB3_392
	.align	16, 0x90
.LBB3_391:                              # %cond.false68.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	-40(%r15), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB3_392:                              # %cond.end83.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	%rax, %r12
	movl	-64(%r15), %eax
	testq	%rax, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jne	.LBB3_394
# BB#393:                               # %land.lhs.true.i1991
                                        #   in Loop: Header=BB3_386 Depth=2
	cmpq	$0, -88(%r15)
	jne	.LBB3_401
.LBB3_394:                              # %if.end93.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB3_396
# BB#395:                               # %if.then98.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	8(%r14), %rsi
	movq	-160(%rbp), %rdi        # 8-byte Reload
	callq	plus_constant_wide
	jmp	.LBB3_397
	.align	16, 0x90
.LBB3_396:                              # %if.else.i1993
                                        #   in Loop: Header=BB3_386 Depth=2
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	-160(%rbp), %rdx        # 8-byte Reload
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
.LBB3_397:                              # %if.end106.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	%rax, %rdi
	movq	-184(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movl	-112(%r15), %edi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, -8(%r15)
	movq	-120(%r15), %rcx
	movq	8(%rcx), %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	set_mem_attributes
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	jne	.LBB3_399
# BB#398:                               # %if.then123.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	8(%r12), %rsi
	movq	-160(%rbp), %rdi        # 8-byte Reload
	callq	plus_constant_wide
	jmp	.LBB3_400
	.align	16, 0x90
.LBB3_399:                              # %if.else128.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$75, %rdi
	movq	-160(%rbp), %rdx        # 8-byte Reload
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
.LBB3_400:                              # %if.end133.i
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movl	-112(%r15), %edi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, (%r15)
	movq	-120(%r15), %rcx
	movq	8(%rcx), %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	set_mem_attributes
	movq	-8(%r15), %rdi
	xorq	%rsi, %rsi
	callq	set_mem_alias_set
	movq	(%r15), %rdi
	xorq	%rsi, %rsi
	callq	set_mem_alias_set
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB3_401:                              # %for.inc.i2000
                                        #   in Loop: Header=BB3_386 Depth=2
	movq	%r13, %rax
	andq	%rbx, %rax
	movq	-136(%rbp), %rcx        # 8-byte Reload
	andq	%rbx, %rcx
	incq	%r13
	addq	$168, %r15
	cmpq	%rcx, %rax
	jne	.LBB3_386
.LBB3_402:                              #   in Loop: Header=BB3_278 Depth=1
	movq	-224(%rbp), %r15        # 8-byte Reload
	movq	-136(%rbp), %r13        # 8-byte Reload
	movq	-176(%rbp), %r12        # 8-byte Reload
.LBB3_403:                              # %compute_argument_addresses.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	target_flags(%rip), %eax
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB3_404
# BB#405:                               # %land.lhs.true710
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$4096, %rax             # imm = 0x1000
	movabsq	$0, %rax
	jne	.LBB3_407
# BB#406:                               # %land.lhs.true710
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
.LBB3_407:                              # %land.lhs.true710
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-264(%rbp), %rcx        # 8-byte Reload
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB3_408
# BB#409:                               # %land.lhs.true716
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-240(%rbp), %rdi        # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, -264(%rbp)          # 8-byte Folded Spill
	movq	-232(%rbp), %rsi        # 8-byte Reload
	subq	%rdi, %rsi
	jne	.LBB3_411
# BB#410:                               #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r14
	jmp	.LBB3_420
	.align	16, 0x90
.LBB3_404:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r14
	jmp	.LBB3_420
	.align	16, 0x90
.LBB3_408:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r14
	jmp	.LBB3_420
.LBB3_411:                              # %if.then721
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %r9
	movl	(%r9), %r8d
	movq	$32, %rcx
	movq	%r8, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	testq	%r8, %r8
	je	.LBB3_419
# BB#412:                               # %if.then721
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-152(%rbp), %rax        # 8-byte Reload
	andq	$4096, %rax             # imm = 0x1000
	testq	%rbx, %rax
	jne	.LBB3_419
# BB#413:                               # %land.lhs.true728
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	4(%r9), %eax
	testq	%rax, %rax
	jne	.LBB3_419
# BB#414:                               # %if.then732
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r14
	movl	8(%r9), %eax
	movq	-240(%rbp), %rcx        # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	movq	-432(%rbp), %r10        # 8-byte Reload
	idivq	%r10
	movq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	%r8, %rax
	cqto
	idivq	%r10
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %r10
	movq	%r11, %rax
	jl	.LBB3_418
# BB#415:                               # %if.then.i2013
                                        #   in Loop: Header=BB3_278 Depth=1
	subq	%rdx, %rsi
	testq	%rsi, %rsi
	jle	.LBB3_417
# BB#416:                               # %if.then10.i2017
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r11, %rax
	movq	-432(%rbp), %rcx        # 8-byte Reload
	subq	%rcx, %rax
	addq	%rsi, %rax
	jmp	.LBB3_418
.LBB3_419:                              # %if.then740
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r14
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
	jmp	.LBB3_420
.LBB3_379:                              # %if.then698
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r14, %rdi
	callq	copy_addr_to_reg
	jmp	.LBB3_380
.LBB3_417:                              # %if.else.i2019
                                        #   in Loop: Header=BB3_278 Depth=1
	addq	%r11, %rsi
	movq	%rsi, %rax
.LBB3_418:                              # %combine_pending_stack_adjustment_and_call.exit2026
                                        #   in Loop: Header=BB3_278 Depth=1
	addq	%rdi, %r11
	subq	%rax, %r11
	movl	%eax, (%r9)
	movq	%r11, %rbx
	callq	do_pending_stack_adjust
	movq	%rbx, -232(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB3_420:                              # %if.end747
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	testq	%r15, %r15
	movl	%ecx, 4(%rax)
	jne	.LBB3_421
# BB#426:                               # %if.else.i2042
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	push_temp_slots
	movq	-272(%rbp), %rbx        # 8-byte Reload
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, -288(%rbp)        # 8-byte Spill
	callq	pop_temp_slots
	callq	emit_queue
	movq	%rbx, %rax
	jmp	.LBB3_427
	.align	16, 0x90
.LBB3_421:                              # %if.then.i2030
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	18(%r15), %eax
	testq	$1, %rax
	jne	.LBB3_423
# BB#422:                               # %if.then2.i
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %rdi
	callq	assemble_external
	movl	16(%r15), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r15)
.LBB3_423:                              # %if.end.i2033
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	144(%r15), %rax
	testq	%rax, %rax
	jne	.LBB3_425
# BB#424:                               # %cond.false.i2034
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %rax
.LBB3_425:                              # %cond.end.i2038
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	8(%rax), %rax
	movq	%rax, -288(%rbp)        # 8-byte Spill
	movq	-272(%rbp), %rax        # 8-byte Reload
.LBB3_427:                              # %rtx_for_function_call.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r14, %r9
	movq	%r12, -176(%rbp)        # 8-byte Spill
	movq	8(%rax), %rdi
	movzbl	61(%rdi), %eax
	xorq	%rsi, %rsi
	testq	$254, %rax
	movabsq	$0, %rax
	je	.LBB3_432
# BB#428:                               # %rtx_for_function_call.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	jne	.LBB3_432
# BB#429:                               # %if.else772
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r9, %rax
	movq	%r9, %rbx
	movabsq	$0, %rdx
	jne	.LBB3_431
# BB#430:                               # %if.else772
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rdx
.LBB3_431:                              # %if.else772
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %rsi
	callq	hard_function_value
	movq	%rbx, %r9
	xorq	%rsi, %rsi
.LBB3_432:                              # %if.end779
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, -256(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r14
	testq	%rax, %rax
	movq	-144(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rbx
	movabsq	$0, %rax
	movq	%rax, -200(%rbp)        # 8-byte Spill
	jle	.LBB3_500
	.align	16, 0x90
.LBB3_433:                              # %for.body.i2046
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	$0, 32(%rbx)
	je	.LBB3_447
# BB#434:                               # %land.lhs.true.i2049
                                        #   in Loop: Header=BB3_433 Depth=2
	movl	56(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB3_447
# BB#435:                               # %if.then.i2051
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	16(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB3_436
# BB#437:                               # %if.then7.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	%rsi, %r13
	movq	%rdx, %r12
	movq	%r9, %r15
	callq	push_temp_slots
	movq	(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, 16(%rbx)
	movq	%rax, %rdi
	callq	preserve_temp_slots
	callq	pop_temp_slots
	callq	emit_queue
	movq	16(%rbx), %rax
	movq	%rbx, %rcx
	jmp	.LBB3_438
.LBB3_436:                              # %if.then.if.end_crit_edge.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	%r9, %r15
	imulq	$168, %rsi, %rcx
	movq	%rsi, %r13
	addq	%rdx, %rcx
	movq	%rdx, %r12
.LBB3_438:                              # %if.end.i2060
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	(%rcx), %rcx
	movq	8(%rcx), %rcx
	movl	60(%rcx), %esi
	movl	8(%rbx), %edi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rsi, %rdi
	je	.LBB3_440
# BB#439:                               # %if.then24.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movl	48(%rbx), %ecx
	movq	%rax, %rdx
	callq	convert_modes
	movq	%rax, 16(%rbx)
.LBB3_440:                              # %if.end47.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	%r15, %r9
	movq	%r12, %rdx
	movq	%r13, %rsi
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	$1, -200(%rbp)          # 8-byte Folded Spill
	movq	-136(%rbp), %r13        # 8-byte Reload
	je	.LBB3_447
# BB#441:                               # %if.end47.i
                                        #   in Loop: Header=BB3_433 Depth=2
	cmpq	$63, %rcx
	jne	.LBB3_443
# BB#442:                               # %land.lhs.true60.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	je	.LBB3_447
.LBB3_443:                              # %land.lhs.true68.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movl	8(%rbx), %ecx
	cmpq	$51, %rcx
	je	.LBB3_447
# BB#444:                               # %land.lhs.true73.i2066
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	%rsi, %r13
	movq	%rdx, %r12
	movq	%r9, %r15
	movq	$47, %rsi
	movq	%rax, %rdi
	callq	rtx_cost
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB3_446
# BB#445:                               # %if.then84.i
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	16(%rbx), %rsi
	movl	8(%rbx), %edi
	callq	copy_to_mode_reg
	movq	%rax, 16(%rbx)
.LBB3_446:                              # %for.inc.i2071
                                        #   in Loop: Header=BB3_433 Depth=2
	movq	%r15, %r9
	movq	%r12, %rdx
	movq	%r13, %rsi
	movq	-136(%rbp), %r13        # 8-byte Reload
	.align	16, 0x90
.LBB3_447:                              # %for.inc.i2071
                                        #   in Loop: Header=BB3_433 Depth=2
	incq	%rsi
	movq	%r14, %rax
	andq	%r9, %rax
	movq	%r13, %r15
	andq	%r9, %r15
	incq	%r14
	addq	$168, %rbx
	cmpq	%r15, %rax
	jne	.LBB3_433
# BB#448:                               # %precompute_register_parameters.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB3_471
# BB#449:                               # %for.body790.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	cmpq	$0, -328(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	movq	%rax, -208(%rbp)        # 8-byte Spill
	je	.LBB3_451
# BB#450:                               # %for.body790.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
	movq	%rax, -208(%rbp)        # 8-byte Spill
.LBB3_451:                              # %for.body790.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%r13, %r13
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB3_452:                              # %for.body790
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_460 Depth 3
                                        #       Child Loop BB3_467 Depth 3
	movq	%r9, %r12
	imulq	$168, %r13, %rcx
	cmpq	$0, 32(%rdx,%rcx)
	je	.LBB3_455
# BB#453:                               # %lor.lhs.false795
                                        #   in Loop: Header=BB3_452 Depth=2
	movl	56(%rdx,%rcx), %eax
	testq	%rax, %rax
	jne	.LBB3_455
# BB#454:                               #   in Loop: Header=BB3_452 Depth=2
	movq	%r12, %r9
	jmp	.LBB3_470
	.align	16, 0x90
.LBB3_455:                              # %if.then800
                                        #   in Loop: Header=BB3_452 Depth=2
	leaq	(%rdx,%rcx), %rbx
	movq	%rcx, -192(%rbp)        # 8-byte Spill
	movq	%rsi, %r14
	callq	get_last_insn
	movq	%rax, -160(%rbp)        # 8-byte Spill
	movq	%rbx, %rdi
	movq	-176(%rbp), %rsi        # 8-byte Reload
	movq	-152(%rbp), %rdx        # 8-byte Reload
	movq	-208(%rbp), %rcx        # 8-byte Reload
	callq	store_one_arg
	movq	%r14, %rsi
	movq	%r12, %r9
	testq	%r9, %rax
	jne	.LBB3_469
# BB#456:                               # %lor.lhs.false809
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r9, %rax
	jne	.LBB3_470
# BB#457:                               # %land.lhs.true812
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	-160(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB3_459
# BB#458:                               # %if.then.i2086
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	movq	%r9, %rbx
	callq	get_insns
	movq	%rbx, %r9
	movq	%rax, %rbx
	jmp	.LBB3_460
.LBB3_459:                              # %if.else.i2089
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	movq	24(%rax), %rbx
	jmp	.LBB3_460
	.align	16, 0x90
.LBB3_463:                              # %for.inc.i2099
                                        #   in Loop: Header=BB3_460 Depth=3
	movq	24(%rbx), %rbx
.LBB3_460:                              # %for.cond.preheader.i2090
                                        #   Parent Loop BB3_278 Depth=1
                                        #     Parent Loop BB3_452 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB3_464
# BB#461:                               # %for.body.i2094
                                        #   in Loop: Header=BB3_460 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_463
# BB#462:                               # %land.lhs.true.i2096
                                        #   in Loop: Header=BB3_460 Depth=3
	movq	32(%rbx), %rdi
	movq	%r9, %r12
	callq	check_sibcall_argument_overlap_1
	movq	%r12, %r9
	testq	%r9, %rax
	movq	$1, %r8
	je	.LBB3_463
	jmp	.LBB3_465
.LBB3_464:                              #   in Loop: Header=BB3_452 Depth=2
	xorq	%r8, %r8
.LBB3_465:                              # %for.end.i2102
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	-144(%rbp), %rax        # 8-byte Reload
	movq	-192(%rbp), %rcx        # 8-byte Reload
	movq	80(%rax,%rcx), %rbx
	movq	96(%rax,%rcx), %r10
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rbx, %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	movabsq	$4294967232, %rsi       # imm = 0xFFFFFFC0
	jge	.LBB3_468
# BB#466:                               # %for.body21.lr.ph.i
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	stored_args_map(%rip), %rdi
	.align	16, 0x90
.LBB3_467:                              # %for.body21.i
                                        #   Parent Loop BB3_278 Depth=1
                                        #     Parent Loop BB3_452 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rax
	andq	%rsi, %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%rdi,%rax)
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	movq	%r10, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_467
.LBB3_468:                              # %check_sibcall_argument_overlap.exit
                                        #   in Loop: Header=BB3_452 Depth=2
	testq	%r8, %r8
	movq	-184(%rbp), %rsi        # 8-byte Reload
	je	.LBB3_470
	.align	16, 0x90
.LBB3_469:                              # %if.then817
                                        #   in Loop: Header=BB3_452 Depth=2
	movq	$1, %rsi
.LBB3_470:                              # %for.inc820
                                        #   in Loop: Header=BB3_452 Depth=2
	incq	%r13
	movq	%r13, %rax
	andq	%r9, %rax
	cmpq	%r15, %rax
	movq	-144(%rbp), %rdx        # 8-byte Reload
	jne	.LBB3_452
.LBB3_471:                              # %for.end822
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-136(%rbp), %r13        # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jg	.LBB3_473
# BB#472:                               # %for.end822
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
.LBB3_473:                              # %for.end822
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-200(%rbp), %rcx        # 8-byte Reload
	testq	%r9, %rcx
	movq	-224(%rbp), %rcx        # 8-byte Reload
	je	.LBB3_474
# BB#475:                               # %for.end822
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rax, %rax
	jne	.LBB3_476
# BB#477:                               # %for.body828.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	cmpq	$0, -328(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	movq	%rax, -192(%rbp)        # 8-byte Spill
	je	.LBB3_479
# BB#478:                               # %for.body828.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
	movq	%rax, -192(%rbp)        # 8-byte Spill
.LBB3_479:                              # %for.body828.lr.ph
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_480:                              # %for.body828
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_488 Depth 3
                                        #       Child Loop BB3_495 Depth 3
	imulq	$168, %r13, %r14
	movl	52(%rdx,%r14), %eax
	testq	%rax, %rax
	je	.LBB3_498
# BB#481:                               # %land.lhs.true833
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	%r9, %r12
	movl	56(%rdx,%r14), %eax
	testq	%rax, %rax
	jne	.LBB3_482
# BB#483:                               # %if.then838
                                        #   in Loop: Header=BB3_480 Depth=2
	leaq	(%rdx,%r14), %rbx
	callq	get_last_insn
	movq	%rax, -160(%rbp)        # 8-byte Spill
	movq	%rbx, %rdi
	movq	-176(%rbp), %rsi        # 8-byte Reload
	movq	-152(%rbp), %rdx        # 8-byte Reload
	movq	-192(%rbp), %rcx        # 8-byte Reload
	callq	store_one_arg
	movq	%r12, %r9
	testq	%r9, %rax
	jne	.LBB3_497
# BB#484:                               # %lor.lhs.false848
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r9, %rax
	jne	.LBB3_498
# BB#485:                               # %land.lhs.true851
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	-160(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB3_487
# BB#486:                               # %if.then.i2108
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	%r9, %rbx
	callq	get_insns
	movq	%rbx, %r9
	movq	%rax, %rbx
	jmp	.LBB3_488
	.align	16, 0x90
.LBB3_482:                              #   in Loop: Header=BB3_480 Depth=2
	movq	%r12, %r9
	jmp	.LBB3_498
.LBB3_487:                              # %if.else.i2111
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	24(%rax), %rbx
	jmp	.LBB3_488
	.align	16, 0x90
.LBB3_491:                              # %for.inc.i2130
                                        #   in Loop: Header=BB3_488 Depth=3
	movq	24(%rbx), %rbx
.LBB3_488:                              # %for.cond.preheader.i2114
                                        #   Parent Loop BB3_278 Depth=1
                                        #     Parent Loop BB3_480 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB3_492
# BB#489:                               # %for.body.i2121
                                        #   in Loop: Header=BB3_488 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_491
# BB#490:                               # %land.lhs.true.i2126
                                        #   in Loop: Header=BB3_488 Depth=3
	movq	32(%rbx), %rdi
	movq	%r9, %r12
	callq	check_sibcall_argument_overlap_1
	movq	%r12, %r9
	testq	%r9, %rax
	movq	$1, %r8
	je	.LBB3_491
	jmp	.LBB3_493
.LBB3_492:                              #   in Loop: Header=BB3_480 Depth=2
	xorq	%r8, %r8
.LBB3_493:                              # %for.end.i2138
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	-144(%rbp), %rax        # 8-byte Reload
	movq	80(%rax,%r14), %rbx
	movq	96(%rax,%r14), %r10
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rbx, %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdi
	movabsq	$4294967232, %rsi       # imm = 0xFFFFFFC0
	jge	.LBB3_496
# BB#494:                               # %for.body21.lr.ph.i2139
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	stored_args_map(%rip), %rdi
	.align	16, 0x90
.LBB3_495:                              # %for.body21.i2150
                                        #   Parent Loop BB3_278 Depth=1
                                        #     Parent Loop BB3_480 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rax
	andq	%rsi, %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%rdi,%rax)
	incq	%rbx
	movq	%rbx, %rax
	andq	%r9, %rax
	movq	%r10, %rcx
	andq	%r9, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_495
.LBB3_496:                              # %check_sibcall_argument_overlap.exit2151
                                        #   in Loop: Header=BB3_480 Depth=2
	testq	%r8, %r8
	je	.LBB3_498
.LBB3_497:                              # %if.then856
                                        #   in Loop: Header=BB3_480 Depth=2
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB3_498:                              # %for.inc859
                                        #   in Loop: Header=BB3_480 Depth=2
	incq	%r13
	movq	%r13, %rax
	andq	%r9, %rax
	cmpq	%r15, %rax
	movq	-144(%rbp), %rdx        # 8-byte Reload
	jne	.LBB3_480
# BB#499:                               #   in Loop: Header=BB3_278 Depth=1
	movq	-224(%rbp), %r15        # 8-byte Reload
	movq	-136(%rbp), %r13        # 8-byte Reload
	movq	-184(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB3_500
.LBB3_474:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%rcx, %r15
	jmp	.LBB3_500
.LBB3_476:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%rcx, %r15
	.align	16, 0x90
.LBB3_500:                              # %if.end862
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	target_flags(%rip), %eax
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB3_502
# BB#501:                               # %land.lhs.true865
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	movq	$12, %rcx
	shrq	%cl, %rax
	movq	-264(%rbp), %rcx        # 8-byte Reload
	testq	%rcx, %rax
	jne	.LBB3_503
	jmp	.LBB3_504
	.align	16, 0x90
.LBB3_502:                              # %land.lhs.true868
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rsi, -184(%rbp)        # 8-byte Spill
	cmpq	$0, -264(%rbp)          # 8-byte Folded Reload
	je	.LBB3_504
.LBB3_503:                              # %if.then871
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-240(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-232(%rbp), %rsi        # 8-byte Reload
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	movq	%r9, %rbx
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
	movq	%rbx, %r9
.LBB3_504:                              # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r9, %rax
	movabsq	$0, %rax
	jne	.LBB3_506
# BB#505:                               # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
.LBB3_506:                              # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %r12
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rcx
	jne	.LBB3_508
# BB#507:                               # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rcx
.LBB3_508:                              # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r9, %r15
	cmpq	$0, -376(%rbp)          # 8-byte Folded Reload
	jne	.LBB3_512
# BB#509:                               # %if.end876
                                        #   in Loop: Header=BB3_278 Depth=1
	orq	%rcx, %rax
	jne	.LBB3_512
# BB#510:                               # %if.then883
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	target_flags(%rip), %ebx
	movq	struct_value_rtx(%rip), %r14
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rsi, %rsi
	movq	-248(%rbp), %rdi        # 8-byte Reload
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	struct_value_rtx(%rip), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB3_512
# BB#511:                               # %if.then894
                                        #   in Loop: Header=BB3_278 Depth=1
	leaq	-128(%rbp), %rdi
	callq	use_reg
.LBB3_512:                              # %if.end896
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	-288(%rbp), %rdi        # 8-byte Reload
	movq	%r12, %rsi
	leaq	-128(%rbp), %rdx
	movq	-200(%rbp), %rcx        # 8-byte Reload
	callq	prepare_call_address
	movq	%rax, -192(%rbp)        # 8-byte Spill
	testq	%rbx, %rbx
	movq	%r15, %rbx
	movq	-144(%rbp), %rdx        # 8-byte Reload
	jle	.LBB3_541
# BB#513:                               # %for.body.lr.ph.i2154
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-152(%rbp), %rsi        # 8-byte Reload
	andq	$256, %rsi              # imm = 0x100
	movq	%rsi, -176(%rbp)        # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_514:                              # %for.body.i2156
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_528 Depth 3
	imulq	$168, %r13, %r12
	leaq	(%rdx,%r12), %r8
	testq	%rbx, %rsi
	jne	.LBB3_515
# BB#516:                               # %cond.false.i2159
                                        #   in Loop: Header=BB3_514 Depth=2
	leaq	32(%rdx,%r12), %rax
	jmp	.LBB3_517
	.align	16, 0x90
.LBB3_515:                              # %cond.true.i2158
                                        #   in Loop: Header=BB3_514 Depth=2
	leaq	40(%rdx,%r12), %rax
.LBB3_517:                              # %cond.end.i2162
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB3_540
# BB#518:                               # %if.then.i2164
                                        #   in Loop: Header=BB3_514 Depth=2
	movslq	52(%rdx,%r12), %r15
	testq	%rbx, %r15
	movq	%r15, %rax
	jne	.LBB3_521
# BB#519:                               # %cond.false10.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	(%r8), %rax
	movq	8(%rax), %rdi
	movl	60(%rdi), %eax
	andq	$65024, %rax            # imm = 0xFE00
	cmpq	$26112, %rax            # imm = 0x6600
	movq	$-1, %rax
	jne	.LBB3_521
# BB#520:                               # %cond.true16.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%r8, -160(%rbp)         # 8-byte Spill
	callq	int_size_in_bytes
	movq	-160(%rbp), %r8         # 8-byte Reload
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rsi,%rax), %rax
	addq	$4, %rsi
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rsi
	cqto
	idivq	%rsi
	movq	-144(%rbp), %rdx        # 8-byte Reload
.LBB3_521:                              # %cond.end33.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movzbl	(%r14), %ecx
	cmpq	$39, %rcx
	jne	.LBB3_523
# BB#522:                               # %if.then40.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%rax, -160(%rbp)        # 8-byte Spill
	movq	(%r8), %rax
	movq	16(%rdx,%r12), %r12
	movq	8(%rax), %rdi
	callq	int_size_in_bytes
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB3_532
	.align	16, 0x90
.LBB3_523:                              # %if.else.i2178
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	%rbx, %rcx
	jne	.LBB3_525
# BB#524:                               # %if.then52.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%rax, -160(%rbp)        # 8-byte Spill
	movq	16(%rdx,%r12), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	jmp	.LBB3_532
.LBB3_525:                              # %if.else57.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%rax, -160(%rbp)        # 8-byte Spill
	movl	144(%rdx,%r12), %eax
	testq	%rax, %rax
	jne	.LBB3_526
# BB#529:                               # %if.else78.i
                                        #   in Loop: Header=BB3_514 Depth=2
	testq	%rbx, %r15
	je	.LBB3_531
# BB#530:                               # %lor.lhs.false.i2185
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	-144(%rbp), %rax        # 8-byte Reload
	movl	56(%rax,%r12), %eax
	testq	%rax, %rax
	je	.LBB3_532
.LBB3_531:                              # %if.then84.i2187
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	-144(%rbp), %rbx        # 8-byte Reload
	movq	16(%rbx,%r12), %rdi
	movl	8(%r14), %r15d
	callq	validize_mem
	movl	8(%rbx,%r12), %ecx
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	-160(%rbp), %rdx        # 8-byte Reload
	callq	move_block_to_reg
	jmp	.LBB3_532
.LBB3_526:                              # %for.cond63.preheader.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB3_532
# BB#527:                               # %for.body69.lr.ph.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	-144(%rbp), %rax        # 8-byte Reload
	leaq	144(%rax,%r12), %rcx
	leaq	136(%rax,%r12), %r15
	movq	%rcx, %r12
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB3_528:                              # %for.body69.i
                                        #   Parent Loop BB3_278 Depth=1
                                        #     Parent Loop BB3_514 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	8(%r14), %esi
	movl	word_mode(%rip), %edi
	addq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	(%r15), %rcx
	movq	(%rcx,%rbx,8), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	incq	%rbx
	movslq	(%r12), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB3_528
	.align	16, 0x90
.LBB3_532:                              # %if.end97.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movzbl	(%r14), %eax
	cmpq	$39, %rax
	jne	.LBB3_534
# BB#533:                               # %if.then102.i
                                        #   in Loop: Header=BB3_514 Depth=2
	leaq	-128(%rbp), %rdi
	movq	%r14, %rsi
	callq	use_group_regs
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB3_539
	.align	16, 0x90
.LBB3_534:                              # %if.else103.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	-160(%rbp), %rsi        # 8-byte Reload
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB3_536
# BB#535:                               # %if.then106.i
                                        #   in Loop: Header=BB3_514 Depth=2
	leaq	-128(%rbp), %rdi
	movq	%r14, %rsi
	movq	%rcx, %rbx
	callq	use_reg
	jmp	.LBB3_539
.LBB3_536:                              # %if.else107.i
                                        #   in Loop: Header=BB3_514 Depth=2
	testq	%rcx, %rsi
	movq	%rcx, %rbx
	movq	$1, %rdx
	je	.LBB3_538
# BB#537:                               # %if.else107.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	%rsi, %rdx
.LBB3_538:                              # %if.else107.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movl	8(%r14), %esi
	leaq	-128(%rbp), %rdi
	callq	use_regs
.LBB3_539:                              # %for.inc120.i
                                        #   in Loop: Header=BB3_514 Depth=2
	movq	-144(%rbp), %rdx        # 8-byte Reload
	movq	-176(%rbp), %rsi        # 8-byte Reload
.LBB3_540:                              # %for.inc120.i
                                        #   in Loop: Header=BB3_514 Depth=2
	incq	%r13
	movq	%r13, %rax
	andq	%rbx, %rax
	movq	-136(%rbp), %rcx        # 8-byte Reload
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_514
.LBB3_541:                              # %load_register_parameters.exit
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	emit_queue
	callq	get_last_insn
	movq	global_trees+216(%rip), %rdx
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	$1, %rcx
	leaq	-112(%rbp), %rdi
	callq	function_arg
	movq	%rax, %rsi
	movq	-168(%rbp), %r8         # 8-byte Reload
	testq	%rbx, %r8
	movq	%rbx, %r12
	je	.LBB3_546
# BB#542:                               # %land.lhs.true903
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movslq	8(%rax), %rax
	cqto
	idivq	-432(%rbp)              # 8-byte Folded Reload
	testq	%rdx, %rdx
	movq	-152(%rbp), %r15        # 8-byte Reload
	jne	.LBB3_543
# BB#544:                               # %land.lhs.true913
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-128(%rbp), %rax
	movq	-240(%rbp), %rbx        # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	subq	$48, %rsp
	movq	%r15, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	-360(%rbp), %rax        # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	-256(%rbp), %rax        # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	%rsi, (%rsp)
	movq	-192(%rbp), %rdi        # 8-byte Reload
	movq	-224(%rbp), %rsi        # 8-byte Reload
	movq	-344(%rbp), %rdx        # 8-byte Reload
	movq	%rbx, %rcx
	movq	-232(%rbp), %r8         # 8-byte Reload
	movq	-368(%rbp), %r9         # 8-byte Reload
	callq	emit_call_1
	addq	$48, %rsp
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	8(%rax), %ecx
	movl	(%rax), %eax
	subq	%rax, %rcx
	andq	%r12, %rcx
	movq	-336(%rbp), %rax        # 8-byte Reload
	andq	%r12, %rax
	cmpq	%rcx, %rax
	jne	.LBB3_545
# BB#547:                               # %land.lhs.true924
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$4096, %r15             # imm = 0x1000
	jne	.LBB3_548
# BB#558:                               # %land.lhs.true983
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$4, %r15
	je	.LBB3_562
# BB#559:                               # %if.then986
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-256(%rbp), %rax        # 8-byte Reload
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	-272(%rbp), %rcx        # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, %r14
	cmpq	$13, %rcx
	jne	.LBB3_561
# BB#560:                               # %if.then1001
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$128, %rsi
	movq	%r14, %rdi
	callq	mark_reg_pointer
.LBB3_561:                              # %if.end1002
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r14, %rdi
	movq	-256(%rbp), %rsi        # 8-byte Reload
	callq	emit_move_insn
	callq	get_last_insn
	movq	%rax, %rbx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$18, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insns
	movq	%r14, -256(%rbp)        # 8-byte Spill
	jmp	.LBB3_562
	.align	16, 0x90
.LBB3_546:                              # %if.end922
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	-128(%rbp), %rax
	movq	-240(%rbp), %rbx        # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	subq	$48, %rsp
	movq	-152(%rbp), %r15        # 8-byte Reload
	movq	%r15, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	-360(%rbp), %rax        # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	-256(%rbp), %rax        # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	%rsi, (%rsp)
	movq	-192(%rbp), %rdi        # 8-byte Reload
	movq	-224(%rbp), %rsi        # 8-byte Reload
	movq	-344(%rbp), %rdx        # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r8, %r14
	movq	-232(%rbp), %r8         # 8-byte Reload
	movq	-368(%rbp), %r9         # 8-byte Reload
	callq	emit_call_1
	movq	%r14, %rax
	addq	$48, %rsp
	jmp	.LBB3_563
	.align	16, 0x90
.LBB3_548:                              # %if.then927
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-256(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB3_550
# BB#549:                               # %lor.lhs.false931
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	(%rax), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jne	.LBB3_551
.LBB3_550:                              # %if.then936
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	%r14, %rdi
	callq	emit_insns
	jmp	.LBB3_562
.LBB3_551:                              # %if.else939
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	-272(%rbp), %rcx        # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %edx
	movq	%rax, %rbx
	cmpq	$13, %rdx
	jne	.LBB3_553
# BB#552:                               # %if.then953
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	8(%rcx), %rax
	movl	64(%rax), %esi
	movq	%rbx, %rdi
	callq	mark_reg_pointer
.LBB3_553:                              # %for.cond960.preheader
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, -160(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	-136(%rbp), %r15        # 8-byte Reload
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	movq	$1, %r14
	testq	%rdx, %rdx
	movq	-472(%rbp), %rbx        # 8-byte Reload
	jle	.LBB3_555
	.align	16, 0x90
.LBB3_554:                              # %for.body963
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rdx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r14, %rcx
	andq	%r12, %rcx
	movq	%r15, %rdx
	andq	%r12, %rdx
	incq	%r14
	addq	$168, %rbx
	cmpq	%rdx, %rcx
	jne	.LBB3_554
.LBB3_555:                              # %for.end969
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	-192(%rbp), %rdx        # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	testq	$512, -152(%rbp)        # 8-byte Folded Reload
                                        # imm = 0x200
	je	.LBB3_557
# BB#556:                               # %if.then974
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r14
.LBB3_557:                              # %if.end979
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-256(%rbp), %rdx        # 8-byte Reload
	movq	%rbx, %rdi
	movq	-160(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r14, %rcx
	callq	emit_libcall_block
	movq	%rbx, -256(%rbp)        # 8-byte Spill
	movq	-152(%rbp), %r15        # 8-byte Reload
	.align	16, 0x90
.LBB3_562:                              # %if.end1015
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
.LBB3_563:                              # %if.end1015
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$66, %r15
	jne	.LBB3_565
# BB#564:                               # %if.end1015
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%r12, %rax
	jne	.LBB3_570
.LBB3_565:                              # %if.then1021
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %r14
	movq	%r12, %rbx
	callq	get_last_insn
	.align	16, 0x90
.LBB3_566:                              # %while.cond
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	je	.LBB3_569
# BB#567:                               # %while.body
                                        #   in Loop: Header=BB3_566 Depth=2
	movq	16(%rax), %rax
	cmpq	%r13, %rax
	jne	.LBB3_566
	jmp	.LBB3_568
	.align	16, 0x90
.LBB3_569:                              # %while.end
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, %rdi
	callq	emit_barrier_after
	movq	%rbx, %r12
	movq	%r14, %r15
.LBB3_570:                              # %if.end1036
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$64, %r15
	je	.LBB3_572
# BB#571:                               # %if.then1039
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$32, %rcx
	movl	%ecx, 424(%rax)
.LBB3_572:                              # %if.end1043
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rbx
	movq	-216(%rbp), %r12        # 8-byte Reload
	testq	%r12, %r12
	movq	-136(%rbp), %r13        # 8-byte Reload
	je	.LBB3_580
# BB#573:                               # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	movabsq	$0, %rax
	jne	.LBB3_575
# BB#574:                               # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rax
.LBB3_575:                              # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	(%r12), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$66, %rdx
	movabsq	$0, %rdx
	je	.LBB3_577
# BB#576:                               # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rdx
.LBB3_577:                              # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	$67108864, %rcx         # imm = 0x4000000
	je	.LBB3_580
# BB#578:                               # %land.lhs.true1046
                                        #   in Loop: Header=BB3_278 Depth=1
	orq	%rax, %rdx
	jne	.LBB3_580
# BB#579:                               # %if.then1059
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
.LBB3_580:                              # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$1, %rdi
	callq	any_pending_cleanups
	testq	%r12, %r12
	movabsq	$0, %rcx
	jne	.LBB3_582
# BB#581:                               # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rcx
.LBB3_582:                              # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r8
	andq	%r8, %rax
	movq	-272(%rbp), %rdx        # 8-byte Reload
	je	.LBB3_584
# BB#583:                               # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
.LBB3_584:                              # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rax, %rax
	je	.LBB3_590
# BB#585:                               # %if.end1062
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%rcx, %rcx
	jne	.LBB3_590
# BB#586:                               # %land.lhs.true1067
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	(%r12), %eax
	cmpq	$61, %rax
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
	jne	.LBB3_590
# BB#587:                               # %land.lhs.true1072
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	8(%r12), %ecx
	xorq	%rax, %rax
	cmpq	$53, %rcx
	jb	.LBB3_589
# BB#588:                               # %select.mid
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rax
.LBB3_589:                              # %select.end
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rax, %r12
.LBB3_590:                              # %if.end1079
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	8(%rdx), %rax
	movl	60(%rax), %esi
	movq	$9, %rcx
	movq	%rsi, %rbx
	shrq	%cl, %rbx
	andq	$127, %rbx
	je	.LBB3_592
# BB#591:                               # %if.end1079
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-392(%rbp), %rcx        # 8-byte Reload
	testq	%r8, %rcx
	jne	.LBB3_592
# BB#593:                               # %if.else1092
                                        #   in Loop: Header=BB3_278 Depth=1
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	je	.LBB3_597
# BB#594:                               # %if.then1094
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%r12, %r12
	je	.LBB3_596
# BB#595:                               # %lor.lhs.false1097
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	je	.LBB3_613
.LBB3_596:                              # %if.then1102
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %rdi
	movq	-248(%rbp), %rsi        # 8-byte Reload
	movq	%r8, %r14
	callq	memory_address
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r12
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	-272(%rbp), %rsi        # 8-byte Reload
	callq	set_mem_attributes
	movq	%r14, %r8
	jmp	.LBB3_613
	.align	16, 0x90
.LBB3_592:                              # %if.then1091
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	const_int_rtx+512(%rip), %r12
	jmp	.LBB3_613
	.align	16, 0x90
.LBB3_597:                              # %if.else1132
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-256(%rbp), %rcx        # 8-byte Reload
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$39, %rcx
	jne	.LBB3_605
# BB#598:                               # %if.then1137
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%r12, %r12
	jne	.LBB3_603
# BB#599:                               # %if.then1140
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	16(%rax), %edx
	movq	$10, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rdx
	xorq	%rdi, %rdi
	cmpq	$20, %rdx
	jne	.LBB3_602
# BB#600:                               # %land.rhs1175
                                        #   in Loop: Header=BB3_278 Depth=1
	cmpq	$0, 8(%rax)
	movabsq	$0, %rdi
	je	.LBB3_602
# BB#601:                               # %land.rhs1175
                                        #   in Loop: Header=BB3_278 Depth=1
	movabsq	$1, %rdi
.LBB3_602:                              # %land.end1181
                                        #   in Loop: Header=BB3_278 Depth=1
	andq	$2, %rbx
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdi
	orq	%rbx, %rsi
	orq	%rdi, %rsi
	orq	$1, %rsi
	movq	%rax, %rdi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	preserve_temp_slots
.LBB3_603:                              # %if.end1187
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rdi
	movq	-256(%rbp), %r14        # 8-byte Reload
	movq	%r14, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rax
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
	jne	.LBB3_613
# BB#604:                               # %if.then1190
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-272(%rbp), %rax        # 8-byte Reload
	movq	8(%rax), %rdi
	movq	%r8, %rbx
	callq	int_size_in_bytes
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	emit_group_store
	jmp	.LBB3_612
.LBB3_605:                              # %if.else1196
                                        #   in Loop: Header=BB3_278 Depth=1
	testq	%r12, %r12
	je	.LBB3_609
# BB#606:                               # %land.lhs.true1198
                                        #   in Loop: Header=BB3_278 Depth=1
	movzbl	2(%r12), %ecx
	andq	%r8, %rbx
	cmpq	%rbx, %rcx
	jne	.LBB3_609
# BB#607:                               # %land.lhs.true1211
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	%rdx, %rbx
	jne	.LBB3_609
# BB#608:                               # %if.then1220
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rdi
	movq	-256(%rbp), %rsi        # 8-byte Reload
	movq	%r8, %rbx
	callq	emit_move_insn
	jmp	.LBB3_612
.LBB3_609:                              # %if.else1222
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r8, %rbx
	andq	$65024, %rsi            # imm = 0xFE00
	cmpq	$26112, %rsi            # imm = 0x6600
	jne	.LBB3_611
# BB#610:                               # %if.then1232
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, %rdi
	movq	-256(%rbp), %rsi        # 8-byte Reload
	movq	%rax, %rdx
	callq	copy_blkmode_from_reg
	movq	%rax, %r12
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_612
.LBB3_611:                              # %if.else1236
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-256(%rbp), %rdi        # 8-byte Reload
	callq	copy_to_reg
	movq	%rax, %r12
.LBB3_612:                              # %if.end1243
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%rbx, %r8
	.align	16, 0x90
.LBB3_613:                              # %if.end1243
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-120(%rbp), %rsi
	testq	%rsi, %rsi
	je	.LBB3_616
# BB#614:                               # %if.end1243
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, %rax
	andq	$1024, %rax             # imm = 0x400
	testq	%r8, %rax
	jne	.LBB3_616
# BB#615:                               # %if.then1248
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r8, %rbx
	callq	emit_stack_restore
	movq	%rbx, %r8
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movq	-280(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, (%rax)
	movq	-408(%rbp), %rax        # 8-byte Reload
	movl	%eax, stack_arg_under_construction(%rip)
	movq	-384(%rbp), %rax        # 8-byte Reload
	movl	%eax, highest_outgoing_arg_in_use(%rip)
	movq	-400(%rbp), %rax        # 8-byte Reload
	movq	%rax, stack_usage_map(%rip)
	movq	$1, -184(%rbp)          # 8-byte Folded Spill
	jmp	.LBB3_626
	.align	16, 0x90
.LBB3_616:                              # %if.else1251
                                        #   in Loop: Header=BB3_278 Depth=1
	movl	target_flags(%rip), %eax
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB3_626
# BB#617:                               # %if.else1251
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r8, %rax
	je	.LBB3_626
# BB#618:                               # %for.cond1260.preheader
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r12
	testq	%rax, %rax
	movq	-456(%rbp), %rbx        # 8-byte Reload
	jle	.LBB3_625
	.align	16, 0x90
.LBB3_619:                              # %for.body1263
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB3_624
# BB#620:                               # %if.then1268
                                        #   in Loop: Header=BB3_619 Depth=2
	movq	%r8, %r13
	movq	-8(%rbx), %rcx
	movzbl	2(%rax), %r15d
	movq	8(%rcx), %rsi
	movq	%r15, %rdi
	callq	memory_address
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	(%rbx), %rsi
	movq	%rax, %r14
	cmpq	$51, %r15
	jne	.LBB3_621
# BB#622:                               # %if.else1289
                                        #   in Loop: Header=BB3_619 Depth=2
	movq	%rsi, %rdi
	callq	validize_mem
	movq	-32(%rbx), %rsi
	movq	%rax, %r15
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	emit_block_move
	jmp	.LBB3_623
	.align	16, 0x90
.LBB3_621:                              # %if.then1284
                                        #   in Loop: Header=BB3_619 Depth=2
	movq	%r14, %rdi
	callq	emit_move_insn
.LBB3_623:                              # %for.inc1301
                                        #   in Loop: Header=BB3_619 Depth=2
	movq	%r13, %r8
	movq	-136(%rbp), %r13        # 8-byte Reload
.LBB3_624:                              # %for.inc1301
                                        #   in Loop: Header=BB3_619 Depth=2
	movq	%r12, %rax
	andq	%r8, %rax
	movq	%r13, %rcx
	andq	%r8, %rcx
	incq	%r12
	addq	$168, %rbx
	cmpq	%rcx, %rax
	jne	.LBB3_619
.LBB3_625:                              # %for.end1303
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-384(%rbp), %rax        # 8-byte Reload
	movl	%eax, highest_outgoing_arg_in_use(%rip)
	movq	-400(%rbp), %rax        # 8-byte Reload
	movq	%rax, stack_usage_map(%rip)
	movq	-152(%rbp), %r15        # 8-byte Reload
	movq	-216(%rbp), %r12        # 8-byte Reload
.LBB3_626:                              # %if.end1305
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r15, -152(%rbp)        # 8-byte Spill
	testq	$8, %r15
	je	.LBB3_629
# BB#627:                               # %land.lhs.true1308
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	cfun(%rip), %rsi
	cmpq	$0, 168(%rsi)
	je	.LBB3_629
# BB#628:                               # %if.then1311
                                        #   in Loop: Header=BB3_278 Depth=1
	addq	$184, %rsi
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%r8, %rbx
	callq	emit_stack_save
	movq	%rbx, %r8
.LBB3_629:                              # %for.cond1313.preheader
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r14
	testq	%rax, %rax
	movq	-296(%rbp), %rbx        # 8-byte Reload
	jle	.LBB3_633
	.align	16, 0x90
.LBB3_630:                              # %for.body1316
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB3_632
# BB#631:                               # %if.then1320
                                        #   in Loop: Header=BB3_630 Depth=2
	movq	%r8, %r15
	callq	free
	movq	%r15, %r8
.LBB3_632:                              # %for.inc1325
                                        #   in Loop: Header=BB3_630 Depth=2
	movq	%r14, %rax
	andq	%r8, %rax
	movq	%r13, %rcx
	andq	%r8, %rcx
	incq	%r14
	addq	$168, %rbx
	cmpq	%rcx, %rax
	jne	.LBB3_630
.LBB3_633:                              # %for.end1327
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
	testq	%r8, %rax
	movq	%r8, %rbx
	jne	.LBB3_635
# BB#634:                               # %if.then1330
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	expand_end_target_temps
.LBB3_635:                              # %if.end1331
                                        #   in Loop: Header=BB3_278 Depth=1
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	-168(%rbp), %rdx        # 8-byte Reload
	testq	%rbx, %rdx
	jne	.LBB3_636
# BB#637:                               # %if.then1335
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movq	%rdx, %r12
	movq	cfun(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	16(%rax), %rax
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	-312(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, (%rax)
	movq	-320(%rbp), %rcx        # 8-byte Reload
	movl	%ecx, 8(%rax)
	movq	$1, %rax
	movq	-296(%rbp), %rcx        # 8-byte Reload
	jle	.LBB3_639
	.align	16, 0x90
.LBB3_638:                              # %for.body1343
                                        #   Parent Loop BB3_278 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$0, -120(%rcx)
	movq	$0, (%rcx)
	movq	$0, -24(%rcx)
	movq	%rax, %rdx
	andq	%rbx, %rdx
	movq	%r13, %rsi
	andq	%rbx, %rsi
	incq	%rax
	addq	$168, %rcx
	cmpq	%rsi, %rdx
	jne	.LBB3_638
.LBB3_639:                              # %for.end1353
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	stored_args_map(%rip), %rdi
	callq	free
	movq	%r14, %r15
	movq	%r12, %rdx
	jmp	.LBB3_640
	.align	16, 0x90
.LBB3_636:                              #   in Loop: Header=BB3_278 Depth=1
	movq	%r12, -216(%rbp)        # 8-byte Spill
	movq	-304(%rbp), %r15        # 8-byte Reload
	movq	%r14, -424(%rbp)        # 8-byte Spill
.LBB3_640:                              # %for.inc1359
                                        #   in Loop: Header=BB3_278 Depth=1
	movq	-184(%rbp), %rax        # 8-byte Reload
	testq	%rbx, %rax
	movq	%r15, %r14
	je	.LBB3_642
# BB#641:                               # %for.inc1359
                                        #   in Loop: Header=BB3_278 Depth=1
	xorq	%r14, %r14
.LBB3_642:                              # %for.inc1359
                                        #   in Loop: Header=BB3_278 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB3_278
# BB#282:                               # %for.cond1362.preheader
	cmpq	$0, -424(%rbp)          # 8-byte Folded Reload
	je	.LBB3_288
# BB#283:
	movq	-424(%rbp), %rbx        # 8-byte Reload
	.align	16, 0x90
.LBB3_284:                              # %for.body1364
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB3_287
# BB#285:                               # %land.lhs.true1369
                                        #   in Loop: Header=BB3_284 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$141, %rax
	jne	.LBB3_287
# BB#286:                               # %if.then1377
                                        #   in Loop: Header=BB3_284 Depth=1
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	replace_call_placeholder
.LBB3_287:                              # %for.inc1379
                                        #   in Loop: Header=BB3_284 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_284
.LBB3_288:                              # %for.cond1384.preheader
	testq	%r14, %r14
	movq	%r14, %r12
	movq	-480(%rbp), %r14        # 8-byte Reload
	je	.LBB3_289
	.align	16, 0x90
.LBB3_643:                              # %for.body1386
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %eax
	cmpq	$34, %rax
	jne	.LBB3_646
# BB#644:                               # %land.lhs.true1391
                                        #   in Loop: Header=BB3_643 Depth=1
	movq	32(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$141, %rax
	jne	.LBB3_646
# BB#645:                               # %if.then1399
                                        #   in Loop: Header=BB3_643 Depth=1
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	replace_call_placeholder
.LBB3_646:                              # %for.inc1401
                                        #   in Loop: Header=BB3_643 Depth=1
	movq	24(%r15), %r15
	testq	%r15, %r15
	jne	.LBB3_643
.LBB3_289:                              # %for.cond1406.preheader
	testq	%r14, %r14
	je	.LBB3_295
# BB#290:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB3_291:                              # %for.body1408
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB3_294
# BB#292:                               # %land.lhs.true1413
                                        #   in Loop: Header=BB3_291 Depth=1
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$141, %rax
	jne	.LBB3_294
# BB#293:                               # %if.then1421
                                        #   in Loop: Header=BB3_291 Depth=1
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	replace_call_placeholder
.LBB3_294:                              # %for.inc1423
                                        #   in Loop: Header=BB3_291 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB3_291
.LBB3_295:                              # %for.end1427
	movq	%r14, %rax
	orq	%r12, %rax
	je	.LBB3_647
# BB#296:                               # %if.then1431
	testq	%r14, %r14
	movq	-152(%rbp), %rbx        # 8-byte Reload
	je	.LBB3_298
# BB#297:                               # %if.then1433
	movq	cfun(%rip), %rax
	movq	232(%rax), %rax
	movl	(%rax), %ecx
	orq	$268435456, %rcx        # imm = 0x10000000
	movl	%ecx, (%rax)
.LBB3_298:                              # %if.end1437
	movq	cfun(%rip), %rax
	movq	232(%rax), %r9
	movq	$141, %rdi
	xorq	%rsi, %rsi
	movq	-424(%rbp), %rdx        # 8-byte Reload
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	gen_rtx_fmt_uuuu
	movq	%rax, %rdi
	callq	emit_call_insn
	jmp	.LBB3_648
.LBB3_647:                              # %if.else1441
	movq	-424(%rbp), %rdi        # 8-byte Reload
	callq	emit_insns
	movq	-152(%rbp), %rbx        # 8-byte Reload
.LBB3_648:                              # %if.end1443
	movl	expand_call.currently_expanding_call(%rip), %eax
	decq	%rax
	movl	%eax, expand_call.currently_expanding_call(%rip)
	testq	$1024, %rbx             # imm = 0x400
	movq	-216(%rbp), %rbx        # 8-byte Reload
	je	.LBB3_650
# BB#649:                               # %if.then1446
	callq	clear_pending_stack_adjust
	movq	global_rtl+16(%rip), %rdx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	gen_rtx
	movq	%rax, %rdi
	callq	emit_insn
	movq	global_rtl+64(%rip), %rdi
	movq	global_rtl+16(%rip), %rsi
	callq	emit_move_insn
	callq	save_stack_pointer
.LBB3_650:                              # %return
	movq	%rbx, %rax
.LBB3_651:                              # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB3_568:                              # %if.then1033
	movabsq	$.L.str3, %rdi
	movabsq	$.L__FUNCTION__.expand_call, %rdx
	movq	$3148, %rsi             # imm = 0xC4C
	callq	fancy_abort
.LBB3_653:                              # %if.then9.i1932
	movabsq	$.L.str3, %rdi
	movabsq	$.L__FUNCTION__.precompute_arguments, %rdx
	movq	$1512, %rsi             # imm = 0x5E8
	callq	fancy_abort
.LBB3_73:                               # %if.then159
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.expand_call, %rcx
	movq	$2299, %rsi             # imm = 0x8FB
	jmp	.LBB3_74
.LBB3_256:                              # %sw.default
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.expand_call, %rcx
	movq	$2528, %rsi             # imm = 0x9E0
	jmp	.LBB3_74
.LBB3_337:                              # %if.then26.i
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.compute_argument_block_size, %rcx
	movq	$1425, %rsi             # imm = 0x591
	jmp	.LBB3_74
.LBB3_543:                              # %if.then908
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.expand_call, %rcx
	movq	$3049, %rsi             # imm = 0xBE9
	jmp	.LBB3_74
.LBB3_545:                              # %if.then921
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.expand_call, %rcx
	movq	$3060, %rsi             # imm = 0xBF4
.LBB3_74:                               # %if.then159
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp49:
	.size	expand_call, .Ltmp49-expand_call
	.cfi_endproc

	.globl	emit_library_call
	.align	16, 0x90
	.type	emit_library_call,@function
emit_library_call:                      # @emit_library_call
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$88, %rsp
.Ltmp51:
	.cfi_def_cfa_offset 96
	movq	%rcx, %r10
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rsi
	movq	%r9, 56(%rsp)
	movq	%r8, 48(%rsp)
	leaq	16(%rsp), %rdx
	movq	%rdx, 80(%rsp)
	leaq	96(%rsp), %rdx
	movq	%rdx, 72(%rsp)
	movq	$48, 68(%rsp)
	movq	$32, 64(%rsp)
	leaq	64(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r8, (%rsp)
	movq	%rax, %r8
	movq	%r10, %r9
	callq	emit_library_call_value_1
	addq	$88, %rsp
	retq
.Ltmp52:
	.size	emit_library_call, .Ltmp52-emit_library_call
	.cfi_endproc

	.align	16, 0x90
	.type	store_one_arg,@function
store_one_arg:                          # @store_one_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 176
.Ltmp65:
	.cfi_offset %rbx, -48
.Ltmp66:
	.cfi_offset %r12, -40
.Ltmp67:
	.cfi_offset %r13, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	(%r13), %rdi
	movzbl	16(%rdi), %ebx
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	$1, %rax
	testq	%rbx, %rbx
	je	.LBB5_114
# BB#1:                                 # %if.end
	movq	%rdi, 112(%rsp)         # 8-byte Spill
	callq	push_temp_slots
	movl	target_flags(%rip), %eax
	movq	%r15, %rdi
	movq	%r15, 120(%rsp)         # 8-byte Spill
	andq	$256, %rdi              # imm = 0x100
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB5_26
# BB#2:                                 # %if.end
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdi
	jne	.LBB5_26
# BB#3:                                 # %if.then3
	testq	%r12, %r12
	je	.LBB5_26
# BB#4:                                 # %if.then3
	testq	%r15, %r14
	jne	.LBB5_26
# BB#5:                                 # %land.lhs.true7
	cmpq	$0, 112(%r13)
	je	.LBB5_26
# BB#6:                                 # %if.then9
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	120(%r13), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	xorq	%rbx, %rbx
	cmpq	$75, %rcx
	jne	.LBB5_8
# BB#7:                                 # %if.then13
	movq	16(%rax), %rax
	movq	8(%rax), %rbx
.LBB5_8:                                # %if.end23
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	96(%r13), %rdi
	sarq	%cl, %rax
	movq	%rbx, %r14
	andq	%r15, %r14
	addq	%rdi, %r14
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movq	%rbx, %rdx
	jge	.LBB5_13
# BB#9:                                 # %for.body.lr.ph
	movq	stack_usage_map(%rip), %r8
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB5_11:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r8,%rdx), %eax
	testq	%rax, %rax
	je	.LBB5_10
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_13
.LBB5_10:                               # %for.cond
                                        #   in Loop: Header=BB5_11 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB5_11
.LBB5_13:                               # %for.end
	movq	%r14, %rax
	andq	%r15, %rax
	andq	%r15, %rdx
	cmpq	%rax, %rdx
	je	.LBB5_22
# BB#14:                                # %if.then38
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	mode_for_size
	movq	120(%r13), %rcx
	movq	8(%rcx), %rsi
	movq	%rax, %r12
	andq	%r12, %r15
	movq	%r12, %rdi
	callq	memory_address
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	cmpq	$51, %r15
	jne	.LBB5_20
# BB#15:                                # %if.then50
	movq	%rax, %r15
	movq	(%r13), %rax
	movq	8(%rax), %rdi
	movl	16(%rdi), %eax
	movq	$10, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movl	60(%rdi), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rax
	xorq	%r8, %r8
	cmpq	$20, %rax
	jne	.LBB5_19
# BB#16:                                # %land.rhs
	cmpq	$0, 8(%rdi)
	je	.LBB5_17
# BB#18:                                # %land.rhs
	movabsq	$1, %r8
	jmp	.LBB5_19
.LBB5_20:                               # %if.else88
	movq	%r12, %rdi
	movq	%rax, %r15
	callq	gen_reg_rtx
	movq	%rax, 128(%r13)
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	jmp	.LBB5_21
.LBB5_17:
	movabsq	$0, %r8
.LBB5_19:                               # %land.end
	andq	$2, %rdx
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %r8
	orq	%rdx, %rsi
	orq	%r8, %rsi
	orq	$1, %rsi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, 128(%r13)
	movq	%rax, %rdi
	callq	preserve_temp_slots
	movq	128(%r13), %rdi
	callq	validize_mem
	movq	(%r13), %rdi
	movq	%rax, %r12
	callq	expr_size
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	emit_block_move
.LBB5_21:                               # %land.lhs.true99
	movq	96(%rsp), %r12          # 8-byte Reload
.LBB5_22:                               # %land.lhs.true99
	cmpq	$0, 112(%r13)
	movq	72(%rsp), %rdi          # 8-byte Reload
	je	.LBB5_26
# BB#23:                                # %land.lhs.true99
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB5_26
# BB#24:                                # %for.body106.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rbx
	movq	$1, %rax
	.align	16, 0x90
.LBB5_25:                               # %for.body106
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	stack_usage_map(%rip), %rcx
	movb	%al, -1(%rcx,%rbx)
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	cmpq	%rdx, %rsi
	jl	.LBB5_25
.LBB5_26:                               # %if.end113
	movl	56(%r13), %eax
	testq	%rax, %rax
	je	.LBB5_36
# BB#27:
	movq	$1, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	jmp	.LBB5_28
.LBB5_36:                               # %if.end123
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdi
	je	.LBB5_37
# BB#38:                                # %if.end123
	leaq	40(%r13), %rcx
	jmp	.LBB5_39
.LBB5_37:
	leaq	32(%r13), %rcx
.LBB5_39:                               # %if.end123
	movl	52(%r13), %edx
	movq	(%rcx), %rsi
	testq	%rdx, %rdx
	jne	.LBB5_40
# BB#41:                                # %if.end123
	movabsq	$1, %r14
	jmp	.LBB5_42
.LBB5_40:
	movabsq	$0, %r14
.LBB5_42:                               # %if.end123
	testq	%rsi, %rsi
	je	.LBB5_28
# BB#43:                                # %if.end123
	testq	%rdx, %rdx
	movq	120(%rsp), %rbx         # 8-byte Reload
	jne	.LBB5_29
# BB#44:                                # %if.then129
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.store_one_arg, %rcx
	movq	$4370, %rsi             # imm = 0x1112
	jmp	.LBB5_45
.LBB5_28:                               # %if.end130
	movq	120(%rsp), %rbx         # 8-byte Reload
.LBB5_29:                               # %if.end130
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movl	144(%r13), %ecx
	testq	%rcx, %rcx
	movq	112(%rsp), %r15         # 8-byte Reload
	je	.LBB5_31
# BB#30:                                # %select.mid
	xorq	%rsi, %rsi
.LBB5_31:                               # %select.end
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	16(%r13), %r12
	testq	%r12, %r12
	jne	.LBB5_52
# BB#32:                                # %if.then137
	testq	%rax, %rax
	movq	%r15, %rdi
	je	.LBB5_34
# BB#33:                                # %if.then140
	movl	stack_arg_under_construction(%rip), %eax
	incq	%rax
	movl	%eax, stack_arg_under_construction(%rip)
.LBB5_34:                               # %if.end142
	testq	%r14, %r14
	movq	%r14, 104(%rsp)         # 8-byte Spill
	je	.LBB5_35
# BB#46:                                # %lor.lhs.false
	movq	8(%rdi), %rax
	leaq	8(%rdi), %r14
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	movl	8(%r13), %ecx
	andq	$127, %rax
	leaq	8(%r13), %rbx
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	jne	.LBB5_48
# BB#47:                                # %cond.false
	movq	112(%r13), %rsi
	jmp	.LBB5_48
.LBB5_35:                               # %if.end142.cond.end_crit_edge
	leaq	8(%r13), %rbx
	leaq	8(%rdi), %r14
	xorq	%rsi, %rsi
.LBB5_48:                               # %cond.end
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rdi, %r15
	callq	expand_expr
	movq	%rax, %r12
	movq	%r12, 16(%r13)
	movq	(%r14), %rax
	movl	60(%rax), %esi
	movl	(%rbx), %edi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	cmpq	%rsi, %rdi
	je	.LBB5_50
# BB#49:                                # %if.then166
	movl	48(%r13), %ecx
	movq	%r12, %rdx
	callq	convert_modes
	movq	%rax, %r12
	movq	%r12, 16(%r13)
.LBB5_50:                               # %if.end178
	movq	104(%rsp), %r14         # 8-byte Reload
	movl	56(%r13), %eax
	testq	%rax, %rax
	movq	120(%rsp), %rbx         # 8-byte Reload
	je	.LBB5_52
# BB#51:                                # %if.then181
	movl	stack_arg_under_construction(%rip), %eax
	decq	%rax
	movl	%eax, stack_arg_under_construction(%rip)
.LBB5_52:                               # %if.end183
	testq	$8, %rbx
	je	.LBB5_54
# BB#53:                                # %if.then186
	callq	do_pending_stack_adjust
	movq	16(%r13), %r12
.LBB5_54:                               # %if.end187
	movq	112(%r13), %rax
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	cmpq	%rax, %r12
	je	.LBB5_113
# BB#55:                                # %if.else193
	movq	%r14, 104(%rsp)         # 8-byte Spill
	movl	8(%r13), %edx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$51, %rdx
	jne	.LBB5_56
# BB#67:                                # %if.else295
	movq	104(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB5_69
# BB#68:                                # %cond.false318
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	96(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%rax, %rdi
	callq	expand_expr
	movq	%r15, %r14
	jmp	.LBB5_70
.LBB5_56:                               # %if.then197
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %edi
	testq	$33554432, %rsi         # imm = 0x2000000
	je	.LBB5_58
# BB#57:                                # %cond.true205
	addq	$7, %rdi
	andq	$504, %rdi              # imm = 0x1F8
	jmp	.LBB5_59
.LBB5_69:                               # %if.else331
	movq	96(%r13), %rbx
	movq	%r15, %r14
	movq	8(%r14), %rdi
	callq	int_size_in_bytes
	movl	target_flags(%rip), %edx
	subq	%rax, %rbx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	imulq	%rax, %rdx
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rdx, %rax
	addq	%rbx, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	expr_size
.LBB5_70:                               # %if.end346
	xorq	%rdi, %rdi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rdx, %rcx
	je	.LBB5_88
# BB#71:                                # %land.lhs.true349
	movq	16(%r13), %rsi
	movzbl	(%rsi), %ecx
	cmpq	$66, %rcx
	jne	.LBB5_88
# BB#72:                                # %if.then355
	movq	cfun(%rip), %rdx
	movq	8(%rsi), %rcx
	movq	128(%rdx), %rdx
	xorq	%rsi, %rsi
	cmpq	%rdx, %rcx
	je	.LBB5_78
# BB#73:                                # %lor.lhs.false363
	movzbl	(%rcx), %esi
	xorq	%rdi, %rdi
	cmpq	$75, %rsi
	jne	.LBB5_88
# BB#74:                                # %land.lhs.true371
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB5_88
# BB#75:                                # %land.lhs.true381
	movq	16(%rcx), %rsi
	movzbl	(%rsi), %esi
	cmpq	$54, %rsi
	jne	.LBB5_88
# BB#76:                                # %if.then392
	xorq	%rsi, %rsi
	cmpq	%rdx, %rcx
	je	.LBB5_78
# BB#77:                                # %if.then399
	movq	16(%rcx), %rcx
	movslq	8(%rcx), %rsi
.LBB5_78:                               # %if.end410
	cmpq	$0, 72(%r13)
	jne	.LBB5_80
# BB#79:                                # %lor.lhs.false414
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB5_80
# BB#81:                                # %if.end420
	movq	64(%r13), %rcx
	cmpq	%rsi, %rcx
	jle	.LBB5_84
# BB#82:                                # %if.then426
	movq	8(%rax), %rdx
	addq	%rsi, %rdx
	cmpq	%rdx, %rcx
	jl	.LBB5_87
# BB#83:
	movabsq	$0, %rdi
	jmp	.LBB5_88
.LBB5_58:                               # %cond.false208
	incq	%rdi
	andq	$510, %rdi              # imm = 0x1FE
.LBB5_59:                               # %cond.end211
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movq	$3, %rcx
	sarq	%cl, %rsi
	leaq	-1(%rdi,%rsi), %r14
	movq	%r14, %rax
	cqto
	idivq	%rsi
	movq	72(%r13), %rsi
	movq	8(%r15), %rax
	addq	%rdi, %rdx
	subq	%rdx, %r14
	testq	%rsi, %rsi
	je	.LBB5_60
# BB#61:                                # %cond.false247
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	64(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_62
.LBB5_60:                               # %cond.true234
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	64(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_62:                               # %cond.end258
	movq	%rax, %r15
	movq	160(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB5_63
# BB#64:                                # %cond.false276
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	152(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_65
.LBB5_63:                               # %cond.true263
	movq	152(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_65:                               # %cond.end287
	movl	8(%r13), %esi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%rax, 40(%rsp)
	movq	%r15, 24(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	%r14, 8(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$0, 32(%rsp)
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdi
	movq	88(%rsp), %r9           # 8-byte Reload
	callq	emit_push_insn
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	je	.LBB5_113
# BB#66:                                # %if.then291
	movq	112(%r13), %rax
	jmp	.LBB5_112
.LBB5_84:                               # %if.else438
	xorq	%rdi, %rdi
	cmpq	%rsi, %rcx
	jge	.LBB5_88
# BB#85:                                # %if.then444
	movq	8(%rax), %rdx
	addq	%rcx, %rdx
	cmpq	%rdx, %rsi
	jge	.LBB5_86
.LBB5_87:                               # %if.then444
	movabsq	$1, %rdi
	jmp	.LBB5_88
.LBB5_86:
	movabsq	$0, %rdi
.LBB5_88:                               # %if.end459
	movq	%rdi, 120(%rsp)         # 8-byte Spill
	movq	%rax, 72(%rsp)          # 8-byte Spill
	cmpq	$0, 32(%r13)
	je	.LBB5_104
# BB#89:                                # %land.lhs.true462
	movl	56(%r13), %eax
	testq	%rax, %rax
	je	.LBB5_104
# BB#90:                                # %if.then465
	movq	64(%r13), %rsi
	testq	%rsi, %rsi
	jns	.LBB5_104
# BB#91:                                # %land.lhs.true471
	cmpq	$0, 72(%r13)
	je	.LBB5_94
# BB#92:                                # %if.then475
	movabsq	$.L.str5, %rdi
	jmp	.LBB5_93
.LBB5_94:                               # %land.lhs.true482
	cmpq	$0, 104(%r13)
	je	.LBB5_96
# BB#95:                                # %if.then486
	movabsq	$.L.str6, %rdi
.LBB5_93:                               # %if.end578
	xorq	%rax, %rax
	callq	error
.LBB5_104:                              # %if.end578
	movq	8(%r14), %r14
	movq	16(%r13), %rax
	movq	72(%r13), %rsi
	movl	64(%r14), %r15d
	testq	%rsi, %rsi
	je	.LBB5_105
# BB#106:                               # %cond.false604
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	64(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_107
.LBB5_105:                              # %cond.true591
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	64(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_107:                              # %cond.end615
	movq	%rax, %r12
	movq	160(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB5_108
# BB#109:                               # %cond.false634
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	152(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_110
.LBB5_108:                              # %cond.true621
	movq	152(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_110:                              # %cond.end645
	movq	%r15, %r8
	movq	112(%rsp), %rdi         # 8-byte Reload
	movl	8(%r13), %esi
	movq	%rax, 40(%rsp)
	movq	%r12, 24(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$0, 32(%rsp)
	movq	%r14, %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	88(%rsp), %r9           # 8-byte Reload
	callq	emit_push_insn
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	je	.LBB5_113
# BB#111:                               # %if.then649
	movq	120(%r13), %rax
.LBB5_112:                              # %if.end654
	movq	%rax, 16(%r13)
.LBB5_113:                              # %if.end654
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	callq	emit_queue
	xorq	%rdi, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	callq	pop_temp_slots
	movq	120(%rsp), %rax         # 8-byte Reload
.LBB5_114:                              # %return
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_96:                               # %land.lhs.true493
	movq	96(%r13), %rax
	addq	%rsi, %rax
	testq	%rax, %rax
	jle	.LBB5_104
# BB#97:                                # %if.then502
	negq	%rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	8(%r14), %r12
	movq	16(%r13), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	72(%r13), %rsi
	movl	64(%r12), %ecx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	testq	%rsi, %rsi
	je	.LBB5_98
# BB#99:                                # %cond.false532
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	64(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_100
.LBB5_98:                               # %cond.true519
	movq	64(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_100:                              # %cond.end543
	movq	%rax, %r14
	movq	160(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB5_101
# BB#102:                               # %cond.false562
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	152(%r13), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
	jmp	.LBB5_103
.LBB5_101:                              # %cond.true549
	movq	152(%r13), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
.LBB5_103:                              # %cond.end573
	movq	%rax, 40(%rsp)
	movq	%r14, 24(%rsp)
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$0, 32(%rsp)
	movq	48(%rsp), %rdi          # 8-byte Reload
	movl	8(%r13), %esi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	88(%rsp), %r9           # 8-byte Reload
	callq	emit_push_insn
	movq	112(%rsp), %r14         # 8-byte Reload
	jmp	.LBB5_104
.LBB5_80:                               # %if.then419
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.store_one_arg, %rcx
	movq	$4515, %rsi             # imm = 0x11A3
.LBB5_45:                               # %if.then129
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp70:
	.size	store_one_arg, .Ltmp70-store_one_arg
	.cfi_endproc

	.align	16, 0x90
	.type	emit_call_1,@function
emit_call_1:                            # @emit_call_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp82:
	.cfi_def_cfa_offset 80
.Ltmp83:
	.cfi_offset %rbx, -48
.Ltmp84:
	.cfi_offset %r12, -40
.Ltmp85:
	.cfi_offset %r13, -32
.Ltmp86:
	.cfi_offset %r14, -24
.Ltmp87:
	.cfi_offset %r15, -16
	movq	%r9, %r14
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %r12
	movq	%rdx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	movq	%r8, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	ix86_return_pops_args
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	movzbl	(%r15), %eax
	cmpq	$68, %rax
	jne	.LBB6_2
# BB#1:
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	jmp	.LBB6_3
.LBB6_2:                                # %if.then
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	memory_address
	movq	%rax, %r15
.LBB6_3:                                # %if.end
	movq	104(%rsp), %r12
	movq	88(%rsp), %r14
	movzbl	target_flags+3(%rip), %eax
	movq	80(%rsp), %rbx
	testq	$2, %rax
	jne	.LBB6_10
# BB#4:                                 # %land.lhs.true
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_10
# BB#5:                                 # %land.lhs.true
	movq	112(%rsp), %rax
	andq	$1024, %rax             # imm = 0x400
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_10
# BB#6:                                 # %if.then14
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	testq	%r14, %r14
	je	.LBB6_8
# BB#7:                                 # %if.then17
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	(%rsp), %r8             # 8-byte Reload
	callq	gen_call_value_pop
	jmp	.LBB6_9
.LBB6_10:                               # %if.else24
	movq	$2, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_MEM
	testq	%r14, %r14
	je	.LBB6_12
# BB#11:                                # %if.then26
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	gen_call_value
	jmp	.LBB6_13
.LBB6_12:                               # %if.else30
	movq	%rax, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	gen_call
.LBB6_13:                               # %if.end35
	movq	%rax, %rdi
	callq	emit_call_insn
	xorq	%r14, %r14
	jmp	.LBB6_14
.LBB6_8:                                # %if.else
	movq	%rax, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	gen_call_pop
.LBB6_9:                                # %if.end22
	movq	%rax, %rdi
	callq	emit_call_insn
	movq	$1, %r14
.LBB6_14:                               # %if.end35
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB6_15
	.align	16, 0x90
.LBB6_17:                               # %for.inc
                                        #   in Loop: Header=BB6_15 Depth=1
	movq	16(%rbx), %rbx
.LBB6_15:                               # %if.end35
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB6_18
# BB#16:                                # %land.rhs
                                        #   in Loop: Header=BB6_15 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	jne	.LBB6_17
# BB#19:                                # %if.end44
	movq	112(%rsp), %rax
	testq	$512, %rax              # imm = 0x200
	je	.LBB6_21
# BB#20:                                # %if.then47
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
.LBB6_21:                               # %if.end52
	movq	64(%rbx), %rax
	testq	%rax, %rax
	je	.LBB6_24
	.align	16, 0x90
.LBB6_22:                               # %for.cond61
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	16(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB6_22
# BB#23:                                # %for.end72
	movq	%r12, 16(%rcx)
	jmp	.LBB6_25
.LBB6_24:                               # %if.else76
	movq	%r12, 64(%rbx)
.LBB6_25:                               # %if.end80
	movq	112(%rsp), %r15
	andq	$513, %r15              # imm = 0x201
	je	.LBB6_27
# BB#26:                                # %if.then83
	movl	(%rbx), %eax
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, (%rbx)
.LBB6_27:                               # %if.end86
	movq	112(%rsp), %rax
	testq	$16, %rax
	je	.LBB6_29
# BB#28:                                # %if.then89
	movq	const_int_rtx+512(%rip), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$23, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB6_29:                               # %if.end97
	movq	112(%rsp), %rax
	testq	$2, %rax
	je	.LBB6_31
# BB#30:                                # %if.then100
	movq	const_int_rtx+512(%rip), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$26, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB6_31:                               # %if.end108
	movq	112(%rsp), %rax
	testq	$2048, %rax             # imm = 0x800
	movq	16(%rsp), %r12          # 8-byte Reload
	je	.LBB6_33
# BB#32:                                # %if.then111
	movq	const_int_rtx+512(%rip), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$29, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
.LBB6_33:                               # %if.end119
	movq	112(%rsp), %rax
	testq	$32, %rax
	je	.LBB6_35
# BB#34:                                # %if.then122
	movq	const_int_rtx+512(%rip), %rdx
	movq	56(%rbx), %rcx
	movq	$3, %rdi
	movq	$28, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, 56(%rbx)
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$16, %rcx
	movl	%ecx, 424(%rax)
.LBB6_35:                               # %if.end133
	movl	(%rbx), %eax
	movq	$16, %rcx
	movq	112(%rsp), %rdx
	shlq	%cl, %rdx
	andq	$16777216, %rdx         # imm = 0x1000000
	movabsq	$4278190079, %rcx       # imm = 0xFEFFFFFF
	andq	%rax, %rcx
	orq	%rdx, %rcx
	movl	%ecx, (%rbx)
	movq	cfun(%rip), %rax
	movq	16(%rax), %rdx
	movq	96(%rsp), %rax
	movl	%eax, 4(%rdx)
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_36
# BB#37:                                # %if.then142
	testq	%r14, %r14
	jne	.LBB6_39
# BB#38:                                # %if.then144
	movq	global_rtl+16(%rip), %rdx
	movq	$49, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	64(%rbx), %rcx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, 64(%rbx)
.LBB6_39:                               # %if.end153
	movq	8(%rsp), %rbx           # 8-byte Reload
	subq	%r13, %rbx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	gen_rtx_CONST_INT
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rdx
	movl	8(%rdx), %ecx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	subq	%r12, %rcx
	movl	%ecx, 8(%rdx)
	jmp	.LBB6_40
.LBB6_36:
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB6_40:                               # %if.end159
	movzbl	target_flags+1(%rip), %eax
	testq	$16, %rax
	je	.LBB6_41
# BB#48:                                # %if.else192
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB6_49
# BB#50:                                # %if.then194
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	anti_adjust_stack  # TAILCALL
.LBB6_41:                               # %if.then162
	testq	%rbx, %rbx
	je	.LBB6_49
# BB#42:                                # %if.then165
	movq	112(%rsp), %rax
	testq	$1024, %rax             # imm = 0x400
	je	.LBB6_44
# BB#43:                                # %if.then168
	movl	8(%rdx), %eax
	subq	%rbx, %rax
	movl	%eax, 8(%rdx)
.LBB6_49:                               # %if.end197
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_44:                               # %if.else174
	movl	flag_defer_pop(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_47
# BB#45:                                # %land.lhs.true176
	movl	4(%rdx), %eax
	orq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_46
.LBB6_47:                               # %if.else188
	movq	24(%rsp), %rdi          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	adjust_stack  # TAILCALL
.LBB6_46:                               # %if.then184
	movl	(%rdx), %eax
	addq	%rbx, %rax
	movl	%eax, (%rdx)
	jmp	.LBB6_49
.LBB6_18:                               # %if.then43
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.emit_call_1, %rcx
	movq	$580, %rsi              # imm = 0x244
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp88:
	.size	emit_call_1, .Ltmp88-emit_call_1
	.cfi_endproc

	.align	16, 0x90
	.type	emit_library_call_value_1,@function
emit_library_call_value_1:              # @emit_library_call_value_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp92:
	.cfi_def_cfa_offset 16
.Ltmp93:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp94:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$296, %rsp              # imm = 0x128
.Ltmp95:
	.cfi_offset %rbx, -56
.Ltmp96:
	.cfi_offset %r12, -48
.Ltmp97:
	.cfi_offset %r13, -40
.Ltmp98:
	.cfi_offset %r14, -32
.Ltmp99:
	.cfi_offset %r15, -24
	movq	cfun(%rip), %rax
	movq	%r9, -128(%rbp)         # 8-byte Spill
	movq	%r8, %r12
	movq	%rdx, -248(%rbp)        # 8-byte Spill
	movq	%rsi, -208(%rbp)        # 8-byte Spill
	movq	%rdi, -328(%rbp)        # 8-byte Spill
	movq	16(%rbp), %r13
	movq	16(%rax), %rdx
	movl	4(%rdx), %edx
	movq	%rdx, -296(%rbp)        # 8-byte Spill
	movq	$0, -112(%rbp)
	movslq	highest_outgoing_arg_in_use(%rip), %r15
	movq	stack_usage_map(%rip), %rdx
	movq	%rdx, -280(%rbp)        # 8-byte Spill
	decq	%rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB7_2
# BB#1:
	movq	$16, %r14
.LBB7_11:                               # %sw.epilog
	movslq	404(%rax), %rdx
	movslq	ix86_preferred_stack_boundary(%rip), %rcx
	cmpq	%rcx, %rdx
	jge	.LBB7_13
# BB#12:                                # %if.then
	movl	%ecx, 404(%rax)
.LBB7_13:                               # %if.end
	movq	%rsi, %rbx
	testq	%rbx, %r12
	je	.LBB7_14
# BB#15:                                # %land.lhs.true
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	type_for_mode
	movq	%rax, %rdi
	callq	aggregate_value_p
	testq	%rbx, %rax
	je	.LBB7_14
# BB#16:                                # %if.then16
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ebx
	cmpq	$0, -248(%rbp)          # 8-byte Folded Reload
	je	.LBB7_18
# BB#17:                                # %land.lhs.true19
	movq	-248(%rbp), %rcx        # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$66, %rax
	movq	%rcx, %rax
	je	.LBB7_19
.LBB7_18:                               # %if.else
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	type_for_mode
	xorq	%rsi, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
.LBB7_19:                               # %if.end25
	movq	%rbx, -304(%rbp)        # 8-byte Spill
	movq	%rax, -224(%rbp)        # 8-byte Spill
	andq	$2098, %r14             # imm = 0x832
	jmp	.LBB7_20
.LBB7_14:
	movq	$0, -304(%rbp)          # 8-byte Folded Spill
	movq	$0, -224(%rbp)          # 8-byte Folded Spill
.LBB7_20:                               # %if.end26
	movq	%r14, %rbx
	movq	%rbx, -288(%rbp)        # 8-byte Spill
	movq	%r15, -256(%rbp)        # 8-byte Spill
	movq	-128(%rbp), %rax        # 8-byte Reload
	leaq	1(%rax), %rax
	movq	%rax, -120(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,8), %rdx
	leaq	15(%rax,%rax,8), %rax
	andq	$-16, %rax
	movq	%rsp, %rdi
	subq	%rax, %rdi
	movq	%rdi, -176(%rbp)        # 8-byte Spill
	movq	%rdi, %rsp
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	memset
	leaq	-104(%rbp), %rdi
	xorq	%rsi, %rsi
	movq	-208(%rbp), %rdx        # 8-byte Reload
	callq	init_cumulative_args
	andq	$4096, %rbx             # imm = 0x1000
	movq	%rbx, -312(%rbp)        # 8-byte Spill
	movq	$0, -48(%rbp)
	movq	$0, -56(%rbp)
	je	.LBB7_22
# BB#21:                                # %if.then33
	callq	start_sequence
.LBB7_22:                               # %if.end34
	movq	%r12, -232(%rbp)        # 8-byte Spill
	callq	push_temp_slots
	cmpq	$0, -224(%rbp)          # 8-byte Folded Reload
	je	.LBB7_23
# BB#24:                                # %if.end34
	movq	struct_value_rtx(%rip), %rax
	testq	%rax, %rax
	movq	-128(%rbp), %r14        # 8-byte Reload
	jne	.LBB7_38
# BB#25:                                # %if.then41
	movq	-224(%rbp), %rax        # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB7_28
# BB#26:                                # %if.then41
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_30
# BB#27:                                # %if.then41
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$28823, %rdx            # imm = 0x7097
	jne	.LBB7_31
	jmp	.LBB7_30
.LBB7_2:                                # %entry
	jmpq	*.LJTI7_0(,%rcx,8)
.LBB7_3:                                # %sw.bb
	movq	$17, %r14
	jmp	.LBB7_11
.LBB7_23:
	movq	-128(%rbp), %r14        # 8-byte Reload
	jmp	.LBB7_38
.LBB7_28:                               # %if.then41
	cmpq	$134, %rcx
	je	.LBB7_31
# BB#29:                                # %if.then41
	cmpq	$140, %rcx
	je	.LBB7_31
.LBB7_30:                               # %if.then92
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
.LBB7_31:                               # %if.end94
	movq	-176(%rbp), %rcx        # 8-byte Reload
	movq	%rax, (%rcx)
	movq	%rcx, %rbx
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movl	%esi, 8(%rbx)
	xorq	%rax, %rax
	movl	%eax, 24(%rbx)
	leaq	-104(%rbp), %rdi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	callq	function_arg
	movq	%rbx, %rsi
	movl	target_flags(%rip), %edi
	movq	%rax, 16(%rsi)
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	testq	%rax, %rax
	je	.LBB7_32
# BB#33:                                # %if.end94
	movabsq	$1, %rdx
	jmp	.LBB7_34
.LBB7_32:
	movabsq	$0, %rdx
.LBB7_34:                               # %if.end94
	leaq	32(%rsi), %r9
	leaq	48(%rsi), %r14
	subq	$16, %rsp
	leaq	-72(%rbp), %rax
	movq	%rax, 8(%rsp)
	leaq	-56(%rbp), %r8
	movq	%rsi, %rbx
	movabsq	$0, %rsi
	xorq	%rcx, %rcx
	movq	%r14, (%rsp)
	callq	locate_and_pad_parm
	movq	%rbx, %rcx
	addq	$16, %rsp
	cmpq	$0, 16(%rcx)
	je	.LBB7_36
# BB#35:                                # %lor.lhs.false127
	movl	24(%rcx), %eax
	testq	%rax, %rax
	je	.LBB7_37
.LBB7_36:                               # %if.then136
	movq	(%r14), %rax
	addq	%rax, -56(%rbp)
.LBB7_37:                               # %if.end143
	movq	%rcx, -176(%rbp)        # 8-byte Spill
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	leaq	-104(%rbp), %rdi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	$1, %r15
	callq	function_arg_advance
	movq	-120(%rbp), %r14        # 8-byte Reload
.LBB7_38:                               # %for.cond.preheader
	movq	%r14, -128(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r15
	jge	.LBB7_69
# BB#39:                                # %for.body.lr.ph
	leaq	(%r15,%r15,8), %rax
	leaq	1(%r15), %r14
	movq	-176(%rbp), %rcx        # 8-byte Reload
	leaq	32(%rcx,%rax,8), %rbx
	movabsq	$0, %r15
	.align	16, 0x90
.LBB7_40:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %edx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$40, %rdx
	ja	.LBB7_42
# BB#41:                                # %vaarg.in_reg
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	16(%r13), %rax
	addq	%rsi, %rax
	addq	$8, %rdx
	movl	%edx, (%r13)
	jmp	.LBB7_43
	.align	16, 0x90
.LBB7_42:                               # %vaarg.in_mem
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	8(%r13), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 8(%r13)
.LBB7_43:                               # %vaarg.end
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	(%rax), %rax
	movq	%rdx, %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$40, %rcx
	ja	.LBB7_45
# BB#44:                                # %vaarg.in_reg155
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	16(%r13), %rdi
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdi, %rsi
	addq	$8, %rdx
	movl	%edx, (%r13)
	jmp	.LBB7_46
	.align	16, 0x90
.LBB7_45:                               # %vaarg.in_mem157
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	8(%r13), %rsi
	leaq	8(%rsi), %rcx
	movq	%rcx, 8(%r13)
.LBB7_46:                               # %vaarg.end161
                                        #   in Loop: Header=BB7_40 Depth=1
	movl	(%rsi), %r12d
	cmpq	$51, %r12
	je	.LBB7_49
# BB#47:                                # %lor.lhs.false165
                                        #   in Loop: Header=BB7_40 Depth=1
	movl	(%rax), %edx
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%r12, %rsi
	je	.LBB7_51
# BB#48:                                # %lor.lhs.false165
                                        #   in Loop: Header=BB7_40 Depth=1
	testq	%rsi, %rsi
	jne	.LBB7_49
.LBB7_51:                               # %if.end177
                                        #   in Loop: Header=BB7_40 Depth=1
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$133, %rdx
	jg	.LBB7_54
# BB#52:                                # %if.end177
                                        #   in Loop: Header=BB7_40 Depth=1
	addq	$-54, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$14, %rdx
	ja	.LBB7_56
# BB#53:                                # %if.end177
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	testq	$28823, %rsi            # imm = 0x7097
	jne	.LBB7_57
	jmp	.LBB7_56
	.align	16, 0x90
.LBB7_54:                               # %if.end177
                                        #   in Loop: Header=BB7_40 Depth=1
	cmpq	$134, %rdx
	je	.LBB7_57
# BB#55:                                # %if.end177
                                        #   in Loop: Header=BB7_40 Depth=1
	cmpq	$140, %rdx
	je	.LBB7_57
.LBB7_56:                               # %if.then227
                                        #   in Loop: Header=BB7_40 Depth=1
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_operand
.LBB7_57:                               # %if.end229
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	%rax, -32(%rbx)
	movl	%r12d, -24(%rbx)
	xorq	%rdx, %rdx
	movq	$1, %rcx
	leaq	-104(%rbp), %rdi
	movq	%r12, %rsi
	callq	function_arg
	testq	%rax, %rax
	movq	%rax, -16(%rbx)
	movabsq	$0, %rax
	movl	%eax, -8(%rbx)
	movq	%r15, %rdx
	je	.LBB7_59
# BB#58:                                # %if.end229
                                        #   in Loop: Header=BB7_40 Depth=1
	movabsq	$1, %rdx
.LBB7_59:                               # %if.end229
                                        #   in Loop: Header=BB7_40 Depth=1
	leaq	16(%rbx), %rax
	subq	$16, %rsp
	leaq	-72(%rbp), %rcx
	movq	%rcx, 8(%rsp)
	movabsq	$0, %rsi
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	%r12, %rdi
	leaq	-56(%rbp), %r8
	movq	%rbx, %r9
	callq	locate_and_pad_parm
	addq	$16, %rsp
	cmpq	$0, 24(%rbx)
	jne	.LBB7_60
# BB#61:                                # %land.lhs.true263
                                        #   in Loop: Header=BB7_40 Depth=1
	movl	-8(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_63
# BB#62:                                # %if.then268
                                        #   in Loop: Header=BB7_40 Depth=1
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	imulq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	subq	%rdx, 16(%rbx)
.LBB7_63:                               # %if.end281
                                        #   in Loop: Header=BB7_40 Depth=1
	testq	%rax, %rax
	movq	%r15, %rax
	je	.LBB7_65
# BB#64:                                # %if.end281
                                        #   in Loop: Header=BB7_40 Depth=1
	movabsq	$1, %rax
.LBB7_65:                               # %if.end281
                                        #   in Loop: Header=BB7_40 Depth=1
	cmpq	$0, -16(%rbx)
	je	.LBB7_67
# BB#66:                                # %if.end281
                                        #   in Loop: Header=BB7_40 Depth=1
	testq	%rax, %rax
	je	.LBB7_68
.LBB7_67:                               # %if.then296
                                        #   in Loop: Header=BB7_40 Depth=1
	movq	16(%rbx), %rax
	addq	%rax, -56(%rbp)
.LBB7_68:                               # %if.end303
                                        #   in Loop: Header=BB7_40 Depth=1
	xorq	%rdx, %rdx
	movq	$1, %rcx
	leaq	-104(%rbp), %rdi
	movq	%r12, %rsi
	callq	function_arg_advance
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	-128(%rbp), %rdx        # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$72, %rbx
	cmpq	%rdx, %rax
	jl	.LBB7_40
.LBB7_69:                               # %for.end
	movq	-208(%rbp), %rdi        # 8-byte Reload
	callq	assemble_external_libcall
	movq	-56(%rbp), %rsi
	movq	%rsi, -264(%rbp)        # 8-byte Spill
	movq	cfun(%rip), %r8
	movq	16(%r8), %rax
	movslq	ix86_preferred_stack_boundary(%rip), %rdx
	movslq	8(%rax), %rbx
	movq	$60, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
	leaq	(%rsi,%rbx), %rax
	leaq	-1(%rdi,%rax), %rsi
	movq	%rsi, %rax
	cqto
	idivq	%rdi
	subq	%rdx, %rsi
	subq	%rbx, %rsi
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jns	.LBB7_71
# BB#70:                                # %for.end
	xorq	%rsi, %rsi
.LBB7_71:                               # %for.end
	movq	%rsi, -56(%rbp)
	movslq	76(%r8), %rax
	cmpq	%rax, %rsi
	movq	-256(%rbp), %r14        # 8-byte Reload
	movq	-128(%rbp), %r12        # 8-byte Reload
	jle	.LBB7_73
# BB#72:                                # %if.then338
	movl	%esi, 76(%r8)
.LBB7_73:                               # %if.end342
	movl	target_flags(%rip), %eax
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB7_83
# BB#74:                                # %if.then345
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r14
	movq	%r14, %r15
	jge	.LBB7_76
# BB#75:                                # %if.then345
	movq	%rsi, %r15
.LBB7_76:                               # %if.then345
	movl	%r15d, highest_outgoing_arg_in_use(%rip)
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	testq	%r13, %r14
	movq	%rbx, stack_usage_map(%rip)
	je	.LBB7_78
# BB#77:                                # %if.then357
	movq	%rbx, %rdi
	movq	-280(%rbp), %rsi        # 8-byte Reload
	movq	%r14, %rdx
	callq	memcpy
.LBB7_78:                               # %if.end359
	movq	%r14, %rax
	andq	%r13, %rax
	movq	%r15, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	je	.LBB7_80
# BB#79:                                # %if.then362
	addq	%r14, %rbx
	subq	%r14, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	memset
.LBB7_80:                               # %if.end367
	movl	virtuals_instantiated(%rip), %eax
	testq	%rax, %rax
	je	.LBB7_82
# BB#81:                                # %if.then369
	movq	global_rtl+16(%rip), %rdi
	xorq	%rsi, %rsi
	callq	plus_constant_wide
	jmp	.LBB7_85
.LBB7_83:                               # %if.else373
	testq	$6144, %rax             # imm = 0x1800
	je	.LBB7_88
# BB#84:                                # %if.then379
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	push_block
.LBB7_85:                               # %if.end384
	movq	%rax, %r14
	jmp	.LBB7_86
.LBB7_82:                               # %if.else371
	movq	global_rtl+72(%rip), %r14
.LBB7_86:                               # %if.end384
	movq	-176(%rbp), %rsi        # 8-byte Reload
	xorq	%rbx, %rbx
	testq	%r14, %r14
	jne	.LBB7_91
# BB#87:                                # %if.end384.land.lhs.true387_crit_edge
	movq	%rsi, -176(%rbp)        # 8-byte Spill
	movl	target_flags(%rip), %eax
.LBB7_88:                               # %land.lhs.true387
	xorq	%r14, %r14
	testq	$6144, %rax             # imm = 0x1800
	movq	$1, %rbx
	jne	.LBB7_90
# BB#89:                                # %if.then393
	movq	-56(%rbp), %rsi
	movq	-264(%rbp), %rax        # 8-byte Reload
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
.LBB7_90:                               # %if.end398
	movq	-176(%rbp), %rsi        # 8-byte Reload
.LBB7_91:                               # %if.end398
	movq	%rbx, -320(%rbp)        # 8-byte Spill
	movl	target_flags(%rip), %eax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	andq	$6144, %rax             # imm = 0x1800
	movq	$-1, -240(%rbp)         # 8-byte Folded Spill
	je	.LBB7_93
# BB#92:                                # %if.end398
	movq	$1, -240(%rbp)          # 8-byte Folded Spill
.LBB7_93:                               # %if.end398
	movq	%r14, -120(%rbp)        # 8-byte Spill
	movq	-128(%rbp), %rcx        # 8-byte Reload
	leaq	-1(%rcx), %rcx
	movq	%rcx, -272(%rbp)        # 8-byte Spill
	testq	%rdx, %rdx
	jle	.LBB7_117
# BB#94:                                # %for.body489.lr.ph
	movq	$32, %rcx
	movq	-272(%rbp), %r12        # 8-byte Reload
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	xorq	%r14, %r14
	testq	%r13, %rax
	je	.LBB7_96
# BB#95:                                # %for.body489.lr.ph
	xorq	%r12, %r12
.LBB7_96:                               # %for.body489.lr.ph
	movq	$32, %rcx
	movq	-240(%rbp), %r9         # 8-byte Reload
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	%r9, -184(%rbp)         # 8-byte Spill
	movq	-128(%rbp), %rcx        # 8-byte Reload
	andq	%r13, %rcx
	movq	%rcx, -192(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB7_97:                               # %for.body489
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_103 Depth 2
                                        #     Child Loop BB7_114 Depth 2
	leaq	(%r12,%r12,8), %rbx
	movl	8(%rsi,%rbx,8), %r8d
	movq	(%rsi,%rbx,8), %rdi
	movq	16(%rsi,%rbx,8), %rdx
	movl	24(%rsi,%rbx,8), %eax
	testq	%rdx, %rdx
	je	.LBB7_99
# BB#98:                                # %for.body489
                                        #   in Loop: Header=BB7_97 Depth=1
	testq	%r13, %rax
	je	.LBB7_116
.LBB7_99:                               # %if.then511
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	%rdx, -160(%rbp)        # 8-byte Spill
	movq	%rax, -152(%rbp)        # 8-byte Spill
	movq	%rdi, -144(%rbp)        # 8-byte Spill
	movq	%r8, -136(%rbp)         # 8-byte Spill
	movzbl	target_flags+1(%rip), %eax
	xorq	%r15, %r15
	testq	$16, %rax
	leaq	32(%rsi,%rbx,8), %rax
	movabsq	$0, %r13
	je	.LBB7_108
# BB#100:                               # %if.then514
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	(%rax), %r15
	movq	%rax, -168(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	48(%rsi,%rbx,8), %rdi
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdi,%r15), %r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movq	%r15, %rdx
	jge	.LBB7_105
# BB#101:                               # %for.body530.lr.ph
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	stack_usage_map(%rip), %r8
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	.align	16, 0x90
.LBB7_103:                              # %for.body530
                                        #   Parent Loop BB7_97 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r8,%rdx), %eax
	testq	%rax, %rax
	je	.LBB7_102
# BB#104:                               # %for.body530
                                        #   in Loop: Header=BB7_103 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_105
.LBB7_102:                              # %for.cond527
                                        #   in Loop: Header=BB7_103 Depth=2
	incq	%rdx
	movq	$32, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jl	.LBB7_103
.LBB7_105:                              # %for.end542
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %rdx
	cmpq	%rax, %rdx
	je	.LBB7_107
# BB#106:                               # %if.then545
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	$3, %rcx
	shlq	%cl, %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	mode_for_size
	movq	-168(%rbp), %rcx        # 8-byte Reload
	movq	(%rcx), %rsi
	movq	%rax, -200(%rbp)        # 8-byte Spill
	movq	-120(%rbp), %rdi        # 8-byte Reload
	callq	plus_constant_wide
	movq	-200(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	memory_address
	movq	-200(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, -216(%rbp)        # 8-byte Spill
	movq	-200(%rbp), %rdi        # 8-byte Reload
	callq	gen_reg_rtx
	movq	-176(%rbp), %rcx        # 8-byte Reload
	movq	%rax, 64(%rcx,%rbx,8)
	movq	%rax, %rdi
	movq	-216(%rbp), %rsi        # 8-byte Reload
	callq	emit_move_insn
.LBB7_107:                              # %if.end571
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	-168(%rbp), %rax        # 8-byte Reload
.LBB7_108:                              # %if.end571
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	(%rax), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	-64(%rbp), %rsi
	movq	%rax, -168(%rbp)        # 8-byte Spill
	testq	%rsi, %rsi
	jne	.LBB7_110
# BB#109:                               # %cond.true580
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	-72(%rbp), %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB7_111
	.align	16, 0x90
.LBB7_110:                              # %cond.false590
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	sizetype_tab+8(%rip), %rdi
	callq	convert
	movq	-72(%rbp), %rdi
	movq	%rax, %rbx
	movq	$1, %rsi
	callq	size_int_wide
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	callq	expand_expr
.LBB7_111:                              # %cond.end599
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	-120(%rbp), %rbx        # 8-byte Reload
	subq	$48, %rsp
	movq	%rax, 40(%rsp)
	movq	-168(%rbp), %rax        # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	%rbx, 16(%rsp)
	movq	-160(%rbp), %rax        # 8-byte Reload
	movq	%rax, (%rsp)
	movq	$0, 32(%rsp)
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	$0, 8(%rsp)
	movq	-144(%rbp), %rdi        # 8-byte Reload
	movq	-136(%rbp), %rsi        # 8-byte Reload
	movq	-152(%rbp), %r9         # 8-byte Reload
	callq	emit_push_insn
	addq	$48, %rsp
	movl	target_flags(%rip), %eax
	testq	$4096, %rax             # imm = 0x1000
	movq	$1, %rsi
	je	.LBB7_115
# BB#112:                               # %cond.end599
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB7_115
# BB#113:                               # %for.body607.lr.ph
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	incq	%r15
	.align	16, 0x90
.LBB7_114:                              # %for.body607
                                        #   Parent Loop BB7_97 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	stack_usage_map(%rip), %rax
	movb	%sil, -1(%rax,%r15)
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rdx, %rax
	jl	.LBB7_114
.LBB7_115:                              # %if.end613
                                        #   in Loop: Header=BB7_97 Depth=1
	movq	%rbx, -120(%rbp)        # 8-byte Spill
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	-176(%rbp), %rsi        # 8-byte Reload
	movq	-184(%rbp), %r9         # 8-byte Reload
	movq	-192(%rbp), %rcx        # 8-byte Reload
.LBB7_116:                              # %for.inc618
                                        #   in Loop: Header=BB7_97 Depth=1
	incq	%r14
	movq	%r14, %rax
	andq	%r13, %rax
	addq	%r9, %r12
	cmpq	%rcx, %rax
	jne	.LBB7_97
.LBB7_117:                              # %for.end621
	movq	%rsi, -176(%rbp)        # 8-byte Spill
	cmpq	$0, -320(%rbp)          # 8-byte Folded Reload
	je	.LBB7_120
# BB#118:                               # %land.lhs.true624
	movzbl	target_flags+1(%rip), %eax
	testq	$24, %rax
	je	.LBB7_120
# BB#119:                               # %if.then630
	movq	-56(%rbp), %rsi
	movq	-264(%rbp), %rax        # 8-byte Reload
	subq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %rdi
	callq	anti_adjust_stack
.LBB7_120:                              # %if.end635
	movl	target_flags(%rip), %ebx
	xorq	%rsi, %rsi
	movq	-208(%rbp), %rdi        # 8-byte Reload
	callq	protect_from_queue
	movzbl	(%rax), %ecx
	cmpq	$68, %rcx
	je	.LBB7_122
# BB#121:                               # %if.then3.i
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	memory_address
.LBB7_122:                              # %prepare_call_address.exit
	movq	%rax, -144(%rbp)        # 8-byte Spill
	movq	-128(%rbp), %rdx        # 8-byte Reload
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_141
# BB#123:                               # %for.body649.lr.ph
	andq	$6144, %rbx             # imm = 0x1800
	movq	$32, %rcx
	movq	-272(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r14, %r14
	testq	%r13, %rbx
	je	.LBB7_125
# BB#124:                               # %for.body649.lr.ph
	xorq	%rax, %rax
.LBB7_125:                              # %for.body649.lr.ph
	leaq	(%rax,%rax,8), %rax
	movq	-176(%rbp), %rcx        # 8-byte Reload
	leaq	24(%rcx,%rax,8), %r15
	movq	$32, %rcx
	movq	-240(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,8), %r12
	.align	16, 0x90
.LBB7_126:                              # %for.body649
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB7_137
# BB#127:                               # %land.lhs.true664
                                        #   in Loop: Header=BB7_126 Depth=1
	movzbl	(%rdi), %eax
	movq	-24(%r15), %rsi
	cmpq	$39, %rax
	jne	.LBB7_134
# BB#128:                               # %if.then669
                                        #   in Loop: Header=BB7_126 Depth=1
	movzbl	2(%rsi), %eax
	movq	%rdx, %rbx
	movzbl	mode_size(%rax), %edx
	callq	emit_group_load
	jmp	.LBB7_136
	.align	16, 0x90
.LBB7_134:                              # %if.else676
                                        #   in Loop: Header=BB7_126 Depth=1
	movl	(%r15), %eax
	testq	%r13, %rax
	jne	.LBB7_137
# BB#135:                               # %if.then682
                                        #   in Loop: Header=BB7_126 Depth=1
	movq	%rdx, %rbx
	callq	emit_move_insn
.LBB7_136:                              # %if.end685
                                        #   in Loop: Header=BB7_126 Depth=1
	movq	%rbx, %rdx
.LBB7_137:                              # %if.end685
                                        #   in Loop: Header=BB7_126 Depth=1
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	incq	%rcx
	movl	%ecx, 4(%rax)
	incq	%r14
	movq	%r14, %rax
	andq	%r13, %rax
	movq	%rdx, %rbx
	andq	%r13, %rbx
	addq	%r12, %r15
	cmpq	%rbx, %rax
	jne	.LBB7_126
# BB#129:                               # %for.cond693.preheader
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_141
# BB#130:                               # %for.body696.preheader
	movq	-176(%rbp), %rax        # 8-byte Reload
	leaq	16(%rax), %r15
	movq	$1, %r12
	.align	16, 0x90
.LBB7_131:                              # %for.body696
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rsi
	testq	%rsi, %rsi
	je	.LBB7_140
# BB#132:                               # %land.lhs.true703
                                        #   in Loop: Header=BB7_131 Depth=1
	movq	%rdx, %r14
	movzbl	(%rsi), %eax
	leaq	-112(%rbp), %rdi
	cmpq	$39, %rax
	jne	.LBB7_138
# BB#133:                               # %if.then708
                                        #   in Loop: Header=BB7_131 Depth=1
	callq	use_group_regs
	jmp	.LBB7_139
	.align	16, 0x90
.LBB7_138:                              # %if.then712
                                        #   in Loop: Header=BB7_131 Depth=1
	callq	use_reg
.LBB7_139:                              # %for.inc715
                                        #   in Loop: Header=BB7_131 Depth=1
	movq	%r14, %rdx
.LBB7_140:                              # %for.inc715
                                        #   in Loop: Header=BB7_131 Depth=1
	movq	%r12, %rax
	andq	%r13, %rax
	incq	%r12
	addq	$72, %r15
	cmpq	%rbx, %rax
	jne	.LBB7_131
.LBB7_141:                              # %for.end717
	movq	-224(%rbp), %r14        # 8-byte Reload
	testq	%r14, %r14
	je	.LBB7_145
# BB#142:                               # %for.end717
	movq	struct_value_rtx(%rip), %r12
	testq	%r12, %r12
	je	.LBB7_145
# BB#143:                               # %if.then725
	movl	target_flags(%rip), %ebx
	movq	$25, %rcx
	movq	8(%r14), %rdi
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	xorq	%rsi, %rsi
	callq	force_operand
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	force_reg
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	movq	struct_value_rtx(%rip), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB7_145
# BB#144:                               # %if.then739
	leaq	-112(%rbp), %rdi
	callq	use_reg
.LBB7_145:                              # %if.end741
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
	movl	4(%rcx), %eax
	incq	%rax
	movl	%eax, 4(%rcx)
	xorq	%rax, %rax
	testq	%r14, %r14
	jne	.LBB7_146
# BB#147:                               # %if.end741
	movq	-232(%rbp), %r15        # 8-byte Reload
	testq	%r13, %r15
	je	.LBB7_149
# BB#148:                               # %cond.true750
	movq	%r15, %rdi
	callq	hard_libcall_value
	movq	cfun(%rip), %rcx
	movq	16(%rcx), %rcx
	jmp	.LBB7_149
.LBB7_146:
	movq	-232(%rbp), %r15        # 8-byte Reload
.LBB7_149:                              # %cond.end753
	movq	%rax, -136(%rbp)        # 8-byte Spill
	movslq	ix86_preferred_stack_boundary(%rip), %rax
	movl	8(%rcx), %edx
	movq	$60, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$7, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	decq	%rsi
	andq	%rdx, %rsi
	testq	%r13, %rsi
	movq	%r13, %r14
	jne	.LBB7_150
# BB#151:                               # %if.end762
	callq	get_last_insn
	movq	-208(%rbp), %rcx        # 8-byte Reload
	movq	8(%rcx), %rdi
	movq	%rax, %rbx
	callq	get_identifier
	movq	%rax, -152(%rbp)        # 8-byte Spill
	testq	%r14, %r15
	je	.LBB7_152
# BB#153:                               # %cond.false770
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%r15, -232(%rbp)        # 8-byte Spill
	callq	type_for_mode
	movq	%rax, %rdi
	jmp	.LBB7_154
.LBB7_152:                              # %cond.true769
	movq	%r15, -232(%rbp)        # 8-byte Spill
	movq	global_trees+216(%rip), %rdi
.LBB7_154:                              # %cond.end772
	xorq	%rsi, %rsi
	callq	build_function_type
	movq	-56(%rbp), %r15
	movq	global_trees+216(%rip), %rdx
	movq	%rax, %r12
	leaq	-104(%rbp), %rdi
	xorq	%rsi, %rsi
	movq	$1, %rcx
	callq	function_arg
	movq	-112(%rbp), %rcx
	movq	-296(%rbp), %rdx        # 8-byte Reload
	incq	%rdx
	subq	$48, %rsp
	movq	-288(%rbp), %r13        # 8-byte Reload
	movq	%r13, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	-136(%rbp), %rcx        # 8-byte Reload
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	-144(%rbp), %rdi        # 8-byte Reload
	movq	-152(%rbp), %rsi        # 8-byte Reload
	movq	%r12, %rdx
	movq	-264(%rbp), %rcx        # 8-byte Reload
	movq	%r15, %r8
	movq	-304(%rbp), %r9         # 8-byte Reload
	callq	emit_call_1
	addq	$48, %rsp
	testq	$66, %r13
	movq	%r13, %r12
	je	.LBB7_160
# BB#155:                               # %if.then782
	callq	get_last_insn
	.align	16, 0x90
.LBB7_156:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$34, %rcx
	je	.LBB7_159
# BB#157:                               # %while.body
                                        #   in Loop: Header=BB7_156 Depth=1
	movq	16(%rax), %rax
	cmpq	%rbx, %rax
	jne	.LBB7_156
# BB#158:                               # %if.then793
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.emit_library_call_value_1, %rcx
	movq	$4069, %rsi             # imm = 0xFE5
	jmp	.LBB7_50
.LBB7_159:                              # %while.end
	movq	%rax, %rdi
	callq	emit_barrier_after
.LBB7_160:                              # %if.end796
	movq	cfun(%rip), %rax
	movq	16(%rax), %rax
	movl	4(%rax), %ecx
	decq	%rcx
	movq	%r14, %r13
	movq	-312(%rbp), %rdx        # 8-byte Reload
	testq	%r13, %rdx
	movl	%ecx, 4(%rax)
	movq	-224(%rbp), %r14        # 8-byte Reload
	je	.LBB7_171
# BB#161:                               # %if.then802
	movq	-136(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB7_163
# BB#162:                               # %lor.lhs.false805
	movl	(%rax), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	jne	.LBB7_164
.LBB7_163:                              # %if.then810
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	callq	emit_insns
	jmp	.LBB7_171
.LBB7_164:                              # %if.else813
	movq	$32, %rcx
	movq	-128(%rbp), %r14        # 8-byte Reload
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	callq	gen_reg_rtx
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	jle	.LBB7_165
# BB#166:
	movq	%rcx, -152(%rbp)        # 8-byte Spill
	movq	$1, %r15
	andq	%r13, %r14
	movq	-176(%rbp), %rbx        # 8-byte Reload
	.align	16, 0x90
.LBB7_167:                              # %for.body822
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdx
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%r15, %rcx
	andq	%r13, %rcx
	incq	%r15
	addq	$72, %rbx
	cmpq	%r14, %rcx
	jne	.LBB7_167
	jmp	.LBB7_168
.LBB7_165:
	movq	%rcx, -152(%rbp)        # 8-byte Spill
.LBB7_168:                              # %for.end829
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	-144(%rbp), %rdx        # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	testq	$512, %r12              # imm = 0x200
	je	.LBB7_170
# BB#169:                               # %if.then834
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %rbx
.LBB7_170:                              # %if.end839
	movq	-136(%rbp), %rdx        # 8-byte Reload
	movq	%r15, %rdi
	movq	-152(%rbp), %rsi        # 8-byte Reload
	movq	%rbx, %rcx
	callq	emit_libcall_block
	movq	-224(%rbp), %r14        # 8-byte Reload
.LBB7_171:                              # %if.end841
	callq	pop_temp_slots
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	je	.LBB7_172
# BB#173:                               # %if.end841
	movq	-328(%rbp), %rax        # 8-byte Reload
	testq	%r13, %rax
	movq	-128(%rbp), %rdx        # 8-byte Reload
	je	.LBB7_174
# BB#175:                               # %if.then846
	testq	%r14, %r14
	je	.LBB7_182
# BB#176:                               # %if.then848
	movq	%rdx, %r15
	movq	-248(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	movq	%r14, %rbx
	je	.LBB7_178
# BB#177:                               # %if.then848
	movq	%rax, %rbx
.LBB7_178:                              # %if.then848
	cmpq	%r14, %rbx
	je	.LBB7_179
# BB#181:                               # %if.then855
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	emit_move_insn
	movq	%r15, %rdx
	jmp	.LBB7_185
.LBB7_172:
	movq	-248(%rbp), %rbx        # 8-byte Reload
	movq	-128(%rbp), %rdx        # 8-byte Reload
	jmp	.LBB7_185
.LBB7_174:
	movq	-248(%rbp), %rbx        # 8-byte Reload
	jmp	.LBB7_185
.LBB7_182:                              # %if.else858
	movq	%rdx, %r14
	movq	-232(%rbp), %rdi        # 8-byte Reload
	callq	hard_libcall_value
	movq	%rax, %rbx
	movq	-248(%rbp), %r15        # 8-byte Reload
	testq	%r15, %r15
	je	.LBB7_184
# BB#183:                               # %if.then861
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	emit_move_insn
	movq	%r15, %rbx
.LBB7_184:                              # %if.end868
	movq	%r14, %rdx
	jmp	.LBB7_185
.LBB7_179:
	movq	%r14, %rbx
	movq	%r15, %rdx
.LBB7_185:                              # %if.end868
	movq	-176(%rbp), %rsi        # 8-byte Reload
	movzbl	target_flags+1(%rip), %eax
	testq	$16, %rax
	je	.LBB7_192
# BB#186:                               # %for.cond899.preheader
	movq	%rbx, -136(%rbp)        # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_191
# BB#187:                               # %for.body902.preheader
	addq	$64, %rsi
	movq	$1, %rbx
	andq	%r13, %rdx
	.align	16, 0x90
.LBB7_188:                              # %for.body902
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rax
	testq	%rax, %rax
	je	.LBB7_190
# BB#189:                               # %if.then907
                                        #   in Loop: Header=BB7_188 Depth=1
	movq	%rsi, %r15
	movq	-32(%r15), %rsi
	movzbl	2(%rax), %r14d
	movq	-120(%rbp), %rdi        # 8-byte Reload
	movq	%rdx, %r12
	callq	plus_constant_wide
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	memory_address
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	(%r15), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	movq	%r15, %rsi
	movq	%r12, %rdx
.LBB7_190:                              # %for.inc928
                                        #   in Loop: Header=BB7_188 Depth=1
	movq	%rbx, %rax
	andq	%r13, %rax
	incq	%rbx
	addq	$72, %rsi
	cmpq	%rdx, %rax
	jne	.LBB7_188
.LBB7_191:                              # %for.end930
	movq	-256(%rbp), %rax        # 8-byte Reload
	movl	%eax, highest_outgoing_arg_in_use(%rip)
	movq	-280(%rbp), %rax        # 8-byte Reload
	movq	%rax, stack_usage_map(%rip)
	movq	-136(%rbp), %rbx        # 8-byte Reload
.LBB7_192:                              # %if.end931
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB7_4:                                # %sw.bb2
	movq	$528, %r14              # imm = 0x210
	jmp	.LBB7_11
.LBB7_5:                                # %sw.bb4
	movq	$4113, %r14             # imm = 0x1011
	jmp	.LBB7_11
.LBB7_6:                                # %sw.bb6
	movq	$4624, %r14             # imm = 0x1210
	jmp	.LBB7_11
.LBB7_7:                                # %sw.bb8
	movq	$18, %r14
	jmp	.LBB7_11
.LBB7_8:                                # %sw.bb10
	movq	$2, %r14
	jmp	.LBB7_11
.LBB7_9:                                # %sw.bb11
	movq	$2048, %r14             # imm = 0x800
	jmp	.LBB7_11
.LBB7_10:                               # %sw.bb12
	movq	$32, %r14
	jmp	.LBB7_11
.LBB7_49:                               # %if.then176
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.emit_library_call_value_1, %rcx
	movq	$3641, %rsi             # imm = 0xE39
	jmp	.LBB7_50
.LBB7_60:                               # %if.then259
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.emit_library_call_value_1, %rcx
	movq	$3724, %rsi             # imm = 0xE8C
	jmp	.LBB7_50
.LBB7_150:                              # %if.then761
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.emit_library_call_value_1, %rcx
	movq	$4032, %rsi             # imm = 0xFC0
.LBB7_50:                               # %if.then176
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp100:
	.size	emit_library_call_value_1, .Ltmp100-emit_library_call_value_1
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI7_0:
	.quad	.LBB7_3
	.quad	.LBB7_4
	.quad	.LBB7_5
	.quad	.LBB7_6
	.quad	.LBB7_7
	.quad	.LBB7_8
	.quad	.LBB7_9
	.quad	.LBB7_10

	.text
	.globl	emit_library_call_value
	.align	16, 0x90
	.type	emit_library_call_value,@function
emit_library_call_value:                # @emit_library_call_value
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$88, %rsp
.Ltmp102:
	.cfi_def_cfa_offset 96
	movq	%r8, %r10
	movq	%rcx, %rax
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	%r9, 56(%rsp)
	leaq	16(%rsp), %rdi
	movq	%rdi, 80(%rsp)
	leaq	96(%rsp), %rdi
	movq	%rdi, 72(%rsp)
	movq	$48, 68(%rsp)
	movq	$40, 64(%rsp)
	leaq	64(%rsp), %r8
	movq	$1, %rdi
	movq	%r8, (%rsp)
	movq	%rax, %r8
	movq	%r10, %r9
	callq	emit_library_call_value_1
	addq	$88, %rsp
	retq
.Ltmp103:
	.size	emit_library_call_value, .Ltmp103-emit_library_call_value
	.cfi_endproc

	.align	16, 0x90
	.type	check_sibcall_argument_overlap_1,@function
check_sibcall_argument_overlap_1:       # @check_sibcall_argument_overlap_1
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
	subq	$32, %rsp
.Ltmp115:
	.cfi_def_cfa_offset 80
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
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB9_21
# BB#1:                                 # %if.end
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB9_12
# BB#2:                                 # %if.then2
	movq	cfun(%rip), %rdx
	movq	8(%rdi), %rcx
	movq	128(%rdx), %rdx
	xorq	%r9, %r9
	cmpq	%rdx, %rcx
	je	.LBB9_7
# BB#3:                                 # %if.else
	movzbl	(%rcx), %esi
	xorq	%rbx, %rbx
	cmpq	$75, %rsi
	jne	.LBB9_21
# BB#4:                                 # %land.lhs.true
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB9_21
# BB#5:                                 # %land.lhs.true19
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB9_21
# BB#6:                                 # %if.then29
	movq	8(%rcx), %r9
.LBB9_7:                                # %if.end40
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %r10d
	testq	%r10, %r10
	movabsq	$0, %rbx
	je	.LBB9_21
# BB#8:                                 # %for.body.lr.ph
	movq	stored_args_map(%rip), %r8
	movl	(%r8), %r15d
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r15
	movabsq	$4294967232, %r11       # imm = 0xFFFFFFC0
	.align	16, 0x90
.LBB9_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%rdx,%r9), %rax
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	%r15, %rcx
	jae	.LBB9_11
# BB#10:                                # %land.lhs.true49
                                        #   in Loop: Header=BB9_9 Depth=1
	movq	%rax, %rbx
	andq	%r11, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	16(%r8,%rbx), %rsi
	movq	$1, %rbx
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rdi, %rsi
	jne	.LBB9_21
.LBB9_11:                               # %for.inc
                                        #   in Loop: Header=BB9_9 Depth=1
	incq	%rdx
	movq	%rdx, %rax
	andq	%r14, %rax
	xorq	%rbx, %rbx
	cmpq	%r10, %rax
	jb	.LBB9_9
	jmp	.LBB9_21
.LBB9_12:                               # %if.end56
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movzbl	rtx_length(%rcx), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB9_21
# BB#13:                                # %for.body65.lr.ph
	movq	rtx_format(,%rcx,8), %r12
	xorq	%r13, %r13
	.align	16, 0x90
.LBB9_14:                               # %for.body65
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_18 Depth 2
	movzbl	(%r12), %eax
	cmpq	$101, %rax
	jne	.LBB9_15
# BB#19:                                # %if.then69
                                        #   in Loop: Header=BB9_14 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax,%r13,8), %rdi
	callq	check_sibcall_argument_overlap_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rbx
	je	.LBB9_20
	jmp	.LBB9_21
	.align	16, 0x90
.LBB9_15:                               # %for.body65
                                        #   in Loop: Header=BB9_14 Depth=1
	cmpq	$69, %rax
	jne	.LBB9_20
# BB#16:                                # %for.cond82.preheader
                                        #   in Loop: Header=BB9_14 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax,%r13,8), %r15
	movslq	(%r15), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$1, %rbx
	movq	$1, %r14
	jle	.LBB9_20
	.align	16, 0x90
.LBB9_18:                               # %for.body88
                                        #   Parent Loop BB9_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r15,%r14,8), %rdi
	callq	check_sibcall_argument_overlap_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_21
# BB#17:                                # %for.cond82
                                        #   in Loop: Header=BB9_18 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r14
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB9_18
	.align	16, 0x90
.LBB9_20:                               # %for.inc104
                                        #   in Loop: Header=BB9_14 Depth=1
	incq	%r13
	incq	%r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB9_14
.LBB9_21:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	check_sibcall_argument_overlap_1, .Ltmp121-check_sibcall_argument_overlap_1
	.cfi_endproc

	.align	16, 0x90
	.type	calls_function_1,@function
calls_function_1:                       # @calls_function_1
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
	subq	$16, %rsp
.Ltmp133:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %r15
	movzbl	16(%r15), %r14d
	movzbl	tree_code_type(%r14), %r13d
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	callq	first_rtl_op
	movq	%rax, %r12
	cmpq	$146, %r14
	movq	$1, %rbx
	ja	.LBB10_52
# BB#1:                                 # %if.end
	cmpq	$117, %r14
	jg	.LBB10_30
# BB#2:                                 # %if.end
	cmpq	$45, %r14
	jg	.LBB10_10
# BB#3:                                 # %if.end
	cmpq	$2, %r14
	jne	.LBB10_4
# BB#21:                                # %for.cond128.preheader
	testq	%r15, %r15
	je	.LBB10_51
# BB#22:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB10_23:                              # %for.body131
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r15), %rdi
	movq	%rbx, %rsi
	callq	calls_function_1
	testq	%r14, %rax
	jne	.LBB10_24
# BB#43:                                # %for.inc138
                                        #   in Loop: Header=BB10_23 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB10_23
	jmp	.LBB10_51
.LBB10_30:                              # %if.end
	cmpq	$118, %r14
	jne	.LBB10_18
# BB#31:                                # %sw.bb75
	xorq	%rbx, %rbx
	cmpq	$0, 48(%r15)
	jne	.LBB10_52
# BB#32:                                # %if.end82
	movq	calls_function_save_exprs(%rip), %rsi
	movq	%r15, %rdi
	callq	value_member
	testq	%rax, %rax
	jne	.LBB10_52
# BB#33:                                # %if.end86
	movq	calls_function_save_exprs(%rip), %rdx
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	movq	%r15, %rsi
	callq	tree_cons
	movq	%rax, calls_function_save_exprs(%rip)
	movq	32(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB10_52
# BB#34:                                # %land.rhs
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	calls_function_1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB10_35
# BB#36:                                # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB10_52
.LBB10_10:                              # %if.end
	cmpq	$46, %r14
	jne	.LBB10_11
# BB#25:                                # %sw.bb64
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB10_51
# BB#26:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	8(%rsp), %r15           # 8-byte Reload
	.align	16, 0x90
.LBB10_28:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movq	%r15, %rsi
	callq	calls_function_1
	testq	%r14, %rax
	jne	.LBB10_29
# BB#27:                                # %for.cond
                                        #   in Loop: Header=BB10_28 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_28
	jmp	.LBB10_51
.LBB10_4:                               # %if.end
	cmpq	$4, %r14
	jne	.LBB10_18
# BB#5:                                 # %sw.bb98
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	movq	8(%rsp), %r12           # 8-byte Reload
	je	.LBB10_38
# BB#6:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_7:                               # %for.body101
                                        # =>This Inner Loop Header: Depth=1
	movq	104(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB10_37
# BB#8:                                 # %land.lhs.true104
                                        #   in Loop: Header=BB10_7 Depth=1
	movq	%r12, %rsi
	callq	calls_function_1
	testq	%r14, %rax
	jne	.LBB10_9
.LBB10_37:                              # %for.inc111
                                        #   in Loop: Header=BB10_7 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_7
.LBB10_38:                              # %for.end114
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB10_51
# BB#39:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_41:                              # %for.body118
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	calls_function_1
	testq	%r14, %rax
	jne	.LBB10_42
# BB#40:                                # %for.cond116
                                        #   in Loop: Header=BB10_41 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_41
	jmp	.LBB10_51
.LBB10_11:                              # %if.end
	cmpq	$53, %r14
	jne	.LBB10_18
# BB#12:                                # %sw.bb
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB10_52
# BB#13:                                # %if.else
	movq	32(%r15), %rax
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$23, %rdx
	jne	.LBB10_15
# BB#14:                                # %land.lhs.true
	movzbl	62(%rcx), %ecx
	testq	$2, %rcx
	jne	.LBB10_52
.LBB10_15:                              # %if.else28
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB10_18
# BB#16:                                # %land.lhs.true38
	movq	32(%rax), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$30, %rax
	jne	.LBB10_18
# BB#17:                                # %land.lhs.true51
	xorq	%rsi, %rsi
	callq	special_function_p
	testq	$8, %rax
	jne	.LBB10_52
.LBB10_18:                              # %sw.epilog
	movq	$56, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$113, %rax
	jg	.LBB10_44
# BB#19:                                # %sw.epilog
	addq	$-49, %r13
	andq	$255, %r13
	cmpq	$52, %r13
	ja	.LBB10_51
# BB#20:                                # %sw.epilog
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	movabsq	$5066549580793859, %rcx # imm = 0x12000000000803
	testq	%rcx, %rax
	jne	.LBB10_45
	jmp	.LBB10_51
.LBB10_44:                              # %sw.epilog
	andq	$255, %r13
	cmpq	$114, %r13
	jne	.LBB10_51
.LBB10_45:                              # %for.cond160.preheader
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB10_51
# BB#46:
	movq	$4, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	8(%rsp), %r13           # 8-byte Reload
	.align	16, 0x90
.LBB10_47:                              # %for.body163
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB10_50
# BB#48:                                # %land.lhs.true170
                                        #   in Loop: Header=BB10_47 Depth=1
	movq	%r13, %rsi
	callq	calls_function_1
	testq	%r14, %rax
	jne	.LBB10_49
.LBB10_50:                              # %for.inc179
                                        #   in Loop: Header=BB10_47 Depth=1
	leaq	-3(%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdx, %rax
	jl	.LBB10_47
.LBB10_51:
	xorq	%rbx, %rbx
.LBB10_52:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_24:
	movq	$1, %rbx
	jmp	.LBB10_52
.LBB10_29:
	movq	$1, %rbx
	jmp	.LBB10_52
.LBB10_49:
	movq	$1, %rbx
	jmp	.LBB10_52
.LBB10_42:
	movq	$1, %rbx
	jmp	.LBB10_52
.LBB10_9:
	movq	$1, %rbx
	jmp	.LBB10_52
.LBB10_35:
	movabsq	$0, %rbx
	jmp	.LBB10_52
.Ltmp139:
	.size	calls_function_1, .Ltmp139-calls_function_1
	.cfi_endproc

	.type	expand_call.currently_expanding_call,@object # @expand_call.currently_expanding_call
	.local	expand_call.currently_expanding_call
	.comm	expand_call.currently_expanding_call,4,4
	.type	highest_outgoing_arg_in_use,@object # @highest_outgoing_arg_in_use
	.local	highest_outgoing_arg_in_use
	.comm	highest_outgoing_arg_in_use,4,4
	.type	stack_usage_map,@object # @stack_usage_map
	.local	stack_usage_map
	.comm	stack_usage_map,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"can't inline call to `%s'"
	.size	.L.str, 26

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"called from here"
	.size	.L.str1, 17

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"function call has aggregate value"
	.size	.L.str2, 34

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"calls.c"
	.size	.L.str3, 8

	.type	.L__FUNCTION__.expand_call,@object # @__FUNCTION__.expand_call
.L__FUNCTION__.expand_call:
	.asciz	"expand_call"
	.size	.L__FUNCTION__.expand_call, 12

	.type	stack_arg_under_construction,@object # @stack_arg_under_construction
	.comm	stack_arg_under_construction,4,4
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"__bb_fork_func"
	.size	.L.str4, 15

	.type	stored_args_map,@object # @stored_args_map
	.local	stored_args_map
	.comm	stored_args_map,8,8
	.type	.L__FUNCTION__.store_one_arg,@object # @__FUNCTION__.store_one_arg
.L__FUNCTION__.store_one_arg:
	.asciz	"store_one_arg"
	.size	.L__FUNCTION__.store_one_arg, 14

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"variable offset is passed partially in stack and in reg"
	.size	.L.str5, 56

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"variable size is passed partially in stack and in reg"
	.size	.L.str6, 54

	.type	.L__FUNCTION__.emit_library_call_value_1,@object # @__FUNCTION__.emit_library_call_value_1
.L__FUNCTION__.emit_library_call_value_1:
	.asciz	"emit_library_call_value_1"
	.size	.L__FUNCTION__.emit_library_call_value_1, 26

	.type	.L__FUNCTION__.emit_call_1,@object # @__FUNCTION__.emit_call_1
.L__FUNCTION__.emit_call_1:
	.asciz	"emit_call_1"
	.size	.L__FUNCTION__.emit_call_1, 12

	.type	.L__FUNCTION__.compute_argument_block_size,@object # @__FUNCTION__.compute_argument_block_size
.L__FUNCTION__.compute_argument_block_size:
	.asciz	"compute_argument_block_size"
	.size	.L__FUNCTION__.compute_argument_block_size, 28

	.type	.L__FUNCTION__.precompute_arguments,@object # @__FUNCTION__.precompute_arguments
.L__FUNCTION__.precompute_arguments:
	.asciz	"precompute_arguments"
	.size	.L__FUNCTION__.precompute_arguments, 21

	.type	calls_function_save_exprs,@object # @calls_function_save_exprs
	.local	calls_function_save_exprs
	.comm	calls_function_save_exprs,8,8
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"inlining failed in call to `%s'"
	.size	.L.str7, 32

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"alloca"
	.size	.L.str8, 7

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"__builtin_alloca"
	.size	.L.str9, 17

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"setjmp"
	.size	.L.str10, 7

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"setjmp_syscall"
	.size	.L.str11, 15

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"sigsetjmp"
	.size	.L.str12, 10

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"savectx"
	.size	.L.str13, 8

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"siglongjmp"
	.size	.L.str14, 11

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"qsetjmp"
	.size	.L.str15, 8

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"vfork"
	.size	.L.str16, 6

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"longjmp"
	.size	.L.str17, 8

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"fork"
	.size	.L.str18, 5

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"clone"
	.size	.L.str19, 6

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"malloc"
	.size	.L.str20, 7

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"calloc"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"strdup"
	.size	.L.str22, 7


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
