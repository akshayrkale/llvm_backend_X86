	.file	"reload.c.bc"
	.text
	.globl	get_secondary_mem
	.align	16, 0x90
	.type	get_secondary_mem,@function
get_secondary_mem:                      # @get_secondary_mem
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
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %r8
	movq	%rsi, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	target_flags(%rip), %edi
	movzbl	mode_bitsize(%rax,%rax), %edx
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdi, %rcx
	cmpq	%rcx, %rdx
	jae	.LBB0_4
# BB#1:                                 # %land.lhs.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$7, %rcx
	ja	.LBB0_4
# BB#2:                                 # %land.lhs.true
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$170, %rax
	je	.LBB0_4
# BB#3:                                 # %if.then
	xorq	%rdx, %rdx
	movq	%rcx, %rsi
	movq	%r8, %rbx
	callq	mode_for_size
	movq	%rbx, %r8
	movq	%rax, %r14
.LBB0_4:                                # %if.end
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	imulq	$240, %r12, %r15
	movq	secondary_memlocs_elim(%r15,%rbx,8), %r13
	testq	%r13, %r13
	jne	.LBB0_17
# BB#5:                                 # %if.end36
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	secondary_memlocs(,%r12,8), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_7
# BB#6:                                 # %if.then41
	movzbl	mode_size(%r12), %esi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	assign_stack_local
	movq	%rax, %rdi
	movq	%rdi, secondary_memlocs(,%r12,8)
.LBB0_7:                                # %if.end48
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	eliminate_regs
	movq	%rax, %r13
	movq	8(%r13), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_16
# BB#8:                                 # %land.lhs.true55
	movq	secondary_memlocs(,%r12,8), %rax
	cmpq	%rax, %r13
	jne	.LBB0_10
# BB#9:                                 # %if.end62
	movq	%r13, %rdi
	callq	copy_rtx
	movq	%rax, %r13
.LBB0_10:                               # %if.then64
	movq	24(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	$3, %r9
	testq	%rcx, %rax
	je	.LBB0_15
# BB#11:                                # %cond.false
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB0_12
# BB#13:                                # %cond.false
	movabsq	$1, %rax
	jmp	.LBB0_14
.LBB0_12:
	movabsq	$0, %rax
.LBB0_14:                               # %cond.false
	xorq	$1, %rax
	leaq	5(,%rax,4), %r9
.LBB0_15:                               # %cond.end
	movq	8(%r13), %rdx
	leaq	8(%r13), %rcx
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	16(%rsp), %r8           # 8-byte Reload
	callq	find_reloads_address
.LBB0_16:                               # %if.end78
	leaq	secondary_memlocs_elim(%r15,%rbx,8), %rax
	movq	%r13, (%rax)
.LBB0_17:                               # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	get_secondary_mem, .Ltmp17-get_secondary_mem
	.cfi_endproc

	.globl	strict_memory_address_p
	.align	16, 0x90
	.type	strict_memory_address_p,@function
strict_memory_address_p:                # @strict_memory_address_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp19:
	.cfi_def_cfa_offset 16
	movq	$1, %rdx
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB1_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB1_1:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp20:
	.size	strict_memory_address_p, .Ltmp20-strict_memory_address_p
	.cfi_endproc

	.align	16, 0x90
	.type	find_reloads_address,@function
find_reloads_address:                   # @find_reloads_address
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
	subq	$96, %rsp
.Ltmp32:
	.cfi_def_cfa_offset 144
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
	movq	%rdx, %r12
	movq	152(%rsp), %r13
	movzbl	(%r12), %eax
	movq	144(%rsp), %rbx
	movq	%r9, 56(%rsp)           # 8-byte Spill
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, %r14
	cmpq	$61, %rax
	jne	.LBB2_36
# BB#1:                                 # %if.then
	movslq	8(%r12), %r15
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r15,8), %rdi
	testq	%rdi, %rdi
	movq	%rdi, 72(%rsp)
	je	.LBB2_5
# BB#2:                                 # %land.lhs.true
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	eliminate_regs
	testq	%rax, %rax
	movq	%rax, 72(%rsp)
	je	.LBB2_5
# BB#3:                                 # %land.lhs.true3
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_5
# BB#4:                                 # %if.then6
	movq	72(%rsp), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	xorq	%r13, %r13
	jmp	.LBB2_127
.LBB2_36:                               # %if.end204
	movq	%r13, %rbx
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_43
# BB#37:                                # %if.then207
	movzbl	(%r12), %eax
	cmpq	$75, %rax
	jne	.LBB2_41
# BB#38:                                # %land.lhs.true212
	movq	16(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB2_41
# BB#39:                                # %land.lhs.true220
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_41
# BB#40:                                # %land.lhs.true228
	movl	8(%rax), %eax
	movq	reg_equiv_constant(%rip), %rcx
	xorq	%r13, %r13
	cmpq	$0, (%rcx,%rax,8)
	je	.LBB2_127
.LBB2_41:                               # %if.end240
	xorq	%r13, %r13
	movb	%r13b, subst_reg_equivs_changed(%rip)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	subst_reg_equivs
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movzbl	subst_reg_equivs_changed(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_127
# BB#42:                                # %if.end244
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_127
.LBB2_43:                               # %if.end249
	movl	(%r12), %r13d
	movq	%r13, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rsi, %rsi
	cmpq	$83, %rax
	jne	.LBB2_45
# BB#44:                                # %if.then254
	movq	8(%r12), %rax
	movl	(%rax), %r13d
	addq	$8, %r12
	movq	$1, %rsi
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	%rax, %r12
.LBB2_45:                               # %if.end258
	movq	%r13, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$75, %rax
	jne	.LBB2_46
# BB#70:                                # %land.lhs.true381
	movq	8(%r12), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB2_96
# BB#71:                                # %land.lhs.true389
	movl	8(%rcx), %ecx
	cmpq	$52, %rcx
	ja	.LBB2_85
# BB#72:                                # %land.lhs.true398
	cmpq	$20, %rcx
	ja	.LBB2_74
# BB#73:                                # %land.lhs.true398
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$1114367, %rdx          # imm = 0x1100FF
	jne	.LBB2_77
.LBB2_74:                               # %lor.lhs.false425
	leaq	-37(%rcx), %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	cmpq	$8, %rdx
	jb	.LBB2_77
# BB#75:                                # %lor.lhs.false443
	movq	reg_renumber(%rip), %rdx
	movzbl	(%rdx,%rcx,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB2_85
# BB#76:                                # %lor.lhs.false443
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rdx
	je	.LBB2_85
.LBB2_77:                               # %land.lhs.true479
	movq	16(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB2_85
# BB#78:                                # %if.then487
	movq	%rsi, %r14
	testq	%r15, %r15
	movq	144(%rsp), %rbx
	je	.LBB2_82
# BB#79:                                # %if.then489
	movq	(%r15), %rdi
	callq	copy_rtx
	movq	%rax, (%r15)
	addq	$8, %rax
	testq	%r14, %r14
	je	.LBB2_81
# BB#80:                                # %if.then495
	movq	(%rax), %rax
	addq	$8, %rax
.LBB2_81:                               # %if.end500
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_82:                               # %if.end500
	movzbl	double_reg_address_ok(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_84
# BB#83:                                # %if.then502
	movq	%r12, %rdi
	callq	copy_rtx
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	16(%rax), %rdi
	movzbl	2(%rax), %ecx
	leaq	16(%rax), %rsi
	movq	$10, %rdx
	movq	%rbx, (%rsp)
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_part
	xorq	%r13, %r13
	jmp	.LBB2_127
.LBB2_5:                                # %if.end
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	(%rax,%r15,8), %rax
	testq	%rax, %rax
	movq	%rax, 72(%rsp)
	je	.LBB2_28
# BB#6:                                 # %if.then10
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%r15,8)
	jne	.LBB2_8
# BB#7:                                 # %if.then10
	movl	num_not_at_initial_offset(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_35
.LBB2_8:                                # %if.then15
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	make_memloc
	movq	%rax, 72(%rsp)
	movq	8(%rax), %rsi
	movzbl	2(%rax), %edi
	movq	$1, %rdx
	callq	legitimate_address_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB2_13
# BB#9:                                 # %if.then23
	movq	72(%rsp), %rcx
	movzbl	2(%rcx), %edi
	movq	8(%rcx), %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%rsi, %rax
	movq	$4, %r9
	cmpq	$3, %rax
	je	.LBB2_12
# BB#10:                                # %cond.false
	cmpq	$5, %rax
	movq	$6, %r9
	je	.LBB2_12
# BB#11:                                # %cond.false
	movq	56(%rsp), %r9           # 8-byte Reload
.LBB2_12:                               # %cond.end37
	addq	$8, %rcx
	movq	%r13, 8(%rsp)
	xorq	%rsi, %rsi
	movq	%rbx, (%rsp)
	movq	48(%rsp), %r8           # 8-byte Reload
	callq	find_reloads_address
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB2_13:                               # %if.end40
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	jle	.LBB2_21
# BB#14:                                # %land.lhs.true42
	movq	%rsi, %rbx
	movq	72(%rsp), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	legitimate_address_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB2_21
# BB#15:                                # %land.lhs.true45
	movq	72(%rsp), %rdi
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB2_24
# BB#16:                                # %land.lhs.true45
	cmpq	$75, %rcx
	jne	.LBB2_21
# BB#17:                                # %land.lhs.true59
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_21
# BB#18:                                # %land.lhs.true69
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_22
# BB#19:                                # %land.lhs.true69
	addq	$-54, %rcx
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB2_21
# BB#20:                                # %land.lhs.true69
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_24
	jmp	.LBB2_21
.LBB2_28:                               # %if.else
	cmpq	$52, %r15
	jg	.LBB2_35
# BB#29:                                # %land.lhs.true167
	cmpq	$8, %r15
	jl	.LBB2_34
# BB#30:                                # %switch.early.test
	movq	%r15, %rcx
	orq	$4, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$20, %rcx
	je	.LBB2_34
# BB#31:                                # %lor.lhs.false173
	leaq	-37(%r15), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB2_34
# BB#32:                                # %lor.lhs.false177
	movq	reg_renumber(%rip), %rax
	movzbl	(%rax,%r15,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB2_35
# BB#33:                                # %lor.lhs.false177
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rax
	je	.LBB2_35
.LBB2_34:                               # %land.lhs.true194
	movq	this_insn(%rip), %rsi
	xorq	%rcx, %rcx
	xorq	%r13, %r13
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	regno_clobbered_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_35
	jmp	.LBB2_127
.LBB2_46:                               # %if.end258
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	cmpq	$66, %rax
	jne	.LBB2_87
# BB#47:                                # %if.then263
	movq	%r12, 72(%rsp)
	movzbl	2(%r12), %edi
	movq	8(%r12), %rdx
	movq	56(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	$4, %r9
	cmpq	$3, %rax
	movq	144(%rsp), %r14
	je	.LBB2_50
# BB#48:                                # %cond.false276
	cmpq	$5, %rax
	movq	$6, %r9
	je	.LBB2_50
# BB#49:                                # %cond.false276
	movq	56(%rsp), %r9           # 8-byte Reload
.LBB2_50:                               # %cond.end283
	leaq	8(%r12), %rcx
	movq	%r14, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	je	.LBB2_52
# BB#51:                                # %cond.end283
	leaq	-1(%r14), %rax
.LBB2_52:                               # %cond.end283
	movq	%rbx, 8(%rsp)
	leaq	72(%rsp), %rsi
	movq	%rax, (%rsp)
	movq	48(%rsp), %r8           # 8-byte Reload
	callq	find_reloads_address
	movq	72(%rsp), %rax
	cmpq	%r12, %rax
	je	.LBB2_58
# BB#53:                                # %cond.end283
	testq	%r15, %r15
	je	.LBB2_58
# BB#54:                                # %if.then296
	movq	(%r15), %rdi
	callq	copy_rtx
	movq	%rax, (%r15)
	movq	72(%rsp), %rcx
	movq	8(%rax), %rax
	movq	%rcx, 88(%rsp)
	movq	%rax, 80(%rsp)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB2_128
# BB#55:                                # %copy_replacements.exit
	movl	n_replacements(%rip), %edx
	leaq	88(%rsp), %rdi
	leaq	80(%rsp), %rsi
	callq	copy_replacements_1
	movq	(%r15), %rax
	addq	$8, %rax
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB2_57
# BB#56:                                # %if.then305
	movq	(%rax), %rax
	addq	$8, %rax
.LBB2_57:                               # %if.end310
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB2_58:                               # %if.end310
	movq	72(%rsp), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB2_69
# BB#59:                                # %lor.lhs.false313
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	movzbl	indirect_symref_ok(%rip), %ebx
	cmpq	$68, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %r8
	movq	%rdx, %rsi
	jne	.LBB2_61
# BB#60:                                # %lor.lhs.false313
	movq	%r8, %rsi
.LBB2_61:                               # %lor.lhs.false313
	testq	%rbx, %rbx
	jne	.LBB2_63
# BB#62:                                # %lor.lhs.false313
	movq	%r8, %rdx
.LBB2_63:                               # %lor.lhs.false313
	testq	%rsi, %rdx
	jne	.LBB2_69
# BB#64:                                # %lor.lhs.false313
	cmpq	$66, %rcx
	je	.LBB2_69
# BB#65:                                # %lor.lhs.false331
	xorq	%r13, %r13
	cmpq	$61, %rcx
	je	.LBB2_127
# BB#66:                                # %lor.lhs.false331
	cmpq	$75, %rcx
	jne	.LBB2_69
# BB#67:                                # %land.lhs.true347
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB2_69
# BB#68:                                # %land.lhs.true358
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	je	.LBB2_127
.LBB2_69:                               # %if.then369
	movzbl	2(%rdi), %r9d
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$12, %r8
	movq	$0, (%rsp)
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	push_reload
	jmp	.LBB2_125
.LBB2_85:                               # %if.else520
	cmpq	$75, %rax
	jne	.LBB2_86
.LBB2_96:                               # %land.lhs.true525
	movq	16(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB2_86
# BB#97:                                # %land.lhs.true533
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB2_87
# BB#98:                                # %land.lhs.true541
	movq	8(%rax), %rax
	movq	global_rtl+24(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_102
# BB#99:                                # %land.lhs.true541
	movq	global_rtl+32(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_102
# BB#100:                               # %land.lhs.true541
	movq	global_rtl+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_102
# BB#101:                               # %land.lhs.true541
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_87
.LBB2_102:                              # %land.lhs.true577
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	memory_address_p
	movl	(%r12), %r13d
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_87
# BB#103:                               # %if.then580
	movq	8(%r12), %rax
	movq	16(%r12), %rcx
	movq	8(%rax), %rdi
	movq	8(%rcx), %rsi
	movq	$16, %rcx
	shrq	%cl, %r13
	andq	$255, %r13
	callq	plus_constant_wide
	movq	8(%r12), %rcx
	movq	16(%rcx), %rcx
	movq	$75, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%r12, (%rax)
	movq	8(%r12), %rdi
	movzbl	2(%r12), %ecx
	leaq	8(%r12), %rsi
	movq	$12, %rdx
	movq	144(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r8
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r9
	callq	find_reloads_address_part
	movq	16(%r12), %rdi
	leaq	16(%r12), %rdx
	jmp	.LBB2_104
.LBB2_86:
	movq	%rsi, 40(%rsp)          # 8-byte Spill
.LBB2_87:                               # %if.else619
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$75, %r13
	jne	.LBB2_105
# BB#88:                                # %land.lhs.true624
	movq	16(%r12), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB2_105
# BB#89:                                # %land.lhs.true632
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB2_105
# BB#90:                                # %land.lhs.true640
	movq	16(%rax), %rax
	movq	global_rtl+24(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_94
# BB#91:                                # %land.lhs.true640
	movq	global_rtl+32(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_94
# BB#92:                                # %land.lhs.true640
	movq	global_rtl+40(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB2_94
# BB#93:                                # %land.lhs.true640
	movq	global_rtl+16(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_105
.LBB2_94:                               # %land.lhs.true676
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	memory_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_95
.LBB2_105:                              # %if.end722
	movq	%r12, 72(%rsp)
	movzbl	(%r12), %eax
	cmpq	$75, %rax
	jne	.LBB2_112
# BB#106:                               # %if.end729
	movq	%r12, %rdi
	callq	subst_indexed_address
	cmpq	%r12, %rax
	movq	%rax, 72(%rsp)
	je	.LBB2_112
# BB#107:                               # %land.lhs.true732
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_112
# BB#108:                               # %if.then735
	movq	72(%rsp), %rdi
	xorq	%r13, %r13
	movb	%r13b, subst_reg_equivs_changed(%rip)
	movq	%rbx, %rsi
	callq	subst_reg_equivs
	movzbl	subst_reg_equivs_changed(%rip), %ecx
	movq	%rax, 72(%rsp)
	testq	%rcx, %rcx
	je	.LBB2_111
# BB#109:                               # %lor.lhs.false738
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_112
# BB#110:                               # %lor.lhs.false738.if.then741_crit_edge
	movq	72(%rsp), %rax
.LBB2_111:                              # %if.then741
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	jmp	.LBB2_127
.LBB2_112:                              # %if.end743
	movzbl	(%r12), %ecx
	cmpq	$133, %rcx
	jg	.LBB2_116
# BB#113:                               # %if.end743
	addq	$-54, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	movq	144(%rsp), %r13
	ja	.LBB2_115
# BB#114:                               # %if.end743
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB2_118
	jmp	.LBB2_115
.LBB2_116:                              # %if.end743
	cmpq	$134, %rcx
	movq	144(%rsp), %r13
	je	.LBB2_118
# BB#117:                               # %if.end743
	cmpq	$140, %rcx
	jne	.LBB2_115
.LBB2_118:                              # %land.lhs.true783
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_119
.LBB2_115:                              # %if.end816
	xorq	%rsi, %rsi
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	%r13, %r9
	callq	find_reloads_address_1
	jmp	.LBB2_126
.LBB2_119:                              # %if.then786
	testq	%r15, %r15
	je	.LBB2_120
# BB#121:                               # %land.lhs.true788
	movl	(%r12), %eax
	andq	$67174399, %rax         # imm = 0x400FFFF
	cmpq	$67108932, %rax         # imm = 0x4000044
	movq	64(%rsp), %rsi          # 8-byte Reload
	jne	.LBB2_124
# BB#122:                               # %if.then798
	movq	(%r15), %rdi
	callq	copy_rtx
	movq	%rax, %rsi
	movq	%rsi, (%r15)
	addq	$8, %rsi
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB2_124
# BB#123:                               # %if.then804
	movq	(%rsi), %rsi
	addq	$8, %rsi
	jmp	.LBB2_124
.LBB2_120:
	movq	64(%rsp), %rsi          # 8-byte Reload
.LBB2_124:                              # %if.end809
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$12, %rdx
	movq	%r13, (%rsp)
	movq	%r12, %rdi
	movq	%rax, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_part
.LBB2_125:                              # %return
	movq	40(%rsp), %rax          # 8-byte Reload
	xorq	$1, %rax
.LBB2_126:                              # %return
	movq	%rax, %r13
	jmp	.LBB2_127
.LBB2_84:                               # %if.else513
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$12, %rdx
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_part
	xorq	$1, %r14
	movq	%r14, %r13
	jmp	.LBB2_127
.LBB2_95:                               # %if.then679
	movq	8(%r12), %rax
	movq	16(%r12), %rcx
	movq	8(%rax), %r14
	movq	16(%rax), %rdi
	movq	8(%rcx), %rsi
	movzbl	2(%r12), %r15d
	callq	plus_constant_wide
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	%rax, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%r12, (%rax)
	movq	16(%r12), %rdi
	movzbl	2(%r12), %ecx
	leaq	16(%r12), %rsi
	movq	$12, %rdx
	movq	144(%rsp), %rax
	movq	%rax, (%rsp)
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %r8
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r9
	callq	find_reloads_address_part
	movq	8(%r12), %rdi
	leaq	8(%r12), %rdx
.LBB2_104:                              # %return
	movq	%rbx, (%rsp)
	movq	$1, %rsi
	xorq	%r9, %r9
	xorq	%r13, %r13
	movq	%r15, %rcx
	movq	%r14, %r8
	callq	find_reloads_address_1
	jmp	.LBB2_127
.LBB2_22:                               # %land.lhs.true69
	cmpq	$134, %rcx
	je	.LBB2_24
# BB#23:                                # %land.lhs.true69
	cmpq	$140, %rcx
	jne	.LBB2_21
.LBB2_24:                               # %if.then149
	movq	%r13, %r14
	movl	replace_reloads(%rip), %eax
	xorq	%r13, %r13
	testq	%rax, %rax
	je	.LBB2_127
# BB#25:                                # %if.then149
	movl	num_not_at_initial_offset(%rip), %eax
	testq	%rdx, %rax
	je	.LBB2_127
# BB#26:                                # %land.lhs.true153
	movq	reg_equiv_mem(%rip), %rax
	movq	%rdx, %rbx
	movq	(%rax,%r15,8), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	jne	.LBB2_127
# BB#27:                                # %if.then158
	movq	72(%rsp), %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	$48, %rdi
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	$2, %rcx
	movb	%cl, 2(%rax)
	jmp	.LBB2_127
.LBB2_21:                               # %if.end164
	movq	72(%rsp), %r12
.LBB2_35:                               # %if.end199
	movzbl	2(%r12), %r9d
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$12, %r8
	movq	$0, (%rsp)
	movq	%r12, %rdi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	push_reload
	movq	$1, %r13
.LBB2_127:                              # %return
	movq	%r13, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_128:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_replacements, %rcx
	movq	$5935, %rsi             # imm = 0x172F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp38:
	.size	find_reloads_address, .Ltmp38-find_reloads_address
	.cfi_endproc

	.globl	clear_secondary_mem
	.align	16, 0x90
	.type	clear_secondary_mem,@function
clear_secondary_mem:                    # @clear_secondary_mem
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp40:
	.cfi_def_cfa_offset 16
	movabsq	$secondary_memlocs, %rdi
	xorq	%rsi, %rsi
	movq	$472, %rdx              # imm = 0x1D8
	callq	memset
	popq	%rax
	retq
.Ltmp41:
	.size	clear_secondary_mem, .Ltmp41-clear_secondary_mem
	.cfi_endproc

	.globl	push_reload
	.align	16, 0x90
	.type	push_reload,@function
push_reload:                            # @push_reload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp50:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp51:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp52:
	.cfi_def_cfa_offset 48
	subq	$176, %rsp
.Ltmp53:
	.cfi_def_cfa_offset 224
.Ltmp54:
	.cfi_offset %rbx, -48
.Ltmp55:
	.cfi_offset %r12, -40
.Ltmp56:
	.cfi_offset %r13, -32
.Ltmp57:
	.cfi_offset %r14, -24
.Ltmp58:
	.cfi_offset %r15, -16
	movq	%r9, %r12
	movq	%r8, 144(%rsp)          # 8-byte Spill
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB4_3
# BB#1:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB4_3
# BB#2:                                 # %if.then
	movzbl	2(%rdi), %r12d
.LBB4_3:                                # %if.end
	movq	224(%rsp), %r13
	testq	%rax, %r13
	jne	.LBB4_6
# BB#4:                                 # %if.end
	testq	%rbx, %rbx
	je	.LBB4_6
# BB#5:                                 # %if.then5
	movzbl	2(%rbx), %r13d
.LBB4_6:                                # %if.end9
	testq	%rdi, %rdi
	movabsq	$0, %r15
	je	.LBB4_13
# BB#7:                                 # %land.lhs.true11
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_12
# BB#8:                                 # %if.then15
	movslq	8(%rdi), %rax
	cmpq	$53, %rax
	jl	.LBB4_12
# BB#9:                                 # %land.lhs.true17
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB4_12
# BB#10:                                # %land.lhs.true21
	movq	reg_equiv_constant(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB4_12
# BB#11:                                # %land.lhs.true21
	movq	%rax, %rdi
.LBB4_12:                               # %land.lhs.true21
	movq	%rdi, %r15
.LBB4_13:                               # %if.end30
	testq	%rbx, %rbx
	je	.LBB4_14
# BB#15:                                # %land.lhs.true33
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB4_20
# BB#16:                                # %if.then38
	movslq	8(%rbx), %rax
	cmpq	$53, %rax
	jl	.LBB4_20
# BB#17:                                # %land.lhs.true45
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB4_20
# BB#18:                                # %land.lhs.true51
	movq	reg_equiv_constant(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB4_20
# BB#19:                                # %land.lhs.true51
	movq	%rax, %rbx
.LBB4_20:                               # %if.end60
	testq	%r15, %r15
	je	.LBB4_29
# BB#21:                                # %if.end60
	testq	%rbx, %rbx
	je	.LBB4_29
# BB#22:                                # %land.lhs.true66
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	jne	.LBB4_29
# BB#23:                                # %land.lhs.true71
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB4_29
# BB#24:                                # %if.then72
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	addq	$-96, %rcx
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	ja	.LBB4_29
# BB#25:                                # %if.then72
	movq	$1, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$19, %rsi
	je	.LBB4_26
# BB#28:                                # %sw.bb84
	movq	8(%rbx), %rax
	movq	8(%rax), %rsi
	movq	%rbx, %rdi
	callq	replace_equiv_address_nv
	movq	%rax, %rbx
	jmp	.LBB4_29
.LBB4_14:
	xorq	%rbx, %rbx
.LBB4_29:                               # %if.end92
	movq	232(%rsp), %r14
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	testq	%r15, %r15
	je	.LBB4_30
# BB#31:                                # %land.lhs.true95
	movzbl	(%r15), %eax
	cmpq	$63, %rax
	jne	.LBB4_32
# BB#33:                                # %land.lhs.true100
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	movq	%r15, %rdi
	callq	subreg_lowpart_p
	orq	%r14, %rax
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	je	.LBB4_34
# BB#38:                                # %land.lhs.true104
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movq	8(%r15), %rsi
	movl	(%rsi), %r8d
	leaq	8(%r15), %r13
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB4_41
# BB#39:                                # %land.lhs.true104
	leaq	-54(%rax), %rcx
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB4_43
# BB#40:                                # %land.lhs.true104
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB4_74
	jmp	.LBB4_43
.LBB4_30:
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	xorq	%rdi, %rdi
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB4_83
.LBB4_32:
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB4_36
.LBB4_34:
	movq	%r15, %rsi
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	%r12, %r8
	jmp	.LBB4_35
.LBB4_41:                               # %land.lhs.true104
	cmpq	$134, %rax
	je	.LBB4_74
# BB#42:                                # %land.lhs.true104
	cmpq	$140, %rax
	je	.LBB4_74
.LBB4_43:                               # %lor.lhs.false168
	cmpq	$75, %rax
	je	.LBB4_74
# BB#44:                                # %lor.lhs.false168
	testq	%rdi, %r14
	jne	.LBB4_74
# BB#45:                                # %lor.lhs.false178
	cmpq	$61, %rax
	jne	.LBB4_61
# BB#46:                                # %land.lhs.true186
	movl	8(%rsi), %ecx
	cmpq	$52, %rcx
	jbe	.LBB4_49
	jmp	.LBB4_47
.LBB4_26:                               # %if.then72
	shlq	%cl, %rdx
	testq	$44, %rdx
	je	.LBB4_29
# BB#27:                                # %sw.bb
	movq	8(%rax), %rsi
	movq	%r15, %rdi
	callq	replace_equiv_address_nv
	movq	%rax, %r15
	jmp	.LBB4_29
.LBB4_61:                               # %lor.lhs.false195
	cmpq	$66, %rax
	jne	.LBB4_62
.LBB4_47:                               # %land.lhs.true203
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$16, %rcx
	movq	%r8, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB4_74
# BB#48:                                # %lor.lhs.false218
	cmpq	$61, %rax
	jne	.LBB4_62
.LBB4_49:                               # %land.lhs.true226
	movl	8(%rsi), %eax
	cmpq	$52, %rax
	ja	.LBB4_62
# BB#50:                                # %land.lhs.true235
	testq	%rbx, %rbx
	je	.LBB4_52
# BB#51:                                # %lor.lhs.false238
	movq	%r15, %rdi
	callq	subreg_lowpart_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_62
.LBB4_52:                               # %land.lhs.true241
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	target_flags(%rip), %r9d
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	movq	%r9, %r10
	shrq	%cl, %r10
	andq	$4, %r10
	leaq	4(%r10), %r11
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%r11, %rcx
	cmpq	%rcx, %rax
	ja	.LBB4_72
# BB#53:                                # %land.lhs.true248
	movq	(%r13), %rsi
	movl	(%rsi), %r8d
	movq	$16, %rcx
	movq	%r8, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	movq	%r11, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rax, %rcx
	jbe	.LBB4_72
# BB#54:                                # %land.lhs.true263
	xorq	%rdx, %rdx
	movq	%rcx, %rax
	idivq	%r11
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movl	8(%rsi), %eax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	je	.LBB4_58
# BB#55:                                # %land.lhs.true263
	leaq	-21(%rax), %rdx
	andq	%r14, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_58
# BB#56:                                # %land.lhs.true263
	leaq	-45(%rax), %rdx
	andq	%r14, %rdx
	cmpq	$8, %rdx
	jb	.LBB4_58
# BB#57:                                # %land.lhs.true263
	addq	$-29, %rax
	andq	%r14, %rax
	cmpq	$7, %rax
	jbe	.LBB4_58
# BB#65:                                # %cond.false
	cmpq	$24, %rdi
	jne	.LBB4_66
# BB#68:                                # %cond.true389
	movq	$24, %rcx
	shrq	%cl, %r9
	andq	$2, %r9
	xorq	$6, %r9
	jmp	.LBB4_70
.LBB4_58:                               # %cond.true
	movl	mode_class(,%rdi,4), %eax
	cmpq	$5, %rax
	movq	$2, %r9
	movq	%r14, %rcx
	je	.LBB4_71
# BB#59:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB4_60
# BB#63:                                # %lor.rhs
	movabsq	$1, %r9
	incq	%r9
	jmp	.LBB4_71
.LBB4_66:                               # %cond.false
	cmpq	$18, %rdi
	jne	.LBB4_69
# BB#67:                                # %cond.true376
	movq	$25, %rcx
	shrq	%cl, %r9
	andq	$1, %r9
	xorq	$3, %r9
	jmp	.LBB4_70
.LBB4_60:
	movabsq	$0, %r9
	incq	%r9
	jmp	.LBB4_71
.LBB4_69:                               # %cond.false393
	orq	$3, %r10
	addq	%rcx, %r10
	movq	%r10, %rax
	cqto
	idivq	%r11
	movq	%rax, %r9
.LBB4_70:                               # %cond.end413
	movq	%r14, %rcx
.LBB4_71:                               # %cond.end413
	andq	%rcx, %r9
	movq	168(%rsp), %rax         # 8-byte Reload
	andq	%rcx, %rax
	cmpq	%r9, %rax
	movq	232(%rsp), %r14
	jne	.LBB4_74
.LBB4_72:                               # %lor.lhs.false417
	movq	%r15, %rdi
	callq	subreg_regno
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	ix86_hard_regno_mode_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_73
.LBB4_62:
	movq	%r15, %rsi
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	%r12, %r8
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB4_35
.LBB4_73:                               # %lor.lhs.false417.if.then421_crit_edge
	movq	(%r13), %rsi
	movl	(%rsi), %r8d
.LBB4_74:                               # %if.then421
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	cmpq	$66, %rax
	jne	.LBB4_75
# BB#76:                                # %if.then429
	movzbl	mode_size(%r8), %eax
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_size(%r12), %ecx
	cmpq	%rcx, %rax
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r13, 112(%rsp)         # 8-byte Spill
	movq	128(%rsp), %r13         # 8-byte Reload
	jbe	.LBB4_35
# BB#77:                                # %if.then441
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_reload, %rcx
	movq	$1035, %rsi             # imm = 0x40B
	jmp	.LBB4_78
.LBB4_75:
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r13, 112(%rsp)         # 8-byte Spill
	movq	128(%rsp), %r13         # 8-byte Reload
.LBB4_35:                               # %if.end447
	testq	%rsi, %rsi
	movq	%r8, %r12
	movq	%rsi, %r15
	movabsq	$0, %rdi
	je	.LBB4_83
.LBB4_36:                               # %land.lhs.true450
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	reload_inner_reg_of_subreg
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_37
# BB#79:                                # %if.then453
	movq	%r13, %r14
	movq	%r12, %r13
	movq	8(%r15), %rdi
	movl	(%rdi), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB4_80
# BB#81:                                # %if.then461
	movq	$16, %rcx
	movzbl	2(%r15), %eax
	movl	16(%r15), %edx
	movl	8(%rdi), %edi
	shrq	%cl, %rsi
	andq	$255, %rsi
	leaq	8(%r15), %r12
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movq	8(%r15), %rcx
	movl	8(%rcx), %edx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	find_valid_class
	movq	8(%r15), %rdi
	movq	%rax, %r8
	jmp	.LBB4_82
.LBB4_37:
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB4_83
.LBB4_80:                               # %if.then453.if.end488_crit_edge
	leaq	8(%r15), %r12
	movq	144(%rsp), %r8          # 8-byte Reload
.LBB4_82:                               # %if.end488
	movq	248(%rsp), %rax
	movq	256(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdx
	callq	push_reload
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	%r13, %r12
	movq	%r14, %r13
	movq	232(%rsp), %r14
.LBB4_83:                               # %if.end496
	testq	%rbx, %rbx
	je	.LBB4_84
# BB#85:                                # %land.lhs.true499
	movzbl	(%rbx), %eax
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	cmpq	$63, %rax
	jne	.LBB4_86
# BB#91:                                # %land.lhs.true504
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	subreg_lowpart_p
	orq	%r14, %rax
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	je	.LBB4_92
# BB#93:                                # %land.lhs.true509
	movq	%r14, %rdi
	movq	%r13, %r15
	movq	8(%rbx), %rax
	movl	(%rax), %r8d
	leaq	8(%rbx), %r13
	movq	%rbx, 152(%rsp)         # 8-byte Spill
	movq	%r8, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$133, %rdx
	jg	.LBB4_100
# BB#94:                                # %land.lhs.true509
	leaq	-54(%rdx), %rcx
	andq	%rsi, %rcx
	movq	%rsi, %r14
	cmpq	$14, %rcx
	ja	.LBB4_101
# BB#95:                                # %land.lhs.true509
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB4_96
	jmp	.LBB4_101
.LBB4_84:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	160(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_145
.LBB4_86:
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	jmp	.LBB4_87
.LBB4_92:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%r13, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_98
.LBB4_100:                              # %land.lhs.true509
	movq	%rsi, %r14
	cmpq	$134, %rdx
	je	.LBB4_96
.LBB4_101:                              # %lor.lhs.false565
	cmpq	$140, %rdx
	je	.LBB4_96
# BB#102:                               # %lor.lhs.false565
	testq	%r14, %rdi
	jne	.LBB4_96
# BB#103:                               # %lor.lhs.false575
	cmpq	$61, %rdx
	jne	.LBB4_105
# BB#104:                               # %land.lhs.true583
	movl	8(%rax), %ecx
	cmpq	$52, %rcx
	ja	.LBB4_106
	jmp	.LBB4_108
.LBB4_105:                              # %lor.lhs.false592
	cmpq	$66, %rdx
	jne	.LBB4_127
.LBB4_106:                              # %land.lhs.true600
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_size(%rsi), %esi
	movq	$16, %rcx
	movq	%r8, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %rsi
	ja	.LBB4_96
# BB#107:                               # %lor.lhs.false615
	cmpq	$61, %rdx
	jne	.LBB4_127
.LBB4_108:                              # %land.lhs.true623
	movl	8(%rax), %r11d
	cmpq	$52, %r11
	ja	.LBB4_127
# BB#109:                               # %land.lhs.true632
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	target_flags(%rip), %r10d
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	movq	%r10, %rsi
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	4(%rsi), %rdi
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rdi, %rcx
	cmpq	%rcx, %rax
	ja	.LBB4_126
# BB#110:                               # %land.lhs.true641
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movzbl	mode_size(%r8), %r9d
	movq	%rdi, %rax
	andq	%r14, %rax
	cmpq	%rax, %r9
	jbe	.LBB4_126
# BB#111:                               # %land.lhs.true656
	xorq	%rdx, %rdx
	movq	%r9, %rax
	idivq	%rdi
	movq	%rax, %rbx
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r11, %rax
	cmpq	$8, %rax
	je	.LBB4_115
# BB#112:                               # %lor.lhs.false687
	cmpq	$52, %r11
	ja	.LBB4_114
# BB#113:                               # %lor.lhs.false687
	movq	$1, %rax
	movq	%r11, %rcx
	shlq	%cl, %rax
	movabsq	$8972015417425920, %rcx # imm = 0x1FE0001FE00000
	testq	%rcx, %rax
	jne	.LBB4_115
.LBB4_114:                              # %lor.lhs.false723
	addq	$-29, %r11
	andq	%r14, %r11
	cmpq	$7, %r11
	ja	.LBB4_120
.LBB4_115:                              # %cond.true741
	movl	mode_class(,%r8,4), %eax
	cmpq	$5, %rax
	movq	$2, %r10
	je	.LBB4_125
# BB#116:                               # %lor.rhs752
	cmpq	$6, %rax
	jne	.LBB4_117
# BB#118:                               # %lor.rhs752
	movabsq	$1, %r10
	incq	%r10
	jmp	.LBB4_125
.LBB4_117:
	movabsq	$0, %r10
	incq	%r10
	jmp	.LBB4_125
.LBB4_120:                              # %cond.false765
	cmpq	$24, %r8
	jne	.LBB4_121
# BB#123:                               # %cond.true787
	movq	$24, %rcx
	shrq	%cl, %r10
	andq	$2, %r10
	xorq	$6, %r10
	jmp	.LBB4_125
.LBB4_121:                              # %cond.false765
	cmpq	$18, %r8
	jne	.LBB4_124
# BB#122:                               # %cond.true774
	movq	$25, %rcx
	shrq	%cl, %r10
	andq	$1, %r10
	xorq	$3, %r10
	jmp	.LBB4_125
.LBB4_124:                              # %cond.false791
	orq	$3, %rsi
	addq	%r9, %rsi
	movq	%rsi, %rax
	cqto
	idivq	%rdi
	movq	%rax, %r10
.LBB4_125:                              # %cond.end814
	andq	%r14, %r10
	andq	%r14, %rbx
	cmpq	%r10, %rbx
	jne	.LBB4_96
.LBB4_126:                              # %lor.lhs.false818
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	subreg_regno
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r14, %rax
	je	.LBB4_96
.LBB4_127:                              # %lor.lhs.false822
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	-10(%rax), %rax
	movq	%r14, %rsi
	andq	%rsi, %rax
	cmpq	$2, %rax
	ja	.LBB4_128
# BB#129:                               # %land.lhs.true831
	movl	target_flags(%rip), %eax
	testq	$33554432, %rax         # imm = 0x2000000
	movq	152(%rsp), %rbx         # 8-byte Reload
	movq	104(%rsp), %rcx         # 8-byte Reload
	jne	.LBB4_130
# BB#131:                               # %land.lhs.true831
	movq	%r15, %rax
	andq	%rsi, %rax
	cmpq	$2, %rax
	jne	.LBB4_130
# BB#132:                               # %land.rhs851
	movq	%rsi, %r14
	movq	(%r13), %rax
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	movq	$2, %rax
	jne	.LBB4_96
# BB#133:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rsi
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_98
.LBB4_96:                               # %if.then864
	movq	(%r13), %rbx
	movl	(%rbx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$66, %rdx
	jne	.LBB4_97
# BB#134:                               # %land.lhs.true872
	movzbl	mode_size(%rax), %edx
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movzbl	mode_size(%r15), %ecx
	cmpq	%rcx, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%r13, %rcx
	movq	%r14, %rsi
	jbe	.LBB4_98
# BB#135:                               # %if.then884
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_reload, %rcx
	movq	$1135, %rsi             # imm = 0x46F
	jmp	.LBB4_78
.LBB4_97:
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%r13, %rcx
	movq	%r14, %rsi
	jmp	.LBB4_98
.LBB4_128:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rax
	movq	152(%rsp), %rbx         # 8-byte Reload
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_98
.LBB4_130:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rax
.LBB4_98:                               # %if.end889
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rax, %r13
	testq	%rbx, %rbx
	je	.LBB4_99
.LBB4_87:                               # %land.lhs.true892
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	reload_inner_reg_of_subreg
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_89
# BB#88:                                # %if.then895
	movq	8(%rbx), %r15
	movzbl	2(%rbx), %ecx
	movl	16(%rbx), %edx
	movzbl	2(%r15), %esi
	movl	8(%r15), %edi
	movq	%r12, %r14
	leaq	8(%rbx), %r12
	callq	subreg_regno_offset
	movq	8(%rbx), %rcx
	movl	8(%rcx), %edx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	find_valid_class
	movq	248(%rsp), %rcx
	movq	%rcx, 24(%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r15, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r12, %rcx
	movq	%r14, %r12
	movq	%rax, %r8
	callq	push_reload
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
.LBB4_89:                               # %if.end935
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	movq	$1, %r15
	movq	168(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB4_90
# BB#136:                               # %land.lhs.true941
	movq	%rbx, %rdx
	movzbl	(%rdx), %eax
	cmpq	$66, %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	jne	.LBB4_137
# BB#138:                               # %land.lhs.true946
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	je	.LBB4_141
# BB#139:                               # %land.lhs.true946
	cmpq	$66, %rax
	jne	.LBB4_140
.LBB4_141:                              # %land.lhs.true956
	movq	%rcx, %r14
	movq	8(%rdx), %rsi
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movq	%rdi, %rbx
	callq	reg_overlap_mentioned_for_reload_p
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	je	.LBB4_142
# BB#143:                               # %land.lhs.true956
	movabsq	$1, %rax
	jmp	.LBB4_144
.LBB4_90:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rbx, %rdx
	movq	160(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_145
.LBB4_137:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB4_145
.LBB4_99:
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	160(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB4_145
.LBB4_142:
	movabsq	$0, %rax
.LBB4_144:                              # %land.lhs.true956
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r14, %rcx
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rdi
	jmp	.LBB4_145
.LBB4_140:
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB4_145:                              # %if.end963
	testq	%rcx, %rcx
	jne	.LBB4_151
# BB#146:                               # %land.lhs.true966
	movl	(%rdi), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB4_151
# BB#147:                               # %land.lhs.true971
	movq	8(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_151
# BB#148:                               # %land.lhs.true979
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB4_151
# BB#149:                               # %land.lhs.true979
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	jne	.LBB4_151
# BB#150:                               # %if.then990
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%rsi, %r14
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movq	%r12, %r13
	movq	%rdx, %r12
	callq	subreg_regno
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%r12, %rdx
	movq	%r13, %r12
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdi
.LBB4_151:                              # %if.end996
	testq	%r15, %r15
	je	.LBB4_152
# BB#153:                               # %land.lhs.true999
	movl	(%rdx), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB4_154
# BB#155:                               # %land.lhs.true1004
	movq	%rdi, %r15
	movq	%rsi, %r14
	movq	8(%rdx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_159
# BB#156:                               # %land.lhs.true1012
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB4_159
# BB#157:                               # %land.lhs.true1012
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	jne	.LBB4_159
# BB#158:                               # %if.then1023
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movq	%rdx, %rdi
	callq	subreg_regno
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	%rax, %rdx
.LBB4_159:                              # %if.end1029
	movq	%r15, %rdi
	jmp	.LBB4_160
.LBB4_152:
	movq	%rsi, %r14
	jmp	.LBB4_160
.LBB4_154:
	movq	%rsi, %r14
.LBB4_160:                              # %if.end1029
	movq	%rdx, %rbx
	testq	%rdi, %rdi
	je	.LBB4_161
# BB#162:                               # %if.then1032
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	ix86_preferred_reload_class
	jmp	.LBB4_163
.LBB4_161:
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movq	144(%rsp), %rax         # 8-byte Reload
.LBB4_163:                              # %if.end1034
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	movq	%r14, %r9
	je	.LBB4_167
# BB#164:                               # %if.then1036
	movq	%r12, %rcx
	andq	%r9, %rcx
	cmpq	$2, %rcx
	movq	%rbx, %r10
	jne	.LBB4_174
# BB#165:                               # %land.lhs.true1039
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	jne	.LBB4_174
# BB#166:                               # %land.lhs.true1042
	movq	%rax, %rcx
	andq	%r9, %rcx
.LBB4_172:                              # %land.lhs.true1077
	cmpq	$24, %rcx
	ja	.LBB4_174
# BB#173:                               # %land.lhs.true1077
	movq	$8, %rdi
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$16784384, %rdx         # imm = 0x1001C00
	jne	.LBB4_175
	jmp	.LBB4_174
.LBB4_167:                              # %if.else
	movq	168(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	movq	%rbx, %r10
	je	.LBB4_174
# BB#168:                               # %land.lhs.true1060
	movzbl	(%rcx), %edx
	cmpq	$63, %rdx
	jne	.LBB4_174
# BB#169:                               # %if.then1065
	movq	8(%rcx), %rcx
	movl	(%rcx), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$131072, %rcx           # imm = 0x20000
	jne	.LBB4_174
# BB#170:                               # %land.lhs.true1074
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	je	.LBB4_171
.LBB4_174:                              # %cond.false1090
	movq	%rax, %rdi
.LBB4_175:                              # %if.end1094
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB4_180
# BB#176:                               # %if.then1096
	movq	%r13, %rcx
	andq	%r9, %rcx
	cmpq	$2, %rcx
	jne	.LBB4_180
# BB#177:                               # %land.lhs.true1099
	movzbl	target_flags+3(%rip), %ecx
	testq	$2, %rcx
	jne	.LBB4_180
# BB#178:                               # %land.lhs.true1102
	movq	%rdi, %rcx
	andq	%r9, %rcx
	cmpq	$24, %rcx
	ja	.LBB4_180
# BB#179:                               # %land.lhs.true1102
	movq	$8, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$16784384, %rsi         # imm = 0x1001C00
	jne	.LBB4_181
.LBB4_180:                              # %cond.false1115
	movq	%rdi, %rdx
.LBB4_181:                              # %if.end1118
	testq	%r10, %r10
	je	.LBB4_187
# BB#182:                               # %land.lhs.true1121
	movzbl	(%r10), %eax
	cmpq	$63, %rax
	jne	.LBB4_187
# BB#183:                               # %if.then1126
	movq	8(%r10), %rax
	movl	(%rax), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$131072, %rax           # imm = 0x20000
	jne	.LBB4_187
# BB#184:                               # %land.lhs.true1135
	movzbl	target_flags+3(%rip), %eax
	testq	$2, %rax
	jne	.LBB4_187
# BB#185:                               # %land.lhs.true1138
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$24, %rcx
	ja	.LBB4_187
# BB#186:                               # %land.lhs.true1138
	movq	$8, %rsi
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$16784384, %rax         # imm = 0x1001C00
	jne	.LBB4_188
.LBB4_187:                              # %cond.false1151
	movq	%rdx, %rsi
.LBB4_188:                              # %if.end1154
	movl	this_insn_is_asm(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_232
# BB#189:                               # %if.then1156
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	movq	%r12, 136(%rsp)         # 8-byte Spill
	ja	.LBB4_191
# BB#190:                               # %select.mid
	movq	%r13, %r12
.LBB4_191:                              # %select.end
	movq	%r10, %rbx
	testq	%r9, %r12
	je	.LBB4_193
# BB#192:
	movq	%r13, 128(%rsp)         # 8-byte Spill
	jmp	.LBB4_198
.LBB4_193:                              # %if.then1170
	movq	%r9, %r14
	movq	this_insn(%rip), %rdi
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	error_for_asm
	movl	word_mode(%rip), %r12d
	movq	168(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB4_195
# BB#194:                               # %if.then1170
	movq	%r12, 136(%rsp)         # 8-byte Spill
.LBB4_195:                              # %if.then1170
	testq	%rbx, %rbx
	movq	%r12, %rax
	movq	%r14, %r9
	jne	.LBB4_197
# BB#196:                               # %if.then1170
	movq	%r13, %rax
.LBB4_197:                              # %if.then1170
	movq	%rax, 128(%rsp)         # 8-byte Spill
.LBB4_198:                              # %if.end1179
	movq	%rbx, 152(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	160(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r12, %rbx
	andq	%r9, %rbx
	xorq	%r13, %r13
	cmpq	$18, %rbx
	jne	.LBB4_213
# BB#199:
	movabsq	$4294967288, %r14       # imm = 0xFFFFFFF8
	xorq	%r15, %r15
	.align	16, 0x90
.LBB4_200:                              # %for.body.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_208 Depth 2
	movq	$18, %rsi
	movq	%r13, %rdi
	movq	%r9, %rbx
	callq	ix86_hard_regno_mode_ok
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB4_210
# BB#201:                               # %land.lhs.true1184.us
                                        #   in Loop: Header=BB4_200 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB4_210
# BB#202:                               # %if.then1189.us
                                        #   in Loop: Header=BB4_200 Depth=1
	movq	%r13, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	je	.LBB4_206
# BB#203:                               # %if.then1189.us
                                        #   in Loop: Header=BB4_200 Depth=1
	leaq	-21(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_206
# BB#204:                               # %if.then1189.us
                                        #   in Loop: Header=BB4_200 Depth=1
	leaq	-45(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	jb	.LBB4_206
# BB#205:                               # %if.then1189.us
                                        #   in Loop: Header=BB4_200 Depth=1
	leaq	-29(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB4_206
# BB#211:                               # %cond.end1256.us
                                        #   in Loop: Header=BB4_200 Depth=1
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	xorq	$3, %rdx
	jmp	.LBB4_212
.LBB4_206:                              # %cond.true1213.us
                                        #   in Loop: Header=BB4_200 Depth=1
	movq	96(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	addq	$-5, %rcx
	andq	%r9, %rcx
	cmpq	$2, %rcx
	movq	$2, %rdx
	movq	%r13, %rcx
	jae	.LBB4_230
.LBB4_212:                              # %for.body1261.lr.ph.us
                                        #   in Loop: Header=BB4_200 Depth=1
	movq	$1, %rsi
	.align	16, 0x90
.LBB4_208:                              # %for.body1261.us
                                        #   Parent Loop BB4_200 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	(%r13,%rsi), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %rax
	je	.LBB4_209
# BB#207:                               # %for.cond1258.us
                                        #   in Loop: Header=BB4_208 Depth=2
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdx, %rdi
	jl	.LBB4_208
.LBB4_209:                              # %for.end.us
                                        #   in Loop: Header=BB4_200 Depth=1
	andq	%r9, %rdx
	andq	%r9, %rsi
	cmpq	%rdx, %rsi
	movq	%r13, %rcx
	je	.LBB4_230
.LBB4_210:                              # %for.inc1276.us
                                        #   in Loop: Header=BB4_200 Depth=1
	incq	%r13
	incq	%r15
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movq	%r15, %rcx
	jl	.LBB4_200
	jmp	.LBB4_230
.LBB4_213:
	xorq	%r14, %r14
	.align	16, 0x90
.LBB4_214:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_227 Depth 2
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r9, %r15
	callq	ix86_hard_regno_mode_ok
	movq	%r15, %r9
	testq	%r9, %rax
	je	.LBB4_229
# BB#215:                               # %land.lhs.true1184
                                        #   in Loop: Header=BB4_214 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %r8
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%rax, %r8
	je	.LBB4_229
# BB#216:                               # %if.then1189
                                        #   in Loop: Header=BB4_214 Depth=1
	movq	%r13, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB4_220
# BB#217:                               # %if.then1189
                                        #   in Loop: Header=BB4_214 Depth=1
	leaq	-21(%r13), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB4_220
# BB#218:                               # %if.then1189
                                        #   in Loop: Header=BB4_214 Depth=1
	leaq	-45(%r13), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	jb	.LBB4_220
# BB#219:                               # %if.then1189
                                        #   in Loop: Header=BB4_214 Depth=1
	leaq	-29(%r13), %rax
	andq	%r9, %rax
	cmpq	$7, %rax
	jbe	.LBB4_220
# BB#221:                               # %cond.false1225
                                        #   in Loop: Header=BB4_214 Depth=1
	cmpq	$24, %rbx
	jne	.LBB4_223
# BB#222:                               # %cond.true1235
                                        #   in Loop: Header=BB4_214 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB4_224
.LBB4_220:                              # %cond.true1213
                                        #   in Loop: Header=BB4_214 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	addq	$-5, %rax
	andq	%r9, %rax
	cmpq	$2, %rax
	movq	$2, %rax
	movq	%r13, %rcx
	jb	.LBB4_225
	jmp	.LBB4_230
.LBB4_223:                              # %cond.false1239
                                        #   in Loop: Header=BB4_214 Depth=1
	movl	target_flags(%rip), %edi
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB4_224:                              # %cond.end1256
                                        #   in Loop: Header=BB4_214 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rdx
	cmpq	$2, %rdi
	jl	.LBB4_228
.LBB4_225:                              # %for.body1261.lr.ph
                                        #   in Loop: Header=BB4_214 Depth=1
	movq	$1, %rdx
	.align	16, 0x90
.LBB4_227:                              # %for.body1261
                                        #   Parent Loop BB4_214 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	(%r13,%rdx), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %r8
	je	.LBB4_228
# BB#226:                               # %for.cond1258
                                        #   in Loop: Header=BB4_227 Depth=2
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	jl	.LBB4_227
.LBB4_228:                              # %for.end
                                        #   in Loop: Header=BB4_214 Depth=1
	andq	%r9, %rax
	andq	%r9, %rdx
	cmpq	%rax, %rdx
	movq	%r13, %rcx
	je	.LBB4_230
.LBB4_229:                              # %for.inc1276
                                        #   in Loop: Header=BB4_214 Depth=1
	incq	%r13
	incq	%r14
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	movq	%r14, %rcx
	jl	.LBB4_214
.LBB4_230:                              # %for.end1278
	andq	%r9, %rcx
	cmpq	$53, %rcx
	jne	.LBB4_231
# BB#237:                               # %if.end1283.thread
	movq	%r9, %rbx
	movq	this_insn(%rip), %rdi
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	callq	error_for_asm
	movq	$24, 160(%rsp)          # 8-byte Folded Spill
	movq	152(%rsp), %r10         # 8-byte Reload
	movq	136(%rsp), %r12         # 8-byte Reload
	movq	128(%rsp), %r13         # 8-byte Reload
	jmp	.LBB4_238
.LBB4_231:
	movq	152(%rsp), %r10         # 8-byte Reload
	movq	136(%rsp), %r12         # 8-byte Reload
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	160(%rsp), %rsi         # 8-byte Reload
.LBB4_232:                              # %if.end1283
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB4_234
# BB#233:
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	movq	%r9, %rbx
	jmp	.LBB4_238
.LBB4_234:                              # %land.lhs.true1286
	movq	%r9, %rbx
	movq	240(%rsp), %rcx
	testq	%rax, %rcx
	je	.LBB4_236
# BB#235:                               # %land.lhs.true1286
	movq	$0, 160(%rsp)           # 8-byte Folded Spill
	movq	256(%rsp), %rcx
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB4_236
.LBB4_238:                              # %if.end1293
	movslq	n_earlyclobbers(%rip), %rax
	testq	%rax, %rax
	movq	%rbx, %r8
	jle	.LBB4_242
# BB#239:
	movq	$1, %rdx
	.align	16, 0x90
.LBB4_240:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_earlyclobbers-8(,%rdx,8), %rcx
	cmpq	%r10, %rcx
	je	.LBB4_244
# BB#241:                               # %for.cond.i.i
                                        #   in Loop: Header=BB4_240 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB4_240
.LBB4_242:                              # %for.cond.preheader.i
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movslq	n_reloads(%rip), %r12
	testq	%r12, %r12
	jle	.LBB4_243
# BB#247:                               # %for.body.lr.ph.i
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movq	$32, %rcx
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB4_248:                              # %for.body.i2891
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_296 Depth 2
	imulq	$104, %r13, %r14
	movl	rld+16(%r14), %esi
	movq	%rbx, %rdi
	callq	reg_class_subset_p
	movq	%r15, %r8
	testq	%r8, %rax
	jne	.LBB4_250
# BB#249:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB4_248 Depth=1
	leaq	rld+16(%r14), %rax
	movl	(%rax), %edi
	movq	%rbx, %rsi
	movq	%rbx, %r15
	movq	%r8, %rbx
	callq	reg_class_subset_p
	movq	%rbx, %r8
	movq	%r15, %rbx
	testq	%r8, %rax
	je	.LBB4_298
.LBB4_250:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	rld+64(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB4_252
# BB#251:                               # %lor.lhs.false12.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %r15
	movq	%rbx, %r12
	movq	%r8, %rbx
	callq	true_regnum
	movq	%rbx, %r8
	movq	%r12, %rbx
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%r15, %rdx
	je	.LBB4_298
.LBB4_252:                              # %land.lhs.true20.i
                                        #   in Loop: Header=BB4_248 Depth=1
	leaq	rld(%r14), %r15
	cmpq	$0, 168(%rsp)           # 8-byte Folded Reload
	je	.LBB4_269
# BB#253:                               # %land.lhs.true22.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r15), %rdi
	movq	168(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB4_258
# BB#254:                               # %lor.lhs.false27.i
                                        #   in Loop: Header=BB4_248 Depth=1
	testq	%rdi, %rdi
	je	.LBB4_269
# BB#255:                               # %land.lhs.true32.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	%rbx, %r12
	movq	%r8, %rbx
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_259
# BB#256:                               # %cond.true.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%rbx, %r8
	movq	%r12, %rbx
	jne	.LBB4_269
# BB#257:                               # %land.lhs.true40.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	8(%rdi), %eax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB4_269
.LBB4_258:                              # %land.lhs.true60.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$0, 120(%rsp)           # 8-byte Folded Reload
	je	.LBB4_261
	jmp	.LBB4_269
.LBB4_259:                              # %cond.false.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	%r12, %rbx
	je	.LBB4_269
# BB#260:                               # %land.lhs.true54.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r15), %rdi
	movq	%rbx, %r12
	movq	%r8, %rbx
	callq	side_effects_p
	movq	%rbx, %r8
	movq	%r12, %rbx
	movq	120(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	testq	%r8, %rax
	jne	.LBB4_269
.LBB4_261:                              # %land.lhs.true62.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	152(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	leaq	rld+8(%r14), %r12
	je	.LBB4_285
# BB#262:                               # %lor.lhs.false64.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB4_285
# BB#263:                               # %lor.lhs.false64.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	%rsi, %rdi
	je	.LBB4_285
# BB#264:                               # %land.lhs.true79.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	%r8, %rbx
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_267
# BB#265:                               # %cond.true86.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	movq	%rbx, %r8
	movq	160(%rsp), %rbx         # 8-byte Reload
	jne	.LBB4_269
# BB#266:                               # %land.lhs.true90.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	8(%rdi), %eax
	movq	152(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB4_269
	jmp	.LBB4_285
.LBB4_267:                              # %cond.false101.i
                                        #   in Loop: Header=BB4_248 Depth=1
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	je	.LBB4_269
# BB#268:                               # %land.lhs.true107.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r12), %rdi
	movq	%r8, %rbx
	callq	side_effects_p
	movq	%rbx, %r8
	movq	160(%rsp), %rbx         # 8-byte Reload
	testq	%r8, %rax
	je	.LBB4_285
	.align	16, 0x90
.LBB4_269:                              # %lor.lhs.false113.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	je	.LBB4_298
# BB#270:                               # %land.lhs.true115.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	rld+8(%r14), %rdi
	leaq	rld+8(%r14), %r12
	movq	152(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB4_277
# BB#271:                               # %lor.lhs.false120.i
                                        #   in Loop: Header=BB4_248 Depth=1
	testq	%rdi, %rdi
	je	.LBB4_298
# BB#272:                               # %land.lhs.true125.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	%r8, %rbx
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_275
# BB#273:                               # %cond.true132.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%rbx, %r8
	movq	160(%rsp), %rbx         # 8-byte Reload
	jne	.LBB4_298
# BB#274:                               # %land.lhs.true136.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	8(%rdi), %eax
	movq	152(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB4_277
	jmp	.LBB4_298
.LBB4_275:                              # %cond.false147.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	je	.LBB4_298
# BB#276:                               # %land.lhs.true153.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r12), %rdi
	movq	%r8, %rbx
	callq	side_effects_p
	movq	%rbx, %r8
	movq	160(%rsp), %rbx         # 8-byte Reload
	testq	%r8, %rax
	jne	.LBB4_298
.LBB4_277:                              # %land.lhs.true159.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	168(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB4_285
# BB#278:                               # %lor.lhs.false161.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB4_285
# BB#279:                               # %lor.lhs.false161.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	%rsi, %rdi
	je	.LBB4_285
# BB#280:                               # %land.lhs.true176.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	%r8, %rbx
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_283
# BB#281:                               # %cond.true183.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	movq	%rbx, %r8
	movq	160(%rsp), %rbx         # 8-byte Reload
	jne	.LBB4_298
# BB#282:                               # %land.lhs.true187.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	8(%rdi), %eax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB4_285
	jmp	.LBB4_298
.LBB4_283:                              # %cond.false198.i
                                        #   in Loop: Header=BB4_248 Depth=1
	callq	rtx_equal_p
	movq	%rbx, %r8
	testq	%r8, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	je	.LBB4_298
# BB#284:                               # %land.lhs.true204.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r15), %rdi
	movq	%rbx, %r15
	movq	%r8, %rbx
	callq	side_effects_p
	movq	%rbx, %r8
	movq	%r15, %rbx
	testq	%r8, %rax
	jne	.LBB4_298
.LBB4_285:                              # %land.lhs.true210.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	(%r12), %rax
	testq	%rax, %rax
	je	.LBB4_287
# BB#286:                               # %lor.lhs.false215.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movslq	n_earlyclobbers(%rip), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB4_287
	.align	16, 0x90
.LBB4_296:                              # %for.body.i691.i
                                        #   Parent Loop BB4_248 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reload_earlyclobbers-8(,%rsi,8), %rcx
	cmpq	%rax, %rcx
	je	.LBB4_298
# BB#295:                               # %for.cond.i686.i
                                        #   in Loop: Header=BB4_296 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB4_296
.LBB4_287:                              # %land.lhs.true221.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	256(%rsp), %rcx
	andq	%r8, %rcx
	cmpq	$9, %rcx
	je	.LBB4_342
# BB#288:                               # %lor.lhs.false228.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	rld+92(%r14), %eax
	cmpq	$9, %rax
	je	.LBB4_342
# BB#289:                               # %lor.lhs.false232.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB4_291
# BB#290:                               # %land.lhs.true237.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movl	rld+72(%r14), %edx
	movq	248(%rsp), %rsi
	andq	%r8, %rsi
	cmpq	%rsi, %rdx
	je	.LBB4_342
.LBB4_291:                              # %lor.lhs.false242.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$10, %rcx
	je	.LBB4_297
# BB#292:                               # %lor.lhs.false242.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$7, %rcx
	jne	.LBB4_293
# BB#341:                               # %land.lhs.true251.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$7, %rax
	jne	.LBB4_298
	jmp	.LBB4_342
.LBB4_297:                              # %land.lhs.true258.i
                                        #   in Loop: Header=BB4_248 Depth=1
	cmpq	$10, %rax
	jne	.LBB4_298
	jmp	.LBB4_342
.LBB4_293:                              # %lor.lhs.false242.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	256(%rsp), %rcx
	testq	%r8, %rcx
	jne	.LBB4_298
# BB#294:                               # %land.lhs.true244.i
                                        #   in Loop: Header=BB4_248 Depth=1
	testq	%rax, %rax
	je	.LBB4_342
	.align	16, 0x90
.LBB4_298:                              # %for.inc.i
                                        #   in Loop: Header=BB4_248 Depth=1
	movq	%r8, %r15
	incq	%r13
	movslq	n_reloads(%rip), %r12
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rax
	jl	.LBB4_248
# BB#245:                               # %for.cond265.preheader.i
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	movq	152(%rsp), %r10         # 8-byte Reload
	jle	.LBB4_246
# BB#299:                               # %for.body267.lr.ph.i
	movq	%r10, %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB4_300:                              # %for.body267.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_334 Depth 2
	imulq	$104, %rbx, %r12
	movl	rld+16(%r12), %esi
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r15, %rax
	jne	.LBB4_302
# BB#301:                               # %lor.lhs.false273.i
                                        #   in Loop: Header=BB4_300 Depth=1
	leaq	rld+16(%r12), %rax
	movl	(%rax), %edi
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r15, %rax
	je	.LBB4_339
.LBB4_302:                              # %land.lhs.true279.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	rld+64(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB4_304
# BB#303:                               # %lor.lhs.false284.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	reg_class_contents(,%rax,8), %r14
	callq	true_regnum
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	%r14, %rdx
	je	.LBB4_339
.LBB4_304:                              # %land.lhs.true295.i
                                        #   in Loop: Header=BB4_300 Depth=1
	testq	%r13, %r13
	jne	.LBB4_339
# BB#305:                               # %land.lhs.true297.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	$0, rld+8(%r12)
	jne	.LBB4_339
# BB#306:                               # %land.lhs.true302.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	rld(%r12), %rax
	testq	%rax, %rax
	je	.LBB4_339
# BB#307:                               # %land.lhs.true307.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %ecx
	leaq	rld+8(%r12), %r14
	leaq	rld(%r12), %r9
	cmpq	$61, %rcx
	jne	.LBB4_315
# BB#308:                               # %land.lhs.true311.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movzbl	(%rax), %ecx
	movzbl	rtx_class(%rcx), %ecx
	cmpq	$97, %rcx
	jne	.LBB4_315
# BB#309:                               # %land.lhs.true321.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	8(%rax), %rdi
	movq	168(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB4_323
# BB#310:                               # %lor.lhs.false329.i
                                        #   in Loop: Header=BB4_300 Depth=1
	testq	%rdi, %rdi
	je	.LBB4_315
# BB#311:                               # %land.lhs.true338.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_313
# BB#312:                               # %land.lhs.true354.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movl	8(%rdi), %eax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB4_315
	jmp	.LBB4_323
.LBB4_313:                              # %cond.false369.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r9, 120(%rsp)          # 8-byte Spill
	callq	rtx_equal_p
	movq	120(%rsp), %r9          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB4_315
# BB#314:                               # %land.lhs.true378.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	(%r9), %rax
	movq	8(%rax), %rdi
	callq	side_effects_p
	movq	120(%rsp), %r9          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB4_323
.LBB4_315:                              # %lor.lhs.false387.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	(%r9), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB4_339
# BB#316:                               # %land.lhs.true395.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$97, %rax
	jne	.LBB4_339
# BB#317:                               # %land.lhs.true403.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	cmpq	%rsi, %rdi
	je	.LBB4_323
# BB#318:                               # %lor.lhs.false412.i
                                        #   in Loop: Header=BB4_300 Depth=1
	testq	%rdi, %rdi
	je	.LBB4_339
# BB#319:                               # %land.lhs.true418.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_321
# BB#320:                               # %land.lhs.true434.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movl	8(%rdi), %eax
	movl	8(%rsi), %ecx
	cmpq	%rcx, %rax
	je	.LBB4_323
	jmp	.LBB4_339
.LBB4_321:                              # %cond.false449.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	%r9, 120(%rsp)          # 8-byte Spill
	callq	rtx_equal_p
	testq	%r15, %rax
	je	.LBB4_339
# BB#322:                               # %land.lhs.true458.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	callq	side_effects_p
	movq	120(%rsp), %r9          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB4_339
.LBB4_323:                              # %land.lhs.true464.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	(%r14), %rax
	testq	%rax, %rax
	je	.LBB4_325
# BB#324:                               # %lor.lhs.false470.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movslq	n_earlyclobbers(%rip), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB4_325
	.align	16, 0x90
.LBB4_334:                              # %for.body.i681.i
                                        #   Parent Loop BB4_300 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	reload_earlyclobbers-8(,%rsi,8), %rcx
	cmpq	%rax, %rcx
	je	.LBB4_339
# BB#333:                               # %for.cond.i676.i
                                        #   in Loop: Header=BB4_334 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rdx, %rdi
	jl	.LBB4_334
.LBB4_325:                              # %land.lhs.true476.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	256(%rsp), %rcx
	andq	%r15, %rcx
	cmpq	$9, %rcx
	je	.LBB4_336
# BB#326:                               # %lor.lhs.false485.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movl	rld+92(%r12), %eax
	cmpq	$9, %rax
	je	.LBB4_336
# BB#327:                               # %lor.lhs.false491.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB4_329
# BB#328:                               # %land.lhs.true497.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movl	rld+72(%r12), %edx
	movq	248(%rsp), %rsi
	andq	%r15, %rsi
	cmpq	%rsi, %rdx
	je	.LBB4_336
.LBB4_329:                              # %lor.lhs.false503.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	$10, %rcx
	je	.LBB4_335
# BB#330:                               # %lor.lhs.false503.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	$7, %rcx
	jne	.LBB4_331
# BB#338:                               # %land.lhs.true515.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	$7, %rax
	jne	.LBB4_339
	jmp	.LBB4_336
.LBB4_335:                              # %land.lhs.true524.i
                                        #   in Loop: Header=BB4_300 Depth=1
	cmpq	$10, %rax
	jne	.LBB4_339
	jmp	.LBB4_336
.LBB4_331:                              # %lor.lhs.false503.i
                                        #   in Loop: Header=BB4_300 Depth=1
	movq	256(%rsp), %rcx
	testq	%r15, %rcx
	jne	.LBB4_339
# BB#332:                               # %land.lhs.true506.i
                                        #   in Loop: Header=BB4_300 Depth=1
	testq	%rax, %rax
	je	.LBB4_336
	.align	16, 0x90
.LBB4_339:                              # %for.inc541.i
                                        #   in Loop: Header=BB4_300 Depth=1
	incq	%rbx
	movslq	n_reloads(%rip), %r12
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rax
	jl	.LBB4_300
# BB#340:
	movq	%r15, %r8
	movq	%r13, %r10
	jmp	.LBB4_343
.LBB4_244:                              # %if.then.i
	movq	%r13, 128(%rsp)         # 8-byte Spill
	movq	%r12, 136(%rsp)         # 8-byte Spill
	movl	n_reloads(%rip), %r12d
	jmp	.LBB4_343
.LBB4_243:
	movq	%r13, 128(%rsp)         # 8-byte Spill
	jmp	.LBB4_343
.LBB4_342:                              # %find_reusable_reload.exit.loopexit3072
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	movq	%r13, %r12
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB4_343
.LBB4_246:
	movq	%r15, %r8
.LBB4_343:                              # %find_reusable_reload.exit
	movl	n_reloads(%rip), %eax
	movq	%r12, %rcx
	andq	%r8, %rcx
	cmpq	%rax, %rcx
	jne	.LBB4_519
# BB#344:                               # %if.then1297
	testq	%r10, %r10
	movq	168(%rsp), %rcx         # 8-byte Reload
	je	.LBB4_345
# BB#346:                               # %land.lhs.true1300
	movq	$1317, 144(%rsp)        # 8-byte Folded Spill
                                        # imm = 0x525
	movl	(%r10), %edx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$62, %rax
	movq	160(%rsp), %rbx         # 8-byte Reload
	jne	.LBB4_348
# BB#347:
	movq	%r8, %r15
	jmp	.LBB4_502
.LBB4_519:                              # %if.else1421
	movq	136(%rsp), %rsi         # 8-byte Reload
	testq	%r8, %rsi
	movq	112(%rsp), %rbx         # 8-byte Reload
	je	.LBB4_522
# BB#520:                               # %land.lhs.true1424
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movslq	rld+20(%rax), %rcx
	movzbl	mode_size(%rdx), %edx
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rdx
	jbe	.LBB4_522
# BB#521:                               # %if.then1436
	leaq	rld+20(%rax), %rax
	movl	%esi, (%rax)
.LBB4_522:                              # %if.end1440
	movq	128(%rsp), %rsi         # 8-byte Reload
	testq	%r8, %rsi
	je	.LBB4_525
# BB#523:                               # %land.lhs.true1443
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movslq	rld+24(%rax), %rcx
	movzbl	mode_size(%rdx), %edx
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rcx, %rdx
	jbe	.LBB4_525
# BB#524:                               # %if.then1455
	leaq	rld+24(%rax), %rax
	movl	%esi, (%rax)
.LBB4_525:                              # %if.end1459
	xorq	%rcx, %rcx
	movq	168(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB4_539
# BB#526:                               # %if.then1462
	movq	%rax, %rdi
	movq	%r8, %r15
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	je	.LBB4_528
# BB#527:                               # %cond.true1465
	movq	(%rbx), %rdx
.LBB4_528:                              # %cond.end1467
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rbx
	movq	rld(%rbx), %rsi
	leaq	rld(%rbx), %r13
	cmpq	%rdi, %rsi
	je	.LBB4_529
# BB#530:                               # %land.lhs.true1474
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	movq	%r10, %r14
	callq	rtx_equal_p
	movq	%r15, %r8
	testq	%r8, %rax
	je	.LBB4_531
# BB#532:                               # %land.lhs.true1480
	movzbl	rld+96(%rbx), %eax
	testq	$1, %rax
	movq	%r14, %r10
	je	.LBB4_534
# BB#533:                               # %land.lhs.true1480
	movq	240(%rsp), %rax
	testq	%r8, %rax
	jne	.LBB4_537
.LBB4_534:                              # %if.then1489
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movq	%r8, %r15
	movslq	rld+72(%rbx), %rax
	movq	$32, %rcx
	movq	248(%rsp), %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB4_536
# BB#535:                               # %if.then1495
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	remove_address_replacements
	movq	(%r13), %rdi
	movq	rld+40(%rbx), %rdx
	movq	%r15, %r8
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB4_538
.LBB4_345:
	movq	$1317, 144(%rsp)        # 8-byte Folded Spill
                                        # imm = 0x525
	movq	%r8, %r15
	movq	160(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB4_502
.LBB4_348:                              # %if.then1305
	movq	%rcx, %r9
	movq	256(%rsp), %rsi
	leaq	-3(%rsi), %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$4, %rcx
	movq	%rsi, %r13
	jb	.LBB4_350
# BB#349:                               # %if.then1305
	movq	$5, %r13
.LBB4_350:                              # %if.then1305
	cmpq	$63, %rax
	jne	.LBB4_351
# BB#352:                               # %land.lhs.true.i2901
	movq	8(%r10), %rdi
	movq	$16, %rcx
	movzbl	2(%rdi), %eax
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %ecx
	movzbl	mode_size(%rax), %edx
	cmpq	%rdx, %rcx
	ja	.LBB4_354
# BB#353:                               # %land.lhs.true.i2901
	movq	128(%rsp), %rax         # 8-byte Reload
.LBB4_354:                              # %land.lhs.true.i2901
	ja	.LBB4_356
# BB#355:                               # %land.lhs.true.i2901
	movq	%r10, %rdi
.LBB4_356:                              # %land.lhs.true.i2901
	movl	(%rdi), %edx
	jmp	.LBB4_357
.LBB4_529:
	movq	%r15, %r8
	jmp	.LBB4_538
.LBB4_351:
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%r10, %rdi
.LBB4_357:                              # %if.end25.i
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB4_361
# BB#358:                               # %land.lhs.true30.i
	movl	8(%rdi), %ecx
	cmpq	$53, %rcx
	jb	.LBB4_361
# BB#359:                               # %land.lhs.true35.i
	movq	reg_equiv_mem(%rip), %rdx
	movq	(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB4_361
# BB#360:                               # %land.lhs.true35.i
	movq	%rcx, %rdi
.LBB4_361:                              # %if.then51.i
	leaq	-10(%rbx), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	ja	.LBB4_362
# BB#363:                               # %land.lhs.true60.i2904
	movl	target_flags(%rip), %ecx
	testq	$33554432, %rcx         # imm = 0x2000000
	jne	.LBB4_364
# BB#365:                               # %land.lhs.true60.i2904
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$2, %rax
	movq	%r9, %rcx
	jne	.LBB4_502
# BB#366:                               # %cond.false.i2905
	movl	reload_out_optab+8(%rip), %esi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1317, %rsi             # imm = 0x525
	movq	$8, %r12
	jne	.LBB4_368
# BB#367:
	movq	$1317, %rax             # imm = 0x525
	movq	$1317, %rsi             # imm = 0x525
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB4_407
.LBB4_362:
	movq	%r9, %rcx
	jmp	.LBB4_502
.LBB4_531:
	movq	%r14, %r10
	jmp	.LBB4_537
.LBB4_536:                              # %if.else1503
	movq	(%r13), %rdi
	callq	remove_address_replacements
	movq	%r15, %r8
	movq	152(%rsp), %r10         # 8-byte Reload
.LBB4_537:                              # %if.end1509
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	144(%rsp), %rdx         # 8-byte Reload
.LBB4_538:                              # %if.end1509
	movq	%rdi, (%r13)
	movq	%rdx, rld+40(%rbx)
	movq	%rdi, %rcx
.LBB4_539:                              # %if.end1516
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	%r8, %r14
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%r10, %r10
	je	.LBB4_543
# BB#540:                               # %if.then1519
	imulq	$104, %rax, %rcx
	movq	%r10, rld+8(%rcx)
	xorq	%rdx, %rdx
	movq	104(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB4_542
# BB#541:                               # %cond.true1524
	movq	(%rsi), %rdx
.LBB4_542:                              # %cond.end1526
	movq	%rdx, rld+48(%rcx)
.LBB4_543:                              # %if.end1531
	movq	%r10, %r15
	imulq	$104, %rax, %rbx
	movl	rld+16(%rbx), %esi
	movq	160(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	callq	reg_class_subset_p
	movq	%r14, %r8
	testq	%r8, %rax
	je	.LBB4_545
# BB#544:                               # %if.then1537
	leaq	rld+16(%rbx), %rax
	movl	%r13d, (%rax)
.LBB4_545:                              # %if.end1541
	movzbl	rld+96(%rbx), %eax
	movq	240(%rsp), %rcx
	andq	%rax, %rcx
	andq	$1, %rcx
	andq	$254, %rax
	orq	%rcx, %rax
	movb	%al, rld+96(%rbx)
	movl	rld+92(%rbx), %edx
	movl	rld+72(%rbx), %r9d
	movq	256(%rsp), %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rdx
	movq	%r15, %r10
	movq	248(%rsp), %rdi
	movq	%rdi, %r14
	movq	168(%rsp), %r11         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	jne	.LBB4_549
# BB#546:                               # %lor.lhs.false1559
	movq	%r14, %rsi
	andq	%r8, %rsi
	movq	%r9, %rax
	andq	%r8, %rax
	cmpq	%rsi, %rax
	movq	%r14, %rdi
	je	.LBB4_551
# BB#547:                               # %switch.early.test
	cmpq	$10, %rcx
	ja	.LBB4_549
# BB#548:                               # %switch.early.test
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$1153, %rax             # imm = 0x481
	jne	.LBB4_550
.LBB4_549:                              # %if.then1574
	leaq	rld+92(%rbx), %rcx
	movq	$9, %rax
	movl	%eax, (%rcx)
.LBB4_550:                              # %if.end1578
	movq	%r9, %rdi
.LBB4_551:                              # %if.end1578
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jle	.LBB4_553
# BB#552:                               # %if.end1578
	movq	%r14, %rdi
.LBB4_553:                              # %if.end1578
	movl	%edi, rld+72(%rbx)
	movq	104(%rsp), %r9          # 8-byte Reload
	jmp	.LBB4_554
.LBB4_171:                              # %land.lhs.true1077
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	jmp	.LBB4_172
.LBB4_364:
	movq	%r9, %rcx
	jmp	.LBB4_502
.LBB4_368:                              # %land.lhs.true78.i
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %r14
	movq	%r10, 152(%rsp)         # 8-byte Spill
	leaq	(%rax,%rax,4), %rax
	movq	insn_data+24(,%rax,8), %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB4_372
# BB#369:                               # %land.lhs.true84.i
	movq	$2, %rsi
	callq	*%rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB4_370
.LBB4_372:                              # %if.then96.i
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	testq	%rax, %rax
	movq	$24, %r15
	je	.LBB4_377
# BB#373:                               # %if.else108.i
	movq	$12, %r15
	cmpq	$114, %rax
	je	.LBB4_377
# BB#374:                               # %cond.false129.i
	movq	$11, %r15
	cmpq	$82, %rax
	je	.LBB4_377
# BB#375:                               # %cond.false129.i
	cmpq	$113, %rax
	jne	.LBB4_380
# BB#376:                               # %cond.true138.i
	movl	target_flags(%rip), %r15d
	movq	$23, %rcx
	shrq	%cl, %r15
	andq	$4, %r15
	orq	$8, %r15
	jmp	.LBB4_377
.LBB4_336:                              # %if.then530.i
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	movq	%rbx, %r12
	movq	%r15, %r8
	movq	%r13, %r10
	jne	.LBB4_343
# BB#337:                               # %if.then535.i
	movq	(%r9), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rbx, %r12
	jmp	.LBB4_343
.LBB4_370:
	movq	$1317, %rax             # imm = 0x525
	movq	$1317, %rsi             # imm = 0x525
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB4_371
.LBB4_380:                              # %cond.false142.i
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$101, %rdx
	jg	.LBB4_391
# BB#381:                               # %cond.false142.i
	movq	$8, %r15
	cmpq	$81, %rax
	je	.LBB4_377
	jmp	.LBB4_382
.LBB4_391:                              # %cond.false142.i
	cmpq	$117, %rax
	je	.LBB4_397
# BB#392:                               # %cond.false142.i
	cmpq	$116, %rax
	jne	.LBB4_393
# BB#396:                               # %cond.true161.i
	movzbl	target_flags(%rip), %eax
	movq	$13, %r15
	jmp	.LBB4_395
.LBB4_397:                              # %cond.true173.i
	movzbl	target_flags(%rip), %eax
	movq	$14, %r15
	jmp	.LBB4_395
.LBB4_393:                              # %cond.false142.i
	cmpq	$102, %rax
	jne	.LBB4_382
# BB#394:                               # %cond.true151.i
	movzbl	target_flags(%rip), %eax
	movq	$15, %r15
.LBB4_395:                              # %cond.true151.i
	testq	$33, %rax
	jne	.LBB4_377
	jmp	.LBB4_404
.LBB4_382:                              # %cond.false181.i
	movq	$1, %r15
	cmpq	$97, %rax
	je	.LBB4_377
# BB#383:                               # %cond.false186.i
	movq	$4, %r15
	cmpq	$98, %rax
	je	.LBB4_377
# BB#384:                               # %cond.false191.i
	movq	$3, %r15
	cmpq	$99, %rax
	je	.LBB4_377
# BB#385:                               # %cond.false196.i
	movq	%r13, %rsi
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$119, %rdx
	jg	.LBB4_398
# BB#386:                               # %cond.false196.i
	cmpq	$89, %rax
	jne	.LBB4_387
# BB#402:                               # %cond.end275.i
	movl	target_flags(%rip), %r15d
	movq	$14, %rcx
	shrq	%cl, %r15
	andq	$16, %r15
	jmp	.LBB4_403
.LBB4_236:                              # %if.then1292
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_reload, %rcx
	movq	$1258, %rsi             # imm = 0x4EA
	jmp	.LBB4_78
.LBB4_398:                              # %cond.false196.i
	cmpq	$121, %rax
	jne	.LBB4_399
# BB#401:                               # %cond.true223.i
	movzbl	target_flags+1(%rip), %eax
	movq	$17, %r15
	testq	$64, %rax
.LBB4_403:                              # %cond.end275.i
	movq	%rsi, %r13
	jne	.LBB4_377
	jmp	.LBB4_404
.LBB4_387:                              # %cond.false196.i
	movq	$2, %r15
	cmpq	$100, %rax
	movq	%rsi, %r13
	je	.LBB4_377
	jmp	.LBB4_388
.LBB4_399:                              # %cond.false196.i
	cmpq	$120, %rax
	movq	%rsi, %r13
	jne	.LBB4_388
# BB#400:                               # %cond.true205.i
	movzbl	target_flags+2(%rip), %eax
	movq	$16, %r15
	testq	$5, %rax
	jne	.LBB4_377
	jmp	.LBB4_404
.LBB4_388:                              # %cond.false228.i
	movq	$7, %r15
	cmpq	$65, %rax
	je	.LBB4_377
# BB#389:                               # %cond.false233.i
	movq	$6, %r15
	cmpq	$68, %rax
	je	.LBB4_377
# BB#390:                               # %cond.false238.i
	movq	$5, %r15
	cmpq	$83, %rax
	jne	.LBB4_404
.LBB4_377:                              # %if.end298.i
	movq	56(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB4_379
# BB#378:                               # %lor.lhs.false308.i
	movzbl	1(%rax), %eax
	cmpq	$38, %rax
	jne	.LBB4_379
# BB#405:                               # %if.end319.i
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	reg_class_subset_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB4_408
# BB#406:                               # %if.then322.i
	movzbl	64(%rbx), %edi
	movq	$1317, %rax             # imm = 0x525
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	152(%rsp), %r10         # 8-byte Reload
	movl	reload_out_optab+8(%rip), %esi
	jmp	.LBB4_407
.LBB4_408:                              # %if.else329.i
	movq	56(%rbx), %rax
	movzbl	2(%rax), %eax
	movzbl	64(%rbx), %ecx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	cmpq	$114, %rax
	movq	$12, 144(%rsp)          # 8-byte Folded Spill
	je	.LBB4_436
# BB#409:                               # %cond.false352.i
	movq	$11, 144(%rsp)          # 8-byte Folded Spill
	cmpq	$82, %rax
	je	.LBB4_436
# BB#410:                               # %cond.false352.i
	cmpq	$113, %rax
	jne	.LBB4_412
# BB#411:                               # %cond.true361.i
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	orq	$8, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	jmp	.LBB4_436
.LBB4_412:                              # %cond.false365.i
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$101, %rsi
	jg	.LBB4_423
# BB#413:                               # %cond.false365.i
	movq	$8, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$81, %rax
	je	.LBB4_436
	jmp	.LBB4_414
.LBB4_423:                              # %cond.false365.i
	cmpq	$117, %rax
	je	.LBB4_428
# BB#424:                               # %cond.false365.i
	cmpq	$116, %rax
	jne	.LBB4_425
# BB#427:                               # %cond.true386.i
	movzbl	target_flags(%rip), %eax
	movq	$13, 144(%rsp)          # 8-byte Folded Spill
	jmp	.LBB4_429
.LBB4_428:                              # %cond.true398.i
	movzbl	target_flags(%rip), %eax
	movq	$14, 144(%rsp)          # 8-byte Folded Spill
	jmp	.LBB4_429
.LBB4_425:                              # %cond.false365.i
	cmpq	$102, %rax
	jne	.LBB4_414
# BB#426:                               # %cond.true374.i
	movzbl	target_flags(%rip), %eax
	movq	$15, 144(%rsp)          # 8-byte Folded Spill
.LBB4_429:                              # %cond.true398.i
	testq	$33, %rax
	jne	.LBB4_436
	jmp	.LBB4_430
.LBB4_414:                              # %cond.false406.i
	movq	$1, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$97, %rax
	je	.LBB4_436
# BB#415:                               # %cond.false411.i
	movq	$4, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$98, %rax
	je	.LBB4_436
# BB#416:                               # %cond.false416.i
	movq	$3, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$99, %rax
	je	.LBB4_436
# BB#417:                               # %cond.false421.i
	movq	%r15, %rbx
	movq	%r13, %rdi
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$119, %rsi
	jg	.LBB4_431
# BB#418:                               # %cond.false421.i
	cmpq	$89, %rax
	jne	.LBB4_419
# BB#680:                               # %if.end515.i
	movl	target_flags(%rip), %eax
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$16, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	jmp	.LBB4_435
.LBB4_404:                              # %if.then279.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_secondary_reload, %rcx
	movq	$388, %rsi              # imm = 0x184
	jmp	.LBB4_78
.LBB4_379:                              # %if.then318.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.push_secondary_reload, %rcx
	movq	$397, %rsi              # imm = 0x18D
.LBB4_78:                               # %if.then441
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB4_431:                              # %cond.false421.i
	cmpq	$121, %rax
	jne	.LBB4_432
# BB#434:                               # %cond.true448.i
	movzbl	target_flags+1(%rip), %eax
	movq	$17, 144(%rsp)          # 8-byte Folded Spill
	testq	$64, %rax
.LBB4_435:                              # %cond.true448.i
	movq	%rdi, %r13
	movq	%rbx, %r15
	jne	.LBB4_436
	jmp	.LBB4_430
.LBB4_419:                              # %cond.false421.i
	movq	$2, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$100, %rax
	movq	%rdi, %r13
	movq	%rbx, %r15
	je	.LBB4_436
	jmp	.LBB4_420
.LBB4_432:                              # %cond.false421.i
	cmpq	$120, %rax
	movq	%rdi, %r13
	movq	%rbx, %r15
	jne	.LBB4_420
# BB#433:                               # %cond.true430.i
	movzbl	target_flags+2(%rip), %eax
	movq	$16, 144(%rsp)          # 8-byte Folded Spill
	testq	$5, %rax
	jne	.LBB4_436
	jmp	.LBB4_430
.LBB4_420:                              # %cond.false453.i
	movq	$7, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$65, %rax
	je	.LBB4_436
# BB#421:                               # %cond.false458.i
	movq	$6, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$68, %rax
	je	.LBB4_436
# BB#422:                               # %cond.false463.i
	movq	$5, 144(%rsp)           # 8-byte Folded Spill
	cmpq	$83, %rax
	jne	.LBB4_430
.LBB4_436:                              # %for.cond.i2908.preheader
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movslq	n_reloads(%rip), %rbx
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	jle	.LBB4_463
# BB#437:                               # %for.body.i2909.lr.ph
	movq	$1, %r15
	movabsq	$rld+96, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB4_438:                              # %for.body.i2909
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %eax
	testq	$4, %rax
	je	.LBB4_462
# BB#439:                               # %land.lhs.true528.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-80(%r12), %esi
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r14, %rax
	jne	.LBB4_441
# BB#440:                               # %lor.lhs.false534.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-80(%r12), %edi
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r14, %rax
	je	.LBB4_462
.LBB4_441:                              # %land.lhs.true549.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-72(%r12), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB4_462
# BB#442:                               # %land.lhs.true563.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-8(%r12), %eax
	cmpq	$1317, %rax             # imm = 0x525
	jne	.LBB4_462
# BB#443:                               # %land.lhs.true568.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	%r13, %rbx
	andq	%r14, %rbx
	cmpq	$9, %rbx
	je	.LBB4_453
# BB#444:                               # %lor.lhs.false577.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-4(%r12), %eax
	cmpq	$9, %rax
	je	.LBB4_453
# BB#445:                               # %lor.lhs.false582.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	%rax, %rbx
	jne	.LBB4_447
# BB#446:                               # %land.lhs.true588.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	-24(%r12), %ecx
	movq	248(%rsp), %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	je	.LBB4_453
.LBB4_447:                              # %lor.lhs.false594.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	$10, %rbx
	je	.LBB4_452
# BB#448:                               # %lor.lhs.false594.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	$7, %rbx
	jne	.LBB4_449
# BB#451:                               # %land.lhs.true606.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	$7, %rax
	je	.LBB4_453
	jmp	.LBB4_462
.LBB4_452:                              # %land.lhs.true615.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	$10, %rax
	jne	.LBB4_462
	jmp	.LBB4_453
.LBB4_449:                              # %lor.lhs.false594.i
                                        #   in Loop: Header=BB4_438 Depth=1
	testq	%r14, %r13
	jne	.LBB4_462
# BB#450:                               # %land.lhs.true597.i
                                        #   in Loop: Header=BB4_438 Depth=1
	testq	%rax, %rax
	jne	.LBB4_462
.LBB4_453:                              # %if.then629.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movl	%eax, -72(%r12)
	movl	-80(%r12), %esi
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r14, %rax
	je	.LBB4_455
# BB#454:                               # %if.then639.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	%eax, -80(%r12)
.LBB4_455:                              # %if.end643.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movslq	-24(%r12), %rax
	movq	$32, %rcx
	movq	248(%rsp), %rsi
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jle	.LBB4_457
# BB#456:                               # %if.end643.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	%rsi, %rax
.LBB4_457:                              # %if.end643.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movl	%eax, -24(%r12)
	movzbl	(%r12), %ecx
	movq	%rcx, %rdx
	movq	240(%rsp), %rsi
	andq	%rsi, %rdx
	andq	$1, %rdx
	andq	$250, %rcx
	orq	%rdx, %rcx
	orq	$4, %rcx
	movb	%cl, (%r12)
	movl	-4(%r12), %ecx
	cmpq	%rcx, %rbx
	jne	.LBB4_461
# BB#458:                               # %lor.lhs.false679.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	248(%rsp), %rcx
	andq	%r14, %rcx
	andq	%r14, %rax
	cmpq	%rcx, %rax
	je	.LBB4_462
# BB#459:                               # %switch.early.test.i
                                        #   in Loop: Header=BB4_438 Depth=1
	cmpq	$10, %rbx
	ja	.LBB4_461
# BB#460:                               # %switch.early.test.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	$1153, %rax             # imm = 0x481
	jne	.LBB4_462
.LBB4_461:                              # %if.then694.i
                                        #   in Loop: Header=BB4_438 Depth=1
	movq	$9, %rax
	movl	%eax, -4(%r12)
.LBB4_462:                              # %for.inc.i2911
                                        #   in Loop: Header=BB4_438 Depth=1
	movslq	n_reloads(%rip), %rbx
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r15, %rdx
	incq	%r15
	addq	$104, %r12
	cmpq	%rbx, %rax
	jl	.LBB4_438
.LBB4_463:                              # %for.end.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%rax, %rcx
	movq	%rdx, %rsi
	andq	%rax, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB4_464
# BB#465:                               # %if.then702.i
	imulq	$104, %rbx, %rcx
	movq	$0, rld+8(%rcx)
	movq	$0, rld(%rcx)
	movq	144(%rsp), %rdx         # 8-byte Reload
	movl	%edx, rld+16(%rcx)
	xorq	%rdx, %rdx
	movl	%edx, rld+20(%rcx)
	movq	96(%rsp), %rsi          # 8-byte Reload
	movl	%esi, rld+24(%rcx)
	movq	$0, rld+64(%rcx)
	movzbl	rld+96(%rcx), %esi
	movq	240(%rsp), %rdi
	andq	$1, %rdi
	andq	$248, %rsi
	orq	%rdi, %rsi
	movl	%edx, rld+36(%rcx)
	movq	$0, rld+48(%rcx)
	movq	$0, rld+40(%rcx)
	movq	248(%rsp), %rdx
	movl	%edx, rld+72(%rcx)
	movl	%r13d, rld+92(%rcx)
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movl	%eax, rld+76(%rcx)
	movl	%eax, rld+80(%rcx)
	movq	$1317, %rax             # imm = 0x525
	movl	%eax, rld+84(%rcx)
	movl	%eax, rld+88(%rcx)
	movq	$1317, 56(%rsp)         # 8-byte Folded Spill
                                        # imm = 0x525
	orq	$6, %rsi
	movb	%sil, rld+96(%rcx)
	leaq	1(%rbx), %r15
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	movl	%r15d, n_reloads(%rip)
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB4_466
.LBB4_464:
	movl	reload_out_optab+8(%rip), %eax
	movq	$1317, %rsi             # imm = 0x525
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	120(%rsp), %r12         # 8-byte Reload
.LBB4_371:                              # %if.end772.ithread-pre-split
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	152(%rsp), %r10         # 8-byte Reload
.LBB4_407:                              # %if.end772.ithread-pre-split
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movl	n_reloads(%rip), %r15d
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r12, 120(%rsp)         # 8-byte Spill
	movq	%rdx, 144(%rsp)         # 8-byte Spill
.LBB4_466:                              # %if.end772.i
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jle	.LBB4_494
# BB#467:                               # %for.body776.i.lr.ph
	movq	$1, %r13
	movabsq	$rld+96, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB4_468:                              # %for.body776.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %eax
	testq	$4, %rax
	je	.LBB4_493
# BB#469:                               # %land.lhs.true785.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-80(%r14), %esi
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r8, %r15
	movq	%r10, %rbx
	callq	reg_class_subset_p
	movq	%rbx, %r10
	movq	%r15, %r8
	testq	%r12, %rax
	jne	.LBB4_471
# BB#470:                               # %lor.lhs.false791.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-80(%r14), %edi
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	%r8, %r15
	movq	%r10, %rbx
	callq	reg_class_subset_p
	movq	%rbx, %r10
	movq	%r15, %r8
	testq	%r12, %rax
	je	.LBB4_493
.LBB4_471:                              # %land.lhs.true807.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-72(%r14), %eax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB4_493
# BB#472:                               # %land.lhs.true823.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-16(%r14), %eax
	movq	144(%rsp), %rcx         # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jne	.LBB4_493
# BB#473:                               # %land.lhs.true839.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-8(%r14), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB4_493
# BB#474:                               # %land.lhs.true845.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	64(%rsp), %r15          # 8-byte Reload
	andq	%r12, %r15
	cmpq	$9, %r15
	je	.LBB4_484
# BB#475:                               # %lor.lhs.false854.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-4(%r14), %eax
	cmpq	$9, %rax
	je	.LBB4_484
# BB#476:                               # %lor.lhs.false860.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	%rax, %r15
	jne	.LBB4_478
# BB#477:                               # %land.lhs.true866.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	-24(%r14), %ecx
	movq	248(%rsp), %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	je	.LBB4_484
.LBB4_478:                              # %lor.lhs.false872.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	$10, %r15
	je	.LBB4_483
# BB#479:                               # %lor.lhs.false872.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	$7, %r15
	jne	.LBB4_480
# BB#482:                               # %land.lhs.true884.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	$7, %rax
	je	.LBB4_484
	jmp	.LBB4_493
.LBB4_483:                              # %land.lhs.true893.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	$10, %rax
	jne	.LBB4_493
	jmp	.LBB4_484
.LBB4_480:                              # %lor.lhs.false872.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB4_493
# BB#481:                               # %land.lhs.true875.i
                                        #   in Loop: Header=BB4_468 Depth=1
	testq	%rax, %rax
	jne	.LBB4_493
.LBB4_484:                              # %if.then907.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	%r10, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	movl	%eax, -72(%r14)
	movl	-80(%r14), %esi
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	reg_class_subset_p
	testq	%r12, %rax
	je	.LBB4_486
# BB#485:                               # %if.then917.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	%eax, -80(%r14)
.LBB4_486:                              # %if.end921.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movslq	-24(%r14), %rax
	movq	$32, %rcx
	movq	248(%rsp), %rsi
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rbx, %r10
	jle	.LBB4_488
# BB#487:                               # %if.end921.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	%rsi, %rax
.LBB4_488:                              # %if.end921.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movl	%eax, -24(%r14)
	movzbl	(%r14), %ecx
	movq	%rcx, %rdx
	movq	240(%rsp), %rsi
	andq	%rsi, %rdx
	andq	$1, %rdx
	andq	$250, %rcx
	orq	%rdx, %rcx
	orq	$4, %rcx
	movb	%cl, (%r14)
	movl	-4(%r14), %ecx
	cmpq	%rcx, %r15
	jne	.LBB4_492
# BB#489:                               # %lor.lhs.false960.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	248(%rsp), %rcx
	andq	%r12, %rcx
	andq	%r12, %rax
	cmpq	%rcx, %rax
	je	.LBB4_493
# BB#490:                               # %switch.early.test10.i
                                        #   in Loop: Header=BB4_468 Depth=1
	cmpq	$10, %r15
	ja	.LBB4_492
# BB#491:                               # %switch.early.test10.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	$1, %rax
	movq	%r15, %rcx
	shlq	%cl, %rax
	testq	$1153, %rax             # imm = 0x481
	jne	.LBB4_493
.LBB4_492:                              # %if.then975.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movq	$9, %rax
	movl	%eax, -4(%r14)
	.align	16, 0x90
.LBB4_493:                              # %for.inc981.i
                                        #   in Loop: Header=BB4_468 Depth=1
	movslq	n_reloads(%rip), %r15
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r13, %rax
	incq	%r13
	addq	$104, %r14
	cmpq	%r15, %rdx
	jl	.LBB4_468
.LBB4_494:                              # %for.end983.i
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%r12, %rcx
	movq	%rax, %rdx
	andq	%r12, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB4_495
# BB#496:                               # %if.end996.i
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movq	$0, rld+8(%rax)
	movq	$0, rld(%rax)
	movq	120(%rsp), %r9          # 8-byte Reload
	movl	%r9d, rld+16(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, rld+20(%rax)
	movq	96(%rsp), %rdx          # 8-byte Reload
	movl	%edx, rld+24(%rax)
	movq	$0, rld+64(%rax)
	movzbl	rld+96(%rax), %edi
	movq	240(%rsp), %rsi
	andq	$1, %rsi
	andq	$248, %rdi
	orq	%rsi, %rdi
	orq	$6, %rdi
	leaq	1(%r15), %rsi
	movq	56(%rsp), %rbx          # 8-byte Reload
	cmpq	$1317, %rbx             # imm = 0x525
	movl	%ecx, rld+36(%rax)
	movq	$0, rld+48(%rax)
	movq	$0, rld+40(%rax)
	movq	248(%rsp), %rcx
	movl	%ecx, rld+72(%rax)
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, rld+92(%rax)
	movl	%r12d, rld+76(%rax)
	movq	144(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, rld+80(%rax)
	movq	$1317, %rcx             # imm = 0x525
	movl	%ecx, rld+84(%rax)
	movq	72(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, rld+88(%rax)
	movb	%dil, rld+96(%rax)
	movl	%esi, n_reloads(%rip)
	jne	.LBB4_497
# BB#498:                               # %land.lhs.true1097.i
	movq	$1317, 144(%rsp)        # 8-byte Folded Spill
                                        # imm = 0x525
	movq	%r10, %rbx
	movq	%r8, %r14
	movq	$1, %rcx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r9, %rsi
	movq	%rdx, %r13
	callq	ix86_secondary_memory_needed
	testq	%r12, %rax
	je	.LBB4_500
# BB#499:                               # %if.then1100.i
	movq	%r13, %rsi
	movq	248(%rsp), %rdx
	movq	256(%rsp), %rcx
	callq	get_secondary_mem
.LBB4_500:                              # %if.end1307
	movq	%r14, %r8
	movq	%rbx, %r10
	jmp	.LBB4_501
.LBB4_495:
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movq	%rax, %r15
	jmp	.LBB4_501
.LBB4_497:
	movq	%rbx, 144(%rsp)         # 8-byte Spill
.LBB4_501:                              # %if.end1307
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	168(%rsp), %rcx         # 8-byte Reload
.LBB4_502:                              # %if.end1307
	testq	%rcx, %rcx
	je	.LBB4_507
# BB#503:                               # %land.lhs.true1310
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	jne	.LBB4_507
# BB#504:                               # %land.lhs.true1315
	movl	8(%rcx), %eax
	cmpq	$52, %rax
	ja	.LBB4_507
# BB#505:                               # %land.lhs.true1321
	movl	regclass_map(,%rax,4), %edi
	movq	%rcx, %r13
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %r12
	movq	%r8, %rbx
	movq	%r10, %r14
	callq	ix86_secondary_memory_needed
	movq	%r14, %r10
	movq	%rbx, %r8
	movq	%r12, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r13, %rcx
	je	.LBB4_507
# BB#506:                               # %if.then1329
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	248(%rsp), %rdx
	movq	%rcx, %r13
	movq	256(%rsp), %rcx
	movq	%r8, %r14
	movq	%rbx, %r12
	movq	%r10, %rbx
	callq	get_secondary_mem
	movq	%r13, %rcx
	movq	%rbx, %r10
	movq	%r12, %rbx
	movq	%r14, %r8
.LBB4_507:                              # %if.end1331
	movslq	n_reloads(%rip), %r12
	imulq	$104, %r12, %rdx
	movq	%rcx, rld(%rdx)
	movq	%rcx, %r13
	movq	%r10, rld+8(%rdx)
	movl	%ebx, rld+16(%rdx)
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	%eax, rld+20(%rdx)
	movq	128(%rsp), %rax         # 8-byte Reload
	movl	%eax, rld+24(%rdx)
	movq	$0, rld+64(%rdx)
	movzbl	rld+96(%rdx), %eax
	movq	240(%rsp), %rcx
	andq	$1, %rcx
	andq	$-2, %rax
	orq	%rcx, %rax
	xorq	%rcx, %rcx
	movl	%ecx, rld+36(%rdx)
	movq	%rax, %rsi
	andq	$253, %rsi
	movb	%sil, rld+96(%rdx)
	movq	112(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	movabsq	$0, %rdi
	je	.LBB4_509
# BB#508:                               # %cond.true1362
	movq	(%rsi), %rdi
.LBB4_509:                              # %cond.end1364
	movq	%rsi, %r9
	leaq	rld+96(%rdx), %r11
	imulq	$104, %r12, %rsi
	movq	%rdi, rld+40(%rsi)
	movq	104(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	movq	248(%rsp), %rdx
	je	.LBB4_511
# BB#510:                               # %cond.true1369
	movq	(%rdi), %rcx
.LBB4_511:                              # %cond.end1371
	movq	%rdi, %rbx
	andq	$249, %rax
	leaq	1(%r12), %rdi
	testq	%r10, %r10
	movq	%rcx, rld+48(%rsi)
	movl	%edx, rld+72(%rsi)
	movq	256(%rsp), %rcx
	movl	%ecx, rld+92(%rsi)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movl	%r14d, rld+76(%rsi)
	movl	%r15d, rld+80(%rsi)
	movq	$1317, %rcx             # imm = 0x525
	movl	%ecx, rld+84(%rsi)
	movq	144(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, rld+88(%rsi)
	movb	%al, (%r11)
	movl	%edi, n_reloads(%rip)
	je	.LBB4_512
# BB#513:                               # %land.lhs.true1399
	movzbl	(%r10), %eax
	cmpq	$61, %rax
	movq	%r9, %r15
	movq	%rbx, %r9
	jne	.LBB4_514
# BB#515:                               # %land.lhs.true1404
	movl	8(%r10), %eax
	cmpq	$52, %rax
	ja	.LBB4_514
# BB#516:                               # %land.lhs.true1410
	movq	%r10, 152(%rsp)         # 8-byte Spill
	movl	regclass_map(,%rax,4), %esi
	movq	$1, %rcx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	callq	ix86_secondary_memory_needed
	testq	%r14, %rax
	je	.LBB4_517
# BB#518:                               # %if.then1418
	movq	%rbx, %rsi
	movq	248(%rsp), %rdx
	movq	256(%rsp), %rcx
	callq	get_secondary_mem
.LBB4_517:
	movq	%r13, %r11
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	152(%rsp), %r10         # 8-byte Reload
	movq	104(%rsp), %r9          # 8-byte Reload
	movq	%r15, %rdx
	jmp	.LBB4_554
.LBB4_514:
	movq	%r13, %r11
	movq	%r15, %rdx
	jmp	.LBB4_554
.LBB4_512:
	movq	%r13, %r11
	movq	%r9, %rdx
	movq	%rbx, %r9
.LBB4_554:                              # %if.end1594
	movq	%r12, 144(%rsp)         # 8-byte Spill
	movq	%r11, 168(%rsp)         # 8-byte Spill
	testq	%r11, %r11
	je	.LBB4_557
# BB#555:                               # %land.lhs.true1597
	movq	(%rdx), %rax
	cmpq	%rax, %r11
	je	.LBB4_557
# BB#556:                               # %if.then1600
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movzbl	rld+96(%rax), %ecx
	orq	$2, %rcx
	movb	%cl, rld+96(%rax)
.LBB4_557:                              # %if.end1607
	movl	replace_reloads(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_563
# BB#558:                               # %if.then1609
	testq	%rdx, %rdx
	je	.LBB4_560
# BB#559:                               # %if.then1612
	movslq	n_replacements(%rip), %rax
	leaq	1(%rax), %rcx
	movl	%ecx, n_replacements(%rip)
	leaq	(%rax,%rax,2), %rax
	movl	%r12d, replacements+16(,%rax,8)
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, replacements+8(,%rax,8)
	movq	%rdx, replacements(,%rax,8)
	movq	136(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, replacements+20(,%rax,8)
.LBB4_560:                              # %if.end1617
	testq	%r9, %r9
	je	.LBB4_563
# BB#561:                               # %if.end1617
	cmpq	%rdx, %r9
	je	.LBB4_563
# BB#562:                               # %if.then1623
	movslq	n_replacements(%rip), %rax
	leaq	1(%rax), %rcx
	movl	%ecx, n_replacements(%rip)
	leaq	(%rax,%rax,2), %rax
	movl	%r12d, replacements+16(,%rax,8)
	movq	%r9, replacements(,%rax,8)
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, replacements+8(,%rax,8)
	movq	128(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, replacements+20(,%rax,8)
.LBB4_563:                              # %if.end1633
	movq	%r8, %r15
	testq	%r10, %r10
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rdi
	jne	.LBB4_565
# BB#564:                               # %if.end1633
	movq	%rcx, %rdi
.LBB4_565:                              # %if.end1633
	testq	%r11, %r11
	jne	.LBB4_567
# BB#566:                               # %if.end1633
	movq	%rcx, %rax
.LBB4_567:                              # %if.end1633
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$104, %rsi, %rbx
	cmpq	%r10, %r11
	leaq	rld+64(%rbx), %rsi
	je	.LBB4_584
# BB#568:                               # %if.end1633
	orq	%rdi, %rax
	jne	.LBB4_584
# BB#569:                               # %land.lhs.true1642
	cmpq	$0, (%rsi)
	jne	.LBB4_584
# BB#570:                               # %if.then1648
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	movslq	n_earlyclobbers(%rip), %rax
	movl	rld+16(%rbx), %r8d
	testq	%rax, %rax
	jle	.LBB4_571
# BB#574:
	movq	$1, %rsi
	.align	16, 0x90
.LBB4_575:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_earlyclobbers-8(,%rsi,8), %rcx
	cmpq	%r10, %rcx
	movq	$1, %rcx
	je	.LBB4_576
# BB#572:                               # %for.cond.i
                                        #   in Loop: Header=BB4_575 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB4_575
# BB#573:
	xorq	%rcx, %rcx
	jmp	.LBB4_576
.LBB4_571:
	xorq	%rcx, %rcx
.LBB4_576:                              # %earlyclobber_operand_p.exit
	movq	%rcx, 16(%rsp)
	movq	%r12, 8(%rsp)
	movq	%r8, (%rsp)
	movq	%r11, %rdi
	movq	%r10, %rsi
	movq	%r9, %rcx
	movq	136(%rsp), %r8          # 8-byte Reload
	movq	128(%rsp), %r9          # 8-byte Reload
	movq	%r11, %r13
	movq	%r10, %r14
	callq	find_dummy_reload
	movq	%r14, %r10
	movq	%r13, %r11
	cmpq	%r10, %rax
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rax, (%rsi)
	jne	.LBB4_584
# BB#577:                               # %land.lhs.true1662
	movzbl	(%r11), %ecx
	cmpq	$133, %rcx
	jg	.LBB4_580
# BB#578:                               # %land.lhs.true1662
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB4_584
# BB#579:                               # %land.lhs.true1662
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24727, %rax            # imm = 0x6097
	jne	.LBB4_582
	jmp	.LBB4_584
.LBB4_580:                              # %land.lhs.true1662
	cmpq	$134, %rcx
	je	.LBB4_582
# BB#581:                               # %land.lhs.true1662
	cmpq	$140, %rcx
	jne	.LBB4_584
.LBB4_582:                              # %land.lhs.true1707
	movq	this_insn(%rip), %rsi
	movq	static_reload_reg_p(%rip), %r8
	movl	8(%r10), %ecx
	xorq	%rdx, %rdx
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r11, %rdi
	movq	%r12, %r9
	movq	%r11, %r13
	movq	%r10, %r14
	callq	find_equiv_reg
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %r10
	movq	%r13, %r11
	testq	%rax, %rax
	je	.LBB4_584
# BB#583:                               # %if.then1714
	movq	%r10, rld(%rbx)
.LBB4_584:                              # %if.end1719
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	cmpq	$0, (%rsi)
	movq	%r15, %r9
	jne	.LBB4_676
# BB#585:                               # %if.end1719
	testq	%r11, %r11
	je	.LBB4_676
# BB#586:                               # %if.then1728
	testq	%r10, %r10
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 120(%rsp)         # 8-byte Spill
	je	.LBB4_589
# BB#587:                               # %land.lhs.true1731
	movq	$32, %rcx
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %eax
	movq	$32, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	ja	.LBB4_589
# BB#588:                               # %select.mid129
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 120(%rsp)         # 8-byte Spill
.LBB4_589:                              # %if.end1741
	movq	this_insn(%rip), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB4_676
# BB#590:                               # %for.body1747.lr.ph
	cmpq	%r10, %r11
	movabsq	$0, %rax
	jne	.LBB4_592
# BB#591:                               # %for.body1747.lr.ph
	movabsq	$1, %rax
.LBB4_592:                              # %for.body1747.lr.ph
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	testq	%r10, %r10
	movabsq	$0, %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	jne	.LBB4_594
# BB#593:                               # %for.body1747.lr.ph
	movabsq	$1, %rcx
.LBB4_594:                              # %for.body1747.lr.ph
	orq	%rax, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	136(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	160(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB4_595:                              # %for.body1747
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_670 Depth 2
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB4_675
# BB#596:                               # %land.lhs.true1753
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	8(%rbx), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB4_675
# BB#597:                               # %land.lhs.true1761
                                        #   in Loop: Header=BB4_595 Depth=1
	movslq	8(%rdi), %rax
	cmpq	$52, %rax
	jg	.LBB4_675
# BB#598:                               # %land.lhs.true1770
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r11, %rsi
	movq	%r9, %r15
	movq	%rax, %r12
	movq	%r11, %r13
	movq	%r10, %r14
	callq	reg_mentioned_p
	movq	%r14, %r10
	movq	%r13, %r11
	movq	%r12, %r13
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	%r15, %r9
	testq	%r9, %rax
	je	.LBB4_675
# BB#599:                               # %land.lhs.true1776
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r13, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	movabsq	$0, %rax
	jne	.LBB4_601
# BB#600:                               # %land.lhs.true1776
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rax
.LBB4_601:                              # %land.lhs.true1776
                                        #   in Loop: Header=BB4_595 Depth=1
	leaq	-21(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	leaq	-45(%r13), %rax
	andq	%r9, %rax
	cmpq	$8, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rcx, %rax
	leaq	-29(%r13), %rcx
	andq	%r9, %rcx
	cmpq	$8, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	je	.LBB4_606
# BB#602:                               # %cond.true1800
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r10, %r14
	movq	%r9, %r15
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB4_611
# BB#603:                               # %lor.rhs1805
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rsi
	jne	.LBB4_605
# BB#604:                               # %lor.rhs1805
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rsi
.LBB4_605:                              # %lor.rhs1805
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rsi
	jmp	.LBB4_611
.LBB4_606:                              # %cond.false1812
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB4_607
# BB#609:                               # %cond.true1822
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r10, %r14
	movq	%r9, %r15
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB4_611
.LBB4_607:                              # %cond.false1812
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r9, %r15
	movq	%r10, %r14
	cmpq	$18, %rax
	jne	.LBB4_610
# BB#608:                               # %cond.true1815
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB4_611
.LBB4_610:                              # %cond.false1826
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB4_611:                              # %cond.end1843
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rdx
	addq	%r13, %rsi
	movq	%r13, %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	movq	%r15, %r9
	testq	%r9, %rax
	movq	%r14, %r10
	movq	168(%rsp), %r11         # 8-byte Reload
	jne	.LBB4_675
# BB#612:                               # %land.lhs.true1851
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	jne	.LBB4_624
# BB#613:                               # %lor.lhs.false1857
                                        #   in Loop: Header=BB4_595 Depth=1
	testq	$1, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB4_618
# BB#614:                               # %cond.true1881
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r10, %r14
	movq	%r9, %r15
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB4_623
# BB#615:                               # %lor.rhs1886
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rsi
	jne	.LBB4_617
# BB#616:                               # %lor.rhs1886
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rsi
.LBB4_617:                              # %lor.rhs1886
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rsi
	jmp	.LBB4_623
.LBB4_618:                              # %cond.false1893
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB4_619
# BB#621:                               # %cond.true1903
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r10, %r14
	movq	%r9, %r15
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB4_623
.LBB4_619:                              # %cond.false1893
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r9, %r15
	movq	%r10, %r14
	cmpq	$18, %rax
	jne	.LBB4_622
# BB#620:                               # %cond.true1896
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB4_623
.LBB4_622:                              # %cond.false1907
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	104(%rsp), %rax         # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB4_623:                              # %cond.end1924
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rdx
	addq	%r13, %rsi
	movq	%r13, %rdi
	callq	hard_reg_set_here_p
	movq	%r15, %r9
	testq	%r9, %rax
	movq	%r14, %r10
	movq	168(%rsp), %r11         # 8-byte Reload
	jne	.LBB4_675
.LBB4_624:                              # %land.lhs.true1932
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	%r10, %r11
	jne	.LBB4_627
# BB#625:                               # %lor.lhs.false1935
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	(%r11), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB4_675
# BB#626:                               # %land.lhs.true1940
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	4(%rsi), %rdi
	leaq	3(%rsi,%rax), %rax
	cqto
	idivq	%rdi
	movq	%rax, %rcx
	movq	8(%r11), %rax
	movzbl	2(%rax), %eax
	movzbl	mode_size(%rax), %eax
	leaq	3(%rax,%rsi), %rax
	cqto
	idivq	%rdi
	andq	%r9, %rcx
	andq	%r9, %rax
	cmpq	%rax, %rcx
	jne	.LBB4_675
.LBB4_627:                              # %land.lhs.true1976
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	8(%rbx), %rax
	movzbl	2(%rax), %eax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rcx
	ja	.LBB4_675
# BB#628:                               # %land.lhs.true1991
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	%r13, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r15
	movq	%r13, %r12
	movq	%r11, %r13
	movq	%r10, %r14
	callq	ix86_hard_regno_mode_ok
	movq	%r14, %r10
	movq	%r13, %r11
	movq	%r12, %rdi
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	%r15, %r9
	testq	%r9, %rax
	je	.LBB4_675
# BB#629:                               # %land.lhs.true1994
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %r15
	movq	%rdi, %r12
	movq	%r11, %r13
	movq	%r10, %r14
	callq	ix86_hard_regno_mode_ok
	movq	%r14, %r10
	movq	%r13, %r11
	movq	%r12, %r14
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	%r15, %r9
	testq	%r9, %rax
	je	.LBB4_675
# BB#630:                               # %if.then1997
                                        #   in Loop: Header=BB4_595 Depth=1
	testq	$1, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB4_639
# BB#631:                               # %cond.true2022
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	$2, %rsi
	je	.LBB4_635
# BB#632:                               # %lor.rhs2027
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rsi
	jne	.LBB4_634
# BB#633:                               # %lor.rhs2027
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rsi
.LBB4_634:                              # %lor.rhs2027
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rsi
.LBB4_635:                              # %cond.true2090
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	je	.LBB4_668
# BB#636:                               # %lor.rhs2095
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rdi
	jne	.LBB4_638
# BB#637:                               # %lor.rhs2095
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rdi
.LBB4_638:                              # %lor.rhs2095
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rdi
	jmp	.LBB4_649
.LBB4_639:                              # %cond.false2034
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB4_640
# BB#643:                               # %cond.true2044
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %edi
	movq	$24, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB4_644
.LBB4_640:                              # %cond.false2034
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$18, %rax
	jne	.LBB4_642
# BB#641:                               # %cond.end2065.thread3046
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB4_644
.LBB4_642:                              # %cond.false2048
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %edi
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB4_644:                              # %cond.false2102
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB4_645
# BB#647:                               # %cond.true2112
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	$24, %rcx
	shrq	%cl, %rdi
	andq	$2, %rdi
	xorq	$6, %rdi
	jmp	.LBB4_649
.LBB4_645:                              # %cond.false2102
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$18, %rax
	jne	.LBB4_648
# BB#646:                               # %cond.true2105
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	xorq	$3, %rdi
	jmp	.LBB4_649
.LBB4_648:                              # %cond.false2116
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	movq	%rax, %rdi
.LBB4_649:                              # %cond.end2133
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	jle	.LBB4_659
# BB#650:                               # %cond.true2137
                                        #   in Loop: Header=BB4_595 Depth=1
	testq	$1, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB4_655
# BB#651:                               # %cond.true2161
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB4_668
# BB#652:                               # %lor.rhs2166
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB4_654
# BB#653:                               # %lor.rhs2166
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rax
.LBB4_654:                              # %lor.rhs2166
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rax
	movq	$1, %rsi
	jmp	.LBB4_670
.LBB4_659:                              # %cond.false2206
                                        #   in Loop: Header=BB4_595 Depth=1
	testq	$1, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB4_664
# BB#660:                               # %cond.true2230
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB4_668
# BB#661:                               # %lor.rhs2235
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB4_663
# BB#662:                               # %lor.rhs2235
                                        #   in Loop: Header=BB4_595 Depth=1
	movabsq	$1, %rax
.LBB4_663:                              # %lor.rhs2235
                                        #   in Loop: Header=BB4_595 Depth=1
	incq	%rax
	movq	$1, %rsi
	jmp	.LBB4_670
.LBB4_655:                              # %cond.false2173
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	je	.LBB4_679
# BB#656:                               # %cond.false2173
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$18, %rax
	je	.LBB4_657
# BB#658:                               # %cond.false2187
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB4_667
.LBB4_664:                              # %cond.false2242
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	andq	%r9, %rax
	cmpq	$24, %rax
	jne	.LBB4_665
.LBB4_679:                              # %cond.true2183
                                        #   in Loop: Header=BB4_595 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	movq	$1, %rsi
	jmp	.LBB4_670
.LBB4_665:                              # %cond.false2242
                                        #   in Loop: Header=BB4_595 Depth=1
	cmpq	$18, %rax
	jne	.LBB4_666
.LBB4_657:                              # %cond.true2176
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	movq	$1, %rsi
	jmp	.LBB4_670
.LBB4_666:                              # %cond.false2256
                                        #   in Loop: Header=BB4_595 Depth=1
	movl	target_flags(%rip), %esi
	movq	64(%rsp), %rax          # 8-byte Reload
.LBB4_667:                              # %cond.end2275
                                        #   in Loop: Header=BB4_595 Depth=1
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	xorq	%rcx, %rcx
	testq	%r9, %rax
	movabsq	$0, %rdx
	je	.LBB4_673
.LBB4_668:                              # %for.body2280.lr.ph
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	$1, %rsi
.LBB4_670:                              # %for.body2280
                                        #   Parent Loop BB4_595 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rsi,%r14), %rcx
	andq	%r9, %rcx
	movzbl	fixed_regs(%rcx), %edi
	leaq	-1(%rsi), %rdx
	testq	%rdi, %rdi
	jne	.LBB4_672
# BB#671:                               # %lor.lhs.false2286
                                        #   in Loop: Header=BB4_670 Depth=2
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	reg_class_contents(,%rdi,8), %r8
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %r8
	je	.LBB4_672
# BB#669:                               # %for.cond2277
                                        #   in Loop: Header=BB4_670 Depth=2
	movq	%rsi, %rcx
	andq	%r9, %rcx
	movq	%rsi, %rdx
	movq	%rax, %rdi
	andq	%r9, %rdi
	incq	%rsi
	cmpq	%rdi, %rcx
	jb	.LBB4_670
.LBB4_672:                              #   in Loop: Header=BB4_595 Depth=1
	movq	%rax, %rcx
.LBB4_673:                              # %for.end2298
                                        #   in Loop: Header=BB4_595 Depth=1
	andq	%r9, %rcx
	andq	%r9, %rdx
	cmpq	%rcx, %rdx
	je	.LBB4_674
	.align	16, 0x90
.LBB4_675:                              # %for.inc2308
                                        #   in Loop: Header=BB4_595 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_595
	jmp	.LBB4_676
.LBB4_674:                              # %if.then2301
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r10, %rbx
	callq	gen_rtx_REG
	movq	%rbx, %r10
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
.LBB4_676:                              # %if.end2313
	testq	%r10, %r10
	je	.LBB4_678
# BB#677:                               # %if.then2315
	movl	%r12d, output_reloadnum(%rip)
.LBB4_678:                              # %if.end2316
	movq	%r12, %rax
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_430:
	movl	reload_out_optab+8(%rip), %eax
	movq	$1317, %rsi             # imm = 0x525
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r8
	movq	152(%rsp), %r10         # 8-byte Reload
	jmp	.LBB4_407
.Ltmp59:
	.size	push_reload, .Ltmp59-push_reload
	.cfi_endproc

	.align	16, 0x90
	.type	reload_inner_reg_of_subreg,@function
reload_inner_reg_of_subreg:             # @reload_inner_reg_of_subreg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp64:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 32
.Ltmp67:
	.cfi_offset %rbx, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %ecx
	movq	%rsi, %rbx
	xorq	%rax, %rax
	cmpq	$63, %rcx
	jne	.LBB5_29
# BB#1:                                 # %if.end
	movq	8(%rdi), %r15
	movzbl	(%r15), %ecx
	cmpq	$133, %rcx
	movq	$1, %rax
	jg	.LBB5_16
# BB#2:                                 # %if.end
	addq	$-54, %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$21, %rcx
	ja	.LBB5_18
# BB#3:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$2121751, %rdx          # imm = 0x206017
	jne	.LBB5_29
# BB#4:                                 # %if.end
	cmpq	$7, %rcx
	jne	.LBB5_18
# BB#5:                                 # %lor.lhs.false40
	movl	8(%r15), %eax
	cmpq	$53, %rax
	jae	.LBB5_18
# BB#6:                                 # %if.end45
	callq	subreg_regno
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r14, %rax
	movq	$1, %rax
	je	.LBB5_29
# BB#7:                                 # %if.end48
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rbx), %edx
	movq	$23, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	4(%rdi), %rbx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rbx, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	ja	.LBB5_29
# BB#8:                                 # %land.lhs.true
	movzbl	2(%r15), %ecx
	movzbl	mode_size(%rcx), %r9d
	movq	%rbx, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %r9
	jbe	.LBB5_29
# BB#9:                                 # %land.rhs
	xorq	%rdx, %rdx
	movq	%r9, %rax
	idivq	%rbx
	movq	%rax, %r8
	movl	8(%r15), %eax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	je	.LBB5_13
# BB#10:                                # %land.rhs
	leaq	-21(%rax), %rdx
	andq	%r14, %rdx
	cmpq	$8, %rdx
	jb	.LBB5_13
# BB#11:                                # %land.rhs
	leaq	-45(%rax), %rdx
	andq	%r14, %rdx
	cmpq	$8, %rdx
	jb	.LBB5_13
# BB#12:                                # %land.rhs
	addq	$-29, %rax
	andq	%r14, %rax
	cmpq	$7, %rax
	jbe	.LBB5_13
# BB#21:                                # %cond.false
	cmpq	$24, %rcx
	jne	.LBB5_22
# BB#24:                                # %cond.true149
	movq	$24, %rcx
	shrq	%cl, %rsi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB5_26
.LBB5_16:                               # %if.end
	cmpq	$134, %rcx
	je	.LBB5_29
# BB#17:                                # %if.end
	cmpq	$140, %rcx
	je	.LBB5_29
.LBB5_18:                               # %if.then44
	xorq	%rax, %rax
.LBB5_29:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB5_13:                               # %cond.true
	movl	mode_class(,%rcx,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB5_26
# BB#14:                                # %lor.rhs
	cmpq	$6, %rax
	jne	.LBB5_15
# BB#19:                                # %lor.rhs
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB5_26
.LBB5_22:                               # %cond.false
	cmpq	$18, %rcx
	jne	.LBB5_25
# BB#23:                                # %cond.true139
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB5_26
.LBB5_25:                               # %cond.false153
	orq	$3, %rdi
	addq	%r9, %rdi
	movq	%rdi, %rax
	cqto
	idivq	%rbx
	movq	%rax, %rsi
	jmp	.LBB5_26
.LBB5_15:
	movabsq	$0, %rsi
	incq	%rsi
.LBB5_26:                               # %cond.end170
	andq	%r14, %rsi
	andq	%r14, %r8
	cmpq	%rsi, %r8
	je	.LBB5_27
# BB#28:                                # %cond.end170
	movabsq	$1, %rax
	jmp	.LBB5_29
.LBB5_27:
	movabsq	$0, %rax
	jmp	.LBB5_29
.Ltmp70:
	.size	reload_inner_reg_of_subreg, .Ltmp70-reload_inner_reg_of_subreg
	.cfi_endproc

	.align	16, 0x90
	.type	find_valid_class,@function
find_valid_class:                       # @find_valid_class
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
	movq	%rsi, %r12
	movq	%rdi, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdx
	movl	regclass_map(,%rdx,4), %eax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	movq	$1, %rbx
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	$0, (%rsp)              # 8-byte Folded Spill
	.align	16, 0x90
.LBB6_1:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_2 Depth 2
	movq	$1, %r15
	.align	16, 0x90
.LBB6_2:                                # %for.body3
                                        #   Parent Loop BB6_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r15), %rcx
	movq	reg_class_contents(,%rbx,8), %rdx
	movq	$1, %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	je	.LBB6_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB6_2 Depth=2
	addq	%r12, %rcx
	movq	$1, %rax
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	je	.LBB6_6
# BB#4:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB6_2 Depth=2
	movq	%rcx, %rdi
	movq	%r14, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	movabsq	$0, %rax
	je	.LBB6_6
# BB#5:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB6_2 Depth=2
	movabsq	$1, %rax
	.align	16, 0x90
.LBB6_6:                                # %for.inc
                                        #   in Loop: Header=BB6_2 Depth=2
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$52, %rdx
	jg	.LBB6_8
# BB#7:                                 # %for.inc
                                        #   in Loop: Header=BB6_2 Depth=2
	incq	%r15
	testq	%rax, %rax
	jne	.LBB6_2
.LBB6_8:                                # %for.end
                                        #   in Loop: Header=BB6_1 Depth=1
	testq	%rax, %rax
	je	.LBB6_14
# BB#9:                                 # %if.end18
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	ix86_register_move_cost
	movl	reg_class_size(,%rbx,4), %r15d
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	cmpq	%rcx, %r15
	jbe	.LBB6_12
# BB#10:                                # %land.lhs.true23
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB6_13
# BB#11:                                # %land.lhs.true23
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB6_13
.LBB6_12:                               # %lor.lhs.false26
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jle	.LBB6_14
.LBB6_13:                               # %if.then28
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	ix86_register_move_cost
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	%r15, 16(%rsp)          # 8-byte Spill
.LBB6_14:                               # %for.inc33
                                        #   in Loop: Header=BB6_1 Depth=1
	incq	%rbx
	cmpq	$25, %rbx
	jne	.LBB6_1
# BB#15:                                # %for.end35
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB6_17
# BB#16:                                # %if.end38
	movq	(%rsp), %rax            # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_17:                               # %if.then37
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_valid_class, %rcx
	movq	$706, %rsi              # imm = 0x2C2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp88:
	.size	find_valid_class, .Ltmp88-find_valid_class
	.cfi_endproc

	.globl	reg_overlap_mentioned_for_reload_p
	.align	16, 0x90
	.type	reg_overlap_mentioned_for_reload_p,@function
reg_overlap_mentioned_for_reload_p:     # @reg_overlap_mentioned_for_reload_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp95:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp96:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp97:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp98:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp99:
	.cfi_def_cfa_offset 48
.Ltmp100:
	.cfi_offset %rbx, -48
.Ltmp101:
	.cfi_offset %r12, -40
.Ltmp102:
	.cfi_offset %r13, -32
.Ltmp103:
	.cfi_offset %r14, -24
.Ltmp104:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	(%rbx), %r8d
	movq	%rsi, %r14
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$64, %rax
	je	.LBB7_2
# BB#1:                                 # %lor.lhs.false
	movzbl	rtx_class(%rax), %eax
	cmpq	$97, %rax
	jne	.LBB7_3
.LBB7_2:                                # %if.then
	movq	8(%rbx), %rbx
	movl	(%rbx), %r8d
.LBB7_3:                                # %if.end
	movq	%r8, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	xorq	%rax, %rax
	cmpq	$133, %rdx
	jg	.LBB7_7
# BB#4:                                 # %if.end
	leaq	-54(%rdx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_9
# BB#5:                                 # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB7_6
	jmp	.LBB7_9
.LBB7_7:                                # %if.end
	cmpq	$134, %rdx
	je	.LBB7_6
# BB#8:                                 # %if.end
	cmpq	$140, %rdx
	je	.LBB7_6
.LBB7_9:                                # %lor.lhs.false45
	movzbl	(%r14), %esi
	cmpq	$133, %rsi
	jg	.LBB7_12
# BB#10:                                # %lor.lhs.false45
	addq	$-54, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB7_14
# BB#11:                                # %lor.lhs.false45
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB7_6
	jmp	.LBB7_14
.LBB7_12:                               # %lor.lhs.false45
	cmpq	$134, %rsi
	je	.LBB7_6
# BB#13:                                # %lor.lhs.false45
	cmpq	$140, %rsi
	je	.LBB7_6
.LBB7_14:                               # %if.else
	addq	$-59, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rdx
	cmpq	$16, %rdx
	ja	.LBB7_27
# BB#15:                                # %if.else
	jmpq	*.LJTI7_0(,%rdx,8)
.LBB7_22:                               # %if.then163
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	reg_mentioned_p  # TAILCALL
.LBB7_18:                               # %if.then122
	movslq	8(%rbx), %rdi
	cmpq	$53, %rdi
	jl	.LBB7_30
# BB#19:                                # %if.then128
	movq	reg_equiv_memory_loc(%rip), %rcx
	cmpq	$0, (%rcx,%rdi,8)
	je	.LBB7_20
.LBB7_45:                               # %if.then131
	movq	%r14, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	refers_to_mem_for_reload_p  # TAILCALL
.LBB7_16:                               # %if.then90
	movq	8(%rbx), %rax
	movslq	8(%rax), %r12
	cmpq	$53, %r12
	movq	$1, %r15
	jge	.LBB7_17
# BB#28:                                # %if.end184
	movl	16(%rbx), %edx
	movzbl	2(%rax), %esi
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%r12, %rdi
	movq	%r8, %rcx
	callq	subreg_regno_offset
	movq	%rax, %rdi
	addq	%r12, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$52, %rax
	jg	.LBB7_44
# BB#29:                                # %if.end184.cond.true_crit_edge
	movl	(%rbx), %r8d
.LBB7_30:                               # %cond.true
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	cmpq	$8, %rax
	je	.LBB7_34
# BB#31:                                # %cond.true
	leaq	-21(%rdi), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB7_34
# BB#32:                                # %cond.true
	leaq	-45(%rdi), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB7_34
# BB#33:                                # %cond.true
	leaq	-29(%rdi), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB7_34
# BB#39:                                # %cond.false
	cmpq	$24, %r8
	jne	.LBB7_40
# BB#42:                                # %cond.true241
	movzbl	target_flags+3(%rip), %r15d
	andq	$2, %r15
	xorq	$6, %r15
	jmp	.LBB7_44
.LBB7_23:                               # %if.then170
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r13, %rax
	movq	$1, %rax
	jne	.LBB7_6
# BB#24:                                # %lor.rhs
	movq	16(%rbx), %rdi
	movq	%r14, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r13, %rax
	je	.LBB7_25
# BB#26:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB7_6
.LBB7_17:
	movq	%r12, %rdi
	jmp	.LBB7_44
.LBB7_27:                               # %if.else181
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.reg_overlap_mentioned_for_reload_p, %rdx
	movq	$6255, %rsi             # imm = 0x186F
	callq	fancy_abort
.LBB7_34:                               # %cond.true209
	movl	mode_class(,%r8,4), %eax
	cmpq	$5, %rax
	movq	$2, %r15
	je	.LBB7_44
# BB#35:                                # %lor.rhs217
	cmpq	$6, %rax
	jne	.LBB7_36
# BB#37:                                # %lor.rhs217
	movabsq	$1, %r15
	incq	%r15
	jmp	.LBB7_44
.LBB7_20:                               # %if.else133
	movq	reg_equiv_constant(%rip), %rcx
	cmpq	$0, (%rcx,%rdi,8)
	jne	.LBB7_6
# BB#21:                                # %if.end139
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.reg_overlap_mentioned_for_reload_p, %rcx
	movq	$6243, %rsi             # imm = 0x1863
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB7_25:
	movabsq	$0, %rax
.LBB7_6:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_40:                               # %cond.false
	cmpq	$18, %r8
	jne	.LBB7_43
# BB#41:                                # %cond.true232
	movl	target_flags(%rip), %r15d
	movq	$25, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	xorq	$3, %r15
	jmp	.LBB7_44
.LBB7_36:
	movabsq	$0, %r15
	incq	%r15
	jmp	.LBB7_44
.LBB7_43:                               # %cond.false245
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r15
.LBB7_44:                               # %cond.end265
	addq	%rdi, %r15
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	refers_to_regno_for_reload_p  # TAILCALL
.Ltmp105:
	.size	reg_overlap_mentioned_for_reload_p, .Ltmp105-reg_overlap_mentioned_for_reload_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI7_0:
	.quad	.LBB7_22
	.quad	.LBB7_27
	.quad	.LBB7_18
	.quad	.LBB7_22
	.quad	.LBB7_16
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_45
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_22
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_27
	.quad	.LBB7_23

	.text
	.globl	remove_address_replacements
	.align	16, 0x90
	.type	remove_address_replacements,@function
remove_address_replacements:            # @remove_address_replacements
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp112:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp113:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp114:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp115:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp116:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp117:
	.cfi_def_cfa_offset 240
.Ltmp118:
	.cfi_offset %rbx, -48
.Ltmp119:
	.cfi_offset %r12, -40
.Ltmp120:
	.cfi_offset %r13, -32
.Ltmp121:
	.cfi_offset %r14, -24
.Ltmp122:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	leaq	(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$180, %rdx
	callq	memset
	movslq	n_replacements(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %r14
	jle	.LBB8_6
# BB#1:
	movq	$1, %r12
	movabsq	$replacements, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r14, %r14
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdi
	movq	%r15, %rsi
	callq	loc_mentioned_in_p
	testq	%r13, %rax
	jne	.LBB8_3
# BB#4:                                 # %if.else
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	$32, %rcx
	leaq	1(%r14), %rax
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	16(%rbx), %rdx
	sarq	%cl, %r14
	leaq	(%r14,%r14,2), %rcx
	movq	%rdx, replacements+16(,%rcx,8)
	movq	(%rbx), %rdx
	movq	8(%rbx), %rsi
	movq	%rsi, replacements+8(,%rcx,8)
	movq	%rdx, replacements(,%rcx,8)
	movslq	16(%rbx), %rcx
	movzbl	(%rsp,%rcx), %edx
	orq	$2, %rdx
	movb	%dl, (%rsp,%rcx)
	movq	%rax, %r14
	jmp	.LBB8_5
	.align	16, 0x90
.LBB8_3:                                # %if.then
                                        #   in Loop: Header=BB8_2 Depth=1
	movslq	16(%rbx), %rax
	movzbl	(%rsp,%rax), %ecx
	orq	$1, %rcx
	movb	%cl, (%rsp,%rax)
.LBB8_5:                                # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	movslq	n_replacements(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$24, %rbx
	cmpq	%rax, %rdx
	jl	.LBB8_2
.LBB8_6:                                # %for.end
	movl	n_reloads(%rip), %ebx
	movl	%r14d, n_replacements(%rip)
	xorq	%rax, %rax
	jmp	.LBB8_7
	.align	16, 0x90
.LBB8_10:                               # %if.then28
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	%rbx, %rdi
	callq	deallocate_reload_reg
	movq	(%r14), %rdi
	callq	remove_address_replacements
	movq	$0, (%r14)
	movq	$1, %rax
.LBB8_7:                                # %for.cond19.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_8 Depth 2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$104, %rbx, %rcx
	leaq	rld(%rcx), %r14
	.align	16, 0x90
.LBB8_8:                                # %for.cond19
                                        #   Parent Loop BB8_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB8_11
# BB#9:                                 # %for.body22
                                        #   in Loop: Header=BB8_8 Depth=2
	movzbl	-1(%rsp,%rbx), %ecx
	decq	%rbx
	addq	$-104, %r14
	cmpq	$1, %rcx
	jne	.LBB8_8
	jmp	.LBB8_10
.LBB8_11:                               # %for.end37
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp123:
	.size	remove_address_replacements, .Ltmp123-remove_address_replacements
	.cfi_endproc

	.align	16, 0x90
	.type	find_dummy_reload,@function
find_dummy_reload:                      # @find_dummy_reload
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp130:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp131:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp132:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp133:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp135:
	.cfi_def_cfa_offset 144
.Ltmp136:
	.cfi_offset %rbx, -48
.Ltmp137:
	.cfi_offset %r12, -40
.Ltmp138:
	.cfi_offset %r13, -32
.Ltmp139:
	.cfi_offset %r14, -24
.Ltmp140:
	.cfi_offset %r15, -16
	movq	%rcx, %r11
	movq	$32, %rcx
	movq	%r9, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movzbl	mode_size(%r14), %r15d
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	mode_size(%rbx), %r10d
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	cmpq	%r10, %r15
	je	.LBB9_3
# BB#1:                                 # %land.lhs.true
	movl	target_flags(%rip), %eax
	movq	$23, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	addq	$4, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	xorq	%rdx, %rdx
	cmpq	%rcx, %r15
	ja	.LBB9_95
# BB#2:                                 # %land.lhs.true
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %r10
	ja	.LBB9_95
.LBB9_3:                                # %while.cond.preheader
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	%r11, 56(%rsp)          # 8-byte Spill
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%r10, 24(%rsp)          # 8-byte Spill
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	xorq	%r12, %r12
	cmpq	$63, %rcx
	movq	%rsi, %rbx
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movabsq	$0, %r13
	jne	.LBB9_8
# BB#4:
	movq	%rdi, %r15
	movq	80(%rsp), %rbx          # 8-byte Reload
	xorq	%r13, %r13
	.align	16, 0x90
.LBB9_5:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB9_6
# BB#10:                                # %land.lhs.true25
                                        #   in Loop: Header=BB9_5 Depth=1
	movl	8(%rcx), %edi
	cmpq	$52, %rdi
	ja	.LBB9_11
# BB#12:                                # %if.then33
                                        #   in Loop: Header=BB9_5 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%rbx), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movq	8(%rbx), %rbx
	movl	(%rbx), %esi
	addq	%rax, %r13
	jmp	.LBB9_13
	.align	16, 0x90
.LBB9_6:                                #   in Loop: Header=BB9_5 Depth=1
	movq	%rcx, %rbx
	jmp	.LBB9_13
	.align	16, 0x90
.LBB9_11:                               #   in Loop: Header=BB9_5 Depth=1
	movq	%rcx, %rbx
.LBB9_13:                               # %if.end51
                                        #   in Loop: Header=BB9_5 Depth=1
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	movq	%rsi, %rax
	je	.LBB9_5
# BB#7:                                 # %while.cond.while.cond55.preheader_crit_edge
	movq	%r15, %rdi
.LBB9_8:                                # %while.cond55.preheader
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rdi, %r15
	cmpq	$63, %rcx
	jne	.LBB9_9
# BB#14:
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB9_15:                               # %while.body60
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB9_16
# BB#17:                                # %land.lhs.true68
                                        #   in Loop: Header=BB9_15 Depth=1
	movl	8(%rcx), %edi
	cmpq	$52, %rdi
	ja	.LBB9_18
# BB#19:                                # %if.then77
                                        #   in Loop: Header=BB9_15 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r15), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movq	8(%r15), %r15
	movl	(%r15), %esi
	addq	%rax, %r12
	jmp	.LBB9_20
	.align	16, 0x90
.LBB9_16:                               #   in Loop: Header=BB9_15 Depth=1
	movq	%rcx, %r15
	jmp	.LBB9_20
	.align	16, 0x90
.LBB9_18:                               #   in Loop: Header=BB9_15 Depth=1
	movq	%rcx, %r15
.LBB9_20:                               # %if.end98
                                        #   in Loop: Header=BB9_15 Depth=1
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	movq	%rsi, %rax
	je	.LBB9_15
	jmp	.LBB9_21
.LBB9_9:
	movq	%rdi, 64(%rsp)          # 8-byte Spill
.LBB9_21:                               # %while.end102
	movq	144(%rsp), %rsi
	movq	%r15, %rdi
	callq	ix86_preferred_reload_class
	movzbl	(%rbx), %ecx
	xorq	%rdx, %rdx
	cmpq	$61, %rcx
	jne	.LBB9_50
# BB#22:                                # %land.lhs.true108
	movl	8(%rbx), %esi
	cmpq	$52, %rsi
	ja	.LBB9_50
# BB#23:                                # %if.then114
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	(%rsi,%r13), %rdi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB9_27
# BB#24:                                # %if.then114
	leaq	-21(%rsi,%r13), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB9_27
# BB#25:                                # %if.then114
	leaq	-45(%rsi,%r13), %rdx
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	jb	.LBB9_27
# BB#26:                                # %if.then114
	leaq	-29(%rsi,%r13), %rax
	andq	%rcx, %rax
	cmpq	$7, %rax
	jbe	.LBB9_27
# BB#32:                                # %cond.false
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$24, %rcx
	jne	.LBB9_33
# BB#35:                                # %cond.true160
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB9_37
.LBB9_27:                               # %cond.true
	movl	mode_class(,%r14,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB9_37
# BB#28:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB9_29
# BB#30:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB9_37
.LBB9_33:                               # %cond.false
	cmpq	$18, %rcx
	jne	.LBB9_36
# BB#34:                                # %cond.true153
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB9_37
.LBB9_29:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB9_37
.LBB9_36:                               # %cond.false164
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB9_37:                               # %cond.end178
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %r13
	movq	const_int_rtx+512(%rip), %rdx
	movq	%rdx, (%rcx)
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rdi, %rcx
	andq	%r8, %rcx
	xorq	%rdx, %rdx
	cmpq	$52, %rcx
	ja	.LBB9_49
# BB#38:                                # %land.lhs.true182
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%r8, %r13
	movq	%rdi, %r14
	callq	ix86_hard_regno_mode_ok
	testq	%r13, %rax
	je	.LBB9_39
# BB#40:                                # %land.lhs.true185
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rdx
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	(%rax,%r14), %rsi
	movq	%r14, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rdx
	movq	48(%rsp), %r13          # 8-byte Reload
	jne	.LBB9_49
# BB#41:                                # %for.cond.preheader
	movq	%r13, %r11
	xorq	%rdx, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rax, %rsi
	movabsq	$0, %rcx
	movq	%r14, %r9
	movq	%rsi, %r10
	movq	%rax, %r14
	je	.LBB9_46
# BB#42:                                # %for.body.lr.ph
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	reg_class_contents(,%rax,8), %r8
	movq	$1, %rax
	movq	%r10, %rsi
	andq	%r14, %rsi
	.align	16, 0x90
.LBB9_44:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax,%r9), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %r8
	je	.LBB9_45
# BB#43:                                # %for.cond
                                        #   in Loop: Header=BB9_44 Depth=1
	movq	%rax, %rdi
	andq	%r14, %rdi
	movq	%rax, %rcx
	incq	%rax
	cmpq	%rsi, %rdi
	jb	.LBB9_44
	jmp	.LBB9_46
.LBB9_39:
	xorq	%rdx, %rdx
	movq	48(%rsp), %r13          # 8-byte Reload
	jmp	.LBB9_49
.LBB9_45:
	decq	%rax
	movq	%rax, %rcx
.LBB9_46:                               # %for.end
	movq	%r9, %rsi
	andq	%r14, %r10
	andq	%r14, %rcx
	cmpq	%r10, %rcx
	movq	%r11, %r13
	jne	.LBB9_49
# BB#47:                                # %if.then204
	movq	80(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	movq	%rcx, %rdx
	je	.LBB9_49
# BB#48:                                # %if.else
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	gen_rtx_REG
	movq	%rax, %rdx
.LBB9_49:                               # %if.end213
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%r13, (%rax)
	movq	16(%rsp), %rax          # 8-byte Reload
.LBB9_50:                               # %if.end214
	movl	hard_regs_live_known(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB9_95
# BB#51:                                # %land.lhs.true216
	movzbl	(%r15), %ecx
	cmpq	$61, %rcx
	jne	.LBB9_95
# BB#52:                                # %land.lhs.true221
	movq	%rax, %r14
	movl	8(%r15), %eax
	cmpq	$52, %rax
	ja	.LBB9_95
# BB#53:                                # %land.lhs.true227
	testq	%rdx, %rdx
	movq	%rdx, %r13
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB9_56
# BB#54:                                # %lor.lhs.false230
	movq	this_insn(%rip), %rdi
	movq	$10, %rsi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB9_55
.LBB9_56:                               # %land.lhs.true233
	movq	this_insn(%rip), %rdi
	movq	$1, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB9_57
# BB#58:                                # %land.lhs.true236
	movl	8(%r15), %edi
	movzbl	fixed_regs(%rdi), %eax
	testq	%rax, %rax
	je	.LBB9_60
# BB#59:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_57:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_55:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_60:                               # %land.lhs.true243
	movzbl	2(%rbx), %eax
	testq	%rax, %rax
	je	.LBB9_62
# BB#61:                                # %land.lhs.true243
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB9_62:                               # %land.lhs.true243
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	ix86_hard_regno_mode_ok
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB9_63
# BB#64:                                # %if.then261
	movl	8(%r15), %eax
	leaq	(%rax,%r12), %rdi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB9_68
# BB#65:                                # %if.then261
	leaq	-21(%rax,%r12), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB9_68
# BB#66:                                # %if.then261
	leaq	-45(%rax,%r12), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB9_68
# BB#67:                                # %if.then261
	leaq	-29(%rax,%r12), %rax
	andq	%rdx, %rax
	cmpq	$7, %rax
	jbe	.LBB9_68
# BB#73:                                # %cond.false303
	movq	40(%rsp), %rax          # 8-byte Reload
	andq	%rdx, %rax
	cmpq	$24, %rax
	jne	.LBB9_74
# BB#76:                                # %cond.true313
	movzbl	target_flags+3(%rip), %r12d
	andq	$2, %r12
	xorq	$6, %r12
	jmp	.LBB9_78
.LBB9_63:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_68:                               # %cond.true291
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %r12
	je	.LBB9_78
# BB#69:                                # %lor.rhs296
	cmpq	$6, %rax
	jne	.LBB9_70
# BB#71:                                # %lor.rhs296
	movabsq	$1, %r12
	incq	%r12
	jmp	.LBB9_78
.LBB9_74:                               # %cond.false303
	cmpq	$18, %rax
	jne	.LBB9_77
# BB#75:                                # %cond.true306
	movl	target_flags(%rip), %r12d
	movq	$25, %rcx
	shrq	%cl, %r12
	andq	$1, %r12
	xorq	$3, %r12
	jmp	.LBB9_78
.LBB9_70:
	movabsq	$0, %r12
	incq	%r12
	jmp	.LBB9_78
.LBB9_77:                               # %cond.false317
	movl	target_flags(%rip), %esi
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %r12
.LBB9_78:                               # %cond.end334
	leaq	(%r12,%rdi), %r15
	xorq	%rcx, %rcx
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%rdi, %rbx
	callq	refers_to_regno_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_80
# BB#79:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_80:                               # %land.lhs.true339
	movq	this_insn(%rip), %rax
	movq	32(%rax), %rdx
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	hard_reg_set_here_p
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	je	.LBB9_82
# BB#81:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_82:                               # %land.lhs.true346
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	160(%rsp), %rax
	testq	%r9, %rax
	je	.LBB9_85
# BB#83:                                # %lor.lhs.false348
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%r9, %rbx
	callq	refers_to_regno_for_reload_p
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB9_85
# BB#84:
	movq	%r13, %rdx
	jmp	.LBB9_95
.LBB9_85:                               # %for.cond357.preheader
	xorq	%rcx, %rcx
	testq	%r9, %r12
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB9_90
# BB#86:                                # %for.body360.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	reg_class_contents(,%r14,8), %rdx
	movq	$1, %rax
	movq	%r12, %rsi
	andq	%r9, %rsi
.LBB9_88:                               # %for.body360
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rax,%rbx), %rcx
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%rdi, %rdx
	je	.LBB9_89
# BB#87:                                # %for.cond357
                                        #   in Loop: Header=BB9_88 Depth=1
	movq	%rax, %rdi
	andq	%r9, %rdi
	movq	%rax, %rcx
	incq	%rax
	cmpq	%rsi, %rdi
	jb	.LBB9_88
	jmp	.LBB9_90
.LBB9_89:
	decq	%rax
	movq	%rax, %rcx
.LBB9_90:                               # %for.end372
	andq	%r9, %r12
	andq	%r9, %rcx
	cmpq	%r12, %rcx
	movq	%r13, %rdx
	jne	.LBB9_95
# BB#91:                                # %if.then375
	movq	$32, %rcx
	movq	152(%rsp), %rdi
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	js	.LBB9_94
# BB#92:                                # %if.then375
	cmpq	%r8, %rdx
	jne	.LBB9_94
# BB#93:                                # %if.then381
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	imulq	$104, %rdi, %rax
	movq	$0, rld+8(%rax)
.LBB9_94:                               # %if.end385
	movq	64(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$61, %rax
	movq	%rcx, %rdx
	movq	40(%rsp), %rdi          # 8-byte Reload
	jne	.LBB9_96
.LBB9_95:                               # %return
	movq	%rdx, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_96:                               # %if.else391
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	gen_rtx_REG  # TAILCALL
.Ltmp141:
	.size	find_dummy_reload, .Ltmp141-find_dummy_reload
	.cfi_endproc

	.globl	earlyclobber_operand_p
	.align	16, 0x90
	.type	earlyclobber_operand_p,@function
earlyclobber_operand_p:                 # @earlyclobber_operand_p
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_earlyclobbers(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB10_1
# BB#4:
	movq	$1, %rsi
	.align	16, 0x90
.LBB10_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_earlyclobbers-8(,%rsi,8), %rax
	cmpq	%rdi, %rax
	movq	$1, %rax
	je	.LBB10_6
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB10_5 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%rdx, %rax
	jl	.LBB10_5
# BB#3:
	xorq	%rax, %rax
	retq
.LBB10_1:
	xorq	%rax, %rax
	retq
.LBB10_6:                               # %return
	retq
.Ltmp142:
	.size	earlyclobber_operand_p, .Ltmp142-earlyclobber_operand_p
	.cfi_endproc

	.globl	find_equiv_reg
	.align	16, 0x90
	.type	find_equiv_reg,@function
find_equiv_reg:                         # @find_equiv_reg
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
	subq	$144, %rsp
.Ltmp154:
	.cfi_def_cfa_offset 192
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
	movq	%r8, %r10
	movq	%r10, 136(%rsp)         # 8-byte Spill
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB11_1
# BB#3:                                 # %if.else
	movl	(%rdi), %esi
	movq	%rsi, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	$1, %r14
	xorq	%r9, %r9
	cmpq	$133, %rbx
	jg	.LBB11_7
# BB#4:                                 # %if.else
	leaq	-54(%rbx), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$21, %rcx
	jbe	.LBB11_6
# BB#5:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_1:
	movq	%r9, 120(%rsp)          # 8-byte Spill
.LBB11_2:                               # %while.body.preheader
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	xorq	%r14, %r14
	xorq	%r9, %r9
	jmp	.LBB11_34
.LBB11_7:                               # %if.else
	movq	%rax, 120(%rsp)         # 8-byte Spill
	cmpq	$134, %rbx
	jne	.LBB11_9
# BB#8:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB11_34
.LBB11_6:                               # %if.else
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	xorq	%rax, %rax
	jmpq	*.LJTI11_0(,%rcx,8)
.LBB11_10:                              # %if.then2
	movl	8(%rdi), %eax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	jmp	.LBB11_2
.LBB11_9:                               # %if.else
	cmpq	$140, %rbx
	movabsq	$0, %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB11_33:                              # %land.lhs.true130
	movabsq	$0, %rax
	jne	.LBB11_388
	jmp	.LBB11_34
.LBB11_11:                              # %if.then7
	xorq	%rax, %rax
	testq	$134217728, %rsi        # imm = 0x8000000
	jne	.LBB11_388
# BB#12:                                # %if.end
	movq	8(%rdi), %rcx
	movl	flag_float_store(%rip), %ebx
	movzbl	(%rcx), %eax
	testq	%rbx, %rbx
	je	.LBB11_17
# BB#13:                                # %land.lhs.true
	movq	$14, %rcx
	shrq	%cl, %rsi
	andq	$1020, %rsi             # imm = 0x3FC
	movl	mode_class(%rsi), %ecx
	cmpq	$2, %rcx
	je	.LBB11_14
# BB#15:                                # %land.lhs.true
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	addq	$-96, %rax
	movq	$1, %r9
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$6, %rcx
	movabsq	$0, %r14
	movq	%r8, 120(%rsp)          # 8-byte Spill
	jmp	.LBB11_16
.LBB11_18:                              # %land.lhs.true59
	movq	8(%rdi), %rcx
	movq	global_rtl+16(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB11_26
# BB#19:                                # %land.lhs.true64
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %esi
	cmpq	$133, %rsi
	jg	.LBB11_22
# BB#20:                                # %land.lhs.true64
	addq	$-54, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	ja	.LBB11_25
# BB#21:                                # %land.lhs.true64
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB11_23
	jmp	.LBB11_25
.LBB11_22:                              # %land.lhs.true64
	cmpq	$134, %rsi
	je	.LBB11_23
# BB#24:                                # %land.lhs.true64
	cmpq	$140, %rsi
	jne	.LBB11_25
.LBB11_23:
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	movq	$1, %r14
	movq	%r8, 120(%rsp)          # 8-byte Spill
	jmp	.LBB11_34
.LBB11_25:                              # %if.else121
	xorq	%rax, %rax
	cmpq	$75, %rbx
	jne	.LBB11_388
.LBB11_26:                              # %land.lhs.true125
	movq	8(%rdi), %rcx
	movq	global_rtl+24(%rip), %rsi
	xorq	%rax, %rax
	cmpq	%rsi, %rcx
	jne	.LBB11_388
# BB#27:                                # %land.lhs.true130
	movq	16(%rdi), %rcx
	movzbl	(%rcx), %eax
	cmpq	$133, %rax
	jg	.LBB11_30
# BB#28:                                # %land.lhs.true130
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	jbe	.LBB11_387
# BB#29:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_17:                              # %if.end22
	addq	$-96, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	andq	%rcx, %rax
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	cmpq	$6, %rax
	movq	$1, %r9
	movabsq	$0, %r14
.LBB11_16:                              # %land.lhs.true
	movabsq	$0, %rax
	jae	.LBB11_34
	jmp	.LBB11_388
.LBB11_30:                              # %land.lhs.true130
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	cmpq	$134, %rax
	jne	.LBB11_32
# BB#31:
	xorq	%r9, %r9
	movq	%r8, 120(%rsp)          # 8-byte Spill
	jmp	.LBB11_34
.LBB11_387:                             # %land.lhs.true130
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	movabsq	$0, %r9
	movq	%r8, 120(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	je	.LBB11_388
.LBB11_34:                              # %while.body.preheader
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %r12
	testq	%r12, %r12
	movabsq	$0, %rax
	je	.LBB11_388
# BB#35:                                # %lor.lhs.false198.lr.ph
	movq	%r9, 80(%rsp)           # 8-byte Spill
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	192(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB11_36:                              # %lor.lhs.false198
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_107 Depth 2
	movzbl	(%r12), %eax
	cmpq	$36, %rax
	je	.LBB11_37
# BB#38:                                # %lor.lhs.false198
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$32, %rax
	jne	.LBB11_91
# BB#39:                                # %land.lhs.true207
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$2, %r10
	jae	.LBB11_40
# BB#42:                                # %if.then215
                                        #   in Loop: Header=BB11_36 Depth=1
	movzbl	rtx_class+32(%rip), %eax
	jmp	.LBB11_43
	.align	16, 0x90
.LBB11_40:                              # %lor.lhs.false211
                                        #   in Loop: Header=BB11_36 Depth=1
	movslq	8(%r12), %rax
	movslq	reload_first_uid(%rip), %rcx
	cmpq	%rcx, %rax
	jge	.LBB11_91
# BB#41:                                # %lor.lhs.false211
                                        #   in Loop: Header=BB11_36 Depth=1
	movzbl	rtx_class+32(%rip), %eax
	andq	$255, %rax
.LBB11_43:                              # %if.then215
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$105, %rax
	jne	.LBB11_91
# BB#44:                                # %cond.true
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	32(%r12), %r15
	movzbl	(%r15), %eax
	cmpq	$47, %rax
	je	.LBB11_46
# BB#45:                                # %cond.false
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	single_set_2
	movq	%rax, %r15
.LBB11_46:                              # %cond.end237
                                        #   in Loop: Header=BB11_36 Depth=1
	testq	%r15, %r15
	je	.LBB11_47
# BB#48:                                # %land.lhs.true241
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB11_53
# BB#49:                                # %land.lhs.true244
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r15), %rdi
	callq	true_regnum
	andq	%r13, %rax
	movq	120(%rsp), %rbx         # 8-byte Reload
	andq	%r13, %rbx
	cmpq	%rbx, %rax
	jne	.LBB11_51
# BB#50:                                # %land.lhs.true251
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_84
.LBB11_51:                              # %land.lhs.true261
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	callq	true_regnum
	andq	%r13, %rax
	cmpq	%rbx, %rax
	jne	.LBB11_53
# BB#52:                                # %land.lhs.true268
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_84
.LBB11_53:                              # %lor.lhs.false275
                                        #   in Loop: Header=BB11_36 Depth=1
	testq	%r13, %r14
	je	.LBB11_57
# BB#54:                                # %land.lhs.true277
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r15), %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB11_57
# BB#55:                                # %land.lhs.true283
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r13, %rax
	jne	.LBB11_57
# BB#56:                                # %land.lhs.true289
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_84
.LBB11_57:                              # %lor.lhs.false296
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$0, 80(%rsp)            # 8-byte Folded Reload
	je	.LBB11_62
# BB#58:                                # %land.lhs.true298
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB11_60
# BB#59:                                # %land.lhs.true305
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r15), %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rbx
	callq	rtx_renumbered_equal_p
	movq	%rbx, %r11
	testq	%r13, %rax
	jne	.LBB11_84
.LBB11_60:                              # %land.lhs.true313
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB11_62
# BB#61:                                # %land.lhs.true320
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rsi
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rbx
	callq	rtx_renumbered_equal_p
	movq	%rbx, %r11
	testq	%r13, %rax
	jne	.LBB11_84
.LBB11_62:                              # %lor.lhs.false326
                                        #   in Loop: Header=BB11_36 Depth=1
	xorq	%rax, %rax
	testq	%r13, %r14
	movq	136(%rsp), %r10         # 8-byte Reload
	je	.LBB11_92
# BB#63:                                # %land.lhs.true328
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$0, 56(%r12)
	je	.LBB11_75
# BB#64:                                # %land.lhs.true334
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB11_75
# BB#65:                                # %land.lhs.true337
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%rbx), %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB11_67
# BB#66:                                # %land.lhs.true343
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_84
.LBB11_67:                              # %lor.lhs.false350
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rax
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB11_75
# BB#68:                                # %land.lhs.true358
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%rbx), %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB11_75
# BB#69:                                # %land.lhs.true366
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB11_75
# BB#70:                                # %land.lhs.true377
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB11_75
# BB#71:                                # %land.lhs.true382
                                        #   in Loop: Header=BB11_36 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	operand_subword
	testq	%rax, %rax
	je	.LBB11_75
# BB#72:                                # %land.lhs.true389
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB11_75
# BB#73:                                # %land.lhs.true392
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	operand_subword
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB11_75
# BB#74:                                # %land.lhs.true398
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB11_84
.LBB11_75:                              # %land.lhs.true404
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$3, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB11_47
# BB#76:                                # %land.lhs.true407
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	movq	136(%rsp), %r10         # 8-byte Reload
	jne	.LBB11_91
# BB#77:                                # %land.lhs.true415
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%rax), %rdi
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$55, %rcx
	jne	.LBB11_91
# BB#78:                                # %land.lhs.true423
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$14, %rcx
	shrq	%cl, %rax
	andq	$1020, %rax             # imm = 0x3FC
	movl	mode_class(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB11_91
# BB#79:                                # %land.lhs.true434
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB11_91
# BB#80:                                # %land.lhs.true439
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	operand_subword
	testq	%rax, %rax
	je	.LBB11_47
# BB#81:                                # %land.lhs.true446
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB11_47
# BB#82:                                # %land.lhs.true449
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	8(%r15), %rdi
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	operand_subword
	testq	%rax, %rax
	je	.LBB11_47
# BB#83:                                # %land.lhs.true455
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%rax, %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	true_regnum
	movq	%rax, %r11
	movq	$32, %rcx
                                        # kill: RAX<def> R11<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB11_47
.LBB11_84:                              # %if.then459
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	$32, %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB11_90
# BB#85:                                # %if.then462
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%rdx, %rax
	andq	%r13, %rax
	andq	%r13, %r11
	cmpq	%rax, %r11
	movq	136(%rsp), %r10         # 8-byte Reload
	jne	.LBB11_91
	jmp	.LBB11_86
.LBB11_47:                              #   in Loop: Header=BB11_36 Depth=1
	movq	%r14, %rax
	movq	136(%rsp), %r10         # 8-byte Reload
	jmp	.LBB11_92
.LBB11_90:                              # %if.else467
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%r11, %rax
	andq	%r13, %rax
	cmpq	$53, %rax
	movq	136(%rsp), %r10         # 8-byte Reload
	jae	.LBB11_91
# BB#94:                                # %if.else471
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	%r11, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB11_98
# BB#95:                                # %if.else471
                                        #   in Loop: Header=BB11_36 Depth=1
	leaq	-21(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_98
# BB#96:                                # %if.else471
                                        #   in Loop: Header=BB11_36 Depth=1
	leaq	-45(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_98
# BB#97:                                # %if.else471
                                        #   in Loop: Header=BB11_36 Depth=1
	leaq	-29(%r11), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB11_98
# BB#102:                               # %cond.false505
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	192(%rsp), %rax
	andq	%r13, %rax
	cmpq	$24, %rax
	jne	.LBB11_103
# BB#105:                               # %cond.true514
                                        #   in Loop: Header=BB11_36 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_107
.LBB11_98:                              # %cond.true495
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_107
# BB#99:                                # %lor.rhs
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB11_101
# BB#100:                               # %lor.rhs
                                        #   in Loop: Header=BB11_36 Depth=1
	movabsq	$1, %rax
.LBB11_101:                             # %lor.rhs
                                        #   in Loop: Header=BB11_36 Depth=1
	incq	%rax
	jmp	.LBB11_107
.LBB11_103:                             # %cond.false505
                                        #   in Loop: Header=BB11_36 Depth=1
	cmpq	$18, %rax
	jne	.LBB11_106
# BB#104:                               # %cond.true508
                                        #   in Loop: Header=BB11_36 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_107
.LBB11_106:                             # %cond.false518
                                        #   in Loop: Header=BB11_36 Depth=1
	movl	target_flags(%rip), %esi
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	.align	16, 0x90
.LBB11_107:                             # %for.cond
                                        #   Parent Loop BB11_36 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB11_87
# BB#108:                               # %for.body
                                        #   in Loop: Header=BB11_107 Depth=2
	decq	%rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	reg_class_contents(,%rcx,8), %rdx
	leaq	(%rax,%r11), %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	jne	.LBB11_107
	.align	16, 0x90
.LBB11_91:                              #   in Loop: Header=BB11_36 Depth=1
	movq	%r14, %rax
.LBB11_92:                              # %while.body.backedge
                                        #   in Loop: Header=BB11_36 Depth=1
	movq	16(%r12), %r12
	testq	%r12, %r12
	movq	%rax, %r14
	jne	.LBB11_36
# BB#93:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_37:
	xorq	%rax, %rax
.LBB11_388:                             # %return
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_86:
	movq	%rdx, %r11
.LBB11_87:                              # %if.end549
	cmpq	$0, 56(%r12)
	je	.LBB11_109
# BB#88:                                # %land.lhs.true557
	movq	$10, %rsi
	movq	%r12, %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%r11, %r15
	movq	%r10, %rbx
	callq	find_reg_note
	movq	%rbx, %r10
	movq	%r15, %r11
	testq	%rax, %rax
	je	.LBB11_109
# BB#89:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_109:                             # %if.end561
	movq	%r11, %rax
	andq	%r13, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	cmpq	$7, %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB11_114
# BB#110:                               # %if.end561
	movq	120(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$7, %rax
	je	.LBB11_114
# BB#111:                               # %lor.lhs.false567
	movq	%r13, %rbx
	testq	%rdx, %rdx
	je	.LBB11_112
# BB#113:                               # %land.lhs.true569
	movq	global_rtl+16(%rip), %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rdx, %r15
	movq	%r11, %r13
	movq	%r10, 136(%rsp)         # 8-byte Spill
	callq	reg_overlap_mentioned_for_reload_p
	movq	136(%rsp), %r10         # 8-byte Reload
	movq	%r13, %r11
	movq	%r15, %rdx
	movq	%rbx, %r13
	testq	%r13, %rax
	je	.LBB11_115
.LBB11_114:                             # %if.then572
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB11_115
.LBB11_112:
	movq	%rbx, %r13
.LBB11_115:                             # %if.end573
	movq	72(%rsp), %rsi          # 8-byte Reload
	movzbl	2(%rsi), %eax
	movq	192(%rsp), %rcx
	andq	%r13, %rcx
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	cmpq	%rcx, %rax
	jne	.LBB11_116
# BB#117:                               # %if.end580
	testq	%rdx, %rdx
	movq	96(%rsp), %r8           # 8-byte Reload
	je	.LBB11_136
# BB#118:                               # %land.lhs.true582
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB11_120
# BB#119:                               # %cond.false602
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%r8, %r15
	movq	%rdx, %rbx
	movq	%r11, %r13
	movq	%r10, 136(%rsp)         # 8-byte Spill
	callq	single_set_2
	movq	136(%rsp), %r10         # 8-byte Reload
	movq	%r13, %r11
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r15, %r8
.LBB11_120:                             # %cond.end610
	movq	8(%rax), %rax
	cmpq	%rax, %rsi
	jne	.LBB11_136
# BB#121:                               # %land.lhs.true617
	movq	%r11, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB11_125
# BB#122:                               # %land.lhs.true617
	leaq	-21(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_125
# BB#123:                               # %land.lhs.true617
	leaq	-45(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_125
# BB#124:                               # %land.lhs.true617
	leaq	-29(%r11), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB11_125
# BB#130:                               # %cond.false653
	movq	112(%rsp), %rax         # 8-byte Reload
	cmpq	$24, %rax
	jne	.LBB11_131
# BB#133:                               # %cond.true663
	movq	%r10, 136(%rsp)         # 8-byte Spill
	movq	%r13, %rbx
	movq	%rdx, %r15
	movzbl	target_flags+3(%rip), %esi
	andq	$2, %rsi
	xorq	$6, %rsi
	jmp	.LBB11_135
.LBB11_116:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_32:                              # %land.lhs.true130
	cmpq	$140, %rax
	movabsq	$0, %r9
	movq	%r8, 120(%rsp)          # 8-byte Spill
	jmp	.LBB11_33
.LBB11_125:                             # %cond.true641
	movq	%r10, 136(%rsp)         # 8-byte Spill
	movq	%r13, %rbx
	movq	%rdx, %r15
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rsi
	je	.LBB11_135
# BB#126:                               # %lor.rhs646
	cmpq	$6, %rax
	jne	.LBB11_127
# BB#128:                               # %lor.rhs646
	movabsq	$1, %rsi
	incq	%rsi
	jmp	.LBB11_135
.LBB11_14:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_131:                             # %cond.false653
	movq	%r13, %rbx
	cmpq	$18, %rax
	jne	.LBB11_134
# BB#132:                               # %cond.true656
	movq	%r10, 136(%rsp)         # 8-byte Spill
	movq	%rdx, %r15
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$3, %rsi
	jmp	.LBB11_135
.LBB11_127:
	movabsq	$0, %rsi
	incq	%rsi
	jmp	.LBB11_135
.LBB11_134:                             # %cond.false667
	movq	%r10, 136(%rsp)         # 8-byte Spill
	movq	%rdx, %r15
	movl	target_flags(%rip), %esi
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, %rsi
.LBB11_135:                             # %cond.end684
	addq	%r11, %rsi
	xorq	%rcx, %rcx
	movq	%r11, %rdi
	movq	%r8, %rdx
	movq	%r11, %r13
	callq	refers_to_regno_for_reload_p
	movq	%r13, %r11
	movq	%rbx, %r13
	testq	%r13, %rax
	movabsq	$0, %rax
	movq	%r15, %rdx
	movq	136(%rsp), %r10         # 8-byte Reload
	jne	.LBB11_388
.LBB11_136:                             # %if.end690
	orq	%rdx, %r14
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rsi
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rsi
	leaq	-21(%rax), %rdi
	leaq	-45(%rax), %r9
	leaq	-29(%rax), %r8
	testq	%r13, %r14
	je	.LBB11_145
# BB#137:                               # %if.end690.if.end839_crit_edge
	movq	112(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB11_138
.LBB11_145:                             # %land.lhs.true694
	cmpq	$8, %rsi
	movq	112(%rsp), %rbx         # 8-byte Reload
	je	.LBB11_149
# BB#146:                               # %land.lhs.true694
	movq	%rdi, %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_149
# BB#147:                               # %land.lhs.true694
	movq	%r9, %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_149
# BB#148:                               # %land.lhs.true694
	movq	%r8, %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB11_149
# BB#154:                               # %cond.false730
	cmpq	$24, %rbx
	jne	.LBB11_155
# BB#157:                               # %cond.true740
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_159
.LBB11_149:                             # %cond.true718
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_159
# BB#150:                               # %lor.rhs723
	cmpq	$6, %rcx
	jne	.LBB11_151
# BB#152:                               # %lor.rhs723
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB11_159
.LBB11_155:                             # %cond.false730
	cmpq	$18, %rbx
	jne	.LBB11_158
# BB#156:                               # %cond.true733
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_159
.LBB11_151:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB11_159
.LBB11_158:                             # %cond.false744
	movq	%rbx, %r15
	movq	%r10, %r14
	movq	%r13, %r10
	movl	target_flags(%rip), %ebx
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
                                        # kill: R13<def> R10<kill>
	movq	%r14, %r10
	movq	%r15, %rbx
.LBB11_159:                             # %cond.end761
	movq	120(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jle	.LBB11_138
# BB#160:                               # %land.lhs.true766
	movq	%r11, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB11_164
# BB#161:                               # %land.lhs.true766
	leaq	-21(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_164
# BB#162:                               # %land.lhs.true766
	leaq	-45(%r11), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_164
# BB#163:                               # %land.lhs.true766
	leaq	-29(%r11), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB11_164
# BB#169:                               # %cond.false802
	cmpq	$24, %rbx
	jne	.LBB11_170
# BB#172:                               # %cond.true812
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_174
.LBB11_164:                             # %cond.true790
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_174
# BB#165:                               # %lor.rhs795
	cmpq	$6, %rcx
	jne	.LBB11_166
# BB#167:                               # %lor.rhs795
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB11_174
.LBB11_170:                             # %cond.false802
	cmpq	$18, %rbx
	jne	.LBB11_173
# BB#171:                               # %cond.true805
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_174
.LBB11_166:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB11_174
.LBB11_173:                             # %cond.false816
	movq	%rbx, %r15
	movq	%r10, %r14
	movq	%r13, %r10
	movl	target_flags(%rip), %ebx
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
                                        # kill: R13<def> R10<kill>
	movq	%r14, %r10
	movq	%r15, %rbx
.LBB11_174:                             # %cond.end833
	addq	%r11, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB11_138
# BB#175:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_138:                             # %if.end839
	cmpq	$8, %rsi
	je	.LBB11_142
# BB#139:                               # %if.end839
	andq	%r13, %rdi
	cmpq	$8, %rdi
	jb	.LBB11_142
# BB#140:                               # %if.end839
	andq	%r13, %r9
	cmpq	$8, %r9
	jb	.LBB11_142
# BB#141:                               # %if.end839
	andq	%r13, %r8
	cmpq	$7, %r8
	jbe	.LBB11_142
# BB#178:                               # %cond.false875
	cmpq	$24, %rbx
	jne	.LBB11_179
# BB#181:                               # %cond.true885
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_183
.LBB11_142:                             # %cond.true863
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_183
# BB#143:                               # %lor.rhs868
	cmpq	$6, %rcx
	jne	.LBB11_144
# BB#176:                               # %lor.rhs868
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB11_183
.LBB11_179:                             # %cond.false875
	cmpq	$18, %rbx
	jne	.LBB11_182
# BB#180:                               # %cond.true878
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_183
.LBB11_182:                             # %cond.false889
	movl	target_flags(%rip), %esi
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	jmp	.LBB11_183
.LBB11_144:
	movabsq	$0, %rax
	incq	%rax
.LBB11_183:                             # %cond.end906
	movq	%r11, %rdx
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rdx
	cmpq	$8, %rdx
	je	.LBB11_187
# BB#184:                               # %cond.end906
	leaq	-21(%r11), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_187
# BB#185:                               # %cond.end906
	leaq	-45(%r11), %rcx
	andq	%r13, %rcx
	cmpq	$8, %rcx
	jb	.LBB11_187
# BB#186:                               # %cond.end906
	leaq	-29(%r11), %rcx
	andq	%r13, %rcx
	cmpq	$7, %rcx
	jbe	.LBB11_187
# BB#192:                               # %cond.false943
	movq	%rax, 48(%rsp)          # 8-byte Spill
	cmpq	$24, %rbx
	jne	.LBB11_193
# BB#195:                               # %cond.true953
	movq	%r11, 56(%rsp)          # 8-byte Spill
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_197
.LBB11_187:                             # %cond.true931
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r11, 56(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_197
# BB#188:                               # %lor.rhs936
	cmpq	$6, %rcx
	jne	.LBB11_189
# BB#190:                               # %lor.rhs936
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB11_197
.LBB11_193:                             # %cond.false943
	movq	%r11, 56(%rsp)          # 8-byte Spill
	cmpq	$18, %rbx
	jne	.LBB11_196
# BB#194:                               # %cond.true946
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_197
.LBB11_189:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB11_197
.LBB11_196:                             # %cond.false957
	movl	target_flags(%rip), %esi
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB11_197:                             # %cond.end974
	movq	%rax, %r15
	movq	%r13, %r8
	testq	%r10, %r10
	je	.LBB11_224
# BB#198:                               # %cond.end974
	cmpq	$1, %r10
	je	.LBB11_205
# BB#199:                               # %for.cond983.preheader
	movq	$32, %rcx
	movq	%r15, %rbx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB11_204
# BB#200:                               # %for.body986.lr.ph
	movq	$32, %rcx
	movq	56(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r10,%rax,2), %rax
	movq	$1, %rdx
.LBB11_202:                             # %for.body986
                                        # =>This Inner Loop Header: Depth=1
	movswq	-2(%rax,%rdx,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB11_203
# BB#201:                               # %for.cond983
                                        #   in Loop: Header=BB11_202 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB11_202
.LBB11_204:                             # %if.end997
	movq	%rbx, %r15
	testq	%r10, %r10
	je	.LBB11_224
.LBB11_205:                             # %for.cond1002.preheader
	movslq	n_reloads(%rip), %r14
	testq	%r14, %r14
	jle	.LBB11_224
# BB#206:                               # %for.body1005.lr.ph
	movq	56(%rsp), %rax          # 8-byte Reload
	leaq	(%r15,%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movl	target_flags(%rip), %r10d
	movq	$23, %rcx
	movq	%r10, %r9
	shrq	%cl, %r9
	andq	$4, %r9
	addq	$4, %r9
	movq	$25, %rcx
	movq	%r10, %r11
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	movq	$24, %rcx
	shrq	%cl, %r10
	andq	$2, %r10
	xorq	$6, %r10
	movq	$1, %rdi
	movabsq	$rld+64, %rsi
.LBB11_207:                             # %for.body1005
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rax
	testq	%rax, %rax
	je	.LBB11_223
# BB#208:                               # %land.lhs.true1010
                                        #   in Loop: Header=BB11_207 Depth=1
	cmpq	$0, -64(%rsi)
	je	.LBB11_223
# BB#209:                               # %if.then1014
                                        #   in Loop: Header=BB11_207 Depth=1
	movl	8(%rax), %ebx
	movzbl	2(%rax), %ecx
	movq	%rbx, %rax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdx, %rax
	cmpq	$8, %rax
	je	.LBB11_213
# BB#210:                               # %if.then1014
                                        #   in Loop: Header=BB11_207 Depth=1
	leaq	-21(%rbx), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB11_213
# BB#211:                               # %if.then1014
                                        #   in Loop: Header=BB11_207 Depth=1
	leaq	-45(%rbx), %rax
	andq	%r8, %rax
	cmpq	$8, %rax
	jb	.LBB11_213
# BB#212:                               # %if.then1014
                                        #   in Loop: Header=BB11_207 Depth=1
	leaq	-29(%rbx), %rax
	andq	%r8, %rax
	cmpq	$7, %rax
	jbe	.LBB11_213
# BB#217:                               # %cond.false1068
                                        #   in Loop: Header=BB11_207 Depth=1
	cmpq	$18, %rcx
	movq	%r11, %rax
	je	.LBB11_220
# BB#218:                               # %cond.false1068
                                        #   in Loop: Header=BB11_207 Depth=1
	cmpq	$24, %rcx
	movq	%r10, %rax
	je	.LBB11_220
# BB#219:                               # %cond.false1094
                                        #   in Loop: Header=BB11_207 Depth=1
	movzbl	mode_size(%rcx), %eax
	leaq	-1(%rax,%r9), %rax
	cqto
	idivq	%r9
	jmp	.LBB11_220
.LBB11_213:                             # %cond.true1044
                                        #   in Loop: Header=BB11_207 Depth=1
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_220
# BB#214:                               # %lor.rhs1055
                                        #   in Loop: Header=BB11_207 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB11_216
# BB#215:                               # %lor.rhs1055
                                        #   in Loop: Header=BB11_207 Depth=1
	movabsq	$1, %rax
.LBB11_216:                             # %lor.rhs1055
                                        #   in Loop: Header=BB11_207 Depth=1
	incq	%rax
.LBB11_220:                             # %cond.end1117
                                        #   in Loop: Header=BB11_207 Depth=1
	movq	$32, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	%rdx, %r13
	jge	.LBB11_223
# BB#221:                               # %cond.end1117
                                        #   in Loop: Header=BB11_207 Depth=1
	addq	%rbx, %rax
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB11_222
.LBB11_223:                             # %for.inc1129
                                        #   in Loop: Header=BB11_207 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rdi
	addq	$104, %rsi
	cmpq	%r14, %rax
	jl	.LBB11_207
.LBB11_224:                             # %if.end1132
	xorq	%rdx, %rdx
	movq	80(%rsp), %r9           # 8-byte Reload
	testq	%r9, %r9
	movq	96(%rsp), %rax          # 8-byte Reload
	je	.LBB11_227
# BB#225:                               # %if.then1134
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	addq	$-54, %rcx
	movq	%rcx, %rax
	andq	%r8, %rax
	cmpq	$14, %rax
	movq	$1, %rdx
	ja	.LBB11_227
# BB#226:                               # %switch.lookup
	andq	$32767, %rcx            # imm = 0x7FFF
	movq	$8172, %rdx             # imm = 0x1FEC
	shrq	%cl, %rdx
	andq	$1, %rdx
.LBB11_227:                             # %if.end1175
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %r14
	cmpq	%r12, %r14
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	48(%rsp), %r10          # 8-byte Reload
	movq	%r15, %r11
	movq	%r11, 32(%rsp)          # 8-byte Spill
	je	.LBB11_228
# BB#229:                               # %if.end1183.lr.ph
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	88(%rsp), %rax          # 8-byte Reload
	orq	%r9, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rdx         # 8-byte Reload
	leaq	(%r10,%rdx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	leaq	(%r11,%rsi), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	andq	%r8, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%r8, %r13
.LBB11_230:                             # %if.end1183
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_241 Depth 2
                                        #     Child Loop BB11_244 Depth 2
                                        #     Child Loop BB11_251 Depth 2
                                        #     Child Loop BB11_298 Depth 2
                                        #       Child Loop BB11_304 Depth 3
                                        #     Child Loop BB11_355 Depth 2
	movzbl	(%r14), %r8d
	cmpq	$34, %r8
	jne	.LBB11_237
# BB#231:                               # %if.then1188
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB11_232
# BB#233:                               # %if.end1194
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$52, 64(%rsp)           # 8-byte Folded Reload
	ja	.LBB11_235
# BB#234:                               # %if.end1194
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$1, %rdx
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB11_235
.LBB11_241:                             # %for.body1204
                                        #   Parent Loop BB11_230 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs-1(%rbx,%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB11_242
# BB#240:                               # %for.cond1201
                                        #   in Loop: Header=BB11_241 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB11_241
.LBB11_235:                             # %if.end1214
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$52, 104(%rsp)          # 8-byte Folded Reload
	ja	.LBB11_237
# BB#236:                               # %if.end1214
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$1, %rdx
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB11_237
.LBB11_244:                             # %for.body1224
                                        #   Parent Loop BB11_230 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	call_used_regs-1(%rax,%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB11_245
# BB#243:                               # %for.cond1221
                                        #   in Loop: Header=BB11_244 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	cmpq	%rdi, %rsi
	jl	.LBB11_244
.LBB11_237:                             # %if.end1235
                                        #   in Loop: Header=BB11_230 Depth=1
	movzbl	rtx_class(%r8), %eax
	cmpq	$105, %rax
	jne	.LBB11_350
# BB#238:                               # %if.then1243
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r9, %rbx
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	32(%r14), %r14
	movq	%r14, %rdi
	callq	volatile_insn_p
	testq	%r13, %rax
	jne	.LBB11_239
# BB#246:                               # %if.end1250
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$38, %rcx
	movq	%rbx, %r9
	jne	.LBB11_248
# BB#247:                               # %if.then1255
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	16(%r14), %r14
	movl	(%r14), %eax
.LBB11_248:                             # %if.end1259
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r13, %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$39, %rax
	je	.LBB11_297
# BB#249:                               # %if.end1259
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$47, %rax
	je	.LBB11_251
# BB#250:                               # %if.end1259
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$49, %rax
	jne	.LBB11_349
.LBB11_251:                             # %while.cond1273
                                        #   Parent Loop BB11_230 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r14), %r14
	movl	(%r14), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_251
# BB#252:                               # %while.cond1273
                                        #   in Loop: Header=BB11_251 Depth=2
	leaq	-132(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_251
# BB#253:                               # %while.cond1273
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$61, %rax
	jne	.LBB11_284
# BB#254:                               # %if.then1303
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	8(%r14), %r15d
	cmpq	$52, %r15
	movq	$1, %rax
	ja	.LBB11_268
# BB#255:                               # %if.then1312
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r15, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$8, %rax
	je	.LBB11_259
# BB#256:                               # %if.then1312
                                        #   in Loop: Header=BB11_230 Depth=1
	leaq	-21(%r15), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB11_259
# BB#257:                               # %if.then1312
                                        #   in Loop: Header=BB11_230 Depth=1
	leaq	-45(%r15), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB11_259
# BB#258:                               # %if.then1312
                                        #   in Loop: Header=BB11_230 Depth=1
	leaq	-29(%r15), %rax
	andq	%rbx, %rax
	cmpq	$7, %rax
	jbe	.LBB11_259
# BB#263:                               # %cond.false1354
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$24, %rsi
	jne	.LBB11_264
# BB#266:                               # %cond.true1370
                                        #   in Loop: Header=BB11_230 Depth=1
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_268
.LBB11_297:                             # %if.then1474
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	8(%r14), %rax
	movslq	(%rax), %r10
	testq	%r10, %r10
	jle	.LBB11_349
	jmp	.LBB11_298
.LBB11_303:                             # %for.cond1479.backedge.for.body1482_crit_edge
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	8(%r14), %rax
.LBB11_298:                             # %for.body1482
                                        #   Parent Loop BB11_230 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB11_304 Depth 3
	movq	(%rax,%r10,8), %r15
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$38, %rcx
	jne	.LBB11_300
# BB#299:                               # %if.then1492
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	16(%r15), %r15
	movl	(%r15), %eax
.LBB11_300:                             # %if.end1496
                                        #   in Loop: Header=BB11_298 Depth=2
	decq	%r10
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$47, %rax
	je	.LBB11_304
# BB#301:                               # %if.end1496
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$49, %rax
	jne	.LBB11_302
	.align	16, 0x90
.LBB11_304:                             # %while.cond1511
                                        #   Parent Loop BB11_230 Depth=1
                                        #     Parent Loop BB11_298 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%r15), %r15
	movl	(%r15), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	leaq	-63(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_304
# BB#305:                               # %while.cond1511
                                        #   in Loop: Header=BB11_304 Depth=3
	leaq	-132(%rax), %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_304
# BB#306:                               # %while.cond1511
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$61, %rax
	jne	.LBB11_337
# BB#307:                               # %if.then1541
                                        #   in Loop: Header=BB11_298 Depth=2
	movl	8(%r15), %edi
	cmpq	$52, %rdi
	movq	$1, %rax
	movq	96(%rsp), %r8           # 8-byte Reload
	ja	.LBB11_321
# BB#308:                               # %if.then1552
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	%rdi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$8, %rax
	je	.LBB11_312
# BB#309:                               # %if.then1552
                                        #   in Loop: Header=BB11_298 Depth=2
	leaq	-21(%rdi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB11_312
# BB#310:                               # %if.then1552
                                        #   in Loop: Header=BB11_298 Depth=2
	leaq	-45(%rdi), %rax
	andq	%rbx, %rax
	cmpq	$8, %rax
	jb	.LBB11_312
# BB#311:                               # %if.then1552
                                        #   in Loop: Header=BB11_298 Depth=2
	leaq	-29(%rdi), %rax
	andq	%rbx, %rax
	cmpq	$7, %rax
	jbe	.LBB11_312
# BB#316:                               # %cond.false1594
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$24, %rsi
	jne	.LBB11_317
# BB#319:                               # %cond.true1610
                                        #   in Loop: Header=BB11_298 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_321
.LBB11_337:                             # %if.else1668
                                        #   in Loop: Header=BB11_298 Depth=2
	testq	%r9, %r9
	je	.LBB11_342
# BB#338:                               # %if.else1668
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$66, %rax
	jne	.LBB11_342
# BB#339:                               # %land.lhs.true1675
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	%r10, (%rsp)            # 8-byte Spill
	movq	%r9, %r13
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdi
	callq	push_operand
	testq	%rbx, %rax
	je	.LBB11_340
# BB#341:                               # %land.lhs.true1675.if.else1682_crit_edge
                                        #   in Loop: Header=BB11_298 Depth=2
	movl	(%r15), %esi
	movq	%r13, %r9
	movq	(%rsp), %r10            # 8-byte Reload
.LBB11_342:                             # %if.else1682
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB11_346
# BB#343:                               # %if.else1682
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB11_346
# BB#344:                               # %land.lhs.true1690
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	cmpq	$0, (%rax,%rcx,8)
	jne	.LBB11_345
.LBB11_346:                             # %if.else1696
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB11_302
# BB#347:                               # %land.lhs.true1698
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r15, %rdi
	movq	%rbx, %r15
	movq	%r9, %r13
	movq	%r10, %rbx
	callq	push_operand
	movq	%rbx, %r10
	movq	%r13, %r9
	movq	%r15, %rbx
	testq	%rbx, %rax
	je	.LBB11_302
	jmp	.LBB11_348
.LBB11_312:                             # %cond.true1576
                                        #   in Loop: Header=BB11_298 Depth=2
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_321
# BB#313:                               # %lor.rhs1584
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB11_315
# BB#314:                               # %lor.rhs1584
                                        #   in Loop: Header=BB11_298 Depth=2
	movabsq	$1, %rax
.LBB11_315:                             # %lor.rhs1584
                                        #   in Loop: Header=BB11_298 Depth=2
	incq	%rax
	jmp	.LBB11_321
.LBB11_317:                             # %cond.false1594
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$18, %rsi
	jne	.LBB11_320
# BB#318:                               # %cond.true1600
                                        #   in Loop: Header=BB11_298 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_321
.LBB11_320:                             # %cond.false1614
                                        #   in Loop: Header=BB11_298 Depth=2
	movl	target_flags(%rip), %r11d
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %r11
	andq	$4, %r11
	leaq	3(%rax,%r11), %rax
	addq	$4, %r11
	cqto
	idivq	%r11
.LBB11_321:                             # %if.end1637
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdi, %rax
	cmpq	%rdx, %rsi
	jge	.LBB11_324
# BB#322:                               # %if.end1637
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB11_323
.LBB11_324:                             # %if.end1646
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB11_327
# BB#325:                               # %if.end1646
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB11_326
.LBB11_327:                             # %if.end1655
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	%rdi, %r13
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB11_328
# BB#334:                               # %if.end1661
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$7, %r13
	jne	.LBB11_302
# BB#335:                               # %if.end1661
                                        #   in Loop: Header=BB11_298 Depth=2
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB11_302
	jmp	.LBB11_336
.LBB11_328:                             # %land.lhs.true1657
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	%r10, (%rsp)            # 8-byte Spill
	movq	%r15, %rdi
	movq	%r8, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	cmpq	$7, %r13
	movabsq	$0, %rcx
	jne	.LBB11_330
# BB#329:                               # %land.lhs.true1657
                                        #   in Loop: Header=BB11_298 Depth=2
	movabsq	$1, %rcx
.LBB11_330:                             # %land.lhs.true1657
                                        #   in Loop: Header=BB11_298 Depth=2
	testq	%rbx, %rax
	movq	80(%rsp), %r9           # 8-byte Reload
	movq	(%rsp), %r10            # 8-byte Reload
	jne	.LBB11_331
# BB#332:                               # %land.lhs.true1657
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rcx
	jne	.LBB11_333
.LBB11_302:                             # %for.cond1479.backedge
                                        #   in Loop: Header=BB11_298 Depth=2
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB11_303
	jmp	.LBB11_349
.LBB11_284:                             # %if.else1428
                                        #   in Loop: Header=BB11_230 Depth=1
	testq	%r9, %r9
	je	.LBB11_290
# BB#285:                               # %if.else1428
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%rbx, %r15
	cmpq	$66, %rax
	jne	.LBB11_289
# BB#286:                               # %land.lhs.true1435
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r9, %rbx
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rdi
	callq	push_operand
	testq	%r15, %rax
	je	.LBB11_287
# BB#288:                               # %land.lhs.true1435.if.else1442_crit_edge
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	(%r14), %esi
	movq	%rbx, %r9
.LBB11_289:                             # %if.else1442
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r15, %rbx
.LBB11_290:                             # %if.else1442
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	jne	.LBB11_294
# BB#291:                               # %if.else1442
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$53, %rax
	jl	.LBB11_294
# BB#292:                               # %land.lhs.true1450
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%rdx,8)
	jne	.LBB11_293
.LBB11_294:                             # %if.else1456
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB11_349
# BB#295:                               # %land.lhs.true1458
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rdi
	movq	%rbx, %r14
	movq	%r9, %r15
	callq	push_operand
	movq	%r15, %r9
	movq	%r14, %rbx
	testq	%rbx, %rax
	je	.LBB11_349
	jmp	.LBB11_296
.LBB11_259:                             # %cond.true1336
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_268
# BB#260:                               # %lor.rhs1344
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%rbx, %rsi
	movq	%r9, %rdx
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB11_262
# BB#261:                               # %lor.rhs1344
                                        #   in Loop: Header=BB11_230 Depth=1
	movabsq	$1, %rax
.LBB11_262:                             # %lor.rhs1344
                                        #   in Loop: Header=BB11_230 Depth=1
	incq	%rax
	movq	%rdx, %r9
	movq	%rsi, %rbx
	jmp	.LBB11_268
.LBB11_264:                             # %cond.false1354
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$18, %rsi
	jne	.LBB11_267
# BB#265:                               # %cond.true1360
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_268
.LBB11_267:                             # %cond.false1374
                                        #   in Loop: Header=BB11_230 Depth=1
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB11_268:                             # %if.end1397
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%r15, %rax
	cmpq	%rdx, %rsi
	jge	.LBB11_271
# BB#269:                               # %if.end1397
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB11_270
.LBB11_271:                             # %if.end1406
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jge	.LBB11_274
# BB#272:                               # %if.end1406
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB11_273
.LBB11_274:                             # %if.end1415
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB11_275
# BB#281:                               # %if.end1421
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$7, %r15
	jne	.LBB11_349
# BB#282:                               # %if.end1421
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB11_349
	jmp	.LBB11_283
.LBB11_275:                             # %land.lhs.true1417
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%r9, %r13
	movq	%r14, %rdi
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	reg_overlap_mentioned_for_reload_p
	cmpq	$7, %r15
	movabsq	$0, %rcx
	jne	.LBB11_277
# BB#276:                               # %land.lhs.true1417
                                        #   in Loop: Header=BB11_230 Depth=1
	movabsq	$1, %rcx
.LBB11_277:                             # %land.lhs.true1417
                                        #   in Loop: Header=BB11_230 Depth=1
	testq	%rbx, %rax
	jne	.LBB11_278
# BB#279:                               # %land.lhs.true1417
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rcx
	movq	%r13, %r9
	jne	.LBB11_280
.LBB11_349:                             # %if.end1714
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	%rbx, %r13
	movq	40(%rsp), %r14          # 8-byte Reload
	movzbl	(%r14), %eax
	cmpq	$34, %rax
	movq	48(%rsp), %r10          # 8-byte Reload
	movq	32(%rsp), %r11          # 8-byte Reload
	jne	.LBB11_350
# BB#352:                               # %land.lhs.true1719
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	64(%r14), %rax
	testq	%rax, %rax
	je	.LBB11_350
# BB#353:                               # %for.cond1728.preheader
                                        #   in Loop: Header=BB11_230 Depth=1
	cmpq	$0, 16(%rax)
	je	.LBB11_350
# BB#354:                               #   in Loop: Header=BB11_230 Depth=1
	leaq	16(%rax), %r15
.LBB11_355:                             # %for.body1734
                                        #   Parent Loop BB11_230 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$49, %rcx
	jne	.LBB11_386
# BB#356:                               # %if.then1742
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	8(%rax), %rdi
	movl	(%rdi), %esi
	movq	%rsi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB11_379
# BB#357:                               # %if.then1751
                                        #   in Loop: Header=BB11_355 Depth=2
	movl	8(%rdi), %r8d
	movq	%r8, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$8, %rax
	je	.LBB11_361
# BB#358:                               # %if.then1751
                                        #   in Loop: Header=BB11_355 Depth=2
	leaq	-21(%r8), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_361
# BB#359:                               # %if.then1751
                                        #   in Loop: Header=BB11_355 Depth=2
	leaq	-45(%r8), %rax
	andq	%r13, %rax
	cmpq	$8, %rax
	jb	.LBB11_361
# BB#360:                               # %if.then1751
                                        #   in Loop: Header=BB11_355 Depth=2
	leaq	-29(%r8), %rax
	andq	%r13, %rax
	cmpq	$7, %rax
	jbe	.LBB11_361
# BB#365:                               # %cond.false1798
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$24, %rsi
	jne	.LBB11_366
# BB#368:                               # %cond.true1814
                                        #   in Loop: Header=BB11_355 Depth=2
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB11_370
.LBB11_379:                             # %if.else1866
                                        #   in Loop: Header=BB11_355 Depth=2
	testq	%r9, %r9
	je	.LBB11_383
# BB#380:                               # %if.else1866
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$66, %rax
	jne	.LBB11_383
# BB#381:                               # %land.lhs.true1873
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r11, %rbx
	movq	%rdi, %r14
	callq	push_operand
	movq	%r14, %rdi
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	%rbx, %r11
	movq	48(%rsp), %r10          # 8-byte Reload
	movq	80(%rsp), %r9           # 8-byte Reload
	testq	%r13, %rax
	je	.LBB11_382
.LBB11_383:                             # %if.else1880
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	je	.LBB11_386
# BB#384:                               # %land.lhs.true1882
                                        #   in Loop: Header=BB11_355 Depth=2
	movzbl	2(%rdi), %esi
	movq	%r9, %r14
	callq	push_operand
	movq	%r14, %r9
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	32(%rsp), %r11          # 8-byte Reload
	movq	48(%rsp), %r10          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB11_386
	jmp	.LBB11_385
.LBB11_361:                             # %cond.true1780
                                        #   in Loop: Header=BB11_355 Depth=2
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB11_370
# BB#362:                               # %lor.rhs1788
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB11_364
# BB#363:                               # %lor.rhs1788
                                        #   in Loop: Header=BB11_355 Depth=2
	movabsq	$1, %rax
.LBB11_364:                             # %lor.rhs1788
                                        #   in Loop: Header=BB11_355 Depth=2
	incq	%rax
	jmp	.LBB11_370
.LBB11_366:                             # %cond.false1798
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$18, %rsi
	jne	.LBB11_369
# BB#367:                               # %cond.true1804
                                        #   in Loop: Header=BB11_355 Depth=2
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB11_370
.LBB11_369:                             # %cond.false1818
                                        #   in Loop: Header=BB11_355 Depth=2
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rsi), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB11_370:                             # %cond.end1838
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	$32, %rcx
	movq	24(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%r8, %rax
	cmpq	%rdx, %rsi
	jge	.LBB11_373
# BB#371:                               # %cond.end1838
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	$32, %rcx
	movq	120(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jg	.LBB11_372
.LBB11_373:                             # %if.else1848
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%rdx, %r8
	jge	.LBB11_376
# BB#374:                               # %if.else1848
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	$32, %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jg	.LBB11_375
.LBB11_376:                             # %if.else1857
                                        #   in Loop: Header=BB11_355 Depth=2
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB11_386
# BB#377:                               # %land.lhs.true1859
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r9, %r14
	callq	reg_overlap_mentioned_for_reload_p
	movq	%r14, %r9
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	32(%rsp), %r11          # 8-byte Reload
	movq	48(%rsp), %r10          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB11_378
.LBB11_386:                             # %for.inc1893
                                        #   in Loop: Header=BB11_355 Depth=2
	movq	(%r15), %rax
	cmpq	$0, 16(%rax)
	leaq	16(%rax), %r15
	jne	.LBB11_355
.LBB11_350:                             # %while.body1176.backedge
                                        #   in Loop: Header=BB11_230 Depth=1
	movq	16(%r14), %r14
	cmpq	%r12, %r14
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	128(%rsp), %rax         # 8-byte Reload
	jne	.LBB11_230
# BB#351:
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_388
.LBB11_228:
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_388
.LBB11_242:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_245:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_203:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_222:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_232:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_239:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_323:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_340:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_345:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_348:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_326:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_336:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_331:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_333:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_287:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_293:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_296:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_270:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_273:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_283:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_278:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_280:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_382:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_385:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_372:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_375:
	xorq	%rax, %rax
	jmp	.LBB11_388
.LBB11_378:
	xorq	%rax, %rax
	jmp	.LBB11_388
.Ltmp160:
	.size	find_equiv_reg, .Ltmp160-find_equiv_reg
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_34
	.quad	.LBB11_34
	.quad	.LBB11_34
	.quad	.LBB11_388
	.quad	.LBB11_34
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_10
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_11
	.quad	.LBB11_34
	.quad	.LBB11_34
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_388
	.quad	.LBB11_18

	.text
	.globl	refers_to_regno_for_reload_p
	.align	16, 0x90
	.type	refers_to_regno_for_reload_p,@function
refers_to_regno_for_reload_p:           # @refers_to_regno_for_reload_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp167:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp168:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp169:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp170:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp171:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp172:
	.cfi_def_cfa_offset 96
.Ltmp173:
	.cfi_offset %rbx, -48
.Ltmp174:
	.cfi_offset %r12, -40
.Ltmp175:
	.cfi_offset %r13, -32
.Ltmp176:
	.cfi_offset %r14, -24
.Ltmp177:
	.cfi_offset %r15, -16
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %rbx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
.LBB12_1:                               # %tailrecurse
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_4 Depth 2
                                        #       Child Loop BB12_65 Depth 3
                                        #         Child Loop BB12_75 Depth 4
                                        #       Child Loop BB12_59 Depth 3
	testq	%rbx, %rbx
	jne	.LBB12_4
	jmp	.LBB12_2
	.align	16, 0x90
.LBB12_70:                              # %repeat.backedge
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	(%rbx), %rbx
.LBB12_4:                               # %repeat
                                        #   Parent Loop BB12_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB12_65 Depth 3
                                        #         Child Loop BB12_75 Depth 4
                                        #       Child Loop BB12_59 Depth 3
	movl	(%rbx), %edx
	movq	%rdx, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$60, %r13
	jg	.LBB12_16
# BB#5:                                 # %repeat
                                        #   in Loop: Header=BB12_4 Depth=2
	cmpq	$47, %r13
	je	.LBB12_7
# BB#6:                                 # %repeat
                                        #   in Loop: Header=BB12_4 Depth=2
	cmpq	$49, %r13
	jne	.LBB12_63
.LBB12_7:                               # %sw.bb196
                                        #   in Loop: Header=BB12_4 Depth=2
	leaq	8(%rbx), %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB12_61
# BB#8:                                 # %land.lhs.true202
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	(%r14), %rdx
	movl	(%rdx), %ecx
	movq	%rcx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB12_14
# BB#9:                                 # %land.lhs.true210
                                        #   in Loop: Header=BB12_4 Depth=2
	leaq	8(%rdx), %rax
	movq	40(%rsp), %rsi          # 8-byte Reload
	cmpq	%rsi, %rax
	je	.LBB12_14
# BB#10:                                # %land.lhs.true219
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	(%rax), %rax
	movzbl	(%rax), %esi
	cmpq	$61, %rsi
	jne	.LBB12_14
# BB#11:                                # %land.lhs.true230
                                        #   in Loop: Header=BB12_4 Depth=2
	movl	8(%rax), %esi
	cmpq	$53, %rsi
	jb	.LBB12_14
# BB#12:                                # %land.lhs.true242
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB12_3
# BB#13:                                # %land.lhs.true242.lor.lhs.false251_crit_edge
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	(%r14), %rdx
	movl	(%rdx), %ecx
.LBB12_14:                              # %lor.lhs.false251
                                        #   in Loop: Header=BB12_4 Depth=2
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_60
# BB#15:                                # %lor.lhs.false259
                                        #   in Loop: Header=BB12_4 Depth=2
	movslq	n_earlyclobbers(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rsi
	jle	.LBB12_61
	.align	16, 0x90
.LBB12_59:                              # %for.body.i
                                        #   Parent Loop BB12_1 Depth=1
                                        #     Parent Loop BB12_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	reload_earlyclobbers-8(,%rsi,8), %rcx
	cmpq	%rdx, %rcx
	je	.LBB12_60
# BB#58:                                # %for.cond.i
                                        #   in Loop: Header=BB12_59 Depth=3
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rsi
	cmpq	%rax, %rdi
	jl	.LBB12_59
	jmp	.LBB12_61
	.align	16, 0x90
.LBB12_16:                              # %repeat
                                        #   in Loop: Header=BB12_4 Depth=2
	cmpq	$63, %r13
	jne	.LBB12_17
# BB#38:                                # %sw.bb80
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_63
# BB#39:                                # %land.lhs.true87
                                        #   in Loop: Header=BB12_4 Depth=2
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB12_63
	jmp	.LBB12_40
.LBB12_60:                              # %land.lhs.true265
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB12_3
.LBB12_61:                              # %if.end272
                                        #   in Loop: Header=BB12_4 Depth=2
	cmpq	$49, %r13
	je	.LBB12_2
# BB#62:                                # %lor.lhs.false275
                                        #   in Loop: Header=BB12_4 Depth=2
	addq	$16, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	jne	.LBB12_70
	jmp	.LBB12_2
.LBB12_17:                              # %repeat
                                        #   in Loop: Header=BB12_4 Depth=2
	cmpq	$61, %r13
	je	.LBB12_18
	.align	16, 0x90
.LBB12_63:                              # %sw.epilog
                                        #   in Loop: Header=BB12_4 Depth=2
	movzbl	rtx_length(%r13), %r15d
	testq	%r15, %r15
	je	.LBB12_2
# BB#64:                                # %for.body.lr.ph
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	rtx_format(,%r13,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB12_65:                              # %for.body
                                        #   Parent Loop BB12_1 Depth=1
                                        #     Parent Loop BB12_4 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB12_75 Depth 4
	movq	%r15, %r14
	movq	16(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r14), %eax
	leaq	-1(%r14), %r15
	cmpq	$69, %rax
	jne	.LBB12_66
# BB#73:                                # %if.then328
                                        #   in Loop: Header=BB12_65 Depth=3
	movq	(%rbx,%r14,8), %rax
	movslq	(%rax), %rdx
	testq	%rdx, %rdx
	jle	.LBB12_72
# BB#74:                                # %for.body336.lr.ph
                                        #   in Loop: Header=BB12_65 Depth=3
	movq	$3, %rcx
	leaq	-1(%rdx), %r13
	shlq	%cl, %rdx
	movq	40(%rsp), %r12          # 8-byte Reload
	subq	%rdx, %r12
	jmp	.LBB12_75
	.align	16, 0x90
.LBB12_78:                              # %for.cond333.backedge.for.body336_crit_edge
                                        #   in Loop: Header=BB12_75 Depth=4
	movq	(%rbx,%r14,8), %rax
	decq	%r13
	addq	$8, %r12
.LBB12_75:                              # %for.body336
                                        #   Parent Loop BB12_1 Depth=1
                                        #     Parent Loop BB12_4 Depth=2
                                        #       Parent Loop BB12_65 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%r12, %rax
	je	.LBB12_77
# BB#76:                                # %land.lhs.true345
                                        #   in Loop: Header=BB12_75 Depth=4
	movq	8(%rax,%r13,8), %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB12_3
.LBB12_77:                              # %for.cond333.backedge
                                        #   in Loop: Header=BB12_75 Depth=4
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB12_78
	jmp	.LBB12_72
	.align	16, 0x90
.LBB12_66:                              # %for.body
                                        #   in Loop: Header=BB12_65 Depth=3
	cmpq	$101, %rax
	jne	.LBB12_72
# BB#67:                                # %land.lhs.true299
                                        #   in Loop: Header=BB12_65 Depth=3
	leaq	8(%rbx,%r15,8), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB12_72
# BB#68:                                # %if.then306
                                        #   in Loop: Header=BB12_65 Depth=3
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r15
	je	.LBB12_69
# BB#71:                                # %if.else
                                        #   in Loop: Header=BB12_65 Depth=3
	movq	(%rax), %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	refers_to_regno_for_reload_p
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB12_3
	.align	16, 0x90
.LBB12_72:                              # %for.cond.backedge
                                        #   in Loop: Header=BB12_65 Depth=3
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB12_65
	jmp	.LBB12_2
.LBB12_69:                              # %if.then309
                                        #   in Loop: Header=BB12_4 Depth=2
	addq	$8, %rbx
	jmp	.LBB12_70
.LBB12_18:                              # %sw.bb
                                        #   in Loop: Header=BB12_1 Depth=1
	movl	8(%rbx), %esi
	cmpq	$53, %rsi
	jb	.LBB12_22
# BB#19:                                # %if.then2
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	(%rax,%rsi,8), %rbx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	testq	%rbx, %rbx
	jne	.LBB12_1
# BB#20:                                # %if.end7
	movq	reg_equiv_constant(%rip), %rcx
	xorq	%rax, %rax
	cmpq	$0, (%rcx,%rsi,8)
	jne	.LBB12_3
# BB#21:                                # %if.end12
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.refers_to_regno_for_reload_p, %rcx
	movq	$6115, %rsi             # imm = 0x17E3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB12_2:
	xorq	%rax, %rax
.LBB12_3:                               # %return
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_40:                              # %if.then96
	movq	%rbx, %rdi
	callq	subreg_regno
	movq	%rax, %r8
                                        # kill: RAX<def> R8<kill>
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$52, %rax
	movq	$1, %rdx
	movq	32(%rsp), %rdi          # 8-byte Reload
	ja	.LBB12_55
# BB#41:                                # %cond.true100
	movzbl	2(%rbx), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	je	.LBB12_45
# BB#42:                                # %cond.true100
	leaq	-21(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_45
# BB#43:                                # %cond.true100
	leaq	-45(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB12_45
# BB#44:                                # %cond.true100
	leaq	-29(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB12_45
# BB#50:                                # %cond.false142
	cmpq	$24, %rax
	jne	.LBB12_51
# BB#53:                                # %cond.true158
	movzbl	target_flags+3(%rip), %edx
	andq	$2, %rdx
	xorq	$6, %rdx
	jmp	.LBB12_55
.LBB12_45:                              # %cond.true124
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	movq	$2, %rdx
	je	.LBB12_55
# BB#46:                                # %lor.rhs132
	cmpq	$6, %rax
	jne	.LBB12_47
# BB#48:                                # %lor.rhs132
	movabsq	$1, %rdx
	incq	%rdx
	jmp	.LBB12_55
.LBB12_51:                              # %cond.false142
	cmpq	$18, %rax
	jne	.LBB12_54
# BB#52:                                # %cond.true148
	movl	target_flags(%rip), %edx
	movq	$25, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	xorq	$3, %rdx
	jmp	.LBB12_55
.LBB12_47:
	movabsq	$0, %rdx
	incq	%rdx
	jmp	.LBB12_55
.LBB12_54:                              # %cond.false162
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
	movq	%rax, %rdx
.LBB12_55:                              # %cond.end185
	movq	24(%rsp), %rbx          # 8-byte Reload
	andq	%rsi, %rbx
	movq	%r8, %rcx
	andq	%rsi, %rcx
	xorq	%rax, %rax
	cmpq	%rbx, %rcx
	jae	.LBB12_3
# BB#56:                                # %land.rhs190
	addq	%r8, %rdx
	andq	%rsi, %rdx
	andq	%rsi, %rdi
	cmpq	%rdx, %rdi
.LBB12_57:                              # %return
	sbbq	%rax, %rax
	andq	$1, %rax
	jmp	.LBB12_3
.LBB12_22:                              # %if.end13
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%rdi, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %rsi
	jae	.LBB12_3
# BB#23:                                # %cond.true
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rsi, %rax
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	cmpq	$8, %rax
	je	.LBB12_27
# BB#24:                                # %cond.true
	leaq	-21(%rsi), %rax
	andq	%rdi, %rax
	cmpq	$8, %rax
	jb	.LBB12_27
# BB#25:                                # %cond.true
	leaq	-45(%rsi), %rax
	andq	%rdi, %rax
	cmpq	$8, %rax
	jb	.LBB12_27
# BB#26:                                # %cond.true
	leaq	-29(%rsi), %rax
	andq	%rdi, %rax
	cmpq	$7, %rax
	jbe	.LBB12_27
# BB#32:                                # %cond.false
	movq	%rdi, %rbx
	cmpq	$24, %rdx
	jne	.LBB12_33
# BB#35:                                # %cond.true53
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB12_37
.LBB12_27:                              # %cond.true29
	movq	%rdi, %rbx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB12_37
# BB#28:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB12_29
# BB#30:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB12_37
.LBB12_33:                              # %cond.false
	cmpq	$18, %rdx
	jne	.LBB12_36
# BB#34:                                # %cond.true45
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB12_37
.LBB12_29:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB12_37
.LBB12_36:                              # %cond.false57
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rdx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB12_37:                              # %cond.end75
	addq	%rsi, %rax
	movq	%rbx, %rdx
	andq	%rdx, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jmp	.LBB12_57
.Ltmp178:
	.size	refers_to_regno_for_reload_p, .Ltmp178-refers_to_regno_for_reload_p
	.cfi_endproc

	.align	16, 0x90
	.type	hard_reg_set_here_p,@function
hard_reg_set_here_p:                    # @hard_reg_set_here_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp187:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp188:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp189:
	.cfi_def_cfa_offset 48
.Ltmp190:
	.cfi_offset %rbx, -48
.Ltmp191:
	.cfi_offset %r12, -40
.Ltmp192:
	.cfi_offset %r13, -32
.Ltmp193:
	.cfi_offset %r14, -24
.Ltmp194:
	.cfi_offset %r15, -16
	movzbl	(%rdx), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	cmpq	$39, %rax
	je	.LBB13_22
# BB#1:                                 # %entry
	cmpq	$47, %rax
	je	.LBB13_3
# BB#2:                                 # %entry
	cmpq	$49, %rax
	jne	.LBB13_21
	.align	16, 0x90
.LBB13_3:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rdx
	movl	(%rdx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	je	.LBB13_3
# BB#4:                                 # %while.cond
	cmpq	$61, %rcx
	jne	.LBB13_21
# BB#5:                                 # %if.then13
	movl	8(%rdx), %edi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r15
	cmpq	%r15, %rdi
	jae	.LBB13_21
# BB#6:                                 # %land.lhs.true
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rdi, %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	$8, %rdx
	je	.LBB13_10
# BB#7:                                 # %land.lhs.true
	leaq	-21(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB13_10
# BB#8:                                 # %land.lhs.true
	leaq	-45(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB13_10
# BB#9:                                 # %land.lhs.true
	leaq	-29(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$7, %rcx
	jbe	.LBB13_10
# BB#15:                                # %cond.false
	cmpq	$24, %rax
	jne	.LBB13_16
# BB#18:                                # %cond.true53
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB13_20
.LBB13_22:                              # %if.then83
	movq	8(%rdx), %r12
	movslq	(%r12), %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB13_23:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_21
# BB#24:                                # %for.body
                                        #   in Loop: Header=BB13_23 Depth=1
	movq	(%r12,%rbx,8), %rdx
	decq	%rbx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	hard_reg_set_here_p
	testq	%r13, %rax
	movq	$1, %rax
	je	.LBB13_23
	jmp	.LBB13_25
.LBB13_10:                              # %cond.true
	movl	mode_class(,%rax,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB13_20
# BB#11:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB13_12
# BB#13:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB13_20
.LBB13_16:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB13_19
# BB#17:                                # %cond.true46
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB13_20
.LBB13_12:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB13_20
.LBB13_19:                              # %cond.false57
	movl	target_flags(%rip), %ebx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rbx
	andq	$4, %rbx
	leaq	3(%rax,%rbx), %rax
	addq	$4, %rbx
	cqto
	idivq	%rbx
.LBB13_20:                              # %cond.end72
	addq	%rdi, %rax
	andq	%rsi, %rax
	andq	%rsi, %r14
	cmpq	%r14, %rax
	movq	$1, %rax
	ja	.LBB13_25
.LBB13_21:                              # %if.end98
	xorq	%rax, %rax
.LBB13_25:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp195:
	.size	hard_reg_set_here_p, .Ltmp195-hard_reg_set_here_p
	.cfi_endproc

	.globl	transfer_replacements
	.align	16, 0x90
	.type	transfer_replacements,@function
transfer_replacements:                  # @transfer_replacements
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_replacements(%rip), %r8
	testq	%r8, %r8
	jle	.LBB14_5
# BB#1:
	movq	$1, %r9
	movabsq	$replacements+16, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rax), %ecx
	cmpq	%rsi, %rcx
	jne	.LBB14_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	%edi, (%rax)
.LBB14_4:                               # %for.inc
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r9
	addq	$24, %rax
	cmpq	%r8, %rdx
	jl	.LBB14_2
.LBB14_5:                               # %for.end
	retq
.Ltmp196:
	.size	transfer_replacements, .Ltmp196-transfer_replacements
	.cfi_endproc

	.globl	operands_match_p
	.align	16, 0x90
	.type	operands_match_p,@function
operands_match_p:                       # @operands_match_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp203:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp204:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp205:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp206:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp207:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp208:
	.cfi_def_cfa_offset 80
.Ltmp209:
	.cfi_offset %rbx, -48
.Ltmp210:
	.cfi_offset %r12, -40
.Ltmp211:
	.cfi_offset %r13, -32
.Ltmp212:
	.cfi_offset %r14, -24
.Ltmp213:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r13
	cmpq	%r14, %r13
	movq	$1, %r15
	je	.LBB15_53
	.align	16, 0x90
.LBB15_1:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r13), %eax
	movq	%rax, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$61, %rbx
	je	.LBB15_4
# BB#2:                                 # %if.end
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$63, %rbx
	jne	.LBB15_24
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%r13), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_24
	.align	16, 0x90
.LBB15_4:                               # %land.lhs.true6
                                        #   in Loop: Header=BB15_1 Depth=1
	movl	(%r14), %r8d
	movq	%r8, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	je	.LBB15_7
# BB#5:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$63, %rcx
	jne	.LBB15_24
# BB#6:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%r14), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	je	.LBB15_7
.LBB15_24:                              # %if.end92
                                        #   in Loop: Header=BB15_1 Depth=1
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$98, %rax
	je	.LBB15_26
# BB#25:                                # %if.end92
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$101, %rbx
	jne	.LBB15_27
.LBB15_26:                              # %if.then101
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%r13), %r13
	cmpq	%r14, %r13
	jne	.LBB15_1
	jmp	.LBB15_53
.LBB15_7:                               # %if.then21
	cmpq	$63, %rbx
	jne	.LBB15_10
# BB#8:                                 # %if.then23
	movq	8(%r13), %rsi
	movslq	8(%rsi), %r12
	cmpq	$52, %r12
	movq	$63, %rcx
	jg	.LBB15_14
# BB#9:                                 # %if.end31
	movl	16(%r13), %edx
	movzbl	2(%rsi), %esi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	%r12, %rdi
	movq	%rax, %rcx
	callq	subreg_regno_offset
	movl	(%r14), %r8d
	movq	%rax, %r15
	addq	%r12, %r15
	jmp	.LBB15_11
.LBB15_27:                              # %if.end106
	movl	(%r14), %r8d
	movq	%r8, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	addq	$-96, %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$4, %rcx
	ja	.LBB15_13
# BB#28:                                # %if.end106
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$19, %rax
	movq	%rbx, %rcx
	je	.LBB15_14
# BB#29:                                # %if.then121
	movq	8(%r14), %rsi
	movq	%r13, %rdi
	callq	operands_match_p
	testq	%r15, %rax
	je	.LBB15_30
# BB#31:                                # %if.then121
	movabsq	$1, %rax
	jmp	.LBB15_32
.LBB15_10:                              # %if.else
	movl	8(%r13), %r15d
.LBB15_11:                              # %if.end52
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB15_20
# BB#12:                                # %if.then56
	movq	8(%r14), %rax
	movslq	8(%rax), %r12
	cmpq	$52, %r12
	jle	.LBB15_19
.LBB15_13:
	movq	%rbx, %rcx
.LBB15_14:                              # %slow
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%r15, %r15
	cmpq	%rax, %rcx
	jne	.LBB15_53
# BB#15:                                # %if.end132
	cmpq	$68, %rcx
	jne	.LBB15_16
# BB#35:                                # %if.then147
	movq	8(%r13), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB15_36
# BB#37:                                # %if.then147
	movabsq	$1, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_20:                              # %if.else86
	movl	8(%r14), %eax
	jmp	.LBB15_21
.LBB15_16:                              # %if.end132
	cmpq	$67, %rcx
	jne	.LBB15_38
# BB#17:                                # %if.then135
	movq	8(%r13), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB15_18
# BB#34:                                # %if.then135
	movabsq	$1, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_19:                              # %if.end65
	movl	16(%r14), %edx
	movzbl	2(%rax), %esi
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	%r12, %rdi
	movq	%r8, %rcx
	callq	subreg_regno_offset
	addq	%r12, %rax
.LBB15_21:                              # %if.end90
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%rcx, %r15
	cmpq	%rax, %r15
	jne	.LBB15_22
# BB#23:                                # %if.end90
	movabsq	$1, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_22:
	movabsq	$0, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_30:
	movabsq	$0, %rax
.LBB15_32:                              # %if.then121
	addq	%rax, %rax
	jmp	.LBB15_33
.LBB15_36:
	movabsq	$0, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_38:                              # %if.end155
	movl	(%r13), %eax
	xorq	%r8, %rax
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB15_53
# BB#39:                                # %if.end165
	movzbl	rtx_length(%rcx), %r12d
	xorq	%r15, %r15
	testq	%r12, %r12
	je	.LBB15_52
# BB#40:                                # %for.body.lr.ph.lr.ph
	movq	rtx_format(,%rcx,8), %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB15_41:                              # %for.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_42 Depth 2
                                        #     Child Loop BB15_48 Depth 2
                                        #       Child Loop BB15_49 Depth 3
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	decq	%r12
.LBB15_42:                              # %for.body
                                        #   Parent Loop BB15_41 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	(%rbx,%r12), %rax
	cmpq	$68, %rax
	jg	.LBB15_45
# BB#43:                                # %for.body
                                        #   in Loop: Header=BB15_42 Depth=2
	andq	%rdx, %rax
	cmpq	$48, %rax
	je	.LBB15_62
	jmp	.LBB15_44
.LBB15_45:                              # %for.body
                                        #   in Loop: Header=BB15_42 Depth=2
	cmpq	$100, %rax
	jle	.LBB15_46
# BB#54:                                # %for.body
                                        #   in Loop: Header=BB15_42 Depth=2
	andq	%rdx, %rax
	cmpq	$101, %rax
	je	.LBB15_60
# BB#55:                                # %for.body
                                        #   in Loop: Header=BB15_42 Depth=2
	cmpq	$105, %rax
	jne	.LBB15_56
# BB#59:                                # %sw.bb187
                                        #   in Loop: Header=BB15_42 Depth=2
	movl	8(%r13,%r12,8), %eax
	movl	8(%r14,%r12,8), %ecx
	jmp	.LBB15_58
.LBB15_60:                              # %sw.bb199
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	8(%r13,%r12,8), %rdi
	movq	8(%r14,%r12,8), %rsi
	movq	%rdx, %r15
	callq	operands_match_p
	movq	%r15, %rdx
	xorq	%r15, %r15
	testq	%rdx, %rax
	je	.LBB15_53
# BB#61:                                # %sw.bb199
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	$1, %r15
	andq	%rdx, %rax
	cmpq	$2, %rax
	jne	.LBB15_62
	jmp	.LBB15_51
.LBB15_56:                              # %for.body
                                        #   in Loop: Header=BB15_42 Depth=2
	cmpq	$119, %rax
	jne	.LBB15_44
# BB#57:                                # %sw.bb
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	8(%r13,%r12,8), %rax
	movq	8(%r14,%r12,8), %rcx
.LBB15_58:                              # %sw.bb
                                        #   in Loop: Header=BB15_42 Depth=2
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jne	.LBB15_53
.LBB15_62:                              # %for.cond.backedge
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%r12
	testq	%rax, %rax
	jg	.LBB15_42
	jmp	.LBB15_63
.LBB15_46:                              # %for.body
                                        #   in Loop: Header=BB15_41 Depth=1
	andq	%rdx, %rax
	cmpq	$69, %rax
	jne	.LBB15_44
# BB#47:                                # %sw.bb217
                                        #   in Loop: Header=BB15_41 Depth=1
	movq	8(%r13,%r12,8), %rax
	movq	8(%r14,%r12,8), %rcx
	movl	(%rax), %ebx
	movl	(%rcx), %eax
	xorq	%r15, %r15
	cmpq	%rax, %rbx
	je	.LBB15_48
	jmp	.LBB15_53
.LBB15_66:                              # %if.then260
                                        #   in Loop: Header=BB15_48 Depth=2
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB15_48:                              # %for.cond236.outer
                                        #   Parent Loop BB15_41 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB15_49 Depth 3
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
.LBB15_49:                              # %for.cond236
                                        #   Parent Loop BB15_41 Depth=1
                                        #     Parent Loop BB15_48 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_50
# BB#64:                                # %for.body239
                                        #   in Loop: Header=BB15_49 Depth=3
	movq	8(%r13,%r12,8), %rax
	movq	8(%r14,%r12,8), %rcx
	movq	(%rax,%rbx,8), %rdi
	movq	(%rcx,%rbx,8), %rsi
	callq	operands_match_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_53
# BB#65:                                # %for.body239
                                        #   in Loop: Header=BB15_49 Depth=3
	decq	%rbx
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB15_49
	jmp	.LBB15_66
.LBB15_50:                              #   in Loop: Header=BB15_41 Depth=1
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB15_51:                              # %for.cond.outer.backedge
                                        #   in Loop: Header=BB15_41 Depth=1
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r15, 24(%rsp)          # 8-byte Spill
	jg	.LBB15_41
	jmp	.LBB15_52
.LBB15_18:
	movabsq	$0, %r15
	movq	%r15, %rax
	jmp	.LBB15_33
.LBB15_63:
	movq	24(%rsp), %r15          # 8-byte Reload
.LBB15_52:                              # %for.end264
	incq	%r15
.LBB15_53:                              # %return
	movq	%r15, %rax
.LBB15_33:                              # %if.then121
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_44:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.operands_match_p, %rcx
	movq	$2206, %rsi             # imm = 0x89E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp214:
	.size	operands_match_p, .Ltmp214-operands_match_p
	.cfi_endproc

	.globl	safe_from_earlyclobber
	.align	16, 0x90
	.type	safe_from_earlyclobber,@function
safe_from_earlyclobber:                 # @safe_from_earlyclobber
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp218:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp219:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp220:
	.cfi_def_cfa_offset 96
.Ltmp221:
	.cfi_offset %rbx, -24
.Ltmp222:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	leaq	40(%rsp), %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	decompose
	movq	64(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	56(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	40(%rsp), %rax
	movq	48(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	immune_p
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp223:
	.size	safe_from_earlyclobber, .Ltmp223-safe_from_earlyclobber
	.cfi_endproc

	.align	16, 0x90
	.type	decompose,@function
decompose:                              # @decompose
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp230:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp231:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp232:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp233:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp234:
	.cfi_def_cfa_offset 48
.Ltmp235:
	.cfi_offset %rbx, -48
.Ltmp236:
	.cfi_offset %r12, -40
.Ltmp237:
	.cfi_offset %r13, -32
.Ltmp238:
	.cfi_offset %r14, -24
.Ltmp239:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB17_1
.LBB17_80:                              # %if.then585
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%r13), %rsi
	.align	16, 0x90
.LBB17_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r13
	movl	(%r13), %ebx
	movq	%rbx, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	xorq	%rcx, %rcx
	movq	$1, %rdx
	xorq	%rax, %rax
	cmpq	$133, %rdi
	jg	.LBB17_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB17_1 Depth=1
	addq	$-54, %rdi
	andq	%r15, %rdi
	cmpq	$14, %rdi
	ja	.LBB17_91
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB17_1 Depth=1
	xorq	%rsi, %rsi
	jmpq	*.LJTI17_0(,%rdi,8)
.LBB17_78:                              # %if.then565
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%r13), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB17_1
# BB#79:                                # %if.end577
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r13, %rdi
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB17_80
# BB#81:                                # %if.else589
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	2(%r13), %ecx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	je	.LBB17_85
# BB#82:                                # %if.else589
	leaq	-21(%rax), %rdx
	andq	%r15, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_85
# BB#83:                                # %if.else589
	leaq	-45(%rax), %rdx
	andq	%r15, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_85
# BB#84:                                # %if.else589
	addq	$-29, %rax
	andq	%r15, %rax
	cmpq	$7, %rax
	ja	.LBB17_73
.LBB17_85:                              # %cond.true622
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB17_77
# BB#86:                                # %lor.rhs630
	cmpq	$6, %rcx
	jne	.LBB17_87
# BB#88:                                # %lor.rhs630
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB17_77
.LBB17_4:                               # %tailrecurse
	cmpq	$134, %rdi
	jne	.LBB17_92
# BB#5:
	xorq	%rsi, %rsi
	jmp	.LBB17_93
.LBB17_92:                              # %tailrecurse
	cmpq	$140, %rdi
	movabsq	$0, %rsi
	jne	.LBB17_91
.LBB17_93:                              # %if.end737
	movl	%ecx, (%r14)
	movl	%edx, 4(%r14)
	movq	$0, 8(%r14)
	movq	%rsi, 16(%r14)
.LBB17_94:                              # %return
	movq	%rax, 24(%r14)
.LBB17_95:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_61:                              # %if.then454
	movq	%r13, %rdi
	callq	true_regnum
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB17_62
# BB#64:                                # %if.else471
	movzbl	2(%r13), %ecx
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	cmpq	$8, %rdx
	je	.LBB17_68
# BB#65:                                # %if.else471
	leaq	-21(%rax), %rdx
	andq	%r15, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_68
# BB#66:                                # %if.else471
	leaq	-45(%rax), %rdx
	andq	%r15, %rdx
	cmpq	$8, %rdx
	jb	.LBB17_68
# BB#67:                                # %if.else471
	addq	$-29, %rax
	andq	%r15, %rax
	cmpq	$7, %rax
	jbe	.LBB17_68
.LBB17_73:                              # %cond.false
	cmpq	$24, %rcx
	jne	.LBB17_74
# BB#90:                                # %cond.true532
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB17_77
.LBB17_6:                               # %if.then
	movq	8(%r13), %rax
	movl	(%rax), %edx
	movq	%rdx, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	$58, %rsi
	je	.LBB17_20
# BB#7:                                 # %if.then
	xorq	%r12, %r12
	leaq	-100(%rsi), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jb	.LBB17_14
# BB#8:                                 # %if.then
	addq	$-96, %rsi
	andq	%r15, %rsi
	cmpq	$3, %rsi
	ja	.LBB17_21
# BB#9:                                 # %if.then16
	movq	8(%rax), %rax
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$255, %rbx
	movzbl	mode_size(%rbx), %ecx
	movl	8(%rax), %esi
	movq	%rcx, %rdx
	negq	%rdx
	cmpq	$7, %rsi
	jne	.LBB17_10
# BB#11:                                # %if.then16
	movabsq	$1, %rsi
	jmp	.LBB17_12
.LBB17_91:                              # %if.else733
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.decompose, %rdx
	movq	$2354, %rsi             # imm = 0x932
	callq	fancy_abort
.LBB17_62:                              # %if.then462
	movl	8(%r13), %esi
	leaq	1(%rsi), %rax
	jmp	.LBB17_63
.LBB17_68:                              # %cond.true
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB17_77
# BB#69:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB17_70
# BB#71:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB17_77
.LBB17_14:                              # %if.then48
	movq	16(%rax), %rcx
	movzbl	(%rcx), %edi
	cmpq	$75, %rdi
	jne	.LBB17_19
# BB#15:                                # %land.lhs.true
	movq	8(%rax), %rdi
	movq	8(%rcx), %rbx
	cmpq	%rbx, %rdi
	jne	.LBB17_19
# BB#16:                                # %land.lhs.true67
	movq	16(%rcx), %r8
	movzbl	(%r8), %ecx
	cmpq	$133, %rcx
	jg	.LBB17_23
# BB#17:                                # %land.lhs.true67
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_19
# BB#18:                                # %land.lhs.true67
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$24599, %rbx            # imm = 0x6017
	jne	.LBB17_25
	jmp	.LBB17_19
.LBB17_10:
	movabsq	$0, %rsi
.LBB17_12:                              # %if.then16
	xorq	%rdi, %rdi
	movl	%edi, (%r14)
	jmp	.LBB17_13
.LBB17_74:                              # %cond.false
	cmpq	$18, %rcx
	jne	.LBB17_76
# BB#75:                                # %cond.true523
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB17_77
.LBB17_76:                              # %cond.false536
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rcx), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
	jmp	.LBB17_77
.LBB17_70:
	movabsq	$0, %rax
	incq	%rax
.LBB17_77:                              # %cond.end554
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rsi, %rax
.LBB17_63:                              # %if.end737
	movq	$1, %rcx
	xorq	%rdx, %rdx
	jmp	.LBB17_93
.LBB17_23:                              # %land.lhs.true67
	cmpq	$134, %rcx
	je	.LBB17_25
# BB#24:                                # %land.lhs.true67
	cmpq	$140, %rcx
	jne	.LBB17_19
.LBB17_25:                              # %if.then155
	movq	8(%r8), %rax
	movl	8(%rdi), %edx
	movq	%rax, %rcx
	negq	%rcx
	cmpq	$7, %rdx
	jne	.LBB17_26
# BB#27:                                # %if.then155
	movabsq	$1, %rdx
	jmp	.LBB17_28
.LBB17_19:                              # %if.end188
	cmpq	$58, %rsi
	jne	.LBB17_21
.LBB17_20:                              # %if.then193
	movq	8(%rax), %rax
	movl	(%rax), %edx
	movq	$1, %r12
.LBB17_21:                              # %if.end197
	andq	$65535, %rdx            # imm = 0xFFFF
	xorq	%rsi, %rsi
	cmpq	$75, %rdx
	jne	.LBB17_22
# BB#29:                                # %if.then202
	movq	8(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$133, %rcx
	jg	.LBB17_35
# BB#30:                                # %if.then202
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	ja	.LBB17_32
# BB#31:                                # %if.then202
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	$24599, %rdi            # imm = 0x6017
	jne	.LBB17_37
	jmp	.LBB17_32
.LBB17_22:
	xorq	%rcx, %rcx
	jmp	.LBB17_42
.LBB17_35:                              # %if.then202
	cmpq	$134, %rcx
	je	.LBB17_37
# BB#36:                                # %if.then202
	cmpq	$140, %rcx
	jne	.LBB17_32
.LBB17_37:                              # %if.then266
	movq	16(%rax), %rsi
	movq	%rdx, %rcx
	jmp	.LBB17_42
.LBB17_32:                              # %if.else
	movq	16(%rax), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB17_39
# BB#33:                                # %if.else
	addq	$-54, %rcx
	andq	%r15, %rcx
	cmpq	$14, %rcx
	jbe	.LBB17_38
# BB#34:
	xorq	%rcx, %rcx
	jmp	.LBB17_42
.LBB17_39:                              # %if.else
	cmpq	$134, %rcx
	je	.LBB17_41
# BB#40:                                # %if.else
	cmpq	$140, %rcx
	movabsq	$0, %rcx
	jne	.LBB17_42
	jmp	.LBB17_41
.LBB17_38:                              # %if.else
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	$24599, %rbx            # imm = 0x6017
	movabsq	$0, %rcx
	je	.LBB17_42
.LBB17_41:                              # %if.then336
	movq	%rdx, %rsi
	movq	%rdi, %rcx
.LBB17_42:                              # %if.end345
	movq	const_int_rtx+512(%rip), %r15
	testq	%rcx, %rcx
	je	.LBB17_44
# BB#43:                                # %if.end345
	movq	%rsi, %rax
.LBB17_44:                              # %if.end345
	je	.LBB17_46
# BB#45:                                # %if.end345
	movq	%rcx, %r15
.LBB17_46:                              # %if.end345
	movl	(%r15), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$58, %rdx
	jne	.LBB17_48
# BB#47:                                # %if.then354
	movq	8(%r15), %r15
	movl	(%r15), %ecx
.LBB17_48:                              # %if.end358
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB17_56
# BB#49:                                # %if.end358
	cmpq	$75, %rcx
	jne	.LBB17_54
# BB#50:                                # %if.then363
	movq	8(%r15), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB17_52
# BB#51:                                # %if.then371
	movq	16(%r15), %rcx
	movzbl	2(%rax), %esi
	movq	$75, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	8(%r15), %r15
	jmp	.LBB17_56
.LBB17_54:                              # %if.then412
	movzbl	2(%rax), %esi
	jmp	.LBB17_55
.LBB17_52:                              # %if.else381
	movq	16(%r15), %rdx
	movzbl	(%rdx), %edx
	movzbl	2(%rax), %esi
	cmpq	$54, %rdx
	jne	.LBB17_55
# BB#53:                                # %if.then389
	movq	$75, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_ee
	movq	16(%r15), %r15
	jmp	.LBB17_56
.LBB17_55:                              # %if.end418
	movq	$75, %rdi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	gen_rtx_fmt_ee
	movq	const_int_rtx+512(%rip), %r15
.LBB17_56:                              # %if.end418
	testq	%r12, %r12
	je	.LBB17_59
# BB#57:                                # %land.lhs.true419
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB17_59
# BB#58:                                # %if.then424
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	$58, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
.LBB17_59:                              # %if.end429
	movzbl	(%r15), %ecx
	cmpq	$54, %rcx
	jne	.LBB17_96
# BB#60:                                # %if.end435
	movzbl	2(%r13), %ecx
	movq	8(%r15), %rdx
	movzbl	mode_size(%rcx), %ecx
	addq	%rdx, %rcx
	xorq	%rsi, %rsi
	movl	%esi, (%r14)
.LBB17_13:                              # %return
	movl	%esi, 4(%r14)
	movq	%rax, 8(%r14)
	movq	%rdx, 16(%r14)
	movq	%rcx, 24(%r14)
	jmp	.LBB17_95
.LBB17_26:
	movabsq	$0, %rdx
.LBB17_28:                              # %if.then155
	xorq	%rsi, %rsi
	movl	%esi, (%r14)
	movl	%edx, 4(%r14)
	movq	%rdi, 8(%r14)
	movq	%rcx, 16(%r14)
	jmp	.LBB17_94
.LBB17_87:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB17_77
.LBB17_96:                              # %if.then434
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.decompose, %rcx
	movq	$2315, %rsi             # imm = 0x90B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp240:
	.size	decompose, .Ltmp240-decompose
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_93
	.quad	.LBB17_93
	.quad	.LBB17_93
	.quad	.LBB17_91
	.quad	.LBB17_93
	.quad	.LBB17_91
	.quad	.LBB17_91
	.quad	.LBB17_61
	.quad	.LBB17_93
	.quad	.LBB17_78
	.quad	.LBB17_91
	.quad	.LBB17_91
	.quad	.LBB17_6
	.quad	.LBB17_93
	.quad	.LBB17_93

	.text
	.align	16, 0x90
	.type	immune_p,@function
immune_p:                               # @immune_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp247:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp248:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp249:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp250:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp251:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp252:
	.cfi_def_cfa_offset 96
.Ltmp253:
	.cfi_offset %rbx, -48
.Ltmp254:
	.cfi_offset %r12, -40
.Ltmp255:
	.cfi_offset %r13, -32
.Ltmp256:
	.cfi_offset %r14, -24
.Ltmp257:
	.cfi_offset %r15, -16
	movq	96(%rsp), %rdx
	movq	%rdi, %rax
	leaq	96(%rsp), %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rdx
	je	.LBB18_3
# BB#1:                                 # %if.then
	movq	16(%rbx), %rdi
	movq	24(%rbx), %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	refers_to_regno_for_reload_p
	testq	%r12, %rax
	je	.LBB18_37
# BB#2:
	movabsq	$0, %r13
	jmp	.LBB18_38
.LBB18_3:                               # %if.end
	movq	$1, %r13
	movq	$32, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB18_38
# BB#4:                                 # %if.end5
	movzbl	(%rsi), %ecx
	cmpq	$66, %rcx
	jne	.LBB18_39
# BB#5:                                 # %if.end8
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB18_38
# BB#6:                                 # %if.end14
	leaq	16(%rsp), %rdi
	movq	%rax, %rsi
	callq	decompose
	movq	24(%rsp), %r14
	movq	32(%rsp), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	8(%rbx), %r15
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB18_7
# BB#34:                                # %if.end236
	movq	24(%rbx), %rax
	movq	$1, %r13
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jge	.LBB18_38
# BB#35:                                # %lor.rhs
	movq	16(%rbx), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB18_36
.LBB18_37:                              # %lor.rhs
	movabsq	$1, %r13
.LBB18_38:                              # %return
	movq	%r13, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_7:                               # %if.then18
	movzbl	(%r14), %eax
	movq	%r14, %rsi
	cmpq	$133, %rax
	jg	.LBB18_10
# BB#8:                                 # %if.then18
	leaq	-54(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	movq	%r15, %rdi
	ja	.LBB18_25
# BB#9:                                 # %if.then18
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB18_12
	jmp	.LBB18_25
.LBB18_10:                              # %if.then18
	cmpq	$134, %rax
	movq	%r15, %rdi
	je	.LBB18_12
# BB#11:                                # %if.then18
	cmpq	$140, %rax
	jne	.LBB18_25
.LBB18_12:                              # %land.lhs.true
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB18_15
# BB#13:                                # %land.lhs.true
	addq	$-54, %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	ja	.LBB18_17
# BB#14:                                # %land.lhs.true
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB18_38
	jmp	.LBB18_17
.LBB18_36:
	movabsq	$0, %r13
	jmp	.LBB18_38
.LBB18_15:                              # %land.lhs.true
	cmpq	$134, %rcx
	je	.LBB18_38
# BB#16:                                # %land.lhs.true
	cmpq	$140, %rcx
	je	.LBB18_38
.LBB18_17:                              # %if.end113
	cmpq	$133, %rax
	jg	.LBB18_20
# BB#18:                                # %if.end113
	addq	$-54, %rax
	andq	%r12, %rax
	cmpq	$14, %rax
	ja	.LBB18_25
# BB#19:                                # %if.end113
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB18_22
	jmp	.LBB18_25
.LBB18_20:                              # %if.end113
	cmpq	$134, %rax
	je	.LBB18_22
# BB#21:                                # %if.end113
	cmpq	$140, %rax
	jne	.LBB18_25
.LBB18_22:                              # %land.lhs.true161
	movq	global_rtl+24(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB18_38
# BB#23:                                # %land.lhs.true161
	movq	global_rtl+32(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB18_38
# BB#24:                                # %land.lhs.true161
	movq	global_rtl+16(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB18_38
.LBB18_25:                              # %if.end174
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB18_28
# BB#26:                                # %if.end174
	addq	$-54, %rcx
	andq	%r12, %rcx
	cmpq	$14, %rcx
	ja	.LBB18_33
# BB#27:                                # %if.end174
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB18_30
	jmp	.LBB18_33
.LBB18_28:                              # %if.end174
	cmpq	$134, %rcx
	je	.LBB18_30
# BB#29:                                # %if.end174
	cmpq	$140, %rcx
	jne	.LBB18_33
.LBB18_30:                              # %land.lhs.true222
	movq	global_rtl+24(%rip), %rax
	cmpq	%rax, %rsi
	je	.LBB18_38
# BB#31:                                # %land.lhs.true222
	movq	global_rtl+32(%rip), %rax
	cmpq	%rax, %rsi
	je	.LBB18_38
# BB#32:                                # %land.lhs.true222
	movq	global_rtl+16(%rip), %rax
	cmpq	%rax, %rsi
	je	.LBB18_38
.LBB18_33:                              # %if.end235
	xorq	%r13, %r13
	jmp	.LBB18_38
.LBB18_39:                              # %if.then7
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.immune_p, %rcx
	movq	$2374, %rsi             # imm = 0x946
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp258:
	.size	immune_p, .Ltmp258-immune_p
	.cfi_endproc

	.globl	find_reloads
	.align	16, 0x90
	.type	find_reloads,@function
find_reloads:                           # @find_reloads
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp265:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp266:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp267:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp268:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 48
	subq	$3872, %rsp             # imm = 0xF20
.Ltmp270:
	.cfi_def_cfa_offset 3920
.Ltmp271:
	.cfi_offset %rbx, -48
.Ltmp272:
	.cfi_offset %r12, -40
.Ltmp273:
	.cfi_offset %r13, -32
.Ltmp274:
	.cfi_offset %r14, -24
.Ltmp275:
	.cfi_offset %r15, -16
	movzbl	(%rdi), %eax
	movq	%rdx, 192(%rsp)         # 8-byte Spill
	movq	%rsi, %rbx
	movzbl	rtx_class(%rax), %eax
	movq	32(%rdi), %r12
	xorq	%r14, %r14
	cmpq	$105, %rax
	movabsq	$0, %rax
	jne	.LBB19_3
# BB#1:                                 # %cond.true
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	movq	%r12, %rax
	je	.LBB19_3
# BB#2:                                 # %cond.false
	movq	%r12, %rsi
	movq	%rdi, %r15
	movq	%r8, %r13
	movq	%rcx, 280(%rsp)         # 8-byte Spill
	callq	single_set_2
	movq	280(%rsp), %rcx         # 8-byte Reload
	movq	%r13, %r8
	movq	%r15, %rdi
.LBB19_3:                               # %cond.end18
	movq	%rcx, 280(%rsp)         # 8-byte Spill
	movq	%r8, 264(%rsp)          # 8-byte Spill
	movq	%rax, 272(%rsp)         # 8-byte Spill
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	%rdi, this_insn(%rip)
	movl	%r14d, n_reloads(%rip)
	movl	%r14d, n_replacements(%rip)
	movl	%r14d, n_earlyclobbers(%rip)
	movl	%ebx, replace_reloads(%rip)
	movq	%rbx, 184(%rsp)         # 8-byte Spill
	movl	%ecx, hard_regs_live_known(%rip)
	movq	%r8, static_reload_reg_p(%rip)
	movzbl	(%rdi), %r15d
	addq	$-33, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r15, %rbx
	andq	%r14, %rbx
	cmpq	$2, %rbx
	sbbq	%rax, %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movabsq	$secondary_memlocs_elim, %rdi
	xorq	%rsi, %rsi
	movq	$14160, %rdx            # imm = 0x3750
	callq	memset
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	jne	.LBB19_9
# BB#4:                                 # %land.lhs.true
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_9
# BB#5:                                 # %land.lhs.true40
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB19_9
# BB#6:                                 # %land.lhs.true48
	movq	16(%r12), %rcx
	movl	(%rcx), %edi
	movq	%rdi, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB19_9
# BB#7:                                 # %land.lhs.true56
	movl	8(%rcx), %edx
	cmpq	$52, %rdx
	ja	.LBB19_9
# BB#8:                                 # %land.lhs.true65
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movl	regclass_map(,%rdx,4), %esi
	movl	regclass_map(,%rax,4), %edx
	callq	ix86_register_move_cost
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	xorq	%rcx, %rcx
	cmpq	$2, %rdx
	je	.LBB19_60
.LBB19_9:                               # %if.end91
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	extract_insn
	movl	recog_data+996(%rip), %edi
	movabsq	$4294901760, %rdx       # imm = 0xFFFF0000
	andq	%rdi, %rdx
	movq	$56, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	%rax, 296(%rsp)         # 8-byte Spill
	movq	$16, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$40, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	xorq	%rcx, %rcx
	testq	$255, %rdi
	movl	%eax, reload_n_operands(%rip)
	movq	%rax, %r12
	je	.LBB19_60
# BB#10:                                # %if.end91
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	testq	$255, %rsi
	je	.LBB19_60
# BB#11:                                # %if.end100
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	movq	%rbx, 152(%rsp)         # 8-byte Spill
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	208(%rsp), %rax         # 8-byte Reload
	movl	40(%rax), %eax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	$31, %rcx
	shrq	%cl, %rax
	movl	%eax, this_insn_is_asm(%rip)
	movq	$56, %rcx
	movl	recog_data+996(%rip), %ebx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	movq	$2, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	leaq	656(%rsp), %rdi
	movabsq	$recog_data+720, %rsi
	callq	memcpy
	movq	$3, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	leaq	3632(%rsp), %rdi
	movabsq	$recog_data+480, %rsi
	callq	memcpy
	testq	%rbx, %rbx
	movq	%r14, %r11
	movq	%r11, %r13
	movq	%r12, %r14
	movq	%r14, 296(%rsp)         # 8-byte Spill
	jle	.LBB19_106
# BB#12:                                # %for.body.lr.ph
	leaq	-1(%r14), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$0, %rcx
	andq	%r13, %rax
	movq	%rax, 288(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB19_13:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_14 Depth 2
                                        #       Child Loop BB19_21 Depth 3
	movq	recog_data(,%r12,8), %rax
	movq	3632(%rsp,%r12,8), %rdi
	movq	%rax, 784(%rsp,%r12,8)
	movq	%rdi, 648(%rsp)
	xorq	%rax, %rax
	movl	%eax, 2960(%rsp,%r12,4)
.LBB19_14:                              # %while.cond.outer
                                        #   Parent Loop BB19_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_21 Depth 3
	movq	%r13, 320(%rsp)         # 8-byte Spill
	movq	%r12, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	%rdx, 344(%rsp)         # 8-byte Spill
	movq	%r13, %rsi
	andq	%rax, %rsi
	movq	%rsi, 336(%rsp)         # 8-byte Spill
	leaq	1(%r13), %rsi
	andq	%rax, %rsi
	movq	%rsi, 312(%rsp)         # 8-byte Spill
	cmpq	%rsi, %rdx
	movq	%rcx, %rax
	jne	.LBB19_16
# BB#15:                                # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	movabsq	$1, %rax
.LBB19_16:                              # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	movq	336(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, 344(%rsp)         # 8-byte Folded Reload
	movq	%rcx, 328(%rsp)         # 8-byte Spill
	jne	.LBB19_18
# BB#17:                                # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	movabsq	$1, %rcx
	movq	%rcx, 328(%rsp)         # 8-byte Spill
.LBB19_18:                              # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	movq	$1, %rdx
	je	.LBB19_20
# BB#19:                                # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
.LBB19_20:                              # %while.cond.outer
                                        #   in Loop: Header=BB19_14 Depth=2
	orq	%rax, 328(%rsp)         # 8-byte Folded Spill
	addq	%r12, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 304(%rsp)         # 8-byte Spill
	jmp	.LBB19_21
	.align	16, 0x90
.LBB19_35:                              # %while.cond.backedge
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	648(%rsp), %rdi
.LBB19_21:                              # %while.cond
                                        #   Parent Loop BB19_13 Depth=1
                                        #     Parent Loop BB19_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	1(%rdi), %rcx
	movq	%rcx, 648(%rsp)
	movsbq	(%rdi), %rdx
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$36, %rdx
	jg	.LBB19_29
# BB#22:                                # %while.cond
                                        #   in Loop: Header=BB19_21 Depth=3
	testq	$255, %rdx
	jne	.LBB19_23
	jmp	.LBB19_26
	.align	16, 0x90
.LBB19_29:                              # %while.cond
                                        #   in Loop: Header=BB19_21 Depth=3
	cmpq	$43, %rax
	je	.LBB19_33
# BB#30:                                # %while.cond
                                        #   in Loop: Header=BB19_21 Depth=3
	cmpq	$37, %rax
	je	.LBB19_36
# BB#31:                                # %while.cond
                                        #   in Loop: Header=BB19_21 Depth=3
	cmpq	$61, %rax
	jne	.LBB19_23
# BB#32:                                # %if.then121
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	$2, %rax
	jmp	.LBB19_34
	.align	16, 0x90
.LBB19_23:                              # %if.else137
                                        #   in Loop: Header=BB19_21 Depth=3
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB19_35
# BB#24:                                # %if.then143
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	$10, %rdx
	leaq	648(%rsp), %rsi
	callq	strtoul
	movq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	recog_data(,%rbx,8), %rdi
	movq	recog_data(,%r12,8), %rsi
	movq	%r14, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	callq	operands_match_p
	imulq	$30, %rbx, %rcx
	movq	344(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, %r15
	leaq	1024(%rsp,%rcx), %r15
	movb	%al, (%r12,%r15)
	je	.LBB19_25
# BB#39:                                # %if.end159
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_35
# BB#40:                                # %if.then162
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	336(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB19_42
# BB#41:                                # %if.then162
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	312(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB19_45
.LBB19_42:                              # %if.then168
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	336(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	$1, %r13
	je	.LBB19_44
# BB#43:                                # %if.then168
                                        #   in Loop: Header=BB19_21 Depth=3
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB19_44:                              # %if.then168
                                        #   in Loop: Header=BB19_21 Depth=3
	addq	%r14, %r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	recog_data(,%r13,8), %rdi
	movq	recog_data(,%r12,8), %rsi
	callq	operands_match_p
	imulq	$30, %r13, %rcx
	leaq	1024(%rsp,%rcx), %rcx
	movb	%al, (%r12,%rcx)
	movq	320(%rsp), %r13         # 8-byte Reload
.LBB19_45:                              # %if.end183
                                        #   in Loop: Header=BB19_21 Depth=3
	cmpq	$0, 328(%rsp)           # 8-byte Folded Reload
	je	.LBB19_35
# BB#46:                                # %if.then190
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	recog_data(,%rbx,8), %rdi
	movq	304(%rsp), %rbx         # 8-byte Reload
	movq	recog_data(,%rbx,8), %rsi
	callq	operands_match_p
	movb	%al, (%rbx,%r15)
	movq	648(%rsp), %rdi
	jmp	.LBB19_21
	.align	16, 0x90
.LBB19_33:                              # %if.then126
                                        #   in Loop: Header=BB19_21 Depth=3
	movq	$1, %rax
.LBB19_34:                              # %while.cond.backedge
                                        #   in Loop: Header=BB19_21 Depth=3
	movl	%eax, 2960(%rsp,%r12,4)
	jmp	.LBB19_35
	.align	16, 0x90
.LBB19_36:                              # %if.then132
                                        #   in Loop: Header=BB19_14 Depth=2
	movq	288(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, 344(%rsp)         # 8-byte Folded Reload
	movq	%rcx, %rdi
	movq	%r12, %r13
	movabsq	$0, %rcx
	jne	.LBB19_14
	jmp	.LBB19_37
	.align	16, 0x90
.LBB19_26:                              # %for.inc
                                        #   in Loop: Header=BB19_13 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	296(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	jl	.LBB19_13
# BB#27:                                # %for.cond212.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_28
# BB#47:                                # %for.body215.lr.ph
	movq	%r13, 320(%rsp)         # 8-byte Spill
	movq	272(%rsp), %rax         # 8-byte Reload
	leaq	8(%rax), %rax
	movq	%rax, 344(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	xorq	%r13, %r13
	jmp	.LBB19_48
.LBB19_58:                              # %land.lhs.true289
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$78, %rax
	jne	.LBB19_92
	jmp	.LBB19_59
	.align	16, 0x90
.LBB19_61:                              # %if.else317
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$63, %rcx
	je	.LBB19_65
# BB#62:                                # %if.else317
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$75, %rcx
	movq	208(%rsp), %rbx         # 8-byte Reload
	je	.LBB19_75
# BB#63:                                # %if.else317
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$66, %rcx
	jne	.LBB19_74
# BB#64:                                # %if.then320
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	recog_data+240(,%r13,8), %rsi
	movq	8(%r14), %rdx
	movzbl	2(%r14), %edi
	leaq	8(%r14), %rcx
	movq	%rbx, 8(%rsp)
	movq	192(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, (%rsp)
	movq	%r13, %r8
	movq	%rax, %r9
	callq	find_reloads_address
	movq	recog_data+240(,%r13,8), %rcx
	movl	%eax, (%r15)
	movq	(%rcx), %r14
	jmp	.LBB19_92
.LBB19_65:                              # %if.then354
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	8(%r14), %rcx
	movq	%rcx, 336(%rsp)         # 8-byte Spill
	movq	272(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	movq	208(%rsp), %rbx         # 8-byte Reload
	jne	.LBB19_67
# BB#66:                                #   in Loop: Header=BB19_48 Depth=1
	xorq	%r8, %r8
	jmp	.LBB19_69
.LBB19_74:                              # %lor.lhs.false419
                                        #   in Loop: Header=BB19_48 Depth=1
	movzbl	rtx_class(%rcx), %edx
	cmpq	$49, %rdx
	jne	.LBB19_76
.LBB19_75:                              # %if.then425
                                        #   in Loop: Header=BB19_48 Depth=1
	xorq	%r8, %r8
	movq	%r15, (%rsp)
	movq	%r14, %rdi
.LBB19_91:                              # %if.end511
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r9
	callq	find_reloads_toplev
	movq	%rax, %r14
	jmp	.LBB19_92
.LBB19_67:                              # %land.rhs
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	recog_data+240(,%r13,8), %rcx
	cmpq	%rcx, 344(%rsp)         # 8-byte Folded Reload
	movabsq	$0, %r8
	jne	.LBB19_69
# BB#68:                                # %land.rhs
                                        #   in Loop: Header=BB19_48 Depth=1
	movabsq	$1, %r8
.LBB19_69:                              # %land.end
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	%r15, (%rsp)
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r9
	callq	find_reloads_toplev
	movq	%rax, %r14
	movq	184(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_92
# BB#70:                                # %land.lhs.true377
                                        #   in Loop: Header=BB19_48 Depth=1
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB19_92
# BB#71:                                # %land.lhs.true382
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	336(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB19_92
# BB#72:                                # %land.lhs.true387
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rdx
	jb	.LBB19_92
# BB#73:                                # %if.then402
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	336(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	emit_insn_before
	movl	8(%rbx), %ecx
	movq	reg_equiv_memory_loc(%rip), %rdx
	movq	(%rdx,%rcx,8), %rdx
	movq	$4, %rsi
	movq	%rax, %rdi
	callq	set_unique_reg_note
	.align	16, 0x90
.LBB19_92:                              # %if.end511
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	%r14, recog_data(,%r13,8)
	movq	%r14, 784(%rsp,%r13,8)
.LBB19_93:                              # %if.end511
                                        #   in Loop: Header=BB19_48 Depth=1
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB19_99
# BB#94:                                # %land.lhs.true518
                                        #   in Loop: Header=BB19_48 Depth=1
	movl	8(%r14), %edi
	xorq	%rbx, %rbx
	cmpq	$53, %rdi
	movabsq	$0, %rax
	jb	.LBB19_96
# BB#95:                                # %cond.true526
                                        #   in Loop: Header=BB19_48 Depth=1
	callq	reg_preferred_class
	movq	recog_data(,%r13,8), %r14
.LBB19_96:                              # %land.lhs.true540
                                        #   in Loop: Header=BB19_48 Depth=1
	movl	%eax, 3504(%rsp,%r12)
	movl	8(%r14), %edi
	cmpq	$53, %rdi
	jb	.LBB19_100
# BB#97:                                # %land.rhs548
                                        #   in Loop: Header=BB19_48 Depth=1
	callq	reg_alternate_class
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rbx
	jne	.LBB19_100
# BB#98:                                # %land.rhs548
                                        #   in Loop: Header=BB19_48 Depth=1
	movabsq	$1, %rbx
	jmp	.LBB19_100
	.align	16, 0x90
.LBB19_99:                              # %land.end557.critedge
                                        #   in Loop: Header=BB19_48 Depth=1
	xorq	%rbx, %rbx
	movl	%ebx, 3504(%rsp,%r12)
.LBB19_100:                             # %land.end557
                                        #   in Loop: Header=BB19_48 Depth=1
	movb	%bl, 3472(%rsp,%r13)
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$4, %r12
	movq	296(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB19_48
	jmp	.LBB19_101
.LBB19_76:                              # %if.else437
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$61, %rcx
	jne	.LBB19_93
# BB#77:                                # %if.then440
                                        #   in Loop: Header=BB19_48 Depth=1
	movslq	8(%r14), %rcx
	movq	reg_equiv_constant(%rip), %rdx
	movq	(%rdx,%rcx,8), %rdi
	testq	%rdi, %rdi
	jne	.LBB19_79
# BB#78:                                #   in Loop: Header=BB19_48 Depth=1
	movq	%r14, %rdi
	jmp	.LBB19_85
.LBB19_79:                              # %land.lhs.true450
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$0, 272(%rsp)           # 8-byte Folded Reload
	je	.LBB19_82
# BB#80:                                # %lor.lhs.false453
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	recog_data+240(,%r13,8), %rdx
	cmpq	%rdx, 344(%rsp)         # 8-byte Folded Reload
	jne	.LBB19_82
# BB#81:                                #   in Loop: Header=BB19_48 Depth=1
	movq	%r14, %rdi
	jmp	.LBB19_85
.LBB19_82:                              # %if.then461
                                        #   in Loop: Header=BB19_48 Depth=1
	movl	656(%rsp,%r12), %edx
	testq	%rdx, %rdx
	jne	.LBB19_84
# BB#83:                                # %if.then466
                                        #   in Loop: Header=BB19_48 Depth=1
	movzbl	2(%r14), %edx
	movl	%edx, 656(%rsp,%r12)
.LBB19_84:                              # %if.end474
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	%rdi, recog_data(,%r13,8)
	movq	%rdi, 784(%rsp,%r13,8)
.LBB19_85:                              # %if.end481
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	reg_equiv_memory_loc(%rip), %rdx
	cmpq	$0, (%rdx,%rcx,8)
	jne	.LBB19_87
# BB#86:                                #   in Loop: Header=BB19_48 Depth=1
	movq	%rdi, %r14
	jmp	.LBB19_93
.LBB19_87:                              # %land.lhs.true486
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	reg_equiv_address(%rip), %rdx
	cmpq	$0, (%rdx,%rcx,8)
	jne	.LBB19_90
# BB#88:                                # %land.lhs.true486
                                        #   in Loop: Header=BB19_48 Depth=1
	movl	num_not_at_initial_offset(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB19_90
# BB#89:                                #   in Loop: Header=BB19_48 Depth=1
	movq	%rdi, %r14
	jmp	.LBB19_93
.LBB19_90:                              # %if.then493
                                        #   in Loop: Header=BB19_48 Depth=1
	xorq	%r8, %r8
	movq	%r15, (%rsp)
	jmp	.LBB19_91
	.align	16, 0x90
.LBB19_48:                              # %for.body215
                                        # =>This Inner Loop Header: Depth=1
	movq	recog_data(,%r13,8), %r14
	movl	2960(%rsp,%r12), %eax
	movzbl	(%r14), %ecx
	leaq	3344(%rsp,%r12), %r15
	xorq	%rdx, %rdx
	movl	%edx, 3344(%rsp,%r12)
	testq	%rax, %rax
	jne	.LBB19_50
# BB#49:                                # %cond.end233.thread
                                        #   in Loop: Header=BB19_48 Depth=1
	xorq	%r9, %r9
	movl	%r9d, 3216(%rsp,%r12)
	movq	$3, %rax
	jmp	.LBB19_53
	.align	16, 0x90
.LBB19_50:                              # %cond.false242
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$2, %rax
	movabsq	$0, %rax
	je	.LBB19_52
# BB#51:                                # %cond.false242
                                        #   in Loop: Header=BB19_48 Depth=1
	movabsq	$1, %rax
.LBB19_52:                              # %cond.false242
                                        #   in Loop: Header=BB19_48 Depth=1
	leaq	1(,%rax,8), %r9
	movl	%r9d, 3216(%rsp,%r12)
	leaq	5(,%rax,4), %rax
.LBB19_53:                              # %cond.end248
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	3632(%rsp,%r13,8), %rdx
	movl	%eax, 3088(%rsp,%r12)
	movzbl	(%rdx), %edx
	testq	%rdx, %rdx
	je	.LBB19_93
# BB#54:                                # %cond.end248
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$112, %rdx
	jne	.LBB19_61
# BB#55:                                # %if.then265
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	recog_data+240(,%r13,8), %rcx
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r14, %rdx
	movq	%r13, %r8
	callq	find_reloads_address
	movq	recog_data+240(,%r13,8), %rax
	movq	(%rax), %r14
	movzbl	(%r14), %eax
	movzbl	rtx_class(%rax), %ecx
	cmpq	$111, %rcx
	je	.LBB19_57
# BB#56:                                # %if.then265
                                        #   in Loop: Header=BB19_48 Depth=1
	cmpq	$63, %rax
	jne	.LBB19_92
.LBB19_57:                              # %land.lhs.true289
                                        #   in Loop: Header=BB19_48 Depth=1
	movq	recog_data(,%r13,8), %rax
	movzbl	(%rax), %eax
	cmpq	$75, %rax
	jne	.LBB19_58
.LBB19_59:                              # %if.then303
	movq	208(%rsp), %rdi         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 40(%rdi)
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	192(%rsp), %rdx         # 8-byte Reload
	movq	280(%rsp), %rcx         # 8-byte Reload
	movq	264(%rsp), %r8          # 8-byte Reload
	callq	find_reloads
	movq	%rax, %rcx
	jmp	.LBB19_60
.LBB19_101:                             # %for.end564
	movq	272(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	movq	320(%rsp), %r13         # 8-byte Reload
	je	.LBB19_28
# BB#102:                               # %for.end564
	movq	%rax, %rdx
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	296(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r11
	jl	.LBB19_106
# BB#103:                               # %land.lhs.true570
	movq	recog_data(%rip), %rax
	movq	8(%rdx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_106
# BB#104:                               # %land.lhs.true576
	movq	recog_data+8(%rip), %rax
	movq	16(%rdx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_106
# BB#105:                               # %if.then582
	movslq	3504(%rsp), %rax
	movslq	3508(%rsp), %rcx
	imulq	$100, %rax, %rax
	movl	reg_class_subunion(%rax,%rcx,4), %eax
	movl	%eax, 3508(%rsp)
	movl	%eax, 3504(%rsp)
	movzbl	3473(%rsp), %eax
	movzbl	3472(%rsp), %edx
	orq	%rdx, %rax
	movb	%al, 3472(%rsp)
	movq	$8, %rcx
	shrq	%cl, %rdx
	orq	%rax, %rdx
	movb	%dl, 3473(%rsp)
	jmp	.LBB19_106
.LBB19_28:
	movq	296(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r11
.LBB19_106:                             # %try_swapped.preheader
	leaq	-1(%r14), %rcx
	andq	%r11, %rcx
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	leaq	4(,%rcx,4), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	leaq	1(%rcx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$660, 88(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x294
	movabsq	$0, %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 216(%rsp)           # 8-byte Folded Spill
	andq	$1, 176(%rsp)           # 8-byte Folded Spill
	movq	168(%rsp), %rax         # 8-byte Reload
	jmp	.LBB19_107
	.align	16, 0x90
.LBB19_515:                             # %for.end2494
                                        #   in Loop: Header=BB19_107 Depth=1
	movl	3504(%rsp,%r9,4), %eax
	movl	3504(%rsp,%r8,4), %ecx
	movl	%ecx, 3504(%rsp,%r9,4)
	movzbl	3472(%rsp,%r9), %ecx
	movzbl	3472(%rsp,%r8), %edx
	movl	%eax, 3504(%rsp,%r8,4)
	xorq	$1, 216(%rsp)           # 8-byte Folded Spill
	movb	%dl, 3472(%rsp,%r9)
	movb	%cl, 3472(%rsp,%r8)
	leaq	3632(%rsp), %rdi
	movabsq	$recog_data+480, %rsi
	movq	128(%rsp), %rdx         # 8-byte Reload
	callq	memcpy
	movq	%r14, %r11
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%r15, %r14
.LBB19_107:                             # %try_swapped
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_109 Depth 2
                                        #       Child Loop BB19_111 Depth 3
                                        #         Child Loop BB19_117 Depth 4
                                        #         Child Loop BB19_140 Depth 4
                                        #           Child Loop BB19_182 Depth 5
                                        #           Child Loop BB19_149 Depth 5
                                        #       Child Loop BB19_465 Depth 3
                                        #         Child Loop BB19_473 Depth 4
                                        #         Child Loop BB19_489 Depth 4
                                        #     Child Loop BB19_511 Depth 2
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_508
# BB#108:                               # %for.cond607.preheader.lr.ph
                                        #   in Loop: Header=BB19_107 Depth=1
	xorq	%rbx, %rbx
.LBB19_109:                             # %for.cond607.preheader
                                        #   Parent Loop BB19_107 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_111 Depth 3
                                        #         Child Loop BB19_117 Depth 4
                                        #         Child Loop BB19_140 Depth 4
                                        #           Child Loop BB19_182 Depth 5
                                        #           Child Loop BB19_149 Depth 5
                                        #       Child Loop BB19_465 Depth 3
                                        #         Child Loop BB19_473 Depth 4
                                        #         Child Loop BB19_489 Depth 4
	movq	%rbx, 120(%rsp)         # 8-byte Spill
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r9
	movabsq	$0, %rax
	movq	%rax, 256(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 248(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 288(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	jle	.LBB19_110
	.align	16, 0x90
.LBB19_111:                             # %for.body610
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB19_117 Depth 4
                                        #         Child Loop BB19_140 Depth 4
                                        #           Child Loop BB19_182 Depth 5
                                        #           Child Loop BB19_149 Depth 5
	movq	3632(%rsp,%r9,8), %rbx
	movq	recog_data(,%r9,8), %r10
	movq	%rbx, 640(%rsp)
	movl	(%r10), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_class(%rax), %eax
	cmpq	$49, %rax
	jne	.LBB19_114
# BB#112:                               # %land.lhs.true624
                                        #   in Loop: Header=BB19_111 Depth=3
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_114
# BB#113:                               # %if.then628
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	8(%r10), %r10
	movl	(%r10), %r8d
.LBB19_114:                             # %while.cond633.preheader
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%r12, %r12
	cmpq	$63, %rax
	jne	.LBB19_115
# BB#116:                               # %while.body638.lr.ph.lr.ph
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$0, 344(%rsp)           # 8-byte Folded Spill
	movq	$0, 272(%rsp)           # 8-byte Folded Spill
.LBB19_117:                             # %while.body638
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_111 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%r10), %rax
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB19_118
# BB#119:                               # %land.lhs.true646
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	%r14, %r12
	movl	8(%rax), %edi
	cmpq	$52, %rdi
	ja	.LBB19_120
# BB#121:                               # %if.then655
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r10), %edx
	shrq	%cl, %r8
	andq	$255, %rsi
	andq	$255, %r8
	movq	%r8, %rcx
	movq	%r9, %r15
	movq	%r10, %r14
	callq	subreg_regno_offset
	movq	%r14, %r10
	movq	%r15, %r9
	movq	8(%r10), %r10
	movl	(%r10), %r8d
	addq	%rax, 272(%rsp)         # 8-byte Folded Spill
	jmp	.LBB19_122
	.align	16, 0x90
.LBB19_118:                             #   in Loop: Header=BB19_117 Depth=4
	movq	%rsi, %r8
	movq	%rax, %r10
	jmp	.LBB19_123
	.align	16, 0x90
.LBB19_120:                             #   in Loop: Header=BB19_117 Depth=4
	movq	%rsi, %r8
	movq	%rax, %r10
.LBB19_122:                             # %if.end676
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	%r12, %r14
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r11
	xorq	%r12, %r12
.LBB19_123:                             # %if.end676
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	movq	$1, %rdx
	cmpq	$133, %rax
	jg	.LBB19_126
# BB#124:                               # %if.end676
                                        #   in Loop: Header=BB19_117 Depth=4
	leaq	-54(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$21, %rcx
	ja	.LBB19_133
# BB#125:                               # %if.end676
                                        #   in Loop: Header=BB19_117 Depth=4
	jmpq	*.LJTI19_0(,%rcx,8)
.LBB19_128:                             # %land.lhs.true734
                                        #   in Loop: Header=BB19_117 Depth=4
	movl	8(%r10), %ecx
	cmpq	$52, %rcx
	jbe	.LBB19_133
.LBB19_129:                             # %land.lhs.true740
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	$16, %rcx
	movq	%r8, %rdx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	$127, %rcx
	ja	.LBB19_132
# BB#130:                               # %land.lhs.true749
                                        #   in Loop: Header=BB19_117 Depth=4
	movslq	656(%rsp,%r9,4), %rcx
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%rdx), %edx
	cmpq	%rdx, %rcx
	ja	.LBB19_131
.LBB19_132:                             # %while.cond633.backedge
                                        #   in Loop: Header=BB19_117 Depth=4
	cmpq	$63, %rax
	je	.LBB19_117
	jmp	.LBB19_133
.LBB19_131:                             # %if.then769
                                        #   in Loop: Header=BB19_117 Depth=4
	movq	$1, 344(%rsp)           # 8-byte Folded Spill
	cmpq	$63, %rax
	movq	$1, %rdx
	je	.LBB19_117
	jmp	.LBB19_134
	.align	16, 0x90
.LBB19_115:                             #   in Loop: Header=BB19_111 Depth=3
	xorq	%rdx, %rdx
	movq	$0, 272(%rsp)           # 8-byte Folded Spill
	jmp	.LBB19_134
	.align	16, 0x90
.LBB19_126:                             # %if.end676
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$134, %rax
	je	.LBB19_134
# BB#127:                               # %if.end676
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$140, %rax
	je	.LBB19_134
.LBB19_133:                             # %while.end771.loopexit
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %rdx         # 8-byte Reload
	.align	16, 0x90
.LBB19_134:                             # %while.end771
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r10, 264(%rsp)         # 8-byte Spill
	movl	%r12d, 2832(%rsp,%r9,4)
	movb	%r12b, 2768(%rsp,%r9)
	movb	%r12b, 2800(%rsp,%r9)
	movb	%r12b, 2736(%rsp,%r9)
	movb	%r12b, 2704(%rsp,%r9)
	movl	%r11d, 2576(%rsp,%r9,4)
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_136
# BB#135:                               # %while.end771
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, %rdi
	cmpq	$44, %rax
	jne	.LBB19_137
.LBB19_136:                             # %if.then791
                                        #   in Loop: Header=BB19_111 Depth=3
	xorq	%rdi, %rdi
	movq	$1, %r12
.LBB19_137:                             # %while.cond793.preheader
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%rax, %rax
	jne	.LBB19_139
# BB#138:                               # %while.end1915.thread
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%rbx, 3632(%rsp,%r9,8)
	xorq	%rcx, %rcx
	xorq	%rsi, %rsi
	movq	$0, 240(%rsp)           # 8-byte Folded Spill
	xorq	%rax, %rax
	xorq	%r15, %r15
	movq	%r11, 312(%rsp)         # 8-byte Spill
	jmp	.LBB19_397
	.align	16, 0x90
.LBB19_139:                             # %land.rhs796.lr.ph
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%rdi, 320(%rsp)         # 8-byte Spill
	movq	%rdx, 336(%rsp)         # 8-byte Spill
	movq	%r9, 344(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	%r11, 312(%rsp)         # 8-byte Spill
	movq	$0, 232(%rsp)           # 8-byte Folded Spill
	movq	$0, 240(%rsp)           # 8-byte Folded Spill
	movq	$0, 224(%rsp)           # 8-byte Folded Spill
	movq	$0, 328(%rsp)           # 8-byte Folded Spill
	movq	$0, 304(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB19_140:                             # %land.rhs796
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_111 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB19_182 Depth 5
                                        #           Child Loop BB19_149 Depth 5
	leaq	1(%rbx), %rcx
	movq	%rcx, 640(%rsp)
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$44, %rdx
	je	.LBB19_141
# BB#142:                               # %while.body803
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	addq	$-33, %rsi
	andq	%r11, %rsi
	cmpq	$82, %rsi
	ja	.LBB19_310
# BB#143:                               # %while.body803
                                        #   in Loop: Header=BB19_140 Depth=4
	jmpq	*.LJTI19_1(,%rsi,8)
.LBB19_152:                             # %sw.bb826
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, 280(%rsp)         # 8-byte Spill
	movq	$10, %rdx
	movq	%r11, %r14
	movq	%rbx, %rdi
	leaq	640(%rsp), %rsi
	callq	strtoul
	movq	%rax, %rcx
	movq	216(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	movq	344(%rsp), %r8          # 8-byte Reload
	movl	%ecx, 2576(%rsp,%r8,4)
	movq	%rcx, %rdi
	je	.LBB19_167
# BB#153:                               # %land.lhs.true833
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rdi, %rdx
	andq	%r14, %rdx
	movq	%r13, %rbx
	andq	%r14, %rbx
	leaq	1(%r13), %rax
	cmpq	%rbx, %rdx
	jne	.LBB19_155
# BB#154:                               # %land.lhs.true833
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%r8, %rsi
	andq	%r14, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB19_155
.LBB19_167:                             # %cond.false873
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$30, %rax, %rcx
	leaq	1024(%rsp,%rcx), %rcx
	movzbl	(%r8,%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB19_162
	jmp	.LBB19_168
	.align	16, 0x90
.LBB19_310:                             # %sw.default
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	movq	$12, %rdi
	cmpq	$114, %rdx
	je	.LBB19_383
# BB#311:                               # %cond.false1600
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$11, %rdi
	cmpq	$82, %rdx
	je	.LBB19_383
# BB#312:                               # %cond.false1600
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$113, %rdx
	jne	.LBB19_314
# BB#313:                               # %cond.false1600.cond.true1758_crit_edge
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
.LBB19_356:                             # %cond.true1758
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$23, %rcx
	shrq	%cl, %rsi
	orq	$8, %rsi
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_281:                             # %sw.bb1394
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %ecx
	cmpq	$54, %rcx
	jne	.LBB19_388
# BB#282:                               # %land.lhs.true1399
                                        #   in Loop: Header=BB19_140 Depth=4
	addq	$-73, %rax
	andq	$255, %rax
	cmpq	$5, %rax
	ja	.LBB19_388
# BB#283:                               # %land.lhs.true1399
                                        #   in Loop: Header=BB19_140 Depth=4
	jmpq	*.LJTI19_2(,%rax,8)
.LBB19_284:                             # %cond.true1402
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	cmpq	$32, %rax
	jb	.LBB19_291
	jmp	.LBB19_388
.LBB19_314:                             # %cond.false1611
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$101, %rsi
	jg	.LBB19_325
# BB#315:                               # %cond.false1611
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$81, %rdx
	movq	$8, %rdi
	je	.LBB19_383
	jmp	.LBB19_316
.LBB19_325:                             # %cond.false1611
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r11
	cmpq	$117, %rdx
	je	.LBB19_338
# BB#326:                               # %cond.false1611
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$116, %rdx
	movq	%r11, %r12
	jne	.LBB19_327
# BB#333:                               # %cond.true1627
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$1, %rsi
	je	.LBB19_329
	jmp	.LBB19_334
.LBB19_155:                             # %cond.true840
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r8, %rcx
	andq	%r14, %rcx
	cmpq	%rbx, %rcx
	je	.LBB19_157
# BB#156:                               # %cond.true840
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rax, %rsi
	andq	%r14, %rsi
	cmpq	%rsi, %rcx
	movq	%r8, %rsi
	jne	.LBB19_158
.LBB19_157:                             # %cond.true847
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	1(%r13,%r13), %rsi
	subq	%r8, %rsi
.LBB19_158:                             # %cond.end852
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rdx
	je	.LBB19_160
# BB#159:                               # %cond.end852
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	%r14, %rax
	movq	%rdi, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	movq	%rdi, %rax
	jne	.LBB19_161
.LBB19_160:                             # %cond.true861
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	1(%r13,%r13), %rax
	subq	%rdi, %rax
.LBB19_161:                             # %cond.end866
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$30, %rax, %rax
	leaq	1024(%rsp,%rax), %rax
	movzbl	(%rsi,%rax), %edx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rdx, %rdx
	jne	.LBB19_168
.LBB19_162:                             # %if.else906
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r13, 320(%rsp)         # 8-byte Spill
	movq	%rdi, 312(%rsp)         # 8-byte Spill
	movzbl	2768(%rsp,%rax), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rbx
	je	.LBB19_164
# BB#163:                               # %if.else906
                                        #   in Loop: Header=BB19_140 Depth=4
	movabsq	$1, %rbx
.LBB19_164:                             # %if.else906
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	2832(%rsp,%rax,4), %r10d
	movq	288(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rbx
	xorq	%rcx, %rcx
	movb	%cl, 2768(%rsp,%rax)
	testq	%r10, %r10
	movq	$1, %r13
	je	.LBB19_166
# BB#165:                               # %if.else906
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	248(%rsp), %r13         # 8-byte Reload
.LBB19_166:                             # %if.else906
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	recog_data(,%r8,8), %rdi
	movq	recog_data(,%rax,8), %rsi
	movq	recog_data+240(,%r8,8), %rdx
	movq	recog_data+240(,%rax,8), %rcx
	movsbq	2704(%rsp,%rax), %r11
	movq	%r8, %r15
	movl	656(%rsp,%rax,4), %r9d
	movl	656(%rsp,%r15,4), %r8d
	leaq	2832(%rsp,%rax,4), %r12
	movq	%r11, 16(%rsp)
	movq	%r14, 8(%rsp)
	movq	%r10, (%rsp)
	callq	find_dummy_reload
	movq	%r15, %r8
	movq	%r14, %r11
	negq	%rax
	sbbq	%rax, %rax
	addq	%rax, %rbx
	movq	%rbx, 288(%rsp)         # 8-byte Spill
	movq	%r13, 248(%rsp)         # 8-byte Spill
	movq	280(%rsp), %r9          # 8-byte Reload
	movq	264(%rsp), %r10         # 8-byte Reload
	movq	304(%rsp), %rcx         # 8-byte Reload
	movq	336(%rsp), %rdx         # 8-byte Reload
	movq	312(%rsp), %rdi         # 8-byte Reload
	movq	320(%rsp), %r13         # 8-byte Reload
	jmp	.LBB19_177
.LBB19_168:                             # %if.then880
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	2736(%rsp,%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_169
# BB#170:                               # %land.lhs.true885
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	recog_data(,%rax,8), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$66, %rcx
	jne	.LBB19_169
# BB#171:                               # %land.lhs.true892
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	2832(%rsp,%rax,4), %ecx
	testq	%rcx, %rcx
	movq	%r14, %r11
	jne	.LBB19_172
# BB#173:                               # %land.lhs.true897
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	2768(%rsp,%rax), %ecx
	testq	%rcx, %rcx
	movq	$1, %rcx
	movq	280(%rsp), %r9          # 8-byte Reload
	movq	336(%rsp), %rdx         # 8-byte Reload
	je	.LBB19_175
# BB#174:                               # %select.mid
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	248(%rsp), %rcx         # 8-byte Reload
.LBB19_175:                             # %select.end
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rcx, 248(%rsp)         # 8-byte Spill
	jmp	.LBB19_176
.LBB19_169:                             #   in Loop: Header=BB19_140 Depth=4
	movq	%r14, %r11
.LBB19_172:                             #   in Loop: Header=BB19_140 Depth=4
	movq	280(%rsp), %r9          # 8-byte Reload
	movq	336(%rsp), %rdx         # 8-byte Reload
.LBB19_176:                             # %if.end902
                                        #   in Loop: Header=BB19_140 Depth=4
	movsbq	2768(%rsp,%rax), %rcx
	leaq	2832(%rsp,%rax,4), %r12
	movq	264(%rsp), %r10         # 8-byte Reload
.LBB19_177:                             # %if.end943
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r12), %r15d
	movl	%r15d, 2832(%rsp,%r8,4)
	testq	%r11, %rcx
	je	.LBB19_180
# BB#178:                               # %if.end943
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	%r11, %rdx
	jne	.LBB19_180
# BB#179:                               #   in Loop: Header=BB19_140 Depth=4
	movq	%rdi, 312(%rsp)         # 8-byte Spill
	movq	%rdx, 336(%rsp)         # 8-byte Spill
	movq	%rcx, 304(%rsp)         # 8-byte Spill
	movq	%r8, 344(%rsp)          # 8-byte Spill
	movq	$0, 320(%rsp)           # 8-byte Folded Spill
	movq	%r9, %r12
	jmp	.LBB19_388
.LBB19_180:                             # %for.cond952.preheader
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rdx, 336(%rsp)         # 8-byte Spill
	movq	%rcx, 304(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r8, %rax
	movq	%r8, 344(%rsp)          # 8-byte Spill
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movq	$1, %rax
	movq	256(%rsp), %rbx         # 8-byte Reload
	jle	.LBB19_181
	.align	16, 0x90
.LBB19_182:                             # %for.body955
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_111 Depth=3
                                        #         Parent Loop BB19_140 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movl	2572(%rsp,%rax,4), %ecx
	movq	%rdi, %rdx
	andq	%r11, %rdx
	cmpq	%rdx, %rcx
	movq	$1, %rcx
	je	.LBB19_184
# BB#183:                               # %select.mid366
                                        #   in Loop: Header=BB19_182 Depth=5
	movq	%rsi, %rcx
.LBB19_184:                             # %select.end365
                                        #   in Loop: Header=BB19_182 Depth=5
	movq	%rax, %rdx
	andq	%r11, %rdx
	movq	%rbx, %rsi
	andq	%r11, %rsi
	incq	%rax
	cmpq	%rsi, %rdx
	movq	%rcx, %rsi
	jne	.LBB19_182
# BB#185:                               #   in Loop: Header=BB19_140 Depth=4
	movq	%rdi, 312(%rsp)         # 8-byte Spill
	movq	%rcx, 320(%rsp)         # 8-byte Spill
	movq	%r9, %r12
	jmp	.LBB19_388
.LBB19_181:                             #   in Loop: Header=BB19_140 Depth=4
	movq	$0, 320(%rsp)           # 8-byte Folded Spill
	movq	%rdi, 312(%rsp)         # 8-byte Spill
	movq	%r9, %r12
	jmp	.LBB19_388
.LBB19_254:                             # %sw.bb1273
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$55, %rax
	jne	.LBB19_388
# BB#255:                               # %land.lhs.true1278
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	cmpq	$72, %rdx
	jne	.LBB19_256
# BB#260:                               # %cond.true1287
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	movq	%r10, %rdi
	callq	standard_sse_constant_p
	jmp	.LBB19_258
.LBB19_147:                             # %sw.bb812
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$600, 200(%rsp)         # 8-byte Folded Spill
                                        # imm = 0x258
	jmp	.LBB19_388
.LBB19_148:                             # %while.cond814.preheader
                                        #   in Loop: Header=BB19_140 Depth=4
	addq	$2, %rbx
	jmp	.LBB19_149
.LBB19_144:                             # %sw.bb804
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	344(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	andq	%r11, %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB19_388
# BB#145:                               # %sw.bb804
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rdx, %r13
	jmp	.LBB19_388
.LBB19_247:                             # %sw.bb1258
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 160(%rsp)           # 8-byte Folded Spill
	movq	$1, 232(%rsp)           # 8-byte Folded Spill
	jmp	.LBB19_388
.LBB19_201:                             # %sw.bb1047
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	jne	.LBB19_388
# BB#202:                               # %land.lhs.true1052
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	3344(%rsp,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB19_388
# BB#203:                               # %land.lhs.true1056
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$98, %rax
	jne	.LBB19_388
# BB#204:                               # %if.then1072
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_205:                             # %sw.bb1074
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	jne	.LBB19_388
# BB#206:                               # %land.lhs.true1079
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	3344(%rsp,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB19_388
# BB#207:                               # %land.lhs.true1083
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$99, %rax
	jne	.LBB19_388
# BB#208:                               # %if.then1099
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_146:                             # %sw.bb810
                                        #   in Loop: Header=BB19_140 Depth=4
	addq	$6, 200(%rsp)           # 8-byte Folded Spill
	jmp	.LBB19_388
.LBB19_248:                             # %sw.bb1259
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$55, %rax
	movq	$1, %rax
	je	.LBB19_250
# BB#249:                               # %sw.bb1259
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %rax
.LBB19_250:                             # %sw.bb1259
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rax, %r12
	jmp	.LBB19_388
.LBB19_251:                             # %sw.bb1266
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$55, %rax
	movq	$1, %rax
	je	.LBB19_253
# BB#252:                               # %sw.bb1266
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %rax
.LBB19_253:                             # %sw.bb1266
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rax, %r12
	jmp	.LBB19_388
.LBB19_209:                             # %sw.bb1101
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	336(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_388
# BB#210:                               # %if.end1104
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	jne	.LBB19_388
# BB#211:                               # %land.lhs.true1109
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	movq	192(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	movq	%r11, %rbx
	movq	%r10, %rdi
	movq	%r10, %r12
	jne	.LBB19_212
# BB#215:                               # %cond.false1114
                                        #   in Loop: Header=BB19_140 Depth=4
	callq	offsettable_nonstrict_memref_p
	jmp	.LBB19_213
.LBB19_292:                             # %sw.bb1492
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	336(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_309
# BB#293:                               # %land.lhs.true1494
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$133, %rax
	jg	.LBB19_297
# BB#294:                               # %land.lhs.true1494
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	-54(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$21, %rcx
	ja	.LBB19_298
# BB#295:                               # %land.lhs.true1494
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB19_300
# BB#296:                               # %land.lhs.true1494
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$2097408, %rdx          # imm = 0x200100
	jne	.LBB19_309
	jmp	.LBB19_298
.LBB19_265:                             # %while.body803.sw.bb1311_crit_edge
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r10), %eax
	jmp	.LBB19_266
.LBB19_187:                             # %sw.bb971
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	336(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_388
# BB#188:                               # %if.end974
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$66, %rax
	je	.LBB19_192
# BB#189:                               # %if.end974
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$61, %rax
	jne	.LBB19_193
# BB#190:                               # %land.lhs.true984
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	8(%r10), %ecx
	cmpq	$53, %rcx
	movq	$1, 240(%rsp)           # 8-byte Folded Spill
	jb	.LBB19_388
# BB#191:                               # %land.lhs.true990
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	reg_renumber(%rip), %rdx
	movswq	(%rdx,%rcx,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB19_193
.LBB19_192:                             # %if.then999
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
.LBB19_193:                             # %if.end1000
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 240(%rsp)           # 8-byte Folded Spill
	cmpq	$133, %rax
	jg	.LBB19_196
# BB#194:                               # %if.end1000
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	-54(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_388
# BB#195:                               # %if.end1000
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	movq	320(%rsp), %rcx         # 8-byte Reload
	jne	.LBB19_198
	jmp	.LBB19_388
.LBB19_277:                             # %sw.bb1376
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB19_280
# BB#278:                               # %sw.bb1376
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$55, %rcx
	jne	.LBB19_388
# BB#279:                               # %land.lhs.true1386
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB19_388
.LBB19_280:                             # %if.then1392
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_222:                             # %sw.bb1141
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	336(%rsp), %rdx         # 8-byte Reload
	testq	%r11, %rdx
	jne	.LBB19_388
# BB#223:                               # %if.end1144
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB19_230
# BB#224:                               # %land.lhs.true1149
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, 280(%rsp)         # 8-byte Spill
	movq	192(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	movq	%r11, %rbx
	movq	%r10, %rdi
	movq	%r10, %r14
	jne	.LBB19_225
# BB#226:                               # %cond.false1154
                                        #   in Loop: Header=BB19_140 Depth=4
	callq	offsettable_nonstrict_memref_p
	jmp	.LBB19_227
.LBB19_186:                             # %sw.bb968
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$12, %r15
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	%r15d, 2832(%rsp,%rax,4)
	movq	$0, 320(%rsp)           # 8-byte Folded Spill
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_262:                             # %sw.bb1293
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB19_388
# BB#263:                               # %sw.bb1293
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$55, %rcx
	jne	.LBB19_266
# BB#264:                               # %land.lhs.true1303
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$16711680, %rax         # imm = 0xFF0000
	je	.LBB19_388
.LBB19_266:                             # %sw.bb1311
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$133, %rax
	jg	.LBB19_269
# BB#267:                               # %sw.bb1311
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	-54(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_388
# BB#268:                               # %sw.bb1311
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB19_271
	jmp	.LBB19_388
	.align	16, 0x90
.LBB19_151:                             # %while.body823
                                        #   in Loop: Header=BB19_149 Depth=5
	movq	%rbx, 640(%rsp)
	incq	%rbx
.LBB19_149:                             # %while.cond814
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_111 Depth=3
                                        #         Parent Loop BB19_140 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movzbl	-1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_388
# BB#150:                               # %while.cond814
                                        #   in Loop: Header=BB19_149 Depth=5
	cmpq	$44, %rax
	jne	.LBB19_151
	jmp	.LBB19_388
.LBB19_338:                             # %cond.true1638
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$1, %rsi
	movq	%r11, %r12
	je	.LBB19_329
	jmp	.LBB19_339
.LBB19_327:                             # %cond.false1611
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$102, %rdx
	jne	.LBB19_316
# BB#328:                               # %cond.true1618
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$1, %rsi
	jne	.LBB19_373
.LBB19_329:                             # %lor.rhs
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$32, %rsi
	je	.LBB19_330
# BB#354:                               # %cond.false1751
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$82, %rdx
	je	.LBB19_383
# BB#355:                               # %cond.false1751
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$113, %rdx
	je	.LBB19_356
# BB#357:                               # %cond.false1762
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	cmpq	$101, %rdi
	jg	.LBB19_370
# BB#358:                               # %cond.false1762
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$81, %rdx
	movq	$8, %rdi
	je	.LBB19_383
	jmp	.LBB19_359
.LBB19_316:                             # %cond.false1646
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdi
	cmpq	$97, %rdx
	je	.LBB19_383
# BB#317:                               # %cond.false1650
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$4, %rdi
	cmpq	$98, %rdx
	je	.LBB19_383
# BB#318:                               # %cond.false1654
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$3, %rdi
	cmpq	$99, %rdx
	je	.LBB19_383
# BB#319:                               # %cond.false1658
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$119, %rsi
	jg	.LBB19_343
# BB#320:                               # %cond.false1658
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$89, %rdx
	jne	.LBB19_321
# BB#352:                               # %cond.end1728
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$262144, %rsi           # imm = 0x40000
	je	.LBB19_387
	jmp	.LBB19_353
.LBB19_269:                             # %sw.bb1311
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$134, %rax
	je	.LBB19_271
# BB#270:                               # %sw.bb1311
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$140, %rax
	jne	.LBB19_388
.LBB19_271:                             # %land.lhs.true1351
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB19_276
# BB#272:                               # %lor.lhs.false1353
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	-67(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$2, %rcx
	jb	.LBB19_275
# BB#273:                               # %lor.lhs.false1353
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$58, %rax
	jne	.LBB19_276
# BB#274:                               # %land.lhs.true1368
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %r14
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	symbolic_reference_mentioned_p
	movq	%rbx, %r10
	movq	%r14, %r11
	testq	%r11, %rax
	je	.LBB19_276
.LBB19_275:                             # %lor.lhs.false1371
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %r14
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	legitimate_pic_address_disp_p
	movq	%rbx, %r10
	movq	%r14, %r11
	testq	%r11, %rax
	je	.LBB19_388
.LBB19_276:                             # %if.then1374
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_256:                             # %land.lhs.true1278
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$71, %rdx
	jne	.LBB19_387
# BB#257:                               # %cond.true1281
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	movq	%r10, %rdi
	callq	standard_80387_constant_p
	jmp	.LBB19_258
.LBB19_285:                             # %cond.true1416
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	cmpq	$64, %rax
	jb	.LBB19_291
	jmp	.LBB19_388
.LBB19_286:                             # %cond.true1431
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	addq	$128, %rax
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB19_291
	jmp	.LBB19_388
.LBB19_287:                             # %cond.true1446
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	cmpq	$255, %rax
	je	.LBB19_291
# BB#288:                               # %cond.true1446
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$65535, %rax            # imm = 0xFFFF
	je	.LBB19_291
	jmp	.LBB19_388
.LBB19_289:                             # %cond.true1461
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	cmpq	$4, %rax
	jb	.LBB19_291
	jmp	.LBB19_388
.LBB19_290:                             # %cond.true1476
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r10), %rax
	cmpq	$255, %rax
	ja	.LBB19_388
.LBB19_291:                             # %sw.bb1491
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_297:                             # %land.lhs.true1494
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$134, %rax
	jne	.LBB19_298
.LBB19_300:                             # %lor.lhs.false1544
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	flag_pic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB19_305
	jmp	.LBB19_301
.LBB19_298:                             # %lor.lhs.false1539
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$140, %rax
	jne	.LBB19_305
# BB#299:                               # %lor.lhs.false1539
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	flag_pic(%rip), %ecx
	testq	%r11, %rcx
	je	.LBB19_305
.LBB19_301:                             # %lor.lhs.false1546
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	-67(%rax), %rcx
	andq	%r11, %rcx
	cmpq	$2, %rcx
	jb	.LBB19_304
# BB#302:                               # %lor.lhs.false1546
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$58, %rax
	jne	.LBB19_305
# BB#303:                               # %land.lhs.true1561
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %r14
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	symbolic_reference_mentioned_p
	movq	%rbx, %r10
	movq	%r14, %r11
	testq	%r11, %rax
	je	.LBB19_305
.LBB19_304:                             # %lor.lhs.false1564
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %r14
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	legitimate_pic_address_disp_p
	movq	%rbx, %r10
	movq	%r14, %r11
	testq	%r11, %rax
	je	.LBB19_309
.LBB19_305:                             # %land.lhs.true1567
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %eax
	cmpq	$61, %rax
	jne	.LBB19_308
# BB#306:                               # %lor.lhs.false1572
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	8(%r10), %eax
	cmpq	$53, %rax
	jb	.LBB19_309
# BB#307:                               # %land.lhs.true1578
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rax
	testq	%rax, %rax
	jns	.LBB19_309
.LBB19_308:                             # %if.then1587
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
.LBB19_309:                             # %sw.bb1589
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	imulq	$100, %r15, %rax
	leaq	reg_class_subunion+48(%rax), %rax
	jmp	.LBB19_384
.LBB19_196:                             # %if.end1000
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$134, %rax
	movq	320(%rsp), %rcx         # 8-byte Reload
	je	.LBB19_198
# BB#197:                               # %if.end1000
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$140, %rax
	jne	.LBB19_388
.LBB19_198:                             # %land.lhs.true1040
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$134, %rax
	je	.LBB19_200
# BB#199:                               # %land.lhs.true1040
                                        #   in Loop: Header=BB19_140 Depth=4
	xorq	%rcx, %rcx
.LBB19_200:                             # %land.lhs.true1040
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rcx, 320(%rsp)         # 8-byte Spill
	jmp	.LBB19_388
.LBB19_212:                             # %cond.true1111
                                        #   in Loop: Header=BB19_140 Depth=4
	callq	offsettable_memref_p
.LBB19_213:                             # %cond.true1111
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	%rbx, %rax
	movq	%rbx, %r11
	jne	.LBB19_214
# BB#216:                               # %land.lhs.true1117
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	8(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	%r12, %r10
	jne	.LBB19_221
# BB#217:                               # %land.lhs.true1125
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	192(%rsp), %rcx         # 8-byte Reload
	testq	%r11, %rcx
	jne	.LBB19_219
# BB#218:                               #   in Loop: Header=BB19_140 Depth=4
	movq	%r14, %r12
	jmp	.LBB19_388
.LBB19_225:                             # %cond.true1151
                                        #   in Loop: Header=BB19_140 Depth=4
	callq	offsettable_memref_p
.LBB19_227:                             # %cond.false1154
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r14, %r10
	testq	%rbx, %rax
	movq	%rbx, %r11
	jne	.LBB19_239
# BB#228:                               # %lor.lhs.false1157
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	3344(%rsp,%rax,4), %eax
	testq	%rax, %rax
	jne	.LBB19_239
# BB#229:                               # %lor.lhs.false1157.lor.lhs.false1161_crit_edge
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%r10), %eax
	movq	336(%rsp), %rdx         # 8-byte Reload
	movq	280(%rsp), %r12         # 8-byte Reload
.LBB19_230:                             # %lor.lhs.false1161
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB19_231
# BB#232:                               # %land.lhs.true1166
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	8(%r10), %eax
	cmpq	$53, %rax
	jb	.LBB19_233
# BB#234:                               # %land.lhs.true1172
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rdx, 336(%rsp)         # 8-byte Spill
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB19_240
# BB#235:                               # %land.lhs.true1181
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	movq	reg_equiv_mem(%rip), %rcx
	movq	(%rcx,%rax,8), %rdi
	testq	%rdi, %rdi
	je	.LBB19_238
# BB#236:                               # %land.lhs.true1189
                                        #   in Loop: Header=BB19_140 Depth=4
	callq	offsettable_memref_p
	movq	264(%rsp), %r10         # 8-byte Reload
	movq	%rbx, %r11
	testq	%r11, %rax
	jne	.LBB19_239
# BB#237:                               # %land.lhs.true1189.lor.lhs.false1197_crit_edge
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	movl	8(%r10), %eax
.LBB19_238:                             # %lor.lhs.false1197
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	reg_equiv_address(%rip), %rcx
	movq	%rbx, %r11
	andq	%r11, %rax
	cmpq	$0, (%rcx,%rax,8)
	je	.LBB19_240
.LBB19_239:                             # %if.then1205
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r14
	jmp	.LBB19_240
.LBB19_231:                             #   in Loop: Header=BB19_140 Depth=4
	movq	%rdx, 336(%rsp)         # 8-byte Spill
	jmp	.LBB19_240
.LBB19_214:                             #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r10
	movq	%r14, %r12
	jmp	.LBB19_388
.LBB19_343:                             # %cond.false1658
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$121, %rdx
	jne	.LBB19_344
# BB#346:                               # %cond.true1681
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$16384, %rsi            # imm = 0x4000
	movq	%rbx, %r11
	je	.LBB19_388
	jmp	.LBB19_347
.LBB19_233:                             #   in Loop: Header=BB19_140 Depth=4
	movq	%rdx, 336(%rsp)         # 8-byte Spill
.LBB19_240:                             # %if.end1206
                                        #   in Loop: Header=BB19_140 Depth=4
	movzbl	(%r10), %ecx
	cmpq	$139, %rcx
	jg	.LBB19_245
# BB#241:                               # %if.end1206
                                        #   in Loop: Header=BB19_140 Depth=4
	addq	$-54, %rcx
	andq	%r11, %rcx
	cmpq	$14, %rcx
	jbe	.LBB19_243
# BB#242:                               #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	movq	$1, 240(%rsp)           # 8-byte Folded Spill
	movq	%r14, %r12
	jmp	.LBB19_388
.LBB19_245:                             # %if.end1206
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$140, %rcx
	movq	%r14, %r12
	jne	.LBB19_244
	jmp	.LBB19_246
.LBB19_243:                             # %if.end1206
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$28695, %rax            # imm = 0x7017
	movq	%r14, %r12
	je	.LBB19_244
.LBB19_246:                             # %if.then1256
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	movq	$0, 320(%rsp)           # 8-byte Folded Spill
	movq	$1, 240(%rsp)           # 8-byte Folded Spill
	jmp	.LBB19_388
.LBB19_244:                             #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 224(%rsp)           # 8-byte Folded Spill
	movq	$1, 240(%rsp)           # 8-byte Folded Spill
	jmp	.LBB19_388
.LBB19_370:                             # %cond.false1762
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$117, %rdx
	je	.LBB19_339
# BB#371:                               # %cond.false1762
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$116, %rdx
	jne	.LBB19_372
.LBB19_334:                             # %cond.true1780
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$1, %rsi
	movq	$13, %rdi
	jne	.LBB19_383
# BB#335:                               # %lor.rhs1783
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$32, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB19_337
# BB#336:                               # %lor.rhs1783
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$13, %rsi
.LBB19_337:                             # %lor.rhs1783
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_339:                             # %cond.true1791
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$1, %rsi
	movq	$14, %rdi
	jne	.LBB19_383
# BB#340:                               # %lor.rhs1794
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$32, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB19_342
# BB#341:                               # %lor.rhs1794
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$14, %rsi
.LBB19_342:                             # %lor.rhs1794
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_321:                             # %cond.false1658
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$2, %rdi
	cmpq	$100, %rdx
	je	.LBB19_383
	jmp	.LBB19_322
.LBB19_344:                             # %cond.false1658
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$120, %rdx
	jne	.LBB19_322
# BB#345:                               # %cond.true1665
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	target_flags(%rip), %esi
	testq	$327680, %rsi           # imm = 0x50000
	jne	.LBB19_380
	jmp	.LBB19_387
.LBB19_322:                             # %cond.false1686
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$7, %rdi
	cmpq	$65, %rdx
	je	.LBB19_383
# BB#323:                               # %cond.false1690
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$82, %rax
	jg	.LBB19_350
# BB#324:                               # %cond.false1690
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$6, %rdi
	cmpq	$68, %rdx
	je	.LBB19_383
	jmp	.LBB19_387
.LBB19_219:                             # %lor.lhs.false1128
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	8(%rax), %eax
	movq	reg_equiv_address(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	jne	.LBB19_220
.LBB19_221:                             # %if.then1139
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	jmp	.LBB19_388
.LBB19_372:                             # %cond.false1762
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$102, %rdx
	jne	.LBB19_359
.LBB19_373:                             # %cond.true1769
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$1, %rsi
	movq	$15, %rdi
	jne	.LBB19_383
# BB#374:                               # %lor.rhs1772
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$32, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB19_376
# BB#375:                               # %lor.rhs1772
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$15, %rsi
.LBB19_376:                             # %lor.rhs1772
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_359:                             # %cond.false1799
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %rdi
	cmpq	$97, %rdx
	je	.LBB19_383
# BB#360:                               # %cond.false1803
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$4, %rdi
	cmpq	$98, %rdx
	je	.LBB19_383
# BB#361:                               # %cond.false1807
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$3, %rdi
	cmpq	$99, %rdx
	je	.LBB19_383
# BB#362:                               # %cond.false1811
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$119, %rax
	jg	.LBB19_377
# BB#363:                               # %cond.false1811
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$89, %rdx
	jne	.LBB19_364
.LBB19_353:                             # %cond.true1826
                                        #   in Loop: Header=BB19_140 Depth=4
	andq	$262144, %rsi           # imm = 0x40000
	movq	$14, %rcx
	shrq	%cl, %rsi
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_220:                             #   in Loop: Header=BB19_140 Depth=4
	movq	%r14, %r12
	jmp	.LBB19_388
.LBB19_350:                             # %cond.false1690
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$83, %rdx
	je	.LBB19_367
.LBB19_330:                             # %if.then1732
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$90, %rdx
	jne	.LBB19_331
# BB#351:                               # %cond.true1741
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	movq	%r10, %rdi
	callq	x86_64_zero_extended_value
	jmp	.LBB19_258
.LBB19_331:                             # %if.then1732
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$101, %rdx
	jne	.LBB19_387
# BB#332:                               # %cond.true1735
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r12, %r14
	movq	%r10, %rdi
	callq	x86_64_sign_extended_value
.LBB19_258:                             # %cond.true1281
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rbx, %r11
	testq	%r11, %rax
	jne	.LBB19_259
# BB#261:                               #   in Loop: Header=BB19_140 Depth=4
	movq	264(%rsp), %r10         # 8-byte Reload
	movq	%r14, %r12
	jmp	.LBB19_388
.LBB19_259:                             # %if.then1291
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
	movq	264(%rsp), %r10         # 8-byte Reload
	jmp	.LBB19_388
.LBB19_377:                             # %cond.false1811
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$121, %rdx
	jne	.LBB19_379
# BB#378:                               #   in Loop: Header=BB19_140 Depth=4
	movq	%rbx, %r11
.LBB19_347:                             # %cond.true1834
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	andq	$16384, %rsi            # imm = 0x4000
	movq	$14, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB19_349
# BB#348:                               # %cond.true1834
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$17, %rsi
.LBB19_349:                             # %cond.true1834
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rsi, %rdi
	jmp	.LBB19_383
.LBB19_364:                             # %cond.false1811
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$2, %rdi
	cmpq	$100, %rdx
	je	.LBB19_383
	jmp	.LBB19_365
.LBB19_379:                             # %cond.false1811
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$120, %rdx
	jne	.LBB19_365
.LBB19_380:                             # %cond.true1818
                                        #   in Loop: Header=BB19_140 Depth=4
	testq	$327680, %rsi           # imm = 0x50000
	movabsq	$0, %rdi
	je	.LBB19_382
# BB#381:                               # %cond.true1818
                                        #   in Loop: Header=BB19_140 Depth=4
	movabsq	$1, %rdi
.LBB19_382:                             # %cond.true1818
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$4, %rcx
	shlq	%cl, %rdi
	jmp	.LBB19_383
.LBB19_365:                             # %cond.false1839
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$7, %rdi
	cmpq	$65, %rdx
	je	.LBB19_383
# BB#366:                               # %cond.false1843
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$6, %rdi
	cmpq	$68, %rdx
	je	.LBB19_383
.LBB19_367:                             # %cond.false1847
                                        #   in Loop: Header=BB19_140 Depth=4
	cmpq	$83, %rdx
	movabsq	$0, %rax
	jne	.LBB19_369
# BB#368:                               # %cond.false1847
                                        #   in Loop: Header=BB19_140 Depth=4
	movabsq	$1, %rax
.LBB19_369:                             # %cond.false1847
                                        #   in Loop: Header=BB19_140 Depth=4
	leaq	(%rax,%rax,4), %rdi
	.align	16, 0x90
.LBB19_383:                             # %cond.end1881
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	imulq	$100, %r15, %rax
	leaq	reg_class_subunion(%rax,%rdi,4), %rax
	movq	%rbx, %r11
.LBB19_384:                             # %reg1891
                                        #   in Loop: Header=BB19_140 Depth=4
	movl	(%rax), %r15d
	movq	344(%rsp), %rdx         # 8-byte Reload
	movl	%r15d, 2832(%rsp,%rdx,4)
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$16711680, %rcx         # imm = 0xFF0000
	cmpq	$3342336, %rcx          # imm = 0x330000
	je	.LBB19_388
# BB#385:                               # %if.end1898
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%r11, %rbx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB19_386
# BB#391:                               # %land.lhs.true1903
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	recog_data(,%rdx,8), %rax
	movzbl	2(%rax), %ecx
	movq	%r10, %rdi
	movq	%r10, %r14
	movq	%r15, %rsi
	movq	272(%rsp), %rdx         # 8-byte Reload
	callq	reg_fits_class_p
	movq	%rbx, %r11
	testq	%r11, %rax
	je	.LBB19_393
# BB#392:                               # %land.lhs.true1903
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, %r12
.LBB19_393:                             # %land.lhs.true1903
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 328(%rsp)           # 8-byte Folded Spill
	movq	%r14, %r10
	jmp	.LBB19_388
.LBB19_386:                             #   in Loop: Header=BB19_140 Depth=4
	movq	$1, 328(%rsp)           # 8-byte Folded Spill
.LBB19_387:                             # %while.cond793.backedge
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	%rbx, %r11
	.align	16, 0x90
.LBB19_388:                             # %while.cond793.backedge
                                        #   in Loop: Header=BB19_140 Depth=4
	movq	640(%rsp), %rbx
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB19_140
	jmp	.LBB19_389
	.align	16, 0x90
.LBB19_141:                             #   in Loop: Header=BB19_111 Depth=3
	movq	%rcx, %rbx
.LBB19_389:                             # %while.end1915
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %r9          # 8-byte Reload
	movq	%rbx, 3632(%rsp,%r9,8)
	movq	328(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_394
# BB#390:                               #   in Loop: Header=BB19_111 Depth=3
	movq	304(%rsp), %rcx         # 8-byte Reload
	movq	336(%rsp), %rdx         # 8-byte Reload
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	224(%rsp), %rsi         # 8-byte Reload
	movq	320(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB19_397
	.align	16, 0x90
.LBB19_394:                             # %land.lhs.true1919
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %r15
	movq	336(%rsp), %rdx         # 8-byte Reload
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	224(%rsp), %rsi         # 8-byte Reload
	movq	320(%rsp), %rdi         # 8-byte Reload
	je	.LBB19_396
# BB#395:                               # %land.lhs.true1919
                                        #   in Loop: Header=BB19_111 Depth=3
	xorq	%rdi, %rdi
.LBB19_396:                             # %land.lhs.true1919
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	304(%rsp), %rcx         # 8-byte Reload
.LBB19_397:                             # %if.end1925
                                        #   in Loop: Header=BB19_111 Depth=3
	movb	%al, 2704(%rsp,%r9)
	testq	%r11, %r12
	je	.LBB19_400
# BB#398:                               # %if.end1925
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %rdx
	jne	.LBB19_400
# BB#399:                               # %if.then1932
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, %rax
	movb	%al, 2768(%rsp,%r9)
	movq	296(%rsp), %r14         # 8-byte Reload
	jmp	.LBB19_452
	.align	16, 0x90
.LBB19_400:                             # %if.else1935
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %rcx
	je	.LBB19_403
# BB#401:                               # %if.else1935
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %rdx
	jne	.LBB19_403
# BB#402:                               # %if.then1939
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, %rax
	movb	%al, 2800(%rsp,%r9)
	movq	296(%rsp), %r14         # 8-byte Reload
	jmp	.LBB19_452
	.align	16, 0x90
.LBB19_403:                             # %if.else1942
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%rcx, 304(%rsp)         # 8-byte Spill
	movq	%r12, 280(%rsp)         # 8-byte Spill
	movb	%sil, 2736(%rsp,%r9)
	testq	%r11, %rdi
	je	.LBB19_405
# BB#404:                               # %if.else1942
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, 248(%rsp)           # 8-byte Folded Spill
.LBB19_405:                             # %if.else1942
                                        #   in Loop: Header=BB19_111 Depth=3
	movzbl	(%r10), %eax
	movq	288(%rsp), %rcx         # 8-byte Reload
	leaq	1(%rcx), %r14
	cmpq	$61, %rax
	jne	.LBB19_406
# BB#407:                               # %land.lhs.true1954
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r13, 320(%rsp)         # 8-byte Spill
	testq	%r11, %r15
	jne	.LBB19_408
# BB#409:                               # %land.lhs.true1959
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$32, %rcx
	movq	312(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	$1, %rcx
	js	.LBB19_411
	jmp	.LBB19_410
.LBB19_406:                             #   in Loop: Header=BB19_111 Depth=3
	movq	%r13, 320(%rsp)         # 8-byte Spill
.LBB19_410:                             # %land.lhs.true1959
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	248(%rsp), %rcx         # 8-byte Reload
.LBB19_411:                             # %if.end1965
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%rcx, 344(%rsp)         # 8-byte Spill
	cmpq	$139, %rax
	jg	.LBB19_416
# BB#412:                               # %if.end1965
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$0, 336(%rsp)           # 8-byte Folded Spill
	movq	%r9, %r12
	addq	$-54, %rax
	andq	%r11, %rax
	cmpq	$14, %rax
	jbe	.LBB19_414
# BB#413:                               #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, 288(%rsp)         # 8-byte Spill
	movq	%r12, %r9
	jmp	.LBB19_425
.LBB19_416:                             # %if.end1965
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$0, 336(%rsp)           # 8-byte Folded Spill
	movq	%r9, %r12
	movq	%r11, %rbx
	cmpq	$140, %rax
	jne	.LBB19_415
	jmp	.LBB19_417
.LBB19_408:                             #   in Loop: Header=BB19_111 Depth=3
	movq	$0, 336(%rsp)           # 8-byte Folded Spill
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	%rax, 344(%rsp)         # 8-byte Spill
	jmp	.LBB19_424
.LBB19_414:                             # %if.end1965
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r11, %rbx
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	je	.LBB19_415
.LBB19_417:                             # %land.lhs.true2010
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r10, %rdi
	movq	%r10, %r13
	movq	%r15, %rsi
	callq	ix86_preferred_reload_class
	movq	%rbx, %r11
	testq	%r11, %rax
	jne	.LBB19_418
# BB#419:                               # %land.lhs.true2018
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r12, %r9
	movl	656(%rsp,%r9,4), %eax
	testq	%rax, %rax
	jne	.LBB19_421
# BB#420:                               #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, 288(%rsp)         # 8-byte Spill
	movq	%r13, %r10
	jmp	.LBB19_425
.LBB19_415:                             #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, 288(%rsp)         # 8-byte Spill
	movq	%rbx, %r11
	movq	%r12, %r9
	jmp	.LBB19_425
.LBB19_418:                             #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, 288(%rsp)         # 8-byte Spill
	movq	%r12, %r9
	movq	%r13, %r10
	jmp	.LBB19_425
.LBB19_421:                             # %if.then2023
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %r15
	movq	%r13, %r10
	je	.LBB19_423
# BB#422:                               # %if.then2023
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	288(%rsp), %rax         # 8-byte Reload
	addq	$2, %rax
	movq	%rax, %r14
.LBB19_423:                             # %if.then2023
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, 336(%rsp)           # 8-byte Folded Spill
.LBB19_424:                             # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, 288(%rsp)         # 8-byte Spill
.LBB19_425:                             # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	movl	(%r10), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB19_428
# BB#426:                               # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	addq	$-54, %rcx
	andq	%r11, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_430
# BB#427:                               # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB19_433
	jmp	.LBB19_430
.LBB19_428:                             # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$134, %rcx
	je	.LBB19_433
# BB#429:                               # %if.end2031
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$140, %rcx
	je	.LBB19_433
.LBB19_430:                             # %land.lhs.true2071
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %r15
	je	.LBB19_433
# BB#431:                               # %land.lhs.true2076
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r11, %r14
	movq	%r10, %rdi
	movq	%r15, %rsi
	movq	%r9, %rbx
	movq	%r10, %r12
	callq	ix86_preferred_reload_class
	movq	%r12, %r10
	movq	%rbx, %r9
	movq	%r14, %r11
	testq	%r11, %rax
	movq	$1, 248(%rsp)           # 8-byte Folded Spill
	movq	280(%rsp), %r12         # 8-byte Reload
	je	.LBB19_438
# BB#432:                               # %land.lhs.true2076.if.else2083_crit_edge
                                        #   in Loop: Header=BB19_111 Depth=3
	movl	(%r10), %eax
	.align	16, 0x90
.LBB19_433:                             # %if.else2083
                                        #   in Loop: Header=BB19_111 Depth=3
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$62, %rax
	movq	280(%rsp), %r12         # 8-byte Reload
	je	.LBB19_437
# BB#434:                               # %land.lhs.true2088
                                        #   in Loop: Header=BB19_111 Depth=3
	movl	2960(%rsp,%r9,4), %eax
	testq	%rax, %rax
	je	.LBB19_437
# BB#435:                               # %land.lhs.true2088
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$1, 152(%rsp)           # 8-byte Folded Reload
	ja	.LBB19_437
# BB#436:                               # %land.lhs.true2095
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	$10, %rsi
	movq	%r11, %r14
	movq	208(%rsp), %rdi         # 8-byte Reload
	movq	%r10, %rdx
	movq	%r9, %r13
	movq	%r10, %rbx
	callq	find_reg_note
	movq	%rbx, %r10
	movq	%r13, %r9
	movq	%r14, %r11
	testq	%rax, %rax
	movq	$1, 248(%rsp)           # 8-byte Folded Spill
	je	.LBB19_438
.LBB19_437:                             # %if.else2099
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %rax         # 8-byte Reload
	movq	%rax, 248(%rsp)         # 8-byte Spill
.LBB19_438:                             # %if.end2111
                                        #   in Loop: Header=BB19_111 Depth=3
	movzbl	(%r10), %eax
	cmpq	$62, %rax
	jne	.LBB19_440
# BB#439:                               #   in Loop: Header=BB19_111 Depth=3
	movq	296(%rsp), %r14         # 8-byte Reload
	movq	320(%rsp), %r13         # 8-byte Reload
	jmp	.LBB19_447
.LBB19_440:                             # %if.end2111
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$61, %rax
	movq	320(%rsp), %r13         # 8-byte Reload
	jne	.LBB19_442
# BB#441:                               # %land.lhs.true2116
                                        #   in Loop: Header=BB19_111 Depth=3
	movl	8(%r10), %ecx
	cmpq	$52, %rcx
	ja	.LBB19_446
.LBB19_442:                             # %land.lhs.true2127
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r12, %r14
	movq	240(%rsp), %rcx         # 8-byte Reload
	testq	%r11, %rcx
	movabsq	$0, %rcx
	je	.LBB19_444
# BB#443:                               # %land.lhs.true2127
                                        #   in Loop: Header=BB19_111 Depth=3
	movabsq	$1, %rcx
.LBB19_444:                             # %land.lhs.true2127
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	336(%rsp), %rdx         # 8-byte Reload
	testq	%rcx, %rdx
	movq	%r14, %r12
	jne	.LBB19_446
# BB#445:                               # %land.lhs.true2127
                                        #   in Loop: Header=BB19_111 Depth=3
	addq	$2, 200(%rsp)           # 8-byte Folded Spill
.LBB19_446:                             # %land.lhs.true2127
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	296(%rsp), %r14         # 8-byte Reload
.LBB19_447:                             # %if.end2133
                                        #   in Loop: Header=BB19_111 Depth=3
	movl	3216(%rsp,%r9,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB19_449
# BB#448:                               #   in Loop: Header=BB19_111 Depth=3
	movq	304(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB19_452
.LBB19_449:                             # %land.lhs.true2138
                                        #   in Loop: Header=BB19_111 Depth=3
	cmpq	$62, %rax
	movabsq	$0, %rax
	movq	304(%rsp), %rcx         # 8-byte Reload
	je	.LBB19_451
# BB#450:                               # %land.lhs.true2138
                                        #   in Loop: Header=BB19_111 Depth=3
	movabsq	$1, %rax
.LBB19_451:                             # %land.lhs.true2138
                                        #   in Loop: Header=BB19_111 Depth=3
	addq	%rax, 200(%rsp)         # 8-byte Folded Spill
	.align	16, 0x90
.LBB19_452:                             # %if.end2147
                                        #   in Loop: Header=BB19_111 Depth=3
	orq	%r12, %rcx
	testq	%r11, %rcx
	jne	.LBB19_463
# BB#453:                               # %if.end2147
                                        #   in Loop: Header=BB19_111 Depth=3
	testq	%r11, %r15
	je	.LBB19_463
# BB#454:                               # %land.lhs.true2156
                                        #   in Loop: Header=BB19_111 Depth=3
	movslq	656(%rsp,%r9,4), %rax
	movl	target_flags(%rip), %edx
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rdx
	cmpq	%rdx, %rax
	ja	.LBB19_463
# BB#455:                               # %land.lhs.true2167
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, %r12
	movq	%r11, %r14
	movslq	3504(%rsp,%r9,4), %rbx
	movl	reg_class_size(,%rbx,4), %eax
	cmpq	$2, %rax
	jb	.LBB19_456
# BB#457:                               # %if.then2174
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r9, 344(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	reg_class_subset_p
	movq	%r14, %r11
	testq	%r11, %rax
	jne	.LBB19_458
# BB#459:                               # %if.then2181
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r11, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	reg_class_subset_p
	movq	%r14, %r11
	testq	%r11, %rax
	jne	.LBB19_460
# BB#461:                               # %if.else2193
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %r9          # 8-byte Reload
	movsbq	3472(%rsp,%r9), %rax
	movq	200(%rsp), %rcx         # 8-byte Reload
	leaq	2(%rcx,%rax,2), %rcx
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	jmp	.LBB19_462
.LBB19_456:                             #   in Loop: Header=BB19_111 Depth=3
	movq	%r14, %r11
	jmp	.LBB19_462
.LBB19_458:                             #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %r9          # 8-byte Reload
	jmp	.LBB19_462
.LBB19_460:                             # %if.then2188
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	344(%rsp), %r9          # 8-byte Reload
	movl	%ebx, 2832(%rsp,%r9,4)
	.align	16, 0x90
.LBB19_462:                             # %for.inc2203
                                        #   in Loop: Header=BB19_111 Depth=3
	movq	%r12, %r14
.LBB19_463:                             # %for.inc2203
                                        #   in Loop: Header=BB19_111 Depth=3
	incq	%r9
	incq	256(%rsp)               # 8-byte Folded Spill
	movq	%r9, %rax
	andq	%r11, %rax
	movq	%r14, %r15
	andq	%r11, %r15
	cmpq	%r15, %rax
	jne	.LBB19_111
# BB#464:                               # %for.cond2206.preheader
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	jle	.LBB19_494
	.align	16, 0x90
.LBB19_465:                             # %for.body2209
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB19_473 Depth 4
                                        #         Child Loop BB19_489 Depth 4
	movzbl	2704(%rsp,%r14), %eax
	testq	%rax, %rax
	je	.LBB19_493
# BB#466:                               # %land.lhs.true2214
                                        #   in Loop: Header=BB19_465 Depth=3
	movzbl	2768(%rsp,%r14), %eax
	testq	%rax, %rax
	jne	.LBB19_468
# BB#467:                               # %lor.lhs.false2219
                                        #   in Loop: Header=BB19_465 Depth=3
	movzbl	2800(%rsp,%r14), %eax
	testq	%rax, %rax
	je	.LBB19_493
.LBB19_468:                             # %if.then2224
                                        #   in Loop: Header=BB19_465 Depth=3
	movq	%r11, %r12
	movq	%r13, 320(%rsp)         # 8-byte Spill
	movq	296(%rsp), %r13         # 8-byte Reload
	movq	recog_data(,%r14,8), %rsi
	leaq	608(%rsp), %rdi
	callq	decompose
	movl	2960(%rsp,%r14,4), %eax
	testq	%rax, %rax
	je	.LBB19_469
# BB#470:                               # %if.end2232
                                        #   in Loop: Header=BB19_465 Depth=3
	movslq	2832(%rsp,%r14,4), %rax
	movq	%rax, 344(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%r12, %r11
	jne	.LBB19_473
# BB#471:                               # %if.then2237
                                        #   in Loop: Header=BB19_465 Depth=3
	movq	%r11, %r12
	movl	this_insn_is_asm(%rip), %eax
	xorq	%rcx, %rcx
	movb	%cl, 2704(%rsp,%r14)
	testq	%rax, %rax
	je	.LBB19_960
# BB#472:                               # %if.then2241
                                        #   in Loop: Header=BB19_465 Depth=3
	movq	this_insn(%rip), %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movabsq	$.L.str3, %rsi
	callq	error_for_asm
	movq	%r12, %r11
	.align	16, 0x90
.LBB19_473:                             # %for.body2248
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_465 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	recog_data(,%rbx,8), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB19_474
# BB#476:                               # %land.lhs.true2260
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	%rbx, %rcx
	andq	%r11, %rcx
	movq	%r14, %rdx
	andq	%r11, %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_485
	jmp	.LBB19_477
	.align	16, 0x90
.LBB19_474:                             # %lor.lhs.false2255
                                        #   in Loop: Header=BB19_473 Depth=4
	movl	2960(%rsp,%rbx,4), %ecx
	cmpq	$2, %rcx
	je	.LBB19_485
# BB#475:                               # %lor.lhs.false2255
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	%r14, %rcx
	andq	%r11, %rcx
	movq	%rbx, %rdx
	andq	%r11, %rdx
	cmpq	%rcx, %rdx
	je	.LBB19_485
.LBB19_477:                             # %land.lhs.true2263
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	recog_data+480(,%rbx,8), %rcx
	movzbl	(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB19_485
# BB#478:                               # %land.lhs.true2269
                                        #   in Loop: Header=BB19_473 Depth=4
	movl	2576(%rsp,%rbx,4), %ecx
	movq	%r14, %rdx
	andq	%r11, %rdx
	cmpq	%rdx, %rcx
	movq	%r11, %r12
	jne	.LBB19_481
# BB#479:                               # %land.lhs.true2274
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	recog_data(,%r14,8), %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	movq	%r12, %r11
	jne	.LBB19_485
# BB#480:                               # %land.lhs.true2274.land.lhs.true2281_crit_edge
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	%r11, %r12
	movq	recog_data(,%rbx,8), %rax
.LBB19_481:                             # %land.lhs.true2281
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	recog_data(,%r14,8), %rsi
	movq	632(%rsp), %rcx
	movq	%rcx, 24(%rsp)
	movq	624(%rsp), %rcx
	movq	%rcx, 16(%rsp)
	movq	608(%rsp), %rcx
	movq	616(%rsp), %rdx
	movq	%rdx, 8(%rsp)
	movq	%rcx, (%rsp)
	movq	%rax, %rdi
	callq	immune_p
	movq	%r12, %r11
	testq	%r11, %rax
	jne	.LBB19_485
# BB#482:                               # %if.then2288
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	reg_class_size(,%rax,4), %eax
	cmpq	$1, %rax
	jne	.LBB19_486
# BB#483:                               # %land.lhs.true2295
                                        #   in Loop: Header=BB19_473 Depth=4
	movq	recog_data(,%rbx,8), %rax
	movl	(%rax), %eax
	orq	$2, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB19_486
# BB#484:                               # %if.then2309
                                        #   in Loop: Header=BB19_473 Depth=4
	incq	288(%rsp)               # 8-byte Folded Spill
	xorq	%rax, %rax
	movb	%al, 2768(%rsp,%rbx)
	movb	%al, 2800(%rsp,%rbx)
	.align	16, 0x90
.LBB19_485:                             # %for.inc2318
                                        #   in Loop: Header=BB19_473 Depth=4
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r13, %rax
	jl	.LBB19_473
.LBB19_486:                             # %for.end2320
                                        #   in Loop: Header=BB19_465 Depth=3
	movq	%rbx, %rax
	andq	%r11, %rax
	cmpq	%r15, %rax
	jne	.LBB19_488
# BB#487:                               #   in Loop: Header=BB19_465 Depth=3
	movq	320(%rsp), %r13         # 8-byte Reload
	jmp	.LBB19_493
.LBB19_488:                             # %for.body2332.lr.ph
                                        #   in Loop: Header=BB19_465 Depth=3
	xorq	%rax, %rax
	movb	%al, 2768(%rsp,%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movb	%al, 2800(%rsp,%rbx)
	incq	288(%rsp)               # 8-byte Folded Spill
	movq	$1, %rax
	movq	320(%rsp), %r13         # 8-byte Reload
	.align	16, 0x90
.LBB19_489:                             # %for.body2332
                                        #   Parent Loop BB19_107 Depth=1
                                        #     Parent Loop BB19_109 Depth=2
                                        #       Parent Loop BB19_465 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movl	2572(%rsp,%rax,4), %ecx
	movq	%r14, %rdx
	andq	%r11, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB19_492
# BB#490:                               # %land.lhs.true2337
                                        #   in Loop: Header=BB19_489 Depth=4
	movzbl	2799(%rsp,%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_492
# BB#491:                               # %if.then2342
                                        #   in Loop: Header=BB19_489 Depth=4
	xorq	%rcx, %rcx
	movb	%cl, 2767(%rsp,%rax)
	movb	%cl, 2799(%rsp,%rax)
	incq	288(%rsp)               # 8-byte Folded Spill
.LBB19_492:                             # %for.inc2349
                                        #   in Loop: Header=BB19_489 Depth=4
	movq	%rax, %rcx
	andq	%r11, %rcx
	incq	%rax
	cmpq	%r15, %rcx
	jne	.LBB19_489
	.align	16, 0x90
.LBB19_493:                             # %for.inc2354
                                        #   in Loop: Header=BB19_465 Depth=3
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	296(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB19_465
.LBB19_494:                             # %for.end2356
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	288(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	je	.LBB19_495
# BB#501:                               # %if.end2405
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	248(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_502
# BB#503:                               # %if.end2405
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	288(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%rax,2), %rax
	movq	200(%rsp), %rcx         # 8-byte Reload
	leaq	(%rcx,%rax,2), %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	88(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	120(%rsp), %rbx         # 8-byte Reload
	jle	.LBB19_507
# BB#504:                               # %for.cond2413.preheader
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_506
# BB#505:                               # %for.body2416.lr.ph
                                        #   in Loop: Header=BB19_109 Depth=2
	leaq	2448(%rsp), %rdi
	leaq	2832(%rsp), %rsi
	movq	56(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	movq	%rbx, %r14
	callq	memcpy
	leaq	2000(%rsp), %rdi
	leaq	2768(%rsp), %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	callq	memcpy
	leaq	2032(%rsp), %rdi
	leaq	2800(%rsp), %rsi
	movq	%rbx, %rdx
	callq	memcpy
	leaq	1968(%rsp), %rdi
	leaq	2736(%rsp), %rsi
	movq	%rbx, %rdx
	callq	memcpy
	leaq	2192(%rsp), %rdi
	leaq	2576(%rsp), %rsi
	movq	%r15, %rdx
	callq	memcpy
	leaq	1936(%rsp), %rdi
	leaq	2704(%rsp), %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	%r14, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%rdi, %r11
.LBB19_506:                             # %for.inc2445
                                        #   in Loop: Header=BB19_109 Depth=2
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	jmp	.LBB19_507
.LBB19_502:                             #   in Loop: Header=BB19_109 Depth=2
	movq	120(%rsp), %rbx         # 8-byte Reload
.LBB19_507:                             # %for.inc2445
                                        #   in Loop: Header=BB19_109 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	296(%rsp), %r14         # 8-byte Reload
	jl	.LBB19_109
	.align	16, 0x90
.LBB19_508:                             # %for.end2447
                                        #   in Loop: Header=BB19_107 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_522
# BB#509:                               # %if.then2450
                                        #   in Loop: Header=BB19_107 Depth=1
	movq	216(%rsp), %rax         # 8-byte Reload
	testq	%r11, %rax
	jne	.LBB19_516
# BB#510:                               # %if.then2453
                                        #   in Loop: Header=BB19_107 Depth=1
	movq	%r14, %r15
	movq	%r11, %r14
	leaq	1(%r13), %rsi
	movq	$32, %rcx
	movq	%rsi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	784(%rsp,%r8,8), %rax
	movq	$32, %rcx
	movq	%r13, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	784(%rsp,%r9,8), %rcx
	movq	%rax, recog_data(,%r9,8)
	movq	%rcx, recog_data(,%r8,8)
	movsbq	recog_data+997(%rip), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	jle	.LBB19_515
	.align	16, 0x90
.LBB19_511:                             # %for.body2470
                                        #   Parent Loop BB19_107 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movsbq	recog_data+992(%rbx), %rax
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%r13, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_513
# BB#512:                               # %for.body2470
                                        #   in Loop: Header=BB19_511 Depth=2
	movq	%rsi, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB19_514
.LBB19_513:                             # %if.then2483
                                        #   in Loop: Header=BB19_511 Depth=2
	movq	recog_data(,%rax,8), %rax
	movq	recog_data+960(,%rbx,8), %rcx
	movq	%rax, (%rcx)
.LBB19_514:                             # %for.inc2492
                                        #   in Loop: Header=BB19_511 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jl	.LBB19_511
	jmp	.LBB19_515
.LBB19_110:
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB19_496
.LBB19_516:                             # %if.else2519
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	784(%rsp,%rax,8), %rdx
	leaq	1(%r13), %r8
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	784(%rsp,%rsi,8), %rcx
	movq	%rdx, recog_data(,%rax,8)
	movq	%rcx, recog_data(,%rsi,8)
	movsbq	recog_data+997(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB19_522
# BB#517:
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r13, %rbx
	andq	%r9, %rbx
.LBB19_518:                             # %for.body2534
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%rsi), %rax
	movq	%rax, %rcx
	andq	%r9, %rcx
	cmpq	%rbx, %rcx
	je	.LBB19_520
# BB#519:                               # %for.body2534
                                        #   in Loop: Header=BB19_518 Depth=1
	movq	%r8, %rdi
	andq	%r9, %rdi
	cmpq	%rdi, %rcx
	jne	.LBB19_521
.LBB19_520:                             # %if.then2547
                                        #   in Loop: Header=BB19_518 Depth=1
	movq	recog_data(,%rax,8), %rax
	movq	recog_data+960(,%rsi,8), %rcx
	movq	%rax, (%rcx)
.LBB19_521:                             # %for.inc2556
                                        #   in Loop: Header=BB19_518 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB19_518
.LBB19_522:                             # %if.end2560
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	88(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$660, %rcx              # imm = 0x294
	jne	.LBB19_525
# BB#523:                               # %if.then2563
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_524
# BB#961:                               # %if.then2566
	movabsq	$.L.str4, %rax
	movabsq	$.L.str, %rdx
	movabsq	$.L__FUNCTION__.find_reloads, %rbx
	movq	$3578, %rcx             # imm = 0xDFA
	movq	%rax, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %r8
	callq	_fatal_insn
.LBB19_525:                             # %finish
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	72(%rsp), %rax          # 8-byte Reload
	jg	.LBB19_526
	jmp	.LBB19_540
.LBB19_495:
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	296(%rsp), %r14         # 8-byte Reload
.LBB19_496:                             # %if.then2359
	movq	%r14, %r15
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_498
# BB#497:                               # %if.then2362
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	784(%rsp,%rax,8), %rdx
	leaq	1(%r13), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	784(%rsp,%rsi,8), %rcx
	movq	%rdx, recog_data(,%rax,8)
	movq	%rcx, recog_data(,%rsi,8)
.LBB19_498:                             # %for.cond2374.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_499
# BB#500:                               # %finish.thread
	leaq	2000(%rsp), %rdi
	leaq	2768(%rsp), %rsi
	movq	48(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	callq	memcpy
	leaq	2032(%rsp), %rdi
	leaq	2800(%rsp), %rsi
	movq	%r14, %rdx
	callq	memcpy
	leaq	2448(%rsp), %rdi
	leaq	2832(%rsp), %rsi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	callq	memcpy
	leaq	1968(%rsp), %rdi
	leaq	2736(%rsp), %rsi
	movq	%r14, %rdx
	callq	memcpy
	leaq	2192(%rsp), %rdi
	leaq	2576(%rsp), %rsi
	movq	%rbx, %rdx
	callq	memcpy
	leaq	1936(%rsp), %rdi
	leaq	2704(%rsp), %rsi
	movq	%r14, %rdx
	callq	memcpy
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%r15, %r14
.LBB19_526:                             # %for.body2585.lr.ph
	movq	%rax, 72(%rsp)          # 8-byte Spill
	leaq	2064(%rsp), %rdi
	movq	$255, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	memset
	xorq	%rcx, %rcx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	%r14, %r11
	andq	%r10, %r11
	.align	16, 0x90
.LBB19_527:                             # %for.body2585
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2000(%rsp,%rcx), %eax
	testq	%rax, %rax
	jne	.LBB19_530
# BB#528:                               # %land.lhs.true2589
                                        #   in Loop: Header=BB19_527 Depth=1
	movslq	2192(%rsp,%rcx,4), %rsi
	testq	%rsi, %rsi
	js	.LBB19_530
# BB#529:                               # %if.then2594
                                        #   in Loop: Header=BB19_527 Depth=1
	movl	%ecx, 2064(%rsp,%rsi,4)
.LBB19_530:                             # %for.inc2600
                                        #   in Loop: Header=BB19_527 Depth=1
	incq	%rcx
	movq	%rcx, %rax
	andq	%r10, %rax
	cmpq	%r11, %rax
	jne	.LBB19_527
# BB#531:                               # %for.cond2603.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB19_539
# BB#532:                               # %for.body2606.preheader
	movq	$56, %rcx
	movq	%rax, %r9
	shlq	%cl, %r9
	movq	$56, %rcx
	sarq	%cl, %r9
	decq	%r9
	andq	%r10, %r9
	incq	%r9
	xorq	%rcx, %rcx
	movabsq	$8589934588, %rdx       # imm = 0x1FFFFFFFC
	andq	%r9, %rdx
	je	.LBB19_536
# BB#533:                               # %vector.body.preheader
	movq	%rdx, 344(%rsp)         # 8-byte Spill
	movq	%r13, 320(%rsp)         # 8-byte Spill
	movq	%r14, 296(%rsp)         # 8-byte Spill
	movq	$56, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	andq	%r10, %rbx
	incq	%rbx
	andq	$-4, %rbx
	leaq	2000(%rsp), %rcx
	leaq	2032(%rsp), %rax
	.align	16, 0x90
.LBB19_534:                             # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rax), %r14d
	movzbl	2(%rax), %r15d
	movzbl	1(%rax), %r12d
	movzbl	(%rax), %r8d
	movzbl	3(%rcx), %edx
	movzbl	2(%rcx), %r13d
	movzbl	1(%rcx), %edi
	movzbl	(%rcx), %esi
	orq	%r8, %rsi
	orq	%r12, %rdi
	orq	%r15, %r13
	orq	%r14, %rdx
	movb	%dl, 3(%rcx)
	movb	%r13b, 2(%rcx)
	movb	%dil, 1(%rcx)
	movb	%sil, (%rcx)
	addq	$4, %rcx
	addq	$4, %rax
	addq	$-4, %rbx
	jne	.LBB19_534
# BB#535:
	movq	344(%rsp), %rcx         # 8-byte Reload
	movq	296(%rsp), %r14         # 8-byte Reload
	movq	320(%rsp), %r13         # 8-byte Reload
.LBB19_536:                             # %middle.block
	cmpq	%rcx, %r9
	je	.LBB19_539
# BB#537:                               # %for.body2606.preheader9
	incq	%rcx
.LBB19_538:                             # %for.body2606
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2031(%rsp,%rcx), %eax
	movzbl	1999(%rsp,%rcx), %edx
	orq	%rax, %rdx
	movb	%dl, 1999(%rsp,%rcx)
	movq	%rcx, %rax
	andq	%r10, %rax
	incq	%rcx
	cmpq	%r11, %rax
	jne	.LBB19_538
.LBB19_539:
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB19_540
.LBB19_499:
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	%r15, %r14
.LBB19_540:                             # %for.end2617
	movq	%r14, 296(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	testq	%r9, %rax
	je	.LBB19_545
# BB#541:                               # %if.then2619
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	1(%r13), %r8
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	recog_data(,%rdx,8), %rcx
	movq	recog_data(,%rsi,8), %rdi
	movq	784(%rsp,%rdx,8), %rbx
	movq	%rdi, recog_data(,%rdx,8)
	movq	784(%rsp,%rsi,8), %rdi
	movq	%rcx, recog_data(,%rsi,8)
	movq	recog_data+240(,%rsi,8), %rcx
	movq	%rdi, 784(%rsp,%rdx,8)
	movq	%rbx, 784(%rsp,%rsi,8)
	movq	recog_data+240(,%rdx,8), %rdx
	movq	(%rcx), %rcx
	movq	(%rdx), %rdi
	movq	%rcx, (%rdx)
	movq	recog_data+240(,%rsi,8), %rcx
	movq	%rdi, (%rcx)
	movslq	n_reloads(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB19_545
# BB#542:
	movq	$1, %rsi
	movabsq	$rld+72, %rdi
	movq	%r13, %rbx
	andq	%r9, %rbx
	.align	16, 0x90
.LBB19_543:                             # %for.body2654
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdi), %ecx
	cmpq	%rbx, %rcx
	jne	.LBB19_559
# BB#544:                               # %if.then2659
                                        #   in Loop: Header=BB19_543 Depth=1
	movl	%r8d, (%rdi)
	jmp	.LBB19_561
.LBB19_559:                             # %if.else2664
                                        #   in Loop: Header=BB19_543 Depth=1
	movq	%r8, %rax
	andq	%r9, %rax
	cmpq	%rax, %rcx
	jne	.LBB19_561
# BB#560:                               # %if.then2671
                                        #   in Loop: Header=BB19_543 Depth=1
	movl	%r13d, (%rdi)
.LBB19_561:                             # %for.inc2677
                                        #   in Loop: Header=BB19_543 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	addq	$104, %rdi
	cmpq	%rdx, %rax
	jl	.LBB19_543
.LBB19_545:                             # %for.cond2681.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_573
# BB#546:                               # %for.body2684.lr.ph
	leaq	2320(%rsp), %rdi
	movq	$255, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	memset
	movq	$1, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	296(%rsp), %r12         # 8-byte Reload
	andq	%r14, %r12
	.align	16, 0x90
.LBB19_547:                             # %for.body2684
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1935(%rsp,%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_552
# BB#548:                               # %land.lhs.true2691
                                        #   in Loop: Header=BB19_547 Depth=1
	movl	3212(%rsp,%rbx,4), %eax
	cmpq	$9, %rax
	je	.LBB19_552
# BB#549:                               # %if.then2696
                                        #   in Loop: Header=BB19_547 Depth=1
	movq	recog_data-8(,%rbx,8), %rdx
	movq	$10, %rsi
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	find_reg_note
	testq	%rax, %rax
	movq	$2, %rax
	jne	.LBB19_551
# BB#550:                               # %if.then2696
                                        #   in Loop: Header=BB19_547 Depth=1
	movq	$9, %rax
.LBB19_551:                             # %if.then2696
                                        #   in Loop: Header=BB19_547 Depth=1
	movl	%eax, 3212(%rsp,%rbx,4)
.LBB19_552:                             # %for.inc2705
                                        #   in Loop: Header=BB19_547 Depth=1
	movq	%rbx, %rax
	andq	%r14, %rax
	incq	%rbx
	cmpq	%r12, %rax
	jne	.LBB19_547
# BB#553:                               # %for.cond2708.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_573
# BB#554:                               # %for.body2711.lr.ph
	xorq	%rbx, %rbx
	movq	64(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB19_555:                             # %for.body2711
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_566 Depth 2
                                        #     Child Loop BB19_568 Depth 2
	movzbl	2000(%rsp,%rbx), %eax
	testq	%rax, %rax
	jne	.LBB19_572
# BB#556:                               # %land.lhs.true2715
                                        #   in Loop: Header=BB19_555 Depth=1
	movq	recog_data(,%rbx,8), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$139, %rcx
	jg	.LBB19_562
# BB#557:                               # %land.lhs.true2715
                                        #   in Loop: Header=BB19_555 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB19_572
# BB#558:                               # %land.lhs.true2715
                                        #   in Loop: Header=BB19_555 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB19_563
	jmp	.LBB19_572
.LBB19_562:                             # %land.lhs.true2715
                                        #   in Loop: Header=BB19_555 Depth=1
	cmpq	$140, %rcx
	jne	.LBB19_572
.LBB19_563:                             # %land.lhs.true2778
                                        #   in Loop: Header=BB19_555 Depth=1
	movl	2448(%rsp,%rbx,4), %esi
	callq	ix86_preferred_reload_class
	testq	%r14, %rax
	jne	.LBB19_572
# BB#564:                               # %land.lhs.true2788
                                        #   in Loop: Header=BB19_555 Depth=1
	movl	656(%rsp,%rbx,4), %edi
	testq	%rdi, %rdi
	je	.LBB19_572
# BB#565:                               # %if.then2793
                                        #   in Loop: Header=BB19_555 Depth=1
	movq	$32, %rcx
	movq	%r13, %r15
	movq	recog_data(,%rbx,8), %rsi
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	callq	force_const_mem
	movl	3088(%rsp,%rbx,4), %edx
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	callq	find_reloads_toplev
	movq	%rax, recog_data(,%rbx,8)
	movq	recog_data+480(,%rbx,8), %rdx
	testq	%r15, %r15
	movq	%rax, 784(%rsp,%rbx,8)
	movq	%r13, %rax
	jle	.LBB19_568
	.align	16, 0x90
.LBB19_566:                             # %while.cond1.i
                                        #   Parent Loop BB19_555 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %ecx
	incq	%rdx
	cmpq	$44, %rcx
	jne	.LBB19_566
# BB#567:                               # %while.end.i
                                        #   in Loop: Header=BB19_566 Depth=2
	decq	%rax
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jg	.LBB19_566
	.align	16, 0x90
.LBB19_568:                             # %while.cond6.i
                                        #   Parent Loop BB19_555 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %ecx
	cmpq	$44, %rcx
	jbe	.LBB19_569
.LBB19_570:                             # %while.body13.i
                                        #   in Loop: Header=BB19_568 Depth=2
	incq	%rdx
	andq	$253, %rcx
	cmpq	$109, %rcx
	jne	.LBB19_568
	jmp	.LBB19_571
.LBB19_569:                             # %while.cond6.i
                                        #   in Loop: Header=BB19_568 Depth=2
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$17626545782785, %rsi   # imm = 0x100800000001
	testq	%rsi, %rax
	je	.LBB19_570
	jmp	.LBB19_572
.LBB19_571:                             # %if.then2810
                                        #   in Loop: Header=BB19_555 Depth=1
	movq	$1, %rax
	movb	%al, 2000(%rsp,%rbx)
	.align	16, 0x90
.LBB19_572:                             # %for.inc2815
                                        #   in Loop: Header=BB19_555 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%r12, %rax
	jne	.LBB19_555
.LBB19_573:                             # %for.end2817
	movq	80(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	296(%rsp), %r12         # 8-byte Reload
	movq	104(%rsp), %rbx         # 8-byte Reload
	je	.LBB19_579
# BB#574:                               # %for.cond2820.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jle	.LBB19_677
# BB#575:
	movq	$1, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %rdx
	andq	%r8, %rdx
	.align	16, 0x90
.LBB19_576:                             # %for.body2823
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1935(%rsp,%rax), %esi
	testq	%rsi, %rsi
	je	.LBB19_578
# BB#577:                               # %if.then2827
                                        #   in Loop: Header=BB19_576 Depth=1
	movslq	n_earlyclobbers(%rip), %rsi
	movq	recog_data-8(,%rax,8), %rdi
	movq	%rbx, %rcx
	leaq	1(%rsi), %rbx
	movl	%ebx, n_earlyclobbers(%rip)
	movq	%rcx, %rbx
	movq	%rdi, reload_earlyclobbers(,%rsi,8)
.LBB19_578:                             # %for.inc2834
                                        #   in Loop: Header=BB19_576 Depth=1
	movq	%rax, %rsi
	andq	%r8, %rsi
	incq	%rax
	cmpq	%rdx, %rsi
	jne	.LBB19_576
.LBB19_579:                             # %for.cond2838.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	jle	.LBB19_677
# BB#580:                               # %for.body2841.lr.ph
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	andq	%r11, %rbx
	cmpq	$1, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %rax
	movabsq	$0, %rcx
	movq	%rcx, 344(%rsp)         # 8-byte Spill
	jbe	.LBB19_582
# BB#581:                               # %for.body2841.lr.ph
	movabsq	$1, %rcx
	movq	%rcx, 344(%rsp)         # 8-byte Spill
.LBB19_582:                             # %for.body2841.lr.ph
	leaq	insn_data+24(,%rax,8), %rax
	movq	%rax, 336(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB19_583:                             # %for.body2841
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_662 Depth 2
                                        #     Child Loop BB19_625 Depth 2
                                        #     Child Loop BB19_648 Depth 2
                                        #     Child Loop BB19_590 Depth 2
	movzbl	2000(%rsp,%r13), %eax
	testq	%rax, %rax
	jne	.LBB19_621
# BB#584:                               # %if.then2845
                                        #   in Loop: Header=BB19_583 Depth=1
	movslq	2192(%rsp,%r13,4), %rax
	testq	%rax, %rax
	jns	.LBB19_669
# BB#585:                               # %if.else2851
                                        #   in Loop: Header=BB19_583 Depth=1
	movslq	2064(%rsp,%r13,4), %rbx
	cmpq	$-1, %rbx
	jne	.LBB19_612
# BB#586:                               # %land.lhs.true2856
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	%r11, %r15
	movzbl	1968(%rsp,%r13), %eax
	testq	%rax, %rax
	je	.LBB19_603
# BB#587:                               # %land.lhs.true2861
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%r13,8), %rdx
	movzbl	(%rdx), %eax
	cmpq	$66, %rax
	jne	.LBB19_603
# BB#588:                               # %if.then2868
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	8(%rdx), %rdi
	movzbl	2(%rdi), %r9d
	addq	$8, %rdx
	movq	%r13, 24(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movabsq	$0, %rsi
	xorq	%rcx, %rcx
	movq	$12, %r8
	movq	$0, (%rsp)
	callq	push_reload
	movl	%eax, 2320(%rsp,%r13,4)
	movq	recog_data(,%r13,8), %rcx
	movzbl	2(%rcx), %ecx
	movzbl	mode_size(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	2960(%rsp,%r13,4), %esi
	sarq	%cl, %rax
	imulq	$104, %rax, %rax
	movl	%edx, rld+36(%rax)
	cmpq	$2, %rsi
	movq	%r15, %r11
	jne	.LBB19_669
# BB#589:                               # %for.cond2908.preheader
                                        #   in Loop: Header=BB19_583 Depth=1
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rdx
	movabsq	$rld+92, %rsi
	jle	.LBB19_669
.LBB19_590:                             # %for.body2911
                                        #   Parent Loop BB19_583 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	-20(%rsi), %ecx
	movq	%r13, %rdi
	andq	%r11, %rdi
	cmpq	%rdi, %rcx
	jne	.LBB19_602
# BB#591:                               # %if.then2917
                                        #   in Loop: Header=BB19_590 Depth=2
	movl	(%rsi), %ecx
	cmpq	$6, %rcx
	jne	.LBB19_592
# BB#600:                               # %if.then2932
                                        #   in Loop: Header=BB19_590 Depth=2
	movq	$4, %rcx
	jmp	.LBB19_601
.LBB19_592:                             # %if.then2917
                                        #   in Loop: Header=BB19_590 Depth=2
	cmpq	$5, %rcx
	jne	.LBB19_602
# BB#593:                               # %if.then2922
                                        #   in Loop: Header=BB19_590 Depth=2
	movq	$3, %rcx
.LBB19_601:                             # %for.inc2939
                                        #   in Loop: Header=BB19_590 Depth=2
	movl	%ecx, (%rsi)
.LBB19_602:                             # %for.inc2939
                                        #   in Loop: Header=BB19_590 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	addq	$104, %rsi
	cmpq	%rax, %rdi
	jl	.LBB19_590
	jmp	.LBB19_669
.LBB19_621:                             # %if.else3122
                                        #   in Loop: Header=BB19_583 Depth=1
	movslq	2064(%rsp,%r13,4), %rax
	movslq	2192(%rsp,%r13,4), %rbx
	testq	%rax, %rax
	jns	.LBB19_654
# BB#622:                               # %land.lhs.true3127
                                        #   in Loop: Header=BB19_583 Depth=1
	testq	%rbx, %rbx
	jns	.LBB19_654
# BB#623:                               # %land.lhs.true3127
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	optimize(%rip), %eax
	testq	%r11, %rax
	je	.LBB19_654
# BB#624:                               # %if.then3134
                                        #   in Loop: Header=BB19_583 Depth=1
	leaq	recog_data(,%r13,8), %rcx
	movq	%rcx, %rax
	jmp	.LBB19_625
	.align	16, 0x90
.LBB19_628:                             # %while.body3144
                                        #   in Loop: Header=BB19_625 Depth=2
	addq	$8, %rax
.LBB19_625:                             # %while.cond3139
                                        #   Parent Loop BB19_583 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rax
	movzbl	(%rax), %edx
	cmpq	$66, %rdx
	je	.LBB19_630
# BB#626:                               # %while.cond3139
                                        #   in Loop: Header=BB19_625 Depth=2
	cmpq	$61, %rdx
	je	.LBB19_629
# BB#627:                               # %while.cond3139
                                        #   in Loop: Header=BB19_625 Depth=2
	cmpq	$63, %rdx
	je	.LBB19_628
	jmp	.LBB19_642
.LBB19_654:                             # %if.else3332
                                        #   in Loop: Header=BB19_583 Depth=1
	testq	%rbx, %rbx
	js	.LBB19_669
# BB#655:                               # %land.lhs.true3337
                                        #   in Loop: Header=BB19_583 Depth=1
	movzbl	2000(%rsp,%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_669
# BB#656:                               # %land.lhs.true3344
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %eax
	testq	%rax, %rax
	jne	.LBB19_669
# BB#657:                               # %land.lhs.true3349
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%rbx,4), %eax
	cmpq	$2, %rax
	movabsq	$0, %rax
	jne	.LBB19_659
# BB#658:                               # %land.lhs.true3349
                                        #   in Loop: Header=BB19_583 Depth=1
	movabsq	$1, %rax
.LBB19_659:                             # %land.lhs.true3349
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	344(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB19_669
# BB#660:                               # %land.lhs.true3349
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	optimize(%rip), %eax
	testq	%r11, %rax
	je	.LBB19_669
# BB#661:                               # %if.then3362
                                        #   in Loop: Header=BB19_583 Depth=1
	leaq	recog_data(,%r13,8), %rax
	jmp	.LBB19_662
	.align	16, 0x90
.LBB19_665:                             # %while.body3372
                                        #   in Loop: Header=BB19_662 Depth=2
	addq	$8, %rax
.LBB19_662:                             # %while.cond3367
                                        #   Parent Loop BB19_583 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	je	.LBB19_667
# BB#663:                               # %while.cond3367
                                        #   in Loop: Header=BB19_662 Depth=2
	cmpq	$61, %rcx
	je	.LBB19_666
# BB#664:                               # %while.cond3367
                                        #   in Loop: Header=BB19_662 Depth=2
	cmpq	$63, %rcx
	je	.LBB19_665
	jmp	.LBB19_669
.LBB19_612:                             # %if.else3028
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %eax
	testq	%rax, %rax
	jne	.LBB19_616
# BB#613:                               # %land.lhs.true3033
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	%r11, %r15
	movl	2960(%rsp,%rbx,4), %eax
	cmpq	$2, %rax
	jne	.LBB19_615
# BB#614:                               # %if.then3040
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%r13,8), %rdi
	movq	recog_data(,%rbx,8), %rsi
	movq	recog_data+240(,%r13,8), %rdx
	movq	recog_data+240(,%rbx,8), %rcx
	movl	656(%rsp,%r13,4), %r9d
	movl	2448(%rsp,%r13,4), %r8d
	movl	656(%rsp,%rbx,4), %eax
	movq	%r13, 24(%rsp)
	movq	%rax, (%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	callq	push_reload
	movl	output_reloadnum(%rip), %ecx
	movl	%eax, 2320(%rsp,%r13,4)
	movl	%ecx, 2320(%rsp,%rbx,4)
	movq	%r15, %r11
	jmp	.LBB19_669
.LBB19_603:                             # %if.then2948
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %eax
	movq	recog_data(,%r13,8), %rdi
	cmpq	$2, %rax
	jne	.LBB19_604
# BB#607:                               # %cond.end2987.cond.false3006_crit_edge
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data+240(,%r13,8), %r10
	movl	656(%rsp,%r13,4), %ebx
	xorq	%rdx, %rdx
	movq	%rdi, %rsi
	xorq	%rdi, %rdi
	xorq	%r9, %r9
	jmp	.LBB19_608
.LBB19_604:                             # %if.then2948
                                        #   in Loop: Header=BB19_583 Depth=1
	testq	%rax, %rax
	jne	.LBB19_606
# BB#605:                               # %cond.end2977.thread
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data+240(,%r13,8), %rdx
	movl	656(%rsp,%r13,4), %r9d
	xorq	%r10, %r10
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	jmp	.LBB19_608
.LBB19_629:                             # %land.lhs.true3158
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	8(%rax), %esi
	cmpq	$53, %rsi
	jb	.LBB19_642
.LBB19_630:                             # %land.lhs.true3164
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2448(%rsp,%r13,4), %r8d
	testq	%r8, %r8
	jne	.LBB19_632
# BB#631:                               # %lor.lhs.false3169
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %esi
	cmpq	$2, %rsi
	jne	.LBB19_642
.LBB19_632:                             # %land.lhs.true3176
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %edi
	testq	%rdi, %rdi
	jne	.LBB19_633
# BB#636:                               # %cond.end3236.thread6366
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%r13,8), %rdi
	movq	recog_data+240(,%r13,8), %rdx
	movl	656(%rsp,%r13,4), %r9d
	xorq	%r10, %r10
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	jmp	.LBB19_639
.LBB19_615:                             # %if.else3068thread-pre-split
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %eax
	movq	%r15, %r11
.LBB19_616:                             # %if.else3068
                                        #   in Loop: Header=BB19_583 Depth=1
	andq	%r11, %rax
	cmpq	$2, %rax
	jne	.LBB19_619
# BB#617:                               # %land.lhs.true3073
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%rbx,4), %eax
	testq	%rax, %rax
	jne	.LBB19_619
# BB#618:                               # %if.then3080
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%rbx,8), %rdi
	movq	recog_data(,%r13,8), %rsi
	movq	recog_data+240(,%rbx,8), %rdx
	movq	recog_data+240(,%r13,8), %rcx
	movl	656(%rsp,%rbx,4), %r9d
	movl	2448(%rsp,%r13,4), %r8d
	movl	656(%rsp,%r13,4), %eax
	movq	%r13, 24(%rsp)
	movq	%rax, (%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	%r11, %r14
	callq	push_reload
	movq	%r14, %r11
	movl	output_reloadnum(%rip), %ecx
	movl	%eax, 2320(%rsp,%rbx,4)
	movl	%ecx, 2320(%rsp,%r13,4)
	jmp	.LBB19_669
.LBB19_606:                             # %cond.end2977
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data+240(,%r13,8), %r10
	movl	656(%rsp,%r13,4), %r9d
	movq	recog_data(,%r13,8), %rsi
	movq	%r10, %rdx
	movq	%r9, %rbx
.LBB19_608:                             # %cond.end3009
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	2448(%rsp,%r13,4), %r8d
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	js	.LBB19_610
# BB#609:                               # %cond.false3014
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	336(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	leaq	(%r13,%r13,2), %rcx
	movsbq	18(%rax,%rcx,8), %rcx
.LBB19_610:                             # %cond.end3021
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	3216(%rsp,%r13,4), %eax
	movq	%rax, 32(%rsp)
	movq	%r13, 24(%rsp)
	movq	%rcx, 8(%rsp)
	movq	%rbx, (%rsp)
	movq	$0, 16(%rsp)
	jmp	.LBB19_611
.LBB19_633:                             # %lor.lhs.false3181
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	this_insn_is_asm(%rip), %esi
	movq	176(%rsp), %rbx         # 8-byte Reload
	orq	%rbx, %rsi
	testq	%r11, %rsi
	jne	.LBB19_642
# BB#634:                               # %if.then3185
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	(%rcx), %rsi
	cmpq	$2, %rdi
	jne	.LBB19_635
# BB#637:                               # %cond.end3224.thread6353
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data+240(,%r13,8), %r10
	xorq	%r9, %r9
	xorq	%rdx, %rdx
	xorq	%rdi, %rdi
	jmp	.LBB19_638
.LBB19_642:                             # %if.else3266
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	184(%rsp), %rcx         # 8-byte Reload
	testq	%r11, %rcx
	je	.LBB19_669
# BB#643:                               # %land.lhs.true3268
                                        #   in Loop: Header=BB19_583 Depth=1
	cmpq	$66, %rdx
	je	.LBB19_647
# BB#644:                               # %land.lhs.true3268
                                        #   in Loop: Header=BB19_583 Depth=1
	cmpq	$61, %rdx
	jne	.LBB19_669
# BB#645:                               # %land.lhs.true3278
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB19_669
# BB#646:                               # %land.lhs.true3284
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	reg_renumber(%rip), %rcx
	movswq	(%rcx,%rax,2), %rax
	testq	%rax, %rax
	jns	.LBB19_669
.LBB19_647:                             # %if.then3293
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data+240(,%r13,8), %rbx
	jmp	.LBB19_648
	.align	16, 0x90
.LBB19_650:                             # %while.body3301
                                        #   in Loop: Header=BB19_648 Depth=2
	addq	$8, %rbx
.LBB19_648:                             # %while.cond3296
                                        #   Parent Loop BB19_583 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	je	.LBB19_651
# BB#649:                               # %while.cond3296
                                        #   in Loop: Header=BB19_648 Depth=2
	cmpq	$63, %rax
	je	.LBB19_650
	jmp	.LBB19_669
.LBB19_651:                             # %if.then3310
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2960(%rsp,%r13,4), %r15d
	cmpq	$2, %r15
	je	.LBB19_653
# BB#652:                               # %if.end3320
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r11, %r14
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	emit_insn_before
	movq	%r14, %r11
	movq	$2, %rcx
	movb	%cl, 2(%rax)
	testq	%r15, %r15
	je	.LBB19_669
.LBB19_653:                             # %if.then3325
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	movq	%r11, %rbx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	208(%rsp), %rsi         # 8-byte Reload
	callq	emit_insn_after
	movq	%rbx, %r11
	jmp	.LBB19_669
.LBB19_666:                             # %land.lhs.true3386
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	8(%rax), %eax
	cmpq	$53, %rax
	jb	.LBB19_669
.LBB19_667:                             # %land.lhs.true3392
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	2448(%rsp,%rbx,4), %r8d
	testq	%r8, %r8
	je	.LBB19_669
# BB#668:                               # %if.then3399
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%rbx,8), %rdi
	movq	recog_data(,%r13,8), %rsi
	movq	recog_data+240(,%rbx,8), %rdx
	movq	recog_data+240(,%r13,8), %rcx
	movl	656(%rsp,%rbx,4), %r9d
	movl	656(%rsp,%r13,4), %eax
	movq	%rbx, 24(%rsp)
	movq	%rax, (%rsp)
	movq	$9, 32(%rsp)
	movq	$1, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	%r11, %r14
	callq	push_reload
	movq	%r14, %r11
	movl	%eax, 2320(%rsp,%rbx,4)
	movl	%eax, 2320(%rsp,%r13,4)
	jmp	.LBB19_669
.LBB19_635:                             # %cond.end3204
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	recog_data(,%r13,8), %rcx
	movq	recog_data+240(,%r13,8), %rdx
	movl	656(%rsp,%r13,4), %r9d
	movq	%rsi, %rdi
	movq	%rdx, %r10
	movq	%rcx, %rsi
.LBB19_638:                             # %cond.false3243
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	656(%rsp,%r13,4), %ebx
.LBB19_639:                             # %cond.end3246
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	%r11, %r15
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	js	.LBB19_641
# BB#640:                               # %cond.false3251
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	336(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	leaq	(%r13,%r13,2), %rcx
	movsbq	18(%rax,%rcx,8), %rcx
.LBB19_641:                             # %cond.end3259
                                        #   in Loop: Header=BB19_583 Depth=1
	movl	3216(%rsp,%r13,4), %eax
	movq	%rax, 32(%rsp)
	movq	%r13, 24(%rsp)
	movq	%rcx, 8(%rsp)
	movq	%rbx, (%rsp)
	movq	$1, 16(%rsp)
.LBB19_611:                             # %for.inc3435
                                        #   in Loop: Header=BB19_583 Depth=1
	movq	%r10, %rcx
	callq	push_reload
	movl	%eax, 2320(%rsp,%r13,4)
	movq	%r15, %r11
	.align	16, 0x90
.LBB19_669:                             # %for.inc3435
                                        #   in Loop: Header=BB19_583 Depth=1
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r12, %rax
	jl	.LBB19_583
# BB#594:                               # %for.cond3438.preheader
	movq	$56, %rcx
	movl	recog_data+996(%rip), %eax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movq	208(%rsp), %rdi         # 8-byte Reload
	jle	.LBB19_677
# BB#595:                               # %for.body3441.lr.ph
	xorq	%rbx, %rbx
	movabsq	$0, %rsi
	movq	%r12, %r13
	andq	%r11, %r13
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB19_596:                             # %for.body3441
                                        # =>This Inner Loop Header: Depth=1
	movq	784(%rsp,%rbx,8), %r15
	movq	recog_data+240(,%rbx,8), %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	testq	%r11, %rcx
	jne	.LBB19_597
# BB#673:                               # %if.else3476
                                        #   in Loop: Header=BB19_596 Depth=1
	movq	(%rax), %rax
	cmpq	%rax, %r15
	movq	%rsi, %rax
	je	.LBB19_675
# BB#674:                               # %if.else3476
                                        #   in Loop: Header=BB19_596 Depth=1
	movabsq	$1, %rax
.LBB19_675:                             # %if.else3476
                                        #   in Loop: Header=BB19_596 Depth=1
	orq	%rax, %rdx
	jmp	.LBB19_676
.LBB19_597:                             # %if.then3443
                                        #   in Loop: Header=BB19_596 Depth=1
	movq	%r15, (%rax)
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	je	.LBB19_676
# BB#598:                               # %land.lhs.true3453
                                        #   in Loop: Header=BB19_596 Depth=1
	movq	%rsi, %r14
	movzbl	(%r15), %eax
	cmpq	$67, %rax
	jne	.LBB19_599
# BB#670:                               # %land.lhs.true3458
                                        #   in Loop: Header=BB19_596 Depth=1
	movq	%rdx, 304(%rsp)         # 8-byte Spill
	movq	8(%r15), %rdx
	movq	$13, %rsi
	movq	%rdi, %r12
	callq	find_reg_note
	testq	%rax, %rax
	jne	.LBB19_671
# BB#672:                               # %if.then3464
                                        #   in Loop: Header=BB19_596 Depth=1
	movq	8(%r15), %rdx
	movq	56(%r12), %rcx
	movq	$4, %rdi
	movq	$13, %rsi
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r12)
.LBB19_671:                             #   in Loop: Header=BB19_596 Depth=1
	movq	%r12, %rdi
	movq	296(%rsp), %r12         # 8-byte Reload
	movq	304(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r14, %rsi
	jmp	.LBB19_676
.LBB19_599:                             #   in Loop: Header=BB19_596 Depth=1
	movq	%r14, %rsi
	.align	16, 0x90
.LBB19_676:                             # %for.inc3485
                                        #   in Loop: Header=BB19_596 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r11, %rax
	cmpq	%r13, %rax
	jne	.LBB19_596
.LBB19_677:                             # %for.end3487
	movq	%rdx, 304(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_685
# BB#678:                               # %for.end3487
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	184(%rsp), %rax         # 8-byte Reload
	testq	%r8, %rax
	je	.LBB19_685
# BB#679:                               # %if.then3492
	movq	96(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%rax,4), %rcx
	movsbq	insn_data+33(,%rcx,8), %rdx
	testq	%rdx, %rdx
	jle	.LBB19_685
# BB#680:                               # %for.body3500.lr.ph
	movl	replace_reloads(%rip), %r9d
	leaq	insn_data+24(,%rcx,8), %r10
	decq	%rdx
	.align	16, 0x90
.LBB19_681:                             # %for.body3500
                                        # =>This Inner Loop Header: Depth=1
	movsbq	recog_data+992(%rdx), %rbx
	movq	recog_data+240(,%rbx,8), %rcx
	movq	(%rcx), %rcx
	movq	recog_data+960(,%rdx,8), %rax
	movq	%rcx, (%rax)
	movslq	2320(%rsp,%rbx,4), %rcx
	testq	%rcx, %rcx
	js	.LBB19_684
# BB#682:                               # %for.body3500
                                        #   in Loop: Header=BB19_681 Depth=1
	testq	%r8, %r9
	je	.LBB19_684
# BB#683:                               # %if.then.i5594
                                        #   in Loop: Header=BB19_681 Depth=1
	movq	(%r10), %rax
	leaq	(%rbx,%rbx,2), %rbx
	movzbl	16(%rax,%rbx,8), %eax
	movslq	n_replacements(%rip), %rbx
	movq	recog_data+960(,%rdx,8), %rsi
	leaq	1(%rbx), %rdi
	movl	%edi, n_replacements(%rip)
	leaq	(%rbx,%rbx,2), %rdi
	movl	%ecx, replacements+16(,%rdi,8)
	movq	%rsi, replacements(,%rdi,8)
	movq	$0, replacements+8(,%rdi,8)
	movl	%eax, replacements+20(,%rdi,8)
.LBB19_684:                             # %for.cond3497.backedge
                                        #   in Loop: Header=BB19_681 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rdx
	testq	%rax, %rax
	jg	.LBB19_681
.LBB19_685:                             # %if.end3528
	movq	%r12, 296(%rsp)         # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_799
# BB#686:                               # %if.then3530
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	jle	.LBB19_823
# BB#687:
	movq	$1, %rdx
	movabsq	$rld+8, %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB19_688:                             # %for.body.i5584
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rsi)
	je	.LBB19_691
# BB#689:                               # %if.then.i5586
                                        #   in Loop: Header=BB19_688 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	jns	.LBB19_799
# BB#690:                               #   in Loop: Header=BB19_688 Depth=1
	leaq	-1(%rdx), %rbx
.LBB19_691:                             # %for.inc.i5588
                                        #   in Loop: Header=BB19_688 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	addq	$104, %rsi
	cmpq	%rax, %rdi
	jl	.LBB19_688
# BB#692:                               # %for.end.i
	movq	$32, %rcx
	movq	%rbx, %rdx
	movq	%rbx, %r15
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB19_799
# BB#693:                               # %lor.lhs.false.i
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$104, %rdx, %r14
	movzbl	rld+96(%r14), %ecx
	testq	$1, %rcx
	jne	.LBB19_799
# BB#694:                               # %if.end9.i
	cmpq	$0, rld(%r14)
	jne	.LBB19_799
# BB#695:                               # %if.end14.i
	movslq	n_earlyclobbers(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB19_699
# BB#696:
	movq	rld+8(%r14), %rsi
	movq	$1, %rbx
.LBB19_697:                             # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	reload_earlyclobbers-8(,%rbx,8), %rcx
	cmpq	%rsi, %rcx
	je	.LBB19_799
# BB#698:                               # %for.cond.i.i
                                        #   in Loop: Header=BB19_697 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rbx
	cmpq	%rdx, %rdi
	jl	.LBB19_697
.LBB19_699:                             # %for.body23.lr.ph.i
	leaq	rld+8(%r14), %rbx
	leaq	rld+72(%r14), %r13
	movq	$1, %rdx
	movabsq	$rld+92, %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB19_700:                             # %for.body23.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %ecx
	addq	$-5, %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	ja	.LBB19_702
# BB#701:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB19_700 Depth=1
	movl	-20(%rsi), %ecx
	movl	(%r13), %edi
	cmpq	%rdi, %rcx
	je	.LBB19_799
.LBB19_702:                             # %for.inc40.i
                                        #   in Loop: Header=BB19_700 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	addq	$104, %rsi
	cmpq	%rax, %rdi
	jl	.LBB19_700
# BB#703:                               # %for.body45.lr.ph.i
	leaq	rld+16(%r14), %rax
	movq	%rax, 344(%rsp)         # 8-byte Spill
	leaq	rld+24(%r14), %rax
	movq	%rax, 336(%rsp)         # 8-byte Spill
	movq	$1, 328(%rsp)           # 8-byte Folded Spill
	movabsq	$rld+96, %rax
	movq	%rax, 320(%rsp)         # 8-byte Spill
.LBB19_704:                             # %for.body45.i
                                        # =>This Inner Loop Header: Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	cmpq	$0, -96(%rax)
	je	.LBB19_754
# BB#705:                               # %land.lhs.true50.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$3, %rax
	jne	.LBB19_754
# BB#706:                               # %land.lhs.true65.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	-4(%rax), %eax
	cmpq	$9, %rax
	ja	.LBB19_708
# BB#707:                               # %land.lhs.true65.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$608, %rdx              # imm = 0x260
	jne	.LBB19_754
.LBB19_708:                             # %land.lhs.true80.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	-80(%rax), %eax
	movq	$12, %rsi
	movq	%rax, %rdi
	callq	reg_classes_intersect_p
	movq	320(%rsp), %rcx         # 8-byte Reload
	movslq	-76(%rcx), %rcx
	testq	%r12, %rax
	jne	.LBB19_713
# BB#709:                               # %cond.true.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movl	mode_class(,%rcx,4), %eax
	cmpq	$5, %rax
	movq	$2, 312(%rsp)           # 8-byte Folded Spill
	je	.LBB19_716
# BB#710:                               # %lor.rhs.i
                                        #   in Loop: Header=BB19_704 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, 312(%rsp)         # 8-byte Spill
	jne	.LBB19_712
# BB#711:                               # %lor.rhs.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movabsq	$1, %rax
	movq	%rax, 312(%rsp)         # 8-byte Spill
.LBB19_712:                             # %lor.rhs.i
                                        #   in Loop: Header=BB19_704 Depth=1
	incq	312(%rsp)               # 8-byte Folded Spill
	jmp	.LBB19_716
.LBB19_713:                             # %cond.false.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rcx, %rax
	andq	%r12, %rax
	cmpq	$18, %rax
	movq	$17, %rax
	je	.LBB19_715
# BB#714:                               # %cond.false.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rcx, %rax
.LBB19_715:                             # %cond.false.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	movq	%rax, 312(%rsp)         # 8-byte Spill
.LBB19_716:                             # %cond.end113.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	$12, %rsi
	movq	%rax, %rdi
	callq	reg_classes_intersect_p
	movq	336(%rsp), %rcx         # 8-byte Reload
	movslq	(%rcx), %rsi
	testq	%r12, %rax
	jne	.LBB19_721
# BB#717:                               # %cond.true120.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movl	mode_class(,%rsi,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB19_724
# BB#718:                               # %lor.rhs127.i
                                        #   in Loop: Header=BB19_704 Depth=1
	cmpq	$6, %rcx
	movabsq	$0, %rax
	jne	.LBB19_720
# BB#719:                               # %lor.rhs127.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movabsq	$1, %rax
.LBB19_720:                             # %lor.rhs127.i
                                        #   in Loop: Header=BB19_704 Depth=1
	incq	%rax
	jmp	.LBB19_724
.LBB19_721:                             # %cond.false137.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rsi, %rax
	andq	%r12, %rax
	cmpq	$18, %rax
	movq	$17, %rax
	je	.LBB19_723
# BB#722:                               # %cond.false137.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rsi, %rax
.LBB19_723:                             # %cond.false137.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movl	target_flags(%rip), %edi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rdi
	andq	$4, %rdi
	leaq	3(%rax,%rdi), %rax
	addq	$4, %rdi
	cqto
	idivq	%rdi
.LBB19_724:                             # %cond.end162.i
                                        #   in Loop: Header=BB19_704 Depth=1
	andq	%r12, %rax
	movq	312(%rsp), %rcx         # 8-byte Reload
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jne	.LBB19_754
# BB#725:                               # %land.lhs.true166.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	-60(%rax), %eax
	testq	%rax, %rax
	jne	.LBB19_754
# BB#726:                               # %land.lhs.true172.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	cmpq	$0, -32(%rax)
	jne	.LBB19_754
# BB#727:                               # %land.lhs.true177.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movslq	-24(%rax), %rax
	imulq	$240, %rsi, %rcx
	movq	secondary_memlocs_elim(%rcx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB19_730
# BB#728:                               # %lor.lhs.false190.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movslq	(%r13), %rdx
	movq	secondary_memlocs_elim(%rcx,%rdx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB19_730
# BB#729:                               # %lor.lhs.false203.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB19_754
.LBB19_730:                             # %cond.true227.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	-80(%rax), %eax
	movq	344(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB19_754
# BB#731:                               # %land.lhs.true254.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	-96(%rax), %rcx
	movq	(%rbx), %rax
	cmpq	%rax, %rcx
	je	.LBB19_741
# BB#732:                               # %lor.lhs.false263.i
                                        #   in Loop: Header=BB19_704 Depth=1
	testq	%rcx, %rcx
	je	.LBB19_738
# BB#733:                               # %land.lhs.true269.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB19_736
# BB#734:                               # %cond.true277.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movzbl	(%rax), %edx
	cmpq	$61, %rdx
	jne	.LBB19_738
# BB#735:                               # %land.lhs.true285.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movl	8(%rcx), %ecx
	movl	8(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB19_738
	jmp	.LBB19_741
.LBB19_736:                             # %cond.false298.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	je	.LBB19_738
# BB#737:                               # %land.lhs.true307.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	-96(%rax), %rax
	movq	%rax, %rdi
	callq	side_effects_p
	testq	%r12, %rax
	je	.LBB19_741
.LBB19_738:                             # %lor.lhs.false313.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	(%rbx), %rax
	movq	320(%rsp), %rcx         # 8-byte Reload
	movq	-96(%rcx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r12, %rax
	jne	.LBB19_754
# BB#739:                               # %land.lhs.true322.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	-96(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_741
# BB#740:                               # %land.lhs.true330.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	(%rbx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r12, %rax
	jne	.LBB19_754
.LBB19_741:                             # %land.lhs.true339.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rcx         # 8-byte Reload
	movq	-96(%rcx), %rax
	movl	-76(%rcx), %ecx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reload_inner_reg_of_subreg
	testq	%r12, %rax
	jne	.LBB19_754
# BB#742:                               # %land.lhs.true348.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	-4(%rax), %eax
	cmpq	$1, %rax
	jbe	.LBB19_743
.LBB19_754:                             # %for.inc476.i
                                        #   in Loop: Header=BB19_704 Depth=1
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	328(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rsi
	movq	%rsi, 328(%rsp)         # 8-byte Spill
	addq	$104, 320(%rsp)         # 8-byte Folded Spill
	cmpq	%rax, %rdx
	jl	.LBB19_704
# BB#755:                               # %for.end478.i
	movq	this_insn(%rip), %rax
	movslq	40(%rax), %rcx
	cmpq	$-1, %rcx
	je	.LBB19_799
# BB#756:                               # %for.cond485.preheader.i
	leaq	(%rcx,%rcx,4), %rcx
	movsbq	insn_data+32(,%rcx,8), %rdx
	cmpq	$2, %rdx
	jl	.LBB19_761
# BB#757:                               # %for.body494.lr.ph.i
	movq	insn_data+24(,%rcx,8), %rsi
	addq	$32, %rsi
	movq	$2, %rdi
.LBB19_758:                             # %for.body494.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$43, %rcx
	je	.LBB19_799
# BB#759:                               # %for.body494.i
                                        #   in Loop: Header=BB19_758 Depth=1
	cmpq	$61, %rcx
	je	.LBB19_799
# BB#760:                               # %for.inc522.i
                                        #   in Loop: Header=BB19_758 Depth=1
	movq	$32, %rcx
	movq	%rdi, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	incq	%rdi
	addq	$24, %rsi
	cmpq	%rdx, %r8
	jl	.LBB19_758
.LBB19_761:                             # %for.end524.i
	movq	56(%rax), %r15
	testq	%r15, %r15
	je	.LBB19_799
# BB#762:                               # %for.body529.lr.ph.i
	leaq	rld+80(%r14), %r13
.LBB19_763:                             # %for.body529.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r15), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB19_798
# BB#764:                               # %land.lhs.true535.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_798
# BB#765:                               # %land.lhs.true543.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	(%rbx), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	reg_overlap_mentioned_for_reload_p
	testq	%r12, %rax
	jne	.LBB19_798
# BB#766:                               # %land.lhs.true552.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	8(%r15), %rax
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB19_798
# BB#767:                               # %land.lhs.true561.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	336(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %ecx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	ix86_hard_regno_mode_ok
	testq	%r12, %rax
	je	.LBB19_798
# BB#768:                               # %land.lhs.true573.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	8(%r15), %r10
	movq	344(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movl	8(%r10), %esi
	movq	reg_class_contents(,%rax,8), %rax
	movq	$1, %rdi
	movq	%rsi, %rcx
	shlq	%cl, %rdi
	testq	%rax, %rdi
	je	.LBB19_798
# BB#769:                               # %land.lhs.true587.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	336(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %r8
	movq	%rsi, %rax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	andq	%rcx, %rax
	cmpq	$8, %rax
	je	.LBB19_773
# BB#770:                               # %land.lhs.true587.i
                                        #   in Loop: Header=BB19_763 Depth=1
	leaq	-21(%rsi), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB19_773
# BB#771:                               # %land.lhs.true587.i
                                        #   in Loop: Header=BB19_763 Depth=1
	leaq	-45(%rsi), %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	jb	.LBB19_773
# BB#772:                               # %land.lhs.true587.i
                                        #   in Loop: Header=BB19_763 Depth=1
	leaq	-29(%rsi), %rax
	andq	%r12, %rax
	cmpq	$7, %rax
	jbe	.LBB19_773
# BB#781:                               # %cond.false677.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	%r8, %rax
	andq	%r12, %rax
	cmpq	$24, %rax
	jne	.LBB19_782
# BB#785:                               # %cond.true693.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movl	target_flags(%rip), %r11d
	movq	$24, %rcx
	movq	%r11, %r9
	shrq	%cl, %r9
	andq	$2, %r9
	xorq	$6, %r9
	jmp	.LBB19_786
.LBB19_773:                             # %cond.true659.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movl	mode_class(,%r8,4), %eax
	cmpq	$5, %rax
	movq	$2, %r9
	je	.LBB19_777
# BB#774:                               # %lor.rhs667.i
                                        #   in Loop: Header=BB19_763 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %r9
	jne	.LBB19_776
# BB#775:                               # %lor.rhs667.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movabsq	$1, %rax
	movq	%rax, %r9
.LBB19_776:                             # %lor.rhs667.i
                                        #   in Loop: Header=BB19_763 Depth=1
	incq	%r9
.LBB19_777:                             # %cond.true790.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movzbl	2(%r10), %eax
	movl	mode_class(,%rax,4), %eax
	cmpq	$5, %rax
	je	.LBB19_792
# BB#778:                               # %lor.rhs801.i
                                        #   in Loop: Header=BB19_763 Depth=1
	cmpq	$6, %rax
	movabsq	$0, %rax
	movq	%rax, %r11
	jne	.LBB19_780
# BB#779:                               # %lor.rhs801.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movabsq	$1, %rax
	movq	%rax, %r11
.LBB19_780:                             # %lor.rhs801.i
                                        #   in Loop: Header=BB19_763 Depth=1
	incq	%r11
	jmp	.LBB19_791
.LBB19_782:                             # %cond.false677.i
                                        #   in Loop: Header=BB19_763 Depth=1
	cmpq	$18, %rax
	jne	.LBB19_784
# BB#783:                               # %cond.end717.thread1166.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movl	target_flags(%rip), %r11d
	movq	$25, %rcx
	movq	%r11, %r9
	shrq	%cl, %r9
	andq	$1, %r9
	xorq	$3, %r9
	jmp	.LBB19_786
.LBB19_784:                             # %cond.false697.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movl	target_flags(%rip), %r11d
	movzbl	mode_size(%r8), %eax
	movq	$23, %rcx
	movq	%r11, %r9
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
	movq	%rax, %r9
.LBB19_786:                             # %cond.false814.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movzbl	2(%r10), %eax
	cmpq	$24, %rax
	jne	.LBB19_787
# BB#789:                               # %cond.true836.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	$24, %rcx
	shrq	%cl, %r11
	andq	$2, %r11
	xorq	$6, %r11
	jmp	.LBB19_791
.LBB19_787:                             # %cond.false814.i
                                        #   in Loop: Header=BB19_763 Depth=1
	cmpq	$18, %rax
	jne	.LBB19_790
# BB#788:                               # %cond.true823.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	$25, %rcx
	shrq	%cl, %r11
	andq	$1, %r11
	xorq	$3, %r11
	jmp	.LBB19_791
.LBB19_790:                             # %cond.false840.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %r11
	andq	$4, %r11
	leaq	3(%rax,%r11), %rax
	addq	$4, %r11
	cqto
	idivq	%r11
	movq	%rax, %r11
.LBB19_791:                             # %cond.end863.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	cmpq	%r11, %r9
	jg	.LBB19_798
.LBB19_792:                             # %land.lhs.true867.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movslq	(%r13), %rax
	cmpq	$-1, %rax
	je	.LBB19_796
# BB#793:                               # %lor.lhs.false873.i
                                        #   in Loop: Header=BB19_763 Depth=1
	imulq	$104, %rax, %rax
	movslq	rld+16(%rax), %rcx
	movq	reg_class_contents(,%rcx,8), %rcx
	testq	%rdi, %rcx
	jne	.LBB19_798
# BB#794:                               # %land.lhs.true889.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movslq	rld+80(%rax), %rax
	cmpq	$-1, %rax
	je	.LBB19_796
# BB#795:                               # %lor.lhs.false895.i
                                        #   in Loop: Header=BB19_763 Depth=1
	imulq	$104, %rax, %rax
	movslq	rld+16(%rax), %rax
	movq	reg_class_contents(,%rax,8), %rax
	testq	%rdi, %rax
	jne	.LBB19_798
.LBB19_796:                             # %land.lhs.true911.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movzbl	fixed_regs(%rsi), %eax
	testq	%rax, %rax
	je	.LBB19_797
.LBB19_798:                             # %for.inc936.i
                                        #   in Loop: Header=BB19_763 Depth=1
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB19_763
	jmp	.LBB19_799
.LBB19_619:                             # %if.else3108
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB19_620
.LBB19_524:                             # %if.end2567
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	208(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	error_for_asm
	movq	const_int_rtx+512(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	xorq	%rcx, %rcx
	movq	%rax, 32(%rbx)
	movl	%ecx, n_reloads(%rip)
	jmp	.LBB19_60
.LBB19_25:                              # %if.then158
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.find_reloads, %rdx
	movq	$2594, %rsi             # imm = 0xA22
	callq	fancy_abort
.LBB19_37:                              # %if.then135
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads, %rcx
	movq	$2580, %rsi             # imm = 0xA14
	jmp	.LBB19_38
.LBB19_469:                             # %if.then2231
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads, %rcx
	movq	$3403, %rsi             # imm = 0xD4B
	jmp	.LBB19_38
.LBB19_960:                             # %if.else2242
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads, %rcx
	movq	$3412, %rsi             # imm = 0xD54
	jmp	.LBB19_38
.LBB19_743:                             # %if.then368.i
	movq	%rbx, %rcx
	movq	(%rcx), %rax
	movq	320(%rsp), %rdx         # 8-byte Reload
	movq	%rax, -88(%rdx)
	movq	rld+48(%r14), %rax
	movq	%rax, -48(%rdx)
	movq	336(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movl	%eax, -72(%rdx)
	movq	$0, (%rcx)
	movq	$9, %rax
	movl	%eax, -4(%rdx)
	movq	%rdx, %rbx
	movl	rld+80(%r14), %eax
	cmpq	%r12, %rax
	je	.LBB19_745
# BB#744:                               # %if.then396.i
	movq	%rbx, %rcx
	movl	%eax, -16(%rcx)
	movl	rld+88(%r14), %eax
	movl	%eax, -8(%rcx)
                                        # kill: RBX<def> RCX<kill>
.LBB19_745:                             # %if.end408.i
	movq	336(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movslq	(%r13), %rcx
	imulq	$240, %rax, %rax
	movq	secondary_memlocs_elim(%rax,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB19_747
# BB#746:                               # %if.then421.i
	movslq	-24(%rbx), %rdx
	movq	%rcx, secondary_memlocs_elim(%rax,%rdx,8)
.LBB19_747:                             # %if.end442.i
	movl	-80(%rbx), %eax
	movq	344(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %ecx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	reg_class_subset_p
	testq	%r12, %rax
	je	.LBB19_749
# BB#748:                               # %if.then451.i
	movq	344(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movl	%eax, -80(%rbx)
.LBB19_749:                             # %for.cond459.preheader.i
	movslq	n_replacements(%rip), %rax
	testq	%rax, %rax
	jle	.LBB19_799
# BB#750:
	decq	328(%rsp)               # 8-byte Folded Spill
	movq	$1, %rcx
	movabsq	$replacements+16, %rdx
	andq	%r12, %r15
	andq	%r12, %rax
.LBB19_751:                             # %for.body462.i
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %esi
	cmpq	%r15, %rsi
	jne	.LBB19_753
# BB#752:                               # %if.then467.i
                                        #   in Loop: Header=BB19_751 Depth=1
	movq	328(%rsp), %rsi         # 8-byte Reload
	movl	%esi, (%rdx)
.LBB19_753:                             # %for.inc472.i
                                        #   in Loop: Header=BB19_751 Depth=1
	movq	%rcx, %rsi
	andq	%r12, %rsi
	incq	%rcx
	addq	$24, %rdx
	cmpq	%rax, %rsi
	jne	.LBB19_751
	jmp	.LBB19_799
.LBB19_797:                             # %if.then921.i
	movq	%r8, %rdi
	callq	gen_rtx_REG
	movq	%rax, rld+64(%r14)
.LBB19_799:                             # %for.cond3532.preheader
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	jle	.LBB19_823
# BB#800:
	xorq	%rdi, %rdi
	movq	$1, 312(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_801:                             # %for.body3535
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_803 Depth 2
                                        #       Child Loop BB19_813 Depth 3
	movq	%rdi, 344(%rsp)         # 8-byte Spill
	leaq	1(%rdi), %rdx
	movq	%rdx, 328(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB19_820
# BB#802:                               # %for.body3541.lr.ph
                                        #   in Loop: Header=BB19_801 Depth=1
	movq	344(%rsp), %rax         # 8-byte Reload
	imulq	$104, %rax, %rax
	leaq	rld+92(%rax), %r14
	leaq	rld(%rax), %rcx
	movq	%rcx, 336(%rsp)         # 8-byte Spill
	leaq	rld+72(%rax), %rax
	movq	%rax, 320(%rsp)         # 8-byte Spill
	movq	312(%rsp), %r13         # 8-byte Reload
	movq	328(%rsp), %r15         # 8-byte Reload
	.align	16, 0x90
.LBB19_803:                             # %for.body3541
                                        #   Parent Loop BB19_801 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_813 Depth 3
	movl	(%r14), %eax
	addq	$-3, %rax
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB19_819
# BB#804:                               # %land.lhs.true3565
                                        #   in Loop: Header=BB19_803 Depth=2
	imulq	$104, %r13, %r12
	movl	rld+92(%r12), %eax
	addq	$-3, %rax
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB19_819
# BB#805:                               # %land.lhs.true3589
                                        #   in Loop: Header=BB19_803 Depth=2
	movq	336(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	rld(%r12), %rsi
	callq	rtx_equal_p
	testq	%rbx, %rax
	je	.LBB19_819
# BB#806:                               # %land.lhs.true3597
                                        #   in Loop: Header=BB19_803 Depth=2
	movq	320(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rax
	movslq	2320(%rsp,%rax,4), %rcx
	testq	%rcx, %rcx
	js	.LBB19_808
# BB#807:                               # %lor.lhs.false3605
                                        #   in Loop: Header=BB19_803 Depth=2
	imulq	$104, %rcx, %rcx
	movzbl	rld+96(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB19_819
.LBB19_808:                             # %land.lhs.true3617
                                        #   in Loop: Header=BB19_803 Depth=2
	movslq	rld+72(%r12), %rcx
	movslq	2320(%rsp,%rcx,4), %rdx
	testq	%rdx, %rdx
	js	.LBB19_810
# BB#809:                               # %lor.lhs.false3625
                                        #   in Loop: Header=BB19_803 Depth=2
	imulq	$104, %rdx, %rdx
	movzbl	rld+96(%rdx), %edx
	testq	$1, %rdx
	je	.LBB19_819
.LBB19_810:                             # %land.lhs.true3638
                                        #   in Loop: Header=BB19_803 Depth=2
	movl	2192(%rsp,%rax,4), %edx
	movq	%rcx, %rsi
	andq	%rbx, %rsi
	cmpq	%rsi, %rdx
	je	.LBB19_812
# BB#811:                               # %lor.lhs.false3649
                                        #   in Loop: Header=BB19_803 Depth=2
	movl	2192(%rsp,%rcx,4), %ecx
	andq	%rbx, %rax
	cmpq	%rax, %rcx
	jne	.LBB19_819
.LBB19_812:                             # %for.cond3661.preheader
                                        #   in Loop: Header=BB19_803 Depth=2
	movslq	n_replacements(%rip), %rax
	testq	%rax, %rax
	movq	$1, %rdx
	movabsq	$replacements+16, %rsi
	jle	.LBB19_816
	.align	16, 0x90
.LBB19_813:                             # %for.body3664
                                        #   Parent Loop BB19_801 Depth=1
                                        #     Parent Loop BB19_803 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rsi), %ecx
	movq	%r13, %rdi
	andq	%rbx, %rdi
	cmpq	%rdi, %rcx
	jne	.LBB19_815
# BB#814:                               # %if.then3669
                                        #   in Loop: Header=BB19_813 Depth=3
	movq	344(%rsp), %rcx         # 8-byte Reload
	movl	%ecx, (%rsi)
.LBB19_815:                             # %for.inc3674
                                        #   in Loop: Header=BB19_813 Depth=3
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rdx
	addq	$24, %rsi
	cmpq	%rax, %rdi
	jl	.LBB19_813
.LBB19_816:                             # %for.end3676
                                        #   in Loop: Header=BB19_803 Depth=2
	movl	(%r14), %edx
	leaq	rld(%r12), %rax
	movq	$8, %rcx
	orq	$2, %rdx
	andq	%rbx, %rdx
	cmpq	$6, %rdx
	je	.LBB19_818
# BB#817:                               # %if.else3692
                                        #   in Loop: Header=BB19_803 Depth=2
	movq	$7, %rcx
.LBB19_818:                             # %if.end3696
                                        #   in Loop: Header=BB19_803 Depth=2
	movl	%ecx, (%r14)
	movq	$0, (%rax)
	.align	16, 0x90
.LBB19_819:                             # %for.inc3701
                                        #   in Loop: Header=BB19_803 Depth=2
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r13
	cmpq	%rax, %rdx
	jl	.LBB19_803
.LBB19_820:                             # %for.cond3532.loopexit
                                        #   in Loop: Header=BB19_801 Depth=1
	movq	$32, %rcx
	movq	328(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	312(%rsp)               # 8-byte Folded Spill
	cmpq	%rsi, %rdx
	jl	.LBB19_801
# BB#821:                               # %for.cond3707.preheader
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB19_822
# BB#827:                               # %for.cond3707.preheader
	movabsq	$1, %r8
	jmp	.LBB19_828
.LBB19_823:                             # %for.cond3707.preheader.thread
	testq	%rax, %rax
	movq	296(%rsp), %rbx         # 8-byte Reload
	jle	.LBB19_824
# BB#825:                               # %for.cond3707.preheader.thread
	movabsq	$1, %r8
	movq	208(%rsp), %r9          # 8-byte Reload
	jmp	.LBB19_860
.LBB19_822:
	movabsq	$0, %r8
.LBB19_828:                             # %for.cond3707.preheader
	jle	.LBB19_829
# BB#830:
	movq	$1, %r12
	movabsq	$rld+72, %rdi
	movq	$8, %r11
	movq	$7, %r9
	movq	$10, %r10
	.align	16, 0x90
.LBB19_831:                             # %for.body3710
                                        # =>This Inner Loop Header: Depth=1
	movzbl	24(%rdi), %edx
	movl	20(%rdi), %esi
	testq	$4, %rdx
	je	.LBB19_834
# BB#832:                               # %land.lhs.true3718
                                        #   in Loop: Header=BB19_831 Depth=1
	movslq	(%rdi), %rdx
	movl	3216(%rsp,%rdx,4), %r14d
	movq	%rsi, %rcx
	andq	%rbx, %rcx
	cmpq	%r14, %rcx
	jne	.LBB19_834
# BB#833:                               # %if.then3729
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	3088(%rsp,%rdx,4), %esi
	movl	%esi, 20(%rdi)
.LBB19_834:                             # %if.end3738
                                        #   in Loop: Header=BB19_831 Depth=1
	addq	$-3, %rsi
	andq	%rbx, %rsi
	cmpq	$3, %rsi
	ja	.LBB19_851
# BB#835:                               # %land.lhs.true3762
                                        #   in Loop: Header=BB19_831 Depth=1
	movslq	(%rdi), %rcx
	movslq	2320(%rsp,%rcx,4), %rdx
	testq	%rdx, %rdx
	js	.LBB19_837
# BB#836:                               # %lor.lhs.false3770
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rdx, %rcx
	movzbl	rld+96(%rcx), %ecx
	testq	$1, %rcx
	jne	.LBB19_837
.LBB19_851:                             # %if.end3887
                                        #   in Loop: Header=BB19_831 Depth=1
	cmpq	$2, %rsi
	jae	.LBB19_852
# BB#853:                               # %land.lhs.true3899
                                        #   in Loop: Header=BB19_831 Depth=1
	movslq	(%rdi), %rdx
	movslq	2320(%rsp,%rdx,4), %rcx
	testq	%rcx, %rcx
	js	.LBB19_856
# BB#854:                               # %land.lhs.true3907
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rcx, %rcx
	movl	rld+92(%rcx), %ecx
	cmpq	$9, %rcx
	jne	.LBB19_856
# BB#855:                               # %if.then3918
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	%r10d, 20(%rdi)
	jmp	.LBB19_856
.LBB19_837:                             # %if.then3783
                                        #   in Loop: Header=BB19_831 Depth=1
	cmpq	$1, %rsi
	ja	.LBB19_842
# BB#838:                               # %land.lhs.true3795
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	4(%rdi), %r15d
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rbx, %r15
	je	.LBB19_842
# BB#839:                               # %if.then3800
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rdx, %r14
	movl	%r11d, rld+92(%r14)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	testq	%r15, %r15
	jle	.LBB19_842
# BB#840:                               # %land.lhs.true3811
                                        #   in Loop: Header=BB19_831 Depth=1
	movslq	rld+76(%r14), %rcx
	cmpq	$-1, %rcx
	je	.LBB19_842
# BB#841:                               # %if.then3817
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rcx, %rcx
	movl	%r11d, rld+92(%rcx)
.LBB19_842:                             # %if.end3825
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	20(%rdi), %ecx
	leaq	-5(%rcx), %rdx
	andq	%rbx, %rdx
	cmpq	$1, %rdx
	ja	.LBB19_848
# BB#843:                               # %land.lhs.true3837
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	8(%rdi), %r14d
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rbx, %r14
	je	.LBB19_847
# BB#844:                               # %if.then3842
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rdx, %rcx
	movl	%r11d, rld+92(%rcx)
	testq	%r14, %r14
	je	.LBB19_847
# BB#845:                               # %land.lhs.true3852
                                        #   in Loop: Header=BB19_831 Depth=1
	movslq	rld+80(%rcx), %rcx
	cmpq	$-1, %rcx
	je	.LBB19_847
# BB#846:                               # %if.then3858
                                        #   in Loop: Header=BB19_831 Depth=1
	imulq	$104, %rcx, %rcx
	movl	%r11d, rld+92(%rcx)
.LBB19_847:                             # %if.end3866thread-pre-split
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	20(%rdi), %ecx
.LBB19_848:                             # %if.end3866
                                        #   in Loop: Header=BB19_831 Depth=1
	orq	$2, %rcx
	andq	%rbx, %rcx
	cmpq	$6, %rcx
	jne	.LBB19_850
# BB#849:                               # %if.then3878
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	%r11d, 20(%rdi)
	movl	(%rdi), %edx
	jmp	.LBB19_856
.LBB19_852:                             # %if.end3887.if.end3922_crit_edge
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	(%rdi), %edx
	jmp	.LBB19_856
.LBB19_850:                             # %if.else3882
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	%r9d, 20(%rdi)
	movl	(%rdi), %edx
	.align	16, 0x90
.LBB19_856:                             # %if.end3922
                                        #   in Loop: Header=BB19_831 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movslq	2192(%rsp,%rdx,4), %rcx
	testq	%rcx, %rcx
	js	.LBB19_858
# BB#857:                               # %if.then3930
                                        #   in Loop: Header=BB19_831 Depth=1
	movl	%ecx, (%rdi)
.LBB19_858:                             # %for.inc3940
                                        #   in Loop: Header=BB19_831 Depth=1
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	addq	$104, %rdi
	cmpq	%rsi, %rdx
	jl	.LBB19_831
	jmp	.LBB19_859
.LBB19_824:
	movabsq	$0, %r8
	movq	208(%rsp), %r9          # 8-byte Reload
	jmp	.LBB19_860
.LBB19_829:
	xorq	%r8, %r8
.LBB19_859:
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	296(%rsp), %rbx         # 8-byte Reload
.LBB19_860:                             # %for.end3942
	movq	$56, %rcx
	movl	recog_data+996(%rip), %edx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB19_863
# BB#861:
	movq	$1, %rcx
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rbx
	.align	16, 0x90
.LBB19_862:                             # %for.body3950
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, 348(%rsp,%rcx,4)
	movl	%edx, 476(%rsp,%rcx,4)
	movq	%rcx, %rdi
	andq	%rsi, %rdi
	incq	%rcx
	cmpq	%rbx, %rdi
	jne	.LBB19_862
.LBB19_863:                             # %for.end3957
	testq	%r8, %r8
	je	.LBB19_876
# BB#864:
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movq	%rcx, 336(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	movq	%rax, %rsi
.LBB19_865:                             # %for.body3962.lr.ph.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_866 Depth 2
                                        #       Child Loop BB19_867 Depth 3
	movq	%rcx, %r10
.LBB19_866:                             # %for.body3962.lr.ph
                                        #   Parent Loop BB19_865 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_867 Depth 3
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	imulq	$104, %rsi, %rcx
	decq	%rsi
	leaq	rld-12(%rcx), %rdi
	.align	16, 0x90
.LBB19_867:                             # %for.body3962
                                        #   Parent Loop BB19_865 Depth=1
                                        #     Parent Loop BB19_866 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdi), %ecx
	cmpq	$3, %rcx
	je	.LBB19_893
# BB#868:                               # %for.body3962
                                        #   in Loop: Header=BB19_867 Depth=3
	cmpq	$5, %rcx
	jne	.LBB19_869
# BB#897:                               # %sw.bb3988
                                        #   in Loop: Header=BB19_867 Depth=3
	movslq	-20(%rdi), %rcx
	movslq	352(%rsp,%rcx,4), %rdx
	cmpq	$-1, %rdx
	leaq	1(%rdx), %rdx
	movl	%edx, 352(%rsp,%rcx,4)
	jl	.LBB19_894
	jmp	.LBB19_898
.LBB19_893:                             # %sw.bb3972
                                        #   in Loop: Header=BB19_867 Depth=3
	movslq	-20(%rdi), %rcx
	movslq	480(%rsp,%rcx,4), %rbx
	cmpq	$-2, %rbx
	leaq	1(%rbx), %rbx
	movl	%ebx, 480(%rsp,%rcx,4)
	jg	.LBB19_896
.LBB19_894:                             # %for.cond3959.backedge
                                        #   in Loop: Header=BB19_867 Depth=3
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rsi
	addq	$-104, %rdi
	testq	%rdx, %rdx
	jg	.LBB19_867
	jmp	.LBB19_895
.LBB19_869:                             # %for.body3962
                                        #   in Loop: Header=BB19_867 Depth=3
	cmpq	$7, %rcx
	jne	.LBB19_894
	jmp	.LBB19_870
.LBB19_898:                             # %if.then3997
                                        #   in Loop: Header=BB19_866 Depth=2
	movl	%esi, 352(%rsp,%rcx,4)
	jmp	.LBB19_899
.LBB19_896:                             # %if.then3981
                                        #   in Loop: Header=BB19_866 Depth=2
	movl	%esi, 480(%rsp,%rcx,4)
.LBB19_899:                             # %for.cond3959.outer5682.backedge
                                        #   in Loop: Header=BB19_866 Depth=2
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %r10
	testq	%rdx, %rdx
	jg	.LBB19_866
	jmp	.LBB19_900
.LBB19_870:                             # %sw.bb3966
                                        #   in Loop: Header=BB19_865 Depth=1
	movq	%r9, %rdx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	336(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%r9
	cmpq	$-2, %rbx
	movq	%rsi, 336(%rsp)         # 8-byte Spill
	jg	.LBB19_872
# BB#871:                               # %sw.bb3966
                                        #   in Loop: Header=BB19_865 Depth=1
	movq	%r9, 336(%rsp)          # 8-byte Spill
.LBB19_872:                             # %sw.bb3966
                                        #   in Loop: Header=BB19_865 Depth=1
	movq	$1, %rcx
	jg	.LBB19_874
# BB#873:                               # %sw.bb3966
                                        #   in Loop: Header=BB19_865 Depth=1
	movq	%r10, %rcx
.LBB19_874:                             # %sw.bb3966
                                        #   in Loop: Header=BB19_865 Depth=1
	testq	%rdi, %rdi
	movq	%rdx, %r9
	jg	.LBB19_865
	jmp	.LBB19_875
.LBB19_895:
	movq	%r10, %rcx
.LBB19_875:                             # %for.end4008
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB19_876
.LBB19_900:                             # %for.cond4011.preheader
	testq	%r8, %r8
	je	.LBB19_876
# BB#901:
	xorq	%r11, %r11
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB19_902:                             # %for.body4014
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_912 Depth 2
	movq	%r11, 344(%rsp)         # 8-byte Spill
	imulq	$104, %r11, %rdx
	movl	rld+92(%rdx), %ecx
	movq	$7, %rbx
	cmpq	$8, %rcx
	movq	336(%rsp), %r10         # 8-byte Reload
	je	.LBB19_907
# BB#903:                               # %for.body4014
                                        #   in Loop: Header=BB19_902 Depth=1
	cmpq	$6, %rcx
	jne	.LBB19_904
# BB#906:                               # %sw.bb4027
                                        #   in Loop: Header=BB19_902 Depth=1
	movslq	rld+72(%rdx), %rcx
	movl	352(%rsp,%rcx,4), %r10d
	movq	$5, %rbx
	jmp	.LBB19_907
.LBB19_904:                             # %for.body4014
                                        #   in Loop: Header=BB19_902 Depth=1
	cmpq	$4, %rcx
	jne	.LBB19_918
# BB#905:                               # %sw.bb4021
                                        #   in Loop: Header=BB19_902 Depth=1
	movslq	rld+72(%rdx), %rcx
	movl	480(%rsp,%rcx,4), %r10d
	movq	$3, %rbx
.LBB19_907:                             # %sw.epilog4034
                                        #   in Loop: Header=BB19_902 Depth=1
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB19_918
# BB#908:                               # %if.else4038
                                        #   in Loop: Header=BB19_902 Depth=1
	leaq	rld+92(%rdx), %r14
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB19_910
# BB#909:                               # %if.then4041
                                        #   in Loop: Header=BB19_902 Depth=1
	movl	%ebx, (%r14)
	jmp	.LBB19_918
.LBB19_910:                             # %if.else4045
                                        #   in Loop: Header=BB19_902 Depth=1
	movq	%r14, 312(%rsp)         # 8-byte Spill
	leaq	-1(%rax), %r15
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB19_918
# BB#911:                               # %for.body4050.lr.ph
                                        #   in Loop: Header=BB19_902 Depth=1
	leaq	rld+96(%rdx), %rsi
	movq	%rsi, 328(%rsp)         # 8-byte Spill
	leaq	rld(%rdx), %rcx
	movq	%rcx, 320(%rsp)         # 8-byte Spill
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	$26, %rax, %rax
	leaq	rld+92(,%rax,4), %r12
	.align	16, 0x90
.LBB19_912:                             # %for.body4050
                                        #   Parent Loop BB19_902 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r12), %eax
	movq	%rbx, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_917
# BB#913:                               # %land.lhs.true4056
                                        #   in Loop: Header=BB19_912 Depth=2
	movq	%r10, %r14
	movq	%r8, %r13
	movzbl	(%rsi), %eax
	testq	$4, %rax
	jne	.LBB19_914
# BB#916:                               # %cond.false4071
                                        #   in Loop: Header=BB19_912 Depth=2
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	-92(%r12), %rsi
	callq	reg_mentioned_p
	movq	328(%rsp), %rsi         # 8-byte Reload
	movq	344(%rsp), %r11         # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	movq	%r13, %r8
	testq	%r8, %rax
	movq	%r14, %r10
	je	.LBB19_917
	jmp	.LBB19_915
.LBB19_914:                             # %cond.true4065
                                        #   in Loop: Header=BB19_912 Depth=2
	movl	-16(%r12), %eax
	movq	%r11, %rcx
	movq	%r13, %r8
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	movq	%r14, %r10
	je	.LBB19_915
.LBB19_917:                             # %for.cond4047.backedge
                                        #   in Loop: Header=BB19_912 Depth=2
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$-104, %r12
	cmpq	%rdx, %rax
	jg	.LBB19_912
	jmp	.LBB19_918
.LBB19_915:                             # %if.then4080
                                        #   in Loop: Header=BB19_902 Depth=1
	movq	312(%rsp), %rax         # 8-byte Reload
	movl	%ebx, (%rax)
.LBB19_918:                             # %for.inc4090
                                        #   in Loop: Header=BB19_902 Depth=1
	incq	%r11
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB19_902
.LBB19_876:                             # %for.cond4094.preheader
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB19_955
# BB#877:
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_878:                             # %for.body4097
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_884 Depth 2
                                        #       Child Loop BB19_938 Depth 3
	imulq	$104, %rbx, %rdx
	cmpq	$0, rld(%rdx)
	je	.LBB19_943
# BB#879:                               # %land.lhs.true4103
                                        #   in Loop: Header=BB19_878 Depth=1
	cmpq	$0, rld+8(%rdx)
	jne	.LBB19_943
# BB#880:                               # %land.lhs.true4108
                                        #   in Loop: Header=BB19_878 Depth=1
	movl	rld+92(%rdx), %ecx
	cmpq	$10, %rcx
	ja	.LBB19_943
# BB#881:                               # %land.lhs.true4108
                                        #   in Loop: Header=BB19_878 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$1408, %rsi             # imm = 0x580
	je	.LBB19_943
# BB#882:                               # %for.cond4127.preheader
                                        #   in Loop: Header=BB19_878 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB19_943
# BB#883:                               # %for.body4130.lr.ph
                                        #   in Loop: Header=BB19_878 Depth=1
	leaq	rld(%rdx), %r8
	movq	%r8, 336(%rsp)          # 8-byte Spill
	leaq	rld+92(%rdx), %r10
	movq	%r10, 328(%rsp)         # 8-byte Spill
	leaq	rld+16(%rdx), %r11
	movq	%r11, 320(%rsp)         # 8-byte Spill
	leaq	rld+96(%rdx), %rax
	movq	%rax, 344(%rsp)         # 8-byte Spill
	leaq	rld+64(%rdx), %rax
	movq	%rax, 312(%rsp)         # 8-byte Spill
	leaq	rld+72(%rdx), %rax
	movq	%rax, 296(%rsp)         # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB19_884:                             # %for.body4130
                                        #   Parent Loop BB19_878 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_938 Depth 3
	movq	%r13, %r14
	andq	%r15, %r14
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r14, %rax
	je	.LBB19_942
# BB#885:                               # %land.lhs.true4133
                                        #   in Loop: Header=BB19_884 Depth=2
	imulq	$104, %r13, %r12
	movq	rld(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB19_942
# BB#886:                               # %land.lhs.true4139
                                        #   in Loop: Header=BB19_884 Depth=2
	cmpq	$0, rld+8(%r12)
	jne	.LBB19_942
# BB#887:                               # %land.lhs.true4145
                                        #   in Loop: Header=BB19_884 Depth=2
	movl	rld+92(%r12), %eax
	movl	(%r10), %ecx
	cmpq	%rcx, %rax
	jne	.LBB19_942
# BB#888:                               # %land.lhs.true4154
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	(%r8), %rdi
	cmpq	%rsi, %rdi
	je	.LBB19_931
# BB#889:                               # %lor.lhs.false4163
                                        #   in Loop: Header=BB19_884 Depth=2
	testq	%rdi, %rdi
	je	.LBB19_942
# BB#890:                               # %land.lhs.true4169
                                        #   in Loop: Header=BB19_884 Depth=2
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB19_929
# BB#891:                               # %cond.true4177
                                        #   in Loop: Header=BB19_884 Depth=2
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	jne	.LBB19_942
# BB#892:                               # %land.lhs.true4185
                                        #   in Loop: Header=BB19_884 Depth=2
	movl	8(%rdi), %eax
	movl	8(%rsi), %ecx
	cmpq	%rcx, %rax
	je	.LBB19_931
	jmp	.LBB19_942
.LBB19_929:                             # %cond.false4200
                                        #   in Loop: Header=BB19_884 Depth=2
	callq	rtx_equal_p
	movq	320(%rsp), %r11         # 8-byte Reload
	movq	328(%rsp), %r10         # 8-byte Reload
	movq	336(%rsp), %r8          # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB19_942
# BB#930:                               # %land.lhs.true4209
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	(%r8), %rdi
	callq	side_effects_p
	movq	320(%rsp), %r11         # 8-byte Reload
	movq	328(%rsp), %r10         # 8-byte Reload
	movq	336(%rsp), %r8          # 8-byte Reload
	movq	208(%rsp), %r9          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB19_942
.LBB19_931:                             # %land.lhs.true4215
                                        #   in Loop: Header=BB19_884 Depth=2
	movl	(%r11), %eax
	movl	rld+16(%r12), %ecx
	cmpq	%rcx, %rax
	jne	.LBB19_942
# BB#932:                               # %land.lhs.true4223
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	344(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$2, %rax
	jne	.LBB19_942
# BB#933:                               # %land.lhs.true4231
                                        #   in Loop: Header=BB19_884 Depth=2
	movzbl	rld+96(%r12), %eax
	testq	$2, %rax
	jne	.LBB19_942
# BB#934:                               # %land.lhs.true4240
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	312(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	rld+64(%r12), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_942
# BB#935:                               # %if.then4248
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	296(%rsp), %rax         # 8-byte Reload
	movslq	(%rax), %rcx
	movslq	rld+72(%r12), %rax
	cmpq	%rax, %rcx
	jle	.LBB19_937
# BB#936:                               # %if.then4248
                                        #   in Loop: Header=BB19_884 Depth=2
	movq	%rax, %rcx
.LBB19_937:                             # %if.then4248
                                        #   in Loop: Header=BB19_884 Depth=2
	movslq	n_replacements(%rip), %rax
	movq	296(%rsp), %rdx         # 8-byte Reload
	movl	%ecx, (%rdx)
	testq	%rax, %rax
	movq	$1, %rcx
	movabsq	$replacements+16, %rdx
	jle	.LBB19_941
.LBB19_938:                             # %for.body.i
                                        #   Parent Loop BB19_878 Depth=1
                                        #     Parent Loop BB19_884 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rdx), %esi
	cmpq	%r14, %rsi
	jne	.LBB19_940
# BB#939:                               # %if.then.i
                                        #   in Loop: Header=BB19_938 Depth=3
	movl	%ebx, (%rdx)
.LBB19_940:                             # %for.inc.i
                                        #   in Loop: Header=BB19_938 Depth=3
	movq	%rcx, %rsi
	andq	%r15, %rsi
	movq	%rax, %rdi
	andq	%r15, %rdi
	incq	%rcx
	addq	$24, %rdx
	cmpq	%rdi, %rsi
	jne	.LBB19_938
.LBB19_941:                             # %transfer_replacements.exit
                                        #   in Loop: Header=BB19_884 Depth=2
	leaq	rld(%r12), %rax
	movq	$0, (%rax)
	.align	16, 0x90
.LBB19_942:                             # %for.inc4274
                                        #   in Loop: Header=BB19_884 Depth=2
	incq	%r13
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB19_884
	.align	16, 0x90
.LBB19_943:                             # %for.inc4278
                                        #   in Loop: Header=BB19_878 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB19_878
# BB#919:                               # %for.cond4281.preheader
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_955
# BB#920:
	movq	$1, %r14
	movabsq	$rld+32, %rbx
	movabsq	$0, %r12
	.align	16, 0x90
.LBB19_921:                             # %for.body4284
                                        # =>This Inner Loop Header: Depth=1
	movq	%r9, %r13
	movl	-12(%rbx), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movslq	-8(%rbx), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rax, %rax
	je	.LBB19_923
# BB#922:                               # %lor.lhs.false4289
                                        #   in Loop: Header=BB19_921 Depth=1
	movzbl	mode_size(%rsi), %ecx
	movzbl	mode_size(%rdx), %edx
	cmpq	%rdx, %rcx
	jbe	.LBB19_924
.LBB19_923:                             # %cond.true4303
                                        #   in Loop: Header=BB19_921 Depth=1
	movq	%rsi, %rax
.LBB19_924:                             # %cond.end4311
                                        #   in Loop: Header=BB19_921 Depth=1
	movl	%eax, -4(%rbx)
	movl	-16(%rbx), %edi
	movq	$12, %rsi
	callq	reg_classes_intersect_p
	movslq	-4(%rbx), %rcx
	testq	%r15, %rax
	jne	.LBB19_956
# BB#925:                               # %cond.true4321
                                        #   in Loop: Header=BB19_921 Depth=1
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	%r13, %r9
	je	.LBB19_959
# BB#926:                               # %lor.rhs4329
                                        #   in Loop: Header=BB19_921 Depth=1
	cmpq	$6, %rcx
	movq	%r12, %rax
	jne	.LBB19_928
# BB#927:                               # %lor.rhs4329
                                        #   in Loop: Header=BB19_921 Depth=1
	movabsq	$1, %rax
.LBB19_928:                             # %lor.rhs4329
                                        #   in Loop: Header=BB19_921 Depth=1
	incq	%rax
	jmp	.LBB19_959
.LBB19_956:                             # %cond.false4339
                                        #   in Loop: Header=BB19_921 Depth=1
	movq	%rcx, %rax
	andq	%r15, %rax
	cmpq	$18, %rax
	movq	$17, %rax
	movq	%r13, %r9
	je	.LBB19_958
# BB#957:                               # %cond.false4339
                                        #   in Loop: Header=BB19_921 Depth=1
	movq	%rcx, %rax
.LBB19_958:                             # %cond.false4339
                                        #   in Loop: Header=BB19_921 Depth=1
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB19_959:                             # %cond.end4363
                                        #   in Loop: Header=BB19_921 Depth=1
	movl	%eax, (%rbx)
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$104, %rbx
	cmpq	%rax, %rdx
	jl	.LBB19_921
# BB#944:                               # %for.cond4370.preheader
	testq	%rax, %rax
	jle	.LBB19_955
# BB#945:
	movq	$1, %r14
	movabsq	$rld, %rbx
.LBB19_946:                             # %for.body4373
                                        # =>This Inner Loop Header: Depth=1
	movl	92(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB19_954
# BB#947:                               # %land.lhs.true4379
                                        #   in Loop: Header=BB19_946 Depth=1
	movq	32(%r9), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	jne	.LBB19_954
# BB#948:                               # %land.lhs.true4387
                                        #   in Loop: Header=BB19_946 Depth=1
	movq	8(%rax), %r12
	movzbl	(%r12), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_954
# BB#949:                               # %land.lhs.true4398
                                        #   in Loop: Header=BB19_946 Depth=1
	movq	16(%rax), %rax
	movq	(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB19_954
# BB#950:                               # %if.then4410
                                        #   in Loop: Header=BB19_946 Depth=1
	movl	8(%r12), %ecx
	cmpq	$52, %rcx
	ja	.LBB19_954
# BB#951:                               # %land.lhs.true4425
                                        #   in Loop: Header=BB19_946 Depth=1
	movl	16(%rbx), %eax
	movq	reg_class_contents(,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB19_954
# BB#952:                               # %land.lhs.true4433
                                        #   in Loop: Header=BB19_946 Depth=1
	movl	28(%rbx), %esi
	movq	%rcx, %rdi
	movq	%r9, %r13
	callq	ix86_hard_regno_mode_ok
	movq	%r13, %r9
	testq	%r15, %rax
	je	.LBB19_954
# BB#953:                               # %if.then4439
                                        #   in Loop: Header=BB19_946 Depth=1
	movq	%r12, 64(%rbx)
.LBB19_954:                             # %for.inc4445
                                        #   in Loop: Header=BB19_946 Depth=1
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$104, %rbx
	cmpq	%rax, %rdx
	jl	.LBB19_946
.LBB19_955:
	movq	304(%rsp), %rcx         # 8-byte Reload
.LBB19_60:                              # %cleanup
	movq	%rcx, %rax
	addq	$3872, %rsp             # imm = 0xF20
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_620:                             # %if.then3111
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads, %rcx
	movq	$3790, %rsi             # imm = 0xECE
.LBB19_38:                              # %if.then135
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp276:
	.size	find_reloads, .Ltmp276-find_reloads
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_134
	.quad	.LBB19_134
	.quad	.LBB19_134
	.quad	.LBB19_133
	.quad	.LBB19_134
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_128
	.quad	.LBB19_133
	.quad	.LBB19_117
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_129
	.quad	.LBB19_134
	.quad	.LBB19_134
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_133
	.quad	.LBB19_134
.LJTI19_1:
	.quad	.LBB19_147
	.quad	.LBB19_310
	.quad	.LBB19_148
	.quad	.LBB19_310
	.quad	.LBB19_144
	.quad	.LBB19_247
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_388
	.quad	.LBB19_388
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_152
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_201
	.quad	.LBB19_388
	.quad	.LBB19_205
	.quad	.LBB19_146
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_248
	.quad	.LBB19_251
	.quad	.LBB19_254
	.quad	.LBB19_254
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_281
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_209
	.quad	.LBB19_310
	.quad	.LBB19_291
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_292
	.quad	.LBB19_310
	.quad	.LBB19_265
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_310
	.quad	.LBB19_187
	.quad	.LBB19_277
	.quad	.LBB19_222
	.quad	.LBB19_186
	.quad	.LBB19_310
	.quad	.LBB19_309
	.quad	.LBB19_262
.LJTI19_2:
	.quad	.LBB19_284
	.quad	.LBB19_285
	.quad	.LBB19_286
	.quad	.LBB19_287
	.quad	.LBB19_289
	.quad	.LBB19_290

	.text
	.align	16, 0x90
	.type	find_reloads_toplev,@function
find_reloads_toplev:                    # @find_reloads_toplev
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp283:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp284:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp285:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp286:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp287:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp288:
	.cfi_def_cfa_offset 144
.Ltmp289:
	.cfi_offset %rbx, -48
.Ltmp290:
	.cfi_offset %r12, -40
.Ltmp291:
	.cfi_offset %r13, -32
.Ltmp292:
	.cfi_offset %r14, -24
.Ltmp293:
	.cfi_offset %r15, -16
	movq	%rdi, %rax
	movq	%rax, 88(%rsp)
	movl	(%rax), %edi
	movq	144(%rsp), %r14
	movq	%r9, 40(%rsp)           # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rcx, %r10
	movq	%r10, 56(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	movq	%rsi, %r8
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$66, %rbx
	je	.LBB20_17
# BB#1:                                 # %entry
	movq	rtx_format(,%rbx,8), %rdi
	cmpq	$63, %rbx
	jne	.LBB20_2
# BB#21:                                # %land.lhs.true68
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB20_53
# BB#22:                                # %if.then75
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%r9, %r15
	movq	%r10, %r13
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movslq	8(%rcx), %r14
	movq	%rax, %rdi
	callq	subreg_lowpart_p
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	testq	%r11, %rax
	je	.LBB20_27
# BB#23:                                # %if.then75
	cmpq	$53, %r14
	jl	.LBB20_27
# BB#24:                                # %land.lhs.true88
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rax
	testq	%rax, %rax
	jns	.LBB20_27
# BB#25:                                # %land.lhs.true93
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r14,8), %rsi
	testq	%rsi, %rsi
	je	.LBB20_27
# BB#26:                                # %land.lhs.true98
	movq	88(%rsp), %rax
	movzbl	2(%rax), %edi
	callq	gen_lowpart_common
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	testq	%rax, %rax
	jne	.LBB20_20
.LBB20_27:                              # %if.end108
	movq	88(%rsp), %r8
	movl	target_flags(%rip), %esi
	movzbl	2(%r8), %eax
	movzbl	mode_bitsize(%rax,%rax), %eax
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$20, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	%r13, %r10
	movq	%r15, %r9
	jne	.LBB20_35
# BB#28:                                # %if.end108
	cmpq	$53, %r14
	jl	.LBB20_35
# BB#29:                                # %land.lhs.true121
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rax
	testq	%rax, %rax
	jns	.LBB20_35
# BB#30:                                # %land.lhs.true127
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r14,8), %rdi
	testq	%rdi, %rdi
	je	.LBB20_35
# BB#31:                                # %land.lhs.true132
	movl	16(%r8), %eax
	movabsq	$4286578688, %rcx       # imm = 0xFF800000
	andq	%rcx, %rsi
	movq	$23, %rcx
	shrq	%cl, %rsi
	addq	$4, %rsi
	andq	%r11, %rsi
	xorq	%rdx, %rdx
	divq	%rsi
	movq	8(%r8), %rcx
	movzbl	2(%rcx), %ecx
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	movq	%r10, %r13
	movq	%r9, %r15
	callq	operand_subword
	movq	%r15, %r9
	movq	%r13, %r10
	testq	%rax, %rax
	je	.LBB20_35
# BB#32:                                # %if.then150
	movq	88(%rsp), %rcx
	movzbl	2(%rcx), %edi
	movq	%rax, %rsi
	callq	gen_lowpart_common
	testq	%rax, %rax
	jne	.LBB20_20
# BB#33:                                # %if.then156
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_toplev, %rcx
	movq	$4434, %rsi             # imm = 0x1152
	jmp	.LBB20_34
.LBB20_17:                              # %if.then52
	movq	%rax, 80(%rsp)
	movq	$16, %rcx
	movq	8(%rax), %rdx
	shrq	%cl, %rdi
	andq	$255, %rdi
	addq	$8, %rax
	movq	%r9, 8(%rsp)
	leaq	80(%rsp), %rsi
	movq	%r10, (%rsp)
	movq	%rax, %rcx
	movq	%r12, %r9
	callq	find_reloads_address
	testq	%r14, %r14
	je	.LBB20_19
# BB#18:                                # %if.then64
	movl	%eax, (%r14)
.LBB20_19:                              # %if.end65
	movq	80(%rsp), %rax
	jmp	.LBB20_20
.LBB20_2:                               # %entry
	cmpq	$61, %rbx
	jne	.LBB20_53
# BB#3:                                 # %if.then
	movslq	8(%rax), %rbx
	movq	reg_equiv_constant(%rip), %rcx
	movq	(%rcx,%rbx,8), %rcx
	testq	%rcx, %rcx
	movq	144(%rsp), %r15
	je	.LBB20_7
# BB#4:                                 # %if.then
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	64(%rsp), %rsi          # 8-byte Reload
	testq	%rdx, %rsi
	jne	.LBB20_7
# BB#5:                                 # %if.then5
	movq	%rcx, 88(%rsp)
	jmp	.LBB20_6
.LBB20_35:                              # %if.end158
	cmpq	$53, %r14
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %rdi          # 8-byte Reload
	jl	.LBB20_53
# BB#36:                                # %land.lhs.true161
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rcx
	testq	%rcx, %rcx
	jns	.LBB20_47
# BB#37:                                # %land.lhs.true167
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r14,8), %rax
	testq	%rax, %rax
	je	.LBB20_44
# BB#38:                                # %land.lhs.true172
	movq	%rdi, %r11
	movq	%r9, %rdi
	movq	%r10, %r9
	movzbl	(%rax), %edx
	cmpq	$54, %rdx
	jne	.LBB20_43
# BB#39:                                # %land.lhs.true179
	movq	88(%rsp), %rdx
	movq	8(%rdx), %rsi
	movzbl	2(%rdx), %r10d
	movzbl	2(%rsi), %esi
	movzbl	mode_size(%r10), %r10d
	movzbl	mode_size(%rsi), %esi
	cmpq	%rsi, %r10
	jae	.LBB20_43
# BB#40:                                # %if.then197
	movl	16(%rdx), %edx
	movq	$35, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	movq	8(%rax), %rsi
	sarq	%cl, %rdi
	cmpq	$63, %rdi
	movq	$63, %rcx
	jg	.LBB20_42
# BB#41:                                # %if.then197
	andq	$536870911, %rdx        # imm = 0x1FFFFFFF
	shlq	$3, %rdx
	movq	%rdx, %rcx
.LBB20_42:                              # %if.then197
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	jmp	.LBB20_20
.LBB20_7:                               # %if.else
	movq	%r9, %r13
	movq	reg_equiv_memory_loc(%rip), %rcx
	cmpq	$0, (%rcx,%rbx,8)
	je	.LBB20_6
# BB#8:                                 # %land.lhs.true11
	movq	reg_equiv_address(%rip), %rcx
	cmpq	$0, (%rcx,%rbx,8)
	jne	.LBB20_10
# BB#9:                                 # %land.lhs.true11
	movl	num_not_at_initial_offset(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB20_6
.LBB20_10:                              # %if.then16
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%r8, %r14
	callq	make_memloc
	movq	%r14, %r8
	movq	reg_equiv_address(%rip), %rcx
	cmpq	$0, (%rcx,%rbx,8)
	movq	%rax, %r14
	jne	.LBB20_12
# BB#11:                                # %lor.lhs.false20
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
	movq	%r14, %rdi
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB20_6
.LBB20_12:                              # %if.then25
	movl	replace_reloads(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_15
# BB#13:                                # %land.lhs.true27
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	recog_data(,%rax,8), %rax
	movq	88(%rsp), %rdx
	cmpq	%rdx, %rax
	je	.LBB20_15
# BB#14:                                # %if.then31
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r8, %rbx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_insn_before
	movq	%rbx, %r8
	movq	$2, %rcx
	movb	%cl, 2(%rax)
.LBB20_15:                              # %if.end
	movq	%r14, 88(%rsp)
	movq	8(%r14), %rdx
	movzbl	2(%r14), %edi
	leaq	8(%r14), %rcx
	movq	%r13, 8(%rsp)
	leaq	88(%rsp), %rsi
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r12, %r9
	callq	find_reloads_address
	testq	%r15, %r15
	je	.LBB20_6
# BB#16:                                # %if.then45
	movl	%eax, (%r15)
	movq	88(%rsp), %rax
	jmp	.LBB20_20
.LBB20_43:                              # %land.lhs.true213
	testq	%rcx, %rcx
	movq	%r9, %r10
	movq	%rdi, %r9
	movq	%r11, %rdi
	jns	.LBB20_47
.LBB20_44:                              # %land.lhs.true219
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%r14,8), %rax
	testq	%rax, %rax
	je	.LBB20_47
# BB#45:                                # %land.lhs.true224
	movzbl	2(%rax), %eax
	testq	%rax, %rax
	je	.LBB20_46
.LBB20_47:                              # %land.lhs.true236
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%r14,8)
	jne	.LBB20_52
# BB#48:                                # %lor.lhs.false241
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r14,8), %rax
	testq	%rax, %rax
	je	.LBB20_53
# BB#49:                                # %land.lhs.true246
	movq	88(%rsp), %rcx
	movq	8(%rax), %rsi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movzbl	2(%rcx), %edi
	movq	$1, %rdx
	movq	%r8, %r15
	movq	%r10, 56(%rsp)          # 8-byte Spill
	movq	%r9, %r13
	callq	legitimate_address_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %r9
	movq	56(%rsp), %r10          # 8-byte Reload
	movq	%r15, %r8
	testq	%rcx, %rax
	je	.LBB20_52
# BB#50:                                # %lor.lhs.false257
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r14,8), %rdi
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%r10, %r15
	movq	%r9, %r13
	movq	%rcx, %r14
	callq	offsettable_memref_p
	movq	%r14, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %r9
	movq	%r15, %r10
	movq	72(%rsp), %r8           # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB20_52
# BB#51:                                # %lor.lhs.false257
	movl	num_not_at_initial_offset(%rip), %eax
	testq	%rcx, %rax
	je	.LBB20_53
.LBB20_52:                              # %if.then264
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi
	movq	$1, %rsi
	movq	%r8, %rdx
	movq	%r12, %rcx
	movq	%r8, %r14
	movq	%r10, %r8
	movq	%r10, %r15
	movq	%r9, %r13
	callq	find_reloads_subreg_address
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %r9
	movq	%r15, %r10
	movq	%r14, %r8
	movq	%rax, 88(%rsp)
.LBB20_53:                              # %if.end268
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movzbl	rtx_length(%rbx), %r13d
	decq	%rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB20_54
	.align	16, 0x90
.LBB20_67:                              # %if.end335
                                        #   in Loop: Header=BB20_54 Depth=1
	addq	%r12, %rbx
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%r11,%r13,8), %rcx
	movq	%rax, (%rcx,%r15,8)
	movq	%rbx, %r13
.LBB20_54:                              # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_55 Depth 2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rdi,%rax), %rdx
	xorq	%r12, %r12
	.align	16, 0x90
.LBB20_55:                              # %for.cond
                                        #   Parent Loop BB20_54 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %r15
	leaq	(%rax,%r15), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB20_6
# BB#56:                                # %for.body
                                        #   in Loop: Header=BB20_55 Depth=2
	movzbl	(%rdx,%r15), %ecx
	leaq	-1(%r15), %r12
	cmpq	$101, %rcx
	jne	.LBB20_55
# BB#57:                                # %if.then279
                                        #   in Loop: Header=BB20_54 Depth=1
	movq	88(%rsp), %rax
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	8(%rax,%rbx,8), %rax
	movq	-8(%rax,%r15,8), %rdi
	movq	144(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r8, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %rcx
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%r9, %r14
	callq	find_reloads_toplev
	movq	88(%rsp), %r11
	leaq	8(%r11,%rbx,8), %rcx
	movq	-8(%rcx,%r15,8), %rcx
	cmpq	%rcx, %rax
	jne	.LBB20_59
# BB#58:                                #   in Loop: Header=BB20_54 Depth=1
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r10          # 8-byte Reload
	movq	%r14, %r9
	movq	48(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB20_67
	.align	16, 0x90
.LBB20_59:                              # %land.lhs.true291
                                        #   in Loop: Header=BB20_54 Depth=1
	movzbl	(%rax), %edx
	cmpq	$133, %rdx
	jg	.LBB20_62
# BB#60:                                # %land.lhs.true291
                                        #   in Loop: Header=BB20_54 Depth=1
	leaq	-54(%rdx), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$14, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r10          # 8-byte Reload
	movq	%r14, %r9
	movq	48(%rsp), %rdi          # 8-byte Reload
	ja	.LBB20_63
# BB#61:                                # %land.lhs.true291
                                        #   in Loop: Header=BB20_54 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$24599, %rsi            # imm = 0x6017
	jne	.LBB20_67
	jmp	.LBB20_63
.LBB20_62:                              # %land.lhs.true291
                                        #   in Loop: Header=BB20_54 Depth=1
	cmpq	$134, %rdx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	56(%rsp), %r10          # 8-byte Reload
	movq	%r14, %r9
	movq	48(%rsp), %rdi          # 8-byte Reload
	je	.LBB20_67
.LBB20_63:                              # %lor.lhs.false326
                                        #   in Loop: Header=BB20_54 Depth=1
	movq	%rax, %r14
	cmpq	$140, %rdx
	je	.LBB20_66
# BB#64:                                # %lor.lhs.false326
                                        #   in Loop: Header=BB20_54 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB20_66
# BB#65:                                # %if.then333
                                        #   in Loop: Header=BB20_54 Depth=1
	movq	%r11, %rdi
	callq	shallow_copy_rtx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %r9           # 8-byte Reload
	movq	56(%rsp), %r10          # 8-byte Reload
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	%rax, %r11
	movq	%r11, 88(%rsp)
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB20_66:                              # %if.end335
                                        #   in Loop: Header=BB20_54 Depth=1
	movq	%r14, %rax
	jmp	.LBB20_67
.LBB20_6:                               # %for.end
	movq	88(%rsp), %rax
.LBB20_20:                              # %return
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_46:                              # %if.then232
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_toplev, %rcx
	movq	$4464, %rsi             # imm = 0x1170
.LBB20_34:                              # %if.then156
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp294:
	.size	find_reloads_toplev, .Ltmp294-find_reloads_toplev
	.cfi_endproc

	.globl	form_sum
	.align	16, 0x90
	.type	form_sum,@function
form_sum:                               # @form_sum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp300:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp301:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp302:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp303:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp304:
	.cfi_def_cfa_offset 48
.Ltmp305:
	.cfi_offset %rbx, -40
.Ltmp306:
	.cfi_offset %r12, -32
.Ltmp307:
	.cfi_offset %r14, -24
.Ltmp308:
	.cfi_offset %r15, -16
	movq	%rsi, %rdx
	movq	%rdi, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$414331165718085632, %r15 # imm = 0x5C0000000000000
	jmp	.LBB21_1
	.align	16, 0x90
.LBB21_34:                              # %if.then180
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%r12), %rsi
	movq	%r8, %rdi
	callq	form_sum
	movq	16(%r12), %rdx
	movq	%rax, %r12
.LBB21_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r12), %edi
	movq	$16, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	jne	.LBB21_4
# BB#2:                                 # %if.end
                                        #   in Loop: Header=BB21_1 Depth=1
	movzbl	2(%rdx), %esi
	testq	%rsi, %rsi
	jne	.LBB21_4
# BB#3:                                 # %if.then5
                                        #   in Loop: Header=BB21_1 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
.LBB21_4:                               # %if.end6
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB21_5
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB21_1 Depth=1
	movl	(%rdx), %ebx
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	je	.LBB21_8
# BB#9:                                 # %if.else19
                                        #   in Loop: Header=BB21_1 Depth=1
	leaq	-67(%rcx), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jb	.LBB21_16
# BB#10:                                # %lor.lhs.false26
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$133, %rcx
	jg	.LBB21_14
# BB#11:                                # %lor.lhs.false26
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$58, %rcx
	ja	.LBB21_12
# BB#13:                                # %lor.lhs.false26
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	jne	.LBB21_16
	jmp	.LBB21_12
	.align	16, 0x90
.LBB21_14:                              # %lor.lhs.false26
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB21_16
# BB#15:                                # %lor.lhs.false26
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$140, %rcx
	jne	.LBB21_12
	.align	16, 0x90
.LBB21_16:                              # %if.then50
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	%rdx, %r8
	jmp	.LBB21_17
	.align	16, 0x90
.LBB21_12:                              #   in Loop: Header=BB21_1 Depth=1
	movq	%rdi, %rbx
	movq	%r12, %r8
	movq	%rdx, %r12
.LBB21_17:                              # %if.end53
                                        #   in Loop: Header=BB21_1 Depth=1
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$75, %rbx
	jne	.LBB21_21
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	16(%r8), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$133, %rcx
	jg	.LBB21_29
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB21_1 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_21
# BB#20:                                # %land.lhs.true
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB21_31
	jmp	.LBB21_21
	.align	16, 0x90
.LBB21_29:                              # %land.lhs.true
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB21_31
# BB#30:                                # %land.lhs.true
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$140, %rcx
	jne	.LBB21_21
.LBB21_31:                              # %if.then111
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	8(%r8), %rbx
	movq	%r12, %rsi
	callq	form_sum
	movq	%rax, %rdx
	movq	%rbx, %r12
	jmp	.LBB21_1
	.align	16, 0x90
.LBB21_21:                              # %if.end120
                                        #   in Loop: Header=BB21_1 Depth=1
	movzbl	(%r12), %edx
	cmpq	$75, %rdx
	jne	.LBB21_25
# BB#22:                                # %land.lhs.true124
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	16(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$133, %rcx
	jg	.LBB21_32
# BB#23:                                # %land.lhs.true124
                                        #   in Loop: Header=BB21_1 Depth=1
	addq	$-54, %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_25
# BB#24:                                # %land.lhs.true124
                                        #   in Loop: Header=BB21_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB21_34
	jmp	.LBB21_25
	.align	16, 0x90
.LBB21_32:                              # %land.lhs.true124
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$134, %rcx
	je	.LBB21_34
# BB#33:                                # %land.lhs.true124
                                        #   in Loop: Header=BB21_1 Depth=1
	cmpq	$140, %rcx
	je	.LBB21_34
.LBB21_25:                              # %if.end189
	cmpq	$133, %rbx
	jg	.LBB21_35
# BB#26:                                # %if.end189
	leaq	-54(%rbx), %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_28
# BB#27:                                # %if.end189
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB21_37
	jmp	.LBB21_28
.LBB21_35:                              # %if.end189
	cmpq	$134, %rbx
	je	.LBB21_37
# BB#36:                                # %if.end189
	cmpq	$140, %rbx
	jne	.LBB21_28
.LBB21_37:                              # %land.lhs.true221
	cmpq	$133, %rdx
	jg	.LBB21_40
# BB#38:                                # %land.lhs.true221
	leaq	-54(%rdx), %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB21_28
# BB#39:                                # %land.lhs.true221
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB21_42
	jmp	.LBB21_28
.LBB21_40:                              # %land.lhs.true221
	cmpq	$134, %rdx
	je	.LBB21_42
# BB#41:                                # %land.lhs.true221
	cmpq	$140, %rdx
	jne	.LBB21_28
.LBB21_42:                              # %if.then253
	cmpq	$58, %rbx
	jne	.LBB21_44
# BB#43:                                # %if.then257
	movq	8(%r8), %r8
.LBB21_44:                              # %if.end261
	cmpq	$58, %rdx
	jne	.LBB21_46
# BB#45:                                # %if.then265
	movq	8(%r12), %r12
.LBB21_46:                              # %if.end269
	movq	$75, %rdi
	movq	%r8, %rdx
	movq	%r12, %rcx
	callq	gen_rtx_fmt_ee
	movq	$58, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_e  # TAILCALL
.LBB21_28:                              # %if.end272
	movq	$75, %rdi
	movq	%r8, %rdx
	movq	%r12, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB21_5:                               # %if.then10
	movq	8(%r12), %rsi
	movq	%rdx, %rdi
	jmp	.LBB21_6
.LBB21_8:                               # %if.then14
	movq	8(%rdx), %rsi
	movq	%r12, %rdi
.LBB21_6:                               # %if.then10
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	plus_constant_wide  # TAILCALL
.Ltmp309:
	.size	form_sum, .Ltmp309-form_sum
	.cfi_endproc

	.globl	subst_reloads
	.align	16, 0x90
	.type	subst_reloads,@function
subst_reloads:                          # @subst_reloads
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp315:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp316:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp317:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp318:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp319:
	.cfi_def_cfa_offset 48
.Ltmp320:
	.cfi_offset %rbx, -40
.Ltmp321:
	.cfi_offset %r12, -32
.Ltmp322:
	.cfi_offset %r14, -24
.Ltmp323:
	.cfi_offset %r15, -16
	movslq	n_replacements(%rip), %rax
	movq	%rdi, %r14
	testq	%rax, %rax
	jle	.LBB22_18
# BB#1:                                 # %for.body.lr.ph
	movq	$1, %r12
	movabsq	$replacements+20, %rbx
	jmp	.LBB22_2
.LBB22_16:                              # %if.else
                                        #   in Loop: Header=BB22_2 Depth=1
	movl	16(%rcx), %eax
	movl	16(%r15), %edi
	addq	%rax, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movzbl	mode_size(%rdx), %r8d
	sarq	%cl, %rax
	cqto
	idivq	%r8
	movq	-20(%rbx), %rax
	movq	%rsi, (%rax)
	movq	-12(%rbx), %rax
	movq	(%rax), %rax
	subq	%rdx, %rdi
	movl	%edi, 16(%rax)
	jmp	.LBB22_17
	.align	16, 0x90
.LBB22_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	-4(%rbx), %rax
	imulq	$104, %rax, %rax
	movq	rld+64(%rax), %r15
	testq	%r15, %r15
	jne	.LBB22_3
# BB#14:                                # %if.else82
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	rld+96(%rax), %eax
	testq	$1, %rax
	jne	.LBB22_17
	jmp	.LBB22_15
	.align	16, 0x90
.LBB22_3:                               # %if.then
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	-20(%rbx), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$67, %rcx
	jne	.LBB22_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	(%r14), %ecx
	cmpq	$33, %rcx
	jne	.LBB22_6
# BB#5:                                 # %if.then7
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	8(%rax), %rdx
	movq	56(%r14), %rcx
	movq	$4, %rdi
	movq	$13, %rsi
	callq	gen_rtx_fmt_ue
	movq	%rax, 56(%r14)
.LBB22_6:                               # %if.end
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	2(%r15), %eax
	movl	(%rbx), %edi
	cmpq	%rdi, %rax
	je	.LBB22_9
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB22_2 Depth=1
	testq	%rdi, %rdi
	je	.LBB22_9
# BB#8:                                 # %if.then22
                                        #   in Loop: Header=BB22_2 Depth=1
	movl	8(%r15), %esi
	callq	gen_rtx_REG
	movq	%rax, %r15
.LBB22_9:                               # %if.end27
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	-12(%rbx), %rax
	testq	%rax, %rax
	je	.LBB22_13
# BB#10:                                # %land.lhs.true29
                                        #   in Loop: Header=BB22_2 Depth=1
	movzbl	(%r15), %ecx
	cmpq	$63, %rcx
	jne	.LBB22_13
# BB#11:                                # %if.then33
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	(%rax), %rcx
	movq	8(%r15), %rsi
	movzbl	2(%rcx), %edx
	movzbl	2(%rsi), %edi
	cmpq	%rdi, %rdx
	jne	.LBB22_16
# BB#12:                                # %if.then45
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	%rsi, (%rax)
	jmp	.LBB22_17
	.align	16, 0x90
.LBB22_13:                              # %if.else79
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	-20(%rbx), %rax
	movq	%r15, (%rax)
.LBB22_17:                              # %for.inc
                                        #   in Loop: Header=BB22_2 Depth=1
	movslq	n_replacements(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$24, %rbx
	cmpq	%rax, %rdx
	jl	.LBB22_2
.LBB22_18:                              # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB22_15:                              # %if.then89
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.subst_reloads, %rcx
	movq	$5921, %rsi             # imm = 0x1721
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp324:
	.size	subst_reloads, .Ltmp324-subst_reloads
	.cfi_endproc

	.globl	copy_replacements
	.align	16, 0x90
	.type	copy_replacements,@function
copy_replacements:                      # @copy_replacements
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp326:
	.cfi_def_cfa_offset 32
	movq	%rdi, 16(%rsp)
	movq	%rsi, 8(%rsp)
	movzbl	(%rdi), %eax
	cmpq	$63, %rax
	je	.LBB23_2
# BB#1:                                 # %if.end
	movl	n_replacements(%rip), %edx
	leaq	16(%rsp), %rdi
	leaq	8(%rsp), %rsi
	callq	copy_replacements_1
	addq	$24, %rsp
	retq
.LBB23_2:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.copy_replacements, %rcx
	movq	$5935, %rsi             # imm = 0x172F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp327:
	.size	copy_replacements, .Ltmp327-copy_replacements
	.cfi_endproc

	.align	16, 0x90
	.type	copy_replacements_1,@function
copy_replacements_1:                    # @copy_replacements_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp334:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp335:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp336:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp337:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp338:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp339:
	.cfi_def_cfa_offset 80
.Ltmp340:
	.cfi_offset %rbx, -48
.Ltmp341:
	.cfi_offset %r12, -40
.Ltmp342:
	.cfi_offset %r13, -32
.Ltmp343:
	.cfi_offset %r14, -24
.Ltmp344:
	.cfi_offset %r15, -16
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB24_7
# BB#1:
	movq	$1, %rax
	movabsq	$replacements+20, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB24_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-12(%rcx), %rbx
	cmpq	%rdi, %rbx
	jne	.LBB24_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB24_2 Depth=1
	movslq	n_replacements(%rip), %r10
	movq	-20(%rcx), %r9
	leaq	(%r10,%r10,2), %rdx
	movq	%r9, replacements(,%rdx,8)
	movq	%rsi, replacements+8(,%rdx,8)
	movl	-4(%rcx), %ebx
	movl	%ebx, replacements+16(,%rdx,8)
	movl	(%rcx), %r9d
	leaq	1(%r10), %rbx
	movl	%ebx, n_replacements(%rip)
	movl	%r9d, replacements+20(,%rdx,8)
	jmp	.LBB24_6
	.align	16, 0x90
.LBB24_4:                               # %if.else
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	-20(%rcx), %rdx
	cmpq	%rdi, %rdx
	jne	.LBB24_6
# BB#5:                                 # %if.then18
                                        #   in Loop: Header=BB24_2 Depth=1
	movslq	n_replacements(%rip), %r9
	leaq	(%r9,%r9,2), %rbx
	movq	%rsi, replacements(,%rbx,8)
	movq	$0, replacements+8(,%rbx,8)
	movl	-4(%rcx), %edx
	movl	%edx, replacements+16(,%rbx,8)
	movl	(%rcx), %r10d
	leaq	1(%r9), %rdx
	movl	%edx, n_replacements(%rip)
	movl	%r10d, replacements+20(,%rbx,8)
.LBB24_6:                               # %for.inc
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	%rax, %rdx
	andq	%r8, %rdx
	movq	24(%rsp), %rbx          # 8-byte Reload
	andq	%r8, %rbx
	incq	%rax
	addq	$24, %rcx
	cmpq	%rbx, %rdx
	jne	.LBB24_2
.LBB24_7:                               # %for.end
	movq	(%rdi), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_length(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB24_12
# BB#8:                                 # %for.body41.lr.ph
	movq	(%rsi), %rbx
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB24_9:                               # %for.body41
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_16 Depth 2
	movq	%rdx, %r15
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%r15), %eax
	leaq	-1(%r15), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB24_10
# BB#14:                                # %if.then60
                                        #   in Loop: Header=BB24_9 Depth=1
	movq	(%r12,%r15,8), %rdi
	movslq	(%rdi), %r13
	testq	%r13, %r13
	jle	.LBB24_11
# BB#15:                                # %for.body67.lr.ph
                                        #   in Loop: Header=BB24_9 Depth=1
	movq	$3, %rcx
	leaq	-1(%r13), %r14
	shlq	%cl, %r13
	jmp	.LBB24_16
	.align	16, 0x90
.LBB24_17:                              # %for.body67.for.body67_crit_edge
                                        #   in Loop: Header=BB24_16 Depth=2
	movq	(%r12,%r15,8), %rdi
	decq	%r14
	addq	$-8, %r13
.LBB24_16:                              # %for.body67
                                        #   Parent Loop BB24_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx,%r15,8), %rsi
	addq	%r13, %rsi
	addq	%r13, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	copy_replacements_1
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB24_17
	jmp	.LBB24_11
	.align	16, 0x90
.LBB24_10:                              # %for.body41
                                        #   in Loop: Header=BB24_9 Depth=1
	cmpq	$101, %rax
	jne	.LBB24_11
# BB#13:                                # %if.then47
                                        #   in Loop: Header=BB24_9 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	8(%r12,%rax,8), %rdi
	leaq	8(%rbx,%rax,8), %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	copy_replacements_1
.LBB24_11:                              # %for.cond38.backedge
                                        #   in Loop: Header=BB24_9 Depth=1
	movq	$32, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB24_9
.LBB24_12:                              # %for.end86
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp345:
	.size	copy_replacements_1, .Ltmp345-copy_replacements_1
	.cfi_endproc

	.globl	move_replacements
	.align	16, 0x90
	.type	move_replacements,@function
move_replacements:                      # @move_replacements
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_replacements(%rip), %r8
	testq	%r8, %r8
	jle	.LBB25_7
# BB#1:
	movq	$1, %r9
	movabsq	$replacements+8, %rax
	.align	16, 0x90
.LBB25_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB25_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	%rsi, (%rax)
	jmp	.LBB25_6
	.align	16, 0x90
.LBB25_4:                               # %if.else
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	-8(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB25_6
# BB#5:                                 # %if.then8
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	%rsi, -8(%rax)
	movq	$0, (%rax)
.LBB25_6:                               # %for.inc
                                        #   in Loop: Header=BB25_2 Depth=1
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r9
	addq	$24, %rax
	cmpq	%r8, %rdx
	jl	.LBB25_2
.LBB25_7:                               # %for.end
	retq
.Ltmp346:
	.size	move_replacements, .Ltmp346-move_replacements
	.cfi_endproc

	.globl	find_replacement
	.align	16, 0x90
	.type	find_replacement,@function
find_replacement:                       # @find_replacement
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp350:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp351:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp352:
	.cfi_def_cfa_offset 32
.Ltmp353:
	.cfi_offset %rbx, -24
.Ltmp354:
	.cfi_offset %r14, -16
	movslq	n_replacements(%rip), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	jle	.LBB26_14
# BB#1:                                 # %for.body.lr.ph
	leaq	(%rax,%rax,2), %rax
	leaq	replacements(,%rax,8), %rcx
	movabsq	$replacements, %rax
	.align	16, 0x90
.LBB26_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	16(%rax), %rdx
	imulq	$104, %rdx, %rdx
	movq	rld+64(%rdx), %rsi
	testq	%rsi, %rsi
	je	.LBB26_13
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	(%rax), %rdx
	cmpq	%rbx, %rdx
	je	.LBB26_4
# BB#8:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB26_2 Depth=1
	movq	8(%rax), %rdx
	cmpq	%rbx, %rdx
	je	.LBB26_9
.LBB26_13:                              # %for.inc
                                        #   in Loop: Header=BB26_2 Depth=1
	addq	$24, %rax
	cmpq	%rcx, %rax
	jb	.LBB26_2
.LBB26_14:                              # %for.end
	movq	(%rbx), %rsi
	movzbl	(%rsi), %ecx
	addq	$-75, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB26_18
# BB#15:                                # %for.end
	cmpq	$2, %rax
	je	.LBB26_18
# BB#16:                                # %if.then94
	addq	$8, %rsi
	movq	%rsi, %rdi
	callq	find_replacement
	movq	(%rbx), %rdi
	movq	%rax, %r14
	addq	$16, %rdi
	callq	find_replacement
	movq	(%rbx), %rsi
	movq	8(%rsi), %rcx
	cmpq	%rcx, %r14
	jne	.LBB26_19
# BB#17:                                # %lor.lhs.false108
	movq	16(%rsi), %rcx
	cmpq	%rcx, %rax
	je	.LBB26_18
.LBB26_19:                              # %if.then114
	movl	(%rsi), %esi
	movq	%rsi, %rdi
	andq	$65535, %rdi            # imm = 0xFFFF
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB26_4:                               # %if.then
	movl	20(%rax), %edi
	testq	%rdi, %rdi
	je	.LBB26_18
# BB#5:                                 # %land.lhs.true5
	movzbl	2(%rsi), %eax
	cmpq	%rdi, %rax
	je	.LBB26_18
# BB#6:                                 # %if.then8
	movl	8(%rsi), %esi
	jmp	.LBB26_7
.LBB26_9:                               # %if.then14
	movl	(%rsi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB26_11
# BB#10:                                # %if.then18
	movq	(%rbx), %rax
	movl	8(%rsi), %ebx
	movq	8(%rax), %rcx
	movl	16(%rax), %edx
	movzbl	2(%rax), %r14d
	movzbl	2(%rcx), %esi
	movl	8(%rcx), %edi
	movq	%r14, %rcx
	callq	subreg_regno_offset
	addq	%rax, %rbx
	movq	%r14, %rdi
	movq	%rbx, %rsi
.LBB26_7:                               # %if.then8
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_REG  # TAILCALL
.LBB26_11:                              # %if.else44
	movq	(%rbx), %rdx
	movzbl	2(%rdx), %edi
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	cmpq	%rdi, %rax
	jne	.LBB26_12
.LBB26_18:                              # %return
	movq	%rsi, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB26_12:                              # %if.else53
	movl	16(%rsi), %eax
	movl	16(%rdx), %ebx
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movzbl	mode_size(%rdi), %r8d
	sarq	%cl, %rax
	cqto
	idivq	%r8
	movq	8(%rsi), %rsi
	subq	%rdx, %rbx
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	gen_rtx_SUBREG  # TAILCALL
.Ltmp355:
	.size	find_replacement, .Ltmp355-find_replacement
	.cfi_endproc

	.globl	refers_to_mem_for_reload_p
	.align	16, 0x90
	.type	refers_to_mem_for_reload_p,@function
refers_to_mem_for_reload_p:             # @refers_to_mem_for_reload_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp362:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp363:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp364:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp365:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp366:
	.cfi_def_cfa_offset 48
.Ltmp367:
	.cfi_offset %rbx, -48
.Ltmp368:
	.cfi_offset %r12, -40
.Ltmp369:
	.cfi_offset %r13, -32
.Ltmp370:
	.cfi_offset %r14, -24
.Ltmp371:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	(%r15), %eax
	cmpq	$66, %rax
	movq	$1, %r14
	je	.LBB27_13
# BB#1:                                 # %entry
	cmpq	$61, %rax
	jne	.LBB27_6
# BB#2:                                 # %if.then4
	movl	8(%r15), %eax
	xorq	%r14, %r14
	cmpq	$53, %rax
	jb	.LBB27_13
# BB#3:                                 # %land.rhs
	movq	reg_equiv_memory_loc(%rip), %rcx
	cmpq	$0, (%rcx,%rax,8)
	je	.LBB27_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %r14
	jmp	.LBB27_13
.LBB27_6:                               # %if.end10
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB27_12
# BB#7:                                 # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r12
	decq	%rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB27_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB27_11
# BB#9:                                 # %land.lhs.true
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	8(%r15,%rbx,8), %rdi
	movzbl	(%rdi), %eax
	cmpq	$66, %rax
	je	.LBB27_13
# BB#10:                                # %lor.lhs.false
                                        #   in Loop: Header=BB27_8 Depth=1
	callq	refers_to_mem_for_reload_p
	testq	%r13, %rax
	jne	.LBB27_13
.LBB27_11:                              # %for.cond.backedge
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB27_8
.LBB27_12:
	xorq	%r14, %r14
	jmp	.LBB27_13
.LBB27_4:
	movabsq	$0, %r14
.LBB27_13:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp372:
	.size	refers_to_mem_for_reload_p, .Ltmp372-refers_to_mem_for_reload_p
	.cfi_endproc

	.globl	regno_clobbered_p
	.align	16, 0x90
	.type	regno_clobbered_p,@function
regno_clobbered_p:                      # @regno_clobbered_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %r8
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rdi, %rax
	cmpq	$8, %rax
	je	.LBB28_4
# BB#1:                                 # %entry
	leaq	-21(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB28_4
# BB#2:                                 # %entry
	leaq	-45(%rdi), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB28_4
# BB#3:                                 # %entry
	movq	%rdi, %rcx
	addq	$-29, %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB28_4
# BB#9:                                 # %cond.false
	andq	%rdx, %rax
	cmpq	$24, %rax
	jne	.LBB28_10
# BB#12:                                # %cond.true22
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB28_14
.LBB28_4:                               # %cond.true
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	mode_class(,%rdx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB28_14
# BB#5:                                 # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB28_6
# BB#7:                                 # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB28_14
.LBB28_10:                              # %cond.false
	cmpq	$18, %rax
	jne	.LBB28_13
# BB#11:                                # %cond.true18
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB28_14
.LBB28_6:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB28_14
.LBB28_13:                              # %cond.false26
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	target_flags(%rip), %r9d
	sarq	%cl, %rdx
	movzbl	mode_size(%rdx), %eax
	movq	$23, %rcx
	shrq	%cl, %r9
	andq	$4, %r9
	leaq	3(%rax,%r9), %rax
	addq	$4, %r9
	cqto
	idivq	%r9
.LBB28_14:                              # %cond.end38
	movq	32(%rsi), %rcx
	movzbl	(%rcx), %edx
	addq	%rdi, %rax
	cmpq	$49, %rdx
	je	.LBB28_17
# BB#15:                                # %lor.lhs.false44
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r8
	je	.LBB28_22
# BB#16:                                # %lor.lhs.false44
	cmpq	$47, %rdx
	jne	.LBB28_22
.LBB28_17:                              # %land.lhs.true54
	movq	8(%rcx), %r9
	movzbl	(%r9), %esi
	cmpq	$61, %rsi
	jne	.LBB28_22
# BB#18:                                # %if.then
	movl	8(%r9), %edx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	%rax, %rdx
	sbbq	%rcx, %rcx
	andq	%rdi, %rsi
	cmpq	%rsi, %rdx
	jb	.LBB28_19
# BB#20:                                # %if.then
	movabsq	$1, %rax
	andq	%rax, %rcx
	movq	%rcx, %rax
	retq
.LBB28_22:                              # %if.end
	cmpq	$39, %rdx
	jne	.LBB28_35
# BB#23:                                # %if.then84
	movq	8(%rcx), %r9
	movslq	(%r9), %rsi
	testq	%rsi, %rsi
	jle	.LBB28_35
# BB#24:                                # %for.body.lr.ph
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	decq	%rsi
	andq	%rcx, %rdi
	andq	%rcx, %rax
	testq	%rcx, %r8
	je	.LBB28_30
	.align	16, 0x90
.LBB28_25:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r9,%rsi,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$49, %rdx
	jne	.LBB28_29
# BB#26:                                # %land.lhs.true115
                                        #   in Loop: Header=BB28_25 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB28_29
# BB#27:                                # %if.then123
                                        #   in Loop: Header=BB28_25 Depth=1
	movl	8(%rcx), %r8d
	cmpq	%rdi, %r8
	jb	.LBB28_29
# BB#28:                                # %if.then123
                                        #   in Loop: Header=BB28_25 Depth=1
	movq	$1, %rcx
	cmpq	%rax, %r8
	jb	.LBB28_36
	.align	16, 0x90
.LBB28_29:                              # %for.cond.backedge
                                        #   in Loop: Header=BB28_25 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rsi
	testq	%rdx, %rdx
	jg	.LBB28_25
	jmp	.LBB28_35
	.align	16, 0x90
.LBB28_30:                              # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r9,%rsi,8), %rcx
	movzbl	(%rcx), %edx
	cmpq	$49, %rdx
	jne	.LBB28_34
# BB#31:                                # %land.lhs.true115.us
                                        #   in Loop: Header=BB28_30 Depth=1
	movq	8(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB28_34
# BB#32:                                # %if.then123.us
                                        #   in Loop: Header=BB28_30 Depth=1
	movl	8(%rcx), %r8d
	cmpq	%rdi, %r8
	jb	.LBB28_34
# BB#33:                                # %if.then123.us
                                        #   in Loop: Header=BB28_30 Depth=1
	movq	$1, %rcx
	cmpq	%rax, %r8
	jb	.LBB28_36
	.align	16, 0x90
.LBB28_34:                              # %for.cond.backedge.us
                                        #   in Loop: Header=BB28_30 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%rsi
	testq	%rdx, %rdx
	jg	.LBB28_30
.LBB28_35:
	xorq	%rcx, %rcx
.LBB28_36:                              # %return
	movq	%rcx, %rax
	retq
.LBB28_19:
	movabsq	$0, %rax
	andq	%rax, %rcx
	movq	%rcx, %rax
	retq
.Ltmp373:
	.size	regno_clobbered_p, .Ltmp373-regno_clobbered_p
	.cfi_endproc

	.globl	debug_reload_to_stream
	.align	16, 0x90
	.type	debug_reload_to_stream,@function
debug_reload_to_stream:                 # @debug_reload_to_stream
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp380:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp381:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp382:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp383:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp384:
	.cfi_def_cfa_offset 48
.Ltmp385:
	.cfi_offset %rbx, -48
.Ltmp386:
	.cfi_offset %r12, -40
.Ltmp387:
	.cfi_offset %r13, -32
.Ltmp388:
	.cfi_offset %r14, -24
.Ltmp389:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	jne	.LBB29_2
# BB#1:
	movq	stderr(%rip), %rbx
.LBB29_2:                               # %entry
	movslq	n_reloads(%rip), %rax
	testq	%rax, %rax
	jle	.LBB29_33
# BB#3:
	movq	$1, %r14
	movabsq	$rld+96, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$.L.str8, %r13
	.align	16, 0x90
.LBB29_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%r14), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str6, %rsi
	callq	fprintf
	cmpq	$0, -96(%r15)
	je	.LBB29_6
# BB#5:                                 # %if.then2
                                        #   in Loop: Header=BB29_4 Depth=1
	movslq	-76(%r15), %rax
	movq	mode_name(,%rax,8), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str7, %rsi
	callq	fprintf
	movq	-96(%r15), %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	print_inline_rtx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB29_6:                               # %if.end12
                                        #   in Loop: Header=BB29_4 Depth=1
	cmpq	$0, -88(%r15)
	je	.LBB29_8
# BB#7:                                 # %if.then16
                                        #   in Loop: Header=BB29_4 Depth=1
	movslq	-72(%r15), %rax
	movq	mode_name(,%rax,8), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str9, %rsi
	callq	fprintf
	movq	-88(%r15), %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	print_inline_rtx
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB29_8:                               # %if.end26
                                        #   in Loop: Header=BB29_4 Depth=1
	movslq	-80(%r15), %rax
	movq	reg_class_names(,%rax,8), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str10, %rsi
	callq	fprintf
	movslq	-4(%r15), %rax
	movq	reload_when_needed_name(,%rax,8), %rdx
	movl	-24(%r15), %ecx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str11, %rsi
	callq	fprintf
	movzbl	(%r15), %eax
	testq	$1, %rax
	je	.LBB29_10
# BB#9:                                 # %if.then42
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$10, %rsi
	movq	$1, %rdx
	movabsq	$.L.str12, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movzbl	(%r15), %eax
.LBB29_10:                              # %if.end44
                                        #   in Loop: Header=BB29_4 Depth=1
	testq	$8, %rax
	je	.LBB29_12
# BB#11:                                # %if.then51
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$10, %rsi
	movq	$1, %rdx
	movabsq	$.L.str13, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB29_12:                              # %if.end53
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	-60(%r15), %edx
	testq	%rdx, %rdx
	je	.LBB29_14
# BB#13:                                # %if.then57
                                        #   in Loop: Header=BB29_4 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str14, %rsi
	callq	fprintf
.LBB29_14:                              # %if.end62
                                        #   in Loop: Header=BB29_4 Depth=1
	movzbl	(%r15), %eax
	testq	$2, %rax
	je	.LBB29_16
# BB#15:                                # %if.then70
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$15, %rsi
	movq	$1, %rdx
	movabsq	$.L.str15, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movzbl	(%r15), %eax
.LBB29_16:                              # %if.end72
                                        #   in Loop: Header=BB29_4 Depth=1
	testq	$4, %rax
	je	.LBB29_18
# BB#17:                                # %if.then80
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$20, %rsi
	movq	$1, %rdx
	movabsq	$.L.str16, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB29_18:                              # %if.end82
                                        #   in Loop: Header=BB29_4 Depth=1
	cmpq	$0, -56(%r15)
	je	.LBB29_20
# BB#19:                                # %if.then86
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$17, %rsi
	movq	$1, %rdx
	movabsq	$.L.str17, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movq	-56(%r15), %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	print_inline_rtx
.LBB29_20:                              # %if.end91
                                        #   in Loop: Header=BB29_4 Depth=1
	cmpq	$0, -48(%r15)
	je	.LBB29_22
# BB#21:                                # %if.then95
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$18, %rsi
	movq	$1, %rdx
	movabsq	$.L.str18, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movq	-48(%r15), %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	print_inline_rtx
.LBB29_22:                              # %if.end100
                                        #   in Loop: Header=BB29_4 Depth=1
	cmpq	$0, -32(%r15)
	je	.LBB29_24
# BB#23:                                # %if.then104
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$18, %rsi
	movq	$1, %rdx
	movabsq	$.L.str19, %rdi
	movq	%rbx, %rcx
	callq	fwrite
	movq	-32(%r15), %rsi
	movq	$24, %rdx
	movq	%rbx, %rdi
	callq	print_inline_rtx
.LBB29_24:                              # %if.end109
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	-20(%r15), %ecx
	cmpq	%r12, %rcx
	movq	%r13, %rdx
	je	.LBB29_26
# BB#25:                                # %if.then113
                                        #   in Loop: Header=BB29_4 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str20, %rsi
	movq	%r13, %rdx
	callq	fprintf
	movabsq	$.L.str21, %rdx
.LBB29_26:                              # %if.end118
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	-16(%r15), %ecx
	cmpq	%r12, %rcx
	je	.LBB29_28
# BB#27:                                # %if.then122
                                        #   in Loop: Header=BB29_4 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str22, %rsi
	callq	fprintf
.LBB29_28:                              # %if.end127
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	-12(%r15), %eax
	cmpq	$1317, %rax             # imm = 0x525
	movq	%r13, %rdx
	je	.LBB29_30
# BB#29:                                # %if.then131
                                        #   in Loop: Header=BB29_4 Depth=1
	leaq	(%rax,%rax,4), %rax
	movq	insn_data(,%rax,8), %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str23, %rsi
	movq	%r13, %rdx
	callq	fprintf
	movabsq	$.L.str21, %rdx
.LBB29_30:                              # %if.end138
                                        #   in Loop: Header=BB29_4 Depth=1
	movl	-8(%r15), %eax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB29_32
# BB#31:                                # %if.then142
                                        #   in Loop: Header=BB29_4 Depth=1
	leaq	(%rax,%rax,4), %rax
	movq	insn_data(,%rax,8), %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str24, %rsi
	callq	fprintf
.LBB29_32:                              # %if.end150
                                        #   in Loop: Header=BB29_4 Depth=1
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	fputc
	movslq	n_reloads(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$104, %r15
	cmpq	%rax, %rdx
	jl	.LBB29_4
.LBB29_33:                              # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp390:
	.size	debug_reload_to_stream, .Ltmp390-debug_reload_to_stream
	.cfi_endproc

	.globl	debug_reload
	.align	16, 0x90
	.type	debug_reload,@function
debug_reload:                           # @debug_reload
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rdi
	jmp	debug_reload_to_stream  # TAILCALL
.Ltmp391:
	.size	debug_reload, .Ltmp391-debug_reload
	.cfi_endproc

	.align	16, 0x90
	.type	make_memloc,@function
make_memloc:                            # @make_memloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp396:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp397:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp398:
	.cfi_def_cfa_offset 32
.Ltmp399:
	.cfi_offset %rbx, -32
.Ltmp400:
	.cfi_offset %r14, -24
.Ltmp401:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	$32, %rcx
	movq	reg_equiv_memory_loc(%rip), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%rax,%rbx,8), %rax
	movq	%rdi, %r14
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	eliminate_regs
	movq	8(%rax), %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	rtx_varies_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB31_2
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	callq	copy_rtx
	movq	%rax, %r15
.LBB31_2:                               # %if.end
	movq	reg_equiv_memory_loc(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	replace_equiv_address_nv
	movzbl	2(%r14), %esi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%rax, %rdi
	callq	adjust_address_1
	movq	reg_equiv_memory_loc(%rip), %rcx
	movq	(%rcx,%rbx,8), %rcx
	cmpq	%rcx, %rax
	je	.LBB31_4
# BB#3:                                 # %if.end12
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB31_4:                               # %if.then10
	movq	%rax, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	copy_rtx  # TAILCALL
.Ltmp402:
	.size	make_memloc, .Ltmp402-make_memloc
	.cfi_endproc

	.align	16, 0x90
	.type	subst_reg_equivs,@function
subst_reg_equivs:                       # @subst_reg_equivs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp409:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp410:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp411:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp412:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp413:
	.cfi_def_cfa_offset 48
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
	movq	%rdi, %r12
	movzbl	(%r12), %eax
	movq	%rsi, %r14
	cmpq	$133, %rax
	jg	.LBB32_9
# BB#1:                                 # %entry
	leaq	-54(%rax), %rcx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rcx
	cmpq	$21, %rcx
	ja	.LBB32_19
# BB#2:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57399, %rdx            # imm = 0xE037
	je	.LBB32_4
# BB#3:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_9:                               # %entry
	cmpq	$134, %rax
	jne	.LBB32_19
# BB#10:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_4:                               # %entry
	cmpq	$7, %rcx
	jne	.LBB32_5
# BB#11:                                # %sw.bb1
	movslq	8(%r12), %rbx
	movq	reg_equiv_constant(%rip), %rax
	movq	(%rax,%rbx,8), %r15
	testq	%r15, %r15
	je	.LBB32_13
# BB#12:                                # %if.then
	movq	$1, %rax
	movb	%al, subst_reg_equivs_changed(%rip)
	jmp	.LBB32_27
.LBB32_5:                               # %entry
	cmpq	$21, %rcx
	jne	.LBB32_19
# BB#6:                                 # %sw.bb20
	movq	8(%r12), %rcx
	movq	global_rtl+24(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB32_19
# BB#7:                                 # %land.lhs.true24
	movq	16(%r12), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	jne	.LBB32_19
# BB#8:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_19:                              # %sw.epilog
	movzbl	rtx_length(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB32_20
# BB#21:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r15
	decq	%rbx
	.align	16, 0x90
.LBB32_22:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB32_23
# BB#25:                                # %if.then44
                                        #   in Loop: Header=BB32_22 Depth=1
	movq	8(%r12,%rbx,8), %rdi
	movq	%r14, %rsi
	callq	subst_reg_equivs
	movq	%rax, 8(%r12,%rbx,8)
.LBB32_23:                              # %for.cond.backedge
                                        #   in Loop: Header=BB32_22 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB32_22
# BB#24:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_20:
	movq	%r12, %r15
.LBB32_27:                              # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_13:                              # %if.end
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	je	.LBB32_14
# BB#15:                                # %if.end
	movl	num_not_at_initial_offset(%rip), %eax
	testq	%r13, %rax
	je	.LBB32_16
# BB#17:                                # %if.then8
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	make_memloc
	movq	reg_equiv_mem(%rip), %rcx
	movq	(%rcx,%rbx,8), %rsi
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	rtx_equal_p
	testq	%r13, %rax
	je	.LBB32_26
# BB#18:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_14:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_16:
	movq	%r12, %r15
	jmp	.LBB32_27
.LBB32_26:                              # %if.then13
	movq	$1, %rax
	movb	%al, subst_reg_equivs_changed(%rip)
	movq	$48, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	$2, %rcx
	movb	%cl, 2(%rax)
	jmp	.LBB32_27
.Ltmp419:
	.size	subst_reg_equivs, .Ltmp419-subst_reg_equivs
	.cfi_endproc

	.align	16, 0x90
	.type	find_reloads_address_part,@function
find_reloads_address_part:              # @find_reloads_address_part
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp426:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp427:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp428:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp429:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp430:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp431:
	.cfi_def_cfa_offset 112
.Ltmp432:
	.cfi_offset %rbx, -48
.Ltmp433:
	.cfi_offset %r12, -40
.Ltmp434:
	.cfi_offset %r13, -32
.Ltmp435:
	.cfi_offset %r14, -24
.Ltmp436:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movl	(%r13), %eax
	movq	%r9, %r15
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r12
	movq	%rsi, %rbx
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$133, %rdx
	jg	.LBB33_3
# BB#1:                                 # %entry
	addq	$-54, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB33_7
# BB#2:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB33_5
	jmp	.LBB33_7
.LBB33_3:                               # %entry
	cmpq	$134, %rdx
	je	.LBB33_5
# BB#4:                                 # %entry
	cmpq	$140, %rdx
	jne	.LBB33_7
.LBB33_5:                               # %land.lhs.true
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	ix86_preferred_reload_class
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB33_17
# BB#6:                                 # %land.lhs.true.if.else_crit_edge
	movl	(%r13), %eax
.LBB33_7:                               # %if.else
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$75, %rax
	jne	.LBB33_16
# BB#8:                                 # %land.lhs.true37
	movq	16(%r13), %rdi
	movzbl	(%rdi), %eax
	cmpq	$133, %rax
	jg	.LBB33_11
# BB#9:                                 # %land.lhs.true37
	addq	$-54, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB33_16
# BB#10:                                # %land.lhs.true37
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB33_13
	jmp	.LBB33_16
.LBB33_17:                              # %if.then
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	force_const_mem
	movq	%rax, %r13
	movq	%r13, 56(%rsp)
	movq	8(%r13), %rdx
	leaq	8(%r13), %rcx
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	leaq	56(%rsp), %rsi
	jmp	.LBB33_15
.LBB33_11:                              # %land.lhs.true37
	cmpq	$134, %rax
	je	.LBB33_13
# BB#12:                                # %land.lhs.true37
	cmpq	$140, %rax
	jne	.LBB33_16
.LBB33_13:                              # %land.lhs.true93
	movq	%r12, %rsi
	callq	ix86_preferred_reload_class
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB33_16
# BB#14:                                # %if.then99
	movq	16(%r13), %rsi
	movzbl	2(%r13), %edi
	callq	force_const_mem
	movq	%rax, 48(%rsp)
	movq	8(%r13), %rdx
	movzbl	2(%r13), %esi
	movq	$75, %rdi
	movq	%rax, %r13
	movq	%r13, %rcx
	callq	gen_rtx_fmt_ee
	movq	8(%r13), %rdx
	movq	%r13, %rcx
	movq	%rax, %r13
	leaq	8(%rcx), %rcx
	movq	112(%rsp), %rax
	movq	%rax, (%rsp)
	leaq	48(%rsp), %rsi
.LBB33_15:                              # %if.end121
	movq	$0, 8(%rsp)
	movq	%r14, %rdi
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	%r15, %r9
	callq	find_reloads_address
.LBB33_16:                              # %if.end121
	movq	%r15, 32(%rsp)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$0, (%rsp)
	movq	%r13, %rdi
	movq	%rbx, %rdx
	movq	%r12, %r8
	movq	%r14, %r9
	callq	push_reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp437:
	.size	find_reloads_address_part, .Ltmp437-find_reloads_address_part
	.cfi_endproc

	.align	16, 0x90
	.type	find_reloads_address_1,@function
find_reloads_address_1:                 # @find_reloads_address_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp444:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp445:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp446:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp447:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp448:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp449:
	.cfi_def_cfa_offset 144
.Ltmp450:
	.cfi_offset %rbx, -48
.Ltmp451:
	.cfi_offset %r12, -40
.Ltmp452:
	.cfi_offset %r13, -32
.Ltmp453:
	.cfi_offset %r14, -24
.Ltmp454:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	%r15, 88(%rsp)
	movl	(%r15), %eax
	movq	144(%rsp), %r11
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	andq	$65535, %r12            # imm = 0xFFFF
	cmpq	$95, %r12
	jg	.LBB34_10
# BB#1:                                 # %entry
	cmpq	$74, %r12
	jg	.LBB34_16
# BB#2:                                 # %entry
	cmpq	$61, %r12
	je	.LBB34_174
# BB#3:                                 # %entry
	cmpq	$63, %r12
	jne	.LBB34_4
# BB#213:                               # %sw.bb1355
	movq	8(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB34_244
# BB#214:                               # %if.then1363
	movl	8(%rax), %eax
	cmpq	$52, %rax
	ja	.LBB34_230
# BB#215:                               # %if.then1372
	movq	%r11, %rbx
	movq	%r15, %rdi
	callq	subreg_regno
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rdx, %rcx
	je	.LBB34_220
# BB#216:                               # %cond.true1376
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$7, %rsi
	movq	%rbx, %r11
	jl	.LBB34_244
# BB#217:                               # %cond.true1376
	leaq	-37(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_244
# BB#218:                               # %lor.lhs.false1385
	movq	$32, %rcx
	movq	reg_renumber(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rsi,%rax,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_226
# BB#219:                               # %lor.lhs.false1385
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
.LBB34_225:                             # %lor.lhs.false1418
	testq	%rcx, %rax
	jne	.LBB34_244
	jmp	.LBB34_226
.LBB34_10:                              # %entry
	leaq	-96(%r12), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$4, %rcx
	jae	.LBB34_11
# BB#123:                               # %sw.bb867
	movq	8(%r15), %rbx
	movzbl	(%rbx), %ecx
	addq	$8, %r15
	cmpq	$66, %rcx
	movq	48(%rsp), %r12          # 8-byte Reload
	jne	.LBB34_124
# BB#168:                               # %if.then1116
	movq	$16, %rcx
	movq	8(%rbx), %rdx
	shrq	%cl, %rax
	andq	$255, %rax
	addq	$8, %rbx
	movq	%r11, 8(%rsp)
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rax, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rcx
	movq	%r12, %r8
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address
	movq	88(%rsp), %rdi
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB34_169
# BB#170:                               # %if.then1116
	movabsq	$1, %rax
	jmp	.LBB34_171
.LBB34_16:                              # %entry
	cmpq	$75, %r12
	jne	.LBB34_244
# BB#17:                                # %sw.bb
	movq	8(%r15), %r8
	movq	16(%r15), %rdi
	movl	(%r8), %eax
	movl	(%rdi), %r13d
	movq	%rax, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	cmpq	$63, %r14
	movq	%r13, %rbx
	movq	%r8, %rdx
	jne	.LBB34_23
# BB#18:                                # %if.then
	movq	8(%r8), %rdx
	movl	(%rdx), %esi
	movq	%rsi, %r14
	andq	$65535, %r14            # imm = 0xFFFF
	cmpq	$61, %r14
	jne	.LBB34_19
# BB#20:                                # %land.lhs.true
	movq	%rdi, %r15
	movl	8(%rdx), %ebx
	cmpq	$52, %rbx
	movq	$61, %r14
	ja	.LBB34_21
# BB#22:                                # %if.then19
	movq	$16, %rcx
	shrq	%cl, %rsi
	movq	$16, %rcx
	movl	16(%r8), %edx
	shrq	%cl, %rax
	andq	$255, %rsi
	andq	$255, %rax
	movq	%rbx, %rdi
	movq	%rax, %rcx
	movq	%r8, %r12
	callq	subreg_regno_offset
	addq	%rax, %rbx
	movl	word_mode(%rip), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movq	%r12, %r8
	movq	144(%rsp), %r11
	movq	%r15, %rdi
	movl	(%rdi), %ebx
	movq	%rax, %rdx
	jmp	.LBB34_23
.LBB34_11:                              # %entry
	leaq	-100(%r12), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jae	.LBB34_244
# BB#12:                                # %sw.bb623
	movq	16(%r15), %rdx
	movzbl	(%rdx), %eax
	addq	$-75, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB34_248
# BB#13:                                # %if.end642
	movq	8(%r15), %rax
	movq	8(%rdx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB34_14
# BB#96:                                # %if.end649
	movq	16(%rdx), %rdi
	movzbl	(%rdi), %ecx
	leaq	8(%rdx), %r13
	cmpq	$61, %rcx
	movq	48(%rsp), %r12          # 8-byte Reload
	jne	.LBB34_97
# BB#98:                                # %if.then657
	movl	8(%rdi), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_101
# BB#99:                                # %if.then657
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rbx   # imm = 0x1FE00000007F
	testq	%rbx, %rsi
	je	.LBB34_101
# BB#100:
	movq	%r11, %r15
	jmp	.LBB34_104
.LBB34_124:                             # %sw.bb867
	cmpq	$61, %rcx
	jne	.LBB34_248
# BB#125:                               # %if.then875
	movslq	8(%rbx), %r12
	cmpq	$53, %r12
	jl	.LBB34_128
# BB#126:                               # %land.lhs.true885
	movq	reg_equiv_constant(%rip), %rax
	cmpq	$0, (%rax,%r12,8)
	jne	.LBB34_127
.LBB34_128:                             # %if.end891
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%r12,8)
	je	.LBB34_135
# BB#129:                               # %land.lhs.true896
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%r12,8)
	jne	.LBB34_131
# BB#130:                               # %land.lhs.true896
	movl	num_not_at_initial_offset(%rip), %eax
	testq	%r14, %rax
	je	.LBB34_135
.LBB34_131:                             # %if.then903
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	make_memloc
	movq	reg_equiv_address(%rip), %rcx
	movq	%rax, 80(%rsp)
	cmpq	$0, (%rcx,%r12,8)
	jne	.LBB34_134
# BB#132:                               # %lor.lhs.false912
	movq	reg_equiv_mem(%rip), %rcx
	movq	(%rcx,%r12,8), %rsi
	movq	%rax, %rdi
	callq	rtx_equal_p
	testq	%r14, %rax
	jne	.LBB34_135
# BB#133:                               # %lor.lhs.false912.if.then917_crit_edge
	movq	80(%rsp), %rax
.LBB34_134:                             # %if.then917
	movq	8(%rax), %rdx
	movzbl	2(%rax), %edi
	leaq	8(%rax), %rcx
	movq	144(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	80(%rsp), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address
	movq	88(%rsp), %rax
	movl	(%rax), %esi
	movq	%rsi, %rdi
	movq	$16, %rcx
	movq	80(%rsp), %rdx
	shrq	%cl, %rsi
	andq	$65535, %rdi            # imm = 0xFFFF
	andq	$255, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, 88(%rsp)
.LBB34_135:                             # %if.end935
	movq	reg_renumber(%rip), %rdx
	movswq	(%rdx,%r12,2), %rax
	testq	%rax, %rax
	jns	.LBB34_137
# BB#136:                               # %if.end935
	movq	%r12, %rax
.LBB34_137:                             # %if.end935
	cmpq	$52, %rax
	movq	144(%rsp), %rdi
	jg	.LBB34_149
# BB#138:                               # %lor.lhs.false948
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r14, %rcx
	je	.LBB34_143
# BB#139:                               # %cond.true
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$7, %rsi
	jl	.LBB34_248
# BB#140:                               # %cond.true
	leaq	-37(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_248
# BB#141:                               # %lor.lhs.false958
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	(%rdx,%rsi,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_149
# BB#142:                               # %lor.lhs.false958
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
.LBB34_148:                             # %lor.lhs.false990
	testq	%rcx, %rdx
	jne	.LBB34_248
	jmp	.LBB34_149
.LBB34_174:                             # %sw.bb1195
	movslq	8(%r15), %r14
	movq	reg_equiv_constant(%rip), %rcx
	movq	(%rcx,%r14,8), %rdi
	testq	%rdi, %rdi
	je	.LBB34_179
# BB#175:                               # %if.then1204
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rdx          # 8-byte Reload
	testq	%rcx, %rdx
	movq	48(%rsp), %r8           # 8-byte Reload
	je	.LBB34_176
# BB#177:                               # %if.then1204
	movabsq	$1, %rcx
	jmp	.LBB34_178
.LBB34_4:                               # %entry
	cmpq	$66, %r12
	jne	.LBB34_244
# BB#5:                                 # %sw.bb1166
	movq	$16, %rcx
	shrq	%cl, %rax
	movq	8(%r15), %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	movq	$4, %r9
	cmpq	$3, %rcx
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB34_8
# BB#6:                                 # %cond.false1179
	cmpq	$5, %rcx
	movq	$6, %r9
	je	.LBB34_8
# BB#7:                                 # %cond.false1179
	movq	72(%rsp), %r9           # 8-byte Reload
.LBB34_8:                               # %cond.end1186
	andq	$255, %rax
	addq	$8, %r15
	movq	%r11, 8(%rsp)
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%r15, %rcx
	movq	%rbx, %r8
	callq	find_reloads_address
	movq	(%r13), %rdi
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB34_9
# BB#172:                               # %cond.end1186
	movabsq	$1, %rax
	jmp	.LBB34_173
.LBB34_169:
	movabsq	$0, %rax
.LBB34_171:                             # %if.then1116
	movzbl	2(%rdi), %r9d
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%r12, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$0, (%rsp)
	movq	%r13, %rdx
	callq	push_reload
	movq	this_insn(%rip), %rcx
	movq	88(%rsp), %rdx
	movq	32(%rcx), %rdi
	movq	8(%rdx), %rsi
	jmp	.LBB34_167
.LBB34_19:
	movq	%r13, %rbx
	jmp	.LBB34_23
.LBB34_179:                             # %if.end1212
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%r14,8)
	movq	48(%rsp), %r12          # 8-byte Reload
	je	.LBB34_188
# BB#180:                               # %land.lhs.true1216
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%r14,8)
	jne	.LBB34_182
# BB#181:                               # %land.lhs.true1216
	movl	num_not_at_initial_offset(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB34_188
.LBB34_182:                             # %if.then1223
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r11, %rbx
	callq	make_memloc
	movq	%rbx, %r8
	movq	reg_equiv_address(%rip), %rcx
	cmpq	$0, (%rcx,%r14,8)
	movq	%rax, %rbx
	jne	.LBB34_184
# BB#183:                               # %lor.lhs.false1230
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r14,8), %rsi
	movq	%rbx, %rdi
	movq	%r8, %r15
	callq	rtx_equal_p
	movq	%r15, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB34_188
.LBB34_184:                             # %if.then1235
	movq	%rbx, 88(%rsp)
	movzbl	2(%rbx), %edi
	movq	8(%rbx), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	movq	$4, %r9
	cmpq	$3, %rax
	je	.LBB34_187
# BB#185:                               # %cond.false1248
	cmpq	$5, %rax
	movq	$6, %r9
	je	.LBB34_187
# BB#186:                               # %cond.false1248
	movq	72(%rsp), %r9           # 8-byte Reload
.LBB34_187:                             # %cond.end1255
	addq	$8, %rbx
	movq	%r8, 8(%rsp)
	leaq	88(%rsp), %rsi
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %rcx
	movq	%r12, %r8
	callq	find_reloads_address
.LBB34_188:                             # %if.end1259
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%r14,2), %rdi
	testq	%rdi, %rdi
	jns	.LBB34_190
# BB#189:                               # %if.end1259
	movq	%r14, %rdi
.LBB34_190:                             # %if.end1259
	cmpq	$52, %rdi
	jg	.LBB34_202
# BB#191:                               # %lor.lhs.false1272
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rsi, %rcx
	je	.LBB34_196
# BB#192:                               # %cond.true1274
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$7, %rdx
	jl	.LBB34_208
# BB#193:                               # %cond.true1274
	leaq	-37(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_208
# BB#194:                               # %lor.lhs.false1283
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_202
# BB#195:                               # %lor.lhs.false1283
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
.LBB34_201:                             # %lor.lhs.false1316
	testq	%rcx, %rax
	jne	.LBB34_208
	jmp	.LBB34_202
.LBB34_97:
	movq	%r11, %r15
	jmp	.LBB34_104
.LBB34_176:
	movabsq	$0, %rcx
.LBB34_178:                             # %if.then1204
	xorq	$1, %rcx
	leaq	10(%rcx,%rcx), %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rsp)
	movq	%r13, %rsi
	movq	%rax, %rcx
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_part
	movq	$1, %r14
	jmp	.LBB34_249
.LBB34_230:                             # %if.else1444
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%r11, %rbx
	testq	%r15, %rax
	je	.LBB34_231
# BB#232:                               # %if.else1444
	movabsq	$1, %rax
	jmp	.LBB34_233
.LBB34_9:
	movabsq	$0, %rax
.LBB34_173:                             # %cond.end1186
	movq	88(%rsp), %rcx
	movzbl	2(%rcx), %r9d
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%rbx, 24(%rsp)
	jmp	.LBB34_206
.LBB34_21:
	movq	%r13, %rbx
	movq	%r15, %rdi
.LBB34_23:                              # %if.end41
	movq	%rbx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	jne	.LBB34_24
# BB#26:                                # %if.then45
	movq	8(%rdi), %rax
	movl	(%rax), %r15d
	movq	%r15, %r13
	andq	$65535, %r13            # imm = 0xFFFF
	cmpq	$61, %r13
	jne	.LBB34_25
# BB#27:                                # %land.lhs.true52
	movl	8(%rax), %r12d
	cmpq	$52, %r12
	movq	$61, %r13
	ja	.LBB34_28
# BB#29:                                # %if.end86.thread
	movq	$16, %rcx
	shrq	%cl, %r15
	movq	$16, %rcx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movl	16(%rdi), %edx
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	shrq	%cl, %rbx
	andq	$255, %r15
	andq	$255, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rcx
	movq	%r8, %rbx
	callq	subreg_regno_offset
	addq	%rax, %r12
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	gen_rtx_REG
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %r8
	movq	144(%rsp), %r11
	jmp	.LBB34_30
.LBB34_24:
	andq	$65535, %r13            # imm = 0xFFFF
	movq	%rdi, %rax
.LBB34_25:                              # %if.end86
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	cmpq	$66, %r13
	movq	48(%rsp), %r15          # 8-byte Reload
	jne	.LBB34_31
	jmp	.LBB34_34
.LBB34_28:
	movq	%rdi, 56(%rsp)          # 8-byte Spill
.LBB34_30:                              # %switch.early.test
	movq	48(%rsp), %r15          # 8-byte Reload
.LBB34_31:                              # %switch.early.test
	leaq	-120(%r14), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$3, %rcx
	jb	.LBB34_34
# BB#32:                                # %switch.early.test
	cmpq	$66, %r14
	je	.LBB34_40
# BB#33:                                # %switch.early.test
	cmpq	$78, %r14
	je	.LBB34_34
# BB#38:                                # %switch.early.test9
	movq	%rdx, %rsi
	leaq	-78(%r13), %rcx
	andq	%rdi, %rcx
	cmpq	$44, %rcx
	ja	.LBB34_41
# BB#39:                                # %switch.early.test9
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$30786325577729, %rcx   # imm = 0x1C0000000001
	testq	%rcx, %rdx
	je	.LBB34_41
.LBB34_40:                              # %if.then113
	movq	88(%rsp), %rdx
	addq	$8, %rdx
	movq	%r11, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%r8, %rdi
	movq	%r15, %rcx
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	movq	%r11, %r13
	callq	find_reloads_address_1
	movq	88(%rsp), %rdx
	addq	$16, %rdx
	movq	$1, %rsi
	movq	%r13, (%rsp)
	jmp	.LBB34_35
.LBB34_143:                             # %cond.false
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$8, %rsi
	jl	.LBB34_248
# BB#144:                               # %switch.early.test41
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%r14, %rcx
	cmpq	$20, %rcx
	je	.LBB34_248
# BB#145:                               # %lor.lhs.false984
	leaq	-37(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_248
# BB#146:                               # %lor.lhs.false990
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	(%rdx,%rsi,2), %ecx
	cmpq	$44, %rcx
	jbe	.LBB34_147
.LBB34_149:                             # %if.then1008
	movq	88(%rsp), %rcx
	movq	8(%rcx), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$66, %rcx
	je	.LBB34_151
# BB#150:                               # %cond.false1021
	movq	$32, %rcx
	movq	reg_equiv_mem(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rdx,%rax,8), %rbx
.LBB34_151:                             # %cond.end
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	$4, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	testq	%rdi, %rdi
	je	.LBB34_163
# BB#152:                               # %land.lhs.true1029
	movzbl	(%rdi), %ecx
	cmpq	$32, %rcx
	jne	.LBB34_163
# BB#153:                               # %land.lhs.true1029
	testq	%rbx, %rbx
	je	.LBB34_163
# BB#154:                               # %land.lhs.true1036
	movq	optab_table(%rip), %rcx
	movslq	8(%rcx,%rax), %r12
	movzbl	2(%rbx), %esi
	movq	%rbx, %rdi
	callq	memory_operand
	testq	%r14, %rax
	je	.LBB34_163
# BB#155:                               # %land.lhs.true1042
	movq	%r12, %rax
	andq	%r14, %rax
	cmpq	$1317, %rax             # imm = 0x525
	je	.LBB34_158
# BB#156:                               # %land.lhs.true1045
	movl	target_flags(%rip), %esi
	leaq	(%r12,%r12,4), %rax
	movq	insn_data+24(,%rax,8), %r12
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%rbx, %rdi
	callq	*(%r12)
	testq	%r14, %rax
	je	.LBB34_158
# BB#157:                               # %land.lhs.true1054
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	%rbx, %rdi
	callq	*24(%r12)
	testq	%r14, %rax
	je	.LBB34_158
.LBB34_163:                             # %if.else1081
	movq	88(%rsp), %rdi
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB34_164
# BB#165:                               # %if.else1081
	movabsq	$1, %rax
	jmp	.LBB34_166
.LBB34_164:
	movabsq	$0, %rax
.LBB34_166:                             # %if.else1081
	movq	48(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rdi), %r9d
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%r9, (%rsp)
	movq	$0, 16(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$0, 8(%rsp)
	movq	%r13, %rdx
	callq	push_reload
	movq	this_insn(%rip), %rcx
	movq	32(%rcx), %rdi
	movq	(%r15), %rsi
.LBB34_167:                             # %return
	movq	%rax, %rbx
	callq	find_inc_amount
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	imulq	$104, %rbx, %rcx
	movl	%eax, rld+36(%rcx)
	movq	$1, %r14
	jmp	.LBB34_249
.LBB34_101:                             # %lor.lhs.false684
	movq	%r11, %r15
	movq	reg_renumber(%rip), %rsi
	movzbl	(%rsi,%rcx,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_103
# BB#102:                               # %lor.lhs.false684
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rsi
	jne	.LBB34_104
.LBB34_103:                             # %if.then720
	addq	$16, %rdx
	movq	$1, %rsi
	movq	%r15, (%rsp)
	movq	%r12, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_1
	movq	(%r13), %rax
.LBB34_104:                             # %if.end729
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB34_122
# BB#105:                               # %if.then737
	movq	%r12, %r13
	movslq	8(%rax), %rbx
	cmpq	$53, %rbx
	jl	.LBB34_108
# BB#106:                               # %land.lhs.true746
	movq	reg_equiv_constant(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	jne	.LBB34_107
.LBB34_108:                             # %if.end752
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	je	.LBB34_114
# BB#109:                               # %land.lhs.true757
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%rbx,8)
	jne	.LBB34_111
# BB#110:                               # %land.lhs.true757
	movl	num_not_at_initial_offset(%rip), %eax
	testq	%r14, %rax
	je	.LBB34_114
.LBB34_111:                             # %if.then763
	movq	88(%rsp), %rax
	movq	8(%rax), %rdi
	movq	%rbx, %rsi
	callq	make_memloc
	movq	reg_equiv_address(%rip), %rcx
	cmpq	$0, (%rcx,%rbx,8)
	movq	%rax, %r12
	jne	.LBB34_113
# BB#112:                               # %lor.lhs.false771
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%rbx,8), %rsi
	movq	%r12, %rdi
	callq	rtx_equal_p
	testq	%r14, %rax
	je	.LBB34_113
.LBB34_114:                             # %if.end801
	movq	reg_renumber(%rip), %rax
	movswq	(%rax,%rbx,2), %rdx
	testq	%rdx, %rdx
	jns	.LBB34_116
# BB#115:                               # %if.end801
	movq	%rbx, %rdx
.LBB34_116:                             # %if.end801
	cmpq	$8, %rdx
	jl	.LBB34_248
# BB#117:                               # %switch.early.test53
	andq	%rdx, %r14
	cmpq	$44, %r14
	ja	.LBB34_119
# BB#118:                               # %switch.early.test53
	movq	$1, %rsi
	movq	%r14, %rcx
	shlq	%cl, %rsi
	movabsq	$35046934249472, %rcx   # imm = 0x1FE000110000
	testq	%rcx, %rsi
	jne	.LBB34_248
.LBB34_119:                             # %lor.lhs.false826
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_121
# BB#120:                               # %lor.lhs.false826
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rax
	jne	.LBB34_248
.LBB34_121:                             # %if.then844
	movq	88(%rsp), %rax
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rdi
	movq	8(%rax), %rsi
	movzbl	2(%rax), %r9d
	leaq	8(%rax), %rcx
	movq	%r13, 24(%rsp)
	movq	%r9, (%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$12, %r8
	movq	$0, 8(%rsp)
	callq	push_reload
	jmp	.LBB34_248
.LBB34_113:                             # %if.then776
	movq	8(%r12), %rdx
	movzbl	2(%r12), %edi
	leaq	8(%r12), %rcx
	movq	%r15, 8(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	xorq	%rsi, %rsi
	movq	$9, %r9
	xorq	%r14, %r14
	movq	%r13, %rbx
	movq	%rbx, %r8
	callq	find_reloads_address
	movq	88(%rsp), %rdx
	movzbl	2(%rdx), %r9d
	addq	$8, %rdx
	movq	%rbx, 24(%rsp)
	movq	%r9, (%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$12, %r8
	movq	$0, 8(%rsp)
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	72(%rsp), %rcx          # 8-byte Reload
	callq	push_reload
	jmp	.LBB34_249
.LBB34_231:
	movabsq	$0, %rax
.LBB34_233:                             # %if.else1444
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r14
	movq	$12, %rsi
	movq	%r14, %rdi
	callq	reg_classes_intersect_p
	movq	88(%rsp), %rdi
	movq	8(%rdi), %rcx
	movzbl	2(%rcx), %ecx
	testq	%r15, %rax
	je	.LBB34_234
# BB#239:                               # %cond.false1471
	cmpq	$18, %rcx
	movq	$17, %rax
	movq	%rbx, %r11
	je	.LBB34_241
# BB#240:                               # %cond.false1471
	movq	%rcx, %rax
.LBB34_241:                             # %cond.false1471
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
	jmp	.LBB34_242
.LBB34_220:                             # %cond.false1403
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$8, %rsi
	movq	%rbx, %r11
	jl	.LBB34_244
# BB#221:                               # %switch.early.test51
	movq	%rax, %rcx
	orq	$4, %rcx
	andq	%rdx, %rcx
	cmpq	$20, %rcx
	je	.LBB34_244
# BB#222:                               # %lor.lhs.false1412
	leaq	-37(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_244
# BB#223:                               # %lor.lhs.false1418
	movq	$32, %rcx
	movq	reg_renumber(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rsi,%rax,2), %ecx
	cmpq	$44, %rcx
	jbe	.LBB34_224
.LBB34_226:                             # %if.then1436
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB34_227
# BB#228:                               # %if.then1436
	movabsq	$1, %rax
	jmp	.LBB34_229
.LBB34_234:                             # %cond.true1449
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	movq	%rbx, %r11
	je	.LBB34_242
# BB#235:                               # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB34_236
# BB#237:                               # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB34_242
.LBB34_158:                             # %if.then1065
	movq	88(%rsp), %rax
	movq	8(%rax), %rdi
	movq	%rdi, 88(%rsp)
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB34_159
# BB#160:                               # %if.then1065
	movabsq	$1, %rax
	jmp	.LBB34_161
.LBB34_196:                             # %cond.false1301
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$8, %rdx
	jl	.LBB34_208
# BB#197:                               # %switch.early.test46
	movq	%rdi, %rcx
	orq	$4, %rcx
	andq	%rsi, %rcx
	cmpq	$20, %rcx
	je	.LBB34_208
# BB#198:                               # %lor.lhs.false1310
	leaq	-37(%rdi), %rcx
	andq	%rsi, %rcx
	cmpq	$8, %rcx
	jae	.LBB34_199
.LBB34_208:                             # %if.end1341
	movq	88(%rsp), %rbx
	movq	%rsi, %r12
	movq	this_insn(%rip), %rsi
	movzbl	2(%rbx), %r15d
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	%r15, %rdx
	callq	regno_clobbered_p
	testq	%r12, %rax
	je	.LBB34_249
# BB#209:                               # %if.then1347
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	je	.LBB34_210
# BB#211:                               # %if.then1347
	movabsq	$1, %rax
	jmp	.LBB34_212
.LBB34_41:                              # %if.else122
	leaq	-54(%r14), %rcx
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB34_44
# BB#42:                                # %if.else122
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24593, %rdx            # imm = 0x6011
	je	.LBB34_44
# BB#43:                                # %if.then130
	movq	88(%rsp), %rdx
	addq	$16, %rdx
	movq	%r11, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	56(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB34_47
.LBB34_210:
	movabsq	$0, %rax
.LBB34_212:                             # %if.then1347
	movq	48(%rsp), %rcx          # 8-byte Reload
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$0, (%rsp)
	movq	%rbx, %rdi
	movq	%r13, %rdx
	movq	%r15, %r9
	jmp	.LBB34_207
.LBB34_159:
	movabsq	$0, %rax
.LBB34_161:                             # %if.then1065
	movq	48(%rsp), %rcx          # 8-byte Reload
	movzbl	2(%rdi), %r9d
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	%rcx, 24(%rsp)
	movq	%r9, (%rsp)
	movq	$9, 32(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	%rdi, %rsi
	movq	%r15, %rdx
	movq	%r15, %rcx
	callq	push_reload
	jmp	.LBB34_248
.LBB34_44:                              # %if.else135
	leaq	-54(%r13), %rcx
	andq	%rdi, %rcx
	cmpq	$14, %rcx
	ja	.LBB34_49
# BB#45:                                # %if.else135
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24593, %rdx            # imm = 0x6011
	je	.LBB34_49
# BB#46:                                # %if.then143
	movq	88(%rsp), %rdx
	addq	$8, %rdx
	movq	%r11, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%r8, %rdi
.LBB34_47:                              # %return
	movq	%r15, %rcx
.LBB34_48:                              # %return
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_1
	jmp	.LBB34_249
.LBB34_236:
	movabsq	$0, %rax
	incq	%rax
.LBB34_242:                             # %cond.end1500
	movq	%r14, %rcx
	andq	%r15, %rcx
	movl	reg_class_size(,%rcx,4), %ecx
	andq	%r15, %rax
	cmpq	%rcx, %rax
	jbe	.LBB34_244
# BB#243:                               # %if.then1506
	xorq	%rsi, %rsi
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%r11, %r9
	callq	find_reloads_subreg_address
	movq	%rax, 88(%rsp)
	movzbl	2(%rax), %r9d
	movq	%rbx, 32(%rsp)
	movq	%r15, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movabsq	$0, %rsi
	xorq	%rcx, %rcx
	movq	$0, (%rsp)
	movq	%rax, %rdi
	movq	%r13, %rdx
	movq	%r14, %r8
	jmp	.LBB34_207
.LBB34_244:                             # %sw.epilog
	movzbl	rtx_length(%r12), %ebx
	testq	%rbx, %rbx
	je	.LBB34_248
# BB#245:                               # %for.body.lr.ph
	movq	rtx_format(,%r12,8), %r14
	movq	$3, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	decq	%rbx
	movq	48(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB34_246:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14,%rbx), %eax
	cmpq	$101, %rax
	jne	.LBB34_247
# BB#250:                               # %if.then1528
                                        #   in Loop: Header=BB34_246 Depth=1
	movq	88(%rsp), %rdx
	movq	(%rdx,%r15), %rdi
	addq	%r15, %rdx
	movq	%r11, (%rsp)
	movq	56(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	%r11, %r13
	callq	find_reloads_address_1
	movq	%r13, %r11
.LBB34_247:                             # %for.cond.backedge
                                        #   in Loop: Header=BB34_246 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$-8, %r15
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB34_246
	jmp	.LBB34_248
.LBB34_227:
	movabsq	$0, %rax
.LBB34_229:                             # %if.then1436
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	88(%rsp), %rdi
	jmp	.LBB34_205
.LBB34_49:                              # %if.else148
	cmpq	$61, %r14
	jne	.LBB34_93
# BB#50:                                # %if.else148
	cmpq	$61, %r13
	jne	.LBB34_93
# BB#51:                                # %if.then152
	movl	8(%rsi), %r9d
	cmpq	$44, %r9
	ja	.LBB34_53
# BB#52:                                # %if.then152
	movq	$1, %rsi
	movq	%r9, %rcx
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rsi
	jne	.LBB34_56
.LBB34_53:                              # %lor.lhs.false167
	movq	reg_renumber(%rip), %r10
	movzbl	(%r10,%r9,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_55
# BB#54:                                # %lor.lhs.false167
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rsi
	je	.LBB34_55
.LBB34_56:                              # %land.lhs.true192
	movq	%r8, %rbx
	movl	8(%rax), %esi
	cmpq	$20, %rsi
	ja	.LBB34_58
# BB#57:                                # %land.lhs.true192
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	$1114367, %rax          # imm = 0x1100FF
	jne	.LBB34_248
.LBB34_58:                              # %lor.lhs.false210
	leaq	-37(%rsi), %rax
	andq	%rdi, %rax
	cmpq	$8, %rax
	jb	.LBB34_248
# BB#59:                                # %lor.lhs.false222
	movq	reg_renumber(%rip), %r10
	movzbl	(%r10,%rsi,2), %eax
	cmpq	$44, %rax
	ja	.LBB34_61
# BB#60:                                # %lor.lhs.false222
	movq	$1, %r8
	movq	%rax, %rcx
	shlq	%cl, %r8
	movabsq	$35046933135615, %rax   # imm = 0x1FE0000000FF
	testq	%rax, %r8
	movabsq	$0, %r14
	jne	.LBB34_249
	jmp	.LBB34_61
.LBB34_93:                              # %if.else592
	cmpq	$61, %r14
	jne	.LBB34_94
.LBB34_34:                              # %if.then95
	movq	88(%rsp), %rdx
	addq	$8, %rdx
	movq	$1, %rsi
	movq	%r11, (%rsp)
	movq	%r8, %rdi
	movq	%r15, %rcx
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	movq	%r11, %r14
	callq	find_reloads_address_1
	movq	88(%rsp), %rdx
	addq	$16, %rdx
	movq	%r14, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
.LBB34_35:                              # %return
	movq	56(%rsp), %rdi          # 8-byte Reload
.LBB34_36:                              # %return
	movq	%r15, %rcx
	movq	%rbx, %r8
.LBB34_37:                              # %return
	movq	%r12, %r9
	callq	find_reloads_address_1
	jmp	.LBB34_249
.LBB34_147:                             # %lor.lhs.false990
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	jmp	.LBB34_148
.LBB34_199:                             # %lor.lhs.false1316
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx,2), %ecx
	cmpq	$44, %rcx
	jbe	.LBB34_200
.LBB34_202:                             # %if.then1334
	movq	88(%rsp), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	48(%rsp), %rcx          # 8-byte Reload
	je	.LBB34_203
# BB#204:                               # %if.then1334
	movabsq	$1, %rax
	jmp	.LBB34_205
.LBB34_203:
	movabsq	$0, %rax
.LBB34_205:                             # %if.then1334
	movzbl	2(%rdi), %r9d
	xorq	$1, %rax
	leaq	10(%rax,%rax), %r8
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%rcx, 24(%rsp)
.LBB34_206:                             # %return
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	$0, (%rsp)
	movq	%r13, %rdx
.LBB34_207:                             # %return
	callq	push_reload
	movq	$1, %r14
	jmp	.LBB34_249
.LBB34_94:                              # %if.else604
	movq	%r8, 40(%rsp)           # 8-byte Spill
	xorq	%r14, %r14
	cmpq	$61, %r13
	jne	.LBB34_249
# BB#95:                                # %if.then607
	movq	88(%rsp), %rdx
	addq	$16, %rdx
	movq	$1, %rsi
	movq	%r11, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rcx
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	movq	%r11, %r14
	callq	find_reloads_address_1
	movq	88(%rsp), %rdx
	addq	$8, %rdx
	movq	%r14, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	40(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB34_36
.LBB34_224:                             # %lor.lhs.false1418
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	jmp	.LBB34_225
.LBB34_55:                              # %lor.lhs.false167.if.else250_crit_edge
	movq	%r8, %rbx
	movl	8(%rax), %esi
.LBB34_61:                              # %if.else250
	movq	%rsi, %rax
	andq	%rdi, %rax
	cmpq	$44, %rax
	ja	.LBB34_63
# BB#62:                                # %if.else250
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rdx
	jne	.LBB34_65
.LBB34_63:                              # %lor.lhs.false268
	movzbl	(%r10,%rax,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_70
# BB#64:                                # %lor.lhs.false268
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135487, %rcx   # imm = 0x1FE00000007F
	testq	%rcx, %rdx
	je	.LBB34_70
.LBB34_65:                              # %land.lhs.true295
	movq	%r11, %r8
	movq	%r15, %r11
	cmpq	$20, %r9
	ja	.LBB34_67
# BB#66:                                # %land.lhs.true295
	movq	$1, %rdx
	movq	%r9, %rcx
	shlq	%cl, %rdx
	testq	$1114367, %rdx          # imm = 0x1100FF
	jne	.LBB34_248
.LBB34_67:                              # %lor.lhs.false313
	leaq	-37(%r9), %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_248
# BB#68:                                # %lor.lhs.false325
	movzbl	(%r10,%r9,2), %ecx
	cmpq	$44, %rcx
	movq	%r11, %r15
	movq	%r8, %r11
	movabsq	$0, %r14
	ja	.LBB34_70
# BB#69:                                # %lor.lhs.false325
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rdx
	jne	.LBB34_249
.LBB34_70:                              # %if.else353
	movq	%r11, %r8
	movq	%r15, %r11
	cmpq	$20, %rax
	ja	.LBB34_72
# BB#71:                                # %if.else353
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$1114367, %rdx          # imm = 0x1100FF
	jne	.LBB34_75
.LBB34_72:                              # %lor.lhs.false371
	addq	$-37, %rsi
	andq	%rdi, %rsi
	cmpq	$8, %rsi
	jb	.LBB34_75
# BB#73:                                # %lor.lhs.false383
	movzbl	(%r10,%rax,2), %esi
	cmpq	$44, %rsi
	ja	.LBB34_77
# BB#74:                                # %lor.lhs.false383
	movq	$1, %rdx
	movq	%rsi, %rcx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	testq	%rcx, %rdx
	je	.LBB34_77
.LBB34_75:                              # %if.then410
	movq	88(%rsp), %rax
	addq	$8, %rax
	movq	$1, %rsi
	movq	%r8, (%rsp)
	movq	%rbx, %rdi
	jmp	.LBB34_76
.LBB34_200:                             # %lor.lhs.false1316
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$35046933135615, %rcx   # imm = 0x1FE0000000FF
	jmp	.LBB34_201
.LBB34_14:                              # %if.then648
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_address_1, %rcx
	movq	$5305, %rsi             # imm = 0x14B9
	jmp	.LBB34_15
.LBB34_127:                             # %if.then890
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_address_1, %rcx
	movq	$5393, %rsi             # imm = 0x1511
	jmp	.LBB34_15
.LBB34_122:                             # %if.else865
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_address_1, %rcx
	movq	$5376, %rsi             # imm = 0x1500
	jmp	.LBB34_15
.LBB34_107:                             # %if.then751
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.find_reloads_address_1, %rcx
	movq	$5324, %rsi             # imm = 0x14CC
.LBB34_15:                              # %if.then648
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB34_77:                              # %if.else415
	cmpq	$20, %r9
	ja	.LBB34_79
# BB#78:                                # %if.else415
	movq	$1, %rdx
	movq	%r9, %rcx
	shlq	%cl, %rdx
	testq	$1114367, %rdx          # imm = 0x1100FF
	jne	.LBB34_82
.LBB34_79:                              # %lor.lhs.false433
	leaq	-37(%r9), %rcx
	andq	%rdi, %rcx
	cmpq	$8, %rcx
	jb	.LBB34_82
# BB#80:                                # %lor.lhs.false445
	movzbl	(%r10,%r9,2), %ecx
	cmpq	$44, %rcx
	ja	.LBB34_83
# BB#81:                                # %lor.lhs.false445
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$35046933135615, %rdi   # imm = 0x1FE0000000FF
	testq	%rdi, %rdx
	je	.LBB34_83
.LBB34_82:                              # %if.then472
	movq	88(%rsp), %rax
	addq	$16, %rax
	movq	$1, %rsi
	movq	%r8, (%rsp)
	movq	56(%rsp), %rdi          # 8-byte Reload
.LBB34_76:                              # %return
	movq	%rax, %rdx
	movq	%r11, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	find_reloads_address_1
.LBB34_248:
	xorq	%r14, %r14
.LBB34_249:                             # %return
	movq	%r14, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_83:                              # %if.else477
	cmpq	$7, %rax
	jb	.LBB34_86
# BB#84:                                # %if.else477
	leaq	-37(%rsi), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$8, %rax
	jb	.LBB34_86
# BB#85:                                # %if.else477
	cmpq	$6, %rsi
	jbe	.LBB34_86
# BB#87:                                # %if.else527
	movq	88(%rsp), %rax
	cmpq	$7, %r9
	jb	.LBB34_90
# BB#88:                                # %if.else527
	leaq	-37(%rcx), %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$8, %rdx
	jb	.LBB34_90
# BB#89:                                # %if.else527
	cmpq	$6, %rcx
	jbe	.LBB34_90
# BB#92:                                # %if.else577
	addq	$8, %rax
	movq	$1, %rsi
	movq	%r8, %r15
	movq	%r15, (%rsp)
	movq	%rbx, %rdi
	movq	%rax, %rdx
	movq	%r11, %rbx
	movq	%rbx, %rcx
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%r13, %r8
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r9
	callq	find_reloads_address_1
	movq	88(%rsp), %rax
	addq	$16, %rax
	movq	%r15, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%r13, %r8
	jmp	.LBB34_37
.LBB34_86:                              # %if.then522
	movq	88(%rsp), %rax
	addq	$8, %rax
	movq	%r8, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%rbx, %rdi
	jmp	.LBB34_91
.LBB34_90:                              # %if.then572
	addq	$16, %rax
	movq	%r8, (%rsp)
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	56(%rsp), %rdi          # 8-byte Reload
.LBB34_91:                              # %return
	movq	%rax, %rdx
	movq	%r11, %rcx
	jmp	.LBB34_48
.Ltmp455:
	.size	find_reloads_address_1, .Ltmp455-find_reloads_address_1
	.cfi_endproc

	.align	16, 0x90
	.type	subst_indexed_address,@function
subst_indexed_address:                  # @subst_indexed_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp460:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp461:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp462:
	.cfi_def_cfa_offset 32
.Ltmp463:
	.cfi_offset %rbx, -32
.Ltmp464:
	.cfi_offset %r14, -24
.Ltmp465:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB35_25
# BB#1:                                 # %if.then
	movq	8(%rbx), %r14
	movzbl	(%r14), %eax
	movq	16(%rbx), %r15
	cmpq	$61, %rax
	jne	.LBB35_6
# BB#2:                                 # %land.lhs.true
	movslq	8(%r14), %rcx
	cmpq	$53, %rcx
	jl	.LBB35_6
# BB#3:                                 # %land.lhs.true10
	movq	reg_renumber(%rip), %rdx
	movswq	(%rdx,%rcx,2), %rdx
	testq	%rdx, %rdx
	jns	.LBB35_6
# BB#4:                                 # %land.lhs.true14
	movq	reg_equiv_constant(%rip), %rdx
	movq	(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB35_6
# BB#5:
	movq	%rcx, %r14
	movq	%r15, %rsi
	jmp	.LBB35_16
.LBB35_6:                               # %if.else
	movl	(%r15), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB35_10
# BB#7:                                 # %land.lhs.true26
	movslq	8(%r15), %rdx
	cmpq	$53, %rdx
	jl	.LBB35_10
# BB#8:                                 # %land.lhs.true32
	movq	reg_renumber(%rip), %rsi
	movswq	(%rsi,%rdx,2), %rsi
	testq	%rsi, %rsi
	jns	.LBB35_10
# BB#9:                                 # %land.lhs.true38
	movq	reg_equiv_constant(%rip), %rsi
	movq	(%rsi,%rdx,8), %rsi
	testq	%rsi, %rsi
	jne	.LBB35_16
.LBB35_10:                              # %if.else46
	cmpq	$75, %rax
	jne	.LBB35_14
# BB#11:                                # %land.lhs.true51
	movq	%r14, %rdi
	callq	subst_indexed_address
	cmpq	%r14, %rax
	je	.LBB35_13
# BB#12:
	movq	%rax, %r14
	movq	%r15, %rsi
	jmp	.LBB35_16
.LBB35_13:                              # %land.lhs.true51.if.else55_crit_edge
	movl	(%r15), %ecx
.LBB35_14:                              # %if.else55
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB35_25
# BB#15:                                # %land.lhs.true60
	movq	%r15, %rdi
	callq	subst_indexed_address
	movq	%rax, %rsi
	cmpq	%r15, %rsi
	je	.LBB35_25
.LBB35_16:                              # %if.end68
	movzbl	(%rsi), %eax
	cmpq	$75, %rax
	jne	.LBB35_18
# BB#17:                                # %if.then73
	movq	16(%rsi), %rcx
	addq	$8, %rsi
	movq	%rsi, %rax
	movq	%rcx, %rsi
	jmp	.LBB35_20
.LBB35_18:                              # %if.else80
	movzbl	(%r14), %eax
	cmpq	$75, %rax
	jne	.LBB35_23
# BB#19:                                # %if.then85
	movq	8(%r14), %rcx
	movq	%r14, %rax
	addq	$16, %rax
	movq	%rcx, %r14
.LBB35_20:                              # %if.end93
	movq	(%rax), %rdi
	testq	%rsi, %rsi
	je	.LBB35_21
# BB#22:                                # %if.then96
	callq	form_sum
	movq	%rax, %rsi
	jmp	.LBB35_23
.LBB35_21:
	movq	%rdi, %rsi
.LBB35_23:                              # %if.end98
	testq	%rsi, %rsi
	je	.LBB35_24
# BB#26:                                # %if.then101
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	form_sum  # TAILCALL
.LBB35_24:
	movq	%r14, %rbx
.LBB35_25:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp466:
	.size	subst_indexed_address, .Ltmp466-subst_indexed_address
	.cfi_endproc

	.align	16, 0x90
	.type	find_inc_amount,@function
find_inc_amount:                        # @find_inc_amount
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp473:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp474:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp475:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp476:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp477:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp478:
	.cfi_def_cfa_offset 64
.Ltmp479:
	.cfi_offset %rbx, -48
.Ltmp480:
	.cfi_offset %r12, -40
.Ltmp481:
	.cfi_offset %r13, -32
.Ltmp482:
	.cfi_offset %r14, -24
.Ltmp483:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movl	(%rdi), %edx
	movq	%rsi, %rbx
	movq	%rdx, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$66, %rax
	jne	.LBB36_10
# BB#1:                                 # %if.then
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	8(%rcx), %rcx
	movl	(%rcx), %esi
	movq	%rsi, %rdi
	andq	$65532, %rdi            # imm = 0xFFFC
	cmpq	$96, %rdi
	jne	.LBB36_4
# BB#2:                                 # %land.lhs.true
	movq	8(%rcx), %rdi
	cmpq	%rbx, %rdi
	je	.LBB36_3
.LBB36_4:                               # %if.else
	andq	$65534, %rsi            # imm = 0xFFFE
	cmpq	$100, %rsi
	jne	.LBB36_10
# BB#5:                                 # %land.lhs.true32
	movq	16(%rcx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$75, %rsi
	jne	.LBB36_10
# BB#6:                                 # %land.lhs.true40
	movq	8(%rcx), %rcx
	movq	8(%rdx), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB36_10
# BB#7:                                 # %land.lhs.true40
	cmpq	%rbx, %rcx
	jne	.LBB36_10
# BB#8:                                 # %land.lhs.true58
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$54, %rdx
	jne	.LBB36_10
# BB#9:                                 # %if.then69
	movslq	8(%rcx), %rax
	movq	$31, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	addq	%rdx, %rax
	xorq	%rdx, %rax
	jmp	.LBB36_21
.LBB36_10:                              # %if.end82
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB36_11
# BB#12:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB36_13:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB36_19 Depth 2
	movq	%r15, %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movzbl	-1(%rcx,%rax), %ecx
	leaq	-1(%rax), %r15
	cmpq	$69, %rcx
	jne	.LBB36_14
# BB#18:                                # %if.then111
                                        #   in Loop: Header=BB36_13 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %r12
	movslq	(%r12), %r14
	.align	16, 0x90
.LBB36_19:                              # %for.cond116
                                        #   Parent Loop BB36_13 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB36_16
# BB#20:                                # %for.body119
                                        #   in Loop: Header=BB36_19 Depth=2
	movq	(%r12,%r14,8), %rdi
	decq	%r14
	movq	%rbx, %rsi
	callq	find_inc_amount
	testq	%r13, %rax
	je	.LBB36_19
	jmp	.LBB36_21
	.align	16, 0x90
.LBB36_14:                              # %for.body
                                        #   in Loop: Header=BB36_13 Depth=1
	cmpq	$101, %rcx
	jne	.LBB36_16
# BB#15:                                # %if.then96
                                        #   in Loop: Header=BB36_13 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	%rbx, %rsi
	callq	find_inc_amount
	testq	%r13, %rax
	jne	.LBB36_21
.LBB36_16:                              # %for.cond.backedge
                                        #   in Loop: Header=BB36_13 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB36_13
# BB#17:
	xorq	%rax, %rax
	jmp	.LBB36_21
.LBB36_11:
	xorq	%rax, %rax
	jmp	.LBB36_21
.LBB36_3:                               # %if.then19
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movzbl	mode_size(%rdx), %eax
.LBB36_21:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp484:
	.size	find_inc_amount, .Ltmp484-find_inc_amount
	.cfi_endproc

	.align	16, 0x90
	.type	find_reloads_subreg_address,@function
find_reloads_subreg_address:            # @find_reloads_subreg_address
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp491:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp492:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp493:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp494:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp495:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp496:
	.cfi_def_cfa_offset 112
.Ltmp497:
	.cfi_offset %rbx, -48
.Ltmp498:
	.cfi_offset %r12, -40
.Ltmp499:
	.cfi_offset %r13, -32
.Ltmp500:
	.cfi_offset %r14, -24
.Ltmp501:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movslq	8(%rax), %r14
	movq	reg_equiv_memory_loc(%rip), %rax
	cmpq	$0, (%rax,%r14,8)
	movq	%rsi, %r12
	je	.LBB37_16
# BB#1:                                 # %if.then
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%r8, 32(%rsp)           # 8-byte Spill
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r12
	jne	.LBB37_5
# BB#2:                                 # %land.lhs.true
	movq	reg_equiv_address(%rip), %rax
	cmpq	$0, (%rax,%r14,8)
	jne	.LBB37_4
# BB#3:                                 # %lor.lhs.false
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r14,8), %rdi
	callq	offsettable_memref_p
	xorq	%r12, %r12
	testq	%r15, %rax
	jne	.LBB37_5
.LBB37_4:                               # %if.then11
	movq	$1, %r12
.LBB37_5:                               # %if.end
	movl	num_not_at_initial_offset(%rip), %eax
	orq	%r12, %rax
	testq	%r15, %rax
	je	.LBB37_16
# BB#6:                                 # %if.then15
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	make_memloc
	movq	%rax, %r13
	testq	%r15, %r12
	movq	%r13, 56(%rsp)
	jne	.LBB37_9
# BB#7:                                 # %lor.lhs.false21
	movq	reg_equiv_mem(%rip), %rax
	movq	(%rax,%r14,8), %rsi
	movq	%r13, %rdi
	callq	rtx_equal_p
	testq	%r15, %rax
	jne	.LBB37_16
# BB#8:                                 # %lor.lhs.false21.if.then26_crit_edge
	movq	56(%rsp), %r13
.LBB37_9:                               # %if.then26
	movq	8(%r13), %rdi
	movslq	16(%rbx), %rsi
	callq	plus_constant_wide
	movq	%rax, 8(%r13)
	movq	%rbx, %r12
	movl	(%r12), %eax
	movq	56(%rsp), %rdx
	movq	$16, %rcx
	shrq	%cl, %rax
	movb	%al, 2(%rdx)
	movq	56(%rsp), %rcx
	movzbl	2(%rcx), %edi
	movq	8(%rcx), %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%rax, %r15
	movq	$4, %r9
	cmpq	$3, %r15
	je	.LBB37_12
# BB#10:                                # %cond.false
	cmpq	$5, %r15
	movq	$6, %r9
	je	.LBB37_12
# BB#11:                                # %cond.false
	movq	%rax, %r9
.LBB37_12:                              # %cond.end118
	addq	$8, %rcx
	movq	48(%rsp), %r14          # 8-byte Reload
	movq	%r14, 8(%rsp)
	leaq	56(%rsp), %rsi
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	callq	find_reloads_address
	movl	replace_reloads(%rip), %eax
	testq	%rax, %rax
	je	.LBB37_15
# BB#13:                                # %land.lhs.true122
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	recog_data(,%rbx,8), %rax
	cmpq	%r12, %rax
	je	.LBB37_15
# BB#14:                                # %if.then127
	movq	8(%r12), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	$2, %rcx
	movb	%cl, 2(%rax)
.LBB37_15:                              # %if.end136
	movq	56(%rsp), %rbx
.LBB37_16:                              # %if.end139
	movq	%rbx, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp502:
	.size	find_reloads_subreg_address, .Ltmp502-find_reloads_subreg_address
	.cfi_endproc

	.type	secondary_memlocs_elim,@object # @secondary_memlocs_elim
	.local	secondary_memlocs_elim
	.comm	secondary_memlocs_elim,14160,16
	.type	secondary_memlocs,@object # @secondary_memlocs
	.local	secondary_memlocs
	.comm	secondary_memlocs,472,16
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"reload.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.push_reload,@object # @__FUNCTION__.push_reload
.L__FUNCTION__.push_reload:
	.asciz	"push_reload"
	.size	.L__FUNCTION__.push_reload, 12

	.type	this_insn_is_asm,@object # @this_insn_is_asm
	.local	this_insn_is_asm
	.comm	this_insn_is_asm,4,4
	.type	this_insn,@object       # @this_insn
	.local	this_insn
	.comm	this_insn,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"cannot reload integer constant operand in `asm'"
	.size	.L.str1, 48

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"impossible register constraint in `asm'"
	.size	.L.str2, 40

	.type	n_reloads,@object       # @n_reloads
	.comm	n_reloads,4,4
	.type	rld,@object             # @rld
	.comm	rld,18720,16
	.type	replace_reloads,@object # @replace_reloads
	.local	replace_reloads
	.comm	replace_reloads,4,4
	.type	n_replacements,@object  # @n_replacements
	.local	n_replacements
	.comm	n_replacements,4,4
	.type	replacements,@object    # @replacements
	.local	replacements
	.comm	replacements,3600,16
	.type	static_reload_reg_p,@object # @static_reload_reg_p
	.local	static_reload_reg_p
	.comm	static_reload_reg_p,8,8
	.type	output_reloadnum,@object # @output_reloadnum
	.local	output_reloadnum
	.comm	output_reloadnum,4,4
	.type	n_earlyclobbers,@object # @n_earlyclobbers
	.comm	n_earlyclobbers,4,4
	.type	reload_earlyclobbers,@object # @reload_earlyclobbers
	.comm	reload_earlyclobbers,240,16
	.type	.L__FUNCTION__.operands_match_p,@object # @__FUNCTION__.operands_match_p
.L__FUNCTION__.operands_match_p:
	.asciz	"operands_match_p"
	.size	.L__FUNCTION__.operands_match_p, 17

	.type	hard_regs_live_known,@object # @hard_regs_live_known
	.local	hard_regs_live_known
	.comm	hard_regs_live_known,4,4
	.type	reload_n_operands,@object # @reload_n_operands
	.comm	reload_n_operands,4,4
	.type	.L__FUNCTION__.find_reloads,@object # @__FUNCTION__.find_reloads
.L__FUNCTION__.find_reloads:
	.asciz	"find_reloads"
	.size	.L__FUNCTION__.find_reloads, 13

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"`&' constraint used with no register class"
	.size	.L.str3, 43

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"unable to generate reloads for:"
	.size	.L.str4, 32

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"inconsistent operand constraints in an `asm'"
	.size	.L.str5, 45

	.type	.L__FUNCTION__.subst_reloads,@object # @__FUNCTION__.subst_reloads
.L__FUNCTION__.subst_reloads:
	.asciz	"subst_reloads"
	.size	.L__FUNCTION__.subst_reloads, 14

	.type	.L__FUNCTION__.copy_replacements,@object # @__FUNCTION__.copy_replacements
.L__FUNCTION__.copy_replacements:
	.asciz	"copy_replacements"
	.size	.L__FUNCTION__.copy_replacements, 18

	.type	.L__FUNCTION__.refers_to_regno_for_reload_p,@object # @__FUNCTION__.refers_to_regno_for_reload_p
.L__FUNCTION__.refers_to_regno_for_reload_p:
	.asciz	"refers_to_regno_for_reload_p"
	.size	.L__FUNCTION__.refers_to_regno_for_reload_p, 29

	.type	.L__FUNCTION__.reg_overlap_mentioned_for_reload_p,@object # @__FUNCTION__.reg_overlap_mentioned_for_reload_p
.L__FUNCTION__.reg_overlap_mentioned_for_reload_p:
	.asciz	"reg_overlap_mentioned_for_reload_p"
	.size	.L__FUNCTION__.reg_overlap_mentioned_for_reload_p, 35

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Reload %d: "
	.size	.L.str6, 12

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"reload_in (%s) = "
	.size	.L.str7, 18

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\n\t"
	.size	.L.str8, 3

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"reload_out (%s) = "
	.size	.L.str9, 19

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%s, "
	.size	.L.str10, 5

	.type	reg_class_names,@object # @reg_class_names
	.section	.rodata,"a",@progbits
	.align	16
reg_class_names:
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.size	reg_class_names, 200

	.type	.L.str11,@object        # @.str11
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str11:
	.asciz	"%s (opnum = %d)"
	.size	.L.str11, 16

	.type	reload_when_needed_name,@object # @reload_when_needed_name
	.section	.rodata,"a",@progbits
	.align	16
reload_when_needed_name:
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.size	reload_when_needed_name, 88

	.type	.L.str12,@object        # @.str12
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str12:
	.asciz	", optional"
	.size	.L.str12, 11

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	", nongroup"
	.size	.L.str13, 11

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	", inc by %d"
	.size	.L.str14, 12

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	", can't combine"
	.size	.L.str15, 16

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	", secondary_reload_p"
	.size	.L.str16, 21

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"\n\treload_in_reg: "
	.size	.L.str17, 18

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"\n\treload_out_reg: "
	.size	.L.str18, 19

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"\n\treload_reg_rtx: "
	.size	.L.str19, 19

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"%ssecondary_in_reload = %d"
	.size	.L.str20, 27

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	", "
	.size	.L.str21, 3

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"%ssecondary_out_reload = %d\n"
	.size	.L.str22, 29

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"%ssecondary_in_icode = %s"
	.size	.L.str23, 26

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"%ssecondary_out_icode = %s"
	.size	.L.str24, 27

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"RELOAD_FOR_INPUT"
	.size	.L.str26, 17

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"RELOAD_FOR_OUTPUT"
	.size	.L.str27, 18

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"RELOAD_FOR_INSN"
	.size	.L.str28, 16

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"RELOAD_FOR_INPUT_ADDRESS"
	.size	.L.str29, 25

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"RELOAD_FOR_INPADDR_ADDRESS"
	.size	.L.str30, 27

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"RELOAD_FOR_OUTPUT_ADDRESS"
	.size	.L.str31, 26

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"RELOAD_FOR_OUTADDR_ADDRESS"
	.size	.L.str32, 27

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"RELOAD_FOR_OPERAND_ADDRESS"
	.size	.L.str33, 27

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"RELOAD_FOR_OPADDR_ADDR"
	.size	.L.str34, 23

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"RELOAD_OTHER"
	.size	.L.str35, 13

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"RELOAD_FOR_OTHER_ADDRESS"
	.size	.L.str36, 25

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"NO_REGS"
	.size	.L.str37, 8

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"AREG"
	.size	.L.str38, 5

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"DREG"
	.size	.L.str39, 5

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"CREG"
	.size	.L.str40, 5

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"BREG"
	.size	.L.str41, 5

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"SIREG"
	.size	.L.str42, 6

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"DIREG"
	.size	.L.str43, 6

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"AD_REGS"
	.size	.L.str44, 8

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"Q_REGS"
	.size	.L.str45, 7

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"NON_Q_REGS"
	.size	.L.str46, 11

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"INDEX_REGS"
	.size	.L.str47, 11

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"LEGACY_REGS"
	.size	.L.str48, 12

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"GENERAL_REGS"
	.size	.L.str49, 13

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"FP_TOP_REG"
	.size	.L.str50, 11

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"FP_SECOND_REG"
	.size	.L.str51, 14

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"FLOAT_REGS"
	.size	.L.str52, 11

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"SSE_REGS"
	.size	.L.str53, 9

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"MMX_REGS"
	.size	.L.str54, 9

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"FP_TOP_SSE_REGS"
	.size	.L.str55, 16

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"FP_SECOND_SSE_REGS"
	.size	.L.str56, 19

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"FLOAT_SSE_REGS"
	.size	.L.str57, 15

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"FLOAT_INT_REGS"
	.size	.L.str58, 15

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"INT_SSE_REGS"
	.size	.L.str59, 13

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"FLOAT_INT_SSE_REGS"
	.size	.L.str60, 19

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"ALL_REGS"
	.size	.L.str61, 9

	.type	subst_reg_equivs_changed,@object # @subst_reg_equivs_changed
	.local	subst_reg_equivs_changed
	.comm	subst_reg_equivs_changed,1,1
	.type	.L__FUNCTION__.find_reloads_address_1,@object # @__FUNCTION__.find_reloads_address_1
.L__FUNCTION__.find_reloads_address_1:
	.asciz	"find_reloads_address_1"
	.size	.L__FUNCTION__.find_reloads_address_1, 23

	.type	.L__FUNCTION__.find_reloads_toplev,@object # @__FUNCTION__.find_reloads_toplev
.L__FUNCTION__.find_reloads_toplev:
	.asciz	"find_reloads_toplev"
	.size	.L__FUNCTION__.find_reloads_toplev, 20

	.type	.L__FUNCTION__.immune_p,@object # @__FUNCTION__.immune_p
.L__FUNCTION__.immune_p:
	.asciz	"immune_p"
	.size	.L__FUNCTION__.immune_p, 9

	.type	.L__FUNCTION__.decompose,@object # @__FUNCTION__.decompose
.L__FUNCTION__.decompose:
	.asciz	"decompose"
	.size	.L__FUNCTION__.decompose, 10

	.type	.L__FUNCTION__.push_secondary_reload,@object # @__FUNCTION__.push_secondary_reload
.L__FUNCTION__.push_secondary_reload:
	.asciz	"push_secondary_reload"
	.size	.L__FUNCTION__.push_secondary_reload, 22

	.type	.L__FUNCTION__.find_valid_class,@object # @__FUNCTION__.find_valid_class
.L__FUNCTION__.find_valid_class:
	.asciz	"find_valid_class"
	.size	.L__FUNCTION__.find_valid_class, 17


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
