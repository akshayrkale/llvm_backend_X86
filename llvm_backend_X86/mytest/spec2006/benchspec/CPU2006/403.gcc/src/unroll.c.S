	.file	"unroll.c.bc"
	.text
	.globl	unroll_loop
	.align	16, 0x90
	.type	unroll_loop,@function
unroll_loop:                            # @unroll_loop
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
	subq	$304, %rsp              # imm = 0x130
.Ltmp11:
	.cfi_def_cfa_offset 352
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
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$50, %rax
	jle	.LBB0_1
# BB#7:                                 # %if.then
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#8:                                 # %if.then3
	movabsq	$.L.str, %rdi
	movq	$33, %rsi
	jmp	.LBB0_9
.LBB0_1:                                # %for.cond.preheader
	movq	%rsi, 264(%rsp)         # 8-byte Spill
	movq	192(%rbx), %r14
	movq	200(%rbx), %r13
	movq	160(%rbx), %r11
	cmpq	%r13, %r14
	je	.LBB0_18
# BB#2:
	movq	%rdx, %r15
	movq	%rbx, %r10
	xorq	%rax, %rax
	movabsq	$4294967199, %rdx       # imm = 0xFFFFFF9F
	movabsq	$4294967198, %rsi       # imm = 0xFFFFFF9E
	movabsq	$4294967294, %r8        # imm = 0xFFFFFFFE
	movabsq	$4294967210, %r9        # imm = 0xFFFFFFAA
	movq	%r14, %rdi
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %ebx
	cmpq	$37, %rbx
	jne	.LBB0_14
# BB#4:                                 # %if.then7
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	40(%rdi), %ebx
	cmpq	%rdx, %rbx
	jne	.LBB0_5
# BB#13:                                # %if.then14
                                        #   in Loop: Header=BB0_3 Depth=1
	incq	%rax
	jmp	.LBB0_14
	.align	16, 0x90
.LBB0_5:                                # %if.then7
                                        #   in Loop: Header=BB0_3 Depth=1
	cmpq	%rsi, %rbx
	jne	.LBB0_10
# BB#6:                                 # %if.then9
                                        #   in Loop: Header=BB0_3 Depth=1
	incq	%rcx
	jmp	.LBB0_14
.LBB0_10:                               # %if.end17
                                        #   in Loop: Header=BB0_3 Depth=1
	andq	%r8, %rbx
	cmpq	%r9, %rbx
	je	.LBB0_11
	.align	16, 0x90
.LBB0_14:                               # %for.inc
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	24(%rdi), %rdi
	cmpq	%r13, %rdi
	jne	.LBB0_3
# BB#15:                                # %for.end
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	movq	%r10, %rbx
	movq	%r15, %rdx
	jne	.LBB0_16
.LBB0_18:                               # %if.end41
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB0_20
# BB#19:                                # %if.then43
	movq	$0, 96(%r11)
.LBB0_20:                               # %if.end44
	movq	%rdx, 248(%rsp)         # 8-byte Spill
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	movq	%r11, %r12
	je	.LBB0_23
# BB#21:                                # %land.lhs.true
	cmpq	$0, 96(%r12)
	je	.LBB0_23
# BB#22:                                # %if.then48
	movabsq	$.L.str3, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	loop_dump_stream(%rip), %rdi
	movq	96(%r12), %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	loop_dump_stream(%rip), %rcx
	movabsq	$.L.str5, %rdi
	movq	$13, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB0_23:                               # %if.end53
	movq	%r14, 240(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	callq	prev_nonnote_insn
	movq	96(%r12), %r15
	movq	%r12, 152(%rsp)         # 8-byte Spill
	movq	%rax, 224(%rsp)         # 8-byte Spill
	testq	%r15, %r15
	je	.LBB0_62
# BB#24:                                # %if.end53
	cmpq	$1, %r15
	jne	.LBB0_44
# BB#25:                                # %if.then57
	movq	176(%rbx), %r14
	movq	192(%rbx), %rdi
	movq	%rbx, %r12
	callq	next_nonnote_insn
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	pc_set
	testq	%rax, %rax
	je	.LBB0_32
# BB#26:                                # %if.end.i
	movq	16(%rax), %r15
	testq	%r15, %r15
	je	.LBB0_32
# BB#27:                                # %if.end4.i
	movq	%r14, %rdi
	callq	next_nonnote_insn
	testq	%rax, %rax
	je	.LBB0_32
# BB#28:                                # %lor.lhs.false.i
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB0_32
# BB#29:                                # %if.end8.i
	movq	8(%r15), %rcx
	movl	48(%rax), %eax
	movl	48(%rcx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB0_32
# BB#30:                                # %if.end8.i
	testq	%rbx, %rbx
	je	.LBB0_32
# BB#31:                                # %if.then61
	movq	%rbx, %rdi
	callq	delete_related_insns
.LBB0_32:                               # %if.end63
	movq	224(%rsp), %rdi         # 8-byte Reload
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB0_33
# BB#35:                                # %if.then76
	callq	delete_related_insns
	movq	240(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_36
.LBB0_16:                               # %if.then36
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#17:                                # %if.then38
	movabsq	$.L.str2, %rdi
	movq	$43, %rsi
	jmp	.LBB0_9
.LBB0_44:                               # %land.lhs.true102
	cmpq	$99, %r15
	ja	.LBB0_48
# BB#45:                                # %land.lhs.true105
	movq	$32, %rcx
	movq	264(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%r15, %rax
	cmpq	$99, %rax
	ja	.LBB0_47
# BB#46:                                # %if.then109
	movq	$0, 264(%rsp)           # 8-byte Folded Spill
	xorq	%rcx, %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB0_68
.LBB0_33:                               # %if.end63
	cmpq	$35, %rax
	movq	240(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_36
# BB#34:                                # %if.then67
	movq	16(%rdi), %rdi
	callq	delete_related_insns
.LBB0_36:                               # %if.end79
	movq	168(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB0_38
# BB#37:                                # %if.then81
	callq	delete_related_insns
.LBB0_38:                               # %if.end84
	movq	176(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB0_40
# BB#39:                                # %if.then87
	callq	delete_related_insns
.LBB0_40:                               # %if.end90
	testq	%rbx, %rbx
	je	.LBB0_42
# BB#41:                                # %if.then92
	movq	%rbx, %rdi
	callq	delete_related_insns
.LBB0_42:                               # %if.end94
	testq	%r13, %r13
	je	.LBB0_88
# BB#43:                                # %if.then96
	movq	%r13, %rdi
	addq	$304, %rsp              # imm = 0x130
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	delete_related_insns  # TAILCALL
.LBB0_11:                               # %if.then26
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#12:                                # %if.then28
	movabsq	$.L.str1, %rdi
	movq	$45, %rsi
	jmp	.LBB0_9
.LBB0_47:                               # %if.else112
	testq	%r15, %r15
	je	.LBB0_62
.LBB0_48:                               # %for.cond117.preheader
	xorq	%rax, %rax
	movl	%eax, factors+4(%rip)
	movl	%eax, factors+12(%rip)
	movl	%eax, factors+20(%rip)
	movl	%eax, factors+28(%rip)
	movq	factors+24(%rip), %rsi
	movq	%rsi, 256(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rsi, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%r14
	movq	$32, %rcx
	shrq	%cl, %rsi
	testq	%rdx, %rdx
	je	.LBB0_50
# BB#49:
	movq	%r13, 232(%rsp)         # 8-byte Spill
	jmp	.LBB0_52
	.align	16, 0x90
.LBB0_50:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	incq	%rsi
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%r14
	xorq	%rdx, %rdx
	movq	%rax, %r15
                                        # kill: RAX<def> R15<kill>
	divq	%r14
	testq	%rdx, %rdx
	je	.LBB0_50
# BB#51:                                # %while.cond.for.inc143_crit_edge
	movq	%r13, 232(%rsp)         # 8-byte Spill
	movl	%esi, factors+28(%rip)
.LBB0_52:                               # %for.inc143
	movq	factors+16(%rip), %rdi
	movq	%rdi, 208(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%r13
	movq	$32, %rcx
	movq	%rdi, %r9
	shrq	%cl, %r9
	testq	%rdx, %rdx
	je	.LBB0_536
# BB#53:
	movq	%rbx, 216(%rsp)         # 8-byte Spill
	jmp	.LBB0_538
	.align	16, 0x90
.LBB0_536:                              # %while.body.1
                                        # =>This Inner Loop Header: Depth=1
	incq	%r9
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%r13
	xorq	%rdx, %rdx
	movq	%rax, %r15
                                        # kill: RAX<def> R15<kill>
	divq	%r13
	testq	%rdx, %rdx
	je	.LBB0_536
# BB#537:                               # %while.cond.for.inc143_crit_edge.1
	movq	%rbx, 216(%rsp)         # 8-byte Spill
	movl	%r9d, factors+20(%rip)
.LBB0_538:                              # %for.inc143.1
	movq	factors+8(%rip), %rdi
	movq	%rdi, 200(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%rbx
	movq	$32, %rcx
	movq	%rdi, %r10
	shrq	%cl, %r10
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%rdx, %rdx
	jne	.LBB0_541
	.align	16, 0x90
.LBB0_539:                              # %while.body.2
                                        # =>This Inner Loop Header: Depth=1
	incq	%r10
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%rbx
	xorq	%rdx, %rdx
	movq	%rax, %r15
                                        # kill: RAX<def> R15<kill>
	divq	%rbx
	testq	%rdx, %rdx
	je	.LBB0_539
# BB#540:                               # %while.cond.for.inc143_crit_edge.2
	movl	%r10d, factors+12(%rip)
.LBB0_541:                              # %for.inc143.2
	movq	factors(%rip), %r8
	movq	$32, %rcx
	movq	%r8, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%rdi
	movq	$32, %rcx
	movq	%r8, %r11
	shrq	%cl, %r11
	testq	%rdx, %rdx
	jne	.LBB0_544
	.align	16, 0x90
.LBB0_542:                              # %while.body.3
                                        # =>This Inner Loop Header: Depth=1
	incq	%r11
	xorq	%rdx, %rdx
	movq	%r15, %rax
	divq	%rdi
	xorq	%rdx, %rdx
	movq	%rax, %r15
                                        # kill: RAX<def> R15<kill>
	divq	%rdi
	testq	%rdx, %rdx
	je	.LBB0_542
# BB#543:                               # %while.cond.for.inc143_crit_edge.3
	movl	%r11d, factors+4(%rip)
.LBB0_544:                              # %for.inc143.3
	movq	$32, %rcx
	movq	264(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%r12, %rsi
	leaq	-1(%rsi), %rcx
	movl	%ecx, factors+28(%rip)
	movq	$1, %r15
	je	.LBB0_545
# BB#54:
	movq	256(%rsp), %rsi         # 8-byte Reload
	.align	16, 0x90
.LBB0_55:                               # %while.body156
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rax
	imulq	%rdx, %rax
	cmpq	$99, %rax
	ja	.LBB0_56
# BB#57:                                # %if.then164
                                        #   in Loop: Header=BB0_55 Depth=1
	imulq	%rsi, %r15
	testq	%r12, %rcx
	leaq	-1(%rcx), %rcx
	movl	%ecx, factors+28(%rip)
	movq	%rax, %rdx
	jne	.LBB0_55
	jmp	.LBB0_58
.LBB0_545:
	movq	%rdx, %rax
	jmp	.LBB0_58
.LBB0_56:
	movq	%rdx, %rax
.LBB0_58:                               # %for.inc177
	testq	%r12, %r9
	leaq	-1(%r9), %rdx
	movl	%edx, factors+20(%rip)
	je	.LBB0_59
# BB#519:
	movq	208(%rsp), %rsi         # 8-byte Reload
	.align	16, 0x90
.LBB0_520:                              # %while.body156.1
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rcx
	imulq	%rax, %rcx
	cmpq	$99, %rcx
	ja	.LBB0_521
# BB#522:                               # %if.then164.1
                                        #   in Loop: Header=BB0_520 Depth=1
	imulq	%rsi, %r15
	testq	%r12, %rdx
	leaq	-1(%rdx), %rdx
	movl	%edx, factors+20(%rip)
	movq	%rcx, %rax
	jne	.LBB0_520
	jmp	.LBB0_523
.LBB0_59:
	movq	%rax, %rcx
	jmp	.LBB0_523
.LBB0_521:
	movq	%rax, %rcx
.LBB0_523:                              # %for.inc177.1
	testq	%r12, %r10
	leaq	-1(%r10), %rdx
	movl	%edx, factors+12(%rip)
	je	.LBB0_524
# BB#525:
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	200(%rsp), %rsi         # 8-byte Reload
	.align	16, 0x90
.LBB0_526:                              # %while.body156.2
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	imulq	%rcx, %rax
	cmpq	$99, %rax
	ja	.LBB0_527
# BB#528:                               # %if.then164.2
                                        #   in Loop: Header=BB0_526 Depth=1
	imulq	%rsi, %r15
	testq	%r12, %rdx
	leaq	-1(%rdx), %rdx
	movl	%edx, factors+12(%rip)
	movq	%rax, %rcx
	jne	.LBB0_526
	jmp	.LBB0_529
.LBB0_524:
	movq	%rcx, %rax
	movq	232(%rsp), %r13         # 8-byte Reload
	jmp	.LBB0_529
.LBB0_527:
	movq	%rcx, %rax
.LBB0_529:                              # %for.inc177.2
	testq	%r12, %r11
	leaq	-1(%r11), %rcx
	movl	%ecx, factors+4(%rip)
	je	.LBB0_530
# BB#531:
	movq	216(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB0_532:                              # %while.body156.3
                                        # =>This Inner Loop Header: Depth=1
	imulq	%rdi, %rax
	cmpq	$99, %rax
	ja	.LBB0_534
# BB#533:                               # %if.then164.3
                                        #   in Loop: Header=BB0_532 Depth=1
	imulq	%r8, %r15
	testq	%r12, %rcx
	leaq	-1(%rcx), %rcx
	movl	%ecx, factors+4(%rip)
	jne	.LBB0_532
	jmp	.LBB0_534
.LBB0_530:
	movq	216(%rsp), %rbx         # 8-byte Reload
.LBB0_534:                              # %for.inc177.3
	movq	%r15, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB0_535
# BB#60:                                # %if.then182
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_62
# BB#61:                                # %if.then184
	movabsq	$.L.str6, %rdi
	movq	$34, %rsi
	movq	$1, %rdx
	callq	fwrite
.LBB0_62:                               # %if.then194
	movq	$35, %rcx
	movq	264(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$100, %rax
	movq	$8, %r15
	movq	$2, %rcx
	jge	.LBB0_64
# BB#63:
	movq	$1, 264(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB0_68
.LBB0_64:                               # %if.else199
	movq	$34, %rcx
	shlq	%cl, %rdx
	movq	$34, %rcx
	sarq	%cl, %rdx
	cmpq	$25, %rdx
	jge	.LBB0_65
# BB#66:                                # %if.else199
	movabsq	$1, %rax
	jmp	.LBB0_67
.LBB0_65:
	movabsq	$0, %rax
.LBB0_67:                               # %if.else199
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	$2, %rcx
	movq	$1, 264(%rsp)           # 8-byte Folded Spill
	leaq	2(%rax,%rax), %r15
	jmp	.LBB0_68
.LBB0_535:
	movq	$0, 264(%rsp)           # 8-byte Folded Spill
	movq	$1, %rcx
.LBB0_68:                               # %if.end207
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_70
# BB#69:                                # %if.then209
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%rcx, %r14
	callq	fprintf
	movq	%r14, %rcx
.LBB0_70:                               # %if.end211
	testq	$2, %rcx
	jne	.LBB0_75
# BB#71:
	movq	%rcx, %rdx
	movq	240(%rsp), %rax         # 8-byte Reload
	jmp	.LBB0_72
.LBB0_74:                               # %while.body227
                                        #   in Loop: Header=BB0_72 Depth=1
	movq	%rcx, %rdx
	movq	24(%rax), %rax
.LBB0_72:                               # %while.cond218
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB0_88
# BB#73:                                # %while.cond218
                                        #   in Loop: Header=BB0_72 Depth=1
	cmpq	$36, %rcx
	movq	%rdx, %rcx
	jne	.LBB0_74
.LBB0_75:                               # %if.end238
	movq	%rbx, 216(%rsp)         # 8-byte Spill
	movq	%rcx, %rdx
	andq	%r12, %rdx
	cmpq	$1, %rdx
	jne	.LBB0_76
# BB#84:                                # %if.then275
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	224(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$33, %rax
	jne	.LBB0_85
# BB#87:                                # %if.then292
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	movq	%rcx, %rbx
	movq	240(%rsp), %rax         # 8-byte Reload
	jmp	.LBB0_101
.LBB0_76:                               # %if.end238
	testq	%r12, %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	224(%rsp), %rcx         # 8-byte Reload
	jne	.LBB0_89
# BB#77:                                # %if.then241
	movzbl	(%rcx), %eax
	movq	24(%rcx), %rbx
	cmpq	$33, %rax
	jne	.LBB0_78
# BB#80:                                # %if.then261
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rax
	jmp	.LBB0_81
.LBB0_85:                               # %if.then275
	cmpq	$35, %rax
	jne	.LBB0_82
# BB#86:                                # %if.then280
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rbx
	movq	16(%rbx), %rax
	jmp	.LBB0_81
.LBB0_89:                               # %if.else303
	movzbl	(%rcx), %eax
	cmpq	$33, %rax
	jne	.LBB0_90
# BB#92:                                # %if.then320
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	24(%rcx), %rbx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	jmp	.LBB0_93
.LBB0_78:                               # %if.then241
	cmpq	$35, %rax
	jne	.LBB0_82
# BB#79:                                # %if.then249
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rax
	movq	16(%rax), %rax
.LBB0_81:                               # %if.end363
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%r14, %r14
	movq	240(%rsp), %rax         # 8-byte Reload
	jmp	.LBB0_101
.LBB0_90:                               # %if.else303
	cmpq	$35, %rax
	jne	.LBB0_82
# BB#91:                                # %if.then308
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rbx
	movq	16(%rbx), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
.LBB0_93:                               # %if.end330
	callq	gen_label_rtx
	movq	%rax, %r14
	movq	240(%rsp), %rax         # 8-byte Reload
	xorq	%rdi, %rdi
	jmp	.LBB0_94
.LBB0_82:                               # %if.else265
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#83:                                # %if.then267
	movabsq	$.L.str8, %rdi
	movq	$55, %rsi
	jmp	.LBB0_9
.LBB0_97:                               # %while.body343
                                        #   in Loop: Header=BB0_94 Depth=1
	movq	24(%rax), %rax
.LBB0_94:                               # %while.cond332
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB0_98
# BB#95:                                # %while.cond332
                                        #   in Loop: Header=BB0_94 Depth=1
	cmpq	$36, %rcx
	jne	.LBB0_97
# BB#96:
	movq	240(%rsp), %rax         # 8-byte Reload
	movq	224(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_101
.LBB0_98:                               # %if.then352
	movq	loop_dump_stream(%rip), %rcx
	movq	24(%rax), %r12
	testq	%rcx, %rcx
	movq	$1, %rdi
	je	.LBB0_100
# BB#99:                                # %if.then357
	movabsq	$.L.str9, %rdi
	movq	$53, %rsi
	movq	$1, %rdx
	callq	fwrite
	movq	$1, %rdi
.LBB0_100:                              # %if.end363
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	%r12, %rax
.LBB0_101:                              # %if.end363
	movq	%rax, 200(%rsp)         # 8-byte Spill
	leaq	24(%rax), %rdx
	movq	%rdx, %rsi
	jmp	.LBB0_102
	.align	16, 0x90
.LBB0_104:                              # %while.body372
                                        #   in Loop: Header=BB0_102 Depth=1
	addq	$24, %rsi
.LBB0_102:                              # %while.cond367
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rsi
	movzbl	(%rsi), %eax
	cmpq	$36, %rax
	je	.LBB0_107
# BB#103:                               # %while.cond367
                                        #   in Loop: Header=BB0_102 Depth=1
	cmpq	$37, %rax
	je	.LBB0_104
# BB#105:                               # %if.then381
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#106:                               # %if.then383
	movabsq	$.L.str10, %rdi
	movq	$66, %rsi
	jmp	.LBB0_9
.LBB0_107:                              # %if.end386
	movq	%r14, 168(%rsp)         # 8-byte Spill
	cmpq	$0, 56(%rsi)
	je	.LBB0_110
# BB#108:                               # %if.then390
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#109:                               # %if.then392
	movabsq	$.L.str11, %rdi
	movq	$44, %rsi
.LBB0_9:                                # %if.then3
	movq	$1, %rdx
	addq	$304, %rsp              # imm = 0x130
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB0_110:                              # %if.end395
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	cmpq	$0, 264(%rsp)           # 8-byte Folded Reload
	je	.LBB0_111
# BB#120:                               # %land.lhs.true398
	movzbl	(%rcx), %eax
	cmpq	$35, %rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	jne	.LBB0_124
# BB#121:                               # %land.lhs.true403
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	movq	16(%rcx), %rcx
	movzbl	(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB0_113
# BB#122:                               # %land.lhs.true411
	movq	64(%rcx), %rcx
	cmpq	%rcx, %rsi
	je	.LBB0_113
# BB#123:                               # %if.then420
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	24(%rcx), %rdx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movq	80(%rsp), %rdi          # 8-byte Reload
.LBB0_124:                              # %land.lhs.true427
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	cmpq	$33, %rax
	jne	.LBB0_112
# BB#125:                               # %land.lhs.true432
	movq	64(%rcx), %rax
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	cmpq	%rax, %rsi
	je	.LBB0_113
# BB#126:                               # %if.then438
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_88
# BB#127:                               # %if.then440
	movabsq	$.L.str12, %rdi
	movq	$62, %rsi
	jmp	.LBB0_9
.LBB0_88:                               # %return
	addq	$304, %rsp              # imm = 0x130
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_111:
	movq	%rdi, 80(%rsp)          # 8-byte Spill
.LBB0_112:                              # %if.end443
	movq	%rcx, 224(%rsp)         # 8-byte Spill
.LBB0_113:                              # %if.end443
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	callq	max_label_num
	movq	%rax, %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	get_max_uid
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	$1, %rdi
	movq	$648, %rsi              # imm = 0x288
	callq	xcalloc
	movq	%rax, %r14
	movq	$0, 256(%rsp)           # 8-byte Folded Spill
	testq	%rbx, %rbx
	jle	.LBB0_115
# BB#114:                               # %if.then449
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 40(%r14)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 256(%rsp)         # 8-byte Spill
.LBB0_115:                              # %if.end454
	movq	%r12, 144(%rsp)         # 8-byte Spill
	movq	%r14, %r12
	cmpq	%r13, 200(%rsp)         # 8-byte Folded Reload
	movq	%r13, 232(%rsp)         # 8-byte Spill
	je	.LBB0_133
# BB#116:                               # %for.body458.lr.ph
	movabsq	$0, %r14
	movq	$1, %r13
	movq	200(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB0_117:                              # %for.body458
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_141 Depth 2
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB0_118
# BB#128:                               # %if.then474
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	64(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB0_129
# BB#137:                               # %if.else492
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	32(%rbx), %r8
	movl	(%r8), %ecx
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	jne	.LBB0_130
# BB#138:                               # %if.then508
                                        #   in Loop: Header=BB0_117 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$45, %rcx
	movq	%r14, %r9
	jne	.LBB0_140
# BB#139:                               # %if.then508
                                        #   in Loop: Header=BB0_117 Depth=1
	movabsq	$1, %r9
.LBB0_140:                              # %if.then508
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	8(%r8,%r9,8), %rdi
	movslq	(%rdi), %rdx
	testq	%rdx, %rdx
	movq	$1, %rsi
	jle	.LBB0_130
	jmp	.LBB0_141
	.align	16, 0x90
.LBB0_142:                              # %for.body525.for.body525_crit_edge
                                        #   in Loop: Header=BB0_141 Depth=2
	movq	8(%r8,%r9,8), %rdi
	incq	%rsi
.LBB0_141:                              # %for.body525
                                        #   Parent Loop BB0_117 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdi,%rsi,8), %rdi
	movq	8(%rdi), %rdi
	movslq	48(%rdi), %rax
	movq	40(%r12), %rcx
	movq	%rdi, (%rcx,%rax,8)
	movq	%rsi, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdi
	andq	%rdi, %rax
	movq	%rdx, %rcx
	andq	%rdi, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_142
	jmp	.LBB0_130
.LBB0_118:                              # %for.body458
                                        #   in Loop: Header=BB0_117 Depth=1
	cmpq	$36, %rax
	jne	.LBB0_130
# BB#119:                               # %if.then463
                                        #   in Loop: Header=BB0_117 Depth=1
	movslq	48(%rbx), %rax
	movq	256(%rsp), %rcx         # 8-byte Reload
	movb	%r13b, (%rcx,%rax)
	jmp	.LBB0_130
.LBB0_129:                              # %if.then479
                                        #   in Loop: Header=BB0_117 Depth=1
	movslq	48(%rax), %rcx
	movq	40(%r12), %rdx
	movq	%rax, (%rdx,%rcx,8)
.LBB0_130:                              # %if.end547
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	$13, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB0_132
# BB#131:                               # %if.then550
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	8(%rax), %rax
	movslq	48(%rax), %rcx
	movq	40(%r12), %rdx
	movq	%rax, (%rdx,%rcx,8)
.LBB0_132:                              # %for.inc564
                                        #   in Loop: Header=BB0_117 Depth=1
	movq	24(%rbx), %rbx
	movq	232(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rbx
	jne	.LBB0_117
.LBB0_133:                              # %for.end568
	movq	$32, %rcx
	movq	160(%rsp), %rdi         # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	movq	%rdi, 160(%rsp)         # 8-byte Spill
	callq	xmalloc
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movq	%rax, 48(%r12)
	movq	$0, 80(%r12)
	callq	max_reg_num
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, 120(%rsp)         # 8-byte Spill
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, splittable_regs(%rip)
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, splittable_regs_updates(%rip)
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, addr_combined_regs(%rip)
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	176(%rsp), %rcx         # 8-byte Reload
	movslq	8(%rcx), %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	movq	%rax, 264(%rsp)         # 8-byte Spill
	cmpq	%rdx, %rcx
	jge	.LBB0_174
# BB#134:                               # %if.then586
	movq	200(%rsp), %rdi         # 8-byte Reload
	movslq	8(%rdi), %rax
	cmpq	%rdx, %rax
	jge	.LBB0_135
# BB#143:                               # %cond.true602
	movq	uid_luid(%rip), %rdx
	movl	(%rdx,%rax,4), %r10d
	movq	176(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	movl	(%rdx,%rcx,4), %r11d
	cmpq	$33, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB0_145
# BB#144:                               # %cond.true602
	movq	%rdx, %rcx
.LBB0_145:                              # %cond.true602
	movq	240(%rsp), %rsi         # 8-byte Reload
	cmpq	%rsi, %rdi
	jne	.LBB0_147
# BB#146:                               # %cond.true602
	movq	%rdx, %rax
.LBB0_147:                              # %cond.true602
	movl	max_reg_before_loop(%rip), %edx
	cmpq	$54, %rdx
	jb	.LBB0_174
# BB#148:
	subq	%rcx, %r11
	movq	%r11, 112(%rsp)         # 8-byte Spill
	addq	%rax, %r10
	movq	%r10, 104(%rsp)         # 8-byte Spill
	movq	$53, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB0_149:                              # %for.body626
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_155 Depth 2
                                        #     Child Loop BB0_163 Depth 2
	movq	reg_n_info(%rip), %rax
	movq	%rbx, %r13
	andq	%r12, %r13
	movq	32(%rax,%r13,8), %rdx
	movslq	(%rdx), %rax
	testq	%rax, %rax
	jle	.LBB0_173
# BB#150:                               # %land.lhs.true631
                                        #   in Loop: Header=BB0_149 Depth=1
	movslq	max_uid_for_loop(%rip), %rsi
	cmpq	%rsi, %rax
	jg	.LBB0_173
# BB#151:                               # %land.lhs.true639
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	uid_luid(%rip), %r8
	movslq	(%r8,%rax,4), %r9
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %r9
	jl	.LBB0_173
# BB#152:                               # %land.lhs.true649
                                        #   in Loop: Header=BB0_149 Depth=1
	movslq	4(%rdx), %r9
	testq	%r9, %r9
	jle	.LBB0_173
# BB#153:                               # %land.lhs.true649
                                        #   in Loop: Header=BB0_149 Depth=1
	cmpq	%rsi, %r9
	jg	.LBB0_173
# BB#154:                               # %land.lhs.true664
                                        #   in Loop: Header=BB0_149 Depth=1
	movslq	(%r8,%r9,4), %r8
	movq	$32, %rcx
	movq	%r11, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdx, %rdx
	cmpq	%rsi, %r8
	movq	200(%rsp), %r14         # 8-byte Reload
	jg	.LBB0_173
.LBB0_155:                              # %while.cond.i
                                        #   Parent Loop BB0_149 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r14), %r14
	movl	8(%r14), %edi
	movzbl	(%r14), %ecx
	movq	%rax, %rsi
	andq	%r12, %rsi
	cmpq	%rsi, %rdi
	je	.LBB0_159
# BB#156:                               # %while.body.i
                                        #   in Loop: Header=BB0_155 Depth=2
	cmpq	$33, %rcx
	movq	$1, %rcx
	je	.LBB0_158
# BB#157:                               # %while.body.i
                                        #   in Loop: Header=BB0_155 Depth=2
	movq	%rdx, %rcx
.LBB0_158:                              # %while.body.i
                                        #   in Loop: Header=BB0_155 Depth=2
	movq	176(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, %r14
	movq	%rcx, %rdx
	jne	.LBB0_155
	jmp	.LBB0_168
.LBB0_159:                              # %while.end.i
                                        #   in Loop: Header=BB0_149 Depth=1
	movzbl	rtx_class(%rcx), %eax
	cmpq	$105, %rax
	jne	.LBB0_168
# BB#160:                               # %lor.lhs.false.i1730
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	%r9, 88(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rdx, %r12
	callq	dead_or_set_regno_p
	movq	%r12, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	104(%rsp), %r10         # 8-byte Reload
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	112(%rsp), %r11         # 8-byte Reload
	testq	%r12, %rax
	je	.LBB0_168
# BB#161:                               # %if.end16.i
                                        #   in Loop: Header=BB0_149 Depth=1
	testq	%r12, %rdx
	je	.LBB0_167
# BB#162:                               # %while.cond21.preheader.i
                                        #   in Loop: Header=BB0_149 Depth=1
	movl	8(%r14), %eax
	andq	%r12, %rcx
	jmp	.LBB0_163
.LBB0_166:                              # %if.end37.i
                                        #   in Loop: Header=BB0_163 Depth=2
	movq	24(%r14), %r14
	movl	8(%r14), %eax
.LBB0_163:                              # %while.cond21.preheader.i
                                        #   Parent Loop BB0_149 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rcx, %rax
	je	.LBB0_167
# BB#164:                               # %while.body27.i
                                        #   in Loop: Header=BB0_163 Depth=2
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	je	.LBB0_168
# BB#165:                               # %if.else.i
                                        #   in Loop: Header=BB0_163 Depth=2
	movq	176(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	jne	.LBB0_166
.LBB0_167:                              # %if.then687
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	264(%rsp), %rax         # 8-byte Reload
	movq	$1, %rcx
	movb	%cl, (%rax,%r13)
.LBB0_168:                              # %if.end690
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_173
# BB#169:                               # %if.then692
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	%r11, %r12
	movq	%r10, %r14
	movq	264(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax,%r13), %eax
	testq	%rax, %rax
	jne	.LBB0_170
# BB#171:                               # %if.else698
                                        #   in Loop: Header=BB0_149 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str15, %rsi
	jmp	.LBB0_172
.LBB0_170:                              # %if.then696
                                        #   in Loop: Header=BB0_149 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str14, %rsi
.LBB0_172:                              # %for.inc703
                                        #   in Loop: Header=BB0_149 Depth=1
	movq	%rbx, %rdx
	callq	fprintf
	movq	%r14, %r10
	movq	%r12, %r11
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB0_173:                              # %for.inc703
                                        #   in Loop: Header=BB0_149 Depth=1
	movl	max_reg_before_loop(%rip), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jb	.LBB0_149
.LBB0_174:                              # %if.end706
	cmpq	$2, 184(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	jne	.LBB0_176
# BB#175:                               # %if.end706
	movq	%rdx, %rcx
.LBB0_176:                              # %if.end706
	movq	80(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	216(%rsp), %rdi         # 8-byte Reload
	jne	.LBB0_178
# BB#177:                               # %if.end706
	movq	%rdx, %rax
.LBB0_178:                              # %if.end706
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	248(%rsp), %rsi         # 8-byte Reload
	testq	%rdx, %rsi
	je	.LBB0_259
# BB#179:                               # %if.end706
	andq	%rax, %rcx
	je	.LBB0_259
# BB#180:                               # %if.then713
	leaq	296(%rsp), %rsi
	leaq	288(%rsp), %rdx
	leaq	280(%rsp), %rcx
	leaq	276(%rsp), %r8
	callq	precondition_loop_p
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB0_259
# BB#181:                               # %if.then716
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	88(%rax), %esi
	leaq	-106(%rsi), %rcx
	movq	%rcx, %rax
	andq	%rdx, %rax
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	cmpq	$5, %rax
	ja	.LBB0_183
# BB#182:                               # %switch.lookup
	andq	$63, %rcx
	movq	$51, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
.LBB0_183:                              # %lor.end
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rsi, %rax
	cmpq	$108, %rax
	jne	.LBB0_184
# BB#185:                               # %lor.end
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB0_186
.LBB0_259:                              # %if.end1028
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	208(%rsp), %rcx         # 8-byte Reload
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB0_237
# BB#260:                               # %if.end1028
	movl	flag_unroll_all_loops(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB0_237
# BB#261:                               # %if.then1033
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_512
# BB#262:                               # %if.then1035
	movabsq	$.L.str17, %rdi
	movq	$51, %rsi
	movq	$1, %rdx
	callq	fwrite
	jmp	.LBB0_512
.LBB0_184:
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
.LBB0_186:                              # %lor.end
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	96(%rsp), %r12          # 8-byte Reload
	movq	%rax, 32(%r12)
	movabsq	$.L.str16, %rdx
	movq	$16, %rsi
	movq	%rbx, %rdi
	callq	varray_init
	movq	%rax, 64(%r12)
	movq	%r12, %rdi
	movq	%rax, global_const_equiv_varray(%rip)
	movq	128(%rsp), %rsi         # 8-byte Reload
	leaq	-1(%rsi), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$58, %rax
	movq	$57, %rax
	jl	.LBB0_187
# BB#188:                               # %for.body.lr.ph.i
	leaq	-1(%rsi), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	movq	%rdi, %rbx
.LBB0_189:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rsi), %rcx
	movq	32(%rbx), %rdi
	movq	%rcx, (%rdi,%rsi)
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	$-8, %rsi
	cmpq	$57, %rdi
	jg	.LBB0_189
	jmp	.LBB0_190
.LBB0_187:
	movq	%rdi, %rbx
.LBB0_190:                              # %for.body5.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rcx
	movq	$0, (%rcx,%rax,8)
	movq	$32, %rcx
	leaq	-1(%rax), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rdx, %rax
	jg	.LBB0_190
# BB#191:                               # %init_reg_map.exit
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	432(%rax), %rax
	movq	32(%rbx), %rcx
	movq	%rax, 432(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	424(%rax), %rax
	movq	32(%rbx), %rcx
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	%rax, 424(%rcx)
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$4, %rax
	jg	.LBB0_193
# BB#192:                               # %init_reg_map.exit
	movq	%r15, %rax
.LBB0_193:                              # %init_reg_map.exit
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	280(%rsp), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	8(%rax), %r13
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%r13, %rax
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$63, %rcx
	sarq	%cl, %r14
	movq	$31, %rcx
	shrq	%cl, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	callq	start_sequence
	movq	288(%rsp), %rbx
	movzbl	(%rbx), %eax
	cmpq	$75, %rax
	jne	.LBB0_195
# BB#194:                               # %if.then761
	movq	8(%rbx), %rdi
	movl	276(%rsp), %r12d
	callq	copy_rtx
	movq	16(%rbx), %rdi
	movq	%rax, %rbx
	callq	copy_rtx
	movq	$75, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, %rbx
	movq	%rbx, 288(%rsp)
.LBB0_195:                              # %if.end771
	leaq	(%r13,%r14), %r12
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	nonmemory_operand
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_197
# BB#196:                               # %if.then774
	movq	%rbx, %rdi
	movq	%r12, %rbx
	movq	%rcx, %r12
	callq	copy_rtx
	movl	276(%rsp), %edi
	movq	%rax, %rsi
	callq	force_reg
	movq	%r12, %rcx
	movq	%rbx, %r12
	movq	%rax, %rbx
	movq	%rbx, 288(%rsp)
.LBB0_197:                              # %if.end777
	movq	136(%rsp), %rax         # 8-byte Reload
	xorq	%r14, %r12
	andq	%rcx, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rcx, %r14
	cmpq	$102, %rax
	je	.LBB0_199
# BB#198:                               # %if.end777
	movq	112(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, 104(%rsp)         # 8-byte Folded Reload
	jne	.LBB0_199
# BB#200:                               # %if.else786
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$31, %rcx
	movq	296(%rsp), %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	shrq	%cl, %r13
	andq	$9, %r13
	addq	$77, %r13
	callq	copy_rtx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movl	276(%rsp), %edi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	expand_simple_unop
	jmp	.LBB0_201
.LBB0_199:                              # %if.then783
	movq	296(%rsp), %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	callq	copy_rtx
	movq	$76, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movl	276(%rsp), %edi
	movq	%rbx, %rdx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	callq	expand_simple_binop
.LBB0_201:                              # %if.end791
	movq	%r12, %rsi
	movq	%r12, 248(%rsp)         # 8-byte Spill
	movq	184(%rsp), %r13         # 8-byte Reload
	imulq	%r13, %rsi
	decq	%rsi
	movq	$32, %rcx
	movzbl	2(%rax), %r12d
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	movq	%rax, %rbx
	callq	gen_rtx_CONST_INT
	movq	$83, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	expand_simple_binop
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %r12
	testq	%rbx, %rbx
	movq	%r14, %rdx
	jle	.LBB0_206
# BB#202:                               # %for.body805.lr.ph
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	movq	%r15, %r13
	jl	.LBB0_204
# BB#203:                               # %for.body805.lr.ph
	movq	$4, %r13
.LBB0_204:                              # %for.body805.lr.ph
	movq	$1, %rbx
	andq	%rdx, %r13
.LBB0_205:                              # %for.body805
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %r14
	movq	%rdx, %r12
	callq	gen_label_rtx
	movq	%r12, %rdx
	movq	%r14, %r12
	movq	%rax, -8(%r12,%rbx,8)
	movq	%rbx, %rax
	andq	%rdx, %rax
	incq	%rbx
	cmpq	%r13, %rax
	jne	.LBB0_205
.LBB0_206:                              # %for.end811
	cmpq	$102, 136(%rsp)         # 8-byte Folded Reload
	je	.LBB0_211
# BB#207:                               # %if.then814
	movq	%rdx, %r14
	movq	$75, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movl	276(%rsp), %edi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	120(%rsp), %rcx         # 8-byte Reload
	callq	expand_simple_binop
	cmpq	$0, 104(%rsp)           # 8-byte Folded Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	jne	.LBB0_208
# BB#209:                               # %if.then814
	movabsq	$1, %rcx
	jmp	.LBB0_210
.LBB0_208:
	movabsq	$0, %rcx
.LBB0_210:                              # %if.then814
	movq	8(%r12), %rbx
	leaq	104(%rcx,%rcx), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, (%rsp)
	movq	%rax, %rdi
	movl	276(%rsp), %r8d
	movq	88(%rsp), %r9           # 8-byte Reload
	callq	emit_cmp_and_jump_insns
	callq	get_last_insn
	movq	$10, %rsi
	movq	$1, %rdx
	movq	%rax, %rdi
	callq	predict_insn_def
	callq	get_last_insn
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	movq	%r14, %rdx
.LBB0_211:                              # %for.cond831.preheader
	movq	%r12, 120(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rsi         # 8-byte Reload
	leaq	-1(%rsi), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_224
# BB#212:                               # %for.body835.lr.ph
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB0_213
# BB#214:                               # %for.body835.lr.ph
	movq	%rsi, %r12
	movabsq	$1, %rax
	jmp	.LBB0_215
.LBB0_213:
	movq	%rsi, %r12
	movabsq	$0, %rax
.LBB0_215:                              # %for.body835.lr.ph
	leaq	104(%rax,%rax), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	%r12, %rsi
	jl	.LBB0_216
# BB#217:                               # %for.body835.lr.ph
	movq	$3, %rax
	jmp	.LBB0_218
.LBB0_216:
	leaq	-1(%r15), %rax
.LBB0_218:                              # %for.body835.lr.ph
	xorq	%r14, %r14
	andq	%rdx, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB0_219:                              # %for.body835
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r13
	subq	%r14, %r13
	testq	%rdx, %r14
	movq	$103, %r12
	movabsq	$0, %rsi
	je	.LBB0_223
# BB#220:                               # %if.else839
                                        #   in Loop: Header=BB0_219 Depth=1
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	movq	%r14, %rsi
	je	.LBB0_222
# BB#221:                               # %if.else839
                                        #   in Loop: Header=BB0_219 Depth=1
	movq	%r13, %rsi
.LBB0_222:                              # %if.else839
                                        #   in Loop: Header=BB0_219 Depth=1
	movq	88(%rsp), %r12          # 8-byte Reload
.LBB0_223:                              # %if.end845
                                        #   in Loop: Header=BB0_219 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	imulq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%r14,8), %rbx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%r12, %rdx
	movl	276(%rsp), %r8d
	callq	emit_cmp_and_jump_insns
	callq	get_last_insn
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	callq	get_last_insn
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$10000, %rax            # imm = 0x2710
	xorq	%rdx, %rdx
	idivq	%r13
	movq	$11, %rsi
	movq	%rax, %rdx
	callq	predict_insn
	incq	%r14
	movq	%r14, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movq	184(%rsp), %rsi         # 8-byte Reload
	jne	.LBB0_219
.LBB0_224:                              # %for.end868
	movq	%rsi, %r12
	movq	248(%rsp), %rax         # 8-byte Reload
	andq	%rdx, %rax
	cmpq	$1, %rax
	movq	232(%rsp), %r13         # 8-byte Reload
	je	.LBB0_229
# BB#225:                               # %if.then871
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB0_227
# BB#226:                               # %if.then875
	movq	248(%rsp), %rsi         # 8-byte Reload
	decq	%rsi
	movq	$106, %r14
	jmp	.LBB0_228
.LBB0_227:                              # %if.else877
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rax         # 8-byte Reload
	imulq	%rax, %rsi
	incq	%rsi
	movq	$104, %r14
.LBB0_228:                              # %if.end880
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rbx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%r14, %rdx
	movl	276(%rsp), %r8d
	callq	emit_cmp_and_jump_insns
	callq	get_last_insn
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
.LBB0_229:                              # %if.end894
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	loop_insn_hoist
	movq	224(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %ecx
	cmpq	$33, %rcx
	je	.LBB0_232
# BB#230:                               # %if.end894
	cmpq	$35, %rcx
	jne	.LBB0_546
# BB#231:                               # %if.then901
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
.LBB0_232:                              # %if.end919
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %rdx
	jle	.LBB0_233
# BB#244:                               # %for.body923.lr.ph
	movq	%r12, 184(%rsp)         # 8-byte Spill
	movq	16(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jl	.LBB0_246
# BB#245:                               # %for.body923.lr.ph
	movq	$4, %r15
.LBB0_246:                              # %for.body923.lr.ph
	movq	240(%rsp), %rax         # 8-byte Reload
	leaq	16(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$1, %rsi
	movq	144(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r13, %rdx
	andq	%rdx, %rax
	movq	%rax, 248(%rsp)         # 8-byte Spill
	movq	128(%rsp), %r14         # 8-byte Reload
	andq	%rdx, %r14
	andq	%rdx, %r15
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB0_247:                              # %for.body923
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_256 Depth 2
                                        #     Child Loop BB0_249 Depth 2
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	144(%rsp), %r12         # 8-byte Reload
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	184(%rsp), %rax         # 8-byte Reload
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdi
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	movq	%rdx, %r13
	callq	emit_label_after
	movq	48(%rbx), %rdi
	xorq	%rsi, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
	callq	memset
	movq	64(%rbx), %rdi
	movq	(%rdi), %rdx
	addq	$32, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	%r13, %rdx
	testq	%r12, %r12
	movabsq	$0, %rax
	movl	%eax, 72(%rbx)
	movq	%rbx, %r12
	movq	$1, %rbx
	jle	.LBB0_248
	.align	16, 0x90
.LBB0_256:                              # %for.body942
                                        #   Parent Loop BB0_247 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	256(%rsp), %rax         # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_258
# BB#257:                               # %if.then946
                                        #   in Loop: Header=BB0_256 Depth=2
	movq	%rdx, %r13
	callq	gen_label_rtx
	movq	%r13, %rdx
	movq	40(%r12), %rcx
	movq	%rax, -8(%rcx,%rbx,8)
.LBB0_258:                              # %for.inc952
                                        #   in Loop: Header=BB0_256 Depth=2
	movq	%rbx, %rax
	andq	%rdx, %rax
	incq	%rbx
	movq	248(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB0_256
.LBB0_248:                              # %for.cond955.preheader
                                        #   in Loop: Header=BB0_247 Depth=1
	movq	$54, %r13
	cmpq	$53, %r14
	jbe	.LBB0_252
	.align	16, 0x90
.LBB0_249:                              # %for.body958
                                        #   Parent Loop BB0_247 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	264(%rsp), %rax         # 8-byte Reload
	movzbl	-1(%rax,%r13), %eax
	testq	%rax, %rax
	je	.LBB0_251
# BB#250:                               # %if.then962
                                        #   in Loop: Header=BB0_249 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	-8(%rax,%r13,8), %rax
	movzbl	2(%rax), %edi
	movq	%rdx, %rbx
	callq	gen_reg_rtx
	movq	32(%r12), %rcx
	movq	%rax, -8(%rcx,%r13,8)
	movq	cfun(%rip), %rax
	movq	32(%r12), %rcx
	movq	24(%rax), %rax
	movq	-8(%rcx,%r13,8), %rcx
	movq	80(%rax), %rax
	movq	-8(%rax,%r13,8), %rsi
	movl	8(%rcx), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	%rbx, %rdx
.LBB0_251:                              # %for.inc982
                                        #   in Loop: Header=BB0_249 Depth=2
	movq	%r13, %rax
	andq	%rdx, %rax
	incq	%r13
	cmpq	%r14, %rax
	jne	.LBB0_249
.LBB0_252:                              # %for.end984
                                        #   in Loop: Header=BB0_247 Depth=1
	movq	176(%rsp), %rax         # 8-byte Reload
	andq	%rdx, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_255
# BB#253:                               # %if.then988
                                        #   in Loop: Header=BB0_247 Depth=1
	movq	224(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$35, %rax
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	jne	.LBB0_255
# BB#254:                               # %if.then993
                                        #   in Loop: Header=BB0_247 Depth=1
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB0_255:                              # %if.end1002
                                        #   in Loop: Header=BB0_247 Depth=1
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, 32(%rsp)
	movq	240(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	%r13, 16(%rsp)
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	xorq	%r9, %r9
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	200(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rbx
	movq	%rbx, %rcx
	movq	168(%rsp), %r8          # 8-byte Reload
	callq	copy_loop_body
	movq	176(%rsp), %rsi         # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%r15, %rax
	jne	.LBB0_247
	jmp	.LBB0_234
.LBB0_233:                              # %if.end919.for.end1005_crit_edge
	movq	%r12, 184(%rsp)         # 8-byte Spill
	movq	240(%rsp), %rax         # 8-byte Reload
	leaq	16(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB0_234:                              # %for.end1005
	movq	%r13, %r14
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	(%rbx), %rdi
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	callq	emit_label_after
	movq	224(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$35, %rax
	movq	%rcx, %rax
	jne	.LBB0_236
# BB#235:                               # %if.then1015
	movq	16(%rcx), %rax
.LBB0_236:                              # %if.end1028.thread
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	16(%rax), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	free
	movq	$1, 208(%rsp)           # 8-byte Folded Spill
	movq	184(%rsp), %r15         # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %r13
.LBB0_237:                              # %if.end1038
	movq	%r15, 184(%rsp)         # 8-byte Spill
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	testq	%rbx, %rbx
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	%r15d, 104(%rax)
	jne	.LBB0_455
# BB#238:                               # %if.else1042
	movq	216(%rsp), %rax         # 8-byte Reload
	movq	160(%rax), %rax
	movq	224(%rax), %rbx
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	testq	%rbx, %rbx
	je	.LBB0_454
# BB#239:                               # %for.body.lr.ph.i1738
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
.LBB0_240:                              # %for.body.i1739
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_241 Depth 2
                                        #     Child Loop BB0_285 Depth 2
                                        #     Child Loop BB0_314 Depth 2
                                        #       Child Loop BB0_315 Depth 3
                                        #     Child Loop BB0_323 Depth 2
                                        #       Child Loop BB0_331 Depth 3
                                        #       Child Loop BB0_337 Depth 3
                                        #         Child Loop BB0_343 Depth 4
                                        #       Child Loop BB0_336 Depth 3
                                        #       Child Loop BB0_433 Depth 3
                                        #       Child Loop BB0_418 Depth 3
	movq	%rbx, 248(%rsp)         # 8-byte Spill
	movq	8(%rbx), %rbx
	movq	const_int_rtx+512(%rip), %r15
	jmp	.LBB0_241
.LBB0_267:                              # %if.then.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	movq	72(%rbx), %rdx
	movl	48(%rbx), %ecx
	movq	%r15, %rdi
	callq	fold_rtx_mult_add
	movq	128(%rbx), %rbx
	movq	%rax, %r15
.LBB0_241:                              # %for.body.i1739
                                        #   Parent Loop BB0_240 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_268
# BB#242:                               # %for.body.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	movzbl	100(%rbx), %eax
	testq	$8, %rax
	je	.LBB0_243
# BB#263:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	movq	64(%rbx), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB0_264
# BB#265:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB0_241 Depth=2
	andq	$32, %rax
	testq	$65535, %rax            # imm = 0xFFFF
	je	.LBB0_267
# BB#266:                               #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_268:                              # %biv_total_increment.exit.i
                                        #   in Loop: Header=BB0_240 Depth=1
	testq	%r15, %r15
	jne	.LBB0_270
# BB#269:                               #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_270:                              # %lor.lhs.false.i1744
                                        #   in Loop: Header=BB0_240 Depth=1
	movzbl	(%r15), %eax
	cmpq	$54, %rax
	jne	.LBB0_271
# BB#272:                               # %if.end.i1745
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	$1, %r14
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	208(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB0_283
# BB#273:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	216(%rsp), %rcx         # 8-byte Reload
	movl	240(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB0_275
# BB#274:                               # %land.lhs.true.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$2, %rax
	jne	.LBB0_283
.LBB0_275:                              # %land.lhs.true7.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	200(%rcx), %rax
	movslq	8(%rax), %rsi
	movslq	max_uid_for_loop(%rip), %rax
	cmpq	%rax, %rsi
	jge	.LBB0_547
# BB#276:                               # %cond.true.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %ecx
	movq	reg_n_info(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rdx
	movq	uid_luid(%rip), %rcx
	movslq	4(%rdx), %rdi
	movslq	(%rcx,%rdi,4), %rdi
	movslq	(%rcx,%rsi,4), %rsi
	cmpq	%rsi, %rdi
	jge	.LBB0_281
# BB#277:                               # %lor.lhs.false19.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rsi         # 8-byte Reload
	movq	72(%rsi), %rsi
	testq	%rsi, %rsi
	je	.LBB0_281
# BB#278:                               # %lor.lhs.false21.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movslq	8(%rsi), %rsi
	cmpq	%rax, %rsi
	jge	.LBB0_281
# BB#279:                               # %cond.true40.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movslq	(%rdx), %rax
	movslq	(%rcx,%rax,4), %rax
	movslq	(%rcx,%rsi,4), %rcx
	cmpq	%rcx, %rax
	jl	.LBB0_281
# BB#280:                               # %lor.lhs.false51.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rax
	movq	80(%rcx), %rcx
	movq	32(%rax), %rdi
	movq	16(%rcx), %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_283
.LBB0_281:                              # %land.lhs.true56.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	248(%rsp), %rsi         # 8-byte Reload
	callq	final_biv_value
	movq	%rax, 104(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB0_283
# BB#282:                               # %land.lhs.true56.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movabsq	$1, %r14
.LBB0_283:                              # %if.end60.i
                                        #   in Loop: Header=BB0_240 Depth=1
	testq	%r14, %r14
	je	.LBB0_295
# BB#284:                               # %if.end60.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rbx
	jmp	.LBB0_285
.LBB0_271:                              #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_294:                              # %for.inc.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	128(%rbx), %rbx
.LBB0_285:                              # %if.end60.i
                                        #   Parent Loop BB0_240 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB0_295
# BB#286:                               # %for.body65.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	(%rbx), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB0_287
# BB#288:                               # %cond.true72.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	32(%rdi), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB0_290
# BB#289:                               # %cond.false86.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	%rax, %rsi
	callq	single_set_2
.LBB0_290:                              # %cond.end96.i
                                        #   in Loop: Header=BB0_285 Depth=2
	testq	%rax, %rax
	je	.LBB0_287
# BB#291:                               # %lor.lhs.false100.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	jne	.LBB0_287
# BB#292:                               # %lor.lhs.false108.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movl	8(%rcx), %ecx
	movq	248(%rsp), %rdx         # 8-byte Reload
	movl	(%rdx), %edx
	cmpq	%rdx, %rcx
	jne	.LBB0_287
# BB#293:                               # %lor.lhs.false117.i
                                        #   in Loop: Header=BB0_285 Depth=2
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$75, %rax
	je	.LBB0_294
	jmp	.LBB0_287
.LBB0_295:                              # %for.end.i
                                        #   in Loop: Header=BB0_240 Depth=1
	testq	%r14, %r14
	je	.LBB0_287
# BB#296:                               # %if.then129.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, %r14
	testq	%rax, %rcx
	jne	.LBB0_306
# BB#297:                               # %if.then132.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %r12         # 8-byte Reload
	movq	40(%r12), %rsi
	movzbl	(%rsi), %eax
	cmpq	$61, %rax
	movq	216(%rsp), %r9          # 8-byte Reload
	jne	.LBB0_302
# BB#298:                               # %land.lhs.true137.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	%r9, %rbx
	movl	8(%rsi), %eax
	movl	(%r12), %ecx
	cmpq	%rcx, %rax
	je	.LBB0_303
# BB#299:                               # %land.lhs.true137.i
                                        #   in Loop: Header=BB0_240 Depth=1
	cmpq	$53, %rax
	jb	.LBB0_303
# BB#300:                               # %lor.lhs.false152.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	%rbx, %rdi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_301
.LBB0_303:                              # %if.then156.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	8(%r12), %rax
	movl	48(%rax), %edi
	callq	gen_reg_rtx
	movq	8(%r12), %rcx
	movq	%rbx, %r13
	movq	%rax, %rbx
	movq	72(%rcx), %rsi
	movl	8(%rbx), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	8(%r12), %rax
	movq	16(%rax), %rsi
	movq	%rbx, %rdi
	callq	gen_move_insn
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	loop_insn_hoist
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	movq	232(%rsp), %r13         # 8-byte Reload
	je	.LBB0_305
# BB#304:                               # %if.then168.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	8(%rbx), %ecx
	movl	(%r12), %edx
	xorq	%rax, %rax
	movabsq	$.L.str55, %rsi
	callq	fprintf
.LBB0_305:                              # %if.end174.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	(%r12), %eax
	movq	splittable_regs(%rip), %rcx
	movq	%rbx, (%rcx,%rax,8)
	jmp	.LBB0_307
.LBB0_287:                              #   in Loop: Header=BB0_240 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	216(%rsp), %r9          # 8-byte Reload
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_312
.LBB0_306:                              # %if.else183.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
	movq	248(%rsp), %r12         # 8-byte Reload
	movl	(%r12), %eax
	movq	splittable_regs(%rip), %rdx
	movq	%rcx, (%rdx,%rax,8)
.LBB0_307:                              # %if.end187.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	216(%rsp), %r9          # 8-byte Reload
	jmp	.LBB0_308
.LBB0_243:                              #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_264:                              #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_301:                              # %lor.lhs.false152.if.else_crit_edge.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	40(%r12), %rsi
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	%rbx, %r9
.LBB0_302:                              # %if.else.i1750
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	(%r12), %eax
	movq	splittable_regs(%rip), %rcx
	movq	%rsi, (%rcx,%rax,8)
.LBB0_308:                              # %if.end187.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	4(%r12), %ecx
	movq	splittable_regs_updates(%rip), %rdx
	movl	%ecx, (%rdx,%rax,4)
	movl	4(%r12), %ecx
	movq	loop_dump_stream(%rip), %rdi
	movq	120(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	jne	.LBB0_310
# BB#309:                               #   in Loop: Header=BB0_240 Depth=1
	movq	%r12, 248(%rsp)         # 8-byte Spill
	jmp	.LBB0_311
.LBB0_310:                              # %if.then193.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	(%r12), %edx
	movq	%r12, 248(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	movabsq	$.L.str56, %rsi
	movq	%r9, %rbx
	callq	fprintf
	movq	%rbx, %r9
.LBB0_311:                              # %if.end197.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	%r14, %rcx
.LBB0_312:                              # %if.end197.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %r14
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	testq	%r14, %r14
	je	.LBB0_446
# BB#313:                               #   in Loop: Header=BB0_240 Depth=1
	movq	160(%r9), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB0_314:                              # %for.body.i323.i
                                        #   Parent Loop BB0_240 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_315 Depth 3
	movq	128(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB0_321
	.align	16, 0x90
.LBB0_315:                              # %for.body4.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_314 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%r14), %rax
	movq	(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_319
# BB#316:                               # %land.lhs.true.i327.i
                                        #   in Loop: Header=BB0_315 Depth=3
	movq	8(%r14), %rdi
	movq	8(%rbx), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_319
# BB#317:                               # %land.lhs.true8.i.i
                                        #   in Loop: Header=BB0_315 Depth=3
	cmpq	$0, 152(%rbx)
	jne	.LBB0_319
# BB#318:                               # %if.then.i328.i
                                        #   in Loop: Header=BB0_315 Depth=3
	movq	%r14, 152(%rbx)
	.align	16, 0x90
.LBB0_319:                              # %for.inc.i.i
                                        #   in Loop: Header=BB0_315 Depth=3
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_315
# BB#320:                               # %for.cond.loopexit.i.i
                                        #   in Loop: Header=BB0_314 Depth=2
	movq	128(%r14), %r14
	testq	%r14, %r14
	jne	.LBB0_314
.LBB0_321:                              # %for.cond16.preheader.i.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %r8
	testq	%r8, %r8
	movq	216(%rsp), %r9          # 8-byte Reload
	movq	208(%rsp), %rcx         # 8-byte Reload
	je	.LBB0_446
# BB#322:                               # %for.body18.lr.ph.i.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB0_323
.LBB0_367:                              # %if.then160.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	136(%r8), %rcx
	testq	%rcx, %rcx
	je	.LBB0_372
# BB#368:                               # %land.lhs.true162.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$0, 8(%rcx)
	jne	.LBB0_372
# BB#369:                               # %if.then166.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	movq	%r14, %r9
	jne	.LBB0_371
# BB#370:                               #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_372:                              # %if.else171.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	24(%r8), %esi
	cmpq	$1, %rsi
	jne	.LBB0_382
# BB#373:                               # %if.then174.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	testq	%rdx, %rdx
	je	.LBB0_374
# BB#375:                               # %land.lhs.true176.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	112(%rsp), %rdx         # 8-byte Reload
	movzbl	(%rdx), %ecx
	cmpq	$54, %rcx
	je	.LBB0_381
# BB#376:                               # %land.lhs.true176.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$61, %rcx
	je	.LBB0_381
# BB#377:                               # %land.lhs.true176.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$75, %rcx
	jne	.LBB0_380
# BB#378:                               # %lor.lhs.false188.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%rdx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_380
# BB#379:                               # %lor.lhs.false195.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	16(%rdx), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$54, %rcx
	je	.LBB0_381
.LBB0_380:                              # %if.then202.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	48(%r8), %edi
	movq	%r8, %rbx
	callq	gen_reg_rtx
	movq	%rax, %r14
	movq	72(%rbx), %rsi
	movl	8(%r14), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	248(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rsi
	movq	64(%rbx), %rdx
	movq	72(%rbx), %rcx
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %r8
	callq	loop_iv_add_mult_hoist
	movq	%r14, %rdx
	movq	%rbx, %r8
	movq	8(%r8), %rdi
	jmp	.LBB0_381
.LBB0_382:                              # %if.else220.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	$0, 144(%r8)
	movq	152(%r8), %rax
	testq	%rax, %rax
	jne	.LBB0_383
# BB#400:                               # %if.else235.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	208(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	je	.LBB0_431
# BB#401:                               # %land.lhs.true237.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	testq	%rsi, %rsi
	jne	.LBB0_409
# BB#402:                               # %land.lhs.true237.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	testq	%rcx, %rcx
	je	.LBB0_409
# BB#403:                               # %land.lhs.true243.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	24(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB0_409
# BB#404:                               # %land.lhs.true247.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	101(%rcx), %eax
	testq	$2, %rax
	je	.LBB0_409
# BB#405:                               # %land.lhs.true254.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	64(%rcx), %rdi
	movq	64(%r8), %rsi
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_409
# BB#406:                               # %land.lhs.true260.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	136(%r8), %rax
	movq	72(%r8), %rcx
	movq	72(%rax), %rax
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rbx
	callq	rtx_equal_p
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_409
# BB#407:                               # %land.lhs.true266.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%r12), %rax
	movq	%r8, %rdi
	movq	%rax, %rsi
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	%r8, %rbx
	callq	verify_addresses
	movq	%rbx, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_408
.LBB0_409:                              # %if.then277.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	48(%r8), %edi
	movq	%r8, %rbx
	callq	gen_reg_rtx
	movq	136(%rbx), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	8(%rbx), %r14
	movq	72(%rbx), %rsi
	movl	8(%rax), %edi
	movq	%rax, %r13
	xorq	%rdx, %rdx
	callq	record_base_value
	movzbl	(%r14), %eax
	cmpq	$75, %rax
	jne	.LBB0_414
# BB#410:                               # %land.lhs.true292.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	16(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB0_414
# BB#411:                               # %if.then299.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%rax), %rsi
	movq	%r13, %rdi
	callq	plus_constant_wide
	movq	%rbx, %rdi
	movq	%rax, 32(%rdi)
	movq	8(%r12), %rsi
                                        # kill: RBX<def> RDI<kill>
	movq	184(%rsp), %rdx         # 8-byte Reload
	callq	verify_addresses
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_412
.LBB0_414:                              # %if.else330.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r13, 32(%rbx)
.LBB0_415:                              # %if.end332.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rax
	cmpq	%r13, %rax
	jne	.LBB0_425
# BB#416:                               # %land.lhs.true335.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%r12), %rsi
	movq	%rbx, %rdi
	movq	184(%rsp), %rdx         # 8-byte Reload
	callq	verify_addresses
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_425
# BB#417:                               # %for.cond340.preheader.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	128(%rbx), %rax
	jmp	.LBB0_418
.LBB0_374:                              #   in Loop: Header=BB0_323 Depth=2
	movq	112(%rsp), %rdx         # 8-byte Reload
.LBB0_381:                              # %if.end213.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%rdi), %eax
	movq	splittable_regs(%rip), %rcx
	movq	%rdx, (%rcx,%rax,8)
	jmp	.LBB0_394
.LBB0_383:                              # %if.then223.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	32(%rax), %rax
	movq	%rax, 32(%r8)
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_386
# BB#384:                               # %if.then228.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	(%r8), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str59, %rsi
	movq	%r8, %rbx
	callq	fprintf
.LBB0_385:                              # %if.end441.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%rbx, %r8
	jmp	.LBB0_386
.LBB0_371:                              # %if.then168.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	$43, %rsi
	movq	$1, %rdx
	movabsq	$.L.str58, %rdi
	movq	%r8, %r14
	movq	%r9, %rbx
	callq	fwrite
	movq	%rbx, %r9
	movq	%r14, %r8
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_431:                              # %if.else413.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%r8)
	movq	8(%r12), %rsi
	movq	%r8, %rdi
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	%r8, %rbx
	callq	verify_addresses
	movq	%rbx, %r8
	testq	%r14, %rax
	jne	.LBB0_386
# BB#432:                               # %for.cond419.preheader.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	128(%r8), %rax
	jmp	.LBB0_433
.LBB0_425:                              # %if.end359.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r14, 8(%rbx)
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, 136(%rbx)
	movzbl	100(%rbx), %eax
	orq	$512, %rax              # imm = 0x200
	movw	%ax, 100(%rbx)
	movq	%r14, %rdi
	callq	copy_rtx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	216(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	loop_insn_hoist
	movq	%rbx, %r8
	movq	192(%r14), %rax
	movq	16(%rax), %rdi
	movslq	40(%rdi), %rax
	testq	%rax, %rax
	movq	232(%rsp), %r13         # 8-byte Reload
	jns	.LBB0_386
# BB#426:                               # %cond.end391.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r8, %rbx
	callq	recog_memoized_1
	movq	%rbx, %r8
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB0_386
# BB#427:                               # %if.then394.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r13, %r14
	movq	216(%rsp), %rcx         # 8-byte Reload
	movq	192(%rcx), %rax
	movq	%rcx, %r13
	movq	16(%rax), %rax
	movq	%rax, %rdi
	movq	%r8, %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	callq	delete_related_insns
	callq	start_sequence
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_operand
	cmpq	%rbx, %rax
	movq	%rbx, %rcx
	je	.LBB0_429
# BB#428:                               # %if.then403.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB0_429:                              # %if.end405.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%r13, %rax
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	loop_insn_hoist
	movq	loop_dump_stream(%rip), %rax
	testq	%rax, %rax
	movq	%r14, %r13
	movq	56(%rsp), %r8           # 8-byte Reload
	je	.LBB0_386
# BB#430:                               # %if.then409.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	$30, %rsi
	movq	$1, %rdx
	movabsq	$.L.str62, %rcx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	movq	%r8, %rbx
	callq	fwrite
	jmp	.LBB0_385
.LBB0_435:                              # %for.cond419.backedge.i.i
                                        #   in Loop: Header=BB0_433 Depth=3
	movq	128(%rax), %rax
.LBB0_433:                              # %for.cond419.preheader.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB0_437
# BB#434:                               # %for.body421.i.i
                                        #   in Loop: Header=BB0_433 Depth=3
	movq	152(%rax), %rcx
	cmpq	%r8, %rcx
	jne	.LBB0_435
# BB#436:                               # %if.then424.i.i
                                        #   in Loop: Header=BB0_433 Depth=3
	movq	$0, 152(%rax)
	movq	128(%rax), %rax
	jmp	.LBB0_433
.LBB0_437:                              # %for.end429.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_438
# BB#439:                               # %if.then431.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	(%r8), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str61, %rsi
	movq	%r8, %rbx
	callq	fprintf
	movq	%rbx, %r8
.LBB0_438:                              #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	216(%rsp), %r9          # 8-byte Reload
	jmp	.LBB0_445
.LBB0_412:                              # %if.then309.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	16(%r14), %rax
	movq	8(%rax), %rax
	negq	%rax
	movq	%rax, 144(%rbx)
	movq	loop_dump_stream(%rip), %rcx
	movq	8(%r14), %r14
	testq	%rcx, %rcx
	je	.LBB0_415
# BB#413:                               # %if.then321.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%r13), %edx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str60, %rcx
	movq	%rcx, %rsi
	callq	fprintf
	jmp	.LBB0_415
.LBB0_420:                              # %for.cond340.backedge.i.i
                                        #   in Loop: Header=BB0_418 Depth=3
	movq	128(%rax), %rax
.LBB0_418:                              # %for.cond340.preheader.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rax, %rax
	je	.LBB0_422
# BB#419:                               # %for.body342.i.i
                                        #   in Loop: Header=BB0_418 Depth=3
	movq	152(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB0_420
# BB#421:                               # %if.then345.i.i
                                        #   in Loop: Header=BB0_418 Depth=3
	movq	$0, 152(%rax)
	movq	128(%rax), %rax
	jmp	.LBB0_418
.LBB0_422:                              # %for.end350.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	jne	.LBB0_424
# BB#423:                               #   in Loop: Header=BB0_323 Depth=2
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	216(%rsp), %r9          # 8-byte Reload
	jmp	.LBB0_341
.LBB0_424:                              # %if.then352.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	(%rbx), %rax
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str61, %rcx
	movq	%rcx, %rsi
	callq	fprintf
	movq	%rbx, %r8
	movq	232(%rsp), %r13         # 8-byte Reload
	movq	216(%rsp), %r9          # 8-byte Reload
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_408:                              # %if.then269.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	136(%r8), %rax
	movq	32(%rax), %rax
	movq	%rax, 32(%r8)
	movzbl	100(%r8), %eax
	orq	$1024, %rax             # imm = 0x400
	movw	%ax, 100(%r8)
	movq	232(%rsp), %r13         # 8-byte Reload
.LBB0_386:                              # %if.end441.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	32(%r8), %rax
	movq	40(%r8), %rcx
	movq	%rax, (%rcx)
	movq	136(%r8), %rax
	testq	%rax, %rax
	je	.LBB0_389
# BB#387:                               # %land.lhs.true445.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	24(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB0_389
# BB#388:                               # %if.then449.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%rax), %rcx
	movl	8(%rcx), %ecx
	movq	addr_combined_regs(%rip), %rdx
	movq	%rax, (%rdx,%rcx,8)
.LBB0_389:                              # %if.end458.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	jne	.LBB0_392
# BB#390:                               # %if.then463.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%rax), %eax
	movq	splittable_regs(%rip), %rcx
	movq	%rdx, (%rcx,%rax,8)
	cmpq	$0, 136(%r8)
	jne	.LBB0_392
# BB#391:                               # %if.then472.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r8, 136(%r8)
	movq	8(%r8), %rax
	movl	8(%rax), %eax
	movq	addr_combined_regs(%rip), %rcx
	movq	%r8, (%rcx,%rax,8)
.LBB0_392:                              # %if.end481.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB0_394
# BB#393:                               # %if.then483.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	$27, %rsi
	movq	$1, %rdx
	movabsq	$.L.str63, %rdi
	movq	%r8, %rbx
	callq	fwrite
	movq	%rbx, %r8
.LBB0_394:                              # %if.end489.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	movq	216(%rsp), %r9          # 8-byte Reload
	jne	.LBB0_398
# BB#395:                               # %if.then494.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	100(%r8), %edx
	movq	$1, %rcx
	testq	$4, %rdx
	jne	.LBB0_397
# BB#396:                               # %if.then501.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	16(%r8), %rcx
	movl	8(%rcx), %edx
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	208(%rcx), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rsi,%rdx), %rcx
	movl	4(%rcx), %ecx
.LBB0_397:                              # %if.end508.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%rax), %eax
	movq	splittable_regs_updates(%rip), %rdx
	movl	%ecx, (%rdx,%rax,4)
.LBB0_398:                              # %if.end515.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	loop_dump_stream(%rip), %rdi
	incq	120(%rsp)               # 8-byte Folded Spill
	testq	%rdi, %rdi
	jne	.LBB0_440
# BB#399:                               #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_440:                              # %if.then517.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r9, %r14
	movq	32(%r8), %rax
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	je	.LBB0_444
# BB#441:                               # %if.else523.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$61, %rcx
	je	.LBB0_443
# BB#442:                               # %if.then528.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%rax), %rax
.LBB0_443:                              # %if.else536.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%rax), %edx
.LBB0_444:                              # %if.end542.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	(%r8), %rax
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str64, %rsi
	movq	%r8, %rbx
	callq	fprintf
	movq	%rbx, %r8
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %r9
	jmp	.LBB0_445
.LBB0_323:                              # %for.body18.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_331 Depth 3
                                        #       Child Loop BB0_337 Depth 3
                                        #         Child Loop BB0_343 Depth 4
                                        #       Child Loop BB0_336 Depth 3
                                        #       Child Loop BB0_433 Depth 3
                                        #       Child Loop BB0_418 Depth 3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB0_325
# BB#324:                               # %land.lhs.true20.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	100(%r8), %eax
	testq	$4, %rax
	jne	.LBB0_445
.LBB0_325:                              # %if.end23.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r9, %r14
	movl	24(%r8), %eax
	testq	%rax, %rax
	je	.LBB0_339
# BB#326:                               # %land.lhs.true25.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	100(%r8), %eax
	testq	$8, %rax
	je	.LBB0_327
# BB#328:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	(%r8), %rbx
	movq	%r8, %r12
	movq	192(%r14), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	200(%r14), %rdi
	movq	%rdi, 112(%rsp)         # 8-byte Spill
	callq	prev_nonnote_insn
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	jne	.LBB0_330
# BB#329:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	16(%rax), %rax
.LBB0_330:                              # %while.cond.preheader.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	3(%rbx), %ecx
	testq	$8, %rcx
	movq	%r12, %r8
	je	.LBB0_332
	.align	16, 0x90
.LBB0_331:                              # %while.body.i.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rbx
	movzbl	3(%rbx), %ecx
	testq	$8, %rcx
	jne	.LBB0_331
.LBB0_332:                              # %while.end.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	%rax, %rbx
	je	.LBB0_339
# BB#333:                               # %while.end.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	112(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rbx
	je	.LBB0_339
# BB#334:                               # %for.cond.preheader.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	24(%rbx), %rcx
	cmpq	%rax, %rcx
	je	.LBB0_339
# BB#335:                               # %for.body.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	%rbx, 88(%rsp)          # 8-byte Folded Reload
	jne	.LBB0_337
.LBB0_336:                              # %for.cond.backedge.us.i.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rcx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB0_336
	jmp	.LBB0_339
.LBB0_337:                              # %for.body.i.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB0_343 Depth 4
	movzbl	(%rcx), %edx
	cmpq	$33, %rdx
	jne	.LBB0_338
# BB#342:                               # %for.body24.lr.ph.i.i.i
                                        #   in Loop: Header=BB0_337 Depth=3
	movq	64(%rcx), %rdx
	movq	88(%rsp), %rsi          # 8-byte Reload
.LBB0_343:                              # %for.body24.i.i.i
                                        #   Parent Loop BB0_240 Depth=1
                                        #     Parent Loop BB0_323 Depth=2
                                        #       Parent Loop BB0_337 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%rdx, %rsi
	je	.LBB0_327
# BB#344:                               # %for.inc.i.i.i
                                        #   in Loop: Header=BB0_343 Depth=4
	movq	24(%rsi), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB0_343
.LBB0_338:                              # %for.cond.backedge.i.i.i
                                        #   in Loop: Header=BB0_337 Depth=3
	movq	24(%rcx), %rcx
	cmpq	%rax, %rcx
	jne	.LBB0_337
.LBB0_339:                              # %if.end35.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	64(%r8), %rdi
	movq	const_int_rtx+512(%rip), %rdx
	movl	48(%r8), %ecx
	movq	%r8, %rbx
	movq	%r15, %rsi
	callq	fold_rtx_mult_add
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB0_345
# BB#340:                               #   in Loop: Header=BB0_323 Depth=2
	movq	%r14, %r9
.LBB0_341:                              # %for.inc549.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%rbx, %r8
	jmp	.LBB0_445
.LBB0_345:                              # %lor.lhs.false38.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	movq	%r14, %r9
	movq	208(%rsp), %rcx         # 8-byte Reload
	jne	.LBB0_346
# BB#347:                               # %if.end43.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	movq	%rbx, %r8
	je	.LBB0_359
# BB#348:                               # %land.lhs.true45.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	240(%r9), %eax
	testq	%rax, %rax
	jne	.LBB0_350
# BB#349:                               # %land.lhs.true45.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB0_365
.LBB0_350:                              # %land.lhs.true49.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	24(%r8), %eax
	testq	%rax, %rax
	je	.LBB0_365
# BB#351:                               # %land.lhs.true52.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	32(%r8), %rax
	movl	8(%rax), %eax
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB0_357
# BB#352:                               # %lor.lhs.false54.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rcx
	movq	(%r8), %rdi
	movl	(%rcx), %eax
	movl	8(%rdi), %edx
	cmpq	%rdx, %rax
	je	.LBB0_355
# BB#353:                               # %land.lhs.true64.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	$6, %rsi
	xorq	%rdx, %rdx
	movq	%r8, %rbx
	movq	%r9, %r14
	callq	find_reg_note
	movq	%r14, %r9
	movq	%rbx, %r8
	testq	%rax, %rax
	je	.LBB0_357
# BB#354:                               # %lor.lhs.false68.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	32(%r8), %rcx
	movl	8(%rcx), %ecx
	movq	reg_n_info(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
	movq	8(%rax), %rax
	movl	(%rcx), %edx
	movl	8(%rax), %eax
	cmpq	%rax, %rdx
	jne	.LBB0_357
.LBB0_355:                              # %lor.lhs.false84.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	200(%r9), %rax
	movslq	8(%rax), %rax
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rax
	jge	.LBB0_548
# BB#356:                               # %cond.true.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	uid_luid(%rip), %rdx
	movslq	4(%rcx), %rcx
	movslq	(%rdx,%rcx,4), %rcx
	movslq	(%rdx,%rax,4), %rax
	cmpq	%rax, %rcx
	jl	.LBB0_365
.LBB0_357:                              # %land.lhs.true106.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$0, 88(%r8)
	jne	.LBB0_365
# BB#358:                               #   in Loop: Header=BB0_323 Depth=2
	movq	208(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB0_445
.LBB0_346:                              #   in Loop: Header=BB0_323 Depth=2
	movq	%rbx, %r8
	jmp	.LBB0_445
.LBB0_359:                              # %if.then112.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r9, %r14
	movq	248(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	splittable_regs(%rip), %rcx
	movq	(%rcx,%rax,8), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_364
# BB#360:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	248(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rbx
	movzbl	(%rbx), %ecx
	cmpq	$61, %rcx
	jne	.LBB0_364
# BB#361:                               # %lor.lhs.false123.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movl	8(%rbx), %ecx
	cmpq	%rax, %rcx
	je	.LBB0_363
# BB#362:                               # %lor.lhs.false123.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	cmpq	$52, %rcx
	ja	.LBB0_364
.LBB0_363:                              # %if.else138.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r12, 112(%rsp)         # 8-byte Spill
	movq	248(%rsp), %r12         # 8-byte Reload
	movq	8(%r12), %rax
	movl	48(%rax), %edi
	movq	%r8, 56(%rsp)           # 8-byte Spill
	callq	gen_reg_rtx
	movq	8(%r12), %rcx
	movq	%rax, %rbx
	movq	72(%rcx), %rsi
	movl	8(%rbx), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	8(%r12), %rax
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	16(%rax), %rsi
	movq	%rbx, %rdi
	callq	gen_move_insn
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	loop_insn_hoist
	movq	56(%rsp), %r8           # 8-byte Reload
.LBB0_364:                              # %if.end150.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r8, %rdi
	movq	%rbx, %rsi
	movq	%r8, %rbx
	callq	extend_value_for_giv
	movq	64(%rbx), %rdi
	movq	72(%rbx), %rdx
	movl	48(%rbx), %ecx
	movq	%rax, %rsi
	callq	fold_rtx_mult_add
	movq	%rbx, %r8
	movq	$1, %rdx
	jmp	.LBB0_366
.LBB0_365:                              # %if.else156.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	%r9, %r14
	movq	const_int_rtx+512(%rip), %rax
	xorq	%rdx, %rdx
.LBB0_366:                              # %if.end157.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	8(%r8), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_367
.LBB0_327:                              #   in Loop: Header=BB0_323 Depth=2
	movq	%r14, %r9
	movq	208(%rsp), %rcx         # 8-byte Reload
.LBB0_445:                              # %for.inc549.i.i
                                        #   in Loop: Header=BB0_323 Depth=2
	movq	128(%r8), %r8
	testq	%r8, %r8
	jne	.LBB0_323
.LBB0_446:                              # %find_splittable_givs.exit.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	addq	%rax, 120(%rsp)         # 8-byte Folded Spill
	movq	104(%rsp), %r12         # 8-byte Reload
	testq	%r12, %r12
	jne	.LBB0_448
# BB#447:                               #   in Loop: Header=BB0_240 Depth=1
	movq	%r9, 216(%rsp)          # 8-byte Spill
	movq	248(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB0_453
.LBB0_448:                              # %if.then201.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	240(%r9), %ecx
	movq	248(%rsp), %rbx         # 8-byte Reload
	movq	8(%rbx), %rax
	testq	%rcx, %rcx
	movq	%r9, %r14
	jne	.LBB0_450
# BB#449:                               # %if.then204.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	16(%rax), %rdi
	movq	%r12, %rsi
	callq	gen_move_insn
	movq	%r14, %rdi
	movq	%r14, 216(%rsp)         # 8-byte Spill
	movq	%rax, %rsi
	callq	loop_insn_sink
	jmp	.LBB0_453
.LBB0_450:                              # %if.else209.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movl	48(%rax), %edi
	callq	gen_reg_rtx
	movq	8(%rbx), %rcx
	movq	%r14, %r15
	movq	%r15, 216(%rsp)         # 8-byte Spill
	movq	%rax, %r14
	movq	72(%rcx), %rsi
	movl	8(%r14), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	8(%rbx), %rax
	movq	16(%rax), %rsi
	movq	%r14, %rdi
	callq	gen_move_insn
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	loop_insn_hoist
	movq	8(%rbx), %rax
	movq	16(%rax), %rdi
	movq	%r12, %rsi
	callq	gen_move_insn
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	loop_insn_hoist
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB0_452
# BB#451:                               # %if.then228.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	8(%rbx), %rax
	movq	16(%rax), %rax
	movl	8(%r14), %ecx
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movabsq	$.L.str57, %rsi
	callq	fprintf
.LBB0_452:                              # %if.end238.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	8(%rbx), %rax
	movq	%r14, 16(%rax)
.LBB0_453:                              # %for.inc243.i
                                        #   in Loop: Header=BB0_240 Depth=1
	movq	64(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB0_240
.LBB0_454:                              # %find_splittable_regs.exit
	movq	$32, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
.LBB0_455:                              # %if.end1045
	movq	%r13, 232(%rsp)         # 8-byte Spill
	callq	max_reg_num
	movq	%rax, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rbx, %r14
	andq	%r13, %r14
	movq	$3, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	callq	xmalloc
	movq	96(%rsp), %r12          # 8-byte Reload
	movq	%rax, 32(%r12)
	leaq	-1(%rbx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$58, %rdx
	movq	$57, %rdx
	jl	.LBB0_458
# BB#456:                               # %for.body.lr.ph.i1755
	decq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	jmp	.LBB0_457
.LBB0_549:                              # %for.body.i1766.for.body.i1766_crit_edge
                                        #   in Loop: Header=BB0_457 Depth=1
	movq	32(%r12), %rax
	addq	$-8, %rbx
.LBB0_457:                              # %for.body.i1766
                                        # =>This Inner Loop Header: Depth=1
	movq	cfun(%rip), %rcx
	movq	24(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	(%rcx,%rbx), %rcx
	movq	%rcx, (%rax,%rbx)
	decq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$58, %rax
	jge	.LBB0_549
.LBB0_458:
	movq	184(%rsp), %rsi         # 8-byte Reload
	.align	16, 0x90
.LBB0_459:                              # %for.body5.i1771
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r12), %rax
	movq	$0, (%rax,%rdx,8)
	movq	$32, %rcx
	leaq	-1(%rdx), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%rax, %rdx
	jg	.LBB0_459
# BB#460:                               # %init_reg_map.exit1780
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	432(%rax), %rax
	movq	32(%r12), %rcx
	movq	%rax, 432(%rcx)
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	424(%rax), %rax
	movq	32(%r12), %rcx
	movq	%rax, 424(%rcx)
	movq	64(%r12), %rax
	testq	%rax, %rax
	movq	240(%rsp), %r15         # 8-byte Reload
	jne	.LBB0_462
# BB#461:                               # %if.then1054
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	imulq	%rcx, %rax
	leaq	(%r14,%rax,2), %rdi
	movabsq	$.L__FUNCTION__.unroll_loop, %rdx
	movq	$16, %rsi
	callq	varray_init
	movq	%rax, 64(%r12)
.LBB0_462:                              # %if.end1062
	movq	%rax, global_const_equiv_varray(%rip)
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	224(%rax), %rax
	jmp	.LBB0_463
.LBB0_466:                              # %for.inc1080
                                        #   in Loop: Header=BB0_463 Depth=1
	movq	64(%rax), %rax
.LBB0_463:                              # %if.end1062
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB0_467
# BB#464:                               # %for.body1066
                                        #   in Loop: Header=BB0_463 Depth=1
	movq	8(%rax), %rcx
	movq	16(%rcx), %rcx
	movl	8(%rcx), %esi
	movl	(%rax), %edx
	cmpq	%rdx, %rsi
	je	.LBB0_466
# BB#465:                               # %if.then1072
                                        #   in Loop: Header=BB0_463 Depth=1
	movq	32(%r12), %rsi
	movq	%rcx, (%rsi,%rdx,8)
	jmp	.LBB0_466
.LBB0_467:                              # %for.end1081
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	64(%rax), %rcx
	movq	%rcx, 120(%r12)
	movq	80(%rax), %rax
	movq	%rax, 128(%r12)
	movq	%r12, %r14
	movq	208(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB0_470
# BB#468:                               # %if.then1089
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	24(%rax), %rbx
	movl	(%rbx), %eax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$32, %rax
	jne	.LBB0_470
# BB#469:                               # %if.then1102
	movq	32(%rbx), %rsi
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	remap_split_bivs
	movq	%rax, 32(%rbx)
.LBB0_470:                              # %for.cond1112.preheader
	movq	$32, %rcx
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %r12
	jle	.LBB0_483
# BB#471:                               # %for.body1115.lr.ph
	leaq	-1(%rdx), %rax
	movq	144(%rsp), %r14         # 8-byte Reload
	andq	%r13, %r14
	andq	%r13, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	$0, 248(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_472:                              # %for.body1115
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_485 Depth 2
                                        #     Child Loop BB0_474 Depth 2
	movq	$32, %rcx
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	48(%r12), %rdi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
	callq	memset
	movq	64(%r12), %rdi
	movq	(%rdi), %rdx
	addq	$32, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movl	%eax, 72(%r12)
	movq	$1, %rbx
	movq	256(%rsp), %r15         # 8-byte Reload
	jle	.LBB0_473
	.align	16, 0x90
.LBB0_485:                              # %for.body1130
                                        #   Parent Loop BB0_472 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	-1(%r15,%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_487
# BB#486:                               # %if.then1134
                                        #   in Loop: Header=BB0_485 Depth=2
	callq	gen_label_rtx
	movq	40(%r12), %rcx
	movq	%rax, -8(%rcx,%rbx,8)
.LBB0_487:                              # %for.inc1140
                                        #   in Loop: Header=BB0_485 Depth=2
	movq	%rbx, %rax
	andq	%r13, %rax
	incq	%rbx
	cmpq	%r14, %rax
	jne	.LBB0_485
.LBB0_473:                              # %for.cond1143.preheader
                                        #   in Loop: Header=BB0_472 Depth=1
	movq	128(%rsp), %rbx         # 8-byte Reload
	andq	%r13, %rbx
	movq	$54, %r15
	cmpq	$53, %rbx
	jbe	.LBB0_477
	.align	16, 0x90
.LBB0_474:                              # %for.body1146
                                        #   Parent Loop BB0_472 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	264(%rsp), %rax         # 8-byte Reload
	movzbl	-1(%rax,%r15), %eax
	testq	%rax, %rax
	je	.LBB0_476
# BB#475:                               # %if.then1150
                                        #   in Loop: Header=BB0_474 Depth=2
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	-8(%rax,%r15,8), %rax
	movzbl	2(%rax), %edi
	callq	gen_reg_rtx
	movq	32(%r12), %rcx
	movq	%rax, -8(%rcx,%r15,8)
	movq	cfun(%rip), %rax
	movq	32(%r12), %rcx
	movq	24(%rax), %rax
	movq	-8(%rcx,%r15,8), %rcx
	movq	80(%rax), %rax
	movq	-8(%rax,%r15,8), %rsi
	movl	8(%rcx), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
.LBB0_476:                              # %for.inc1173
                                        #   in Loop: Header=BB0_474 Depth=2
	movq	%r15, %rax
	andq	%r13, %rax
	incq	%r15
	cmpq	%rbx, %rax
	jne	.LBB0_474
.LBB0_477:                              # %for.end1175
                                        #   in Loop: Header=BB0_472 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	testq	%r13, %rax
	movq	240(%rsp), %r15         # 8-byte Reload
	movq	200(%rsp), %rsi         # 8-byte Reload
	jne	.LBB0_480
# BB#478:                               # %for.end1175
                                        #   in Loop: Header=BB0_472 Depth=1
	cmpq	%rsi, %r15
	je	.LBB0_480
# BB#479:                               # %if.then1181
                                        #   in Loop: Header=BB0_472 Depth=1
	movq	16(%rsi), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	32(%rax), %r15
	movq	16(%r15), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %esi
	movq	%r12, %rdi
	callq	get_label_from_map
	movq	%rax, %rbx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_u00
	movq	%rax, 16(%r15)
	movq	240(%rsp), %r15         # 8-byte Reload
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rbx, 64(%rax)
	movl	32(%rbx), %eax
	incq	%rax
	movl	%eax, 32(%rbx)
	movq	200(%rsp), %rsi         # 8-byte Reload
.LBB0_480:                              # %if.end1209
                                        #   in Loop: Header=BB0_472 Depth=1
	movq	248(%rsp), %rax         # 8-byte Reload
	andq	%r13, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	movabsq	$0, %r9
	movq	216(%rsp), %rdi         # 8-byte Reload
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	184(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_482
# BB#481:                               # %if.end1209
                                        #   in Loop: Header=BB0_472 Depth=1
	movabsq	$1, %r9
.LBB0_482:                              # %if.end1209
                                        #   in Loop: Header=BB0_472 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%rsp)
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	%rcx, (%rsp)
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	%r12, %rcx
	movq	168(%rsp), %r8          # 8-byte Reload
	callq	copy_loop_body
	movq	248(%rsp), %rax         # 8-byte Reload
	incq	%rax
	movq	%rax, 248(%rsp)         # 8-byte Spill
	andq	%r13, %rax
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_472
.LBB0_483:                              # %for.end1215
	movq	%r12, 96(%rsp)          # 8-byte Spill
	movq	%r15, %r12
	callq	gen_label_rtx
	movq	208(%rsp), %rcx         # 8-byte Reload
	movq	%rax, %rdi
	testq	%r13, %rcx
	je	.LBB0_484
# BB#488:                               # %if.else1221
	movq	176(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB0_489
.LBB0_484:                              # %if.then1218
	movq	224(%rsp), %rsi         # 8-byte Reload
.LBB0_489:                              # %if.end1224
	callq	emit_label_after
	movq	%rax, %rbx
	movq	232(%rsp), %r15         # 8-byte Reload
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	cmpq	%rbx, %rax
	je	.LBB0_499
# BB#490:
	movabsq	$4294967208, %r14       # imm = 0xFFFFFFA8
	.align	16, 0x90
.LBB0_491:                              # %while.body1231
                                        # =>This Inner Loop Header: Depth=1
	movq	192(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB0_495
# BB#492:                               # %land.lhs.true1234
                                        #   in Loop: Header=BB0_491 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB0_493
# BB#496:                               # %land.lhs.true1249
                                        #   in Loop: Header=BB0_491 Depth=1
	movl	40(%rax), %ecx
	cmpq	%r14, %rcx
	je	.LBB0_495
	jmp	.LBB0_497
.LBB0_493:                              # %land.lhs.true1234
                                        #   in Loop: Header=BB0_491 Depth=1
	cmpq	$36, %rcx
	jne	.LBB0_497
# BB#494:                               # %land.lhs.true1239
                                        #   in Loop: Header=BB0_491 Depth=1
	cmpq	$0, 56(%rax)
	jne	.LBB0_495
.LBB0_497:                              # %if.then1255
                                        #   in Loop: Header=BB0_491 Depth=1
	movq	%rax, %rdi
	callq	delete_related_insns
	jmp	.LBB0_498
.LBB0_495:                              # %if.else1257
                                        #   in Loop: Header=BB0_491 Depth=1
	movq	24(%rax), %rax
.LBB0_498:                              # %while.cond1228.backedge
                                        #   in Loop: Header=BB0_491 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB0_491
.LBB0_499:                              # %while.end1262
	movzbl	3(%rbx), %eax
	testq	$8, %rax
	jne	.LBB0_500
# BB#501:                               # %if.end1268
	movq	%rbx, %rdi
	callq	delete_related_insns
	movq	168(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB0_503
# BB#502:                               # %if.then1271
	movq	%r15, %rsi
	callq	emit_label_after
.LBB0_503:                              # %egress
	movq	208(%rsp), %rax         # 8-byte Reload
	testq	%r13, %rax
	movq	216(%rsp), %rbx         # 8-byte Reload
	jne	.LBB0_512
# BB#504:                               # %if.then1276
	movq	168(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB0_506
# BB#505:                               # %if.then1279
	callq	delete_related_insns
.LBB0_506:                              # %if.end1282
	movq	176(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB0_508
# BB#507:                               # %if.then1285
	callq	delete_related_insns
.LBB0_508:                              # %if.end1288
	movq	%r12, %rdi
	testq	%rdi, %rdi
	je	.LBB0_510
# BB#509:                               # %if.then1290
	callq	delete_related_insns
.LBB0_510:                              # %if.end1292
	testq	%r15, %r15
	je	.LBB0_512
# BB#511:                               # %if.then1294
	movq	%r15, %rdi
	callq	delete_related_insns
.LBB0_512:                              # %if.end1297
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	64(%rbx), %rdi
	testq	%rdi, %rdi
	movq	256(%rsp), %r14         # 8-byte Reload
	je	.LBB0_514
# BB#513:                               # %if.then1303
	callq	free
	movq	$0, 64(%rbx)
.LBB0_514:                              # %if.end1307
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB0_516
# BB#515:                               # %if.then1310
	callq	free
	movq	%r14, %rdi
	callq	free
.LBB0_516:                              # %if.end1312
	movq	48(%rbx), %rdi
	callq	free
	movq	splittable_regs(%rip), %rdi
	callq	free
	movq	splittable_regs_updates(%rip), %rdi
	callq	free
	movq	addr_combined_regs(%rip), %rdi
	callq	free
	movq	264(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB0_518
# BB#517:                               # %if.then1316
	callq	free
.LBB0_518:                              # %if.end1318
	movq	%rbx, %rdi
	addq	$304, %rsp              # imm = 0x130
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB0_135:                              # %cond.false
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.unroll_loop, %rcx
	movq	$811, %rsi              # imm = 0x32B
	jmp	.LBB0_136
.LBB0_546:                              # %if.else917
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.unroll_loop, %rcx
	movq	$1104, %rsi             # imm = 0x450
	jmp	.LBB0_136
.LBB0_547:                              # %cond.false.i
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.find_splittable_regs, %rcx
	movq	$2545, %rsi             # imm = 0x9F1
	jmp	.LBB0_136
.LBB0_548:                              # %cond.false.i.i
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.find_splittable_givs, %rcx
	movq	$2772, %rsi             # imm = 0xAD4
	jmp	.LBB0_136
.LBB0_500:                              # %if.then1267
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.unroll_loop, %rcx
	movq	$1337, %rsi             # imm = 0x539
.LBB0_136:                              # %cond.false
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	unroll_loop, .Ltmp17-unroll_loop
	.cfi_endproc

	.globl	set_dominates_use
	.align	16, 0x90
	.type	set_dominates_use,@function
set_dominates_use:                      # @set_dominates_use
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 64
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdi, %rax
	xorq	%r13, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rsi
	xorq	%r12, %r12
	.align	16, 0x90
.LBB1_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rbx
	movl	8(%rbx), %edi
	movzbl	(%rbx), %ecx
	cmpq	%rsi, %rdi
	je	.LBB1_5
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	$33, %rcx
	movq	$1, %rcx
	je	.LBB1_4
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r13, %rcx
.LBB1_4:                                # %while.body
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%r14, %rbx
	movq	%rcx, %r13
	jne	.LBB1_1
	jmp	.LBB1_14
.LBB1_5:                                # %while.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movzbl	rtx_class(%rcx), %ecx
	xorq	%r12, %r12
	cmpq	$105, %rcx
	jne	.LBB1_14
# BB#6:                                 # %lor.lhs.false
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	dead_or_set_regno_p
	testq	%r15, %rax
	je	.LBB1_14
# BB#7:                                 # %if.end16
	testq	%r15, %r13
	je	.LBB1_13
# BB#8:                                 # %while.cond21.preheader
	movl	8(%rbx), %eax
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%r15, %rcx
	jmp	.LBB1_9
.LBB1_12:                               # %if.end37
                                        #   in Loop: Header=BB1_9 Depth=1
	movq	24(%rbx), %rbx
	movl	8(%rbx), %eax
.LBB1_9:                                # %while.cond21.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rcx, %rax
	je	.LBB1_13
# BB#10:                                # %while.body27
                                        #   in Loop: Header=BB1_9 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB1_14
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB1_9 Depth=1
	cmpq	%r14, %rbx
	jne	.LBB1_12
.LBB1_13:
	movq	$1, %r12
.LBB1_14:                               # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	set_dominates_use, .Ltmp35-set_dominates_use
	.cfi_endproc

	.globl	precondition_loop_p
	.align	16, 0x90
	.type	precondition_loop_p,@function
precondition_loop_p:                    # @precondition_loop_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	160(%rbx), %r14
	movq	96(%r14), %rax
	movq	%rcx, %r13
	movq	%rdx, %r15
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB2_6
# BB#1:                                 # %if.then
	movq	%r8, %rbx
	movq	80(%r14), %rcx
	cmpq	$0, 8(%rcx)
	jle	.LBB2_3
# BB#2:                                 # %if.then3
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rcx, (%r12)
	movq	const_int_rtx+520(%rip), %rcx
	movq	%rcx, (%r13)
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	jmp	.LBB2_4
.LBB2_6:                                # %if.end13
	cmpq	$0, 72(%r14)
	je	.LBB2_7
# BB#10:                                # %if.else20
	cmpq	$0, 32(%r14)
	je	.LBB2_11
# BB#13:                                # %if.else28
	movq	80(%r14), %rax
	testq	%rax, %rax
	je	.LBB2_14
# BB#16:                                # %if.else36
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movzbl	(%rax), %ecx
	cmpq	$54, %rcx
	jne	.LBB2_17
# BB#19:                                # %if.else44
	movq	192(%rbx), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	8(%rax), %rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB2_23
# BB#20:                                # %land.lhs.true
	movq	80(%r14), %rax
	movq	8(%rax), %rdi
	negq	%rdi
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_21
.LBB2_23:                               # %if.end66
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB2_24
# BB#26:                                # %if.end74
	movzbl	(%rsi), %eax
	cmpq	$75, %rax
	jne	.LBB2_27
# BB#29:                                # %land.lhs.true88
	movq	8(%rsi), %rax
	movl	8(%rax), %eax
	jmp	.LBB2_30
.LBB2_3:                                # %if.else
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_CONST_INT
	movq	%rax, (%r12)
	movq	const_int_rtx+504(%rip), %rax
	movq	%rax, (%r13)
	movq	const_int_rtx+512(%rip), %rax
.LBB2_4:                                # %if.end
	movq	%rax, (%r15)
	movl	word_mode(%rip), %eax
	movl	%eax, (%rbx)
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	je	.LBB2_51
# BB#5:                                 # %if.then7
	movabsq	$.L.str18, %rdi
	movq	$54, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	callq	fwrite
	movq	loop_dump_stream(%rip), %rdi
	movq	96(%r14), %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	callq	fprintf
	movq	loop_dump_stream(%rip), %rcx
	movabsq	$.L.str19, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_7:                                # %if.then15
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#8:                                 # %if.then17
	movabsq	$.L.str20, %rdi
	movq	$52, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_11:                               # %if.then23
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#12:                                # %if.then25
	movabsq	$.L.str21, %rdi
	movq	$47, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_14:                               # %if.then31
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#15:                                # %if.then33
	movabsq	$.L.str22, %rdi
	movq	$49, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_17:                               # %if.then39
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#18:                                # %if.then41
	movabsq	$.L.str23, %rdi
	movq	$43, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_24:                               # %if.then69
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#25:                                # %if.then71
	movabsq	$.L.str25, %rdi
	movq	$37, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_27:                               # %if.end74
	cmpq	$61, %rax
	jne	.LBB2_31
# BB#28:                                # %land.lhs.true79
	movl	8(%rsi), %eax
.LBB2_30:                               # %land.lhs.true88
	movl	max_reg_before_loop(%rip), %ecx
	cmpq	%rcx, %rax
	jae	.LBB2_32
.LBB2_31:                               # %lor.lhs.false96
	movq	%rbx, %rdi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_32
# BB#34:                                # %if.end105
	movq	48(%r14), %rax
	movzbl	2(%rax), %eax
	movl	mode_class(,%rax,4), %eax
	cmpq	$2, %rax
	je	.LBB2_36
# BB#35:                                # %lor.lhs.false111
	movq	32(%r14), %rax
	movzbl	2(%rax), %ecx
	movl	mode_class(,%rcx,4), %ecx
	cmpq	$2, %rcx
	jne	.LBB2_38
.LBB2_36:                               # %if.then119
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#37:                                # %if.then121
	movabsq	$.L.str27, %rdi
	movq	$56, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_32:                               # %if.then100
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#33:                                # %if.then102
	movabsq	$.L.str26, %rdi
	movq	$44, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_21:                               # %if.then57
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#22:                                # %if.then59
	movabsq	$.L.str24, %rdi
	movq	$54, %rsi
	movq	$1, %rdx
.LBB2_50:                               # %return
	callq	fwrite
.LBB2_51:                               # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_38:                               # %if.end124
	movq	(%rsp), %rcx            # 8-byte Reload
	movslq	8(%rcx), %rcx
	movslq	max_uid_for_loop(%rip), %rdx
	cmpq	%rdx, %rcx
	jge	.LBB2_52
# BB#39:                                # %cond.true
	movq	72(%r14), %rdx
	movq	reg_n_info(%rip), %rsi
	movl	8(%rdx), %edx
	movq	32(%rsi,%rdx,8), %rdx
	movq	uid_luid(%rip), %rsi
	movslq	(%rdx), %rdx
	movslq	(%rsi,%rdx,4), %rdx
	movslq	(%rsi,%rcx,4), %rcx
	cmpq	%rcx, %rdx
	movq	8(%rsp), %rsi           # 8-byte Reload
	jle	.LBB2_42
# BB#40:                                # %if.then142
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB2_51
# BB#41:                                # %if.then144
	movabsq	$.L.str28, %rdi
	movq	$59, %rsi
	movq	$1, %rdx
	jmp	.LBB2_50
.LBB2_42:                               # %if.end147
	movq	%rax, (%r12)
	movq	80(%r14), %rax
	movq	%rax, (%r13)
	movq	48(%r14), %rax
	movq	%rax, (%r15)
	movzbl	2(%rax), %ecx
	movl	%ecx, (%rsi)
	movq	(%r12), %rax
	movzbl	2(%rax), %eax
	testq	%rcx, %rcx
	je	.LBB2_43
# BB#45:                                # %if.else162
	cmpq	%rax, %rcx
	je	.LBB2_48
# BB#46:                                # %land.lhs.true167
	movzbl	mode_size(%rcx), %ecx
	movzbl	mode_size(%rax), %edx
	cmpq	%rdx, %rcx
	jae	.LBB2_48
	jmp	.LBB2_47
.LBB2_43:                               # %if.then155
	movl	%eax, (%rsi)
	testq	%rax, %rax
	jne	.LBB2_48
# BB#44:                                # %if.then160
	movl	word_mode(%rip), %eax
.LBB2_47:                               # %if.then178
	movl	%eax, (%rsi)
.LBB2_48:                               # %if.end183
	movq	loop_dump_stream(%rip), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	je	.LBB2_51
# BB#49:                                # %if.then185
	movabsq	$.L.str29, %rdi
	movq	$29, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	jmp	.LBB2_50
.LBB2_52:                               # %cond.false
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.precondition_loop_p, %rcx
	movq	$1512, %rsi             # imm = 0x5E8
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp53:
	.size	precondition_loop_p, .Ltmp53-precondition_loop_p
	.cfi_endproc

	.align	16, 0x90
	.type	copy_loop_body,@function
copy_loop_body:                         # @copy_loop_body
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 160
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	168(%rsp), %r12
	movq	160(%rdi), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%r9, 80(%rsp)           # 8-byte Spill
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	%rcx, %r15
	movq	%r15, 88(%rsp)          # 8-byte Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r9
	je	.LBB3_1
# BB#2:                                 # %if.else
	movslq	48(%r12), %rax
	movq	40(%r15), %rcx
	movq	%r12, (%rcx,%rax,8)
	xorq	%rax, %rax
	jmp	.LBB3_3
.LBB3_1:                                # %if.then
	callq	gen_label_rtx
	movslq	48(%r12), %rcx
	movq	40(%r15), %rdx
	movq	%rax, (%rdx,%rcx,8)
.LBB3_3:                                # %if.end
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movabsq	$0, %r14
	movabsq	$1, %r12
	movq	%r14, %rax
	je	.LBB3_5
# BB#4:                                 # %if.end
	movq	%r12, %rax
.LBB3_5:                                # %if.end
	movq	%rax, 56(%rsp)          # 8-byte Spill
	callq	start_sequence
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	xorq	%r13, %r13
	callq	emit_note
	movq	160(%rsp), %rax
	testq	%rbx, %rax
	movq	%r14, %rax
	je	.LBB3_7
# BB#6:                                 # %if.end
	movq	%r12, %rax
.LBB3_7:                                # %if.end
	andq	%rax, 56(%rsp)          # 8-byte Folded Spill
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	104(%rsp), %r8          # 8-byte Reload
	jmp	.LBB3_8
	.align	16, 0x90
.LBB3_9:                                # %do.body
                                        #   in Loop: Header=BB3_8 Depth=1
	jmpq	*.LJTI3_0(,%rax,8)
.LBB3_10:                               # %sw.bb
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	rtx_class+32(%rip), %eax
	movq	32(%r12), %r13
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	cmpq	$105, %rax
	movabsq	$0, %r14
	jne	.LBB3_41
# BB#11:                                # %cond.true
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	movq	%r13, %rax
	je	.LBB3_13
# BB#12:                                # %cond.false
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	single_set_2
.LBB3_13:                               # %cond.end36
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB3_41
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%rax), %rcx
	movzbl	(%rcx), %edx
	cmpq	$61, %rdx
	movabsq	$0, %r14
	jne	.LBB3_41
# BB#15:                                # %land.lhs.true46
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	8(%rcx), %edx
	movq	addr_combined_regs(%rip), %rcx
	movq	(%rcx,%rdx,8), %rsi
	testq	%rsi, %rsi
	movabsq	$0, %r14
	je	.LBB3_41
# BB#16:                                # %if.then55
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	16(%rsi), %rcx
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movl	8(%rcx), %esi
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	208(%rcx), %rdi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	8(%rdi,%rsi), %r14
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	calculate_giv_inc
	movq	24(%r14), %r13
	movq	%rax, %r14
	testq	%r13, %r13
	je	.LBB3_40
# BB#17:                                # %for.body.lr.ph
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB3_32
	.align	16, 0x90
.LBB3_18:                               # %for.body.us
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	24(%r13), %eax
	testq	%rax, %rax
	jne	.LBB3_31
# BB#19:                                # %land.lhs.true79.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	136(%r13), %rax
	cmpq	%rdx, %rax
	jne	.LBB3_31
# BB#20:                                # %if.then82.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	40(%r13), %rax
	movq	32(%r13), %rcx
	movq	(%rax), %rdi
	cmpq	%rcx, %rdi
	jne	.LBB3_31
# BB#21:                                # %if.end86.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	64(%r13), %r8
	movq	64(%rdx), %r9
	movq	8(%r14), %rsi
	movq	%r14, %r15
	cmpq	%r9, %r8
	je	.LBB3_23
# BB#22:                                # %if.then93.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	cqto
	idivq	8(%r9)
	movq	8(%r8), %rsi
	imulq	%rax, %rsi
.LBB3_23:                               # %if.then115.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	%rbx, %r14
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	plus_constant_wide
	movq	%rax, 32(%r13)
	movq	40(%r13), %rcx
	movq	%rax, (%rcx)
	movq	32(%r13), %rdi
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	movq	%rdi, %r8
	je	.LBB3_25
# BB#24:                                # %if.else124.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	8(%rdi), %r8
.LBB3_25:                               # %if.end129.us
                                        #   in Loop: Header=BB3_18 Depth=2
	cmpq	$0, 152(%r13)
	movq	%r14, %rbx
	jne	.LBB3_30
# BB#26:                                # %land.lhs.true131.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movzbl	101(%r13), %eax
	testq	$4, %rax
	jne	.LBB3_30
# BB#27:                                # %if.then135.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	144(%r13), %rsi
	movq	%r8, %r14
	callq	plus_constant_wide
	movq	%r14, %r8
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB3_30
# BB#28:                                # %if.then142.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	8(%rax), %rdx
	movq	16(%rax), %rcx
	movzbl	2(%r8), %edi
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r8, %r14
                                        # kill: R8<def> R14<kill>
	callq	expand_simple_binop
	movq	%r14, %r8
	cmpq	%r8, %rax
	je	.LBB3_30
# BB#29:                                # %if.then.i.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	%r8, %rdi
	movq	%rax, %rsi
	movq	%r8, %r14
	callq	emit_move_insn
	movq	%r14, %r8
.LBB3_30:                               # %if.end150.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	144(%r13), %rsi
	negq	%rsi
	movq	%r8, %rdi
	callq	plus_constant_wide
	movq	%rax, 32(%r13)
	movq	40(%r13), %rcx
	movq	%rax, (%rcx)
	movq	%r15, %r14
	movq	24(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB3_31:                               # %for.inc.us
                                        #   in Loop: Header=BB3_18 Depth=2
	movq	128(%r13), %r13
	testq	%r13, %r13
	jne	.LBB3_18
	jmp	.LBB3_39
.LBB3_72:                               # %sw.bb358
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r15, %r14
	movq	%r8, %r15
	movq	32(%r12), %rdi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	56(%r12), %rdi
	movq	%rax, %r13
	movq	%r14, %rsi
	callq	initial_reg_note_copy
	movq	%rax, 56(%r13)
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB3_74
# BB#73:                                # %if.then375
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	48(%rax), %esi
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	get_label_from_map
	movq	%rax, 64(%r13)
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
	movq	64(%r12), %rbx
.LBB3_74:                               # %if.end392
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	168(%rsp), %rax
	cmpq	%rax, %rbx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jne	.LBB3_81
# BB#75:                                # %if.end392
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	%r15, %r12
	jne	.LBB3_81
# BB#76:                                # %if.end392
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB3_81
# BB#77:                                # %if.then403
                                        #   in Loop: Header=BB3_8 Depth=1
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	invert_jump
	testq	%rbx, %rax
	jne	.LBB3_81
# BB#78:                                # %if.then406
                                        #   in Loop: Header=BB3_8 Depth=1
	callq	gen_label_rtx
	movq	%rax, %r15
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_jump_insn_after
	movq	%r14, 64(%rax)
	movl	32(%r14), %ecx
	incq	%rcx
	movl	%ecx, 32(%r14)
	movq	%rax, %rdi
	callq	emit_barrier_after
	movq	%r15, %r14
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_label_after
	xorq	%rax, %rax
	movl	%eax, 32(%r14)
	movq	%r14, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	redirect_jump
	movq	104(%rsp), %r15         # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB3_79
.LBB3_81:                               # %if.end427
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r15, %r14
	movq	%r13, %rdi
	movq	88(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	try_constants
	cmpq	$0, 64(%r12)
	jne	.LBB3_82
# BB#96:                                # %if.else533
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r13), %rax
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65534, %rdx            # imm = 0xFFFE
	cmpq	$44, %rdx
	jne	.LBB3_101
# BB#97:                                # %if.then549
                                        #   in Loop: Header=BB3_8 Depth=1
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$45, %rcx
	movabsq	$0, %rcx
	jne	.LBB3_99
# BB#98:                                # %if.then549
                                        #   in Loop: Header=BB3_8 Depth=1
	movabsq	$1, %rcx
.LBB3_99:                               # %if.then549
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%rax,%rcx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rdx
	jle	.LBB3_101
	.align	16, 0x90
.LBB3_100:                              # %for.body564
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%rdx,8), %rsi
	movq	8(%rsi), %rsi
	movl	32(%rsi), %edi
	incq	%rdi
	movl	%edi, 32(%rsi)
	movq	%rdx, %rsi
	andq	%rbx, %rsi
	movq	%rcx, %rdi
	andq	%rbx, %rdi
	incq	%rdx
	cmpq	%rdi, %rsi
	jne	.LBB3_100
	jmp	.LBB3_101
.LBB3_106:                              # %sw.bb600
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r12), %rdi
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_call_insn
	movq	56(%r12), %rdi
	movq	%rax, %r13
	movq	%r15, %rsi
	callq	initial_reg_note_copy
	movq	%rax, 56(%r13)
	movl	(%r12), %eax
	movl	(%r13), %ecx
	andq	$16777216, %rax         # imm = 0x1000000
	movabsq	$4278190079, %rdx       # imm = 0xFEFFFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, (%r13)
	movq	64(%r12), %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, 64(%r13)
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	try_constants
	movq	64(%r15), %rax
	leaq	32(%rax), %rdx
	.align	16, 0x90
.LBB3_107:                              # %vector.body
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$4, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$0, (%rdx)
	movq	$0, 48(%rax,%rsi)
	movq	$0, 64(%rax,%rsi)
	movq	$0, 80(%rax,%rsi)
	addq	$4, %r14
	addq	$64, %rdx
	cmpq	$52, %r14
	jne	.LBB3_107
# BB#108:                               # %for.body628.preheader
                                        #   in Loop: Header=BB3_8 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB3_109:                              # %for.body628
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$0, 864(%rax,%rcx)
	addq	$16, %rcx
	cmpq	$16, %rcx
	jne	.LBB3_109
# BB#110:                               #   in Loop: Header=BB3_8 Depth=1
	movq	104(%rsp), %r8          # 8-byte Reload
	jmp	.LBB3_123
.LBB3_113:                              # %sw.bb650
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r8, %r14
	callq	emit_barrier
	jmp	.LBB3_114
.LBB3_111:                              # %sw.bb638
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	168(%rsp), %rax
	cmpq	%rax, %r12
	je	.LBB3_123
# BB#112:                               # %if.then641
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	48(%r12), %esi
	movq	%r15, %rdi
	movq	%r8, %r14
	callq	get_label_from_map
	movq	%rax, %rdi
	callq	emit_label
	movq	%r14, %r8
	movl	72(%r15), %ecx
	movq	%rax, %r13
	incq	%rcx
	movl	%ecx, 72(%r15)
	jmp	.LBB3_123
.LBB3_115:                              # %sw.bb652
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	40(%r12), %esi
	leaq	99(%rsi), %rcx
	andq	%rbx, %rcx
	cmpq	$19, %rcx
	ja	.LBB3_119
# BB#116:                               # %sw.bb652
                                        #   in Loop: Header=BB3_8 Depth=1
	xorq	%r13, %r13
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$526337, %rax           # imm = 0x80801
	jne	.LBB3_123
# BB#117:                               # %sw.bb652
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$96, %rax
	je	.LBB3_119
# BB#118:                               # %lor.lhs.false682
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB3_123
.LBB3_119:                              # %if.then687
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r12), %rdi
	movq	%r8, %r14
	callq	emit_note
.LBB3_114:                              # %sw.epilog
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r14, %r8
	movq	%rax, %r13
	jmp	.LBB3_123
.LBB3_82:                               # %if.then432
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_94
# BB#83:                                # %cond.true440
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB3_85
# BB#84:                                # %cond.false452
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB3_85:                               # %cond.end460
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rax, %rax
	je	.LBB3_94
# BB#86:                                # %if.then463
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	16(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$72, %rcx
	jne	.LBB3_87
# BB#88:                                # %if.then480
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	16(%rax), %rcx
	movq	global_rtl(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB3_89
# BB#90:                                # %if.else493
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	24(%rax), %rax
	jmp	.LBB3_91
	.align	16, 0x90
.LBB3_32:                               # %for.body
                                        #   Parent Loop BB3_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	24(%r13), %eax
	testq	%rax, %rax
	jne	.LBB3_38
# BB#33:                                # %land.lhs.true79
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	136(%r13), %rax
	cmpq	%rdx, %rax
	jne	.LBB3_38
# BB#34:                                # %if.then82
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	40(%r13), %rax
	movq	32(%r13), %rcx
	movq	(%rax), %rdi
	cmpq	%rcx, %rdi
	jne	.LBB3_38
# BB#35:                                # %if.end86
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	64(%r13), %r8
	movq	64(%rdx), %r9
	movq	%rdx, %r15
	movq	8(%r14), %rsi
	cmpq	%r9, %r8
	je	.LBB3_37
# BB#36:                                # %if.then93
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	cqto
	idivq	8(%r9)
	movq	8(%r8), %rsi
	imulq	%rax, %rsi
.LBB3_37:                               # %if.end104
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	callq	plus_constant_wide
	movq	%rax, 32(%r13)
	movq	40(%r13), %rcx
	movq	%rax, (%rcx)
	movq	%r15, %rdx
	.align	16, 0x90
.LBB3_38:                               # %for.inc
                                        #   in Loop: Header=BB3_32 Depth=2
	movq	128(%r13), %r13
	testq	%r13, %r13
	jne	.LBB3_32
.LBB3_39:                               #   in Loop: Header=BB3_8 Depth=1
	movq	88(%rsp), %r15          # 8-byte Reload
.LBB3_40:                               # %if.end158
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB3_41:                               # %if.end158
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB3_47
# BB#42:                                # %cond.true166
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r12), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB3_44
# BB#43:                                # %cond.false178
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB3_44:                               # %cond.end186
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rax, %rax
	movq	%r14, %rdx
	je	.LBB3_47
# BB#45:                                # %land.lhs.true189
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%rax), %r14
	movzbl	(%r14), %ecx
	cmpq	$61, %rcx
	jne	.LBB3_46
# BB#48:                                # %land.lhs.true197
                                        #   in Loop: Header=BB3_8 Depth=1
	movl	8(%r14), %esi
	movq	splittable_regs(%rip), %rcx
	cmpq	$0, (%rcx,%rsi,8)
	jne	.LBB3_49
.LBB3_46:                               #   in Loop: Header=BB3_8 Depth=1
	movq	%rax, 96(%rsp)          # 8-byte Spill
.LBB3_47:                               # %if.else320
                                        #   in Loop: Header=BB3_8 Depth=1
	xorq	%rdx, %rdx
	xorq	%r14, %r14
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	copy_rtx_and_substitute
	movq	%rax, %rdi
	callq	emit_insn
.LBB3_67:                               # %if.end323
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%rax, %r13
	movq	56(%r12), %rdi
	movq	%r15, %rsi
	callq	initial_reg_note_copy
	movq	%rax, 56(%r13)
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	try_constants
	testq	%r14, %r14
	je	.LBB3_68
# BB#69:                                # %if.then332
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rcx
	movq	64(%r15), %rax
	movslq	8(%rcx), %rdx
	movq	(%rax), %rcx
	cmpq	%rcx, %rdx
	movq	104(%rsp), %r8          # 8-byte Reload
	jae	.LBB3_123
# BB#70:                                # %land.lhs.true343
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	40(%rax,%rdx), %ecx
	movl	72(%r15), %esi
	cmpq	%rsi, %rcx
	jne	.LBB3_123
# BB#71:                                # %if.then349
                                        #   in Loop: Header=BB3_8 Depth=1
	leaq	40(%rax,%rdx), %rax
	movl	%ebx, (%rax)
	jmp	.LBB3_123
.LBB3_68:                               #   in Loop: Header=BB3_8 Depth=1
	movq	104(%rsp), %r8          # 8-byte Reload
	jmp	.LBB3_123
.LBB3_87:                               # %if.then463
                                        #   in Loop: Header=BB3_8 Depth=1
	cmpq	$67, %rcx
	jne	.LBB3_94
.LBB3_91:                               # %if.end503
                                        #   in Loop: Header=BB3_8 Depth=1
	addq	$8, %rax
	jmp	.LBB3_92
.LBB3_49:                               # %if.then207
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	movq	%rsi, %rdi
	movq	%rsi, %r13
	jne	.LBB3_51
# BB#50:                                # %if.then220
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	calculate_giv_inc
	movl	8(%r14), %edi
	movq	%rax, %rdx
.LBB3_51:                               # %if.end222
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	160(%rsp), %rax
	testq	%rbx, %rax
	jne	.LBB3_55
# BB#52:                                # %if.then228
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	splittable_regs(%rip), %rax
	andq	%rbx, %rdi
	movq	(%rax,%rdi,8), %rdi
	movq	8(%rdx), %rsi
	callq	plus_constant_wide
	movq	splittable_regs(%rip), %rcx
	movq	%rax, (%rcx,%r13,8)
	movq	splittable_regs(%rip), %rax
	movq	(%rax,%r13,8), %rdx
	movzbl	(%rdx), %eax
	cmpq	$75, %rax
	jne	.LBB3_54
# BB#53:                                # %if.then243
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	8(%rdx), %rax
	movq	16(%rdx), %rcx
	movq	%rax, %rdx
	jmp	.LBB3_64
.LBB3_89:                               # %if.then486
                                        #   in Loop: Header=BB3_8 Depth=1
	addq	$8, %rcx
	movq	%rcx, %rax
.LBB3_92:                               # %if.end503
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_94
# BB#93:                                # %land.lhs.true505
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB3_95
.LBB3_94:                               # %if.else514
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	64(%r12), %rax
	movl	48(%rax), %esi
	movq	%r15, %rdi
	callq	get_label_from_map
.LBB3_95:                               # %if.end525
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%rax, 64(%r13)
	movl	32(%rax), %ecx
	incq	%rcx
	movl	%ecx, 32(%rax)
.LBB3_101:                              # %if.end582
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r12, %rdi
	callq	any_condjump_p
	testq	%rbx, %rax
	je	.LBB3_122
# BB#102:                               # %land.lhs.true585
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r12, %rdi
	callq	onlyjump_p
	testq	%rbx, %rax
	je	.LBB3_122
# BB#103:                               # %land.lhs.true588
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	624(%r15), %rax
	testq	%rax, %rax
	movq	%r14, %r8
	je	.LBB3_123
# BB#104:                               # %if.then590
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r8, %r14
	movq	global_rtl(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_121
# BB#105:                               # %if.then594
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r13, %rdi
	callq	delete_insn
	xorq	%r13, %r13
	jmp	.LBB3_122
.LBB3_121:                              # %if.else596
                                        #   in Loop: Header=BB3_8 Depth=1
	callq	emit_barrier
.LBB3_122:                              # %sw.epilog
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r14, %r8
	.align	16, 0x90
.LBB3_123:                              # %sw.epilog
                                        #   in Loop: Header=BB3_8 Depth=1
	movslq	8(%r12), %rax
	movq	48(%r15), %rcx
	cmpq	%r8, %r12
	movq	%r13, (%rcx,%rax,8)
	jne	.LBB3_8
	jmp	.LBB3_124
.LBB3_55:                               # %if.else258
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	216(%rcx), %eax
	cmpq	%rax, %r13
	jae	.LBB3_58
# BB#56:                                # %land.lhs.true261
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	208(%rcx), %rax
	movq	$4, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movl	(%rax,%rsi), %ecx
	cmpq	$1, %rcx
	jne	.LBB3_58
# BB#57:                                # %if.then267
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$4, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	8(%rax,%rsi), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %r14
.LBB3_58:                               # %if.end274
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	splittable_regs(%rip), %rax
	andq	%rbx, %rdi
	movq	(%rax,%rdi,8), %rcx
	movzbl	2(%r14), %esi
	movq	$75, %rdi
	callq	simplify_gen_binary
	movq	splittable_regs(%rip), %rcx
	movq	%rax, (%rcx,%r13,8)
	movq	splittable_regs(%rip), %rax
	movq	(%rax,%r13,8), %rcx
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB3_60
# BB#59:                                # %lor.lhs.false
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	splittable_regs_updates(%rip), %rax
	movl	(%rax,%r13,4), %edx
	cmpq	$1, %rdx
	leaq	-1(%rdx), %rdx
	movl	%edx, (%rax,%r13,4)
	jne	.LBB3_60
# BB#63:                                # %if.else310
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	32(%r15), %rax
	movq	%r14, (%rax,%r13,8)
	movq	%r14, %rdx
	jmp	.LBB3_64
.LBB3_60:                               # %if.then290
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	2(%r14), %edi
	movq	%rcx, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	callq	gen_reg_rtx
	movq	32(%r15), %rcx
	movq	%r13, %rdx
	movq	%rax, %r13
	movq	%r13, (%rcx,%rdx,8)
	movq	const_int_rtx+512(%rip), %rax
	movl	8(%r13), %edi
	cmpq	%rax, %rbx
	movq	%r14, %rsi
	je	.LBB3_62
# BB#61:                                # %cond.false303
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	2(%r14), %esi
	movq	%rdi, %rbx
	movq	$75, %rdi
	movq	%r14, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rdi
	movq	%rax, %rsi
.LBB3_62:                               # %cond.end308
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %rdx
	callq	record_base_value
	movq	%r14, %rdx
	movq	%r13, %r14
	movq	24(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB3_64
.LBB3_54:                               # %if.else254
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	const_int_rtx+512(%rip), %rcx
.LBB3_64:                               # %if.end315
                                        #   in Loop: Header=BB3_8 Depth=1
	movzbl	2(%r14), %edi
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r14, %r8
	callq	expand_simple_binop
	cmpq	%r14, %rax
	je	.LBB3_66
# BB#65:                                # %if.then.i1035
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
.LBB3_66:                               # %emit_unrolled_add.exit1036
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	$1, %r14
	callq	get_last_insn
	jmp	.LBB3_67
	.align	16, 0x90
.LBB3_8:                                # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_107 Depth 2
                                        #     Child Loop BB3_109 Depth 2
                                        #     Child Loop BB3_100 Depth 2
                                        #     Child Loop BB3_32 Depth 2
                                        #     Child Loop BB3_18 Depth 2
	movq	24(%r12), %r12
	movq	$0, 88(%r15)
	movzbl	(%r12), %eax
	addq	$-32, %rax
	andq	%rbx, %rax
	cmpq	$5, %rax
	jbe	.LBB3_9
# BB#120:                               # %sw.default
	movabsq	$.L.str13, %rdi
	movabsq	$.L__FUNCTION__.copy_loop_body, %rdx
	movq	$2280, %rsi             # imm = 0x8E8
	callq	fancy_abort
.LBB3_124:
	movq	72(%rsp), %rdi          # 8-byte Reload
	.align	16, 0x90
.LBB3_125:                              # %do.body703
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_129 Depth 2
                                        #       Child Loop BB3_130 Depth 3
	movq	24(%rdi), %rdi
	movzbl	(%rdi), %eax
	addq	$-32, %rax
	andq	%rbx, %rax
	cmpq	$2, %rax
	ja	.LBB3_138
# BB#126:                               # %land.lhs.true721
                                        #   in Loop: Header=BB3_125 Depth=1
	movslq	8(%rdi), %rax
	movq	48(%r15), %rcx
	movq	(%rcx,%rax,8), %rdx
	testq	%rdx, %rdx
	je	.LBB3_138
# BB#127:                               # %if.then729
                                        #   in Loop: Header=BB3_125 Depth=1
	movq	56(%rdx), %rcx
	testq	%rcx, %rcx
	je	.LBB3_138
# BB#128:                               #   in Loop: Header=BB3_125 Depth=1
	addq	$56, %rdx
.LBB3_129:                              # %while.body.lr.ph.i
                                        #   Parent Loop BB3_125 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB3_130 Depth 3
	movq	%rcx, %rax
	.align	16, 0x90
.LBB3_130:                              # %while.body.i
                                        #   Parent Loop BB3_125 Depth=1
                                        #     Parent Loop BB3_129 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movl	(%rax), %esi
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$4, %rcx
	jne	.LBB3_137
# BB#131:                               # %if.then.i1041
                                        #   in Loop: Header=BB3_130 Depth=3
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	$5, %rsi
	jne	.LBB3_133
# BB#132:                               # %if.then4.i
                                        #   in Loop: Header=BB3_130 Depth=3
	movq	16(%rax), %rax
	testq	%rax, %rax
	movq	%rax, (%rdx)
	jne	.LBB3_130
	jmp	.LBB3_138
	.align	16, 0x90
.LBB3_133:                              # %if.else.i
                                        #   in Loop: Header=BB3_129 Depth=2
	movq	8(%rax), %rcx
	movslq	8(%rcx), %rcx
	movq	48(%r15), %rdx
	movq	(%rdx,%rcx,8), %rcx
	testq	%rcx, %rcx
	jne	.LBB3_136
# BB#134:                               # %if.then12.i
                                        #   in Loop: Header=BB3_129 Depth=2
	cmpq	$13, %rsi
	je	.LBB3_137
	jmp	.LBB3_135
.LBB3_136:                              # %if.else18.i
                                        #   in Loop: Header=BB3_129 Depth=2
	movq	%rcx, 8(%rax)
.LBB3_137:                              # %if.end24.i
                                        #   in Loop: Header=BB3_129 Depth=2
	movq	16(%rax), %rcx
	addq	$16, %rax
	testq	%rcx, %rcx
	movq	%rax, %rdx
	jne	.LBB3_129
	.align	16, 0x90
.LBB3_138:                              # %do.cond740
                                        #   in Loop: Header=BB3_125 Depth=1
	cmpq	%r8, %rdi
	jne	.LBB3_125
# BB#139:                               # %do.end743
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	%rbx, %r15
	movq	192(%rsp), %rbx
	movq	176(%rsp), %r14
	je	.LBB3_140
	jmp	.LBB3_146
	.align	16, 0x90
.LBB3_145:                              # %for.inc781
                                        #   in Loop: Header=BB3_140 Depth=1
	movq	24(%rbx), %rbx
.LBB3_140:                              # %do.end743
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %rbx
	je	.LBB3_146
# BB#141:                               # %for.body749
                                        #   in Loop: Header=BB3_140 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB3_145
# BB#142:                               # %land.lhs.true754
                                        #   in Loop: Header=BB3_140 Depth=1
	movl	40(%rbx), %esi
	leaq	99(%rsi), %rcx
	andq	%r15, %rcx
	cmpq	$19, %rcx
	ja	.LBB3_144
# BB#143:                               # %land.lhs.true754
                                        #   in Loop: Header=BB3_140 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$524353, %rax           # imm = 0x80041
	jne	.LBB3_145
.LBB3_144:                              # %if.then772
                                        #   in Loop: Header=BB3_140 Depth=1
	movq	32(%rbx), %rdi
	callq	emit_note
	jmp	.LBB3_145
.LBB3_146:                              # %if.end786
	movq	48(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB3_149
# BB#147:                               # %land.lhs.true788
	movslq	32(%rdi), %rax
	testq	%rax, %rax
	jle	.LBB3_149
# BB#148:                               # %if.then794
	callq	emit_label
.LBB3_149:                              # %if.end796
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	xorq	%rsi, %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	184(%rsp), %rdx
	movq	%rbx, %rcx
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	loop_insn_emit_before  # TAILCALL
.LBB3_79:                               # %if.then424
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.copy_loop_body, %rcx
	movq	$2134, %rsi             # imm = 0x856
	jmp	.LBB3_80
.LBB3_135:                              # %if.then17.i
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.final_reg_note_copy, %rcx
	movq	$1762, %rsi             # imm = 0x6E2
.LBB3_80:                               # %if.then424
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp71:
	.size	copy_loop_body, .Ltmp71-copy_loop_body
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_10
	.quad	.LBB3_72
	.quad	.LBB3_106
	.quad	.LBB3_113
	.quad	.LBB3_111
	.quad	.LBB3_115

	.text
	.align	16, 0x90
	.type	remap_split_bivs,@function
remap_split_bivs:                       # @remap_split_bivs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 64
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	%rdi, %r14
	xorq	%rax, %rax
	testq	%r13, %r13
	je	.LBB4_8
# BB#1:                                 # %if.end
	movzbl	(%r13), %eax
	leaq	-54(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$15, %rcx
	ja	.LBB4_9
# BB#2:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$57651, %rdx            # imm = 0xE133
	je	.LBB4_4
# BB#3:
	movq	%r13, %rax
	jmp	.LBB4_8
.LBB4_4:                                # %if.end
	cmpq	$7, %rcx
	jne	.LBB4_9
# BB#5:                                 # %sw.bb2
	movq	160(%r14), %rcx
	movl	8(%r13), %edx
	movl	216(%rcx), %esi
	cmpq	%rsi, %rdx
	jae	.LBB4_9
# BB#6:                                 # %land.lhs.true
	movq	208(%rcx), %rsi
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	(%rsi,%rdi), %ecx
	cmpq	$1, %rcx
	jne	.LBB4_9
# BB#7:                                 # %if.then9
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rsi,%rdx), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	jmp	.LBB4_8
.LBB4_9:                                # %sw.epilog
	movzbl	rtx_length(%rax), %r15d
	testq	%r15, %r15
	je	.LBB4_10
# BB#11:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB4_12:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_18 Depth 2
	movq	%r15, %rbx
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	-1(%rax,%rbx), %eax
	leaq	-1(%rbx), %r15
	cmpq	$69, %rax
	jne	.LBB4_13
# BB#17:                                # %for.cond42.preheader
                                        #   in Loop: Header=BB4_12 Depth=1
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %r12
	jle	.LBB4_14
	.align	16, 0x90
.LBB4_18:                               # %for.body48
                                        #   Parent Loop BB4_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rsi
	movq	%r14, %rdi
	callq	remap_split_bivs
	movq	(%r13,%rbx,8), %rcx
	movq	%rax, (%rcx,%r12,8)
	movq	(%r13,%rbx,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%r12
	cmpq	%rdx, %rsi
	jl	.LBB4_18
	jmp	.LBB4_14
	.align	16, 0x90
.LBB4_13:                               # %for.body
                                        #   in Loop: Header=BB4_12 Depth=1
	cmpq	$101, %rax
	jne	.LBB4_14
# BB#16:                                # %if.then28
                                        #   in Loop: Header=BB4_12 Depth=1
	movq	(%r13,%rbx,8), %rsi
	movq	%r14, %rdi
	callq	remap_split_bivs
	movq	%rax, (%r13,%rbx,8)
.LBB4_14:                               # %for.cond.backedge
                                        #   in Loop: Header=BB4_12 Depth=1
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB4_12
# BB#15:
	movq	%r13, %rax
	jmp	.LBB4_8
.LBB4_10:
	movq	%r13, %rax
.LBB4_8:                                # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	remap_split_bivs, .Ltmp89-remap_split_bivs
	.cfi_endproc

	.globl	emit_unrolled_add
	.align	16, 0x90
	.type	emit_unrolled_add,@function
emit_unrolled_add:                      # @emit_unrolled_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp93:
	.cfi_def_cfa_offset 32
.Ltmp94:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	2(%rbx), %edi
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	$75, %rsi
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%rcx, %rdx
	movq	%rax, %rcx
	movq	%rbx, %r8
	callq	expand_simple_binop
	cmpq	%rbx, %rax
	je	.LBB5_1
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	jmp	emit_move_insn  # TAILCALL
.LBB5_1:                                # %if.end
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp95:
	.size	emit_unrolled_add, .Ltmp95-emit_unrolled_add
	.cfi_endproc

	.globl	back_branch_in_range_p
	.align	16, 0x90
	.type	back_branch_in_range_p,@function
back_branch_in_range_p:                 # @back_branch_in_range_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp100:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 32
.Ltmp103:
	.cfi_offset %rbx, -32
.Ltmp104:
	.cfi_offset %r14, -24
.Ltmp105:
	.cfi_offset %r15, -16
	movq	192(%rdi), %r15
	movq	200(%rdi), %r14
	movq	%rsi, %rbx
	movq	%r14, %rdi
	callq	prev_nonnote_insn
	movzbl	(%rax), %ecx
	cmpq	$35, %rcx
	jne	.LBB6_3
# BB#1:                                 # %if.then
	movq	16(%rax), %rax
	jmp	.LBB6_3
	.align	16, 0x90
.LBB6_2:                                # %while.body
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	24(%rbx), %rbx
.LBB6_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rbx), %ecx
	testq	$8, %rcx
	jne	.LBB6_2
# BB#4:                                 # %while.end
	xorq	%rcx, %rcx
	cmpq	%rax, %rbx
	je	.LBB6_5
# BB#7:                                 # %while.end
	cmpq	%r14, %rbx
	je	.LBB6_5
# BB#8:                                 # %for.cond.preheader
	movq	24(%rbx), %rdx
	cmpq	%rax, %rdx
	je	.LBB6_5
# BB#9:                                 # %for.body.lr.ph
	cmpq	%rbx, %r15
	je	.LBB6_10
.LBB6_11:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_14 Depth 2
	movzbl	(%rdx), %ecx
	cmpq	$33, %rcx
	jne	.LBB6_12
# BB#13:                                # %for.body24.lr.ph
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	64(%rdx), %rsi
	movq	%r15, %rdi
	.align	16, 0x90
.LBB6_14:                               # %for.body24
                                        #   Parent Loop BB6_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rcx
	cmpq	%rsi, %rdi
	je	.LBB6_6
# BB#15:                                # %for.inc
                                        #   in Loop: Header=BB6_14 Depth=2
	movq	24(%rdi), %rdi
	cmpq	%rbx, %rdi
	jne	.LBB6_14
.LBB6_12:                               # %for.cond.backedge
                                        #   in Loop: Header=BB6_11 Depth=1
	movq	24(%rdx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB6_11
.LBB6_5:
	xorq	%rcx, %rcx
.LBB6_6:                                # %return
	movq	%rcx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
	.align	16, 0x90
.LBB6_10:                               # %for.cond.backedge.us
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdx), %rdx
	cmpq	%rax, %rdx
	jne	.LBB6_10
	jmp	.LBB6_6
.Ltmp106:
	.size	back_branch_in_range_p, .Ltmp106-back_branch_in_range_p
	.cfi_endproc

	.globl	biv_total_increment
	.align	16, 0x90
	.type	biv_total_increment,@function
biv_total_increment:                    # @biv_total_increment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 16
.Ltmp110:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rbx
	movq	const_int_rtx+512(%rip), %rax
	testq	%rbx, %rbx
	je	.LBB7_1
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	100(%rbx), %edx
	xorq	%rcx, %rcx
	testq	$8, %rdx
	je	.LBB7_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	64(%rbx), %rsi
	movq	const_int_rtx+520(%rip), %rdi
	cmpq	%rdi, %rsi
	jne	.LBB7_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB7_2 Depth=1
	andq	$32, %rdx
	testq	$65535, %rdx            # imm = 0xFFFF
	jne	.LBB7_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	72(%rbx), %rdx
	movl	48(%rbx), %ecx
	movq	%rax, %rdi
	callq	fold_rtx_mult_add
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rax, %rcx
	jne	.LBB7_2
	jmp	.LBB7_6
.LBB7_1:
	movq	%rax, %rcx
.LBB7_6:                                # %return
	movq	%rcx, %rax
	popq	%rbx
	retq
.Ltmp111:
	.size	biv_total_increment, .Ltmp111-biv_total_increment
	.cfi_endproc

	.align	16, 0x90
	.type	fold_rtx_mult_add,@function
fold_rtx_mult_add:                      # @fold_rtx_mult_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp121:
	.cfi_def_cfa_offset 48
.Ltmp122:
	.cfi_offset %rbx, -40
.Ltmp123:
	.cfi_offset %r12, -32
.Ltmp124:
	.cfi_offset %r14, -24
.Ltmp125:
	.cfi_offset %r15, -16
	movl	(%rdi), %eax
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rsi, %r12
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r15, %rbx
	andq	%rdx, %rbx
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$255, %rsi
	cmpq	%rbx, %rsi
	je	.LBB8_2
# BB#1:                                 # %entry
	testq	%rsi, %rsi
	jne	.LBB8_18
.LBB8_2:                                # %lor.lhs.false
	movzbl	2(%r12), %ecx
	andq	%r15, %rdx
	cmpq	%rdx, %rcx
	je	.LBB8_4
# BB#3:                                 # %lor.lhs.false
	testq	%rcx, %rcx
	jne	.LBB8_18
.LBB8_4:                                # %lor.lhs.false14
	movzbl	2(%r14), %ecx
	cmpq	%rdx, %rcx
	je	.LBB8_6
# BB#5:                                 # %lor.lhs.false14
	testq	%rcx, %rcx
	jne	.LBB8_18
.LBB8_6:                                # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	movq	%rdi, %rbx
	je	.LBB8_8
# BB#7:                                 # %if.end
	movq	%r12, %rbx
.LBB8_8:                                # %if.end
	je	.LBB8_10
# BB#9:                                 # %if.end
	movq	%rdi, %r12
.LBB8_10:                               # %if.end
	movq	$78, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	jne	.LBB8_12
# BB#11:                                # %if.then29
	movq	$78, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	gen_rtx_fmt_ee
.LBB8_12:                               # %if.end31
	movzbl	(%r14), %ecx
	cmpq	$54, %rcx
	movq	%rax, %rbx
	je	.LBB8_14
# BB#13:                                # %if.end31
	movq	%r14, %rbx
.LBB8_14:                               # %if.end31
	je	.LBB8_16
# BB#15:                                # %if.end31
	movq	%rax, %r14
.LBB8_16:                               # %if.end31
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	simplify_binary_operation
	testq	%rax, %rax
	je	.LBB8_19
# BB#17:                                # %if.end41
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB8_19:                               # %if.then39
	movq	$75, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	gen_rtx_fmt_ee  # TAILCALL
.LBB8_18:                               # %if.then
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.fold_rtx_mult_add, %rcx
	movq	$2422, %rsi             # imm = 0x976
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp126:
	.size	fold_rtx_mult_add, .Ltmp126-fold_rtx_mult_add
	.cfi_endproc

	.globl	final_biv_value
	.align	16, 0x90
	.type	final_biv_value,@function
final_biv_value:                        # @final_biv_value
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
	subq	$16, %rsp
.Ltmp138:
	.cfi_def_cfa_offset 64
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
	movq	%rsi, %rbx
	movq	8(%rbx), %rax
	movslq	48(%rax), %rax
	movl	mode_class(,%rax,4), %eax
	movq	%rdi, %r15
	xorq	%r14, %r14
	cmpq	$1, %rax
	jne	.LBB9_22
# BB#1:                                 # %if.end
	movzbl	88(%rbx), %eax
	testq	$8, %rax
	je	.LBB9_4
# BB#2:                                 # %if.then2
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_21
# BB#3:                                 # %if.then4
	movl	(%rbx), %edx
	movabsq	$.L.str30, %rsi
	jmp	.LBB9_20
.LBB9_4:                                # %if.end6
	movq	160(%r15), %rax
	movq	96(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB9_17
# BB#5:                                 # %land.lhs.true
	movl	240(%r15), %eax
	testq	%rax, %rax
	jne	.LBB9_17
# BB#6:                                 # %land.lhs.true9
	movq	40(%rbx), %rsi
	movq	%r15, %rdi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_17
# BB#7:                                 # %if.then12
	movq	8(%rbx), %r12
	movq	const_int_rtx+512(%rip), %r13
	jmp	.LBB9_8
.LBB9_12:                               # %if.then.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movq	72(%r12), %rdx
	movl	48(%r12), %ecx
	movq	%r13, %rdi
	callq	fold_rtx_mult_add
	movq	128(%r12), %r12
	movq	%rax, %r13
.LBB9_8:                                # %if.then12
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB9_13
# BB#9:                                 # %for.body.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movzbl	100(%r12), %eax
	testq	$8, %rax
	je	.LBB9_17
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movq	64(%r12), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB9_17
# BB#11:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB9_8 Depth=1
	andq	$32, %rax
	testq	$65535, %rax            # imm = 0xFFFF
	je	.LBB9_12
	jmp	.LBB9_17
.LBB9_13:                               # %biv_total_increment.exit
	testq	%r13, %r13
	je	.LBB9_17
# BB#14:                                # %land.lhs.true15
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_17
# BB#15:                                # %if.then18
	movq	8(%rbx), %rax
	movl	48(%rax), %edi
	callq	gen_reg_rtx
	movq	8(%rbx), %rcx
	movq	%rax, %r14
	movq	72(%rcx), %rsi
	movl	8(%r14), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	xorq	%rdi, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	gen_rtx_CONST_INT
	movq	40(%rbx), %rcx
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	%r14, %r8
	callq	loop_iv_add_mult_sink
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_22
# BB#16:                                # %if.then27
	movl	(%rbx), %edx
	movabsq	$.L.str31, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB9_22
.LBB9_17:                               # %if.end32
	movq	8(%rbx), %rax
	movq	16(%rax), %rsi
	movq	%r15, %rdi
	callq	reg_dead_after_loop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_22
# BB#18:                                # %if.then36
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB9_21
# BB#19:                                # %if.then38
	movl	(%rbx), %edx
	movabsq	$.L.str32, %rsi
.LBB9_20:                               # %if.end41
	xorq	%rax, %rax
	callq	fprintf
.LBB9_21:                               # %if.end41
	movq	const_int_rtx+512(%rip), %r14
.LBB9_22:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp144:
	.size	final_biv_value, .Ltmp144-final_biv_value
	.cfi_endproc

	.align	16, 0x90
	.type	reg_dead_after_loop,@function
reg_dead_after_loop:                    # @reg_dead_after_loop
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
	movq	%rdi, %rbx
	movq	232(%rbx), %rdx
	movq	%rsi, %r15
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB10_3
# BB#1:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rdx), %rdx
	incq	%rcx
	testq	%rdx, %rdx
	jne	.LBB10_2
.LBB10_3:                               # %for.end
	movl	240(%rbx), %edx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB10_22
# BB#4:                                 # %if.end
	movq	200(%rbx), %rdx
	movq	$67, %rdi
	xorq	%rsi, %rsi
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	callq	gen_rtx_fmt_u00
	movq	232(%rbx), %rcx
	movq	%rax, %r14
	testq	%r14, %r14
	movq	%rcx, 16(%r14)
	je	.LBB10_21
.LBB10_5:                               # %for.body7
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_6 Depth 2
                                        #       Child Loop BB10_7 Depth 3
	leaq	8(%r14), %rbx
	jmp	.LBB10_6
	.align	16, 0x90
.LBB10_19:                              # %if.else77
                                        #   in Loop: Header=BB10_6 Depth=2
	incq	8(%rsp)                 # 8-byte Folded Spill
	addq	$64, %rbx
.LBB10_6:                               # %while.cond.outer
                                        #   Parent Loop BB10_5 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB10_7 Depth 3
	movq	(%rbx), %rbx
	.align	16, 0x90
.LBB10_7:                               # %while.cond
                                        #   Parent Loop BB10_5 Depth=1
                                        #     Parent Loop BB10_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB10_20
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB10_7 Depth=3
	movzbl	(%rbx), %r13d
	movzbl	rtx_class(%r13), %eax
	cmpq	$105, %rax
	jne	.LBB10_15
# BB#9:                                 # %if.then18
                                        #   in Loop: Header=BB10_7 Depth=3
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	callq	reg_referenced_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	jne	.LBB10_22
# BB#10:                                # %if.end25
                                        #   in Loop: Header=BB10_7 Depth=3
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB10_15
# BB#11:                                # %cond.true
                                        #   in Loop: Header=BB10_7 Depth=3
	movq	32(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$47, %rcx
	je	.LBB10_13
# BB#12:                                # %cond.false
                                        #   in Loop: Header=BB10_7 Depth=3
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	single_set_2
.LBB10_13:                              # %cond.end49
                                        #   in Loop: Header=BB10_7 Depth=3
	testq	%rax, %rax
	je	.LBB10_15
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB10_7 Depth=3
	movq	8(%rax), %rdi
	movq	%r15, %rsi
	callq	rtx_equal_p
	testq	%r12, %rax
	jne	.LBB10_20
	.align	16, 0x90
.LBB10_15:                              # %if.end59
                                        #   in Loop: Header=BB10_7 Depth=3
	cmpq	$33, %r13
	jne	.LBB10_7
# BB#16:                                # %if.then62
                                        #   in Loop: Header=BB10_6 Depth=2
	movq	32(%rbx), %rax
	movzbl	(%rax), %eax
	cmpq	$51, %rax
	je	.LBB10_20
# BB#17:                                # %if.else
                                        #   in Loop: Header=BB10_6 Depth=2
	movq	%rbx, %rdi
	callq	any_uncondjump_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	je	.LBB10_22
# BB#18:                                # %if.else
                                        #   in Loop: Header=BB10_6 Depth=2
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$20, %rdx
	jle	.LBB10_19
	jmp	.LBB10_22
	.align	16, 0x90
.LBB10_20:                              # %for.inc87
                                        #   in Loop: Header=BB10_5 Depth=1
	movq	16(%r14), %r14
	testq	%r14, %r14
	jne	.LBB10_5
.LBB10_21:
	movq	$1, %rax
.LBB10_22:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp162:
	.size	reg_dead_after_loop, .Ltmp162-reg_dead_after_loop
	.cfi_endproc

	.globl	final_giv_value
	.align	16, 0x90
	.type	final_giv_value,@function
final_giv_value:                        # @final_giv_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp169:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp170:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp171:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp172:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp173:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp174:
	.cfi_def_cfa_offset 80
.Ltmp175:
	.cfi_offset %rbx, -48
.Ltmp176:
	.cfi_offset %r12, -40
.Ltmp177:
	.cfi_offset %r13, -32
.Ltmp178:
	.cfi_offset %r14, -24
.Ltmp179:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r15
	movq	16(%r12), %rcx
	movq	160(%r15), %rax
	movl	8(%rcx), %edx
	movq	208(%rax), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rsi,%rdx), %rdx
	movzbl	88(%rdx), %ecx
	testq	$8, %rcx
	je	.LBB11_3
# BB#1:                                 # %if.then
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_30
# BB#2:                                 # %if.then6
	movq	32(%r12), %rax
	movl	8(%rax), %edx
	movabsq	$.L.str33, %rsi
	jmp	.LBB11_29
.LBB11_3:                               # %if.end10
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	96(%rax), %r14
	testq	%r14, %r14
	je	.LBB11_25
# BB#4:                                 # %land.lhs.true
	movl	240(%r15), %eax
	testq	%rax, %rax
	jne	.LBB11_25
# BB#5:                                 # %land.lhs.true12
	movzbl	100(%r12), %eax
	testq	$16, %rax
	je	.LBB11_25
# BB#6:                                 # %if.then18
	movq	200(%r15), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rbx
	movq	const_int_rtx+512(%rip), %r13
	jmp	.LBB11_7
.LBB11_11:                              # %if.then.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movq	72(%rbx), %rdx
	movl	48(%rbx), %ecx
	movq	%r13, %rdi
	callq	fold_rtx_mult_add
	movq	128(%rbx), %rbx
	movq	%rax, %r13
.LBB11_7:                               # %if.then18
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB11_12
# BB#8:                                 # %for.body.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movzbl	100(%rbx), %eax
	testq	$8, %rax
	je	.LBB11_25
# BB#9:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movq	64(%rbx), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB11_25
# BB#10:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB11_7 Depth=1
	andq	$32, %rax
	testq	$65535, %rax            # imm = 0xFFFF
	je	.LBB11_11
	jmp	.LBB11_25
.LBB11_12:                              # %biv_total_increment.exit
	testq	%r13, %r13
	je	.LBB11_25
# BB#13:                                # %land.lhs.true21
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	loop_invariant_p
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB11_25
# BB#14:                                # %land.lhs.true24
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rsi
	movq	%r15, %rdi
	callq	loop_invariant_p
	testq	%rbx, %rax
	je	.LBB11_25
# BB#15:                                # %if.then27
	movl	48(%r12), %edi
	callq	gen_reg_rtx
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rcx
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r12
	movq	72(%rcx), %rsi
	movl	8(%r12), %edi
	xorq	%rdx, %rdx
	callq	record_base_value
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	extend_value_for_giv
	movq	%rax, %r13
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	gen_rtx_CONST_INT
	movq	40(%rbx), %rsi
	movq	%rax, %r14
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	callq	extend_value_for_giv
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%rax, %rcx
	movq	%r12, %r8
	callq	loop_iv_add_mult_sink
	movq	(%rbx), %rax
	movq	24(%rax), %r13
	jmp	.LBB11_16
.LBB11_25:                              # %if.end71
	movzbl	100(%r12), %eax
	testq	$1, %rax
	jne	.LBB11_32
# BB#26:                                # %if.end77
	movq	32(%r12), %rsi
	movq	%r12, %rbx
	movq	%r15, %rdi
	callq	reg_dead_after_loop
	xorq	%r12, %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB11_31
# BB#27:                                # %if.then81
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_30
# BB#28:                                # %if.then83
	movq	32(%rbx), %rax
	movl	8(%rax), %edx
	movabsq	$.L.str35, %rsi
.LBB11_29:                              # %if.end89
	xorq	%rax, %rax
	callq	fprintf
.LBB11_30:                              # %if.end89
	movq	const_int_rtx+512(%rip), %r12
.LBB11_31:                              # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_24:                              # %for.cond.loopexit
                                        #   in Loop: Header=BB11_16 Depth=1
	movq	24(%r13), %r13
.LBB11_16:                              # %if.then27
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_23 Depth 2
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	je	.LBB11_17
# BB#19:                                # %for.cond42.preheader
                                        #   in Loop: Header=BB11_16 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r14
	jmp	.LBB11_23
	.align	16, 0x90
.LBB11_22:                              # %for.inc
                                        #   in Loop: Header=BB11_23 Depth=2
	movq	128(%r14), %r14
.LBB11_23:                              # %for.inc
                                        #   Parent Loop BB11_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r14, %r14
	je	.LBB11_24
# BB#20:                                # %for.body44
                                        #   in Loop: Header=BB11_23 Depth=2
	movq	(%r14), %rax
	cmpq	%r13, %rax
	jne	.LBB11_22
# BB#21:                                # %if.then47
                                        #   in Loop: Header=BB11_23 Depth=2
	callq	start_sequence
	movq	72(%r14), %rcx
	movzbl	2(%r12), %edi
	movq	$76, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$3, (%rsp)
	movq	%r12, %rdx
	callq	expand_simple_binop
	movq	%rax, %r12
	callq	gen_sequence
	movq	%rax, %rbx
	callq	end_sequence
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	loop_insn_sink
	jmp	.LBB11_22
.LBB11_17:                              # %for.end60
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	64(%rbx), %rdx
	movq	72(%rbx), %rcx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r12, %r8
	callq	loop_iv_add_mult_sink
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_31
# BB#18:                                # %if.then63
	movq	32(%rbx), %rax
	movl	8(%rax), %edx
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB11_31
.LBB11_32:                              # %if.then76
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.final_giv_value, %rcx
	movq	$3387, %rsi             # imm = 0xD3B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp180:
	.size	final_giv_value, .Ltmp180-final_giv_value
	.cfi_endproc

	.globl	loop_iterations
	.align	16, 0x90
	.type	loop_iterations,@function
loop_iterations:                        # @loop_iterations
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp187:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp188:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp189:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp190:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp191:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp192:
	.cfi_def_cfa_offset 128
.Ltmp193:
	.cfi_offset %rbx, -48
.Ltmp194:
	.cfi_offset %r12, -40
.Ltmp195:
	.cfi_offset %r13, -32
.Ltmp196:
	.cfi_offset %r14, -24
.Ltmp197:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	160(%r13), %r14
	movq	$0, 96(%r14)
	movq	$0, 80(%r14)
	movq	$0, 72(%r14)
	movq	$0, 64(%r14)
	movq	$0, 56(%r14)
	movq	$0, 48(%r14)
	movq	$0, 40(%r14)
	movq	$0, 32(%r14)
	movq	$1, %rax
	movl	%eax, 104(%r14)
	movq	$0, 112(%r14)
	movq	200(%r13), %rax
	movq	16(%rax), %rsi
	movzbl	(%rsi), %eax
	cmpq	$33, %rax
	jne	.LBB12_1
# BB#3:                                 # %if.end9
	movq	64(%rsi), %rax
	movslq	32(%rax), %rax
	cmpq	$2, %rax
	jge	.LBB12_4
# BB#8:                                 # %if.end21
	movq	208(%r13), %r8
	testq	%r8, %r8
	je	.LBB12_21
# BB#9:                                 # %land.lhs.true
	movq	176(%r13), %r11
	testq	%r11, %r11
	je	.LBB12_21
# BB#10:                                # %if.then24
	movq	16(%rsi), %rdi
	movl	max_uid_for_loop(%rip), %r9d
	movq	uid_luid(%rip), %r10
	.align	16, 0x90
.LBB12_11:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %eax
	cmpq	$33, %rax
	jne	.LBB12_20
# BB#12:                                # %if.then31
                                        #   in Loop: Header=BB12_11 Depth=1
	movq	64(%rdi), %rax
	testq	%rax, %rax
	je	.LBB12_13
# BB#15:                                # %if.end41
                                        #   in Loop: Header=BB12_11 Depth=1
	movslq	8(%rax), %rbx
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB12_20
# BB#16:                                # %cond.true
                                        #   in Loop: Header=BB12_11 Depth=1
	movslq	8(%r8), %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB12_24
# BB#17:                                # %cond.true69
                                        #   in Loop: Header=BB12_11 Depth=1
	movl	(%r10,%rbx,4), %ebx
	movslq	(%r10,%rax,4), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jle	.LBB12_20
# BB#18:                                # %cond.true88
                                        #   in Loop: Header=BB12_11 Depth=1
	movslq	8(%r11), %rax
	movq	$32, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB12_26
# BB#19:                                # %cond.true105
                                        #   in Loop: Header=BB12_11 Depth=1
	movslq	(%r10,%rax,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jl	.LBB12_4
	.align	16, 0x90
.LBB12_20:                              # %do.cond
                                        #   in Loop: Header=BB12_11 Depth=1
	movq	16(%rdi), %rdi
	cmpq	%r11, %rdi
	jne	.LBB12_11
.LBB12_21:                              # %if.end128
	movq	%r13, %rdi
	callq	get_condition_for_loop
	testq	%rax, %rax
	je	.LBB12_22
# BB#27:                                # %if.end136
	movq	8(%rax), %rbx
	movl	(%rbx), %edi
	movq	%rdi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB12_28
# BB#30:                                # %if.end153
	movzbl	(%rax), %r12d
	movl	8(%rbx), %edx
	movl	216(%r14), %esi
	movq	16(%rax), %r8
	cmpq	%rsi, %rdx
	jb	.LBB12_33
# BB#31:                                # %if.end153
	movq	%rdi, %rax
	andq	$134217728, %rax        # imm = 0x8000000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_32
.LBB12_33:                              # %if.end162
	cmpq	%rsi, %rdx
	jb	.LBB12_36
# BB#34:                                # %if.then168
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#35:                                # %if.then170
	movabsq	$.L.str41, %rdi
	movq	$57, %rsi
	jmp	.LBB12_7
.LBB12_1:                               # %if.then
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#2:                                 # %if.then8
	movabsq	$.L.str36, %rdi
	movq	$52, %rsi
	jmp	.LBB12_7
.LBB12_4:                               # %if.then16
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#5:                                 # %if.then18
	movabsq	$.L.str37, %rdi
.LBB12_6:                               # %return
	movq	$47, %rsi
	jmp	.LBB12_7
.LBB12_22:                              # %if.then131
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#23:                                # %if.then133
	movabsq	$.L.str39, %rdi
	movq	$44, %rsi
	jmp	.LBB12_7
.LBB12_28:                              # %if.then148
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#29:                                # %if.then150
	movabsq	$.L.str40, %rdi
	movq	$50, %rsi
	jmp	.LBB12_7
.LBB12_36:                              # %if.else
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %eax
	cmpq	$65, %rax
	jb	.LBB12_39
# BB#37:                                # %if.then180
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#38:                                # %if.then182
	movabsq	$.L.str42, %rdi
	movq	$64, %rsi
	jmp	.LBB12_7
.LBB12_13:                              # %if.then36
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#14:                                # %if.then38
	movabsq	$.L.str38, %rdi
	movq	$48, %rsi
.LBB12_7:                               # %return
	movq	$1, %rdx
	callq	fwrite
.LBB12_234:                             # %return
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_39:                              # %if.else185
	movl	mode_class(,%rdi,4), %eax
	cmpq	$1, %rax
	jne	.LBB12_40
# BB#42:                                # %if.else198
	movq	208(%r14), %rax
	movq	$4, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movl	(%rax,%rdi), %ecx
	cmpq	$3, %rcx
	jne	.LBB12_43
# BB#55:                                # %if.then250
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rdi
	movq	16(%rdi), %rcx
	movl	8(%rcx), %edx
	cmpq	%rsi, %rdx
	jae	.LBB12_56
# BB#57:                                # %if.end265
	movzbl	100(%rdi), %ecx
	andq	$48, %rcx
	cmpq	$16, %rcx
	jne	.LBB12_58
# BB#60:                                # %if.end284
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	8(%rax), %r12
	movq	const_int_rtx+512(%rip), %rax
	jmp	.LBB12_61
.LBB12_40:                              # %if.then193
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#41:                                # %if.then195
	movabsq	$.L.str43, %rdi
	jmp	.LBB12_6
.LBB12_43:                              # %if.else198
	cmpq	$1, %rcx
	jne	.LBB12_83
# BB#44:                                # %if.end214
	movq	%r8, 64(%rsp)           # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	8(%rax,%rdx), %rax
	movq	8(%rax), %r15
	movzbl	100(%r15), %edx
	movq	%rdx, %rcx
	andq	$48, %rcx
	cmpq	$16, %rcx
	jne	.LBB12_45
# BB#47:                                # %if.end238
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	40(%rax), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %rax
	testq	%r15, %r15
	jne	.LBB12_49
	jmp	.LBB12_48
.LBB12_54:                              # %if.then.i.for.body.i_crit_edge
                                        #   in Loop: Header=BB12_49 Depth=1
	movzbl	100(%r15), %edx
.LBB12_49:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	$8, %rdx
	je	.LBB12_50
# BB#51:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB12_49 Depth=1
	movq	64(%r15), %rsi
	movq	const_int_rtx+520(%rip), %rcx
	cmpq	%rcx, %rsi
	jne	.LBB12_50
# BB#52:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB12_49 Depth=1
	andq	$32, %rdx
	testq	$65535, %rdx            # imm = 0xFFFF
	jne	.LBB12_50
# BB#53:                                # %if.then.i
                                        #   in Loop: Header=BB12_49 Depth=1
	movq	72(%r15), %rdx
	movl	48(%r15), %ecx
	movq	%rax, %rdi
	callq	fold_rtx_mult_add
	movq	128(%r15), %r15
	testq	%r15, %r15
	jne	.LBB12_54
.LBB12_48:
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	jmp	.LBB12_80
.LBB12_66:                              # %if.then.i915
                                        #   in Loop: Header=BB12_61 Depth=1
	movq	72(%r12), %rdx
	movl	48(%r12), %ecx
	movq	%rax, %rdi
	callq	fold_rtx_mult_add
	movq	128(%r12), %r12
.LBB12_61:                              # %if.end284
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB12_67
# BB#62:                                # %for.body.i902
                                        #   in Loop: Header=BB12_61 Depth=1
	movzbl	100(%r12), %ecx
	xorq	%r15, %r15
	testq	$8, %rcx
	movabsq	$0, %rdx
	je	.LBB12_77
# BB#63:                                # %land.lhs.true.i908
                                        #   in Loop: Header=BB12_61 Depth=1
	movq	64(%r12), %rsi
	movq	const_int_rtx+520(%rip), %rdx
	xorq	%r15, %r15
	cmpq	%rdx, %rsi
	jne	.LBB12_64
# BB#65:                                # %land.lhs.true.i908
                                        #   in Loop: Header=BB12_61 Depth=1
	andq	$32, %rcx
	testq	$65535, %rcx            # imm = 0xFFFF
	movabsq	$0, %rdx
	je	.LBB12_66
	jmp	.LBB12_77
.LBB12_58:                              # %if.then279
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#59:                                # %if.then281
	movabsq	$.L.str45, %rdi
	movq	$71, %rsi
	jmp	.LBB12_7
.LBB12_83:                              # %if.else335
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#84:                                # %if.then337
	movabsq	$.L.str48, %rdi
	movq	$53, %rsi
	jmp	.LBB12_7
.LBB12_67:                              # %biv_total_increment.exit917
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB12_77
# BB#68:                                # %if.then296
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	64(%rbx), %r15
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	extend_value_for_giv
	movq	const_int_rtx+512(%rip), %rdx
	movl	48(%rbx), %ecx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	fold_rtx_mult_add
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rbx
	movq	%rax, %rdx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB12_77
# BB#69:                                # %for.body.lr.ph
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB12_70:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	(%rbx), %rsi
	callq	loop_insn_first_p
	testq	%r12, %rax
	je	.LBB12_75
# BB#71:                                # %if.then304
                                        #   in Loop: Header=BB12_70 Depth=1
	movq	72(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB12_72
# BB#74:                                # %if.end318
                                        #   in Loop: Header=BB12_70 Depth=1
	movq	8(%rax), %rax
	subq	%rax, %r15
.LBB12_75:                              # %for.inc
                                        #   in Loop: Header=BB12_70 Depth=1
	movq	128(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB12_70
# BB#76:
	movq	40(%rsp), %rdx          # 8-byte Reload
.LBB12_77:                              # %if.end323
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	loop_dump_stream(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB12_79
# BB#78:                                # %if.then325
	movabsq	$.L.str47, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	fprintf
.LBB12_79:                              # %if.end327
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rsi
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	extend_value_for_giv
	movq	64(%rbx), %r12
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	plus_constant_wide
	movq	72(%rbx), %rdx
	movl	48(%rbx), %ecx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	fold_rtx_mult_add
	movq	48(%rsp), %r12          # 8-byte Reload
	jmp	.LBB12_80
.LBB12_45:                              # %if.then233
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#46:                                # %if.then235
	movabsq	$.L.str44, %rdi
	movq	$69, %rsi
	jmp	.LBB12_7
.LBB12_50:
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB12_80:                              # %if.end344
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	je	.LBB12_234
# BB#81:                                # %if.end348
	leaq	-102(%r12), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$9, %rcx
	ja	.LBB12_92
# BB#82:                                # %if.end348
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	xorq	%rsi, %rsi
	movq	$1, %r15
	jmpq	*.LJTI12_0(,%rcx,8)
.LBB12_87:                              # %sw.bb352
	xorq	%rsi, %rsi
	jmp	.LBB12_91
.LBB12_72:                              # %if.then309
	movq	loop_dump_stream(%rip), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB12_234
# BB#73:                                # %if.then311
	movl	8(%rax), %edx
	movabsq	$.L.str46, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	fprintf
	jmp	.LBB12_234
.LBB12_85:                              # %sw.bb350
	movq	$1, %rsi
.LBB12_86:                              # %sw.bb351
	movq	%rbx, %rdx
	movq	%rbx, %r15
	jmp	.LBB12_94
.LBB12_90:                              # %sw.bb355
	movq	$1, %rsi
.LBB12_91:                              # %sw.bb357
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	jmp	.LBB12_94
.LBB12_93:                              # %sw.bb
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %r15
	jmp	.LBB12_94
.LBB12_88:                              # %sw.bb353
	movq	$1, %rsi
.LBB12_89:                              # %sw.bb354
	xorq	%r15, %r15
.LBB12_94:                              # %sw.epilog
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movzbl	(%rdi), %eax
	cmpq	$61, %rax
	jne	.LBB12_99
# BB#95:                                # %land.lhs.true362
	movq	%r13, %rdi
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rdi
	je	.LBB12_99
# BB#96:                                # %if.then365
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	192(%r13), %rdi
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	loop_find_equiv_value
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	loop_invariant_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rdi
	je	.LBB12_98
# BB#97:                                # %if.then365
	movq	%r15, %rdi
.LBB12_98:                              # %if.then365
	movq	48(%rsp), %r15          # 8-byte Reload
.LBB12_99:                              # %if.end371
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%r15, %r15
	je	.LBB12_100
# BB#101:                               # %if.then373
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB12_102
.LBB12_100:
	movq	%rdi, 24(%rsp)          # 8-byte Spill
.LBB12_102:                             # %if.end376
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, 32(%r14)
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, 40(%r14)
	movq	%rbx, %rsi
	callq	plus_constant_wide
	movq	%r15, %rdx
	movq	%rax, 48(%r14)
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, 80(%r14)
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 72(%r14)
	movl	%r12d, 88(%r14)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 112(%r14)
	movzbl	(%rdx), %eax
	cmpq	$61, %rax
	movq	%rcx, %r15
	jne	.LBB12_103
# BB#104:                               # %if.then390
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rbx          # 8-byte Reload
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$75, %rcx
	jne	.LBB12_105
# BB#106:                               # %if.then395
	movq	8(%rbx), %r12
	movl	(%r12), %eax
	leaq	16(%rbx), %rcx
	jmp	.LBB12_107
.LBB12_103:
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB12_146
.LBB12_105:
	movabsq	$const_int_rtx+512, %rcx
	movq	%rbx, %r12
.LBB12_107:                             # %if.end403
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB12_134
# BB#108:                               # %if.end403
	cmpq	%rdx, %r12
	je	.LBB12_134
# BB#109:                               # %if.then411
	movq	(%rcx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	192(%r13), %rdi
	movq	%rdx, %rsi
	movq	%rdx, %rbx
	callq	loop_find_equiv_value
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	je	.LBB12_111
# BB#110:                               # %if.then411
	cmpq	$75, %rcx
	jne	.LBB12_126
.LBB12_111:                             # %land.lhs.true.i921
	movzbl	(%r12), %esi
	cmpq	$61, %rsi
	je	.LBB12_113
# BB#112:                               # %land.lhs.true.i921
	cmpq	$75, %rsi
	jne	.LBB12_126
.LBB12_113:                             # %if.then.i922
	movq	%r15, %r9
	movabsq	$const_int_rtx+512, %rdi
	cmpq	$75, %rcx
	movq	%rax, %rcx
	movq	%rdi, %rdx
	jne	.LBB12_115
# BB#114:                               # %if.then14.i
	movq	8(%rax), %rcx
	leaq	16(%rax), %rdx
.LBB12_115:                             # %if.end.i
	movq	(%rdx), %rdx
	cmpq	$75, %rsi
	movq	%r12, %rsi
	jne	.LBB12_117
# BB#116:                               # %if.then21.i
	movq	8(%r12), %rsi
	leaq	16(%r12), %rdi
.LBB12_117:                             # %if.end29.i
	movzbl	(%rcx), %r8d
	movq	(%rdi), %rdi
	cmpq	$61, %r8
	movq	%r9, %r8
	jne	.LBB12_120
# BB#118:                               # %land.lhs.true33.i
	cmpq	%rsi, %rcx
	je	.LBB12_124
# BB#119:                               # %land.lhs.true33.i
	cmpq	%rdi, %rcx
	je	.LBB12_124
.LBB12_120:                             # %if.else38.i
	movzbl	(%rdx), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_126
# BB#121:                               # %land.lhs.true42.i
	cmpq	%rsi, %rdx
	je	.LBB12_122
# BB#123:                               # %land.lhs.true42.i
	cmpq	%rdi, %rdx
	movq	%rdx, %rcx
	jne	.LBB12_126
	jmp	.LBB12_124
.LBB12_134:                             # %if.else439
	cmpq	$0, 168(%r13)
	je	.LBB12_135
# BB#136:                               # %if.else439
	cmpq	$54, %rax
	jne	.LBB12_137
# BB#138:                               # %if.then446
	movq	192(%r13), %rdi
	movq	%rdx, %rsi
	callq	loop_find_equiv_value
	movq	%rax, %rbx
	movzbl	(%rbx), %eax
	cmpq	$76, %rax
	jne	.LBB12_139
# BB#140:                               # %land.lhs.true453
	movq	%r15, %r12
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_141
# BB#142:                               # %if.then461
	movq	192(%r13), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	loop_find_equiv_value
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB12_141
# BB#143:                               # %land.lhs.true470
	movq	8(%rax), %rcx
	movq	16(%rbx), %rdx
	cmpq	%rdx, %rcx
	je	.LBB12_144
.LBB12_141:
	movq	%r12, %rax
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB12_145
.LBB12_92:                              # %sw.default
	movabsq	$.L.str13, %rdi
	movabsq	$.L__FUNCTION__.loop_iterations, %rdx
	movq	$3820, %rsi             # imm = 0xEEC
	callq	fancy_abort
.LBB12_135:
	movq	48(%rsp), %r12          # 8-byte Reload
	jmp	.LBB12_146
.LBB12_137:
	movq	48(%rsp), %r12          # 8-byte Reload
	jmp	.LBB12_146
.LBB12_139:
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB12_146
.LBB12_24:                              # %cond.false76
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.loop_iterations, %rcx
	movq	$3601, %rsi             # imm = 0xE11
	jmp	.LBB12_25
.LBB12_26:                              # %cond.false112
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.loop_iterations, %rcx
	movq	$3602, %rsi             # imm = 0xE12
	jmp	.LBB12_25
.LBB12_32:                              # %if.then161
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.loop_iterations, %rcx
	movq	$3651, %rsi             # imm = 0xE43
	jmp	.LBB12_25
.LBB12_56:                              # %if.then264
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.loop_iterations, %rcx
	movq	$3718, %rsi             # imm = 0xE86
	jmp	.LBB12_25
.LBB12_64:
	xorq	%rdx, %rdx
	jmp	.LBB12_77
.LBB12_122:
	movq	%rdx, %rcx
.LBB12_124:                             # %find_common_reg_term.exit
	testq	%rcx, %rcx
	je	.LBB12_126
# BB#125:
	movq	%rax, %rdx
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%r8, %r15
	jmp	.LBB12_146
.LBB12_126:                             # %if.else417
	movq	192(%r13), %rdi
	movq	%r12, %rsi
	callq	loop_find_equiv_value
	movq	72(%r14), %rcx
	cmpq	%rcx, %rax
	movq	%rbx, %rcx
	je	.LBB12_128
# BB#127:                               # %select.mid
	movq	%rax, %rcx
.LBB12_128:                             # %select.end
	cmpq	%rbx, %rcx
	je	.LBB12_130
# BB#129:
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB12_145
.LBB12_144:                             # %if.then479
	movq	16(%rax), %rdx
	movq	%r12, %rax
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB12_145:                             # %if.end487
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rax, %r15
	jmp	.LBB12_146
.LBB12_130:                             # %if.then426
	movq	const_int_rtx+512(%rip), %rax
	cmpq	%rax, 64(%rsp)          # 8-byte Folded Reload
	je	.LBB12_131
# BB#133:                               # %cond.false430
	movzbl	2(%rbx), %eax
	movq	$75, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rbx, %rdx
	movq	%rax, %rbx
	jmp	.LBB12_132
.LBB12_131:
	movq	%rbx, %rdx
                                        # kill: RBX<def> RDX<kill>
.LBB12_132:                             # %if.end487
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB12_146:                             # %if.end487
	cmpq	$0, 168(%r13)
	je	.LBB12_147
# BB#148:                               # %land.lhs.true490
	movq	%rdx, %rax
	movzbl	(%rax), %edx
	movq	%rax, %rdi
	cmpq	$61, %rdx
	je	.LBB12_151
# BB#149:                               # %land.lhs.true490
	cmpq	$75, %rdx
	jne	.LBB12_150
.LBB12_151:                             # %land.lhs.true.i929
	movzbl	(%rbx), %eax
	cmpq	$61, %rax
	je	.LBB12_153
# BB#152:                               # %land.lhs.true.i929
	cmpq	$75, %rax
	jne	.LBB12_150
.LBB12_153:                             # %if.then.i931
	movq	%rsi, %r9
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movabsq	$const_int_rtx+512, %rcx
	cmpq	$75, %rdx
	movq	%rdi, %r11
	movq	%rcx, %rsi
	jne	.LBB12_155
# BB#154:                               # %if.then14.i936
	movq	8(%rdi), %r11
	leaq	16(%rdi), %rsi
.LBB12_155:                             # %if.end.i941
	movq	(%rsi), %r8
	cmpq	$75, %rax
	movq	%rbx, %r10
	jne	.LBB12_157
# BB#156:                               # %if.then21.i946
	movq	8(%rbx), %r10
	leaq	16(%rbx), %rcx
.LBB12_157:                             # %if.end29.i953
	movzbl	(%r11), %esi
	movq	(%rcx), %r12
	cmpq	$61, %rsi
	jne	.LBB12_160
# BB#158:                               # %land.lhs.true33.i957
	cmpq	%r10, %r11
	je	.LBB12_165
# BB#159:                               # %land.lhs.true33.i957
	cmpq	%r12, %r11
	je	.LBB12_165
.LBB12_160:                             # %if.else38.i961
	movzbl	(%r8), %ecx
	cmpq	$61, %rcx
	jne	.LBB12_161
# BB#162:                               # %land.lhs.true42.i965
	cmpq	%r10, %r8
	je	.LBB12_163
# BB#164:                               # %land.lhs.true42.i965
	cmpq	%r12, %r8
	movq	%r8, %r11
	je	.LBB12_165
.LBB12_161:
	movq	%rbx, %r12
	movq	%rdi, %rax
	movq	%r9, %rsi
	jmp	.LBB12_180
.LBB12_147:
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %r12
	movq	%rdx, %rax
	jmp	.LBB12_180
.LBB12_150:
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %r12
	movq	%rdi, %rax
	jmp	.LBB12_180
.LBB12_163:
	movq	%r8, %r11
.LBB12_165:                             # %find_common_reg_term.exit968
	movq	%rbx, %rcx
	movq	%r15, %rsi
	testq	%r11, %r11
	je	.LBB12_166
# BB#167:                               # %if.then493
	cmpq	%r11, %rdi
	je	.LBB12_168
# BB#171:                               # %if.end.i981
	cmpq	$75, %rdx
	jne	.LBB12_175
# BB#172:                               # %if.then2.i987
	movq	%rdi, %rdx
	movq	8(%rdx), %rdi
	movq	16(%rdx), %rdx
	cmpq	%r11, %rdi
	je	.LBB12_173
# BB#174:                               # %if.else.i989
	cmpq	%r11, %rdx
	je	.LBB12_169
	jmp	.LBB12_175
.LBB12_166:
	movq	%rcx, %r12
	movq	%rdi, %rax
	movq	%rsi, %r15
	movq	%r9, %rsi
	jmp	.LBB12_180
.LBB12_168:                             # %if.then.i977
	movq	const_int_rtx+512(%rip), %rdi
	jmp	.LBB12_169
.LBB12_173:
	movq	%rdx, %rdi
.LBB12_169:                             # %subtract_reg_term.exit992
	cmpq	%r11, %rcx
	je	.LBB12_170
# BB#176:                               # %if.end.i973
	cmpq	$75, %rax
	movq	%rsi, %rax
	movq	%r9, %rdx
	movq	%rdi, %rsi
	jne	.LBB12_175
# BB#177:                               # %if.then2.i
	movq	%rsi, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	movq	%rcx, %rax
	movq	8(%rax), %r12
	movq	16(%rax), %rax
	cmpq	%r11, %r12
	je	.LBB12_178
# BB#179:                               # %if.else.i
	cmpq	%r11, %rax
	movq	%rdx, %r15
	movq	%rdi, %rax
	je	.LBB12_180
.LBB12_175:                             # %if.end18.i990
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.subtract_reg_term, %rcx
	movq	$3476, %rsi             # imm = 0xD94
	jmp	.LBB12_25
.LBB12_170:                             # %if.then.i970
	movq	const_int_rtx+512(%rip), %r12
	movq	%rsi, %r15
	movq	%r9, %rsi
	movq	%rdi, %rax
	jmp	.LBB12_180
.LBB12_178:
	movq	%rax, %r12
	movq	%rdx, %r15
	movq	%rdi, %rax
.LBB12_180:                             # %if.end496
	movq	%rax, 56(%r14)
	movq	%r12, 64(%r14)
	cmpq	$103, 48(%rsp)          # 8-byte Folded Reload
	jne	.LBB12_182
# BB#181:                               # %if.then501
	movq	$0, 48(%r14)
	movq	$0, 64(%r14)
.LBB12_182:                             # %if.end504
	movq	%rax, 72(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB12_183
# BB#185:                               # %if.end512
	movl	(%rsi), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$54, %rcx
	je	.LBB12_186
# BB#187:                               # %if.end512
	cmpq	$61, %rcx
	je	.LBB12_190
# BB#188:                               # %if.end512
	cmpq	$63, %rcx
	jne	.LBB12_189
.LBB12_190:                             # %if.then527
	movq	192(%r13), %rdi
	callq	loop_find_equiv_value
	movl	(%rax), %edx
	movq	%rax, %r13
	jmp	.LBB12_191
.LBB12_183:                             # %if.then507
	movq	loop_dump_stream(%rip), %rcx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB12_234
# BB#184:                               # %if.then509
	movabsq	$.L.str49, %rdi
	movq	$54, %rsi
	jmp	.LBB12_7
.LBB12_186:
	movq	%rsi, %r13
	jmp	.LBB12_196
.LBB12_189:
	movq	%rsi, %r13
.LBB12_191:                             # %if.end529
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$54, %rdx
	jne	.LBB12_192
# BB#195:                               # %if.end540
	movq	%r13, 80(%r14)
.LBB12_196:                             # %if.end542
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB12_197
# BB#199:                               # %if.else553
	cmpq	$103, 48(%rsp)          # 8-byte Folded Reload
	jne	.LBB12_203
# BB#200:                               # %if.then556
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB12_234
# BB#201:                               # %if.then558
	movabsq	$.L.str52, %rcx
	movq	$37, %rsi
	jmp	.LBB12_202
.LBB12_192:                             # %if.then534
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB12_234
# BB#193:                               # %if.then536
	movabsq	$.L.str50, %rcx
	movq	$46, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	movq	loop_dump_stream(%rip), %rax
	movq	%rax, %rdi
	movq	%r13, %rsi
	jmp	.LBB12_194
.LBB12_197:                             # %if.then547
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB12_234
# BB#198:                               # %if.then549
	movabsq	$.L.str51, %rcx
	movq	$44, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	movq	loop_dump_stream(%rip), %rax
	movq	%rax, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB12_194
.LBB12_203:                             # %if.else561
	movzbl	(%r12), %eax
	cmpq	$54, %rax
	jne	.LBB12_204
# BB#206:                               # %if.end574
	movq	8(%r12), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB12_211
# BB#207:                               # %if.then576
	cmpq	%rcx, %rax
	jbe	.LBB12_208
# BB#209:                               # %if.then576
	movabsq	$1, %rdx
	jmp	.LBB12_210
.LBB12_204:                             # %if.then566
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB12_234
# BB#205:                               # %if.then568
	movabsq	$.L.str53, %rcx
	movq	$42, %rsi
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	movq	loop_dump_stream(%rip), %rax
	movq	%rax, %rdi
	movq	%r12, %rsi
.LBB12_194:                             # %return
	callq	print_simple_rtl
	movq	loop_dump_stream(%rip), %rax
	movabsq	$.L.str19, %rcx
	movq	$2, %rsi
	jmp	.LBB12_202
.LBB12_211:                             # %if.else594
	cmpq	%rcx, %rax
	movabsq	$0, %rsi
	movabsq	$1, %rdi
	movq	%rsi, %rdx
	jle	.LBB12_213
# BB#212:                               # %if.else594
	movq	%rdi, %rdx
.LBB12_213:                             # %if.else594
	jge	.LBB12_215
# BB#214:                               # %if.else594
	movq	%rdi, %rsi
	jmp	.LBB12_215
.LBB12_208:
	movabsq	$0, %rdx
.LBB12_210:                             # %if.then576
	sbbq	%rsi, %rsi
	andq	$1, %rsi
.LBB12_215:                             # %if.else594
	subq	%rsi, %rdx
	movq	8(%r13), %rsi
	testq	%rsi, %rsi
	movq	$1, %rbx
	jg	.LBB12_217
# BB#216:                               # %if.else619
	movq	%rsi, %rdi
	negq	%rdi
	sbbq	%rbx, %rbx
.LBB12_217:                             # %if.end628
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rbx
	movq	%rdx, %r8
	andq	%rdi, %r8
	cmpq	%rbx, %r8
	jne	.LBB12_221
# BB#218:                               # %if.end628
	testq	%rdi, %rdx
	je	.LBB12_221
# BB#219:                               # %land.lhs.true634
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %r8
	je	.LBB12_223
# BB#220:                               # %land.lhs.true634
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB12_223
.LBB12_221:                             # %if.else641
	movq	loop_dump_stream(%rip), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB12_234
# BB#222:                               # %if.then643
	movabsq	$.L.str54, %rcx
	movq	$34, %rsi
.LBB12_202:                             # %return
	movq	$1, %rdx
	movq	%rcx, %rdi
	movq	%rax, %rcx
	callq	fwrite
	jmp	.LBB12_234
.LBB12_223:                             # %if.end646
	testq	%rsi, %rsi
	jle	.LBB12_225
# BB#224:                               # %if.then652
	subq	%rcx, %rax
	jmp	.LBB12_227
.LBB12_225:                             # %if.else660
	jns	.LBB12_235
# BB#226:                               # %if.then663
	subq	%rax, %rcx
	negq	%rsi
	movq	%rcx, %rax
.LBB12_227:                             # %if.end674
	movzbl	2(%r15), %ecx
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	addq	%rdi, %rcx
	movq	$2, %rdi
	shlq	%cl, %rdi
	decq	%rdi
	andq	%rax, %rdi
	xorq	%rbx, %rbx
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	%rsi
	movq	%rdx, %rax
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB12_229
# BB#228:
	movq	%rax, %rbx
	jmp	.LBB12_230
.LBB12_235:                             # %if.else672
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.loop_iterations, %rcx
	movq	$4090, %rsi             # imm = 0xFFA
.LBB12_25:                              # %cond.false76
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB12_229:                             # %land.lhs.true686
	testq	%rax, %rax
	jne	.LBB12_234
.LBB12_230:                             # %if.end690
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	%rsi
	testq	%rbx, %rbx
	je	.LBB12_231
# BB#232:                               # %if.end690
	movabsq	$1, %rbx
	jmp	.LBB12_233
.LBB12_231:
	movabsq	$0, %rbx
.LBB12_233:                             # %if.end690
	addq	%rax, %rbx
	movq	%rbx, 96(%r14)
	jmp	.LBB12_234
.Ltmp198:
	.size	loop_iterations, .Ltmp198-loop_iterations
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_91
	.quad	.LBB12_87
	.quad	.LBB12_86
	.quad	.LBB12_91
	.quad	.LBB12_94
	.quad	.LBB12_89
	.quad	.LBB12_85
	.quad	.LBB12_90
	.quad	.LBB12_93
	.quad	.LBB12_88

	.text
	.align	16, 0x90
	.type	loop_find_equiv_value,@function
loop_find_equiv_value:                  # @loop_find_equiv_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp205:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp206:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp207:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp208:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp209:
	.cfi_def_cfa_offset 48
.Ltmp210:
	.cfi_offset %rbx, -48
.Ltmp211:
	.cfi_offset %r12, -40
.Ltmp212:
	.cfi_offset %r13, -32
.Ltmp213:
	.cfi_offset %r14, -24
.Ltmp214:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	16(%r14), %rbx
	movq	%rsi, %r15
	testq	%rbx, %rbx
	je	.LBB13_6
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB13_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %eax
	cmpq	$36, %rax
	je	.LBB13_6
# BB#3:                                 # %if.else
                                        #   in Loop: Header=BB13_2 Depth=1
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	reg_set_p
	testq	%r13, %rax
	jne	.LBB13_7
.LBB13_5:                               # %for.cond.backedge
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_2
	jmp	.LBB13_6
.LBB13_7:                               # %if.then7
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_6
# BB#8:                                 # %cond.true
	movq	32(%rbx), %r12
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB13_10
# BB#9:                                 # %cond.false
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	single_set_2
	movq	%rax, %r12
.LBB13_10:                              # %cond.end31
	testq	%r12, %r12
	je	.LBB13_6
# BB#11:                                # %land.lhs.true34
	movq	8(%r12), %rax
	cmpq	%r15, %rax
	je	.LBB13_12
.LBB13_6:                               # %for.end
	movq	%r15, %rax
.LBB13_22:                              # %if.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_12:                              # %if.then40
	movq	$4, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB13_18
# BB#13:                                # %land.lhs.true43
	movq	8(%rax), %rdx
	movzbl	(%rdx), %ecx
	cmpq	$133, %rcx
	jg	.LBB13_16
# BB#14:                                # %land.lhs.true43
	addq	$-54, %rcx
	andq	%r13, %rcx
	cmpq	$14, %rcx
	ja	.LBB13_18
# BB#15:                                # %land.lhs.true43
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$24599, %rax            # imm = 0x6017
	jne	.LBB13_19
	jmp	.LBB13_18
.LBB13_16:                              # %land.lhs.true43
	cmpq	$134, %rcx
	je	.LBB13_19
# BB#17:                                # %land.lhs.true43
	cmpq	$140, %rcx
	je	.LBB13_19
.LBB13_18:                              # %if.else118
	movq	16(%r12), %rdx
.LBB13_19:                              # %if.end
	movq	%rdx, %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	modified_between_p
	testq	%r13, %rax
	je	.LBB13_21
# BB#20:                                # %if.end
	movq	%r15, %r12
.LBB13_21:                              # %if.end
	movq	%r12, %rax
	jmp	.LBB13_22
.Ltmp215:
	.size	loop_find_equiv_value, .Ltmp215-loop_find_equiv_value
	.cfi_endproc

	.align	16, 0x90
	.type	verify_addresses,@function
verify_addresses:                       # @verify_addresses
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp221:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp222:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp223:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp224:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp225:
	.cfi_def_cfa_offset 48
.Ltmp226:
	.cfi_offset %rbx, -40
.Ltmp227:
	.cfi_offset %r12, -32
.Ltmp228:
	.cfi_offset %r14, -24
.Ltmp229:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %rdi
	movq	40(%rbx), %rax
	movq	(%rax), %r14
	leaq	-1(%rdx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	imulq	%rsi, %rax
	movq	%rax, %rsi
	callq	plus_constant_wide
	movq	40(%rbx), %rcx
	movq	(%rcx), %rdi
	movq	(%rbx), %rdx
	movq	32(%rbx), %rsi
	movq	%rax, %r15
	callq	validate_replace_rtx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB14_2
# BB#1:                                 # %lor.lhs.false
	movq	40(%rbx), %rax
	movq	(%rbx), %rdx
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	validate_replace_rtx
	testq	%r12, %rax
	movq	$1, %r15
	jne	.LBB14_3
.LBB14_2:                               # %if.then
	xorq	%r15, %r15
.LBB14_3:                               # %if.end
	movq	40(%rbx), %rax
	movq	(%rbx), %rdx
	movq	(%rax), %rdi
	movq	%r14, %rsi
	callq	validate_replace_rtx
	testq	%r12, %rax
	je	.LBB14_5
# BB#4:                                 # %if.end13
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB14_5:                               # %if.then12
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.verify_addresses, %rcx
	movq	$2687, %rsi             # imm = 0xA7F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp230:
	.size	verify_addresses, .Ltmp230-verify_addresses
	.cfi_endproc

	.align	16, 0x90
	.type	calculate_giv_inc,@function
calculate_giv_inc:                      # @calculate_giv_inc
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
	subq	$32, %rsp
.Ltmp242:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, %r13
	movq	%rsi, 24(%rsp)
	xorq	%r15, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	jmp	.LBB15_1
	.align	16, 0x90
.LBB15_40:                              # %cond.end214
                                        #   in Loop: Header=BB15_1 Depth=1
	callq	get_last_insn
	movq	%rax, %rdi
	callq	delete_related_insns
	movq	$1, %r15
.LBB15_1:                               # %retry
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r13), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	je	.LBB15_8
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	24(%rsp), %rax
	movq	16(%rax), %rdi
	movq	%rdi, 24(%rsp)
	movq	32(%rdi), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	je	.LBB15_4
# BB#3:                                 # %cond.false
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	%r13, %rsi
	callq	single_set_2
	movq	%rax, %r13
.LBB15_4:                               # %cond.end24
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	16(%r13), %rax
	movzbl	(%rax), %eax
	cmpq	$75, %rax
	jne	.LBB15_5
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB15_1 Depth=1
	callq	get_last_insn
	movq	%rax, %rdi
	callq	delete_related_insns
	movq	16(%r13), %rax
.LBB15_8:                               # %if.end36
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	16(%rax), %rbx
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	je	.LBB15_31
# BB#9:                                 # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	leaq	24(%rsp), %rsi
	callq	find_last_value
	movq	%rax, %rbx
	movzbl	(%rbx), %r14d
	cmpq	$134, %r14
	jg	.LBB15_19
# BB#10:                                # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$74, %r14
	jg	.LBB15_14
# BB#11:                                # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$66, %r14
	jne	.LBB15_28
# BB#12:                                # %if.then61
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	24(%rsp), %rdi
	movq	$4, %rsi
	xorq	%rdx, %rdx
	callq	find_reg_note
	testq	%rax, %rax
	je	.LBB15_28
# BB#13:                                # %if.then63
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%rax), %rbx
	jmp	.LBB15_28
	.align	16, 0x90
.LBB15_19:                              # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$135, %r14
	jne	.LBB15_28
# BB#20:                                # %if.then53
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	16(%rbx), %rbx
	jmp	.LBB15_28
.LBB15_14:                              # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	leaq	-75(%r14), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$12, %rcx
	ja	.LBB15_28
# BB#15:                                # %if.then47
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$4609, %rax             # imm = 0x1201
	je	.LBB15_28
# BB#16:                                # %if.then82
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%rbx), %rdi
	movq	16(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	leaq	24(%rsp), %rsi
	callq	find_last_value
	movq	%rax, %rbx
	callq	get_last_insn
	movq	%rax, %rdi
	callq	delete_related_insns
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rcx), %eax
	cmpq	$54, %rax
	jne	.LBB15_18
# BB#17:                                # %lor.lhs.false98
                                        #   in Loop: Header=BB15_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB15_18
# BB#21:                                # %if.end104
                                        #   in Loop: Header=BB15_1 Depth=1
	cmpq	$84, %r14
	jne	.LBB15_23
# BB#22:                                # %if.then107
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%rbx), %rax
	movq	8(%rcx), %rsi
	orq	%rax, %rsi
	jmp	.LBB15_26
.LBB15_23:                              # %if.else114
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%rbx), %rsi
	movq	8(%rcx), %rax
	cmpq	$75, %r14
	jne	.LBB15_25
# BB#24:                                # %if.then117
                                        #   in Loop: Header=BB15_1 Depth=1
	addq	%rsi, %rax
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	jmp	.LBB15_27
.LBB15_25:                              # %if.else125
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	%rax, %rcx
	shlq	%cl, %rsi
.LBB15_26:                              # %if.end137
                                        #   in Loop: Header=BB15_1 Depth=1
	xorq	%rdi, %rdi
.LBB15_27:                              # %if.end137
                                        #   in Loop: Header=BB15_1 Depth=1
	callq	gen_rtx_CONST_INT
	movq	%rax, %rbx
	.align	16, 0x90
.LBB15_28:                              # %if.end137
                                        #   in Loop: Header=BB15_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB15_29
# BB#30:                                # %if.end143
                                        #   in Loop: Header=BB15_1 Depth=1
	callq	get_last_insn
	movq	%rax, %rdi
	callq	delete_related_insns
.LBB15_31:                              # %if.end146
                                        #   in Loop: Header=BB15_1 Depth=1
	testq	%r12, %r12
	jne	.LBB15_33
# BB#32:                                #   in Loop: Header=BB15_1 Depth=1
	movq	%rbx, %r12
	jmp	.LBB15_34
	.align	16, 0x90
.LBB15_33:                              # %if.then148
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	8(%r12), %rax
	movq	8(%rbx), %rsi
	addq	%rax, %rsi
	xorq	%rdi, %rdi
	callq	gen_rtx_CONST_INT
	movq	%rax, %r12
.LBB15_34:                              # %if.end158
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	16(%r13), %rax
	movq	8(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB15_36
# BB#35:                                # %lor.lhs.false169
                                        #   in Loop: Header=BB15_1 Depth=1
	movl	8(%rax), %eax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB15_42
.LBB15_36:                              # %if.then180
                                        #   in Loop: Header=BB15_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB15_41
# BB#37:                                # %if.then183
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	24(%rsp), %rax
	movq	16(%rax), %rdi
	movq	%rdi, 24(%rsp)
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	xorq	%r13, %r13
	cmpq	$105, %rax
	jne	.LBB15_40
# BB#38:                                # %cond.true194
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	32(%rdi), %r13
	movzbl	(%r13), %eax
	cmpq	$47, %rax
	je	.LBB15_40
# BB#39:                                # %cond.false206
                                        #   in Loop: Header=BB15_1 Depth=1
	movq	%r13, %rsi
	callq	single_set_2
	movq	%rax, %r13
	jmp	.LBB15_40
.LBB15_42:                              # %if.end219
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_5:                               # %if.then33
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.calculate_giv_inc, %rcx
	movq	$1605, %rsi             # imm = 0x645
	jmp	.LBB15_6
.LBB15_18:                              # %if.then103
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.calculate_giv_inc, %rcx
	movq	$1651, %rsi             # imm = 0x673
	jmp	.LBB15_6
.LBB15_29:                              # %if.then142
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.calculate_giv_inc, %rcx
	movq	$1662, %rsi             # imm = 0x67E
	jmp	.LBB15_6
.LBB15_41:                              # %if.end218
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.calculate_giv_inc, %rcx
	movq	$1695, %rsi             # imm = 0x69F
.LBB15_6:                               # %if.then33
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp248:
	.size	calculate_giv_inc, .Ltmp248-calculate_giv_inc
	.cfi_endproc

	.align	16, 0x90
	.type	initial_reg_note_copy,@function
initial_reg_note_copy:                  # @initial_reg_note_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp255:
	.cfi_def_cfa_offset 32
.Ltmp256:
	.cfi_offset %rbx, -32
.Ltmp257:
	.cfi_offset %r14, -24
.Ltmp258:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB16_7
# BB#1:                                 # %if.end
	movzbl	(%rbx), %edi
	callq	rtx_alloc
	movl	(%rbx), %edx
	movq	%rax, %r15
	movq	$16, %rcx
	shrq	%cl, %rdx
	movb	%dl, 2(%r15)
	movzbl	(%rbx), %eax
	cmpq	$4, %rax
	jne	.LBB16_2
# BB#5:                                 # %if.then16
	movq	8(%rbx), %rdi
	callq	copy_rtx
	jmp	.LBB16_6
.LBB16_7:                               # %return
	xorq	%rax, %rax
	jmp	.LBB16_8
.LBB16_2:                               # %if.end
	cmpq	$3, %rax
	jne	.LBB16_4
# BB#3:                                 # %if.then8
	movq	8(%rbx), %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	copy_rtx_and_substitute
.LBB16_6:                               # %if.end26
	movq	%rax, 8(%r15)
	movq	16(%rbx), %rdi
	movq	%r14, %rsi
	callq	initial_reg_note_copy
	movq	%rax, 16(%r15)
	movq	%r15, %rax
.LBB16_8:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB16_4:                               # %if.else24
	movabsq	$.L.str13, %rax
	movabsq	$.L__FUNCTION__.initial_reg_note_copy, %rcx
	movq	$1724, %rsi             # imm = 0x6BC
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp259:
	.size	initial_reg_note_copy, .Ltmp259-initial_reg_note_copy
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Unrolling failure: Loop too big.\n"
	.size	.L.str, 34

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Unrolling failure: cannot unroll EH regions.\n"
	.size	.L.str1, 46

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Unrolling failure: Unbalanced block notes.\n"
	.size	.L.str2, 44

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Loop unrolling: "
	.size	.L.str3, 17

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%lld"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" iterations.\n"
	.size	.L.str5, 14

	.type	factors,@object         # @factors
	.data
	.align	16
factors:
	.long	2                       # 0x2
	.long	0                       # 0x0
	.long	3                       # 0x3
	.long	0                       # 0x0
	.long	5                       # 0x5
	.long	0                       # 0x0
	.long	7                       # 0x7
	.long	0                       # 0x0
	.size	factors, 32

	.type	.L.str6,@object         # @.str6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str6:
	.asciz	"Loop unrolling: No factors found.\n"
	.size	.L.str6, 35

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Unrolling loop %d times.\n"
	.size	.L.str7, 26

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Unrolling failure: loop does not end with a JUMP_INSN.\n"
	.size	.L.str8, 56

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Splitting not safe, because loop not entered at top.\n"
	.size	.L.str9, 54

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Unrolling failure: unknown insns between BEG note and loop label.\n"
	.size	.L.str10, 67

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Unrolling failure: loop start label is gone\n"
	.size	.L.str11, 45

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Unrolling failure: final conditional branch not to loop start\n"
	.size	.L.str12, 63

	.type	splittable_regs,@object # @splittable_regs
	.local	splittable_regs
	.comm	splittable_regs,8,8
	.type	splittable_regs_updates,@object # @splittable_regs_updates
	.local	splittable_regs_updates
	.comm	splittable_regs_updates,8,8
	.type	addr_combined_regs,@object # @addr_combined_regs
	.local	addr_combined_regs
	.comm	addr_combined_regs,8,8
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"unroll.c"
	.size	.L.str13, 9

	.type	.L__FUNCTION__.unroll_loop,@object # @__FUNCTION__.unroll_loop
.L__FUNCTION__.unroll_loop:
	.asciz	"unroll_loop"
	.size	.L__FUNCTION__.unroll_loop, 12

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Marked reg %d as local\n"
	.size	.L.str14, 24

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Did not mark reg %d as local\n"
	.size	.L.str15, 30

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"unroll_loop_precondition"
	.size	.L.str16, 25

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Unrolling failure: Naive unrolling not being done.\n"
	.size	.L.str17, 52

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Preconditioning: Success, number of iterations known, "
	.size	.L.str18, 55

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	".\n"
	.size	.L.str19, 3

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"Preconditioning: Could not find iteration variable.\n"
	.size	.L.str20, 53

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Preconditioning: Could not find initial value.\n"
	.size	.L.str21, 48

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Preconditioning: Could not find increment value.\n"
	.size	.L.str22, 50

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Preconditioning: Increment not a constant.\n"
	.size	.L.str23, 44

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Preconditioning: Increment not a constant power of 2.\n"
	.size	.L.str24, 55

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Preconditioning: EQ comparison loop.\n"
	.size	.L.str25, 38

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Preconditioning: Final value not invariant.\n"
	.size	.L.str26, 45

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Preconditioning: Floating point final or initial value.\n"
	.size	.L.str27, 57

	.type	.L__FUNCTION__.precondition_loop_p,@object # @__FUNCTION__.precondition_loop_p
.L__FUNCTION__.precondition_loop_p:
	.asciz	"precondition_loop_p"
	.size	.L__FUNCTION__.precondition_loop_p, 20

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Preconditioning: Iteration var not live before loop start.\n"
	.size	.L.str28, 60

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Preconditioning: Successful.\n"
	.size	.L.str29, 30

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Final biv value for %d, reversed biv.\n"
	.size	.L.str30, 39

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"Final biv value for %d, calculated.\n"
	.size	.L.str31, 37

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Final biv value for %d, biv dead after loop exit.\n"
	.size	.L.str32, 51

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"Final giv value for %d, depends on reversed biv\n"
	.size	.L.str33, 49

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"Final giv value for %d, calc from biv's value.\n"
	.size	.L.str34, 48

	.type	.L__FUNCTION__.final_giv_value,@object # @__FUNCTION__.final_giv_value
.L__FUNCTION__.final_giv_value:
	.asciz	"final_giv_value"
	.size	.L__FUNCTION__.final_giv_value, 16

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Final giv value for %d, giv dead after loop exit.\n"
	.size	.L.str35, 51

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"Loop iterations: No final conditional branch found.\n"
	.size	.L.str36, 53

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"Loop iterations: Loop has multiple back edges.\n"
	.size	.L.str37, 48

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Loop iterations: Jump insn has null JUMP_LABEL.\n"
	.size	.L.str38, 49

	.type	.L__FUNCTION__.loop_iterations,@object # @__FUNCTION__.loop_iterations
.L__FUNCTION__.loop_iterations:
	.asciz	"loop_iterations"
	.size	.L__FUNCTION__.loop_iterations, 16

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"Loop iterations: No final comparison found.\n"
	.size	.L.str39, 45

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Loop iterations: Comparison not against register.\n"
	.size	.L.str40, 51

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Loop iterations: No reg_iv_type entry for iteration var.\n"
	.size	.L.str41, 58

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"Loop iterations: Iteration var rejected because mode too large.\n"
	.size	.L.str42, 65

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"Loop iterations: Iteration var not an integer.\n"
	.size	.L.str43, 48

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Loop iterations: Basic induction var not set once in each iteration.\n"
	.size	.L.str44, 70

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"Loop iterations: General induction var not set once in each iteration.\n"
	.size	.L.str45, 72

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"Loop iterations: Basic induction var add_val is REG %d.\n"
	.size	.L.str46, 57

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"Loop iterations: Giv iterator, initial value bias %ld.\n"
	.size	.L.str47, 56

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"Loop iterations: Not basic or general induction var.\n"
	.size	.L.str48, 54

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"Loop iterations: Increment value can't be calculated.\n"
	.size	.L.str49, 55

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"Loop iterations: Increment value not constant "
	.size	.L.str50, 47

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"Loop iterations: Initial value not constant "
	.size	.L.str51, 45

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"Loop iterations: EQ comparison loop.\n"
	.size	.L.str52, 38

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"Loop iterations: Final value not constant "
	.size	.L.str53, 43

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"Loop iterations: Not normal loop.\n"
	.size	.L.str54, 35

	.type	.L__FUNCTION__.subtract_reg_term,@object # @__FUNCTION__.subtract_reg_term
.L__FUNCTION__.subtract_reg_term:
	.asciz	"subtract_reg_term"
	.size	.L__FUNCTION__.subtract_reg_term, 18

	.type	.L__FUNCTION__.find_splittable_regs,@object # @__FUNCTION__.find_splittable_regs
.L__FUNCTION__.find_splittable_regs:
	.asciz	"find_splittable_regs"
	.size	.L__FUNCTION__.find_splittable_regs, 21

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"Biv %d initial value remapped to %d.\n"
	.size	.L.str55, 38

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"Biv %d safe to split.\n"
	.size	.L.str56, 23

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"Biv %d mapped to %d for split.\n"
	.size	.L.str57, 32

	.type	.L__FUNCTION__.find_splittable_givs,@object # @__FUNCTION__.find_splittable_givs
.L__FUNCTION__.find_splittable_givs:
	.asciz	"find_splittable_givs"
	.size	.L__FUNCTION__.find_splittable_givs, 21

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"giv combined with unreduced giv not split.\n"
	.size	.L.str58, 44

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"Sharing address givs in insn %d\n"
	.size	.L.str59, 33

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Eliminating constant from giv %d\n"
	.size	.L.str60, 34

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"Invalid address for giv at insn %d\n"
	.size	.L.str61, 36

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"Invalid init insn, rewritten.\n"
	.size	.L.str62, 31

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"DEST_ADDR giv being split.\n"
	.size	.L.str63, 28

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Giv %d at insn %d safe to split.\n"
	.size	.L.str64, 34

	.type	.L__FUNCTION__.verify_addresses,@object # @__FUNCTION__.verify_addresses
.L__FUNCTION__.verify_addresses:
	.asciz	"verify_addresses"
	.size	.L__FUNCTION__.verify_addresses, 17

	.type	.L__FUNCTION__.fold_rtx_mult_add,@object # @__FUNCTION__.fold_rtx_mult_add
.L__FUNCTION__.fold_rtx_mult_add:
	.asciz	"fold_rtx_mult_add"
	.size	.L__FUNCTION__.fold_rtx_mult_add, 18

	.type	.L__FUNCTION__.copy_loop_body,@object # @__FUNCTION__.copy_loop_body
.L__FUNCTION__.copy_loop_body:
	.asciz	"copy_loop_body"
	.size	.L__FUNCTION__.copy_loop_body, 15

	.type	.L__FUNCTION__.final_reg_note_copy,@object # @__FUNCTION__.final_reg_note_copy
.L__FUNCTION__.final_reg_note_copy:
	.asciz	"final_reg_note_copy"
	.size	.L__FUNCTION__.final_reg_note_copy, 20

	.type	.L__FUNCTION__.initial_reg_note_copy,@object # @__FUNCTION__.initial_reg_note_copy
.L__FUNCTION__.initial_reg_note_copy:
	.asciz	"initial_reg_note_copy"
	.size	.L__FUNCTION__.initial_reg_note_copy, 22

	.type	.L__FUNCTION__.calculate_giv_inc,@object # @__FUNCTION__.calculate_giv_inc
.L__FUNCTION__.calculate_giv_inc:
	.asciz	"calculate_giv_inc"
	.size	.L__FUNCTION__.calculate_giv_inc, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
