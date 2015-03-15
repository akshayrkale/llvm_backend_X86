	.file	"doio.c.bc"
	.text
	.globl	Perl_do_open
	.align	16, 0x90
	.type	Perl_do_open,@function
Perl_do_open:                           # @Perl_do_open
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	movq	32(%rsp), %rax
	movq	%rax, (%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	callq	Perl_do_openn
	addq	$24, %rsp
	retq
.Ltmp2:
	.size	Perl_do_open, .Ltmp2-Perl_do_open
	.cfi_endproc

	.globl	Perl_do_openn
	.align	16, 0x90
	.type	Perl_do_openn,@function
Perl_do_openn:                          # @Perl_do_openn
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
	subq	$176, %rsp
.Ltmp14:
	.cfi_def_cfa_offset 224
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
	movq	232(%rsp), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r9, 96(%rsp)           # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB1_4
# BB#1:                                 # %land.lhs.true
	movzbl	16(%rbx), %eax
	cmpq	$13, %rax
	jne	.LBB1_4
# BB#2:                                 # %land.lhs.true1
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_4
# BB#3:                                 # %cond.true
	movq	16(%rax), %r13
	testq	%r13, %r13
	jne	.LBB1_5
.LBB1_4:                                # %cond.false10
	movq	%rbx, %rdi
	callq	Perl_gv_IOadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %r13
.LBB1_5:                                # %cond.end
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	movl	%edx, 172(%rsp)
	movq	$0, 160(%rsp)
	movq	$1, %rax
	movl	%eax, PL_forkprocess(%rip)
	movq	PL_op(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$0, %rdi
	movabsq	$0, %rsi
	movabsq	$0, %rcx
	je	.LBB1_8
# BB#6:                                 # %land.lhs.true15
	movzbl	32(%rax), %ecx
	cmpq	$191, %rcx
	movabsq	$0, %rbx
	movabsq	$0, %rdi
	movabsq	$0, %rsi
	movabsq	$0, %rcx
	jne	.LBB1_8
# BB#7:                                 # %if.then
	movzbl	37(%rax), %ecx
	movq	%rcx, %rbx
	andq	$16, %rbx
	movq	%rcx, %rdi
	andq	$32, %rdi
	movq	%rcx, %rsi
	andq	$64, %rsi
	andq	$128, %rcx
.LBB1_8:                                # %if.end
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	(%r13), %rax
	movq	56(%rax), %rdi
	testq	%rdi, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	$32, %rbx
	movq	%rcx, %rsi
	movabsq	$0, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	je	.LBB1_35
# BB#9:                                 # %if.then32
	movq	%r12, 112(%rsp)         # 8-byte Spill
	callq	Perl_PerlIO_fileno
	movq	(%r13), %rdx
	movzbl	162(%rdx), %ebx
	movq	%rax, %r12
	xorq	%r14, %r14
	cmpq	$45, %rbx
	jne	.LBB1_11
# BB#10:
	movq	$32, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	xorq	%rdx, %rdx
	jmp	.LBB1_34
.LBB1_11:                               # %if.else
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_14
# BB#12:                                # %land.lhs.true43
	movq	$32, %rcx
	movq	%r12, %rax
	movq	PL_maxsysfd(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rsi, %rax
	jle	.LBB1_13
.LBB1_14:                               # %if.else53
	movq	56(%rdx), %rdi
	cmpq	$124, %rbx
	jne	.LBB1_16
# BB#15:                                # %if.then59
	movq	%r13, 104(%rsp)         # 8-byte Spill
	callq	Perl_my_pclose
	jmp	.LBB1_20
.LBB1_16:                               # %if.else64
	movq	64(%rdx), %rax
	cmpq	%rax, %rdi
	je	.LBB1_19
# BB#17:                                # %if.then71
	testq	%rax, %rax
	je	.LBB1_19
# BB#18:                                # %if.then75
	movq	%rax, %rdi
	callq	Perl_PerlIO_close
	movq	(%r13), %rcx
	movq	%r13, 104(%rsp)         # 8-byte Spill
	movq	56(%rcx), %rdi
	movq	%rax, %r13
	callq	Perl_PerlIO_close
	jmp	.LBB1_21
.LBB1_13:                               # %if.then47
	movq	56(%rdx), %r14
	movq	64(%rdx), %rdx
	jmp	.LBB1_33
.LBB1_19:                               # %if.else87
	movq	%r13, 104(%rsp)         # 8-byte Spill
	callq	Perl_PerlIO_close
.LBB1_20:                               # %if.end94
	movq	%rax, %r13
.LBB1_21:                               # %if.end94
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r13
	movq	$32, %rbx
	cmpq	%rsi, %r13
	je	.LBB1_23
# BB#22:
	xorq	%rdx, %rdx
	movq	104(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_34
.LBB1_23:                               # %land.lhs.true97
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	PL_maxsysfd(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$32, %rbx
	cmpq	%rax, %r12
	movq	104(%rsp), %r13         # 8-byte Reload
	jle	.LBB1_24
# BB#25:                                # %if.then101
	movq	%rsi, %r12
	movq	PL_stderrgv(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_29
# BB#26:                                # %land.lhs.true103
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB1_29
# BB#27:                                # %land.lhs.true108
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_29
# BB#28:                                # %land.lhs.true113
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB1_30
.LBB1_29:                               # %cond.false126
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
.LBB1_30:                               # %cond.end128
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_32
# BB#31:                                # %cond.end128
	movq	%rax, %rcx
.LBB1_32:                               # %cond.end128
	movq	(%rcx), %rax
	movq	64(%rax), %rdx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	PerlIO_printf
	xorq	%rdx, %rdx
	movq	$32, %rbx
.LBB1_33:                               # %if.end142
	movq	%r12, %rsi
	jmp	.LBB1_34
.LBB1_24:
	xorq	%rdx, %rdx
.LBB1_34:                               # %if.end142
	movq	%r14, 144(%rsp)         # 8-byte Spill
	movq	(%r13), %rax
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	112(%rsp), %r12         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
.LBB1_35:                               # %if.end147
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB1_45
# BB#36:                                # %if.then149
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_38
# BB#37:                                # %if.then152
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB1_38:                               # %if.end153
	testq	$67, %r15
	je	.LBB1_44
# BB#39:                                # %if.then156
	testq	$3, %r15
	je	.LBB1_40
# BB#42:                                # %if.then167
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_43
	jmp	.LBB1_44
.LBB1_45:                               # %if.else184
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rbx, 128(%rsp)         # 8-byte Spill
	movq	%r13, 104(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	callq	Perl_savepvn
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	Perl_save_freepv
	movabsq	$4294981120, %r15       # imm = 0x100003600
	movq	%r14, %rbx
	jmp	.LBB1_46
	.align	16, 0x90
.LBB1_381:                              # %for.inc
                                        #   in Loop: Header=BB1_46 Depth=1
	incq	%rbx
.LBB1_46:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_48
# BB#47:                                # %for.cond
                                        #   in Loop: Header=BB1_46 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	jne	.LBB1_381
.LBB1_48:                               # %while.cond.preheader
	movq	%r12, 112(%rsp)         # 8-byte Spill
	leaq	(%r14,%r13), %r12
	cmpq	%rbx, %r12
	jbe	.LBB1_53
# BB#49:
	xorq	%rax, %rax
.LBB1_50:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r12), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_53
# BB#51:                                # %land.rhs
                                        #   in Loop: Header=BB1_50 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r15, %rdx
	je	.LBB1_53
# BB#52:                                # %while.body
                                        #   in Loop: Header=BB1_50 Depth=1
	movb	%al, -1(%r12)
	decq	%r12
	cmpq	%rbx, %r12
	ja	.LBB1_50
.LBB1_53:                               # %while.end
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_58
# BB#54:                                # %if.then233
	movq	232(%rsp), %rax
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB1_56
# BB#55:                                # %cond.true237
	callq	Perl_savesvpv
	jmp	.LBB1_57
.LBB1_40:                               # %lor.lhs.false163
	testq	$64, %r15
	je	.LBB1_44
# BB#41:                                # %lor.lhs.false163
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB1_44
.LBB1_43:                               # %if.then169
	movabsq	$.L.str2, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB1_44:                               # %if.end172
	movq	$35, %rax
	movb	%al, 160(%rsp)
	leaq	161(%rsp), %rsi
	leaq	172(%rsp), %rdx
	movq	%r15, %rdi
	callq	PerlIO_intmode2str
	movq	(%r13), %rcx
	movb	%al, 162(%rcx)
	movq	%r12, %rdi
	callq	strlen
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 152(%rsp)
	leaq	152(%rsp), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%rsp)
	movq	$1, (%rsp)
	leaq	160(%rsp), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rdi, %rdi
	xorq	%r9, %r9
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rcx
	movq	96(%rsp), %r8           # 8-byte Reload
.LBB1_312:                              # %if.end1179
	callq	PerlIO_openn
	movq	%rax, %rbx
	xorq	%r14, %r14
.LBB1_314:                              # %if.end1179
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	testq	%rbx, %rbx
	je	.LBB1_315
# BB#324:                               # %if.end1220
	testq	%rax, %rax
	je	.LBB1_328
# BB#325:                               # %land.lhs.true1224
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB1_328
# BB#326:                               # %land.lhs.true1228
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB1_330
# BB#327:                               # %lor.lhs.false1232
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$4, %rax
	jne	.LBB1_330
.LBB1_328:                              # %lor.lhs.false1240
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB1_345
# BB#329:                               # %land.lhs.true1244
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_345
.LBB1_330:                              # %if.then1248
	movq	(%r13), %rax
	movzbl	162(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB1_334
# BB#331:                               # %land.lhs.true1254
	callq	Perl_PerlIO_stdout
	cmpq	%rax, %rbx
	je	.LBB1_339
# BB#332:                               # %lor.lhs.false1258
	callq	Perl_PerlIO_stderr
	cmpq	%rax, %rbx
	je	.LBB1_339
# BB#333:                               # %lor.lhs.false1258.if.else1280_crit_edge
	movq	(%r13), %rax
	movzbl	162(%rax), %eax
.LBB1_334:                              # %if.else1280
	andq	$255, %rax
	cmpq	$62, %rax
	jne	.LBB1_345
# BB#335:                               # %land.lhs.true1286
	callq	Perl_PerlIO_stdin
	cmpq	%rax, %rbx
	jne	.LBB1_345
# BB#336:                               # %if.then1290
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_338
# BB#337:                               # %if.then1290
	movq	%rax, %rcx
.LBB1_338:                              # %if.then1290
	movq	(%rcx), %rax
	movq	64(%rax), %rdx
	movabsq	$.L.str17, %rsi
	movq	$5, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
	jmp	.LBB1_345
.LBB1_315:                              # %if.then1181
	testq	%rax, %rax
	movq	%r12, %rdi
	jne	.LBB1_316
	jmp	.LBB1_319
.LBB1_58:                               # %if.else243
	movq	%r12, %rax
	subq	%rbx, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	jmp	.LBB1_59
.LBB1_56:                               # %cond.false239
	movabsq	$.L.str3, %rdi
	xorq	%rsi, %rsi
	callq	Perl_savepvn
.LBB1_57:                               # %cond.end241
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	Perl_save_freepv
	movq	%r13, 32(%rsp)          # 8-byte Spill
.LBB1_59:                               # %if.end244
	movzbl	(%rbx), %eax
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movb	%al, 162(%rcx)
	movzbl	(%rbx), %eax
	cmpq	$43, %rax
	jne	.LBB1_60
# BB#61:                                # %land.lhs.true250
	movzbl	1(%rbx), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$62, %rax
	movq	%r15, %rdi
	jne	.LBB1_248
# BB#62:                                # %land.lhs.true260
	leaq	1(%rbx), %r14
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_65
# BB#63:                                # %lor.lhs.false262
	cmpq	%r14, %r12
	jbe	.LBB1_248
# BB#64:                                # %land.lhs.true266
	movzbl	-1(%r12), %eax
	cmpq	$124, %rax
	je	.LBB1_248
.LBB1_65:                               # %if.then271
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	movq	$43, %rax
	je	.LBB1_67
# BB#66:                                # %if.then273
	movabsq	$.L.str4, %rsi
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
	movq	%r15, %rdi
	movzbl	(%rbx), %eax
.LBB1_67:                               # %if.end274
	movb	%al, 161(%rsp)
	movq	$1, %rax
	movl	%eax, 172(%rsp)
	movzbl	(%r14), %eax
	jmp	.LBB1_68
.LBB1_60:
	movq	%rbx, %r14
	movq	%r15, %rdi
.LBB1_68:                               # %if.end277
	andq	$255, %rax
	cmpq	$124, %rax
	movq	240(%rsp), %rbx
	je	.LBB1_75
# BB#69:                                # %if.end277
	cmpq	$62, %rax
	jne	.LBB1_70
# BB#127:                               # %if.then452
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_129
# BB#128:                               # %if.then454
	movabsq	$.L.str4, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB1_129:                              # %if.end455
	movzbl	1(%r14), %eax
	cmpq	$62, %rax
	jne	.LBB1_131
# BB#130:                               # %if.then460
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$97, %rcx
	movb	%cl, 162(%rax)
	addq	$2, %r14
	movb	%cl, 160(%rsp)
	jmp	.LBB1_132
.LBB1_75:                               # %if.then281
	testq	%rbx, %rbx
	je	.LBB1_76
# BB#77:                                # %if.then283
	movzbl	1(%r14), %eax
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	cmpq	$45, %rax
	je	.LBB1_79
	jmp	.LBB1_78
.LBB1_70:                               # %if.end277
	cmpq	$60, %rax
	jne	.LBB1_245
# BB#71:                                # %for.cond774.preheader
	addq	$2, %r14
	jmp	.LBB1_72
.LBB1_74:                               # %for.cond774.backedge
                                        #   in Loop: Header=BB1_72 Depth=1
	incq	%r14
.LBB1_72:                               # %for.cond774
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r14), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_222
# BB#73:                                # %for.cond774
                                        #   in Loop: Header=BB1_72 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdi, %rax
	jne	.LBB1_74
.LBB1_222:                              # %for.end798
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	$114, %rax
	movb	%al, 160(%rsp)
	leaq	-1(%r14), %rbx
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	je	.LBB1_224
# BB#223:                               # %if.then801
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$98, %rcx
	jmp	.LBB1_226
.LBB1_339:                              # %if.then1262
	callq	Perl_PerlIO_stdout
	cmpq	%rax, %rbx
	je	.LBB1_340
# BB#341:                               # %if.then1262
	movabsq	$.L.str16, %rdx
	jmp	.LBB1_342
.LBB1_131:                              # %if.else465
	incq	%r14
	movq	$119, %rax
	movb	%al, 160(%rsp)
.LBB1_132:                              # %if.end467
	movq	%r14, %rbx
	movq	$1, %rax
	movl	%eax, 172(%rsp)
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	$255, %rax
	je	.LBB1_134
# BB#133:                               # %if.then469
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$98, %rcx
	jmp	.LBB1_136
.LBB1_245:                              # %if.else904
	testq	%rbx, %rbx
	je	.LBB1_247
# BB#246:                               # %if.else904
	cmpq	$45, %rax
	jne	.LBB1_247
# BB#249:                               # %land.lhs.true911
	movzbl	1(%r14), %eax
	cmpq	$124, %rax
	je	.LBB1_257
# BB#250:
	leaq	1(%r14), %rax
	jmp	.LBB1_251
.LBB1_340:
	movabsq	$.L.str15, %rdx
.LBB1_342:                              # %if.then1262
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_344
# BB#343:                               # %if.then1262
	movq	%rax, %rcx
.LBB1_344:                              # %if.then1262
	movq	(%rcx), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str14, %rsi
	movq	$5, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB1_345:                              # %if.end1306
	movq	%rbx, %rdi
	callq	Perl_PerlIO_fileno
	movq	(%r13), %rcx
	movzbl	162(%rcx), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	je	.LBB1_353
# BB#346:                               # %if.end1306
	cmpq	$124, %rcx
	je	.LBB1_353
# BB#347:                               # %land.lhs.true1318
	cmpq	$45, %rcx
	je	.LBB1_353
# BB#348:                               # %land.lhs.true1318
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_353
# BB#349:                               # %if.then1327
	movabsq	$PL_statbuf, %rsi
	movq	%r15, %rdi
	callq	fstat
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_350
# BB#351:                               # %if.end1333
	movl	PL_statbuf+24(%rip), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$49152, %rax            # imm = 0xC000
	jne	.LBB1_353
# BB#352:                               # %if.then1337
	movq	(%r13), %rax
	movq	$115, %rcx
	movb	%cl, 162(%rax)
.LBB1_353:                              # %if.end1341
	cmpq	$0, 144(%rsp)           # 8-byte Folded Reload
	je	.LBB1_368
# BB#354:                               # %if.then1343
	movq	%rbx, 112(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB1_357
# BB#355:                               # %if.then1345
	movq	%rdi, %rbx
	callq	Perl_PerlIO_flush
	movq	%rbx, %rdi
	movq	144(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB1_357
# BB#356:                               # %if.then1349
	movq	%rdi, %rbx
	callq	Perl_PerlIO_close
	movq	%rbx, %rdi
.LBB1_357:                              # %if.end1352
	movq	%rdi, 136(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	movq	80(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	je	.LBB1_367
# BB#358:                               # %if.then1355
	movq	%r15, %rdi
	movq	%rdx, %rsi
	movq	%rsi, %r12
	callq	dup2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_359
# BB#360:                               # %if.end1361
	movq	PL_fdpid(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$1, %rdx
	movq	%r15, %rsi
	callq	Perl_av_fetch
	movq	(%rax), %rbx
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB1_362
# BB#361:                               # %lor.rhs1368
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
.LBB1_362:                              # %lor.end1372
	movq	(%rbx), %rax
	movq	24(%rax), %rbx
	movq	$0, 24(%rax)
	movq	PL_fdpid(%rip), %rdi
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rdx
	callq	Perl_av_fetch
	movq	(%rax), %r15
	movzbl	16(%r15), %eax
	testq	%rax, %rax
	jne	.LBB1_364
# BB#363:                               # %lor.rhs1383
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
.LBB1_364:                              # %lor.end1387
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	(%r15), %rax
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 24(%rax)
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB1_366
# BB#365:                               # %if.then1393
	movq	112(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	dup
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	Perl_PerlIO_close
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	dup2
	movq	%r12, %rdi
	callq	close
	jmp	.LBB1_367
.LBB1_359:                              # %if.then1359
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	Perl_PerlIO_close
	jmp	.LBB1_379
.LBB1_366:                              # %if.else1399
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	Perl_PerlIO_close
.LBB1_367:                              # %if.end1402
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_PerlIO_clearerr
	movq	%rbx, %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %r15
.LBB1_368:                              # %if.end1404
	movq	(%r13), %rax
	movq	%rbx, 56(%rax)
	movq	%rbx, %r12
	movzbl	163(%rax), %ecx
	andq	$223, %rcx
	movb	%cl, 163(%rax)
	movl	172(%rsp), %ecx
	testq	%rcx, %rcx
	movq	$1, %rbx
	je	.LBB1_380
# BB#369:                               # %if.then1412
	movzbl	162(%rax), %ecx
	cmpq	$115, %rcx
	je	.LBB1_373
# BB#370:                               # %lor.lhs.false1418
	cmpq	$62, %rcx
	jne	.LBB1_384
# BB#371:                               # %lor.lhs.false1418
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB1_384
# BB#372:                               # %land.lhs.true1427
	movl	PL_statbuf+24(%rip), %ecx
	andq	$61440, %rcx            # imm = 0xF000
	cmpq	$8192, %rcx             # imm = 0x2000
	jne	.LBB1_384
.LBB1_373:                              # %if.then1431
	movzbl	160(%rsp), %eax
	cmpq	$35, %rax
	je	.LBB1_376
# BB#374:                               # %if.then1431
	cmpq	$73, %rax
	jne	.LBB1_375
.LBB1_376:                              # %if.then1440
	leaq	161(%rsp), %rsi
	jmp	.LBB1_377
.LBB1_384:                              # %if.else1452
	movq	%r12, 64(%rax)
	jmp	.LBB1_380
.LBB1_375:
	leaq	160(%rsp), %rsi
.LBB1_377:                              # %if.end1442
	movq	$119, %rax
	movb	%al, (%rsi)
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	PerlIO_openn
	movq	(%r13), %rcx
	testq	%rax, %rax
	movq	%rax, 64(%rcx)
	jne	.LBB1_380
# BB#378:                               # %if.then1447
	movq	%r12, %rdi
	callq	Perl_PerlIO_close
	movq	(%r13), %rax
	movq	$0, 56(%rax)
	jmp	.LBB1_379
.LBB1_76:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	jmp	.LBB1_80
.LBB1_134:                              # %if.else472
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB1_137
# BB#135:                               # %if.then474
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$116, %rcx
.LBB1_136:                              # %if.end478
	movw	%cx, 160(%rsp,%rax)
.LBB1_137:                              # %if.end478
	movzbl	(%rbx), %ecx
	cmpq	$38, %rcx
	je	.LBB1_138
# BB#200:                               # %for.cond654.preheader
	incq	%rbx
	movq	%rbx, %r14
	movq	240(%rsp), %rbx
	movabsq	$4294981120, %rdx       # imm = 0x100003600
	jmp	.LBB1_201
	.align	16, 0x90
.LBB1_382:                              # %for.inc676
                                        #   in Loop: Header=BB1_201 Depth=1
	movzbl	(%r14), %ecx
	incq	%r14
.LBB1_201:                              # %for.cond654
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	$45, %rcx
	ja	.LBB1_209
# BB#202:                               # %for.cond654
                                        #   in Loop: Header=BB1_201 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdx, %rax
	jne	.LBB1_382
# BB#203:                               # %for.cond654
	cmpq	$45, %rcx
	jne	.LBB1_209
# BB#204:                               # %land.lhs.true682
	movzbl	(%r14), %ecx
	cmpq	$58, %rcx
	ja	.LBB1_209
# BB#205:                               # %land.lhs.true682
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$288230380446692865, %rcx # imm = 0x400000100003601
	testq	%rcx, %rax
	je	.LBB1_209
# BB#206:                               # %if.then715
	callq	Perl_PerlIO_stdout
	movq	%rbx, %rdx
	movq	%rax, %rbx
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$45, %rcx
	movb	%cl, 162(%rax)
	cmpq	$2, %rdx
	movq	%rdx, %r15
	jl	.LBB1_207
# BB#208:                               # %if.then722
	movabsq	$.L.str12, %rdi
	movq	$45, %rsi
	xorq	%rax, %rax
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	callq	Perl_croak
	jmp	.LBB1_213
.LBB1_209:                              # %if.else724
	decq	%r14
	testq	%rbx, %rbx
	je	.LBB1_211
# BB#210:
	movq	232(%rsp), %rax
	jmp	.LBB1_212
.LBB1_350:                              # %if.then1331
	movq	%rbx, %rdi
	callq	Perl_PerlIO_close
	jmp	.LBB1_379
.LBB1_247:
	movq	%r14, %rbx
.LBB1_248:                              # %if.else904.lor.lhs.false916_crit_edge
	movq	%rbx, %r14
	leaq	1(%rbx), %rax
	movq	240(%rsp), %rbx
.LBB1_251:                              # %lor.lhs.false916
	testq	%rbx, %rbx
	jne	.LBB1_252
# BB#253:                               # %lor.lhs.false916
	movabsq	$1, %rcx
	jmp	.LBB1_254
.LBB1_252:
	movabsq	$0, %rcx
.LBB1_254:                              # %lor.lhs.false916
	cmpq	%rax, %r12
	jbe	.LBB1_298
# BB#255:                               # %lor.lhs.false916
	testq	%rcx, %rcx
	je	.LBB1_298
# BB#256:                               # %land.lhs.true922
	movzbl	-1(%r12), %eax
	cmpq	$124, %rax
	jne	.LBB1_298
.LBB1_257:                              # %if.then927
	testq	%rbx, %rbx
	movq	96(%rsp), %rax          # 8-byte Reload
	je	.LBB1_259
# BB#258:                               # %if.then929
	movzbl	(%rax), %ecx
	addq	$2, %r14
	movq	%rax, %r12
	jmp	.LBB1_266
.LBB1_298:                              # %if.else1109
	testq	%rbx, %rbx
	je	.LBB1_300
# BB#299:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB1_78:                               # %unknown_open_mode
	movq	%rbx, %r15
	movq	%rdi, %rbx
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	112(%rsp), %rdx         # 8-byte Reload
	callq	Perl_croak
	movq	%rbx, %rdi
	movq	%r15, %rbx
.LBB1_79:                               # %if.end290
	incq	%r14
.LBB1_80:                               # %if.end292
	incq	%r14
	movq	96(%rsp), %r15          # 8-byte Reload
	jmp	.LBB1_81
	.align	16, 0x90
.LBB1_83:                               # %for.cond294.backedge
                                        #   in Loop: Header=BB1_81 Depth=1
	incq	%r14
.LBB1_81:                               # %for.cond294
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_84
# BB#82:                                # %for.cond294
                                        #   in Loop: Header=BB1_81 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdi, %rax
	jne	.LBB1_83
.LBB1_84:                               # %for.end318
	testq	%rbx, %rbx
	jne	.LBB1_86
# BB#85:                                # %if.then320
	subq	%r14, %r12
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	%r14, %r15
.LBB1_86:                               # %if.end324
	movzbl	(%r15), %eax
	cmpq	$45, %rax
	movq	104(%rsp), %r13         # 8-byte Reload
	jne	.LBB1_87
# BB#96:                                # %land.lhs.true361
	testq	%rbx, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %rax
	movq	%rcx, %rdx
	je	.LBB1_98
# BB#97:                                # %land.lhs.true361
	movq	%rax, %rdx
.LBB1_98:                               # %land.lhs.true361
	movzbl	1(%r15), %esi
	testq	%rsi, %rsi
	jne	.LBB1_100
# BB#99:                                # %land.lhs.true361
	movq	%rax, %rcx
.LBB1_100:                              # %land.lhs.true361
	movzbl	PL_tainting(%rip), %eax
	orq	%rdx, %rcx
	jne	.LBB1_102
	jmp	.LBB1_104
.LBB1_87:                               # %if.end324
	testq	%rax, %rax
	jne	.LBB1_101
# BB#88:                                # %if.then328
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB1_92
# BB#89:                                # %land.lhs.true331
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB1_92
# BB#90:                                # %land.lhs.true335
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB1_94
# BB#91:                                # %lor.lhs.false339
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$16, %rax
	jne	.LBB1_94
.LBB1_92:                               # %lor.lhs.false346
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB1_95
# BB#93:                                # %land.lhs.true350
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_95
.LBB1_94:                               # %if.then354
	movabsq	$.L.str6, %rsi
	movq	$10, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB1_95:                               # %if.end355
	callq	__errno_location
	movq	$32, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB1_379
.LBB1_300:                              # %if.end1112
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$60, %rcx
	movb	%cl, 162(%rax)
	movq	%r14, %r12
	jmp	.LBB1_301
.LBB1_303:                              # %for.inc1137
                                        #   in Loop: Header=BB1_301 Depth=1
	incq	%r12
.LBB1_301:                              # %for.cond1115
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r12), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_304
# BB#302:                               # %for.cond1115
                                        #   in Loop: Header=BB1_301 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdi, %rax
	jne	.LBB1_303
.LBB1_304:                              # %for.end1139
	movq	$114, %rax
	movb	%al, 160(%rsp)
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	je	.LBB1_306
# BB#305:                               # %if.then1142
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$98, %rcx
	movw	%cx, 160(%rsp,%rax)
	movq	104(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_308
.LBB1_101:                              # %lor.lhs.false366
	movzbl	PL_tainting(%rip), %eax
	testq	%rbx, %rbx
	je	.LBB1_104
.LBB1_102:                              # %lor.lhs.false366
	testq	$255, %rax
	je	.LBB1_104
# BB#103:                               # %if.then370
	callq	Perl_taint_env
	movzbl	PL_tainting(%rip), %eax
.LBB1_104:                              # %if.end372
	testq	$255, %rax
	je	.LBB1_106
# BB#105:                               # %if.then374
	movabsq	$.L.str7, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB1_106:                              # %if.end375
	testq	%rbx, %rbx
	jne	.LBB1_115
# BB#107:                               # %land.lhs.true377
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	-1(%rax,%r15), %eax
	cmpq	$124, %rax
	jne	.LBB1_115
# BB#108:                               # %if.then382
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, -1(%r15,%rcx)
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB1_112
# BB#109:                               # %land.lhs.true387
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB1_112
# BB#110:                               # %land.lhs.true391
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB1_114
# BB#111:                               # %lor.lhs.false395
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$16, %rax
	jne	.LBB1_114
.LBB1_112:                              # %lor.lhs.false403
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB1_115
# BB#113:                               # %land.lhs.true407
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_115
.LBB1_114:                              # %if.then411
	movabsq	$.L.str8, %rsi
	movq	$10, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB1_115:                              # %if.end413
	movq	$119, %rax
	movb	%al, 160(%rsp)
	movq	$1, %rax
	movl	%eax, 172(%rsp)
	testq	$255, 72(%rsp)          # 8-byte Folded Reload
	je	.LBB1_117
# BB#116:                               # %if.then416
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$98, %rcx
	jmp	.LBB1_119
.LBB1_117:                              # %if.else419
	cmpq	$0, 48(%rsp)            # 8-byte Folded Reload
	je	.LBB1_120
# BB#118:                               # %if.then421
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$116, %rcx
.LBB1_119:                              # %if.end425
	movw	%cx, 160(%rsp,%rax)
.LBB1_120:                              # %if.end425
	cmpq	$2, %rbx
	jl	.LBB1_122
# BB#121:                               # %if.then428
	leaq	160(%rsp), %rdi
	movq	%rbx, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	callq	Perl_my_popen_list
	jmp	.LBB1_123
.LBB1_122:                              # %if.else432
	leaq	160(%rsp), %rsi
	movq	%r15, %rdi
	callq	Perl_my_popen
.LBB1_123:                              # %if.end435
	testq	%rbx, %rbx
	je	.LBB1_124
# BB#125:                               # %if.then437
	movzbl	(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB1_124
# BB#126:                               # %if.then439
	leaq	160(%rsp), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	PerlIO_apply_layers
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r15, %r12
	jne	.LBB1_379
	jmp	.LBB1_314
.LBB1_124:
	movq	%r15, %r12
	movq	%rax, %rbx
	jmp	.LBB1_314
.LBB1_259:                              # %if.else931
	leaq	-1(%r12), %rax
	xorq	%rdx, %rdx
	cmpq	%r14, %rax
	movb	%dl, -1(%r12)
	jbe	.LBB1_263
.LBB1_260:                              # %land.rhs936
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%rax), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_263
# BB#261:                               # %land.rhs936
                                        #   in Loop: Header=BB1_260 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	%rdi, %rsi
	je	.LBB1_263
# BB#262:                               # %while.body963
                                        #   in Loop: Header=BB1_260 Depth=1
	movb	%dl, -1(%rax)
	decq	%rax
	cmpq	%r14, %rax
	ja	.LBB1_260
	jmp	.LBB1_263
.LBB1_383:                              # %for.inc988
                                        #   in Loop: Header=BB1_263 Depth=1
	incq	%r14
.LBB1_263:                              # %for.cond966
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_265
# BB#264:                               # %for.cond966
                                        #   in Loop: Header=BB1_263 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdi, %rax
	jne	.LBB1_383
.LBB1_265:
	movq	%r14, %r12
.LBB1_266:                              # %if.end994
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$45, %rax
	jne	.LBB1_267
# BB#276:                               # %land.lhs.true1033
	movq	%rdi, %r15
	movzbl	1(%r12), %ecx
	movzbl	PL_tainting(%rip), %eax
	orq	%rbx, %rcx
	je	.LBB1_280
# BB#277:                               # %land.lhs.true1033
	testq	$255, %rax
	jne	.LBB1_279
	jmp	.LBB1_280
.LBB1_267:                              # %if.end994
	testq	$255, %rcx
	jne	.LBB1_278
# BB#268:                               # %if.then998
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB1_272
# BB#269:                               # %land.lhs.true1002
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB1_272
# BB#270:                               # %land.lhs.true1006
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB1_274
# BB#271:                               # %lor.lhs.false1010
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$16, %rax
	jne	.LBB1_274
.LBB1_272:                              # %lor.lhs.false1018
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB1_275
# BB#273:                               # %land.lhs.true1022
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_275
.LBB1_274:                              # %if.then1026
	movabsq	$.L.str6, %rsi
	movq	$10, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB1_275:                              # %if.end1027
	callq	__errno_location
	movq	$32, %rcx
	jmp	.LBB1_176
.LBB1_306:                              # %if.else1145
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	movq	104(%rsp), %r13         # 8-byte Reload
	je	.LBB1_308
# BB#307:                               # %if.then1147
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$116, %rcx
	movw	%cx, 160(%rsp,%rax)
.LBB1_308:                              # %if.end1151
	movzbl	(%r12), %eax
	cmpq	$45, %rax
	jne	.LBB1_311
# BB#309:                               # %land.lhs.true1155
	movzbl	1(%r12), %eax
	testq	%rax, %rax
	je	.LBB1_310
.LBB1_311:                              # %if.end1170
	movq	%r14, %rdi
	callq	strlen
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 152(%rsp)
	leaq	152(%rsp), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%rsp)
	movq	$1, (%rsp)
	leaq	160(%rsp), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_312
.LBB1_224:                              # %if.else804
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	je	.LBB1_227
# BB#225:                               # %if.then806
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$116, %rcx
.LBB1_226:                              # %if.end810
	movw	%cx, 160(%rsp,%rax)
.LBB1_227:                              # %if.end810
	movzbl	(%rbx), %eax
	cmpq	$38, %rax
	jne	.LBB1_228
.LBB1_138:
	movabsq	$4294981120, %rdx       # imm = 0x100003600
	movzbl	1(%rbx), %r13d
	leaq	1(%rbx), %rax
	addq	$2, %rbx
	cmpq	$61, %r13
	jne	.LBB1_139
# BB#140:                               # %duplicity
	movabsq	$1, %r15
	jmp	.LBB1_141
.LBB1_139:
	movabsq	$0, %r15
.LBB1_141:                              # %duplicity
	je	.LBB1_143
# BB#142:                               # %duplicity
	movq	%rax, %rbx
.LBB1_143:                              # %duplicity
	xorq	$1, %r15
	addq	%r15, %r15
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_144
# BB#147:                               # %if.else496
	cmpq	$2, %rax
	jl	.LBB1_149
# BB#148:                               # %if.then499
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movsbq	162(%rax), %rsi
	movq	%rdx, %r14
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	%r14, %rdx
	jmp	.LBB1_149
.LBB1_144:                              # %land.lhs.true491
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB1_149
# BB#145:                               # %land.lhs.true491
	movq	224(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_149
# BB#146:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%rbx, %r14
	xorq	%r15, %r15
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	224(%rsp), %rbx
	jmp	.LBB1_213
.LBB1_151:                              # %for.inc526
                                        #   in Loop: Header=BB1_149 Depth=1
	incq	%rbx
.LBB1_149:                              # %for.cond504
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_152
# BB#150:                               # %for.cond504
                                        #   in Loop: Header=BB1_149 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rdx, %rax
	jne	.LBB1_151
.LBB1_152:                              # %for.end528
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_157
# BB#153:                               # %land.lhs.true530
	movq	232(%rsp), %rax
	movq	(%rax), %rdi
	movq	16(%rdi), %rax
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB1_160
# BB#154:                               # %lor.lhs.false534
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_157
# BB#155:                               # %land.lhs.true538
	callq	Perl_looks_like_number
	testq	%rax, %rax
	je	.LBB1_156
# BB#159:                               # %land.lhs.true538.if.then541_crit_edge
	movq	232(%rsp), %rax
	movq	(%rax), %rdi
	movq	16(%rdi), %rax
.LBB1_160:                              # %if.then541
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB1_162
# BB#161:                               # %cond.true545
	movq	(%rdi), %rax
	movq	24(%rax), %r15
	jmp	.LBB1_163
.LBB1_228:                              # %if.end810
	cmpq	$45, %rax
	jne	.LBB1_233
# BB#229:                               # %land.lhs.true819
	movzbl	(%r14), %ecx
	cmpq	$58, %rcx
	ja	.LBB1_233
# BB#230:                               # %land.lhs.true819
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$288230380446692865, %rcx # imm = 0x400000100003601
	testq	%rcx, %rax
	je	.LBB1_233
# BB#231:                               # %if.then852
	callq	Perl_PerlIO_stdin
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rax, %rbx
	movq	$45, %rax
	movb	%al, 162(%rcx)
	movq	240(%rsp), %rax
	cmpq	$2, %rax
	jl	.LBB1_237
# BB#232:                               # %if.then859
	movabsq	$.L.str13, %rdi
	movq	$45, %rsi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB1_237
.LBB1_233:                              # %if.else861
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_235
# BB#234:
	movq	232(%rsp), %rax
	movq	240(%rsp), %rdi
	jmp	.LBB1_236
.LBB1_211:                              # %if.then726
	movq	%r14, %rdi
	callq	strlen
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 152(%rsp)
	leaq	152(%rsp), %rax
	movq	$1, %rbx
	xorq	%r14, %r14
.LBB1_212:                              # %if.end730
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%rsp)
	movq	%rbx, (%rsp)
	movq	%rbx, %r15
	leaq	160(%rsp), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r14, %rdi
	callq	PerlIO_openn
	movq	%rax, %rbx
	jmp	.LBB1_213
.LBB1_278:                              # %if.then1040
	movq	%rdi, %r15
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB1_282
.LBB1_279:                              # %if.then1042
	callq	Perl_taint_env
	movzbl	PL_tainting(%rip), %eax
.LBB1_280:                              # %if.end1044
	testq	$255, %rax
	je	.LBB1_282
# BB#281:                               # %if.then1046
	movabsq	$.L.str7, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB1_282:                              # %if.end1047
	movq	$114, %rax
	movb	%al, 160(%rsp)
	testq	$255, 56(%rsp)          # 8-byte Folded Reload
	je	.LBB1_284
# BB#283:                               # %if.then1050
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$98, %rcx
	jmp	.LBB1_286
.LBB1_284:                              # %if.else1053
	testq	$255, 40(%rsp)          # 8-byte Folded Reload
	je	.LBB1_287
# BB#285:                               # %if.then1055
	leaq	160(%rsp), %rdi
	callq	strlen
	movq	$116, %rcx
.LBB1_286:                              # %if.end1059
	movw	%cx, 160(%rsp,%rax)
.LBB1_287:                              # %if.end1059
	cmpq	$2, %rbx
	jl	.LBB1_289
# BB#288:                               # %if.then1062
	leaq	160(%rsp), %rdi
	movq	%rbx, %rsi
	movq	232(%rsp), %rdx
	callq	Perl_my_popen_list
	jmp	.LBB1_290
.LBB1_289:                              # %if.else1066
	leaq	160(%rsp), %rsi
	movq	%r12, %rdi
	callq	Perl_my_popen
.LBB1_290:                              # %if.end1069
	movq	%rax, %rdx
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	testq	%rbx, %rbx
	movq	$124, %rcx
	movb	%cl, 162(%rax)
	movq	%r15, %rbx
	je	.LBB1_291
# BB#292:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_293
.LBB1_297:                              # %for.inc1096
                                        #   in Loop: Header=BB1_293 Depth=1
	incq	%r14
.LBB1_293:                              # %for.cond1074
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r14), %ecx
	cmpq	$32, %rcx
	ja	.LBB1_313
# BB#294:                               # %for.cond1074
                                        #   in Loop: Header=BB1_293 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rbx, %rax
	jne	.LBB1_297
# BB#295:                               # %for.cond1074
	testq	%rcx, %rcx
	je	.LBB1_296
.LBB1_313:                              # %if.then1100
	leaq	160(%rsp), %rsi
	movq	%rdx, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	PerlIO_apply_layers
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	104(%rsp), %r13         # 8-byte Reload
	jne	.LBB1_379
	jmp	.LBB1_314
.LBB1_291:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB1_296:
	movq	104(%rsp), %r13         # 8-byte Reload
	movq	%rdx, %rbx
	jmp	.LBB1_314
.LBB1_162:                              # %cond.false547
	callq	Perl_sv_2uv
	movq	%rax, %r15
.LBB1_163:                              # %cond.end549
	xorq	%r14, %r14
	jmp	.LBB1_164
.LBB1_310:                              # %if.then1160
	callq	Perl_PerlIO_stdin
	movq	(%r13), %rcx
	movq	%rax, %rbx
	movq	$45, %rax
	movb	%al, 162(%rcx)
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_314
.LBB1_235:                              # %if.then863
	movq	%rbx, %rdi
	callq	strlen
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 152(%rsp)
	leaq	152(%rsp), %rax
	movq	$1, %rdi
	xorq	%rbx, %rbx
.LBB1_236:                              # %if.end867
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%rsp)
	leaq	160(%rsp), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%rdi, (%rsp)
	movq	%rbx, %rdi
	callq	PerlIO_openn
	movq	%rbx, %r14
	movq	%rax, %rbx
.LBB1_237:                              # %if.end871
	testq	%rbx, %rbx
	movabsq	$4294981120, %rdi       # imm = 0x100003600
	jne	.LBB1_238
# BB#239:                               # %if.end871
	testq	%r14, %r14
	je	.LBB1_238
# BB#240:                               # %land.lhs.true875
	movzbl	(%r14), %eax
	testq	%rax, %rax
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB1_221
# BB#241:                               # %land.lhs.true875
	cmpq	$58, %rax
	je	.LBB1_221
# BB#242:                               # %land.lhs.true882
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_221
# BB#243:                               # %lor.lhs.false890
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_221
# BB#244:                               # %lor.lhs.false890
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_220
.LBB1_238:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_214
.LBB1_156:                              # %land.lhs.true538.if.else552_crit_edge
	movzbl	(%rbx), %ecx
.LBB1_157:                              # %if.else552
	addq	$-48, %rcx
	andq	$255, %rcx
	cmpq	$9, %rcx
	ja	.LBB1_168
# BB#158:                               # %if.then560
	movq	%rbx, %rdi
	callq	atoi
	movq	%rax, %r15
	movq	240(%rsp), %r14
.LBB1_164:                              # %if.else635
	movq	%r14, 24(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	je	.LBB1_166
# BB#165:                               # %if.else635
	movq	%rbx, %r14
.LBB1_166:                              # %if.else635
	cmpq	$61, %r13
	je	.LBB1_167
# BB#190:                               # %if.else635
	movabsq	$1, %rbx
	jmp	.LBB1_191
.LBB1_167:
	movabsq	$0, %rbx
.LBB1_191:                              # %if.else635
	jne	.LBB1_193
# BB#192:
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_194
.LBB1_193:                              # %if.then637
	movq	%r15, %rdi
	callq	dup
	movq	%rax, %r15
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB1_194:                              # %if.end640
	movq	232(%rsp), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%rsp)
	leaq	160(%rsp), %rsi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	PerlIO_openn
	testq	%rax, %rax
	movq	%rax, %rcx
	jne	.LBB1_195
# BB#196:                               # %if.end640
	movabsq	$1, %rax
	jmp	.LBB1_197
.LBB1_195:
	movabsq	$0, %rax
.LBB1_197:                              # %if.end640
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB1_198
# BB#199:                               # %if.then647
	movq	%r15, %rdi
	callq	close
	xorq	%rbx, %rbx
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	88(%rsp), %r13          # 8-byte Reload
	jmp	.LBB1_213
.LBB1_198:
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	%rcx, %rbx
.LBB1_213:                              # %if.end735
	testq	%rbx, %rbx
	jne	.LBB1_214
# BB#215:                               # %if.end735
	testq	%r14, %r14
	je	.LBB1_214
# BB#216:                               # %land.lhs.true739
	movzbl	(%r14), %eax
	testq	%rax, %rax
	movabsq	$4294981120, %rdi       # imm = 0x100003600
	movq	%r15, %rbx
	je	.LBB1_221
# BB#217:                               # %land.lhs.true739
	cmpq	$58, %rax
	je	.LBB1_221
# BB#218:                               # %land.lhs.true746
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_221
# BB#219:                               # %lor.lhs.false754
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB1_221
.LBB1_220:                              # %lor.lhs.false754
	cmpq	$95, %rax
	jne	.LBB1_78
.LBB1_221:                              # %if.end1179.thread
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	movq	104(%rsp), %r13         # 8-byte Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB1_319
.LBB1_316:                              # %land.lhs.true1185
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB1_319
# BB#317:                               # %land.lhs.true1189
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB1_321
# BB#318:                               # %lor.lhs.false1193
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$4, %rax
	jne	.LBB1_321
.LBB1_319:                              # %lor.lhs.false1201
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB1_379
# BB#320:                               # %land.lhs.true1205
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB1_379
.LBB1_321:                              # %land.lhs.true1209
	movq	(%r13), %rax
	movzbl	162(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB1_379
# BB#322:                               # %land.lhs.true1215
	movq	$10, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB1_379
# BB#323:                               # %if.then1218
	movabsq	$PL_warn_nl, %rsi
	movabsq	$.L.str4, %rdx
	movq	$9, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
	jmp	.LBB1_379
.LBB1_214:
	movq	96(%rsp), %r12          # 8-byte Reload
	movq	104(%rsp), %r13         # 8-byte Reload
	jmp	.LBB1_314
.LBB1_168:                              # %if.else562
	movq	240(%rsp), %rax
	testq	%rax, %rax
	je	.LBB1_170
# BB#169:                               # %if.then564
	movq	232(%rsp), %rax
	movq	(%rax), %rdi
	callq	Perl_sv_2io
.LBB1_174:                              # %if.end585
	testq	%rax, %rax
	je	.LBB1_175
# BB#177:                               # %if.end589
	movq	(%rax), %rcx
	movq	56(%rcx), %r14
	testq	%r14, %r14
	je	.LBB1_178
# BB#179:                               # %if.then593
	movq	%rax, %r13
	movq	%r14, %rdi
	callq	Perl_PerlIO_flush
	movq	%r14, %rdi
	callq	Perl_PerlIO_fileno
	callq	Perl_PerlIO_stdout
	cmpq	%rax, %r14
	je	.LBB1_181
# BB#180:                               # %lor.lhs.false599
	callq	Perl_PerlIO_stderr
	cmpq	%rax, %r14
	je	.LBB1_181
# BB#182:                               # %if.else606
	callq	Perl_PerlIO_stdin
	movq	%r14, %rdi
	cmpq	%rax, %r14
	je	.LBB1_183
# BB#184:                               # %if.else613
	movq	(%r13), %rax
	movzbl	162(%rax), %eax
	cmpq	$115, %rax
	jne	.LBB1_187
# BB#185:                               # %if.then619
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$115, %rcx
	jmp	.LBB1_186
.LBB1_170:                              # %if.else566
	xorq	%rsi, %rsi
	movq	$15, %rdx
	movq	%rbx, %rdi
	callq	Perl_gv_fetchpv
	testq	%rax, %rax
	je	.LBB1_175
# BB#171:                               # %land.lhs.true569
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB1_175
# BB#172:                               # %land.lhs.true574
	movq	(%rax), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_175
# BB#173:                               # %cond.true578
	movq	16(%rax), %rax
	jmp	.LBB1_174
.LBB1_175:                              # %if.then587
	callq	__errno_location
	movq	$22, %rcx
.LBB1_176:                              # %say_false
	movl	%ecx, (%rax)
	movq	104(%rsp), %r13         # 8-byte Reload
.LBB1_379:                              # %say_false
	movq	(%r13), %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 56(%rax)
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 64(%rax)
	movq	128(%rsp), %rcx         # 8-byte Reload
	movb	%cl, 162(%rax)
	xorq	%rbx, %rbx
.LBB1_380:                              # %return
	movq	%rbx, %rax
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_178:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	240(%rsp), %r14
	jmp	.LBB1_164
.LBB1_181:                              # %if.then603
	movq	%r14, %rdi
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$62, %rcx
	jmp	.LBB1_186
.LBB1_207:
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_213
.LBB1_183:                              # %if.then610
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$60, %rcx
.LBB1_186:                              # %if.then633
	movb	%cl, 162(%rax)
.LBB1_187:                              # %if.then633
	movq	240(%rsp), %r14
	testq	%r14, %r14
	je	.LBB1_189
# BB#188:                               # %if.then633
	movq	%rbx, %r14
.LBB1_189:                              # %if.then633
	xorq	%rsi, %rsi
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rdx
	callq	PerlIO_fdupopen
	movq	240(%rsp), %rbx
	movq	%rbx, %r15
	movq	%rax, %rbx
	movq	88(%rsp), %r13          # 8-byte Reload
	jmp	.LBB1_213
.Ltmp20:
	.size	Perl_do_openn, .Ltmp20-Perl_do_openn
	.cfi_endproc

	.globl	Perl_do_open9
	.align	16, 0x90
	.type	Perl_do_open9,@function
Perl_do_open9:                          # @Perl_do_open9
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 48
	movq	56(%rsp), %rax
	movq	48(%rsp), %r10
	movq	%rax, 32(%rsp)
	leaq	32(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	%r10, (%rsp)
	movq	$1, 16(%rsp)
	callq	Perl_do_openn
	addq	$40, %rsp
	retq
.Ltmp23:
	.size	Perl_do_open9, .Ltmp23-Perl_do_open9
	.cfi_endproc

	.globl	Perl_nextargv
	.align	16, 0x90
	.type	Perl_nextargv,@function
Perl_nextargv:                          # @Perl_nextargv
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
	subq	$48, %rsp
.Ltmp35:
	.cfi_def_cfa_offset 96
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
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rbx
	cmpq	$0, PL_argvoutgv(%rip)
	jne	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str18, %rdi
	movq	$1, %rsi
	movq	$15, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, PL_argvoutgv(%rip)
.LBB3_2:                                # %if.end
	testq	%rbx, %rbx
	je	.LBB3_10
# BB#3:                                 # %land.lhs.true
	movq	(%rbx), %rax
	movzbl	163(%rax), %ecx
	movq	%rcx, %rdx
	andq	$3, %rdx
	cmpq	$3, %rdx
	jne	.LBB3_10
# BB#4:                                 # %if.then10
	andq	$253, %rcx
	movb	%cl, 163(%rax)
	cmpq	$0, PL_inplace(%rip)
	je	.LBB3_10
# BB#5:                                 # %if.then17
	movq	PL_argvout_stack(%rip), %rdi
	testq	%rdi, %rdi
	jne	.LBB3_7
# BB#6:                                 # %if.then19
	callq	Perl_newAV
	movq	%rax, %rdi
	movq	%rdi, PL_argvout_stack(%rip)
.LBB3_7:                                # %if.end21
	movq	PL_defoutgv(%rip), %rsi
	testq	%rsi, %rsi
	movq	%rsi, PL_Sv(%rip)
	je	.LBB3_9
# BB#8:                                 # %land.rhs
	incq	8(%rsi)
.LBB3_9:                                # %land.end
	callq	Perl_av_push
.LBB3_10:                               # %if.end25
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movzbl	PL_filemode+1(%rip), %eax
	testq	$12, %rax
	je	.LBB3_17
# BB#11:                                # %if.then28
	movq	PL_argvoutgv(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_15
# BB#12:                                # %land.lhs.true30
	movzbl	16(%rdi), %eax
	cmpq	$13, %rax
	jne	.LBB3_15
# BB#13:                                # %land.lhs.true33
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_15
# BB#14:                                # %cond.true
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB3_16
.LBB3_15:                               # %cond.false45
	callq	Perl_gv_IOadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
.LBB3_16:                               # %cond.end
	movq	(%rax), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_flush
	movq	PL_oldname(%rip), %rdi
	movl	PL_filemode(%rip), %esi
	callq	chmod
.LBB3_17:                               # %if.end53
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, PL_lastfd(%rip)
	xorq	%rbx, %rbx
	movl	%ebx, PL_filemode(%rip)
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB3_111
# BB#18:                                # %while.cond.preheader
	callq	Perl_av_len
	testq	%rax, %rax
	js	.LBB3_94
# BB#19:                                # %while.body.lr.ph
	movabsq	$0, %r12
	.align	16, 0x90
.LBB3_20:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_51 Depth 2
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rdi
	callq	Perl_av_shift
	movq	%rax, %r13
	movq	%r13, %rdi
	callq	Perl_save_freesv
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	$2, %rdx
	movq	%r13, %rsi
	callq	Perl_sv_setsv_flags
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	16(%rdi), %rax
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB3_22
# BB#21:                                # %if.then77
                                        #   in Loop: Header=BB3_20 Depth=1
	callq	Perl_mg_set
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	movq	16(%rdi), %rax
.LBB3_22:                               # %if.end82
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%rdi, PL_Sv(%rip)
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB3_23
# BB#24:                                # %cond.false93
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	$2, %rdx
	leaq	40(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	40(%rsp), %rdx
	movq	%rax, %rsi
	jmp	.LBB3_25
	.align	16, 0x90
.LBB3_23:                               # %cond.true90
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 40(%rsp)
	movq	(%rax), %rsi
.LBB3_25:                               # %cond.end95
                                        #   in Loop: Header=BB3_20 Depth=1
	cmpq	$0, PL_inplace(%rip)
	movq	%rsi, PL_oldname(%rip)
	movq	%r12, %rcx
	je	.LBB3_27
# BB#26:                                # %cond.end95
                                        #   in Loop: Header=BB3_20 Depth=1
	movabsq	$1, %rcx
.LBB3_27:                               # %cond.end95
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	callq	Perl_do_openn
	testq	$255, %rax
	jne	.LBB3_28
# BB#85:                                # %if.else282
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB3_89
# BB#86:                                # %lor.lhs.false286
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB3_89
# BB#87:                                # %lor.lhs.false290
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB3_93
# BB#88:                                # %land.lhs.true294
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$64, %rax
	je	.LBB3_93
	.align	16, 0x90
.LBB3_89:                               # %if.then302
                                        #   in Loop: Header=BB3_20 Depth=1
	callq	__errno_location
	movl	(%rax), %r15d
	movq	PL_oldname(%rip), %rdi
	movabsq	$PL_statbuf, %rsi
	callq	stat
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB3_92
# BB#90:                                # %land.lhs.true307
                                        #   in Loop: Header=BB3_20 Depth=1
	movl	PL_statbuf+24(%rip), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$32768, %rax            # imm = 0x8000
	jne	.LBB3_91
.LBB3_92:                               # %if.else312
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rbx
	movq	%r15, %rdi
	callq	strerror
	movq	%rax, %rcx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str26, %rsi
	movq	%rbx, %rdx
	callq	Perl_warner
	jmp	.LBB3_93
	.align	16, 0x90
.LBB3_28:                               # %if.then101
                                        #   in Loop: Header=BB3_20 Depth=1
	cmpq	$0, PL_inplace(%rip)
	je	.LBB3_84
# BB#29:                                # %if.then103
                                        #   in Loop: Header=BB3_20 Depth=1
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB3_31
# BB#30:                                # %if.then105
                                        #   in Loop: Header=BB3_20 Depth=1
	xorq	%rdi, %rdi
	movabsq	$.L.str19, %rsi
	callq	Perl_taint_proper
.LBB3_31:                               # %if.end106
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	40(%rsp), %rax
	cmpq	$1, %rax
	jne	.LBB3_34
# BB#32:                                # %land.lhs.true109
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rax
	movzbl	(%rax), %eax
	cmpq	$45, %rax
	je	.LBB3_33
.LBB3_34:                               # %if.end121
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%r12, %rbx
	movl	PL_statbuf+24(%rip), %eax
	movl	%eax, PL_filemode(%rip)
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$32768, %rax            # imm = 0x8000
	jne	.LBB3_35
# BB#48:                                # %if.end145
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_inplace(%rip), %r12
	movzbl	(%r12), %eax
	testq	%rax, %rax
	jne	.LBB3_49
# BB#62:                                # %if.else192
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rdi
	callq	unlink
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %r12
	jns	.LBB3_71
# BB#63:                                # %if.then196
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB3_67
# BB#64:                                # %lor.lhs.false200
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB3_67
# BB#65:                                # %lor.lhs.false204
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB3_68
# BB#66:                                # %land.lhs.true208
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$64, %rax
	je	.LBB3_68
.LBB3_67:                               # %if.then216
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rbx
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rcx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str23, %rsi
	movq	%rbx, %rdx
	callq	Perl_warner
.LBB3_68:                               # %if.end219
                                        #   in Loop: Header=BB3_20 Depth=1
	testq	%r14, %r14
	jne	.LBB3_42
# BB#69:                                #   in Loop: Header=BB3_20 Depth=1
	movq	PL_argvgv(%rip), %rax
	jmp	.LBB3_43
	.align	16, 0x90
.LBB3_35:                               # %if.then125
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB3_39
# BB#36:                                # %lor.lhs.false
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB3_39
# BB#37:                                # %lor.lhs.false131
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB3_40
# BB#38:                                # %land.lhs.true135
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$64, %rax
	je	.LBB3_40
.LBB3_39:                               # %if.then142
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rdx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str21, %rsi
	callq	Perl_warner
.LBB3_40:                               # %if.end143
                                        #   in Loop: Header=BB3_20 Depth=1
	testq	%r14, %r14
	movq	%rbx, %r12
	jne	.LBB3_42
# BB#41:                                #   in Loop: Header=BB3_20 Depth=1
	movq	PL_argvgv(%rip), %rax
	jmp	.LBB3_43
.LBB3_49:                               # %if.then147
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	$42, %rsi
	movq	%r12, %rdi
	callq	strchr
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB3_53
# BB#50:                                # %if.then150
                                        #   in Loop: Header=BB3_20 Depth=1
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movabsq	$.L.str3, %rsi
	callq	Perl_sv_setpvn
	.align	16, 0x90
.LBB3_51:                               # %do.body
                                        #   Parent Loop BB3_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rdx
	subq	%r12, %rdx
	movq	$2, %rcx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_catpvn_flags
	movq	PL_oldname(%rip), %rsi
	movq	40(%rsp), %rdx
	movq	$2, %rcx
	movq	%r13, %rdi
	callq	Perl_sv_catpvn_flags
	movq	%r15, %r12
	incq	%r12
	movq	$42, %rsi
	movq	%r12, %rdi
	callq	strchr
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB3_51
# BB#52:                                # %do.end
                                        #   in Loop: Header=BB3_20 Depth=1
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB3_54
.LBB3_53:                               # %if.else
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_catpv
.LBB3_54:                               # %if.end156
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	(%r13), %rax
	movq	(%rax), %rdi
	callq	unlink
	movq	(%r13), %rax
	movq	PL_oldname(%rip), %rdi
	movq	(%rax), %rsi
	callq	link
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB3_70
# BB#55:                                # %if.then165
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	movq	%rbx, %r12
	je	.LBB3_59
# BB#56:                                # %lor.lhs.false169
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB3_59
# BB#57:                                # %lor.lhs.false173
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB3_60
# BB#58:                                # %land.lhs.true177
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$64, %rax
	je	.LBB3_60
.LBB3_59:                               # %if.then185
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %r15
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rbx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str22, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	Perl_warner
.LBB3_60:                               # %if.end188
                                        #   in Loop: Header=BB3_20 Depth=1
	testq	%r14, %r14
	jne	.LBB3_42
# BB#61:                                #   in Loop: Header=BB3_20 Depth=1
	movq	PL_argvgv(%rip), %rax
	jmp	.LBB3_43
.LBB3_91:                               # %if.then311
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rdx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str21, %rsi
	callq	Perl_warner
	jmp	.LBB3_93
.LBB3_70:                               # %if.end190
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rdi
	callq	unlink
	movq	%rbx, %r12
.LBB3_71:                               # %if.end222
                                        #   in Loop: Header=BB3_20 Depth=1
	cmpq	$0, PL_inplace(%rip)
	movq	%r12, %rdx
	jne	.LBB3_73
# BB#72:                                # %if.end222
                                        #   in Loop: Header=BB3_20 Depth=1
	movabsq	$1, %rdx
.LBB3_73:                               # %if.end222
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str24, %rsi
	callq	Perl_sv_setpvn
	movq	PL_oldname(%rip), %rsi
	movq	40(%rsp), %rdx
	movq	$2, %rcx
	movq	%r13, %rdi
	callq	Perl_sv_catpvn_flags
	callq	__errno_location
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movq	(%r13), %rax
	movq	PL_argvoutgv(%rip), %rdi
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	cmpq	$0, PL_inplace(%rip)
	movq	%r12, %rcx
	je	.LBB3_75
# BB#74:                                # %if.end222
                                        #   in Loop: Header=BB3_20 Depth=1
	movabsq	$1, %rcx
.LBB3_75:                               # %if.end222
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$65, %r8
	movq	$438, %r9               # imm = 0x1B6
	movq	$0, (%rsp)
	callq	Perl_do_openn
	testq	$255, %rax
	jne	.LBB3_83
# BB#76:                                # %if.then234
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB3_80
# BB#77:                                # %lor.lhs.false238
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB3_80
# BB#78:                                # %lor.lhs.false242
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB3_81
# BB#79:                                # %land.lhs.true246
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$64, %rax
	je	.LBB3_81
.LBB3_80:                               # %if.then254
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	PL_oldname(%rip), %rbx
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rcx
	movq	$23, %rdi
	xorq	%rax, %rax
	movabsq	$.L.str25, %rsi
	movq	%rbx, %rdx
	callq	Perl_warner
.LBB3_81:                               # %if.end257
                                        #   in Loop: Header=BB3_20 Depth=1
	testq	%r14, %r14
	je	.LBB3_82
.LBB3_42:                               # %if.end143
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	%r14, %rax
	jmp	.LBB3_43
.LBB3_82:                               #   in Loop: Header=BB3_20 Depth=1
	movq	PL_argvgv(%rip), %rax
	.align	16, 0x90
.LBB3_43:                               # %if.end143
                                        #   in Loop: Header=BB3_20 Depth=1
	testq	%rax, %rax
	je	.LBB3_93
# BB#44:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_20 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB3_93
# BB#45:                                # %land.lhs.true12.i
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	(%rax), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_93
# BB#46:                                # %cond.end.i
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB3_93
# BB#47:                                # %if.end46.i
                                        #   in Loop: Header=BB3_20 Depth=1
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	io_close
	movq	(%rbx), %rax
	movq	$32, %rcx
	movb	%cl, 162(%rax)
	.align	16, 0x90
.LBB3_93:                               # %while.cond.backedge
                                        #   in Loop: Header=BB3_20 Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rdi
	callq	Perl_av_len
	testq	%rax, %rax
	jns	.LBB3_20
.LBB3_94:                               # %while.end
	movq	32(%rsp), %r15          # 8-byte Reload
	testq	%r15, %r15
	je	.LBB3_97
# BB#95:                                # %land.lhs.true318
	movq	(%r15), %rax
	movzbl	163(%rax), %ecx
	testq	$1, %rcx
	je	.LBB3_97
# BB#96:                                # %if.then324
	orq	$2, %rcx
	movb	%cl, 163(%rax)
.LBB3_97:                               # %if.end329
	cmpq	$0, PL_inplace(%rip)
	movabsq	$0, %rbx
	je	.LBB3_111
# BB#98:                                # %if.then331
	movq	PL_argvoutgv(%rip), %rax
	testq	%rax, %rax
	jne	.LBB3_100
# BB#99:
	movq	PL_argvgv(%rip), %rax
.LBB3_100:                              # %if.then331
	testq	%rax, %rax
	je	.LBB3_105
# BB#101:                               # %lor.lhs.false.i482
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB3_105
# BB#102:                               # %land.lhs.true12.i487
	movq	(%rax), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB3_105
# BB#103:                               # %cond.end.i490
	movq	16(%rax), %r14
	testq	%r14, %r14
	je	.LBB3_105
# BB#104:                               # %if.end46.i496
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	io_close
	movq	(%r14), %rax
	movq	$32, %rcx
	movb	%cl, 162(%rax)
.LBB3_105:                              # %Perl_do_close.exit499
	testq	%r15, %r15
	je	.LBB3_110
# BB#106:                               # %land.lhs.true334
	movq	(%r15), %rax
	movzbl	163(%rax), %eax
	testq	$1, %rax
	je	.LBB3_110
# BB#107:                               # %land.lhs.true334
	movq	PL_argvout_stack(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB3_110
# BB#108:                               # %land.lhs.true342
	movq	(%rdi), %rax
	cmpq	$0, 8(%rax)
	js	.LBB3_110
# BB#109:                               # %if.then346
	callq	Perl_av_pop
	movq	%rax, %r14
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	setdefout
	movq	%r14, %rdi
	callq	Perl_sv_free
	jmp	.LBB3_111
.LBB3_110:                              # %if.end349
	movabsq	$.L.str20, %rdi
	movq	$1, %rsi
	movq	$15, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rcx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %rdi
	callq	setdefout
	jmp	.LBB3_111
.LBB3_33:                               # %if.then113
	movabsq	$.L.str20, %rdi
	movq	$1, %rsi
	movq	$15, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	setdefout
	jmp	.LBB3_84
.LBB3_83:                               # %if.end259
	movq	PL_argvoutgv(%rip), %rdi
	xorq	%rax, %rax
	callq	setdefout
	movq	PL_argvoutgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_fileno
	movl	%eax, PL_lastfd(%rip)
	movabsq	$PL_statbuf, %rsi
	movq	%rax, %rdi
	callq	fstat
	movq	PL_oldname(%rip), %rdi
	movl	PL_filemode(%rip), %esi
	callq	chmod
.LBB3_84:                               # %if.end276
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rbx
.LBB3_111:                              # %return
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp41:
	.size	Perl_nextargv, .Ltmp41-Perl_nextargv
	.cfi_endproc

	.globl	Perl_do_close
	.align	16, 0x90
	.type	Perl_do_close,@function
Perl_do_close:                          # @Perl_do_close
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp45:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp47:
	.cfi_def_cfa_offset 32
.Ltmp48:
	.cfi_offset %rbx, -24
.Ltmp49:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	testq	%rdi, %rdi
	jne	.LBB4_2
# BB#1:
	movq	PL_argvgv(%rip), %rdi
.LBB4_2:                                # %entry
	testq	%rdi, %rdi
	je	.LBB4_4
# BB#3:                                 # %lor.lhs.false
	movzbl	16(%rdi), %eax
	cmpq	$13, %rax
	jne	.LBB4_4
# BB#6:                                 # %land.lhs.true12
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB4_8
# BB#7:                                 # %cond.end
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB4_8
# BB#16:                                # %if.end46
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	io_close
	movq	(%rbx), %rcx
	movq	%rax, %rbx
	testq	$255, %r14
	je	.LBB4_18
# BB#17:                                # %if.then51
	movq	$0, 88(%rcx)
	movq	$0, 80(%rcx)
	movq	96(%rcx), %rax
	movq	%rax, 104(%rcx)
.LBB4_18:                               # %if.end56
	movq	$32, %rax
	movb	%al, 162(%rcx)
	jmp	.LBB4_19
.LBB4_4:                                # %if.then3
	xorq	%rbx, %rbx
	testq	$255, %r14
	jne	.LBB4_5
	jmp	.LBB4_19
.LBB4_8:                                # %if.then17
	xorq	%rbx, %rbx
	testq	$255, %r14
	je	.LBB4_19
# BB#9:                                 # %if.then19
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB4_13
# BB#10:                                # %land.lhs.true21
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB4_13
# BB#11:                                # %land.lhs.true24
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB4_15
# BB#12:                                # %lor.lhs.false27
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$64, %rax
	jne	.LBB4_15
.LBB4_13:                               # %lor.lhs.false32
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB4_5
# BB#14:                                # %land.lhs.true36
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB4_5
.LBB4_15:                               # %if.then40
	movq	PL_op(%rip), %rax
	movzbl	32(%rax), %edx
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	report_evil_fh
.LBB4_5:                                # %if.then5
	callq	__errno_location
	movq	$9, %rcx
	movl	%ecx, (%rax)
.LBB4_19:                               # %return
	movq	$56, %rcx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp50:
	.size	Perl_do_close, .Ltmp50-Perl_do_close
	.cfi_endproc

	.globl	Perl_io_close
	.align	16, 0x90
	.type	Perl_io_close,@function
Perl_io_close:                          # @Perl_io_close
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp55:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 32
.Ltmp58:
	.cfi_offset %rbx, -32
.Ltmp59:
	.cfi_offset %r14, -24
.Ltmp60:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	movq	%rsi, %rbx
	testq	%rdi, %rdi
	je	.LBB5_22
# BB#1:                                 # %if.then
	movzbl	162(%rax), %ecx
	movq	$1, %r15
	cmpq	$45, %rcx
	je	.LBB5_21
# BB#2:                                 # %if.then
	cmpq	$124, %rcx
	jne	.LBB5_10
# BB#3:                                 # %if.then4
	callq	Perl_my_pclose
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	$255, %rbx
	je	.LBB5_8
# BB#4:                                 # %if.then9
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	%rcx, %rdx
	movq	$-1, %rcx
	je	.LBB5_6
# BB#5:                                 # %if.then9
	andq	$65535, %rax            # imm = 0xFFFF
	movq	%rax, %rcx
.LBB5_6:                                # %if.then9
	testq	%rcx, %rcx
	movq	%rcx, PL_statusvalue(%rip)
	je	.LBB5_20
# BB#7:
	movabsq	$0, %r15
	jmp	.LBB5_21
.LBB5_22:                               # %if.else76
	xorq	%r15, %r15
	testq	$255, %rbx
	je	.LBB5_24
# BB#23:                                # %if.then78
	callq	__errno_location
	movq	$9, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB5_24
.LBB5_10:                               # %if.else28
	movq	64(%rax), %rax
	testq	%rax, %rax
	je	.LBB5_17
# BB#11:                                # %if.else28
	cmpq	%rdi, %rax
	je	.LBB5_17
# BB#12:                                # %if.then37
	movq	%rax, %rdi
	callq	Perl_PerlIO_error
	movq	(%r14), %rcx
	movq	64(%rcx), %rdi
	movq	%rax, %rbx
	callq	Perl_PerlIO_close
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	je	.LBB5_16
# BB#13:                                # %land.rhs
	testq	$255, %rbx
	jne	.LBB5_14
# BB#15:                                # %land.rhs
	movabsq	$1, %r15
	jmp	.LBB5_16
.LBB5_17:                               # %if.else52
	callq	Perl_PerlIO_error
	movq	(%r14), %rcx
	movq	56(%rcx), %rdi
	movq	%rax, %rbx
	callq	Perl_PerlIO_close
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	je	.LBB5_21
# BB#18:                                # %land.rhs63
	testq	$255, %rbx
	je	.LBB5_20
# BB#19:
	movabsq	$0, %r15
	jmp	.LBB5_21
.LBB5_8:                                # %if.else
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB5_9
.LBB5_20:                               # %land.rhs63
	movabsq	$1, %r15
	jmp	.LBB5_21
.LBB5_9:
	movabsq	$0, %r15
	jmp	.LBB5_21
.LBB5_14:
	movabsq	$0, %r15
.LBB5_16:                               # %land.end
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_close
.LBB5_21:                               # %if.end71
	movq	(%r14), %rax
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
.LBB5_24:                               # %if.end81
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp61:
	.size	Perl_io_close, .Ltmp61-Perl_io_close
	.cfi_endproc

	.globl	Perl_do_eof
	.align	16, 0x90
	.type	Perl_do_eof,@function
Perl_do_eof:                            # @Perl_do_eof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp69:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp71:
	.cfi_def_cfa_offset 48
.Ltmp72:
	.cfi_offset %rbx, -40
.Ltmp73:
	.cfi_offset %r12, -32
.Ltmp74:
	.cfi_offset %r14, -24
.Ltmp75:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	testq	%r12, %r12
	je	.LBB6_25
# BB#1:                                 # %land.lhs.true
	movzbl	16(%r12), %eax
	cmpq	$13, %rax
	jne	.LBB6_25
# BB#2:                                 # %land.lhs.true1
	movq	(%r12), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB6_25
# BB#3:                                 # %cond.end
	movq	16(%rax), %r14
	testq	%r14, %r14
	je	.LBB6_25
# BB#4:                                 # %if.else
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB6_8
# BB#5:                                 # %land.lhs.true7
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB6_8
# BB#6:                                 # %land.lhs.true10
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB6_10
# BB#7:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$4, %rax
	jne	.LBB6_10
.LBB6_8:                                # %lor.lhs.false17
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB6_12
# BB#9:                                 # %land.lhs.true21
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB6_12
.LBB6_10:                               # %land.lhs.true25
	movq	(%r14), %rax
	movzbl	162(%rax), %eax
	cmpq	$62, %rax
	jne	.LBB6_12
# BB#11:                                # %if.then30
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	report_evil_fh
.LBB6_12:                               # %while.cond.preheader
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_13:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB6_25
# BB#14:                                # %while.body
                                        #   in Loop: Header=BB6_13 Depth=1
	callq	PerlIO_has_cntptr
	testq	%r15, %rax
	je	.LBB6_16
# BB#15:                                # %if.then38
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_get_cnt
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jg	.LBB6_26
.LBB6_16:                               # %if.end46
                                        #   in Loop: Header=BB6_13 Depth=1
	callq	__errno_location
	movq	(%r14), %rcx
	movl	(%rax), %ebx
	movq	56(%rcx), %rdi
	callq	PerlIO_getc
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	%r15, %rcx
	jne	.LBB6_17
# BB#18:                                # %if.end58
                                        #   in Loop: Header=BB6_13 Depth=1
	callq	__errno_location
	movl	%ebx, (%rax)
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	callq	PerlIO_has_cntptr
	testq	%r15, %rax
	je	.LBB6_22
# BB#19:                                # %land.lhs.true64
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	callq	PerlIO_canset_cnt
	testq	%r15, %rax
	je	.LBB6_22
# BB#20:                                # %if.then69
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_get_cnt
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$-2, %rax
	jg	.LBB6_22
# BB#21:                                # %if.then75
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	(%r14), %rax
	movq	56(%rax), %rdi
	movq	%r15, %rsi
	callq	Perl_PerlIO_set_cnt
	.align	16, 0x90
.LBB6_22:                               # %if.end79
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	PL_op(%rip), %rax
	movsbq	36(%rax), %rax
	testq	%rax, %rax
	jns	.LBB6_25
# BB#23:                                # %if.end79
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	PL_argvgv(%rip), %rax
	cmpq	%r12, %rax
	jne	.LBB6_25
# BB#24:                                # %lor.lhs.false86
                                        #   in Loop: Header=BB6_13 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	nextargv
	testq	%r15, %rax
	jne	.LBB6_13
.LBB6_25:
	movq	$1, %rax
.LBB6_26:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB6_17:                               # %if.then53
	movq	(%r14), %rcx
	movq	56(%rcx), %rdi
	movq	%rax, %rsi
	callq	PerlIO_ungetc
	callq	__errno_location
	movl	%ebx, (%rax)
	xorq	%rax, %rax
	jmp	.LBB6_26
.Ltmp76:
	.size	Perl_do_eof, .Ltmp76-Perl_do_eof
	.cfi_endproc

	.globl	Perl_do_tell
	.align	16, 0x90
	.type	Perl_do_tell,@function
Perl_do_tell:                           # @Perl_do_tell
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp78:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	je	.LBB7_5
# BB#1:                                 # %land.lhs.true2
	movzbl	16(%rdi), %eax
	cmpq	$13, %rax
	jne	.LBB7_5
# BB#2:                                 # %land.lhs.true3
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB7_5
# BB#3:                                 # %cond.end
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB7_5
# BB#4:                                 # %land.lhs.true8
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	movq	%rcx, %rsi
	je	.LBB7_5
# BB#14:                                # %if.then
	movq	%rax, %rdi
	popq	%rax
	jmp	Perl_PerlIO_tell  # TAILCALL
.LBB7_5:                                # %if.end
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB7_10
# BB#6:                                 # %land.lhs.true12
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB7_10
# BB#7:                                 # %land.lhs.true15
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB7_12
# BB#8:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$64, %rax
	jne	.LBB7_12
# BB#9:                                 # %lor.lhs.false22
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$16, %rax
	jne	.LBB7_12
.LBB7_10:                               # %lor.lhs.false30
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB7_13
# BB#11:                                # %land.lhs.true34
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB7_13
.LBB7_12:                               # %if.then38
	movq	PL_op(%rip), %rax
	movzbl	32(%rax), %edx
	xorq	%rax, %rax
	callq	report_evil_fh
.LBB7_13:                               # %return
	callq	__errno_location
	movq	%rax, %rcx
	movq	$9, %rdx
	movq	$-1, %rax
	movl	%edx, (%rcx)
	popq	%rdx
	retq
.Ltmp79:
	.size	Perl_do_tell, .Ltmp79-Perl_do_tell
	.cfi_endproc

	.globl	Perl_do_seek
	.align	16, 0x90
	.type	Perl_do_seek,@function
Perl_do_seek:                           # @Perl_do_seek
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp81:
	.cfi_def_cfa_offset 16
	movq	%rsi, %rax
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	je	.LBB8_6
# BB#1:                                 # %land.lhs.true2
	movzbl	16(%rdi), %ecx
	cmpq	$13, %rcx
	jne	.LBB8_6
# BB#2:                                 # %land.lhs.true3
	movq	(%rdi), %rcx
	movq	56(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB8_6
# BB#3:                                 # %cond.end
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB8_6
# BB#4:                                 # %land.lhs.true8
	movq	(%rcx), %rsi
	movq	56(%rsi), %r8
	testq	%r8, %r8
	movq	%rcx, %rsi
	je	.LBB8_6
# BB#5:                                 # %if.then
	movq	%r8, %rdi
	movq	%rax, %rsi
	callq	Perl_PerlIO_seek
	movq	%rax, %rcx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rcx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	popq	%rdx
	retq
.LBB8_6:                                # %if.end
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB8_11
# BB#7:                                 # %land.lhs.true15
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB8_11
# BB#8:                                 # %land.lhs.true19
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB8_13
# BB#9:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$64, %rax
	jne	.LBB8_13
# BB#10:                                # %lor.lhs.false28
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$16, %rax
	jne	.LBB8_13
.LBB8_11:                               # %lor.lhs.false36
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB8_14
# BB#12:                                # %land.lhs.true40
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB8_14
.LBB8_13:                               # %if.then44
	movq	PL_op(%rip), %rax
	movzbl	32(%rax), %edx
	xorq	%rax, %rax
	callq	report_evil_fh
.LBB8_14:                               # %if.end47
	callq	__errno_location
	movq	$9, %rcx
	movl	%ecx, (%rax)
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp82:
	.size	Perl_do_seek, .Ltmp82-Perl_do_seek
	.cfi_endproc

	.globl	Perl_do_sysseek
	.align	16, 0x90
	.type	Perl_do_sysseek,@function
Perl_do_sysseek:                        # @Perl_do_sysseek
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp88:
	.cfi_def_cfa_offset 32
.Ltmp89:
	.cfi_offset %rbx, -24
.Ltmp90:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	je	.LBB9_5
# BB#1:                                 # %land.lhs.true2
	movzbl	16(%rdi), %eax
	cmpq	$13, %rax
	jne	.LBB9_5
# BB#2:                                 # %land.lhs.true3
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_5
# BB#3:                                 # %cond.end
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB9_5
# BB#4:                                 # %land.lhs.true8
	movq	(%rcx), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	movq	%rcx, %rsi
	je	.LBB9_5
# BB#14:                                # %if.then
	movq	%rax, %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	lseek  # TAILCALL
.LBB9_5:                                # %if.end
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB9_10
# BB#6:                                 # %land.lhs.true13
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB9_10
# BB#7:                                 # %land.lhs.true16
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB9_12
# BB#8:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$64, %rax
	jne	.LBB9_12
# BB#9:                                 # %lor.lhs.false23
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$16, %rax
	jne	.LBB9_12
.LBB9_10:                               # %lor.lhs.false31
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB9_13
# BB#11:                                # %land.lhs.true35
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB9_13
.LBB9_12:                               # %if.then39
	movq	PL_op(%rip), %rax
	movzbl	32(%rax), %edx
	xorq	%rax, %rax
	callq	report_evil_fh
.LBB9_13:                               # %return
	callq	__errno_location
	movq	%rax, %rcx
	movq	$9, %rdx
	movq	$-1, %rax
	movl	%edx, (%rcx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp91:
	.size	Perl_do_sysseek, .Ltmp91-Perl_do_sysseek
	.cfi_endproc

	.globl	Perl_mode_from_discipline
	.align	16, 0x90
	.type	Perl_mode_from_discipline,@function
Perl_mode_from_discipline:              # @Perl_mode_from_discipline
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp94:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp95:
	.cfi_def_cfa_offset 32
.Ltmp96:
	.cfi_offset %rbx, -16
	testq	%rdi, %rdi
	je	.LBB10_28
# BB#1:                                 # %if.then
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB10_3
# BB#2:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB10_5
.LBB10_3:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
.LBB10_4:                               # %while.cond
	movq	%rax, %rbx
	jmp	.LBB10_5
	.align	16, 0x90
.LBB10_27:                              # %if.then119
                                        #   in Loop: Header=BB10_5 Depth=1
	decq	8(%rsp)
	incq	%rbx
.LBB10_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$58, %rcx
	ja	.LBB10_9
# BB#6:                                 # %while.cond
                                        #   in Loop: Header=BB10_5 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4294981120, %rdx       # imm = 0x100003600
	testq	%rdx, %rax
	jne	.LBB10_27
# BB#7:                                 # %while.cond
                                        #   in Loop: Header=BB10_5 Depth=1
	testq	%rcx, %rcx
	je	.LBB10_28
# BB#8:                                 # %while.cond
                                        #   in Loop: Header=BB10_5 Depth=1
	cmpq	$58, %rcx
	jne	.LBB10_9
# BB#13:                                # %if.then5
                                        #   in Loop: Header=BB10_5 Depth=1
	movsbq	1(%rbx), %rcx
	leaq	1(%rbx), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$114, %rax
	jne	.LBB10_14
# BB#22:                                # %sw.bb
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	2(%rbx), %eax
	cmpq	$97, %rax
	jne	.LBB10_16
# BB#23:                                # %land.lhs.true
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	3(%rbx), %eax
	cmpq	$119, %rax
	jne	.LBB10_10
# BB#24:                                # %land.lhs.true15
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	4(%rbx), %ecx
	cmpq	$58, %rcx
	ja	.LBB10_10
# BB#25:                                # %land.lhs.true15
                                        #   in Loop: Header=BB10_5 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$288230380446692865, %rcx # imm = 0x400000100003601
	testq	%rcx, %rax
	je	.LBB10_10
# BB#26:                                # %if.then47
                                        #   in Loop: Header=BB10_5 Depth=1
	addq	$-4, 8(%rsp)
	addq	$4, %rbx
	jmp	.LBB10_5
.LBB10_14:                              # %if.then5
                                        #   in Loop: Header=BB10_5 Depth=1
	cmpq	$99, %rax
	jne	.LBB10_10
# BB#15:                                # %if.then5.sw.bb48_crit_edge
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	2(%rbx), %eax
.LBB10_16:                              # %sw.bb48
                                        #   in Loop: Header=BB10_5 Depth=1
	andq	$255, %rax
	cmpq	$114, %rax
	jne	.LBB10_10
# BB#17:                                # %land.lhs.true53
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	3(%rbx), %eax
	cmpq	$108, %rax
	jne	.LBB10_10
# BB#18:                                # %land.lhs.true58
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	4(%rbx), %eax
	cmpq	$102, %rax
	jne	.LBB10_10
# BB#19:                                # %land.lhs.true63
                                        #   in Loop: Header=BB10_5 Depth=1
	movzbl	5(%rbx), %ecx
	cmpq	$58, %rcx
	ja	.LBB10_10
# BB#20:                                # %land.lhs.true63
                                        #   in Loop: Header=BB10_5 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$288230380446692865, %rcx # imm = 0x400000100003601
	testq	%rcx, %rax
	je	.LBB10_10
# BB#21:                                # %if.then96
                                        #   in Loop: Header=BB10_5 Depth=1
	addq	$-5, 8(%rsp)
	addq	$5, %rbx
	jmp	.LBB10_5
.LBB10_9:                               # %while.cond.fail_discipline_crit_edge
	leaq	1(%rbx), %rdi
.LBB10_10:                              # %fail_discipline
	movq	$58, %rsi
	callq	strchr
	movq	8(%rsp), %rcx
	testq	%rax, %rax
	jne	.LBB10_12
# BB#11:                                # %if.then124
	leaq	(%rbx,%rcx), %rax
.LBB10_12:                              # %if.end126
	subq	%rax, %rbx
	addq	%rcx, %rbx
	movq	%rbx, 8(%rsp)
	jmp	.LBB10_4
.LBB10_28:                              # %if.end130
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp97:
	.size	Perl_mode_from_discipline, .Ltmp97-Perl_mode_from_discipline
	.cfi_endproc

	.globl	Perl_do_binmode
	.align	16, 0x90
	.type	Perl_do_binmode,@function
Perl_do_binmode:                        # @Perl_do_binmode
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str27, %rcx
	jmp	PerlIO_binmode  # TAILCALL
.Ltmp98:
	.size	Perl_do_binmode, .Ltmp98-Perl_do_binmode
	.cfi_endproc

	.globl	Perl_do_print
	.align	16, 0x90
	.type	Perl_do_print,@function
Perl_do_print:                          # @Perl_do_print
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp104:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp105:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp106:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp107:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp108:
	.cfi_def_cfa_offset 48
.Ltmp109:
	.cfi_offset %rbx, -40
.Ltmp110:
	.cfi_offset %r12, -32
.Ltmp111:
	.cfi_offset %r14, -24
.Ltmp112:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	movq	$1, %r15
	je	.LBB12_51
# BB#1:                                 # %if.end
	cmpq	$0, PL_ofmt(%rip)
	je	.LBB12_15
# BB#2:                                 # %if.then2
	movq	16(%rbx), %rax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB12_4
# BB#3:                                 # %if.then4
	movq	%rbx, %rdi
	callq	Perl_mg_get
	movq	16(%rbx), %rax
.LBB12_4:                               # %if.end5
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB12_8
# BB#5:                                 # %land.lhs.true
	movq	(%rbx), %rcx
	movq	24(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB12_8
# BB#6:                                 # %if.then9
	movq	PL_ofmt(%rip), %rbx
	callq	int64_to_float64
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	PerlIO_printf
	movq	%r14, %rdi
	callq	Perl_PerlIO_error
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_50
# BB#7:
	movabsq	$0, %r15
	jmp	.LBB12_51
.LBB12_8:                               # %if.end16
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB12_10
# BB#9:                                 # %land.lhs.true20
	movq	(%rbx), %rax
	movq	32(%rax), %r12
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB12_13
.LBB12_10:                              # %lor.lhs.false
	movq	%rbx, %rdi
	callq	Perl_looks_like_number
	testq	%rax, %rax
	je	.LBB12_15
# BB#11:                                # %land.lhs.true26
	movq	%rbx, %rdi
	callq	Perl_sv_2nv
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB12_15
# BB#12:                                # %land.lhs.true26.if.then30_crit_edge
	movq	(%rbx), %rax
	movq	32(%rax), %r12
.LBB12_13:                              # %if.then30
	movq	PL_ofmt(%rip), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	PerlIO_printf
	movq	%r14, %rdi
	callq	Perl_PerlIO_error
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_50
# BB#14:
	movabsq	$0, %r15
	jmp	.LBB12_51
.LBB12_15:                              # %if.end40
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	jne	.LBB12_16
# BB#24:                                # %sw.bb70
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB12_32
# BB#25:                                # %if.then74
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB12_27
# BB#26:                                # %if.then78
	movq	%rbx, %rdi
	callq	Perl_mg_get
	movq	16(%rbx), %rax
.LBB12_27:                              # %if.end80
	movq	(%rbx), %rcx
	movq	24(%rcx), %rdx
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rax
	je	.LBB12_29
# BB#28:                                # %if.then84
	movabsq	$.L.str28, %rsi
	jmp	.LBB12_30
.LBB12_16:                              # %if.end40
	testq	%rcx, %rcx
	jne	.LBB12_32
# BB#17:                                # %sw.bb
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB12_21
# BB#18:                                # %land.lhs.true45
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB12_21
# BB#19:                                # %land.lhs.true49
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB12_23
# BB#20:                                # %lor.lhs.false53
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	10(%rax), %eax
	testq	$4, %rax
	jne	.LBB12_23
.LBB12_21:                              # %lor.lhs.false59
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB12_51
# BB#22:                                # %land.lhs.true63
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB12_51
.LBB12_23:                              # %if.then67
	xorq	%rax, %rax
	callq	report_uninit
	jmp	.LBB12_51
.LBB12_32:                              # %sw.default
	movq	%r14, %rdi
	callq	PerlIO_isutf8
	movq	16(%rbx), %rcx
	andq	$536870912, %rcx        # imm = 0x20000000
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB12_35
# BB#33:                                # %if.then99
	testq	%rcx, %rcx
	jne	.LBB12_43
# BB#34:                                # %if.then103
	movq	%rbx, %rdi
	callq	Perl_sv_mortalcopy
	movq	%rax, %rbx
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_upgrade_flags
	jmp	.LBB12_43
.LBB12_35:                              # %if.else107
	testq	%rcx, %rcx
	je	.LBB12_43
# BB#36:                                # %land.lhs.true111
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	jne	.LBB12_43
# BB#37:                                # %if.then115
	movq	%rbx, %rdi
	callq	Perl_sv_mortalcopy
	movq	%rax, %rbx
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_downgrade
	testq	$255, %rax
	jne	.LBB12_43
# BB#38:                                # %land.lhs.true119
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB12_42
# BB#39:                                # %lor.lhs.false123
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB12_42
# BB#40:                                # %lor.lhs.false127
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB12_43
# BB#41:                                # %land.lhs.true131
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	11(%rax), %eax
	testq	$1, %rax
	je	.LBB12_43
.LBB12_42:                              # %if.then139
	movabsq	$.L.str30, %rsi
	movq	$44, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB12_43:                              # %if.end142
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB12_45
# BB#44:                                # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rdx
	movq	%rdx, (%rsp)
	movq	(%rax), %rsi
	jmp	.LBB12_46
.LBB12_45:                              # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%rsp), %rdx
	movq	%rax, %rsi
.LBB12_46:                              # %cond.end
	testq	%rdx, %rdx
	je	.LBB12_48
# BB#47:                                # %land.lhs.true152
	movq	%r14, %rdi
	callq	Perl_PerlIO_write
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB12_51
.LBB12_48:                              # %if.end157
	movq	%r14, %rdi
	callq	Perl_PerlIO_error
	testq	%r12, %rax
	je	.LBB12_50
# BB#49:
	movabsq	$0, %r15
	jmp	.LBB12_51
.LBB12_29:                              # %if.else
	movabsq	$.L.str29, %rsi
.LBB12_30:                              # %if.end90
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	movq	%r14, %rdi
	callq	Perl_PerlIO_error
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_31
.LBB12_50:                              # %if.end157
	movabsq	$1, %r15
.LBB12_51:                              # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB12_31:
	movabsq	$0, %r15
	jmp	.LBB12_51
.Ltmp113:
	.size	Perl_do_print, .Ltmp113-Perl_do_print
	.cfi_endproc

	.globl	Perl_my_stat
	.align	16, 0x90
	.type	Perl_my_stat,@function
Perl_my_stat:                           # @Perl_my_stat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp116:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp117:
	.cfi_def_cfa_offset 32
.Ltmp118:
	.cfi_offset %rbx, -16
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %ecx
	movq	PL_stack_sp(%rip), %rdi
	testq	$16, %rcx
	je	.LBB13_20
# BB#1:                                 # %if.then
	movq	PL_stack_max(%rip), %rcx
	subq	%rdi, %rcx
	cmpq	$7, %rcx
	jg	.LBB13_3
# BB#2:                                 # %if.then2
	movq	$1, %rdx
	movq	%rdi, %rsi
	callq	Perl_stack_grow
	movq	PL_op(%rip), %rax
.LBB13_3:                               # %if.end
	movq	40(%rax), %rdi
	jmp	.LBB13_4
.LBB13_20:                              # %if.else61
	movq	(%rdi), %rax
	addq	$-8, %rdi
	movq	%rdi, PL_stack_sp(%rip)
	movq	16(%rax), %rcx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$13, %rdx
	jne	.LBB13_22
# BB#21:
	movq	%rax, %rdi
	jmp	.LBB13_4
.LBB13_22:                              # %if.else67
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB13_24
# BB#23:                                # %land.lhs.true71
	movq	(%rax), %rdx
	movq	(%rdx), %rdi
	movzbl	16(%rdi), %edx
	cmpq	$13, %rdx
	jne	.LBB13_24
.LBB13_4:                               # %do_fstat
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	je	.LBB13_10
# BB#5:                                 # %land.lhs.true
	movzbl	16(%rdi), %eax
	cmpq	$13, %rax
	jne	.LBB13_10
# BB#6:                                 # %land.lhs.true7
	movq	(%rdi), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB13_10
# BB#7:                                 # %cond.end
	movq	16(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB13_10
# BB#8:                                 # %land.lhs.true12
	movq	(%rbx), %rax
	cmpq	$0, 56(%rax)
	je	.LBB13_9
# BB#38:                                # %if.then15
	movq	PL_statname(%rip), %rax
	movq	%rdi, PL_statgv(%rip)
	movabsq	$.L.str3, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movq	$235, PL_laststype(%rip)
	movq	(%rbx), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_fileno
	movabsq	$PL_statcache, %rsi
	movq	%rax, %rdi
	callq	fstat
	movl	%eax, PL_laststatval(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	jmp	.LBB13_37
.LBB13_24:                              # %if.end81
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB13_26
# BB#25:                                # %cond.true86
	movq	(%rax), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 8(%rsp)
	movq	(%rax), %rsi
	jmp	.LBB13_27
.LBB13_26:                              # %cond.false90
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%rax, %rdi
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rdx
	movq	%rax, %rsi
.LBB13_27:                              # %cond.end92
	movq	PL_statname(%rip), %rdi
	movq	$0, PL_statgv(%rip)
	callq	Perl_sv_setpvn
	movq	PL_statname(%rip), %rax
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	$235, PL_laststype(%rip)
	movabsq	$PL_statcache, %rsi
	movq	%rbx, %rdi
	callq	stat
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, PL_laststatval(%rip)
	jns	.LBB13_36
# BB#28:                                # %land.lhs.true99
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB13_32
# BB#29:                                # %land.lhs.true103
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB13_32
# BB#30:                                # %land.lhs.true107
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB13_34
# BB#31:                                # %lor.lhs.false111
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$4, %rax
	jne	.LBB13_34
.LBB13_32:                              # %lor.lhs.false119
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB13_36
# BB#33:                                # %land.lhs.true123
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB13_36
.LBB13_34:                              # %land.lhs.true127
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	strchr
	testq	%rax, %rax
	je	.LBB13_36
# BB#35:                                # %if.then130
	movabsq	$PL_warn_nl, %rsi
	movabsq	$.L.str31, %rdx
	movq	$9, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
	jmp	.LBB13_36
.LBB13_9:
	movq	%rbx, %rsi
.LBB13_10:                              # %if.else
	movq	PL_defgv(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB13_36
# BB#11:                                # %if.end25
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB13_16
# BB#12:                                # %land.lhs.true28
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB13_16
# BB#13:                                # %land.lhs.true32
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB13_18
# BB#14:                                # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$64, %rax
	jne	.LBB13_18
# BB#15:                                # %lor.lhs.false41
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$16, %rax
	jne	.LBB13_18
.LBB13_16:                              # %lor.lhs.false49
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB13_19
# BB#17:                                # %land.lhs.true53
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB13_19
.LBB13_18:                              # %if.then57
	movq	PL_op(%rip), %rax
	movzbl	32(%rax), %edx
	xorq	%rax, %rax
	callq	report_evil_fh
.LBB13_19:                              # %if.end60
	movq	PL_statname(%rip), %rdi
	movq	$0, PL_statgv(%rip)
	movabsq	$.L.str3, %rsi
	callq	Perl_sv_setpv
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, PL_laststatval(%rip)
	movq	$-1, %rax
	jmp	.LBB13_37
.LBB13_36:                              # %if.end131
	movslq	PL_laststatval(%rip), %rax
.LBB13_37:                              # %return
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp119:
	.size	Perl_my_stat, .Ltmp119-Perl_my_stat
	.cfi_endproc

	.globl	Perl_my_lstat
	.align	16, 0x90
	.type	Perl_my_lstat,@function
Perl_my_lstat:                          # @Perl_my_lstat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp123:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp125:
	.cfi_def_cfa_offset 32
.Ltmp126:
	.cfi_offset %rbx, -24
.Ltmp127:
	.cfi_offset %r14, -16
	movq	PL_op(%rip), %rcx
	movzbl	36(%rcx), %edx
	movq	PL_stack_sp(%rip), %rax
	testq	$16, %rdx
	je	.LBB14_16
# BB#1:                                 # %if.then
	movq	PL_stack_max(%rip), %rdx
	subq	%rax, %rdx
	cmpq	$7, %rdx
	jg	.LBB14_3
# BB#2:                                 # %if.then2
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	Perl_stack_grow
	movq	PL_op(%rip), %rcx
.LBB14_3:                               # %if.end
	movq	40(%rcx), %rcx
	movq	PL_defgv(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB14_4
# BB#6:                                 # %if.end11
	movq	PL_curcop(%rip), %rcx
	cmpq	$0, 88(%rcx)
	je	.LBB14_10
# BB#7:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rcx
	movq	88(%rcx), %rcx
	cmpq	$48, %rcx
	je	.LBB14_10
# BB#8:                                 # %land.lhs.true17
	movq	PL_curcop(%rip), %rcx
	movq	88(%rcx), %rcx
	cmpq	$24, %rcx
	je	.LBB14_12
# BB#9:                                 # %lor.lhs.false
	movq	PL_curcop(%rip), %rcx
	movq	88(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	(%rcx), %rcx
	movzbl	1(%rcx), %ecx
	testq	$4, %rcx
	jne	.LBB14_12
.LBB14_10:                              # %lor.lhs.false25
	movq	PL_curcop(%rip), %rcx
	cmpq	$0, 88(%rcx)
	jne	.LBB14_16
# BB#11:                                # %land.lhs.true29
	movzbl	PL_dowarn(%rip), %ecx
	testq	$1, %rcx
	je	.LBB14_16
.LBB14_12:                              # %if.then33
	movq	PL_op(%rip), %rax
	movq	40(%rax), %rax
	jmp	.LBB14_13
.LBB14_16:                              # %if.end44
	movq	$234, PL_laststype(%rip)
	movq	$0, PL_statgv(%rip)
	movq	(%rax), %r14
	addq	$-8, %rax
	movq	%rax, PL_stack_sp(%rip)
	movzbl	18(%r14), %eax
	testq	$8, %rax
	je	.LBB14_25
# BB#17:                                # %land.lhs.true47
	movq	(%r14), %rax
	movq	(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$13, %rax
	jne	.LBB14_25
# BB#18:                                # %land.lhs.true53
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB14_22
# BB#19:                                # %land.lhs.true57
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB14_22
# BB#20:                                # %land.lhs.true61
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB14_24
# BB#21:                                # %lor.lhs.false65
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_24
.LBB14_22:                              # %lor.lhs.false73
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB14_25
# BB#23:                                # %land.lhs.true77
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB14_25
.LBB14_24:                              # %if.then81
	movq	(%r14), %rax
	movq	(%rax), %rax
.LBB14_13:                              # %if.then33
	movq	(%rax), %rcx
	movq	56(%rcx), %rcx
	movq	48(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB14_15
# BB#14:                                # %if.then33
	movq	%rcx, %rax
.LBB14_15:                              # %if.then33
	movq	(%rax), %rax
	movq	64(%rax), %rdx
	movabsq	$.L.str33, %rsi
	movq	$5, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, PL_laststatval(%rip)
	movq	$-1, %rax
	jmp	.LBB14_44
.LBB14_25:                              # %if.end101
	movzbl	18(%r14), %eax
	movq	PL_statname(%rip), %rbx
	testq	$4, %rax
	je	.LBB14_27
# BB#26:                                # %cond.true106
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rsi
	jmp	.LBB14_28
.LBB14_4:                               # %if.then5
	movq	PL_laststype(%rip), %rax
	cmpq	$234, %rax
	je	.LBB14_43
# BB#5:                                 # %if.then8
	movabsq	$.L.str32, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB14_43
.LBB14_27:                              # %cond.false110
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rsi
.LBB14_28:                              # %cond.end112
	movq	%rbx, %rdi
	callq	Perl_sv_setpv
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB14_30
# BB#29:                                # %cond.true118
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB14_31
.LBB14_30:                              # %cond.false123
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB14_31:                              # %cond.end125
	movabsq	$PL_statcache, %rsi
	callq	stat
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, PL_laststatval(%rip)
	jns	.LBB14_43
# BB#32:                                # %land.lhs.true130
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB14_36
# BB#33:                                # %land.lhs.true134
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB14_36
# BB#34:                                # %land.lhs.true138
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB14_38
# BB#35:                                # %lor.lhs.false142
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	2(%rax), %eax
	testq	$4, %rax
	jne	.LBB14_38
.LBB14_36:                              # %lor.lhs.false150
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB14_43
# BB#37:                                # %land.lhs.true154
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB14_43
.LBB14_38:                              # %land.lhs.true158
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB14_40
# BB#39:                                # %cond.true163
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rdi
	jmp	.LBB14_41
.LBB14_40:                              # %cond.false168
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB14_41:                              # %cond.end170
	movq	$10, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB14_43
# BB#42:                                # %if.then174
	movabsq	$PL_warn_nl, %rsi
	movabsq	$.L.str34, %rdx
	movq	$9, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB14_43:                              # %if.end175
	movslq	PL_laststatval(%rip), %rax
.LBB14_44:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp128:
	.size	Perl_my_lstat, .Ltmp128-Perl_my_lstat
	.cfi_endproc

	.globl	Perl_do_aexec
	.align	16, 0x90
	.type	Perl_do_aexec,@function
Perl_do_aexec:                          # @Perl_do_aexec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp130:
	.cfi_def_cfa_offset 16
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%rax, %rax
	callq	do_aexec5
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	popq	%rdx
	retq
.Ltmp131:
	.size	Perl_do_aexec, .Ltmp131-Perl_do_aexec
	.cfi_endproc

	.globl	Perl_do_aexec5
	.align	16, 0x90
	.type	Perl_do_aexec5,@function
Perl_do_aexec5:                         # @Perl_do_aexec5
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp138:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp139:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp140:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp141:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp142:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp143:
	.cfi_def_cfa_offset 96
.Ltmp144:
	.cfi_offset %rbx, -48
.Ltmp145:
	.cfi_offset %r12, -40
.Ltmp146:
	.cfi_offset %r13, -32
.Ltmp147:
	.cfi_offset %r14, -24
.Ltmp148:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %rbx
	cmpq	%rbx, %r12
	jbe	.LBB16_33
# BB#1:                                 # %if.then
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	$8, %rdi
	subq	%rbx, %rdi
	addq	%r12, %rdi
	andq	$-8, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r14
	movq	%r14, PL_Argv(%rip)
	addq	$8, %rbx
	cmpq	%r12, %rbx
	jbe	.LBB16_3
# BB#2:
	movq	%r15, 16(%rsp)          # 8-byte Spill
	jmp	.LBB16_9
.LBB16_3:
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movabsq	$.L.str3, %r13
	leaq	40(%rsp), %r15
	.align	16, 0x90
.LBB16_4:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%r13, %rax
	je	.LBB16_8
# BB#5:                                 # %if.then2
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	%rdi, PL_Sv(%rip)
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB16_6
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	$2, %rdx
	movq	%r15, %rsi
	callq	Perl_sv_2pv_flags
	jmp	.LBB16_8
	.align	16, 0x90
.LBB16_6:                               # %cond.true
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %rax
.LBB16_8:                               # %while.cond.backedge
                                        #   in Loop: Header=BB16_4 Depth=1
	movq	%rax, (%r14)
	addq	$8, %r14
	addq	$8, %rbx
	cmpq	%r12, %rbx
	jbe	.LBB16_4
.LBB16_9:                               # %while.end
	movq	$0, (%r14)
	movq	24(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB16_20
# BB#10:                                # %if.then9
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB16_11
# BB#12:                                # %if.end22
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %r12
	jmp	.LBB16_13
.LBB16_20:                              # %land.lhs.true
	movq	PL_Argv(%rip), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	xorq	%r12, %r12
	cmpq	$47, %rax
	je	.LBB16_21
# BB#15:                                # %if.then31
	movzbl	PL_tainting(%rip), %eax
	xorq	%r12, %r12
	testq	%rax, %rax
	jne	.LBB16_16
	jmp	.LBB16_17
.LBB16_11:                              # %if.end22.thread
	leaq	40(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r12
.LBB16_13:                              # %land.lhs.true27
	movzbl	(%r12), %eax
	cmpq	$47, %rax
	je	.LBB16_17
# BB#14:                                # %land.lhs.true27
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB16_17
.LBB16_16:                              # %if.then33
	callq	Perl_taint_env
.LBB16_17:                              # %if.end35
	testq	%rbx, %rbx
	je	.LBB16_21
# BB#18:                                # %land.lhs.true37
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB16_21
# BB#19:                                # %if.then40
	movq	PL_Argv(%rip), %rsi
	movq	%r12, %rdi
	jmp	.LBB16_22
.LBB16_21:                              # %if.else42
	movq	PL_Argv(%rip), %rsi
	movq	(%rsi), %rdi
.LBB16_22:                              # %if.end45
	callq	execvp
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB16_26
# BB#23:                                # %land.lhs.true48
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB16_26
# BB#24:                                # %land.lhs.true52
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB16_28
# BB#25:                                # %lor.lhs.false56
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$64, %rax
	jne	.LBB16_28
.LBB16_26:                              # %lor.lhs.false64
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB16_31
# BB#27:                                # %land.lhs.true68
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB16_31
.LBB16_28:                              # %if.then72
	testq	%rbx, %rbx
	jne	.LBB16_30
# BB#29:                                # %cond.false75
	movq	PL_Argv(%rip), %rax
	movq	(%rax), %r12
.LBB16_30:                              # %cond.end77
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rcx
	movabsq	$.L.str35, %rsi
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	Perl_warner
.LBB16_31:                              # %if.end81
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB16_33
# BB#32:                                # %if.then83
	callq	__errno_location
	movl	(%rax), %eax
	movl	%eax, 36(%rsp)
	leaq	36(%rsp), %rsi
	movq	$4, %rdx
	movq	%rbx, %rdi
	callq	write
	movq	%rbx, %rdi
	callq	close
.LBB16_33:                              # %if.end88
	xorq	%rax, %rax
	callq	do_execfree
	xorq	%rax, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp149:
	.size	Perl_do_aexec5, .Ltmp149-Perl_do_aexec5
	.cfi_endproc

	.globl	Perl_do_execfree
	.align	16, 0x90
	.type	Perl_do_execfree,@function
Perl_do_execfree:                       # @Perl_do_execfree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp151:
	.cfi_def_cfa_offset 16
	movq	PL_Argv(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB17_2
# BB#1:                                 # %if.then
	callq	Perl_safesysfree
	movq	$0, PL_Argv(%rip)
.LBB17_2:                               # %if.end
	movq	PL_Cmd(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB17_4
# BB#3:                                 # %if.then2
	callq	Perl_safesysfree
	movq	$0, PL_Cmd(%rip)
.LBB17_4:                               # %if.end3
	popq	%rax
	retq
.Ltmp152:
	.size	Perl_do_execfree, .Ltmp152-Perl_do_execfree
	.cfi_endproc

	.globl	Perl_do_exec
	.align	16, 0x90
	.type	Perl_do_exec,@function
Perl_do_exec:                           # @Perl_do_exec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp154:
	.cfi_def_cfa_offset 16
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	do_exec3
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	popq	%rdx
	retq
.Ltmp155:
	.size	Perl_do_exec, .Ltmp155-Perl_do_exec
	.cfi_endproc

	.globl	Perl_do_exec3
	.align	16, 0x90
	.type	Perl_do_exec3,@function
Perl_do_exec3:                          # @Perl_do_exec3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp162:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp163:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp164:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp165:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp167:
	.cfi_def_cfa_offset 96
.Ltmp168:
	.cfi_offset %rbx, -48
.Ltmp169:
	.cfi_offset %r12, -40
.Ltmp170:
	.cfi_offset %r13, -32
.Ltmp171:
	.cfi_offset %r14, -24
.Ltmp172:
	.cfi_offset %r15, -16
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r13
	addq	$4, %r13
	movabsq	$4294981120, %r15       # imm = 0x100003600
	jmp	.LBB19_1
	.align	16, 0x90
.LBB19_3:                               # %while.body
                                        #   in Loop: Header=BB19_1 Depth=1
	incq	%r13
.LBB19_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-4(%r13), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_4
# BB#2:                                 # %while.cond
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	jne	.LBB19_3
.LBB19_4:                               # %while.end
	movq	PL_cshname(%rip), %rsi
	movq	PL_cshlen(%rip), %rbx
	leaq	-4(%r13), %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdx
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_18
# BB#5:                                 # %land.lhs.true
	leaq	-4(%r13,%rbx), %rdi
	movabsq	$.L.str36, %rsi
	movq	$3, %rdx
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_18
# BB#6:                                 # %if.then
	xorq	%rax, %rax
	movb	%al, 40(%rsp)
	movq	$25389, %rax            # imm = 0x632D
	movw	%ax, 38(%rsp)
	movzbl	-1(%r13,%rbx), %eax
	cmpq	$102, %rax
	jne	.LBB19_7
# BB#8:                                 # %if.then26
	leaq	38(%rsp), %rdi
	callq	strlen
	movq	$102, %rcx
	movw	%cx, 38(%rsp,%rax)
	movzbl	(%r13,%rbx), %eax
	leaq	(%r13,%rbx), %rbx
	jmp	.LBB19_9
.LBB19_7:
	leaq	-1(%r13,%rbx), %rbx
.LBB19_9:                               # %if.end
	andq	$255, %rax
	cmpq	$32, %rax
	jne	.LBB19_11
# BB#10:
	incq	%rbx
.LBB19_11:                              # %if.end
	movzbl	(%rbx), %eax
	cmpq	$39, %rax
	jne	.LBB19_18
# BB#12:
	leaq	1(%rbx), %rcx
	.align	16, 0x90
.LBB19_13:                              # %while.cond41
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rbx), %eax
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB19_13
# BB#14:                                # %while.end45
	movzbl	-1(%rbx), %eax
	cmpq	$10, %rax
	jne	.LBB19_16
# BB#15:                                # %if.then49
	xorq	%rax, %rax
	movb	%al, -1(%rbx)
	movzbl	-2(%rbx), %eax
	leaq	-1(%rbx), %rbx
.LBB19_16:                              # %if.end51
	andq	$255, %rax
	cmpq	$39, %rax
	jne	.LBB19_18
# BB#17:                                # %if.then56
	xorq	%rax, %rax
	movb	%al, -1(%rbx)
	movq	PL_cshname(%rip), %rdi
	movabsq	$.L.str39, %rsi
	leaq	38(%rsp), %rdx
	xorq	%r8, %r8
	xorq	%rax, %rax
	callq	execl
	movq	$39, %rax
	movb	%al, -1(%rbx)
	jmp	.LBB19_82
.LBB19_18:                              # %if.end62
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %r14d
	cmpq	$46, %r14
	jne	.LBB19_21
# BB#19:                                # %land.lhs.true66
	movzbl	-3(%r13), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_21
# BB#20:                                # %land.lhs.true66
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	jne	.LBB19_71
.LBB19_21:                              # %if.end92
	movabsq	$.L.str40, %rsi
	movq	$4, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_24
# BB#22:                                # %land.lhs.true95
	movzbl	(%r13), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_24
# BB#23:                                # %land.lhs.true95
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	jne	.LBB19_71
.LBB19_24:                              # %for.cond.preheader
	xorq	%rsi, %rsi
	testq	%r14, %r14
	movq	24(%rsp), %r12          # 8-byte Reload
	je	.LBB19_33
# BB#25:
	movq	$1, %rax
	movq	%r14, %rcx
	.align	16, 0x90
.LBB19_26:                              # %land.rhs124
                                        # =>This Inner Loop Header: Depth=1
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB19_30
# BB#27:                                # %land.rhs124
                                        #   in Loop: Header=BB19_26 Depth=1
	leaq	-97(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jb	.LBB19_30
# BB#28:                                # %lor.lhs.false140
                                        #   in Loop: Header=BB19_26 Depth=1
	leaq	-48(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$10, %rdx
	jb	.LBB19_30
# BB#29:                                # %lor.lhs.false140
                                        #   in Loop: Header=BB19_26 Depth=1
	andq	$255, %rcx
	cmpq	$95, %rcx
	jne	.LBB19_32
.LBB19_30:                              # %for.inc
                                        #   in Loop: Header=BB19_26 Depth=1
	movzbl	-4(%r13,%rax), %ecx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB19_26
# BB#31:
	movq	%r14, %rsi
	movq	24(%rsp), %r12          # 8-byte Reload
	jmp	.LBB19_33
.LBB19_32:                              # %for.end
	cmpq	$61, %rcx
	movq	%r14, %rsi
	movq	24(%rsp), %r12          # 8-byte Reload
	je	.LBB19_71
	jmp	.LBB19_33
	.align	16, 0x90
.LBB19_84:                              # %for.inc311
                                        #   in Loop: Header=BB19_33 Depth=1
	movzbl	1(%r12), %esi
	incq	%r12
.LBB19_33:                              # %for.cond160
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rbx
	andq	$255, %rbx
	cmpq	$32, %rbx
	je	.LBB19_84
# BB#34:                                # %for.cond160
                                        #   in Loop: Header=BB19_33 Depth=1
	testq	$255, %rsi
	je	.LBB19_56
# BB#35:                                # %land.lhs.true166
                                        #   in Loop: Header=BB19_33 Depth=1
	leaq	-65(%rsi), %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jb	.LBB19_84
# BB#36:                                # %land.lhs.true166
                                        #   in Loop: Header=BB19_33 Depth=1
	leaq	-97(%rsi), %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jb	.LBB19_84
# BB#37:                                # %land.lhs.true182
                                        #   in Loop: Header=BB19_33 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movabsq	$.L.str41, %rdi
	movq	$21, %rdx
	callq	memchr
	testq	%rax, %rax
	je	.LBB19_84
# BB#38:                                # %if.then186
	cmpq	$62, %rbx
	jne	.LBB19_39
# BB#42:                                # %land.lhs.true198
	movzbl	1(%r12), %eax
	cmpq	$38, %rax
	jne	.LBB19_71
# BB#43:                                # %land.lhs.true203
	movzbl	2(%r12), %eax
	cmpq	$49, %rax
	jne	.LBB19_71
# BB#44:                                # %land.lhs.true203
	addq	$-3, %r13
	cmpq	%r13, %r12
	jbe	.LBB19_71
# BB#45:                                # %land.lhs.true212
	movzbl	-1(%r12), %eax
	cmpq	$50, %rax
	jne	.LBB19_71
# BB#46:                                # %land.lhs.true217
	movzbl	-2(%r12), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_71
# BB#47:                                # %land.lhs.true217
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r15, %rax
	je	.LBB19_71
# BB#48:                                # %land.lhs.true242
	movzbl	3(%r12), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_71
# BB#49:                                # %land.lhs.true242
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4294981121, %rdx       # imm = 0x100003601
	testq	%rdx, %rax
	je	.LBB19_71
# BB#50:
	movq	$4, %rax
	jmp	.LBB19_51
.LBB19_39:                              # %if.then186
	cmpq	$10, %rbx
	jne	.LBB19_71
# BB#40:                                # %land.lhs.true190
	movzbl	1(%r12), %eax
	testq	%rax, %rax
	jne	.LBB19_71
# BB#41:                                # %if.then193
	xorq	%rax, %rax
	movb	%al, (%r12)
.LBB19_56:                              # %for.end313
	movq	24(%rsp), %rbx          # 8-byte Reload
	subq	%rbx, %r12
	movq	$63, %rcx
	movq	%r12, %rax
	shrq	%cl, %rax
	addq	%r12, %rax
	movabsq	$4611686018427387902, %rcx # imm = 0x3FFFFFFFFFFFFFFE
	andq	%rax, %rcx
	leaq	16(,%rcx,4), %rdi
	callq	Perl_safesysmalloc
	movq	%rax, PL_Argv(%rip)
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	Perl_savepvn
	movq	%rax, PL_Cmd(%rip)
	movzbl	(%rax), %ecx
	movq	PL_Argv(%rip), %rdx
	testq	%rcx, %rcx
	je	.LBB19_68
# BB#57:
	xorq	%rsi, %rsi
	jmp	.LBB19_58
	.align	16, 0x90
.LBB19_85:                              # %while.body347
                                        #   in Loop: Header=BB19_58 Depth=1
	movzbl	1(%rax), %ecx
	incq	%rax
.LBB19_58:                              # %while.cond322
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_62 Depth 2
	andq	$255, %rcx
	cmpq	$32, %rcx
	ja	.LBB19_61
# BB#59:                                # %while.cond322
                                        #   in Loop: Header=BB19_58 Depth=1
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%r15, %rdi
	jne	.LBB19_85
# BB#60:                                # %while.cond322
                                        #   in Loop: Header=BB19_58 Depth=1
	testq	%rcx, %rcx
	je	.LBB19_62
.LBB19_61:                              # %if.then351
                                        #   in Loop: Header=BB19_58 Depth=1
	movq	%rax, (%rdx)
	addq	$8, %rdx
	jmp	.LBB19_62
	.align	16, 0x90
.LBB19_65:                              # %while.body379.critedge
                                        #   in Loop: Header=BB19_62 Depth=2
	incq	%rax
.LBB19_62:                              # %while.cond354
                                        #   Parent Loop BB19_58 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$32, %rcx
	ja	.LBB19_65
# BB#63:                                # %while.cond354
                                        #   in Loop: Header=BB19_62 Depth=2
	movq	$1, %rdi
	shlq	%cl, %rdi
	testq	%r15, %rdi
	jne	.LBB19_66
# BB#64:                                # %while.cond354
                                        #   in Loop: Header=BB19_62 Depth=2
	testq	%rcx, %rcx
	jne	.LBB19_65
	jmp	.LBB19_67
.LBB19_66:                              # %if.then383
                                        #   in Loop: Header=BB19_58 Depth=1
	movb	%sil, (%rax)
	incq	%rax
.LBB19_67:                              # %for.cond319.backedge
                                        #   in Loop: Header=BB19_58 Depth=1
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB19_58
.LBB19_68:                              # %for.end386
	movq	$0, (%rdx)
	movq	PL_Argv(%rip), %rsi
	movq	(%rsi), %rdi
	testq	%rdi, %rdi
	je	.LBB19_81
# BB#69:                                # %if.then389
	callq	execvp
	callq	__errno_location
	movl	(%rax), %eax
	cmpq	$8, %rax
	jne	.LBB19_72
# BB#70:                                # %if.then395
	xorq	%rax, %rax
	callq	do_execfree
.LBB19_71:                              # %doshell
	movq	PL_sh_path(%rip), %rdi
	movabsq	$.L.str42, %rsi
	movabsq	$.L.str37, %rdx
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	execl
	jmp	.LBB19_82
.LBB19_72:                              # %if.end397
	callq	__errno_location
	movl	(%rax), %eax
	movl	%eax, 32(%rsp)
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB19_76
# BB#73:                                # %land.lhs.true401
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB19_76
# BB#74:                                # %land.lhs.true405
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB19_78
# BB#75:                                # %lor.lhs.false409
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	1(%rax), %eax
	testq	$64, %rax
	jne	.LBB19_78
.LBB19_76:                              # %lor.lhs.false414
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB19_79
# BB#77:                                # %land.lhs.true418
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB19_79
.LBB19_78:                              # %if.then422
	movq	PL_Argv(%rip), %rax
	movq	(%rax), %rbx
	callq	__errno_location
	movl	(%rax), %edi
	callq	strerror
	movq	%rax, %rcx
	movabsq	$.L.str35, %rsi
	movq	$7, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	Perl_warner
.LBB19_79:                              # %if.end426
	movq	16(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB19_81
# BB#80:                                # %if.then428
	leaq	32(%rsp), %rsi
	movq	$4, %rdx
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	callq	write
	movq	%rbx, %rdi
	callq	close
.LBB19_81:                              # %if.end432
	xorq	%rax, %rax
	callq	do_execfree
.LBB19_82:                              # %return
	xorq	%rax, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_83:                              # %while.body297
                                        #   in Loop: Header=BB19_51 Depth=1
	movzbl	(%r12,%rax), %ecx
	incq	%rax
.LBB19_51:                              # %while.cond272
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	cmpq	$32, %rcx
	ja	.LBB19_71
# BB#52:                                # %while.cond272
                                        #   in Loop: Header=BB19_51 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r15, %rdx
	jne	.LBB19_83
# BB#53:                                # %while.cond272
	testq	%rcx, %rcx
	jne	.LBB19_71
# BB#54:                                # %land.lhs.true301
	movq	$1, %rdi
	movq	$2, %rsi
	callq	dup2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB19_71
# BB#55:                                # %if.then305
	xorq	%rax, %rax
	movb	%al, -2(%r12)
	jmp	.LBB19_56
.Ltmp173:
	.size	Perl_do_exec3, .Ltmp173-Perl_do_exec3
	.cfi_endproc

	.globl	Perl_apply
	.align	16, 0x90
	.type	Perl_apply,@function
Perl_apply:                             # @Perl_apply
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp180:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp181:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp182:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp183:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp185:
	.cfi_def_cfa_offset 64
.Ltmp186:
	.cfi_offset %rbx, -48
.Ltmp187:
	.cfi_offset %r12, -40
.Ltmp188:
	.cfi_offset %r13, -32
.Ltmp189:
	.cfi_offset %r14, -24
.Ltmp190:
	.cfi_offset %r15, -16
	movzbl	PL_tainting(%rip), %eax
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB20_7
# BB#1:                                 # %while.cond.preheader
	leaq	8(%r12), %rbx
	jmp	.LBB20_2
	.align	16, 0x90
.LBB20_4:                               # %while.cond.backedge
                                        #   in Loop: Header=BB20_2 Depth=1
	addq	$8, %rbx
.LBB20_2:                               # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %rbx
	ja	.LBB20_7
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	(%rbx), %rdi
	movzbl	17(%rdi), %eax
	testq	$224, %rax
	je	.LBB20_4
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB20_2 Depth=1
	callq	Perl_sv_tainted
	testq	$255, %rax
	je	.LBB20_4
# BB#6:                                 # %if.then3
	movq	$1, %rax
	movb	%al, PL_tainted(%rip)
.LBB20_7:                               # %if.end4
	cmpq	$266, %r13              # imm = 0x10A
	jne	.LBB20_8
# BB#30:                                # %sw.bb54
	movzbl	PL_tainted(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_33
# BB#31:                                # %sw.bb54
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB20_33
# BB#32:                                # %if.then58
	movabsq	$.L.str44, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB20_33:                              # %if.end60
	movq	%r14, %r15
	subq	%r12, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	addq	$8, %r12
	cmpq	%r14, %r12
	ja	.LBB20_50
# BB#34:
	movabsq	$0, %rbx
	.align	16, 0x90
.LBB20_35:                              # %while.body69
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdi
	movq	%rdi, PL_Sv(%rip)
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB20_36
# BB#37:                                # %cond.false79
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	$2, %rdx
	leaq	8(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r13
	jmp	.LBB20_38
	.align	16, 0x90
.LBB20_36:                              # %cond.true74
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r13
.LBB20_38:                              # %cond.end81
                                        #   in Loop: Header=BB20_35 Depth=1
	movzbl	PL_tainted(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_41
# BB#39:                                # %cond.end81
                                        #   in Loop: Header=BB20_35 Depth=1
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB20_41
# BB#40:                                # %if.then86
                                        #   in Loop: Header=BB20_35 Depth=1
	xorq	%rdi, %rdi
	movabsq	$.L.str44, %rsi
	callq	Perl_taint_proper
.LBB20_41:                              # %if.end88
                                        #   in Loop: Header=BB20_35 Depth=1
	movl	PL_euid(%rip), %eax
	testq	%rax, %rax
	jne	.LBB20_43
# BB#42:                                # %if.end88
                                        #   in Loop: Header=BB20_35 Depth=1
	movzbl	PL_unsafe(%rip), %eax
	testq	$255, %rax
	jne	.LBB20_43
# BB#46:                                # %if.else
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	%r13, %rdi
	movabsq	$PL_statbuf, %rsi
	callq	stat
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB20_48
# BB#47:                                # %lor.lhs.false101
                                        #   in Loop: Header=BB20_35 Depth=1
	movl	PL_statbuf+24(%rip), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB20_51
.LBB20_48:                              # %if.then105
                                        #   in Loop: Header=BB20_35 Depth=1
	decq	%r15
	jmp	.LBB20_49
	.align	16, 0x90
.LBB20_43:                              # %if.then92
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	%r13, %rdi
	callq	unlink
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rax
	je	.LBB20_45
# BB#44:                                # %if.then92
                                        #   in Loop: Header=BB20_35 Depth=1
	movabsq	$1, %rax
.LBB20_45:                              # %if.then92
                                        #   in Loop: Header=BB20_35 Depth=1
	subq	%rax, %r15
	jmp	.LBB20_49
.LBB20_51:                              # %if.else107
                                        #   in Loop: Header=BB20_35 Depth=1
	movq	%r13, %rdi
	callq	unlink
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rax
	je	.LBB20_53
# BB#52:                                # %if.else107
                                        #   in Loop: Header=BB20_35 Depth=1
	movabsq	$1, %rax
.LBB20_53:                              # %if.else107
                                        #   in Loop: Header=BB20_35 Depth=1
	subq	%rax, %r15
	.align	16, 0x90
.LBB20_49:                              # %while.cond65.backedge
                                        #   in Loop: Header=BB20_35 Depth=1
	addq	$8, %r12
	cmpq	%r14, %r12
	jbe	.LBB20_35
	jmp	.LBB20_50
.LBB20_8:                               # %if.end4
	xorq	%r15, %r15
	cmpq	$267, %r13              # imm = 0x10B
	jne	.LBB20_50
# BB#9:                                 # %sw.bb
	movzbl	PL_tainted(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_12
# BB#10:                                # %sw.bb
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB20_12
# BB#11:                                # %if.then8
	movabsq	$.L.str43, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB20_12:                              # %if.end10
	leaq	8(%r12), %r13
	cmpq	%r14, %r13
	ja	.LBB20_50
# BB#13:                                # %if.then14
	movq	(%r13), %rdi
	movq	%rdi, PL_Sv(%rip)
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB20_15
# BB#14:                                # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rbx
	jmp	.LBB20_16
.LBB20_15:                              # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %rbx
.LBB20_16:                              # %cond.end
	movzbl	PL_tainted(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_19
# BB#17:                                # %cond.end
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB20_19
# BB#18:                                # %if.then22
	movabsq	$.L.str43, %rsi
	xorq	%rdi, %rdi
	callq	Perl_taint_proper
.LBB20_19:                              # %if.end24
	movq	%r14, %r15
	subq	%r13, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	addq	$16, %r12
	jmp	.LBB20_20
.LBB20_29:                              # %if.end46
                                        #   in Loop: Header=BB20_20 Depth=1
	subq	%rax, %r15
	addq	$8, %r12
.LBB20_20:                              # %if.end24
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %r12
	ja	.LBB20_50
# BB#21:                                # %while.body29
                                        #   in Loop: Header=BB20_20 Depth=1
	movq	(%r12), %rdi
	movq	%rdi, PL_Sv(%rip)
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB20_22
# BB#23:                                # %cond.false37
                                        #   in Loop: Header=BB20_20 Depth=1
	movq	$2, %rdx
	leaq	8(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r13
	jmp	.LBB20_24
.LBB20_22:                              # %cond.true34
                                        #   in Loop: Header=BB20_20 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r13
.LBB20_24:                              # %cond.end39
                                        #   in Loop: Header=BB20_20 Depth=1
	movzbl	PL_tainted(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_27
# BB#25:                                # %cond.end39
                                        #   in Loop: Header=BB20_20 Depth=1
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB20_27
# BB#26:                                # %if.then44
                                        #   in Loop: Header=BB20_20 Depth=1
	xorq	%rdi, %rdi
	movabsq	$.L.str43, %rsi
	callq	Perl_taint_proper
.LBB20_27:                              # %if.end46
                                        #   in Loop: Header=BB20_20 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	chmod
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rax
	je	.LBB20_29
# BB#28:                                # %if.end46
                                        #   in Loop: Header=BB20_20 Depth=1
	movabsq	$1, %rax
	jmp	.LBB20_29
.LBB20_50:                              # %sw.epilog
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp191:
	.size	Perl_apply, .Ltmp191-Perl_apply
	.cfi_endproc

	.globl	Perl_cando
	.align	16, 0x90
	.type	Perl_cando,@function
Perl_cando:                             # @Perl_cando
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp196:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp197:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp198:
	.cfi_def_cfa_offset 32
.Ltmp199:
	.cfi_offset %rbx, -32
.Ltmp200:
	.cfi_offset %r14, -24
.Ltmp201:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rsi
	jne	.LBB21_1
# BB#2:                                 # %entry
	movabsq	$PL_uid, %rax
	jmp	.LBB21_3
.LBB21_1:
	movabsq	$PL_euid, %rax
.LBB21_3:                               # %entry
	movl	(%rax), %eax
	testq	%rax, %rax
	je	.LBB21_4
# BB#10:                                # %if.end9
	movl	28(%r14), %ecx
	cmpq	%rax, %rcx
	jne	.LBB21_12
# BB#11:                                # %if.then16
	movl	24(%r14), %eax
	andq	%rbx, %rax
	testq	%r15, %rax
	jmp	.LBB21_16
.LBB21_4:                               # %if.then
	andq	%r15, %rbx
	movq	$1, %rax
	cmpq	$64, %rbx
	jne	.LBB21_18
# BB#5:                                 # %if.then2
	movl	24(%r14), %esi
	testq	$73, %rsi
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	je	.LBB21_7
# BB#6:                                 # %if.then2
	movq	%rdx, %rax
.LBB21_7:                               # %if.then2
	andq	$61440, %rsi            # imm = 0xF000
	cmpq	$16384, %rsi            # imm = 0x4000
	jne	.LBB21_9
# BB#8:                                 # %if.then2
	movq	%rdx, %rcx
.LBB21_9:                               # %if.then2
	orq	%rcx, %rax
	jmp	.LBB21_18
.LBB21_12:                              # %if.else22
	movl	32(%r14), %edi
	xorq	%rax, %rax
	callq	ingroup
	movl	24(%r14), %edx
	testq	%r15, %rax
	je	.LBB21_14
# BB#13:                                # %if.then24
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rbx
	movq	$3, %rcx
	jmp	.LBB21_15
.LBB21_14:                              # %if.else30
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %rbx
	movq	$6, %rcx
.LBB21_15:                              # %if.else30
	shrq	%cl, %rbx
	testq	%rbx, %rdx
.LBB21_16:                              # %if.else30
	movq	$1, %rax
	jne	.LBB21_18
# BB#17:                                # %if.end38
	xorq	%rax, %rax
.LBB21_18:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp202:
	.size	Perl_cando, .Ltmp202-Perl_cando
	.cfi_endproc

	.globl	Perl_ingroup
	.align	16, 0x90
	.type	Perl_ingroup,@function
Perl_ingroup:                           # @Perl_ingroup
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	testq	%rax, %rsi
	jne	.LBB22_1
# BB#2:                                 # %entry
	movabsq	$PL_gid, %rax
	jmp	.LBB22_3
.LBB22_1:
	movabsq	$PL_egid, %rax
.LBB22_3:                               # %entry
	movl	(%rax), %eax
	cmpq	%rdi, %rax
	jne	.LBB22_4
# BB#5:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB22_4:
	movabsq	$0, %rax
	retq
.Ltmp203:
	.size	Perl_ingroup, .Ltmp203-Perl_ingroup
	.cfi_endproc

	.globl	Perl_start_glob
	.align	16, 0x90
	.type	Perl_start_glob,@function
Perl_start_glob:                        # @Perl_start_glob
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp208:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp209:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp210:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp211:
	.cfi_def_cfa_offset 64
.Ltmp212:
	.cfi_offset %rbx, -32
.Ltmp213:
	.cfi_offset %r14, -24
.Ltmp214:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	callq	Perl_push_scope
	movq	%rbx, %rdi
	callq	Perl_save_freesv
	movq	PL_cshname(%rip), %rsi
	movq	PL_cshlen(%rip), %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_setpvn
	movabsq	$.L.str45, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_catsv_flags
	movabsq	$.L.str46, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movq	(%rbx), %rax
	movq	PL_last_in_gv(%rip), %rdi
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	callq	Perl_do_openn
	movq	(%r14), %rax
	movq	56(%rax), %rbx
	callq	Perl_pop_scope
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp215:
	.size	Perl_start_glob, .Ltmp215-Perl_start_glob
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Warning: unable to close filehandle %s properly.\n"
	.size	.L.str, 50

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"panic: sysopen with multiple args"
	.size	.L.str1, 34

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"sysopen"
	.size	.L.str2, 8

	.type	.L.str3,@object         # @.str3
.L.str3:
	.zero	1
	.size	.L.str3, 1

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"open"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Unknown open() mode '%.*s'"
	.size	.L.str5, 27

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Missing command in piped open"
	.size	.L.str6, 30

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"piped open"
	.size	.L.str7, 11

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Can't open bidirectional pipe"
	.size	.L.str8, 30

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"More than one argument to '%c&' open"
	.size	.L.str11, 37

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"More than one argument to '>%c' open"
	.size	.L.str12, 37

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"More than one argument to '<%c' open"
	.size	.L.str13, 37

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Filehandle STD%s reopened as %s only for input"
	.size	.L.str14, 47

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"OUT"
	.size	.L.str15, 4

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"ERR"
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Filehandle STDIN reopened as %s only for output"
	.size	.L.str17, 48

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"ARGVOUT"
	.size	.L.str18, 8

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"inplace open"
	.size	.L.str19, 13

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"STDOUT"
	.size	.L.str20, 7

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Can't do inplace edit: %s is not a regular file"
	.size	.L.str21, 48

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Can't rename %s to %_: %s, skipping file"
	.size	.L.str22, 41

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Can't remove %s: %s, skipping file"
	.size	.L.str23, 35

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	">"
	.size	.L.str24, 2

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Can't do inplace edit on %s: %s"
	.size	.L.str25, 32

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Can't open %s: %s"
	.size	.L.str26, 18

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	":raw"
	.size	.L.str27, 5

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"%lu"
	.size	.L.str28, 4

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"%ld"
	.size	.L.str29, 4

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Wide character in print"
	.size	.L.str30, 24

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"stat"
	.size	.L.str31, 5

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"The stat preceding -l _ wasn't an lstat"
	.size	.L.str32, 40

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"Use of -l on filehandle %s"
	.size	.L.str33, 27

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"lstat"
	.size	.L.str34, 6

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Can't exec \"%s\": %s"
	.size	.L.str35, 20

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	" -c"
	.size	.L.str36, 4

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"-c"
	.size	.L.str37, 3

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"csh"
	.size	.L.str39, 4

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"exec"
	.size	.L.str40, 5

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"$&*(){}[]'\";\\|?<>~`\n"
	.size	.L.str41, 21

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"sh"
	.size	.L.str42, 3

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"chmod"
	.size	.L.str43, 6

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"unlink"
	.size	.L.str44, 7

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	" -cf 'set nonomatch; glob "
	.size	.L.str45, 27

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"' 2>/dev/null |"
	.size	.L.str46, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
