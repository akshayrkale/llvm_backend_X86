	.file	"Parser.c.bc"
	.text
	.align	16, 0x90
	.type	magic_free_pstate,@function
magic_free_pstate:                      # @magic_free_pstate
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB0_2
# BB#1:                                 # %cond.true.i
	movq	(%rdi), %rax
	movq	24(%rax), %rbx
	jmp	.LBB0_3
.LBB0_2:                                # %cond.false.i
	callq	Perl_sv_2iv
	movq	%rax, %rbx
.LBB0_3:                                # %cond.end.i
	movq	(%rbx), %rax
	cmpq	$369695076, %rax        # imm = 0x16091964
	je	.LBB0_5
# BB#4:                                 # %if.then.i
	movabsq	$.L.str106, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
.LBB0_5:                                # %get_pstate_iv.exit
	movq	8(%rbx), %rdi
	callq	Perl_sv_free
	movq	72(%rbx), %rdi
	callq	Perl_sv_free
	movq	112(%rbx), %rdi
	callq	Perl_sv_free
	movq	128(%rbx), %rdi
	callq	Perl_sv_free
	movq	152(%rbx), %rdi
	callq	Perl_sv_free
	movq	160(%rbx), %rdi
	callq	Perl_sv_free
	movq	168(%rbx), %rdi
	callq	Perl_sv_free
	movq	176(%rbx), %rdi
	callq	Perl_sv_free
	movq	184(%rbx), %rdi
	callq	Perl_sv_free
	movq	192(%rbx), %rdi
	callq	Perl_sv_free
	movq	200(%rbx), %rdi
	callq	Perl_sv_free
	movq	208(%rbx), %rdi
	callq	Perl_sv_free
	movq	216(%rbx), %rdi
	callq	Perl_sv_free
	movq	224(%rbx), %rdi
	callq	Perl_sv_free
	movq	232(%rbx), %rdi
	callq	Perl_sv_free
	movq	240(%rbx), %rdi
	callq	Perl_sv_free
	movq	248(%rbx), %rdi
	callq	Perl_sv_free
	movq	256(%rbx), %rdi
	callq	Perl_sv_free
	movq	264(%rbx), %rdi
	callq	Perl_sv_free
	movq	272(%rbx), %rdi
	callq	Perl_sv_free
	movq	280(%rbx), %rdi
	callq	Perl_sv_free
	movq	288(%rbx), %rdi
	callq	Perl_sv_free
	movq	296(%rbx), %rdi
	callq	Perl_sv_free
	movq	312(%rbx), %rdi
	callq	Perl_sv_free
	movq	320(%rbx), %rdi
	callq	Perl_sv_free
	movq	328(%rbx), %rdi
	callq	Perl_sv_free
	movq	336(%rbx), %rdi
	callq	Perl_sv_free
	movq	360(%rbx), %rdi
	callq	Perl_sv_free
	movq	$0, (%rbx)
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	magic_free_pstate, .Ltmp4-magic_free_pstate
	.cfi_endproc

	.globl	XS_HTML__Parser__alloc_pstate
	.align	16, 0x90
	.type	XS_HTML__Parser__alloc_pstate,@function
XS_HTML__Parser__alloc_pstate:          # @XS_HTML__Parser__alloc_pstate
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp9:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp10:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 32
.Ltmp12:
	.cfi_offset %rbx, -32
.Ltmp13:
	.cfi_offset %r14, -24
.Ltmp14:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB1_2:                                # %if.end
	movq	8(%rax,%r15,8), %rax
	movq	(%rax), %rax
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB1_4
# BB#3:                                 # %lor.lhs.false
	movzbl	16(%r14), %eax
	cmpq	$11, %rax
	je	.LBB1_5
.LBB1_4:                                # %if.then7
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB1_5:                                # %if.end8
	movq	$368, %rdi              # imm = 0x170
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	$368, %rdx              # imm = 0x170
	movq	%rbx, %rdi
	callq	memset
	movq	$369695076, (%rbx)      # imm = 0x16091964
	movabsq	$.L.str22, %rdi
	movq	$1, %rsi
	callq	Perl_get_hv
	movq	%rax, 352(%rbx)
	movq	$20, %rdi
	callq	Perl_newSV
	movq	%rax, 360(%rbx)
	movq	%rbx, %rdi
	callq	Perl_newSViv
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	$126, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	Perl_sv_magic
	movq	$126, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	movq	$vtbl_free_pstate, 8(%rax)
	orq	$8388608, 16(%rbx)      # imm = 0x800000
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movabsq	$.L.str23, %rsi
	movq	$17, %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%rax, %rcx
	callq	Perl_hv_store
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp15:
	.size	XS_HTML__Parser__alloc_pstate, .Ltmp15-XS_HTML__Parser__alloc_pstate
	.cfi_endproc

	.globl	XS_HTML__Parser_parse
	.align	16, 0x90
	.type	XS_HTML__Parser_parse,@function
XS_HTML__Parser_parse:                  # @XS_HTML__Parser_parse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp24:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp25:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp27:
	.cfi_def_cfa_offset 64
.Ltmp28:
	.cfi_offset %rbx, -48
.Ltmp29:
	.cfi_offset %r12, -40
.Ltmp30:
	.cfi_offset %r13, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	movq	%rbx, %r12
	subq	%rdx, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	cmpq	$2, %r12
	je	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str24, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB2_2:                                # %if.end
	movq	8(%rax,%r15,8), %r14
	movq	16(%rax,%r15,8), %r15
	movq	(%r14), %rdi
	movq	$3, %rcx
	shlq	%cl, %r12
	callq	get_pstate_hv
	movq	%rax, %r13
	movzbl	40(%r13), %eax
	testq	%rax, %rax
	je	.LBB2_4
# BB#3:                                 # %if.then9
	movabsq	$.L.str25, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB2_4:                                # %if.end10
	movq	$1, %rax
	movb	%al, 40(%r13)
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB2_33
# BB#5:                                 # %land.lhs.true
	movq	(%r15), %rax
	movq	(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$12, %rax
	jne	.LBB2_33
# BB#6:                                 # %do.body.preheader
	subq	%r12, %rbx
	leaq	41(%r13), %r12
	jmp	.LBB2_7
	.align	16, 0x90
.LBB2_32:                               # %land.rhs
                                        #   in Loop: Header=BB2_7 Depth=1
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_7
	jmp	.LBB2_34
	.align	16, 0x90
.LBB2_25:                               # %if.then98
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB2_26
# BB#27:                                # %cond.false108
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	$2, %rdx
	movq	%rbx, %rdi
	leaq	8(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rsi
	jmp	.LBB2_29
.LBB2_26:                               # %cond.true103
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rbx), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	jmp	.LBB2_29
	.align	16, 0x90
.LBB2_7:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rcx
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rax
	jne	.LBB2_9
# BB#8:                                 # %if.then19
                                        #   in Loop: Header=BB2_7 Depth=1
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB2_9:                                # %if.end20
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	PL_stack_base(%rip), %rcx
	subq	%rcx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%rbx, (%rax)
	movq	$4, %rsi
	movq	%r15, %rdi
	callq	Perl_call_sv
	movq	PL_stack_sp(%rip), %rcx
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB2_11
# BB#10:                                # %cond.true
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rcx), %rbx
	addq	$-8, %rcx
.LBB2_11:                               # %cond.end
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	PL_errgv(%rip), %rax
	movq	%rcx, PL_stack_sp(%rip)
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB2_23
# BB#12:                                # %cond.false31
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB2_13
# BB#17:                                # %cond.false55
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB2_18
# BB#19:                                # %cond.false69
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	$131072, %rax           # imm = 0x20000
	jne	.LBB2_20
# BB#21:                                # %cond.false83
                                        #   in Loop: Header=BB2_7 Depth=1
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB2_23
	jmp	.LBB2_22
	.align	16, 0x90
.LBB2_13:                               # %cond.true38
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB2_23
# BB#14:                                # %land.lhs.true44
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB2_22
# BB#15:                                # %lor.lhs.false
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	%rcx, %rcx
	je	.LBB2_23
# BB#16:                                # %land.lhs.true49
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB2_22
	jmp	.LBB2_23
.LBB2_18:                               # %cond.true62
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB2_22
	jmp	.LBB2_23
.LBB2_20:                               # %cond.true76
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB2_23
	.align	16, 0x90
.LBB2_22:                               # %if.then90
                                        #   in Loop: Header=BB2_7 Depth=1
	xorq	%rax, %rax
	movw	%ax, 40(%r13)
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB2_23:                               # %if.end92
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	%rbx, %rbx
	je	.LBB2_28
# BB#24:                                # %land.lhs.true94
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	16(%rbx), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	jne	.LBB2_25
.LBB2_28:                               # %if.else
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	$0, 8(%rsp)
	xorq	%rsi, %rsi
.LBB2_29:                               # %if.end112
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	%rsi, %rsi
	je	.LBB2_31
# BB#30:                                # %if.end112
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	%rbx, %rsi
.LBB2_31:                               # %if.end112
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	%r13, %rdi
	movq	%r14, %rdx
	callq	parse
	movq	PL_stack_sp(%rip), %rbx
	cmpq	$0, 8(%rsp)
	jne	.LBB2_32
	jmp	.LBB2_34
.LBB2_33:                               # %if.else121
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	parse
	movq	PL_stack_sp(%rip), %rbx
	leaq	41(%r13), %r12
.LBB2_34:                               # %if.end122
	xorq	%rax, %rax
	movb	%al, 40(%r13)
	movzbl	(%r12), %ecx
	testq	%rcx, %rcx
	je	.LBB2_36
# BB#35:                                # %if.then126
	movb	%al, (%r12)
	callq	Perl_sv_newmortal
	movq	%rax, 8(%rbx)
	jmp	.LBB2_37
.LBB2_36:                               # %if.else130
	movq	%r14, 8(%rbx)
.LBB2_37:                               # %if.end132
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	XS_HTML__Parser_parse, .Ltmp33-XS_HTML__Parser_parse
	.cfi_endproc

	.align	16, 0x90
	.type	get_pstate_hv,@function
get_pstate_hv:                          # @get_pstate_hv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 32
.Ltmp40:
	.cfi_offset %rbx, -24
.Ltmp41:
	.cfi_offset %r14, -16
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB3_2
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rbx), %eax
	cmpq	$11, %rax
	je	.LBB3_3
.LBB3_2:                                # %if.then
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_3:                                # %if.end
	movabsq	$.L.str23, %rsi
	movq	$17, %rdx
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB3_11
# BB#4:                                 # %if.then2
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB3_10
# BB#5:                                 # %if.then6
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB3_7
# BB#6:                                 # %cond.true.i
	movq	(%rdi), %rax
	movq	24(%rax), %r14
	jmp	.LBB3_8
.LBB3_10:                               # %if.else
	movabsq	$.L.str104, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_11:                               # %if.end11
	movabsq	$.L.str105, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB3_12
.LBB3_7:                                # %cond.false.i
	callq	Perl_sv_2iv
	movq	%rax, %r14
.LBB3_8:                                # %cond.end.i
	movq	(%r14), %rax
	cmpq	$369695076, %rax        # imm = 0x16091964
	je	.LBB3_12
# BB#9:                                 # %if.then.i
	movabsq	$.L.str106, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	Perl_croak
.LBB3_12:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp42:
	.size	get_pstate_hv, .Ltmp42-get_pstate_hv
	.cfi_endproc

	.align	16, 0x90
	.type	parse,@function
parse:                                  # @parse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp51:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp52:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp53:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp54:
	.cfi_def_cfa_offset 128
.Ltmp55:
	.cfi_offset %rbx, -48
.Ltmp56:
	.cfi_offset %r12, -40
.Ltmp57:
	.cfi_offset %r13, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
	.cfi_offset %r15, -16
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rdi, %r13
	testq	%rbx, %rbx
	je	.LBB4_1
# BB#32:                                # %if.end75
	movzbl	145(%r13), %eax
	testq	%rax, %rax
	je	.LBB4_34
# BB#33:                                # %if.then77
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_downgrade
.LBB4_34:                               # %if.end79
	movq	8(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB4_40
# BB#35:                                # %land.lhs.true82
	movzbl	18(%rdi), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB4_40
# BB#36:                                # %if.then87
	movq	$2, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_catsv_flags
	movq	8(%r13), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB4_38
# BB#37:                                # %cond.true94
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	(%rax), %r12
	jmp	.LBB4_39
.LBB4_40:                               # %if.else
	movq	16(%rbx), %r15
	testq	$262144, %r15           # imm = 0x40000
	je	.LBB4_42
# BB#41:                                # %cond.true113
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	(%rax), %r12
	jmp	.LBB4_43
.LBB4_1:                                # %if.then
	movq	8(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB4_22
# BB#2:                                 # %land.lhs.true
	movq	16(%rdi), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB4_22
# BB#3:                                 # %if.then4
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB4_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB4_6
.LBB4_42:                               # %cond.false118
	leaq	72(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	16(%rbx), %r15
	movq	%rax, %r12
.LBB4_43:                               # %cond.end120
	andq	$536870912, %r15        # imm = 0x20000000
	cmpq	$0, 16(%r13)
	jne	.LBB4_86
# BB#44:                                # %if.then127
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$6, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r13, %rdi
	movq	%r12, %rdx
	movq	%r12, %rcx
	callq	report_event
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB4_69
# BB#45:                                # %land.lhs.true131
	movzbl	304(%r13), %eax
	testq	%rax, %rax
	je	.LBB4_69
# BB#46:                                # %land.lhs.true134
	movzbl	145(%r13), %eax
	testq	%rax, %rax
	jne	.LBB4_69
# BB#47:                                # %land.lhs.true137
	movq	72(%rsp), %rbx
	testq	%r15, %r15
	jne	.LBB4_50
# BB#48:                                # %land.lhs.true137
	cmpq	$3, %rbx
	jb	.LBB4_50
# BB#49:                                # %land.lhs.true142
	movabsq	$.L.str81, %rsi
	movq	$3, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_68
.LBB4_50:                               # %lor.lhs.false
	testq	%r15, %r15
	je	.LBB4_53
# BB#51:                                # %lor.lhs.false
	cmpq	$6, %rbx
	jb	.LBB4_53
# BB#52:                                # %land.lhs.true149
	movabsq	$.L.str82, %rsi
	movq	$6, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_68
.LBB4_53:                               # %lor.lhs.false152
	testq	%r15, %r15
	jne	.LBB4_69
# BB#54:                                # %land.lhs.true154
	leaq	(%r12,%rbx), %rax
	movq	%rax, %rsi
.LBB4_55:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r12, %rsi
	jbe	.LBB4_60
# BB#56:                                # %land.rhs.i389
                                        #   in Loop: Header=BB4_55 Depth=1
	movsbq	-1(%rsi), %rcx
	decq	%rsi
	testq	%rcx, %rcx
	jns	.LBB4_58
# BB#57:                                # %land.rhs.i389
                                        #   in Loop: Header=BB4_55 Depth=1
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$192, %rdx
	jb	.LBB4_55
.LBB4_58:                               # %land.lhs.true.i
	addq	$64, %rcx
	andq	$255, %rcx
	cmpq	$62, %rcx
	jb	.LBB4_60
# BB#59:                                # %land.lhs.true.i
	incq	%rsi
.LBB4_60:                               # %if.end.i
	movq	%r12, %rcx
	subq	%rsi, %rcx
	addq	%rbx, %rcx
	je	.LBB4_64
# BB#61:                                # %land.lhs.true22.i
	movzbl	(%rsi), %edx
	movzbl	PL_utf8skip(%rdx), %edx
	cmpq	%rcx, %rdx
	je	.LBB4_63
# BB#62:                                # %land.lhs.true22.i
	movq	%rsi, %rax
.LBB4_63:                               # %land.lhs.true22.i
	movq	%rax, %rsi
.LBB4_64:                               # %if.end28.i
	movq	%r12, %rax
.LBB4_65:                               # %while.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rsi, %rax
	jae	.LBB4_69
# BB#66:                                # %while.body.i.i
                                        #   in Loop: Header=BB4_65 Depth=1
	movsbq	(%rax), %rcx
	incq	%rax
	testq	%rcx, %rcx
	jns	.LBB4_65
# BB#67:                                # %probably_utf8_chunk.exit
	subq	%r12, %rsi
	movq	%r12, %rdi
	callq	Perl_is_utf8_string
	testq	$255, %rax
	je	.LBB4_69
.LBB4_68:                               # %if.then158
	movabsq	$.L.str83, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
.LBB4_69:                               # %if.end159
	movzbl	PL_dowarn(%rip), %ebx
	testq	$1, %rbx
	je	.LBB4_74
# BB#70:                                # %if.end159
	testq	%r15, %r15
	je	.LBB4_74
# BB#71:                                # %if.end159
	movq	72(%rsp), %rax
	cmpq	$2, %rax
	jb	.LBB4_74
# BB#72:                                # %land.lhs.true168
	movabsq	$.L.str84, %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_74
# BB#73:                                # %if.then171
	movabsq	$.L.str85, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
	movzbl	PL_dowarn(%rip), %ebx
.LBB4_74:                               # %if.end172
	testq	$1, %rbx
	je	.LBB4_86
# BB#75:                                # %if.then176
	movq	72(%rsp), %rbx
	testq	%r15, %r15
	jne	.LBB4_80
# BB#76:                                # %if.then176
	cmpq	$4, %rbx
	jb	.LBB4_80
# BB#77:                                # %land.lhs.true181
	movzbl	(%r12), %eax
	testq	%rax, %rax
	je	.LBB4_79
# BB#78:                                # %lor.lhs.false184
	movabsq	$.L.str87, %rsi
	movq	$4, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_79
.LBB4_80:                               # %if.else188
	testq	%r15, %r15
	jne	.LBB4_87
# BB#81:                                # %if.else188
	cmpq	$2, %rbx
	jb	.LBB4_87
# BB#82:                                # %land.lhs.true193
	movabsq	$.L.str89, %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	strncmp
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB4_84
# BB#83:                                # %lor.lhs.false196
	movabsq	$.L.str84, %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	strncmp
	xorq	%r15, %r15
	testq	%rbx, %rax
	jne	.LBB4_86
.LBB4_84:                               # %if.then199
	movabsq	$.L.str90, %rdi
	jmp	.LBB4_85
.LBB4_38:                               # %cond.false101
	leaq	72(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	8(%r13), %rdi
	movq	%rax, %r12
.LBB4_39:                               # %cond.end104
	movq	16(%rdi), %r15
	andq	$536870912, %r15        # imm = 0x20000000
.LBB4_86:                               # %if.end204thread-pre-split
	movq	72(%rsp), %rbx
.LBB4_87:                               # %if.end204
	testq	%rbx, %rbx
	je	.LBB4_101
# BB#88:                                # %if.end207
	addq	%r12, %rbx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	parse_buf
	cmpq	%rbx, %rax
	je	.LBB4_90
# BB#89:                                # %lor.lhs.false212
	movzbl	41(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB4_93
.LBB4_90:                               # %if.then215
	movq	8(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB4_101
# BB#91:                                # %if.then218
	movq	16(%rdi), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rdi)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB4_101
# BB#92:                                # %land.rhs
	callq	Perl_sv_backoff
	jmp	.LBB4_101
.LBB4_93:                               # %if.else230
	movq	8(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB4_99
# BB#94:                                # %if.then233
	movzbl	18(%rdi), %ecx
	testq	$1807, %rcx             # imm = 0x70F
	je	.LBB4_96
# BB#95:                                # %if.then238
	movq	%rax, %rsi
	callq	Perl_sv_chop
	jmp	.LBB4_101
.LBB4_5:                                # %cond.false
	leaq	72(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	72(%rsp), %rcx
	movq	8(%r13), %rdi
	movq	%rax, %rbx
.LBB4_6:                                # %cond.end
	movq	16(%rdi), %r15
	leaq	(%rbx,%rcx), %r12
	andq	$536870912, %r15        # imm = 0x20000000
	testq	%rcx, %rcx
	jle	.LBB4_12
	.align	16, 0x90
.LBB4_7:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%r13), %r14
	testq	%r14, %r14
	jne	.LBB4_8
# BB#14:                                # %if.end27
                                        #   in Loop: Header=BB4_7 Depth=1
	movzbl	137(%r13), %eax
	testq	%rax, %rax
	jne	.LBB4_12
# BB#15:                                # %land.lhs.true29
                                        #   in Loop: Header=BB4_7 Depth=1
	movzbl	57(%r13), %ecx
	movzbl	(%rbx), %eax
	andq	$255, %rax
	testq	%rcx, %rcx
	jne	.LBB4_18
# BB#16:                                # %land.lhs.true31
                                        #   in Loop: Header=BB4_7 Depth=1
	cmpq	$60, %rax
	jne	.LBB4_12
# BB#17:                                # %if.then34
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	$1, %rax
	movb	%al, 57(%r13)
	jmp	.LBB4_11
	.align	16, 0x90
.LBB4_8:                                # %if.then17
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r14, %rdi
	movabsq	$.L.str80, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_10
# BB#9:                                 # %land.lhs.true21
                                        #   in Loop: Header=BB4_7 Depth=1
	movzbl	144(%r13), %eax
	testq	%rax, %rax
	je	.LBB4_12
.LBB4_10:                               # %if.end
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r14, 64(%r13)
	movq	$0, 48(%r13)
.LBB4_11:                               # %while.cond.backedge
                                        #   in Loop: Header=BB4_7 Depth=1
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	40(%rsp), %r8           # 8-byte Reload
	callq	parse_buf
	movq	%rax, %rbx
	cmpq	%r12, %rbx
	jb	.LBB4_7
	jmp	.LBB4_12
.LBB4_99:                               # %if.else252
	subq	%rax, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_newSVpv
	movq	%rax, 8(%r13)
	testq	%r15, %r15
	je	.LBB4_101
# BB#100:                               # %if.then259
	orq	$536870912, 16(%rax)    # imm = 0x20000000
	jmp	.LBB4_101
.LBB4_79:                               # %if.then187
	movabsq	$.L.str88, %rdi
.LBB4_85:                               # %if.end204thread-pre-split
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	Perl_warn
	jmp	.LBB4_86
.LBB4_96:                               # %if.else240
	subq	%rax, %rbx
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_setpvn
	movq	8(%r13), %rax
	movq	16(%rax), %rcx
	testq	%r15, %r15
	je	.LBB4_98
# BB#97:                                # %if.then243
	orq	$536870912, %rcx        # imm = 0x20000000
	movq	%rcx, 16(%rax)
	jmp	.LBB4_101
.LBB4_18:                               # %land.lhs.true40
	cmpq	$60, %rax
	jne	.LBB4_12
# BB#19:                                # %while.end.thread
	leaq	1(%rbx), %rax
	movq	%rax, 48(%rsp)
	movq	%r12, 56(%rsp)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	leaq	48(%rsp), %r9
	movq	$1, %rsi
	movq	$1, (%rsp)
	jmp	.LBB4_20
.LBB4_12:                               # %while.end
	cmpq	%r12, %rbx
	jae	.LBB4_21
# BB#13:                                # %if.then51
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
.LBB4_20:                               # %if.end52
	movq	%r13, %rdi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	report_event
.LBB4_21:                               # %if.end52
	movq	8(%r13), %rdi
	callq	Perl_sv_free
	movq	$0, 8(%r13)
.LBB4_22:                               # %if.end55
	movq	72(%r13), %rbx
	testq	%rbx, %rbx
	je	.LBB4_27
# BB#23:                                # %land.lhs.true57
	movzbl	18(%rbx), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB4_27
# BB#24:                                # %if.then62
	movzbl	141(%r13), %r14d
	movzbl	56(%r13), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	24(%r13), %r15
	movq	32(%r13), %r12
	xorq	%rax, %rax
	movb	%al, 141(%r13)
	movq	$0, 72(%r13)
	movzbl	80(%r13), %eax
	movb	%al, 56(%r13)
	movq	88(%r13), %rax
	movq	96(%r13), %rcx
	movq	%rcx, 24(%r13)
	movq	%rax, 16(%r13)
	movq	104(%r13), %rax
	movq	%rax, 32(%r13)
	movq	(%rbx), %rax
	movq	16(%rbx), %r8
	movq	(%rax), %rdx
	movq	8(%rax), %rcx
	addq	%rdx, %rcx
	andq	$536870912, %r8         # imm = 0x20000000
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r13, %rdi
	callq	report_event
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%rbx)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB4_26
# BB#25:                                # %land.rhs.i
	movq	%rbx, %rdi
	callq	Perl_sv_backoff
.LBB4_26:                               # %flush_pending_text.exit
	movb	%r14b, 141(%r13)
	movq	%rbx, 72(%r13)
	movq	24(%rsp), %rax          # 8-byte Reload
	movb	%al, 56(%r13)
	movq	%r15, 24(%r13)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r13)
	movq	%r12, 32(%r13)
.LBB4_27:                               # %if.end63
	movq	336(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB4_29
# BB#28:                                # %if.then65
	callq	Perl_sv_free
	movq	$0, 336(%r13)
.LBB4_29:                               # %if.end68
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$0, (%rsp)
	leaq	71(%rsp), %rdx
	movq	$7, %rsi
	xorq	%r8, %r8
	xorq	%r9, %r9
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	movq	%rdx, %rcx
	callq	report_event
	movq	$0, 16(%r13)
	cmpq	$0, 24(%r13)
	je	.LBB4_31
# BB#30:                                # %if.then71
	movq	$1, 24(%r13)
.LBB4_31:                               # %if.end73
	movq	$0, 32(%r13)
	movq	$0, 48(%r13)
	movb	%bl, 56(%r13)
.LBB4_101:                              # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_98:                               # %if.else246
	andq	$-536870913, %rcx       # imm = 0xFFFFFFFFDFFFFFFF
	movq	%rcx, 16(%rax)
	jmp	.LBB4_101
.Ltmp60:
	.size	parse, .Ltmp60-parse
	.cfi_endproc

	.globl	XS_HTML__Parser_eof
	.align	16, 0x90
	.type	XS_HTML__Parser_eof,@function
XS_HTML__Parser_eof:                    # @XS_HTML__Parser_eof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp67:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp68:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp69:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp70:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp71:
	.cfi_def_cfa_offset 48
.Ltmp72:
	.cfi_offset %rbx, -48
.Ltmp73:
	.cfi_offset %r12, -40
.Ltmp74:
	.cfi_offset %r13, -32
.Ltmp75:
	.cfi_offset %r14, -24
.Ltmp76:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r15
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r15, %r13
	subq	%rdx, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	cmpq	$1, %r13
	je	.LBB5_2
# BB#1:                                 # %if.then
	movabsq	$.L.str26, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB5_2:                                # %if.end
	movq	8(%rax,%rbx,8), %r14
	movq	(%r14), %rdi
	callq	get_pstate_hv
	movq	%rax, %r12
	movzbl	40(%r12), %eax
	testq	%rax, %rax
	je	.LBB5_4
# BB#3:                                 # %if.then7
	movq	$1, %rax
	movb	%al, 41(%r12)
	jmp	.LBB5_5
.LBB5_4:                                # %if.else
	movq	$1, %rax
	movb	%al, 40(%r12)
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	%r14, %rdx
	callq	parse
	movb	%bl, 40(%r12)
.LBB5_5:                                # %if.end10
	movq	$1, %rax
	subq	%r13, %rax
	leaq	(%r15,%rax,8), %rcx
	movq	%r14, (%r15,%rax,8)
	movq	%rcx, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp77:
	.size	XS_HTML__Parser_eof, .Ltmp77-XS_HTML__Parser_eof
	.cfi_endproc

	.globl	XS_HTML__Parser_strict_comment
	.align	16, 0x90
	.type	XS_HTML__Parser_strict_comment,@function
XS_HTML__Parser_strict_comment:         # @XS_HTML__Parser_strict_comment
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
.Ltmp89:
	.cfi_offset %rbx, -48
.Ltmp90:
	.cfi_offset %r12, -40
.Ltmp91:
	.cfi_offset %r13, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rcx
	movq	PL_stack_sp(%rip), %r13
	movq	PL_stack_base(%rip), %rax
	leaq	-8(%rcx), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rcx), %r15
	leaq	(%rax,%r15,8), %rsi
	movq	(%rdi), %rdx
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	movq	88(%rdx), %rbx
	sarq	%cl, %r15
	subq	%rsi, %r13
	cmpq	$7, %r13
	jg	.LBB6_2
# BB#1:                                 # %if.then
	movq	96(%rdx), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movabsq	$.L.str27, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB6_2:                                # %if.end
	movq	8(%rax,%r15,8), %rax
	movq	(%rax), %rdi
	leaq	1(%r15), %r12
	callq	get_pstate_hv
	movq	%rax, %r14
	leaq	-1(%rbx), %rax
	cmpq	$9, %rax
	jbe	.LBB6_3
# BB#14:                                # %sw.default
	movabsq	$.L.str28, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
                                        # implicit-def: R14
.LBB6_15:                               # %sw.epilog
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB6_17
# BB#16:
	movabsq	$PL_sv_yes, %rbx
	jmp	.LBB6_18
.LBB6_3:                                # %if.end
	jmpq	*.LJTI6_0(,%rax,8)
.LBB6_4:                                # %sw.bb
	addq	$137, %r14
	jmp	.LBB6_15
.LBB6_17:                               # %select.mid
	movabsq	$PL_sv_no, %rbx
.LBB6_18:                               # %select.end
	cmpq	$9, %r13
	jl	.LBB6_37
# BB#19:                                # %if.then19
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r15,8), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB6_36
# BB#20:                                # %cond.false
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB6_27
# BB#21:                                # %cond.true26
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB6_36
# BB#22:                                # %land.rhs
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB6_36
# BB#23:                                # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB6_36
# BB#24:                                # %land.rhs35
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB6_25
# BB#26:                                # %land.rhs35
	movabsq	$1, %rax
	jmp	.LBB6_36
.LBB6_27:                               # %cond.false41
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB6_31
# BB#28:                                # %cond.true47
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	je	.LBB6_29
# BB#30:                                # %cond.true47
	movabsq	$1, %rax
	jmp	.LBB6_36
.LBB6_5:                                # %sw.bb8
	addq	$138, %r14
	jmp	.LBB6_15
.LBB6_6:                                # %sw.bb9
	addq	$140, %r14
	jmp	.LBB6_15
.LBB6_7:                                # %sw.bb10
	addq	$141, %r14
	jmp	.LBB6_15
.LBB6_8:                                # %sw.bb11
	addq	$136, %r14
	jmp	.LBB6_15
.LBB6_9:                                # %sw.bb12
	addq	$142, %r14
	jmp	.LBB6_15
.LBB6_10:                               # %sw.bb13
	addq	$143, %r14
	jmp	.LBB6_15
.LBB6_11:                               # %sw.bb14
	addq	$139, %r14
	jmp	.LBB6_15
.LBB6_12:                               # %sw.bb15
	addq	$144, %r14
	jmp	.LBB6_15
.LBB6_13:                               # %sw.bb16
	addq	$145, %r14
	jmp	.LBB6_15
.LBB6_31:                               # %cond.false53
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB6_35
# BB#32:                                # %cond.true59
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB6_33
# BB#34:                                # %cond.true59
	movabsq	$1, %rax
	jmp	.LBB6_36
.LBB6_29:
	movabsq	$0, %rax
	jmp	.LBB6_36
.LBB6_25:
	movabsq	$0, %rax
	jmp	.LBB6_36
.LBB6_35:                               # %cond.false65
	callq	Perl_sv_2bool
	jmp	.LBB6_36
.LBB6_33:
	movabsq	$0, %rax
.LBB6_36:                               # %cond.end75
	movb	%al, (%r14)
.LBB6_37:                               # %if.end78
	movq	PL_stack_base(%rip), %rax
	movq	%rbx, 8(%rax,%r15,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r15,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp94:
	.size	XS_HTML__Parser_strict_comment, .Ltmp94-XS_HTML__Parser_strict_comment
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI6_0:
	.quad	.LBB6_4
	.quad	.LBB6_5
	.quad	.LBB6_6
	.quad	.LBB6_7
	.quad	.LBB6_8
	.quad	.LBB6_9
	.quad	.LBB6_10
	.quad	.LBB6_11
	.quad	.LBB6_12
	.quad	.LBB6_13

	.text
	.globl	XS_HTML__Parser_boolean_attribute_value
	.align	16, 0x90
	.type	XS_HTML__Parser_boolean_attribute_value,@function
XS_HTML__Parser_boolean_attribute_value: # @XS_HTML__Parser_boolean_attribute_value
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
.Ltmp106:
	.cfi_offset %rbx, -48
.Ltmp107:
	.cfi_offset %r12, -40
.Ltmp108:
	.cfi_offset %r13, -32
.Ltmp109:
	.cfi_offset %r14, -24
.Ltmp110:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r12
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rdx, %r12
	cmpq	$7, %r12
	jg	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$.L.str29, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB7_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rax
	movq	(%rax), %rdi
	leaq	1(%rbx), %r13
	callq	get_pstate_hv
	movq	%rax, %r15
	movq	152(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB7_3
# BB#4:                                 # %cond.true
	callq	Perl_newSVsv
	movq	%rax, %r14
	jmp	.LBB7_5
.LBB7_3:
	movabsq	$PL_sv_undef, %r14
.LBB7_5:                                # %cond.end
	cmpq	$9, %r12
	jl	.LBB7_7
# BB#6:                                 # %if.then9
	movq	152(%r15), %rdi
	callq	Perl_sv_free
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%rbx,8), %rdi
	callq	Perl_newSVsv
	movq	%rax, 152(%r15)
.LBB7_7:                                # %if.end15
	movq	PL_stack_base(%rip), %rax
	movq	%r14, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp111:
	.size	XS_HTML__Parser_boolean_attribute_value, .Ltmp111-XS_HTML__Parser_boolean_attribute_value
	.cfi_endproc

	.globl	XS_HTML__Parser_ignore_tags
	.align	16, 0x90
	.type	XS_HTML__Parser_ignore_tags,@function
XS_HTML__Parser_ignore_tags:            # @XS_HTML__Parser_ignore_tags
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp118:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp119:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp120:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp121:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp123:
	.cfi_def_cfa_offset 96
.Ltmp124:
	.cfi_offset %rbx, -48
.Ltmp125:
	.cfi_offset %r12, -40
.Ltmp126:
	.cfi_offset %r13, -32
.Ltmp127:
	.cfi_offset %r14, -24
.Ltmp128:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rsi
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rbx
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	(%rdi), %rdx
	movq	$3, %rcx
	sarq	%cl, %r14
	subq	%rbx, %rsi
	movq	88(%rdx), %rbx
	movq	$3, %rcx
	movq	%rsi, %rdi
	sarq	%cl, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	cmpq	$7, %rsi
	jg	.LBB8_2
# BB#1:                                 # %if.then
	movq	96(%rdx), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movabsq	$.L.str27, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB8_2:                                # %if.end
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	8(%rax,%r14,8), %rax
	movq	(%rax), %rdi
	leaq	1(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	callq	get_pstate_hv
	movq	%rax, 40(%rsp)          # 8-byte Spill
	cmpq	$3, %rbx
	je	.LBB8_7
# BB#3:                                 # %if.end
	cmpq	$2, %rbx
	jne	.LBB8_4
# BB#6:                                 # %sw.bb8
	addq	$320, 40(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x140
	jmp	.LBB8_9
.LBB8_7:                                # %sw.bb9
	addq	$328, 40(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x148
	jmp	.LBB8_9
.LBB8_4:                                # %if.end
	cmpq	$1, %rbx
	jne	.LBB8_8
# BB#5:                                 # %sw.bb
	addq	$312, 40(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x138
	jmp	.LBB8_9
.LBB8_8:                                # %sw.default
	movabsq	$.L.str30, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
                                        # implicit-def: RAX
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB8_9:                                # %sw.epilog
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %eax
	andq	$3, %rax
	leaq	-2(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB8_12
# BB#10:                                # %sw.epilog
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB8_13
# BB#11:                                # %cond.false25
	callq	Perl_block_gimme
	cmpq	$128, %rax
	je	.LBB8_13
.LBB8_12:                               # %if.then33
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB8_13:                               # %if.end34
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	decq	24(%rsp)                # 8-byte Folded Spill
	je	.LBB8_29
# BB#14:                                # %if.then35
	testq	%rdi, %rdi
	je	.LBB8_16
# BB#15:                                # %if.then37
	callq	Perl_hv_clear
	jmp	.LBB8_17
.LBB8_29:                               # %if.else77
	testq	%rdi, %rdi
	je	.LBB8_31
# BB#30:                                # %if.then79
	callq	Perl_sv_free
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	$0, (%rax)
	jmp	.LBB8_31
.LBB8_16:                               # %if.else
	callq	Perl_newHV
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
.LBB8_17:                               # %for.cond.preheader
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jle	.LBB8_31
# BB#18:
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB8_19:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_23 Depth 2
	movq	PL_stack_base(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	1(%rdx,%rcx), %rcx
	movq	(%rax,%rcx,8), %rbx
	movzbl	18(%rbx), %eax
	incq	%rdx
	testq	$8, %rax
	jne	.LBB8_20
# BB#27:                                # %if.else70
                                        #   in Loop: Header=BB8_19 Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r14
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_store_ent
	jmp	.LBB8_28
	.align	16, 0x90
.LBB8_20:                               # %if.then49
                                        #   in Loop: Header=BB8_19 Depth=1
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	(%rbx), %rax
	movq	(%rax), %r12
	movzbl	16(%r12), %eax
	cmpq	$10, %rax
	jne	.LBB8_26
# BB#21:                                # %if.then55
                                        #   in Loop: Header=BB8_19 Depth=1
	movq	%r12, %rdi
	callq	Perl_av_len
	movq	%rax, %r13
	cmpq	$-1, %r13
	je	.LBB8_28
# BB#22:                                # %for.body61.preheader
                                        #   in Loop: Header=BB8_19 Depth=1
	incq	%r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB8_23:                               # %for.body61
                                        #   Parent Loop BB8_19 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB8_25
# BB#24:                                # %if.then64
                                        #   in Loop: Header=BB8_23 Depth=2
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %r14
	movq	(%rax), %r15
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_store_ent
.LBB8_25:                               # %for.inc
                                        #   in Loop: Header=BB8_23 Depth=2
	incq	%rbx
	cmpq	%rbx, %r13
	jne	.LBB8_23
	jmp	.LBB8_28
	.align	16, 0x90
.LBB8_26:                               # %if.else68
                                        #   in Loop: Header=BB8_19 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str32, %rdi
	callq	Perl_croak
.LBB8_28:                               # %for.cond.backedge
                                        #   in Loop: Header=BB8_19 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
	cmpq	%rax, %rdx
	jne	.LBB8_19
.LBB8_31:                               # %if.end81
	movq	PL_stack_base(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp129:
	.size	XS_HTML__Parser_ignore_tags, .Ltmp129-XS_HTML__Parser_ignore_tags
	.cfi_endproc

	.globl	XS_HTML__Parser_handler
	.align	16, 0x90
	.type	XS_HTML__Parser_handler,@function
XS_HTML__Parser_handler:                # @XS_HTML__Parser_handler
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp136:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp137:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp138:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp139:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp140:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp141:
	.cfi_def_cfa_offset 144
.Ltmp142:
	.cfi_offset %rbx, -48
.Ltmp143:
	.cfi_offset %r12, -40
.Ltmp144:
	.cfi_offset %r13, -32
.Ltmp145:
	.cfi_offset %r14, -24
.Ltmp146:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r14
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r14, %rsi
	subq	%rdx, %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	$3, %rcx
	movq	%rsi, %r15
	sarq	%cl, %r15
	cmpq	$15, %rsi
	jg	.LBB9_2
# BB#1:                                 # %if.then
	movabsq	$.L.str33, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB9_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rax
	movq	(%rax), %rdi
	callq	get_pstate_hv
	movq	PL_stack_base(%rip), %rcx
	movq	16(%rcx,%rbx,8), %rdi
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movzbl	18(%rdi), %ecx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	$4, %rcx
	je	.LBB9_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movq	(%rax), %r13
	xorq	%r12, %r12
	jmp	.LBB9_6
.LBB9_4:                                # %cond.false
	leaq	72(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %r13
	xorq	%r12, %r12
	.align	16, 0x90
.LBB9_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	event_id_str(,%r12,8), %rsi
	movq	%r13, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_7
# BB#5:                                 # %for.cond
                                        #   in Loop: Header=BB9_6 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$8, %rax
	movq	$-1, %rbx
	jle	.LBB9_6
	jmp	.LBB9_8
.LBB9_7:                                # %for.end
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r12, %rbx
	jns	.LBB9_9
.LBB9_8:                                # %if.then18
	movabsq	$.L.str34, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	Perl_croak
	movq	%rbx, %r12
.LBB9_9:                                # %if.end19
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$4, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	160(%rbx,%rax), %rdi
	leaq	160(%rbx,%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB9_14
# BB#10:                                # %if.then23
	movzbl	16(%rdi), %eax
	cmpq	$10, %rax
	jne	.LBB9_12
# BB#11:                                # %cond.true28
	callq	Perl_newRV
	jmp	.LBB9_13
.LBB9_14:                               # %if.else
	movq	$1, %rax
	subq	%r15, %rax
	leaq	(%r14,%rax,8), %r15
	movq	$PL_sv_undef, (%r14,%rax,8)
	jmp	.LBB9_15
.LBB9_12:                               # %cond.false32
	callq	Perl_newSVsv
.LBB9_13:                               # %cond.end36
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	$1, %rcx
	subq	%r15, %rcx
	leaq	(%r14,%rcx,8), %r15
	movq	%rax, (%r14,%rcx,8)
.LBB9_15:                               # %if.end40
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	$25, %rax
	jl	.LBB9_77
# BB#16:                                # %if.then42
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	168(%rbx,%r12), %rdi
	leaq	168(%rbx,%r12), %r14
	callq	Perl_sv_free
	movq	$0, 168(%rbx,%r12)
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	32(%rax,%rcx,8), %rbx
	movabsq	$.L.str63, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	16(%rbx), %rcx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB9_18
# BB#17:                                # %cond.true.i
	movq	(%rbx), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 88(%rsp)
	movq	(%rax), %r15
	jmp	.LBB9_19
.LBB9_18:                               # %cond.false.i
	leaq	88(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	88(%rsp), %rdx
	movq	16(%rbx), %rcx
	movq	%rax, %r15
.LBB9_19:                               # %cond.end.i
	movq	%r14, (%rsp)            # 8-byte Spill
	testq	$536870912, %rcx        # imm = 0x20000000
	je	.LBB9_21
# BB#20:                                # %if.then.i
	movq	64(%rsp), %rax          # 8-byte Reload
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB9_21:                               # %while.cond.i.preheader
	addq	%r15, %rdx
	decq	%r15
	.align	16, 0x90
.LBB9_22:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%r15), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r15
	testq	$1, %rcx
	jne	.LBB9_22
# BB#23:                                # %while.end.i
	movq	%rdx, %r14
	cmpq	$64, %rax
	jne	.LBB9_29
# BB#24:
	movq	%r15, %rbx
	.align	16, 0x90
.LBB9_25:                               # %while.cond13.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%rbx), %ecx
	movzbl	hctype(%rcx), %eax
	incq	%rbx
	testq	$1, %rax
	jne	.LBB9_25
# BB#26:                                # %while.end21.i
	movq	$64, %rax
	cmpq	$123, %rcx
	jne	.LBB9_29
# BB#27:                                # %if.then25.i
	movq	$22, %rax
	movb	%al, 87(%rsp)
	leaq	87(%rsp), %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	Perl_sv_catpvn_flags
	movq	%rbx, %r15
	.align	16, 0x90
.LBB9_28:                               # %while.cond28.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%r15), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r15
	testq	$1, %rcx
	jne	.LBB9_28
.LBB9_29:                               # %while.cond39.preheader.i
	movq	%r14, 40(%rsp)          # 8-byte Spill
	cmpq	%r14, %r15
	jae	.LBB9_76
# BB#30:                                # %while.body42.lr.ph.i
	movzbl	hctype(%rax), %ecx
	.align	16, 0x90
.LBB9_32:                               # %while.body42.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_54 Depth 2
                                        #     Child Loop BB9_34 Depth 2
                                        #     Child Loop BB9_36 Depth 2
                                        #     Child Loop BB9_66 Depth 2
                                        #     Child Loop BB9_69 Depth 2
                                        #     Child Loop BB9_75 Depth 2
	testq	$2, %rcx
	movq	%r15, %rcx
	jne	.LBB9_34
# BB#33:                                # %while.body42.i
                                        #   in Loop: Header=BB9_32 Depth=1
	andq	$255, %rax
	cmpq	$64, %rax
	movq	%r15, %rcx
	je	.LBB9_34
# BB#51:                                # %if.else120.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$34, %rax
	je	.LBB9_53
# BB#52:                                # %if.else120.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$39, %rax
	jne	.LBB9_64
.LBB9_53:                               # %while.cond131.i.preheader
                                        #   in Loop: Header=BB9_32 Depth=1
	leaq	1(%r15), %rdx
	.align	16, 0x90
.LBB9_54:                               # %while.cond131.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rbx
	movzbl	(%rbx), %ecx
	cmpq	%r14, %rbx
	jae	.LBB9_57
# BB#55:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB9_54 Depth=2
	movq	%rcx, %rsi
	andq	$255, %rsi
	cmpq	$92, %rsi
	je	.LBB9_57
# BB#56:                                # %land.lhs.true134.i
                                        #   in Loop: Header=BB9_54 Depth=2
	leaq	1(%rbx), %rdx
	cmpq	%rax, %rsi
	jne	.LBB9_54
.LBB9_57:                               # %while.end144.i
                                        #   in Loop: Header=BB9_32 Depth=1
	andq	$255, %rcx
	cmpq	%rax, %rcx
	jne	.LBB9_61
# BB#58:                                # %if.then149.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	%r14, %r13
	movq	%rbx, %r14
	subq	%r15, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	addq	%rax, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$256, %rax              # imm = 0x100
	jl	.LBB9_60
# BB#59:                                # %if.then159.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str66, %rdi
	callq	Perl_croak
.LBB9_60:                               # %if.end160.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	$21, %rax
	movb	%al, 84(%rsp)
	movb	%r14b, 85(%rsp)
	movq	$2, %rdx
	movq	$2, %rcx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	leaq	84(%rsp), %rsi
	callq	Perl_sv_catpvn_flags
	incq	%r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$2, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	Perl_sv_catpvn_flags
	incq	%rbx
	movq	%r13, %r14
	jmp	.LBB9_65
	.align	16, 0x90
.LBB9_34:                               # %while.cond56.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%rcx), %eax
	movzbl	hctype(%rax), %eax
	incq	%rcx
	testq	$4, %rax
	jne	.LBB9_34
# BB#35:                                # %for.cond.preheader.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	%rcx, %rdx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	subq	%r15, %rdx
	movq	$1, %r12
	movabsq	$argname, %r13
	.align	16, 0x90
.LBB9_36:                               # %for.body.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13), %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rdx, %r14
                                        # kill: RDX<def> R14<kill>
	callq	strncmp
	movq	%r14, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_38
# BB#37:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB9_36 Depth=2
	movzbl	(%rbx,%rdx), %eax
	testq	%rax, %rax
	je	.LBB9_40
.LBB9_38:                               # %for.inc.i
                                        #   in Loop: Header=BB9_36 Depth=2
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$8, %r13
	cmpq	$21, %rax
	jl	.LBB9_36
# BB#39:                                # %if.else.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str65, %rdi
	movq	%rdx, %rsi
	movq	%r15, %rdx
	callq	Perl_croak
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	56(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB9_65
.LBB9_61:                               # %if.else167.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	cmpq	$92, %rcx
	jne	.LBB9_63
# BB#62:                                # %if.then171.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movabsq	$.L.str67, %rdi
	callq	Perl_croak
	jmp	.LBB9_65
.LBB9_40:                               # %if.then81.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movb	%r12b, 86(%rsp)
	movq	$1, %rdx
	movq	$2, %rcx
	movq	64(%rsp), %rdi          # 8-byte Reload
	leaq	86(%rsp), %rsi
	callq	Perl_sv_catpvn_flags
	leaq	-17(%r12), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	movq	48(%rsp), %r14          # 8-byte Reload
	ja	.LBB9_43
# BB#41:                                # %if.then90.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$0, 24(%r14)
	jne	.LBB9_43
# BB#42:                                # %if.then92.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	$1, 24(%r14)
.LBB9_43:                               # %if.end95.i
                                        #   in Loop: Header=BB9_32 Depth=1
	leaq	-7(%r12), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	movq	56(%rsp), %rbx          # 8-byte Reload
	jb	.LBB9_50
# BB#44:                                # %if.end95.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$11, %r12
	jne	.LBB9_45
.LBB9_50:                               # %if.then113.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movzbl	304(%r14), %eax
	incq	%rax
	movb	%al, 304(%r14)
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB9_65
.LBB9_64:                               # %if.else175.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str69, %rdi
	movq	%r15, %rsi
	callq	Perl_croak
	movq	%r15, %rbx
	jmp	.LBB9_65
.LBB9_63:                               # %if.else172.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movabsq	$.L.str68, %rdi
	callq	Perl_croak
	jmp	.LBB9_65
.LBB9_45:                               # %if.end95.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$13, %r12
	jne	.LBB9_46
# BB#47:                                # %if.then98.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$0, 112(%r14)
	jne	.LBB9_48
# BB#49:                                # %if.then100.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str63, %rdi
	callq	Perl_newSVpvn
	movq	%rax, 112(%r14)
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB9_65
.LBB9_46:                               #   in Loop: Header=BB9_32 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	jmp	.LBB9_65
.LBB9_48:                               #   in Loop: Header=BB9_32 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB9_65:                               # %while.cond178.i.preheader
                                        #   in Loop: Header=BB9_32 Depth=1
	decq	%rbx
	movq	%rbx, %r15
	.align	16, 0x90
.LBB9_66:                               # %while.cond178.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r15), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r15
	testq	$1, %rcx
	jne	.LBB9_66
# BB#67:                                # %while.end186.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	$125, %rax
	jne	.LBB9_72
# BB#68:                                # %land.lhs.true190.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$22, %rax
	jne	.LBB9_72
	.align	16, 0x90
.LBB9_69:                               # %while.cond199.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r15), %eax
	movzbl	hctype(%rax), %eax
	incq	%r15
	testq	$1, %rax
	jne	.LBB9_69
# BB#70:                                # %while.end207.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	%r14, %r15
	jae	.LBB9_72
# BB#71:                                # %if.then210.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str70, %rdi
	movq	%r15, %rsi
	callq	Perl_croak
	.align	16, 0x90
.LBB9_72:                               # %if.end212.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	%r14, %r15
	je	.LBB9_76
# BB#73:                                # %if.end216.i
                                        #   in Loop: Header=BB9_32 Depth=1
	movzbl	(%r15), %eax
	cmpq	$44, %rax
	je	.LBB9_75
# BB#74:                                # %if.then220.i
                                        #   in Loop: Header=BB9_32 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str71, %rdi
	callq	Perl_croak
	.align	16, 0x90
.LBB9_75:                               # %while.cond223.i
                                        #   Parent Loop BB9_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r15), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r15
	testq	$1, %rcx
	jne	.LBB9_75
# BB#31:                                # %while.cond39.loopexit.i
                                        #   in Loop: Header=BB9_32 Depth=1
	cmpq	%r14, %r15
	jb	.LBB9_32
.LBB9_76:                               # %argspec_compile.exit
	movq	(%rsp), %rax            # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, (%rax)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB9_77:                               # %if.end48
	cmpq	$17, %rax
	jl	.LBB9_87
# BB#78:                                # %if.then50
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	(%r14), %rdi
	callq	Perl_sv_free
	movq	$0, (%r14)
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	24(%rax,%rcx,8), %rbx
	movq	16(%rbx), %rcx
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB9_82
# BB#79:                                # %if.then.i89
	movq	(%rbx), %rax
	movq	(%rax), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$10, %rax
	jne	.LBB9_80
# BB#84:                                # %if.then7.i
	movq	%rcx, PL_Sv(%rip)
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB9_86
# BB#85:                                # %land.rhs.i
	incq	8(%rcx)
	movq	%rcx, %rax
	jmp	.LBB9_86
.LBB9_80:                               # %if.then.i89
	cmpq	$12, %rax
	je	.LBB9_83
# BB#81:                                # %if.end10.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%rbx), %rcx
.LBB9_82:                               # %if.end11.i
	xorq	%rax, %rax
	testq	$118423552, %rcx        # imm = 0x70F0000
	je	.LBB9_86
.LBB9_83:                               # %cond.true.i93
	movq	%rbx, %rdi
	callq	Perl_newSVsv
.LBB9_86:                               # %check_handler.exit
	movq	%rax, (%r14)
.LBB9_87:                               # %if.end57
	movq	%r15, PL_stack_sp(%rip)
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp147:
	.size	XS_HTML__Parser_handler, .Ltmp147-XS_HTML__Parser_handler
	.cfi_endproc

	.globl	XS_HTML__Entities_decode_entities
	.align	16, 0x90
	.type	XS_HTML__Entities_decode_entities,@function
XS_HTML__Entities_decode_entities:      # @XS_HTML__Entities_decode_entities
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp154:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp155:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp156:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp157:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp158:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp159:
	.cfi_def_cfa_offset 80
.Ltmp160:
	.cfi_offset %rbx, -48
.Ltmp161:
	.cfi_offset %r12, -40
.Ltmp162:
	.cfi_offset %r13, -32
.Ltmp163:
	.cfi_offset %r14, -24
.Ltmp164:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_sp(%rip), %r14
	movq	%r14, (%rsp)            # 8-byte Spill
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	subq	%rax, %r14
	movq	$3, %rcx
	movq	%r14, %rax
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str22, %rdi
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	callq	Perl_get_hv
	movq	PL_op(%rip), %rcx
	movzbl	36(%rcx), %ecx
	movq	%rax, %r12
	andq	$3, %rcx
	cmpq	$1, %rcx
	je	.LBB10_6
# BB#1:                                 # %cond.false
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$1, %r15
	cmpq	$2, %rcx
	je	.LBB10_6
# BB#2:                                 # %cond.false13
	cmpq	$3, %rcx
	movabsq	$0, %r15
	je	.LBB10_6
# BB#3:                                 # %cond.false20
	callq	Perl_block_gimme
	testq	%rax, %rax
	jne	.LBB10_4
# BB#5:                                 # %cond.false20
	movabsq	$1, %r15
	jmp	.LBB10_6
.LBB10_4:
	movabsq	$0, %r15
.LBB10_6:                               # %cond.end24
	incq	24(%rsp)                # 8-byte Folded Spill
	cmpq	$8, %r14
	jle	.LBB10_7
# BB#8:                                 # %cond.end24
	movabsq	$1, %rax
	jmp	.LBB10_9
.LBB10_7:
	movabsq	$0, %rax
.LBB10_9:                               # %cond.end24
	testq	%rax, %r15
	movq	$1, %rax
	jne	.LBB10_11
# BB#10:                                # %cond.end24
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB10_11:                              # %cond.end24
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB10_22
# BB#12:                                # %for.body.preheader
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	incq	%rbx
	xorq	%r14, %r14
	movq	16(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB10_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %eax
	movq	%rax, %rcx
	andq	$3, %rcx
	cmpq	$1, %rcx
	jne	.LBB10_17
# BB#14:                                # %cond.end59.thread101
                                        #   in Loop: Header=BB10_13 Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%r14,%rax), %r13
	jmp	.LBB10_15
	.align	16, 0x90
.LBB10_17:                              # %cond.false39
                                        #   in Loop: Header=BB10_13 Depth=1
	testq	$2, %rax
	jne	.LBB10_18
# BB#19:                                # %cond.end59
                                        #   in Loop: Header=BB10_13 Depth=1
	callq	Perl_block_gimme
	movq	PL_stack_base(%rip), %rcx
	movq	(%rcx,%rbx,8), %rdi
	cmpq	$128, %rax
	movq	%rbx, %r13
	jne	.LBB10_20
	.align	16, 0x90
.LBB10_15:                              # %if.else
                                        #   in Loop: Header=BB10_13 Depth=1
	movzbl	18(%rdi), %eax
	testq	$128, %rax
	je	.LBB10_21
# BB#16:                                # %if.then75
                                        #   in Loop: Header=BB10_13 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str35, %rdi
	callq	Perl_croak
	jmp	.LBB10_21
	.align	16, 0x90
.LBB10_18:                              # %cond.end59.thread
                                        #   in Loop: Header=BB10_13 Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%r14,%rax), %r13
.LBB10_20:                              # %if.then63
                                        #   in Loop: Header=BB10_13 Depth=1
	callq	Perl_newSVsv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, (%rcx,%r13,8)
.LBB10_21:                              # %if.end77
                                        #   in Loop: Header=BB10_13 Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	(%rax,%r13,8), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rsi
	callq	decode_entities
	incq	%r14
	incq	%rbx
	decq	%r15
	jne	.LBB10_13
.LBB10_22:                              # %for.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	subq	%rax, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp165:
	.size	XS_HTML__Entities_decode_entities, .Ltmp165-XS_HTML__Entities_decode_entities
	.cfi_endproc

	.align	16, 0x90
	.type	decode_entities,@function
decode_entities:                        # @decode_entities
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
	subq	$128, %rsp
.Ltmp177:
	.cfi_def_cfa_offset 176
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
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB11_2
# BB#1:                                 # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 120(%rsp)
	movq	(%rax), %r12
	jmp	.LBB11_3
.LBB11_2:                               # %cond.false
	leaq	120(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	120(%rsp), %rcx
	movq	%rax, %r12
.LBB11_3:                               # %cond.end
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	addq	%r12, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%r12, %r14
	jmp	.LBB11_4
.LBB11_80:                              # %if.then283
                                        #   in Loop: Header=BB11_4 Depth=1
	callq	Perl_safesysfree
	.align	16, 0x90
.LBB11_4:                               # %while.cond.outer81
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_5 Depth 2
                                        #     Child Loop BB11_9 Depth 2
                                        #     Child Loop BB11_50 Depth 2
                                        #     Child Loop BB11_23 Depth 2
                                        #     Child Loop BB11_16 Depth 2
                                        #     Child Loop BB11_27 Depth 2
	leaq	3(%r12), %rdx
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	.align	16, 0x90
.LBB11_5:                               # %while.cond
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rcx
	leaq	(%r12,%rax), %rdx
	cmpq	%rdi, %rdx
	jae	.LBB11_81
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB11_5 Depth=2
	movzbl	(%rdx), %esi
	leaq	1(%rax), %r15
	leaq	1(%rcx), %rdx
	cmpq	$38, %rsi
	movb	%sil, (%r14,%rax)
	movq	%r15, %rax
	jne	.LBB11_5
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB11_4 Depth=1
	movzbl	(%r12,%r15), %edx
	leaq	(%r12,%r15), %r13
	movq	%r14, 24(%rsp)          # 8-byte Spill
	leaq	(%r14,%r15), %r14
	cmpq	$35, %rdx
	jne	.LBB11_8
# BB#14:                                # %if.then9
                                        #   in Loop: Header=BB11_4 Depth=1
	movzbl	1(%r12,%r15), %eax
	movq	%rax, %rdx
	orq	$32, %rdx
	andq	$255, %rdx
	cmpq	$120, %rdx
	jne	.LBB11_21
# BB#15:                                # %while.cond19.preheader
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	PL_hexdigit(%rip), %rbx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_16:                              # %while.cond19
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	%rcx, %r12
	movzbl	(%r12), %esi
	movq	%rax, %r8
	testq	%rsi, %rsi
	je	.LBB11_28
# BB#17:                                # %while.body20
                                        #   in Loop: Header=BB11_16 Depth=2
	movq	$56, %rcx
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%rbx, %rdi
	movq	%r12, %r14
	movq	%r8, %r12
	callq	strchr
	movq	%r12, %r8
	movq	%r14, %r12
	testq	%rax, %rax
	je	.LBB11_28
# BB#18:                                # %if.end25
                                        #   in Loop: Header=BB11_16 Depth=2
	movq	$4, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	subq	%rbx, %rax
	andq	$15, %rax
	orq	%rdx, %rax
	leaq	1(%r12), %rcx
	testq	%r8, %r8
	movq	$1, %rdx
	je	.LBB11_16
# BB#19:                                # %if.end25
                                        #   in Loop: Header=BB11_16 Depth=2
	cmpq	%r8, %rax
	ja	.LBB11_16
	jmp	.LBB11_20
	.align	16, 0x90
.LBB11_8:                               # %while.cond115.preheader
                                        #   in Loop: Header=BB11_4 Depth=1
	addq	%r15, %r12
	xorq	%rax, %rax
	jmp	.LBB11_9
	.align	16, 0x90
.LBB11_13:                              # %while.body142
                                        #   in Loop: Header=BB11_9 Depth=2
	movzbl	1(%r12,%rax), %edx
	incq	%rax
.LBB11_9:                               # %while.cond115
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-65(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB11_13
# BB#10:                                # %while.cond115
                                        #   in Loop: Header=BB11_9 Depth=2
	leaq	-97(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB11_13
# BB#11:                                # %lor.lhs.false131
                                        #   in Loop: Header=BB11_9 Depth=2
	leaq	-48(%rdx), %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	jb	.LBB11_13
# BB#12:                                # %lor.lhs.false131
                                        #   in Loop: Header=BB11_9 Depth=2
	andq	$255, %rdx
	cmpq	$95, %rdx
	je	.LBB11_13
# BB#43:                                # %while.end144
                                        #   in Loop: Header=BB11_4 Depth=1
	addq	%rax, %r12
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB11_26
# BB#44:                                # %while.end144
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB11_26
# BB#45:                                # %if.then149
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%r12, %rdx
	subq	%r13, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rsi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	jne	.LBB11_46
# BB#49:                                # %if.else173
                                        #   in Loop: Header=BB11_4 Depth=1
	testq	$255, 32(%rsp)          # 8-byte Folded Reload
	movq	%r12, %rbx
	je	.LBB11_25
	.align	16, 0x90
.LBB11_50:                              # %while.cond178
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	%rbx, %rdx
	subq	%r13, %rdx
	jbe	.LBB11_25
# BB#51:                                # %while.body181
                                        #   in Loop: Header=BB11_50 Depth=2
	xorq	%rcx, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB11_50
# BB#52:                                # %if.then187
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	(%rax), %rdi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB11_53
# BB#54:                                # %cond.false197
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	$2, %rdx
	leaq	112(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	jmp	.LBB11_55
.LBB11_28:                              # %if.end51
                                        #   in Loop: Header=BB11_4 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB11_29
.LBB11_20:                              #   in Loop: Header=BB11_4 Depth=1
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	16(%rsp), %r14          # 8-byte Reload
	jmp	.LBB11_26
.LBB11_21:                              # %while.cond33.preheader
                                        #   in Loop: Header=BB11_4 Depth=1
	leaq	1(%r12,%r15), %r12
	leaq	-48(%rax), %rcx
	andq	$255, %rcx
	xorq	%r8, %r8
	cmpq	$10, %rcx
	jae	.LBB11_25
	.align	16, 0x90
.LBB11_23:                              # %while.body40
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r8, %rdx
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	leaq	(%rdx,%rdx,4), %rcx
	testq	%rdx, %rdx
	leaq	-48(%rax,%rcx,2), %r8
	je	.LBB11_22
# BB#24:                                # %while.body40
                                        #   in Loop: Header=BB11_23 Depth=2
	cmpq	%rdx, %r8
	jb	.LBB11_25
.LBB11_22:                              # %while.cond33
                                        #   in Loop: Header=BB11_23 Depth=2
	movzbl	1(%r12), %eax
	incq	%r12
	leaq	-48(%rax), %rcx
	andq	$255, %rcx
	cmpq	$9, %rcx
	jbe	.LBB11_23
.LBB11_29:                              # %if.then53
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	$536870912, 16(%rax)    # imm = 0x20000000
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r15), %rsi
	jne	.LBB11_32
# BB#30:                                # %if.then53
                                        #   in Loop: Header=BB11_4 Depth=1
	cmpq	$255, %r8
	ja	.LBB11_32
# BB#31:                                # %if.then60
                                        #   in Loop: Header=BB11_4 Depth=1
	movb	%r8b, 99(%rsp)
	movq	$1, 112(%rsp)
	xorq	%rax, %rax
	leaq	99(%rsp), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_58
.LBB11_25:                              #   in Loop: Header=BB11_4 Depth=1
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_26
.LBB11_46:                              # %if.then156
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	(%rax), %rdi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB11_47
# BB#48:                                # %cond.false166
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	$2, %rdx
	leaq	112(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	jmp	.LBB11_56
.LBB11_47:                              # %cond.true161
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 112(%rsp)
	movq	(%rax), %rax
	jmp	.LBB11_56
.LBB11_32:                              # %if.else62
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%r8, %rax
	movabsq	$4294966272, %rcx       # imm = 0xFFFFFC00
	andq	%rcx, %rax
	movq	%rsi, %r14
	cmpq	$55296, %rax            # imm = 0xD800
	jne	.LBB11_33
# BB#36:                                # %if.then85
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%r8, %r13
	jmp	.LBB11_41
.LBB11_53:                              # %cond.true192
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 112(%rsp)
	movq	(%rax), %rax
.LBB11_55:                              # %if.end210
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%rbx, %r12
	movq	48(%rsp), %rbx          # 8-byte Reload
.LBB11_56:                              # %if.end210
                                        #   in Loop: Header=BB11_4 Depth=1
	testq	%rax, %rax
	jne	.LBB11_57
	.align	16, 0x90
.LBB11_26:                              # %while.cond286.preheader
                                        #   in Loop: Header=BB11_4 Depth=1
	cmpq	%r12, %r13
	jae	.LBB11_4
	.align	16, 0x90
.LBB11_27:                              # %while.body289
                                        #   Parent Loop BB11_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	incq	%r13
	movb	%al, (%r14)
	incq	%r14
	cmpq	%r13, %r12
	jne	.LBB11_27
	jmp	.LBB11_4
.LBB11_33:                              # %if.else62
                                        #   in Loop: Header=BB11_4 Depth=1
	cmpq	$56320, %rax            # imm = 0xDC00
	jne	.LBB11_37
# BB#34:                                # %if.then68
                                        #   in Loop: Header=BB11_4 Depth=1
	xorq	%r13, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movq	%rcx, %rax
	je	.LBB11_41
# BB#35:                                # %if.then71
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	$10, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdx
	movabsq	$-243194379878006784, %rax # imm = 0xFCA0000000000000
	addq	%rax, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	9216(%rdx,%r8), %r8
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	-3(%rax,%r15), %r14
	jmp	.LBB11_42
.LBB11_37:                              # %if.else87
                                        #   in Loop: Header=BB11_4 Depth=1
	leaq	-64976(%r8), %rax
	cmpq	$32, %rax
	jb	.LBB11_40
# BB#38:                                # %lor.lhs.false93
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%r8, %rax
	andq	$65534, %rax            # imm = 0xFFFE
	cmpq	$65534, %rax            # imm = 0xFFFE
	je	.LBB11_40
# BB#39:                                # %lor.lhs.false93
                                        #   in Loop: Header=BB11_4 Depth=1
	xorq	%r13, %r13
	cmpq	$1114112, %r8           # imm = 0x110000
	jb	.LBB11_42
.LBB11_40:                              # %if.then100
                                        #   in Loop: Header=BB11_4 Depth=1
	xorq	%r13, %r13
.LBB11_41:                              # %if.end103
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	$65533, %r8             # imm = 0xFFFD
.LBB11_42:                              # %if.end103
                                        #   in Loop: Header=BB11_4 Depth=1
	leaq	99(%rsp), %r15
	movq	%r15, %rdi
	movq	%r8, %rsi
	callq	Perl_uvuni_to_utf8
	subq	%r15, %rax
	movq	%rax, 112(%rsp)
	movq	$1, %rax
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %rsi
	jmp	.LBB11_58
.LBB11_57:                              # %if.then212.loopexit
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movzbl	19(%rax), %eax
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	%r15, %rcx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%rcx, %rsi
.LBB11_58:                              # %if.then212
                                        #   in Loop: Header=BB11_4 Depth=1
	movzbl	(%r12), %ecx
	cmpq	$59, %rcx
	jne	.LBB11_60
# BB#59:                                #   in Loop: Header=BB11_4 Depth=1
	incq	%r12
.LBB11_60:                              # %select.end
                                        #   in Loop: Header=BB11_4 Depth=1
	movzbl	(%r12), %ecx
	cmpq	$38, %rcx
	je	.LBB11_62
# BB#61:                                # %select.mid56
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
.LBB11_62:                              # %select.end55
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	16(%rbx), %rcx
	andq	$536870912, %rcx        # imm = 0x20000000
	movabsq	$0, %rdx
	jne	.LBB11_64
# BB#63:                                # %select.end55
                                        #   in Loop: Header=BB11_4 Depth=1
	movabsq	$1, %rdx
.LBB11_64:                              # %select.end55
                                        #   in Loop: Header=BB11_4 Depth=1
	decq	%rsi
	andq	%rax, %rdx
	cmpq	$1, %rdx
	jne	.LBB11_66
# BB#65:                                # %if.then231
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	(%rbx), %rax
	movq	(%rax), %rdi
	subq	%rdi, %rsi
	movq	%rsi, 88(%rsp)
	leaq	88(%rsp), %rsi
	callq	Perl_bytes_to_utf8
	movq	%rax, %r15
	movq	64(%rsp), %rax          # 8-byte Reload
	subq	%r12, %rax
	movq	%rax, 80(%rsp)
	movq	%r12, %rdi
	leaq	80(%rsp), %rsi
	callq	Perl_bytes_to_utf8
	movq	88(%rsp), %rdx
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpvn
	movq	80(%rsp), %rdx
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catpvn_flags
	orq	$536870912, 16(%rbx)    # imm = 0x20000000
	movq	%r15, %rdi
	callq	Perl_safesysfree
	movq	%r14, %rdi
	callq	Perl_safesysfree
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	88(%rsp), %rcx
	movq	80(%rsp), %rdx
	leaq	(%rax,%rcx), %r12
	addq	%rcx, %rdx
	addq	%rax, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r12, %r14
	jmp	.LBB11_71
.LBB11_66:                              # %if.else257
                                        #   in Loop: Header=BB11_4 Depth=1
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	jne	.LBB11_68
# BB#67:                                # %if.else257
                                        #   in Loop: Header=BB11_4 Depth=1
	movabsq	$1, %rcx
.LBB11_68:                              # %if.else257
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%rsi, %rbx
	xorq	%rdi, %rdi
	orq	%rax, %rcx
	jne	.LBB11_70
# BB#69:                                # %if.then263
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	leaq	112(%rsp), %rsi
	callq	Perl_bytes_to_utf8
	movq	%rax, %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
.LBB11_70:                              # %if.end266
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%rbx, %r14
.LBB11_71:                              # %if.end266
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	112(%rsp), %rbx
	leaq	(%r14,%rbx), %rax
	cmpq	%r12, %rax
	jbe	.LBB11_72
# BB#73:                                # %if.then270
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%r14, %rcx
	subq	%r12, %rcx
	movq	(%rax), %r13
	addq	%rcx, %rbx
	movq	16(%rax), %rax
	subq	%r13, %r14
	subq	%r13, %r12
	movq	64(%rsp), %rcx          # 8-byte Reload
	subq	%r13, %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	leaq	1(%rcx,%rbx), %rsi
	cmpq	%rsi, %rax
	jae	.LBB11_75
# BB#74:                                # %cond.true.i
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	Perl_sv_grow
	movq	(%r15), %rax
	movq	(%rax), %r13
.LBB11_75:                              # %grow_gap.exit
                                        #   in Loop: Header=BB11_4 Depth=1
	addq	%r13, %r14
	leaq	(%r13,%r12), %rsi
	leaq	(%r12,%rbx), %rdi
	addq	%r13, %rdi
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	subq	%r12, %rdx
	movq	%rdi, %r12
	callq	memmove
	addq	%rbx, %r15
	addq	%r13, %r15
	movq	%r15, 64(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB11_76
.LBB11_72:                              #   in Loop: Header=BB11_4 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB11_77
.LBB11_76:                              # %while.cond276thread-pre-split
                                        #   in Loop: Header=BB11_4 Depth=1
	movq	112(%rsp), %rbx
.LBB11_77:                              # %while.cond276
                                        #   in Loop: Header=BB11_4 Depth=1
	testq	%rbx, %rbx
	leaq	-1(%rbx), %rax
	movq	%rax, 112(%rsp)
	jne	.LBB11_78
# BB#79:                                # %while.end281
                                        #   in Loop: Header=BB11_4 Depth=1
	testq	%rdi, %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB11_4
	jmp	.LBB11_80
.LBB11_78:                              # %while.body278
                                        #   in Loop: Header=BB11_4 Depth=1
	movzbl	(%rcx), %eax
	incq	%rcx
	movb	%al, (%r14)
	incq	%r14
	jmp	.LBB11_76
.LBB11_81:                              # %while.end294
	xorq	%rcx, %rcx
	movb	%cl, (%r14,%rax)
	movq	(%rbx), %rcx
	movq	(%rcx), %rdx
	leaq	(%r14,%rax), %rax
	subq	%rdx, %rax
	movq	%rax, 8(%rcx)
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp183:
	.size	decode_entities, .Ltmp183-decode_entities
	.cfi_endproc

	.globl	XS_HTML__Entities__decode_entities
	.align	16, 0x90
	.type	XS_HTML__Entities__decode_entities,@function
XS_HTML__Entities__decode_entities:     # @XS_HTML__Entities__decode_entities
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r13
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	subq	%rdx, %rbx
	cmpq	$15, %rbx
	jg	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB12_2:                               # %if.end
	movq	8(%rax,%r13,8), %r14
	movq	16(%rax,%r13,8), %rdx
	xorq	%r15, %r15
	cmpq	$17, %rbx
	movabsq	$0, %r12
	jl	.LBB12_21
# BB#3:                                 # %cond.true
	movq	24(%rax,%r13,8), %rdi
	testq	%rdi, %rdi
	movabsq	$0, %r12
	je	.LBB12_21
# BB#4:                                 # %cond.false
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB12_11
# BB#5:                                 # %cond.true15
	movq	(%rdi), %rax
	xorq	%r12, %r12
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB12_21
# BB#6:                                 # %land.rhs
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	movq	$1, %r12
	ja	.LBB12_21
# BB#7:                                 # %lor.rhs
	testq	%rcx, %rcx
	movabsq	$0, %r12
	je	.LBB12_21
# BB#8:                                 # %land.rhs22
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB12_9
# BB#10:                                # %land.rhs22
	movabsq	$1, %r12
	jmp	.LBB12_21
.LBB12_11:                              # %cond.false26
	movq	%rdx, %rbx
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB12_15
# BB#12:                                # %cond.true32
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	movq	%rbx, %rdx
	je	.LBB12_13
# BB#14:                                # %cond.true32
	movabsq	$1, %r12
	jmp	.LBB12_21
.LBB12_15:                              # %cond.false38
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB12_19
# BB#16:                                # %cond.true44
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB12_17
# BB#18:                                # %cond.true44
	movabsq	$1, %r12
	jmp	.LBB12_20
.LBB12_13:
	movabsq	$0, %r12
	jmp	.LBB12_21
.LBB12_9:
	movabsq	$0, %r12
	jmp	.LBB12_21
.LBB12_19:                              # %cond.false50
	callq	Perl_sv_2bool
	movq	%rax, %r12
	jmp	.LBB12_20
.LBB12_17:
	movabsq	$0, %r12
.LBB12_20:                              # %cond.end62
	movq	%rbx, %rdx
.LBB12_21:                              # %cond.end62
	movq	16(%rdx), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB12_25
# BB#22:                                # %if.then68
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB12_24
# BB#23:                                # %land.lhs.true
	movq	(%rdx), %rax
	movq	(%rax), %r15
	movzbl	16(%r15), %eax
	cmpq	$11, %rax
	je	.LBB12_25
.LBB12_24:                              # %if.else
	movabsq	$.L.str37, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
                                        # implicit-def: R15
.LBB12_25:                              # %if.end82
	movzbl	18(%r14), %eax
	testq	$128, %rax
	je	.LBB12_27
# BB#26:                                # %if.then86
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB12_27:                              # %if.end87
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	decode_entities
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp200:
	.size	XS_HTML__Entities__decode_entities, .Ltmp200-XS_HTML__Entities__decode_entities
	.cfi_endproc

	.globl	XS_HTML__Entities__probably_utf8_chunk
	.align	16, 0x90
	.type	XS_HTML__Entities__probably_utf8_chunk,@function
XS_HTML__Entities__probably_utf8_chunk: # @XS_HTML__Entities__probably_utf8_chunk
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp205:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp206:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp207:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp208:
	.cfi_def_cfa_offset 48
.Ltmp209:
	.cfi_offset %rbx, -32
.Ltmp210:
	.cfi_offset %r14, -24
.Ltmp211:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB13_2
# BB#1:                                 # %if.then
	movabsq	$.L.str38, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB13_2:                               # %if.end
	movq	8(%rax,%r14,8), %rbx
	leaq	1(%r14), %r15
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_downgrade
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB13_4
# BB#3:                                 # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB13_5
.LBB13_4:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rcx
	movq	%rax, %rdi
.LBB13_5:                               # %cond.end
	leaq	(%rdi,%rcx), %rax
	movq	%rax, %rsi
	.align	16, 0x90
.LBB13_6:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rdi, %rsi
	jbe	.LBB13_11
# BB#7:                                 # %land.rhs.i
                                        #   in Loop: Header=BB13_6 Depth=1
	movsbq	-1(%rsi), %rbx
	decq	%rsi
	testq	%rbx, %rbx
	jns	.LBB13_9
# BB#8:                                 # %land.rhs.i
                                        #   in Loop: Header=BB13_6 Depth=1
	movq	%rbx, %rdx
	andq	$255, %rdx
	cmpq	$192, %rdx
	jb	.LBB13_6
.LBB13_9:                               # %land.lhs.true.i
	addq	$64, %rbx
	andq	$255, %rbx
	cmpq	$62, %rbx
	jb	.LBB13_11
# BB#10:                                # %land.lhs.true.i
	incq	%rsi
.LBB13_11:                              # %if.end.i
	movq	%rdi, %rdx
	subq	%rsi, %rdx
	addq	%rcx, %rdx
	je	.LBB13_15
# BB#12:                                # %land.lhs.true22.i
	movzbl	(%rsi), %ecx
	movzbl	PL_utf8skip(%rcx), %ecx
	cmpq	%rdx, %rcx
	je	.LBB13_14
# BB#13:                                # %land.lhs.true22.i
	movq	%rsi, %rax
.LBB13_14:                              # %land.lhs.true22.i
	movq	%rax, %rsi
.LBB13_15:                              # %if.end28.i
	movabsq	$PL_sv_no, %rax
	movq	%rdi, %rcx
	.align	16, 0x90
.LBB13_16:                              # %while.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rsi, %rcx
	jae	.LBB13_21
# BB#17:                                # %while.body.i.i
                                        #   in Loop: Header=BB13_16 Depth=1
	movsbq	(%rcx), %rdx
	incq	%rcx
	testq	%rdx, %rdx
	jns	.LBB13_16
# BB#18:                                # %if.end31.i
	subq	%rdi, %rsi
	callq	Perl_is_utf8_string
	testq	$255, %rax
	jne	.LBB13_19
# BB#20:                                # %if.end31.i
	movabsq	$PL_sv_no, %rax
	jmp	.LBB13_21
.LBB13_19:
	movabsq	$PL_sv_yes, %rax
.LBB13_21:                              # %probably_utf8_chunk.exit
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r14,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp212:
	.size	XS_HTML__Entities__probably_utf8_chunk, .Ltmp212-XS_HTML__Entities__probably_utf8_chunk
	.cfi_endproc

	.globl	XS_HTML__Entities_UNICODE_SUPPORT
	.align	16, 0x90
	.type	XS_HTML__Entities_UNICODE_SUPPORT,@function
XS_HTML__Entities_UNICODE_SUPPORT:      # @XS_HTML__Entities_UNICODE_SUPPORT
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp218:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp219:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp220:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp221:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp222:
	.cfi_def_cfa_offset 48
.Ltmp223:
	.cfi_offset %rbx, -40
.Ltmp224:
	.cfi_offset %r12, -32
.Ltmp225:
	.cfi_offset %r14, -24
.Ltmp226:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rdx
	je	.LBB14_2
# BB#1:                                 # %if.then
	movabsq	$.L.str39, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB14_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	leaq	1(%rbx), %r15
	testq	$32, %rcx
	je	.LBB14_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB14_5
.LBB14_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB14_5:                               # %cond.end
	movq	PL_stack_base(%rip), %r12
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB14_7
# BB#6:                                 # %if.then10
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB14_7:                               # %if.end12
	movq	%r14, 8(%r12,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp227:
	.size	XS_HTML__Entities_UNICODE_SUPPORT, .Ltmp227-XS_HTML__Entities_UNICODE_SUPPORT
	.cfi_endproc

	.globl	boot_HTML__Parser
	.align	16, 0x90
	.type	boot_HTML__Parser,@function
boot_HTML__Parser:                      # @boot_HTML__Parser
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp232:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp233:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp234:
	.cfi_def_cfa_offset 32
.Ltmp235:
	.cfi_offset %rbx, -32
.Ltmp236:
	.cfi_offset %r14, -24
.Ltmp237:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movabsq	$.L.str41, %rdi
	movabsq	$XS_HTML__Parser__alloc_pstate, %rsi
	movabsq	$.L.str40, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str42, %rdi
	movabsq	$XS_HTML__Parser_parse, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str43, %rdi
	movabsq	$XS_HTML__Parser_eof, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str44, %rdi
	movabsq	$XS_HTML__Parser_strict_comment, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$9, 88(%rax)
	movabsq	$.L.str45, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$8, 88(%rax)
	movabsq	$.L.str46, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$5, 88(%rax)
	movabsq	$.L.str47, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$7, 88(%rax)
	movabsq	$.L.str48, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$4, 88(%rax)
	movabsq	$.L.str49, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$1, 88(%rax)
	movabsq	$.L.str50, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$3, 88(%rax)
	movabsq	$.L.str51, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$6, 88(%rax)
	movabsq	$.L.str52, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$2, 88(%rax)
	movabsq	$.L.str53, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$10, 88(%rax)
	movabsq	$.L.str54, %rdi
	movabsq	$XS_HTML__Parser_boolean_attribute_value, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str55, %rdi
	movabsq	$XS_HTML__Parser_ignore_tags, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$2, 88(%rax)
	movabsq	$.L.str56, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$3, 88(%rax)
	movabsq	$.L.str57, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$1, 88(%rax)
	movabsq	$.L.str58, %rdi
	movabsq	$XS_HTML__Parser_handler, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str59, %rdi
	movabsq	$XS_HTML__Entities_decode_entities, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str60, %rdi
	movabsq	$XS_HTML__Entities__decode_entities, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str61, %rdi
	movabsq	$XS_HTML__Entities__probably_utf8_chunk, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str62, %rdi
	movabsq	$XS_HTML__Entities_UNICODE_SUPPORT, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str63, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp238:
	.size	boot_HTML__Parser, .Ltmp238-boot_HTML__Parser
	.cfi_endproc

	.align	16, 0x90
	.type	parse_buf,@function
parse_buf:                              # @parse_buf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp245:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp246:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp247:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp248:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp249:
	.cfi_def_cfa_offset 48
	subq	$752, %rsp              # imm = 0x2F0
.Ltmp250:
	.cfi_def_cfa_offset 800
.Ltmp251:
	.cfi_offset %rbx, -48
.Ltmp252:
	.cfi_offset %r12, -40
.Ltmp253:
	.cfi_offset %r13, -32
.Ltmp254:
	.cfi_offset %r14, -24
.Ltmp255:
	.cfi_offset %r15, -16
	movq	%rdi, %r10
	movq	%r10, 144(%rsp)         # 8-byte Spill
	movzbl	41(%r10), %eax
	movq	%rdx, %r14
	movq	%r14, 160(%rsp)         # 8-byte Spill
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB16_2
# BB#1:
	movq	%r12, %r15
.LBB16_530:                             # %DONE
	movq	%r15, %rax
	addq	$752, %rsp              # imm = 0x2F0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_2:                               # %while.cond1.preheader.lr.ph
	movq	%r8, 128(%rsp)          # 8-byte Spill
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	%r14, %rax
	leaq	-3(%r14), %rcx
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	leaq	-2(%r14), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	leaq	-1(%r14), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$0, %r13
	movabsq	$1, %rsi
	movq	%r12, %rcx
	negq	%rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
.LBB16_3:                               # %while.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_6 Depth 2
                                        #       Child Loop BB16_10 Depth 3
                                        #       Child Loop BB16_33 Depth 3
                                        #       Child Loop BB16_40 Depth 3
                                        #     Child Loop BB16_50 Depth 2
                                        #       Child Loop BB16_59 Depth 3
                                        #     Child Loop BB16_83 Depth 2
                                        #       Child Loop BB16_74 Depth 3
                                        #     Child Loop BB16_114 Depth 2
                                        #     Child Loop BB16_117 Depth 2
                                        #     Child Loop BB16_120 Depth 2
                                        #       Child Loop BB16_121 Depth 3
                                        #       Child Loop BB16_131 Depth 3
                                        #       Child Loop BB16_134 Depth 3
                                        #       Child Loop BB16_197 Depth 3
                                        #       Child Loop BB16_137 Depth 3
                                        #       Child Loop BB16_172 Depth 3
                                        #       Child Loop BB16_183 Depth 3
                                        #       Child Loop BB16_143 Depth 3
                                        #       Child Loop BB16_166 Depth 3
                                        #       Child Loop BB16_187 Depth 3
                                        #     Child Loop BB16_157 Depth 2
                                        #     Child Loop BB16_207 Depth 2
                                        #       Child Loop BB16_209 Depth 3
                                        #     Child Loop BB16_238 Depth 2
                                        #     Child Loop BB16_243 Depth 2
                                        #     Child Loop BB16_249 Depth 2
                                        #     Child Loop BB16_272 Depth 2
                                        #     Child Loop BB16_515 Depth 2
                                        #     Child Loop BB16_394 Depth 2
                                        #     Child Loop BB16_400 Depth 2
                                        #     Child Loop BB16_410 Depth 2
                                        #     Child Loop BB16_421 Depth 2
                                        #       Child Loop BB16_424 Depth 3
                                        #       Child Loop BB16_472 Depth 3
                                        #       Child Loop BB16_482 Depth 3
                                        #       Child Loop BB16_437 Depth 3
                                        #       Child Loop BB16_469 Depth 3
                                        #       Child Loop BB16_446 Depth 3
                                        #       Child Loop BB16_456 Depth 3
                                        #     Child Loop BB16_299 Depth 2
                                        #       Child Loop BB16_300 Depth 3
                                        #         Child Loop BB16_301 Depth 4
                                        #       Child Loop BB16_323 Depth 3
                                        #     Child Loop BB16_329 Depth 2
                                        #     Child Loop BB16_333 Depth 2
                                        #       Child Loop BB16_338 Depth 3
                                        #     Child Loop BB16_349 Depth 2
                                        #       Child Loop BB16_350 Depth 3
                                        #       Child Loop BB16_352 Depth 3
                                        #         Child Loop BB16_353 Depth 4
                                        #         Child Loop BB16_355 Depth 4
                                        #       Child Loop BB16_375 Depth 3
                                        #     Child Loop BB16_496 Depth 2
	movq	48(%r10), %r15
	movq	%r15, 152(%rsp)         # 8-byte Spill
	testq	%r15, %r15
	jne	.LBB16_5
# BB#4:                                 #   in Loop: Header=BB16_3 Depth=1
	movq	%rcx, %r15
	jmp	.LBB16_73
	.align	16, 0x90
.LBB16_5:                               # %while.body3.preheader
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	%r11, %r13
	leaq	1(%r15), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB16_6:                               # %while.body3
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_10 Depth 3
                                        #       Child Loop BB16_33 Depth 3
                                        #       Child Loop BB16_40 Depth 3
	movq	%r15, %rdi
	movabsq	$.L.str93, %rsi
	callq	strcmp
	movq	%r13, %rbx
	testq	%rbx, %rax
	movq	$1, %rax
	je	.LBB16_9
# BB#7:                                 # %lor.rhs
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	%r15, %rdi
	movabsq	$.L.str94, %rsi
	callq	strcmp
	testq	%rbx, %rax
	movabsq	$0, %rax
	jne	.LBB16_9
# BB#8:                                 # %lor.rhs
                                        #   in Loop: Header=BB16_6 Depth=2
	movabsq	$1, %rax
.LBB16_9:                               # %lor.end
                                        #   in Loop: Header=BB16_6 Depth=2
	xorq	%rcx, %rcx
	cmpq	%r14, %r12
	movabsq	$0, %rsi
	movq	168(%rsp), %r8          # 8-byte Reload
	jae	.LBB16_28
	.align	16, 0x90
.LBB16_10:                              # %while.body11
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %edx
	cmpq	$60, %rdx
	jne	.LBB16_12
# BB#11:                                # %while.body11
                                        #   in Loop: Header=BB16_10 Depth=3
	testq	$255, %rsi
	je	.LBB16_28
.LBB16_12:                              # %if.end
                                        #   in Loop: Header=BB16_10 Depth=3
	testq	%rax, %rax
	je	.LBB16_13
# BB#14:                                # %if.then17
                                        #   in Loop: Header=BB16_10 Depth=3
	testq	$255, %rcx
	jne	.LBB16_15
# BB#16:                                # %if.else
                                        #   in Loop: Header=BB16_10 Depth=3
	movq	$1, %rcx
	cmpq	$92, %rdx
	jne	.LBB16_18
# BB#17:                                #   in Loop: Header=BB16_10 Depth=3
	movq	%rsi, %rdx
	jmp	.LBB16_27
	.align	16, 0x90
.LBB16_13:                              #   in Loop: Header=BB16_10 Depth=3
	movq	%rsi, %rdx
	jmp	.LBB16_27
	.align	16, 0x90
.LBB16_15:                              #   in Loop: Header=BB16_10 Depth=3
	movq	%rsi, %rdx
	xorq	%rcx, %rcx
	jmp	.LBB16_27
.LBB16_18:                              # %if.else24
                                        #   in Loop: Header=BB16_10 Depth=3
	xorq	%rcx, %rcx
	testq	$255, %rsi
	je	.LBB16_20
# BB#19:                                # %if.else24
                                        #   in Loop: Header=BB16_10 Depth=3
	movq	%rsi, %rdi
	andq	$255, %rdi
	cmpq	%rdi, %rdx
	je	.LBB16_26
.LBB16_20:                              # %if.else33
                                        #   in Loop: Header=BB16_10 Depth=3
	cmpq	$10, %rdx
	je	.LBB16_26
# BB#21:                                # %if.else33
                                        #   in Loop: Header=BB16_10 Depth=3
	cmpq	$13, %rdx
	je	.LBB16_26
# BB#22:                                # %if.else41
                                        #   in Loop: Header=BB16_10 Depth=3
	testq	$255, %rsi
	je	.LBB16_24
# BB#23:                                #   in Loop: Header=BB16_10 Depth=3
	movq	%rsi, %rdx
	jmp	.LBB16_27
.LBB16_24:                              # %land.lhs.true43
                                        #   in Loop: Header=BB16_10 Depth=3
	cmpq	$34, %rdx
	je	.LBB16_27
# BB#25:                                # %land.lhs.true43
                                        #   in Loop: Header=BB16_10 Depth=3
	cmpq	$39, %rdx
	je	.LBB16_27
.LBB16_26:                              #   in Loop: Header=BB16_10 Depth=3
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB16_27:                              # %if.end57
                                        #   in Loop: Header=BB16_10 Depth=3
	incq	%r12
	cmpq	%r14, %r12
	movq	%rdx, %rsi
	jb	.LBB16_10
.LBB16_28:                              # %while.end
                                        #   in Loop: Header=BB16_6 Depth=2
	cmpq	%r14, %r12
	je	.LBB16_29
# BB#30:                                # %if.end61
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	%rbx, %r13
	movzbl	1(%r12), %eax
	cmpq	$47, %rax
	jne	.LBB16_31
# BB#32:                                # %if.then66
                                        #   in Loop: Header=BB16_6 Depth=2
	movzbl	(%r15), %edx
	leaq	2(%r12), %rbx
	testq	%rdx, %rdx
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rbx, %r15
	je	.LBB16_37
	.align	16, 0x90
.LBB16_33:                              # %land.rhs
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movsbq	(%r15), %rsi
	leaq	-65(%rsi), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jae	.LBB16_35
# BB#34:                                #   in Loop: Header=BB16_33 Depth=3
	addq	$32, %rsi
.LBB16_35:                              # %land.rhs
                                        #   in Loop: Header=BB16_33 Depth=3
	andq	%r13, %rsi
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	andq	%r13, %rdx
	cmpq	%rdx, %rsi
	jne	.LBB16_43
# BB#36:                                # %while.body83
                                        #   in Loop: Header=BB16_33 Depth=3
	movzbl	(%rax), %edx
	incq	%r15
	incq	%rax
	testq	%rdx, %rdx
	jne	.LBB16_33
.LBB16_37:                              # %land.lhs.true88
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	152(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str80, %rsi
	callq	strcmp
	testq	%r13, %rax
	jne	.LBB16_39
# BB#38:                                # %lor.lhs.false92
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	144(%rsp), %rax         # 8-byte Reload
	movzbl	144(%rax), %eax
	testq	%rax, %rax
	je	.LBB16_43
.LBB16_39:                              # %if.then95
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	%rbx, 176(%rsp)
	movq	%r15, 184(%rsp)
	.align	16, 0x90
.LBB16_40:                              # %while.cond98
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_6 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r15), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r15
	testq	$1, %rcx
	jne	.LBB16_40
# BB#41:                                # %while.end103
                                        #   in Loop: Header=BB16_6 Depth=2
	cmpq	$62, %rax
	je	.LBB16_47
# BB#42:                                #   in Loop: Header=BB16_6 Depth=2
	decq	%r15
.LBB16_43:                              # %while.cond1.backedge
                                        #   in Loop: Header=BB16_6 Depth=2
	movq	%r15, %r12
	movq	152(%rsp), %r15         # 8-byte Reload
	jmp	.LBB16_44
	.align	16, 0x90
.LBB16_31:                              #   in Loop: Header=BB16_6 Depth=2
	incq	%r12
.LBB16_44:                              # %while.cond1.backedge
                                        #   in Loop: Header=BB16_6 Depth=2
	testq	%r15, %r15
	jne	.LBB16_6
# BB#45:                                #   in Loop: Header=BB16_3 Depth=1
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	144(%rsp), %r10         # 8-byte Reload
	jmp	.LBB16_46
.LBB16_47:                              # %if.then107
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	cmpq	%r12, %rcx
	movq	144(%rsp), %rdi         # 8-byte Reload
	je	.LBB16_49
# BB#48:                                # %if.then111
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	movq	%rcx, %rdx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%rdi, %rbx
	callq	report_event
	movq	%rbx, %rdi
.LBB16_49:                              # %if.end112
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$3, %rsi
	movq	$1, (%rsp)
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	leaq	176(%rsp), %r9
	movq	%rdi, %rbx
	callq	report_event
	movq	%rbx, %r10
	movq	$0, 48(%r10)
	xorq	%rax, %rax
	movb	%al, 56(%r10)
	movq	%r15, %r12
.LBB16_46:                              # %while.cond118.outer
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, %r11
	movabsq	$0, %r13
	jmp	.LBB16_72
	.align	16, 0x90
.LBB16_50:                              # %while.cond118
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_59 Depth 3
	movq	%rax, %rcx
	andq	%r11, %rcx
	cmpq	$2, %rcx
	jne	.LBB16_51
# BB#57:                                # %while.cond128.preheader
                                        #   in Loop: Header=BB16_50 Depth=2
	movzbl	(%rbx), %ecx
	jmp	.LBB16_59
	.align	16, 0x90
.LBB16_58:                              # %while.cond128
                                        #   in Loop: Header=BB16_59 Depth=3
	movzbl	1(%rbx), %ecx
	incq	%rbx
.LBB16_59:                              # %while.cond128
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_50 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	$93, %rcx
	movq	%r13, %rcx
	jne	.LBB16_61
# BB#60:                                # %while.cond128
                                        #   in Loop: Header=BB16_59 Depth=3
	movq	%rsi, %rcx
.LBB16_61:                              # %while.cond128
                                        #   in Loop: Header=BB16_59 Depth=3
	cmpq	%r14, %rbx
	jae	.LBB16_63
# BB#62:                                # %land.rhs131
                                        #   in Loop: Header=BB16_59 Depth=3
	testq	$1, %rcx
	je	.LBB16_58
	jmp	.LBB16_64
	.align	16, 0x90
.LBB16_63:                              # %while.end138
                                        #   in Loop: Header=BB16_50 Depth=2
	testq	%rcx, %rcx
	je	.LBB16_66
.LBB16_64:                              # %if.then142
                                        #   in Loop: Header=BB16_50 Depth=2
	movzbl	1(%rbx), %ecx
	cmpq	$93, %rcx
	jne	.LBB16_65
# BB#67:                                # %if.then148
                                        #   in Loop: Header=BB16_50 Depth=2
	movzbl	2(%rbx), %ecx
	cmpq	$62, %rcx
	je	.LBB16_69
# BB#68:                                #   in Loop: Header=BB16_50 Depth=2
	addq	$2, %rbx
	jmp	.LBB16_66
	.align	16, 0x90
.LBB16_65:                              #   in Loop: Header=BB16_50 Depth=2
	incq	%rbx
.LBB16_66:                              # %if.end162
                                        #   in Loop: Header=BB16_50 Depth=2
	cmpq	%r14, %rbx
	jne	.LBB16_50
	jmp	.LBB16_530
.LBB16_51:                              # %while.cond168.preheader
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r14, %rbx
	jae	.LBB16_52
.LBB16_83:                              # %land.rhs171.lr.ph
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_74 Depth 3
	movq	144(%rsp), %rdi         # 8-byte Reload
	.align	16, 0x90
.LBB16_74:                              # %land.rhs171
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_83 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%rbx), %eax
	cmpq	$60, %rax
	je	.LBB16_52
# BB#75:                                # %while.body176
                                        #   in Loop: Header=BB16_74 Depth=3
	movl	120(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB16_79
# BB#76:                                # %while.body176
                                        #   in Loop: Header=BB16_74 Depth=3
	cmpq	$93, %rax
	jne	.LBB16_79
# BB#77:                                # %if.then183
                                        #   in Loop: Header=BB16_74 Depth=3
	movzbl	1(%rbx), %eax
	cmpq	$93, %rax
	jne	.LBB16_78
# BB#80:                                # %if.then189
                                        #   in Loop: Header=BB16_74 Depth=3
	movq	%r13, %r12
	movq	%r11, %r13
	movzbl	2(%rbx), %eax
	cmpq	$62, %rax
	je	.LBB16_82
# BB#81:                                #   in Loop: Header=BB16_74 Depth=3
	addq	$2, %rbx
	movq	%r13, %r11
	movq	%r12, %r13
	movabsq	$1, %rsi
	jmp	.LBB16_79
	.align	16, 0x90
.LBB16_78:                              #   in Loop: Header=BB16_74 Depth=3
	incq	%rbx
.LBB16_79:                              # %if.end200
                                        #   in Loop: Header=BB16_74 Depth=3
	incq	%rbx
	cmpq	%r14, %rbx
	jb	.LBB16_74
	jmp	.LBB16_52
.LBB16_82:                              # %if.then194
                                        #   in Loop: Header=BB16_83 Depth=2
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	120(%rsp), %r15         # 8-byte Reload
	movq	%r15, %r8
	callq	report_event
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$10, %rsi
	movq	%rbx, %rdx
	leaq	3(%rbx), %rbx
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	report_event
	movq	144(%rsp), %r15         # 8-byte Reload
	movq	128(%r15), %rdi
	callq	Perl_av_pop
	movq	%rax, %rdi
	callq	Perl_sv_free
	movq	%r15, %rdi
	callq	marked_section_update
	cmpq	%r14, %rbx
	movq	%rbx, %r15
	movq	%r13, %r11
	movq	%r12, %r13
	movabsq	$1, %rsi
	jb	.LBB16_83
	jmp	.LBB16_55
.LBB16_52:                              # %while.end202
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r15, %rbx
	je	.LBB16_55
# BB#53:                                # %if.then205
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %r12
	movzbl	(%rbx), %eax
	cmpq	$60, %rax
	jne	.LBB16_84
# BB#54:                                # %if.then209
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	report_event
	movq	%rbx, %r15
	movq	%r12, %r11
	movabsq	$1, %rsi
.LBB16_55:                              # %if.end266
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r14, %rax
	subq	%rbx, %rax
	cmpq	$3, %rax
	jl	.LBB16_56
# BB#102:                               # %if.end270
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	1(%rbx), %eax
	movzbl	hctype(%rax), %ecx
	incq	%rbx
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	testq	$2, %rcx
	jne	.LBB16_103
# BB#224:                               # %if.else279
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$33, %rax
	movq	144(%rsp), %r10         # 8-byte Reload
	je	.LBB16_292
# BB#225:                               # %if.else279
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$63, %rax
	jne	.LBB16_226
# BB#513:                               # %if.then295
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	2(%r15), %rax
	cmpq	%r14, %rax
	movq	%rax, 736(%rsp)
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#514:                               # %while.body.lr.ph.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	144(%rsp), %rdi         # 8-byte Reload
.LBB16_515:                             # %while.body.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$62, %rcx
	jne	.LBB16_523
# BB#516:                               # %if.then.i561
                                        #   in Loop: Header=BB16_515 Depth=2
	movq	%r11, %rbx
	movq	%rax, 744(%rsp)
	movzbl	140(%rdi), %edx
	leaq	1(%rax), %rcx
	testq	%rdx, %rdx
	movabsq	$0, %r13
	je	.LBB16_520
# BB#517:                               # %if.then5.i
                                        #   in Loop: Header=BB16_515 Depth=2
	movq	%rcx, %rdx
	subq	%r15, %rdx
	cmpq	$4, %rdx
	movq	%rbx, %r11
	jl	.LBB16_524
# BB#518:                               # %lor.lhs.false.i566
                                        #   in Loop: Header=BB16_515 Depth=2
	movzbl	-1(%rax), %edx
	cmpq	$63, %rdx
	jne	.LBB16_524
	jmp	.LBB16_519
.LBB16_523:                             # %if.end15.i
                                        #   in Loop: Header=BB16_515 Depth=2
	incq	%rax
	movq	%rax, %rcx
.LBB16_524:                             # %while.cond.backedge.i
                                        #   in Loop: Header=BB16_515 Depth=2
	cmpq	%r14, %rcx
	movq	%rcx, %rax
	jb	.LBB16_515
	jmp	.LBB16_525
.LBB16_103:                             # %if.then277
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	144(%rsp), %rcx         # 8-byte Reload
	movzbl	138(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB16_104
# BB#105:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	140(%rcx), %eax
	testq	%rax, %rax
	movq	%r13, %rax
	jne	.LBB16_107
# BB#106:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rsi, %rax
.LBB16_107:                             # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$16, %rdx
	je	.LBB16_109
# BB#108:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$4, %rdx
.LBB16_109:                             # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$8, %rsi
	je	.LBB16_111
# BB#110:                               # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$2, %rsi
.LBB16_111:                             # %lor.lhs.false.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	4(,%rax,4), %rax
	jmp	.LBB16_112
.LBB16_69:                              # %if.then153
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	3(%rbx), %r12
	cmpq	%rbx, %r15
	je	.LBB16_71
# BB#70:                                # %if.then157
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r10, %r15
	callq	report_event
	movq	%r15, %r10
.LBB16_71:                              # %if.end158
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$10, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r10, %rdi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r10, %rbx
	callq	report_event
	movq	128(%rbx), %rdi
	callq	Perl_av_pop
	movq	%rax, %rdi
	callq	Perl_sv_free
	movq	%rbx, %rdi
	callq	marked_section_update
	movq	%rbx, %r10
	movq	%r12, %r15
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
.LBB16_72:                              # %while.cond118.outer
                                        #   in Loop: Header=BB16_3 Depth=1
	movabsq	$1, %rsi
.LBB16_73:                              # %while.cond118.outer
                                        #   in Loop: Header=BB16_3 Depth=1
	movl	120(%r10), %eax
	andq	$-2, %rax
	movq	%r12, %rbx
	jmp	.LBB16_50
.LBB16_104:                             #   in Loop: Header=BB16_3 Depth=1
	movq	$4, %rdx
	movq	$2, %rsi
	movq	$4, %rax
.LBB16_112:                             # %if.end.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	leaq	2(%r15), %r13
	jmp	.LBB16_114
	.align	16, 0x90
.LBB16_113:                             # %while.cond.i
                                        #   in Loop: Header=BB16_114 Depth=2
	incq	%r13
.LBB16_114:                             # %while.cond.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %r13
	jae	.LBB16_116
# BB#115:                               # %land.rhs.i
                                        #   in Loop: Header=BB16_114 Depth=2
	movzbl	(%r13), %ecx
	movzbl	hctype(%rcx), %ecx
	testq	%rax, %rcx
	jne	.LBB16_113
.LBB16_116:                             # %if.end14.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 192(%rsp)
	movq	%r13, 200(%rsp)
	decq	%r13
	.align	16, 0x90
.LBB16_117:                             # %while.cond23.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r13), %eax
	movzbl	hctype(%rax), %eax
	incq	%r13
	testq	$1, %rax
	jne	.LBB16_117
# BB#118:                               # %while.end31.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	%r13, %r14
	movq	%r15, %rdx
	je	.LBB16_526
# BB#119:                               # %while.cond36.preheader.i
                                        #   in Loop: Header=BB16_3 Depth=1
	testq	%rsi, %rax
	movq	$16, 136(%rsp)          # 8-byte Folded Spill
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	leaq	192(%rsp), %rax
	movq	%rax, %r9
	movq	%r13, %r8
	movq	144(%rsp), %r10         # 8-byte Reload
	je	.LBB16_159
.LBB16_120:                             # %while.cond47.preheader.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_121 Depth 3
                                        #       Child Loop BB16_131 Depth 3
                                        #       Child Loop BB16_134 Depth 3
                                        #       Child Loop BB16_197 Depth 3
                                        #       Child Loop BB16_137 Depth 3
                                        #       Child Loop BB16_172 Depth 3
                                        #       Child Loop BB16_183 Depth 3
                                        #       Child Loop BB16_143 Depth 3
                                        #       Child Loop BB16_166 Depth 3
                                        #       Child Loop BB16_187 Depth 3
	leaq	1(%r13), %rax
	leaq	3(%r13), %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	subq	%r13, %rdx
	movq	%r13, %rsi
	movq	56(%rsp), %r10          # 8-byte Reload
	.align	16, 0x90
.LBB16_121:                             # %while.cond47.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rax, %r14
	movq	%rsi, %r8
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	jae	.LBB16_123
# BB#122:                               # %land.rhs50.i
                                        #   in Loop: Header=BB16_121 Depth=3
	movzbl	(%r14), %eax
	movzbl	hctype(%rax), %edi
	leaq	1(%r14), %rax
	leaq	1(%r8), %rsi
	leaq	1(%r12), %rcx
	leaq	-1(%rbx), %rdx
	testq	%r10, %rdi
	jne	.LBB16_121
.LBB16_123:                             # %while.end60.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	%r14, 160(%rsp)         # 8-byte Folded Reload
	movq	136(%rsp), %rdx         # 8-byte Reload
	je	.LBB16_222
# BB#124:                               # %if.end64.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %rcx
	movq	%rcx, %rax
	andq	%r11, %rax
	movq	%rdx, %rsi
	andq	%r11, %rsi
	cmpq	%rsi, %rax
	jne	.LBB16_125
# BB#126:                               # %if.then68.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, %rsi
	jl	.LBB16_128
# BB#127:                               # %if.then68.i
                                        #   in Loop: Header=BB16_120 Depth=2
	leaq	(%rdx,%rdx), %rsi
.LBB16_128:                             # %if.then68.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%r8, 168(%rsp)          # 8-byte Spill
	leaq	192(%rsp), %rax
	movq	$32, %rcx
	cmpq	%rax, %r9
	jne	.LBB16_129
# BB#130:                               # %if.else.i486.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rsi, %rdi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r9
	movq	$32, %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	testq	%rdx, %rdx
	movq	%r9, %rcx
	leaq	192(%rsp), %rdx
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r8           # 8-byte Reload
	jle	.LBB16_132
	.align	16, 0x90
.LBB16_131:                             # %for.body.i492.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	andq	%r11, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%r8, %rsi
	jne	.LBB16_131
	jmp	.LBB16_132
.LBB16_125:                             #   in Loop: Header=BB16_120 Depth=2
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	jmp	.LBB16_133
.LBB16_129:                             # %if.then1.i481.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%r9, %rdi
	callq	Perl_safesysrealloc
	movq	%rax, %r9
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
.LBB16_132:                             #   in Loop: Header=BB16_120 Depth=2
	movq	168(%rsp), %r8          # 8-byte Reload
.LBB16_133:                             # %if.end73.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	$32, %rcx
	movq	152(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r13, (%r9,%rax)
	movq	%r14, 8(%r9,%rax)
	.align	16, 0x90
.LBB16_134:                             # %while.cond83.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	1(%r8), %eax
	movq	%r12, %rdi
	movq	%rbx, %rcx
	movzbl	hctype(%rax), %edx
	incq	%r8
	leaq	1(%rdi), %r12
	leaq	-1(%rcx), %rbx
	testq	$1, %rdx
	jne	.LBB16_134
# BB#135:                               # %while.end91.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	%r8, 160(%rsp)          # 8-byte Folded Reload
	movq	144(%rsp), %r10         # 8-byte Reload
	je	.LBB16_222
# BB#136:                               # %if.end95.i
                                        #   in Loop: Header=BB16_120 Depth=2
	xorq	%r12, %r12
	cmpq	$61, %rax
	jne	.LBB16_190
	.align	16, 0x90
.LBB16_137:                             # %while.cond101.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rbx
	movzbl	1(%r8,%rbx), %eax
	movzbl	hctype(%rax), %esi
	movq	%rdi, %rdx
	leaq	1(%rbx), %r12
	leaq	1(%rdx), %rdi
	testq	$1, %rsi
	jne	.LBB16_137
# BB#138:                               # %while.end109.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	104(%rsp), %rdi         # 8-byte Reload
	leaq	(%r8,%rdi), %rdi
	addq	%r12, %rdi
	je	.LBB16_222
# BB#139:                               # %if.end113.i
                                        #   in Loop: Header=BB16_120 Depth=2
	leaq	(%r8,%r12), %r14
	cmpq	$34, %rax
	je	.LBB16_142
# BB#140:                               # %if.end113.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	$62, %rax
	je	.LBB16_151
# BB#141:                               # %if.end113.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	$39, %rax
	jne	.LBB16_148
.LBB16_142:                             # %while.cond147.i.preheader
                                        #   in Loop: Header=BB16_120 Depth=2
	subq	%rbx, %rcx
	movq	160(%rsp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB16_143:                             # %while.cond147.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %r8
	movq	%rcx, %rsi
	cmpq	%rbx, %r8
	jae	.LBB16_145
# BB#144:                               # %land.rhs150.i
                                        #   in Loop: Header=BB16_143 Depth=3
	movzbl	(%r8), %edi
	leaq	1(%r8), %rdx
	leaq	-1(%rsi), %rcx
	cmpq	%rax, %rdi
	jne	.LBB16_143
.LBB16_145:                             # %while.end158.i
                                        #   in Loop: Header=BB16_120 Depth=2
	testq	%rsi, %rsi
	je	.LBB16_222
# BB#146:                               # %if.end162.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	addq	$2, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	andq	%r11, %rax
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rbx
	andq	%r11, %rbx
	cmpq	%rbx, %rax
	jne	.LBB16_147
# BB#161:                               # %if.then167.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, %rax
	leaq	192(%rsp), %r12
	jl	.LBB16_163
# BB#162:                               # %if.then167.i
                                        #   in Loop: Header=BB16_120 Depth=2
	leaq	(%rdx,%rdx), %rax
.LBB16_163:                             # %if.then167.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, %r13
	movq	%r8, 168(%rsp)          # 8-byte Spill
	movq	%r11, %r14
	movq	$32, %rcx
	cmpq	%r12, %r9
	jne	.LBB16_164
# BB#165:                               # %if.else.i528.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rdi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r9
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$1, %rax
	testq	%r13, %r13
	movq	%r9, %rcx
	movq	%r12, %rdx
	movq	%r14, %r11
	jle	.LBB16_167
.LBB16_166:                             # %for.body.i534.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	andq	%r11, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%rbx, %rsi
	jne	.LBB16_166
.LBB16_167:                             #   in Loop: Header=BB16_120 Depth=2
	movq	160(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB16_168
.LBB16_190:                             # %if.else241.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	addq	$2, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	andq	%r11, %rax
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rbx
	andq	%r11, %rbx
	cmpq	%rbx, %rax
	jne	.LBB16_191
# BB#192:                               # %if.then245.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, %rax
	leaq	192(%rsp), %r12
	jl	.LBB16_194
# BB#193:                               # %if.then245.i
                                        #   in Loop: Header=BB16_120 Depth=2
	leaq	(%rdx,%rdx), %rax
.LBB16_194:                             # %if.then245.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, %r13
	movq	%r8, 168(%rsp)          # 8-byte Spill
	movq	%r11, %r14
	movq	$32, %rcx
	cmpq	%r12, %r9
	jne	.LBB16_195
# BB#196:                               # %if.else.i570.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rdi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r9
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$1, %rax
	testq	%r13, %r13
	movq	%r9, %rcx
	movq	%r12, %rdx
	movq	%r14, %r11
	jle	.LBB16_198
.LBB16_197:                             # %for.body.i576.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	andq	%r11, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%rbx, %rsi
	jne	.LBB16_197
	jmp	.LBB16_198
.LBB16_191:                             #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	jmp	.LBB16_199
.LBB16_147:                             #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rdx         # 8-byte Reload
	incq	%r8
	jmp	.LBB16_186
.LBB16_195:                             # %if.then1.i565.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rsi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%r9, %rdi
	callq	Perl_safesysrealloc
	movq	%rax, %r9
	movq	%r14, %r11
.LBB16_198:                             #   in Loop: Header=BB16_120 Depth=2
	movq	144(%rsp), %r10         # 8-byte Reload
	movq	168(%rsp), %r8          # 8-byte Reload
.LBB16_199:                             # %if.end250.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	$0, 8(%r9,%rax)
	movq	$0, (%r9,%rax)
	movzbl	(%r8), %eax
	movzbl	hctype(%rax), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB16_189
.LBB16_148:                             # %while.cond184.preheader.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	160(%rsp), %rdi         # 8-byte Reload
	cmpq	%rdi, %r14
	movq	%r14, %r8
	jae	.LBB16_175
# BB#149:                               # %while.cond184.preheader.i
                                        #   in Loop: Header=BB16_120 Depth=2
	andq	$8, %rsi
	testq	$255, %rsi
	movq	%r14, %r8
	je	.LBB16_175
# BB#150:                               # %while.body194.i.lr.ph
                                        #   in Loop: Header=BB16_120 Depth=2
	movzbl	140(%r10), %ecx
.LBB16_172:                             # %while.body194.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %r8
	testq	$255, %rcx
	je	.LBB16_170
# BB#173:                               # %while.body194.i
                                        #   in Loop: Header=BB16_172 Depth=3
	andq	$255, %rax
	cmpq	$47, %rax
	je	.LBB16_174
.LBB16_170:                             # %while.cond184.i
                                        #   in Loop: Header=BB16_172 Depth=3
	cmpq	%rdi, %r8
	jae	.LBB16_175
# BB#171:                               # %while.cond184.land.rhs187_crit_edge.i
                                        #   in Loop: Header=BB16_172 Depth=3
	movzbl	(%r8), %eax
	movzbl	hctype(%rax), %esi
	leaq	1(%r8), %rdx
	testq	$8, %rsi
	jne	.LBB16_172
	jmp	.LBB16_175
.LBB16_164:                             # %if.then1.i523.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rsi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%r9, %rdi
	callq	Perl_safesysrealloc
	movq	%rax, %r9
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %r11
.LBB16_168:                             # %if.end172.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	144(%rsp), %r10         # 8-byte Reload
	movq	168(%rsp), %r8          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	incq	%r8
	jmp	.LBB16_186
.LBB16_174:                             # %while.body194.i.while.end204.i.loopexit_crit_edge
                                        #   in Loop: Header=BB16_120 Depth=2
	decq	%r8
.LBB16_175:                             # %while.end204.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	%rdi, %r8
	je	.LBB16_222
# BB#176:                               # %if.end208.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	addq	$2, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	andq	%r11, %rax
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rbx
	andq	%r11, %rbx
	cmpq	%rbx, %rax
	jne	.LBB16_177
# BB#178:                               # %if.then212.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, %rax
	leaq	192(%rsp), %r12
	jl	.LBB16_180
# BB#179:                               # %if.then212.i
                                        #   in Loop: Header=BB16_120 Depth=2
	leaq	(%rdx,%rdx), %rax
.LBB16_180:                             # %if.then212.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, %r13
	movq	%r8, 168(%rsp)          # 8-byte Spill
	movq	%r11, %r14
	movq	$32, %rcx
	cmpq	%r12, %r9
	jne	.LBB16_181
# BB#182:                               # %if.else.i549.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rdi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r9
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$1, %rax
	testq	%r13, %r13
	movq	%r9, %rcx
	movq	%r12, %rdx
	movq	%r14, %r11
	jle	.LBB16_184
.LBB16_183:                             # %for.body.i555.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	andq	%r11, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%rbx, %rsi
	jne	.LBB16_183
.LBB16_184:                             #   in Loop: Header=BB16_120 Depth=2
	movq	160(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB16_185
.LBB16_177:                             #   in Loop: Header=BB16_120 Depth=2
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB16_186
.LBB16_181:                             # %if.then1.i544.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	%rax, %rsi
	movq	%rax, 136(%rsp)         # 8-byte Spill
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	%r9, %rdi
	callq	Perl_safesysrealloc
	movq	%rax, %r9
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %r11
.LBB16_185:                             # %if.end217.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	144(%rsp), %r10         # 8-byte Reload
	movq	168(%rsp), %r8          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
.LBB16_186:                             # %if.end217.i
                                        #   in Loop: Header=BB16_120 Depth=2
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r14, (%r9,%rax)
	movq	%r8, 8(%r9,%rax)
	movq	48(%rsp), %rcx          # 8-byte Reload
	decq	%r8
	.align	16, 0x90
.LBB16_187:                             # %while.cond228.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_120 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	1(%r8), %eax
	movzbl	hctype(%rax), %eax
	incq	%r8
	testq	$1, %rax
	jne	.LBB16_187
# BB#188:                               # %while.end236.i
                                        #   in Loop: Header=BB16_120 Depth=2
	cmpq	%r8, %rdx
	je	.LBB16_222
.LBB16_189:                             # %while.cond36.backedge.i
                                        #   in Loop: Header=BB16_120 Depth=2
	andq	%rcx, %rax
	testq	$255, %rax
	movq	%r8, %r13
	jne	.LBB16_120
	jmp	.LBB16_159
.LBB16_292:                             # %if.then289
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	2(%r15), %eax
	leaq	2(%r15), %r8
	movq	%r8, 152(%rsp)          # 8-byte Spill
	cmpq	$91, %rax
	je	.LBB16_348
# BB#293:                               # %if.then289
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %r12
	cmpq	$62, %rax
	jne	.LBB16_294
# BB#389:                               # %if.then303.thread
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, 624(%rsp)
	movq	%r8, 632(%rsp)
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	leaq	3(%r15), %rbx
	movq	$1, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	leaq	624(%rsp), %r9
	movq	%r10, %r15
	callq	report_event
	movq	%rbx, %rcx
	movq	%r15, %r10
                                        # kill: RBX<def> RCX<kill>
	movq	%r12, %r11
	movabsq	$1, %rsi
	jmp	.LBB16_529
.LBB16_226:                             # %if.else279
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$47, %rax
	jne	.LBB16_227
# BB#228:                               # %if.then283
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	138(%r10), %eax
	leaq	2(%r15), %r8
	testq	%rax, %rax
	jne	.LBB16_229
# BB#230:                               # %lor.lhs.false.i485
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	140(%r10), %eax
	testq	%rax, %rax
	movq	%r13, %rax
	jne	.LBB16_232
# BB#231:                               # %lor.lhs.false.i485
                                        #   in Loop: Header=BB16_3 Depth=1
	movabsq	$1, %rax
.LBB16_232:                             # %lor.lhs.false.i485
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$8, %rsi
	je	.LBB16_234
# BB#233:                               # %lor.lhs.false.i485
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$2, %rsi
.LBB16_234:                             # %lor.lhs.false.i485
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	4(,%rax,4), %rcx
	jmp	.LBB16_235
.LBB16_227:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
	jmp	.LBB16_529
.LBB16_348:                             # %if.then17.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	136(%r10), %eax
	leaq	3(%r15), %r9
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	testq	%rax, %rax
	movq	%r9, %r14
	je	.LBB16_492
	jmp	.LBB16_349
.LBB16_294:                             # %if.then289
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$45, %rax
	movq	%r12, %r11
	jne	.LBB16_390
# BB#295:                               # %if.then.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	3(%r15), %r9
	cmpq	%r14, %r9
	movq	%r15, %rdx
	je	.LBB16_526
# BB#296:                               # %if.end.i502
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	(%r9), %eax
	cmpq	$45, %rax
	movabsq	$1, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	144(%rsp), %r10         # 8-byte Reload
	jne	.LBB16_492
# BB#297:                               # %if.end9.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	137(%r10), %eax
	leaq	4(%r15), %r13
	testq	%rax, %rax
	jne	.LBB16_298
# BB#325:                               # %if.else63.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	57(%r10), %eax
	testq	%rax, %rax
	jne	.LBB16_326
# BB#331:                               # %if.else86.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, 640(%rsp)
	movq	160(%rsp), %r14         # 8-byte Reload
	cmpq	%r14, %r13
	movq	%r13, %r12
	jae	.LBB16_332
.LBB16_333:                             # %land.rhs93.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_338 Depth 3
	movzbl	(%r12), %eax
	cmpq	$45, %rax
	jne	.LBB16_334
# BB#336:                               # %if.then104.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	movq	%r12, 648(%rsp)
	movzbl	1(%r12), %ecx
	leaq	1(%r12), %rax
	cmpq	$45, %rcx
	movq	%rax, %rcx
	jne	.LBB16_341
# BB#337:                               # %if.then109.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	addq	$2, %r12
.LBB16_338:                             # %while.cond111.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_333 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %ecx
	movzbl	hctype(%rcx), %edx
	incq	%r12
	testq	$1, %rdx
	jne	.LBB16_338
# BB#339:                               # %while.end118.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	cmpq	$62, %rcx
	je	.LBB16_344
# BB#340:                               #   in Loop: Header=BB16_333 Depth=2
	decq	%r12
	movq	%r12, %rcx
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB16_341:                             # %if.end126.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	cmpq	%r14, %rcx
	jb	.LBB16_335
	jmp	.LBB16_342
.LBB16_334:                             # %while.body98.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	incq	%r12
	movq	%r12, %rax
.LBB16_335:                             # %while.cond90.backedge.i.i
                                        #   in Loop: Header=BB16_333 Depth=2
	cmpq	%r14, %rax
	movq	%rax, %r12
	jb	.LBB16_333
.LBB16_332:                             # %while.end100.thread220.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, 648(%rsp)
	movq	%r12, %rcx
.LBB16_342:                             # %if.end133.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	cmpq	%r14, %rcx
	movq	%r13, %r12
	je	.LBB16_346
# BB#343:                               # %if.end133.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	xorq	%r12, %r12
	jmp	.LBB16_346
.LBB16_229:                             #   in Loop: Header=BB16_3 Depth=1
	movq	$2, %rsi
	movq	$4, %rcx
.LBB16_235:                             # %if.end.i489
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	(%r8), %eax
	movzbl	hctype(%rax), %edx
	testq	%rsi, %rdx
	jne	.LBB16_236
# BB#266:                               # %if.else41.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	137(%r10), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_267
# BB#269:                               # %if.then43.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r14, %r8
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#270:                               # %while.body.i99.i.preheader
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$62, %rax
	movq	%r8, %rcx
	je	.LBB16_290
# BB#271:                               # %if.end.i100.i.preheader
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r14, %r10
	leaq	3(%r15), %r13
	movq	$32, %rsi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movabsq	$0, %r12
.LBB16_272:                             # %if.end.i100.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$34, %rdx
	je	.LBB16_274
# BB#273:                               # %if.end.i100.i
                                        #   in Loop: Header=BB16_272 Depth=2
	cmpq	$39, %rdx
	jne	.LBB16_282
.LBB16_274:                             # %if.then9.i104.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movq	%r9, %rbx
	andq	$255, %rbx
	xorq	%rdi, %rdi
	cmpq	%rbx, %rdx
	movq	%r12, %rbx
	jne	.LBB16_276
# BB#275:                               # %if.then9.i104.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movabsq	$1, %rbx
.LBB16_276:                             # %if.then9.i104.i
                                        #   in Loop: Header=BB16_272 Depth=2
	je	.LBB16_278
# BB#277:                               # %if.then9.i104.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movq	%r9, %rdi
.LBB16_278:                             # %if.then9.i104.i
                                        #   in Loop: Header=BB16_272 Depth=2
	orq	%rcx, %rbx
	testq	$1, %rbx
	movq	%rdi, %r9
	jne	.LBB16_282
# BB#279:                               # %land.lhs.true16.i105.i
                                        #   in Loop: Header=BB16_272 Depth=2
	andq	%r11, %rsi
	cmpq	$32, %rsi
	je	.LBB16_281
# BB#280:                               # %land.lhs.true16.i105.i
                                        #   in Loop: Header=BB16_272 Depth=2
	xorq	%r9, %r9
	cmpq	$61, %rsi
	jne	.LBB16_282
.LBB16_281:                             # %if.then24.i106.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movq	%rax, %r9
.LBB16_282:                             # %if.end27.i111.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movq	%r10, %r14
	cmpq	%r14, %r13
	jae	.LBB16_525
# BB#283:                               # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movq	%r14, %r10
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	%rax, %rsi
	movzbl	(%r13), %eax
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%r9, %rcx
	andq	$255, %rcx
	movq	%r12, %rbx
	jne	.LBB16_285
# BB#284:                               # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movabsq	$1, %rbx
.LBB16_285:                             # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	testq	%rcx, %rcx
	movq	%r12, %rcx
	je	.LBB16_287
# BB#286:                               # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	movabsq	$1, %rcx
.LBB16_287:                             # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	incq	%r13
	cmpq	$62, %rax
	jne	.LBB16_272
# BB#288:                               # %if.end27.i111.while.body.i99_crit_edge.i
                                        #   in Loop: Header=BB16_272 Depth=2
	testq	%rbx, %rbx
	je	.LBB16_272
# BB#289:                               # %skip_until_gt.exit113.i.loopexit
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, %rcx
	decq	%rcx
	movq	%r10, %r14
.LBB16_290:                             # %skip_until_gt.exit113.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r14, %rcx
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#291:                               # %if.then47.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, 448(%rsp)
	movq	%rcx, 456(%rsp)
	incq	%rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	movq	$1, (%rsp)
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%rcx, %r12
	leaq	448(%rsp), %r9
	jmp	.LBB16_265
.LBB16_236:                             # %if.then6.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, 464(%rsp)
	leaq	3(%r15), %r9
	xorq	%rax, %rax
	jmp	.LBB16_238
.LBB16_237:                             # %while.cond.i492
                                        #   in Loop: Header=BB16_238 Depth=2
	incq	%r9
.LBB16_238:                             # %while.cond.i492
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %r9
	jae	.LBB16_241
# BB#239:                               # %land.rhs.i493
                                        #   in Loop: Header=BB16_238 Depth=2
	movzbl	(%r9), %edx
	movzbl	hctype(%rdx), %edx
	testq	%rcx, %rdx
	jne	.LBB16_237
# BB#240:                               #   in Loop: Header=BB16_3 Depth=1
	movq	$1, %rax
.LBB16_241:                             # %while.end.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, 472(%rsp)
	movzbl	139(%r10), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_242
# BB#247:                               # %if.else28.i
                                        #   in Loop: Header=BB16_3 Depth=1
	testq	%rax, %rax
	movq	%r15, %rdx
	je	.LBB16_526
# BB#248:                               #   in Loop: Header=BB16_3 Depth=1
	movq	$32, %rcx
	xorq	%rdx, %rdx
	movabsq	$0, %rbx
	movabsq	$1, %r8
.LBB16_249:                             # %while.body.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r9), %eax
	testq	$255, %rdx
	movq	%rbx, %rdi
	jne	.LBB16_251
# BB#250:                               # %while.body.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	movq	%r8, %rdi
.LBB16_251:                             # %while.body.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	cmpq	$62, %rax
	jne	.LBB16_253
# BB#252:                               # %while.body.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	movq	$62, %rsi
	testq	%rdi, %rdi
	jne	.LBB16_244
.LBB16_253:                             # %if.end.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	cmpq	$34, %rax
	je	.LBB16_256
# BB#254:                               # %if.end.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	cmpq	$39, %rax
	jne	.LBB16_255
.LBB16_256:                             # %if.then9.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	movq	%rdx, %rdi
	andq	$255, %rdi
	xorq	%rsi, %rsi
	cmpq	%rdi, %rax
	je	.LBB16_258
# BB#257:                               # %if.then9.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	movq	%rdx, %rsi
.LBB16_258:                             # %if.then9.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	je	.LBB16_263
# BB#259:                               # %if.then9.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	testq	$255, %rdx
	jne	.LBB16_263
# BB#260:                               # %land.lhs.true16.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	andq	%r11, %rcx
	cmpq	$32, %rcx
	je	.LBB16_262
# BB#261:                               # %land.lhs.true16.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	xorq	%rsi, %rsi
	cmpq	$61, %rcx
	jne	.LBB16_263
.LBB16_262:                             # %if.then24.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	movq	%rax, %rsi
	jmp	.LBB16_263
.LBB16_255:                             #   in Loop: Header=BB16_249 Depth=2
	movq	%rdx, %rsi
.LBB16_263:                             # %if.end27.i.i
                                        #   in Loop: Header=BB16_249 Depth=2
	incq	%r9
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %r9
	movq	%rax, %rcx
	movq	%rsi, %rdx
	jb	.LBB16_249
	jmp	.LBB16_525
.LBB16_390:                             # %if.end39.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	movabsq	$1, %rsi
	jb	.LBB16_392
# BB#391:                               # %if.end39.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	-97(%rax), %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	movq	%r8, %r9
	ja	.LBB16_492
.LBB16_392:                             # %if.then53.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	3(%r15), %r12
	jmp	.LBB16_394
.LBB16_393:                             # %while.cond.i511
                                        #   in Loop: Header=BB16_394 Depth=2
	incq	%r12
.LBB16_394:                             # %while.cond.i511
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	160(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %r12
	jae	.LBB16_396
# BB#395:                               # %land.rhs.i515
                                        #   in Loop: Header=BB16_394 Depth=2
	movzbl	(%r12), %ecx
	movzbl	hctype(%rcx), %ecx
	testq	$4, %rcx
	jne	.LBB16_393
.LBB16_396:                             # %while.end.i520
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, %rcx
	subq	%r8, %rcx
	cmpq	$6, %rcx
	jne	.LBB16_397
# BB#409:                               # %land.lhs.true75.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, %r14
	movzbl	140(%r10), %r8d
	movq	$3, %rdx
	jmp	.LBB16_410
.LBB16_417:                             # %if.end31.i501.while.body.i486_crit_edge.i
                                        #   in Loop: Header=BB16_410 Depth=2
	movzbl	(%r15,%rdx), %eax
	incq	%rdx
.LBB16_410:                             # %while.body.i486.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	testq	$255, %r8
	jne	.LBB16_418
# BB#411:                               # %if.then.i495.i
                                        #   in Loop: Header=BB16_410 Depth=2
	addq	$-65, %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jae	.LBB16_413
# BB#412:                               #   in Loop: Header=BB16_410 Depth=2
	addq	$32, %rdi
.LBB16_413:                             # %if.then.i495.i
                                        #   in Loop: Header=BB16_410 Depth=2
	movsbq	.L.str96-3(%rdx), %rax
	andq	%r11, %rdi
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jae	.LBB16_415
# BB#414:                               #   in Loop: Header=BB16_410 Depth=2
	addq	$32, %rax
.LBB16_415:                             # %if.then.i495.i
                                        #   in Loop: Header=BB16_410 Depth=2
	andq	%r11, %rax
	cmpq	%rax, %rdi
	je	.LBB16_416
	jmp	.LBB16_419
.LBB16_418:                             # %if.else.i497.i
                                        #   in Loop: Header=BB16_410 Depth=2
	movzbl	.L.str96-3(%rdx), %ecx
	andq	$255, %rax
	cmpq	%rcx, %rax
	jne	.LBB16_419
.LBB16_416:                             # %if.end31.i501.i
                                        #   in Loop: Header=BB16_410 Depth=2
	cmpq	$8, %rdx
	jne	.LBB16_417
	jmp	.LBB16_420
.LBB16_267:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
	jmp	.LBB16_268
.LBB16_242:                             # %while.cond19.i.preheader
                                        #   in Loop: Header=BB16_3 Depth=1
	decq	%r9
.LBB16_243:                             # %while.cond19.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	1(%r9), %esi
	movzbl	hctype(%rsi), %eax
	incq	%r9
	testq	$1, %rax
	jne	.LBB16_243
.LBB16_244:                             # %if.end29.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r14, %r9
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#245:                               # %if.then32.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$62, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB16_246
# BB#264:                               # %if.then36.i
                                        #   in Loop: Header=BB16_3 Depth=1
	incq	%r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$3, %rsi
	movq	$1, (%rsp)
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r9, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r9, %r12
	leaq	464(%rsp), %r9
	jmp	.LBB16_265
.LBB16_151:                             # %if.then117.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	addq	$2, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	andq	%r11, %rax
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %r14
	andq	%r11, %r14
	cmpq	%r14, %rax
	jne	.LBB16_158
# BB#152:                               # %if.then121.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, %rax
	jl	.LBB16_154
# BB#153:                               # %if.then121.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	(%rdx,%rdx), %rax
.LBB16_154:                             # %if.then121.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rdx, %rbx
	movq	%r8, 168(%rsp)          # 8-byte Spill
	leaq	192(%rsp), %r13
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	cmpq	%r13, %r9
	jne	.LBB16_155
# BB#156:                               # %if.else.i507.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rax, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r9
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rax
	testq	%rbx, %rbx
	movq	%r9, %rcx
	movq	%r13, %rdx
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	168(%rsp), %r8          # 8-byte Reload
	jle	.LBB16_158
.LBB16_157:                             # %for.body.i513.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	andq	%r11, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%r14, %rsi
	jne	.LBB16_157
	jmp	.LBB16_158
.LBB16_519:                             # %if.end.i567
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %rbx
	decq	%rax
	movq	%rax, 744(%rsp)
.LBB16_520:                             # %if.end14.i568
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$5, %rsi
	movq	$1, (%rsp)
	movq	%r15, %rdx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%rcx, %r12
	leaq	736(%rsp), %r9
	callq	report_event
	movq	%r12, %rcx
	movq	%rbx, %r11
	jmp	.LBB16_521
.LBB16_380:                             # %if.then69.i.i
                                        #   in Loop: Header=BB16_349 Depth=2
	addq	$2, %rax
	movq	%rax, %r14
	movq	144(%rsp), %r10         # 8-byte Reload
.LBB16_349:                             # %while.cond.outer.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_350 Depth 3
                                        #       Child Loop BB16_352 Depth 3
                                        #         Child Loop BB16_353 Depth 4
                                        #         Child Loop BB16_355 Depth 4
                                        #       Child Loop BB16_375 Depth 3
	decq	%r14
.LBB16_350:                             # %while.cond.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_349 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	1(%r14), %eax
	movzbl	hctype(%rax), %ecx
	incq	%r14
	testq	$1, %rcx
	jne	.LBB16_350
# BB#351:                               # %while.cond2.preheader.i.i
                                        #   in Loop: Header=BB16_349 Depth=2
	testq	$2, %rcx
	je	.LBB16_371
	jmp	.LBB16_352
.LBB16_367:                             # %for.body.i.i469.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB16_369
# BB#368:                               #   in Loop: Header=BB16_352 Depth=3
	addq	$32, %rcx
.LBB16_369:                             # %for.body.i.i469.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movb	%cl, (%rax)
	incq	%rax
	jmp	.LBB16_365
.LBB16_352:                             # %while.cond10.preheader.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_349 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB16_353 Depth 4
                                        #         Child Loop BB16_355 Depth 4
	leaq	1(%r14), %r12
.LBB16_353:                             # %while.cond10.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_349 Depth=2
                                        #       Parent Loop BB16_352 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r12, %rax
	movzbl	(%rax), %ecx
	movzbl	hctype(%rcx), %ecx
	leaq	1(%rax), %r12
	testq	$4, %rcx
	jne	.LBB16_353
# BB#354:                               # %while.cond19.i.preheader.i
                                        #   in Loop: Header=BB16_352 Depth=3
	decq	%r12
	testq	$1, %rcx
	movq	%r12, %rbx
	je	.LBB16_356
.LBB16_355:                             # %while.cond19.while.cond19_crit_edge.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_349 Depth=2
                                        #       Parent Loop BB16_352 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	1(%rax), %ecx
	movzbl	hctype(%rcx), %ecx
	incq	%rax
	testq	$1, %rcx
	movq	%rax, %rbx
	jne	.LBB16_355
.LBB16_356:                             # %while.end27.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rbx
	je	.LBB16_357
# BB#358:                               # %if.end30.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	%r9, %r13
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	jne	.LBB16_360
# BB#359:                               # %if.then32.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	callq	Perl_newAV
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB16_360:                             # %if.end33.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	subq	%r14, %r12
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r14
	movq	16(%r14), %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB16_362
# BB#361:                               # %if.then36.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	orq	$536870912, %rax        # imm = 0x20000000
	movq	%rax, 16(%r14)
.LBB16_362:                             # %if.end37.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB16_364
# BB#363:                               # %cond.true.i.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 656(%rsp)
	movq	(%rax), %rax
	jmp	.LBB16_366
.LBB16_364:                             # %cond.false.i.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	$2, %rdx
	movq	%r14, %rdi
	leaq	656(%rsp), %rsi
	callq	Perl_sv_pvn_force_flags
.LBB16_365:                             # %for.condthread-pre-split.i.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	656(%rsp), %rcx
.LBB16_366:                             # %for.cond.i.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 656(%rsp)
	jne	.LBB16_367
# BB#370:                               # %sv_lower.exit.i.i
                                        #   in Loop: Header=BB16_352 Depth=3
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	Perl_av_push
	movzbl	(%rbx), %eax
	movzbl	hctype(%rax), %ecx
	testq	$2, %rcx
	movq	%rbx, %r14
	movq	144(%rsp), %r10         # 8-byte Reload
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$1, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%r13, %r9
	jne	.LBB16_352
.LBB16_371:                             # %while.end39.i.i
                                        #   in Loop: Header=BB16_349 Depth=2
	cmpq	$91, %rax
	je	.LBB16_381
# BB#372:                               # %while.end39.i.i
                                        #   in Loop: Header=BB16_349 Depth=2
	movq	%r9, %r13
	movq	%rsi, %r12
	movq	%r11, %rbx
	cmpq	$45, %rax
	jne	.LBB16_491
# BB#373:                               # %if.then43.i472.i
                                        #   in Loop: Header=BB16_349 Depth=2
	movzbl	1(%r14), %eax
	cmpq	$45, %rax
	jne	.LBB16_491
# BB#374:                               # %if.then48.i.i
                                        #   in Loop: Header=BB16_349 Depth=2
	addq	$2, %r14
	movq	%rbx, %r11
	movq	%r12, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%r13, %r9
	jmp	.LBB16_375
.LBB16_377:                             # %while.body58.i.i
                                        #   in Loop: Header=BB16_375 Depth=3
	incq	%rax
	movq	%rax, %r14
.LBB16_375:                             # %while.cond52.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_349 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r14, %rax
	movq	160(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, %rax
	jae	.LBB16_378
# BB#376:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB16_375 Depth=3
	movzbl	(%rax), %ecx
	cmpq	$45, %rcx
	jne	.LBB16_377
.LBB16_378:                             # %while.end60.i.i
                                        #   in Loop: Header=BB16_375 Depth=3
	cmpq	%rdx, %rax
	je	.LBB16_357
# BB#379:                               # %if.end64.i.i
                                        #   in Loop: Header=BB16_375 Depth=3
	movzbl	1(%rax), %ecx
	leaq	1(%rax), %r14
	cmpq	$45, %rcx
	jne	.LBB16_375
	jmp	.LBB16_380
.LBB16_357:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, %rbx
	movq	%r11, %r13
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	%rbx, %r14
.LBB16_386:                             # %if.end23.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%rbx, %r14
	movq	%r15, %rdx
	movq	%r13, %r11
	je	.LBB16_388
# BB#387:                               # %if.end23.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r14, %rdx
.LBB16_388:                             # %if.end23.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
	jmp	.LBB16_526
.LBB16_397:                             # %while.end.i520
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	$7, %rcx
	jne	.LBB16_398
# BB#399:                               # %land.lhs.true68.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, %r14
	movzbl	140(%r10), %r8d
	movq	$-6, %rdx
	jmp	.LBB16_400
.LBB16_408:                             # %if.end31.i.while.body.i_crit_edge.i
                                        #   in Loop: Header=BB16_400 Depth=2
	movzbl	9(%r15,%rdx), %eax
	incq	%rdx
.LBB16_400:                             # %while.body.i.i522
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$56, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	testq	$255, %r8
	jne	.LBB16_406
# BB#401:                               # %if.then.i476.i
                                        #   in Loop: Header=BB16_400 Depth=2
	addq	$-65, %rax
	andq	$255, %rax
	cmpq	$26, %rax
	jae	.LBB16_403
# BB#402:                               #   in Loop: Header=BB16_400 Depth=2
	addq	$32, %rdi
.LBB16_403:                             # %if.then.i476.i
                                        #   in Loop: Header=BB16_400 Depth=2
	movsbq	.L.str95+6(%rdx), %rax
	andq	%r11, %rdi
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jae	.LBB16_405
# BB#404:                               #   in Loop: Header=BB16_400 Depth=2
	addq	$32, %rax
.LBB16_405:                             # %if.then.i476.i
                                        #   in Loop: Header=BB16_400 Depth=2
	andq	%r11, %rax
	cmpq	%rax, %rdi
	je	.LBB16_407
	jmp	.LBB16_419
.LBB16_406:                             # %if.else.i.i
                                        #   in Loop: Header=BB16_400 Depth=2
	movzbl	.L.str95+6(%rdx), %ecx
	andq	$255, %rax
	cmpq	%rcx, %rax
	jne	.LBB16_419
.LBB16_407:                             # %if.end31.i.i
                                        #   in Loop: Header=BB16_400 Depth=2
	testq	%rdx, %rdx
	jne	.LBB16_408
.LBB16_420:                             # %if.end91.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r14, %r8
	movq	%r8, 496(%rsp)
	movq	%r12, 504(%rsp)
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r12
	movq	$8, %r14
	movq	$1, %r13
	leaq	496(%rsp), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$1, %rdx
	movq	%r12, %r9
	jae	.LBB16_442
.LBB16_421:                             # %land.rhs103.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_424 Depth 3
                                        #       Child Loop BB16_472 Depth 3
                                        #       Child Loop BB16_482 Depth 3
                                        #       Child Loop BB16_437 Depth 3
                                        #       Child Loop BB16_469 Depth 3
                                        #       Child Loop BB16_446 Depth 3
                                        #       Child Loop BB16_456 Depth 3
	movzbl	(%r12), %eax
	movzbl	hctype(%rax), %eax
	testq	$1, %rax
	je	.LBB16_422
# BB#423:                               # %while.cond112.i.preheader
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	2(%r12), %rdx
.LBB16_424:                             # %while.cond112.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rax
	movq	%rdx, %rcx
	leaq	1(%rax), %r12
	movq	160(%rsp), %rdx         # 8-byte Reload
	cmpq	%rdx, %r12
	jae	.LBB16_426
# BB#425:                               # %land.rhs115.i
                                        #   in Loop: Header=BB16_424 Depth=3
	movzbl	(%r12), %edx
	movzbl	hctype(%rdx), %edi
	leaq	1(%rcx), %rdx
	testq	$1, %rdi
	jne	.LBB16_424
.LBB16_426:                             # %while.end124.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	je	.LBB16_427
# BB#430:                               # %if.end128.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movsbq	(%r12), %rdx
	cmpq	$61, %rdx
	movq	%r14, %rbx
	jg	.LBB16_440
# BB#431:                               # %if.end128.i
                                        #   in Loop: Header=BB16_421 Depth=2
	andq	$255, %rdx
	cmpq	$34, %rdx
	je	.LBB16_446
# BB#432:                               # %if.end128.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	$39, %rdx
	jne	.LBB16_433
.LBB16_446:                             # %while.cond139.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %r9
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r9
	jae	.LBB16_448
# BB#447:                               # %land.rhs142.i
                                        #   in Loop: Header=BB16_446 Depth=3
	movzbl	(%r9), %eax
	leaq	1(%r9), %rcx
	cmpq	%rdx, %rax
	jne	.LBB16_446
.LBB16_448:                             # %while.end150.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	%r9, 160(%rsp)          # 8-byte Folded Reload
	je	.LBB16_427
# BB#449:                               # %if.end154.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	1(%r13), %rdx
	movq	%rdx, %rax
	andq	%r11, %rax
	movq	%rbx, %r14
	andq	%r11, %r14
	cmpq	%r14, %rax
	jne	.LBB16_450
# BB#451:                               # %if.then159.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, 72(%rsp)            # 8-byte Folded Spill
	jl	.LBB16_453
# BB#452:                               # %if.then159.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	(%rbx,%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB16_453:                             # %if.then159.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%r9, 168(%rsp)          # 8-byte Spill
	leaq	496(%rsp), %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	$32, %rcx
	cmpq	%rax, %rdi
	jne	.LBB16_454
# BB#455:                               # %if.else.i524.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	72(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rsi
	testq	%rbx, %rbx
	movq	%rax, %rcx
	movq	%rax, %r10
	leaq	496(%rsp), %rdx
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	jle	.LBB16_457
.LBB16_456:                             # %for.body.i530.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rax
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rax, (%rcx)
	movq	%rsi, %rax
	andq	%r11, %rax
	incq	%rsi
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%r14, %rax
	jne	.LBB16_456
.LBB16_457:                             #   in Loop: Header=BB16_421 Depth=2
	movabsq	$1, %rsi
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	%r10, %rax
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %r14          # 8-byte Reload
	jmp	.LBB16_458
.LBB16_440:                             # %if.end128.i
                                        #   in Loop: Header=BB16_421 Depth=2
	andq	$255, %rdx
	cmpq	$62, %rdx
	jne	.LBB16_472
	jmp	.LBB16_441
.LBB16_433:                             # %if.end128.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	$45, %rdx
	jne	.LBB16_472
# BB#434:                               # %if.then177.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	%rax, 80(%rsp)          # 8-byte Folded Reload
	je	.LBB16_427
# BB#435:                               # %if.end183.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	%r8, %r14
	movzbl	2(%rax), %ecx
	cmpq	$45, %rcx
	jne	.LBB16_487
# BB#436:                               # %if.end188.i
                                        #   in Loop: Header=BB16_421 Depth=2
	addq	$3, %rax
	jmp	.LBB16_437
.LBB16_472:                             # %while.cond244.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %r9
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r9
	jae	.LBB16_474
# BB#473:                               # %land.rhs247.i
                                        #   in Loop: Header=BB16_472 Depth=3
	movzbl	(%r9), %eax
	movzbl	hctype(%rax), %eax
	leaq	1(%r9), %rcx
	testq	$8, %rax
	jne	.LBB16_472
.LBB16_474:                             # %while.end256.i
                                        #   in Loop: Header=BB16_421 Depth=2
	cmpq	%r9, 160(%rsp)          # 8-byte Folded Reload
	je	.LBB16_427
# BB#475:                               # %if.end260.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	1(%r13), %rdx
	movq	%rdx, %rax
	andq	%r11, %rax
	movq	%rbx, %r14
	andq	%r11, %r14
	cmpq	%r14, %rax
	jne	.LBB16_476
# BB#477:                               # %if.then264.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, 72(%rsp)            # 8-byte Folded Spill
	jl	.LBB16_479
# BB#478:                               # %if.then264.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	(%rbx,%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB16_479:                             # %if.then264.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%r9, 168(%rsp)          # 8-byte Spill
	leaq	496(%rsp), %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	$32, %rcx
	cmpq	%rax, %rdi
	jne	.LBB16_480
# BB#481:                               # %if.else.i566.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	72(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	callq	Perl_safesysmalloc
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$1, %rsi
	testq	%rbx, %rbx
	movq	%rax, %rcx
	movq	%rax, %r10
	leaq	496(%rsp), %rdx
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	jle	.LBB16_483
.LBB16_482:                             # %for.body.i572.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rax
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rax, (%rcx)
	movq	%rsi, %rax
	andq	%r11, %rax
	incq	%rsi
	addq	$16, %rdx
	addq	$16, %rcx
	cmpq	%r14, %rax
	jne	.LBB16_482
.LBB16_483:                             #   in Loop: Header=BB16_421 Depth=2
	movabsq	$1, %rsi
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	%r10, %rax
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %r14          # 8-byte Reload
	jmp	.LBB16_484
.LBB16_476:                             #   in Loop: Header=BB16_421 Depth=2
	movq	%rbx, %r14
	movq	136(%rsp), %rax         # 8-byte Reload
	jmp	.LBB16_484
.LBB16_450:                             #   in Loop: Header=BB16_421 Depth=2
	movq	%rbx, %r14
	movq	136(%rsp), %rax         # 8-byte Reload
	jmp	.LBB16_458
.LBB16_454:                             # %if.then1.i519.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	Perl_safesysrealloc
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$1, %rsi
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
.LBB16_458:                             # %if.end164.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	%rax, 136(%rsp)         # 8-byte Spill
	incq	%r9
	jmp	.LBB16_485
.LBB16_439:                             # %while.body200.i
                                        #   in Loop: Header=BB16_437 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	incq	%rax
.LBB16_437:                             # %while.cond192.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rax, %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %rcx
	jae	.LBB16_459
# BB#438:                               # %land.rhs195.i
                                        #   in Loop: Header=BB16_437 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	(%rax), %eax
	cmpq	$45, %rax
	jne	.LBB16_439
.LBB16_459:                             # %while.end202.i
                                        #   in Loop: Header=BB16_437 Depth=3
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, 168(%rsp)         # 8-byte Folded Reload
	je	.LBB16_427
# BB#460:                               # %if.end206.i
                                        #   in Loop: Header=BB16_437 Depth=3
	movq	168(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB16_427
# BB#461:                               # %if.end211.i
                                        #   in Loop: Header=BB16_437 Depth=3
	movzbl	(%rax), %ecx
	cmpq	$45, %rcx
	jne	.LBB16_437
# BB#462:                               # %if.then215.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	1(%r13), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r11, %rcx
	andq	%rcx, %rax
	movq	%rbx, %rdx
	andq	%rcx, %rdx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	cmpq	%rdx, %rax
	jne	.LBB16_463
# BB#464:                               # %if.then220.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, 72(%rsp)            # 8-byte Folded Spill
	jl	.LBB16_466
# BB#465:                               # %if.then220.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	(%rbx,%rbx), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB16_466:                             # %if.then220.i
                                        #   in Loop: Header=BB16_421 Depth=2
	leaq	496(%rsp), %rax
	cmpq	%rax, 136(%rsp)         # 8-byte Folded Reload
	jne	.LBB16_467
# BB#468:                               # %if.else.i545.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rdx
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rax
	testq	%rsi, %rsi
	movq	%rdx, %rcx
	leaq	496(%rsp), %rdx
	jle	.LBB16_470
.LBB16_469:                             # %for.body.i551.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_421 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %rsi
	jne	.LBB16_469
	jmp	.LBB16_470
.LBB16_480:                             # %if.then1.i561.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	callq	Perl_safesysrealloc
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$1, %rsi
	movq	168(%rsp), %r9          # 8-byte Reload
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
.LBB16_484:                             # %if.end269.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB16_485:                             # %while.cond100.backedge.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	%r12, (%rax,%r13)
	movq	%r9, 8(%rax,%r13)
	movq	144(%rsp), %r10         # 8-byte Reload
	jmp	.LBB16_471
.LBB16_463:                             #   in Loop: Header=BB16_421 Depth=2
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	jmp	.LBB16_470
.LBB16_467:                             # %if.then1.i540.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	$32, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB16_470:                             # %if.end225.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	168(%rsp), %r9          # 8-byte Reload
	addq	$2, %r9
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	shlq	%cl, %r13
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%r12, (%rax,%r13)
	movq	%r9, 8(%rax,%r13)
	movq	144(%rsp), %r10         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r11
	movabsq	$1, %rax
	movq	%rax, %rsi
	movq	%r14, %r8
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	72(%rsp), %r14          # 8-byte Reload
.LBB16_471:                             # %while.cond100.backedge.i
                                        #   in Loop: Header=BB16_421 Depth=2
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r9
	movq	%r9, %r12
	movq	%rdx, %r13
	jb	.LBB16_421
	jmp	.LBB16_442
.LBB16_246:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
	movabsq	$0, %r13
	movabsq	$1, %rsi
	jmp	.LBB16_528
.LBB16_155:                             # %if.then1.i502.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, %rdi
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, %r9
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	168(%rsp), %r8          # 8-byte Reload
.LBB16_158:                             # %if.end126.i
                                        #   in Loop: Header=BB16_3 Depth=1
	addq	%r12, %r8
	movq	$32, %rcx
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r8, (%r9,%rax)
	movq	%r8, 8(%r9,%rax)
	movq	144(%rsp), %r10         # 8-byte Reload
.LBB16_159:                             # %while.end261.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	140(%r10), %eax
	xorq	%r14, %r14
	testq	%rax, %rax
	jne	.LBB16_200
# BB#160:                               #   in Loop: Header=BB16_3 Depth=1
	movabsq	$0, %r13
	jmp	.LBB16_202
.LBB16_200:                             # %land.lhs.true265.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	(%r8), %eax
	cmpq	$47, %rax
	movabsq	$0, %r13
	jne	.LBB16_202
# BB#201:                               # %if.then269.i
                                        #   in Loop: Header=BB16_3 Depth=1
	incq	%r8
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r8
	movq	$1, %r14
	jne	.LBB16_202
.LBB16_222:                             # %PREMATURE.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	192(%rsp), %rax
	cmpq	%rax, %r9
	movq	%r15, %rdx
	movq	160(%rsp), %r14         # 8-byte Reload
	je	.LBB16_526
# BB#223:                               # %if.then358.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, %rdi
.LBB16_429:                             # %if.then307.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %rbx
	callq	Perl_safesysfree
	movq	%rbx, %r11
.LBB16_525:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rdx
.LBB16_526:                             # %if.end301
                                        #   in Loop: Header=BB16_3 Depth=1
	testq	%rdx, %rdx
	movq	%rdx, %rcx
	movabsq	$0, %r13
	movabsq	$1, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	jne	.LBB16_522
# BB#527:                               #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
.LBB16_528:                             # %while.cond.backedge
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	144(%rsp), %r10         # 8-byte Reload
	jmp	.LBB16_529
.LBB16_202:                             # %if.end275.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %r12
	movzbl	(%r8), %eax
	cmpq	$62, %rax
	jne	.LBB16_219
# BB#203:                               # %if.then279.i
                                        #   in Loop: Header=BB16_3 Depth=1
	incq	%r8
	movq	%r8, 168(%rsp)          # 8-byte Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$2, %rsi
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%r8, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r9, 136(%rsp)          # 8-byte Spill
	movq	%r10, %rbx
	callq	report_event
	testq	%r14, %r14
	je	.LBB16_205
# BB#204:                               # %if.then282.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$3, %rsi
	movq	$1, (%rsp)
	movq	%rbx, %rdi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rdx, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	136(%rsp), %r9          # 8-byte Reload
	callq	report_event
.LBB16_205:                             # %if.end283.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	140(%rbx), %eax
	testq	%rax, %rax
	movq	%r12, %r11
	movq	%rbx, %r10
	jne	.LBB16_216
# BB#206:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rax
	movq	8(%rcx), %r9
	subq	%rax, %r9
	movq	$6, %rax
	xorq	%rcx, %rcx
.LBB16_207:                             # %for.body.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_209 Depth 3
	movq	%r9, %rdx
	andq	%r11, %rdx
	andq	%r11, %rax
	cmpq	%rax, %rdx
	jne	.LBB16_215
# BB#208:                               # %if.then302.i
                                        #   in Loop: Header=BB16_207 Depth=2
	leaq	(%rcx,%rcx,2), %r8
	movq	literal_mode_elem+8(,%r8,8), %rsi
	xorq	%rdi, %rdi
	movq	%r9, %rax
.LBB16_209:                             # %while.cond311.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_207 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r11, %rax
	je	.LBB16_215
# BB#210:                               # %while.body313.i
                                        #   in Loop: Header=BB16_209 Depth=3
	movsbq	1(%r15,%rdi), %rdx
	leaq	-65(%rdx), %rbx
	andq	$255, %rbx
	cmpq	$26, %rbx
	jae	.LBB16_212
# BB#211:                               #   in Loop: Header=BB16_209 Depth=3
	addq	$32, %rdx
.LBB16_212:                             # %while.body313.i
                                        #   in Loop: Header=BB16_209 Depth=3
	movsbq	(%rsi,%rdi), %rbx
	andq	%r11, %rdx
	andq	%r11, %rbx
	cmpq	%rbx, %rdx
	jne	.LBB16_215
# BB#213:                               # %if.end327.i
                                        #   in Loop: Header=BB16_209 Depth=3
	decq	%rax
	incq	%rdi
	testq	%r11, %rax
	jne	.LBB16_209
	jmp	.LBB16_214
.LBB16_215:                             # %for.inc.i
                                        #   in Loop: Header=BB16_207 Depth=2
	incq	%rcx
	leaq	(%rcx,%rcx,2), %rax
	movl	literal_mode_elem(,%rax,8), %eax
	testq	%rax, %rax
	jne	.LBB16_207
	jmp	.LBB16_216
.LBB16_219:                             # %if.end349.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	192(%rsp), %rax
	cmpq	%rax, %r9
	je	.LBB16_220
# BB#221:                               # %if.then353.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, %rdi
	movq	%r10, %rbx
	callq	Perl_safesysfree
	movq	%rbx, %r10
.LBB16_220:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
	movq	160(%rsp), %r14         # 8-byte Reload
	movq	%r12, %r11
.LBB16_268:                             # %while.cond.backedge
                                        #   in Loop: Header=BB16_3 Depth=1
	movabsq	$1, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_529
.LBB16_214:                             # %if.then331.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	literal_mode_elem+16(,%r8,8), %eax
	movq	%rsi, 48(%r10)
	movb	%al, 56(%r10)
.LBB16_216:                             # %if.end343.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	192(%rsp), %rax
	movq	136(%rsp), %rdi         # 8-byte Reload
	cmpq	%rax, %rdi
	je	.LBB16_217
# BB#218:                               # %if.then347.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %rbx
	callq	Perl_safesysfree
	movq	%rbx, %r11
.LBB16_217:                             #   in Loop: Header=BB16_3 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
	movabsq	$1, %rsi
	movq	168(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB16_522
.LBB16_419:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, %r9
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	%r14, %r8
	jmp	.LBB16_492
.LBB16_298:                             #   in Loop: Header=BB16_3 Depth=1
	movq	$4, 152(%rsp)           # 8-byte Folded Spill
	xorq	%rcx, %rcx
	leaq	672(%rsp), %rax
	movq	%rax, %r9
	movq	%r13, %r12
	movq	%r13, %rax
	jmp	.LBB16_299
.LBB16_398:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, %r9
	jmp	.LBB16_492
.LBB16_326:                             # %if.then65.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, 656(%rsp)
	movq	160(%rsp), %r14         # 8-byte Reload
	cmpq	%r14, %r13
	movq	%r13, %r12
	jae	.LBB16_327
.LBB16_329:                             # %land.rhs71.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r12), %eax
	cmpq	$62, %rax
	je	.LBB16_330
# BB#328:                               # %while.cond68.i.i
                                        #   in Loop: Header=BB16_329 Depth=2
	incq	%r12
	cmpq	%r14, %r12
	jb	.LBB16_329
.LBB16_327:                             # %if.else85.critedge.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, 664(%rsp)
	movq	%r13, %r12
	jmp	.LBB16_346
.LBB16_491:                             # %FAIL.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	movq	%rbx, %r11
	movq	%r12, %rsi
	movq	96(%rsp), %rbx          # 8-byte Reload
	movq	144(%rsp), %r10         # 8-byte Reload
	movq	%r13, %r9
	movq	152(%rsp), %r8          # 8-byte Reload
	jmp	.LBB16_492
.LBB16_344:                             # %if.then122.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	movq	$1, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	leaq	640(%rsp), %rax
	movq	%rax, %r9
	jmp	.LBB16_345
.LBB16_381:                             # %if.then76.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, 168(%rsp)          # 8-byte Spill
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	%r11, %r13
	movq	%r10, %r12
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	jne	.LBB16_383
# BB#382:                               # %if.then79.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	callq	Perl_newAV
	movq	%rax, %rbx
	movq	%rbx, 136(%rsp)         # 8-byte Spill
	movq	$7, %rsi
	movabsq	$.L.str97, %rax
	movq	%rax, %rdi
	callq	Perl_newSVpvn
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
.LBB16_383:                             # %if.end82.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	128(%r12), %rbx
	incq	%r14
	testq	%rbx, %rbx
	jne	.LBB16_385
# BB#384:                               # %if.then84.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	callq	Perl_newAV
	movq	%rax, %rbx
	movq	%rbx, 128(%r12)
.LBB16_385:                             # %if.end87.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newRV_noinc
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	%r12, %rdi
	callq	marked_section_update
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$10, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r12, %rdi
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdx
	movq	%r14, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	report_event
	jmp	.LBB16_386
.LBB16_422:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, %rdx
	movq	%r12, %r9
	jmp	.LBB16_442
.LBB16_441:                             # %if.end128.i.while.end283.i.loopexit_crit_edge
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r13, %rdx
	movq	%r12, %r9
	movq	144(%rsp), %r10         # 8-byte Reload
.LBB16_442:                             # %while.end283.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
	cmpq	%r14, %r9
	je	.LBB16_428
# BB#443:                               # %if.end287.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	(%r9), %eax
	cmpq	$62, %rax
	jne	.LBB16_488
# BB#444:                               # %if.then291.i
                                        #   in Loop: Header=BB16_3 Depth=1
	incq	%r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movabsq	$0, %rsi
	movq	%rdx, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%r9, %rcx
	movq	%r9, %r12
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %r9
	callq	report_event
	leaq	496(%rsp), %rax
	cmpq	%rax, %rbx
	movq	%rbx, %rdi
	je	.LBB16_445
# BB#486:                               # %if.then296.i
                                        #   in Loop: Header=BB16_3 Depth=1
	callq	Perl_safesysfree
.LBB16_445:                             #   in Loop: Header=BB16_3 Depth=1
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movabsq	$0, %r13
	movabsq	$1, %rsi
	movq	%r12, %rcx
	jmp	.LBB16_522
.LBB16_487:                             # %FAIL.i.loopexit
                                        #   in Loop: Header=BB16_3 Depth=1
	addq	$2, %rax
	movq	%rax, %r9
	movq	144(%rsp), %r10         # 8-byte Reload
	movabsq	$1, %rax
	movq	%rax, %rsi
	movq	%r14, %r8
.LBB16_488:                             # %FAIL.i
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	496(%rsp), %rax
	movq	136(%rsp), %r13         # 8-byte Reload
	cmpq	%rax, %r13
	movq	%r13, %rdi
	jne	.LBB16_490
# BB#489:                               #   in Loop: Header=BB16_3 Depth=1
	movq	96(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB16_492
.LBB16_427:                             #   in Loop: Header=BB16_3 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
.LBB16_428:                             # %PREMATURE.i536
                                        #   in Loop: Header=BB16_3 Depth=1
	leaq	496(%rsp), %rax
	movq	136(%rsp), %r13         # 8-byte Reload
	cmpq	%rax, %r13
	movq	%r13, %rdi
	movq	%r15, %rdx
	je	.LBB16_526
	jmp	.LBB16_429
.LBB16_324:                             # %if.end54.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	$4, %rcx
	movq	168(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%r8, %r9
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, (%r9,%rax)
	movq	%r14, 8(%r9,%rax)
	xorq	%rax, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r11
	movq	72(%rsp), %rcx          # 8-byte Reload
.LBB16_299:                             # %while.cond1.outer.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB16_300 Depth 3
                                        #         Child Loop BB16_301 Depth 4
                                        #       Child Loop BB16_323 Depth 3
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	144(%rsp), %rdi         # 8-byte Reload
.LBB16_300:                             # %while.cond1.outer222.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_299 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB16_301 Depth 4
	movq	%rax, %rcx
	jmp	.LBB16_301
.LBB16_304:                             # %while.body7.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	incq	%r14
	movq	%r14, %r12
.LBB16_301:                             # %while.cond1.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_299 Depth=2
                                        #       Parent Loop BB16_300 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%r12, %r14
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	jae	.LBB16_305
# BB#302:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	movzbl	(%r14), %eax
	cmpq	$45, %rax
	je	.LBB16_305
# BB#303:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	cmpq	$62, %rax
	jne	.LBB16_304
.LBB16_305:                             # %while.end.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r14
	je	.LBB16_306
# BB#308:                               # %if.end15.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	movzbl	(%r14), %eax
	leaq	1(%r14), %r12
	cmpq	$62, %rax
	jne	.LBB16_313
# BB#309:                               # %if.then19.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	testq	%rcx, %rcx
	jne	.LBB16_301
	jmp	.LBB16_310
.LBB16_313:                             # %if.end29.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	movq	160(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, %r12
	je	.LBB16_306
# BB#314:                               # %if.end39.i.i
                                        #   in Loop: Header=BB16_301 Depth=4
	movzbl	(%r12), %eax
	cmpq	$45, %rax
	jne	.LBB16_301
# BB#315:                               # %if.then43.i.i
                                        #   in Loop: Header=BB16_300 Depth=3
	leaq	2(%r14), %r12
	testq	%rcx, %rcx
	movq	%r12, %rax
	je	.LBB16_300
# BB#316:                               # %if.then46.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	%r9, %r8
	movq	168(%rsp), %rax         # 8-byte Reload
	leaq	1(%rax), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	andq	%r11, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	andq	%r11, %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	cmpq	%rcx, %rax
	jne	.LBB16_317
# BB#318:                               # %if.then49.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	$32, %rcx
	movq	152(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$4, %rax
	movq	$8, 64(%rsp)            # 8-byte Folded Spill
	jl	.LBB16_320
# BB#319:                               # %if.then49.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%rax), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB16_320:                             # %if.then49.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	leaq	672(%rsp), %rax
	movq	$32, %rcx
	cmpq	%rax, %r8
	jne	.LBB16_321
# BB#322:                               # %if.else.i.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rax, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rdx
	movq	$32, %rcx
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %rax
	testq	%rsi, %rsi
	movq	%rdx, %rcx
	movq	%rdx, %r8
	leaq	672(%rsp), %rdx
	jle	.LBB16_324
.LBB16_323:                             # %for.body.i.i.i
                                        #   Parent Loop BB16_3 Depth=1
                                        #     Parent Loop BB16_299 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rsi
	movq	8(%rdx), %rdi
	movq	%rdi, 8(%rcx)
	movq	%rsi, (%rcx)
	movq	%rax, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	incq	%rax
	addq	$16, %rdx
	addq	$16, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %rsi
	jne	.LBB16_323
	jmp	.LBB16_324
.LBB16_317:                             #   in Loop: Header=BB16_299 Depth=2
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jmp	.LBB16_324
.LBB16_321:                             # %if.then1.i.i.i
                                        #   in Loop: Header=BB16_299 Depth=2
	movq	64(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%r8, %rdi
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, %r8
	jmp	.LBB16_324
.LBB16_306:                             # %if.then10.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	leaq	672(%rsp), %rax
	cmpq	%rax, %r9
	movq	%r13, %r12
	movq	160(%rsp), %r14         # 8-byte Reload
	je	.LBB16_346
# BB#307:                               # %if.then14.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r9, %rdi
	movq	%r11, %rbx
	callq	Perl_safesysfree
	movq	%rbx, %r11
	movq	%r13, %r12
	jmp	.LBB16_346
.LBB16_310:                             # %if.end23.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r9, %rbx
                                        # kill: R9<def> RBX<kill>
	callq	report_event
	movq	%rbx, %rdi
	leaq	672(%rsp), %rax
	cmpq	%rax, %rdi
	je	.LBB16_311
# BB#312:                               # %if.then27.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	callq	Perl_safesysfree
.LBB16_311:                             #   in Loop: Header=BB16_3 Depth=1
	movq	160(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	jmp	.LBB16_346
.LBB16_330:                             # %if.then82.i.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, 664(%rsp)
	incq	%r12
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	movq	$1, (%rsp)
	movq	%r10, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	leaq	656(%rsp), %r9
.LBB16_345:                             # %parse_comment.exit.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %rbx
	callq	report_event
	movq	%rbx, %r11
.LBB16_346:                             # %parse_comment.exit.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r13, %r12
	movq	%r15, %rdx
	je	.LBB16_526
# BB#347:                               # %parse_comment.exit.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r12, %rdx
	jmp	.LBB16_526
.LBB16_490:                             # %if.then302.i535
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %r14
	movq	%rsi, %rbx
	movq	%r10, %r12
	movq	%r9, %r13
	movq	%r8, 152(%rsp)          # 8-byte Spill
	callq	Perl_safesysfree
	movq	152(%rsp), %r8          # 8-byte Reload
	movq	%r13, %r9
	movq	%r12, %r10
	movq	%rbx, %rsi
	movq	%r14, %r11
	movq	96(%rsp), %rbx          # 8-byte Reload
.LBB16_492:                             # %DECL_FAIL.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	137(%r10), %eax
	testq	%rax, %rax
	jne	.LBB16_493
# BB#494:                               # %if.end312.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	160(%rsp), %r14         # 8-byte Reload
	cmpq	%r14, %r9
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#495:                               #   in Loop: Header=BB16_3 Depth=1
	movq	%r8, %r10
	movq	$32, %rcx
	xorq	%rdx, %rdx
	movabsq	$0, %rbx
	movabsq	$1, %r8
.LBB16_496:                             # %while.body.i576.i
                                        #   Parent Loop BB16_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r9), %eax
	testq	$255, %rdx
	movq	%rbx, %rsi
	jne	.LBB16_498
# BB#497:                               # %while.body.i576.i
                                        #   in Loop: Header=BB16_496 Depth=2
	movq	%r8, %rsi
.LBB16_498:                             # %while.body.i576.i
                                        #   in Loop: Header=BB16_496 Depth=2
	cmpq	$62, %rax
	jne	.LBB16_500
# BB#499:                               # %while.body.i576.i
                                        #   in Loop: Header=BB16_496 Depth=2
	testq	%rsi, %rsi
	jne	.LBB16_511
.LBB16_500:                             # %if.end.i.i546
                                        #   in Loop: Header=BB16_496 Depth=2
	cmpq	$34, %rax
	je	.LBB16_503
# BB#501:                               # %if.end.i.i546
                                        #   in Loop: Header=BB16_496 Depth=2
	cmpq	$39, %rax
	jne	.LBB16_502
.LBB16_503:                             # %if.then9.i.i549
                                        #   in Loop: Header=BB16_496 Depth=2
	movq	%rdx, %rdi
	andq	$255, %rdi
	xorq	%rsi, %rsi
	cmpq	%rdi, %rax
	je	.LBB16_505
# BB#504:                               # %if.then9.i.i549
                                        #   in Loop: Header=BB16_496 Depth=2
	movq	%rdx, %rsi
.LBB16_505:                             # %if.then9.i.i549
                                        #   in Loop: Header=BB16_496 Depth=2
	je	.LBB16_510
# BB#506:                               # %if.then9.i.i549
                                        #   in Loop: Header=BB16_496 Depth=2
	testq	$255, %rdx
	jne	.LBB16_510
# BB#507:                               # %land.lhs.true16.i.i550
                                        #   in Loop: Header=BB16_496 Depth=2
	andq	%r11, %rcx
	cmpq	$32, %rcx
	je	.LBB16_509
# BB#508:                               # %land.lhs.true16.i.i550
                                        #   in Loop: Header=BB16_496 Depth=2
	xorq	%rsi, %rsi
	cmpq	$61, %rcx
	jne	.LBB16_510
.LBB16_509:                             # %if.then24.i.i551
                                        #   in Loop: Header=BB16_496 Depth=2
	movq	%rax, %rsi
	jmp	.LBB16_510
.LBB16_502:                             #   in Loop: Header=BB16_496 Depth=2
	movq	%rdx, %rsi
.LBB16_510:                             # %if.end27.i.i554
                                        #   in Loop: Header=BB16_496 Depth=2
	incq	%r9
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	%r14, %r9
	movq	%rax, %rcx
	movq	%rsi, %rdx
	jb	.LBB16_496
	jmp	.LBB16_525
.LBB16_493:                             #   in Loop: Header=BB16_3 Depth=1
	movq	%r15, %rcx
	movq	160(%rsp), %r14         # 8-byte Reload
	movabsq	$0, %r13
	jmp	.LBB16_529
.LBB16_511:                             # %skip_until_gt.exit.i
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r14, %r9
	movq	%r15, %rdx
	jae	.LBB16_526
# BB#512:                               # %if.then316.i
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r10, 480(%rsp)
	movq	%r9, 488(%rsp)
	incq	%r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$1, %rsi
	movq	$1, (%rsp)
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r9, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	movq	%r9, %r12
	leaq	480(%rsp), %r9
.LBB16_265:                             # %if.then303
                                        #   in Loop: Header=BB16_3 Depth=1
	movq	%r11, %rbx
	callq	report_event
	movq	%r12, %rcx
	movq	%rbx, %r11
	movabsq	$0, %r13
.LBB16_521:                             # %if.then303
                                        #   in Loop: Header=BB16_3 Depth=1
	movabsq	$1, %rsi
.LBB16_522:                             # %if.then303
                                        #   in Loop: Header=BB16_3 Depth=1
	cmpq	%r15, %rcx
	movq	%rcx, %rbx
	movq	144(%rsp), %r10         # 8-byte Reload
	je	.LBB16_530
.LBB16_529:                             # %while.cond.backedge
                                        #   in Loop: Header=BB16_3 Depth=1
	movzbl	41(%r10), %eax
	testq	%rax, %rax
	movq	%rbx, %r12
	movq	%rbx, %r15
	je	.LBB16_3
	jmp	.LBB16_530
.LBB16_29:
	movq	%r8, %r15
	jmp	.LBB16_530
.LBB16_56:
	movq	%rbx, %r15
	jmp	.LBB16_530
.LBB16_84:                              # %if.else210
	movzbl	-1(%rbx), %eax
	movzbl	hctype(%rax), %eax
	leaq	-1(%rbx), %r14
	testq	$1, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	je	.LBB16_87
# BB#85:                                # %while.cond218.preheader
	cmpq	%r15, %r14
	jb	.LBB16_100
# BB#86:                                # %land.rhs221.preheader
	addq	$-2, %rbx
	movq	%rbx, %r14
	jmp	.LBB16_94
.LBB16_93:                              # %while.cond218.land.rhs221_crit_edge
                                        #   in Loop: Header=BB16_94 Depth=1
	movzbl	(%r14), %eax
	movzbl	hctype(%rax), %eax
	decq	%r14
.LBB16_94:                              # %land.rhs221
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rax
	je	.LBB16_95
# BB#92:                                # %while.cond218
                                        #   in Loop: Header=BB16_94 Depth=1
	cmpq	%r15, %r14
	jae	.LBB16_93
	jmp	.LBB16_100
.LBB16_87:                              # %while.cond232.preheader
	cmpq	%r15, %r14
	jb	.LBB16_98
# BB#88:                                # %land.rhs235.preheader
	addq	$-2, %rbx
	movq	%rbx, %r14
	jmp	.LBB16_89
.LBB16_91:                              # %while.cond232.land.rhs235_crit_edge
                                        #   in Loop: Header=BB16_89 Depth=1
	movzbl	(%r14), %eax
	movzbl	hctype(%rax), %eax
	decq	%r14
.LBB16_89:                              # %land.rhs235
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rax
	jne	.LBB16_96
# BB#90:                                # %while.cond232
                                        #   in Loop: Header=BB16_89 Depth=1
	cmpq	%r15, %r14
	jae	.LBB16_91
	jmp	.LBB16_98
.LBB16_95:                              # %land.rhs221.if.end259.loopexit1_crit_edge
	incq	%r14
	jmp	.LBB16_100
.LBB16_96:                              # %land.rhs235.while.cond246.preheader.loopexit_crit_edge
	incq	%r14
	jmp	.LBB16_98
.LBB16_97:                              # %while.cond246
                                        #   in Loop: Header=BB16_98 Depth=1
	decq	%r14
.LBB16_98:                              # %while.cond246
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %r14
	jb	.LBB16_100
# BB#99:                                # %land.rhs249
                                        #   in Loop: Header=BB16_98 Depth=1
	movzbl	(%r14), %eax
	movzbl	hctype(%rax), %eax
	testq	$1, %rax
	jne	.LBB16_97
.LBB16_100:                             # %if.end259
	incq	%r14
	cmpq	%r15, %r14
	je	.LBB16_530
# BB#101:                               # %if.then263
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r15, %rdx
	movq	%r14, %rcx
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	report_event
	movq	%r14, %r15
	jmp	.LBB16_530
.Ltmp256:
	.size	parse_buf, .Ltmp256-parse_buf
	.cfi_endproc

	.align	16, 0x90
	.type	report_event,@function
report_event:                           # @report_event
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp263:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp264:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp265:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp266:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp267:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp268:
	.cfi_def_cfa_offset 336
.Ltmp269:
	.cfi_offset %rbx, -48
.Ltmp270:
	.cfi_offset %r12, -40
.Ltmp271:
	.cfi_offset %r13, -32
.Ltmp272:
	.cfi_offset %r14, -24
.Ltmp273:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	16(%r12), %rax
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	24(%r12), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	64(%r12), %rbx
	movq	32(%r12), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%r9, 184(%rsp)          # 8-byte Spill
	movq	%r8, 216(%rsp)          # 8-byte Spill
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r13
	testq	%rbx, %rbx
	je	.LBB17_4
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$4, %rax
	je	.LBB17_4
# BB#2:                                 # %entry
	cmpq	$1, %rax
	je	.LBB17_4
# BB#3:                                 # %if.then
	movq	%rbx, 232(%rsp)
	movq	%rbx, %rdi
	callq	strlen
	addq	%rbx, %rax
	movq	%rax, 240(%rsp)
	movq	$0, 64(%r12)
	movq	344(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	231(%rsp), %rdx
	leaq	232(%rsp), %r9
	movq	$3, %rsi
	xorq	%r8, %r8
	movq	$1, (%rsp)
	movq	%r12, %rdi
	movq	%rdx, %rcx
	callq	report_event
.LBB17_4:                               # %if.end
	movq	PL_stack_sp(%rip), %rax
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_6
# BB#5:                                 # %cond.true
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_utf8_distance
	jmp	.LBB17_7
.LBB17_6:                               # %cond.false
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	%r14, %rax
	subq	%r15, %rax
.LBB17_7:                               # %cond.end
	addq	%rax, 16(%r12)
	cmpq	$0, 208(%rsp)           # 8-byte Folded Reload
	je	.LBB17_22
# BB#8:                                 # %while.cond.preheader
	cmpq	%r14, %r15
	jae	.LBB17_18
# BB#9:
	xorq	%rsi, %rsi
	movq	%r15, %rax
	.align	16, 0x90
.LBB17_10:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB17_12
# BB#11:                                # %if.then21
                                        #   in Loop: Header=BB17_10 Depth=1
	incq	24(%r12)
	movq	%rax, %rsi
.LBB17_12:                              # %if.end23
                                        #   in Loop: Header=BB17_10 Depth=1
	incq	%rax
	cmpq	%rax, %r14
	jne	.LBB17_10
# BB#13:                                # %while.end
	testq	%rsi, %rsi
	je	.LBB17_18
# BB#14:                                # %if.then25
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_16
# BB#15:                                # %cond.true27
	movq	%r14, %rdi
	callq	Perl_utf8_distance
	jmp	.LBB17_17
.LBB17_18:                              # %if.else
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_20
# BB#19:                                # %cond.true37
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_utf8_distance
	jmp	.LBB17_21
.LBB17_20:                              # %cond.false39
	movq	%r14, %rax
	subq	%r15, %rax
.LBB17_21:                              # %cond.end43
	addq	%rax, 32(%r12)
	jmp	.LBB17_22
.LBB17_16:                              # %cond.false29
	movq	%r14, %rax
	subq	%rsi, %rax
.LBB17_17:                              # %cond.end33
	decq	%rax
	movq	%rax, 32(%r12)
.LBB17_22:                              # %if.end48
	movq	%r15, 200(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r13, %r15
	andq	%rcx, %r15
	movq	%r15, 160(%rsp)         # 8-byte Spill
	cmpq	$10, %r15
	je	.LBB17_47
# BB#23:                                # %if.end52
	movl	120(%r12), %eax
	cmpq	$4, %rax
	je	.LBB17_47
# BB#24:                                # %if.end56
	cmpq	$0, 320(%r12)
	jne	.LBB17_27
# BB#25:                                # %lor.lhs.false
	cmpq	$0, 312(%r12)
	jne	.LBB17_27
# BB#26:                                # %lor.lhs.false59
	cmpq	$0, 328(%r12)
	je	.LBB17_66
.LBB17_27:                              # %if.then61
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %r13
	cmpq	$2, %r13
	jne	.LBB17_65
# BB#28:                                # %if.then67
	movq	%rcx, %r13
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	360(%r12), %rbx
	subq	%rsi, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_setpvn
	movq	16(%rbx), %rax
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	jne	.LBB17_29
# BB#30:                                # %if.then67
	andq	$-536870913, %rax       # imm = 0xFFFFFFFFDFFFFFFF
	jmp	.LBB17_31
.LBB17_65:                              # %if.else137
	cmpq	$0, 336(%r12)
	jne	.LBB17_47
	jmp	.LBB17_66
.LBB17_29:
	orq	$536870912, %rax        # imm = 0x20000000
.LBB17_31:                              # %if.then67
	movq	%rax, 16(%rbx)
	movzbl	140(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_41
# BB#32:                                # %lor.lhs.false83
	movzbl	143(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_41
# BB#33:                                # %if.then86
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB17_35
# BB#34:                                # %cond.true.i1376
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 264(%rsp)
	movq	(%rax), %rax
	jmp	.LBB17_37
.LBB17_35:                              # %cond.false.i1378
	leaq	264(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_pvn_force_flags
.LBB17_36:                              # %for.condthread-pre-split.i1381
	movq	264(%rsp), %rcx
.LBB17_37:                              # %for.cond.i1385
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 264(%rsp)
	je	.LBB17_41
# BB#38:                                # %for.body.i1390
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB17_40
# BB#39:
	addq	$32, %rcx
.LBB17_40:                              # %for.body.i1390
	movb	%cl, (%rax)
	incq	%rax
	jmp	.LBB17_36
.LBB17_41:                              # %if.end88
	movq	336(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_58
# BB#42:                                # %if.then90
	movq	%rbx, %rsi
	callq	Perl_sv_eq
	testq	%rax, %rax
	je	.LBB17_47
# BB#43:                                # %if.then94
	movl	344(%r12), %eax
	cmpq	$2, %r15
	jne	.LBB17_45
# BB#44:                                # %if.then97
	incq	%rax
	movl	%eax, 344(%r12)
	jmp	.LBB17_47
.LBB17_58:                              # %if.end109
	movq	328(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_61
# BB#59:                                # %land.lhs.true112
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB17_61
# BB#60:                                # %if.then116
	movq	%rbx, %rdi
	callq	Perl_newSVsv
	movq	%rax, 336(%r12)
	movq	$1, %rax
	movl	%eax, 344(%r12)
	jmp	.LBB17_47
.LBB17_61:                              # %if.end120
	movq	320(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_63
# BB#62:                                # %land.lhs.true123
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	jne	.LBB17_47
.LBB17_63:                              # %if.end128
	movq	312(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_66
# BB#64:                                # %land.lhs.true131
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	je	.LBB17_47
.LBB17_66:                              # %if.end143
	movq	$4, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	160(%r12,%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB17_68
# BB#67:
	leaq	160(%r12,%rax), %rdx
	jmp	.LBB17_70
.LBB17_68:                              # %if.then146
	movq	288(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_47
# BB#69:
	leaq	288(%r12), %rdx
.LBB17_70:                              # %if.end153
	movq	16(%rdi), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$10, %rcx
	je	.LBB17_82
# BB#71:                                # %cond.false163
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB17_76
# BB#72:                                # %cond.true168
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB17_310
# BB#73:                                # %land.lhs.true171
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB17_82
# BB#74:                                # %lor.lhs.false174
	testq	%rcx, %rcx
	je	.LBB17_310
# BB#75:                                # %land.lhs.true177
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB17_82
	jmp	.LBB17_310
.LBB17_45:                              # %if.else99
	decq	%rax
	testq	%r13, %rax
	movl	%eax, 344(%r12)
	jne	.LBB17_47
# BB#46:                                # %if.then103
	movq	336(%r12), %rdi
	callq	Perl_sv_free
	movq	$0, 336(%r12)
.LBB17_47:                              # %IGNORE_EVENT
	movq	112(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_310
# BB#48:                                # %if.then994
	cmpq	$4, %r15
	je	.LBB17_54
# BB#49:                                # %land.lhs.true997
	movq	72(%r12), %r15
	testq	%r15, %r15
	movq	200(%rsp), %rbx         # 8-byte Reload
	je	.LBB17_55
# BB#50:                                # %land.lhs.true1000
	movq	16(%r15), %r8
	testq	$118423552, %r8         # imm = 0x70F0000
	je	.LBB17_55
# BB#51:                                # %if.then1005
	movzbl	141(%r12), %ebx
	movzbl	56(%r12), %eax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	16(%r12), %rax
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	24(%r12), %r13
	movq	32(%r12), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	movb	%al, 141(%r12)
	movq	$0, 72(%r12)
	movzbl	80(%r12), %eax
	movb	%al, 56(%r12)
	movq	88(%r12), %rax
	movq	96(%r12), %rcx
	movq	%rcx, 24(%r12)
	movq	%rax, 16(%r12)
	movq	104(%r12), %rax
	movq	%rax, 32(%r12)
	movq	(%r15), %rax
	movq	(%rax), %rdx
	movq	8(%rax), %rcx
	addq	%rdx, %rcx
	andq	$536870912, %r8         # imm = 0x20000000
	movq	344(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r12, %rdi
	callq	report_event
	movq	16(%r15), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%r15)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB17_53
# BB#52:                                # %land.rhs.i1487
	movq	%r15, %rdi
	callq	Perl_sv_backoff
.LBB17_53:                              # %flush_pending_text.exit1488
	movb	%bl, 141(%r12)
	movq	%r15, 72(%r12)
	movq	192(%rsp), %rax         # 8-byte Reload
	movb	%al, 56(%r12)
	movq	%r13, 24(%r12)
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%r12)
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%r12)
	movq	112(%r12), %rdi
.LBB17_54:                              # %if.end1006
	movq	200(%rsp), %rbx         # 8-byte Reload
.LBB17_55:                              # %if.end1006
	movq	16(%rdi), %rax
	andq	$536870912, %rax        # imm = 0x20000000
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_307
# BB#56:                                # %land.lhs.true1008
	testq	%rax, %rax
	jne	.LBB17_308
# BB#57:                                # %if.then1013
	movq	$2, %rsi
	callq	Perl_sv_utf8_upgrade_flags
	movq	112(%r12), %rdi
	jmp	.LBB17_308
.LBB17_307:                             # %lor.lhs.false1018
	testq	%rax, %rax
	jne	.LBB17_94
.LBB17_308:                             # %if.then1023
	subq	%rbx, %r14
	movq	$2, %rcx
.LBB17_309:                             # %return
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	Perl_sv_catpvn_flags
	jmp	.LBB17_310
.LBB17_76:                              # %cond.false183
	movq	%rdx, %rbx
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB17_78
# BB#77:                                # %cond.true188
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jmp	.LBB17_81
.LBB17_78:                              # %cond.false193
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB17_80
# BB#79:                                # %cond.true198
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jmp	.LBB17_81
.LBB17_80:                              # %cond.false203
	callq	Perl_sv_2bool
	testq	$255, %rax
.LBB17_81:                              # %cond.false203
	movq	%rbx, %rdx
	je	.LBB17_310
.LBB17_82:                              # %if.end209
	movzbl	141(%r12), %ebx
	movq	72(%r12), %r13
	testq	%rbx, %rbx
	je	.LBB17_99
# BB#83:                                # %if.end209
	cmpq	$4, %r15
	jne	.LBB17_99
# BB#84:                                # %if.then215
	testq	%r13, %r13
	movq	200(%rsp), %rbx         # 8-byte Reload
	jne	.LBB17_86
# BB#85:                                # %if.then217
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_newSV
	movq	%rax, %r13
	movq	%r13, 72(%r12)
.LBB17_86:                              # %if.end220
	movq	16(%r13), %r8
	movzbl	56(%r12), %r15d
	testq	$118423552, %r8         # imm = 0x70F0000
	je	.LBB17_91
# BB#87:                                # %if.then225
	movzbl	80(%r12), %eax
	movq	%r15, %rcx
	andq	$255, %rcx
	cmpq	%rax, %rcx
	jne	.LBB17_88
# BB#95:                                # %if.end242
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	jne	.LBB17_96
	jmp	.LBB17_93
.LBB17_99:                              # %if.else274
	testq	%r13, %r13
	je	.LBB17_104
# BB#100:                               # %land.lhs.true277
	movq	16(%r13), %r8
	testq	$118423552, %r8         # imm = 0x70F0000
	je	.LBB17_104
# BB#101:                               # %if.then282
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	movzbl	56(%r12), %eax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	16(%r12), %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	24(%r12), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	32(%r12), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	movb	%al, 141(%r12)
	movq	$0, 72(%r12)
	movzbl	80(%r12), %eax
	movb	%al, 56(%r12)
	movq	88(%r12), %rax
	movq	96(%r12), %rcx
	movq	%rcx, 24(%r12)
	movq	%rax, 16(%r12)
	movq	104(%r12), %rax
	movq	%rax, 32(%r12)
	movq	(%r13), %rax
	movq	(%rax), %rdx
	movq	8(%rax), %rcx
	addq	%rdx, %rcx
	andq	$536870912, %r8         # imm = 0x20000000
	movq	344(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r12, %rdi
	callq	report_event
	movq	16(%r13), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%r13)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB17_103
# BB#102:                               # %land.rhs.i
	movq	%r13, %rdi
	callq	Perl_sv_backoff
.LBB17_103:                             # %flush_pending_text.exit
	movb	%bl, 141(%r12)
	movq	%r13, 72(%r12)
	movq	152(%rsp), %rax         # 8-byte Reload
	movb	%al, 56(%r12)
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%r12)
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%r12)
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%r12)
	movq	PL_stack_sp(%rip), %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rdx         # 8-byte Reload
.LBB17_104:                             # %if.end284
	movq	8(%rdx), %rdi
	testq	%rdi, %rdi
	je	.LBB17_105
# BB#106:                               # %cond.true287
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB17_108
# BB#107:                               # %cond.true293
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 248(%rsp)
	movq	(%rax), %rbx
	jmp	.LBB17_109
.LBB17_105:
	movabsq	$.L.str63, %rbx
	jmp	.LBB17_109
.LBB17_88:                              # %if.then230
	movzbl	141(%r12), %ebx
	movq	16(%r12), %rcx
	movq	%rcx, 192(%rsp)         # 8-byte Spill
	movq	24(%r12), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	32(%r12), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	movb	%cl, 141(%r12)
	movq	$0, 72(%r12)
	movb	%al, 56(%r12)
	movq	88(%r12), %rax
	movq	96(%r12), %rcx
	movq	%rcx, 24(%r12)
	movq	%rax, 16(%r12)
	movq	104(%r12), %rax
	movq	%rax, 32(%r12)
	movq	(%r13), %rax
	movq	(%rax), %rdx
	movq	8(%rax), %rcx
	addq	%rdx, %rcx
	andq	$536870912, %r8         # imm = 0x20000000
	movq	344(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$4, %rsi
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r12, %rdi
	callq	report_event
	movq	16(%r13), %rax
	movq	%rax, %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	movq	%rcx, 16(%r13)
	testq	$2097152, %rax          # imm = 0x200000
	je	.LBB17_90
# BB#89:                                # %land.rhs.i1465
	movq	%r13, %rdi
	callq	Perl_sv_backoff
.LBB17_90:                              # %flush_pending_text.exit1466
	movb	%bl, 141(%r12)
	movq	%r13, 72(%r12)
	movb	%r15b, 56(%r12)
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%r12)
	movq	192(%rsp), %rax         # 8-byte Reload
	movq	%rax, 16(%r12)
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	%rax, 32(%r12)
	movq	200(%rsp), %rbx         # 8-byte Reload
.LBB17_91:                              # %INIT_PEND_TEXT
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rax, 88(%r12)
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	%rax, 96(%r12)
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	%rax, 104(%r12)
	movb	%r15b, 80(%r12)
	movabsq	$.L.str63, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_setpvn
	movq	72(%r12), %r13
	movq	16(%r13), %r8
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_92
.LBB17_96:                              # %land.lhs.true244
	testq	$536870912, %r8         # imm = 0x20000000
	jne	.LBB17_98
# BB#97:                                # %if.then249
	movq	$2, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_utf8_upgrade_flags
	movq	72(%r12), %r13
	jmp	.LBB17_98
.LBB17_92:                              # %if.end242.thread
	andq	$-536870913, %r8        # imm = 0xFFFFFFFFDFFFFFFF
	movq	%r8, 16(%r13)
.LBB17_93:                              # %lor.lhs.false254
	testq	$536870912, %r8         # imm = 0x20000000
	je	.LBB17_98
.LBB17_94:                              # %if.else1028
	subq	%rbx, %r14
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_upgrade_flags
	movq	72(%r12), %rdi
	movq	$2, %rdx
	movq	%rbx, %rsi
	callq	Perl_sv_catsv_flags
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB17_310:                             # %return
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_98:                              # %if.then259
	subq	%rbx, %r14
	movq	$2, %rcx
	movq	%r13, %rdi
	jmp	.LBB17_309
.LBB17_108:                             # %cond.false300
	leaq	248(%rsp), %rsi
	movq	%rdx, %rbx
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rbx, %rdx
	movq	%rax, %rbx
.LBB17_109:                             # %cond.end306
	movq	(%rdx), %rsi
	movzbl	16(%rsi), %ecx
	movzbl	(%rbx), %eax
	andq	$255, %rax
	cmpq	$10, %rcx
	jne	.LBB17_113
# BB#110:                               # %if.then313
	cmpq	$22, %rax
	jne	.LBB17_112
# BB#111:                               # %if.then317
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	incq	%rbx
	movq	192(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB17_119
.LBB17_113:                             # %if.else323
	cmpq	$22, %rax
	je	.LBB17_114
# BB#115:                               # %if.else323
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	jmp	.LBB17_116
.LBB17_112:                             # %if.else320
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	callq	Perl_newAV
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	192(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB17_119
.LBB17_114:
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	incq	%rbx
.LBB17_116:                             # %if.else323
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB17_118
# BB#117:                               # %if.then333
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB17_118:                             # %if.end334
	movq	PL_stack_base(%rip), %rcx
	movq	192(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
.LBB17_119:                             # %if.end339
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	%rbx, %rdx
	movzbl	(%rdx), %ebx
	testq	%rbx, %rbx
	je	.LBB17_120
# BB#121:                               # %for.body.lr.ph
	movq	$32, %rcx
	movq	336(%rsp), %rax
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	$33, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	leaq	.L.str91(%r15), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%r14, %rcx
	movq	184(%rsp), %rax         # 8-byte Reload
	leaq	24(%rax), %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	leaq	40(%rax), %rsi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	leaq	8(%rax), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rdx, %rsi
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	200(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	jmp	.LBB17_122
.LBB17_120:
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	jmp	.LBB17_280
.LBB17_169:                             #   in Loop: Header=BB17_122 Depth=1
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	xorq	%r13, %r13
.LBB17_171:                             # %if.end526
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$32, %rcx
	movq	336(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rsi
	cmpq	$2, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	jl	.LBB17_213
.LBB17_172:                             # %for.body530
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	movq	-24(%rcx), %rdi
	movq	-16(%rcx), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpvn
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_174
# BB#173:                               # %if.then547
                                        #   in Loop: Header=BB17_172 Depth=2
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB17_174:                             # %if.end550
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	-8(%rcx), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_175
# BB#186:                               # %if.else602
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	152(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_188
# BB#187:                               # %if.else608
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, %rdi
.LBB17_188:                             # %if.end611
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, 136(%rsp)         # 8-byte Spill
	callq	Perl_newSVsv
	jmp	.LBB17_189
.LBB17_175:                             # %if.then556
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	(%rcx), %rsi
	movzbl	(%rdi), %eax
	subq	%rdi, %rsi
	cmpq	$34, %rax
	je	.LBB17_177
# BB#176:                               # %if.then556
                                        #   in Loop: Header=BB17_172 Depth=2
	cmpq	$39, %rax
	jne	.LBB17_178
.LBB17_177:                             # %if.then578
                                        #   in Loop: Header=BB17_172 Depth=2
	incq	%rdi
	addq	$-2, %rsi
.LBB17_178:                             # %if.end581
                                        #   in Loop: Header=BB17_172 Depth=2
	callq	Perl_newSVpvn
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_180
# BB#179:                               # %if.then584
                                        #   in Loop: Header=BB17_172 Depth=2
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB17_180:                             # %if.end587
                                        #   in Loop: Header=BB17_172 Depth=2
	movzbl	142(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB17_189
# BB#181:                               # %if.then589
                                        #   in Loop: Header=BB17_172 Depth=2
	movzbl	145(%r12), %ecx
	testq	%rcx, %rcx
	je	.LBB17_183
# BB#182:                               # %if.then591
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_utf8_decode
	movq	112(%rsp), %rax         # 8-byte Reload
.LBB17_183:                             # %if.end593
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	352(%r12), %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	decode_entities
	movzbl	145(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_185
# BB#184:                               #   in Loop: Header=BB17_172 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	jmp	.LBB17_189
.LBB17_185:                             # %if.then597
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	andq	$-536870913, 16(%rax)   # imm = 0xFFFFFFFFDFFFFFFF
.LBB17_189:                             # %if.end611
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movzbl	140(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_199
# BB#190:                               # %lor.lhs.false615
                                        #   in Loop: Header=BB17_172 Depth=2
	movzbl	143(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_199
# BB#191:                               # %if.then619
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	16(%rdi), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB17_193
# BB#192:                               # %cond.true.i1399
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 272(%rsp)
	movq	(%rax), %rax
	jmp	.LBB17_195
.LBB17_193:                             # %cond.false.i1401
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	$2, %rdx
	leaq	272(%rsp), %rsi
	callq	Perl_sv_pvn_force_flags
.LBB17_194:                             # %for.condthread-pre-split.i1404
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	272(%rsp), %rcx
.LBB17_195:                             # %for.cond.i1408
                                        #   in Loop: Header=BB17_172 Depth=2
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 272(%rsp)
	jne	.LBB17_196
.LBB17_199:                             # %if.end621
                                        #   in Loop: Header=BB17_172 Depth=2
	cmpq	$7, %rbx
	jne	.LBB17_204
# BB#200:                               # %if.then624
                                        #   in Loop: Header=BB17_172 Depth=2
	xorq	%rdx, %rdx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	Perl_hv_exists_ent
	testq	$255, %rax
	jne	.LBB17_202
# BB#201:                               # %lor.lhs.false628
                                        #   in Loop: Header=BB17_172 Depth=2
	xorq	%rcx, %rcx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	112(%rsp), %rdx         # 8-byte Reload
	callq	Perl_hv_store_ent
	testq	%rax, %rax
	jne	.LBB17_203
.LBB17_202:                             # %if.then631
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
.LBB17_203:                             # %if.end632
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_free
	jmp	.LBB17_212
.LBB17_204:                             # %if.else633
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	144(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	jne	.LBB17_205
# BB#206:                               # %if.else636
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	PL_stack_max(%rip), %rax
	movq	192(%rsp), %rdi         # 8-byte Reload
	subq	%rdi, %rax
	cmpq	$7, %rax
	jg	.LBB17_208
# BB#207:                               # %if.then643
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	$1, %rdx
	movq	%rdi, %rsi
	callq	Perl_stack_grow
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%rax, 192(%rsp)         # 8-byte Spill
.LBB17_208:                             # %if.end645
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	%rsi, %rdi
	callq	Perl_sv_2mortal
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
	movq	PL_stack_max(%rip), %rax
	addq	$8, %rcx
	subq	%rcx, %rax
	cmpq	$7, %rax
	movq	%rcx, %rdi
	jg	.LBB17_209
# BB#210:                               # %if.then655
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	$1, %rdx
	movq	%rdi, %rsi
	callq	Perl_stack_grow
	movq	%rax, 192(%rsp)         # 8-byte Spill
	jmp	.LBB17_211
.LBB17_205:                             # %if.then635
                                        #   in Loop: Header=BB17_172 Depth=2
	callq	Perl_av_push
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	Perl_av_push
	jmp	.LBB17_212
.LBB17_196:                             # %for.body.i1413
                                        #   in Loop: Header=BB17_172 Depth=2
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB17_198
# BB#197:                               #   in Loop: Header=BB17_172 Depth=2
	addq	$32, %rcx
.LBB17_198:                             # %for.body.i1413
                                        #   in Loop: Header=BB17_172 Depth=2
	movb	%cl, (%rax)
	incq	%rax
	jmp	.LBB17_194
.LBB17_209:                             #   in Loop: Header=BB17_172 Depth=2
	movq	%rdi, 192(%rsp)         # 8-byte Spill
.LBB17_211:                             # %if.end657
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	Perl_sv_2mortal
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
	addq	$8, %rcx
	movq	%rcx, 192(%rsp)         # 8-byte Spill
.LBB17_212:                             # %for.inc663
                                        #   in Loop: Header=BB17_172 Depth=2
	movq	$32, %rcx
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	336(%rsp), %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$2, %rsi
	movq	128(%rsp), %rcx         # 8-byte Reload
	addq	$32, %rcx
	cmpq	%rdx, %rax
	jl	.LBB17_172
.LBB17_213:                             # %sw.epilog
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB17_272
	jmp	.LBB17_279
.LBB17_158:                             # %cond.false.i1424
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$2, %rdx
	movq	%r13, %rdi
	leaq	280(%rsp), %rsi
	callq	Perl_sv_pvn_force_flags
	jmp	.LBB17_159
.LBB17_236:                             # %for.body.i
                                        #   in Loop: Header=BB17_229 Depth=2
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB17_238
# BB#237:                               #   in Loop: Header=BB17_229 Depth=2
	addq	$32, %rcx
.LBB17_238:                             # %for.body.i
                                        #   in Loop: Header=BB17_229 Depth=2
	movb	%cl, (%rax)
	incq	%rax
	jmp	.LBB17_234
.LBB17_229:                             # %if.end704
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movq	-8(%rbx), %rdi
	movq	(%rbx), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpvn
	movzbl	140(%r12), %ecx
	movq	%rax, %r13
	testq	%rcx, %rcx
	jne	.LBB17_239
# BB#230:                               # %lor.lhs.false708
                                        #   in Loop: Header=BB17_229 Depth=2
	movzbl	143(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_239
# BB#231:                               # %if.then712
                                        #   in Loop: Header=BB17_229 Depth=2
	movq	16(%r13), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB17_233
# BB#232:                               # %cond.true.i
                                        #   in Loop: Header=BB17_229 Depth=2
	movq	(%r13), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 256(%rsp)
	movq	(%rax), %rax
	jmp	.LBB17_235
.LBB17_233:                             # %cond.false.i
                                        #   in Loop: Header=BB17_229 Depth=2
	movq	$2, %rdx
	movq	%r13, %rdi
	leaq	256(%rsp), %rsi
	callq	Perl_sv_pvn_force_flags
.LBB17_234:                             # %for.condthread-pre-split.i
                                        #   in Loop: Header=BB17_229 Depth=2
	movq	256(%rsp), %rcx
.LBB17_235:                             # %for.cond.i
                                        #   in Loop: Header=BB17_229 Depth=2
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 256(%rsp)
	jne	.LBB17_236
.LBB17_239:                             # %if.end714
                                        #   in Loop: Header=BB17_229 Depth=2
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	Perl_av_push
	movq	$32, %rcx
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	336(%rsp), %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$2, %rsi
	addq	$32, %rbx
	cmpq	%rdx, %rax
	jl	.LBB17_229
.LBB17_240:                             # %for.end717
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
.LBB17_148:                             # %if.then841
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_newRV_noinc
	jmp	.LBB17_149
.LBB17_159:                             # %for.condthread-pre-split.i1427
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	280(%rsp), %rcx
.LBB17_160:                             # %for.cond.i1431
                                        #   in Loop: Header=BB17_122 Depth=1
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 280(%rsp)
	jne	.LBB17_161
.LBB17_164:                             # %if.end500
                                        #   in Loop: Header=BB17_122 Depth=1
	andq	$255, %rbx
	cmpq	$6, %rbx
	jne	.LBB17_272
# BB#165:                               # %if.end500
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$2, %r15
	je	.LBB17_272
# BB#166:                               # %if.then506
                                        #   in Loop: Header=BB17_122 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %r8
	movq	%r13, %rdi
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	Perl_sv_insert
	jmp	.LBB17_272
.LBB17_161:                             # %for.body.i1436
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB17_163
# BB#162:                               #   in Loop: Header=BB17_122 Depth=1
	addq	$32, %rcx
.LBB17_163:                             # %for.body.i1436
                                        #   in Loop: Header=BB17_122 Depth=1
	movb	%cl, (%rax)
	incq	%rax
	jmp	.LBB17_159
	.align	16, 0x90
.LBB17_122:                             # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_229 Depth 2
                                        #     Child Loop BB17_219 Depth 2
                                        #       Child Loop BB17_224 Depth 3
                                        #     Child Loop BB17_172 Depth 2
                                        #     Child Loop BB17_143 Depth 2
                                        #     Child Loop BB17_139 Depth 2
                                        #     Child Loop BB17_127 Depth 2
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	movq	$56, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	leaq	-1(%rsi), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$20, %rax
	jbe	.LBB17_123
# BB#275:                               # %sw.default
                                        #   in Loop: Header=BB17_122 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str92, %rdi
	callq	Perl_newSVpvf
.LBB17_149:                             # %if.then841
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
.LBB17_272:                             # %if.then841
                                        #   in Loop: Header=BB17_122 Depth=1
	testq	%r13, %r13
	je	.LBB17_215
	jmp	.LBB17_251
.LBB17_123:                             # %for.body
                                        #   in Loop: Header=BB17_122 Depth=1
	jmpq	*.LJTI17_0(,%rax,8)
.LBB17_150:                             # %sw.bb468
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$32, %rcx
	movq	336(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB17_215
# BB#151:                               # %if.then471
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movq	8(%rax), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_153
# BB#152:                               # %if.then484
                                        #   in Loop: Header=BB17_122 Depth=1
	orq	$536870912, 16(%r13)    # imm = 0x20000000
.LBB17_153:                             # %if.end487
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	140(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_164
# BB#154:                               # %lor.lhs.false491
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	143(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_164
# BB#155:                               # %lor.lhs.false491
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	$4, %rax
	je	.LBB17_164
# BB#156:                               # %if.then498
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	16(%r13), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB17_158
# BB#157:                               # %cond.true.i1422
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	(%r13), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 280(%rsp)
	movq	(%rax), %rax
	jmp	.LBB17_160
.LBB17_167:                             # %sw.bb512
                                        #   in Loop: Header=BB17_122 Depth=1
	andq	$255, %rbx
	cmpq	$2, %r15
	jne	.LBB17_214
# BB#168:                               # %if.then515
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$7, %rbx
	jne	.LBB17_169
# BB#170:                               # %if.then521
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_newHV
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB17_171
.LBB17_124:                             # %sw.bb
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	344(%rsp), %rdi
	jmp	.LBB17_274
.LBB17_125:                             # %sw.bb346
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$32, %rcx
	movq	336(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB17_215
# BB#126:                               # %for.body358.lr.ph
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_newAV
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rax, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	Perl_av_extend
	movq	$1, %r13
	movq	64(%rsp), %rbx          # 8-byte Reload
	movabsq	$PL_sv_undef, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB17_127:                             # %for.body358
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_128
# BB#131:                               # %if.else382
                                        #   in Loop: Header=BB17_127 Depth=2
	movq	152(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_133
# BB#132:                               # %cond.false387
                                        #   in Loop: Header=BB17_127 Depth=2
	movq	128(%rsp), %rdi         # 8-byte Reload
.LBB17_133:                             # %cond.end389
                                        #   in Loop: Header=BB17_127 Depth=2
	callq	Perl_newSVsv
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	jmp	.LBB17_134
.LBB17_128:                             # %if.then363
                                        #   in Loop: Header=BB17_127 Depth=2
	movq	(%rbx), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpvn
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_130
# BB#129:                               # %if.then378
                                        #   in Loop: Header=BB17_127 Depth=2
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB17_130:                             # %if.end381
                                        #   in Loop: Header=BB17_127 Depth=2
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	movq	%rax, 128(%rsp)         # 8-byte Spill
.LBB17_134:                             # %for.inc
                                        #   in Loop: Header=BB17_127 Depth=2
	callq	Perl_av_push
	movq	%r13, %rax
	andq	%r15, %rax
	movq	336(%rsp), %rcx
	andq	%r15, %rcx
	incq	%r13
	addq	$16, %rbx
	cmpq	%rcx, %rax
	jne	.LBB17_127
# BB#135:                               # %for.end
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newRV_noinc
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	movq	160(%rsp), %r15         # 8-byte Reload
	jmp	.LBB17_272
.LBB17_136:                             # %sw.bb396
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$32, %rcx
	movq	336(%rsp), %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB17_215
# BB#137:                               # %land.lhs.true399
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	200(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	jb	.LBB17_215
# BB#138:                               # %for.body414.lr.ph
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_newAV
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rax, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	Perl_av_extend
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	movq	$1, %rbx
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rcx
	movq	%rax, %r13
	je	.LBB17_143
.LBB17_139:                             # %for.body414.us
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movq	-8(%rcx), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_140
# BB#141:                               # %if.else458.us
                                        #   in Loop: Header=BB17_139 Depth=2
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movq	136(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	xorq	%rdi, %rdi
	jmp	.LBB17_142
.LBB17_140:                             # %cond.end455.us
                                        #   in Loop: Header=BB17_139 Depth=2
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	Perl_utf8_distance
	movq	%rax, %rdi
	callq	Perl_newSViv
	movq	136(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	-8(%rax), %rsi
	movq	(%rax), %rdi
	callq	Perl_utf8_distance
	movq	%rax, %rdi
.LBB17_142:                             # %for.inc462.us
                                        #   in Loop: Header=BB17_139 Depth=2
	callq	Perl_newSViv
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	336(%rsp), %rcx
	andq	%rdx, %rcx
	incq	%rbx
	movq	128(%rsp), %rdx         # 8-byte Reload
	addq	$16, %rdx
	cmpq	%rcx, %rax
	movq	%rdx, %rcx
	jne	.LBB17_139
	jmp	.LBB17_147
.LBB17_143:                             # %for.body414
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%r13), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_144
# BB#145:                               # %if.else458
                                        #   in Loop: Header=BB17_143 Depth=2
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_av_push
	xorq	%rdi, %rdi
	jmp	.LBB17_146
.LBB17_144:                             # %cond.end455
                                        #   in Loop: Header=BB17_143 Depth=2
	movq	200(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rdi
	callq	Perl_newSViv
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	-8(%r13), %rax
	movq	(%r13), %rdi
	subq	%rax, %rdi
.LBB17_146:                             # %for.inc462
                                        #   in Loop: Header=BB17_143 Depth=2
	callq	Perl_newSViv
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_av_push
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	336(%rsp), %rcx
	andq	%rdx, %rcx
	incq	%rbx
	addq	$16, %r13
	cmpq	%rcx, %rax
	jne	.LBB17_143
.LBB17_147:                             # %for.end464
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	136(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB17_148
.LBB17_216:                             # %sw.bb672
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$2, %r15
	jne	.LBB17_215
# BB#217:                               # %if.then675
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$32, %rcx
	movq	336(%rsp), %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	Perl_newAV
	movq	%rax, 128(%rsp)         # 8-byte Spill
	cmpq	$2, %rbx
	jl	.LBB17_240
# BB#218:                               # %for.body684.lr.ph
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$3, %rsi
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r13
	movq	%rax, %rbx
	je	.LBB17_229
.LBB17_219:                             # %if.end704.us
                                        #   Parent Loop BB17_122 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB17_224 Depth 3
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movq	-8(%r13), %rdi
	movq	(%r13), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	orq	$536870912, 16(%rbx)    # imm = 0x20000000
	movzbl	140(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_228
# BB#220:                               # %lor.lhs.false708.us
                                        #   in Loop: Header=BB17_219 Depth=2
	movzbl	143(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_228
# BB#221:                               # %if.then712.us
                                        #   in Loop: Header=BB17_219 Depth=2
	movq	16(%rbx), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB17_222
# BB#223:                               # %cond.true.i.us
                                        #   in Loop: Header=BB17_219 Depth=2
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 256(%rsp)
	movq	(%rax), %rax
	jmp	.LBB17_224
.LBB17_222:                             # %cond.false.i.us
                                        #   in Loop: Header=BB17_219 Depth=2
	movq	$2, %rdx
	movq	%rbx, %rdi
	leaq	256(%rsp), %rsi
	callq	Perl_sv_pvn_force_flags
	movq	256(%rsp), %rcx
	jmp	.LBB17_224
.LBB17_227:                             # %for.body.i.us
                                        #   in Loop: Header=BB17_224 Depth=3
	movb	%cl, (%rax)
	movq	256(%rsp), %rcx
	incq	%rax
.LBB17_224:                             # %for.cond.i.us
                                        #   Parent Loop BB17_122 Depth=1
                                        #     Parent Loop BB17_219 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rcx, %rcx
	leaq	-1(%rcx), %rcx
	movq	%rcx, 256(%rsp)
	je	.LBB17_228
# BB#225:                               # %for.body.i.us
                                        #   in Loop: Header=BB17_224 Depth=3
	movzbl	(%rax), %ecx
	leaq	-65(%rcx), %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jae	.LBB17_227
# BB#226:                               #   in Loop: Header=BB17_224 Depth=3
	addq	$32, %rcx
	jmp	.LBB17_227
.LBB17_228:                             # %if.end714.us
                                        #   in Loop: Header=BB17_219 Depth=2
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_av_push
	movq	$32, %rcx
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	336(%rsp), %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$2, %rsi
	addq	$32, %r13
	cmpq	%rdx, %rax
	jl	.LBB17_219
	jmp	.LBB17_240
.LBB17_241:                             # %sw.bb721
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	200(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_272
# BB#242:                               # %if.then728
                                        #   in Loop: Header=BB17_122 Depth=1
	orq	$536870912, 16(%r13)    # imm = 0x20000000
	jmp	.LBB17_251
.LBB17_243:                             # %sw.bb732
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$4, %r15
	jne	.LBB17_215
# BB#244:                               # %if.then735
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	200(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	je	.LBB17_246
# BB#245:                               # %if.then742
                                        #   in Loop: Header=BB17_122 Depth=1
	orq	$536870912, 16(%r13)    # imm = 0x20000000
.LBB17_246:                             # %if.end745
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	56(%r12), %eax
	testq	%rax, %rax
	jne	.LBB17_272
# BB#247:                               # %if.then748
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	145(%r12), %eax
	testq	%rax, %rax
	je	.LBB17_249
# BB#248:                               # %if.then751
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r13, %rdi
	callq	Perl_sv_utf8_decode
.LBB17_249:                             # %if.end753
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	352(%r12), %rsi
	movq	$1, %rdx
	movq	%r13, %rdi
	callq	decode_entities
	movzbl	145(%r12), %eax
	testq	%rax, %rax
	je	.LBB17_272
# BB#250:                               # %if.then758
                                        #   in Loop: Header=BB17_122 Depth=1
	andq	$-536870913, 16(%r13)   # imm = 0xFFFFFFFFDFFFFFFF
	jmp	.LBB17_251
.LBB17_255:                             # %sw.bb764
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$4, %r15
	jne	.LBB17_215
# BB#256:                               # %if.then767
                                        #   in Loop: Header=BB17_122 Depth=1
	movzbl	56(%r12), %eax
	testq	%rax, %rax
	movabsq	$PL_sv_yes, %r13
	jne	.LBB17_251
# BB#257:                               # %select.mid
                                        #   in Loop: Header=BB17_122 Depth=1
	movabsq	$PL_sv_no, %r13
	jmp	.LBB17_251
.LBB17_258:                             # %sw.bb773
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	112(%r12), %rdi
	callq	Perl_sv_2mortal
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movabsq	$.L.str63, %rdi
	callq	Perl_newSVpvn
	movq	%rax, 112(%r12)
	jmp	.LBB17_272
.LBB17_259:                             # %sw.bb777
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	176(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSViv
	jmp	.LBB17_149
.LBB17_260:                             # %sw.bb780
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB17_262
# BB#261:                               # %cond.true782
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r14, %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	Perl_utf8_distance
	movq	%rax, %rdi
.LBB17_262:                             # %cond.end788
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	176(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rdi
	callq	Perl_newSViv
	jmp	.LBB17_149
.LBB17_263:                             # %sw.bb793
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$0, 216(%rsp)           # 8-byte Folded Reload
	movq	96(%rsp), %rdi          # 8-byte Reload
	je	.LBB17_265
# BB#264:                               # %cond.true795
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r14, %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	Perl_utf8_distance
	movq	%rax, %rdi
.LBB17_265:                             # %cond.end801
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_newSViv
	jmp	.LBB17_149
.LBB17_266:                             # %sw.bb805
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSViv
	jmp	.LBB17_149
.LBB17_267:                             # %sw.bb808
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	Perl_newSViv
	jmp	.LBB17_149
.LBB17_268:                             # %sw.bb811
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	event_id_str(,%r15,8), %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	jmp	.LBB17_149
.LBB17_273:                             # %sw.bb835
                                        #   in Loop: Header=BB17_122 Depth=1
	movabsq	$PL_sv_undef, %rdi
.LBB17_274:                             # %if.then841
                                        #   in Loop: Header=BB17_122 Depth=1
	callq	Perl_sv_mortalcopy
	movq	%rax, %r13
	jmp	.LBB17_272
.LBB17_269:                             # %sw.bb816
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movzbl	1(%rax), %ebx
	leaq	2(%rax), %rdi
	movq	%rbx, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movzbl	19(%rcx), %ecx
	movq	%rax, %r13
	testq	$32, %rcx
	je	.LBB17_271
# BB#270:                               # %if.then829
                                        #   in Loop: Header=BB17_122 Depth=1
	orq	$536870912, 16(%r13)    # imm = 0x20000000
.LBB17_271:                             # %if.end832
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	leaq	1(%rbx,%rax), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	jmp	.LBB17_272
.LBB17_214:                             # %if.else666
                                        #   in Loop: Header=BB17_122 Depth=1
	cmpq	$8, %rbx
	je	.LBB17_279
.LBB17_215:                             # %if.then843
                                        #   in Loop: Header=BB17_122 Depth=1
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_sv_mortalcopy
	movq	%rax, %r13
.LBB17_251:                             # %if.end845
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	144(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	jne	.LBB17_252
# BB#276:                               # %if.else851
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	PL_stack_max(%rip), %rcx
	movq	192(%rsp), %rax         # 8-byte Reload
	subq	%rax, %rcx
	cmpq	$7, %rcx
	jg	.LBB17_278
# BB#277:                               # %if.then858
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	Perl_stack_grow
.LBB17_278:                             # %if.end860
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r13, 8(%rax)
	addq	$8, %rax
	movq	%rax, 192(%rsp)         # 8-byte Spill
	jmp	.LBB17_279
.LBB17_252:                             # %if.then847
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r13, PL_Sv(%rip)
	testq	%r13, %r13
	je	.LBB17_254
# BB#253:                               # %land.rhs
                                        #   in Loop: Header=BB17_122 Depth=1
	incq	8(%r13)
.LBB17_254:                             # %land.end
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	%r13, %rsi
	callq	Perl_av_push
.LBB17_279:                             # %for.inc865
                                        #   in Loop: Header=BB17_122 Depth=1
	movq	152(%rsp), %rsi         # 8-byte Reload
	movzbl	1(%rsi), %ebx
	incq	%rsi
	testq	%rbx, %rbx
	jne	.LBB17_122
.LBB17_280:                             # %for.end867
	movq	144(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	movq	192(%rsp), %rax         # 8-byte Reload
	je	.LBB17_283
# BB#281:                               # %if.then869
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	cmpq	%rbx, %rdi
	je	.LBB17_305
# BB#282:                               # %if.then873
	callq	Perl_newRV_noinc
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	Perl_av_push
	jmp	.LBB17_305
.LBB17_283:                             # %if.else877
	movq	%rax, PL_stack_sp(%rip)
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rdi
	cmpq	$1, %rax
	jne	.LBB17_289
# BB#284:                               # %land.lhs.true881
	movq	16(%rdi), %rax
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB17_285
.LBB17_289:                             # %if.else906
	movq	$134, %rsi
	callq	Perl_call_sv
	jmp	.LBB17_290
.LBB17_285:                             # %if.then886
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB17_287
# BB#286:                               # %cond.true893
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 248(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB17_288
.LBB17_287:                             # %cond.false900
	leaq	248(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB17_288:                             # %cond.end903
	movq	$134, %rsi
	callq	Perl_call_method
.LBB17_290:                             # %if.end909
	movq	PL_errgv(%rip), %rax
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB17_302
# BB#291:                               # %cond.false913
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB17_296
# BB#292:                               # %cond.true920
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB17_302
# BB#293:                               # %land.lhs.true926
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB17_301
# BB#294:                               # %lor.lhs.false930
	testq	%rcx, %rcx
	je	.LBB17_302
# BB#295:                               # %land.lhs.true933
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB17_301
	jmp	.LBB17_302
.LBB17_296:                             # %cond.false940
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB17_298
# BB#297:                               # %cond.true947
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB17_301
	jmp	.LBB17_302
.LBB17_298:                             # %cond.false955
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB17_300
# BB#299:                               # %cond.true962
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB17_301
	jmp	.LBB17_302
.LBB17_300:                             # %cond.false970
	callq	Perl_sv_2bool
	testq	$255, %rax
	je	.LBB17_302
.LBB17_301:                             # %if.then977
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB17_302:                             # %if.end978
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB17_304
# BB#303:                               # %if.then981
	callq	Perl_free_tmps
.LBB17_304:                             # %if.end982
	callq	Perl_pop_scope
.LBB17_305:                             # %if.end983
	movq	112(%r12), %rax
	testq	%rax, %rax
	je	.LBB17_310
# BB#306:                               # %if.then986
	movq	(%rax), %rax
	movq	$0, 8(%rax)
	jmp	.LBB17_310
.Ltmp274:
	.size	report_event, .Ltmp274-report_event
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_124
	.quad	.LBB17_125
	.quad	.LBB17_136
	.quad	.LBB17_150
	.quad	.LBB17_150
	.quad	.LBB17_150
	.quad	.LBB17_167
	.quad	.LBB17_167
	.quad	.LBB17_216
	.quad	.LBB17_241
	.quad	.LBB17_243
	.quad	.LBB17_255
	.quad	.LBB17_258
	.quad	.LBB17_259
	.quad	.LBB17_260
	.quad	.LBB17_263
	.quad	.LBB17_266
	.quad	.LBB17_267
	.quad	.LBB17_268
	.quad	.LBB17_273
	.quad	.LBB17_269

	.text
	.align	16, 0x90
	.type	marked_section_update,@function
marked_section_update:                  # @marked_section_update
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
	subq	$48, %rsp
.Ltmp286:
	.cfi_def_cfa_offset 96
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
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	128(%rdi), %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	testq	%rbx, %rbx
	movl	%eax, 120(%rdi)
	je	.LBB18_23
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	Perl_av_len
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB18_20
# BB#2:                                 # %for.body.lr.ph
	incq	%rax
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB18_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_6 Depth 2
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB18_19
# BB#4:                                 # %if.then6
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %r14
	movq	%r14, %rdi
	callq	Perl_av_len
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB18_19
# BB#5:                                 # %for.body12.lr.ph
                                        #   in Loop: Header=BB18_3 Depth=1
	incq	%r15
	movq	$1, %rbx
	.align	16, 0x90
.LBB18_6:                               # %for.body12
                                        #   Parent Loop BB18_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%rbx), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB18_18
# BB#7:                                 # %if.then17
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB18_8
# BB#9:                                 # %cond.false
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	$2, %rdx
	leaq	40(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r13
	jmp	.LBB18_10
	.align	16, 0x90
.LBB18_8:                               # %cond.true
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %r13
.LBB18_10:                              # %cond.end
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	%r13, %rdi
	movabsq	$.L.str97, %rsi
	callq	strcmp
	testq	%r12, %rax
	movq	$1, %rax
	je	.LBB18_16
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	%r13, %rdi
	movabsq	$.L.str101, %rsi
	callq	strcmp
	testq	%r12, %rax
	movq	$2, %rax
	je	.LBB18_16
# BB#12:                                # %if.else29
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	%r13, %rdi
	movabsq	$.L.str102, %rsi
	callq	strcmp
	testq	%r12, %rax
	movq	$3, %rax
	je	.LBB18_16
# BB#13:                                # %if.else33
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	%r13, %rdi
	movabsq	$.L.str103, %rsi
	callq	strcmp
	testq	%r12, %rax
	movabsq	$0, %rax
	jne	.LBB18_15
# BB#14:                                # %if.else33
                                        #   in Loop: Header=BB18_6 Depth=2
	movabsq	$1, %rax
.LBB18_15:                              # %if.else33
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	$2, %rcx
	shlq	%cl, %rax
	.align	16, 0x90
.LBB18_16:                              # %if.end40
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	120(%rcx), %ecx
	movq	%rax, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	jae	.LBB18_18
# BB#17:                                # %if.then44
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 120(%rcx)
.LBB18_18:                              # %for.inc
                                        #   in Loop: Header=BB18_6 Depth=2
	movq	%rbx, %rax
	andq	%r12, %rax
	movq	%r15, %rcx
	andq	%r12, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	jne	.LBB18_6
.LBB18_19:                              # %for.inc49
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	incq	%rsi
	movq	%rsi, %rax
	andq	%r12, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	16(%rsp), %rbx          # 8-byte Reload
	jne	.LBB18_3
.LBB18_20:                              # %if.end52.loopexit
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	120(%rax), %eax
	cmpq	$3, %rax
	jne	.LBB18_21
# BB#22:                                # %if.end52.loopexit
	movabsq	$1, %rax
	jmp	.LBB18_23
.LBB18_21:
	movabsq	$0, %rax
.LBB18_23:                              # %if.end52
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 56(%rcx)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp292:
	.size	marked_section_update, .Ltmp292-marked_section_update
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"self"
	.size	.L.str, 5

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"tokens"
	.size	.L.str1, 7

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"tokenpos"
	.size	.L.str2, 9

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"token0"
	.size	.L.str3, 7

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"tagname"
	.size	.L.str4, 8

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"tag"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"attr"
	.size	.L.str6, 5

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"@attr"
	.size	.L.str7, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"attrseq"
	.size	.L.str8, 8

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"text"
	.size	.L.str9, 5

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"dtext"
	.size	.L.str10, 6

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"is_cdata"
	.size	.L.str11, 9

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"skipped_text"
	.size	.L.str12, 13

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"offset"
	.size	.L.str13, 7

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"offset_end"
	.size	.L.str14, 11

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"length"
	.size	.L.str15, 7

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"line"
	.size	.L.str16, 5

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"column"
	.size	.L.str17, 7

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"event"
	.size	.L.str18, 6

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"undef"
	.size	.L.str19, 6

	.type	argname,@object         # @argname
	.data
	.globl	argname
	.align	16
argname:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.size	argname, 160

	.type	vtbl_free_pstate,@object # @vtbl_free_pstate
	.globl	vtbl_free_pstate
	.align	8
vtbl_free_pstate:
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	magic_free_pstate
	.quad	0
	.quad	0
	.size	vtbl_free_pstate, 56

	.type	.L.str20,@object        # @.str20
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str20:
	.asciz	"Usage: HTML::Parser::_alloc_pstate(self)"
	.size	.L.str20, 41

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Not a reference to a hash"
	.size	.L.str21, 26

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"HTML::Entities::entity2char"
	.size	.L.str22, 28

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"_hparser_xs_state"
	.size	.L.str23, 18

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Usage: HTML::Parser::parse(self, chunk)"
	.size	.L.str24, 40

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Parse loop not allowed"
	.size	.L.str25, 23

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Usage: HTML::Parser::eof(self)"
	.size	.L.str26, 31

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Usage: %s(pstate, ...)"
	.size	.L.str27, 23

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Unknown boolean attribute (%d)"
	.size	.L.str28, 31

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Usage: HTML::Parser::boolean_attribute_value(pstate, ...)"
	.size	.L.str29, 58

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Unknown tag-list attribute (%d)"
	.size	.L.str30, 32

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"Can't report tag lists yet"
	.size	.L.str31, 27

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Tag list must be plain scalars and arrays"
	.size	.L.str32, 42

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"Usage: HTML::Parser::handler(pstate, eventname, ...)"
	.size	.L.str33, 53

	.type	event_id_str,@object    # @event_id_str
	.section	.rodata,"a",@progbits
	.align	16
event_id_str:
	.quad	.L.str72
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str9
	.quad	.L.str76
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.size	event_id_str, 72

	.type	.L.str34,@object        # @.str34
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str34:
	.asciz	"No handler for %s events"
	.size	.L.str34, 25

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Can't inline decode readonly string"
	.size	.L.str35, 36

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"Usage: HTML::Entities::_decode_entities(string, entities, ...)"
	.size	.L.str36, 63

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"2nd argument must be hash reference"
	.size	.L.str37, 36

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Usage: HTML::Entities::_probably_utf8_chunk(string)"
	.size	.L.str38, 52

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"Usage: HTML::Entities::UNICODE_SUPPORT()"
	.size	.L.str39, 41

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Parser.c"
	.size	.L.str40, 9

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"HTML::Parser::_alloc_pstate"
	.size	.L.str41, 28

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"HTML::Parser::parse"
	.size	.L.str42, 20

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"HTML::Parser::eof"
	.size	.L.str43, 18

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"HTML::Parser::closing_plaintext"
	.size	.L.str44, 32

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"HTML::Parser::strict_end"
	.size	.L.str45, 25

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"HTML::Parser::marked_sections"
	.size	.L.str46, 30

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"HTML::Parser::case_sensitive"
	.size	.L.str47, 29

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"HTML::Parser::unbroken_text"
	.size	.L.str48, 28

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"HTML::Parser::strict_comment"
	.size	.L.str49, 29

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"HTML::Parser::xml_mode"
	.size	.L.str50, 23

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"HTML::Parser::attr_encoded"
	.size	.L.str51, 27

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"HTML::Parser::strict_names"
	.size	.L.str52, 27

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"HTML::Parser::utf8_mode"
	.size	.L.str53, 24

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"HTML::Parser::boolean_attribute_value"
	.size	.L.str54, 38

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"HTML::Parser::ignore_tags"
	.size	.L.str55, 26

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"HTML::Parser::ignore_elements"
	.size	.L.str56, 30

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"HTML::Parser::report_tags"
	.size	.L.str57, 26

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"HTML::Parser::handler"
	.size	.L.str58, 22

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"HTML::Entities::decode_entities"
	.size	.L.str59, 32

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"HTML::Entities::_decode_entities"
	.size	.L.str60, 33

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"HTML::Entities::_probably_utf8_chunk"
	.size	.L.str61, 37

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"HTML::Entities::UNICODE_SUPPORT"
	.size	.L.str62, 32

	.type	.L.str63,@object        # @.str63
.L.str63:
	.zero	1
	.size	.L.str63, 1

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Only code or array references allowed as handler"
	.size	.L.str64, 49

	.type	hctype,@object          # @hctype
	.section	.rodata,"a",@progbits
	.align	16
hctype:
	.ascii	"xxxxxxxxx\001\001x\001\001xxxxxxxxxxxxxxxxxx\001xxxxxxxxxxxx||X||||||||||~xx(\000xx~~~~~~~~~~~~~~~~~~~~~~~~~~xxxx~x~~~~~~~~~~~~~~~~~~~~~~~~~~xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\001xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
	.size	hctype, 256

	.type	.L.str65,@object        # @.str65
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str65:
	.asciz	"Unrecognized identifier %.*s in argspec"
	.size	.L.str65, 40

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"Literal string is longer than 255 chars in argspec"
	.size	.L.str66, 51

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"Backslash reserved for literal string in argspec"
	.size	.L.str67, 49

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"Unterminated literal string in argspec"
	.size	.L.str68, 39

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"Bad argspec (%s)"
	.size	.L.str69, 17

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"Bad argspec: stuff after @{...} (%s)"
	.size	.L.str70, 37

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"Missing comma separator in argspec"
	.size	.L.str71, 35

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"declaration"
	.size	.L.str72, 12

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"comment"
	.size	.L.str73, 8

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"start"
	.size	.L.str74, 6

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"end"
	.size	.L.str75, 4

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"process"
	.size	.L.str76, 8

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"start_document"
	.size	.L.str77, 15

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"end_document"
	.size	.L.str78, 13

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"default"
	.size	.L.str79, 8

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"plaintext"
	.size	.L.str80, 10

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"\357\273\277"
	.size	.L.str81, 4

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"\303\257\302\273\302\277"
	.size	.L.str82, 7

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"Parsing of undecoded UTF-8 will give garbage when decoding entities"
	.size	.L.str83, 68

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"\377\376"
	.size	.L.str84, 3

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"Parsing string decoded with wrong endianess"
	.size	.L.str85, 44

	.type	.L.str87,@object        # @.str87
	.section	.rodata,"a",@progbits
.L.str87:
	.asciz	"\376\377\000\000"
	.size	.L.str87, 5

	.type	.L.str88,@object        # @.str88
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str88:
	.asciz	"Parsing of undecoded UTF-32"
	.size	.L.str88, 28

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"\376\377"
	.size	.L.str89, 3

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"Parsing of undecoded UTF-16"
	.size	.L.str90, 28

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"!##/#?#"
	.size	.L.str91, 8

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"Bad argspec %d"
	.size	.L.str92, 15

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"script"
	.size	.L.str93, 7

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"style"
	.size	.L.str94, 6

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"DOCTYPE"
	.size	.L.str95, 8

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"ENTITY"
	.size	.L.str96, 7

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"include"
	.size	.L.str97, 8

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"xmp"
	.size	.L.str98, 4

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"title"
	.size	.L.str99, 6

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"textarea"
	.size	.L.str100, 9

	.type	literal_mode_elem,@object # @literal_mode_elem
	.section	.rodata,"a",@progbits
	.align	16
literal_mode_elem:
	.long	6                       # 0x6
	.zero	4
	.quad	.L.str93
	.long	1                       # 0x1
	.zero	4
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str94
	.long	1                       # 0x1
	.zero	4
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str98
	.long	1                       # 0x1
	.zero	4
	.long	9                       # 0x9
	.zero	4
	.quad	.L.str80
	.long	1                       # 0x1
	.zero	4
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str99
	.long	0                       # 0x0
	.zero	4
	.long	8                       # 0x8
	.zero	4
	.quad	.L.str100
	.long	0                       # 0x0
	.zero	4
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	literal_mode_elem, 168

	.type	.L.str101,@object       # @.str101
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str101:
	.asciz	"rcdata"
	.size	.L.str101, 7

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"cdata"
	.size	.L.str102, 6

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"ignore"
	.size	.L.str103, 7

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"_hparser_xs_state element is not a reference"
	.size	.L.str104, 45

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"Can't find '_hparser_xs_state' element in HTML::Parser hash"
	.size	.L.str105, 60

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"Bad signature in parser state object at %p"
	.size	.L.str106, 43


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
