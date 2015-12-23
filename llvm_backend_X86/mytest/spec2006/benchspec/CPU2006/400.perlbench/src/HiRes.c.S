	.file	"HiRes.c.bc"
	.text
	.globl	hrt_usleep
	.align	16, 0x90
	.type	hrt_usleep,@function
hrt_usleep:                             # @hrt_usleep
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp1:
	.cfi_def_cfa_offset 32
	movq	$0, 8(%rsp)
	movq	%rdi, 16(%rsp)
	leaq	8(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	select
	addq	$24, %rsp
	retq
.Ltmp2:
	.size	hrt_usleep, .Ltmp2-hrt_usleep
	.cfi_endproc

	.globl	XS_Time__HiRes_constant
	.align	16, 0x90
	.type	XS_Time__HiRes_constant,@function
XS_Time__HiRes_constant:                # @XS_Time__HiRes_constant
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
	subq	$16, %rsp
.Ltmp14:
	.cfi_def_cfa_offset 64
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r12
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r12, %r13
	subq	%rax, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	cmpq	$1, %r13
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB1_2:                                # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %edx
	movq	$3, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	movq	%r12, %r14
	subq	%rsi, %r14
	testq	$32, %rdx
	je	.LBB1_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	jmp	.LBB1_5
.LBB1_4:                                # %cond.false
	callq	Perl_sv_newmortal
.LBB1_5:                                # %cond.end
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB1_7
# BB#6:                                 # %cond.true11
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r15
	jmp	.LBB1_8
.LBB1_7:                                # %cond.false13
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rcx
	movq	%rax, %r15
.LBB1_8:                                # %cond.end15
	addq	$-8, %rcx
	cmpq	$7, %rcx
	ja	.LBB1_32
# BB#9:                                 # %cond.end15
	jmpq	*.LJTI1_0(,%rcx,8)
.LBB1_10:                               # %sw.bb.i
	movsbq	7(%r15), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$112, %rax
	jne	.LBB1_11
# BB#15:                                # %sw.bb2.i
	movabsq	$.L.str20, %rsi
	jmp	.LBB1_13
.LBB1_16:                               # %sw.bb7.i
	movsbq	7(%r15), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	cmpq	$104, %rax
	jg	.LBB1_21
# BB#17:                                # %sw.bb7.i
	andq	%rbx, %rax
	cmpq	$80, %rax
	jne	.LBB1_18
# BB#35:                                # %sw.bb.i.i
	movabsq	$.L.str24, %rsi
	movq	$11, %rdx
	movq	%r15, %rdi
	callq	memcmp
	testq	%rbx, %rax
	movq	$2, %rcx
	jne	.LBB1_32
	jmp	.LBB1_36
.LBB1_26:                               # %sw.bb9.i
	movsbq	6(%r15), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$105, %rax
	jne	.LBB1_27
# BB#30:                                # %sw.bb17.i
	movabsq	$.L.str22, %rsi
	jmp	.LBB1_29
.LBB1_31:                               # %sw.bb23.i
	movabsq	$.L.str23, %rsi
	movq	$15, %rdx
	movq	%r15, %rdi
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_32
# BB#34:                                # %sw.bb21
	movabsq	$.L.str2, %rdi
	jmp	.LBB1_33
.LBB1_11:                               # %sw.bb.i
	cmpq	$109, %rax
	jne	.LBB1_32
# BB#12:                                # %sw.bb1.i
	movabsq	$.L.str19, %rsi
.LBB1_13:                               # %sw.bb1.i
	movq	$7, %rdx
	jmp	.LBB1_14
.LBB1_21:                               # %sw.bb7.i
	andq	%rbx, %rax
	cmpq	$105, %rax
	jne	.LBB1_22
# BB#24:                                # %sw.bb6.i.i
	movabsq	$.L.str26, %rsi
	movq	$11, %rdx
	movq	%r15, %rdi
	callq	memcmp
	xorq	%rcx, %rcx
	testq	%rbx, %rax
	je	.LBB1_36
# BB#25:                                # %if.end10.i.i
	movabsq	$.L.str27, %rsi
	movq	$11, %rdx
	jmp	.LBB1_14
.LBB1_27:                               # %sw.bb9.i
	cmpq	$95, %rax
	jne	.LBB1_32
# BB#28:                                # %sw.bb12.i
	movabsq	$.L.str21, %rsi
.LBB1_29:                               # %sw.bb12.i
	movq	$14, %rdx
	movq	%r15, %rdi
	callq	memcmp
	testq	%rbx, %rax
	movq	$1, %rcx
	jne	.LBB1_32
	jmp	.LBB1_36
.LBB1_18:                               # %sw.bb7.i
	cmpq	$82, %rax
	jne	.LBB1_32
# BB#19:                                # %sw.bb1.i.i
	movabsq	$.L.str25, %rsi
	movq	$11, %rdx
	jmp	.LBB1_14
.LBB1_22:                               # %sw.bb7.i
	cmpq	$108, %rax
	jne	.LBB1_32
# BB#23:                                # %sw.bb15.i.i
	movabsq	$.L.str28, %rsi
	movq	$11, %rdx
.LBB1_14:                               # %sw.bb1.i
	movq	%r15, %rdi
	callq	memcmp
	xorq	%rcx, %rcx
	testq	%rbx, %rax
	je	.LBB1_36
.LBB1_32:                               # %sw.bb
	movabsq	$.L.str1, %rdi
.LBB1_33:                               # %sw.epilog
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_newSVpvf
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	$1, %rcx
	subq	%r13, %rcx
	leaq	(%r12,%rcx,8), %r14
	movq	%rax, (%r12,%rcx,8)
.LBB1_41:                               # %sw.epilog
	movq	%r14, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_36:                               # %sw.bb25
	movq	%rcx, %r15
	movq	PL_stack_max(%rip), %rax
	subq	%r14, %rax
	cmpq	$7, %rax
	movq	(%rsp), %rbx            # 8-byte Reload
	jg	.LBB1_38
# BB#37:                                # %if.then32
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r14
.LBB1_38:                               # %if.end34
	movq	%r15, %rsi
	movq	$PL_sv_undef, 8(%r14)
	movq	%rbx, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%rbx), %eax
	testq	$64, %rax
	je	.LBB1_40
# BB#39:                                # %if.then39
	movq	%rbx, %rdi
	callq	Perl_mg_set
.LBB1_40:                               # %if.end41
	movq	%rbx, 16(%r14)
	addq	$16, %r14
	jmp	.LBB1_41
.Ltmp20:
	.size	XS_Time__HiRes_constant, .Ltmp20-XS_Time__HiRes_constant
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_10
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_16
	.quad	.LBB1_32
	.quad	.LBB1_32
	.quad	.LBB1_26
	.quad	.LBB1_31

	.text
	.globl	XS_Time__HiRes_usleep
	.align	16, 0x90
	.type	XS_Time__HiRes_usleep,@function
XS_Time__HiRes_usleep:                  # @XS_Time__HiRes_usleep
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
	subq	$64, %rsp
.Ltmp32:
	.cfi_def_cfa_offset 112
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	subq	%rdx, %rbx
	cmpq	$8, %rbx
	je	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB2_2:                                # %if.end
	movq	8(%rax,%r12,8), %rdi
	movzbl	18(%rdi), %eax
	leaq	1(%r12), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	$2, %rax
	je	.LBB2_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	32(%rax), %r15
	jmp	.LBB2_5
.LBB2_4:                                # %cond.false
	callq	Perl_sv_2nv
	movq	%rax, %r15
.LBB2_5:                                # %cond.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	testq	$32, %rcx
	je	.LBB2_7
# BB#6:                                 # %cond.true12
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r13
	jmp	.LBB2_8
.LBB2_7:                                # %cond.false14
	callq	Perl_sv_newmortal
	movq	%rax, %r13
.LBB2_8:                                # %cond.end16
	leaq	32(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	testq	%rbx, %rbx
	jle	.LBB2_23
# BB#9:                                 # %if.then21
	movq	%r13, (%rsp)            # 8-byte Spill
	movabsq	$4696837146684686336, %rbx # imm = 0x412E848000000000
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r14
	movabsq	$1, %rax
	movq	%r14, %r13
	je	.LBB2_11
# BB#10:                                # %if.then21
	movq	%rax, %r13
.LBB2_11:                               # %if.then21
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB2_13
# BB#12:                                # %if.then21
	movabsq	$1, %r14
.LBB2_13:                               # %if.then21
	orq	%r14, %r13
	jne	.LBB2_16
# BB#14:                                # %if.then24
	movabsq	$4696837146684686336, %rsi # imm = 0x412E848000000000
	movq	%r15, %rdi
	callq	float64_div
	movq	%rax, %rdi
	callq	float64_to_int64
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB2_22
# BB#15:                                # %if.then27
	movq	%rbx, %rdi
	callq	sleep
	movq	%rbx, %rdi
	callq	int64_to_float64
	movabsq	$-4526534890170089472, %rsi # imm = 0xC12E848000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r15
	jmp	.LBB2_22
.LBB2_23:                               # %if.else38
	movq	$2147450879, %rdi       # imm = 0x7FFF7FFF
	callq	sleep
	jmp	.LBB2_24
.LBB2_16:                               # %if.else
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r14
	movabsq	$1, %r13
	movq	%r14, %rbx
	je	.LBB2_18
# BB#17:                                # %if.else
	movq	%r13, %rbx
.LBB2_18:                               # %if.else
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_20
# BB#19:                                # %if.else
	movq	%r13, %r14
.LBB2_20:                               # %if.else
	orq	%r14, %rbx
	jne	.LBB2_22
# BB#21:                                # %if.then34
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_croak
.LBB2_22:                               # %if.end36
	movq	(%rsp), %r13            # 8-byte Reload
	movq	%r15, %rdi
	callq	float64_to_int64
	movq	$0, 48(%rsp)
	movq	%rax, 56(%rsp)
	leaq	48(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	select
.LBB2_24:                               # %if.end40
	leaq	16(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	movq	16(%rsp), %rdi
	movq	24(%rsp), %rbx
	movq	32(%rsp), %rax
	movq	40(%rsp), %r14
	subq	%rax, %rdi
	callq	int64_to_float64
	movabsq	$4696837146684686336, %rsi # imm = 0x412E848000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %r15
	subq	%r14, %rbx
	movq	%rbx, %rdi
	callq	int64_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	PL_stack_base(%rip), %rbx
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setnv
	movzbl	17(%r13), %eax
	testq	$64, %rax
	je	.LBB2_26
# BB#25:                                # %if.then55
	movq	%r13, %rdi
	callq	Perl_mg_set
.LBB2_26:                               # %if.end57
	movq	%r13, 8(%rbx,%r12,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	XS_Time__HiRes_usleep, .Ltmp38-XS_Time__HiRes_usleep
	.cfi_endproc

	.globl	XS_Time__HiRes_sleep
	.align	16, 0x90
	.type	XS_Time__HiRes_sleep,@function
XS_Time__HiRes_sleep:                   # @XS_Time__HiRes_sleep
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
	subq	$80, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 128
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	movq	PL_stack_base(%rip), %rcx
	leaq	-8(%rax), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rax), %r13
	leaq	(%rcx,%r13,8), %rdx
	movq	PL_op(%rip), %rax
	movq	$3, %rcx
	shlq	%cl, %r13
	movq	$3, %rcx
	movzbl	37(%rax), %esi
	sarq	%cl, %r13
	subq	%rdx, %rbx
	leaq	1(%r13), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	$32, %rsi
	je	.LBB3_2
# BB#1:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB3_3
.LBB3_2:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB3_3:                                # %cond.end
	leaq	48(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	testq	%rbx, %rbx
	jle	.LBB3_22
# BB#4:                                 # %if.then
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r13,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$2, %rax
	je	.LBB3_6
# BB#5:                                 # %cond.true11
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	32(%rax), %r12
	jmp	.LBB3_7
.LBB3_22:                               # %if.else41
	movq	$2147450879, %rdi       # imm = 0x7FFF7FFF
	callq	sleep
	jmp	.LBB3_23
.LBB3_6:                                # %cond.false14
	movq	%r14, 16(%rsp)          # 8-byte Spill
	callq	Perl_sv_2nv
	movq	%rax, %r12
.LBB3_7:                                # %cond.end18
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r14
	movq	%r15, %rbx
	je	.LBB3_9
# BB#8:                                 # %cond.end18
	movq	%r14, %rbx
.LBB3_9:                                # %cond.end18
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB3_11
# BB#10:                                # %cond.end18
	movq	%r14, %r15
.LBB3_11:                               # %cond.end18
	orq	%r15, %rbx
	je	.LBB3_12
# BB#21:                                # %if.else
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	Perl_croak
	movq	16(%rsp), %r14          # 8-byte Reload
	jmp	.LBB3_23
.LBB3_12:                               # %if.then22
	movq	%r12, %rdi
	callq	float64_to_int64
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rax, %rdi
	callq	int64_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movabsq	$4696837146684686336, %rsi # imm = 0x412E848000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4607182418800017408, %rsi # imm = 0x3FF0000000000000
	movq	%r12, %rdi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$1, %rax
	movq	%rbx, %r15
	movq	16(%rsp), %r14          # 8-byte Reload
	je	.LBB3_14
# BB#13:                                # %if.then22
	movq	%rax, %r15
.LBB3_14:                               # %if.then22
	movq	%r12, %rdi
	movabsq	$4607182418800017408, %rsi # imm = 0x3FF0000000000000
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB3_16
# BB#15:                                # %if.then22
	movabsq	$1, %rbx
.LBB3_16:                               # %if.then22
	orq	%rbx, %r15
	jne	.LBB3_18
# BB#17:                                # %if.then28
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	sleep
.LBB3_18:                               # %if.end
	movq	8(%rsp), %rbx           # 8-byte Reload
	testq	%rbx, %rbx
	jns	.LBB3_20
# BB#19:                                # %if.then37
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	movq	%rbx, %rcx
	callq	Perl_croak
.LBB3_20:                               # %if.end39
	movq	$0, 64(%rsp)
	movq	%rbx, 72(%rsp)
	leaq	64(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	select
.LBB3_23:                               # %if.end43
	leaq	32(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	movq	32(%rsp), %rdi
	movq	40(%rsp), %rbx
	movq	48(%rsp), %rax
	movq	56(%rsp), %r12
	subq	%rax, %rdi
	callq	int64_to_float64
	movq	%rax, %r15
	subq	%r12, %rbx
	movq	%rbx, %rdi
	callq	int64_to_float64
	movabsq	$4517329193108106637, %rsi # imm = 0x3EB0C6F7A0B5ED8D
	movq	%rax, %rdi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	PL_stack_base(%rip), %rbx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setnv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB3_25
# BB#24:                                # %if.then58
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB3_25:                               # %if.end60
	movq	%r14, 8(%rbx,%r13,8)
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	XS_Time__HiRes_sleep, .Ltmp56-XS_Time__HiRes_sleep
	.cfi_endproc

	.globl	XS_Time__HiRes_gettimeofday
	.align	16, 0x90
	.type	XS_Time__HiRes_gettimeofday,@function
XS_Time__HiRes_gettimeofday:            # @XS_Time__HiRes_gettimeofday
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 64
.Ltmp67:
	.cfi_offset %rbx, -40
.Ltmp68:
	.cfi_offset %r12, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r12
	movq	PL_stack_base(%rip), %rcx
	leaq	-8(%rax), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rax), %rax
	leaq	(%rcx,%rax,8), %rax
	movq	%r12, %rbx
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	je	.LBB4_2
# BB#1:                                 # %if.then
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB4_2:                                # %if.end
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r12
	leaq	8(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %eax
	andq	$3, %rax
	je	.LBB4_6
# BB#3:                                 # %cond.true
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB4_4
# BB#5:                                 # %cond.true
	movabsq	$1, %rax
	jmp	.LBB4_7
.LBB4_6:                                # %cond.false
	callq	Perl_dowantarray
	jmp	.LBB4_7
.LBB4_4:
	movabsq	$0, %rax
.LBB4_7:                                # %cond.end
	movq	PL_stack_max(%rip), %rcx
	subq	%r12, %rcx
	cmpq	$1, %rax
	jne	.LBB4_11
# BB#8:                                 # %if.then16
	cmpq	$15, %rcx
	jg	.LBB4_10
# BB#9:                                 # %if.then23
	movq	$2, %rdx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r12
.LBB4_10:                               # %if.end25
	movq	8(%rsp), %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r12)
	movq	16(%rsp), %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 16(%r12)
	addq	$16, %r12
	jmp	.LBB4_14
.LBB4_11:                               # %if.else
	cmpq	$7, %rcx
	jg	.LBB4_13
# BB#12:                                # %if.then38
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r12
.LBB4_13:                               # %if.end40
	movq	8(%rsp), %rdi
	movq	16(%rsp), %r14
	callq	int64_to_float64
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	int64_to_float64
	movabsq	$4696837146684686336, %rsi # imm = 0x412E848000000000
	movq	%rax, %rdi
	callq	float64_div
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	Perl_newSVnv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r12)
	addq	$8, %r12
.LBB4_14:                               # %if.end49
	movq	%r12, PL_stack_sp(%rip)
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	XS_Time__HiRes_gettimeofday, .Ltmp71-XS_Time__HiRes_gettimeofday
	.cfi_endproc

	.globl	XS_Time__HiRes_time
	.align	16, 0x90
	.type	XS_Time__HiRes_time,@function
XS_Time__HiRes_time:                    # @XS_Time__HiRes_time
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
	je	.LBB5_2
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB5_2:                                # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	leaq	1(%rbx), %r13
	testq	$32, %rcx
	je	.LBB5_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB5_5
.LBB5_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB5_5:                                # %cond.end
	leaq	(%rsp), %rdi
	xorq	%rsi, %rsi
	callq	gettimeofday
	movq	(%rsp), %rdi
	movq	8(%rsp), %r12
	callq	int64_to_float64
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	int64_to_float64
	movabsq	$4696837146684686336, %rsi # imm = 0x412E848000000000
	movq	%rax, %rdi
	callq	float64_div
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	PL_stack_base(%rip), %r15
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setnv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB5_7
# BB#6:                                 # %if.then13
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB5_7:                                # %if.end15
	movq	%r14, 8(%r15,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	XS_Time__HiRes_time, .Ltmp89-XS_Time__HiRes_time
	.cfi_endproc

	.globl	boot_Time__HiRes
	.align	16, 0x90
	.type	boot_Time__HiRes,@function
boot_Time__HiRes:                       # @boot_Time__HiRes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp94:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp95:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp96:
	.cfi_def_cfa_offset 32
.Ltmp97:
	.cfi_offset %rbx, -32
.Ltmp98:
	.cfi_offset %r14, -24
.Ltmp99:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movabsq	$.L.str11, %rdi
	movabsq	$XS_Time__HiRes_constant, %rsi
	movabsq	$.L.str10, %r14
	movq	%r14, %rdx
	callq	Perl_newXS
	movabsq	$.L.str12, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str13, %rdi
	movabsq	$XS_Time__HiRes_usleep, %rsi
	movq	%r14, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str14, %rdi
	movabsq	$XS_Time__HiRes_sleep, %rsi
	movq	%r14, %rdx
	callq	Perl_newXS
	movabsq	$.L.str15, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str16, %rdi
	movabsq	$XS_Time__HiRes_gettimeofday, %rsi
	movq	%r14, %rdx
	callq	Perl_newXS
	movabsq	$.L.str17, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str18, %rdi
	movabsq	$XS_Time__HiRes_time, %rsi
	movq	%r14, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%rbx, %rsi
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
.Ltmp100:
	.size	boot_Time__HiRes, .Ltmp100-boot_Time__HiRes
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Time::HiRes::constant(sv)"
	.size	.L.str, 33

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"%s is not a valid Time::HiRes macro"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Your vendor has not defined Time::HiRes macro %s, used"
	.size	.L.str2, 55

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Usage: Time::HiRes::usleep(useconds)"
	.size	.L.str4, 37

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Time::HiRes::usleep(%g): negative time not invented yet"
	.size	.L.str5, 56

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Time::HiRes::sleep(%g): internal error: useconds < 0 (unsigned %lu signed %ld)"
	.size	.L.str6, 79

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Time::HiRes::sleep(%g): negative time not invented yet"
	.size	.L.str7, 55

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Usage: Time::HiRes::gettimeofday()"
	.size	.L.str8, 35

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Usage: Time::HiRes::time()"
	.size	.L.str9, 27

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"HiRes.c"
	.size	.L.str10, 8

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Time::HiRes::constant"
	.size	.L.str11, 22

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"$"
	.size	.L.str12, 2

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Time::HiRes::usleep"
	.size	.L.str13, 20

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Time::HiRes::sleep"
	.size	.L.str14, 19

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	";@"
	.size	.L.str15, 3

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Time::HiRes::gettimeofday"
	.size	.L.str16, 26

	.type	.L.str17,@object        # @.str17
.L.str17:
	.zero	1
	.size	.L.str17, 1

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Time::HiRes::time"
	.size	.L.str18, 18

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"d_ualar"
	.size	.L.str19, 8

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"d_uslee"
	.size	.L.str20, 8

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"ITIMER_VIRTUAL"
	.size	.L.str21, 15

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"d_gettimeofday"
	.size	.L.str22, 15

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"ITIMER_REALPROF"
	.size	.L.str23, 16

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"ITIMER_PROF"
	.size	.L.str24, 12

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"ITIMER_REAL"
	.size	.L.str25, 12

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"d_getitimer"
	.size	.L.str26, 12

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"d_setitimer"
	.size	.L.str27, 12

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"d_nanosleep"
	.size	.L.str28, 12


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
