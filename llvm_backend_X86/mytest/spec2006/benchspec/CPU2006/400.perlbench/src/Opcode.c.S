	.file	"Opcode.c.bc"
	.text
	.globl	XS_Opcode__safe_pkg_prep
	.align	16, 0x90
	.type	XS_Opcode__safe_pkg_prep,@function
XS_Opcode__safe_pkg_prep:               # @XS_Opcode__safe_pkg_prep
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r15
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	movq	%r15, %rbx
	subq	%rdx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$1, %rbx
	je	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB0_2:                                # %if.end
	movq	8(%rax,%r14,8), %rdi
	movzbl	18(%rdi), %eax
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r15
	testq	$4, %rax
	je	.LBB0_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	(%rax), %rbx
	jmp	.LBB0_5
.LBB0_4:                                # %cond.false
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rbx
.LBB0_5:                                # %cond.end
	callq	Perl_push_scope
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	(%r14), %rax
	movq	80(%rax), %rbx
	movabsq	$.L.str1, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_8
# BB#6:                                 # %if.then15
	movq	%rbx, %rdi
	callq	Perl_safesysfree
	movabsq	$.L.str1, %rdi
	callq	Perl_savepv
	movq	(%r14), %rcx
	movq	%rax, 80(%rcx)
	movq	PL_defgv(%rip), %rcx
	movabsq	$.L.str2, %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	Perl_hv_store
	movq	PL_defgv(%rip), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB0_8
# BB#7:                                 # %land.rhs
	incq	8(%rax)
.LBB0_8:                                # %if.end24
	callq	Perl_pop_scope
	movq	%r15, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	XS_Opcode__safe_pkg_prep, .Ltmp10-XS_Opcode__safe_pkg_prep
	.cfi_endproc

	.globl	XS_Opcode__safe_call_sv
	.align	16, 0x90
	.type	XS_Opcode__safe_call_sv,@function
XS_Opcode__safe_call_sv:                # @XS_Opcode__safe_call_sv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$464, %rsp              # imm = 0x1D0
.Ltmp22:
	.cfi_def_cfa_offset 512
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r13
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rdx
	leaq	(%rdx,%r14,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	movq	%r13, %rbx
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$3, %rbx
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rdx
.LBB1_2:                                # %if.end
	movq	8(%rdx,%r14,8), %rdi
	movzbl	18(%rdi), %eax
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r13
	testq	$4, %rax
	je	.LBB1_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	(%rax), %r15
	jmp	.LBB1_5
.LBB1_4:                                # %cond.false
	callq	Perl_sv_2pv_nolen
	movq	PL_stack_base(%rip), %rdx
	movq	%rax, %r15
.LBB1_5:                                # %cond.end
	movq	16(%rdx,%r14,8), %r12
	movq	24(%rdx,%r14,8), %r14
	callq	Perl_push_scope
	movq	PL_op_mask(%rip), %rbx
	movabsq	$PL_op_mask, %rdi
	callq	Perl_save_vptr
	movslq	PL_maxo(%rip), %rdx
	leaq	(%rsp), %rax
	movq	%rax, PL_op_mask(%rip)
	leaq	(%rsp), %rdi
	testq	%rbx, %rbx
	je	.LBB1_7
# BB#6:                                 # %if.then1.i
	movq	%rbx, %rsi
	callq	memcpy
	jmp	.LBB1_8
.LBB1_7:                                # %if.else.i
	xorq	%rsi, %rsi
	callq	memset
.LBB1_8:                                # %opmask_addlocal.exit
	movq	%r12, %rdi
	callq	opmask_add
	movabsq	$PL_endav, %rdi
	callq	Perl_save_aptr
	callq	Perl_newAV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, PL_endav(%rip)
	movabsq	$PL_defstash, %rdi
	callq	Perl_save_hptr
	movq	$4, %rsi
	movq	%r15, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, PL_defstash(%rip)
	movabsq	$PL_curstash, %rdi
	callq	Perl_save_hptr
	movq	PL_defstash(%rip), %rax
	movq	%rax, PL_curstash(%rip)
	movabsq	$.L.str4, %rdi
	movq	$4, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rdi
	callq	Perl_sv_free
	movq	PL_defstash(%rip), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB1_10
# BB#9:                                 # %land.rhs
	incq	8(%rax)
.LBB1_10:                               # %land.end
	movq	(%rbx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	PL_incgv(%rip), %rdi
	callq	Perl_save_hash
	movq	%rax, %rbx
	movabsq	$.L.str5, %rdi
	movq	$1, %rsi
	movq	$11, %rdx
	callq	Perl_gv_fetchpv
	movq	%rax, %rdi
	callq	Perl_gv_HVadd
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB1_12
# BB#11:                                # %land.rhs32
	incq	8(%rax)
.LBB1_12:                               # %land.end36
	movq	PL_incgv(%rip), %rcx
	movq	(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rcx
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rax
	jne	.LBB1_14
# BB#13:                                # %if.then43
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB1_14:                               # %if.end44
	movq	PL_stack_base(%rip), %rcx
	subq	%rcx, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	movq	%r13, (%rax)
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %eax
	andq	$3, %rax
	je	.LBB1_18
# BB#15:                                # %cond.true51
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB1_16
# BB#17:                                # %cond.true51
	movabsq	$1, %rsi
	jmp	.LBB1_19
.LBB1_18:                               # %cond.false59
	callq	Perl_dowantarray
	movq	%rax, %rsi
	jmp	.LBB1_19
.LBB1_16:
	movabsq	$0, %rsi
.LBB1_19:                               # %cond.end61
	orq	$20, %rsi
	movq	%r14, %rdi
	callq	Perl_call_sv
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	PL_stack_sp(%rip), %rbx
	callq	Perl_pop_scope
	movq	%rbx, PL_stack_sp(%rip)
	addq	$464, %rsp              # imm = 0x1D0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	XS_Opcode__safe_call_sv, .Ltmp28-XS_Opcode__safe_call_sv
	.cfi_endproc

	.globl	XS_Opcode_verify_opset
	.align	16, 0x90
	.type	XS_Opcode_verify_opset,@function
XS_Opcode_verify_opset:                 # @XS_Opcode_verify_opset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -48
.Ltmp41:
	.cfi_offset %r12, -40
.Ltmp42:
	.cfi_offset %r13, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %r14
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	movq	%r14, %rcx
	negq	%rcx
	leaq	-8(%rbx,%rcx), %rcx
	cmpq	$9, %rcx
	jb	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB2_2:                                # %if.end
	movq	PL_op(%rip), %rcx
	movzbl	37(%rcx), %edx
	movq	8(%rax,%r15,8), %r13
	subq	%r14, %rbx
	leaq	1(%r15), %r12
	testq	$32, %rdx
	je	.LBB2_4
# BB#3:                                 # %cond.true
	movq	24(%rcx), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB2_5
.LBB2_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB2_5:                                # %cond.end
	xorq	%rax, %rax
	cmpq	$16, %rbx
	jl	.LBB2_9
# BB#6:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r15,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB2_8
# BB#7:                                 # %cond.true15
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB2_9
.LBB2_8:                                # %cond.false18
	callq	Perl_sv_2iv
.LBB2_9:                                # %if.end25
	movq	16(%r13), %rcx
	testq	$118423552, %rcx        # imm = 0x70F0000
	je	.LBB2_10
# BB#11:                                # %if.else.i
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB2_12
# BB#13:                                # %if.else5.i
	movq	(%r13), %rcx
	movq	8(%rcx), %rcx
	movq	my_cxt.2(%rip), %rdx
	xorq	%rbx, %rbx
	cmpq	%rdx, %rcx
	je	.LBB2_15
# BB#14:                                # %select.mid
	movabsq	$.L.str63, %rbx
	jmp	.LBB2_15
.LBB2_10:
	movabsq	$.L.str61, %rbx
	jmp	.LBB2_15
.LBB2_12:
	movabsq	$.L.str62, %rbx
.LBB2_15:                               # %if.end8.i
	testq	%rbx, %rbx
	je	.LBB2_18
# BB#16:                                # %if.end8.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_18
# BB#17:                                # %if.then11.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
.LBB2_18:                               # %verify_opset.exit
	testq	%rbx, %rbx
	jne	.LBB2_19
# BB#20:                                # %verify_opset.exit
	movabsq	$1, %rsi
	jmp	.LBB2_21
.LBB2_19:
	movabsq	$0, %rsi
.LBB2_21:                               # %verify_opset.exit
	movq	PL_stack_base(%rip), %rbx
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB2_23
# BB#22:                                # %if.then33
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB2_23:                               # %if.end35
	movq	%r14, 8(%rbx,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	XS_Opcode_verify_opset, .Ltmp45-XS_Opcode_verify_opset
	.cfi_endproc

	.globl	XS_Opcode_invert_opset
	.align	16, 0x90
	.type	XS_Opcode_invert_opset,@function
XS_Opcode_invert_opset:                 # @XS_Opcode_invert_opset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp51:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp52:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp55:
	.cfi_def_cfa_offset 48
.Ltmp56:
	.cfi_offset %rbx, -40
.Ltmp57:
	.cfi_offset %r12, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
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
	je	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB3_2:                                # %if.end
	movq	8(%rax,%r15,8), %rbx
	movq	my_cxt.2(%rip), %r14
	leaq	1(%r15), %r12
	testq	%rbx, %rbx
	je	.LBB3_11
# BB#3:                                 # %if.then.i
	movq	16(%rbx), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB3_4
# BB#5:                                 # %if.else.i.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB3_6
# BB#7:                                 # %if.else5.i.i
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	cmpq	%r14, %rax
	je	.LBB3_10
# BB#8:
	movabsq	$.L.str63, %rsi
	jmp	.LBB3_9
.LBB3_11:                               # %if.else.i
	movq	%r14, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	jmp	.LBB3_12
.LBB3_4:
	movabsq	$.L.str61, %rsi
	jmp	.LBB3_9
.LBB3_6:
	movabsq	$.L.str62, %rsi
.LBB3_9:                                # %if.then11.i.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_10:                               # %verify_opset.exit.i
	movq	%rbx, %rdi
	callq	Perl_newSVsv
	movq	%rax, %rbx
.LBB3_12:                               # %new_opset.exit
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movq	(%rax), %rcx
	movq	(%rcx), %r9
	testq	%r14, %r14
	je	.LBB3_19
# BB#13:                                # %while.body.preheader
	movq	%r14, %rsi
	andq	$-4, %rsi
	xorq	%rdi, %rdi
	movq	%r14, %r8
	andq	$-4, %r8
	movq	%r14, %r10
	je	.LBB3_17
# BB#14:                                # %vector.body.preheader
	movq	%r14, %r10
	subq	%rsi, %r10
	leaq	-4(%r14,%r9), %rdi
	movq	%r8, %rsi
	.align	16, 0x90
.LBB3_15:                               # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rdi), %ebx
	movzbl	2(%rdi), %edx
	movzbl	1(%rdi), %ecx
	movzbl	(%rdi), %r11d
	notq	%r11
	notq	%rcx
	notq	%rdx
	notq	%rbx
	movb	%bl, 3(%rdi)
	movb	%dl, 2(%rdi)
	movb	%cl, 1(%rdi)
	movb	%r11b, (%rdi)
	addq	$-4, %rdi
	addq	$-4, %rsi
	jne	.LBB3_15
# BB#16:
	movq	%r8, %rdi
.LBB3_17:                               # %middle.block
	cmpq	%rdi, %r14
	je	.LBB3_19
	.align	16, 0x90
.LBB3_18:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r9,%r10), %ecx
	movq	%r10, %rdx
	decq	%rdx
	notq	%rcx
	movb	%cl, -1(%r9,%r10)
	movq	%rdx, %r10
	jne	.LBB3_18
.LBB3_19:                               # %while.end
	movl	PL_maxo(%rip), %ecx
	andq	$7, %rcx
	je	.LBB3_21
# BB#20:                                # %if.then11
	movq	my_cxt.2(%rip), %rdx
	movzbl	-1(%rdx,%r9), %esi
	movq	$255, %rdi
	shlq	%cl, %rdi
	notq	%rdi
	andq	%rdi, %rsi
	movb	%sil, -1(%rdx,%r9)
.LBB3_21:                               # %if.end18
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp60:
	.size	XS_Opcode_invert_opset, .Ltmp60-XS_Opcode_invert_opset
	.cfi_endproc

	.globl	XS_Opcode_opset_to_ops
	.align	16, 0x90
	.type	XS_Opcode_opset_to_ops,@function
XS_Opcode_opset_to_ops:                 # @XS_Opcode_opset_to_ops
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
	subq	$32, %rsp
.Ltmp72:
	.cfi_def_cfa_offset 80
.Ltmp73:
	.cfi_offset %rbx, -48
.Ltmp74:
	.cfi_offset %r12, -40
.Ltmp75:
	.cfi_offset %r13, -32
.Ltmp76:
	.cfi_offset %r14, -24
.Ltmp77:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r12
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rsi
	leaq	(%rsi,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r12, %r15
	subq	%rdx, %r15
	negq	%rdx
	movq	$3, %rcx
	movq	%r15, %r13
	sarq	%cl, %r13
	leaq	-8(%r12,%rdx), %rcx
	cmpq	$9, %rcx
	jb	.LBB4_2
# BB#1:                                 # %if.then
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rsi
.LBB4_2:                                # %if.end
	movq	$3, %rcx
	movq	8(%rsi,%rbx,8), %r14
	shlq	%cl, %r13
	xorq	%rax, %rax
	cmpq	$16, %r15
	jl	.LBB4_6
# BB#3:                                 # %if.else
	movq	16(%rsi,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB4_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB4_6
.LBB4_5:                                # %cond.false
	callq	Perl_sv_2iv
.LBB4_6:                                # %if.end16
	movq	%rax, %rbx
	movzbl	18(%r14), %eax
	subq	%r13, %r12
	testq	$4, %rax
	je	.LBB4_8
# BB#7:                                 # %cond.true21
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rsp)
	movq	(%rax), %rax
	jmp	.LBB4_9
.LBB4_8:                                # %cond.false24
	leaq	24(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
.LBB4_9:                                # %cond.end26
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB4_11
# BB#10:                                # %cond.true29
	callq	Perl_get_op_descs
	jmp	.LBB4_12
.LBB4_11:                               # %cond.false31
	callq	Perl_get_op_names
.LBB4_12:                               # %cond.end33
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	16(%r14), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB4_13
# BB#14:                                # %if.else.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB4_15
# BB#16:                                # %if.else5.i
	movq	(%r14), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB4_19
# BB#17:
	movabsq	$.L.str63, %rsi
	jmp	.LBB4_18
.LBB4_13:
	movabsq	$.L.str61, %rsi
	jmp	.LBB4_18
.LBB4_15:
	movabsq	$.L.str62, %rsi
.LBB4_18:                               # %if.then11.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	my_cxt.2(%rip), %rax
.LBB4_19:                               # %for.cond.preheader
	testq	%rax, %rax
	jle	.LBB4_30
# BB#20:                                # %for.body.lr.ph
	movl	PL_maxo(%rip), %eax
	xorq	%r13, %r13
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB4_21:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_23 Depth 2
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jge	.LBB4_29
# BB#22:                                # %for.body46.lr.ph
                                        #   in Loop: Header=BB4_21 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movsbq	(%rcx,%r13), %r14
	andq	$65535, %r14            # imm = 0xFFFF
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rdx
	xorq	%rbx, %rbx
	movq	%rdx, %r15
	.align	16, 0x90
.LBB4_23:                               # %for.body46
                                        #   Parent Loop BB4_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %r14
	je	.LBB4_27
# BB#24:                                # %if.then50
                                        #   in Loop: Header=BB4_23 Depth=2
	movq	PL_stack_max(%rip), %rax
	subq	%r12, %rax
	cmpq	$7, %rax
	jg	.LBB4_26
# BB#25:                                # %if.then57
                                        #   in Loop: Header=BB4_23 Depth=2
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r12
.LBB4_26:                               # %if.end59
                                        #   in Loop: Header=BB4_23 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	-8(%rax,%r15,8), %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r12)
	movl	PL_maxo(%rip), %eax
	addq	$8, %r12
.LBB4_27:                               # %for.inc
                                        #   in Loop: Header=BB4_23 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$7, %rdx
	movq	%r15, %rdx
	jg	.LBB4_29
# BB#28:                                # %for.inc
                                        #   in Loop: Header=BB4_23 Depth=2
	incq	%r15
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB4_23
.LBB4_29:                               # %for.inc67
                                        #   in Loop: Header=BB4_21 Depth=1
	movq	my_cxt.2(%rip), %rcx
	incq	%r13
	cmpq	%rcx, %r13
	jl	.LBB4_21
.LBB4_30:                               # %for.end69
	movq	%r12, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp78:
	.size	XS_Opcode_opset_to_ops, .Ltmp78-XS_Opcode_opset_to_ops
	.cfi_endproc

	.globl	XS_Opcode_opset
	.align	16, 0x90
	.type	XS_Opcode_opset,@function
XS_Opcode_opset:                        # @XS_Opcode_opset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp87:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp88:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp89:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp90:
	.cfi_def_cfa_offset 80
.Ltmp91:
	.cfi_offset %rbx, -48
.Ltmp92:
	.cfi_offset %r12, -40
.Ltmp93:
	.cfi_offset %r13, -32
.Ltmp94:
	.cfi_offset %r14, -24
.Ltmp95:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_sp(%rip), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	$3, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	my_cxt.2(%rip), %rdi
	subq	%rax, %r12
	movq	$3, %rcx
	movq	%r12, %rax
	sarq	%cl, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%r12, %r12
	jle	.LBB5_13
# BB#1:                                 # %for.body.preheader
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	movq	(%rax), %r12
	leaq	8(,%r15,8), %r15
	.align	16, 0x90
.LBB5_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	addq	%r15, %rax
	movq	(%rax,%r14,8), %rsi
	movq	16(%rsi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB5_5
# BB#3:                                 # %if.else5.i
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	(%rsi), %rcx
	movq	8(%rcx), %rcx
	movq	my_cxt.2(%rip), %rdx
	cmpq	%rdx, %rcx
	movq	$1, %r13
	jne	.LBB5_5
# BB#4:                                 #   in Loop: Header=BB5_2 Depth=1
	movabsq	$.L.str9, %rbx
	jmp	.LBB5_12
	.align	16, 0x90
.LBB5_5:                                # %if.else
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB5_6
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	$2, %rdx
	movq	%rsi, %rdi
	leaq	24(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
	jmp	.LBB5_8
	.align	16, 0x90
.LBB5_6:                                # %cond.true
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	(%rsi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rsp)
	movq	(%rax), %rbx
.LBB5_8:                                # %cond.end
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB5_9
# BB#10:                                # %if.then34
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	24(%rsp), %rsi
	incq	%rbx
	decq	%rsi
	movq	%rsi, 24(%rsp)
	xorq	%r13, %r13
	jmp	.LBB5_11
	.align	16, 0x90
.LBB5_9:                                # %cond.end.if.end_crit_edge
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	24(%rsp), %rsi
	movq	$1, %r13
.LBB5_11:                               # %if.end
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rbx, %rdi
	callq	get_op_bitspec
	movq	%rax, %rsi
.LBB5_12:                               # %if.end37
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	set_opset_bits
	incq	%r14
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jl	.LBB5_2
.LBB5_13:                               # %for.end
	movq	PL_stack_base(%rip), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, 8(%rax,%rcx,8)
	movq	PL_stack_base(%rip), %rax
	incq	%rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp96:
	.size	XS_Opcode_opset, .Ltmp96-XS_Opcode_opset
	.cfi_endproc

	.align	16, 0x90
	.type	get_op_bitspec,@function
get_op_bitspec:                         # @get_op_bitspec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp100:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp102:
	.cfi_def_cfa_offset 32
.Ltmp103:
	.cfi_offset %rbx, -24
.Ltmp104:
	.cfi_offset %r14, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	jne	.LBB6_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	strlen
.LBB6_2:                                # %if.end
	movq	my_cxt.0(%rip), %rdi
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_fetch
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB6_4
# BB#3:                                 # %lor.lhs.false
	movq	(%r14), %rax
	movzbl	18(%rax), %ecx
	testq	$1807, %rcx             # imm = 0x70F
	jne	.LBB6_12
.LBB6_4:                                # %if.end7
	movzbl	(%rbx), %eax
	cmpq	$58, %rax
	jne	.LBB6_6
# BB#5:                                 # %if.then9
	movabsq	$.L.str57, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
	movzbl	(%rbx), %eax
.LBB6_6:                                # %if.end10
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$33, %rcx
	jne	.LBB6_8
# BB#7:                                 # %if.then14
	movabsq	$.L.str58, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
	movzbl	(%rbx), %eax
.LBB6_8:                                # %if.end15
	leaq	-65(%rax), %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jb	.LBB6_10
# BB#9:                                 # %if.end15
	addq	$-97, %rax
	andq	$255, %rax
	cmpq	$25, %rax
	ja	.LBB6_11
.LBB6_10:                               # %if.then30
	movabsq	$.L.str59, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
.LBB6_11:                               # %if.end31
	movabsq	$.L.str60, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	Perl_croak
	movq	(%r14), %rax
.LBB6_12:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp105:
	.size	get_op_bitspec, .Ltmp105-get_op_bitspec
	.cfi_endproc

	.align	16, 0x90
	.type	set_opset_bits,@function
set_opset_bits:                         # @set_opset_bits
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
	subq	$16, %rsp
.Ltmp117:
	.cfi_def_cfa_offset 64
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
	movq	16(%rsi), %rbx
	movq	%rcx, %r8
	movq	%rdx, %r15
	movq	%rdi, %r14
	testq	$65536, %rbx            # imm = 0x10000
	je	.LBB7_6
# BB#1:                                 # %cond.true
	movq	(%rsi), %rax
	movq	24(%rax), %rdx
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	sarq	%cl, %rax
	movslq	PL_maxo(%rip), %rsi
	movq	$35, %rcx
	sarq	%cl, %rbx
	movq	%rdx, %r12
	andq	$7, %r12
	cmpq	%rsi, %rax
	jge	.LBB7_3
# BB#2:                                 # %cond.true
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB7_4
.LBB7_3:                                # %if.then8
	movabsq	$.L.str52, %rdi
	xorq	%rax, %rax
	movq	%r8, %rsi
	callq	Perl_croak
.LBB7_4:                                # %if.end14
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r15
	je	.LBB7_28
# BB#5:                                 # %if.then16
	movzbl	(%r14,%rbx), %ecx
	orq	%rax, %rcx
	jmp	.LBB7_29
.LBB7_6:                                # %if.else26
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB7_27
# BB#7:                                 # %land.lhs.true
	movq	(%rsi), %rax
	movq	8(%rax), %rdx
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB7_27
# BB#8:                                 # %if.end51
	movq	(%rax), %r10
	leaq	-1(%rdx), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r15
	je	.LBB7_18
# BB#9:                                 # %while.cond.preheader
	testq	%rdx, %rdx
	je	.LBB7_30
# BB#10:                                # %while.body.preheader
	xorq	%rsi, %rsi
	movq	%rdx, %rax
	andq	$-4, %rax
	je	.LBB7_16
# BB#11:                                # %vector.memcheck
	leaq	-1(%rdx), %rdi
	leaq	(%r14,%rdi), %rbx
	cmpq	%r10, %rbx
	ja	.LBB7_13
# BB#12:                                # %vector.memcheck
	addq	%r10, %rdi
	cmpq	%r14, %rdi
	jbe	.LBB7_16
.LBB7_13:                               # %vector.body.preheader
	subq	%rax, %rcx
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rdx, %rdi
	andq	$-4, %rdi
	leaq	-4(%r14,%rdx), %rbx
	leaq	-4(%r10,%rdx), %rsi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB7_14:                               # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rsi), %r11d
	movzbl	2(%rsi), %r15d
	movzbl	1(%rsi), %r12d
	movzbl	(%rsi), %r13d
	movzbl	3(%rbx), %edx
	movzbl	2(%rbx), %eax
	movzbl	1(%rbx), %r8d
	movzbl	(%rbx), %r9d
	orq	%r13, %r9
	orq	%r12, %r8
	orq	%r15, %rax
	orq	%r11, %rdx
	movb	%dl, 3(%rbx)
	movb	%al, 2(%rbx)
	movb	%r8b, 1(%rbx)
	movb	%r9b, (%rbx)
	addq	$-4, %rbx
	addq	$-4, %rsi
	addq	$-4, %rdi
	jne	.LBB7_14
# BB#15:
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
.LBB7_16:                               # %middle.block
	cmpq	%rsi, %rdx
	je	.LBB7_30
	.align	16, 0x90
.LBB7_17:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r10,%rcx), %eax
	movzbl	(%r14,%rcx), %edx
	orq	%rax, %rdx
	movb	%dl, (%r14,%rcx)
	decq	%rcx
	cmpq	$-1, %rcx
	jne	.LBB7_17
	jmp	.LBB7_30
.LBB7_27:                               # %if.else77
	andq	$255, %rbx
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	movq	%r8, %rsi
	movq	%rbx, %rdx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_croak  # TAILCALL
.LBB7_28:                               # %if.else
	movzbl	(%r14,%rbx), %ecx
	notq	%rax
	andq	%rax, %rcx
.LBB7_29:                               # %if.end82
	movb	%cl, (%r14,%rbx)
.LBB7_30:                               # %if.end82
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_18:                               # %while.cond63.preheader
	testq	%rdx, %rdx
	je	.LBB7_30
# BB#19:                                # %while.body67.preheader
	xorq	%rsi, %rsi
	movq	%rdx, %rbx
	andq	$-4, %rbx
	je	.LBB7_25
# BB#20:                                # %vector.memcheck145
	leaq	-1(%rdx), %rdi
	leaq	(%r14,%rdi), %rax
	cmpq	%r10, %rax
	ja	.LBB7_22
# BB#21:                                # %vector.memcheck145
	addq	%r10, %rdi
	cmpq	%r14, %rdi
	jbe	.LBB7_25
.LBB7_22:                               # %vector.body130.preheader
	subq	%rbx, %rcx
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	%rdx, %r9
	andq	$-4, %r9
	leaq	-4(%r14,%rdx), %rbx
	leaq	-4(%r10,%rdx), %rsi
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB7_23:                               # %vector.body130
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rsi), %r13d
	movzbl	1(%rsi), %r12d
	movzbl	2(%rsi), %r15d
	movzbl	3(%rsi), %r11d
	notq	%r11
	notq	%r15
	notq	%r12
	notq	%r13
	movzbl	3(%rbx), %edi
	movzbl	2(%rbx), %edx
	movzbl	1(%rbx), %eax
	movzbl	(%rbx), %r8d
	andq	%r13, %r8
	andq	%r12, %rax
	andq	%r15, %rdx
	andq	%r11, %rdi
	movb	%dil, 3(%rbx)
	movb	%dl, 2(%rbx)
	movb	%al, 1(%rbx)
	movb	%r8b, (%rbx)
	addq	$-4, %rbx
	addq	$-4, %rsi
	addq	$-4, %r9
	jne	.LBB7_23
# BB#24:
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
.LBB7_25:                               # %middle.block131
	cmpq	%rsi, %rdx
	je	.LBB7_30
	.align	16, 0x90
.LBB7_26:                               # %while.body67
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r10,%rcx), %eax
	movzbl	(%r14,%rcx), %edx
	notq	%rax
	andq	%rax, %rdx
	movb	%dl, (%r14,%rcx)
	decq	%rcx
	cmpq	$-1, %rcx
	jne	.LBB7_26
	jmp	.LBB7_30
.Ltmp123:
	.size	set_opset_bits, .Ltmp123-set_opset_bits
	.cfi_endproc

	.globl	XS_Opcode_permit_only
	.align	16, 0x90
	.type	XS_Opcode_permit_only,@function
XS_Opcode_permit_only:                  # @XS_Opcode_permit_only
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
	subq	$64, %rsp
.Ltmp135:
	.cfi_def_cfa_offset 112
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
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	PL_stack_sp(%rip), %r13
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	(%rdi), %rdx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %r13
	movq	88(%rdx), %r12
	movq	$3, %rcx
	movq	%r13, %rsi
	sarq	%cl, %rsi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	cmpq	$7, %r13
	jg	.LBB8_2
# BB#1:                                 # %if.then
	movq	96(%rdx), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB8_2:                                # %if.end
	movq	8(%rax,%rbx,8), %r14
	movzbl	18(%r14), %eax
	leaq	1(%rbx), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	testq	$8, %rax
	je	.LBB8_4
# BB#3:                                 # %lor.lhs.false
	movq	(%r14), %rax
	movq	(%rax), %rdi
	movq	16(%rdi), %rax
	andq	$4351, %rax             # imm = 0x10FF
	cmpq	$4107, %rax             # imm = 0x100B
	je	.LBB8_5
.LBB8_4:                                # %if.then18
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	(%r14), %rax
	movq	(%rax), %rdi
.LBB8_5:                                # %if.end19
	movabsq	$.L.str12, %rsi
	movq	$4, %rdx
	movq	$1, %rcx
	callq	Perl_hv_fetch
	movq	(%rax), %r14
	testq	$-3, %r12
	je	.LBB8_6
# BB#20:                                # %if.else
	movq	16(%r14), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB8_21
# BB#22:                                # %if.else.i121
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB8_23
# BB#24:                                # %if.else5.i
	movq	(%r14), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_27
# BB#25:
	movabsq	$.L.str63, %rsi
	jmp	.LBB8_26
.LBB8_6:                                # %if.then25
	cmpq	$2, %r12
	jb	.LBB8_7
# BB#8:                                 # %if.then25
	xorq	%r15, %r15
	jmp	.LBB8_9
.LBB8_21:
	movabsq	$.L.str61, %rsi
	jmp	.LBB8_26
.LBB8_7:
	movq	my_cxt.1(%rip), %r15
.LBB8_9:                                # %if.then25
	testq	%r15, %r15
	je	.LBB8_18
# BB#10:                                # %if.then.i
	movq	16(%r15), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB8_11
# BB#12:                                # %if.else.i.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB8_13
# BB#14:                                # %if.else5.i.i
	movq	(%r15), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB8_17
# BB#15:
	movabsq	$.L.str63, %rsi
	jmp	.LBB8_16
.LBB8_23:
	movabsq	$.L.str62, %rsi
.LBB8_26:                               # %if.then11.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB8_27
.LBB8_18:                               # %if.else.i
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movq	(%r15), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%r15), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%r15), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r15)
	jmp	.LBB8_19
.LBB8_11:
	movabsq	$.L.str61, %rsi
	jmp	.LBB8_16
.LBB8_13:
	movabsq	$.L.str62, %rsi
.LBB8_16:                               # %if.then11.i.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB8_17:                               # %verify_opset.exit.i
	movq	%r15, %rdi
	callq	Perl_newSVsv
	movq	%rax, %r15
.LBB8_19:                               # %new_opset.exit
	movq	%r15, %rdi
	callq	Perl_sv_2mortal
	movq	$2, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setsv_flags
.LBB8_27:                               # %if.end32
	cmpq	$9, %r13
	jl	.LBB8_43
# BB#28:                                # %for.body.lr.ph
	movq	(%r14), %rax
	movq	(%rax), %r15
	cmpq	$2, %r12
	sbbq	%rax, %rax
	andq	$1, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	cmpq	$1, %r12
	jbe	.LBB8_29
# BB#30:                                # %for.body.lr.ph
	movabsq	$1, %rax
	jmp	.LBB8_31
.LBB8_29:
	movabsq	$0, %rax
.LBB8_31:                               # %for.body.lr.ph
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	16(,%rax,8), %r13
	movq	$1, %r14
	.align	16, 0x90
.LBB8_32:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	addq	%r13, %rax
	movq	-8(%rax,%r14,8), %rsi
	movq	16(%rsi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB8_35
# BB#33:                                # %if.else5.i134
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	(%rsi), %rcx
	movq	8(%rcx), %rcx
	movq	my_cxt.2(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB8_35
# BB#34:                                #   in Loop: Header=BB8_32 Depth=1
	movq	40(%rsp), %r12          # 8-byte Reload
	movabsq	$.L.str9, %rbx
	jmp	.LBB8_42
	.align	16, 0x90
.LBB8_35:                               # %if.else50
                                        #   in Loop: Header=BB8_32 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB8_36
# BB#37:                                # %cond.false68
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	$2, %rdx
	movq	%rsi, %rdi
	leaq	56(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
	jmp	.LBB8_38
	.align	16, 0x90
.LBB8_36:                               # %cond.true58
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	(%rsi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 56(%rsp)
	movq	(%rax), %rbx
.LBB8_38:                               # %cond.end73
                                        #   in Loop: Header=BB8_32 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$33, %rax
	jne	.LBB8_39
# BB#40:                                # %if.then78
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	56(%rsp), %rsi
	incq	%rbx
	decq	%rsi
	movq	%rsi, 56(%rsp)
	movq	32(%rsp), %r12          # 8-byte Reload
	jmp	.LBB8_41
	.align	16, 0x90
.LBB8_39:                               # %cond.end73.if.end81_crit_edge
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	56(%rsp), %rsi
	movq	40(%rsp), %r12          # 8-byte Reload
.LBB8_41:                               # %if.end81
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	%rbx, %rdi
	callq	get_op_bitspec
	movq	%rax, %rsi
.LBB8_42:                               # %if.end83
                                        #   in Loop: Header=BB8_32 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	set_opset_bits
	incq	%r14
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jl	.LBB8_32
.LBB8_43:                               # %for.end
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	$PL_sv_yes, 8(%rax,%rcx,8)
	movq	PL_stack_base(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp141:
	.size	XS_Opcode_permit_only, .Ltmp141-XS_Opcode_permit_only
	.cfi_endproc

	.globl	XS_Opcode_opdesc
	.align	16, 0x90
	.type	XS_Opcode_opdesc,@function
XS_Opcode_opdesc:                       # @XS_Opcode_opdesc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp148:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp149:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp150:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp151:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp152:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp153:
	.cfi_def_cfa_offset 96
.Ltmp154:
	.cfi_offset %rbx, -48
.Ltmp155:
	.cfi_offset %r12, -40
.Ltmp156:
	.cfi_offset %r13, -32
.Ltmp157:
	.cfi_offset %r14, -24
.Ltmp158:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	movq	PL_stack_base(%rip), %rcx
	leaq	-8(%rax), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	leaq	(%rcx,%r14,8), %rax
	movq	%rbx, %r15
	subq	%rax, %r15
	movq	$3, %rcx
	movq	%r15, %rax
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rax
	andq	$-8, %rax
	subq	%rax, %rbx
	callq	Perl_get_op_descs
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	leaq	8(%rcx,%r14,8), %rdi
	movq	%r15, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	testq	%r15, %r15
	jle	.LBB9_27
# BB#1:
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	movq	(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_16 Depth 2
                                        #       Child Loop BB9_18 Depth 3
	movq	(%rsp), %rax            # 8-byte Reload
	movq	(%rax,%rcx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB9_3
# BB#4:                                 # %cond.false
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	$2, %rdx
	leaq	40(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	40(%rsp), %rsi
	movq	%rax, %r13
	jmp	.LBB9_5
	.align	16, 0x90
.LBB9_3:                                # %cond.true
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 40(%rsp)
	movq	(%rax), %r13
.LBB9_5:                                # %cond.end
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	%r13, %rdi
	callq	get_op_bitspec
	movq	16(%rax), %rdx
	testq	$65536, %rdx            # imm = 0x10000
	jne	.LBB9_6
# BB#12:                                # %if.else
                                        #   in Loop: Header=BB9_2 Depth=1
	testq	$262144, %rdx           # imm = 0x40000
	je	.LBB9_25
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	(%rax), %rax
	movq	8(%rax), %rcx
	movq	my_cxt.2(%rip), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB9_25
# BB#14:                                # %cond.end78
                                        #   in Loop: Header=BB9_2 Depth=1
	testq	%rcx, %rcx
	jle	.LBB9_26
# BB#15:                                # %for.body84.lr.ph
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	PL_maxo(%rip), %eax
	xorq	%r12, %r12
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB9_16:                               # %for.body84
                                        #   Parent Loop BB9_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_18 Depth 3
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jge	.LBB9_24
# BB#17:                                # %for.body94.lr.ph
                                        #   in Loop: Header=BB9_16 Depth=2
	movq	16(%rsp), %rcx          # 8-byte Reload
	movsbq	(%rcx,%r12), %r15
	andq	$65535, %r15            # imm = 0xFFFF
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rdx
	xorq	%r13, %r13
	movq	%rdx, %r14
	.align	16, 0x90
.LBB9_18:                               # %for.body94
                                        #   Parent Loop BB9_2 Depth=1
                                        #     Parent Loop BB9_16 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %r15
	je	.LBB9_22
# BB#19:                                # %if.then98
                                        #   in Loop: Header=BB9_18 Depth=3
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB9_21
# BB#20:                                # %if.then105
                                        #   in Loop: Header=BB9_18 Depth=3
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB9_21:                               # %if.end107
                                        #   in Loop: Header=BB9_18 Depth=3
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	-8(%rax,%r14,8), %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	movl	PL_maxo(%rip), %eax
	addq	$8, %rbx
.LBB9_22:                               # %for.inc
                                        #   in Loop: Header=BB9_18 Depth=3
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$7, %rdx
	movq	%r14, %rdx
	jg	.LBB9_24
# BB#23:                                # %for.inc
                                        #   in Loop: Header=BB9_18 Depth=3
	incq	%r14
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jl	.LBB9_18
.LBB9_24:                               # %for.inc116
                                        #   in Loop: Header=BB9_16 Depth=2
	movq	my_cxt.2(%rip), %rcx
	incq	%r12
	cmpq	%rcx, %r12
	jl	.LBB9_16
	jmp	.LBB9_26
	.align	16, 0x90
.LBB9_6:                                # %cond.true29
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	(%rax), %rax
	movq	24(%rax), %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB9_8
# BB#7:                                 # %cond.true29
                                        #   in Loop: Header=BB9_2 Depth=1
	movl	PL_maxo(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB9_9
.LBB9_8:                                # %if.then40
                                        #   in Loop: Header=BB9_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str13, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	Perl_croak
.LBB9_9:                                # %if.end
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB9_11
# BB#10:                                # %if.then47
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB9_11:                               # %if.end49
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r12,8), %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	jmp	.LBB9_26
	.align	16, 0x90
.LBB9_25:                               # %if.else119
                                        #   in Loop: Header=BB9_2 Depth=1
	andq	$255, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str14, %rdi
	movq	%r13, %rsi
	callq	Perl_croak
.LBB9_26:                               # %for.inc125
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %rcx
	jl	.LBB9_2
.LBB9_27:                               # %for.end127
	movq	%rbx, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp159:
	.size	XS_Opcode_opdesc, .Ltmp159-XS_Opcode_opdesc
	.cfi_endproc

	.globl	XS_Opcode_define_optag
	.align	16, 0x90
	.type	XS_Opcode_define_optag,@function
XS_Opcode_define_optag:                 # @XS_Opcode_define_optag
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp164:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp165:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp167:
	.cfi_def_cfa_offset 48
.Ltmp168:
	.cfi_offset %rbx, -32
.Ltmp169:
	.cfi_offset %r14, -24
.Ltmp170:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$16, %rdx
	je	.LBB10_2
# BB#1:                                 # %if.then
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB10_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %ecx
	movq	16(%rax,%rbx,8), %r14
	leaq	1(%rbx), %r15
	testq	$4, %rcx
	je	.LBB10_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB10_5
.LBB10_4:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rsi
	movq	%rax, %rdi
.LBB10_5:                               # %cond.end
	movq	%r14, %rdx
	callq	put_op_bitspec
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp171:
	.size	XS_Opcode_define_optag, .Ltmp171-XS_Opcode_define_optag
	.cfi_endproc

	.align	16, 0x90
	.type	put_op_bitspec,@function
put_op_bitspec:                         # @put_op_bitspec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp176:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp177:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp178:
	.cfi_def_cfa_offset 32
.Ltmp179:
	.cfi_offset %rbx, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	16(%r14), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r15
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB11_1
# BB#2:                                 # %if.else.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB11_3
# BB#4:                                 # %if.else5.i
	movq	(%r14), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB11_7
# BB#5:
	movabsq	$.L.str63, %rsi
	jmp	.LBB11_6
.LBB11_1:
	movabsq	$.L.str61, %rsi
	jmp	.LBB11_6
.LBB11_3:
	movabsq	$.L.str62, %rsi
.LBB11_6:                               # %if.then11.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB11_7:                               # %verify_opset.exit
	testq	%rbx, %rbx
	jne	.LBB11_9
# BB#8:                                 # %if.then
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
.LBB11_9:                               # %if.end
	movq	my_cxt.0(%rip), %rdi
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	Perl_hv_fetch
	movq	%rax, %rbx
	movq	(%rbx), %rdi
	movzbl	18(%rdi), %eax
	testq	$1807, %rax             # imm = 0x70F
	je	.LBB11_11
# BB#10:                                # %if.then4
	movabsq	$.L.str51, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_croak
	movq	(%rbx), %rdi
.LBB11_11:                              # %if.end5
	movq	$2, %rdx
	movq	%r14, %rsi
	callq	Perl_sv_setsv_flags
	movq	(%rbx), %rax
	orq	$8388608, 16(%rax)      # imm = 0x800000
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp182:
	.size	put_op_bitspec, .Ltmp182-put_op_bitspec
	.cfi_endproc

	.globl	XS_Opcode_empty_opset
	.align	16, 0x90
	.type	XS_Opcode_empty_opset,@function
XS_Opcode_empty_opset:                  # @XS_Opcode_empty_opset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp187:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp188:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp189:
	.cfi_def_cfa_offset 32
.Ltmp190:
	.cfi_offset %rbx, -32
.Ltmp191:
	.cfi_offset %r14, -24
.Ltmp192:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	leaq	1(%r14), %r15
	cmpq	%rax, %rdx
	je	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str16, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB12_2:                               # %if.end
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp193:
	.size	XS_Opcode_empty_opset, .Ltmp193-XS_Opcode_empty_opset
	.cfi_endproc

	.globl	XS_Opcode_full_opset
	.align	16, 0x90
	.type	XS_Opcode_full_opset,@function
XS_Opcode_full_opset:                   # @XS_Opcode_full_opset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp198:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp199:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 32
.Ltmp201:
	.cfi_offset %rbx, -32
.Ltmp202:
	.cfi_offset %r14, -24
.Ltmp203:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	cmpq	%rax, %rdx
	je	.LBB13_2
# BB#1:                                 # %if.then
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB13_2:                               # %if.end
	movq	my_cxt.1(%rip), %rbx
	leaq	1(%r14), %r15
	testq	%rbx, %rbx
	je	.LBB13_11
# BB#3:                                 # %if.then.i
	movq	16(%rbx), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB13_4
# BB#5:                                 # %if.else.i.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB13_6
# BB#7:                                 # %if.else5.i.i
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB13_10
# BB#8:
	movabsq	$.L.str63, %rsi
	jmp	.LBB13_9
.LBB13_11:                              # %if.else.i
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	jmp	.LBB13_12
.LBB13_4:
	movabsq	$.L.str61, %rsi
	jmp	.LBB13_9
.LBB13_6:
	movabsq	$.L.str62, %rsi
.LBB13_9:                               # %if.then11.i.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB13_10:                              # %verify_opset.exit.i
	movq	%rbx, %rdi
	callq	Perl_newSVsv
	movq	%rax, %rbx
.LBB13_12:                              # %new_opset.exit
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp204:
	.size	XS_Opcode_full_opset, .Ltmp204-XS_Opcode_full_opset
	.cfi_endproc

	.globl	XS_Opcode_opmask_add
	.align	16, 0x90
	.type	XS_Opcode_opmask_add,@function
XS_Opcode_opmask_add:                   # @XS_Opcode_opmask_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp208:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp209:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp210:
	.cfi_def_cfa_offset 32
.Ltmp211:
	.cfi_offset %rbx, -24
.Ltmp212:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB14_2
# BB#1:                                 # %if.then
	movabsq	$.L.str18, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB14_2:                               # %if.end
	movq	8(%rax,%rbx,8), %r14
	cmpq	$0, PL_op_mask(%rip)
	jne	.LBB14_4
# BB#3:                                 # %if.then6
	movslq	PL_maxo(%rip), %rdi
	callq	Perl_safesysmalloc
	movslq	PL_maxo(%rip), %rdx
	movq	%rax, PL_op_mask(%rip)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	memset
.LBB14_4:                               # %if.end9
	movq	%r14, %rdi
	callq	opmask_add
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp213:
	.size	XS_Opcode_opmask_add, .Ltmp213-XS_Opcode_opmask_add
	.cfi_endproc

	.align	16, 0x90
	.type	opmask_add,@function
opmask_add:                             # @opmask_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp219:
	.cfi_def_cfa_offset 32
.Ltmp220:
	.cfi_offset %rbx, -24
.Ltmp221:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rax
	testq	$118423552, %rax        # imm = 0x70F0000
	je	.LBB15_1
# BB#2:                                 # %if.else.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB15_3
# BB#4:                                 # %if.else5.i
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	movq	my_cxt.2(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB15_7
# BB#5:
	movabsq	$.L.str63, %rsi
	jmp	.LBB15_6
.LBB15_1:
	movabsq	$.L.str61, %rsi
	jmp	.LBB15_6
.LBB15_3:
	movabsq	$.L.str62, %rsi
.LBB15_6:                               # %if.then11.i
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB15_7:                               # %verify_opset.exit
	cmpq	$0, PL_op_mask(%rip)
	jne	.LBB15_9
# BB#8:                                 # %if.then
	movabsq	$.L.str50, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB15_9:                               # %if.end
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB15_11
# BB#10:                                # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %rax
	jmp	.LBB15_12
.LBB15_11:                              # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
.LBB15_12:                              # %cond.end
	movq	my_cxt.2(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB15_21
# BB#13:
	xorq	%r8, %r8
	xorq	%r10, %r10
	.align	16, 0x90
.LBB15_14:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_17 Depth 2
	movzbl	(%rax,%r8), %r9d
	testq	%r9, %r9
	jne	.LBB15_15
# BB#22:                                # %if.then7
                                        #   in Loop: Header=BB15_14 Depth=1
	addq	$8, %r10
	jmp	.LBB15_20
	.align	16, 0x90
.LBB15_15:                              # %for.cond9.preheader
                                        #   in Loop: Header=BB15_14 Depth=1
	movslq	PL_maxo(%rip), %rsi
	movq	$32, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jge	.LBB15_20
# BB#16:                                # %for.body14.lr.ph
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	incq	%r10
	xorq	%r11, %r11
	movq	%r10, %rdi
	.align	16, 0x90
.LBB15_17:                              # %for.body14
                                        #   Parent Loop BB15_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r11, %r14
	movq	%rdi, %r10
	leaq	1(%r14), %r11
	movq	$32, %rcx
	movq	%r11, %rdi
	shlq	%cl, %rdi
	movq	PL_op_mask(%rip), %rbx
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	movzbl	-1(%rbx,%r10), %edx
	movq	%r14, %rcx
	shlq	%cl, %rsi
	andq	%r9, %rsi
	orq	%rsi, %rdx
	cmpq	$7, %rdi
	movb	%dl, -1(%rbx,%r10)
	jg	.LBB15_19
# BB#18:                                # %for.body14
                                        #   in Loop: Header=BB15_17 Depth=2
	movl	PL_maxo(%rip), %edx
	leaq	1(%r10), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB15_17
.LBB15_19:                              # %for.cond9.for.inc.loopexit_crit_edge
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	my_cxt.2(%rip), %rdx
.LBB15_20:                              # %for.inc
                                        #   in Loop: Header=BB15_14 Depth=1
	incq	%r8
	cmpq	%rdx, %r8
	jl	.LBB15_14
.LBB15_21:                              # %for.end23
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp222:
	.size	opmask_add, .Ltmp222-opmask_add
	.cfi_endproc

	.globl	XS_Opcode_opcodes
	.align	16, 0x90
	.type	XS_Opcode_opcodes,@function
XS_Opcode_opcodes:                      # @XS_Opcode_opcodes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp227:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp228:
	.cfi_def_cfa_offset 32
.Ltmp229:
	.cfi_offset %rbx, -24
.Ltmp230:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r14
	movq	PL_stack_base(%rip), %rcx
	leaq	-8(%rax), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rax), %rax
	leaq	(%rcx,%rax,8), %rax
	movq	%r14, %rbx
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	je	.LBB16_2
# BB#1:                                 # %if.then
	movabsq	$.L.str19, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB16_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	36(%rax), %eax
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r14
	andq	$3, %rax
	je	.LBB16_6
# BB#3:                                 # %cond.true
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB16_4
# BB#5:                                 # %cond.true
	movabsq	$1, %rax
	jmp	.LBB16_7
.LBB16_6:                               # %cond.false
	callq	Perl_dowantarray
	jmp	.LBB16_7
.LBB16_4:
	movabsq	$0, %rax
.LBB16_7:                               # %cond.end
	cmpq	$1, %rax
	jne	.LBB16_9
# BB#8:                                 # %if.then15
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB16_12
.LBB16_9:                               # %if.else
	movq	PL_stack_max(%rip), %rax
	subq	%r14, %rax
	cmpq	$7, %rax
	jg	.LBB16_11
# BB#10:                                # %if.then22
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r14
.LBB16_11:                              # %if.end24
	movslq	PL_maxo(%rip), %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r14)
	addq	$8, %r14
.LBB16_12:                              # %if.end29
	movq	%r14, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp231:
	.size	XS_Opcode_opcodes, .Ltmp231-XS_Opcode_opcodes
	.cfi_endproc

	.globl	XS_Opcode_opmask
	.align	16, 0x90
	.type	XS_Opcode_opmask,@function
XS_Opcode_opmask:                       # @XS_Opcode_opmask
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp236:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp237:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp238:
	.cfi_def_cfa_offset 32
.Ltmp239:
	.cfi_offset %rbx, -32
.Ltmp240:
	.cfi_offset %r14, -24
.Ltmp241:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	cmpq	%rax, %rdx
	je	.LBB17_2
# BB#1:                                 # %if.then
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB17_2:                               # %if.end
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %r15
	movq	(%r15), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%r15), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%r15), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r15)
	movq	%r15, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	PL_op_mask(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB17_9
# BB#3:                                 # %if.then7
	movslq	PL_maxo(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB17_9
# BB#4:
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r14,8), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	jmp	.LBB17_5
	.align	16, 0x90
.LBB17_8:                               # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB17_5 Depth=1
	movq	PL_op_mask(%rip), %rcx
.LBB17_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx,%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB17_7
# BB#6:                                 # %if.then13
                                        #   in Loop: Header=BB17_5 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	%rbx, %rcx
	andq	$7, %rcx
	movq	$1, %rsi
	shlq	%cl, %rsi
	movq	$35, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx), %ecx
	orq	%rsi, %rcx
	movb	%cl, (%rax,%rdx)
	movl	PL_maxo(%rip), %edx
.LBB17_7:                               # %for.inc
                                        #   in Loop: Header=BB17_5 Depth=1
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rdi, %rsi
	jl	.LBB17_8
.LBB17_9:                               # %if.end18
	movq	PL_stack_base(%rip), %rax
	incq	%r14
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp242:
	.size	XS_Opcode_opmask, .Ltmp242-XS_Opcode_opmask
	.cfi_endproc

	.globl	boot_Opcode
	.align	16, 0x90
	.type	boot_Opcode,@function
boot_Opcode:                            # @boot_Opcode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp249:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp250:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp251:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp252:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp253:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp254:
	.cfi_def_cfa_offset 64
.Ltmp255:
	.cfi_offset %rbx, -48
.Ltmp256:
	.cfi_offset %r12, -40
.Ltmp257:
	.cfi_offset %r13, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str23, %rdi
	movabsq	$XS_Opcode__safe_pkg_prep, %rsi
	movabsq	$.L.str22, %r12
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str24, %r14
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str25, %rdi
	movabsq	$XS_Opcode__safe_call_sv, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str26, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str27, %rdi
	movabsq	$XS_Opcode_verify_opset, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str28, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str29, %rdi
	movabsq	$XS_Opcode_invert_opset, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str30, %rdi
	movabsq	$XS_Opcode_opset_to_ops, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str31, %rdi
	movabsq	$XS_Opcode_opset, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str32, %r15
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str33, %rdi
	movabsq	$XS_Opcode_permit_only, %r13
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	(%rax), %rcx
	movq	$0, 88(%rcx)
	movabsq	$.L.str34, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str35, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	(%rax), %rcx
	movq	$3, 88(%rcx)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str36, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	(%rax), %rcx
	movq	$2, 88(%rcx)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str37, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	(%rax), %rcx
	movq	$1, 88(%rcx)
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str38, %rdi
	movabsq	$XS_Opcode_opdesc, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str39, %rdi
	movabsq	$XS_Opcode_define_optag, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str40, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str41, %rdi
	movabsq	$XS_Opcode_empty_opset, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str42, %r15
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str43, %rdi
	movabsq	$XS_Opcode_full_opset, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str44, %rdi
	movabsq	$XS_Opcode_opmask_add, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str45, %rdi
	movabsq	$XS_Opcode_opcodes, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str46, %rdi
	movabsq	$XS_Opcode_opmask, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movl	PL_maxo(%rip), %eax
	leaq	7(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$60, %rcx
	shrq	%cl, %rdx
	andq	$7, %rdx
	leaq	7(%rax,%rdx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$35, %rcx
	sarq	%cl, %rax
	movq	%rax, my_cxt.2(%rip)
	callq	Perl_newHV
	movq	%rax, my_cxt.0(%rip)
	callq	Perl_get_op_names
	movslq	PL_maxo(%rip), %rcx
	movq	%rax, %r14
	testq	%rcx, %rcx
	jle	.LBB18_3
# BB#1:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB18_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	Perl_newSViv
	movq	%rax, %r15
	orq	$8388608, 16(%r15)      # imm = 0x800000
	movq	my_cxt.0(%rip), %r12
	movq	(%r14,%rbx,8), %r13
	movq	%r13, %rdi
	callq	strlen
	xorq	%r8, %r8
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	Perl_hv_store
	incq	%rbx
	movslq	PL_maxo(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB18_2
.LBB18_3:                               # %cond.end.i
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %rax
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	movq	%rbx, %rdi
	callq	Perl_sv_2mortal
	movabsq	$.L.str48, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	put_op_bitspec
	movq	my_cxt.2(%rip), %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	my_cxt.2(%rip), %rdx
	movq	(%rax), %rdi
	incq	%rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	my_cxt.2(%rip), %r9
	movq	(%rbx), %rax
	movq	%r9, 8(%rax)
	movq	16(%rbx), %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	orq	$67371008, %rcx         # imm = 0x4040000
	movq	%rcx, 16(%rbx)
	movq	%rbx, my_cxt.1(%rip)
	movq	(%rax), %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	leaq	(%r9,%r8), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB18_15
# BB#4:                                 # %while.body.lr.ph.i
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r9, %rax
	shlq	%cl, %rax
	movabsq	$-4294967296, %r10      # imm = 0xFFFFFFFF00000000
	addq	%r10, %rax
	movq	$32, %rcx
	shrq	%cl, %rax
	addq	%r8, %rax
	movq	%rax, %rbx
	notq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$-2, %rdi
	jg	.LBB18_6
# BB#5:                                 # %while.body.lr.ph.i
	movq	$-1, %rbx
.LBB18_6:                               # %while.body.lr.ph.i
	leaq	1(%rbx,%rax), %r12
	andq	%r8, %r12
	incq	%r12
	movabsq	$8589934588, %r11       # imm = 0x1FFFFFFFC
	movq	%r12, %r15
	andq	%r11, %r15
	xorq	%rax, %rax
	andq	%r12, %r11
	je	.LBB18_12
# BB#7:                                 # %vector.body.preheader
	movq	$32, %rcx
	movq	%r9, %rdi
	shlq	%cl, %rdi
	addq	%r10, %rdi
	movq	$32, %rcx
	shrq	%cl, %rdi
	addq	%r8, %rdi
	movq	%rdi, %rbx
	notq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$-2, %rax
	jg	.LBB18_9
# BB#8:                                 # %vector.body.preheader
	movq	$-1, %rbx
.LBB18_9:                               # %vector.body.preheader
	subq	%r15, %rsi
	leaq	1(%rbx,%rdi), %rax
	andq	%r8, %rax
	incq	%rax
	andq	$-4, %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	-4(%r14,%rdx), %rcx
	movq	$255, %rdi
	.align	16, 0x90
.LBB18_10:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movb	%dil, 3(%rcx)
	movb	%dil, 2(%rcx)
	movb	%dil, 1(%rcx)
	movb	%dil, (%rcx)
	addq	$-4, %rcx
	addq	$-4, %rax
	jne	.LBB18_10
# BB#11:
	movq	%r11, %rax
.LBB18_12:                              # %middle.block
	cmpq	%rax, %r12
	je	.LBB18_15
# BB#13:                                # %while.body.i.preheader
	decq	%rsi
	movq	$255, %rax
	.align	16, 0x90
.LBB18_14:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movb	%al, (%rdx,%rsi)
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	decq	%rsi
	testq	%rdi, %rdi
	jg	.LBB18_14
.LBB18_15:                              # %while.end.i
	movl	PL_maxo(%rip), %ecx
	movq	8(%rsp), %rbx           # 8-byte Reload
	leaq	1(%rbx), %r14
	andq	$7, %rcx
	movq	$255, %rax
	je	.LBB18_17
# BB#16:                                # %cond.true21.i
	movq	$255, %rax
	shlq	%cl, %rax
	xorq	$255, %rax
.LBB18_17:                              # %op_names_init.exit
	movb	%al, -1(%r9,%rdx)
	movq	my_cxt.1(%rip), %rdx
	movabsq	$.L.str49, %rdi
	xorq	%rsi, %rsi
	callq	put_op_bitspec
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp260:
	.size	boot_Opcode, .Ltmp260-boot_Opcode
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Opcode::_safe_pkg_prep(Package)"
	.size	.L.str, 39

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"main"
	.size	.L.str1, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"_"
	.size	.L.str2, 2

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Usage: Opcode::_safe_call_sv(Package, mask, codesv)"
	.size	.L.str3, 52

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"main::"
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"INC"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Usage: Opcode::verify_opset(opset, fatal = 0)"
	.size	.L.str6, 46

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Usage: Opcode::invert_opset(opset)"
	.size	.L.str7, 35

	.type	my_cxt.0,@object        # @my_cxt.0
	.local	my_cxt.0
	.comm	my_cxt.0,8,8
	.type	my_cxt.1,@object        # @my_cxt.1
	.local	my_cxt.1
	.comm	my_cxt.1,8,8
	.type	my_cxt.2,@object        # @my_cxt.2
	.local	my_cxt.2
	.comm	my_cxt.2,8,8
	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Usage: Opcode::opset_to_ops(opset, desc = 0)"
	.size	.L.str8, 45

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"(opset)"
	.size	.L.str9, 8

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Usage: %s(safe, ...)"
	.size	.L.str10, 21

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Not a Safe object"
	.size	.L.str11, 18

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Mask"
	.size	.L.str12, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"panic: opcode %d (%s) out of range"
	.size	.L.str13, 35

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"panic: invalid bitspec for \"%s\" (type %u)"
	.size	.L.str14, 42

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Usage: Opcode::define_optag(optagsv, mask)"
	.size	.L.str15, 43

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Usage: Opcode::empty_opset()"
	.size	.L.str16, 29

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Usage: Opcode::full_opset()"
	.size	.L.str17, 28

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Usage: Opcode::opmask_add(opset)"
	.size	.L.str18, 33

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Usage: Opcode::opcodes()"
	.size	.L.str19, 25

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"opcodes in list context not yet implemented"
	.size	.L.str20, 44

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Usage: Opcode::opmask()"
	.size	.L.str21, 24

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Opcode.c"
	.size	.L.str22, 9

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Opcode::_safe_pkg_prep"
	.size	.L.str23, 23

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"$"
	.size	.L.str24, 2

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Opcode::_safe_call_sv"
	.size	.L.str25, 22

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"$$$"
	.size	.L.str26, 4

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Opcode::verify_opset"
	.size	.L.str27, 21

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"$;$"
	.size	.L.str28, 4

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Opcode::invert_opset"
	.size	.L.str29, 21

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Opcode::opset_to_ops"
	.size	.L.str30, 21

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"Opcode::opset"
	.size	.L.str31, 14

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	";@"
	.size	.L.str32, 3

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"Opcode::permit_only"
	.size	.L.str33, 20

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"$;@"
	.size	.L.str34, 4

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Opcode::deny"
	.size	.L.str35, 13

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"Opcode::deny_only"
	.size	.L.str36, 18

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"Opcode::permit"
	.size	.L.str37, 15

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Opcode::opdesc"
	.size	.L.str38, 15

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"Opcode::define_optag"
	.size	.L.str39, 21

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"$$"
	.size	.L.str40, 3

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Opcode::empty_opset"
	.size	.L.str41, 20

	.type	.L.str42,@object        # @.str42
.L.str42:
	.zero	1
	.size	.L.str42, 1

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"Opcode::full_opset"
	.size	.L.str43, 19

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Opcode::opmask_add"
	.size	.L.str44, 19

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"Opcode::opcodes"
	.size	.L.str45, 16

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"Opcode::opmask"
	.size	.L.str46, 15

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	":none"
	.size	.L.str48, 6

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	":all"
	.size	.L.str49, 5

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"Can't add to uninitialised PL_op_mask"
	.size	.L.str50, 38

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"Opcode tag \"%s\" already defined"
	.size	.L.str51, 32

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"panic: opcode \"%s\" value %d is invalid"
	.size	.L.str52, 39

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"Unknown operator tag \"%s\""
	.size	.L.str57, 26

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"Can't negate operators here (\"%s\")"
	.size	.L.str58, 35

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"Unknown operator name \"%s\""
	.size	.L.str59, 27

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Unknown operator prefix \"%s\""
	.size	.L.str60, 29

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"undefined"
	.size	.L.str61, 10

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"wrong type"
	.size	.L.str62, 11

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"wrong size"
	.size	.L.str63, 11

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Invalid opset: %s"
	.size	.L.str64, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
