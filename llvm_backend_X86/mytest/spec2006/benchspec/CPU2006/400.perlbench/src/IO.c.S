	.file	"IO.c.bc"
	.text
	.globl	XS_IO__Seekable_getpos
	.align	16, 0x90
	.type	XS_IO__Seekable_getpos,@function
XS_IO__Seekable_getpos:                 # @XS_IO__Seekable_getpos
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
	je	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB0_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r15
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	56(%rax), %r14
	testq	%r14, %r14
	je	.LBB0_5
# BB#3:                                 # %if.then6
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rsi
	movq	%r14, %rdi
	callq	PerlIO_getpos
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_6
# BB#4:                                 # %if.then15
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%rbx,8)
	jmp	.LBB0_6
.LBB0_5:                                # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%rbx,8)
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
.LBB0_6:                                # %if.end22
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	XS_IO__Seekable_getpos, .Ltmp10-XS_IO__Seekable_getpos
	.cfi_endproc

	.globl	XS_IO__Seekable_setpos
	.align	16, 0x90
	.type	XS_IO__Seekable_setpos,@function
XS_IO__Seekable_setpos:                 # @XS_IO__Seekable_setpos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 32
.Ltmp18:
	.cfi_offset %rbx, -32
.Ltmp19:
	.cfi_offset %r14, -24
.Ltmp20:
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
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB1_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r15
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	56(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB1_4
# BB#3:                                 # %if.then8
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%rbx,8), %rsi
	callq	PerlIO_setpos
	movq	%rax, %r14
	jmp	.LBB1_5
.LBB1_4:                                # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB1_5:                                # %if.end11
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	je	.LBB1_9
# BB#6:                                 # %if.then16
	movq	PL_stack_base(%rip), %rcx
	movq	8(%rcx,%rbx,8), %rdi
	testq	%rax, %r14
	je	.LBB1_7
# BB#8:                                 # %if.else21
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, %rsi
	callq	Perl_sv_setiv
	jmp	.LBB1_9
.LBB1_7:                                # %if.then18
	movabsq	$.L.str2, %rsi
	movq	$10, %rdx
	callq	Perl_sv_setpvn
.LBB1_9:                                # %if.end25
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp21:
	.size	XS_IO__Seekable_setpos, .Ltmp21-XS_IO__Seekable_setpos
	.cfi_endproc

	.globl	XS_IO__File_new_tmpfile
	.align	16, 0x90
	.type	XS_IO__File_new_tmpfile,@function
XS_IO__File_new_tmpfile:                # @XS_IO__File_new_tmpfile
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp28:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp29:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp30:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp31:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp33:
	.cfi_def_cfa_offset 64
.Ltmp34:
	.cfi_offset %rbx, -48
.Ltmp35:
	.cfi_offset %r12, -40
.Ltmp36:
	.cfi_offset %r13, -32
.Ltmp37:
	.cfi_offset %r14, -24
.Ltmp38:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r13
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r13
	movq	$3, %rcx
	sarq	%cl, %r13
	subq	%rax, %rbx
	cmpq	$9, %rbx
	jb	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB2_2:                                # %if.end
	leaq	1(%r13), %r12
	cmpq	$8, %rbx
	jge	.LBB2_4
# BB#3:
	movabsq	$.L.str4, %r14
	jmp	.LBB2_7
.LBB2_4:                                # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r13,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB2_6
# BB#5:                                 # %cond.true
	movq	(%rdi), %rax
	movq	(%rax), %r14
	jmp	.LBB2_7
.LBB2_6:                                # %cond.false
	callq	Perl_sv_2pv_nolen
	movq	%rax, %r14
.LBB2_7:                                # %if.end14
	callq	PerlIO_tmpfile
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	Perl_newGVgen
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, PL_Sv(%rip)
	je	.LBB2_9
# BB#8:                                 # %land.rhs
	incq	8(%rbx)
.LBB2_9:                                # %land.end
	movq	(%rbx), %rax
	movq	80(%rax), %rdi
	movq	64(%rax), %rsi
	movq	72(%rax), %rdx
	movq	$2, %rcx
	callq	Perl_hv_delete
	movabsq	$.L.str5, %rsi
	movq	$3, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	%r15, (%rsp)
	movq	%rbx, %rdi
	callq	Perl_do_open
	testq	$255, %rax
	je	.LBB2_11
# BB#10:                                # %if.then24
	movq	%rbx, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r13,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r13,8), %r15
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_bless
	jmp	.LBB2_12
.LBB2_11:                               # %if.else33
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%r13,8)
.LBB2_12:                               # %if.end36
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp39:
	.size	XS_IO__File_new_tmpfile, .Ltmp39-XS_IO__File_new_tmpfile
	.cfi_endproc

	.globl	XS_IO__Poll__poll
	.align	16, 0x90
	.type	XS_IO__Poll__poll,@function
XS_IO__Poll__poll:                      # @XS_IO__Poll__poll
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp48:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp49:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp51:
	.cfi_def_cfa_offset 96
.Ltmp52:
	.cfi_offset %rbx, -48
.Ltmp53:
	.cfi_offset %r12, -40
.Ltmp54:
	.cfi_offset %r13, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_sp(%rip), %rdx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rsi
	movq	$3, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	movq	$3, %rcx
	movq	%rdx, %r14
	sarq	%cl, %r14
	cmpq	$7, %rdx
	jg	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB3_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	leaq	1(%rbx), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	$1, %rax
	je	.LBB3_4
# BB#3:                                 # %cond.true
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB3_5
.LBB3_4:                                # %cond.false
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	callq	Perl_sv_2iv
.LBB3_5:                                # %cond.end
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	-1(%r14), %rax
	movq	$63, %rcx
	shrq	%cl, %rax
	leaq	-1(%r14,%rax), %rax
	movq	$1, %r15
	movq	$1, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rdi
	movq	%rax, %rbx
	shlq	%cl, %rdi
	movq	$32, %rcx
	movq	%rdi, %r13
	sarq	%cl, %r13
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	$29, %rcx
	sarq	%cl, %rdi
	callq	Perl_newSV
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	(%rax), %rax
	movq	(%rax), %r14
	testq	%r13, %r13
	jle	.LBB3_14
# BB#6:                                 # %for.body.preheader
	movq	$3, %rcx
	movq	40(%rsp), %r13          # 8-byte Reload
	shlq	%cl, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rcx
	movq	16(%rcx,%r13), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	jne	.LBB3_8
# BB#9:                                 # %cond.false29
                                        #   in Loop: Header=BB3_7 Depth=1
	callq	Perl_sv_2iv
	movq	PL_stack_base(%rip), %rcx
	jmp	.LBB3_10
	.align	16, 0x90
.LBB3_8:                                # %cond.true23
                                        #   in Loop: Header=BB3_7 Depth=1
	movq	(%rdi), %rax
	movq	24(%rax), %rax
.LBB3_10:                               # %cond.end34
                                        #   in Loop: Header=BB3_7 Depth=1
	movl	%eax, -8(%r14,%r15,8)
	movq	24(%rcx,%r13), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	jne	.LBB3_11
# BB#12:                                # %cond.false50
                                        #   in Loop: Header=BB3_7 Depth=1
	callq	Perl_sv_2iv
	jmp	.LBB3_13
	.align	16, 0x90
.LBB3_11:                               # %cond.true44
                                        #   in Loop: Header=BB3_7 Depth=1
	movq	(%rdi), %rax
	movq	24(%rax), %rax
.LBB3_13:                               # %cond.end55
                                        #   in Loop: Header=BB3_7 Depth=1
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movw	%ax, -4(%r14,%r15,8)
	xorq	%rax, %rax
	movw	%ax, -2(%r14,%r15,8)
	movq	%r15, %rax
	andq	%r12, %rax
	incq	%r15
	addq	$16, %r13
	cmpq	%rcx, %rax
	jne	.LBB3_7
.LBB3_14:                               # %for.end
	movq	$32, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	%r14, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	Perl_my_poll
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%r15, %r15
	jg	.LBB3_15
# BB#16:                                # %for.end
	movabsq	$1, %rcx
	jmp	.LBB3_17
.LBB3_15:
	movabsq	$0, %rcx
.LBB3_17:                               # %for.end
	movq	40(%rsp), %r12          # 8-byte Reload
	testq	%rax, %rax
	movq	%rbx, %r15
	js	.LBB3_21
# BB#18:                                # %for.end
	testq	%rcx, %rcx
	jne	.LBB3_21
# BB#19:                                # %for.body72.preheader
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$1, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r15
	.align	16, 0x90
.LBB3_20:                               # %for.body72
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r12), %rdi
	movslq	-8(%r14,%rbx,8), %rsi
	callq	Perl_sv_setiv
	movq	PL_stack_base(%rip), %rax
	movq	24(%rax,%r12), %rdi
	movswq	-2(%r14,%rbx,8), %rsi
	callq	Perl_sv_setiv
	movq	%rbx, %rax
	andq	%r13, %rax
	incq	%rbx
	addq	$16, %r12
	cmpq	%r15, %rax
	jne	.LBB3_20
.LBB3_21:                               # %if.end92
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	Perl_sv_free
	movq	$32, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 8(%rcx,%rdx,8)
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp57:
	.size	XS_IO__Poll__poll, .Ltmp57-XS_IO__Poll__poll
	.cfi_endproc

	.globl	XS_IO__Handle_blocking
	.align	16, 0x90
	.type	XS_IO__Handle_blocking,@function
XS_IO__Handle_blocking:                 # @XS_IO__Handle_blocking
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp64:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp66:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp67:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp68:
	.cfi_def_cfa_offset 48
.Ltmp69:
	.cfi_offset %rbx, -48
.Ltmp70:
	.cfi_offset %r12, -40
.Ltmp71:
	.cfi_offset %r13, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	movq	%rdx, %rbx
	subq	%rsi, %rbx
	negq	%rsi
	leaq	-8(%rdx,%rsi), %rcx
	cmpq	$9, %rcx
	jb	.LBB4_2
# BB#1:                                 # %if.then
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB4_2:                                # %if.end
	movq	8(%rax,%r12,8), %rdi
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	56(%rax), %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	cmpq	$16, %rbx
	movq	%r15, %rax
	jl	.LBB4_6
# BB#3:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%r12,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB4_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB4_6
.LBB4_5:                                # %cond.false
	callq	Perl_sv_2iv
.LBB4_6:                                # %if.end17
	cmpq	$8, %rbx
	je	.LBB4_10
# BB#7:                                 # %cond.false21
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_8
# BB#9:                                 # %cond.false21
	movabsq	$1, %r15
	jmp	.LBB4_10
.LBB4_8:
	movabsq	$0, %r15
.LBB4_10:                               # %cond.end24
	leaq	1(%r12), %r13
	testq	%r14, %r14
	je	.LBB4_11
# BB#13:                                # %if.end.i
	movq	%r14, %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %rcx
	movq	$3, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fcntl
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB4_12
# BB#14:                                # %if.then3.i
	movq	$11, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	notq	%rbx
	andq	$1, %rbx
	testq	%r15, %r15
	je	.LBB4_15
# BB#16:                                # %if.else.i
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	testq	%r15, %r15
	jg	.LBB4_17
# BB#18:                                # %if.else.i
	movq	%rax, %r15
	jmp	.LBB4_19
.LBB4_11:                               # %if.then.i
	callq	__errno_location
	movq	$9, %rcx
	movl	%ecx, (%rax)
	jmp	.LBB4_12
.LBB4_15:                               # %if.then6.i
	movq	%rax, %r15
	orq	$2048, %r15             # imm = 0x800
	jmp	.LBB4_19
.LBB4_17:
	movq	%rax, %r15
	andq	$-2049, %r15            # imm = 0xFFFFFFFFFFFFF7FF
.LBB4_19:                               # %if.end12.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	je	.LBB4_23
# BB#20:                                # %if.then14.i
	movq	%r14, %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %rcx
	movq	$4, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r15, %rdx
	callq	fcntl
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB4_22
# BB#21:                                # %if.then14.i
	movq	%rbx, %rax
.LBB4_22:                               # %if.then14.i
	movq	%rax, %rbx
.LBB4_23:                               # %io_blocking.exit
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB4_12
# BB#24:                                # %if.then29
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r12,8)
	jmp	.LBB4_25
.LBB4_12:                               # %if.else37
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%r12,8)
.LBB4_25:                               # %return
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	XS_IO__Handle_blocking, .Ltmp74-XS_IO__Handle_blocking
	.cfi_endproc

	.globl	XS_IO__Handle_ungetc
	.align	16, 0x90
	.type	XS_IO__Handle_ungetc,@function
XS_IO__Handle_ungetc:                   # @XS_IO__Handle_ungetc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp81:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp82:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp83:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp84:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp85:
	.cfi_def_cfa_offset 48
.Ltmp86:
	.cfi_offset %rbx, -48
.Ltmp87:
	.cfi_offset %r12, -40
.Ltmp88:
	.cfi_offset %r13, -32
.Ltmp89:
	.cfi_offset %r14, -24
.Ltmp90:
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
	je	.LBB5_2
# BB#1:                                 # %if.then
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB5_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r13
	callq	Perl_sv_2io
	movq	PL_stack_base(%rip), %rcx
	movq	16(%rcx,%rbx,8), %rdi
	movq	(%rax), %rax
	movzbl	18(%rdi), %ecx
	movq	56(%rax), %r14
	testq	$1, %rcx
	je	.LBB5_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %r15
	jmp	.LBB5_5
.LBB5_4:                                # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %r15
.LBB5_5:                                # %cond.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	testq	$32, %rcx
	je	.LBB5_7
# BB#6:                                 # %cond.true17
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r12
	jmp	.LBB5_8
.LBB5_7:                                # %cond.false19
	callq	Perl_sv_newmortal
	movq	%rax, %r12
.LBB5_8:                                # %cond.end21
	testq	%r14, %r14
	je	.LBB5_10
# BB#9:                                 # %if.then24
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	PerlIO_ungetc
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	jmp	.LBB5_11
.LBB5_10:                               # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movq	$-1, %rsi
.LBB5_11:                               # %if.end27
	movq	PL_stack_base(%rip), %r14
	movq	%r12, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB5_13
# BB#12:                                # %if.then34
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB5_13:                               # %if.end36
	movq	%r12, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp91:
	.size	XS_IO__Handle_ungetc, .Ltmp91-XS_IO__Handle_ungetc
	.cfi_endproc

	.globl	XS_IO__Handle_error
	.align	16, 0x90
	.type	XS_IO__Handle_error,@function
XS_IO__Handle_error:                    # @XS_IO__Handle_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 48
.Ltmp102:
	.cfi_offset %rbx, -40
.Ltmp103:
	.cfi_offset %r12, -32
.Ltmp104:
	.cfi_offset %r14, -24
.Ltmp105:
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
	cmpq	$8, %rdx
	je	.LBB6_2
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB6_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r12
	callq	Perl_sv_2io
	movq	PL_op(%rip), %rcx
	movq	(%rax), %rax
	movzbl	37(%rcx), %edx
	movq	56(%rax), %r14
	testq	$32, %rdx
	je	.LBB6_4
# BB#3:                                 # %cond.true
	movq	24(%rcx), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r15
	jmp	.LBB6_5
.LBB6_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r15
.LBB6_5:                                # %cond.end
	testq	%r14, %r14
	je	.LBB6_7
# BB#6:                                 # %if.then9
	movq	%r14, %rdi
	callq	Perl_PerlIO_error
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	jmp	.LBB6_8
.LBB6_7:                                # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movq	$-1, %rsi
.LBB6_8:                                # %if.end12
	movq	PL_stack_base(%rip), %r14
	movq	%r15, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r15), %eax
	testq	$64, %rax
	je	.LBB6_10
# BB#9:                                 # %if.then18
	movq	%r15, %rdi
	callq	Perl_mg_set
.LBB6_10:                               # %if.end20
	movq	%r15, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp106:
	.size	XS_IO__Handle_error, .Ltmp106-XS_IO__Handle_error
	.cfi_endproc

	.globl	XS_IO__Handle_clearerr
	.align	16, 0x90
	.type	XS_IO__Handle_clearerr,@function
XS_IO__Handle_clearerr:                 # @XS_IO__Handle_clearerr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp112:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp113:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp116:
	.cfi_def_cfa_offset 48
.Ltmp117:
	.cfi_offset %rbx, -40
.Ltmp118:
	.cfi_offset %r12, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
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
	cmpq	$8, %rdx
	je	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB7_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r12
	callq	Perl_sv_2io
	movq	PL_op(%rip), %rcx
	movq	(%rax), %rax
	movzbl	37(%rcx), %edx
	movq	56(%rax), %r14
	testq	$32, %rdx
	je	.LBB7_4
# BB#3:                                 # %cond.true
	movq	24(%rcx), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r15
	jmp	.LBB7_5
.LBB7_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r15
.LBB7_5:                                # %cond.end
	testq	%r14, %r14
	je	.LBB7_7
# BB#6:                                 # %if.then9
	movq	%r14, %rdi
	callq	Perl_PerlIO_clearerr
	xorq	%rsi, %rsi
	jmp	.LBB7_8
.LBB7_7:                                # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movq	$-1, %rsi
.LBB7_8:                                # %if.end11
	movq	PL_stack_base(%rip), %r14
	movq	%r15, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r15), %eax
	testq	$64, %rax
	je	.LBB7_10
# BB#9:                                 # %if.then17
	movq	%r15, %rdi
	callq	Perl_mg_set
.LBB7_10:                               # %if.end19
	movq	%r15, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	XS_IO__Handle_clearerr, .Ltmp121-XS_IO__Handle_clearerr
	.cfi_endproc

	.globl	XS_IO__Handle_untaint
	.align	16, 0x90
	.type	XS_IO__Handle_untaint,@function
XS_IO__Handle_untaint:                  # @XS_IO__Handle_untaint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp127:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp128:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp129:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp130:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp131:
	.cfi_def_cfa_offset 48
.Ltmp132:
	.cfi_offset %rbx, -40
.Ltmp133:
	.cfi_offset %r12, -32
.Ltmp134:
	.cfi_offset %r14, -24
.Ltmp135:
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
	cmpq	$8, %rdx
	je	.LBB8_2
# BB#1:                                 # %if.then
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB8_2:                                # %if.end
	movq	PL_op(%rip), %rcx
	movzbl	37(%rcx), %edx
	movq	8(%rax,%rbx,8), %r14
	leaq	1(%rbx), %r12
	testq	$32, %rdx
	je	.LBB8_4
# BB#3:                                 # %cond.true
	movq	24(%rcx), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r15
	jmp	.LBB8_5
.LBB8_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r15
.LBB8_5:                                # %cond.end
	movq	%r14, %rdi
	callq	Perl_sv_2io
	testq	%rax, %rax
	je	.LBB8_7
# BB#6:                                 # %if.then9
	movq	(%rax), %rax
	movzbl	163(%rax), %ecx
	orq	$16, %rcx
	movb	%cl, 163(%rax)
	xorq	%rsi, %rsi
	jmp	.LBB8_8
.LBB8_7:                                # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movq	$-1, %rsi
.LBB8_8:                                # %if.end13
	movq	PL_stack_base(%rip), %r14
	movq	%r15, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r15), %eax
	testq	$64, %rax
	je	.LBB8_10
# BB#9:                                 # %if.then19
	movq	%r15, %rdi
	callq	Perl_mg_set
.LBB8_10:                               # %if.end21
	movq	%r15, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp136:
	.size	XS_IO__Handle_untaint, .Ltmp136-XS_IO__Handle_untaint
	.cfi_endproc

	.globl	XS_IO__Handle_flush
	.align	16, 0x90
	.type	XS_IO__Handle_flush,@function
XS_IO__Handle_flush:                    # @XS_IO__Handle_flush
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp141:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp142:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp143:
	.cfi_def_cfa_offset 32
.Ltmp144:
	.cfi_offset %rbx, -32
.Ltmp145:
	.cfi_offset %r14, -24
.Ltmp146:
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
	cmpq	$8, %rdx
	je	.LBB9_2
# BB#1:                                 # %if.then
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB9_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r15
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	64(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB9_4
# BB#3:                                 # %if.then6
	callq	Perl_PerlIO_flush
	movq	%rax, %r14
	jmp	.LBB9_5
.LBB9_4:                                # %if.else
	callq	__errno_location
	movq	$22, %rcx
	movl	%ecx, (%rax)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB9_5:                                # %if.end9
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	je	.LBB9_9
# BB#6:                                 # %if.then14
	movq	PL_stack_base(%rip), %rcx
	movq	8(%rcx,%rbx,8), %rdi
	testq	%rax, %r14
	je	.LBB9_7
# BB#8:                                 # %if.else19
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, %rsi
	callq	Perl_sv_setiv
	jmp	.LBB9_9
.LBB9_7:                                # %if.then16
	movabsq	$.L.str2, %rsi
	movq	$10, %rdx
	callq	Perl_sv_setpvn
.LBB9_9:                                # %if.end23
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp147:
	.size	XS_IO__Handle_flush, .Ltmp147-XS_IO__Handle_flush
	.cfi_endproc

	.globl	XS_IO__Handle_setbuf
	.align	16, 0x90
	.type	XS_IO__Handle_setbuf,@function
XS_IO__Handle_setbuf:                   # @XS_IO__Handle_setbuf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp150:
	.cfi_def_cfa_offset 16
.Ltmp151:
	.cfi_offset %rbx, -16
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
	cmpq	$7, %rdx
	jg	.LBB10_2
# BB#1:                                 # %if.then
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB10_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2io
	movq	(%rax), %rax
	cmpq	$0, 64(%rax)
	je	.LBB10_4
# BB#3:                                 # %if.then6
	movabsq	$.L.str53, %rdi
	movabsq	$.L.str14, %rsi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB10_4:                               # %if.end8
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	retq
.Ltmp152:
	.size	XS_IO__Handle_setbuf, .Ltmp152-XS_IO__Handle_setbuf
	.cfi_endproc

	.globl	XS_IO__Handle_setvbuf
	.align	16, 0x90
	.type	XS_IO__Handle_setvbuf,@function
XS_IO__Handle_setvbuf:                  # @XS_IO__Handle_setvbuf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp156:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp157:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp158:
	.cfi_def_cfa_offset 32
.Ltmp159:
	.cfi_offset %rbx, -24
.Ltmp160:
	.cfi_offset %r14, -16
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
	leaq	1(%rbx), %r14
	subq	%rax, %rdx
	cmpq	$32, %rdx
	je	.LBB11_2
# BB#1:                                 # %if.then
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB11_2:                               # %if.end
	movabsq	$.L.str53, %rdi
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	callq	Perl_croak
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp161:
	.size	XS_IO__Handle_setvbuf, .Ltmp161-XS_IO__Handle_setvbuf
	.cfi_endproc

	.globl	XS_IO__Handle_sync
	.align	16, 0x90
	.type	XS_IO__Handle_sync,@function
XS_IO__Handle_sync:                     # @XS_IO__Handle_sync
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp165:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp167:
	.cfi_def_cfa_offset 32
.Ltmp168:
	.cfi_offset %rbx, -24
.Ltmp169:
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
	leaq	1(%rbx), %r14
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB12_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2io
	movabsq	$.L.str53, %rdi
	movabsq	$.L.str18, %rsi
	xorq	%rax, %rax
	callq	Perl_croak
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp170:
	.size	XS_IO__Handle_sync, .Ltmp170-XS_IO__Handle_sync
	.cfi_endproc

	.globl	XS_IO__Socket_sockatmark
	.align	16, 0x90
	.type	XS_IO__Socket_sockatmark,@function
XS_IO__Socket_sockatmark:               # @XS_IO__Socket_sockatmark
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp177:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp178:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp179:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp180:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp182:
	.cfi_def_cfa_offset 64
.Ltmp183:
	.cfi_offset %rbx, -48
.Ltmp184:
	.cfi_offset %r12, -40
.Ltmp185:
	.cfi_offset %r13, -32
.Ltmp186:
	.cfi_offset %r14, -24
.Ltmp187:
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
	cmpq	$8, %rdx
	je	.LBB13_2
# BB#1:                                 # %if.then
	movabsq	$.L.str19, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB13_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r15
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	56(%rax), %rdi
	callq	Perl_PerlIO_fileno
	movq	%rax, %rcx
	xorq	%rax, %rax
	movl	%eax, 12(%rsp)
	leaq	12(%rsp), %rdx
	movq	$35077, %rsi            # imm = 0x8905
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	ioctl
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB13_4
# BB#3:                                 # %if.then9
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%rbx,8)
	jmp	.LBB13_8
.LBB13_4:                               # %if.end14
	movl	12(%rsp), %r12d
	movq	$32, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	cmpq	%r13, %r12
	movq	%rax, 8(%rcx,%rbx,8)
	je	.LBB13_8
# BB#5:                                 # %if.then19
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	testq	%r12, %r12
	je	.LBB13_6
# BB#7:                                 # %if.else
	movq	%r14, %rsi
	callq	Perl_sv_setiv
	jmp	.LBB13_8
.LBB13_6:                               # %if.then21
	movabsq	$.L.str2, %rsi
	movq	$10, %rdx
	callq	Perl_sv_setpvn
.LBB13_8:                               # %return
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp188:
	.size	XS_IO__Socket_sockatmark, .Ltmp188-XS_IO__Socket_sockatmark
	.cfi_endproc

	.globl	boot_IO
	.align	16, 0x90
	.type	boot_IO,@function
boot_IO:                                # @boot_IO
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp192:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp193:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp194:
	.cfi_def_cfa_offset 32
.Ltmp195:
	.cfi_offset %rbx, -24
.Ltmp196:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movabsq	$.L.str21, %rdi
	movabsq	$XS_IO__Seekable_getpos, %rsi
	movabsq	$.L.str20, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str22, %rdi
	movabsq	$XS_IO__Seekable_setpos, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str23, %rdi
	movabsq	$XS_IO__File_new_tmpfile, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str24, %rdi
	movabsq	$XS_IO__Poll__poll, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str25, %rdi
	movabsq	$XS_IO__Handle_blocking, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str26, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str27, %rdi
	movabsq	$XS_IO__Handle_ungetc, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str28, %rdi
	movabsq	$XS_IO__Handle_error, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str29, %rdi
	movabsq	$XS_IO__Handle_clearerr, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str30, %rdi
	movabsq	$XS_IO__Handle_untaint, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str31, %rdi
	movabsq	$XS_IO__Handle_flush, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str14, %rdi
	movabsq	$XS_IO__Handle_setbuf, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str16, %rdi
	movabsq	$XS_IO__Handle_setvbuf, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str18, %rdi
	movabsq	$XS_IO__Handle_sync, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str32, %rdi
	movabsq	$XS_IO__Socket_sockatmark, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str33, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str34, %rdi
	movq	$8, %rsi
	movq	$1, %rdx
	callq	Perl_gv_stashpvn
	movq	%rax, %rbx
	movq	$1, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str35, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$2, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str36, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$4, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str37, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$64, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str38, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$4, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str39, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$128, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str40, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$256, %rdi              # imm = 0x100
	callq	Perl_newSViv
	movabsq	$.L.str41, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$64, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str42, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$8, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str43, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$16, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str44, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$32, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str45, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movabsq	$.L.str46, %rdi
	movq	$10, %rsi
	movq	$1, %rdx
	callq	Perl_gv_stashpvn
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str47, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$1, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str48, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$2, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str49, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str50, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$1, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str51, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	$2, %rdi
	callq	Perl_newSViv
	movabsq	$.L.str52, %rsi
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	Perl_newCONSTSUB
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r14,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp197:
	.size	boot_IO, .Ltmp197-boot_IO
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: IO::Seekable::getpos(handle)"
	.size	.L.str, 36

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Usage: IO::Seekable::setpos(handle, pos)"
	.size	.L.str1, 41

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"0 but true"
	.size	.L.str2, 11

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Usage: IO::File::new_tmpfile(packname = \"IO::File\")"
	.size	.L.str3, 52

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"IO::File"
	.size	.L.str4, 9

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"+>&"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Usage: IO::Poll::_poll(timeout, ...)"
	.size	.L.str6, 37

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Usage: IO::Handle::blocking(handle, blk=-1)"
	.size	.L.str7, 44

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Usage: IO::Handle::ungetc(handle, c)"
	.size	.L.str8, 37

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Usage: IO::Handle::error(handle)"
	.size	.L.str9, 33

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Usage: IO::Handle::clearerr(handle)"
	.size	.L.str10, 36

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Usage: IO::Handle::untaint(handle)"
	.size	.L.str11, 35

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Usage: IO::Handle::flush(handle)"
	.size	.L.str12, 33

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Usage: IO::Handle::setbuf(handle, ...)"
	.size	.L.str13, 39

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"IO::Handle::setbuf"
	.size	.L.str14, 19

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Usage: IO::Handle::setvbuf(handle, buf, type, size)"
	.size	.L.str15, 52

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"IO::Handle::setvbuf"
	.size	.L.str16, 20

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Usage: IO::Handle::sync(handle)"
	.size	.L.str17, 32

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"IO::Handle::sync"
	.size	.L.str18, 17

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Usage: IO::Socket::sockatmark(sock)"
	.size	.L.str19, 36

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"IO.c"
	.size	.L.str20, 5

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"IO::Seekable::getpos"
	.size	.L.str21, 21

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"IO::Seekable::setpos"
	.size	.L.str22, 21

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"IO::File::new_tmpfile"
	.size	.L.str23, 22

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"IO::Poll::_poll"
	.size	.L.str24, 16

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"IO::Handle::blocking"
	.size	.L.str25, 21

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"$;$"
	.size	.L.str26, 4

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"IO::Handle::ungetc"
	.size	.L.str27, 19

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"IO::Handle::error"
	.size	.L.str28, 18

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"IO::Handle::clearerr"
	.size	.L.str29, 21

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"IO::Handle::untaint"
	.size	.L.str30, 20

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"IO::Handle::flush"
	.size	.L.str31, 18

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"IO::Socket::sockatmark"
	.size	.L.str32, 23

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"$"
	.size	.L.str33, 2

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"IO::Poll"
	.size	.L.str34, 9

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"POLLIN"
	.size	.L.str35, 7

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"POLLPRI"
	.size	.L.str36, 8

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"POLLOUT"
	.size	.L.str37, 8

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"POLLRDNORM"
	.size	.L.str38, 11

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"POLLWRNORM"
	.size	.L.str39, 11

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"POLLRDBAND"
	.size	.L.str40, 11

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"POLLWRBAND"
	.size	.L.str41, 11

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"POLLNORM"
	.size	.L.str42, 9

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"POLLERR"
	.size	.L.str43, 8

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"POLLHUP"
	.size	.L.str44, 8

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"POLLNVAL"
	.size	.L.str45, 9

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"IO::Handle"
	.size	.L.str46, 11

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"_IOFBF"
	.size	.L.str47, 7

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"_IOLBF"
	.size	.L.str48, 7

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"_IONBF"
	.size	.L.str49, 7

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"SEEK_SET"
	.size	.L.str50, 9

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"SEEK_CUR"
	.size	.L.str51, 9

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"SEEK_END"
	.size	.L.str52, 9

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"%s not implemented on this architecture"
	.size	.L.str53, 40


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
