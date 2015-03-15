	.file	"dump.c.bc"
	.text
	.globl	Perl_dump_indent
	.align	16, 0x90
	.type	Perl_dump_indent,@function
Perl_dump_indent:                       # @Perl_dump_indent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp5:
	.cfi_def_cfa_offset 96
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	PL_dumpindent(%rip), %rdx
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$24, 48(%rsp)
	imulq	%rdi, %rdx
	movabsq	$.L.str, %rsi
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	PerlIO_printf
	leaq	48(%rsp), %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	PerlIO_vprintf
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	Perl_dump_indent, .Ltmp8-Perl_dump_indent
	.cfi_endproc

	.globl	Perl_dump_vindent
	.align	16, 0x90
	.type	Perl_dump_vindent,@function
Perl_dump_vindent:                      # @Perl_dump_vindent
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp13:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 32
.Ltmp16:
	.cfi_offset %rbx, -32
.Ltmp17:
	.cfi_offset %r14, -24
.Ltmp18:
	.cfi_offset %r15, -16
	movq	PL_dumpindent(%rip), %r8
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rbx
	imulq	%rdi, %r8
	movabsq	$.L.str, %rsi
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r8, %rdx
	callq	PerlIO_printf
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	PerlIO_vprintf  # TAILCALL
.Ltmp19:
	.size	Perl_dump_vindent, .Ltmp19-Perl_dump_vindent
	.cfi_endproc

	.globl	Perl_dump_all
	.align	16, 0x90
	.type	Perl_dump_all,@function
Perl_dump_all:                          # @Perl_dump_all
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 16
.Ltmp23:
	.cfi_offset %rbx, -16
	callq	Perl_PerlIO_stderr
	movq	%rax, %rdi
	callq	Perl_PerlIO_setlinebuf
	movq	PL_main_root(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB2_2
# BB#1:                                 # %if.then
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	Perl_do_op_dump
.LBB2_2:                                # %if.end
	movq	PL_defstash(%rip), %rdi
	popq	%rbx
	jmp	Perl_dump_packsubs  # TAILCALL
.Ltmp24:
	.size	Perl_dump_all, .Ltmp24-Perl_dump_all
	.cfi_endproc

	.globl	Perl_op_dump
	.align	16, 0x90
	.type	Perl_op_dump,@function
Perl_op_dump:                           # @Perl_op_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 16
.Ltmp28:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	jmp	Perl_do_op_dump  # TAILCALL
.Ltmp29:
	.size	Perl_op_dump, .Ltmp29-Perl_op_dump
	.cfi_endproc

	.globl	Perl_dump_packsubs
	.align	16, 0x90
	.type	Perl_dump_packsubs,@function
Perl_dump_packsubs:                     # @Perl_dump_packsubs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -40
.Ltmp41:
	.cfi_offset %r12, -32
.Ltmp42:
	.cfi_offset %r14, -24
.Ltmp43:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rcx
	movq	(%rcx), %rax
	testq	%rax, %rax
	je	.LBB4_19
# BB#1:                                 # %for.cond.preheader
	cmpq	$0, 16(%rcx)
	js	.LBB4_19
# BB#2:
	xorq	%r15, %r15
	jmp	.LBB4_3
	.align	16, 0x90
.LBB4_18:                               # %for.inc38.for.body_crit_edge
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	(%rax), %rax
	incq	%r15
.LBB4_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_4 Depth 2
	movq	(%rax,%r15,8), %rbx
	jmp	.LBB4_4
	.align	16, 0x90
.LBB4_16:                               # %for.inc
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	(%rbx), %rbx
.LBB4_4:                                # %for.body
                                        #   Parent Loop BB4_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB4_17
# BB#5:                                 # %for.body7
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	16(%rbx), %r12
	movzbl	16(%r12), %eax
	cmpq	$13, %rax
	jne	.LBB4_16
# BB#6:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	(%r12), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB4_16
# BB#7:                                 # %if.end12
                                        #   in Loop: Header=BB4_4 Depth=2
	cmpq	$0, 64(%rax)
	jne	.LBB4_10
# BB#8:                                 # %cond.false
                                        #   in Loop: Header=BB4_4 Depth=2
	cmpq	$0, 56(%rax)
	je	.LBB4_10
# BB#9:                                 # %if.then19
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	%r12, %rdi
	callq	Perl_dump_sub
	movq	(%r12), %rax
	movq	56(%rax), %rax
.LBB4_10:                               # %if.end20
                                        #   in Loop: Header=BB4_4 Depth=2
	cmpq	$0, 24(%rax)
	je	.LBB4_12
# BB#11:                                # %if.then24
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	%r12, %rdi
	callq	Perl_dump_form
.LBB4_12:                               # %if.end25
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	8(%rbx), %rax
	movq	8(%rax), %rcx
	movzbl	15(%rcx,%rax), %eax
	cmpq	$58, %rax
	jne	.LBB4_16
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_16
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_4 Depth=2
	movq	PL_defstash(%rip), %rax
	cmpq	%rax, %rdi
	je	.LBB4_16
# BB#15:                                # %if.then36
                                        #   in Loop: Header=BB4_4 Depth=2
	callq	Perl_dump_packsubs
	jmp	.LBB4_16
	.align	16, 0x90
.LBB4_17:                               # %for.inc38
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	(%r14), %rax
	movq	16(%rax), %rcx
	cmpq	%rcx, %r15
	jl	.LBB4_18
.LBB4_19:                               # %for.end39
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp44:
	.size	Perl_dump_packsubs, .Ltmp44-Perl_dump_packsubs
	.cfi_endproc

	.globl	Perl_dump_sub
	.align	16, 0x90
	.type	Perl_dump_sub,@function
Perl_dump_sub:                          # @Perl_dump_sub
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 32
.Ltmp51:
	.cfi_offset %rbx, -24
.Ltmp52:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	Perl_sv_newmortal
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_gv_fullname4
	callq	Perl_PerlIO_stderr
	movq	(%rbx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, %rsi
	movabsq	$.L.str2, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	Perl_dump_indent
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rax
	movq	(%rax), %rax
	cmpq	$0, 80(%rax)
	je	.LBB5_1
# BB#3:                                 # %if.then
	callq	Perl_PerlIO_stderr
	movq	(%r14), %rcx
	movq	56(%rcx), %rcx
	movq	56(%rcx), %rcx
	movq	(%rcx), %rdx
	movq	%rax, %rsi
	movq	80(%rdx), %rcx
	movq	88(%rdx), %r8
	movabsq	$.L.str3, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_dump_indent  # TAILCALL
.LBB5_1:                                # %if.else
	movq	72(%rax), %rbx
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	testq	%rbx, %rbx
	je	.LBB5_2
# BB#4:                                 # %if.then19
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_do_op_dump  # TAILCALL
.LBB5_2:                                # %if.else25
	movabsq	$.L.str4, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_dump_indent  # TAILCALL
.Ltmp53:
	.size	Perl_dump_sub, .Ltmp53-Perl_dump_sub
	.cfi_endproc

	.globl	Perl_dump_form
	.align	16, 0x90
	.type	Perl_dump_form,@function
Perl_dump_form:                         # @Perl_dump_form
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp59:
	.cfi_def_cfa_offset 32
.Ltmp60:
	.cfi_offset %rbx, -24
.Ltmp61:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	callq	Perl_sv_newmortal
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_gv_fullname4
	callq	Perl_PerlIO_stderr
	movq	(%rbx), %rcx
	movq	(%rcx), %rcx
	movq	%rax, %rsi
	movabsq	$.L.str5, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	callq	Perl_dump_indent
	movq	(%r14), %rax
	movq	56(%rax), %rax
	movq	24(%rax), %rax
	movq	(%rax), %rax
	movq	72(%rax), %rbx
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	testq	%rbx, %rbx
	je	.LBB6_2
# BB#1:                                 # %if.then
	xorq	%rdi, %rdi
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_do_op_dump  # TAILCALL
.LBB6_2:                                # %if.else
	movabsq	$.L.str4, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_dump_indent  # TAILCALL
.Ltmp62:
	.size	Perl_dump_form, .Ltmp62-Perl_dump_form
	.cfi_endproc

	.globl	Perl_dump_eval
	.align	16, 0x90
	.type	Perl_dump_eval,@function
Perl_dump_eval:                         # @Perl_dump_eval
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 16
.Ltmp66:
	.cfi_offset %rbx, -16
	movq	PL_eval_root(%rip), %rbx
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	jmp	Perl_do_op_dump  # TAILCALL
.Ltmp67:
	.size	Perl_dump_eval, .Ltmp67-Perl_dump_eval
	.cfi_endproc

	.globl	Perl_pv_display
	.align	16, 0x90
	.type	Perl_pv_display,@function
Perl_pv_display:                        # @Perl_pv_display
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
	subq	$16, %rsp
.Ltmp79:
	.cfi_def_cfa_offset 64
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
	movq	%r8, %r12
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%rax, %rax
	cmpq	%r15, %rcx
	jbe	.LBB8_4
# BB#1:                                 # %land.rhs
	movzbl	(%rbx,%r15), %eax
	testq	%rax, %rax
	jne	.LBB8_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB8_4
.LBB8_2:
	movabsq	$0, %rax
.LBB8_4:                                # %land.end
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str6, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setpvn
	testq	%r15, %r15
	je	.LBB8_59
# BB#5:                                 # %for.body.lr.ph
	testq	%r12, %r12
	je	.LBB8_6
# BB#32:
	movabsq	$0, %r13
	jmp	.LBB8_33
	.align	16, 0x90
.LBB8_34:                               # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	movsbq	(%rbx), %rdx
	cmpq	$91, %rdx
	jg	.LBB8_44
# BB#35:                                # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	cmpq	$33, %rdx
	jg	.LBB8_42
# BB#36:                                # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	leaq	-9(%rdx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$4, %rax
	ja	.LBB8_46
# BB#37:                                # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	jmpq	*.LJTI8_0(,%rax,8)
.LBB8_38:                               # %sw.bb
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str7, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
	.align	16, 0x90
.LBB8_44:                               # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$92, %rax
	jne	.LBB8_46
# BB#45:                                # %sw.bb11
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str12, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
	.align	16, 0x90
.LBB8_42:                               # %if.end
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$34, %rax
	jne	.LBB8_46
# BB#43:                                # %sw.bb10
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str11, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
	.align	16, 0x90
.LBB8_46:                               # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	cmpq	$32, %rdx
	movq	%r13, %rax
	jle	.LBB8_48
# BB#47:                                # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	movabsq	$1, %rax
.LBB8_48:                               # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	andq	$255, %rdx
	cmpq	$127, %rdx
	movq	%r13, %rcx
	je	.LBB8_50
# BB#49:                                # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	movabsq	$1, %rcx
.LBB8_50:                               # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	testq	%rax, %rcx
	jne	.LBB8_52
# BB#51:                                # %sw.default
                                        #   in Loop: Header=BB8_33 Depth=1
	cmpq	$32, %rdx
	je	.LBB8_52
# BB#53:                                # %if.else
                                        #   in Loop: Header=BB8_33 Depth=1
	cmpq	$1, %r15
	je	.LBB8_56
# BB#54:                                # %land.lhs.true24
                                        #   in Loop: Header=BB8_33 Depth=1
	movzbl	1(%rbx), %eax
	addq	$-48, %rax
	andq	$255, %rax
	cmpq	$9, %rax
	ja	.LBB8_56
# BB#55:                                # %if.then33
                                        #   in Loop: Header=BB8_33 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str13, %rsi
	jmp	.LBB8_57
	.align	16, 0x90
.LBB8_52:                               # %if.then22
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
.LBB8_56:                               # %if.else35
                                        #   in Loop: Header=BB8_33 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str14, %rsi
.LBB8_57:                               # %for.inc
                                        #   in Loop: Header=BB8_33 Depth=1
	callq	Perl_sv_catpvf
.LBB8_58:                               # %for.inc
                                        #   in Loop: Header=BB8_33 Depth=1
	incq	%rbx
	decq	%r15
	jne	.LBB8_33
	jmp	.LBB8_59
.LBB8_39:                               # %sw.bb7
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str8, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
.LBB8_41:                               # %sw.bb9
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str10, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
.LBB8_40:                               # %sw.bb8
                                        #   in Loop: Header=BB8_33 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str9, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_58
	.align	16, 0x90
.LBB8_33:                               # %land.lhs.true
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rax
	movq	8(%rax), %rax
	cmpq	%r12, %rax
	jb	.LBB8_34
# BB#63:                                # %for.end
	movabsq	$.L.str6, %rsi
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	callq	Perl_sv_catpvn_flags
	movabsq	$.L.str15, %rsi
	movq	$3, %rdx
	jmp	.LBB8_60
.LBB8_6:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$0, %r12
	jmp	.LBB8_7
.LBB8_17:                               # %sw.bb7.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str8, %rsi
	jmp	.LBB8_30
.LBB8_15:                               # %sw.bb9.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str10, %rsi
	jmp	.LBB8_30
.LBB8_16:                               # %sw.bb8.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str9, %rsi
	jmp	.LBB8_30
	.align	16, 0x90
.LBB8_7:                                # %if.end.us
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%rbx), %rdx
	cmpq	$91, %rdx
	jg	.LBB8_11
# BB#8:                                 # %if.end.us
                                        #   in Loop: Header=BB8_7 Depth=1
	cmpq	$33, %rdx
	jg	.LBB8_13
# BB#9:                                 # %if.end.us
                                        #   in Loop: Header=BB8_7 Depth=1
	leaq	-9(%rdx), %rax
	andq	%r13, %rax
	cmpq	$4, %rax
	ja	.LBB8_19
# BB#10:                                # %if.end.us
                                        #   in Loop: Header=BB8_7 Depth=1
	jmpq	*.LJTI8_1(,%rax,8)
.LBB8_18:                               # %sw.bb.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str7, %rsi
	jmp	.LBB8_30
	.align	16, 0x90
.LBB8_11:                               # %if.end.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	$92, %rax
	jne	.LBB8_19
# BB#12:                                # %sw.bb11.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str12, %rsi
	jmp	.LBB8_30
	.align	16, 0x90
.LBB8_13:                               # %if.end.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	$34, %rax
	jne	.LBB8_19
# BB#14:                                # %sw.bb10.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movabsq	$.L.str11, %rsi
	jmp	.LBB8_30
	.align	16, 0x90
.LBB8_19:                               # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	cmpq	$32, %rdx
	movq	%r12, %rax
	jle	.LBB8_21
# BB#20:                                # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movabsq	$1, %rax
.LBB8_21:                               # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	andq	$255, %rdx
	cmpq	$127, %rdx
	movq	%r12, %rcx
	je	.LBB8_23
# BB#22:                                # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movabsq	$1, %rcx
.LBB8_23:                               # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	testq	%rax, %rcx
	jne	.LBB8_29
# BB#24:                                # %sw.default.us
                                        #   in Loop: Header=BB8_7 Depth=1
	cmpq	$32, %rdx
	je	.LBB8_29
# BB#25:                                # %if.else.us
                                        #   in Loop: Header=BB8_7 Depth=1
	cmpq	$1, %r15
	je	.LBB8_28
# BB#26:                                # %land.lhs.true24.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movzbl	1(%rbx), %eax
	addq	$-48, %rax
	andq	$255, %rax
	cmpq	$9, %rax
	ja	.LBB8_28
# BB#27:                                # %if.then33.us
                                        #   in Loop: Header=BB8_7 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str13, %rsi
	callq	Perl_sv_catpvf
	jmp	.LBB8_31
	.align	16, 0x90
.LBB8_29:                               # %if.then22.us
                                        #   in Loop: Header=BB8_7 Depth=1
	movq	$1, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
.LBB8_30:                               # %for.inc.us
                                        #   in Loop: Header=BB8_7 Depth=1
	callq	Perl_sv_catpvn_flags
	jmp	.LBB8_31
.LBB8_28:                               # %if.else35.us
                                        #   in Loop: Header=BB8_7 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str14, %rsi
	callq	Perl_sv_catpvf
	.align	16, 0x90
.LBB8_31:                               # %for.inc.us
                                        #   in Loop: Header=BB8_7 Depth=1
	incq	%rbx
	decq	%r15
	jne	.LBB8_7
.LBB8_59:                               # %if.end41.critedge
	movabsq	$.L.str6, %rsi
	movq	$1, %rdx
.LBB8_60:                               # %if.end41
	movq	$2, %rcx
	movq	%r14, %rdi
	callq	Perl_sv_catpvn_flags
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB8_62
# BB#61:                                # %if.then43
	movabsq	$.L.str16, %rsi
	movq	$2, %rdx
	movq	$2, %rcx
	movq	%r14, %rdi
	callq	Perl_sv_catpvn_flags
.LBB8_62:                               # %if.end44
	movq	(%r14), %rax
	movq	(%rax), %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp85:
	.size	Perl_pv_display, .Ltmp85-Perl_pv_display
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_38
	.quad	.LBB8_39
	.quad	.LBB8_46
	.quad	.LBB8_41
	.quad	.LBB8_40
.LJTI8_1:
	.quad	.LBB8_18
	.quad	.LBB8_17
	.quad	.LBB8_19
	.quad	.LBB8_15
	.quad	.LBB8_16

	.text
	.globl	Perl_sv_peek
	.align	16, 0x90
	.type	Perl_sv_peek,@function
Perl_sv_peek:                           # @Perl_sv_peek
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
	subq	$16, %rsp
.Ltmp97:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %r14
	callq	Perl_sv_newmortal
	movq	%rax, %r13
	movabsq	$.L.str1, %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	movq	%r13, %rdi
	callq	Perl_sv_setpvn
	movabsq	$PL_sv_undef, %rcx
	movabsq	$PL_sv_no, %r15
	movabsq	$PL_sv_yes, %rbx
	jmp	.LBB9_1
	.align	16, 0x90
.LBB9_45:                               # %if.end129
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	(%r14), %rax
	movq	(%rax), %r14
	movabsq	$PL_sv_undef, %rcx
.LBB9_1:                                # %retry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_34 Depth 2
	cmpq	$1431655765, %r14       # imm = 0x55555555
	je	.LBB9_5
# BB#2:                                 # %retry
                                        #   in Loop: Header=BB9_1 Depth=1
	testq	%r14, %r14
	je	.LBB9_3
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB9_1 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$85, %rax
	je	.LBB9_5
# BB#6:                                 # %if.else3
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	%rcx, %r14
	je	.LBB9_10
# BB#7:                                 # %if.else3
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	%r15, %r14
	je	.LBB9_10
# BB#8:                                 # %if.else3
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	%rbx, %r14
	je	.LBB9_10
# BB#9:                                 # %if.else3
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$PL_sv_placeholder, %rax
	cmpq	%rax, %r14
	je	.LBB9_10
# BB#26:                                # %if.else81
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	$0, 8(%r14)
	jne	.LBB9_28
# BB#27:                                # %if.then84
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str24, %rsi
	callq	Perl_sv_catpv
	incq	%r12
	jmp	.LBB9_42
	.align	16, 0x90
.LBB9_10:                               # %if.then11
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r13, %rdi
	cmpq	%rcx, %r14
	jne	.LBB9_12
# BB#11:                                # %if.then13
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$.L.str19, %rsi
	jmp	.LBB9_24
.LBB9_28:                               # %if.else85
                                        #   in Loop: Header=BB9_1 Depth=1
	testq	$262144, PL_debug(%rip) # imm = 0x40000
	je	.LBB9_42
# BB#29:                                # %if.then88
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	PL_tmps_ix(%rip), %rax
	testq	%rax, %rax
	js	.LBB9_30
# BB#33:                                # %for.body.lr.ph
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	PL_tmps_stack(%rip), %rcx
	incq	%rax
	.align	16, 0x90
.LBB9_34:                               # %for.body
                                        #   Parent Loop BB9_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rcx,%rax,8), %rdx
	cmpq	%r14, %rdx
	movq	$1, %rsi
	je	.LBB9_35
# BB#31:                                # %for.cond
                                        #   in Loop: Header=BB9_34 Depth=2
	decq	%rax
	testq	%rax, %rax
	jg	.LBB9_34
# BB#32:                                #   in Loop: Header=BB9_1 Depth=1
	xorq	%rsi, %rsi
	jmp	.LBB9_35
.LBB9_12:                               # %if.else21
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	%r15, %r14
	jne	.LBB9_17
# BB#13:                                # %if.then23
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$.L.str20, %rsi
	callq	Perl_sv_catpv
	movq	16(%r14), %rax
	andq	$112128000, %rax        # imm = 0x6AEF000
	cmpq	$109445120, %rax        # imm = 0x6860000
	jne	.LBB9_25
# BB#14:                                # %land.lhs.true31
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	PL_sv_no(%rip), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB9_25
# BB#15:                                # %land.lhs.true33
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
.LBB9_16:                               # %land.lhs.true33
                                        #   in Loop: Header=BB9_1 Depth=1
	callq	__eqdf2
	testq	%rax, %rax
	jne	.LBB9_25
	jmp	.LBB9_84
.LBB9_17:                               # %if.else38
                                        #   in Loop: Header=BB9_1 Depth=1
	cmpq	%rbx, %r14
	jne	.LBB9_23
# BB#18:                                # %if.then40
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$.L.str21, %rsi
	callq	Perl_sv_catpv
	movq	16(%r14), %rax
	andq	$112128000, %rax        # imm = 0x6AEF000
	cmpq	$109445120, %rax        # imm = 0x6860000
	jne	.LBB9_25
# BB#19:                                # %land.lhs.true49
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	PL_sv_yes(%rip), %rax
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	jne	.LBB9_25
# BB#20:                                # %land.lhs.true53
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB9_25
# BB#21:                                # %land.lhs.true56
                                        #   in Loop: Header=BB9_1 Depth=1
	movzbl	(%rcx), %ecx
	cmpq	$49, %rcx
	jne	.LBB9_25
# BB#22:                                # %land.lhs.true61
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	32(%rax), %rdi
	movabsq	$4607182418800017408, %rsi # imm = 0x3FF0000000000000
	jmp	.LBB9_16
.LBB9_30:                               #   in Loop: Header=BB9_1 Depth=1
	xorq	%rsi, %rsi
.LBB9_35:                               # %for.end
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	8(%r14), %rdx
	cmpq	$2, %rdx
	jb	.LBB9_39
# BB#36:                                # %if.then98
                                        #   in Loop: Header=BB9_1 Depth=1
	testq	%rsi, %rsi
	movabsq	$.L.str26, %rcx
	jne	.LBB9_38
# BB#37:                                # %if.then98
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$.L.str1, %rcx
.LBB9_38:                               # %if.then98
                                        #   in Loop: Header=BB9_1 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str25, %rsi
	callq	Perl_sv_catpvf
	jmp	.LBB9_42
.LBB9_23:                               # %if.else68
                                        #   in Loop: Header=BB9_1 Depth=1
	movabsq	$.L.str22, %rsi
.LBB9_24:                               # %if.else68
                                        #   in Loop: Header=BB9_1 Depth=1
	callq	Perl_sv_catpv
	movq	16(%r14), %rax
	andq	$128970752, %rax        # imm = 0x7AFF000
	cmpq	$8388608, %rax          # imm = 0x800000
	je	.LBB9_84
.LBB9_25:                               # %if.end80
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str23, %rsi
.LBB9_41:                               # %if.end110
                                        #   in Loop: Header=BB9_1 Depth=1
	callq	Perl_sv_catpv
.LBB9_42:                               # %if.end110
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	16(%r14), %rax
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB9_46
# BB#43:                                # %if.then114
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str28, %rsi
	callq	Perl_sv_catpv
	movq	(%r13), %rax
	movq	8(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdx, %rsi
	cmpq	$11, %rsi
	jb	.LBB9_45
	jmp	.LBB9_44
.LBB9_39:                               # %if.else101
                                        #   in Loop: Header=BB9_1 Depth=1
	testq	%rsi, %rsi
	je	.LBB9_42
# BB#40:                                # %if.then103
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r13, %rdi
	movabsq	$.L.str27, %rsi
	jmp	.LBB9_41
.LBB9_5:                                # %if.then2
	movabsq	$.L.str18, %rsi
.LBB9_83:                               # %finish
	movq	%r13, %rdi
	callq	Perl_sv_catpv
.LBB9_84:                               # %finish
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %r12
	je	.LBB9_87
# BB#85:
	movabsq	$.L.str55, %rbx
	.align	16, 0x90
.LBB9_86:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	decq	%r12
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpv
	testq	%r14, %r12
	jne	.LBB9_86
.LBB9_87:                               # %if.end222
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB9_89
# BB#88:                                # %cond.true
	movq	(%r13), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rax
	jmp	.LBB9_90
.LBB9_89:                               # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_2pv_flags
.LBB9_90:                               # %cond.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_46:                               # %if.end131
	andq	$255, %rax
	cmpq	$15, %rax
	jbe	.LBB9_47
# BB#48:                                # %sw.default
	movabsq	$.L.str29, %rsi
	jmp	.LBB9_83
.LBB9_44:                               # %if.then120
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	3(%rdx), %rcx
	movq	%rcx, 8(%rax)
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	movb	%cl, 3(%rax,%rdx)
	movabsq	$.L.str15, %rsi
	jmp	.LBB9_83
.LBB9_3:                                # %if.then
	movabsq	$.L.str17, %rsi
	jmp	.LBB9_83
.LBB9_47:                               # %if.end131
	jmpq	*.LJTI9_0(,%rax,8)
.LBB9_49:                               # %sw.bb
	movabsq	$.L.str30, %rsi
	jmp	.LBB9_83
.LBB9_50:                               # %sw.bb134
	movabsq	$.L.str31, %rsi
	jmp	.LBB9_67
.LBB9_51:                               # %sw.bb135
	movabsq	$.L.str32, %rsi
	jmp	.LBB9_67
.LBB9_52:                               # %sw.bb136
	movabsq	$.L.str33, %rsi
	jmp	.LBB9_67
.LBB9_53:                               # %sw.bb137
	movabsq	$.L.str34, %rsi
	jmp	.LBB9_67
.LBB9_54:                               # %sw.bb138
	movabsq	$.L.str35, %rsi
	jmp	.LBB9_67
.LBB9_55:                               # %sw.bb139
	movabsq	$.L.str36, %rsi
	jmp	.LBB9_67
.LBB9_56:                               # %sw.bb140
	movabsq	$.L.str37, %rsi
	jmp	.LBB9_67
.LBB9_64:                               # %sw.bb154
	movabsq	$.L.str44, %rsi
	jmp	.LBB9_67
.LBB9_57:                               # %sw.bb141
	movabsq	$.L.str38, %rsi
	jmp	.LBB9_67
.LBB9_58:                               # %sw.bb142
	movabsq	$.L.str39, %rsi
	jmp	.LBB9_67
.LBB9_59:                               # %sw.bb143
	movabsq	$.L.str40, %rsi
	jmp	.LBB9_67
.LBB9_60:                               # %sw.bb144
	movq	(%r14), %rax
	movq	96(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_62
# BB#61:                                # %if.then147
	movq	(%rax), %rax
	movq	64(%rax), %rdx
	movabsq	$.L.str41, %rsi
	jmp	.LBB9_80
.LBB9_63:                               # %sw.bb153
	movabsq	$.L.str43, %rsi
	jmp	.LBB9_67
.LBB9_65:                               # %sw.bb155
	movabsq	$.L.str45, %rsi
	jmp	.LBB9_67
.LBB9_66:                               # %sw.bb156
	movabsq	$.L.str46, %rsi
.LBB9_67:                               # %sw.epilog
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movq	16(%r14), %rax
	testq	$67108864, %rax         # imm = 0x4000000
	je	.LBB9_75
# BB#68:                                # %if.then160
	movq	(%r14), %rax
	cmpq	$0, (%rax)
	je	.LBB9_69
# BB#70:                                # %if.else165
	movabsq	$.L.str1, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %r15
	movabsq	$.L.str24, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	18(%r14), %eax
	testq	$32, %rax
	je	.LBB9_72
# BB#71:                                # %if.then170
	movq	(%r14), %rax
	movq	(%rax), %rsi
	movq	24(%rax), %rdx
	subq	%rdx, %rsi
	xorq	%rcx, %rcx
	movq	$127, %r8
	movq	%r15, %rdi
	callq	Perl_pv_display
	movq	%rax, %rcx
	movabsq	$.L.str48, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rdx
	callq	Perl_sv_catpvf
.LBB9_72:                               # %if.end178
	movq	(%r14), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	16(%rax), %rcx
	movq	$127, %r8
	movq	%r15, %rdi
	callq	Perl_pv_display
	movq	%rax, %rcx
	movabsq	$.L.str49, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rdx
	callq	Perl_sv_catpvf
	movzbl	19(%r14), %eax
	testq	$32, %rax
	je	.LBB9_74
# BB#73:                                # %if.then188
	movq	%r14, %rdi
	callq	Perl_sv_len_utf8
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	Perl_sv_uni_display
	movq	%rax, %rcx
	movabsq	$.L.str50, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rdx
	callq	Perl_sv_catpvf
.LBB9_74:                               # %if.end191
	movq	%r15, %rdi
	callq	Perl_sv_free
	jmp	.LBB9_84
.LBB9_75:                               # %if.else193
	testq	$33554432, %rax         # imm = 0x2000000
	je	.LBB9_77
# BB#76:                                # %if.then197
	movq	(%r14), %rax
	movq	32(%rax), %rdx
	movabsq	$.L.str51, %rsi
	jmp	.LBB9_80
.LBB9_69:                               # %if.then164
	movabsq	$.L.str47, %rsi
	jmp	.LBB9_83
.LBB9_77:                               # %if.else200
	testq	$16777216, %rax         # imm = 0x1000000
	je	.LBB9_82
# BB#78:                                # %if.then204
	movq	(%r14), %rcx
	movq	24(%rcx), %rdx
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rax
	je	.LBB9_81
# BB#79:                                # %if.then208
	movabsq	$.L.str52, %rsi
	jmp	.LBB9_80
.LBB9_82:                               # %if.else214
	movabsq	$.L.str54, %rsi
	jmp	.LBB9_83
.LBB9_81:                               # %if.else210
	movabsq	$.L.str53, %rsi
.LBB9_80:                               # %finish
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	Perl_sv_catpvf
	jmp	.LBB9_84
.LBB9_62:                               # %if.else151
	movabsq	$.L.str42, %rsi
	jmp	.LBB9_83
.Ltmp103:
	.size	Perl_sv_peek, .Ltmp103-Perl_sv_peek
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_49
	.quad	.LBB9_50
	.quad	.LBB9_51
	.quad	.LBB9_52
	.quad	.LBB9_53
	.quad	.LBB9_54
	.quad	.LBB9_55
	.quad	.LBB9_56
	.quad	.LBB9_64
	.quad	.LBB9_57
	.quad	.LBB9_58
	.quad	.LBB9_59
	.quad	.LBB9_60
	.quad	.LBB9_63
	.quad	.LBB9_65
	.quad	.LBB9_66

	.text
	.globl	Perl_do_pmop_dump
	.align	16, 0x90
	.type	Perl_do_pmop_dump,@function
Perl_do_pmop_dump:                      # @Perl_do_pmop_dump
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
	subq	$16, %rsp
.Ltmp115:
	.cfi_def_cfa_offset 64
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
	movq	%rdx, %rbx
	movq	%rsi, %r12
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB10_1
# BB#3:                                 # %if.end
	movabsq	$.L.str57, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	Perl_dump_indent
	movq	80(%rbx), %rax
	leaq	1(%r14), %r13
	testq	%rax, %rax
	je	.LBB10_8
# BB#4:                                 # %if.then5
	movl	88(%rbx), %ecx
	andq	$2, %rcx
	movzbl	37(%rbx), %edx
	leaq	47(,%rcx,8), %r9
	movq	32(%rax), %r8
	andq	$64, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB10_5
# BB#6:                                 # %if.then5
	movabsq	$.L.str1, %rcx
	jmp	.LBB10_7
.LBB10_1:                               # %if.then
	movabsq	$.L.str56, %rdx
	jmp	.LBB10_2
.LBB10_8:                               # %if.else11
	movabsq	$.L.str60, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_dump_indent
	jmp	.LBB10_9
.LBB10_5:
	movabsq	$.L.str59, %rcx
.LBB10_7:                               # %if.then5
	movabsq	$.L.str58, %rdx
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r9, %rcx
	callq	Perl_dump_indent
.LBB10_9:                               # %if.end12
	movzbl	32(%rbx), %eax
	cmpq	$13, %rax
	je	.LBB10_12
# BB#10:                                # %land.lhs.true
	cmpq	$0, 56(%rbx)
	je	.LBB10_12
# BB#11:                                # %if.then16
	movabsq	$.L.str61, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_dump_indent
	movq	56(%rbx), %r15
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	Perl_do_op_dump
.LBB10_12:                              # %if.end18
	movq	%r13, 8(%rsp)           # 8-byte Spill
	cmpq	$0, 88(%rbx)
	jne	.LBB10_15
# BB#13:                                # %lor.lhs.false
	movq	80(%rbx), %rax
	testq	%rax, %rax
	je	.LBB10_46
# BB#14:                                # %land.lhs.true23
	movq	24(%rax), %rax
	cmpq	$0, 80(%rax)
	je	.LBB10_46
.LBB10_15:                              # %if.then26
	movabsq	$.L.str1, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	Perl_newSVpvn
	movzbl	104(%rbx), %ecx
	movq	%rax, %r15
	testq	$1, %rcx
	je	.LBB10_17
# BB#16:                                # %if.then30
	movabsq	$.L.str62, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movzbl	104(%rbx), %ecx
.LBB10_17:                              # %if.end31
	testq	$2, %rcx
	je	.LBB10_19
# BB#18:                                # %if.then36
	movabsq	$.L.str63, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
.LBB10_19:                              # %if.end37
	movzbl	88(%rbx), %eax
	testq	$2, %rax
	je	.LBB10_21
# BB#20:                                # %if.then41
	movabsq	$.L.str64, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
.LBB10_21:                              # %if.end42
	movq	80(%rbx), %rax
	testq	%rax, %rax
	je	.LBB10_29
# BB#22:                                # %land.lhs.true45
	movq	24(%rax), %rcx
	cmpq	$0, 80(%rcx)
	je	.LBB10_26
# BB#23:                                # %land.lhs.true52
	movzbl	120(%rax), %ecx
	testq	$64, %rcx
	jne	.LBB10_25
# BB#24:                                # %if.then56
	movabsq	$.L.str65, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	80(%rbx), %rax
.LBB10_25:                              # %if.end57
	testq	%rax, %rax
	je	.LBB10_29
.LBB10_26:                              # %land.lhs.true60
	movq	24(%rax), %rcx
	cmpq	$0, 80(%rcx)
	je	.LBB10_29
# BB#27:                                # %land.lhs.true67
	movzbl	121(%rax), %eax
	testq	$1, %rax
	je	.LBB10_29
# BB#28:                                # %if.then72
	movabsq	$.L.str66, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
.LBB10_29:                              # %if.end73
	movq	88(%rbx), %rax
	testq	$16, %rax
	je	.LBB10_31
# BB#30:                                # %if.then77
	movabsq	$.L.str67, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_31:                              # %if.end78
	testq	$64, %rax
	je	.LBB10_33
# BB#32:                                # %if.then82
	movabsq	$.L.str68, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_33:                              # %if.end83
	testq	$128, %rax
	je	.LBB10_35
# BB#34:                                # %if.then87
	movabsq	$.L.str69, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_35:                              # %if.end88
	testq	$256, %rax              # imm = 0x100
	je	.LBB10_37
# BB#36:                                # %if.then92
	movabsq	$.L.str70, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_37:                              # %if.end93
	testq	$512, %rax              # imm = 0x200
	je	.LBB10_39
# BB#38:                                # %if.then97
	movabsq	$.L.str71, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_39:                              # %if.end98
	testq	$1, %rax
	je	.LBB10_41
# BB#40:                                # %if.then102
	movabsq	$.L.str72, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
	movq	88(%rbx), %rax
.LBB10_41:                              # %if.end103
	testq	$1024, %rax             # imm = 0x400
	je	.LBB10_43
# BB#42:                                # %if.then107
	movabsq	$.L.str73, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_catpv
.LBB10_43:                              # %if.end108
	movq	(%r15), %rax
	cmpq	$0, 8(%rax)
	je	.LBB10_45
# BB#44:                                # %cond.true
	movq	(%rax), %r13
	incq	%r13
.LBB10_45:                              # %cond.end
	movabsq	$.L.str74, %rdx
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r12, %rsi
	movq	%r13, %rcx
	callq	Perl_dump_indent
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB10_46:                              # %if.end112
	movabsq	$.L.str75, %rdx
.LBB10_2:                               # %if.then
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_dump_indent  # TAILCALL
.Ltmp121:
	.size	Perl_do_pmop_dump, .Ltmp121-Perl_do_pmop_dump
	.cfi_endproc

	.globl	Perl_pmop_dump
	.align	16, 0x90
	.type	Perl_pmop_dump,@function
Perl_pmop_dump:                         # @Perl_pmop_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 16
.Ltmp125:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	jmp	Perl_do_pmop_dump  # TAILCALL
.Ltmp126:
	.size	Perl_pmop_dump, .Ltmp126-Perl_pmop_dump
	.cfi_endproc

	.globl	Perl_do_op_dump
	.align	16, 0x90
	.type	Perl_do_op_dump,@function
Perl_do_op_dump:                        # @Perl_do_op_dump
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
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str57, %rdx
	xorq	%rax, %rax
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	Perl_dump_indent
	movzbl	34(%r12), %edx
	leaq	1(%rbx), %r15
	testq	%rdx, %rdx
	je	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str76, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_3
.LBB12_2:                               # %if.else
	movabsq	$.L.str77, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
.LBB12_3:                               # %if.end
	movq	PL_dumpindent(%rip), %rcx
	movzbl	32(%r12), %eax
	imulq	%r15, %rcx
	movabsq	$4294967292, %rbx       # imm = 0xFFFFFFFC
	addq	%rcx, %rbx
	cmpq	$351, %rax              # imm = 0x15F
	jne	.LBB12_5
# BB#4:                                 # %cond.true
	movq	%r12, %rdi
	callq	Perl_custom_op_name
	movq	%rax, %r8
	jmp	.LBB12_6
.LBB12_5:                               # %cond.false
	movq	PL_op_name(,%rax,8), %r8
.LBB12_6:                               # %cond.end
	movabsq	$.L.str78, %rsi
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	PerlIO_printf
	movq	(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_11
# BB#7:                                 # %if.then10
	movzbl	34(%r12), %ecx
	movzbl	34(%rax), %edx
	testq	%rcx, %rcx
	je	.LBB12_10
# BB#8:                                 # %if.then13
	movabsq	$.L.str79, %rsi
	jmp	.LBB12_9
.LBB12_11:                              # %if.else24
	movabsq	$.L.str81, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_12
.LBB12_10:                              # %if.else18
	movabsq	$.L.str80, %rsi
.LBB12_9:                               # %if.end26
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
.LBB12_12:                              # %if.end26
	movq	24(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB12_24
# BB#13:                                # %if.then28
	movzbl	32(%r12), %eax
	testq	%rax, %rax
	je	.LBB12_14
# BB#22:                                # %if.else65
	movabsq	$.L.str86, %rdx
.LBB12_23:                              # %if.end68
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	jmp	.LBB12_24
.LBB12_14:                              # %if.then33
	movq	PL_op_name(,%rcx,8), %rcx
	movabsq	$.L.str82, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	movq	24(%r12), %rax
	cmpq	$174, %rax
	jne	.LBB12_24
# BB#15:                                # %if.then39
	movq	80(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB12_17
# BB#16:                                # %if.then41
	movabsq	$.L.str83, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_17:                              # %if.end43
	movq	48(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_20
# BB#18:                                # %cond.true45
	movq	(%rax), %rax
	movq	80(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB12_20
# BB#19:                                # %if.then49
	movabsq	$.L.str84, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_20:                              # %if.end59
	movq	40(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB12_24
# BB#21:                                # %if.then61
	movabsq	$.L.str85, %rdx
	jmp	.LBB12_23
.LBB12_24:                              # %if.end68
	movzbl	36(%r12), %eax
	testq	%rax, %rax
	je	.LBB12_47
# BB#25:                                # %if.then70
	movabsq	$.L.str1, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	Perl_newSVpvn
	movzbl	36(%r12), %ecx
	movq	%rax, %rbx
	andq	$3, %rcx
	cmpq	$3, %rcx
	je	.LBB12_30
# BB#26:                                # %if.then70
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB12_27
# BB#29:                                # %sw.bb74
	movabsq	$.L.str88, %rsi
	jmp	.LBB12_32
.LBB12_30:                              # %sw.bb75
	movabsq	$.L.str89, %rsi
	jmp	.LBB12_32
.LBB12_27:                              # %if.then70
	cmpq	$1, %rcx
	jne	.LBB12_31
# BB#28:                                # %sw.bb
	movabsq	$.L.str87, %rsi
	jmp	.LBB12_32
.LBB12_31:                              # %sw.default
	movabsq	$.L.str90, %rsi
.LBB12_32:                              # %sw.epilog
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
	testq	$4, %rax
	je	.LBB12_34
# BB#33:                                # %if.then80
	movabsq	$.L.str91, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
.LBB12_34:                              # %if.end81
	testq	$8, %rax
	je	.LBB12_36
# BB#35:                                # %if.then86
	movabsq	$.L.str92, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
.LBB12_36:                              # %if.end87
	testq	$64, %rax
	je	.LBB12_38
# BB#37:                                # %if.then92
	movabsq	$.L.str93, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
.LBB12_38:                              # %if.end93
	testq	$16, %rax
	je	.LBB12_40
# BB#39:                                # %if.then98
	movabsq	$.L.str94, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
.LBB12_40:                              # %if.end99
	testq	$32, %rax
	je	.LBB12_42
# BB#41:                                # %if.then104
	movabsq	$.L.str95, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movzbl	36(%r12), %eax
.LBB12_42:                              # %if.end105
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB12_44
# BB#43:                                # %if.then110
	movabsq	$.L.str96, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
.LBB12_44:                              # %if.end111
	movq	(%rbx), %rax
	cmpq	$0, 8(%rax)
	je	.LBB12_46
# BB#45:                                # %cond.true114
	movq	(%rax), %r13
	incq	%r13
.LBB12_46:                              # %cond.end117
	movabsq	$.L.str97, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r13, %rcx
	callq	Perl_dump_indent
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB12_47:                              # %if.end119
	movzbl	37(%r12), %eax
	testq	%rax, %rax
	je	.LBB12_195
# BB#48:                                # %if.then121
	movabsq	$.L.str1, %rdi
	xorq	%rsi, %rsi
	callq	Perl_newSVpvn
	movzbl	32(%r12), %edx
	movzbl	PL_opargs+1(,%rdx,8), %ecx
	movq	%rax, %r13
	testq	$1, %rcx
	je	.LBB12_51
# BB#49:                                # %if.then129
	movzbl	37(%r12), %eax
	testq	$16, %rax
	je	.LBB12_189
# BB#50:                                # %if.then134
	movabsq	$.L.str98, %rsi
.LBB12_187:                             # %if.end645
	movq	%r13, %rdi
.LBB12_188:                             # %if.end645
	callq	Perl_sv_catpv
.LBB12_189:                             # %if.end645
	movzbl	36(%r12), %eax
	testq	$32, %rax
	je	.LBB12_192
# BB#190:                               # %land.lhs.true
	movsbq	37(%r12), %rax
	testq	%rax, %rax
	jns	.LBB12_192
# BB#191:                               # %if.then654
	movabsq	$.L.str128, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
.LBB12_192:                             # %if.end655
	movq	(%r13), %rax
	cmpq	$0, 8(%rax)
	je	.LBB12_194
# BB#193:                               # %if.then659
	movq	(%rax), %rcx
	incq	%rcx
	movabsq	$.L.str144, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_194:                             # %if.end663
	movq	%r13, %rdi
	callq	Perl_sv_free
.LBB12_195:                             # %if.end664
	movzbl	32(%r12), %eax
	cmpq	$348, %rax              # imm = 0x15C
	jg	.LBB12_206
# BB#196:                               # %if.end664
	cmpq	$311, %rax              # imm = 0x137
	jg	.LBB12_205
# BB#197:                               # %if.end664
	cmpq	$206, %rax
	jg	.LBB12_227
# BB#198:                               # %if.end664
	cmpq	$152, %rax
	jg	.LBB12_203
# BB#199:                               # %if.end664
	leaq	-31(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jb	.LBB12_202
# BB#200:                               # %if.end664
	cmpq	$5, %rax
	je	.LBB12_208
# BB#201:                               # %if.end664
	cmpq	$13, %rax
	jne	.LBB12_231
.LBB12_202:                             # %sw.bb764
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	Perl_do_pmop_dump
	jmp	.LBB12_231
.LBB12_206:                             # %if.end664
	cmpq	$349, %rax              # imm = 0x15D
	je	.LBB12_209
# BB#207:                               # %if.end664
	cmpq	$350, %rax              # imm = 0x15E
	jne	.LBB12_231
.LBB12_208:                             # %sw.bb693
	movq	40(%r12), %rdi
	callq	Perl_sv_peek
	movq	%rax, %rcx
	movabsq	$.L.str147, %rdx
	jmp	.LBB12_230
.LBB12_205:                             # %if.end664
	cmpq	$312, %rax              # imm = 0x138
	je	.LBB12_228
	jmp	.LBB12_231
.LBB12_51:                              # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$347, %rax              # imm = 0x15B
	jg	.LBB12_177
# BB#52:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$235, %rax
	jg	.LBB12_184
# BB#53:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$206, %rax
	jg	.LBB12_100
# BB#54:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$189, %rax
	jg	.LBB12_90
# BB#55:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$177, %rax
	jg	.LBB12_101
# BB#56:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$13, %rax
	jg	.LBB12_62
# BB#57:                                # %if.else136
	cmpq	$5, %rdx
	je	.LBB12_150
# BB#58:                                # %if.else136
	cmpq	$6, %rdx
	je	.LBB12_119
# BB#59:                                # %if.else136
	cmpq	$7, %rdx
	jne	.LBB12_189
# BB#60:                                # %if.then444
	movzbl	37(%r12), %eax
	testq	$32, %rax
	je	.LBB12_189
# BB#61:                                # %if.then449
	movabsq	$.L.str129, %rsi
	jmp	.LBB12_187
.LBB12_227:                             # %if.end664
	cmpq	$207, %rax
	jne	.LBB12_231
	jmp	.LBB12_228
.LBB12_177:                             # %if.else136
	cmpq	$348, %rdx              # imm = 0x15C
	jne	.LBB12_189
# BB#178:                               # %if.then516
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#179:                               # %if.then521
	movabsq	$.L.str136, %rsi
	jmp	.LBB12_187
.LBB12_203:                             # %if.end664
	addq	$-153, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$29, %rcx
	ja	.LBB12_231
# BB#204:                               # %if.end664
	jmpq	*.LJTI12_1(,%rcx,8)
.LBB12_225:                             # %sw.bb754
	movabsq	$.L.str151, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	movq	48(%r12), %rax
.LBB12_223:                             # %if.end744
	testq	%rax, %rax
	je	.LBB12_226
# BB#224:                               # %if.then746
	movzbl	34(%rax), %edx
	movabsq	$.L.str79, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_231
.LBB12_209:                             # %sw.bb696
	movq	80(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB12_211
# BB#210:                               # %if.then699
	movabsq	$.L.str83, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_211:                             # %if.end701
	movq	48(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_214
# BB#212:                               # %cond.true704
	movq	(%rax), %rax
	movq	80(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB12_214
# BB#213:                               # %if.then710
	movabsq	$.L.str84, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_214:                             # %if.end720
	movq	40(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB12_231
# BB#215:                               # %if.then723
	movabsq	$.L.str85, %rdx
	jmp	.LBB12_230
.LBB12_228:                             # %sw.bb765
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_231
# BB#229:                               # %if.then770
	movq	24(%r12), %rcx
	movabsq	$.L.str152, %rdx
.LBB12_230:                             # %sw.epilog774
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
.LBB12_231:                             # %sw.epilog774
	movzbl	36(%r12), %eax
	testq	$4, %rax
	je	.LBB12_235
# BB#232:                               # %if.then779
	movq	40(%r12), %rbx
	jmp	.LBB12_234
	.align	16, 0x90
.LBB12_233:                             # %for.body
                                        #   in Loop: Header=BB12_234 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_do_op_dump
	movq	8(%rbx), %rbx
.LBB12_234:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB12_233
.LBB12_235:                             # %if.end781
	movabsq	$.L.str75, %rdx
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r14, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	Perl_dump_indent  # TAILCALL
.LBB12_184:                             # %if.else136
	addq	$-236, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$6, %rdx
	jae	.LBB12_189
# BB#185:                               # %if.then618
	movzbl	37(%r12), %eax
	testq	$2, %rax
	je	.LBB12_189
# BB#186:                               # %if.then623
	movabsq	$.L.str143, %rsi
	jmp	.LBB12_187
.LBB12_100:                             # %if.else136
	cmpq	$207, %rdx
	je	.LBB12_102
	jmp	.LBB12_189
.LBB12_90:                              # %if.else136
	cmpq	$190, %rdx
	jne	.LBB12_91
# BB#180:                               # %if.then563
	movsbq	37(%r12), %rax
	testq	%rax, %rax
	jns	.LBB12_182
# BB#181:                               # %if.then568
	movabsq	$.L.str141, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_182:                             # %if.end569
	testq	$64, %rax
	je	.LBB12_189
# BB#183:                               # %if.then574
	movabsq	$.L.str142, %rsi
	jmp	.LBB12_187
.LBB12_101:                             # %if.else136
	cmpq	$178, %rdx
	jne	.LBB12_189
	jmp	.LBB12_102
.LBB12_91:                              # %if.else136
	cmpq	$191, %rdx
	jne	.LBB12_189
	jmp	.LBB12_92
.LBB12_226:                             # %if.else751
	movabsq	$.L.str81, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_231
.LBB12_62:                              # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	movq	$48, %rcx
	cmpq	$124, %rax
	jg	.LBB12_76
# BB#63:                                # %if.else136
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$60, %rax
	jg	.LBB12_116
# BB#64:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$34, %rax
	jg	.LBB12_68
# BB#65:                                # %if.else136
	leaq	-14(%rdx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$2, %rax
	jb	.LBB12_119
# BB#66:                                # %if.else136
	cmpq	$17, %rdx
	jne	.LBB12_67
# BB#162:                               # %if.then432
	movsbq	37(%r12), %rax
	testq	%rax, %rax
	jns	.LBB12_189
# BB#163:                               # %if.then437
	movabsq	$.L.str128, %rax
	jmp	.LBB12_75
.LBB12_216:                             # %sw.bb726
	movabsq	$.L.str148, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	movq	56(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_218
# BB#217:                               # %if.then728
	movzbl	34(%rax), %edx
	movabsq	$.L.str79, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_219
.LBB12_76:                              # %if.else136
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$165, %rax
	jg	.LBB12_85
# BB#77:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$156, %rax
	jg	.LBB12_81
# BB#78:                                # %if.else136
	movq	$48, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$48, %rcx
	sarq	%cl, %rax
	cmpq	$149, %rax
	jg	.LBB12_170
# BB#79:                                # %if.else136
	leaq	-125(%rdx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$16, %rax
	ja	.LBB12_189
# BB#80:                                # %if.else136
	jmpq	*.LJTI12_0(,%rax,8)
.LBB12_166:                             # %if.then468
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#167:                               # %if.then473
	movabsq	$.L.str131, %rax
	jmp	.LBB12_75
.LBB12_150:                             # %if.then372
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_152
# BB#151:                               # %if.then377
	movabsq	$.L.str122, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_152:                             # %if.end378
	testq	$8, %rax
	je	.LBB12_154
# BB#153:                               # %if.then383
	movabsq	$.L.str123, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_154:                             # %if.end384
	testq	$32, %rax
	je	.LBB12_156
# BB#155:                               # %if.then389
	movabsq	$.L.str124, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_156:                             # %if.end390
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB12_158
# BB#157:                               # %if.then395
	movabsq	$.L.str125, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_158:                             # %if.end396
	testq	$16, %rax
	je	.LBB12_189
# BB#159:                               # %if.then401
	movabsq	$.L.str126, %rsi
	jmp	.LBB12_187
.LBB12_116:                             # %if.else136
	cmpq	$61, %rdx
	jne	.LBB12_189
# BB#117:                               # %if.then233
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#118:                               # %if.then238
	movabsq	$.L.str108, %rsi
	jmp	.LBB12_187
.LBB12_85:                              # %if.else136
	leaq	-167(%rdx), %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$2, %rax
	jae	.LBB12_86
.LBB12_102:                             # %if.then155
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#103:                               # %if.then160
	movabsq	$.L.str99, %rsi
	jmp	.LBB12_187
.LBB12_218:                             # %if.else733
	movabsq	$.L.str81, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
.LBB12_219:                             # %if.end735
	movabsq	$.L.str149, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	movq	64(%r12), %rax
	testq	%rax, %rax
	je	.LBB12_221
# BB#220:                               # %if.then737
	movzbl	34(%rax), %edx
	movabsq	$.L.str79, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	jmp	.LBB12_222
.LBB12_221:                             # %if.else742
	movabsq	$.L.str81, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
.LBB12_222:                             # %if.end744
	movabsq	$.L.str150, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_dump_indent
	movq	72(%r12), %rax
	jmp	.LBB12_223
.LBB12_68:                              # %if.else136
	cmpq	$35, %rdx
	je	.LBB12_106
# BB#69:                                # %if.else136
	cmpq	$36, %rdx
	jne	.LBB12_70
# BB#104:                               # %if.then185
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#105:                               # %if.then190
	movabsq	$.L.str102, %rsi
	jmp	.LBB12_187
.LBB12_81:                              # %if.else136
	cmpq	$157, %rdx
	jne	.LBB12_82
# BB#160:                               # %if.then408
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#161:                               # %if.then413
	movabsq	$.L.str127, %rsi
	jmp	.LBB12_187
.LBB12_86:                              # %if.else136
	cmpq	$166, %rdx
	jne	.LBB12_87
.LBB12_119:                             # %if.then280
	movzbl	37(%r12), %eax
	cmpq	$166, %rdx
	jne	.LBB12_132
# BB#120:                               # %if.then285
	testq	$8, %rax
	je	.LBB12_122
# BB#121:                               # %if.then290
	movabsq	$.L.str109, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_122:                             # %if.end291
	testq	$16, %rax
	je	.LBB12_124
# BB#123:                               # %if.then296
	movabsq	$.L.str110, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_124:                             # %if.end297
	testq	$32, %rax
	je	.LBB12_126
# BB#125:                               # %if.then302
	movabsq	$.L.str111, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_126:                             # %if.end303
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB12_128
# BB#127:                               # %if.then308
	movabsq	$.L.str112, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_128:                             # %if.end309
	testq	$4, %rax
	je	.LBB12_130
# BB#129:                               # %if.then314
	movabsq	$.L.str113, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_130:                             # %if.end315
	testq	$64, %rax
	je	.LBB12_142
# BB#131:                               # %if.then320
	movabsq	$.L.str114, %rsi
	jmp	.LBB12_141
.LBB12_132:                             # %if.else322
	andq	$96, %rax
	cmpq	$32, %rax
	je	.LBB12_136
# BB#133:                               # %if.else322
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jne	.LBB12_134
# BB#137:                               # %sw.bb328
	movabsq	$.L.str117, %rsi
	jmp	.LBB12_138
.LBB12_136:                             # %sw.bb327
	movabsq	$.L.str116, %rsi
	jmp	.LBB12_138
.LBB12_134:                             # %if.else322
	cmpq	$96, %rax
	jne	.LBB12_139
# BB#135:                               # %sw.bb326
	movabsq	$.L.str115, %rsi
.LBB12_138:                             # %sw.epilog329
	movq	%r13, %rdi
	callq	Perl_sv_catpv
.LBB12_139:                             # %sw.epilog329
	movzbl	37(%r12), %eax
	testq	$8, %rax
	je	.LBB12_142
# BB#140:                               # %if.then334
	movabsq	$.L.str118, %rsi
.LBB12_141:                             # %if.end336
	movq	%r13, %rdi
	callq	Perl_sv_catpv
.LBB12_142:                             # %if.end336
	movzbl	32(%r12), %eax
	cmpq	$127, %rax
	je	.LBB12_144
# BB#143:                               # %if.end336
	cmpq	$135, %rax
	jne	.LBB12_146
.LBB12_144:                             # %if.then346
	movzbl	37(%r12), %eax
	testq	$16, %rax
	je	.LBB12_189
# BB#145:                               # %if.then351
	movabsq	$.L.str119, %rsi
	jmp	.LBB12_187
.LBB12_146:                             # %if.else353
	movzbl	37(%r12), %eax
	testq	$2, %rax
	je	.LBB12_148
# BB#147:                               # %if.then358
	movabsq	$.L.str120, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_148:                             # %if.end359
	testq	$16, %rax
	je	.LBB12_189
# BB#149:                               # %if.then364
	movabsq	$.L.str121, %rsi
	jmp	.LBB12_187
.LBB12_170:                             # %if.else136
	cmpq	$150, %rdx
	jne	.LBB12_189
# BB#171:                               # %if.then492
	movzbl	37(%r12), %eax
	testq	$1, %rax
	je	.LBB12_173
# BB#172:                               # %if.then497
	movabsq	$.L.str133, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_173:                             # %if.end498
	testq	$2, %rax
	je	.LBB12_175
# BB#174:                               # %if.then503
	movabsq	$.L.str134, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_175:                             # %if.end504
	testq	$4, %rax
	je	.LBB12_189
# BB#176:                               # %if.then509
	movabsq	$.L.str135, %rax
	jmp	.LBB12_75
.LBB12_82:                              # %if.else136
	cmpq	$158, %rdx
	jne	.LBB12_189
# BB#83:                                # %if.then420
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#84:                                # %if.then425
	movabsq	$.L.str127, %rax
	jmp	.LBB12_75
.LBB12_87:                              # %if.else136
	cmpq	$171, %rdx
	jne	.LBB12_189
# BB#88:                                # %if.then581
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#89:                                # %if.then586
	movabsq	$.L.str142, %rax
	jmp	.LBB12_75
.LBB12_106:                             # %if.then197
	movzbl	37(%r12), %eax
	testq	$8, %rax
	je	.LBB12_108
# BB#107:                               # %if.then202
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_108:                             # %if.end203
	testq	$16, %rax
	je	.LBB12_110
# BB#109:                               # %if.then208
	movabsq	$.L.str104, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_110:                             # %if.end209
	testq	$32, %rax
	je	.LBB12_112
# BB#111:                               # %if.then214
	movabsq	$.L.str105, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_112:                             # %if.end215
	testq	$4, %rax
	je	.LBB12_114
# BB#113:                               # %if.then220
	movabsq	$.L.str106, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_114:                             # %if.end221
	testq	$64, %rax
	je	.LBB12_189
# BB#115:                               # %if.then226
	movabsq	$.L.str107, %rax
	jmp	.LBB12_75
.LBB12_70:                              # %if.else136
	cmpq	$37, %rdx
	jne	.LBB12_189
# BB#71:                                # %if.then167
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_73
# BB#72:                                # %if.then172
	movabsq	$.L.str100, %rax
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_73:                              # %if.end173
	testq	$32, %rax
	je	.LBB12_189
# BB#74:                                # %if.then178
	movabsq	$.L.str101, %rax
	jmp	.LBB12_75
.LBB12_67:                              # %if.else136
	cmpq	$24, %rdx
	jne	.LBB12_189
.LBB12_92:                              # %if.then533
	movzbl	37(%r12), %eax
	testq	$16, %rax
	je	.LBB12_94
# BB#93:                                # %if.then538
	movabsq	$.L.str137, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_94:                              # %if.end539
	testq	$32, %rax
	je	.LBB12_96
# BB#95:                                # %if.then544
	movabsq	$.L.str138, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_96:                              # %if.end545
	testq	$64, %rax
	je	.LBB12_98
# BB#97:                                # %if.then550
	movabsq	$.L.str139, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movzbl	37(%r12), %eax
.LBB12_98:                              # %if.end551
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB12_189
# BB#99:                                # %if.then556
	movabsq	$.L.str140, %rsi
	jmp	.LBB12_187
.LBB12_168:                             # %if.then480
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#169:                               # %if.then485
	movabsq	$.L.str132, %rax
	jmp	.LBB12_75
.LBB12_164:                             # %if.then456
	movzbl	37(%r12), %eax
	testq	$64, %rax
	je	.LBB12_189
# BB#165:                               # %if.then461
	movabsq	$.L.str130, %rax
.LBB12_75:                              # %if.end645
	movq	%r13, %rdi
	movq	%rax, %rsi
	jmp	.LBB12_188
.Ltmp144:
	.size	Perl_do_op_dump, .Ltmp144-Perl_do_op_dump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI12_0:
	.quad	.LBB12_119
	.quad	.LBB12_189
	.quad	.LBB12_119
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_166
	.quad	.LBB12_168
	.quad	.LBB12_119
	.quad	.LBB12_119
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_189
	.quad	.LBB12_164
.LJTI12_1:
	.quad	.LBB12_225
	.quad	.LBB12_231
	.quad	.LBB12_225
	.quad	.LBB12_225
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_225
	.quad	.LBB12_225
	.quad	.LBB12_231
	.quad	.LBB12_225
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_228
	.quad	.LBB12_228
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_209
	.quad	.LBB12_209
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_228
	.quad	.LBB12_228
	.quad	.LBB12_231
	.quad	.LBB12_231
	.quad	.LBB12_216

	.text
	.globl	Perl_gv_dump
	.align	16, 0x90
	.type	Perl_gv_dump,@function
Perl_gv_dump:                           # @Perl_gv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp148:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp149:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp150:
	.cfi_def_cfa_offset 32
.Ltmp151:
	.cfi_offset %rbx, -24
.Ltmp152:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB13_4
# BB#1:                                 # %if.end
	callq	Perl_sv_newmortal
	movq	%rax, %r14
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_gv_fullname4
	callq	Perl_PerlIO_stderr
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	%rax, %rsi
	movabsq	$.L.str153, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	Perl_dump_indent
	movq	(%rbx), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rsi
	cmpq	%rbx, %rsi
	je	.LBB13_3
# BB#2:                                 # %if.then7
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	Perl_gv_efullname4
	callq	Perl_PerlIO_stderr
	movq	(%r14), %rcx
	movq	(%rcx), %rcx
	movq	%rax, %rsi
	movabsq	$.L.str154, %rdx
	movq	$1, %rdi
	xorq	%rax, %rax
	callq	Perl_dump_indent
.LBB13_3:                               # %if.end14
	callq	Perl_PerlIO_stderr
	movq	$10, %rsi
	movq	%rax, %rdi
	callq	PerlIO_putc
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str75, %rdx
	xorq	%rdi, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	Perl_dump_indent  # TAILCALL
.LBB13_4:                               # %if.then
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str56, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_printf  # TAILCALL
.Ltmp153:
	.size	Perl_gv_dump, .Ltmp153-Perl_gv_dump
	.cfi_endproc

	.globl	Perl_do_magic_dump
	.align	16, 0x90
	.type	Perl_do_magic_dump,@function
Perl_do_magic_dump:                     # @Perl_do_magic_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp160:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp161:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp162:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp163:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp165:
	.cfi_def_cfa_offset 96
.Ltmp166:
	.cfi_offset %rbx, -48
.Ltmp167:
	.cfi_offset %r12, -40
.Ltmp168:
	.cfi_offset %r13, -32
.Ltmp169:
	.cfi_offset %r14, -24
.Ltmp170:
	.cfi_offset %r15, -16
	movq	%r9, 32(%rsp)           # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%rdi, %r15
	testq	%r12, %r12
	je	.LBB14_67
# BB#1:                                 # %for.body.lr.ph
	leaq	2(%r15), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$.L.str198, %r14
	incq	40(%rsp)                # 8-byte Folded Spill
	jmp	.LBB14_2
.LBB14_29:                              # %if.else101
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movabsq	$.L.str182, %rcx
	movq	%rcx, %rdx
	movq	%r8, %rcx
                                        # kill: RBX<def> RDI<kill>
	callq	Perl_dump_indent
	jmp	.LBB14_31
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_35 Depth 2
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	movabsq	$.L.str155, %rdx
	movq	%r12, %rcx
	callq	Perl_dump_indent
	movq	%r15, %rbx
	movq	8(%r12), %r8
	testq	%r8, %r8
	jne	.LBB14_3
# BB#30:                                # %if.else103
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movabsq	$.L.str183, %rdx
	callq	Perl_dump_indent
	jmp	.LBB14_31
	.align	16, 0x90
.LBB14_3:                               # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_sv, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str156, %rcx
	je	.LBB14_28
# BB#4:                                 # %if.else
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_env, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str157, %rcx
	je	.LBB14_28
# BB#5:                                 # %if.else6
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_envelem, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str158, %rcx
	je	.LBB14_28
# BB#6:                                 # %if.else9
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_sig, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str159, %rcx
	je	.LBB14_28
# BB#7:                                 # %if.else12
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_sigelem, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str160, %rcx
	je	.LBB14_28
# BB#8:                                 # %if.else15
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_pack, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str161, %rcx
	je	.LBB14_28
# BB#9:                                 # %if.else18
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_packelem, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str162, %rcx
	je	.LBB14_28
# BB#10:                                # %if.else21
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_dbline, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str163, %rcx
	je	.LBB14_28
# BB#11:                                # %if.else24
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_isa, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str164, %rcx
	je	.LBB14_28
# BB#12:                                # %if.else27
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_arylen, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str165, %rcx
	je	.LBB14_28
# BB#13:                                # %if.else30
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_glob, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str166, %rcx
	je	.LBB14_28
# BB#14:                                # %if.else33
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_mglob, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str167, %rcx
	je	.LBB14_28
# BB#15:                                # %if.else36
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_nkeys, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str168, %rcx
	je	.LBB14_28
# BB#16:                                # %if.else39
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_taint, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str169, %rcx
	je	.LBB14_28
# BB#17:                                # %if.else42
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_substr, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str170, %rcx
	je	.LBB14_28
# BB#18:                                # %if.else45
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_vec, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str171, %rcx
	je	.LBB14_28
# BB#19:                                # %if.else48
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_pos, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str172, %rcx
	je	.LBB14_28
# BB#20:                                # %if.else51
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_bm, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str173, %rcx
	je	.LBB14_28
# BB#21:                                # %if.else54
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_fm, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str174, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#22:                                # %if.else57
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_uvar, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str175, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#23:                                # %if.else60
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_defelem, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str176, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#24:                                # %if.else63
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_amagic, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str177, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#25:                                # %if.else66
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_amagicelem, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str178, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#26:                                # %if.else69
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_backref, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str179, %rax
	movq	%rax, %rcx
	je	.LBB14_28
# BB#27:                                # %if.else72
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$PL_vtbl_utf8, %rax
	cmpq	%rax, %r8
	movabsq	$.L.str180, %rax
	movq	%rax, %rcx
	jne	.LBB14_29
	.align	16, 0x90
.LBB14_28:                              # %if.then100
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movabsq	$.L.str181, %rdx
	callq	Perl_dump_indent
.LBB14_31:                              # %if.end104
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	16(%r12), %ecx
	testq	%rcx, %rcx
	je	.LBB14_33
# BB#32:                                # %if.then106
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movabsq	$.L.str184, %rdx
	callq	Perl_dump_indent
.LBB14_33:                              # %for.cond109.preheader
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r13, %r15
	movq	%rbx, %r13
	movzbl	18(%r12), %r8d
	xorq	%rax, %rax
	movabsq	$magic_names, %rdx
	movabsq	$.L.str378, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_35:                              # %for.body112
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %esi
	movq	%r8, %rdi
	andq	$255, %rdi
	cmpq	%rsi, %rdi
	je	.LBB14_36
# BB#34:                                # %for.cond109
                                        #   in Loop: Header=BB14_35 Depth=2
	incq	%rax
	movq	24(%rdx), %rcx
	movq	%rax, %rsi
	andq	%rbx, %rsi
	addq	$16, %rdx
	cmpq	$39, %rsi
	jne	.LBB14_35
	jmp	.LBB14_38
	.align	16, 0x90
.LBB14_36:                              # %for.end
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	%rbx, %rax
	cmpq	$39, %rax
	jne	.LBB14_37
.LBB14_38:                              # %if.else126
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$56, %rcx
	shlq	%cl, %r8
	movq	$56, %rcx
	sarq	%cl, %r8
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str186, %rdx
	movq	%r8, %rcx
	jmp	.LBB14_39
	.align	16, 0x90
.LBB14_37:                              # %if.then125
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str185, %rdx
.LBB14_39:                              # %if.end129
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	Perl_dump_indent
	movzbl	19(%r12), %ecx
	testq	%rcx, %rcx
	movq	%r15, %rbx
	je	.LBB14_50
# BB#40:                                # %if.then131
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str187, %rdx
	callq	Perl_dump_indent
	movzbl	18(%r12), %eax
	cmpq	$101, %rax
	jne	.LBB14_43
# BB#41:                                # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	19(%r12), %eax
	testq	$1, %rax
	je	.LBB14_43
# BB#42:                                # %if.then141
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str188, %rdx
	callq	Perl_dump_indent
.LBB14_43:                              # %if.end142
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	19(%r12), %eax
	testq	$2, %rax
	je	.LBB14_45
# BB#44:                                # %if.then147
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str189, %rdx
	callq	Perl_dump_indent
	movzbl	19(%r12), %eax
.LBB14_45:                              # %if.end148
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	$4, %rax
	je	.LBB14_47
# BB#46:                                # %if.then153
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str190, %rdx
	callq	Perl_dump_indent
.LBB14_47:                              # %if.end154
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%r12), %eax
	cmpq	$103, %rax
	jne	.LBB14_50
# BB#48:                                # %land.lhs.true159
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	19(%r12), %eax
	testq	$1, %rax
	je	.LBB14_50
# BB#49:                                # %if.then164
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str191, %rdx
	callq	Perl_dump_indent
	.align	16, 0x90
.LBB14_50:                              # %if.end166
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB14_53
# BB#51:                                # %if.then168
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str192, %rdx
	callq	Perl_dump_indent
	movzbl	19(%r12), %eax
	testq	$2, %rax
	je	.LBB14_53
# BB#52:                                # %if.then174
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r12), %rdx
	movq	96(%rsp), %rax
	movq	%rax, (%rsp)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
.LBB14_53:                              # %if.end178
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	40(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB14_55
# BB#54:                                # %if.then180
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str193, %rdx
	callq	Perl_dump_indent
.LBB14_55:                              # %if.end182
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB14_63
# BB#56:                                # %if.then184
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movabsq	$.L.str194, %rdx
	callq	Perl_dump_indent
	movq	40(%r12), %rax
	testq	%rax, %rax
	js	.LBB14_59
# BB#57:                                # %if.then189
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%r12), %eax
	cmpq	$119, %rax
	je	.LBB14_62
# BB#58:                                # %if.then194
                                        #   in Loop: Header=BB14_2 Depth=1
	xorq	%rsi, %rsi
	movabsq	$.L.str1, %rdi
	callq	Perl_newSVpvn
	movq	32(%r12), %rsi
	movq	40(%r12), %rdx
	movq	%rax, %r15
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	96(%rsp), %r8
	callq	Perl_pv_display
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str195, %rsi
	movq	%rcx, %rdx
	callq	PerlIO_printf
	movq	%r15, %rdi
	callq	Perl_sv_free
	jmp	.LBB14_62
	.align	16, 0x90
.LBB14_59:                              # %if.else200
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	cmpq	$-2, %rax
	jne	.LBB14_61
# BB#60:                                # %if.then204
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$.L.str196, %rsi
	callq	PerlIO_puts
	movq	32(%r12), %rdx
	movq	96(%rsp), %rax
	movq	%rax, (%rsp)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
	jmp	.LBB14_66
.LBB14_61:                              # %if.else209
                                        #   in Loop: Header=BB14_2 Depth=1
	movabsq	$.L.str197, %rsi
	callq	PerlIO_puts
.LBB14_62:                              # %if.end212
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB14_63:                              # %if.end214
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	18(%r12), %eax
	cmpq	$119, %rax
	jne	.LBB14_66
# BB#64:                                # %if.then219
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%r12), %r15
	testq	%r15, %r15
	je	.LBB14_66
# BB#65:                                # %for.cond223.preheader
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%r15), %r8
	movq	8(%r15), %r9
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	Perl_dump_indent
	movq	16(%r15), %r8
	movq	24(%r15), %r9
	movq	$1, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	Perl_dump_indent
.LBB14_66:                              # %for.inc236
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	movq	%r13, %r15
	movq	%rbx, %r13
	jne	.LBB14_2
.LBB14_67:                              # %for.end237
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp171:
	.size	Perl_do_magic_dump, .Ltmp171-Perl_do_magic_dump
	.cfi_endproc

	.globl	Perl_do_sv_dump
	.align	16, 0x90
	.type	Perl_do_sv_dump,@function
Perl_do_sv_dump:                        # @Perl_do_sv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp178:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp179:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp180:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp181:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp182:
	.cfi_def_cfa_offset 48
	subq	$896, %rsp              # imm = 0x380
.Ltmp183:
	.cfi_def_cfa_offset 944
.Ltmp184:
	.cfi_offset %rbx, -48
.Ltmp185:
	.cfi_offset %r12, -40
.Ltmp186:
	.cfi_offset %r13, -32
.Ltmp187:
	.cfi_offset %r14, -24
.Ltmp188:
	.cfi_offset %r15, -16
	movq	%r8, 72(%rsp)           # 8-byte Spill
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, %r12
	movq	%rdi, %r15
	testq	%r12, %r12
	je	.LBB15_1
# BB#2:                                 # %if.end
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	movq	16(%r12), %rbx
	movq	PL_dumpindent(%rip), %rcx
	movq	(%r12), %rsi
	movq	8(%r12), %r9
	imulq	%r15, %rcx
	movq	%rcx, (%rsp)
	movabsq	$.L.str205, %rdi
	movabsq	$.L.str1, %r8
	xorq	%rax, %rax
	movq	$.L.str1, 8(%rsp)
	movq	%r12, %rdx
	callq	Perl_newSVpvf
	movq	%rax, %r14
	testq	$256, %rbx              # imm = 0x100
	je	.LBB15_4
# BB#3:                                 # %if.then6
	movabsq	$.L.str206, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_4:                               # %if.end7
	testq	$512, %rbx              # imm = 0x200
	je	.LBB15_6
# BB#5:                                 # %if.then10
	movabsq	$.L.str207, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_6:                               # %if.end11
	movq	%r15, 80(%rsp)          # 8-byte Spill
	testq	$1024, %rbx             # imm = 0x400
	je	.LBB15_8
# BB#7:                                 # %if.then14
	movabsq	$.L.str208, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_8:                               # %if.end15
	testq	$2048, %rbx             # imm = 0x800
	je	.LBB15_10
# BB#9:                                 # %if.then18
	movabsq	$.L.str209, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_10:                              # %if.end19
	testq	$4096, %rbx             # imm = 0x1000
	je	.LBB15_12
# BB#11:                                # %if.then22
	movabsq	$.L.str210, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_12:                              # %if.end23
	testq	$8192, %rbx             # imm = 0x2000
	je	.LBB15_14
# BB#13:                                # %if.then26
	movabsq	$.L.str211, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_14:                              # %if.end27
	testq	$16384, %rbx            # imm = 0x4000
	je	.LBB15_16
# BB#15:                                # %if.then30
	movabsq	$.L.str212, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_16:                              # %if.end31
	testq	$32768, %rbx            # imm = 0x8000
	je	.LBB15_18
# BB#17:                                # %if.then34
	movabsq	$.L.str213, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_18:                              # %if.end35
	testq	$65536, %rbx            # imm = 0x10000
	je	.LBB15_20
# BB#19:                                # %if.then38
	movabsq	$.L.str214, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_20:                              # %if.end39
	testq	$131072, %rbx           # imm = 0x20000
	je	.LBB15_22
# BB#21:                                # %if.then42
	movabsq	$.L.str215, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_22:                              # %if.end43
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB15_24
# BB#23:                                # %if.then46
	movabsq	$.L.str216, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_24:                              # %if.end47
	testq	$524288, %rbx           # imm = 0x80000
	je	.LBB15_27
# BB#25:                                # %if.then50
	movabsq	$.L.str217, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
	movabsq	$2148007936, %rcx       # imm = 0x80080000
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB15_27
# BB#26:                                # %if.then54
	movabsq	$.L.str218, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_27:                              # %if.end56
	testq	$2097152, %rbx          # imm = 0x200000
	je	.LBB15_29
# BB#28:                                # %if.then59
	movabsq	$.L.str219, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_29:                              # %if.end60
	testq	$1048576, %rbx          # imm = 0x100000
	je	.LBB15_31
# BB#30:                                # %if.then63
	movabsq	$.L.str220, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_31:                              # %if.end64
	testq	$8388608, %rbx          # imm = 0x800000
	je	.LBB15_33
# BB#32:                                # %if.then67
	movabsq	$.L.str221, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_33:                              # %if.end68
	movq	%rbx, %r13
	andq	$255, %r13
	testq	$268435456, %rbx        # imm = 0x10000000
	je	.LBB15_36
# BB#34:                                # %if.end68
	cmpq	$11, %r13
	je	.LBB15_36
# BB#35:                                # %if.then73
	movabsq	$.L.str222, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_36:                              # %if.end74
	testq	$16777216, %rbx         # imm = 0x1000000
	je	.LBB15_38
# BB#37:                                # %if.then77
	movabsq	$.L.str223, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_38:                              # %if.end78
	testq	$33554432, %rbx         # imm = 0x2000000
	je	.LBB15_40
# BB#39:                                # %if.then81
	movabsq	$.L.str224, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_40:                              # %if.end82
	testq	$67108864, %rbx         # imm = 0x4000000
	je	.LBB15_42
# BB#41:                                # %if.then85
	movabsq	$.L.str225, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_42:                              # %if.end86
	movq	%rbx, %r15
	andq	$134217728, %r15        # imm = 0x8000000
	je	.LBB15_45
# BB#43:                                # %if.end86
	cmpq	$11, %r13
	je	.LBB15_45
# BB#44:                                # %if.then92
	movabsq	$.L.str226, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_45:                              # %if.end93
	leaq	-7(%r13), %rax
	cmpq	$7, %rax
	ja	.LBB15_105
# BB#46:                                # %if.end93
	jmpq	*.LJTI15_0(,%rax,8)
.LBB15_47:                              # %sw.bb
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
	testq	$4, %rax
	je	.LBB15_49
# BB#48:                                # %if.then98
	movabsq	$.L.str227, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_49:                              # %if.end99
	testq	$16, %rax
	je	.LBB15_51
# BB#50:                                # %if.then105
	movabsq	$.L.str228, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_51:                              # %if.end106
	testq	$1, %rax
	je	.LBB15_53
# BB#52:                                # %if.then112
	movabsq	$.L.str229, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_53:                              # %if.end113
	testq	$2, %rax
	je	.LBB15_55
# BB#54:                                # %if.then119
	movabsq	$.L.str230, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_55:                              # %if.end120
	testq	$512, %rax              # imm = 0x200
	je	.LBB15_57
# BB#56:                                # %if.then126
	movabsq	$.L.str231, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_57:                              # %if.end127
	testq	$32, %rax
	je	.LBB15_59
# BB#58:                                # %if.then133
	movabsq	$.L.str232, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_59:                              # %if.end134
	movzbl	19(%r12), %eax
	testq	$128, %rax
	je	.LBB15_61
# BB#60:                                # %if.then138
	movabsq	$.L.str233, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_61:                              # %if.end139
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
	testq	$256, %rax              # imm = 0x100
	je	.LBB15_63
# BB#62:                                # %if.then145
	movabsq	$.L.str234, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_63:                              # %if.end146
	testq	$64, %rax
	je	.LBB15_65
# BB#64:                                # %if.then152
	movabsq	$.L.str235, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_65:                              # %if.end153
	testq	$128, %rax
	je	.LBB15_67
# BB#66:                                # %if.then159
	movabsq	$.L.str236, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	136(%rax), %eax
.LBB15_67:                              # %if.end160
	testq	$1024, %rax             # imm = 0x400
	je	.LBB15_116
# BB#68:                                # %if.then166
	movabsq	$.L.str237, %rsi
	jmp	.LBB15_115
.LBB15_1:                               # %if.then
	movabsq	$.L.str204, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	Perl_dump_indent
	jmp	.LBB15_303
.LBB15_113:                             # %sw.bb298
	testq	$1073741824, %rbx       # imm = 0x40000000
	je	.LBB15_116
# BB#114:                               # %if.then301
	movabsq	$.L.str259, %rsi
	jmp	.LBB15_115
.LBB15_109:                             # %sw.bb287
	movq	16(%r12), %rax
	testq	$1073741824, %rax       # imm = 0x40000000
	je	.LBB15_111
# BB#110:                               # %if.then291
	movabsq	$.L.str257, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
.LBB15_111:                             # %if.end292
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rax
	je	.LBB15_116
# BB#112:                               # %if.then296
	movabsq	$.L.str258, %rsi
	jmp	.LBB15_115
.LBB15_69:                              # %sw.bb168
	movq	16(%r12), %rax
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB15_71
# BB#70:                                # %if.then172
	movabsq	$.L.str238, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
.LBB15_71:                              # %if.end173
	testq	$1073741824, %rax       # imm = 0x40000000
	je	.LBB15_73
# BB#72:                                # %if.then177
	movabsq	$.L.str239, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
.LBB15_73:                              # %if.end178
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rax
	je	.LBB15_75
# BB#74:                                # %if.then182
	movabsq	$.L.str240, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
.LBB15_75:                              # %if.end183
	testq	$268435456, %rax        # imm = 0x10000000
	je	.LBB15_77
# BB#76:                                # %if.then187
	movabsq	$.L.str241, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_77:                              # %if.end188
	testq	%r15, %r15
	je	.LBB15_116
# BB#78:                                # %if.then191
	movabsq	$.L.str242, %rsi
	jmp	.LBB15_115
.LBB15_79:                              # %sw.bb193
	movq	(%r12), %rax
	movzbl	88(%rax), %ecx
	testq	$1, %rcx
	je	.LBB15_81
# BB#80:                                # %if.then198
	movabsq	$.L.str243, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %ecx
.LBB15_81:                              # %if.end199
	testq	$2, %rcx
	je	.LBB15_83
# BB#82:                                # %if.then205
	movabsq	$.L.str244, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
.LBB15_83:                              # %if.end206
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_86
# BB#84:                                # %land.lhs.true209
	movzbl	72(%rcx), %ecx
	testq	$1, %rcx
	je	.LBB15_86
# BB#85:                                # %if.then214
	movabsq	$.L.str228, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
.LBB15_86:                              # %if.end215
	movzbl	88(%rax), %eax
	testq	$4, %rax
	je	.LBB15_88
# BB#87:                                # %if.then221
	movabsq	$.L.str245, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
.LBB15_88:                              # %if.end222
	testq	$8, %rax
	je	.LBB15_90
# BB#89:                                # %if.then228
	movabsq	$.L.str246, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_90:                              # %if.end229
	movabsq	$2147483648, %rax       # imm = 0x80000000
	testq	%rax, %rbx
	je	.LBB15_92
# BB#91:                                # %if.then232
	movabsq	$.L.str247, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_92:                              # %if.end233
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
	cmpq	$16, %rax
	jb	.LBB15_105
# BB#93:                                # %if.then239
	movabsq	$.L.str248, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
	cmpq	$240, %rax
	jb	.LBB15_95
# BB#94:                                # %if.then246
	movabsq	$.L.str249, %rsi
	jmp	.LBB15_104
.LBB15_95:                              # %if.else
	movabsq	$.L.str24, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
	testq	$16, %rax
	je	.LBB15_97
# BB#96:                                # %if.then252
	movabsq	$.L.str250, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
.LBB15_97:                              # %if.end253
	testq	$32, %rax
	je	.LBB15_99
# BB#98:                                # %if.then259
	movabsq	$.L.str251, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
.LBB15_99:                              # %if.end260
	testq	$64, %rax
	je	.LBB15_101
# BB#100:                               # %if.then266
	movabsq	$.L.str252, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r12), %rax
	movzbl	88(%rax), %eax
.LBB15_101:                             # %if.end267
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB15_103
# BB#102:                               # %if.then273
	movabsq	$.L.str253, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_103:                             # %if.end274
	movabsq	$.L.str254, %rsi
.LBB15_104:                             # %sw.default
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_105:                             # %sw.default
	movq	16(%r12), %rax
	testq	$1073741824, %rax       # imm = 0x40000000
	je	.LBB15_107
# BB#106:                               # %if.then280
	movabsq	$.L.str255, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	16(%r12), %rax
.LBB15_107:                             # %if.end281
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	testq	%rcx, %rax
	je	.LBB15_116
# BB#108:                               # %if.then285
	movabsq	$.L.str256, %rsi
.LBB15_115:                             # %sw.epilog
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_116:                             # %sw.epilog
	cmpq	$11, %r13
	movq	56(%rsp), %rbx          # 8-byte Reload
	je	.LBB15_119
# BB#117:                               # %land.lhs.true305
	movzbl	19(%r12), %eax
	testq	$32, %rax
	je	.LBB15_119
# BB#118:                               # %if.then309
	movabsq	$.L.str260, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_119:                             # %if.end310
	movq	(%r14), %rcx
	movq	(%rcx), %rax
	movq	8(%rcx), %rdx
	movzbl	-1(%rax,%rdx), %esi
	cmpq	$44, %rsi
	jne	.LBB15_121
# BB#120:                               # %if.then317
	decq	%rdx
	movq	%rdx, 8(%rcx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rdx)
.LBB15_121:                             # %if.end322
	movabsq	$.L.str55, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
	movq	(%r14), %rax
	movq	(%rax), %r15
	movabsq	$.L.str261, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	cmpq	$15, %r13
	jbe	.LBB15_122
# BB#142:                               # %sw.default357
	movabsq	$.L.str278, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	PerlIO_printf
	jmp	.LBB15_302
.LBB15_122:                             # %if.end322
	jmpq	*.LJTI15_1(,%r13,8)
.LBB15_123:                             # %sw.bb325
	movabsq	$.L.str262, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	PerlIO_printf
	jmp	.LBB15_302
.LBB15_124:                             # %sw.bb327
	movabsq	$.L.str263, %rsi
	jmp	.LBB15_125
.LBB15_127:                             # %if.end380.thread
	movabsq	$.L.str264, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	PerlIO_printf
	movq	80(%rsp), %r15          # 8-byte Reload
	jmp	.LBB15_149
.LBB15_128:                             # %sw.bb331
	movabsq	$.L.str265, %rsi
	jmp	.LBB15_129
.LBB15_130:                             # %sw.bb333
	movabsq	$.L.str266, %rsi
.LBB15_129:                             # %if.end380
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	PerlIO_printf
	movq	80(%rsp), %r15          # 8-byte Reload
	jmp	.LBB15_147
.LBB15_131:                             # %sw.bb335
	movabsq	$.L.str267, %rsi
	jmp	.LBB15_125
.LBB15_132:                             # %sw.bb337
	movabsq	$.L.str268, %rsi
	jmp	.LBB15_125
.LBB15_134:                             # %sw.bb341
	movabsq	$.L.str270, %rsi
	jmp	.LBB15_125
.LBB15_133:                             # %sw.bb339
	movabsq	$.L.str269, %rsi
	jmp	.LBB15_125
.LBB15_135:                             # %sw.bb343
	movabsq	$.L.str271, %rsi
	jmp	.LBB15_125
.LBB15_136:                             # %sw.bb345
	movabsq	$.L.str272, %rsi
	jmp	.LBB15_125
.LBB15_137:                             # %sw.bb347
	movabsq	$.L.str273, %rsi
	jmp	.LBB15_125
.LBB15_138:                             # %sw.bb349
	movabsq	$.L.str274, %rsi
	jmp	.LBB15_125
.LBB15_139:                             # %sw.bb351
	movabsq	$.L.str275, %rsi
	jmp	.LBB15_125
.LBB15_140:                             # %sw.bb353
	movabsq	$.L.str276, %rsi
	jmp	.LBB15_125
.LBB15_141:                             # %sw.bb355
	movabsq	$.L.str277, %rsi
.LBB15_125:                             # %if.then364
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	PerlIO_printf
	movq	(%r12), %rax
	movzbl	19(%r12), %edx
	movq	24(%rax), %rcx
	testq	$128, %rdx
	movq	80(%rsp), %r15          # 8-byte Reload
	je	.LBB15_143
# BB#126:                               # %if.then368
	movabsq	$.L.str279, %rdx
	jmp	.LBB15_144
.LBB15_143:                             # %if.else370
	movabsq	$.L.str280, %rdx
.LBB15_144:                             # %if.end372
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movzbl	18(%r12), %eax
	testq	$32, %rax
	je	.LBB15_146
# BB#145:                               # %if.then376
	movabsq	$.L.str281, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	PerlIO_printf
.LBB15_146:                             # %if.end378
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_147:                             # %if.end380
	cmpq	$5, %r13
	ja	.LBB15_149
# BB#148:                               # %if.end380
	cmpq	$2, %r13
	jne	.LBB15_150
.LBB15_149:                             # %if.then386
	movq	(%r12), %rax
	movq	32(%rax), %r8
	movabsq	$.L.str282, %rdx
	movq	$15, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
.LBB15_150:                             # %if.end388
	movzbl	18(%r12), %eax
	testq	$8, %rax
	je	.LBB15_153
# BB#151:                               # %if.then392
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movabsq	$.L.str283, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jle	.LBB15_153
# BB#152:                               # %if.then396
	movq	(%r12), %rax
	movq	(%rax), %rdx
	leaq	1(%r15), %rdi
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %rcx
	movq	944(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%rbx, %rsi
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
.LBB15_153:                             # %if.end401
	cmpq	$3, %r13
	jbe	.LBB15_302
# BB#154:                               # %if.end405
	movq	%rbx, %r15
	cmpq	$9, %r13
	ja	.LBB15_163
# BB#155:                               # %if.then408
	movq	(%r12), %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	movq	944(%rsp), %rbx
	je	.LBB15_161
# BB#156:                               # %if.then412
	movabsq	$.L.str284, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movzbl	18(%r12), %eax
	testq	$32, %rax
	je	.LBB15_158
# BB#157:                               # %if.then418
	movq	(%r12), %rax
	movq	(%rax), %rsi
	movq	24(%rax), %rdx
	subq	%rdx, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %r8
	callq	Perl_pv_display
	movq	%rax, %rcx
	movabsq	$.L.str285, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rdx
	callq	PerlIO_printf
.LBB15_158:                             # %if.end428
	movq	(%r12), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	movq	16(%rax), %rcx
	movq	%r14, %rdi
	movq	%rbx, %r8
	callq	Perl_pv_display
	movq	%rax, %rcx
	movabsq	$.L.str286, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rdx
	callq	PerlIO_printf
	movzbl	19(%r12), %eax
	testq	$32, %rax
	je	.LBB15_160
# BB#159:                               # %if.then439
	movq	%r12, %rdi
	callq	Perl_sv_len_utf8
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	Perl_sv_uni_display
	movq	%rax, %rcx
	movabsq	$.L.str50, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rdx
	callq	PerlIO_printf
.LBB15_160:                             # %if.end444
	movabsq	$.L.str287, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	PerlIO_printf
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movabsq	$.L.str288, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	16(%rax), %rcx
	movabsq	$.L.str289, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	jmp	.LBB15_162
.LBB15_161:                             # %if.else450
	movabsq	$.L.str290, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_dump_indent
.LBB15_162:                             # %if.end452
	cmpq	$7, %r13
	jb	.LBB15_169
.LBB15_163:                             # %if.then455
	movq	(%r12), %rax
	movq	40(%rax), %rdx
	testq	%rdx, %rdx
	movq	944(%rsp), %rcx
	je	.LBB15_165
# BB#164:                               # %if.then458
	movq	%rcx, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_magic_dump
	movq	(%r12), %rax
.LBB15_165:                             # %if.end461
	movq	48(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB15_169
# BB#166:                               # %land.lhs.true.i
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str291, %rcx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %r8
	callq	Perl_dump_indent
	movq	(%rbx), %rax
	movq	80(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_168
# BB#167:                               # %if.then.i
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_169
.LBB15_168:                             # %if.else.i
	movq	$10, %rsi
	movq	%r15, %rdi
	callq	PerlIO_putc
.LBB15_169:                             # %if.end468
	leaq	-9(%r13), %rax
	cmpq	$6, %rax
	ja	.LBB15_302
# BB#170:                               # %if.end468
	jmpq	*.LJTI15_2(,%rax,8)
.LBB15_171:                             # %sw.bb469
	movq	(%r12), %rax
	movsbq	80(%rax), %rcx
	movabsq	$.L.str292, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%r15, %rbx
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str293, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str294, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	72(%rax), %rcx
	movabsq	$.L.str295, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movzbl	80(%rax), %ecx
	orq	$32, %rcx
	andq	$255, %rcx
	cmpq	$116, %rcx
	je	.LBB15_302
# BB#172:                               # %if.then486
	movq	72(%rax), %rdx
	movq	%r13, %rdi
	jmp	.LBB15_173
.LBB15_174:                             # %sw.bb492
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movabsq	$.L.str296, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	(%rax), %rdx
	movq	56(%rax), %rax
	subq	%rax, %rdx
	je	.LBB15_176
# BB#175:                               # %if.then499
	movq	$3, %rcx
	sarq	%cl, %rdx
	movabsq	$.L.str297, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	PerlIO_printf
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str298, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	jmp	.LBB15_177
.LBB15_194:                             # %sw.bb557
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movabsq	$.L.str296, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rbx
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	cmpq	$0, (%rax)
	je	.LBB15_219
# BB#195:                               # %land.lhs.true562
	movq	24(%rax), %r15
	movq	32(%rax), %rdi
	callq	float64_to_int64
	subq	%rax, %r15
	je	.LBB15_219
# BB#196:                               # %if.then568
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movabsq	$.L.str307, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	callq	PerlIO_printf
	leaq	96(%rsp), %rdi
	xorq	%rsi, %rsi
	movq	$800, %rdx              # imm = 0x320
	callq	memset
	movq	(%r12), %rcx
	movq	(%rcx), %r8
	movq	16(%rcx), %r9
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
.LBB15_197:                             # %for.body581
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_198 Depth 2
	movq	(%r8,%rbx,8), %rdi
	testq	%rdi, %rdi
	movabsq	$0, %rcx
	je	.LBB15_199
	.align	16, 0x90
.LBB15_198:                             # %for.body588
                                        #   Parent Loop BB15_197 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdi), %rdi
	incq	%rcx
	testq	%rdi, %rdi
	jne	.LBB15_198
.LBB15_199:                             # %for.end591
                                        #   in Loop: Header=BB15_197 Depth=1
	movq	%rcx, %rdi
	andq	%rdx, %rdi
	cmpq	$199, %rdi
	movq	$199, %r15
	ja	.LBB15_201
# BB#200:                               # %for.end591
                                        #   in Loop: Header=BB15_197 Depth=1
	movq	%rcx, %r15
.LBB15_201:                             # %for.end591
                                        #   in Loop: Header=BB15_197 Depth=1
	movq	$32, %rcx
	movq	%r15, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	96(%rsp,%rdi,4), %ecx
	incq	%rcx
	movl	%ecx, 96(%rsp,%rdi,4)
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	jl	.LBB15_203
# BB#202:                               # %for.end591
                                        #   in Loop: Header=BB15_197 Depth=1
	movq	%rax, %r15
.LBB15_203:                             # %for.end591
                                        #   in Loop: Header=BB15_197 Depth=1
	cmpq	%r9, %rbx
	leaq	1(%rbx), %rbx
	movq	%r15, %rax
	jne	.LBB15_197
# BB#204:                               # %for.cond607.preheader
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_205
# BB#206:                               # %for.body610.lr.ph
	leaq	1(%r15), %rax
	xorq	%rbx, %rbx
	andq	%rdx, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB15_207:                             # %for.body610
                                        # =>This Inner Loop Header: Depth=1
	movl	96(%rsp,%rbx,4), %r8d
	testq	%r8, %r8
	je	.LBB15_212
# BB#208:                               # %if.then614
                                        #   in Loop: Header=BB15_207 Depth=1
	movq	%rbx, %r13
	andq	%rdx, %r13
	cmpq	$199, %r13
	movabsq	$.L.str309, %rcx
	je	.LBB15_210
# BB#209:                               # %if.then614
                                        #   in Loop: Header=BB15_207 Depth=1
	movabsq	$.L.str1, %rcx
.LBB15_210:                             # %if.then614
                                        #   in Loop: Header=BB15_207 Depth=1
	xorq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str308, %rsi
	movq	%rbx, %rdx
	callq	PerlIO_printf
	movq	%r15, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	%rax, %r13
	je	.LBB15_212
# BB#211:                               # %if.then624
                                        #   in Loop: Header=BB15_207 Depth=1
	xorq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str310, %rsi
	movq	%rdx, %r13
	callq	PerlIO_printf
	movq	%r13, %rdx
.LBB15_212:                             # %for.inc628
                                        #   in Loop: Header=BB15_207 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB15_207
# BB#213:                               # %for.end630
	movq	$41, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %r13
	callq	PerlIO_putc
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB15_216
# BB#214:                               # %for.body635.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	xorq	%rbx, %rbx
.LBB15_215:                             # %for.body635
                                        # =>This Inner Loop Header: Depth=1
	movl	96(%rsp,%r15,4), %eax
	movq	%r15, %rdi
	imulq	%rdi, %rdi
	imulq	%rax, %rdi
	andq	%r13, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB15_215
	jmp	.LBB15_216
.LBB15_237:                             # %sw.bb748
	movzbl	18(%r12), %eax
	testq	$4, %rax
	je	.LBB15_239
# BB#238:                               # %cond.true757
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movabsq	$.L.str321, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_dump_indent
.LBB15_239:                             # %sw.bb765
	movq	(%r12), %rax
	movq	56(%rax), %rbx
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str322, %rcx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB15_242
# BB#240:                               # %land.lhs.true.i1573
	movq	(%rbx), %rax
	movq	80(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_242
# BB#241:                               # %if.then.i1575
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_243
.LBB15_242:                             # %if.else.i1577
	movq	$10, %rsi
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_243:                             # %Perl_do_hv_dump.exit1578
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	testq	%rcx, %rcx
	movq	80(%rsp), %r15          # 8-byte Reload
	je	.LBB15_245
# BB#244:                               # %if.then769
	movzbl	34(%rcx), %r8d
	movabsq	$.L.str323, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
.LBB15_245:                             # %if.end775
	movq	72(%rax), %rcx
	movabsq	$.L.str324, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	72(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_248
# BB#246:                               # %if.end775
	testq	$255, 48(%rsp)          # 8-byte Folded Reload
	je	.LBB15_248
# BB#247:                               # %if.then783
	leaq	1(%r15), %rdi
	movq	%rbx, %rsi
	callq	Perl_do_op_dump
	movq	(%r12), %rax
.LBB15_248:                             # %if.end787
	movq	80(%rax), %rcx
	movabsq	$.L.str325, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	88(%rax), %rcx
	movabsq	$.L.str326, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	96(%rax), %rcx
	movabsq	$.L.str327, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_do_gvgv_dump
	movq	(%r12), %rax
	movq	104(%rax), %rcx
	movabsq	$.L.str328, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	112(%rax), %rcx
	movabsq	$.L.str329, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movzbl	136(%rax), %ecx
	movabsq	$.L.str330, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	144(%rax), %rcx
	movabsq	$.L.str331, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	cmpq	$14, %r13
	jne	.LBB15_250
# BB#249:                               # %if.then799
	movq	(%r12), %rax
	movq	152(%rax), %rcx
	movabsq	$.L.str332, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
.LBB15_250:                             # %if.end801
	movq	(%r12), %rax
	movq	120(%rax), %rcx
	movabsq	$.L.str333, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jle	.LBB15_252
# BB#251:                               # %if.then805
	movq	(%r12), %rax
	movq	120(%rax), %rdx
	leaq	1(%r15), %rdi
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	do_dump_pad
.LBB15_252:                             # %if.end810
	movq	(%r12), %rax
	movq	128(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_253
# BB#254:                               # %cond.false814
	movq	(%rcx), %rax
	movzbl	136(%rax), %edx
	testq	$4, %rdx
	je	.LBB15_256
# BB#255:
	movabsq	$.L.str336, %r8
	jmp	.LBB15_263
.LBB15_266:                             # %sw.bb871
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str316, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %r13
	movq	%r13, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	72(%rax), %rcx
	movabsq	$.L.str340, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	80(%rax), %r15
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str341, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r13, %rbx
	movq	%r15, %r8
	callq	Perl_dump_indent
	testq	%r15, %r15
	je	.LBB15_269
# BB#267:                               # %land.lhs.true.i1583
	movq	(%r15), %rax
	movq	80(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_269
# BB#268:                               # %if.then.i1585
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_270
.LBB15_275:                             # %sw.bb911
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str356, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rbx
	movq	%rdi, %r15
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str357, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	72(%rax), %rcx
	movabsq	$.L.str358, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	80(%rax), %rcx
	movabsq	$.L.str332, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	88(%rax), %rcx
	movabsq	$.L.str359, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	96(%rax), %rcx
	movabsq	$.L.str360, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	104(%rax), %rcx
	movabsq	$.L.str361, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	112(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_277
# BB#276:                               # %if.then921
	movabsq	$.L.str362, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
.LBB15_277:                             # %if.end924
	movq	%rbx, %rsi
	movq	120(%rax), %rbx
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str363, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r15, %r13
	movq	%rsi, %r15
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB15_280
# BB#278:                               # %land.lhs.true.i1601
	movq	(%rbx), %rax
	movq	64(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_280
# BB#279:                               # %if.then.i1603
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_281
.LBB15_253:
	movabsq	$.L.str335, %r8
	jmp	.LBB15_263
.LBB15_269:                             # %if.else.i1587
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_270:                             # %Perl_do_hv_dump.exit1588
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str342, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	testq	%rax, %rax
	je	.LBB15_302
# BB#271:                               # %if.end882
	movq	(%rax), %rcx
	movabsq	$.L.str343, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	%rbx, %r15
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	8(%rax), %rcx
	movabsq	$.L.str344, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	16(%rax), %rcx
	movabsq	$.L.str345, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	24(%rax), %rcx
	movabsq	$.L.str346, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	32(%rax), %rcx
	movabsq	$.L.str347, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	40(%rax), %rcx
	movabsq	$.L.str348, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str349, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str350, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	72(%rax), %rcx
	movabsq	$.L.str351, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	80(%rax), %rcx
	movabsq	$.L.str352, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	88(%rax), %rcx
	movabsq	$.L.str353, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movzbl	88(%rax), %ecx
	movabsq	$.L.str354, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rax
	movq	48(%rax), %rbx
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str355, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB15_274
# BB#272:                               # %land.lhs.true.i1592
	movq	(%rbx), %rax
	movq	64(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_274
# BB#273:                               # %if.then.i1594
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_302
.LBB15_280:                             # %if.else.i1605
	movq	$10, %rsi
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_281:                             # %Perl_do_gv_dump.exit1606
	movq	(%r12), %rax
	movq	128(%rax), %rcx
	testq	%rcx, %rcx
	movq	%r13, %r15
	je	.LBB15_283
# BB#282:                               # %if.then928
	movabsq	$.L.str364, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
.LBB15_283:                             # %if.end931
	movq	%rbx, %rsi
	movq	136(%rax), %rbx
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str365, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r15, %r13
	movq	%rsi, %r15
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB15_286
# BB#284:                               # %land.lhs.true.i1611
	movq	(%rbx), %rax
	movq	64(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_286
# BB#285:                               # %if.then.i1613
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_287
.LBB15_286:                             # %if.else.i1615
	movq	$10, %rsi
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_287:                             # %Perl_do_gv_dump.exit1616
	movq	(%r12), %rax
	movq	144(%rax), %rcx
	testq	%rcx, %rcx
	movq	%r13, %r15
	je	.LBB15_289
# BB#288:                               # %if.then935
	movabsq	$.L.str366, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
.LBB15_289:                             # %if.end938
	movq	%rbx, %rsi
	movq	152(%rax), %rbx
	movabsq	$.L.str199, %rdx
	movabsq	$.L.str367, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r15, %r13
	movq	%rsi, %r15
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB15_292
# BB#290:                               # %land.lhs.true.i1621
	movq	(%rbx), %rax
	movq	64(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB15_292
# BB#291:                               # %if.then.i1623
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_printf
	jmp	.LBB15_293
.LBB15_292:                             # %if.else.i1625
	movq	$10, %rsi
	movq	%r15, %rbx
	movq	%rbx, %rdi
	callq	PerlIO_putc
.LBB15_293:                             # %Perl_do_gv_dump.exit1626
	movq	(%r12), %rax
	movswq	160(%rax), %rcx
	movabsq	$.L.str368, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r15
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movzbl	162(%rax), %eax
	movq	$56, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$56, %rcx
	sarq	%cl, %rbx
	cmpq	$32, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jle	.LBB15_295
# BB#294:                               # %Perl_do_gv_dump.exit1626
	movq	%rsi, %rdx
.LBB15_295:                             # %Perl_do_gv_dump.exit1626
	cmpq	$127, %rax
	je	.LBB15_297
# BB#296:                               # %Perl_do_gv_dump.exit1626
	movq	%rsi, %rcx
.LBB15_297:                             # %Perl_do_gv_dump.exit1626
	testq	%rdx, %rcx
	jne	.LBB15_299
# BB#298:                               # %Perl_do_gv_dump.exit1626
	cmpq	$32, %rax
	je	.LBB15_299
# BB#300:                               # %if.else962
	movabsq	$.L.str370, %rdx
	jmp	.LBB15_301
.LBB15_299:                             # %if.then958
	movabsq	$.L.str369, %rdx
.LBB15_301:                             # %if.end966
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rcx
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movsbq	163(%rax), %rcx
	movabsq	$.L.str330, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	jmp	.LBB15_302
.LBB15_256:                             # %cond.false821
	movq	PL_main_cv(%rip), %rsi
	cmpq	%rsi, %rcx
	je	.LBB15_257
# BB#258:                               # %cond.false825
	testq	$16, %rdx
	je	.LBB15_260
# BB#259:
	movabsq	$.L.str338, %r8
	jmp	.LBB15_263
.LBB15_176:                             # %if.else507
	movq	$10, %rsi
	movq	%r15, %rdi
	callq	PerlIO_putc
.LBB15_177:                             # %if.end509
	movq	%r13, %rbx
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movabsq	$.L.str299, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	16(%rax), %rcx
	movabsq	$.L.str300, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str301, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movzbl	72(%rax), %ebx
	movabsq	$.L.str1, %r13
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_setpv
	testq	$1, %rbx
	je	.LBB15_179
# BB#178:                               # %if.then517
	movabsq	$.L.str302, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_179:                             # %if.end518
	testq	$2, %rbx
	je	.LBB15_181
# BB#180:                               # %if.then521
	movabsq	$.L.str303, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_181:                             # %if.end522
	testq	$4, %rbx
	je	.LBB15_183
# BB#182:                               # %if.then525
	movabsq	$.L.str304, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_catpv
.LBB15_183:                             # %if.end526
	movq	(%r14), %rax
	cmpq	$0, 8(%rax)
	movq	%r15, %rsi
	je	.LBB15_185
# BB#184:                               # %cond.true
	movq	(%rax), %r13
	incq	%r13
.LBB15_185:                             # %cond.end
	movabsq	$.L.str305, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rsi, %r15
	movq	%r13, %rcx
	callq	Perl_dump_indent
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jle	.LBB15_302
# BB#186:                               # %land.lhs.true535
	movq	%r12, %rdi
	callq	Perl_av_len
	testq	%rax, %rax
	js	.LBB15_302
# BB#187:                               # %for.cond.preheader
	movq	%r12, %rdi
	callq	Perl_av_len
	testq	%rax, %rax
	js	.LBB15_302
# BB#188:                               # %for.cond.preheader
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	jle	.LBB15_302
# BB#189:                               # %for.body.lr.ph
	incq	80(%rsp)                # 8-byte Folded Spill
	incq	64(%rsp)                # 8-byte Folded Spill
	xorq	%rbx, %rbx
.LBB15_190:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	movq	%rax, %r13
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movabsq	$.L.str306, %rdx
	movq	%rbx, %rcx
	callq	Perl_dump_indent
	testq	%r13, %r13
	je	.LBB15_192
# BB#191:                               # %if.then552
                                        #   in Loop: Header=BB15_190 Depth=1
	movq	(%r13), %rdx
	movq	944(%rsp), %rax
	movq	%rax, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
.LBB15_192:                             # %for.inc
                                        #   in Loop: Header=BB15_190 Depth=1
	movq	%r12, %rdi
	callq	Perl_av_len
	cmpq	%rax, %rbx
	jge	.LBB15_302
# BB#193:                               # %for.inc
                                        #   in Loop: Header=BB15_190 Depth=1
	incq	%rbx
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	jl	.LBB15_190
	jmp	.LBB15_302
.LBB15_274:                             # %if.else.i1596
	movq	$10, %rsi
	movq	%r15, %rdi
	callq	PerlIO_putc
	jmp	.LBB15_302
.LBB15_257:
	movabsq	$.L.str337, %r8
	jmp	.LBB15_263
.LBB15_205:                             # %for.end630.thread
	movq	$41, %rsi
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	PerlIO_putc
	xorq	%rbx, %rbx
.LBB15_216:                             # %while.cond.preheader
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$2, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	$1, %rcx
.LBB15_217:                             # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	shrq	%cl, %rdx
	leaq	(%rbx,%rbx), %rax
	testq	%rdx, %rdx
	jne	.LBB15_217
# BB#218:                               # %while.end
	movq	(%r12), %rax
	movq	24(%rax), %r13
	movq	32(%rax), %rdi
	callq	float64_to_int64
	subq	%rax, %r13
	movq	%r13, %rdi
	callq	int64_to_float64
	movq	%rax, %r13
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	%r13, %rdi
	callq	float64_add
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	int64_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, %r15
	movq	$10, %rsi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	PerlIO_putc
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	float64_div
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, %rcx
	movabsq	$.L.str311, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_dump_indent
.LBB15_219:                             # %if.end660
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	PerlIO_putc
	movq	(%r12), %rax
	movq	%rbx, %r15
	movq	24(%rax), %rbx
	movq	32(%rax), %rdi
	callq	float64_to_int64
	subq	%rax, %rbx
	movabsq	$.L.str312, %rdx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	movq	%rbx, %rcx
	movq	%rdi, %rbx
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movabsq	$.L.str299, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	16(%rax), %rcx
	movabsq	$.L.str300, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	56(%rax), %rcx
	movabsq	$.L.str313, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	64(%rax), %rcx
	movabsq	$.L.str314, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
	movq	72(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_221
# BB#220:                               # %if.then675
	movabsq	$.L.str315, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
	movq	(%r12), %rax
.LBB15_221:                             # %if.end678
	movq	80(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_223
# BB#222:                               # %if.then681
	movabsq	$.L.str316, %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_dump_indent
.LBB15_223:                             # %if.end684
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	64(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r15
	jle	.LBB15_302
# BB#224:                               # %land.lhs.true687
	movq	(%r12), %rax
	cmpq	$0, 64(%rax)
	jne	.LBB15_302
# BB#225:                               # %if.then691
	movq	%r12, %rdi
	callq	Perl_hv_iterinit
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_hv_iternext_flags
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%rbx, %rdi
	je	.LBB15_236
# BB#226:                               # %land.rhs699.lr.ph
	incq	%rdi
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	incq	64(%rsp)                # 8-byte Folded Spill
.LBB15_227:                             # %land.rhs699
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB15_236
# BB#228:                               # %while.body703
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	8(%r13), %rax
	movq	(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	decq	%r15
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	Perl_hv_iterkeysv
	movq	%rax, %r15
	movzbl	18(%r15), %eax
	testq	$4, %rax
	jne	.LBB15_229
# BB#230:                               # %cond.false715
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	$2, %rdx
	movq	%r15, %rdi
	leaq	88(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
	jmp	.LBB15_231
.LBB15_229:                             # %cond.true710
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 88(%rsp)
	movq	(%rax), %rbx
.LBB15_231:                             # %cond.end717
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	Perl_hv_iterval
	movq	88(%rsp), %rdx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	944(%rsp), %r8
	callq	Perl_pv_display
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	movabsq	$.L.str317, %rdx
	callq	Perl_dump_indent
	movzbl	19(%r15), %eax
	testq	$32, %rax
	je	.LBB15_233
# BB#232:                               # %if.then725
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	%r15, %rdi
	callq	Perl_sv_len_utf8
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	Perl_sv_uni_display
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str318, %rsi
	movq	%rcx, %rdx
	callq	PerlIO_printf
.LBB15_233:                             # %if.end730
                                        #   in Loop: Header=BB15_227 Depth=1
	movq	8(%r13), %rax
	movq	8(%rax), %rcx
	movzbl	17(%rcx,%rax), %eax
	testq	$4, %rax
	je	.LBB15_235
# BB#234:                               # %if.then739
                                        #   in Loop: Header=BB15_227 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str319, %rsi
	callq	PerlIO_printf
.LBB15_235:                             # %if.end741
                                        #   in Loop: Header=BB15_227 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str320, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	PerlIO_printf
	movq	944(%rsp), %rax
	movq	%rax, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_hv_iternext_flags
	movq	%rax, %r13
	testq	%r13, %r13
	movq	40(%rsp), %r15          # 8-byte Reload
	jne	.LBB15_227
.LBB15_236:                             # %while.end745
	movq	%r12, %rdi
	callq	Perl_hv_iterinit
	jmp	.LBB15_302
.LBB15_260:                             # %cond.false832
	movq	96(%rax), %rax
	testq	%rax, %rax
	je	.LBB15_261
# BB#262:                               # %cond.true836
	movq	(%rax), %rax
	movq	64(%rax), %r8
	jmp	.LBB15_263
.LBB15_261:
	movabsq	$.L.str339, %r8
.LBB15_263:                             # %cond.end849
	movabsq	$.L.str334, %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_dump_indent
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jle	.LBB15_302
# BB#264:                               # %land.lhs.true853
	movq	(%r12), %rax
	movzbl	136(%rax), %ecx
	testq	$3, %rcx
	je	.LBB15_302
# BB#265:                               # %if.then865
	movq	128(%rax), %rdx
	movq	%r15, %rdi
.LBB15_173:                             # %sw.epilog969
	incq	%rdi
	movq	64(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	movq	944(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%rbx, %rsi
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	Perl_do_sv_dump
.LBB15_302:                             # %sw.epilog969
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB15_303:                             # %return
	addq	$896, %rsp              # imm = 0x380
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp189:
	.size	Perl_do_sv_dump, .Ltmp189-Perl_do_sv_dump
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_113
	.quad	.LBB15_109
	.quad	.LBB15_105
	.quad	.LBB15_105
	.quad	.LBB15_69
	.quad	.LBB15_47
	.quad	.LBB15_79
	.quad	.LBB15_47
.LJTI15_1:
	.quad	.LBB15_123
	.quad	.LBB15_124
	.quad	.LBB15_127
	.quad	.LBB15_128
	.quad	.LBB15_130
	.quad	.LBB15_131
	.quad	.LBB15_132
	.quad	.LBB15_134
	.quad	.LBB15_133
	.quad	.LBB15_135
	.quad	.LBB15_136
	.quad	.LBB15_137
	.quad	.LBB15_138
	.quad	.LBB15_139
	.quad	.LBB15_140
	.quad	.LBB15_141
.LJTI15_2:
	.quad	.LBB15_171
	.quad	.LBB15_174
	.quad	.LBB15_194
	.quad	.LBB15_237
	.quad	.LBB15_266
	.quad	.LBB15_239
	.quad	.LBB15_275

	.text
	.globl	Perl_magic_dump
	.align	16, 0x90
	.type	Perl_magic_dump,@function
Perl_magic_dump:                        # @Perl_magic_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp192:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp193:
	.cfi_def_cfa_offset 32
.Ltmp194:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	Perl_do_magic_dump
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp195:
	.size	Perl_magic_dump, .Ltmp195-Perl_magic_dump
	.cfi_endproc

	.globl	Perl_do_hv_dump
	.align	16, 0x90
	.type	Perl_do_hv_dump,@function
Perl_do_hv_dump:                        # @Perl_do_hv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp199:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp201:
	.cfi_def_cfa_offset 32
.Ltmp202:
	.cfi_offset %rbx, -24
.Ltmp203:
	.cfi_offset %r14, -16
	movq	%rcx, %rbx
	movq	%rdx, %rcx
	movq	%rsi, %r14
	movabsq	$.L.str199, %rdx
	xorq	%rax, %rax
                                        # kill: RSI<def> R14<kill>
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB17_3
# BB#1:                                 # %land.lhs.true
	movq	(%rbx), %rax
	movq	80(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB17_3
# BB#2:                                 # %if.then
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_printf  # TAILCALL
.LBB17_3:                               # %if.else
	movq	$10, %rsi
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_putc  # TAILCALL
.Ltmp204:
	.size	Perl_do_hv_dump, .Ltmp204-Perl_do_hv_dump
	.cfi_endproc

	.globl	Perl_do_gv_dump
	.align	16, 0x90
	.type	Perl_do_gv_dump,@function
Perl_do_gv_dump:                        # @Perl_do_gv_dump
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
	movq	%rcx, %rbx
	movq	%rdx, %rcx
	movq	%rsi, %r14
	movabsq	$.L.str199, %rdx
	xorq	%rax, %rax
                                        # kill: RSI<def> R14<kill>
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB18_3
# BB#1:                                 # %land.lhs.true
	movq	(%rbx), %rax
	movq	64(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB18_3
# BB#2:                                 # %if.then
	movabsq	$.L.str200, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_printf  # TAILCALL
.LBB18_3:                               # %if.else
	movq	$10, %rsi
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_putc  # TAILCALL
.Ltmp213:
	.size	Perl_do_gv_dump, .Ltmp213-Perl_do_gv_dump
	.cfi_endproc

	.globl	Perl_do_gvgv_dump
	.align	16, 0x90
	.type	Perl_do_gvgv_dump,@function
Perl_do_gvgv_dump:                      # @Perl_do_gvgv_dump
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
	movq	%rcx, %rbx
	movq	%rdx, %rcx
	movq	%rsi, %r14
	movabsq	$.L.str199, %rdx
	xorq	%rax, %rax
                                        # kill: RSI<def> R14<kill>
	movq	%rbx, %r8
	callq	Perl_dump_indent
	testq	%rbx, %rbx
	je	.LBB19_6
# BB#1:                                 # %land.lhs.true
	movq	(%rbx), %rax
	cmpq	$0, 64(%rax)
	je	.LBB19_6
# BB#2:                                 # %if.then
	movabsq	$.L.str201, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	movq	(%rbx), %rax
	movq	80(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB19_5
# BB#3:                                 # %land.lhs.true4
	movq	(%rcx), %rcx
	movq	80(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB19_5
# BB#4:                                 # %if.then9
	movabsq	$.L.str202, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	movq	(%rbx), %rax
.LBB19_5:                               # %if.end
	movq	64(%rax), %rdx
	movabsq	$.L.str203, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_printf  # TAILCALL
.LBB19_6:                               # %if.else
	movq	$10, %rsi
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	PerlIO_putc  # TAILCALL
.Ltmp222:
	.size	Perl_do_gvgv_dump, .Ltmp222-Perl_do_gvgv_dump
	.cfi_endproc

	.globl	Perl_sv_dump
	.align	16, 0x90
	.type	Perl_sv_dump,@function
Perl_sv_dump:                           # @Perl_sv_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp225:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp226:
	.cfi_def_cfa_offset 32
.Ltmp227:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	Perl_do_sv_dump
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp228:
	.size	Perl_sv_dump, .Ltmp228-Perl_sv_dump
	.cfi_endproc

	.globl	Perl_runops_debug
	.align	16, 0x90
	.type	Perl_runops_debug,@function
Perl_runops_debug:                      # @Perl_runops_debug
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp232:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp233:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp234:
	.cfi_def_cfa_offset 32
.Ltmp235:
	.cfi_offset %rbx, -24
.Ltmp236:
	.cfi_offset %r14, -16
	cmpq	$0, PL_op(%rip)
	je	.LBB21_11
# BB#1:
	movabsq	$.L.str287, %r14
	movabsq	$.L.str372, %rbx
	.align	16, 0x90
.LBB21_2:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movl	PL_sig_pending(%rip), %eax
	testq	%rax, %rax
	je	.LBB21_4
# BB#3:                                 # %if.then11
                                        #   in Loop: Header=BB21_2 Depth=1
	callq	Perl_despatch_signals
.LBB21_4:                               # %if.end12
                                        #   in Loop: Header=BB21_2 Depth=1
	cmpq	$0, PL_debug(%rip)
	je	.LBB21_21
# BB#5:                                 # %if.then14
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_watchaddr(%rip), %rax
	testq	%rax, %rax
	je	.LBB21_8
# BB#6:                                 # %land.lhs.true17
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	(%rax), %rax
	movq	PL_watchok(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB21_8
# BB#7:                                 # %if.then20
                                        #   in Loop: Header=BB21_2 Depth=1
	callq	Perl_PerlIO_stderr
	movq	PL_watchaddr(%rip), %rdx
	movq	PL_watchok(%rip), %rcx
	movq	(%rdx), %r8
	movq	%rax, %rsi
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	callq	PerlIO_printf
.LBB21_8:                               # %if.end22
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_debug(%rip), %rax
	testq	$2, %rax
	je	.LBB21_17
# BB#9:                                 # %if.then25
                                        #   in Loop: Header=BB21_2 Depth=1
	testq	$1048576, PL_debug(%rip) # imm = 0x100000
	jne	.LBB21_10
# BB#16:                                # %if.else
                                        #   in Loop: Header=BB21_2 Depth=1
	callq	Perl_debstack
	jmp	.LBB21_17
.LBB21_10:                              # %if.then28
                                        #   in Loop: Header=BB21_2 Depth=1
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r14, %rsi
	callq	PerlIO_printf
	xorq	%rax, %rax
	callq	deb_stack_all
.LBB21_17:                              # %if.end34
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_debug(%rip), %rax
	testq	$8, %rax
	je	.LBB21_19
# BB#18:                                # %if.then37
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_op(%rip), %rdi
	callq	Perl_debop
.LBB21_19:                              # %if.end39
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_debug(%rip), %rax
	testq	$64, %rax
	je	.LBB21_21
# BB#20:                                # %if.then42
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_op(%rip), %rdi
	xorq	%rax, %rax
	callq	debprof
.LBB21_21:                              # %do.cond
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	PL_op(%rip), %rax
	callq	*16(%rax)
	testq	%rax, %rax
	movq	%rax, PL_op(%rip)
	jne	.LBB21_2
# BB#22:                                # %do.end
	xorq	%rax, %rax
	movb	%al, PL_tainted(%rip)
	jmp	.LBB21_23
.LBB21_11:                              # %if.then
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB21_15
# BB#12:                                # %lor.lhs.false
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB21_15
# BB#13:                                # %lor.lhs.false3
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB21_23
# BB#14:                                # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	5(%rax), %eax
	testq	$16, %rax
	je	.LBB21_23
.LBB21_15:                              # %if.then8
	movabsq	$.L.str371, %rsi
	movq	$22, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB21_23:                              # %return
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp237:
	.size	Perl_runops_debug, .Ltmp237-Perl_runops_debug
	.cfi_endproc

	.globl	Perl_debop
	.align	16, 0x90
	.type	Perl_debop,@function
Perl_debop:                             # @Perl_debop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 32
.Ltmp245:
	.cfi_offset %rbx, -32
.Ltmp246:
	.cfi_offset %r14, -24
.Ltmp247:
	.cfi_offset %r15, -16
	movq	PL_curcop(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_debstash(%rip), %rcx
	movq	%rdi, %rbx
	cmpq	%rcx, %rax
	jne	.LBB22_2
# BB#1:                                 # %land.lhs.true
	testq	$524288, PL_debug(%rip) # imm = 0x80000
	je	.LBB22_26
.LBB22_2:                               # %if.end
	movzbl	32(%rbx), %eax
	cmpq	$351, %rax              # imm = 0x15F
	jne	.LBB22_4
# BB#3:                                 # %cond.true
	movq	%rbx, %rdi
	callq	Perl_custom_op_name
	movq	%rax, %rsi
	jmp	.LBB22_5
.LBB22_4:                               # %cond.false
	movq	PL_op_name(,%rax,8), %rsi
.LBB22_5:                               # %cond.end
	movabsq	$.L.str286, %rdi
	xorq	%rax, %rax
	callq	Perl_deb
	movzbl	32(%rbx), %eax
	leaq	-9(%rax), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$3, %rcx
	jae	.LBB22_6
# BB#15:                                # %sw.bb28
	movq	PL_curstackinfo(%rip), %rax
	movq	16(%rax), %rdi
	xorq	%rax, %rax
	callq	deb_curcv
	testq	%r14, %rax
	je	.LBB22_16
# BB#18:                                # %if.end37
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rax), %rax
	movq	120(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	Perl_av_fetch
	movq	(%rax), %rdi
	movq	24(%rbx), %rsi
	xorq	%rdx, %rdx
	callq	Perl_av_fetch
	movq	(%rax), %r15
	callq	Perl_PerlIO_stderr
	movq	%rax, %r14
	testq	%r15, %r15
	je	.LBB22_17
# BB#19:                                # %if.then39
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB22_21
# BB#20:                                # %cond.true45
	movq	(%r15), %rax
	movq	(%rax), %rdx
	jmp	.LBB22_22
.LBB22_6:                               # %cond.end
	leaq	-6(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jae	.LBB22_7
# BB#9:                                 # %sw.bb9
	cmpq	$0, 40(%rbx)
	je	.LBB22_14
# BB#10:                                # %if.then12
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	40(%rbx), %rsi
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	callq	Perl_gv_fullname4
	callq	Perl_PerlIO_stderr
	movzbl	18(%rbx), %ecx
	movq	%rax, %r14
	testq	$4, %rcx
	je	.LBB22_12
# BB#11:                                # %cond.true19
	movq	(%rbx), %rax
	movq	(%rax), %rdx
	jmp	.LBB22_13
.LBB22_16:                              # %if.end37.thread
	callq	Perl_PerlIO_stderr
	movq	%rax, %r14
.LBB22_17:                              # %if.else53
	addq	$24, %rbx
	movq	(%rbx), %rdx
	movabsq	$.L.str375, %rsi
	jmp	.LBB22_23
.LBB22_7:                               # %cond.end
	cmpq	$5, %rax
	jne	.LBB22_25
# BB#8:                                 # %sw.bb
	callq	Perl_PerlIO_stderr
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	callq	Perl_sv_peek
	movq	%rax, %rcx
	movabsq	$.L.str373, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	jmp	.LBB22_24
.LBB22_14:                              # %if.else
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str374, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	jmp	.LBB22_25
.LBB22_12:                              # %cond.false20
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rdx
.LBB22_13:                              # %cond.end22
	movabsq	$.L.str373, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	PerlIO_printf
	movq	%rbx, %rdi
	callq	Perl_sv_free
	jmp	.LBB22_25
.LBB22_21:                              # %cond.false48
	movq	%r15, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rdx
.LBB22_22:                              # %cond.end50
	movabsq	$.L.str373, %rsi
.LBB22_23:                              # %sw.epilog
	xorq	%rax, %rax
	movq	%r14, %rdi
.LBB22_24:                              # %sw.epilog
	callq	PerlIO_printf
.LBB22_25:                              # %sw.epilog
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str287, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
.LBB22_26:                              # %return
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp248:
	.size	Perl_debop, .Ltmp248-Perl_debop
	.cfi_endproc

	.globl	Perl_watch
	.align	16, 0x90
	.type	Perl_watch,@function
Perl_watch:                             # @Perl_watch
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp250:
	.cfi_def_cfa_offset 16
	movq	%rdi, PL_watchaddr(%rip)
	movq	(%rdi), %rax
	movq	%rax, PL_watchok(%rip)
	callq	Perl_PerlIO_stderr
	movq	PL_watchaddr(%rip), %rdx
	movq	PL_watchok(%rip), %rcx
	movq	%rax, %rdi
	movabsq	$.L.str376, %rsi
	xorq	%rax, %rax
	popq	%r8
	jmp	PerlIO_printf  # TAILCALL
.Ltmp251:
	.size	Perl_watch, .Ltmp251-Perl_watch
	.cfi_endproc

	.globl	Perl_debprofdump
	.align	16, 0x90
	.type	Perl_debprofdump,@function
Perl_debprofdump:                       # @Perl_debprofdump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp255:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp256:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp257:
	.cfi_def_cfa_offset 32
.Ltmp258:
	.cfi_offset %rbx, -24
.Ltmp259:
	.cfi_offset %r14, -16
	movq	PL_profiledata(%rip), %rax
	testq	%rax, %rax
	je	.LBB24_6
# BB#1:
	xorq	%rbx, %rbx
	movabsq	$.L.str377, %r14
	jmp	.LBB24_2
	.align	16, 0x90
.LBB24_5:                               # %for.inc.for.body_crit_edge
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	PL_profiledata(%rip), %rax
	incq	%rbx
.LBB24_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rax,%rbx,8)
	je	.LBB24_4
# BB#3:                                 # %if.then2
                                        #   in Loop: Header=BB24_2 Depth=1
	callq	Perl_PerlIO_stderr
	movq	PL_profiledata(%rip), %rcx
	movq	(%rcx,%rbx,8), %rdx
	movq	PL_op_name(,%rbx,8), %rcx
	movq	%rax, %rsi
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movq	%r14, %rsi
	callq	PerlIO_printf
.LBB24_4:                               # %for.inc
                                        #   in Loop: Header=BB24_2 Depth=1
	cmpq	$351, %rbx              # imm = 0x15F
	jne	.LBB24_5
.LBB24_6:                               # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp260:
	.size	Perl_debprofdump, .Ltmp260-Perl_debprofdump
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%*s"
	.size	.L.str, 4

	.type	.L.str1,@object         # @.str1
.L.str1:
	.zero	1
	.size	.L.str1, 1

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\nSUB %s = "
	.size	.L.str2, 11

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"(xsub 0x%lx %d)\n"
	.size	.L.str3, 17

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"<undef>\n"
	.size	.L.str4, 9

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"\nFORMAT %s = "
	.size	.L.str5, 14

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\""
	.size	.L.str6, 2

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"\\t"
	.size	.L.str7, 3

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\\n"
	.size	.L.str8, 3

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\\r"
	.size	.L.str9, 3

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"\\f"
	.size	.L.str10, 3

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"\\\""
	.size	.L.str11, 3

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"\\\\"
	.size	.L.str12, 3

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"\\%03o"
	.size	.L.str13, 6

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"\\%o"
	.size	.L.str14, 4

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"..."
	.size	.L.str15, 4

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"\\0"
	.size	.L.str16, 3

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"VOID"
	.size	.L.str17, 5

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"WILD"
	.size	.L.str18, 5

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"SV_UNDEF"
	.size	.L.str19, 9

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"SV_NO"
	.size	.L.str20, 6

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"SV_YES"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"SV_PLACEHOLDER"
	.size	.L.str22, 15

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	":"
	.size	.L.str23, 2

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"("
	.size	.L.str24, 2

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"<%lu%s>"
	.size	.L.str25, 8

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"T"
	.size	.L.str26, 2

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"<T>"
	.size	.L.str27, 4

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"\\"
	.size	.L.str28, 2

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"FREED"
	.size	.L.str29, 6

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"UNDEF"
	.size	.L.str30, 6

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"IV"
	.size	.L.str31, 3

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"NV"
	.size	.L.str32, 3

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"RV"
	.size	.L.str33, 3

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"PV"
	.size	.L.str34, 3

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"PVIV"
	.size	.L.str35, 5

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"PVNV"
	.size	.L.str36, 5

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"PVMG"
	.size	.L.str37, 5

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"PVLV"
	.size	.L.str38, 5

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"AV"
	.size	.L.str39, 3

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"HV"
	.size	.L.str40, 3

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"CV(%s)"
	.size	.L.str41, 7

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"CV()"
	.size	.L.str42, 5

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"GV"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"BM"
	.size	.L.str44, 3

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"FM"
	.size	.L.str45, 3

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"IO"
	.size	.L.str46, 3

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"(null)"
	.size	.L.str47, 7

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"[%s]"
	.size	.L.str48, 5

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"%s)"
	.size	.L.str49, 4

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	" [UTF8 \"%s\"]"
	.size	.L.str50, 13

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"(%g)"
	.size	.L.str51, 5

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"(%lu)"
	.size	.L.str52, 6

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"(%ld)"
	.size	.L.str53, 6

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"()"
	.size	.L.str54, 3

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	")"
	.size	.L.str55, 2

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"{}\n"
	.size	.L.str56, 4

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"{\n"
	.size	.L.str57, 3

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"PMf_PRE %c%s%c%s\n"
	.size	.L.str58, 18

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	" (RUNTIME)"
	.size	.L.str59, 11

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"PMf_PRE (RUNTIME)\n"
	.size	.L.str60, 19

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"PMf_REPL = "
	.size	.L.str61, 12

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	",USED"
	.size	.L.str62, 6

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	",TAINTED"
	.size	.L.str63, 9

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	",ONCE"
	.size	.L.str64, 6

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	",SCANFIRST"
	.size	.L.str65, 11

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	",ALL"
	.size	.L.str66, 5

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	",SKIPWHITE"
	.size	.L.str67, 11

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	",CONST"
	.size	.L.str68, 7

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	",KEEP"
	.size	.L.str69, 6

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	",GLOBAL"
	.size	.L.str70, 8

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	",CONTINUE"
	.size	.L.str71, 10

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	",RETAINT"
	.size	.L.str72, 9

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	",EVAL"
	.size	.L.str73, 6

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"PMFLAGS = (%s)\n"
	.size	.L.str74, 16

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"}\n"
	.size	.L.str75, 3

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"%-4d"
	.size	.L.str76, 5

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"    "
	.size	.L.str77, 5

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"%*sTYPE = %s  ===> "
	.size	.L.str78, 20

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"%d\n"
	.size	.L.str79, 4

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"(%d)\n"
	.size	.L.str80, 6

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"DONE\n"
	.size	.L.str81, 6

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"  (was %s)\n"
	.size	.L.str82, 12

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"LINE = %lu\n"
	.size	.L.str83, 12

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"PACKAGE = \"%s\"\n"
	.size	.L.str84, 16

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"LABEL = \"%s\"\n"
	.size	.L.str85, 14

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"TARG = %ld\n"
	.size	.L.str86, 12

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	",VOID"
	.size	.L.str87, 6

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	",SCALAR"
	.size	.L.str88, 8

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	",LIST"
	.size	.L.str89, 6

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	",UNKNOWN"
	.size	.L.str90, 9

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	",KIDS"
	.size	.L.str91, 6

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	",PARENS"
	.size	.L.str92, 8

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	",STACKED"
	.size	.L.str93, 9

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	",REF"
	.size	.L.str94, 5

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	",MOD"
	.size	.L.str95, 5

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	",SPECIAL"
	.size	.L.str96, 9

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"FLAGS = (%s)\n"
	.size	.L.str97, 14

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	",TARGET_MY"
	.size	.L.str98, 11

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	",REFCOUNTED"
	.size	.L.str99, 12

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	",COMMON"
	.size	.L.str100, 8

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	",HASH"
	.size	.L.str101, 6

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	",BACKWARDS"
	.size	.L.str102, 11

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	",SQUASH"
	.size	.L.str103, 8

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	",DELETE"
	.size	.L.str104, 8

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	",COMPLEMENT"
	.size	.L.str105, 12

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	",IDENTICAL"
	.size	.L.str106, 11

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	",GROWS"
	.size	.L.str107, 7

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	",DOLIST"
	.size	.L.str108, 8

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	",AMPER"
	.size	.L.str109, 7

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	",DB"
	.size	.L.str110, 4

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	",HASTARG"
	.size	.L.str111, 9

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	",NOPAREN"
	.size	.L.str112, 9

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	",INARGS"
	.size	.L.str113, 8

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	",NOMOD"
	.size	.L.str114, 7

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	",SV"
	.size	.L.str115, 4

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	",AV"
	.size	.L.str116, 4

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	",HV"
	.size	.L.str117, 4

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	",MAYBE_LVSUB"
	.size	.L.str118, 13

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	",LVAL_DEFER"
	.size	.L.str119, 12

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	",STRICT_REFS"
	.size	.L.str120, 13

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	",OUR_INTRO"
	.size	.L.str121, 11

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	",BARE"
	.size	.L.str122, 6

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	",STRICT"
	.size	.L.str123, 8

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	",ARYBASE"
	.size	.L.str124, 9

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	",WARNING"
	.size	.L.str125, 9

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	",ENTERED"
	.size	.L.str126, 9

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	",LINENUM"
	.size	.L.str127, 9

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	",INTRO"
	.size	.L.str128, 7

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	",EARLY_CV"
	.size	.L.str129, 10

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	",GUESSED"
	.size	.L.str130, 9

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	",SLICE"
	.size	.L.str131, 7

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	",EXISTS_SUB"
	.size	.L.str132, 12

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	",NUMERIC"
	.size	.L.str133, 9

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	",INTEGER"
	.size	.L.str134, 9

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	",REVERSE"
	.size	.L.str135, 9

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	",SVREF"
	.size	.L.str136, 7

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	",IN_RAW"
	.size	.L.str137, 8

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	",IN_CRLF"
	.size	.L.str138, 9

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	",OUT_RAW"
	.size	.L.str139, 9

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	",OUT_CRLF"
	.size	.L.str140, 10

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	",EXIT_VMSISH"
	.size	.L.str141, 13

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	",HUSH_VMSISH"
	.size	.L.str142, 13

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	",FT_ACCESS"
	.size	.L.str143, 11

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"PRIVATE = (%s)\n"
	.size	.L.str144, 16

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"SV = %s\n"
	.size	.L.str147, 9

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"REDO ===> "
	.size	.L.str148, 11

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"NEXT ===> "
	.size	.L.str149, 11

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"LAST ===> "
	.size	.L.str150, 11

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"OTHER ===> "
	.size	.L.str151, 12

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"REFCNT = %lu\n"
	.size	.L.str152, 14

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"GV_NAME = %s"
	.size	.L.str153, 13

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"-> %s"
	.size	.L.str154, 6

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"  MAGIC = 0x%lx\n"
	.size	.L.str155, 17

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"sv"
	.size	.L.str156, 3

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"env"
	.size	.L.str157, 4

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"envelem"
	.size	.L.str158, 8

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"sig"
	.size	.L.str159, 4

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"sigelem"
	.size	.L.str160, 8

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"pack"
	.size	.L.str161, 5

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"packelem"
	.size	.L.str162, 9

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"dbline"
	.size	.L.str163, 7

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"isa"
	.size	.L.str164, 4

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"arylen"
	.size	.L.str165, 7

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"glob"
	.size	.L.str166, 5

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"mglob"
	.size	.L.str167, 6

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"nkeys"
	.size	.L.str168, 6

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"taint"
	.size	.L.str169, 6

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"substr"
	.size	.L.str170, 7

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"vec"
	.size	.L.str171, 4

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"pos"
	.size	.L.str172, 4

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"bm"
	.size	.L.str173, 3

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"fm"
	.size	.L.str174, 3

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"uvar"
	.size	.L.str175, 5

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"defelem"
	.size	.L.str176, 8

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"amagic"
	.size	.L.str177, 7

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"amagicelem"
	.size	.L.str178, 11

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"backref"
	.size	.L.str179, 8

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"utf8"
	.size	.L.str180, 5

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"    MG_VIRTUAL = &PL_vtbl_%s\n"
	.size	.L.str181, 30

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"    MG_VIRTUAL = 0x%lx\n"
	.size	.L.str182, 24

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"    MG_VIRTUAL = 0\n"
	.size	.L.str183, 20

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"    MG_PRIVATE = %d\n"
	.size	.L.str184, 21

	.type	magic_names,@object     # @magic_names
	.section	.rodata,"a",@progbits
	.align	16
magic_names:
	.byte	0                       # 0x0
	.zero	7
	.quad	.L.str378
	.byte	35                      # 0x23
	.zero	7
	.quad	.L.str379
	.byte	42                      # 0x2a
	.zero	7
	.quad	.L.str380
	.byte	46                      # 0x2e
	.zero	7
	.quad	.L.str381
	.byte	60                      # 0x3c
	.zero	7
	.quad	.L.str382
	.byte	65                      # 0x41
	.zero	7
	.quad	.L.str383
	.byte	66                      # 0x42
	.zero	7
	.quad	.L.str384
	.byte	68                      # 0x44
	.zero	7
	.quad	.L.str385
	.byte	69                      # 0x45
	.zero	7
	.quad	.L.str386
	.byte	73                      # 0x49
	.zero	7
	.quad	.L.str387
	.byte	76                      # 0x4c
	.zero	7
	.quad	.L.str388
	.byte	78                      # 0x4e
	.zero	7
	.quad	.L.str389
	.byte	80                      # 0x50
	.zero	7
	.quad	.L.str390
	.byte	83                      # 0x53
	.zero	7
	.quad	.L.str391
	.byte	85                      # 0x55
	.zero	7
	.quad	.L.str392
	.byte	97                      # 0x61
	.zero	7
	.quad	.L.str393
	.byte	99                      # 0x63
	.zero	7
	.quad	.L.str394
	.byte	100                     # 0x64
	.zero	7
	.quad	.L.str395
	.byte	101                     # 0x65
	.zero	7
	.quad	.L.str396
	.byte	102                     # 0x66
	.zero	7
	.quad	.L.str397
	.byte	103                     # 0x67
	.zero	7
	.quad	.L.str398
	.byte	105                     # 0x69
	.zero	7
	.quad	.L.str399
	.byte	107                     # 0x6b
	.zero	7
	.quad	.L.str400
	.byte	108                     # 0x6c
	.zero	7
	.quad	.L.str401
	.byte	109                     # 0x6d
	.zero	7
	.quad	.L.str402
	.byte	110                     # 0x6e
	.zero	7
	.quad	.L.str403
	.byte	111                     # 0x6f
	.zero	7
	.quad	.L.str404
	.byte	112                     # 0x70
	.zero	7
	.quad	.L.str405
	.byte	113                     # 0x71
	.zero	7
	.quad	.L.str406
	.byte	114                     # 0x72
	.zero	7
	.quad	.L.str407
	.byte	115                     # 0x73
	.zero	7
	.quad	.L.str408
	.byte	116                     # 0x74
	.zero	7
	.quad	.L.str409
	.byte	117                     # 0x75
	.zero	7
	.quad	.L.str410
	.byte	118                     # 0x76
	.zero	7
	.quad	.L.str411
	.byte	86                      # 0x56
	.zero	7
	.quad	.L.str412
	.byte	119                     # 0x77
	.zero	7
	.quad	.L.str413
	.byte	120                     # 0x78
	.zero	7
	.quad	.L.str414
	.byte	121                     # 0x79
	.zero	7
	.quad	.L.str415
	.byte	126                     # 0x7e
	.zero	7
	.quad	.L.str416
	.zero	16
	.size	magic_names, 640

	.type	.L.str185,@object       # @.str185
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str185:
	.asciz	"    MG_TYPE = PERL_MAGIC_%s\n"
	.size	.L.str185, 29

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"    MG_TYPE = UNKNOWN(\\%o)\n"
	.size	.L.str186, 28

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"    MG_FLAGS = 0x%02X\n"
	.size	.L.str187, 23

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"      TAINTEDDIR\n"
	.size	.L.str188, 18

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"      REFCOUNTED\n"
	.size	.L.str189, 18

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"      GSKIP\n"
	.size	.L.str190, 13

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"      MINMATCH\n"
	.size	.L.str191, 16

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"    MG_OBJ = 0x%lx\n"
	.size	.L.str192, 20

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"    MG_LEN = %ld\n"
	.size	.L.str193, 18

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"    MG_PTR = 0x%lx"
	.size	.L.str194, 19

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	" %s"
	.size	.L.str195, 4

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	" => HEf_SVKEY\n"
	.size	.L.str196, 15

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	" ???? - please notify IZ"
	.size	.L.str197, 25

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"      %2ld: %lu -> %lu\n"
	.size	.L.str198, 24

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"%s = 0x%lx"
	.size	.L.str199, 11

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"\t\"%s\"\n"
	.size	.L.str200, 7

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"\t\""
	.size	.L.str201, 3

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"%s\" :: \""
	.size	.L.str202, 9

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"%s\"\n"
	.size	.L.str203, 5

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"SV = 0\n"
	.size	.L.str204, 8

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"(0x%lx) at 0x%lx\n%*s  REFCNT = %ld\n%*s  FLAGS = ("
	.size	.L.str205, 50

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"PADBUSY,"
	.size	.L.str206, 9

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"PADTMP,"
	.size	.L.str207, 8

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"PADMY,"
	.size	.L.str208, 7

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"TEMP,"
	.size	.L.str209, 6

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"OBJECT,"
	.size	.L.str210, 8

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"GMG,"
	.size	.L.str211, 5

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"SMG,"
	.size	.L.str212, 5

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"RMG,"
	.size	.L.str213, 5

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"IOK,"
	.size	.L.str214, 5

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"NOK,"
	.size	.L.str215, 5

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"POK,"
	.size	.L.str216, 5

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"ROK,"
	.size	.L.str217, 5

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"WEAKREF,"
	.size	.L.str218, 9

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"OOK,"
	.size	.L.str219, 5

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"FAKE,"
	.size	.L.str220, 6

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"READONLY,"
	.size	.L.str221, 10

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"OVERLOAD,"
	.size	.L.str222, 10

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"pIOK,"
	.size	.L.str223, 6

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"pNOK,"
	.size	.L.str224, 6

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"pPOK,"
	.size	.L.str225, 6

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"SCREAM,"
	.size	.L.str226, 8

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"ANON,"
	.size	.L.str227, 6

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"UNIQUE,"
	.size	.L.str228, 8

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"CLONE,"
	.size	.L.str229, 7

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"CLONED,"
	.size	.L.str230, 8

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"CONST,"
	.size	.L.str231, 7

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"NODEBUG,"
	.size	.L.str232, 9

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"COMPILED,"
	.size	.L.str233, 10

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"LVALUE,"
	.size	.L.str234, 8

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"METHOD,"
	.size	.L.str235, 8

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"LOCKED,"
	.size	.L.str236, 8

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"WEAKOUTSIDE,"
	.size	.L.str237, 13

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"SHAREKEYS,"
	.size	.L.str238, 11

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"LAZYDEL,"
	.size	.L.str239, 9

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"HASKFLAGS,"
	.size	.L.str240, 11

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"REHASH,"
	.size	.L.str241, 8

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"CLONEABLE,"
	.size	.L.str242, 11

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"INTRO,"
	.size	.L.str243, 7

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"MULTI,"
	.size	.L.str244, 7

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"ASSUMECV,"
	.size	.L.str245, 10

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"IN_PAD,"
	.size	.L.str246, 8

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"OUR,"
	.size	.L.str247, 5

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"IMPORT"
	.size	.L.str248, 7

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"ALL,"
	.size	.L.str249, 5

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	" SV"
	.size	.L.str250, 4

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	" AV"
	.size	.L.str251, 4

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	" HV"
	.size	.L.str252, 4

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	" CV"
	.size	.L.str253, 4

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	" ),"
	.size	.L.str254, 4

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"EVALED,"
	.size	.L.str255, 8

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"IsUV,"
	.size	.L.str256, 6

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"TAIL,"
	.size	.L.str257, 6

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"VALID,"
	.size	.L.str258, 7

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"TYPED,"
	.size	.L.str259, 7

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"UTF8"
	.size	.L.str260, 5

	.type	.L.str261,@object       # @.str261
.L.str261:
	.asciz	"SV = "
	.size	.L.str261, 6

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"NULL%s\n"
	.size	.L.str262, 8

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"IV%s\n"
	.size	.L.str263, 6

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"NV%s\n"
	.size	.L.str264, 6

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"RV%s\n"
	.size	.L.str265, 6

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"PV%s\n"
	.size	.L.str266, 6

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"PVIV%s\n"
	.size	.L.str267, 8

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"PVNV%s\n"
	.size	.L.str268, 8

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"PVBM%s\n"
	.size	.L.str269, 8

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"PVMG%s\n"
	.size	.L.str270, 8

	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"PVLV%s\n"
	.size	.L.str271, 8

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"PVAV%s\n"
	.size	.L.str272, 8

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"PVHV%s\n"
	.size	.L.str273, 8

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"PVCV%s\n"
	.size	.L.str274, 8

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"PVGV%s\n"
	.size	.L.str275, 8

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"PVFM%s\n"
	.size	.L.str276, 8

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"PVIO%s\n"
	.size	.L.str277, 8

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"UNKNOWN(0x%lx) %s\n"
	.size	.L.str278, 19

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"  UV = %lu"
	.size	.L.str279, 11

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"  IV = %ld"
	.size	.L.str280, 11

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"  (OFFSET)"
	.size	.L.str281, 11

	.type	.L.str282,@object       # @.str282
.L.str282:
	.asciz	"  NV = %.*g\n"
	.size	.L.str282, 13

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"  RV = 0x%lx\n"
	.size	.L.str283, 14

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"  PV = 0x%lx "
	.size	.L.str284, 14

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"( %s . ) "
	.size	.L.str285, 10

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"%s"
	.size	.L.str286, 3

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"\n"
	.size	.L.str287, 2

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"  CUR = %ld\n"
	.size	.L.str288, 13

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"  LEN = %ld\n"
	.size	.L.str289, 13

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"  PV = 0\n"
	.size	.L.str290, 10

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"  STASH"
	.size	.L.str291, 8

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"  TYPE = %c\n"
	.size	.L.str292, 13

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"  TARGOFF = %ld\n"
	.size	.L.str293, 17

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"  TARGLEN = %ld\n"
	.size	.L.str294, 17

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"  TARG = 0x%lx\n"
	.size	.L.str295, 16

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"  ARRAY = 0x%lx"
	.size	.L.str296, 16

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	" (offset=%ld)\n"
	.size	.L.str297, 15

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"  ALLOC = 0x%lx\n"
	.size	.L.str298, 17

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"  FILL = %ld\n"
	.size	.L.str299, 14

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"  MAX = %ld\n"
	.size	.L.str300, 13

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"  ARYLEN = 0x%lx\n"
	.size	.L.str301, 18

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	",REAL"
	.size	.L.str302, 6

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	",REIFY"
	.size	.L.str303, 7

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	",REUSED"
	.size	.L.str304, 8

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"  FLAGS = (%s)\n"
	.size	.L.str305, 16

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"Elt No. %ld\n"
	.size	.L.str306, 13

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"  ("
	.size	.L.str307, 4

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"%d%s:%d"
	.size	.L.str308, 8

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"+"
	.size	.L.str309, 2

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	", "
	.size	.L.str310, 3

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"  hash quality = %.1f%%"
	.size	.L.str311, 24

	.type	.L.str312,@object       # @.str312
.L.str312:
	.asciz	"  KEYS = %ld\n"
	.size	.L.str312, 14

	.type	.L.str313,@object       # @.str313
.L.str313:
	.asciz	"  RITER = %ld\n"
	.size	.L.str313, 15

	.type	.L.str314,@object       # @.str314
.L.str314:
	.asciz	"  EITER = 0x%lx\n"
	.size	.L.str314, 17

	.type	.L.str315,@object       # @.str315
.L.str315:
	.asciz	"  PMROOT = 0x%lx\n"
	.size	.L.str315, 18

	.type	.L.str316,@object       # @.str316
.L.str316:
	.asciz	"  NAME = \"%s\"\n"
	.size	.L.str316, 15

	.type	.L.str317,@object       # @.str317
.L.str317:
	.asciz	"Elt %s "
	.size	.L.str317, 8

	.type	.L.str318,@object       # @.str318
.L.str318:
	.asciz	"[UTF8 \"%s\"] "
	.size	.L.str318, 13

	.type	.L.str319,@object       # @.str319
.L.str319:
	.asciz	"[REHASH] "
	.size	.L.str319, 10

	.type	.L.str320,@object       # @.str320
.L.str320:
	.asciz	"HASH = 0x%lx\n"
	.size	.L.str320, 14

	.type	.L.str321,@object       # @.str321
.L.str321:
	.asciz	"  PROTOTYPE = \"%s\"\n"
	.size	.L.str321, 20

	.type	.L.str322,@object       # @.str322
.L.str322:
	.asciz	"  COMP_STASH"
	.size	.L.str322, 13

	.type	.L.str323,@object       # @.str323
.L.str323:
	.asciz	"  START = 0x%lx ===> %ld\n"
	.size	.L.str323, 26

	.type	.L.str324,@object       # @.str324
.L.str324:
	.asciz	"  ROOT = 0x%lx\n"
	.size	.L.str324, 16

	.type	.L.str325,@object       # @.str325
.L.str325:
	.asciz	"  XSUB = 0x%lx\n"
	.size	.L.str325, 16

	.type	.L.str326,@object       # @.str326
.L.str326:
	.asciz	"  XSUBANY = %ld\n"
	.size	.L.str326, 17

	.type	.L.str327,@object       # @.str327
.L.str327:
	.asciz	"  GVGV::GV"
	.size	.L.str327, 11

	.type	.L.str328,@object       # @.str328
.L.str328:
	.asciz	"  FILE = \"%s\"\n"
	.size	.L.str328, 15

	.type	.L.str329,@object       # @.str329
.L.str329:
	.asciz	"  DEPTH = %ld\n"
	.size	.L.str329, 15

	.type	.L.str330,@object       # @.str330
.L.str330:
	.asciz	"  FLAGS = 0x%lx\n"
	.size	.L.str330, 17

	.type	.L.str331,@object       # @.str331
.L.str331:
	.asciz	"  OUTSIDE_SEQ = %lu\n"
	.size	.L.str331, 21

	.type	.L.str332,@object       # @.str332
.L.str332:
	.asciz	"  LINES = %ld\n"
	.size	.L.str332, 15

	.type	.L.str333,@object       # @.str333
.L.str333:
	.asciz	"  PADLIST = 0x%lx\n"
	.size	.L.str333, 19

	.type	.L.str334,@object       # @.str334
.L.str334:
	.asciz	"  OUTSIDE = 0x%lx (%s)\n"
	.size	.L.str334, 24

	.type	.L.str335,@object       # @.str335
.L.str335:
	.asciz	"null"
	.size	.L.str335, 5

	.type	.L.str336,@object       # @.str336
.L.str336:
	.asciz	"ANON"
	.size	.L.str336, 5

	.type	.L.str337,@object       # @.str337
.L.str337:
	.asciz	"MAIN"
	.size	.L.str337, 5

	.type	.L.str338,@object       # @.str338
.L.str338:
	.asciz	"UNIQUE"
	.size	.L.str338, 7

	.type	.L.str339,@object       # @.str339
.L.str339:
	.asciz	"UNDEFINED"
	.size	.L.str339, 10

	.type	.L.str340,@object       # @.str340
.L.str340:
	.asciz	"  NAMELEN = %ld\n"
	.size	.L.str340, 17

	.type	.L.str341,@object       # @.str341
.L.str341:
	.asciz	"  GvSTASH"
	.size	.L.str341, 10

	.type	.L.str342,@object       # @.str342
.L.str342:
	.asciz	"  GP = 0x%lx\n"
	.size	.L.str342, 14

	.type	.L.str343,@object       # @.str343
.L.str343:
	.asciz	"    SV = 0x%lx\n"
	.size	.L.str343, 16

	.type	.L.str344,@object       # @.str344
.L.str344:
	.asciz	"    REFCNT = %ld\n"
	.size	.L.str344, 18

	.type	.L.str345,@object       # @.str345
.L.str345:
	.asciz	"    IO = 0x%lx\n"
	.size	.L.str345, 16

	.type	.L.str346,@object       # @.str346
.L.str346:
	.asciz	"    FORM = 0x%lx  \n"
	.size	.L.str346, 20

	.type	.L.str347,@object       # @.str347
.L.str347:
	.asciz	"    AV = 0x%lx\n"
	.size	.L.str347, 16

	.type	.L.str348,@object       # @.str348
.L.str348:
	.asciz	"    HV = 0x%lx\n"
	.size	.L.str348, 16

	.type	.L.str349,@object       # @.str349
.L.str349:
	.asciz	"    CV = 0x%lx\n"
	.size	.L.str349, 16

	.type	.L.str350,@object       # @.str350
.L.str350:
	.asciz	"    CVGEN = 0x%lx\n"
	.size	.L.str350, 19

	.type	.L.str351,@object       # @.str351
.L.str351:
	.asciz	"    GPFLAGS = 0x%lx\n"
	.size	.L.str351, 21

	.type	.L.str352,@object       # @.str352
.L.str352:
	.asciz	"    LINE = %ld\n"
	.size	.L.str352, 16

	.type	.L.str353,@object       # @.str353
.L.str353:
	.asciz	"    FILE = \"%s\"\n"
	.size	.L.str353, 17

	.type	.L.str354,@object       # @.str354
.L.str354:
	.asciz	"    FLAGS = 0x%lx\n"
	.size	.L.str354, 19

	.type	.L.str355,@object       # @.str355
.L.str355:
	.asciz	"    EGV"
	.size	.L.str355, 8

	.type	.L.str356,@object       # @.str356
.L.str356:
	.asciz	"  IFP = 0x%lx\n"
	.size	.L.str356, 15

	.type	.L.str357,@object       # @.str357
.L.str357:
	.asciz	"  OFP = 0x%lx\n"
	.size	.L.str357, 15

	.type	.L.str358,@object       # @.str358
.L.str358:
	.asciz	"  DIRP = 0x%lx\n"
	.size	.L.str358, 16

	.type	.L.str359,@object       # @.str359
.L.str359:
	.asciz	"  PAGE = %ld\n"
	.size	.L.str359, 14

	.type	.L.str360,@object       # @.str360
.L.str360:
	.asciz	"  PAGE_LEN = %ld\n"
	.size	.L.str360, 18

	.type	.L.str361,@object       # @.str361
.L.str361:
	.asciz	"  LINES_LEFT = %ld\n"
	.size	.L.str361, 20

	.type	.L.str362,@object       # @.str362
.L.str362:
	.asciz	"  TOP_NAME = \"%s\"\n"
	.size	.L.str362, 19

	.type	.L.str363,@object       # @.str363
.L.str363:
	.asciz	"  TOP_GV"
	.size	.L.str363, 9

	.type	.L.str364,@object       # @.str364
.L.str364:
	.asciz	"  FMT_NAME = \"%s\"\n"
	.size	.L.str364, 19

	.type	.L.str365,@object       # @.str365
.L.str365:
	.asciz	"  FMT_GV"
	.size	.L.str365, 9

	.type	.L.str366,@object       # @.str366
.L.str366:
	.asciz	"  BOTTOM_NAME = \"%s\"\n"
	.size	.L.str366, 22

	.type	.L.str367,@object       # @.str367
.L.str367:
	.asciz	"  BOTTOM_GV"
	.size	.L.str367, 12

	.type	.L.str368,@object       # @.str368
.L.str368:
	.asciz	"  SUBPROCESS = %ld\n"
	.size	.L.str368, 20

	.type	.L.str369,@object       # @.str369
.L.str369:
	.asciz	"  TYPE = '%c'\n"
	.size	.L.str369, 15

	.type	.L.str370,@object       # @.str370
.L.str370:
	.asciz	"  TYPE = '\\%o'\n"
	.size	.L.str370, 16

	.type	.L.str371,@object       # @.str371
.L.str371:
	.asciz	"NULL OP IN RUN"
	.size	.L.str371, 15

	.type	.L.str372,@object       # @.str372
.L.str372:
	.asciz	"WARNING: %lx changed from %lx to %lx\n"
	.size	.L.str372, 38

	.type	.L.str373,@object       # @.str373
.L.str373:
	.asciz	"(%s)"
	.size	.L.str373, 5

	.type	.L.str374,@object       # @.str374
.L.str374:
	.asciz	"(NULL)"
	.size	.L.str374, 7

	.type	.L.str375,@object       # @.str375
.L.str375:
	.asciz	"[%lu]"
	.size	.L.str375, 6

	.type	.L.str376,@object       # @.str376
.L.str376:
	.asciz	"WATCHING, %lx is currently %lx\n"
	.size	.L.str376, 32

	.type	.L.str377,@object       # @.str377
.L.str377:
	.asciz	"%5lu %s\n"
	.size	.L.str377, 9

	.type	.L.str378,@object       # @.str378
.L.str378:
	.asciz	"sv(\\0)"
	.size	.L.str378, 7

	.type	.L.str379,@object       # @.str379
.L.str379:
	.asciz	"arylen(#)"
	.size	.L.str379, 10

	.type	.L.str380,@object       # @.str380
.L.str380:
	.asciz	"glob(*)"
	.size	.L.str380, 8

	.type	.L.str381,@object       # @.str381
.L.str381:
	.asciz	"pos(.)"
	.size	.L.str381, 7

	.type	.L.str382,@object       # @.str382
.L.str382:
	.asciz	"backref(<)"
	.size	.L.str382, 11

	.type	.L.str383,@object       # @.str383
.L.str383:
	.asciz	"overload(A)"
	.size	.L.str383, 12

	.type	.L.str384,@object       # @.str384
.L.str384:
	.asciz	"bm(B)"
	.size	.L.str384, 6

	.type	.L.str385,@object       # @.str385
.L.str385:
	.asciz	"regdata(D)"
	.size	.L.str385, 11

	.type	.L.str386,@object       # @.str386
.L.str386:
	.asciz	"env(E)"
	.size	.L.str386, 7

	.type	.L.str387,@object       # @.str387
.L.str387:
	.asciz	"isa(I)"
	.size	.L.str387, 7

	.type	.L.str388,@object       # @.str388
.L.str388:
	.asciz	"dbfile(L)"
	.size	.L.str388, 10

	.type	.L.str389,@object       # @.str389
.L.str389:
	.asciz	"shared(N)"
	.size	.L.str389, 10

	.type	.L.str390,@object       # @.str390
.L.str390:
	.asciz	"tied(P)"
	.size	.L.str390, 8

	.type	.L.str391,@object       # @.str391
.L.str391:
	.asciz	"sig(S)"
	.size	.L.str391, 7

	.type	.L.str392,@object       # @.str392
.L.str392:
	.asciz	"uvar(U)"
	.size	.L.str392, 8

	.type	.L.str393,@object       # @.str393
.L.str393:
	.asciz	"overload_elem(a)"
	.size	.L.str393, 17

	.type	.L.str394,@object       # @.str394
.L.str394:
	.asciz	"overload_table(c)"
	.size	.L.str394, 18

	.type	.L.str395,@object       # @.str395
.L.str395:
	.asciz	"regdatum(d)"
	.size	.L.str395, 12

	.type	.L.str396,@object       # @.str396
.L.str396:
	.asciz	"envelem(e)"
	.size	.L.str396, 11

	.type	.L.str397,@object       # @.str397
.L.str397:
	.asciz	"fm(f)"
	.size	.L.str397, 6

	.type	.L.str398,@object       # @.str398
.L.str398:
	.asciz	"regex_global(g)"
	.size	.L.str398, 16

	.type	.L.str399,@object       # @.str399
.L.str399:
	.asciz	"isaelem(i)"
	.size	.L.str399, 11

	.type	.L.str400,@object       # @.str400
.L.str400:
	.asciz	"nkeys(k)"
	.size	.L.str400, 9

	.type	.L.str401,@object       # @.str401
.L.str401:
	.asciz	"dbline(l)"
	.size	.L.str401, 10

	.type	.L.str402,@object       # @.str402
.L.str402:
	.asciz	"mutex(m)"
	.size	.L.str402, 9

	.type	.L.str403,@object       # @.str403
.L.str403:
	.asciz	"shared_scalar(n)"
	.size	.L.str403, 17

	.type	.L.str404,@object       # @.str404
.L.str404:
	.asciz	"collxfrm(o)"
	.size	.L.str404, 12

	.type	.L.str405,@object       # @.str405
.L.str405:
	.asciz	"tiedelem(p)"
	.size	.L.str405, 12

	.type	.L.str406,@object       # @.str406
.L.str406:
	.asciz	"tiedscalar(q)"
	.size	.L.str406, 14

	.type	.L.str407,@object       # @.str407
.L.str407:
	.asciz	"qr(r)"
	.size	.L.str407, 6

	.type	.L.str408,@object       # @.str408
.L.str408:
	.asciz	"sigelem(s)"
	.size	.L.str408, 11

	.type	.L.str409,@object       # @.str409
.L.str409:
	.asciz	"taint(t)"
	.size	.L.str409, 9

	.type	.L.str410,@object       # @.str410
.L.str410:
	.asciz	"uvar_elem(v)"
	.size	.L.str410, 13

	.type	.L.str411,@object       # @.str411
.L.str411:
	.asciz	"vec(v)"
	.size	.L.str411, 7

	.type	.L.str412,@object       # @.str412
.L.str412:
	.asciz	"v-string(V)"
	.size	.L.str412, 12

	.type	.L.str413,@object       # @.str413
.L.str413:
	.asciz	"utf8(w)"
	.size	.L.str413, 8

	.type	.L.str414,@object       # @.str414
.L.str414:
	.asciz	"substr(x)"
	.size	.L.str414, 10

	.type	.L.str415,@object       # @.str415
.L.str415:
	.asciz	"defelem(y)"
	.size	.L.str415, 11

	.type	.L.str416,@object       # @.str416
.L.str416:
	.asciz	"ext(~)"
	.size	.L.str416, 7


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
