	.file	"Peek.c.bc"
	.text
	.globl	_runops_debug
	.align	16, 0x90
	.type	_runops_debug,@function
_runops_debug:                          # @_runops_debug
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	PL_runops(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movabsq	$Perl_runops_debug, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB0_3
.LBB0_1:
	movabsq	$0, %rax
.LBB0_3:                                # %entry
	testq	%rdx, %rdx
	js	.LBB0_7
# BB#4:                                 # %if.then
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rdi
	jne	.LBB0_6
# BB#5:                                 # %if.then
	movabsq	$Perl_runops_standard, %rcx
.LBB0_6:                                # %if.then
	movq	%rcx, PL_runops(%rip)
.LBB0_7:                                # %if.end
	retq
.Ltmp0:
	.size	_runops_debug, .Ltmp0-_runops_debug
	.cfi_endproc

	.globl	DeadCode
	.align	16, 0x90
	.type	DeadCode,@function
DeadCode:                               # @DeadCode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 240
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	callq	Perl_newAV
	movq	%rax, %rdi
	callq	Perl_newRV_noinc
	movq	PL_sv_arenaroot(%rip), %rcx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	xorq	%r12, %r12
	testq	%rcx, %rcx
	je	.LBB1_1
# BB#2:
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_5 Depth 2
                                        #       Child Loop BB1_14 Depth 3
                                        #         Child Loop BB1_15 Depth 4
                                        #         Child Loop BB1_24 Depth 4
                                        #         Child Loop BB1_30 Depth 4
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	movq	8(%rcx), %rax
	cmpq	$2, %rax
	jl	.LBB1_59
# BB#4:                                 # %for.body3.lr.ph.lr.ph
                                        #   in Loop: Header=BB1_3 Depth=1
	leaq	(%rax,%rax,2), %rax
	leaq	(%rcx,%rax,8), %r15
	leaq	24(%rcx), %rax
	movq	%rcx, %r13
	.align	16, 0x90
.LBB1_5:                                # %for.body3
                                        #   Parent Loop BB1_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_14 Depth 3
                                        #         Child Loop BB1_15 Depth 4
                                        #         Child Loop BB1_24 Depth 4
                                        #         Child Loop BB1_30 Depth 4
	movzbl	40(%r13), %ecx
	movq	%rax, %r13
	cmpq	$12, %rcx
	jne	.LBB1_9
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	(%r13), %rax
	cmpq	$0, 80(%rax)
	jne	.LBB1_9
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB1_5 Depth=2
	cmpq	$0, 96(%rax)
	je	.LBB1_9
# BB#8:                                 # %if.end11
                                        #   in Loop: Header=BB1_5 Depth=2
	cmpq	$0, 72(%rax)
	je	.LBB1_9
# BB#11:                                # %if.end15
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	120(%rax), %rbx
	callq	Perl_PerlIO_stderr
	movq	(%r13), %rcx
	movq	96(%rcx), %rcx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movabsq	$.L.str, %rdx
	callq	Perl_do_gvgv_dump
	movq	(%r13), %rax
	cmpq	$0, 112(%rax)
	je	.LBB1_13
# BB#12:                                # %if.then21
                                        #   in Loop: Header=BB1_5 Depth=2
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str1, %rsi
	callq	PerlIO_printf
	.align	16, 0x90
.LBB1_9:                                # %for.cond2.backedge
                                        #   in Loop: Header=BB1_5 Depth=2
	leaq	24(%r13), %rax
	cmpq	%r15, %rax
	jb	.LBB1_5
	jmp	.LBB1_10
.LBB1_13:                               # %if.end24
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	xorq	%r14, %r14
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	movq	$0, 136(%rsp)           # 8-byte Folded Spill
	movq	%rbx, 144(%rsp)         # 8-byte Spill
	jmp	.LBB1_14
	.align	16, 0x90
.LBB1_52:                               # %while.cond.outer.backedge
                                        #   in Loop: Header=BB1_14 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	addq	%rax, 64(%rsp)          # 8-byte Folded Spill
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	%rax, 72(%rsp)          # 8-byte Folded Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	addq	%rax, 80(%rsp)          # 8-byte Folded Spill
	movq	128(%rsp), %rax         # 8-byte Reload
	addq	%rax, 56(%rsp)          # 8-byte Folded Spill
	movq	$32, %rcx
	movq	160(%rsp), %rax         # 8-byte Reload
	sarq	%cl, %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, 48(%rsp)          # 8-byte Folded Spill
	movq	%r14, 136(%rsp)         # 8-byte Spill
	movq	%rax, %r14
.LBB1_14:                               # %while.cond.outer
                                        #   Parent Loop BB1_3 Depth=1
                                        #     Parent Loop BB1_5 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB1_15 Depth 4
                                        #         Child Loop BB1_24 Depth 4
                                        #         Child Loop BB1_30 Depth 4
	movq	%r14, %rcx
	movabsq	$4294967296, %rax       # imm = 0x100000000
	imulq	%rax, %rcx
	addq	%rax, %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	jmp	.LBB1_15
	.align	16, 0x90
.LBB1_21:                               # %if.then40
                                        #   in Loop: Header=BB1_15 Depth=4
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str2, %rsi
	callq	PerlIO_printf
	movabsq	$4294967296, %rax       # imm = 0x100000000
	addq	%rax, 160(%rsp)         # 8-byte Folded Spill
	movq	%r12, %r14
	movq	144(%rsp), %rbx         # 8-byte Reload
.LBB1_15:                               # %while.cond
                                        #   Parent Loop BB1_3 Depth=1
                                        #     Parent Loop BB1_5 Depth=2
                                        #       Parent Loop BB1_14 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	17(%rbx), %eax
	leaq	1(%r14), %r12
	testq	$128, %rax
	jne	.LBB1_16
# BB#17:                                # %cond.false
                                        #   in Loop: Header=BB1_15 Depth=4
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	jmp	.LBB1_18
	.align	16, 0x90
.LBB1_16:                               # %cond.true
                                        #   in Loop: Header=BB1_15 Depth=4
	movq	%rbx, %rdi
	callq	Perl_mg_size
.LBB1_18:                               # %cond.end
                                        #   in Loop: Header=BB1_15 Depth=4
	cmpq	%rax, %r14
	jge	.LBB1_53
# BB#19:                                # %while.body
                                        #   in Loop: Header=BB1_15 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	8(%rax,%r14,8), %rax
	movq	%r14, 152(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movq	(%rax), %rcx
	movq	(%rcx), %r14
	testq	%r14, %r14
	je	.LBB1_21
# BB#20:                                # %while.body
                                        #   in Loop: Header=BB1_15 Depth=4
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %r14
	je	.LBB1_21
# BB#22:                                # %if.end43
                                        #   in Loop: Header=BB1_14 Depth=3
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	(%r14), %rax
	movq	16(%rax), %rdx
	movzbl	72(%rax), %ecx
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	testq	$1, %rcx
	movabsq	$0, %r12
	movabsq	$0, %rcx
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	je	.LBB1_29
# BB#23:                                # %for.cond53.preheader
                                        #   in Loop: Header=BB1_14 Depth=3
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	(%rax), %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	xorq	%r12, %r12
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_37:                               # %for.inc
                                        #   in Loop: Header=BB1_24 Depth=4
	incq	%rbx
.LBB1_24:                               # %for.cond53
                                        #   Parent Loop BB1_3 Depth=1
                                        #     Parent Loop BB1_5 Depth=2
                                        #       Parent Loop BB1_14 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	17(%r14), %eax
	testq	$128, %rax
	jne	.LBB1_25
# BB#26:                                # %cond.false60
                                        #   in Loop: Header=BB1_24 Depth=4
	movq	(%r14), %rax
	movq	8(%rax), %rax
	jmp	.LBB1_27
	.align	16, 0x90
.LBB1_25:                               # %cond.true58
                                        #   in Loop: Header=BB1_24 Depth=4
	movq	%r14, %rdi
	callq	Perl_mg_size
.LBB1_27:                               # %cond.end63
                                        #   in Loop: Header=BB1_24 Depth=4
	cmpq	%rax, %rbx
	jge	.LBB1_28
# BB#32:                                # %for.body67
                                        #   in Loop: Header=BB1_24 Depth=4
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%rbx,8), %rcx
	movq	16(%rcx), %rax
	testq	$524288, %rax           # imm = 0x80000
	jne	.LBB1_33
# BB#34:                                # %if.else
                                        #   in Loop: Header=BB1_24 Depth=4
	andq	$252, %rax
	cmpq	$4, %rax
	jb	.LBB1_37
# BB#35:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_24 Depth=4
	movq	(%rcx), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_37
# BB#36:                                # %if.then87
                                        #   in Loop: Header=BB1_24 Depth=4
	xorq	%rdx, %rdx
	divq	8(%rcx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	120(%rsp), %rdx         # 8-byte Reload
	andq	%rcx, %rdx
	addq	%rax, %rdx
	movq	%rdx, 120(%rsp)         # 8-byte Spill
	jmp	.LBB1_37
	.align	16, 0x90
.LBB1_33:                               # %if.then73
                                        #   in Loop: Header=BB1_24 Depth=4
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movabsq	$.L.str3, %rsi
	callq	PerlIO_printf
	incq	%r12
	incq	%rbx
	jmp	.LBB1_24
	.align	16, 0x90
.LBB1_28:                               #   in Loop: Header=BB1_14 Depth=3
	movq	32(%rsp), %rdx          # 8-byte Reload
.LBB1_29:                               # %if.end101
                                        #   in Loop: Header=BB1_14 Depth=3
	incq	152(%rsp)               # 8-byte Folded Spill
	leaq	8(,%rdx,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$1, %r14
	movq	$0, 128(%rsp)           # 8-byte Folded Spill
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_42:                               # %for.inc178
                                        #   in Loop: Header=BB1_30 Depth=4
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	movq	$4, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rax, %rsi
	callq	Perl_do_sv_dump
	movq	$1, 136(%rsp)           # 8-byte Folded Spill
	incq	%r14
.LBB1_30:                               # %for.cond102
                                        #   Parent Loop BB1_3 Depth=1
                                        #     Parent Loop BB1_5 Depth=2
                                        #       Parent Loop BB1_14 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdi
	movzbl	17(%rdi), %eax
	testq	$128, %rax
	jne	.LBB1_31
# BB#38:                                # %cond.false111
                                        #   in Loop: Header=BB1_30 Depth=4
	movq	(%rdi), %rax
	movq	8(%rax), %rax
	jmp	.LBB1_39
	.align	16, 0x90
.LBB1_31:                               # %cond.true108
                                        #   in Loop: Header=BB1_30 Depth=4
	callq	Perl_mg_size
.LBB1_39:                               # %cond.end115
                                        #   in Loop: Header=BB1_30 Depth=4
	cmpq	%rax, %r14
	jge	.LBB1_50
# BB#40:                                # %for.body119
                                        #   in Loop: Header=BB1_30 Depth=4
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	(%rbx,%r14,8), %rcx
	movq	16(%rcx), %rax
	testq	$524288, %rax           # imm = 0x80000
	jne	.LBB1_41
# BB#43:                                # %if.else130
                                        #   in Loop: Header=BB1_30 Depth=4
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$10, %rdx
	jb	.LBB1_46
# BB#44:                                # %if.then137
                                        #   in Loop: Header=BB1_30 Depth=4
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB1_49
# BB#45:                                # %if.then143
                                        #   in Loop: Header=BB1_30 Depth=4
	incq	%r12
	callq	Perl_PerlIO_stderr
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%r14,8), %rdx
	jmp	.LBB1_42
	.align	16, 0x90
.LBB1_41:                               # %if.then125
                                        #   in Loop: Header=BB1_30 Depth=4
	incq	%r12
	callq	Perl_PerlIO_stderr
	movq	(%rbx,%r14,8), %rdx
	jmp	.LBB1_42
	.align	16, 0x90
.LBB1_46:                               # %if.else149
                                        #   in Loop: Header=BB1_30 Depth=4
	cmpq	$4, %rdx
	jb	.LBB1_49
# BB#47:                                # %land.lhs.true156
                                        #   in Loop: Header=BB1_30 Depth=4
	movq	(%rcx), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	je	.LBB1_49
# BB#48:                                # %if.then162
                                        #   in Loop: Header=BB1_30 Depth=4
	xorq	%rdx, %rdx
	divq	8(%rcx)
	incq	128(%rsp)               # 8-byte Folded Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	112(%rsp), %rdx         # 8-byte Reload
	andq	%rcx, %rdx
	addq	%rax, %rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB1_49:                               # %for.inc178
                                        #   in Loop: Header=BB1_30 Depth=4
	incq	%r14
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_50:                               # %for.end180
                                        #   in Loop: Header=BB1_14 Depth=3
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%rcx, %rdi
	movabsq	$.L.str4, %rsi
	movq	152(%rsp), %rdx         # 8-byte Reload
	movq	%r12, %rcx
	movq	%r12, 168(%rsp)         # 8-byte Spill
	movq	112(%rsp), %r8          # 8-byte Reload
	movq	128(%rsp), %r9          # 8-byte Reload
	callq	PerlIO_printf
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	136(%rsp), %rcx         # 8-byte Reload
	testq	%rax, %rcx
	movq	%rcx, %r12
	movq	144(%rsp), %rbx         # 8-byte Reload
	je	.LBB1_52
# BB#51:                                # %if.then189
                                        #   in Loop: Header=BB1_14 Depth=3
	callq	Perl_PerlIO_stderr
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	movq	$2, %r8
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%rax, %rsi
	movq	%r13, %rdx
	callq	Perl_do_sv_dump
	movq	%r12, %r14
	jmp	.LBB1_52
.LBB1_53:                               # %while.end
                                        #   in Loop: Header=BB1_5 Depth=2
	movzbl	17(%rbx), %eax
	testq	$128, %rax
	jne	.LBB1_54
# BB#55:                                # %cond.false197
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	(%rbx), %rax
	movq	8(%rax), %rax
	jmp	.LBB1_56
.LBB1_54:                               # %cond.true195
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	%rbx, %rdi
	callq	Perl_mg_size
.LBB1_56:                               # %cond.end200
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
	cmpq	$2, %rax
	jl	.LBB1_58
# BB#57:                                # %if.then204
                                        #   in Loop: Header=BB1_5 Depth=2
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, (%rsp)
	movq	%rcx, %rdi
	movabsq	$.L.str5, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	72(%rsp), %r9           # 8-byte Reload
	callq	PerlIO_printf
.LBB1_58:                               # %if.end207
                                        #   in Loop: Header=BB1_5 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r14
	movq	56(%rsp), %rax          # 8-byte Reload
	addq	%rax, 104(%rsp)         # 8-byte Folded Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	addq	%rax, 96(%rsp)          # 8-byte Folded Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	addq	%rax, 88(%rsp)          # 8-byte Folded Spill
	leaq	24(%r13), %rax
	cmpq	%r15, %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	jb	.LBB1_5
	jmp	.LBB1_59
	.align	16, 0x90
.LBB1_10:                               #   in Loop: Header=BB1_3 Depth=1
	movq	176(%rsp), %rcx         # 8-byte Reload
.LBB1_59:                               # %for.inc216
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_3
	jmp	.LBB1_60
.LBB1_1:
	movq	$0, 104(%rsp)           # 8-byte Folded Spill
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
.LBB1_60:                               # %for.end218
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, (%rsp)
	movq	%rcx, %rdi
	movq	%r12, %rdx
	movq	%r14, %rcx
	movq	104(%rsp), %r8          # 8-byte Reload
	movq	96(%rsp), %r9           # 8-byte Reload
	callq	PerlIO_printf
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp18:
	.size	DeadCode, .Ltmp18-DeadCode
	.cfi_endproc

	.globl	fill_mstats
	.align	16, 0x90
	.type	fill_mstats,@function
fill_mstats:                            # @fill_mstats
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	jmp	Perl_croak  # TAILCALL
.Ltmp19:
	.size	fill_mstats, .Ltmp19-fill_mstats
	.cfi_endproc

	.globl	mstats_fillhash
	.align	16, 0x90
	.type	mstats_fillhash,@function
mstats_fillhash:                        # @mstats_fillhash
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	jmp	Perl_croak  # TAILCALL
.Ltmp20:
	.size	mstats_fillhash, .Ltmp20-mstats_fillhash
	.cfi_endproc

	.globl	mstats2hash
	.align	16, 0x90
	.type	mstats2hash,@function
mstats2hash:                            # @mstats2hash
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	jmp	Perl_croak  # TAILCALL
.Ltmp21:
	.size	mstats2hash, .Ltmp21-mstats2hash
	.cfi_endproc

	.globl	XS_Devel__Peek_mstat
	.align	16, 0x90
	.type	XS_Devel__Peek_mstat,@function
XS_Devel__Peek_mstat:                   # @XS_Devel__Peek_mstat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp26:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 32
.Ltmp29:
	.cfi_offset %rbx, -32
.Ltmp30:
	.cfi_offset %r14, -24
.Ltmp31:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	subq	%rax, %rbx
	cmpq	$9, %rbx
	jb	.LBB5_2
# BB#1:                                 # %if.then
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB5_2:                                # %if.end
	cmpq	$8, %rbx
	jge	.LBB5_4
# BB#3:
	movabsq	$.L.str9, %r14
	jmp	.LBB5_7
.LBB5_4:                                # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r15,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB5_6
# BB#5:                                 # %cond.true
	movq	(%rdi), %rax
	movq	(%rax), %r14
	jmp	.LBB5_7
.LBB5_6:                                # %cond.false
	callq	Perl_sv_2pv_nolen
	movq	%rax, %r14
.LBB5_7:                                # %if.end14
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r14, %rdx
	callq	PerlIO_printf
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp32:
	.size	XS_Devel__Peek_mstat, .Ltmp32-XS_Devel__Peek_mstat
	.cfi_endproc

	.globl	XS_Devel__Peek_fill_mstats
	.align	16, 0x90
	.type	XS_Devel__Peek_fill_mstats,@function
XS_Devel__Peek_fill_mstats:             # @XS_Devel__Peek_fill_mstats
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp38:
	.cfi_def_cfa_offset 32
.Ltmp39:
	.cfi_offset %rbx, -24
.Ltmp40:
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
	movq	%rdx, %r14
	subq	%rax, %r14
	negq	%rax
	leaq	-8(%rdx,%rax), %rax
	cmpq	$9, %rax
	jb	.LBB6_2
# BB#1:                                 # %if.then
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB6_2:                                # %if.end
	cmpq	$16, %r14
	jl	.LBB6_5
# BB#3:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	jne	.LBB6_5
# BB#4:                                 # %cond.false
	callq	Perl_sv_2iv
.LBB6_5:                                # %if.end15
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp41:
	.size	XS_Devel__Peek_fill_mstats, .Ltmp41-XS_Devel__Peek_fill_mstats
	.cfi_endproc

	.globl	XS_Devel__Peek_mstats_fillhash
	.align	16, 0x90
	.type	XS_Devel__Peek_mstats_fillhash,@function
XS_Devel__Peek_mstats_fillhash:         # @XS_Devel__Peek_mstats_fillhash
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
	movq	%rdx, %r14
	subq	%rax, %r14
	negq	%rax
	leaq	-8(%rdx,%rax), %rax
	cmpq	$9, %rax
	jb	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB7_2:                                # %if.end
	cmpq	$16, %r14
	jl	.LBB7_5
# BB#3:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	16(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	jne	.LBB7_5
# BB#4:                                 # %cond.false
	callq	Perl_sv_2iv
.LBB7_5:                                # %if.end15
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp50:
	.size	XS_Devel__Peek_mstats_fillhash, .Ltmp50-XS_Devel__Peek_mstats_fillhash
	.cfi_endproc

	.globl	XS_Devel__Peek_mstats2hash
	.align	16, 0x90
	.type	XS_Devel__Peek_mstats2hash,@function
XS_Devel__Peek_mstats2hash:             # @XS_Devel__Peek_mstats2hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp56:
	.cfi_def_cfa_offset 32
.Ltmp57:
	.cfi_offset %rbx, -24
.Ltmp58:
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
	movq	%rdx, %r14
	subq	%rax, %r14
	negq	%rax
	leaq	-16(%rdx,%rax), %rax
	cmpq	$9, %rax
	jb	.LBB8_2
# BB#1:                                 # %if.then
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB8_2:                                # %if.end
	cmpq	$24, %r14
	jl	.LBB8_5
# BB#3:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	24(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	jne	.LBB8_5
# BB#4:                                 # %cond.false
	callq	Perl_sv_2iv
.LBB8_5:                                # %if.end17
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp59:
	.size	XS_Devel__Peek_mstats2hash, .Ltmp59-XS_Devel__Peek_mstats2hash
	.cfi_endproc

	.globl	XS_Devel__Peek_Dump
	.align	16, 0x90
	.type	XS_Devel__Peek_Dump,@function
XS_Devel__Peek_Dump:                    # @XS_Devel__Peek_Dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp66:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp67:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp68:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp69:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp71:
	.cfi_def_cfa_offset 80
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
	movq	PL_stack_base(%rip), %rsi
	leaq	(%rsi,%rbx,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r15, %r14
	subq	%rdx, %r14
	negq	%rdx
	movq	$3, %rcx
	movq	%r14, %r12
	sarq	%cl, %r12
	leaq	-8(%r15,%rdx), %rcx
	cmpq	$9, %rcx
	jb	.LBB9_2
# BB#1:                                 # %if.then
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rsi
.LBB9_2:                                # %if.end
	movq	$3, %rcx
	movq	8(%rsi,%rbx,8), %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	shlq	%cl, %r12
	movq	$4, %rax
	cmpq	$16, %r14
	jl	.LBB9_6
# BB#3:                                 # %if.else
	movq	16(%rsi,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB9_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB9_6
.LBB9_5:                                # %cond.false
	callq	Perl_sv_2iv
.LBB9_6:                                # %if.end16
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movabsq	$.L.str15, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	Perl_get_sv
	testq	%rax, %rax
	movabsq	$0, %r13
	je	.LBB9_10
# BB#7:                                 # %cond.true19
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB9_9
# BB#8:                                 # %cond.true23
	movq	(%rax), %rax
	movq	24(%rax), %r13
	jmp	.LBB9_10
.LBB9_9:                                # %cond.false26
	movq	%rax, %rdi
	callq	Perl_sv_2iv
	movq	%rax, %r13
.LBB9_10:                               # %cond.end31
	movabsq	$.L.str16, %rdi
	xorq	%rsi, %rsi
	callq	Perl_get_sv
	movq	PL_dumpindent(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r14
	movq	$2, PL_dumpindent(%rip)
	callq	Perl_PerlIO_stderr
	movq	%rax, %rsi
	testq	%r14, %r14
	je	.LBB9_31
# BB#11:                                # %cond.false38
	movq	16(%r14), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB9_18
# BB#12:                                # %cond.true42
	movq	(%r14), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB9_28
# BB#13:                                # %land.rhs45
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB9_28
# BB#14:                                # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB9_28
# BB#15:                                # %land.rhs49
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB9_16
# BB#17:                                # %land.rhs49
	movabsq	$1, %rax
	jmp	.LBB9_28
.LBB9_18:                               # %cond.false54
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB9_22
# BB#19:                                # %cond.true58
	movq	(%r14), %rax
	cmpq	$0, 24(%rax)
	je	.LBB9_20
# BB#21:                                # %cond.true58
	movabsq	$1, %rax
	jmp	.LBB9_28
.LBB9_22:                               # %cond.false63
	movq	%rsi, %rbx
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB9_26
# BB#23:                                # %cond.true67
	movq	(%r14), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB9_24
# BB#25:                                # %cond.true67
	movabsq	$1, %rax
	jmp	.LBB9_27
.LBB9_20:
	movabsq	$0, %rax
	jmp	.LBB9_28
.LBB9_16:
	movabsq	$0, %rax
	jmp	.LBB9_28
.LBB9_26:                               # %cond.false71
	movq	%r14, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB9_27
.LBB9_24:
	movabsq	$0, %rax
.LBB9_27:                               # %cond.end80
	movq	%rbx, %rsi
.LBB9_28:                               # %cond.end80
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_29
# BB#30:                                # %cond.end80
	movabsq	$1, %rbx
	jmp	.LBB9_31
.LBB9_29:
	movabsq	$0, %rbx
.LBB9_31:                               # %land.end83
	subq	%r12, %r15
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	movq	%r13, (%rsp)
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r9
	callq	Perl_do_sv_dump
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, PL_dumpindent(%rip)
	movq	%r15, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp77:
	.size	XS_Devel__Peek_Dump, .Ltmp77-XS_Devel__Peek_Dump
	.cfi_endproc

	.globl	XS_Devel__Peek_DumpArray
	.align	16, 0x90
	.type	XS_Devel__Peek_DumpArray,@function
XS_Devel__Peek_DumpArray:               # @XS_Devel__Peek_DumpArray
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
	subq	$48, %rsp
.Ltmp89:
	.cfi_def_cfa_offset 96
.Ltmp90:
	.cfi_offset %rbx, -48
.Ltmp91:
	.cfi_offset %r12, -40
.Ltmp92:
	.cfi_offset %r13, -32
.Ltmp93:
	.cfi_offset %r14, -24
.Ltmp94:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_sp(%rip), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rdx
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	movq	%r12, %r15
	subq	%rdx, %r15
	movq	$3, %rcx
	movq	%r15, %rdx
	sarq	%cl, %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	cmpq	$7, %r15
	jg	.LBB10_2
# BB#1:                                 # %if.then
	movabsq	$.L.str17, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB10_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	movzbl	18(%rdi), %eax
	movq	$3, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	subq	%rdx, %r12
	testq	$1, %rax
	je	.LBB10_4
# BB#3:                                 # %cond.true
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB10_5
.LBB10_4:                               # %cond.false
	movq	%r12, 16(%rsp)          # 8-byte Spill
	callq	Perl_sv_2iv
.LBB10_5:                               # %cond.end
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$.L.str15, %rdi
	xorq	%rsi, %rsi
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	callq	Perl_get_sv
	testq	%rax, %rax
	je	.LBB10_10
# BB#6:                                 # %cond.true13
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB10_8
# BB#7:                                 # %cond.true17
	movq	(%rax), %rax
	movq	24(%rax), %rax
	jmp	.LBB10_9
.LBB10_8:                               # %cond.false20
	movq	%rax, %rdi
	callq	Perl_sv_2iv
.LBB10_9:                               # %cond.end25
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB10_10:                              # %cond.end25
	movabsq	$.L.str16, %rdi
	xorq	%rsi, %rsi
	callq	Perl_get_sv
	movq	PL_dumpindent(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r13
	movq	$2, PL_dumpindent(%rip)
	cmpq	$9, %r15
	jl	.LBB10_30
# BB#11:                                # %for.body.lr.ph
	leaq	16(,%r14,8), %r15
	xorq	%rbx, %rbx
	testq	%r13, %r13
	je	.LBB10_18
	.align	16, 0x90
.LBB10_12:                              # %cond.false39
                                        # =>This Inner Loop Header: Depth=1
	callq	Perl_PerlIO_stderr
	movq	PL_stack_base(%rip), %rcx
	addq	%r15, %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	%rax, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movabsq	$.L.str18, %rsi
	movq	%rbx, %rdx
	callq	PerlIO_printf
	callq	Perl_PerlIO_stderr
	movq	PL_stack_base(%rip), %rdx
	addq	%r15, %rdx
	movq	16(%r13), %rcx
	movq	(%rdx,%rbx,8), %r12
	movq	%rax, %r14
	testq	$262144, %rcx           # imm = 0x40000
	jne	.LBB10_13
# BB#20:                                # %cond.false55
                                        #   in Loop: Header=BB10_12 Depth=1
	testq	$65536, %rcx            # imm = 0x10000
	jne	.LBB10_21
# BB#23:                                # %cond.false64
                                        #   in Loop: Header=BB10_12 Depth=1
	testq	$131072, %rcx           # imm = 0x20000
	jne	.LBB10_24
# BB#26:                                # %cond.false72
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	%r13, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB10_27
	.align	16, 0x90
.LBB10_13:                              # %cond.true43
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	(%r13), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB10_27
# BB#14:                                # %land.rhs46
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB10_27
# BB#15:                                # %lor.rhs
                                        #   in Loop: Header=BB10_12 Depth=1
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB10_27
# BB#16:                                # %land.rhs50
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	movabsq	$0, %rax
	je	.LBB10_27
# BB#17:                                # %land.rhs50
                                        #   in Loop: Header=BB10_12 Depth=1
	movabsq	$1, %rax
	jmp	.LBB10_27
	.align	16, 0x90
.LBB10_21:                              # %cond.true59
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	(%r13), %rax
	cmpq	$0, 24(%rax)
	movabsq	$0, %rax
	je	.LBB10_27
# BB#22:                                # %cond.true59
                                        #   in Loop: Header=BB10_12 Depth=1
	movabsq	$1, %rax
	jmp	.LBB10_27
.LBB10_24:                              # %cond.true68
                                        #   in Loop: Header=BB10_12 Depth=1
	movq	(%r13), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB10_27
# BB#25:                                # %cond.true68
                                        #   in Loop: Header=BB10_12 Depth=1
	movabsq	$1, %rax
	.align	16, 0x90
.LBB10_27:                              # %land.end84
                                        #   in Loop: Header=BB10_12 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %r9
	je	.LBB10_29
# BB#28:                                # %land.end84
                                        #   in Loop: Header=BB10_12 Depth=1
	movabsq	$1, %r9
.LBB10_29:                              # %land.end84
                                        #   in Loop: Header=BB10_12 Depth=1
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	Perl_do_sv_dump
	leaq	1(%rbx), %rax
	addq	$2, %rbx
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rbx
	movq	%rax, %rbx
	jl	.LBB10_12
	jmp	.LBB10_30
.LBB10_18:                              # %land.end84.us.preheader
	movabsq	$.L.str18, %r14
	.align	16, 0x90
.LBB10_19:                              # %land.end84.us
                                        # =>This Inner Loop Header: Depth=1
	callq	Perl_PerlIO_stderr
	movq	PL_stack_base(%rip), %rcx
	addq	%r15, %rcx
	movq	(%rcx,%rbx,8), %rcx
	movq	%rax, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	PerlIO_printf
	callq	Perl_PerlIO_stderr
	movq	PL_stack_base(%rip), %rcx
	addq	%r15, %rcx
	movq	(%rcx,%rbx,8), %rdx
	xorq	%rdi, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	%rax, %rsi
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	Perl_do_sv_dump
	leaq	1(%rbx), %rax
	addq	$2, %rbx
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rbx
	movq	%rax, %rbx
	jl	.LBB10_19
.LBB10_30:                              # %for.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, PL_dumpindent(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp95:
	.size	XS_Devel__Peek_DumpArray, .Ltmp95-XS_Devel__Peek_DumpArray
	.cfi_endproc

	.globl	XS_Devel__Peek_DumpProg
	.align	16, 0x90
	.type	XS_Devel__Peek_DumpProg,@function
XS_Devel__Peek_DumpProg:                # @XS_Devel__Peek_DumpProg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 32
.Ltmp102:
	.cfi_offset %rbx, -24
.Ltmp103:
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
	je	.LBB11_2
# BB#1:                                 # %if.then
	movabsq	$.L.str19, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB11_2:                               # %if.end
	movq	PL_dumpindent(%rip), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r14
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
	movq	PL_main_root(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB11_4
# BB#3:                                 # %if.then6
	callq	Perl_op_dump
.LBB11_4:                               # %if.end7
	movq	%r14, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp104:
	.size	XS_Devel__Peek_DumpProg, .Ltmp104-XS_Devel__Peek_DumpProg
	.cfi_endproc

	.globl	XS_Devel__Peek_SvREFCNT
	.align	16, 0x90
	.type	XS_Devel__Peek_SvREFCNT,@function
XS_Devel__Peek_SvREFCNT:                # @XS_Devel__Peek_SvREFCNT
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 48
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %r15
	leaq	(%r15,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rax, %rdx
	cmpq	$8, %rdx
	je	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str21, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %r15
.LBB12_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	movq	8(%r15,%rbx,8), %r13
	leaq	1(%rbx), %r12
	testq	$32, %rcx
	je	.LBB12_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB12_5
.LBB12_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %r15
	movq	%rax, %r14
.LBB12_5:                               # %cond.end
	movq	8(%r13), %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB12_7
# BB#6:                                 # %if.then11
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB12_7:                               # %if.end13
	movq	%r14, 8(%r15,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	XS_Devel__Peek_SvREFCNT, .Ltmp121-XS_Devel__Peek_SvREFCNT
	.cfi_endproc

	.globl	XS_Devel__Peek_SvREFCNT_inc
	.align	16, 0x90
	.type	XS_Devel__Peek_SvREFCNT_inc,@function
XS_Devel__Peek_SvREFCNT_inc:            # @XS_Devel__Peek_SvREFCNT_inc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp126:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp128:
	.cfi_def_cfa_offset 32
.Ltmp129:
	.cfi_offset %rbx, -32
.Ltmp130:
	.cfi_offset %r14, -24
.Ltmp131:
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
	movq	%r14, %r15
	subq	%rdx, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	cmpq	$1, %r15
	je	.LBB13_2
# BB#1:                                 # %if.then
	movabsq	$.L.str22, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB13_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rax
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB13_4
# BB#3:                                 # %land.rhs
	incq	8(%rax)
.LBB13_4:                               # %land.end
	movq	$1, %rcx
	subq	%r15, %rcx
	leaq	(%r14,%rcx,8), %rdx
	movq	%rax, (%r14,%rcx,8)
	movq	%rdx, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp132:
	.size	XS_Devel__Peek_SvREFCNT_inc, .Ltmp132-XS_Devel__Peek_SvREFCNT_inc
	.cfi_endproc

	.globl	XS_Devel__Peek_SvREFCNT_dec
	.align	16, 0x90
	.type	XS_Devel__Peek_SvREFCNT_dec,@function
XS_Devel__Peek_SvREFCNT_dec:            # @XS_Devel__Peek_SvREFCNT_dec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp137:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp138:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 32
.Ltmp140:
	.cfi_offset %rbx, -32
.Ltmp141:
	.cfi_offset %r14, -24
.Ltmp142:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r14
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	movq	%r14, %rbx
	subq	%rdx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$1, %rbx
	je	.LBB14_2
# BB#1:                                 # %if.then
	movabsq	$.L.str23, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB14_2:                               # %if.end
	movq	8(%rax,%r15,8), %r15
	movq	%r15, %rdi
	callq	Perl_sv_free
	movq	$1, %rax
	subq	%rbx, %rax
	leaq	(%r14,%rax,8), %rcx
	movq	%r15, (%r14,%rax,8)
	movq	%rcx, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp143:
	.size	XS_Devel__Peek_SvREFCNT_dec, .Ltmp143-XS_Devel__Peek_SvREFCNT_dec
	.cfi_endproc

	.globl	XS_Devel__Peek_DeadCode
	.align	16, 0x90
	.type	XS_Devel__Peek_DeadCode,@function
XS_Devel__Peek_DeadCode:                # @XS_Devel__Peek_DeadCode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp149:
	.cfi_def_cfa_offset 32
.Ltmp150:
	.cfi_offset %rbx, -24
.Ltmp151:
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
	cmpq	%rax, %rdx
	je	.LBB15_2
# BB#1:                                 # %if.then
	movabsq	$.L.str24, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB15_2:                               # %if.end
	callq	DeadCode
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp152:
	.size	XS_Devel__Peek_DeadCode, .Ltmp152-XS_Devel__Peek_DeadCode
	.cfi_endproc

	.globl	XS_Devel__Peek_CvGV
	.align	16, 0x90
	.type	XS_Devel__Peek_CvGV,@function
XS_Devel__Peek_CvGV:                    # @XS_Devel__Peek_CvGV
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 16
.Ltmp156:
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
	cmpq	$8, %rdx
	je	.LBB16_2
# BB#1:                                 # %if.then
	movabsq	$.L.str25, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB16_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdx
	movzbl	18(%rdx), %esi
	movabsq	$PL_sv_undef, %rcx
	testq	$8, %rsi
	je	.LBB16_6
# BB#3:                                 # %land.lhs.true
	movq	(%rdx), %rdx
	movq	(%rdx), %rdx
	movzbl	16(%rdx), %esi
	cmpq	$12, %rsi
	jne	.LBB16_6
# BB#4:                                 # %cond.true
	movq	(%rdx), %rcx
	movq	96(%rcx), %rcx
	testq	%rcx, %rcx
	movq	%rcx, PL_Sv(%rip)
	je	.LBB16_6
# BB#5:                                 # %land.rhs
	incq	8(%rcx)
.LBB16_6:                               # %cond.end
	movq	%rcx, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %rbx
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	retq
.Ltmp157:
	.size	XS_Devel__Peek_CvGV, .Ltmp157-XS_Devel__Peek_CvGV
	.cfi_endproc

	.globl	XS_Devel__Peek_runops_debug
	.align	16, 0x90
	.type	XS_Devel__Peek_runops_debug,@function
XS_Devel__Peek_runops_debug:            # @XS_Devel__Peek_runops_debug
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp162:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp163:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 32
.Ltmp165:
	.cfi_offset %rbx, -32
.Ltmp166:
	.cfi_offset %r14, -24
.Ltmp167:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	subq	%rax, %rbx
	cmpq	$9, %rbx
	jb	.LBB17_2
# BB#1:                                 # %if.then
	movabsq	$.L.str26, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB17_2:                               # %if.end
	leaq	1(%r15), %r14
	cmpq	$7, %rbx
	jg	.LBB17_6
# BB#3:                                 # %if.end13.thread
	movq	PL_runops(%rip), %rax
	movabsq	$Perl_runops_debug, %rcx
	cmpq	%rcx, %rax
	jne	.LBB17_4
# BB#5:                                 # %if.end13.thread
	movabsq	$1, %rcx
	jmp	.LBB17_16
.LBB17_6:                               # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r15,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB17_8
# BB#7:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB17_9
.LBB17_4:
	movabsq	$0, %rcx
	jmp	.LBB17_16
.LBB17_8:                               # %cond.false
	callq	Perl_sv_2iv
.LBB17_9:                               # %if.end13
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	PL_runops(%rip), %rdi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$Perl_runops_debug, %rdx
	cmpq	%rdx, %rdi
	jne	.LBB17_10
# BB#11:                                # %if.end13
	movabsq	$1, %rcx
	jmp	.LBB17_12
.LBB17_10:
	movabsq	$0, %rcx
.LBB17_12:                              # %if.end13
	testq	%rsi, %rsi
	js	.LBB17_16
# BB#13:                                # %if.then.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rax
	jne	.LBB17_15
# BB#14:                                # %if.then.i
	movabsq	$Perl_runops_standard, %rdx
.LBB17_15:                              # %if.then.i
	movq	%rdx, PL_runops(%rip)
.LBB17_16:                              # %_runops_debug.exit
	testq	%rcx, %rcx
	jne	.LBB17_17
# BB#18:                                # %_runops_debug.exit
	movabsq	$PL_sv_no, %rax
	jmp	.LBB17_19
.LBB17_17:
	movabsq	$PL_sv_yes, %rax
.LBB17_19:                              # %_runops_debug.exit
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r15,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r15,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp168:
	.size	XS_Devel__Peek_runops_debug, .Ltmp168-XS_Devel__Peek_runops_debug
	.cfi_endproc

	.globl	boot_Devel__Peek
	.align	16, 0x90
	.type	boot_Devel__Peek,@function
boot_Devel__Peek:                       # @boot_Devel__Peek
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp172:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp173:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp174:
	.cfi_def_cfa_offset 32
.Ltmp175:
	.cfi_offset %rbx, -24
.Ltmp176:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movabsq	$.L.str28, %rdi
	movabsq	$XS_Devel__Peek_mstat, %rsi
	movabsq	$.L.str27, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str29, %rdi
	movabsq	$XS_Devel__Peek_fill_mstats, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str30, %rdi
	movabsq	$XS_Devel__Peek_mstats_fillhash, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str31, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str32, %rdi
	movabsq	$XS_Devel__Peek_mstats2hash, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str33, %rsi
	movq	%rax, %rdi
	callq	Perl_sv_setpv
	movabsq	$.L.str34, %rdi
	movabsq	$XS_Devel__Peek_Dump, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str35, %rdi
	movabsq	$XS_Devel__Peek_DumpArray, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str36, %rdi
	movabsq	$XS_Devel__Peek_DumpProg, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str37, %rdi
	movabsq	$XS_Devel__Peek_SvREFCNT, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str38, %rdi
	movabsq	$XS_Devel__Peek_SvREFCNT_inc, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str39, %rdi
	movabsq	$XS_Devel__Peek_SvREFCNT_dec, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str40, %rdi
	movabsq	$XS_Devel__Peek_DeadCode, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str41, %rdi
	movabsq	$XS_Devel__Peek_CvGV, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str42, %rdi
	movabsq	$XS_Devel__Peek_runops_debug, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r14,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp177:
	.size	boot_Devel__Peek, .Ltmp177-boot_Devel__Peek
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"GVGV::GV"
	.size	.L.str, 9

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"  busy\n"
	.size	.L.str1, 8

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"    closure-template\n"
	.size	.L.str2, 22

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"     ref in args!\n"
	.size	.L.str3, 19

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"    level %i: refs: %i, strings: %i in %i,\targsarray: %i, argsstrings: %i\n"
	.size	.L.str4, 75

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"  total: refs: %i, strings: %i in %i,\targsarrays: %i, argsstrings: %i\n"
	.size	.L.str5, 71

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"total: refs: %i, strings: %i in %i\targsarray: %i, argsstrings: %i\n"
	.size	.L.str6, 67

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Cannot report mstats without Perl malloc"
	.size	.L.str7, 41

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Usage: Devel::Peek::mstat(str=\"Devel::Peek::mstat: \")"
	.size	.L.str8, 54

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Devel::Peek::mstat: "
	.size	.L.str9, 21

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"%s: perl not compiled with DEBUGGING_MSTATS\n"
	.size	.L.str10, 45

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Usage: Devel::Peek::fill_mstats(sv, level= 0)"
	.size	.L.str11, 46

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Usage: Devel::Peek::mstats_fillhash(sv, level= 0)"
	.size	.L.str12, 50

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Usage: Devel::Peek::mstats2hash(sv, rv, level= 0)"
	.size	.L.str13, 50

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Usage: Devel::Peek::Dump(sv, lim=4)"
	.size	.L.str14, 36

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"Devel::Peek::pv_limit"
	.size	.L.str15, 22

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Devel::Peek::dump_ops"
	.size	.L.str16, 22

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Usage: Devel::Peek::DumpArray(lim, ...)"
	.size	.L.str17, 40

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Elt No. %ld  0x%lx\n"
	.size	.L.str18, 20

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Usage: Devel::Peek::DumpProg()"
	.size	.L.str19, 31

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"dumpindent is %d"
	.size	.L.str20, 17

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Usage: Devel::Peek::SvREFCNT(sv)"
	.size	.L.str21, 33

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Usage: Devel::Peek::SvREFCNT_inc(sv)"
	.size	.L.str22, 37

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Usage: Devel::Peek::SvREFCNT_dec(sv)"
	.size	.L.str23, 37

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Usage: Devel::Peek::DeadCode()"
	.size	.L.str24, 31

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Usage: Devel::Peek::CvGV(cv)"
	.size	.L.str25, 29

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Usage: Devel::Peek::runops_debug(flag= -1)"
	.size	.L.str26, 43

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Peek.c"
	.size	.L.str27, 7

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Devel::Peek::mstat"
	.size	.L.str28, 19

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Devel::Peek::fill_mstats"
	.size	.L.str29, 25

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Devel::Peek::mstats_fillhash"
	.size	.L.str30, 29

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"\\%;$"
	.size	.L.str31, 5

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Devel::Peek::mstats2hash"
	.size	.L.str32, 25

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"$\\%;$"
	.size	.L.str33, 6

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"Devel::Peek::Dump"
	.size	.L.str34, 18

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Devel::Peek::DumpArray"
	.size	.L.str35, 23

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"Devel::Peek::DumpProg"
	.size	.L.str36, 22

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"Devel::Peek::SvREFCNT"
	.size	.L.str37, 22

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Devel::Peek::SvREFCNT_inc"
	.size	.L.str38, 26

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"Devel::Peek::SvREFCNT_dec"
	.size	.L.str39, 26

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Devel::Peek::DeadCode"
	.size	.L.str40, 22

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"Devel::Peek::CvGV"
	.size	.L.str41, 18

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"Devel::Peek::runops_debug"
	.size	.L.str42, 26


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
