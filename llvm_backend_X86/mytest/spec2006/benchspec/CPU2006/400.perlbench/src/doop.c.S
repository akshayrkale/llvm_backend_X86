	.file	"doop.c.bc"
	.text
	.globl	Perl_do_trans
	.align	16, 0x90
	.type	Perl_do_trans,@function
Perl_do_trans:                          # @Perl_do_trans
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	PL_op(%rip), %rcx
	movq	%rdi, %r14
	movq	16(%r14), %rax
	movzbl	37(%rcx), %ebx
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB0_6
# BB#1:                                 # %if.then
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB0_3
# BB#2:                                 # %if.then6
	movq	%r14, %rdi
	callq	Perl_sv_force_normal
	movq	16(%r14), %rax
.LBB0_3:                                # %if.end
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB0_6
# BB#4:                                 # %land.lhs.true
	movq	PL_op(%rip), %rcx
	movzbl	37(%rcx), %ecx
	testq	$4, %rcx
	jne	.LBB0_6
# BB#5:                                 # %if.then14
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r14), %rax
.LBB0_6:                                # %if.end16
	andq	$3, %rbx
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_8
# BB#7:                                 # %cond.true
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	jmp	.LBB0_9
.LBB0_8:                                # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%rsp), %rcx
.LBB0_9:                                # %cond.end
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB0_29
# BB#10:                                # %if.end23
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$4, %rax
	jne	.LBB0_16
# BB#11:                                # %if.then28
	movq	16(%r14), %rcx
	testq	$67108864, %rcx         # imm = 0x4000000
	jne	.LBB0_15
# BB#12:                                # %if.then32
	movq	%rcx, %rdx
	andq	$10223616, %rdx         # imm = 0x9C0000
	cmpq	$262144, %rdx           # imm = 0x40000
	jne	.LBB0_14
# BB#13:                                # %cond.true37
	movq	(%r14), %rdx
	movq	8(%rdx), %rdx
	movq	%rdx, (%rsp)
	jmp	.LBB0_15
.LBB0_14:                               # %cond.false42
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	PL_op(%rip), %rax
	movq	16(%r14), %rcx
	movzbl	37(%rax), %eax
.LBB0_15:                               # %if.end46
	andq	$1760624639, %rcx       # imm = 0x68F0FFFF
	orq	$67371008, %rcx         # imm = 0x4040000
	movq	%rcx, 16(%r14)
.LBB0_16:                               # %if.end50
	movq	%rbx, %rcx
	xorq	$63, %rcx
	andq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$36, %rcx
	je	.LBB0_22
# BB#17:                                # %if.end50
	cmpq	$4, %rcx
	jne	.LBB0_18
.LBB0_22:                               # %sw.bb61
	xorq	%rax, %rax
	movq	%r14, %rdi
	testq	%rbx, %rbx
	je	.LBB0_24
# BB#23:                                # %if.then63
	callq	do_trans_count_utf8
	jmp	.LBB0_28
.LBB0_18:                               # %if.end50
	xorq	%rax, %rax
	movq	%r14, %rdi
	testq	%rcx, %rcx
	jne	.LBB0_25
# BB#19:                                # %sw.bb
	testq	%rbx, %rbx
	je	.LBB0_21
# BB#20:                                # %if.then56
	callq	do_trans_simple_utf8
	jmp	.LBB0_28
.LBB0_24:                               # %if.else66
	callq	do_trans_count
	jmp	.LBB0_28
.LBB0_25:                               # %sw.default
	testq	%rbx, %rbx
	je	.LBB0_27
# BB#26:                                # %if.then70
	callq	do_trans_complex_utf8
	jmp	.LBB0_28
.LBB0_21:                               # %if.else
	callq	do_trans_simple
	jmp	.LBB0_28
.LBB0_27:                               # %if.else73
	callq	do_trans_complex
.LBB0_28:                               # %return
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB0_29:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	Perl_do_trans, .Ltmp8-Perl_do_trans
	.cfi_endproc

	.globl	Perl_do_join
	.align	16, 0x90
	.type	Perl_do_join,@function
Perl_do_join:                           # @Perl_do_join
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp17:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp20:
	.cfi_def_cfa_offset 96
.Ltmp21:
	.cfi_offset %rbx, -48
.Ltmp22:
	.cfi_offset %r12, -40
.Ltmp23:
	.cfi_offset %r13, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movzbl	18(%r15), %eax
	subq	%r12, %r13
	movq	$3, %rcx
	movq	%r13, %r14
	sarq	%cl, %r14
	testq	$4, %rax
	je	.LBB1_2
# BB#1:                                 # %cond.true
	movq	(%r15), %rax
	movq	8(%rax), %rax
	movq	%rax, 40(%rsp)
	jmp	.LBB1_3
.LBB1_2:                                # %cond.false
	leaq	40(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
.LBB1_3:                                # %cond.end
	xorq	%rsi, %rsi
	testq	%r13, %r13
	jle	.LBB1_5
# BB#4:                                 # %cond.true3
	movq	40(%rsp), %rax
	leaq	-1(%r14), %rsi
	imulq	%rax, %rsi
.LBB1_5:                                # %cond.end5
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movzbl	16(%rbx), %eax
	testq	$252, %rax
	jne	.LBB1_7
# BB#6:                                 # %lor.rhs
	movq	%rsi, %r13
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	%r13, %rsi
.LBB1_7:                                # %lor.end
	movq	(%rbx), %rax
	movq	16(%rax), %rax
	leaq	8(%r12), %r13
	leaq	(%rsi,%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB1_22
# BB#8:                                 # %while.cond.preheader
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jle	.LBB1_9
# BB#10:
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r14, %r12
	movq	%r13, %r15
	.align	16, 0x90
.LBB1_11:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB1_18
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_11 Depth=1
	movq	16(%rdi), %rax
	testq	$268443648, %rax        # imm = 0x10002000
	jne	.LBB1_18
# BB#13:                                # %land.lhs.true
                                        #   in Loop: Header=BB1_11 Depth=1
	movq	%rax, %rcx
	andq	$118423552, %rcx        # imm = 0x70F0000
	je	.LBB1_18
# BB#14:                                # %if.then24
                                        #   in Loop: Header=BB1_11 Depth=1
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB1_15
# BB#16:                                # %cond.false34
                                        #   in Loop: Header=BB1_11 Depth=1
	movq	$2, %rdx
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	leaq	32(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	32(%rsp), %rax
	jmp	.LBB1_17
.LBB1_15:                               # %cond.true29
                                        #   in Loop: Header=BB1_11 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rax
	movq	%rax, 32(%rsp)
.LBB1_17:                               # %cond.end36
                                        #   in Loop: Header=BB1_11 Depth=1
	addq	%rax, %rsi
.LBB1_18:                               # %if.end
                                        #   in Loop: Header=BB1_11 Depth=1
	decq	%r12
	addq	$8, %r15
	testq	%r12, %r12
	jg	.LBB1_11
# BB#19:                                # %while.cond.while.end_crit_edge
	movq	(%rbx), %rax
	movq	16(%rax), %rax
	movq	(%rsp), %r12            # 8-byte Reload
	jmp	.LBB1_20
.LBB1_9:
	movq	%r15, 8(%rsp)           # 8-byte Spill
.LBB1_20:                               # %while.end
	incq	%rsi
	cmpq	%rsi, %rax
	movq	8(%rsp), %r15           # 8-byte Reload
	jae	.LBB1_22
# BB#21:                                # %cond.true45
	movq	%rbx, %rdi
	callq	Perl_sv_grow
.LBB1_22:                               # %if.end58
	movabsq	$.L.str, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_setpvn
	movq	16(%rbx), %rax
	movq	%rax, %rcx
	andq	$-536870913, %rcx       # imm = 0xFFFFFFFFDFFFFFFF
	movq	%rcx, 16(%rbx)
	movzbl	PL_tainting(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB1_25
# BB#23:                                # %if.end58
	andq	$57344, %rax            # imm = 0xE000
	je	.LBB1_25
# BB#24:                                # %if.then69
	movq	%rbx, %rdi
	callq	Perl_sv_untaint
.LBB1_25:                               # %if.end71
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jle	.LBB1_29
# BB#26:                                # %if.then75
	movq	(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB1_28
# BB#27:                                # %if.then77
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_catsv_flags
.LBB1_28:                               # %if.end78
	addq	$16, %r12
	movq	%r12, %r13
.LBB1_29:                               # %if.end80
	decq	%r14
	cmpq	$0, 40(%rsp)
	jne	.LBB1_30
	jmp	.LBB1_33
	.align	16, 0x90
.LBB1_32:                               # %for.body90
                                        #   in Loop: Header=BB1_33 Depth=1
	movq	(%r13), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_catsv_flags
	decq	%r14
	addq	$8, %r13
.LBB1_33:                               # %for.body90
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	jg	.LBB1_32
	jmp	.LBB1_34
	.align	16, 0x90
.LBB1_31:                               # %for.body
                                        #   in Loop: Header=BB1_30 Depth=1
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_catsv_flags
	movq	(%r13), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_catsv_flags
	decq	%r14
	addq	$8, %r13
.LBB1_30:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	jg	.LBB1_31
.LBB1_34:                               # %if.end95
	movzbl	17(%rbx), %eax
	testq	$64, %rax
	je	.LBB1_36
# BB#35:                                # %if.then99
	movq	%rbx, %rdi
	callq	Perl_mg_set
.LBB1_36:                               # %if.end101
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp26:
	.size	Perl_do_join, .Ltmp26-Perl_do_join
	.cfi_endproc

	.globl	Perl_do_sprintf
	.align	16, 0x90
	.type	Perl_do_sprintf,@function
Perl_do_sprintf:                        # @Perl_do_sprintf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp33:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp34:
	.cfi_def_cfa_offset 64
.Ltmp35:
	.cfi_offset %rbx, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movq	(%rbx), %rcx
	movzbl	18(%rcx), %eax
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	$4, %rax
	je	.LBB2_2
# BB#1:                                 # %cond.true
	movq	(%rcx), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 24(%rsp)
	movq	(%rax), %rsi
	jmp	.LBB2_3
.LBB2_2:                                # %cond.false
	leaq	24(%rsp), %rsi
	movq	$2, %rdx
	movq	%rcx, %rdi
	callq	Perl_sv_2pv_flags
	movq	(%rbx), %rcx
	movq	%rax, %rsi
.LBB2_3:                                # %cond.end
	xorq	%rax, %rax
	movb	%al, 23(%rsp)
	movq	16(%r14), %rax
	movq	%rax, %rdx
	andq	$-536870913, %rdx       # imm = 0xFFFFFFFFDFFFFFFF
	movq	%rdx, 16(%r14)
	movzbl	19(%rcx), %ecx
	testq	$32, %rcx
	je	.LBB2_6
# BB#4:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rcx
	movzbl	37(%rcx), %ecx
	testq	$8, %rcx
	jne	.LBB2_6
# BB#5:                                 # %if.then
	orq	$536870912, %rax        # imm = 0x20000000
	movq	%rax, 16(%r14)
.LBB2_6:                                # %if.end
	movq	24(%rsp), %rdx
	addq	$8, %rbx
	decq	%r15
	leaq	23(%rsp), %rax
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	movq	%rbx, %r8
	movq	%r15, %r9
	callq	Perl_sv_vsetpvfn
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB2_8
# BB#7:                                 # %if.then12
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB2_8:                                # %if.end14
	movzbl	23(%rsp), %eax
	testq	%rax, %rax
	je	.LBB2_11
# BB#9:                                 # %if.end14
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB2_11
# BB#10:                                # %if.then18
	movq	%r14, %rdi
	callq	Perl_sv_taint
.LBB2_11:                               # %if.end20
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	Perl_do_sprintf, .Ltmp38-Perl_do_sprintf
	.cfi_endproc

	.globl	Perl_do_vecget
	.align	16, 0x90
	.type	Perl_do_vecget,@function
Perl_do_vecget:                         # @Perl_do_vecget
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
	subq	$16, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %rbx
	movzbl	18(%rbx), %eax
	movq	%rdx, %r13
	movq	%rsi, %r12
	testq	$4, %rax
	je	.LBB3_2
# BB#1:                                 # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r14
	jmp	.LBB3_3
.LBB3_2:                                # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r14
.LBB3_3:                                # %cond.end
	xorq	%r15, %r15
	testq	%r12, %r12
	js	.LBB3_28
# BB#4:                                 # %if.end
	testq	%r13, %r13
	jle	.LBB3_6
# BB#5:                                 # %lor.lhs.false
	leaq	-1(%r13), %rax
	testq	%r13, %rax
	je	.LBB3_7
.LBB3_6:                                # %if.then5
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB3_7:                                # %if.end6
	movzbl	19(%rbx), %eax
	testq	$32, %rax
	je	.LBB3_9
# BB#8:                                 # %if.then10
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_utf8_downgrade
.LBB3_9:                                # %if.end12
	imulq	%r13, %r12
	leaq	(%r13,%r12), %rdx
	leaq	7(%r13,%r12), %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	movq	8(%rsp), %rax
	leaq	7(%rsi,%rdx), %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jbe	.LBB3_20
# BB#10:                                # %if.then15
	cmpq	$9, %r13
	jl	.LBB3_28
# BB#11:                                # %if.else
	movq	$3, %rcx
	sarq	%cl, %r12
	cmpq	$32, %r13
	jne	.LBB3_12
# BB#15:                                # %if.then27
	cmpq	%rax, %r12
	jae	.LBB3_28
# BB#16:                                # %if.else31
	leaq	1(%r12), %rcx
	cmpq	%rax, %rcx
	jae	.LBB3_17
# BB#18:                                # %if.else39
	leaq	2(%r12), %rdx
	movzbl	(%r14,%r12), %esi
	movzbl	1(%r14,%r12), %r15d
	movq	$24, %rcx
	shlq	%cl, %rsi
	movq	$16, %rcx
	shlq	%cl, %r15
	orq	%rsi, %r15
	cmpq	%rax, %rdx
	jae	.LBB3_28
# BB#19:                                # %if.else52
	movzbl	(%r14,%rdx), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	orq	%rax, %r15
	jmp	.LBB3_28
.LBB3_20:                               # %if.else73
	movq	$3, %rcx
	movq	%r12, %rax
	sarq	%cl, %rax
	cmpq	$7, %r13
	jg	.LBB3_22
# BB#21:                                # %if.then76
	movzbl	(%r14,%rax), %eax
	andq	$7, %r12
	movq	%r12, %rcx
	shrq	%cl, %rax
	movq	$1, %r15
	movq	%r13, %rcx
	shlq	%cl, %r15
	decq	%r15
	andq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	jmp	.LBB3_28
.LBB3_22:                               # %if.else87
	cmpq	$32, %r13
	je	.LBB3_27
# BB#23:                                # %if.else87
	cmpq	$16, %r13
	jne	.LBB3_24
# BB#26:                                # %if.then97
	movzbl	(%r14,%rax), %edx
	movzbl	1(%r14,%rax), %r15d
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %r15
	jmp	.LBB3_28
.LBB3_12:                               # %if.else
	cmpq	$16, %r13
	jne	.LBB3_28
# BB#13:                                # %if.then19
	cmpq	%rax, %r12
	jae	.LBB3_28
# BB#14:                                # %if.else22
	movzbl	(%r14,%r12), %r15d
	movq	$8, %rcx
	shlq	%cl, %r15
	jmp	.LBB3_28
.LBB3_27:                               # %if.then108
	movzbl	(%r14,%rax), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	movzbl	1(%r14,%rax), %esi
	movq	$16, %rcx
	shlq	%cl, %rsi
	movzbl	2(%r14,%rax), %edi
	orq	%rdx, %rsi
	movzbl	3(%r14,%rax), %r15d
	movq	$8, %rcx
	shlq	%cl, %rdi
	orq	%rsi, %rdi
	orq	%rdi, %r15
	jmp	.LBB3_28
.LBB3_24:                               # %if.else87
	cmpq	$8, %r13
	jne	.LBB3_28
# BB#25:                                # %if.then91
	movzbl	(%r14,%rax), %r15d
	jmp	.LBB3_28
.LBB3_17:                               # %if.then35
	movzbl	(%r14,%r12), %r15d
	movq	$24, %rcx
	shlq	%cl, %r15
.LBB3_28:                               # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	Perl_do_vecget, .Ltmp56-Perl_do_vecget
	.cfi_endproc

	.globl	Perl_do_vecset
	.align	16, 0x90
	.type	Perl_do_vecset,@function
Perl_do_vecset:                         # @Perl_do_vecset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp68:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	72(%rax), %r12
	testq	%r12, %r12
	je	.LBB4_29
# BB#1:                                 # %if.end
	movq	16(%r12), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB4_3
# BB#2:                                 # %cond.true
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rsp)
	movq	(%rax), %rax
	jmp	.LBB4_4
.LBB4_3:                                # %cond.false
	leaq	24(%rsp), %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	Perl_sv_pvn_force_flags
.LBB4_4:                                # %cond.end
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	16(%r12), %rax
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB4_6
# BB#5:                                 # %if.then6
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_sv_utf8_downgrade
	movq	16(%r12), %rax
.LBB4_6:                                # %if.end8
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r12)
	movzbl	18(%rbx), %eax
	testq	$1, %rax
	je	.LBB4_8
# BB#7:                                 # %cond.true15
	movq	(%rbx), %rcx
	movq	24(%rcx), %rax
	jmp	.LBB4_9
.LBB4_8:                                # %cond.false17
	movq	%rbx, %rdi
	callq	Perl_sv_2uv
	movq	(%rbx), %rcx
.LBB4_9:                                # %cond.end19
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	56(%rcx), %r14
	testq	%r14, %r14
	jns	.LBB4_11
# BB#10:                                # %if.then23
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	(%rbx), %rcx
.LBB4_11:                               # %if.end24
	movq	64(%rcx), %rbx
	testq	%rbx, %rbx
	jle	.LBB4_13
# BB#12:                                # %lor.lhs.false
	leaq	-1(%rbx), %rax
	testq	%rbx, %rax
	je	.LBB4_14
.LBB4_13:                               # %if.then29
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB4_14:                               # %if.end30
	imulq	%rbx, %r14
	leaq	(%rbx,%r14), %rax
	leaq	7(%rbx,%r14), %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	movq	$61, %rcx
	shrq	%cl, %rsi
	movq	24(%rsp), %rdx
	leaq	7(%rsi,%rax), %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	cmpq	%rdx, %r15
	jbe	.LBB4_19
# BB#15:                                # %if.then33
	movq	(%r12), %rax
	movq	16(%rax), %rcx
	leaq	1(%r15), %r13
	cmpq	%r13, %rcx
	jae	.LBB4_17
# BB#16:                                # %cond.true37
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_grow
	movq	24(%rsp), %rdx
	jmp	.LBB4_18
.LBB4_17:                               # %cond.false40
	movq	(%rax), %rax
.LBB4_18:                               # %cond.end43
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	(%rax,%rdx), %rdi
	subq	%rdx, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	memset
	movq	(%r12), %rax
	movq	%r15, 8(%rax)
.LBB4_19:                               # %if.end49
	cmpq	$7, %rbx
	jg	.LBB4_21
# BB#20:                                # %if.then51
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r14, %rax
	andq	$7, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	andq	%rdx, %rbx
	movq	$3, %rcx
	sarq	%cl, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movzbl	(%rdi,%r14), %esi
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rsi, %rbx
	movb	%bl, (%rdi,%r14)
	jmp	.LBB4_27
.LBB4_21:                               # %if.else
	movq	$3, %rcx
	sarq	%cl, %r14
	cmpq	$32, %rbx
	je	.LBB4_26
# BB#22:                                # %if.else
	cmpq	$16, %rbx
	jne	.LBB4_23
# BB#25:                                # %if.then74
	movq	$8, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movb	%al, (%rcx,%r14)
	movb	%dl, 1(%rcx,%r14)
	jmp	.LBB4_27
.LBB4_26:                               # %if.then86
	movq	$24, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rsi, %rax
	shrq	%cl, %rax
	movq	16(%rsp), %rdx          # 8-byte Reload
	movb	%al, (%rdx,%r14)
	movq	$16, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movb	%al, 1(%rdx,%r14)
	movq	$8, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movb	%al, 2(%rdx,%r14)
	movb	%sil, 3(%rdx,%r14)
	jmp	.LBB4_27
.LBB4_23:                               # %if.else
	cmpq	$8, %rbx
	jne	.LBB4_27
# BB#24:                                # %if.then67
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
	movb	%cl, (%rax,%r14)
.LBB4_27:                               # %if.end108
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB4_29
# BB#28:                                # %if.then112
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB4_29:                               # %if.end114
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	Perl_do_vecset, .Ltmp74-Perl_do_vecset
	.cfi_endproc

	.globl	Perl_do_chop
	.align	16, 0x90
	.type	Perl_do_chop,@function
Perl_do_chop:                           # @Perl_do_chop
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
	subq	$16, %rsp
.Ltmp86:
	.cfi_def_cfa_offset 64
.Ltmp87:
	.cfi_offset %rbx, -48
.Ltmp88:
	.cfi_offset %r12, -40
.Ltmp89:
	.cfi_offset %r13, -32
.Ltmp90:
	.cfi_offset %r14, -24
.Ltmp91:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	16(%r12), %rax
	movq	%rdi, %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$11, %rcx
	jne	.LBB5_1
# BB#11:                                # %if.then12
	movq	%r12, %rdi
	callq	Perl_hv_iterinit
	jmp	.LBB5_12
	.align	16, 0x90
.LBB5_13:                               # %while.body
                                        #   in Loop: Header=BB5_12 Depth=1
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	Perl_hv_iterval
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	do_chop
.LBB5_12:                               # %if.then12
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	testq	%rax, %rax
	jne	.LBB5_13
	jmp	.LBB5_43
.LBB5_1:                                # %entry
	cmpq	$10, %rcx
	jne	.LBB5_14
# BB#2:                                 # %if.then
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB5_4
# BB#3:                                 # %cond.true
	movq	%r12, %rdi
	callq	Perl_mg_size
	movq	%rax, %r15
	jmp	.LBB5_5
.LBB5_14:                               # %if.else19
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB5_19
# BB#15:                                # %if.then23
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB5_17
# BB#16:                                # %if.then27
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	Perl_sv_force_normal_flags
	movq	16(%r12), %rax
.LBB5_17:                               # %if.end28
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB5_19
# BB#18:                                # %if.then32
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r12), %rax
.LBB5_19:                               # %if.end36
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB5_21
# BB#20:                                # %cond.true40
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r15
	jmp	.LBB5_22
.LBB5_4:                                # %cond.false
	movq	(%r12), %rax
	movq	8(%rax), %r15
.LBB5_5:                                # %cond.end
	testq	%r15, %r15
	js	.LBB5_43
# BB#6:                                 # %for.body.preheader
	incq	%r15
	xorq	%rbx, %rbx
	movabsq	$PL_sv_undef, %r13
	.align	16, 0x90
.LBB5_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB5_10
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_7 Depth=1
	movq	(%rax), %rsi
	cmpq	%r13, %rsi
	je	.LBB5_10
# BB#9:                                 # %if.then7
                                        #   in Loop: Header=BB5_7 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	do_chop
.LBB5_10:                               # %for.inc
                                        #   in Loop: Header=BB5_7 Depth=1
	incq	%rbx
	cmpq	%rbx, %r15
	jne	.LBB5_7
	jmp	.LBB5_43
.LBB5_21:                               # %cond.false43
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	Perl_sv_2pv_flags
	movq	8(%rsp), %rcx
	movq	%rax, %r15
.LBB5_22:                               # %cond.end45
	testq	%rcx, %rcx
	je	.LBB5_27
# BB#23:                                # %land.lhs.true48
	movq	16(%r12), %rax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB5_27
# BB#24:                                # %if.then52
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB5_26
# BB#25:                                # %cond.true56
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %r15
	jmp	.LBB5_27
.LBB5_26:                               # %cond.false61
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	movq	%r12, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	%rax, %r15
.LBB5_27:                               # %if.end65
	movzbl	19(%r12), %eax
	testq	$32, %rax
	je	.LBB5_37
# BB#28:                                # %land.lhs.true69
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	je	.LBB5_29
.LBB5_37:                               # %if.else105
	testq	%r15, %r15
	je	.LBB5_40
# BB#38:                                # %if.else105
	movq	8(%rsp), %rbx
	testq	%rbx, %rbx
	je	.LBB5_40
# BB#39:                                # %if.then109
	leaq	-1(%rbx), %rax
	movq	%rax, 8(%rsp)
	leaq	-1(%r15,%rbx), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setpvn
	xorq	%rax, %rax
	movb	%al, -1(%r15,%rbx)
	movq	8(%rsp), %rax
	movq	(%r12), %rcx
	movq	%rax, 8(%rcx)
	andq	$1560084479, 16(%r12)   # imm = 0x5CFCFFFF
	jmp	.LBB5_41
.LBB5_29:                               # %if.then72
	testq	%r15, %r15
	je	.LBB5_40
# BB#30:                                # %if.then72
	movq	8(%rsp), %rbx
	testq	%rbx, %rbx
	je	.LBB5_40
# BB#31:                                # %if.then76
	leaq	-1(%rbx), %rax
	testq	%rax, %rax
	leaq	-1(%r15,%rbx), %r13
	jle	.LBB5_35
	.align	16, 0x90
.LBB5_33:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%r13), %rax
	testq	%rax, %rax
	jns	.LBB5_35
# BB#34:                                # %land.rhs
                                        #   in Loop: Header=BB5_33 Depth=1
	andq	$255, %rax
	cmpq	$192, %rax
	jae	.LBB5_35
# BB#32:                                # %while.cond78
                                        #   in Loop: Header=BB5_33 Depth=1
	decq	%r13
	cmpq	%r15, %r13
	ja	.LBB5_33
.LBB5_35:                               # %while.end90
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	Perl_utf8_to_uvchr
	testq	%rax, %rax
	je	.LBB5_41
# BB#36:                                # %if.then93
	addq	%r15, %rbx
	subq	%r13, %rbx
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_setpvn
	xorq	%rax, %rax
	movb	%al, (%r13)
	movq	(%r12), %rax
	subq	%r15, %r13
	movq	%r13, 8(%rax)
	andq	$2096955391, 16(%r12)   # imm = 0x7CFCFFFF
	orq	$536870912, 16(%r14)    # imm = 0x20000000
	jmp	.LBB5_41
.LBB5_40:                               # %if.else118
	movabsq	$.L.str, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setpvn
.LBB5_41:                               # %if.end120
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB5_43
# BB#42:                                # %if.then124
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB5_43:                               # %if.end126
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp92:
	.size	Perl_do_chop, .Ltmp92-Perl_do_chop
	.cfi_endproc

	.globl	Perl_do_chomp
	.align	16, 0x90
	.type	Perl_do_chomp,@function
Perl_do_chomp:                          # @Perl_do_chomp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp101:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp102:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp103:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp104:
	.cfi_def_cfa_offset 112
.Ltmp105:
	.cfi_offset %rbx, -48
.Ltmp106:
	.cfi_offset %r12, -40
.Ltmp107:
	.cfi_offset %r13, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	PL_rs(%rip), %rax
	movq	16(%rax), %rcx
	movq	%rdi, %r14
	xorq	%r12, %r12
	testq	$118423552, %rcx        # imm = 0x70F0000
	je	.LBB6_74
# BB#1:                                 # %if.end
	testq	$524288, %rcx           # imm = 0x80000
	je	.LBB6_6
# BB#2:                                 # %land.lhs.true
	movq	(%rax), %rax
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB6_4
# BB#3:                                 # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %rax
	jmp	.LBB6_5
.LBB6_4:                                # %cond.false
	callq	Perl_sv_2iv
.LBB6_5:                                # %cond.end
	testq	%rax, %rax
	jg	.LBB6_74
.LBB6_6:                                # %if.end13
	movq	16(%r14), %rax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$11, %rcx
	jne	.LBB6_7
# BB#17:                                # %if.then39
	movq	%r14, %rdi
	callq	Perl_hv_iterinit
	movq	%r14, %rdi
	callq	Perl_hv_iternext
	xorq	%r12, %r12
	jmp	.LBB6_18
	.align	16, 0x90
.LBB6_19:                               # %while.body
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_hv_iterval
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	do_chomp
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %r12
	movq	%r14, %rdi
	callq	Perl_hv_iternext
.LBB6_18:                               # %if.then39
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB6_19
	jmp	.LBB6_74
.LBB6_7:                                # %if.end13
	cmpq	$10, %rcx
	jne	.LBB6_20
# BB#8:                                 # %if.then17
	testq	$32768, %rax            # imm = 0x8000
	je	.LBB6_10
# BB#9:                                 # %cond.true21
	movq	%r14, %rdi
	callq	Perl_mg_size
	movq	%rax, %r15
	jmp	.LBB6_11
.LBB6_20:                               # %if.else48
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB6_25
# BB#21:                                # %if.then52
	testq	$1048576, %rax          # imm = 0x100000
	je	.LBB6_23
# BB#22:                                # %if.then56
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_force_normal_flags
	movq	16(%r14), %rax
.LBB6_23:                               # %if.end57
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB6_25
# BB#24:                                # %if.then61
	movabsq	$PL_no_modify, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB6_25:                               # %if.end65
	movq	PL_encoding(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB6_28
# BB#26:                                # %if.then67
	movzbl	19(%r14), %eax
	testq	$32, %rax
	jne	.LBB6_28
# BB#27:                                # %if.then71
	movq	%r14, %rdi
	callq	Perl_sv_recode_to_utf8
.LBB6_28:                               # %if.end74
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB6_30
# BB#29:                                # %cond.true79
	movq	(%r14), %rax
	movq	8(%rax), %rsi
	movq	%rsi, 56(%rsp)
	movq	(%rax), %rax
	jmp	.LBB6_31
.LBB6_10:                               # %cond.false23
	movq	(%r14), %rax
	movq	8(%rax), %r15
.LBB6_11:                               # %cond.end25
	testq	%r15, %r15
	js	.LBB6_74
# BB#12:                                # %for.body.preheader
	incq	%r15
	xorq	%r12, %r12
	movabsq	$PL_sv_undef, %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB6_13:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	je	.LBB6_16
# BB#14:                                # %land.lhs.true30
                                        #   in Loop: Header=BB6_13 Depth=1
	movq	(%rax), %rdi
	cmpq	%r13, %rdi
	je	.LBB6_16
# BB#15:                                # %if.then32
                                        #   in Loop: Header=BB6_13 Depth=1
	xorq	%rax, %rax
	callq	do_chomp
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rax, %r12
.LBB6_16:                               # %for.inc
                                        #   in Loop: Header=BB6_13 Depth=1
	incq	%rbx
	cmpq	%rbx, %r15
	jne	.LBB6_13
	jmp	.LBB6_74
.LBB6_30:                               # %cond.false82
	leaq	56(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	56(%rsp), %rsi
.LBB6_31:                               # %cond.end84
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB6_32
# BB#33:                                # %cond.end84
	testq	%rsi, %rsi
	movabsq	$0, %rbx
	je	.LBB6_73
# BB#34:                                # %if.then89
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	PL_rs(%rip), %rdi
	leaq	-1(%rsi), %rcx
	movq	%rcx, 56(%rsp)
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB6_44
# BB#35:                                # %land.lhs.true93
	movq	(%rdi), %rax
	cmpq	$0, 8(%rax)
	je	.LBB6_36
# BB#43:                                # %cond.true118
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %rax
	jmp	.LBB6_45
.LBB6_32:
	xorq	%rbx, %rbx
	jmp	.LBB6_73
.LBB6_44:                               # %cond.false123
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	leaq	40(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	PL_rs(%rip), %rdi
.LBB6_45:                               # %cond.end125
	movq	16(%rdi), %rcx
	movq	%rax, %r15
	testq	$536870912, %rcx        # imm = 0x20000000
	je	.LBB6_47
# BB#46:                                # %cond.true130
	callq	Perl_sv_len_utf8
	movq	PL_rs(%rip), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, %r13
	jmp	.LBB6_48
.LBB6_47:                               # %cond.false132
	movq	40(%rsp), %r13
.LBB6_48:                               # %cond.end133
	movq	16(%r14), %rax
	andq	$536870912, %rcx        # imm = 0x20000000
	andq	$536870912, %rax        # imm = 0x20000000
	xorq	%rbx, %rbx
	cmpq	%rax, %rcx
	movq	%r15, %r8
	jne	.LBB6_50
# BB#49:
	xorq	%r15, %r15
	jmp	.LBB6_60
.LBB6_50:                               # %if.then141
	testq	%rcx, %rcx
	je	.LBB6_53
# BB#51:                                # %if.then145
	movq	$1, %rax
	movb	%al, 39(%rsp)
	leaq	40(%rsp), %rsi
	leaq	39(%rsp), %rdx
	movq	%r8, %rdi
	callq	Perl_bytes_from_utf8
	movzbl	39(%rsp), %ecx
	movq	%rax, %rbx
	xorq	%r12, %r12
	testq	%rcx, %rcx
	je	.LBB6_59
# BB#52:
	xorq	%rbx, %rbx
	jmp	.LBB6_73
.LBB6_36:                               # %if.then97
	movq	24(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rdx,%rcx), %eax
	xorq	%r12, %r12
	cmpq	$10, %rax
	movabsq	$0, %rbx
	jne	.LBB6_73
# BB#37:                                # %while.cond104thread-pre-split
	testq	%rcx, %rcx
	je	.LBB6_38
# BB#39:                                # %land.rhs.preheader
	addq	%rsi, %rdx
	movq	$1, %r13
	movq	$-2, %rax
	movabsq	$0, %rbx
.LBB6_40:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx,%rax), %ecx
	xorq	%r15, %r15
	cmpq	$10, %rcx
	jne	.LBB6_41
# BB#42:                                # %while.body109
                                        #   in Loop: Header=BB6_40 Depth=1
	leaq	(%rsi,%rax), %rcx
	incq	%r13
	decq	%rax
	xorq	%r15, %r15
	cmpq	%r13, %rsi
	movq	%rcx, 56(%rsp)
	jne	.LBB6_40
	jmp	.LBB6_65
.LBB6_53:                               # %if.else150
	cmpq	$0, PL_encoding(%rip)
	je	.LBB6_58
# BB#54:                                # %if.then152
	movq	40(%rsp), %rsi
	movq	%r8, %rdi
	callq	Perl_newSVpvn
	movq	PL_encoding(%rip), %rsi
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	Perl_sv_recode_to_utf8
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB6_56
# BB#55:                                # %cond.true159
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %r12
	jmp	.LBB6_57
.LBB6_58:                               # %if.else169
	leaq	40(%rsp), %rsi
	movq	%r8, %rdi
	callq	Perl_bytes_to_utf8
	movq	%rax, %rbx
.LBB6_59:                               # %if.end173
	xorq	%r15, %r15
	movq	%rbx, %r8
.LBB6_60:                               # %if.end173
	movq	24(%rsp), %rdi          # 8-byte Reload
.LBB6_61:                               # %if.end173
	movq	40(%rsp), %rdx
	cmpq	$1, %rdx
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB6_63
# BB#62:                                # %if.then176
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	(%rdi,%rax), %eax
	movzbl	(%r8), %ecx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	movq	$1, %r13
	je	.LBB6_65
	jmp	.LBB6_71
.LBB6_63:                               # %if.else184
	movq	56(%rsp), %rax
	leaq	-1(%rdx), %rcx
	xorq	%r12, %r12
	subq	%rcx, %rax
	jb	.LBB6_71
# BB#64:                                # %if.end188
	movq	%rax, 56(%rsp)
	subq	%rdx, %rsi
	addq	%rsi, %rdi
	movq	%r8, %rsi
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_71
	jmp	.LBB6_65
.LBB6_38:
	xorq	%r15, %r15
	movq	$1, %r13
	xorq	%rbx, %rbx
	jmp	.LBB6_65
.LBB6_41:
	xorq	%rbx, %rbx
.LBB6_65:                               # %if.end199
	movq	16(%r14), %rax
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB6_67
# BB#66:                                # %cond.true204
	movq	(%r14), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 48(%rsp)
	jmp	.LBB6_68
.LBB6_67:                               # %cond.false209
	leaq	48(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_pvn_force_flags
	movq	(%r14), %rax
.LBB6_68:                               # %cond.end211
	movq	56(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	(%rax), %rax
	xorq	%rdx, %rdx
	movb	%dl, (%rax,%rcx)
	movq	16(%r14), %rax
	movq	%rax, %rcx
	andq	$2096955391, %rcx       # imm = 0x7CFCFFFF
	movq	%rcx, 16(%r14)
	testq	$16384, %rax            # imm = 0x4000
	je	.LBB6_70
# BB#69:                                # %if.then225
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB6_70:                               # %nope
	movq	%r13, %r12
.LBB6_71:                               # %nope
	testq	%r15, %r15
	je	.LBB6_73
# BB#72:                                # %if.then230
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB6_73:                               # %if.end231
	movq	%rbx, %rdi
	callq	Perl_safesysfree
.LBB6_74:                               # %return
	movq	%r12, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_56:                               # %cond.false164
	leaq	40(%rsp), %rsi
	movq	$2, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, %r12
.LBB6_57:                               # %cond.end166
	movq	%r15, %rdi
	callq	Perl_sv_len_utf8
	movq	%rax, %r13
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %r8
	jmp	.LBB6_61
.Ltmp110:
	.size	Perl_do_chomp, .Ltmp110-Perl_do_chomp
	.cfi_endproc

	.globl	Perl_do_vop
	.align	16, 0x90
	.type	Perl_do_vop,@function
Perl_do_vop:                            # @Perl_do_vop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp122:
	.cfi_def_cfa_offset 176
.Ltmp123:
	.cfi_offset %rbx, -48
.Ltmp124:
	.cfi_offset %r12, -40
.Ltmp125:
	.cfi_offset %r13, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movzbl	19(%rbx), %eax
	movq	%rcx, %r14
	movq	%rsi, %r15
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	testq	$32, %rax
	movabsq	$0, %r13
	je	.LBB7_4
# BB#1:                                 # %land.rhs
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	jne	.LBB7_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %r13
	jmp	.LBB7_4
.LBB7_2:
	movabsq	$0, %r13
.LBB7_4:                                # %land.end
	movzbl	19(%r14), %eax
	testq	$32, %rax
	je	.LBB7_8
# BB#5:                                 # %land.rhs7
	movq	PL_curcop(%rip), %rax
	movzbl	37(%rax), %eax
	testq	$8, %rax
	jne	.LBB7_6
# BB#7:                                 # %land.rhs7
	movabsq	$1, %r12
	jmp	.LBB7_8
.LBB7_6:
	movabsq	$0, %r12
.LBB7_8:                                # %land.end13
	testq	%r12, %r12
	jne	.LBB7_11
# BB#9:                                 # %land.end13
	movq	%r13, %rax
	xorq	$1, %rax
	jne	.LBB7_11
# BB#10:                                # %if.then
	movq	$2, %rsi
	movq	%r14, %rdi
	jmp	.LBB7_13
.LBB7_11:                               # %if.else
	movq	%r12, %rax
	xorq	$1, %rax
	orq	%r13, %rax
	jne	.LBB7_14
# BB#12:                                # %if.then23
	movq	$2, %rsi
	movq	%rbx, %rdi
.LBB7_13:                               # %if.end25
	callq	Perl_sv_utf8_upgrade_flags
.LBB7_14:                               # %if.end25
	cmpq	%rbx, %r15
	jne	.LBB7_17
# BB#15:                                # %lor.lhs.false
	cmpq	$91, 88(%rsp)           # 8-byte Folded Reload
	je	.LBB7_18
# BB#16:                                # %land.lhs.true29
	movl	16(%rbx), %eax
	testq	$118431744, %rax        # imm = 0x70F2000
	jne	.LBB7_18
.LBB7_17:                               # %if.then37
	movabsq	$.L.str, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	callq	Perl_sv_setpvn
.LBB7_18:                               # %if.end38
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	je	.LBB7_20
# BB#19:                                # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 120(%rsp)
	movq	(%rax), %rax
	jmp	.LBB7_21
.LBB7_20:                               # %cond.false
	leaq	120(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
.LBB7_21:                               # %cond.end
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB7_23
# BB#22:                                # %cond.true49
	movq	(%r14), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 112(%rsp)
	movq	(%rax), %rax
	jmp	.LBB7_24
.LBB7_23:                               # %cond.false54
	leaq	112(%rsp), %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_2pv_flags
	movq	112(%rsp), %rdx
.LBB7_24:                               # %cond.end56
	movq	%rax, 80(%rsp)          # 8-byte Spill
	cmpq	%rbx, %r15
	movq	%r15, %rbx
	jne	.LBB7_25
# BB#26:                                # %cond.end56
	movabsq	$1, %rcx
	jmp	.LBB7_27
.LBB7_25:
	movabsq	$0, %rcx
.LBB7_27:                               # %cond.end56
	movq	120(%rsp), %rsi
	cmpq	%rdx, %rsi
	movq	%rsi, %r11
	jb	.LBB7_29
# BB#28:                                # %cond.end56
	movq	%rdx, %r11
.LBB7_29:                               # %cond.end56
	cmpq	%r14, %rbx
	movq	88(%rsp), %r15          # 8-byte Reload
	jne	.LBB7_30
# BB#31:                                # %cond.end56
	movabsq	$1, %rax
	jmp	.LBB7_32
.LBB7_30:
	movabsq	$0, %rax
.LBB7_32:                               # %cond.end56
	orq	%rax, %rcx
	orq	%r12, %r13
	je	.LBB7_37
# BB#33:                                # %cond.end56
	testq	%rcx, %rcx
	je	.LBB7_37
# BB#34:                                # %if.then75
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %r14
	cmpq	$91, %r15
	movq	%r15, %r13
	movq	%r11, %rax
	movq	%r11, %r15
	je	.LBB7_36
# BB#35:                                # %if.then75
	leaq	(%rdx,%rsi), %rax
.LBB7_36:                               # %if.then75
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	leaq	1(%rax), %rbx
	movq	%rbx, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	callq	memset
	leaq	16(%r14), %rcx
	movq	%r15, %r11
	movq	%r13, %r15
	jmp	.LBB7_59
.LBB7_37:                               # %if.else86
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	16(%rbx), %rax
	leaq	16(%rbx), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	testq	$118423552, %rax        # imm = 0x70F0000
	jne	.LBB7_39
# BB#38:                                # %if.else86
	movq	%rax, %rcx
	andq	$248, %rcx
	cmpq	$8, %rcx
	jae	.LBB7_39
# BB#55:                                # %if.else157
	movq	%r13, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %r12
	cmpq	$91, %r15
	movq	%r11, %rax
	movq	%r11, 8(%rsp)           # 8-byte Spill
	je	.LBB7_58
# BB#56:                                # %cond.false161
	cmpq	%rdx, %rsi
	movq	%rsi, %rax
	ja	.LBB7_58
# BB#57:                                # %cond.false161
	movq	%rdx, %rax
.LBB7_58:                               # %cond.end168
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	leaq	1(%rax), %rbx
	movq	%rbx, %rdi
	movq	%rax, %r13
	callq	Perl_safesysmalloc
	movq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	memset
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rdi, %r14
	movq	%r13, %rdx
	callq	Perl_sv_usepvn
	movq	(%r14), %rax
	movq	(%rax), %r12
	movq	8(%rsp), %r11           # 8-byte Reload
	movq	48(%rsp), %rcx          # 8-byte Reload
.LBB7_59:                               # %if.end178
	movq	72(%rsp), %r13          # 8-byte Reload
.LBB7_60:                               # %if.end178
	movq	(%r14), %rax
	movq	%r11, 8(%rax)
	movq	(%rcx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, (%rcx)
	testq	$1, %r13
	je	.LBB7_96
# BB#61:                                # %if.then189
	movq	120(%rsp), %rdx
	movq	112(%rsp), %r13
	cmpq	$91, %r15
	movq	64(%rsp), %rbx          # 8-byte Reload
	je	.LBB7_74
# BB#62:                                # %if.then189
	cmpq	$92, %r15
	jne	.LBB7_63
# BB#68:                                # %while.cond221.preheader
	movq	%r14, 24(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	je	.LBB7_69
# BB#70:                                # %while.cond221.preheader
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB7_67
# BB#71:
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB7_72:                               # %while.body227
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	$105, %rcx
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	leaq	96(%rsp), %r15
	movq	%r15, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r14
	movq	%rax, 64(%rsp)          # 8-byte Spill
	addq	%r14, %rbx
	movq	$105, %rcx
	movq	80(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r15
	addq	%r15, %r12
	movq	%r12, 80(%rsp)          # 8-byte Spill
	movq	%r13, %r12
	subq	%r15, %r12
	movq	64(%rsp), %rcx          # 8-byte Reload
	xorq	%rcx, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_uvuni_to_utf8
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	subq	%r14, %rdx
	je	.LBB7_86
# BB#73:                                # %while.body227
                                        #   in Loop: Header=BB7_72 Depth=1
	cmpq	%r15, %r13
	movq	%r12, %r13
	jne	.LBB7_72
	jmp	.LBB7_86
.LBB7_96:                               # %if.else281
	cmpq	$32, %r11
	movq	80(%rsp), %rbx          # 8-byte Reload
	jb	.LBB7_97
# BB#98:                                # %land.lhs.true284
	testq	$7, %r12
	je	.LBB7_99
.LBB7_97:
	movq	%r14, %r13
	movq	%r11, %rsi
	movq	%rbx, %r10
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rax
.LBB7_130:                              # %if.end361
	cmpq	$91, %r15
	je	.LBB7_174
# BB#131:                               # %if.end361
	cmpq	$92, %r15
	jne	.LBB7_132
# BB#155:                               # %while.cond376.preheader
	testq	%rsi, %rsi
	je	.LBB7_153
# BB#156:                               # %while.body379.preheader
	xorq	%r8, %r8
	movq	%rsi, %rcx
	andq	$-4, %rcx
	movq	%rcx, %r15
	je	.LBB7_157
# BB#158:                               # %vector.memcheck812
	movq	%r11, 8(%rsp)           # 8-byte Spill
	movq	%r13, 24(%rsp)          # 8-byte Spill
	leaq	-1(%rsi), %rdx
	leaq	-1(%r12,%rsi), %rcx
	leaq	(%r10,%rdx), %rbx
	cmpq	%rcx, %r10
	movabsq	$0, %rdi
	movabsq	$1, %r9
	movq	%rdi, %r11
	ja	.LBB7_160
# BB#159:                               # %vector.memcheck812
	movq	%r9, %r11
.LBB7_160:                              # %vector.memcheck812
	cmpq	%rbx, %r12
	movq	%rdi, %rbx
	ja	.LBB7_162
# BB#161:                               # %vector.memcheck812
	movq	%r9, %rbx
.LBB7_162:                              # %vector.memcheck812
	addq	%rax, %rdx
	cmpq	%rcx, %rax
	movq	%rdi, %rcx
	ja	.LBB7_164
# BB#163:                               # %vector.memcheck812
	movq	%r9, %rcx
.LBB7_164:                              # %vector.memcheck812
	cmpq	%rdx, %r12
	ja	.LBB7_166
# BB#165:                               # %vector.memcheck812
	movq	%r9, %rdi
.LBB7_166:                              # %vector.memcheck812
	testq	%rcx, %rdi
	jne	.LBB7_167
# BB#168:                               # %vector.memcheck812
	andq	%r11, %rbx
	jne	.LBB7_167
# BB#169:                               # %vector.body792.preheader
	movq	%r15, 88(%rsp)          # 8-byte Spill
	leaq	(%r12,%r15), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	leaq	(%rax,%r15), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	leaq	(%r10,%r15), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %rdi
	subq	%r15, %rdi
	movq	%rsi, %rbx
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	andq	$-4, %rbx
	.align	16, 0x90
.LBB7_170:                              # %vector.body792
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rax), %r15d
	movq	%r12, %r14
	movzbl	2(%rax), %r12d
	movzbl	1(%rax), %r13d
	movzbl	(%rax), %edx
	movzbl	3(%r10), %ecx
	movzbl	2(%r10), %r8d
	movzbl	1(%r10), %r9d
	movzbl	(%r10), %r11d
	xorq	%rdx, %r11
	xorq	%r13, %r9
	xorq	%r12, %r8
	movq	%r14, %r12
	xorq	%r15, %rcx
	movb	%cl, 3(%r12)
	movb	%r8b, 2(%r12)
	movb	%r9b, 1(%r12)
	movb	%r11b, (%r12)
	addq	$4, %r10
	addq	$4, %rax
	addq	$4, %r12
	addq	$-4, %rbx
	jne	.LBB7_170
# BB#171:
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	88(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %r11           # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB7_172
.LBB7_39:                               # %if.then95
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	andq	$10223616, %rax         # imm = 0x9C0000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB7_41
# BB#40:                                # %cond.true100
	movq	(%rbx), %rcx
	movq	%rbx, %r14
	movq	8(%rcx), %rax
	movq	%rax, 104(%rsp)
	movq	(%rcx), %r12
	jmp	.LBB7_42
.LBB7_74:                               # %while.cond.preheader
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	testq	%rdx, %rdx
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	80(%rsp), %r14          # 8-byte Reload
	je	.LBB7_79
# BB#75:                                # %while.cond.preheader
	testq	%r13, %r13
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	je	.LBB7_79
# BB#76:
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB7_77:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	$105, %rcx
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	leaq	96(%rsp), %r12
	movq	%r12, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r15
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$105, %rcx
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r12
	movq	80(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_uvuni_to_utf8
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	subq	%r15, %rdx
	je	.LBB7_79
# BB#78:                                # %while.body
                                        #   in Loop: Header=BB7_77 Depth=1
	addq	%r15, %rbx
	addq	%r12, %r14
	subq	%r12, %r13
	jne	.LBB7_77
.LBB7_79:                               # %while.end
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB7_81
# BB#80:                                # %if.then215
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	Perl_sv_usepvn
.LBB7_81:                               # %if.end216
	movq	(%r14), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	subq	%rbx, %rcx
	movq	%rcx, 8(%rax)
	jmp	.LBB7_94
.LBB7_63:                               # %if.then189
	cmpq	$93, %r15
	jne	.LBB7_95
# BB#64:                                # %while.cond237.preheader
	testq	%rdx, %rdx
	je	.LBB7_65
# BB#66:                                # %while.cond237.preheader
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	testq	%r13, %r13
	je	.LBB7_67
# BB#82:
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB7_83:                               # %while.body243
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	$105, %rcx
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	leaq	96(%rsp), %r14
	movq	%r14, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r15
	movq	%rax, 56(%rsp)          # 8-byte Spill
	addq	%r15, %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	$105, %rcx
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	Perl_utf8n_to_uvuni
	movq	96(%rsp), %r14
	addq	%r14, %rbx
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	movq	%r13, %r12
	subq	%r14, %r12
	movq	56(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	Perl_uvuni_to_utf8
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
	subq	%r15, %rdx
	je	.LBB7_84
# BB#85:                                # %while.body243
                                        #   in Loop: Header=BB7_83 Depth=1
	cmpq	%r14, %r13
	movq	%r12, %r13
	movq	64(%rsp), %rbx          # 8-byte Reload
	jne	.LBB7_83
	jmp	.LBB7_86
.LBB7_174:                              # %while.cond363.preheader
	testq	%rsi, %rsi
	je	.LBB7_196
# BB#175:                               # %while.body366.preheader
	xorq	%r8, %r8
	movq	%rsi, %r14
	andq	$-4, %r14
	je	.LBB7_176
# BB#177:                               # %vector.memcheck891
	movq	%r13, 24(%rsp)          # 8-byte Spill
	leaq	-1(%rsi), %rdx
	leaq	-1(%r12,%rsi), %rcx
	leaq	(%r10,%rdx), %rbx
	cmpq	%rcx, %r10
	movabsq	$0, %rdi
	movabsq	$1, %r9
	movq	%rdi, %r11
	ja	.LBB7_179
# BB#178:                               # %vector.memcheck891
	movq	%r9, %r11
.LBB7_179:                              # %vector.memcheck891
	cmpq	%rbx, %r12
	movq	%rdi, %rbx
	ja	.LBB7_181
# BB#180:                               # %vector.memcheck891
	movq	%r9, %rbx
.LBB7_181:                              # %vector.memcheck891
	addq	%rax, %rdx
	cmpq	%rcx, %rax
	movq	%rdi, %rcx
	ja	.LBB7_183
# BB#182:                               # %vector.memcheck891
	movq	%r9, %rcx
.LBB7_183:                              # %vector.memcheck891
	cmpq	%rdx, %r12
	ja	.LBB7_185
# BB#184:                               # %vector.memcheck891
	movq	%r9, %rdi
.LBB7_185:                              # %vector.memcheck891
	testq	%rcx, %rdi
	jne	.LBB7_186
# BB#187:                               # %vector.memcheck891
	andq	%r11, %rbx
	jne	.LBB7_186
# BB#188:                               # %vector.body871.preheader
	leaq	(%r12,%r14), %rcx
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	leaq	(%rax,%r14), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	leaq	(%r10,%r14), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %rdi
	subq	%r14, %rdi
	movq	%rsi, %rbx
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	andq	$-4, %rbx
	.align	16, 0x90
.LBB7_189:                              # %vector.body871
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rax), %r15d
	movq	%r12, %rsi
	movzbl	2(%rax), %r12d
	movzbl	1(%rax), %r13d
	movzbl	(%rax), %edx
	movzbl	3(%r10), %ecx
	movzbl	2(%r10), %r8d
	movzbl	1(%r10), %r9d
	movzbl	(%r10), %r11d
	andq	%rdx, %r11
	andq	%r13, %r9
	andq	%r12, %r8
	movq	%rsi, %r12
	andq	%r15, %rcx
	movb	%cl, 3(%r12)
	movb	%r8b, 2(%r12)
	movb	%r9b, 1(%r12)
	movb	%r11b, (%r12)
	addq	$4, %r10
	addq	$4, %rax
	addq	$4, %r12
	addq	$-4, %rbx
	jne	.LBB7_189
# BB#190:
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	64(%rsp), %r10          # 8-byte Reload
	movq	%r14, %r8
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB7_191
.LBB7_132:                              # %if.end361
	cmpq	$93, %r15
	jne	.LBB7_196
# BB#133:                               # %while.cond389.preheader
	testq	%rsi, %rsi
	je	.LBB7_153
# BB#134:                               # %while.body392.preheader
	xorq	%r8, %r8
	movq	%rsi, %rcx
	andq	$-4, %rcx
	je	.LBB7_135
# BB#136:                               # %vector.memcheck
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%r11, 8(%rsp)           # 8-byte Spill
	movq	%r13, 24(%rsp)          # 8-byte Spill
	leaq	-1(%rsi), %rdx
	leaq	-1(%r12,%rsi), %rcx
	movq	%rsi, %r15
	leaq	(%r10,%rdx), %rbx
	cmpq	%rcx, %r10
	movabsq	$0, %rdi
	movabsq	$1, %r9
	movq	%rdi, %r11
	ja	.LBB7_138
# BB#137:                               # %vector.memcheck
	movq	%r9, %r11
.LBB7_138:                              # %vector.memcheck
	cmpq	%rbx, %r12
	movq	%rdi, %rbx
	ja	.LBB7_140
# BB#139:                               # %vector.memcheck
	movq	%r9, %rbx
.LBB7_140:                              # %vector.memcheck
	movq	%r14, %rsi
	addq	%rax, %rdx
	cmpq	%rcx, %rax
	movq	%rdi, %rcx
	ja	.LBB7_142
# BB#141:                               # %vector.memcheck
	movq	%r9, %rcx
.LBB7_142:                              # %vector.memcheck
	cmpq	%rdx, %r12
	movq	%rsi, %r14
	ja	.LBB7_144
# BB#143:                               # %vector.memcheck
	movq	%r9, %rdi
.LBB7_144:                              # %vector.memcheck
	testq	%rcx, %rdi
	jne	.LBB7_145
# BB#146:                               # %vector.memcheck
	andq	%r11, %rbx
	movq	%r15, %rsi
	jne	.LBB7_147
# BB#148:                               # %vector.body.preheader
	movq	%rsi, %rdi
	movq	88(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rdi
	movq	%rsi, %rdx
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	andq	$-4, %rdx
	leaq	(%r12,%rcx), %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	leaq	(%rax,%rcx), %rsi
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	leaq	(%r10,%rcx), %rsi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rcx, %rsi
	.align	16, 0x90
.LBB7_149:                              # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	3(%rax), %r15d
	movq	%r12, %r14
	movzbl	2(%rax), %r12d
	movzbl	1(%rax), %r13d
	movzbl	(%rax), %ebx
	movzbl	3(%r10), %ecx
	movzbl	2(%r10), %r8d
	movzbl	1(%r10), %r9d
	movzbl	(%r10), %r11d
	orq	%rbx, %r11
	orq	%r13, %r9
	orq	%r12, %r8
	movq	%r14, %r12
	orq	%r15, %rcx
	movb	%cl, 3(%r12)
	movb	%r8b, 2(%r12)
	movb	%r9b, 1(%r12)
	movb	%r11b, (%r12)
	addq	$4, %r10
	addq	$4, %rax
	addq	$4, %r12
	addq	$-4, %rdx
	jne	.LBB7_149
# BB#150:
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	%rsi, %r8
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %r11           # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB7_151
.LBB7_41:                               # %cond.false105
	leaq	104(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r11, %r14
	callq	Perl_sv_pvn_force_flags
	movq	%r14, %r11
	movq	(%rbx), %rcx
	movq	%rbx, %r14
	movq	%rax, %r12
.LBB7_42:                               # %cond.end107
	movq	8(%rcx), %rax
	cmpq	%r11, %rax
	jae	.LBB7_47
# BB#43:                                # %if.then113
	movq	16(%rcx), %rax
	leaq	1(%r11), %rbx
	movq	%r11, %r15
	cmpq	%rbx, %rax
	jae	.LBB7_45
# BB#44:                                # %cond.true118
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_grow
	movq	%rax, %r12
	jmp	.LBB7_46
.LBB7_99:                               # %land.lhs.true286
	movq	%r14, %r13
	movq	64(%rsp), %rcx          # 8-byte Reload
	testq	$7, %rcx
	je	.LBB7_101
# BB#100:
	movq	%r11, %rsi
	movq	%rbx, %r10
	movq	%rcx, %rax
	movq	%rcx, %r14
	jmp	.LBB7_130
.LBB7_45:                               # %cond.false121
	movq	(%rcx), %r12
.LBB7_46:                               # %cond.end124
	movq	(%r14), %rax
	movq	8(%rax), %rax
	leaq	(%r12,%rax), %rdi
	subq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	movq	%r15, %r11
	movq	88(%rsp), %r15          # 8-byte Reload
.LBB7_47:                               # %if.end131
	cmpq	$91, %r15
	je	.LBB7_48
# BB#49:                                # %if.end131
	movabsq	$1, %rax
	jmp	.LBB7_50
.LBB7_48:
	movabsq	$0, %rax
.LBB7_50:                               # %if.end131
	movq	$0, 32(%rsp)            # 8-byte Folded Spill
	testq	%r13, %rax
	je	.LBB7_51
# BB#52:                                # %if.then140
	movq	(%r14), %rax
	movq	120(%rsp), %rcx
	movq	112(%rsp), %rdx
	movq	16(%rax), %rdi
	leaq	1(%rcx,%rdx), %rsi
	cmpq	%rsi, %rdi
	jae	.LBB7_54
# BB#53:                                # %cond.true147
	movq	%r14, %rdi
	movq	%r11, %rbx
	callq	Perl_sv_grow
	movq	%rbx, %r11
	movq	%rax, %r12
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB7_60
.LBB7_51:
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB7_60
.LBB7_54:                               # %cond.false151
	movq	(%rax), %r12
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB7_60
.LBB7_69:
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB7_67
.LBB7_157:
	movq	%rsi, %rdi
	jmp	.LBB7_172
.LBB7_176:
	movq	%rsi, %rdi
	jmp	.LBB7_191
.LBB7_101:                              # %land.lhs.true289
	movq	%rcx, %rdx
	movq	%r11, %rsi
	testq	$7, %rbx
	jne	.LBB7_102
# BB#103:                               # %if.then292
	andq	$31, %rsi
	movq	$5, %rcx
	movq	%r11, %rax
	shrq	%cl, %rax
	cmpq	$91, %r15
	je	.LBB7_122
# BB#104:                               # %if.then292
	cmpq	$92, %r15
	movq	%r13, %rdi
	jne	.LBB7_105
# BB#115:                               # %while.cond316.preheader
	testq	%rax, %rax
	je	.LBB7_106
# BB#116:                               # %while.body319.lr.ph
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	notq	%rcx
	movq	56(%rsp), %r9           # 8-byte Reload
	notq	%r9
	cmpq	%r9, %rcx
	movq	%rcx, %rax
	ja	.LBB7_118
# BB#117:                               # %while.body319.lr.ph
	movq	%r9, %rax
.LBB7_118:                              # %while.body319.lr.ph
	movq	%r11, %rsi
	notq	%rax
	andq	$-32, %rax
	cmpq	%rcx, %r9
	ja	.LBB7_120
# BB#119:                               # %while.body319.lr.ph
	movq	%rcx, %r9
.LBB7_120:                              # %while.body319.lr.ph
	movq	%r12, %rdi
	addq	%rax, %r12
	leaq	(%rbx,%rax), %r10
	movq	%rdx, %r14
	addq	%r14, %rax
	notq	%r9
	movq	$5, %rcx
	shrq	%cl, %r9
	negq	%r9
	movq	%r14, %rcx
	movq	%rbx, %rdx
	movq	%rbx, %r11
.LBB7_121:                              # %while.body319
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %r8
	movq	(%rdx), %rbx
	xorq	%r8, %rbx
	movq	%rbx, (%rdi)
	movq	8(%rcx), %r8
	movq	8(%rdx), %rbx
	xorq	%r8, %rbx
	movq	%rbx, 8(%rdi)
	movq	16(%rcx), %r8
	movq	16(%rdx), %rbx
	xorq	%r8, %rbx
	movq	%rbx, 16(%rdi)
	movq	24(%rcx), %r8
	movq	24(%rdx), %rbx
	xorq	%r8, %rbx
	movq	%rbx, 24(%rdi)
	addq	$32, %rcx
	addq	$32, %rdx
	addq	$32, %rdi
	incq	%r9
	jne	.LBB7_121
	jmp	.LBB7_129
.LBB7_167:
	movq	%rsi, %rdi
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %r11           # 8-byte Reload
.LBB7_172:                              # %middle.block793
	cmpq	%r8, %rsi
	je	.LBB7_153
	.align	16, 0x90
.LBB7_173:                              # %while.body379
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	movzbl	(%r10), %edx
	incq	%rax
	incq	%r10
	xorq	%rcx, %rdx
	decq	%rdi
	movb	%dl, (%r12)
	leaq	1(%r12), %r12
	jne	.LBB7_173
	jmp	.LBB7_153
.LBB7_186:
	movq	%rsi, %rdi
	movq	24(%rsp), %r13          # 8-byte Reload
.LBB7_191:                              # %middle.block872
	cmpq	%r8, %rsi
	je	.LBB7_196
	.align	16, 0x90
.LBB7_192:                              # %while.body366
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	movzbl	(%r10), %edx
	incq	%rax
	incq	%r10
	andq	%rcx, %rdx
	decq	%rdi
	movb	%dl, (%r12)
	leaq	1(%r12), %r12
	jne	.LBB7_192
	jmp	.LBB7_196
.LBB7_65:
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
.LBB7_67:
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	%r12, 16(%rsp)          # 8-byte Spill
	movq	%r13, %r12
.LBB7_86:                               # %mop_up_utf
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	movq	24(%rsp), %r14          # 8-byte Reload
	je	.LBB7_88
# BB#87:                                # %if.then258
	movq	%r14, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rdx, %r15
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	Perl_sv_usepvn
	movq	%r15, %rdx
.LBB7_88:                               # %if.end259
	movq	(%r14), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	subq	%rcx, %rsi
	movq	%rsi, 8(%rax)
	testq	%r12, %r12
	je	.LBB7_90
# BB#89:                                # %if.then267
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%r12, %rdx
	callq	Perl_sv_catpvn_flags
	jmp	.LBB7_94
.LBB7_90:                               # %if.else268
	testq	%rdx, %rdx
	je	.LBB7_93
# BB#91:                                # %if.then270
	movq	$2, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB7_94
.LBB7_93:                               # %if.else271
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rsi)
.LBB7_94:                               # %sw.epilog
	movq	48(%rsp), %rcx          # 8-byte Reload
.LBB7_95:                               # %sw.epilog
	movq	%r14, %r13
	orq	$536870912, (%rcx)      # imm = 0x20000000
	jmp	.LBB7_196
.LBB7_84:
	movq	64(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB7_86
.LBB7_135:
	movq	%rsi, %rdi
	jmp	.LBB7_151
.LBB7_145:
	movq	%r15, %rsi
.LBB7_147:
	movq	%rsi, %rdi
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	80(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %r11           # 8-byte Reload
.LBB7_151:                              # %middle.block
	cmpq	%r8, %rsi
	je	.LBB7_153
	.align	16, 0x90
.LBB7_152:                              # %while.body392
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	movzbl	(%r10), %edx
	incq	%rax
	incq	%r10
	orq	%rcx, %rdx
	decq	%rdi
	movb	%dl, (%r12)
	leaq	1(%r12), %r12
	jne	.LBB7_152
.LBB7_153:                              # %mop_up
	movq	112(%rsp), %rdx
	subq	%r11, %rdx
	jbe	.LBB7_193
# BB#154:                               # %if.then403
	addq	%r11, %rbx
	movq	$2, %rcx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB7_196
.LBB7_193:                              # %if.else406
	movq	120(%rsp), %rdx
	subq	%r11, %rdx
	jbe	.LBB7_195
# BB#194:                               # %if.then409
	addq	%r11, %r14
	movq	$2, %rcx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_catpvn_flags
	jmp	.LBB7_196
.LBB7_195:                              # %if.else412
	movq	(%r13), %rax
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	xorq	%rdx, %rdx
	movb	%dl, (%rcx,%rax)
.LBB7_196:                              # %finish
	movzbl	PL_tainting(%rip), %eax
	testq	%rax, %rax
	je	.LBB7_199
# BB#197:                               # %finish
	movzbl	PL_tainted(%rip), %eax
	testq	$255, %rax
	je	.LBB7_199
# BB#198:                               # %if.then426
	movq	%r13, %rdi
	callq	Perl_sv_taint
.LBB7_199:                              # %if.end429
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_122:                              # %while.cond296.preheader
	testq	%rax, %rax
	je	.LBB7_102
# BB#123:                               # %while.body298.lr.ph
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	notq	%rcx
	movq	56(%rsp), %r9           # 8-byte Reload
	notq	%r9
	cmpq	%r9, %rcx
	movq	%rcx, %rax
	ja	.LBB7_125
# BB#124:                               # %while.body298.lr.ph
	movq	%r9, %rax
.LBB7_125:                              # %while.body298.lr.ph
	movq	%r11, %rsi
	notq	%rax
	andq	$-32, %rax
	cmpq	%rcx, %r9
	ja	.LBB7_127
# BB#126:                               # %while.body298.lr.ph
	movq	%rcx, %r9
.LBB7_127:                              # %while.body298.lr.ph
	movq	%r12, %rdi
	addq	%rax, %r12
	leaq	(%rbx,%rax), %r10
	movq	%rdx, %r14
	addq	%r14, %rax
	notq	%r9
	movq	$5, %rcx
	shrq	%cl, %r9
	negq	%r9
	movq	%r14, %rcx
	movq	%rbx, %rdx
	movq	%rbx, %r11
.LBB7_128:                              # %while.body298
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %r8
	movq	(%rdx), %rbx
	andq	%r8, %rbx
	movq	%rbx, (%rdi)
	movq	8(%rcx), %r8
	movq	8(%rdx), %rbx
	andq	%r8, %rbx
	movq	%rbx, 8(%rdi)
	movq	16(%rcx), %r8
	movq	16(%rdx), %rbx
	andq	%r8, %rbx
	movq	%rbx, 16(%rdi)
	movq	24(%rcx), %r8
	movq	24(%rdx), %rbx
	andq	%r8, %rbx
	movq	%rbx, 24(%rdi)
	addq	$32, %rcx
	addq	$32, %rdx
	addq	$32, %rdi
	incq	%r9
	jne	.LBB7_128
.LBB7_129:                              # %while.cond316.sw.epilog359.loopexit646_crit_edge
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%r11, %rbx
	movq	%rsi, %r11
	movq	72(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB7_130
.LBB7_105:                              # %if.then292
	cmpq	$93, %r15
	movq	56(%rsp), %rcx          # 8-byte Reload
	jne	.LBB7_106
# BB#107:                               # %while.cond338.preheader
	testq	%rax, %rax
	je	.LBB7_106
# BB#108:                               # %while.body341.lr.ph
	movq	%rsi, %r13
	movq	16(%rsp), %rsi          # 8-byte Reload
	notq	%rsi
	notq	%rcx
	cmpq	%rcx, %rsi
	movq	%rsi, %rax
	movq	%rsi, %r8
	ja	.LBB7_110
# BB#109:                               # %while.body341.lr.ph
	movq	%rcx, %rax
.LBB7_110:                              # %while.body341.lr.ph
	movq	%r11, 8(%rsp)           # 8-byte Spill
	movq	%rcx, %rsi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	notq	%rax
	andq	$-32, %rax
	cmpq	%r8, %rsi
	movq	%r13, %r9
	ja	.LBB7_112
# BB#111:                               # %while.body341.lr.ph
	movq	%r8, %rsi
.LBB7_112:                              # %while.body341.lr.ph
	movq	%r12, %rdi
	addq	%rax, %r12
	leaq	(%rbx,%rax), %r10
	movq	%rdx, %r14
	addq	%r14, %rax
	notq	%rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	negq	%rsi
	movq	%r14, %rcx
	movq	%rbx, %rdx
	movq	%rbx, %r11
.LBB7_113:                              # %while.body341
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %r8
	movq	(%rdx), %rbx
	orq	%r8, %rbx
	movq	%rbx, (%rdi)
	movq	8(%rcx), %r8
	movq	8(%rdx), %rbx
	orq	%r8, %rbx
	movq	%rbx, 8(%rdi)
	movq	16(%rcx), %r8
	movq	16(%rdx), %rbx
	orq	%r8, %rbx
	movq	%rbx, 16(%rdi)
	movq	24(%rcx), %r8
	movq	24(%rdx), %rbx
	orq	%r8, %rbx
	movq	%rbx, 24(%rdi)
	addq	$32, %rcx
	addq	$32, %rdx
	addq	$32, %rdi
	incq	%rsi
	jne	.LBB7_113
# BB#114:                               # %while.cond338.sw.epilog359.loopexit647_crit_edge
	movq	24(%rsp), %r13          # 8-byte Reload
	movq	%r11, %rbx
	movq	8(%rsp), %r11           # 8-byte Reload
	movq	%r9, %rsi
	jmp	.LBB7_130
.LBB7_106:
	movq	%rdi, %r13
.LBB7_102:
	movq	%rbx, %r10
	movq	%rdx, %r14
	movq	%r14, %rax
	jmp	.LBB7_130
.Ltmp128:
	.size	Perl_do_vop, .Ltmp128-Perl_do_vop
	.cfi_endproc

	.globl	Perl_do_kv
	.align	16, 0x90
	.type	Perl_do_kv,@function
Perl_do_kv:                             # @Perl_do_kv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp137:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp138:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp140:
	.cfi_def_cfa_offset 80
.Ltmp141:
	.cfi_offset %rbx, -48
.Ltmp142:
	.cfi_offset %r12, -40
.Ltmp143:
	.cfi_offset %r13, -32
.Ltmp144:
	.cfi_offset %r14, -24
.Ltmp145:
	.cfi_offset %r15, -16
	movq	PL_op(%rip), %rcx
	movq	PL_stack_sp(%rip), %r15
	movzbl	36(%rcx), %eax
	movq	(%r15), %r14
	andq	$3, %rax
	cmpq	$1, %rax
	movq	$128, %rbx
	je	.LBB8_4
# BB#1:                                 # %cond.false
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	xorq	%rbx, %rbx
	cmpq	$2, %rax
	je	.LBB8_4
# BB#2:                                 # %cond.false9
	movq	$1, %rbx
	cmpq	$3, %rax
	je	.LBB8_4
# BB#3:                                 # %cond.false16
	callq	Perl_block_gimme
	movq	PL_op(%rip), %rcx
	movq	%rax, %rbx
.LBB8_4:                                # %cond.end19
	movzbl	32(%rcx), %eax
	movzbl	16(%r14), %edx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	leaq	-8(%r15), %rdi
	cmpq	$134, %rax
	je	.LBB8_10
# BB#5:                                 # %lor.lhs.false
	cmpq	$130, %rax
	movabsq	$0, %r13
	movabsq	$1, %rdx
	movq	%r13, 16(%rsp)          # 8-byte Spill
	jne	.LBB8_7
# BB#6:                                 # %lor.lhs.false
	movq	%rdx, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
.LBB8_7:                                # %lor.lhs.false
	cmpq	$131, %rax
	jne	.LBB8_9
# BB#8:                                 # %lor.lhs.false
	movq	%rdx, %r13
.LBB8_9:                                # %lor.lhs.false
	cmpq	$11, %rax
	jne	.LBB8_11
.LBB8_10:                               # %if.then
	movq	$1, 16(%rsp)            # 8-byte Folded Spill
	movq	$1, %r13
.LBB8_11:                               # %if.end
	testq	%r14, %r14
	je	.LBB8_12
# BB#21:                                # %if.end62
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	cmpq	$11, 24(%rsp)           # 8-byte Folded Reload
	movq	%r14, %r12
	je	.LBB8_23
# BB#22:                                # %cond.false65
	movq	%r14, %rdi
	callq	Perl_avhv_keys
	movq	%rax, %r12
.LBB8_23:                               # %cond.end67
	movq	%r12, %rdi
	callq	Perl_hv_iterinit
	testq	%rbx, %rbx
	jne	.LBB8_24
# BB#26:                                # %if.then77
	movq	PL_op(%rip), %rax
	movq	24(%rax), %rcx
	movq	PL_curpad(%rip), %rdx
	movzbl	36(%rax), %esi
	movq	(%rdx,%rcx,8), %r14
	testq	$32, %rsi
	jne	.LBB8_29
# BB#27:                                # %lor.lhs.false85
	movzbl	37(%rax), %eax
	testq	$8, %rax
	je	.LBB8_38
# BB#28:                                # %land.lhs.true90
	callq	Perl_is_lvalue_sub
	testq	%rax, %rax
	je	.LBB8_38
.LBB8_29:                               # %if.then93
	movzbl	16(%r14), %eax
	cmpq	$8, %rax
	ja	.LBB8_31
# BB#30:                                # %if.then98
	movq	$9, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_upgrade
	xorq	%rsi, %rsi
	movq	$107, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	Perl_sv_magic
.LBB8_31:                               # %if.end100
	movq	(%r14), %rax
	movq	$107, %rcx
	movb	%cl, 80(%rax)
	movq	72(%rax), %rdi
	cmpq	%r12, %rdi
	je	.LBB8_37
# BB#32:                                # %if.then106
	testq	%rdi, %rdi
	je	.LBB8_34
# BB#33:                                # %if.then110
	callq	Perl_sv_free
.LBB8_34:                               # %if.end113
	testq	%r12, %r12
	movq	%r12, PL_Sv(%rip)
	je	.LBB8_36
# BB#35:                                # %land.rhs
	incq	8(%r12)
.LBB8_36:                               # %land.end
	movq	(%r14), %rax
	movq	%r12, 72(%rax)
	jmp	.LBB8_37
.LBB8_12:                               # %if.then42
	movzbl	36(%rcx), %eax
	testq	$32, %rax
	jne	.LBB8_17
# BB#13:                                # %lor.lhs.false47
	movq	%rdi, %rbx
	movzbl	37(%rcx), %eax
	testq	$8, %rax
	je	.LBB8_14
# BB#15:                                # %land.lhs.true
	callq	Perl_is_lvalue_sub
	testq	%rax, %rax
	movq	%rbx, %rcx
	je	.LBB8_20
# BB#16:                                # %land.lhs.true.if.then53_crit_edge
	movq	PL_op(%rip), %rcx
.LBB8_17:                               # %if.then53
	movq	24(%rcx), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %rax
	movzbl	16(%rax), %ecx
	cmpq	$9, %rcx
	jne	.LBB8_19
# BB#18:                                # %if.then58
	movq	(%rax), %rcx
	movq	$0, 72(%rcx)
.LBB8_19:                               # %if.end59
	movq	%rax, (%r15)
	movq	%r15, %rcx
.LBB8_20:                               # %if.end61
	movq	PL_op(%rip), %rax
	movq	%rcx, PL_stack_sp(%rip)
	jmp	.LBB8_70
.LBB8_24:                               # %cond.end67
	cmpq	$128, %rbx
	jne	.LBB8_44
# BB#25:                                # %if.then72
	movq	PL_op(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, PL_stack_sp(%rip)
	jmp	.LBB8_70
.LBB8_44:                               # %if.end145
	movq	(%r12), %rax
	movq	PL_stack_max(%rip), %rbx
	movq	24(%rax), %r15
	movq	32(%rax), %rdi
	movq	8(%rsp), %rax           # 8-byte Reload
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	callq	float64_to_int64
	subq	%rax, %r15
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	(%r13,%rax), %rdx
	imulq	%r15, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB8_45
# BB#46:                                # %if.then156
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, %rsi
	callq	Perl_stack_grow
	jmp	.LBB8_47
.LBB8_45:
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB8_47:                               # %while.cond168.preheader
	movq	%rax, PL_stack_sp(%rip)
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB8_69
# BB#48:                                # %while.body171.lr.ph
	testq	%r13, %r13
	je	.LBB8_53
# BB#49:                                # %while.body171.lr.ph.while.body171.lr.ph.split_crit_edge
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB8_50
	.align	16, 0x90
.LBB8_61:                               # %if.then173
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_sp(%rip), %r15
	movq	%rbx, %rdi
	callq	Perl_hv_iterkeysv
	movq	PL_stack_max(%rip), %rcx
	movq	%rax, %r13
	subq	%r15, %rcx
	cmpq	$7, %rcx
	jg	.LBB8_63
# BB#62:                                # %if.then182
                                        #   in Loop: Header=BB8_61 Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB8_63:                               # %if.then189
                                        #   in Loop: Header=BB8_61 Depth=1
	cmpq	$11, 24(%rsp)           # 8-byte Folded Reload
	movq	%r13, 8(%r15)
	leaq	8(%r15), %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jne	.LBB8_65
# BB#64:                                # %cond.true191
                                        #   in Loop: Header=BB8_61 Depth=1
	callq	Perl_hv_iterval
	jmp	.LBB8_66
	.align	16, 0x90
.LBB8_65:                               # %cond.false193
                                        #   in Loop: Header=BB8_61 Depth=1
	callq	Perl_avhv_iterval
.LBB8_66:                               # %cond.end195
                                        #   in Loop: Header=BB8_61 Depth=1
	movq	%rax, %rbx
	movq	PL_stack_sp(%rip), %rax
	movq	PL_stack_max(%rip), %rcx
	subq	%rax, %rcx
	cmpq	$7, %rcx
	jg	.LBB8_68
# BB#67:                                # %if.then203
                                        #   in Loop: Header=BB8_61 Depth=1
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	Perl_stack_grow
.LBB8_68:                               # %while.cond168.backedge
                                        #   in Loop: Header=BB8_61 Depth=1
	movq	%rbx, 8(%rax)
	addq	$8, %rax
	movq	%rax, PL_stack_sp(%rip)
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB8_61
	jmp	.LBB8_69
	.align	16, 0x90
.LBB8_53:                               # %if.end187.us
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_sp(%rip), %rax
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB8_60
# BB#54:                                # %if.then189.us
                                        #   in Loop: Header=BB8_53 Depth=1
	cmpq	$11, 24(%rsp)           # 8-byte Folded Reload
	movq	%rax, PL_stack_sp(%rip)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jne	.LBB8_55
# BB#56:                                # %cond.true191.us
                                        #   in Loop: Header=BB8_53 Depth=1
	callq	Perl_hv_iterval
	jmp	.LBB8_57
	.align	16, 0x90
.LBB8_55:                               # %cond.false193.us
                                        #   in Loop: Header=BB8_53 Depth=1
	callq	Perl_avhv_iterval
.LBB8_57:                               # %cond.end195.us
                                        #   in Loop: Header=BB8_53 Depth=1
	movq	%rax, %rbx
	movq	PL_stack_sp(%rip), %rax
	movq	PL_stack_max(%rip), %rcx
	subq	%rax, %rcx
	cmpq	$7, %rcx
	jg	.LBB8_59
# BB#58:                                # %if.then203.us
                                        #   in Loop: Header=BB8_53 Depth=1
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	Perl_stack_grow
.LBB8_59:                               # %if.end205.us
                                        #   in Loop: Header=BB8_53 Depth=1
	movq	%rbx, 8(%rax)
	addq	$8, %rax
.LBB8_60:                               # %while.cond168.backedge.us
                                        #   in Loop: Header=BB8_53 Depth=1
	movq	%rax, PL_stack_sp(%rip)
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB8_53
	jmp	.LBB8_69
	.align	16, 0x90
.LBB8_50:                               # %if.then173.us275
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_sp(%rip), %r14
	movq	%rbx, %rdi
	callq	Perl_hv_iterkeysv
	movq	PL_stack_max(%rip), %rcx
	movq	%rax, %rbx
	subq	%r14, %rcx
	cmpq	$7, %rcx
	jg	.LBB8_52
# BB#51:                                # %if.then182.us281
                                        #   in Loop: Header=BB8_50 Depth=1
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r14
.LBB8_52:                               # %while.cond168.backedge.us304
                                        #   in Loop: Header=BB8_50 Depth=1
	movq	%rbx, 8(%r14)
	addq	$8, %r14
	movq	%r14, PL_stack_sp(%rip)
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB8_50
.LBB8_69:                               # %while.end209
	movq	PL_op(%rip), %rax
	jmp	.LBB8_70
.LBB8_38:                               # %if.end121
	movzbl	17(%r12), %eax
	testq	$128, %rax
	je	.LBB8_71
# BB#39:                                # %cond.true125
	movq	$80, %rsi
	movq	%r12, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	je	.LBB8_71
# BB#40:
	movq	$-1, %rbx
	.align	16, 0x90
.LBB8_41:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rdi
	callq	Perl_hv_iternext
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB8_41
	jmp	.LBB8_42
.LBB8_14:
	movq	%rbx, %rcx
	jmp	.LBB8_20
.LBB8_71:                               # %if.then129
	movq	(%r12), %rax
	movq	24(%rax), %rbx
	movq	32(%rax), %rdi
	callq	float64_to_int64
	subq	%rax, %rbx
.LBB8_42:                               # %if.end136
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB8_37
# BB#43:                                # %if.then140
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB8_37:                               # %if.end118
	movq	%r14, (%r15)
	movq	PL_op(%rip), %rax
	movq	%r15, PL_stack_sp(%rip)
.LBB8_70:                               # %return
	movq	(%rax), %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp146:
	.size	Perl_do_kv, .Ltmp146-Perl_do_kv
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.zero	1
	.size	.L.str, 1

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Illegal number of bits in vec"
	.size	.L.str1, 30

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Negative offset to vec in lvalue context"
	.size	.L.str2, 41


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
