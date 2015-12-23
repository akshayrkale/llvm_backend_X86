	.file	"Cwd.c.bc"
	.text
	.globl	XS_Cwd_fastcwd
	.align	16, 0x90
	.type	XS_Cwd_fastcwd,@function
XS_Cwd_fastcwd:                         # @XS_Cwd_fastcwd
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
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rdx
	je	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB0_2:                                # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	testq	$32, %rcx
	je	.LBB0_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB0_5
.LBB0_4:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB0_5:                                # %cond.end
	movq	%r14, %rdi
	callq	Perl_getcwd_sv
	movzbl	17(%r14), %eax
	movq	PL_stack_base(%rip), %r15
	testq	$64, %rax
	je	.LBB0_7
# BB#6:                                 # %if.then11
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB0_7:                                # %if.end13
	leaq	8(%r15,%rbx,8), %rax
	movq	%r14, 8(%r15,%rbx,8)
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	XS_Cwd_fastcwd, .Ltmp10-XS_Cwd_fastcwd
	.cfi_endproc

	.globl	XS_Cwd_abs_path
	.align	16, 0x90
	.type	XS_Cwd_abs_path,@function
XS_Cwd_abs_path:                        # @XS_Cwd_abs_path
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
	subq	$12304, %rsp            # imm = 0x3010
.Ltmp22:
	.cfi_def_cfa_offset 12352
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
	movq	PL_stack_sp(%rip), %rbx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	subq	%rax, %rbx
	cmpq	$9, %rbx
	jb	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB1_2:                                # %if.end
	xorq	%r15, %r15
	cmpq	$8, %rbx
	jl	.LBB1_4
# BB#3:                                 # %if.else
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r12,8), %r15
.LBB1_4:                                # %if.end10
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	incq	%r12
	testq	$32, %rcx
	je	.LBB1_6
# BB#5:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB1_7
.LBB1_6:                                # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB1_7:                                # %cond.end
	testq	%r15, %r15
	je	.LBB1_8
# BB#9:                                 # %cond.true13
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB1_11
# BB#10:                                # %cond.true17
	movq	(%r15), %rax
	movq	(%rax), %rbx
	jmp	.LBB1_12
.LBB1_8:
	movabsq	$.L.str2, %rbx
	jmp	.LBB1_12
.LBB1_11:                               # %cond.false18
	movq	%r15, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rbx
.LBB1_12:                               # %cond.end23
	leaq	4112(%rsp), %rdi
	movq	$4095, %rsi             # imm = 0xFFF
	callq	getcwd
	testq	%rax, %rax
	je	.LBB1_13
# BB#14:                                # %if.end.i
	leaq	16(%rsp), %r15
	movq	$4095, %rdx             # imm = 0xFFF
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	strncpy
	xorq	%rax, %rax
	movb	%al, 4111(%rsp)
	movq	$47, %rsi
	movq	%r15, %rdi
	callq	strrchr
	testq	%rax, %rax
	je	.LBB1_22
# BB#15:                                # %if.then5.i
	leaq	1(%rax), %r15
	leaq	16(%rsp), %rdi
	cmpq	%rdi, %rax
	je	.LBB1_16
# BB#17:                                # %do.body.i.preheader
	decq	%rax
	.align	16, 0x90
.LBB1_18:                               # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	cmpq	%rdi, %rcx
	jbe	.LBB1_20
# BB#19:                                # %land.rhs.i
                                        #   in Loop: Header=BB1_18 Depth=1
	movzbl	(%rcx), %edx
	leaq	-1(%rcx), %rax
	cmpq	$47, %rdx
	je	.LBB1_18
.LBB1_20:                               # %do.end.i
	xorq	%rax, %rax
	movb	%al, 1(%rcx)
	jmp	.LBB1_21
.LBB1_13:                               # %if.then.i
	movq	$46, %rax
	movw	%ax, 16(%rsp)
	jmp	.LBB1_37
.LBB1_16:
	movabsq	$.L.str6, %rdi
.LBB1_21:                               # %if.end12.i
	callq	chdir
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_29
.LBB1_22:                               # %if.end19.i
	leaq	8208(%rsp), %rdi
	movq	%r15, %rsi
	callq	strcpy
	leaq	16(%rsp), %rdi
	movq	$4096, %rsi             # imm = 0x1000
	callq	getcwd
	testq	%rax, %rax
	je	.LBB1_29
# BB#23:                                # %if.end26.i
	movzbl	8208(%rsp), %eax
	testq	%rax, %rax
	je	.LBB1_34
# BB#24:                                # %if.then39.i
	movzbl	16(%rsp), %ebx
	movq	%rbx, %rax
	andq	$255, %rax
	andq	$65535, %rbx            # imm = 0xFFFF
	cmpq	$256, %rbx              # imm = 0x100
	sbbq	%r15, %r15
	cmpq	$47, %rax
	jne	.LBB1_25
# BB#26:                                # %if.then39.i
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB1_27
.LBB1_25:
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$0, %rax
.LBB1_27:                               # %if.then39.i
	andq	%rax, %r15
	leaq	16(%rsp), %rdi
	callq	strlen
	movq	%rax, %r13
	leaq	8208(%rsp), %rdi
	callq	strlen
	movq	$2, %rcx
	subq	%r15, %rcx
	addq	%r13, %rcx
	addq	%rax, %rcx
	cmpq	$4097, %rcx             # imm = 0x1001
	jb	.LBB1_30
# BB#28:                                # %if.then48.i
	callq	__errno_location
	movq	$36, %rcx
	movl	%ecx, (%rax)
.LBB1_29:                               # %err1.i
	callq	__errno_location
	movl	(%rax), %ebx
	leaq	4112(%rsp), %rdi
	callq	chdir
.LBB1_36:                               # %err2.i
	callq	__errno_location
	movl	%ebx, (%rax)
.LBB1_37:                               # %if.else37
	movabsq	$PL_sv_undef, %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	callq	Perl_sv_setsv_flags
	leaq	16(%r14), %rbx
.LBB1_40:                               # %if.end38
	movzbl	1(%rbx), %eax
	movq	PL_stack_base(%rip), %rbx
	testq	$64, %rax
	je	.LBB1_42
# BB#41:                                # %if.then44
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB1_42:                               # %if.end46
	leaq	(%rbx,%r12,8), %rax
	movq	%r14, (%rbx,%r12,8)
	movq	%rax, PL_stack_sp(%rip)
	addq	$12304, %rsp            # imm = 0x3010
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_30:                               # %if.end50.i
	cmpq	$47, 8(%rsp)            # 8-byte Folded Reload
	jne	.LBB1_32
# BB#31:                                # %if.end50.i
	cmpq	$256, %rbx              # imm = 0x100
	jb	.LBB1_33
.LBB1_32:                               # %if.then53.i
	movq	$47, %rax
	movw	%ax, 16(%rsp,%r13)
.LBB1_33:                               # %if.end55.i
	leaq	16(%rsp), %rdi
	leaq	8208(%rsp), %rsi
	callq	strcat
.LBB1_34:                               # %if.end58.i
	leaq	4112(%rsp), %rdi
	callq	chdir
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_35
# BB#38:                                # %if.then27
	leaq	16(%rsp), %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_sv_setpvn
	movq	16(%r14), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r14)
	movzbl	PL_tainting(%rip), %eax
	leaq	16(%r14), %rbx
	testq	%rax, %rax
	je	.LBB1_40
# BB#39:                                # %if.then35
	movq	%r14, %rdi
	callq	Perl_sv_taint
	jmp	.LBB1_40
.LBB1_35:                               # %if.then63.i
	callq	__errno_location
	movl	(%rax), %ebx
	jmp	.LBB1_36
.Ltmp28:
	.size	XS_Cwd_abs_path, .Ltmp28-XS_Cwd_abs_path
	.cfi_endproc

	.globl	boot_Cwd
	.align	16, 0x90
	.type	boot_Cwd,@function
boot_Cwd:                               # @boot_Cwd
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp34:
	.cfi_def_cfa_offset 32
.Ltmp35:
	.cfi_offset %rbx, -24
.Ltmp36:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movabsq	$.L.str4, %rdi
	movabsq	$XS_Cwd_fastcwd, %rsi
	movabsq	$.L.str3, %r14
	movq	%r14, %rdx
	callq	Perl_newXS
	movabsq	$.L.str5, %rdi
	movabsq	$XS_Cwd_abs_path, %rsi
	movq	%r14, %rdx
	callq	Perl_newXS
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp37:
	.size	boot_Cwd, .Ltmp37-boot_Cwd
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Cwd::fastcwd()"
	.size	.L.str, 22

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Usage: Cwd::abs_path(pathsv=Nullsv)"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"."
	.size	.L.str2, 2

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Cwd.c"
	.size	.L.str3, 6

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Cwd::fastcwd"
	.size	.L.str4, 13

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Cwd::abs_path"
	.size	.L.str5, 14

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"/"
	.size	.L.str6, 2


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
