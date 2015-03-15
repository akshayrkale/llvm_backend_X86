	.file	"locale.c.bc"
	.text
	.globl	Perl_set_numeric_radix
	.align	16, 0x90
	.type	Perl_set_numeric_radix,@function
Perl_set_numeric_radix:                 # @Perl_set_numeric_radix
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	Perl_set_numeric_radix, .Ltmp0-Perl_set_numeric_radix
	.cfi_endproc

	.globl	Perl_new_numeric
	.align	16, 0x90
	.type	Perl_new_numeric,@function
Perl_new_numeric:                       # @Perl_new_numeric
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp1:
	.size	Perl_new_numeric, .Ltmp1-Perl_new_numeric
	.cfi_endproc

	.globl	Perl_set_numeric_standard
	.align	16, 0x90
	.type	Perl_set_numeric_standard,@function
Perl_set_numeric_standard:              # @Perl_set_numeric_standard
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp2:
	.size	Perl_set_numeric_standard, .Ltmp2-Perl_set_numeric_standard
	.cfi_endproc

	.globl	Perl_set_numeric_local
	.align	16, 0x90
	.type	Perl_set_numeric_local,@function
Perl_set_numeric_local:                 # @Perl_set_numeric_local
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp3:
	.size	Perl_set_numeric_local, .Ltmp3-Perl_set_numeric_local
	.cfi_endproc

	.globl	Perl_new_ctype
	.align	16, 0x90
	.type	Perl_new_ctype,@function
Perl_new_ctype:                         # @Perl_new_ctype
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp4:
	.size	Perl_new_ctype, .Ltmp4-Perl_new_ctype
	.cfi_endproc

	.globl	Perl_new_collate
	.align	16, 0x90
	.type	Perl_new_collate,@function
Perl_new_collate:                       # @Perl_new_collate
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp5:
	.size	Perl_new_collate, .Ltmp5-Perl_new_collate
	.cfi_endproc

	.globl	Perl_init_i18nl10n
	.align	16, 0x90
	.type	Perl_init_i18nl10n,@function
Perl_init_i18nl10n:                     # @Perl_init_i18nl10n
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp7:
	.cfi_def_cfa_offset 16
	movabsq	$.L.str2, %rdi
	callq	getenv
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movq	%rax, (%rsp)
	je	.LBB6_2
# BB#1:                                 # %cond.true
	leaq	(%rsp), %rdi
	xorq	%rax, %rax
	callq	parse_unicode_opts
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
.LBB6_2:                                # %cond.end
	movq	$1, %rax
	movq	%rdx, PL_unicode(%rip)
	popq	%rdx
	retq
.Ltmp8:
	.size	Perl_init_i18nl10n, .Ltmp8-Perl_init_i18nl10n
	.cfi_endproc

	.globl	Perl_init_i18nl14n
	.align	16, 0x90
	.type	Perl_init_i18nl14n,@function
Perl_init_i18nl14n:                     # @Perl_init_i18nl14n
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp10:
	.cfi_def_cfa_offset 16
	movabsq	$.L.str2, %rdi
	callq	getenv
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movq	%rax, (%rsp)
	je	.LBB7_2
# BB#1:                                 # %cond.true.i
	leaq	(%rsp), %rdi
	xorq	%rax, %rax
	callq	parse_unicode_opts
	movq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
.LBB7_2:                                # %Perl_init_i18nl10n.exit
	movq	$1, %rax
	movq	%rdx, PL_unicode(%rip)
	popq	%rdx
	retq
.Ltmp11:
	.size	Perl_init_i18nl14n, .Ltmp11-Perl_init_i18nl14n
	.cfi_endproc

	.type	.L.str2,@object         # @.str2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str2:
	.asciz	"PERL_UNICODE"
	.size	.L.str2, 13


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
