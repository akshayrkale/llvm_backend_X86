	.file	"DynaLoader.c.bc"
	.text
	.globl	XS_DynaLoader_dl_error
	.align	16, 0x90
	.type	XS_DynaLoader_dl_error,@function
XS_DynaLoader_dl_error:                 # @XS_DynaLoader_dl_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
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
	leaq	1(%rbx), %r15
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
	movabsq	$.L.str1, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setpv
	movzbl	17(%r14), %eax
	movq	PL_stack_base(%rip), %r12
	testq	$64, %rax
	je	.LBB0_7
# BB#6:                                 # %if.then9
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB0_7:                                # %if.end11
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
.Ltmp14:
	.size	XS_DynaLoader_dl_error, .Ltmp14-XS_DynaLoader_dl_error
	.cfi_endproc

	.globl	boot_DynaLoader
	.align	16, 0x90
	.type	boot_DynaLoader,@function
boot_DynaLoader:                        # @boot_DynaLoader
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 16
.Ltmp18:
	.cfi_offset %rbx, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movabsq	$.L.str3, %rdi
	movabsq	$XS_DynaLoader_dl_error, %rsi
	movabsq	$.L.str2, %rdx
	callq	Perl_newXS
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	retq
.Ltmp19:
	.size	boot_DynaLoader, .Ltmp19-boot_DynaLoader
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: DynaLoader::dl_error()"
	.size	.L.str, 30

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Not implemented"
	.size	.L.str1, 16

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"DynaLoader.c"
	.size	.L.str2, 13

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"DynaLoader::dl_error"
	.size	.L.str3, 21


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
