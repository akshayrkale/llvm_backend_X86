	.file	"Hostname.c.bc"
	.text
	.globl	XS_Sys__Hostname_ghname
	.align	16, 0x90
	.type	XS_Sys__Hostname_ghname,@function
XS_Sys__Hostname_ghname:                # @XS_Sys__Hostname_ghname
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
	movq	PL_stack_sp(%rip), %r14
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	movq	%r14, %rbx
	subq	%rax, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	je	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB0_2:                                # %if.end
	movq	$3, %rcx
	movq	PL_stack_max(%rip), %rax
	shlq	%cl, %rbx
	leaq	1(%r15), %r12
	subq	%rbx, %r14
	subq	%r14, %rax
	cmpq	$7, %rax
	jg	.LBB0_4
# BB#3:                                 # %if.then11
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	Perl_stack_grow
.LBB0_4:                                # %if.then14
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_undef, 8(%rax,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	XS_Sys__Hostname_ghname, .Ltmp14-XS_Sys__Hostname_ghname
	.cfi_endproc

	.globl	boot_Sys__Hostname
	.align	16, 0x90
	.type	boot_Sys__Hostname,@function
boot_Sys__Hostname:                     # @boot_Sys__Hostname
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
	movabsq	$.L.str2, %rdi
	movabsq	$XS_Sys__Hostname_ghname, %rsi
	movabsq	$.L.str1, %rdx
	callq	Perl_newXS
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	retq
.Ltmp19:
	.size	boot_Sys__Hostname, .Ltmp19-boot_Sys__Hostname
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Sys::Hostname::ghname()"
	.size	.L.str, 31

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Hostname.c"
	.size	.L.str1, 11

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Sys::Hostname::ghname"
	.size	.L.str2, 22


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
