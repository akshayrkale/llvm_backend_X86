	.file	"deb.c.bc"
	.text
	.globl	Perl_deb
	.align	16, 0x90
	.type	Perl_deb,@function
Perl_deb:                               # @Perl_deb
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%r9, -16(%rsp)
	movq	%r8, -24(%rsp)
	movq	%rcx, -32(%rsp)
	movq	%rdx, -40(%rsp)
	movq	%rsi, -48(%rsp)
	retq
.Ltmp0:
	.size	Perl_deb, .Ltmp0-Perl_deb
	.cfi_endproc

	.globl	Perl_vdeb
	.align	16, 0x90
	.type	Perl_vdeb,@function
Perl_vdeb:                              # @Perl_vdeb
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp1:
	.size	Perl_vdeb, .Ltmp1-Perl_vdeb
	.cfi_endproc

	.globl	Perl_debstackptrs
	.align	16, 0x90
	.type	Perl_debstackptrs,@function
Perl_debstackptrs:                      # @Perl_debstackptrs
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp2:
	.size	Perl_debstackptrs, .Ltmp2-Perl_debstackptrs
	.cfi_endproc

	.globl	Perl_debstack
	.align	16, 0x90
	.type	Perl_debstack,@function
Perl_debstack:                          # @Perl_debstack
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movq	PL_curcop(%rip), %rax
	movq	48(%rax), %rax
	movq	PL_debstash(%rip), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_2
# BB#1:                                 # %land.lhs.true
	testq	$524288, PL_debug(%rip) # imm = 0x80000
	je	.LBB3_3
.LBB3_2:                                # %if.end
	callq	Perl_PerlIO_stderr
	movq	%rax, %rcx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	PerlIO_printf
	movq	PL_stack_base(%rip), %rdi
	movq	PL_stack_sp(%rip), %rdx
	subq	%rdi, %rdx
	movq	$3, %rcx
	movq	PL_curstackinfo(%rip), %rax
	movq	PL_markstack_ptr(%rip), %r8
	movq	PL_markstack(%rip), %rsi
	sarq	%cl, %rdx
	movq	56(%rax), %r9
	subq	%rsi, %r8
	movq	$3, %rcx
	sarq	%cl, %r8
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	movq	%r9, %rcx
	callq	deb_stack_n
.LBB3_3:                                # %return
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp5:
	.size	Perl_debstack, .Ltmp5-Perl_debstack
	.cfi_endproc

	.globl	Perl_deb_stack_all
	.align	16, 0x90
	.type	Perl_deb_stack_all,@function
Perl_deb_stack_all:                     # @Perl_deb_stack_all
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp6:
	.size	Perl_deb_stack_all, .Ltmp6-Perl_deb_stack_all
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"    =>  "
	.size	.L.str, 9


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
