	.file	"xexit.c.bc"
	.text
	.globl	xexit
	.align	16, 0x90
	.type	xexit,@function
xexit:                                  # @xexit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	_xexit_cleanup(%rip), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %if.then
	callq	*%rax
.LBB0_2:                                # %if.end
	movq	%rbx, %rdi
	callq	exit
.Ltmp4:
	.size	xexit, .Ltmp4-xexit
	.cfi_endproc

	.type	_xexit_cleanup,@object  # @_xexit_cleanup
	.comm	_xexit_cleanup,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
