	.file	"hooks.c.bc"
	.text
	.globl	hook_void_void
	.align	16, 0x90
	.type	hook_void_void,@function
hook_void_void:                         # @hook_void_void
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	hook_void_void, .Ltmp0-hook_void_void
	.cfi_endproc

	.globl	hook_void_bool_false
	.align	16, 0x90
	.type	hook_void_bool_false,@function
hook_void_bool_false:                   # @hook_void_bool_false
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp1:
	.size	hook_void_bool_false, .Ltmp1-hook_void_bool_false
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
