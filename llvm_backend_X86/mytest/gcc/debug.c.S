	.file	"debug.c.bc"
	.text
	.globl	debug_nothing_charstar
	.align	16, 0x90
	.type	debug_nothing_charstar,@function
debug_nothing_charstar:                 # @debug_nothing_charstar
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	debug_nothing_charstar, .Ltmp0-debug_nothing_charstar
	.cfi_endproc

	.globl	debug_nothing_int_charstar
	.align	16, 0x90
	.type	debug_nothing_int_charstar,@function
debug_nothing_int_charstar:             # @debug_nothing_int_charstar
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp1:
	.size	debug_nothing_int_charstar, .Ltmp1-debug_nothing_int_charstar
	.cfi_endproc

	.globl	debug_nothing_int
	.align	16, 0x90
	.type	debug_nothing_int,@function
debug_nothing_int:                      # @debug_nothing_int
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp2:
	.size	debug_nothing_int, .Ltmp2-debug_nothing_int
	.cfi_endproc

	.globl	debug_nothing_int_int
	.align	16, 0x90
	.type	debug_nothing_int_int,@function
debug_nothing_int_int:                  # @debug_nothing_int_int
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp3:
	.size	debug_nothing_int_int, .Ltmp3-debug_nothing_int_int
	.cfi_endproc

	.globl	debug_true_tree
	.align	16, 0x90
	.type	debug_true_tree,@function
debug_true_tree:                        # @debug_true_tree
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp4:
	.size	debug_true_tree, .Ltmp4-debug_true_tree
	.cfi_endproc

	.globl	debug_nothing_void
	.align	16, 0x90
	.type	debug_nothing_void,@function
debug_nothing_void:                     # @debug_nothing_void
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp5:
	.size	debug_nothing_void, .Ltmp5-debug_nothing_void
	.cfi_endproc

	.globl	debug_nothing_tree
	.align	16, 0x90
	.type	debug_nothing_tree,@function
debug_nothing_tree:                     # @debug_nothing_tree
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp6:
	.size	debug_nothing_tree, .Ltmp6-debug_nothing_tree
	.cfi_endproc

	.globl	debug_nothing_rtx
	.align	16, 0x90
	.type	debug_nothing_rtx,@function
debug_nothing_rtx:                      # @debug_nothing_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp7:
	.size	debug_nothing_rtx, .Ltmp7-debug_nothing_rtx
	.cfi_endproc

	.type	do_nothing_debug_hooks,@object # @do_nothing_debug_hooks
	.data
	.globl	do_nothing_debug_hooks
	.align	8
do_nothing_debug_hooks:
	.quad	debug_nothing_charstar
	.quad	debug_nothing_charstar
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int
	.quad	debug_nothing_int_int
	.quad	debug_nothing_int_int
	.quad	debug_true_tree
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int_charstar
	.quad	debug_nothing_int
	.quad	debug_nothing_void
	.quad	debug_nothing_tree
	.quad	debug_nothing_int
	.quad	debug_nothing_tree
	.quad	debug_nothing_tree
	.quad	debug_nothing_tree
	.quad	debug_nothing_tree
	.quad	debug_nothing_rtx
	.size	do_nothing_debug_hooks, 160


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
