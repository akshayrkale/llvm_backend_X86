	.file	"main.c.bc"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	toplev_main  # TAILCALL
.Ltmp0:
	.size	main, .Ltmp0-main
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
