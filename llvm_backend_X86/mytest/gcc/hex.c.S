	.file	"hex.c.bc"
	.text
	.globl	hex_init
	.align	16, 0x90
	.type	hex_init,@function
hex_init:                               # @hex_init
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	hex_init, .Ltmp0-hex_init
	.cfi_endproc

	.type	_hex_value,@object      # @_hex_value
	.section	.rodata,"a",@progbits
	.globl	_hex_value
	.align	16
_hex_value:
	.ascii	"cccccccccccccccccccccccccccccccccccccccccccccccc\000\001\002\003\004\005\006\007\b\tccccccc\n\013\f\r\016\017cccccccccccccccccccccccccc\n\013\f\r\016\017ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc"
	.size	_hex_value, 256


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
