	.file	"book.c.bc"
	.text
	.globl	init_book
	.align	16, 0x90
	.type	init_book,@function
init_book:                              # @init_book
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp0:
	.size	init_book, .Ltmp0-init_book
	.cfi_endproc

	.globl	choose_book_move
	.align	16, 0x90
	.type	choose_book_move,@function
choose_book_move:                       # @choose_book_move
	.cfi_startproc
# BB#0:                                 # %entry
	movq	dummy+16(%rip), %rax
	movq	dummy+8(%rip), %rcx
	movq	dummy(%rip), %rdx
	movq	%rax, 16(%rdi)
	movq	%rcx, 8(%rdi)
	movq	%rdx, (%rdi)
	movq	%rdi, %rax
	retq
.Ltmp1:
	.size	choose_book_move, .Ltmp1-choose_book_move
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
