	.file	"c-convert.c.bc"
	.text
	.globl	convert
	.align	16, 0x90
	.type	convert,@function
convert:                                # @convert
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	8(%rax), %rsi
	movq	%rdi, %rbx
	cmpq	%rbx, %rsi
	je	.LBB0_26
# BB#1:                                 # %lor.lhs.false
	movzbl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB0_26
# BB#2:                                 # %lor.lhs.false
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	je	.LBB0_26
# BB#3:                                 # %lor.lhs.false11
	movzbl	16(%rsi), %edi
	testq	%rdi, %rdi
	je	.LBB0_26
# BB#4:                                 # %if.end
	movq	128(%rbx), %r8
	movq	128(%rsi), %rsi
	cmpq	%rsi, %r8
	je	.LBB0_5
# BB#8:                                 # %if.end36
	cmpq	$5, %rdi
	jne	.LBB0_10
# BB#9:                                 # %if.then44
	movabsq	$.L.str, %rdi
.LBB0_25:                               # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
.LBB0_26:                               # %return
	popq	%rbx
	retq
.LBB0_10:                               # %if.end45
	leaq	-5(%rcx), %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	$6, %rsi
	ja	.LBB0_16
# BB#11:                                # %if.end45
	jmpq	*.LJTI0_0(,%rsi,8)
.LBB0_13:                               # %if.then53
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_to_integer
	jmp	.LBB0_7
.LBB0_16:                               # %if.end70
	andq	$253, %rdx
	cmpq	$13, %rdx
	jne	.LBB0_18
# BB#17:                                # %if.then74
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_to_pointer
	jmp	.LBB0_7
.LBB0_18:                               # %if.end77
	cmpq	$9, %rcx
	je	.LBB0_23
# BB#19:                                # %if.end77
	cmpq	$8, %rcx
	jne	.LBB0_20
# BB#22:                                # %if.then84
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_to_complex
	jmp	.LBB0_7
.LBB0_12:                               # %if.then47
	movq	$114, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	build1  # TAILCALL
.LBB0_14:                               # %if.then58
	movq	%rax, %rdi
	callq	truthvalue_conversion
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jne	.LBB0_5
# BB#15:                                # %if.then65
	movq	32(%rax), %rdx
	movq	$115, %rdi
	movq	%rbx, %rsi
	jmp	.LBB0_6
.LBB0_5:                                # %if.then25
	movq	$115, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
.LBB0_6:                                # %if.then25
	callq	build1
.LBB0_7:                                # %if.then25
	movq	%rax, %rdi
	popq	%rbx
	jmp	fold  # TAILCALL
.LBB0_23:                               # %if.then89
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_to_vector
	jmp	.LBB0_7
.LBB0_20:                               # %if.end77
	cmpq	$7, %rcx
	jne	.LBB0_24
# BB#21:                                # %if.then79
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_to_real
	jmp	.LBB0_7
.LBB0_24:                               # %if.end92
	movabsq	$.L.str1, %rdi
	jmp	.LBB0_25
.Ltmp4:
	.size	convert, .Ltmp4-convert
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_12
	.quad	.LBB0_13
	.quad	.LBB0_16
	.quad	.LBB0_16
	.quad	.LBB0_16
	.quad	.LBB0_13
	.quad	.LBB0_14

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"void value not ignored as it ought to be"
	.size	.L.str, 41

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"conversion to non-scalar type requested"
	.size	.L.str1, 40


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
