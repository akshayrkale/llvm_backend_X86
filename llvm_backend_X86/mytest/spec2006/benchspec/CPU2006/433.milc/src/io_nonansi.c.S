	.file	"io_nonansi.c.bc"
	.text
	.globl	g_open
	.align	16, 0x90
	.type	g_open,@function
g_open:                                 # @g_open
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movq	%rsi, %rcx
	movzbl	(%rcx), %eax
	movq	%rdi, %r14
	movq	$65, %rsi
	cmpq	$119, %rax
	je	.LBB0_8
# BB#1:                                 # %entry
	cmpq	$114, %rax
	jne	.LBB0_2
# BB#4:                                 # %if.then12
	movq	$43, %rsi
	movq	%rcx, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB0_5
# BB#6:                                 # %if.then12
	movabsq	$1, %rsi
	jmp	.LBB0_7
.LBB0_2:                                # %entry
	cmpq	$97, %rax
	jne	.LBB0_14
# BB#3:                                 # %if.then
	movl	this_node(%rip), %esi
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	printf
	jmp	.LBB0_13
.LBB0_5:
	movabsq	$0, %rsi
.LBB0_7:                                # %if.then12
	xorq	$1, %rsi
	addq	%rsi, %rsi
.LBB0_8:                                # %if.end21
	movq	$420, %rdx              # imm = 0x1A4
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	open
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	je	.LBB0_9
# BB#10:                                # %if.end28
	movq	$1, %rdi
	movq	$4, %rsi
	callq	calloc
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB0_12
# BB#11:                                # %if.then32
	movl	this_node(%rip), %esi
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rdi
	callq	terminate
.LBB0_12:                               # %if.end35
	movl	%r15d, (%rbx)
	jmp	.LBB0_13
.LBB0_9:                                # %if.then25
	movl	this_node(%rip), %r15d
	callq	__errno_location
	movl	(%rax), %edx
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rcx
	callq	printf
	jmp	.LBB0_13
.LBB0_14:                               # %if.else17
	movl	this_node(%rip), %esi
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %rdx
	callq	printf
.LBB0_13:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	g_open, .Ltmp10-g_open
	.cfi_endproc

	.globl	g_seek
	.align	16, 0x90
	.type	g_seek,@function
g_seek:                                 # @g_seek
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %edi
	jmp	lseek  # TAILCALL
.Ltmp11:
	.size	g_seek, .Ltmp11-g_seek
	.cfi_endproc

	.globl	g_write
	.align	16, 0x90
	.type	g_write,@function
g_write:                                # @g_write
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp14:
	.cfi_def_cfa_offset 16
.Ltmp15:
	.cfi_offset %rbx, -16
	movq	%rcx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %rsi
	imulq	%rbx, %rdx
	movq	$32, %rcx
	movl	(%rax), %edi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	callq	write
	xorq	%rdx, %rdx
	divq	%rbx
	popq	%rbx
	retq
.Ltmp16:
	.size	g_write, .Ltmp16-g_write
	.cfi_endproc

	.globl	g_read
	.align	16, 0x90
	.type	g_read,@function
g_read:                                 # @g_read
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 16
.Ltmp20:
	.cfi_offset %rbx, -16
	movq	%rcx, %rax
	movq	%rsi, %rbx
	movq	%rdi, %rsi
	imulq	%rbx, %rdx
	movq	$32, %rcx
	movl	(%rax), %edi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	callq	read
	xorq	%rdx, %rdx
	divq	%rbx
	popq	%rbx
	retq
.Ltmp21:
	.size	g_read, .Ltmp21-g_read
	.cfi_endproc

	.globl	g_close
	.align	16, 0x90
	.type	g_close,@function
g_close:                                # @g_close
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp27:
	.cfi_def_cfa_offset 32
.Ltmp28:
	.cfi_offset %rbx, -24
.Ltmp29:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %edi
	callq	close
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	free
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp30:
	.size	g_close, .Ltmp30-g_close
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"g_open: Node %d. Append not supported in PIOFS\n"
	.size	.L.str, 48

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"g_open: Node %d. mode %s not recognized\n"
	.size	.L.str1, 41

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"g_open: Node %d error %d opening %s\n"
	.size	.L.str2, 37

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"g_open: Node %d can't malloc fp\n"
	.size	.L.str3, 33


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
