	.file	"draw.c.bc"
	.text
	.globl	is_draw
	.align	16, 0x90
	.type	is_draw,@function
is_draw:                                # @is_draw
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
.Ltmp5:
	.cfi_offset %rbx, -24
.Ltmp6:
	.cfi_offset %r14, -16
	movslq	fifty(%rip), %r11
	cmpq	$4, %r11
	jl	.LBB0_1
# BB#2:                                 # %if.then
	movslq	move_number(%rip), %r10
	movl	ply(%rip), %r8d
	leaq	(%r8,%r10), %r14
	movq	%r11, %r9
	notq	%r9
	addq	%r9, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r10
	jl	.LBB0_4
# BB#3:                                 # %if.then
	movq	%r10, %r14
.LBB0_4:                                # %if.then
	leaq	-3(%r8,%r10), %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_10
# BB#5:                                 # %if.then
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB0_10
# BB#6:                                 # %for.body.lr.ph
	movl	hash(%rip), %eax
	leaq	-3(%r8,%r10), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	leaq	hash_history(,%rsi,4), %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	.align	16, 0x90
.LBB0_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rsi), %eax
	cmpq	%rax, %rbx
	movq	$1, %rax
	je	.LBB0_21
# BB#7:                                 # %for.cond
                                        #   in Loop: Header=BB0_9 Depth=1
	addq	$-2, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_10
# BB#8:                                 # %for.cond
                                        #   in Loop: Header=BB0_9 Depth=1
	addq	$-8, %rsi
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB0_9
.LBB0_10:                               # %if.end15
	cmpq	$6, %r11
	jl	.LBB0_11
# BB#12:                                # %if.then17
	leaq	-1(%r10), %rdx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%r8, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%r8, %rax
	andq	$-2, %rax
	movq	%r8, %rdi
	subq	%rax, %rdi
	movq	%rdx, %r14
	subq	%rdi, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_13
# BB#14:                                # %if.then17
	addq	%r9, %r10
	addq	%r8, %r10
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	movabsq	$0, %rax
	jl	.LBB0_21
# BB#15:                                # %for.body28.lr.ph
	movl	hash(%rip), %eax
	subq	%rdi, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	hash_history(,%rdx,4), %rdx
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%rax, %r8
	movabsq	$0, %r9
	movabsq	$1, %r11
	.align	16, 0x90
.LBB0_18:                               # %for.body28
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx), %eax
	cmpq	%rax, %r8
	movq	%r9, %rax
	jne	.LBB0_20
# BB#19:                                # %for.body28
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	%r11, %rax
.LBB0_20:                               # %for.body28
                                        #   in Loop: Header=BB0_18 Depth=1
	addq	%rax, %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	$1, %rax
	jge	.LBB0_21
# BB#16:                                # %for.cond23
                                        #   in Loop: Header=BB0_18 Depth=1
	addq	$-2, %r14
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	js	.LBB0_21
# BB#17:                                # %for.cond23
                                        #   in Loop: Header=BB0_18 Depth=1
	addq	$-8, %rdx
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rsi
	jge	.LBB0_18
	jmp	.LBB0_21
.LBB0_1:
	xorq	%rax, %rax
	jmp	.LBB0_21
.LBB0_11:
	xorq	%rax, %rax
	jmp	.LBB0_21
.LBB0_13:
	xorq	%rax, %rax
.LBB0_21:                               # %return
	popq	%rbx
	popq	%r14
	retq
.Ltmp7:
	.size	is_draw, .Ltmp7-is_draw
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
