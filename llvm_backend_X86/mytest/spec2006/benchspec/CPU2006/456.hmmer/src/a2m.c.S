	.file	"a2m.c.bc"
	.text
	.globl	ReadA2M
	.align	16, 0x90
	.type	ReadA2M,@function
ReadA2M:                                # @ReadA2M
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp8:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp9:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp10:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	(%r13), %rdi
	callq	feof
	xorq	%r14, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_20
# BB#1:                                 # %if.end
	movq	$10, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	MSAAlloc
	movq	%rax, %r14
	movq	%r13, %rdi
	callq	MSAFileGetLine
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	je	.LBB0_18
# BB#2:                                 # %while.body.lr.ph.lr.ph.lr.ph
	movabsq	$.L.str, %r12
	xorq	%r15, %r15
.LBB0_3:                                # %while.body.lr.ph.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_5 Depth 2
                                        #     Child Loop BB0_7 Depth 2
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	.align	16, 0x90
.LBB0_5:                                # %while.body.lr.ph
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB0_7
# BB#6:                                 # %while.body.us
                                        #   in Loop: Header=BB0_5 Depth=2
	movzbl	(%rax), %ecx
	cmpq	$62, %rcx
	je	.LBB0_8
# BB#4:                                 # %while.cond.outer.loopexit
                                        #   in Loop: Header=BB0_5 Depth=2
	movq	%r13, %rdi
	callq	MSAFileGetLine
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	jne	.LBB0_5
	jmp	.LBB0_18
	.align	16, 0x90
.LBB0_15:                               # %if.then24
                                        #   in Loop: Header=BB0_7 Depth=2
	leaq	8(%rsp), %rdi
	movq	%r12, %rsi
	leaq	4(%rsp), %rdx
	callq	sre_strtok
	testq	%rax, %rax
	je	.LBB0_16
# BB#21:                                # %if.end29
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	328(%r14), %rcx
	movq	(%r14), %rdx
	movl	(%rcx,%r15,4), %esi
	movl	4(%rsp), %ecx
	leaq	(%rdx,%r15,8), %rdi
	movq	%rax, %rdx
	callq	sre_strcat
	movq	328(%r14), %rcx
	movl	%eax, (%rcx,%r15,4)
.LBB0_16:                               # %while.cond.backedge
                                        #   in Loop: Header=BB0_7 Depth=2
	movq	%r13, %rdi
	callq	MSAFileGetLine
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	je	.LBB0_17
.LBB0_7:                                # %while.body
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rax), %ecx
	cmpq	$62, %rcx
	jne	.LBB0_15
.LBB0_8:                                # %if.then5
                                        #   in Loop: Header=BB0_3 Depth=1
	incq	%rax
	movq	%rax, 8(%rsp)
	leaq	8(%rsp), %rdi
	movq	%r12, %rsi
	leaq	4(%rsp), %rdx
	callq	sre_strtok
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB0_10
# BB#9:                                 # %if.then9
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	8(%r13), %rsi
	movl	16(%r13), %edx
	xorq	%rax, %rax
	movabsq	$.L.str1, %rdi
	callq	Die
.LBB0_10:                               # %if.end10
                                        #   in Loop: Header=BB0_3 Depth=1
	leaq	8(%rsp), %rdi
	movabsq	$.L.str2, %rsi
	leaq	(%rsp), %rdx
	callq	sre_strtok
	movq	312(%r14), %rdi
	movq	%rax, %r12
	movq	%rbx, %rsi
	callq	GKIStoreKey
	movq	%rax, %r15
	movq	$32, %rcx
                                        # kill: RAX<def> R15<kill>
	movslq	320(%r14), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_12
# BB#11:                                # %if.then15
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rdi
	callq	MSAExpand
.LBB0_12:                               # %if.end16
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	4(%rsp), %esi
	movq	%rbx, %rdi
	callq	sre_strdup
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	8(%r14), %rsi
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, (%rsi,%rdx,8)
	testq	%r12, %r12
	je	.LBB0_14
# BB#13:                                # %if.then20
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	MSASetSeqDescription
.LBB0_14:                               # %if.end21
                                        #   in Loop: Header=BB0_3 Depth=1
	movl	28(%r14), %eax
	incq	%rax
	movl	%eax, 28(%r14)
	movq	%r13, %rdi
	callq	MSAFileGetLine
	testq	%rax, %rax
	movq	%rax, 8(%rsp)
	movabsq	$.L.str, %r12
	jne	.LBB0_3
.LBB0_17:                               # %while.end
	testq	%rbx, %rbx
	je	.LBB0_18
# BB#19:                                # %if.end43
	movq	%r14, %rdi
	callq	MSAVerifyParse
	jmp	.LBB0_20
.LBB0_18:                               # %if.then42
	movq	%r14, %rdi
	callq	MSAFree
	xorq	%r14, %r14
.LBB0_20:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	ReadA2M, .Ltmp17-ReadA2M
	.cfi_endproc

	.globl	WriteA2M
	.align	16, 0x90
	.type	WriteA2M,@function
WriteA2M:                               # @WriteA2M
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 128
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	xorq	%r15, %r15
	movb	%r15b, 76(%rsp)
	movslq	28(%rbx), %rax
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB1_8
# BB#1:                                 # %for.body.lr.ph
	leaq	16(%rsp), %r14
	movabsq	$.L.str5, %r12
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_6 Depth 2
	movq	8(%rbx), %rcx
	movq	104(%rbx), %rax
	movq	(%rcx,%r15,8), %rdx
	testq	%rax, %rax
	movabsq	$.L.str4, %rcx
	je	.LBB1_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	(%rax,%r15,8), %rax
	testq	%rax, %rax
	movabsq	$.L.str4, %rcx
	je	.LBB1_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%rax, %rcx
.LBB1_5:                                # %cond.end
                                        #   in Loop: Header=BB1_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	callq	fprintf
	movslq	24(%rbx), %rax
	testq	%rax, %rax
	movq	$60, %r13
	jle	.LBB1_7
	.align	16, 0x90
.LBB1_6:                                # %for.body13
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rax
	movq	(%rax,%r15,8), %rax
	leaq	-60(%rax,%r13), %rsi
	movq	$60, %rdx
	movq	%r14, %rdi
	callq	strncpy
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	fprintf
	movslq	24(%rbx), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$60, %r13
	cmpq	%rax, %rdx
	jl	.LBB1_6
.LBB1_7:                                # %for.inc21
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%r15
	movslq	28(%rbx), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	8(%rsp), %rdi           # 8-byte Reload
	jl	.LBB1_2
.LBB1_8:                                # %for.end22
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	WriteA2M, .Ltmp35-WriteA2M
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	" \t\n"
	.size	.L.str, 4

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Blank name in A2M file %s (line %d)\n"
	.size	.L.str1, 37

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\n"
	.size	.L.str2, 2

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	">%s %s\n"
	.size	.L.str3, 8

	.type	.L.str4,@object         # @.str4
.L.str4:
	.zero	1
	.size	.L.str4, 1

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%s\n"
	.size	.L.str5, 4


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
