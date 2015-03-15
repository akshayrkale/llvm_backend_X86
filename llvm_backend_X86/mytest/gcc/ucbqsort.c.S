	.file	"ucbqsort.c.bc"
	.text
	.globl	specqsort
	.align	16, 0x90
	.type	specqsort,@function
specqsort:                              # @specqsort
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movq	%rcx, %rax
	movq	%rdi, %r15
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$2, %rdi
	jl	.LBB0_28
# BB#1:                                 # %if.end
	movl	%edx, qsz(%rip)
	movq	%rax, qcmp(%rip)
	movq	$2, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movl	%eax, thresh(%rip)
	leaq	(%rdx,%rdx), %rax
	leaq	(%rax,%rax,2), %rax
	movl	%eax, mthresh(%rip)
	movq	$32, %rcx
	movq	%rdx, %r14
	imulq	%rsi, %r14
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%r15, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	cmpq	$4, %rsi
	movq	%rax, %r13
	jl	.LBB0_3
# BB#2:                                 # %if.then4
	movq	%r15, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	qst
	movslq	thresh(%rip), %r13
	movl	qsz(%rip), %edx
	addq	%r15, %r13
.LBB0_3:                                # %if.end7
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%r15, %rbx
	cmpq	%r13, %rbx
	jae	.LBB0_22
# BB#4:
	movq	%r15, %r12
	.align	16, 0x90
.LBB0_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rax
	jg	.LBB0_7
# BB#6:                                 # %for.body
                                        #   in Loop: Header=BB0_5 Depth=1
	movq	%r12, %rax
.LBB0_7:                                # %for.body
                                        #   in Loop: Header=BB0_5 Depth=1
	movslq	qsz(%rip), %rdx
	addq	%rdx, %rbx
	cmpq	%r13, %rbx
	movq	%rax, %r12
	jb	.LBB0_5
# BB#8:                                 # %for.end
	cmpq	%r15, %rax
	je	.LBB0_22
# BB#9:                                 # %if.then15
	testq	%rdx, %rdx
	jle	.LBB0_22
# BB#10:                                # %for.body20.preheader
	addq	%r15, %rdx
	leaq	1(%r15), %rcx
	cmpq	%rcx, %rdx
	movq	%rdx, %rsi
	ja	.LBB0_12
# BB#11:                                # %for.body20.preheader
	movq	%rcx, %rsi
.LBB0_12:                               # %for.body20.preheader
	movq	%rsi, %rbx
	subq	%r15, %rbx
	xorq	%rcx, %rcx
	movq	%rbx, %r8
	andq	$-4, %r8
	je	.LBB0_13
# BB#14:                                # %vector.memcheck
	leaq	-1(%rsi), %rdi
	cmpq	%rdi, %rax
	ja	.LBB0_17
# BB#15:                                # %vector.memcheck
	movq	%r15, %rdi
	notq	%rdi
	addq	%rdi, %rsi
	addq	%rax, %rsi
	cmpq	%r15, %rsi
	jb	.LBB0_17
# BB#16:
	movq	%r15, %rsi
	jmp	.LBB0_20
.LBB0_13:
	movq	%r15, %rsi
	jmp	.LBB0_20
.LBB0_17:                               # %vector.body.preheader
	movq	%r14, 40(%rsp)          # 8-byte Spill
	leaq	(%r15,%r8), %rsi
	movq	%rbx, %rcx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	andq	$-4, %rcx
	leaq	(%rax,%r8), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	.align	16, 0x90
.LBB0_18:                               # %vector.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %r11d
	movzbl	1(%rax), %r14d
	movzbl	2(%rax), %r12d
	movzbl	3(%rax), %r13d
	movzbl	(%rdi), %ebx
	movzbl	2(%rdi), %r8d
	movzbl	1(%rdi), %r9d
	movzbl	3(%rdi), %r10d
	movb	%r10b, 3(%rax)
	movb	%r8b, 2(%rax)
	movb	%r9b, 1(%rax)
	movb	%bl, (%rax)
	movb	%r13b, 3(%rdi)
	movb	%r12b, 2(%rdi)
	movb	%r14b, 1(%rdi)
	movb	%r11b, (%rdi)
	addq	$4, %rdi
	addq	$4, %rax
	addq	$-4, %rcx
	jne	.LBB0_18
# BB#19:
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
.LBB0_20:                               # %middle.block
	cmpq	%rcx, %rbx
	je	.LBB0_21
	.align	16, 0x90
.LBB0_34:                               # %for.body20
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rax), %ecx
	movzbl	(%rsi), %edi
	movb	%dil, (%rax)
	incq	%rax
	movb	%cl, (%rsi)
	leaq	1(%rsi), %rsi
	cmpq	%rdx, %rsi
	jb	.LBB0_34
.LBB0_21:                               # %for.cond18.for.cond24.preheader.loopexit_crit_edge
	movl	qsz(%rip), %edx
.LBB0_22:                               # %for.cond24.preheader
	movq	$32, %rcx
	movq	%rdx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	%r14, %r13
	jge	.LBB0_28
# BB#23:                                # %while.cond.preheader.lr.ph
	leaq	(%r15,%r13), %rax
	.align	16, 0x90
.LBB0_24:                               # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_25 Depth 2
                                        #     Child Loop BB0_30 Depth 2
                                        #       Child Loop BB0_32 Depth 3
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	movq	%r15, %rbx
	.align	16, 0x90
.LBB0_25:                               # %while.cond
                                        #   Parent Loop BB0_24 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %r12
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movq	%rdx, %r14
	sarq	%cl, %r14
	subq	%r14, %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movslq	qsz(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_25
# BB#26:                                # %while.end
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	%rdx, %rax
	subq	%r14, %rax
	addq	%rax, %r12
	cmpq	%r15, %r12
	je	.LBB0_27
# BB#29:                                # %if.then37
                                        #   in Loop: Header=BB0_24 Depth=1
	addq	%rdx, %r13
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	-1(%rax,%r13), %rax
	jmp	.LBB0_30
	.align	16, 0x90
.LBB0_33:                               # %for.end50
                                        #   in Loop: Header=BB0_30 Depth=2
	movb	%sil, (%rdx)
	movl	qsz(%rip), %edx
	decq	%rax
.LBB0_30:                               # %if.then37
                                        #   Parent Loop BB0_24 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_32 Depth 3
	cmpq	%r15, %rax
	jb	.LBB0_27
# BB#31:                                # %for.body43
                                        #   in Loop: Header=BB0_30 Depth=2
	movzbl	(%rax), %esi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rax, %rcx
	subq	%rdx, %rcx
	cmpq	%r12, %rcx
	movq	%rax, %rdx
	jb	.LBB0_33
	.align	16, 0x90
.LBB0_32:                               # %for.body49
                                        #   Parent Loop BB0_24 Depth=1
                                        #     Parent Loop BB0_30 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rcx, %rdi
	movzbl	(%rdi), %ebx
	movb	%bl, (%rdx)
	movslq	qsz(%rip), %rdx
	subq	%rdx, %rcx
	cmpq	%r12, %rcx
	movq	%rdi, %rdx
	jae	.LBB0_32
	jmp	.LBB0_33
	.align	16, 0x90
.LBB0_27:                               # %for.cond24.backedge
                                        #   in Loop: Header=BB0_24 Depth=1
	movq	$32, %rcx
	movq	%rdx, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%r15,%r13), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jb	.LBB0_24
.LBB0_28:                               # %for.end53
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	specqsort, .Ltmp17-specqsort
	.cfi_endproc

	.align	16, 0x90
	.type	qst,@function
qst:                                    # @qst
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
	subq	$64, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 112
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
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %rbx
	subq	%rdi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB1_1:                                # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_15 Depth 2
                                        #     Child Loop BB1_18 Depth 2
                                        #     Child Loop BB1_20 Depth 2
                                        #       Child Loop BB1_21 Depth 3
                                        #       Child Loop BB1_24 Depth 3
                                        #       Child Loop BB1_36 Depth 3
                                        #       Child Loop BB1_39 Depth 3
	movslq	qsz(%rip), %rsi
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
	cqto
	idivq	%rsi
	movq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$33, %rcx
	sarq	%cl, %r14
	imulq	%rsi, %r14
	movq	$32, %rcx
	movq	%r14, %r13
	shlq	%cl, %r13
	movslq	mthresh(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%rdi,%r13), %r12
	cmpq	%rax, %rbx
	jl	.LBB1_19
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	xorq	%rax, %rax
	movq	%rdi, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
                                        # kill: RDI<def> RBX<kill>
	movq	%r12, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rbx
.LBB1_4:                                # %if.then
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	qsz(%rip), %rax
	movq	8(%rsp), %rsi           # 8-byte Reload
	subq	%rax, %rsi
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_9
# BB#5:                                 # %if.then12
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rbx
	movq	%r12, %rcx
	je	.LBB1_7
# BB#6:                                 # %if.then12
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rax, %rcx
.LBB1_7:                                # %if.then12
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_9
# BB#8:                                 # %if.then12
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	48(%rsp), %rbx          # 8-byte Reload
.LBB1_9:                                # %if.then12
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	16(%rsp), %rdi          # 8-byte Reload
	cmpq	%r12, %rbx
	je	.LBB1_19
# BB#10:                                # %if.then27
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	qsz(%rip), %eax
	leaq	-1(%rax), %rdi
	andq	%r15, %rdi
	leaq	1(%rdi), %r10
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r10, %rsi
	movabsq	$8589934588, %rcx       # imm = 0x1FFFFFFFC
	andq	%rcx, %rsi
	je	.LBB1_11
# BB#12:                                # %vector.memcheck224
                                        #   in Loop: Header=BB1_1 Depth=1
	leaq	(%rbx,%rdi), %r9
	movq	16(%rsp), %r8           # 8-byte Reload
	leaq	(%r8,%rdx), %rcx
	cmpq	%r9, %rcx
	ja	.LBB1_14
# BB#13:                                # %vector.memcheck224
                                        #   in Loop: Header=BB1_1 Depth=1
	addq	%rdi, %rdx
	addq	%r8, %rdx
	cmpq	%rdx, %rbx
	ja	.LBB1_14
.LBB1_11:                               #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %r13
	movq	%rbx, %rcx
	xorq	%rdx, %rdx
	jmp	.LBB1_17
.LBB1_14:                               # %vector.body208.preheader
                                        #   in Loop: Header=BB1_1 Depth=1
	addq	%rsi, %r13
	addq	%r8, %r13
	subq	%rsi, %rax
	movq	%r10, %rdi
	movq	%r10, 40(%rsp)          # 8-byte Spill
	andq	$-4, %rdi
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%r8, %r14
	leaq	(%rbx,%rsi), %rcx
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB1_15:                               # %vector.body208
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14), %edx
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movzbl	1(%r14), %edx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movzbl	2(%r14), %r9d
	movzbl	3(%r14), %esi
	movzbl	(%rbx), %r8d
	movzbl	2(%rbx), %r10d
	movzbl	1(%rbx), %edx
	movzbl	3(%rbx), %r11d
	movb	%r11b, 3(%r14)
	movb	%r10b, 2(%r14)
	movb	%dl, 1(%r14)
	movb	%r8b, (%r14)
	movb	%sil, 3(%rbx)
	movb	%r9b, 2(%rbx)
	movq	48(%rsp), %rdx          # 8-byte Reload
	movb	%dl, 1(%rbx)
	movq	56(%rsp), %rdx          # 8-byte Reload
	movb	%dl, (%rbx)
	addq	$4, %rbx
	addq	$4, %r14
	addq	$-4, %rdi
	jne	.LBB1_15
# BB#16:                                #   in Loop: Header=BB1_1 Depth=1
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	32(%rsp), %rdx          # 8-byte Reload
.LBB1_17:                               # %middle.block209
                                        #   in Loop: Header=BB1_1 Depth=1
	cmpq	%rdx, %r10
	movq	16(%rsp), %rdi          # 8-byte Reload
	je	.LBB1_19
	.align	16, 0x90
.LBB1_18:                               # %do.body28
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %edx
	movzbl	(%rcx), %esi
	movb	%sil, (%r13)
	incq	%r13
	movb	%dl, (%rcx)
	incq	%rcx
	decq	%rax
	testq	%r15, %rax
	jne	.LBB1_18
	.align	16, 0x90
.LBB1_19:                               # %if.end31
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movslq	qsz(%rip), %rax
	movq	8(%rsp), %r14           # 8-byte Reload
	subq	%rax, %r14
	movq	%rdi, %r8
	jmp	.LBB1_20
	.align	16, 0x90
.LBB1_31:                               # %if.else69
                                        #   in Loop: Header=BB1_20 Depth=2
	subq	%r14, %rbx
	movq	%rbx, %r14
	movq	%r12, %rbx
	movq	%r13, %r12
	movq	%r13, %r8
.LBB1_32:                               # %swap
                                        #   in Loop: Header=BB1_20 Depth=2
	leaq	-1(%rdx), %rax
	andq	%r15, %rax
	leaq	1(%rax), %rsi
	xorq	%rdi, %rdi
	movq	%rsi, %r9
	movabsq	$8589934588, %rcx       # imm = 0x1FFFFFFFC
	andq	%rcx, %r9
	je	.LBB1_38
# BB#33:                                # %vector.memcheck
                                        #   in Loop: Header=BB1_20 Depth=2
	leaq	(%rbx,%rax), %rcx
	cmpq	%rcx, %r13
	ja	.LBB1_35
# BB#34:                                # %vector.memcheck
                                        #   in Loop: Header=BB1_20 Depth=2
	addq	%r13, %rax
	cmpq	%rax, %rbx
	jbe	.LBB1_38
.LBB1_35:                               # %vector.body.preheader
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	%r14, 56(%rsp)          # 8-byte Spill
	subq	%r9, %rdx
	movq	%rsi, %rdi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	andq	$-4, %rdi
	leaq	(%r13,%r9), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	leaq	(%rbx,%r9), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rsi
	.align	16, 0x90
.LBB1_36:                               # %vector.body
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r13), %eax
	movzbl	1(%r13), %ecx
	movzbl	2(%r13), %r8d
	movzbl	3(%r13), %r9d
	movzbl	(%rbx), %r15d
	movzbl	2(%rbx), %r14d
	movzbl	1(%rbx), %r10d
	movzbl	3(%rbx), %r11d
	movb	%r11b, 3(%r13)
	movb	%r14b, 2(%r13)
	movb	%r10b, 1(%r13)
	movb	%r15b, (%r13)
	movb	%r9b, 3(%rbx)
	movb	%r8b, 2(%rbx)
	movb	%cl, 1(%rbx)
	movb	%al, (%rbx)
	addq	$4, %rbx
	addq	$4, %r13
	addq	$-4, %rdi
	jne	.LBB1_36
# BB#37:                                #   in Loop: Header=BB1_20 Depth=2
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rsi, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
.LBB1_38:                               # %middle.block
                                        #   in Loop: Header=BB1_20 Depth=2
	cmpq	%rdi, %rsi
	je	.LBB1_20
	.align	16, 0x90
.LBB1_39:                               # %do.body74
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r13), %eax
	movzbl	(%rbx), %ecx
	movb	%cl, (%r13)
	incq	%r13
	movb	%al, (%rbx)
	incq	%rbx
	decq	%rdx
	testq	%r15, %rdx
	jne	.LBB1_39
	jmp	.LBB1_20
	.align	16, 0x90
.LBB1_26:                               # %if.end55
                                        #   in Loop: Header=BB1_20 Depth=2
	leaq	(%r13,%rdx), %r8
	movq	%rbx, %r14
	cmpq	%r12, %r13
	jne	.LBB1_28
# BB#27:                                #   in Loop: Header=BB1_20 Depth=2
	movq	%rbx, %r12
	jmp	.LBB1_32
.LBB1_28:                               # %if.else
                                        #   in Loop: Header=BB1_20 Depth=2
	subq	%rdx, %r14
	jmp	.LBB1_32
	.align	16, 0x90
.LBB1_20:                               # %while.cond.outer
                                        #   Parent Loop BB1_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB1_21 Depth 3
                                        #       Child Loop BB1_24 Depth 3
                                        #       Child Loop BB1_36 Depth 3
                                        #       Child Loop BB1_39 Depth 3
	cmpq	%r12, %r8
	movq	%r8, %r13
	jae	.LBB1_22
	.align	16, 0x90
.LBB1_21:                               # %land.rhs
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_22
# BB#48:                                # %while.body
                                        #   in Loop: Header=BB1_21 Depth=3
	movslq	qsz(%rip), %rax
	addq	%rax, %r13
	cmpq	%r12, %r13
	jb	.LBB1_21
.LBB1_22:                               # %while.cond43.preheader
                                        #   in Loop: Header=BB1_20 Depth=2
	cmpq	%r12, %r14
	jbe	.LBB1_29
# BB#23:                                #   in Loop: Header=BB1_20 Depth=2
	movq	%r14, %rbx
	.align	16, 0x90
.LBB1_24:                               # %while.body46
                                        #   Parent Loop BB1_1 Depth=1
                                        #     Parent Loop BB1_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	*qcmp(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movslq	qsz(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB1_26
# BB#25:                                # %if.then51
                                        #   in Loop: Header=BB1_24 Depth=3
	subq	%rdx, %rbx
	cmpq	%r12, %rbx
	ja	.LBB1_24
	jmp	.LBB1_30
	.align	16, 0x90
.LBB1_29:                               # %while.cond43.preheader.while.end65_crit_edge
                                        #   in Loop: Header=BB1_20 Depth=2
	movl	qsz(%rip), %edx
	movq	%r14, %rbx
.LBB1_30:                               # %while.end65
                                        #   in Loop: Header=BB1_20 Depth=2
	movq	$32, %rcx
	movq	%rdx, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	cmpq	%r12, %r13
	jne	.LBB1_31
# BB#40:                                # %for.end
                                        #   in Loop: Header=BB1_1 Depth=1
	addq	%r12, %r14
	movq	%r12, %r13
	movq	16(%rsp), %rdi          # 8-byte Reload
	subq	%rdi, %r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rsp), %rbx           # 8-byte Reload
	subq	%r14, %rbx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movl	thresh(%rip), %eax
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	cmpq	%rsi, %rdx
	jle	.LBB1_41
# BB#43:                                # %if.else98
                                        #   in Loop: Header=BB1_1 Depth=1
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jge	.LBB1_45
# BB#44:                                #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rbx
	movq	%rdi, %r14
	jmp	.LBB1_46
	.align	16, 0x90
.LBB1_41:                               # %if.then93
                                        #   in Loop: Header=BB1_1 Depth=1
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r13
	jl	.LBB1_46
# BB#42:                                # %if.then96
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r12, %rsi
	callq	qst
	jmp	.LBB1_46
	.align	16, 0x90
.LBB1_45:                               # %if.then101
                                        #   in Loop: Header=BB1_1 Depth=1
	movq	%r14, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	qst
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%r13, %rbx
	movq	16(%rsp), %r14          # 8-byte Reload
.LBB1_46:                               # %do.cond104
                                        #   in Loop: Header=BB1_1 Depth=1
	movslq	thresh(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%r14, %rdi
	jge	.LBB1_1
# BB#47:                                # %do.end107
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	qst, .Ltmp35-qst
	.cfi_endproc

	.type	qsz,@object             # @qsz
	.local	qsz
	.comm	qsz,4,4
	.type	qcmp,@object            # @qcmp
	.local	qcmp
	.comm	qcmp,8,8
	.type	thresh,@object          # @thresh
	.local	thresh
	.comm	thresh,4,4
	.type	mthresh,@object         # @mthresh
	.local	mthresh
	.comm	mthresh,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
