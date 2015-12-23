	.file	"rephase.c.bc"
	.text
	.globl	phaseset
	.align	16, 0x90
	.type	phaseset,@function
phaseset:                               # @phaseset
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %r9
	testq	%r9, %r9
	jle	.LBB0_11
# BB#1:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rdx
	movl	nt(%rip), %r10d
	decq	%r10
	addq	$2184, %rdx             # imm = 0x888
	xorq	%r8, %r8
	movabsq	$2147483648, %r11       # imm = 0x80000000
	movabsq	$4294967294, %r12       # imm = 0xFFFFFFFE
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movabsq	$-4616189618054758400, %r15 # imm = 0xBFF0000000000000
	andq	%r13, %r10
	.align	16, 0x90
.LBB0_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$4607182418800017408, %rax # imm = 0x3FF0000000000000
	movq	%rax, (%rdx)
	movswq	-2178(%rdx), %rbx
	movq	%rbx, %rsi
	andq	%r11, %rsi
	movq	$31, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	andq	%r12, %rsi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	andq	%r13, %rcx
	cmpq	$1, %rcx
	movq	%r15, %rsi
	je	.LBB0_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB0_2 Depth=1
	movabsq	$4607182418800017408, %rsi # imm = 0x3FF0000000000000
.LBB0_4:                                # %for.body
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rsi, -24(%rdx)
	movswq	-2184(%rdx), %r14
	movq	%r14, %rax
	andq	%r11, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%r14, %rax
	andq	%r12, %rax
	subq	%rax, %r14
	andq	%r13, %r14
	cmpq	$1, %r14
	jne	.LBB0_6
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB0_2 Depth=1
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	xorq	%rax, %rsi
.LBB0_6:                                # %if.else16
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rsi, -16(%rdx)
	movswq	-2182(%rdx), %rdi
	movq	%rdi, %rax
	andq	%r11, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	addq	%rdi, %rax
	andq	%r12, %rax
	subq	%rax, %rdi
	andq	%r13, %rdi
	cmpq	$1, %rdi
	jne	.LBB0_8
# BB#7:                                 # %if.then26
                                        #   in Loop: Header=BB0_2 Depth=1
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	xorq	%rax, %rsi
.LBB0_8:                                # %if.else32
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%rsi, -8(%rdx)
	andq	%r13, %rbx
	cmpq	%r10, %rbx
	jne	.LBB0_10
# BB#9:                                 # %if.then43
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	%r15, (%rdx)
.LBB0_10:                               # %for.inc
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%r8
	movq	$32, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %rdx             # imm = 0xC80
	cmpq	%r9, %rax
	jl	.LBB0_2
.LBB0_11:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	phaseset, .Ltmp16-phaseset
	.cfi_endproc

	.globl	rephase
	.align	16, 0x90
	.type	rephase,@function
rephase:                                # @rephase
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp28:
	.cfi_def_cfa_offset 64
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movl	phases_in(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rdi, %rdx
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	jne	.LBB1_2
# BB#1:                                 # %entry
	testq	%rcx, %rax
	je	.LBB1_7
.LBB1_2:                                # %lor.lhs.false
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rcx, %rdx
	jne	.LBB1_4
# BB#3:                                 # %lor.lhs.false
	andq	%rcx, %rax
	cmpq	$1, %rax
	je	.LBB1_7
.LBB1_4:                                # %if.then
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_6
# BB#5:                                 # %if.then6
	movabsq	$.Lstr, %rdi
	callq	puts
.LBB1_6:                                # %if.end
	movq	$1, %rdi
	callq	terminate
.LBB1_7:                                # %if.end7
	movslq	sites_on_node(%rip), %r15
	testq	%r15, %r15
	jle	.LBB1_12
# BB#8:                                 # %for.cond9.preheader.lr.ph
	movq	lattice(%rip), %rbx
	xorq	%r12, %r12
	.align	16, 0x90
.LBB1_9:                                # %for.cond9.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_10 Depth 2
	movq	$31, %r13
	movq	$2160, %r14             # imm = 0x870
	.align	16, 0x90
.LBB1_10:                               # %for.cond12.preheader
                                        #   Parent Loop BB1_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx,%r14), %rdi
	movq	-136(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -136(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-128(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -128(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-120(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -120(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-112(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -112(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-104(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -104(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-96(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -96(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-88(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -88(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-80(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -80(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-72(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -72(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-64(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -64(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-56(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -56(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-48(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -48(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-40(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -40(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-32(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -32(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-24(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -24(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-16(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -16(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	-8(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, -8(%rbx,%r13,8)
	movq	(%rbx,%r14), %rdi
	movq	(%rbx,%r13,8), %rsi
	callq	float64_mul
	movq	%rax, (%rbx,%r13,8)
	addq	$18, %r13
	addq	$8, %r14
	cmpq	$103, %r13
	jne	.LBB1_10
# BB#11:                                # %for.inc42
                                        #   in Loop: Header=BB1_9 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%r15, %rax
	jl	.LBB1_9
.LBB1_12:                               # %for.end44
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, phases_in(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	rephase, .Ltmp34-rephase
	.cfi_endproc

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"DUMMY: you fouled up the phases"
	.size	.Lstr, 32


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
