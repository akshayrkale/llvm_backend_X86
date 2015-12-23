	.file	"fold-const.c.bc"
	.text
	.globl	force_fit_type
	.align	16, 0x90
	.type	force_fit_type,@function
force_fit_type:                         # @force_fit_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB0_23
# BB#1:                                 # %if.end7
	movq	8(%rdi), %r11
	movl	16(%r11), %eax
	movq	32(%rdi), %r9
	movq	40(%rdi), %r8
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	jne	.LBB0_3
# BB#2:                                 # %if.then26
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	jmp	.LBB0_4
.LBB0_3:                                # %if.else27
	movl	60(%r11), %eax
	andq	$511, %rax              # imm = 0x1FF
.LBB0_4:                                # %if.end33
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$128, %rbx
	movq	%r8, %rdx
	movq	%r9, %r10
	je	.LBB0_10
# BB#5:                                 # %if.else36
	cmpq	$65, %rbx
	jb	.LBB0_7
# BB#6:                                 # %if.then38
	leaq	-64(%rax), %rcx
	movq	$-1, %rdx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%r8, %rdx
	movq	%rdx, 40(%rdi)
	movq	%r9, %r10
	jmp	.LBB0_10
.LBB0_7:                                # %if.else43
	movq	$0, 40(%rdi)
	xorq	%rdx, %rdx
	cmpq	$63, %rbx
	ja	.LBB0_8
# BB#9:                                 # %if.then48
	movq	$-1, %r10
	movq	%rax, %rcx
	shlq	%cl, %r10
	notq	%r10
	andq	%r9, %r10
	movq	%r10, 32(%rdi)
	jmp	.LBB0_10
.LBB0_8:
	movq	%r9, %r10
.LBB0_10:                               # %if.end58
	movl	16(%r11), %ecx
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB0_13
# BB#11:                                # %land.lhs.true
	andq	$255, %rcx
	cmpq	$6, %rcx
	jne	.LBB0_23
# BB#12:                                # %land.lhs.true72
	movzbl	62(%r11), %ecx
	testq	$2, %rcx
	je	.LBB0_23
.LBB0_13:                               # %if.end81
	cmpq	$128, %rbx
	je	.LBB0_20
# BB#14:                                # %land.lhs.true83
	cmpq	$65, %rbx
	jb	.LBB0_17
# BB#15:                                # %cond.true
	leaq	-65(%rax), %rcx
	movq	$1, %rbx
	shlq	%cl, %rbx
	testq	%rbx, %rdx
	je	.LBB0_20
# BB#16:                                # %if.then104
	addq	$-64, %rax
	movq	$-1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, %rdx
	movq	%rdx, 40(%rdi)
	jmp	.LBB0_20
.LBB0_17:                               # %cond.false
	leaq	-1(%rax), %rcx
	movq	$1, %r11
	shlq	%cl, %r11
	testq	%r11, %r10
	je	.LBB0_20
# BB#18:                                # %if.else111
	movq	$-1, 40(%rdi)
	movq	$-1, %rdx
	cmpq	$63, %rbx
	ja	.LBB0_20
# BB#19:                                # %if.then116
	movq	$-1, %rdx
	movq	$-1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	orq	%rbx, %r10
	movq	%r10, 32(%rdi)
.LBB0_20:                               # %if.end125
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%r9, %r10
	orq	%rsi, %r10
	xorq	%r8, %rdx
	orq	%r10, %rdx
	je	.LBB0_21
# BB#22:                                # %if.end125
	movabsq	$1, %rsi
	jmp	.LBB0_23
.LBB0_21:
	movabsq	$0, %rsi
.LBB0_23:                               # %return
	movq	%rsi, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	force_fit_type, .Ltmp4-force_fit_type
	.cfi_endproc

	.globl	add_double
	.align	16, 0x90
	.type	add_double,@function
add_double:                             # @add_double
	.cfi_startproc
# BB#0:                                 # %entry
	addq	%rdx, %rdi
	sbbq	%rdx, %rdx
	leaq	(%rcx,%rsi), %rax
	subq	%rdx, %rax
	movq	%rdi, (%r8)
	movq	%rax, (%r9)
	xorq	%rsi, %rcx
	notq	%rcx
	xorq	%rsi, %rax
	andq	%rcx, %rax
	movq	$63, %rcx
	shrq	%cl, %rax
	retq
.Ltmp5:
	.size	add_double, .Ltmp5-add_double
	.cfi_endproc

	.globl	neg_double
	.align	16, 0x90
	.type	neg_double,@function
neg_double:                             # @neg_double
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB2_1
# BB#2:                                 # %if.else
	negq	%rdi
	movq	%rdi, (%rdx)
	notq	%rsi
	movq	%rsi, (%rcx)
	xorq	%rax, %rax
	retq
.LBB2_1:                                # %if.then
	movq	$0, (%rdx)
	movq	%rsi, %rax
	negq	%rax
	movq	%rax, (%rcx)
	andq	%rsi, %rax
	movq	$63, %rcx
	shrq	%cl, %rax
	retq
.Ltmp6:
	.size	neg_double, .Ltmp6-neg_double
	.cfi_endproc

	.globl	mul_double
	.align	16, 0x90
	.type	mul_double,@function
mul_double:                             # @mul_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp13:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp15:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp16:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 48
.Ltmp18:
	.cfi_offset %rbx, -48
.Ltmp19:
	.cfi_offset %r12, -40
.Ltmp20:
	.cfi_offset %r13, -32
.Ltmp21:
	.cfi_offset %r14, -24
.Ltmp22:
	.cfi_offset %r15, -16
	movq	%r9, -120(%rsp)         # 8-byte Spill
	movq	%r8, -128(%rsp)         # 8-byte Spill
	movq	%rcx, %rax
	movq	%rax, -112(%rsp)        # 8-byte Spill
	movq	%rdi, -104(%rsp)        # 8-byte Spill
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%rdi, %r9
	andq	%r11, %r9
	movq	%r9, -32(%rsp)
	movq	$32, %rcx
	movq	%rdi, %rbx
	shrq	%cl, %rbx
	movq	%rbx, -24(%rsp)
	movq	%rsi, %rcx
	andq	%r11, %rcx
	movq	%rcx, -16(%rsp)
	movq	$32, %rcx
	movq	%rsi, %rbx
	shrq	%cl, %rbx
	movq	%rbx, -8(%rsp)
	movq	%rdx, %r8
	andq	%r11, %r8
	movq	$32, %rcx
	movq	%rdx, %r15
	shrq	%cl, %r15
	movq	%rax, %r12
	andq	%r11, %r12
	movq	$32, %rcx
	movq	%rax, %r13
	shrq	%cl, %r13
	movq	$0, -40(%rsp)
	movq	$0, -48(%rsp)
	movq	$0, -56(%rsp)
	movq	$0, -64(%rsp)
	movq	$0, -72(%rsp)
	movq	$0, -80(%rsp)
	movq	$0, -88(%rsp)
	movq	$0, -96(%rsp)
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_2:                                # %for.cond3.preheader.for.cond3.preheader_crit_edge
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	-24(%rsp,%rbx,8), %r9
	movq	-88(%rsp,%rbx,8), %rcx
	leaq	1(%rbx), %rbx
.LBB3_1:                                # %for.cond3.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %r14
	imulq	%r9, %r14
	addq	%rcx, %r14
	movq	%r14, %rcx
	andq	%r11, %rcx
	movq	%rcx, -96(%rsp,%rbx,8)
	movq	$32, %rcx
	shrq	%cl, %r14
	movq	%r15, %rcx
	imulq	%r9, %rcx
	movq	-88(%rsp,%rbx,8), %r10
	addq	%r14, %rcx
	addq	%rcx, %r10
	movq	%r10, %rcx
	andq	%r11, %rcx
	movq	%rcx, -88(%rsp,%rbx,8)
	movq	$32, %rcx
	shrq	%cl, %r10
	movq	%r12, %rcx
	imulq	%r9, %rcx
	movq	-80(%rsp,%rbx,8), %rax
	addq	%r10, %rcx
	addq	%rcx, %rax
	movq	%rax, %rcx
	andq	%r11, %rcx
	movq	%rcx, -80(%rsp,%rbx,8)
	movq	$32, %rcx
	shrq	%cl, %rax
	imulq	%r13, %r9
	movq	-72(%rsp,%rbx,8), %rdi
	addq	%rax, %r9
	addq	%r9, %rdi
	movq	%rdi, %rax
	andq	%r11, %rax
	movq	$32, %rcx
	shrq	%cl, %rdi
	cmpq	$3, %rbx
	movq	%rax, -72(%rsp,%rbx,8)
	movq	%rdi, -64(%rsp,%rbx,8)
	jne	.LBB3_2
# BB#3:                                 # %for.end19
	movq	-88(%rsp), %rax
	movq	-96(%rsp), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	addq	%rdi, %rax
	movq	-128(%rsp), %rcx        # 8-byte Reload
	movq	%rax, (%rcx)
	movq	-72(%rsp), %r8
	movq	-80(%rsp), %rax
	movq	$32, %rcx
	shlq	%cl, %r8
	addq	%rax, %r8
	movq	-120(%rsp), %rax        # 8-byte Reload
	movq	%r8, (%rax)
	movq	-64(%rsp), %rdi
	movq	-56(%rsp), %r9
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	-40(%rsp), %rax
	movq	-48(%rsp), %rbx
	addq	%rdi, %r9
	movq	$32, %rcx
	shlq	%cl, %rax
	addq	%rbx, %rax
	testq	%rsi, %rsi
	movq	-112(%rsp), %rbx        # 8-byte Reload
	js	.LBB3_5
# BB#4:
	movq	-104(%rsp), %rdi        # 8-byte Reload
	jmp	.LBB3_9
.LBB3_5:                                # %if.then
	testq	%rdx, %rdx
	je	.LBB3_6
# BB#7:                                 # %if.else.i73
	negq	%rdx
	movq	%rbx, %rcx
	notq	%rcx
	jmp	.LBB3_8
.LBB3_6:                                # %if.then.i70
	movq	%rbx, %rcx
	negq	%rcx
	xorq	%rdx, %rdx
.LBB3_8:                                # %neg_double.exit75
	movq	-104(%rsp), %rdi        # 8-byte Reload
	addq	%rdx, %r9
	sbbq	%rdx, %rdx
	addq	%rax, %rcx
	subq	%rdx, %rcx
	movq	%rcx, %rax
.LBB3_9:                                # %if.end
	testq	%rbx, %rbx
	jns	.LBB3_14
# BB#10:                                # %if.then25
	testq	%rdi, %rdi
	je	.LBB3_11
# BB#12:                                # %if.else.i
	negq	%rdi
	notq	%rsi
	jmp	.LBB3_13
.LBB3_11:                               # %if.then.i
	negq	%rsi
	xorq	%rdi, %rdi
.LBB3_13:                               # %neg_double.exit
	addq	%rdi, %r9
	sbbq	%rcx, %rcx
	addq	%rax, %rsi
	subq	%rcx, %rsi
	movq	%rsi, %rax
.LBB3_14:                               # %if.end28
	testq	%r8, %r8
	js	.LBB3_15
# BB#16:                                # %cond.false
	orq	%r9, %rax
	jmp	.LBB3_17
.LBB3_15:                               # %cond.true
	andq	%r9, %rax
	notq	%rax
.LBB3_17:                               # %cond.end
	testq	%rax, %rax
	je	.LBB3_18
# BB#19:                                # %cond.end
	movabsq	$1, %rax
	jmp	.LBB3_20
.LBB3_18:
	movabsq	$0, %rax
.LBB3_20:                               # %cond.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp23:
	.size	mul_double, .Ltmp23-mul_double
	.cfi_endproc

	.globl	lshift_double
	.align	16, 0x90
	.type	lshift_double,@function
lshift_double:                          # @lshift_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 24
.Ltmp29:
	.cfi_offset %rbx, -24
.Ltmp30:
	.cfi_offset %r14, -16
	movq	%rcx, %r10
	testq	%rdx, %rdx
	js	.LBB4_1
# BB#15:                                # %if.end
	cmpq	$128, %rdx
	jl	.LBB4_18
# BB#16:                                # %if.then2
	movq	$0, (%r9)
	jmp	.LBB4_17
.LBB4_1:                                # %if.then
	movq	24(%rsp), %r11
	movq	%rdx, %rax
	negq	%rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r11
	movq	$63, %rcx
	movq	%rsi, %rbx
	sarq	%cl, %rbx
	testq	%r11, %r11
	je	.LBB4_3
# BB#2:                                 # %if.then
	movq	%rbx, %r11
.LBB4_3:                                # %if.then
	cmpq	$128, %rax
	jl	.LBB4_5
# BB#4:                                 # %if.then.i
	movq	$0, (%r9)
	movq	$0, (%r8)
	jmp	.LBB4_9
.LBB4_18:                               # %if.else
	cmpq	$64, %rdx
	jl	.LBB4_20
# BB#19:                                # %if.then4
	addq	$-64, %rdx
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	movq	%rdi, (%r9)
.LBB4_17:                               # %if.end13
	movq	$0, (%r8)
	xorq	%rdi, %rdi
	jmp	.LBB4_21
.LBB4_5:                                # %if.else.i
	cmpq	$64, %rax
	jl	.LBB4_7
# BB#6:                                 # %if.then2.i
	movq	$0, (%r9)
	movq	$-64, %rcx
	subq	%rdx, %rcx
	shrq	%cl, %rsi
	jmp	.LBB4_8
.LBB4_20:                               # %if.else6
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	movq	$63, %rcx
	subq	%rdx, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	movq	$1, %rcx
	shrq	%cl, %rax
	orq	%rsi, %rax
	movq	%rax, (%r9)
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	movq	%rdi, (%r8)
.LBB4_21:                               # %if.end13
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r10, %rax
	andq	%rdx, %rax
	cmpq	$65, %rax
	jb	.LBB4_23
# BB#22:                                # %cond.true
	movq	(%r9), %rdi
	leaq	-65(%r10), %rcx
	andq	%rdx, %rcx
	sarq	%cl, %rdi
	jmp	.LBB4_24
.LBB4_23:                               # %cond.false
	leaq	-1(%r10), %rcx
	andq	%rdx, %rcx
	shrq	%cl, %rdi
.LBB4_24:                               # %cond.end
	cmpq	$127, %rax
	ja	.LBB4_28
# BB#25:                                # %if.else24
	andq	$1, %rdi
	negq	%rdi
	cmpq	$64, %rax
	jb	.LBB4_27
# BB#26:                                # %if.then26
	addq	$-64, %r10
	movq	$-1, %rax
	movq	(%r9), %rdx
	movq	%r10, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, (%r9)
	jmp	.LBB4_28
.LBB4_27:                               # %if.else35
	movq	%rdi, (%r9)
	movq	$-1, %rax
	movq	(%r8), %rdx
	movq	%r10, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r10, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, (%r8)
	jmp	.LBB4_28
.LBB4_7:                                # %if.else5.i
	movq	%rsi, %rbx
	movq	%rax, %rcx
	shrq	%cl, %rbx
	movq	%rbx, (%r9)
	movq	%rax, %rcx
	shrq	%cl, %rdi
	leaq	63(%rdx), %rcx
	shlq	%cl, %rsi
	addq	%rsi, %rsi
	orq	%rdi, %rsi
.LBB4_8:                                # %if.end11.i
	movq	%rsi, (%r8)
.LBB4_9:                                # %if.end11.i
	andq	%r14, %r10
	cmpq	%rax, %r10
	jle	.LBB4_10
# BB#11:                                # %if.else15.i
	addq	%rdx, %r10
	cmpq	$127, %r10
	jg	.LBB4_28
# BB#12:                                # %if.else21.i
	cmpq	$64, %r10
	jl	.LBB4_14
# BB#13:                                # %if.then26.i
	addq	$-64, %r10
	movq	$-1, %rax
	movq	(%r9), %rdx
	movq	%r10, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r10, %rcx
	shlq	%cl, %r11
	orq	%rdx, %r11
	movq	%r11, (%r9)
	jmp	.LBB4_28
.LBB4_10:                               # %if.then14.i
	movq	%r11, (%r9)
	movq	%r11, (%r8)
	jmp	.LBB4_28
.LBB4_14:                               # %if.else36.i
	movq	%r11, (%r9)
	movq	$-1, %rax
	movq	(%r8), %rdx
	movq	%r10, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r10, %rcx
	shlq	%cl, %r11
	orq	%rdx, %r11
	movq	%r11, (%r8)
.LBB4_28:                               # %if.end44
	popq	%rbx
	popq	%r14
	retq
.Ltmp31:
	.size	lshift_double, .Ltmp31-lshift_double
	.cfi_endproc

	.globl	rshift_double
	.align	16, 0x90
	.type	rshift_double,@function
rshift_double:                          # @rshift_double
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rsp), %r10d
	movq	%rcx, %r11
	movq	$63, %rcx
	movq	%rsi, %rax
	sarq	%cl, %rax
	testq	%r10, %r10
	je	.LBB5_2
# BB#1:                                 # %entry
	movq	%rax, %r10
.LBB5_2:                                # %entry
	cmpq	$128, %rdx
	jl	.LBB5_4
# BB#3:                                 # %if.then
	movq	$0, (%r9)
	movq	$0, (%r8)
	jmp	.LBB5_8
.LBB5_4:                                # %if.else
	cmpq	$64, %rdx
	jl	.LBB5_6
# BB#5:                                 # %if.then2
	movq	$0, (%r9)
	leaq	-64(%rdx), %rcx
	shrq	%cl, %rsi
	jmp	.LBB5_7
.LBB5_6:                                # %if.else5
	movq	%rsi, %rax
	movq	%rdx, %rcx
	shrq	%cl, %rax
	movq	%rax, (%r9)
	movq	%rdx, %rcx
	shrq	%cl, %rdi
	movq	$63, %rcx
	subq	%rdx, %rcx
	shlq	%cl, %rsi
	addq	%rsi, %rsi
	orq	%rdi, %rsi
.LBB5_7:                                # %if.end11
	movq	%rsi, (%r8)
.LBB5_8:                                # %if.end11
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r11
	subq	%rdx, %r11
	jle	.LBB5_9
# BB#10:                                # %if.else15
	cmpq	$127, %r11
	jg	.LBB5_15
# BB#11:                                # %if.else21
	cmpq	$64, %r11
	jl	.LBB5_13
# BB#12:                                # %if.then26
	addq	$-64, %r11
	movq	$-1, %rax
	movq	(%r9), %rdx
	movq	%r11, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r11, %rcx
	shlq	%cl, %r10
	orq	%rdx, %r10
	movq	%r10, (%r9)
	retq
.LBB5_9:                                # %if.then14
	movq	%r10, (%r9)
	jmp	.LBB5_14
.LBB5_13:                               # %if.else36
	movq	%r10, (%r9)
	movq	$-1, %rax
	movq	(%r8), %rdx
	movq	%r11, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%r11, %rcx
	shlq	%cl, %r10
	orq	%rdx, %r10
.LBB5_14:                               # %if.end48
	movq	%r10, (%r8)
.LBB5_15:                               # %if.end48
	retq
.Ltmp32:
	.size	rshift_double, .Ltmp32-rshift_double
	.cfi_endproc

	.globl	lrotate_double
	.align	16, 0x90
	.type	lrotate_double,@function
lrotate_double:                         # @lrotate_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp41:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp42:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp44:
	.cfi_def_cfa_offset 80
.Ltmp45:
	.cfi_offset %rbx, -48
.Ltmp46:
	.cfi_offset %r12, -40
.Ltmp47:
	.cfi_offset %r13, -32
.Ltmp48:
	.cfi_offset %r14, -24
.Ltmp49:
	.cfi_offset %r15, -16
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, %r15
	movq	%rcx, %r10
	movq	%rsi, %r12
	movq	%rdi, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r10, %r14
	movq	%rdx, %rax
	cqto
	idivq	%r14
	movq	$63, %rcx
	movq	%rdx, %rbx
	sarq	%cl, %rbx
	andq	%r14, %rbx
	addq	%rdx, %rbx
	leaq	24(%rsp), %r8
	leaq	16(%rsp), %r9
	movq	$0, (%rsp)
                                        # kill: RDI<def> R13<kill>
                                        # kill: RSI<def> R12<kill>
	movq	%rbx, %rdx
	movq	%r10, %rcx
	callq	lshift_double
	subq	%rbx, %r14
	xorq	%rax, %rax
	cmpq	$127, %r14
	movabsq	$0, %rdx
	movabsq	$0, %rsi
	jg	.LBB6_4
# BB#1:                                 # %if.else.i
	cmpq	$64, %r14
	jl	.LBB6_3
# BB#2:                                 # %if.then2.i
	addq	$-64, %r14
	movq	%r14, %rcx
	shrq	%cl, %r12
	movq	%r12, %rdx
	xorq	%rsi, %rsi
	jmp	.LBB6_4
.LBB6_3:                                # %if.else5.i
	movq	%r12, %rsi
	movq	%r14, %rcx
	shrq	%cl, %rsi
	movq	%r14, %rcx
	shrq	%cl, %r13
	movq	$63, %rcx
	subq	%r14, %rcx
	shlq	%cl, %r12
	addq	%r12, %r12
	orq	%r13, %r12
	movq	%r12, %rdx
.LBB6_4:                                # %if.end11.i
	testq	%rbx, %rbx
	jle	.LBB6_5
# BB#6:                                 # %if.else15.i
	cmpq	$127, %rbx
	jle	.LBB6_8
# BB#7:
	movq	%rsi, %rax
	jmp	.LBB6_11
.LBB6_5:
	xorq	%rdx, %rdx
	jmp	.LBB6_11
.LBB6_8:                                # %if.else21.i
	cmpq	$64, %rbx
	jl	.LBB6_10
# BB#9:                                 # %if.then26.i
	addq	$-64, %rbx
	movq	$-1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	%rsi, %rax
	jmp	.LBB6_11
.LBB6_10:                               # %if.else36.i
	movq	$-1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rsi, %rdx
.LBB6_11:                               # %rshift_double.exit
	movq	24(%rsp), %rcx
	orq	%rdx, %rcx
	movq	%rcx, (%r15)
	movq	16(%rsp), %rcx
	orq	%rax, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rcx, (%rax)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp50:
	.size	lrotate_double, .Ltmp50-lrotate_double
	.cfi_endproc

	.globl	rrotate_double
	.align	16, 0x90
	.type	rrotate_double,@function
rrotate_double:                         # @rrotate_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp59:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp60:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp61:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp62:
	.cfi_def_cfa_offset 80
.Ltmp63:
	.cfi_offset %rbx, -48
.Ltmp64:
	.cfi_offset %r12, -40
.Ltmp65:
	.cfi_offset %r13, -32
.Ltmp66:
	.cfi_offset %r14, -24
.Ltmp67:
	.cfi_offset %r15, -16
	movq	%r9, %r14
	movq	%r8, %r15
	movq	%rcx, %r10
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%r10, %rbx
	movq	%rdx, %rax
	cqto
	idivq	%rbx
	movq	$63, %rcx
	movq	%rdx, %rax
	sarq	%cl, %rax
	andq	%rbx, %rax
	addq	%rdx, %rax
	xorq	%r12, %r12
	cmpq	$127, %rax
	movabsq	$0, %r13
	movabsq	$0, %rdx
	jg	.LBB7_4
# BB#1:                                 # %if.else.i
	cmpq	$64, %rax
	jl	.LBB7_3
# BB#2:                                 # %if.then2.i
	leaq	-64(%rax), %rcx
	movq	%rsi, %r13
	shrq	%cl, %r13
	xorq	%rdx, %rdx
	jmp	.LBB7_4
.LBB7_3:                                # %if.else5.i
	movq	%rsi, %rdx
	movq	%rax, %rcx
	shrq	%cl, %rdx
	movq	%rdi, %r8
	movq	%rax, %rcx
	shrq	%cl, %r8
	movq	$63, %rcx
	subq	%rax, %rcx
	movq	%rsi, %r13
	shlq	%cl, %r13
	addq	%r13, %r13
	orq	%r8, %r13
.LBB7_4:                                # %if.end11.i
	subq	%rax, %rbx
	jle	.LBB7_5
# BB#6:                                 # %if.else15.i
	cmpq	$127, %rbx
	jle	.LBB7_8
# BB#7:
	movq	%rdx, %r12
	jmp	.LBB7_11
.LBB7_5:
	xorq	%r13, %r13
	jmp	.LBB7_11
.LBB7_8:                                # %if.else21.i
	cmpq	$64, %rbx
	jl	.LBB7_10
# BB#9:                                 # %if.then26.i
	leaq	-64(%rbx), %rcx
	movq	$-1, %rax
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rdx
	movq	%rdx, %r12
	jmp	.LBB7_11
.LBB7_10:                               # %if.else36.i
	movq	$-1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %r13
.LBB7_11:                               # %rshift_double.exit
	leaq	24(%rsp), %r8
	leaq	16(%rsp), %r9
	movq	$0, (%rsp)
	movq	%rbx, %rdx
	movq	%r10, %rcx
	callq	lshift_double
	movq	24(%rsp), %rax
	orq	%r13, %rax
	movq	%rax, (%r15)
	movq	16(%rsp), %rax
	orq	%r12, %rax
	movq	%rax, (%r14)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp68:
	.size	rrotate_double, .Ltmp68-rrotate_double
	.cfi_endproc

	.globl	div_and_round_double
	.align	16, 0x90
	.type	div_and_round_double,@function
div_and_round_double:                   # @div_and_round_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp78:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp79:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp80:
	.cfi_def_cfa_offset 288
.Ltmp81:
	.cfi_offset %rbx, -48
.Ltmp82:
	.cfi_offset %r12, -40
.Ltmp83:
	.cfi_offset %r13, -32
.Ltmp84:
	.cfi_offset %r14, -24
.Ltmp85:
	.cfi_offset %r15, -16
	movq	312(%rsp), %r12
	movq	288(%rsp), %r11
	movq	%r9, %rax
	orq	%r8, %rax
	movq	%rcx, %r13
	jne	.LBB8_1
# BB#2:                                 # %entry
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movabsq	$1, %rcx
	jmp	.LBB8_3
.LBB8_1:
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movabsq	$0, %rcx
.LBB8_3:                                # %entry
	movq	$1, %r14
	je	.LBB8_5
# BB#4:                                 # %entry
	movq	%r8, %r14
.LBB8_5:                                # %entry
	xorq	%rdi, %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rsi
	je	.LBB8_7
# BB#6:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %r10
	movq	%r13, %rsi
	movq	%r9, %rbx
	jmp	.LBB8_20
.LBB8_7:                                # %if.then2
	xorq	%rdi, %rdi
	testq	%r13, %r13
	js	.LBB8_9
# BB#8:
	movq	%rdx, %r10
	movq	%r13, %rsi
	jmp	.LBB8_15
.LBB8_9:                                # %if.then4
	testq	%rdx, %rdx
	je	.LBB8_11
# BB#10:                                # %neg_double.exit.thread
	movq	%rdx, %r10
	negq	%r10
	movq	%r13, %rsi
	notq	%rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jmp	.LBB8_15
.LBB8_11:                               # %neg_double.exit
	movq	%r13, %rsi
	negq	%rsi
	xorq	%r10, %r10
	testq	%r13, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	jns	.LBB8_15
# BB#12:                                # %land.lhs.true6
	movq	%r14, %rax
	andq	%r9, %rax
	cmpq	$-1, %rax
	movq	$1, %rax
	je	.LBB8_14
# BB#13:                                # %land.lhs.true6
	movq	%rcx, %rax
.LBB8_14:                               # %land.lhs.true6
	movq	%rax, %rcx
.LBB8_15:                               # %if.end10
	testq	%r9, %r9
	js	.LBB8_17
# BB#16:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	jmp	.LBB8_20
.LBB8_17:                               # %if.then12
	notq	%rdi
	testq	%r14, %r14
	je	.LBB8_18
# BB#19:                                # %if.else.i426
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	negq	%r14
	movq	%r9, %rbx
	notq	%rbx
	jmp	.LBB8_20
.LBB8_18:                               # %if.then.i423
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	negq	%rbx
	xorq	%r14, %r14
.LBB8_20:                               # %if.end16
	movq	%rbx, %rax
	orq	%rsi, %rax
	je	.LBB8_21
# BB#22:                                # %if.end21
	testq	%rsi, %rsi
	je	.LBB8_23
# BB#24:                                # %if.end24
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movabsq	$4294967296, %r8        # imm = 0x100000000
	movq	$0, 152(%rsp)
	movq	$0, 144(%rsp)
	movq	$0, 136(%rsp)
	movq	$0, 128(%rsp)
	movq	$0, 224(%rsp)
	movq	$0, 216(%rsp)
	movq	$0, 208(%rsp)
	movq	$0, 200(%rsp)
	movq	$0, 192(%rsp)
	movq	$0, 184(%rsp)
	movq	$0, 176(%rsp)
	movq	$0, 168(%rsp)
	movq	$0, 160(%rsp)
	movq	%r10, %r11
	andq	%r15, %r11
	movq	%r11, 192(%rsp)
	movq	$32, %rcx
	shrq	%cl, %r10
	movq	%r10, 200(%rsp)
	movq	%rsi, %rdi
	andq	%r15, %rdi
	movq	%rdi, 208(%rsp)
	movq	$32, %rcx
	shrq	%cl, %rsi
	movq	%rsi, 216(%rsp)
	movq	%r14, %r13
	andq	%r15, %r13
	movq	%r13, 160(%rsp)
	movq	$32, %rcx
	movq	%r14, %r12
	shrq	%cl, %r12
	movq	%r12, 168(%rsp)
	movq	%r14, %rax
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rbx, %r14
	andq	%r15, %r14
	movq	$32, %rcx
	movq	%rbx, %r9
	shrq	%cl, %r9
	testq	%rbx, %rbx
	movq	%r14, 176(%rsp)
	movq	%r9, 184(%rsp)
	jne	.LBB8_25
# BB#28:                                # %if.end24
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	cmpq	%r8, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rcx
	movq	$3, %rbx
	jae	.LBB8_26
# BB#29:                                # %for.cond.preheader
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	movq	24(%rsp), %rbx          # 8-byte Reload
	divq	%rbx
	movq	%rax, %r8
	movq	%r8, 152(%rsp)
	movq	$32, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %rdi
	xorq	%rdx, %rdx
	movq	%rdi, %rax
	divq	%rbx
	movq	%rax, %r12
	movq	%r12, 144(%rsp)
	movq	$32, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %r10
	xorq	%rdx, %rdx
	movq	%r10, %rax
	divq	%rbx
	movq	%rax, %rsi
	movq	%rsi, 136(%rsp)
	movq	$32, %rcx
	shlq	%cl, %rdx
	orq	%rdx, %r11
	xorq	%rdx, %rdx
	movq	%r11, %rax
	divq	%rbx
	movq	%rax, 128(%rsp)
	jmp	.LBB8_62
.LBB8_21:                               # %if.then20
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$0, (%r12)
	movq	296(%rsp), %rcx
	movq	$0, (%rcx)
	xorq	%rdx, %rdx
	movq	%r10, %rax
	divq	%r14
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%rax, (%r11)
	movq	304(%rsp), %rbx
	jmp	.LBB8_63
.LBB8_23:                               # %if.then23
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$0, (%r11)
	movq	296(%rsp), %rcx
	movq	$0, (%rcx)
	movq	$0, (%r12)
	movq	304(%rsp), %rbx
	movq	%r10, (%rbx)
	jmp	.LBB8_63
.LBB8_25:
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r9, %rcx
	movq	$3, %rbx
	jmp	.LBB8_26
	.align	16, 0x90
.LBB8_27:                               # %for.cond37.for.cond37_crit_edge
                                        #   in Loop: Header=BB8_26 Depth=1
	movq	152(%rsp,%rbx,8), %rcx
	decq	%rbx
.LBB8_26:                               # %for.cond37
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB8_27
# BB#30:                                # %if.then41
	incq	%rcx
	xorq	%r8, %r8
	xorq	%rdx, %rdx
	movabsq	$4294967296, %rax       # imm = 0x100000000
	divq	%rcx
	cmpq	$2, %rax
	jb	.LBB8_39
# BB#31:                                # %for.cond52.preheader
	imulq	%rax, %r11
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r11, %rcx
	andq	%rdx, %rcx
	movq	%rcx, 192(%rsp)
	movq	$32, %rcx
	shrq	%cl, %r11
	imulq	%rax, %r10
	addq	%r11, %r10
	movq	%r10, %rcx
	andq	%rdx, %rcx
	movq	%rcx, 200(%rsp)
	movq	$32, %rcx
	shrq	%cl, %r10
	imulq	%rax, %rdi
	addq	%r10, %rdi
	movq	%rdi, %rcx
	andq	%rdx, %rcx
	movq	%rcx, 208(%rsp)
	movq	$32, %rcx
	shrq	%cl, %rdi
	imulq	%rax, %rsi
	addq	%rdi, %rsi
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	movq	%rcx, 216(%rsp)
	movq	$32, %rcx
	shrq	%cl, %rsi
	movq	%rsi, 224(%rsp)
	imulq	%rax, %r13
	movq	%r13, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shrq	%cl, %r13
	testq	%rsi, %rsi
	movq	%rsi, 160(%rsp)
	je	.LBB8_33
# BB#32:                                # %for.cond52.preheader
	xorq	%rbx, %rbx
.LBB8_33:                               # %for.cond52.preheader
	imulq	%rax, %r12
	addq	%r13, %r12
	movq	%r12, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shrq	%cl, %r12
	testq	%rsi, %rsi
	movq	%rsi, 168(%rsp)
	je	.LBB8_35
# BB#34:                                # %for.cond52.preheader
	movq	$1, %rbx
.LBB8_35:                               # %for.cond52.preheader
	imulq	%rax, %r14
	addq	%r12, %r14
	movq	%r14, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shrq	%cl, %r14
	testq	%rsi, %rsi
	movq	%rsi, 176(%rsp)
	je	.LBB8_37
# BB#36:                                # %for.cond52.preheader
	movq	$2, %rbx
.LBB8_37:                               # %for.cond52.preheader
	imulq	%rax, %r9
	addq	%r14, %r9
	andq	%rdx, %r9
	movq	%r9, 184(%rsp)
	je	.LBB8_39
# BB#38:                                # %for.cond52.preheader
	movq	$3, %rbx
.LBB8_39:                               # %if.end84
	movq	$4, %r11
	subq	%rbx, %r11
	movq	$32, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$0, %rsi
	movabsq	$0, %rax
	jle	.LBB8_62
# BB#40:                                # %for.body88.lr.ph
	movq	$32, %rcx
	movq	%rbx, %rsi
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	-1(%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	160(%rsp,%rdx,8), %rdi
	movq	%rdi, 88(%rsp)          # 8-byte Spill
	movq	$3, %rax
	subq	%rsi, %rax
	testq	%rdx, %rdx
	js	.LBB8_55
# BB#41:                                # %for.body88.lr.ph.for.body88.lr.ph.split_crit_edge
	leaq	1(%rsi), %r10
	movq	$3, %r8
	subq	%rsi, %r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movabsq	$0, %r13
	movabsq	$1, %r9
	movq	%rsi, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB8_42:                               # %for.body88
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_47 Depth 2
                                        #     Child Loop BB8_52 Depth 2
	movq	%rax, %rsi
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	addq	%rdx, %r11
	movq	$32, %rcx
	movq	%r11, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	192(%rsp,%r14,8), %rax
	leaq	(%rsi,%rdx), %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	192(%rsp,%rdx,8), %r12
	addq	%rsi, %r12
	cmpq	%rdi, %rax
	movq	%r15, %rax
	je	.LBB8_44
# BB#43:                                # %if.then103
                                        #   in Loop: Header=BB8_42 Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rdi
.LBB8_44:                               # %if.end108
                                        #   in Loop: Header=BB8_42 Depth=1
	movq	%rdi, %rcx
	imulq	%rax, %rcx
	subq	%rcx, %r12
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	cmpq	%rcx, %r12
	jae	.LBB8_46
# BB#45:                                # %land.lhs.true114
                                        #   in Loop: Header=BB8_42 Depth=1
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	160(%rsp,%rcx,8), %rdx
	movq	$32, %rcx
	shlq	%cl, %r12
	addq	$-2, %r11
	movq	$32, %rcx
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	movq	192(%rsp,%r11,8), %rcx
	imulq	%rax, %rdx
	addq	%r12, %rcx
	cmpq	%rdx, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	subq	%rcx, %rax
.LBB8_46:                               # %for.body130.lr.ph
                                        #   in Loop: Header=BB8_42 Depth=1
	xorq	%rcx, %rcx
	movq	$1, %rdi
	leaq	192(%rsp), %rbx
	.align	16, 0x90
.LBB8_47:                               # %for.body130
                                        #   Parent Loop BB8_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	152(%rsp,%rdi,8), %rsi
	imulq	%rax, %rsi
	addq	%rcx, %rsi
	movq	$32, %rcx
	movq	%rsi, %r11
	shrq	%cl, %r11
	movq	(%rbx,%r8,8), %rdx
	andq	%r15, %rsi
	subq	%rsi, %rdx
	movq	%rdx, %rcx
	andq	%r15, %rcx
	movq	%rcx, (%rbx,%r8,8)
	movq	$32, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%r13, %rcx
	je	.LBB8_49
# BB#48:                                # %for.body130
                                        #   in Loop: Header=BB8_47 Depth=2
	movq	%r9, %rcx
.LBB8_49:                               # %for.body130
                                        #   in Loop: Header=BB8_47 Depth=2
	addq	%r11, %rcx
	movq	%rdi, %rsi
	andq	%r15, %rsi
	movq	%r10, %rdx
	andq	%r15, %rdx
	incq	%rdi
	addq	$8, %rbx
	cmpq	%rdx, %rsi
	jne	.LBB8_47
# BB#50:                                # %for.end151
                                        #   in Loop: Header=BB8_42 Depth=1
	movq	192(%rsp,%r14,8), %rsi
	cmpq	%rcx, %rsi
	jae	.LBB8_54
# BB#51:                                # %for.body161.lr.ph
                                        #   in Loop: Header=BB8_42 Depth=1
	decq	%rax
	xorq	%rdi, %rdi
	movq	$1, %r11
	leaq	192(%rsp), %rbx
	.align	16, 0x90
.LBB8_52:                               # %for.body161
                                        #   Parent Loop BB8_42 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx,%r8,8), %rsi
	movq	152(%rsp,%r11,8), %rcx
	addq	%rdi, %rsi
	addq	%rcx, %rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	andq	%r15, %rsi
	movq	%rsi, (%rbx,%r8,8)
	movq	%r11, %rcx
	andq	%r15, %rcx
	incq	%r11
	addq	$8, %rbx
	cmpq	%rdx, %rcx
	jne	.LBB8_52
# BB#53:                                # %for.end176
                                        #   in Loop: Header=BB8_42 Depth=1
	addq	%rdi, 192(%rsp,%r14,8)
.LBB8_54:                               # %if.end180
                                        #   in Loop: Header=BB8_42 Depth=1
	movq	104(%rsp), %r11         # 8-byte Reload
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, 128(%rsp,%r8,8)
	leaq	-1(%r11), %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%r8
	testq	%rdx, %rdx
	movq	80(%rsp), %rdx          # 8-byte Reload
	jg	.LBB8_42
	jmp	.LBB8_61
.LBB8_55:                               # %for.body88.lr.ph.split.us
	movq	$3, %r8
	subq	%rsi, %r8
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rsi, %r10
	.align	16, 0x90
.LBB8_56:                               # %for.body88.us
                                        # =>This Inner Loop Header: Depth=1
	movq	%r11, %rbx
	movq	%rax, %r11
	addq	%r10, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdi, %r14
	movq	192(%rsp,%rax,8), %rdi
	leaq	(%r11,%r10), %rdx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	192(%rsp,%rdx,8), %rsi
	addq	%rax, %rsi
	cmpq	%r14, %rdi
	movq	%r14, %rdi
	movq	%r9, %rax
	je	.LBB8_58
# BB#57:                                # %if.then103.us
                                        #   in Loop: Header=BB8_56 Depth=1
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%rdi
.LBB8_58:                               # %if.end108.us
                                        #   in Loop: Header=BB8_56 Depth=1
	movq	%rdi, %rcx
	imulq	%rax, %rcx
	subq	%rcx, %rsi
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	cmpq	%rcx, %rsi
	jae	.LBB8_60
# BB#59:                                # %land.lhs.true114.us
                                        #   in Loop: Header=BB8_56 Depth=1
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	160(%rsp,%rcx,8), %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	addq	$-2, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	192(%rsp,%rbx,8), %rcx
	imulq	%rax, %rdx
	addq	%rsi, %rcx
	cmpq	%rdx, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	subq	%rcx, %rax
.LBB8_60:                               # %if.end180.us
                                        #   in Loop: Header=BB8_56 Depth=1
	movq	%rax, 128(%rsp,%r8,8)
	leaq	-1(%r11), %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%r8
	testq	%rdx, %rdx
	jg	.LBB8_56
.LBB8_61:                               # %for.cond86.if.end186.loopexit566_crit_edge
	movq	128(%rsp), %rax
	movq	136(%rsp), %rsi
	movq	144(%rsp), %r12
	movq	152(%rsp), %r8
.LBB8_62:                               # %if.end186
	movq	$32, %rcx
	shlq	%cl, %rsi
	addq	%rax, %rsi
	movq	288(%rsp), %r11
	movq	%rsi, (%r11)
	movq	$32, %rcx
	shlq	%cl, %r8
	addq	%r12, %r8
	movq	296(%rsp), %rcx
	movq	%r8, (%rcx)
	movq	312(%rsp), %rbx
	movq	%rbx, %r12
	movq	304(%rsp), %rbx
	movq	96(%rsp), %r13          # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	56(%rsp), %r8           # 8-byte Reload
.LBB8_63:                               # %finish_up
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rdi
	je	.LBB8_64
# BB#65:                                # %if.then189
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movq	(%r11), %rax
	movq	(%rcx), %rsi
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB8_66
# BB#67:                                # %if.else.i520
	negq	%rax
	movq	%rax, (%r11)
	notq	%rsi
	jmp	.LBB8_68
.LBB8_64:                               # %finish_up.if.end191_crit_edge
	movq	%r13, 96(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	movq	(%rcx), %rsi
	jmp	.LBB8_69
.LBB8_66:                               # %if.then.i517
	movq	$0, (%r11)
	negq	%rsi
.LBB8_68:                               # %if.end191
	movq	%rsi, (%rcx)
.LBB8_69:                               # %if.end191
	movq	(%r11), %rdi
	movq	%r11, %r15
	movq	%r8, %rdx
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%r9, %rcx
	movq	%r9, 64(%rsp)           # 8-byte Spill
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	mul_double
	movq	(%rbx), %rax
	movq	(%r12), %rcx
	testq	%rax, %rax
	je	.LBB8_70
# BB#71:                                # %if.else.i503
	negq	%rax
	movq	%rax, (%rbx)
	notq	%rcx
	jmp	.LBB8_72
.LBB8_70:                               # %if.then.i500
	movq	$0, (%rbx)
	negq	%rcx
.LBB8_72:                               # %neg_double.exit505
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, (%r12)
	movq	(%rbx), %rax
	addq	%rsi, %rax
	sbbq	%rdx, %rdx
	movq	96(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %rcx
	subq	%rdx, %rcx
	movq	%rax, (%rbx)
	movq	%rcx, (%r12)
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	$-62, %rax
	andq	%r14, %rax
	cmpq	$9, %rax
	ja	.LBB8_103
# BB#73:                                # %neg_double.exit505
	jmpq	*.LJTI8_0(,%rax,8)
.LBB8_77:                               # %sw.bb206
	testq	%r14, %r13
	jne	.LBB8_102
# BB#78:                                # %land.lhs.true208
	movq	(%rbx), %rax
	orq	%rcx, %rax
	je	.LBB8_102
# BB#79:                                # %if.then214
	movq	%rsi, %r14
	movq	(%r15), %rax
	movq	296(%rsp), %rcx
	movq	%rcx, %rdx
	movq	(%rdx), %rcx
	addq	$1, %rax
	sbbq	%rsi, %rsi
	andq	$1, %rsi
	jmp	.LBB8_96
.LBB8_74:                               # %sw.bb195
	testq	%r14, %r13
	je	.LBB8_102
# BB#75:                                # %land.lhs.true197
	movq	(%rbx), %rax
	orq	%rcx, %rax
	je	.LBB8_102
# BB#76:                                # %if.then202
	movq	%rsi, %r14
	movq	(%r15), %rax
	movq	296(%rsp), %rcx
	movq	(%rcx), %rsi
	addq	$-1, %rax
	sbbq	%rdx, %rdx
	negq	%rdx
	leaq	-1(%rsi,%rdx), %rsi
	movq	%rax, (%r15)
	movq	%rsi, (%rcx)
	jmp	.LBB8_98
.LBB8_80:                               # %sw.bb218
	movq	(%rbx), %rdx
	testq	%rcx, %rcx
	js	.LBB8_82
# BB#81:
	movq	16(%rsp), %rax          # 8-byte Reload
	jmp	.LBB8_85
.LBB8_82:                               # %if.then227
	testq	%rdx, %rdx
	movq	16(%rsp), %rax          # 8-byte Reload
	je	.LBB8_83
# BB#84:                                # %if.else.i465
	negq	%rdx
	notq	%rcx
	jmp	.LBB8_85
.LBB8_103:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.div_and_round_double, %rdx
	movq	$823, %rsi              # imm = 0x337
	callq	fancy_abort
.LBB8_83:                               # %if.then.i462
	negq	%rcx
	xorq	%rdx, %rdx
.LBB8_85:                               # %if.end229
	testq	%rax, %rax
	movq	%rax, %rdi
	js	.LBB8_87
# BB#86:
	movq	%rsi, %r14
	movq	%rdi, %r15
	jmp	.LBB8_90
.LBB8_87:                               # %if.then232
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB8_88
# BB#89:                                # %if.else.i455
	negq	%rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	notq	%r15
	jmp	.LBB8_90
.LBB8_88:                               # %if.then.i452
	movq	%rdi, %r15
	negq	%r15
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB8_90:                               # %if.end234
	leaq	120(%rsp), %r8
	leaq	112(%rsp), %r9
	movq	$2, %rdi
	movabsq	$0, %rsi
	callq	mul_double
	movq	112(%rsp), %rax
	cmpq	%rax, %r15
	movq	288(%rsp), %r15
	jb	.LBB8_93
# BB#91:                                # %lor.lhs.false238
	jne	.LBB8_102
# BB#92:                                # %lor.lhs.false238
	movq	120(%rsp), %rax
	cmpq	%rax, 24(%rsp)          # 8-byte Folded Reload
	jae	.LBB8_102
.LBB8_93:                               # %if.then244
	movq	296(%rsp), %rdx
	movq	(%rdx), %rsi
	movq	(%r15), %rax
	testq	%rsi, %rsi
	js	.LBB8_94
# BB#95:                                # %if.else249
	addq	$1, %rax
	sbbq	%rcx, %rcx
	andq	$1, %rcx
.LBB8_96:                               # %sw.epilog
	addq	%rcx, %rsi
	jmp	.LBB8_97
.LBB8_94:                               # %if.then247
	addq	$-1, %rax
	sbbq	%rcx, %rcx
	subq	%rcx, %rsi
	decq	%rsi
.LBB8_97:                               # %sw.epilog
	movq	%rax, (%r15)
	movq	%rsi, (%rdx)
.LBB8_98:                               # %sw.epilog
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %r13          # 8-byte Reload
	movq	(%r15), %rdi
	movq	%rbx, %r8
	movq	%r12, %r9
	callq	mul_double
	movq	(%rbx), %rcx
	movq	(%r12), %rax
	testq	%rcx, %rcx
	je	.LBB8_99
# BB#100:                               # %if.else.i416
	negq	%rcx
	movq	%rcx, (%rbx)
	notq	%rax
	jmp	.LBB8_101
.LBB8_99:                               # %if.then.i413
	movq	$0, (%rbx)
	negq	%rax
.LBB8_101:                              # %neg_double.exit418
	movq	%rax, (%r12)
	movq	(%rbx), %rcx
	addq	%rcx, %r14
	sbbq	%rcx, %rcx
	addq	%rax, %r13
	subq	%rcx, %r13
	movq	%r14, (%rbx)
	movq	%r13, (%r12)
.LBB8_102:                              # %cleanup
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp86:
	.size	div_and_round_double, .Ltmp86-div_and_round_double
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_102
	.quad	.LBB8_77
	.quad	.LBB8_74
	.quad	.LBB8_80
	.quad	.LBB8_102
	.quad	.LBB8_77
	.quad	.LBB8_74
	.quad	.LBB8_80
	.quad	.LBB8_103
	.quad	.LBB8_102

	.text
	.globl	size_int_wide
	.align	16, 0x90
	.type	size_int_wide,@function
size_int_wide:                          # @size_int_wide
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	sizetype_tab(,%rsi,8), %rsi
	jmp	size_int_type_wide  # TAILCALL
.Ltmp87:
	.size	size_int_wide, .Ltmp87-size_int_wide
	.cfi_endproc

	.globl	size_int_type_wide
	.align	16, 0x90
	.type	size_int_type_wide,@function
size_int_type_wide:                     # @size_int_type_wide
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp93:
	.cfi_def_cfa_offset 32
.Ltmp94:
	.cfi_offset %rbx, -24
.Ltmp95:
	.cfi_offset %r14, -16
	cmpq	$0, size_int_type_wide.size_htab(%rip)
	movq	%rsi, %r14
	movq	%rdi, %rbx
	jne	.LBB10_2
# BB#1:                                 # %if.then
	movabsq	$size_htab_hash, %rsi
	movabsq	$size_htab_eq, %rdx
	movq	$1024, %rdi             # imm = 0x400
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, size_int_type_wide.size_htab(%rip)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	ggc_add_deletable_htab
	movq	$25, %rdi
	callq	make_node
	movq	%rax, size_int_type_wide.new_const(%rip)
	movabsq	$size_int_type_wide.new_const, %rdi
	movq	$1, %rsi
	callq	ggc_add_tree_root
.LBB10_2:                               # %if.end
	movq	size_int_type_wide.new_const(%rip), %rax
	movq	%rbx, 32(%rax)
	movq	$63, %rcx
	sarq	%cl, %rbx
	movq	%rbx, 40(%rax)
	movq	%r14, 8(%rax)
	movq	size_int_type_wide.new_const(%rip), %rdi
	xorq	%rsi, %rsi
	callq	force_fit_type
	movq	size_int_type_wide.new_const(%rip), %rdx
	movl	16(%rdx), %esi
	andq	$1, %rax
	movq	$18, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rsi, %rcx
	orq	%rdi, %rcx
	movl	%ecx, 16(%rdx)
	movq	size_int_type_wide.new_const(%rip), %rdx
	movl	16(%rdx), %esi
	movq	$19, %rcx
	shlq	%cl, %rax
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rsi, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%rdx)
	movq	size_int_type_wide.size_htab(%rip), %rdi
	movq	size_int_type_wide.new_const(%rip), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_4
# BB#3:                                 # %if.then18
	movq	size_int_type_wide.new_const(%rip), %rbx
	movq	%rbx, (%rax)
	movq	$25, %rdi
	callq	make_node
	movq	%rax, size_int_type_wide.new_const(%rip)
.LBB10_4:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp96:
	.size	size_int_type_wide, .Ltmp96-size_int_type_wide
	.cfi_endproc

	.align	16, 0x90
	.type	size_htab_hash,@function
size_htab_hash:                         # @size_htab_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	40(%rdi), %rax
	movq	8(%rdi), %rdx
	movq	32(%rdi), %rsi
	xorq	%rax, %rsi
	movl	16(%rdi), %eax
	movq	$3, %rcx
	shrq	%cl, %rdx
	xorq	%rsi, %rdx
	addq	%rax, %rax
	andq	$1048576, %rax          # imm = 0x100000
	xorq	%rdx, %rax
	retq
.Ltmp97:
	.size	size_htab_hash, .Ltmp97-size_htab_hash
	.cfi_endproc

	.align	16, 0x90
	.type	size_htab_eq,@function
size_htab_eq:                           # @size_htab_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movq	40(%rdi), %rcx
	movq	40(%rsi), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB12_6
# BB#1:                                 # %land.lhs.true
	movq	32(%rdi), %rcx
	movq	32(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB12_6
# BB#2:                                 # %land.lhs.true11
	movq	8(%rdi), %rcx
	movq	8(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB12_6
# BB#3:                                 # %land.rhs
	movl	16(%rdi), %eax
	movl	16(%rsi), %ecx
	xorq	%rax, %rcx
	testq	$524288, %rcx           # imm = 0x80000
	jne	.LBB12_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rax
.LBB12_6:                               # %land.end
	retq
.LBB12_4:
	movabsq	$0, %rax
	retq
.Ltmp98:
	.size	size_htab_eq, .Ltmp98-size_htab_eq
	.cfi_endproc

	.globl	size_binop
	.align	16, 0x90
	.type	size_binop,@function
size_binop:                             # @size_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp105:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp107:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp108:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 48
.Ltmp110:
	.cfi_offset %rbx, -48
.Ltmp111:
	.cfi_offset %r12, -40
.Ltmp112:
	.cfi_offset %r13, -32
.Ltmp113:
	.cfi_offset %r14, -24
.Ltmp114:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	8(%rbx), %rsi
	movzbl	16(%rsi), %eax
	movq	%rdx, %r14
	movq	%rdi, %r15
	cmpq	$6, %rax
	jne	.LBB13_18
# BB#1:                                 # %lor.lhs.false
	movzbl	62(%rsi), %eax
	testq	$2, %rax
	je	.LBB13_18
# BB#2:                                 # %lor.lhs.false7
	movq	8(%r14), %rax
	cmpq	%rax, %rsi
	jne	.LBB13_18
# BB#3:                                 # %if.end
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB13_15
# BB#4:                                 # %land.lhs.true
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB13_15
# BB#5:                                 # %if.then21
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r15, %r13
	andq	%r12, %r13
	cmpq	$59, %r13
	jne	.LBB13_8
# BB#6:                                 # %land.lhs.true23
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB13_9
# BB#7:
	movq	%r14, %rax
	jmp	.LBB13_17
.LBB13_15:                              # %if.end43
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB13_17
# BB#16:                                # %if.end43
	cmpq	%r14, %rax
	je	.LBB13_17
# BB#19:                                # %if.end48
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB13_8:                               # %if.else
	leaq	-59(%r15), %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB13_11
.LBB13_9:                               # %land.lhs.true29
	movq	%r14, %rdi
	callq	integer_zerop
	testq	%r12, %rax
	je	.LBB13_11
# BB#10:
	movq	%rbx, %rax
	jmp	.LBB13_17
.LBB13_11:                              # %if.else33
	cmpq	$61, %r13
	jne	.LBB13_14
# BB#12:                                # %land.lhs.true35
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB13_14
# BB#13:
	movq	%r14, %rax
.LBB13_17:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_14:                              # %if.end41
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	int_const_binop  # TAILCALL
.LBB13_18:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.size_binop, %rcx
	movq	$2014, %rsi             # imm = 0x7DE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp115:
	.size	size_binop, .Ltmp115-size_binop
	.cfi_endproc

	.align	16, 0x90
	.type	int_const_binop,@function
int_const_binop:                        # @int_const_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp122:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp123:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp124:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp125:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp127:
	.cfi_def_cfa_offset 160
.Ltmp128:
	.cfi_offset %rbx, -48
.Ltmp129:
	.cfi_offset %r12, -40
.Ltmp130:
	.cfi_offset %r13, -32
.Ltmp131:
	.cfi_offset %r14, -24
.Ltmp132:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	8(%rbx), %r15
	movl	16(%r15), %eax
	movq	$13, %rcx
	movq	%rax, %r13
	shrq	%cl, %r13
	andq	$255, %rax
	xorq	%r12, %r12
	cmpq	$6, %rax
	jne	.LBB14_2
# BB#1:                                 # %land.rhs
	movzbl	62(%r15), %r12d
	andq	$2, %r12
	movq	$1, %rcx
	shrq	%cl, %r12
.LBB14_2:                               # %land.end
	leaq	-59(%rdi), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$30, %rcx
	ja	.LBB14_78
# BB#3:                                 # %land.end
	movq	32(%rbx), %r14
	movq	40(%rbx), %r11
	movq	32(%rdx), %r8
	movq	40(%rdx), %r10
	andq	$1, %r13
	movabsq	$-9223372036854775808, %rax # imm = 0x8000000000000000
	jmpq	*.LJTI14_0(,%rcx,8)
.LBB14_28:                              # %sw.bb54
	testq	%r10, %r10
	jne	.LBB14_39
# BB#29:                                # %sw.bb54
	testq	%r8, %r8
	jle	.LBB14_39
# BB#30:                                # %land.lhs.true59
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	jne	.LBB14_39
# BB#31:                                # %land.lhs.true65
	movl	16(%rdx), %eax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB14_39
# BB#32:                                # %land.lhs.true65
	testq	%r11, %r11
	jne	.LBB14_39
# BB#33:                                # %land.lhs.true65
	testq	%r14, %r14
	js	.LBB14_39
# BB#34:                                # %if.then
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$63, %rdi
	jne	.LBB14_35
# BB#36:                                # %if.then80
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	-1(%r14,%r8), %r14
	jmp	.LBB14_37
.LBB14_39:                              # %sw.bb83
	testq	%r10, %r10
	jne	.LBB14_42
# BB#40:                                # %sw.bb83
	cmpq	$1, %r8
	jne	.LBB14_42
# BB#41:                                # %if.then89
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB14_72
.LBB14_49:                              # %sw.bb105
	testq	%r10, %r10
	jne	.LBB14_59
# BB#50:                                # %sw.bb105
	testq	%r8, %r8
	jle	.LBB14_59
# BB#51:                                # %land.lhs.true111
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	jne	.LBB14_59
# BB#52:                                # %land.lhs.true118
	movl	16(%rdx), %eax
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB14_59
# BB#53:                                # %land.lhs.true118
	testq	%r11, %r11
	jne	.LBB14_59
# BB#54:                                # %land.lhs.true118
	testq	%r14, %r14
	js	.LBB14_59
# BB#55:                                # %if.then131
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$67, %rdi
	jne	.LBB14_56
# BB#57:                                # %if.then134
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	-1(%r14,%r8), %r14
	jmp	.LBB14_58
.LBB14_59:                              # %sw.bb139
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	80(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	88(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	64(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	72(%rsp), %rax
	jmp	.LBB14_47
.LBB14_42:                              # %if.end90
	cmpq	%r10, %r11
	jne	.LBB14_46
# BB#43:                                # %if.end90
	cmpq	%r8, %r14
	jne	.LBB14_46
# BB#44:                                # %if.end90
	movq	%r11, %rax
	orq	%r14, %rax
	je	.LBB14_46
# BB#45:                                # %if.then102
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$1, 88(%rsp)
	movq	$0, 80(%rsp)
	jmp	.LBB14_74
.LBB14_60:                              # %sw.bb141
	testq	%r13, %r13
	je	.LBB14_67
# BB#61:                                # %if.then143
	cmpq	%r10, %r11
	jb	.LBB14_75
# BB#62:                                # %lor.rhs
	je	.LBB14_64
# BB#63:
	movabsq	$0, %rcx
	jmp	.LBB14_65
.LBB14_78:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.int_const_binop, %rdx
	movq	$1671, %rsi             # imm = 0x687
	callq	fancy_abort
.LBB14_46:                              # %if.end103
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	64(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	72(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	80(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	88(%rsp), %rax
.LBB14_47:                              # %sw.epilog
	movq	%rax, (%rsp)
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r11, %rcx
	movq	%r10, %r9
	callq	div_and_round_double
	jmp	.LBB14_48
.LBB14_79:                              # %sw.bb43
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	addq	%r8, %r14
	sbbq	%rcx, %rcx
	xorq	%r11, %rax
	xorq	%r10, %rax
	movq	%r14, %rdx
	leaq	(%r10,%r11), %r14
	subq	%rcx, %r14
	movq	%rdx, 88(%rsp)
	movq	%r14, 80(%rsp)
	xorq	%r11, %r14
	andq	%rax, %r14
	movq	$63, %rcx
	shrq	%cl, %r14
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%r14, %r10
	jmp	.LBB14_80
.LBB14_23:                              # %sw.bb44
	testq	%r8, %r8
	je	.LBB14_24
# BB#25:                                # %if.else.i
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	negq	%r8
	movq	%r8, 88(%rsp)
	movq	%r10, %rcx
	notq	%rcx
	jmp	.LBB14_26
.LBB14_27:                              # %sw.bb52
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	leaq	88(%rsp), %rax
	leaq	80(%rsp), %r9
	movq	%r14, %rdi
	movq	%r11, %rsi
	movq	%r8, %rdx
	movq	%r10, %rcx
	movq	%rax, %r8
	callq	mul_double
.LBB14_48:                              # %sw.epilog
	movq	%rax, %r10
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_80
.LBB14_8:                               # %sw.bb30
	negq	%r8
.LBB14_9:                               # %sw.bb31
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movl	60(%r15), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	movq	%r13, %rdx
	xorq	$1, %rdx
	leaq	88(%rsp), %rax
	leaq	80(%rsp), %r9
	movq	%rdx, (%rsp)
	movq	%r14, %rdi
	movq	%r11, %rsi
	movq	%r8, %rdx
	movq	%rax, %r8
	callq	lshift_double
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	xorq	%r10, %r10
	jmp	.LBB14_80
.LBB14_10:                              # %sw.bb36
	negq	%r8
.LBB14_11:                              # %sw.bb38
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%r15, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movl	60(%rax), %ebx
	andq	$511, %rbx              # imm = 0x1FF
	movq	%r8, %rax
	cqto
	idivq	%rbx
	movq	$63, %rcx
	movq	%r14, %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	sarq	%cl, %r14
	andq	%rbx, %r14
	addq	%rdx, %r14
	leaq	104(%rsp), %r8
	leaq	96(%rsp), %r9
	movq	$0, (%rsp)
	movq	%r11, %rsi
	movq	%r11, %r15
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	lshift_double
	subq	%r14, %rbx
	xorq	%rax, %rax
	cmpq	$127, %rbx
	movabsq	$0, %rdx
	movabsq	$0, %rsi
	jg	.LBB14_15
# BB#12:                                # %if.else.i.i
	cmpq	$64, %rbx
	jl	.LBB14_14
# BB#13:                                # %if.then2.i.i
	addq	$-64, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %r15
	movq	%r15, %rdx
	xorq	%rsi, %rsi
	jmp	.LBB14_15
.LBB14_4:                               # %sw.bb
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	orq	%r14, %r8
	movq	%r8, 88(%rsp)
	orq	%r11, %r10
	movq	%r10, 80(%rsp)
	jmp	.LBB14_74
.LBB14_5:                               # %sw.bb22
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	xorq	%r14, %r8
	movq	%r8, 88(%rsp)
	xorq	%r11, %r10
	movq	%r10, 80(%rsp)
	jmp	.LBB14_74
.LBB14_6:                               # %sw.bb24
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	andq	%r14, %r8
	movq	%r8, 88(%rsp)
	andq	%r11, %r10
	movq	%r10, 80(%rsp)
	jmp	.LBB14_74
.LBB14_7:                               # %sw.bb26
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	notq	%r8
	andq	%r8, %r14
	movq	%r14, 88(%rsp)
	notq	%r10
	andq	%r10, %r11
	jmp	.LBB14_73
.LBB14_67:                              # %if.else
	cmpq	%r10, %r11
	jge	.LBB14_68
.LBB14_75:                              # %if.end167.thr_comm
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$1, 88(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$78, %rdi
	je	.LBB14_72
	jmp	.LBB14_76
.LBB14_14:                              # %if.else5.i.i
	movq	%r15, %rsi
	movq	%rbx, %rcx
	shrq	%cl, %rsi
	movq	%rbx, %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	shrq	%cl, %rdx
	movq	$63, %rcx
	subq	%rbx, %rcx
	shlq	%cl, %r15
	addq	%r15, %r15
	orq	%rdx, %r15
	movq	%r15, %rdx
.LBB14_15:                              # %if.end11.i.i
	testq	%r14, %r14
	movq	56(%rsp), %rbx          # 8-byte Reload
	jle	.LBB14_16
# BB#17:                                # %if.else15.i.i
	cmpq	$127, %r14
	movq	32(%rsp), %r15          # 8-byte Reload
	jle	.LBB14_19
# BB#18:
	movq	%rsi, %rax
	jmp	.LBB14_22
.LBB14_16:
	xorq	%rdx, %rdx
	movq	32(%rsp), %r15          # 8-byte Reload
	jmp	.LBB14_22
.LBB14_19:                              # %if.else21.i.i
	cmpq	$64, %r14
	jl	.LBB14_21
# BB#20:                                # %if.then26.i.i
	addq	$-64, %r14
	movq	$-1, %rax
	movq	%r14, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	%rax, %rsi
	movq	%rsi, %rax
	jmp	.LBB14_22
.LBB14_24:                              # %if.then.i
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$0, 88(%rsp)
	movq	%r10, %rcx
	negq	%rcx
	xorq	%r8, %r8
.LBB14_26:                              # %neg_double.exit
	addq	%r8, %r14
	sbbq	%rdx, %rdx
	addq	%r11, %rcx
	subq	%rdx, %rcx
	movq	%r14, 88(%rsp)
	movq	%rcx, 80(%rsp)
	xorq	%rcx, %r11
	xorq	%rax, %r10
	xorq	%rcx, %r10
	andq	%r11, %r10
	movq	$63, %rcx
	shrq	%cl, %r10
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_80
.LBB14_68:                              # %lor.rhs156
	jne	.LBB14_69
.LBB14_64:                              # %lor.rhs
	movabsq	$1, %rcx
	jmp	.LBB14_65
.LBB14_21:                              # %if.else36.i.i
	movq	$-1, %rsi
	movq	%r14, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rsi, %rdx
.LBB14_22:                              # %lrotate_double.exit
	movq	104(%rsp), %rcx
	orq	%rdx, %rcx
	movq	96(%rsp), %rdx
	movq	%rcx, 88(%rsp)
	orq	%rax, %rdx
	movq	%rdx, 80(%rsp)
	jmp	.LBB14_74
.LBB14_35:
	movq	%rdx, 48(%rsp)          # 8-byte Spill
.LBB14_37:                              # %if.end
	xorq	%r10, %r10
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%r8
	movq	%rax, 88(%rsp)
	jmp	.LBB14_38
.LBB14_69:
	movabsq	$0, %rcx
.LBB14_65:                              # %lor.rhs
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	cmpq	%r8, %r14
	sbbq	%rax, %rax
	andq	%rcx, %rax
	movq	%rax, 88(%rsp)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	$78, %rdi
	jne	.LBB14_66
# BB#70:                                # %if.end167
	movabsq	$1, %rcx
	jmp	.LBB14_71
.LBB14_66:
	movabsq	$0, %rcx
.LBB14_71:                              # %if.end167
	xorq	%rcx, %rax
	je	.LBB14_72
.LBB14_76:                              # %if.else174
	movq	%r8, 88(%rsp)
	movq	%r10, 80(%rsp)
	jmp	.LBB14_74
.LBB14_72:                              # %if.then173
	movq	%r14, 88(%rsp)
.LBB14_73:                              # %sw.epilog
	movq	%r11, 80(%rsp)
.LBB14_74:                              # %sw.epilog
	xorq	%r10, %r10
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
.LBB14_80:                              # %sw.epilog
	testq	%r12, %r12
	je	.LBB14_81
# BB#97:                                # %land.lhs.true177
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rsi
	movq	88(%rsp), %rdi
	testq	%rsi, %rsi
	jne	.LBB14_99
# BB#98:                                # %land.lhs.true177
	testq	%rdi, %rdi
	js	.LBB14_99
# BB#105:                               # %land.lhs.true188
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r10
	je	.LBB14_107
# BB#106:
	movq	%r10, %r14
	xorq	%rsi, %rsi
	jmp	.LBB14_82
.LBB14_81:                              # %sw.epilog.if.else206_crit_edge
	movq	%r10, %r14
	movq	%r12, 56(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi
	movq	80(%rsp), %rsi
	jmp	.LBB14_82
.LBB14_99:                              # %lor.lhs.false
	cmpq	$-1, %rsi
	jne	.LBB14_100
# BB#101:                               # %lor.lhs.false
	testq	%rdi, %rdi
	jns	.LBB14_102
# BB#103:                               # %lor.lhs.false
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r10
	jne	.LBB14_104
.LBB14_107:                             # %land.lhs.true191
	movzbl	18(%rbx), %eax
	movq	%r10, %r14
	testq	$8, %rax
	jne	.LBB14_82
# BB#108:                               # %land.lhs.true197
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	18(%rax), %eax
	testq	$8, %rax
	jne	.LBB14_82
# BB#109:                               # %if.then204
	movq	%r15, %rsi
	callq	size_int_type_wide
	movq	%rax, %r15
	jmp	.LBB14_110
.LBB14_104:
	movq	%r10, %r14
	jmp	.LBB14_82
.LBB14_100:
	movq	%r10, %r14
	jmp	.LBB14_82
.LBB14_102:
	movq	%r10, %r14
.LBB14_82:                              # %if.else206
	callq	build_int_2_wide
	movq	%rax, %r15
	movq	8(%rbx), %rax
	movq	%rbx, %r12
	movq	%rax, 8(%r15)
	testq	%r13, %r13
	movabsq	$0, %rbx
	movabsq	$1, %r13
	movq	%rbx, %rsi
	jne	.LBB14_84
# BB#83:                                # %if.else206
	movq	%r13, %rsi
.LBB14_84:                              # %if.else206
	movq	56(%rsp), %rax          # 8-byte Reload
	orq	%rax, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movq	%rbx, %rax
	je	.LBB14_86
# BB#85:                                # %if.else206
	movq	%r13, %rax
.LBB14_86:                              # %if.else206
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	force_fit_type
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	movq	%rbx, %rsi
	jne	.LBB14_88
# BB#87:                                # %if.else206
	movq	%r13, %rsi
.LBB14_88:                              # %if.else206
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rax
	movq	48(%rsp), %rdi          # 8-byte Reload
	je	.LBB14_90
# BB#89:                                # %if.else206
	movq	%r13, %rax
.LBB14_90:                              # %if.else206
	andq	%rax, %rsi
	movl	16(%r12), %r8d
	movl	16(%rdi), %eax
	movl	16(%r15), %edx
	movq	$19, %rcx
	shlq	%cl, %rsi
	orq	%r8, %rax
	andq	$524288, %rax           # imm = 0x80000
	movq	%rdx, %rcx
	andq	$-524289, %rcx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rsi, %rax
	jne	.LBB14_92
# BB#91:                                # %if.else206
	movq	%r13, %rbx
.LBB14_92:                              # %if.else206
	orq	%rcx, %rax
	movl	%eax, 16(%r15)
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%rbx, %rcx
	je	.LBB14_96
# BB#93:                                # %land.lhs.true260
	movq	40(%r15), %rcx
	movq	80(%rsp), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB14_95
# BB#94:                                # %lor.lhs.false266
	movq	32(%r15), %rcx
	movq	88(%rsp), %rsi
	cmpq	%rsi, %rcx
	je	.LBB14_96
.LBB14_95:                              # %if.then272
	orq	$524288, %rdx           # imm = 0x80000
	movl	%edx, 16(%r15)
	movq	%rdx, %rax
.LBB14_96:                              # %if.end278
	movl	16(%r12), %edx
	movl	16(%rdi), %esi
	movq	$1, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	orq	%rdx, %rdi
	orq	%rsi, %rdi
	andq	$262144, %rdi           # imm = 0x40000
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rax, %rcx
	orq	%rdi, %rcx
	movl	%ecx, 16(%r15)
.LBB14_110:                             # %return
	movq	%r15, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB14_56:
	movq	%rdx, 48(%rsp)          # 8-byte Spill
.LBB14_58:                              # %if.end137
	xorq	%r10, %r10
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%r8
	movq	%rdx, 88(%rsp)
.LBB14_38:                              # %sw.epilog
	movq	$0, 80(%rsp)
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB14_80
.Ltmp133:
	.size	int_const_binop, .Ltmp133-int_const_binop
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_79
	.quad	.LBB14_23
	.quad	.LBB14_27
	.quad	.LBB14_28
	.quad	.LBB14_28
	.quad	.LBB14_28
	.quad	.LBB14_39
	.quad	.LBB14_49
	.quad	.LBB14_49
	.quad	.LBB14_49
	.quad	.LBB14_59
	.quad	.LBB14_78
	.quad	.LBB14_28
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_60
	.quad	.LBB14_60
	.quad	.LBB14_78
	.quad	.LBB14_78
	.quad	.LBB14_9
	.quad	.LBB14_8
	.quad	.LBB14_11
	.quad	.LBB14_10
	.quad	.LBB14_4
	.quad	.LBB14_5
	.quad	.LBB14_6
	.quad	.LBB14_7

	.text
	.globl	fold
	.align	16, 0x90
	.type	fold,@function
fold:                                   # @fold
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp140:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp141:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp142:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp143:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 48
	subq	$576, %rsp              # imm = 0x240
.Ltmp145:
	.cfi_def_cfa_offset 624
.Ltmp146:
	.cfi_offset %rbx, -48
.Ltmp147:
	.cfi_offset %r12, -40
.Ltmp148:
	.cfi_offset %r13, -32
.Ltmp149:
	.cfi_offset %r14, -24
.Ltmp150:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	16(%r14), %eax
	movq	%rax, %rdi
	andq	$255, %rdi
	cmpq	$120, %rdi
	jne	.LBB15_2
# BB#1:
	movq	%r14, %r9
	jmp	.LBB15_1954
.LBB15_2:                               # %entry
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	8(%r14), %rcx
	movzbl	tree_code_type(%rdi), %ebx
	cmpq	$118, %rdi
	jne	.LBB15_7
# BB#3:                                 # %land.lhs.true
	cmpq	$0, 48(%r14)
	jne	.LBB15_4
# BB#5:                                 # %land.lhs.true
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	$99, %rax
	jne	.LBB15_24
# BB#6:
	movq	%r14, %r9
	jmp	.LBB15_1954
.LBB15_7:                               # %if.end
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	$99, %rax
	jne	.LBB15_9
# BB#8:
	movq	%r14, %r9
	jmp	.LBB15_1954
.LBB15_9:                               # %if.end13
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	leaq	-76(%rdi), %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$39, %rcx
	ja	.LBB15_24
# BB#10:                                # %if.end13
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$824633720833, %rcx     # imm = 0xC000000001
	testq	%rcx, %rax
	je	.LBB15_24
# BB#11:                                # %if.then22
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	movq	32(%r14), %rdi
	movq	$0, 168(%rsp)           # 8-byte Folded Spill
	testq	%rdi, %rdi
	je	.LBB15_12
# BB#13:                                # %while.cond.preheader
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	movq	global_trees(%rip), %r10
	.align	16, 0x90
.LBB15_14:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r9
	movl	16(%r9), %ecx
	movq	%rcx, %rsi
	andq	$255, %rsi
	leaq	-114(%rsi), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	ja	.LBB15_18
# BB#15:                                # %land.lhs.true49
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	32(%r9), %rdi
	cmpq	%r10, %rdi
	je	.LBB15_20
# BB#16:                                # %land.lhs.true55
                                        #   in Loop: Header=BB15_14 Depth=1
	movq	8(%r9), %rbx
	movq	8(%rdi), %rax
	movl	60(%rbx), %r11d
	movl	60(%rax), %edx
	xorq	%r11, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	jne	.LBB15_20
# BB#17:                                # %land.rhs
                                        #   in Loop: Header=BB15_14 Depth=1
	movl	16(%rbx), %edx
	movl	16(%rax), %eax
	xorq	%rdx, %rax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB15_14
.LBB15_18:                              # %if.end94
	testq	%r9, %r9
	je	.LBB15_19
.LBB15_20:                              # %land.lhs.true97
	cmpq	$27, %rsi
	jne	.LBB15_23
# BB#21:                                # %if.end105
	movq	32(%r9), %rax
	testq	%rax, %rax
	je	.LBB15_55
# BB#22:                                # %if.end105.land.lhs.true108_crit_edge
	movl	16(%rax), %ecx
.LBB15_23:                              # %land.lhs.true108
	andq	$255, %rcx
	addq	$-25, %rcx
	andq	%r8, %rcx
	cmpq	$2, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	jmp	.LBB15_55
.LBB15_24:                              # %if.else124
	movq	$0, 168(%rsp)           # 8-byte Folded Spill
	movq	$56, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$113, %rax
	jg	.LBB15_29
# BB#25:                                # %if.else124
	leaq	-49(%rbx), %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	jbe	.LBB15_27
# BB#26:
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	jmp	.LBB15_28
.LBB15_29:                              # %if.else124
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	$114, %rax
	jne	.LBB15_28
	jmp	.LBB15_30
.LBB15_4:
	movq	%r14, %r9
	jmp	.LBB15_1954
.LBB15_27:                              # %if.else124
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rax
	je	.LBB15_28
.LBB15_30:                              # %if.then139
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	callq	first_rtl_op
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %r9
	jle	.LBB15_55
# BB#31:                                # %for.body.lr.ph
	andq	$255, %rbx
	cmpq	$60, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	jne	.LBB15_33
# BB#32:                                # %for.body.lr.ph
	movq	%rdx, 160(%rsp)         # 8-byte Spill
.LBB15_33:                              # %for.body.lr.ph
	cmpq	$83, 144(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_35
# BB#34:                                # %for.body.lr.ph
	movq	%rdx, %rcx
.LBB15_35:                              # %for.body.lr.ph
	orq	%rcx, 160(%rsp)         # 8-byte Folded Spill
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	xorq	%r12, %r12
	movq	global_trees(%rip), %r11
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rax
	xorq	%r9, %r9
	movq	$0, 168(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB15_36:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_42 Depth 2
                                        #     Child Loop BB15_38 Depth 2
	movq	32(%r14,%r12,8), %rcx
	testq	%rcx, %rcx
	je	.LBB15_54
# BB#37:                                # %if.end150
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	%r14, %r8
	cmpq	$0, 160(%rsp)           # 8-byte Folded Reload
	je	.LBB15_42
	.align	16, 0x90
.LBB15_38:                              # %while.cond157
                                        #   Parent Loop BB15_36 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rbx
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	addq	$-114, %rcx
	andq	%r10, %rcx
	cmpq	$2, %rcx
	ja	.LBB15_45
# BB#39:                                # %land.lhs.true178
                                        #   in Loop: Header=BB15_38 Depth=2
	movq	32(%rbx), %rcx
	cmpq	%r11, %rcx
	je	.LBB15_45
# BB#40:                                # %land.lhs.true184
                                        #   in Loop: Header=BB15_38 Depth=2
	movq	8(%rbx), %rdi
	movq	8(%rcx), %r15
	movl	60(%rdi), %r14d
	movl	60(%r15), %r13d
	xorq	%r14, %r13
	testq	$65024, %r13            # imm = 0xFE00
	jne	.LBB15_45
# BB#41:                                # %land.rhs204
                                        #   in Loop: Header=BB15_38 Depth=2
	movl	16(%rdi), %edi
	movl	16(%r15), %esi
	xorq	%rdi, %rsi
	testq	$8192, %rsi             # imm = 0x2000
	je	.LBB15_38
	jmp	.LBB15_45
	.align	16, 0x90
.LBB15_42:                              # %while.cond231
                                        #   Parent Loop BB15_36 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rbx
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	addq	$-114, %rcx
	andq	%r10, %rcx
	cmpq	$2, %rcx
	ja	.LBB15_45
# BB#43:                                # %land.lhs.true252
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	32(%rbx), %rcx
	cmpq	%r11, %rcx
	je	.LBB15_45
# BB#44:                                # %land.rhs258
                                        #   in Loop: Header=BB15_42 Depth=2
	movq	8(%rbx), %rsi
	movq	8(%rcx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB15_42
	.align	16, 0x90
.LBB15_45:                              # %if.end284
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$27, %rcx
	jne	.LBB15_47
# BB#46:                                # %if.then291
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	32(%rbx), %rcx
	movl	16(%rcx), %edx
.LBB15_47:                              # %if.end295
                                        #   in Loop: Header=BB15_36 Depth=1
	andq	$255, %rdx
	addq	$-25, %rdx
	andq	%r10, %rdx
	cmpq	$2, %rdx
	jb	.LBB15_49
# BB#48:                                # %if.end295
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	$0, 152(%rsp)           # 8-byte Folded Spill
.LBB15_49:                              # %if.end295
                                        #   in Loop: Header=BB15_36 Depth=1
	testq	%r10, %r12
	movq	%r8, %r14
	jne	.LBB15_51
# BB#50:                                #   in Loop: Header=BB15_36 Depth=1
	movq	%rbx, %r9
	jmp	.LBB15_54
	.align	16, 0x90
.LBB15_51:                              # %if.else314
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	%r12, %rcx
	andq	%r10, %rcx
	cmpq	$1, %rcx
	je	.LBB15_53
# BB#52:                                # %if.else314
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	168(%rsp), %rbx         # 8-byte Reload
.LBB15_53:                              # %if.else314
                                        #   in Loop: Header=BB15_36 Depth=1
	movq	%rbx, 168(%rsp)         # 8-byte Spill
.LBB15_54:                              # %for.inc
                                        #   in Loop: Header=BB15_36 Depth=1
	incq	%r12
	movq	%r12, %rcx
	andq	%r10, %rcx
	cmpq	%rax, %rcx
	jne	.LBB15_36
	jmp	.LBB15_55
.LBB15_28:
	movq	%rdi, 144(%rsp)         # 8-byte Spill
	xorq	%r9, %r9
.LBB15_55:                              # %if.end321
	movq	144(%rsp), %rbx         # 8-byte Reload
	leaq	-59(%rbx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$29, %rcx
	jbe	.LBB15_57
# BB#56:
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	%r9, %r13
	jmp	.LBB15_61
.LBB15_57:                              # %if.end321
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$941096965, %rax        # imm = 0x38180005
	movq	168(%rsp), %r13         # 8-byte Reload
	je	.LBB15_58
# BB#59:                                # %land.lhs.true342
	movzbl	16(%r9), %eax
	addq	$-25, %rax
	andq	%rdx, %rax
	cmpq	$1, %rax
	ja	.LBB15_58
# BB#60:                                # %if.then356
	movq	32(%r14), %rax
	movq	40(%r14), %rcx
	movq	%rcx, 32(%r14)
	movq	%rax, 40(%r14)
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	%r9, %r15
	jmp	.LBB15_61
.LBB15_58:
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	%r13, %r15
	movq	%r9, %r13
.LBB15_61:                              # %if.end369
	leaq	-86(%rbx), %rcx
	andq	%rdx, %rcx
	cmpq	$16, %rcx
	ja	.LBB15_102
# BB#62:                                # %if.end369
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$98309, %rax            # imm = 0x18005
	je	.LBB15_102
# BB#63:                                # %land.lhs.true381
	movzbl	16(%r13), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$60, %rcx
	je	.LBB15_70
# BB#64:                                # %land.lhs.true381
	cmpq	$93, %rax
	je	.LBB15_70
# BB#65:                                # %land.lhs.true381
	cmpq	$91, %rax
	je	.LBB15_70
# BB#66:                                # %land.lhs.true381
	cmpq	$94, %rax
	je	.LBB15_70
# BB#67:                                # %land.lhs.true381
	cmpq	$92, %rax
	je	.LBB15_70
# BB#68:                                # %land.lhs.true381
	cmpq	$95, %rax
	je	.LBB15_70
# BB#69:                                # %land.lhs.true381
	cmpq	$96, %rax
	jne	.LBB15_79
.LBB15_70:                              # %land.lhs.true387
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$60, %rcx
	je	.LBB15_95
# BB#71:                                # %land.lhs.true387
	cmpq	$93, %rax
	je	.LBB15_95
# BB#72:                                # %land.lhs.true387
	cmpq	$91, %rax
	je	.LBB15_95
# BB#73:                                # %land.lhs.true387
	cmpq	$94, %rax
	je	.LBB15_95
# BB#74:                                # %land.lhs.true387
	cmpq	$92, %rax
	je	.LBB15_95
# BB#75:                                # %land.lhs.true387
	cmpq	$95, %rax
	je	.LBB15_95
# BB#76:                                # %land.lhs.true387
	cmpq	$96, %rax
	je	.LBB15_95
# BB#77:                                # %lor.lhs.false394
	cmpq	$88, %rax
	jne	.LBB15_79
# BB#78:                                # %land.lhs.true401
	movq	40(%r15), %rdi
	movq	%rdx, %r14
	callq	integer_onep
	movq	%r14, %rdx
	testq	%rdx, %rax
	jne	.LBB15_95
.LBB15_79:                              # %lor.lhs.false407
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$60, %rcx
	je	.LBB15_86
# BB#80:                                # %lor.lhs.false407
	cmpq	$93, %rax
	je	.LBB15_86
# BB#81:                                # %lor.lhs.false407
	cmpq	$91, %rax
	je	.LBB15_86
# BB#82:                                # %lor.lhs.false407
	cmpq	$94, %rax
	je	.LBB15_86
# BB#83:                                # %lor.lhs.false407
	cmpq	$92, %rax
	je	.LBB15_86
# BB#84:                                # %lor.lhs.false407
	cmpq	$95, %rax
	je	.LBB15_86
# BB#85:                                # %lor.lhs.false407
	cmpq	$96, %rax
	jne	.LBB15_102
.LBB15_86:                              # %land.lhs.true414
	movzbl	16(%r13), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$60, %rcx
	je	.LBB15_95
# BB#87:                                # %land.lhs.true414
	cmpq	$93, %rax
	je	.LBB15_95
# BB#88:                                # %land.lhs.true414
	cmpq	$91, %rax
	je	.LBB15_95
# BB#89:                                # %land.lhs.true414
	cmpq	$94, %rax
	je	.LBB15_95
# BB#90:                                # %land.lhs.true414
	cmpq	$92, %rax
	je	.LBB15_95
# BB#91:                                # %land.lhs.true414
	cmpq	$95, %rax
	je	.LBB15_95
# BB#92:                                # %land.lhs.true414
	cmpq	$96, %rax
	je	.LBB15_95
# BB#93:                                # %lor.lhs.false421
	cmpq	$88, %rax
	jne	.LBB15_102
# BB#94:                                # %land.lhs.true428
	movq	40(%r13), %rdi
	movq	%rdx, %r14
	callq	integer_onep
	movq	%r14, %rdx
	testq	%rdx, %rax
	je	.LBB15_102
.LBB15_95:                              # %if.then434
	movq	$93, %rdi
	cmpq	$88, %rbx
	movq	128(%rsp), %rsi         # 8-byte Reload
	je	.LBB15_100
# BB#96:                                # %cond.false
	cmpq	$86, %rbx
	je	.LBB15_97
# BB#98:                                # %cond.false
	movabsq	$1, %rdi
	jmp	.LBB15_99
.LBB15_102:                             # %if.end447
	movzbl	tree_code_type(%rbx), %eax
	cmpq	$60, %rax
	je	.LBB15_124
# BB#103:                               # %if.end447
	cmpq	$50, %rax
	jne	.LBB15_104
.LBB15_124:                             # %if.then712
	movzbl	16(%r15), %ecx
	cmpq	$51, %rcx
	je	.LBB15_130
# BB#125:                               # %if.then712
	movq	%rdx, %rsi
	movq	%r13, %rdx
	cmpq	$47, %rcx
	jne	.LBB15_128
# BB#126:                               # %if.then719
	movq	32(%r15), %r12
	movq	40(%r15), %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB15_127
.LBB15_104:                             # %if.end447
	cmpq	$49, %rax
	jne	.LBB15_148
# BB#105:                               # %if.then453
	movzbl	16(%r13), %eax
	cmpq	$51, %rax
	jne	.LBB15_106
# BB#108:                               # %if.then477
	movq	32(%r13), %r12
	movq	40(%r13), %rdx
	movq	144(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%r13, %rbx
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	48(%rbx), %rdx
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r8
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r9
	movq	136(%rsp), %rax         # 8-byte Reload
	andq	$254, %rax
	cmpq	$114, %rax
	je	.LBB15_110
# BB#109:                               # %if.then477
	cmpq	$116, %r14
	jne	.LBB15_1954
.LBB15_110:                             # %land.lhs.true501
	movzbl	16(%r9), %eax
	cmpq	$51, %rax
	jne	.LBB15_1954
# BB#111:                               # %land.lhs.true508
	movq	40(%r9), %rax
	movzbl	16(%rax), %ecx
	cmpq	%r14, %rcx
	jne	.LBB15_1954
# BB#112:                               # %land.lhs.true518
	movq	48(%r9), %rcx
	movzbl	16(%rcx), %edx
	cmpq	%r14, %rdx
	movq	%r14, %rbx
	jne	.LBB15_1954
# BB#113:                               # %land.lhs.true528
	movq	%r9, %rdi
	movq	32(%rax), %r9
	movq	32(%rcx), %r8
	movq	8(%r9), %rsi
	movq	8(%r8), %rax
	cmpq	%rax, %rsi
	je	.LBB15_115
# BB#114:
	movq	%rdi, %r9
	jmp	.LBB15_1954
.LBB15_128:                             # %lor.lhs.false736
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$60, %rcx
	movq	%rdx, %r13
	movq	%rsi, %rdx
	jne	.LBB15_137
# BB#129:                               # %lor.lhs.false736
	cmpq	$60, %rax
	je	.LBB15_137
.LBB15_130:                             # %land.lhs.true752
	movl	16(%r13), %r12d
	movq	%r12, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	jne	.LBB15_132
# BB#131:                               # %lor.lhs.false759
	movq	%rdx, %r14
	movq	$25, %rsi
	movq	%r13, %rdi
	callq	count_cond
	movq	%rax, %rbx
	movq	$25, %rsi
	movq	%r15, %rdi
	callq	count_cond
	movq	%r14, %rdx
	addq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$25, %rax
	jg	.LBB15_137
.LBB15_132:                             # %land.lhs.true764
	testq	$256, %r12              # imm = 0x100
	je	.LBB15_135
# BB#133:                               # %lor.lhs.false770
	movq	%rdx, %rbx
	callq	global_bindings_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	jne	.LBB15_137
# BB#134:                               # %land.lhs.true774
	movq	%r13, %rdi
	movq	%rdx, %rbx
	callq	contains_placeholder_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	je	.LBB15_135
.LBB15_137:                             # %if.else779
	movzbl	16(%r13), %eax
	cmpq	$51, %rax
	je	.LBB15_142
# BB#138:                               # %if.else779
	cmpq	$47, %rax
	jne	.LBB15_140
# BB#139:                               # %if.then786
	movq	32(%r13), %r12
	movq	40(%r13), %rdx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r15, %rcx
.LBB15_127:                             # %return
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r12, %rdx
.LBB15_520:                             # %return
	callq	build
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_135:                             # %if.then777
	xorq	%r8, %r8
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r13, %rcx
	jmp	.LBB15_136
.LBB15_12:
	movq	$1, 152(%rsp)           # 8-byte Folded Spill
	xorq	%r9, %r9
	jmp	.LBB15_55
.LBB15_140:                             # %lor.lhs.false803
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB15_148
# BB#141:                               # %land.lhs.true813
	movq	144(%rsp), %rax         # 8-byte Reload
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	je	.LBB15_148
.LBB15_142:                             # %land.lhs.true819
	movl	16(%r15), %r12d
	movq	%r12, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	jne	.LBB15_144
# BB#143:                               # %lor.lhs.false826
	movq	%rdx, %r14
	movq	$25, %rsi
	movq	%r13, %rdi
	callq	count_cond
	movq	%rax, %rbx
	movq	$25, %rsi
	movq	%r15, %rdi
	callq	count_cond
	movq	%r14, %rdx
	addq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$25, %rax
	jg	.LBB15_148
.LBB15_144:                             # %land.lhs.true832
	testq	$256, %r12              # imm = 0x100
	je	.LBB15_147
# BB#145:                               # %lor.lhs.false839
	movq	%rdx, %rbx
	callq	global_bindings_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	jne	.LBB15_148
# BB#146:                               # %land.lhs.true843
	movq	%r15, %rdi
	movq	%rdx, %rbx
	callq	contains_placeholder_p
	movq	%rbx, %rdx
	testq	%rdx, %rax
	jne	.LBB15_148
.LBB15_147:                             # %if.then846
	movq	$1, %r8
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r13, %rdx
	movq	%r15, %rcx
.LBB15_136:                             # %return
	callq	fold_binary_op_with_conditional_arg
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_106:                             # %if.then453
	cmpq	$47, %rax
	jne	.LBB15_122
# BB#107:                               # %if.then460
	movq	32(%r13), %r15
	movq	40(%r13), %rdx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	jmp	.LBB15_520
.LBB15_97:
	movabsq	$0, %rdi
.LBB15_99:                              # %cond.false
	orq	$94, %rdi
.LBB15_100:                             # %cond.end
	xorq	%rax, %rax
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r9
	cmpq	$101, %rbx
	jne	.LBB15_1954
# BB#101:                               # %if.then444
	movq	%r9, %rdi
	callq	invert_truthvalue
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_122:                             # %if.else680
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB15_148
# BB#123:                               # %if.then690
	movq	global_trees+96(%rip), %rdx
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%r13, %r15
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	global_trees+88(%rip), %rdx
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r8
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	build
.LBB15_715:                             # %if.else6204
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_148:                             # %if.end901
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	-32(%rax), %rax
	andq	%rdx, %rax
	cmpq	$96, %rax
	ja	.LBB15_1953
# BB#149:                               # %if.end901
	movq	%r13, 168(%rsp)         # 8-byte Spill
	movq	%r15, %r12
	movq	%rdx, %r14
	movq	120(%rsp), %r9          # 8-byte Reload
	jmpq	*.LJTI15_0(,%rax,8)
.LBB15_1086:                            # %sw.bb4366
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$7, %rcx
	movq	%r12, %r13
	je	.LBB15_1089
# BB#1087:                              # %sw.bb4366
	cmpq	$8, %rcx
	jne	.LBB15_1106
# BB#1088:                              # %land.lhs.true4384
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_1106
.LBB15_1089:                            # %if.then4395
	movzbl	16(%rbx), %ecx
	movl	16(%r13), %eax
	cmpq	$77, %rcx
	jne	.LBB15_1102
# BB#1090:                              # %land.lhs.true4402
	andq	$255, %rax
	cmpq	$77, %rax
	jne	.LBB15_1092
# BB#1091:                              # %if.then4409
	movq	32(%r13), %rdx
	movq	32(%rbx), %rcx
.LBB15_1323:                            # %return
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_1324
.LBB15_19:
	xorq	%r9, %r9
	jmp	.LBB15_55
.LBB15_793:                             # %sw.bb3676
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r14, %rax
	jne	.LBB15_704
# BB#794:                               # %if.end3682
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_796
# BB#795:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_804:                             # %sw.bb3715
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_807
# BB#805:                               # %if.then3718
	movq	global_trees+88(%rip), %rsi
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	convert
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	17(%rdx), %ecx
	movq	%rax, %r9
	testq	$1, %rcx
	je	.LBB15_706
# BB#806:                               # %if.then.i10717
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r9, %rcx
	jmp	.LBB15_520
.LBB15_152:                             # %sw.bb904
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r9
	movq	8(%rax), %r12
	movq	8(%r9), %rsi
	cmpq	%r12, %rsi
	je	.LBB15_1954
# BB#153:                               # %if.end918
	movl	16(%r9), %r13d
	movq	%r13, %rax
	andq	$254, %rax
	cmpq	$114, %rax
	jne	.LBB15_244
# BB#154:                               # %if.then938
	movq	32(%r9), %rbx
	movq	8(%rbx), %r15
	movl	16(%r15), %r8d
	movq	%r8, %rax
	andq	$255, %rax
	leaq	-6(%rax), %rcx
	movq	%rcx, %rdx
	andq	%r14, %rdx
	cmpq	$7, %rdx
	jae	.LBB15_155
# BB#159:                               # %lor.end
	andq	$127, %rcx
	movq	$113, %r11
	shrq	%cl, %r11
	andq	$1, %r11
	movq	%r8, %rcx
	andq	$253, %rcx
	cmpq	$13, %rcx
	jne	.LBB15_160
# BB#161:                               # %lor.end
	movabsq	$1, %rdi
	jmp	.LBB15_162
.LBB15_813:                             # %sw.bb3741
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_704
# BB#814:                               # %if.end3747
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_817
# BB#815:                               # %land.lhs.true3754
	movq	%rbx, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_816
.LBB15_817:                             # %if.end3759
	movq	144(%rsp), %r12         # 8-byte Reload
	cmpq	$85, %r12
	movq	120(%rsp), %r15         # 8-byte Reload
	je	.LBB15_822
# BB#818:                               # %if.end3759
	cmpq	$84, %r12
	jne	.LBB15_485
# BB#819:                               # %land.lhs.true3762
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	movq	$84, %r12
	jne	.LBB15_485
# BB#820:                               # %if.then3769
	movq	$85, %rax
	movb	%al, 16(%r15)
	movq	128(%rsp), %rax         # 8-byte Reload
	movl	60(%rax), %eax
	movq	%r15, %r12
	movq	8(%rbx), %r15
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$254, %rax
	movzbl	mode_bitsize(%rax), %edi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%r15, %rdi
	movq	%r12, %r15
	movq	%rax, %rsi
	callq	convert
	movq	$60, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	%rbx, 40(%r15)
	movq	%rbx, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_821
.LBB15_822:                             # %land.lhs.true3799
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	movq	$85, %r12
	jne	.LBB15_823
# BB#824:                               # %land.lhs.true3806
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	16(%rdx), %r12d
	leaq	-86(%r12), %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB15_829
# BB#825:                               # %land.lhs.true3834
	movq	40(%rdx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_827
# BB#826:                               # %if.then3844
	movq	%rdx, %r14
	movq	32(%r14), %rcx
	movq	$85, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	40(%r14), %rcx
	movq	%rax, %r14
	movq	$85, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	jmp	.LBB15_1343
.LBB15_807:                             # %if.end3720
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_809
# BB#808:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_151:                             # %sw.bb902
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	104(%rax), %rdi
	jmp	.LBB15_1928
.LBB15_294:                             # %sw.bb1469
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %eax
	cmpq	$46, %rax
	jne	.LBB15_295
# BB#296:                               # %if.then1476
	movq	40(%rcx), %rsi
	movq	%r12, %rdi
	callq	purpose_member
	testq	%rax, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#297:                               # %if.then1482
	movq	32(%rax), %r9
	jmp	.LBB15_1954
.LBB15_1881:                            # %sw.bb7402
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movl	pedantic(%rip), %ecx
	andq	$256, %rax              # imm = 0x100
	orq	%rax, %rcx
	testq	%r14, %rcx
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#1882:                              # %if.end7412
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1884
# BB#1883:                              # %if.then7415
	movq	$115, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB15_293
.LBB15_1599:                            # %sw.bb6598
	movq	168(%rsp), %rdi         # 8-byte Reload
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB15_1610
# BB#1600:                              # %if.then6605
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1601
# BB#1602:                              # %if.then6605
	movabsq	$1, %rax
	jmp	.LBB15_1603
.LBB15_1949:                            # %sw.bb7665
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB15_1953
# BB#1950:                              # %land.lhs.true7675
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB15_1953
# BB#1951:                              # %land.lhs.true7688
	movzbl	51(%rax), %eax
	testq	$96, %rax
	je	.LBB15_1953
# BB#1952:                              # %if.then7703
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	fold_builtin
	movq	%rax, %r9
	testq	%r9, %r9
	jne	.LBB15_1954
	jmp	.LBB15_1953
.LBB15_1929:                            # %sw.bb7565
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	has_cleanups
	testq	%r14, %rax
	je	.LBB15_1930
# BB#1931:                              # %if.end7572
	movzbl	16(%rbx), %r12d
	movzbl	tree_code_type(%r12), %eax
	movq	%rbx, %rcx
	movq	32(%rcx), %rbx
	cmpq	$49, %rax
	je	.LBB15_1933
# BB#1932:                              # %if.end7572
	cmpq	$96, %r12
	je	.LBB15_1933
# BB#1934:                              # %if.end7602
	cmpq	$60, %rax
	je	.LBB15_1942
# BB#1935:                              # %if.end7602
	cmpq	$50, %rax
	je	.LBB15_1942
# BB#1936:                              # %if.end7602
	cmpq	$91, %r12
	je	.LBB15_1942
# BB#1937:                              # %if.end7602
	cmpq	$92, %r12
	je	.LBB15_1942
# BB#1938:                              # %if.end7602
	cmpq	$93, %r12
	je	.LBB15_1942
# BB#1939:                              # %if.end7602
	cmpq	$94, %r12
	je	.LBB15_1942
# BB#1940:                              # %if.end7602
	cmpq	$95, %r12
	je	.LBB15_1942
# BB#1941:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_361:                             # %sw.bb1889
	movq	%r12, %rdx
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$77, %rcx
	jne	.LBB15_364
# BB#362:                               # %if.then1896
	movq	32(%rdx), %rcx
	movq	$60, %rdi
	jmp	.LBB15_363
.LBB15_555:                             # %sw.bb2978
	movzbl	16(%r12), %eax
	cmpq	$77, %rax
	jne	.LBB15_557
# BB#556:                               # %if.then2985
	movq	32(%r12), %rcx
	movq	$59, %rdi
.LBB15_363:                             # %return
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	168(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB15_1926
.LBB15_600:                             # %sw.bb3141
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$77, %rax
	movq	%r12, %rbx
	jne	.LBB15_603
# BB#601:                               # %land.lhs.true3148
	movzbl	16(%rbx), %eax
	cmpq	$77, %rax
	jne	.LBB15_603
# BB#602:                               # %if.then3155
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	movq	32(%rbx), %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_1926
.LBB15_773:                             # %sw.bb3569
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$77, %rax
	movq	%r12, %rbx
	jne	.LBB15_776
# BB#774:                               # %land.lhs.true3576
	movzbl	16(%rbx), %eax
	cmpq	$77, %rax
	jne	.LBB15_776
# BB#775:                               # %if.then3583
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	movq	32(%rbx), %rcx
	movq	$70, %rdi
	xorq	%rax, %rax
.LBB15_1324:                            # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
	callq	build
	jmp	.LBB15_715
.LBB15_299:                             # %sw.bb1493
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %eax
	movq	152(%rsp), %rdx         # 8-byte Reload
	testq	%r14, %rdx
	je	.LBB15_316
# BB#300:                               # %if.then1495
	cmpq	$26, %rax
	movq	120(%rsp), %rdx         # 8-byte Reload
	jne	.LBB15_301
# BB#314:                               # %if.then1558
	movq	48(%rcx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rcx), %rax
	movq	40(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	leaq	344(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	360(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	344(%rsp), %rax
	movq	352(%rsp), %rcx
	jmp	.LBB15_315
.LBB15_839:                             # %sw.bb3930
	xorq	%rdx, %rdx
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	%r12, %rbx
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_850
# BB#840:                               # %if.then3933
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rbx, %rdx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_841
# BB#842:                               # %if.end.i10685
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_846
# BB#843:                               # %if.end.i10685
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#844:                               # %if.end.i10685
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#845:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_863:                             # %sw.bb3970
	xorq	%rdx, %rdx
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	%r12, %rbx
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_873
# BB#864:                               # %if.then3973
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rbx, %rdx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_841
# BB#865:                               # %if.end.i10641
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_869
# BB#866:                               # %if.end.i10641
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#867:                               # %if.end.i10641
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#868:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_326:                             # %sw.bb1617
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	16(%rdx), %eax
	movq	152(%rsp), %rcx         # 8-byte Reload
	testq	%r14, %rcx
	je	.LBB15_343
# BB#327:                               # %if.then1619
	cmpq	$26, %rax
	jne	.LBB15_328
# BB#341:                               # %if.then1717
	movq	48(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rdx, %rbx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_negative
	testq	%r14, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#342:                               # %if.then1722
	addq	$32, %rbx
	movq	16(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	leaq	320(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	336(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	320(%rsp), %rax
	movq	328(%rsp), %rcx
.LBB15_315:                             # %return
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	build_real
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_703:                             # %sw.bb3325
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_704
# BB#712:                               # %if.end3331
	movq	%rbx, %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	je	.LBB15_716
# BB#713:                               # %if.then3334
	movq	$90, %rdi
	movq	168(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB15_714
.LBB15_758:                             # %sw.bb3547
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	jne	.LBB15_759
# BB#760:                               # %if.end3553
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	%rbx, %rsi
	movq	128(%rsp), %rbx         # 8-byte Reload
	je	.LBB15_771
# BB#761:                               # %if.then3556
	movq	%rbx, %rdi
	callq	convert
	movzbl	17(%r12), %ecx
	movq	%r12, %rdx
	movq	%rax, %r8
	testq	$1, %rcx
	je	.LBB15_763
# BB#762:                               # %if.then.i10767
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r8, %rcx
	jmp	.LBB15_520
.LBB15_357:                             # %sw.bb1838
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	je	.LBB15_359
# BB#358:                               # %if.then1840
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	32(%r14), %rdi
	movq	40(%r14), %rsi
	notq	%rdi
	notq	%rsi
	callq	build_int_2_wide
	movq	%rax, %rbx
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	force_fit_type
	movq	%rbx, %r9
	movl	16(%r14), %eax
	movl	16(%r9), %ecx
	andq	$524288, %rax           # imm = 0x80000
	andq	$-524289, %rcx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rax, %rcx
	movl	%ecx, 16(%r9)
	movl	16(%r14), %eax
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294705151, %rdx       # imm = 0xFFFBFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r9)
	jmp	.LBB15_1954
.LBB15_889:                             # %sw.bb4025
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_891
# BB#890:                               # %if.then4028
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_554
.LBB15_891:                             # %sw.bb4031
	movq	168(%rsp), %rdi         # 8-byte Reload
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB15_901
# BB#892:                               # %land.lhs.true4038
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_893
.LBB15_901:                             # %if.end4044
	movq	%r12, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_913
# BB#902:                               # %land.lhs.true4051
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_913
# BB#903:                               # %land.lhs.true4054
	cmpq	$91, 144(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_905
# BB#904:                               # %lor.lhs.false4057
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$1, %rax
	je	.LBB15_905
.LBB15_913:                             # %if.end4067
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_923
# BB#914:                               # %if.then4070
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	17(%rdx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_841
# BB#915:                               # %if.end.i10569
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_919
# BB#916:                               # %if.end.i10569
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#917:                               # %if.end.i10569
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#918:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1016:                            # %sw.bb4265
	movq	168(%rsp), %rdi         # 8-byte Reload
	movzbl	16(%rdi), %eax
	leaq	16(%rdi), %r13
	cmpq	$25, %rax
	jne	.LBB15_1017
# BB#1021:                              # %land.lhs.true4272
	callq	integer_zerop
	testq	%r14, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r12, %rbx
	jne	.LBB15_1018
# BB#1022:                              # %if.then4275
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_554
.LBB15_150:                             # %if.end901.sw.bb4278_crit_edge
	movq	168(%rsp), %rax         # 8-byte Reload
	leaq	16(%rax), %r13
.LBB15_1017:
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r12, %rbx
.LBB15_1018:                            # %sw.bb4278
	movzbl	(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB15_1024
# BB#1019:                              # %land.lhs.true4285
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1024
# BB#1020:                              # %if.then4288
	movq	%r15, %rdi
	movq	%rbx, %rsi
	jmp	.LBB15_705
.LBB15_1024:                            # %if.end4291
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	movq	144(%rsp), %r12         # 8-byte Reload
	jne	.LBB15_1036
# BB#1025:                              # %land.lhs.true4298
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1036
# BB#1026:                              # %land.lhs.true4301
	cmpq	$92, %r12
	jne	.LBB15_1028
# BB#1027:                              # %lor.lhs.false4304
	movzbl	1(%r13), %eax
	testq	$1, %rax
	je	.LBB15_1028
.LBB15_1036:                            # %if.end4314
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_1047
# BB#1037:                              # %land.lhs.true4321
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1038
.LBB15_1047:                            # %if.end4326
	movzbl	(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB15_936
# BB#1048:                              # %land.lhs.true4333
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_936
# BB#1049:                              # %if.then4336
	movq	%r15, %rdi
	movq	%r15, %r14
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_925
# BB#1050:                              # %if.end.i10475
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1054
# BB#1051:                              # %if.end.i10475
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1052:                              # %if.end.i10475
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1053:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1058:                            # %sw.bb4339
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_1059
.LBB15_759:                             # %if.then3550
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	jmp	.LBB15_705
.LBB15_888:                             # %sw.bb4014
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	invert_truthvalue
	movzbl	16(%rax), %ecx
	cmpq	$96, %rcx
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
	jmp	.LBB15_812
.LBB15_298:                             # %sw.bb1485
	movq	120(%rsp), %rdx         # 8-byte Reload
	movl	16(%rdx), %eax
	movq	152(%rsp), %rsi         # 8-byte Reload
	andq	$1, %rsi
	shlq	$9, %rsi
	movabsq	$4294966783, %rcx       # imm = 0xFFFFFDFF
	andq	%rax, %rcx
	orq	%rsi, %rcx
	movl	%ecx, 16(%rdx)
	movq	%rdx, %r9
	jmp	.LBB15_1954
.LBB15_290:                             # %sw.bb1450
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	32(%rdx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$117, %rcx
	movq	%rdx, %r9
	jne	.LBB15_1954
# BB#291:                               # %if.then1460
	movq	32(%rax), %rdx
	movq	$117, %rdi
	jmp	.LBB15_292
.LBB15_1885:                            # %sw.bb7419
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#1886:                              # %if.then7421
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
.LBB15_1887:                            # %return
	callq	build_complex
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_346:                             # %sw.bb1751
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	cmpq	$8, %rax
	jne	.LBB15_347
# BB#348:                               # %if.else1762
	movzbl	16(%rsi), %ebx
	movq	%rsi, %rcx
	cmpq	$124, %rbx
	jg	.LBB15_352
# BB#349:                               # %if.else1762
	leaq	-59(%rbx), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jae	.LBB15_350
# BB#355:                               # %if.then1805
	movq	%rcx, %r14
	movq	32(%r14), %rdx
	movq	$126, %rdi
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	40(%r14), %rdx
	movq	%rax, %r14
	movq	$126, %rdi
	movq	%r15, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	jmp	.LBB15_1925
.LBB15_1888:                            # %sw.bb7424
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rax
	movzbl	16(%rax), %eax
	cmpq	$8, %rax
	jne	.LBB15_1889
# BB#1890:                              # %if.else7434
	movq	%rcx, %rbx
	movzbl	16(%rbx), %r12d
	leaq	-59(%r12), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jae	.LBB15_1891
# BB#1904:                              # %if.then7473
	movq	32(%rbx), %rdx
	movq	$127, %rdi
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	40(%rbx), %rdx
	movq	%rax, %r14
	movq	$127, %rdi
	jmp	.LBB15_1924
.LBB15_1905:                            # %sw.bb7494
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rax
	movzbl	16(%rax), %eax
	cmpq	$8, %rax
	jne	.LBB15_1906
# BB#1907:                              # %if.else7505
	movq	%rcx, %rbx
	movzbl	16(%rbx), %r12d
	leaq	-59(%r12), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	jae	.LBB15_1908
# BB#1923:                              # %if.then7544
	movq	32(%rbx), %rdx
	movq	$128, %rdi
	movq	%r15, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	40(%rbx), %rdx
	movq	%rax, %r14
	movq	$128, %rdi
.LBB15_1924:                            # %return
	movq	%r15, %rsi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
.LBB15_1925:                            # %return
	movq	%r15, %rsi
	movq	%r14, %rdx
	jmp	.LBB15_1926
.LBB15_1092:                            # %land.lhs.true4425
	movl	16(%r13), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$26, %rcx
	jne	.LBB15_1102
# BB#1093:                              # %if.then4432
	movq	144(%rsp), %r15         # 8-byte Reload
	leaq	-97(%r15), %rax
	andq	%r14, %rax
	cmpq	$5, %rax
	ja	.LBB15_1098
# BB#1094:                              # %if.then4432
	jmpq	*.LJTI15_4(,%rax,8)
.LBB15_1096:                            # %sw.bb3.i10412
	movq	$99, %r15
	jmp	.LBB15_1101
.LBB15_1102:                            # %if.end4445
	andq	$255, %rax
	cmpq	$26, %rax
	jne	.LBB15_1106
# BB#1103:                              # %land.lhs.true4452
	movq	dconst0+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst0+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst0(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	48(%r13), %rax
	movq	%rax, 16(%rsp)
	movq	32(%r13), %rax
	movq	40(%r13), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r14, %rax
	jne	.LBB15_1106
# BB#1104:                              # %land.lhs.true4458
	leaq	32(%r13), %rax
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	%rcx, (%rsp)
	callq	ereal_isneg
	testq	%r14, %rax
	je	.LBB15_1106
# BB#1105:                              # %if.then4464
	movq	8(%r13), %rdi
	movq	dconst0+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst0+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst0(%rip), %rax
	movq	%rax, (%rsp)
	callq	build_real
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_636
.LBB15_1106:                            # %if.end4471
	movl	16(%rbx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$25, %rax
	jne	.LBB15_1107
# BB#1108:                              # %land.lhs.true4478
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB15_1109
.LBB15_1107:
	movq	%r13, %r8
	movq	%rbx, %r13
	jmp	.LBB15_1116
.LBB15_1109:                            # %if.then4485
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%r13, 32(%rdx)
	movq	%rbx, 40(%rdx)
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-97(%rcx), %rax
	andq	%r14, %rax
	cmpq	$5, %rax
	ja	.LBB15_1098
# BB#1110:                              # %if.then4485
	jmpq	*.LJTI15_5(,%rax,8)
.LBB15_1112:                            # %sw.bb3.i10405
	movq	$99, %rcx
	jmp	.LBB15_1115
.LBB15_796:                             # %if.end3686
	movq	144(%rsp), %r12         # 8-byte Reload
	leaq	-63(%r12), %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	ja	.LBB15_800
# BB#797:                               # %land.lhs.true3692
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	multiple_of_p
	testq	%r14, %rax
	je	.LBB15_800
# BB#798:                               # %if.then3695
	movq	$71, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB15_799
.LBB15_603:                             # %if.end3164
	movq	128(%rsp), %r15         # 8-byte Reload
	movzbl	16(%r15), %eax
	cmpq	$7, %rax
	je	.LBB15_644
# BB#604:                               # %if.end3164
	cmpq	$8, %rax
	jne	.LBB15_606
# BB#605:                               # %land.lhs.true3178
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_606
.LBB15_644:                             # %if.else3253
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_658
# BB#645:                               # %land.lhs.true3255
	movq	%rbx, %rdi
	callq	real_zerop
	testq	%r14, %rax
	je	.LBB15_658
# BB#646:                               # %if.then3258
	movq	%r15, %rdi
	movq	%r15, %r14
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	17(%rcx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_647
# BB#650:                               # %if.end.i10956
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_654
# BB#651:                               # %if.end.i10956
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#652:                               # %if.end.i10956
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#653:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_776:                             # %if.end3592
	movq	%rbx, %rdi
	callq	real_onep
	testq	%r14, %rax
	jne	.LBB15_704
# BB#777:                               # %if.end3598
	movzbl	16(%rbx), %eax
	cmpq	$26, %rax
	jne	.LBB15_786
# BB#778:                               # %if.then3605
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_783
# BB#779:                               # %land.lhs.true3607
	movq	dconst1+16(%rip), %rax
	movq	%rax, 16(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 8(%rsp)
	movq	dconst1(%rip), %rax
	movq	%rax, (%rsp)
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	build_real
	movq	$70, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_783
# BB#780:                               # %if.then3612
	movq	$61, %rdi
	jmp	.LBB15_781
.LBB15_115:                             # %land.lhs.true547
	movq	8(%rdi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB15_121
# BB#116:                               # %land.lhs.true547
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB15_121
# BB#117:                               # %land.lhs.true583
	movzbl	16(%rsi), %ecx
	cmpq	$12, %rcx
	ja	.LBB15_121
# BB#118:                               # %land.lhs.true583
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB15_121
# BB#119:                               # %land.lhs.true643
	movl	60(%rax), %eax
	movl	target_flags(%rip), %edx
	andq	$511, %rax              # imm = 0x1FF
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB15_120
.LBB15_121:                             # %if.then653
	movq	32(%rdi), %rdx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r9, %rcx
	callq	build
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	jmp	.LBB15_293
.LBB15_155:                             # %lor.rhs1001.thread
	movq	%r8, %rax
	andq	$253, %rax
	cmpq	$13, %rax
	jne	.LBB15_156
# BB#157:                               # %lor.rhs1001.thread
	movabsq	$1, %rdi
	jmp	.LBB15_158
.LBB15_809:                             # %if.end3724
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_810
# BB#811:                               # %land.lhs.true3731
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdx
	callq	extract_muldiv
	testq	%rax, %rax
	je	.LBB15_485
.LBB15_812:                             # %if.then3738
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	jmp	.LBB15_554
.LBB15_295:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1610:                            # %if.else6614
	movq	%rdi, %r15
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	48(%r13), %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_1622
# BB#1611:                              # %if.then6620
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%r15, %rdx
	movzbl	17(%rdx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_1612
.LBB15_841:                             # %if.then.i10681
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_364:                             # %if.end1902
	movq	168(%rsp), %rdi         # 8-byte Reload
	movzbl	16(%rdi), %ecx
	cmpq	$77, %rcx
	movq	128(%rsp), %rsi         # 8-byte Reload
	jne	.LBB15_366
# BB#365:                               # %if.then1909
	movq	32(%rdi), %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	jmp	.LBB15_1926
.LBB15_557:                             # %if.end2991
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %ecx
	cmpq	$77, %rcx
	movq	128(%rsp), %rbx         # 8-byte Reload
	jne	.LBB15_560
# BB#558:                               # %if.end2991
	cmpq	$26, %rax
	jne	.LBB15_560
# BB#559:                               # %if.then3005
	movq	8(%r12), %r14
	movq	48(%r12), %rax
	movq	%rax, 16(%rsp)
	movq	32(%r12), %rax
	movq	40(%r12), %rcx
	movq	%rcx, 8(%rsp)
	leaq	248(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	264(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	248(%rsp), %rax
	movq	256(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %rcx
	movq	%rax, %rdx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	jmp	.LBB15_1926
.LBB15_347:                             # %if.then1760
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_554
.LBB15_1889:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1906:                            # %if.then7503
	movq	global_trees+88(%rip), %rsi
	movq	128(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_554
.LBB15_1930:                            # %if.then7568
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r9
	jmp	.LBB15_1954
.LBB15_316:                             # %if.else1563
	cmpq	$60, %rax
	movq	120(%rsp), %rsi         # 8-byte Reload
	jne	.LBB15_317
# BB#319:                               # %land.lhs.true1581
	movq	%rcx, %rdx
	movq	%r14, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_320
# BB#323:                               # %lor.lhs.false1604
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jmp	.LBB15_324
.LBB15_850:                             # %if.end3935
	movq	128(%rsp), %rdx         # 8-byte Reload
	movzbl	16(%rdx), %ecx
	cmpq	$12, %rcx
	movq	$78, %r12
	ja	.LBB15_459
# BB#851:                               # %if.end3935
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB15_459
# BB#852:                               # %land.lhs.true3963
	movq	104(%rdx), %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_853
# BB#854:                               # %if.then3967
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	17(%rcx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_608
# BB#855:                               # %if.end.i10663
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_859
# BB#856:                               # %if.end.i10663
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#857:                               # %if.end.i10663
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#858:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_873:                             # %if.end3975
	movq	128(%rsp), %rdx         # 8-byte Reload
	movzbl	16(%rdx), %ecx
	cmpq	$12, %rcx
	movq	$79, %r12
	ja	.LBB15_459
# BB#874:                               # %if.end3975
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB15_459
# BB#875:                               # %land.lhs.true4003
	movq	112(%rdx), %rax
	testq	%rax, %rax
	je	.LBB15_876
# BB#877:                               # %land.lhs.true4006
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_878
# BB#879:                               # %if.then4011
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	17(%rcx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_608
# BB#880:                               # %if.end.i10619
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_884
# BB#881:                               # %if.end.i10619
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#882:                               # %if.end.i10619
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#883:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_343:                             # %if.else1730
	cmpq	$77, %rax
	je	.LBB15_345
# BB#344:                               # %if.else1730
	cmpq	$80, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
.LBB15_345:                             # %if.then1744
	movq	32(%rdx), %rdx
	movq	$80, %rdi
.LBB15_292:                             # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_293:                             # %return
	callq	build1
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_359:                             # %if.else1876
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$90, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#360:                               # %if.then1883
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r9
	jmp	.LBB15_1954
.LBB15_1059:                            # %if.end4345
	movq	%rbx, %r15
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	je	.LBB15_1068
# BB#1060:                              # %if.then4348
	movq	%r15, %rsi
	callq	convert
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jg	.LBB15_1064
# BB#1061:                              # %if.then4348
	cmpq	$29, %rcx
	ja	.LBB15_617
# BB#1062:                              # %if.then4348
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_617
# BB#1063:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_800:                             # %if.end3698
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_485
# BB#801:                               # %land.lhs.true3705
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	xorq	%rcx, %rcx
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	extract_muldiv
	testq	%rax, %rax
	je	.LBB15_485
# BB#802:                               # %if.then3712
	movq	%r15, %rdi
	movq	%rax, %rsi
	jmp	.LBB15_554
.LBB15_160:
	movabsq	$0, %rdi
.LBB15_162:                             # %lor.end
	cmpq	$7, %rax
	jne	.LBB15_164
# BB#163:
	movq	$1, 144(%rsp)           # 8-byte Folded Spill
	jmp	.LBB15_170
.LBB15_816:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_823:
	movq	%r15, 120(%rsp)         # 8-byte Spill
	jmp	.LBB15_485
.LBB15_301:                             # %if.then1495
	cmpq	$25, %rax
	movq	%rdx, %r9
	jne	.LBB15_1954
# BB#302:                               # %if.then1502
	movq	%rcx, %rax
	movq	32(%rax), %rcx
	movq	40(%rax), %rdx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	je	.LBB15_303
# BB#307:                               # %if.else.i11044
	negq	%rcx
	notq	%rdx
	movq	$1, %r12
	movq	%rdx, %rax
	jmp	.LBB15_308
.LBB15_328:                             # %if.then1619
	movq	%rdx, %rbx
	cmpq	$25, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#329:                               # %if.then1626
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB15_331
# BB#330:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_923:                             # %if.end4072
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	144(%rsp), %r12         # 8-byte Reload
	je	.LBB15_936
# BB#924:                               # %if.then4075
	movq	%r15, %rdi
	movq	%r15, %r14
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	je	.LBB15_928
.LBB15_925:                             # %if.then.i10543
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	jmp	.LBB15_926
.LBB15_936:                             # %truth_andor
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_937
# BB#938:                               # %if.end4080
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	movzbl	16(%rbx), %ecx
	cmpq	%rcx, %rax
	jne	.LBB15_955
# BB#939:                               # %if.end4080
	leaq	-91(%rax), %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB15_955
# BB#940:                               # %land.lhs.true4119
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %r13
	movzbl	17(%r13), %ecx
	testq	$1, %rcx
	jne	.LBB15_955
# BB#941:                               # %if.then4129
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	%rbx, %rcx
	movq	32(%rcx), %rdx
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movq	40(%rcx), %rdx
	movq	%rdx, 136(%rsp)         # 8-byte Spill
                                        # kill: RBX<def> RCX<kill>
	addq	$-93, %rax
	andq	%r14, %rax
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	cmpq	$1, %rax
	ja	.LBB15_943
# BB#942:                               # %land.rhs4160
	leaq	-93(%r12), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB15_943:                             # %land.end4168
	xorq	%rdx, %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_946
# BB#944:                               # %if.then4172
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %r14d
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	136(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB15_945
.LBB15_156:
	movabsq	$0, %rdi
.LBB15_158:                             # %lor.rhs1001.thread
	xorq	%r11, %r11
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	jmp	.LBB15_170
.LBB15_810:
	movq	144(%rsp), %r12         # 8-byte Reload
	jmp	.LBB15_485
.LBB15_164:                             # %lor.rhs1001
	cmpq	$8, %rax
	jne	.LBB15_165
# BB#166:                               # %land.rhs1008
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_167
# BB#168:                               # %land.rhs1008
	movabsq	$1, %rax
	jmp	.LBB15_169
.LBB15_1601:
	movabsq	$0, %rax
.LBB15_1603:                            # %if.then6605
	movq	120(%rsp), %rdx         # 8-byte Reload
	movl	pedantic_lvalues(%rip), %ecx
	movq	40(%rdx,%rax,8), %r9
	testq	%rcx, %rcx
	je	.LBB15_1954
# BB#1604:                              # %if.then.i10099
	movzbl	16(%r9), %eax
	cmpq	$115, %rax
	jg	.LBB15_1607
# BB#1605:                              # %if.then.i10099
	cmpq	$29, %rax
	ja	.LBB15_1609
# BB#1606:                              # %if.then.i10099
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	jne	.LBB15_1954
	jmp	.LBB15_1609
.LBB15_1933:                            # %if.then7595
	movq	8(%rbx), %rsi
	movq	$56, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%r12, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_702
.LBB15_893:                             # %if.then4041
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	convert
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jg	.LBB15_897
# BB#894:                               # %if.then4041
	cmpq	$29, %rcx
	ja	.LBB15_617
# BB#895:                               # %if.then4041
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_617
# BB#896:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_352:                             # %if.else1762
	cmpq	$126, %rbx
	jne	.LBB15_353
# BB#356:                               # %if.then1829
	movq	32(%rcx), %r9
	jmp	.LBB15_1954
.LBB15_1891:                            # %if.else7434
	cmpq	$27, %r12
	jne	.LBB15_1892
# BB#1903:                              # %if.then7456
	movq	32(%rbx), %r9
	jmp	.LBB15_1954
.LBB15_1908:                            # %if.else7505
	cmpq	$27, %r12
	jne	.LBB15_1909
# BB#1922:                              # %if.then7527
	movq	40(%rbx), %r9
	jmp	.LBB15_1954
.LBB15_366:                             # %if.else1915
	movzbl	16(%rsi), %ecx
	cmpq	$7, %rcx
	je	.LBB15_433
# BB#367:                               # %if.else1915
	cmpq	$8, %rcx
	jne	.LBB15_369
# BB#368:                               # %land.lhs.true1929
	movq	8(%rsi), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB15_369
.LBB15_433:                             # %if.else2295
	movq	%rdx, %r12
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB15_436
# BB#434:                               # %land.lhs.true2297
	movq	%r12, %rdi
	callq	real_zerop
	testq	%r14, %rax
	je	.LBB15_435
# BB#443:                               # %if.then2300
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_447
# BB#444:                               # %if.then2300
	cmpq	$29, %rax
	ja	.LBB15_686
# BB#445:                               # %if.then2300
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_686
# BB#446:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_560:                             # %if.end3017
	movzbl	16(%rbx), %eax
	cmpq	$7, %rax
	je	.LBB15_579
# BB#561:                               # %if.end3017
	cmpq	$8, %rax
	jne	.LBB15_563
# BB#562:                               # %land.lhs.true3031
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_563
.LBB15_579:                             # %if.else3092
	movq	%r12, %rbx
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_591
# BB#580:                               # %if.then3094
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB15_582
# BB#581:                               # %land.lhs.true3096
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	real_zerop
	testq	%r14, %rax
	jne	.LBB15_565
.LBB15_582:                             # %if.end3102
	movq	%rbx, %rdi
	callq	real_zerop
	testq	%r14, %rax
	je	.LBB15_591
# BB#583:                               # %if.then3105
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_587
# BB#584:                               # %if.then3105
	cmpq	$29, %rax
	ja	.LBB15_686
# BB#585:                               # %if.then3105
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_686
# BB#586:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_317:                             # %if.else1563
	cmpq	$77, %rax
	movq	%rsi, %r9
	jne	.LBB15_1954
# BB#318:                               # %if.then1570
	movq	32(%rcx), %r9
	jmp	.LBB15_1954
.LBB15_1884:                            # %if.end7417
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB15_554
.LBB15_846:                             # %if.end.i10685
	cmpq	$116, %rax
	jne	.LBB15_848
# BB#847:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_869:                             # %if.end.i10641
	cmpq	$116, %rax
	jne	.LBB15_871
# BB#870:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_716:                             # %if.end3337
	movq	%rbx, %r12
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$88, %rax
	movq	$87, %r15
	jne	.LBB15_455
# BB#717:                               # %land.lhs.true3344
	movzbl	16(%r12), %eax
	cmpq	$88, %rax
	jne	.LBB15_455
# BB#718:                               # %land.lhs.true3351
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_455
# BB#719:                               # %land.lhs.true3361
	movq	40(%r12), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$25, %rdx
	jne	.LBB15_455
# BB#720:                               # %land.lhs.true3371
	movq	$88, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_675
	jmp	.LBB15_455
.LBB15_771:                             # %if.end3558
	movzbl	16(%r12), %eax
	cmpq	$25, %rax
	jne	.LBB15_772
# BB#721:                               # %if.then3565
	movq	$90, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
.LBB15_722:                             # %bit_and
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	je	.LBB15_723
.LBB15_704:                             # %if.then3328
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
.LBB15_705:                             # %if.then3328
	callq	convert
	movq	%rax, %r9
.LBB15_706:                             # %if.then3328
	movzbl	16(%r9), %ecx
	cmpq	$115, %rcx
	jg	.LBB15_709
# BB#707:                               # %if.then3328
	cmpq	$29, %rcx
	ja	.LBB15_711
# BB#708:                               # %if.then3328
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$637534208, %rax        # imm = 0x26000000
	jne	.LBB15_1954
	jmp	.LBB15_711
.LBB15_709:                             # %if.then3328
	cmpq	$116, %rcx
	je	.LBB15_1954
# BB#710:                               # %if.then3328
	cmpq	$121, %rcx
	je	.LBB15_1954
.LBB15_711:                             # %if.end.i10893
	movq	8(%r9), %rsi
	movq	$116, %rdi
	movq	%r9, %rdx
	movq	%r9, %rbx
.LBB15_619:                             # %return
	callq	build1
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_723:                             # %if.end3389
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	128(%rsp), %r15         # 8-byte Reload
	je	.LBB15_734
# BB#724:                               # %if.then3392
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	17(%rdx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_726
# BB#725:                               # %if.then.i10803
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_1028:                            # %if.then4311
	movq	%r15, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jg	.LBB15_1032
# BB#1029:                              # %if.then4311
	cmpq	$29, %rcx
	ja	.LBB15_617
# BB#1030:                              # %if.then4311
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_617
# BB#1031:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1038:                            # %if.then4324
	movq	%r15, %rdi
	movq	%r15, %r14
	movq	%rbx, %rsi
	callq	convert
	movzbl	1(%r13), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_1039
.LBB15_647:                             # %if.then.i10952
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	jmp	.LBB15_648
.LBB15_1113:                            # %sw.bb4.i10406
	movq	$100, %rcx
	jmp	.LBB15_1115
.LBB15_1114:                            # %sw.bb1.i10403
	movq	$97, %rcx
	jmp	.LBB15_1115
.LBB15_1111:                            # %sw.bb2.i10404
	movq	$98, %rcx
.LBB15_1115:                            # %swap_tree_comparison.exit10409
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movb	%cl, 16(%rdx)
	movl	16(%r13), %edx
	movq	%rbx, %r8
.LBB15_1116:                            # %if.end4505
	movl	16(%r8), %eax
	movq	$54, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$63, %rcx
	sarq	%cl, %rbx
	movq	%rax, %r12
	andq	$512, %r12              # imm = 0x200
	movq	%rbx, %r15
	andq	%r8, %r15
	andq	%r13, %rbx
	movq	$8, %rcx
	shrq	%cl, %r12
	decq	%r12
	andq	$512, %rdx              # imm = 0x200
	je	.LBB15_1118
# BB#1117:                              # %if.end4505
	movq	%r13, %r15
.LBB15_1118:                            # %if.end4505
	je	.LBB15_1120
# BB#1119:                              # %if.end4505
	movq	%r8, %rbx
.LBB15_1120:                            # %if.end4505
	testq	%rdx, %rdx
	je	.LBB15_1122
# BB#1121:                              # %if.end4505
	xorq	%r12, %r12
.LBB15_1122:                            # %if.end4505
	testq	%r15, %r15
	je	.LBB15_1157
# BB#1123:                              # %land.lhs.true4526
	movzbl	16(%rbx), %ecx
	cmpq	$131, %rcx
	jne	.LBB15_1124
# BB#1153:                              # %if.then4713
	movq	8(%rbx), %rsi
	movzbl	16(%rsi), %ecx
	addq	$-7, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	ja	.LBB15_1167
# BB#1154:                              # %if.then4713
	jmpq	*.LJTI15_6(,%rcx,8)
.LBB15_1155:                            # %land.lhs.true4749
	movq	8(%rsi), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	je	.LBB15_1157
# BB#1156:                              # %land.lhs.true4749
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-101(%rcx), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jb	.LBB15_1168
	jmp	.LBB15_1157
.LBB15_1124:                            # %land.lhs.true4526
	cmpq	$132, %rcx
	jne	.LBB15_1157
# BB#1125:                              # %if.then4533
	movq	8(%rbx), %rsi
	movzbl	16(%rsi), %ecx
	addq	$-7, %rcx
	andq	%r14, %rcx
	cmpq	$8, %rcx
	ja	.LBB15_1129
# BB#1126:                              # %if.then4533
	jmpq	*.LJTI15_7(,%rcx,8)
.LBB15_1127:                            # %land.lhs.true4569
	movq	8(%rsi), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	je	.LBB15_1157
# BB#1128:                              # %land.lhs.true4569
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-101(%rcx), %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jb	.LBB15_1130
	jmp	.LBB15_1157
.LBB15_1167:                            # %land.lhs.true4760
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-101(%rcx), %rcx
	andq	%r14, %rcx
	cmpq	$1, %rcx
	ja	.LBB15_1157
.LBB15_1168:                            # %if.then4766
	movq	40(%rbx), %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%rbx), %rsi
	movq	32(%rbx), %rdx
	movq	40(%rbx), %rcx
	movq	%rax, %r13
	movq	$129, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rbx
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$39, %rax
	jne	.LBB15_1185
# BB#1169:                              # %land.lhs.true4794
	movq	40(%rcx), %rax
	movzbl	49(%rax), %edx
	testq	$16, %rdx
	je	.LBB15_1185
# BB#1170:                              # %if.then4808
	movq	40(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	testq	%r14, %r12
	je	.LBB15_1171
# BB#1172:                              # %if.else4839
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r15, %rcx
	jmp	.LBB15_1173
.LBB15_1129:                            # %land.lhs.true4580
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-101(%rcx), %rcx
	andq	%r14, %rcx
	cmpq	$1, %rcx
	ja	.LBB15_1157
.LBB15_1130:                            # %if.then4586
	movq	40(%rbx), %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	8(%rbx), %rsi
	movq	32(%rbx), %rdx
	movq	40(%rbx), %rcx
	movq	%rax, %r13
	movq	$130, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rbx
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$39, %rax
	jne	.LBB15_1148
# BB#1131:                              # %land.lhs.true4613
	movq	40(%rcx), %rax
	movzbl	49(%rax), %edx
	testq	$16, %rdx
	je	.LBB15_1148
# BB#1132:                              # %if.then4627
	movq	40(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	xorq	%rax, %rax
	testq	%r14, %r12
	je	.LBB15_1133
# BB#1134:                              # %if.else4654
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r15, %rcx
	jmp	.LBB15_1135
.LBB15_1157:                            # %if.end4892
	movq	8(%r8), %rdx
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB15_1266
# BB#1158:                              # %land.lhs.true4910
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB15_1266
# BB#1159:                              # %land.lhs.true4910
	andq	$254, %rsi
	movzbl	mode_bitsize(%rsi), %esi
	cmpq	$64, %rsi
	ja	.LBB15_1266
# BB#1160:                              # %land.lhs.true4920
	movl	16(%rdx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	ja	.LBB15_1266
# BB#1161:                              # %land.lhs.true4920
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	$48192, %rdi            # imm = 0xBC40
	je	.LBB15_1266
# BB#1162:                              # %if.then4974
	movq	40(%r8), %rdi
	testq	%rdi, %rdi
	jne	.LBB15_1209
# BB#1163:                              # %land.lhs.true4980
	movq	32(%r8), %rbx
	leaq	-1(%rsi), %rcx
	movq	%r8, %r9
	movq	$1, %r8
	shlq	%cl, %r8
	decq	%r8
	cmpq	%r8, %rbx
	movq	%r9, %r8
	jne	.LBB15_1233
# BB#1164:                              # %land.lhs.true4990
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB15_1165
.LBB15_1209:                            # %if.else5022
	movq	%r8, %rbx
	cmpq	$-1, %rdi
	jne	.LBB15_1232
# BB#1210:                              # %land.lhs.true5028
	movq	32(%rbx), %r9
	leaq	-1(%rsi), %rcx
	movq	$1, %r8
	shlq	%cl, %r8
	cmpq	%r8, %r9
	jne	.LBB15_1232
# BB#1211:                              # %land.lhs.true5028
	movq	%rdx, %rcx
	andq	$8192, %rcx             # imm = 0x2000
	testq	%r14, %rcx
	jne	.LBB15_1232
# BB#1212:                              # %if.then5046
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-97, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	movq	%rbx, %r8
	ja	.LBB15_1266
# BB#1213:                              # %if.then5046
	movq	%r8, %rdx
	movq	%r13, %r15
	jmpq	*.LJTI15_9(,%rcx,8)
.LBB15_1214:                            # %sw.bb5051
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1191
# BB#1215:                              # %if.end.i10301
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1219
# BB#1216:                              # %if.end.i10301
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1217:                              # %if.end.i10301
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1218:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_606:                             # %if.then3187
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_620
# BB#607:                               # %if.then3190
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	17(%rcx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_609
.LBB15_608:                             # %if.then.i10988
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_648:                             # %return
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_658:                             # %if.end3260
	movq	%rbx, %rdi
	callq	real_onep
	testq	%r14, %rax
	je	.LBB15_667
# BB#659:                               # %if.then3263
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_663
# BB#660:                               # %if.then3263
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#661:                               # %if.then3263
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#662:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1622:                            # %if.end6623
	movq	%rbx, %rdi
	movq	%rbx, %r12
	callq	integer_zerop
	testq	%r14, %rax
	movq	%r14, %rbx
	jne	.LBB15_1627
# BB#1623:                              # %lor.lhs.false6626
	movzbl	17(%r12), %eax
	testq	$2, %rax
	je	.LBB15_1625
# BB#1624:                              # %land.lhs.true6633
	movq	48(%r13), %rax
	movzbl	17(%rax), %eax
	testq	$2, %rax
	je	.LBB15_1627
.LBB15_1625:                            # %lor.lhs.false6643
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB15_1633
# BB#1626:                              # %land.lhs.true6653
	movq	48(%r13), %rax
	movq	%r15, %rsi
	movq	32(%rsi), %rcx
	movq	40(%rsi), %rdx
                                        # kill: R15<def> RSI<kill>
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	operand_equal_for_comparison_p
	testq	%rbx, %rax
	je	.LBB15_1633
.LBB15_1627:                            # %if.then6665
	movq	%r15, %rdi
	callq	invert_truthvalue
	movq	%rax, %r14
	movzbl	16(%r14), %eax
	cmpq	$96, %rax
	je	.LBB15_1633
# BB#1628:                              # %if.then6673
	movq	%r13, %rax
	movq	40(%rax), %r8
	movq	48(%rax), %rcx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	callq	build
	movq	40(%rax), %rcx
	movq	%rax, %r13
	movq	global_trees(%rip), %rax
.LBB15_1629:                            # %while.cond6684
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movzbl	16(%rdx), %ecx
	movq	%rdx, %r12
	addq	$-114, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	ja	.LBB15_1632
# BB#1630:                              # %land.lhs.true6705
                                        #   in Loop: Header=BB15_1629 Depth=1
	movq	32(%r12), %rcx
	cmpq	%rax, %rcx
	je	.LBB15_1632
# BB#1631:                              # %land.rhs6711
                                        #   in Loop: Header=BB15_1629 Depth=1
	movq	8(%r12), %rdx
	movq	8(%rcx), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB15_1629
.LBB15_1632:
	movq	%r14, %r15
.LBB15_1633:                            # %if.end6739
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	jne	.LBB15_1833
# BB#1634:                              # %land.lhs.true6749
	movq	32(%r15), %rax
	movq	8(%rax), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$7, %rdx
	jne	.LBB15_1635
# BB#1638:                              # %lor.lhs.false6787
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB15_1639
	jmp	.LBB15_1833
.LBB15_1612:                            # %if.end.i10093
	movl	pedantic_lvalues(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_1613
# BB#1614:                              # %if.then.i.i
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1618
# BB#1615:                              # %if.then.i.i
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#1616:                              # %if.then.i.i
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#1617:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_320:                             # %land.lhs.true1581
	cmpq	$8, %rax
	jne	.LBB15_325
# BB#321:                               # %land.lhs.true1595
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_325
# BB#322:                               # %land.lhs.true1595
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rcx, %rax
.LBB15_324:                             # %lor.lhs.false1604
	movq	%rsi, %r9
	je	.LBB15_1954
.LBB15_325:                             # %if.then1606
	movq	%rdx, %rax
	movq	32(%rax), %rcx
	movq	40(%rax), %rdx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_520
.LBB15_1068:                            # %if.end4351
	movq	%rdi, %r12
	movq	%r15, %rdi
	callq	integer_onep
	movq	%rbx, %rdi
	testq	%r14, %rax
	je	.LBB15_1077
# BB#1069:                              # %if.then4354
	callq	invert_truthvalue
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_1073
# BB#1070:                              # %if.then4354
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1071:                              # %if.then4354
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1072:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1064:                            # %if.then4348
	cmpq	$116, %rcx
	jne	.LBB15_1066
# BB#1065:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_937:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_165:
	movq	$0, 144(%rsp)           # 8-byte Folded Spill
	jmp	.LBB15_170
.LBB15_1607:                            # %if.then.i10099
	cmpq	$116, %rax
	je	.LBB15_1954
# BB#1608:                              # %if.then.i10099
	cmpq	$121, %rax
	je	.LBB15_1954
.LBB15_1609:                            # %if.end.i.i10108
	movq	%r9, %rbx
	jmp	.LBB15_618
.LBB15_897:                             # %if.then4041
	cmpq	$116, %rcx
	jne	.LBB15_899
# BB#898:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_353:                             # %if.else1762
	cmpq	$125, %rbx
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#354:                               # %if.then1769
	movq	%rcx, %rax
	movq	32(%rax), %rbx
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	negate_expr
	movq	%rax, %rcx
	movq	$125, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB15_520
.LBB15_1892:                            # %if.else7434
	cmpq	$125, %r12
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#1893:                              # %if.then7441
	movq	%rbx, %rcx
	movq	32(%rcx), %rax
	movq	40(%rcx), %r14
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r14), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_1895
# BB#1894:                              # %if.then.i9815
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_1912
.LBB15_1909:                            # %if.else7505
	cmpq	$125, %r12
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#1910:                              # %if.then7512
	movq	%rbx, %rax
	movq	32(%rax), %r14
	movq	40(%rax), %rax
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r14), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_1914
# BB#1911:                              # %if.then.i
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
.LBB15_1912:                            # %return
	movq	%r14, %rdx
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_734:                             # %if.end3394
	movq	$88, %rdi
	movq	%r15, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	distribute_bit_expr
	movq	%rax, %r9
	testq	%r9, %r9
	jne	.LBB15_1954
# BB#735:                               # %if.end3399
	movl	16(%rbx), %eax
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %edx
	andq	$255, %rax
	cmpq	$25, %rdx
	jne	.LBB15_742
# BB#736:                               # %if.end3399
	cmpq	$115, %rax
	jne	.LBB15_742
# BB#737:                               # %land.lhs.true3413
	movq	32(%rbx), %r9
	movq	8(%r9), %rcx
	movzbl	17(%rcx), %esi
	testq	$32, %rsi
	je	.LBB15_742
# BB#738:                               # %if.then3425
	movl	60(%rcx), %esi
	movl	target_flags(%rip), %edi
	andq	$511, %rsi              # imm = 0x1FF
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rdi, %rcx
	cmpq	%rcx, %rsi
	jae	.LBB15_742
# BB#739:                               # %if.then3425
	movq	%rsi, %rcx
	andq	%r14, %rcx
	cmpq	$63, %rcx
	ja	.LBB15_742
# BB#740:                               # %land.lhs.true3444
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %rdi
	notq	%rdi
	movq	$1, %r8
	movq	%rsi, %rcx
	shlq	%cl, %r8
	decq	%r8
	testq	%rdi, %r8
	je	.LBB15_741
.LBB15_742:                             # %if.end3459
	cmpq	$25, %rax
	jne	.LBB15_753
# BB#743:                               # %if.end3459
	cmpq	$115, %rdx
	jne	.LBB15_753
# BB#744:                               # %land.lhs.true3473
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	8(%rax), %rcx
	movzbl	17(%rcx), %edx
	testq	$32, %rdx
	je	.LBB15_745
# BB#746:                               # %if.then3485
	movl	60(%rcx), %edx
	movl	target_flags(%rip), %esi
	andq	$511, %rdx              # imm = 0x1FF
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	movabsq	$4294967264, %rcx       # imm = 0xFFFFFFE0
	andq	%rsi, %rcx
	cmpq	%rcx, %rdx
	jae	.LBB15_747
# BB#748:                               # %if.then3485
	movq	%rdx, %rcx
	andq	%r14, %rcx
	cmpq	$63, %rcx
	ja	.LBB15_749
# BB#750:                               # %land.lhs.true3505
	movq	32(%rbx), %rsi
	notq	%rsi
	movq	$1, %rdi
	movq	%rdx, %rcx
	shlq	%cl, %rdi
	decq	%rdi
	testq	%rsi, %rdi
	movq	$88, %r12
	jne	.LBB15_459
# BB#751:                               # %if.then3516
	movq	$115, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	jmp	.LBB15_293
.LBB15_821:
	movq	%r15, %r9
	jmp	.LBB15_1954
.LBB15_827:                             # %land.lhs.true3864
	cmpq	$25, %rax
	jne	.LBB15_828
.LBB15_829:                             # %land.lhs.true3871
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	movzbl	16(%rdx), %eax
	cmpq	$85, %rax
	jne	.LBB15_830
# BB#831:                               # %land.lhs.true3878
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_832
# BB#833:                               # %land.lhs.true3888
	cmpq	$0, 40(%rbx)
	je	.LBB15_835
# BB#834:
	movq	$85, %r12
	jmp	.LBB15_485
.LBB15_1942:                            # %if.then7623
	movzbl	17(%rbx), %eax
	movq	40(%rcx), %r15
	testq	$2, %rax
	jne	.LBB15_1945
# BB#1943:                              # %lor.lhs.false7633
	leaq	-91(%r12), %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB15_1946
# BB#1944:                              # %land.lhs.true7639
	movq	%rbx, %rdi
	callq	has_cleanups
	testq	%r14, %rax
	je	.LBB15_1945
.LBB15_1946:                            # %if.end7649
	movzbl	17(%r15), %eax
	testq	$2, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#1947:                              # %if.then7656
	movq	8(%rbx), %rax
	movq	$56, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_1948
.LBB15_763:                             # %if.end.i10771
	movzbl	16(%r8), %eax
	movq	%r8, %rsi
	cmpq	$115, %rax
	jg	.LBB15_767
# BB#764:                               # %if.end.i10771
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#765:                               # %if.end.i10771
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#766:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_905:                             # %if.then4064
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_909
# BB#906:                               # %if.then4064
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#907:                               # %if.then4064
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#908:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_919:                             # %if.end.i10569
	cmpq	$116, %rax
	jne	.LBB15_921
# BB#920:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1054:                            # %if.end.i10475
	cmpq	$116, %rax
	jne	.LBB15_1056
# BB#1055:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_350:                             # %if.else1762
	cmpq	$27, %rbx
	movq	120(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_1954
# BB#351:                               # %if.then1785
	movq	%rcx, %rax
	movq	32(%rax), %rbx
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	negate_expr
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	jmp	.LBB15_1887
.LBB15_1097:                            # %sw.bb4.i10413
	movq	$100, %r15
	jmp	.LBB15_1101
.LBB15_1100:                            # %sw.bb1.i10410
	movq	$97, %r15
	jmp	.LBB15_1101
.LBB15_1095:                            # %sw.bb2.i10411
	movq	$98, %r15
.LBB15_1101:                            # %swap_tree_comparison.exit10416
	movq	32(%rbx), %r14
	movq	8(%r13), %rbx
	movq	48(%r13), %rax
	movq	%rax, 16(%rsp)
	movq	32(%r13), %rax
	movq	40(%r13), %rcx
	movq	%rcx, 8(%rsp)
	leaq	200(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	ereal_negate
	movq	216(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	200(%rsp), %rax
	movq	208(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	callq	build_real
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	jmp	.LBB15_1342
.LBB15_654:                             # %if.end.i10956
	cmpq	$116, %rax
	jne	.LBB15_656
# BB#655:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_783:                             # %if.else3615
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_786
# BB#784:                               # %if.then3617
	movq	48(%rbx), %rax
	movq	%rax, 240(%rsp)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rcx
	movq	%rcx, 232(%rsp)
	movq	%rax, 224(%rsp)
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	leaq	224(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	exact_real_inverse
	testq	%r14, %rax
	je	.LBB15_786
# BB#785:                               # %if.then3630
	movq	240(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	224(%rsp), %rax
	movq	232(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	build_real
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	jmp	.LBB15_782
.LBB15_786:                             # %if.end3637
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_787
# BB#788:                               # %land.lhs.true3639
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$70, %rax
	jne	.LBB15_790
# BB#789:                               # %if.then3646
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r15
	movq	40(%rax), %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rcx
	movq	$70, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	build
	jmp	.LBB15_715
.LBB15_120:
	movq	%rdi, %r9
	jmp	.LBB15_1954
.LBB15_853:
	movq	$78, %r12
	jmp	.LBB15_459
.LBB15_859:                             # %if.end.i10663
	cmpq	$116, %rax
	jne	.LBB15_861
# BB#860:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_876:
	movq	$79, %r12
	jmp	.LBB15_459
.LBB15_878:
	movq	$79, %r12
	jmp	.LBB15_459
.LBB15_884:                             # %if.end.i10619
	cmpq	$116, %rax
	jne	.LBB15_886
# BB#885:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_303:                             # %if.then.i11043
	movq	%rdx, %rax
	negq	%rax
	testq	%rax, %rdx
	js	.LBB15_304
# BB#305:                               # %if.then.i11043
	movabsq	$1, %r12
	xorq	%rcx, %rcx
	jmp	.LBB15_308
.LBB15_331:                             # %if.else1634
	movq	global_trees+104(%rip), %rax
	movq	40(%rax), %rcx
	movq	40(%rbx), %r14
	cmpq	%r14, %rcx
	jge	.LBB15_333
# BB#332:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_928:                             # %if.end.i10547
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_932
# BB#929:                               # %if.end.i10547
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#930:                               # %if.end.i10547
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#931:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_946:                             # %if.else4181
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	je	.LBB15_951
# BB#947:                               # %land.lhs.true4183
	xorq	%rdx, %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_949
# BB#948:                               # %if.then4186
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %r14d
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	152(%rsp), %rcx         # 8-byte Reload
.LBB15_945:                             # %return
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
	callq	build
	jmp	.LBB15_715
.LBB15_167:
	movabsq	$0, %rax
.LBB15_169:                             # %land.rhs1008
	movq	%rax, 144(%rsp)         # 8-byte Spill
.LBB15_170:                             # %lor.end1018
	movl	16(%rsi), %r10d
	movl	60(%r15), %eax
	andq	$511, %rax              # imm = 0x1FF
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %r8
	movq	%r10, %rax
	andq	$255, %rax
	leaq	-6(%rax), %rcx
	andq	$1, %r8
	movq	%rcx, %rdx
	andq	%r14, %rdx
	cmpq	$7, %rdx
	jae	.LBB15_171
# BB#175:                               # %lor.end1056
	movq	%r9, 160(%rsp)          # 8-byte Spill
	andq	$127, %rcx
	movq	$113, %r9
	shrq	%cl, %r9
	andq	$1, %r9
	movq	%r10, %rcx
	andq	$253, %rcx
	cmpq	$13, %rcx
	jne	.LBB15_176
# BB#177:                               # %lor.end1056
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movabsq	$1, %rcx
	jmp	.LBB15_178
.LBB15_171:                             # %lor.rhs1079.thread
	movq	%r9, 160(%rsp)          # 8-byte Spill
	movq	%r10, %rax
	andq	$253, %rax
	cmpq	$13, %rax
	jne	.LBB15_172
# BB#173:                               # %lor.rhs1079.thread
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB15_174
.LBB15_176:
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movabsq	$0, %rcx
.LBB15_178:                             # %lor.end1056
	movq	%rcx, 112(%rsp)         # 8-byte Spill
	cmpq	$7, %rax
	jne	.LBB15_180
# BB#179:
	movq	$1, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB15_186
.LBB15_172:
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
.LBB15_174:                             # %lor.rhs1079.thread
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%r9, %r9
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB15_186
.LBB15_180:                             # %lor.rhs1079
	cmpq	$8, %rax
	jne	.LBB15_181
# BB#182:                               # %land.rhs1086
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_183
# BB#184:                               # %land.rhs1086
	movabsq	$1, %rax
	jmp	.LBB15_185
.LBB15_181:
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB15_186
.LBB15_369:                             # %if.then1938
	movq	%rdx, %rdi
	movq	%rdx, %r12
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_378
# BB#370:                               # %if.then1941
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_374
# BB#371:                               # %if.then1941
	cmpq	$29, %rax
	ja	.LBB15_686
# BB#372:                               # %if.then1941
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_686
# BB#373:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_435:                             # %land.lhs.true2297.if.else2303_crit_edge
	movl	16(%r12), %eax
.LBB15_436:                             # %if.else2303
	andq	$255, %rax
	cmpq	$26, %rax
	movq	$59, %r15
	jne	.LBB15_455
# BB#437:                               # %land.lhs.true2310
	movq	dconst0+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst0+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst0(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	%r12, %rdx
	movq	48(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
                                        # kill: R12<def> RDX<kill>
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r14, %rax
	jne	.LBB15_455
# BB#438:                               # %land.lhs.true2316
	leaq	32(%r12), %rax
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	movq	%rax, 8(%rsp)
	movq	%rcx, (%rsp)
	callq	ereal_isneg
	testq	%r14, %rax
	je	.LBB15_455
# BB#439:                               # %if.then2322
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_451
# BB#440:                               # %if.then2322
	cmpq	$29, %rax
	ja	.LBB15_686
# BB#441:                               # %if.then2322
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_686
# BB#442:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_447:                             # %if.then2300
	cmpq	$116, %rax
	jne	.LBB15_449
# BB#448:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_563:                             # %if.then3040
	movq	%r12, %rbx
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB15_566
# BB#564:                               # %land.lhs.true3042
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_566
.LBB15_565:                             # %if.then3045
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rdi
	callq	negate_expr
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_587:                             # %if.then3105
	cmpq	$116, %rax
	jne	.LBB15_589
# BB#588:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_848:                             # %if.end.i10685
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#849:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_871:                             # %if.end.i10641
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#872:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_772:
	movq	%r12, %rbx
	movq	$89, %r12
	jmp	.LBB15_485
.LBB15_726:                             # %if.end.i10807
	movzbl	16(%rbx), %eax
	movq	%rbx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_730
# BB#727:                               # %if.end.i10807
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#728:                               # %if.end.i10807
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#729:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1032:                            # %if.then4311
	cmpq	$116, %rcx
	jne	.LBB15_1034
# BB#1033:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1039:                            # %if.end.i10497
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1043
# BB#1040:                              # %if.end.i10497
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#1041:                              # %if.end.i10497
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#1042:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1171:                            # %if.then4833
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
.LBB15_1173:                            # %if.end4845
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_1175
# BB#1174:                              # %lor.lhs.false4848
	movq	%r15, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_1184
.LBB15_1175:                            # %if.then4851
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1138
# BB#1176:                              # %if.end.i10368
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1180
# BB#1177:                              # %if.end.i10368
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1178:                              # %if.end.i10368
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1179:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1133:                            # %if.then4648
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
.LBB15_1135:                            # %if.end4660
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	jne	.LBB15_1137
# BB#1136:                              # %lor.lhs.false4663
	movq	%r15, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_1147
.LBB15_1137:                            # %if.then4666
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1138
# BB#1139:                              # %if.end.i10391
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1143
# BB#1140:                              # %if.end.i10391
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1141:                              # %if.end.i10391
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1142:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1165:                            # %if.then4999
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-97, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB15_1266
# BB#1166:                              # %if.then4999
	movq	%r8, %rdx
	movq	%r13, %r15
	jmpq	*.LJTI15_10(,%rcx,8)
.LBB15_1200:                            # %sw.bb5013
	movq	global_trees+96(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1191
# BB#1201:                              # %if.end.i10323
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1205
# BB#1202:                              # %if.end.i10323
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1203:                              # %if.end.i10323
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1204:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1232:                            # %if.else5071
	testq	%rdi, %rdi
	movq	%rbx, %r8
	jne	.LBB15_1266
.LBB15_1233:                            # %land.lhs.true5077
	movq	%r8, %rcx
	movq	%r13, %r8
	movq	32(%rcx), %rdi
	movq	%rcx, %r15
	decq	%rsi
	movq	$1, %rbx
	movq	%rsi, %rcx
	shlq	%cl, %rbx
	decq	%rbx
	andq	$8192, %rdx             # imm = 0x2000
	cmpq	%rbx, %rdi
	jne	.LBB15_1241
# BB#1234:                              # %land.lhs.true5077
	testq	%r14, %rdx
	je	.LBB15_1241
# BB#1235:                              # %land.lhs.true5096
	cmpq	$12, %rax
	ja	.LBB15_1241
# BB#1236:                              # %land.lhs.true5096
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	$7232, %rbx             # imm = 0x1C40
	je	.LBB15_1241
# BB#1237:                              # %if.then5132
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$99, %rax
	jne	.LBB15_1238
# BB#1240:                              # %sw.bb5148
	movq	%r8, %rbx
	movq	8(%rbx), %rax
	movq	%rax, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	8(%r15), %rcx
	movq	%rax, %rbx
	movq	%rcx, %rdi
	callq	signed_type
	movq	global_trees+88(%rip), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$97, %rdi
	jmp	.LBB15_635
.LBB15_1223:                            # %sw.bb5060
	movq	global_trees+96(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1191
# BB#1224:                              # %if.end.i10279
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1228
# BB#1225:                              # %if.end.i10279
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1226:                              # %if.end.i10279
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1227:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1219:                            # %if.end.i10301
	cmpq	$116, %rax
	jne	.LBB15_1221
# BB#1220:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_620:                             # %if.end3192
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_629
# BB#621:                               # %if.then3195
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_625
# BB#622:                               # %if.then3195
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#623:                               # %if.then3195
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#624:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_609:                             # %if.end.i10992
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_613
# BB#610:                               # %if.end.i10992
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#611:                               # %if.end.i10992
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#612:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_667:                             # %if.end3266
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	je	.LBB15_669
# BB#668:
	movq	$61, %r12
	jmp	.LBB15_550
.LBB15_663:                             # %if.then3263
	cmpq	$116, %rax
	jne	.LBB15_665
# BB#664:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1635:                            # %land.lhs.true6749
	cmpq	$8, %rdx
	jne	.LBB15_1639
# BB#1636:                              # %land.lhs.true6773
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB15_1639
# BB#1637:                              # %land.lhs.true6773
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rbx, %rcx
	je	.LBB15_1833
.LBB15_1639:                            # %land.lhs.true6789
	movq	40(%r15), %rcx
	movq	%rax, %rdi
	movq	%r12, %rsi
	movq	%rcx, %rdx
	callq	operand_equal_for_comparison_p
	testq	%rbx, %rax
	je	.LBB15_1833
# BB#1640:                              # %if.then6798
	movq	48(%r13), %rcx
	movzbl	16(%r15), %eax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	global_trees(%rip), %rax
.LBB15_1641:                            # %while.cond6808
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movzbl	16(%rcx), %ecx
	addq	$-114, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	ja	.LBB15_1644
# BB#1642:                              # %land.lhs.true6829
                                        #   in Loop: Header=BB15_1641 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	32(%rcx), %rcx
	cmpq	%rax, %rcx
	je	.LBB15_1644
# BB#1643:                              # %land.rhs6835
                                        #   in Loop: Header=BB15_1641 Depth=1
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	8(%rdx), %rdx
	movq	8(%rcx), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB15_1641
.LBB15_1644:                            # %while.end6861
	movq	40(%r15), %rax
	movq	8(%rax), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$7, %rdx
	je	.LBB15_1647
# BB#1645:                              # %while.end6861
	cmpq	$8, %rdx
	jne	.LBB15_1648
# BB#1646:                              # %land.lhs.true6885
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB15_1648
.LBB15_1647:                            # %cond.true6899
	movq	%rax, %rdi
	callq	real_zerop
	jmp	.LBB15_1649
.LBB15_1613:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1618:                            # %if.then.i.i
	cmpq	$116, %rax
	jne	.LBB15_1620
# BB#1619:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1077:                            # %if.end4358
	callq	integer_onep
	testq	%r14, %rax
	movq	120(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#1078:                              # %if.then4361
	movq	%r15, %rdi
	callq	invert_truthvalue
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_1082
# BB#1079:                              # %if.then4361
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1080:                              # %if.then4361
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1081:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1073:                            # %if.then4354
	cmpq	$116, %rax
	jne	.LBB15_1075
# BB#1074:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1066:                            # %if.then4348
	cmpq	$121, %rcx
	jne	.LBB15_617
# BB#1067:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_899:                             # %if.then4041
	cmpq	$121, %rcx
	jne	.LBB15_617
# BB#900:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1895:                            # %if.end.i9819
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1899
# BB#1896:                              # %if.end.i9819
	cmpq	$29, %rax
	ja	.LBB15_687
# BB#1897:                              # %if.end.i9819
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_687
# BB#1898:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1914:                            # %if.end.i
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1918
# BB#1915:                              # %if.end.i
	cmpq	$29, %rax
	ja	.LBB15_687
# BB#1916:                              # %if.end.i
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_687
# BB#1917:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_741:                             # %if.then3453
	movq	$115, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r9, %rdx
	jmp	.LBB15_293
.LBB15_753:                             # %if.end3522
	cmpq	$90, %rdx
	jne	.LBB15_754
# BB#755:                               # %if.end3522
	cmpq	$90, %rax
	jne	.LBB15_756
# BB#757:                               # %if.then3536
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rcx
	movq	32(%rbx), %rbx
	movq	$86, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	$90, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
.LBB15_714:                             # %return
	callq	build1
	jmp	.LBB15_715
.LBB15_745:
	movq	$88, %r12
	jmp	.LBB15_459
.LBB15_747:
	movq	$88, %r12
	jmp	.LBB15_459
.LBB15_749:
	movq	$88, %r12
	jmp	.LBB15_459
.LBB15_828:
	movq	%r15, 120(%rsp)         # 8-byte Spill
	movq	$85, %r12
	jmp	.LBB15_485
.LBB15_830:
	movq	$85, %r12
	jmp	.LBB15_485
.LBB15_832:
	movq	$85, %r12
	jmp	.LBB15_485
.LBB15_835:                             # %land.lhs.true3894
	cmpq	$0, 40(%rax)
	je	.LBB15_837
# BB#836:
	movq	$85, %r12
	jmp	.LBB15_485
.LBB15_1945:                            # %if.then7642
	movq	8(%r15), %rax
	movq	$56, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB15_1926
.LBB15_767:                             # %if.end.i10771
	cmpq	$116, %rax
	jne	.LBB15_769
# BB#768:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_909:                             # %if.then4064
	cmpq	$116, %rax
	jne	.LBB15_911
# BB#910:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_921:                             # %if.end.i10569
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#922:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1056:                            # %if.end.i10475
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1057:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_656:                             # %if.end.i10956
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#657:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_787:
	movq	$70, %r12
	jmp	.LBB15_485
.LBB15_790:                             # %land.lhs.true3658
	movzbl	16(%rbx), %eax
	cmpq	$70, %rax
	jne	.LBB15_791
# BB#792:                               # %if.then3665
	movq	%rbx, %r14
	movq	32(%r14), %rcx
	movq	$70, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	callq	build
	movq	40(%r14), %rcx
	movq	%rax, %rdx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build
	jmp	.LBB15_715
.LBB15_861:                             # %if.end.i10663
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#862:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_886:                             # %if.end.i10619
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#887:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_304:
	movabsq	$0, %r12
	xorq	%rcx, %rcx
.LBB15_308:                             # %neg_double.exit
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	build_int_2_wide
	movq	%rax, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rcx)
	movq	%rcx, %rdx
	movl	16(%rbx), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
	xorq	%rax, %rax
	testq	%r12, %r12
	jne	.LBB15_312
# BB#309:                               # %land.rhs1517
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB15_310
# BB#311:                               # %land.rhs1517
	movabsq	$1, %rax
	jmp	.LBB15_312
.LBB15_333:                             # %lor.lhs.false1643
	jne	.LBB15_334
# BB#337:                               # %land.lhs.true1652
	movq	32(%rax), %rcx
	movq	32(%rbx), %rax
	cmpq	%rax, %rcx
	jae	.LBB15_335
# BB#338:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_932:                             # %if.end.i10547
	cmpq	$116, %rax
	jne	.LBB15_934
# BB#933:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_951:                             # %lor.lhs.false4211
	movq	152(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$1, %rax
	jne	.LBB15_955
	jmp	.LBB15_952
.LBB15_949:                             # %land.lhs.true4197
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_952
# BB#950:                               # %if.then4200
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %r14d
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	136(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	build
	jmp	.LBB15_715
.LBB15_183:
	movabsq	$0, %rax
.LBB15_185:                             # %land.rhs1086
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB15_186:                             # %lor.end1096
	movl	16(%r12), %edi
	movl	60(%rsi), %ebx
	movq	%rbx, %rax
	andq	$511, %rax              # imm = 0x1FF
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %r10
	movq	%rdi, %rax
	andq	$255, %rax
	leaq	-6(%rax), %rcx
	andq	$1, %r10
	movq	%rcx, %rdx
	andq	%r14, %rdx
	cmpq	$7, %rdx
	jae	.LBB15_187
# BB#191:                               # %lor.end1134
	andq	$127, %rcx
	movq	$113, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	%rdi, %rcx
	andq	$253, %rcx
	cmpq	$13, %rcx
	jne	.LBB15_192
# BB#193:                               # %lor.end1134
	movabsq	$1, %rdx
	jmp	.LBB15_194
.LBB15_187:                             # %lor.rhs1157.thread
	movq	%rdi, %rax
	andq	$253, %rax
	cmpq	$13, %rax
	jne	.LBB15_188
# BB#189:                               # %lor.rhs1157.thread
	movabsq	$1, %rdx
	jmp	.LBB15_190
.LBB15_192:
	movabsq	$0, %rdx
.LBB15_194:                             # %lor.end1134
	cmpq	$7, %rax
	jne	.LBB15_196
# BB#195:
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%r11, 80(%rsp)          # 8-byte Spill
	jmp	.LBB15_201
.LBB15_188:
	movabsq	$0, %rdx
.LBB15_190:                             # %lor.rhs1157.thread
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%r11, 80(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB15_201
.LBB15_196:                             # %lor.rhs1157
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	%r11, 80(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	cmpq	$8, %rax
	jne	.LBB15_201
# BB#197:                               # %land.rhs1164
	movq	8(%r12), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_198
# BB#199:                               # %land.rhs1164
	movabsq	$1, %rax
	jmp	.LBB15_200
.LBB15_378:                             # %if.end1944
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %r9d
	movq	%r9, %rcx
	andq	$255, %rcx
	cmpq	$88, %rcx
	jne	.LBB15_384
# BB#379:                               # %land.lhs.true1951
	movzbl	16(%r12), %ecx
	cmpq	$88, %rcx
	jne	.LBB15_384
# BB#380:                               # %land.lhs.true1958
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$25, %rdx
	jne	.LBB15_384
# BB#381:                               # %land.lhs.true1968
	movq	40(%r12), %rdx
	movzbl	16(%rdx), %esi
	cmpq	$25, %rsi
	jne	.LBB15_384
# BB#382:                               # %land.lhs.true1978
	movq	$88, %rdi
	movq	%rcx, %rsi
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_383
.LBB15_675:                             # %bit_ior
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	je	.LBB15_688
# BB#676:                               # %if.then3287
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	168(%rsp), %rdx         # 8-byte Reload
	movzbl	17(%rdx), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_678
# BB#677:                               # %if.then.i10916
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_688:                             # %if.end3289
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	128(%rsp), %rax         # 8-byte Reload
	je	.LBB15_698
# BB#689:                               # %if.then3292
	movq	%rax, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movzbl	16(%rax), %ecx
	cmpq	$115, %rcx
	jg	.LBB15_693
# BB#690:                               # %if.then3292
	cmpq	$29, %rcx
	ja	.LBB15_697
# BB#691:                               # %if.then3292
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_697
# BB#692:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_678:                             # %if.end.i10920
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_682
# BB#679:                               # %if.end.i10920
	cmpq	$29, %rax
	ja	.LBB15_687
# BB#680:                               # %if.end.i10920
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_687
# BB#681:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_698:                             # %if.end3295
	movq	$86, %rdi
	movq	$86, %r15
	movq	%rax, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	movq	%rbx, %r12
	callq	distribute_bit_expr
	movq	%rax, %r9
	testq	%r9, %r9
	jne	.LBB15_1954
# BB#699:                               # %if.end3300
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$90, %rax
	jne	.LBB15_455
# BB#700:                               # %land.lhs.true3307
	movzbl	16(%r12), %eax
	cmpq	$90, %rax
	jne	.LBB15_455
# BB#701:                               # %if.then3314
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rcx
	movq	32(%r12), %rbx
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	$90, %rdi
	movq	%r14, %rsi
.LBB15_702:                             # %return
	movq	%rax, %rdx
	callq	build1
	jmp	.LBB15_1927
.LBB15_693:                             # %if.then3292
	cmpq	$116, %rcx
	jne	.LBB15_695
# BB#694:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_682:                             # %if.end.i10920
	cmpq	$116, %rax
	jne	.LBB15_684
# BB#683:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_374:                             # %if.then1941
	cmpq	$116, %rax
	jne	.LBB15_376
# BB#375:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_451:                             # %if.then2322
	cmpq	$116, %rax
	jne	.LBB15_453
# BB#452:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_449:                             # %if.then2300
	cmpq	$121, %rax
	jne	.LBB15_686
# BB#450:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_566:                             # %if.end3048
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB15_575
# BB#567:                               # %if.then3051
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	movzbl	16(%rcx), %eax
	movq	%rcx, %rsi
	cmpq	$115, %rax
	jg	.LBB15_571
# BB#568:                               # %if.then3051
	cmpq	$29, %rax
	ja	.LBB15_686
# BB#569:                               # %if.then3051
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_686
# BB#570:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_589:                             # %if.then3105
	cmpq	$121, %rax
	jne	.LBB15_686
# BB#590:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_730:                             # %if.end.i10807
	cmpq	$116, %rax
	jne	.LBB15_732
# BB#731:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1034:                            # %if.then4311
	cmpq	$121, %rcx
	jne	.LBB15_617
# BB#1035:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1043:                            # %if.end.i10497
	cmpq	$116, %rax
	jne	.LBB15_1045
# BB#1044:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1184:                            # %if.end4853
	movq	$1, %rsi
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	type_for_size
	movl	60(%rax), %r15d
	andq	$511, %r15              # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	8(%rbx), %rcx
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rcx, 8(%rax)
	xorq	%rsi, %rsi
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %r15
	andq	%r14, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	160(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	8(%rbx), %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
.LBB15_1185:                            # %if.end4876
	andq	%r14, %r12
	movq	%r13, %rdx
	je	.LBB15_1187
# BB#1186:                              # %if.end4876
	movq	%rbx, %rdx
.LBB15_1187:                            # %if.end4876
	cmpq	$1, %r12
	je	.LBB15_1189
# BB#1188:                              # %if.end4876
	movq	%rbx, %r13
.LBB15_1189:                            # %if.end4876
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_518
.LBB15_1180:                            # %if.end.i10368
	cmpq	$116, %rax
	jne	.LBB15_1182
# BB#1181:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1147:                            # %if.end4668
	movq	$1, %rsi
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	type_for_size
	movq	%rax, %r15
	movl	60(%r15), %eax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	andq	$511, 160(%rsp)         # 8-byte Folded Spill
                                        # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	%r15, 8(%rax)
	xorq	%rsi, %rsi
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	movq	168(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, 160(%rsp)         # 8-byte Folded Spill
	andq	%r14, 160(%rsp)         # 8-byte Folded Spill
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	152(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	8(%rbx), %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
.LBB15_1148:                            # %if.end4689
	andq	%r14, %r12
	movq	%r13, %rcx
	je	.LBB15_1150
# BB#1149:                              # %if.end4689
	movq	%rbx, %rcx
.LBB15_1150:                            # %if.end4689
	cmpq	$1, %r12
	je	.LBB15_1152
# BB#1151:                              # %if.end4689
	movq	%rbx, %r13
.LBB15_1152:                            # %if.end4689
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	jmp	.LBB15_519
.LBB15_1143:                            # %if.end.i10391
	cmpq	$116, %rax
	jne	.LBB15_1145
# BB#1144:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1190:                            # %sw.bb5004
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1191
# BB#1192:                              # %if.end.i10345
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1196
# BB#1193:                              # %if.end.i10345
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1194:                              # %if.end.i10345
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1195:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1205:                            # %if.end.i10323
	cmpq	$116, %rax
	jne	.LBB15_1207
# BB#1206:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1241:                            # %land.lhs.true5167
	andq	%r14, %rsi
	movq	$2, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	decq	%rax
	cmpq	%rax, %rdi
	movq	%r8, %r13
	movq	%r15, %r8
	jne	.LBB15_1266
# BB#1242:                              # %land.lhs.true5167
	testq	%r14, %rdx
	je	.LBB15_1266
# BB#1243:                              # %if.then5186
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-97, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB15_1266
# BB#1244:                              # %if.then5186
	movq	%r8, %rdx
	movq	%r13, %r15
	jmpq	*.LJTI15_8(,%rcx,8)
.LBB15_1264:                            # %sw.bb5203
	movabsq	$4294967040, %rcx       # imm = 0xFFFFFF00
	andq	%rcx, %rax
	orq	$102, %rax
	jmp	.LBB15_1265
.LBB15_1238:                            # %if.then5132
	cmpq	$98, %rax
	movq	%r8, %r13
	movq	%r15, %r8
	jne	.LBB15_1266
# BB#1239:                              # %sw.bb5137
	movq	8(%r13), %rax
	movq	%rax, %rdi
	movq	%r8, %rbx
	callq	signed_type
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	8(%rbx), %rcx
	movq	%rax, %rbx
	movq	%rcx, %rdi
	callq	signed_type
	movq	global_trees+88(%rip), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$100, %rdi
	jmp	.LBB15_635
.LBB15_1228:                            # %if.end.i10279
	cmpq	$116, %rax
	jne	.LBB15_1230
# BB#1229:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1221:                            # %if.end.i10301
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1222:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_629:                             # %if.end3198
	movzbl	16(%rbx), %eax
	cmpq	$82, %rax
	jne	.LBB15_632
# BB#630:                               # %land.lhs.true3205
	movq	32(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_632
# BB#631:                               # %if.then3211
	movq	40(%rbx), %rcx
	movq	$82, %rdi
.LBB15_781:                             # %return
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_782:                             # %return
	movq	168(%rsp), %rdx         # 8-byte Reload
	callq	build
	jmp	.LBB15_715
.LBB15_625:                             # %if.then3195
	cmpq	$116, %rax
	jne	.LBB15_627
# BB#626:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_613:                             # %if.end.i10992
	cmpq	$116, %rax
	jne	.LBB15_615
# BB#614:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_669:                             # %land.lhs.true3268
	movq	%rbx, %rdi
	callq	real_twop
	testq	%r14, %rax
	je	.LBB15_670
# BB#671:                               # %land.lhs.true3271
	callq	global_bindings_p
	testq	%r14, %rax
	je	.LBB15_673
# BB#672:
	movq	$61, %r12
	jmp	.LBB15_459
.LBB15_665:                             # %if.then3263
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#666:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1648:                            # %cond.false6905
	movq	%rax, %rdi
	callq	integer_zerop
.LBB15_1649:                            # %cond.false6905
	testq	%rbx, %rax
	je	.LBB15_1699
# BB#1650:                              # %land.lhs.true6911
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$77, %rax
	jne	.LBB15_1699
# BB#1651:                              # %land.lhs.true6918
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	je	.LBB15_1699
# BB#1652:                              # %if.then6924
	movq	160(%rsp), %rax         # 8-byte Reload
	addq	$-97, %rax
	andq	%rbx, %rax
	cmpq	$5, %rax
	ja	.LBB15_1698
# BB#1653:                              # %if.then6924
	jmpq	*.LJTI15_1(,%rax,8)
.LBB15_1686:                            # %sw.bb6959
	movq	8(%r12), %rax
	movzbl	17(%rax), %ecx
	testq	$32, %rcx
	je	.LBB15_1688
# BB#1687:                              # %if.then6968
	movq	%rax, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	8(%rcx), %rax
	movq	%rcx, %r12
.LBB15_1688:                            # %if.end6973
	movq	$80, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rdi
	callq	negate_expr
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1689
# BB#1690:                              # %if.then.i10026
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1694
# BB#1691:                              # %if.then.i10026
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1692:                              # %if.then.i10026
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1693:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1620:                            # %if.then.i.i
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#1621:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1082:                            # %if.then4361
	cmpq	$116, %rax
	jne	.LBB15_1084
# BB#1083:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1075:                            # %if.then4354
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1076:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1899:                            # %if.end.i9819
	cmpq	$116, %rax
	jne	.LBB15_1901
# BB#1900:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1918:                            # %if.end.i
	cmpq	$116, %rax
	jne	.LBB15_1920
# BB#1919:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_754:
	movq	$88, %r12
	jmp	.LBB15_459
.LBB15_756:
	movq	$88, %r12
	jmp	.LBB15_459
.LBB15_837:                             # %land.lhs.true3903
	movq	128(%rsp), %rcx         # 8-byte Reload
	movl	60(%rcx), %edx
	movq	$8, %rcx
	shrq	%cl, %rdx
	movq	32(%rbx), %rcx
	movq	32(%rax), %rax
	andq	$254, %rdx
	movzbl	mode_bitsize(%rdx), %edx
	addq	%rcx, %rax
	cmpq	%rdx, %rax
	movq	$85, %r12
	jne	.LBB15_485
# BB#838:                               # %if.then3925
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r9
	jmp	.LBB15_1954
.LBB15_769:                             # %if.end.i10771
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#770:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_911:                             # %if.then4064
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#912:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_791:
	movq	$70, %r12
	jmp	.LBB15_485
.LBB15_310:
	movabsq	$0, %rax
	jmp	.LBB15_312
.LBB15_334:                             # %lor.lhs.false1643.if.else1662_crit_edge
	movq	32(%rbx), %rax
.LBB15_335:                             # %if.else1662
	testq	%rax, %rax
	je	.LBB15_336
# BB#339:                               # %if.else.i11053
	negq	%rax
	notq	%r14
	movq	%r14, %rdx
	xorq	%r14, %r14
	jmp	.LBB15_340
.LBB15_934:                             # %if.end.i10547
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#935:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_952:                             # %land.lhs.true4218
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_955
# BB#953:                               # %if.then4221
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %r14d
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	160(%rsp), %rdx         # 8-byte Reload
	movq	152(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	jmp	.LBB15_954
.LBB15_198:
	movabsq	$0, %rax
.LBB15_200:                             # %land.rhs1164
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB15_201:                             # %lor.end1174
	movl	60(%r12), %r11d
	movq	128(%r15), %rax
	movq	128(%r12), %rcx
	movq	%r11, %rbx
	andq	$511, %rbx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jne	.LBB15_205
# BB#202:                               # %land.lhs.true1190
	movq	%rbx, %rax
	andq	%r14, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB15_205
# BB#203:                               # %land.lhs.true1190
	movq	%r9, %r15
	andq	%rsi, %r15
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rcx
	orq	%r15, %rcx
	xorq	$1, %rcx
	je	.LBB15_204
.LBB15_205:                             # %if.end1209
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	andq	$8192, %rdi             # imm = 0x2000
	movq	112(%rsp), %rdx         # 8-byte Reload
	orq	%r9, %rdx
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	128(%rsp), %rax         # 8-byte Reload
	orq	%rax, %rcx
	andq	%rdx, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	144(%rsp), %rdx         # 8-byte Reload
	andq	%rdx, %rax
	cmpq	%r8, %r10
	jne	.LBB15_206
# BB#207:                               # %if.end1209
	movabsq	$1, %r15
	jmp	.LBB15_208
.LBB15_383:                             # %land.lhs.true1978.if.end1989_crit_edge
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %r9d
.LBB15_384:                             # %if.end1989
	movl	16(%r12), %ecx
	andq	$255, %r9
	andq	$255, %rcx
	cmpq	$59, %r9
	jne	.LBB15_386
# BB#385:                               # %if.end1989
	cmpq	$61, %rcx
	je	.LBB15_388
.LBB15_386:                             # %lor.lhs.false2003
	cmpq	$59, %rcx
	jne	.LBB15_404
# BB#387:                               # %land.lhs.true2010
	movq	$59, %r15
	cmpq	$61, %r9
	jne	.LBB15_455
.LBB15_388:                             # %if.then2017
	cmpq	$59, %r9
	movq	%r12, %rbx
	je	.LBB15_390
# BB#389:                               # %if.then2017
	movq	168(%rsp), %rbx         # 8-byte Reload
.LBB15_390:                             # %if.then2017
	movq	168(%rsp), %rdx         # 8-byte Reload
	je	.LBB15_392
# BB#391:                               # %if.then2017
	movq	%r12, %rdx
.LBB15_392:                             # %if.then2017
	movq	32(%rdx), %r8
	movq	40(%rdx), %rdi
	movq	global_trees(%rip), %rdx
.LBB15_393:                             # %while.cond2033
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %r15
	movzbl	16(%r15), %esi
	leaq	-114(%rsi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_396
# BB#394:                               # %land.lhs.true2054
                                        #   in Loop: Header=BB15_393 Depth=1
	movq	32(%r15), %r8
	cmpq	%rdx, %r8
	je	.LBB15_396
# BB#395:                               # %land.rhs2060
                                        #   in Loop: Header=BB15_393 Depth=1
	movq	8(%r15), %rax
	movq	8(%r8), %r10
	movl	60(%rax), %r11d
	movl	60(%r10), %eax
	xorq	%r11, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_393
.LBB15_396:                             # %while.cond2087
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r13
	movzbl	16(%r13), %r8d
	leaq	-114(%r8), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_399
# BB#397:                               # %land.lhs.true2108
                                        #   in Loop: Header=BB15_396 Depth=1
	movq	32(%r13), %rdi
	cmpq	%rdx, %rdi
	je	.LBB15_399
# BB#398:                               # %land.rhs2114
                                        #   in Loop: Header=BB15_396 Depth=1
	movq	8(%r13), %rax
	movq	8(%rdi), %r10
	movl	60(%rax), %r11d
	movl	60(%r10), %eax
	xorq	%r11, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_396
.LBB15_399:                             # %while.end2140
	cmpq	$61, %rsi
	jne	.LBB15_402
# BB#400:                               # %land.lhs.true2147
	cmpq	$61, %r8
	je	.LBB15_404
# BB#401:                               # %if.then2154
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%r13, %rcx
	jmp	.LBB15_1926
.LBB15_695:                             # %if.then3292
	cmpq	$121, %rcx
	jne	.LBB15_697
# BB#696:
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_684:                             # %if.end.i10920
	cmpq	$121, %rax
	jne	.LBB15_687
# BB#685:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_376:                             # %if.then1941
	cmpq	$121, %rax
	jne	.LBB15_686
# BB#377:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_453:                             # %if.then2322
	cmpq	$121, %rax
	jne	.LBB15_686
# BB#454:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_575:                             # %if.end3054
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB15_591
# BB#576:                               # %land.lhs.true3061
	movzbl	16(%rbx), %eax
	cmpq	$61, %rax
	jne	.LBB15_591
# BB#577:                               # %land.lhs.true3068
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movq	40(%rbx), %rcx
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_591
# BB#578:                               # %if.then3077
	movq	168(%rsp), %r15         # 8-byte Reload
	movq	32(%r15), %rcx
	movq	32(%rbx), %rbx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	40(%r15), %rcx
	movq	%rax, %rdx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	jmp	.LBB15_1926
.LBB15_571:                             # %if.then3051
	cmpq	$116, %rax
	jne	.LBB15_573
# BB#572:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_732:                             # %if.end.i10807
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#733:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_617:                             # %if.end.i10606
	movq	%rax, %rbx
	jmp	.LBB15_618
.LBB15_1045:                            # %if.end.i10497
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#1046:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1182:                            # %if.end.i10368
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1183:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1145:                            # %if.end.i10391
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1146:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1196:                            # %if.end.i10345
	cmpq	$116, %rax
	jne	.LBB15_1198
# BB#1197:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1207:                            # %if.end.i10323
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1208:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1255:                            # %sw.bb5200
	movq	global_trees+96(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1191
# BB#1256:                              # %if.end.i10235
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1260
# BB#1257:                              # %if.end.i10235
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1258:                              # %if.end.i10235
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1259:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1245:                            # %sw.bb5191
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r15), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	je	.LBB15_1246
.LBB15_1191:                            # %if.then.i10341
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r8, %rcx
	jmp	.LBB15_520
.LBB15_1254:                            # %sw.bb5007
	movabsq	$4294967040, %rcx       # imm = 0xFFFFFF00
	andq	%rcx, %rax
	orq	$101, %rax
.LBB15_1265:                            # %if.end5215
	movq	120(%rsp), %rcx         # 8-byte Reload
	movl	%eax, 16(%rcx)
	movq	%r15, %r13
	movq	%rdx, %r8
.LBB15_1266:                            # %if.end5215
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	jne	.LBB15_1267
# BB#1268:                              # %land.lhs.true5222
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	movq	144(%rsp), %r12         # 8-byte Reload
	movq	120(%rsp), %r15         # 8-byte Reload
	jne	.LBB15_1270
# BB#1269:
	movq	%r15, 160(%rsp)         # 8-byte Spill
	jmp	.LBB15_1277
.LBB15_1267:
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	144(%rsp), %r12         # 8-byte Reload
.LBB15_1277:                            # %if.end5252
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	integer_zerop
	movq	%rbx, %r8
	testq	%r14, %rax
	je	.LBB15_1309
# BB#1278:                              # %land.lhs.true5255
	movq	8(%r8), %rax
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	ja	.LBB15_1309
# BB#1279:                              # %land.lhs.true5255
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$48192, %rdx            # imm = 0xBC40
	je	.LBB15_1309
# BB#1280:                              # %land.lhs.true5309
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB15_1309
# BB#1281:                              # %if.then5318
	movq	160(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-97, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	jbe	.LBB15_1282
.LBB15_1309:                            # %if.end5343
	cmpq	$101, %r12
	movq	%r12, 144(%rsp)         # 8-byte Spill
	movabsq	$0, %r12
	movabsq	$1, %rax
	movq	%r12, %rcx
	jne	.LBB15_1311
# BB#1310:                              # %if.end5343
	movq	%rax, %rcx
.LBB15_1311:                            # %if.end5343
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	cmpq	$102, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1313
# BB#1312:                              # %if.end5343
	movq	%rax, %r12
.LBB15_1313:                            # %if.end5343
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	-101(%rax), %rax
	andq	%r14, %rax
	xorq	%r15, %r15
	cmpq	$1, %rax
	movq	$1, %r14
	ja	.LBB15_1333
	jmp	.LBB15_1314
.LBB15_1270:                            # %land.lhs.true5229
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_1271
# BB#1272:                              # %if.then5233
	movzbl	16(%r15), %eax
	cmpq	$97, %rax
	jne	.LBB15_1273
# BB#1275:                              # %sw.bb5244
	movq	global_trees+96(%rip), %rdx
	movq	$60, %rdi
	movq	%rbx, %rsi
	callq	const_binop
	movq	32(%r15), %rdx
	movq	%rax, %r15
	movq	$98, %rdi
	xorq	%rax, %rax
	movq	$98, %r12
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rcx
	callq	build
	movq	%r15, %r8
	jmp	.LBB15_1276
.LBB15_1282:                            # %if.then5318
	jmpq	*.LJTI15_11(,%rcx,8)
.LBB15_1294:                            # %sw.bb5338
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r13), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	jne	.LBB15_1285
# BB#1295:                              # %if.end.i10191
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1299
# BB#1296:                              # %if.end.i10191
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#1297:                              # %if.end.i10191
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#1298:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1273:                            # %if.then5233
	cmpq	$100, %rax
	jne	.LBB15_1271
# BB#1274:                              # %sw.bb5238
	movq	global_trees+96(%rip), %rax
	movq	$60, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	32(%r15), %rcx
	movq	%rax, %rbx
	movq	$99, %rdi
	xorq	%rax, %rax
	movq	$99, %r12
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rbx, %r8
.LBB15_1276:                            # %if.end5252
	movq	%rax, 160(%rsp)         # 8-byte Spill
	jmp	.LBB15_1277
.LBB15_1271:
	movq	%r15, 160(%rsp)         # 8-byte Spill
	movq	%rbx, %r8
	jmp	.LBB15_1277
.LBB15_1230:                            # %if.end.i10279
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1231:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_632:                             # %if.end3217
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$82, %rax
	jne	.LBB15_639
# BB#633:                               # %land.lhs.true3224
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB15_639
# BB#634:                               # %if.then3230
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rcx
	movq	$82, %rdi
.LBB15_635:                             # %return
	xorq	%rax, %rax
.LBB15_636:                             # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_637
.LBB15_627:                             # %if.then3195
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#628:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_615:                             # %if.end.i10992
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#616:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_670:
	movq	$61, %r12
	jmp	.LBB15_460
.LBB15_673:                             # %land.lhs.true3275
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	contains_placeholder_p
	testq	%r14, %rax
	movq	$61, %r12
	jne	.LBB15_459
# BB#674:                               # %if.then3278
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	save_expr
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	jmp	.LBB15_520
.LBB15_1699:                            # %if.end6982
	movq	40(%r15), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB15_1723
# BB#1700:                              # %land.lhs.true6988
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB15_1723
# BB#1701:                              # %if.then6991
	cmpq	$101, 160(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1702
# BB#1713:                              # %if.then7000
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1714
# BB#1715:                              # %if.then.i9994
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1719
# BB#1716:                              # %if.then.i9994
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1717:                              # %if.then.i9994
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1718:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1698:                            # %sw.default6981
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fold, %rcx
	movq	$7202, %rsi             # imm = 0x1C22
	jmp	.LBB15_1099
.LBB15_1674:                            # %sw.bb6938
	movq	8(%r12), %rax
	movzbl	17(%rax), %ecx
	testq	$32, %rcx
	je	.LBB15_1676
# BB#1675:                              # %if.then6947
	movq	%rax, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	8(%rcx), %rax
	movq	%rcx, %r12
.LBB15_1676:                            # %if.end6952
	movq	$80, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1677
# BB#1678:                              # %if.then.i10042
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1682
# BB#1679:                              # %if.then.i10042
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1680:                              # %if.then.i10042
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1681:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1654:                            # %sw.bb6925
	movq	40(%r13), %rax
	movq	8(%rax), %rax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rdi
	callq	negate_expr
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1655
# BB#1656:                              # %if.then.i10074
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1660
# BB#1657:                              # %if.then.i10074
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1658:                              # %if.then.i10074
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1659:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1664:                            # %sw.bb6935
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1665
# BB#1666:                              # %if.then.i10058
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1670
# BB#1667:                              # %if.then.i10058
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1668:                              # %if.then.i10058
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1669:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1689:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1694:                            # %if.then.i10026
	cmpq	$116, %rax
	jne	.LBB15_1696
# BB#1695:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1084:                            # %if.then4361
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1085:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1901:                            # %if.end.i9819
	cmpq	$121, %rax
	jne	.LBB15_687
# BB#1902:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1920:                            # %if.end.i
	cmpq	$121, %rax
	jne	.LBB15_687
# BB#1921:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_336:                             # %if.then.i11050
	movq	%r14, %rdx
	negq	%rdx
	andq	%rdx, %r14
	movq	$63, %rcx
	shrq	%cl, %r14
	xorq	%rax, %rax
.LBB15_340:                             # %neg_double.exit11055
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	build_int_2_wide
	movq	%rax, %r12
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%r12)
	movl	16(%rbx), %r15d
	movq	$19, %rcx
	shrq	%cl, %r15
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	force_fit_type
	movq	%r12, %r9
	orq	%r15, %rax
	jmp	.LBB15_313
.LBB15_955:                             # %if.end4234
	movq	120(%rsp), %r13         # 8-byte Reload
	movl	16(%r13), %eax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	32(%r13), %rax
	andq	$253, 160(%rsp)         # 8-byte Folded Spill
	leaq	540(%rsp), %rcx
	leaq	568(%rsp), %rdx
	leaq	544(%rsp), %r8
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rcx
	callq	make_range
	movq	40(%r13), %rcx
	movq	%rax, 152(%rsp)         # 8-byte Spill
	leaq	536(%rsp), %rax
	leaq	560(%rsp), %rdx
	leaq	512(%rsp), %r8
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r8, %rcx
	callq	make_range
	movq	%rax, 136(%rsp)         # 8-byte Spill
	cmpq	$92, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_961
# BB#956:                               # %if.then.i11107
	movl	540(%rsp), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rdx
	jne	.LBB15_958
# BB#957:                               # %if.then.i11107
	movq	%rcx, %rdx
.LBB15_958:                             # %if.then.i11107
	movl	536(%rsp), %esi
	movl	%edx, 540(%rsp)
	testq	%rsi, %rsi
	jne	.LBB15_960
# BB#959:                               # %if.then.i11107
	movq	%rcx, %rax
.LBB15_960:                             # %if.then.i11107
	movl	%eax, 536(%rsp)
.LBB15_961:                             # %if.end.i11110
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	je	.LBB15_964
# BB#962:                               # %if.end.i11110
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB15_964
# BB#963:                               # %lor.lhs.false17.i
	xorq	%rdx, %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_971
.LBB15_964:                             # %land.lhs.true.i
	movq	568(%rsp), %rax
	movq	544(%rsp), %r10
	movq	560(%rsp), %rcx
	movq	512(%rsp), %rdx
	movl	540(%rsp), %r8d
	movl	536(%rsp), %esi
	movq	%rdx, 16(%rsp)
	movq	%rcx, 8(%rsp)
	leaq	532(%rsp), %rcx
	leaq	552(%rsp), %rdx
	leaq	504(%rsp), %r9
	movq	%rsi, (%rsp)
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	movq	%rax, %r8
	movq	%r10, %r9
	callq	merge_ranges
	testq	%r14, %rax
	je	.LBB15_971
# BB#965:                               # %land.lhs.true22.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	jne	.LBB15_968
# BB#966:                               # %cond.false.i
	movq	136(%rsp), %rcx         # 8-byte Reload
	testq	%rcx, %rcx
	jne	.LBB15_968
# BB#967:                               # %cond.false.i
	movq	global_trees+88(%rip), %rcx
.LBB15_968:                             # %cond.end28.i
	movq	552(%rsp), %r9
	movq	504(%rsp), %r8
	movl	532(%rsp), %edx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r9, %rcx
	callq	build_range_check
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB15_971
# BB#969:                               # %if.then32.i
	cmpq	$92, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1010
# BB#970:                               # %cond.true34.i
	movq	%r13, %rdi
	callq	invert_truthvalue
	jmp	.LBB15_1009
.LBB15_206:
	movabsq	$0, %r15
.LBB15_208:                             # %if.end1209
	orq	%rax, %rcx
	je	.LBB15_216
# BB#209:                               # %if.end1209
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	%r14, %rdx
	andq	%rdx, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
                                        # kill: R14<def> RDX<kill>
	cmpq	%rax, %rcx
	jb	.LBB15_216
# BB#210:                               # %if.end1209
	movq	88(%rsp), %rax          # 8-byte Reload
	orq	%rax, %r15
	je	.LBB15_216
# BB#211:                               # %land.lhs.true1229
	movq	$9, %rcx
	movq	%r11, %rax
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_bitsize(%rax,%rax), %ecx
	movq	%rbx, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB15_212
# BB#225:                               # %land.lhs.true1253
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB15_204
	jmp	.LBB15_216
.LBB15_402:                             # %land.lhs.true2166
	cmpq	$61, %r8
	jne	.LBB15_404
# BB#403:                               # %if.then2173
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
.LBB15_1948:                            # %return
	movq	%rcx, %rdx
	movq	%r15, %rcx
	jmp	.LBB15_1926
.LBB15_697:                             # %if.end.i10907
	movq	%rax, %rbx
	jmp	.LBB15_687
.LBB15_591:                             # %if.end3110
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_592
# BB#596:                               # %lor.lhs.false3133
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB15_598
# BB#597:
	movq	$60, %r12
	jmp	.LBB15_459
.LBB15_573:                             # %if.then3051
	cmpq	$121, %rax
	jne	.LBB15_686
# BB#574:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1198:                            # %if.end.i10345
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1199:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1260:                            # %if.end.i10235
	cmpq	$116, %rax
	jne	.LBB15_1262
# BB#1261:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1246:                            # %if.end.i10257
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1250
# BB#1247:                              # %if.end.i10257
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1248:                              # %if.end.i10257
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1249:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1283:                            # %sw.bb5329
	movabsq	$4294967040, %rcx       # imm = 0xFFFFFF00
	andq	%rcx, %rax
	orq	$101, %rax
	movq	$101, 144(%rsp)         # 8-byte Folded Spill
	jmp	.LBB15_1304
.LBB15_1303:                            # %sw.bb5323
	movabsq	$4294967040, %rcx       # imm = 0xFFFFFF00
	andq	%rcx, %rax
	orq	$102, %rax
	movq	$102, 144(%rsp)         # 8-byte Folded Spill
.LBB15_1304:                            # %if.end5343.thread
	movq	160(%rsp), %rcx         # 8-byte Reload
	movl	%eax, 16(%rcx)
	movq	144(%rsp), %rcx         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$101, %rcx
	movabsq	$0, %r12
	movabsq	$1, %rax
	movq	%r12, 168(%rsp)         # 8-byte Spill
	jne	.LBB15_1306
# BB#1305:                              # %if.end5343.thread
	movq	%rax, 168(%rsp)         # 8-byte Spill
.LBB15_1306:                            # %if.end5343.thread
	cmpq	$102, %rcx
	jne	.LBB15_1308
# BB#1307:                              # %if.end5343.thread
	movq	%rax, %r12
.LBB15_1308:                            # %if.end5343.thread
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	-101(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	sbbq	%r14, %r14
	andq	$1, %r14
.LBB15_1314:                            # %land.lhs.true5349
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	jne	.LBB15_1325
# BB#1315:                              # %land.lhs.true5356
	movzbl	16(%r13), %ecx
	leaq	-59(%rcx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$1, %rax
	ja	.LBB15_1325
# BB#1316:                              # %land.lhs.true5370
	movq	40(%r13), %rax
	movzbl	16(%rax), %edx
	cmpq	$25, %rdx
	jne	.LBB15_1325
# BB#1317:                              # %land.lhs.true5380
	movq	%r8, %rdx
	cmpq	$59, %rcx
	jne	.LBB15_1318
# BB#1319:                              # %land.lhs.true5380
	movabsq	$1, %rcx
	jmp	.LBB15_1320
.LBB15_1284:                            # %sw.bb5335
	movq	global_trees+96(%rip), %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%r13), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB15_1286
.LBB15_1285:                            # %if.then.i10209
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r13, %rdx
	movq	%rbx, %rcx
	jmp	.LBB15_520
.LBB15_1299:                            # %if.end.i10191
	cmpq	$116, %rax
	jne	.LBB15_1301
# BB#1300:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_639:                             # %if.end3236
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_640
# BB#641:                               # %land.lhs.true3243
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	$61, %rdx
	xorq	%rcx, %rcx
	movq	$61, %r12
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	extract_muldiv
	testq	%rax, %rax
	jne	.LBB15_642
	jmp	.LBB15_459
.LBB15_1702:                            # %if.then6991
	cmpq	$102, 160(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1723
# BB#1703:                              # %if.then6994
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1704
# BB#1705:                              # %if.then.i10010
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1709
# BB#1706:                              # %if.then.i10010
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1707:                              # %if.then.i10010
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1708:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1714:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1719:                            # %if.then.i9994
	cmpq	$116, %rax
	jne	.LBB15_1721
# BB#1720:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1677:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1682:                            # %if.then.i10042
	cmpq	$116, %rax
	jne	.LBB15_1684
# BB#1683:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1655:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1660:                            # %if.then.i10074
	cmpq	$116, %rax
	jne	.LBB15_1662
# BB#1661:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1665:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1670:                            # %if.then.i10058
	cmpq	$116, %rax
	jne	.LBB15_1672
# BB#1671:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1696:                            # %if.then.i10026
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1697:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_971:                             # %if.else.i11113
	movslq	ix86_branch_cost(%rip), %rax
	xorq	%r13, %r13
	cmpq	$2, %rax
	jl	.LBB15_1010
# BB#972:                               # %if.else.i11113
	cmpq	$0, 152(%rsp)           # 8-byte Folded Reload
	je	.LBB15_1010
# BB#973:                               # %if.else.i11113
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB15_1010
# BB#974:                               # %land.lhs.true44.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	addq	$-91, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	ja	.LBB15_1010
# BB#975:                               # %land.lhs.true56.i
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_1010
# BB#976:
	movq	152(%rsp), %rdx         # 8-byte Reload
.LBB15_977:                             # %while.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	movl	16(%rcx), %eax
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$114, %rdx
	jne	.LBB15_979
# BB#978:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB15_977 Depth=1
	movq	32(%rcx), %rdx
	movq	8(%rcx), %rsi
	movq	8(%rdx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB15_977
.LBB15_979:                             # %while.end.i.i
	movq	%rax, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %edx
	cmpq	$99, %rdx
	je	.LBB15_988
# BB#980:                               # %lor.rhs.i.i
	cmpq	$100, %rdx
	jne	.LBB15_992
# BB#981:                               # %lor.rhs.i.i
	movq	%rax, %rdx
	andq	$1024, %rdx             # imm = 0x400
	testq	%r14, %rdx
	jne	.LBB15_992
# BB#982:                               # %lor.rhs.i.i
	movq	%rax, %rdx
	andq	$2048, %rdx             # imm = 0x800
	testq	%r14, %rdx
	jne	.LBB15_992
# BB#983:                               # %land.lhs.true48.i.i
	movq	48(%rcx), %rcx
	testq	$512, %rcx              # imm = 0x200
	jne	.LBB15_992
# BB#984:                               # %land.lhs.true48.i.i
	movq	%rax, %rdx
	andq	$524288, %rdx           # imm = 0x80000
	testq	%r14, %rdx
	jne	.LBB15_992
# BB#985:                               # %land.lhs.true48.i.i
	movq	%rcx, %rdx
	andq	$256, %rdx              # imm = 0x100
	jne	.LBB15_992
# BB#986:                               # %land.rhs66.i.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB15_988
# BB#987:                               # %land.rhs66.i.i
	andq	$1024, %rcx             # imm = 0x400
	jne	.LBB15_988
.LBB15_992:                             # %if.else78.i
	callq	global_bindings_p
	testq	%r14, %rax
	jne	.LBB15_1010
# BB#993:                               # %land.lhs.true81.i
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	contains_placeholder_p
	testq	%r14, %rax
	jne	.LBB15_1010
# BB#994:                               # %if.then84.i
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	save_expr
	movq	120(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	movl	540(%rsp), %edx
	movq	%rax, 136(%rsp)         # 8-byte Spill
	cmpq	$92, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_998
# BB#995:                               # %cond.true90.i
	testq	%r14, %rdx
	jne	.LBB15_996
# BB#997:                               # %cond.true90.i
	movabsq	$1, %rdx
	jmp	.LBB15_998
.LBB15_212:                             # %land.lhs.true1240
	movq	$9, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shrq	%cl, %rdx
	andq	$127, %rdx
	andq	%r14, %rax
	cmpq	%rdx, %rax
	jne	.LBB15_213
# BB#214:                               # %land.lhs.true1240
	movabsq	$1, %rax
	jmp	.LBB15_215
.LBB15_404:                             # %if.end2179
	cmpq	$61, %r9
	movq	$59, %r15
	jne	.LBB15_455
# BB#405:                               # %if.end2179
	cmpq	$61, %rcx
	jne	.LBB15_455
# BB#406:                               # %if.then2193
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rcx
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	movq	40(%rax), %r13
	movq	%r12, %rax
	movq	32(%rax), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	40(%rax), %rbx
                                        # kill: R12<def> RAX<kill>
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_408
# BB#407:
	movq	%r13, 136(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rbx         # 8-byte Reload
	jmp	.LBB15_431
.LBB15_592:                             # %if.end3110
	cmpq	$8, %rax
	jne	.LBB15_598
# BB#593:                               # %land.lhs.true3124
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_598
# BB#594:                               # %land.lhs.true3124
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%r14, %rax
	jne	.LBB15_598
# BB#595:
	movq	$60, %r12
	jmp	.LBB15_459
.LBB15_686:                             # %if.end.i11095
	movq	%rsi, %rbx
.LBB15_687:                             # %if.end.i.i10929
	movq	8(%rbx), %rax
	movq	$116, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	build1
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1262:                            # %if.end.i10235
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1263:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1250:                            # %if.end.i10257
	cmpq	$116, %rax
	jne	.LBB15_1252
# BB#1251:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1318:
	movabsq	$0, %rcx
.LBB15_1320:                            # %land.lhs.true5380
	addq	$59, %rcx
	movq	%rcx, %rdi
	movq	%rdx, %rbx
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rbx, %r8
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_1325
# BB#1321:                              # %land.lhs.true5394
	movzbl	18(%rcx), %eax
	testq	$4, %rax
	je	.LBB15_1322
.LBB15_1325:                            # %if.else5407
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB15_1333
# BB#1326:                              # %land.lhs.true5413
	movzbl	16(%r13), %eax
	cmpq	$77, %rax
	jne	.LBB15_1330
# BB#1327:                              # %land.lhs.true5420
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	jne	.LBB15_1330
# BB#1328:                              # %land.lhs.true5427
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	negate_expr
	movq	%rbx, %r8
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_1330
# BB#1329:                              # %land.lhs.true5431
	movl	16(%rcx), %eax
	andq	$262399, %rax           # imm = 0x400FF
	cmpq	$25, %rax
	jne	.LBB15_1330
.LBB15_1322:                            # %if.then5401
	movq	32(%r13), %rdx
	jmp	.LBB15_1323
.LBB15_1330:                            # %land.lhs.true5457
	movq	%r8, %rdi
	movq	%r8, %rbx
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %r15
	je	.LBB15_1332
# BB#1331:                              # %land.lhs.true5460
	movzbl	16(%r13), %eax
	cmpq	$60, %rax
	jne	.LBB15_1332
# BB#1955:                              # %if.then5467
	movq	%r13, %rax
	movq	32(%rax), %rcx
	movq	40(%rax), %rbx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
.LBB15_799:                             # %return
	movq	%rbx, %rcx
	callq	build
	jmp	.LBB15_715
.LBB15_1286:                            # %if.end.i10213
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jg	.LBB15_1290
# BB#1287:                              # %if.end.i10213
	cmpq	$29, %rax
	ja	.LBB15_618
# BB#1288:                              # %if.end.i10213
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_618
# BB#1289:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1301:                            # %if.end.i10191
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#1302:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_640:
	movq	$61, %r12
	jmp	.LBB15_459
.LBB15_1723:                            # %if.end7005
	movq	%r15, %rdx
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
                                        # kill: R15<def> RDX<kill>
	movq	%rcx, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	operand_equal_for_comparison_p
	testq	%rbx, %rax
	je	.LBB15_1777
# BB#1724:                              # %if.then7014
	movq	%r15, %rcx
	movq	32(%rcx), %rax
	movq	8(%rax), %rsi
	movq	128(%rsi), %rdx
	movq	128(%rsp), %r8          # 8-byte Reload
	movq	128(%r8), %rdi
	movq	40(%rcx), %rcx
	cmpq	%rdi, %rdx
	movq	%r8, %rdx
	movq	%r8, %r14
	je	.LBB15_1726
# BB#1725:                              # %select.mid
	movq	%rsi, %rdx
.LBB15_1726:                            # %select.end
	movq	160(%rsp), %rsi         # 8-byte Reload
	leaq	-97(%rsi), %rsi
	andq	%rbx, %rsi
	cmpq	$5, %rsi
	ja	.LBB15_1776
# BB#1727:                              # %select.end
	jmpq	*.LJTI15_2(,%rsi,8)
.LBB15_1748:                            # %sw.bb7040
	cmpq	$98, 160(%rsp)          # 8-byte Folded Reload
	movq	%rax, %rbx
	je	.LBB15_1750
# BB#1749:                              # %sw.bb7040
	movq	%rcx, %rbx
.LBB15_1750:                            # %sw.bb7040
	je	.LBB15_1752
# BB#1751:                              # %sw.bb7040
	movq	%rax, %rcx
.LBB15_1752:                            # %sw.bb7040
	movq	$78, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1753
# BB#1754:                              # %if.then.i9946
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1758
# BB#1755:                              # %if.then.i9946
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1756:                              # %if.then.i9946
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1757:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1704:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1709:                            # %if.then.i10010
	cmpq	$116, %rax
	jne	.LBB15_1711
# BB#1710:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1721:                            # %if.then.i9994
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1722:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1684:                            # %if.then.i10042
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1685:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1662:                            # %if.then.i10074
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1663:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1672:                            # %if.then.i10058
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1673:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_988:                             # %if.then62.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$91, %rax
	jne	.LBB15_989
# BB#990:                               # %if.then62.i
	movabsq	$1, %rcx
	jmp	.LBB15_991
.LBB15_996:
	movabsq	$0, %rdx
.LBB15_998:                             # %cond.end95.i
	movq	568(%rsp), %rax
	movq	544(%rsp), %r8
	movq	%rcx, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rcx
	callq	build_range_check
	movq	%rax, 152(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB15_1010
# BB#999:                               # %land.lhs.true99.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movl	536(%rsp), %ecx
	cmpq	$92, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1003
# BB#1000:                              # %cond.true103.i
	testq	%r14, %rcx
	jne	.LBB15_1001
# BB#1002:                              # %cond.true103.i
	movabsq	$1, %rcx
	jmp	.LBB15_1003
.LBB15_213:
	movabsq	$0, %rax
.LBB15_215:                             # %land.lhs.true1240
	movq	56(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	je	.LBB15_204
.LBB15_216:                             # %if.end1263
	movq	80(%rsp), %rax          # 8-byte Reload
	andq	%r9, %rax
	andq	%rax, %rsi
	movq	%rax, %r15
	cmpq	$1, %rsi
	jne	.LBB15_221
# BB#217:                               # %if.end1263
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%r14, %rdx
	andq	%rdx, %rax
	movq	136(%rsp), %rcx         # 8-byte Reload
	andq	%rdx, %rcx
                                        # kill: R14<def> RDX<kill>
	cmpq	%rax, %rcx
	jae	.LBB15_221
# BB#218:                               # %land.lhs.true1272
	testq	%r8, %r8
	je	.LBB15_221
# BB#219:                               # %land.lhs.true1272
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jae	.LBB15_221
# BB#220:                               # %land.lhs.true1272
	testq	%r10, %r10
	je	.LBB15_204
.LBB15_221:                             # %if.end1287
	movq	%r14, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, %rdx
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	andq	%rcx, %rax
	cmpq	%rdx, %rax
	movq	%rax, %rsi
	sbbq	%rax, %rax
	andq	%rcx, %rbx
                                        # kill: R14<def> RCX<kill>
	cmpq	%rdx, %rbx
	sbbq	%rcx, %rcx
	orq	%rax, %rcx
	andq	$1, %rcx
	cmpq	$1, %rcx
	movq	160(%rsp), %r9          # 8-byte Reload
	jne	.LBB15_244
# BB#222:                               # %if.end1287
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	xorq	$1, %rax
	je	.LBB15_244
# BB#223:                               # %land.lhs.true1299
	movq	%r9, %rcx
	cmpq	%r8, %r10
	je	.LBB15_224
# BB#226:                               # %land.lhs.true1299
	movabsq	$1, %rax
	jmp	.LBB15_227
.LBB15_408:                             # %if.else2209
	xorq	%rdx, %rdx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_410
# BB#409:
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%r13, 144(%rsp)         # 8-byte Spill
	jmp	.LBB15_431
.LBB15_598:                             # %land.lhs.true3135
	xorq	%rdx, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	movq	$60, %r12
	je	.LBB15_459
# BB#599:                               # %if.then3138
	movq	global_trees+88(%rip), %rax
.LBB15_642:                             # %if.then3250
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	jmp	.LBB15_554
.LBB15_1252:                            # %if.end.i10257
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1253:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1332:
	movq	%rbx, %r8
.LBB15_1333:                            # %if.else5476
	movq	%r13, %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$6, %rax
	jne	.LBB15_1344
# BB#1334:                              # %land.lhs.true5485
	movzbl	16(%rbx), %eax
	cmpq	$115, %rax
	jne	.LBB15_1344
# BB#1335:                              # %land.lhs.true5492
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r8, %r14
	callq	get_unwidened
	movq	%r14, %r8
	movq	%rax, %r14
	cmpq	%rbx, %r14
	je	.LBB15_1344
# BB#1336:                              # %land.lhs.true5496
	movq	8(%r14), %rsi
	movq	%r8, %rdi
	movq	%r8, %r13
	callq	get_unwidened
	movq	%r13, %r8
	testq	%rax, %rax
	movq	%rax, %r13
	je	.LBB15_1344
# BB#1337:                              # %land.lhs.true5502
	movq	%r8, %rsi
	movq	%rbx, %rdx
	movq	8(%r13), %rcx
	movq	8(%r14), %rax
	cmpq	%rax, %rcx
	je	.LBB15_1341
# BB#1338:                              # %lor.lhs.false5509
	movzbl	16(%r13), %ecx
	cmpq	$25, %rcx
	movq	%rdx, %rbx
	movq	%rsi, %r8
	jne	.LBB15_1344
# BB#1339:                              # %land.lhs.true5516
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r8, 136(%rsp)          # 8-byte Spill
	callq	int_fits_type_p
	movq	136(%rsp), %r8          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1344
# BB#1340:                              # %land.lhs.true5516.if.then5521_crit_edge
	movq	8(%r14), %rcx
.LBB15_1341:                            # %if.then5521
	movq	%rcx, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
.LBB15_1342:                            # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_1343:                            # %return
	movq	%r14, %rdx
	callq	build
	jmp	.LBB15_715
.LBB15_1344:                            # %if.else5527
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	jne	.LBB15_1348
# BB#1345:                              # %land.lhs.true5534
	movl	16(%rbx), %ecx
	andq	$254, %rcx
	cmpq	$78, %rcx
	jne	.LBB15_1348
# BB#1346:                              # %land.lhs.true5548
	movq	40(%rbx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_1348
# BB#1347:                              # %if.then5558
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	optimize_minmax_comparison
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1348:                            # %if.else5560
	cmpq	$25, %rax
	je	.LBB15_1349
# BB#1350:                              # %if.else5560
	movabsq	$1, %rax
	jmp	.LBB15_1351
.LBB15_1349:
	movabsq	$0, %rax
.LBB15_1351:                            # %if.else5560
	cmpq	$98, 144(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1357
# BB#1352:                              # %if.else5560
	testq	%rax, %rax
	jne	.LBB15_1357
# BB#1353:                              # %land.lhs.true5570
	movl	16(%rbx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$80, %rax
	jne	.LBB15_1357
# BB#1354:                              # %land.lhs.true5584
	movq	%r8, %rdi
	movq	%r8, %r14
	callq	negate_expr
	movq	%r14, %r8
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_1357
# BB#1355:                              # %land.lhs.true5588
	movl	16(%rcx), %eax
	andq	$262399, %rax           # imm = 0x400FF
	cmpq	$25, %rax
	jne	.LBB15_1357
# BB#1356:                              # %if.then5602
	movq	32(%rbx), %rdx
	movq	$100, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r8, %r15
	callq	build
	movq	32(%rbx), %rcx
	movq	%rax, %rbx
	movq	$98, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rcx
	movq	$91, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
.LBB15_637:                             # %return
	movq	%rbx, %rdx
	callq	build
	jmp	.LBB15_715
.LBB15_1357:                            # %if.end5618
	movq	%r8, %rdi
	movq	%r8, %r13
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1367
# BB#1358:                              # %if.end5618
	movq	%r15, %rax
	xorq	$1, %rax
	jne	.LBB15_1367
# BB#1359:                              # %land.lhs.true5627
	movzbl	16(%rbx), %eax
	cmpq	$88, %rax
	jne	.LBB15_1367
# BB#1360:                              # %if.then5634
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$82, %rcx
	jne	.LBB15_1364
# BB#1361:                              # %land.lhs.true5644
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1364
# BB#1362:                              # %if.then5653
	movq	%rbx, %r14
	movq	32(%r14), %rax
	movq	8(%r14), %rbx
	movq	40(%r14), %rcx
	jmp	.LBB15_1363
.LBB15_1290:                            # %if.end.i10213
	cmpq	$116, %rax
	jne	.LBB15_1292
# BB#1291:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1777:                            # %if.end7075
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$12, %rax
	ja	.LBB15_1833
# BB#1778:                              # %if.end7075
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB15_1833
# BB#1779:                              # %land.lhs.true7103
	movq	40(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_1833
# BB#1780:                              # %land.lhs.true7113
	movq	168(%rsp), %rcx         # 8-byte Reload
	movzbl	16(%rcx), %ecx
	cmpq	$25, %rcx
	jne	.LBB15_1833
# BB#1781:                              # %if.then7120
	movq	160(%rsp), %rcx         # 8-byte Reload
	addq	$-97, %rcx
	andq	%rbx, %rcx
	cmpq	$5, %rcx
	ja	.LBB15_1831
# BB#1782:                              # %if.then7120
	jmpq	*.LJTI15_3(,%rcx,8)
.LBB15_1783:                            # %sw.bb7133
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	112(%rax), %rax
	movq	$1, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	jne	.LBB15_1833
# BB#1784:                              # %land.lhs.true7138
	movq	40(%r15), %r14
	movq	global_trees+96(%rip), %rax
	movq	$59, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	je	.LBB15_1833
# BB#1785:                              # %if.then7145
	movq	$78, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	168(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1786
# BB#1787:                              # %if.then.i9914
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1791
# BB#1788:                              # %if.then.i9914
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1789:                              # %if.then.i9914
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1790:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1776:                            # %sw.default7074
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fold, %rcx
	movq	$7259, %rsi             # imm = 0x1C5B
	jmp	.LBB15_1099
.LBB15_1762:                            # %sw.bb7057
	cmpq	$100, 160(%rsp)         # 8-byte Folded Reload
	movq	%rax, %rbx
	je	.LBB15_1764
# BB#1763:                              # %sw.bb7057
	movq	%rcx, %rbx
.LBB15_1764:                            # %sw.bb7057
	je	.LBB15_1766
# BB#1765:                              # %sw.bb7057
	movq	%rax, %rcx
.LBB15_1766:                            # %sw.bb7057
	movq	$79, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1767
# BB#1768:                              # %if.then.i9930
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1772
# BB#1769:                              # %if.then.i9930
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1770:                              # %if.then.i9930
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1771:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1728:                            # %sw.bb7034
	movq	%r14, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1729
# BB#1730:                              # %if.then.i9978
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1734
# BB#1731:                              # %if.then.i9978
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1732:                              # %if.then.i9978
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1733:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1738:                            # %sw.bb7037
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1739
# BB#1740:                              # %if.then.i9962
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1744
# BB#1741:                              # %if.then.i9962
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1742:                              # %if.then.i9962
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1743:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1753:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1758:                            # %if.then.i9946
	cmpq	$116, %rax
	jne	.LBB15_1760
# BB#1759:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1711:                            # %if.then.i10010
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1712:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_989:
	movabsq	$0, %rcx
.LBB15_991:                             # %if.then62.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rdx
	movq	32(%rax), %r9
	movq	40(%rax), %r8
	xorq	$1, %rcx
	addq	$93, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	movq	%r9, %rdx
	movq	%r8, %rcx
	jmp	.LBB15_1008
.LBB15_1001:
	movabsq	$0, %rcx
.LBB15_1003:                            # %cond.end108.i
	movq	560(%rsp), %r8
	movq	512(%rsp), %r9
	movq	%rax, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r8, %rcx
	movq	%r9, %r8
	callq	build_range_check
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_1010
# BB#1004:                              # %if.then112.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$91, %rax
	jne	.LBB15_1005
# BB#1006:                              # %if.then112.i
	movabsq	$1, %rdx
	jmp	.LBB15_1007
.LBB15_224:
	movabsq	$0, %rax
.LBB15_227:                             # %land.lhs.true1299
	movq	%r15, %rdx
	andq	%rax, %rdx
	cmpq	$1, %rdx
	movq	%rcx, %r9
	jne	.LBB15_229
# BB#228:                               # %land.lhs.true1299
	cmpq	%rbx, 96(%rsp)          # 8-byte Folded Reload
	jb	.LBB15_244
.LBB15_229:                             # %land.lhs.true1309
	movq	%r9, %rdx
	cmpq	%rbx, 96(%rsp)          # 8-byte Folded Reload
	sbbq	%rax, %rax
	testq	%r14, %rdi
	je	.LBB15_230
# BB#231:                               # %land.lhs.true1309
	movabsq	$1, %rcx
	jmp	.LBB15_232
.LBB15_410:                             # %if.else2213
	xorq	%rdx, %rdx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_412
# BB#411:
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%r13, 144(%rsp)         # 8-byte Spill
	jmp	.LBB15_431
.LBB15_1364:                            # %if.else5677
	movq	40(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$82, %rcx
	jne	.LBB15_1367
# BB#1365:                              # %land.lhs.true5687
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1367
# BB#1366:                              # %if.then5696
	movq	%rbx, %r14
	movq	40(%r14), %rax
	movq	8(%r14), %rbx
	movq	32(%r14), %rcx
.LBB15_1363:                            # %return
	movq	8(%rax), %rdx
	movq	40(%rax), %r8
	movq	$83, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	build
	movq	8(%r14), %rcx
	movq	global_trees+96(%rip), %rdx
	movq	%rax, %r14
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_954:                             # %return
	movq	%rcx, %rdx
	movq	%r13, %rcx
	callq	build
	jmp	.LBB15_715
.LBB15_1292:                            # %if.end.i10213
	cmpq	$121, %rax
	jne	.LBB15_618
# BB#1293:
	movq	%rbx, %r9
	jmp	.LBB15_1954
.LBB15_1831:                            # %sw.default7201
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fold, %rcx
	movq	$7323, %rsi             # imm = 0x1C9B
	jmp	.LBB15_1099
.LBB15_1795:                            # %sw.bb7150
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	104(%rax), %rax
	movq	$1, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	jne	.LBB15_1833
# BB#1796:                              # %land.lhs.true7155
	movq	40(%r15), %r14
	movq	global_trees+96(%rip), %rax
	movq	$60, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	je	.LBB15_1833
# BB#1797:                              # %if.then7162
	movq	$78, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	168(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1798
# BB#1799:                              # %if.then.i9898
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1803
# BB#1800:                              # %if.then.i9898
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1801:                              # %if.then.i9898
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1802:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1807:                            # %sw.bb7167
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	104(%rax), %rax
	movq	$1, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	jne	.LBB15_1833
# BB#1808:                              # %land.lhs.true7172
	movq	40(%r15), %r14
	movq	global_trees+96(%rip), %rax
	movq	$60, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	je	.LBB15_1833
# BB#1809:                              # %if.then7179
	movq	$79, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	168(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1810
# BB#1811:                              # %if.then.i9882
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1815
# BB#1812:                              # %if.then.i9882
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1813:                              # %if.then.i9882
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1814:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1819:                            # %sw.bb7184
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	112(%rax), %rax
	movq	$1, %rdx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	jne	.LBB15_1833
# BB#1820:                              # %land.lhs.true7189
	movq	40(%r15), %r14
	movq	global_trees+96(%rip), %rax
	movq	$59, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	je	.LBB15_1833
# BB#1821:                              # %if.then7196
	movq	$79, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	168(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1822
# BB#1823:                              # %if.then.i9866
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1827
# BB#1824:                              # %if.then.i9866
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1825:                              # %if.then.i9866
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1826:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1832:                            # %sw.bb7121
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r13, %rdx
	movq	32(%rdx), %rcx
	movq	48(%rdx), %r8
	movq	%rax, %r9
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%r9, %rcx
	movq	%r9, %r12
	callq	build
	movq	%rax, %r13
.LBB15_1833:                            # %if.end7204
	movl	16(%r12), %eax
	testq	$512, %rax              # imm = 0x200
	jne	.LBB15_1836
# BB#1834:                              # %lor.lhs.false7211
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$100, %rcx
	je	.LBB15_1836
# BB#1835:                              # %lor.lhs.false7211
	cmpq	$118, %rax
	jne	.LBB15_1845
.LBB15_1836:                            # %land.lhs.true7228
	movq	48(%r13), %rax
	movl	16(%rax), %eax
	testq	$512, %rax              # imm = 0x200
	jne	.LBB15_1845
# BB#1837:                              # %lor.lhs.false7238
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$100, %rcx
	je	.LBB15_1845
# BB#1838:                              # %lor.lhs.false7238
	cmpq	$118, %rax
	je	.LBB15_1845
# BB#1839:                              # %if.then7261
	movq	%r15, %rdi
	callq	invert_truthvalue
	movq	%rax, %r14
	movzbl	16(%r14), %eax
	cmpq	$96, %rax
	je	.LBB15_1845
# BB#1840:                              # %if.then7269
	movq	%r13, %rax
	movq	40(%rax), %r8
	movq	48(%rax), %rcx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	callq	build
	movq	40(%rax), %rcx
	movq	%rax, %r13
	movq	global_trees(%rip), %rax
.LBB15_1841:                            # %while.cond7280
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movzbl	16(%rdx), %ecx
	movq	%rdx, %r12
	addq	$-114, %rcx
	andq	%rbx, %rcx
	cmpq	$2, %rcx
	ja	.LBB15_1844
# BB#1842:                              # %land.lhs.true7301
                                        #   in Loop: Header=BB15_1841 Depth=1
	movq	32(%r12), %rcx
	cmpq	%rax, %rcx
	je	.LBB15_1844
# BB#1843:                              # %land.rhs7307
                                        #   in Loop: Header=BB15_1841 Depth=1
	movq	8(%r12), %rdx
	movq	8(%rcx), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB15_1841
.LBB15_1844:
	movq	%r14, %r15
.LBB15_1845:                            # %if.end7335
	movq	40(%r13), %rax
	movq	%rax, %rdi
	callq	integer_onep
	testq	%rbx, %rax
	je	.LBB15_1859
# BB#1846:                              # %land.lhs.true7341
	movq	48(%r13), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB15_1859
# BB#1847:                              # %land.lhs.true7347
	movq	8(%r15), %rax
	cmpq	%rax, 128(%rsp)         # 8-byte Folded Reload
	je	.LBB15_1848
.LBB15_1859:                            # %if.end7354
	movq	48(%r13), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB15_1860
# BB#1861:                              # %land.lhs.true7360
	movzbl	16(%r15), %eax
	cmpq	$102, %rax
	jne	.LBB15_1862
# BB#1863:                              # %land.lhs.true7367
	movq	40(%r15), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB15_1864
# BB#1865:                              # %land.lhs.true7373
	movq	%r12, %rdi
	callq	integer_pow2p
	testq	%rbx, %rax
	je	.LBB15_1866
# BB#1867:                              # %land.lhs.true7376
	movq	32(%r15), %rax
	movzbl	16(%rax), %ecx
	cmpq	$88, %rcx
	jne	.LBB15_1868
# BB#1869:                              # %land.lhs.true7386
	movq	40(%rax), %rax
	movq	$1, %rdx
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	movq	%r13, %r9
	je	.LBB15_1954
# BB#1870:                              # %if.then7395
	movq	32(%r15), %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movl	pedantic_lvalues(%rip), %ecx
	movq	%rax, %rsi
	testq	%rcx, %rcx
	je	.LBB15_1871
# BB#1872:                              # %if.then.i9835
	movzbl	16(%rsi), %eax
	cmpq	$115, %rax
	jg	.LBB15_1876
# BB#1873:                              # %if.then.i9835
	cmpq	$29, %rax
	ja	.LBB15_1880
# BB#1874:                              # %if.then.i9835
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1880
# BB#1875:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1786:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1791:                            # %if.then.i9914
	cmpq	$116, %rax
	jne	.LBB15_1793
# BB#1792:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1767:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1772:                            # %if.then.i9930
	cmpq	$116, %rax
	jne	.LBB15_1774
# BB#1773:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1729:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1734:                            # %if.then.i9978
	cmpq	$116, %rax
	jne	.LBB15_1736
# BB#1735:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1739:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1744:                            # %if.then.i9962
	cmpq	$116, %rax
	jne	.LBB15_1746
# BB#1745:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1760:                            # %if.then.i9946
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1761:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1005:
	movabsq	$0, %rdx
.LBB15_1007:                            # %if.then112.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rsi
	xorq	$1, %rdx
	addq	$93, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	152(%rsp), %rdx         # 8-byte Reload
.LBB15_1008:                            # %fold_range_test.exit
	callq	build
.LBB15_1009:                            # %fold_range_test.exit
	movq	%rax, %r13
.LBB15_1010:                            # %fold_range_test.exit
	movq	%r13, %r9
	testq	%r9, %r9
	jne	.LBB15_1954
# BB#1011:                              # %if.end4239
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	%r12, %rax
	jne	.LBB15_1014
# BB#1012:                              # %land.lhs.true4246
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	fold_truthop
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB15_1014
# BB#1013:                              # %if.then4253
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	build
	jmp	.LBB15_715
.LBB15_230:
	movabsq	$0, %rcx
.LBB15_232:                             # %land.lhs.true1309
	andq	%rcx, %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rsi
	sbbq	%rcx, %rcx
	andq	%r10, %rcx
	cmpq	%rax, %rcx
	movq	%rdx, %r9
	jne	.LBB15_244
# BB#233:                               # %land.lhs.true1323
	cmpq	%rbx, 96(%rsp)          # 8-byte Folded Reload
	je	.LBB15_235
# BB#234:                               # %land.lhs.true1323
	xorq	$1, 128(%rsp)           # 8-byte Folded Spill
	je	.LBB15_244
.LBB15_235:                             # %land.lhs.true1328
	movq	96(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %rsi
	je	.LBB15_237
# BB#236:                               # %land.lhs.true1328
	movq	56(%rsp), %rax          # 8-byte Reload
	xorq	$1, %rax
	je	.LBB15_244
.LBB15_237:                             # %land.lhs.true1333
	movq	%r9, %rdx
	movq	$9, %rcx
	movq	%r11, %rsi
	shrq	%cl, %rsi
	andq	$127, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %eax
	cmpq	%rax, %rbx
	jne	.LBB15_238
# BB#242:                               # %land.lhs.true1357
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	jmp	.LBB15_243
.LBB15_412:                             # %if.else2217
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	operand_equal_p
	testq	%r14, %rax
	jne	.LBB15_430
# BB#413:                               # %if.else2221
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB15_455
# BB#414:                               # %land.lhs.true2228
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_455
# BB#415:                               # %land.lhs.true2235
	cmpq	$0, 40(%r13)
	jne	.LBB15_455
# BB#416:                               # %land.lhs.true2241
	cmpq	$0, 40(%rbx)
	jne	.LBB15_455
# BB#417:                               # %if.then2247
	movq	32(%r13), %rax
	movq	$63, %rcx
	movq	%rax, %rdx
	movq	32(%rbx), %r8
	movq	%r8, 136(%rsp)          # 8-byte Spill
	sarq	%cl, %rdx
	leaq	(%rax,%rdx), %rsi
	movq	$63, %rcx
	movq	%r8, %rdi
	sarq	%cl, %rdi
	leaq	(%r8,%rdi), %rcx
	xorq	%rdx, %rsi
	xorq	%rdi, %rcx
	cmpq	%rcx, %rsi
	movq	%rax, 112(%rsp)         # 8-byte Spill
	jl	.LBB15_419
# BB#418:                               # %if.then2247
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 112(%rsp)         # 8-byte Spill
.LBB15_419:                             # %if.then2247
	jl	.LBB15_421
# BB#420:                               # %if.then2247
	movq	%rax, 136(%rsp)         # 8-byte Spill
.LBB15_421:                             # %if.then2247
	jl	.LBB15_423
# BB#422:                               # %if.then2247
	movq	%rbx, %r13
.LBB15_423:                             # %if.then2247
	movq	144(%rsp), %rbx         # 8-byte Reload
	jl	.LBB15_425
# BB#424:                               # %if.then2247
	movq	160(%rsp), %rbx         # 8-byte Reload
.LBB15_425:                             # %if.then2247
	jl	.LBB15_427
# BB#426:                               # %if.then2247
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rax, 160(%rsp)         # 8-byte Spill
.LBB15_427:                             # %if.then2247
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	exact_log2_wide
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB15_455
# BB#428:                               # %land.lhs.true2274
	movq	136(%rsp), %rax         # 8-byte Reload
	cqto
	idivq	112(%rsp)               # 8-byte Folded Reload
	movq	%rdx, %rax
	testq	%rax, %rax
	jne	.LBB15_455
# BB#429:                               # %if.then2277
	movq	136(%rsp), %rax         # 8-byte Reload
	cqto
	idivq	112(%rsp)               # 8-byte Folded Reload
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	160(%rsp), %rdx         # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, 144(%rsp)         # 8-byte Spill
.LBB15_430:                             # %if.end2286
	movq	%r13, 136(%rsp)         # 8-byte Spill
.LBB15_431:                             # %if.end2286
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB15_455
# BB#432:                               # %if.then2288
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	144(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	136(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB15_1926
.LBB15_1367:                            # %if.end5722
	testq	%r15, %r15
	je	.LBB15_1373
# BB#1368:                              # %land.lhs.true5728
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1373
# BB#1369:                              # %land.lhs.true5731
	movq	8(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB15_1373
# BB#1370:                              # %land.lhs.true5740
	movzbl	16(%rbx), %eax
	addq	$-66, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB15_1373
# BB#1371:                              # %land.lhs.true5768
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_pow2p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1373
# BB#1372:                              # %if.then5774
	movq	%rbx, %r15
	movq	8(%r15), %rax
	movq	%rax, %rdi
	callq	unsigned_type
	movq	32(%r15), %rcx
	movzbl	16(%r15), %ebx
	movq	%rax, %r14
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	40(%r15), %rcx
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	jmp	.LBB15_1383
.LBB15_1798:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1803:                            # %if.then.i9898
	cmpq	$116, %rax
	jne	.LBB15_1805
# BB#1804:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1810:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1815:                            # %if.then.i9882
	cmpq	$116, %rax
	jne	.LBB15_1817
# BB#1816:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1822:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1827:                            # %if.then.i9866
	cmpq	$116, %rax
	jne	.LBB15_1829
# BB#1828:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1848:                            # %if.then7352
	movl	pedantic_lvalues(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_1849
# BB#1850:                              # %if.then.i9850
	movzbl	16(%r15), %eax
	cmpq	$115, %rax
	jg	.LBB15_1854
# BB#1851:                              # %if.then.i9850
	cmpq	$29, %rax
	ja	.LBB15_1858
# BB#1852:                              # %if.then.i9850
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1858
# BB#1853:
	movq	%r15, %r9
	jmp	.LBB15_1954
.LBB15_1860:
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_1862:
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_1864:
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_1866:
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_1868:
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_1871:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1876:                            # %if.then.i9835
	cmpq	$116, %rax
	jne	.LBB15_1878
# BB#1877:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1793:                            # %if.then.i9914
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1794:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1774:                            # %if.then.i9930
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1775:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1736:                            # %if.then.i9978
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1737:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1746:                            # %if.then.i9962
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1747:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1014:                            # %if.end4259
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	fold_truthop
	testq	%rax, %rax
	je	.LBB15_1953
# BB#1015:                              # %if.end4259
	movq	%rax, 120(%rsp)         # 8-byte Spill
.LBB15_1953:                            # %if.end7710
	movq	120(%rsp), %r9          # 8-byte Reload
.LBB15_1954:                            # %return
	movq	%r9, %rax
	addq	$576, %rsp              # imm = 0x240
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_238:                             # %land.lhs.true1344
	movq	$9, %rcx
	movq	72(%rsp), %rdi          # 8-byte Reload
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	%rsi, %rax
	andq	%r14, %rax
	cmpq	%rdi, %rax
	jne	.LBB15_239
# BB#240:                               # %land.lhs.true1344
	movabsq	$1, %rax
	jmp	.LBB15_241
.LBB15_455:                             # %bit_rotate
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %ecx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movzbl	16(%r12), %eax
	cmpq	$83, %rcx
	jne	.LBB15_457
# BB#456:                               # %bit_rotate
	cmpq	$82, %rax
	je	.LBB15_488
.LBB15_457:                             # %lor.lhs.false2344
	cmpq	$83, %rax
	jne	.LBB15_458
# BB#487:                               # %lor.lhs.false2344
	cmpq	$82, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_458
.LBB15_488:                             # %land.lhs.true2350
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	32(%r12), %rcx
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	je	.LBB15_458
# BB#489:                               # %land.lhs.true2359
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	movq	8(%rdx), %r9
	movzbl	17(%r9), %ecx
	testq	$32, %rcx
	je	.LBB15_458
# BB#490:                               # %if.then2371
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %r8
	movq	40(%r12), %rdi
	movq	global_trees(%rip), %rcx
.LBB15_491:                             # %while.cond2382
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %r13
	movzbl	16(%r13), %esi
	leaq	-114(%rsi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_494
# BB#492:                               # %land.lhs.true2403
                                        #   in Loop: Header=BB15_491 Depth=1
	movq	32(%r13), %r8
	cmpq	%rcx, %r8
	je	.LBB15_494
# BB#493:                               # %land.rhs2409
                                        #   in Loop: Header=BB15_491 Depth=1
	movq	8(%r13), %rax
	movq	8(%r8), %r10
	movl	60(%rax), %r11d
	movl	60(%r10), %eax
	xorq	%r11, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_491
.LBB15_494:                             # %while.cond2436
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movzbl	16(%rax), %edi
	leaq	-114(%rdi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_510
# BB#495:                               # %land.lhs.true2457
                                        #   in Loop: Header=BB15_494 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdi
	cmpq	%rcx, %rdi
	je	.LBB15_497
# BB#496:                               # %land.rhs2463
                                        #   in Loop: Header=BB15_494 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%rdi), %r8
	movl	60(%rax), %r10d
	movl	60(%r8), %eax
	xorq	%r10, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_494
	jmp	.LBB15_497
.LBB15_1373:                            # %if.end5795
	testq	%r12, %r12
	je	.LBB15_1378
# BB#1374:                              # %land.lhs.true5798
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1378
# BB#1375:                              # %land.lhs.true5801
	movzbl	16(%rbx), %eax
	cmpq	$88, %rax
	jne	.LBB15_1378
# BB#1376:                              # %land.lhs.true5808
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1378
# BB#1377:                              # %if.then5814
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	jmp	.LBB15_554
.LBB15_1805:                            # %if.then.i9898
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1806:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1817:                            # %if.then.i9882
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1818:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1829:                            # %if.then.i9866
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1830:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_1849:
	movq	%r15, %r9
	jmp	.LBB15_1954
.LBB15_1854:                            # %if.then.i9850
	cmpq	$116, %rax
	jne	.LBB15_1856
# BB#1855:
	movq	%r15, %r9
	jmp	.LBB15_1954
.LBB15_1878:                            # %if.then.i9835
	cmpq	$121, %rax
	jne	.LBB15_1880
# BB#1879:
	movq	%rsi, %r9
	jmp	.LBB15_1954
.LBB15_239:
	movabsq	$0, %rax
.LBB15_241:                             # %land.lhs.true1344
	movq	56(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
.LBB15_243:                             # %land.lhs.true1357
	movq	%rdx, %r9
	je	.LBB15_204
.LBB15_244:                             # %if.end1368
	andq	$255, %r13
	cmpq	$48, %r13
	jne	.LBB15_249
# BB#245:                               # %land.lhs.true1378
	movq	40(%r9), %rax
	movzbl	17(%rax), %ecx
	testq	$2, %rcx
	je	.LBB15_249
# BB#246:                               # %land.lhs.true1390
	movq	32(%r9), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$39, %rdx
	jne	.LBB15_248
# BB#247:                               # %land.lhs.true1403
	movq	40(%rcx), %rcx
	movzbl	49(%rcx), %ecx
	testq	$16, %rcx
	je	.LBB15_248
.LBB15_249:                             # %if.end1435
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	je	.LBB15_250
# BB#251:                               # %if.end1448
	movzbl	16(%r12), %ecx
	cmpq	$15, %rcx
	movq	120(%rsp), %r13         # 8-byte Reload
	ja	.LBB15_280
# BB#252:                               # %if.end1448
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$48192, %rax            # imm = 0xBC40
	je	.LBB15_253
# BB#255:                               # %if.then.i11035
	movq	%r14, %rbx
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$26, %rdx
	jne	.LBB15_256
# BB#269:                               # %if.then136.i
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	48(%rdx), %rax
	movq	8(%rdx), %r14
	movq	%rax, 496(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rdx, %r13
	movq	%rcx, 488(%rsp)
	movq	%rax, 480(%rsp)
	movq	104(%r12), %rax
	leaq	456(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	real_value_from_int_cst
	movq	112(%r12), %r15
	testq	%r15, %r15
	je	.LBB15_271
# BB#270:                               # %if.then145.i
	leaq	408(%rsp), %rax
	movq	%rax, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	real_value_from_int_cst
	movq	424(%rsp), %rax
	movq	408(%rsp), %rcx
	movq	416(%rsp), %rdx
	movq	%rax, 448(%rsp)
	movq	%rdx, 440(%rsp)
	movq	%rcx, 432(%rsp)
	movabsq	$dconst1, %r14
	leaq	456(%rsp), %rax
	movq	$60, %rsi
	movq	%rax, %rdi
	movq	%rax, %rdx
	movq	%r14, %rcx
	callq	earith
	leaq	432(%rsp), %rax
	movq	$59, %rsi
	movq	%rax, %rdi
	movq	%rax, %rdx
	movq	%r14, %rcx
	jmp	.LBB15_272
.LBB15_250:                             # %if.then1437
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	120(%rsp), %r13         # 8-byte Reload
	movl	16(%r13), %ecx
	andq	$512, %rax              # imm = 0x200
	movabsq	$4294966783, %rdx       # imm = 0xFFFFFDFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r13)
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_248:                             # %if.then1418
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rax, 32(%rdi)
	movq	%r9, %rbx
	callq	fold
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %r9
	movl	16(%r9), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r9)
	jmp	.LBB15_1954
.LBB15_510:                             # %while.end2489
	cmpq	$25, %rsi
	jne	.LBB15_521
# BB#511:                               # %while.end2489
	cmpq	$25, %rdi
	jne	.LBB15_521
# BB#512:                               # %land.lhs.true2503
	cmpq	$0, 40(%r13)
	jne	.LBB15_521
# BB#513:                               # %land.lhs.true2509
	movq	144(%rsp), %rax         # 8-byte Reload
	cmpq	$0, 40(%rax)
	jne	.LBB15_521
# BB#514:                               # %land.lhs.true2515
	movq	32(%r13), %rax
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	32(%rbx), %r8
	movl	60(%r9), %r10d
	addq	%rax, %r8
	andq	$511, %r10              # imm = 0x1FF
	cmpq	%r10, %r8
	jne	.LBB15_521
# BB#515:                               # %if.then2535
	cmpq	$82, 160(%rsp)          # 8-byte Folded Reload
	je	.LBB15_517
# BB#516:                               # %if.then2535
	movq	144(%rsp), %r13         # 8-byte Reload
.LBB15_517:                             # %if.then2535
	movq	$84, %rdi
	xorq	%rax, %rax
	jmp	.LBB15_518
.LBB15_1378:                            # %if.end5816
	testq	%r15, %r15
	je	.LBB15_1384
# BB#1379:                              # %land.lhs.true5822
	movzbl	16(%rbx), %eax
	cmpq	$88, %rax
	jne	.LBB15_1384
# BB#1380:                              # %land.lhs.true5829
	movq	40(%rbx), %rax
	movq	%rax, %rdi
	callq	integer_pow2p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1384
# BB#1381:                              # %land.lhs.true5835
	movq	40(%rbx), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1384
# BB#1382:                              # %if.then5841
	movq	global_trees+88(%rip), %rcx
	movq	168(%rsp), %rdx         # 8-byte Reload
	addq	$101, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
.LBB15_1383:                            # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB15_520
.LBB15_1856:                            # %if.then.i9850
	cmpq	$121, %rax
	jne	.LBB15_1858
# BB#1857:
	movq	%r15, %r9
	jmp	.LBB15_1954
.LBB15_1880:                            # %if.end.i10979
	movq	%rsi, %rbx
	jmp	.LBB15_618
.LBB15_204:                             # %if.then1201
	movq	%r12, %rdi
	movq	104(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB15_554
.LBB15_253:                             # %if.end1448
	movq	%r13, %rdx
	movq	%r14, %rbx
	cmpq	$7, %rcx
	jne	.LBB15_254
# BB#281:                               # %if.then219.i
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$26, %rax
	jne	.LBB15_282
# BB#284:                               # %if.then235.i
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	48(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rdx, %r14
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%rbx, %rax
	je	.LBB15_286
# BB#285:                               # %if.then241.i
	movq	%r14, %rax
	movq	%r12, 8(%rax)
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_256:                             # %if.then.i11035
	cmpq	$25, %rdx
	jne	.LBB15_279
# BB#257:                               # %if.then37.i
	movl	60(%r12), %edx
	movq	%rdx, %rsi
	andq	$511, %rsi              # imm = 0x1FF
	cmpq	$128, %rsi
	movq	%r13, %r9
	ja	.LBB15_1954
# BB#258:                               # %if.end.i11036
	testq	$131072, %rdx           # imm = 0x20000
	je	.LBB15_263
# BB#259:                               # %if.end.i11036
	cmpq	$6, %rcx
	jne	.LBB15_263
# BB#260:                               # %if.end.i11036
	andq	$262144, %rax           # imm = 0x40000
	testq	%rbx, %rax
	jne	.LBB15_263
# BB#261:                               # %land.lhs.true57.i
	movq	$10000, %rsi            # imm = 0x2710
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB15_262
.LBB15_263:                             # %if.end62.i
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	32(%r14), %rax
	movq	40(%r14), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	build_int_2_wide
	movq	%r12, 8(%rax)
	movq	%rax, %r15
	xorq	%rax, %rax
	cmpq	$0, 40(%r14)
	jns	.LBB15_265
# BB#264:                               # %land.rhs.i
	movq	8(%r14), %rax
	movl	16(%r12), %edx
	movq	$13, %rcx
	movl	16(%rax), %eax
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	%rax, %rdx
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB15_265:                             # %land.end.i
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	force_fit_type
	testq	%rbx, %rax
	je	.LBB15_267
# BB#266:                               # %land.lhs.true89.i
	movq	8(%r14), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	movq	$1, %rax
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB15_268
.LBB15_267:                             # %lor.rhs.i
	movzbl	18(%r14), %eax
	andq	$8, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
.LBB15_268:                             # %lor.end.i
	movq	%r15, %rdi
	movl	16(%rdi), %edx
	movq	$19, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	andq	$-524289, %rdx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rsi, %rdx
	movl	%edx, 16(%rdi)
	movl	16(%r14), %esi
	movq	$18, %rcx
	shlq	%cl, %rax
	andq	$262144, %rsi           # imm = 0x40000
	orq	%rax, %rsi
	movabsq	$4294705151, %rax       # imm = 0xFFFBFFFF
	andq	%rdx, %rax
	orq	%rsi, %rax
	movl	%eax, 16(%rdi)
	movq	%rdi, %r13
	jmp	.LBB15_279
.LBB15_271:                             # %if.end152.critedge.i
	movabsq	$dconst1, %rax
	leaq	456(%rsp), %rcx
	movq	$60, %rsi
	movq	%rcx, %rdi
	movq	%rcx, %rdx
	movq	%rax, %rcx
.LBB15_272:                             # %if.end152.i
	callq	earith
	movq	496(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	480(%rsp), %rax
	movq	488(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%rbx, %rax
	je	.LBB15_274
# BB#273:                               # %if.then155.i
	movq	dconst0+16(%rip), %rax
	movq	dconst0+8(%rip), %rcx
	movq	dconst0(%rip), %rdx
	movq	%rax, 496(%rsp)
	movq	%rcx, 488(%rsp)
	movq	%rdx, 480(%rsp)
	jmp	.LBB15_277
.LBB15_521:                             # %if.else2546
	cmpq	$60, %rdi
	jne	.LBB15_497
# BB#522:                               # %if.then2549
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %r8
	movq	40(%rax), %rdi
.LBB15_523:                             # %while.cond2558
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %rdx
	movzbl	16(%rdx), %esi
	leaq	-114(%rsi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_526
# BB#524:                               # %land.lhs.true2579
                                        #   in Loop: Header=BB15_523 Depth=1
	movq	32(%rdx), %r8
	cmpq	%rcx, %r8
	je	.LBB15_526
# BB#525:                               # %land.rhs2585
                                        #   in Loop: Header=BB15_523 Depth=1
	movq	8(%rdx), %rax
	movq	8(%r8), %r10
	movl	60(%rax), %r11d
	movl	60(%r10), %eax
	xorq	%r11, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_523
.LBB15_526:                             # %while.cond2612
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	addq	$-114, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_529
# BB#527:                               # %land.lhs.true2633
                                        #   in Loop: Header=BB15_526 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB15_529
# BB#528:                               # %land.rhs2639
                                        #   in Loop: Header=BB15_526 Depth=1
	movq	8(%rbx), %rax
	movq	8(%rdi), %r8
	movl	60(%rax), %r10d
	movl	60(%r8), %eax
	xorq	%r10, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_526
.LBB15_529:                             # %while.end2665
	cmpq	$25, %rsi
	jne	.LBB15_458
# BB#530:                               # %land.lhs.true2672
	movl	60(%r9), %eax
	andq	$511, %rax              # imm = 0x1FF
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	testq	%r14, %rax
	jne	.LBB15_458
# BB#531:                               # %land.lhs.true2686
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	movq	%r12, %rbx
	movq	%r15, %r12
	je	.LBB15_459
# BB#532:                               # %if.then2689
	cmpq	$82, 160(%rsp)          # 8-byte Folded Reload
	je	.LBB15_533
# BB#534:                               # %if.then2689
	movabsq	$1, %rcx
	jmp	.LBB15_535
.LBB15_1384:                            # %if.end5846
	cmpq	$97, 144(%rsp)          # 8-byte Folded Reload
	je	.LBB15_1386
# BB#1385:                              # %if.end5846
	cmpq	$100, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1404
.LBB15_1386:                            # %land.lhs.true5852
	movq	8(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB15_1394
# BB#1387:                              # %land.lhs.true5861
	movzbl	16(%r13), %eax
	cmpq	$82, %rax
	jne	.LBB15_1394
# BB#1388:                              # %land.lhs.true5868
	movq	32(%r13), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1394
# BB#1389:                              # %if.then5874
	cmpq	$97, 144(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1390
# BB#1391:                              # %if.then5874
	movabsq	$1, %r15
	jmp	.LBB15_1392
.LBB15_1858:                            # %if.end.i.i9859
	movq	$116, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rbx
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %r9
	movl	16(%rbx), %eax
	movl	16(%r9), %ecx
	andq	$512, %rax              # imm = 0x200
	movabsq	$4294966783, %rdx       # imm = 0xFFFFFDFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r9)
	jmp	.LBB15_1954
.LBB15_254:
	movq	%rdx, %r13
	jmp	.LBB15_280
.LBB15_282:                             # %if.then219.i
	cmpq	$25, %rax
	movq	%rdx, %r13
	jne	.LBB15_280
# BB#283:                               # %if.then226.i
	movq	%r12, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	build_real_from_int_cst
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_286:                             # %if.end244.i
	movq	%r14, 368(%rsp)
	movq	%r12, 376(%rsp)
	movabsq	$fold_convert_1, %rax
	leaq	368(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	do_float_handler
	testq	%rbx, %rax
	je	.LBB15_288
# BB#287:                               # %if.then249.i
	movq	384(%rsp), %rdx
	xorq	%rax, %rax
	jmp	.LBB15_289
.LBB15_262:                             # %if.then59.i
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	size_int_type_wide
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_274:                             # %if.else156.i
	movq	496(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	480(%rsp), %rax
	movq	488(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	472(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	456(%rsp), %rax
	movq	464(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	movq	%rbx, %rcx
	andq	%rcx, %rax
	cmpq	%rcx, %rax
                                        # kill: RBX<def> RCX<kill>
	jne	.LBB15_277
# BB#275:                               # %if.else156.i
	testq	%r15, %r15
	je	.LBB15_277
# BB#276:                               # %land.lhs.true162.i
	movq	448(%rsp), %rax
	movq	%rax, 40(%rsp)
	movq	432(%rsp), %rax
	movq	440(%rsp), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	496(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	480(%rsp), %rax
	movq	488(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	movq	%rbx, %rcx
	andq	%rcx, %rax
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	je	.LBB15_278
.LBB15_277:                             # %if.then166.i
	movq	$1, %rbx
.LBB15_278:                             # %if.end168.i
	movq	496(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	480(%rsp), %rax
	movq	488(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	400(%rsp), %rcx
	leaq	392(%rsp), %rdx
	movq	%rax, (%rsp)
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	ereal_to_int
	movq	400(%rsp), %rax
	movq	392(%rsp), %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	build_int_2_wide
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	%r12, 8(%rax)
	movq	%r13, %r15
	movl	16(%r15), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	force_fit_type
	orq	%r14, %rax
	movq	120(%rsp), %rcx         # 8-byte Reload
	movl	16(%rcx), %edx
	movq	%rax, %rcx
	andq	$1, %rcx
	shlq	$19, %rcx
	andq	$-524289, %rdx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rcx, %rdx
	movq	120(%rsp), %rcx         # 8-byte Reload
	movl	%edx, 16(%rcx)
	movl	16(%r15), %esi
	movq	$18, %rcx
	shlq	%cl, %rax
	orq	%rsi, %rax
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	%ecx, 16(%rax)
	movq	120(%rsp), %r13         # 8-byte Reload
.LBB15_279:                             # %if.end209.i
	movq	%r12, 8(%r13)
.LBB15_280:                             # %if.end288.i
	movl	16(%r13), %eax
	orq	$512, %rax              # imm = 0x200
	movl	%eax, 16(%r13)
	movq	%r13, %r9
	jmp	.LBB15_1954
.LBB15_497:                             # %if.else2698
	cmpq	$60, %rsi
	jne	.LBB15_458
# BB#498:                               # %if.then2701
	movq	32(%r13), %r8
	movq	40(%r13), %rdi
.LBB15_499:                             # %while.cond2710
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, %rdx
	movzbl	16(%rdx), %esi
	leaq	-114(%rsi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_502
# BB#500:                               # %land.lhs.true2731
                                        #   in Loop: Header=BB15_499 Depth=1
	movq	32(%rdx), %r8
	cmpq	%rcx, %r8
	je	.LBB15_502
# BB#501:                               # %land.rhs2737
                                        #   in Loop: Header=BB15_499 Depth=1
	movq	8(%rdx), %rax
	movq	8(%r8), %r10
	movl	60(%rax), %r11d
	movl	60(%r10), %eax
	xorq	%r11, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_499
.LBB15_502:                             # %while.cond2764
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	addq	$-114, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB15_505
# BB#503:                               # %land.lhs.true2785
                                        #   in Loop: Header=BB15_502 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB15_505
# BB#504:                               # %land.rhs2791
                                        #   in Loop: Header=BB15_502 Depth=1
	movq	8(%rbx), %rax
	movq	8(%rdi), %r8
	movl	60(%rax), %r10d
	movl	60(%r8), %eax
	xorq	%r10, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB15_502
.LBB15_505:                             # %while.end2817
	cmpq	$25, %rsi
	jne	.LBB15_458
# BB#506:                               # %land.lhs.true2824
	movl	60(%r9), %eax
	andq	$511, %rax              # imm = 0x1FF
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	compare_tree_int
	testq	%r14, %rax
	je	.LBB15_507
.LBB15_458:
	movq	%r12, %rbx
	movq	%r15, %r12
	jmp	.LBB15_459
.LBB15_533:
	movabsq	$0, %rcx
.LBB15_535:                             # %if.then2689
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	orq	$84, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
.LBB15_518:                             # %return
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_519:                             # %return
	movq	%r13, %rcx
	jmp	.LBB15_520
.LBB15_1394:                            # %if.else5888
	cmpq	$97, 144(%rsp)          # 8-byte Folded Reload
	je	.LBB15_1396
# BB#1395:                              # %if.else5888
	cmpq	$100, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1404
.LBB15_1396:                            # %land.lhs.true5894
	movq	8(%rbx), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB15_1404
# BB#1397:                              # %land.lhs.true5903
	movl	16(%r13), %eax
	andq	$254, %rax
	cmpq	$114, %rax
	jne	.LBB15_1404
# BB#1398:                              # %land.lhs.true5917
	movq	32(%r13), %rax
	movzbl	16(%rax), %ecx
	cmpq	$82, %rcx
	jne	.LBB15_1404
# BB#1399:                              # %land.lhs.true5927
	movq	32(%rax), %rax
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1404
# BB#1400:                              # %if.then5936
	cmpq	$97, 144(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1401
# BB#1402:                              # %if.then5936
	movabsq	$1, %r15
	jmp	.LBB15_1403
.LBB15_1390:
	movabsq	$0, %r15
.LBB15_1392:                            # %if.then5874
	movq	8(%rbx), %rcx
	movq	40(%r13), %r8
	xorq	$1, %r15
	addq	$101, %r15
	movq	$83, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	movq	%r8, %rcx
	callq	build
	jmp	.LBB15_1393
.LBB15_288:                             # %if.else251.i
	movq	%r14, %rdi
	callq	copy_node
	movq	%rax, %rdx
	movq	$1, %rax
.LBB15_289:                             # %if.end253.i
	movq	%r14, %rbx
	movl	16(%rbx), %r14d
	movq	$19, %rcx
	shrq	%cl, %r14
.LBB15_312:                             # %land.end1524
	movq	%rdx, %r15
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	force_fit_type
	movq	%r15, %r9
	orq	%r14, %rax
.LBB15_313:                             # %return
	movl	16(%r9), %edx
	movq	%rax, %rcx
	andq	$1, %rcx
	shlq	$19, %rcx
	andq	$-524289, %rdx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rcx, %rdx
	movl	%edx, 16(%r9)
	movl	16(%rbx), %esi
	movq	$18, %rcx
	shlq	%cl, %rax
	orq	%rsi, %rax
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%r9)
	jmp	.LBB15_1954
.LBB15_507:                             # %land.lhs.true2838
	xorq	%rdx, %rdx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	operand_equal_p
	testq	%r14, %rax
	movq	%r12, %rbx
	movq	%r15, %r12
	je	.LBB15_459
# BB#508:                               # %if.then2841
	cmpq	$82, 160(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_509
# BB#536:                               # %if.then2841
	movabsq	$1, %rcx
	jmp	.LBB15_537
.LBB15_459:                             # %associate
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	jne	.LBB15_550
.LBB15_460:                             # %associate.land.lhs.true2855_crit_edge
	movq	128(%rsp), %rax         # 8-byte Reload
	leaq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$7, %rax
	je	.LBB15_463
# BB#461:                               # %land.lhs.true2855
	cmpq	$8, %rax
	jne	.LBB15_465
# BB#462:                               # %land.lhs.true2869
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_465
.LBB15_463:                             # %lor.lhs.false2878
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_485
# BB#464:                               # %lor.lhs.false2878
	movq	%r12, %rax
	andq	%r14, %rax
	cmpq	$61, %rax
	jne	.LBB15_485
.LBB15_465:                             # %if.then2883
	leaq	312(%rsp), %rax
	leaq	304(%rsp), %rcx
	leaq	296(%rsp), %r8
	xorq	%r9, %r9
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %r13
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	split_tree
	movq	%rax, %r15
	movq	%r13, %r12
	andq	%r14, %r12
	cmpq	$60, %r12
	movabsq	$0, %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	jne	.LBB15_467
# BB#466:                               # %if.then2883
	movabsq	$1, %rax
.LBB15_467:                             # %if.then2883
	leaq	288(%rsp), %rcx
	leaq	280(%rsp), %r8
	leaq	272(%rsp), %r9
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	movq	%r9, %r8
	movq	%rax, %r9
	callq	split_tree
	testq	%r15, %r15
	movabsq	$0, %rcx
	je	.LBB15_469
# BB#468:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_469:                             # %if.then2883
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB15_471
# BB#470:                               # %if.then2883
	movabsq	$1, %rdx
.LBB15_471:                             # %if.then2883
	movq	312(%rsp), %rsi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB15_473
# BB#472:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_473:                             # %if.then2883
	movq	288(%rsp), %rsi
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB15_475
# BB#474:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_475:                             # %if.then2883
	movq	304(%rsp), %rsi
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB15_477
# BB#476:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_477:                             # %if.then2883
	movq	280(%rsp), %rsi
	movq	%rsi, 104(%rsp)         # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB15_479
# BB#478:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_479:                             # %if.then2883
	movq	296(%rsp), %rsi
	movq	%rsi, 96(%rsp)          # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	movabsq	$0, %rcx
	je	.LBB15_481
# BB#480:                               # %if.then2883
	movabsq	$1, %rcx
.LBB15_481:                             # %if.then2883
	movq	272(%rsp), %rsi
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	addq	%rcx, %rdx
	testq	%rsi, %rsi
	je	.LBB15_483
# BB#482:                               # %if.then2883
	movabsq	$1, %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
.LBB15_483:                             # %if.then2883
	movq	160(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jl	.LBB15_484
# BB#538:                               # %if.then2921
	cmpq	$60, %r12
	movq	$59, %rcx
	je	.LBB15_540
# BB#539:                               # %if.then2921
	movq	%r13, %rcx
.LBB15_540:                             # %if.then2921
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%rax, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	128(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rcx
	callq	associate_trees
	movq	%rax, %rbx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	callq	associate_trees
	movq	%rax, %r15
	movq	%r15, 312(%rsp)
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	callq	associate_trees
	movq	%rax, %r12
	movq	%r12, 304(%rsp)
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%r13, %rcx
	callq	associate_trees
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%r13, 296(%rsp)
	je	.LBB15_544
# BB#541:                               # %if.then2921
	testq	%r12, %r12
	je	.LBB15_544
# BB#542:                               # %if.then2933
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	tree_int_cst_lt
	movq	$60, %rdx
	testq	%r14, %rax
	je	.LBB15_548
# BB#543:                               # %if.then2936
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	128(%rsp), %rcx         # 8-byte Reload
	callq	associate_trees
	movq	%rax, %r13
	movq	%r13, 296(%rsp)
	movq	$0, 304(%rsp)
	xorq	%r12, %r12
.LBB15_544:                             # %if.end2941
	testq	%r13, %r13
	je	.LBB15_549
# BB#545:                               # %if.then2943
	movq	$60, %rdx
	testq	%r15, %r15
	je	.LBB15_546
# BB#547:                               # %if.else2949
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	128(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rcx
	callq	associate_trees
	movq	%rax, 312(%rsp)
	movq	$59, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rcx
	callq	associate_trees
	movq	%r14, %rdi
	movq	%rax, %rsi
	jmp	.LBB15_554
.LBB15_1404:                            # %if.end5957
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1429
# BB#1405:                              # %if.then5960
	movq	144(%rsp), %rcx         # 8-byte Reload
	addq	$-97, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	ja	.LBB15_1427
# BB#1406:                              # %if.then5960
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$26, %rdx
	je	.LBB15_1407
# BB#1414:                              # %sw.bb5961
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$7, %rcx
	je	.LBB15_1428
# BB#1415:                              # %sw.bb5961
	cmpq	$8, %rcx
	jne	.LBB15_1417
# BB#1416:                              # %land.lhs.true5979
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_1417
.LBB15_1428:                            # %if.end5992
	movq	$101, %rax
	movq	160(%rsp), %rcx         # 8-byte Reload
	movb	%al, 16(%rcx)
	movq	$101, 144(%rsp)         # 8-byte Folded Spill
	jmp	.LBB15_1429
.LBB15_1401:
	movabsq	$0, %r15
.LBB15_1403:                            # %if.then5936
	movq	32(%r13), %rax
	movq	8(%rbx), %r14
	movq	40(%rax), %rcx
	xorq	$1, %r15
	addq	$101, %r15
	movq	$83, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	convert
.LBB15_1393:                            # %return
	movq	8(%rbx), %rcx
	movq	global_trees+88(%rip), %rdx
	movq	%rax, %r14
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	jmp	.LBB15_520
.LBB15_509:
	movabsq	$0, %rcx
.LBB15_537:                             # %if.then2841
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	32(%rax), %rdx
	orq	$84, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	144(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB15_520
.LBB15_484:
	movq	%r13, %r12
.LBB15_485:                             # %binary
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r14, %rax
	je	.LBB15_486
.LBB15_550:                             # %if.end2962
	movq	%r12, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	const_binop
	movq	%rax, %r9
	testq	%r9, %r9
	movq	120(%rsp), %rcx         # 8-byte Reload
	je	.LBB15_551
# BB#552:                               # %if.then2965
	movq	8(%r9), %rax
	movq	8(%rcx), %rdi
	cmpq	%rdi, %rax
	je	.LBB15_1954
# BB#553:                               # %if.then2972
	movq	%r9, %rsi
	jmp	.LBB15_554
.LBB15_486:
	movq	120(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_551:
	movq	%rcx, %r9
	jmp	.LBB15_1954
.LBB15_548:                             # %if.end2941.thread
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	128(%rsp), %rcx         # 8-byte Reload
	callq	associate_trees
	movq	%rax, %r12
	movq	%r12, 304(%rsp)
	movq	$0, 296(%rsp)
.LBB15_549:                             # %if.end2953
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdx
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rcx
	callq	associate_trees
	movq	%rax, 312(%rsp)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	associate_trees
	movq	%r15, %rdi
	movq	%rax, %rsi
	jmp	.LBB15_554
.LBB15_546:                             # %if.then2946
	movq	%rbx, %rdi
	movq	%r13, %rsi
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rcx
	callq	associate_trees
	movq	%rbx, %rdi
	movq	%rax, %rsi
.LBB15_554:                             # %return
	callq	convert
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1407:                            # %if.then5960
	shlq	%cl, %rax
	testq	$5, %rax
	jne	.LBB15_1412
# BB#1408:                              # %if.then5960
	cmpq	$5, %rcx
	jne	.LBB15_1427
# BB#1409:                              # %sw.bb5999
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$7, %rcx
	movq	$102, 144(%rsp)         # 8-byte Folded Spill
	je	.LBB15_1429
# BB#1410:                              # %sw.bb5999
	cmpq	$8, %rcx
	jne	.LBB15_1412
# BB#1411:                              # %land.lhs.true6017
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB15_1412
.LBB15_1429:                            # %if.end6034
	movzbl	16(%r13), %eax
	cmpq	$25, %rax
	jne	.LBB15_1476
# BB#1430:                              # %land.lhs.true6041
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	je	.LBB15_1476
# BB#1431:                              # %if.then6048
	movq	$0, 192(%rsp)
	movq	$0, 184(%rsp)
	xorq	%rax, %rax
	movl	%eax, 180(%rsp)
	leaq	192(%rsp), %rax
	leaq	184(%rsp), %rcx
	leaq	180(%rsp), %r8
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	twoval_comparison_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1476
# BB#1432:                              # %if.then6048
	movq	192(%rsp), %rax
	testq	%rax, %rax
	je	.LBB15_1476
# BB#1433:                              # %if.then6048
	movq	184(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB15_1476
# BB#1434:                              # %land.lhs.true6060
	movzbl	17(%rax), %edx
	testq	$2, %rdx
	je	.LBB15_1436
# BB#1435:                              # %land.lhs.true6067
	movzbl	17(%rcx), %edx
	testq	$2, %rdx
	jne	.LBB15_1476
.LBB15_1436:                            # %land.lhs.true6074
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB15_1476
# BB#1437:                              # %land.lhs.true6081
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB15_1476
# BB#1438:                              # %land.lhs.true6081
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$7232, %rdx             # imm = 0x1C40
	je	.LBB15_1476
# BB#1439:                              # %land.lhs.true6117
	movq	112(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB15_1476
# BB#1440:                              # %land.lhs.true6129
	movq	104(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1476
# BB#1441:                              # %if.then6140
	movq	192(%rsp), %rax
	movq	8(%rax), %rcx
	movq	104(%rcx), %rdx
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	movq	112(%rcx), %r12
	movq	184(%rsp), %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	movq	168(%rsp), %r8          # 8-byte Reload
	callq	eval_subst
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	192(%rsp), %rcx
	movq	184(%rsp), %r8
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	movq	%r12, %r8
	callq	eval_subst
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	192(%rsp), %rcx
	movq	184(%rsp), %r8
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	168(%rsp), %rdx         # 8-byte Reload
	movq	%r8, %rcx
	movq	%r12, %r8
	callq	eval_subst
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1443
# BB#1442:                              # %lor.lhs.false6167
	movq	%r14, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1476
.LBB15_1443:                            # %land.lhs.true6170
	movq	%r15, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1445
# BB#1444:                              # %lor.lhs.false6173
	movq	%r15, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1476
.LBB15_1445:                            # %land.lhs.true6176
	movq	%r12, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1447
# BB#1446:                              # %lor.lhs.false6179
	movq	%r12, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1476
.LBB15_1447:                            # %if.then6182
	movq	%r14, %rdi
	callq	integer_onep
	movq	%rax, %r14
	movq	$2, %rcx
	shlq	%cl, %r14
	movq	%r15, %rdi
	callq	integer_onep
	leaq	(%r14,%rax,2), %r14
	movq	%r12, %rdi
	callq	integer_onep
	addq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$7, %rax
	ja	.LBB15_1474
# BB#1448:                              # %if.then6182
	jmpq	*.LJTI15_12(,%rax,8)
.LBB15_1449:                            # %sw.bb6189
	movq	global_trees+88(%rip), %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	jne	.LBB15_1138
# BB#1450:                              # %if.end.i10150
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1454
# BB#1451:                              # %if.end.i10150
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1452:                              # %if.end.i10150
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1453:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1417:                            # %if.then5990
	movq	integer_types+40(%rip), %rax
	movq	128(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB15_1418
# BB#1419:                              # %if.else.i10173
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$11, %rax
	jne	.LBB15_1421
# BB#1420:                              # %if.then2.i10175
	movq	global_trees+96(%rip), %rax
	jmp	.LBB15_1425
.LBB15_1427:                            # %sw.default6032
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.fold, %rcx
	movq	$6853, %rsi             # imm = 0x1AC5
	jmp	.LBB15_1099
.LBB15_1412:                            # %sw.bb6030
	movq	integer_types+40(%rip), %rax
	movq	128(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB15_1413
# BB#1423:                              # %if.else.i
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$11, %rax
	jne	.LBB15_1426
# BB#1424:                              # %if.then2.i
	movq	global_trees+88(%rip), %rax
	jmp	.LBB15_1425
.LBB15_1476:                            # %if.end6208
	movzbl	16(%rbx), %eax
	leaq	-39(%rax), %rdx
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	-101(%rcx), %rcx
	orq	%rcx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$1, %rdx
	ja	.LBB15_1533
# BB#1477:                              # %land.lhs.true6228
	movl	16(%r13), %esi
	movl	optimize(%rip), %edx
	andq	$255, %rsi
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	testq	%rdx, %rdx
	jne	.LBB15_1479
# BB#1478:                              # %land.lhs.true6228
	cmpq	$25, 168(%rsp)          # 8-byte Folded Reload
	je	.LBB15_1479
.LBB15_1533:                            # %if.end6244
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	ja	.LBB15_1553
# BB#1534:                              # %land.lhs.true6250
	movq	8(%rbx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$8, %rdx
	jne	.LBB15_1543
# BB#1535:                              # %land.lhs.true6259
	cmpq	$27, %rax
	je	.LBB15_1539
# BB#1536:                              # %land.lhs.true6259
	cmpq	$125, %rax
	je	.LBB15_1539
# BB#1537:                              # %switch.early.test
	movzbl	16(%r13), %eax
	cmpq	$27, %rax
	je	.LBB15_1539
# BB#1538:                              # %switch.early.test
	cmpq	$125, %rax
	jne	.LBB15_1543
.LBB15_1539:                            # %if.then6287
	cmpq	$101, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1540
# BB#1541:                              # %if.then6287
	movabsq	$1, %rax
	jmp	.LBB15_1542
.LBB15_1473:                            # %sw.bb6191
	movq	$97, 144(%rsp)          # 8-byte Folded Spill
	jmp	.LBB15_1474
.LBB15_1458:                            # %sw.bb6192
	movq	$101, 144(%rsp)         # 8-byte Folded Spill
	jmp	.LBB15_1474
.LBB15_1459:                            # %sw.bb6193
	movq	$98, 144(%rsp)          # 8-byte Folded Spill
	jmp	.LBB15_1474
.LBB15_1460:                            # %sw.bb6194
	movq	$99, 144(%rsp)          # 8-byte Folded Spill
	jmp	.LBB15_1474
.LBB15_1461:                            # %sw.bb6195
	movq	$102, 144(%rsp)         # 8-byte Folded Spill
	jmp	.LBB15_1474
.LBB15_1462:                            # %sw.bb6196
	movq	$100, 144(%rsp)         # 8-byte Folded Spill
.LBB15_1474:                            # %sw.epilog6199
	movq	192(%rsp), %rcx
	movq	184(%rsp), %rbx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movl	180(%rsp), %ecx
	testq	%rcx, %rcx
	je	.LBB15_715
# BB#1475:                              # %if.then6202
	movq	%rax, %rdi
	callq	save_expr
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1463:                            # %sw.bb6197
	movq	global_trees+96(%rip), %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	movzbl	17(%rbx), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	je	.LBB15_1464
.LBB15_1138:                            # %if.then.i10387
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	128(%rsp), %rsi         # 8-byte Reload
.LBB15_926:                             # %return
	movq	%rbx, %rdx
	movq	%r8, %rcx
	jmp	.LBB15_520
.LBB15_1454:                            # %if.end.i10150
	cmpq	$116, %rax
	jne	.LBB15_1456
# BB#1455:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1418:                            # %if.then.i10168
	movq	global_trees+96(%rip), %r9
	jmp	.LBB15_1954
.LBB15_1421:                            # %if.else8.i10178
	movq	$1, %rdi
	jmp	.LBB15_1422
.LBB15_1413:                            # %if.then.i10162
	movq	global_trees+88(%rip), %r9
	jmp	.LBB15_1954
.LBB15_1426:                            # %if.else8.i
	xorq	%rdi, %rdi
.LBB15_1422:                            # %return
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, %r9
	movq	128(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%r9)
	jmp	.LBB15_1954
.LBB15_1479:                            # %if.then6237
	movq	8(%rbx), %r15
	xorq	%rax, %rax
	movl	%eax, 528(%rsp)
	movl	%eax, 524(%rsp)
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	leaq	528(%rsp), %rax
	leaq	560(%rsp), %rcx
	leaq	568(%rsp), %rdx
	leaq	512(%rsp), %r8
	leaq	504(%rsp), %r9
	leaq	536(%rsp), %r10
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rcx
	movq	%r9, %r8
	movq	%r10, %r9
	callq	get_inner_reference
	movq	%rax, %r14
	cmpq	%rbx, %r14
	je	.LBB15_1530
# BB#1480:                              # %lor.lhs.false.i
	movslq	504(%rsp), %rcx
	movq	560(%rsp), %rax
	movzbl	mode_bitsize(%rcx,%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB15_1530
# BB#1481:                              # %lor.lhs.false.i
	testq	%rax, %rax
	js	.LBB15_1530
# BB#1482:                              # %lor.lhs.false.i
	movq	512(%rsp), %rcx
	testq	%rcx, %rcx
	jne	.LBB15_1530
# BB#1483:                              # %lor.lhs.false15.i
	movzbl	16(%r14), %ecx
	cmpq	$57, %rcx
	je	.LBB15_1530
# BB#1484:                              # %if.end.i11129
	cmpq	$25, 168(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1486
# BB#1485:                              # %if.end47.i.thread
	movq	8(%r14), %rdx
	movq	568(%rsp), %rcx
	movl	64(%rdx), %edx
	movq	$0, 120(%rsp)           # 8-byte Folded Spill
	jmp	.LBB15_1495
.LBB15_1543:                            # %land.lhs.true6322
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1553
# BB#1544:                              # %land.lhs.true6325
	movzbl	16(%rbx), %eax
	cmpq	$53, %rax
	jne	.LBB15_1553
# BB#1545:                              # %land.lhs.true6332
	movq	32(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB15_1553
# BB#1546:                              # %if.then6342
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	jne	.LBB15_1553
# BB#1547:                              # %land.lhs.true6357
	movq	48(%rax), %rdx
	movq	$29, %rcx
	shrq	%cl, %rdx
	orq	$2, %rdx
	andq	$3, %rdx
	cmpq	$2, %rdx
	je	.LBB15_1553
# BB#1548:                              # %land.lhs.true6374
	movl	56(%rax), %eax
	cmpq	$31, %rax
	jne	.LBB15_1553
# BB#1549:                              # %land.lhs.true6378
	movq	40(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB15_1553
# BB#1550:                              # %land.lhs.true6383
	movq	32(%rcx), %rax
	movq	8(%rax), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$13, %rdx
	jne	.LBB15_1553
# BB#1551:                              # %land.lhs.true6394
	cmpq	$0, (%rcx)
	je	.LBB15_1552
.LBB15_1553:                            # %if.end6404
	movq	144(%rsp), %rax         # 8-byte Reload
	andq	$254, %rax
	cmpq	$98, %rax
	jne	.LBB15_1554
# BB#1557:                              # %if.then6410
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	-97(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	ja	.LBB15_1098
# BB#1558:                              # %if.then6410
	movq	$97, %r15
	xorq	%r14, %r14
	movq	%rbx, %rcx
	jmpq	*.LJTI15_13(,%rax,8)
.LBB15_1559:                            # %sw.bb3.i10114
	movq	$99, %r15
	jmp	.LBB15_1568
.LBB15_1540:
	movabsq	$0, %rax
.LBB15_1542:                            # %if.then6287
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	8(%rcx), %r14
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movq	$127, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
	movq	$128, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r15
	movq	$127, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
	movq	$128, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r14
	xorq	$1, 168(%rsp)           # 8-byte Folded Spill
	addq	$91, 168(%rsp)          # 8-byte Folded Spill
	xorq	%rax, %rax
	movq	144(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r12, %rdx
.LBB15_1926:                            # %return
	callq	build
.LBB15_1927:                            # %return
	movq	%rax, %rdi
.LBB15_1928:                            # %return
	callq	fold
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1464:                            # %if.end.i10128
	movzbl	16(%r8), %eax
	cmpq	$115, %rax
	jg	.LBB15_1468
# BB#1465:                              # %if.end.i10128
	cmpq	$29, %rax
	ja	.LBB15_1472
# BB#1466:                              # %if.end.i10128
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	je	.LBB15_1472
# BB#1467:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1456:                            # %if.end.i10150
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1457:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1486:                            # %if.then22.i
	leaq	524(%rsp), %rax
	leaq	544(%rsp), %rcx
	leaq	552(%rsp), %rdx
	leaq	512(%rsp), %r8
	leaq	540(%rsp), %r9
	leaq	532(%rsp), %r10
	movq	%rax, (%rsp)
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rcx
	movq	%r9, %r8
	movq	%r10, %r9
	callq	get_inner_reference
	movq	%rax, 120(%rsp)         # 8-byte Spill
	cmpq	%rbx, %rax
	movq	%rbx, %r13
	je	.LBB15_1530
# BB#1487:                              # %lor.lhs.false26.i
	movq	568(%rsp), %rcx
	movq	552(%rsp), %rax
	cmpq	%rax, %rcx
	jne	.LBB15_1530
# BB#1488:                              # %lor.lhs.false29.i
	movq	560(%rsp), %rax
	movq	544(%rsp), %rdx
	cmpq	%rdx, %rax
	jne	.LBB15_1530
# BB#1489:                              # %lor.lhs.false32.i
	movl	536(%rsp), %edx
	movl	532(%rsp), %esi
	cmpq	%rsi, %rdx
	jne	.LBB15_1530
# BB#1490:                              # %lor.lhs.false32.i
	movq	512(%rsp), %rdx
	testq	%rdx, %rdx
	jne	.LBB15_1530
# BB#1491:                              # %lor.lhs.false38.i
	movq	120(%rsp), %rdx         # 8-byte Reload
	movzbl	16(%rdx), %edx
	cmpq	$57, %rdx
	je	.LBB15_1530
# BB#1492:                              # %if.end47.i
	movq	8(%r14), %rdx
	movl	64(%rdx), %edx
	cmpq	$25, 168(%rsp)          # 8-byte Folded Reload
	je	.LBB15_1495
# BB#1493:                              # %cond.false.i11132
	movq	120(%rsp), %rsi         # 8-byte Reload
	movq	8(%rsi), %rsi
	movl	64(%rsi), %esi
	movq	%rdx, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	cmpq	%rsi, %rdi
	jb	.LBB15_1495
# BB#1494:                              # %cond.false.i11132
	movq	%rsi, %rdx
.LBB15_1495:                            # %cond.end74.i
	movl	528(%rsp), %esi
	movl	word_mode(%rip), %r8d
	testq	%rsi, %rsi
	movq	$1, %r9
	jne	.LBB15_1499
# BB#1496:                              # %lor.rhs.i11133
	movl	524(%rsp), %esi
	testq	%rsi, %rsi
	je	.LBB15_1497
# BB#1498:                              # %lor.rhs.i11133
	movabsq	$1, %r9
	jmp	.LBB15_1499
.LBB15_1552:                            # %if.then6397
	movq	integer_types(%rip), %rcx
	movq	$41, %rdi
	movq	%rcx, %rsi
	movq	%rax, %rdx
	callq	build1
	movq	global_trees+88(%rip), %rcx
	movq	%rax, %rdx
	jmp	.LBB15_1323
.LBB15_1554:
	movq	%r13, %rcx
	movq	%rbx, %r13
.LBB15_1555:                            # %if.end6412
	movq	%rcx, %rbx
	movq	144(%rsp), %rax         # 8-byte Reload
	andq	$253, %rax
	xorq	%r14, %r14
	cmpq	$100, %rax
	jne	.LBB15_1556
# BB#1560:                              # %if.then6418
	movq	144(%rsp), %rcx         # 8-byte Reload
	addq	$-97, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	movq	%rcx, %rax
	ja	.LBB15_1566
# BB#1561:                              # %if.then6418
	movq	$102, %r15
	movq	$1, %r14
	jmpq	*.LJTI15_14(,%rax,8)
.LBB15_1564:                            # %sw.bb4.i
	movq	$100, %r15
	jmp	.LBB15_1568
.LBB15_1098:                            # %sw.default.i10414
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.swap_tree_comparison, %rcx
	movq	$2357, %rsi             # imm = 0x935
	jmp	.LBB15_1099
.LBB15_1468:                            # %if.end.i10128
	cmpq	$116, %rax
	jne	.LBB15_1470
# BB#1469:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1497:
	movabsq	$0, %r9
.LBB15_1499:                            # %lor.end.i11135
	movq	%rax, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rcx
	movq	%r9, %r8
	callq	get_best_mode
	movq	%rax, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB15_1530
# BB#1500:                              # %if.end82.i
	xorq	%rsi, %rsi
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	movq	%r12, %rdi
	callq	type_for_mode
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	type_for_mode
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movzbl	mode_bitsize(%r12,%r12), %esi
	movq	%rax, %r12
	movq	568(%rsp), %rax
	movq	%rsi, %rdx
	negq	%rdx
	movq	560(%rsp), %rcx
	andq	%rax, %rdx
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	subq	%rdx, %rax
	movq	%rax, 568(%rsp)
	cmpq	%rcx, %rsi
	movq	%rsi, %rbx
	je	.LBB15_1530
# BB#1501:                              # %if.end92.i
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%r12, 8(%rax)
	xorq	%rsi, %rsi
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	force_fit_type
	movq	%r12, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	convert
	movq	560(%rsp), %rcx
	movq	sizetype_tab(%rip), %rdx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	%rbx, %rax
	subq	%rcx, %rax
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	560(%rsp), %rcx
	movq	568(%rsp), %rdx
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%rbx, %rax
	subq	%rcx, %rax
	subq	%rdx, %rax
	movq	%rax, %rdi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
	cmpq	$25, 168(%rsp)          # 8-byte Folded Reload
	jne	.LBB15_1502
# BB#1503:                              # %if.end116.i
	movl	536(%rsp), %eax
	testq	%rax, %rax
	je	.LBB15_1512
# BB#1504:                              # %if.then118.i
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	560(%rsp), %rcx
	movq	sizetype_tab(%rip), %rdx
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1519
# BB#1505:                              # %if.then124.i
	cmpq	$102, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1506
# BB#1507:                              # %if.then124.i
	movabsq	$1, %rcx
	jmp	.LBB15_1508
.LBB15_1556:
	movq	144(%rsp), %r15         # 8-byte Reload
	jmp	.LBB15_1568
.LBB15_1566:                            # %sw.default.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.invert_tree_comparison, %rcx
	movq	$2332, %rsi             # imm = 0x91C
.LBB15_1099:                            # %sw.default.i10414
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB15_1565:                            # %sw.bb5.i
	movq	$99, %r15
	jmp	.LBB15_1568
.LBB15_1567:                            # %sw.bb2.i10113
	movq	$98, %r15
	jmp	.LBB15_1568
.LBB15_1563:                            # %sw.bb3.i
	movq	$97, %r15
	movq	$1, %r14
	jmp	.LBB15_1568
.LBB15_1562:                            # %sw.bb1.i
	movq	$101, %r15
.LBB15_1568:                            # %if.end6420
	movzbl	16(%r13), %eax
	cmpq	$26, %rax
	jne	.LBB15_1569
# BB#1580:                              # %land.lhs.true6524
	movzbl	16(%rbx), %eax
	cmpq	$26, %rax
	jne	.LBB15_1581
# BB#1582:                              # %if.then6531
	movq	%r13, %rdx
	movq	48(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rdx, %r12
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1584
# BB#1583:                              # %lor.lhs.false6536
	movq	%rbx, %rdx
	movq	48(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
                                        # kill: RBX<def> RDX<kill>
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1588
.LBB15_1584:                            # %if.then6541
	cmpq	$97, %r15
	jne	.LBB15_1585
# BB#1586:                              # %if.then6541
	movabsq	$1, %rax
	andq	%r14, %rax
	jmp	.LBB15_1594
.LBB15_1470:                            # %if.end.i10128
	cmpq	$121, %rax
	jne	.LBB15_1472
# BB#1471:
	movq	%r8, %r9
	jmp	.LBB15_1954
.LBB15_1502:                            # %if.then106.i
	movq	sizetype_tab(%rip), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r15
	movq	$32, %rcx
	movq	sizetype_tab+24(%rip), %rax
	movq	136(%rsp), %r13         # 8-byte Reload
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rcx
	movl	16(%rcx), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%rcx)
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, %r14
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	sizetype_tab+24(%rip), %rcx
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%r15, %rcx
	callq	build
	movq	%rax, %rcx
	movl	16(%rcx), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%rcx)
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%r14, %rdx
	jmp	.LBB15_1528
.LBB15_1512:                            # %if.else.i11146
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	convert
	movq	560(%rsp), %rcx
	movq	sizetype_tab(%rip), %rdx
	movq	%rax, 168(%rsp)         # 8-byte Spill
	decq	%rcx
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1519
# BB#1513:                              # %land.lhs.true.i11147
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	integer_all_onesp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1514
.LBB15_1519:                            # %if.end154.i
	movq	560(%rsp), %rax
	cmpq	$1, %rax
	jne	.LBB15_1525
# BB#1520:                              # %land.lhs.true157.i
	movq	%r13, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB15_1525
# BB#1521:                              # %if.then160.i
	cmpq	$101, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1522
# BB#1523:                              # %if.then160.i
	movabsq	$1, %rax
	jmp	.LBB15_1524
.LBB15_1506:
	movabsq	$0, %rcx
.LBB15_1508:                            # %if.then124.i
	movabsq	$.L.str1, %rdx
	jne	.LBB15_1510
# BB#1509:
	movabsq	$global_trees+96, %r14
	jmp	.LBB15_1511
.LBB15_1569:                            # %if.end6420
	cmpq	$25, %rax
	jne	.LBB15_1570
# BB#1571:                              # %land.lhs.true6427
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB15_1572
# BB#1573:                              # %if.then6434
	cmpq	$101, %r15
	jne	.LBB15_1575
# BB#1574:                              # %if.then6437
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	tree_int_cst_equal
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	jmp	.LBB15_1594
.LBB15_1581:
	movq	160(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1588:                            # %if.else6550
	movq	%r12, %rdx
	addq	$32, %rdx
	movq	%rbx, %rcx
	addq	$32, %rcx
	movq	16(%rcx), %rax
	movq	%rax, 40(%rsp)
	movq	(%rcx), %rax
	movq	8(%rcx), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 24(%rsp)
	movq	16(%rdx), %rax
	movq	%rax, 16(%rsp)
	movq	(%rdx), %rax
	movq	8(%rdx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	cmpq	$101, %r15
	jne	.LBB15_1591
# BB#1589:                              # %if.then6553
	testq	%rcx, %rax
	je	.LBB15_1593
# BB#1590:
	movabsq	$0, %rax
	jmp	.LBB15_1594
.LBB15_1585:
	movabsq	$0, %rax
	andq	%r14, %rax
	jmp	.LBB15_1594
.LBB15_1472:                            # %if.end.i.i10556
	movq	%r8, %rbx
.LBB15_618:                             # %if.end.i.i11001
	movq	8(%rbx), %rax
	movq	$116, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	jmp	.LBB15_619
.LBB15_1514:                            # %if.then143.i
	cmpq	$102, 144(%rsp)         # 8-byte Folded Reload
	jne	.LBB15_1515
# BB#1516:                              # %if.then143.i
	movabsq	$1, %rcx
	jmp	.LBB15_1517
.LBB15_1522:
	movabsq	$0, %rax
.LBB15_1524:                            # %if.then160.i
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	global_trees+88(%rip), %rax
	addq	$101, 144(%rsp)         # 8-byte Folded Spill
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r13
.LBB15_1525:                            # %if.end165.i
	movq	sizetype_tab(%rip), %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r15
	movq	$32, %rcx
	movq	sizetype_tab+24(%rip), %rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	build
	movq	%rax, %r14
	movl	16(%r14), %eax
	movq	%rax, %rcx
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%r14)
	movl	528(%rsp), %ecx
	testq	%rcx, %rcx
	je	.LBB15_1527
# BB#1526:                              # %if.then170.i
	orq	$10496, %rax            # imm = 0x2900
	movl	%eax, 16(%r14)
.LBB15_1527:                            # %if.end178.i
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	568(%rsp), %rcx
	movq	sizetype_tab(%rip), %rdx
	movq	%rax, %r15
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	$88, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r15
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r15, %rcx
.LBB15_1528:                            # %optimize_bit_field_compare.exit
	callq	build
	jmp	.LBB15_1529
.LBB15_1570:
	movq	160(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1572:
	movq	160(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1575:                            # %if.else6441
	movq	%r13, %rdx
	movq	8(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rdx, %rsi
	movzbl	17(%rax), %eax
	movq	40(%rbx), %rdx
	testq	$32, %rax
	je	.LBB15_1577
# BB#1576:                              # %cond.true6450
	movq	$1, %rax
	cmpq	%rdx, %rcx
	jae	.LBB15_1578
	jmp	.LBB15_1594
.LBB15_1591:                            # %if.else6563
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB15_1592
.LBB15_1593:                            # %if.else6563
	movabsq	$1, %rax
	jmp	.LBB15_1594
.LBB15_1515:
	movabsq	$0, %rcx
.LBB15_1517:                            # %if.then143.i
	movabsq	$.L.str1, %rdx
	je	.LBB15_1518
.LBB15_1510:                            # %if.then124.i
	movabsq	$global_trees+88, %r14
	jmp	.LBB15_1511
.LBB15_1577:                            # %cond.false6481
	movq	$1, %rax
	cmpq	%rdx, %rcx
	jl	.LBB15_1594
.LBB15_1578:                            # %lor.rhs6490
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB15_1594
# BB#1579:                              # %land.rhs6499
	movq	32(%rsi), %rax
	movq	32(%rbx), %rcx
	cmpq	%rcx, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	jmp	.LBB15_1594
.LBB15_1592:
	movabsq	$0, %rax
.LBB15_1594:                            # %if.else6563
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	testq	%rax, %rax
	movq	160(%rsp), %r9          # 8-byte Reload
	je	.LBB15_1954
# BB#1595:                              # %if.end6580
	testq	%r14, %r14
	je	.LBB15_1597
# BB#1596:                              # %if.then6582
	xorq	$1, 32(%rax)
.LBB15_1597:                            # %if.end6586
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rax)
	movzbl	16(%rcx), %ecx
	cmpq	$11, %rcx
	jne	.LBB15_1598
.LBB15_1425:                            # %if.then6595
	movq	%rax, %rdi
	callq	truthvalue_conversion
	movq	%rax, %r9
	jmp	.LBB15_1954
.LBB15_1518:
	movabsq	$global_trees+96, %r14
.LBB15_1511:                            # %if.then124.i
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	warning
	movq	(%r14), %rax
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
.LBB15_1529:                            # %optimize_bit_field_compare.exit
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB15_1530:                            # %optimize_bit_field_compare.exit
	cmpq	$0, 112(%rsp)           # 8-byte Folded Reload
	jne	.LBB15_1532
# BB#1531:                              # %optimize_bit_field_compare.exit
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB15_1532:                            # %optimize_bit_field_compare.exit
	movq	112(%rsp), %r9          # 8-byte Reload
	jmp	.LBB15_1954
.LBB15_1598:
	movq	%rax, %r9
	jmp	.LBB15_1954
.Ltmp151:
	.size	fold, .Ltmp151-fold
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_151
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_294
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1881
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1599
	.quad	.LBB15_1954
	.quad	.LBB15_1949
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1929
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_361
	.quad	.LBB15_555
	.quad	.LBB15_600
	.quad	.LBB15_793
	.quad	.LBB15_793
	.quad	.LBB15_793
	.quad	.LBB15_793
	.quad	.LBB15_804
	.quad	.LBB15_804
	.quad	.LBB15_804
	.quad	.LBB15_804
	.quad	.LBB15_773
	.quad	.LBB15_793
	.quad	.LBB15_152
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_152
	.quad	.LBB15_299
	.quad	.LBB15_839
	.quad	.LBB15_863
	.quad	.LBB15_326
	.quad	.LBB15_1954
	.quad	.LBB15_813
	.quad	.LBB15_813
	.quad	.LBB15_813
	.quad	.LBB15_813
	.quad	.LBB15_675
	.quad	.LBB15_703
	.quad	.LBB15_722
	.quad	.LBB15_758
	.quad	.LBB15_357
	.quad	.LBB15_889
	.quad	.LBB15_1016
	.quad	.LBB15_891
	.quad	.LBB15_150
	.quad	.LBB15_1058
	.quad	.LBB15_888
	.quad	.LBB15_1086
	.quad	.LBB15_1086
	.quad	.LBB15_1086
	.quad	.LBB15_1086
	.quad	.LBB15_1086
	.quad	.LBB15_1086
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_298
	.quad	.LBB15_152
	.quad	.LBB15_152
	.quad	.LBB15_1954
	.quad	.LBB15_290
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1954
	.quad	.LBB15_1885
	.quad	.LBB15_346
	.quad	.LBB15_1888
	.quad	.LBB15_1905
.LJTI15_1:
	.quad	.LBB15_1686
	.quad	.LBB15_1686
	.quad	.LBB15_1674
	.quad	.LBB15_1674
	.quad	.LBB15_1654
	.quad	.LBB15_1664
.LJTI15_2:
	.quad	.LBB15_1748
	.quad	.LBB15_1748
	.quad	.LBB15_1762
	.quad	.LBB15_1762
	.quad	.LBB15_1728
	.quad	.LBB15_1738
.LJTI15_3:
	.quad	.LBB15_1783
	.quad	.LBB15_1795
	.quad	.LBB15_1807
	.quad	.LBB15_1819
	.quad	.LBB15_1832
	.quad	.LBB15_1833
.LJTI15_4:
	.quad	.LBB15_1096
	.quad	.LBB15_1097
	.quad	.LBB15_1100
	.quad	.LBB15_1095
	.quad	.LBB15_1101
	.quad	.LBB15_1101
.LJTI15_5:
	.quad	.LBB15_1112
	.quad	.LBB15_1113
	.quad	.LBB15_1114
	.quad	.LBB15_1111
	.quad	.LBB15_1115
	.quad	.LBB15_1115
.LJTI15_6:
	.quad	.LBB15_1157
	.quad	.LBB15_1155
	.quad	.LBB15_1167
	.quad	.LBB15_1167
	.quad	.LBB15_1167
	.quad	.LBB15_1167
	.quad	.LBB15_1168
	.quad	.LBB15_1167
	.quad	.LBB15_1168
.LJTI15_7:
	.quad	.LBB15_1157
	.quad	.LBB15_1127
	.quad	.LBB15_1129
	.quad	.LBB15_1129
	.quad	.LBB15_1129
	.quad	.LBB15_1129
	.quad	.LBB15_1130
	.quad	.LBB15_1129
	.quad	.LBB15_1130
.LJTI15_8:
	.quad	.LBB15_1264
	.quad	.LBB15_1255
	.quad	.LBB15_1245
	.quad	.LBB15_1254
.LJTI15_9:
	.quad	.LBB15_1214
	.quad	.LBB15_1254
	.quad	.LBB15_1264
	.quad	.LBB15_1223
.LJTI15_10:
	.quad	.LBB15_1264
	.quad	.LBB15_1200
	.quad	.LBB15_1190
	.quad	.LBB15_1254
.LJTI15_11:
	.quad	.LBB15_1294
	.quad	.LBB15_1283
	.quad	.LBB15_1303
	.quad	.LBB15_1284
.LJTI15_12:
	.quad	.LBB15_1449
	.quad	.LBB15_1473
	.quad	.LBB15_1458
	.quad	.LBB15_1459
	.quad	.LBB15_1460
	.quad	.LBB15_1461
	.quad	.LBB15_1462
	.quad	.LBB15_1463
.LJTI15_13:
	.quad	.LBB15_1559
	.quad	.LBB15_1563
	.quad	.LBB15_1568
	.quad	.LBB15_1567
	.quad	.LBB15_1555
	.quad	.LBB15_1555
.LJTI15_14:
	.quad	.LBB15_1564
	.quad	.LBB15_1565
	.quad	.LBB15_1567
	.quad	.LBB15_1563
	.quad	.LBB15_1568
	.quad	.LBB15_1562

	.text
	.globl	size_diffop
	.align	16, 0x90
	.type	size_diffop,@function
size_diffop:                            # @size_diffop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp157:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp158:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp159:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp160:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp161:
	.cfi_def_cfa_offset 48
.Ltmp162:
	.cfi_offset %rbx, -40
.Ltmp163:
	.cfi_offset %r12, -32
.Ltmp164:
	.cfi_offset %r14, -24
.Ltmp165:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movl	16(%rax), %ecx
	movq	%rsi, %r14
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$6, %rdx
	jne	.LBB16_23
# BB#1:                                 # %lor.lhs.false
	movzbl	62(%rax), %edx
	testq	$2, %rdx
	je	.LBB16_23
# BB#2:                                 # %lor.lhs.false6
	movq	8(%r14), %rdx
	cmpq	%rdx, %rax
	jne	.LBB16_23
# BB#3:                                 # %if.end
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB16_4
# BB#5:                                 # %if.end16
	movq	sizetype_tab+24(%rip), %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB16_7
# BB#6:                                 # %if.end16
	movq	%rsi, %rdx
.LBB16_7:                               # %if.end16
	movq	sizetype_tab+40(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB16_9
# BB#8:                                 # %if.end16
	movq	%rsi, %rcx
.LBB16_9:                               # %if.end16
	orq	%rcx, %rdx
	jne	.LBB16_10
# BB#11:                                # %if.end16
	movabsq	$sizetype_tab+8, %rax
	jmp	.LBB16_12
.LBB16_4:                               # %if.then15
	movq	$60, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	jmp	.LBB16_16
.LBB16_10:
	movabsq	$sizetype_tab+32, %rax
.LBB16_12:                              # %if.end16
	movzbl	16(%rbx), %ecx
	movq	(%rax), %r15
	cmpq	$25, %rcx
	jne	.LBB16_14
# BB#13:                                # %lor.lhs.false25
	movzbl	16(%r14), %eax
	cmpq	$25, %rax
	jne	.LBB16_14
# BB#17:                                # %if.end35
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB16_20
# BB#18:                                # %if.then38
	movq	global_trees+88(%rip), %rsi
	movq	%r15, %rdi
	jmp	.LBB16_19
.LBB16_14:                              # %if.then31
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	$60, %rdi
	movq	%rbx, %rsi
.LBB16_15:                              # %if.then31
	movq	%rax, %rdx
.LBB16_16:                              # %if.then31
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	size_binop  # TAILCALL
.LBB16_20:                              # %if.else
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	je	.LBB16_22
# BB#21:                                # %if.then42
	movq	$60, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	size_binop
	movq	%r15, %rdi
	movq	%rax, %rsi
.LBB16_19:                              # %if.then38
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB16_22:                              # %if.else45
	movq	global_trees+88(%rip), %rsi
	movq	%r15, %rdi
	callq	convert
	movq	%rax, %r12
	movq	$60, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$60, %rdi
	movq	%r12, %rsi
	jmp	.LBB16_15
.LBB16_23:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.size_diffop, %rcx
	movq	$2051, %rsi             # imm = 0x803
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp166:
	.size	size_diffop, .Ltmp166-size_diffop
	.cfi_endproc

	.globl	non_lvalue
	.align	16, 0x90
	.type	non_lvalue,@function
non_lvalue:                             # @non_lvalue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp169:
	.cfi_def_cfa_offset 16
.Ltmp170:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %ecx
	cmpq	$115, %rcx
	jg	.LBB17_3
# BB#1:                                 # %entry
	cmpq	$29, %rcx
	ja	.LBB17_5
# BB#2:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$637534208, %rax        # imm = 0x26000000
	jne	.LBB17_6
	jmp	.LBB17_5
.LBB17_3:                               # %entry
	cmpq	$116, %rcx
	je	.LBB17_6
# BB#4:                                 # %entry
	cmpq	$121, %rcx
	je	.LBB17_6
.LBB17_5:                               # %if.end
	movq	8(%rbx), %rsi
	movq	$116, %rdi
	movq	%rbx, %rdx
	callq	build1
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %rbx
.LBB17_6:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp171:
	.size	non_lvalue, .Ltmp171-non_lvalue
	.cfi_endproc

	.globl	pedantic_non_lvalue
	.align	16, 0x90
	.type	pedantic_non_lvalue,@function
pedantic_non_lvalue:                    # @pedantic_non_lvalue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 16
.Ltmp175:
	.cfi_offset %rbx, -16
	movl	pedantic_lvalues(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB18_7
# BB#1:                                 # %if.then
	movzbl	16(%rbx), %ecx
	cmpq	$115, %rcx
	jg	.LBB18_4
# BB#2:                                 # %if.then
	cmpq	$29, %rcx
	ja	.LBB18_6
# BB#3:                                 # %if.then
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$637534208, %rax        # imm = 0x26000000
	jne	.LBB18_7
	jmp	.LBB18_6
.LBB18_4:                               # %if.then
	cmpq	$116, %rcx
	je	.LBB18_7
# BB#5:                                 # %if.then
	cmpq	$121, %rcx
	je	.LBB18_7
.LBB18_6:                               # %if.end.i
	movq	8(%rbx), %rsi
	movq	$116, %rdi
	movq	%rbx, %rdx
	callq	build1
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %rbx
.LBB18_7:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp176:
	.size	pedantic_non_lvalue, .Ltmp176-pedantic_non_lvalue
	.cfi_endproc

	.globl	operand_equal_p
	.align	16, 0x90
	.type	operand_equal_p,@function
operand_equal_p:                        # @operand_equal_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp183:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp185:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp186:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp187:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp188:
	.cfi_def_cfa_offset 64
.Ltmp189:
	.cfi_offset %rbx, -48
.Ltmp190:
	.cfi_offset %r12, -40
.Ltmp191:
	.cfi_offset %r13, -32
.Ltmp192:
	.cfi_offset %r14, -24
.Ltmp193:
	.cfi_offset %r15, -16
	movq	8(%rdi), %rax
	movq	8(%rsi), %rcx
	movl	16(%rax), %eax
	movl	16(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB19_2
# BB#1:
	xorq	%r15, %r15
.LBB19_90:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_2:                               # %while.cond.preheader.lr.ph
	movq	global_trees(%rip), %r10
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB19_3:                               # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_4 Depth 2
                                        #     Child Loop BB19_7 Depth 2
	movq	%rdx, %r9
	.align	16, 0x90
.LBB19_4:                               # %while.cond
                                        #   Parent Loop BB19_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdi, %r14
	movl	16(%r14), %r11d
	movq	%r11, %rdx
	andq	$255, %rdx
	leaq	-114(%rdx), %rax
	andq	%r13, %rax
	cmpq	$2, %rax
	ja	.LBB19_7
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB19_4 Depth=2
	movq	32(%r14), %rdi
	cmpq	%r10, %rdi
	je	.LBB19_7
# BB#6:                                 # %land.rhs
                                        #   in Loop: Header=BB19_4 Depth=2
	movq	8(%r14), %rax
	movq	8(%rdi), %rbx
	movl	60(%rax), %eax
	movl	60(%rbx), %ebx
	xorq	%rax, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB19_4
	.align	16, 0x90
.LBB19_7:                               # %while.cond45
                                        #   Parent Loop BB19_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rsi, %r12
	movl	16(%r12), %eax
	movq	%rax, %rdi
	andq	$255, %rdi
	leaq	-114(%rdi), %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB19_10
# BB#8:                                 # %land.lhs.true63
                                        #   in Loop: Header=BB19_7 Depth=2
	movq	32(%r12), %rsi
	cmpq	%r10, %rsi
	je	.LBB19_10
# BB#9:                                 # %land.rhs68
                                        #   in Loop: Header=BB19_7 Depth=2
	movq	8(%r12), %rbx
	movq	8(%rsi), %rcx
	movl	60(%rbx), %ebx
	movl	60(%rcx), %ecx
	xorq	%rbx, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB19_7
.LBB19_10:                              # %while.end92
                                        #   in Loop: Header=BB19_3 Depth=1
	cmpq	%rdi, %rdx
	jne	.LBB19_11
# BB#12:                                # %lor.lhs.false102
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	8(%r14), %rsi
	movl	16(%rsi), %r8d
	testq	$255, %r8
	movq	%r9, %rbx
	je	.LBB19_13
# BB#14:                                # %lor.lhs.false110
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	8(%r12), %rdi
	movl	16(%rdi), %r9d
	testq	$255, %r9
	je	.LBB19_15
# BB#16:                                # %lor.lhs.false118
                                        #   in Loop: Header=BB19_3 Depth=1
	movl	60(%rsi), %ecx
	movl	60(%rdi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	jne	.LBB19_17
# BB#18:                                # %if.end135
                                        #   in Loop: Header=BB19_3 Depth=1
	cmpq	%r12, %r14
	jne	.LBB19_22
# BB#19:                                # %if.end135
                                        #   in Loop: Header=BB19_3 Depth=1
	testq	%r13, %rbx
	jne	.LBB19_22
# BB#20:                                # %land.lhs.true138
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	$1, %r15
	cmpq	$118, %rdx
	je	.LBB19_90
# BB#21:                                # %lor.lhs.false144
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	%rax, %rcx
	orq	%r11, %rcx
	testq	$256, %rcx              # imm = 0x100
	je	.LBB19_90
.LBB19_22:                              # %if.end158
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	%r11, %rcx
	andq	%rax, %rcx
	testq	$512, %rcx              # imm = 0x200
	je	.LBB19_53
# BB#23:                                # %if.then171
                                        #   in Loop: Header=BB19_3 Depth=1
	cmpq	$120, %rdx
	jg	.LBB19_51
# BB#24:                                # %if.then171
                                        #   in Loop: Header=BB19_3 Depth=1
	leaq	-25(%rdx), %rsi
	andq	%r13, %rsi
	cmpq	$4, %rsi
	ja	.LBB19_53
	jmp	.LBB19_25
.LBB19_51:                              # %if.then171
                                        #   in Loop: Header=BB19_3 Depth=1
	cmpq	$121, %rdx
	je	.LBB19_52
	.align	16, 0x90
.LBB19_53:                              # %if.end282
                                        #   in Loop: Header=BB19_3 Depth=1
	testq	%r13, %rbx
	jne	.LBB19_54
# BB#55:                                # %if.end285
                                        #   in Loop: Header=BB19_3 Depth=1
	movsbq	tree_code_type(%rdx), %rsi
	cmpq	$113, %rsi
	jg	.LBB19_69
# BB#56:                                # %if.end285
                                        #   in Loop: Header=BB19_3 Depth=1
	cmpq	$100, %rsi
	jg	.LBB19_67
# BB#57:                                # %if.end285
                                        #   in Loop: Header=BB19_3 Depth=1
	andq	%r13, %rsi
	cmpq	$49, %rsi
	jne	.LBB19_58
# BB#71:                                # %sw.bb292
                                        #   in Loop: Header=BB19_3 Depth=1
	andq	$254, %r11
	cmpq	$114, %r11
	jne	.LBB19_52
# BB#72:                                # %land.lhs.true306
                                        #   in Loop: Header=BB19_3 Depth=1
	xorq	%r8, %r9
	testq	$8192, %r9              # imm = 0x2000
	je	.LBB19_52
	jmp	.LBB19_73
	.align	16, 0x90
.LBB19_69:                              # %if.end285
                                        #   in Loop: Header=BB19_3 Depth=1
	andq	%r13, %rsi
	cmpq	$114, %rsi
	jne	.LBB19_70
# BB#76:                                # %sw.bb434
                                        #   in Loop: Header=BB19_3 Depth=1
	orq	%rax, %r11
	testq	$256, %r11              # imm = 0x100
	jne	.LBB19_77
# BB#78:                                # %if.end449
                                        #   in Loop: Header=BB19_3 Depth=1
	addq	$-39, %rdx
	andq	%r13, %rdx
	xorq	%r15, %r15
	cmpq	$5, %rdx
	ja	.LBB19_90
# BB#79:                                # %if.end449
                                        #   in Loop: Header=BB19_3 Depth=1
	jmpq	*.LJTI19_1(,%rdx,8)
.LBB19_52:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	32(%r12), %rsi
	movq	32(%r14), %rdi
	movq	8(%rdi), %rax
	movq	8(%rsi), %rcx
	movl	16(%rax), %eax
	movl	16(%rcx), %ecx
	xorq	%rax, %rcx
	xorq	%rdx, %rdx
	testq	$8192, %rcx             # imm = 0x2000
	movabsq	$0, %r15
	je	.LBB19_3
	jmp	.LBB19_90
.LBB19_67:                              # %if.end285
	andq	%r13, %rsi
	cmpq	$101, %rsi
	jne	.LBB19_68
# BB#89:                                # %sw.bb512
	cmpq	$120, %rdx
	movabsq	$0, %r15
	jne	.LBB19_90
# BB#91:                                # %if.then519
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	rtx_equal_p  # TAILCALL
.LBB19_68:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_25:                              # %if.then171
	jmpq	*.LJTI19_0(,%rsi,8)
.LBB19_26:                              # %sw.bb
	orq	%r11, %rax
	xorq	%r15, %r15
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB19_90
# BB#27:                                # %land.rhs188
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_equal
	testq	%r13, %rax
	je	.LBB19_28
# BB#29:                                # %land.rhs188
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_11:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_13:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_15:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_17:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_54:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_77:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_30:                              # %sw.bb191
	orq	%r11, %rax
	xorq	%r15, %r15
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB19_90
# BB#31:                                # %land.rhs205
	addq	$32, %r14
	addq	$32, %r12
	movq	$24, %rdx
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	memcmp
	testq	%r13, %rax
	jne	.LBB19_32
# BB#33:                                # %land.rhs205
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_43:                              # %sw.bb246
	movq	32(%r14), %rdi
	movq	32(%r12), %rsi
	movq	%rbx, %rdx
	callq	operand_equal_p
	xorq	%r15, %r15
	testq	%r13, %rax
	je	.LBB19_90
# BB#44:                                # %land.rhs251
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	movq	%rbx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_45
# BB#46:                                # %land.rhs251
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_34:                              # %sw.bb213
	orq	%r11, %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB19_36
# BB#35:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_47:                              # %sw.bb259
	movl	32(%r14), %eax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movl	32(%r12), %esi
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%r15, %r15
	cmpq	%rsi, %rax
	jne	.LBB19_90
# BB#48:                                # %land.rhs263
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	callq	memcmp
	testq	%r13, %rax
	jne	.LBB19_49
# BB#50:                                # %land.rhs263
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_80:                              # %sw.bb462
	movq	32(%r14), %rdi
	movq	32(%r12), %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_90
# BB#81:                                # %land.rhs471
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_82
# BB#83:                                # %land.rhs471
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_70:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_58:                              # %if.end285
	cmpq	$50, %rsi
	je	.LBB19_60
# BB#59:                                # %if.end285
	cmpq	$60, %rsi
	movabsq	$0, %r15
	jne	.LBB19_90
.LBB19_60:                              # %sw.bb332
	movq	32(%r14), %rdi
	movq	32(%r12), %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_62
# BB#61:                                # %land.lhs.true341
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	movq	$1, %r15
	jne	.LBB19_90
.LBB19_62:                              # %if.end351
	movzbl	16(%r14), %ecx
	addq	$-59, %rcx
	andq	%r13, %rcx
	cmpq	$43, %rcx
	ja	.LBB19_75
# BB#63:                                # %if.end351
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$13195080630277, %rcx   # imm = 0xC0038180005
	testq	%rcx, %rax
	je	.LBB19_75
# BB#64:                                # %land.lhs.true414
	movq	32(%r14), %rdi
	movq	40(%r12), %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_75
# BB#65:                                # %land.rhs423
	movq	40(%r14), %rdi
	movq	32(%r12), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_66
# BB#74:                                # %land.rhs423
	movabsq	$1, %rbx
.LBB19_75:                              # %land.end432
	movq	%rbx, %r15
	jmp	.LBB19_90
.LBB19_36:                              # %if.end228
	movq	32(%r14), %r14
	testq	%r14, %r14
	movq	$1, %r15
	je	.LBB19_90
# BB#37:                                # %if.end228
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	32(%r12), %rbx
	jmp	.LBB19_38
.LBB19_42:                              # %if.end241
                                        #   in Loop: Header=BB19_38 Depth=1
	movq	(%rbx), %rbx
.LBB19_38:                              # %if.end228
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB19_90
# BB#39:                                # %while.body237
                                        #   in Loop: Header=BB19_38 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_40
# BB#41:                                # %if.end241
                                        #   in Loop: Header=BB19_38 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB19_42
	jmp	.LBB19_90
.LBB19_45:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_73:
	xorq	%r15, %r15
	jmp	.LBB19_90
.LBB19_84:                              # %sw.bb482
	movq	32(%r14), %rdi
	movq	32(%r12), %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_90
# BB#85:                                # %land.lhs.true491
	movq	40(%r14), %rdi
	movq	40(%r12), %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_90
# BB#86:                                # %land.rhs500
	movq	48(%r14), %rdi
	movq	48(%r12), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB19_87
# BB#88:                                # %land.rhs500
	movabsq	$1, %r15
	jmp	.LBB19_90
.LBB19_49:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_82:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_28:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_32:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_87:
	movabsq	$0, %r15
	jmp	.LBB19_90
.LBB19_66:
	movabsq	$0, %rbx
	movq	%rbx, %r15
	jmp	.LBB19_90
.LBB19_40:
	xorq	%r15, %r15
	jmp	.LBB19_90
.Ltmp194:
	.size	operand_equal_p, .Ltmp194-operand_equal_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI19_0:
	.quad	.LBB19_26
	.quad	.LBB19_30
	.quad	.LBB19_43
	.quad	.LBB19_34
	.quad	.LBB19_47
.LJTI19_1:
	.quad	.LBB19_80
	.quad	.LBB19_84
	.quad	.LBB19_52
	.quad	.LBB19_90
	.quad	.LBB19_80
	.quad	.LBB19_80

	.text
	.globl	invert_truthvalue
	.align	16, 0x90
	.type	invert_truthvalue,@function
invert_truthvalue:                      # @invert_truthvalue
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp200:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp201:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp202:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp203:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp204:
	.cfi_def_cfa_offset 48
.Ltmp205:
	.cfi_offset %rbx, -40
.Ltmp206:
	.cfi_offset %r12, -32
.Ltmp207:
	.cfi_offset %r14, -24
.Ltmp208:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	16(%r15), %ebx
	movq	%rbx, %rax
	andq	$255, %rax
	je	.LBB20_60
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB20_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	andq	%r12, %rdx
	movzbl	tree_code_type(%rdx), %ecx
	movq	8(%r15), %r14
	cmpq	$60, %rcx
	je	.LBB20_3
# BB#19:                                # %if.end56
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$46, %rsi
	jle	.LBB20_20
# BB#22:                                # %if.end56
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$113, %rsi
	jle	.LBB20_23
# BB#37:                                # %if.end56
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$116, %rdx
	jne	.LBB20_38
# BB#53:                                # %sw.bb166
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	32(%r15), %r15
	movl	16(%r15), %ebx
	movq	%rbx, %rax
	andq	$255, %rax
	jne	.LBB20_2
	jmp	.LBB20_60
.LBB20_20:                              # %if.end56
	cmpq	$25, %rdx
	jne	.LBB20_40
# BB#21:                                # %sw.bb
	movq	%r15, %rdi
	callq	integer_zerop
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	build_int_2_wide
	movq	%r14, %rdi
	movq	%rax, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB20_23:                              # %if.end56
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	cmpq	$75, %rsi
	jg	.LBB20_32
# BB#24:                                # %if.end56
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$50, %rax
	jg	.LBB20_28
# BB#25:                                # %if.end56
	cmpq	$47, %rdx
	jne	.LBB20_40
# BB#26:                                # %sw.bb148
	movq	32(%r15), %rbx
	movq	40(%r15), %rdi
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$47, %rdi
	jmp	.LBB20_27
.LBB20_32:                              # %if.end56
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$87, %rsi
	jg	.LBB20_35
# BB#33:                                # %if.end56
	cmpq	$76, %rdx
	jne	.LBB20_40
	jmp	.LBB20_34
.LBB20_28:                              # %if.end56
	cmpq	$51, %rdx
	je	.LBB20_52
# BB#29:                                # %if.end56
	cmpq	$56, %rdx
	jne	.LBB20_30
# BB#56:                                # %sw.bb193
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	$56, %rdi
	jmp	.LBB20_57
.LBB20_35:                              # %if.end56
	addq	$-88, %rax
	andq	%r12, %rax
	cmpq	$8, %rax
	ja	.LBB20_40
# BB#36:                                # %if.end56
	jmpq	*.LJTI20_0(,%rax,8)
.LBB20_54:                              # %sw.bb181
	movq	40(%r15), %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB20_40
# BB#55:                                # %if.end188
	movq	global_trees+88(%rip), %rsi
	movq	%r14, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$101, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	jmp	.LBB20_48
.LBB20_3:                               # %if.then6
	movq	32(%r15), %rdx
	movq	8(%rdx), %rcx
	movzbl	16(%rcx), %esi
	cmpq	$7, %rsi
	jne	.LBB20_4
# BB#9:                                 # %land.lhs.true40
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB20_10
	jmp	.LBB20_7
.LBB20_52:                              # %sw.bb135
	movq	32(%r15), %r12
	movq	40(%r15), %rdi
	callq	invert_truthvalue
	movq	48(%r15), %rdi
	movq	%rax, %r15
	callq	invert_truthvalue
	movq	%rax, %rbx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	movq	%rbx, %r8
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB20_30:                              # %if.end56
	cmpq	$58, %rdx
	jne	.LBB20_40
# BB#31:                                # %sw.bb157
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rcx
	movq	%rax, %rdx
	movq	$58, %rdi
	jmp	.LBB20_47
.LBB20_4:                               # %if.then6
	cmpq	$8, %rsi
	jne	.LBB20_7
# BB#5:                                 # %land.lhs.true
	movq	8(%rcx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB20_7
# BB#6:                                 # %land.lhs.true
	movl	flag_unsafe_math_optimizations(%rip), %ecx
	testq	%r12, %rcx
	jne	.LBB20_7
.LBB20_10:                              # %land.lhs.true40
	leaq	-101(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jae	.LBB20_11
.LBB20_7:                               # %if.else
	addq	$-97, %rax
	andq	%r12, %rax
	cmpq	$5, %rax
	ja	.LBB20_16
# BB#8:                                 # %if.else
	movq	$102, %rdi
	jmpq	*.LJTI20_1(,%rax,8)
.LBB20_14:                              # %sw.bb4.i
	movq	$100, %rdi
	movq	40(%r15), %rcx
	jmp	.LBB20_47
.LBB20_50:                              # %sw.bb111
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rdi
	movq	%rax, %rbx
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$92, %rdi
	jmp	.LBB20_27
.LBB20_51:                              # %sw.bb121
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rdi
	movq	%rax, %rbx
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$91, %rdi
	jmp	.LBB20_27
.LBB20_42:                              # %sw.bb61
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rdi
	movq	%rax, %rbx
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$94, %rdi
	jmp	.LBB20_27
.LBB20_43:                              # %sw.bb71
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rdi
	movq	%rax, %rbx
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$93, %rdi
.LBB20_27:                              # %sw.bb148
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	jmp	.LBB20_48
.LBB20_44:                              # %sw.bb81
	movq	40(%r15), %rax
	movzbl	16(%rax), %ecx
	movq	32(%r15), %rdx
	cmpq	$96, %rcx
	jne	.LBB20_49
# BB#45:                                # %if.then91
	movq	32(%rax), %rcx
	jmp	.LBB20_46
.LBB20_59:                              # %sw.bb131
	movq	32(%r15), %r15
.LBB20_60:                              # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB20_38:                              # %if.end56
	addq	$-114, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jae	.LBB20_39
.LBB20_34:                              # %sw.bb171
	movq	32(%r15), %rdi
	callq	invert_truthvalue
	andq	$255, %rbx
	movq	%rbx, %rdi
.LBB20_57:                              # %sw.bb193
	movq	%r14, %rsi
	movq	%rax, %rdx
	jmp	.LBB20_58
.LBB20_49:                              # %if.else102
	movq	%rdx, %rdi
	callq	invert_truthvalue
	movq	40(%r15), %rcx
	movq	%rax, %rdx
.LBB20_46:                              # %if.then91
	movq	$95, %rdi
	jmp	.LBB20_47
.LBB20_15:                              # %sw.bb5.i
	movq	$99, %rdi
	movq	40(%r15), %rcx
	jmp	.LBB20_47
.LBB20_12:                              # %sw.bb2.i
	movq	$98, %rdi
	movq	40(%r15), %rcx
	jmp	.LBB20_47
.LBB20_13:                              # %sw.bb3.i
	movq	$97, %rdi
	movq	40(%r15), %rcx
	jmp	.LBB20_47
.LBB20_17:                              # %sw.bb1.i
	movq	$101, %rdi
.LBB20_18:                              # %invert_tree_comparison.exit
	movq	40(%r15), %rcx
.LBB20_47:                              # %if.then91
	xorq	%rax, %rax
	movq	%r14, %rsi
.LBB20_48:                              # %if.then91
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB20_39:                              # %if.end56
	cmpq	$118, %rdx
	je	.LBB20_11
.LBB20_40:                              # %sw.epilog
	movq	8(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$11, %rax
	jne	.LBB20_41
.LBB20_11:                              # %if.then47
	movq	$96, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
.LBB20_58:                              # %sw.bb193
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB20_16:                              # %sw.default.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.invert_tree_comparison, %rdx
	movq	$2332, %rsi             # imm = 0x91C
	callq	fancy_abort
.LBB20_41:                              # %if.then207
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.invert_truthvalue, %rcx
	movq	$2934, %rsi             # imm = 0xB76
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp209:
	.size	invert_truthvalue, .Ltmp209-invert_truthvalue
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI20_0:
	.quad	.LBB20_54
	.quad	.LBB20_40
	.quad	.LBB20_40
	.quad	.LBB20_50
	.quad	.LBB20_51
	.quad	.LBB20_42
	.quad	.LBB20_43
	.quad	.LBB20_44
	.quad	.LBB20_59
.LJTI20_1:
	.quad	.LBB20_14
	.quad	.LBB20_15
	.quad	.LBB20_12
	.quad	.LBB20_13
	.quad	.LBB20_18
	.quad	.LBB20_17

	.text
	.align	16, 0x90
	.type	count_cond,@function
count_cond:                             # @count_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp215:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp216:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp217:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp218:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp219:
	.cfi_def_cfa_offset 48
.Ltmp220:
	.cfi_offset %rbx, -40
.Ltmp221:
	.cfi_offset %r12, -32
.Ltmp222:
	.cfi_offset %r14, -24
.Ltmp223:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	16(%r15), %eax
	movq	%rsi, %r14
	cmpq	$51, %rax
	jne	.LBB21_6
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB21_6
# BB#2:                                 # %if.end3
	movq	40(%r15), %rdi
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	leaq	-1(%r14), %rbx
	movq	%rbx, %rsi
	callq	count_cond
	movq	48(%r15), %rdi
	movq	%rax, %r15
	subq	%r15, %rbx
	movq	%rbx, %rsi
	callq	count_cond
	leaq	1(%r15,%rax), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%r12, %rdx
	jg	.LBB21_4
# BB#3:                                 # %if.end3
	movq	%rax, %r14
.LBB21_4:                               # %if.end3
	movq	%r14, %rax
	jmp	.LBB21_5
.LBB21_6:                               # %return
	xorq	%rax, %rax
.LBB21_5:                               # %if.end3
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp224:
	.size	count_cond, .Ltmp224-count_cond
	.cfi_endproc

	.align	16, 0x90
	.type	fold_binary_op_with_conditional_arg,@function
fold_binary_op_with_conditional_arg:    # @fold_binary_op_with_conditional_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp231:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp232:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp233:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp234:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp235:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp236:
	.cfi_def_cfa_offset 112
.Ltmp237:
	.cfi_offset %rbx, -48
.Ltmp238:
	.cfi_offset %r12, -40
.Ltmp239:
	.cfi_offset %r13, -32
.Ltmp240:
	.cfi_offset %r14, -24
.Ltmp241:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movzbl	16(%rbx), %eax
	movq	%rcx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r13
	cmpq	$51, %rax
	jne	.LBB22_10
# BB#1:                                 # %if.then2
	movq	40(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	8(%rax), %rax
	movq	32(%rbx), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movzbl	16(%rax), %eax
	movq	48(%rbx), %rcx
	cmpq	$5, %rax
	movq	%r13, %rdi
	movq	%r14, %rsi
	jne	.LBB22_5
# BB#2:                                 # %if.then16
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	movq	%r14, %rsi
	jne	.LBB22_4
# BB#3:                                 # %if.then16
	movq	global_trees+216(%rip), %rsi
.LBB22_4:                               # %if.then16
	movq	$47, %rdi
.LBB22_5:                               # %if.end20
	movq	8(%rcx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	jne	.LBB22_6
# BB#7:                                 # %if.then28
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r8
	movq	%r14, %rax
	jne	.LBB22_9
# BB#8:                                 # %if.then28
	movq	global_trees+216(%rip), %rax
.LBB22_9:                               # %if.then28
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	$47, %r13
	jmp	.LBB22_12
.LBB22_10:                              # %if.else33
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	8(%rbx), %r12
	movq	global_trees+96(%rip), %rsi
	movq	%r12, %rdi
	callq	convert
	movq	global_trees+88(%rip), %rsi
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	convert
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%r14, %rsi
	jmp	.LBB22_11
.LBB22_6:
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r8, 56(%rsp)           # 8-byte Spill
.LBB22_11:                              # %if.end37
	movq	%r14, 16(%rsp)          # 8-byte Spill
.LBB22_12:                              # %if.end37
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	xorq	%r12, %r12
	cmpq	$118, %rcx
	je	.LBB22_35
# BB#13:                                # %if.end37
	andq	$512, %rax              # imm = 0x200
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB22_35
# BB#14:                                # %land.lhs.true47
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	callq	global_bindings_p
	testq	%rbx, %rax
	jne	.LBB22_15
# BB#16:                                # %land.lhs.true50
	movl	16(%r15), %eax
	andq	$510, %rax              # imm = 0x1FE
	cmpq	$34, %rax
	je	.LBB22_15
# BB#17:                                # %if.then67
	movq	48(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	xorq	%r12, %r12
	cmpq	$51, %rax
	movabsq	$0, %r13
	je	.LBB22_23
# BB#18:                                # %if.then73
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	movq	48(%rsp), %r8           # 8-byte Reload
	movq	%r8, %rdx
	jne	.LBB22_20
# BB#19:                                # %if.then73
	movq	%r15, %rdx
.LBB22_20:                              # %if.then73
	testq	%rax, %rax
	movq	%r15, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
	jne	.LBB22_22
# BB#21:                                # %if.then73
	movq	%r8, %rcx
.LBB22_22:                              # %if.then73
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
.LBB22_23:                              # %if.end76
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$51, %rax
	je	.LBB22_29
# BB#24:                                # %if.then82
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%rax, %rbx
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdx
	jne	.LBB22_26
# BB#25:                                # %if.then82
	movq	%r15, %rdx
.LBB22_26:                              # %if.then82
	testq	%rbx, %rbx
	movq	%r15, %rcx
	jne	.LBB22_28
# BB#27:                                # %if.then82
	movq	%rax, %rcx
.LBB22_28:                              # %if.then82
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
.LBB22_29:                              # %if.end85
	testq	%r13, %r13
	je	.LBB22_31
# BB#30:                                # %lor.lhs.false87
	movzbl	17(%r13), %eax
	testq	$2, %rax
	jne	.LBB22_40
.LBB22_31:                              # %land.lhs.true94
	testq	%r12, %r12
	je	.LBB22_33
# BB#32:                                # %lor.lhs.false96
	movzbl	17(%r12), %eax
	testq	$2, %rax
	je	.LBB22_33
# BB#34:                                # %if.end106
	testq	%r13, %r13
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
	jne	.LBB22_40
	jmp	.LBB22_35
.LBB22_33:                              # %if.then103
	movq	%r15, %rdi
	callq	save_expr
	movq	%rax, %r15
	xorq	%r12, %r12
.LBB22_15:
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
.LBB22_35:                              # %if.then108
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rax
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	jne	.LBB22_37
# BB#36:                                # %if.then108
	movq	%r15, %rdx
.LBB22_37:                              # %if.then108
	testq	%rax, %rax
	movq	%r15, %rcx
	jne	.LBB22_39
# BB#38:                                # %if.then108
	movq	%rbx, %rcx
.LBB22_39:                              # %if.then108
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r13
.LBB22_40:                              # %if.end111
	testq	%r12, %r12
	jne	.LBB22_46
# BB#41:                                # %if.then113
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rdx
	jne	.LBB22_43
# BB#42:                                # %if.then113
	movq	%r15, %rdx
.LBB22_43:                              # %if.then113
	testq	%rcx, %rcx
	movq	%r15, %rcx
	jne	.LBB22_45
# BB#44:                                # %if.then113
	movq	%rax, %rcx
.LBB22_45:                              # %if.then113
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r12
.LBB22_46:                              # %if.end116
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r12, %r8
	callq	build
	movq	%rax, %rdi
	callq	fold
	movzbl	16(%r15), %ecx
	movq	%rax, %rbx
	cmpq	$118, %rcx
	jne	.LBB22_48
# BB#47:                                # %if.then124
	movq	global_trees+216(%rip), %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strip_compound_expr
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB22_48:                              # %if.else128
	movq	%r14, %rdi
	movq	%rbx, %rsi
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.Ltmp242:
	.size	fold_binary_op_with_conditional_arg, .Ltmp242-fold_binary_op_with_conditional_arg
	.cfi_endproc

	.align	16, 0x90
	.type	negate_expr,@function
negate_expr:                            # @negate_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp246:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp247:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp248:
	.cfi_def_cfa_offset 32
.Ltmp249:
	.cfi_offset %rbx, -24
.Ltmp250:
	.cfi_offset %r14, -16
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB23_12
# BB#1:                                 # %if.end
	movq	8(%rdi), %r14
	movq	global_trees(%rip), %r9
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r14, %rax
	.align	16, 0x90
.LBB23_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %ecx
	leaq	-114(%rcx), %rsi
	andq	%r8, %rsi
	cmpq	$2, %rsi
	ja	.LBB23_6
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB23_2 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r9, %rdi
	je	.LBB23_6
# BB#4:                                 # %land.lhs.true16
                                        #   in Loop: Header=BB23_2 Depth=1
	movq	8(%rdi), %rsi
	movl	60(%rax), %r10d
	movl	60(%rsi), %edx
	xorq	%r10, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	jne	.LBB23_6
# BB#5:                                 # %land.rhs
                                        #   in Loop: Header=BB23_2 Depth=1
	movl	16(%rax), %eax
	movl	16(%rsi), %edx
	xorq	%rax, %rdx
	testq	$8192, %rdx             # imm = 0x2000
	movq	%rsi, %rax
	je	.LBB23_2
.LBB23_6:                               # %while.end
	cmpq	$77, %rcx
	je	.LBB23_13
# BB#7:                                 # %while.end
	cmpq	$60, %rcx
	je	.LBB23_15
# BB#8:                                 # %while.end
	addq	$-25, %rcx
	andq	%r8, %rcx
	cmpq	$1, %rcx
	ja	.LBB23_19
# BB#9:                                 # %sw.bb
	movzbl	17(%r14), %eax
	testq	$32, %rax
	jne	.LBB23_19
# BB#10:                                # %land.lhs.true62
	movq	$77, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	testq	%rax, %rax
	je	.LBB23_19
# BB#11:                                # %land.lhs.true65
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	jne	.LBB23_19
.LBB23_12:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB23_13:                              # %sw.bb73
	movq	32(%rbx), %rsi
	movq	%r14, %rdi
	jmp	.LBB23_14
.LBB23_15:                              # %sw.bb78
	movzbl	16(%r14), %eax
	cmpq	$7, %rax
	jne	.LBB23_16
# BB#20:                                # %lor.lhs.false98
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%rax, %rax
	jne	.LBB23_21
	jmp	.LBB23_19
.LBB23_16:                              # %sw.bb78
	cmpq	$8, %rax
	jne	.LBB23_21
# BB#17:                                # %land.lhs.true90
	movq	8(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	jne	.LBB23_21
# BB#18:                                # %land.lhs.true90
	movl	flag_unsafe_math_optimizations(%rip), %eax
	testq	%r8, %rax
	jne	.LBB23_21
.LBB23_19:                              # %sw.epilog
	movq	8(%rbx), %rsi
	movq	$77, %rdi
	movq	%rbx, %rdx
	callq	build1
	jmp	.LBB23_22
.LBB23_21:                              # %if.then100
	movq	40(%rbx), %rdx
	movq	8(%rbx), %rsi
	movq	32(%rbx), %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	callq	build
.LBB23_22:                              # %sw.epilog
	movq	%rax, %rdi
	callq	fold
	movq	%r14, %rdi
	movq	%rax, %rsi
.LBB23_14:                              # %sw.bb73
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	convert  # TAILCALL
.Ltmp251:
	.size	negate_expr, .Ltmp251-negate_expr
	.cfi_endproc

	.align	16, 0x90
	.type	const_binop,@function
const_binop:                            # @const_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp258:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp259:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp260:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp261:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 48
	subq	$176, %rsp
.Ltmp263:
	.cfi_def_cfa_offset 224
.Ltmp264:
	.cfi_offset %rbx, -48
.Ltmp265:
	.cfi_offset %r12, -40
.Ltmp266:
	.cfi_offset %r13, -32
.Ltmp267:
	.cfi_offset %r14, -24
.Ltmp268:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rcx
	movq	%rdi, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB24_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r15
	movzbl	16(%r15), %eax
	leaq	-114(%rax), %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB24_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	32(%r15), %rsi
	cmpq	%rcx, %rsi
	je	.LBB24_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	8(%r15), %rdi
	movq	8(%rsi), %rbx
	movl	60(%rdi), %edi
	movl	60(%rbx), %ebx
	xorq	%rdi, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB24_1
	.align	16, 0x90
.LBB24_4:                               # %while.cond32
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rbx
	movzbl	16(%rbx), %edx
	addq	$-114, %rdx
	andq	%r13, %rdx
	cmpq	$2, %rdx
	ja	.LBB24_7
# BB#5:                                 # %land.lhs.true50
                                        #   in Loop: Header=BB24_4 Depth=1
	movq	32(%rbx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB24_7
# BB#6:                                 # %land.rhs55
                                        #   in Loop: Header=BB24_4 Depth=1
	movq	8(%rbx), %rsi
	movq	8(%rdx), %rdi
	movl	60(%rsi), %esi
	movl	60(%rdi), %edi
	xorq	%rsi, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB24_4
.LBB24_7:                               # %while.end79
	cmpq	$27, %rax
	je	.LBB24_20
# BB#8:                                 # %while.end79
	cmpq	$26, %rax
	jne	.LBB24_9
# BB#11:                                # %if.then90
	movq	48(%r15), %rax
	movq	%rax, 168(%rsp)
	movq	32(%r15), %rax
	movq	40(%r15), %rcx
	movq	%rcx, 160(%rsp)
	movq	%rax, 152(%rsp)
	movq	48(%rbx), %rax
	movq	%rax, 144(%rsp)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rcx
	movq	%rcx, 136(%rsp)
	movq	%rax, 128(%rsp)
	movq	168(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	152(%rsp), %rax
	movq	160(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%r13, %rax
	je	.LBB24_13
# BB#12:
	movq	%r15, %r14
	jmp	.LBB24_19
.LBB24_20:                              # %if.then158
	movq	8(%r15), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	32(%r15), %r14
	movq	40(%r15), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	32(%rbx), %r15
	movq	40(%rbx), %rbx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	andq	%r13, %r12
	cmpq	$69, %rax
	jg	.LBB24_29
# BB#21:                                # %if.then158
	cmpq	$59, %r12
	je	.LBB24_25
# BB#22:                                # %if.then158
	cmpq	$60, %r12
	jne	.LBB24_23
# BB#28:                                # %sw.bb171
	movq	$60, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %r14
	movq	$60, %rdi
	jmp	.LBB24_26
.LBB24_9:                               # %while.end79
	xorq	%r14, %r14
	cmpq	$25, %rax
	jne	.LBB24_19
# BB#10:                                # %if.then
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	int_const_binop
	movq	%rax, %r14
	jmp	.LBB24_19
.LBB24_29:                              # %if.then158
	cmpq	$70, %r12
	movq	%rbx, %r13
	jne	.LBB24_43
# BB#30:                                # %sw.bb183
	movq	$61, %rdi
	movq	%r15, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	$61, %rdi
	movq	%r13, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	8(%r14), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	cmpq	$11, %rcx
	ja	.LBB24_32
# BB#31:                                # %sw.bb183
	movq	$62, %r12
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3136, %rax             # imm = 0xC40
	jne	.LBB24_36
.LBB24_32:                              # %lor.rhs
	cmpq	$12, %rcx
	jne	.LBB24_33
# BB#34:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB24_35
.LBB24_13:                              # %if.else
	movq	144(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	128(%rsp), %rax
	movq	136(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	target_isnan
	testq	%r13, %rax
	je	.LBB24_15
# BB#14:
	movq	%rbx, %r14
	jmp	.LBB24_19
.LBB24_25:                              # %sw.bb
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %r14
	movq	$59, %rdi
.LBB24_26:                              # %return
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	const_binop
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	jmp	.LBB24_27
.LBB24_23:                              # %if.then158
	cmpq	$61, %r12
	jne	.LBB24_43
# BB#24:                                # %sw.bb175
	movq	$61, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %r12
	movq	$61, %rdi
	movq	48(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	$60, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %r12
	movq	$61, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	$61, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	jmp	.LBB24_27
.LBB24_33:
	movabsq	$0, %rax
.LBB24_35:                              # %lor.rhs
	xorq	$1, %rax
	leaq	62(,%rax,8), %r12
.LBB24_36:                              # %lor.end
	movq	$61, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	$61, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	const_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	8(%r14), %rcx
	movzbl	16(%rcx), %ecx
	movq	%rax, 24(%rsp)          # 8-byte Spill
	cmpq	$11, %rcx
	ja	.LBB24_38
# BB#37:                                # %lor.end
	movq	$62, %r12
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3136, %rax             # imm = 0xC40
	jne	.LBB24_42
.LBB24_38:                              # %lor.rhs244
	cmpq	$12, %rcx
	jne	.LBB24_39
# BB#40:                                # %lor.rhs244
	movabsq	$1, %rax
	jmp	.LBB24_41
.LBB24_39:
	movabsq	$0, %rax
.LBB24_41:                              # %lor.rhs244
	xorq	$1, %rax
	leaq	62(,%rax,8), %r12
.LBB24_42:                              # %lor.end252
	movq	$61, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	$61, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	$60, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
.LBB24_27:                              # %return
	movq	%rax, %rdx
	callq	build_complex
	movq	%rax, %r14
.LBB24_19:                              # %return
	movq	%r14, %rax
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_15:                              # %if.end100
	movq	8(%r15), %rax
	movq	%rax, 64(%rsp)
	movq	168(%rsp), %rax
	movq	%rax, 88(%rsp)
	movq	152(%rsp), %rax
	movq	160(%rsp), %rcx
	movq	%rcx, 80(%rsp)
	movq	%rax, 72(%rsp)
	movq	144(%rsp), %rax
	movq	128(%rsp), %rcx
	movq	136(%rsp), %rdx
	movq	%rax, 112(%rsp)
	movq	%rdx, 104(%rsp)
	movq	%rcx, 96(%rsp)
	movl	%r12d, 56(%rsp)
	movabsq	$const_binop_1, %rdi
	leaq	56(%rsp), %rsi
	callq	do_float_handler
	testq	%r13, %rax
	je	.LBB24_17
# BB#16:                                # %if.then109
	movq	120(%rsp), %r14
	xorq	%rsi, %rsi
	jmp	.LBB24_18
.LBB24_17:                              # %if.else111
	movq	%r15, %rdi
	callq	copy_node
	movq	%rax, %r14
	movq	$1, %rsi
.LBB24_18:                              # %if.end113
	movq	%r14, %rdi
	callq	force_fit_type
	movl	16(%r15), %edx
	movq	$19, %rcx
	shrq	%cl, %rdx
	orq	%rax, %rdx
	movl	16(%rbx), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	orq	%rdx, %rax
	movl	16(%r14), %edx
	movq	%rax, %rcx
	andq	$1, %rcx
	shlq	$19, %rcx
	andq	$-524289, %rdx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rcx, %rdx
	movl	%edx, 16(%r14)
	movl	16(%r15), %esi
	movl	16(%rbx), %edi
	movq	$18, %rcx
	shlq	%cl, %rax
	orq	%rsi, %rax
	orq	%rdi, %rax
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294705151, %rcx       # imm = 0xFFFBFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, 16(%r14)
	jmp	.LBB24_19
.LBB24_43:                              # %sw.default
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.const_binop, %rcx
	movq	$1908, %rsi             # imm = 0x774
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp269:
	.size	const_binop, .Ltmp269-const_binop
	.cfi_endproc

	.align	16, 0x90
	.type	split_tree,@function
split_tree:                             # @split_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp276:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp277:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp278:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp279:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp280:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp281:
	.cfi_def_cfa_offset 64
.Ltmp282:
	.cfi_offset %rbx, -48
.Ltmp283:
	.cfi_offset %r12, -40
.Ltmp284:
	.cfi_offset %r13, -32
.Ltmp285:
	.cfi_offset %r14, -24
.Ltmp286:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r14
	movq	$0, (%r14)
	movq	$0, (%r13)
	movq	$0, (%r8)
	movq	global_trees(%rip), %r11
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB25_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %r15
	movl	16(%r15), %r9d
	movq	%r9, %rax
	andq	$255, %rax
	leaq	-114(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB25_5
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	32(%r15), %rdi
	cmpq	%r11, %rdi
	je	.LBB25_5
# BB#3:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	8(%r15), %rdx
	movq	8(%rdi), %rcx
	movl	60(%rdx), %r10d
	movl	60(%rcx), %ebx
	xorq	%r10, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	jne	.LBB25_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB25_1 Depth=1
	movl	16(%rdx), %edx
	movl	16(%rcx), %ecx
	xorq	%rdx, %rcx
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB25_1
.LBB25_5:                               # %while.end
	leaq	-25(%rax), %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	ja	.LBB25_7
# BB#6:                                 # %if.then
	movq	%r15, (%r13)
	xorq	%r15, %r15
	jmp	.LBB25_48
.LBB25_7:                               # %if.else
	andq	%r12, %rsi
	cmpq	%rsi, %rax
	je	.LBB25_18
# BB#8:                                 # %lor.lhs.false66
	movq	8(%r15), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$7, %rcx
	je	.LBB25_14
# BB#9:                                 # %lor.lhs.false66
	cmpq	$8, %rcx
	jne	.LBB25_11
# BB#10:                                # %land.lhs.true82
	movq	8(%rdx), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	je	.LBB25_14
.LBB25_11:                              # %land.lhs.true92
	cmpq	$60, %rsi
	jne	.LBB25_12
# BB#17:                                # %land.lhs.true102
	cmpq	$59, %rax
	jne	.LBB25_14
	jmp	.LBB25_18
.LBB25_12:                              # %land.lhs.true92
	cmpq	$59, %rsi
	jne	.LBB25_14
# BB#13:                                # %land.lhs.true94
	cmpq	$60, %rax
	jne	.LBB25_14
.LBB25_18:                              # %if.then108
	movq	32(%r15), %rdi
	movq	40(%r15), %rcx
	cmpq	$60, %rax
	jne	.LBB25_19
# BB#20:                                # %if.then108
	movabsq	$1, %r9
	jmp	.LBB25_21
.LBB25_14:                              # %if.else194
	testq	$512, %r9               # imm = 0x200
	je	.LBB25_48
# BB#15:                                # %if.then201
	movq	%r15, (%r14)
	xorq	%r15, %r15
	jmp	.LBB25_48
.LBB25_19:
	movabsq	$0, %r9
.LBB25_21:                              # %if.then108
	movzbl	16(%rdi), %eax
	addq	$-25, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	ja	.LBB25_23
# BB#22:                                # %if.end149.thread
	movq	%rdi, (%r13)
	xorq	%rdx, %rdx
	xorq	%rsi, %rsi
	jmp	.LBB25_29
.LBB25_23:                              # %if.else134
	movzbl	16(%rcx), %eax
	addq	$-25, %rax
	andq	%r12, %rax
	xorq	%rsi, %rsi
	cmpq	$1, %rax
	ja	.LBB25_24
# BB#25:                                # %if.then148
	movq	%rcx, (%r13)
	xorq	%rcx, %rcx
	movq	%r9, %rdx
	jmp	.LBB25_26
.LBB25_24:
	xorq	%rdx, %rdx
.LBB25_26:                              # %if.end149
	testq	%rdi, %rdi
	je	.LBB25_29
# BB#27:                                # %land.lhs.true152
	movzbl	17(%rdi), %eax
	testq	$2, %rax
	je	.LBB25_28
# BB#56:                                # %if.then157
	movq	%rdi, (%r14)
	xorq	%rsi, %rsi
	xorq	%rdi, %rdi
	jmp	.LBB25_33
.LBB25_28:
	movq	%rdi, %rsi
.LBB25_29:                              # %if.else158
	xorq	%rdi, %rdi
	testq	%rcx, %rcx
	je	.LBB25_30
# BB#31:                                # %land.lhs.true161
	movzbl	17(%rcx), %eax
	testq	$2, %rax
	je	.LBB25_36
# BB#32:                                # %if.then168
	movq	%rcx, (%r14)
	xorq	%rcx, %rcx
	movq	%r9, %rdi
	jmp	.LBB25_33
.LBB25_30:
	xorq	%rcx, %rcx
.LBB25_33:                              # %if.end170.thread
	movq	%r14, %rbx
	testq	%rsi, %rsi
	je	.LBB25_34
# BB#35:                                # %if.end170.thread
	movabsq	$1, %r14
	jmp	.LBB25_37
.LBB25_34:
	movabsq	$0, %r14
.LBB25_37:                              # %if.else177
	xorq	%rax, %rax
	testq	%r14, %r14
	movq	%rsi, %r15
	jne	.LBB25_39
# BB#38:                                # %if.else177
	movq	%rcx, %r15
.LBB25_39:                              # %if.else177
	jne	.LBB25_41
# BB#40:                                # %if.else177
	movq	%r9, %rax
.LBB25_41:                              # %if.else177
	movq	%rax, %r14
.LBB25_42:                              # %if.end183
	testq	%rdx, %rdx
	je	.LBB25_44
# BB#43:                                # %if.then185
	movq	(%r13), %rax
	movq	%rax, (%r8)
	movq	$0, (%r13)
.LBB25_44:                              # %if.end186
	movq	%r8, (%rsp)             # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB25_46
# BB#45:                                # %if.then188
	movq	(%rbx), %rdi
	callq	negate_expr
	movq	%rax, (%rbx)
.LBB25_46:                              # %if.end189
	testq	%r12, %r14
	je	.LBB25_47
# BB#51:                                # %if.then191
	movq	%r15, %rdi
	callq	negate_expr
	movq	%rax, %r15
.LBB25_47:
	movq	%rbx, %r14
	movq	(%rsp), %r8             # 8-byte Reload
.LBB25_48:                              # %if.end205
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r12, %rax
	je	.LBB25_55
# BB#49:                                # %if.then207
	movq	(%r13), %rax
	testq	%rax, %rax
	je	.LBB25_52
# BB#50:                                # %if.then209
	movq	%rax, (%r8)
	movq	$0, (%r13)
	jmp	.LBB25_54
.LBB25_55:                              # %if.end217
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB25_52:                              # %if.else210
	movq	(%r8), %rax
	testq	%rax, %rax
	je	.LBB25_54
# BB#53:                                # %if.then212
	movq	%rax, (%r13)
	movq	$0, (%r8)
.LBB25_54:                              # %if.end214
	movq	(%r14), %rdi
	callq	negate_expr
	movq	%rax, (%r14)
	movq	%r15, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	negate_expr  # TAILCALL
.LBB25_36:                              # %if.end170
	movq	%r14, %rbx
	xorq	%r14, %r14
	testq	%rsi, %rsi
	movabsq	$0, %rdi
	movabsq	$0, %rsi
	jne	.LBB25_42
	jmp	.LBB25_37
.Ltmp287:
	.size	split_tree, .Ltmp287-split_tree
	.cfi_endproc

	.align	16, 0x90
	.type	associate_trees,@function
associate_trees:                        # @associate_trees
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp294:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp295:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp296:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp297:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp298:
	.cfi_def_cfa_offset 48
.Ltmp299:
	.cfi_offset %rbx, -48
.Ltmp300:
	.cfi_offset %r12, -40
.Ltmp301:
	.cfi_offset %r13, -32
.Ltmp302:
	.cfi_offset %r14, -24
.Ltmp303:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB26_3
# BB#1:                                 # %if.else
	testq	%r12, %r12
	je	.LBB26_2
# BB#4:                                 # %if.end3
	movzbl	16(%rbx), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r14, %rdx
	cmpq	%rdx, %rax
	je	.LBB26_8
# BB#5:                                 # %lor.lhs.false
	movzbl	16(%r12), %ecx
	cmpq	%rdx, %rcx
	je	.LBB26_8
# BB#6:                                 # %lor.lhs.false
	cmpq	$60, %rax
	je	.LBB26_8
# BB#7:                                 # %lor.lhs.false
	cmpq	$60, %rcx
	je	.LBB26_8
# BB#15:                                # %if.end49
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB26_8:                               # %if.then23
	cmpq	$77, %rax
	jne	.LBB26_10
# BB#9:                                 # %if.then29
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	32(%rbx), %rsi
	movq	%rax, %rbx
	jmp	.LBB26_12
.LBB26_2:
	movq	%rbx, %r12
.LBB26_3:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB26_10:                              # %if.else32
	movzbl	16(%r12), %r13d
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	cmpq	$77, %r13
	jne	.LBB26_14
# BB#11:                                # %if.then38
	movq	32(%r12), %rsi
.LBB26_12:                              # %if.then38
	movq	%r15, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	jmp	.LBB26_13
.LBB26_14:                              # %if.else45
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
.LBB26_13:                              # %if.then38
	movq	%r15, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.Ltmp304:
	.size	associate_trees, .Ltmp304-associate_trees
	.cfi_endproc

	.align	16, 0x90
	.type	extract_muldiv,@function
extract_muldiv:                         # @extract_muldiv
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp311:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp312:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp313:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp314:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp315:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp316:
	.cfi_def_cfa_offset 128
.Ltmp317:
	.cfi_offset %rbx, -48
.Ltmp318:
	.cfi_offset %r12, -40
.Ltmp319:
	.cfi_offset %r13, -32
.Ltmp320:
	.cfi_offset %r14, -24
.Ltmp321:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB27_1
.LBB27_67:                              # %if.then313
                                        #   in Loop: Header=BB27_1 Depth=1
	xorq	$1, %rax
	leaq	61(%rax,%rax,2), %r14
	movq	%r13, %r15
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %r15
	movq	72(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r8           # 8-byte Reload
	.align	16, 0x90
.LBB27_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r15), %rdi
	movzbl	16(%r15), %r14d
	testq	%r8, %r8
	je	.LBB27_3
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB27_1 Depth=1
	movl	60(%r8), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	movl	60(%rdi), %edx
	andq	$127, %rax
	movzbl	mode_size(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %ecx
	cmpq	%rcx, %rax
	movq	%r8, %r13
	ja	.LBB27_4
.LBB27_3:                               # %cond.false
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	%rdi, %r13
.LBB27_4:                               # %cond.end
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	%rsi, %rdi
	callq	integer_zerop
	xorq	%r12, %r12
	testq	%rbx, %rax
	jne	.LBB27_113
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	%r14, %rcx
	movzbl	tree_code_type(%rcx), %eax
	cmpq	$50, %rax
	jne	.LBB27_6
# BB#9:                                 # %if.then32
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	32(%r15), %rdx
	movq	40(%r15), %r8
	jmp	.LBB27_10
	.align	16, 0x90
.LBB27_6:                               # %if.end
                                        #   in Loop: Header=BB27_1 Depth=1
	xorq	%r8, %r8
	cmpq	$49, %rax
	jne	.LBB27_7
# BB#8:                                 # %if.end26.thread
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	32(%r15), %rdx
	jmp	.LBB27_10
	.align	16, 0x90
.LBB27_7:                               #   in Loop: Header=BB27_1 Depth=1
	xorq	%rdx, %rdx
.LBB27_10:                              # %if.end39
                                        #   in Loop: Header=BB27_1 Depth=1
	cmpq	$57, %rcx
	jle	.LBB27_11
# BB#15:                                # %if.end39
                                        #   in Loop: Header=BB27_1 Depth=1
	cmpq	$113, %rcx
	jg	.LBB27_18
# BB#16:                                # %if.end39
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	leaq	-58(%rcx), %rax
	movq	%rcx, %r12
	andq	%rbx, %rax
	cmpq	$25, %rax
	ja	.LBB27_40
# BB#17:                                # %if.end39
                                        #   in Loop: Header=BB27_1 Depth=1
	jmpq	*.LJTI27_0(,%rax,8)
.LBB27_60:                              # %sw.bb280
                                        #   in Loop: Header=BB27_1 Depth=1
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	jne	.LBB27_40
# BB#61:                                # %land.lhs.true287
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	global_trees+128(%rip), %rsi
	movq	8(%rsi), %rax
	movl	60(%rax), %eax
	movq	32(%r8), %rcx
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rcx, %rax
	jbe	.LBB27_40
# BB#62:                                # %land.lhs.true297
                                        #   in Loop: Header=BB27_1 Depth=1
	cmpq	$0, 40(%r8)
	jne	.LBB27_40
# BB#63:                                # %land.lhs.true302
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	$82, %rdi
	movq	%r8, %rdx
	callq	const_binop
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB27_40
# BB#64:                                # %land.lhs.true307
                                        #   in Loop: Header=BB27_1 Depth=1
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	jne	.LBB27_40
# BB#65:                                # %if.then313
                                        #   in Loop: Header=BB27_1 Depth=1
	cmpq	$82, %r12
	movabsq	$0, %rax
	jne	.LBB27_67
# BB#66:                                # %if.then313
                                        #   in Loop: Header=BB27_1 Depth=1
	movabsq	$1, %rax
	jmp	.LBB27_67
.LBB27_11:                              # %if.end39
	cmpq	$25, %rcx
	jne	.LBB27_40
# BB#12:                                # %sw.bb
	movq	64(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	cmpq	$61, %rax
	je	.LBB27_14
# BB#13:                                # %lor.lhs.false
	movq	$66, %rdi
	movq	%r15, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB27_40
.LBB27_14:                              # %if.then45
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdx
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	const_binop  # TAILCALL
.LBB27_18:                              # %if.end39
	leaq	-114(%rcx), %rax
	andq	%rbx, %rax
	cmpq	$3, %rax
	jae	.LBB27_19
# BB#28:                                # %sw.bb50
	movzbl	16(%rdx), %eax
	movzbl	tree_code_type(%rax), %ecx
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB27_30
# BB#29:                                # %sw.bb50
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rax
	je	.LBB27_30
# BB#31:                                # %land.lhs.true90
	movq	8(%rdx), %rdi
	movl	16(%rdi), %ecx
	movq	%rdx, %r14
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB27_32
# BB#33:                                # %land.lhs.true98
	movl	60(%rdi), %eax
	andq	$255, %rcx
	cmpq	$6, %rcx
	jne	.LBB27_35
# BB#34:                                # %land.lhs.true98
	movq	%rax, %rcx
	andq	$131072, %rcx           # imm = 0x20000
	testq	%rbx, %rcx
	jne	.LBB27_36
.LBB27_35:                              # %land.lhs.true115
	movl	60(%r13), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$9, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	andq	$127, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rcx, %rdx
	ja	.LBB27_40
	jmp	.LBB27_36
.LBB27_30:                              # %sw.bb50.if.end158_crit_edge
	movq	8(%rdx), %rdi
	movq	%rdx, %r14
	jmp	.LBB27_37
.LBB27_19:                              # %if.end39
	cmpq	$118, %rcx
	jne	.LBB27_40
# BB#20:                                # %sw.bb233
	cmpq	$0, 48(%r15)
	jne	.LBB27_40
# BB#21:                                # %land.lhs.true239
	movq	32(%r15), %rdi
	movzbl	17(%rdi), %eax
	testq	$1, %rax
	jne	.LBB27_40
# BB#22:                                # %land.lhs.true248
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	testq	%rax, %rax
	je	.LBB27_40
# BB#23:                                # %if.then255
	movq	%rax, %rdi
	callq	save_expr
	movzbl	17(%r15), %ecx
	movq	%rax, %r12
	testq	$64, %rcx
	je	.LBB27_26
# BB#24:                                # %land.lhs.true262
	movl	16(%r12), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$118, %rcx
	jne	.LBB27_26
# BB#25:                                # %if.then269
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%r12)
.LBB27_26:                              # %if.end274
	movq	%r15, %rdi
	callq	is_pending_size
	testq	%rbx, %rax
	je	.LBB27_113
# BB#27:                                # %if.then277
	movq	%r12, %rdi
	callq	put_pending_size
	jmp	.LBB27_113
.LBB27_32:                              # %land.lhs.true90.lor.lhs.false136_crit_edge
	movl	60(%rdi), %eax
.LBB27_36:                              # %lor.lhs.false136
	movl	60(%r13), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movzbl	mode_size(%rdx), %edx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_size(%rax), %eax
	cmpq	%rax, %rdx
	jb	.LBB27_40
.LBB27_37:                              # %if.end158
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	64(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rbx
	cmpq	$61, %rbx
	movq	%r13, %rcx
	je	.LBB27_39
# BB#38:                                # %if.end158
	xorq	%rcx, %rcx
.LBB27_39:                              # %if.end158
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	extract_muldiv
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB27_113
	jmp	.LBB27_40
.LBB27_114:                             # %sw.bb489.loopexit
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	%r12, %rdx
	cmpq	%rax, %rdx
	jne	.LBB27_116
# BB#115:
	movq	%r8, %r15
	movq	%r13, %rbx
	movq	72(%rsp), %r12          # 8-byte Reload
.LBB27_121:                             # %land.lhs.true491
	movq	%rdx, %r13
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	testq	%rax, %rax
	je	.LBB27_122
# BB#151:                               # %if.then495
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	jmp	.LBB27_54
.LBB27_116:                             # %if.else500.thread
	cmpq	$61, %rax
	jne	.LBB27_117
# BB#118:                               # %if.else500.thread
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB27_119
.LBB27_68:                              # %sw.bb321
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	callq	extract_muldiv
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rbx
	movq	%rbx, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	movq	%rax, %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %r14
	je	.LBB27_74
# BB#69:                                # %sw.bb321
	testq	%r15, %r15
	je	.LBB27_74
# BB#70:                                # %land.lhs.true329
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$61, %rax
	je	.LBB27_73
# BB#71:                                # %lor.lhs.false332
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	multiple_of_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB27_73
# BB#72:                                # %lor.lhs.false335
	movq	%r13, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	multiple_of_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB27_74
.LBB27_73:                              # %if.then338
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %r14
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r13, %rsi
	jmp	.LBB27_55
.LBB27_41:                              # %sw.bb173
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	testq	%rax, %rax
	je	.LBB27_40
# BB#42:                                # %if.then177
	movq	%r13, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build1
	jmp	.LBB27_57
.LBB27_43:                              # %sw.bb182
	movl	16(%r13), %eax
	movq	48(%rsp), %rcx          # 8-byte Reload
	movl	16(%rcx), %ecx
	xorq	%rax, %rcx
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB27_40
# BB#44:                                # %if.end196
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	callq	extract_muldiv
	movq	%rbx, %rdi
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB27_40
# BB#45:                                # %land.lhs.true200
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB27_40
# BB#46:                                # %if.then204
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB27_48
# BB#47:
	movq	%r13, %rbx
	jmp	.LBB27_52
.LBB27_117:
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
.LBB27_119:                             # %if.else500.thread
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	xorq	%rcx, %rcx
	jmp	.LBB27_131
.LBB27_122:
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	movq	%r13, %rdx
	jmp	.LBB27_123
.LBB27_74:                              # %if.end343
	movq	%rbx, %r15
	cmpq	$60, %r12
	jne	.LBB27_75
# BB#76:                                # %if.then346
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	negate_expr
	movq	$59, 32(%rsp)           # 8-byte Folded Spill
	jmp	.LBB27_77
.LBB27_58:                              # %sw.bb218
	movq	32(%r15), %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	extract_muldiv
	movq	%rax, %rdx
	testq	%rdx, %rdx
	je	.LBB27_40
# BB#59:                                # %if.then225
	movq	8(%rdx), %rsi
	movq	40(%r15), %rcx
	movq	$58, %rdi
	xorq	%rax, %rax
.LBB27_106:                             # %if.then.i
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB27_101:                             # %sw.bb460
	movq	64(%rsp), %r14          # 8-byte Reload
	leaq	-66(%r14), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$3, %rax
	movq	72(%rsp), %r12          # 8-byte Reload
	ja	.LBB27_120
# BB#102:                               # %land.lhs.true472
	movq	40(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB27_120
# BB#103:                               # %land.lhs.true482
	movq	$66, %rdi
	movq	%r8, %rsi
	movq	%r12, %rdx
	movq	%r8, %r15
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movq	%r15, %r8
	testq	%rbx, %rax
	je	.LBB27_120
# BB#104:                               # %if.then486
	movq	global_trees+88(%rip), %rsi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	convert
	movq	40(%rsp), %rdx          # 8-byte Reload
	movzbl	17(%rdx), %ecx
	movq	%rax, %r12
	testq	$1, %rcx
	je	.LBB27_107
# BB#105:                               # %if.then.i
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r12, %rcx
	jmp	.LBB27_106
.LBB27_120:                             # %sw.bb489
	movq	%r8, %r15
	movq	%r14, %rax
	andq	%rbx, %rax
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	$61, %rdx
	cmpq	$61, %rax
	movq	%r13, %rbx
	je	.LBB27_121
.LBB27_123:                             # %if.else500
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$61, %rax
	movabsq	$0, %r14
	movabsq	$1, %rcx
	movq	%r14, %rsi
	jne	.LBB27_125
# BB#124:                               # %if.else500
	movq	%rcx, %rsi
.LBB27_125:                             # %if.else500
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rbx, %r13
	cmpq	$61, %rdx
	movq	%r15, %r8
	jne	.LBB27_127
# BB#126:                               # %if.else500
	movq	%rcx, %r14
.LBB27_127:                             # %if.else500
	jne	.LBB27_128
# BB#129:                               # %if.else500
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	cmpq	$61, %rax
	jne	.LBB27_130
# BB#136:                               # %land.lhs.true506
	movq	$61, %rdx
	movq	%r8, %rdi
	movq	%r12, %rsi
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %rbx
	callq	extract_muldiv
	movq	%rbx, %r8
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	24(%rsp), %rcx          # 8-byte Reload
	je	.LBB27_131
# BB#137:                               # %if.then510
	movq	%r13, %r15
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$61, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	jmp	.LBB27_55
.LBB27_75:
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	16(%rsp), %rax          # 8-byte Reload
.LBB27_77:                              # %if.end348
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB27_40
# BB#78:                                # %if.end356
	movq	%rax, %rdi
	movq	%rax, %rbx
	callq	tree_int_cst_sgn
	movq	%rbx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB27_80
# BB#79:                                # %lor.lhs.false360
	movq	%r12, %rdi
	movq	%rsi, %rbx
	callq	tree_int_cst_sgn
	movq	%rbx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB27_82
.LBB27_80:                              # %if.then364
	leaq	-61(%r15), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$7, %rax
	ja	.LBB27_40
# BB#81:                                # %if.then364
	movq	$64, 56(%rsp)           # 8-byte Folded Spill
	jmpq	*.LJTI27_1(,%rax,8)
.LBB27_82:                              # %if.end384
	movq	%r15, %rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$61, %rax
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	jne	.LBB27_85
# BB#83:
	movq	$1, %r15
	movq	$61, 56(%rsp)           # 8-byte Folded Spill
	jmp	.LBB27_86
.LBB27_128:
	movq	%rdx, 32(%rsp)          # 8-byte Spill
.LBB27_130:
	movq	%r12, 72(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rcx          # 8-byte Reload
.LBB27_131:                             # %if.else515
	movzbl	16(%r8), %eax
	xorq	%r12, %r12
	cmpq	$25, %rax
	jne	.LBB27_113
# BB#132:                               # %if.end525
	testq	%rcx, %rcx
	movq	%r13, %rbx
	je	.LBB27_138
# BB#133:                               # %land.lhs.true528
	movq	%rbx, %rdi
	movq	%r8, %rsi
	movq	%r8, %r12
	callq	convert
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	$61, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%r12, %r8
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB27_138
# BB#134:                               # %land.lhs.true534
	movzbl	18(%r15), %eax
	testq	$8, %rax
	je	.LBB27_135
.LBB27_138:                             # %if.end545
	movl	16(%rbx), %eax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB27_141
# BB#139:                               # %lor.lhs.false552
	andq	$255, %rax
	cmpq	$6, %rax
	jne	.LBB27_40
# BB#140:                               # %land.lhs.true559
	movzbl	62(%rbx), %eax
	testq	$2, %rax
	je	.LBB27_40
.LBB27_141:                             # %land.lhs.true566
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r12
	cmpq	$71, %rax
	jne	.LBB27_142
# BB#143:                               # %land.lhs.true566
	movabsq	$1, %rax
	jmp	.LBB27_144
.LBB27_142:
	movabsq	$0, %rax
.LBB27_144:                             # %land.lhs.true566
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	jne	.LBB27_147
# BB#145:                               # %lor.lhs.false572
	movq	64(%rsp), %rax          # 8-byte Reload
	leaq	-66(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$4, %rax
	jb	.LBB27_40
# BB#146:                               # %lor.lhs.false572
	xorq	$1, %r14
	jne	.LBB27_40
.LBB27_147:                             # %if.then587
	movq	$66, %rdi
	movq	%r8, %rsi
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	movq	%r8, %r13
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB27_149
# BB#148:                               # %if.then591
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %r14
	movq	$62, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	jmp	.LBB27_53
.LBB27_149:                             # %if.else597
	movq	$66, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB27_40
# BB#150:                               # %if.then601
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %r12
	callq	convert
	movq	%rax, %r14
	movq	$62, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	const_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB27_54
.LBB27_135:                             # %if.then541
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%r15, %rcx
	jmp	.LBB27_56
.LBB27_48:                              # %if.then208
	cmpq	$78, %r12
	jne	.LBB27_49
# BB#50:                                # %if.then208
	movabsq	$1, %r12
	jmp	.LBB27_51
.LBB27_107:                             # %if.end.i
	movzbl	16(%r12), %eax
	cmpq	$115, %rax
	jg	.LBB27_110
# BB#108:                               # %if.end.i
	cmpq	$29, %rax
	ja	.LBB27_112
# BB#109:                               # %if.end.i
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	jne	.LBB27_113
	jmp	.LBB27_112
.LBB27_49:
	movabsq	$0, %r12
.LBB27_51:                              # %if.then208
	movq	%r13, %rbx
	orq	$78, %r12
.LBB27_52:                              # %if.end212
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
.LBB27_53:                              # %if.end212
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
.LBB27_54:                              # %if.end212
	movq	%rbx, %rsi
.LBB27_55:                              # %if.end212
	movq	%r14, %rdx
.LBB27_56:                              # %if.end212
	callq	build
.LBB27_57:                              # %if.end212
	movq	%rax, %rdi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB27_84:                              # %if.then370
	movq	$63, 56(%rsp)           # 8-byte Folded Spill
.LBB27_85:                              # %lor.lhs.false387
	movq	$66, %rdi
	movq	%r12, %rdx
	movq	%rsi, %rbx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movq	%rbx, %rsi
	xorq	%r15, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB27_40
.LBB27_86:                              # %if.then391
	movq	%r13, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	callq	convert
	movq	%rax, %r13
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB27_40
# BB#87:                                # %lor.lhs.false397
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	jne	.LBB27_40
# BB#88:                                # %if.end407
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	16(%rax), %eax
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB27_92
# BB#89:                                # %land.lhs.true414
	andq	$255, %rax
	cmpq	$6, %rax
	jne	.LBB27_91
# BB#90:                                # %land.lhs.true421
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	62(%rax), %eax
	testq	$2, %rax
	jne	.LBB27_92
.LBB27_91:                              # %land.lhs.true428
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB27_40
.LBB27_92:                              # %if.end432
	testq	%r14, %r14
	je	.LBB27_99
# BB#93:                                # %land.lhs.true435
	movzbl	16(%r14), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rdx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	je	.LBB27_94
# BB#95:                                # %land.lhs.true435
	movabsq	$1, %rax
	jmp	.LBB27_96
.LBB27_110:                             # %if.end.i
	cmpq	$116, %rax
	je	.LBB27_113
# BB#111:                               # %if.end.i
	cmpq	$121, %rax
	je	.LBB27_113
.LBB27_112:                             # %if.end.i.i
	movq	8(%r12), %rax
	movq	$116, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	build1
	movl	16(%r12), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r12
	jmp	.LBB27_113
.LBB27_94:
	movabsq	$0, %rax
.LBB27_96:                              # %land.lhs.true435
	orq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB27_99
# BB#97:                                # %if.then445
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	jmp	.LBB27_98
.LBB27_99:                              # %if.end449
	testq	%r15, %r15
	je	.LBB27_40
# BB#100:                               # %if.then452
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r14, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
.LBB27_98:                              # %if.then445
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	jmp	.LBB27_56
.LBB27_40:                              # %sw.epilog
	xorq	%r12, %r12
.LBB27_113:                             # %return
	movq	%r12, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp322:
	.size	extract_muldiv, .Ltmp322-extract_muldiv
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI27_0:
	.quad	.LBB27_58
	.quad	.LBB27_68
	.quad	.LBB27_68
	.quad	.LBB27_101
	.quad	.LBB27_114
	.quad	.LBB27_114
	.quad	.LBB27_114
	.quad	.LBB27_114
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_114
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_41
	.quad	.LBB27_43
	.quad	.LBB27_43
	.quad	.LBB27_41
	.quad	.LBB27_40
	.quad	.LBB27_60
	.quad	.LBB27_60
.LJTI27_1:
	.quad	.LBB27_82
	.quad	.LBB27_40
	.quad	.LBB27_85
	.quad	.LBB27_84
	.quad	.LBB27_40
	.quad	.LBB27_40
	.quad	.LBB27_82
	.quad	.LBB27_82

	.text
	.align	16, 0x90
	.type	distribute_bit_expr,@function
distribute_bit_expr:                    # @distribute_bit_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp329:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp330:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp331:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp332:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp333:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp334:
	.cfi_def_cfa_offset 64
.Ltmp335:
	.cfi_offset %rbx, -48
.Ltmp336:
	.cfi_offset %r12, -40
.Ltmp337:
	.cfi_offset %r13, -32
.Ltmp338:
	.cfi_offset %r14, -24
.Ltmp339:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movzbl	16(%r12), %eax
	movzbl	16(%rbx), %ecx
	movq	%rdi, %r15
	cmpq	%rcx, %rax
	jne	.LBB28_13
# BB#1:                                 # %entry
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB28_13
# BB#2:                                 # %lor.lhs.false12
	cmpq	$86, %rax
	je	.LBB28_4
# BB#3:                                 # %lor.lhs.false12
	cmpq	$88, %rax
	jne	.LBB28_13
.LBB28_4:                               # %if.end
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	32(%r12), %rdi
	movq	32(%rbx), %rsi
	leaq	32(%r12), %r14
	addq	$32, %rbx
	xorq	%rdx, %rdx
	callq	operand_equal_p
	movq	32(%r12), %rdi
	testq	%r13, %rax
	je	.LBB28_6
# BB#5:                                 # %if.then26
	movq	%rdi, %r13
	addq	$8, %r14
	addq	$8, %rbx
	jmp	.LBB28_12
.LBB28_6:                               # %if.else
	movq	8(%rbx), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB28_8
# BB#7:                                 # %if.then44
	movq	(%r14), %r13
	addq	$8, %r14
	jmp	.LBB28_12
.LBB28_8:                               # %if.else54
	movq	8(%r14), %rdi
	movq	(%rbx), %rsi
	leaq	8(%rbx), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%rdx, %rdx
	callq	operand_equal_p
	movq	8(%r14), %rdi
	testq	%r13, %rax
	je	.LBB28_10
# BB#9:
	movq	%rdi, %r13
	movq	(%rsp), %rbx            # 8-byte Reload
	jmp	.LBB28_12
.LBB28_10:                              # %if.else73
	movq	(%rsp), %rax            # 8-byte Reload
	movq	(%rax), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB28_13
# BB#11:                                # %if.then82
	movq	8(%r14), %r13
.LBB28_12:                              # %if.end96
	movq	(%rbx), %rcx
	movq	(%r14), %rdx
	movzbl	16(%r12), %r14d
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	build
	movq	%rax, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB28_13:                              # %return
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp340:
	.size	distribute_bit_expr, .Ltmp340-distribute_bit_expr
	.cfi_endproc

	.align	16, 0x90
	.type	multiple_of_p,@function
multiple_of_p:                          # @multiple_of_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp347:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp348:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp349:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp350:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp351:
	.cfi_def_cfa_offset 48
.Ltmp352:
	.cfi_offset %rbx, -48
.Ltmp353:
	.cfi_offset %r12, -40
.Ltmp354:
	.cfi_offset %r13, -32
.Ltmp355:
	.cfi_offset %r14, -24
.Ltmp356:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rdi, %r12
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	operand_equal_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB29_33
# BB#1:                                 # %if.end.lr.ph
	xorq	%r14, %r14
.LBB29_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r12), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$6, %rax
	jne	.LBB29_34
# BB#3:                                 # %if.end2
                                        #   in Loop: Header=BB29_2 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$58, %rax
	jle	.LBB29_4
# BB#10:                                # %if.end2
                                        #   in Loop: Header=BB29_2 Depth=1
	cmpq	$81, %rax
	jle	.LBB29_11
# BB#16:                                # %if.end2
                                        #   in Loop: Header=BB29_2 Depth=1
	cmpq	$82, %rax
	je	.LBB29_25
# BB#17:                                # %if.end2
                                        #   in Loop: Header=BB29_2 Depth=1
	cmpq	$115, %rax
	jne	.LBB29_18
# BB#30:                                # %sw.bb64
                                        #   in Loop: Header=BB29_2 Depth=1
	movq	32(%rbx), %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$6, %rcx
	jne	.LBB29_34
# BB#31:                                # %lor.lhs.false
                                        #   in Loop: Header=BB29_2 Depth=1
	movl	60(%r12), %ecx
	movl	60(%rax), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jae	.LBB29_32
	jmp	.LBB29_34
.LBB29_25:                              # %sw.bb25
                                        #   in Loop: Header=BB29_2 Depth=1
	movq	40(%rbx), %rdx
	movzbl	16(%rdx), %eax
	cmpq	$25, %rax
	jne	.LBB29_34
# BB#26:                                # %if.then34
                                        #   in Loop: Header=BB29_2 Depth=1
	movq	global_trees+128(%rip), %rsi
	movq	8(%rsi), %rax
	movl	60(%rax), %eax
	movq	32(%rdx), %rcx
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rcx, %rax
	jbe	.LBB29_34
# BB#27:                                # %land.lhs.true
                                        #   in Loop: Header=BB29_2 Depth=1
	cmpq	$0, 40(%rdx)
	jne	.LBB29_34
# BB#28:                                # %land.lhs.true50
                                        #   in Loop: Header=BB29_2 Depth=1
	movq	$82, %rdi
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB29_34
# BB#29:                                # %land.lhs.true55
                                        #   in Loop: Header=BB29_2 Depth=1
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	je	.LBB29_32
	jmp	.LBB29_34
.LBB29_18:                              # %if.end2
                                        #   in Loop: Header=BB29_2 Depth=1
	cmpq	$118, %rax
	jne	.LBB29_34
# BB#19:                                # %if.end2.sw.bb93_crit_edge
                                        #   in Loop: Header=BB29_2 Depth=1
	movq	32(%rbx), %rbx
.LBB29_32:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB29_2 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	operand_equal_p
	testq	%r13, %rax
	je	.LBB29_2
.LBB29_33:
	movq	$1, %r14
.LBB29_34:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB29_4:                               # %if.end2
	cmpq	$25, %rax
	jne	.LBB29_34
# BB#5:                                 # %sw.bb98
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	jne	.LBB29_34
# BB#6:                                 # %lor.lhs.false105
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB29_9
# BB#7:                                 # %land.lhs.true111
	movq	%rbx, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB29_34
# BB#8:                                 # %lor.lhs.false115
	movq	%r15, %rdi
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB29_34
.LBB29_9:                               # %if.end120
	movq	$66, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	integer_zerop  # TAILCALL
.LBB29_11:                              # %if.end2
	leaq	-59(%rax), %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	jae	.LBB29_12
# BB#21:                                # %sw.bb14
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	multiple_of_p
	xorq	%r14, %r14
	testq	%r13, %rax
	je	.LBB29_34
# BB#22:                                # %land.rhs
	movq	40(%rbx), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	multiple_of_p
	testq	%r13, %rax
	je	.LBB29_23
# BB#24:                                # %land.rhs
	movabsq	$1, %r14
	jmp	.LBB29_34
.LBB29_12:                              # %if.end2
	cmpq	$61, %rax
	jne	.LBB29_34
# BB#13:                                # %sw.bb
	movq	32(%rbx), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	multiple_of_p
	testq	%r13, %rax
	movq	$1, %r14
	jne	.LBB29_34
# BB#14:                                # %lor.rhs
	movq	40(%rbx), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	multiple_of_p
	testq	%r13, %rax
	je	.LBB29_15
# BB#20:                                # %lor.rhs
	movabsq	$1, %r14
	jmp	.LBB29_34
.LBB29_23:
	movabsq	$0, %r14
	jmp	.LBB29_34
.LBB29_15:
	movabsq	$0, %r14
	jmp	.LBB29_34
.Ltmp357:
	.size	multiple_of_p, .Ltmp357-multiple_of_p
	.cfi_endproc

	.align	16, 0x90
	.type	fold_truthop,@function
fold_truthop:                           # @fold_truthop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp364:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp365:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp366:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp367:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp368:
	.cfi_def_cfa_offset 48
	subq	$288, %rsp              # imm = 0x120
.Ltmp369:
	.cfi_def_cfa_offset 336
.Ltmp370:
	.cfi_offset %rbx, -48
.Ltmp371:
	.cfi_offset %r12, -40
.Ltmp372:
	.cfi_offset %r13, -32
.Ltmp373:
	.cfi_offset %r14, -24
.Ltmp374:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movl	16(%r14), %r12d
	movq	%rcx, %r13
	movq	%rsi, %rbx
	movq	%rdi, %r11
	xorq	%r15, %r15
	testq	$256, %r12              # imm = 0x100
	jne	.LBB30_174
# BB#1:                                 # %lor.lhs.false
	movl	16(%r13), %eax
	testq	$256, %rax              # imm = 0x100
	jne	.LBB30_174
# BB#2:                                 # %if.end
	andq	$255, %r12
	cmpq	$88, %r12
	jne	.LBB30_6
# BB#3:                                 # %land.lhs.true
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movq	40(%r14), %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$88, %r12
	je	.LBB30_5
# BB#4:                                 # %if.then16
	movq	global_trees+88(%rip), %rcx
	movq	$102, %rdi
	xorq	%rax, %rax
	movq	$102, %r12
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %r14
.LBB30_5:                               # %if.end18
	movq	112(%rsp), %r11         # 8-byte Reload
	movl	16(%r13), %eax
.LBB30_6:                               # %if.end18
	andq	$255, %rax
	cmpq	$88, %rax
	jne	.LBB30_7
# BB#8:                                 # %land.lhs.true20
	movq	%r11, 112(%rsp)         # 8-byte Spill
	movq	40(%r13), %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB30_9
# BB#10:                                # %if.then26
	movq	global_trees+88(%rip), %rcx
	movq	$102, %rdi
	xorq	%rax, %rax
	movq	$102, %r15
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	build
	movq	%rax, %r13
	jmp	.LBB30_11
.LBB30_7:
	movq	%rax, %r15
	jmp	.LBB30_12
.LBB30_9:
	movq	$88, %r15
.LBB30_11:                              # %if.end28
	movq	112(%rsp), %r11         # 8-byte Reload
.LBB30_12:                              # %if.end28
	movzbl	tree_code_type(%r12), %eax
	cmpq	$60, %rax
	jne	.LBB30_13
# BB#14:                                # %lor.lhs.false32
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movzbl	tree_code_type(%r15), %eax
	cmpq	$60, %rax
	jne	.LBB30_15
# BB#16:                                # %if.end39
	movq	%r15, 88(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %r11
	cmpq	$93, %r11
	movabsq	$0, %rdi
	movabsq	$1, %rax
	movq	%rdi, %rdx
	jne	.LBB30_18
# BB#17:                                # %if.end39
	movq	%rax, %rdx
.LBB30_18:                              # %if.end39
	cmpq	$91, %r11
	movq	%rdi, %rcx
	movabsq	$0, %r15
	jne	.LBB30_20
# BB#19:                                # %if.end39
	movq	%rax, %rcx
.LBB30_20:                              # %if.end39
	orq	%rcx, %rdx
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	jne	.LBB30_22
# BB#21:                                # %if.end39
	movq	%rax, %rdi
.LBB30_22:                              # %if.end39
	movq	32(%r14), %r9
	movq	40(%r14), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	32(%r13), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movslq	ix86_branch_cost(%rip), %rax
	movq	40(%r13), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	cmpq	$2, %rax
	jl	.LBB30_50
# BB#23:                                # %land.lhs.true58
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$7, %rax
	je	.LBB30_50
# BB#24:                                # %land.lhs.true58
	addq	$93, %rdi
	cmpq	$8, %rax
	jne	.LBB30_25
# BB#26:                                # %land.lhs.true75
	movq	8(%rsi), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	movq	104(%rsp), %rdx         # 8-byte Reload
	je	.LBB30_50
	jmp	.LBB30_27
.LBB30_13:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_15:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_25:
	movq	104(%rsp), %rdx         # 8-byte Reload
.LBB30_27:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rax
	movl	16(%rax), %r8d
	movq	%r8, %rdx
	andq	$254, %rdx
	cmpq	$114, %rdx
	jne	.LBB30_29
# BB#28:                                # %land.rhs.i
                                        #   in Loop: Header=BB30_27 Depth=1
	movq	32(%rax), %rdx
	movl	60(%rsi), %ecx
	movq	8(%rdx), %rsi
	movl	60(%rsi), %ebx
	xorq	%rcx, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB30_27
.LBB30_29:                              # %while.end.i
	movq	%r8, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %edx
	cmpq	$99, %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	je	.LBB30_38
# BB#30:                                # %lor.rhs.i
	cmpq	$100, %rdx
	jne	.LBB30_50
# BB#31:                                # %lor.rhs.i
	movq	%r8, %rcx
	andq	$1024, %rcx             # imm = 0x400
	testq	%r10, %rcx
	jne	.LBB30_50
# BB#32:                                # %lor.rhs.i
	movq	%r8, %rcx
	andq	$2048, %rcx             # imm = 0x800
	testq	%r10, %rcx
	jne	.LBB30_50
# BB#33:                                # %land.lhs.true48.i
	movq	48(%rax), %rdx
	testq	$512, %rdx              # imm = 0x200
	jne	.LBB30_50
# BB#34:                                # %land.lhs.true48.i
	movq	%r8, %rax
	andq	$524288, %rax           # imm = 0x80000
	testq	%r10, %rax
	jne	.LBB30_50
# BB#35:                                # %land.lhs.true48.i
	movq	%rdx, %rax
	andq	$256, %rax              # imm = 0x100
	jne	.LBB30_50
# BB#36:                                # %land.rhs66.i
	testq	$262144, %r8            # imm = 0x40000
	movq	80(%rsp), %rsi          # 8-byte Reload
	je	.LBB30_38
# BB#37:                                # %land.rhs66.i
	andq	$1024, %rdx             # imm = 0x400
	movq	80(%rsp), %rsi          # 8-byte Reload
	je	.LBB30_50
.LBB30_38:                              # %while.cond.i659
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movl	16(%rdx), %ecx
	movq	%rcx, %rax
	andq	$254, %rax
	cmpq	$114, %rax
	jne	.LBB30_40
# BB#39:                                # %land.rhs.i672
                                        #   in Loop: Header=BB30_38 Depth=1
	movq	32(%rdx), %rsi
	movq	8(%rdx), %rax
	movq	8(%rsi), %rbx
	movl	60(%rax), %eax
	movl	60(%rbx), %ebx
	xorq	%rax, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB30_38
.LBB30_40:                              # %while.end.i677
	movq	%rcx, %rax
	andq	$255, %rax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$99, %rax
	je	.LBB30_49
# BB#41:                                # %lor.rhs.i685
	cmpq	$100, %rax
	jne	.LBB30_50
# BB#42:                                # %lor.rhs.i685
	movq	%rcx, %rax
	andq	$1024, %rax             # imm = 0x400
	testq	%r10, %rax
	jne	.LBB30_50
# BB#43:                                # %lor.rhs.i685
	movq	%rcx, %rax
	andq	$2048, %rax             # imm = 0x800
	testq	%r10, %rax
	jne	.LBB30_50
# BB#44:                                # %land.lhs.true48.i696
	movq	48(%rdx), %rdx
	testq	$512, %rdx              # imm = 0x200
	jne	.LBB30_50
# BB#45:                                # %land.lhs.true48.i696
	movq	%rcx, %rax
	andq	$524288, %rax           # imm = 0x80000
	testq	%r10, %rax
	jne	.LBB30_50
# BB#46:                                # %land.lhs.true48.i696
	movq	%rdx, %rax
	andq	$256, %rax              # imm = 0x100
	jne	.LBB30_50
# BB#47:                                # %land.rhs66.i699
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB30_49
# BB#48:                                # %land.rhs66.i699
	andq	$1024, %rdx             # imm = 0x400
	jne	.LBB30_49
.LBB30_50:                              # %if.end94
	movq	%r11, 112(%rsp)         # 8-byte Spill
	leaq	-101(%r12), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	leaq	-101(%rcx), %rcx
	orq	%rax, %rcx
	andq	%r10, %rcx
	cmpq	$1, %rcx
	ja	.LBB30_174
# BB#51:                                # %if.end107
	xorq	%r15, %r15
	movl	%r15d, 124(%rsp)
	leaq	152(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	184(%rsp), %rax
	leaq	280(%rsp), %rsi
	leaq	272(%rsp), %rdx
	leaq	204(%rsp), %rcx
	leaq	220(%rsp), %r8
	leaq	124(%rsp), %rbx
	movq	%rax, (%rsp)
	movq	%r9, %rdi
	movq	%rbx, %r9
	callq	decode_field_reference
	movq	%rax, %r14
	leaq	144(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	176(%rsp), %rax
	leaq	264(%rsp), %rsi
	leaq	256(%rsp), %rdx
	leaq	200(%rsp), %rcx
	leaq	216(%rsp), %r8
	movq	%rax, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %r9
	callq	decode_field_reference
	movq	%rax, 56(%rsp)          # 8-byte Spill
	leaq	136(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	168(%rsp), %rax
	leaq	248(%rsp), %rsi
	leaq	240(%rsp), %rdx
	leaq	196(%rsp), %rcx
	leaq	212(%rsp), %r8
	movq	%rax, (%rsp)
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %r9
	callq	decode_field_reference
	movq	%rax, %r13
	leaq	128(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	160(%rsp), %rax
	leaq	232(%rsp), %rsi
	leaq	224(%rsp), %rdx
	leaq	192(%rsp), %rcx
	leaq	208(%rsp), %r8
	movq	%rax, (%rsp)
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %r9
	callq	decode_field_reference
	movl	124(%rsp), %ecx
	testq	%rcx, %rcx
	jne	.LBB30_174
# BB#52:                                # %if.end107
	testq	%r14, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r14, %rdi
	je	.LBB30_174
# BB#53:                                # %if.end107
	testq	%r13, %r13
	je	.LBB30_174
# BB#54:                                # %lor.lhs.false119
	movq	%rax, %r15
	xorq	%rdx, %rdx
	movq	%rdi, %r14
	movq	%r13, %rsi
	callq	operand_equal_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB30_55
# BB#56:                                # %if.end123
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	movq	%r14, %rbx
	movq	%r15, %rsi
	jne	.LBB30_58
# BB#57:                                # %land.lhs.true130
	movq	80(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	je	.LBB30_63
.LBB30_58:                              # %if.else
	cmpq	$0, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB30_59
# BB#60:                                # %if.else
	movq	%rbx, %r14
	movq	%rcx, %rbx
	testq	%rsi, %rsi
	je	.LBB30_61
# BB#62:                                # %lor.lhs.false143
	xorq	%rdx, %rdx
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	operand_equal_p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	movabsq	$0, %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movabsq	$0, %r15
	movq	%r14, %rbx
	je	.LBB30_174
.LBB30_63:                              # %if.end149
	testq	$1, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB30_64
# BB#65:                                # %if.end149
	movq	%rcx, %r13
	movabsq	$1, %r15
	jmp	.LBB30_66
.LBB30_55:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_49:                              # %if.then92
	xorq	%rax, %rax
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	%r13, %rcx
.LBB30_172:                             # %return
	callq	build
.LBB30_173:                             # %return
	movq	%rax, %r15
	jmp	.LBB30_174
.LBB30_64:
	movq	%rcx, %r13
	movabsq	$0, %r15
.LBB30_66:                              # %if.end149
	addq	$101, %r15
	cmpq	%r15, %r12
	jne	.LBB30_68
# BB#67:
	movq	%r13, %rcx
	jmp	.LBB30_75
.LBB30_68:                              # %if.then155
	movq	%rbx, %r14
	movq	72(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB30_69
# BB#70:                                # %land.lhs.true157
	callq	integer_zerop
	movq	%r13, %rbx
	testq	%rbx, %rax
	je	.LBB30_71
# BB#72:                                # %land.lhs.true160
	movq	184(%rsp), %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	callq	integer_pow2p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB30_73
# BB#74:                                # %if.then163
	movq	$1, %rax
	movl	%eax, 220(%rsp)
	movq	%r14, %rbx
.LBB30_75:                              # %if.end166
	movq	%r15, %rax
	andq	%rcx, %rax
	cmpq	%rax, 88(%rsp)          # 8-byte Folded Reload
	je	.LBB30_83
# BB#76:                                # %if.then169
	movq	%rcx, %r12
	movq	80(%rsp), %rdi          # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB30_77
# BB#78:                                # %land.lhs.true171
	movq	%rbx, %r14
	callq	integer_zerop
	movq	%r12, %rbx
	testq	%rbx, %rax
	je	.LBB30_79
# BB#80:                                # %land.lhs.true174
	movq	168(%rsp), %rdi
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	callq	integer_pow2p
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB30_81
# BB#82:                                # %if.then177
	movq	$1, %rax
	movl	%eax, 212(%rsp)
	movq	%r14, %rbx
.LBB30_83:                              # %if.end180
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%rcx, %r15
	movq	272(%rsp), %rcx
	movq	240(%rsp), %rax
	cmpq	%rax, %rcx
	movq	%rcx, %r14
	jle	.LBB30_85
# BB#84:                                # %if.end180
	movq	%rax, %r14
.LBB30_85:                              # %if.end180
	movq	280(%rsp), %rdi
	movq	248(%rsp), %rdx
	addq	%rcx, %rdi
	addq	%rax, %rdx
	cmpq	%rdx, %rdi
	jge	.LBB30_87
# BB#86:                                # %if.end180
	movq	%rdx, %rdi
.LBB30_87:                              # %if.end180
	movq	8(%rbx), %rax
	movl	64(%rax), %edx
	movl	124(%rsp), %r8d
	movl	word_mode(%rip), %ecx
	subq	%r14, %rdi
	movq	%r14, %rsi
	callq	get_best_mode
	movq	%rbx, %rcx
	testq	%r15, %rax
	movabsq	$0, %r15
	je	.LBB30_174
# BB#88:                                # %if.end202
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %edi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%rdi, %rbx
	negq	%rbx
	andq	%r14, %rbx
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	callq	type_for_size
	movq	272(%rsp), %r12
	movq	240(%rsp), %r13
	movq	184(%rsp), %rsi
	movq	%rax, %r14
	subq	%rbx, %r12
	subq	%rbx, %r13
	movq	%r13, 48(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	convert
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	168(%rsp), %rsi
	movq	%rax, %rbx
	movq	%rbx, 184(%rsp)
	movq	%r14, %rdi
	callq	convert
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rax, 168(%rsp)
	xorq	%r13, %r13
	movq	72(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	movabsq	$0, %r15
	je	.LBB30_95
# BB#89:                                # %if.then219
	movq	%r14, %rdi
	callq	convert
	movq	280(%rsp), %rsi
	movq	152(%rsp), %rcx
	movl	220(%rsp), %edx
	movq	%rax, %rdi
	callq	unextend
	movq	sizetype_tab(%rip), %rsi
	movq	%rbx, %r15
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rbx
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %r15
	movq	$90, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	$88, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB30_90
.LBB30_95:                              # %if.end241
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rsi          # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB30_99
# BB#96:                                # %if.then243
	movq	%r14, %rdi
	callq	convert
	movq	248(%rsp), %rsi
	movq	136(%rsp), %rcx
	movl	212(%rsp), %edx
	movq	%rax, %rdi
	callq	unextend
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	$90, %rdi
	movq	%r14, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	$88, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r13
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB30_97
.LBB30_99:                              # %if.end265
	testq	%r15, %r15
	je	.LBB30_100
# BB#158:                               # %if.end455
	movq	$88, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB30_164
# BB#159:                               # %land.lhs.true459
	movq	$88, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	const_binop
	movq	%rax, %r12
	movq	$88, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	simple_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB30_160
.LBB30_164:                             # %if.end472
	movl	220(%rsp), %eax
	testq	%rax, %rax
	movq	$1, 112(%rsp)           # 8-byte Folded Spill
	jne	.LBB30_169
# BB#165:                               # %lor.rhs476
	movl	212(%rsp), %eax
	testq	%rax, %rax
	je	.LBB30_166
# BB#167:                               # %lor.rhs476
	movabsq	$1, %rax
	jmp	.LBB30_168
.LBB30_59:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_61:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_69:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_71:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_77:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_73:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_79:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_81:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_90:                              # %if.then230
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	notq	%rsi
	andq	$1, %rsi
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	warning
	testq	$1, %rbx
	je	.LBB30_92
# BB#91:
	movabsq	$global_trees+88, %rax
	jmp	.LBB30_93
.LBB30_97:                              # %if.then254
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	notq	%rcx
	andq	$1, %rcx
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	warning
	testq	$1, %rbx
	jne	.LBB30_98
.LBB30_92:                              # %if.then230
	movabsq	$global_trees+96, %rax
	jmp	.LBB30_93
.LBB30_100:                             # %if.then268
	movq	280(%rsp), %rax
	movq	264(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB30_101
# BB#102:                               # %lor.lhs.false271
	movq	248(%rsp), %rcx
	movq	232(%rsp), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB30_103
# BB#104:                               # %lor.lhs.false274
	movl	220(%rsp), %edx
	movl	216(%rsp), %esi
	cmpq	%rsi, %rdx
	jne	.LBB30_105
# BB#106:                               # %lor.lhs.false277
	movl	212(%rsp), %edx
	movl	208(%rsp), %esi
	cmpq	%rsi, %rdx
	jne	.LBB30_107
# BB#108:                               # %lor.lhs.false280
	movq	272(%rsp), %rdi
	movq	240(%rsp), %rbx
	movq	256(%rsp), %rdx
	movq	224(%rsp), %rsi
	subq	%rbx, %rdi
	movq	%rdx, %rbx
	subq	%rsi, %rbx
	cmpq	%rbx, %rdi
	jne	.LBB30_109
# BB#110:                               # %if.end286
	cmpq	%rsi, %rdx
	movq	%rdx, %rbx
	jle	.LBB30_112
# BB#111:                               # %if.end286
	movq	%rsi, %rbx
.LBB30_112:                             # %if.end286
	addq	%rax, %rdx
	addq	%rcx, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB30_114
# BB#113:                               # %if.end286
	movq	%rsi, %rdx
.LBB30_114:                             # %if.end286
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movl	64(%rax), %eax
	movl	124(%rsp), %r8d
	movl	word_mode(%rip), %ecx
	subq	%rbx, %rdx
	movq	%rdx, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	get_best_mode
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB30_115
# BB#116:                               # %if.end314
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_bitsize(%rax,%rax), %eax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	112(%rsp), %r13         # 8-byte Reload
	negq	%r13
	andq	%rbx, %r13
	movq	%r13, 80(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	type_for_size
	movq	256(%rsp), %r15
	movq	224(%rsp), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	176(%rsp), %rcx
	movq	%rax, %rbx
	subq	%r13, %r15
	movq	%r15, 72(%rsp)          # 8-byte Spill
	subq	%r13, 64(%rsp)          # 8-byte Folded Spill
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, %r13
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	160(%rsp), %rcx
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, %r13
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %r13
	movq	%r13, 160(%rsp)
	movq	$86, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rax, 184(%rsp)
	movq	$86, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	%rax, %r13
	movq	%r13, 176(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jne	.LBB30_132
# BB#117:                               # %if.end314
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jne	.LBB30_132
# BB#118:                               # %if.then339
	movl	220(%rsp), %eax
	testq	%rax, %rax
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	jne	.LBB30_123
# BB#119:                               # %lor.rhs343
	movl	212(%rsp), %eax
	testq	%rax, %rax
	je	.LBB30_120
# BB#121:                               # %lor.rhs343
	movabsq	$1, %rax
	jmp	.LBB30_122
.LBB30_160:                             # %if.then465
	cmpq	$91, 112(%rsp)          # 8-byte Folded Reload
	je	.LBB30_163
# BB#161:                               # %if.then465
	cmpq	$93, 112(%rsp)          # 8-byte Folded Reload
	jne	.LBB30_162
.LBB30_163:                             # %if.else470
	movabsq	$.L.str4, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning
	movq	global_trees+88(%rip), %rax
	jmp	.LBB30_94
.LBB30_166:
	movabsq	$0, %rax
.LBB30_168:                             # %lor.rhs476
	movq	%rax, 112(%rsp)         # 8-byte Spill
.LBB30_169:                             # %lor.end478
	movq	sizetype_tab(%rip), %rax
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	sizetype_tab+24(%rip), %rcx
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	movq	$13, %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	shlq	%cl, %rdx
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rax, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 16(%rbx)
	movq	$86, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	88(%rsp), %rdx          # 8-byte Reload
	callq	const_binop
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 184(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	andq	$511, 88(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	80(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
	xorq	%rsi, %rsi
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	subq	%r12, 88(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r12
	andq	%r12, 88(%rsp)          # 8-byte Folded Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB30_171
# BB#170:                               # %if.then485
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	112(%rsp), %rcx         # 8-byte Reload
	callq	build
	movq	%rax, %rbx
.LBB30_171:                             # %if.end487
	movq	$86, %rdi
	movq	%r15, %rsi
	movq	%r13, %rdx
	callq	const_binop
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB30_172
.LBB30_98:
	movabsq	$global_trees+88, %rax
.LBB30_93:                              # %if.then230
	movq	(%rax), %rax
	jmp	.LBB30_94
.LBB30_101:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_103:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_105:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_107:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_109:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_115:
	xorq	%r15, %r15
	jmp	.LBB30_174
.LBB30_132:                             # %if.end368
	movq	280(%rsp), %rcx
	movq	272(%rsp), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	240(%rsp), %rdx
	leaq	(%rax,%rcx), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB30_135
# BB#133:                               # %land.lhs.true372
	movq	264(%rsp), %rax
	movq	256(%rsp), %rdi
	movq	224(%rsp), %r8
	addq	%rax, %rdi
	cmpq	%r8, %rdi
	jne	.LBB30_135
# BB#134:                               # %land.lhs.true372.if.then384_crit_edge
	movq	248(%rsp), %rax
	movq	%rsi, %rdx
	jmp	.LBB30_138
.LBB30_120:
	movabsq	$0, %rax
.LBB30_122:                             # %lor.rhs343
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB30_123:                             # %lor.end345
	movq	sizetype_tab(%rip), %rax
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	sizetype_tab+24(%rip), %rcx
	movq	%rax, %r15
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	build
	movq	%rax, %r15
	movl	16(%r15), %eax
	movq	$13, %rcx
	movq	72(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rcx, %rax
	orq	%rdx, %rax
	movl	%eax, 16(%r15)
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %eax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	andq	$511, 72(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx)
	xorq	%rsi, %rsi
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	subq	%r12, 72(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r12
	andq	%r12, 72(%rsp)          # 8-byte Folded Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB30_125
# BB#124:                               # %if.then350
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	build
	movq	%rax, %r15
.LBB30_125:                             # %if.end352
	movl	216(%rsp), %eax
	testq	%rax, %rax
	movq	$1, %r14
	jne	.LBB30_129
# BB#126:                               # %lor.rhs356
	movl	208(%rsp), %eax
	testq	%rax, %rax
	je	.LBB30_127
# BB#128:                               # %lor.rhs356
	movabsq	$1, %r14
	jmp	.LBB30_129
.LBB30_162:                             # %if.then468
	movabsq	$.L.str3, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	warning
	movq	global_trees+96(%rip), %rax
.LBB30_94:                              # %return
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	convert
	jmp	.LBB30_173
.LBB30_135:                             # %lor.lhs.false376
	movq	248(%rsp), %rax
	leaq	(%rax,%rdx), %rsi
	cmpq	%rsi, 80(%rsp)          # 8-byte Folded Reload
	jne	.LBB30_136
# BB#137:                               # %land.lhs.true380
	movq	224(%rsp), %rsi
	movq	232(%rsp), %rdi
	movq	256(%rsp), %r8
	addq	%rsi, %rdi
	cmpq	%rdi, %r8
	movabsq	$0, %r15
	jne	.LBB30_174
.LBB30_138:                             # %if.then384
	addq	%rcx, %rax
	cmpq	%rdx, 80(%rsp)          # 8-byte Folded Reload
	jle	.LBB30_140
# BB#139:                               # %if.then384
	movq	%rdx, 80(%rsp)          # 8-byte Spill
.LBB30_140:                             # %if.then384
	movl	220(%rsp), %ecx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	sizetype_tab(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	%rax, %r15
	movq	$32, %rcx
	movq	sizetype_tab+24(%rip), %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %rcx
	callq	build
	movq	%rax, %r15
	movl	16(%r15), %eax
	andq	$1, 24(%rsp)            # 8-byte Folded Spill
	shlq	$13, 24(%rsp)           # 8-byte Folded Spill
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rcx, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	orq	%rcx, %rax
	movl	%eax, 16(%r15)
	movq	264(%rsp), %rdx
	movq	232(%rsp), %rax
	movq	256(%rsp), %rsi
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	224(%rsp), %rcx
	addq	%rdx, %rax
	cmpq	%rcx, %rsi
	jle	.LBB30_142
# BB#141:                               # %if.then384
	movq	%rcx, 80(%rsp)          # 8-byte Spill
.LBB30_142:                             # %if.then384
	movl	216(%rsp), %ecx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	sizetype_tab(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %rdi
	movq	%rdx, %rsi
	callq	size_int_type_wide
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	sizetype_tab+24(%rip), %rax
	movq	80(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	build
	movq	%rax, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movl	16(%rcx), %eax
	andq	$1, 32(%rsp)            # 8-byte Folded Spill
	shlq	$13, 32(%rsp)           # 8-byte Folded Spill
	movabsq	$4294959103, %rdx       # imm = 0xFFFFDFFF
	andq	%rdx, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	orq	%rdx, %rax
	movl	%eax, 16(%rcx)
	movq	48(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jle	.LBB30_144
# BB#143:                               # %if.then384
	movq	48(%rsp), %r12          # 8-byte Reload
.LBB30_144:                             # %if.then384
	movq	sizetype_tab(%rip), %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rax, 184(%rsp)
	movq	64(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jle	.LBB30_146
# BB#145:                               # %if.then384
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB30_146:                             # %if.then384
	movq	sizetype_tab(%rip), %rax
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %r12
	cmpq	%rbx, %r14
	movq	%r12, 176(%rsp)
	je	.LBB30_147
# BB#148:                               # %if.then424
	movq	112(%rsp), %rax         # 8-byte Reload
	cmpq	%rax, 40(%rsp)          # 8-byte Folded Reload
	jbe	.LBB30_150
# BB#149:                               # %if.then427
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	88(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	%rax, 184(%rsp)
	jmp	.LBB30_153
.LBB30_127:
	movabsq	$0, %r14
.LBB30_129:                             # %lor.end358
	movq	sizetype_tab(%rip), %rax
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	sizetype_tab+24(%rip), %rcx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	%rax, %r8
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	callq	build
	movq	%rax, %rdx
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$13, %rcx
	shlq	%cl, %r14
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rcx, %rax
	orq	%r14, %rax
	movl	%eax, 16(%rdx)
	movq	8(%r13), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %eax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	andq	$511, 80(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, %r14
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	%rax, 8(%r14)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	movq	112(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, 80(%rsp)          # 8-byte Folded Spill
	andq	%r12, 80(%rsp)          # 8-byte Folded Spill
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, %r14
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB30_131
# BB#130:                               # %if.then364
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	88(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	callq	build
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB30_131:                             # %if.end366
	xorq	%rax, %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB30_172
.LBB30_136:
	xorq	%r15, %r15
.LBB30_174:                             # %return
	movq	%r15, %rax
	addq	$288, %rsp              # imm = 0x120
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB30_147:
	movq	%r14, %rbx
	jmp	.LBB30_153
.LBB30_150:                             # %if.else430
	jae	.LBB30_151
# BB#152:                               # %if.then433
	movq	%r14, %rbx
	movq	%rbx, %rdi
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	convert
	movq	%rax, %r12
	movq	%r12, 176(%rsp)
	jmp	.LBB30_153
.LBB30_151:
	movq	%r14, %rbx
.LBB30_153:                             # %if.end438
	movq	88(%rsp), %r13          # 8-byte Reload
	movq	8(%r13), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	280(%rsp), %rax
	movq	248(%rsp), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	60(%rcx), %r14d
	addq	%rax, 72(%rsp)          # 8-byte Folded Spill
	andq	$511, %r14              # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
	xorq	%rsi, %rsi
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%r13, %rdi
	movq	%r13, %r14
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB30_155
# BB#154:                               # %if.then443
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %r15
.LBB30_155:                             # %if.end445
	movq	8(%r12), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	264(%rsp), %rax
	movq	232(%rsp), %r13
	movq	88(%rsp), %rcx          # 8-byte Reload
	movl	60(%rcx), %r14d
	addq	%rax, %r13
	andq	$511, %r14              # imm = 0x1FF
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	signed_type
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx)
	xorq	%rsi, %rsi
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	force_fit_type
	movq	sizetype_tab(%rip), %rax
	subq	%r13, %r14
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %r13
	andq	%r13, %r14
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%r14, %rdi
	movq	%rcx, %rsi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	testq	%r13, %rax
	jne	.LBB30_157
# BB#156:                               # %if.then450
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %rcx
	callq	build
	movq	%rax, 80(%rsp)          # 8-byte Spill
.LBB30_157:                             # %if.end452
	xorq	%rax, %rax
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	movq	80(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB30_172
.Ltmp375:
	.size	fold_truthop, .Ltmp375-fold_truthop
	.cfi_endproc

	.align	16, 0x90
	.type	optimize_minmax_comparison,@function
optimize_minmax_comparison:             # @optimize_minmax_comparison
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp382:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp383:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp384:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp385:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp386:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp387:
	.cfi_def_cfa_offset 80
.Ltmp388:
	.cfi_offset %rbx, -48
.Ltmp389:
	.cfi_offset %r12, -40
.Ltmp390:
	.cfi_offset %r13, -32
.Ltmp391:
	.cfi_offset %r14, -24
.Ltmp392:
	.cfi_offset %r15, -16
	movq	8(%rdi), %r10
	movq	32(%rdi), %rcx
	movq	40(%rdi), %rsi
	movq	global_trees(%rip), %r8
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB31_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %r14
	movl	16(%r14), %r13d
	movq	%r13, %r15
	andq	$255, %r15
	leaq	-114(%r15), %rcx
	andq	%r11, %rcx
	cmpq	$2, %rcx
	ja	.LBB31_5
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	32(%r14), %rcx
	cmpq	%r8, %rcx
	je	.LBB31_5
# BB#3:                                 # %land.lhs.true21
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	8(%r14), %rdx
	movq	8(%rcx), %rbx
	movl	60(%rdx), %r9d
	movl	60(%rbx), %eax
	xorq	%r9, %rax
	testq	$65024, %rax            # imm = 0xFE00
	jne	.LBB31_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB31_1 Depth=1
	movl	16(%rdx), %eax
	movl	16(%rbx), %edx
	xorq	%rax, %rdx
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB31_1
.LBB31_5:                               # %while.end
	movq	%r10, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	40(%r14), %rbx
	movq	%rbx, %rdi
	movq	%rsi, %r12
	callq	tree_int_cst_equal
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	andq	$254, %r13
	cmpq	$78, %r13
	jne	.LBB31_6
# BB#8:                                 # %lor.lhs.false72
	movl	16(%r12), %ecx
	andq	$262399, %rcx           # imm = 0x400FF
	cmpq	$25, %rcx
	movq	%r12, %rcx
	jne	.LBB31_6
# BB#9:                                 # %lor.lhs.false83
	movl	16(%rbx), %edx
	andq	$262399, %rdx           # imm = 0x400FF
	cmpq	$25, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jne	.LBB31_6
# BB#10:                                # %if.end
	movq	24(%rsp), %rbx          # 8-byte Reload
	movzbl	16(%rbx), %edx
	addq	$-97, %rdx
	andq	%rsi, %rdx
	cmpq	$5, %rdx
	ja	.LBB31_7
# BB#11:                                # %if.end
	movq	32(%r14), %r12
	jmpq	*.LJTI31_0(,%rdx,8)
.LBB31_12:                              # %sw.bb
	movq	%rbx, %rdi
	callq	invert_truthvalue
	movq	%rax, %rdi
	callq	optimize_minmax_comparison
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	invert_truthvalue  # TAILCALL
.LBB31_6:
	movq	24(%rsp), %rbx          # 8-byte Reload
.LBB31_7:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB31_48:                              # %sw.bb139
	cmpq	$79, %r15
	je	.LBB31_49
# BB#50:                                # %sw.bb139
	movabsq	$1, %rdx
	jmp	.LBB31_51
.LBB31_13:                              # %sw.bb103
	movq	$101, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rcx, %r12
	callq	build
	movq	%rax, %rdi
	callq	optimize_minmax_comparison
	movq	%rax, %rbx
	movq	$99, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	build
	movq	%rax, %rdi
	callq	optimize_minmax_comparison
	movq	%rax, %rcx
	movq	$92, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	jmp	.LBB31_37
.LBB31_14:                              # %sw.bb110
	cmpq	$79, %r15
	jne	.LBB31_18
# BB#15:                                # %sw.bb110
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rsi, %rdx
	je	.LBB31_18
# BB#16:                                # %if.then114
	movq	$98, %rdi
	jmp	.LBB31_17
.LBB31_49:
	movabsq	$0, %rdx
.LBB31_51:                              # %sw.bb139
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	8(%rsp), %rdi           # 8-byte Reload
	orq	%rdi, %rax
	testq	%rsi, %rax
	je	.LBB31_54
# BB#52:                                # %sw.bb139
	testq	%rdx, %rdx
	je	.LBB31_53
.LBB31_54:                              # %if.else148
	cmpq	$79, %r15
	jne	.LBB31_56
# BB#55:                                # %if.then150
	movq	global_trees+96(%rip), %rsi
	jmp	.LBB31_23
.LBB31_18:                              # %if.else
	cmpq	$79, %r15
	jne	.LBB31_21
# BB#19:                                # %if.else
	testq	%rsi, %rax
	je	.LBB31_21
# BB#20:                                # %if.then120
	movq	$101, %rdi
.LBB31_17:                              # %if.then114
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB31_36
.LBB31_56:                              # %if.else152
	cmpq	$78, %r15
	jne	.LBB31_53
# BB#57:                                # %if.else152
	testq	%rsi, %rax
	jne	.LBB31_22
.LBB31_53:                              # %if.then145
	movq	$99, %rdi
.LBB31_35:                              # %if.then129
	xorq	%rax, %rax
	movq	%r14, %rsi
.LBB31_36:                              # %if.then129
	movq	%r12, %rdx
.LBB31_37:                              # %if.then129
	callq	build
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB31_21:                              # %if.else123
	cmpq	$79, %r15
	movq	16(%rsp), %r14          # 8-byte Reload
	jne	.LBB31_33
.LBB31_22:                              # %if.then125
	movq	global_trees+88(%rip), %rsi
.LBB31_23:                              # %if.then125
	movq	%r14, %rdi
	callq	convert
	movzbl	17(%r12), %ecx
	movq	%rax, %rbx
	testq	$1, %rcx
	je	.LBB31_26
# BB#24:                                # %if.then.i
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
.LBB31_25:                              # %if.then.i
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB31_26:                              # %if.end.i
	movzbl	16(%rbx), %ecx
	cmpq	$115, %rcx
	jg	.LBB31_29
# BB#27:                                # %if.end.i
	cmpq	$29, %rcx
	ja	.LBB31_31
# BB#28:                                # %if.end.i
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$637534208, %rax        # imm = 0x26000000
	jne	.LBB31_7
	jmp	.LBB31_31
.LBB31_33:                              # %if.else127
	movq	8(%rsp), %rdx           # 8-byte Reload
	testq	%rsi, %rdx
	je	.LBB31_38
# BB#34:                                # %if.then129
	movq	$100, %rdi
	jmp	.LBB31_35
.LBB31_29:                              # %if.end.i
	cmpq	$116, %rcx
	je	.LBB31_7
# BB#30:                                # %if.end.i
	cmpq	$121, %rcx
	je	.LBB31_7
.LBB31_31:                              # %if.end.i.i
	movq	8(%rbx), %rsi
	movq	$116, %rdi
.LBB31_32:                              # %return
	movq	%rbx, %rdx
	callq	build1
	movl	16(%rbx), %ecx
	movl	16(%rax), %edx
	andq	$512, %rcx              # imm = 0x200
	movabsq	$4294966783, %rsi       # imm = 0xFFFFFDFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %rbx
	jmp	.LBB31_7
.LBB31_38:                              # %if.else132
	testq	%rsi, %rax
	je	.LBB31_47
# BB#39:                                # %if.then134
	movq	global_trees+88(%rip), %rsi
	movq	%r14, %rdi
	callq	convert
	movzbl	17(%r12), %ecx
	movq	%rax, %r8
	testq	$1, %rcx
	je	.LBB31_41
# BB#40:                                # %if.then.i247
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r8, %rcx
	jmp	.LBB31_25
.LBB31_47:                              # %if.else136
	movq	$101, %rdi
	jmp	.LBB31_35
.LBB31_41:                              # %if.end.i251
	movzbl	16(%r8), %eax
	movq	%r8, %rbx
	cmpq	$115, %rax
	jg	.LBB31_44
# BB#42:                                # %if.end.i251
	cmpq	$29, %rax
	ja	.LBB31_46
# BB#43:                                # %if.end.i251
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	testq	$637534208, %rdx        # imm = 0x26000000
	jne	.LBB31_7
	jmp	.LBB31_46
.LBB31_44:                              # %if.end.i251
	cmpq	$116, %rax
	je	.LBB31_7
# BB#45:                                # %if.end.i251
	cmpq	$121, %rax
	je	.LBB31_7
.LBB31_46:                              # %if.end.i.i260
	movq	8(%rbx), %rax
	movq	$116, %rdi
	movq	%rax, %rsi
	jmp	.LBB31_32
.Ltmp393:
	.size	optimize_minmax_comparison, .Ltmp393-optimize_minmax_comparison
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI31_0:
	.quad	.LBB31_12
	.quad	.LBB31_12
	.quad	.LBB31_48
	.quad	.LBB31_13
	.quad	.LBB31_14
	.quad	.LBB31_12

	.text
	.align	16, 0x90
	.type	twoval_comparison_p,@function
twoval_comparison_p:                    # @twoval_comparison_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp400:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp401:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp402:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp403:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp404:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp405:
	.cfi_def_cfa_offset 64
.Ltmp406:
	.cfi_offset %rbx, -48
.Ltmp407:
	.cfi_offset %r12, -40
.Ltmp408:
	.cfi_offset %r13, -32
.Ltmp409:
	.cfi_offset %r14, -24
.Ltmp410:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r13
	movq	%rdi, %r15
	movabsq	$0, %r11
	movabsq	$1, %r10
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movabsq	$52776558133249, %r9    # imm = 0x300000000001
	jmp	.LBB32_1
	.align	16, 0x90
.LBB32_36:                              # %sw.bb
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	32(%r15), %r15
.LBB32_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15), %esi
	movzbl	tree_code_type(%rsi), %edx
	cmpq	$96, %rsi
	movq	%r11, %rcx
	jne	.LBB32_3
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%r10, %rcx
.LBB32_3:                               # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%rbx, %r14
	cmpq	$101, %rdx
	movq	%r11, %rax
	jne	.LBB32_5
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%r10, %rax
.LBB32_5:                               # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%r11, %rbx
	je	.LBB32_7
# BB#6:                                 # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%r10, %rbx
.LBB32_7:                               # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	andq	%rcx, %rax
	movq	$49, %rdi
	jne	.LBB32_9
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	%rdx, %rdi
.LBB32_9:                               # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	testq	%rax, %rax
	jne	.LBB32_10
# BB#11:                                # %tailrecurse
                                        #   in Loop: Header=BB32_1 Depth=1
	testq	%rbx, %rbx
	movq	%r14, %rbx
	jne	.LBB32_22
# BB#12:                                # %land.lhs.true8
                                        #   in Loop: Header=BB32_1 Depth=1
	leaq	-47(%rsi), %rcx
	andq	%rbx, %rcx
	cmpq	$45, %rcx
	ja	.LBB32_14
# BB#13:                                # %land.lhs.true8
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	$50, %rdi
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r9, %rax
	jne	.LBB32_22
	.align	16, 0x90
.LBB32_14:                              # %if.else17
                                        #   in Loop: Header=BB32_1 Depth=1
	cmpq	$101, %rdx
	jne	.LBB32_15
# BB#16:                                # %if.else17
                                        #   in Loop: Header=BB32_1 Depth=1
	cmpq	$118, %rsi
	movq	%rdx, %rdi
	jne	.LBB32_22
# BB#17:                                # %land.lhs.true24
                                        #   in Loop: Header=BB32_1 Depth=1
	cmpq	$0, 48(%r15)
	movq	$101, %rdi
	jne	.LBB32_22
# BB#18:                                # %land.lhs.true28
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	32(%r15), %rax
	movzbl	17(%rax), %eax
	testq	$1, %rax
	jne	.LBB32_22
# BB#19:                                # %if.then35
                                        #   in Loop: Header=BB32_1 Depth=1
	xorq	%rax, %rax
	cmpq	$0, (%r13)
	jne	.LBB32_50
# BB#20:                                # %lor.lhs.false37
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	$0, (%rcx)
	jne	.LBB32_50
# BB#21:                                # %if.end
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	$1, %rax
	movl	%eax, (%r8)
	movq	$49, %rdi
	jmp	.LBB32_22
	.align	16, 0x90
.LBB32_10:                              #   in Loop: Header=BB32_1 Depth=1
	movq	%r14, %rbx
	jmp	.LBB32_22
.LBB32_15:                              #   in Loop: Header=BB32_1 Depth=1
	movq	%rdx, %rdi
	.align	16, 0x90
.LBB32_22:                              # %if.end42
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	cmpq	$98, %rdi
	jg	.LBB32_29
# BB#23:                                # %if.end42
                                        #   in Loop: Header=BB32_1 Depth=1
	andq	%rbx, %rdi
	cmpq	$49, %rdi
	je	.LBB32_36
# BB#24:                                # %if.end42
	cmpq	$50, %rdi
	jne	.LBB32_25
# BB#37:                                # %sw.bb47
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r8, %r14
	movq	%r14, %rcx
	callq	twoval_comparison_p
	xorq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB32_51
# BB#38:                                # %land.rhs
	movq	40(%r15), %rdi
	movq	%r13, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r14, %rcx
	callq	twoval_comparison_p
	testq	%rbx, %rax
	jne	.LBB32_40
# BB#39:
	movabsq	$0, %r12
	jmp	.LBB32_51
.LBB32_29:                              # %if.end42
	movq	$1, %rax
	andq	%rbx, %rdi
	cmpq	$99, %rdi
	jne	.LBB32_30
.LBB32_50:                              # %return.loopexit
	movq	%rax, %r12
	jmp	.LBB32_51
.LBB32_30:                              # %if.end42
	cmpq	$101, %rdi
	jne	.LBB32_51
# BB#31:                                # %sw.bb59
	cmpq	$51, %rsi
	jne	.LBB32_51
# BB#32:                                # %if.then62
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r8, %r14
	movq	%r14, %rcx
	callq	twoval_comparison_p
	xorq	%r12, %r12
	testq	%rbx, %rax
	je	.LBB32_51
# BB#33:                                # %land.lhs.true68
	movq	40(%r15), %rdi
	movq	%r13, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r14, %rcx
	callq	twoval_comparison_p
	testq	%rbx, %rax
	je	.LBB32_51
# BB#34:                                # %land.rhs74
	movq	48(%r15), %rdi
	movq	%r13, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%r14, %rcx
	callq	twoval_comparison_p
	testq	%rbx, %rax
	je	.LBB32_35
.LBB32_40:                              # %land.rhs74
	movabsq	$1, %r12
	jmp	.LBB32_51
.LBB32_35:
	movabsq	$0, %r12
.LBB32_51:                              # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB32_25:                              # %if.end42
	cmpq	$60, %rdi
	jne	.LBB32_51
# BB#26:                                # %sw.bb83
	movq	32(%r15), %rdi
	movq	40(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB32_51
# BB#27:                                # %if.end93
	movq	(%r13), %rdi
	movq	32(%r15), %rsi
	testq	%rdi, %rdi
	je	.LBB32_28
# BB#41:                                # %if.else100
	movq	%r13, %rbx
	xorq	%rdx, %rdx
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB32_45
# BB#42:                                # %if.else107
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rdi
	movq	32(%r15), %rsi
	testq	%rdi, %rdi
	je	.LBB32_43
# BB#44:                                # %if.else114
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB32_45
	jmp	.LBB32_51
.LBB32_28:                              # %if.then96
	movq	%rsi, (%r13)
	movq	%r13, %rbx
	jmp	.LBB32_45
.LBB32_43:                              # %if.then110
	movq	%rsi, (%rax)
.LBB32_45:                              # %if.end125
	movq	(%rbx), %rdi
	movq	40(%r15), %rsi
	xorq	%rdx, %rdx
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB32_49
# BB#46:                                # %if.else132
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rdi
	movq	40(%r15), %rsi
	testq	%rdi, %rdi
	je	.LBB32_47
# BB#48:                                # %if.else139
	xorq	%rdx, %rdx
	xorq	%r12, %r12
	callq	operand_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB32_51
.LBB32_49:                              # %if.end149
	movq	$1, %r12
	jmp	.LBB32_51
.LBB32_47:                              # %if.then135
	movq	%rsi, (%rax)
	movq	$1, %r12
	jmp	.LBB32_51
.Ltmp411:
	.size	twoval_comparison_p, .Ltmp411-twoval_comparison_p
	.cfi_endproc

	.align	16, 0x90
	.type	eval_subst,@function
eval_subst:                             # @eval_subst
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp418:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp419:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp420:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp421:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp422:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp423:
	.cfi_def_cfa_offset 96
.Ltmp424:
	.cfi_offset %rbx, -48
.Ltmp425:
	.cfi_offset %r12, -40
.Ltmp426:
	.cfi_offset %r13, -32
.Ltmp427:
	.cfi_offset %r14, -24
.Ltmp428:
	.cfi_offset %r15, -16
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	movabsq	$0, %r9
	movabsq	$1, %r8
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB33_1
	.align	16, 0x90
.LBB33_36:                              # %sw.bb38
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$40, %r14
	movq	(%r14), %r14
.LBB33_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r14), %ebx
	movzbl	tree_code_type(%rbx), %ecx
	cmpq	$96, %rbx
	movq	%r9, %rdx
	jne	.LBB33_3
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%r8, %rdx
.LBB33_3:                               # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$101, %rcx
	movq	%r9, %rdi
	jne	.LBB33_5
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%r8, %rdi
.LBB33_5:                               # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%r9, %rax
	je	.LBB33_7
# BB#6:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%r8, %rax
.LBB33_7:                               # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	andq	%rdx, %rdi
	movq	$49, %rdx
	jne	.LBB33_9
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%rcx, %rdx
.LBB33_9:                               # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	testq	%rdi, %rdi
	jne	.LBB33_13
# BB#10:                                # %tailrecurse
                                        #   in Loop: Header=BB33_1 Depth=1
	testq	%rax, %rax
	jne	.LBB33_13
# BB#11:                                # %land.lhs.true10
                                        #   in Loop: Header=BB33_1 Depth=1
	leaq	-91(%rbx), %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	movq	$50, %rdx
	jb	.LBB33_13
# BB#12:                                # %land.lhs.true10
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	%rcx, %rdx
.LBB33_13:                              # %if.end16
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	$56, %rcx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	cmpq	$100, %rdx
	jle	.LBB33_14
# BB#31:                                # %if.end16
                                        #   in Loop: Header=BB33_1 Depth=1
	andq	%r12, %rdx
	cmpq	$101, %rdx
	jne	.LBB33_39
# BB#32:                                # %sw.bb32
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$47, %rbx
	je	.LBB33_36
# BB#33:                                # %sw.bb32
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$51, %rbx
	je	.LBB33_37
# BB#34:                                # %sw.bb32
                                        #   in Loop: Header=BB33_1 Depth=1
	cmpq	$118, %rbx
	jne	.LBB33_17
# BB#35:                                # %sw.bb33
                                        #   in Loop: Header=BB33_1 Depth=1
	addq	$32, %r14
	movq	(%r14), %r14
	jmp	.LBB33_1
.LBB33_14:                              # %if.end16
	andq	%r12, %rdx
	cmpq	$49, %rdx
	je	.LBB33_29
# BB#15:                                # %if.end16
	cmpq	$50, %rdx
	jne	.LBB33_16
# BB#30:                                # %sw.bb21
	movq	8(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%r14), %rdi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdx
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %r8
	callq	eval_subst
	movq	40(%r14), %rdi
	movq	%rax, %r14
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%r13, %rdx
	movq	%r15, %rcx
	movq	%r12, %r8
	callq	eval_subst
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	callq	build
	jmp	.LBB33_38
.LBB33_29:                              # %sw.bb
	movq	8(%r14), %r15
	movq	32(%r14), %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	eval_subst
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	build1
	jmp	.LBB33_38
.LBB33_16:                              # %if.end16
	cmpq	$60, %rdx
	jne	.LBB33_39
.LBB33_17:                              # %sw.bb58
	movq	8(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%r14), %r15
	movq	40(%r14), %r13
	movq	%rsi, %rcx
	cmpq	%rcx, %r15
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB33_22
# BB#18:                                # %lor.lhs.false67
	xorq	%rdx, %rdx
	movq	%rcx, %r14
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	operand_equal_p
	movq	%r14, %rcx
	testq	%r12, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	jne	.LBB33_22
# BB#19:                                # %if.else70
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	je	.LBB33_21
# BB#20:                                # %lor.lhs.false73
	xorq	%rdx, %rdx
	movq	%rcx, %r14
	movq	%r15, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	operand_equal_p
	movq	%r14, %rcx
	testq	%r12, %rax
	movq	%r15, %r14
	je	.LBB33_22
.LBB33_21:                              # %if.then76
	movq	24(%rsp), %r14          # 8-byte Reload
.LBB33_22:                              # %if.end78
	movq	%r13, %r15
	cmpq	%rcx, %r15
	je	.LBB33_27
# BB#23:                                # %lor.lhs.false81
	xorq	%rdx, %rdx
	movq	%rcx, %rsi
	movq	%r15, %rdi
	callq	operand_equal_p
	testq	%r12, %rax
	jne	.LBB33_27
# BB#24:                                # %if.else85
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r15
	je	.LBB33_26
# BB#25:                                # %lor.lhs.false88
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	operand_equal_p
	testq	%r12, %rax
	movq	%r15, 40(%rsp)          # 8-byte Spill
	je	.LBB33_27
.LBB33_26:                              # %if.then91
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB33_27:                              # %if.end93
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	build
.LBB33_38:                              # %if.end93
	movq	%rax, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB33_39:                              # %return
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB33_37:                              # %sw.bb43
	movq	8(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	32(%r14), %rdi
	movq	%rsi, %r15
                                        # kill: RSI<def> R15<kill>
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdx
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rcx
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r8
	callq	eval_subst
	movq	40(%r14), %rdi
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	eval_subst
	movq	48(%r14), %rdi
	movq	%rax, %r14
	movq	%r15, %rsi
	movq	%r12, %rdx
	movq	%r13, %rcx
	movq	%rbx, %r8
	callq	eval_subst
	movq	%rax, %rbx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%r14, %rcx
	movq	%rbx, %r8
	callq	build
	jmp	.LBB33_38
.Ltmp429:
	.size	eval_subst, .Ltmp429-eval_subst
	.cfi_endproc

	.align	16, 0x90
	.type	operand_equal_for_comparison_p,@function
operand_equal_for_comparison_p:         # @operand_equal_for_comparison_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp436:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp437:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp438:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp439:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp440:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp441:
	.cfi_def_cfa_offset 64
.Ltmp442:
	.cfi_offset %rbx, -48
.Ltmp443:
	.cfi_offset %r12, -40
.Ltmp444:
	.cfi_offset %r13, -32
.Ltmp445:
	.cfi_offset %r14, -24
.Ltmp446:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdx, %rdx
                                        # kill: RDI<def> R14<kill>
                                        # kill: RSI<def> R12<kill>
	callq	operand_equal_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	movq	$1, %rbx
	jne	.LBB34_19
# BB#1:                                 # %if.end
	movq	8(%r14), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$12, %rcx
	ja	.LBB34_18
# BB#2:                                 # %if.end
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB34_18
# BB#3:                                 # %lor.lhs.false25
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	cmpq	$12, %rcx
	ja	.LBB34_18
# BB#4:                                 # %lor.lhs.false25
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$7232, %rsi             # imm = 0x1C40
	movabsq	$0, %rbx
	je	.LBB34_19
# BB#5:                                 # %while.cond.preheader
	movq	global_trees(%rip), %r8
	movq	%r14, %rsi
	.align	16, 0x90
.LBB34_6:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	movzbl	16(%rdi), %esi
	addq	$-114, %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB34_9
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB34_6 Depth=1
	movq	32(%rdi), %rsi
	cmpq	%r8, %rsi
	je	.LBB34_9
# BB#8:                                 # %land.rhs
                                        #   in Loop: Header=BB34_6 Depth=1
	movl	60(%rdx), %ebx
	movq	8(%rsi), %rdx
	movl	60(%rdx), %ecx
	xorq	%rbx, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB34_6
.LBB34_9:
	movq	%r12, %rdx
	.align	16, 0x90
.LBB34_10:                              # %while.cond96
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rsi
	movzbl	16(%rsi), %ecx
	addq	$-114, %rcx
	andq	%r13, %rcx
	cmpq	$2, %rcx
	ja	.LBB34_13
# BB#11:                                # %land.lhs.true114
                                        #   in Loop: Header=BB34_10 Depth=1
	movq	32(%rsi), %rdx
	cmpq	%r8, %rdx
	je	.LBB34_13
# BB#12:                                # %land.rhs119
                                        #   in Loop: Header=BB34_10 Depth=1
	movl	60(%rax), %ecx
	movq	8(%rdx), %rax
	movl	60(%rax), %ebx
	xorq	%rcx, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB34_10
.LBB34_13:                              # %while.end143
	xorq	%rdx, %rdx
	callq	operand_equal_p
	testq	%r13, %rax
	movq	$1, %rbx
	jne	.LBB34_19
# BB#14:                                # %if.end147
	leaq	12(%rsp), %rsi
	movq	%r12, %rdi
	callq	get_narrower
	movq	%rax, (%rsp)            # 8-byte Spill
	leaq	8(%rsp), %rsi
	movq	%r15, %rdi
	callq	get_narrower
	movl	12(%rsp), %edi
	movl	8(%rsp), %ecx
	cmpq	%rcx, %rdi
	jne	.LBB34_18
# BB#15:                                # %land.lhs.true156
	movq	8(%r12), %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	8(%rdx), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	andq	%r13, %rcx
	cmpq	%rcx, %rdx
	jae	.LBB34_18
# BB#16:                                # %land.lhs.true164
	movq	8(%rax), %rax
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rcx, %rax
	jae	.LBB34_18
# BB#17:                                # %if.then172
	movq	8(%r14), %r15
	callq	signed_or_unsigned_type
	movq	%rax, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	convert
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	operand_equal_p
	testq	%r13, %rax
	jne	.LBB34_19
.LBB34_18:                              # %if.end185
	xorq	%rbx, %rbx
.LBB34_19:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp447:
	.size	operand_equal_for_comparison_p, .Ltmp447-operand_equal_for_comparison_p
	.cfi_endproc

	.globl	tree_expr_nonnegative_p
	.align	16, 0x90
	.type	tree_expr_nonnegative_p,@function
tree_expr_nonnegative_p:                # @tree_expr_nonnegative_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp451:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp452:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp453:
	.cfi_def_cfa_offset 32
.Ltmp454:
	.cfi_offset %rbx, -24
.Ltmp455:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$1, %rax
	jmp	.LBB35_1
	.align	16, 0x90
.LBB35_28:                              # %sw.bb9
                                        #   in Loop: Header=BB35_1 Depth=1
	addq	$32, %rbx
	movq	(%rbx), %rbx
.LBB35_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$46, %rcx
	jle	.LBB35_2
# BB#4:                                 # %tailrecurse
                                        #   in Loop: Header=BB35_1 Depth=1
	cmpq	$115, %rcx
	jg	.LBB35_7
# BB#5:                                 # %tailrecurse
                                        #   in Loop: Header=BB35_1 Depth=1
	leaq	-47(%rcx), %rdx
	andq	%r14, %rdx
	cmpq	$34, %rdx
	ja	.LBB35_29
# BB#6:                                 # %tailrecurse
                                        #   in Loop: Header=BB35_1 Depth=1
	jmpq	*.LJTI35_0(,%rdx,8)
.LBB35_27:                              # %sw.bb28
                                        #   in Loop: Header=BB35_1 Depth=1
	addq	$40, %rbx
	movq	(%rbx), %rbx
	jmp	.LBB35_1
	.align	16, 0x90
.LBB35_7:                               # %tailrecurse
                                        #   in Loop: Header=BB35_1 Depth=1
	cmpq	$116, %rcx
	je	.LBB35_28
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB35_1 Depth=1
	cmpq	$118, %rcx
	je	.LBB35_28
# BB#9:                                 # %tailrecurse
	cmpq	$120, %rcx
	jne	.LBB35_29
# BB#10:                                # %sw.bb78
	movq	40(%rbx), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	rtl_expr_nonnegative_p  # TAILCALL
.LBB35_2:                               # %tailrecurse
	cmpq	$25, %rcx
	jne	.LBB35_29
# BB#3:                                 # %sw.bb1
	movq	%rbx, %rdi
	callq	tree_int_cst_sgn
	movq	%rax, %rcx
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rcx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	jmp	.LBB35_44
.LBB35_29:                              # %sw.default
	movzbl	tree_code_type(%rcx), %eax
	cmpq	$60, %rax
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB35_31
# BB#30:                                # %sw.default
	movq	%rsi, %rax
.LBB35_31:                              # %sw.default
	cmpq	$93, %rcx
	movq	%rdx, %rdi
	jne	.LBB35_33
# BB#32:                                # %sw.default
	movq	%rsi, %rdi
.LBB35_33:                              # %sw.default
	orq	%rdi, %rax
	cmpq	$91, %rcx
	movq	%rdx, %rdi
	jne	.LBB35_35
# BB#34:                                # %sw.default
	movq	%rsi, %rdi
.LBB35_35:                              # %sw.default
	orq	%rdi, %rax
	cmpq	$94, %rcx
	movq	%rdx, %rdi
	jne	.LBB35_37
# BB#36:                                # %sw.default
	movq	%rsi, %rdi
.LBB35_37:                              # %sw.default
	orq	%rdi, %rax
	cmpq	$92, %rcx
	movq	%rdx, %rdi
	jne	.LBB35_39
# BB#38:                                # %sw.default
	movq	%rsi, %rdi
.LBB35_39:                              # %sw.default
	orq	%rdi, %rax
	cmpq	$95, %rcx
	movq	%rdx, %rdi
	jne	.LBB35_41
# BB#40:                                # %sw.default
	movq	%rsi, %rdi
.LBB35_41:                              # %sw.default
	orq	%rdi, %rax
	cmpq	$96, %rcx
	jne	.LBB35_43
# BB#42:                                # %sw.default
	movq	%rsi, %rdx
.LBB35_43:                              # %sw.default
	orq	%rdx, %rax
	andq	$1, %rax
.LBB35_44:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB35_11:                              # %sw.bb2
	movq	32(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	je	.LBB35_44
# BB#12:                                # %land.rhs
	movq	40(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	testq	%r14, %rax
	je	.LBB35_13
# BB#14:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB35_44
.LBB35_13:
	movabsq	$0, %rax
	jmp	.LBB35_44
.LBB35_15:                              # %sw.bb14
	movq	40(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	je	.LBB35_44
# BB#16:                                # %land.rhs20
	movq	48(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	testq	%r14, %rax
	je	.LBB35_17
# BB#18:                                # %land.rhs20
	movabsq	$1, %rax
	jmp	.LBB35_44
.LBB35_19:                              # %sw.bb33
	movq	32(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r14, %rcx
	je	.LBB35_44
# BB#20:                                # %land.rhs39
	movq	40(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	testq	%r14, %rax
	je	.LBB35_21
# BB#22:                                # %land.rhs39
	movabsq	$1, %rax
	jmp	.LBB35_44
.LBB35_23:                              # %sw.bb47
	movq	32(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB35_44
# BB#24:                                # %lor.rhs
	movq	40(%rbx), %rdi
	callq	tree_expr_nonnegative_p
	testq	%r14, %rax
	je	.LBB35_25
# BB#26:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB35_44
.LBB35_17:
	movabsq	$0, %rax
	jmp	.LBB35_44
.LBB35_21:
	movabsq	$0, %rax
	jmp	.LBB35_44
.LBB35_25:
	movabsq	$0, %rax
	jmp	.LBB35_44
.Ltmp456:
	.size	tree_expr_nonnegative_p, .Ltmp456-tree_expr_nonnegative_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI35_0:
	.quad	.LBB35_27
	.quad	.LBB35_27
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_15
	.quad	.LBB35_27
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_11
	.quad	.LBB35_11
	.quad	.LBB35_11
	.quad	.LBB35_11
	.quad	.LBB35_28
	.quad	.LBB35_28
	.quad	.LBB35_28
	.quad	.LBB35_28
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_29
	.quad	.LBB35_19
	.quad	.LBB35_23
	.quad	.LBB35_44
	.quad	.LBB35_44

	.text
	.globl	rtl_expr_nonnegative_p
	.align	16, 0x90
	.type	rtl_expr_nonnegative_p,@function
rtl_expr_nonnegative_p:                 # @rtl_expr_nonnegative_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp462:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp463:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp464:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp465:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp466:
	.cfi_def_cfa_offset 48
.Ltmp467:
	.cfi_offset %rbx, -40
.Ltmp468:
	.cfi_offset %r12, -32
.Ltmp469:
	.cfi_offset %r14, -24
.Ltmp470:
	.cfi_offset %r15, -16
	movl	(%rdi), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jg	.LBB36_14
# BB#1:                                 # %entry
	cmpq	$54, %rcx
	je	.LBB36_6
# BB#2:                                 # %entry
	cmpq	$55, %rcx
	jne	.LBB36_3
# BB#8:                                 # %sw.bb1
	xorq	%rax, %rax
	testq	$16711680, %rdx         # imm = 0xFF0000
	jne	.LBB36_16
# BB#9:                                 # %if.then
	movq	24(%rdi), %rax
	jmp	.LBB36_7
.LBB36_14:                              # %entry
	movq	$1, %rax
	addq	$-67, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB36_16
	jmp	.LBB36_15
.LBB36_6:                               # %sw.bb
	movq	8(%rdi), %rax
.LBB36_7:                               # %return
	movq	$63, %rcx
	shrq	%cl, %rax
	xorq	$1, %rax
	jmp	.LBB36_16
.LBB36_3:                               # %entry
	cmpq	$56, %rcx
	jne	.LBB36_15
# BB#4:                                 # %sw.bb11
	movq	8(%rdi), %r15
	movslq	(%r15), %r14
	testq	%r14, %r14
	movq	$1, %rbx
	jle	.LBB36_5
# BB#12:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB36_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%rbx,8), %rdi
	callq	rtl_expr_nonnegative_p
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r12, %rcx
	je	.LBB36_16
# BB#10:                                # %for.cond
                                        #   in Loop: Header=BB36_13 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	cmpq	%r14, %rax
	jl	.LBB36_13
# BB#11:
	movq	$1, %rax
	jmp	.LBB36_16
.LBB36_15:                              # %sw.default
	xorq	%rax, %rax
.LBB36_16:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB36_5:
	movq	$1, %rax
	jmp	.LBB36_16
.Ltmp471:
	.size	rtl_expr_nonnegative_p, .Ltmp471-rtl_expr_nonnegative_p
	.cfi_endproc

	.align	16, 0x90
	.type	decode_field_reference,@function
decode_field_reference:                 # @decode_field_reference
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp478:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp479:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp480:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp481:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp482:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp483:
	.cfi_def_cfa_offset 80
.Ltmp484:
	.cfi_offset %rbx, -48
.Ltmp485:
	.cfi_offset %r12, -40
.Ltmp486:
	.cfi_offset %r13, -32
.Ltmp487:
	.cfi_offset %r14, -24
.Ltmp488:
	.cfi_offset %r15, -16
	movq	8(%rdi), %rax
	movzbl	16(%rax), %ebx
	movq	%r8, %r10
	movq	%rcx, %r8
	movq	%rsi, %r14
	cmpq	$12, %rbx
	jbe	.LBB37_2
# BB#1:
	xorq	%rax, %rax
	jmp	.LBB37_28
.LBB37_2:                               # %entry
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	testq	$7232, %rsi             # imm = 0x1C40
	je	.LBB37_3
# BB#4:                                 # %while.cond.preheader
	movq	global_trees(%rip), %r12
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	.align	16, 0x90
.LBB37_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %edi
	leaq	-114(%rdi), %rsi
	andq	%r11, %rsi
	cmpq	$2, %rsi
	ja	.LBB37_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB37_5 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r12, %rdi
	je	.LBB37_17
# BB#7:                                 # %land.rhs
                                        #   in Loop: Header=BB37_5 Depth=1
	movl	60(%rax), %esi
	movq	8(%rdi), %rax
	movl	60(%rax), %ecx
	xorq	%rsi, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB37_5
	jmp	.LBB37_17
.LBB37_8:                               # %while.end
	xorq	%r15, %r15
	cmpq	$88, %rdi
	jne	.LBB37_17
# BB#9:                                 # %if.then68
	movq	32(%rbx), %rax
	movq	40(%rbx), %r15
	.align	16, 0x90
.LBB37_10:                              # %while.cond75
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movzbl	16(%rbx), %eax
	addq	$-114, %rax
	andq	%r11, %rax
	cmpq	$2, %rax
	ja	.LBB37_13
# BB#11:                                # %land.lhs.true93
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	32(%rbx), %rax
	cmpq	%r12, %rax
	je	.LBB37_13
# BB#12:                                # %land.rhs98
                                        #   in Loop: Header=BB37_10 Depth=1
	movq	8(%rbx), %rcx
	movq	8(%rax), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB37_10
	.align	16, 0x90
.LBB37_13:                              # %while.cond123
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r15), %edi
	leaq	-114(%rdi), %rax
	andq	%r11, %rax
	cmpq	$2, %rax
	ja	.LBB37_16
# BB#14:                                # %land.lhs.true141
                                        #   in Loop: Header=BB37_13 Depth=1
	movq	32(%r15), %rdi
	xorq	%rax, %rax
	cmpq	%r12, %rdi
	je	.LBB37_28
# BB#15:                                # %land.rhs146
                                        #   in Loop: Header=BB37_13 Depth=1
	movq	8(%r15), %rcx
	movq	8(%rdi), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	movq	%rdi, %r15
	je	.LBB37_13
	jmp	.LBB37_28
.LBB37_3:
	xorq	%rax, %rax
	jmp	.LBB37_28
.LBB37_16:                              # %while.end170
	xorq	%rax, %rax
	cmpq	$25, %rdi
	jne	.LBB37_28
.LBB37_17:                              # %if.end178
	leaq	24(%rsp), %rcx
	movq	%r9, (%rsp)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r10, %r9
	callq	get_inner_reference
	movq	%rax, %r12
	cmpq	%rbx, %r12
	jne	.LBB37_20
# BB#18:                                # %if.end178
	testq	%r15, %r15
	jne	.LBB37_20
# BB#19:
	xorq	%rax, %rax
	jmp	.LBB37_28
.LBB37_20:                              # %lor.lhs.false182
	movq	(%r14), %rdi
	testq	%rdi, %rdi
	js	.LBB37_21
# BB#22:                                # %lor.lhs.false182
	movq	24(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB37_23
# BB#24:                                # %lor.lhs.false186
	movzbl	16(%r12), %eax
	cmpq	$57, %rax
	movabsq	$0, %rax
	je	.LBB37_28
# BB#25:                                # %if.end193
	movq	$1, %rsi
	callq	type_for_size
	movq	%rax, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movl	60(%rbx), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$-1, %rdi
	movq	$-1, %rsi
	callq	build_int_2_wide
	movq	%rbx, 8(%rax)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rax, %r13
	callq	force_fit_type
	movq	(%r14), %rax
	movq	sizetype_tab(%rip), %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	$511, %rcx              # imm = 0x1FF
	movq	%rcx, %rdi
	movq	%rcx, %rbx
	subq	%rax, %rdi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	(%r14), %rcx
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, %r14
	subq	%rcx, %rbx
	movq	%rbx, %rdi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
	testq	%r15, %r15
	je	.LBB37_27
# BB#26:                                # %if.then212
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	convert
	movq	%rax, %rcx
	movq	$88, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rbx
.LBB37_27:                              # %if.end216
	movq	88(%rsp), %rax
	movq	80(%rsp), %rcx
	movq	%rbx, (%rcx)
	movq	%r15, (%rax)
	movq	%r12, %rax
	jmp	.LBB37_28
.LBB37_21:
	xorq	%rax, %rax
	jmp	.LBB37_28
.LBB37_23:
	xorq	%rax, %rax
.LBB37_28:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp489:
	.size	decode_field_reference, .Ltmp489-decode_field_reference
	.cfi_endproc

	.align	16, 0x90
	.type	unextend,@function
unextend:                               # @unextend
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp496:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp497:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp498:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp499:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp500:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp501:
	.cfi_def_cfa_offset 64
.Ltmp502:
	.cfi_offset %rbx, -48
.Ltmp503:
	.cfi_offset %r12, -40
.Ltmp504:
	.cfi_offset %r13, -32
.Ltmp505:
	.cfi_offset %r14, -24
.Ltmp506:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %r12
	movl	60(%r12), %eax
	movq	%rcx, %rdi
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$254, %rax
	movzbl	mode_bitsize(%rax), %r15d
	movq	%rsi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %rcx
	andq	%rax, %rcx
	cmpq	%rcx, %r15
	je	.LBB38_9
# BB#1:                                 # %entry
	testq	%rax, %rdx
	jne	.LBB38_9
# BB#2:                                 # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	leaq	-1(%rbx), %rdi
	movq	$32, %rcx
	movq	sizetype_tab(%rip), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	sizetype_tab(%rip), %rsi
	movq	%rax, %r13
	movq	$1, %rdi
	callq	size_int_type_wide
	movq	$88, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movzbl	17(%r12), %ecx
	movq	%rax, %r13
	testq	$32, %rcx
	je	.LBB38_4
# BB#3:                                 # %if.then13
	movq	%r12, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %r13
.LBB38_4:                               # %if.end16
	movq	sizetype_tab(%rip), %rsi
	leaq	-1(%r15), %rdi
	callq	size_int_type_wide
	movq	$82, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %r13
	notq	%rbx
	addq	%r15, %rbx
	movq	$32, %rcx
	movq	sizetype_tab(%rip), %rsi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	callq	size_int_type_wide
	movq	$83, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
	movq	8(%rsp), %rsi           # 8-byte Reload
	testq	%rsi, %rsi
	je	.LBB38_6
# BB#5:                                 # %if.then28
	movq	8(%r14), %rdi
	callq	convert
	movq	$88, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	const_binop
	movq	%rax, %rbx
.LBB38_6:                               # %if.end33
	movzbl	17(%r12), %eax
	testq	$32, %rax
	je	.LBB38_8
# BB#7:                                 # %if.then40
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
.LBB38_8:                               # %if.end42
	movq	$87, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	const_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB38_9:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp507:
	.size	unextend, .Ltmp507-unextend
	.cfi_endproc

	.align	16, 0x90
	.type	make_range,@function
make_range:                             # @make_range
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp514:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp515:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp516:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp517:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp518:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp519:
	.cfi_def_cfa_offset 208
.Ltmp520:
	.cfi_offset %rbx, -48
.Ltmp521:
	.cfi_offset %r12, -40
.Ltmp522:
	.cfi_offset %r13, -32
.Ltmp523:
	.cfi_offset %r14, -24
.Ltmp524:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %rdi
	movq	global_trees+88(%rip), %rax
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r15
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	movabsq	$0, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	movq	%r15, %r12
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB39_1
.LBB39_30:                              # %sw.bb142
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	global_trees+88(%rip), %rsi
	movq	%r12, %rdi
	callq	convert
	movq	$60, %rdi
	xorq	%rcx, %rcx
	movq	$1, %r9
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	range_binop
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %r15
	movq	%r15, 144(%rsp)
	movq	%r12, %rdi
	callq	convert
	movq	$60, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r12, %rsi
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	%rax, %rdx
	movq	112(%rsp), %r8          # 8-byte Reload
	callq	range_binop
	movq	%rax, %r12
	movq	%r12, 136(%rsp)
	movq	96(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB39_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r14), %ebx
	movzbl	tree_code_type(%rbx), %eax
	movq	%r12, 120(%rsp)         # 8-byte Spill
	leaq	-49(%rax), %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	jbe	.LBB39_3
# BB#2:                                 #   in Loop: Header=BB39_1 Depth=1
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movabsq	$0, %r13
	movq	128(%rsp), %r12         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB39_14
	.align	16, 0x90
.LBB39_3:                               # %while.body
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rdx
	movq	128(%rsp), %r12         # 8-byte Reload
	movq	104(%rsp), %rsi         # 8-byte Reload
	je	.LBB39_4
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	32(%r14), %rcx
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	andq	$255, %rax
	cmpq	$60, %rax
	jbe	.LBB39_7
# BB#6:                                 #   in Loop: Header=BB39_1 Depth=1
	movq	%r14, 80(%rsp)          # 8-byte Spill
	jmp	.LBB39_9
.LBB39_7:                               # %if.then
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$1154610354467110912, %rcx # imm = 0x1006000000000000
	testq	%rcx, %rdx
	je	.LBB39_9
# BB#8:                                 # %if.then41
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r12
.LBB39_9:                               # %if.end
                                        #   in Loop: Header=BB39_1 Depth=1
	cmpq	$50, %rax
	movabsq	$0, %r13
	je	.LBB39_13
# BB#10:                                # %if.end
                                        #   in Loop: Header=BB39_1 Depth=1
	cmpq	$60, %rax
	je	.LBB39_13
# BB#11:                                # %if.end
                                        #   in Loop: Header=BB39_1 Depth=1
	cmpq	$101, %rax
	jne	.LBB39_14
# BB#12:                                # %land.lhs.true
                                        #   in Loop: Header=BB39_1 Depth=1
	movslq	tree_code_length(,%rbx,4), %rax
	cmpq	$2, %rax
	jl	.LBB39_14
	.align	16, 0x90
.LBB39_13:                              # %if.then65
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	jmp	.LBB39_14
.LBB39_4:                               #   in Loop: Header=BB39_1 Depth=1
	movq	%r14, 80(%rsp)          # 8-byte Spill
	movabsq	$0, %r13
	.align	16, 0x90
.LBB39_14:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%r12, %r12
	movq	%r13, %rax
	je	.LBB39_16
# BB#15:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_16:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%rsi, %rsi
	movq	%r13, %rcx
	jne	.LBB39_18
# BB#17:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rcx
.LBB39_18:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 112(%rsp)         # 8-byte Spill
	testq	%rcx, %rax
	movq	%r12, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jne	.LBB39_20
# BB#19:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rsi, %rax
.LBB39_20:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, %r15
	cmpq	$76, %rbx
	jg	.LBB39_28
# BB#21:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	leaq	-59(%rbx), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jae	.LBB39_83
# BB#22:                                # %sw.bb151
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB39_83
# BB#23:                                # %if.end159
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	cmpq	$60, %rbx
	movq	%r13, %rbx
	jne	.LBB39_25
# BB#24:                                # %if.end159
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rbx
.LBB39_25:                              # %if.end159
                                        #   in Loop: Header=BB39_1 Depth=1
	xorq	$1, %rbx
	addq	$59, %rbx
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	112(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %r14          # 8-byte Reload
	movq	%r14, %r8
	callq	range_binop
	movq	%rax, %r15
	movq	%r15, 144(%rsp)
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	%r14, %r8
	callq	range_binop
	movq	%rax, %r13
	testq	%r15, %r15
	movq	%r13, 136(%rsp)
	je	.LBB39_65
# BB#26:                                # %land.lhs.true169
                                        #   in Loop: Header=BB39_1 Depth=1
	movzbl	18(%r15), %eax
	testq	$8, %rax
	jne	.LBB39_27
.LBB39_65:                              # %lor.lhs.false175
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r12, 128(%rsp)         # 8-byte Spill
	xorq	%r12, %r12
	testq	%r13, %r13
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB39_1
# BB#66:                                # %land.lhs.true178
                                        #   in Loop: Header=BB39_1 Depth=1
	movzbl	18(%r13), %eax
	testq	$8, %rax
	jne	.LBB39_67
# BB#68:                                # %if.end186
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%r15, %r15
	movq	%r13, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB39_1
# BB#69:                                # %land.lhs.true190
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%r13, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB39_1
# BB#70:                                # %if.then193
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	global_trees+96(%rip), %r8
	movq	$59, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	range_binop
	movq	global_trees+96(%rip), %r8
	movq	$60, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%rbx, %rsi
	movq	%rax, %rbx
	movq	%r15, %rdx
	callq	range_binop
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB39_72
# BB#71:                                # %land.lhs.true198
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r13, %rdi
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	tree_int_cst_equal
	xorq	%r15, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	jne	.LBB39_1
.LBB39_72:                              # %if.else
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	120(%rsp), %rdx         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	movabsq	$0, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	jne	.LBB39_74
# BB#73:                                # %if.else
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB39_74:                              # %if.else
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rbx, %r15
	movq	%rdx, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	jmp	.LBB39_1
	.align	16, 0x90
.LBB39_28:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	cmpq	$89, %rbx
	jg	.LBB39_31
# BB#29:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	cmpq	$77, %rbx
	je	.LBB39_30
	jmp	.LBB39_83
	.align	16, 0x90
.LBB39_31:                              # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	leaq	-90(%rbx), %rax
	andq	%r14, %rax
	cmpq	$26, %rax
	ja	.LBB39_83
# BB#32:                                # %if.end70
                                        #   in Loop: Header=BB39_1 Depth=1
	jmpq	*.LJTI39_0(,%rax,8)
.LBB39_36:                              # %sw.bb78
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB39_83
# BB#37:                                # %sw.bb78
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r12, 128(%rsp)         # 8-byte Spill
	cmpq	$0, 120(%rsp)           # 8-byte Folded Reload
	je	.LBB39_83
# BB#38:                                # %lor.lhs.false84
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB39_83
# BB#39:                                # %lor.lhs.false87
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	integer_zerop
	testq	%r14, %rax
	je	.LBB39_83
# BB#40:                                # %lor.lhs.false90
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB39_83
# BB#41:                                # %if.end98
                                        #   in Loop: Header=BB39_1 Depth=1
	addq	$-97, %rbx
	andq	%r14, %rbx
	cmpq	$5, %rbx
	ja	.LBB39_111
# BB#42:                                # %if.end98
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r15
	movq	%rax, %r12
	jmpq	*.LJTI39_1(,%rbx,8)
.LBB39_50:                              # %sw.bb110
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	movq	%r13, %rax
	jne	.LBB39_52
# BB#51:                                # %sw.bb110
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_52:                              # %sw.bb110
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB39_53:                              # %sw.bb104
                                        #   in Loop: Header=BB39_1 Depth=1
	xorq	%r15, %r15
	movq	88(%rsp), %r12          # 8-byte Reload
	jmp	.LBB39_54
.LBB39_75:                              # %sw.bb208
                                        #   in Loop: Header=BB39_1 Depth=1
	movl	60(%r12), %eax
	movl	60(%r15), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	ja	.LBB39_83
# BB#76:                                # %if.end219
                                        #   in Loop: Header=BB39_1 Depth=1
	movzbl	16(%r12), %ecx
	cmpq	$12, %rcx
	movq	120(%rsp), %rbx         # 8-byte Reload
	ja	.LBB39_83
# BB#77:                                # %if.end219
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB39_83
# BB#78:                                # %lor.lhs.false247
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	112(%rsp), %r13         # 8-byte Reload
	testq	%r13, %r13
	je	.LBB39_80
# BB#79:                                # %land.lhs.true250
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	int_fits_type_p
	testq	%r14, %rax
	je	.LBB39_83
.LBB39_80:                              # %lor.lhs.false253
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%rbx, %rbx
	je	.LBB39_85
# BB#81:                                # %land.lhs.true256
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	int_fits_type_p
	testq	%r14, %rax
	je	.LBB39_82
.LBB39_85:                              # %if.end260
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	%r13, 144(%rsp)
	xorq	%r12, %r12
	testq	%r13, %r13
	movq	%r13, %r14
	movq	%rbx, 136(%rsp)
	movabsq	$0, %r13
	je	.LBB39_87
# BB#86:                                # %if.then263
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %r13
	movq	%r13, 144(%rsp)
.LBB39_87:                              # %if.end265
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%rbx, %rbx
	je	.LBB39_89
# BB#88:                                # %if.then268
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %r12
	movq	%r12, 136(%rsp)
.LBB39_89:                              # %if.end270
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB39_101
# BB#90:                                # %land.lhs.true277
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB39_101
# BB#91:                                # %if.then286
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	128(%rsp), %rbx         # 8-byte Reload
	movl	60(%rbx), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	$1, %rsi
	callq	type_for_mode
	movq	112(%rax), %rsi
	testq	%rsi, %rsi
	jne	.LBB39_93
# BB#92:                                # %cond.false
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	112(%rbx), %rsi
.LBB39_93:                              # %cond.end
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rbx, %rdi
	callq	convert
	movq	global_trees+96(%rip), %rsi
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$83, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	convert
	testq	%r14, %r14
	je	.LBB39_96
# BB#94:                                # %if.then305
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r14, 112(%rsp)         # 8-byte Spill
	movq	%r15, 16(%rsp)
	movq	%rax, 8(%rsp)
	movq	$1, %rcx
	movq	$1, (%rsp)
	leaq	156(%rsp), %rdi
	leaq	144(%rsp), %rsi
	leaq	136(%rsp), %rdx
	movq	%r13, %r8
	movq	%r12, %r9
	callq	merge_ranges
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB39_83
# BB#95:                                # %if.end310
                                        #   in Loop: Header=BB39_1 Depth=1
	movl	156(%rsp), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	je	.LBB39_99
	jmp	.LBB39_100
.LBB39_64:                              # %sw.bb147
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	negate_expr
	movq	global_trees+96(%rip), %rsi
	movq	%rax, %rbx
	movq	%r12, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %r14
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	120(%rsp), %r12         # 8-byte Reload
	jmp	.LBB39_1
.LBB39_33:                              # %sw.bb
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 104(%rsp)         # 8-byte Spill
	movq	%r12, 128(%rsp)         # 8-byte Spill
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	movq	%r13, %rax
	jne	.LBB39_35
# BB#34:                                # %sw.bb
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_35:                              # %sw.bb
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	112(%rsp), %r15         # 8-byte Reload
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	96(%rsp), %r14          # 8-byte Reload
	jmp	.LBB39_1
.LBB39_46:                              # %sw.bb105
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	movq	%r13, %rax
	jne	.LBB39_48
# BB#47:                                # %sw.bb105
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_48:                              # %sw.bb105
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB39_49:                              # %sw.bb109
                                        #   in Loop: Header=BB39_1 Depth=1
	xorq	%r12, %r12
	movq	88(%rsp), %r15          # 8-byte Reload
	jmp	.LBB39_54
.LBB39_43:                              # %sw.bb100
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	movq	%r13, %rax
	jne	.LBB39_45
# BB#44:                                # %sw.bb100
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_45:                              # %sw.bb100
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r15
	movq	%rax, %r12
.LBB39_54:                              # %sw.epilog
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	128(%rsp), %rax         # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB39_1
# BB#55:                                # %land.lhs.true118
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%r15, %r15
	je	.LBB39_57
# BB#56:                                # %land.lhs.true118
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%r12, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	jne	.LBB39_1
.LBB39_57:                              # %if.then124
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	global_trees+88(%rip), %rsi
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	convert
	movq	%rax, 8(%rsp)
	movq	$0, 16(%rsp)
	movq	$1, (%rsp)
	leaq	156(%rsp), %rdi
	leaq	144(%rsp), %rsi
	leaq	136(%rsp), %rdx
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r8
	movq	%r12, %r9
	callq	merge_ranges
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB39_58
# BB#59:                                # %if.end129
                                        #   in Loop: Header=BB39_1 Depth=1
	movl	156(%rsp), %eax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	136(%rsp), %r12
	movq	144(%rsp), %r15
	testq	%r12, %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	jne	.LBB39_1
# BB#60:                                # %if.end129
                                        #   in Loop: Header=BB39_1 Depth=1
	testq	%r15, %r15
	movq	96(%rsp), %r14          # 8-byte Reload
	je	.LBB39_1
# BB#61:                                # %if.then134
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	156(%rsp), %ecx
	testq	%rax, %rcx
	movabsq	$0, %rax
	jne	.LBB39_63
# BB#62:                                # %if.then134
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_63:                              # %if.then134
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	global_trees+96(%rip), %r8
	movq	$60, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r15, %rdx
	callq	range_binop
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %r12
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	convert
	movq	%rax, %r15
	movq	96(%rsp), %r14          # 8-byte Reload
	jmp	.LBB39_1
.LBB39_96:                              # %if.else313
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%r15, 16(%rsp)
	movq	%rax, 8(%rsp)
	movq	$1, (%rsp)
	xorq	%rcx, %rcx
	leaq	156(%rsp), %rdi
	leaq	144(%rsp), %rsi
	leaq	136(%rsp), %rdx
	movq	%r13, %r8
	movq	%r12, %r9
	callq	merge_ranges
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB39_97
# BB#98:                                # %if.end318
                                        #   in Loop: Header=BB39_1 Depth=1
	movl	156(%rsp), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	movabsq	$0, %rax
	je	.LBB39_100
.LBB39_99:                              # %if.end318
                                        #   in Loop: Header=BB39_1 Depth=1
	movabsq	$1, %rax
.LBB39_100:                             # %if.end318
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB39_101:                             # %if.end322
                                        #   in Loop: Header=BB39_1 Depth=1
	movq	144(%rsp), %r15
	movq	136(%rsp), %r12
	movq	96(%rsp), %r14          # 8-byte Reload
	jmp	.LBB39_1
.LBB39_27:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB39_83
.LBB39_67:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB39_83:                              # %while.end
	movq	80(%rsp), %rbx          # 8-byte Reload
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB39_84
# BB#102:                               # %if.then331
	movq	integer_types+40(%rip), %rsi
	movq	$100, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	xorq	%r15, %r15
	movq	%rbx, %rdx
	movq	112(%rsp), %r8          # 8-byte Reload
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB39_106
# BB#103:                               # %land.rhs
	movq	integer_types+40(%rip), %rsi
	movq	$98, %rdi
	movq	$1, %rcx
	movq	$1, %r9
	movq	%rbx, %rdx
	movq	120(%rsp), %r8          # 8-byte Reload
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r14, %rax
	je	.LBB39_104
# BB#105:                               # %land.rhs
	movabsq	$1, %r15
	jmp	.LBB39_106
.LBB39_84:
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	120(%rsp), %rdx         # 8-byte Reload
	movq	112(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB39_110
.LBB39_104:
	movabsq	$0, %r15
.LBB39_106:                             # %land.end
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%r14, %rax
	cmpq	%r15, %rax
	jne	.LBB39_107
# BB#108:                               # %land.end
	movabsq	$1, %rcx
	jmp	.LBB39_109
.LBB39_107:
	movabsq	$0, %rcx
.LBB39_109:                             # %land.end
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
.LBB39_110:                             # %if.end340
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	%ecx, (%rax)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rsi, (%rax)
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rdx, (%rax)
	movq	%rbx, %rax
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB39_111:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.make_range, %rdx
	movq	$3505, %rsi             # imm = 0xDB1
	callq	fancy_abort
.LBB39_58:
	movq	%r15, 112(%rsp)         # 8-byte Spill
	movq	%r12, 120(%rsp)         # 8-byte Spill
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	%rax, 80(%rsp)          # 8-byte Spill
	jmp	.LBB39_83
.LBB39_82:
	movq	%r13, 112(%rsp)         # 8-byte Spill
	jmp	.LBB39_83
.LBB39_97:
	movq	$0, 112(%rsp)           # 8-byte Folded Spill
	jmp	.LBB39_83
.Ltmp525:
	.size	make_range, .Ltmp525-make_range
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI39_0:
	.quad	.LBB39_64
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_33
	.quad	.LBB39_36
	.quad	.LBB39_36
	.quad	.LBB39_36
	.quad	.LBB39_36
	.quad	.LBB39_36
	.quad	.LBB39_36
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_83
	.quad	.LBB39_75
	.quad	.LBB39_75
	.quad	.LBB39_75
.LJTI39_1:
	.quad	.LBB39_49
	.quad	.LBB39_50
	.quad	.LBB39_53
	.quad	.LBB39_46
	.quad	.LBB39_43
	.quad	.LBB39_54

	.text
	.align	16, 0x90
	.type	merge_ranges,@function
merge_ranges:                           # @merge_ranges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp532:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp533:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp534:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp535:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp536:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp537:
	.cfi_def_cfa_offset 128
.Ltmp538:
	.cfi_offset %rbx, -48
.Ltmp539:
	.cfi_offset %r12, -40
.Ltmp540:
	.cfi_offset %r13, -32
.Ltmp541:
	.cfi_offset %r14, -24
.Ltmp542:
	.cfi_offset %r15, -16
	movq	136(%rsp), %r13
	movq	%r9, %r15
	movq	%r8, %r14
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	%r14, %rax
	orq	%r13, %rax
	movq	$1, %rbx
	movq	$1, %rax
	je	.LBB40_4
# BB#1:                                 # %lor.rhs
	movq	integer_types+40(%rip), %rsi
	movq	$101, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r14, %rdx
	movq	%r13, %r8
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB40_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB40_4
.LBB40_2:
	movabsq	$0, %rax
.LBB40_4:                               # %lor.end
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	144(%rsp), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	movq	%r15, %rax
	orq	%rcx, %rax
	je	.LBB40_8
# BB#5:                                 # %lor.rhs6
	movq	integer_types+40(%rip), %rsi
	movq	$101, %rdi
	movq	$1, %rcx
	movq	$1, %r9
	movq	%r15, %rdx
	movq	144(%rsp), %r8
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB40_6
# BB#7:                                 # %lor.rhs6
	movabsq	$1, %rbx
	jmp	.LBB40_8
.LBB40_6:
	movabsq	$0, %rbx
.LBB40_8:                               # %lor.end10
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	128(%rsp), %rbx
	movq	integer_types+40(%rip), %rsi
	movq	$99, %rdi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	%r14, %rdx
	movq	%r13, %r8
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB40_12
# BB#9:                                 # %lor.lhs.false
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB40_10
# BB#11:                                # %land.lhs.true16
	movq	integer_types+40(%rip), %rsi
	movq	$99, %rdi
	movq	$1, %rcx
	movq	$1, %r9
	movq	144(%rsp), %rdx
	movq	%r15, %r8
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB40_10
.LBB40_12:                              # %if.then
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	%r15, 64(%rsp)          # 8-byte Spill
	movq	%r14, %r8
	movq	56(%rsp), %r13          # 8-byte Reload
	jmp	.LBB40_13
.LBB40_10:
	movq	144(%rsp), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r13, %r8
	movq	%rbx, %r13
	movq	%r15, 72(%rsp)          # 8-byte Spill
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rbx          # 8-byte Reload
.LBB40_13:                              # %if.end
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	integer_types+40(%rip), %rsi
	movq	$97, %rdi
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	72(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdx
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	movq	integer_types+40(%rip), %rsi
	movq	%rax, %r14
	movq	$98, %rdi
	movq	$1, %rcx
	movq	$1, %r9
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%r15, %r8
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r12, %rbx
	je	.LBB40_19
# BB#14:                                # %if.end
	testq	%r12, %r13
	je	.LBB40_19
# BB#15:                                # %if.then27
	xorq	%rbx, %rbx
	testq	%r12, %r14
	movabsq	$0, %rcx
	movabsq	$0, %rdx
	jne	.LBB40_50
# BB#16:                                # %if.else
	testq	%r12, %rax
	je	.LBB40_18
# BB#17:                                # %if.else
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB40_18:                              # %if.else
	movq	$1, %rbx
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB40_50
.LBB40_19:                              # %if.else35
	testq	%r12, %rbx
	je	.LBB40_34
# BB#20:                                # %if.else35
	testq	%r12, %r13
	jne	.LBB40_34
# BB#21:                                # %if.then39
	movq	$1, %rbx
	testq	%r12, %r14
	je	.LBB40_23
# BB#22:
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	72(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB40_50
.LBB40_34:                              # %if.else64
	testq	%r12, %rbx
	jne	.LBB40_43
# BB#35:                                # %if.else64
	testq	%r12, %r13
	je	.LBB40_43
# BB#36:                                # %if.then68
	movq	$1, %rbx
	testq	%r12, %r14
	je	.LBB40_38
# BB#37:
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB40_49
.LBB40_43:                              # %if.else80
	testq	%r12, %r14
	je	.LBB40_45
# BB#44:                                # %if.then82
	movq	integer_types+40(%rip), %r14
	movq	global_trees+96(%rip), %r8
	movq	$59, %rdi
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	$1, %r9
	xorq	%rbx, %rbx
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	range_binop
	movq	$101, %rdi
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	%r14, %rsi
	movq	%rax, %rdx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	range_binop
	movq	%rax, %rdi
	callq	integer_onep
	testq	%r12, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	jne	.LBB40_50
	jmp	.LBB40_51
.LBB40_23:                              # %if.else42
	xorq	%rbx, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%rcx, %rdi
	movabsq	$0, %rcx
	movabsq	$0, %rdx
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB40_50
# BB#24:                                # %if.else47
	testq	%r12, %rax
	je	.LBB40_25
# BB#26:                                # %if.else47
	movabsq	$1, %rcx
	jmp	.LBB40_27
.LBB40_38:                              # %if.else71
	testq	%r12, %rax
	je	.LBB40_39
# BB#40:                                # %if.else71
	movabsq	$1, %rax
	jmp	.LBB40_41
.LBB40_45:                              # %if.else90
	testq	%r12, %rax
	movq	16(%rsp), %rax          # 8-byte Reload
	je	.LBB40_47
# BB#46:                                # %if.else90
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB40_47:                              # %if.else90
	xorq	%rbx, %rbx
	jmp	.LBB40_48
.LBB40_25:
	movabsq	$0, %rcx
.LBB40_27:                              # %if.else47
	andq	%rdi, %rcx
	cmpq	$1, %rcx
	jne	.LBB40_29
# BB#28:                                # %if.then51
	movq	global_trees+96(%rip), %r8
	movq	$59, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	64(%rsp), %rdx          # 8-byte Reload
	callq	range_binop
	movq	%rax, %rcx
	movq	$1, %rbx
	movq	72(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB40_50
.LBB40_39:
	movabsq	$0, %rax
.LBB40_41:                              # %if.else71
	xorq	%rbx, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	orq	%rcx, %rax
	movabsq	$0, %rcx
	movabsq	$0, %rdx
	jne	.LBB40_50
# BB#42:                                # %if.else76
	movq	global_trees+96(%rip), %r8
	movq	$59, %rdi
	xorq	%rsi, %rsi
	movq	$1, %rcx
	xorq	%r9, %r9
	movq	$1, %rbx
	movq	72(%rsp), %rdx          # 8-byte Reload
	callq	range_binop
.LBB40_48:                              # %if.end98
	movq	%rax, %rcx
.LBB40_49:                              # %if.end98
	movq	64(%rsp), %rdx          # 8-byte Reload
.LBB40_50:                              # %if.end98
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	%ebx, (%rax)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rcx, (%rax)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%rdx, (%rax)
	movq	$1, %rbx
.LBB40_51:                              # %return
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB40_29:                              # %if.else53
	testq	%r12, %rax
	movq	%rsi, %r14
	jne	.LBB40_30
# BB#31:                                # %if.else53
	movabsq	$1, %rax
	jmp	.LBB40_32
.LBB40_30:
	movabsq	$0, %rax
.LBB40_32:                              # %if.else53
	xorq	%rbx, %rbx
	movq	8(%rsp), %rcx           # 8-byte Reload
	orq	%rcx, %rax
	je	.LBB40_51
# BB#33:                                # %if.then57
	movq	global_trees+96(%rip), %r8
	movq	$60, %rdi
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	xorq	%r9, %r9
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	range_binop
	movq	%rax, %rdx
	movq	$1, %rbx
	movq	%r14, %rcx
	jmp	.LBB40_50
.Ltmp543:
	.size	merge_ranges, .Ltmp543-merge_ranges
	.cfi_endproc

	.align	16, 0x90
	.type	build_range_check,@function
build_range_check:                      # @build_range_check
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp550:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp551:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp552:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp553:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp554:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp555:
	.cfi_def_cfa_offset 80
.Ltmp556:
	.cfi_offset %rbx, -48
.Ltmp557:
	.cfi_offset %r12, -40
.Ltmp558:
	.cfi_offset %r13, -32
.Ltmp559:
	.cfi_offset %r14, -24
.Ltmp560:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r12
	movq	%rsi, %rcx
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	jne	.LBB41_1
# BB#2:                                 # %entry
	movabsq	$1, %rdx
	jmp	.LBB41_3
.LBB41_1:
	movabsq	$0, %rdx
	jmp	.LBB41_3
	.align	16, 0x90
.LBB41_23:                              # %if.then49
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	movq	%r13, %rdx
	movq	%r12, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	global_trees+88(%rip), %rsi
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	convert
	xorq	%rdx, %rdx
	movq	%rbx, %rcx
	movq	%rax, %r12
.LBB41_3:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	$1, %rdx
	je	.LBB41_5
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	$1, %rdx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	movq	%rcx, %rbx
	movq	%r12, %rcx
	movq	%r14, %r8
	callq	build_range_check
	movq	%rbx, %rcx
	testq	%rax, %rax
	jne	.LBB41_25
.LBB41_5:                               # %if.else
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	%r12, %rax
	orq	%r14, %rax
	je	.LBB41_26
# BB#6:                                 # %if.else8
                                        #   in Loop: Header=BB41_3 Depth=1
	testq	%r12, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB41_7
# BB#10:                                # %if.else13
                                        #   in Loop: Header=BB41_3 Depth=1
	testq	%r14, %r14
	je	.LBB41_11
# BB#13:                                # %if.else18
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	%rcx, %r13
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	operand_equal_p
	testq	%rbx, %rax
	jne	.LBB41_14
# BB#15:                                # %if.else24
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB41_18
# BB#16:                                # %land.lhs.true27
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	jne	.LBB41_17
.LBB41_18:                              # %if.else32
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	%r12, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	jne	.LBB41_19
# BB#21:                                # %if.else40
                                        #   in Loop: Header=BB41_3 Depth=1
	movq	$60, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	const_binop
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB41_24
# BB#22:                                # %land.lhs.true43
                                        #   in Loop: Header=BB41_3 Depth=1
	movzbl	18(%r14), %eax
	testq	$8, %rax
	je	.LBB41_23
.LBB41_24:                              # %return
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB41_7:                               # %if.then10
	movq	$98, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	jmp	.LBB41_8
.LBB41_11:                              # %if.then15
	movq	$100, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, %rdx
	movq	%r12, %rcx
	jmp	.LBB41_9
.LBB41_14:                              # %if.then21
	movq	$101, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	movq	%r12, %rcx
	jmp	.LBB41_9
.LBB41_19:                              # %if.then35
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	unsigned_type
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	%rax, %r15
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB41_26
# BB#20:                                # %if.else8.i
	movq	$98, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r15, %rdx
	jmp	.LBB41_9
.LBB41_26:                              # %if.then6
	movq	global_trees+96(%rip), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB41_25:                              # %if.then
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	invert_truthvalue  # TAILCALL
.LBB41_17:                              # %if.else8.i95
	movq	$98, %rdi
	xorq	%rax, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
.LBB41_8:                               # %if.then10
	movq	%r14, %rcx
.LBB41_9:                               # %if.then10
	callq	build
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.Ltmp561:
	.size	build_range_check, .Ltmp561-build_range_check
	.cfi_endproc

	.align	16, 0x90
	.type	range_binop,@function
range_binop:                            # @range_binop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp566:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp567:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp568:
	.cfi_def_cfa_offset 32
.Ltmp569:
	.cfi_offset %rbx, -32
.Ltmp570:
	.cfi_offset %r14, -24
.Ltmp571:
	.cfi_offset %r15, -16
	movq	%rcx, %r10
	movq	%rdx, %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB42_11
# BB#1:                                 # %entry
	testq	%r8, %r8
	je	.LBB42_11
# BB#2:                                 # %if.then
	movq	8(%rbx), %rdi
	testq	%rsi, %rsi
	movq	%rdi, %r15
	je	.LBB42_4
# BB#3:                                 # %if.then
	movq	%rsi, %r15
.LBB42_4:                               # %if.then
	movq	%r8, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	global_trees(%rip), %rdx
	movq	%rax, %rcx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB42_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movzbl	16(%rax), %edi
	leaq	-114(%rdi), %rcx
	andq	%r8, %rcx
	cmpq	$2, %rcx
	ja	.LBB42_8
# BB#6:                                 # %land.lhs.true22
                                        #   in Loop: Header=BB42_5 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB42_8
# BB#7:                                 # %land.rhs
                                        #   in Loop: Header=BB42_5 Depth=1
	movq	8(%rax), %rbx
	movq	8(%rcx), %rsi
	movl	60(%rbx), %ebx
	movl	60(%rsi), %esi
	xorq	%rbx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB42_5
.LBB42_8:                               # %while.end
	cmpq	$25, %rdi
	je	.LBB42_10
# BB#9:                                 # %while.end
	xorq	%rax, %rax
	jmp	.LBB42_10
.LBB42_11:                              # %if.end
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	tree_code_type(%rax), %ecx
	xorq	%rax, %rax
	cmpq	$60, %rcx
	jne	.LBB42_10
# BB#12:                                # %if.end56
	xorq	%rax, %rax
	testq	%rbx, %rbx
	movabsq	$0, %rcx
	jne	.LBB42_14
# BB#13:                                # %cond.false60
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r10
	cmpq	$1, %r10
	sbbq	%rcx, %rcx
	orq	$1, %rcx
.LBB42_14:                              # %cond.end62
	testq	%r8, %r8
	jne	.LBB42_16
# BB#15:                                # %cond.false67
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r9
	cmpq	$1, %r9
	sbbq	%rax, %rax
	orq	$1, %rax
.LBB42_16:                              # %cond.end70
	addq	$-97, %r14
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %r14
	cmpq	$5, %r14
	ja	.LBB42_35
# BB#17:                                # %cond.end70
	jmpq	*.LJTI42_0(,%r14,8)
.LBB42_22:                              # %sw.bb77
	cmpq	%rax, %rcx
	jl	.LBB42_30
# BB#23:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_10:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB42_24:                              # %sw.bb80
	cmpq	%rax, %rcx
	jle	.LBB42_30
# BB#25:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_26:                              # %sw.bb83
	cmpq	%rax, %rcx
	jg	.LBB42_30
# BB#27:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_28:                              # %sw.bb86
	cmpq	%rax, %rcx
	jge	.LBB42_30
# BB#29:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_18:                              # %sw.bb
	andq	%rdx, %rax
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	je	.LBB42_30
# BB#19:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_20:                              # %sw.bb74
	andq	%rdx, %rax
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	je	.LBB42_21
.LBB42_30:                              # %sw.bb86
	movabsq	$1, %rax
.LBB42_31:                              # %sw.epilog
	testq	%rax, %rax
	jne	.LBB42_32
# BB#33:                                # %sw.epilog
	movabsq	$global_trees+88, %rax
	jmp	.LBB42_34
.LBB42_32:
	movabsq	$global_trees+96, %rax
.LBB42_34:                              # %sw.epilog
	movq	(%rax), %rax
	movq	%rsi, %rdi
	movq	%rax, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	convert  # TAILCALL
.LBB42_21:
	movabsq	$0, %rax
	jmp	.LBB42_31
.LBB42_35:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.range_binop, %rdx
	movq	$3411, %rsi             # imm = 0xD53
	callq	fancy_abort
.Ltmp572:
	.size	range_binop, .Ltmp572-range_binop
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI42_0:
	.quad	.LBB42_22
	.quad	.LBB42_24
	.quad	.LBB42_26
	.quad	.LBB42_28
	.quad	.LBB42_18
	.quad	.LBB42_20

	.text
	.align	16, 0x90
	.type	const_binop_1,@function
const_binop_1:                          # @const_binop_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp576:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp577:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp578:
	.cfi_def_cfa_offset 96
.Ltmp579:
	.cfi_offset %rbx, -24
.Ltmp580:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	(%rbx), %esi
	leaq	16(%rbx), %rdx
	leaq	40(%rbx), %rcx
	leaq	48(%rsp), %rdi
	callq	earith
	movq	8(%rbx), %r14
	movl	60(%r14), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	64(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	48(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	leaq	24(%rsp), %rdi
	movq	%rax, (%rsp)
	callq	real_value_truncate
	movq	40(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	24(%rsp), %rax
	movq	32(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	movq	%rax, 64(%rbx)
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp581:
	.size	const_binop_1, .Ltmp581-const_binop_1
	.cfi_endproc

	.align	16, 0x90
	.type	fold_convert_1,@function
fold_convert_1:                         # @fold_convert_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp585:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp586:
	.cfi_def_cfa_offset 24
	subq	$56, %rsp
.Ltmp587:
	.cfi_def_cfa_offset 80
.Ltmp588:
	.cfi_offset %rbx, -24
.Ltmp589:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %r14
	movl	60(%r14), %esi
	movq	(%rbx), %rax
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	48(%rax), %rcx
	movq	%rcx, 16(%rsp)
	movq	32(%rax), %rcx
	movq	40(%rax), %rax
	movq	%rax, 8(%rsp)
	leaq	32(%rsp), %rdi
	movq	%rcx, (%rsp)
	callq	real_value_truncate
	movq	48(%rsp), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rsp), %rax
	movq	40(%rsp), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	movq	%r14, %rdi
	callq	build_real
	movq	%rax, 16(%rbx)
	addq	$56, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp590:
	.size	fold_convert_1, .Ltmp590-fold_convert_1
	.cfi_endproc

	.align	16, 0x90
	.type	strip_compound_expr,@function
strip_compound_expr:                    # @strip_compound_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp594:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp595:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp596:
	.cfi_def_cfa_offset 32
.Ltmp597:
	.cfi_offset %rbx, -24
.Ltmp598:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rsi, %r14
	cmpq	$51, %rax
	jne	.LBB45_1
# BB#5:                                 # %if.then19
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 32(%rbx)
	movq	40(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 40(%rbx)
	movq	48(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 48(%rbx)
	jmp	.LBB45_11
.LBB45_1:                               # %entry
	cmpq	$47, %rax
	jne	.LBB45_6
# BB#2:                                 # %land.lhs.true
	movq	32(%rbx), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$114, %rdx
	jne	.LBB45_6
# BB#3:                                 # %land.lhs.true7
	movq	32(%rcx), %rcx
	cmpq	%r14, %rcx
	je	.LBB45_4
.LBB45_6:                               # %if.else40
	movzbl	tree_code_type(%rax), %eax
	cmpq	$60, %rax
	je	.LBB45_10
# BB#7:                                 # %if.else40
	cmpq	$50, %rax
	jne	.LBB45_8
.LBB45_10:                              # %if.then63
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 32(%rbx)
	movq	40(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 40(%rbx)
	jmp	.LBB45_11
.LBB45_8:                               # %if.else40
	cmpq	$49, %rax
	jne	.LBB45_11
# BB#9:                                 # %if.then44
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	strip_compound_expr
	movq	%rax, 32(%rbx)
	jmp	.LBB45_11
.LBB45_4:                               # %if.then
	movq	40(%rbx), %rbx
.LBB45_11:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp599:
	.size	strip_compound_expr, .Ltmp599-strip_compound_expr
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"fold-const.c"
	.size	.L.str, 13

	.type	.L__FUNCTION__.div_and_round_double,@object # @__FUNCTION__.div_and_round_double
.L__FUNCTION__.div_and_round_double:
	.asciz	"div_and_round_double"
	.size	.L__FUNCTION__.div_and_round_double, 21

	.type	size_int_type_wide.size_htab,@object # @size_int_type_wide.size_htab
	.local	size_int_type_wide.size_htab
	.comm	size_int_type_wide.size_htab,8,8
	.type	size_int_type_wide.new_const,@object # @size_int_type_wide.new_const
	.local	size_int_type_wide.new_const
	.comm	size_int_type_wide.new_const,8,8
	.type	.L__FUNCTION__.size_binop,@object # @__FUNCTION__.size_binop
.L__FUNCTION__.size_binop:
	.asciz	"size_binop"
	.size	.L__FUNCTION__.size_binop, 11

	.type	.L__FUNCTION__.size_diffop,@object # @__FUNCTION__.size_diffop
.L__FUNCTION__.size_diffop:
	.asciz	"size_diffop"
	.size	.L__FUNCTION__.size_diffop, 12

	.type	pedantic_lvalues,@object # @pedantic_lvalues
	.comm	pedantic_lvalues,4,4
	.type	.L__FUNCTION__.invert_truthvalue,@object # @__FUNCTION__.invert_truthvalue
.L__FUNCTION__.invert_truthvalue:
	.asciz	"invert_truthvalue"
	.size	.L__FUNCTION__.invert_truthvalue, 18

	.type	.L__FUNCTION__.fold,@object # @__FUNCTION__.fold
.L__FUNCTION__.fold:
	.asciz	"fold"
	.size	.L__FUNCTION__.fold, 5

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"comparison is always %d due to width of bit-field"
	.size	.L.str1, 50

	.type	.L__FUNCTION__.swap_tree_comparison,@object # @__FUNCTION__.swap_tree_comparison
.L__FUNCTION__.swap_tree_comparison:
	.asciz	"swap_tree_comparison"
	.size	.L__FUNCTION__.swap_tree_comparison, 21

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"comparison is always %d"
	.size	.L.str2, 24

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"`or' of unmatched not-equal tests is always 1"
	.size	.L.str3, 46

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"`and' of mutually exclusive equal-tests is always 0"
	.size	.L.str4, 52

	.type	.L__FUNCTION__.range_binop,@object # @__FUNCTION__.range_binop
.L__FUNCTION__.range_binop:
	.asciz	"range_binop"
	.size	.L__FUNCTION__.range_binop, 12

	.type	.L__FUNCTION__.make_range,@object # @__FUNCTION__.make_range
.L__FUNCTION__.make_range:
	.asciz	"make_range"
	.size	.L__FUNCTION__.make_range, 11

	.type	.L__FUNCTION__.const_binop,@object # @__FUNCTION__.const_binop
.L__FUNCTION__.const_binop:
	.asciz	"const_binop"
	.size	.L__FUNCTION__.const_binop, 12

	.type	.L__FUNCTION__.invert_tree_comparison,@object # @__FUNCTION__.invert_tree_comparison
.L__FUNCTION__.invert_tree_comparison:
	.asciz	"invert_tree_comparison"
	.size	.L__FUNCTION__.invert_tree_comparison, 23

	.type	.L__FUNCTION__.int_const_binop,@object # @__FUNCTION__.int_const_binop
.L__FUNCTION__.int_const_binop:
	.asciz	"int_const_binop"
	.size	.L__FUNCTION__.int_const_binop, 16


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
