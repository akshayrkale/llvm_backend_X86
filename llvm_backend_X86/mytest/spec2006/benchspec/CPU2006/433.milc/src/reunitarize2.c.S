	.file	"reunitarize2.c.bc"
	.text
	.globl	check_deviation
	.align	16, 0x90
	.type	check_deviation,@function
check_deviation:                        # @check_deviation
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
	movq	max_deviation(%rip), %r14
	movq	%rdi, %rbx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r13
	movq	%r15, %r12
	je	.LBB0_2
# BB#1:                                 # %entry
	movq	%r13, %r12
.LBB0_2:                                # %entry
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB0_4
# BB#3:                                 # %entry
	movq	%r13, %r15
.LBB0_4:                                # %entry
	orq	%r15, %r12
	jne	.LBB0_6
# BB#5:                                 # %if.then
	movq	%rbx, max_deviation(%rip)
.LBB0_6:                                # %if.end
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	av_deviation(%rip), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, av_deviation(%rip)
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	movq	%rbx, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_7
# BB#8:                                 # %if.end
	movabsq	$1, %rax
	jmp	.LBB0_9
.LBB0_7:
	movabsq	$0, %rax
.LBB0_9:                                # %if.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	check_deviation, .Ltmp16-check_deviation
	.cfi_endproc

	.globl	reunit_report_problem_matrix
	.align	16, 0x90
	.type	reunit_report_problem_matrix,@function
reunit_report_problem_matrix:           # @reunit_report_problem_matrix
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 32
.Ltmp24:
	.cfi_offset %rbx, -32
.Ltmp25:
	.cfi_offset %r14, -24
.Ltmp26:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	xorq	%rax, %rax
	callq	mynode
	movq	%rax, %rcx
	movabsq	$.L.str, %rdi
	movabsq	$4547007122018943789, %r8 # imm = 0x3F1A36E2EB1C432D
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	printf
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	(%rbx), %rsi
	movabsq	$.L.str2, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	8(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	16(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	24(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	32(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	40(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	48(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	56(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	64(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	72(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	80(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	88(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	96(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	104(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	112(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	120(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	128(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	136(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movabsq	$.Lstr9, %rdi
	callq	puts
	movq	(%rbx), %rsi
	movabsq	$.L.str5, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	8(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	16(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	24(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	32(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	40(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	48(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	56(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	64(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	72(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	80(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	88(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	96(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	104(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	112(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	120(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	128(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	136(%rbx), %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movabsq	$.Lstr10, %rdi
	callq	puts
	movq	stdout(%rip), %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fflush  # TAILCALL
.Ltmp27:
	.size	reunit_report_problem_matrix, .Ltmp27-reunit_report_problem_matrix
	.cfi_endproc

	.globl	reunit_su3
	.align	16, 0x90
	.type	reunit_su3,@function
reunit_su3:                             # @reunit_su3
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp34:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp35:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp36:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 48
	subq	$192, %rsp
.Ltmp39:
	.cfi_def_cfa_offset 240
.Ltmp40:
	.cfi_offset %rbx, -48
.Ltmp41:
	.cfi_offset %r12, -40
.Ltmp42:
	.cfi_offset %r13, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rdi
	movq	8(%r14), %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	16(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	24(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	40(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	%rax, %rdi
	callq	float64_add
	movq	max_deviation(%rip), %r12
	movq	%rax, %rbx
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %rax
	movq	%r15, %r13
	je	.LBB2_2
# BB#1:                                 # %entry
	movq	%rax, %r13
.LBB2_2:                                # %entry
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_4
# BB#3:                                 # %entry
	movabsq	$1, %r15
.LBB2_4:                                # %entry
	orq	%r15, %r13
	jne	.LBB2_6
# BB#5:                                 # %if.then.i
	movq	%rbx, max_deviation(%rip)
.LBB2_6:                                # %check_deviation.exit
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	av_deviation(%rip), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, av_deviation(%rip)
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	movq	%rbx, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jle	.LBB2_8
# BB#7:                                 # %check_deviation.exit
	movabsq	$1, %rax
.LBB2_8:                                # %check_deviation.exit
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	sqrt
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%rax, %rsi
	callq	float64_div
	movq	(%r14), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	%rax, (%r14)
	movq	%rax, %r13
	movq	8(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 8(%r14)
	movq	%rax, %r15
	movq	16(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rax, 16(%r14)
	movq	24(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	%rax, 24(%r14)
	movq	32(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%rax, 32(%r14)
	movq	40(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%rax, 40(%r14)
	movq	48(%r14), %rsi
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	movq	56(%r14), %rbx
	movq	%rbx, 96(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	64(%r14), %rsi
	movq	%rsi, 144(%rsp)         # 8-byte Spill
	movq	%rax, %rbx
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	72(%r14), %r12
	movq	%rax, %rbx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	80(%r14), %rsi
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movq	%rax, %rbx
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	88(%r14), %r13
	movq	%rax, %rbx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r15
	movq	%r15, 184(%rsp)         # 8-byte Spill
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, %rbx
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%rbx, 176(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	max_deviation(%rip), %r15
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r13
	movq	%r13, %r12
	je	.LBB2_10
# BB#9:                                 # %check_deviation.exit
	movabsq	$1, %r12
.LBB2_10:                               # %check_deviation.exit
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_12
# BB#11:                                # %check_deviation.exit
	movabsq	$1, %r13
.LBB2_12:                               # %check_deviation.exit
	orq	%r13, %r12
	jne	.LBB2_14
# BB#13:                                # %if.then.i869
	movq	%rbx, max_deviation(%rip)
.LBB2_14:                               # %check_deviation.exit874
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	av_deviation(%rip), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, av_deviation(%rip)
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	movq	%rbx, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	jle	.LBB2_16
# BB#15:                                # %check_deviation.exit874
	movabsq	$1, %rax
.LBB2_16:                               # %check_deviation.exit874
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	(%r14), %r15
	movq	8(%r14), %r13
	movq	184(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %r12
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	48(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	%rax, 48(%r14)
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	56(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rax, 56(%r14)
	movq	16(%r14), %rsi
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	movq	24(%r14), %r13
	movq	184(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	64(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rax, 64(%r14)
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	72(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rax, 72(%r14)
	movq	32(%r14), %rsi
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	movq	40(%r14), %r12
	movq	184(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	callq	float64_mul
	movq	%rax, %r13
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	80(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r13
	movq	%r13, 80(%r14)
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	176(%rsp), %rdi         # 8-byte Reload
	movq	128(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	88(%r14), %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	%r15, 88(%r14)
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, %r15
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	%r15, %rdi
	callq	float64_add
	movq	max_deviation(%rip), %r12
	movq	%rax, %rbx
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	andq	%rax, %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r13
	je	.LBB2_18
# BB#17:                                # %check_deviation.exit874
	movabsq	$1, %r13
.LBB2_18:                               # %check_deviation.exit874
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB2_20
# BB#19:                                # %check_deviation.exit874
	movabsq	$1, %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
.LBB2_20:                               # %check_deviation.exit874
	movq	168(%rsp), %rax         # 8-byte Reload
	orq	%rax, %r13
	jne	.LBB2_22
# BB#21:                                # %if.then.i862
	movq	%rbx, max_deviation(%rip)
.LBB2_22:                               # %check_deviation.exit867
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	av_deviation(%rip), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, av_deviation(%rip)
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	movq	%rbx, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jle	.LBB2_24
# BB#23:                                # %check_deviation.exit867
	movabsq	$1, %rax
.LBB2_24:                               # %check_deviation.exit867
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	sqrt
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	movq	%rax, %rsi
	callq	float64_div
	movq	48(%r14), %rsi
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	%rax, 48(%r14)
	movq	56(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rax, 56(%r14)
	movq	64(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rax, 64(%r14)
	movq	72(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	%rax, 72(%r14)
	movq	80(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	%rax, 80(%r14)
	movq	%rax, %r15
	movq	88(%r14), %rsi
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rax, 88(%r14)
	movq	%rax, %r12
	movq	96(%r14), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	104(%r14), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	112(%r14), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	120(%r14), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	128(%r14), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	136(%r14), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	(%r14), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	8(%r14), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	16(%r14), %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	24(%r14), %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	32(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%r14), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%rdi, %r13
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	8(%rsp), %r12           # 8-byte Reload
	movq	%r12, %rdi
	movq	120(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	136(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rdi
	movq	128(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_mul
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 96(%r14)
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 104(%r14)
	movq	%r12, %rdi
	movq	184(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r15, %rdi
	movq	152(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	144(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	160(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 112(%r14)
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 120(%r14)
	movq	%r13, %rdi
	movq	120(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	160(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	128(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	152(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	float64_mul
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %r12
	movq	144(%rsp), %r15         # 8-byte Reload
	movq	%r15, 128(%r14)
	movabsq	$-9223372036854775808, %rdi # imm = 0x8000000000000000
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, 136(%r14)
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	float64_sub
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rbx
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_add
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	136(%rsp), %rsi         # 8-byte Reload
	callq	float64_sub
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	float64_add
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_sub
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_add
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	max_deviation(%rip), %r14
	movq	%rax, %r12
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r13
	movq	%r13, %r15
	je	.LBB2_26
# BB#25:                                # %check_deviation.exit867
	movabsq	$1, %r15
.LBB2_26:                               # %check_deviation.exit867
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	104(%rsp), %rbx         # 8-byte Reload
	js	.LBB2_28
# BB#27:                                # %check_deviation.exit867
	movabsq	$1, %r13
.LBB2_28:                               # %check_deviation.exit867
	movq	112(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rbx
	orq	%r13, %r15
	jne	.LBB2_30
# BB#29:                                # %if.then.i855
	movq	%r12, max_deviation(%rip)
.LBB2_30:                               # %check_deviation.exit860
	movq	96(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	av_deviation(%rip), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, av_deviation(%rip)
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	movq	%r12, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_31
# BB#32:                                # %check_deviation.exit860
	movabsq	$1, %rax
	jmp	.LBB2_33
.LBB2_31:
	movabsq	$0, %rax
.LBB2_33:                               # %check_deviation.exit860
	addq	%rax, %rbx
	movq	%rbx, %rax
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	reunit_su3, .Ltmp45-reunit_su3
	.cfi_endproc

	.globl	reunitarize
	.align	16, 0x90
	.type	reunitarize,@function
reunitarize:                            # @reunitarize
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp52:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp53:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp54:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp55:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp56:
	.cfi_def_cfa_offset 48
.Ltmp57:
	.cfi_offset %rbx, -48
.Ltmp58:
	.cfi_offset %r12, -40
.Ltmp59:
	.cfi_offset %r13, -32
.Ltmp60:
	.cfi_offset %r14, -24
.Ltmp61:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %rax
	movq	$0, max_deviation(%rip)
	movq	$0, av_deviation(%rip)
	testq	%rax, %rax
	jle	.LBB3_25
# BB#1:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %r12
	addq	$544, %r12              # imm = 0x220
	xorq	%r14, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_2:                                # %for.cond1.preheader
                                        # =>This Inner Loop Header: Depth=1
	leaq	-432(%r12), %rbx
	movq	%rbx, %rdi
	callq	reunit_su3
	addq	%rax, %r13
	testq	%r15, %rax
	je	.LBB3_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB3_2 Depth=1
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reunit_report_problem_matrix
.LBB3_4:                                # %if.end
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$101, %rax
	jl	.LBB3_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$.Lstr12, %rdi
	callq	puts
	movq	$1, %rdi
	callq	terminate
.LBB3_6:                                # %for.inc
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	-288(%r12), %rbx
	movq	%rbx, %rdi
	callq	reunit_su3
	addq	%rax, %r13
	testq	%r15, %rax
	je	.LBB3_8
# BB#7:                                 # %if.then.1
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reunit_report_problem_matrix
.LBB3_8:                                # %if.end.1
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$101, %rax
	jl	.LBB3_10
# BB#9:                                 # %if.then5.1
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$.Lstr12, %rdi
	callq	puts
	movq	$1, %rdi
	callq	terminate
.LBB3_10:                               # %for.inc.1
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	-144(%r12), %rbx
	movq	%rbx, %rdi
	callq	reunit_su3
	addq	%rax, %r13
	testq	%r15, %rax
	je	.LBB3_12
# BB#11:                                # %if.then.2
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	reunit_report_problem_matrix
.LBB3_12:                               # %if.end.2
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$101, %rax
	jl	.LBB3_14
# BB#13:                                # %if.then5.2
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$.Lstr12, %rdi
	callq	puts
	movq	$1, %rdi
	callq	terminate
.LBB3_14:                               # %for.inc.2
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	%r12, %rdi
	callq	reunit_su3
	addq	%rax, %r13
	testq	%r15, %rax
	je	.LBB3_16
# BB#15:                                # %if.then.3
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$3, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	reunit_report_problem_matrix
.LBB3_16:                               # %if.end.3
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$101, %rax
	jl	.LBB3_18
# BB#17:                                # %if.then5.3
                                        #   in Loop: Header=BB3_2 Depth=1
	movabsq	$.Lstr12, %rdi
	callq	puts
	movq	$1, %rdi
	callq	terminate
.LBB3_18:                               # %for.inc.3
                                        #   in Loop: Header=BB3_2 Depth=1
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB3_2
# BB#19:                                # %for.end10
	movq	max_deviation(%rip), %rbx
	movabsq	$4547007122018943789, %r14 # imm = 0x3F1A36E2EB1C432D
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %rax
	movq	%r15, %r12
	je	.LBB3_21
# BB#20:                                # %for.end10
	movq	%rax, %r12
.LBB3_21:                               # %for.end10
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB3_23
# BB#22:                                # %for.end10
	movabsq	$1, %r15
.LBB3_23:                               # %for.end10
	orq	%r15, %r12
	jne	.LBB3_25
# BB#24:                                # %if.then12
	xorq	%rax, %rax
	callq	mynode
	movq	max_deviation(%rip), %rdx
	movq	%rax, %rcx
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	$100, %r13
	jl	.LBB3_25
# BB#26:                                # %if.then17
	movabsq	$.Lstr12, %rdi
	callq	puts
	movq	$1, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	terminate  # TAILCALL
.LBB3_25:                               # %if.end20
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp62:
	.size	reunitarize, .Ltmp62-reunitarize
	.cfi_endproc

	.type	max_deviation,@object   # @max_deviation
	.comm	max_deviation,8,8
	.type	av_deviation,@object    # @av_deviation
	.comm	av_deviation,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Unitarity problem on node %d, site %d, dir %d tolerance=%e\n"
	.size	.L.str, 60

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%f "
	.size	.L.str2, 4

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%08x "
	.size	.L.str5, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"reunitarize: Node %d unitarity problem, maximum deviation=%e\n"
	.size	.L.str8, 62

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"SU3 matrix:"
	.size	.Lstr, 12

	.type	.Lstr9,@object          # @str9
.Lstr9:
	.asciz	"repeat in hex:"
	.size	.Lstr9, 15

	.type	.Lstr10,@object         # @str10
.Lstr10:
	.asciz	"  \n "
	.size	.Lstr10, 5

	.type	.Lstr12,@object         # @str12
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr12:
	.asciz	"Unitarity error count exceeded."
	.size	.Lstr12, 32


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
