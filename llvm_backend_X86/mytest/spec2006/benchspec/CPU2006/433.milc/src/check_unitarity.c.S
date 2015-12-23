	.file	"check_unitarity.c.bc"
	.text
	.globl	check_unitarity
	.align	16, 0x90
	.type	check_unitarity,@function
check_unitarity:                        # @check_unitarity
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
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_1
# BB#2:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %rcx
	addq	$112, %rcx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	xorq	%rbx, %rbx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB0_3:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_4 Depth 2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %r15
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%rsi, %rax
	.align	16, 0x90
.LBB0_4:                                # %for.body3
                                        #   Parent Loop BB0_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r13
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	check_su3
	movq	%rax, %r14
	movq	%r14, %rdi
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB0_6
# BB#5:                                 # %for.body3
                                        #   in Loop: Header=BB0_4 Depth=2
	movabsq	$1, %rbx
.LBB0_6:                                # %for.body3
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r14, %rdi
	movabsq	$4547007122018943789, %rsi # imm = 0x3F1A36E2EB1C432D
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rax
	jg	.LBB0_8
# BB#7:                                 # %for.body3
                                        #   in Loop: Header=BB0_4 Depth=2
	movabsq	$1, %rax
.LBB0_8:                                # %for.body3
                                        #   in Loop: Header=BB0_4 Depth=2
	orq	%rax, %rbx
	jne	.LBB0_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB0_4 Depth=2
	xorq	%rax, %rax
	callq	mynode
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str, %rdi
	movq	%rcx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	callq	printf
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	(%r15), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str2, %rbx
	movq	%rbx, %rdi
	callq	printf
	movq	8(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	16(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	24(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	32(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	40(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	48(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	56(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	64(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	72(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	80(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	88(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	96(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	104(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	112(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	120(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	128(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	136(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movabsq	$.Lstr8, %rdi
	callq	puts
	movq	(%r15), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str5, %rbx
	movq	%rbx, %rdi
	callq	printf
	movq	8(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	16(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	24(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	32(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	40(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	48(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	56(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	64(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	72(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	80(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	88(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movq	96(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	104(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	112(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	120(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	128(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	136(%r15), %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	movq	$10, %rdi
	callq	putchar
	movabsq	$.Lstr9, %rdi
	callq	puts
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rdi
	callq	terminate
.LBB0_10:                               # %if.end
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rax
	movq	32(%rsp), %r12          # 8-byte Reload
	js	.LBB0_12
# BB#11:                                # %if.end
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%r13, %rax
.LBB0_12:                               # %if.end
                                        #   in Loop: Header=BB0_4 Depth=2
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rbx, %rsi
	movq	%rax, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	addq	$144, %r15
	cmpq	$4, %rax
	movq	%rsi, %rax
	jne	.LBB0_4
# BB#13:                                # %for.inc67
                                        #   in Loop: Header=BB0_3 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	incq	%rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	$1808, %rcx             # imm = 0x710
	cmpq	%rdx, %rax
	jl	.LBB0_3
# BB#14:                                # %for.end69
	movq	%rsi, %r15
	movq	24(%rsp), %rdi          # 8-byte Reload
	andq	$1073741823, %rdi       # imm = 0x3FFFFFFF
	shlq	$2, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rdi
	callq	sqrt
	movabsq	$4547007122018943789, %r14 # imm = 0x3F1A36E2EB1C432D
	movq	%r15, %rdi
	movq	%r15, %r13
	movq	%r14, %rsi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r12
	movq	%r15, %rbx
	je	.LBB0_16
# BB#15:                                # %for.end69
	movq	%r12, %rbx
.LBB0_16:                               # %for.end69
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	__ledf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB0_18
# BB#17:                                # %for.end69
	movq	%r12, %r15
.LBB0_18:                               # %for.end69
	orq	%r15, %rbx
	jne	.LBB0_19
# BB#20:                                # %if.then74
	xorq	%rax, %rax
	callq	mynode
	movq	%rax, %rcx
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r13, %rbx
	movq	%rbx, %rdx
	callq	printf
	movq	%rbx, %rax
	jmp	.LBB0_21
.LBB0_1:                                # %for.end69.thread
	movabsq	$9221120237041090560, %rdi # imm = 0x7FF8000000000000
	callq	sqrt
	xorq	%rax, %rax
	jmp	.LBB0_21
.LBB0_19:
	movq	%r13, %rax
.LBB0_21:                               # %if.end77
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	check_unitarity, .Ltmp17-check_unitarity
	.cfi_endproc

	.globl	check_su3
	.align	16, 0x90
	.type	check_su3,@function
check_su3:                              # @check_su3
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
	subq	$96, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 144
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
	movq	%rax, %rdi
	callq	sqrt
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, %r12
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$1, %r15
	movq	%rbx, %r13
	jle	.LBB1_2
# BB#1:                                 # %entry
	movq	%r15, %r13
.LBB1_2:                                # %entry
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB1_4
# BB#3:                                 # %entry
	movq	%r15, %rbx
.LBB1_4:                                # %entry
	movabsq	$9223372036854775807, %rax # imm = 0x7FFFFFFFFFFFFFFF
	orq	%r13, %rbx
	jne	.LBB1_5
# BB#6:                                 # %entry
	movabsq	$-4616189618054758400, %r13 # imm = 0xBFF0000000000000
	movq	%rax, %r15
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB1_7
.LBB1_5:
	andq	%rax, %r12
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movabsq	$-4616189618054758400, %r13 # imm = 0xBFF0000000000000
	movq	%rax, %r15
.LBB1_7:                                # %entry
	movq	48(%r14), %rdi
	movq	56(%r14), %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	64(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	72(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	80(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	88(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	float64_add
	movq	%rax, %r12
	andq	%r15, %r12
	movq	%r15, %r13
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rbx, %rax
	js	.LBB1_9
# BB#8:                                 # %entry
	movq	%rax, %r12
.LBB1_9:                                # %entry
	movq	96(%r14), %rdi
	movq	104(%r14), %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	112(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	120(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	128(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	136(%r14), %rdi
	movq	%rax, %rbx
	movq	%rdi, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rdi
	movabsq	$-4616189618054758400, %rsi # imm = 0xBFF0000000000000
	callq	float64_add
	movq	%rax, %rbx
	andq	%r13, %rbx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_11
# BB#10:                                # %entry
	movq	%r12, %rbx
.LBB1_11:                               # %entry
	movq	%rbx, 88(%rsp)          # 8-byte Spill
	movq	48(%r14), %rsi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	(%r14), %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	8(%r14), %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	callq	float64_mul
	movq	56(%r14), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	16(%r14), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	64(%r14), %rsi
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	24(%r14), %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	72(%r14), %r12
	movq	%rax, %rbx
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%r14), %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	80(%r14), %rsi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	40(%r14), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	88(%r14), %r15
	movq	%rax, %rbx
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	88(%rsp), %rbx          # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_13
# BB#12:                                # %entry
	movq	%rbx, %r15
.LBB1_13:                               # %entry
	movq	%r15, 88(%rsp)          # 8-byte Spill
	movq	96(%r14), %rsi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	(%r14), %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	8(%r14), %rbx
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	callq	float64_mul
	movq	104(%r14), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	16(%r14), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	112(%r14), %rsi
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	24(%r14), %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	120(%r14), %r12
	movq	%rax, %rbx
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	32(%r14), %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	128(%r14), %rsi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	40(%r14), %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	136(%r14), %r15
	movq	%rax, %rbx
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r13
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rax, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r15, %rdi
	movq	88(%rsp), %rbx          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_15
# BB#14:                                # %entry
	movq	%rbx, %r15
.LBB1_15:                               # %entry
	movq	96(%r14), %rsi
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	48(%r14), %rdi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	56(%r14), %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	callq	float64_mul
	movq	104(%r14), %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	float64_mul
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	64(%r14), %rdi
	movq	%rdi, 48(%rsp)          # 8-byte Spill
	movq	112(%r14), %rsi
	movq	%rsi, 88(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	72(%r14), %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	120(%r14), %r13
	movq	%rax, %rbx
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	80(%r14), %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	128(%r14), %rsi
	movq	%rsi, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	88(%r14), %rdi
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	136(%r14), %r14
	movq	%rax, %rbx
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %r12
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rbx
	movq	%rax, %r15
	movq	80(%rsp), %rdi          # 8-byte Reload
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%r15, %rdi
	movq	%rbx, %r15
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	88(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rbx
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	float64_mul
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r12, %rsi
	callq	float64_mul
	movq	%rax, %r14
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	float64_add
	movq	%rax, %rdi
	callq	sqrt
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	__ltdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB1_17
# BB#16:                                # %entry
	movq	%r15, %rbx
.LBB1_17:                               # %entry
	movq	%rbx, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp35:
	.size	check_su3, .Ltmp35-check_su3
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Unitarity problem on node %d, site %d, dir %d, deviation=%f\n"
	.size	.L.str, 61

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%f "
	.size	.L.str2, 4

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%08x "
	.size	.L.str5, 6

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Unitarity problem on node %d, maximum deviation=%f\n"
	.size	.L.str7, 52

	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"SU3 matrix:"
	.size	.Lstr, 12

	.type	.Lstr8,@object          # @str8
.Lstr8:
	.asciz	"repeat in hex:"
	.size	.Lstr8, 15

	.type	.Lstr9,@object          # @str9
.Lstr9:
	.asciz	"  \n "
	.size	.Lstr9, 5


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
