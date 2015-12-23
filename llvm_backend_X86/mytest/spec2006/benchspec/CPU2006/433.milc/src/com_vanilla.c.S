	.file	"com_vanilla.c.bc"
	.text
	.globl	initialize_machine
	.align	16, 0x90
	.type	initialize_machine,@function
initialize_machine:                     # @initialize_machine
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp0:
	.size	initialize_machine, .Ltmp0-initialize_machine
	.cfi_endproc

	.globl	make_nn_gathers
	.align	16, 0x90
	.type	make_nn_gathers,@function
make_nn_gathers:                        # @make_nn_gathers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -24
.Ltmp8:
	.cfi_offset %r14, -16
	movq	$8, %rdi
	movq	$8, %rsi
	callq	calloc
	movq	%rax, neighbor(%rip)
	xorq	%rax, %rax
	movl	%eax, n_gathers(%rip)
	movl	%eax, 4(%rsp)
	movabsq	$neighbor_coords_special, %r14
	leaq	4(%rsp), %rbx
	.align	16, 0x90
.LBB1_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	make_gather
	movl	4(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	movl	%eax, 4(%rsp)
	jl	.LBB1_1
# BB#2:                                 # %for.end
	movq	neighbor(%rip), %rax
	movq	8(%rax), %rcx
	movq	16(%rax), %rdx
	movq	24(%rax), %rsi
	movq	32(%rax), %rdi
	movq	48(%rax), %r8
	movq	56(%rax), %rbx
	movq	%rcx, 56(%rax)
	movq	%rdx, 8(%rax)
	movq	%rsi, 48(%rax)
	movq	%rdi, 16(%rax)
	movq	%rbx, 32(%rax)
	movq	%r8, 24(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp9:
	.size	make_nn_gathers, .Ltmp9-make_nn_gathers
	.cfi_endproc

	.globl	make_gather
	.align	16, 0x90
	.type	make_gather,@function
make_gather:                            # @make_gather
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp16:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp17:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp18:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp19:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp21:
	.cfi_def_cfa_offset 160
.Ltmp22:
	.cfi_offset %rbx, -48
.Ltmp23:
	.cfi_offset %r12, -40
.Ltmp24:
	.cfi_offset %r13, -32
.Ltmp25:
	.cfi_offset %r14, -24
.Ltmp26:
	.cfi_offset %r15, -16
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rdx, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB2_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB2_3
.LBB2_1:
	movabsq	$0, %rax
.LBB2_3:                                # %entry
	movq	%r8, 56(%rsp)           # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movl	n_gathers(%rip), %ecx
	leaq	1(%rax,%rcx), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$33, %rax
	movl	%esi, n_gathers(%rip)
	jge	.LBB2_4
# BB#7:                                 # %if.end7
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	64(%rsp), %r13          # 8-byte Reload
	andq	%r12, %r13
	movq	neighbor(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	realloc
	movslq	sites_on_node(%rip), %r15
	movq	%rax, %r14
	movq	%r14, neighbor(%rip)
	movq	$4, %rsi
	movq	%r15, %rdi
	callq	calloc
	movl	n_gathers(%rip), %ebx
	cmpq	$1, %r13
	jne	.LBB2_11
# BB#8:                                 # %if.then11
	leaq	-2(%rbx), %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rax, %rax
	movq	%rax, (%r14,%rdx,8)
	je	.LBB2_10
# BB#9:                                 # %if.end21
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	$4, %rsi
	movq	%r15, %rdi
	callq	calloc
	decq	%rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rax, %rax
	movq	%rax, (%r14,%rbx,8)
	jne	.LBB2_12
	jmp	.LBB2_10
.LBB2_11:                               # %if.else36
	movq	%r13, 32(%rsp)          # 8-byte Spill
	decq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rax, %rax
	movq	%rax, (%r14,%rdx,8)
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	je	.LBB2_10
.LBB2_12:                               # %if.end51
	testq	%r15, %r15
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	40(%rsp), %r14          # 8-byte Reload
	jle	.LBB2_60
# BB#13:                                # %for.body.lr.ph
	movq	%r14, %rbx
	movq	lattice(%rip), %r14
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%r12, %rdx
	testq	%rdx, %rax
	je	.LBB2_23
# BB#14:                                # %for.body.preheader
	addq	$4, %r14
	xorq	%r12, %r12
	andq	%rdx, %rax
	movq	%rdx, %r13
	movq	%rax, 56(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB2_15:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movswq	-4(%r14), %rdi
	movswq	-2(%r14), %rsi
	movswq	(%r14), %rdx
	movswq	2(%r14), %rcx
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	100(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$1, %r9
	leaq	108(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r15
	callq	*%r15
	movl	108(%rsp), %edi
	movl	104(%rsp), %esi
	leaq	(%rsi,%rdi), %r9
	movl	100(%rsp), %edx
	addq	%rdx, %r9
	movl	96(%rsp), %r8d
	addq	%r8, %r9
	movq	%r9, %r10
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rax, %r10
	movq	$31, %rcx
	shrq	%cl, %r10
	movq	%rsi, %rbx
	orq	%rdi, %rbx
	orq	%rdx, %rbx
	orq	%r8, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	js	.LBB2_20
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB2_15 Depth=1
	movl	nx(%rip), %ebx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	jge	.LBB2_20
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB2_15 Depth=1
	movl	ny(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jge	.LBB2_20
# BB#18:                                # %for.body
                                        #   in Loop: Header=BB2_15 Depth=1
	movl	nz(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jge	.LBB2_20
# BB#19:                                # %for.body
                                        #   in Loop: Header=BB2_15 Depth=1
	movl	nt(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rbx
	jge	.LBB2_20
# BB#44:                                # %if.end98
                                        #   in Loop: Header=BB2_15 Depth=1
	cmpq	$1, 56(%rsp)            # 8-byte Folded Reload
	jne	.LBB2_50
# BB#45:                                # %land.lhs.true118
                                        #   in Loop: Header=BB2_15 Depth=1
	addq	%r9, %r10
	andq	$-2, %r10
	subq	%r10, %r9
	movq	%r9, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB2_46
# BB#49:                                # %land.lhs.true129
                                        #   in Loop: Header=BB2_15 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB2_50
	jmp	.LBB2_48
	.align	16, 0x90
.LBB2_46:                               # %land.lhs.true118
                                        #   in Loop: Header=BB2_15 Depth=1
	testq	%r13, %r9
	jne	.LBB2_50
# BB#47:                                # %land.lhs.true121
                                        #   in Loop: Header=BB2_15 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$2, %rax
	je	.LBB2_48
	.align	16, 0x90
.LBB2_50:                               # %if.end145
                                        #   in Loop: Header=BB2_15 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB2_56
# BB#51:                                # %if.then148
                                        #   in Loop: Header=BB2_15 Depth=1
	leaq	80(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	84(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	88(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$1, %r9
	leaq	92(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r8, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	callq	*%r15
	movswq	-4(%r14), %rax
	movl	92(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#52:                                # %lor.lhs.false153
                                        #   in Loop: Header=BB2_15 Depth=1
	movswq	-2(%r14), %rax
	movl	88(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#53:                                # %lor.lhs.false158
                                        #   in Loop: Header=BB2_15 Depth=1
	movswq	(%r14), %rax
	movl	84(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#54:                                # %lor.lhs.false163
                                        #   in Loop: Header=BB2_15 Depth=1
	movswq	2(%r14), %rax
	movl	80(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
.LBB2_56:                               # %for.inc
                                        #   in Loop: Header=BB2_15 Depth=1
	movq	%r15, %rbx
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %r15
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$1808, %r14             # imm = 0x710
	cmpq	%r15, %rax
	jl	.LBB2_15
	jmp	.LBB2_57
.LBB2_23:                               # %for.body.us.preheader
	movq	%rdx, %r13
	addq	$4, %r14
	xorq	%r12, %r12
	.align	16, 0x90
.LBB2_24:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movswq	-4(%r14), %rdi
	movswq	-2(%r14), %rsi
	movswq	(%r14), %rdx
	movswq	2(%r14), %rcx
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	100(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$1, %r9
	leaq	108(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %r8           # 8-byte Reload
	movq	%rbx, %r15
	callq	*%r15
	movl	108(%rsp), %edi
	movl	104(%rsp), %esi
	leaq	(%rsi,%rdi), %r9
	movl	100(%rsp), %edx
	addq	%rdx, %r9
	movl	96(%rsp), %r8d
	addq	%r8, %r9
	movq	%r9, %rax
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	movq	%rsi, %rbx
	orq	%rdi, %rbx
	orq	%rdx, %rbx
	orq	%r8, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	js	.LBB2_20
# BB#25:                                # %for.body.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movl	nx(%rip), %r10d
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r10, %rbx
	jge	.LBB2_20
# BB#26:                                # %for.body.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movl	ny(%rip), %r10d
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r10, %rbx
	jge	.LBB2_20
# BB#27:                                # %for.body.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movl	nz(%rip), %r10d
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r10, %rbx
	jge	.LBB2_20
# BB#28:                                # %for.body.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movl	nt(%rip), %r10d
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%r10, %rbx
	jge	.LBB2_20
# BB#29:                                # %land.lhs.true.us
                                        #   in Loop: Header=BB2_24 Depth=1
	addq	%r9, %rax
	andq	$-2, %rax
	subq	%rax, %r9
	testq	%r13, %r9
	jne	.LBB2_30
# BB#32:                                # %land.lhs.true103.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$1, %rax
	jne	.LBB2_33
	jmp	.LBB2_48
	.align	16, 0x90
.LBB2_30:                               # %land.lhs.true.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movq	%r9, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB2_38
# BB#31:                                # %land.lhs.true110.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$2, %rax
	je	.LBB2_48
.LBB2_33:                               # %lor.lhs.false115.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB2_38
# BB#34:                                # %land.lhs.true118.us
                                        #   in Loop: Header=BB2_24 Depth=1
	testq	%r13, %r9
	jne	.LBB2_35
# BB#37:                                # %land.lhs.true121.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$2, %rax
	jne	.LBB2_38
	jmp	.LBB2_48
.LBB2_35:                               # %land.lhs.true118.us
                                        #   in Loop: Header=BB2_24 Depth=1
	andq	%r13, %r9
	cmpq	$1, %r9
	jne	.LBB2_38
# BB#36:                                # %land.lhs.true129.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movzbl	4(%r14), %eax
	cmpq	$1, %rax
	je	.LBB2_48
	.align	16, 0x90
.LBB2_38:                               # %if.end145.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	jne	.LBB2_43
# BB#39:                                # %if.then148.us
                                        #   in Loop: Header=BB2_24 Depth=1
	leaq	80(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	84(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	88(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$1, %r9
	leaq	92(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r8, %rcx
	movq	72(%rsp), %r8           # 8-byte Reload
	callq	*%r15
	movswq	-4(%r14), %rax
	movl	92(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#40:                                # %lor.lhs.false153.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movswq	-2(%r14), %rax
	movl	88(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#41:                                # %lor.lhs.false158.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movswq	(%r14), %rax
	movl	84(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
# BB#42:                                # %lor.lhs.false163.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movswq	2(%r14), %rax
	movl	80(%rsp), %ecx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB2_55
.LBB2_43:                               # %for.inc.us
                                        #   in Loop: Header=BB2_24 Depth=1
	movq	%r15, %rbx
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movslq	sites_on_node(%rip), %r15
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$1808, %r14             # imm = 0x710
	cmpq	%r15, %rax
	jl	.LBB2_24
.LBB2_57:                               # %for.end
	testq	%r15, %r15
	movq	72(%rsp), %r13          # 8-byte Reload
	movq	%rbx, %r14
	jle	.LBB2_60
# BB#58:                                # %for.body184.lr.ph
	movq	lattice(%rip), %rbx
	movq	$32, %rcx
	movq	48(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$1, %r12
	.align	16, 0x90
.LBB2_59:                               # %for.body184
                                        # =>This Inner Loop Header: Depth=1
	movswq	(%rbx), %rdi
	movswq	2(%rbx), %rsi
	movswq	4(%rbx), %rdx
	movswq	6(%rbx), %rcx
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	100(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	$1, %r9
	leaq	108(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r13, %r8
	callq	*%r14
	movl	96(%rsp), %ecx
	movl	100(%rsp), %edx
	movl	104(%rsp), %esi
	movl	108(%rsp), %edi
	callq	node_index
	movq	neighbor(%rip), %rcx
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	(%rcx,%rdx,8), %rcx
	movl	%eax, -4(%rcx,%r12,4)
	movslq	sites_on_node(%rip), %r15
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r12
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%r15, %rax
	jl	.LBB2_59
.LBB2_60:                               # %for.end201
	cmpq	$1, 32(%rsp)            # 8-byte Folded Reload
	jne	.LBB2_64
# BB#61:                                # %for.end201
	testq	%r15, %r15
	jle	.LBB2_64
# BB#62:                                # %for.body210.lr.ph
	movq	lattice(%rip), %rbx
	movq	48(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$1, %r12
	.align	16, 0x90
.LBB2_63:                               # %for.body210
                                        # =>This Inner Loop Header: Depth=1
	movswq	(%rbx), %rdi
	movswq	2(%rbx), %rsi
	movswq	4(%rbx), %rdx
	movswq	6(%rbx), %rcx
	leaq	96(%rsp), %rax
	movq	%rax, 24(%rsp)
	leaq	100(%rsp), %rax
	movq	%rax, 16(%rsp)
	leaq	104(%rsp), %rax
	movq	%rax, 8(%rsp)
	leaq	108(%rsp), %rax
	movq	%rax, (%rsp)
	movq	%r13, %r8
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	callq	*%r14
	movl	96(%rsp), %ecx
	movl	100(%rsp), %edx
	movl	104(%rsp), %esi
	movl	108(%rsp), %edi
	callq	node_index
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%r15,8), %rcx
	movl	%eax, -4(%rcx,%r12,4)
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rax, %rdx
	jl	.LBB2_63
.LBB2_64:                               # %return
	movq	48(%rsp), %rax          # 8-byte Reload
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_4:                                # %if.then3
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_6
# BB#5:                                 # %if.then5
	movabsq	$.Lstr15, %rax
	movq	%rax, %rdi
	callq	puts
.LBB2_6:                                # %if.end6
	movq	$1, %rdi
	callq	exit
.LBB2_10:                               # %if.then19
	movl	this_node(%rip), %ecx
	movabsq	$.L.str2, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	printf
	movq	$1, %rdi
	callq	terminate
.LBB2_20:                               # %if.then87.loopexit1
	leaq	-2(%r14), %r15
	leaq	2(%r14), %rbx
	movq	%r14, %r12
	addq	$-4, %r12
	movabsq	$.Lstr14, %rax
	jmp	.LBB2_21
.LBB2_48:                               # %if.then134.loopexit2
	leaq	-2(%r14), %r15
	leaq	2(%r14), %rbx
	movq	%r14, %r12
	addq	$-4, %r12
	movabsq	$.Lstr13, %rax
.LBB2_21:                               # %if.then87
	movq	%rax, %rdi
	callq	puts
	movswq	(%r12), %rcx
	movswq	(%r15), %rdx
	movswq	(%r14), %r8
	movswq	(%rbx), %rbx
	movl	108(%rsp), %r9d
	movl	104(%rsp), %esi
	movl	100(%rsp), %eax
	movl	96(%rsp), %edi
	movq	%rdi, 16(%rsp)
	movq	%rax, 8(%rsp)
	movabsq	$.L.str4, %rdi
	jmp	.LBB2_22
.LBB2_55:                               # %if.then168.loopexit3
	leaq	-2(%r14), %r15
	leaq	2(%r14), %rbx
	movq	%r14, %r12
	addq	$-4, %r12
	movabsq	$.Lstr, %rax
	movq	%rax, %rdi
	callq	puts
	movswq	(%r12), %rcx
	movswq	(%r15), %rdx
	movswq	(%r14), %r8
	movswq	(%rbx), %rbx
	movl	92(%rsp), %r9d
	movl	88(%rsp), %esi
	movl	84(%rsp), %eax
	movl	80(%rsp), %edi
	movq	%rdi, 16(%rsp)
	movq	%rax, 8(%rsp)
	movabsq	$.L.str7, %rdi
.LBB2_22:                               # %if.then87
	xorq	%rax, %rax
	movq	%rsi, (%rsp)
	movq	%rcx, %rsi
	movq	%r8, %rcx
	movq	%rbx, %r8
	callq	printf
	movq	$1, %rdi
	callq	terminate
.Ltmp27:
	.size	make_gather, .Ltmp27-make_gather
	.cfi_endproc

	.globl	neighbor_coords_special
	.align	16, 0x90
	.type	neighbor_coords_special,@function
neighbor_coords_special:                # @neighbor_coords_special
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 32
.Ltmp35:
	.cfi_offset %rbx, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	56(%rsp), %r10
	movq	48(%rsp), %r11
	movq	40(%rsp), %r14
	movq	32(%rsp), %r15
	movl	(%r8), %ebx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %r9
	cmpq	$1, %r9
	je	.LBB3_2
# BB#1:                                 # %entry
	movq	$7, %rcx
	subq	%rbx, %rcx
	movq	%rcx, %rbx
.LBB3_2:                                # %entry
	movl	%edi, (%r15)
	movl	%esi, (%r14)
	movl	%edx, (%r11)
	movl	%eax, (%r10)
	andq	%r8, %rbx
	cmpq	$7, %rbx
	ja	.LBB3_17
# BB#3:                                 # %entry
	jmpq	*.LJTI3_0(,%rbx,8)
.LBB3_4:                                # %sw.bb
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	movslq	nx(%rip), %rsi
	sarq	%cl, %rdi
	movq	%rdi, %rax
	jmp	.LBB3_6
.LBB3_8:                                # %sw.bb5
	incq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movslq	ny(%rip), %rdi
	sarq	%cl, %rsi
	movq	%rsi, %rax
	jmp	.LBB3_10
.LBB3_11:                               # %sw.bb12
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movslq	nz(%rip), %rsi
	sarq	%cl, %rdx
	movq	%rdx, %rax
	jmp	.LBB3_13
.LBB3_14:                               # %sw.bb19
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	nt(%rip), %rsi
	jmp	.LBB3_15
.LBB3_16:                               # %sw.bb22
	movslq	nt(%rip), %rsi
	leaq	-1(%rax,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
.LBB3_15:                               # %sw.epilog
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movl	%edx, (%r10)
	jmp	.LBB3_7
.LBB3_12:                               # %sw.bb15
	movslq	nz(%rip), %rsi
	leaq	-1(%rdx,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB3_13:                               # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r11)
	jmp	.LBB3_7
.LBB3_9:                                # %sw.bb8
	movslq	ny(%rip), %rdi
	leaq	-1(%rsi,%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB3_10:                               # %sw.epilog
	cqto
	idivq	%rdi
	movl	%edx, (%r14)
	jmp	.LBB3_7
.LBB3_5:                                # %sw.bb1
	movslq	nx(%rip), %rsi
	leaq	-1(%rdi,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB3_6:                                # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r15)
.LBB3_7:                                # %sw.epilog
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB3_17:                               # %sw.default
	movabsq	$.Lstr17, %rdi
	callq	puts
	movq	$1, %rdi
	callq	exit
.Ltmp38:
	.size	neighbor_coords_special, .Ltmp38-neighbor_coords_special
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_4
	.quad	.LBB3_8
	.quad	.LBB3_11
	.quad	.LBB3_14
	.quad	.LBB3_16
	.quad	.LBB3_12
	.quad	.LBB3_9
	.quad	.LBB3_5

	.text
	.globl	sort_eight_special
	.align	16, 0x90
	.type	sort_eight_special,@function
sort_eight_special:                     # @sort_eight_special
	.cfi_startproc
# BB#0:                                 # %entry
	movq	56(%rdi), %rax
	movq	%rax, -16(%rsp)
	movq	48(%rdi), %rax
	movq	%rax, -24(%rsp)
	movq	40(%rdi), %rax
	movq	%rax, -32(%rsp)
	movq	32(%rdi), %rax
	movq	%rax, -40(%rsp)
	movq	24(%rdi), %rax
	movq	%rax, -48(%rsp)
	movq	16(%rdi), %rax
	movq	%rax, -56(%rsp)
	movq	(%rdi), %rax
	movq	8(%rdi), %rcx
	movq	%rcx, -64(%rsp)
	movq	%rax, -72(%rsp)
	movq	-72(%rsp), %rax
	movq	%rax, (%rdi)
	movq	-64(%rsp), %rax
	movq	%rax, 56(%rdi)
	movq	-56(%rsp), %rax
	movq	%rax, 8(%rdi)
	movq	-48(%rsp), %rax
	movq	%rax, 48(%rdi)
	movq	-40(%rsp), %rax
	movq	%rax, 16(%rdi)
	movq	-32(%rsp), %rax
	movq	%rax, 40(%rdi)
	movq	-24(%rsp), %rax
	movq	-16(%rsp), %rcx
	movq	%rcx, 32(%rdi)
	movq	%rax, 24(%rdi)
	retq
.Ltmp39:
	.size	sort_eight_special, .Ltmp39-sort_eight_special
	.cfi_endproc

	.globl	sort_eight_neighborlists
	.align	16, 0x90
	.type	sort_eight_neighborlists,@function
sort_eight_neighborlists:               # @sort_eight_neighborlists
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	neighbor(%rip), %rax
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	8(%rax,%rdi,8), %r10
	movq	16(%rax,%rdi,8), %rdx
	movq	24(%rax,%rdi,8), %rsi
	movq	32(%rax,%rdi,8), %r8
	movq	48(%rax,%rdi,8), %r9
	movq	56(%rax,%rdi,8), %rcx
	movq	%r10, 56(%rax,%rdi,8)
	movq	%rdx, 8(%rax,%rdi,8)
	movq	%rsi, 48(%rax,%rdi,8)
	movq	%r8, 16(%rax,%rdi,8)
	movq	%rcx, 32(%rax,%rdi,8)
	movq	%r9, 24(%rax,%rdi,8)
	retq
.Ltmp40:
	.size	sort_eight_neighborlists, .Ltmp40-sort_eight_neighborlists
	.cfi_endproc

	.globl	terminate
	.align	16, 0x90
	.type	terminate,@function
terminate:                              # @terminate
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp46:
	.cfi_def_cfa_offset 32
.Ltmp47:
	.cfi_offset %rbx, -24
.Ltmp48:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	time
	movq	%rbx, %rdi
	callq	ctime
	movq	%rax, %rcx
	movabsq	$.L.str10, %rdi
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	callq	printf
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	printf
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	stderr(%rip), %rdi
	callq	fflush
	movq	%r14, %rdi
	callq	exit
.Ltmp49:
	.size	terminate, .Ltmp49-terminate
	.cfi_endproc

	.globl	neighbor_coords
	.align	16, 0x90
	.type	neighbor_coords,@function
neighbor_coords:                        # @neighbor_coords
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp52:
	.cfi_def_cfa_offset 16
.Ltmp53:
	.cfi_offset %rbx, -16
	movq	%rcx, %rax
	movq	32(%rsp), %r10
	movq	24(%rsp), %r11
	movq	16(%rsp), %rbx
	movl	%edi, (%r9)
	movl	%esi, (%rbx)
	movl	%edx, (%r11)
	movl	%eax, (%r10)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$7, %rcx
	ja	.LBB7_15
# BB#1:                                 # %entry
	jmpq	*.LJTI7_0(,%rcx,8)
.LBB7_2:                                # %sw.bb
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	movslq	nx(%rip), %rsi
	sarq	%cl, %rdi
	movq	%rdi, %rax
	jmp	.LBB7_4
.LBB7_5:                                # %sw.bb4
	incq	%rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movslq	ny(%rip), %rdi
	sarq	%cl, %rsi
	movq	%rsi, %rax
	jmp	.LBB7_7
.LBB7_8:                                # %sw.bb11
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movslq	nz(%rip), %rsi
	sarq	%cl, %rdx
	movq	%rdx, %rax
	jmp	.LBB7_10
.LBB7_12:                               # %sw.bb18
	incq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	nt(%rip), %rsi
	jmp	.LBB7_13
.LBB7_14:                               # %sw.bb21
	movslq	nt(%rip), %rsi
	leaq	-1(%rax,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
.LBB7_13:                               # %sw.epilog
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movl	%edx, (%r10)
	popq	%rbx
	retq
.LBB7_9:                                # %sw.bb14
	movslq	nz(%rip), %rsi
	leaq	-1(%rdx,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB7_10:                               # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r11)
	popq	%rbx
	retq
.LBB7_6:                                # %sw.bb7
	movslq	ny(%rip), %rdi
	leaq	-1(%rsi,%rdi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB7_7:                                # %sw.epilog
	cqto
	idivq	%rdi
	movl	%edx, (%rbx)
	popq	%rbx
	retq
.LBB7_3:                                # %sw.bb1
	movslq	nx(%rip), %rsi
	leaq	-1(%rdi,%rsi), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB7_4:                                # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r9)
	popq	%rbx
	retq
.LBB7_15:                               # %sw.default
	movabsq	$.Lstr17, %rdi
	callq	puts
	movq	$1, %rdi
	callq	exit
.Ltmp54:
	.size	neighbor_coords, .Ltmp54-neighbor_coords
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI7_0:
	.quad	.LBB7_2
	.quad	.LBB7_5
	.quad	.LBB7_8
	.quad	.LBB7_12
	.quad	.LBB7_14
	.quad	.LBB7_9
	.quad	.LBB7_6
	.quad	.LBB7_3

	.text
	.globl	start_handlers
	.align	16, 0x90
	.type	start_handlers,@function
start_handlers:                         # @start_handlers
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp55:
	.size	start_handlers, .Ltmp55-start_handlers
	.cfi_endproc

	.globl	start_gather
	.align	16, 0x90
	.type	start_gather,@function
start_gather:                           # @start_gather
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%rax, %rsi
	cmpq	$1, %rsi
	je	.LBB9_9
# BB#1:                                 # %entry
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB9_2
# BB#6:                                 # %for.cond.preheader
	movslq	even_sites_on_node(%rip), %r9
	testq	%r9, %r9
	jle	.LBB9_12
# BB#7:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	.align	16, 0x90
.LBB9_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	lattice(%rip), %rax
	movslq	-4(%rcx,%rsi,4), %rcx
	imulq	$1808, %rcx, %rcx       # imm = 0x710
	addq	%rax, %rcx
	addq	%rdi, %rcx
	movq	%rcx, -8(%r8,%rsi,8)
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r9, %rax
	jl	.LBB9_8
	jmp	.LBB9_12
.LBB9_9:                                # %sw.bb7
	movslq	even_sites_on_node(%rip), %rax
	movslq	sites_on_node(%rip), %r9
	cmpq	%r9, %rax
	jge	.LBB9_12
# BB#10:                                # %for.body12.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	incq	%rax
	.align	16, 0x90
.LBB9_11:                               # %for.body12
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	lattice(%rip), %rsi
	movslq	-4(%rcx,%rax,4), %rcx
	imulq	$1808, %rcx, %rcx       # imm = 0x710
	addq	%rsi, %rcx
	addq	%rdi, %rcx
	movq	%rcx, -8(%r8,%rax,8)
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rax
	cmpq	%r9, %rsi
	jl	.LBB9_11
	jmp	.LBB9_12
.LBB9_2:                                # %entry
	cmpq	$3, %rcx
	jne	.LBB9_12
# BB#3:                                 # %for.cond28.preheader
	movslq	sites_on_node(%rip), %r9
	testq	%r9, %r9
	jle	.LBB9_12
# BB#4:                                 # %for.body30.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	.align	16, 0x90
.LBB9_5:                                # %for.body30
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	lattice(%rip), %rax
	movslq	-4(%rcx,%rsi,4), %rcx
	imulq	$1808, %rcx, %rcx       # imm = 0x710
	addq	%rax, %rcx
	addq	%rdi, %rcx
	movq	%rcx, -8(%r8,%rsi,8)
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rsi
	cmpq	%r9, %rax
	jl	.LBB9_5
.LBB9_12:                               # %sw.epilog
	xorq	%rax, %rax
	retq
.Ltmp56:
	.size	start_gather, .Ltmp56-start_gather
	.cfi_endproc

	.globl	restart_gather
	.align	16, 0x90
	.type	restart_gather,@function
restart_gather:                         # @restart_gather
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp57:
	.size	restart_gather, .Ltmp57-restart_gather
	.cfi_endproc

	.globl	start_gather_from_temp
	.align	16, 0x90
	.type	start_gather_from_temp,@function
start_gather_from_temp:                 # @start_gather_from_temp
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%rcx, %rax
	andq	%r9, %rax
	cmpq	$1, %rax
	je	.LBB11_9
# BB#1:                                 # %entry
	andq	%r9, %rcx
	cmpq	$2, %rcx
	jne	.LBB11_2
# BB#6:                                 # %for.cond.preheader
	movslq	even_sites_on_node(%rip), %r9
	testq	%r9, %r9
	jle	.LBB11_12
# BB#7:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %r10
	.align	16, 0x90
.LBB11_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movl	-4(%rcx,%r10,4), %eax
	imulq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdi, %rax
	movq	%rax, -8(%r8,%r10,8)
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r10
	cmpq	%r9, %rax
	jl	.LBB11_8
	jmp	.LBB11_12
.LBB11_9:                               # %sw.bb5
	movslq	even_sites_on_node(%rip), %r10
	movslq	sites_on_node(%rip), %r9
	cmpq	%r9, %r10
	jge	.LBB11_12
# BB#10:                                # %for.body10.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r10
	.align	16, 0x90
.LBB11_11:                              # %for.body10
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movl	-4(%rcx,%r10,4), %eax
	imulq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdi, %rax
	movq	%rax, -8(%r8,%r10,8)
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r10
	cmpq	%r9, %rax
	jl	.LBB11_11
	jmp	.LBB11_12
.LBB11_2:                               # %entry
	cmpq	$3, %rcx
	jne	.LBB11_12
# BB#3:                                 # %for.cond25.preheader
	movslq	sites_on_node(%rip), %r9
	testq	%r9, %r9
	jle	.LBB11_12
# BB#4:                                 # %for.body27.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %r10
	.align	16, 0x90
.LBB11_5:                               # %for.body27
                                        # =>This Inner Loop Header: Depth=1
	movq	neighbor(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movl	-4(%rcx,%r10,4), %eax
	imulq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	%rdi, %rax
	movq	%rax, -8(%r8,%r10,8)
	movq	$32, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r10
	cmpq	%r9, %rax
	jl	.LBB11_5
.LBB11_12:                              # %sw.epilog
	xorq	%rax, %rax
	retq
.Ltmp58:
	.size	start_gather_from_temp, .Ltmp58-start_gather_from_temp
	.cfi_endproc

	.globl	restart_gather_from_temp
	.align	16, 0x90
	.type	restart_gather_from_temp,@function
restart_gather_from_temp:               # @restart_gather_from_temp
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp59:
	.size	restart_gather_from_temp, .Ltmp59-restart_gather_from_temp
	.cfi_endproc

	.globl	wait_gather
	.align	16, 0x90
	.type	wait_gather,@function
wait_gather:                            # @wait_gather
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp60:
	.size	wait_gather, .Ltmp60-wait_gather
	.cfi_endproc

	.globl	cleanup_gather
	.align	16, 0x90
	.type	cleanup_gather,@function
cleanup_gather:                         # @cleanup_gather
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp61:
	.size	cleanup_gather, .Ltmp61-cleanup_gather
	.cfi_endproc

	.globl	start_general_gather
	.align	16, 0x90
	.type	start_general_gather,@function
start_general_gather:                   # @start_general_gather
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp70:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp71:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp72:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp73:
	.cfi_def_cfa_offset 64
.Ltmp74:
	.cfi_offset %rbx, -48
.Ltmp75:
	.cfi_offset %r12, -40
.Ltmp76:
	.cfi_offset %r13, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movl	g_gather_flag(%rip), %eax
	movq	%r8, (%rsp)             # 8-byte Spill
	movq	%rdx, %r12
	testq	%rax, %rax
	jne	.LBB15_19
# BB#1:                                 # %if.end
	movl	even_sites_on_node(%rip), %eax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	cmpq	$1, %rdx
	movq	%rax, %r14
	je	.LBB15_3
# BB#2:                                 # %if.end
	xorq	%r14, %r14
.LBB15_3:                               # %if.end
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB15_5
# BB#4:                                 # %if.end
	movl	sites_on_node(%rip), %eax
.LBB15_5:                               # %if.end
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB15_18
# BB#6:                                 # %for.body.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	imulq	$1808, %r14, %r15       # imm = 0x710
	addq	%rax, %r15
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, %r13
	incq	%r14
	.align	16, 0x90
.LBB15_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%r12), %eax
	movswq	(%r15), %rdi
	testq	%rax, %rax
	je	.LBB15_9
# BB#8:                                 # %if.then11
                                        #   in Loop: Header=BB15_7 Depth=1
	movslq	nx(%rip), %rsi
	addq	%rax, %rdi
	addq	%rsi, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, %rax
	cqto
	idivq	%rsi
	movq	%rdx, %rdi
.LBB15_9:                               # %if.end16
                                        #   in Loop: Header=BB15_7 Depth=1
	movl	4(%r12), %eax
	movswq	2(%r15), %rsi
	testq	%rax, %rax
	je	.LBB15_11
# BB#10:                                # %if.then20
                                        #   in Loop: Header=BB15_7 Depth=1
	movslq	ny(%rip), %rbx
	addq	%rax, %rsi
	addq	%rbx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rax
	cqto
	idivq	%rbx
	movq	%rdx, %rsi
.LBB15_11:                              # %if.end29
                                        #   in Loop: Header=BB15_7 Depth=1
	movl	8(%r12), %eax
	movswq	4(%r15), %rbx
	testq	%rax, %rax
	je	.LBB15_13
# BB#12:                                # %if.then33
                                        #   in Loop: Header=BB15_7 Depth=1
	movslq	nz(%rip), %r8
	addq	%rax, %rbx
	addq	%r8, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rax
	cqto
	idivq	%r8
	movq	%rdx, %rbx
.LBB15_13:                              # %if.end42
                                        #   in Loop: Header=BB15_7 Depth=1
	movl	12(%r12), %ecx
	movswq	6(%r15), %rax
	testq	%rcx, %rcx
	je	.LBB15_15
# BB#14:                                # %if.then46
                                        #   in Loop: Header=BB15_7 Depth=1
	movslq	nt(%rip), %r8
	addq	%rcx, %rax
	addq	%r8, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%r8
	movq	%rdx, %rax
.LBB15_15:                              # %if.end55
                                        #   in Loop: Header=BB15_7 Depth=1
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	node_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	lattice(%rip), %rdx
	sarq	%cl, %rax
	imulq	$1808, %rax, %rax       # imm = 0x710
	addq	%rdx, %rax
	addq	%r13, %rax
	cmpq	$2, 8(%rsp)             # 8-byte Folded Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	%rax, -8(%rcx,%r14,8)
	movabsq	$even_sites_on_node, %rax
	je	.LBB15_17
# BB#16:                                # %if.end55
                                        #   in Loop: Header=BB15_7 Depth=1
	movabsq	$sites_on_node, %rax
.LBB15_17:                              # %if.end55
                                        #   in Loop: Header=BB15_7 Depth=1
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$1808, %r15             # imm = 0x710
	cmpq	%rax, %rdx
	jl	.LBB15_7
.LBB15_18:                              # %for.end
	movq	$1, %rcx
	xorq	%rax, %rax
	movl	%ecx, g_gather_flag(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB15_19:                              # %if.then
	movq	stderr(%rip), %rcx
	movabsq	$.L.str8, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	movq	$1, %rdi
	callq	exit
.Ltmp79:
	.size	start_general_gather, .Ltmp79-start_general_gather
	.cfi_endproc

	.globl	wait_general_gather
	.align	16, 0x90
	.type	wait_general_gather,@function
wait_general_gather:                    # @wait_general_gather
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, g_gather_flag(%rip)
	retq
.Ltmp80:
	.size	wait_general_gather, .Ltmp80-wait_general_gather
	.cfi_endproc

	.globl	cleanup_general_gather
	.align	16, 0x90
	.type	cleanup_general_gather,@function
cleanup_general_gather:                 # @cleanup_general_gather
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp81:
	.size	cleanup_general_gather, .Ltmp81-cleanup_general_gather
	.cfi_endproc

	.globl	field_pointer_at_coordinates
	.align	16, 0x90
	.type	field_pointer_at_coordinates,@function
field_pointer_at_coordinates:           # @field_pointer_at_coordinates
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp84:
	.cfi_def_cfa_offset 16
.Ltmp85:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rdx
	movq	%r9, %rcx
	callq	node_index
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	lattice(%rip), %rcx
	imulq	$1808, %rax, %rax       # imm = 0x710
	addq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp86:
	.size	field_pointer_at_coordinates, .Ltmp86-field_pointer_at_coordinates
	.cfi_endproc

	.globl	field_pointer_at_direction
	.align	16, 0x90
	.type	field_pointer_at_direction,@function
field_pointer_at_direction:             # @field_pointer_at_direction
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rcx, %rax
	movq	lattice(%rip), %rcx
	subq	%rcx, %rdx
	movq	$4, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	neighbor(%rip), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsi,%rax,8), %rax
	movabsq	$-7999030616033345391, %rcx # imm = 0x90FDBC090FDBC091
	imulq	%rdx, %rcx
	movslq	(%rax,%rcx,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rdi, %rax
	retq
.Ltmp87:
	.size	field_pointer_at_direction, .Ltmp87-field_pointer_at_direction
	.cfi_endproc

	.globl	cleanup_field_pointer
	.align	16, 0x90
	.type	cleanup_field_pointer,@function
cleanup_field_pointer:                  # @cleanup_field_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp88:
	.size	cleanup_field_pointer, .Ltmp88-cleanup_field_pointer
	.cfi_endproc

	.globl	send_field
	.align	16, 0x90
	.type	send_field,@function
send_field:                             # @send_field
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.Lstr21, %rdi
	jmp	puts  # TAILCALL
.Ltmp89:
	.size	send_field, .Ltmp89-send_field
	.cfi_endproc

	.globl	get_field
	.align	16, 0x90
	.type	get_field,@function
get_field:                              # @get_field
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.Lstr21, %rdi
	jmp	puts  # TAILCALL
.Ltmp90:
	.size	get_field, .Ltmp90-get_field
	.cfi_endproc

	.globl	machine_type
	.align	16, 0x90
	.type	machine_type,@function
machine_type:                           # @machine_type
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$name, %rax
	retq
.Ltmp91:
	.size	machine_type, .Ltmp91-machine_type
	.cfi_endproc

	.globl	mynode
	.align	16, 0x90
	.type	mynode,@function
mynode:                                 # @mynode
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp92:
	.size	mynode, .Ltmp92-mynode
	.cfi_endproc

	.globl	numnodes
	.align	16, 0x90
	.type	numnodes,@function
numnodes:                               # @numnodes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp93:
	.size	numnodes, .Ltmp93-numnodes
	.cfi_endproc

	.globl	g_sync
	.align	16, 0x90
	.type	g_sync,@function
g_sync:                                 # @g_sync
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp94:
	.size	g_sync, .Ltmp94-g_sync
	.cfi_endproc

	.globl	g_floatsum
	.align	16, 0x90
	.type	g_floatsum,@function
g_floatsum:                             # @g_floatsum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp95:
	.size	g_floatsum, .Ltmp95-g_floatsum
	.cfi_endproc

	.globl	g_doublesum
	.align	16, 0x90
	.type	g_doublesum,@function
g_doublesum:                            # @g_doublesum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp96:
	.size	g_doublesum, .Ltmp96-g_doublesum
	.cfi_endproc

	.globl	g_vecdoublesum
	.align	16, 0x90
	.type	g_vecdoublesum,@function
g_vecdoublesum:                         # @g_vecdoublesum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp97:
	.size	g_vecdoublesum, .Ltmp97-g_vecdoublesum
	.cfi_endproc

	.globl	g_complexsum
	.align	16, 0x90
	.type	g_complexsum,@function
g_complexsum:                           # @g_complexsum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp98:
	.size	g_complexsum, .Ltmp98-g_complexsum
	.cfi_endproc

	.globl	g_dcomplexsum
	.align	16, 0x90
	.type	g_dcomplexsum,@function
g_dcomplexsum:                          # @g_dcomplexsum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp99:
	.size	g_dcomplexsum, .Ltmp99-g_dcomplexsum
	.cfi_endproc

	.globl	g_veccomplexsum
	.align	16, 0x90
	.type	g_veccomplexsum,@function
g_veccomplexsum:                        # @g_veccomplexsum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp100:
	.size	g_veccomplexsum, .Ltmp100-g_veccomplexsum
	.cfi_endproc

	.globl	g_wvectorsum
	.align	16, 0x90
	.type	g_wvectorsum,@function
g_wvectorsum:                           # @g_wvectorsum
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp101:
	.size	g_wvectorsum, .Ltmp101-g_wvectorsum
	.cfi_endproc

	.globl	g_xor32
	.align	16, 0x90
	.type	g_xor32,@function
g_xor32:                                # @g_xor32
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp102:
	.size	g_xor32, .Ltmp102-g_xor32
	.cfi_endproc

	.globl	g_floatmax
	.align	16, 0x90
	.type	g_floatmax,@function
g_floatmax:                             # @g_floatmax
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp103:
	.size	g_floatmax, .Ltmp103-g_floatmax
	.cfi_endproc

	.globl	g_doublemax
	.align	16, 0x90
	.type	g_doublemax,@function
g_doublemax:                            # @g_doublemax
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp104:
	.size	g_doublemax, .Ltmp104-g_doublemax
	.cfi_endproc

	.globl	broadcast_float
	.align	16, 0x90
	.type	broadcast_float,@function
broadcast_float:                        # @broadcast_float
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp105:
	.size	broadcast_float, .Ltmp105-broadcast_float
	.cfi_endproc

	.globl	broadcast_double
	.align	16, 0x90
	.type	broadcast_double,@function
broadcast_double:                       # @broadcast_double
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp106:
	.size	broadcast_double, .Ltmp106-broadcast_double
	.cfi_endproc

	.globl	broadcast_complex
	.align	16, 0x90
	.type	broadcast_complex,@function
broadcast_complex:                      # @broadcast_complex
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp107:
	.size	broadcast_complex, .Ltmp107-broadcast_complex
	.cfi_endproc

	.globl	broadcast_dcomplex
	.align	16, 0x90
	.type	broadcast_dcomplex,@function
broadcast_dcomplex:                     # @broadcast_dcomplex
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp108:
	.size	broadcast_dcomplex, .Ltmp108-broadcast_dcomplex
	.cfi_endproc

	.globl	broadcast_bytes
	.align	16, 0x90
	.type	broadcast_bytes,@function
broadcast_bytes:                        # @broadcast_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp109:
	.size	broadcast_bytes, .Ltmp109-broadcast_bytes
	.cfi_endproc

	.globl	send_integer
	.align	16, 0x90
	.type	send_integer,@function
send_integer:                           # @send_integer
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.Lstr21, %rdi
	jmp	puts  # TAILCALL
.Ltmp110:
	.size	send_integer, .Ltmp110-send_integer
	.cfi_endproc

	.globl	receive_integer
	.align	16, 0x90
	.type	receive_integer,@function
receive_integer:                        # @receive_integer
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$.Lstr21, %rdi
	jmp	puts  # TAILCALL
.Ltmp111:
	.size	receive_integer, .Ltmp111-receive_integer
	.cfi_endproc

	.globl	dclock
	.align	16, 0x90
	.type	dclock,@function
dclock:                                 # @dclock
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp112:
	.size	dclock, .Ltmp112-dclock
	.cfi_endproc

	.globl	time_stamp
	.align	16, 0x90
	.type	time_stamp,@function
time_stamp:                             # @time_stamp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp118:
	.cfi_def_cfa_offset 32
.Ltmp119:
	.cfi_offset %rbx, -24
.Ltmp120:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	time
	movq	%rbx, %rdi
	callq	ctime
	movq	%rax, %rcx
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%rcx, %rdx
	callq	printf
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp121:
	.size	time_stamp, .Ltmp121-time_stamp
	.cfi_endproc

	.globl	normal_exit
	.align	16, 0x90
	.type	normal_exit,@function
normal_exit:                            # @normal_exit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp123:
	.cfi_def_cfa_offset 16
	callq	exit
.Ltmp124:
	.size	normal_exit, .Ltmp124-normal_exit
	.cfi_endproc

	.type	neighbor,@object        # @neighbor
	.comm	neighbor,8,8
	.type	n_gathers,@object       # @n_gathers
	.comm	n_gathers,4,4
	.type	.L.str2,@object         # @.str2
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str2:
	.asciz	"NODE %d: no room for neighbor vector\n"
	.size	.L.str2, 38

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"It mapped %d %d %d %d to %d %d %d %d\n"
	.size	.L.str4, 38

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"It's square mapped %d %d %d %d to %d %d %d %d\n"
	.size	.L.str7, 47

	.type	g_gather_flag,@object   # @g_gather_flag
	.bss
	.globl	g_gather_flag
	.align	4
g_gather_flag:
	.long	0                       # 0x0
	.size	g_gather_flag, 4

	.type	.L.str8,@object         # @.str8
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str8:
	.asciz	"ERROR: node %d, two general_gathers() at once!\n"
	.size	.L.str8, 48

	.type	name,@object            # @name
	.data
	.align	16
name:
	.asciz	"Scalar processor"
	.size	name, 17

	.type	.L.str10,@object        # @.str10
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str10:
	.asciz	"%s: %s\n"
	.size	.L.str10, 8

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"termination"
	.size	.L.str11, 12

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Termination: status = %d\n"
	.size	.L.str12, 26

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"DUMMY! Your gather mapping is not its own inverse"
	.size	.Lstr, 50

	.type	.Lstr13,@object         # @str13
	.align	16
.Lstr13:
	.asciz	"DUMMY! Your gather mapping does not obey claimed parity"
	.size	.Lstr13, 56

	.type	.Lstr14,@object         # @str14
	.align	16
.Lstr14:
	.asciz	"DUMMY! Your gather mapping does not stay in lattice"
	.size	.Lstr14, 52

	.type	.Lstr15,@object         # @str15
	.align	16
.Lstr15:
	.asciz	"Too many gathers! change MAX_GATHERS"
	.size	.Lstr15, 37

	.type	.Lstr17,@object         # @str17
	.align	16
.Lstr17:
	.asciz	"BOTCH: bad direction"
	.size	.Lstr17, 21

	.type	.Lstr21,@object         # @str21
	.align	16
.Lstr21:
	.asciz	"BOTCH: this never happens"
	.size	.Lstr21, 26


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
