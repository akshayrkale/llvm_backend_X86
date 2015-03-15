	.file	"rtl.c.bc"
	.text
	.globl	rtvec_alloc
	.align	16, 0x90
	.type	rtvec_alloc,@function
rtvec_alloc:                            # @rtvec_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp3:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp4:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp5:
	.cfi_def_cfa_offset 32
.Ltmp6:
	.cfi_offset %rbx, -24
.Ltmp7:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	leaq	-1(%r14), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	16(,%rax,8), %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	leaq	8(%rbx), %rdi
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%r14d, (%rbx)
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	rtvec_alloc, .Ltmp8-rtvec_alloc
	.cfi_endproc

	.globl	rtx_alloc
	.align	16, 0x90
	.type	rtx_alloc,@function
rtx_alloc:                              # @rtx_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 16
.Ltmp12:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	rtx_length(%rax), %eax
	leaq	8(,%rax,8), %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	andq	$65535, %rbx            # imm = 0xFFFF
	movl	%ebx, (%rax)
	popq	%rbx
	retq
.Ltmp13:
	.size	rtx_alloc, .Ltmp13-rtx_alloc
	.cfi_endproc

	.globl	copy_rtx
	.align	16, 0x90
	.type	copy_rtx,@function
copy_rtx:                               # @copy_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp20:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp21:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp22:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp23:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp25:
	.cfi_def_cfa_offset 64
.Ltmp26:
	.cfi_offset %rbx, -48
.Ltmp27:
	.cfi_offset %r12, -40
.Ltmp28:
	.cfi_offset %r13, -32
.Ltmp29:
	.cfi_offset %r14, -24
.Ltmp30:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %r15d
	leaq	-36(%r15), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$35, %rcx
	ja	.LBB2_8
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$64535396353, %rdx      # imm = 0xF069C0001
	testq	%rdx, %rax
	je	.LBB2_3
# BB#2:
	movq	%rbx, %r14
	jmp	.LBB2_25
.LBB2_3:                                # %entry
	cmpq	$22, %rcx
	jne	.LBB2_8
# BB#4:                                 # %sw.bb1
	movq	8(%rbx), %rax
	movzbl	(%rax), %ecx
	cmpq	$75, %rcx
	jne	.LBB2_8
# BB#5:                                 # %land.lhs.true
	movq	8(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$68, %rcx
	jne	.LBB2_8
# BB#6:                                 # %land.lhs.true13
	movq	16(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$54, %rax
	jne	.LBB2_8
# BB#7:
	movq	%rbx, %r14
	jmp	.LBB2_25
.LBB2_8:                                # %sw.epilog
	movzbl	rtx_length(%r15), %eax
	leaq	8(,%rax,8), %rdi
	callq	ggc_alloc
	movq	%rax, %r14
	movq	$0, (%r14)
	movl	%r15d, (%r14)
	movq	(%rbx), %rax
	movq	%rax, (%r14)
	addq	$-32, %r15
	cmpq	$3, %r15
	movq	$1610612735, %rcx       # imm = 0x5FFFFFFF
	jb	.LBB2_10
# BB#9:                                 # %sw.epilog
	movq	$-536870913, %rcx       # imm = 0xFFFFFFFFDFFFFFFF
.LBB2_10:                               # %sw.epilog
	andq	%rax, %rcx
	movl	%ecx, (%r14)
	movq	%rbx, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movl	(%r15), %edx
	andq	$16777216, %rdx         # imm = 0x1000000
	andq	$-553648129, %rcx       # imm = 0xFFFFFFFFDEFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, (%r14)
	movl	(%r15), %edx
	movabsq	$3724541951, %rsi       # imm = 0xDDFFFFFF
	andq	%rcx, %rsi
	andq	$65535, %rax            # imm = 0xFFFF
	movzbl	rtx_length(%rax), %ecx
	andq	$33554432, %rdx         # imm = 0x2000000
	orq	%rdx, %rsi
	movl	%esi, (%r14)
	testq	%rcx, %rcx
	je	.LBB2_25
# BB#11:                                # %for.body.lr.ph
	movq	rtx_format(,%rax,8), %r13
	xorq	%r12, %r12
	.align	16, 0x90
.LBB2_12:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_23 Depth 2
	movq	8(%r15,%r12,8), %rax
	movq	%rax, 8(%r14,%r12,8)
	movsbq	(%r13), %rcx
	incq	%r13
	cmpq	$68, %rcx
	jg	.LBB2_15
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB2_12 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$48, %rcx
	je	.LBB2_24
	jmp	.LBB2_14
	.align	16, 0x90
.LBB2_15:                               # %for.body
                                        #   in Loop: Header=BB2_12 Depth=1
	addq	$-69, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$50, %rcx
	ja	.LBB2_14
# BB#16:                                # %for.body
                                        #   in Loop: Header=BB2_12 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$1618549835612160, %rdx # imm = 0x5C0100000C000
	testq	%rdx, %rax
	jne	.LBB2_24
# BB#17:                                # %for.body
                                        #   in Loop: Header=BB2_12 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$131073, %rax           # imm = 0x20001
	je	.LBB2_18
# BB#21:                                # %sw.bb80
                                        #   in Loop: Header=BB2_12 Depth=1
	movq	8(%r15,%r12,8), %rax
	testq	%rax, %rax
	je	.LBB2_24
# BB#22:                                # %if.then86
                                        #   in Loop: Header=BB2_12 Depth=1
	movslq	(%rax), %rbx
	leaq	-1(%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	16(,%rax,8), %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	leaq	8(%r15), %rdi
	movq	$3, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, (%r15)
	movq	%r15, 8(%r14,%r12,8)
	movslq	(%r15), %rax
	movq	8(%rsp), %r15           # 8-byte Reload
	testq	%rax, %rax
	movq	$1, %rbx
	jle	.LBB2_24
	.align	16, 0x90
.LBB2_23:                               # %for.body104
                                        #   Parent Loop BB2_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r15,%r12,8), %rax
	movq	(%rax,%rbx,8), %rdi
	callq	copy_rtx
	movq	8(%r14,%r12,8), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	8(%r14,%r12,8), %rax
	movslq	(%rax), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rax, %rdx
	jl	.LBB2_23
	jmp	.LBB2_24
.LBB2_18:                               # %for.body
                                        #   in Loop: Header=BB2_12 Depth=1
	cmpq	$32, %rcx
	jne	.LBB2_14
# BB#19:                                # %sw.bb62
                                        #   in Loop: Header=BB2_12 Depth=1
	movq	8(%r15,%r12,8), %rdi
	testq	%rdi, %rdi
	je	.LBB2_24
# BB#20:                                # %if.then69
                                        #   in Loop: Header=BB2_12 Depth=1
	callq	copy_rtx
	movq	%rax, 8(%r14,%r12,8)
	.align	16, 0x90
.LBB2_24:                               # %for.inc123
                                        #   in Loop: Header=BB2_12 Depth=1
	movzbl	(%r14), %eax
	incq	%r12
	movzbl	rtx_length(%rax), %eax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB2_12
.LBB2_25:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_14:                               # %sw.default121
	movabsq	$.L.str312, %rdi
	movabsq	$.L__FUNCTION__.copy_rtx, %rdx
	movq	$440, %rsi              # imm = 0x1B8
	callq	fancy_abort
.Ltmp31:
	.size	copy_rtx, .Ltmp31-copy_rtx
	.cfi_endproc

	.globl	shallow_copy_rtx
	.align	16, 0x90
	.type	shallow_copy_rtx,@function
shallow_copy_rtx:                       # @shallow_copy_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 32
.Ltmp39:
	.cfi_offset %rbx, -32
.Ltmp40:
	.cfi_offset %r14, -24
.Ltmp41:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	(%rbx), %r15d
	movzbl	rtx_length(%r15), %r14d
	leaq	8(,%r14,8), %rdi
	callq	ggc_alloc
	movq	$0, (%rax)
	movl	%r15d, (%rax)
	movl	(%rbx), %ecx
	andq	$16711680, %rcx         # imm = 0xFF0000
	orq	%r15, %rcx
	movl	%ecx, (%rax)
	movl	(%rbx), %edx
	andq	$268435456, %rdx        # imm = 0x10000000
	orq	%rcx, %rdx
	movl	%edx, (%rax)
	movl	(%rbx), %ecx
	andq	$134217728, %rcx        # imm = 0x8000000
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%rbx), %edx
	andq	$67108864, %rdx         # imm = 0x4000000
	orq	%rcx, %rdx
	movl	%edx, (%rax)
	movl	(%rbx), %ecx
	andq	$1073741824, %rcx       # imm = 0x40000000
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movl	(%rbx), %edx
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	andq	%rdx, %rsi
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	orq	%rsi, %rcx
	testq	%r14, %r14
	movl	%ecx, (%rax)
	je	.LBB3_3
# BB#1:                                 # %for.body.lr.ph
	movq	$1, %rdx
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx,%rdx,8), %rcx
	movq	%rcx, (%rax,%rdx,8)
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%r14, %rsi
	jl	.LBB3_2
.LBB3_3:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp42:
	.size	shallow_copy_rtx, .Ltmp42-shallow_copy_rtx
	.cfi_endproc

	.globl	get_mode_alignment
	.align	16, 0x90
	.type	get_mode_alignment,@function
get_mode_alignment:                     # @get_mode_alignment
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	leaq	-19(%rdi), %rax
	cmpq	$6, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	leaq	-25(%rdi), %rcx
	cmpq	$6, %rcx
	sbbq	%rcx, %rcx
	andq	$1, %rcx
	orq	%rax, %rcx
	jne	.LBB4_1
# BB#2:                                 # %entry
	movabsq	$mode_size, %rax
	jmp	.LBB4_3
.LBB4_1:
	movabsq	$mode_unit_size, %rax
.LBB4_3:                                # %entry
	movzbl	(%rax,%rdi), %edx
	movq	%rdx, %rsi
	negq	%rsi
	movq	%rdx, %rax
	andq	%rsi, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	andq	%rdx, %rsi
	movq	$1, %rcx
	je	.LBB4_5
# BB#4:                                 # %entry
	movq	%rax, %rcx
.LBB4_5:                                # %entry
	cmpq	$128, %rcx
	movq	$128, %rax
	ja	.LBB4_7
# BB#6:                                 # %entry
	movq	%rcx, %rax
.LBB4_7:                                # %entry
	retq
.Ltmp43:
	.size	get_mode_alignment, .Ltmp43-get_mode_alignment
	.cfi_endproc

	.globl	rtx_equal_p
	.align	16, 0x90
	.type	rtx_equal_p,@function
rtx_equal_p:                            # @rtx_equal_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp55:
	.cfi_def_cfa_offset 64
.Ltmp56:
	.cfi_offset %rbx, -48
.Ltmp57:
	.cfi_offset %r12, -40
.Ltmp58:
	.cfi_offset %r13, -32
.Ltmp59:
	.cfi_offset %r14, -24
.Ltmp60:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	cmpq	%r15, %rbx
	je	.LBB5_1
# BB#2:                                 # %if.end
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB5_49
# BB#3:                                 # %if.end
	testq	%r15, %r15
	je	.LBB5_49
# BB#4:                                 # %if.end4
	movl	(%rbx), %edx
	movl	(%r15), %eax
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rax, %rsi
	andq	$65535, %rsi            # imm = 0xFFFF
	cmpq	%rsi, %rcx
	jne	.LBB5_49
# BB#5:                                 # %if.end9
	xorq	%rdx, %rax
	testq	$16711680, %rax         # imm = 0xFF0000
	jne	.LBB5_49
# BB#6:                                 # %if.end17
	leaq	-54(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$14, %rdx
	ja	.LBB5_19
# BB#7:                                 # %if.end17
	jmpq	*.LJTI5_0(,%rdx,8)
.LBB5_19:                               # %sw.epilog
	movzbl	rtx_length(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB5_20
# BB#21:                                # %for.body.lr.ph
	movq	rtx_format(,%rcx,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB5_22:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_44 Depth 2
	movq	%rdx, %r13
	movq	(%rsp), %rax            # 8-byte Reload
	movsbq	-1(%rax,%r13), %rax
	leaq	-1(%r13), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$68, %rax
	jg	.LBB5_24
# BB#23:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$48, %rax
	je	.LBB5_39
	jmp	.LBB5_26
.LBB5_24:                               # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	cmpq	$82, %rax
	jg	.LBB5_27
# BB#25:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$69, %rax
	je	.LBB5_30
	jmp	.LBB5_26
.LBB5_27:                               # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	cmpq	$100, %rax
	jg	.LBB5_32
# BB#28:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$83, %rax
	je	.LBB5_45
# BB#29:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	cmpq	$86, %rax
	jne	.LBB5_26
.LBB5_30:                               # %sw.bb78
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	(%rbx,%r13,8), %rax
	movq	(%r15,%r13,8), %rdx
	movl	(%rax), %esi
	movl	(%rdx), %ecx
	cmpq	%rcx, %rsi
	jne	.LBB5_49
# BB#31:                                # %for.cond91.preheader
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$1, %r12
	testq	%rsi, %rsi
	jle	.LBB5_39
	jmp	.LBB5_44
	.align	16, 0x90
.LBB5_43:                               # %for.cond91.for.body99_crit_edge
                                        #   in Loop: Header=BB5_44 Depth=2
	movq	(%r15,%r13,8), %rdx
	incq	%r12
.LBB5_44:                               # %for.body99
                                        #   Parent Loop BB5_22 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax,%r12,8), %rdi
	movq	(%rdx,%r12,8), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_49
# BB#42:                                # %for.cond91
                                        #   in Loop: Header=BB5_44 Depth=2
	movq	(%rbx,%r13,8), %rax
	movslq	(%rax), %rdx
	movq	$32, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	jl	.LBB5_43
	jmp	.LBB5_39
.LBB5_32:                               # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	cmpq	$104, %rax
	jg	.LBB5_35
# BB#33:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$101, %rax
	jne	.LBB5_26
# BB#34:                                # %sw.bb117
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	(%rbx,%r13,8), %rdi
	movq	(%r15,%r13,8), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_39
	jmp	.LBB5_49
.LBB5_35:                               # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	addq	$-105, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$14, %rax
	ja	.LBB5_26
# BB#36:                                # %for.body
                                        #   in Loop: Header=BB5_22 Depth=1
	jmpq	*.LJTI5_1(,%rax,8)
.LBB5_41:                               # %sw.bb66
                                        #   in Loop: Header=BB5_22 Depth=1
	movl	(%rbx,%r13,8), %eax
	movl	(%r15,%r13,8), %ecx
	jmp	.LBB5_38
.LBB5_45:                               # %sw.bb131
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	(%rbx,%r13,8), %rdi
	movq	(%r15,%r13,8), %rsi
	testq	%rdi, %rdi
	jne	.LBB5_47
# BB#46:                                # %lor.lhs.false137
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rsi, %rsi
	je	.LBB5_39
	jmp	.LBB5_49
.LBB5_47:                               # %lor.lhs.false148
                                        #   in Loop: Header=BB5_22 Depth=1
	testq	%rsi, %rsi
	je	.LBB5_49
# BB#48:                                # %lor.lhs.false154
                                        #   in Loop: Header=BB5_22 Depth=1
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB5_39
	jmp	.LBB5_49
.LBB5_37:                               # %sw.bb54
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	(%rbx,%r13,8), %rax
	movq	(%r15,%r13,8), %rcx
.LBB5_38:                               # %sw.bb54
                                        #   in Loop: Header=BB5_22 Depth=1
	cmpq	%rcx, %rax
	jne	.LBB5_49
.LBB5_39:                               # %for.cond.backedge
                                        #   in Loop: Header=BB5_22 Depth=1
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_22
# BB#40:
	movq	$1, %r14
	jmp	.LBB5_49
.LBB5_1:
	movq	$1, %r14
.LBB5_49:                               # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_20:
	movq	$1, %r14
	jmp	.LBB5_49
.LBB5_8:                                # %sw.bb
	movl	8(%rbx), %ecx
	movl	8(%r15), %edx
	xorq	%r14, %r14
	cmpq	%rdx, %rcx
	jne	.LBB5_49
# BB#9:                                 # %land.rhs
	movl	rtx_equal_function_value_matters(%rip), %ecx
	testq	%rcx, %rcx
	movq	$1, %r14
	je	.LBB5_49
# BB#10:                                # %lor.rhs
	testq	$1073741824, %rax       # imm = 0x40000000
	jne	.LBB5_11
# BB#12:                                # %lor.rhs
	movabsq	$1, %r14
	jmp	.LBB5_49
.LBB5_13:                               # %sw.bb29
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_14
# BB#15:                                # %sw.bb29
	movabsq	$1, %r14
	jmp	.LBB5_49
.LBB5_16:                               # %sw.bb36
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB5_17
# BB#18:                                # %sw.bb36
	movabsq	$1, %r14
	jmp	.LBB5_49
.LBB5_14:
	movabsq	$0, %r14
	jmp	.LBB5_49
.LBB5_17:
	movabsq	$0, %r14
	jmp	.LBB5_49
.LBB5_26:                               # %sw.default168
	movabsq	$.L.str312, %rdi
	movabsq	$.L__FUNCTION__.rtx_equal_p, %rdx
	movq	$611, %rsi              # imm = 0x263
	callq	fancy_abort
.LBB5_11:
	movabsq	$0, %r14
	jmp	.LBB5_49
.Ltmp61:
	.size	rtx_equal_p, .Ltmp61-rtx_equal_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI5_0:
	.quad	.LBB5_49
	.quad	.LBB5_49
	.quad	.LBB5_49
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_8
	.quad	.LBB5_49
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_19
	.quad	.LBB5_13
	.quad	.LBB5_16
.LJTI5_1:
	.quad	.LBB5_41
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_41
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_26
	.quad	.LBB5_45
	.quad	.LBB5_39
	.quad	.LBB5_39
	.quad	.LBB5_26
	.quad	.LBB5_37

	.type	rtx_length,@object      # @rtx_length
	.globl	rtx_length
	.align	16
rtx_length:
	.ascii	"\001\001\001\002\002\003\002\001\003\003\002\002\002\005\004\004\b\004\003\004\002\007\001\003\001\001\003\001\002\002\002\001\007\b\b\003\b\006\002\001\001\007\002\002\001\005\003\002\001\001\002\000\002\001\001\004\001\001\001\000\001\002\001\002\001\002\002\003\001\000\003\005\003\002\002\002\002\001\002\002\002\002\002\002\002\002\001\002\002\002\002\002\002\002\002\002\001\001\001\001\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\002\001\001\001\001\001\001\001\001\001\001\001\001\003\003\001\002\r\n\003\002\001\004\003\002\002\001\002\002\002\002\001\001\001"
	.size	rtx_length, 153

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"UnKnown"
	.size	.L.str, 8

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"nil"
	.size	.L.str1, 4

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"include"
	.size	.L.str2, 8

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"expr_list"
	.size	.L.str3, 10

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"insn_list"
	.size	.L.str4, 10

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"match_operand"
	.size	.L.str5, 14

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"match_scratch"
	.size	.L.str6, 14

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"match_dup"
	.size	.L.str7, 10

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"match_operator"
	.size	.L.str8, 15

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"match_parallel"
	.size	.L.str9, 15

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"match_op_dup"
	.size	.L.str10, 13

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"match_par_dup"
	.size	.L.str11, 14

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"match_insn"
	.size	.L.str12, 11

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"define_insn"
	.size	.L.str13, 12

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"define_peephole"
	.size	.L.str14, 16

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"define_split"
	.size	.L.str15, 13

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"define_insn_and_split"
	.size	.L.str16, 22

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"define_peephole2"
	.size	.L.str17, 17

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"define_combine"
	.size	.L.str18, 15

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"define_expand"
	.size	.L.str19, 14

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"define_delay"
	.size	.L.str20, 13

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"define_function_unit"
	.size	.L.str21, 21

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"define_asm_attributes"
	.size	.L.str22, 22

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"define_cond_exec"
	.size	.L.str23, 17

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"sequence"
	.size	.L.str24, 9

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"address"
	.size	.L.str25, 8

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"define_attr"
	.size	.L.str26, 12

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"attr"
	.size	.L.str27, 5

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"set_attr"
	.size	.L.str28, 9

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"set_attr_alternative"
	.size	.L.str29, 21

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"eq_attr"
	.size	.L.str30, 8

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"attr_flag"
	.size	.L.str31, 10

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"insn"
	.size	.L.str32, 5

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"jump_insn"
	.size	.L.str33, 10

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"call_insn"
	.size	.L.str34, 10

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"barrier"
	.size	.L.str35, 8

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"code_label"
	.size	.L.str36, 11

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"note"
	.size	.L.str37, 5

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"cond_exec"
	.size	.L.str38, 10

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"parallel"
	.size	.L.str39, 9

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"asm_input"
	.size	.L.str40, 10

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"asm_operands"
	.size	.L.str41, 13

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"unspec"
	.size	.L.str42, 7

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"unspec_volatile"
	.size	.L.str43, 16

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"addr_vec"
	.size	.L.str44, 9

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"addr_diff_vec"
	.size	.L.str45, 14

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"prefetch"
	.size	.L.str46, 9

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"set"
	.size	.L.str47, 4

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"use"
	.size	.L.str48, 4

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"clobber"
	.size	.L.str49, 8

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"call"
	.size	.L.str50, 5

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"return"
	.size	.L.str51, 7

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"trap_if"
	.size	.L.str52, 8

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"resx"
	.size	.L.str53, 5

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"const_int"
	.size	.L.str54, 10

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"const_double"
	.size	.L.str55, 13

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"const_vector"
	.size	.L.str56, 13

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"const_string"
	.size	.L.str57, 13

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"const"
	.size	.L.str58, 6

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"pc"
	.size	.L.str59, 3

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"value"
	.size	.L.str60, 6

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"reg"
	.size	.L.str61, 4

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"scratch"
	.size	.L.str62, 8

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"subreg"
	.size	.L.str63, 7

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"strict_low_part"
	.size	.L.str64, 16

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"concat"
	.size	.L.str65, 7

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"mem"
	.size	.L.str66, 4

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"label_ref"
	.size	.L.str67, 10

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"symbol_ref"
	.size	.L.str68, 11

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"cc0"
	.size	.L.str69, 4

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"addressof"
	.size	.L.str70, 10

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"queued"
	.size	.L.str71, 7

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"if_then_else"
	.size	.L.str72, 13

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"cond"
	.size	.L.str73, 5

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"compare"
	.size	.L.str74, 8

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"plus"
	.size	.L.str75, 5

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"minus"
	.size	.L.str76, 6

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"neg"
	.size	.L.str77, 4

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"mult"
	.size	.L.str78, 5

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"div"
	.size	.L.str79, 4

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"mod"
	.size	.L.str80, 4

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"udiv"
	.size	.L.str81, 5

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"umod"
	.size	.L.str82, 5

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"and"
	.size	.L.str83, 4

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"ior"
	.size	.L.str84, 4

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"xor"
	.size	.L.str85, 4

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"not"
	.size	.L.str86, 4

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"ashift"
	.size	.L.str87, 7

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"rotate"
	.size	.L.str88, 7

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"ashiftrt"
	.size	.L.str89, 9

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"lshiftrt"
	.size	.L.str90, 9

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"rotatert"
	.size	.L.str91, 9

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"smin"
	.size	.L.str92, 5

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"smax"
	.size	.L.str93, 5

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"umin"
	.size	.L.str94, 5

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"umax"
	.size	.L.str95, 5

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"pre_dec"
	.size	.L.str96, 8

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"pre_inc"
	.size	.L.str97, 8

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"post_dec"
	.size	.L.str98, 9

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"post_inc"
	.size	.L.str99, 9

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"pre_modify"
	.size	.L.str100, 11

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"post_modify"
	.size	.L.str101, 12

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"ne"
	.size	.L.str102, 3

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"eq"
	.size	.L.str103, 3

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"ge"
	.size	.L.str104, 3

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"gt"
	.size	.L.str105, 3

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"le"
	.size	.L.str106, 3

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"lt"
	.size	.L.str107, 3

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"geu"
	.size	.L.str108, 4

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"gtu"
	.size	.L.str109, 4

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"leu"
	.size	.L.str110, 4

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"ltu"
	.size	.L.str111, 4

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"unordered"
	.size	.L.str112, 10

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"ordered"
	.size	.L.str113, 8

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"uneq"
	.size	.L.str114, 5

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"unge"
	.size	.L.str115, 5

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"ungt"
	.size	.L.str116, 5

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"unle"
	.size	.L.str117, 5

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"unlt"
	.size	.L.str118, 5

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"ltgt"
	.size	.L.str119, 5

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"sign_extend"
	.size	.L.str120, 12

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"zero_extend"
	.size	.L.str121, 12

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"truncate"
	.size	.L.str122, 9

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"float_extend"
	.size	.L.str123, 13

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"float_truncate"
	.size	.L.str124, 15

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"float"
	.size	.L.str125, 6

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"fix"
	.size	.L.str126, 4

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"unsigned_float"
	.size	.L.str127, 15

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"unsigned_fix"
	.size	.L.str128, 13

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"abs"
	.size	.L.str129, 4

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"sqrt"
	.size	.L.str130, 5

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"ffs"
	.size	.L.str131, 4

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"sign_extract"
	.size	.L.str132, 13

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"zero_extract"
	.size	.L.str133, 13

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"high"
	.size	.L.str134, 5

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"lo_sum"
	.size	.L.str135, 7

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"range_info"
	.size	.L.str136, 11

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"range_reg"
	.size	.L.str137, 10

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"range_var"
	.size	.L.str138, 10

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"range_live"
	.size	.L.str139, 11

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"constant_p_rtx"
	.size	.L.str140, 15

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"call_placeholder"
	.size	.L.str141, 17

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"vec_merge"
	.size	.L.str142, 10

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"vec_select"
	.size	.L.str143, 11

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"vec_concat"
	.size	.L.str144, 11

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"vec_duplicate"
	.size	.L.str145, 14

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"ss_plus"
	.size	.L.str146, 8

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"us_plus"
	.size	.L.str147, 8

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"ss_minus"
	.size	.L.str148, 9

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"us_minus"
	.size	.L.str149, 9

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"ss_truncate"
	.size	.L.str150, 12

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"us_truncate"
	.size	.L.str151, 12

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"phi"
	.size	.L.str152, 4

	.type	rtx_name,@object        # @rtx_name
	.section	.rodata,"a",@progbits
	.globl	rtx_name
	.align	16
rtx_name:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.quad	.L.str62
	.quad	.L.str63
	.quad	.L.str64
	.quad	.L.str65
	.quad	.L.str66
	.quad	.L.str67
	.quad	.L.str68
	.quad	.L.str69
	.quad	.L.str70
	.quad	.L.str71
	.quad	.L.str72
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str76
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.quad	.L.str80
	.quad	.L.str81
	.quad	.L.str82
	.quad	.L.str83
	.quad	.L.str84
	.quad	.L.str85
	.quad	.L.str86
	.quad	.L.str87
	.quad	.L.str88
	.quad	.L.str89
	.quad	.L.str90
	.quad	.L.str91
	.quad	.L.str92
	.quad	.L.str93
	.quad	.L.str94
	.quad	.L.str95
	.quad	.L.str96
	.quad	.L.str97
	.quad	.L.str98
	.quad	.L.str99
	.quad	.L.str100
	.quad	.L.str101
	.quad	.L.str102
	.quad	.L.str103
	.quad	.L.str104
	.quad	.L.str105
	.quad	.L.str106
	.quad	.L.str107
	.quad	.L.str108
	.quad	.L.str109
	.quad	.L.str110
	.quad	.L.str111
	.quad	.L.str112
	.quad	.L.str113
	.quad	.L.str114
	.quad	.L.str115
	.quad	.L.str116
	.quad	.L.str117
	.quad	.L.str118
	.quad	.L.str119
	.quad	.L.str120
	.quad	.L.str121
	.quad	.L.str122
	.quad	.L.str123
	.quad	.L.str124
	.quad	.L.str125
	.quad	.L.str126
	.quad	.L.str127
	.quad	.L.str128
	.quad	.L.str129
	.quad	.L.str130
	.quad	.L.str131
	.quad	.L.str132
	.quad	.L.str133
	.quad	.L.str134
	.quad	.L.str135
	.quad	.L.str136
	.quad	.L.str137
	.quad	.L.str138
	.quad	.L.str139
	.quad	.L.str140
	.quad	.L.str141
	.quad	.L.str142
	.quad	.L.str143
	.quad	.L.str144
	.quad	.L.str145
	.quad	.L.str146
	.quad	.L.str147
	.quad	.L.str148
	.quad	.L.str149
	.quad	.L.str150
	.quad	.L.str151
	.quad	.L.str152
	.size	rtx_name, 1224

	.type	.L.str153,@object       # @.str153
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str153:
	.asciz	"VOID"
	.size	.L.str153, 5

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"BI"
	.size	.L.str154, 3

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"QI"
	.size	.L.str155, 3

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"HI"
	.size	.L.str156, 3

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"SI"
	.size	.L.str157, 3

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"DI"
	.size	.L.str158, 3

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"TI"
	.size	.L.str159, 3

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"OI"
	.size	.L.str160, 3

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"PQI"
	.size	.L.str161, 4

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"PHI"
	.size	.L.str162, 4

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"PSI"
	.size	.L.str163, 4

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"PDI"
	.size	.L.str164, 4

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"QF"
	.size	.L.str165, 3

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"HF"
	.size	.L.str166, 3

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"TQF"
	.size	.L.str167, 4

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"SF"
	.size	.L.str168, 3

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"DF"
	.size	.L.str169, 3

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"XF"
	.size	.L.str170, 3

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"TF"
	.size	.L.str171, 3

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"QC"
	.size	.L.str172, 3

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"HC"
	.size	.L.str173, 3

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"SC"
	.size	.L.str174, 3

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"DC"
	.size	.L.str175, 3

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"XC"
	.size	.L.str176, 3

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"TC"
	.size	.L.str177, 3

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"CQI"
	.size	.L.str178, 4

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"CHI"
	.size	.L.str179, 4

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"CSI"
	.size	.L.str180, 4

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"CDI"
	.size	.L.str181, 4

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"CTI"
	.size	.L.str182, 4

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"COI"
	.size	.L.str183, 4

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"V2QI"
	.size	.L.str184, 5

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"V2HI"
	.size	.L.str185, 5

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"V2SI"
	.size	.L.str186, 5

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"V2DI"
	.size	.L.str187, 5

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"V4QI"
	.size	.L.str188, 5

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"V4HI"
	.size	.L.str189, 5

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"V4SI"
	.size	.L.str190, 5

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"V4DI"
	.size	.L.str191, 5

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"V8QI"
	.size	.L.str192, 5

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"V8HI"
	.size	.L.str193, 5

	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"V8SI"
	.size	.L.str194, 5

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"V8DI"
	.size	.L.str195, 5

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"V16QI"
	.size	.L.str196, 6

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"V2SF"
	.size	.L.str197, 5

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"V2DF"
	.size	.L.str198, 5

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"V4SF"
	.size	.L.str199, 5

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"V4DF"
	.size	.L.str200, 5

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"V8SF"
	.size	.L.str201, 5

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"V8DF"
	.size	.L.str202, 5

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"V16SF"
	.size	.L.str203, 6

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"BLK"
	.size	.L.str204, 4

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"CC"
	.size	.L.str205, 3

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"CCGC"
	.size	.L.str206, 5

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"CCGOC"
	.size	.L.str207, 6

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"CCNO"
	.size	.L.str208, 5

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"CCZ"
	.size	.L.str209, 4

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"CCFP"
	.size	.L.str210, 5

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"CCFPU"
	.size	.L.str211, 6

	.type	mode_name,@object       # @mode_name
	.section	.rodata,"a",@progbits
	.globl	mode_name
	.align	16
mode_name:
	.quad	.L.str153
	.quad	.L.str154
	.quad	.L.str155
	.quad	.L.str156
	.quad	.L.str157
	.quad	.L.str158
	.quad	.L.str159
	.quad	.L.str160
	.quad	.L.str161
	.quad	.L.str162
	.quad	.L.str163
	.quad	.L.str164
	.quad	.L.str165
	.quad	.L.str166
	.quad	.L.str167
	.quad	.L.str168
	.quad	.L.str169
	.quad	.L.str170
	.quad	.L.str171
	.quad	.L.str172
	.quad	.L.str173
	.quad	.L.str174
	.quad	.L.str175
	.quad	.L.str176
	.quad	.L.str177
	.quad	.L.str178
	.quad	.L.str179
	.quad	.L.str180
	.quad	.L.str181
	.quad	.L.str182
	.quad	.L.str183
	.quad	.L.str184
	.quad	.L.str185
	.quad	.L.str186
	.quad	.L.str187
	.quad	.L.str188
	.quad	.L.str189
	.quad	.L.str190
	.quad	.L.str191
	.quad	.L.str192
	.quad	.L.str193
	.quad	.L.str194
	.quad	.L.str195
	.quad	.L.str196
	.quad	.L.str197
	.quad	.L.str198
	.quad	.L.str199
	.quad	.L.str200
	.quad	.L.str201
	.quad	.L.str202
	.quad	.L.str203
	.quad	.L.str204
	.quad	.L.str205
	.quad	.L.str206
	.quad	.L.str207
	.quad	.L.str208
	.quad	.L.str209
	.quad	.L.str210
	.quad	.L.str211
	.size	mode_name, 472

	.type	mode_class,@object      # @mode_class
	.globl	mode_class
	.align	16
mode_class:
	.long	0                       # 0x0
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	6                       # 0x6
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	5                       # 0x5
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	7                       # 0x7
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	8                       # 0x8
	.long	0                       # 0x0
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.long	4                       # 0x4
	.size	mode_class, 236

	.type	mode_bitsize,@object    # @mode_bitsize
	.globl	mode_bitsize
	.align	16
mode_bitsize:
	.short	0                       # 0x0
	.short	1                       # 0x1
	.short	8                       # 0x8
	.short	16                      # 0x10
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	256                     # 0x100
	.short	8                       # 0x8
	.short	16                      # 0x10
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	8                       # 0x8
	.short	16                      # 0x10
	.short	24                      # 0x18
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	96                      # 0x60
	.short	128                     # 0x80
	.short	16                      # 0x10
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	192                     # 0xc0
	.short	256                     # 0x100
	.short	16                      # 0x10
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	256                     # 0x100
	.short	512                     # 0x200
	.short	16                      # 0x10
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	32                      # 0x20
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	256                     # 0x100
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	256                     # 0x100
	.short	512                     # 0x200
	.short	128                     # 0x80
	.short	64                      # 0x40
	.short	128                     # 0x80
	.short	128                     # 0x80
	.short	256                     # 0x100
	.short	256                     # 0x100
	.short	512                     # 0x200
	.short	512                     # 0x200
	.short	0                       # 0x0
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	32                      # 0x20
	.size	mode_bitsize, 118

	.type	mode_size,@object       # @mode_size
	.globl	mode_size
	.align	16
mode_size:
	.ascii	"\000\001\001\002\004\b\020 \001\002\004\b\001\002\003\004\b\f\020\002\004\b\020\030 \002\004\b\020 @\002\004\b\020\004\b\020 \b\020 @\020\b\020\020  @@\000\004\004\004\004\004\004\004"
	.size	mode_size, 59

	.type	mode_unit_size,@object  # @mode_unit_size
	.globl	mode_unit_size
	.align	16
mode_unit_size:
	.ascii	"\000\001\001\002\004\b\020 \001\002\004\b\001\002\003\004\b\f\020\001\002\004\b\f\020\001\002\004\b\020 \001\002\004\b\001\002\004\b\001\002\004\b\001\004\b\004\b\004\b\004\000\004\004\004\004\004\004\004"
	.size	mode_unit_size, 59

	.type	mode_wider_mode,@object # @mode_wider_mode
	.globl	mode_wider_mode
	.align	16
mode_wider_mode:
	.asciz	"\000\002\003\004\005\006\007\000\t\n\013\000\r\016\017\020\021\022\000\024\025\026\027\030\000\032\033\034\035\036\000#'+) !\"*$%&\000(.0-1/\000\000\000\000\000\000\000\000\000"
	.size	mode_wider_mode, 59

	.type	mode_mask_array,@object # @mode_mask_array
	.globl	mode_mask_array
	.align	16
mode_mask_array:
	.quad	0                       # 0x0
	.quad	1                       # 0x1
	.quad	255                     # 0xff
	.quad	65535                   # 0xffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	255                     # 0xff
	.quad	65535                   # 0xffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	255                     # 0xff
	.quad	65535                   # 0xffff
	.quad	16777215                # 0xffffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	65535                   # 0xffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	65535                   # 0xffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	65535                   # 0xffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	4294967295              # 0xffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	-1                      # 0xffffffffffffffff
	.quad	0                       # 0x0
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.quad	4294967295              # 0xffffffff
	.size	mode_mask_array, 472

	.type	inner_mode_array,@object # @inner_mode_array
	.globl	inner_mode_array
	.align	16
inner_mode_array:
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	12                      # 0xc
	.long	13                      # 0xd
	.long	15                      # 0xf
	.long	16                      # 0x10
	.long	17                      # 0x11
	.long	18                      # 0x12
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	6                       # 0x6
	.long	7                       # 0x7
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	5                       # 0x5
	.long	2                       # 0x2
	.long	15                      # 0xf
	.long	16                      # 0x10
	.long	15                      # 0xf
	.long	16                      # 0x10
	.long	15                      # 0xf
	.long	16                      # 0x10
	.long	15                      # 0xf
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.size	inner_mode_array, 236

	.type	class_narrowest_mode,@object # @class_narrowest_mode
	.globl	class_narrowest_mode
	.align	16
class_narrowest_mode:
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	12                      # 0xc
	.long	8                       # 0x8
	.long	52                      # 0x34
	.long	25                      # 0x19
	.long	19                      # 0x13
	.long	31                      # 0x1f
	.long	44                      # 0x2c
	.size	class_narrowest_mode, 36

	.type	.L.str212,@object       # @.str212
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str212:
	.asciz	"*"
	.size	.L.str212, 2

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"s"
	.size	.L.str213, 2

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"ee"
	.size	.L.str214, 3

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"ue"
	.size	.L.str215, 3

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"iss"
	.size	.L.str216, 4

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"is"
	.size	.L.str217, 3

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"i"
	.size	.L.str218, 2

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"isE"
	.size	.L.str219, 4

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"iE"
	.size	.L.str220, 3

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"sEsTV"
	.size	.L.str221, 6

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"EsTV"
	.size	.L.str222, 5

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"EsES"
	.size	.L.str223, 5

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"sEsTsESV"
	.size	.L.str224, 9

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"Ess"
	.size	.L.str225, 4

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"sEss"
	.size	.L.str226, 5

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"eE"
	.size	.L.str227, 3

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"siieiiV"
	.size	.L.str228, 8

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"V"
	.size	.L.str229, 2

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"E"
	.size	.L.str230, 2

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"e"
	.size	.L.str231, 2

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"sse"
	.size	.L.str232, 4

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"ss"
	.size	.L.str233, 3

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"sE"
	.size	.L.str234, 3

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"iuueiee"
	.size	.L.str235, 8

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"iuueiee0"
	.size	.L.str236, 9

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"iuueieee"
	.size	.L.str237, 9

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"iuu"
	.size	.L.str238, 4

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"iuu00iss"
	.size	.L.str239, 9

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"iuu0ni"
	.size	.L.str240, 7

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"ssiEEsi"
	.size	.L.str241, 8

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"Ei"
	.size	.L.str242, 3

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"eEee0"
	.size	.L.str243, 6

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"eee"
	.size	.L.str244, 4

	.type	.L.str245,@object       # @.str245
.L.str245:
	.zero	1
	.size	.L.str245, 1

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"w"
	.size	.L.str246, 2

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"0www"
	.size	.L.str247, 5

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"0"
	.size	.L.str248, 2

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"i0"
	.size	.L.str249, 3

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"ei"
	.size	.L.str250, 3

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"e0"
	.size	.L.str251, 3

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"u00"
	.size	.L.str252, 4

	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"eit"
	.size	.L.str253, 4

	.type	.L.str254,@object       # @.str254
.L.str254:
	.asciz	"eeeee"
	.size	.L.str254, 6

	.type	.L.str255,@object       # @.str255
.L.str255:
	.asciz	"Ee"
	.size	.L.str255, 3

	.type	.L.str256,@object       # @.str256
.L.str256:
	.asciz	"uuEiiiiiibbii"
	.size	.L.str256, 14

	.type	.L.str257,@object       # @.str257
.L.str257:
	.asciz	"iiiiiiiitt"
	.size	.L.str257, 11

	.type	.L.str258,@object       # @.str258
.L.str258:
	.asciz	"eti"
	.size	.L.str258, 4

	.type	.L.str259,@object       # @.str259
.L.str259:
	.asciz	"bi"
	.size	.L.str259, 3

	.type	.L.str260,@object       # @.str260
.L.str260:
	.asciz	"uuuu"
	.size	.L.str260, 5

	.type	rtx_format,@object      # @rtx_format
	.section	.rodata,"a",@progbits
	.globl	rtx_format
	.align	16
rtx_format:
	.quad	.L.str212
	.quad	.L.str212
	.quad	.L.str213
	.quad	.L.str214
	.quad	.L.str215
	.quad	.L.str216
	.quad	.L.str217
	.quad	.L.str218
	.quad	.L.str219
	.quad	.L.str219
	.quad	.L.str220
	.quad	.L.str220
	.quad	.L.str217
	.quad	.L.str221
	.quad	.L.str222
	.quad	.L.str223
	.quad	.L.str224
	.quad	.L.str223
	.quad	.L.str225
	.quad	.L.str226
	.quad	.L.str227
	.quad	.L.str228
	.quad	.L.str229
	.quad	.L.str225
	.quad	.L.str230
	.quad	.L.str231
	.quad	.L.str232
	.quad	.L.str213
	.quad	.L.str233
	.quad	.L.str234
	.quad	.L.str233
	.quad	.L.str213
	.quad	.L.str235
	.quad	.L.str236
	.quad	.L.str237
	.quad	.L.str238
	.quad	.L.str239
	.quad	.L.str240
	.quad	.L.str214
	.quad	.L.str230
	.quad	.L.str213
	.quad	.L.str241
	.quad	.L.str242
	.quad	.L.str242
	.quad	.L.str230
	.quad	.L.str243
	.quad	.L.str244
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str245
	.quad	.L.str214
	.quad	.L.str218
	.quad	.L.str246
	.quad	.L.str247
	.quad	.L.str230
	.quad	.L.str213
	.quad	.L.str231
	.quad	.L.str245
	.quad	.L.str248
	.quad	.L.str249
	.quad	.L.str248
	.quad	.L.str250
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str251
	.quad	.L.str252
	.quad	.L.str213
	.quad	.L.str245
	.quad	.L.str253
	.quad	.L.str254
	.quad	.L.str244
	.quad	.L.str255
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str244
	.quad	.L.str244
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str256
	.quad	.L.str257
	.quad	.L.str258
	.quad	.L.str259
	.quad	.L.str231
	.quad	.L.str260
	.quad	.L.str244
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str214
	.quad	.L.str231
	.quad	.L.str231
	.quad	.L.str230
	.size	rtx_format, 1224

	.type	rtx_class,@object       # @rtx_class
	.globl	rtx_class
	.align	16
rtx_class:
	.ascii	"xxxxxmmmmmmmmxxxxxxxxxxxxmxxxxxxiiixxxxxxxxxxxxxxxxxxxooxooooooxxoooooox3x2c21c2222ccc122222ccccaaaaaa<<<<<<<<<<<<<<<<<<111111111111bbooxxxxxxxxxxcc2211x"
	.size	rtx_class, 153

	.type	.L.str261,@object       # @.str261
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str261:
	.asciz	"NOTE_INSN_DELETED"
	.size	.L.str261, 18

	.type	.L.str262,@object       # @.str262
.L.str262:
	.asciz	"NOTE_INSN_BLOCK_BEG"
	.size	.L.str262, 20

	.type	.L.str263,@object       # @.str263
.L.str263:
	.asciz	"NOTE_INSN_BLOCK_END"
	.size	.L.str263, 20

	.type	.L.str264,@object       # @.str264
.L.str264:
	.asciz	"NOTE_INSN_LOOP_BEG"
	.size	.L.str264, 19

	.type	.L.str265,@object       # @.str265
.L.str265:
	.asciz	"NOTE_INSN_LOOP_END"
	.size	.L.str265, 19

	.type	.L.str266,@object       # @.str266
.L.str266:
	.asciz	"NOTE_INSN_LOOP_CONT"
	.size	.L.str266, 20

	.type	.L.str267,@object       # @.str267
.L.str267:
	.asciz	"NOTE_INSN_LOOP_VTOP"
	.size	.L.str267, 20

	.type	.L.str268,@object       # @.str268
.L.str268:
	.asciz	"NOTE_INSN_LOOP_END_TOP_COND"
	.size	.L.str268, 28

	.type	.L.str269,@object       # @.str269
.L.str269:
	.asciz	"NOTE_INSN_FUNCTION_END"
	.size	.L.str269, 23

	.type	.L.str270,@object       # @.str270
.L.str270:
	.asciz	"NOTE_INSN_PROLOGUE_END"
	.size	.L.str270, 23

	.type	.L.str271,@object       # @.str271
.L.str271:
	.asciz	"NOTE_INSN_EPILOGUE_BEG"
	.size	.L.str271, 23

	.type	.L.str272,@object       # @.str272
.L.str272:
	.asciz	"NOTE_INSN_DELETED_LABEL"
	.size	.L.str272, 24

	.type	.L.str273,@object       # @.str273
.L.str273:
	.asciz	"NOTE_INSN_FUNCTION_BEG"
	.size	.L.str273, 23

	.type	.L.str274,@object       # @.str274
.L.str274:
	.asciz	"NOTE_INSN_EH_REGION_BEG"
	.size	.L.str274, 24

	.type	.L.str275,@object       # @.str275
.L.str275:
	.asciz	"NOTE_INSN_EH_REGION_END"
	.size	.L.str275, 24

	.type	.L.str276,@object       # @.str276
.L.str276:
	.asciz	"NOTE_INSN_REPEATED_LINE_NUMBER"
	.size	.L.str276, 31

	.type	.L.str277,@object       # @.str277
.L.str277:
	.asciz	"NOTE_INSN_RANGE_BEG"
	.size	.L.str277, 20

	.type	.L.str278,@object       # @.str278
.L.str278:
	.asciz	"NOTE_INSN_RANGE_END"
	.size	.L.str278, 20

	.type	.L.str279,@object       # @.str279
.L.str279:
	.asciz	"NOTE_INSN_LIVE"
	.size	.L.str279, 15

	.type	.L.str280,@object       # @.str280
.L.str280:
	.asciz	"NOTE_INSN_BASIC_BLOCK"
	.size	.L.str280, 22

	.type	.L.str281,@object       # @.str281
.L.str281:
	.asciz	"NOTE_INSN_EXPECTED_VALUE"
	.size	.L.str281, 25

	.type	note_insn_name,@object  # @note_insn_name
	.section	.rodata,"a",@progbits
	.globl	note_insn_name
	.align	16
note_insn_name:
	.quad	.L.str245
	.quad	.L.str261
	.quad	.L.str262
	.quad	.L.str263
	.quad	.L.str264
	.quad	.L.str265
	.quad	.L.str266
	.quad	.L.str267
	.quad	.L.str268
	.quad	.L.str269
	.quad	.L.str270
	.quad	.L.str271
	.quad	.L.str272
	.quad	.L.str273
	.quad	.L.str274
	.quad	.L.str275
	.quad	.L.str276
	.quad	.L.str277
	.quad	.L.str278
	.quad	.L.str279
	.quad	.L.str280
	.quad	.L.str281
	.size	note_insn_name, 176

	.type	.L.str282,@object       # @.str282
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str282:
	.asciz	"REG_DEAD"
	.size	.L.str282, 9

	.type	.L.str283,@object       # @.str283
.L.str283:
	.asciz	"REG_INC"
	.size	.L.str283, 8

	.type	.L.str284,@object       # @.str284
.L.str284:
	.asciz	"REG_EQUIV"
	.size	.L.str284, 10

	.type	.L.str285,@object       # @.str285
.L.str285:
	.asciz	"REG_EQUAL"
	.size	.L.str285, 10

	.type	.L.str286,@object       # @.str286
.L.str286:
	.asciz	"REG_WAS_0"
	.size	.L.str286, 10

	.type	.L.str287,@object       # @.str287
.L.str287:
	.asciz	"REG_RETVAL"
	.size	.L.str287, 11

	.type	.L.str288,@object       # @.str288
.L.str288:
	.asciz	"REG_LIBCALL"
	.size	.L.str288, 12

	.type	.L.str289,@object       # @.str289
.L.str289:
	.asciz	"REG_NONNEG"
	.size	.L.str289, 11

	.type	.L.str290,@object       # @.str290
.L.str290:
	.asciz	"REG_NO_CONFLICT"
	.size	.L.str290, 16

	.type	.L.str291,@object       # @.str291
.L.str291:
	.asciz	"REG_UNUSED"
	.size	.L.str291, 11

	.type	.L.str292,@object       # @.str292
.L.str292:
	.asciz	"REG_CC_SETTER"
	.size	.L.str292, 14

	.type	.L.str293,@object       # @.str293
.L.str293:
	.asciz	"REG_CC_USER"
	.size	.L.str293, 12

	.type	.L.str294,@object       # @.str294
.L.str294:
	.asciz	"REG_LABEL"
	.size	.L.str294, 10

	.type	.L.str295,@object       # @.str295
.L.str295:
	.asciz	"REG_DEP_ANTI"
	.size	.L.str295, 13

	.type	.L.str296,@object       # @.str296
.L.str296:
	.asciz	"REG_DEP_OUTPUT"
	.size	.L.str296, 15

	.type	.L.str297,@object       # @.str297
.L.str297:
	.asciz	"REG_BR_PROB"
	.size	.L.str297, 12

	.type	.L.str298,@object       # @.str298
.L.str298:
	.asciz	"REG_EXEC_COUNT"
	.size	.L.str298, 15

	.type	.L.str299,@object       # @.str299
.L.str299:
	.asciz	"REG_NOALIAS"
	.size	.L.str299, 12

	.type	.L.str300,@object       # @.str300
.L.str300:
	.asciz	"REG_SAVE_AREA"
	.size	.L.str300, 14

	.type	.L.str301,@object       # @.str301
.L.str301:
	.asciz	"REG_BR_PRED"
	.size	.L.str301, 12

	.type	.L.str302,@object       # @.str302
.L.str302:
	.asciz	"REG_FRAME_RELATED_EXPR"
	.size	.L.str302, 23

	.type	.L.str303,@object       # @.str303
.L.str303:
	.asciz	"REG_EH_CONTEXT"
	.size	.L.str303, 15

	.type	.L.str304,@object       # @.str304
.L.str304:
	.asciz	"REG_EH_REGION"
	.size	.L.str304, 14

	.type	.L.str305,@object       # @.str305
.L.str305:
	.asciz	"REG_SAVE_NOTE"
	.size	.L.str305, 14

	.type	.L.str306,@object       # @.str306
.L.str306:
	.asciz	"REG_MAYBE_DEAD"
	.size	.L.str306, 15

	.type	.L.str307,@object       # @.str307
.L.str307:
	.asciz	"REG_NORETURN"
	.size	.L.str307, 13

	.type	.L.str308,@object       # @.str308
.L.str308:
	.asciz	"REG_NON_LOCAL_GOTO"
	.size	.L.str308, 19

	.type	.L.str309,@object       # @.str309
.L.str309:
	.asciz	"REG_SETJMP"
	.size	.L.str309, 11

	.type	.L.str310,@object       # @.str310
.L.str310:
	.asciz	"REG_ALWAYS_RETURN"
	.size	.L.str310, 18

	.type	.L.str311,@object       # @.str311
.L.str311:
	.asciz	"REG_VTABLE_REF"
	.size	.L.str311, 15

	.type	reg_note_name,@object   # @reg_note_name
	.section	.rodata,"a",@progbits
	.globl	reg_note_name
	.align	16
reg_note_name:
	.quad	.L.str245
	.quad	.L.str282
	.quad	.L.str283
	.quad	.L.str284
	.quad	.L.str285
	.quad	.L.str286
	.quad	.L.str287
	.quad	.L.str288
	.quad	.L.str289
	.quad	.L.str290
	.quad	.L.str291
	.quad	.L.str292
	.quad	.L.str293
	.quad	.L.str294
	.quad	.L.str295
	.quad	.L.str296
	.quad	.L.str297
	.quad	.L.str298
	.quad	.L.str299
	.quad	.L.str300
	.quad	.L.str301
	.quad	.L.str302
	.quad	.L.str303
	.quad	.L.str304
	.quad	.L.str305
	.quad	.L.str306
	.quad	.L.str307
	.quad	.L.str308
	.quad	.L.str309
	.quad	.L.str310
	.quad	.L.str311
	.size	reg_note_name, 248

	.type	.L.str312,@object       # @.str312
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str312:
	.asciz	"rtl.c"
	.size	.L.str312, 6

	.type	.L__FUNCTION__.copy_rtx,@object # @__FUNCTION__.copy_rtx
.L__FUNCTION__.copy_rtx:
	.asciz	"copy_rtx"
	.size	.L__FUNCTION__.copy_rtx, 9

	.type	rtx_equal_function_value_matters,@object # @rtx_equal_function_value_matters
	.comm	rtx_equal_function_value_matters,4,4
	.type	.L__FUNCTION__.rtx_equal_p,@object # @__FUNCTION__.rtx_equal_p
.L__FUNCTION__.rtx_equal_p:
	.asciz	"rtx_equal_p"
	.size	.L__FUNCTION__.rtx_equal_p, 12

	.type	generating_concat_p,@object # @generating_concat_p
	.comm	generating_concat_p,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
