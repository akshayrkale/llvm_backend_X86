	.file	"sbitmap.c.bc"
	.text
	.globl	sbitmap_alloc
	.align	16, 0x90
	.type	sbitmap_alloc,@function
sbitmap_alloc:                          # @sbitmap_alloc
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
	movq	%rdi, %r14
	leaq	63(%r14), %rax
	movabsq	$4294967232, %rbx       # imm = 0xFFFFFFC0
	andq	%rax, %rbx
	movq	$6, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	movq	%rbx, %r15
	shlq	%cl, %r15
	leaq	16(,%rbx,8), %rax
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rax, %rdi
	callq	xmalloc
	movl	%r14d, (%rax)
	movl	%ebx, 4(%rax)
	movl	%r15d, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	sbitmap_alloc, .Ltmp10-sbitmap_alloc
	.cfi_endproc

	.globl	sbitmap_vector_alloc
	.align	16, 0x90
	.type	sbitmap_vector_alloc,@function
sbitmap_vector_alloc:                   # @sbitmap_vector_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 64
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r15
	leaq	63(%rsi), %rax
	movabsq	$4294967232, %r12       # imm = 0xFFFFFFC0
	andq	%rax, %r12
	movq	$6, %rcx
	shrq	%cl, %r12
	movq	$3, %rcx
	movq	%r12, %r13
	shlq	%cl, %r13
	leaq	16(,%r12,8), %rbx
	movq	$3, %rcx
	movq	%r15, %r14
	shlq	%cl, %r14
	movq	%rbx, %rax
	imulq	%r15, %rax
	leaq	(%rax,%r15,8), %rax
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	andq	%rax, %rdi
	callq	xmalloc
	movq	%rbx, %r8
	movq	8(%rsp), %rbx           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r15
	je	.LBB1_3
# BB#1:
	movq	$1, %rdx
	andq	%rcx, %r15
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rsi
	andq	%rcx, %rsi
	leaq	(%rax,%rsi), %rdi
	movq	%rdi, -8(%rax,%rdx,8)
	movl	%ebx, (%rax,%rsi)
	movq	%rsi, %rdi
	orq	$4, %rdi
	movl	%r12d, (%rax,%rdi)
	movl	%r13d, 8(%rax,%rsi)
	addq	%r8, %r14
	movq	%rdx, %rsi
	andq	%rcx, %rsi
	incq	%rdx
	cmpq	%r15, %rsi
	jne	.LBB1_2
.LBB1_3:                                # %for.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	sbitmap_vector_alloc, .Ltmp28-sbitmap_vector_alloc
	.cfi_endproc

	.globl	sbitmap_copy
	.align	16, 0x90
	.type	sbitmap_copy,@function
sbitmap_copy:                           # @sbitmap_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp30:
	.cfi_def_cfa_offset 16
	movl	4(%rdi), %edx
	addq	$16, %rsi
	addq	$16, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memcpy
	popq	%rax
	retq
.Ltmp31:
	.size	sbitmap_copy, .Ltmp31-sbitmap_copy
	.cfi_endproc

	.globl	sbitmap_equal
	.align	16, 0x90
	.type	sbitmap_equal,@function
sbitmap_equal:                          # @sbitmap_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 16
	movl	4(%rdi), %edx
	addq	$16, %rsi
	addq	$16, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB3_1:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp34:
	.size	sbitmap_equal, .Ltmp34-sbitmap_equal
	.cfi_endproc

	.globl	sbitmap_zero
	.align	16, 0x90
	.type	sbitmap_zero,@function
sbitmap_zero:                           # @sbitmap_zero
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp36:
	.cfi_def_cfa_offset 16
	movl	8(%rdi), %edx
	addq	$16, %rdi
	xorq	%rsi, %rsi
	callq	memset
	popq	%rax
	retq
.Ltmp37:
	.size	sbitmap_zero, .Ltmp37-sbitmap_zero
	.cfi_endproc

	.globl	sbitmap_ones
	.align	16, 0x90
	.type	sbitmap_ones,@function
sbitmap_ones:                           # @sbitmap_ones
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 16
.Ltmp41:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	8(%rbx), %edx
	leaq	16(%rbx), %rdi
	movq	$255, %rsi
	callq	memset
	movl	(%rbx), %eax
	andq	$63, %rax
	je	.LBB5_2
# BB#1:                                 # %if.then
	movq	$64, %rcx
	subq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	4(%rbx), %edx
	andq	%rax, %rcx
	movq	$-1, %rsi
	shrq	%cl, %rsi
	decq	%rdx
	andq	%rax, %rdx
	movq	%rsi, 16(%rbx,%rdx,8)
.LBB5_2:                                # %if.end
	popq	%rbx
	retq
.Ltmp42:
	.size	sbitmap_ones, .Ltmp42-sbitmap_ones
	.cfi_endproc

	.globl	sbitmap_vector_zero
	.align	16, 0x90
	.type	sbitmap_vector_zero,@function
sbitmap_vector_zero:                    # @sbitmap_vector_zero
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp50:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp52:
	.cfi_def_cfa_offset 48
.Ltmp53:
	.cfi_offset %rbx, -40
.Ltmp54:
	.cfi_offset %r12, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r12
	je	.LBB6_3
# BB#1:
	movq	$1, %rbx
	andq	%r15, %r12
	.align	16, 0x90
.LBB6_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%rbx,8), %rdi
	movl	8(%rdi), %edx
	addq	$16, %rdi
	xorq	%rsi, %rsi
	callq	memset
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	cmpq	%r12, %rax
	jne	.LBB6_2
.LBB6_3:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp57:
	.size	sbitmap_vector_zero, .Ltmp57-sbitmap_vector_zero
	.cfi_endproc

	.globl	sbitmap_vector_ones
	.align	16, 0x90
	.type	sbitmap_vector_ones,@function
sbitmap_vector_ones:                    # @sbitmap_vector_ones
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp64:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp66:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp67:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp68:
	.cfi_def_cfa_offset 48
.Ltmp69:
	.cfi_offset %rbx, -48
.Ltmp70:
	.cfi_offset %r12, -40
.Ltmp71:
	.cfi_offset %r13, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r12
	je	.LBB7_5
# BB#1:
	movq	$1, %rbx
	andq	%r15, %r12
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r14,%rbx,8), %r13
	movl	8(%r13), %edx
	leaq	16(%r13), %rdi
	movq	$255, %rsi
	callq	memset
	movl	(%r13), %eax
	andq	$63, %rax
	je	.LBB7_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	$64, %rcx
	subq	%rax, %rcx
	movl	4(%r13), %eax
	andq	%r15, %rcx
	movq	$-1, %rdx
	shrq	%cl, %rdx
	decq	%rax
	andq	%r15, %rax
	movq	%rdx, 16(%r13,%rax,8)
.LBB7_4:                                # %sbitmap_ones.exit
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	cmpq	%r12, %rax
	jne	.LBB7_2
.LBB7_5:                                # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	sbitmap_vector_ones, .Ltmp74-sbitmap_vector_ones
	.cfi_endproc

	.globl	sbitmap_union_of_diff
	.align	16, 0x90
	.type	sbitmap_union_of_diff,@function
sbitmap_union_of_diff:                  # @sbitmap_union_of_diff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 32
.Ltmp82:
	.cfi_offset %rbx, -32
.Ltmp83:
	.cfi_offset %r14, -24
.Ltmp84:
	.cfi_offset %r15, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB8_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rcx
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r9, %r9
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %r11
	movq	(%rdx), %r15
	movq	(%rsi), %r14
	movq	(%rdi), %rbx
	notq	%r11
	andq	%r15, %r11
	orq	%r14, %r11
	cmpq	%r11, %rbx
	je	.LBB8_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%r11, (%rdi)
	movq	$1, %rax
.LBB8_4:                                # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	addq	$8, %rcx
	incq	%r9
	movq	%r9, %rbx
	andq	%r10, %rbx
	addq	$8, %rdi
	cmpq	%r8, %rbx
	jb	.LBB8_2
.LBB8_5:                                # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp85:
	.size	sbitmap_union_of_diff, .Ltmp85-sbitmap_union_of_diff
	.cfi_endproc

	.globl	sbitmap_not
	.align	16, 0x90
	.type	sbitmap_not,@function
sbitmap_not:                            # @sbitmap_not
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %r8d
	testq	%r8, %r8
	je	.LBB9_3
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%rcx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rax
	addq	$8, %rsi
	notq	%rax
	movq	%rax, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rax
	andq	%rdx, %rax
	cmpq	%r8, %rax
	jb	.LBB9_2
.LBB9_3:                                # %for.end
	retq
.Ltmp86:
	.size	sbitmap_not, .Ltmp86-sbitmap_not
	.cfi_endproc

	.globl	sbitmap_difference
	.align	16, 0x90
	.type	sbitmap_difference,@function
sbitmap_difference:                     # @sbitmap_difference
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %r8d
	testq	%r8, %r8
	je	.LBB10_3
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%rcx, %rcx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %rax
	movq	(%rsi), %r10
	addq	$8, %rsi
	addq	$8, %rdx
	notq	%rax
	andq	%r10, %rax
	movq	%rax, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rax
	andq	%r9, %rax
	cmpq	%r8, %rax
	jb	.LBB10_2
.LBB10_3:                               # %for.end
	retq
.Ltmp87:
	.size	sbitmap_difference, .Ltmp87-sbitmap_difference
	.cfi_endproc

	.globl	sbitmap_a_and_b
	.align	16, 0x90
	.type	sbitmap_a_and_b,@function
sbitmap_a_and_b:                        # @sbitmap_a_and_b
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 16
.Ltmp91:
	.cfi_offset %rbx, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB11_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r10, %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %r11
	movq	(%rdx), %rcx
	movq	(%rdi), %rbx
	andq	%r11, %rcx
	cmpq	%rcx, %rbx
	je	.LBB11_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB11_2 Depth=1
	movq	%rcx, (%rdi)
	movq	$1, %rax
.LBB11_4:                               # %for.inc
                                        #   in Loop: Header=BB11_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	incq	%r10
	movq	%r10, %rcx
	andq	%r9, %rcx
	addq	$8, %rdi
	cmpq	%r8, %rcx
	jb	.LBB11_2
.LBB11_5:                               # %for.end
	popq	%rbx
	retq
.Ltmp92:
	.size	sbitmap_a_and_b, .Ltmp92-sbitmap_a_and_b
	.cfi_endproc

	.globl	sbitmap_a_xor_b
	.align	16, 0x90
	.type	sbitmap_a_xor_b,@function
sbitmap_a_xor_b:                        # @sbitmap_a_xor_b
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 16
.Ltmp96:
	.cfi_offset %rbx, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB12_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r10, %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %r11
	movq	(%rdx), %rcx
	movq	(%rdi), %rbx
	xorq	%r11, %rcx
	cmpq	%rcx, %rbx
	je	.LBB12_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rcx, (%rdi)
	movq	$1, %rax
.LBB12_4:                               # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	incq	%r10
	movq	%r10, %rcx
	andq	%r9, %rcx
	addq	$8, %rdi
	cmpq	%r8, %rcx
	jb	.LBB12_2
.LBB12_5:                               # %for.end
	popq	%rbx
	retq
.Ltmp97:
	.size	sbitmap_a_xor_b, .Ltmp97-sbitmap_a_xor_b
	.cfi_endproc

	.globl	sbitmap_a_or_b
	.align	16, 0x90
	.type	sbitmap_a_or_b,@function
sbitmap_a_or_b:                         # @sbitmap_a_or_b
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 16
.Ltmp101:
	.cfi_offset %rbx, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB13_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r10, %r10
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB13_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %r11
	movq	(%rdx), %rcx
	movq	(%rdi), %rbx
	orq	%r11, %rcx
	cmpq	%rcx, %rbx
	je	.LBB13_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	%rcx, (%rdi)
	movq	$1, %rax
.LBB13_4:                               # %for.inc
                                        #   in Loop: Header=BB13_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	incq	%r10
	movq	%r10, %rcx
	andq	%r9, %rcx
	addq	$8, %rdi
	cmpq	%r8, %rcx
	jb	.LBB13_2
.LBB13_5:                               # %for.end
	popq	%rbx
	retq
.Ltmp102:
	.size	sbitmap_a_or_b, .Ltmp102-sbitmap_a_or_b
	.cfi_endproc

	.globl	sbitmap_a_subset_b_p
	.align	16, 0x90
	.type	sbitmap_a_subset_b_p,@function
sbitmap_a_subset_b_p:                   # @sbitmap_a_subset_b_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %r8d
	testq	%r8, %r8
	movq	$1, %rax
	je	.LBB14_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rsi
	addq	$16, %rdi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB14_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rdx
	movq	(%rsi), %r10
	orq	%r10, %rdx
	cmpq	%r10, %rdx
	jne	.LBB14_3
# BB#4:                                 # %for.inc
                                        #   in Loop: Header=BB14_2 Depth=1
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r9, %rdx
	addq	$8, %rdi
	addq	$8, %rsi
	cmpq	%r8, %rdx
	jb	.LBB14_2
.LBB14_5:                               # %return
	retq
.LBB14_3:
	xorq	%rax, %rax
	retq
.Ltmp103:
	.size	sbitmap_a_subset_b_p, .Ltmp103-sbitmap_a_subset_b_p
	.cfi_endproc

	.globl	sbitmap_a_or_b_and_c
	.align	16, 0x90
	.type	sbitmap_a_or_b_and_c,@function
sbitmap_a_or_b_and_c:                   # @sbitmap_a_or_b_and_c
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp108:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp109:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp110:
	.cfi_def_cfa_offset 32
.Ltmp111:
	.cfi_offset %rbx, -32
.Ltmp112:
	.cfi_offset %r14, -24
.Ltmp113:
	.cfi_offset %r15, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB15_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rcx
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r9, %r9
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB15_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %r14
	movq	(%rcx), %r11
	movq	(%rsi), %r15
	movq	(%rdi), %rbx
	andq	%r14, %r11
	orq	%r15, %r11
	cmpq	%r11, %rbx
	je	.LBB15_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	%r11, (%rdi)
	movq	$1, %rax
.LBB15_4:                               # %for.inc
                                        #   in Loop: Header=BB15_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	addq	$8, %rcx
	incq	%r9
	movq	%r9, %rbx
	andq	%r10, %rbx
	addq	$8, %rdi
	cmpq	%r8, %rbx
	jb	.LBB15_2
.LBB15_5:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp114:
	.size	sbitmap_a_or_b_and_c, .Ltmp114-sbitmap_a_or_b_and_c
	.cfi_endproc

	.globl	sbitmap_a_and_b_or_c
	.align	16, 0x90
	.type	sbitmap_a_and_b_or_c,@function
sbitmap_a_and_b_or_c:                   # @sbitmap_a_and_b_or_c
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 32
.Ltmp122:
	.cfi_offset %rbx, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movl	4(%rdi), %r8d
	xorq	%rax, %rax
	testq	%r8, %r8
	je	.LBB16_5
# BB#1:                                 # %for.body.lr.ph
	addq	$16, %rcx
	addq	$16, %rdx
	addq	$16, %rsi
	addq	$16, %rdi
	xorq	%r9, %r9
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	.align	16, 0x90
.LBB16_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %r14
	movq	(%rcx), %r11
	movq	(%rsi), %r15
	movq	(%rdi), %rbx
	orq	%r14, %r11
	andq	%r15, %r11
	cmpq	%r11, %rbx
	je	.LBB16_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	%r11, (%rdi)
	movq	$1, %rax
.LBB16_4:                               # %for.inc
                                        #   in Loop: Header=BB16_2 Depth=1
	addq	$8, %rsi
	addq	$8, %rdx
	addq	$8, %rcx
	incq	%r9
	movq	%r9, %rbx
	andq	%r10, %rbx
	addq	$8, %rdi
	cmpq	%r8, %rbx
	jb	.LBB16_2
.LBB16_5:                               # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	sbitmap_a_and_b_or_c, .Ltmp125-sbitmap_a_and_b_or_c
	.cfi_endproc

	.globl	sbitmap_intersection_of_succs
	.align	16, 0x90
	.type	sbitmap_intersection_of_succs,@function
sbitmap_intersection_of_succs:          # @sbitmap_intersection_of_succs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp135:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp136:
	.cfi_def_cfa_offset 48
.Ltmp137:
	.cfi_offset %rbx, -48
.Ltmp138:
	.cfi_offset %r12, -40
.Ltmp139:
	.cfi_offset %r13, -32
.Ltmp140:
	.cfi_offset %r14, -24
.Ltmp141:
	.cfi_offset %r15, -16
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rax,%rdx,8), %rax
	movq	%rdi, %r15
	movq	40(%rax), %rbx
	movl	4(%r15), %r13d
	movq	%rsi, %r14
	testq	%rbx, %rbx
	je	.LBB17_11
# BB#1:
	movabsq	$entry_exit_blocks+112, %r12
	.align	16, 0x90
.LBB17_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	cmpq	%r12, %rax
	jne	.LBB17_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB17_3
.LBB17_11:                              # %if.then7
	movl	8(%r15), %edx
	leaq	16(%r15), %rdi
	movq	$255, %rsi
	callq	memset
	movl	(%r15), %eax
	andq	$63, %rax
	je	.LBB17_14
# BB#12:                                # %if.then.i
	movq	$64, %rcx
	subq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdx
	shrq	%cl, %rdx
	leaq	-1(%r13), %rcx
	andq	%rax, %rcx
	movq	%rdx, 16(%r15,%rcx,8)
	jmp	.LBB17_14
.LBB17_4:                               # %for.cond9.preheader
	movslq	88(%rax), %rax
	movq	(%r14,%rax,8), %rsi
	addq	$16, %r15
	addq	$16, %rsi
	movq	$3, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	%r15, %rdi
	callq	memcpy
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB17_14
# BB#5:                                 # %for.body11.lr.ph
	testq	%r13, %r13
	je	.LBB17_13
# BB#6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB17_7:                               # %for.body11.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_9 Depth 2
	movq	24(%rax), %rdx
	cmpq	%r12, %rdx
	je	.LBB17_10
# BB#8:                                 # %for.body24.lr.ph.us
                                        #   in Loop: Header=BB17_7 Depth=1
	movslq	88(%rdx), %rcx
	movq	(%r14,%rcx,8), %rdx
	addq	$16, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	.align	16, 0x90
.LBB17_9:                               # %for.body24.us
                                        #   Parent Loop BB17_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rbx
	addq	$8, %rdx
	andq	%rbx, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rsi
	andq	%r8, %rsi
	cmpq	%r13, %rsi
	jne	.LBB17_9
.LBB17_10:                              # %for.cond9.backedge.us
                                        #   in Loop: Header=BB17_7 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB17_7
	jmp	.LBB17_14
	.align	16, 0x90
.LBB17_13:                              # %for.cond9.backedge
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB17_13
.LBB17_14:                              # %if.end31
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp142:
	.size	sbitmap_intersection_of_succs, .Ltmp142-sbitmap_intersection_of_succs
	.cfi_endproc

	.globl	sbitmap_intersection_of_preds
	.align	16, 0x90
	.type	sbitmap_intersection_of_preds,@function
sbitmap_intersection_of_preds:          # @sbitmap_intersection_of_preds
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp149:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp150:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp151:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp152:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp153:
	.cfi_def_cfa_offset 48
.Ltmp154:
	.cfi_offset %rbx, -48
.Ltmp155:
	.cfi_offset %r12, -40
.Ltmp156:
	.cfi_offset %r13, -32
.Ltmp157:
	.cfi_offset %r14, -24
.Ltmp158:
	.cfi_offset %r15, -16
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rax,%rdx,8), %rax
	movq	%rdi, %r15
	movq	32(%rax), %rbx
	movl	4(%r15), %r13d
	movq	%rsi, %r14
	testq	%rbx, %rbx
	je	.LBB18_11
# BB#1:
	movabsq	$entry_exit_blocks, %r12
	.align	16, 0x90
.LBB18_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rax
	cmpq	%r12, %rax
	jne	.LBB18_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB18_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB18_3
.LBB18_11:                              # %if.then8
	movl	8(%r15), %edx
	leaq	16(%r15), %rdi
	movq	$255, %rsi
	callq	memset
	movl	(%r15), %eax
	andq	$63, %rax
	je	.LBB18_14
# BB#12:                                # %if.then.i
	movq	$64, %rcx
	subq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdx
	shrq	%cl, %rdx
	leaq	-1(%r13), %rcx
	andq	%rax, %rcx
	movq	%rdx, 16(%r15,%rcx,8)
	jmp	.LBB18_14
.LBB18_4:                               # %for.cond10.preheader
	movslq	88(%rax), %rax
	movq	(%r14,%rax,8), %rsi
	addq	$16, %r15
	addq	$16, %rsi
	movq	$3, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	%r15, %rdi
	callq	memcpy
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB18_14
# BB#5:                                 # %for.body12.lr.ph
	testq	%r13, %r13
	je	.LBB18_13
# BB#6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB18_7:                               # %for.body12.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_9 Depth 2
	movq	16(%rax), %rdx
	cmpq	%r12, %rdx
	je	.LBB18_10
# BB#8:                                 # %for.body25.lr.ph.us
                                        #   in Loop: Header=BB18_7 Depth=1
	movslq	88(%rdx), %rcx
	movq	(%r14,%rcx,8), %rdx
	addq	$16, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	.align	16, 0x90
.LBB18_9:                               # %for.body25.us
                                        #   Parent Loop BB18_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rbx
	addq	$8, %rdx
	andq	%rbx, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rsi
	andq	%r8, %rsi
	cmpq	%r13, %rsi
	jne	.LBB18_9
.LBB18_10:                              # %for.cond10.backedge.us
                                        #   in Loop: Header=BB18_7 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB18_7
	jmp	.LBB18_14
	.align	16, 0x90
.LBB18_13:                              # %for.cond10.backedge
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB18_13
.LBB18_14:                              # %if.end32
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp159:
	.size	sbitmap_intersection_of_preds, .Ltmp159-sbitmap_intersection_of_preds
	.cfi_endproc

	.globl	sbitmap_union_of_succs
	.align	16, 0x90
	.type	sbitmap_union_of_succs,@function
sbitmap_union_of_succs:                 # @sbitmap_union_of_succs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp166:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp167:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp168:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp169:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp170:
	.cfi_def_cfa_offset 48
.Ltmp171:
	.cfi_offset %rbx, -48
.Ltmp172:
	.cfi_offset %r12, -40
.Ltmp173:
	.cfi_offset %r13, -32
.Ltmp174:
	.cfi_offset %r14, -24
.Ltmp175:
	.cfi_offset %r15, -16
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rax,%rdx,8), %rax
	movq	40(%rax), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB19_11
# BB#1:
	movl	4(%r15), %r13d
	movabsq	$entry_exit_blocks+112, %r12
	.align	16, 0x90
.LBB19_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	cmpq	%r12, %rax
	jne	.LBB19_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB19_3
.LBB19_11:                              # %if.then7
	movl	8(%r15), %edx
	addq	$16, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
.LBB19_13:                              # %if.end31
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_4:                               # %for.cond9.preheader
	movslq	88(%rax), %rax
	movq	(%r14,%rax,8), %rsi
	addq	$16, %r15
	addq	$16, %rsi
	movq	$3, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	%r15, %rdi
	callq	memcpy
	movq	8(%rbx), %rax
	testq	%rax, %rax
	je	.LBB19_13
# BB#5:                                 # %for.body11.lr.ph
	testq	%r13, %r13
	je	.LBB19_12
# BB#6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB19_7:                               # %for.body11.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_9 Depth 2
	movq	24(%rax), %rdx
	cmpq	%r12, %rdx
	je	.LBB19_10
# BB#8:                                 # %for.body24.lr.ph.us
                                        #   in Loop: Header=BB19_7 Depth=1
	movslq	88(%rdx), %rcx
	movq	(%r14,%rcx,8), %rdx
	addq	$16, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	.align	16, 0x90
.LBB19_9:                               # %for.body24.us
                                        #   Parent Loop BB19_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rbx
	addq	$8, %rdx
	orq	%rbx, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rsi
	andq	%r8, %rsi
	cmpq	%r13, %rsi
	jne	.LBB19_9
.LBB19_10:                              # %for.cond9.backedge.us
                                        #   in Loop: Header=BB19_7 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB19_7
	jmp	.LBB19_13
	.align	16, 0x90
.LBB19_12:                              # %for.cond9.backedge
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB19_12
	jmp	.LBB19_13
.Ltmp176:
	.size	sbitmap_union_of_succs, .Ltmp176-sbitmap_union_of_succs
	.cfi_endproc

	.globl	sbitmap_union_of_preds
	.align	16, 0x90
	.type	sbitmap_union_of_preds,@function
sbitmap_union_of_preds:                 # @sbitmap_union_of_preds
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
.Ltmp188:
	.cfi_offset %rbx, -48
.Ltmp189:
	.cfi_offset %r12, -40
.Ltmp190:
	.cfi_offset %r13, -32
.Ltmp191:
	.cfi_offset %r14, -24
.Ltmp192:
	.cfi_offset %r15, -16
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	32(%rax,%rdx,8), %rax
	movq	32(%rax), %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rbx, %rbx
	je	.LBB20_11
# BB#1:
	movl	4(%r15), %r13d
	movabsq	$entry_exit_blocks, %r12
	.align	16, 0x90
.LBB20_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rax
	cmpq	%r12, %rax
	jne	.LBB20_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB20_3
.LBB20_11:                              # %if.then8
	movl	8(%r15), %edx
	addq	$16, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
.LBB20_13:                              # %if.end32
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_4:                               # %for.cond10.preheader
	movslq	88(%rax), %rax
	movq	(%r14,%rax,8), %rsi
	addq	$16, %r15
	addq	$16, %rsi
	movq	$3, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	%r15, %rdi
	callq	memcpy
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB20_13
# BB#5:                                 # %for.body12.lr.ph
	testq	%r13, %r13
	je	.LBB20_12
# BB#6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB20_7:                               # %for.body12.us
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_9 Depth 2
	movq	16(%rax), %rdx
	cmpq	%r12, %rdx
	je	.LBB20_10
# BB#8:                                 # %for.body25.lr.ph.us
                                        #   in Loop: Header=BB20_7 Depth=1
	movslq	88(%rdx), %rcx
	movq	(%r14,%rcx,8), %rdx
	addq	$16, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	.align	16, 0x90
.LBB20_9:                               # %for.body25.us
                                        #   Parent Loop BB20_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rbx
	addq	$8, %rdx
	orq	%rbx, (%rdi)
	incq	%rcx
	addq	$8, %rdi
	movq	%rcx, %rsi
	andq	%r8, %rsi
	cmpq	%r13, %rsi
	jne	.LBB20_9
.LBB20_10:                              # %for.cond10.backedge.us
                                        #   in Loop: Header=BB20_7 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB20_7
	jmp	.LBB20_13
	.align	16, 0x90
.LBB20_12:                              # %for.cond10.backedge
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB20_12
	jmp	.LBB20_13
.Ltmp193:
	.size	sbitmap_union_of_preds, .Ltmp193-sbitmap_union_of_preds
	.cfi_endproc

	.globl	sbitmap_first_set_bit
	.align	16, 0x90
	.type	sbitmap_first_set_bit,@function
sbitmap_first_set_bit:                  # @sbitmap_first_set_bit
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %r8d
	testq	%r8, %r8
	je	.LBB21_1
# BB#2:
	xorq	%r9, %r9
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB21_3:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_4 Depth 2
	movq	16(%rdi,%r9,8), %r10
	xorq	%rdx, %rdx
	testq	%r10, %r10
	je	.LBB21_7
	.align	16, 0x90
.LBB21_4:                               # %for.body4
                                        #   Parent Loop BB21_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rsi
	movq	%rdx, %rcx
	shlq	%cl, %rsi
	testq	%r10, %rsi
	jne	.LBB21_5
# BB#6:                                 # %for.inc
                                        #   in Loop: Header=BB21_4 Depth=2
	incq	%rdx
	movq	%rdx, %rcx
	andq	%rax, %rcx
	cmpq	$64, %rcx
	jb	.LBB21_4
.LBB21_7:                               # %for.inc9
                                        #   in Loop: Header=BB21_3 Depth=1
	incq	%r9
	movq	%r9, %rcx
	andq	%rax, %rcx
	cmpq	%r8, %rcx
	jb	.LBB21_3
# BB#8:                                 # %return
	retq
.LBB21_5:                               # %if.then6
	movq	$6, %rcx
	shlq	%cl, %r9
	addq	%r9, %rdx
	movq	%rdx, %rax
	retq
.LBB21_1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.Ltmp194:
	.size	sbitmap_first_set_bit, .Ltmp194-sbitmap_first_set_bit
	.cfi_endproc

	.globl	sbitmap_last_set_bit
	.align	16, 0x90
	.type	sbitmap_last_set_bit,@function
sbitmap_last_set_bit:                   # @sbitmap_last_set_bit
	.cfi_startproc
# BB#0:                                 # %entry
	movl	4(%rdi), %edx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB22_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r8
	leaq	-1(%r8), %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB22_5
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB22_1 Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	16(%rdi,%rsi,8), %rsi
	testq	%rsi, %rsi
	je	.LBB22_1
# BB#3:                                 # %if.then
	movq	$6, %rcx
	shlq	%cl, %r8
	movabsq	$-9223372036854775808, %rdx # imm = 0x8000000000000000
	movq	%r8, %rax
	movq	$1, %rcx
	.align	16, 0x90
.LBB22_4:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	decq	%rax
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	testq	%rsi, %rdx
	movq	%rdi, %rdx
	je	.LBB22_4
.LBB22_5:                               # %return
	retq
.Ltmp195:
	.size	sbitmap_last_set_bit, .Ltmp195-sbitmap_last_set_bit
	.cfi_endproc

	.globl	dump_sbitmap
	.align	16, 0x90
	.type	dump_sbitmap,@function
dump_sbitmap:                           # @dump_sbitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp202:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp204:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp205:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp206:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp207:
	.cfi_def_cfa_offset 112
.Ltmp208:
	.cfi_offset %rbx, -48
.Ltmp209:
	.cfi_offset %r12, -40
.Ltmp210:
	.cfi_offset %r13, -32
.Ltmp211:
	.cfi_offset %r14, -24
.Ltmp212:
	.cfi_offset %r15, -16
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movl	4(%rsi), %ebx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movl	(%rsi), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movabsq	$.L.str, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%rax, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB23_16
# BB#1:                                 # %entry
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB23_16
# BB#2:
	xorq	%r12, %r12
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%r13, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB23_3:                               # %for.cond2.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB23_7 Depth 2
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jae	.LBB23_14
# BB#4:                                 # %for.body7.lr.ph
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	%rdx, %rsi
	movq	24(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rsi
	movq	%rsi, %rax
	andq	%r13, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	cmpq	%rcx, %rax
	ja	.LBB23_6
# BB#5:                                 # %for.body7.lr.ph
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	$-64, %rsi
.LBB23_6:                               # %for.body7.lr.ph
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	negq	%r15
	movq	$1, %r14
	movq	%rdx, %rbx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB23_7:                               # %for.body7
                                        #   Parent Loop BB23_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rsi
	andq	%r13, %rsi
	movq	%rsi, %rax
	movabsq	$-3689348814741910323, %rcx # imm = 0xCCCCCCCCCCCCCCCD
	mulq	%rcx
	movq	$2, %rcx
	shrq	%cl, %rdx
	movq	%rbx, %rax
	andq	%r13, %rax
	je	.LBB23_10
# BB#8:                                 # %for.body7
                                        #   in Loop: Header=BB23_7 Depth=2
	movabsq	$4611686018427387902, %rax # imm = 0x3FFFFFFFFFFFFFFE
	andq	%rax, %rdx
	leaq	(%rdx,%rdx,4), %rax
	subq	%rax, %rsi
	testq	%r13, %rsi
	jne	.LBB23_10
# BB#9:                                 # %if.then
                                        #   in Loop: Header=BB23_7 Depth=2
	movq	$32, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	callq	fputc
.LBB23_10:                              # %if.end
                                        #   in Loop: Header=BB23_7 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	16(%rax,%r12,8), %rax
	leaq	-1(%r14), %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	movabsq	$0, %rdx
	je	.LBB23_12
# BB#11:                                # %if.end
                                        #   in Loop: Header=BB23_7 Depth=2
	movabsq	$1, %rdx
.LBB23_12:                              # %if.end
                                        #   in Loop: Header=BB23_7 Depth=2
	xorq	%rax, %rax
	movq	56(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str2, %rsi
	callq	fprintf
	incq	%rbx
	movq	%r14, %rax
	andq	%r13, %rax
	movq	%r15, %rcx
	andq	%r13, %rcx
	incq	%r14
	cmpq	%rcx, %rax
	jne	.LBB23_7
# BB#13:                                # %for.cond2.for.inc14_crit_edge
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	32(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rdx
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %rcx           # 8-byte Reload
.LBB23_14:                              # %for.inc14
                                        #   in Loop: Header=BB23_3 Depth=1
	incq	%r12
	movq	%r12, %rax
	andq	%r13, %rax
	cmpq	%rbx, %rax
	jae	.LBB23_16
# BB#15:                                # %for.inc14
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jb	.LBB23_3
.LBB23_16:                              # %for.end16
	movq	$10, %rdi
	movq	56(%rsp), %rsi          # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp213:
	.size	dump_sbitmap, .Ltmp213-dump_sbitmap
	.cfi_endproc

	.globl	debug_sbitmap
	.align	16, 0x90
	.type	debug_sbitmap,@function
debug_sbitmap:                          # @debug_sbitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp220:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp221:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp222:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp223:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp224:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp225:
	.cfi_def_cfa_offset 64
.Ltmp226:
	.cfi_offset %rbx, -48
.Ltmp227:
	.cfi_offset %r12, -40
.Ltmp228:
	.cfi_offset %r13, -32
.Ltmp229:
	.cfi_offset %r14, -24
.Ltmp230:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	stderr(%rip), %rdi
	movl	(%r15), %edx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	fprintf
	movl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB24_11
# BB#1:
	movq	$30, %rdi
	movabsq	$4294967232, %r13       # imm = 0xFFFFFFC0
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movabsq	$0, %r14
	jmp	.LBB24_2
.LBB24_3:                               # %if.then
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	%rdi, %rax
	andq	%r12, %rax
	cmpq	$71, %rax
	jb	.LBB24_5
# BB#4:                                 # %if.then3
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	$10, %rdi
	movq	stderr(%rip), %rsi
	callq	fputc
	xorq	%rdi, %rdi
.LBB24_5:                               # %if.end
                                        #   in Loop: Header=BB24_2 Depth=1
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movabsq	$.L.str5, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	$9, %rax
	movq	%r14, %rcx
	jbe	.LBB24_7
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB24_2 Depth=1
	movabsq	$1, %rcx
.LBB24_7:                               # %if.end
                                        #   in Loop: Header=BB24_2 Depth=1
	cmpq	$99, %rax
	movq	%r14, %rdx
	movq	8(%rsp), %rdi           # 8-byte Reload
	jbe	.LBB24_9
# BB#8:                                 # %if.end
                                        #   in Loop: Header=BB24_2 Depth=1
	movabsq	$1, %rdx
.LBB24_9:                               # %if.end
                                        #   in Loop: Header=BB24_2 Depth=1
	movl	(%r15), %eax
	addq	%rdx, %rcx
	leaq	1(%rdi,%rcx), %rdi
	jmp	.LBB24_10
	.align	16, 0x90
.LBB24_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	andq	%r13, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	16(%r15,%rdx), %rdx
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	jne	.LBB24_3
.LBB24_10:                              # %for.inc
                                        #   in Loop: Header=BB24_2 Depth=1
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movq	%rax, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB24_2
.LBB24_11:                              # %for.end
	movabsq	$.L.str6, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	stderr(%rip), %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.Ltmp231:
	.size	debug_sbitmap, .Ltmp231-debug_sbitmap
	.cfi_endproc

	.globl	dump_sbitmap_vector
	.align	16, 0x90
	.type	dump_sbitmap_vector,@function
dump_sbitmap_vector:                    # @dump_sbitmap_vector
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp240:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp241:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp242:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp243:
	.cfi_def_cfa_offset 64
.Ltmp244:
	.cfi_offset %rbx, -48
.Ltmp245:
	.cfi_offset %r12, -40
.Ltmp246:
	.cfi_offset %r13, -32
.Ltmp247:
	.cfi_offset %r14, -24
.Ltmp248:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r12
	movq	%rsi, %rdx
	movq	%rdi, %r13
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
                                        # kill: RDI<def> R13<kill>
	callq	fprintf
	testq	%r15, %r15
	jle	.LBB25_3
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r14
	.align	16, 0x90
.LBB25_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str8, %rsi
	movq	%r12, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	movq	%r13, %rdi
	callq	dump_sbitmap
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%r14, %rax
	jne	.LBB25_2
.LBB25_3:                               # %for.end
	movq	$10, %rdi
	movq	%r13, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp249:
	.size	dump_sbitmap_vector, .Ltmp249-dump_sbitmap_vector
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"  "
	.size	.L.str, 3

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%d"
	.size	.L.str2, 3

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"n_bits = %d, set = {"
	.size	.L.str4, 21

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%d "
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"}\n"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s\n"
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%s %d\n"
	.size	.L.str8, 7


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
