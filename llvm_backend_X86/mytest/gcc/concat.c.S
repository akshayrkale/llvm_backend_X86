	.file	"concat.c.bc"
	.text
	.globl	concat_length
	.align	16, 0x90
	.type	concat_length,@function
concat_length:                          # @concat_length
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
	subq	$80, %rsp
.Ltmp7:
	.cfi_def_cfa_offset 112
.Ltmp8:
	.cfi_offset %rbx, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	xorq	%rax, %rax
	testq	%rdi, %rdi
	leaq	(%rsp), %rcx
	movq	%rcx, 64(%rsp)
	leaq	112(%rsp), %rcx
	movq	%rcx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	je	.LBB0_6
# BB#1:                                 # %for.body.lr.ph.i
	movl	48(%rsp), %r15d
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	callq	strlen
	addq	%rbx, %rax
	movq	%r15, %rcx
	andq	%r14, %rcx
	cmpq	$40, %rcx
	ja	.LBB0_4
# BB#3:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	64(%rsp), %rsi
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$8, %r15
	movl	%r15d, 48(%rsp)
	jmp	.LBB0_5
	.align	16, 0x90
.LBB0_4:                                # %vaarg.in_mem.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	56(%rsp), %rdx
	leaq	8(%rdx), %rcx
	movq	%rcx, 56(%rsp)
.LBB0_5:                                # %vaarg.end.i
                                        #   in Loop: Header=BB0_2 Depth=1
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	jne	.LBB0_2
.LBB0_6:                                # %vconcat_length.exit
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	concat_length, .Ltmp11-concat_length
	.cfi_endproc

	.globl	concat_copy
	.align	16, 0x90
	.type	concat_copy,@function
concat_copy:                            # @concat_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
.Ltmp21:
	.cfi_def_cfa_offset 112
.Ltmp22:
	.cfi_offset %rbx, -40
.Ltmp23:
	.cfi_offset %r12, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	testq	%rbx, %rbx
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movq	%r14, %r15
	je	.LBB1_6
# BB#1:                                 # %for.body.lr.ph.i
	movq	%r14, %r15
	.align	16, 0x90
.LBB1_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r12
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	memcpy
	movl	48(%rsp), %eax
	addq	%r12, %r15
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB1_4
# BB#3:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB1_5
	.align	16, 0x90
.LBB1_4:                                # %vaarg.in_mem.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
.LBB1_5:                                # %vaarg.end.i
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	jne	.LBB1_2
.LBB1_6:                                # %vconcat_copy.exit
	xorq	%rax, %rax
	movb	%al, (%r15)
	movq	%r14, %rax
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp26:
	.size	concat_copy, .Ltmp26-concat_copy
	.cfi_endproc

	.globl	concat_copy2
	.align	16, 0x90
	.type	concat_copy2,@function
concat_copy2:                           # @concat_copy2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp31:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp32:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp33:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp34:
	.cfi_def_cfa_offset 112
.Ltmp35:
	.cfi_offset %rbx, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	testq	%rbx, %rbx
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	libiberty_concat_ptr(%rip), %r14
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	je	.LBB2_5
	.align	16, 0x90
.LBB2_1:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movl	48(%rsp), %eax
	addq	%r15, %r14
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB2_3
# BB#2:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB2_4
	.align	16, 0x90
.LBB2_3:                                # %vaarg.in_mem.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
.LBB2_4:                                # %vaarg.end.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_1
.LBB2_5:                                # %vconcat_copy.exit
	xorq	%rax, %rax
	movb	%al, (%r14)
	movq	libiberty_concat_ptr(%rip), %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp38:
	.size	concat_copy2, .Ltmp38-concat_copy2
	.cfi_endproc

	.globl	concat
	.align	16, 0x90
	.type	concat,@function
concat:                                 # @concat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp45:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp46:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp47:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 48
	subq	$112, %rsp
.Ltmp50:
	.cfi_def_cfa_offset 160
.Ltmp51:
	.cfi_offset %rbx, -48
.Ltmp52:
	.cfi_offset %r12, -40
.Ltmp53:
	.cfi_offset %r13, -32
.Ltmp54:
	.cfi_offset %r14, -24
.Ltmp55:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	testq	%r12, %r12
	leaq	(%rsp), %r13
	movq	%r13, 96(%rsp)
	leaq	160(%rsp), %rbx
	movq	%rbx, 88(%rsp)
	movq	$48, 84(%rsp)
	movq	$8, 80(%rsp)
	movq	$1, %rax
	je	.LBB3_7
# BB#1:                                 # %for.body.lr.ph.i
	movl	80(%rsp), %ebx
	xorq	%rax, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%r12, %rdi
	.align	16, 0x90
.LBB3_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r15
	callq	strlen
	addq	%r15, %rax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	$40, %rcx
	ja	.LBB3_4
# BB#3:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	96(%rsp), %rsi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$8, %rbx
	movl	%ebx, 80(%rsp)
	jmp	.LBB3_5
	.align	16, 0x90
.LBB3_4:                                # %vaarg.in_mem.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	88(%rsp), %rdx
	leaq	8(%rdx), %rcx
	movq	%rcx, 88(%rsp)
.LBB3_5:                                # %vaarg.end.i
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	jne	.LBB3_2
# BB#6:                                 # %vconcat_length.exit.loopexit
	incq	%rax
	leaq	160(%rsp), %rbx
.LBB3_7:                                # %vconcat_length.exit
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, %r14
	testq	%r12, %r12
	movq	%r13, 64(%rsp)
	movq	%rbx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
	movq	%r14, %r15
	je	.LBB3_13
# BB#8:                                 # %for.body.lr.ph.i17
	movq	%r14, %r15
	.align	16, 0x90
.LBB3_9:                                # %for.body.i21
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movl	48(%rsp), %eax
	addq	%rbx, %r15
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB3_11
# BB#10:                                # %vaarg.in_reg.i23
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	64(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 48(%rsp)
	jmp	.LBB3_12
	.align	16, 0x90
.LBB3_11:                               # %vaarg.in_mem.i26
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 56(%rsp)
.LBB3_12:                               # %vaarg.end.i30
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	(%rcx), %r12
	testq	%r12, %r12
	jne	.LBB3_9
.LBB3_13:                               # %vconcat_copy.exit
	xorq	%rax, %rax
	movb	%al, (%r15)
	movq	%r14, %rax
	addq	$112, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp56:
	.size	concat, .Ltmp56-concat
	.cfi_endproc

	.globl	reconcat
	.align	16, 0x90
	.type	reconcat,@function
reconcat:                               # @reconcat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp68:
	.cfi_def_cfa_offset 176
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
	movq	%rsi, %r13
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r9, 56(%rsp)
	movq	%r8, 48(%rsp)
	movq	%rcx, 40(%rsp)
	movq	%rdx, 32(%rsp)
	testq	%r13, %r13
	leaq	16(%rsp), %r14
	movq	%r14, 112(%rsp)
	leaq	176(%rsp), %rbx
	movq	%rbx, 104(%rsp)
	movq	$48, 100(%rsp)
	movq	$16, 96(%rsp)
	movq	$1, %rax
	je	.LBB4_7
# BB#1:                                 # %for.body.lr.ph.i
	movl	96(%rsp), %ebx
	xorq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r13, %rdi
	.align	16, 0x90
.LBB4_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r12
	callq	strlen
	addq	%r12, %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	$40, %rcx
	ja	.LBB4_4
# BB#3:                                 # %vaarg.in_reg.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	112(%rsp), %rsi
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	%rsi, %rdx
	addq	$8, %rbx
	movl	%ebx, 96(%rsp)
	jmp	.LBB4_5
	.align	16, 0x90
.LBB4_4:                                # %vaarg.in_mem.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	104(%rsp), %rdx
	leaq	8(%rdx), %rcx
	movq	%rcx, 104(%rsp)
.LBB4_5:                                # %vaarg.end.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rdx), %rdi
	testq	%rdi, %rdi
	jne	.LBB4_2
# BB#6:                                 # %vconcat_length.exit.loopexit
	incq	%rax
	leaq	176(%rsp), %rbx
.LBB4_7:                                # %vconcat_length.exit
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, %r15
	testq	%r13, %r13
	movq	%r14, 80(%rsp)
	movq	%rbx, 72(%rsp)
	movq	$48, 68(%rsp)
	movq	$16, 64(%rsp)
	movq	%r15, %r12
	je	.LBB4_13
# BB#8:                                 # %for.body.lr.ph.i18
	movq	%r15, %r12
	.align	16, 0x90
.LBB4_9:                                # %for.body.i22
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movl	64(%rsp), %eax
	addq	%rbx, %r12
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$40, %rax
	ja	.LBB4_11
# BB#10:                                # %vaarg.in_reg.i24
                                        #   in Loop: Header=BB4_9 Depth=1
	movq	80(%rsp), %rcx
	addq	%rdx, %rcx
	addq	$8, %rax
	movl	%eax, 64(%rsp)
	jmp	.LBB4_12
	.align	16, 0x90
.LBB4_11:                               # %vaarg.in_mem.i27
                                        #   in Loop: Header=BB4_9 Depth=1
	movq	72(%rsp), %rcx
	leaq	8(%rcx), %rax
	movq	%rax, 72(%rsp)
.LBB4_12:                               # %vaarg.end.i31
                                        #   in Loop: Header=BB4_9 Depth=1
	movq	(%rcx), %r13
	testq	%r13, %r13
	jne	.LBB4_9
.LBB4_13:                               # %vconcat_copy.exit
	xorq	%rax, %rax
	movb	%al, (%r12)
	movq	8(%rsp), %rdi           # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB4_15
# BB#14:                                # %if.then
	callq	free
.LBB4_15:                               # %if.end
	movq	%r15, %rax
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	reconcat, .Ltmp74-reconcat
	.cfi_endproc

	.type	libiberty_concat_ptr,@object # @libiberty_concat_ptr
	.comm	libiberty_concat_ptr,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
