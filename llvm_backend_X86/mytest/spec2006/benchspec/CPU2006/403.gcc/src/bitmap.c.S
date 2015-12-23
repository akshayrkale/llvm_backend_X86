	.file	"bitmap.c.bc"
	.text
	.globl	bitmap_release_memory
	.align	16, 0x90
	.type	bitmap_release_memory,@function
bitmap_release_memory:                  # @bitmap_release_memory
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	bitmap_obstack_init(%rip), %eax
	movq	$0, bitmap_free(%rip)
	cmpq	$1, %rax
	jne	.LBB0_1
# BB#2:                                 # %if.then
	xorq	%rax, %rax
	movabsq	$bitmap_obstack, %rdi
	xorq	%rsi, %rsi
	movb	%al, bitmap_obstack_init(%rip)
	jmp	obstack_free  # TAILCALL
.LBB0_1:                                # %if.end3
	retq
.Ltmp0:
	.size	bitmap_release_memory, .Ltmp0-bitmap_release_memory
	.cfi_endproc

	.globl	bitmap_clear
	.align	16, 0x90
	.type	bitmap_clear,@function
bitmap_clear:                           # @bitmap_clear
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB1_3
# BB#1:                                 # %for.body.lr.ph
	movq	bitmap_free(%rip), %rcx
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rcx, (%rax)
	movq	%rax, bitmap_free(%rip)
	movq	%rax, %rcx
	movq	%rdx, %rax
	jne	.LBB1_2
.LBB1_3:                                # %for.end
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	retq
.Ltmp1:
	.size	bitmap_clear, .Ltmp1-bitmap_clear
	.cfi_endproc

	.globl	bitmap_copy
	.align	16, 0x90
	.type	bitmap_copy,@function
bitmap_copy:                            # @bitmap_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp8:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp9:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp10:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp11:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 48
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	(%r13), %rax
	testq	%rax, %rax
	je	.LBB2_3
# BB#1:                                 # %for.body.lr.ph.i
	movq	bitmap_free(%rip), %rcx
	.align	16, 0x90
.LBB2_2:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rcx, (%rax)
	movq	%rax, bitmap_free(%rip)
	movq	%rax, %rcx
	movq	%rdx, %rax
	jne	.LBB2_2
.LBB2_3:                                # %bitmap_clear.exit
	movq	$0, 8(%r13)
	movq	$0, (%r13)
	movq	(%rsi), %rbx
	testq	%rbx, %rbx
	je	.LBB2_20
# BB#4:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	movabsq	$bitmap_obstack, %r14
	movabsq	$free, %r12
	.align	16, 0x90
.LBB2_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	bitmap_free(%rip), %rax
	testq	%rax, %rax
	jne	.LBB2_6
# BB#7:                                 # %if.else.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movzbl	bitmap_obstack_init(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_9
# BB#8:                                 # %if.then1.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	$1, %rax
	movb	%al, bitmap_obstack_init(%rip)
	xorq	%rsi, %rsi
	movq	$8, %rdx
	movq	%r14, %rdi
	movabsq	$xmalloc, %rcx
	movq	%r12, %r8
	callq	_obstack_begin
.LBB2_9:                                # %if.end.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	bitmap_obstack+32(%rip), %rax
	movq	bitmap_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$39, %rax
	jg	.LBB2_11
# BB#10:                                # %if.then4.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	$40, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	bitmap_obstack+24(%rip), %rcx
.LBB2_11:                               # %if.end5.i
                                        #   in Loop: Header=BB2_5 Depth=1
	addq	$40, %rcx
	movq	%rcx, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_13
# BB#12:                                # %if.then12.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movzbl	bitmap_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, bitmap_obstack+80(%rip)
.LBB2_13:                               # %if.end13.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movslq	bitmap_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+32(%rip), %r8
	movq	bitmap_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB2_15
# BB#14:                                # %if.then33.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%r8, bitmap_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_15:                               # %if.end36.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%rdx, bitmap_obstack+16(%rip)
	jmp	.LBB2_16
	.align	16, 0x90
.LBB2_6:                                # %if.then.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	(%rax), %rcx
	movq	%rcx, bitmap_free(%rip)
.LBB2_16:                               # %bitmap_element_allocate.exit
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movl	16(%rbx), %ecx
	movl	%ecx, 16(%rax)
	movq	24(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	32(%rbx), %rcx
	movq	%rcx, 32(%rax)
	testq	%r15, %r15
	jne	.LBB2_18
# BB#17:                                # %if.then
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%rax, 8(%r13)
	movq	%rax, (%r13)
	movl	16(%rbx), %ecx
	movl	%ecx, 16(%r13)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	jmp	.LBB2_19
	.align	16, 0x90
.LBB2_18:                               # %if.else
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	%r15, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%r15)
.LBB2_19:                               # %if.end
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rax, %r15
	jne	.LBB2_5
.LBB2_20:                               # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp18:
	.size	bitmap_copy, .Ltmp18-bitmap_copy
	.cfi_endproc

	.globl	bitmap_clear_bit
	.align	16, 0x90
	.type	bitmap_clear_bit,@function
bitmap_clear_bit:                       # @bitmap_clear_bit
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967168, %r8        # imm = 0xFFFFFF80
	movq	8(%rdi), %rax
	andq	%rsi, %r8
	movq	$7, %rcx
	shrq	%cl, %r8
	testq	%rax, %rax
	je	.LBB3_22
# BB#1:                                 # %lor.lhs.false.i
	movl	16(%rdi), %edx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %r8
	cmpq	%r8, %rdx
	jne	.LBB3_3
# BB#2:
	movq	%rax, %rdx
	jmp	.LBB3_10
.LBB3_3:                                # %if.end.i
	jbe	.LBB3_6
	.align	16, 0x90
.LBB3_4:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	8(%rdx), %rax
	testq	%rax, %rax
	je	.LBB3_8
# BB#5:                                 # %land.rhs.i
                                        #   in Loop: Header=BB3_4 Depth=1
	movl	16(%rdx), %ecx
	cmpq	%r8, %rcx
	ja	.LBB3_4
	jmp	.LBB3_8
	.align	16, 0x90
.LBB3_6:                                # %for.cond21.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	(%rdx), %rax
	testq	%rax, %rax
	je	.LBB3_8
# BB#7:                                 # %land.rhs24.i
                                        #   in Loop: Header=BB3_6 Depth=1
	movl	16(%rdx), %ecx
	cmpq	%r8, %rcx
	jb	.LBB3_6
.LBB3_8:                                # %if.end34.i
	movq	%rdx, 8(%rdi)
	movl	16(%rdx), %eax
	testq	%rdx, %rdx
	movl	%eax, 16(%rdi)
	je	.LBB3_22
# BB#9:                                 # %if.end34.i
	andq	%r9, %rax
	cmpq	%r8, %rax
	jne	.LBB3_22
.LBB3_10:                               # %if.then
	movq	$3, %rcx
	movq	%rsi, %r8
	shrq	%cl, %r8
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	notq	%rax
	andq	$8, %r8
	andq	%rax, 24(%rdx,%r8)
	movq	24(%rdx), %rax
	movq	32(%rdx), %rcx
	orq	%rax, %rcx
	jne	.LBB3_22
# BB#11:                                # %if.then3
	movq	8(%rdx), %rcx
	movq	(%rdx), %rax
	testq	%rcx, %rcx
	je	.LBB3_13
# BB#12:                                # %if.then.i
	movq	%rax, (%rcx)
.LBB3_13:                               # %if.end.i13
	testq	%rax, %rax
	je	.LBB3_15
# BB#14:                                # %if.then5.i
	movq	%rcx, 8(%rax)
.LBB3_15:                               # %if.end7.i
	movq	(%rdi), %rsi
	cmpq	%rdx, %rsi
	jne	.LBB3_17
# BB#16:                                # %if.then8.i
	movq	%rax, (%rdi)
.LBB3_17:                               # %if.then12.i
	testq	%rax, %rax
	jne	.LBB3_19
# BB#18:                                # %if.then12.i
	movq	%rcx, %rax
.LBB3_19:                               # %if.then12.i
	testq	%rax, %rax
	movq	%rax, 8(%rdi)
	je	.LBB3_21
# BB#20:                                # %if.then17.i
	movl	16(%rax), %eax
	movl	%eax, 16(%rdi)
.LBB3_21:                               # %bitmap_element_free.exit
	movq	bitmap_free(%rip), %rax
	movq	%rax, (%rdx)
	movq	%rdx, bitmap_free(%rip)
.LBB3_22:                               # %if.end4
	retq
.Ltmp19:
	.size	bitmap_clear_bit, .Ltmp19-bitmap_clear_bit
	.cfi_endproc

	.globl	bitmap_set_bit
	.align	16, 0x90
	.type	bitmap_set_bit,@function
bitmap_set_bit:                         # @bitmap_set_bit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp25:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp26:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp29:
	.cfi_def_cfa_offset 48
.Ltmp30:
	.cfi_offset %rbx, -40
.Ltmp31:
	.cfi_offset %r12, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967168, %rbx       # imm = 0xFFFFFF80
	movq	8(%r14), %rdi
	andq	%rsi, %rbx
	movq	$7, %rcx
	shrq	%cl, %rbx
	testq	%rdi, %rdi
	je	.LBB4_11
# BB#1:                                 # %lor.lhs.false.i
	movl	16(%r14), %eax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rbx, %rdx
	andq	%r8, %rdx
	cmpq	%rdx, %rax
	jne	.LBB4_4
# BB#2:
	movq	%rdi, %rax
	jmp	.LBB4_3
.LBB4_4:                                # %if.end.i
	jbe	.LBB4_7
	.align	16, 0x90
.LBB4_5:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_9
# BB#6:                                 # %land.rhs.i
                                        #   in Loop: Header=BB4_5 Depth=1
	movl	16(%rax), %ecx
	cmpq	%rdx, %rcx
	ja	.LBB4_5
	jmp	.LBB4_9
	.align	16, 0x90
.LBB4_7:                                # %for.cond21.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_9
# BB#8:                                 # %land.rhs24.i
                                        #   in Loop: Header=BB4_7 Depth=1
	movl	16(%rax), %ecx
	cmpq	%rdx, %rcx
	jb	.LBB4_7
.LBB4_9:                                # %if.end34.i
	movq	%rax, 8(%r14)
	movl	16(%rax), %edi
	testq	%rax, %rax
	movl	%edi, 16(%r14)
	je	.LBB4_11
# BB#10:                                # %if.end34.i
	andq	%r8, %rdi
	cmpq	%rdx, %rdi
	jne	.LBB4_11
.LBB4_3:                                # %if.else
	movq	$3, %rcx
	movq	%rsi, %rdx
	shrq	%cl, %rdx
	movq	$1, %rdi
	movq	%rsi, %rcx
	shlq	%cl, %rdi
	andq	$8, %rdx
	orq	%rdi, 24(%rax,%rdx)
	jmp	.LBB4_35
.LBB4_11:                               # %if.then
	movq	$6, %rcx
	movq	%rsi, %r15
	shrq	%cl, %r15
	movq	bitmap_free(%rip), %rax
	movq	$1, %r12
	movq	%rsi, %rcx
	shlq	%cl, %r12
	andq	$1, %r15
	testq	%rax, %rax
	je	.LBB4_13
# BB#12:                                # %if.then.i26
	movq	(%rax), %rcx
	movq	%rcx, bitmap_free(%rip)
	jmp	.LBB4_22
.LBB4_13:                               # %if.else.i27
	movzbl	bitmap_obstack_init(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_15
# BB#14:                                # %if.then1.i
	movq	$1, %rax
	movb	%al, bitmap_obstack_init(%rip)
	movabsq	$bitmap_obstack, %rdi
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	movq	$8, %rdx
	callq	_obstack_begin
.LBB4_15:                               # %if.end.i28
	movq	bitmap_obstack+32(%rip), %rax
	movq	bitmap_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$39, %rax
	jg	.LBB4_17
# BB#16:                                # %if.then4.i
	movabsq	$bitmap_obstack, %rdi
	movq	$40, %rsi
	callq	_obstack_newchunk
	movq	bitmap_obstack+24(%rip), %rcx
.LBB4_17:                               # %if.end5.i
	addq	$40, %rcx
	movq	%rcx, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB4_19
# BB#18:                                # %if.then12.i
	movzbl	bitmap_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, bitmap_obstack+80(%rip)
.LBB4_19:                               # %if.end13.i
	movslq	bitmap_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+32(%rip), %r8
	movq	bitmap_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB4_21
# BB#20:                                # %if.then33.i
	movq	%r8, bitmap_obstack+24(%rip)
	movq	%r8, %rdx
.LBB4_21:                               # %if.end36.i
	movq	%rdx, bitmap_obstack+16(%rip)
.LBB4_22:                               # %bitmap_element_allocate.exit
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movl	%ebx, 16(%rax)
	movq	%r12, 24(%rax,%r15,8)
	movl	16(%rax), %r8d
	cmpq	$0, (%r14)
	je	.LBB4_23
# BB#24:                                # %if.else.i
	movl	16(%r14), %ecx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	8(%r14), %rsi
	andq	%r8, %rdi
	cmpq	%rcx, %rdi
	leaq	16(%r14), %rdx
	jae	.LBB4_30
	.align	16, 0x90
.LBB4_25:                               # %for.cond.i21
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rbx
	movq	8(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB4_28
# BB#26:                                # %land.rhs.i22
                                        #   in Loop: Header=BB4_25 Depth=1
	movl	16(%rsi), %ecx
	cmpq	%rdi, %rcx
	ja	.LBB4_25
# BB#27:                                # %if.then13.i
	movq	%rax, (%rsi)
	jmp	.LBB4_29
	.align	16, 0x90
.LBB4_30:                               # %for.cond24.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rbx
	movq	(%rbx), %rsi
	xorq	%rcx, %rcx
	testq	%rsi, %rsi
	je	.LBB4_33
# BB#31:                                # %land.rhs27.i
                                        #   in Loop: Header=BB4_30 Depth=1
	movl	16(%rsi), %ecx
	cmpq	%rdi, %rcx
	jb	.LBB4_30
# BB#32:                                # %if.then38.i
	movq	%rax, 8(%rsi)
	movq	%rsi, %rcx
.LBB4_33:                               # %if.end41.i
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movq	%rax, (%rbx)
	jmp	.LBB4_34
.LBB4_23:                               # %if.then.i
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%r14)
	leaq	16(%r14), %rdx
	jmp	.LBB4_34
.LBB4_28:                               # %if.else16.i
	movq	%rax, (%r14)
	xorq	%rsi, %rsi
.LBB4_29:                               # %if.end.i23
	movq	%rsi, 8(%rax)
	movq	%rbx, (%rax)
	movq	%rax, 8(%rbx)
.LBB4_34:                               # %bitmap_element_link.exit
	movq	%rax, 8(%r14)
	movl	%r8d, (%rdx)
.LBB4_35:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	bitmap_set_bit, .Ltmp34-bitmap_set_bit
	.cfi_endproc

	.globl	bitmap_bit_p
	.align	16, 0x90
	.type	bitmap_bit_p,@function
bitmap_bit_p:                           # @bitmap_bit_p
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967168, %r8        # imm = 0xFFFFFF80
	andq	%rsi, %r8
	movq	8(%rdi), %rdx
	movq	$7, %rcx
	shrq	%cl, %r8
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB5_11
# BB#1:                                 # %lor.lhs.false.i
	movl	16(%rdi), %ecx
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %r8
	cmpq	%r8, %rcx
	jne	.LBB5_3
# BB#2:
	movq	%rdx, %r9
	jmp	.LBB5_10
.LBB5_3:                                # %if.end.i
	jbe	.LBB5_6
	.align	16, 0x90
.LBB5_4:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r9
	movq	8(%r9), %rdx
	testq	%rdx, %rdx
	je	.LBB5_8
# BB#5:                                 # %land.rhs.i
                                        #   in Loop: Header=BB5_4 Depth=1
	movl	16(%r9), %ecx
	cmpq	%r8, %rcx
	ja	.LBB5_4
	jmp	.LBB5_8
	.align	16, 0x90
.LBB5_6:                                # %for.cond21.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r9
	movq	(%r9), %rdx
	testq	%rdx, %rdx
	je	.LBB5_8
# BB#7:                                 # %land.rhs24.i
                                        #   in Loop: Header=BB5_6 Depth=1
	movl	16(%r9), %ecx
	cmpq	%r8, %rcx
	jb	.LBB5_6
.LBB5_8:                                # %if.end34.i
	movq	%r9, 8(%rdi)
	movl	16(%r9), %edx
	testq	%r9, %r9
	movl	%edx, 16(%rdi)
	je	.LBB5_11
# BB#9:                                 # %if.end34.i
	andq	%r10, %rdx
	cmpq	%r8, %rdx
	jne	.LBB5_11
.LBB5_10:                               # %if.end
	movq	$3, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	andq	$8, %rax
	movq	24(%r9,%rax), %rax
	andq	$63, %rsi
	movq	%rsi, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
.LBB5_11:                               # %return
	retq
.Ltmp35:
	.size	bitmap_bit_p, .Ltmp35-bitmap_bit_p
	.cfi_endproc

	.globl	bitmap_first_set_bit
	.align	16, 0x90
	.type	bitmap_first_set_bit,@function
bitmap_first_set_bit:                   # @bitmap_first_set_bit
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %r9
	testq	%r9, %r9
	je	.LBB6_1
# BB#2:                                 # %if.end
	movq	24(%r9), %rax
	xorq	%r8, %r8
	testq	%rax, %rax
	jne	.LBB6_4
# BB#3:                                 # %if.then2
	movq	32(%r9), %rax
	movq	$64, %r8
.LBB6_4:                                # %if.end5
	movq	%rax, %rdi
	negq	%rdi
	andq	%rax, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	jne	.LBB6_5
# BB#6:                                 # %if.end5
	movabsq	$1, %rsi
	jmp	.LBB6_7
.LBB6_1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB6_5:
	movabsq	$0, %rsi
.LBB6_7:                                # %if.end5
	movq	$32, %rcx
	movq	%rdi, %rdx
	shrq	%cl, %rdx
	testq	%rax, %rax
	je	.LBB6_9
# BB#8:                                 # %if.end5
	movq	%rdi, %rdx
.LBB6_9:                                # %if.end5
	movq	$5, %rcx
	shlq	%cl, %rsi
	testq	$65535, %rdx            # imm = 0xFFFF
	jne	.LBB6_11
# BB#10:                                # %if.then12
	movq	$16, %rcx
	shrq	%cl, %rdx
	orq	$16, %rsi
.LBB6_11:                               # %if.end15
	testq	$255, %rdx
	jne	.LBB6_13
# BB#12:                                # %if.then18
	movq	$8, %rcx
	shrq	%cl, %rdx
	addq	$8, %rsi
.LBB6_13:                               # %if.end21
	testq	$240, %rdx
	je	.LBB6_15
# BB#14:                                # %if.end21
	addq	$4, %rsi
.LBB6_15:                               # %if.end21
	testq	$204, %rdx
	je	.LBB6_17
# BB#16:                                # %if.end21
	addq	$2, %rsi
.LBB6_17:                               # %if.end21
	testq	$170, %rdx
	je	.LBB6_18
# BB#19:                                # %if.end21
	movabsq	$1, %rdx
	jmp	.LBB6_20
.LBB6_18:
	movabsq	$0, %rdx
.LBB6_20:                               # %if.end21
	movl	16(%r9), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	orq	%r8, %rax
	orq	%rdx, %rax
	addq	%rsi, %rax
	retq
.Ltmp36:
	.size	bitmap_first_set_bit, .Ltmp36-bitmap_first_set_bit
	.cfi_endproc

	.globl	bitmap_last_set_bit
	.align	16, 0x90
	.type	bitmap_last_set_bit,@function
bitmap_last_set_bit:                    # @bitmap_last_set_bit
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB7_1
	.align	16, 0x90
.LBB7_2:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_2
# BB#3:                                 # %while.end
	movq	32(%rax), %rdx
	testq	%rdx, %rdx
	movq	$64, %r8
	jne	.LBB7_5
# BB#4:                                 # %if.then4
	movq	24(%rax), %rdx
	xorq	%r8, %r8
.LBB7_5:                                # %if.end7
	movq	$32, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB7_6
# BB#7:                                 # %if.end7
	movabsq	$1, %rdi
	jmp	.LBB7_8
.LBB7_1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB7_6:
	movabsq	$0, %rdi
.LBB7_8:                                # %if.end7
	jne	.LBB7_10
# BB#9:                                 # %if.end7
	movq	%rdx, %rsi
.LBB7_10:                               # %if.end7
	movq	$5, %rcx
	shlq	%cl, %rdi
	movabsq	$4294901760, %rcx       # imm = 0xFFFF0000
	testq	%rcx, %rsi
	je	.LBB7_12
# BB#11:                                # %if.then12
	movq	$16, %rcx
	shrq	%cl, %rsi
	orq	$16, %rdi
.LBB7_12:                               # %if.end15
	testq	$65280, %rsi            # imm = 0xFF00
	je	.LBB7_14
# BB#13:                                # %if.then18
	movq	$8, %rcx
	shrq	%cl, %rsi
	addq	$8, %rdi
.LBB7_14:                               # %if.end21
	testq	$240, %rsi
	je	.LBB7_16
# BB#15:                                # %if.then24
	movq	$4, %rcx
	shrq	%cl, %rsi
	addq	$4, %rdi
.LBB7_16:                               # %if.end27
	testq	$12, %rsi
	je	.LBB7_18
# BB#17:                                # %if.then30
	movq	$2, %rcx
	shrq	%cl, %rsi
	addq	$2, %rdi
.LBB7_18:                               # %if.end33
	movq	$1, %rcx
	shrq	%cl, %rsi
	movl	16(%rax), %eax
	andq	$1, %rsi
	addq	%rdi, %rsi
	movq	$7, %rcx
	shlq	%cl, %rax
	orq	%r8, %rax
	addq	%rsi, %rax
	retq
.Ltmp37:
	.size	bitmap_last_set_bit, .Ltmp37-bitmap_last_set_bit
	.cfi_endproc

	.globl	bitmap_operation
	.align	16, 0x90
	.type	bitmap_operation,@function
bitmap_operation:                       # @bitmap_operation
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp44:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp45:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp46:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp47:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp49:
	.cfi_def_cfa_offset 112
.Ltmp50:
	.cfi_offset %rbx, -48
.Ltmp51:
	.cfi_offset %r12, -40
.Ltmp52:
	.cfi_offset %r13, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movq	(%rsi), %rbx
	movq	(%rdx), %rsi
	movq	%rdi, %r9
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rbx, %rbx
	movq	%rdi, %r8
	je	.LBB8_2
# BB#1:                                 # %cond.true
	movl	16(%rbx), %r8d
.LBB8_2:                                # %cond.end
	testq	%rsi, %rsi
	je	.LBB8_4
# BB#3:                                 # %cond.true3
	movl	16(%rsi), %edi
.LBB8_4:                                # %cond.end6
	movq	(%r9), %rax
	movq	$0, 8(%r9)
	movq	$0, (%r9)
	xorq	%r12, %r12
	movq	%rbx, %rdx
	orq	%rsi, %rdx
	je	.LBB8_80
# BB#5:                                 # %while.body.lr.ph
	xorq	%r12, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB8_6:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_20 Depth 2
                                        #     Child Loop BB8_73 Depth 2
                                        #     Child Loop BB8_68 Depth 2
	movq	%rdi, %rcx
	andq	%r14, %rcx
	movq	%r8, %rdx
	andq	%r14, %rdx
	cmpq	%rcx, %rdx
	jne	.LBB8_12
# BB#7:                                 # %if.then
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	movq	%r14, %r10
	je	.LBB8_9
# BB#8:                                 # %cond.true14
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	16(%rcx), %r10d
.LBB8_9:                                # %cond.end17
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	(%rsi), %rdx
	testq	%rdx, %rdx
	jne	.LBB8_11
# BB#10:                                #   in Loop: Header=BB8_6 Depth=1
	movq	%r8, %r15
	movq	%rsi, %r11
	movq	%rbx, %rdi
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	%r10, %r8
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_12:                               # %if.else
                                        #   in Loop: Header=BB8_6 Depth=1
	jae	.LBB8_16
# BB#13:                                # %if.then27
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	(%rbx), %rcx
	testq	%rcx, %rcx
	jne	.LBB8_15
# BB#14:                                #   in Loop: Header=BB8_6 Depth=1
	movq	%r8, %r15
	movabsq	$bitmap_zero_bits, %rcx
	movq	%rcx, %r11
	movq	%rbx, %rdi
	movq	%r14, %r8
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_11:                               # %cond.true21
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	16(%rdx), %edi
	movq	%rdi, 56(%rsp)          # 8-byte Spill
	movq	%r8, %r15
	movq	%rsi, %r11
	movq	%rbx, %rdi
	movq	%r10, %r8
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_16:                               # %if.else35
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	(%rsi), %rcx
	testq	%rcx, %rcx
	jne	.LBB8_18
# BB#17:                                #   in Loop: Header=BB8_6 Depth=1
	movq	%rdi, %r15
	movq	%rsi, %r11
	movabsq	$bitmap_zero_bits, %rcx
	movq	%rcx, %rdi
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB8_19
.LBB8_15:                               # %cond.true30
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	16(%rcx), %edx
	movq	%r8, %r15
	movabsq	$bitmap_zero_bits, %rsi
	movq	%rsi, %r11
	movq	%rbx, %rdi
	movq	%rdx, %r8
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	jmp	.LBB8_19
.LBB8_18:                               # %cond.true38
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	16(%rcx), %edx
	movq	%rdi, %r15
	movq	%rsi, %r11
	movabsq	$bitmap_zero_bits, %rsi
	movq	%rsi, %rdi
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB8_19:                               # %while.cond44.preheader
                                        #   in Loop: Header=BB8_6 Depth=1
	xorq	%r13, %r13
	testq	%rax, %rax
	je	.LBB8_24
	.align	16, 0x90
.LBB8_20:                               # %land.rhs
                                        #   Parent Loop BB8_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r13
	movl	16(%r13), %eax
	movq	%r15, %rcx
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	jae	.LBB8_22
# BB#21:                                # %while.body48
                                        #   in Loop: Header=BB8_20 Depth=2
	movq	(%r13), %rax
	xorq	%rdx, %rdx
	movq	bitmap_free(%rip), %rcx
	testq	%rax, %rax
	movq	%rcx, (%r13)
	movq	%r13, bitmap_free(%rip)
	movq	$1, %r12
	jne	.LBB8_20
	jmp	.LBB8_26
	.align	16, 0x90
.LBB8_22:                               # %land.lhs.true
                                        #   in Loop: Header=BB8_6 Depth=1
	jne	.LBB8_24
# BB#23:                                # %if.then54
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	(%r13), %rax
	jmp	.LBB8_37
	.align	16, 0x90
.LBB8_24:                               # %if.else56
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	bitmap_free(%rip), %rax
	testq	%rax, %rax
	jne	.LBB8_25
# BB#27:                                # %if.else.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	%r11, 8(%rsp)           # 8-byte Spill
	movzbl	bitmap_obstack_init(%rip), %eax
	testq	%rax, %rax
	jne	.LBB8_29
# BB#28:                                # %if.then1.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	$1, %rax
	movb	%al, bitmap_obstack_init(%rip)
	xorq	%rsi, %rsi
	movq	$8, %rdx
	movabsq	$bitmap_obstack, %rdi
	movabsq	$xmalloc, %rcx
	movq	%r8, %rax
	movabsq	$free, %r8
	movq	%rax, 16(%rsp)          # 8-byte Spill
	callq	_obstack_begin
	movq	16(%rsp), %r8           # 8-byte Reload
.LBB8_29:                               # %if.end.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	bitmap_obstack+32(%rip), %rcx
	movq	bitmap_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$39, %rcx
	jg	.LBB8_31
# BB#30:                                # %if.then4.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	$40, %rsi
	movabsq	$bitmap_obstack, %rdi
	movq	%r8, 16(%rsp)           # 8-byte Spill
	callq	_obstack_newchunk
	movq	16(%rsp), %r8           # 8-byte Reload
	movq	bitmap_obstack+24(%rip), %rax
.LBB8_31:                               # %if.end5.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r8, %r10
	addq	$40, %rax
	movq	%rax, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+16(%rip), %rcx
	cmpq	%rcx, %rax
	movq	24(%rsp), %r9           # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r14
	jne	.LBB8_33
# BB#32:                                # %if.then12.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movzbl	bitmap_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, bitmap_obstack+80(%rip)
.LBB8_33:                               # %if.end13.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movslq	bitmap_obstack+48(%rip), %rdx
	addq	%rdx, %rax
	notq	%rdx
	andq	%rax, %rdx
	movq	%rdx, bitmap_obstack+24(%rip)
	movq	bitmap_obstack+32(%rip), %r8
	movq	bitmap_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB8_35
# BB#34:                                # %if.then33.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r8, bitmap_obstack+24(%rip)
	movq	%r8, %rdx
.LBB8_35:                               # %if.end36.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rdx, bitmap_obstack+16(%rip)
	movq	8(%rsp), %r11           # 8-byte Reload
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r10, %r8
	jmp	.LBB8_36
	.align	16, 0x90
.LBB8_25:                               #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, %rdx
	movq	%rax, %r13
.LBB8_26:                               # %if.then.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, %rcx
	movq	(%rcx), %rax
	movq	%rax, bitmap_free(%rip)
	movq	%rdx, %r13
.LBB8_36:                               # %bitmap_element_allocate.exit
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, %rax
	movq	$0, 32(%rcx)
	movq	$0, 24(%rcx)
	movq	%rcx, %r13
.LBB8_37:                               # %if.end57
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	$4, %rcx
	ja	.LBB8_39
# BB#38:                                # %if.end57
                                        #   in Loop: Header=BB8_6 Depth=1
	jmpq	*.LJTI8_0(,%rcx,8)
.LBB8_40:                               # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r14, %r9
	movq	24(%rdi), %rsi
	movq	32(%rdi), %r10
	movq	24(%r11), %rcx
	movq	32(%r11), %rdx
	movq	24(%r13), %rbx
	movq	32(%r13), %rdi
	andq	%rsi, %rcx
	cmpq	%rbx, %rcx
	movabsq	$0, %rsi
	je	.LBB8_42
# BB#41:                                # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rsi
.LBB8_42:                               # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%r12, %rsi
	andq	%r10, %rdx
	jmp	.LBB8_43
.LBB8_46:                               # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	24(%r11), %rsi
	movq	32(%r11), %rbx
	movq	24(%rdi), %rcx
	movq	32(%rdi), %rdx
	movq	24(%r13), %rdi
	movq	32(%r13), %r10
	notq	%rsi
	andq	%rsi, %rcx
	cmpq	%rdi, %rcx
	movabsq	$0, %rsi
	je	.LBB8_48
# BB#47:                                # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rsi
.LBB8_48:                               # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%r12, %rsi
	notq	%rbx
	andq	%rbx, %rdx
	jmp	.LBB8_49
.LBB8_52:                               # %do.body112
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r14, %r9
	movq	24(%rdi), %rsi
	movq	32(%rdi), %r10
	movq	24(%r11), %rcx
	movq	32(%r11), %rdx
	movq	24(%r13), %rbx
	movq	32(%r13), %rdi
	orq	%rsi, %rcx
	cmpq	%rbx, %rcx
	movabsq	$0, %rsi
	je	.LBB8_54
# BB#53:                                # %do.body112
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rsi
.LBB8_54:                               # %do.body112
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%r12, %rsi
	orq	%r10, %rdx
.LBB8_43:                               # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	cmpq	%rdi, %rdx
	movabsq	$0, %rdi
	je	.LBB8_45
# BB#44:                                # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rdi
.LBB8_45:                               # %do.body
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%rdi, %rsi
	movq	%rcx, 24(%r13)
	movq	%rdx, 32(%r13)
	movq	%rsi, %r12
	movq	%r9, %r14
	jmp	.LBB8_63
.LBB8_58:                               # %do.body180
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	24(%rdi), %rsi
	movq	32(%rdi), %r10
	movq	24(%r11), %rcx
	movq	32(%r11), %rdx
	movq	24(%r13), %rbx
	movq	32(%r13), %rdi
	xorq	%rsi, %rcx
	cmpq	%rbx, %rcx
	movabsq	$0, %rsi
	je	.LBB8_60
# BB#59:                                # %do.body180
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rsi
.LBB8_60:                               # %do.body180
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r8, %r14
	orq	%r12, %rsi
	xorq	%r10, %rdx
	cmpq	%rdi, %rdx
	movabsq	$0, %rdi
	je	.LBB8_62
# BB#61:                                # %do.body180
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rdi
.LBB8_62:                               # %do.body180
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%rdi, %rsi
	movq	%rcx, 24(%r13)
	movq	%rdx, 32(%r13)
	movq	%rsi, %r12
	movq	%r14, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r14
.LBB8_63:                               # %sw.epilog
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	24(%rsp), %r9           # 8-byte Reload
	jmp	.LBB8_64
.LBB8_55:                               # %do.body145
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	24(%r11), %rsi
	movq	32(%r11), %rbx
	movq	24(%rdi), %rcx
	movq	32(%rdi), %rdx
	movq	24(%r13), %rdi
	movq	32(%r13), %r10
	notq	%rsi
	orq	%rsi, %rcx
	cmpq	%rdi, %rcx
	movabsq	$0, %rsi
	je	.LBB8_57
# BB#56:                                # %do.body145
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rsi
.LBB8_57:                               # %do.body145
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%r12, %rsi
	notq	%rbx
	orq	%rbx, %rdx
.LBB8_49:                               # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	cmpq	%r10, %rdx
	movabsq	$0, %rdi
	je	.LBB8_51
# BB#50:                                # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	movabsq	$1, %rdi
.LBB8_51:                               # %do.body78
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%rdi, %rsi
	movq	%rcx, 24(%r13)
	movq	%rdx, 32(%r13)
	movq	%rsi, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r14
.LBB8_64:                               # %sw.epilog
                                        #   in Loop: Header=BB8_6 Depth=1
	orq	%rcx, %rdx
	jne	.LBB8_65
# BB#78:                                # %if.else215
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	bitmap_free(%rip), %rcx
	movq	%rcx, (%r13)
	movq	%r13, bitmap_free(%rip)
	jmp	.LBB8_79
	.align	16, 0x90
.LBB8_65:                               # %if.then213
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	%r15d, 16(%r13)
	cmpq	$0, (%r9)
	jne	.LBB8_67
# BB#66:                                # %if.then.i335
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	$0, 8(%r13)
	movq	$0, (%r13)
	movq	%r13, (%r9)
	jmp	.LBB8_77
	.align	16, 0x90
.LBB8_67:                               # %if.else.i336
                                        #   in Loop: Header=BB8_6 Depth=1
	movl	16(%r9), %esi
	movq	8(%r9), %rcx
	movq	%r15, %rdx
	andq	%r14, %rdx
	cmpq	%rsi, %rdx
	jae	.LBB8_73
	.align	16, 0x90
.LBB8_68:                               # %for.cond.i
                                        #   Parent Loop BB8_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rsi
	movq	8(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB8_71
# BB#69:                                # %land.rhs.i
                                        #   in Loop: Header=BB8_68 Depth=2
	movl	16(%rcx), %edi
	cmpq	%rdx, %rdi
	ja	.LBB8_68
# BB#70:                                # %if.then13.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, (%rcx)
	jmp	.LBB8_72
	.align	16, 0x90
.LBB8_73:                               # %for.cond24.i
                                        #   Parent Loop BB8_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rsi
	movq	(%rsi), %rcx
	xorq	%rdi, %rdi
	testq	%rcx, %rcx
	je	.LBB8_76
# BB#74:                                # %land.rhs27.i
                                        #   in Loop: Header=BB8_73 Depth=2
	movl	16(%rcx), %edi
	cmpq	%rdx, %rdi
	jb	.LBB8_73
# BB#75:                                # %if.then38.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, 8(%rcx)
	movq	%rcx, %rdi
.LBB8_76:                               # %if.end41.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rdi, (%r13)
	movq	%rsi, 8(%r13)
	movq	%r13, (%rsi)
	jmp	.LBB8_77
.LBB8_71:                               # %if.else16.i
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, (%r9)
	xorq	%rcx, %rcx
.LBB8_72:                               # %if.end.i338
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%rcx, 8(%r13)
	movq	%rsi, (%r13)
	movq	%r13, 8(%rsi)
.LBB8_77:                               # %bitmap_element_link.exit
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r13, 8(%r9)
	movl	%r15d, 16(%r9)
.LBB8_79:                               # %while.cond.backedge
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	movq	40(%rsp), %rsi          # 8-byte Reload
	orq	%rsi, %rcx
	jne	.LBB8_6
.LBB8_80:                               # %while.end218
	testq	%rax, %rax
	je	.LBB8_84
# BB#81:
	movq	%rax, %rdx
	.align	16, 0x90
.LBB8_82:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rcx
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	jne	.LBB8_82
# BB#83:                                # %for.end
	movq	bitmap_free(%rip), %rdx
	movq	%rdx, (%rcx)
	movq	%rax, bitmap_free(%rip)
	movq	$1, %r12
.LBB8_84:                               # %if.end225
	movq	%r12, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_39:                               # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.bitmap_operation, %rdx
	movq	$616, %rsi              # imm = 0x268
	callq	fancy_abort
.Ltmp55:
	.size	bitmap_operation, .Ltmp55-bitmap_operation
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_40
	.quad	.LBB8_46
	.quad	.LBB8_52
	.quad	.LBB8_58
	.quad	.LBB8_55

	.text
	.globl	bitmap_equal_p
	.align	16, 0x90
	.type	bitmap_equal_p,@function
bitmap_equal_p:                         # @bitmap_equal_p
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp57:
	.cfi_def_cfa_offset 32
	movq	%rsi, %rax
	movq	%rdi, %rdx
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	leaq	(%rsp), %rdi
	movq	$3, %rcx
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	bitmap_operation
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	jmp	.LBB9_3
.LBB9_1:
	movabsq	$0, %rax
.LBB9_3:                                # %entry
	movq	(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB9_6
# BB#4:                                 # %for.body.lr.ph.i
	movq	bitmap_free(%rip), %rdx
	.align	16, 0x90
.LBB9_5:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rsi
	testq	%rsi, %rsi
	movq	%rdx, (%rcx)
	movq	%rcx, bitmap_free(%rip)
	movq	%rcx, %rdx
	movq	%rsi, %rcx
	jne	.LBB9_5
.LBB9_6:                                # %bitmap_clear.exit
	addq	$24, %rsp
	retq
.Ltmp58:
	.size	bitmap_equal_p, .Ltmp58-bitmap_equal_p
	.cfi_endproc

	.globl	bitmap_ior_and_compl
	.align	16, 0x90
	.type	bitmap_ior_and_compl,@function
bitmap_ior_and_compl:                   # @bitmap_ior_and_compl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 48
.Ltmp65:
	.cfi_offset %rbx, -24
.Ltmp66:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	leaq	(%rsp), %r14
	movq	$1, %rcx
	movq	%r14, %rdi
	callq	bitmap_operation
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	bitmap_operation
	movq	(%rsp), %rax
	testq	%rax, %rax
	je	.LBB10_3
# BB#1:                                 # %for.body.lr.ph.i
	movq	bitmap_free(%rip), %rcx
	.align	16, 0x90
.LBB10_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rcx, (%rax)
	movq	%rax, bitmap_free(%rip)
	movq	%rax, %rcx
	movq	%rdx, %rax
	jne	.LBB10_2
.LBB10_3:                               # %bitmap_clear.exit
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp67:
	.size	bitmap_ior_and_compl, .Ltmp67-bitmap_ior_and_compl
	.cfi_endproc

	.globl	bitmap_union_of_diff
	.align	16, 0x90
	.type	bitmap_union_of_diff,@function
bitmap_union_of_diff:                   # @bitmap_union_of_diff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp74:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp75:
	.cfi_def_cfa_offset 64
.Ltmp76:
	.cfi_offset %rbx, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	leaq	8(%rsp), %r15
	movq	$1, %rcx
	movq	%r15, %rdi
	movq	%rdx, %rsi
	movq	%rax, %rdx
	callq	bitmap_operation
	movq	$2, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	bitmap_operation
	movq	8(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB11_3
# BB#1:                                 # %for.body.lr.ph.i
	movq	bitmap_free(%rip), %rdx
	.align	16, 0x90
.LBB11_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rsi
	testq	%rsi, %rsi
	movq	%rdx, (%rcx)
	movq	%rcx, bitmap_free(%rip)
	movq	%rcx, %rdx
	movq	%rsi, %rcx
	jne	.LBB11_2
.LBB11_3:                               # %bitmap_clear.exit
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	bitmap_union_of_diff, .Ltmp79-bitmap_union_of_diff
	.cfi_endproc

	.globl	bitmap_initialize
	.align	16, 0x90
	.type	bitmap_initialize,@function
bitmap_initialize:                      # @bitmap_initialize
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movq	%rdi, %rax
	retq
.Ltmp80:
	.size	bitmap_initialize, .Ltmp80-bitmap_initialize
	.cfi_endproc

	.globl	debug_bitmap_file
	.align	16, 0x90
	.type	debug_bitmap_file,@function
debug_bitmap_file:                      # @debug_bitmap_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp87:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp88:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp89:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp90:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp91:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp92:
	.cfi_def_cfa_offset 64
.Ltmp93:
	.cfi_offset %rbx, -48
.Ltmp94:
	.cfi_offset %r12, -40
.Ltmp95:
	.cfi_offset %r13, -32
.Ltmp96:
	.cfi_offset %r14, -24
.Ltmp97:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movabsq	$.L.str1, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	(%rbx), %rdx
	movabsq	$.L.str2, %r15
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movabsq	$.L.str3, %rdi
	movq	$11, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	8(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movl	16(%rbx), %edx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	(%rbx), %r12
	jmp	.LBB13_1
	.align	16, 0x90
.LBB13_14:                              # %for.inc29.1
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	$3, %rsi
	movq	$1, %rdx
	movabsq	$.L.str11, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	(%r12), %r12
.LBB13_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_3 Depth 2
                                        #     Child Loop BB13_9 Depth 2
	testq	%r12, %r12
	je	.LBB13_15
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	$9, %rdi
	movq	%r14, %rsi
	callq	fputc
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str2, %rbx
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movq	$8, %rsi
	movq	$1, %rdx
	movabsq	$.L.str6, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	(%r12), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movq	$8, %rsi
	movq	$1, %rdx
	movabsq	$.L.str7, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	8(%r12), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	fprintf
	movl	16(%r12), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str8, %rsi
	callq	fprintf
	movq	$26, %r13
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB13_3:                               # %for.body18
                                        #   Parent Loop BB13_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r12), %rax
	movq	$1, %rdx
	movq	%rbx, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB13_7
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB13_3 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$71, %rax
	jl	.LBB13_6
# BB#5:                                 # %if.then21
                                        #   in Loop: Header=BB13_3 Depth=2
	movq	$4, %rsi
	movq	$1, %rdx
	movabsq	$.L.str9, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	$24, %r13
.LBB13_6:                               # %if.end
                                        #   in Loop: Header=BB13_3 Depth=2
	movl	16(%r12), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rbx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str10, %rsi
	callq	fprintf
	addq	$4, %r13
.LBB13_7:                               # %for.inc
                                        #   in Loop: Header=BB13_3 Depth=2
	incq	%rbx
	cmpq	$64, %rbx
	jne	.LBB13_3
# BB#8:                                 # %for.inc29
                                        #   in Loop: Header=BB13_1 Depth=1
	xorq	%r15, %r15
	.align	16, 0x90
.LBB13_9:                               # %for.body18.1
                                        #   Parent Loop BB13_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%r12), %rax
	movq	$1, %rdx
	movq	%r15, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB13_13
# BB#10:                                # %if.then.1
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$71, %rax
	jl	.LBB13_12
# BB#11:                                # %if.then21.1
                                        #   in Loop: Header=BB13_9 Depth=2
	movq	$4, %rsi
	movq	$1, %rdx
	movabsq	$.L.str9, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	$24, %r13
.LBB13_12:                              # %if.end.1
                                        #   in Loop: Header=BB13_9 Depth=2
	movl	16(%r12), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r15,%rax), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str10, %rsi
	callq	fprintf
	addq	$4, %r13
.LBB13_13:                              # %for.inc.1
                                        #   in Loop: Header=BB13_9 Depth=2
	incq	%r15
	cmpq	$64, %r15
	jne	.LBB13_9
	jmp	.LBB13_14
.LBB13_15:                              # %for.end35
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp98:
	.size	debug_bitmap_file, .Ltmp98-debug_bitmap_file
	.cfi_endproc

	.globl	debug_bitmap
	.align	16, 0x90
	.type	debug_bitmap,@function
debug_bitmap:                           # @debug_bitmap
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stdout(%rip), %rax
	movq	%rdi, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	jmp	debug_bitmap_file  # TAILCALL
.Ltmp99:
	.size	debug_bitmap, .Ltmp99-debug_bitmap
	.cfi_endproc

	.globl	bitmap_print
	.align	16, 0x90
	.type	bitmap_print,@function
bitmap_print:                           # @bitmap_print
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp106:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp107:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp108:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp109:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp110:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp111:
	.cfi_def_cfa_offset 64
.Ltmp112:
	.cfi_offset %rbx, -48
.Ltmp113:
	.cfi_offset %r12, -40
.Ltmp114:
	.cfi_offset %r13, -32
.Ltmp115:
	.cfi_offset %r14, -24
.Ltmp116:
	.cfi_offset %r15, -16
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rdi, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %rdi
	movq	%rax, %rsi
	callq	fputs
	movq	(%rbx), %r13
	testq	%r13, %r13
	je	.LBB15_13
# BB#1:
	movabsq	$.L.str12, %rdx
	.align	16, 0x90
.LBB15_2:                               # %for.cond6.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB15_3 Depth 2
                                        #     Child Loop BB15_8 Depth 2
	movq	24(%r13), %r15
	testq	%r15, %r15
	movq	$1, %r12
	je	.LBB15_7
	.align	16, 0x90
.LBB15_3:                               # %for.body13
                                        #   Parent Loop BB15_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r12), %rbx
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	testq	%r15, %r14
	je	.LBB15_6
# BB#4:                                 # %if.then15
                                        #   in Loop: Header=BB15_3 Depth=2
	movl	16(%r13), %eax
	notq	%r14
	movq	$7, %rcx
	shlq	%cl, %rax
	addq	%rax, %rbx
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str13, %rsi
	movq	%rbx, %rcx
	callq	fprintf
	andq	%r14, %r15
	movabsq	$.L.str14, %rdx
	je	.LBB15_5
.LBB15_6:                               # %for.inc
                                        #   in Loop: Header=BB15_3 Depth=2
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB15_3
	jmp	.LBB15_7
.LBB15_5:                               #   in Loop: Header=BB15_2 Depth=1
	movabsq	$.L.str14, %rdx
	.align	16, 0x90
.LBB15_7:                               # %if.end25
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	32(%r13), %r15
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB15_12
	.align	16, 0x90
.LBB15_8:                               # %for.body13.1
                                        #   Parent Loop BB15_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rbx
	movq	%r14, %rcx
	shlq	%cl, %rbx
	testq	%r15, %rbx
	je	.LBB15_11
# BB#9:                                 # %if.then15.1
                                        #   in Loop: Header=BB15_8 Depth=2
	movl	16(%r13), %eax
	notq	%rbx
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%r14,%rax), %rcx
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	movabsq	$.L.str13, %rsi
	callq	fprintf
	andq	%rbx, %r15
	movabsq	$.L.str14, %rdx
	je	.LBB15_10
.LBB15_11:                              # %for.inc.1
                                        #   in Loop: Header=BB15_8 Depth=2
	incq	%r14
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB15_8
	jmp	.LBB15_12
.LBB15_10:                              #   in Loop: Header=BB15_2 Depth=1
	movabsq	$.L.str14, %rdx
	.align	16, 0x90
.LBB15_12:                              # %if.end25.1
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB15_2
.LBB15_13:                              # %do.end
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputs  # TAILCALL
.Ltmp117:
	.size	bitmap_print, .Ltmp117-bitmap_print
	.cfi_endproc

	.type	bitmap_free,@object     # @bitmap_free
	.local	bitmap_free
	.comm	bitmap_free,8,8
	.type	bitmap_obstack_init,@object # @bitmap_obstack_init
	.local	bitmap_obstack_init
	.comm	bitmap_obstack_init,1,1
	.type	bitmap_obstack,@object  # @bitmap_obstack
	.local	bitmap_obstack
	.comm	bitmap_obstack,88,8
	.type	bitmap_zero_bits,@object # @bitmap_zero_bits
	.comm	bitmap_zero_bits,40,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"bitmap.c"
	.size	.L.str, 9

	.type	.L__FUNCTION__.bitmap_operation,@object # @__FUNCTION__.bitmap_operation
.L__FUNCTION__.bitmap_operation:
	.asciz	"bitmap_operation"
	.size	.L__FUNCTION__.bitmap_operation, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\nfirst = "
	.size	.L.str1, 10

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%p"
	.size	.L.str2, 3

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	" current = "
	.size	.L.str3, 12

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	" indx = %u\n"
	.size	.L.str4, 12

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	" next = "
	.size	.L.str6, 9

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	" prev = "
	.size	.L.str7, 9

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	" indx = %u\n\t\tbits = {"
	.size	.L.str8, 22

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\n\t\t\t"
	.size	.L.str9, 5

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	" %u"
	.size	.L.str10, 4

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	" }\n"
	.size	.L.str11, 4

	.type	.L.str12,@object        # @.str12
.L.str12:
	.zero	1
	.size	.L.str12, 1

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"%s%d"
	.size	.L.str13, 5

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	", "
	.size	.L.str14, 3


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
