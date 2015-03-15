	.file	"biariencode.c.bc"
	.text
	.globl	arienco_create_encoding_environment
	.align	16, 0x90
	.type	arienco_create_encoding_environment,@function
arienco_create_encoding_environment:    # @arienco_create_encoding_environment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	$1, %rdi
	movq	$104, %rsi
	callq	calloc
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	callq	no_mem_exit
.LBB0_2:                                # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp4:
	.size	arienco_create_encoding_environment, .Ltmp4-arienco_create_encoding_environment
	.cfi_endproc

	.globl	arienco_delete_encoding_environment
	.align	16, 0x90
	.type	arienco_delete_encoding_environment,@function
arienco_delete_encoding_environment:    # @arienco_delete_encoding_environment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
.Ltmp8:
	.cfi_offset %rbx, -16
	testq	%rdi, %rdi
	je	.LBB1_1
# BB#2:                                 # %if.else
	popq	%rbx
	jmp	free  # TAILCALL
.LBB1_1:                                # %if.then
	movabsq	$errortext, %rbx
	movabsq	$.L.str1, %rdx
	movq	$300, %rsi              # imm = 0x12C
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	snprintf
	movq	$200, %rsi
	movq	%rbx, %rdi
	popq	%rbx
	jmp	error  # TAILCALL
.Ltmp9:
	.size	arienco_delete_encoding_environment, .Ltmp9-arienco_delete_encoding_environment
	.cfi_endproc

	.globl	arienco_start_encoding
	.align	16, 0x90
	.type	arienco_start_encoding,@function
arienco_start_encoding:                 # @arienco_start_encoding
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movl	%eax, (%rdi)
	movl	%eax, 16(%rdi)
	movl	%eax, 8(%rdi)
	movq	$9, %rcx
	movl	%ecx, 12(%rdi)
	movq	%rsi, 24(%rdi)
	movq	%rdx, 32(%rdi)
	movq	$510, %rcx              # imm = 0x1FE
	movl	%ecx, 4(%rdi)
	movl	%eax, 80(%rdi)
	movl	(%rdx), %ecx
	movl	%ecx, 96(%rdi)
	movl	%eax, 88(%rdi)
	retq
.Ltmp10:
	.size	arienco_start_encoding, .Ltmp10-arienco_start_encoding
	.cfi_endproc

	.globl	arienco_bits_written
	.align	16, 0x90
	.type	arienco_bits_written,@function
arienco_bits_written:                   # @arienco_bits_written
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rax
	movl	(%rax), %eax
	movl	16(%rdi), %ecx
	movl	12(%rdi), %edx
	leaq	8(%rcx,%rax,8), %rax
	subq	%rdx, %rax
	retq
.Ltmp11:
	.size	arienco_bits_written, .Ltmp11-arienco_bits_written
	.cfi_endproc

	.globl	arienco_done_encoding
	.align	16, 0x90
	.type	arienco_done_encoding,@function
arienco_done_encoding:                  # @arienco_done_encoding
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 24
.Ltmp17:
	.cfi_offset %rbx, -24
.Ltmp18:
	.cfi_offset %r14, -16
	movl	(%rdi), %esi
	movl	8(%rdi), %eax
	addq	%rax, %rax
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	%rax, %rsi
	movl	%esi, 8(%rdi)
	movl	12(%rdi), %r10d
	decq	%r10
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	testq	%r11, %r10
	movl	%r10d, 12(%rdi)
	jne	.LBB4_1
# BB#9:                                 # %if.then
	movq	32(%rdi), %rax
	movslq	(%rax), %rcx
	leaq	1(%rcx), %rdx
	movl	%edx, (%rax)
	movq	24(%rdi), %rax
	movb	%sil, (%rax,%rcx)
	movq	$8, %r10
	movl	%r10d, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB4_1
# BB#10:                                # %while.body.lr.ph
	movl	88(%rdi), %r8d
	addq	$-8, %rax
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	andq	%rax, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$3, %rcx
	leaq	1(%r8,%rsi), %r8
	shlq	%cl, %rsi
	subq	%rsi, %rax
	movl	%eax, 80(%rdi)
	movl	%r8d, 88(%rdi)
.LBB4_1:                                # %while.cond8.preheader
	movl	16(%rdi), %esi
	testq	%rsi, %rsi
	je	.LBB4_5
# BB#2:
	movq	$8, %r9
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB4_3:                                # %while.body11
                                        # =>This Inner Loop Header: Depth=1
	decq	%rsi
	movl	%esi, 16(%rdi)
	movl	8(%rdi), %edx
	addq	%rdx, %rdx
	movl	(%rdi), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rdx, %rax
	xorq	$1, %rax
	movl	%eax, 8(%rdi)
	decq	%r10
	testq	%r11, %r10
	movl	%r10d, 12(%rdi)
	jne	.LBB4_4
# BB#11:                                # %if.then25
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rdx)
	movl	%r9d, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB4_13
# BB#12:                                # %while.body38.lr.ph
                                        #   in Loop: Header=BB4_3 Depth=1
	movl	88(%rdi), %edx
	addq	$-8, %rax
	movq	%rax, %rsi
	andq	%r8, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$3, %rcx
	leaq	1(%rdx,%rsi), %rdx
	shlq	%cl, %rsi
	subq	%rsi, %rax
	movl	%eax, 80(%rdi)
	movl	%edx, 88(%rdi)
.LBB4_13:                               # %while.cond8.loopexit
                                        #   in Loop: Header=BB4_3 Depth=1
	movl	16(%rdi), %esi
	movq	$8, %r10
.LBB4_4:                                # %while.cond8.backedge
                                        #   in Loop: Header=BB4_3 Depth=1
	testq	%r11, %rsi
	jne	.LBB4_3
.LBB4_5:                                # %while.end45
	movl	8(%rdi), %edx
	addq	%rdx, %rdx
	movl	(%rdi), %eax
	movq	$8, %r8
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	orq	%rdx, %rax
	movl	%eax, 8(%rdi)
	leaq	-1(%r10), %rcx
	testq	%r11, %rcx
	movl	%ecx, 12(%rdi)
	je	.LBB4_6
# BB#14:                                # %if.end76
	leaq	1(%rax,%rax), %rax
	movl	%eax, 8(%rdi)
	addq	$-2, %r10
	testq	%r11, %r10
	movl	%r10d, 12(%rdi)
	jne	.LBB4_15
# BB#16:                                # %if.then85
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rdx)
	movq	$8, %r10
	movl	%r10d, 12(%rdi)
	movslq	80(%rdi), %rax
	leaq	32(%rdi), %r8
	leaq	24(%rdi), %r9
	leaq	80(%rdi), %r14
	cmpq	$8, %rax
	jl	.LBB4_18
# BB#17:                                # %while.body98.lr.ph
	movl	88(%rdi), %edx
	addq	$-8, %rax
	movabsq	$4294967288, %rbx       # imm = 0xFFFFFFF8
	andq	%rax, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%rdx,%rbx), %rdx
	shlq	%cl, %rbx
	subq	%rbx, %rax
	movl	%eax, 80(%rdi)
	movl	%edx, 88(%rdi)
	jmp	.LBB4_18
.LBB4_6:                                # %if.then57
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rdx)
	movl	%r8d, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB4_8
# BB#7:                                 # %while.body70.lr.ph
	movl	88(%rdi), %edx
	addq	$-8, %rax
	movabsq	$4294967288, %rsi       # imm = 0xFFFFFFF8
	andq	%rax, %rsi
	movq	$3, %rcx
	shrq	%cl, %rsi
	movq	$3, %rcx
	leaq	1(%rdx,%rsi), %rdx
	shlq	%cl, %rsi
	subq	%rsi, %rax
	movl	%eax, 80(%rdi)
	movl	%edx, 88(%rdi)
.LBB4_8:                                # %if.end76.thread
	movl	8(%rdi), %eax
	leaq	1(%rax,%rax), %rax
	movl	%eax, 8(%rdi)
	movq	$7, %r10
	movl	%r10d, 12(%rdi)
.LBB4_15:                               # %if.end76.if.end104_crit_edge
	leaq	32(%rdi), %r8
	leaq	24(%rdi), %r9
	leaq	80(%rdi), %r14
.LBB4_18:                               # %if.end104
	movq	img(%rip), %rax
	movslq	24(%rax), %rax
	movq	stats(%rip), %rcx
	movl	1328(%rcx,%rax,4), %edx
	movq	$8, %rbx
	subq	%r10, %rbx
	addq	%rbx, %rdx
	movl	%edx, 1328(%rcx,%rax,4)
	movl	12(%rdi), %edx
	movabsq	$4294967288, %r10       # imm = 0xFFFFFFF8
	jmp	.LBB4_19
	.align	16, 0x90
.LBB4_22:                               # %while.body134.lr.ph
                                        #   in Loop: Header=BB4_19 Depth=1
	movl	88(%rdi), %ebx
	addq	$-8, %rsi
	movq	%rsi, %rax
	andq	%r10, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	$3, %rcx
	leaq	1(%rbx,%rax), %rbx
	shlq	%cl, %rax
	subq	%rax, %rsi
	movl	%esi, (%r14)
	movl	%ebx, 88(%rdi)
.LBB4_19:                               # %while.cond109
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rax
	andq	%r11, %rax
	cmpq	$8, %rax
	je	.LBB4_23
# BB#20:                                # %while.body113
                                        #   in Loop: Header=BB4_19 Depth=1
	movl	8(%rdi), %eax
	addq	%rax, %rax
	movl	%eax, 8(%rdi)
	decq	%rdx
	testq	%r11, %rdx
	movl	%edx, 12(%rdi)
	jne	.LBB4_19
# BB#21:                                # %if.then121
                                        #   in Loop: Header=BB4_19 Depth=1
	movq	(%r8), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rbx
	movl	%ebx, (%rcx)
	movq	(%r9), %rcx
	movb	%al, (%rcx,%rdx)
	movq	$8, %rdx
	movl	%edx, 12(%rdi)
	movslq	(%r14), %rsi
	cmpq	$8, %rsi
	jl	.LBB4_19
	jmp	.LBB4_22
.LBB4_23:                               # %while.end141
	movl	88(%rdi), %eax
	movl	(%r14), %ecx
	leaq	(%rcx,%rax,8), %rax
	movl	%eax, 88(%rdi)
	movq	(%r8), %rcx
	movl	(%rcx), %ecx
	movl	96(%rdi), %edx
	subq	%rdx, %rcx
	movl	%ecx, 96(%rdi)
	movq	img(%rip), %rcx
	movq	14160(%rcx), %rdx
	movl	12(%rcx), %ecx
	movl	12(%rdx), %edx
	movq	$31, %rsi
	subq	%rcx, %rsi
	addq	%rdx, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$5, %rcx
	shrq	%cl, %rsi
	movl	%esi, 88(%rdi)
	popq	%rbx
	popq	%r14
	retq
.Ltmp19:
	.size	arienco_done_encoding, .Ltmp19-arienco_done_encoding
	.cfi_endproc

	.globl	biari_encode_symbol
	.align	16, 0x90
	.type	biari_encode_symbol,@function
biari_encode_symbol:                    # @biari_encode_symbol
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 32
.Ltmp27:
	.cfi_offset %rbx, -32
.Ltmp28:
	.cfi_offset %r14, -24
.Ltmp29:
	.cfi_offset %r15, -16
	movl	4(%rdi), %r8d
	movl	(%rdi), %r14d
	movq	$6, %rcx
	movq	%r8, %rbx
	movl	cabac_encoding(%rip), %r10d
	movzbl	(%rdx), %r9d
	shrq	%cl, %rbx
	andq	$3, %rbx
	testq	%r10, %r10
	je	.LBB5_2
# BB#1:                                 # %if.then
	incq	8(%rdx)
.LBB5_2:                                # %if.end
	movzbl	rLPS_table_64x4(%rbx,%r9,4), %ecx
	testq	$65535, %rsi            # imm = 0xFFFF
	je	.LBB5_3
# BB#4:                                 # %if.end
	movabsq	$1, %rbx
	jmp	.LBB5_5
.LBB5_3:
	movabsq	$0, %rbx
.LBB5_5:                                # %if.end
	movzbl	2(%rdx), %esi
	subq	%rcx, %r8
	cmpq	%rsi, %rbx
	jne	.LBB5_6
# BB#9:                                 # %while.cond.preheader
	movzbl	AC_next_state_MPS_64(%r9,%r9), %ecx
	movw	%cx, (%rdx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r8, %rcx
	cmpq	$256, %rcx              # imm = 0x100
	movq	%r8, %rcx
	jae	.LBB5_34
	jmp	.LBB5_10
.LBB5_6:                                # %if.then11
	testq	%r9, %r9
	jne	.LBB5_8
# BB#7:                                 # %if.then14
	xorq	$1, %rsi
	movb	%sil, 2(%rdx)
.LBB5_8:                                # %while.cond.preheader.thread
	movzbl	AC_next_state_LPS_64(%r9,%r9), %esi
	addq	%r8, %r14
	movw	%si, (%rdx)
.LBB5_10:                               # %while.body.lr.ph
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	movq	$8, %r11
	movabsq	$4294967294, %r10       # imm = 0xFFFFFFFE
	movq	%rcx, %r8
	.align	16, 0x90
.LBB5_11:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_25 Depth 2
                                        #     Child Loop BB5_14 Depth 2
	movq	%r14, %rcx
	andq	%rdx, %rcx
	cmpq	$512, %rcx              # imm = 0x200
	jb	.LBB5_22
# BB#12:                                # %if.then33
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	8(%rdi), %ecx
	leaq	1(%rcx,%rcx), %rcx
	movl	%ecx, 8(%rdi)
	movl	12(%rdi), %esi
	decq	%rsi
	testq	%rdx, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB5_13
# BB#17:                                # %if.then37
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	32(%rdi), %rsi
	movslq	(%rsi), %rbx
	leaq	1(%rbx), %rax
	movl	%eax, (%rsi)
	movq	24(%rdi), %rax
	movb	%cl, (%rax,%rbx)
	movq	$8, %rsi
	movl	%esi, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB5_13
# BB#18:                                # %while.body47.lr.ph
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	88(%rdi), %r15d
	addq	$-8, %rax
	movq	%rax, %rbx
	andq	%r9, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%r15,%rbx), %r15
	shlq	%cl, %rbx
	subq	%rbx, %rax
	movl	%eax, 80(%rdi)
	movl	%r15d, 88(%rdi)
.LBB5_13:                               # %while.cond52.preheader
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB5_16
	.align	16, 0x90
.LBB5_14:                               # %while.body55
                                        #   Parent Loop BB5_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %ebx
	addq	%rbx, %rbx
	movl	%ebx, 8(%rdi)
	decq	%rsi
	testq	%rdx, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB5_15
# BB#19:                                # %if.then65
                                        #   in Loop: Header=BB5_14 Depth=2
	movq	32(%rdi), %rax
	movslq	(%rax), %rcx
	leaq	1(%rcx), %rsi
	movl	%esi, (%rax)
	movq	24(%rdi), %rax
	movb	%bl, (%rax,%rcx)
	movl	%r11d, 12(%rdi)
	movslq	80(%rdi), %rsi
	cmpq	$8, %rsi
	jl	.LBB5_21
# BB#20:                                # %while.body78.lr.ph
                                        #   in Loop: Header=BB5_14 Depth=2
	movl	88(%rdi), %eax
	addq	$-8, %rsi
	movq	%rsi, %rbx
	andq	%r9, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%rax,%rbx), %rax
	shlq	%cl, %rbx
	subq	%rbx, %rsi
	movl	%esi, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB5_21:                               # %while.cond52.loopexit
                                        #   in Loop: Header=BB5_14 Depth=2
	movl	16(%rdi), %ecx
	movq	$8, %rsi
.LBB5_15:                               # %while.cond52.backedge
                                        #   in Loop: Header=BB5_14 Depth=2
	testq	%rdx, %rcx
	jne	.LBB5_14
.LBB5_16:                               # %while.end85
                                        #   in Loop: Header=BB5_11 Depth=1
	addq	$-512, %r14             # imm = 0xFFFFFFFFFFFFFE00
	jmp	.LBB5_33
	.align	16, 0x90
.LBB5_22:                               # %if.else87
                                        #   in Loop: Header=BB5_11 Depth=1
	cmpq	$255, %rcx
	ja	.LBB5_32
# BB#23:                                # %if.then90
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	8(%rdi), %ecx
	addq	%rcx, %rcx
	movl	%ecx, 8(%rdi)
	movl	12(%rdi), %esi
	decq	%rsi
	testq	%rdx, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB5_24
# BB#27:                                # %if.then98
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	32(%rdi), %rax
	movslq	(%rax), %rsi
	leaq	1(%rsi), %rbx
	movl	%ebx, (%rax)
	movq	24(%rdi), %rax
	movb	%cl, (%rax,%rsi)
	movq	$8, %rsi
	movl	%esi, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB5_24
# BB#28:                                # %while.body111.lr.ph
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	88(%rdi), %r15d
	addq	$-8, %rax
	movq	%rax, %rbx
	andq	%r9, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%r15,%rbx), %r15
	shlq	%cl, %rbx
	subq	%rbx, %rax
	movl	%eax, 80(%rdi)
	movl	%r15d, 88(%rdi)
.LBB5_24:                               # %while.cond118.preheader
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB5_33
	.align	16, 0x90
.LBB5_25:                               # %while.body122
                                        #   Parent Loop BB5_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %eax
	leaq	1(%rax,%rax), %rbx
	movl	%ebx, 8(%rdi)
	decq	%rsi
	testq	%rdx, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB5_26
# BB#29:                                # %if.then133
                                        #   in Loop: Header=BB5_25 Depth=2
	movq	32(%rdi), %rax
	movslq	(%rax), %rcx
	leaq	1(%rcx), %rsi
	movl	%esi, (%rax)
	movq	24(%rdi), %rax
	movb	%bl, (%rax,%rcx)
	movl	%r11d, 12(%rdi)
	movslq	80(%rdi), %rsi
	cmpq	$8, %rsi
	jl	.LBB5_31
# BB#30:                                # %while.body146.lr.ph
                                        #   in Loop: Header=BB5_25 Depth=2
	movl	88(%rdi), %eax
	addq	$-8, %rsi
	movq	%rsi, %rbx
	andq	%r9, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%rax,%rbx), %rax
	shlq	%cl, %rbx
	subq	%rbx, %rsi
	movl	%esi, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB5_31:                               # %while.cond118.loopexit
                                        #   in Loop: Header=BB5_25 Depth=2
	movl	16(%rdi), %ecx
	movq	$8, %rsi
.LBB5_26:                               # %while.cond118.backedge
                                        #   in Loop: Header=BB5_25 Depth=2
	testq	%rdx, %rcx
	jne	.LBB5_25
	jmp	.LBB5_33
	.align	16, 0x90
.LBB5_32:                               # %if.else154
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	16(%rdi), %eax
	incq	%rax
	movl	%eax, 16(%rdi)
	addq	$-256, %r14
.LBB5_33:                               # %if.end159
                                        #   in Loop: Header=BB5_11 Depth=1
	addq	%r8, %r8
	addq	%r14, %r14
	movq	%r8, %rax
	andq	%r10, %rax
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB5_11
.LBB5_34:                               # %while.end162
	movl	%r8d, 4(%rdi)
	movl	%r14d, (%rdi)
	movl	80(%rdi), %eax
	incq	%rax
	movl	%eax, 80(%rdi)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp30:
	.size	biari_encode_symbol, .Ltmp30-biari_encode_symbol
	.cfi_endproc

	.globl	biari_encode_symbol_eq_prob
	.align	16, 0x90
	.type	biari_encode_symbol_eq_prob,@function
biari_encode_symbol_eq_prob:            # @biari_encode_symbol_eq_prob
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %r8d
	addq	%r8, %r8
	testq	$65535, %rsi            # imm = 0xFFFF
	je	.LBB6_2
# BB#1:                                 # %if.then
	movl	4(%rdi), %eax
	addq	%rax, %r8
.LBB6_2:                                # %if.end
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r8, %rax
	andq	%r11, %rax
	cmpq	$1024, %rax             # imm = 0x400
	jb	.LBB6_14
# BB#3:                                 # %if.then4
	movl	8(%rdi), %eax
	leaq	1(%rax,%rax), %rax
	movl	%eax, 8(%rdi)
	movl	12(%rdi), %esi
	decq	%rsi
	testq	%r11, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB6_4
# BB#9:                                 # %if.then9
	movq	32(%rdi), %rcx
	movslq	(%rcx), %r9
	leaq	1(%r9), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%r9)
	movq	$8, %rsi
	movl	%esi, 12(%rdi)
	movslq	80(%rdi), %r10
	cmpq	$8, %r10
	jl	.LBB6_4
# BB#10:                                # %while.body.lr.ph
	movl	88(%rdi), %r9d
	addq	$-8, %r10
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r10, %rax
	movq	$3, %rcx
	shrq	%cl, %rax
	movq	$3, %rcx
	leaq	1(%r9,%rax), %r9
	shlq	%cl, %rax
	subq	%rax, %r10
	movl	%r10d, 80(%rdi)
	movl	%r9d, 88(%rdi)
.LBB6_4:                                # %while.cond18.preheader
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB6_8
# BB#5:
	movq	$8, %r10
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB6_6:                                # %while.body21
                                        # =>This Inner Loop Header: Depth=1
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %eax
	addq	%rax, %rax
	movl	%eax, 8(%rdi)
	decq	%rsi
	testq	%r11, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB6_7
# BB#11:                                # %if.then31
                                        #   in Loop: Header=BB6_6 Depth=1
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rsi
	leaq	1(%rsi), %rdx
	movl	%edx, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rsi)
	movl	%r10d, 12(%rdi)
	movslq	80(%rdi), %rsi
	cmpq	$8, %rsi
	jl	.LBB6_13
# BB#12:                                # %while.body44.lr.ph
                                        #   in Loop: Header=BB6_6 Depth=1
	movl	88(%rdi), %eax
	addq	$-8, %rsi
	movq	%rsi, %rdx
	andq	%r9, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	$3, %rcx
	leaq	1(%rax,%rdx), %rax
	shlq	%cl, %rdx
	subq	%rdx, %rsi
	movl	%esi, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB6_13:                               # %while.cond18.loopexit
                                        #   in Loop: Header=BB6_6 Depth=1
	movl	16(%rdi), %ecx
	movq	$8, %rsi
.LBB6_7:                                # %while.cond18.backedge
                                        #   in Loop: Header=BB6_6 Depth=1
	testq	%r11, %rcx
	jne	.LBB6_6
.LBB6_8:                                # %while.end51
	addq	$-1024, %r8             # imm = 0xFFFFFFFFFFFFFC00
	jmp	.LBB6_26
.LBB6_14:                               # %if.else
	cmpq	$511, %rax              # imm = 0x1FF
	ja	.LBB6_25
# BB#15:                                # %if.then55
	movl	8(%rdi), %eax
	addq	%rax, %rax
	movl	%eax, 8(%rdi)
	movl	12(%rdi), %esi
	decq	%rsi
	testq	%r11, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB6_16
# BB#20:                                # %if.then63
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rdx)
	movq	$8, %rsi
	movl	%esi, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB6_16
# BB#21:                                # %while.body76.lr.ph
	movl	88(%rdi), %r9d
	addq	$-8, %rax
	movabsq	$4294967288, %rdx       # imm = 0xFFFFFFF8
	andq	%rax, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	$3, %rcx
	leaq	1(%r9,%rdx), %r9
	shlq	%cl, %rdx
	subq	%rdx, %rax
	movl	%eax, 80(%rdi)
	movl	%r9d, 88(%rdi)
.LBB6_16:                               # %while.cond83.preheader
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB6_26
# BB#17:
	movq	$8, %r10
	movabsq	$4294967288, %r9        # imm = 0xFFFFFFF8
	.align	16, 0x90
.LBB6_18:                               # %while.body87
                                        # =>This Inner Loop Header: Depth=1
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %eax
	leaq	1(%rax,%rax), %rax
	movl	%eax, 8(%rdi)
	decq	%rsi
	testq	%r11, %rsi
	movl	%esi, 12(%rdi)
	jne	.LBB6_19
# BB#22:                                # %if.then98
                                        #   in Loop: Header=BB6_18 Depth=1
	movq	32(%rdi), %rcx
	movslq	(%rcx), %rdx
	leaq	1(%rdx), %rsi
	movl	%esi, (%rcx)
	movq	24(%rdi), %rcx
	movb	%al, (%rcx,%rdx)
	movl	%r10d, 12(%rdi)
	movslq	80(%rdi), %rsi
	cmpq	$8, %rsi
	jl	.LBB6_24
# BB#23:                                # %while.body111.lr.ph
                                        #   in Loop: Header=BB6_18 Depth=1
	movl	88(%rdi), %eax
	addq	$-8, %rsi
	movq	%rsi, %rdx
	andq	%r9, %rdx
	movq	$3, %rcx
	shrq	%cl, %rdx
	movq	$3, %rcx
	leaq	1(%rax,%rdx), %rax
	shlq	%cl, %rdx
	subq	%rdx, %rsi
	movl	%esi, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB6_24:                               # %while.cond83.loopexit
                                        #   in Loop: Header=BB6_18 Depth=1
	movl	16(%rdi), %ecx
	movq	$8, %rsi
.LBB6_19:                               # %while.cond83.backedge
                                        #   in Loop: Header=BB6_18 Depth=1
	testq	%r11, %rcx
	jne	.LBB6_18
	jmp	.LBB6_26
.LBB6_25:                               # %if.else119
	movl	16(%rdi), %eax
	incq	%rax
	movl	%eax, 16(%rdi)
	addq	$-512, %r8              # imm = 0xFFFFFFFFFFFFFE00
.LBB6_26:                               # %if.end124
	movl	%r8d, (%rdi)
	movl	80(%rdi), %eax
	incq	%rax
	movl	%eax, 80(%rdi)
	retq
.Ltmp31:
	.size	biari_encode_symbol_eq_prob, .Ltmp31-biari_encode_symbol_eq_prob
	.cfi_endproc

	.globl	biari_encode_symbol_final
	.align	16, 0x90
	.type	biari_encode_symbol_final,@function
biari_encode_symbol_final:              # @biari_encode_symbol_final
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
	movl	4(%rdi), %ecx
	movl	(%rdi), %edx
	addq	$-2, %rcx
	andq	$65535, %rsi            # imm = 0xFFFF
	je	.LBB7_1
# BB#2:                                 # %entry
	movq	%rcx, %r15
	jmp	.LBB7_3
.LBB7_1:
	xorq	%r15, %r15
.LBB7_3:                                # %entry
	testq	%rsi, %rsi
	movq	%rcx, %r11
	je	.LBB7_5
# BB#4:                                 # %entry
	movq	$2, %r11
.LBB7_5:                                # %entry
	addq	%rdx, %r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r11, %rdx
	andq	%rsi, %rdx
	cmpq	$256, %rdx              # imm = 0x100
	jae	.LBB7_6
# BB#7:                                 # %while.body.lr.ph
	movabsq	$4294967288, %r8        # imm = 0xFFFFFFF8
	movq	$8, %r9
	movabsq	$4294967294, %r10       # imm = 0xFFFFFFFE
	.align	16, 0x90
.LBB7_8:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_22 Depth 2
                                        #     Child Loop BB7_11 Depth 2
	movq	%r15, %rcx
	andq	%rsi, %rcx
	cmpq	$512, %rcx              # imm = 0x200
	jb	.LBB7_19
# BB#9:                                 # %if.then2
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	8(%rdi), %ecx
	leaq	1(%rcx,%rcx), %rcx
	movl	%ecx, 8(%rdi)
	movl	12(%rdi), %edx
	decq	%rdx
	testq	%rsi, %rdx
	movl	%edx, 12(%rdi)
	jne	.LBB7_10
# BB#14:                                # %if.then5
                                        #   in Loop: Header=BB7_8 Depth=1
	movq	32(%rdi), %rdx
	movslq	(%rdx), %r14
	leaq	1(%r14), %rbx
	movl	%ebx, (%rdx)
	movq	24(%rdi), %rdx
	movb	%cl, (%rdx,%r14)
	movq	$8, %rdx
	movl	%edx, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB7_10
# BB#15:                                # %while.body11.lr.ph
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	88(%rdi), %r14d
	addq	$-8, %rax
	movq	%rax, %rbx
	andq	%r8, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%r14,%rbx), %r14
	shlq	%cl, %rbx
	subq	%rbx, %rax
	movl	%eax, 80(%rdi)
	movl	%r14d, 88(%rdi)
.LBB7_10:                               # %while.cond16.preheader
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB7_13
	.align	16, 0x90
.LBB7_11:                               # %while.body19
                                        #   Parent Loop BB7_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %ebx
	addq	%rbx, %rbx
	movl	%ebx, 8(%rdi)
	decq	%rdx
	testq	%rsi, %rdx
	movl	%edx, 12(%rdi)
	jne	.LBB7_12
# BB#16:                                # %if.then29
                                        #   in Loop: Header=BB7_11 Depth=2
	movq	32(%rdi), %rax
	movslq	(%rax), %rcx
	leaq	1(%rcx), %rdx
	movl	%edx, (%rax)
	movq	24(%rdi), %rax
	movb	%bl, (%rax,%rcx)
	movl	%r9d, 12(%rdi)
	movslq	80(%rdi), %rdx
	cmpq	$8, %rdx
	jl	.LBB7_18
# BB#17:                                # %while.body42.lr.ph
                                        #   in Loop: Header=BB7_11 Depth=2
	movl	88(%rdi), %eax
	addq	$-8, %rdx
	movq	%rdx, %rbx
	andq	%r8, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%rax,%rbx), %rax
	shlq	%cl, %rbx
	subq	%rbx, %rdx
	movl	%edx, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB7_18:                               # %while.cond16.loopexit
                                        #   in Loop: Header=BB7_11 Depth=2
	movl	16(%rdi), %ecx
	movq	$8, %rdx
.LBB7_12:                               # %while.cond16.backedge
                                        #   in Loop: Header=BB7_11 Depth=2
	testq	%rsi, %rcx
	jne	.LBB7_11
.LBB7_13:                               # %while.end49
                                        #   in Loop: Header=BB7_8 Depth=1
	addq	$-512, %r15             # imm = 0xFFFFFFFFFFFFFE00
	jmp	.LBB7_30
	.align	16, 0x90
.LBB7_19:                               # %if.else
                                        #   in Loop: Header=BB7_8 Depth=1
	cmpq	$255, %rcx
	ja	.LBB7_29
# BB#20:                                # %if.then53
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	8(%rdi), %ecx
	addq	%rcx, %rcx
	movl	%ecx, 8(%rdi)
	movl	12(%rdi), %edx
	decq	%rdx
	testq	%rsi, %rdx
	movl	%edx, 12(%rdi)
	jne	.LBB7_21
# BB#24:                                # %if.then61
                                        #   in Loop: Header=BB7_8 Depth=1
	movq	32(%rdi), %rax
	movslq	(%rax), %rdx
	leaq	1(%rdx), %rbx
	movl	%ebx, (%rax)
	movq	24(%rdi), %rax
	movb	%cl, (%rax,%rdx)
	movq	$8, %rdx
	movl	%edx, 12(%rdi)
	movslq	80(%rdi), %rax
	cmpq	$8, %rax
	jl	.LBB7_21
# BB#25:                                # %while.body74.lr.ph
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	88(%rdi), %r14d
	addq	$-8, %rax
	movq	%rax, %rbx
	andq	%r8, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%r14,%rbx), %r14
	shlq	%cl, %rbx
	subq	%rbx, %rax
	movl	%eax, 80(%rdi)
	movl	%r14d, 88(%rdi)
.LBB7_21:                               # %while.cond81.preheader
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	16(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB7_30
	.align	16, 0x90
.LBB7_22:                               # %while.body85
                                        #   Parent Loop BB7_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rcx
	movl	%ecx, 16(%rdi)
	movl	8(%rdi), %eax
	leaq	1(%rax,%rax), %rbx
	movl	%ebx, 8(%rdi)
	decq	%rdx
	testq	%rsi, %rdx
	movl	%edx, 12(%rdi)
	jne	.LBB7_23
# BB#26:                                # %if.then96
                                        #   in Loop: Header=BB7_22 Depth=2
	movq	32(%rdi), %rax
	movslq	(%rax), %rcx
	leaq	1(%rcx), %rdx
	movl	%edx, (%rax)
	movq	24(%rdi), %rax
	movb	%bl, (%rax,%rcx)
	movl	%r9d, 12(%rdi)
	movslq	80(%rdi), %rdx
	cmpq	$8, %rdx
	jl	.LBB7_28
# BB#27:                                # %while.body109.lr.ph
                                        #   in Loop: Header=BB7_22 Depth=2
	movl	88(%rdi), %eax
	addq	$-8, %rdx
	movq	%rdx, %rbx
	andq	%r8, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rcx
	leaq	1(%rax,%rbx), %rax
	shlq	%cl, %rbx
	subq	%rbx, %rdx
	movl	%edx, 80(%rdi)
	movl	%eax, 88(%rdi)
.LBB7_28:                               # %while.cond81.loopexit
                                        #   in Loop: Header=BB7_22 Depth=2
	movl	16(%rdi), %ecx
	movq	$8, %rdx
.LBB7_23:                               # %while.cond81.backedge
                                        #   in Loop: Header=BB7_22 Depth=2
	testq	%rsi, %rcx
	jne	.LBB7_22
	jmp	.LBB7_30
	.align	16, 0x90
.LBB7_29:                               # %if.else117
                                        #   in Loop: Header=BB7_8 Depth=1
	movl	16(%rdi), %eax
	incq	%rax
	movl	%eax, 16(%rdi)
	addq	$-256, %r15
.LBB7_30:                               # %if.end122
                                        #   in Loop: Header=BB7_8 Depth=1
	addq	%r11, %r11
	addq	%r15, %r15
	movq	%r11, %rax
	andq	%r10, %rax
	cmpq	$256, %rax              # imm = 0x100
	jb	.LBB7_8
	jmp	.LBB7_31
.LBB7_6:
	movq	%rcx, %r11
.LBB7_31:                               # %while.end125
	movl	%r11d, 4(%rdi)
	movl	%r15d, (%rdi)
	movl	80(%rdi), %eax
	incq	%rax
	movl	%eax, 80(%rdi)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp42:
	.size	biari_encode_symbol_final, .Ltmp42-biari_encode_symbol_final
	.cfi_endproc

	.globl	biari_init_context
	.align	16, 0x90
	.type	biari_init_context,@function
biari_init_context:                     # @biari_init_context
	.cfi_startproc
# BB#0:                                 # %entry
	movq	img(%rip), %rax
	movslq	40(%rax), %rdx
	movl	(%rsi), %ecx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	js	.LBB8_2
# BB#1:                                 # %entry
	movq	%rdx, %rax
.LBB8_2:                                # %entry
	imulq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movl	4(%rsi), %edx
	movq	$36, %rcx
	sarq	%cl, %rax
	addq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %rax
	jle	.LBB8_4
# BB#3:                                 # %entry
	movq	%rdx, %rax
.LBB8_4:                                # %entry
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$127, %rdx
	jl	.LBB8_6
# BB#5:                                 # %entry
	movq	$126, %rax
.LBB8_6:                                # %entry
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$64, %rdx
	jl	.LBB8_8
# BB#7:                                 # %if.then
	addq	$65472, %rax            # imm = 0xFFC0
	movw	%ax, (%rdi)
	movq	$1, %rax
	jmp	.LBB8_9
.LBB8_8:                                # %if.else
	movq	$63, %rcx
	subq	%rax, %rcx
	movw	%cx, (%rdi)
	xorq	%rax, %rax
.LBB8_9:                                # %if.end
	movb	%al, 2(%rdi)
	movq	$0, 8(%rdi)
	retq
.Ltmp43:
	.size	biari_init_context, .Ltmp43-biari_init_context
	.cfi_endproc

	.type	rLPS_table_64x4,@object # @rLPS_table_64x4
	.section	.rodata,"a",@progbits
	.globl	rLPS_table_64x4
	.align	16
rLPS_table_64x4:
	.ascii	"\200\260\320\360"
	.ascii	"\200\247\305\343"
	.ascii	"\200\236\273\330"
	.ascii	"{\226\262\315"
	.ascii	"t\216\251\303"
	.ascii	"o\207\240\271"
	.ascii	"i\200\230\257"
	.ascii	"dz\220\246"
	.ascii	"_t\211\236"
	.ascii	"Zn\202\226"
	.ascii	"Uh{\216"
	.ascii	"Qcu\207"
	.ascii	"M^o\200"
	.ascii	"IYiz"
	.ascii	"EUdt"
	.ascii	"BP_n"
	.ascii	">LZh"
	.ascii	";HVc"
	.ascii	"8EQ^"
	.ascii	"5AMY"
	.ascii	"3>IU"
	.ascii	"0;EP"
	.ascii	".8BL"
	.ascii	"+5?H"
	.ascii	")2;E"
	.ascii	"'08A"
	.ascii	"%-6>"
	.ascii	"#+3;"
	.ascii	"!)08"
	.ascii	" '.5"
	.ascii	"\036%+2"
	.ascii	"\035#)0"
	.ascii	"\033!'-"
	.ascii	"\032\037%+"
	.ascii	"\030\036#)"
	.ascii	"\027\034!'"
	.ascii	"\026\033 %"
	.ascii	"\025\032\036#"
	.ascii	"\024\030\035!"
	.ascii	"\023\027\033\037"
	.ascii	"\022\026\032\036"
	.ascii	"\021\025\031\034"
	.ascii	"\020\024\027\033"
	.ascii	"\017\023\026\031"
	.ascii	"\016\022\025\030"
	.ascii	"\016\021\024\027"
	.ascii	"\r\020\023\026"
	.ascii	"\f\017\022\025"
	.ascii	"\f\016\021\024"
	.ascii	"\013\016\020\023"
	.ascii	"\013\r\017\022"
	.ascii	"\n\f\017\021"
	.ascii	"\n\f\016\020"
	.ascii	"\t\013\r\017"
	.ascii	"\t\013\f\016"
	.ascii	"\b\n\f\016"
	.ascii	"\b\t\013\r"
	.ascii	"\007\t\013\f"
	.ascii	"\007\t\n\f"
	.ascii	"\007\b\n\013"
	.ascii	"\006\b\t\013"
	.ascii	"\006\007\t\n"
	.ascii	"\006\007\b\t"
	.zero	4,2
	.size	rLPS_table_64x4, 256

	.type	AC_next_state_MPS_64,@object # @AC_next_state_MPS_64
	.globl	AC_next_state_MPS_64
	.align	16
AC_next_state_MPS_64:
	.short	1                       # 0x1
	.short	2                       # 0x2
	.short	3                       # 0x3
	.short	4                       # 0x4
	.short	5                       # 0x5
	.short	6                       # 0x6
	.short	7                       # 0x7
	.short	8                       # 0x8
	.short	9                       # 0x9
	.short	10                      # 0xa
	.short	11                      # 0xb
	.short	12                      # 0xc
	.short	13                      # 0xd
	.short	14                      # 0xe
	.short	15                      # 0xf
	.short	16                      # 0x10
	.short	17                      # 0x11
	.short	18                      # 0x12
	.short	19                      # 0x13
	.short	20                      # 0x14
	.short	21                      # 0x15
	.short	22                      # 0x16
	.short	23                      # 0x17
	.short	24                      # 0x18
	.short	25                      # 0x19
	.short	26                      # 0x1a
	.short	27                      # 0x1b
	.short	28                      # 0x1c
	.short	29                      # 0x1d
	.short	30                      # 0x1e
	.short	31                      # 0x1f
	.short	32                      # 0x20
	.short	33                      # 0x21
	.short	34                      # 0x22
	.short	35                      # 0x23
	.short	36                      # 0x24
	.short	37                      # 0x25
	.short	38                      # 0x26
	.short	39                      # 0x27
	.short	40                      # 0x28
	.short	41                      # 0x29
	.short	42                      # 0x2a
	.short	43                      # 0x2b
	.short	44                      # 0x2c
	.short	45                      # 0x2d
	.short	46                      # 0x2e
	.short	47                      # 0x2f
	.short	48                      # 0x30
	.short	49                      # 0x31
	.short	50                      # 0x32
	.short	51                      # 0x33
	.short	52                      # 0x34
	.short	53                      # 0x35
	.short	54                      # 0x36
	.short	55                      # 0x37
	.short	56                      # 0x38
	.short	57                      # 0x39
	.short	58                      # 0x3a
	.short	59                      # 0x3b
	.short	60                      # 0x3c
	.short	61                      # 0x3d
	.short	62                      # 0x3e
	.short	62                      # 0x3e
	.short	63                      # 0x3f
	.size	AC_next_state_MPS_64, 128

	.type	AC_next_state_LPS_64,@object # @AC_next_state_LPS_64
	.globl	AC_next_state_LPS_64
	.align	16
AC_next_state_LPS_64:
	.short	0                       # 0x0
	.short	0                       # 0x0
	.short	1                       # 0x1
	.short	2                       # 0x2
	.short	2                       # 0x2
	.short	4                       # 0x4
	.short	4                       # 0x4
	.short	5                       # 0x5
	.short	6                       # 0x6
	.short	7                       # 0x7
	.short	8                       # 0x8
	.short	9                       # 0x9
	.short	9                       # 0x9
	.short	11                      # 0xb
	.short	11                      # 0xb
	.short	12                      # 0xc
	.short	13                      # 0xd
	.short	13                      # 0xd
	.short	15                      # 0xf
	.short	15                      # 0xf
	.short	16                      # 0x10
	.short	16                      # 0x10
	.short	18                      # 0x12
	.short	18                      # 0x12
	.short	19                      # 0x13
	.short	19                      # 0x13
	.short	21                      # 0x15
	.short	21                      # 0x15
	.short	22                      # 0x16
	.short	22                      # 0x16
	.short	23                      # 0x17
	.short	24                      # 0x18
	.short	24                      # 0x18
	.short	25                      # 0x19
	.short	26                      # 0x1a
	.short	26                      # 0x1a
	.short	27                      # 0x1b
	.short	27                      # 0x1b
	.short	28                      # 0x1c
	.short	29                      # 0x1d
	.short	29                      # 0x1d
	.short	30                      # 0x1e
	.short	30                      # 0x1e
	.short	30                      # 0x1e
	.short	31                      # 0x1f
	.short	32                      # 0x20
	.short	32                      # 0x20
	.short	33                      # 0x21
	.short	33                      # 0x21
	.short	33                      # 0x21
	.short	34                      # 0x22
	.short	34                      # 0x22
	.short	35                      # 0x23
	.short	35                      # 0x23
	.short	35                      # 0x23
	.short	36                      # 0x24
	.short	36                      # 0x24
	.short	36                      # 0x24
	.short	37                      # 0x25
	.short	37                      # 0x25
	.short	37                      # 0x25
	.short	38                      # 0x26
	.short	38                      # 0x26
	.short	63                      # 0x3f
	.size	AC_next_state_LPS_64, 128

	.type	binCount,@object        # @binCount
	.bss
	.globl	binCount
	.align	4
binCount:
	.long	0                       # 0x0
	.size	binCount, 4

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"arienco_create_encoding_environment: eep"
	.size	.L.str, 41

	.type	errortext,@object       # @errortext
	.comm	errortext,300,16
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Error freeing eep (NULL pointer)"
	.size	.L.str1, 33

	.type	top_pic,@object         # @top_pic
	.comm	top_pic,8,8
	.type	bottom_pic,@object      # @bottom_pic
	.comm	bottom_pic,8,8
	.type	frame_pic,@object       # @frame_pic
	.comm	frame_pic,8,8
	.type	frame_pic2,@object      # @frame_pic2
	.comm	frame_pic2,8,8
	.type	frame_pic3,@object      # @frame_pic3
	.comm	frame_pic3,8,8
	.type	imgY_org,@object        # @imgY_org
	.comm	imgY_org,8,8
	.type	imgUV_org,@object       # @imgUV_org
	.comm	imgUV_org,8,8
	.type	img4Y_tmp,@object       # @img4Y_tmp
	.comm	img4Y_tmp,8,8
	.type	log2_max_frame_num_minus4,@object # @log2_max_frame_num_minus4
	.comm	log2_max_frame_num_minus4,4,4
	.type	log2_max_pic_order_cnt_lsb_minus4,@object # @log2_max_pic_order_cnt_lsb_minus4
	.comm	log2_max_pic_order_cnt_lsb_minus4,4,4
	.type	me_tot_time,@object     # @me_tot_time
	.comm	me_tot_time,4,4
	.type	me_time,@object         # @me_time
	.comm	me_time,4,4
	.type	active_pps,@object      # @active_pps
	.comm	active_pps,8,8
	.type	active_sps,@object      # @active_sps
	.comm	active_sps,8,8
	.type	mb_adaptive,@object     # @mb_adaptive
	.comm	mb_adaptive,4,4
	.type	MBPairIsField,@object   # @MBPairIsField
	.comm	MBPairIsField,4,4
	.type	wp_weight,@object       # @wp_weight
	.comm	wp_weight,8,8
	.type	wp_offset,@object       # @wp_offset
	.comm	wp_offset,8,8
	.type	wbp_weight,@object      # @wbp_weight
	.comm	wbp_weight,8,8
	.type	luma_log_weight_denom,@object # @luma_log_weight_denom
	.comm	luma_log_weight_denom,4,4
	.type	chroma_log_weight_denom,@object # @chroma_log_weight_denom
	.comm	chroma_log_weight_denom,4,4
	.type	wp_luma_round,@object   # @wp_luma_round
	.comm	wp_luma_round,4,4
	.type	wp_chroma_round,@object # @wp_chroma_round
	.comm	wp_chroma_round,4,4
	.type	imgY_org_top,@object    # @imgY_org_top
	.comm	imgY_org_top,8,8
	.type	imgY_org_bot,@object    # @imgY_org_bot
	.comm	imgY_org_bot,8,8
	.type	imgUV_org_top,@object   # @imgUV_org_top
	.comm	imgUV_org_top,8,8
	.type	imgUV_org_bot,@object   # @imgUV_org_bot
	.comm	imgUV_org_bot,8,8
	.type	imgY_org_frm,@object    # @imgY_org_frm
	.comm	imgY_org_frm,8,8
	.type	imgUV_org_frm,@object   # @imgUV_org_frm
	.comm	imgUV_org_frm,8,8
	.type	imgY_com,@object        # @imgY_com
	.comm	imgY_com,8,8
	.type	imgUV_com,@object       # @imgUV_com
	.comm	imgUV_com,8,8
	.type	direct_ref_idx,@object  # @direct_ref_idx
	.comm	direct_ref_idx,8,8
	.type	direct_pdir,@object     # @direct_pdir
	.comm	direct_pdir,8,8
	.type	pixel_map,@object       # @pixel_map
	.comm	pixel_map,8,8
	.type	refresh_map,@object     # @refresh_map
	.comm	refresh_map,8,8
	.type	intras,@object          # @intras
	.comm	intras,4,4
	.type	Iframe_ctr,@object      # @Iframe_ctr
	.comm	Iframe_ctr,4,4
	.type	Pframe_ctr,@object      # @Pframe_ctr
	.comm	Pframe_ctr,4,4
	.type	Bframe_ctr,@object      # @Bframe_ctr
	.comm	Bframe_ctr,4,4
	.type	frame_no,@object        # @frame_no
	.comm	frame_no,4,4
	.type	nextP_tr_fld,@object    # @nextP_tr_fld
	.comm	nextP_tr_fld,4,4
	.type	nextP_tr_frm,@object    # @nextP_tr_frm
	.comm	nextP_tr_frm,4,4
	.type	tot_time,@object        # @tot_time
	.comm	tot_time,4,4
	.type	resTrans_R,@object      # @resTrans_R
	.comm	resTrans_R,1024,16
	.type	resTrans_G,@object      # @resTrans_G
	.comm	resTrans_G,1024,16
	.type	resTrans_B,@object      # @resTrans_B
	.comm	resTrans_B,1024,16
	.type	rec_resR,@object        # @rec_resR
	.comm	rec_resR,1024,16
	.type	rec_resG,@object        # @rec_resG
	.comm	rec_resG,1024,16
	.type	rec_resB,@object        # @rec_resB
	.comm	rec_resB,1024,16
	.type	mprRGB,@object          # @mprRGB
	.comm	mprRGB,3072,16
	.type	dc_level,@object        # @dc_level
	.comm	dc_level,128,16
	.type	dc_level_temp,@object   # @dc_level_temp
	.comm	dc_level_temp,128,16
	.type	cbp_chroma_block,@object # @cbp_chroma_block
	.comm	cbp_chroma_block,128,16
	.type	cbp_chroma_block_temp,@object # @cbp_chroma_block_temp
	.comm	cbp_chroma_block_temp,128,16
	.type	b8_ipredmode8x8,@object # @b8_ipredmode8x8
	.comm	b8_ipredmode8x8,64,16
	.type	b8_intra_pred_modes8x8,@object # @b8_intra_pred_modes8x8
	.comm	b8_intra_pred_modes8x8,64,16
	.type	gop_structure,@object   # @gop_structure
	.comm	gop_structure,8,8
	.type	rdopt,@object           # @rdopt
	.comm	rdopt,8,8
	.type	rddata_top_frame_mb,@object # @rddata_top_frame_mb
	.comm	rddata_top_frame_mb,3360,8
	.type	rddata_bot_frame_mb,@object # @rddata_bot_frame_mb
	.comm	rddata_bot_frame_mb,3360,8
	.type	rddata_top_field_mb,@object # @rddata_top_field_mb
	.comm	rddata_top_field_mb,3360,8
	.type	rddata_bot_field_mb,@object # @rddata_bot_field_mb
	.comm	rddata_bot_field_mb,3360,8
	.type	p_stat,@object          # @p_stat
	.comm	p_stat,8,8
	.type	p_log,@object           # @p_log
	.comm	p_log,8,8
	.type	p_trace,@object         # @p_trace
	.comm	p_trace,8,8
	.type	p_in,@object            # @p_in
	.comm	p_in,4,4
	.type	p_dec,@object           # @p_dec
	.comm	p_dec,4,4
	.type	glob_remapping_of_pic_nums_idc_l0,@object # @glob_remapping_of_pic_nums_idc_l0
	.comm	glob_remapping_of_pic_nums_idc_l0,80,16
	.type	glob_abs_diff_pic_num_minus1_l0,@object # @glob_abs_diff_pic_num_minus1_l0
	.comm	glob_abs_diff_pic_num_minus1_l0,80,16
	.type	glob_long_term_pic_idx_l0,@object # @glob_long_term_pic_idx_l0
	.comm	glob_long_term_pic_idx_l0,80,16
	.type	glob_remapping_of_pic_nums_idc_l1,@object # @glob_remapping_of_pic_nums_idc_l1
	.comm	glob_remapping_of_pic_nums_idc_l1,80,16
	.type	glob_abs_diff_pic_num_minus1_l1,@object # @glob_abs_diff_pic_num_minus1_l1
	.comm	glob_abs_diff_pic_num_minus1_l1,80,16
	.type	glob_long_term_pic_idx_l1,@object # @glob_long_term_pic_idx_l1
	.comm	glob_long_term_pic_idx_l1,80,16
	.type	Bytes_After_Header,@object # @Bytes_After_Header
	.comm	Bytes_After_Header,4,4
	.type	rpc_bytes_to_go,@object # @rpc_bytes_to_go
	.comm	rpc_bytes_to_go,4,4
	.type	rpc_bits_to_go,@object  # @rpc_bits_to_go
	.comm	rpc_bits_to_go,4,4

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
