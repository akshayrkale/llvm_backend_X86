	.file	"df.c.bc"
	.text
	.globl	df_ref_remove
	.align	16, 0x90
	.type	df_ref_remove,@function
df_ref_remove:                          # @df_ref_remove
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
	movq	%rsi, %rbx
	movl	32(%rbx), %eax
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
                                        # kill: RSI<def> RBX<kill>
	testq	%rax, %rax
	je	.LBB0_1
# BB#8:                                 # %if.else
	callq	df_use_unlink
	movq	8(%rbx), %rax
	movslq	8(%rax), %rax
	movq	56(%r14), %rdx
	leaq	(%rax,%rax,2), %rsi
	movq	8(%rdx,%rsi,8), %rax
	testq	%rax, %rax
	je	.LBB0_16
# BB#9:                                 # %if.then.i22
	movq	(%rax), %rcx
	movq	8(%rax), %rdi
	leaq	8(%rdx,%rsi,8), %rdx
	testq	%rcx, %rcx
	je	.LBB0_10
# BB#12:                                # %if.else.i26
	cmpq	%rbx, %rdi
	je	.LBB0_15
	.align	16, 0x90
.LBB0_13:                               # %for.cond.i31
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	%rcx, %rax
	testq	%rax, %rax
	je	.LBB0_16
# BB#14:                                # %for.body.i35
                                        #   in Loop: Header=BB0_13 Depth=1
	movq	8(%rax), %rsi
	movq	(%rax), %rcx
	cmpq	%rbx, %rsi
	jne	.LBB0_13
	jmp	.LBB0_15
.LBB0_1:                                # %if.then
	callq	df_def_unlink
	movq	8(%rbx), %rax
	movslq	8(%rax), %rax
	movq	56(%r14), %rdx
	leaq	(%rax,%rax,2), %rsi
	movq	(%rdx,%rsi,8), %rax
	testq	%rax, %rax
	je	.LBB0_16
# BB#2:                                 # %if.then.i
	movq	(%rax), %rcx
	movq	8(%rax), %rdi
	leaq	(%rdx,%rsi,8), %rdx
	testq	%rcx, %rcx
	je	.LBB0_3
# BB#5:                                 # %if.else.i
	cmpq	%rbx, %rdi
	je	.LBB0_15
	.align	16, 0x90
.LBB0_6:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movq	%rcx, %rax
	testq	%rax, %rax
	je	.LBB0_16
# BB#7:                                 # %for.body.i
                                        #   in Loop: Header=BB0_6 Depth=1
	movq	8(%rax), %rsi
	movq	(%rax), %rcx
	cmpq	%rbx, %rsi
	jne	.LBB0_6
.LBB0_15:                               # %if.then15.i36
	movq	%rcx, (%rdx)
	jmp	.LBB0_16
.LBB0_10:                               # %if.then2.i23
	cmpq	%rbx, %rdi
	jne	.LBB0_17
# BB#11:                                # %if.end.i25
	movq	$0, (%rdx)
	jmp	.LBB0_16
.LBB0_3:                                # %if.then2.i
	cmpq	%rbx, %rdi
	jne	.LBB0_17
# BB#4:                                 # %if.end.i
	movq	$0, (%rdx)
.LBB0_16:                               # %if.end
	movq	$1, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB0_17:                               # %if.then4.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_unlink, %rcx
	movq	$692, %rsi              # imm = 0x2B4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp8:
	.size	df_ref_remove, .Ltmp8-df_ref_remove
	.cfi_endproc

	.align	16, 0x90
	.type	df_def_unlink,@function
df_def_unlink:                          # @df_def_unlink
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp13:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 32
.Ltmp16:
	.cfi_offset %rbx, -32
.Ltmp17:
	.cfi_offset %r14, -24
.Ltmp18:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	(%r15), %rcx
	movzbl	(%rcx), %eax
	movq	%rdi, %r14
	cmpq	$63, %rax
	jne	.LBB1_2
# BB#1:                                 # %cond.true
	movq	8(%rcx), %rcx
.LBB1_2:                                # %cond.end
	movq	24(%r15), %rax
	movl	8(%rcx), %edi
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_13:                               # %df_ref_unlink.exit
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rax), %rax
.LBB1_3:                                # %cond.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_10 Depth 2
	testq	%rax, %rax
	je	.LBB1_14
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	8(%rax), %rsi
	movq	24(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB1_13
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rcx), %rdx
	movq	8(%rcx), %rbx
	testq	%rdx, %rdx
	jne	.LBB1_8
# BB#6:                                 # %if.then2.i
                                        #   in Loop: Header=BB1_3 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB1_15
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	$0, 24(%rsi)
	movq	(%rax), %rax
	jmp	.LBB1_3
.LBB1_8:                                # %if.else.i
                                        #   in Loop: Header=BB1_3 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB1_10
# BB#9:                                 # %if.then7.i
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	%rdx, 24(%rsi)
	movq	(%rax), %rax
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_10:                               # %for.cond.i
                                        #   Parent Loop BB1_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rsi
	movq	%rdx, %rcx
	testq	%rcx, %rcx
	je	.LBB1_13
# BB#11:                                # %for.body.i
                                        #   in Loop: Header=BB1_10 Depth=2
	movq	8(%rcx), %rbx
	movq	(%rcx), %rdx
	cmpq	%r15, %rbx
	jne	.LBB1_10
# BB#12:                                # %if.then15.i
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	%rdx, (%rsi)
	jmp	.LBB1_13
.LBB1_14:                               # %for.end
	movq	$0, 24(%r15)
	movq	40(%r14), %rax
	movq	$5, %rcx
	shlq	%cl, %rdi
	addq	%rax, %rdi
	movq	%r15, %rsi
	callq	df_ref_unlink
	movslq	36(%r15), %rax
	movq	16(%r14), %rcx
	movq	$0, (%rcx,%rax,8)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB1_15:                               # %if.then4.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_unlink, %rcx
	movq	$692, %rsi              # imm = 0x2B4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp19:
	.size	df_def_unlink, .Ltmp19-df_def_unlink
	.cfi_endproc

	.align	16, 0x90
	.type	df_ref_unlink,@function
df_ref_unlink:                          # @df_ref_unlink
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 16
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB2_10
# BB#1:                                 # %if.then
	movq	(%rcx), %rax
	movq	8(%rcx), %rdx
	testq	%rax, %rax
	je	.LBB2_2
# BB#4:                                 # %if.else
	cmpq	%rsi, %rdx
	je	.LBB2_5
	.align	16, 0x90
.LBB2_6:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rdx
	movq	%rax, %rcx
	testq	%rcx, %rcx
	je	.LBB2_7
# BB#8:                                 # %for.body
                                        #   in Loop: Header=BB2_6 Depth=1
	movq	8(%rcx), %rdi
	movq	(%rcx), %rax
	cmpq	%rsi, %rdi
	jne	.LBB2_6
# BB#9:                                 # %if.then15
	movq	%rax, (%rdx)
.LBB2_10:                               # %return
	popq	%rdx
	retq
.LBB2_2:                                # %if.then2
	cmpq	%rsi, %rdx
	jne	.LBB2_11
# BB#3:                                 # %if.end
	movq	$0, (%rdi)
	movq	%rcx, %rax
	popq	%rdx
	retq
.LBB2_5:                                # %if.then7
	movq	%rax, (%rdi)
	movq	%rcx, %rax
	popq	%rdx
	retq
.LBB2_7:
	movq	%rdx, %rax
	popq	%rdx
	retq
.LBB2_11:                               # %if.then4
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_unlink, %rcx
	movq	$692, %rsi              # imm = 0x2B4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp22:
	.size	df_ref_unlink, .Ltmp22-df_ref_unlink
	.cfi_endproc

	.align	16, 0x90
	.type	df_use_unlink,@function
df_use_unlink:                          # @df_use_unlink
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp26:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 32
.Ltmp29:
	.cfi_offset %rbx, -24
.Ltmp30:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	(%rbx), %rax
	movzbl	(%rax), %ecx
	movq	%rdi, %r14
	cmpq	$63, %rcx
	jne	.LBB3_2
# BB#1:                                 # %cond.true
	movq	8(%rax), %rax
.LBB3_2:                                # %cond.end
	movq	24(%rbx), %rcx
	movl	8(%rax), %r8d
	jmp	.LBB3_3
	.align	16, 0x90
.LBB3_13:                               # %df_ref_unlink.exit
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	(%rcx), %rcx
.LBB3_3:                                # %cond.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_10 Depth 2
	testq	%rcx, %rcx
	je	.LBB3_14
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	8(%rcx), %rdi
	movq	24(%rdi), %rdx
	testq	%rdx, %rdx
	je	.LBB3_13
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	(%rdx), %rsi
	movq	8(%rdx), %rax
	testq	%rsi, %rsi
	jne	.LBB3_8
# BB#6:                                 # %if.then2.i
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB3_15
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	$0, 24(%rdi)
	movq	(%rcx), %rcx
	jmp	.LBB3_3
.LBB3_8:                                # %if.else.i
                                        #   in Loop: Header=BB3_3 Depth=1
	cmpq	%rbx, %rax
	jne	.LBB3_10
# BB#9:                                 # %if.then7.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rsi, 24(%rdi)
	movq	(%rcx), %rcx
	jmp	.LBB3_3
	.align	16, 0x90
.LBB3_10:                               # %for.cond.i
                                        #   Parent Loop BB3_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rdi
	movq	%rsi, %rdx
	testq	%rdx, %rdx
	je	.LBB3_13
# BB#11:                                # %for.body.i
                                        #   in Loop: Header=BB3_10 Depth=2
	movq	8(%rdx), %rax
	movq	(%rdx), %rsi
	cmpq	%rbx, %rax
	jne	.LBB3_10
# BB#12:                                # %if.then15.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rsi, (%rdi)
	jmp	.LBB3_13
.LBB3_14:                               # %for.end
	movq	$0, 24(%rbx)
	movq	40(%r14), %rax
	movq	$5, %rcx
	shlq	%cl, %r8
	leaq	8(%rax,%r8), %rdi
	movq	%rbx, %rsi
	callq	df_ref_unlink
	movslq	36(%rbx), %rax
	movq	24(%r14), %rcx
	movq	$0, (%rcx,%rax,8)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB3_15:                               # %if.then4.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_unlink, %rcx
	movq	$692, %rsi              # imm = 0x2B4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp31:
	.size	df_use_unlink, .Ltmp31-df_use_unlink
	.cfi_endproc

	.globl	df_init
	.align	16, 0x90
	.type	df_init,@function
df_init:                                # @df_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 16
	movq	$1, %rdi
	movq	$192, %rsi
	callq	xcalloc
	movq	%rax, ddf(%rip)
	popq	%rdx
	retq
.Ltmp34:
	.size	df_init, .Ltmp34-df_init
	.cfi_endproc

	.globl	df_analyse
	.align	16, 0x90
	.type	df_analyse,@function
df_analyse:                             # @df_analyse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp41:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp43:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp46:
	.cfi_def_cfa_offset 64
.Ltmp47:
	.cfi_offset %rbx, -48
.Ltmp48:
	.cfi_offset %r12, -40
.Ltmp49:
	.cfi_offset %r13, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	92(%r12), %eax
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r15
	testq	%rax, %rax
	je	.LBB5_4
# BB#1:                                 # %entry
	movl	n_basic_blocks(%rip), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	%rdx, %rax
	jne	.LBB5_2
.LBB5_4:                                # %if.end
	testq	%rax, %rax
	je	.LBB5_18
# BB#5:                                 # %for.body.lr.ph.i
	xorq	%rbx, %rbx
	testq	%r15, %r15
	jne	.LBB5_9
# BB#6:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_7:                                # %for.body.us18.i
                                        # =>This Inner Loop Header: Depth=1
	movq	120(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	testq	%r14, %rax
	jne	.LBB5_22
# BB#8:                                 # %for.inc.us26.i
                                        #   in Loop: Header=BB5_7 Depth=1
	movl	92(%r12), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_7
	jmp	.LBB5_18
.LBB5_9:                                # %for.body.lr.ph.i
	cmpq	$-1, %r15
	jne	.LBB5_13
# BB#10:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	$1, %r13
	.align	16, 0x90
.LBB5_11:                               # %for.body.us.i
                                        # =>This Inner Loop Header: Depth=1
	movq	120(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	testq	%r14, %rax
	jne	.LBB5_35
# BB#12:                                # %for.inc.us.i
                                        #   in Loop: Header=BB5_11 Depth=1
	movl	92(%r12), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r14, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_11
	jmp	.LBB5_18
.LBB5_13:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB5_14:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	120(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	je	.LBB5_17
# BB#15:                                # %lor.lhs.false3.i
                                        #   in Loop: Header=BB5_14 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	jne	.LBB5_16
.LBB5_17:                               # %for.inc.i
                                        #   in Loop: Header=BB5_14 Depth=1
	movl	92(%r12), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r13, %rcx
	cmpq	%rax, %rcx
	jb	.LBB5_14
.LBB5_18:                               # %lor.lhs.false
	movl	(%r12), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rdx           # 8-byte Reload
	andq	%rdx, %rcx
	xorq	%r13, %r13
	cmpq	%rcx, %rax
	je	.LBB5_39
# BB#19:                                # %lor.lhs.false.if.then5_crit_edge
	xorq	%r13, %r13
.LBB5_20:                               # %if.then5
	cmpq	$-1, %r15
	jne	.LBB5_21
.LBB5_35:                               # %if.then14
	movq	120(%r12), %r15
	jmp	.LBB5_36
.LBB5_21:                               # %if.then5
	testq	%r15, %r15
	jne	.LBB5_36
.LBB5_22:                               # %if.then7
	movl	92(%r12), %eax
	testq	%rax, %rax
	je	.LBB5_24
# BB#23:                                # %if.then10
	movq	%r12, %rdi
	callq	df_free
.LBB5_24:                               # %if.end11
	callq	max_reg_num
	movq	%rax, %rbx
	movabsq	$df_ref_obstack, %rdi
	callq	gcc_obstack_init
	callq	get_max_uid
	movq	%rax, %r15
	leaq	1(%r15), %r13
	xorq	%r14, %r14
	movl	%r14d, 68(%r12)
	movl	%r14d, 76(%r12)
	movl	%r13d, 72(%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r13, %rdi
	andq	%rax, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 16(%r12)
	movl	%r14d, 80(%r12)
	movl	%r14d, 88(%r12)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	leaq	2(%r15,%r15), %rax
	movl	%eax, 84(%r12)
	movq	%r13, %rdi
	andq	$2147483647, %rdi       # imm = 0x7FFFFFFF
	shlq	$4, %rdi
	callq	xmalloc
	movq	%rax, 24(%r12)
	movl	%ebx, 96(%r12)
	movl	n_basic_blocks(%rip), %eax
	movl	%eax, 92(%r12)
	andq	%r14, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, 32(%r12)
	movl	64(%r12), %eax
	movq	$2, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	movq	%r13, %rcx
	andq	%r14, %rcx
	je	.LBB5_26
# BB#25:                                # %if.end11
	movq	%r13, %rbx
.LBB5_26:                               # %if.end11
	addq	%rax, %rbx
	movq	56(%r12), %rdi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, 56(%r12)
	movl	64(%r12), %ecx
	leaq	(%rcx,%rcx,2), %rdx
	leaq	(%rax,%rdx,8), %rdi
	movq	%rbx, %rax
	subq	%rcx, %rax
	andq	%r14, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%ebx, 64(%r12)
	cmpq	$0, 112(%r12)
	jne	.LBB5_28
# BB#27:                                # %if.then10.i.i
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 112(%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
.LBB5_28:                               # %df_insn_table_realloc.exit.i
	movl	48(%r12), %eax
	movl	96(%r12), %edx
	movq	$2, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	testq	%rdx, %rdx
	je	.LBB5_30
# BB#29:                                # %df_insn_table_realloc.exit.i
	movq	%rdx, %rbx
.LBB5_30:                               # %df_insn_table_realloc.exit.i
	addq	%rax, %rbx
	movq	40(%r12), %rdi
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$5, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 40(%r12)
	movl	48(%r12), %edi
	movq	$5, %rcx
	movq	%rbx, %rdx
	subq	%rdi, %rdx
	shlq	%cl, %rdi
	addq	%rax, %rdi
	andq	%r14, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	memset
	movl	%ebx, 48(%r12)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 120(%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
	movl	%r14d, (%r12)
	movl	92(%r12), %edi
	movq	$112, %rsi
	callq	xcalloc
	movq	%rax, 8(%r12)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 128(%r12)
	movslq	n_basic_blocks(%rip), %rcx
	testq	%rcx, %rcx
	jle	.LBB5_34
# BB#31:                                # %for.body.i45.preheader
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	bitmap_set_bit
	movslq	n_basic_blocks(%rip), %rax
	cmpq	$2, %rax
	jl	.LBB5_34
# BB#32:
	movq	$1, %rbx
	.align	16, 0x90
.LBB5_33:                               # %for.body.for.body_crit_edge.i
                                        # =>This Inner Loop Header: Depth=1
	movq	128(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	movslq	n_basic_blocks(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB5_33
.LBB5_34:                               # %df_alloc.exit
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	df_analyse_1
	movq	$1, %r13
	jmp	.LBB5_39
.LBB5_36:                               # %if.end15
	movl	92(%r12), %eax
	testq	%rax, %rax
	je	.LBB5_37
# BB#38:                                # %if.end19
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	df_analyse_1
	movq	120(%r12), %rdi
	callq	bitmap_clear
.LBB5_39:                               # %if.end22
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_16:
	movq	$1, %r13
	jmp	.LBB5_20
.LBB5_2:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_analyse, %rcx
	movq	$2302, %rsi             # imm = 0x8FE
	jmp	.LBB5_3
.LBB5_37:                               # %if.then18
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_analyse, %rcx
	movq	$2325, %rsi             # imm = 0x915
.LBB5_3:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp52:
	.size	df_analyse, .Ltmp52-df_analyse
	.cfi_endproc

	.align	16, 0x90
	.type	df_free,@function
df_free:                                # @df_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 16
.Ltmp56:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$255, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	df_bitmaps_free
	movq	8(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_2
# BB#1:                                 # %if.then
	callq	free
.LBB6_2:                                # %if.end
	movq	$0, 8(%rbx)
	movq	56(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_4
# BB#3:                                 # %if.then4
	callq	free
.LBB6_4:                                # %if.end6
	movq	$0, 56(%rbx)
	xorq	%rax, %rax
	movl	%eax, 64(%rbx)
	movq	16(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_6
# BB#5:                                 # %if.then9
	callq	free
.LBB6_6:                                # %if.end11
	movq	$0, 16(%rbx)
	movq	$0, 68(%rbx)
	movq	24(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_8
# BB#7:                                 # %if.then14
	callq	free
.LBB6_8:                                # %if.end16
	movq	$0, 24(%rbx)
	movq	$0, 80(%rbx)
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_10
# BB#9:                                 # %if.then19
	callq	free
.LBB6_10:                               # %if.end21
	movq	$0, 40(%rbx)
	xorq	%rax, %rax
	movl	%eax, 48(%rbx)
	movq	120(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_12
# BB#11:                                # %if.then27
	callq	bitmap_clear
	movq	120(%rbx), %rdi
	callq	free
	movq	$0, 120(%rbx)
.LBB6_12:                               # %if.end32
	movq	$0, 120(%rbx)
	movq	112(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_14
# BB#13:                                # %if.then39
	callq	bitmap_clear
	movq	112(%rbx), %rdi
	callq	free
	movq	$0, 112(%rbx)
.LBB6_14:                               # %if.end45
	movq	$0, 112(%rbx)
	movq	128(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB6_16
# BB#15:                                # %if.then49
	callq	bitmap_clear
	movq	128(%rbx), %rdi
	callq	free
	movq	$0, 128(%rbx)
.LBB6_16:                               # %do.end54
	movabsq	$df_ref_obstack, %rdi
	xorq	%rsi, %rsi
	movq	$0, 128(%rbx)
	popq	%rbx
	jmp	obstack_free  # TAILCALL
.Ltmp57:
	.size	df_free, .Ltmp57-df_free
	.cfi_endproc

	.align	16, 0x90
	.type	df_analyse_1,@function
df_analyse_1:                           # @df_analyse_1
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
	subq	$192, %rsp
.Ltmp69:
	.cfi_def_cfa_offset 240
.Ltmp70:
	.cfi_offset %rbx, -48
.Ltmp71:
	.cfi_offset %r12, -40
.Ltmp72:
	.cfi_offset %r13, -32
.Ltmp73:
	.cfi_offset %r14, -24
.Ltmp74:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%rdx, %r9
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r9, %rdi
	andq	%rdx, %rdi
	testq	$16, %r9
	movq	%r9, %rsi
	je	.LBB7_2
# BB#1:                                 # %entry
	movq	%r9, %rcx
	orq	$65, %rcx
	movq	%rcx, %rsi
.LBB7_2:                                # %entry
	movq	$2, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	movq	%r9, %rbx
	andq	$2, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$6, %rcx
	shlq	%cl, %rbx
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movq	$3, %rcx
	shrq	%cl, %rdi
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	jne	.LBB7_4
# BB#3:                                 # %if.then17
	movq	128(%r15), %rcx
	movq	%rcx, 64(%rsp)          # 8-byte Spill
.LBB7_4:                                # %if.end18
	movq	%rsi, 56(%rsp)          # 8-byte Spill
	orq	%rsi, %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	orq	%rsi, %rdi
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movl	%r9d, (%r15)
	movq	%rdx, %rbx
	testq	%rbx, %r8
	je	.LBB7_28
# BB#5:                                 # %if.then21
	movq	%r9, 48(%rsp)           # 8-byte Spill
	callq	max_reg_num
	movl	48(%r15), %edx
	andq	%rbx, %rax
	cmpq	%rdx, %rax
	jb	.LBB7_7
# BB#6:                                 # %if.then.i
	movq	$2, %rcx
	movq	%rdx, %r14
	shrq	%cl, %r14
	addq	%rdx, %r14
	movq	40(%r15), %rdi
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$5, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 40(%r15)
	movl	48(%r15), %edi
	movq	$5, %rcx
	movq	%r14, %rdx
	subq	%rdi, %rdx
	shlq	%cl, %rdi
	addq	%rax, %rdi
	andq	%rbx, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%r14d, 48(%r15)
.LBB7_7:                                # %if.end.i
	movl	68(%r15), %eax
	movl	%eax, 100(%r15)
	movl	80(%r15), %eax
	movl	%eax, 104(%r15)
	movq	120(%r15), %rax
	jmp	.LBB7_8
	.align	16, 0x90
.LBB7_27:                               # %for.end37.i
                                        #   in Loop: Header=BB7_8 Depth=1
	movq	144(%rsp), %rax         # 8-byte Reload
.LBB7_8:                                # %if.end.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_10 Depth 2
                                        #       Child Loop BB7_12 Depth 3
                                        #         Child Loop BB7_14 Depth 4
                                        #           Child Loop BB7_16 Depth 5
                                        #           Child Loop BB7_19 Depth 5
	movq	(%rax), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_49
# BB#9:                                 # %for.cond11.preheader.i
                                        #   in Loop: Header=BB7_8 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_10:                               # %for.body13.i
                                        #   Parent Loop BB7_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_12 Depth 3
                                        #         Child Loop BB7_14 Depth 4
                                        #           Child Loop BB7_16 Depth 5
                                        #           Child Loop BB7_19 Depth 5
	movq	%rcx, 128(%rsp)         # 8-byte Spill
	movq	%r15, %r14
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	movq	%rbx, %r15
	je	.LBB7_26
# BB#11:                                # %for.body18.preheader.i
                                        #   in Loop: Header=BB7_10 Depth=2
	movq	$6, %rcx
	movq	128(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_12:                               # %for.body18.i
                                        #   Parent Loop BB7_8 Depth=1
                                        #     Parent Loop BB7_10 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_14 Depth 4
                                        #           Child Loop BB7_16 Depth 5
                                        #           Child Loop BB7_19 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_25
# BB#13:                                # %if.then20.i
                                        #   in Loop: Header=BB7_12 Depth=3
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	%rdx, 168(%rsp)         # 8-byte Spill
	movq	144(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	136(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 160(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%r15, %rdx
	movq	32(%rcx,%rdx,8), %rbx
	movq	%rbx, 184(%rsp)         # 8-byte Spill
	movq	%rbx, %r13
	jmp	.LBB7_14
	.align	16, 0x90
.LBB7_23:                               # %for.inc.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	addq	$24, %r13
.LBB7_14:                               # %for.cond.i.i
                                        #   Parent Loop BB7_8 Depth=1
                                        #     Parent Loop BB7_10 Depth=2
                                        #       Parent Loop BB7_12 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_16 Depth 5
                                        #           Child Loop BB7_19 Depth 5
	movq	(%r13), %r13
	movl	8(%r13), %esi
	movq	112(%r14), %rdi
	callq	bitmap_bit_p
	testq	%r15, %rax
	je	.LBB7_22
# BB#15:                                # %if.then.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movl	8(%r13), %ecx
	movq	56(%r14), %rax
	leaq	(%rcx,%rcx,2), %r15
	movq	%r14, %r12
	movq	(%rax,%r15,8), %r14
	testq	%r14, %r14
	je	.LBB7_18
	.align	16, 0x90
.LBB7_16:                               # %for.body.i.i.i
                                        #   Parent Loop BB7_8 Depth=1
                                        #     Parent Loop BB7_10 Depth=2
                                        #       Parent Loop BB7_12 Depth=3
                                        #         Parent Loop BB7_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%r14), %rsi
	movq	%r12, %rdi
	callq	df_def_unlink
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB7_16
# BB#17:                                # %for.cond.for.end_crit_edge.i.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movq	56(%r12), %rax
.LBB7_18:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movq	8(%rax,%r15,8), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r14
	je	.LBB7_21
	.align	16, 0x90
.LBB7_19:                               # %for.body7.i.i.i
                                        #   Parent Loop BB7_8 Depth=1
                                        #     Parent Loop BB7_10 Depth=2
                                        #       Parent Loop BB7_12 Depth=3
                                        #         Parent Loop BB7_14 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	df_use_unlink
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_19
# BB#20:                                # %for.cond5.for.end11_crit_edge.i.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movq	56(%r14), %rax
.LBB7_21:                               # %df_insn_refs_unlink.exit.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movq	$0, 8(%rax,%r15,8)
	movq	$0, (%rax,%r15,8)
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	df_insn_refs_record
	movq	112(%r14), %rdi
	movl	8(%r13), %esi
	callq	bitmap_clear_bit
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	movq	184(%rsp), %rbx         # 8-byte Reload
.LBB7_22:                               # %if.end.i.i
                                        #   in Loop: Header=BB7_14 Depth=4
	movq	8(%rbx), %rax
	cmpq	%rax, %r13
	jne	.LBB7_23
# BB#24:                                # %df_bb_refs_update.exit.i
                                        #   in Loop: Header=BB7_12 Depth=3
	movq	152(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	168(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	160(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_26
.LBB7_25:                               # %for.inc.i
                                        #   in Loop: Header=BB7_12 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%r15, %rax
	cmpq	$64, %rax
	jb	.LBB7_12
.LBB7_26:                               # %if.end34.i
                                        #   in Loop: Header=BB7_10 Depth=2
	movq	%r15, %rbx
	movq	128(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	movq	%r14, %r15
	jne	.LBB7_10
	jmp	.LBB7_27
.LBB7_28:                               # %if.else
	movq	%r9, 48(%rsp)           # 8-byte Spill
	movl	68(%r15), %eax
	movl	%eax, 100(%r15)
	movl	80(%r15), %eax
	movl	%eax, 104(%r15)
	movq	%r15, %r13
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rbx, %rsi
	je	.LBB7_48
	.align	16, 0x90
.LBB7_29:                               # %for.cond7.preheader.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_30 Depth 2
                                        #       Child Loop BB7_32 Depth 3
                                        #     Child Loop BB7_39 Depth 2
                                        #       Child Loop BB7_41 Depth 3
	movq	%rdx, 184(%rsp)         # 8-byte Spill
	movq	24(%rdx), %rdi
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB7_38
	.align	16, 0x90
.LBB7_30:                               # %for.body14.i
                                        #   Parent Loop BB7_29 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_32 Depth 3
	movq	$1, %r15
	movq	%r14, %rcx
	shlq	%cl, %r15
	testq	%rdi, %r15
	je	.LBB7_37
# BB#31:                                # %if.then16.i
                                        #   in Loop: Header=BB7_30 Depth=2
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	addq	%r14, %rax
	andq	%rsi, %rax
	movq	32(%rdx,%rax,8), %r12
	movq	%r12, %rbx
	jmp	.LBB7_32
	.align	16, 0x90
.LBB7_35:                               # %for.inc.i.i496
                                        #   in Loop: Header=BB7_32 Depth=3
	addq	$24, %rbx
.LBB7_32:                               # %for.cond.i.i492
                                        #   Parent Loop BB7_29 Depth=1
                                        #     Parent Loop BB7_30 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_34
# BB#33:                                # %if.then.i.i493
                                        #   in Loop: Header=BB7_32 Depth=3
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	df_insn_refs_record
.LBB7_34:                               # %if.end.i.i494
                                        #   in Loop: Header=BB7_32 Depth=3
	movq	8(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB7_35
# BB#36:                                # %df_bb_refs_record.exit.i
                                        #   in Loop: Header=BB7_30 Depth=2
	notq	%r15
	movq	168(%rsp), %rdi         # 8-byte Reload
	andq	%r15, %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	184(%rsp), %rdx         # 8-byte Reload
	je	.LBB7_38
.LBB7_37:                               # %for.inc.i499
                                        #   in Loop: Header=BB7_30 Depth=2
	incq	%r14
	movq	%r14, %rax
	andq	%rsi, %rax
	cmpq	$64, %rax
	jb	.LBB7_30
.LBB7_38:                               # %if.end28.i
                                        #   in Loop: Header=BB7_29 Depth=1
	movq	32(%rdx), %rdi
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB7_47
	.align	16, 0x90
.LBB7_39:                               # %for.body14.1.i
                                        #   Parent Loop BB7_29 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_41 Depth 3
	movq	$1, %r15
	movq	%r14, %rcx
	shlq	%cl, %r15
	testq	%rdi, %r15
	je	.LBB7_46
# BB#40:                                # %if.then16.1.i
                                        #   in Loop: Header=BB7_39 Depth=2
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movl	16(%rdx), %eax
	movq	$7, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	leaq	64(%r14,%rax), %rax
	andq	%rsi, %rax
	movq	32(%rdx,%rax,8), %r12
	movq	%r12, %rbx
	jmp	.LBB7_41
	.align	16, 0x90
.LBB7_44:                               # %for.inc.i.1.i
                                        #   in Loop: Header=BB7_41 Depth=3
	addq	$24, %rbx
.LBB7_41:                               # %for.cond.i.1.i
                                        #   Parent Loop BB7_29 Depth=1
                                        #     Parent Loop BB7_39 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rbx), %rbx
	movzbl	(%rbx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_43
# BB#42:                                # %if.then.i.1.i
                                        #   in Loop: Header=BB7_41 Depth=3
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	df_insn_refs_record
.LBB7_43:                               # %if.end.i.1.i
                                        #   in Loop: Header=BB7_41 Depth=3
	movq	8(%r12), %rax
	cmpq	%rax, %rbx
	jne	.LBB7_44
# BB#45:                                # %df_bb_refs_record.exit.1.i
                                        #   in Loop: Header=BB7_39 Depth=2
	notq	%r15
	movq	168(%rsp), %rdi         # 8-byte Reload
	andq	%r15, %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	184(%rsp), %rdx         # 8-byte Reload
	je	.LBB7_47
.LBB7_46:                               # %for.inc.1.i
                                        #   in Loop: Header=BB7_39 Depth=2
	incq	%r14
	movq	%r14, %rax
	andq	%rsi, %rax
	cmpq	$64, %rax
	jb	.LBB7_39
.LBB7_47:                               # %if.end28.1.i
                                        #   in Loop: Header=BB7_29 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB7_29
.LBB7_48:                               # %df_refs_record.exit
	movq	%rsi, %rbx
	movq	%r13, %r15
.LBB7_49:                               # %if.end24
	movq	%r15, %rdi
	callq	df_refs_process
	xorq	%rsi, %rsi
	andq	$4, 72(%rsp)            # 8-byte Folded Spill
	movq	80(%rsp), %rdx          # 8-byte Reload
	je	.LBB7_53
# BB#50:                                # %land.lhs.true.i
	movq	%rdx, %r12
	movl	96(%r15), %r14d
	callq	max_reg_num
	andq	%rbx, %rax
	cmpq	%rax, %r14
	movq	$6, %rsi
	jb	.LBB7_52
# BB#51:                                # %select.mid
	xorq	%rsi, %rsi
.LBB7_52:                               # %select.end
	movq	%r12, %rdx
.LBB7_53:                               # %if.end.i503
	andq	$2, %rdx
	je	.LBB7_56
# BB#54:                                # %land.lhs.true3.i
	movl	88(%r15), %eax
	movl	80(%r15), %ecx
	cmpq	%rcx, %rax
	jae	.LBB7_56
# BB#55:
	orq	$2, %rsi
.LBB7_56:                               # %if.end7.i
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	$1, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	je	.LBB7_57
# BB#58:                                # %land.lhs.true10.i
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movl	76(%r15), %eax
	movl	68(%r15), %ecx
	cmpq	%rcx, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
	orq	%rax, %rsi
	jmp	.LBB7_59
.LBB7_57:
	movq	%rdx, 80(%rsp)          # 8-byte Spill
.LBB7_59:                               # %if.end14.i
	testq	%rbx, %rsi
	je	.LBB7_61
# BB#60:                                # %if.then16.i506
	movq	%r15, %rdi
	callq	df_bitmaps_free
.LBB7_61:                               # %if.end17.i
	movl	68(%r15), %eax
	movl	%eax, 76(%r15)
	movl	80(%r15), %eax
	movl	%eax, 88(%r15)
	movl	92(%r15), %eax
	testq	%rax, %rax
	movq	%r15, %rdx
	movq	%rdx, 176(%rsp)         # 8-byte Spill
	je	.LBB7_79
# BB#62:                                # %for.body.lr.ph.i
	xorq	%r13, %r13
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB7_72
	.align	16, 0x90
.LBB7_63:                               # %land.lhs.true28.us.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	%r13, %rcx
	andq	%rbx, %rcx
	movq	%rbx, %r14
	movq	32(%rax,%rcx,8), %rax
	movslq	88(%rax), %rax
	movq	8(%rdx), %r12
	imulq	$112, %rax, %rbx
	cmpq	$0, 16(%r12,%rbx)
	jne	.LBB7_65
# BB#64:                                # %if.then30.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	leaq	16(%r12,%rbx), %r15
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 8(%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r15)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 24(%r12,%rbx)
	xorq	%rax, %rax
	movl	%eax, 96(%r12,%rbx)
.LBB7_65:                               # %if.end42.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB7_68
# BB#66:                                # %land.lhs.true45.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	cmpq	$0, 48(%r12,%rbx)
	jne	.LBB7_68
# BB#67:                                # %if.then47.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	leaq	48(%r12,%rbx), %r15
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 32(%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 40(%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r15)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 56(%r12,%rbx)
	xorq	%rax, %rax
	movl	%eax, 100(%r12,%rbx)
.LBB7_68:                               # %if.end59.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%r14, %rax
	je	.LBB7_71
# BB#69:                                # %land.lhs.true62.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	cmpq	$0, 80(%r12,%rbx)
	jne	.LBB7_71
# BB#70:                                # %if.then64.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	leaq	80(%r12,%rbx), %r15
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 64(%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 72(%r12,%rbx)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r15)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 88(%r12,%rbx)
	xorq	%rax, %rax
	movl	%eax, 104(%r12,%rbx)
.LBB7_71:                               # %for.inc.us.i
                                        #   in Loop: Header=BB7_63 Depth=1
	movq	176(%rsp), %rdx         # 8-byte Reload
	movl	92(%rdx), %eax
	incq	%r13
	movq	%r13, %rcx
	movq	%r14, %rbx
	andq	%rbx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB7_63
	jmp	.LBB7_79
	.align	16, 0x90
.LBB7_72:                               # %if.end42.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	%r13, %rcx
	andq	%rbx, %rcx
	movq	32(%rax,%rcx,8), %rax
	movslq	88(%rax), %r15
	movq	8(%rdx), %r14
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	%rdx, %rbx
	je	.LBB7_75
# BB#73:                                # %land.lhs.true45.i
                                        #   in Loop: Header=BB7_72 Depth=1
	imulq	$112, %r15, %r12
	cmpq	$0, 48(%r14,%r12)
	jne	.LBB7_75
# BB#74:                                # %if.then47.i
                                        #   in Loop: Header=BB7_72 Depth=1
	leaq	48(%r14,%r12), %rbx
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 32(%r14,%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 40(%r14,%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%rbx)
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 56(%r14,%r12)
	xorq	%rax, %rax
	movl	%eax, 100(%r14,%r12)
.LBB7_75:                               # %if.end59.i
                                        #   in Loop: Header=BB7_72 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB7_78
# BB#76:                                # %land.lhs.true62.i
                                        #   in Loop: Header=BB7_72 Depth=1
	imulq	$112, %r15, %r15
	cmpq	$0, 80(%r14,%r15)
	jne	.LBB7_78
# BB#77:                                # %if.then64.i
                                        #   in Loop: Header=BB7_72 Depth=1
	leaq	80(%r14,%r15), %r12
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 64(%r14,%r15)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 72(%r14,%r15)
	movq	%rax, %rdi
	callq	bitmap_clear
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r12)
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 88(%r14,%r15)
	xorq	%rax, %rax
	movl	%eax, 104(%r14,%r15)
.LBB7_78:                               # %for.inc.i509
                                        #   in Loop: Header=BB7_72 Depth=1
	movl	92(%rbx), %eax
	incq	%r13
	movq	%r13, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rsi
	andq	%rsi, %rcx
	cmpq	%rax, %rcx
	movq	%rbx, %rdx
	movq	%rsi, %rbx
	jb	.LBB7_72
.LBB7_79:                               # %df_bitmaps_alloc.exit
	movq	%rdx, 176(%rsp)         # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	%rdx, %r14
	je	.LBB7_102
# BB#80:                                # %for.cond7.preheader.lr.ph.i
	movq	136(%rsp), %r8          # 8-byte Reload
	.align	16, 0x90
.LBB7_81:                               # %for.cond7.preheader.i514
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_82 Depth 2
                                        #       Child Loop BB7_84 Depth 3
                                        #     Child Loop BB7_92 Depth 2
                                        #       Child Loop BB7_94 Depth 3
	movq	24(%r8), %r9
	xorq	%rax, %rax
	testq	%r9, %r9
	je	.LBB7_91
	.align	16, 0x90
.LBB7_82:                               # %for.body14.i520
                                        #   Parent Loop BB7_81 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_84 Depth 3
	movq	$1, %r10
	movq	%rax, %rcx
	shlq	%cl, %r10
	testq	%r9, %r10
	je	.LBB7_90
# BB#83:                                # %if.then16.i530
                                        #   in Loop: Header=BB7_82 Depth=2
	movl	16(%r8), %edx
	movq	$7, %rcx
	movq	basic_block_info(%rip), %rsi
	shlq	%cl, %rdx
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	%rbx, %r11
	movq	32(%rsi,%rdx,8), %rdx
	xorq	%rbx, %rbx
	movq	%rdx, %rcx
	jmp	.LBB7_84
	.align	16, 0x90
.LBB7_88:                               # %for.inc.i.i542
                                        #   in Loop: Header=BB7_84 Depth=3
	addq	$24, %rcx
.LBB7_84:                               # %for.cond.i.i537
                                        #   Parent Loop BB7_81 Depth=1
                                        #     Parent Loop BB7_82 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rcx), %rcx
	movzbl	(%rcx), %esi
	movzbl	rtx_class(%rsi), %esi
	cmpq	$105, %rsi
	jne	.LBB7_85
# BB#86:                                # %if.then.i.i539
                                        #   in Loop: Header=BB7_84 Depth=3
	movslq	8(%rcx), %rdi
	movq	56(%r14), %rsi
	leaq	(%rdi,%rdi,2), %rdi
	movl	%ebx, 16(%rsi,%rdi,8)
	incq	%rbx
	jmp	.LBB7_87
	.align	16, 0x90
.LBB7_85:                               # %for.cond.if.end_crit_edge.i.i
                                        #   in Loop: Header=BB7_84 Depth=3
	movq	56(%r14), %rsi
.LBB7_87:                               # %if.end.i.i540
                                        #   in Loop: Header=BB7_84 Depth=3
	movslq	8(%rcx), %rdi
	leaq	(%rdi,%rdi,2), %rdi
	movl	%ebx, 16(%rsi,%rdi,8)
	movq	8(%rdx), %rsi
	cmpq	%rsi, %rcx
	jne	.LBB7_88
# BB#89:                                # %df_bb_luids_set.exit.i
                                        #   in Loop: Header=BB7_82 Depth=2
	notq	%r10
	andq	%r10, %r9
	movq	%r11, %rbx
	je	.LBB7_91
.LBB7_90:                               # %for.inc.i547
                                        #   in Loop: Header=BB7_82 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$64, %rcx
	jb	.LBB7_82
.LBB7_91:                               # %if.end29.i
                                        #   in Loop: Header=BB7_81 Depth=1
	movq	32(%r8), %r9
	xorq	%rax, %rax
	testq	%r9, %r9
	je	.LBB7_101
	.align	16, 0x90
.LBB7_92:                               # %for.body14.1.i555
                                        #   Parent Loop BB7_81 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_94 Depth 3
	movq	$1, %r10
	movq	%rax, %rcx
	shlq	%cl, %r10
	testq	%r9, %r10
	je	.LBB7_100
# BB#93:                                # %if.then16.1.i565
                                        #   in Loop: Header=BB7_92 Depth=2
	movl	16(%r8), %edx
	movq	$7, %rcx
	movq	basic_block_info(%rip), %rsi
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rcx
	andq	%rbx, %rcx
	movq	%rbx, %r11
	movq	32(%rsi,%rcx,8), %rcx
	xorq	%rbx, %rbx
	movq	%rcx, %rdx
	jmp	.LBB7_94
	.align	16, 0x90
.LBB7_98:                               # %for.inc.i.1.i576
                                        #   in Loop: Header=BB7_94 Depth=3
	addq	$24, %rdx
.LBB7_94:                               # %for.cond.i.1.i572
                                        #   Parent Loop BB7_81 Depth=1
                                        #     Parent Loop BB7_92 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	(%rdx), %rdx
	movzbl	(%rdx), %esi
	movzbl	rtx_class(%rsi), %esi
	cmpq	$105, %rsi
	jne	.LBB7_95
# BB#96:                                # %if.then.i.1.i573
                                        #   in Loop: Header=BB7_94 Depth=3
	movslq	8(%rdx), %rdi
	movq	56(%r14), %rsi
	leaq	(%rdi,%rdi,2), %rdi
	movl	%ebx, 16(%rsi,%rdi,8)
	incq	%rbx
	jmp	.LBB7_97
	.align	16, 0x90
.LBB7_95:                               # %for.cond.if.end_crit_edge.i.1.i
                                        #   in Loop: Header=BB7_94 Depth=3
	movq	56(%r14), %rsi
.LBB7_97:                               # %if.end.i.1.i574
                                        #   in Loop: Header=BB7_94 Depth=3
	movslq	8(%rdx), %rdi
	leaq	(%rdi,%rdi,2), %rdi
	movl	%ebx, 16(%rsi,%rdi,8)
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB7_98
# BB#99:                                # %df_bb_luids_set.exit.1.i
                                        #   in Loop: Header=BB7_92 Depth=2
	notq	%r10
	andq	%r10, %r9
	movq	%r11, %rbx
	je	.LBB7_101
.LBB7_100:                              # %for.inc.1.i581
                                        #   in Loop: Header=BB7_92 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$64, %rcx
	jb	.LBB7_92
.LBB7_101:                              # %if.end29.1.i
                                        #   in Loop: Header=BB7_81 Depth=1
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB7_81
.LBB7_102:                              # %df_luids_set.exit
	testq	$64, 56(%rsp)           # 8-byte Folded Reload
	je	.LBB7_128
# BB#103:                               # %df_luids_set.exit
	cmpq	$0, 136(%rsp)           # 8-byte Folded Reload
	je	.LBB7_128
	.align	16, 0x90
.LBB7_104:                              # %for.cond7.preheader.i589
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_105 Depth 2
                                        #       Child Loop BB7_107 Depth 3
                                        #         Child Loop BB7_110 Depth 4
                                        #           Child Loop BB7_114 Depth 5
	movq	%r14, %r15
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_105:                              # %for.body9.i
                                        #   Parent Loop BB7_104 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_107 Depth 3
                                        #         Child Loop BB7_110 Depth 4
                                        #           Child Loop BB7_114 Depth 5
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_126
# BB#106:                               # %for.body14.preheader.i
                                        #   in Loop: Header=BB7_105 Depth=2
	movq	$6, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_107:                              # %for.body14.i597
                                        #   Parent Loop BB7_104 Depth=1
                                        #     Parent Loop BB7_105 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_110 Depth 4
                                        #           Child Loop BB7_114 Depth 5
	movq	$1, %rax
	movq	%rcx, %rsi
                                        # kill: RCX<def> RSI<kill>
	shlq	%cl, %rax
	testq	%rdx, %rax
	jne	.LBB7_109
# BB#108:                               #   in Loop: Header=BB7_107 Depth=3
	movq	%rsi, %rcx
	jmp	.LBB7_125
	.align	16, 0x90
.LBB7_109:                              # %if.then16.i608
                                        #   in Loop: Header=BB7_107 Depth=3
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	8(%rax), %rcx
	jmp	.LBB7_110
	.align	16, 0x90
.LBB7_123:                              # %for.inc29.i.i
                                        #   in Loop: Header=BB7_110 Depth=4
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rcx
.LBB7_110:                              # %if.then16.i608
                                        #   Parent Loop BB7_104 Depth=1
                                        #     Parent Loop BB7_105 Depth=2
                                        #       Parent Loop BB7_107 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_114 Depth 5
	testq	%rcx, %rcx
	je	.LBB7_124
# BB#111:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB7_110 Depth=4
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB7_124
# BB#112:                               # %for.body.i.i
                                        #   in Loop: Header=BB7_110 Depth=4
	movzbl	(%rcx), %eax
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_123
# BB#113:                               # %if.end.i.i615
                                        #   in Loop: Header=BB7_110 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	movq	56(%r15), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	(%rcx,%rax,8), %r13
	testq	%r13, %r13
	je	.LBB7_123
	.align	16, 0x90
.LBB7_114:                              # %for.body10.i.i
                                        #   Parent Loop BB7_104 Depth=1
                                        #     Parent Loop BB7_105 Depth=2
                                        #       Parent Loop BB7_107 Depth=3
                                        #         Parent Loop BB7_110 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%r13), %r14
	movq	(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_116
# BB#115:                               # %cond.true.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movq	8(%rax), %rax
.LBB7_116:                              # %cond.end.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movl	8(%rax), %r12d
	movq	40(%r15), %rax
	movq	$5, %rcx
	movq	%r12, %rsi
	movq	df_ref_obstack+32(%rip), %rdi
	movq	df_ref_obstack+24(%rip), %rdx
	shlq	%cl, %rsi
	movq	(%rax,%rsi), %r8
	subq	%rdx, %rdi
	cmpq	$15, %rdi
	jg	.LBB7_118
# BB#117:                               # %if.then.i.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movq	$16, %rsi
	movabsq	$df_ref_obstack, %rdi
	movq	%r8, %rbx
	callq	_obstack_newchunk
	movq	%rbx, %r8
	movq	df_ref_obstack+24(%rip), %rdx
.LBB7_118:                              # %if.end.i.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	addq	$16, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rdx
	jne	.LBB7_120
# BB#119:                               # %if.then8.i.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movzbl	df_ref_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, df_ref_obstack+80(%rip)
.LBB7_120:                              # %if.end9.i.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movslq	df_ref_obstack+48(%rip), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB7_122
# BB#121:                               # %if.then29.i.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB7_122:                              # %df_link_create.exit.i.i
                                        #   in Loop: Header=BB7_114 Depth=5
	movq	%rcx, df_ref_obstack+16(%rip)
	movq	%r8, (%rax)
	movq	%r14, 8(%rax)
	movq	40(%r15), %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	%rax, (%rdx,%r12)
	movq	(%r13), %r13
	testq	%r13, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jne	.LBB7_114
	jmp	.LBB7_123
	.align	16, 0x90
.LBB7_124:                              # %df_bb_reg_def_chain_create.exit.i
                                        #   in Loop: Header=BB7_107 Depth=3
	movq	144(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	160(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	152(%rsp), %rcx         # 8-byte Reload
	je	.LBB7_126
.LBB7_125:                              # %for.inc.i624
                                        #   in Loop: Header=BB7_107 Depth=3
	incq	%rcx
	movq	%rcx, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_107
.LBB7_126:                              # %if.end28.i626
                                        #   in Loop: Header=BB7_105 Depth=2
	movq	120(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_105
# BB#127:                               # %for.end31.i
                                        #   in Loop: Header=BB7_104 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	movq	%r15, %r14
	jne	.LBB7_104
.LBB7_128:                              # %if.end29
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	$128, %rax
	movq	104(%rsp), %rcx         # 8-byte Reload
	orq	%rcx, %rax
	je	.LBB7_154
# BB#129:                               # %if.then32
	movq	64(%rsp), %rax          # 8-byte Reload
	jmp	.LBB7_130
	.align	16, 0x90
.LBB7_153:                              # %for.end31.i740
                                        #   in Loop: Header=BB7_130 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
.LBB7_130:                              # %if.then32
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_132 Depth 2
                                        #       Child Loop BB7_134 Depth 3
                                        #         Child Loop BB7_136 Depth 4
                                        #           Child Loop BB7_140 Depth 5
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_154
# BB#131:                               # %for.cond7.preheader.i635
                                        #   in Loop: Header=BB7_130 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_132:                              # %for.body9.i639
                                        #   Parent Loop BB7_130 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_134 Depth 3
                                        #         Child Loop BB7_136 Depth 4
                                        #           Child Loop BB7_140 Depth 5
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_152
# BB#133:                               # %for.body14.preheader.i640
                                        #   in Loop: Header=BB7_132 Depth=2
	movq	$6, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_134:                              # %for.body14.i646
                                        #   Parent Loop BB7_130 Depth=1
                                        #     Parent Loop BB7_132 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_136 Depth 4
                                        #           Child Loop BB7_140 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_151
# BB#135:                               # %if.then16.i659
                                        #   in Loop: Header=BB7_134 Depth=3
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %rax
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	(%rax), %rcx
	jmp	.LBB7_136
	.align	16, 0x90
.LBB7_149:                              # %for.inc29.i.i729
                                        #   in Loop: Header=BB7_136 Depth=4
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	24(%rcx), %rcx
.LBB7_136:                              # %if.then16.i659
                                        #   Parent Loop BB7_130 Depth=1
                                        #     Parent Loop BB7_132 Depth=2
                                        #       Parent Loop BB7_134 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_140 Depth 5
	testq	%rcx, %rcx
	je	.LBB7_150
# BB#137:                               # %land.rhs.i.i664
                                        #   in Loop: Header=BB7_136 Depth=4
	movq	168(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB7_150
# BB#138:                               # %for.body.i.i670
                                        #   in Loop: Header=BB7_136 Depth=4
	movzbl	(%rcx), %eax
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_149
# BB#139:                               # %if.end.i.i676
                                        #   in Loop: Header=BB7_136 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	movq	56(%r14), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	8(%rcx,%rax,8), %r13
	testq	%r13, %r13
	je	.LBB7_149
	.align	16, 0x90
.LBB7_140:                              # %for.body10.i.i683
                                        #   Parent Loop BB7_130 Depth=1
                                        #     Parent Loop BB7_132 Depth=2
                                        #       Parent Loop BB7_134 Depth=3
                                        #         Parent Loop BB7_136 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%rbx, %r15
	movq	8(%r13), %r14
	movq	(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_142
# BB#141:                               # %cond.true.i.i686
                                        #   in Loop: Header=BB7_140 Depth=5
	movq	8(%rax), %rax
.LBB7_142:                              # %cond.end.i.i695
                                        #   in Loop: Header=BB7_140 Depth=5
	movl	8(%rax), %r12d
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movq	$5, %rcx
	movq	%r12, %rsi
	movq	df_ref_obstack+32(%rip), %rdi
	movq	df_ref_obstack+24(%rip), %rdx
	shlq	%cl, %rsi
	movq	8(%rax,%rsi), %r9
	subq	%rdx, %rdi
	cmpq	$15, %rdi
	jg	.LBB7_144
# BB#143:                               # %if.then.i.i.i697
                                        #   in Loop: Header=BB7_140 Depth=5
	movq	$16, %rsi
	movabsq	$df_ref_obstack, %rdi
	movq	%r9, %rbx
	callq	_obstack_newchunk
	movq	%rbx, %r9
	movq	df_ref_obstack+24(%rip), %rdx
.LBB7_144:                              # %if.end.i.i.i700
                                        #   in Loop: Header=BB7_140 Depth=5
	movq	%r15, %r8
	addq	$16, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rdx
	jne	.LBB7_146
# BB#145:                               # %if.then8.i.i.i703
                                        #   in Loop: Header=BB7_140 Depth=5
	movzbl	df_ref_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, df_ref_obstack+80(%rip)
.LBB7_146:                              # %if.end9.i.i.i717
                                        #   in Loop: Header=BB7_140 Depth=5
	movslq	df_ref_obstack+48(%rip), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB7_148
# BB#147:                               # %if.then29.i.i.i718
                                        #   in Loop: Header=BB7_140 Depth=5
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB7_148:                              # %df_link_create.exit.i.i724
                                        #   in Loop: Header=BB7_140 Depth=5
	movq	%rcx, df_ref_obstack+16(%rip)
	movq	%r9, (%rax)
	movq	%r14, 8(%rax)
	movq	176(%rsp), %r14         # 8-byte Reload
	movq	40(%r14), %rdx
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	%rax, 8(%rdx,%r12)
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%r8, %rbx
	jne	.LBB7_140
	jmp	.LBB7_149
	.align	16, 0x90
.LBB7_150:                              # %df_bb_reg_use_chain_create.exit.i
                                        #   in Loop: Header=BB7_134 Depth=3
	movq	144(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	160(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	152(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_152
.LBB7_151:                              # %for.inc.i734
                                        #   in Loop: Header=BB7_134 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_134
.LBB7_152:                              # %if.end28.i737
                                        #   in Loop: Header=BB7_132 Depth=2
	movq	120(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_132
	jmp	.LBB7_153
.LBB7_154:                              # %if.end33
	movq	%rbx, %r15
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 144(%r14)
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 152(%r14)
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 160(%r14)
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 176(%r14)
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 168(%r14)
	movslq	n_basic_blocks(%rip), %rdi
	movq	$2, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 184(%r14)
	movq	144(%r14), %rdi
	movq	152(%r14), %rsi
	callq	flow_depth_first_order_compute
	movq	160(%r14), %rdi
	callq	flow_reverse_top_sort_order_compute
	movslq	n_basic_blocks(%rip), %rdi
	testq	%rdi, %rdi
	jle	.LBB7_157
# BB#155:                               # %for.body.lr.ph
	movq	144(%r14), %r8
	movq	152(%r14), %r9
	movq	176(%r14), %r10
	movq	168(%r14), %r11
	movq	160(%r14), %rax
	movq	184(%r14), %rdx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_156:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	(%r8,%rsi,4), %rcx
	movl	%esi, (%r10,%rcx,4)
	movslq	(%r9,%rsi,4), %rcx
	movl	%esi, (%r11,%rcx,4)
	movslq	(%rax,%rsi,4), %rcx
	movl	%esi, (%rdx,%rcx,4)
	incq	%rsi
	movslq	n_basic_blocks(%rip), %rdi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdi, %rbx
	jl	.LBB7_156
.LBB7_157:                              # %for.end
	movq	%r15, %rbx
	movq	112(%rsp), %rax         # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB7_187
# BB#158:                               # %if.then73
	movzbl	(%r14), %eax
	testq	$1, %rax
	movq	64(%rsp), %rax          # 8-byte Reload
	jne	.LBB7_160
# BB#159:                               # %cond.false
	movq	128(%r14), %rax
.LBB7_160:                              # %cond.end
	movq	(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_183
	.align	16, 0x90
.LBB7_161:                              # %for.cond7.preheader.i747
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_162 Depth 2
                                        #       Child Loop BB7_164 Depth 3
                                        #         Child Loop BB7_167 Depth 4
                                        #           Child Loop BB7_170 Depth 5
                                        #             Child Loop BB7_175 Depth 6
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_162:                              # %for.body9.i751
                                        #   Parent Loop BB7_161 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_164 Depth 3
                                        #         Child Loop BB7_167 Depth 4
                                        #           Child Loop BB7_170 Depth 5
                                        #             Child Loop BB7_175 Depth 6
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_180
# BB#163:                               # %for.body14.preheader.i752
                                        #   in Loop: Header=BB7_162 Depth=2
	movq	$6, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_164:                              # %for.body14.i758
                                        #   Parent Loop BB7_161 Depth=1
                                        #     Parent Loop BB7_162 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_167 Depth 4
                                        #           Child Loop BB7_170 Depth 5
                                        #             Child Loop BB7_175 Depth 6
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_179
# BB#165:                               # %if.then16.i769
                                        #   in Loop: Header=BB7_164 Depth=3
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movslq	88(%rcx), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	(%rcx), %rcx
	movq	8(%r14), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB7_178
# BB#166:                               # %land.rhs.lr.ph.i.i
                                        #   in Loop: Header=BB7_164 Depth=3
	movq	152(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %r14
	movq	144(%rsp), %rax         # 8-byte Reload
	leaq	8(%rax,%r14), %r13
	addq	%rax, %r14
	.align	16, 0x90
.LBB7_167:                              # %land.rhs.i.i772
                                        #   Parent Loop BB7_161 Depth=1
                                        #     Parent Loop BB7_162 Depth=2
                                        #       Parent Loop BB7_164 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_170 Depth 5
                                        #             Child Loop BB7_175 Depth 6
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB7_178
# BB#168:                               # %for.body.i.i776
                                        #   in Loop: Header=BB7_167 Depth=4
	movzbl	(%rcx), %eax
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_177
# BB#169:                               # %if.end.i.i779
                                        #   in Loop: Header=BB7_167 Depth=4
	movq	168(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %eax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	56(%rcx), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	(%rcx,%rax,8), %rbx
	jmp	.LBB7_170
	.align	16, 0x90
.LBB7_176:                              # %for.end.i.i
                                        #   in Loop: Header=BB7_170 Depth=5
	movq	(%r13), %rdi
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	36(%rax), %esi
	callq	bitmap_set_bit
	movq	(%rbx), %rbx
.LBB7_170:                              # %if.end.i.i779
                                        #   Parent Loop BB7_161 Depth=1
                                        #     Parent Loop BB7_162 Depth=2
                                        #       Parent Loop BB7_164 Depth=3
                                        #         Parent Loop BB7_167 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB7_175 Depth 6
	testq	%rbx, %rbx
	je	.LBB7_177
# BB#171:                               # %for.body12.i.i
                                        #   in Loop: Header=BB7_170 Depth=5
	movq	8(%rbx), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_173
# BB#172:                               # %cond.true.i.i782
                                        #   in Loop: Header=BB7_170 Depth=5
	movq	8(%rax), %rax
.LBB7_173:                              # %cond.end.i.i785
                                        #   in Loop: Header=BB7_170 Depth=5
	movl	8(%rax), %eax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %r12
	jmp	.LBB7_175
	.align	16, 0x90
.LBB7_174:                              # %for.body29.i.i
                                        #   in Loop: Header=BB7_175 Depth=6
	movq	8(%r12), %r15
	movq	(%r14), %rdi
	movl	36(%r15), %esi
	callq	bitmap_set_bit
	movq	(%r13), %rdi
	movl	36(%r15), %esi
	callq	bitmap_clear_bit
	movq	(%r12), %r12
.LBB7_175:                              # %for.body29.i.i
                                        #   Parent Loop BB7_161 Depth=1
                                        #     Parent Loop BB7_162 Depth=2
                                        #       Parent Loop BB7_164 Depth=3
                                        #         Parent Loop BB7_167 Depth=4
                                        #           Parent Loop BB7_170 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	testq	%r12, %r12
	jne	.LBB7_174
	jmp	.LBB7_176
	.align	16, 0x90
.LBB7_177:                              # %for.inc37.i.i
                                        #   in Loop: Header=BB7_167 Depth=4
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_167
.LBB7_178:                              # %df_bb_rd_local_compute.exit.i
                                        #   in Loop: Header=BB7_164 Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	152(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %rax
	movq	$1, %rcx
	movq	144(%rsp), %rsi         # 8-byte Reload
	movl	%ecx, 96(%rsi,%rax)
	testq	%rdx, %rdx
	movq	176(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_180
.LBB7_179:                              # %for.inc.i793
                                        #   in Loop: Header=BB7_164 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_164
.LBB7_180:                              # %if.end28.i796
                                        #   in Loop: Header=BB7_162 Depth=2
	movq	96(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_162
# BB#181:                               # %for.end31.i799
                                        #   in Loop: Header=BB7_161 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB7_161
# BB#182:                               # %df_rd_local_compute.exit.loopexit
	movl	n_basic_blocks(%rip), %edi
.LBB7_183:                              # %df_rd_local_compute.exit
	movq	%r14, 176(%rsp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r12
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, %r15
	testq	%rdx, %rdx
	jle	.LBB7_186
# BB#184:                               # %for.body94.lr.ph
	movq	basic_block_info(%rip), %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rsi
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB7_185:                              # %for.body94
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	16(%rsi,%rcx), %rcx
	movq	%rcx, (%r14,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	24(%rsi,%rcx), %rcx
	movq	%rcx, (%r12,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	8(%rsi,%rcx), %rcx
	movq	%rcx, (%r13,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	(%rsi,%rcx), %rcx
	movq	%rcx, (%r15,%rdi,8)
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdx, %rbx
	jl	.LBB7_185
.LBB7_186:                              # %for.end133
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	128(%rbx), %r8
	movq	168(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	$df_rd_transfer_function, 8(%rsp)
	xorq	%r9, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	iterative_dataflow_bitmap
	movq	%r14, %rdi
	movq	%rbx, %r14
	callq	free
	movq	%r12, %rdi
	callq	free
	movq	%r13, %rdi
	callq	free
	movq	%r15, %rdi
	callq	free
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB7_187:                              # %if.end136
	xorq	%rax, %rax
	testq	$16, 56(%rsp)           # 8-byte Folded Reload
	je	.LBB7_233
# BB#188:                               # %if.then139
	movq	128(%r14), %rax
	jmp	.LBB7_189
	.align	16, 0x90
.LBB7_231:                              # %for.end31.i900
                                        #   in Loop: Header=BB7_189 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
.LBB7_189:                              # %if.then139
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_191 Depth 2
                                        #       Child Loop BB7_193 Depth 3
                                        #         Child Loop BB7_196 Depth 4
                                        #           Child Loop BB7_199 Depth 5
                                        #             Child Loop BB7_217 Depth 6
                                        #           Child Loop BB7_212 Depth 5
	movq	(%rax), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_232
# BB#190:                               # %for.cond7.preheader.i807
                                        #   in Loop: Header=BB7_189 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_191:                              # %for.body9.i811
                                        #   Parent Loop BB7_189 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_193 Depth 3
                                        #         Child Loop BB7_196 Depth 4
                                        #           Child Loop BB7_199 Depth 5
                                        #             Child Loop BB7_217 Depth 6
                                        #           Child Loop BB7_212 Depth 5
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_230
# BB#192:                               # %for.body14.preheader.i812
                                        #   in Loop: Header=BB7_191 Depth=2
	movq	$6, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_193:                              # %for.body14.i818
                                        #   Parent Loop BB7_189 Depth=1
                                        #     Parent Loop BB7_191 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_196 Depth 4
                                        #           Child Loop BB7_199 Depth 5
                                        #             Child Loop BB7_217 Depth 6
                                        #           Child Loop BB7_212 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_229
# BB#194:                               # %if.then16.i832
                                        #   in Loop: Header=BB7_193 Depth=3
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %r12
	movq	%r12, 144(%rsp)         # 8-byte Spill
	movslq	88(%r12), %r15
	movq	%r14, %rax
	movq	8(%rax), %r14
	movl	96(%rax), %edx
	movq	32(%rax), %rdi
	movq	%rdi, 168(%rsp)         # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movq	(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB7_228
# BB#195:                               # %land.rhs.lr.ph.i.i834
                                        #   in Loop: Header=BB7_193 Depth=3
	imulq	$112, %r15, %rax
	leaq	16(%r14,%rax), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB7_196:                              # %land.rhs.i.i838
                                        #   Parent Loop BB7_189 Depth=1
                                        #     Parent Loop BB7_191 Depth=2
                                        #       Parent Loop BB7_193 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_199 Depth 5
                                        #             Child Loop BB7_217 Depth 6
                                        #           Child Loop BB7_212 Depth 5
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB7_228
# BB#197:                               # %for.body.i.i841
                                        #   in Loop: Header=BB7_196 Depth=4
	movzbl	(%rcx), %eax
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_227
# BB#198:                               # %if.end.i.i844
                                        #   in Loop: Header=BB7_196 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movl	8(%rax), %ecx
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	8(%rax,%rcx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB7_211
	.align	16, 0x90
.LBB7_199:                              # %for.body15.i.i
                                        #   Parent Loop BB7_189 Depth=1
                                        #     Parent Loop BB7_191 Depth=2
                                        #       Parent Loop BB7_193 Depth=3
                                        #         Parent Loop BB7_196 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB7_217 Depth 6
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	8(%rcx), %r15
	movq	(%r15), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_201
# BB#200:                               # %cond.true.i.i847
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	8(%rax), %rax
.LBB7_201:                              # %cond.end.i.i851
                                        #   in Loop: Header=BB7_199 Depth=5
	movl	8(%rax), %eax
	movq	$0, 24(%r15)
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %r14
	testq	%r14, %r14
	jne	.LBB7_202
# BB#216:                               # %if.else.i.i
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %r14
	jmp	.LBB7_217
	.align	16, 0x90
.LBB7_202:                              # %if.then30.i.i
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	df_ref_obstack+32(%rip), %rax
	movq	df_ref_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB7_204
# BB#203:                               # %if.then.i.i.i857
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	$16, %rsi
	movabsq	$df_ref_obstack, %rdi
	callq	_obstack_newchunk
	movq	df_ref_obstack+24(%rip), %rcx
.LBB7_204:                              # %if.end.i.i.i860
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	%rbx, %r8
	addq	$16, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB7_206
# BB#205:                               # %if.then8.i.i.i863
                                        #   in Loop: Header=BB7_199 Depth=5
	movzbl	df_ref_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, df_ref_obstack+80(%rip)
.LBB7_206:                              # %if.end9.i.i.i877
                                        #   in Loop: Header=BB7_199 Depth=5
	movslq	df_ref_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB7_208
# BB#207:                               # %if.then29.i.i.i878
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB7_208:                              # %df_link_create.exit.i.i881
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	%rdx, df_ref_obstack+16(%rip)
	movq	$0, (%rax)
	movq	%r14, 8(%rax)
	movq	%rax, 24(%r15)
	movq	%r8, %rbx
	jmp	.LBB7_209
	.align	16, 0x90
.LBB7_226:                              # %for.inc.i.i886
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	(%r14), %r14
.LBB7_217:                              # %if.else.i.i
                                        #   Parent Loop BB7_189 Depth=1
                                        #     Parent Loop BB7_191 Depth=2
                                        #       Parent Loop BB7_193 Depth=3
                                        #         Parent Loop BB7_196 Depth=4
                                        #           Parent Loop BB7_199 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	testq	%r14, %r14
	je	.LBB7_209
# BB#218:                               # %for.body38.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	8(%r14), %r12
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rdi
	movl	36(%r12), %esi
	callq	bitmap_bit_p
	testq	%rbx, %rax
	je	.LBB7_226
# BB#219:                               # %if.then42.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	df_ref_obstack+32(%rip), %rax
	movq	df_ref_obstack+24(%rip), %rcx
	movq	24(%r15), %r13
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB7_221
# BB#220:                               # %if.then.i124.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	$16, %rsi
	movabsq	$df_ref_obstack, %rdi
	callq	_obstack_newchunk
	movq	df_ref_obstack+24(%rip), %rcx
.LBB7_221:                              # %if.end.i127.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	%rbx, %r8
	addq	$16, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB7_223
# BB#222:                               # %if.then8.i130.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movzbl	df_ref_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, df_ref_obstack+80(%rip)
.LBB7_223:                              # %if.end9.i144.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movslq	df_ref_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB7_225
# BB#224:                               # %if.then29.i145.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB7_225:                              # %df_link_create.exit148.i.i
                                        #   in Loop: Header=BB7_217 Depth=6
	movq	%rdx, df_ref_obstack+16(%rip)
	movq	%r13, (%rax)
	movq	%r12, 8(%rax)
	movq	%rax, 24(%r15)
	movq	%r8, %rbx
	jmp	.LBB7_226
	.align	16, 0x90
.LBB7_209:                              # %for.inc48.i.i
                                        #   in Loop: Header=BB7_199 Depth=5
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_199
# BB#210:                               # %for.cond13.for.end50_crit_edge.i.i
                                        #   in Loop: Header=BB7_196 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
.LBB7_211:                              # %for.end50.i.i
                                        #   in Loop: Header=BB7_196 Depth=4
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	(%rax,%rcx,8), %rax
	jmp	.LBB7_212
	.align	16, 0x90
.LBB7_215:                              # %cond.end72.i.i
                                        #   in Loop: Header=BB7_212 Depth=5
	movslq	8(%rdx), %rdx
	movq	168(%rsp), %rsi         # 8-byte Reload
	movq	%rcx, (%rsi,%rdx,8)
	movq	(%rax), %rax
.LBB7_212:                              # %for.end50.i.i
                                        #   Parent Loop BB7_189 Depth=1
                                        #     Parent Loop BB7_191 Depth=2
                                        #       Parent Loop BB7_193 Depth=3
                                        #         Parent Loop BB7_196 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	testq	%rax, %rax
	je	.LBB7_227
# BB#213:                               # %for.body57.i.i
                                        #   in Loop: Header=BB7_212 Depth=5
	movq	8(%rax), %rcx
	movq	(%rcx), %rdx
	movzbl	(%rdx), %esi
	cmpq	$63, %rsi
	jne	.LBB7_215
# BB#214:                               # %cond.true65.i.i
                                        #   in Loop: Header=BB7_212 Depth=5
	movq	8(%rdx), %rdx
	jmp	.LBB7_215
	.align	16, 0x90
.LBB7_227:                              # %for.inc82.i.i
                                        #   in Loop: Header=BB7_196 Depth=4
	movq	152(%rsp), %rcx         # 8-byte Reload
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_196
.LBB7_228:                              # %df_bb_ud_chain_create.exit.i
                                        #   in Loop: Header=BB7_193 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	128(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	176(%rsp), %r14         # 8-byte Reload
	movq	120(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_230
.LBB7_229:                              # %for.inc.i894
                                        #   in Loop: Header=BB7_193 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_193
.LBB7_230:                              # %if.end28.i897
                                        #   in Loop: Header=BB7_191 Depth=2
	movq	88(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_191
	jmp	.LBB7_231
.LBB7_232:                              # %df_ud_chain_create.exit
	movq	48(%rsp), %rax          # 8-byte Reload
	notq	%rax
	andq	$1, %rax
.LBB7_233:                              # %if.end146
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB7_264
# BB#234:                               # %if.then149
	movzbl	(%r14), %eax
	testq	$2, %rax
	movq	64(%rsp), %rax          # 8-byte Reload
	jne	.LBB7_236
# BB#235:                               # %cond.false154
	movq	128(%r14), %rax
	jmp	.LBB7_236
	.align	16, 0x90
.LBB7_259:                              # %for.end31.i982
                                        #   in Loop: Header=BB7_236 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
.LBB7_236:                              # %cond.end156
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_238 Depth 2
                                        #       Child Loop BB7_240 Depth 3
                                        #         Child Loop BB7_243 Depth 4
                                        #           Child Loop BB7_246 Depth 5
                                        #             Child Loop BB7_250 Depth 6
                                        #           Child Loop BB7_254 Depth 5
	movq	(%rax), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_260
# BB#237:                               # %for.cond7.preheader.i908
                                        #   in Loop: Header=BB7_236 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_238:                              # %for.body9.i912
                                        #   Parent Loop BB7_236 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_240 Depth 3
                                        #         Child Loop BB7_243 Depth 4
                                        #           Child Loop BB7_246 Depth 5
                                        #             Child Loop BB7_250 Depth 6
                                        #           Child Loop BB7_254 Depth 5
	movq	%rcx, 96(%rsp)          # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_258
# BB#239:                               # %for.body14.preheader.i913
                                        #   in Loop: Header=BB7_238 Depth=2
	movq	$6, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_240:                              # %for.body14.i919
                                        #   Parent Loop BB7_236 Depth=1
                                        #     Parent Loop BB7_238 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_243 Depth 4
                                        #           Child Loop BB7_246 Depth 5
                                        #             Child Loop BB7_250 Depth 6
                                        #           Child Loop BB7_254 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_257
# BB#241:                               # %if.then16.i932
                                        #   in Loop: Header=BB7_240 Depth=3
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	104(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 128(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movslq	88(%rcx), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	8(%rcx), %r15
	movq	8(%r14), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	testq	%r15, %r15
	je	.LBB7_256
# BB#242:                               # %land.rhs.lr.ph.i.i933
                                        #   in Loop: Header=BB7_240 Depth=3
	movq	152(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %rax
	movq	144(%rsp), %rcx         # 8-byte Reload
	leaq	40(%rcx,%rax), %r14
	leaq	32(%rcx,%rax), %r13
	.align	16, 0x90
.LBB7_243:                              # %land.rhs.i.i937
                                        #   Parent Loop BB7_236 Depth=1
                                        #     Parent Loop BB7_238 Depth=2
                                        #       Parent Loop BB7_240 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_246 Depth 5
                                        #             Child Loop BB7_250 Depth 6
                                        #           Child Loop BB7_254 Depth 5
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %r15
	je	.LBB7_256
# BB#244:                               # %for.body.i.i942
                                        #   in Loop: Header=BB7_243 Depth=4
	movzbl	(%r15), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_255
# BB#245:                               # %if.end.i.i947
                                        #   in Loop: Header=BB7_243 Depth=4
	movl	8(%r15), %ecx
	movq	%r15, 168(%rsp)         # 8-byte Spill
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	(%rax,%rcx,8), %r15
	testq	%r15, %r15
	je	.LBB7_253
	.align	16, 0x90
.LBB7_246:                              # %for.body12.i.i953
                                        #   Parent Loop BB7_236 Depth=1
                                        #     Parent Loop BB7_238 Depth=2
                                        #       Parent Loop BB7_240 Depth=3
                                        #         Parent Loop BB7_243 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB7_250 Depth 6
	movq	8(%r15), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_248
# BB#247:                               # %cond.true.i.i956
                                        #   in Loop: Header=BB7_246 Depth=5
	movq	8(%rax), %rax
.LBB7_248:                              # %cond.end.i.i962
                                        #   in Loop: Header=BB7_246 Depth=5
	movl	8(%rax), %eax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rbx
	jmp	.LBB7_250
	.align	16, 0x90
.LBB7_249:                              # %for.body28.i.i
                                        #   in Loop: Header=BB7_250 Depth=6
	movq	8(%rbx), %r12
	movq	(%r13), %rdi
	movl	36(%r12), %esi
	callq	bitmap_set_bit
	movq	(%r14), %rdi
	movl	36(%r12), %esi
	callq	bitmap_clear_bit
	movq	(%rbx), %rbx
.LBB7_250:                              # %for.body28.i.i
                                        #   Parent Loop BB7_236 Depth=1
                                        #     Parent Loop BB7_238 Depth=2
                                        #       Parent Loop BB7_240 Depth=3
                                        #         Parent Loop BB7_243 Depth=4
                                        #           Parent Loop BB7_246 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	testq	%rbx, %rbx
	jne	.LBB7_249
# BB#251:                               # %for.inc31.i.i
                                        #   in Loop: Header=BB7_246 Depth=5
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB7_246
# BB#252:                               # %for.cond10.for.end33_crit_edge.i.i
                                        #   in Loop: Header=BB7_243 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
.LBB7_253:                              # %for.end33.i.i
                                        #   in Loop: Header=BB7_243 Depth=4
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	8(%rax,%rcx,8), %rbx
	testq	%rbx, %rbx
	movq	168(%rsp), %r15         # 8-byte Reload
	je	.LBB7_255
	.align	16, 0x90
.LBB7_254:                              # %for.body40.i.i
                                        #   Parent Loop BB7_236 Depth=1
                                        #     Parent Loop BB7_238 Depth=2
                                        #       Parent Loop BB7_240 Depth=3
                                        #         Parent Loop BB7_243 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rbx), %rax
	movq	(%r14), %rdi
	movl	36(%rax), %esi
	callq	bitmap_set_bit
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_254
.LBB7_255:                              # %for.inc48.i.i971
                                        #   in Loop: Header=BB7_243 Depth=4
	movq	16(%r15), %r15
	testq	%r15, %r15
	jne	.LBB7_243
.LBB7_256:                              # %df_bb_ru_local_compute.exit.i
                                        #   in Loop: Header=BB7_240 Depth=3
	movq	120(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	136(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	152(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %rax
	movq	$1, %rcx
	movq	144(%rsp), %rsi         # 8-byte Reload
	movl	%ecx, 100(%rsi,%rax)
	testq	%rdx, %rdx
	movq	176(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	128(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_258
.LBB7_257:                              # %for.inc.i976
                                        #   in Loop: Header=BB7_240 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_240
.LBB7_258:                              # %if.end28.i979
                                        #   in Loop: Header=BB7_238 Depth=2
	movq	96(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_238
	jmp	.LBB7_259
.LBB7_260:                              # %df_ru_local_compute.exit
	movq	%r14, 176(%rsp)         # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r15
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, %r12
	testq	%rdx, %rdx
	jle	.LBB7_263
# BB#261:                               # %for.body178.lr.ph
	movq	basic_block_info(%rip), %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rsi
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB7_262:                              # %for.body178
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	48(%rsi,%rcx), %rcx
	movq	%rcx, (%r14,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	56(%rsi,%rcx), %rcx
	movq	%rcx, (%r13,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	40(%rsi,%rcx), %rcx
	movq	%rcx, (%r15,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	32(%rsi,%rcx), %rcx
	movq	%rcx, (%r12,%rdi,8)
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdx, %rbx
	jl	.LBB7_262
.LBB7_263:                              # %for.end221
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	128(%rbx), %r8
	movq	184(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	$df_ru_transfer_function, 8(%rsp)
	movq	$1, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	callq	iterative_dataflow_bitmap
	movq	%r14, %rdi
	movq	%rbx, %r14
	callq	free
	movq	%r13, %rdi
	callq	free
	movq	%r15, %rdi
	callq	free
	movq	%r12, %rdi
	callq	free
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB7_264:                              # %if.end224
	testq	$8, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB7_265
# BB#266:                               # %if.then227
	movq	%rbx, %r15
	movq	128(%r14), %rbx
	movq	%r14, 176(%rsp)         # 8-byte Spill
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	(%rbx), %rcx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	%r15, %rbx
	movq	%rax, %r14
	movq	%r14, 168(%rsp)         # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB7_298
	.align	16, 0x90
.LBB7_267:                              # %for.cond8.preheader.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_268 Depth 2
                                        #       Child Loop BB7_270 Depth 3
                                        #         Child Loop BB7_272 Depth 4
                                        #           Child Loop BB7_276 Depth 5
                                        #             Child Loop BB7_279 Depth 6
                                        #           Child Loop BB7_292 Depth 5
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_268:                              # %for.body10.i
                                        #   Parent Loop BB7_267 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_270 Depth 3
                                        #         Child Loop BB7_272 Depth 4
                                        #           Child Loop BB7_276 Depth 5
                                        #             Child Loop BB7_279 Depth 6
                                        #           Child Loop BB7_292 Depth 5
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_296
# BB#269:                               # %for.body15.preheader.i
                                        #   in Loop: Header=BB7_268 Depth=2
	movq	$6, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_270:                              # %for.body15.i
                                        #   Parent Loop BB7_267 Depth=1
                                        #     Parent Loop BB7_268 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_272 Depth 4
                                        #           Child Loop BB7_276 Depth 5
                                        #             Child Loop BB7_279 Depth 6
                                        #           Child Loop BB7_292 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_295
# BB#271:                               # %if.then17.i
                                        #   in Loop: Header=BB7_270 Depth=3
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	%rdx, 128(%rsp)         # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 120(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	%rbx, %r15
	movq	32(%rcx,%rdx,8), %rbx
	movq	%rbx, 152(%rsp)         # 8-byte Spill
	movslq	88(%rbx), %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	imulq	$112, %rax, %rax
	movq	56(%rcx,%rax), %rsi
	movq	%r14, %rdi
	callq	bitmap_copy
	movq	8(%rbx), %r12
	jmp	.LBB7_272
	.align	16, 0x90
.LBB7_293:                              # %for.inc53.i.i
                                        #   in Loop: Header=BB7_272 Depth=4
	movq	16(%r12), %r12
.LBB7_272:                              # %if.then17.i
                                        #   Parent Loop BB7_267 Depth=1
                                        #     Parent Loop BB7_268 Depth=2
                                        #       Parent Loop BB7_270 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_276 Depth 5
                                        #             Child Loop BB7_279 Depth 6
                                        #           Child Loop BB7_292 Depth 5
	testq	%r12, %r12
	je	.LBB7_294
# BB#273:                               # %land.rhs.i.i1005
                                        #   in Loop: Header=BB7_272 Depth=4
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %r12
	je	.LBB7_294
# BB#274:                               # %for.body.i.i1010
                                        #   in Loop: Header=BB7_272 Depth=4
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_293
# BB#275:                               # %if.end.i.i1015
                                        #   in Loop: Header=BB7_272 Depth=4
	movl	8(%r12), %ecx
	movq	%r12, 144(%rsp)         # 8-byte Spill
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	(%rax,%rcx,8), %rcx
	testq	%rcx, %rcx
	movq	%r15, %r12
	je	.LBB7_291
	.align	16, 0x90
.LBB7_276:                              # %for.body12.i.i1021
                                        #   Parent Loop BB7_267 Depth=1
                                        #     Parent Loop BB7_268 Depth=2
                                        #       Parent Loop BB7_270 Depth=3
                                        #         Parent Loop BB7_272 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB7_279 Depth 6
	movq	%rcx, 160(%rsp)         # 8-byte Spill
	movq	8(%rcx), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_278
# BB#277:                               # %cond.true.i.i1024
                                        #   in Loop: Header=BB7_276 Depth=5
	movq	8(%rax), %rax
.LBB7_278:                              # %cond.end.i.i1031
                                        #   in Loop: Header=BB7_276 Depth=5
	movl	8(%rax), %eax
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	$0, 24(%rcx)
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	40(%rcx), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %r13
	jmp	.LBB7_279
	.align	16, 0x90
.LBB7_288:                              # %for.inc.i.i1069
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	(%r13), %r13
.LBB7_279:                              # %cond.end.i.i1031
                                        #   Parent Loop BB7_267 Depth=1
                                        #     Parent Loop BB7_268 Depth=2
                                        #       Parent Loop BB7_270 Depth=3
                                        #         Parent Loop BB7_272 Depth=4
                                        #           Parent Loop BB7_276 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	testq	%r13, %r13
	je	.LBB7_289
# BB#280:                               # %for.body28.i.i1035
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	8(%r13), %r15
	movl	36(%r15), %esi
	movq	%r14, %rdi
	callq	bitmap_bit_p
	testq	%r12, %rax
	je	.LBB7_288
# BB#281:                               # %if.then31.i.i
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	df_ref_obstack+32(%rip), %rax
	movq	df_ref_obstack+24(%rip), %rcx
	movq	184(%rsp), %rdx         # 8-byte Reload
	movq	24(%rdx), %r14
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB7_283
# BB#282:                               # %if.then.i.i.i1041
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	$16, %rsi
	movabsq	$df_ref_obstack, %rdi
	callq	_obstack_newchunk
	movq	df_ref_obstack+24(%rip), %rcx
.LBB7_283:                              # %if.end.i.i.i1044
                                        #   in Loop: Header=BB7_279 Depth=6
	addq	$16, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB7_285
# BB#284:                               # %if.then8.i.i.i1047
                                        #   in Loop: Header=BB7_279 Depth=6
	movzbl	df_ref_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, df_ref_obstack+80(%rip)
.LBB7_285:                              # %if.end9.i.i.i1061
                                        #   in Loop: Header=BB7_279 Depth=6
	movslq	df_ref_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB7_287
# BB#286:                               # %if.then29.i.i.i1062
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB7_287:                              # %df_link_create.exit.i.i1065
                                        #   in Loop: Header=BB7_279 Depth=6
	movq	%rdx, df_ref_obstack+16(%rip)
	movq	%r14, (%rax)
	movq	%r15, 8(%rax)
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	%rax, 24(%rcx)
	movl	36(%r15), %esi
	movq	168(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	bitmap_clear_bit
	movq	%rbx, %r14
	jmp	.LBB7_288
	.align	16, 0x90
.LBB7_289:                              # %for.inc37.i.i1072
                                        #   in Loop: Header=BB7_276 Depth=5
	movq	160(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB7_276
# BB#290:                               # %for.cond10.for.end39_crit_edge.i.i
                                        #   in Loop: Header=BB7_272 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
.LBB7_291:                              # %for.end39.i.i
                                        #   in Loop: Header=BB7_272 Depth=4
	movq	%r12, %r15
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	8(%rax,%rcx,8), %rbx
	testq	%rbx, %rbx
	movq	144(%rsp), %r12         # 8-byte Reload
	je	.LBB7_293
	.align	16, 0x90
.LBB7_292:                              # %for.body46.i.i
                                        #   Parent Loop BB7_267 Depth=1
                                        #     Parent Loop BB7_268 Depth=2
                                        #       Parent Loop BB7_270 Depth=3
                                        #         Parent Loop BB7_272 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%rbx), %rax
	movl	36(%rax), %esi
	movq	%r14, %rdi
	callq	bitmap_set_bit
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_292
	jmp	.LBB7_293
	.align	16, 0x90
.LBB7_294:                              # %df_bb_du_chain_create.exit.i
                                        #   in Loop: Header=BB7_270 Depth=3
	movq	112(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	128(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	%r15, %rbx
	movq	120(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_296
.LBB7_295:                              # %for.inc.i1080
                                        #   in Loop: Header=BB7_270 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_270
.LBB7_296:                              # %if.end29.i1082
                                        #   in Loop: Header=BB7_268 Depth=2
	movq	88(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_268
# BB#297:                               # %for.end32.i
                                        #   in Loop: Header=BB7_267 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB7_267
.LBB7_298:                              # %do.body37.i
	testq	%r14, %r14
	je	.LBB7_300
# BB#299:                               # %if.then38.i
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	%r14, %rdi
	callq	free
.LBB7_300:                              # %df_du_chain_create.exit
	movq	40(%rsp), %rax          # 8-byte Reload
	xorq	$2, %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	orq	%rax, %rcx
	movq	%rcx, %rsi
	movq	176(%rsp), %r14         # 8-byte Reload
	jmp	.LBB7_301
.LBB7_265:
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB7_301:                              # %if.end234
	testq	%rbx, %rsi
	je	.LBB7_303
# BB#302:                               # %if.then236
	movq	%r14, %rdi
	callq	df_bitmaps_free
.LBB7_303:                              # %if.end237
	movq	72(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB7_335
# BB#304:                               # %if.then240
	movzbl	(%r14), %eax
	testq	$4, %rax
	jne	.LBB7_306
# BB#305:                               # %cond.false245
	movq	128(%r14), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
.LBB7_306:                              # %cond.end247
	movq	64(%rsp), %rax          # 8-byte Reload
	jmp	.LBB7_307
	.align	16, 0x90
.LBB7_330:                              # %for.end31.i1166
                                        #   in Loop: Header=BB7_307 Depth=1
	movq	120(%rsp), %rax         # 8-byte Reload
.LBB7_307:                              # %cond.end247
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_309 Depth 2
                                        #       Child Loop BB7_311 Depth 3
                                        #         Child Loop BB7_314 Depth 4
                                        #           Child Loop BB7_317 Depth 5
                                        #           Child Loop BB7_322 Depth 5
	movq	(%rax), %rax
	movq	%rax, 120(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB7_331
# BB#308:                               # %for.cond7.preheader.i1092
                                        #   in Loop: Header=BB7_307 Depth=1
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_309:                              # %for.body9.i1096
                                        #   Parent Loop BB7_307 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_311 Depth 3
                                        #         Child Loop BB7_314 Depth 4
                                        #           Child Loop BB7_317 Depth 5
                                        #           Child Loop BB7_322 Depth 5
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_329
# BB#310:                               # %for.body14.preheader.i1097
                                        #   in Loop: Header=BB7_309 Depth=2
	movq	$6, %rcx
	movq	104(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_311:                              # %for.body14.i1103
                                        #   Parent Loop BB7_307 Depth=1
                                        #     Parent Loop BB7_309 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_314 Depth 4
                                        #           Child Loop BB7_317 Depth 5
                                        #           Child Loop BB7_322 Depth 5
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_328
# BB#312:                               # %if.then16.i1116
                                        #   in Loop: Header=BB7_311 Depth=3
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	%rdx, 144(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	112(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 136(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	movslq	88(%rcx), %rax
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	8(%rcx), %r13
	movq	8(%r14), %rax
	movq	%rax, 152(%rsp)         # 8-byte Spill
	testq	%r13, %r13
	je	.LBB7_327
# BB#313:                               # %land.rhs.lr.ph.i.i1117
                                        #   in Loop: Header=BB7_311 Depth=3
	movq	160(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %rax
	movq	152(%rsp), %rcx         # 8-byte Reload
	leaq	72(%rcx,%rax), %r12
	leaq	64(%rcx,%rax), %r14
	.align	16, 0x90
.LBB7_314:                              # %land.rhs.i.i1121
                                        #   Parent Loop BB7_307 Depth=1
                                        #     Parent Loop BB7_309 Depth=2
                                        #       Parent Loop BB7_311 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_317 Depth 5
                                        #           Child Loop BB7_322 Depth 5
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %r13
	je	.LBB7_327
# BB#315:                               # %for.body.i.i1126
                                        #   in Loop: Header=BB7_314 Depth=4
	movzbl	(%r13), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_326
# BB#316:                               # %if.end.i.i1131
                                        #   in Loop: Header=BB7_314 Depth=4
	movl	8(%r13), %ecx
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	(%rax,%rcx,8), %r15
	testq	%r15, %r15
	je	.LBB7_321
	.align	16, 0x90
.LBB7_317:                              # %for.body12.i.i1137
                                        #   Parent Loop BB7_307 Depth=1
                                        #     Parent Loop BB7_309 Depth=2
                                        #       Parent Loop BB7_311 Depth=3
                                        #         Parent Loop BB7_314 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%r15), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_319
# BB#318:                               # %cond.true.i.i1140
                                        #   in Loop: Header=BB7_317 Depth=5
	movq	8(%rax), %rax
.LBB7_319:                              # %cond.end.i.i1147
                                        #   in Loop: Header=BB7_317 Depth=5
	movq	(%r14), %rdi
	movl	8(%rax), %ebx
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movq	(%r12), %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB7_317
# BB#320:                               # %for.cond10.for.end_crit_edge.i.i
                                        #   in Loop: Header=BB7_314 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
.LBB7_321:                              # %for.end.i.i1150
                                        #   in Loop: Header=BB7_314 Depth=4
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	8(%rax,%rcx,8), %rbx
	jmp	.LBB7_322
	.align	16, 0x90
.LBB7_325:                              # %cond.end44.i.i
                                        #   in Loop: Header=BB7_322 Depth=5
	movl	8(%rax), %esi
	callq	bitmap_set_bit
	movq	(%rbx), %rbx
.LBB7_322:                              # %for.end.i.i1150
                                        #   Parent Loop BB7_307 Depth=1
                                        #     Parent Loop BB7_309 Depth=2
                                        #       Parent Loop BB7_311 Depth=3
                                        #         Parent Loop BB7_314 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	testq	%rbx, %rbx
	je	.LBB7_326
# BB#323:                               # %for.body29.i.i1152
                                        #   in Loop: Header=BB7_322 Depth=5
	movq	8(%rbx), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	movq	(%r12), %rdi
	cmpq	$63, %rcx
	jne	.LBB7_325
# BB#324:                               # %cond.true37.i.i
                                        #   in Loop: Header=BB7_322 Depth=5
	movq	8(%rax), %rax
	jmp	.LBB7_325
	.align	16, 0x90
.LBB7_326:                              # %for.inc52.i.i
                                        #   in Loop: Header=BB7_314 Depth=4
	movq	16(%r13), %r13
	testq	%r13, %r13
	jne	.LBB7_314
.LBB7_327:                              # %df_bb_lr_local_compute.exit.i
                                        #   in Loop: Header=BB7_311 Depth=3
	movq	128(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	144(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	160(%rsp), %rax         # 8-byte Reload
	imulq	$112, %rax, %rax
	movq	$1, %rcx
	movq	152(%rsp), %rsi         # 8-byte Reload
	movl	%ecx, 104(%rsi,%rax)
	testq	%rdx, %rdx
	movq	176(%rsp), %r14         # 8-byte Reload
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	136(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_329
.LBB7_328:                              # %for.inc.i1160
                                        #   in Loop: Header=BB7_311 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_311
.LBB7_329:                              # %if.end28.i1163
                                        #   in Loop: Header=BB7_309 Depth=2
	movq	104(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_309
	jmp	.LBB7_330
.LBB7_331:                              # %df_lr_local_compute.exit
	movq	%r14, 176(%rsp)         # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r14
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r12
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdi
	movq	%rax, %r13
	movq	$3, %rcx
	shlq	%cl, %rdi
	callq	xmalloc
	movslq	n_basic_blocks(%rip), %rdx
	movq	%rax, %r15
	testq	%rdx, %rdx
	jle	.LBB7_334
# BB#332:                               # %for.body267.lr.ph
	movq	basic_block_info(%rip), %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rsi
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB7_333:                              # %for.body267
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	80(%rsi,%rcx), %rcx
	movq	%rcx, (%r14,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	88(%rsi,%rcx), %rcx
	movq	%rcx, (%r12,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	72(%rsi,%rcx), %rcx
	movq	%rcx, (%r13,%rdi,8)
	movq	32(%rax,%rdi,8), %rcx
	movslq	88(%rcx), %rcx
	imulq	$112, %rcx, %rcx
	movq	64(%rsi,%rcx), %rcx
	movq	%rcx, (%r15,%rdi,8)
	incq	%rdi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdx, %rbx
	jl	.LBB7_333
.LBB7_334:                              # %for.end310
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	128(%rbx), %r8
	movq	184(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	$0, 24(%rsp)
	movq	$df_lr_transfer_function, 8(%rsp)
	movq	$1, %r9
	movq	$0, (%rsp)
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	movq	%r15, %rcx
	callq	iterative_dataflow_bitmap
	movq	%r14, %rdi
	movq	%rbx, %r14
	callq	free
	movq	%r12, %rdi
	callq	free
	movq	%r13, %rdi
	callq	free
	movq	%r15, %rdi
	callq	free
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
.LBB7_335:                              # %if.end313
	movq	%r14, 176(%rsp)         # 8-byte Spill
	testq	$32, 56(%rsp)           # 8-byte Folded Reload
	je	.LBB7_373
# BB#336:                               # %if.then316
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	%rbx, %r14
	movq	128(%rax), %rbx
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	(%rbx), %rcx
	movq	%rcx, 136(%rsp)         # 8-byte Spill
	movq	%r14, %rbx
	movq	%rax, %r14
	testq	%rcx, %rcx
	je	.LBB7_371
.LBB7_337:                              # %for.cond8.preheader.i1178
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_338 Depth 2
                                        #       Child Loop BB7_340 Depth 3
                                        #         Child Loop BB7_342 Depth 4
                                        #           Child Loop BB7_346 Depth 5
                                        #           Child Loop BB7_351 Depth 5
                                        #           Child Loop BB7_356 Depth 5
                                        #             Child Loop BB7_357 Depth 6
                                        #             Child Loop BB7_362 Depth 6
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB7_338:                              # %for.body10.i1182
                                        #   Parent Loop BB7_337 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB7_340 Depth 3
                                        #         Child Loop BB7_342 Depth 4
                                        #           Child Loop BB7_346 Depth 5
                                        #           Child Loop BB7_351 Depth 5
                                        #           Child Loop BB7_356 Depth 5
                                        #             Child Loop BB7_357 Depth 6
                                        #             Child Loop BB7_362 Depth 6
	movq	%rcx, 120(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	24(%rax,%rcx,8), %rdx
	testq	%rdx, %rdx
	je	.LBB7_369
# BB#339:                               # %for.body15.preheader.i1183
                                        #   in Loop: Header=BB7_338 Depth=2
	movq	$6, %rcx
	movq	120(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB7_340:                              # %for.body15.i1189
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB7_342 Depth 4
                                        #           Child Loop BB7_346 Depth 5
                                        #           Child Loop BB7_351 Depth 5
                                        #           Child Loop BB7_356 Depth 5
                                        #             Child Loop BB7_357 Depth 6
                                        #             Child Loop BB7_362 Depth 6
	movq	$1, %rax
	movq	%rsi, %rcx
	shlq	%cl, %rax
	testq	%rdx, %rax
	je	.LBB7_368
# BB#341:                               # %if.then17.i1202
                                        #   in Loop: Header=BB7_340 Depth=3
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	%rdx, 160(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movq	basic_block_info(%rip), %rcx
	movq	128(%rsp), %rdx         # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	movq	%rsi, 152(%rsp)         # 8-byte Spill
	addq	%rax, %rdx
	andq	%rbx, %rdx
	movq	32(%rcx,%rdx,8), %r15
	movq	%r15, 184(%rsp)         # 8-byte Spill
	movslq	88(%r15), %rax
	movq	176(%rsp), %rdx         # 8-byte Reload
	movq	8(%rdx), %rcx
	movq	40(%rdx), %r13
	imulq	$112, %rax, %rax
	movq	88(%rcx,%rax), %rsi
	movq	%r14, %rdi
	callq	bitmap_copy
	movq	8(%r15), %r12
	jmp	.LBB7_342
	.align	16, 0x90
.LBB7_366:                              # %for.inc112.i.i
                                        #   in Loop: Header=BB7_342 Depth=4
	movq	16(%r12), %r12
.LBB7_342:                              # %if.then17.i1202
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB7_346 Depth 5
                                        #           Child Loop BB7_351 Depth 5
                                        #           Child Loop BB7_356 Depth 5
                                        #             Child Loop BB7_357 Depth 6
                                        #             Child Loop BB7_362 Depth 6
	testq	%r12, %r12
	je	.LBB7_367
# BB#343:                               # %land.rhs.i.i1206
                                        #   in Loop: Header=BB7_342 Depth=4
	movq	184(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	16(%rax), %rax
	cmpq	%rax, %r12
	je	.LBB7_367
# BB#344:                               # %for.body.i.i1211
                                        #   in Loop: Header=BB7_342 Depth=4
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB7_366
# BB#345:                               # %if.end.i.i1216
                                        #   in Loop: Header=BB7_342 Depth=4
	movl	8(%r12), %ecx
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rcx, 168(%rsp)         # 8-byte Spill
	movq	(%rax,%rcx,8), %r15
	testq	%r15, %r15
	je	.LBB7_350
	.align	16, 0x90
.LBB7_346:                              # %for.body12.i.i1222
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        #         Parent Loop BB7_342 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	8(%r15), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_348
# BB#347:                               # %cond.true.i.i1225
                                        #   in Loop: Header=BB7_346 Depth=5
	movq	8(%rax), %rax
.LBB7_348:                              # %cond.end.i.i1234
                                        #   in Loop: Header=BB7_346 Depth=5
	movl	8(%rax), %ebx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	bitmap_clear_bit
	movq	$5, %rcx
	shlq	%cl, %rbx
	movl	20(%r13,%rbx), %eax
	incq	%rax
	movl	%eax, 20(%r13,%rbx)
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB7_346
# BB#349:                               # %for.cond10.for.end_crit_edge.i.i1236
                                        #   in Loop: Header=BB7_342 Depth=4
	movq	176(%rsp), %rax         # 8-byte Reload
	movq	56(%rax), %rax
.LBB7_350:                              # %for.end.i.i1238
                                        #   in Loop: Header=BB7_342 Depth=4
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	8(%rax,%rcx,8), %r15
	jmp	.LBB7_351
	.align	16, 0x90
.LBB7_354:                              # %cond.end45.i.i
                                        #   in Loop: Header=BB7_351 Depth=5
	movl	8(%rax), %ebx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	bitmap_set_bit
	movq	$5, %rcx
	shlq	%cl, %rbx
	movl	24(%r13,%rbx), %eax
	incq	%rax
	movl	%eax, 24(%r13,%rbx)
	movq	(%r15), %r15
.LBB7_351:                              # %for.end.i.i1238
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        #         Parent Loop BB7_342 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	testq	%r15, %r15
	je	.LBB7_355
# BB#352:                               # %for.body31.i.i
                                        #   in Loop: Header=BB7_351 Depth=5
	movq	8(%r15), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB7_354
# BB#353:                               # %cond.true38.i.i
                                        #   in Loop: Header=BB7_351 Depth=5
	movq	8(%rax), %rax
	jmp	.LBB7_354
	.align	16, 0x90
.LBB7_355:                              # %do.body.i.i
                                        #   in Loop: Header=BB7_342 Depth=4
	movq	(%r14), %r8
	testq	%r8, %r8
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	je	.LBB7_366
	.align	16, 0x90
.LBB7_356:                              # %for.cond74.preheader.i.i
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        #         Parent Loop BB7_342 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB7_357 Depth 6
                                        #             Child Loop BB7_362 Depth 6
	movq	24(%r8), %rsi
	testq	%rsi, %rsi
	movq	$1, %rdi
	je	.LBB7_361
	.align	16, 0x90
.LBB7_357:                              # %for.body86.i.i
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        #         Parent Loop BB7_342 Depth=4
                                        #           Parent Loop BB7_356 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	%rbx, %r9
	leaq	-1(%rdi), %rax
	movq	$1, %rbx
	movq	%rax, %rcx
	shlq	%cl, %rbx
	testq	%rsi, %rbx
	jne	.LBB7_359
# BB#358:                               #   in Loop: Header=BB7_357 Depth=6
	movq	%r9, %rbx
	jmp	.LBB7_360
	.align	16, 0x90
.LBB7_359:                              # %if.then89.i.i
                                        #   in Loop: Header=BB7_357 Depth=6
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rax, %rdx
	andq	%r9, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	16(%r13,%rdx), %eax
	notq	%rbx
	incq	%rax
	andq	%rbx, %rsi
	movq	%r9, %rbx
	movl	%eax, 16(%r13,%rdx)
	je	.LBB7_361
.LBB7_360:                              # %for.inc102.i.i
                                        #   in Loop: Header=BB7_357 Depth=6
	movq	%rdi, %rax
	andq	%rbx, %rax
	incq	%rdi
	cmpq	$64, %rax
	jb	.LBB7_357
.LBB7_361:                              # %if.end105.i.i
                                        #   in Loop: Header=BB7_356 Depth=5
	movq	32(%r8), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB7_365
	.align	16, 0x90
.LBB7_362:                              # %for.body86.1.i.i
                                        #   Parent Loop BB7_337 Depth=1
                                        #     Parent Loop BB7_338 Depth=2
                                        #       Parent Loop BB7_340 Depth=3
                                        #         Parent Loop BB7_342 Depth=4
                                        #           Parent Loop BB7_356 Depth=5
                                        # =>          This Inner Loop Header: Depth=6
	movq	$1, %rdi
	movq	%rax, %rcx
	shlq	%cl, %rdi
	testq	%rsi, %rdi
	je	.LBB7_364
# BB#363:                               # %if.then89.1.i.i
                                        #   in Loop: Header=BB7_362 Depth=6
	movl	16(%r8), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%rax,%rdx), %rdx
	andq	%rbx, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	16(%r13,%rdx), %ecx
	notq	%rdi
	incq	%rcx
	andq	%rdi, %rsi
	movl	%ecx, 16(%r13,%rdx)
	je	.LBB7_365
.LBB7_364:                              # %for.inc102.1.i.i
                                        #   in Loop: Header=BB7_362 Depth=6
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$64, %rcx
	jb	.LBB7_362
.LBB7_365:                              # %if.end105.1.i.i
                                        #   in Loop: Header=BB7_356 Depth=5
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB7_356
	jmp	.LBB7_366
	.align	16, 0x90
.LBB7_367:                              # %df_bb_reg_info_compute.exit.i
                                        #   in Loop: Header=BB7_340 Depth=3
	movq	144(%rsp), %rax         # 8-byte Reload
	notq	%rax
	movq	160(%rsp), %rdx         # 8-byte Reload
	andq	%rax, %rdx
	movq	152(%rsp), %rsi         # 8-byte Reload
	je	.LBB7_369
.LBB7_368:                              # %for.inc.i1248
                                        #   in Loop: Header=BB7_340 Depth=3
	incq	%rsi
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	$64, %rax
	jb	.LBB7_340
.LBB7_369:                              # %if.end29.i1251
                                        #   in Loop: Header=BB7_338 Depth=2
	movq	120(%rsp), %rcx         # 8-byte Reload
	incq	%rcx
	cmpq	$2, %rcx
	jne	.LBB7_338
# BB#370:                               # %for.end32.i1255
                                        #   in Loop: Header=BB7_337 Depth=1
	movq	136(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB7_337
.LBB7_371:                              # %do.body37.i1257
	testq	%r14, %r14
	je	.LBB7_373
# BB#372:                               # %if.then38.i1258
	movq	%r14, %rdi
	callq	bitmap_clear
	movq	%r14, %rdi
	callq	free
.LBB7_373:                              # %if.end318
	movq	176(%rsp), %rbx         # 8-byte Reload
	movq	144(%rbx), %rdi
	callq	free
	movq	152(%rbx), %rdi
	callq	free
	movq	160(%rbx), %rdi
	callq	free
	movq	168(%rbx), %rdi
	callq	free
	movq	176(%rbx), %rdi
	callq	free
	movq	184(%rbx), %rdi
	addq	$192, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp75:
	.size	df_analyse_1, .Ltmp75-df_analyse_1
	.cfi_endproc

	.globl	df_finish
	.align	16, 0x90
	.type	df_finish,@function
df_finish:                              # @df_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 16
.Ltmp79:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	df_free
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.Ltmp80:
	.size	df_finish, .Ltmp80-df_finish
	.cfi_endproc

	.globl	df_insn_delete
	.align	16, 0x90
	.type	df_insn_delete,@function
df_insn_delete:                         # @df_insn_delete
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 32
.Ltmp88:
	.cfi_offset %rbx, -32
.Ltmp89:
	.cfi_offset %r14, -24
.Ltmp90:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	(%r14), %rax
	movq	%rdx, %rbx
	movq	%rdi, %r15
	cmpq	%rbx, %rax
	je	.LBB9_2
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	delete_insn
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	df_insn_modify
	movq	24(%rbx), %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB9_2:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_insn_delete, %rcx
	movq	$2434, %rsi             # imm = 0x982
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp91:
	.size	df_insn_delete, .Ltmp91-df_insn_delete
	.cfi_endproc

	.globl	df_insn_modify
	.align	16, 0x90
	.type	df_insn_modify,@function
df_insn_modify:                         # @df_insn_modify
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 48
.Ltmp102:
	.cfi_offset %rbx, -40
.Ltmp103:
	.cfi_offset %r12, -32
.Ltmp104:
	.cfi_offset %r14, -24
.Ltmp105:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	8(%rdx), %r14d
	movl	64(%rbx), %eax
	movq	%rsi, %r15
	cmpq	%rax, %r14
	jae	.LBB10_2
# BB#1:                                 # %entry.if.end_crit_edge
	leaq	112(%rbx), %r12
	jmp	.LBB10_4
.LBB10_2:                               # %if.then
	movq	$2, %rcx
	movq	%rax, %r12
	shrq	%cl, %r12
	addq	%rax, %r12
	movq	56(%rbx), %rdi
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, 56(%rbx)
	movl	64(%rbx), %ecx
	leaq	(%rcx,%rcx,2), %rdx
	leaq	(%rax,%rdx,8), %rdi
	movq	%r12, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%r12d, 64(%rbx)
	cmpq	$0, 112(%rbx)
	leaq	112(%rbx), %r12
	jne	.LBB10_4
# BB#3:                                 # %if.then10.i
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, (%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
.LBB10_4:                               # %if.end
	movq	120(%rbx), %rdi
	movl	88(%r15), %esi
	callq	bitmap_set_bit
	movq	(%r12), %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	bitmap_set_bit  # TAILCALL
.Ltmp106:
	.size	df_insn_modify, .Ltmp106-df_insn_modify
	.cfi_endproc

	.globl	df_insn_mem_replace
	.align	16, 0x90
	.type	df_insn_mem_replace,@function
df_insn_mem_replace:                    # @df_insn_mem_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp114:
	.cfi_def_cfa_offset 80
.Ltmp115:
	.cfi_offset %rbx, -32
.Ltmp116:
	.cfi_offset %r14, -24
.Ltmp117:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%rdx, 40(%rsp)
	movq	%rdx, 24(%rsp)
	movq	%rcx, 8(%rsp)
	movq	%r8, 16(%rsp)
	xorq	%rbx, %rbx
	movl	%ebx, 32(%rsp)
	movabsq	$df_rtx_mem_replace, %rsi
	leaq	40(%rsp), %rdi
	leaq	8(%rsp), %rdx
	callq	for_each_rtx
	movl	32(%rsp), %eax
	testq	%rax, %rax
	je	.LBB11_2
# BB#1:                                 # %if.then
	movq	40(%rsp), %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	df_insn_modify
	movl	32(%rsp), %ebx
.LBB11_2:                               # %if.end
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp118:
	.size	df_insn_mem_replace, .Ltmp118-df_insn_mem_replace
	.cfi_endproc

	.align	16, 0x90
	.type	df_rtx_mem_replace,@function
df_rtx_mem_replace:                     # @df_rtx_mem_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp123:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp124:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp125:
	.cfi_def_cfa_offset 32
.Ltmp126:
	.cfi_offset %rbx, -32
.Ltmp127:
	.cfi_offset %r14, -24
.Ltmp128:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	%rsi, %r14
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB12_6
# BB#1:                                 # %if.end
	movzbl	(%rax), %ecx
	cmpq	$55, %rcx
	jne	.LBB12_3
# BB#2:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB12_6
.LBB12_3:                               # %if.end
	cmpq	$66, %rcx
	jne	.LBB12_6
# BB#4:                                 # %sw.epilog
	movq	(%r14), %rdi
	movq	%rax, %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_6
# BB#5:                                 # %if.end2
	movq	8(%r14), %rdx
	movq	16(%r14), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	callq	validate_change
	movl	24(%r14), %eax
	incq	%rax
	movl	%eax, 24(%r14)
.LBB12_6:                               # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp129:
	.size	df_rtx_mem_replace, .Ltmp129-df_rtx_mem_replace
	.cfi_endproc

	.globl	df_refs_reg_replace
	.align	16, 0x90
	.type	df_refs_reg_replace,@function
df_refs_reg_replace:                    # @df_refs_reg_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp136:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp137:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp138:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp139:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp140:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp141:
	.cfi_def_cfa_offset 96
.Ltmp142:
	.cfi_offset %rbx, -48
.Ltmp143:
	.cfi_offset %r12, -40
.Ltmp144:
	.cfi_offset %r13, -32
.Ltmp145:
	.cfi_offset %r14, -24
.Ltmp146:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rsi, %r13
	movq	%rdi, (%rsp)            # 8-byte Spill
	testq	%r13, %r13
	jne	.LBB13_2
# BB#1:                                 # %if.then
	movq	(%rsp), %rax            # 8-byte Reload
	movq	128(%rax), %r13
.LBB13_2:                               # %if.end
	movq	%r15, 16(%rsp)
	movq	%r8, 24(%rsp)
	movq	%r8, 8(%rsp)            # 8-byte Spill
	xorq	%rax, %rax
	movl	%eax, 40(%rsp)
	jmp	.LBB13_3
	.align	16, 0x90
.LBB13_17:                              # %for.inc
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	(%rbx), %rbx
.LBB13_3:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB13_18
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	8(%rbx), %r14
	movq	8(%r14), %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_17
# BB#5:                                 # %if.end6
                                        #   in Loop: Header=BB13_3 Depth=1
	movslq	8(%r12), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %esi
	movq	%r13, %rdi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_19
# BB#6:                                 # %if.then12
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	8(%r14), %rdi
	movzbl	(%rdi), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB13_13
# BB#7:                                 # %if.end.i
                                        #   in Loop: Header=BB13_3 Depth=1
	testq	%r15, %r15
	je	.LBB13_11
# BB#8:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	(%r14), %rax
	cmpq	%r15, %rax
	jne	.LBB13_9
.LBB13_11:                              # %if.end5.i
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	16(%r14), %rsi
	movq	$1, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_13
# BB#12:                                # %if.end9.i
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	8(%r14), %rdx
	movslq	8(%rdx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	df_insn_modify
.LBB13_13:                              # %df_ref_reg_replace.exit
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB13_15
# BB#14:                                # %lor.lhs.false
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	8(%rax), %rax
	movq	8(%r14), %rcx
	movq	8(%rax), %rax
	cmpq	%rax, %rcx
	je	.LBB13_17
.LBB13_15:                              # %land.lhs.true
                                        #   in Loop: Header=BB13_3 Depth=1
	cmpq	$0, 56(%r12)
	je	.LBB13_17
# BB#16:                                # %if.then24
                                        #   in Loop: Header=BB13_3 Depth=1
	movq	%r12, 32(%rsp)
	addq	$56, %r12
	movq	%r12, %rdi
	movabsq	$df_rtx_reg_replace, %rsi
	leaq	16(%rsp), %rdx
	callq	for_each_rtx
	jmp	.LBB13_17
.LBB13_18:                              # %for.end
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_19:                              # %if.else
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_refs_reg_replace, %rcx
	movq	$2624, %rsi             # imm = 0xA40
	jmp	.LBB13_10
.LBB13_9:                               # %if.then4.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_reg_replace, %rcx
	movq	$2664, %rsi             # imm = 0xA68
.LBB13_10:                              # %if.then4.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp147:
	.size	df_refs_reg_replace, .Ltmp147-df_refs_reg_replace
	.cfi_endproc

	.globl	df_ref_reg_replace
	.align	16, 0x90
	.type	df_ref_reg_replace,@function
df_ref_reg_replace:                     # @df_ref_reg_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp152:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp153:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 32
.Ltmp155:
	.cfi_offset %rbx, -32
.Ltmp156:
	.cfi_offset %r14, -24
.Ltmp157:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	8(%rbx), %rax
	movzbl	(%rax), %esi
	movzbl	rtx_class(%rsi), %esi
	movq	%rcx, %r8
	movq	%rdi, %r14
	xorq	%r15, %r15
	cmpq	$105, %rsi
	jne	.LBB14_5
# BB#1:                                 # %if.end
	testq	%rdx, %rdx
	je	.LBB14_3
# BB#2:                                 # %land.lhs.true
	movq	(%rbx), %rcx
	cmpq	%rdx, %rcx
	jne	.LBB14_6
.LBB14_3:                               # %if.end5
	movq	16(%rbx), %rsi
	movq	$1, %rcx
	movq	%rax, %rdi
	movq	%r8, %rdx
	callq	validate_change
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB14_5
# BB#4:                                 # %if.end9
	movq	8(%rbx), %rdx
	movslq	8(%rdx), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rsi
	movq	%r14, %rdi
	callq	df_insn_modify
	movq	$1, %r15
.LBB14_5:                               # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB14_6:                               # %if.then4
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_reg_replace, %rcx
	movq	$2664, %rsi             # imm = 0xA68
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp158:
	.size	df_ref_reg_replace, .Ltmp158-df_ref_reg_replace
	.cfi_endproc

	.align	16, 0x90
	.type	df_rtx_reg_replace,@function
df_rtx_reg_replace:                     # @df_rtx_reg_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 16
.Ltmp162:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	(%rax), %rcx
	movq	%rsi, %rbx
	testq	%rcx, %rcx
	je	.LBB15_3
# BB#1:                                 # %if.end
	movq	(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB15_3
# BB#2:                                 # %if.then2
	movq	8(%rbx), %rdx
	movq	16(%rbx), %rdi
	movq	$1, %rcx
	movq	%rax, %rsi
	callq	validate_change
	movl	24(%rbx), %eax
	incq	%rax
	movl	%eax, 24(%rbx)
.LBB15_3:                               # %return
	xorq	%rax, %rax
	popq	%rbx
	retq
.Ltmp163:
	.size	df_rtx_reg_replace, .Ltmp163-df_rtx_reg_replace
	.cfi_endproc

	.globl	df_reg_replace
	.align	16, 0x90
	.type	df_reg_replace,@function
df_reg_replace:                         # @df_reg_replace
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp170:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp171:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp172:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp173:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp174:
	.cfi_def_cfa_offset 48
.Ltmp175:
	.cfi_offset %rbx, -48
.Ltmp176:
	.cfi_offset %r12, -40
.Ltmp177:
	.cfi_offset %r13, -32
.Ltmp178:
	.cfi_offset %r14, -24
.Ltmp179:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rdi, %r13
	movl	8(%r14), %ebx
	movq	40(%r13), %rax
	movq	%rcx, %r15
	movq	$5, %rcx
	shlq	%cl, %rbx
	movq	(%rax,%rbx), %rdx
	movq	%rsi, %r12
                                        # kill: RDI<def> R13<kill>
                                        # kill: RSI<def> R12<kill>
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	df_refs_reg_replace
	movq	40(%r13), %rax
	movq	8(%rax,%rbx), %rdx
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r14, %rcx
	movq	%r15, %r8
	callq	df_refs_reg_replace
	movq	$1, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp180:
	.size	df_reg_replace, .Ltmp180-df_reg_replace
	.cfi_endproc

	.globl	df_bb_def_use_swap
	.align	16, 0x90
	.type	df_bb_def_use_swap,@function
df_bb_def_use_swap:                     # @df_bb_def_use_swap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp187:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp188:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp189:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp190:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp191:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp192:
	.cfi_def_cfa_offset 64
.Ltmp193:
	.cfi_offset %rbx, -48
.Ltmp194:
	.cfi_offset %r12, -40
.Ltmp195:
	.cfi_offset %r13, -32
.Ltmp196:
	.cfi_offset %r14, -24
.Ltmp197:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rdi, %r15
	movl	8(%r12), %eax
	movq	56(%r15), %r9
	leaq	(%rax,%rax,2), %rax
	movq	(%r9,%rax,8), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB17_33
# BB#1:
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r8, %rdx
	.align	16, 0x90
.LBB17_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rbx
	movq	(%rbx), %rcx
	movzbl	(%rcx), %esi
	cmpq	$63, %rsi
	jne	.LBB17_5
# BB#4:                                 # %cond.true.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	8(%rcx), %rcx
.LBB17_5:                               # %cond.end.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movl	8(%rcx), %ecx
	cmpq	%rdx, %rcx
	je	.LBB17_6
# BB#2:                                 # %for.cond.i
                                        #   in Loop: Header=BB17_3 Depth=1
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	jne	.LBB17_3
	jmp	.LBB17_33
.LBB17_6:                               # %df_bb_insn_regno_first_def_find.exit
	testq	%rbx, %rbx
	je	.LBB17_33
# BB#7:                                 # %if.end
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	8(%rcx), %r14d
	movq	$32, %rcx
	leaq	(%r14,%r14,2), %rsi
	movq	8(%r9,%rsi,8), %rsi
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	jmp	.LBB17_9
	.align	16, 0x90
.LBB17_8:                               # %for.cond.i59
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	(%rsi), %rsi
.LBB17_9:                               # %for.cond.i59
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB17_33
# BB#10:                                # %for.body.i66
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	8(%rsi), %r13
	movq	(%r13), %rcx
	movzbl	(%rcx), %edi
	cmpq	$63, %rdi
	jne	.LBB17_12
# BB#11:                                # %cond.true.i69
                                        #   in Loop: Header=BB17_9 Depth=1
	movq	8(%rcx), %rcx
.LBB17_12:                              # %cond.end.i74
                                        #   in Loop: Header=BB17_9 Depth=1
	movl	8(%rcx), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB17_8
# BB#13:                                # %df_bb_insn_regno_last_use_find.exit
	testq	%r13, %r13
	je	.LBB17_33
# BB#14:                                # %if.end4
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	df_use_unlink
	movq	56(%r15), %rsi
	leaq	(%r14,%r14,2), %r8
	movq	8(%rsi,%r8,8), %rcx
	testq	%rcx, %rcx
	je	.LBB17_22
# BB#15:                                # %if.then.i81
	movq	(%rcx), %rdx
	movq	8(%rcx), %rdi
	leaq	8(%rsi,%r8,8), %rsi
	testq	%rdx, %rdx
	je	.LBB17_16
# BB#18:                                # %if.else.i85
	cmpq	%r13, %rdi
	je	.LBB17_21
	.align	16, 0x90
.LBB17_19:                              # %for.cond.i90
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rsi
	movq	%rdx, %rcx
	testq	%rcx, %rcx
	je	.LBB17_22
# BB#20:                                # %for.body.i94
                                        #   in Loop: Header=BB17_19 Depth=1
	movq	8(%rcx), %rax
	movq	(%rcx), %rdx
	cmpq	%r13, %rax
	jne	.LBB17_19
.LBB17_21:                              # %if.then15.i95
	movq	%rdx, (%rsi)
	jmp	.LBB17_22
.LBB17_16:                              # %if.then2.i82
	cmpq	%r13, %rdi
	jne	.LBB17_34
# BB#17:                                # %if.end.i84
	movq	$0, (%rsi)
.LBB17_22:                              # %df_ref_unlink.exit97
	movslq	8(%r12), %rax
	movq	56(%r15), %rsi
	leaq	(%rax,%rax,2), %rdi
	movq	(%rsi,%rdi,8), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB17_32
# BB#23:                                # %if.then.i
	movq	(%rcx), %rdx
	movq	8(%rcx), %rax
	leaq	(%rsi,%rdi,8), %rsi
	testq	%rdx, %rdx
	je	.LBB17_24
# BB#26:                                # %if.else.i
	cmpq	%rbx, %rax
	je	.LBB17_27
	.align	16, 0x90
.LBB17_28:                              # %for.cond.i48
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rsi
	movq	%rdx, %rcx
	testq	%rcx, %rcx
	je	.LBB17_29
# BB#30:                                # %for.body.i49
                                        #   in Loop: Header=BB17_28 Depth=1
	movq	8(%rcx), %rax
	movq	(%rcx), %rdx
	cmpq	%rbx, %rax
	jne	.LBB17_28
# BB#31:                                # %if.then15.i
	movq	%rdx, (%rsi)
	jmp	.LBB17_32
.LBB17_24:                              # %if.then2.i
	cmpq	%rbx, %rax
	jne	.LBB17_34
# BB#25:                                # %if.end.i
	movq	$0, (%rsi)
	movq	%rcx, %rdx
	jmp	.LBB17_32
.LBB17_27:                              # %if.then7.i
	movq	%rdx, (%rsi)
	movq	%rcx, %rdx
	jmp	.LBB17_32
.LBB17_29:
	movq	%rsi, %rdx
.LBB17_32:                              # %df_ref_unlink.exit
	movq	%rbx, 8(%rdx)
	movq	56(%r15), %rax
	movq	(%rax,%r8,8), %rcx
	movq	%rcx, (%rdx)
	movq	%rdx, (%rax,%r8,8)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 8(%rbx)
	movq	%rbx, %rax
.LBB17_33:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_34:                              # %if.then4.i83
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_unlink, %rcx
	movq	$692, %rsi              # imm = 0x2B4
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp198:
	.size	df_bb_def_use_swap, .Ltmp198-df_bb_def_use_swap
	.cfi_endproc

	.globl	df_pattern_emit_before
	.align	16, 0x90
	.type	df_pattern_emit_before,@function
df_pattern_emit_before:                 # @df_pattern_emit_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp205:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp206:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp207:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp208:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp209:
	.cfi_def_cfa_offset 48
.Ltmp210:
	.cfi_offset %rbx, -48
.Ltmp211:
	.cfi_offset %r12, -40
.Ltmp212:
	.cfi_offset %r13, -32
.Ltmp213:
	.cfi_offset %r14, -24
.Ltmp214:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	(%r15), %rax
	movq	%rcx, %rbx
	movq	%rdi, %r14
	cmpq	%rbx, %rax
	je	.LBB18_4
# BB#1:                                 # %if.end
	movq	16(%rbx), %r13
	movq	%rsi, %rdi
	movq	%rbx, %rsi
	callq	emit_insn_before
	movq	%rax, %r12
	cmpq	%rbx, %r12
	je	.LBB18_3
# BB#2:                                 # %if.end3
	movq	24(%r13), %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	df_insns_modify
.LBB18_3:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB18_4:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_pattern_emit_before, %rcx
	movq	$2770, %rsi             # imm = 0xAD2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp215:
	.size	df_pattern_emit_before, .Ltmp215-df_pattern_emit_before
	.cfi_endproc

	.align	16, 0x90
	.type	df_insns_modify,@function
df_insns_modify:                        # @df_insns_modify
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp223:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp224:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp225:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp226:
	.cfi_def_cfa_offset 48
.Ltmp227:
	.cfi_offset %rbx, -48
.Ltmp228:
	.cfi_offset %r12, -40
.Ltmp229:
	.cfi_offset %r13, -32
.Ltmp230:
	.cfi_offset %r14, -24
.Ltmp231:
	.cfi_offset %r15, -16
	movq	%rdx, %rbx
	movl	(%rbx), %eax
	movq	%rcx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	jmp	.LBB19_1
	.align	16, 0x90
.LBB19_7:                               # %for.inc
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	24(%rbx), %rbx
	movl	(%rbx), %eax
.LBB19_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	andq	$67174399, %rcx         # imm = 0x400FFFF
	cmpq	$34, %rcx
	je	.LBB19_8
# BB#2:                                 # %entry
                                        #   in Loop: Header=BB19_1 Depth=1
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$36, %rax
	je	.LBB19_8
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB19_1 Depth=1
	movl	8(%rbx), %ecx
	movl	64(%r12), %eax
	cmpq	%rax, %rcx
	jb	.LBB19_6
# BB#4:                                 # %if.then7
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	$2, %rcx
	movq	%rax, %r13
	shrq	%cl, %r13
	addq	%rax, %r13
	movq	56(%r12), %rdi
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, 56(%r12)
	movl	64(%r12), %ecx
	leaq	(%rcx,%rcx,2), %rdx
	leaq	(%rax,%rdx,8), %rdi
	movq	%r13, %rax
	subq	%rcx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdx
	xorq	%rsi, %rsi
	callq	memset
	movl	%r13d, 64(%r12)
	cmpq	$0, 112(%r12)
	jne	.LBB19_6
# BB#5:                                 # %if.then10.i
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	callq	bitmap_initialize
	movq	%rax, 112(%r12)
	movq	%rax, %rdi
	callq	bitmap_clear
.LBB19_6:                               # %if.end8
                                        #   in Loop: Header=BB19_1 Depth=1
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	df_insn_modify
	cmpq	%r14, %rbx
	jne	.LBB19_7
# BB#9:                                 # %for.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB19_8:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_insns_modify, %rcx
	movq	$2742, %rsi             # imm = 0xAB6
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp232:
	.size	df_insns_modify, .Ltmp232-df_insns_modify
	.cfi_endproc

	.globl	df_pattern_emit_after
	.align	16, 0x90
	.type	df_pattern_emit_after,@function
df_pattern_emit_after:                  # @df_pattern_emit_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp240:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp242:
	.cfi_def_cfa_offset 48
.Ltmp243:
	.cfi_offset %rbx, -40
.Ltmp244:
	.cfi_offset %r12, -32
.Ltmp245:
	.cfi_offset %r14, -24
.Ltmp246:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rdi, %r12
	movq	%rsi, %rdi
	movq	%r15, %rsi
	callq	emit_insn_after
	movq	%rax, %rbx
	cmpq	%r15, %rbx
	je	.LBB20_2
# BB#1:                                 # %if.end
	movq	24(%r15), %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	df_insns_modify
.LBB20_2:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp247:
	.size	df_pattern_emit_after, .Ltmp247-df_pattern_emit_after
	.cfi_endproc

	.globl	df_jump_pattern_emit_after
	.align	16, 0x90
	.type	df_jump_pattern_emit_after,@function
df_jump_pattern_emit_after:             # @df_jump_pattern_emit_after
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp253:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp254:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp255:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp256:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp257:
	.cfi_def_cfa_offset 48
.Ltmp258:
	.cfi_offset %rbx, -40
.Ltmp259:
	.cfi_offset %r12, -32
.Ltmp260:
	.cfi_offset %r14, -24
.Ltmp261:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rdx, %r14
	movq	%rdi, %r12
	movq	%rsi, %rdi
	movq	%r15, %rsi
	callq	emit_jump_insn_after
	movq	%rax, %rbx
	cmpq	%r15, %rbx
	je	.LBB21_2
# BB#1:                                 # %if.end
	movq	24(%r15), %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	callq	df_insns_modify
.LBB21_2:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp262:
	.size	df_jump_pattern_emit_after, .Ltmp262-df_jump_pattern_emit_after
	.cfi_endproc

	.globl	df_insn_move_before
	.align	16, 0x90
	.type	df_insn_move_before,@function
df_insn_move_before:                    # @df_insn_move_before
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp269:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp270:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp271:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp272:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp273:
	.cfi_def_cfa_offset 48
.Ltmp274:
	.cfi_offset %rbx, -48
.Ltmp275:
	.cfi_offset %r12, -40
.Ltmp276:
	.cfi_offset %r13, -32
.Ltmp277:
	.cfi_offset %r14, -24
.Ltmp278:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %rbx
	movq	%rdi, %r15
	testq	%rsi, %rsi
	je	.LBB22_1
# BB#5:                                 # %if.end
	movl	8(%rdx), %eax
	movq	56(%r15), %rsi
	leaq	(%rax,%rax,2), %rax
	movq	(%rsi,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB22_8
	.align	16, 0x90
.LBB22_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rsi
	movq	8(%rsi), %rsi
	movslq	8(%rsi), %rsi
	movq	basic_block_for_insn(%rip), %rdi
	movq	%rbx, 32(%rdi,%rsi,8)
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB22_6
# BB#7:                                 # %for.cond.for.end_crit_edge
	movq	56(%r15), %rsi
.LBB22_8:                               # %for.end
	movq	8(%rsi,%rax,8), %rax
	jmp	.LBB22_10
	.align	16, 0x90
.LBB22_9:                               # %for.body15
                                        #   in Loop: Header=BB22_10 Depth=1
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rsi
	movq	%rbx, 32(%rsi,%rcx,8)
	movq	(%rax), %rax
.LBB22_10:                              # %for.body15
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB22_9
# BB#11:                                # %for.end27
	movq	%rdx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_insn_before  # TAILCALL
.LBB22_1:                               # %if.then
	movq	(%rbx), %rax
	cmpq	%r14, %rax
	je	.LBB22_12
# BB#2:                                 # %if.end.i
	movq	16(%r14), %r13
	movq	%rdx, %rdi
	movq	%r14, %rsi
	callq	emit_insn_before
	movq	%rax, %r12
	cmpq	%r14, %r12
	je	.LBB22_4
# BB#3:                                 # %if.end3.i
	movq	24(%r13), %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rcx
	callq	df_insns_modify
	movq	%r12, %r14
.LBB22_4:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB22_12:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_pattern_emit_before, %rcx
	movq	$2770, %rsi             # imm = 0xAD2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp279:
	.size	df_insn_move_before, .Ltmp279-df_insn_move_before
	.cfi_endproc

	.globl	df_insn_regno_def_p
	.align	16, 0x90
	.type	df_insn_regno_def_p,@function
df_insn_regno_def_p:                    # @df_insn_regno_def_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdx), %eax
	movq	56(%rdi), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	(%rdx,%rax,8), %rdx
	testq	%rdx, %rdx
	je	.LBB23_1
# BB#4:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	.align	16, 0x90
.LBB23_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdx), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %esi
	cmpq	$63, %rsi
	jne	.LBB23_7
# BB#6:                                 # %cond.true
                                        #   in Loop: Header=BB23_5 Depth=1
	movq	8(%rax), %rax
.LBB23_7:                               # %cond.end
                                        #   in Loop: Header=BB23_5 Depth=1
	movl	8(%rax), %eax
	cmpq	%rcx, %rax
	movq	$1, %rax
	je	.LBB23_8
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB23_5 Depth=1
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB23_5
# BB#3:
	xorq	%rax, %rax
	retq
.LBB23_1:
	xorq	%rax, %rax
	retq
.LBB23_8:                               # %return
	retq
.Ltmp280:
	.size	df_insn_regno_def_p, .Ltmp280-df_insn_regno_def_p
	.cfi_endproc

	.globl	df_insn_dominates_all_uses_p
	.align	16, 0x90
	.type	df_insn_dominates_all_uses_p,@function
df_insn_dominates_all_uses_p:           # @df_insn_dominates_all_uses_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rdx), %eax
	movq	56(%rdi), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	(%rcx,%rax,8), %r8
	jmp	.LBB24_1
	.align	16, 0x90
.LBB24_9:                               # %for.inc
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	(%r8), %r8
.LBB24_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB24_3 Depth 2
                                        #       Child Loop BB24_6 Depth 3
	testq	%r8, %r8
	je	.LBB24_10
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	8(%r8), %rdx
	movq	24(%rdx), %rsi
	jmp	.LBB24_3
	.align	16, 0x90
.LBB24_8:                               # %for.inc6.i
                                        #   in Loop: Header=BB24_3 Depth=2
	movq	(%rsi), %rsi
.LBB24_3:                               # %for.body
                                        #   Parent Loop BB24_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB24_6 Depth 3
	testq	%rsi, %rsi
	je	.LBB24_9
# BB#4:                                 # %for.body.i
                                        #   in Loop: Header=BB24_3 Depth=2
	movq	8(%rsi), %rax
	movq	24(%rax), %rdi
	jmp	.LBB24_6
	.align	16, 0x90
.LBB24_5:                               # %for.cond2.i
                                        #   in Loop: Header=BB24_6 Depth=3
	movq	(%rdi), %rdi
.LBB24_6:                               # %for.cond2.i
                                        #   Parent Loop BB24_1 Depth=1
                                        #     Parent Loop BB24_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rdi, %rdi
	je	.LBB24_8
# BB#7:                                 # %for.body4.i
                                        #   in Loop: Header=BB24_6 Depth=3
	movq	8(%rdi), %rcx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	je	.LBB24_5
	jmp	.LBB24_11
.LBB24_10:
	movq	$1, %rax
.LBB24_11:                              # %return
	retq
.Ltmp281:
	.size	df_insn_dominates_all_uses_p, .Ltmp281-df_insn_dominates_all_uses_p
	.cfi_endproc

	.globl	df_insn_dominates_uses_p
	.align	16, 0x90
	.type	df_insn_dominates_uses_p,@function
df_insn_dominates_uses_p:               # @df_insn_dominates_uses_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp288:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp289:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp290:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp291:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp292:
	.cfi_def_cfa_offset 48
.Ltmp293:
	.cfi_offset %rbx, -48
.Ltmp294:
	.cfi_offset %r12, -40
.Ltmp295:
	.cfi_offset %r13, -32
.Ltmp296:
	.cfi_offset %r14, -24
.Ltmp297:
	.cfi_offset %r15, -16
	movl	8(%rdx), %eax
	movq	56(%rdi), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	(%rdx,%rax,8), %r12
	movq	%rcx, %r14
	jmp	.LBB25_1
	.align	16, 0x90
.LBB25_11:                              # %for.inc
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	(%r12), %r12
.LBB25_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB25_4 Depth 2
                                        #       Child Loop BB25_8 Depth 3
	testq	%r12, %r12
	je	.LBB25_12
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	8(%r12), %r13
	movq	8(%r13), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %esi
	movq	%r14, %rdi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB25_11
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	24(%r13), %rbx
	jmp	.LBB25_4
	.align	16, 0x90
.LBB25_10:                              # %for.inc10.i
                                        #   in Loop: Header=BB25_4 Depth=2
	movq	(%rbx), %rbx
.LBB25_4:                               # %land.lhs.true
                                        #   Parent Loop BB25_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB25_8 Depth 3
	testq	%rbx, %rbx
	je	.LBB25_11
# BB#5:                                 # %for.body.i
                                        #   in Loop: Header=BB25_4 Depth=2
	movq	8(%rbx), %r15
	movq	8(%r15), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%rax,8), %rax
	movl	88(%rax), %esi
	movq	%r14, %rdi
	callq	bitmap_bit_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB25_10
# BB#6:                                 # %if.then.i
                                        #   in Loop: Header=BB25_4 Depth=2
	movq	24(%r15), %rcx
	jmp	.LBB25_8
	.align	16, 0x90
.LBB25_7:                               # %for.cond4.i
                                        #   in Loop: Header=BB25_8 Depth=3
	movq	(%rcx), %rcx
.LBB25_8:                               # %for.cond4.i
                                        #   Parent Loop BB25_1 Depth=1
                                        #     Parent Loop BB25_4 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rcx, %rcx
	je	.LBB25_10
# BB#9:                                 # %for.body6.i
                                        #   in Loop: Header=BB25_8 Depth=3
	movq	8(%rcx), %rdx
	xorq	%rax, %rax
	cmpq	%r13, %rdx
	je	.LBB25_7
	jmp	.LBB25_13
.LBB25_12:
	movq	$1, %rax
.LBB25_13:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp298:
	.size	df_insn_dominates_uses_p, .Ltmp298-df_insn_dominates_uses_p
	.cfi_endproc

	.globl	df_regno_bb
	.align	16, 0x90
	.type	df_regno_bb,@function
df_regno_bb:                            # @df_regno_bb
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	40(%rdi), %rdx
	andq	%rsi, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	8(%rdx,%rax), %rax
	xorq	%rcx, %rcx
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB26_2
# BB#1:                                 # %cond.true
	movq	8(%rsi), %rdx
.LBB26_2:                               # %cond.end
	testq	%rax, %rax
	je	.LBB26_4
# BB#3:                                 # %cond.true7
	movq	8(%rax), %rcx
.LBB26_4:                               # %cond.end10
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB26_6
# BB#5:                                 # %cond.true13
	movq	8(%rdx), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rax,8), %rax
.LBB26_6:                               # %cond.end18
	testq	%rcx, %rcx
	je	.LBB26_8
# BB#7:                                 # %cond.true21
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rsi
.LBB26_8:                               # %cond.end31
	cmpq	%rsi, %rax
	je	.LBB26_10
# BB#9:                                 # %cond.end31
	xorq	%rax, %rax
.LBB26_10:                              # %cond.end31
	retq
.Ltmp299:
	.size	df_regno_bb, .Ltmp299-df_regno_bb
	.cfi_endproc

	.globl	df_reg_global_p
	.align	16, 0x90
	.type	df_reg_global_p,@function
df_reg_global_p:                        # @df_reg_global_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rsi), %eax
	movq	40(%rdi), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	8(%rdx,%rax), %rcx
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB27_2
# BB#1:                                 # %cond.true.i
	movq	8(%rsi), %rdx
.LBB27_2:                               # %cond.end.i
	testq	%rcx, %rcx
	je	.LBB27_4
# BB#3:                                 # %cond.true7.i
	movq	8(%rcx), %rax
.LBB27_4:                               # %cond.end10.i
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB27_6
# BB#5:                                 # %cond.true13.i
	movq	8(%rdx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
.LBB27_6:                               # %cond.end18.i
	testq	%rax, %rax
	je	.LBB27_8
# BB#7:                                 # %cond.true21.i
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rax,8), %rsi
.LBB27_8:                               # %df_regno_bb.exit
	cmpq	%rsi, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	movq	%rdx, %rax
	jne	.LBB27_10
# BB#9:                                 # %df_regno_bb.exit
	movq	%rsi, %rax
.LBB27_10:                              # %df_regno_bb.exit
	testq	%rcx, %rcx
	je	.LBB27_12
# BB#11:                                # %df_regno_bb.exit
	movq	%rsi, %rdx
.LBB27_12:                              # %df_regno_bb.exit
	andq	%rdx, %rax
	retq
.Ltmp300:
	.size	df_reg_global_p, .Ltmp300-df_reg_global_p
	.cfi_endproc

	.globl	df_reg_lifetime
	.align	16, 0x90
	.type	df_reg_lifetime,@function
df_reg_lifetime:                        # @df_reg_lifetime
	.cfi_startproc
# BB#0:                                 # %entry
	movl	8(%rsi), %eax
	movq	40(%rdi), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movl	16(%rdx,%rax), %eax
	retq
.Ltmp301:
	.size	df_reg_lifetime, .Ltmp301-df_reg_lifetime
	.cfi_endproc

	.globl	df_bb_reg_live_start_p
	.align	16, 0x90
	.type	df_bb_reg_live_start_p,@function
df_bb_reg_live_start_p:                 # @df_bb_reg_live_start_p
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	88(%rsi), %rax
	movq	8(%rdi), %rcx
	imulq	$112, %rax, %rax
	movq	80(%rcx,%rax), %rdi
	movl	8(%rdx), %esi
	jmp	bitmap_bit_p  # TAILCALL
.Ltmp302:
	.size	df_bb_reg_live_start_p, .Ltmp302-df_bb_reg_live_start_p
	.cfi_endproc

	.globl	df_bb_reg_live_end_p
	.align	16, 0x90
	.type	df_bb_reg_live_end_p,@function
df_bb_reg_live_end_p:                   # @df_bb_reg_live_end_p
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	88(%rsi), %rax
	movq	8(%rdi), %rcx
	imulq	$112, %rax, %rax
	movq	88(%rcx,%rax), %rdi
	movl	8(%rdx), %esi
	jmp	bitmap_bit_p  # TAILCALL
.Ltmp303:
	.size	df_bb_reg_live_end_p, .Ltmp303-df_bb_reg_live_end_p
	.cfi_endproc

	.globl	df_bb_regs_lives_compare
	.align	16, 0x90
	.type	df_bb_regs_lives_compare,@function
df_bb_regs_lives_compare:               # @df_bb_regs_lives_compare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp306:
	.cfi_def_cfa_offset 16
.Ltmp307:
	.cfi_offset %rbx, -16
	movl	8(%rdx), %edx
	movq	40(%rdi), %r9
	movl	8(%rcx), %eax
	movq	$5, %rcx
	shlq	%cl, %rdx
	movq	(%r9,%rdx), %r8
	movq	8(%r9,%rdx), %r10
	xorq	%r11, %r11
	testq	%r8, %r8
	movabsq	$0, %rcx
	je	.LBB31_2
# BB#1:                                 # %cond.true.i
	movq	8(%r8), %rcx
.LBB31_2:                               # %cond.end.i
	testq	%r10, %r10
	je	.LBB31_4
# BB#3:                                 # %cond.true7.i
	movq	8(%r10), %r11
.LBB31_4:                               # %cond.end10.i
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	movabsq	$0, %rdx
	je	.LBB31_6
# BB#5:                                 # %cond.true13.i
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rdx
.LBB31_6:                               # %cond.end18.i
	testq	%r11, %r11
	je	.LBB31_8
# BB#7:                                 # %cond.true21.i
	movq	8(%r11), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rbx
	movq	32(%rbx,%rcx,8), %rbx
.LBB31_8:                               # %df_regno_bb.exit
	cmpq	%rbx, %rdx
	je	.LBB31_10
# BB#9:                                 # %df_regno_bb.exit
	xorq	%rdx, %rdx
.LBB31_10:                              # %df_regno_bb.exit
	cmpq	%rsi, %rdx
	jne	.LBB31_42
# BB#11:                                # %lor.lhs.false
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%r9,%rax), %rcx
	movq	8(%r9,%rax), %r9
	xorq	%r11, %r11
	testq	%rcx, %rcx
	movabsq	$0, %rax
	je	.LBB31_13
# BB#12:                                # %cond.true.i141
	movq	8(%rcx), %rax
.LBB31_13:                              # %cond.end.i144
	testq	%r9, %r9
	je	.LBB31_15
# BB#14:                                # %cond.true7.i146
	movq	8(%r9), %r11
.LBB31_15:                              # %cond.end10.i149
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movabsq	$0, %rdx
	je	.LBB31_17
# BB#16:                                # %cond.true13.i157
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rax,8), %rdx
.LBB31_17:                              # %cond.end18.i160
	testq	%r11, %r11
	je	.LBB31_19
# BB#18:                                # %cond.true21.i168
	movq	8(%r11), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rbx
	movq	32(%rbx,%rax,8), %rbx
.LBB31_19:                              # %df_regno_bb.exit172
	cmpq	%rbx, %rdx
	je	.LBB31_21
# BB#20:                                # %df_regno_bb.exit172
	xorq	%rdx, %rdx
.LBB31_21:                              # %df_regno_bb.exit172
	cmpq	%rsi, %rdx
	jne	.LBB31_42
# BB#22:                                # %if.end
	movq	basic_block_for_insn(%rip), %rax
	testq	%rcx, %rcx
	je	.LBB31_23
	.align	16, 0x90
.LBB31_26:                              # %for.body.i132
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rdx
	movq	8(%rdx), %rbx
	movslq	8(%rbx), %rbx
	movq	32(%rax,%rbx,8), %rbx
	cmpq	%rsi, %rbx
	je	.LBB31_27
# BB#24:                                # %for.cond.i123
                                        #   in Loop: Header=BB31_26 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB31_26
# BB#25:
	xorq	%rdx, %rdx
	jmp	.LBB31_27
.LBB31_23:
	xorq	%rdx, %rdx
	jmp	.LBB31_27
	.align	16, 0x90
.LBB31_28:                              # %for.body.i110.for.body.i110_crit_edge
                                        #   in Loop: Header=BB31_27 Depth=1
	movq	(%r10), %r10
.LBB31_27:                              # %for.body.i110
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r10), %rcx
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rbx
	movq	32(%rax,%rbx,8), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB31_28
# BB#29:                                # %df_bb_regno_last_use_find.exit112
	movq	8(%rdx), %rcx
	movslq	8(%rcx), %rdx
	movq	56(%rdi), %rcx
	leaq	(%rdx,%rdx,2), %rdx
	movslq	16(%rcx,%rdx,8), %rdx
	leaq	(%rbx,%rbx,2), %rdi
	movslq	16(%rcx,%rdi,8), %rdi
	cmpq	%rdi, %rdx
	jle	.LBB31_31
# BB#30:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	popq	%rbx
	retq
.LBB31_31:                              # %if.end21
	testq	%r8, %r8
	je	.LBB31_32
	.align	16, 0x90
.LBB31_35:                              # %for.body.i89
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r8), %rdx
	movq	8(%rdx), %rdi
	movslq	8(%rdi), %rdi
	movq	32(%rax,%rdi,8), %rdi
	cmpq	%rsi, %rdi
	je	.LBB31_36
# BB#33:                                # %for.cond.i80
                                        #   in Loop: Header=BB31_35 Depth=1
	movq	(%r8), %r8
	testq	%r8, %r8
	jne	.LBB31_35
# BB#34:
	xorq	%rdx, %rdx
	jmp	.LBB31_36
.LBB31_32:
	xorq	%rdx, %rdx
	jmp	.LBB31_36
	.align	16, 0x90
.LBB31_37:                              # %for.body.i.for.body.i_crit_edge
                                        #   in Loop: Header=BB31_36 Depth=1
	movq	(%r9), %r9
.LBB31_36:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r9), %rdi
	movq	8(%rdi), %rdi
	movslq	8(%rdi), %rdi
	movq	32(%rax,%rdi,8), %rbx
	cmpq	%rsi, %rbx
	jne	.LBB31_37
# BB#38:                                # %df_bb_regno_last_use_find.exit
	movq	8(%rdx), %rax
	movslq	8(%rax), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	16(%rcx,%rax,8), %rax
	leaq	(%rdi,%rdi,2), %rdx
	movslq	16(%rcx,%rdx,8), %rcx
	cmpq	%rcx, %rax
	jle	.LBB31_39
# BB#40:                                # %df_bb_regno_last_use_find.exit
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB31_39:
	movabsq	$0, %rax
	popq	%rbx
	retq
.LBB31_42:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_bb_regs_lives_compare, %rcx
	movq	$3081, %rsi             # imm = 0xC09
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp308:
	.size	df_bb_regs_lives_compare, .Ltmp308-df_bb_regs_lives_compare
	.cfi_endproc

	.globl	df_bb_single_def_use_insn_find
	.align	16, 0x90
	.type	df_bb_single_def_use_insn_find,@function
df_bb_single_def_use_insn_find:         # @df_bb_single_def_use_insn_find
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp310:
	.cfi_def_cfa_offset 16
	movl	8(%rdx), %eax
	movq	56(%rdi), %rdx
	leaq	(%rax,%rax,2), %rax
	movq	(%rdx,%rax,8), %rax
	testq	%rax, %rax
	je	.LBB32_12
# BB#1:
	movl	8(%rcx), %edx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	.align	16, 0x90
.LBB32_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rdx
	movq	(%rdx), %rsi
	movzbl	(%rsi), %edi
	cmpq	$63, %rdi
	jne	.LBB32_5
# BB#4:                                 # %cond.true.i
                                        #   in Loop: Header=BB32_3 Depth=1
	movq	8(%rsi), %rsi
.LBB32_5:                               # %cond.end.i
                                        #   in Loop: Header=BB32_3 Depth=1
	movl	8(%rsi), %esi
	cmpq	%rcx, %rsi
	je	.LBB32_6
# BB#2:                                 # %for.cond.i
                                        #   in Loop: Header=BB32_3 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB32_3
	jmp	.LBB32_12
.LBB32_6:                               # %df_bb_insn_regno_first_def_find.exit
	testq	%rdx, %rdx
	je	.LBB32_12
# BB#7:                                 # %if.end
	movq	24(%rdx), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB32_11
# BB#8:                                 # %if.end3
	movq	8(%rcx), %rdx
	testq	%rdx, %rdx
	je	.LBB32_11
# BB#9:                                 # %if.end6
	cmpq	$0, (%rcx)
	jne	.LBB32_11
# BB#10:                                # %if.end9
	movq	8(%rdx), %rax
.LBB32_11:                              # %return
	popq	%rdx
	retq
.LBB32_12:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_bb_single_def_use_insn_find, %rcx
	movq	$3215, %rsi             # imm = 0xC8F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp311:
	.size	df_bb_single_def_use_insn_find, .Ltmp311-df_bb_single_def_use_insn_find
	.cfi_endproc

	.globl	df_dump
	.align	16, 0x90
	.type	df_dump,@function
df_dump:                                # @df_dump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp318:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp319:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp320:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp321:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp322:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp323:
	.cfi_def_cfa_offset 128
.Ltmp324:
	.cfi_offset %rbx, -48
.Ltmp325:
	.cfi_offset %r12, -40
.Ltmp326:
	.cfi_offset %r13, -32
.Ltmp327:
	.cfi_offset %r14, -24
.Ltmp328:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB33_106
# BB#1:                                 # %entry
	testq	%r14, %r14
	je	.LBB33_106
# BB#2:                                 # %if.end
	movq	%rdi, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movabsq	$.L.str1, %rdi
	movq	$19, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movl	92(%rbx), %r9d
	movl	88(%rbx), %r8d
	movl	76(%rbx), %ecx
	movl	96(%rbx), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r14, 56(%rsp)          # 8-byte Spill
	callq	fprintf
	movq	%rbx, %rsi
	testq	$1, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB33_8
# BB#3:                                 # %if.then4
	movq	%rsi, %rbx
	movabsq	$.L.str3, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	fwrite
	movq	%rbx, %rsi
	movl	92(%rsi), %eax
	testq	%rax, %rax
	je	.LBB33_8
# BB#4:                                 # %for.body.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB33_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	%rbx, %rdx
	andq	%rdi, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	8(%rsi), %r14
	imulq	$112, %rcx, %r15
	cmpq	$0, 16(%r14,%r15)
	je	.LBB33_7
# BB#6:                                 # %if.end12
                                        #   in Loop: Header=BB33_5 Depth=1
	leaq	16(%r14,%r15), %r13
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str4, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	(%r13), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %r13
	movq	%r13, %rdx
                                        # kill: R13<def> RDX<kill>
	movabsq	$.L.str6, %rax
	movq	%rax, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	8(%r14,%r15), %rsi
	movq	%r12, %rdi
	movq	%r13, %rdx
	movabsq	$.L.str6, %r13
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str9, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	24(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rsi          # 8-byte Reload
	movl	92(%rsi), %eax
.LBB33_7:                               # %for.inc
                                        #   in Loop: Header=BB33_5 Depth=1
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	movq	%rax, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_5
.LBB33_8:                               # %if.end18
	testq	$16, 40(%rsp)           # 8-byte Folded Reload
	movq	56(%rsp), %r14          # 8-byte Reload
	je	.LBB33_23
# BB#9:                                 # %if.then21
	movq	%rsi, %rbx
	movabsq	$.L.str10, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	%rbx, %rsi
	movl	76(%rsi), %eax
	testq	%rax, %rax
	je	.LBB33_23
# BB#10:                                # %for.body26.lr.ph
	xorq	%r15, %r15
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$.L.str28, %r12
	.align	16, 0x90
.LBB33_11:                              # %for.body26
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_17 Depth 2
	movq	16(%rsi), %rcx
	movq	%r15, %rbx
	andq	%rdi, %rbx
	movq	(%rcx,%rbx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB33_22
# BB#12:                                # %if.then30
                                        #   in Loop: Header=BB33_11 Depth=1
	movq	8(%rcx), %rax
	movslq	8(%rax), %r9
	movq	basic_block_for_insn(%rip), %rdx
	movq	(%rcx), %rax
	movq	32(%rdx,%r9,8), %rcx
	movq	56(%rsi), %rdx
	movq	%rsi, %r13
	leaq	(%r9,%r9,2), %rsi
	movl	16(%rdx,%rsi,8), %r8d
	movzbl	(%rax), %edx
	movl	88(%rcx), %ecx
	cmpq	$63, %rdx
	jne	.LBB33_14
# BB#13:                                # %cond.true
                                        #   in Loop: Header=BB33_11 Depth=1
	movq	8(%rax), %rax
.LBB33_14:                              # %cond.end
                                        #   in Loop: Header=BB33_11 Depth=1
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%rdx, (%rsp)
	movq	%r14, %rdi
	movabsq	$.L.str11, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	16(%r13), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	40(%rax), %ecx
	testq	$1, %rcx
	je	.LBB33_16
# BB#15:                                # %if.then79
                                        #   in Loop: Header=BB33_11 Depth=1
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str12, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	16(%r13), %rax
	movq	(%rax,%rbx,8), %rax
.LBB33_16:                              # %if.end81
                                        #   in Loop: Header=BB33_11 Depth=1
	movq	24(%rax), %rbx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str31, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_17
	.align	16, 0x90
.LBB33_20:                              # %select.end
                                        #   in Loop: Header=BB33_17 Depth=2
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
.LBB33_17:                              # %if.end81
                                        #   Parent Loop BB33_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB33_21
# BB#18:                                # %for.body.i
                                        #   in Loop: Header=BB33_17 Depth=2
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB33_20
# BB#19:                                # %select.mid
                                        #   in Loop: Header=BB33_17 Depth=2
	movq	$117, %rdx
	jmp	.LBB33_20
	.align	16, 0x90
.LBB33_21:                              # %df_chain_dump.exit
                                        #   in Loop: Header=BB33_11 Depth=1
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	%r13, %rsi
	movl	76(%rsi), %eax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
.LBB33_22:                              # %for.inc87
                                        #   in Loop: Header=BB33_11 Depth=1
	incq	%r15
	movq	%r15, %rcx
	andq	%rdi, %rcx
	movq	%rax, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_11
.LBB33_23:                              # %if.end90
	testq	$2, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB33_29
# BB#24:                                # %if.then93
	movq	%rsi, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movabsq	$.L.str13, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	fwrite
	movq	%rbx, %rsi
	movl	92(%rsi), %eax
	testq	%rax, %rax
	je	.LBB33_29
# BB#25:                                # %for.body98.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB33_26:                              # %for.body98
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	%rbx, %rdx
	andq	%rdi, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	8(%rsi), %r14
	imulq	$112, %rcx, %r15
	cmpq	$0, 48(%r14,%r15)
	je	.LBB33_28
# BB#27:                                # %if.end111
                                        #   in Loop: Header=BB33_26 Depth=1
	leaq	48(%r14,%r15), %r13
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str4, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	(%r13), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %r13
	movq	%r13, %rdx
                                        # kill: R13<def> RDX<kill>
	movabsq	$.L.str6, %rax
	movq	%rax, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str7, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	40(%r14,%r15), %rsi
	movq	%r12, %rdi
	movq	%r13, %rdx
	movabsq	$.L.str6, %r13
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str8, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	32(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str9, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	56(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rsi          # 8-byte Reload
	movl	92(%rsi), %eax
.LBB33_28:                              # %for.inc117
                                        #   in Loop: Header=BB33_26 Depth=1
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	movq	%rax, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_26
.LBB33_29:                              # %if.end120
	testq	$8, 40(%rsp)            # 8-byte Folded Reload
	movq	56(%rsp), %r14          # 8-byte Reload
	je	.LBB33_44
# BB#30:                                # %if.then123
	movq	%rsi, %rbx
	movabsq	$.L.str14, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	%rbx, %rsi
	movl	88(%rsi), %eax
	testq	%rax, %rax
	je	.LBB33_44
# BB#31:                                # %for.body128.lr.ph
	xorq	%r15, %r15
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movabsq	$.L.str28, %r12
	.align	16, 0x90
.LBB33_32:                              # %for.body128
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_38 Depth 2
	movq	24(%rsi), %rcx
	movq	%r15, %rbx
	andq	%rdi, %rbx
	movq	(%rcx,%rbx,8), %rcx
	testq	%rcx, %rcx
	je	.LBB33_43
# BB#33:                                # %if.then132
                                        #   in Loop: Header=BB33_32 Depth=1
	movq	8(%rcx), %rax
	movslq	8(%rax), %r9
	movq	basic_block_for_insn(%rip), %rdx
	movq	(%rcx), %rax
	movq	32(%rdx,%r9,8), %rcx
	movq	56(%rsi), %rdx
	movq	%rsi, %r13
	leaq	(%r9,%r9,2), %rsi
	movl	16(%rdx,%rsi,8), %r8d
	movzbl	(%rax), %edx
	movl	88(%rcx), %ecx
	cmpq	$63, %rdx
	jne	.LBB33_35
# BB#34:                                # %cond.true170
                                        #   in Loop: Header=BB33_32 Depth=1
	movq	8(%rax), %rax
.LBB33_35:                              # %cond.end183
                                        #   in Loop: Header=BB33_32 Depth=1
	movl	8(%rax), %edx
	xorq	%rax, %rax
	movq	%rdx, (%rsp)
	movq	%r14, %rdi
	movabsq	$.L.str15, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	24(%r13), %rax
	movq	(%rax,%rbx,8), %rax
	movzbl	40(%rax), %ecx
	testq	$1, %rcx
	je	.LBB33_37
# BB#36:                                # %if.then195
                                        #   in Loop: Header=BB33_32 Depth=1
	movq	$11, %rsi
	movq	$1, %rdx
	movabsq	$.L.str12, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	24(%r13), %rax
	movq	(%rax,%rbx,8), %rax
.LBB33_37:                              # %if.end197
                                        #   in Loop: Header=BB33_32 Depth=1
	movq	24(%rax), %rbx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str31, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_38
	.align	16, 0x90
.LBB33_41:                              # %select.end53
                                        #   in Loop: Header=BB33_38 Depth=2
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
.LBB33_38:                              # %if.end197
                                        #   Parent Loop BB33_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB33_42
# BB#39:                                # %for.body.i549
                                        #   in Loop: Header=BB33_38 Depth=2
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB33_41
# BB#40:                                # %select.mid54
                                        #   in Loop: Header=BB33_38 Depth=2
	movq	$117, %rdx
	jmp	.LBB33_41
	.align	16, 0x90
.LBB33_42:                              # %df_chain_dump.exit551
                                        #   in Loop: Header=BB33_32 Depth=1
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	%r13, %rsi
	movl	88(%rsi), %eax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
.LBB33_43:                              # %for.inc204
                                        #   in Loop: Header=BB33_32 Depth=1
	incq	%r15
	movq	%r15, %rcx
	andq	%rdi, %rcx
	movq	%rax, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_32
.LBB33_44:                              # %if.end207
	testq	$4, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB33_50
# BB#45:                                # %if.then210
	movq	%rsi, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movabsq	$.L.str16, %rdi
	movq	$11, %rsi
	movq	$1, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	fwrite
	movq	%rbx, %rsi
	movl	92(%rsi), %eax
	testq	%rax, %rax
	je	.LBB33_50
# BB#46:                                # %for.body215.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	56(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB33_47:                              # %for.body215
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	%rbx, %rdx
	andq	%rdi, %rdx
	movq	32(%rcx,%rdx,8), %rcx
	movslq	88(%rcx), %rcx
	movq	8(%rsi), %r14
	imulq	$112, %rcx, %r15
	cmpq	$0, 80(%r14,%r15)
	je	.LBB33_49
# BB#48:                                # %if.end228
                                        #   in Loop: Header=BB33_47 Depth=1
	leaq	80(%r14,%r15), %r13
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str4, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	(%r13), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %r13
	movq	%r13, %rdx
                                        # kill: R13<def> RDX<kill>
	movabsq	$.L.str6, %rax
	movq	%rax, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str17, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	72(%r14,%r15), %rsi
	movq	%r12, %rdi
	movq	%r13, %rdx
	movabsq	$.L.str6, %r13
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str18, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	64(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str9, %rsi
	movq	%rbx, %rdx
	callq	fprintf
	movq	88(%r14,%r15), %rsi
	movq	%r12, %rdi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	bitmap_print
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rsi          # 8-byte Reload
	movl	92(%rsi), %eax
.LBB33_49:                              # %for.inc234
                                        #   in Loop: Header=BB33_47 Depth=1
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	movq	%rax, %rdx
	andq	%rdi, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_47
.LBB33_50:                              # %if.end237
	testq	$224, 40(%rsp)          # 8-byte Folded Reload
	movq	56(%rsp), %r14          # 8-byte Reload
	je	.LBB33_105
# BB#51:                                # %if.then240
	movq	40(%rsi), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movabsq	$.L.str19, %rdi
	movq	$15, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	%rbx, %rax
	movl	96(%rax), %eax
	testq	%rax, %rax
	je	.LBB33_105
# BB#52:                                # %for.body245.lr.ph
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	$128, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movabsq	$0, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movabsq	$1, %rcx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jne	.LBB33_54
# BB#53:                                # %for.body245.lr.ph
	movq	%rcx, 24(%rsp)          # 8-byte Spill
.LBB33_54:                              # %for.body245.lr.ph
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rbx
	andq	$32, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	andq	$64, %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rdx
	jne	.LBB33_56
# BB#55:                                # %for.body245.lr.ph
	movq	%rcx, %rdx
.LBB33_56:                              # %for.body245.lr.ph
	movq	32(%rsp), %rsi          # 8-byte Reload
	orq	%rbx, %rsi
	jne	.LBB33_58
# BB#57:                                # %for.body245.lr.ph
	movq	%rcx, 16(%rsp)          # 8-byte Spill
.LBB33_58:                              # %for.body245.lr.ph
	orq	%rdx, 24(%rsp)          # 8-byte Folded Spill
	andq	$96, 40(%rsp)           # 8-byte Folded Spill
	xorq	%r13, %r13
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB33_59:                              # %for.body245
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB33_87 Depth 2
                                        #     Child Loop BB33_98 Depth 2
	testq	%r15, %rbx
	je	.LBB33_62
# BB#60:                                # %land.lhs.true
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %rdx
	andq	%r15, %rdx
	movq	$5, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	24(%rcx,%rsi), %ecx
	testq	%rcx, %rcx
	jne	.LBB33_66
# BB#61:                                # %lor.lhs.false252
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$5, %rcx
	shlq	%cl, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	20(%rcx,%rdx), %ecx
	testq	%rcx, %rcx
	jne	.LBB33_66
	.align	16, 0x90
.LBB33_62:                              # %lor.lhs.false257
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB33_64
# BB#63:                                # %land.lhs.true260
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %rdx
	andq	%r15, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	$0, (%rcx,%rdx)
	jne	.LBB33_66
.LBB33_64:                              # %lor.lhs.false265
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	testq	%r15, %rcx
	je	.LBB33_104
# BB#65:                                # %land.lhs.true268
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %rdx
	andq	%r15, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	cmpq	$0, 8(%rcx,%rdx)
	je	.LBB33_104
	.align	16, 0x90
.LBB33_66:                              # %if.then273
                                        #   in Loop: Header=BB33_59 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str20, %rsi
	movq	%r13, %rdx
	callq	fprintf
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB33_80
# BB#67:                                # %if.then280
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	40(%rcx), %rdx
	andq	%r15, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	(%rdx,%rax), %rsi
	movq	8(%rdx,%rax), %rcx
	xorq	%rax, %rax
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	je	.LBB33_69
# BB#68:                                # %cond.true.i
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	8(%rsi), %rdx
.LBB33_69:                              # %cond.end.i
                                        #   in Loop: Header=BB33_59 Depth=1
	testq	%rcx, %rcx
	je	.LBB33_71
# BB#70:                                # %cond.true7.i
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	8(%rcx), %rax
.LBB33_71:                              # %cond.end10.i
                                        #   in Loop: Header=BB33_59 Depth=1
	xorq	%rsi, %rsi
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	je	.LBB33_73
# BB#72:                                # %cond.true13.i
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	8(%rdx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rcx,8), %rcx
.LBB33_73:                              # %cond.end18.i
                                        #   in Loop: Header=BB33_59 Depth=1
	testq	%rax, %rax
	je	.LBB33_75
# BB#74:                                # %cond.true21.i
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	8(%rax), %rax
	movslq	8(%rax), %rax
	movq	basic_block_for_insn(%rip), %rdx
	movq	32(%rdx,%rax,8), %rsi
.LBB33_75:                              # %df_regno_bb.exit
                                        #   in Loop: Header=BB33_59 Depth=1
	cmpq	%rsi, %rcx
	je	.LBB33_77
# BB#76:                                # %df_regno_bb.exit
                                        #   in Loop: Header=BB33_59 Depth=1
	xorq	%rcx, %rcx
.LBB33_77:                              # %df_regno_bb.exit
                                        #   in Loop: Header=BB33_59 Depth=1
	testq	%rcx, %rcx
	jne	.LBB33_78
# BB#79:                                # %if.else
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$5, %rsi
	movq	$1, %rdx
	movabsq	$.L.str22, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_80
.LBB33_78:                              # %if.then284
                                        #   in Loop: Header=BB33_59 Depth=1
	movl	88(%rcx), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str21, %rsi
	callq	fprintf
.LBB33_80:                              # %if.end289
                                        #   in Loop: Header=BB33_59 Depth=1
	testq	%r15, %rbx
	je	.LBB33_84
# BB#81:                                # %if.then292
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %r12
	andq	%r15, %r12
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	16(%rcx,%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str23, %rsi
	callq	fprintf
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB33_83
# BB#82:                                # %if.end311
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movl	20(%rcx,%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str25, %rsi
	callq	fprintf
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	jne	.LBB33_86
.LBB33_83:                              # %if.then325.thread
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str26, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_95
	.align	16, 0x90
.LBB33_84:                              # %lor.lhs.false299.critedge
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	48(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB33_92
# BB#85:                                # %if.end311.thread
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str24, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	%r13, %r12
	andq	%r15, %r12
.LBB33_86:                              # %if.then314
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r12), %rbx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str31, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_87
	.align	16, 0x90
.LBB33_90:                              # %select.end73
                                        #   in Loop: Header=BB33_87 Depth=2
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str28, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
.LBB33_87:                              # %if.then314
                                        #   Parent Loop BB33_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB33_91
# BB#88:                                # %for.body.i566
                                        #   in Loop: Header=BB33_87 Depth=2
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB33_90
# BB#89:                                # %select.mid74
                                        #   in Loop: Header=BB33_87 Depth=2
	movq	$117, %rdx
	jmp	.LBB33_90
	.align	16, 0x90
.LBB33_91:                              # %df_chain_dump.exit568
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB33_92:                              # %if.end319
                                        #   in Loop: Header=BB33_59 Depth=1
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB33_103
# BB#93:                                # %if.then325
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$6, %rsi
	movq	$1, %rdx
	movabsq	$.L.str26, %rdi
	movq	%r14, %rcx
	callq	fwrite
	testq	%r15, %rbx
	je	.LBB33_96
# BB#94:                                # %if.then325.if.then329_crit_edge
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%r13, %r12
	andq	%r15, %r12
.LBB33_95:                              # %if.then329
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	64(%rsp), %rax          # 8-byte Reload
	movl	24(%rax,%r12), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str25, %rsi
	callq	fprintf
.LBB33_96:                              # %if.end334
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	testq	%r15, %rax
	je	.LBB33_103
# BB#97:                                # %if.then337
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	%rbx, %r12
	movq	%r13, %rax
	andq	%r15, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx,%rax), %rbx
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str31, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB33_98
	.align	16, 0x90
.LBB33_101:                             # %select.end75
                                        #   in Loop: Header=BB33_98 Depth=2
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str28, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
.LBB33_98:                              # %if.then337
                                        #   Parent Loop BB33_59 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB33_102
# BB#99:                                # %for.body.i579
                                        #   in Loop: Header=BB33_98 Depth=2
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB33_101
# BB#100:                               # %select.mid76
                                        #   in Loop: Header=BB33_98 Depth=2
	movq	$117, %rdx
	jmp	.LBB33_101
	.align	16, 0x90
.LBB33_102:                             # %df_chain_dump.exit581
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	%r12, %rbx
.LBB33_103:                             # %if.end342
                                        #   in Loop: Header=BB33_59 Depth=1
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	96(%rax), %eax
.LBB33_104:                             # %for.inc345
                                        #   in Loop: Header=BB33_59 Depth=1
	incq	%r13
	movq	%r13, %rcx
	andq	%r15, %rcx
	movq	%rax, %rdx
	andq	%r15, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB33_59
.LBB33_105:                             # %if.end348
	movq	$10, %rdi
	movq	%r14, %rsi
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB33_106:                             # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp329:
	.size	df_dump, .Ltmp329-df_dump
	.cfi_endproc

	.globl	df_insn_debug
	.align	16, 0x90
	.type	df_insn_debug,@function
df_insn_debug:                          # @df_insn_debug
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp336:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp337:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp338:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp339:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp340:
	.cfi_def_cfa_offset 48
.Ltmp341:
	.cfi_offset %rbx, -48
.Ltmp342:
	.cfi_offset %r12, -40
.Ltmp343:
	.cfi_offset %r13, -32
.Ltmp344:
	.cfi_offset %r14, -24
.Ltmp345:
	.cfi_offset %r15, -16
	movl	8(%rsi), %r9d
	movq	%rdx, %r13
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r9, %rax
	movl	64(%r14), %edx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %r9
	jae	.LBB34_16
# BB#1:                                 # %if.end
	movq	56(%r14), %rdx
	leaq	(%r9,%r9,2), %r12
	movq	(%rdx,%r12,8), %rcx
	testq	%rcx, %rcx
	jne	.LBB34_4
# BB#2:                                 # %if.else
	movq	8(%rdx,%r12,8), %rcx
	testq	%rcx, %rcx
	je	.LBB34_3
.LBB34_4:                               # %if.then17
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rsi
	movq	32(%rsi,%rcx,8), %rcx
	movl	88(%rcx), %ecx
.LBB34_5:                               # %if.end34
	leaq	(%rax,%rax,2), %rax
	movl	16(%rdx,%rax,8), %r8d
	movabsq	$.L.str27, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r9, %rdx
	callq	fprintf
	movq	56(%r14), %rax
	movq	(%rax,%r12,8), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB34_10
# BB#6:
	movabsq	$.L.str28, %r15
	.align	16, 0x90
.LBB34_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB34_9
# BB#8:                                 # %select.mid
                                        #   in Loop: Header=BB34_7 Depth=1
	movq	$117, %rdx
.LBB34_9:                               # %select.end
                                        #   in Loop: Header=BB34_7 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB34_7
.LBB34_10:                              # %df_chain_dump.exit
	movq	$125, %rdi
	movq	%r13, %rsi
	callq	fputc
	movabsq	$.L.str26, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	56(%r14), %rax
	movq	8(%rax,%r12,8), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB34_15
# BB#11:
	movabsq	$.L.str28, %r14
	.align	16, 0x90
.LBB34_12:                              # %for.body.i78
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB34_14
# BB#13:                                # %select.mid7
                                        #   in Loop: Header=BB34_12 Depth=1
	movq	$117, %rdx
.LBB34_14:                              # %select.end6
                                        #   in Loop: Header=BB34_12 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB34_12
.LBB34_15:                              # %df_chain_dump.exit80
	movq	$125, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r13, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB34_16:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_3:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB34_5
.Ltmp346:
	.size	df_insn_debug, .Ltmp346-df_insn_debug
	.cfi_endproc

	.globl	df_insn_debug_regno
	.align	16, 0x90
	.type	df_insn_debug_regno,@function
df_insn_debug_regno:                    # @df_insn_debug_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp353:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp354:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp355:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp356:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp357:
	.cfi_def_cfa_offset 48
.Ltmp358:
	.cfi_offset %rbx, -48
.Ltmp359:
	.cfi_offset %r12, -40
.Ltmp360:
	.cfi_offset %r13, -32
.Ltmp361:
	.cfi_offset %r14, -24
.Ltmp362:
	.cfi_offset %r15, -16
	movl	8(%rsi), %r9d
	movq	%rdx, %r13
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%r9, %rax
	movl	64(%r14), %edx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %r9
	jae	.LBB35_20
# BB#1:                                 # %if.end
	movq	56(%r14), %rdx
	leaq	(%r9,%r9,2), %r12
	movq	(%rdx,%r12,8), %rcx
	testq	%rcx, %rcx
	jne	.LBB35_4
# BB#2:                                 # %if.else
	movq	8(%rdx,%r12,8), %rcx
	testq	%rcx, %rcx
	je	.LBB35_3
.LBB35_4:                               # %if.then17
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movslq	8(%rcx), %rcx
	movq	basic_block_for_insn(%rip), %rsi
	movq	32(%rsi,%rcx,8), %rcx
	movl	88(%rcx), %ecx
.LBB35_5:                               # %if.end34
	leaq	(%rax,%rax,2), %rax
	movl	16(%rdx,%rax,8), %r8d
	movabsq	$.L.str27, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r9, %rdx
	callq	fprintf
	movq	56(%r14), %rax
	movq	(%rax,%r12,8), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB35_12
# BB#6:
	movabsq	$.L.str32, %r15
	.align	16, 0x90
.LBB35_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB35_9
# BB#8:                                 # %select.mid
                                        #   in Loop: Header=BB35_7 Depth=1
	movq	$117, %rdx
.LBB35_9:                               # %select.end
                                        #   in Loop: Header=BB35_7 Depth=1
	movq	(%rax), %rsi
	movzbl	(%rsi), %edi
	movl	36(%rax), %ecx
	cmpq	$63, %rdi
	jne	.LBB35_11
# BB#10:                                # %cond.true.i
                                        #   in Loop: Header=BB35_7 Depth=1
	movq	8(%rsi), %rsi
.LBB35_11:                              # %cond.end.i
                                        #   in Loop: Header=BB35_7 Depth=1
	movl	8(%rsi), %r8d
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_7
.LBB35_12:                              # %df_chain_dump_regno.exit
	movq	$125, %rdi
	movq	%r13, %rsi
	callq	fputc
	movabsq	$.L.str26, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	movq	56(%r14), %rax
	movq	8(%rax,%r12,8), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r13, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB35_19
# BB#13:
	movabsq	$.L.str32, %r14
	.align	16, 0x90
.LBB35_14:                              # %for.body.i79
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB35_16
# BB#15:                                # %select.mid8
                                        #   in Loop: Header=BB35_14 Depth=1
	movq	$117, %rdx
.LBB35_16:                              # %select.end7
                                        #   in Loop: Header=BB35_14 Depth=1
	movq	(%rax), %rsi
	movzbl	(%rsi), %edi
	movl	36(%rax), %ecx
	cmpq	$63, %rdi
	jne	.LBB35_18
# BB#17:                                # %cond.true.i82
                                        #   in Loop: Header=BB35_14 Depth=1
	movq	8(%rsi), %rsi
.LBB35_18:                              # %cond.end.i89
                                        #   in Loop: Header=BB35_14 Depth=1
	movl	8(%rsi), %r8d
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_14
.LBB35_19:                              # %df_chain_dump_regno.exit91
	movq	$125, %rdi
	movq	%r13, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r13, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB35_20:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB35_3:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB35_5
.Ltmp363:
	.size	df_insn_debug_regno, .Ltmp363-df_insn_debug_regno
	.cfi_endproc

	.globl	debug_df_insn
	.align	16, 0x90
	.type	debug_df_insn,@function
debug_df_insn:                          # @debug_df_insn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp366:
	.cfi_def_cfa_offset 16
.Ltmp367:
	.cfi_offset %rbx, -16
	movq	ddf(%rip), %rax
	movq	stderr(%rip), %rdx
	movq	%rdi, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	df_insn_debug
	movq	%rbx, %rdi
	popq	%rbx
	jmp	debug_rtx  # TAILCALL
.Ltmp368:
	.size	debug_df_insn, .Ltmp368-debug_df_insn
	.cfi_endproc

	.globl	debug_df_reg
	.align	16, 0x90
	.type	debug_df_reg,@function
debug_df_reg:                           # @debug_df_reg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp375:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp376:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp377:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp378:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp379:
	.cfi_def_cfa_offset 48
.Ltmp380:
	.cfi_offset %rbx, -48
.Ltmp381:
	.cfi_offset %r12, -40
.Ltmp382:
	.cfi_offset %r13, -32
.Ltmp383:
	.cfi_offset %r14, -24
.Ltmp384:
	.cfi_offset %r15, -16
	movq	ddf(%rip), %r12
	movl	8(%rdi), %r13d
	movl	48(%r12), %eax
	cmpq	%r13, %rax
	ja	.LBB37_1
# BB#12:                                # %df_regno_debug.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB37_1:                               # %if.end.i
	movq	40(%r12), %rax
	movq	stderr(%rip), %r14
	movq	$5, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movl	16(%rax,%rbx), %ecx
	movabsq	$.L.str30, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	fprintf
	movq	40(%r12), %rax
	movq	(%rax,%rbx), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB37_6
# BB#2:
	movabsq	$.L.str28, %r15
	.align	16, 0x90
.LBB37_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB37_5
# BB#4:                                 # %select.mid
                                        #   in Loop: Header=BB37_3 Depth=1
	movq	$117, %rdx
.LBB37_5:                               # %select.end
                                        #   in Loop: Header=BB37_3 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB37_3
.LBB37_6:                               # %df_chain_dump.exit
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movabsq	$.L.str26, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	40(%r12), %rax
	movq	$5, %rcx
	shlq	%cl, %r13
	movq	8(%rax,%r13), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	jne	.LBB37_7
.LBB37_11:                              # %df_chain_dump.exit15
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB37_7:
	movabsq	$.L.str28, %r15
.LBB37_8:                               # %for.body.i13
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB37_10
# BB#9:                                 # %select.mid4
                                        #   in Loop: Header=BB37_8 Depth=1
	movq	$117, %rdx
.LBB37_10:                              # %select.end3
                                        #   in Loop: Header=BB37_8 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB37_8
	jmp	.LBB37_11
.Ltmp385:
	.size	debug_df_reg, .Ltmp385-debug_df_reg
	.cfi_endproc

	.globl	debug_df_regno
	.align	16, 0x90
	.type	debug_df_regno,@function
debug_df_regno:                         # @debug_df_regno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp392:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp393:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp394:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp395:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 48
.Ltmp397:
	.cfi_offset %rbx, -48
.Ltmp398:
	.cfi_offset %r12, -40
.Ltmp399:
	.cfi_offset %r13, -32
.Ltmp400:
	.cfi_offset %r14, -24
.Ltmp401:
	.cfi_offset %r15, -16
	movq	ddf(%rip), %r12
	movl	48(%r12), %eax
	movq	%rdi, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rdx, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	ja	.LBB38_1
# BB#12:                                # %df_regno_debug.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB38_1:                               # %if.end.i
	movq	stderr(%rip), %r14
	movq	40(%r12), %rax
	andq	%rdx, %r13
	movq	$5, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movl	16(%rax,%rbx), %ecx
	movabsq	$.L.str30, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movq	40(%r12), %rax
	movq	(%rax,%rbx), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB38_6
# BB#2:
	movabsq	$.L.str28, %r15
	.align	16, 0x90
.LBB38_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB38_5
# BB#4:                                 # %select.mid
                                        #   in Loop: Header=BB38_3 Depth=1
	movq	$117, %rdx
.LBB38_5:                               # %select.end
                                        #   in Loop: Header=BB38_3 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB38_3
.LBB38_6:                               # %df_chain_dump.exit
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movabsq	$.L.str26, %rdi
	movq	$6, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	40(%r12), %rax
	movq	$5, %rcx
	shlq	%cl, %r13
	movq	8(%rax,%r13), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	jne	.LBB38_7
.LBB38_11:                              # %df_chain_dump.exit15
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB38_7:
	movabsq	$.L.str28, %r15
.LBB38_8:                               # %for.body.i13
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB38_10
# BB#9:                                 # %select.mid4
                                        #   in Loop: Header=BB38_8 Depth=1
	movq	$117, %rdx
.LBB38_10:                              # %select.end3
                                        #   in Loop: Header=BB38_8 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB38_8
	jmp	.LBB38_11
.Ltmp402:
	.size	debug_df_regno, .Ltmp402-debug_df_regno
	.cfi_endproc

	.globl	debug_df_ref
	.align	16, 0x90
	.type	debug_df_ref,@function
debug_df_ref:                           # @debug_df_ref
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ddf(%rip), %rax
	movq	stderr(%rip), %rdx
	movq	%rdi, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	jmp	df_ref_debug  # TAILCALL
.Ltmp403:
	.size	debug_df_ref, .Ltmp403-debug_df_ref
	.cfi_endproc

	.align	16, 0x90
	.type	df_ref_debug,@function
df_ref_debug:                           # @df_ref_debug
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp409:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp410:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp411:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp412:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp413:
	.cfi_def_cfa_offset 48
.Ltmp414:
	.cfi_offset %rbx, -40
.Ltmp415:
	.cfi_offset %r12, -32
.Ltmp416:
	.cfi_offset %r14, -24
.Ltmp417:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movl	32(%r14), %eax
	movq	%rdx, %r12
	movq	%rdi, %r15
	testq	%rax, %rax
	movq	$100, %rdx
	je	.LBB40_2
# BB#1:                                 # %select.mid
	movq	$117, %rdx
.LBB40_2:                               # %select.end
	movl	36(%r14), %ecx
	movabsq	$.L.str28, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	movq	(%r14), %rax
	movzbl	(%rax), %ecx
	cmpq	$63, %rcx
	jne	.LBB40_4
# BB#3:                                 # %cond.true
	movq	8(%rax), %rax
.LBB40_4:                               # %cond.end
	movq	8(%r14), %rcx
	movslq	8(%rcx), %r9
	movq	basic_block_for_insn(%rip), %rcx
	movq	32(%rcx,%r9,8), %rcx
	movq	56(%r15), %rdx
	leaq	(%r9,%r9,2), %rsi
	movl	16(%rdx,%rsi,8), %r8d
	movl	88(%rcx), %ecx
	movl	8(%rax), %edx
	movabsq	$.L.str29, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	fprintf
	movq	24(%r14), %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r12, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	je	.LBB40_9
# BB#5:
	movabsq	$.L.str28, %r14
	.align	16, 0x90
.LBB40_6:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB40_8
# BB#7:                                 # %select.mid2
                                        #   in Loop: Header=BB40_6 Depth=1
	movq	$117, %rdx
.LBB40_8:                               # %select.end1
                                        #   in Loop: Header=BB40_6 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB40_6
.LBB40_9:                               # %df_chain_dump.exit
	movq	$125, %rdi
	movq	%r12, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r12, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp418:
	.size	df_ref_debug, .Ltmp418-df_ref_debug
	.cfi_endproc

	.globl	debug_df_defno
	.align	16, 0x90
	.type	debug_df_defno,@function
debug_df_defno:                         # @debug_df_defno
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ddf(%rip), %rax
	movq	16(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	movq	(%rcx,%rdx,8), %rsi
	movq	stderr(%rip), %rdx
	movq	%rax, %rdi
	jmp	df_ref_debug  # TAILCALL
.Ltmp419:
	.size	debug_df_defno, .Ltmp419-debug_df_defno
	.cfi_endproc

	.globl	debug_df_useno
	.align	16, 0x90
	.type	debug_df_useno,@function
debug_df_useno:                         # @debug_df_useno
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ddf(%rip), %rax
	movq	24(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	movq	(%rcx,%rdx,8), %rsi
	movq	stderr(%rip), %rdx
	movq	%rax, %rdi
	jmp	df_ref_debug  # TAILCALL
.Ltmp420:
	.size	debug_df_useno, .Ltmp420-debug_df_useno
	.cfi_endproc

	.globl	debug_df_chain
	.align	16, 0x90
	.type	debug_df_chain,@function
debug_df_chain:                         # @debug_df_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp425:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp426:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp427:
	.cfi_def_cfa_offset 32
.Ltmp428:
	.cfi_offset %rbx, -32
.Ltmp429:
	.cfi_offset %r14, -24
.Ltmp430:
	.cfi_offset %r15, -16
	movq	stderr(%rip), %r14
	movq	%rdi, %rbx
	movabsq	$.L.str31, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	testq	%rbx, %rbx
	jne	.LBB43_1
.LBB43_5:                               # %df_chain_dump.exit
	movq	$125, %rdi
	movq	%r14, %rsi
	callq	fputc
	movq	$10, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB43_1:
	movabsq	$.L.str28, %r15
	.align	16, 0x90
.LBB43_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movl	32(%rax), %ecx
	testq	%rcx, %rcx
	movq	$100, %rdx
	je	.LBB43_4
# BB#3:                                 # %select.mid
                                        #   in Loop: Header=BB43_2 Depth=1
	movq	$117, %rdx
.LBB43_4:                               # %select.end
                                        #   in Loop: Header=BB43_2 Depth=1
	movl	36(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB43_2
	jmp	.LBB43_5
.Ltmp431:
	.size	debug_df_chain, .Ltmp431-debug_df_chain
	.cfi_endproc

	.globl	iterative_dataflow_sbitmap
	.align	16, 0x90
	.type	iterative_dataflow_sbitmap,@function
iterative_dataflow_sbitmap:             # @iterative_dataflow_sbitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp438:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp439:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp440:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp441:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp442:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp443:
	.cfi_def_cfa_offset 176
.Ltmp444:
	.cfi_offset %rbx, -48
.Ltmp445:
	.cfi_offset %r12, -40
.Ltmp446:
	.cfi_offset %r13, -32
.Ltmp447:
	.cfi_offset %r14, -24
.Ltmp448:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	%r8, %r14
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r15
	movq	%r15, 112(%rsp)         # 8-byte Spill
	callq	sbitmap_alloc
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	sbitmap_zero
	movq	%rbx, %rdi
	callq	sbitmap_zero
	callq	fibheap_new
	movq	(%r14), %rdx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	jmp	.LBB44_1
	.align	16, 0x90
.LBB44_37:                              # %if.end47.1
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	(%rdx), %rdx
.LBB44_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB44_3 Depth 2
                                        #     Child Loop BB44_26 Depth 2
	testq	%rdx, %rdx
	je	.LBB44_6
# BB#2:                                 # %for.cond8.preheader
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	24(%rdx), %r12
	testq	%r12, %r12
	movq	$1, %r15
	je	.LBB44_25
	.align	16, 0x90
.LBB44_3:                               # %for.body15
                                        #   Parent Loop BB44_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r15), %r13
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB44_24
# BB#4:                                 # %if.then17
                                        #   in Loop: Header=BB44_3 Depth=2
	movl	16(%rdx), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %r13
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rbx,4), %rsi
	notq	%rax
	andq	%rax, %r12
	movq	%r13, %r14
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	fibheap_insert
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	112(%rsp), %rcx         # 8-byte Reload
	orq	%rax, 16(%rcx,%r14)
	movq	104(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB44_22
# BB#5:                                 # %if.then32
                                        #   in Loop: Header=BB44_3 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB44_23
	.align	16, 0x90
.LBB44_22:                              # %if.else
                                        #   in Loop: Header=BB44_3 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB44_23:                              # %if.end41
                                        #   in Loop: Header=BB44_3 Depth=2
	movq	(%rax,%rbx,8), %rdi
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	callq	sbitmap_copy
	testq	%r12, %r12
	movq	88(%rsp), %rdx          # 8-byte Reload
	je	.LBB44_25
.LBB44_24:                              # %for.inc
                                        #   in Loop: Header=BB44_3 Depth=2
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r15
	cmpq	$64, %rax
	jb	.LBB44_3
.LBB44_25:                              # %if.end47
                                        #   in Loop: Header=BB44_1 Depth=1
	movq	32(%rdx), %r15
	xorq	%r13, %r13
	testq	%r15, %r15
	je	.LBB44_37
	.align	16, 0x90
.LBB44_26:                              # %for.body15.1
                                        #   Parent Loop BB44_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%r15, %rax
	je	.LBB44_36
# BB#27:                                # %if.then17.1
                                        #   in Loop: Header=BB44_26 Depth=2
	movl	16(%rdx), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%r13,%rdx), %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rbx,4), %rsi
	notq	%rax
	andq	%rax, %r15
	movq	%r12, %r14
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	fibheap_insert
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	112(%rsp), %rcx         # 8-byte Reload
	orq	%rax, 16(%rcx,%r14)
	movq	104(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB44_28
# BB#34:                                # %if.then32.1
                                        #   in Loop: Header=BB44_26 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB44_35
	.align	16, 0x90
.LBB44_28:                              # %if.else.1
                                        #   in Loop: Header=BB44_26 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB44_35:                              # %if.end41.1
                                        #   in Loop: Header=BB44_26 Depth=2
	movq	(%rax,%rbx,8), %rdi
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	callq	sbitmap_copy
	testq	%r15, %r15
	movq	88(%rsp), %rdx          # 8-byte Reload
	je	.LBB44_37
.LBB44_36:                              # %for.inc.1
                                        #   in Loop: Header=BB44_26 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB44_26
	jmp	.LBB44_37
.LBB44_6:                               # %while.cond54.preheader
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	%r14, %rax
	movq	64(%rsp), %r15          # 8-byte Reload
	jmp	.LBB44_7
	.align	16, 0x90
.LBB44_29:                              # %do.end162
                                        #   in Loop: Header=BB44_7 Depth=1
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	sbitmap_zero
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	andq	%r14, %rax
	cmpq	%r14, %rax
.LBB44_7:                               # %while.cond54.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB44_9 Depth 2
                                        #     Child Loop BB44_14 Depth 2
                                        #       Child Loop BB44_16 Depth 3
                                        #       Child Loop BB44_20 Depth 3
	movq	120(%rsp), %rbx         # 8-byte Reload
	je	.LBB44_30
# BB#8:                                 # %while.cond59.preheader
                                        #   in Loop: Header=BB44_7 Depth=1
	movq	%rbx, %rdi
	callq	fibheap_empty
	testq	%r14, %rax
	movq	%r14, %r12
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	200(%rsp), %r13
	jne	.LBB44_12
	.align	16, 0x90
.LBB44_9:                               # %while.body61
                                        #   Parent Loop BB44_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	fibheap_extract_min
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rdi
	movl	88(%rdi), %ecx
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r15,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB44_11
# BB#10:                                # %if.then77
                                        #   in Loop: Header=BB44_9 Depth=2
	movq	%r13, 32(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	%r15, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	hybrid_search_sbitmap
.LBB44_11:                              # %while.cond59.backedge
                                        #   in Loop: Header=BB44_9 Depth=2
	movq	%rbx, %rdi
	callq	fibheap_empty
	testq	%r12, %rax
	je	.LBB44_9
.LBB44_12:                              # %while.end79
                                        #   in Loop: Header=BB44_7 Depth=1
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	andq	%r12, %rax
	cmpq	%r12, %rax
	je	.LBB44_30
# BB#13:                                # %for.cond109.preheader
                                        #   in Loop: Header=BB44_7 Depth=1
	movq	%r12, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r13
	jmp	.LBB44_14
	.align	16, 0x90
.LBB44_33:                              # %if.end155.1
                                        #   in Loop: Header=BB44_14 Depth=2
	movq	%rdx, %r14
	movq	(%r13), %r13
.LBB44_14:                              # %for.cond109.preheader
                                        #   Parent Loop BB44_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB44_16 Depth 3
                                        #       Child Loop BB44_20 Depth 3
	testq	%r13, %r13
	je	.LBB44_29
# BB#15:                                # %for.cond113.preheader
                                        #   in Loop: Header=BB44_14 Depth=2
	movq	24(%r13), %r15
	testq	%r15, %r15
	movq	$1, %rbx
	movq	%r14, %rdx
	je	.LBB44_19
	.align	16, 0x90
.LBB44_16:                              # %for.body127
                                        #   Parent Loop BB44_7 Depth=1
                                        #     Parent Loop BB44_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rax
	movq	$1, %r14
	movq	%rax, %rcx
	shlq	%cl, %r14
	testq	%r15, %r14
	je	.LBB44_18
# BB#17:                                # %if.then134
                                        #   in Loop: Header=BB44_16 Depth=3
	movq	%rdx, %r12
	movl	16(%r13), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rax,4), %rsi
	notq	%r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rdx
	callq	fibheap_insert
	movq	%r12, %rdx
	andq	%r14, %r15
	je	.LBB44_19
.LBB44_18:                              # %for.inc152
                                        #   in Loop: Header=BB44_16 Depth=3
	movq	%rbx, %rax
	andq	%rdx, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB44_16
.LBB44_19:                              # %if.end155
                                        #   in Loop: Header=BB44_14 Depth=2
	movq	32(%r13), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB44_33
	.align	16, 0x90
.LBB44_20:                              # %for.body127.1
                                        #   Parent Loop BB44_7 Depth=1
                                        #     Parent Loop BB44_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %r12
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	testq	%r15, %r14
	jne	.LBB44_31
# BB#21:                                #   in Loop: Header=BB44_20 Depth=3
	movq	%r12, %rdx
	jmp	.LBB44_32
	.align	16, 0x90
.LBB44_31:                              # %if.then134.1
                                        #   in Loop: Header=BB44_20 Depth=3
	movl	16(%r13), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	192(%rsp), %rax
	movslq	(%rax,%rdx,4), %rsi
	notq	%r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	fibheap_insert
	andq	%r14, %r15
	movq	%r12, %rdx
	je	.LBB44_33
.LBB44_32:                              # %for.inc152.1
                                        #   in Loop: Header=BB44_20 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	cmpq	$64, %rax
	jb	.LBB44_20
	jmp	.LBB44_33
.LBB44_30:                              # %while.end165
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	%r15, %rdi
	callq	free
	movq	%rbx, %rdi
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fibheap_delete  # TAILCALL
.Ltmp449:
	.size	iterative_dataflow_sbitmap, .Ltmp449-iterative_dataflow_sbitmap
	.cfi_endproc

	.align	16, 0x90
	.type	hybrid_search_sbitmap,@function
hybrid_search_sbitmap:                  # @hybrid_search_sbitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp456:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp457:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp458:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp459:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp460:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp461:
	.cfi_def_cfa_offset 176
.Ltmp462:
	.cfi_offset %rbx, -48
.Ltmp463:
	.cfi_offset %r12, -40
.Ltmp464:
	.cfi_offset %r13, -32
.Ltmp465:
	.cfi_offset %r14, -24
.Ltmp466:
	.cfi_offset %r15, -16
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movq	208(%rsp), %r12
	movl	88(%r15), %r13d
	movq	200(%rsp), %rax
	movq	192(%rsp), %r10
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$1, %rdi
	movq	%r13, %rcx
	shlq	%cl, %rdi
	movq	$6, %rcx
	movq	%r13, %rbx
	shrq	%cl, %rbx
	orq	%rdi, 16(%r10,%rbx,8)
	movq	16(%rax,%rbx,8), %rax
	movq	%r9, 96(%rsp)           # 8-byte Spill
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	testq	%rax, %rdi
	je	.LBB45_34
# BB#1:                                 # %if.then
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB45_2
# BB#11:                                # %if.else
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%r14,8), %rdi
	leaq	(%rax,%r14,8), %r13
	callq	sbitmap_zero
	movq	%r15, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB45_19
# BB#12:
	movabsq	$entry_exit_blocks+112, %r12
	movq	176(%rsp), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	.align	16, 0x90
.LBB45_13:                              # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	cmpq	%r12, %rax
	je	.LBB45_18
# BB#14:                                # %if.end42
                                        #   in Loop: Header=BB45_13 Depth=1
	cmpq	$1, %r15
	jne	.LBB45_15
# BB#17:                                # %sw.bb53
                                        #   in Loop: Header=BB45_13 Depth=1
	movslq	88(%rax), %rax
	movq	(%r13), %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	jmp	.LBB45_18
	.align	16, 0x90
.LBB45_15:                              # %if.end42
                                        #   in Loop: Header=BB45_13 Depth=1
	movq	176(%rsp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB45_18
# BB#16:                                # %sw.bb43
                                        #   in Loop: Header=BB45_13 Depth=1
	movslq	88(%rax), %rax
	movq	(%r13), %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_or_b
	.align	16, 0x90
.LBB45_18:                              # %for.inc64
                                        #   in Loop: Header=BB45_13 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_13
.LBB45_19:                              # %if.end66.loopexit319
	movq	112(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r14,8), %r12
	jmp	.LBB45_20
.LBB45_2:                               # %if.then11
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%r14,8), %rdi
	leaq	(%rax,%r14,8), %r12
	callq	sbitmap_zero
	movq	%r15, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB45_10
# BB#3:
	movabsq	$entry_exit_blocks, %r13
	movq	176(%rsp), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	.align	16, 0x90
.LBB45_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rax
	cmpq	%r13, %rax
	je	.LBB45_9
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB45_4 Depth=1
	cmpq	$1, %r15
	jne	.LBB45_6
# BB#8:                                 # %sw.bb25
                                        #   in Loop: Header=BB45_4 Depth=1
	movslq	88(%rax), %rax
	movq	(%r12), %rdi
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_and_b
	jmp	.LBB45_9
	.align	16, 0x90
.LBB45_6:                               # %if.end
                                        #   in Loop: Header=BB45_4 Depth=1
	movq	176(%rsp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB45_9
# BB#7:                                 # %sw.bb
                                        #   in Loop: Header=BB45_4 Depth=1
	movslq	88(%rax), %rax
	movq	(%r12), %rdi
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	%rdi, %rsi
	callq	sbitmap_a_or_b
	.align	16, 0x90
.LBB45_9:                               # %for.inc
                                        #   in Loop: Header=BB45_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_4
.LBB45_10:                              # %if.end66.loopexit
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r14,8), %r13
.LBB45_20:                              # %if.end66
	movq	(%r12), %rdx
	movq	(%r13), %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14,8), %r8
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14,8), %r9
	leaq	124(%rsp), %rsi
	movq	208(%rsp), %r12
	movq	%r12, (%rsp)
	movq	72(%rsp), %r15          # 8-byte Reload
	movl	88(%r15), %r13d
	movq	%r13, %rdi
	movq	184(%rsp), %rax
	callq	*%rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	notq	%rcx
	movq	200(%rsp), %rax
	movq	56(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, 16(%rax,%rdx,8)
	movl	124(%rsp), %eax
	testq	%rax, %rax
	movq	192(%rsp), %r10
	je	.LBB45_34
# BB#21:                                # %if.then84
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB45_22
# BB#28:                                # %if.else113
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB45_34
# BB#29:
	movabsq	$entry_exit_blocks, %rdx
	.align	16, 0x90
.LBB45_30:                              # %for.body117
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB45_33
# BB#31:                                # %lor.lhs.false120
                                        #   in Loop: Header=BB45_30 Depth=1
	movq	24(%rax), %rsi
	movl	88(%rsi), %esi
	cmpq	%r13, %rsi
	je	.LBB45_33
# BB#32:                                # %if.end125
                                        #   in Loop: Header=BB45_30 Depth=1
	movl	88(%rcx), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	movq	200(%rsp), %rdi
	orq	%rsi, 16(%rdi,%rcx,8)
.LBB45_33:                              # %for.inc138
                                        #   in Loop: Header=BB45_30 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB45_30
	jmp	.LBB45_34
.LBB45_22:                              # %if.then86
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.LBB45_35
# BB#23:
	movabsq	$entry_exit_blocks+112, %rdx
	.align	16, 0x90
.LBB45_24:                              # %for.body90
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB45_27
# BB#25:                                # %lor.lhs.false
                                        #   in Loop: Header=BB45_24 Depth=1
	movl	88(%rcx), %ecx
	cmpq	%r13, %rcx
	je	.LBB45_27
# BB#26:                                # %if.end97
                                        #   in Loop: Header=BB45_24 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	movq	200(%rsp), %rdi
	orq	%rsi, 16(%rdi,%rcx,8)
.LBB45_27:                              # %for.inc110
                                        #   in Loop: Header=BB45_24 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB45_24
.LBB45_34:                              # %if.end143
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB45_35
# BB#42:                                # %if.else177
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB45_49
# BB#43:
	movabsq	$entry_exit_blocks, %r14
	.align	16, 0x90
.LBB45_44:                              # %for.body181
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB45_48
# BB#45:                                # %lor.lhs.false184
                                        #   in Loop: Header=BB45_44 Depth=1
	movl	88(%rdi), %ecx
	cmpq	%r13, %rcx
	je	.LBB45_48
# BB#46:                                # %if.end189
                                        #   in Loop: Header=BB45_44 Depth=1
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r10,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB45_48
# BB#47:                                # %if.then203
                                        #   in Loop: Header=BB45_44 Depth=1
	movq	%r12, 32(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	%r10, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	96(%rsp), %r9           # 8-byte Reload
	movq	%r10, %r15
	callq	hybrid_search_sbitmap
	movq	%r15, %r10
	.align	16, 0x90
.LBB45_48:                              # %for.inc206
                                        #   in Loop: Header=BB45_44 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_44
	jmp	.LBB45_49
.LBB45_35:                              # %if.then145
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB45_49
# BB#36:
	movabsq	$entry_exit_blocks+112, %r14
	.align	16, 0x90
.LBB45_37:                              # %for.body149
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB45_41
# BB#38:                                # %lor.lhs.false152
                                        #   in Loop: Header=BB45_37 Depth=1
	movl	88(%rdi), %ecx
	cmpq	%r13, %rcx
	je	.LBB45_41
# BB#39:                                # %if.end157
                                        #   in Loop: Header=BB45_37 Depth=1
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r10,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB45_41
# BB#40:                                # %if.then171
                                        #   in Loop: Header=BB45_37 Depth=1
	movq	%r12, 32(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	%r10, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	xorq	%r9, %r9
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	%r10, %r15
	callq	hybrid_search_sbitmap
	movq	%r15, %r10
	.align	16, 0x90
.LBB45_41:                              # %for.inc174
                                        #   in Loop: Header=BB45_37 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_37
.LBB45_49:                              # %if.end209
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp467:
	.size	hybrid_search_sbitmap, .Ltmp467-hybrid_search_sbitmap
	.cfi_endproc

	.globl	iterative_dataflow_bitmap
	.align	16, 0x90
	.type	iterative_dataflow_bitmap,@function
iterative_dataflow_bitmap:              # @iterative_dataflow_bitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp474:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp475:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp476:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp477:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp478:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp479:
	.cfi_def_cfa_offset 176
.Ltmp480:
	.cfi_offset %rbx, -48
.Ltmp481:
	.cfi_offset %r12, -40
.Ltmp482:
	.cfi_offset %r13, -32
.Ltmp483:
	.cfi_offset %r14, -24
.Ltmp484:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	movq	%r9, 104(%rsp)          # 8-byte Spill
	movq	%r8, %r14
	movq	%r14, 48(%rsp)          # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 96(%rsp)          # 8-byte Spill
	movq	%rsi, 80(%rsp)          # 8-byte Spill
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	sbitmap_alloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, %r15
	movq	%r15, 112(%rsp)         # 8-byte Spill
	callq	sbitmap_alloc
	movq	%rax, %rbx
	movq	%rbx, 64(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	sbitmap_zero
	movq	%rbx, %rdi
	callq	sbitmap_zero
	callq	fibheap_new
	movq	(%r14), %rdx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	jmp	.LBB46_1
	.align	16, 0x90
.LBB46_37:                              # %if.end47.1
                                        #   in Loop: Header=BB46_1 Depth=1
	movq	(%rdx), %rdx
.LBB46_1:                               # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_3 Depth 2
                                        #     Child Loop BB46_26 Depth 2
	testq	%rdx, %rdx
	je	.LBB46_6
# BB#2:                                 # %for.cond8.preheader
                                        #   in Loop: Header=BB46_1 Depth=1
	movq	%rdx, 88(%rsp)          # 8-byte Spill
	movq	24(%rdx), %r12
	testq	%r12, %r12
	movq	$1, %r15
	je	.LBB46_25
	.align	16, 0x90
.LBB46_3:                               # %for.body15
                                        #   Parent Loop BB46_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	-1(%r15), %r13
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%r12, %rax
	je	.LBB46_24
# BB#4:                                 # %if.then17
                                        #   in Loop: Header=BB46_3 Depth=2
	movl	16(%rdx), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %r13
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rbx,4), %rsi
	notq	%rax
	andq	%rax, %r12
	movq	%r13, %r14
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	fibheap_insert
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	112(%rsp), %rcx         # 8-byte Reload
	orq	%rax, 16(%rcx,%r14)
	movq	104(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_22
# BB#5:                                 # %if.then32
                                        #   in Loop: Header=BB46_3 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB46_23
	.align	16, 0x90
.LBB46_22:                              # %if.else
                                        #   in Loop: Header=BB46_3 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB46_23:                              # %if.end41
                                        #   in Loop: Header=BB46_3 Depth=2
	movq	(%rax,%rbx,8), %rdi
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	callq	bitmap_copy
	testq	%r12, %r12
	movq	88(%rsp), %rdx          # 8-byte Reload
	je	.LBB46_25
.LBB46_24:                              # %for.inc
                                        #   in Loop: Header=BB46_3 Depth=2
	movq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r15
	cmpq	$64, %rax
	jb	.LBB46_3
.LBB46_25:                              # %if.end47
                                        #   in Loop: Header=BB46_1 Depth=1
	movq	32(%rdx), %r15
	xorq	%r13, %r13
	testq	%r15, %r15
	je	.LBB46_37
	.align	16, 0x90
.LBB46_26:                              # %for.body15.1
                                        #   Parent Loop BB46_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$1, %rax
	movq	%r13, %rcx
	shlq	%cl, %rax
	testq	%r15, %rax
	je	.LBB46_36
# BB#27:                                # %if.then17.1
                                        #   in Loop: Header=BB46_26 Depth=2
	movl	16(%rdx), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%r13,%rdx), %r12
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rbx,4), %rsi
	notq	%rax
	andq	%rax, %r15
	movq	%r12, %r14
	movabsq	$4294967232, %rax       # imm = 0xFFFFFFC0
	andq	%rax, %r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rdx
	callq	fibheap_insert
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	movq	$3, %rcx
	shrq	%cl, %r14
	movq	112(%rsp), %rcx         # 8-byte Reload
	orq	%rax, 16(%rcx,%r14)
	movq	104(%rsp), %rax         # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB46_28
# BB#34:                                # %if.then32.1
                                        #   in Loop: Header=BB46_26 Depth=2
	movq	80(%rsp), %rax          # 8-byte Reload
	jmp	.LBB46_35
	.align	16, 0x90
.LBB46_28:                              # %if.else.1
                                        #   in Loop: Header=BB46_26 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
.LBB46_35:                              # %if.end41.1
                                        #   in Loop: Header=BB46_26 Depth=2
	movq	(%rax,%rbx,8), %rdi
	movq	96(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%rbx,8), %rsi
	callq	bitmap_copy
	testq	%r15, %r15
	movq	88(%rsp), %rdx          # 8-byte Reload
	je	.LBB46_37
.LBB46_36:                              # %for.inc.1
                                        #   in Loop: Header=BB46_26 Depth=2
	incq	%r13
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$64, %rax
	jb	.LBB46_26
	jmp	.LBB46_37
.LBB46_6:                               # %while.cond54.preheader
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	%r14, %rax
	movq	64(%rsp), %r15          # 8-byte Reload
	jmp	.LBB46_7
	.align	16, 0x90
.LBB46_29:                              # %do.end162
                                        #   in Loop: Header=BB46_7 Depth=1
	movq	64(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rdi
	callq	sbitmap_zero
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	andq	%r14, %rax
	cmpq	%r14, %rax
.LBB46_7:                               # %while.cond54.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_9 Depth 2
                                        #     Child Loop BB46_14 Depth 2
                                        #       Child Loop BB46_16 Depth 3
                                        #       Child Loop BB46_20 Depth 3
	movq	120(%rsp), %rbx         # 8-byte Reload
	je	.LBB46_30
# BB#8:                                 # %while.cond59.preheader
                                        #   in Loop: Header=BB46_7 Depth=1
	movq	%rbx, %rdi
	callq	fibheap_empty
	testq	%r14, %rax
	movq	%r14, %r12
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	200(%rsp), %r13
	jne	.LBB46_12
	.align	16, 0x90
.LBB46_9:                               # %while.body61
                                        #   Parent Loop BB46_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	fibheap_extract_min
	movq	$32, %rcx
	movq	basic_block_info(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	32(%rdx,%rax,8), %rdi
	movl	88(%rdi), %ecx
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r15,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rax, %rdx
	jne	.LBB46_11
# BB#10:                                # %if.then77
                                        #   in Loop: Header=BB46_9 Depth=2
	movq	%r13, 32(%rsp)
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	%rax, 24(%rsp)
	movq	%r15, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	72(%rsp), %rsi          # 8-byte Reload
	movq	80(%rsp), %rdx          # 8-byte Reload
	movq	96(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	movq	104(%rsp), %r9          # 8-byte Reload
	callq	hybrid_search_bitmap
.LBB46_11:                              # %while.cond59.backedge
                                        #   in Loop: Header=BB46_9 Depth=2
	movq	%rbx, %rdi
	callq	fibheap_empty
	testq	%r12, %rax
	je	.LBB46_9
.LBB46_12:                              # %while.end79
                                        #   in Loop: Header=BB46_7 Depth=1
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	sbitmap_first_set_bit
	andq	%r12, %rax
	cmpq	%r12, %rax
	je	.LBB46_30
# BB#13:                                # %for.cond109.preheader
                                        #   in Loop: Header=BB46_7 Depth=1
	movq	%r12, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %r13
	jmp	.LBB46_14
	.align	16, 0x90
.LBB46_33:                              # %if.end155.1
                                        #   in Loop: Header=BB46_14 Depth=2
	movq	%rdx, %r14
	movq	(%r13), %r13
.LBB46_14:                              # %for.cond109.preheader
                                        #   Parent Loop BB46_7 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB46_16 Depth 3
                                        #       Child Loop BB46_20 Depth 3
	testq	%r13, %r13
	je	.LBB46_29
# BB#15:                                # %for.cond113.preheader
                                        #   in Loop: Header=BB46_14 Depth=2
	movq	24(%r13), %r15
	testq	%r15, %r15
	movq	$1, %rbx
	movq	%r14, %rdx
	je	.LBB46_19
	.align	16, 0x90
.LBB46_16:                              # %for.body127
                                        #   Parent Loop BB46_7 Depth=1
                                        #     Parent Loop BB46_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	leaq	-1(%rbx), %rax
	movq	$1, %r14
	movq	%rax, %rcx
	shlq	%cl, %r14
	testq	%r15, %r14
	je	.LBB46_18
# BB#17:                                # %if.then134
                                        #   in Loop: Header=BB46_16 Depth=3
	movq	%rdx, %r12
	movl	16(%r13), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rdx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	192(%rsp), %rcx
	movslq	(%rcx,%rax,4), %rsi
	notq	%r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rdx
	callq	fibheap_insert
	movq	%r12, %rdx
	andq	%r14, %r15
	je	.LBB46_19
.LBB46_18:                              # %for.inc152
                                        #   in Loop: Header=BB46_16 Depth=3
	movq	%rbx, %rax
	andq	%rdx, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB46_16
.LBB46_19:                              # %if.end155
                                        #   in Loop: Header=BB46_14 Depth=2
	movq	32(%r13), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB46_33
	.align	16, 0x90
.LBB46_20:                              # %for.body127.1
                                        #   Parent Loop BB46_7 Depth=1
                                        #     Parent Loop BB46_14 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %r12
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	testq	%r15, %r14
	jne	.LBB46_31
# BB#21:                                #   in Loop: Header=BB46_20 Depth=3
	movq	%r12, %rdx
	jmp	.LBB46_32
	.align	16, 0x90
.LBB46_31:                              # %if.then134.1
                                        #   in Loop: Header=BB46_20 Depth=3
	movl	16(%r13), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	leaq	64(%rbx,%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	192(%rsp), %rax
	movslq	(%rax,%rdx,4), %rsi
	notq	%r14
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	fibheap_insert
	andq	%r14, %r15
	movq	%r12, %rdx
	je	.LBB46_33
.LBB46_32:                              # %for.inc152.1
                                        #   in Loop: Header=BB46_20 Depth=3
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	cmpq	$64, %rax
	jb	.LBB46_20
	jmp	.LBB46_33
.LBB46_30:                              # %while.end165
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	%r15, %rdi
	callq	free
	movq	%rbx, %rdi
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fibheap_delete  # TAILCALL
.Ltmp485:
	.size	iterative_dataflow_bitmap, .Ltmp485-iterative_dataflow_bitmap
	.cfi_endproc

	.align	16, 0x90
	.type	hybrid_search_bitmap,@function
hybrid_search_bitmap:                   # @hybrid_search_bitmap
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp492:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp493:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp494:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp495:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp496:
	.cfi_def_cfa_offset 48
	subq	$128, %rsp
.Ltmp497:
	.cfi_def_cfa_offset 176
.Ltmp498:
	.cfi_offset %rbx, -48
.Ltmp499:
	.cfi_offset %r12, -40
.Ltmp500:
	.cfi_offset %r13, -32
.Ltmp501:
	.cfi_offset %r14, -24
.Ltmp502:
	.cfi_offset %r15, -16
	movq	%rcx, 88(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movq	208(%rsp), %r12
	movl	88(%r15), %r13d
	movq	200(%rsp), %rax
	movq	192(%rsp), %r10
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$1, %rdi
	movq	%r13, %rcx
	shlq	%cl, %rdi
	movq	$6, %rcx
	movq	%r13, %rbx
	shrq	%cl, %rbx
	orq	%rdi, 16(%r10,%rbx,8)
	movq	16(%rax,%rbx,8), %rax
	movq	%r9, 96(%rsp)           # 8-byte Spill
	movq	%r8, 80(%rsp)           # 8-byte Spill
	movq	%rdx, 104(%rsp)         # 8-byte Spill
	movq	%rsi, 112(%rsp)         # 8-byte Spill
	testq	%rax, %rdi
	je	.LBB47_36
# BB#1:                                 # %if.then
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rax          # 8-byte Reload
	testq	%rcx, %rax
	je	.LBB47_2
# BB#12:                                # %if.else
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%r14,8), %rdi
	leaq	(%rax,%r14,8), %r13
	callq	bitmap_clear
	movq	%r15, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	40(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB47_21
# BB#13:
	movabsq	$entry_exit_blocks+112, %r12
	movq	176(%rsp), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	.align	16, 0x90
.LBB47_14:                              # %for.body39
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rax
	cmpq	%r12, %rax
	je	.LBB47_20
# BB#15:                                # %if.end42
                                        #   in Loop: Header=BB47_14 Depth=1
	cmpq	$1, %r15
	jne	.LBB47_16
# BB#18:                                # %sw.bb53
                                        #   in Loop: Header=BB47_14 Depth=1
	movslq	88(%rax), %rax
	movq	(%r13), %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	xorq	%rcx, %rcx
	jmp	.LBB47_19
	.align	16, 0x90
.LBB47_16:                              # %if.end42
                                        #   in Loop: Header=BB47_14 Depth=1
	movq	176(%rsp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB47_20
# BB#17:                                # %sw.bb43
                                        #   in Loop: Header=BB47_14 Depth=1
	movslq	88(%rax), %rax
	movq	(%r13), %rdi
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	$2, %rcx
.LBB47_19:                              # %for.inc64
                                        #   in Loop: Header=BB47_14 Depth=1
	movq	%rdi, %rsi
	callq	bitmap_operation
.LBB47_20:                              # %for.inc64
                                        #   in Loop: Header=BB47_14 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB47_14
.LBB47_21:                              # %if.end66.loopexit319
	movq	112(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r14,8), %r12
	jmp	.LBB47_22
.LBB47_2:                               # %if.then11
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax,%r14,8), %rdi
	leaq	(%rax,%r14,8), %r12
	callq	bitmap_clear
	movq	%r15, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB47_11
# BB#3:
	movabsq	$entry_exit_blocks, %r13
	movq	176(%rsp), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	.align	16, 0x90
.LBB47_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rax
	cmpq	%r13, %rax
	je	.LBB47_10
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB47_4 Depth=1
	cmpq	$1, %r15
	jne	.LBB47_6
# BB#8:                                 # %sw.bb25
                                        #   in Loop: Header=BB47_4 Depth=1
	movslq	88(%rax), %rax
	movq	(%r12), %rdi
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	xorq	%rcx, %rcx
	jmp	.LBB47_9
	.align	16, 0x90
.LBB47_6:                               # %if.end
                                        #   in Loop: Header=BB47_4 Depth=1
	movq	176(%rsp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB47_10
# BB#7:                                 # %sw.bb
                                        #   in Loop: Header=BB47_4 Depth=1
	movslq	88(%rax), %rax
	movq	(%r12), %rdi
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx,%rax,8), %rdx
	movq	$2, %rcx
.LBB47_9:                               # %for.inc
                                        #   in Loop: Header=BB47_4 Depth=1
	movq	%rdi, %rsi
	callq	bitmap_operation
.LBB47_10:                              # %for.inc
                                        #   in Loop: Header=BB47_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB47_4
.LBB47_11:                              # %if.end66.loopexit
	movq	104(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r14,8), %r13
.LBB47_22:                              # %if.end66
	movq	(%r12), %rdx
	movq	(%r13), %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14,8), %r8
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax,%r14,8), %r9
	leaq	124(%rsp), %rsi
	movq	208(%rsp), %r12
	movq	%r12, (%rsp)
	movq	72(%rsp), %r15          # 8-byte Reload
	movl	88(%r15), %r13d
	movq	%r13, %rdi
	movq	184(%rsp), %rax
	callq	*%rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	notq	%rcx
	movq	200(%rsp), %rax
	movq	56(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, 16(%rax,%rdx,8)
	movl	124(%rsp), %eax
	testq	%rax, %rax
	movq	192(%rsp), %r10
	je	.LBB47_36
# BB#23:                                # %if.then84
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB47_24
# BB#30:                                # %if.else113
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB47_36
# BB#31:
	movabsq	$entry_exit_blocks, %rdx
	.align	16, 0x90
.LBB47_32:                              # %for.body117
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB47_35
# BB#33:                                # %lor.lhs.false120
                                        #   in Loop: Header=BB47_32 Depth=1
	movq	24(%rax), %rsi
	movl	88(%rsi), %esi
	cmpq	%r13, %rsi
	je	.LBB47_35
# BB#34:                                # %if.end125
                                        #   in Loop: Header=BB47_32 Depth=1
	movl	88(%rcx), %ecx
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	movq	200(%rsp), %rdi
	orq	%rsi, 16(%rdi,%rcx,8)
.LBB47_35:                              # %for.inc138
                                        #   in Loop: Header=BB47_32 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB47_32
	jmp	.LBB47_36
.LBB47_24:                              # %if.then86
	movq	40(%r15), %rax
	testq	%rax, %rax
	je	.LBB47_37
# BB#25:
	movabsq	$entry_exit_blocks+112, %rdx
	.align	16, 0x90
.LBB47_26:                              # %for.body90
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	cmpq	%rdx, %rcx
	je	.LBB47_29
# BB#27:                                # %lor.lhs.false
                                        #   in Loop: Header=BB47_26 Depth=1
	movl	88(%rcx), %ecx
	cmpq	%r13, %rcx
	je	.LBB47_29
# BB#28:                                # %if.end97
                                        #   in Loop: Header=BB47_26 Depth=1
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	movq	200(%rsp), %rdi
	orq	%rsi, 16(%rdi,%rcx,8)
.LBB47_29:                              # %for.inc110
                                        #   in Loop: Header=BB47_26 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB47_26
.LBB47_36:                              # %if.end143
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB47_37
# BB#44:                                # %if.else177
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB47_51
# BB#45:
	movabsq	$entry_exit_blocks, %r14
	.align	16, 0x90
.LBB47_46:                              # %for.body181
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB47_50
# BB#47:                                # %lor.lhs.false184
                                        #   in Loop: Header=BB47_46 Depth=1
	movl	88(%rdi), %ecx
	cmpq	%r13, %rcx
	je	.LBB47_50
# BB#48:                                # %if.end189
                                        #   in Loop: Header=BB47_46 Depth=1
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r10,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB47_50
# BB#49:                                # %if.then203
                                        #   in Loop: Header=BB47_46 Depth=1
	movq	%r12, 32(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	%r10, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	96(%rsp), %r9           # 8-byte Reload
	movq	%r10, %r15
	callq	hybrid_search_bitmap
	movq	%r15, %r10
	.align	16, 0x90
.LBB47_50:                              # %for.inc206
                                        #   in Loop: Header=BB47_46 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB47_46
	jmp	.LBB47_51
.LBB47_37:                              # %if.then145
	movq	40(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB47_51
# BB#38:
	movabsq	$entry_exit_blocks+112, %r14
	.align	16, 0x90
.LBB47_39:                              # %for.body149
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rdi
	cmpq	%r14, %rdi
	je	.LBB47_43
# BB#40:                                # %lor.lhs.false152
                                        #   in Loop: Header=BB47_39 Depth=1
	movl	88(%rdi), %ecx
	cmpq	%r13, %rcx
	je	.LBB47_43
# BB#41:                                # %if.end157
                                        #   in Loop: Header=BB47_39 Depth=1
	movq	%rcx, %rax
	shrq	$6, %rax
	movq	16(%r10,%rax,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	jne	.LBB47_43
# BB#42:                                # %if.then171
                                        #   in Loop: Header=BB47_39 Depth=1
	movq	%r12, 32(%rsp)
	movq	200(%rsp), %rax
	movq	%rax, 24(%rsp)
	movq	%r10, 16(%rsp)
	movq	184(%rsp), %rax
	movq	%rax, 8(%rsp)
	xorq	%r9, %r9
	movq	176(%rsp), %rax
	movq	%rax, (%rsp)
	movq	112(%rsp), %rsi         # 8-byte Reload
	movq	104(%rsp), %rdx         # 8-byte Reload
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	80(%rsp), %r8           # 8-byte Reload
	movq	%r10, %r15
	callq	hybrid_search_bitmap
	movq	%r15, %r10
	.align	16, 0x90
.LBB47_43:                              # %for.inc174
                                        #   in Loop: Header=BB47_39 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB47_39
.LBB47_51:                              # %if.end209
	addq	$128, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp503:
	.size	hybrid_search_bitmap, .Ltmp503-hybrid_search_bitmap
	.cfi_endproc

	.align	16, 0x90
	.type	df_refs_process,@function
df_refs_process:                        # @df_refs_process
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp509:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp510:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp511:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp512:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp513:
	.cfi_def_cfa_offset 48
.Ltmp514:
	.cfi_offset %rbx, -40
.Ltmp515:
	.cfi_offset %r12, -32
.Ltmp516:
	.cfi_offset %r14, -24
.Ltmp517:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	100(%r14), %ebx
	movl	68(%r14), %eax
	cmpq	%rax, %rbx
	je	.LBB48_3
# BB#1:                                 # %for.body.lr.ph
	movq	56(%r14), %rdx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB48_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%r14), %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	movq	(%rax,%rcx,8), %rdi
	movq	8(%rdi), %rax
	movl	8(%rax), %eax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r12
	movq	(%rdx,%r12), %rsi
	callq	df_link_create
	movq	56(%r14), %rdx
	movq	%rax, (%rdx,%r12)
	movl	68(%r14), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jne	.LBB48_2
.LBB48_3:                               # %for.end
	movl	104(%r14), %ebx
	movl	80(%r14), %eax
	cmpq	%rax, %rbx
	je	.LBB48_6
# BB#4:                                 # %for.body11.lr.ph
	movq	56(%r14), %rdx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB48_5:                               # %for.body11
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r14), %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	movq	(%rax,%rcx,8), %rdi
	movq	8(%rdi), %rax
	movl	8(%rax), %eax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r12
	movq	8(%rdx,%r12), %rsi
	callq	df_link_create
	movq	56(%r14), %rdx
	movq	%rax, 8(%rdx,%r12)
	movl	80(%r14), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jne	.LBB48_5
.LBB48_6:                               # %for.end30
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp518:
	.size	df_refs_process, .Ltmp518-df_refs_process
	.cfi_endproc

	.align	16, 0x90
	.type	df_rd_transfer_function,@function
df_rd_transfer_function:                # @df_rd_transfer_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp521:
	.cfi_def_cfa_offset 16
.Ltmp522:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	%rcx, %rdi
	movq	%r8, %rsi
	movq	%r9, %rcx
	callq	bitmap_union_of_diff
	movl	%eax, (%rbx)
	popq	%rbx
	retq
.Ltmp523:
	.size	df_rd_transfer_function, .Ltmp523-df_rd_transfer_function
	.cfi_endproc

	.align	16, 0x90
	.type	df_ru_transfer_function,@function
df_ru_transfer_function:                # @df_ru_transfer_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp526:
	.cfi_def_cfa_offset 16
.Ltmp527:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	%rdx, %rdi
	movq	%r8, %rsi
	movq	%rcx, %rdx
	movq	%r9, %rcx
	callq	bitmap_union_of_diff
	movl	%eax, (%rbx)
	popq	%rbx
	retq
.Ltmp528:
	.size	df_ru_transfer_function, .Ltmp528-df_ru_transfer_function
	.cfi_endproc

	.align	16, 0x90
	.type	df_bitmaps_free,@function
df_bitmaps_free:                        # @df_bitmaps_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp535:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp536:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp537:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp538:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp539:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp540:
	.cfi_def_cfa_offset 96
.Ltmp541:
	.cfi_offset %rbx, -48
.Ltmp542:
	.cfi_offset %r12, -40
.Ltmp543:
	.cfi_offset %r13, -32
.Ltmp544:
	.cfi_offset %r14, -24
.Ltmp545:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	92(%rdi), %eax
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB51_37
# BB#1:                                 # %for.body.lr.ph
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, %rcx
	andq	$1, %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%rax, %rcx
	andq	$2, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	andq	$4, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB51_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	%r14, %rcx
	andq	%rdx, %rcx
	movq	32(%rax,%rcx,8), %rax
	movslq	88(%rax), %rax
	movq	8(%rdi), %r12
	imulq	$112, %rax, %r13
	movq	%r12, %rbx
	addq	%r13, %rbx
	je	.LBB51_36
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB51_2 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB51_14
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB51_2 Depth=1
	cmpq	$0, 16(%r12,%r13)
	je	.LBB51_14
# BB#5:                                 # %do.body
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB51_7
# BB#6:                                 # %if.then8
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_7:                               # %do.end
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	8(%r12,%r13), %rdi
	leaq	16(%r12,%r13), %r15
	leaq	8(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_9
# BB#8:                                 # %if.then16
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_9:                               # %do.end21
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB51_11
# BB#10:                                # %if.then26
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%r15), %rdi
	callq	free
	movq	$0, (%r15)
.LBB51_11:                              # %do.end31
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%r15)
	movq	24(%r12,%r13), %rdi
	leaq	24(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_13
# BB#12:                                # %if.then35
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_13:                              # %do.end40
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB51_14:                              # %if.end42
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB51_25
# BB#15:                                # %land.lhs.true45
                                        #   in Loop: Header=BB51_2 Depth=1
	cmpq	$0, 48(%r12,%r13)
	je	.LBB51_25
# BB#16:                                # %do.body48
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	32(%r12,%r13), %rdi
	leaq	32(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_18
# BB#17:                                # %if.then50
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_18:                              # %do.end55
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	40(%r12,%r13), %rdi
	leaq	48(%r12,%r13), %r15
	leaq	40(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_20
# BB#19:                                # %if.then59
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_20:                              # %do.end64
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB51_22
# BB#21:                                # %if.then69
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%r15), %rdi
	callq	free
	movq	$0, (%r15)
.LBB51_22:                              # %do.end74
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%r15)
	movq	56(%r12,%r13), %rdi
	leaq	56(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_24
# BB#23:                                # %if.then78
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_24:                              # %do.end83
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB51_25:                              # %if.end85
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rdx, %rax
	je	.LBB51_36
# BB#26:                                # %land.lhs.true88
                                        #   in Loop: Header=BB51_2 Depth=1
	cmpq	$0, 80(%r12,%r13)
	je	.LBB51_36
# BB#27:                                # %do.body91
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	64(%r12,%r13), %rdi
	leaq	64(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_29
# BB#28:                                # %if.then93
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_29:                              # %do.end98
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	72(%r12,%r13), %rdi
	leaq	80(%r12,%r13), %r15
	leaq	72(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_31
# BB#30:                                # %if.then102
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_31:                              # %do.end107
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB51_33
# BB#32:                                # %if.then112
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%r15), %rdi
	callq	free
	movq	$0, (%r15)
.LBB51_33:                              # %do.end117
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%r15)
	movq	88(%r12,%r13), %rdi
	leaq	88(%r12,%r13), %rbx
	testq	%rdi, %rdi
	je	.LBB51_35
# BB#34:                                # %if.then121
                                        #   in Loop: Header=BB51_2 Depth=1
	callq	bitmap_clear
	movq	(%rbx), %rdi
	callq	free
	movq	$0, (%rbx)
.LBB51_35:                              # %do.end126
                                        #   in Loop: Header=BB51_2 Depth=1
	movq	$0, (%rbx)
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB51_36:                              # %for.inc
                                        #   in Loop: Header=BB51_2 Depth=1
	movl	92(%rdi), %eax
	incq	%r14
	movq	%r14, %rcx
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB51_2
.LBB51_37:                              # %for.end
	movl	(%rdi), %eax
	movabsq	$4294967288, %rcx       # imm = 0xFFFFFFF8
	movq	8(%rsp), %rdx           # 8-byte Reload
	orq	%rcx, %rdx
	xorq	$7, %rdx
	andq	%rdx, %rax
	movl	%eax, (%rdi)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp546:
	.size	df_bitmaps_free, .Ltmp546-df_bitmaps_free
	.cfi_endproc

	.align	16, 0x90
	.type	df_lr_transfer_function,@function
df_lr_transfer_function:                # @df_lr_transfer_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp549:
	.cfi_def_cfa_offset 16
.Ltmp550:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	movq	%rdx, %rdi
	movq	%r8, %rsi
	movq	%rcx, %rdx
	movq	%r9, %rcx
	callq	bitmap_union_of_diff
	movl	%eax, (%rbx)
	popq	%rbx
	retq
.Ltmp551:
	.size	df_lr_transfer_function, .Ltmp551-df_lr_transfer_function
	.cfi_endproc

	.align	16, 0x90
	.type	df_link_create,@function
df_link_create:                         # @df_link_create
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp556:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp557:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp558:
	.cfi_def_cfa_offset 32
.Ltmp559:
	.cfi_offset %rbx, -32
.Ltmp560:
	.cfi_offset %r14, -24
.Ltmp561:
	.cfi_offset %r15, -16
	movq	df_ref_obstack+32(%rip), %rax
	movq	df_ref_obstack+24(%rip), %rcx
	movq	%rsi, %r15
	movq	%rdi, %r14
	subq	%rcx, %rax
	cmpq	$15, %rax
	jg	.LBB53_2
# BB#1:                                 # %if.then
	movabsq	$df_ref_obstack, %rdi
	movq	$16, %rsi
	callq	_obstack_newchunk
	movq	df_ref_obstack+24(%rip), %rcx
.LBB53_2:                               # %if.end
	addq	$16, %rcx
	movq	%rcx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB53_4
# BB#3:                                 # %if.then8
	movzbl	df_ref_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, df_ref_obstack+80(%rip)
.LBB53_4:                               # %if.end9
	movslq	df_ref_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB53_6
# BB#5:                                 # %if.then29
	movq	%rbx, df_ref_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB53_6:                               # %if.end32
	movq	%rdx, df_ref_obstack+16(%rip)
	movq	%r15, (%rax)
	movq	%r14, 8(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp562:
	.size	df_link_create, .Ltmp562-df_link_create
	.cfi_endproc

	.align	16, 0x90
	.type	df_insn_refs_record,@function
df_insn_refs_record:                    # @df_insn_refs_record
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp569:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp570:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp571:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp572:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp573:
	.cfi_def_cfa_offset 48
.Ltmp574:
	.cfi_offset %rbx, -48
.Ltmp575:
	.cfi_offset %r12, -40
.Ltmp576:
	.cfi_offset %r13, -32
.Ltmp577:
	.cfi_offset %r14, -24
.Ltmp578:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movzbl	(%r12), %eax
	movzbl	rtx_class(%rax), %eax
	movq	%rdi, %r14
	cmpq	$105, %rax
	jne	.LBB54_34
# BB#1:                                 # %if.then
	movq	32(%r12), %r13
	movzbl	(%r13), %eax
	leaq	32(%r12), %r15
	cmpq	$39, %rax
	je	.LBB54_35
# BB#2:                                 # %if.then
	cmpq	$47, %rax
	je	.LBB54_4
# BB#3:                                 # %if.then
	cmpq	$49, %rax
	jne	.LBB54_5
.LBB54_4:                               # %if.then.i
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%r12, %rdx
	callq	df_def_record_1
	jmp	.LBB54_5
.LBB54_35:                              # %if.then3.i
	movq	8(%r13), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB54_36
	jmp	.LBB54_5
	.align	16, 0x90
.LBB54_39:                              # %for.cond.backedge.for.body_crit_edge.i
                                        #   in Loop: Header=BB54_36 Depth=1
	movq	8(%r13), %rax
.LBB54_36:                              # %for.body.lr.ph.i
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	8(%rax,%rbx,8), %rsi
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	je	.LBB54_40
# BB#37:                                # %for.body.i
                                        #   in Loop: Header=BB54_36 Depth=1
	cmpq	$49, %rax
	jne	.LBB54_38
.LBB54_40:                              # %if.then14.i
                                        #   in Loop: Header=BB54_36 Depth=1
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	df_def_record_1
.LBB54_38:                              # %for.cond.backedge.i
                                        #   in Loop: Header=BB54_36 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB54_39
.LBB54_5:                               # %df_defs_record.exit
	movzbl	1(%r14), %eax
	testq	$8, %rax
	je	.LBB54_11
# BB#6:                                 # %if.then3
	movq	56(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB54_11
# BB#7:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB54_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	2(%rbx), %eax
	addq	$-3, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	ja	.LBB54_10
# BB#9:                                 # %sw.bb
                                        #   in Loop: Header=BB54_8 Depth=1
	leaq	8(%rbx), %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r12, %rcx
	callq	df_uses_record
.LBB54_10:                              # %for.inc
                                        #   in Loop: Header=BB54_8 Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB54_8
.LBB54_11:                              # %if.end
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	jne	.LBB54_21
# BB#12:                                # %if.then20
	movq	64(%r12), %rbx
	jmp	.LBB54_13
	.align	16, 0x90
.LBB54_16:                              # %for.inc43
                                        #   in Loop: Header=BB54_13 Depth=1
	movq	16(%rbx), %rbx
.LBB54_13:                              # %if.then20
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB54_17
# BB#14:                                # %for.body27
                                        #   in Loop: Header=BB54_13 Depth=1
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$48, %rax
	jne	.LBB54_16
# BB#15:                                # %if.then35
                                        #   in Loop: Header=BB54_13 Depth=1
	addq	$8, %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r12, %rcx
	callq	df_uses_record
	jmp	.LBB54_16
.LBB54_17:                              # %for.end47
	movl	reg_raw_mode+28(%rip), %edi
	movq	$7, %rsi
	callq	gen_rtx_REG
	movzbl	2(%rax), %esi
	movq	$48, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	leaq	8(%rax), %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%r12, %rcx
	callq	df_uses_record
	movzbl	1(%r14), %eax
	testq	$4, %rax
	je	.LBB54_21
	.align	16, 0x90
.LBB54_18:                              # %for.body58
                                        # =>This Inner Loop Header: Depth=1
	movzbl	global_regs(%rbx), %eax
	testq	%rax, %rax
	je	.LBB54_20
# BB#19:                                # %df_reg_use_gen.exit
                                        #   in Loop: Header=BB54_18 Depth=1
	movl	reg_raw_mode(,%rbx,4), %edi
	movq	%rbx, %rsi
	callq	gen_rtx_REG
	movzbl	2(%rax), %esi
	movq	$48, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	leaq	8(%rax), %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r12, %rcx
	callq	df_uses_record
.LBB54_20:                              # %for.inc68
                                        #   in Loop: Header=BB54_18 Depth=1
	incq	%rbx
	cmpq	$53, %rbx
	jne	.LBB54_18
.LBB54_21:                              # %if.end71
	movq	$1, %rdx
	xorq	%r8, %r8
	xorq	%r13, %r13
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	df_uses_record
	movzbl	(%r12), %eax
	cmpq	$34, %rax
	jne	.LBB54_34
# BB#22:                                # %if.then79
	movzbl	1(%r14), %eax
	testq	$4, %rax
	je	.LBB54_29
	.align	16, 0x90
.LBB54_23:                              # %for.body88
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB54_42 Depth 2
	movq	regs_invalidated_by_call(%rip), %rax
	movq	$1, %rdx
	movq	%r13, %rcx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB54_28
# BB#24:                                # %df_reg_clobber_gen.exit
                                        #   in Loop: Header=BB54_23 Depth=1
	movl	reg_raw_mode(,%r13,4), %edi
	movq	%r13, %rsi
	callq	gen_rtx_REG
	movzbl	2(%rax), %esi
	movq	$49, %rdi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	%rax, %r15
	movzbl	(%r15), %eax
	cmpq	$39, %rax
	je	.LBB54_41
# BB#25:                                # %df_reg_clobber_gen.exit
                                        #   in Loop: Header=BB54_23 Depth=1
	cmpq	$47, %rax
	je	.LBB54_27
# BB#26:                                # %df_reg_clobber_gen.exit
                                        #   in Loop: Header=BB54_23 Depth=1
	cmpq	$49, %rax
	jne	.LBB54_28
.LBB54_27:                              # %if.then.i26
                                        #   in Loop: Header=BB54_23 Depth=1
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	df_def_record_1
	jmp	.LBB54_28
.LBB54_41:                              # %if.then3.i31
                                        #   in Loop: Header=BB54_23 Depth=1
	movq	8(%r15), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jle	.LBB54_28
	jmp	.LBB54_42
	.align	16, 0x90
.LBB54_45:                              # %for.cond.backedge.for.body_crit_edge.i42
                                        #   in Loop: Header=BB54_42 Depth=2
	movq	8(%r15), %rax
.LBB54_42:                              # %for.body.lr.ph.i32
                                        #   Parent Loop BB54_23 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rbx
	movq	8(%rax,%rbx,8), %rsi
	movzbl	(%rsi), %eax
	cmpq	$47, %rax
	je	.LBB54_46
# BB#43:                                # %for.body.i38
                                        #   in Loop: Header=BB54_42 Depth=2
	cmpq	$49, %rax
	jne	.LBB54_44
.LBB54_46:                              # %if.then14.i43
                                        #   in Loop: Header=BB54_42 Depth=2
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	df_def_record_1
.LBB54_44:                              # %for.cond.backedge.i40
                                        #   in Loop: Header=BB54_42 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB54_45
	.align	16, 0x90
.LBB54_28:                              # %for.inc94
                                        #   in Loop: Header=BB54_23 Depth=1
	incq	%r13
	cmpq	$53, %r13
	jne	.LBB54_23
.LBB54_29:                              # %if.end97
	movq	64(%r12), %rbx
	jmp	.LBB54_30
	.align	16, 0x90
.LBB54_33:                              # %for.inc116
                                        #   in Loop: Header=BB54_30 Depth=1
	movq	16(%rbx), %rbx
.LBB54_30:                              # %if.end97
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB54_34
# BB#31:                                # %for.body103
                                        #   in Loop: Header=BB54_30 Depth=1
	movq	8(%rbx), %rsi
	movzbl	(%rsi), %eax
	cmpq	$49, %rax
	jne	.LBB54_33
# BB#32:                                # %if.then.i47
                                        #   in Loop: Header=BB54_30 Depth=1
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	df_def_record_1
	jmp	.LBB54_33
.LBB54_34:                              # %if.end122
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp579:
	.size	df_insn_refs_record, .Ltmp579-df_insn_refs_record
	.cfi_endproc

	.align	16, 0x90
	.type	df_uses_record,@function
df_uses_record:                         # @df_uses_record
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp586:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp587:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp588:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp589:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp590:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp591:
	.cfi_def_cfa_offset 96
.Ltmp592:
	.cfi_offset %rbx, -48
.Ltmp593:
	.cfi_offset %r12, -40
.Ltmp594:
	.cfi_offset %r13, -32
.Ltmp595:
	.cfi_offset %r14, -24
.Ltmp596:
	.cfi_offset %r15, -16
	movq	%r8, %r12
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	jmp	.LBB55_1
.LBB55_11:                              # %tailrecurse.outer.backedge
                                        #   in Loop: Header=BB55_1 Depth=1
	addq	$8, %r9
	movq	%r9, %rsi
	movq	%rax, %rdx
	.align	16, 0x90
.LBB55_1:                               # %tailrecurse.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB55_2 Depth 2
                                        #       Child Loop BB55_3 Depth 3
                                        #         Child Loop BB55_32 Depth 4
                                        #           Child Loop BB55_37 Depth 5
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %r13
	movq	(%rsi), %rax
.LBB55_2:                               # %tailrecurse
                                        #   Parent Loop BB55_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB55_3 Depth 3
                                        #         Child Loop BB55_32 Depth 4
                                        #           Child Loop BB55_37 Depth 5
	testq	%rax, %rax
	movq	%rax, %r9
	movq	%rsi, %rdx
	je	.LBB55_9
.LBB55_3:                               # %if.end
                                        #   Parent Loop BB55_1 Depth=1
                                        #     Parent Loop BB55_2 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB55_32 Depth 4
                                        #           Child Loop BB55_37 Depth 5
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movzbl	(%r9), %r14d
	cmpq	$95, %r14
	jg	.LBB55_28
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB55_3 Depth=3
	leaq	-41(%r14), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$27, %rcx
	ja	.LBB55_30
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB55_3 Depth=3
	movq	$2, %rax
	jmpq	*.LJTI55_0(,%rcx,8)
	.align	16, 0x90
.LBB55_28:                              # %if.end
                                        #   in Loop: Header=BB55_3 Depth=3
	leaq	-96(%r14), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$6, %rax
	jae	.LBB55_30
# BB#29:                                # %sw.bb85
                                        #   in Loop: Header=BB55_3 Depth=3
	movq	8(%r9), %rsi
	leaq	8(%r9), %rdx
	xorq	%r8, %r8
	movq	%r9, %rbx
	movq	$1, %r9
	movq	%r15, %rdi
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	df_ref_record
	movq	%rbx, %r9
.LBB55_30:                              # %sw.epilog93
                                        #   in Loop: Header=BB55_3 Depth=3
	movzbl	rtx_length(%r14), %edx
	testq	%rdx, %rdx
	je	.LBB55_9
# BB#31:                                # %for.body101.lr.ph
                                        #   in Loop: Header=BB55_3 Depth=3
	movq	rtx_format(,%r14,8), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	.align	16, 0x90
.LBB55_32:                              # %for.body101
                                        #   Parent Loop BB55_1 Depth=1
                                        #     Parent Loop BB55_2 Depth=2
                                        #       Parent Loop BB55_3 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB55_37 Depth 5
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	(%rsp), %rax            # 8-byte Reload
	movzbl	-1(%rax,%rdx), %eax
	leaq	-1(%rdx), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	cmpq	$69, %rax
	jne	.LBB55_33
# BB#35:                                # %for.cond126.preheader
                                        #   in Loop: Header=BB55_32 Depth=4
	movq	(%r9,%rdx,8), %rsi
	movslq	(%rsi), %rax
	testq	%rax, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r8
	movq	%r13, %rax
	jle	.LBB55_34
# BB#36:                                #   in Loop: Header=BB55_32 Depth=4
	movq	$8, %r12
	movq	$1, %r13
	.align	16, 0x90
.LBB55_37:                              # %for.body134
                                        #   Parent Loop BB55_1 Depth=1
                                        #     Parent Loop BB55_2 Depth=2
                                        #       Parent Loop BB55_3 Depth=3
                                        #         Parent Loop BB55_32 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	addq	%r12, %rsi
	movq	%r15, %rdi
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, %rbx
                                        # kill: RCX<def> RBX<kill>
	movq	%r8, %r14
                                        # kill: R8<def> R14<kill>
	callq	df_uses_record
	movq	%r14, %r8
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	24(%rsp), %r9           # 8-byte Reload
	movq	(%r9,%rax,8), %rsi
	movslq	(%rsi), %rax
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rbx, %rcx
	incq	%r13
	addq	$8, %r12
	cmpq	%rax, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	jl	.LBB55_37
	jmp	.LBB55_34
	.align	16, 0x90
.LBB55_33:                              # %for.body101
                                        #   in Loop: Header=BB55_32 Depth=4
	cmpq	$101, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r12, %r8
	movq	%r13, %rax
	jne	.LBB55_34
# BB#38:                                # %if.then107
                                        #   in Loop: Header=BB55_32 Depth=4
	movq	%rax, %r13
	movq	%r8, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB55_39
# BB#40:                                # %if.end114
                                        #   in Loop: Header=BB55_32 Depth=4
	leaq	8(%r9,%rcx,8), %rsi
	movq	%r15, %r14
	movq	%r14, %rdi
	movq	%r13, %rdx
	movq	16(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	movq	%r12, %rbx
	movq	%rbx, %r8
	movq	%r9, %r12
	callq	df_uses_record
	movq	%rbx, %r8
	movq	%r13, %rax
	movq	%r15, %rcx
	movq	%r14, %r15
	movq	%r12, %r9
.LBB55_34:                              # %for.cond98.backedge
                                        #   in Loop: Header=BB55_32 Depth=4
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rax, %r13
	movq	%r8, %r12
	movq	$32, %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB55_32
	jmp	.LBB55_9
.LBB55_39:                              # %if.then110
                                        #   in Loop: Header=BB55_3 Depth=3
	movq	8(%r9), %rax
	movq	%r9, %rdx
	addq	$8, %rdx
	testq	%rax, %rax
	movq	%rax, %r9
	jne	.LBB55_3
	jmp	.LBB55_9
.LBB55_12:                              # %sw.bb16
                                        #   in Loop: Header=BB55_2 Depth=2
	movq	8(%r9), %rax
	movzbl	(%rax), %ecx
	leaq	8(%r9), %rsi
	cmpq	$61, %rcx
	jne	.LBB55_2
	jmp	.LBB55_13
.LBB55_14:                              # %sw.bb29
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	8(%r9), %r14
	addq	$16, %r9
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r9, %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	df_uses_record
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$131, %rcx
	jg	.LBB55_24
# BB#15:                                # %sw.bb29
                                        #   in Loop: Header=BB55_1 Depth=1
	addq	$-59, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$7, %rcx
	ja	.LBB55_27
# BB#16:                                # %sw.bb29
                                        #   in Loop: Header=BB55_1 Depth=1
	jmpq	*.LJTI55_1(,%rcx,8)
.LBB55_17:                              # %if.end.i
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	8(%r14), %rcx
	movl	target_flags(%rip), %edx
	movzbl	2(%rcx), %ecx
	movzbl	mode_size(%rcx), %esi
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	movq	%rdx, %rcx
	movabsq	$4294967292, %rdi       # imm = 0xFFFFFFFC
	andq	%rdi, %rcx
	cmpq	%rcx, %rsi
	jbe	.LBB55_9
# BB#18:                                # %read_modify_subreg_p.exit
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rax
	andq	$255, %rax
	movzbl	mode_size(%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	%rdx, %rax
	ja	.LBB55_9
# BB#19:                                #   in Loop: Header=BB55_1 Depth=1
	movq	$1, %r13
	addq	$8, %r14
	movq	%r14, %rsi
	jmp	.LBB55_20
.LBB55_10:                              # %sw.bb1
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	8(%r9), %r9
	movzbl	(%r9), %eax
	cmpq	$66, %rax
	movq	$3, %rax
	je	.LBB55_11
	jmp	.LBB55_9
.LBB55_24:                              # %sw.bb29
                                        #   in Loop: Header=BB55_1 Depth=1
	addq	$-132, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jae	.LBB55_27
# BB#25:                                # %sw.bb61
                                        #   in Loop: Header=BB55_1 Depth=1
	leaq	8(%r14), %rsi
	movq	$1, %rdx
	movq	$1, %r8
	movq	$1, %r13
	movq	%r15, %rdi
	movq	%r14, %rbx
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	callq	df_uses_record
	leaq	16(%rbx), %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	xorq	%r12, %r12
	movq	%r15, %rdi
	movq	%r14, %rcx
	callq	df_uses_record
	addq	$24, %rbx
	movq	%rbx, %rsi
	jmp	.LBB55_26
.LBB55_22:                              # %sw.bb49
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	8(%r14), %r14
	movzbl	(%r14), %eax
	cmpq	$63, %rax
	jne	.LBB55_41
# BB#23:                                # %if.end57
                                        #   in Loop: Header=BB55_1 Depth=1
	addq	$8, %r14
	movq	%r14, %rsi
	movq	$1, %r13
.LBB55_20:                              # %tailrecurse.outer.outer.backedge
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	$1, %r12
	movq	16(%rsp), %r14          # 8-byte Reload
	jmp	.LBB55_26
.LBB55_21:                              # %sw.bb45
                                        #   in Loop: Header=BB55_1 Depth=1
	addq	$8, %r14
	movq	%r14, %rsi
	xorq	%r12, %r12
	movq	$3, %r13
	movq	16(%rsp), %r14          # 8-byte Reload
.LBB55_26:                              # %tailrecurse.outer.outer.backedge
                                        #   in Loop: Header=BB55_1 Depth=1
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rdx
	jmp	.LBB55_1
.LBB55_13:                              # %sw.bb28
	movq	%r15, %rdi
	movq	%r9, %rsi
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r13, %r8
	movq	%r12, %r9
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	df_ref_record  # TAILCALL
.LBB55_6:                               # %for.cond.preheader
	movq	32(%r9), %rsi
	movslq	(%rsi), %rax
	testq	%rax, %rax
	jle	.LBB55_9
# BB#7:
	movq	$1, %r14
	movq	$8, %r12
.LBB55_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	addq	%r12, %rsi
	movq	$1, %rdx
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r9, %rbx
	callq	df_uses_record
	movq	%rbx, %r9
	movq	32(%r9), %rsi
	movslq	(%rsi), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$8, %r12
	cmpq	%rax, %rdx
	jl	.LBB55_8
.LBB55_9:                               # %for.end148
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB55_27:                              # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.df_uses_record, %rdx
	movq	$1114, %rsi             # imm = 0x45A
	callq	fancy_abort
.LBB55_41:                              # %if.then56
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_uses_record, %rcx
	movq	$1101, %rsi             # imm = 0x44D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp597:
	.size	df_uses_record, .Ltmp597-df_uses_record
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI55_0:
	.quad	.LBB55_6
	.quad	.LBB55_30
	.quad	.LBB55_30
	.quad	.LBB55_9
	.quad	.LBB55_9
	.quad	.LBB55_30
	.quad	.LBB55_14
	.quad	.LBB55_30
	.quad	.LBB55_10
	.quad	.LBB55_30
	.quad	.LBB55_30
	.quad	.LBB55_30
	.quad	.LBB55_30
	.quad	.LBB55_9
	.quad	.LBB55_9
	.quad	.LBB55_9
	.quad	.LBB55_30
	.quad	.LBB55_9
	.quad	.LBB55_9
	.quad	.LBB55_30
	.quad	.LBB55_13
	.quad	.LBB55_30
	.quad	.LBB55_12
	.quad	.LBB55_30
	.quad	.LBB55_30
	.quad	.LBB55_11
	.quad	.LBB55_9
	.quad	.LBB55_9
.LJTI55_1:
	.quad	.LBB55_9
	.quad	.LBB55_27
	.quad	.LBB55_9
	.quad	.LBB55_27
	.quad	.LBB55_17
	.quad	.LBB55_22
	.quad	.LBB55_27
	.quad	.LBB55_21

	.text
	.align	16, 0x90
	.type	df_ref_record,@function
df_ref_record:                          # @df_ref_record
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp604:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp605:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp606:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp607:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp608:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp609:
	.cfi_def_cfa_offset 80
.Ltmp610:
	.cfi_offset %rbx, -48
.Ltmp611:
	.cfi_offset %r12, -40
.Ltmp612:
	.cfi_offset %r13, -32
.Ltmp613:
	.cfi_offset %r14, -24
.Ltmp614:
	.cfi_offset %r15, -16
	movq	%rsi, %rax
	movl	(%rax), %r10d
	movq	%rcx, %rbx
	movq	%r10, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB56_2
# BB#1:
	movq	%r10, %r11
	movq	%rax, %rsi
	jmp	.LBB56_8
.LBB56_2:                               # %entry
	cmpq	$63, %rcx
	jne	.LBB56_30
# BB#3:                                 # %land.lhs.true7
	movq	%rdi, %r14
	movq	$16, %rcx
	movq	%r10, %rdi
	shrq	%cl, %rdi
	movslq	word_mode(%rip), %rcx
	movq	8(%rax), %rsi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %r15d
	movzbl	mode_size(%rcx), %ecx
	movl	(%rsi), %r11d
	cmpq	%rcx, %r15
	jb	.LBB56_6
# BB#4:                                 # %lor.lhs.false
	movq	$16, %rcx
	movq	%r11, %rdi
	shrq	%cl, %rdi
	andq	$255, %rdi
	movzbl	mode_size(%rdi), %ecx
	cmpq	%rcx, %r15
	jae	.LBB56_6
# BB#5:
	movq	%r10, %r11
	movq	%rax, %rsi
	jmp	.LBB56_7
.LBB56_6:                               # %if.then30
	addq	$8, %rax
	movq	%rax, %rdx
.LBB56_7:                               # %if.end34
	movq	%r14, %rdi
.LBB56_8:                               # %if.end34
	movq	%r11, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$63, %rax
	movq	%rsi, %rax
	jne	.LBB56_10
# BB#9:                                 # %cond.true
	movq	8(%rsi), %rax
.LBB56_10:                              # %cond.end
	movl	8(%rax), %r13d
	movq	$32, %rcx
	movq	%r13, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$52, %r13
	ja	.LBB56_31
# BB#11:                                # %if.then46
	movzbl	1(%rdi), %eax
	testq	$4, %rax
	je	.LBB56_29
# BB#12:                                # %if.end48
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rbx, %r12
	movq	%r8, %r10
	movq	%r9, %r8
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%r13, %rax
	movq	$16, %rcx
	shrq	%cl, %r11
	andq	$255, %r11
	cmpq	$8, %rax
	je	.LBB56_16
# BB#13:                                # %if.end48
	leaq	-21(%r13), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB56_16
# BB#14:                                # %if.end48
	leaq	-45(%r13), %rcx
	andq	%rax, %rcx
	cmpq	$8, %rcx
	jb	.LBB56_16
# BB#15:                                # %if.end48
	leaq	-29(%r13), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	jbe	.LBB56_16
# BB#21:                                # %cond.false88
	cmpq	$24, %r11
	jne	.LBB56_22
# BB#24:                                # %cond.true104
	movzbl	target_flags+3(%rip), %eax
	andq	$2, %rax
	xorq	$6, %rax
	jmp	.LBB56_26
.LBB56_31:                              # %if.else
	movq	%rbx, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	df_ref_record_1  # TAILCALL
.LBB56_16:                              # %cond.true72
	movl	mode_class(,%r11,4), %ecx
	cmpq	$5, %rcx
	movq	$2, %rax
	je	.LBB56_26
# BB#17:                                # %lor.rhs
	cmpq	$6, %rcx
	jne	.LBB56_18
# BB#19:                                # %lor.rhs
	movabsq	$1, %rax
	incq	%rax
	jmp	.LBB56_26
.LBB56_22:                              # %cond.false88
	cmpq	$18, %r11
	jne	.LBB56_25
# BB#23:                                # %cond.true94
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	xorq	$3, %rax
	jmp	.LBB56_26
.LBB56_18:
	movabsq	$0, %rax
	incq	%rax
	jmp	.LBB56_26
.LBB56_25:                              # %cond.false108
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%r11), %eax
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	leaq	3(%rax,%rsi), %rax
	addq	$4, %rsi
	cqto
	idivq	%rsi
.LBB56_26:                              # %cond.end125
	movq	$32, %rcx
	addq	%r13, %rax
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %r13
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%r10, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	movq	8(%rsp), %rbx           # 8-byte Reload
	jge	.LBB56_29
# BB#27:                                # %for.body.lr.ph
	incq	%r15
	.align	16, 0x90
.LBB56_28:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	reg_raw_mode-4(,%r15,4), %edi
	movq	%rax, %r13
	leaq	-1(%r15), %rsi
	callq	gen_rtx_REG
	movq	%r14, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	16(%rsp), %r8           # 8-byte Reload
	movq	24(%rsp), %r9           # 8-byte Reload
	callq	df_ref_record_1
	movq	%r13, %rax
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB56_28
.LBB56_29:                              # %if.end132
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB56_30:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.df_ref_record, %rcx
	movq	$875, %rsi              # imm = 0x36B
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp615:
	.size	df_ref_record, .Ltmp615-df_ref_record
	.cfi_endproc

	.align	16, 0x90
	.type	df_ref_record_1,@function
df_ref_record_1:                        # @df_ref_record_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp622:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp623:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp624:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp625:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp626:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp627:
	.cfi_def_cfa_offset 64
.Ltmp628:
	.cfi_offset %rbx, -48
.Ltmp629:
	.cfi_offset %r12, -40
.Ltmp630:
	.cfi_offset %r13, -32
.Ltmp631:
	.cfi_offset %r14, -24
.Ltmp632:
	.cfi_offset %r15, -16
	movq	df_ref_obstack+32(%rip), %rbx
	movq	df_ref_obstack+24(%rip), %rax
	movq	%rsi, %r10
	movq	%rdi, %r14
	subq	%rax, %rbx
	cmpq	$47, %rbx
	jg	.LBB57_2
# BB#1:                                 # %if.then.i
	movabsq	$df_ref_obstack, %rdi
	movq	$48, %rsi
	movq	%r9, %rbx
	movq	%rcx, %r12
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r14
	movq	%r10, %r15
	movq	%r8, %r13
	callq	_obstack_newchunk
	movq	%r13, %r8
	movq	%r15, %r10
	movq	%r14, %rdx
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r12, %rcx
	movq	%rbx, %r9
	movq	df_ref_obstack+24(%rip), %rax
.LBB57_2:                               # %if.end.i
	addq	$48, %rax
	movq	%rax, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+16(%rip), %r12
	cmpq	%r12, %rax
	jne	.LBB57_4
# BB#3:                                 # %if.then8.i
	movzbl	df_ref_obstack+80(%rip), %esi
	orq	$2, %rsi
	movb	%sil, df_ref_obstack+80(%rip)
.LBB57_4:                               # %if.end9.i
	movslq	df_ref_obstack+48(%rip), %rsi
	addq	%rsi, %rax
	notq	%rsi
	andq	%rax, %rsi
	movq	%rsi, df_ref_obstack+24(%rip)
	movq	df_ref_obstack+32(%rip), %r11
	movq	df_ref_obstack+8(%rip), %rax
	movq	%rsi, %rbx
	subq	%rax, %rbx
	movq	%r11, %rdi
	subq	%rax, %rdi
	cmpq	%rdi, %rbx
	jle	.LBB57_6
# BB#5:                                 # %if.then29.i
	movq	%r11, df_ref_obstack+24(%rip)
	movq	%r11, %rsi
.LBB57_6:                               # %if.end32.i
	movq	%rsi, df_ref_obstack+16(%rip)
	movq	%r10, (%r12)
	movq	%rdx, 16(%r12)
	movq	%rcx, 8(%r12)
	movq	$0, 24(%r12)
	movl	%r8d, 32(%r12)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r8
	movl	%r9d, 40(%r12)
	je	.LBB57_7
# BB#11:                                # %if.else.i
	movl	80(%r14), %ecx
	movl	84(%r14), %eax
	cmpq	%rax, %rcx
	jae	.LBB57_13
# BB#12:                                # %if.else.if.end68_crit_edge.i
	movq	24(%r14), %rax
	jmp	.LBB57_14
.LBB57_7:                               # %if.then41.i
	movl	68(%r14), %ecx
	movl	72(%r14), %eax
	cmpq	%rax, %rcx
	jae	.LBB57_9
# BB#8:                                 # %if.then41.if.end51_crit_edge.i
	movq	16(%r14), %rax
	jmp	.LBB57_10
.LBB57_13:                              # %if.then58.i
	movq	$2, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movl	%esi, 84(%r14)
	movq	24(%r14), %rdi
	andq	%rbx, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 24(%r14)
	movl	80(%r14), %ecx
.LBB57_14:                              # %if.end68.i
	movl	%ecx, 36(%r12)
	leaq	1(%rcx), %rdx
	movl	%edx, 80(%r14)
	jmp	.LBB57_15
.LBB57_9:                               # %if.then44.i
	movq	$2, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movl	%esi, 72(%r14)
	movq	16(%r14), %rdi
	andq	%rbx, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	callq	xrealloc
	movq	%rax, 16(%r14)
	movl	68(%r14), %ecx
.LBB57_10:                              # %if.end51.i
	movl	%ecx, 36(%r12)
	leaq	1(%rcx), %rdx
	movl	%edx, 68(%r14)
.LBB57_15:                              # %df_ref_create.exit
	andq	%rcx, %rbx
	movq	%r12, (%rax,%rbx,8)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp633:
	.size	df_ref_record_1, .Ltmp633-df_ref_record_1
	.cfi_endproc

	.align	16, 0x90
	.type	df_def_record_1,@function
df_def_record_1:                        # @df_def_record_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp639:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp640:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp641:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp642:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp643:
	.cfi_def_cfa_offset 48
.Ltmp644:
	.cfi_offset %rbx, -40
.Ltmp645:
	.cfi_offset %r12, -32
.Ltmp646:
	.cfi_offset %r14, -24
.Ltmp647:
	.cfi_offset %r15, -16
	movq	%rsi, %rax
	movq	8(%rax), %r12
	movl	(%r12), %esi
	movq	%rdx, %r14
	movq	%rdi, %r15
	movq	%rsi, %rcx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	$3342375, %rcx          # imm = 0x330027
	jne	.LBB58_1
# BB#6:                                 # %if.then
	movq	8(%r12), %rax
	movslq	(%rax), %rbx
	testq	%rbx, %rbx
	jg	.LBB58_7
	jmp	.LBB58_16
	.align	16, 0x90
.LBB58_8:                               # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB58_7 Depth=1
	movq	8(%r12), %rax
.LBB58_7:                               # %for.body.lr.ph
                                        # =>This Inner Loop Header: Depth=1
	decq	%rbx
	movq	8(%rax,%rbx,8), %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	df_def_record_1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB58_8
	jmp	.LBB58_16
.LBB58_1:                               # %while.cond.preheader
	addq	$8, %rax
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	addq	$4, %rdx
	xorq	%r9, %r9
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	jmp	.LBB58_2
	.align	16, 0x90
.LBB58_11:                              # %if.end28
                                        #   in Loop: Header=BB58_2 Depth=1
	movq	8(%r12), %rcx
	movl	(%rcx), %esi
	addq	$8, %r12
	orq	$1, %r9
	movq	%r12, %rax
	movq	%rcx, %r12
.LBB58_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$131, %rcx
	jg	.LBB58_15
# BB#3:                                 # %while.cond
                                        #   in Loop: Header=BB58_2 Depth=1
	cmpq	$63, %rcx
	je	.LBB58_9
# BB#4:                                 # %while.cond
                                        #   in Loop: Header=BB58_2 Depth=1
	cmpq	$64, %rcx
	jne	.LBB58_13
# BB#5:                                 # %if.then24
                                        #   in Loop: Header=BB58_2 Depth=1
	movq	8(%r12), %r12
	jmp	.LBB58_11
	.align	16, 0x90
.LBB58_15:                              # %while.cond
                                        #   in Loop: Header=BB58_2 Depth=1
	addq	$-132, %rcx
	andq	%rdi, %rcx
	cmpq	$2, %rcx
	jb	.LBB58_11
	jmp	.LBB58_16
	.align	16, 0x90
.LBB58_9:                               # %if.end.i
                                        #   in Loop: Header=BB58_2 Depth=1
	movq	8(%r12), %rcx
	movzbl	2(%rcx), %ecx
	movzbl	mode_size(%rcx), %ecx
	cmpq	%rdx, %rcx
	jbe	.LBB58_12
# BB#10:                                # %lor.end
                                        #   in Loop: Header=BB58_2 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rsi
	andq	$255, %rsi
	movzbl	mode_size(%rsi), %ecx
	cmpq	%rdx, %rcx
	jbe	.LBB58_11
.LBB58_12:                              # %land.lhs.true39
	movq	8(%r12), %rcx
	movzbl	(%rcx), %ecx
.LBB58_13:                              # %land.lhs.true39
	cmpq	$61, %rcx
	jne	.LBB58_16
# BB#14:                                # %if.then46
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	df_ref_record  # TAILCALL
.LBB58_16:                              # %if.end47
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp648:
	.size	df_def_record_1, .Ltmp648-df_def_record_1
	.cfi_endproc

	.type	ddf,@object             # @ddf
	.local	ddf
	.comm	ddf,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"df.c"
	.size	.L.str, 5

	.type	.L__FUNCTION__.df_analyse,@object # @__FUNCTION__.df_analyse
.L__FUNCTION__.df_analyse:
	.asciz	"df_analyse"
	.size	.L__FUNCTION__.df_analyse, 11

	.type	.L__FUNCTION__.df_insn_delete,@object # @__FUNCTION__.df_insn_delete
.L__FUNCTION__.df_insn_delete:
	.asciz	"df_insn_delete"
	.size	.L__FUNCTION__.df_insn_delete, 15

	.type	.L__FUNCTION__.df_refs_reg_replace,@object # @__FUNCTION__.df_refs_reg_replace
.L__FUNCTION__.df_refs_reg_replace:
	.asciz	"df_refs_reg_replace"
	.size	.L__FUNCTION__.df_refs_reg_replace, 20

	.type	.L__FUNCTION__.df_ref_reg_replace,@object # @__FUNCTION__.df_ref_reg_replace
.L__FUNCTION__.df_ref_reg_replace:
	.asciz	"df_ref_reg_replace"
	.size	.L__FUNCTION__.df_ref_reg_replace, 19

	.type	.L__FUNCTION__.df_pattern_emit_before,@object # @__FUNCTION__.df_pattern_emit_before
.L__FUNCTION__.df_pattern_emit_before:
	.asciz	"df_pattern_emit_before"
	.size	.L__FUNCTION__.df_pattern_emit_before, 23

	.type	.L__FUNCTION__.df_bb_regs_lives_compare,@object # @__FUNCTION__.df_bb_regs_lives_compare
.L__FUNCTION__.df_bb_regs_lives_compare:
	.asciz	"df_bb_regs_lives_compare"
	.size	.L__FUNCTION__.df_bb_regs_lives_compare, 25

	.type	.L__FUNCTION__.df_bb_single_def_use_insn_find,@object # @__FUNCTION__.df_bb_single_def_use_insn_find
.L__FUNCTION__.df_bb_single_def_use_insn_find:
	.asciz	"df_bb_single_def_use_insn_find"
	.size	.L__FUNCTION__.df_bb_single_def_use_insn_find, 31

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\nDataflow summary:\n"
	.size	.L.str1, 20

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"n_regs = %d, n_defs = %d, n_uses = %d, n_bbs = %d\n"
	.size	.L.str2, 51

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Reaching defs:\n"
	.size	.L.str3, 16

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"bb %d in  \t"
	.size	.L.str4, 12

	.type	.L.str5,@object         # @.str5
.L.str5:
	.zero	1
	.size	.L.str5, 1

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\n"
	.size	.L.str6, 2

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"bb %d gen \t"
	.size	.L.str7, 12

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"bb %d kill\t"
	.size	.L.str8, 12

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"bb %d out \t"
	.size	.L.str9, 12

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Use-def chains:\n"
	.size	.L.str10, 17

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"d%d bb %d luid %d insn %d reg %d "
	.size	.L.str11, 34

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"read/write "
	.size	.L.str12, 12

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Reaching uses:\n"
	.size	.L.str13, 16

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Def-use chains:\n"
	.size	.L.str14, 17

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"u%d bb %d luid %d insn %d reg %d "
	.size	.L.str15, 34

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Live regs:\n"
	.size	.L.str16, 12

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"bb %d use \t"
	.size	.L.str17, 12

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"bb %d def \t"
	.size	.L.str18, 12

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Register info:\n"
	.size	.L.str19, 16

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"reg %d"
	.size	.L.str20, 7

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	" bb %d"
	.size	.L.str21, 7

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	" bb ?"
	.size	.L.str22, 6

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	" life %d"
	.size	.L.str23, 9

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	" defs "
	.size	.L.str24, 7

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"%d "
	.size	.L.str25, 4

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	" uses "
	.size	.L.str26, 7

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"insn %d bb %d luid %d defs "
	.size	.L.str27, 28

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"%c%d "
	.size	.L.str28, 6

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"reg %d bb %d luid %d insn %d chain "
	.size	.L.str29, 36

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"reg %d life %d defs "
	.size	.L.str30, 21

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"{ "
	.size	.L.str31, 3

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"%c%d(%d) "
	.size	.L.str32, 10

	.type	.L__FUNCTION__.df_insns_modify,@object # @__FUNCTION__.df_insns_modify
.L__FUNCTION__.df_insns_modify:
	.asciz	"df_insns_modify"
	.size	.L__FUNCTION__.df_insns_modify, 16

	.type	df_ref_obstack,@object  # @df_ref_obstack
	.local	df_ref_obstack
	.comm	df_ref_obstack,88,8
	.type	.L__FUNCTION__.df_uses_record,@object # @__FUNCTION__.df_uses_record
.L__FUNCTION__.df_uses_record:
	.asciz	"df_uses_record"
	.size	.L__FUNCTION__.df_uses_record, 15

	.type	.L__FUNCTION__.df_ref_record,@object # @__FUNCTION__.df_ref_record
.L__FUNCTION__.df_ref_record:
	.asciz	"df_ref_record"
	.size	.L__FUNCTION__.df_ref_record, 14

	.type	.L__FUNCTION__.df_ref_unlink,@object # @__FUNCTION__.df_ref_unlink
.L__FUNCTION__.df_ref_unlink:
	.asciz	"df_ref_unlink"
	.size	.L__FUNCTION__.df_ref_unlink, 14


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
