	.file	"c-format.c.bc"
	.text
	.globl	set_Wformat
	.align	16, 0x90
	.type	set_Wformat,@function
set_Wformat:                            # @set_Wformat
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$1, %rax
	movl	%edi, warn_format(%rip)
	movl	%edi, warn_format_y2k(%rip)
	movl	%edi, warn_format_extra_args(%rip)
	je	.LBB0_2
# BB#1:                                 # %if.then
	movl	%edi, warn_format_nonliteral(%rip)
	movl	%edi, warn_format_security(%rip)
.LBB0_2:                                # %if.end
	retq
.Ltmp0:
	.size	set_Wformat, .Ltmp0-set_Wformat
	.cfi_endproc

	.globl	handle_format_attribute
	.align	16, 0x90
	.type	handle_format_attribute,@function
handle_format_attribute:                # @handle_format_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 32
	subq	$32, %rsp
.Ltmp8:
	.cfi_def_cfa_offset 64
.Ltmp9:
	.cfi_offset %rbx, -32
.Ltmp10:
	.cfi_offset %r14, -24
.Ltmp11:
	.cfi_offset %r15, -16
	movq	(%rdi), %rbx
	movq	%r8, %r15
	movq	%rcx, %r14
	movq	%rdx, %rax
	leaq	8(%rsp), %rsi
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	decode_format_attr
	testq	%rax, %rax
	je	.LBB1_19
# BB#1:                                 # %if.end
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_10
# BB#2:                                 # %land.rhs.lr.ph
	movq	16(%rsp), %rcx
	movq	%rcx, %rdx
	.align	16, 0x90
.LBB1_3:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	decq	%rdx
	je	.LBB1_5
# BB#4:                                 # %for.inc
                                        #   in Loop: Header=BB1_3 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB1_3
	jmp	.LBB1_7
.LBB1_5:                                # %lor.lhs.false
	movq	32(%rax), %rdx
	movzbl	16(%rdx), %esi
	cmpq	$13, %rsi
	jne	.LBB1_7
# BB#6:                                 # %lor.lhs.false7
	movq	8(%rdx), %rdx
	movq	128(%rdx), %rdx
	movq	integer_types(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB1_9
.LBB1_7:                                # %if.then14
	testq	$16, %r14
	jne	.LBB1_19
# BB#8:                                 # %if.then16
	movabsq	$.L.str, %rdi
	jmp	.LBB1_18
.LBB1_9:                                # %if.else
	movq	24(%rsp), %rdx
	testq	%rdx, %rdx
	jne	.LBB1_14
	jmp	.LBB1_10
	.align	16, 0x90
.LBB1_13:                               # %while.body
                                        #   in Loop: Header=BB1_14 Depth=1
	movq	(%rax), %rax
	incq	%rcx
.LBB1_14:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB1_13
# BB#15:                                # %while.end
	cmpq	%rdx, %rcx
	jne	.LBB1_16
.LBB1_10:                               # %if.end34
	movl	8(%rsp), %eax
	cmpq	$2, %rax
	jne	.LBB1_20
# BB#11:                                # %land.lhs.true
	cmpq	$0, 24(%rsp)
	je	.LBB1_20
# BB#12:                                # %if.then38
	movabsq	$.L.str2, %rdi
.LBB1_18:                               # %return
	xorq	%rax, %rax
	callq	error
.LBB1_19:                               # %return
	movq	$1, %rax
	movb	%al, (%r15)
.LBB1_20:                               # %return
	xorq	%rax, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB1_16:                               # %if.then26
	testq	$16, %r14
	jne	.LBB1_19
# BB#17:                                # %if.then29
	movabsq	$.L.str1, %rdi
	jmp	.LBB1_18
.Ltmp12:
	.size	handle_format_attribute, .Ltmp12-handle_format_attribute
	.cfi_endproc

	.align	16, 0x90
	.type	decode_format_attr,@function
decode_format_attr:                     # @decode_format_attr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp19:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp20:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp21:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp22:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 48
.Ltmp24:
	.cfi_offset %rbx, -48
.Ltmp25:
	.cfi_offset %r12, -40
.Ltmp26:
	.cfi_offset %r13, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movq	32(%rdi), %rax
	movzbl	16(%rax), %ecx
	movq	%rdx, %r15
	movq	%rsi, %r14
	cmpq	$1, %rcx
	jne	.LBB2_1
# BB#5:                                 # %if.else
	movq	(%rdi), %rcx
	movq	(%rcx), %rdx
	movq	32(%rdx), %r13
	movq	32(%rcx), %rbx
	movq	32(%rax), %r12
	movq	%r12, %rdi
	callq	decode_format_type
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	$4, %rdx
	movl	%eax, (%r14)
	jne	.LBB2_6
# BB#16:                                # %if.then13
	testq	%rcx, %r15
	jne	.LBB2_17
# BB#18:                                # %if.end16
	movabsq	$.L.str212, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rsi
	callq	warning
	jmp	.LBB2_25
.LBB2_1:                                # %if.then
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB2_2
# BB#4:                                 # %if.end
	movabsq	$.L.str211, %rdi
	jmp	.LBB2_24
	.align	16, 0x90
.LBB2_7:                                # %while.body
                                        #   in Loop: Header=BB2_6 Depth=1
	movq	32(%rbx), %rbx
.LBB2_6:                                # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jae	.LBB2_9
	jmp	.LBB2_7
	.align	16, 0x90
.LBB2_8:                                # %while.body53
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	32(%r13), %r13
.LBB2_9:                                # %while.body53
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%r13), %edx
	leaq	-114(%rdx), %rsi
	andq	%rcx, %rsi
	cmpq	$3, %rsi
	jb	.LBB2_8
# BB#10:                                # %while.end57
	andq	$255, %rax
	cmpq	$25, %rax
	jne	.LBB2_14
# BB#11:                                # %lor.lhs.false63
	cmpq	$0, 40(%rbx)
	jne	.LBB2_14
# BB#12:                                # %lor.lhs.false63
	cmpq	$25, %rdx
	jne	.LBB2_14
# BB#13:                                # %lor.lhs.false72
	cmpq	$0, 40(%r13)
	je	.LBB2_20
.LBB2_14:                               # %if.then77
	testq	%rcx, %r15
	jne	.LBB2_15
# BB#19:                                # %if.end80
	movabsq	$.L.str3, %rdi
.LBB2_24:                               # %return
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB2_25:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_20:                               # %if.end81
	movq	32(%rbx), %rax
	movq	%rax, 8(%r14)
	movq	32(%r13), %rdx
	movq	%rdx, 16(%r14)
	decq	%rdx
	cmpq	%rax, %rdx
	movq	$1, %rbx
	jae	.LBB2_25
# BB#21:                                # %if.then92
	testq	%rcx, %r15
	jne	.LBB2_22
# BB#23:                                # %if.end95
	movabsq	$.L.str213, %rdi
	jmp	.LBB2_24
.LBB2_17:                               # %if.then15
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.decode_format_attr, %rcx
	movq	$264, %rsi              # imm = 0x108
	jmp	.LBB2_3
.LBB2_2:                                # %if.then10
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.decode_format_attr, %rcx
	movq	$251, %rsi
	jmp	.LBB2_3
.LBB2_15:                               # %if.then79
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.decode_format_attr, %rcx
	movq	$288, %rsi              # imm = 0x120
	jmp	.LBB2_3
.LBB2_22:                               # %if.then94
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.decode_format_attr, %rcx
	movq	$298, %rsi              # imm = 0x12A
.LBB2_3:                                # %if.then10
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp29:
	.size	decode_format_attr, .Ltmp29-decode_format_attr
	.cfi_endproc

	.globl	handle_format_arg_attribute
	.align	16, 0x90
	.type	handle_format_arg_attribute,@function
handle_format_arg_attribute:            # @handle_format_arg_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 16
.Ltmp33:
	.cfi_offset %rbx, -16
	movq	(%rdi), %r9
	movq	%r8, %rbx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdx), %rdx
	movzbl	16(%rdx), %eax
	leaq	-114(%rax), %rdi
	andq	%rsi, %rdi
	cmpq	$3, %rdi
	jb	.LBB3_1
# BB#2:                                 # %lor.rhs
	cmpq	$25, %rax
	jne	.LBB3_4
# BB#3:                                 # %lor.lhs.false16
	cmpq	$0, 40(%rdx)
	je	.LBB3_5
.LBB3_4:                                # %if.then
	movabsq	$.L.str3, %rdi
.LBB3_21:                               # %if.end67
	xorq	%rax, %rax
	callq	error
.LBB3_22:                               # %if.end67
	movq	$1, %rax
	movb	%al, (%rbx)
.LBB3_23:                               # %return
	xorq	%rax, %rax
	popq	%rbx
	retq
.LBB3_5:                                # %if.end
	movq	24(%r9), %rsi
	testq	%rsi, %rsi
	je	.LBB3_17
# BB#6:                                 # %if.then22
	movq	32(%rdx), %rdx
	cmpq	$1, %rdx
	je	.LBB3_13
# BB#7:                                 # %for.inc.preheader
	addq	$-2, %rdx
	movabsq	$0, %rax
	movabsq	$1, %r8
	.align	16, 0x90
.LBB3_8:                                # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	movq	%rax, %rdi
	jne	.LBB3_10
# BB#9:                                 # %for.inc
                                        #   in Loop: Header=BB3_8 Depth=1
	movq	%r8, %rdi
.LBB3_10:                               # %for.inc
                                        #   in Loop: Header=BB3_8 Depth=1
	testq	%rdx, %rdx
	je	.LBB3_12
# BB#11:                                # %for.inc
                                        #   in Loop: Header=BB3_8 Depth=1
	decq	%rdx
	testq	%rdi, %rdi
	je	.LBB3_8
.LBB3_12:                               # %for.end
	testq	%rsi, %rsi
	je	.LBB3_15
.LBB3_13:                               # %lor.lhs.false27
	movq	32(%rsi), %rax
	movzbl	16(%rax), %edx
	cmpq	$13, %rdx
	jne	.LBB3_15
# BB#14:                                # %lor.lhs.false35
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB3_17
.LBB3_15:                               # %if.then42
	testq	$16, %rcx
	jne	.LBB3_22
# BB#16:                                # %if.then44
	movabsq	$.L.str, %rdi
	jmp	.LBB3_21
.LBB3_17:                               # %if.end47
	movq	8(%r9), %rax
	movzbl	16(%rax), %edx
	cmpq	$13, %rdx
	jne	.LBB3_19
# BB#18:                                # %lor.lhs.false55
	movq	8(%rax), %rax
	movq	128(%rax), %rax
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rax
	je	.LBB3_23
.LBB3_19:                               # %if.then63
	testq	$16, %rcx
	jne	.LBB3_22
# BB#20:                                # %if.then66
	movabsq	$.L.str4, %rdi
	jmp	.LBB3_21
.Ltmp34:
	.size	handle_format_arg_attribute, .Ltmp34-handle_format_arg_attribute
	.cfi_endproc

	.globl	check_function_format
	.align	16, 0x90
	.type	check_function_format,@function
check_function_format:                  # @check_function_format
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
	subq	$96, %rsp
.Ltmp46:
	.cfi_def_cfa_offset 144
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
	movq	%rdx, %r13
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB4_62
# BB#1:                                 # %for.body.lr.ph
	movabsq	$.L.str5, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB4_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_5 Depth 2
                                        #     Child Loop BB4_18 Depth 2
                                        #     Child Loop BB4_50 Depth 2
                                        #     Child Loop BB4_57 Depth 2
	movq	24(%r12), %rsi
	movq	%r15, %rdi
	callq	is_attribute_p
	testq	%r14, %rax
	je	.LBB4_61
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r15, %rbx
	movq	32(%r12), %rdi
	movq	$1, %rdx
	leaq	40(%rsp), %rsi
	callq	decode_format_attr
	testq	%r13, %r13
	je	.LBB4_46
# BB#4:                                 # %if.end.lr.ph.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	48(%rsp), %r15
	movq	%r15, %rcx
	movq	%r13, %rax
	.align	16, 0x90
.LBB4_5:                                # %if.end.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rcx
	je	.LBB4_7
# BB#6:                                 # %if.end3.i
                                        #   in Loop: Header=BB4_5 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_5
	jmp	.LBB4_46
.LBB4_7:                                # %for.end.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB4_46
# BB#8:                                 # %if.end8.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rax), %r13
	xorq	%rax, %rax
	movl	%eax, 88(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 72(%rsp)
	movq	$0, 64(%rsp)
	movq	32(%rsp), %rdi          # 8-byte Reload
	leaq	64(%rsp), %rsi
	leaq	40(%rsp), %rdx
	movq	%r13, %r8
	movq	%r15, %r9
	callq	check_format_info_recurse
	movslq	64(%rsp), %rax
	testq	%rax, %rax
	jle	.LBB4_28
# BB#9:                                 # %if.then11.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	40(%rsp), %eax
	cmpq	$2, %rax
	jne	.LBB4_12
# BB#10:                                # %if.then12.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_format_nonliteral(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_28
# BB#11:                                # %if.then14.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str7, %rsi
	jmp	.LBB4_27
.LBB4_12:                               # %if.else.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	56(%rsp), %rax
	testq	%rax, %rax
	je	.LBB4_28
# BB#13:                                # %while.cond.preheader.i
                                        #   in Loop: Header=BB4_2 Depth=1
	leaq	1(%r15), %rdx
	testq	%r13, %r13
	movabsq	$0, %rsi
	movq	%rsi, %rcx
	movabsq	$1, %rdi
	jne	.LBB4_15
# BB#14:                                # %while.cond.preheader.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rdi, %rcx
.LBB4_15:                               # %while.cond.preheader.i
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	%rax, %rdx
	jae	.LBB4_22
# BB#16:                                # %while.cond.preheader.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r13, %r13
	je	.LBB4_22
# BB#17:                                # %if.end22.i.preheader
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$2, %r15
	.align	16, 0x90
.LBB4_18:                               # %if.end22.i
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%rsi, %rcx
	jne	.LBB4_20
# BB#19:                                # %if.end22.i
                                        #   in Loop: Header=BB4_18 Depth=2
	movq	%rdi, %rcx
.LBB4_20:                               # %if.end22.i
                                        #   in Loop: Header=BB4_18 Depth=2
	cmpq	%rax, %r15
	jae	.LBB4_22
# BB#21:                                # %if.end22.i
                                        #   in Loop: Header=BB4_18 Depth=2
	incq	%r15
	testq	%r13, %r13
	jne	.LBB4_18
.LBB4_22:                               # %while.end.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_format_nonliteral(%rip), %eax
	testq	%rcx, %rcx
	je	.LBB4_25
# BB#23:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_format_security(%rip), %ecx
	orq	%rax, %rcx
	testq	%r14, %rcx
	jne	.LBB4_24
.LBB4_25:                               # %if.else30.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r14, %rax
	je	.LBB4_28
# BB#26:                                # %if.then32.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str9, %rsi
	jmp	.LBB4_27
.LBB4_24:                               # %if.then29.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str8, %rsi
.LBB4_27:                               # %if.end37.i
                                        #   in Loop: Header=BB4_2 Depth=1
	callq	status_warning
.LBB4_28:                               # %if.end37.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	68(%rsp), %rcx
	movl	64(%rsp), %eax
	testq	%rcx, %rcx
	jle	.LBB4_33
# BB#29:                                # %if.end37.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r14, %rax
	jne	.LBB4_33
# BB#30:                                # %land.lhs.true43.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	88(%rsp), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB4_33
# BB#31:                                # %land.lhs.true43.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_format_extra_args(%rip), %ecx
	testq	%r14, %rcx
	je	.LBB4_33
# BB#32:                                # %if.then48.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str10, %rsi
	callq	status_warning
	movl	64(%rsp), %eax
.LBB4_33:                               # %if.end49.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	72(%rsp), %rcx
	testq	%rcx, %rcx
	jle	.LBB4_38
# BB#34:                                # %if.end49.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r14, %rax
	jne	.LBB4_38
# BB#35:                                # %land.lhs.true55.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	88(%rsp), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB4_38
# BB#36:                                # %land.lhs.true55.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_format_extra_args(%rip), %ecx
	testq	%r14, %rcx
	je	.LBB4_38
# BB#37:                                # %if.then60.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str11, %rsi
	callq	status_warning
	movl	64(%rsp), %eax
.LBB4_38:                               # %if.end61.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	80(%rsp), %rcx
	testq	%rcx, %rcx
	jle	.LBB4_42
# BB#39:                                # %if.end61.i
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%r14, %rax
	jne	.LBB4_42
# BB#40:                                # %if.end61.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	88(%rsp), %eax
	testq	%r14, %rax
	jne	.LBB4_42
# BB#41:                                # %if.then70.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str12, %rsi
	callq	status_warning
.LBB4_42:                               # %if.end71.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	76(%rsp), %rax
	testq	%rax, %rax
	jle	.LBB4_44
# BB#43:                                # %if.then74.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str13, %rsi
	callq	status_warning
.LBB4_44:                               # %if.end75.i
                                        #   in Loop: Header=BB4_2 Depth=1
	movslq	84(%rsp), %rax
	testq	%rax, %rax
	movq	24(%rsp), %r13          # 8-byte Reload
	jle	.LBB4_46
# BB#45:                                # %if.then78.i
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str14, %rsi
	callq	status_warning
	.align	16, 0x90
.LBB4_46:                               # %check_format_info.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$0, 56(%rsp)
	movq	%rbx, %r15
	jne	.LBB4_61
# BB#47:                                # %check_format_info.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	warn_missing_format_attribute(%rip), %eax
	testq	%r14, %rax
	je	.LBB4_61
# BB#48:                                # %check_format_info.exit
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	40(%rsp), %eax
	andq	%r14, %rax
	cmpq	$2, %rax
	je	.LBB4_61
# BB#49:                                # %if.then7
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movq	48(%rax), %rbx
	jmp	.LBB4_50
	.align	16, 0x90
.LBB4_53:                               # %for.inc
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	(%rbx), %rbx
.LBB4_50:                               # %if.then7
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB4_55
# BB#51:                                # %for.body11
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	24(%rbx), %rsi
	movq	%r15, %rdi
	callq	is_attribute_p
	testq	%r14, %rax
	je	.LBB4_53
# BB#52:                                # %land.lhs.true16
                                        #   in Loop: Header=BB4_50 Depth=2
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
	movq	32(%rax), %rdi
	callq	decode_format_type
	movl	40(%rsp), %ecx
	andq	%r14, %rax
	cmpq	%rcx, %rax
	jne	.LBB4_53
# BB#54:                                # %for.end
                                        #   in Loop: Header=BB4_2 Depth=1
	testq	%rbx, %rbx
	jne	.LBB4_61
	.align	16, 0x90
.LBB4_55:                               # %if.then27
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rax
	testq	%rax, %rax
	je	.LBB4_61
# BB#56:                                # %for.body30.lr.ph
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	integer_types(%rip), %rcx
	.align	16, 0x90
.LBB4_57:                               # %for.body30
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rdx
	movzbl	16(%rdx), %esi
	cmpq	$13, %rsi
	jne	.LBB4_59
# BB#58:                                # %land.lhs.true35
                                        #   in Loop: Header=BB4_57 Depth=2
	movq	8(%rdx), %rdx
	movq	128(%rdx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB4_60
.LBB4_59:                               # %for.inc44
                                        #   in Loop: Header=BB4_57 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB4_57
	jmp	.LBB4_61
.LBB4_60:                               # %if.then49
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	40(%rsp), %eax
	leaq	(%rax,%rax,2), %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	format_types(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str6, %rdi
	callq	warning
	.align	16, 0x90
.LBB4_61:                               # %for.inc57
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB4_2
.LBB4_62:                               # %for.end60
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp52:
	.size	check_function_format, .Ltmp52-check_function_format
	.cfi_endproc

	.align	16, 0x90
	.type	decode_format_type,@function
decode_format_type:                     # @decode_format_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp61:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp62:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 96
.Ltmp65:
	.cfi_offset %rbx, -48
.Ltmp66:
	.cfi_offset %r12, -40
.Ltmp67:
	.cfi_offset %r13, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	leaq	2(%rbx), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movabsq	$-4294967296, %rsi      # imm = 0xFFFFFFFF00000000
	addq	%rdx, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movabsq	$-8589934592, %rsi      # imm = 0xFFFFFFFE00000000
	addq	%rdx, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	xorq	%r13, %r13
	movq	$1, %r15
	movabsq	$format_types, %r14
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB5_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14), %rbx
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	callq	strcmp
	leaq	-1(%r15), %rcx
	testq	%r12, %rax
	je	.LBB5_2
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	callq	strlen
	movq	%r12, %rsi
	leaq	4(%rax), %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, 32(%rsp)          # 8-byte Folded Reload
	jne	.LBB5_10
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$95, %rcx
	jne	.LBB5_10
# BB#5:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movzbl	1(%rcx), %ecx
	cmpq	$95, %rcx
	jne	.LBB5_10
# BB#6:                                 # %land.lhs.true19
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movzbl	(%rcx,%rdx), %ecx
	cmpq	$95, %rcx
	jne	.LBB5_10
# BB#7:                                 # %land.lhs.true25
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	movzbl	(%rcx,%rdx), %ecx
	cmpq	$95, %rcx
	jne	.LBB5_10
# BB#8:                                 # %land.lhs.true32
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%rsi, %r12
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	strncmp
	movq	%r12, %rsi
	testq	%rsi, %rax
	je	.LBB5_9
	.align	16, 0x90
.LBB5_10:                               # %for.inc
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rsi, %r12
	incq	%r13
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r15
	addq	$96, %r14
	cmpq	$4, %rax
	jl	.LBB5_1
	jmp	.LBB5_11
.LBB5_2:
	movq	%rcx, %r13
.LBB5_11:                               # %for.end
	movq	%r13, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_9:
	movq	24(%rsp), %r13          # 8-byte Reload
	jmp	.LBB5_11
.Ltmp70:
	.size	decode_format_type, .Ltmp70-decode_format_type
	.cfi_endproc

	.align	16, 0x90
	.type	check_format_info_recurse,@function
check_format_info_recurse:              # @check_format_info_recurse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 48
	subq	$816, %rsp              # imm = 0x330
.Ltmp82:
	.cfi_def_cfa_offset 864
.Ltmp83:
	.cfi_offset %rbx, -48
.Ltmp84:
	.cfi_offset %r12, -40
.Ltmp85:
	.cfi_offset %r13, -32
.Ltmp86:
	.cfi_offset %r14, -24
.Ltmp87:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movl	16(%r15), %eax
	movq	%rdx, 320(%rsp)         # 8-byte Spill
	movq	%rsi, 328(%rsp)         # 8-byte Spill
	movq	%rdi, 312(%rsp)         # 8-byte Spill
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$53, %rcx
	jne	.LBB6_1
# BB#3:                                 # %if.then6
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	48(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB6_19
# BB#4:                                 # %for.body.lr.ph
	movq	%r9, 296(%rsp)          # 8-byte Spill
	movq	%r8, 304(%rsp)          # 8-byte Spill
	xorq	%r14, %r14
	movabsq	$.L.str15, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_5:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_7 Depth 2
                                        #     Child Loop BB6_14 Depth 2
	movq	24(%rbx), %rsi
	movq	%r12, %rdi
	callq	is_attribute_p
	testq	%r13, %rax
	je	.LBB6_17
# BB#6:                                 # %if.then16
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	32(%rbx), %rax
	.align	16, 0x90
.LBB6_7:                                # %while.cond
                                        #   Parent Loop BB6_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rax), %rax
	movzbl	16(%rax), %ecx
	leaq	-114(%rcx), %rdx
	andq	%r13, %rdx
	cmpq	$3, %rdx
	jb	.LBB6_7
# BB#8:                                 # %lor.rhs
                                        #   in Loop: Header=BB6_5 Depth=1
	cmpq	$25, %rcx
	jne	.LBB6_10
# BB#9:                                 # %lor.lhs.false43
                                        #   in Loop: Header=BB6_5 Depth=1
	cmpq	$0, 40(%rax)
	jne	.LBB6_10
# BB#12:                                # %if.end47
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	40(%r15), %rcx
	testq	%rcx, %rcx
	je	.LBB6_17
# BB#13:                                #   in Loop: Header=BB6_5 Depth=1
	movq	32(%rax), %rax
	movq	$1, %rdx
	.align	16, 0x90
.LBB6_14:                               # %for.body56
                                        #   Parent Loop BB6_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %rsi
	andq	%r13, %rsi
	movq	%rdx, %rdi
	andq	%r13, %rdi
	cmpq	%rsi, %rdi
	je	.LBB6_15
# BB#16:                                # %for.inc
                                        #   in Loop: Header=BB6_14 Depth=2
	movq	(%rcx), %rcx
	incq	%rdx
	testq	%rcx, %rcx
	jne	.LBB6_14
	jmp	.LBB6_17
.LBB6_15:                               # %if.then59
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	32(%rcx), %rcx
	movq	312(%rsp), %rdi         # 8-byte Reload
	movq	328(%rsp), %rsi         # 8-byte Reload
	movq	320(%rsp), %rdx         # 8-byte Reload
	movq	304(%rsp), %r8          # 8-byte Reload
	movq	296(%rsp), %r9          # 8-byte Reload
	callq	check_format_info_recurse
	movq	$1, %r14
	.align	16, 0x90
.LBB6_17:                               # %for.inc65
                                        #   in Loop: Header=BB6_5 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_5
# BB#18:                                # %for.end68
	testq	$1, %r14
	movq	304(%rsp), %r8          # 8-byte Reload
	movq	296(%rsp), %r9          # 8-byte Reload
	jne	.LBB6_570
.LBB6_19:                               # %for.end68.if.end72_crit_edge
	movl	16(%r15), %eax
	jmp	.LBB6_20
.LBB6_1:                                # %entry
	cmpq	$115, %rcx
	jne	.LBB6_20
# BB#2:                                 # %if.then
	movq	32(%r15), %rcx
	movq	312(%rsp), %rdi         # 8-byte Reload
	movq	328(%rsp), %rsi         # 8-byte Reload
	movq	320(%rsp), %rdx         # 8-byte Reload
	callq	check_format_info_recurse
	jmp	.LBB6_570
.LBB6_20:                               # %if.end72
	movq	%r9, %r13
	andq	$255, %rax
	cmpq	$51, %rax
	jne	.LBB6_22
# BB#21:                                # %if.then79
	movq	40(%r15), %rcx
	movq	312(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	328(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	320(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdx
	movq	%r8, 304(%rsp)          # 8-byte Spill
	movq	%r13, %r9
	callq	check_format_info_recurse
	movq	48(%r15), %rcx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	movq	304(%rsp), %r8          # 8-byte Reload
	movq	%r13, %r9
	callq	check_format_info_recurse
	jmp	.LBB6_570
.LBB6_22:                               # %if.end86
	movq	%r8, 304(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB6_35
# BB#23:                                # %if.then89
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	callq	status_warning
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rax
	incq	%r13
	movq	304(%rsp), %rdi         # 8-byte Reload
	testq	%rdi, %rdi
	jne	.LBB6_24
# BB#25:                                # %if.then89
	movabsq	$1, %rcx
	jmp	.LBB6_26
.LBB6_35:                               # %if.end108
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	xorq	%r12, %r12
	cmpq	$59, %rcx
	jne	.LBB6_50
# BB#36:                                # %if.then115
	movq	32(%r15), %rdi
	movq	40(%r15), %rsi
	movq	global_trees(%rip), %r8
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_37:                               # %while.cond122
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movzbl	16(%rax), %edx
	leaq	-114(%rdx), %rdi
	andq	%r12, %rdi
	cmpq	$2, %rdi
	ja	.LBB6_40
# BB#38:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_37 Depth=1
	movq	32(%rax), %rdi
	cmpq	%r8, %rdi
	je	.LBB6_40
# BB#39:                                # %land.rhs
                                        #   in Loop: Header=BB6_37 Depth=1
	movq	8(%rax), %rbx
	movq	8(%rdi), %rcx
	movl	60(%rbx), %ebx
	movl	60(%rcx), %ecx
	xorq	%rbx, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB6_37
	.align	16, 0x90
.LBB6_40:                               # %while.cond170
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %r15
	movzbl	16(%r15), %esi
	leaq	-114(%rsi), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	ja	.LBB6_45
# BB#41:                                # %land.lhs.true191
                                        #   in Loop: Header=BB6_40 Depth=1
	movq	32(%r15), %rsi
	cmpq	%r8, %rsi
	je	.LBB6_43
# BB#42:                                # %land.rhs197
                                        #   in Loop: Header=BB6_40 Depth=1
	movq	8(%r15), %rcx
	movq	8(%rsi), %rdi
	movl	60(%rcx), %ecx
	movl	60(%rdi), %edi
	xorq	%rcx, %rdi
	testq	$65024, %rdi            # imm = 0xFE00
	je	.LBB6_40
	jmp	.LBB6_43
.LBB6_24:
	movabsq	$0, %rcx
.LBB6_26:                               # %if.then89
	cmpq	%rax, %r13
	jae	.LBB6_32
# BB#27:
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	.align	16, 0x90
.LBB6_28:                               # %while.body93
                                        # =>This Inner Loop Header: Depth=1
	testq	$1, %rcx
	jne	.LBB6_570
# BB#29:                                # %if.end97
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	(%rdi), %rdi
	incq	%r13
	testq	%rdi, %rdi
	movq	%rdx, %rcx
	jne	.LBB6_31
# BB#30:                                # %if.end97
                                        #   in Loop: Header=BB6_28 Depth=1
	movq	%rsi, %rcx
.LBB6_31:                               # %if.end97
                                        #   in Loop: Header=BB6_28 Depth=1
	cmpq	%rax, %r13
	jb	.LBB6_28
.LBB6_32:                               # %while.end101
	testq	%rcx, %rcx
	je	.LBB6_34
# BB#33:                                # %if.then104
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	24(%rcx), %eax
	incq	%rax
	movl	%eax, 24(%rcx)
	jmp	.LBB6_570
.LBB6_34:                               # %if.else
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	4(%rcx), %eax
	incq	%rax
	movl	%eax, 4(%rcx)
	jmp	.LBB6_570
.LBB6_45:                               # %while.end222
	cmpq	$25, %rsi
	jne	.LBB6_43
# BB#46:
	movq	%r14, %rbx
	movq	%r15, %r14
	movq	%rax, %r15
	jmp	.LBB6_47
.LBB6_43:                               # %if.else230
	movq	%r14, %rbx
	cmpq	$25, %rdx
	movq	%rax, %r14
	jne	.LBB6_44
.LBB6_47:                               # %if.end241
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB6_44
# BB#48:                                # %lor.lhs.false244
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	tree_low_cst
	movq	%rax, %r12
	testq	%r12, %r12
	js	.LBB6_44
# BB#49:                                # %lor.lhs.false244.if.end252_crit_edge
	movl	16(%r15), %eax
	movq	%rbx, %r14
.LBB6_50:                               # %if.end252
	andq	$255, %rax
	cmpq	$121, %rax
	jne	.LBB6_44
# BB#51:                                # %if.end262
	movq	32(%r15), %rbx
	movzbl	16(%rbx), %eax
	xorq	%r15, %r15
	cmpq	$34, %rax
	jne	.LBB6_52
# BB#53:                                # %land.lhs.true272
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$18, %rax
	jne	.LBB6_54
# BB#55:                                # %land.lhs.true281
	movq	%rbx, %rdi
	callq	decl_constant_value
	cmpq	%rbx, %rax
	je	.LBB6_56
# BB#57:                                # %land.lhs.true285
	movzbl	16(%rax), %ecx
	cmpq	$29, %rcx
	jne	.LBB6_58
# BB#59:                                # %if.then292
	movq	64(%rbx), %r15
	jmp	.LBB6_60
.LBB6_52:
	movq	%rbx, %rax
	jmp	.LBB6_60
.LBB6_54:
	movq	%rbx, %rax
	jmp	.LBB6_60
.LBB6_56:
	movq	%rbx, %rax
	jmp	.LBB6_60
.LBB6_58:
	movq	%rbx, %rax
.LBB6_60:                               # %if.end293
	movzbl	16(%rax), %ecx
	cmpq	$29, %rcx
	jne	.LBB6_44
# BB#61:                                # %if.end303
	movq	8(%rax), %rcx
	movq	8(%rcx), %rcx
	movq	128(%rcx), %rcx
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rcx
	movq	304(%rsp), %rbx         # 8-byte Reload
	je	.LBB6_63
# BB#62:                                # %if.then311
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	12(%rcx), %eax
	incq	%rax
	movl	%eax, 12(%rcx)
	jmp	.LBB6_570
.LBB6_63:                               # %if.end313
	movq	40(%rax), %rdx
	movslq	32(%rax), %rdi
	testq	%r15, %r15
	je	.LBB6_77
# BB#64:                                # %if.then317
	movq	%rdi, %rbx
	movq	%rdx, 296(%rsp)         # 8-byte Spill
	movzbl	16(%r15), %eax
	cmpq	$25, %rax
	jne	.LBB6_65
# BB#66:                                # %if.end325
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	host_integerp
	testq	%r14, %rax
	je	.LBB6_67
# BB#68:                                # %if.then328
	movq	32(%r15), %rax
	testq	%rax, %rax
	movq	%rbx, %rdi
	movq	304(%rsp), %rbx         # 8-byte Reload
	jle	.LBB6_76
# BB#69:                                # %land.lhs.true334
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB6_71
# BB#70:                                # %land.lhs.true334
	movq	%rsi, %rdx
.LBB6_71:                               # %land.lhs.true334
	cmpq	%rax, %rdi
	jle	.LBB6_73
# BB#72:                                # %land.lhs.true334
	movq	%rsi, %rcx
.LBB6_73:                               # %land.lhs.true334
	testq	%rcx, %rdx
	jne	.LBB6_75
# BB#74:                                # %land.lhs.true334
	movq	%rdi, %rax
.LBB6_75:                               # %land.lhs.true334
	movq	%rax, %rdi
.LBB6_76:                               # %if.end347
	movq	296(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB6_77
.LBB6_67:
	movq	296(%rsp), %rdx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	304(%rsp), %rbx         # 8-byte Reload
.LBB6_77:                               # %if.end347
	testq	%r12, %r12
	je	.LBB6_80
# BB#78:                                # %if.then349
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	subq	%r12, %rdi
	jle	.LBB6_44
# BB#79:                                # %if.end356
	addq	%r12, %rdx
.LBB6_80:                               # %if.end359
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_81
# BB#82:                                # %if.end364
	movq	%rdi, %rax
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB6_84
# BB#83:                                # %if.then367
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	16(%rcx), %eax
	incq	%rax
	movl	%eax, 16(%rcx)
	jmp	.LBB6_570
.LBB6_44:                               # %if.else238
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %eax
	incq	%rax
	movl	%eax, (%rcx)
.LBB6_570:                              # %return
	addq	$816, %rsp              # imm = 0x330
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_84:                               # %if.end369
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	-1(%rdx,%rdi), %eax
	testq	%rax, %rax
	je	.LBB6_85
.LBB6_81:                               # %if.then362
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	20(%rcx), %eax
	incq	%rax
	movl	%eax, 20(%rcx)
	jmp	.LBB6_570
.LBB6_85:                               # %while.cond378.preheader
	movq	%rdi, 288(%rsp)         # 8-byte Spill
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %r12
	movq	%r13, %rcx
	leaq	1(%rcx), %rax
	cmpq	%r12, %rax
	jae	.LBB6_86
.LBB6_87:                               # %while.body383
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB6_570
# BB#88:                                # %if.end387
                                        #   in Loop: Header=BB6_87 Depth=1
	movq	(%rbx), %rbx
	leaq	1(%rcx), %r15
	addq	$2, %rcx
	cmpq	%r12, %rcx
	movq	%r15, %rcx
	jb	.LBB6_87
	jmp	.LBB6_89
.LBB6_86:
	movq	%rcx, %r15
.LBB6_89:                               # %while.end391
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	24(%rcx), %eax
	incq	%rax
	movl	%eax, 24(%rcx)
	movq	%rdx, 808(%rsp)
	movq	%rdx, 296(%rsp)         # 8-byte Spill
	movq	%rbx, 800(%rsp)
	movq	320(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	%rax, 280(%rsp)         # 8-byte Spill
	leaq	(%rax,%rax,2), %rax
	movq	$5, %rcx
	movq	%rax, %r14
	movq	%rax, %r13
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	movl	%r12d, dollar_first_arg_num(%rip)
	movl	%eax, dollar_arguments_count(%rip)
	movl	%eax, dollar_max_arg_used(%rip)
	movb	%al, dollar_format_warned(%rip)
	jle	.LBB6_94
# BB#90:                                # %while.end391
	testq	%rbx, %rbx
	je	.LBB6_94
# BB#91:
	movq	%rbx, %rcx
.LBB6_92:                               # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rcx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB6_92
# BB#93:                                # %if.end.loopexit.i.i
	movl	%eax, dollar_arguments_count(%rip)
.LBB6_94:                               # %if.end.i.i
	movslq	dollar_arguments_alloc(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB6_100
# BB#95:                                # %if.then2.i.i
	movq	dollar_arguments_used(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB6_97
# BB#96:                                # %if.then4.i.i
	callq	free
.LBB6_97:                               # %if.end5.i.i
	movq	dollar_arguments_pointer_p(%rip), %rax
	testq	%rax, %rax
	je	.LBB6_99
# BB#98:                                # %if.then7.i.i
	movq	%rax, %rdi
	callq	free
.LBB6_99:                               # %if.end8.i.i
	movslq	dollar_arguments_count(%rip), %rax
	movl	%eax, dollar_arguments_alloc(%rip)
	movq	%rax, %rdi
	callq	xmalloc
	movslq	dollar_arguments_alloc(%rip), %rcx
	movq	%rax, dollar_arguments_used(%rip)
	movq	%rcx, %rdi
	callq	xmalloc
	movl	dollar_arguments_alloc(%rip), %edx
	movq	%rax, dollar_arguments_pointer_p(%rip)
.LBB6_100:                              # %if.end11.i.i
	movq	%rbx, 304(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdx
	je	.LBB6_110
# BB#101:                               # %if.then13.i.i
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	dollar_arguments_used(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	memset
	testq	%r12, %r12
	jg	.LBB6_102
# BB#103:                               # %if.then13.i.i
	movabsq	$1, %rax
	jmp	.LBB6_104
.LBB6_10:                               # %if.then46
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.check_format_info_recurse, %rcx
	movq	$1434, %rsi             # imm = 0x59A
	jmp	.LBB6_11
.LBB6_65:                               # %if.then324
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.check_format_info_recurse, %rcx
	movq	$1559, %rsi             # imm = 0x617
	jmp	.LBB6_11
.LBB6_102:
	movabsq	$0, %rax
.LBB6_104:                              # %if.then13.i.i
	cmpq	$0, 304(%rsp)           # 8-byte Folded Reload
	je	.LBB6_110
# BB#105:                               # %if.then13.i.i
	testq	%rax, %rax
	jne	.LBB6_110
# BB#106:
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	304(%rsp), %rdx         # 8-byte Reload
.LBB6_107:                              # %while.body20.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdx), %rsi
	movq	8(%rsi), %rsi
	movzbl	16(%rsi), %esi
	cmpq	$13, %rsi
	movq	%rax, %rsi
	jne	.LBB6_109
# BB#108:                               # %while.body20.i.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	%rcx, %rsi
.LBB6_109:                              # %while.body20.i.i
                                        #   in Loop: Header=BB6_107 Depth=1
	movq	dollar_arguments_pointer_p(%rip), %rdi
	movb	%sil, (%rdi,%rbx)
	movq	(%rdx), %rdx
	incq	%rbx
	testq	%rdx, %rdx
	jne	.LBB6_107
.LBB6_110:                              # %while.body.preheader.i
	movq	format_types+40(%r14), %rax
	movq	%rax, 264(%rsp)         # 8-byte Spill
	movq	format_types+48(%r14), %rcx
	movq	%rcx, 256(%rsp)         # 8-byte Spill
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	movq	$5, %rcx
	movq	%r13, %rsi
	shlq	%cl, %rsi
	leaq	format_types+60(%rsi), %rcx
	movq	%rcx, 240(%rsp)         # 8-byte Spill
	leaq	format_types+80(%rsi), %rcx
	movq	%rcx, 232(%rsp)         # 8-byte Spill
	leaq	format_types+64(%rsi), %rcx
	movq	%rcx, 224(%rsp)         # 8-byte Spill
	leaq	format_types+68(%rsi), %rcx
	movq	%rcx, 216(%rsp)         # 8-byte Spill
	leaq	format_types+8(%rsi), %rcx
	movq	%rcx, 208(%rsp)         # 8-byte Spill
	leaq	format_types+16(%rsi), %rcx
	movq	%rcx, 200(%rsp)         # 8-byte Spill
	leaq	format_types+72(%rsi), %rcx
	movq	%rcx, 192(%rsp)         # 8-byte Spill
	leaq	format_types+76(%rsi), %rcx
	movq	%rcx, 184(%rsp)         # 8-byte Spill
	leaq	format_types+88(%rsi), %rcx
	movq	%rcx, 176(%rsp)         # 8-byte Spill
	leaq	format_types+24(%rsi), %rcx
	movq	%rcx, 152(%rsp)         # 8-byte Spill
	leaq	4(%rax), %rcx
	movq	%rcx, 144(%rsp)         # 8-byte Spill
	leaq	40(%rax), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, 96(%rsp)          # 8-byte Spill
	decq	288(%rsp)               # 8-byte Folded Spill
	leaq	format_types(%r14), %rax
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	280(%rsp), %rax         # 8-byte Reload
	andq	%rax, %rdx
	movq	%rdx, 248(%rsp)         # 8-byte Spill
	leaq	format_types+32(%rsi), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
.LBB6_111:                              # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_150 Depth 2
                                        #       Child Loop BB6_152 Depth 3
                                        #     Child Loop BB6_168 Depth 2
                                        #     Child Loop BB6_197 Depth 2
                                        #     Child Loop BB6_221 Depth 2
                                        #     Child Loop BB6_227 Depth 2
                                        #     Child Loop BB6_253 Depth 2
                                        #       Child Loop BB6_257 Depth 3
                                        #     Child Loop BB6_278 Depth 2
                                        #     Child Loop BB6_295 Depth 2
                                        #       Child Loop BB6_296 Depth 3
                                        #       Child Loop BB6_324 Depth 3
                                        #       Child Loop BB6_321 Depth 3
                                        #     Child Loop BB6_358 Depth 2
                                        #       Child Loop BB6_364 Depth 3
                                        #       Child Loop BB6_367 Depth 3
                                        #     Child Loop BB6_465 Depth 2
                                        #       Child Loop BB6_475 Depth 3
                                        #       Child Loop BB6_480 Depth 3
                                        #     Child Loop BB6_121 Depth 2
                                        #     Child Loop BB6_124 Depth 2
	movq	808(%rsp), %rax
	movq	$0, 792(%rsp)
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB6_135
# BB#112:                               # %if.then.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	296(%rsp), %rcx         # 8-byte Reload
	subq	%rcx, %rax
	movq	288(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB6_114
# BB#113:                               # %if.then8.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str18, %rcx
	movq	%rcx, %rsi
	callq	status_warning
.LBB6_114:                              # %if.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	cmpq	$0, 16(%rax)
	je	.LBB6_118
# BB#115:                               # %if.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	800(%rsp), %rax
	testq	%rax, %rax
	je	.LBB6_118
# BB#116:                               # %if.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB6_118
# BB#117:                               # %if.then17.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	24(%rcx), %eax
	decq	%rax
	movl	%eax, 24(%rcx)
	movl	4(%rcx), %eax
	incq	%rax
	movl	%eax, 4(%rcx)
.LBB6_118:                              # %if.end18.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	96(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	jle	.LBB6_569
# BB#119:                               # %if.then21.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movslq	dollar_max_arg_used(%rip), %r8
	testq	%r8, %r8
	jle	.LBB6_132
# BB#120:                               # %for.body.lr.ph.i.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%r12, %r12
	movq	$1, %r14
	cmpq	$1, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_121
.LBB6_124:                              # %for.body.i.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	dollar_arguments_used(%rip), %rax
	movzbl	-1(%rax,%r14), %eax
	testq	%rax, %rax
	jne	.LBB6_130
# BB#125:                               # %land.lhs.true.i.i
                                        #   in Loop: Header=BB6_124 Depth=2
	movl	dollar_first_arg_num(%rip), %eax
	testq	%rax, %rax
	jne	.LBB6_127
# BB#126:                               #   in Loop: Header=BB6_124 Depth=2
	movq	$1, %r12
	jmp	.LBB6_130
.LBB6_127:                              # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB6_124 Depth=2
	movq	dollar_arguments_pointer_p(%rip), %rax
	movzbl	-1(%rax,%r14), %eax
	testq	%rax, %rax
	jne	.LBB6_128
# BB#129:                               # %if.else.i.i
                                        #   in Loop: Header=BB6_124 Depth=2
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str64, %rcx
	movq	%rcx, %rsi
	movq	%r14, %rdx
	movq	%r8, %rcx
	callq	status_warning
	movl	dollar_max_arg_used(%rip), %r8d
	jmp	.LBB6_130
.LBB6_128:                              #   in Loop: Header=BB6_124 Depth=2
	movq	$1, %r12
.LBB6_130:                              # %for.inc.i.i
                                        #   in Loop: Header=BB6_124 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	cmpq	%rdx, %rax
	jl	.LBB6_124
# BB#131:                               # %for.end.i.i
                                        #   in Loop: Header=BB6_111 Depth=1
	testq	$1, %r12
	jne	.LBB6_134
	jmp	.LBB6_132
.LBB6_135:                              # %if.end23.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 808(%rsp)
	movzbl	(%rax), %edx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	cmpq	$37, %rdx
	jne	.LBB6_569
# BB#136:                               # %if.end28.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	(%rcx), %ecx
	cmpq	$37, %rcx
	jne	.LBB6_137
# BB#140:                               # %if.then37.i
                                        #   in Loop: Header=BB6_111 Depth=1
	addq	$2, %rax
	movq	%rax, 808(%rsp)
	jmp	.LBB6_569
.LBB6_121:                              # %for.body.us.i.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	dollar_arguments_used(%rip), %rax
	movzbl	-1(%rax,%r14), %eax
	testq	%rax, %rax
	jne	.LBB6_123
# BB#122:                               # %if.else.us.i.i
                                        #   in Loop: Header=BB6_121 Depth=2
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str64, %rcx
	movq	%rcx, %rsi
	movq	%r14, %rdx
	movq	%r8, %rcx
	callq	status_warning
	movl	dollar_max_arg_used(%rip), %r8d
.LBB6_123:                              # %for.inc.us.i.i
                                        #   in Loop: Header=BB6_121 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	cmpq	%rdx, %rax
	jl	.LBB6_121
.LBB6_132:                              # %lor.lhs.false10.i.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	dollar_first_arg_num(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_569
# BB#133:                               # %lor.lhs.false10.i.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	dollar_arguments_count(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%rax, %r8
	jge	.LBB6_569
.LBB6_134:                              # %if.then15.i.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	328(%rsp), %rcx         # 8-byte Reload
	movl	24(%rcx), %eax
	decq	%rax
	movl	%eax, 24(%rcx)
	movl	8(%rcx), %eax
	incq	%rax
	movl	%eax, 8(%rcx)
	jmp	.LBB6_569
.LBB6_137:                              # %if.end28.i
                                        #   in Loop: Header=BB6_111 Depth=1
	testq	%rcx, %rcx
	jne	.LBB6_141
# BB#138:                               # %if.then32.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str19, %rcx
	jmp	.LBB6_139
.LBB6_141:                              # %if.end39.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%r14, %r14
	cmpq	$0, 248(%rsp)           # 8-byte Folded Reload
	movb	%r14b, 336(%rsp)
	jne	.LBB6_142
# BB#143:                               # %if.end39.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB6_144
# BB#145:                               # %if.then46.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rdx, %rdx
	movq	312(%rsp), %rdi         # 8-byte Reload
	leaq	808(%rsp), %rax
	movq	%rax, %rsi
	movq	304(%rsp), %rcx         # 8-byte Reload
	leaq	792(%rsp), %rax
	movq	%rax, %r8
	movq	112(%rsp), %r9          # 8-byte Reload
	callq	maybe_read_dollar_number
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	je	.LBB6_569
# BB#146:                               # %if.else.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$0, %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	jle	.LBB6_148
# BB#147:                               # %if.then52.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	320(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_148
.LBB6_142:                              #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_148
.LBB6_144:                              #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 80(%rsp)            # 8-byte Folded Spill
.LBB6_148:                              # %while.cond59.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r12
	movzbl	(%r12), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB6_166
# BB#149:                               # %land.rhs.lr.ph.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	152(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
.LBB6_150:                              # %land.rhs.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_152 Depth 3
	movq	$56, %rcx
	movq	88(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_163
# BB#151:                               # %while.body68.i
                                        #   in Loop: Header=BB6_150 Depth=2
	movq	264(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	movq	144(%rsp), %r13         # 8-byte Reload
	je	.LBB6_155
.LBB6_152:                              # %for.body.us.i1447.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_150 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	72(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	jne	.LBB6_154
# BB#153:                               # %if.else.us.i1448.i
                                        #   in Loop: Header=BB6_152 Depth=3
	movl	(%r13), %eax
	testq	%rax, %rax
	je	.LBB6_156
.LBB6_154:                              # %for.inc.us.i1451.i
                                        #   in Loop: Header=BB6_152 Depth=3
	movl	36(%r13), %eax
	addq	$40, %r13
	testq	%rax, %rax
	jne	.LBB6_152
	jmp	.LBB6_155
.LBB6_156:                              # %get_flag_spec.exit.i
                                        #   in Loop: Header=BB6_150 Depth=2
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_157
# BB#158:                               # %if.else76.i
                                        #   in Loop: Header=BB6_150 Depth=2
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	88(%rsp), %rcx          # 8-byte Reload
	movb	%cl, 336(%rsp,%rdx)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	jmp	.LBB6_159
.LBB6_157:                              # %if.then75.i
                                        #   in Loop: Header=BB6_150 Depth=2
	movq	12(%r13), %rcx
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str20, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	status_warning
	movq	808(%rsp), %r12
.LBB6_159:                              # %if.end85.i
                                        #   in Loop: Header=BB6_150 Depth=2
	movl	4(%r13), %eax
	testq	%rax, %rax
	je	.LBB6_162
# BB#160:                               # %if.then87.i
                                        #   in Loop: Header=BB6_150 Depth=2
	leaq	1(%r12), %rax
	movq	%rax, 808(%rsp)
	movzbl	1(%r12), %ecx
	testq	%rcx, %rcx
	movq	%rax, %r12
	je	.LBB6_161
.LBB6_162:                              # %if.end94.i
                                        #   in Loop: Header=BB6_150 Depth=2
	movq	%r12, %rax
	leaq	1(%rax), %r12
	movq	%r12, 808(%rsp)
	movzbl	1(%rax), %eax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB6_150
	jmp	.LBB6_166
.LBB6_163:                              # %while.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	232(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r14
	testq	%r14, %r14
	je	.LBB6_165
# BB#164:                               # %while.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	andq	$255, %rax
	cmpq	$42, %rax
	jne	.LBB6_165
# BB#175:                               # %if.then105.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	240(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%r12
	cmpq	$0, 800(%rsp)
	movb	%dl, 336(%rsp,%rsi)
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	movq	%r12, 808(%rsp)
	je	.LBB6_176
# BB#179:                               # %if.end120.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB6_185
# BB#180:                               # %if.then123.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	movabsq	$0, %rax
	jne	.LBB6_182
# BB#181:                               # %if.then123.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_182:                              # %if.then123.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	312(%rsp), %rdi         # 8-byte Reload
	leaq	808(%rsp), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	304(%rsp), %rcx         # 8-byte Reload
	leaq	800(%rsp), %rax
	movq	%rax, %r8
	movq	112(%rsp), %r9          # 8-byte Reload
	callq	maybe_read_dollar_number
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	je	.LBB6_569
# BB#183:                               # %if.else131.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB6_185
# BB#184:                               # %if.then134.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	320(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	-1(%rax,%rdx), %r15
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_186
.LBB6_161:                              # %if.then92.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str21, %rcx
	jmp	.LBB6_178
.LBB6_165:                              #   in Loop: Header=BB6_111 Depth=1
	movq	88(%rsp), %r14          # 8-byte Reload
.LBB6_166:                              # %while.cond169.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%r14, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	testq	$4, %rax
	je	.LBB6_190
# BB#167:                               # %while.body177.i.preheader
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%r12
	xorq	%rax, %rax
.LBB6_168:                              # %while.body177.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %r14
	cmpq	$48, %r14
	je	.LBB6_170
# BB#169:                               # %while.body177.i
                                        #   in Loop: Header=BB6_168 Depth=2
	movq	$1, %rax
.LBB6_170:                              # %while.body177.i
                                        #   in Loop: Header=BB6_168 Depth=2
	movq	%r12, 808(%rsp)
	movzbl	(%r12), %r14d
	movzbl	_sch_istable(%r14,%r14), %ecx
	incq	%r12
	testq	$4, %rcx
	jne	.LBB6_168
# BB#171:                               # %while.end184.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_174
# BB#172:                               # %while.end184.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_174
# BB#173:                               # %if.then192.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str24, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	status_warning
.LBB6_174:                              # %if.then196.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	240(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movb	%dl, 336(%rsp,%rsi)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	movb	%cl, 336(%rsp,%rax)
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_190
.LBB6_185:                              # %if.end142thread-pre-split.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rdx
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
.LBB6_186:                              # %if.end142.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 88(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	je	.LBB6_190
# BB#187:                               # %if.then146.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	800(%rsp), %rcx
	movq	32(%rcx), %rax
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	jne	.LBB6_189
# BB#188:                               # %if.then149.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rcx), %rcx
	movq	%rcx, 800(%rsp)
	incq	%r15
.LBB6_189:                              # %if.end151.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%r14), %rcx
	movq	%rcx, 728(%rsp)
	leaq	736(%rsp), %rcx
	movq	$0, 16(%rcx)
	movq	$0, 8(%rcx)
	movq	$0, (%rcx)
	movq	$.L.str23, 760(%rsp)
	movq	%rax, 768(%rsp)
	movl	%r15d, 776(%rsp)
	movq	$0, 784(%rsp)
	leaq	728(%rsp), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB6_190:                              # %if.end209.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_191
# BB#571:                               # %if.end249.land.lhs.true252_crit_edge1699.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r14
	movzbl	(%r14), %eax
	jmp	.LBB6_199
.LBB6_191:                              # %if.end209.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$3, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_222
# BB#192:                               # %land.lhs.true212.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r14
	movzbl	(%r14), %eax
	cmpq	$35, %rax
	jne	.LBB6_199
# BB#193:                               # %if.then216.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	1(%r14), %r12
	movq	%r12, 808(%rsp)
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	224(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movb	%dl, 336(%rsp,%rsi)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	movzbl	1(%r14), %eax
	movzbl	_sch_istable(%rax,%rax), %ecx
	testq	$4, %rcx
	movq	%r12, %r14
	jne	.LBB6_195
# BB#194:                               # %if.then235.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str25, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	status_warning
	movq	808(%rsp), %r14
	movzbl	(%r14), %eax
	movzbl	_sch_istable(%rax,%rax), %ecx
.LBB6_195:                              # %while.cond238.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	testq	$4, %rcx
	je	.LBB6_199
# BB#196:                               # %while.body246.i.preheader
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%r14
.LBB6_197:                              # %while.body246.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r14, 808(%rsp)
	movzbl	(%r14), %eax
	movzbl	_sch_istable(%rax,%rax), %ecx
	incq	%r14
	testq	$4, %rcx
	jne	.LBB6_197
# BB#198:                               # %land.lhs.true252.i.loopexit
                                        #   in Loop: Header=BB6_111 Depth=1
	decq	%r14
.LBB6_199:                              # %land.lhs.true252.i
                                        #   in Loop: Header=BB6_111 Depth=1
	andq	$255, %rax
	cmpq	$46, %rax
	jne	.LBB6_222
# BB#200:                               # %if.then256.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	1(%r14), %r13
	movq	%r13, 808(%rsp)
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	216(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movb	%dl, 336(%rsp,%rsi)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	176(%rsp), %rcx         # 8-byte Reload
	movq	(%rcx), %r12
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	testq	%r12, %r12
	je	.LBB6_216
# BB#201:                               # %land.lhs.true270.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	(%r13), %eax
	cmpq	$42, %rax
	jne	.LBB6_216
# BB#202:                               # %if.then274.i
                                        #   in Loop: Header=BB6_111 Depth=1
	addq	$2, %r14
	movq	%r14, 808(%rsp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB6_208
# BB#203:                               # %if.then278.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	movabsq	$0, %rax
	jne	.LBB6_205
# BB#204:                               # %if.then278.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_205:                              # %if.then278.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	312(%rsp), %rdi         # 8-byte Reload
	leaq	808(%rsp), %rcx
	movq	%rcx, %rsi
	movq	%rax, %rdx
	movq	304(%rsp), %rcx         # 8-byte Reload
	leaq	800(%rsp), %rax
	movq	%rax, %r8
	movq	112(%rsp), %r9          # 8-byte Reload
	callq	maybe_read_dollar_number
	movq	%rax, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rdx, %rcx
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	je	.LBB6_569
# BB#206:                               # %if.else286.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB6_208
# BB#207:                               # %if.then289.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	320(%rsp), %rcx         # 8-byte Reload
	movq	16(%rcx), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	-1(%rax,%rdx), %r15
	movq	$1, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_209
.LBB6_216:                              # %if.else337.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 280(%rsp)           # 8-byte Folded Reload
	je	.LBB6_219
# BB#217:                               # %land.lhs.true341.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	(%r13), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	jne	.LBB6_219
# BB#218:                               # %if.then349.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rcx
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str27, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	status_warning
	movq	808(%rsp), %r13
.LBB6_219:                              # %while.cond352.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	(%r13), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB6_222
# BB#220:                               # %while.body360.i.preheader
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%r13
.LBB6_221:                              # %while.body360.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r13, 808(%rsp)
	movzbl	(%r13), %eax
	movzbl	_sch_istable(%rax,%rax), %eax
	incq	%r13
	testq	$4, %rax
	jne	.LBB6_221
	jmp	.LBB6_222
.LBB6_208:                              # %if.end297thread-pre-split.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	movq	16(%rax), %rdx
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
.LBB6_209:                              # %if.end297.i
                                        #   in Loop: Header=BB6_111 Depth=1
	testq	%rdx, %rdx
	jne	.LBB6_210
.LBB6_222:                              #   in Loop: Header=BB6_111 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
	jmp	.LBB6_223
.LBB6_210:                              # %if.then301.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	800(%rsp), %rcx
	testq	%rcx, %rcx
	je	.LBB6_176
# BB#211:                               # %if.end305.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rcx), %rax
	cmpq	$0, 96(%rsp)            # 8-byte Folded Reload
	jne	.LBB6_213
# BB#212:                               # %if.then310.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rcx), %rcx
	movq	%rcx, 800(%rsp)
	incq	%r15
.LBB6_213:                              # %if.end314.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%r12), %rcx
	movq	%rcx, 664(%rsp)
	leaq	672(%rsp), %rcx
	movq	$0, 16(%rcx)
	movq	$0, 8(%rcx)
	movq	$0, (%rcx)
	movq	$.L.str26, 696(%rsp)
	movq	%rax, 704(%rsp)
	movl	%r15d, 712(%rsp)
	movq	$0, 720(%rsp)
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB6_215
# BB#214:                               #   in Loop: Header=BB6_111 Depth=1
	leaq	664(%rsp), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	jmp	.LBB6_223
.LBB6_215:                              # %if.then329.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	664(%rsp), %rax
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 56(%rcx)
.LBB6_223:                              # %if.end364.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rax, 72(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	cmpq	$2, 280(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	je	.LBB6_251
# BB#224:                               # %while.cond367.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	208(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	movq	(%r8), %rdx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	je	.LBB6_234
# BB#225:                               # %land.rhs371.lr.ph.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %rcx
	movzbl	(%rcx), %esi
	addq	$16, %r8
.LBB6_227:                              # %land.rhs371.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rdx), %edi
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	%rax, %rdi
	je	.LBB6_228
# BB#226:                               # %while.cond367.i
                                        #   in Loop: Header=BB6_227 Depth=2
	movq	16(%r8), %rdx
	addq	$32, %r8
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jne	.LBB6_227
	jmp	.LBB6_234
.LBB6_228:                              # %while.end381.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	movabsq	$0, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	je	.LBB6_234
# BB#229:                               # %if.then385.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	1(%rcx), %rsi
	movq	%rsi, 808(%rsp)
	cmpq	$0, (%r8)
	leaq	-16(%r8), %rdx
	je	.LBB6_232
# BB#230:                               # %land.lhs.true389.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rdx), %rax
	movzbl	(%rax), %eax
	movzbl	(%rsi), %esi
	cmpq	%rsi, %rax
	jne	.LBB6_232
# BB#231:                               # %if.then396.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%r8, %rdx
	addq	$2, %rcx
	movq	%rcx, 808(%rsp)
	leaq	8(%r8), %rcx
	addq	$12, %r8
	jmp	.LBB6_233
.LBB6_232:                              # %if.else399.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	-8(%r8), %rcx
	addq	$-4, %r8
.LBB6_233:                              # %if.end401.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rdx), %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movl	(%rcx), %eax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movl	(%r8), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	184(%rsp), %rcx         # 8-byte Reload
	movzbl	(%rcx), %edx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rsi
	sarq	%cl, %rsi
	movb	%dl, 336(%rsp,%rsi)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
.LBB6_234:                              # %if.end411.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_247
# BB#235:                               # %if.then413.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB6_238
# BB#236:                               # %cond.true.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_238
# BB#237:                               # %select.mid
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rdx, %rdx
.LBB6_238:                              # %cond.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	movq	$1, %rdi
	je	.LBB6_242
# BB#239:                               # %cond.false421.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_242
# BB#240:                               # %cond.false424.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc94(%rip), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, %rdi
	je	.LBB6_242
# BB#241:                               # %cond.false424.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rcx
	movq	%rcx, %rdi
.LBB6_242:                              # %cond.end429.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jle	.LBB6_247
# BB#243:                               # %if.then433.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, %rsi
	movabsq	$.L.str29, %rcx
	je	.LBB6_246
# BB#244:                               # %cond.false437.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$4, %rax
	movabsq	$.L.str30, %rax
	movq	%rax, %rcx
	je	.LBB6_246
# BB#245:                               # %cond.false437.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$.L.str31, %rax
	movq	%rax, %rcx
.LBB6_246:                              # %cond.end441.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str28, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	status_warning
.LBB6_247:                              # %if.end446.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_248
# BB#250:                               # %if.end446.if.then488_crit_edge.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r14
	movzbl	(%r14), %ebx
	jmp	.LBB6_264
.LBB6_248:                              # %if.end446.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$2, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_249
.LBB6_251:                              # %while.cond450.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r14
	movzbl	(%r14), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB6_263
# BB#252:                               # %land.rhs454.lr.ph.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r12
.LBB6_253:                              # %land.rhs454.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_257 Depth 3
	movq	$56, %rcx
	movq	40(%rsp), %r13          # 8-byte Reload
	shlq	%cl, %r13
	movq	$56, %rcx
	sarq	%cl, %r13
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_254
# BB#255:                               # %while.body461.i
                                        #   in Loop: Header=BB6_253 Depth=2
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_256
# BB#261:                               # %if.else472.i
                                        #   in Loop: Header=BB6_253 Depth=2
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	movb	%cl, 336(%rsp,%rdx)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	jmp	.LBB6_262
.LBB6_256:                              # %if.then467.i
                                        #   in Loop: Header=BB6_253 Depth=2
	movq	264(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	je	.LBB6_155
.LBB6_257:                              # %for.body.us.i1459.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_253 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r13, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	andq	%rsi, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB6_259
# BB#258:                               # %if.else.us.i1462.i
                                        #   in Loop: Header=BB6_257 Depth=3
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB6_260
.LBB6_259:                              # %for.inc.us.i1467.i
                                        #   in Loop: Header=BB6_257 Depth=3
	movl	36(%rax), %ecx
	addq	$40, %rax
	testq	%rcx, %rcx
	jne	.LBB6_257
	jmp	.LBB6_155
.LBB6_260:                              # %get_flag_spec.exit1469.i
                                        #   in Loop: Header=BB6_253 Depth=2
	movq	12(%rax), %rcx
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str20, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	status_warning
	movq	808(%rsp), %r14
.LBB6_262:                              # %if.end481.i
                                        #   in Loop: Header=BB6_253 Depth=2
	movq	%r14, %rax
	leaq	1(%rax), %r14
	movq	%r14, 808(%rsp)
	movzbl	1(%rax), %eax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB6_253
	jmp	.LBB6_263
.LBB6_249:                              # %if.end446.if.end520_crit_edge.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %r14
	jmp	.LBB6_269
.LBB6_254:                              #   in Loop: Header=BB6_111 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB6_263:                              # %if.end484.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, 280(%rsp)           # 8-byte Folded Reload
	jne	.LBB6_269
.LBB6_264:                              # %if.then488.i
                                        #   in Loop: Header=BB6_111 Depth=1
	andq	$255, %rbx
	cmpq	$97, %rbx
	jne	.LBB6_269
# BB#265:                               # %if.then488.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc99(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB6_269
# BB#266:                               # %if.then494.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	1(%r14), %eax
	addq	$-83, %rax
	andq	$255, %rax
	cmpq	$32, %rax
	ja	.LBB6_269
# BB#267:                               # %if.then494.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$4294967553, %rax       # imm = 0x100000101
	testq	%rax, %rdx
	je	.LBB6_269
# BB#268:                               # %if.then508.i
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%r14
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strlen
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	sarq	%cl, %rdx
	movq	$97, %rcx
	movb	%cl, 336(%rsp,%rdx)
	movabsq	$4294967296, %rcx       # imm = 0x100000000
	addq	%rcx, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movabsq	$0, %rcx
	movb	%cl, 336(%rsp,%rax)
	movq	%r14, 808(%rsp)
.LBB6_269:                              # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$2, 280(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	je	.LBB6_271
# BB#270:                               # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_271:                              # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	(%r14), %edx
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	cmpq	$37, %rdx
	movabsq	$0, %rcx
	jne	.LBB6_273
# BB#272:                               # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rcx
.LBB6_273:                              # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	testq	%rdx, %rdx
	je	.LBB6_275
# BB#274:                               # %if.end520.i
                                        #   in Loop: Header=BB6_111 Depth=1
	andq	%rcx, %rax
	jne	.LBB6_275
# BB#276:                               # %if.end532.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	200(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	incq	%r14
	movq	%r14, 808(%rsp)
	movq	(%rax), %rax
	jmp	.LBB6_278
.LBB6_275:                              # %if.then531.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str32, %rcx
.LBB6_139:                              # %cleanup.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rcx, %rsi
	callq	status_warning
	jmp	.LBB6_569
.LBB6_277:                              # %while.cond534.i
                                        #   in Loop: Header=BB6_278 Depth=2
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	248(%rcx), %rax
	addq	$248, %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
.LBB6_278:                              # %while.cond534.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rax, %rax
	je	.LBB6_299
# BB#279:                               # %land.rhs538.i
                                        #   in Loop: Header=BB6_278 Depth=2
	movq	%rax, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_277
# BB#280:                               # %if.end560.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_293
# BB#281:                               # %if.then562.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	12(%rax), %eax
	cmpq	$2, %rax
	movq	%rax, %rdx
	jne	.LBB6_284
# BB#282:                               # %cond.true566.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_284
# BB#283:                               # %select.mid219
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rdx, %rdx
.LBB6_284:                              # %cond.end571.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	movq	$1, %rdi
	je	.LBB6_288
# BB#285:                               # %cond.false576.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_288
# BB#286:                               # %cond.false579.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc94(%rip), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, %rdi
	je	.LBB6_288
# BB#287:                               # %cond.false579.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rcx
	movq	%rcx, %rdi
.LBB6_288:                              # %cond.end584.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jle	.LBB6_293
# BB#289:                               # %if.then588.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, %rsi
	movabsq	$.L.str29, %rcx
	je	.LBB6_292
# BB#290:                               # %cond.false592.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$4, %rax
	movabsq	$.L.str30, %rax
	movq	%rax, %rcx
	je	.LBB6_292
# BB#291:                               # %cond.false592.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$.L.str31, %rax
	movq	%rax, %rcx
.LBB6_292:                              # %cond.end597.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str35, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	status_warning
.LBB6_293:                              # %for.cond.preheader.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movzbl	336(%rsp), %eax
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$0, %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	je	.LBB6_352
# BB#294:                               # %for.body.lr.ph.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rbx, %rbx
	leaq	336(%rsp), %rcx
	movq	%rcx, %r14
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
.LBB6_295:                              # %for.body.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_296 Depth 3
                                        #       Child Loop BB6_324 Depth 3
                                        #       Child Loop BB6_321 Depth 3
	movq	$56, %rcx
	movq	%rax, %rdx
	movq	264(%rsp), %rsi         # 8-byte Reload
	movl	(%rsi), %esi
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	testq	%rsi, %rsi
	movq	136(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	je	.LBB6_155
.LBB6_296:                              # %for.body.us.i1476.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_295 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rdx, %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	andq	%rdi, %rsi
	cmpq	%rcx, %rsi
	jne	.LBB6_298
# BB#297:                               # %if.else.us.i1479.i
                                        #   in Loop: Header=BB6_296 Depth=3
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	-36(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB6_303
.LBB6_298:                              # %for.inc.us.i1484.i
                                        #   in Loop: Header=BB6_296 Depth=3
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	(%rcx), %esi
	addq	$40, %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%rsi, %rsi
	jne	.LBB6_296
	jmp	.LBB6_155
.LBB6_303:                              # %get_flag_spec.exit1486.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	%rbx, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movb	%al, 336(%rsp,%rdx)
	movsbq	(%r14), %r12
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	je	.LBB6_351
# BB#304:                               # %if.end624.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	232(%rax), %rax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_306
# BB#305:                               # %if.then632.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	-24(%rax), %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r8
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str36, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	status_warning
	incq	40(%rsp)                # 8-byte Folded Spill
	jmp	.LBB6_351
.LBB6_306:                              # %if.end636.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_351
# BB#307:                               # %if.then638.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	-8(%rax), %eax
	cmpq	$2, %rax
	movq	%rax, %rdx
	jne	.LBB6_310
# BB#308:                               # %cond.true642.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_310
# BB#309:                               # %select.mid224
                                        #   in Loop: Header=BB6_295 Depth=2
	xorq	%rdx, %rdx
.LBB6_310:                              # %cond.end647.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	movq	$1, %rdi
	je	.LBB6_314
# BB#311:                               # %cond.false652.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_314
# BB#312:                               # %cond.false655.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	flag_isoc94(%rip), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, %rdi
	je	.LBB6_314
# BB#313:                               # %cond.false655.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movabsq	$1, %rcx
	movq	%rcx, %rdi
.LBB6_314:                              # %cond.end660.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jle	.LBB6_319
# BB#315:                               # %if.then664.i
                                        #   in Loop: Header=BB6_295 Depth=2
	cmpq	$1, %rsi
	movabsq	$.L.str29, %rcx
	je	.LBB6_318
# BB#316:                               # %cond.false668.i
                                        #   in Loop: Header=BB6_295 Depth=2
	cmpq	$4, %rax
	movabsq	$.L.str30, %rax
	movq	%rax, %rcx
	je	.LBB6_318
# BB#317:                               # %cond.false668.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movabsq	$.L.str31, %rax
	movq	%rax, %rcx
.LBB6_318:                              # %cond.end673.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	-16(%rax), %r8
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str37, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	callq	status_warning
	movzbl	(%r14), %r12d
.LBB6_319:                              # %if.end675.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	$56, %rcx
	shlq	%cl, %r12
	movq	264(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	240(%rcx), %r13
	movq	$56, %rcx
	sarq	%cl, %r12
	testq	%rax, %rax
	je	.LBB6_328
# BB#320:                               # %for.body.lr.ph.i1489.i
                                        #   in Loop: Header=BB6_295 Depth=2
	testq	%r13, %r13
	movq	264(%rsp), %r14         # 8-byte Reload
	jne	.LBB6_324
.LBB6_321:                              # %for.body.us.i1493.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_295 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	jne	.LBB6_323
# BB#322:                               # %if.else.us.i1496.i
                                        #   in Loop: Header=BB6_321 Depth=3
	movl	4(%r14), %eax
	testq	%rax, %rax
	je	.LBB6_329
.LBB6_323:                              # %for.inc.us.i1501.i
                                        #   in Loop: Header=BB6_321 Depth=3
	movl	40(%r14), %eax
	addq	$40, %r14
	testq	%rax, %rax
	jne	.LBB6_321
	jmp	.LBB6_328
.LBB6_324:                              # %for.body.i1503.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_295 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	andq	%rdx, %rax
	cmpq	%rcx, %rax
	jne	.LBB6_327
# BB#325:                               # %if.then6.i.i
                                        #   in Loop: Header=BB6_324 Depth=3
	movl	4(%r14), %eax
	testq	%rax, %rax
	je	.LBB6_327
# BB#326:                               # %land.lhs.true.i1506.i
                                        #   in Loop: Header=BB6_324 Depth=3
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_329
.LBB6_327:                              # %for.inc.i1510.i
                                        #   in Loop: Header=BB6_324 Depth=3
	movl	40(%r14), %eax
	addq	$40, %r14
	testq	%rax, %rax
	jne	.LBB6_324
.LBB6_328:                              # %for.end.i1511.i
                                        #   in Loop: Header=BB6_295 Depth=2
	testq	%r13, %r13
	jne	.LBB6_351
	jmp	.LBB6_155
.LBB6_329:                              # %get_flag_spec.exit1512.i
                                        #   in Loop: Header=BB6_295 Depth=2
	testq	%r14, %r14
	je	.LBB6_351
# BB#330:                               # %land.lhs.true682.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	32(%r14), %eax
	cmpq	$2, %rax
	movq	%rax, %rdx
	jne	.LBB6_333
# BB#331:                               # %cond.true686.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_333
# BB#332:                               # %select.mid229
                                        #   in Loop: Header=BB6_295 Depth=2
	xorq	%rdx, %rdx
.LBB6_333:                              # %cond.end691.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	-8(%rcx), %esi
	cmpq	$2, %rsi
	jne	.LBB6_336
# BB#334:                               # %cond.true696.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rsi
	jne	.LBB6_336
# BB#335:                               # %select.mid234
                                        #   in Loop: Header=BB6_295 Depth=2
	xorq	%rsi, %rsi
.LBB6_336:                              # %cond.end701.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rsi, %rdx
	jle	.LBB6_351
# BB#337:                               # %if.then705.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	24(%r14), %r8
	testq	%r8, %r8
	jne	.LBB6_339
# BB#338:                               # %cond.false712.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	-16(%rcx), %r8
.LBB6_339:                              # %cond.end714.i
                                        #   in Loop: Header=BB6_295 Depth=2
	cmpq	$2, %rax
	movq	%rax, %rdx
	jne	.LBB6_342
# BB#340:                               # %cond.true719.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_342
# BB#341:                               # %select.mid236
                                        #   in Loop: Header=BB6_295 Depth=2
	xorq	%rdx, %rdx
.LBB6_342:                              # %cond.end724.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	movq	$1, %rdi
	je	.LBB6_346
# BB#343:                               # %cond.false729.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_346
# BB#344:                               # %cond.false732.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movl	flag_isoc94(%rip), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, %rdi
	je	.LBB6_346
# BB#345:                               # %cond.false732.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movabsq	$1, %rcx
	movq	%rcx, %rdi
.LBB6_346:                              # %cond.end737.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jle	.LBB6_351
# BB#347:                               # %if.then741.i
                                        #   in Loop: Header=BB6_295 Depth=2
	cmpq	$1, %rsi
	movabsq	$.L.str29, %rcx
	je	.LBB6_350
# BB#348:                               # %cond.false745.i
                                        #   in Loop: Header=BB6_295 Depth=2
	cmpq	$4, %rax
	movabsq	$.L.str30, %rax
	movq	%rax, %rcx
	je	.LBB6_350
# BB#349:                               # %cond.false745.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movabsq	$.L.str31, %rax
	movq	%rax, %rcx
.LBB6_350:                              # %cond.end750.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str38, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r8, %rcx
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	status_warning
.LBB6_351:                              # %for.inc.i
                                        #   in Loop: Header=BB6_295 Depth=2
	movzbl	337(%rsp,%rbx), %eax
	leaq	337(%rsp,%rbx), %r14
	leaq	1(%rbx), %rbx
	testq	%rax, %rax
	jne	.LBB6_295
.LBB6_352:                              # %for.end.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	subq	%rax, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$1, 280(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %rax
	movb	%al, 336(%rsp,%rbx)
	movabsq	$0, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jne	.LBB6_357
# BB#353:                               # %land.lhs.true771.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$97, %rsi
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strchr
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	je	.LBB6_355
# BB#354:                               # %land.lhs.true771.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
.LBB6_355:                              # %land.lhs.true771.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	192(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	leaq	336(%rsp), %rcx
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	strchr
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	je	.LBB6_357
# BB#356:                               # %land.lhs.true771.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB6_357:                              # %if.end778.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	256(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %r12d
	xorq	%r14, %r14
	jmp	.LBB6_358
.LBB6_299:                              # %if.then550.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$172, %rax
	jne	.LBB6_300
# BB#302:                               # %if.else558.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str34, %rcx
	jmp	.LBB6_301
.LBB6_380:                              # %for.inc860.i
                                        #   in Loop: Header=BB6_358 Depth=2
	incq	%r14
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	256(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx,%rax), %r12d
.LBB6_358:                              # %if.end778.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_364 Depth 3
                                        #       Child Loop BB6_367 Depth 3
	testq	%r12, %r12
	je	.LBB6_381
# BB#359:                               # %for.body784.i
                                        #   in Loop: Header=BB6_358 Depth=2
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_380
# BB#360:                               # %if.end795.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	256(%rsp), %rcx         # 8-byte Reload
	movl	4(%rcx,%rax), %r13d
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_380
# BB#361:                               # %if.end803.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	256(%rsp), %rcx         # 8-byte Reload
	movl	12(%rcx,%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB6_363
# BB#362:                               # %land.lhs.true808.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	240(%rax), %rax
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_380
.LBB6_363:                              # %if.end817.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	264(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %ecx
	testq	%rcx, %rcx
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	%rcx, %rdx
	je	.LBB6_155
.LBB6_364:                              # %for.body.us.i1519.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_358 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	andq	%rdi, %rdx
	cmpq	%rsi, %rdx
	jne	.LBB6_370
# BB#365:                               # %if.else.us.i1522.i
                                        #   in Loop: Header=BB6_364 Depth=3
	movl	(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB6_366
.LBB6_370:                              # %for.inc.us.i1527.i
                                        #   in Loop: Header=BB6_364 Depth=3
	movl	36(%rax), %edx
	addq	$40, %rax
	testq	%rdx, %rdx
	jne	.LBB6_364
	jmp	.LBB6_155
.LBB6_366:                              #   in Loop: Header=BB6_358 Depth=2
	movq	144(%rsp), %rsi         # 8-byte Reload
.LBB6_367:                              # %for.body.us.i1538.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_358 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r13, %rdx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rdx
	andq	%rdi, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB6_369
# BB#368:                               # %if.else.us.i1541.i
                                        #   in Loop: Header=BB6_367 Depth=3
	movl	(%rsi), %ecx
	testq	%rcx, %rcx
	je	.LBB6_371
.LBB6_369:                              # %for.inc.us.i1546.i
                                        #   in Loop: Header=BB6_367 Depth=3
	movl	36(%rsi), %ecx
	addq	$40, %rsi
	testq	%rcx, %rcx
	jne	.LBB6_367
	jmp	.LBB6_155
.LBB6_371:                              # %get_flag_spec.exit1550.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	$4, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	256(%rsp), %rcx         # 8-byte Reload
	movl	8(%rcx,%rdx), %edi
	movq	12(%rax), %rdx
	movq	12(%rsi), %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	xorq	%rax, %rax
	testq	%rdi, %rdi
	jne	.LBB6_372
# BB#376:                               # %if.else844.i
                                        #   in Loop: Header=BB6_358 Depth=2
	testq	%rbx, %rbx
	je	.LBB6_378
# BB#377:                               # %if.then850.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str41, %rsi
	jmp	.LBB6_374
.LBB6_372:                              # %if.then829.i
                                        #   in Loop: Header=BB6_358 Depth=2
	testq	%rbx, %rbx
	je	.LBB6_375
# BB#373:                               # %if.then835.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str39, %rsi
.LBB6_374:                              # %for.inc860.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	status_warning
	jmp	.LBB6_380
.LBB6_378:                              # %if.else854.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str42, %rsi
	jmp	.LBB6_379
.LBB6_375:                              # %if.else839.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str40, %rsi
.LBB6_379:                              # %for.inc860.i
                                        #   in Loop: Header=BB6_358 Depth=2
	movq	%r9, %r8
	callq	status_warning
	jmp	.LBB6_380
.LBB6_300:                              # %if.then557.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str33, %rcx
.LBB6_301:                              # %cleanup.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rcx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
	jmp	.LBB6_569
.LBB6_381:                              # %for.end862.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_format_y2k(%rip), %eax
	testq	%rax, %rax
	je	.LBB6_395
# BB#382:                               # %if.then864.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	240(%rax), %r14
	movq	$52, %rsi
	movq	%r14, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_383
# BB#386:                               # %if.else877.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$51, %rsi
	movq	%r14, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB6_392
# BB#387:                               # %if.else883.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$50, %rsi
	movq	%r14, %rdi
	callq	strchr
	testq	%rax, %rax
	movabsq	$0, %rax
	jne	.LBB6_389
# BB#388:                               # %if.else883.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_389:                              # %if.else883.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	$1, %rax
	addq	%rax, %rax
	jmp	.LBB6_390
.LBB6_383:                              # %if.then869.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$69, %rsi
	leaq	336(%rsp), %rax
	movq	%rax, %rdi
	callq	strchr
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB6_385
# BB#384:                               # %if.then869.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_385:                              # %if.then869.i
                                        #   in Loop: Header=BB6_111 Depth=1
	orq	$2, %rax
.LBB6_390:                              # %if.end891.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jne	.LBB6_391
# BB#393:                               # %if.then898.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str44, %rcx
	jmp	.LBB6_394
.LBB6_391:                              # %if.end891.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$3, %rax
	jne	.LBB6_395
.LBB6_392:                              # %if.then894.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str43, %rcx
.LBB6_394:                              # %if.end901.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rcx, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_395:                              # %if.end901.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	240(%rax), %rax
	movq	$91, %rsi
	movq	%rax, %rdi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_404
# BB#396:                               # %if.then906.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	808(%rsp), %rax
	movzbl	(%rax), %ecx
	cmpq	$94, %rcx
	jne	.LBB6_398
# BB#397:                               # %if.then910.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	1(%rax), %rdx
	movq	%rdx, 808(%rsp)
	movzbl	1(%rax), %ecx
	movq	%rdx, %rax
.LBB6_398:                              # %if.end912.i
                                        #   in Loop: Header=BB6_111 Depth=1
	andq	$255, %rcx
	cmpq	$93, %rcx
	jne	.LBB6_401
# BB#399:                               # %if.then916.i
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%rax
	jmp	.LBB6_400
.LBB6_176:                              # %if.then119.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
.LBB6_177:                              # %cleanup.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str22, %rcx
.LBB6_178:                              # %cleanup.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rcx, %rsi
	callq	status_warning
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_569
.LBB6_400:                              # %while.cond919.i.preheader
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rax, 808(%rsp)
.LBB6_401:                              # %while.cond919.i.preheader
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%rax
	movzbl	-1(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB6_402
# BB#403:                               # %if.then933.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str45, %rcx
	movq	%rcx, %rsi
	callq	status_warning
	jmp	.LBB6_404
.LBB6_402:                              # %while.cond919.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$93, %rcx
	jne	.LBB6_400
.LBB6_404:                              # %if.end935.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	cmpq	$2, 280(%rsp)           # 8-byte Folded Reload
	movabsq	$0, %r12
	movabsq	$0, %rbx
	je	.LBB6_436
# BB#405:                               # %if.then939.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	48(%rsp), %rcx          # 8-byte Reload
	andq	%rax, %rcx
	leaq	(%rcx,%rcx,2), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx,%rax,8), %rcx
	testq	%rcx, %rcx
	je	.LBB6_407
# BB#406:                               # %cond.end949.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rcx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_409
.LBB6_407:                              # %if.then961.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str46, %rcx
	movq	%rcx, %rsi
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	24(%rsp), %rcx          # 8-byte Reload
	callq	status_warning
	movq	800(%rsp), %rax
	incq	%r15
	testq	%rax, %rax
	je	.LBB6_176
# BB#408:                               # %if.end966.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rax), %rax
	movq	%rax, 800(%rsp)
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_569
.LBB6_409:                              # %if.else969.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	pedantic(%rip), %ecx
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	24(%rdx,%rax,8), %r12
	testq	%rcx, %rcx
	je	.LBB6_436
# BB#410:                               # %land.lhs.true971.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	16(%rcx,%rax,8), %edx
	movq	%rdx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	movq	%rdx, %rsi
	jne	.LBB6_413
# BB#411:                               # %cond.true974.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rsi
	jne	.LBB6_413
# BB#412:                               # %select.mid248
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rsi, %rsi
.LBB6_413:                              # %cond.end978.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$2, %rcx
	jne	.LBB6_416
# BB#414:                               # %cond.true982.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, 56(%rsp)            # 8-byte Folded Spill
	jne	.LBB6_416
# BB#415:                               # %select.mid250
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
.LBB6_416:                              # %cond.end986.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	56(%rsp), %rdi          # 8-byte Reload
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	jle	.LBB6_436
# BB#417:                               # %land.lhs.true990.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$2, %rax
	movq	%rdx, %rsi
	jne	.LBB6_420
# BB#418:                               # %cond.true993.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rsi
	jne	.LBB6_420
# BB#419:                               # %select.mid252
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rsi, %rsi
.LBB6_420:                              # %cond.end997.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	12(%rcx), %edi
	cmpq	$2, %rdi
	jne	.LBB6_423
# BB#421:                               # %cond.true1002.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_423
# BB#422:                               # %select.mid254
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rdi, %rdi
.LBB6_423:                              # %cond.end1007.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdi, %rsi
	jle	.LBB6_436
# BB#424:                               # %if.then1011.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$2, %rax
	jne	.LBB6_427
# BB#425:                               # %cond.true1014.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	warn_long_long(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdx
	jne	.LBB6_427
# BB#426:                               # %select.mid256
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rdx, %rdx
.LBB6_427:                              # %cond.end1018.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	movq	$1, %rdi
	je	.LBB6_431
# BB#428:                               # %cond.false1023.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc99(%rip), %ecx
	testq	%rcx, %rcx
	movq	$3, %rdi
	jne	.LBB6_431
# BB#429:                               # %cond.false1026.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movl	flag_isoc94(%rip), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movq	%rcx, %rdi
	je	.LBB6_431
# BB#430:                               # %cond.false1026.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rcx
	movq	%rcx, %rdi
.LBB6_431:                              # %cond.end1031.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdi, %rdx
	jle	.LBB6_436
# BB#432:                               # %if.then1035.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$1, %rsi
	movabsq	$.L.str29, %rcx
	je	.LBB6_435
# BB#433:                               # %cond.false1039.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$4, %rax
	movabsq	$.L.str30, %rax
	movq	%rax, %rcx
	je	.LBB6_435
# BB#434:                               # %cond.false1039.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$.L.str31, %rax
	movq	%rax, %rcx
.LBB6_435:                              # %cond.end1043.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %r9
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str47, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	movq	24(%rsp), %r8           # 8-byte Reload
	callq	status_warning
.LBB6_436:                              # %if.end1049.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	320(%rsp), %rax         # 8-byte Reload
	cmpq	$0, 16(%rax)
	je	.LBB6_569
# BB#437:                               # %if.end1054.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	8(%rax), %eax
	testq	%rax, %rax
	jne	.LBB6_439
# BB#438:                               # %land.lhs.true1058.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, %rbx
	je	.LBB6_440
.LBB6_439:                              # %lor.lhs.false1061.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB6_440
# BB#446:                               # %if.else1072.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rdx          # 8-byte Reload
	testq	%rcx, %rdx
	jne	.LBB6_447
# BB#452:                               # %if.else1077.i
                                        #   in Loop: Header=BB6_111 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB6_454
# BB#453:                               # %if.then1081.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str50, %rcx
	jmp	.LBB6_178
.LBB6_440:                              # %if.then1063.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	80(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB6_444
# BB#441:                               # %if.then1066.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	jne	.LBB6_442
# BB#445:                               # %if.else1069.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str49, %rcx
	jmp	.LBB6_443
.LBB6_447:                              # %if.then1075.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$32, %rcx
	movq	792(%rsp), %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rdx, 800(%rsp)
	movq	%rsi, %r15
	jmp	.LBB6_448
.LBB6_454:                              # %if.else1082.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	800(%rsp), %rdx
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	testq	%rdx, %rdx
	jne	.LBB6_448
# BB#455:                               # %if.then1086.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	jmp	.LBB6_177
.LBB6_442:                              # %if.then1068.i
                                        #   in Loop: Header=BB6_111 Depth=1
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str48, %rcx
.LBB6_443:                              # %if.end1141.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rcx, %rsi
	callq	status_warning
.LBB6_444:                              # %if.end1141.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	jne	.LBB6_465
	jmp	.LBB6_569
.LBB6_448:                              # %if.end1088.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	(%rdx), %rcx
	movq	32(%rdx), %r14
	movq	%rcx, 800(%rsp)
	movq	%rbx, 600(%rsp)
	movq	%r12, 608(%rsp)
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movl	%eax, 616(%rsp)
	movabsq	$0, %rax
	movl	%eax, 620(%rsp)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	240(%rax), %r12
	movq	$99, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	movabsq	$0, %rax
	je	.LBB6_450
# BB#449:                               # %if.end1088.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movabsq	$1, %rax
.LBB6_450:                              # %if.end1088.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	movl	%eax, 620(%rsp)
	movabsq	$0, %rax
	movl	%eax, 624(%rsp)
	movl	%eax, 628(%rsp)
	jne	.LBB6_451
# BB#456:                               # %if.else1111.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$87, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_458
# BB#457:                               # %if.then1116.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$1, %rax
	movl	%eax, 624(%rsp)
.LBB6_458:                              # %if.end1118.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$82, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	je	.LBB6_460
# BB#459:                               # %if.then1123.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$1, %rax
	movl	%eax, 628(%rsp)
	jmp	.LBB6_460
.LBB6_451:                              # %if.then1109.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$1, %rax
	movl	%eax, 624(%rsp)
.LBB6_460:                              # %if.end1126.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	$0, 632(%rsp)
	movq	%r14, 640(%rsp)
	movl	%r15d, 648(%rsp)
	movq	$0, 656(%rsp)
	cmpq	$0, 72(%rsp)            # 8-byte Folded Reload
	je	.LBB6_462
# BB#461:                               # %if.then1134.i
                                        #   in Loop: Header=BB6_111 Depth=1
	leaq	600(%rsp), %rax
	movq	72(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 56(%rcx)
.LBB6_462:                              # %if.end1141.thread.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 88(%rsp)            # 8-byte Folded Reload
	leaq	600(%rsp), %rax
	je	.LBB6_464
# BB#463:                               # %if.end1141.thread.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	88(%rsp), %rax          # 8-byte Reload
.LBB6_464:                              # %if.end1141.thread.i
                                        #   in Loop: Header=BB6_111 Depth=1
	movq	%rax, 88(%rsp)          # 8-byte Spill
.LBB6_465:                              # %for.body.i1553.i
                                        #   Parent Loop BB6_111 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB6_475 Depth 3
                                        #       Child Loop BB6_480 Depth 3
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %r12
	movq	8(%r12), %r13
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r13
	je	.LBB6_568
# BB#466:                               # %if.end.i1555.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%rcx, 72(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB6_467
# BB#468:                               # %if.end6.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rcx          # 8-byte Reload
	movl	48(%rcx), %edx
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	global_trees+216(%rip), %rdx
	movl	16(%rcx), %ecx
	cmpq	%rdx, 72(%rsp)          # 8-byte Folded Reload
	jne	.LBB6_471
# BB#469:                               # %land.lhs.true.i1556.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB6_475
	jmp	.LBB6_470
.LBB6_471:                              # %if.end10.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	jne	.LBB6_475
# BB#472:                               # %if.then13.i1558.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	simple_type_promotes_to
	testq	%rax, %rax
	je	.LBB6_474
# BB#473:                               # %if.then13.i1558.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rax, 72(%rsp)          # 8-byte Spill
.LBB6_474:                              # %if.then13.i1558.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	global_trees(%rip), %rax
.LBB6_475:                              # %while.cond.i.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_465 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %r14
	movzbl	16(%r14), %ecx
	addq	$-114, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	ja	.LBB6_478
# BB#476:                               # %land.lhs.true31.i.i
                                        #   in Loop: Header=BB6_475 Depth=3
	movq	32(%r14), %r12
	cmpq	%rax, %r12
	je	.LBB6_478
# BB#477:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB6_475 Depth=3
	movq	8(%r14), %rcx
	movq	8(%r12), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB6_475
.LBB6_478:                              # %for.cond52.preheader.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movslq	16(%rax), %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB6_509
# BB#479:                               # %for.body55.lr.ph.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	xorq	%rbx, %rbx
.LBB6_480:                              # %for.body55.i.i
                                        #   Parent Loop BB6_111 Depth=1
                                        #     Parent Loop BB6_465 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	16(%r13), %ecx
	movq	%r14, %r12
	cmpq	$13, %rcx
	jne	.LBB6_528
# BB#481:                               # %if.then61.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	8(%r13), %r13
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r13
	je	.LBB6_509
# BB#482:                               # %if.end66.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	24(%rax), %eax
	testq	%rax, %rax
	je	.LBB6_487
# BB#483:                               # %if.end66.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB6_487
# BB#484:                               # %if.end66.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	testq	%r12, %r12
	je	.LBB6_487
# BB#485:                               # %land.lhs.true71.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	%r12, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_487
# BB#486:                               # %if.then74.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str51, %rcx
	movq	%rcx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_487:                              # %if.end75.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	28(%rax), %eax
	testq	%rax, %rax
	je	.LBB6_492
# BB#488:                               # %if.end75.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB6_492
# BB#489:                               # %if.end75.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	testq	%r12, %r12
	je	.LBB6_492
# BB#490:                               # %land.lhs.true81.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	%r12, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_492
# BB#491:                               # %if.then84.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str52, %rcx
	movq	%rcx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_492:                              # %if.end85.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	xorq	%r14, %r14
	testq	%r12, %r12
	je	.LBB6_495
# BB#493:                               # %land.lhs.true87.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movzbl	16(%r12), %eax
	cmpq	$121, %rax
	jne	.LBB6_495
# BB#494:                               # %if.then93.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	32(%r12), %r14
.LBB6_495:                              # %if.end97.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	24(%rax), %eax
	testq	%rax, %rax
	je	.LBB6_503
# BB#496:                               # %if.end97.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB6_503
# BB#497:                               # %land.lhs.true102.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movzbl	17(%r13), %eax
	testq	$16, %rax
	jne	.LBB6_502
# BB#498:                               # %lor.lhs.false108.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	testq	%r14, %r14
	je	.LBB6_503
# BB#499:                               # %land.lhs.true110.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$99, %rcx
	je	.LBB6_502
# BB#500:                               # %land.lhs.true110.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	cmpq	$100, %rcx
	jne	.LBB6_503
# BB#501:                               # %land.lhs.true128.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB6_503
.LBB6_502:                              # %if.then135.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str53, %rcx
	movq	%rcx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_503:                              # %if.end136.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB6_508
# BB#504:                               # %if.end136.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movl	pedantic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_508
# BB#505:                               # %land.lhs.true141.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movzbl	17(%r13), %eax
	testq	$24, %rax
	jne	.LBB6_507
# BB#506:                               # %lor.lhs.false154.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	movzbl	62(%r13), %eax
	testq	$32, %rax
	je	.LBB6_508
.LBB6_507:                              # %if.then160.i.i
                                        #   in Loop: Header=BB6_480 Depth=3
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str54, %rcx
	movq	%rcx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_508:                              # %for.inc.i1566.i
                                        #   in Loop: Header=BB6_480 Depth=3
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rdx
	movq	88(%rsp), %rax          # 8-byte Reload
	movslq	16(%rax), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB6_480
	jmp	.LBB6_509
.LBB6_528:                              # %if.else162.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB6_531
# BB#529:                               # %if.then166.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str55, %rcx
	jmp	.LBB6_530
.LBB6_531:                              # %if.else167.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	xorq	%rax, %rax
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str56, %rcx
.LBB6_530:                              # %for.end.i1567.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rcx, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	status_warning
.LBB6_509:                              # %for.end.i1567.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movslq	16(%rax), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB6_568
# BB#510:                               # %if.end174.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movl	20(%rax), %ecx
	movq	128(%r13), %rax
	xorq	%r14, %r14
	testq	%rcx, %rcx
	je	.LBB6_515
# BB#511:                               # %if.then177.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	integer_types(%rip), %rcx
	cmpq	%rcx, %rax
	movq	$1, %r14
	je	.LBB6_515
# BB#512:                               # %if.then177.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	integer_types+8(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB6_515
# BB#513:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	integer_types+16(%rip), %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rcx
	movq	%rcx, %r14
	jne	.LBB6_515
# BB#514:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$1, %rcx
	movq	%rcx, %r14
.LBB6_515:                              # %if.end185.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	je	.LBB6_568
# BB#516:                               # %if.end189.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	global_trees+216(%rip), %rcx
	cmpq	%rcx, 72(%rsp)          # 8-byte Folded Reload
	jne	.LBB6_520
# BB#517:                               # %land.lhs.true192.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movl	pedantic(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB6_568
# BB#518:                               # %lor.lhs.false194.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$1, %rcx
	jne	.LBB6_520
# BB#519:                               # %lor.lhs.false194.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	testq	%r14, %r14
	jne	.LBB6_568
.LBB6_520:                              # %if.end200.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rsp), %rcx          # 8-byte Reload
	movl	16(%rcx), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$6, %rdx
	jne	.LBB6_534
# BB#521:                               # %land.lhs.true207.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movzbl	16(%rax), %edx
	cmpq	$6, %rdx
	jne	.LBB6_534
# BB#522:                               # %land.lhs.true214.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movl	pedantic(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB6_526
# BB#523:                               # %land.lhs.true214.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	je	.LBB6_526
# BB#524:                               # %lor.lhs.false219.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rbx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$1, %rdx
	jne	.LBB6_534
# BB#525:                               # %lor.lhs.false219.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	testq	%r14, %r14
	je	.LBB6_534
.LBB6_526:                              # %land.lhs.true224.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rax, %rdi
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB6_527
# BB#532:                               # %cond.false.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	callq	signed_type
	jmp	.LBB6_533
.LBB6_527:                              # %cond.true.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	callq	unsigned_type
.LBB6_533:                              # %cond.false.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	je	.LBB6_568
.LBB6_534:                              # %if.end237.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	integer_types(%rip), %rax
	cmpq	%rax, 72(%rsp)          # 8-byte Folded Reload
	jne	.LBB6_541
# BB#535:                               # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	jne	.LBB6_537
# BB#536:                               # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$1, %rax
.LBB6_537:                              # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$2, %rbx
	movabsq	$0, %rcx
	jge	.LBB6_539
# BB#538:                               # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$1, %rcx
.LBB6_539:                              # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	orq	%rcx, %rax
	cmpq	$1, %rax
	jne	.LBB6_541
# BB#540:                               # %land.lhs.true240.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	testq	%r14, %r14
	jne	.LBB6_568
.LBB6_541:                              # %if.end248.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	96(%rax), %rax
	movq	72(%rax), %rdx
	movq	96(%r13), %rax
	movzbl	16(%r13), %ecx
	movq	32(%rdx), %r14
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movq	%rcx, %rdx
	je	.LBB6_551
# BB#542:                               # %land.lhs.true255.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	$6, %rdx
	cmpq	$6, %rcx
	jne	.LBB6_544
# BB#543:                               #   in Loop: Header=BB6_465 Depth=2
	movq	%rax, %rsi
	jmp	.LBB6_551
.LBB6_544:                              # %land.lhs.true255.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	cmpq	$13, %rcx
	jne	.LBB6_547
# BB#545:                               # %land.lhs.true269.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	8(%r13), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$6, %rdx
	movq	$13, %rdx
	jne	.LBB6_547
# BB#546:                               #   in Loop: Header=BB6_465 Depth=2
	movq	%rax, %rsi
	jmp	.LBB6_551
.LBB6_547:                              # %if.then278.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movzbl	16(%rax), %edx
	cmpq	$33, %rdx
	jne	.LBB6_549
# BB#548:                               # %land.lhs.true287.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rax), %rdx
	testq	%rdx, %rdx
	jne	.LBB6_550
.LBB6_549:                              # %if.else302.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rax, %rdx
.LBB6_550:                              # %if.end309.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	32(%rdx), %r12
	testq	%r12, %r12
	movq	%rcx, %rdx
	movq	%rax, %rsi
	jne	.LBB6_554
.LBB6_551:                              # %if.then312.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	cmpq	$13, %rdx
	movabsq	$.L.str57, %rax
	movq	%rax, %r12
	je	.LBB6_553
# BB#552:                               # %if.then312.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movabsq	$.L.str58, %rax
	movq	%rax, %r12
.LBB6_553:                              # %if.then312.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%rsi, %rax
.LBB6_554:                              # %if.end322.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	cmpq	$6, %rcx
	jne	.LBB6_560
# BB#555:                               # %land.lhs.true329.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rsp), %rcx          # 8-byte Reload
	movzbl	16(%rcx), %ecx
	cmpq	$6, %rcx
	jne	.LBB6_560
# BB#556:                               # %land.lhs.true336.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movl	60(%r13), %ecx
	movq	72(%rsp), %rdx          # 8-byte Reload
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	jne	.LBB6_560
# BB#557:                               # %land.lhs.true336.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	testq	%rax, %rax
	je	.LBB6_560
# BB#558:                               # %land.lhs.true351.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movzbl	16(%rax), %ecx
	cmpq	$33, %rcx
	jne	.LBB6_560
# BB#559:                               # %if.then360.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	72(%rax), %rax
	movq	32(%rax), %r12
.LBB6_560:                              # %if.end368.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_568
# BB#561:                               # %if.then372.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r13
	testq	%r13, %r13
	je	.LBB6_564
# BB#562:                               # %land.lhs.true375.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_564
# BB#563:                               # %land.lhs.true375.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	%r13, %r14
.LBB6_564:                              # %if.end382.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	jne	.LBB6_565
# BB#566:                               # %if.else388.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str60, %rcx
	movq	%rcx, %rsi
	movq	%r14, %rdx
	movq	%r12, %rcx
	jmp	.LBB6_567
.LBB6_565:                              # %if.then386.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	312(%rsp), %rdi         # 8-byte Reload
	movabsq	$.L.str59, %rdx
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	movq	%r14, %rcx
.LBB6_567:                              # %for.inc391.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	80(%rsp), %r8           # 8-byte Reload
	callq	status_warning
.LBB6_568:                              # %for.inc391.i.i
                                        #   in Loop: Header=BB6_465 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB6_465
.LBB6_569:                              # %cleanup.i
                                        #   in Loop: Header=BB6_111 Depth=1
	cmpq	$0, 40(%rsp)            # 8-byte Folded Reload
	je	.LBB6_111
	jmp	.LBB6_570
.LBB6_155:                              # %for.end.i1452.i
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.get_flag_spec, %rcx
	movq	$1276, %rsi             # imm = 0x4FC
	jmp	.LBB6_11
.LBB6_467:                              # %if.then5.i.i
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.check_format_types, %rcx
	movq	$2275, %rsi             # imm = 0x8E3
	jmp	.LBB6_11
.LBB6_470:                              # %if.then9.i.i
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.check_format_types, %rcx
	movq	$2277, %rsi             # imm = 0x8E5
.LBB6_11:                               # %if.then46
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp88:
	.size	check_format_info_recurse, .Ltmp88-check_format_info_recurse
	.cfi_endproc

	.align	16, 0x90
	.type	status_warning,@function
status_warning:                         # @status_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp91:
	.cfi_def_cfa_offset 16
	subq	$448, %rsp              # imm = 0x1C0
.Ltmp92:
	.cfi_def_cfa_offset 464
.Ltmp93:
	.cfi_offset %rbx, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	testq	%rdi, %rdi
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	464(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	je	.LBB7_2
# BB#1:                                 # %if.then
	movq	$1, %rax
	movl	%eax, (%rdi)
	jmp	.LBB7_3
.LBB7_2:                                # %if.else
	movq	input_filename(%rip), %rcx
	movl	lineno(%rip), %r8d
	leaq	72(%rsp), %rbx
	leaq	48(%rsp), %rdx
	movq	$1, %r9
	movq	%rbx, %rdi
	callq	set_diagnostic_context
	movq	%rbx, %rdi
	callq	report_diagnostic
.LBB7_3:                                # %if.end
	addq	$448, %rsp              # imm = 0x1C0
	popq	%rbx
	retq
.Ltmp94:
	.size	status_warning, .Ltmp94-status_warning
	.cfi_endproc

	.align	16, 0x90
	.type	maybe_read_dollar_number,@function
maybe_read_dollar_number:               # @maybe_read_dollar_number
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp103:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp104:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp105:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp106:
	.cfi_def_cfa_offset 80
.Ltmp107:
	.cfi_offset %rbx, -48
.Ltmp108:
	.cfi_offset %r12, -40
.Ltmp109:
	.cfi_offset %r13, -32
.Ltmp110:
	.cfi_offset %r14, -24
.Ltmp111:
	.cfi_offset %r15, -16
	movq	(%rsi), %rbx
	movzbl	(%rbx), %eax
	movzbl	_sch_istable(%rax,%rax), %r10d
	movq	%rcx, %r15
	testq	$4, %r10
	je	.LBB8_3
# BB#1:                                 # %while.cond.preheader
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$56, %rcx
	sarq	%cl, %rdx
	xorq	%r14, %r14
	testq	$4, %r10
	je	.LBB8_2
# BB#5:
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movabsq	$0, %r8
	movabsq	$1, %r10
	movabsq	$7378697629483820647, %r11 # imm = 0x6666666666666667
	xorq	%r12, %r12
	.align	16, 0x90
.LBB8_6:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	andq	%r9, %rdi
	leaq	(%r14,%r14,4), %rax
	leaq	-48(%rdx,%rax,2), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r8, %r13
	js	.LBB8_8
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r10, %r13
.LBB8_8:                                # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	imulq	%r11
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$2, %rcx
	shrq	%cl, %rdx
	addq	%rax, %rdx
	andq	%r9, %rdx
	cmpq	%rdi, %rdx
	movq	%r8, %rax
	jne	.LBB8_10
# BB#9:                                 # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	%r10, %rax
.LBB8_10:                               # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	testq	%rax, %r13
	jne	.LBB8_12
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movq	$1, %r12
.LBB8_12:                               # %while.body
                                        #   in Loop: Header=BB8_6 Depth=1
	movzbl	1(%rbx), %eax
	movq	$56, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movzbl	_sch_istable(%rax,%rax), %edi
	movq	$56, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	testq	$4, %rdi
	jne	.LBB8_6
	jmp	.LBB8_13
.LBB8_3:                                # %if.then
	xorq	%r13, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rdx
	jne	.LBB8_4
	jmp	.LBB8_43
.LBB8_2:
	movq	%r9, (%rsp)             # 8-byte Spill
	movq	%r8, 8(%rsp)            # 8-byte Spill
	xorq	%r12, %r12
.LBB8_13:                               # %while.end
	cmpq	$36, %rax
	jne	.LBB8_14
# BB#15:                                # %if.end25
	incq	%rbx
	movq	%rbx, (%rsi)
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_21
# BB#16:                                # %if.end25
	movzbl	dollar_format_warned(%rip), %eax
	testq	$1, %rax
	jne	.LBB8_21
# BB#17:                                # %if.then28
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB8_19
# BB#18:
	movabsq	$.L.str29, %rdx
	jmp	.LBB8_20
.LBB8_14:                               # %if.then21
	xorq	%r13, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	je	.LBB8_43
.LBB8_4:                                # %if.then4
	movabsq	$.L.str50, %rsi
	xorq	%rax, %rax
	callq	status_warning
	movq	%rbx, %r13
	jmp	.LBB8_43
.LBB8_19:                               # %select.mid
	movabsq	$.L.str30, %rdx
.LBB8_20:                               # %select.end
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movabsq	$.L.str61, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	status_warning
	movq	$1, %rax
	movb	%al, dollar_format_warned(%rip)
.LBB8_21:                               # %if.end31
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r12
	jne	.LBB8_25
# BB#22:                                # %if.end31
	testq	%r13, %r14
	je	.LBB8_25
# BB#23:                                # %lor.lhs.false36
	movl	dollar_first_arg_num(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_26
# BB#24:                                # %lor.lhs.false36
	movl	dollar_arguments_count(%rip), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jle	.LBB8_26
.LBB8_25:                               # %if.then41
	movabsq	$.L.str62, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	status_warning
	jmp	.LBB8_43
.LBB8_26:                               # %if.end42
	movslq	dollar_max_arg_used(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jle	.LBB8_28
# BB#27:                                # %if.then45
	movl	%r14d, dollar_max_arg_used(%rip)
	movq	%r14, %rax
.LBB8_28:                               # %while.cond47.preheader
	movslq	dollar_arguments_alloc(%rip), %r12
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %r12
	jge	.LBB8_30
	.align	16, 0x90
.LBB8_29:                               # %while.body50
                                        # =>This Inner Loop Header: Depth=1
	leaq	16(%r12,%r12), %r12
	movq	dollar_arguments_used(%rip), %rdi
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rsi
	callq	xrealloc
	movq	dollar_arguments_pointer_p(%rip), %rdi
	movq	%rax, dollar_arguments_used(%rip)
	movq	%rbx, %rsi
	callq	xrealloc
	movslq	dollar_arguments_alloc(%rip), %rcx
	movq	%rax, dollar_arguments_pointer_p(%rip)
	movq	dollar_arguments_used(%rip), %rdi
	addq	%rcx, %rdi
	movq	%r12, %rdx
	subq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	movslq	dollar_max_arg_used(%rip), %rax
	movl	%r12d, dollar_arguments_alloc(%rip)
	cmpq	%rax, %rbx
	jl	.LBB8_29
.LBB8_30:                               # %while.end59
	movq	(%rsp), %rsi            # 8-byte Reload
	movzbl	56(%rsi), %eax
	testq	$8, %rax
	je	.LBB8_33
# BB#31:                                # %while.end59.if.else73_crit_edge
	movq	dollar_arguments_used(%rip), %rdx
	leaq	-1(%r14), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	jmp	.LBB8_32
.LBB8_33:                               # %land.lhs.true62
	movq	$32, %rcx
	movq	%r14, %rax
	movq	dollar_arguments_used(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	-1(%rdx,%rax), %ecx
	decq	%rax
	cmpq	$1, %rcx
	jne	.LBB8_32
# BB#34:                                # %if.then69
	movq	$2, %rcx
	movb	%cl, (%rdx,%rax)
	movq	(%rsi), %rcx
	movabsq	$.L.str63, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rdx
	callq	status_warning
	jmp	.LBB8_35
.LBB8_32:                               # %if.else73
	movq	$1, %rcx
	movb	%cl, (%rdx,%rax)
.LBB8_35:                               # %if.end77
	movq	8(%rsp), %rdi           # 8-byte Reload
	movl	dollar_first_arg_num(%rip), %eax
	testq	%rax, %rax
	je	.LBB8_42
# BB#36:                                # %if.then79
	movq	%r15, (%rdi)
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB8_40
# BB#37:
	movq	$1, %rax
	.align	16, 0x90
.LBB8_38:                               # %land.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%r15, %r15
	je	.LBB8_41
# BB#39:                                # %for.body
                                        #   in Loop: Header=BB8_38 Depth=1
	movq	(%r15), %r15
	movq	%r15, (%rdi)
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r14, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB8_38
.LBB8_40:                               # %for.end
	testq	%r15, %r15
	movq	%r14, %r13
	jne	.LBB8_43
.LBB8_41:                               # %if.then86
	movabsq	$.L.str16, %rax
	movabsq	$.L__FUNCTION__.maybe_read_dollar_number, %rcx
	movq	$1198, %rsi             # imm = 0x4AE
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB8_42:                               # %if.else88
	movq	$0, (%rdi)
	movq	%r14, %r13
.LBB8_43:                               # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp112:
	.size	maybe_read_dollar_number, .Ltmp112-maybe_read_dollar_number
	.cfi_endproc

	.type	warn_format,@object     # @warn_format
	.comm	warn_format,4,4
	.type	warn_format_y2k,@object # @warn_format_y2k
	.comm	warn_format_y2k,4,4
	.type	warn_format_extra_args,@object # @warn_format_extra_args
	.comm	warn_format_extra_args,4,4
	.type	warn_format_nonliteral,@object # @warn_format_nonliteral
	.comm	warn_format_nonliteral,4,4
	.type	warn_format_security,@object # @warn_format_security
	.comm	warn_format_security,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"format string arg not a string type"
	.size	.L.str, 36

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"args to be formatted is not '...'"
	.size	.L.str1, 34

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"strftime formats cannot format arguments"
	.size	.L.str2, 41

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"format string has invalid operand number"
	.size	.L.str3, 41

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"function does not return string type"
	.size	.L.str4, 37

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"format"
	.size	.L.str5, 7

	.type	format_types,@object    # @format_types
	.section	.rodata,"a",@progbits
	.align	16
format_types:
	.quad	.L.str65
	.quad	printf_length_specs
	.quad	print_char_table
	.quad	.L.str66
	.quad	0
	.quad	printf_flag_specs
	.quad	printf_flag_pairs
	.long	89                      # 0x59
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	112                     # 0x70
	.long	0                       # 0x0
	.long	76                      # 0x4c
	.quad	integer_types+40
	.quad	integer_types+40
	.quad	.L.str67
	.quad	scanf_length_specs
	.quad	scan_char_table
	.quad	.L.str68
	.quad	0
	.quad	scanf_flag_specs
	.quad	scanf_flag_pairs
	.long	179                     # 0xb3
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	42                      # 0x2a
	.long	76                      # 0x4c
	.quad	0
	.quad	0
	.quad	.L.str69
	.quad	0
	.quad	time_char_table
	.quad	.L.str70
	.quad	.L.str71
	.quad	strftime_flag_specs
	.quad	strftime_flag_pairs
	.long	4                       # 0x4
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	0
	.quad	0
	.quad	.L.str72
	.quad	strfmon_length_specs
	.quad	monetary_char_table
	.quad	.L.str73
	.quad	0
	.quad	strfmon_flag_specs
	.quad	strfmon_flag_pairs
	.long	1                       # 0x1
	.long	119                     # 0x77
	.long	35                      # 0x23
	.long	112                     # 0x70
	.long	0                       # 0x0
	.long	76                      # 0x4c
	.quad	0
	.quad	0
	.size	format_types, 384

	.type	.L.str6,@object         # @.str6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str6:
	.asciz	"function might be possible candidate for `%s' format attribute"
	.size	.L.str6, 63

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"format not a string literal, format string not checked"
	.size	.L.str7, 55

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"format not a string literal and no format arguments"
	.size	.L.str8, 52

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"format not a string literal, argument types not checked"
	.size	.L.str9, 56

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"too many arguments for format"
	.size	.L.str10, 30

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"unused arguments in $-style format"
	.size	.L.str11, 35

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"zero-length format string"
	.size	.L.str12, 26

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"format is a wide character string"
	.size	.L.str13, 34

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"unterminated format string"
	.size	.L.str14, 27

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"format_arg"
	.size	.L.str15, 11

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"c-format.c"
	.size	.L.str16, 11

	.type	.L__FUNCTION__.check_format_info_recurse,@object # @__FUNCTION__.check_format_info_recurse
.L__FUNCTION__.check_format_info_recurse:
	.asciz	"check_format_info_recurse"
	.size	.L__FUNCTION__.check_format_info_recurse, 26

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"null format string"
	.size	.L.str17, 19

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"embedded `\\0' in format"
	.size	.L.str18, 24

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"spurious trailing `%%' in format"
	.size	.L.str19, 33

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"repeated %s in format"
	.size	.L.str20, 22

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"missing fill character at end of strfmon format"
	.size	.L.str21, 48

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"too few arguments for format"
	.size	.L.str22, 29

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"field width"
	.size	.L.str23, 12

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"zero width in %s format"
	.size	.L.str24, 24

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"empty left precision in %s format"
	.size	.L.str25, 34

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"field precision"
	.size	.L.str26, 16

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"empty precision in %s format"
	.size	.L.str27, 29

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"%s does not support the `%s' %s length modifier"
	.size	.L.str28, 48

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"ISO C++"
	.size	.L.str29, 8

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"ISO C"
	.size	.L.str30, 6

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"ISO C89"
	.size	.L.str31, 8

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"conversion lacks type at end of format"
	.size	.L.str32, 39

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"unknown conversion type character `%c' in format"
	.size	.L.str33, 49

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"unknown conversion type character 0x%x in format"
	.size	.L.str34, 49

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"%s does not support the `%%%c' %s format"
	.size	.L.str35, 41

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"%s used with `%%%c' %s format"
	.size	.L.str36, 30

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"%s does not support %s"
	.size	.L.str37, 23

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"%s does not support %s with the `%%%c' %s format"
	.size	.L.str38, 49

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"%s ignored with %s and `%%%c' %s format"
	.size	.L.str39, 40

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"%s ignored with %s in %s format"
	.size	.L.str40, 32

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"use of %s and %s together with `%%%c' %s format"
	.size	.L.str41, 48

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"use of %s and %s together in %s format"
	.size	.L.str42, 39

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"`%%%c' yields only last 2 digits of year in some locales"
	.size	.L.str43, 57

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"`%%%c' yields only last 2 digits of year"
	.size	.L.str44, 41

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"no closing `]' for `%%[' format"
	.size	.L.str45, 32

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"use of `%s' length modifier with `%c' type character"
	.size	.L.str46, 53

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"%s does not support the `%%%s%c' %s format"
	.size	.L.str47, 43

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"operand number specified with suppressed assignment"
	.size	.L.str48, 52

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"operand number specified for format taking no argument"
	.size	.L.str49, 55

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"missing $ operand number in format"
	.size	.L.str50, 35

	.type	.L__FUNCTION__.check_format_types,@object # @__FUNCTION__.check_format_types
.L__FUNCTION__.check_format_types:
	.asciz	"check_format_types"
	.size	.L__FUNCTION__.check_format_types, 19

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"writing through null pointer (arg %d)"
	.size	.L.str51, 38

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"reading through null pointer (arg %d)"
	.size	.L.str52, 38

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"writing into constant object (arg %d)"
	.size	.L.str53, 38

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"extra type qualifiers in format argument (arg %d)"
	.size	.L.str54, 50

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"format argument is not a pointer (arg %d)"
	.size	.L.str55, 42

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"format argument is not a pointer to a pointer (arg %d)"
	.size	.L.str56, 55

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"pointer"
	.size	.L.str57, 8

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"different type"
	.size	.L.str58, 15

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"%s is not type %s (arg %d)"
	.size	.L.str59, 27

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"%s format, %s arg (arg %d)"
	.size	.L.str60, 27

	.type	.L__FUNCTION__.get_flag_spec,@object # @__FUNCTION__.get_flag_spec
.L__FUNCTION__.get_flag_spec:
	.asciz	"get_flag_spec"
	.size	.L__FUNCTION__.get_flag_spec, 14

	.type	dollar_format_warned,@object # @dollar_format_warned
	.local	dollar_format_warned
	.comm	dollar_format_warned,1,1
	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"%s does not support %%n$ operand number formats"
	.size	.L.str61, 48

	.type	dollar_first_arg_num,@object # @dollar_first_arg_num
	.local	dollar_first_arg_num
	.comm	dollar_first_arg_num,4,4
	.type	dollar_arguments_count,@object # @dollar_arguments_count
	.local	dollar_arguments_count
	.comm	dollar_arguments_count,4,4
	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"operand number out of range in format"
	.size	.L.str62, 38

	.type	dollar_max_arg_used,@object # @dollar_max_arg_used
	.local	dollar_max_arg_used
	.comm	dollar_max_arg_used,4,4
	.type	dollar_arguments_alloc,@object # @dollar_arguments_alloc
	.local	dollar_arguments_alloc
	.comm	dollar_arguments_alloc,4,4
	.type	dollar_arguments_used,@object # @dollar_arguments_used
	.local	dollar_arguments_used
	.comm	dollar_arguments_used,8,8
	.type	dollar_arguments_pointer_p,@object # @dollar_arguments_pointer_p
	.local	dollar_arguments_pointer_p
	.comm	dollar_arguments_pointer_p,8,8
	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"format argument %d used more than once in %s format"
	.size	.L.str63, 52

	.type	.L__FUNCTION__.maybe_read_dollar_number,@object # @__FUNCTION__.maybe_read_dollar_number
.L__FUNCTION__.maybe_read_dollar_number:
	.asciz	"maybe_read_dollar_number"
	.size	.L__FUNCTION__.maybe_read_dollar_number, 25

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"format argument %d unused before used argument %d in $-style format"
	.size	.L.str64, 68

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"printf"
	.size	.L.str65, 7

	.type	printf_length_specs,@object # @printf_length_specs
	.section	.rodata,"a",@progbits
	.align	16
printf_length_specs:
	.quad	.L.str138
	.long	2                       # 0x2
	.long	0                       # 0x0
	.quad	.L.str177
	.long	1                       # 0x1
	.long	3                       # 0x3
	.quad	.L.str178
	.long	3                       # 0x3
	.long	0                       # 0x0
	.quad	.L.str179
	.long	4                       # 0x4
	.long	2                       # 0x2
	.quad	.L.str180
	.long	4                       # 0x4
	.long	4                       # 0x4
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str96
	.long	5                       # 0x5
	.long	0                       # 0x0
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str139
	.long	6                       # 0x6
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str210
	.long	6                       # 0x6
	.long	4                       # 0x4
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str181
	.long	7                       # 0x7
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str118
	.long	8                       # 0x8
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	32
	.size	printf_length_specs, 288

	.type	print_char_table,@object # @print_char_table
	.align	16
print_char_table:
	.quad	.L.str153
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+40
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+8
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+56
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+72
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+72
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str154
	.quad	c_global_trees+40
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str155
	.quad	global_trees+240
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str156
	.quad	c_global_trees+56
	.quad	.L.str194
	.quad	.L.str195
	.quad	.L.str163
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+48
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+16
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+32
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+64
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str160
	.quad	c_global_trees+32
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str161
	.quad	c_global_trees+48
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str162
	.quad	c_global_trees+64
	.quad	.L.str196
	.quad	.L.str195
	.quad	.L.str159
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+48
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+16
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+32
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+64
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str160
	.quad	c_global_trees+32
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str161
	.quad	c_global_trees+48
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str162
	.quad	c_global_trees+64
	.quad	.L.str197
	.quad	.L.str195
	.quad	.L.str198
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str199
	.quad	.L.str95
	.quad	.L.str200
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str201
	.quad	.L.str95
	.quad	.L.str167
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+40
	.zero	24
	.zero	24
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str202
	.quad	c_global_trees+24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str203
	.quad	.L.str95
	.quad	.L.str170
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types
	.zero	24
	.zero	24
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str204
	.quad	.L.str205
	.quad	.L.str120
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+216
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str203
	.quad	.L.str167
	.quad	.L.str174
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+40
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+8
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+56
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+72
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str154
	.quad	c_global_trees+40
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str155
	.quad	global_trees+240
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str156
	.quad	c_global_trees+56
	.quad	.L.str95
	.quad	.L.str158
	.quad	.L.str206
	.long	0                       # 0x0
	.long	3                       # 0x3
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str199
	.quad	.L.str95
	.quad	.L.str207
	.long	0                       # 0x0
	.long	3                       # 0x3
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str201
	.quad	.L.str95
	.quad	.L.str129
	.long	0                       # 0x0
	.long	4                       # 0x4
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str202
	.quad	c_global_trees+24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str203
	.quad	.L.str95
	.quad	.L.str176
	.long	1                       # 0x1
	.long	4                       # 0x4
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str204
	.quad	.L.str208
	.quad	.L.str209
	.long	0                       # 0x0
	.long	4                       # 0x4
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+216
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str204
	.quad	.L.str95
	.zero	248
	.size	print_char_table, 3720

	.type	.L.str66,@object        # @.str66
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str66:
	.asciz	" +#0-'I"
	.size	.L.str66, 8

	.type	printf_flag_pairs,@object # @printf_flag_pairs
	.section	.rodata,"a",@progbits
	.align	16
printf_flag_pairs:
	.long	32                      # 0x20
	.long	43                      # 0x2b
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	48                      # 0x30
	.long	45                      # 0x2d
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	48                      # 0x30
	.long	112                     # 0x70
	.long	1                       # 0x1
	.long	105                     # 0x69
	.zero	16
	.size	printf_flag_pairs, 64

	.type	.L.str67,@object        # @.str67
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str67:
	.asciz	"scanf"
	.size	.L.str67, 6

	.type	scanf_length_specs,@object # @scanf_length_specs
	.section	.rodata,"a",@progbits
	.align	16
scanf_length_specs:
	.quad	.L.str138
	.long	2                       # 0x2
	.long	0                       # 0x0
	.quad	.L.str177
	.long	1                       # 0x1
	.long	3                       # 0x3
	.quad	.L.str178
	.long	3                       # 0x3
	.long	0                       # 0x0
	.quad	.L.str179
	.long	4                       # 0x4
	.long	2                       # 0x2
	.quad	.L.str180
	.long	4                       # 0x4
	.long	4                       # 0x4
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str96
	.long	5                       # 0x5
	.long	0                       # 0x0
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str139
	.long	6                       # 0x6
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str181
	.long	7                       # 0x7
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str118
	.long	8                       # 0x8
	.long	3                       # 0x3
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	32
	.size	scanf_length_specs, 256

	.type	scan_char_table,@object # @scan_char_table
	.align	16
scan_char_table:
	.quad	.L.str153
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+40
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+8
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+56
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+72
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+72
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str154
	.quad	c_global_trees+40
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str155
	.quad	global_trees+240
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str156
	.quad	c_global_trees+56
	.quad	.L.str157
	.quad	.L.str158
	.quad	.L.str159
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+48
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+16
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+32
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+64
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str160
	.quad	c_global_trees+32
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str161
	.quad	c_global_trees+48
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str162
	.quad	c_global_trees+64
	.quad	.L.str157
	.quad	.L.str158
	.quad	.L.str163
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+48
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+16
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+32
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+64
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	4                       # 0x4
	.zero	4
	.quad	0
	.quad	integer_types+80
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str160
	.quad	c_global_trees+32
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str161
	.quad	c_global_trees+48
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str162
	.quad	c_global_trees+64
	.quad	.L.str164
	.quad	.L.str158
	.quad	.L.str165
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+192
	.zero	24
	.zero	24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str166
	.quad	.L.str158
	.quad	.L.str167
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types
	.zero	24
	.zero	24
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str164
	.quad	.L.str169
	.quad	.L.str170
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types
	.zero	24
	.zero	24
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str171
	.quad	.L.str169
	.quad	.L.str172
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types
	.zero	24
	.zero	24
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str171
	.quad	.L.str173
	.quad	.L.str120
	.long	2                       # 0x2
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+216
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str164
	.quad	.L.str158
	.quad	.L.str174
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+40
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	integer_types+8
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	integer_types+56
	.long	2                       # 0x2
	.zero	4
	.quad	0
	.quad	integer_types+72
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str154
	.quad	c_global_trees+40
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str155
	.quad	global_trees+240
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str156
	.quad	c_global_trees+56
	.quad	.L.str95
	.quad	.L.str158
	.quad	.L.str175
	.long	1                       # 0x1
	.long	3                       # 0x3
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+192
	.zero	24
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.long	3                       # 0x3
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str166
	.quad	.L.str158
	.quad	.L.str129
	.long	1                       # 0x1
	.long	4                       # 0x4
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str164
	.quad	.L.str158
	.quad	.L.str176
	.long	1                       # 0x1
	.long	4                       # 0x4
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str168
	.quad	c_global_trees
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str171
	.quad	.L.str158
	.zero	248
	.size	scan_char_table, 3224

	.type	.L.str68,@object        # @.str68
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str68:
	.asciz	"*'I"
	.size	.L.str68, 4

	.type	scanf_flag_pairs,@object # @scanf_flag_pairs
	.section	.rodata,"a",@progbits
	.align	16
scanf_flag_pairs:
	.long	42                      # 0x2a
	.long	76                      # 0x4c
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	16
	.size	scanf_flag_pairs, 32

	.type	.L.str69,@object        # @.str69
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str69:
	.asciz	"strftime"
	.size	.L.str69, 9

	.type	time_char_table,@object # @time_char_table
	.section	.rodata,"a",@progbits
	.align	16
time_char_table:
	.quad	.L.str111
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str112
	.quad	.L.str95
	.quad	.L.str113
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str114
	.quad	.L.str115
	.quad	.L.str116
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str117
	.quad	.L.str95
	.quad	.L.str118
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str117
	.quad	.L.str119
	.quad	.L.str120
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str121
	.quad	.L.str95
	.quad	.L.str122
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str114
	.quad	.L.str95
	.quad	.L.str123
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str124
	.quad	.L.str125
	.quad	.L.str126
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str127
	.quad	.L.str119
	.quad	.L.str128
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	216
	.quad	.L.str95
	.quad	.L.str95
	.quad	.L.str129
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str127
	.quad	.L.str119
	.quad	.L.str130
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str95
	.quad	.L.str131
	.quad	.L.str132
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str117
	.quad	.L.str95
	.quad	.L.str133
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str95
	.quad	.L.str95
	.quad	.L.str134
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str135
	.quad	.L.str136
	.quad	.L.str137
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str117
	.quad	.L.str119
	.quad	.L.str138
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str112
	.quad	.L.str95
	.quad	.L.str139
	.long	0                       # 0x0
	.long	3                       # 0x3
	.zero	216
	.quad	.L.str140
	.quad	.L.str119
	.quad	.L.str141
	.long	0                       # 0x0
	.long	4                       # 0x4
	.zero	216
	.quad	.L.str117
	.quad	.L.str95
	.quad	.L.str142
	.long	0                       # 0x0
	.long	4                       # 0x4
	.zero	216
	.quad	.L.str95
	.quad	.L.str95
	.zero	248
	.size	time_char_table, 4960

	.type	.L.str70,@object        # @.str70
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str70:
	.asciz	"_-0^#"
	.size	.L.str70, 6

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"EO"
	.size	.L.str71, 3

	.type	strftime_flag_pairs,@object # @strftime_flag_pairs
	.section	.rodata,"a",@progbits
	.align	16
strftime_flag_pairs:
	.long	69                      # 0x45
	.long	79                      # 0x4f
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	95                      # 0x5f
	.long	45                      # 0x2d
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	95                      # 0x5f
	.long	48                      # 0x30
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	45                      # 0x2d
	.long	48                      # 0x30
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	94                      # 0x5e
	.long	35                      # 0x23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	16
	.size	strftime_flag_pairs, 96

	.type	.L.str72,@object        # @.str72
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str72:
	.asciz	"strfmon"
	.size	.L.str72, 8

	.type	strfmon_length_specs,@object # @strfmon_length_specs
	.section	.rodata,"a",@progbits
	.align	16
strfmon_length_specs:
	.quad	.L.str96
	.long	5                       # 0x5
	.long	0                       # 0x0
	.quad	0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	32
	.size	strfmon_length_specs, 64

	.type	monetary_char_table,@object # @monetary_char_table
	.align	16
monetary_char_table:
	.quad	.L.str93
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+200
	.zero	24
	.zero	24
	.zero	24
	.zero	24
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	global_trees+208
	.zero	24
	.zero	24
	.zero	24
	.quad	.L.str94
	.quad	.L.str95
	.zero	248
	.size	monetary_char_table, 496

	.type	.L.str73,@object        # @.str73
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str73:
	.asciz	"=^+(!-"
	.size	.L.str73, 7

	.type	strfmon_flag_pairs,@object # @strfmon_flag_pairs
	.section	.rodata,"a",@progbits
	.align	16
strfmon_flag_pairs:
	.long	43                      # 0x2b
	.long	40                      # 0x28
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	16
	.size	strfmon_flag_pairs, 32

	.type	.L.str74,@object        # @.str74
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str74:
	.asciz	"fill character"
	.size	.L.str74, 15

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"fill character in strfmon format"
	.size	.L.str75, 33

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"`^' flag"
	.size	.L.str76, 9

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"the `^' strfmon flag"
	.size	.L.str77, 21

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"`+' flag"
	.size	.L.str78, 9

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"the `+' strfmon flag"
	.size	.L.str79, 21

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"`(' flag"
	.size	.L.str80, 9

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"the `(' strfmon flag"
	.size	.L.str81, 21

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"`!' flag"
	.size	.L.str82, 9

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"the `!' strfmon flag"
	.size	.L.str83, 21

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"`-' flag"
	.size	.L.str84, 9

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"the `-' strfmon flag"
	.size	.L.str85, 21

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"field width in strfmon format"
	.size	.L.str86, 30

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"left precision"
	.size	.L.str87, 15

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"left precision in strfmon format"
	.size	.L.str88, 33

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"right precision"
	.size	.L.str89, 16

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"right precision in strfmon format"
	.size	.L.str90, 34

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"length modifier"
	.size	.L.str91, 16

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"length modifier in strfmon format"
	.size	.L.str92, 34

	.type	strfmon_flag_specs,@object # @strfmon_flag_specs
	.section	.rodata,"a",@progbits
	.align	16
strfmon_flag_specs:
	.long	61                      # 0x3d
	.long	0                       # 0x0
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str74
	.quad	.L.str75
	.long	0                       # 0x0
	.zero	4
	.long	94                      # 0x5e
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str76
	.quad	.L.str77
	.long	0                       # 0x0
	.zero	4
	.long	43                      # 0x2b
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str78
	.quad	.L.str79
	.long	0                       # 0x0
	.zero	4
	.long	40                      # 0x28
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str80
	.quad	.L.str81
	.long	0                       # 0x0
	.zero	4
	.long	33                      # 0x21
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str82
	.quad	.L.str83
	.long	0                       # 0x0
	.zero	4
	.long	45                      # 0x2d
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str84
	.quad	.L.str85
	.long	0                       # 0x0
	.zero	4
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str23
	.quad	.L.str86
	.long	0                       # 0x0
	.zero	4
	.long	35                      # 0x23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str87
	.quad	.L.str88
	.long	0                       # 0x0
	.zero	4
	.long	112                     # 0x70
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str89
	.quad	.L.str90
	.long	0                       # 0x0
	.zero	4
	.long	76                      # 0x4c
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str91
	.quad	.L.str92
	.long	0                       # 0x0
	.zero	4
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	strfmon_flag_specs, 440

	.type	.L.str93,@object        # @.str93
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str93:
	.asciz	"in"
	.size	.L.str93, 3

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"=^+(!-w#p"
	.size	.L.str94, 10

	.type	.L.str95,@object        # @.str95
.L.str95:
	.zero	1
	.size	.L.str95, 1

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"L"
	.size	.L.str96, 2

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"`_' flag"
	.size	.L.str97, 9

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"the `_' strftime flag"
	.size	.L.str98, 22

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"the `-' strftime flag"
	.size	.L.str99, 22

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"`0' flag"
	.size	.L.str100, 9

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"the `0' strftime flag"
	.size	.L.str101, 22

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"the `^' strftime flag"
	.size	.L.str102, 22

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"`#' flag"
	.size	.L.str103, 9

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"the `#' strftime flag"
	.size	.L.str104, 22

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"field width in strftime format"
	.size	.L.str105, 31

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"`E' modifier"
	.size	.L.str106, 13

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"the `E' strftime modifier"
	.size	.L.str107, 26

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"`O' modifier"
	.size	.L.str108, 13

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"the `O' strftime modifier"
	.size	.L.str109, 26

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"the `O' modifier"
	.size	.L.str110, 17

	.type	strftime_flag_specs,@object # @strftime_flag_specs
	.section	.rodata,"a",@progbits
	.align	16
strftime_flag_specs:
	.long	95                      # 0x5f
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str97
	.quad	.L.str98
	.long	4                       # 0x4
	.zero	4
	.long	45                      # 0x2d
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str84
	.quad	.L.str99
	.long	4                       # 0x4
	.zero	4
	.long	48                      # 0x30
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str100
	.quad	.L.str101
	.long	4                       # 0x4
	.zero	4
	.long	94                      # 0x5e
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str76
	.quad	.L.str102
	.long	4                       # 0x4
	.zero	4
	.long	35                      # 0x23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str103
	.quad	.L.str104
	.long	4                       # 0x4
	.zero	4
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str23
	.quad	.L.str105
	.long	4                       # 0x4
	.zero	4
	.long	69                      # 0x45
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str106
	.quad	.L.str107
	.long	3                       # 0x3
	.zero	4
	.long	79                      # 0x4f
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str108
	.quad	.L.str109
	.long	3                       # 0x3
	.zero	4
	.long	79                      # 0x4f
	.long	111                     # 0x6f
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	.L.str110
	.long	4                       # 0x4
	.zero	4
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	strftime_flag_specs, 400

	.type	.L.str111,@object       # @.str111
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str111:
	.asciz	"ABZab"
	.size	.L.str111, 6

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"^#"
	.size	.L.str112, 3

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"cx"
	.size	.L.str113, 3

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"E"
	.size	.L.str114, 2

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"3"
	.size	.L.str115, 2

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"HIMSUWdmw"
	.size	.L.str116, 10

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"-_0Ow"
	.size	.L.str117, 6

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"j"
	.size	.L.str118, 2

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"o"
	.size	.L.str119, 2

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"p"
	.size	.L.str120, 2

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"#"
	.size	.L.str121, 2

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"X"
	.size	.L.str122, 2

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"y"
	.size	.L.str123, 2

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"EO-_0w"
	.size	.L.str124, 7

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"4"
	.size	.L.str125, 2

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"Y"
	.size	.L.str126, 2

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"-_0EOw"
	.size	.L.str127, 7

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"%"
	.size	.L.str128, 2

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"C"
	.size	.L.str129, 2

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"D"
	.size	.L.str130, 2

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"2"
	.size	.L.str131, 2

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"eVu"
	.size	.L.str132, 4

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"FRTnrt"
	.size	.L.str133, 7

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"g"
	.size	.L.str134, 2

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"O-_0w"
	.size	.L.str135, 6

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"2o"
	.size	.L.str136, 3

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"G"
	.size	.L.str137, 2

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"h"
	.size	.L.str138, 2

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"z"
	.size	.L.str139, 2

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"O"
	.size	.L.str140, 2

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"kls"
	.size	.L.str141, 4

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"P"
	.size	.L.str142, 2

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"assignment suppression"
	.size	.L.str143, 23

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"the assignment suppression scanf feature"
	.size	.L.str144, 41

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"`a' flag"
	.size	.L.str145, 9

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"the `a' scanf flag"
	.size	.L.str146, 19

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"field width in scanf format"
	.size	.L.str147, 28

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"length modifier in scanf format"
	.size	.L.str148, 32

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"`'' flag"
	.size	.L.str149, 9

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"the `'' scanf flag"
	.size	.L.str150, 19

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"`I' flag"
	.size	.L.str151, 9

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"the `I' scanf flag"
	.size	.L.str152, 19

	.type	scanf_flag_specs,@object # @scanf_flag_specs
	.section	.rodata,"a",@progbits
	.align	16
scanf_flag_specs:
	.long	42                      # 0x2a
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str143
	.quad	.L.str144
	.long	0                       # 0x0
	.zero	4
	.long	97                      # 0x61
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str145
	.quad	.L.str146
	.long	4                       # 0x4
	.zero	4
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str23
	.quad	.L.str147
	.long	0                       # 0x0
	.zero	4
	.long	76                      # 0x4c
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str91
	.quad	.L.str148
	.long	0                       # 0x0
	.zero	4
	.long	39                      # 0x27
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str149
	.quad	.L.str150
	.long	4                       # 0x4
	.zero	4
	.long	73                      # 0x49
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str151
	.quad	.L.str152
	.long	4                       # 0x4
	.zero	4
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	scanf_flag_specs, 280

	.type	.L.str153,@object       # @.str153
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str153:
	.asciz	"di"
	.size	.L.str153, 3

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"signed size_t"
	.size	.L.str154, 14

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"ptrdiff_t"
	.size	.L.str155, 10

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"intmax_t"
	.size	.L.str156, 9

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"*w'I"
	.size	.L.str157, 5

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"W"
	.size	.L.str158, 2

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"u"
	.size	.L.str159, 2

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"size_t"
	.size	.L.str160, 7

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"unsigned ptrdiff_t"
	.size	.L.str161, 19

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"uintmax_t"
	.size	.L.str162, 10

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"oxX"
	.size	.L.str163, 4

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"*w"
	.size	.L.str164, 3

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"efgEG"
	.size	.L.str165, 6

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"*w'"
	.size	.L.str166, 4

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"c"
	.size	.L.str167, 2

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"wchar_t"
	.size	.L.str168, 8

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"cW"
	.size	.L.str169, 3

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"s"
	.size	.L.str170, 2

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"*aw"
	.size	.L.str171, 4

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"["
	.size	.L.str172, 2

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"cW["
	.size	.L.str173, 4

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"n"
	.size	.L.str174, 2

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"FaA"
	.size	.L.str175, 4

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"S"
	.size	.L.str176, 2

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"hh"
	.size	.L.str177, 3

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"l"
	.size	.L.str178, 2

	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"ll"
	.size	.L.str179, 3

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"q"
	.size	.L.str180, 2

	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"t"
	.size	.L.str181, 2

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"` ' flag"
	.size	.L.str182, 9

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"the ` ' printf flag"
	.size	.L.str183, 20

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"the `+' printf flag"
	.size	.L.str184, 20

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"the `#' printf flag"
	.size	.L.str185, 20

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"the `0' printf flag"
	.size	.L.str186, 20

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"the `-' printf flag"
	.size	.L.str187, 20

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"the `'' printf flag"
	.size	.L.str188, 20

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"the `I' printf flag"
	.size	.L.str189, 20

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"field width in printf format"
	.size	.L.str190, 29

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"precision"
	.size	.L.str191, 10

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"precision in printf format"
	.size	.L.str192, 27

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"length modifier in printf format"
	.size	.L.str193, 33

	.type	printf_flag_specs,@object # @printf_flag_specs
	.section	.rodata,"a",@progbits
	.align	16
printf_flag_specs:
	.long	32                      # 0x20
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str182
	.quad	.L.str183
	.long	0                       # 0x0
	.zero	4
	.long	43                      # 0x2b
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str78
	.quad	.L.str184
	.long	0                       # 0x0
	.zero	4
	.long	35                      # 0x23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str103
	.quad	.L.str185
	.long	0                       # 0x0
	.zero	4
	.long	48                      # 0x30
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str100
	.quad	.L.str186
	.long	0                       # 0x0
	.zero	4
	.long	45                      # 0x2d
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str84
	.quad	.L.str187
	.long	0                       # 0x0
	.zero	4
	.long	39                      # 0x27
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str149
	.quad	.L.str188
	.long	4                       # 0x4
	.zero	4
	.long	73                      # 0x49
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str151
	.quad	.L.str189
	.long	4                       # 0x4
	.zero	4
	.long	119                     # 0x77
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str23
	.quad	.L.str190
	.long	0                       # 0x0
	.zero	4
	.long	112                     # 0x70
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str191
	.quad	.L.str192
	.long	0                       # 0x0
	.zero	4
	.long	76                      # 0x4c
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str91
	.quad	.L.str193
	.long	0                       # 0x0
	.zero	4
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.zero	4
	.quad	0
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	printf_flag_specs, 440

	.type	.L.str194,@object       # @.str194
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str194:
	.asciz	"-wp0 +'I"
	.size	.L.str194, 9

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"i"
	.size	.L.str195, 2

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"-wp0#"
	.size	.L.str196, 6

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"-wp0'I"
	.size	.L.str197, 7

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"fgG"
	.size	.L.str198, 4

	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"-wp0 +#'"
	.size	.L.str199, 9

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"eE"
	.size	.L.str200, 3

	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"-wp0 +#"
	.size	.L.str201, 8

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"wint_t"
	.size	.L.str202, 7

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"-w"
	.size	.L.str203, 3

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"-wp"
	.size	.L.str204, 4

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"cR"
	.size	.L.str205, 3

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"F"
	.size	.L.str206, 2

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"aA"
	.size	.L.str207, 3

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"R"
	.size	.L.str208, 2

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"m"
	.size	.L.str209, 2

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"Z"
	.size	.L.str210, 2

	.type	.L__FUNCTION__.decode_format_attr,@object # @__FUNCTION__.decode_format_attr
.L__FUNCTION__.decode_format_attr:
	.asciz	"decode_format_attr"
	.size	.L__FUNCTION__.decode_format_attr, 19

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"unrecognized format specifier"
	.size	.L.str211, 30

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"`%s' is an unrecognized format function type"
	.size	.L.str212, 45

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"format string arg follows the args to be formatted"
	.size	.L.str213, 51


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
