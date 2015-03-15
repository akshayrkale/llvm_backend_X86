	.file	"obstack.c.bc"
	.text
	.align	16, 0x90
	.type	print_and_abort,@function
print_and_abort:                        # @print_and_abort
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	callq	fwrite
	movl	obstack_exit_failure(%rip), %edi
	callq	exit
.Ltmp2:
	.size	print_and_abort, .Ltmp2-print_and_abort
	.cfi_endproc

	.globl	_obstack_begin
	.align	16, 0x90
	.type	_obstack_begin,@function
_obstack_begin:                         # @_obstack_begin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp8:
	.cfi_def_cfa_offset 32
.Ltmp9:
	.cfi_offset %rbx, -24
.Ltmp10:
	.cfi_offset %r14, -16
	movq	%rcx, %rax
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	movq	$4072, %rdi             # imm = 0xFE8
	je	.LBB1_2
# BB#1:                                 # %entry
	movq	%rbx, %rdi
.LBB1_2:                                # %entry
	testq	%rcx, %rdx
	movq	%rax, 56(%r14)
	movq	%r8, 64(%r14)
	movq	%rdi, (%r14)
	movq	$7, %rcx
	je	.LBB1_4
# BB#3:                                 # %entry
	decq	%rdx
	movq	%rdx, %rcx
.LBB1_4:                                # %entry
	movl	%ecx, 48(%r14)
	movzbl	80(%r14), %ecx
	andq	$254, %rcx
	movb	%cl, 80(%r14)
	callq	*%rax
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 8(%r14)
	jne	.LBB1_6
# BB#5:                                 # %if.then17
	callq	*obstack_alloc_failed_handler(%rip)
.LBB1_6:                                # %if.end18
	leaq	16(%rbx), %rax
	movq	%rax, 16(%r14)
	movq	%rax, 24(%r14)
	movq	(%r14), %rax
	addq	%rbx, %rax
	movq	%rax, (%rbx)
	movq	%rax, 32(%r14)
	movq	$0, 8(%rbx)
	movzbl	80(%r14), %ecx
	andq	$249, %rcx
	movq	$1, %rax
	movb	%cl, 80(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp11:
	.size	_obstack_begin, .Ltmp11-_obstack_begin
	.cfi_endproc

	.globl	_obstack_begin_1
	.align	16, 0x90
	.type	_obstack_begin_1,@function
_obstack_begin_1:                       # @_obstack_begin_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp17:
	.cfi_def_cfa_offset 32
.Ltmp18:
	.cfi_offset %rbx, -24
.Ltmp19:
	.cfi_offset %r14, -16
	movq	%rcx, %rax
	movq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	movq	$4072, %rsi             # imm = 0xFE8
	je	.LBB2_2
# BB#1:                                 # %entry
	movq	%rdi, %rsi
.LBB2_2:                                # %entry
	testq	%rcx, %rdx
	movq	%rax, 56(%rbx)
	movq	%r8, 64(%rbx)
	movq	%rsi, (%rbx)
	movq	$7, %rcx
	je	.LBB2_4
# BB#3:                                 # %entry
	decq	%rdx
	movq	%rdx, %rcx
.LBB2_4:                                # %entry
	movl	%ecx, 48(%rbx)
	movq	%r9, 72(%rbx)
	movzbl	80(%rbx), %ecx
	orq	$1, %rcx
	movb	%cl, 80(%rbx)
	movq	%r9, %rdi
	callq	*%rax
	movq	%rax, %r14
	testq	%r14, %r14
	movq	%r14, 8(%rbx)
	jne	.LBB2_6
# BB#5:                                 # %if.then18
	callq	*obstack_alloc_failed_handler(%rip)
.LBB2_6:                                # %if.end19
	leaq	16(%r14), %rax
	movq	%rax, 16(%rbx)
	movq	%rax, 24(%rbx)
	movq	(%rbx), %rax
	addq	%r14, %rax
	movq	%rax, (%r14)
	movq	%rax, 32(%rbx)
	movq	$0, 8(%r14)
	movzbl	80(%rbx), %ecx
	andq	$249, %rcx
	movq	$1, %rax
	movb	%cl, 80(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp20:
	.size	_obstack_begin_1, .Ltmp20-_obstack_begin_1
	.cfi_endproc

	.globl	_obstack_newchunk
	.align	16, 0x90
	.type	_obstack_newchunk,@function
_obstack_newchunk:                      # @_obstack_newchunk
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp29:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp30:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 48
.Ltmp32:
	.cfi_offset %rbx, -48
.Ltmp33:
	.cfi_offset %r12, -40
.Ltmp34:
	.cfi_offset %r13, -32
.Ltmp35:
	.cfi_offset %r14, -24
.Ltmp36:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %r12
	movq	8(%r15), %r14
	movq	24(%r15), %r13
	movq	16(%r15), %rax
	subq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$3, %rcx
	movq	%r13, %rax
	sarq	%cl, %rax
	addq	%r13, %rsi
	leaq	100(%rax,%rsi), %rax
	cmpq	%r12, %rax
	jl	.LBB3_2
# BB#1:                                 # %entry
	movq	%rax, %r12
.LBB3_2:                                # %entry
	movzbl	80(%r15), %ecx
	movq	56(%r15), %rax
	testq	$1, %rcx
	je	.LBB3_4
# BB#3:                                 # %cond.true
	movq	72(%r15), %rdi
	movq	%r12, %rsi
	callq	*%rax
	jmp	.LBB3_5
.LBB3_4:                                # %cond.false
	movq	%r12, %rdi
	callq	*%rax
.LBB3_5:                                # %cond.end
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB3_7
# BB#6:                                 # %if.then8
	callq	*obstack_alloc_failed_handler(%rip)
.LBB3_7:                                # %if.end9
	movq	%rbx, 8(%r15)
	movq	%r14, 8(%rbx)
	addq	%rbx, %r12
	movq	%r12, 32(%r15)
	movq	%r12, (%rbx)
	movslq	48(%r15), %rcx
	xorq	%rax, %rax
	cmpq	$7, %rcx
	jl	.LBB3_12
# BB#8:                                 # %if.then15
	movq	$2, %rcx
	movq	%r13, %rax
	shrq	%cl, %rax
	movq	%rax, %rcx
	decq	%rcx
	js	.LBB3_11
# BB#9:                                 # %for.body.lr.ph
	movq	16(%r15), %rdx
	.align	16, 0x90
.LBB3_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rdx,%rcx,4), %esi
	movl	%esi, 16(%rbx,%rcx,4)
	decq	%rcx
	jns	.LBB3_10
.LBB3_11:                               # %for.end
	movq	$2, %rcx
	shlq	%cl, %rax
.LBB3_12:                               # %for.cond22.preheader
	movq	16(%r15), %rcx
	cmpq	%r13, %rax
	jge	.LBB3_14
	.align	16, 0x90
.LBB3_13:                               # %for.body25
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rcx,%rax), %ecx
	movb	%cl, 16(%rbx,%rax)
	movq	16(%r15), %rcx
	incq	%rax
	cmpq	%rax, %r13
	jne	.LBB3_13
.LBB3_14:                               # %for.end31
	leaq	16(%r14), %rax
	cmpq	%rax, %rcx
	jne	.LBB3_19
# BB#15:                                # %land.lhs.true
	movzbl	80(%r15), %eax
	testq	$2, %rax
	jne	.LBB3_19
# BB#16:                                # %if.then41
	movq	8(%r14), %rax
	movq	%rax, 8(%rbx)
	movzbl	80(%r15), %ecx
	movq	64(%r15), %rax
	testq	$1, %rcx
	je	.LBB3_18
# BB#17:                                # %if.then49
	movq	72(%r15), %rdi
	movq	%r14, %rsi
	callq	*%rax
	jmp	.LBB3_19
.LBB3_18:                               # %if.else51
	movq	%r14, %rdi
	callq	*%rax
.LBB3_19:                               # %if.end54
	leaq	16(%rbx), %rax
	movq	%rax, 16(%r15)
	leaq	16(%rbx,%r13), %rax
	movq	%rax, 24(%r15)
	movzbl	80(%r15), %eax
	andq	$253, %rax
	movb	%al, 80(%r15)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp37:
	.size	_obstack_newchunk, .Ltmp37-_obstack_newchunk
	.cfi_endproc

	.globl	_obstack_allocated_p
	.align	16, 0x90
	.type	_obstack_allocated_p,@function
_obstack_allocated_p:                   # @_obstack_allocated_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rcx
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_4:                                # %while.body
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rcx), %rcx
.LBB4_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB4_5
# BB#2:                                 # %land.rhs
                                        #   in Loop: Header=BB4_1 Depth=1
	cmpq	%rsi, %rcx
	jae	.LBB4_4
# BB#3:                                 # %lor.rhs
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	(%rcx), %rax
	cmpq	%rsi, %rax
	movq	$1, %rax
	jb	.LBB4_4
	jmp	.LBB4_6
.LBB4_5:
	xorq	%rax, %rax
.LBB4_6:                                # %while.end
	retq
.Ltmp38:
	.size	_obstack_allocated_p, .Ltmp38-_obstack_allocated_p
	.cfi_endproc

	.globl	_obstack_free
	.align	16, 0x90
	.type	_obstack_free,@function
_obstack_free:                          # @_obstack_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 32
.Ltmp46:
	.cfi_offset %rbx, -32
.Ltmp47:
	.cfi_offset %r14, -24
.Ltmp48:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB5_7
	.align	16, 0x90
.LBB5_1:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %rax
	jae	.LBB5_3
# BB#2:                                 # %lor.rhs
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%rax), %rcx
	cmpq	%r14, %rcx
	jae	.LBB5_9
.LBB5_3:                                # %while.body
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	8(%rax), %r15
	movzbl	80(%rbx), %edx
	movq	64(%rbx), %rcx
	testq	$1, %rdx
	jne	.LBB5_4
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rax, %rdi
	callq	*%rcx
	jmp	.LBB5_6
	.align	16, 0x90
.LBB5_4:                                # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	72(%rbx), %rdi
	movq	%rax, %rsi
	callq	*%rcx
.LBB5_6:                                # %do.end
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	80(%rbx), %eax
	orq	$2, %rax
	movb	%al, 80(%rbx)
	testq	%r15, %r15
	movq	%r15, %rax
	jne	.LBB5_1
.LBB5_7:                                # %if.else10
	testq	%r14, %r14
	je	.LBB5_10
# BB#8:                                 # %if.then12
	callq	abort
.LBB5_9:                                # %if.then7
	movq	%r14, 24(%rbx)
	movq	%r14, 16(%rbx)
	movq	%rcx, 32(%rbx)
	movq	%rax, 8(%rbx)
.LBB5_10:                               # %if.end14
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp49:
	.size	_obstack_free, .Ltmp49-_obstack_free
	.cfi_endproc

	.globl	obstack_free
	.align	16, 0x90
	.type	obstack_free,@function
obstack_free:                           # @obstack_free
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp56:
	.cfi_def_cfa_offset 32
.Ltmp57:
	.cfi_offset %rbx, -32
.Ltmp58:
	.cfi_offset %r14, -24
.Ltmp59:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB6_7
	.align	16, 0x90
.LBB6_1:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %rax
	jae	.LBB6_3
# BB#2:                                 # %lor.rhs
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	(%rax), %rcx
	cmpq	%r14, %rcx
	jae	.LBB6_9
.LBB6_3:                                # %while.body
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	8(%rax), %r15
	movzbl	80(%rbx), %edx
	movq	64(%rbx), %rcx
	testq	$1, %rdx
	jne	.LBB6_4
# BB#5:                                 # %if.else
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	%rax, %rdi
	callq	*%rcx
	jmp	.LBB6_6
	.align	16, 0x90
.LBB6_4:                                # %if.then
                                        #   in Loop: Header=BB6_1 Depth=1
	movq	72(%rbx), %rdi
	movq	%rax, %rsi
	callq	*%rcx
.LBB6_6:                                # %do.end
                                        #   in Loop: Header=BB6_1 Depth=1
	movzbl	80(%rbx), %eax
	orq	$2, %rax
	movb	%al, 80(%rbx)
	testq	%r15, %r15
	movq	%r15, %rax
	jne	.LBB6_1
.LBB6_7:                                # %if.else10
	testq	%r14, %r14
	je	.LBB6_10
# BB#8:                                 # %if.then12
	callq	abort
.LBB6_9:                                # %if.then7
	movq	%r14, 24(%rbx)
	movq	%r14, 16(%rbx)
	movq	%rcx, 32(%rbx)
	movq	%rax, 8(%rbx)
.LBB6_10:                               # %if.end14
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp60:
	.size	obstack_free, .Ltmp60-obstack_free
	.cfi_endproc

	.globl	_obstack_memory_used
	.align	16, 0x90
	.type	_obstack_memory_used,@function
_obstack_memory_used:                   # @_obstack_memory_used
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB7_3
# BB#1:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	andq	%rdx, %rax
	movq	%rax, %rsi
	subq	%rcx, %rsi
	movq	(%rcx), %rax
	movq	8(%rcx), %rcx
	addq	%rsi, %rax
	testq	%rcx, %rcx
	jne	.LBB7_2
.LBB7_3:                                # %for.end
	retq
.Ltmp61:
	.size	_obstack_memory_used, .Ltmp61-_obstack_memory_used
	.cfi_endproc

	.type	obstack_alloc_failed_handler,@object # @obstack_alloc_failed_handler
	.data
	.globl	obstack_alloc_failed_handler
	.align	8
obstack_alloc_failed_handler:
	.quad	print_and_abort
	.size	obstack_alloc_failed_handler, 8

	.type	obstack_exit_failure,@object # @obstack_exit_failure
	.globl	obstack_exit_failure
	.align	4
obstack_exit_failure:
	.long	1                       # 0x1
	.size	obstack_exit_failure, 4

	.type	_obstack,@object        # @_obstack
	.comm	_obstack,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"memory exhausted\n"
	.size	.L.str, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
