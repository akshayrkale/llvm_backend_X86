	.file	"cpplib.c.bc"
	.text
	.globl	_cpp_handle_directive
	.align	16, 0x90
	.type	_cpp_handle_directive,@function
_cpp_handle_directive:                  # @_cpp_handle_directive
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	$1, %rax
	movb	%al, 8(%r12)
	xorq	%rax, %rax
	movb	%al, 11(%r12)
	movl	64(%r12), %eax
	movq	%rsi, %r14
	movl	%eax, 68(%r12)
                                        # kill: RDI<def> R12<kill>
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB0_37
# BB#1:                                 # %entry
	cmpq	$57, %rcx
	jne	.LBB0_2
# BB#8:                                 # %land.lhs.true
	movl	736(%r12), %ecx
	cmpq	$9, %rcx
	je	.LBB0_5
# BB#9:                                 # %if.then12
	movzbl	770(%r12), %eax
	testq	%rax, %rax
	movabsq	$linemarker_dir, %r15
	je	.LBB0_13
# BB#10:                                # %land.lhs.true16
	movzbl	771(%r12), %eax
	testq	%rax, %rax
	jne	.LBB0_13
# BB#11:                                # %land.lhs.true19
	movzbl	9(%r12), %eax
	testq	%rax, %rax
	jne	.LBB0_13
# BB#12:                                # %if.then21
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
	jmp	.LBB0_13
.LBB0_2:                                # %entry
	cmpq	$56, %rcx
	jne	.LBB0_5
# BB#3:                                 # %if.then
	movq	8(%rax), %rcx
	movzbl	18(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_5
# BB#4:                                 # %if.end24
	leaq	(%rcx,%rcx,2), %rcx
	leaq	dtable(,%rcx,8), %r15
	addq	$-24, %r15
	je	.LBB0_5
.LBB0_13:                               # %if.then26
	movzbl	19(%r15), %eax
	testq	$2, %rax
	jne	.LBB0_15
# BB#14:                                # %if.then29
	xorq	%rcx, %rcx
	movb	%cl, 184(%r12)
.LBB0_15:                               # %if.end30
	movzbl	771(%r12), %ecx
	testq	%rcx, %rcx
	je	.LBB0_16
# BB#17:                                # %land.lhs.true35
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %r14
	jne	.LBB0_49
# BB#18:                                # %land.lhs.true35
	movq	%rax, %rdx
	andq	$8, %rdx
	testq	$255, %rdx
	je	.LBB0_49
# BB#19:                                # %if.else42
	andq	$4, %rax
	testq	%rcx, %rcx
	movb	%al, 10(%r12)
	jne	.LBB0_33
	jmp	.LBB0_20
.LBB0_5:                                # %if.else72
	movl	736(%r12), %ecx
	cmpq	$9, %rcx
	jne	.LBB0_6
.LBB0_49:                               # %if.end96
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	_cpp_backup_tokens
	xorq	%rax, %rax
	jmp	.LBB0_45
.LBB0_6:                                # %if.else78
	movzbl	9(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_37
# BB#7:                                 # %if.then82
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB0_37
.LBB0_16:                               # %if.else42.thread
	andq	$4, %rax
	movb	%al, 10(%r12)
.LBB0_20:                               # %if.then51
	movzbl	770(%r12), %eax
	testq	%rax, %rax
	je	.LBB0_24
# BB#21:                                # %land.lhs.true.i
	movzbl	9(%r12), %eax
	testq	%rax, %rax
	jne	.LBB0_24
# BB#22:                                # %land.lhs.true2.i
	movzbl	18(%r15), %eax
	cmpq	$2, %rax
	jne	.LBB0_24
# BB#23:                                # %if.then.i136
	movq	8(%r15), %rdx
	movabsq	$.L.str30, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
.LBB0_24:                               # %if.end.i
	movzbl	761(%r12), %eax
	testq	%rax, %rax
	je	.LBB0_33
# BB#25:                                # %if.then7.i
	movabsq	$dtable+216, %rax
	cmpq	%rax, %r15
	je	.LBB0_26
# BB#27:                                # %if.else.i
	movzbl	18(%r15), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB0_30
# BB#28:                                # %land.lhs.true12.i
	testq	$255, %rax
	jne	.LBB0_33
# BB#29:                                # %if.then17.i
	movq	8(%r15), %rdx
	movabsq	$.L.str32, %rsi
	jmp	.LBB0_32
.LBB0_26:                               # %if.then10.i
	movabsq	$.L.str31, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_warning
	jmp	.LBB0_33
.LBB0_30:                               # %land.lhs.true21.i
	testq	$255, %rax
	je	.LBB0_33
# BB#31:                                # %if.then26.i
	movq	8(%r15), %rdx
	movabsq	$.L.str33, %rsi
.LBB0_32:                               # %if.end52
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_warning
.LBB0_33:                               # %if.end52
	movzbl	9(%r12), %eax
	testq	%rax, %rax
	je	.LBB0_35
# BB#34:                                # %land.lhs.true57
	movzbl	19(%r15), %eax
	testq	$1, %rax
	jne	.LBB0_36
	jmp	.LBB0_37
.LBB0_35:                               # %if.end87
	testq	%r15, %r15
	je	.LBB0_37
.LBB0_36:                               # %if.then89
	movq	%r15, 160(%r12)
	movq	%r12, %rdi
	callq	*(%r15)
.LBB0_37:                               # %if.then.i
	movq	152(%r12), %rax
	leaq	96(%r12), %rbx
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_38:                               # %while.body.i.i
                                        #   in Loop: Header=BB0_39 Depth=1
	movq	%r12, %rdi
	callq	_cpp_pop_context
	movq	152(%r12), %rax
.LBB0_39:                               # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rax
	jne	.LBB0_38
# BB#40:                                # %while.end.i.i
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB0_42
	.align	16, 0x90
.LBB0_41:                               # %while.cond2.i.i
                                        #   in Loop: Header=BB0_42 Depth=1
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB0_42:                               # %while.cond2.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB0_41
# BB#43:                                # %skip_rest_of_line.exit.i
	movl	244(%r12), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB0_45
# BB#44:                                # %if.then2.i
	leaq	200(%r12), %rcx
	movq	%rcx, 232(%r12)
	movq	216(%r12), %rcx
	movq	%rcx, 192(%r12)
.LBB0_45:                               # %end_directive.exit
	movzbl	745(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_46
# BB#47:                                # %end_directive.exit
	movabsq	$1, %rcx
	jmp	.LBB0_48
.LBB0_46:
	movabsq	$0, %rcx
.LBB0_48:                               # %end_directive.exit
	movb	%cl, 11(%r12)
	xorq	%rcx, %rcx
	movb	%cl, 8(%r12)
	movb	%cl, 10(%r12)
	movq	$0, 160(%r12)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	_cpp_handle_directive, .Ltmp14-_cpp_handle_directive
	.cfi_endproc

	.globl	_cpp_do_file_change
	.align	16, 0x90
	.type	_cpp_do_file_change,@function
_cpp_do_file_change:                    # @_cpp_do_file_change
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 16
.Ltmp18:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	64(%rbx), %r9d
	movq	%rcx, %r10
	movq	%rdx, %rax
	leaq	24(%rbx), %rdi
	movq	%r8, %rdx
	movq	%r9, %rcx
	movq	%rax, %r8
	movq	%r10, %r9
	callq	add_line_map
	movq	%rax, 56(%rbx)
	movq	592(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_1
# BB#2:                                 # %if.then
	movq	%rbx, %rdi
	movq	%rax, %rsi
	popq	%rbx
	jmpq	*%rcx  # TAILCALL
.LBB1_1:                                # %if.end
	popq	%rbx
	retq
.Ltmp19:
	.size	_cpp_do_file_change, .Ltmp19-_cpp_do_file_change
	.cfi_endproc

	.globl	cpp_register_pragma
	.align	16, 0x90
	.type	cpp_register_pragma,@function
cpp_register_pragma:                    # @cpp_register_pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp26:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp28:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp29:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp30:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp31:
	.cfi_def_cfa_offset 64
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
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r13
	testq	%rcx, %rcx
	je	.LBB2_20
# BB#1:                                 # %if.end
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	testq	%r15, %r15
	je	.LBB2_2
# BB#3:                                 # %if.then3
	movq	%r15, %rdi
	callq	strlen
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	cpp_lookup
	movq	576(%r13), %rbx
	movq	%rax, %r12
	jmp	.LBB2_4
	.align	16, 0x90
.LBB2_6:                                # %while.body.i
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	(%rbx), %rbx
.LBB2_4:                                # %if.then3
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB2_7
# BB#5:                                 # %land.rhs.i
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	8(%rbx), %rax
	cmpq	%r12, %rax
	jne	.LBB2_6
# BB#8:                                 # %if.else
	movl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB2_9
	jmp	.LBB2_16
.LBB2_7:                                # %if.then7
	movq	$32, %rsi
	movq	%r13, %rdi
	callq	_cpp_aligned_alloc
	movq	%rax, %rbx
	movq	%r12, 8(%rbx)
	movq	$1, %rax
	movl	%eax, 16(%rbx)
	movq	$0, 24(%rbx)
	movq	576(%r13), %rax
	movq	%rax, (%rbx)
	movq	%rbx, 576(%r13)
.LBB2_9:                                # %if.end12
	addq	$24, %rbx
	jmp	.LBB2_10
.LBB2_2:
	leaq	576(%r13), %rbx
.LBB2_10:                               # %if.end14
	movq	%r14, %rdi
	callq	strlen
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	cpp_lookup
	movq	(%rbx), %rcx
	movq	%rax, %r12
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_13:                               # %while.body.i75
                                        #   in Loop: Header=BB2_11 Depth=1
	movq	(%rcx), %rcx
.LBB2_11:                               # %if.end14
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB2_14
# BB#12:                                # %land.rhs.i72
                                        #   in Loop: Header=BB2_11 Depth=1
	movq	8(%rcx), %rax
	cmpq	%r12, %rax
	jne	.LBB2_13
# BB#15:                                # %if.then20
	movl	16(%rcx), %eax
	testq	%rax, %rax
	je	.LBB2_17
.LBB2_16:                               # %clash
	movq	8(%r12), %rdx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
.LBB2_19:                               # %if.else27
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_ice  # TAILCALL
.LBB2_14:                               # %if.end32
	movq	$32, %rsi
	movq	%r13, %rdi
	callq	_cpp_aligned_alloc
	movq	%r12, 8(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 24(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	%rax, (%rbx)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_17:                               # %if.else24
	testq	%r15, %r15
	je	.LBB2_18
# BB#21:                                # %if.then26
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r15, %rdx
	movq	%r14, %rcx
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_ice  # TAILCALL
.LBB2_18:                               # %if.else27
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r14, %rdx
	jmp	.LBB2_19
.LBB2_20:                               # %if.then
	callq	abort
.Ltmp37:
	.size	cpp_register_pragma, .Ltmp37-cpp_register_pragma
	.cfi_endproc

	.globl	_cpp_init_internal_pragmas
	.align	16, 0x90
	.type	_cpp_init_internal_pragmas,@function
_cpp_init_internal_pragmas:             # @_cpp_init_internal_pragmas
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp47:
	.cfi_def_cfa_offset 48
.Ltmp48:
	.cfi_offset %rbx, -40
.Ltmp49:
	.cfi_offset %r12, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$.L.str5, %r14
	movabsq	$do_pragma_poison, %r15
	xorq	%rsi, %rsi
                                        # kill: RDI<def> RBX<kill>
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	cpp_register_pragma
	movabsq	$.L.str6, %rdx
	movabsq	$do_pragma_once, %rcx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	cpp_register_pragma
	movabsq	$.L.str7, %r12
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	callq	cpp_register_pragma
	movabsq	$.L.str8, %rdx
	movabsq	$do_pragma_system_header, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	cpp_register_pragma
	movabsq	$.L.str9, %rdx
	movabsq	$do_pragma_dependency, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	cpp_register_pragma  # TAILCALL
.Ltmp52:
	.size	_cpp_init_internal_pragmas, .Ltmp52-_cpp_init_internal_pragmas
	.cfi_endproc

	.align	16, 0x90
	.type	do_pragma_poison,@function
do_pragma_poison:                       # @do_pragma_poison
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp59:
	.cfi_def_cfa_offset 32
.Ltmp60:
	.cfi_offset %rbx, -32
.Ltmp61:
	.cfi_offset %r14, -24
.Ltmp62:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	$1, %rax
	movb	%al, 14(%r15)
	movabsq	$.L.str27, %r14
	jmp	.LBB4_1
	.align	16, 0x90
.LBB4_8:                                # %if.end15
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	%rbx, %rdi
	callq	_cpp_free_definition
	movzbl	21(%rbx), %eax
	orq	$10, %rax
	movb	%al, 21(%rbx)
.LBB4_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	cmpq	$56, %rcx
	jne	.LBB4_2
# BB#5:                                 # %if.end6
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rax), %rbx
	movzbl	21(%rbx), %eax
	testq	$2, %rax
	jne	.LBB4_1
# BB#6:                                 # %if.end8
                                        #   in Loop: Header=BB4_1 Depth=1
	movzbl	20(%rbx), %eax
	cmpq	$1, %rax
	jne	.LBB4_8
# BB#7:                                 # %if.then14
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	8(%rbx), %rdx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	cpp_warning
	jmp	.LBB4_8
.LBB4_2:                                # %for.cond
	cmpq	$67, %rcx
	je	.LBB4_4
# BB#3:                                 # %if.then5
	movabsq	$.L.str26, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	cpp_error
.LBB4_4:                                # %for.end
	xorq	%rax, %rax
	movb	%al, 14(%r15)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp63:
	.size	do_pragma_poison, .Ltmp63-do_pragma_poison
	.cfi_endproc

	.align	16, 0x90
	.type	do_pragma_once,@function
do_pragma_once:                         # @do_pragma_once
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 16
.Ltmp67:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$.L.str28, %rsi
	xorq	%rax, %rax
                                        # kill: RDI<def> RBX<kill>
	callq	cpp_warning
	movq	(%rbx), %rax
	cmpq	$0, 32(%rax)
	je	.LBB5_1
# BB#2:                                 # %if.else
	movq	48(%rax), %rdi
	callq	_cpp_never_reread
	jmp	.LBB5_3
.LBB5_1:                                # %if.then
	movabsq	$.L.str29, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_warning
.LBB5_3:                                # %if.end
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB5_5
# BB#4:                                 # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB5_6
.LBB5_5:                                # %check_eol.exit
	popq	%rbx
	retq
.LBB5_6:                                # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	popq	%rbx
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp68:
	.size	do_pragma_once, .Ltmp68-do_pragma_once
	.cfi_endproc

	.align	16, 0x90
	.type	do_pragma_system_header,@function
do_pragma_system_header:                # @do_pragma_system_header
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp74:
	.cfi_def_cfa_offset 32
.Ltmp75:
	.cfi_offset %rbx, -24
.Ltmp76:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	cmpq	$0, 32(%rax)
	je	.LBB6_11
# BB#1:                                 # %if.else
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB6_4
# BB#2:                                 # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB6_4
# BB#3:                                 # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_pedwarn
.LBB6_4:                                # %check_eol.exit
	movq	152(%rbx), %rax
	leaq	96(%rbx), %r14
	jmp	.LBB6_6
	.align	16, 0x90
.LBB6_5:                                # %while.body.i
                                        #   in Loop: Header=BB6_6 Depth=1
	movq	%rbx, %rdi
	callq	_cpp_pop_context
	movq	152(%rbx), %rax
.LBB6_6:                                # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r14, %rax
	jne	.LBB6_5
# BB#7:                                 # %while.end.i
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB6_9
	.align	16, 0x90
.LBB6_8:                                # %while.cond2.i
                                        #   in Loop: Header=BB6_9 Depth=1
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB6_9:                                # %while.cond2.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB6_8
# BB#10:                                # %skip_rest_of_line.exit
	movq	$1, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_make_system_header  # TAILCALL
.LBB6_11:                               # %if.then
	movabsq	$.L.str24, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_warning  # TAILCALL
.Ltmp77:
	.size	do_pragma_system_header, .Ltmp77-do_pragma_system_header
	.cfi_endproc

	.align	16, 0x90
	.type	do_pragma_dependency,@function
do_pragma_dependency:                   # @do_pragma_dependency
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp83:
	.cfi_def_cfa_offset 32
.Ltmp84:
	.cfi_offset %rbx, -24
.Ltmp85:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	parse_include
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_6
# BB#1:                                 # %if.end
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	_cpp_compare_file_date
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB7_7
# BB#2:                                 # %if.else
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB7_6
# BB#3:                                 # %if.then5
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str18, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	cpp_warning
	movq	%r14, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB7_6
# BB#4:                                 # %if.then9
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	_cpp_backup_tokens
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_6
# BB#5:                                 # %if.then.i
	movzbl	15(%r14), %eax
	movq	stderr(%rip), %rsi
	incq	%rax
	movb	%al, 15(%r14)
	movq	%r14, %rdi
	callq	cpp_output_line
	movzbl	15(%r14), %eax
	decq	%rax
	movb	%al, 15(%r14)
.LBB7_6:                                # %if.end12
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB7_7:                                # %if.then2
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_warning  # TAILCALL
.Ltmp86:
	.size	do_pragma_dependency, .Ltmp86-do_pragma_dependency
	.cfi_endproc

	.globl	_cpp_do__Pragma
	.align	16, 0x90
	.type	_cpp_do__Pragma,@function
_cpp_do__Pragma:                        # @_cpp_do__Pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp93:
	.cfi_def_cfa_offset 32
.Ltmp94:
	.cfi_offset %rbx, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	.align	16, 0x90
.LBB8_1:                                # %for.cond.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$66, %rax
	je	.LBB8_1
# BB#2:                                 # %for.cond.i.i
	cmpq	$22, %rax
	jne	.LBB8_9
	.align	16, 0x90
.LBB8_3:                                # %for.cond.i27.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	callq	cpp_get_token
	movq	%rax, %rbx
	movzbl	6(%rbx), %eax
	cmpq	$66, %rax
	je	.LBB8_3
# BB#4:                                 # %get_token_no_padding.exit28.i
	addq	$-61, %rax
	andq	$255, %rax
	cmpq	$1, %rax
	ja	.LBB8_9
	.align	16, 0x90
.LBB8_5:                                # %for.cond.i33.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$66, %rax
	je	.LBB8_5
# BB#6:                                 # %for.cond.i33.i
	cmpq	$23, %rax
	jne	.LBB8_9
# BB#7:                                 # %get__Pragma_string.exit
	testq	%rbx, %rbx
	je	.LBB8_9
# BB#8:                                 # %if.end
	movl	64(%r14), %r15d
	movq	16(%rbx), %rdx
	movl	8(%rbx), %esi
	movq	%r14, %rdi
	callq	destringize_and_run
	movl	%r15d, 64(%r14)
	movq	(%r14), %rax
	movq	$64, %rcx
	movb	%cl, 68(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB8_9:                                # %if.then
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	cpp_error  # TAILCALL
.Ltmp97:
	.size	_cpp_do__Pragma, .Ltmp97-_cpp_do__Pragma
	.cfi_endproc

	.align	16, 0x90
	.type	destringize_and_run,@function
destringize_and_run:                    # @destringize_and_run
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp100:
	.cfi_def_cfa_offset 16
.Ltmp101:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp102:
	.cfi_def_cfa_register %rbp
	leaq	1(%rsi), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %r8
	subq	%rax, %r8
	movq	%r8, %rsp
	movq	%rsi, %rax
	andq	%rcx, %rax
	andq	%rcx, %rsi
	movq	%r8, %rcx
	je	.LBB9_8
# BB#1:
	addq	%rdx, %rax
	movq	%r8, %rcx
	.align	16, 0x90
.LBB9_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %esi
	cmpq	$92, %rsi
	jne	.LBB9_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_2 Depth=1
	movzbl	1(%rdx), %esi
	cmpq	$34, %rsi
	je	.LBB9_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_2 Depth=1
	cmpq	$92, %rsi
	jne	.LBB9_5
.LBB9_6:                                # %if.then
                                        #   in Loop: Header=BB9_2 Depth=1
	incq	%rdx
	jmp	.LBB9_7
.LBB9_5:                                #   in Loop: Header=BB9_2 Depth=1
	movq	$92, %rsi
	.align	16, 0x90
.LBB9_7:                                # %if.end
                                        #   in Loop: Header=BB9_2 Depth=1
	incq	%rdx
	cmpq	%rax, %rdx
	movb	%sil, (%rcx)
	leaq	1(%rcx), %rcx
	jb	.LBB9_2
.LBB9_8:                                # %for.end
	xorq	%rax, %rax
	movb	%al, (%rcx)
	subq	%r8, %rcx
	movq	$11, %rsi
	movq	%r8, %rdx
	callq	run_directive
	movq	%rbp, %rsp
	popq	%rbp
	retq
.Ltmp103:
	.size	destringize_and_run, .Ltmp103-destringize_and_run
	.cfi_endproc

	.globl	_cpp_test_assertion
	.align	16, 0x90
	.type	_cpp_test_assertion,@function
_cpp_test_assertion:                    # @_cpp_test_assertion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp112:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp113:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp114:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp115:
	.cfi_def_cfa_offset 80
.Ltmp116:
	.cfi_offset %rbx, -48
.Ltmp117:
	.cfi_offset %r12, -40
.Ltmp118:
	.cfi_offset %r13, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	leaq	24(%rsp), %rsi
	movq	$4, %rdx
	callq	parse_assertion
	testq	%rax, %rax
	je	.LBB10_18
# BB#1:                                 # %if.then
	movzbl	20(%rax), %ecx
	xorq	%rdx, %rdx
	cmpq	$2, %rcx
	jne	.LBB10_17
# BB#2:                                 # %land.rhs
	movq	24(%rsp), %r12
	testq	%r12, %r12
	movq	$1, %rdx
	je	.LBB10_17
# BB#3:                                 # %lor.rhs
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB10_4
# BB#5:                                 # %for.body.lr.ph.i
	movq	%rax, 8(%rsp)           # 8-byte Spill
	leaq	24(%rax), %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_6:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB10_9 Depth 2
	movl	8(%rbx), %eax
	movl	8(%r12), %ecx
	cmpq	%rcx, %rax
	jne	.LBB10_11
# BB#7:                                 # %for.cond2.preheader.i
                                        #   in Loop: Header=BB10_6 Depth=1
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB10_13
	.align	16, 0x90
.LBB10_9:                               # %for.body5.i
                                        #   Parent Loop BB10_6 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	andq	%r13, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	leaq	16(%rbx,%rax), %rdi
	leaq	16(%r12,%rax), %rsi
	callq	_cpp_equiv_tokens
	movl	8(%rbx), %ecx
	testq	%r13, %rax
	je	.LBB10_10
# BB#8:                                 # %for.cond2.i
                                        #   in Loop: Header=BB10_9 Depth=2
	incq	%r15
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	%r15, %rdx
	andq	%r13, %rdx
	cmpq	%rax, %rdx
	jb	.LBB10_9
.LBB10_10:                              # %for.end.i
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	(%r14), %rbx
	andq	%r13, %rcx
	andq	%r13, %r15
	cmpq	%rcx, %r15
	je	.LBB10_13
.LBB10_11:                              # %for.inc16.i
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%rbx, %r14
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_6
	jmp	.LBB10_12
.LBB10_4:
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB10_12:
	xorq	%rbx, %rbx
.LBB10_13:                              # %find_answer.exit
	testq	%rbx, %rbx
	je	.LBB10_14
# BB#15:                                # %find_answer.exit
	movabsq	$1, %rdx
	jmp	.LBB10_16
.LBB10_14:
	movabsq	$0, %rdx
.LBB10_16:                              # %find_answer.exit
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB10_17:                              # %land.end
	movl	%edx, (%rbx)
.LBB10_18:                              # %if.end
	testq	%rax, %rax
	jne	.LBB10_19
# BB#20:                                # %if.end
	movabsq	$1, %rax
	jmp	.LBB10_21
.LBB10_19:
	movabsq	$0, %rax
.LBB10_21:                              # %if.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	_cpp_test_assertion, .Ltmp121-_cpp_test_assertion
	.cfi_endproc

	.align	16, 0x90
	.type	parse_assertion,@function
parse_assertion:                        # @parse_assertion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp125:
	.cfi_def_cfa_offset 16
.Ltmp126:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp127:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp128:
	.cfi_offset %rbx, -56
.Ltmp129:
	.cfi_offset %r12, -48
.Ltmp130:
	.cfi_offset %r13, -40
.Ltmp131:
	.cfi_offset %r14, -32
.Ltmp132:
	.cfi_offset %r15, -24
	movq	%rdi, %r13
	movzbl	15(%r13), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r15
	incq	%rax
	movb	%al, 15(%r13)
	movq	$0, (%r15)
                                        # kill: RDI<def> R13<kill>
	callq	cpp_get_token
	movq	%rax, %r14
	movzbl	6(%r14), %eax
	cmpq	$56, %rax
	jne	.LBB11_1
# BB#5:                                 # %if.else7
	movq	%r13, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$22, %rax
	jne	.LBB11_10
# BB#6:                                 # %for.cond.preheader.i
	movq	%r14, -56(%rbp)         # 8-byte Spill
	leaq	72(%r13), %r14
	movq	%r14, -48(%rbp)         # 8-byte Spill
	xorq	%r12, %r12
	jmp	.LBB11_7
.LBB11_1:                               # %entry
	cmpq	$67, %rax
	jne	.LBB11_4
# BB#2:                                 # %if.then
	movabsq	$.L.str12, %rsi
	jmp	.LBB11_3
.LBB11_10:                              # %if.then.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$17, %rcx
	jne	.LBB11_11
# BB#13:                                # %land.lhs.true.i
	cmpq	$67, %rax
	je	.LBB11_23
	jmp	.LBB11_14
.LBB11_4:                               # %if.then6
	movabsq	$.L.str13, %rsi
	jmp	.LBB11_3
	.align	16, 0x90
.LBB11_19:                              # %for.inc.i
                                        #   in Loop: Header=BB11_7 Depth=1
	incq	%r12
	movq	-48(%rbp), %r14         # 8-byte Reload
.LBB11_7:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r13, %rdi
	callq	cpp_get_token
	movq	%rax, %rbx
	movzbl	6(%rbx), %eax
	cmpq	$23, %rax
	je	.LBB11_20
# BB#8:                                 # %for.cond.i
                                        #   in Loop: Header=BB11_7 Depth=1
	cmpq	$67, %rax
	je	.LBB11_9
# BB#15:                                # %if.end24.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movq	(%r14), %rcx
	movq	%r12, %rdx
	movq	16(%rcx), %rax
	movq	24(%rcx), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	leaq	(%rdx,%rdx,2), %r14
	leaq	40(,%r14,8), %rdx
	subq	%rax, %rcx
	cmpq	%rdx, %rcx
	jae	.LBB11_17
# BB#16:                                # %if.then28.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movq	$40, %rdx
	movq	%r13, %rdi
	movq	-48(%rbp), %rsi         # 8-byte Reload
	callq	_cpp_extend_buff
	movq	72(%r13), %rax
	movq	16(%rax), %rax
.LBB11_17:                              # %if.end30.i
                                        #   in Loop: Header=BB11_7 Depth=1
	movq	16(%rbx), %rcx
	movq	%rcx, 32(%rax,%r14,8)
	movq	(%rbx), %rcx
	movq	8(%rbx), %rdx
	movq	%rdx, 24(%rax,%r14,8)
	movq	%rcx, 16(%rax,%r14,8)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r12
	jne	.LBB11_19
# BB#18:                                # %if.then35.i
                                        #   in Loop: Header=BB11_7 Depth=1
	addq	$16, %rax
	movzbl	7(%rax,%r14,8), %ecx
	andq	$254, %rcx
	movb	%cl, 7(%rax,%r14,8)
	jmp	.LBB11_19
.LBB11_20:                              # %for.end.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	je	.LBB11_21
# BB#22:                                # %if.end42.i
	movq	(%r14), %rax
	movq	16(%rax), %rax
	movl	%r12d, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%r15)
	movq	-56(%rbp), %r14         # 8-byte Reload
	jmp	.LBB11_23
.LBB11_11:                              # %if.then.i
	cmpq	$4, %rcx
	jne	.LBB11_14
# BB#12:                                # %if.then3.i
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	_cpp_backup_tokens
.LBB11_23:                              # %if.then10
	movq	8(%r14), %rax
	movl	(%rax), %edx
	leaq	1(%rdx), %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %rcx
	movq	%rcx, %rbx
	subq	%rax, %rbx
	negq	%rax
	movq	%rbx, %rsp
	movq	$35, %rsi
	movb	%sil, (%rcx,%rax)
	movq	8(%r14), %rax
	movq	8(%rax), %rsi
	leaq	1(%rbx), %rdi
	callq	memcpy
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	cpp_lookup
	movq	%rax, %rbx
	jmp	.LBB11_24
.LBB11_14:                              # %if.end10.i
	movabsq	$.L.str14, %rsi
	jmp	.LBB11_3
.LBB11_21:                              # %if.then41.i
	movabsq	$.L.str16, %rsi
.LBB11_3:                               # %if.end19
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	callq	cpp_error
.LBB11_24:                              # %if.end19
	movzbl	15(%r13), %eax
	decq	%rax
	movb	%al, 15(%r13)
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB11_9:                               # %if.then23.i
	movabsq	$.L.str15, %rsi
	jmp	.LBB11_3
.Ltmp133:
	.size	parse_assertion, .Ltmp133-parse_assertion
	.cfi_endproc

	.globl	cpp_define
	.align	16, 0x90
	.type	cpp_define,@function
cpp_define:                             # @cpp_define
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp137:
	.cfi_def_cfa_offset 16
.Ltmp138:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp139:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
.Ltmp140:
	.cfi_offset %rbx, -48
.Ltmp141:
	.cfi_offset %r12, -40
.Ltmp142:
	.cfi_offset %r14, -32
.Ltmp143:
	.cfi_offset %r15, -24
	movq	%rsi, %r12
	movq	%rdi, %r14
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r15
	movq	%rsp, %rbx
	leaq	18(%r15), %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movq	$61, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	je	.LBB12_2
# BB#1:                                 # %if.then
	subq	%r12, %rax
	movq	$32, %rcx
	movb	%cl, (%rbx,%rax)
	jmp	.LBB12_3
.LBB12_2:                               # %if.else
	movq	$12576, %rax            # imm = 0x3120
	movw	%ax, (%rbx,%r15)
	leaq	2(%r15), %r15
.LBB12_3:                               # %if.end
	xorq	%rax, %rax
	movb	%al, (%rbx,%r15)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	run_directive
	leaq	-32(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp144:
	.size	cpp_define, .Ltmp144-cpp_define
	.cfi_endproc

	.align	16, 0x90
	.type	run_directive,@function
run_directive:                          # @run_directive
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp151:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp152:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp153:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp154:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 48
.Ltmp156:
	.cfi_offset %rbx, -48
.Ltmp157:
	.cfi_offset %r12, -40
.Ltmp158:
	.cfi_offset %r13, -32
.Ltmp159:
	.cfi_offset %r14, -24
.Ltmp160:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	512(%r12), %rax
	movq	520(%r12), %rdi
	movq	%rcx, %r15
	movq	%rdx, %rbx
	movq	%rsi, %r14
	subq	%rax, %rdi
	cmpq	$143, %rdi
	jg	.LBB13_2
# BB#1:                                 # %if.then.i
	leaq	488(%r12), %rdi
	movq	$144, %rsi
	callq	_obstack_newchunk
	movq	512(%r12), %rax
.LBB13_2:                               # %if.end.i
	addq	$144, %rax
	movq	%rax, 512(%r12)
	movq	504(%r12), %r13
	cmpq	%r13, %rax
	jne	.LBB13_4
# BB#3:                                 # %if.then8.i
	movzbl	568(%r12), %ecx
	orq	$2, %rcx
	movb	%cl, 568(%r12)
.LBB13_4:                               # %if.end9.i
	movslq	536(%r12), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, 512(%r12)
	movq	520(%r12), %rax
	movq	496(%r12), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB13_6
# BB#5:                                 # %if.then29.i
	movq	%rax, 512(%r12)
	movq	%rax, %rcx
.LBB13_6:                               # %cpp_push_buffer.exit
	movq	%rcx, 504(%r12)
	xorq	%rsi, %rsi
	movq	$144, %rdx
	movq	%r13, %rdi
	callq	memset
	movq	%rbx, (%r13)
	movq	%rbx, 40(%r13)
	movq	%rbx, 24(%r13)
	addq	%rbx, %r15
	movq	%r15, 16(%r13)
	movq	$1, %rax
	movb	%al, 81(%r13)
	movq	(%r12), %rcx
	movq	%rcx, 32(%r13)
	movb	%al, 83(%r13)
	movq	%r13, (%r12)
	movb	%al, 8(%r12)
	movabsq	$0, %rcx
	movb	%cl, 11(%r12)
	movl	64(%r12), %eax
	movl	%eax, 68(%r12)
	movb	%cl, 68(%r13)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%r14,%r14,2), %rax
	leaq	dtable(,%rax,8), %rcx
	movq	%rcx, 160(%r12)
	movq	%r12, %rdi
	callq	*dtable(,%rax,8)
	movq	152(%r12), %rax
	leaq	96(%r12), %rbx
	jmp	.LBB13_8
	.align	16, 0x90
.LBB13_7:                               # %while.body.i.i
                                        #   in Loop: Header=BB13_8 Depth=1
	movq	%r12, %rdi
	callq	_cpp_pop_context
	movq	152(%r12), %rax
.LBB13_8:                               # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rax
	jne	.LBB13_7
# BB#9:                                 # %while.end.i.i
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB13_11
	.align	16, 0x90
.LBB13_10:                              # %while.cond2.i.i
                                        #   in Loop: Header=BB13_11 Depth=1
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB13_11:                              # %while.cond2.i.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB13_10
# BB#12:                                # %skip_rest_of_line.exit.i
	movl	244(%r12), %eax
	testq	%rax, %rax
	jne	.LBB13_14
# BB#13:                                # %if.then2.i
	leaq	200(%r12), %rax
	movq	%rax, 232(%r12)
	movq	216(%r12), %rax
	movq	%rax, 192(%r12)
.LBB13_14:                              # %end_directive.exit
	movzbl	745(%r12), %eax
	testq	%rax, %rax
	jne	.LBB13_15
# BB#16:                                # %end_directive.exit
	movabsq	$1, %rax
	jmp	.LBB13_17
.LBB13_15:
	movabsq	$0, %rax
.LBB13_17:                              # %end_directive.exit
	movb	%al, 11(%r12)
	xorq	%r13, %r13
	movb	%r13b, 8(%r12)
	movb	%r13b, 10(%r12)
	movq	$0, 160(%r12)
	movq	(%r12), %r14
	movq	56(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB13_20
# BB#18:
	movabsq	$.L.str11, %r15
	.align	16, 0x90
.LBB13_19:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movslq	28(%rbx), %rax
	leaq	(%rax,%rax,2), %rax
	movq	dtable+8(,%rax,8), %r8
	movl	8(%rbx), %esi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rcx
	callq	cpp_error_with_line
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_19
.LBB13_20:                              # %for.end.i
	movb	%r13b, 9(%r12)
	movq	32(%r14), %rax
	movq	%rax, (%r12)
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB13_22
# BB#21:                                # %if.then.i11
	movq	%r12, %rdi
	callq	_cpp_pop_file_buffer
	testq	%rax, %rax
	jne	.LBB13_25
.LBB13_22:                              # %if.then6.i
	movq	496(%r12), %rax
	cmpq	%rax, %r14
	jbe	.LBB13_26
# BB#23:                                # %land.lhs.true.i
	movq	520(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB13_26
# BB#24:                                # %if.then8.i18
	movq	%r14, 504(%r12)
	movq	%r14, 512(%r12)
.LBB13_25:                              # %_cpp_pop_buffer.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB13_26:                              # %if.else.i
	addq	$488, %r12              # imm = 0x1E8
	movq	%r12, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	obstack_free  # TAILCALL
.Ltmp161:
	.size	run_directive, .Ltmp161-run_directive
	.cfi_endproc

	.globl	_cpp_define_builtin
	.align	16, 0x90
	.type	_cpp_define_builtin,@function
_cpp_define_builtin:                    # @_cpp_define_builtin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp165:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp166:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp167:
	.cfi_def_cfa_offset 32
.Ltmp168:
	.cfi_offset %rbx, -24
.Ltmp169:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	strlen
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	run_directive  # TAILCALL
.Ltmp170:
	.size	_cpp_define_builtin, .Ltmp170-_cpp_define_builtin
	.cfi_endproc

	.globl	cpp_undef
	.align	16, 0x90
	.type	cpp_undef,@function
cpp_undef:                              # @cpp_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp175:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp176:
	.cfi_def_cfa_offset 32
.Ltmp177:
	.cfi_offset %rbx, -24
.Ltmp178:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	strlen
	movq	$7, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	run_directive  # TAILCALL
.Ltmp179:
	.size	cpp_undef, .Ltmp179-cpp_undef
	.cfi_endproc

	.globl	cpp_assert
	.align	16, 0x90
	.type	cpp_assert,@function
cpp_assert:                             # @cpp_assert
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$16, %rdx
	jmp	handle_assertion  # TAILCALL
.Ltmp180:
	.size	cpp_assert, .Ltmp180-cpp_assert
	.cfi_endproc

	.align	16, 0x90
	.type	handle_assertion,@function
handle_assertion:                       # @handle_assertion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp184:
	.cfi_def_cfa_offset 16
.Ltmp185:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp186:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp187:
	.cfi_offset %rbx, -56
.Ltmp188:
	.cfi_offset %r12, -48
.Ltmp189:
	.cfi_offset %r13, -40
.Ltmp190:
	.cfi_offset %r14, -32
.Ltmp191:
	.cfi_offset %r15, -24
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r12
	movq	$61, %rsi
	movq	%rbx, %rdi
	callq	strchr
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB17_2
# BB#1:                                 # %if.then
	movq	%r14, -48(%rbp)         # 8-byte Spill
	movq	%rsp, %r14
	leaq	17(%r12), %rax
	andq	$-16, %rax
	subq	%rax, %r14
	movq	%r14, %rsp
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	callq	memcpy
	subq	%rbx, %r13
	movq	$40, %rax
	movb	%al, (%r14,%r13)
	movq	$41, %rax
	movw	%ax, (%r12,%r14)
	incq	%r12
	movq	%r14, %rbx
	movq	-48(%rbp), %r14         # 8-byte Reload
.LBB17_2:                               # %if.end
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	callq	run_directive
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp192:
	.size	handle_assertion, .Ltmp192-handle_assertion
	.cfi_endproc

	.globl	cpp_unassert
	.align	16, 0x90
	.type	cpp_unassert,@function
cpp_unassert:                           # @cpp_unassert
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$17, %rdx
	jmp	handle_assertion  # TAILCALL
.Ltmp193:
	.size	cpp_unassert, .Ltmp193-cpp_unassert
	.cfi_endproc

	.globl	cpp_errors
	.align	16, 0x90
	.type	cpp_errors,@function
cpp_errors:                             # @cpp_errors
	.cfi_startproc
# BB#0:                                 # %entry
	movl	248(%rdi), %eax
	retq
.Ltmp194:
	.size	cpp_errors, .Ltmp194-cpp_errors
	.cfi_endproc

	.globl	cpp_get_options
	.align	16, 0x90
	.type	cpp_get_options,@function
cpp_get_options:                        # @cpp_get_options
	.cfi_startproc
# BB#0:                                 # %entry
	leaq	648(%rdi), %rax
	retq
.Ltmp195:
	.size	cpp_get_options, .Ltmp195-cpp_get_options
	.cfi_endproc

	.globl	cpp_get_callbacks
	.align	16, 0x90
	.type	cpp_get_callbacks,@function
cpp_get_callbacks:                      # @cpp_get_callbacks
	.cfi_startproc
# BB#0:                                 # %entry
	leaq	584(%rdi), %rax
	retq
.Ltmp196:
	.size	cpp_get_callbacks, .Ltmp196-cpp_get_callbacks
	.cfi_endproc

	.globl	cpp_get_line_maps
	.align	16, 0x90
	.type	cpp_get_line_maps,@function
cpp_get_line_maps:                      # @cpp_get_line_maps
	.cfi_startproc
# BB#0:                                 # %entry
	leaq	24(%rdi), %rax
	retq
.Ltmp197:
	.size	cpp_get_line_maps, .Ltmp197-cpp_get_line_maps
	.cfi_endproc

	.globl	cpp_set_callbacks
	.align	16, 0x90
	.type	cpp_set_callbacks,@function
cpp_set_callbacks:                      # @cpp_set_callbacks
	.cfi_startproc
# BB#0:                                 # %entry
	movq	48(%rsi), %rax
	movq	%rax, 632(%rdi)
	movq	40(%rsi), %rax
	movq	%rax, 624(%rdi)
	movq	32(%rsi), %rax
	movq	%rax, 616(%rdi)
	movq	24(%rsi), %rax
	movq	%rax, 608(%rdi)
	movq	16(%rsi), %rax
	movq	%rax, 600(%rdi)
	movq	(%rsi), %rax
	movq	8(%rsi), %rcx
	movq	%rcx, 592(%rdi)
	movq	%rax, 584(%rdi)
	retq
.Ltmp198:
	.size	cpp_set_callbacks, .Ltmp198-cpp_set_callbacks
	.cfi_endproc

	.globl	cpp_push_buffer
	.align	16, 0x90
	.type	cpp_push_buffer,@function
cpp_push_buffer:                        # @cpp_push_buffer
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
	subq	$16, %rsp
.Ltmp210:
	.cfi_def_cfa_offset 64
.Ltmp211:
	.cfi_offset %rbx, -48
.Ltmp212:
	.cfi_offset %r12, -40
.Ltmp213:
	.cfi_offset %r13, -32
.Ltmp214:
	.cfi_offset %r14, -24
.Ltmp215:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	512(%r13), %rax
	movq	520(%r13), %rdi
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r14
	subq	%rax, %rdi
	cmpq	$143, %rdi
	jg	.LBB24_2
# BB#1:                                 # %if.then
	leaq	488(%r13), %rdi
	movq	$144, %rsi
	callq	_obstack_newchunk
	movq	512(%r13), %rax
.LBB24_2:                               # %if.end
	addq	$144, %rax
	movq	%rax, 512(%r13)
	movq	504(%r13), %rbx
	cmpq	%rbx, %rax
	jne	.LBB24_4
# BB#3:                                 # %if.then8
	movzbl	568(%r13), %ecx
	orq	$2, %rcx
	movb	%cl, 568(%r13)
.LBB24_4:                               # %if.end9
	movslq	536(%r13), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, 512(%r13)
	movq	520(%r13), %rax
	movq	496(%r13), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB24_6
# BB#5:                                 # %if.then29
	movq	%rax, 512(%r13)
	movq	%rax, %rcx
.LBB24_6:                               # %if.end32
	movq	%rcx, 504(%r13)
	xorq	%rsi, %rsi
	movq	$144, %rdx
	movq	%rbx, %rdi
	callq	memset
	movq	%r14, (%rbx)
	movq	%r14, 40(%rbx)
	movq	%r14, 24(%rbx)
	addq	%r14, %r12
	movq	%r12, 16(%rbx)
	movb	%r15b, 81(%rbx)
	movq	(%r13), %rax
	movq	%rax, 32(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB24_7
# BB#8:                                 # %if.end32
	movabsq	$1, %rax
	jmp	.LBB24_9
.LBB24_7:
	movabsq	$0, %rax
.LBB24_9:                               # %if.end32
	movb	%al, 83(%rbx)
	movq	$64, %rax
	movb	%al, 68(%rbx)
	movq	%rbx, (%r13)
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp216:
	.size	cpp_push_buffer, .Ltmp216-cpp_push_buffer
	.cfi_endproc

	.globl	_cpp_pop_buffer
	.align	16, 0x90
	.type	_cpp_pop_buffer,@function
_cpp_pop_buffer:                        # @_cpp_pop_buffer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp223:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp224:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp225:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp226:
	.cfi_def_cfa_offset 48
.Ltmp227:
	.cfi_offset %rbx, -40
.Ltmp228:
	.cfi_offset %r12, -32
.Ltmp229:
	.cfi_offset %r14, -24
.Ltmp230:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	(%r12), %r14
	movq	56(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB25_3
# BB#1:
	movabsq	$.L.str11, %r15
	.align	16, 0x90
.LBB25_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	28(%rbx), %rax
	leaq	(%rax,%rax,2), %rax
	movq	dtable+8(,%rax,8), %r8
	movl	8(%rbx), %esi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r15, %rcx
	callq	cpp_error_with_line
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB25_2
.LBB25_3:                               # %for.end
	xorq	%rax, %rax
	movb	%al, 9(%r12)
	movq	32(%r14), %rax
	movq	%rax, (%r12)
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB25_5
# BB#4:                                 # %if.then
	movq	%r12, %rdi
	callq	_cpp_pop_file_buffer
	testq	%rax, %rax
	jne	.LBB25_8
.LBB25_5:                               # %if.then6
	movq	496(%r12), %rax
	cmpq	%rax, %r14
	jbe	.LBB25_9
# BB#6:                                 # %land.lhs.true
	movq	520(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB25_9
# BB#7:                                 # %if.then8
	movq	%r14, 504(%r12)
	movq	%r14, 512(%r12)
.LBB25_8:                               # %if.end10
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB25_9:                               # %if.else
	addq	$488, %r12              # imm = 0x1E8
	movq	%r12, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	obstack_free  # TAILCALL
.Ltmp231:
	.size	_cpp_pop_buffer, .Ltmp231-_cpp_pop_buffer
	.cfi_endproc

	.globl	_cpp_init_directives
	.align	16, 0x90
	.type	_cpp_init_directives,@function
_cpp_init_directives:                   # @_cpp_init_directives
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp236:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp237:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp238:
	.cfi_def_cfa_offset 32
.Ltmp239:
	.cfi_offset %rbx, -32
.Ltmp240:
	.cfi_offset %r14, -24
.Ltmp241:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$1, %r15
	movabsq	$dtable+16, %rbx
	.align	16, 0x90
.LBB26_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rbx), %rsi
	movzbl	(%rbx), %edx
	movq	%r14, %rdi
	callq	cpp_lookup
	movb	%r15b, 18(%rax)
	incq	%r15
	addq	$24, %rbx
	cmpq	$20, %r15
	jne	.LBB26_1
# BB#2:                                 # %for.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp242:
	.size	_cpp_init_directives, .Ltmp242-_cpp_init_directives
	.cfi_endproc

	.align	16, 0x90
	.type	parse_include,@function
parse_include:                          # @parse_include
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp249:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp250:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp251:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp252:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp253:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp254:
	.cfi_def_cfa_offset 64
.Ltmp255:
	.cfi_offset %rbx, -48
.Ltmp256:
	.cfi_offset %r12, -40
.Ltmp257:
	.cfi_offset %r13, -32
.Ltmp258:
	.cfi_offset %r14, -24
.Ltmp259:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	160(%r12), %rax
	movabsq	$dtable+264, %rcx
	cmpq	%rcx, %rax
	je	.LBB27_1
# BB#2:                                 # %if.else
	movq	8(%rax), %r15
	jmp	.LBB27_3
.LBB27_1:
	movabsq	$.L.str20, %r15
.LBB27_3:                               # %if.end
	movq	%r12, %rdi
	callq	cpp_get_token
	movq	%rax, %r14
	movzbl	6(%r14), %eax
	cmpq	$3, %rax
	je	.LBB27_7
# BB#4:                                 # %if.end
	cmpq	$61, %rax
	je	.LBB27_17
# BB#5:                                 # %if.end
	cmpq	$63, %rax
	je	.LBB27_17
# BB#6:                                 # %if.then12
	movabsq	$.L.str21, %rsi
	jmp	.LBB27_19
.LBB27_7:                               # %if.end13
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	$1024, %rdi             # imm = 0x400
	movq	$1024, 8(%rsp)          # 8-byte Folded Spill
                                        # imm = 0x400
	callq	xmalloc
	movq	%rax, %rbx
	xorq	%r13, %r13
	jmp	.LBB27_8
	.align	16, 0x90
.LBB27_14:                              # %if.end15.i
                                        #   in Loop: Header=BB27_8 Depth=1
	addq	%rbx, %r13
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	cpp_spell_token
	movq	%rax, %r13
	subq	%rbx, %r13
.LBB27_8:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rdi
	callq	cpp_get_token
	movq	%rax, %r14
	movzbl	6(%r14), %eax
	cmpq	$2, %rax
	je	.LBB27_16
# BB#9:                                 # %for.cond.i
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	%rbx, %r15
	cmpq	$67, %rax
	je	.LBB27_15
# BB#10:                                # %if.end.i
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	%r14, %rdi
	callq	cpp_token_len
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	leaq	(%rbx,%r13), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rcx, %rax
	jbe	.LBB27_12
# BB#11:                                # %if.then9.i
                                        #   in Loop: Header=BB27_8 Depth=1
	addq	%rax, %rbx
	addq	%rbx, %rbx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	xrealloc
	movq	%rax, %r15
	movq	%rbx, 8(%rsp)           # 8-byte Spill
.LBB27_12:                              # %if.end12.i
                                        #   in Loop: Header=BB27_8 Depth=1
	movzbl	7(%r14), %eax
	movq	%r15, %rbx
	testq	$1, %rax
	je	.LBB27_14
# BB#13:                                # %if.then14.i
                                        #   in Loop: Header=BB27_8 Depth=1
	movq	$32, %rax
	movb	%al, (%rbx,%r13)
	incq	%r13
	jmp	.LBB27_14
.LBB27_16:                              # %glue_header_name.exit
	leaq	1(%r13), %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	memcpy
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	movb	%bl, (%r15,%r13)
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movq	%rax, %r14
	movq	$63, %rax
	movb	%al, 6(%r14)
	movb	%bl, 7(%r14)
	movl	%r13d, 8(%r14)
	movq	%r15, 16(%r14)
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	free
	testq	%r14, %r14
	movq	(%rsp), %r15            # 8-byte Reload
	je	.LBB27_20
.LBB27_17:                              # %if.end18
	movl	8(%r14), %eax
	testq	%rax, %rax
	movq	%r14, %rbx
	jne	.LBB27_20
# BB#18:                                # %if.then20
	movabsq	$.L.str22, %rsi
.LBB27_19:                              # %return
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	cpp_error
.LBB27_20:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB27_15:                              # %glue_header_name.exit.thread
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	callq	cpp_error
	movq	%r15, %rdi
	callq	free
	jmp	.LBB27_20
.Ltmp260:
	.size	parse_include, .Ltmp260-parse_include
	.cfi_endproc

	.align	16, 0x90
	.type	do_linemarker,@function
do_linemarker:                          # @do_linemarker
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp267:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp268:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp269:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp270:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp271:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp272:
	.cfi_def_cfa_offset 80
.Ltmp273:
	.cfi_offset %rbx, -48
.Ltmp274:
	.cfi_offset %r12, -40
.Ltmp275:
	.cfi_offset %r13, -32
.Ltmp276:
	.cfi_offset %r14, -24
.Ltmp277:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	56(%r12), %rax
	movq	(%rax), %r14
	movzbl	21(%rax), %r13d
	movq	$1, %rsi
                                        # kill: RDI<def> R12<kill>
	callq	_cpp_backup_tokens
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$57, %rcx
	jne	.LBB28_14
# BB#1:                                 # %lor.lhs.false
	movl	8(%rax), %ecx
	xorq	%r15, %r15
	testq	%rcx, %rcx
	je	.LBB28_5
# BB#2:                                 # %while.body.i.preheader
	movq	16(%rax), %rdx
	xorq	%r15, %r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB28_3:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %edi
	movzbl	_sch_istable(%rdi,%rdi), %ebx
	testq	$4, %rbx
	je	.LBB28_14
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB28_3 Depth=1
	decq	%rcx
	incq	%rdx
	leaq	(%r15,%r15,4), %rbx
	leaq	-48(%rdi,%rbx,2), %r15
	testq	%rsi, %rcx
	jne	.LBB28_3
.LBB28_5:                               # %if.end
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	movq	$2, %r14
	cmpq	$67, %rcx
	je	.LBB28_56
# BB#6:                                 # %if.end
	cmpq	$61, %rcx
	jne	.LBB28_65
# BB#7:                                 # %if.then13
	movl	8(%rax), %ebx
	movq	16(%rax), %r13
	movq	%r13, 24(%rsp)
	leaq	1(%rbx), %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rbx, %rbx
	movq	%rax, %r14
	je	.LBB28_12
# BB#8:
	addq	%r13, %rbx
	movq	16(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB28_9:                               # %while.body.i89
                                        # =>This Inner Loop Header: Depth=1
	leaq	1(%r13), %rax
	movq	%rax, 24(%rsp)
	movzbl	(%r13), %eax
	cmpq	$92, %rax
	jne	.LBB28_11
# BB#10:                                # %if.else.i
                                        #   in Loop: Header=BB28_9 Depth=1
	movq	$255, %rcx
	xorq	%r8, %r8
	movq	%r12, %rdi
	leaq	24(%rsp), %rsi
	movq	%rbx, %rdx
	callq	cpp_parse_escape
.LBB28_11:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB28_9 Depth=1
	movb	%al, (%r14)
	movq	24(%rsp), %r13
	incq	%r14
	cmpq	%rbx, %r13
	jb	.LBB28_9
.LBB28_12:                              # %dequote_string.exit
	xorq	%rax, %rax
	movb	%al, (%r14)
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	movq	$2, %r14
	cmpq	$67, %rcx
	jne	.LBB28_16
# BB#13:
	xorq	%r13, %r13
	jmp	.LBB28_53
.LBB28_14:                              # %if.then
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str35, %rsi
.LBB28_15:                              # %return
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB28_64
.LBB28_65:                              # %if.then47
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str36, %rsi
	jmp	.LBB28_15
.LBB28_16:                              # %dequote_string.exit
	cmpq	$57, %rcx
	jne	.LBB28_45
# BB#17:                                # %land.lhs.true.i
	movl	8(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB28_45
# BB#18:                                # %if.then.i94
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	leaq	-48(%rcx), %rbx
	movq	%rbx, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$4, %rsi
	ja	.LBB28_45
# BB#19:                                # %switch.early.test
	movq	$2, %rdi
	addq	$-48, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$4, %rcx
	ja	.LBB28_42
# BB#20:                                # %switch.early.test
	jmpq	*.LJTI28_0(,%rcx,8)
.LBB28_21:                              # %if.then24
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	_cpp_fake_include
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	xorq	%r14, %r14
	cmpq	$67, %rcx
	movabsq	$0, %r13
	je	.LBB28_53
# BB#22:                                # %if.then24
	cmpq	$57, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	jne	.LBB28_31
# BB#23:                                # %land.lhs.true.i101
	movl	8(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB28_31
# BB#24:                                # %if.then.i108
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	addq	$-48, %rcx
	movq	%rcx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rdi, %rdx
	jbe	.LBB28_31
# BB#25:                                # %if.then.i108
	cmpq	$4, %rdx
	ja	.LBB28_31
# BB#26:                                # %land.lhs.true9.i110
	jne	.LBB28_28
# BB#27:                                # %land.lhs.true9.i110
	cmpq	$3, %rdi
	jne	.LBB28_31
.LBB28_28:                              # %land.lhs.true14.i113
	cmpq	$2, %rdx
	jne	.LBB28_29
# BB#30:                                # %land.lhs.true14.i113
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	movq	%rcx, %rbx
	movabsq	$0, %rdi
	je	.LBB28_42
.LBB28_31:                              # %if.then27.i117
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	xorq	%r14, %r14
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	xorq	%r13, %r13
	jmp	.LBB28_53
.LBB28_32:                              # %if.then28
	movq	%rsi, %r13
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	movq	$1, %r14
	cmpq	$67, %rcx
	jne	.LBB28_34
# BB#33:
	xorq	%r13, %r13
	jmp	.LBB28_53
.LBB28_34:                              # %if.then28
	cmpq	$57, %rcx
	movq	%r13, %rdi
	jne	.LBB28_45
# BB#35:                                # %land.lhs.true.i125
	movl	8(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB28_45
# BB#36:                                # %if.then.i132
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	addq	$-48, %rcx
	movq	%rcx, %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	%rdi, %rdx
	jbe	.LBB28_45
# BB#37:                                # %if.then.i132
	cmpq	$4, %rdx
	ja	.LBB28_45
# BB#38:                                # %land.lhs.true9.i136
	jne	.LBB28_40
# BB#39:                                # %land.lhs.true9.i136
	cmpq	$3, %rdi
	jne	.LBB28_45
.LBB28_40:                              # %land.lhs.true14.i140
	cmpq	$2, %rdx
	jne	.LBB28_41
# BB#44:                                # %land.lhs.true14.i140
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rbx
	movq	%rcx, %rbx
	movq	$1, %rdi
	je	.LBB28_42
.LBB28_45:                              # %if.then27.i
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
.LBB28_53:                              # %if.end40
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB28_56
# BB#54:                                # %land.lhs.true.i179
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB28_56
# BB#55:                                # %if.then.i180
	movq	160(%r12), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
.LBB28_56:                              # %if.end50
	movq	152(%r12), %rax
	leaq	96(%r12), %rbx
	jmp	.LBB28_58
	.align	16, 0x90
.LBB28_57:                              # %while.body.i184
                                        #   in Loop: Header=BB28_58 Depth=1
	movq	%r12, %rdi
	callq	_cpp_pop_context
	movq	152(%r12), %rax
.LBB28_58:                              # %while.body.i184
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rax
	jne	.LBB28_57
# BB#59:                                # %while.end.i189
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB28_61
	.align	16, 0x90
.LBB28_60:                              # %while.cond2.i
                                        #   in Loop: Header=BB28_61 Depth=1
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB28_61:                              # %while.cond2.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB28_60
# BB#62:                                # %skip_rest_of_line.exit
	movl	64(%r12), %ecx
	leaq	24(%r12), %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	movq	16(%rsp), %r8           # 8-byte Reload
	movq	%r15, %r9
	callq	add_line_map
	movq	%rax, 56(%r12)
	movq	592(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB28_64
# BB#63:                                # %if.then.i
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	*%rcx
.LBB28_64:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_29:
	movq	%rcx, %rbx
	xorq	%rdi, %rdi
	jmp	.LBB28_42
.LBB28_41:
	movq	%rcx, %rbx
	movq	$1, %rdi
.LBB28_42:                              # %if.end31
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$3, %rbx
	jne	.LBB28_43
# BB#46:                                # %if.then34
	movq	%rdi, %rbx
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	movq	$1, %r13
	cmpq	$67, %rcx
	je	.LBB28_52
# BB#47:                                # %if.then34
	cmpq	$57, %rcx
	jne	.LBB28_51
# BB#48:                                # %land.lhs.true.i152
	movl	8(%rax), %ecx
	cmpq	$1, %rcx
	jne	.LBB28_51
# BB#49:                                # %if.then.i159
	movq	16(%rax), %rcx
	movzbl	(%rcx), %ecx
	cmpq	$52, %rcx
	jne	.LBB28_51
# BB#50:
	movq	%rbx, %r14
	movq	$2, %r13
	jmp	.LBB28_53
.LBB28_43:
	movq	%rdi, %r14
	xorq	%r13, %r13
	jmp	.LBB28_53
.LBB28_51:                              # %if.then27.i171
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
.LBB28_52:                              # %if.end40
	movq	%rbx, %r14
	jmp	.LBB28_53
.Ltmp278:
	.size	do_linemarker, .Ltmp278-do_linemarker
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI28_0:
	.quad	.LBB28_45
	.quad	.LBB28_21
	.quad	.LBB28_32
	.quad	.LBB28_42
	.quad	.LBB28_45

	.text
	.align	16, 0x90
	.type	do_define,@function
do_define:                              # @do_define
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp282:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp283:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp284:
	.cfi_def_cfa_offset 32
.Ltmp285:
	.cfi_offset %rbx, -24
.Ltmp286:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	lex_macro_node
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB29_3
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	_cpp_create_definition
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB29_3
# BB#2:                                 # %if.then3
	movq	608(%rbx), %rax
	testq	%rax, %rax
	je	.LBB29_3
# BB#4:                                 # %if.then5
	movl	68(%rbx), %esi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmpq	*%rax  # TAILCALL
.LBB29_3:                               # %if.end9
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp287:
	.size	do_define, .Ltmp287-do_define
	.cfi_endproc

	.align	16, 0x90
	.type	do_include,@function
do_include:                             # @do_include
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rsi, %rsi
	jmp	do_include_common  # TAILCALL
.Ltmp288:
	.size	do_include, .Ltmp288-do_include
	.cfi_endproc

	.align	16, 0x90
	.type	do_endif,@function
do_endif:                               # @do_endif
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp293:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp294:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp295:
	.cfi_def_cfa_offset 32
.Ltmp296:
	.cfi_offset %rbx, -32
.Ltmp297:
	.cfi_offset %r14, -24
.Ltmp298:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	(%rbx), %r15
	movq	56(%r15), %r14
	testq	%r14, %r14
	je	.LBB31_11
# BB#1:                                 # %if.else
	movzbl	25(%r14), %eax
	testq	%rax, %rax
	jne	.LBB31_5
# BB#2:                                 # %if.then2
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB31_5
# BB#3:                                 # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB31_5
# BB#4:                                 # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_pedwarn
.LBB31_5:                               # %if.end
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB31_8
# BB#6:                                 # %land.lhs.true
	movq	16(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB31_8
# BB#7:                                 # %if.then5
	movq	$1, %rdx
	movb	%dl, 184(%rbx)
	movq	%rcx, 168(%rbx)
.LBB31_8:                               # %if.end8
	movq	%rax, 56(%r15)
	movzbl	25(%r14), %eax
	movb	%al, 9(%rbx)
	movq	496(%rbx), %rax
	cmpq	%rax, %r14
	jbe	.LBB31_12
# BB#9:                                 # %land.lhs.true15
	movq	520(%rbx), %rax
	cmpq	%rax, %r14
	jae	.LBB31_12
# BB#10:                                # %if.end21
	movq	%r14, 504(%rbx)
	movq	%r14, 512(%rbx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB31_12:                              # %if.else19
	addq	$488, %rbx              # imm = 0x1E8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	obstack_free  # TAILCALL
.LBB31_11:                              # %if.then
	movabsq	$.L.str74, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	cpp_error  # TAILCALL
.Ltmp299:
	.size	do_endif, .Ltmp299-do_endif
	.cfi_endproc

	.align	16, 0x90
	.type	do_ifdef,@function
do_ifdef:                               # @do_ifdef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp303:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp304:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp305:
	.cfi_def_cfa_offset 32
.Ltmp306:
	.cfi_offset %rbx, -24
.Ltmp307:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	9(%rbx), %eax
	testq	%rax, %rax
	movq	$1, %r14
	jne	.LBB32_8
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	lex_macro_node
	testq	%rax, %rax
	je	.LBB32_8
# BB#2:                                 # %if.then4
	movzbl	20(%rax), %eax
	cmpq	$1, %rax
	je	.LBB32_3
# BB#4:                                 # %if.then4
	movabsq	$1, %r14
	jmp	.LBB32_5
.LBB32_3:
	movabsq	$0, %r14
.LBB32_5:                               # %if.then4
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB32_8
# BB#6:                                 # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB32_8
# BB#7:                                 # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_pedwarn
.LBB32_8:                               # %if.end6
	movq	$3, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	push_conditional  # TAILCALL
.Ltmp308:
	.size	do_ifdef, .Ltmp308-do_ifdef
	.cfi_endproc

	.align	16, 0x90
	.type	do_if,@function
do_if:                                  # @do_if
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp311:
	.cfi_def_cfa_offset 16
.Ltmp312:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	9(%rbx), %eax
	testq	%rax, %rax
	movq	$1, %rsi
	jne	.LBB33_4
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	_cpp_parse_expr
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB33_2
# BB#3:                                 # %if.then
	movabsq	$1, %rsi
	jmp	.LBB33_4
.LBB33_2:
	movabsq	$0, %rsi
.LBB33_4:                               # %if.end
	movq	176(%rbx), %rcx
	movq	$4, %rdx
	movq	%rbx, %rdi
	popq	%rbx
	jmp	push_conditional  # TAILCALL
.Ltmp313:
	.size	do_if, .Ltmp313-do_if
	.cfi_endproc

	.align	16, 0x90
	.type	do_else,@function
do_else:                                # @do_else
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp317:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp318:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp319:
	.cfi_def_cfa_offset 32
.Ltmp320:
	.cfi_offset %rbx, -24
.Ltmp321:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB34_7
# BB#1:                                 # %if.else
	movl	28(%rbx), %eax
	cmpq	$5, %rax
	jne	.LBB34_3
# BB#2:                                 # %if.then3
	movabsq	$.L.str73, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error
	movl	8(%rbx), %esi
	movabsq	$.L.str64, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error_with_line
.LBB34_3:                               # %if.end
	movq	$5, %rax
	movl	%eax, 28(%rbx)
	movzbl	24(%rbx), %eax
	movb	%al, 9(%r14)
	movq	$1, %rax
	movb	%al, 24(%rbx)
	movq	$0, 16(%rbx)
	movzbl	25(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB34_6
# BB#4:                                 # %if.then7
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB34_6
# BB#5:                                 # %land.lhs.true.i
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB34_8
.LBB34_6:                               # %if.end9
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB34_7:                               # %if.then
	movabsq	$.L.str72, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_error  # TAILCALL
.LBB34_8:                               # %if.then.i
	movq	160(%r14), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp322:
	.size	do_else, .Ltmp322-do_else
	.cfi_endproc

	.align	16, 0x90
	.type	do_ifndef,@function
do_ifndef:                              # @do_ifndef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp328:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp329:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp330:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp331:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp332:
	.cfi_def_cfa_offset 48
.Ltmp333:
	.cfi_offset %rbx, -40
.Ltmp334:
	.cfi_offset %r12, -32
.Ltmp335:
	.cfi_offset %r14, -24
.Ltmp336:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	9(%r12), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	$1, %r14
	jne	.LBB35_9
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	callq	lex_macro_node
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB35_9
# BB#2:                                 # %if.then4
	movzbl	20(%r15), %eax
	cmpq	$1, %rax
	jne	.LBB35_3
# BB#4:                                 # %if.then4
	movabsq	$1, %r14
	jmp	.LBB35_5
.LBB35_3:
	movabsq	$0, %r14
.LBB35_5:                               # %if.then4
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB35_8
# BB#6:                                 # %land.lhs.true.i
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB35_8
# BB#7:                                 # %if.then.i
	movq	160(%r12), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
.LBB35_8:                               # %if.end6
	movq	%r15, %rbx
.LBB35_9:                               # %if.end6
	movq	$6, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	push_conditional  # TAILCALL
.Ltmp337:
	.size	do_ifndef, .Ltmp337-do_ifndef
	.cfi_endproc

	.align	16, 0x90
	.type	do_undef,@function
do_undef:                               # @do_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp341:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp342:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp343:
	.cfi_def_cfa_offset 32
.Ltmp344:
	.cfi_offset %rbx, -24
.Ltmp345:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	lex_macro_node
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB36_7
# BB#1:                                 # %land.lhs.true
	movzbl	20(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	jne	.LBB36_7
# BB#2:                                 # %if.then
	movq	616(%r14), %r8
	movq	$8, %rcx
	shrq	%cl, %rax
	testq	%r8, %r8
	je	.LBB36_4
# BB#3:                                 # %if.then2
	movl	68(%r14), %esi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	*%r8
	movzbl	21(%rbx), %eax
.LBB36_4:                               # %if.end
	testq	$16, %rax
	je	.LBB36_6
# BB#5:                                 # %if.then6
	movq	8(%rbx), %rdx
	movabsq	$.L.str67, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_warning
.LBB36_6:                               # %if.end7
	movq	%rbx, %rdi
	callq	_cpp_free_definition
.LBB36_7:                               # %if.end8
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB36_9
# BB#8:                                 # %land.lhs.true.i
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB36_10
.LBB36_9:                               # %check_eol.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB36_10:                              # %if.then.i
	movq	160(%r14), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp346:
	.size	do_undef, .Ltmp346-do_undef
	.cfi_endproc

	.align	16, 0x90
	.type	do_line,@function
do_line:                                # @do_line
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
	subq	$16, %rsp
.Ltmp358:
	.cfi_def_cfa_offset 64
.Ltmp359:
	.cfi_offset %rbx, -48
.Ltmp360:
	.cfi_offset %r12, -40
.Ltmp361:
	.cfi_offset %r13, -32
.Ltmp362:
	.cfi_offset %r14, -24
.Ltmp363:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	56(%r14), %rax
	movq	(%rax), %r13
	movzbl	769(%r14), %r15d
                                        # kill: RDI<def> R14<kill>
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$57, %rcx
	jne	.LBB37_19
# BB#1:                                 # %lor.lhs.false
	movl	8(%rax), %ecx
	xorq	%r12, %r12
	testq	%rcx, %rcx
	je	.LBB37_5
# BB#2:                                 # %while.body.i.preheader
	movq	16(%rax), %rdx
	xorq	%r12, %r12
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB37_3:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdx), %edi
	movzbl	_sch_istable(%rdi,%rdi), %ebx
	testq	$4, %rbx
	je	.LBB37_19
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB37_3 Depth=1
	decq	%rcx
	incq	%rdx
	leaq	(%r12,%r12,4), %rbx
	leaq	-48(%rdi,%rbx,2), %r12
	testq	%rsi, %rcx
	jne	.LBB37_3
.LBB37_5:                               # %if.end
	movzbl	770(%r14), %eax
	testq	%rax, %rax
	je	.LBB37_10
# BB#6:                                 # %land.lhs.true
	movq	$2147483647, %rax       # imm = 0x7FFFFFFF
	testq	$255, %r15
	jne	.LBB37_8
# BB#7:                                 # %select.mid
	movq	$32767, %rax            # imm = 0x7FFF
.LBB37_8:                               # %select.end
	leaq	-1(%r12), %rcx
	cmpq	%rax, %rcx
	jb	.LBB37_10
# BB#9:                                 # %if.then16
	movabsq	$.L.str66, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_pedwarn
.LBB37_10:                              # %if.end17
	movq	%r14, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB37_23
# BB#11:                                # %if.end17
	cmpq	$61, %rcx
	jne	.LBB37_32
# BB#12:                                # %if.then24
	movl	8(%rax), %ebx
	movq	16(%rax), %r13
	movq	%r13, 8(%rsp)
	leaq	1(%rbx), %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movq	%r14, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rbx, %rbx
	movq	%rax, %r15
	je	.LBB37_17
# BB#13:
	addq	%r13, %rbx
	movq	(%rsp), %r15            # 8-byte Reload
	.align	16, 0x90
.LBB37_14:                              # %while.body.i71
                                        # =>This Inner Loop Header: Depth=1
	leaq	1(%r13), %rax
	movq	%rax, 8(%rsp)
	movzbl	(%r13), %eax
	cmpq	$92, %rax
	jne	.LBB37_16
# BB#15:                                # %if.else.i
                                        #   in Loop: Header=BB37_14 Depth=1
	movq	$255, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	leaq	8(%rsp), %rsi
	movq	%rbx, %rdx
	callq	cpp_parse_escape
.LBB37_16:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB37_14 Depth=1
	movb	%al, (%r15)
	movq	8(%rsp), %r13
	incq	%r15
	cmpq	%rbx, %r13
	jb	.LBB37_14
.LBB37_17:                              # %dequote_string.exit
	xorq	%rax, %rax
	movb	%al, (%r15)
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB37_21
# BB#18:
	movq	(%rsp), %r13            # 8-byte Reload
	jmp	.LBB37_23
.LBB37_19:                              # %if.then
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str65, %rsi
.LBB37_20:                              # %return
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB37_31
.LBB37_32:                              # %if.then37
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str36, %rsi
	jmp	.LBB37_20
.LBB37_21:                              # %land.lhs.true.i
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	movq	(%rsp), %r13            # 8-byte Reload
	je	.LBB37_23
# BB#22:                                # %if.then.i75
	movq	160(%r14), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_pedwarn
.LBB37_23:                              # %if.end40
	movq	152(%r14), %rax
	leaq	96(%r14), %rbx
	jmp	.LBB37_25
	.align	16, 0x90
.LBB37_24:                              # %while.body.i78
                                        #   in Loop: Header=BB37_25 Depth=1
	movq	%r14, %rdi
	callq	_cpp_pop_context
	movq	152(%r14), %rax
.LBB37_25:                              # %while.body.i78
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rax
	jne	.LBB37_24
# BB#26:                                # %while.end.i82
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB37_28
	.align	16, 0x90
.LBB37_27:                              # %while.cond2.i
                                        #   in Loop: Header=BB37_28 Depth=1
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB37_28:                              # %while.cond2.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB37_27
# BB#29:                                # %skip_rest_of_line.exit
	movq	56(%r14), %rax
	movzbl	21(%rax), %edx
	movl	64(%r14), %ecx
	leaq	24(%r14), %rdi
	movq	$2, %rsi
	movq	%r13, %r8
	movq	%r12, %r9
	callq	add_line_map
	movq	%rax, 56(%r14)
	movq	592(%r14), %rcx
	testq	%rcx, %rcx
	je	.LBB37_31
# BB#30:                                # %if.then.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	*%rcx
.LBB37_31:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp364:
	.size	do_line, .Ltmp364-do_line
	.cfi_endproc

	.align	16, 0x90
	.type	do_elif,@function
do_elif:                                # @do_elif
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp368:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp369:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp370:
	.cfi_def_cfa_offset 32
.Ltmp371:
	.cfi_offset %rbx, -24
.Ltmp372:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	(%r14), %rax
	movq	56(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB38_11
# BB#1:                                 # %if.else
	movl	28(%rbx), %eax
	cmpq	$5, %rax
	jne	.LBB38_3
# BB#2:                                 # %if.then3
	movabsq	$.L.str63, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error
	movl	8(%rbx), %esi
	movabsq	$.L.str64, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error_with_line
.LBB38_3:                               # %if.end
	movq	$9, %rax
	movl	%eax, 28(%rbx)
	movzbl	24(%rbx), %eax
	testq	%rax, %rax
	je	.LBB38_5
# BB#4:                                 # %if.then5
	movq	$1, %rax
	movb	%al, 9(%r14)
	jmp	.LBB38_10
.LBB38_11:                              # %if.then
	movabsq	$.L.str62, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_error  # TAILCALL
.LBB38_5:                               # %if.else6
	xorq	%rax, %rax
	movb	%al, 9(%r14)
	movq	%r14, %rdi
	callq	_cpp_parse_expr
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB38_7
# BB#6:                                 # %if.else6
	movq	%rdx, %rcx
.LBB38_7:                               # %if.else6
	testq	%rsi, %rsi
	jne	.LBB38_9
# BB#8:                                 # %if.else6
	movq	%rdx, %rax
.LBB38_9:                               # %if.else6
	movb	%al, 9(%r14)
	movb	%cl, 24(%rbx)
.LBB38_10:                              # %if.end19
	movq	$0, 16(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp373:
	.size	do_elif, .Ltmp373-do_elif
	.cfi_endproc

	.align	16, 0x90
	.type	do_error,@function
do_error:                               # @do_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp377:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp378:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp379:
	.cfi_def_cfa_offset 32
.Ltmp380:
	.cfi_offset %rbx, -24
.Ltmp381:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$3, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
                                        # kill: RDI<def> RBX<kill>
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB39_1
.LBB39_2:                               # %do_diagnostic.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB39_1:                               # %if.then.i
	movq	160(%rbx), %rax
	movq	stderr(%rip), %r14
	movq	8(%rax), %rdx
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movzbl	15(%rbx), %eax
	incq	%rax
	movb	%al, 15(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	cpp_output_line
	movzbl	15(%rbx), %eax
	decq	%rax
	movb	%al, 15(%rbx)
	jmp	.LBB39_2
.Ltmp382:
	.size	do_error, .Ltmp382-do_error
	.cfi_endproc

	.align	16, 0x90
	.type	do_pragma,@function
do_pragma:                              # @do_pragma
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp388:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp389:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp390:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp391:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp392:
	.cfi_def_cfa_offset 48
.Ltmp393:
	.cfi_offset %rbx, -40
.Ltmp394:
	.cfi_offset %r12, -32
.Ltmp395:
	.cfi_offset %r14, -24
.Ltmp396:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	15(%r12), %eax
	incq	%rax
	movb	%al, 15(%r12)
                                        # kill: RDI<def> R12<kill>
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	xorq	%r15, %r15
	cmpq	$56, %rcx
	movq	$1, %r14
	jne	.LBB40_13
# BB#1:                                 # %if.then
	movq	576(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB40_13
# BB#2:
	movq	8(%rax), %rcx
	.align	16, 0x90
.LBB40_3:                               # %land.rhs.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdx
	cmpq	%rcx, %rdx
	je	.LBB40_5
# BB#4:                                 # %while.body.i
                                        #   in Loop: Header=BB40_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB40_3
	jmp	.LBB40_13
.LBB40_5:                               # %land.lhs.true
	movl	16(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB40_6
# BB#7:                                 # %if.then3
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$56, %rcx
	movq	$2, %r14
	jne	.LBB40_13
# BB#8:                                 # %if.then9
	movq	24(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB40_13
# BB#9:
	movq	8(%rax), %rdx
	.align	16, 0x90
.LBB40_10:                              # %land.rhs.i58
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rsi
	cmpq	%rdx, %rsi
	je	.LBB40_11
# BB#12:                                # %while.body.i61
                                        #   in Loop: Header=BB40_10 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB40_10
	jmp	.LBB40_13
.LBB40_6:
	movq	%rbx, %r15
	jmp	.LBB40_13
.LBB40_11:
	movq	%rcx, %r15
.LBB40_13:                              # %if.end14
	movq	584(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB40_15
# BB#14:                                # %if.then16
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	*%rcx
.LBB40_15:                              # %if.end19
	testq	%r15, %r15
	je	.LBB40_17
# BB#16:                                # %if.then21
	movq	%r12, %rdi
	callq	*24(%r15)
	jmp	.LBB40_19
.LBB40_17:                              # %if.else23
	cmpq	$0, 632(%r12)
	je	.LBB40_19
# BB#18:                                # %if.then26
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	_cpp_backup_tokens
	movl	68(%r12), %esi
	movq	%r12, %rdi
	callq	*632(%r12)
.LBB40_19:                              # %if.end30
	movzbl	15(%r12), %eax
	decq	%rax
	movb	%al, 15(%r12)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp397:
	.size	do_pragma, .Ltmp397-do_pragma
	.cfi_endproc

	.align	16, 0x90
	.type	do_warning,@function
do_warning:                             # @do_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp401:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp402:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp403:
	.cfi_def_cfa_offset 32
.Ltmp404:
	.cfi_offset %rbx, -24
.Ltmp405:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
                                        # kill: RDI<def> RBX<kill>
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB41_1
.LBB41_2:                               # %do_diagnostic.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB41_1:                               # %if.then.i
	movq	160(%rbx), %rax
	movq	stderr(%rip), %r14
	movq	8(%rax), %rdx
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movzbl	15(%rbx), %eax
	incq	%rax
	movb	%al, 15(%rbx)
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	cpp_output_line
	movzbl	15(%rbx), %eax
	decq	%rax
	movb	%al, 15(%rbx)
	jmp	.LBB41_2
.Ltmp406:
	.size	do_warning, .Ltmp406-do_warning
	.cfi_endproc

	.align	16, 0x90
	.type	do_include_next,@function
do_include_next:                        # @do_include_next
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rsi
	jmp	do_include_common  # TAILCALL
.Ltmp407:
	.size	do_include_next, .Ltmp407-do_include_next
	.cfi_endproc

	.align	16, 0x90
	.type	do_ident,@function
do_ident:                               # @do_ident
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp410:
	.cfi_def_cfa_offset 16
.Ltmp411:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB43_1
# BB#2:                                 # %if.else
	movq	624(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB43_4
# BB#3:                                 # %if.then1
	movl	68(%rbx), %esi
	addq	$8, %rax
	movq	%rbx, %rdi
	movq	%rax, %rdx
	callq	*%rcx
	jmp	.LBB43_4
.LBB43_1:                               # %if.then
	movabsq	$.L.str61, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_error
.LBB43_4:                               # %if.end5
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB43_6
# BB#5:                                 # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB43_7
.LBB43_6:                               # %check_eol.exit
	popq	%rbx
	retq
.LBB43_7:                               # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	popq	%rbx
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp412:
	.size	do_ident, .Ltmp412-do_ident
	.cfi_endproc

	.align	16, 0x90
	.type	do_import,@function
do_import:                              # @do_import
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$2, %rsi
	jmp	do_include_common  # TAILCALL
.Ltmp413:
	.size	do_import, .Ltmp413-do_import
	.cfi_endproc

	.align	16, 0x90
	.type	do_assert,@function
do_assert:                              # @do_assert
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp420:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp421:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp422:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp423:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp424:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp425:
	.cfi_def_cfa_offset 80
.Ltmp426:
	.cfi_offset %rbx, -48
.Ltmp427:
	.cfi_offset %r12, -40
.Ltmp428:
	.cfi_offset %r13, -32
.Ltmp429:
	.cfi_offset %r14, -24
.Ltmp430:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	leaq	24(%rsp), %rsi
	movq	$16, %rdx
                                        # kill: RDI<def> RBX<kill>
	callq	parse_assertion
	testq	%rax, %rax
	je	.LBB45_15
# BB#1:                                 # %if.then
	movq	24(%rsp), %r13
	movq	$0, (%r13)
	movzbl	20(%rax), %ecx
	cmpq	$2, %rcx
	jne	.LBB45_13
# BB#2:                                 # %if.then1
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	24(%rax), %rbx
	leaq	24(%rax), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB45_12
# BB#3:                                 # %for.body.lr.ph.i
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	16(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB45_4:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB45_9 Depth 2
	movl	8(%rbx), %eax
	movl	8(%r13), %ecx
	cmpq	%rcx, %rax
	jne	.LBB45_11
# BB#5:                                 # %for.cond2.preheader.i
                                        #   in Loop: Header=BB45_4 Depth=1
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB45_6
	.align	16, 0x90
.LBB45_9:                               # %for.body5.i
                                        #   Parent Loop BB45_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	andq	%r12, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	leaq	16(%rbx,%rax), %rdi
	leaq	16(%r13,%rax), %rsi
	callq	_cpp_equiv_tokens
	movl	8(%rbx), %ecx
	testq	%r12, %rax
	je	.LBB45_10
# BB#8:                                 # %for.cond2.i
                                        #   in Loop: Header=BB45_9 Depth=2
	incq	%r15
	movq	%rcx, %rax
	andq	%r12, %rax
	movq	%r15, %rdx
	andq	%r12, %rdx
	cmpq	%rax, %rdx
	jb	.LBB45_9
.LBB45_10:                              # %for.end.i
                                        #   in Loop: Header=BB45_4 Depth=1
	movq	(%r14), %rbx
	andq	%r12, %rcx
	andq	%r12, %r15
	cmpq	%rcx, %r15
	je	.LBB45_6
.LBB45_11:                              # %for.inc16.i
                                        #   in Loop: Header=BB45_4 Depth=1
	movq	%rbx, %r14
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_4
	jmp	.LBB45_12
.LBB45_6:                               # %find_answer.exit
	testq	%rbx, %rbx
	je	.LBB45_12
# BB#7:                                 # %if.then4
	movq	(%rsp), %rax            # 8-byte Reload
	movq	8(%rax), %rdx
	incq	%rdx
	movabsq	$.L.str57, %rsi
	xorq	%rax, %rax
	movq	8(%rsp), %rdi           # 8-byte Reload
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_warning  # TAILCALL
.LBB45_12:                              # %if.end
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, (%r13)
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	(%rsp), %rax            # 8-byte Reload
.LBB45_13:                              # %if.end6
	movq	$2, %rcx
	movb	%cl, 20(%rax)
	movq	%r13, 24(%rax)
	movq	72(%rbx), %rax
	movl	8(%r13), %ecx
	decq	%rcx
	movq	16(%rax), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	leaq	(%rsi,%rsi,2), %rcx
	leaq	40(%rdx,%rcx,8), %rcx
	movq	%rcx, 16(%rax)
	movq	192(%rbx), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB45_15
# BB#14:                                # %land.lhs.true.i
	movq	%rbx, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB45_16
.LBB45_15:                              # %if.end11
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB45_16:                              # %if.then.i
	movq	160(%rbx), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp431:
	.size	do_assert, .Ltmp431-do_assert
	.cfi_endproc

	.align	16, 0x90
	.type	do_unassert,@function
do_unassert:                            # @do_unassert
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
	subq	$32, %rsp
.Ltmp443:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, %r14
	leaq	24(%rsp), %rsi
	movq	$17, %rdx
                                        # kill: RDI<def> R14<kill>
	callq	parse_assertion
	testq	%rax, %rax
	je	.LBB46_19
# BB#1:                                 # %land.lhs.true
	movzbl	20(%rax), %ecx
	cmpq	$2, %rcx
	jne	.LBB46_19
# BB#2:                                 # %if.then
	movq	24(%rsp), %r13
	testq	%r13, %r13
	je	.LBB46_18
# BB#3:                                 # %if.then2
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB46_14
# BB#4:                                 # %for.body.lr.ph.i
	movq	%r14, 16(%rsp)          # 8-byte Spill
	leaq	24(%rax), %r15
	movq	%r15, (%rsp)            # 8-byte Spill
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB46_5:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB46_10 Depth 2
	movl	8(%rbx), %eax
	movl	8(%r13), %ecx
	cmpq	%rcx, %rax
	jne	.LBB46_12
# BB#6:                                 # %for.cond2.preheader.i
                                        #   in Loop: Header=BB46_5 Depth=1
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB46_7
	.align	16, 0x90
.LBB46_10:                              # %for.body5.i
                                        #   Parent Loop BB46_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %rax
	andq	%r14, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	leaq	16(%rbx,%rax), %rdi
	leaq	16(%r13,%rax), %rsi
	callq	_cpp_equiv_tokens
	movl	8(%rbx), %ecx
	testq	%r14, %rax
	je	.LBB46_11
# BB#9:                                 # %for.cond2.i
                                        #   in Loop: Header=BB46_10 Depth=2
	incq	%r12
	movq	%rcx, %rax
	andq	%r14, %rax
	movq	%r12, %rdx
	andq	%r14, %rdx
	cmpq	%rax, %rdx
	jb	.LBB46_10
.LBB46_11:                              # %for.end.i
                                        #   in Loop: Header=BB46_5 Depth=1
	movq	(%r15), %rbx
	andq	%r14, %rcx
	andq	%r14, %r12
	cmpq	%rcx, %r12
	je	.LBB46_7
.LBB46_12:                              # %for.inc16.i
                                        #   in Loop: Header=BB46_5 Depth=1
	movq	%rbx, %r15
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB46_5
	jmp	.LBB46_13
.LBB46_18:                              # %if.else
	movq	%rax, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	_cpp_free_definition  # TAILCALL
.LBB46_7:                               # %find_answer.exit
	testq	%rbx, %rbx
	je	.LBB46_13
# BB#8:                                 # %if.then5
	movq	(%rbx), %rax
	movq	%rax, (%r15)
.LBB46_13:                              # %if.end
	movq	(%rsp), %rax            # 8-byte Reload
	cmpq	$0, (%rax)
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB46_15
.LBB46_14:                              # %if.then7
	xorq	%rcx, %rcx
	movb	%cl, 20(%rax)
.LBB46_15:                              # %if.end9
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB46_19
# BB#16:                                # %land.lhs.true.i
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB46_17
.LBB46_19:                              # %if.end11
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB46_17:                              # %if.then.i
	movq	160(%r14), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_pedwarn  # TAILCALL
.Ltmp449:
	.size	do_unassert, .Ltmp449-do_unassert
	.cfi_endproc

	.align	16, 0x90
	.type	do_sccs,@function
do_sccs:                                # @do_sccs
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp450:
	.size	do_sccs, .Ltmp450-do_sccs
	.cfi_endproc

	.align	16, 0x90
	.type	do_include_common,@function
do_include_common:                      # @do_include_common
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp456:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp457:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp458:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp459:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp460:
	.cfi_def_cfa_offset 48
.Ltmp461:
	.cfi_offset %rbx, -40
.Ltmp462:
	.cfi_offset %r12, -32
.Ltmp463:
	.cfi_offset %r14, -24
.Ltmp464:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jne	.LBB48_1
# BB#4:                                 # %land.lhs.true2
	movzbl	760(%r12), %eax
	testq	%rax, %rax
	movq	$2, %r14
	je	.LBB48_7
# BB#5:                                 # %if.then4
	xorq	%rax, %rax
	movb	%al, 760(%r12)
	movabsq	$.L.str59, %rsi
	xorq	%rax, %rax
	jmp	.LBB48_6
.LBB48_1:                               # %entry
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB48_7
# BB#2:                                 # %land.lhs.true
	movq	(%r12), %rax
	cmpq	$0, 32(%rax)
	movq	$1, %r14
	jne	.LBB48_7
# BB#3:                                 # %if.then
	movabsq	$.L.str58, %rsi
	xorq	%rax, %rax
	xorq	%r14, %r14
.LBB48_6:                               # %if.end7
	movq	%r12, %rdi
	callq	cpp_warning
.LBB48_7:                               # %if.end7
	movq	%r12, %rdi
	callq	parse_include
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB48_21
# BB#8:                                 # %if.then9
	movl	44(%r12), %eax
	cmpq	$200, %rax
	jb	.LBB48_9
# BB#22:                                # %if.then12
	movabsq	$.L.str60, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	cpp_fatal  # TAILCALL
.LBB48_21:                              # %if.end21
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB48_9:                               # %if.else13
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	cmpq	$67, %rax
	je	.LBB48_12
# BB#10:                                # %land.lhs.true.i
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB48_12
# BB#11:                                # %if.then.i
	movq	160(%r12), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str25, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
.LBB48_12:                              # %check_eol.exit
	movq	152(%r12), %rax
	leaq	96(%r12), %rbx
	jmp	.LBB48_14
	.align	16, 0x90
.LBB48_13:                              # %while.body.i
                                        #   in Loop: Header=BB48_14 Depth=1
	movq	%r12, %rdi
	callq	_cpp_pop_context
	movq	152(%r12), %rax
.LBB48_14:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rax
	jne	.LBB48_13
# BB#15:                                # %while.end.i
	movq	192(%r12), %rax
	movzbl	-18(%rax), %eax
	jmp	.LBB48_17
	.align	16, 0x90
.LBB48_16:                              # %while.cond2.i
                                        #   in Loop: Header=BB48_17 Depth=1
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
.LBB48_17:                              # %while.cond2.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$67, %rax
	jne	.LBB48_16
# BB#18:                                # %skip_rest_of_line.exit
	movq	600(%r12), %rax
	testq	%rax, %rax
	je	.LBB48_20
# BB#19:                                # %if.then15
	movq	160(%r12), %rcx
	movq	8(%rcx), %rdx
	movl	68(%r12), %esi
	movq	%r12, %rdi
	movq	%r15, %rcx
	callq	*%rax
.LBB48_20:                              # %if.end18
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	_cpp_execute_include  # TAILCALL
.Ltmp465:
	.size	do_include_common, .Ltmp465-do_include_common
	.cfi_endproc

	.align	16, 0x90
	.type	lex_macro_node,@function
lex_macro_node:                         # @lex_macro_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp469:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp470:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp471:
	.cfi_def_cfa_offset 32
.Ltmp472:
	.cfi_offset %rbx, -24
.Ltmp473:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$56, %rdx
	jne	.LBB49_1
# BB#6:                                 # %if.end10
	movq	8(%rax), %rax
	movzbl	21(%rax), %ecx
	xorq	%rbx, %rbx
	testq	$2, %rcx
	jne	.LBB49_11
# BB#7:                                 # %if.end18
	movq	784(%r14), %rcx
	cmpq	%rcx, %rax
	je	.LBB49_9
# BB#8:
	movq	%rax, %rbx
	jmp	.LBB49_11
.LBB49_1:                               # %entry
	cmpq	$67, %rdx
	jne	.LBB49_3
# BB#2:                                 # %if.then5
	movq	160(%r14), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str68, %rsi
	jmp	.LBB49_10
.LBB49_3:                               # %if.else
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB49_5
# BB#4:                                 # %if.then6
	movq	8(%rax), %rax
	movq	8(%rax), %rdx
	movabsq	$.L.str69, %rsi
	jmp	.LBB49_10
.LBB49_5:                               # %if.else8
	movabsq	$.L.str70, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	cpp_error
	jmp	.LBB49_11
.LBB49_9:                               # %if.then21
	movq	8(%rax), %rdx
	movabsq	$.L.str71, %rsi
.LBB49_10:                              # %return
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	cpp_error
.LBB49_11:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp474:
	.size	lex_macro_node, .Ltmp474-lex_macro_node
	.cfi_endproc

	.align	16, 0x90
	.type	push_conditional,@function
push_conditional:                       # @push_conditional
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp481:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp482:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp483:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp484:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp485:
	.cfi_def_cfa_offset 48
.Ltmp486:
	.cfi_offset %rbx, -48
.Ltmp487:
	.cfi_offset %r12, -40
.Ltmp488:
	.cfi_offset %r13, -32
.Ltmp489:
	.cfi_offset %r14, -24
.Ltmp490:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	512(%rbx), %rdi
	movq	520(%rbx), %rax
	movq	(%rbx), %r13
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r14
	subq	%rdi, %rax
	cmpq	$31, %rax
	jg	.LBB50_2
# BB#1:                                 # %if.then
	leaq	488(%rbx), %rdi
	movq	$32, %rsi
	callq	_obstack_newchunk
	movq	512(%rbx), %rdi
.LBB50_2:                               # %if.end
	addq	$32, %rdi
	movq	%rdi, 512(%rbx)
	movq	504(%rbx), %rax
	cmpq	%rax, %rdi
	jne	.LBB50_4
# BB#3:                                 # %if.then9
	movzbl	568(%rbx), %ecx
	orq	$2, %rcx
	movb	%cl, 568(%rbx)
.LBB50_4:                               # %if.end10
	movslq	536(%rbx), %rcx
	addq	%rcx, %rdi
	notq	%rcx
	andq	%rdi, %rcx
	movq	%rcx, 512(%rbx)
	movq	520(%rbx), %r8
	movq	496(%rbx), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB50_6
# BB#5:                                 # %if.then30
	movq	%r8, 512(%rbx)
	movq	%r8, %rcx
.LBB50_6:                               # %if.end33
	movq	%rcx, 504(%rbx)
	movl	68(%rbx), %ecx
	movl	%ecx, 8(%rax)
	movq	56(%r13), %rcx
	movq	%rcx, (%rax)
	movzbl	9(%rbx), %edi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	movabsq	$0, %rcx
	movabsq	$1, %rsi
	movq	%rcx, %rdx
	jne	.LBB50_8
# BB#7:                                 # %if.end33
	movq	%rsi, %rdx
.LBB50_8:                               # %if.end33
	testq	%rdi, %rdi
	je	.LBB50_10
# BB#9:                                 # %if.end33
	movq	%rsi, %rcx
.LBB50_10:                              # %if.end33
	orq	%rcx, %rdx
	movb	%dl, 24(%rax)
	movb	%cl, 25(%rax)
	movl	%r12d, 28(%rax)
	movzbl	184(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB50_13
# BB#11:                                # %land.lhs.true
	cmpq	$0, 168(%rbx)
	je	.LBB50_12
.LBB50_13:                              # %if.else
	movq	$0, 16(%rax)
	jmp	.LBB50_14
.LBB50_12:                              # %if.then48
	movq	%r15, 16(%rax)
.LBB50_14:                              # %if.end51
	movb	%r14b, 9(%rbx)
	movq	%rax, 56(%r13)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp491:
	.size	push_conditional, .Ltmp491-push_conditional
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"style of line directive is a GCC extension"
	.size	.L.str, 43

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"invalid preprocessing directive #%s"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"registering \"%s\" as both a pragma and a pragma namespace"
	.size	.L.str2, 57

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"#pragma %s %s is already registered"
	.size	.L.str3, 36

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"#pragma %s is already registered"
	.size	.L.str4, 33

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"poison"
	.size	.L.str5, 7

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"once"
	.size	.L.str6, 5

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"GCC"
	.size	.L.str7, 4

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"system_header"
	.size	.L.str8, 14

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"dependency"
	.size	.L.str9, 11

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"_Pragma takes a parenthesized string literal"
	.size	.L.str10, 45

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"unterminated #%s"
	.size	.L.str11, 17

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"assertion without predicate"
	.size	.L.str12, 28

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"predicate must be an identifier"
	.size	.L.str13, 32

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"missing '(' after predicate"
	.size	.L.str14, 28

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"missing ')' to complete answer"
	.size	.L.str15, 31

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"predicate's answer is empty"
	.size	.L.str16, 28

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"cannot find source %s"
	.size	.L.str17, 22

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"current file is older than %s"
	.size	.L.str18, 30

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"#%s "
	.size	.L.str19, 5

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"pragma dependency"
	.size	.L.str20, 18

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"#%s expects \"FILENAME\" or <FILENAME>"
	.size	.L.str21, 37

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"empty file name in #%s"
	.size	.L.str22, 23

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"missing terminating > character"
	.size	.L.str23, 32

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"#pragma system_header ignored outside include file"
	.size	.L.str24, 51

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"extra tokens at end of #%s directive"
	.size	.L.str25, 37

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"invalid #pragma GCC poison directive"
	.size	.L.str26, 37

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"poisoning existing macro \"%s\""
	.size	.L.str27, 30

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"#pragma once is obsolete"
	.size	.L.str28, 25

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"#pragma once in main file"
	.size	.L.str29, 26

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"#%s is a GCC extension"
	.size	.L.str30, 23

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"suggest not using #elif in traditional C"
	.size	.L.str31, 41

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"traditional C ignores #%s with the # indented"
	.size	.L.str32, 46

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"suggest hiding #%s from traditional C with an indented #"
	.size	.L.str33, 57

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"#"
	.size	.L.str34, 2

	.type	linemarker_dir,@object  # @linemarker_dir
	.section	.rodata,"a",@progbits
	.align	8
linemarker_dir:
	.quad	do_linemarker
	.quad	.L.str34
	.short	1                       # 0x1
	.byte	0                       # 0x0
	.byte	8                       # 0x8
	.zero	4
	.size	linemarker_dir, 24

	.type	.L.str35,@object        # @.str35
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str35:
	.asciz	"\"%s\" after # is not a positive integer"
	.size	.L.str35, 39

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"\"%s\" is not a valid filename"
	.size	.L.str36, 29

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"invalid flag \"%s\" in line directive"
	.size	.L.str37, 36

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"define"
	.size	.L.str38, 7

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"include"
	.size	.L.str39, 8

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"endif"
	.size	.L.str40, 6

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"ifdef"
	.size	.L.str41, 6

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"if"
	.size	.L.str42, 3

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"else"
	.size	.L.str43, 5

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"ifndef"
	.size	.L.str44, 7

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"undef"
	.size	.L.str45, 6

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"line"
	.size	.L.str46, 5

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"elif"
	.size	.L.str47, 5

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"error"
	.size	.L.str48, 6

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"pragma"
	.size	.L.str49, 7

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"warning"
	.size	.L.str50, 8

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"include_next"
	.size	.L.str51, 13

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"ident"
	.size	.L.str52, 6

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"import"
	.size	.L.str53, 7

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"assert"
	.size	.L.str54, 7

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"unassert"
	.size	.L.str55, 9

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"sccs"
	.size	.L.str56, 5

	.type	dtable,@object          # @dtable
	.section	.rodata,"a",@progbits
	.align	16
dtable:
	.quad	do_define
	.quad	.L.str38
	.short	6                       # 0x6
	.byte	0                       # 0x0
	.byte	8                       # 0x8
	.zero	4
	.quad	do_include
	.quad	.L.str39
	.short	7                       # 0x7
	.byte	0                       # 0x0
	.byte	4                       # 0x4
	.zero	4
	.quad	do_endif
	.quad	.L.str40
	.short	5                       # 0x5
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.zero	4
	.quad	do_ifdef
	.quad	.L.str41
	.short	5                       # 0x5
	.byte	0                       # 0x0
	.byte	3                       # 0x3
	.zero	4
	.quad	do_if
	.quad	.L.str42
	.short	2                       # 0x2
	.byte	0                       # 0x0
	.byte	3                       # 0x3
	.zero	4
	.quad	do_else
	.quad	.L.str43
	.short	4                       # 0x4
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.zero	4
	.quad	do_ifndef
	.quad	.L.str44
	.short	6                       # 0x6
	.byte	0                       # 0x0
	.byte	3                       # 0x3
	.zero	4
	.quad	do_undef
	.quad	.L.str45
	.short	5                       # 0x5
	.byte	0                       # 0x0
	.byte	8                       # 0x8
	.zero	4
	.quad	do_line
	.quad	.L.str46
	.short	4                       # 0x4
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.zero	4
	.quad	do_elif
	.quad	.L.str47
	.short	4                       # 0x4
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.zero	4
	.quad	do_error
	.quad	.L.str48
	.short	5                       # 0x5
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.zero	4
	.quad	do_pragma
	.quad	.L.str49
	.short	6                       # 0x6
	.byte	1                       # 0x1
	.byte	8                       # 0x8
	.zero	4
	.quad	do_warning
	.quad	.L.str50
	.short	7                       # 0x7
	.byte	2                       # 0x2
	.byte	0                       # 0x0
	.zero	4
	.quad	do_include_next
	.quad	.L.str51
	.short	12                      # 0xc
	.byte	2                       # 0x2
	.byte	4                       # 0x4
	.zero	4
	.quad	do_ident
	.quad	.L.str52
	.short	5                       # 0x5
	.byte	2                       # 0x2
	.byte	8                       # 0x8
	.zero	4
	.quad	do_import
	.quad	.L.str53
	.short	6                       # 0x6
	.byte	2                       # 0x2
	.byte	4                       # 0x4
	.zero	4
	.quad	do_assert
	.quad	.L.str54
	.short	6                       # 0x6
	.byte	2                       # 0x2
	.byte	0                       # 0x0
	.zero	4
	.quad	do_unassert
	.quad	.L.str55
	.short	8                       # 0x8
	.byte	2                       # 0x2
	.byte	0                       # 0x0
	.zero	4
	.quad	do_sccs
	.quad	.L.str56
	.short	4                       # 0x4
	.byte	2                       # 0x2
	.byte	0                       # 0x0
	.zero	4
	.size	dtable, 456

	.type	.L.str57,@object        # @.str57
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str57:
	.asciz	"\"%s\" re-asserted"
	.size	.L.str57, 17

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"#include_next in primary source file"
	.size	.L.str58, 37

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"#import is obsolete, use an #ifndef wrapper in the header file"
	.size	.L.str59, 63

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"#include nested too deeply"
	.size	.L.str60, 27

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"invalid #ident directive"
	.size	.L.str61, 25

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"#elif without #if"
	.size	.L.str62, 18

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"#elif after #else"
	.size	.L.str63, 18

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"the conditional began here"
	.size	.L.str64, 27

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"\"%s\" after #line is not a positive integer"
	.size	.L.str65, 43

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"line number out of range"
	.size	.L.str66, 25

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"undefining \"%s\""
	.size	.L.str67, 16

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"no macro name given in #%s directive"
	.size	.L.str68, 37

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"\"%s\" cannot be used as a macro name as it is an operator in C++"
	.size	.L.str69, 64

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"macro names must be identifiers"
	.size	.L.str70, 32

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"\"%s\" cannot be used as a macro name"
	.size	.L.str71, 36

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"#else without #if"
	.size	.L.str72, 18

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"#else after #else"
	.size	.L.str73, 18

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"#endif without #if"
	.size	.L.str74, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
