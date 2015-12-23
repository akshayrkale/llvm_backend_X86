	.file	"../../mytest/gcc/c-common.c.bc"
	.text
	.globl	c_expand_start_cond
	.align	16, 0x90
	.type	c_expand_start_cond,@function
c_expand_start_cond:                    # @c_expand_start_cond
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
	movl	if_stack_space(%rip), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB0_1
# BB#2:                                 # %if.else
	movl	if_stack_pointer(%rip), %ecx
	cmpq	%rcx, %rax
	jne	.LBB0_5
# BB#3:                                 # %if.then2
	addq	$10, %rax
	movl	%eax, if_stack_space(%rip)
	movq	if_stack(%rip), %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	%rax, %rsi
	callq	xrealloc
	jmp	.LBB0_4
.LBB0_1:                                # %if.then
	movq	$10, %rax
	movl	%eax, if_stack_space(%rip)
	movq	$320, %rdi              # imm = 0x140
	callq	xmalloc
.LBB0_4:                                # %if.end4
	movq	%rax, if_stack(%rip)
.LBB0_5:                                # %if.end4
	movq	%r15, 32(%rbx)
	movq	%rbx, %rdi
	callq	add_stmt
	movslq	if_stack_pointer(%rip), %rax
	movq	if_stack(%rip), %rdx
	movq	$5, %rcx
	shlq	%cl, %rax
	movl	%r14d, (%rdx,%rax)
	movslq	if_stack_pointer(%rip), %rax
	movq	input_filename(%rip), %rsi
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	%rsi, 8(%rdx,%rax)
	movl	lineno(%rip), %ecx
	movq	if_stack(%rip), %rdx
	movl	%ecx, 4(%rdx,%rax)
	movslq	if_stack_pointer(%rip), %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rdx,%rax)
	movslq	if_stack_pointer(%rip), %rax
	movq	$5, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	%rbx, 24(%rdx,%rsi)
	incq	%rax
	movl	%eax, if_stack_pointer(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	c_expand_start_cond, .Ltmp10-c_expand_start_cond
	.cfi_endproc

	.globl	c_finish_then
	.align	16, 0x90
	.type	c_finish_then,@function
c_finish_then:                          # @c_finish_then
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 16
.Ltmp14:
	.cfi_offset %rbx, -16
	movl	if_stack_pointer(%rip), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	if_stack(%rip), %rdx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	24(%rdx,%rax), %rbx
	movq	(%rbx), %rax
	movq	%rax, 40(%rbx)
	movq	$0, (%rbx)
	callq	current_stmt_tree
	movq	%rbx, (%rax)
	popq	%rbx
	retq
.Ltmp15:
	.size	c_finish_then, .Ltmp15-c_finish_then
	.cfi_endproc

	.globl	c_expand_end_cond
	.align	16, 0x90
	.type	c_expand_end_cond,@function
c_expand_end_cond:                      # @c_expand_end_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp17:
	.cfi_def_cfa_offset 16
	movl	if_stack_pointer(%rip), %eax
	decq	%rax
	movl	%eax, if_stack_pointer(%rip)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	if_stack(%rip), %rdx
	sarq	%cl, %rax
	movq	$5, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movl	16(%rdx,%rsi), %ecx
	testq	%rcx, %rcx
	je	.LBB2_2
# BB#1:                                 # %if.then
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	8(%rdx,%rax), %rdi
	movl	4(%rdx,%rax), %esi
	movabsq	$.L.str, %rdx
	xorq	%rax, %rax
	callq	warning_with_file_and_line
.LBB2_2:                                # %if.end
	callq	current_stmt_tree
	movq	$0, 8(%rax)
	popq	%rax
	retq
.Ltmp18:
	.size	c_expand_end_cond, .Ltmp18-c_expand_end_cond
	.cfi_endproc

	.globl	c_expand_start_else
	.align	16, 0x90
	.type	c_expand_start_else,@function
c_expand_start_else:                    # @c_expand_start_else
	.cfi_startproc
# BB#0:                                 # %entry
	movl	warn_parentheses(%rip), %ecx
	movl	if_stack_pointer(%rip), %eax
	testq	%rcx, %rcx
	je	.LBB3_2
# BB#1:                                 # %entry
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %rdx
	jle	.LBB3_2
# BB#3:                                 # %land.lhs.true1
	leaq	-1(%rax), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	if_stack(%rip), %rdx
	movq	$5, %rcx
	shlq	%cl, %rsi
	movl	(%rdx,%rsi), %r8d
	leaq	-2(%rax), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$5, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movl	(%rdx,%rsi), %ecx
	cmpq	%rcx, %r8
	jne	.LBB3_5
# BB#4:                                 # %if.then
	movq	$5, %rcx
	shlq	%cl, %rdi
	movq	$1, %rax
	movl	%eax, 16(%rdx,%rdi)
	movl	if_stack_pointer(%rip), %eax
	jmp	.LBB3_5
.LBB3_2:                                # %entry.if.end_crit_edge
	movq	if_stack(%rip), %rdx
.LBB3_5:                                # %if.end
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	xorq	%rcx, %rcx
	movl	%ecx, 16(%rdx,%rax)
	movl	if_stack_pointer(%rip), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movl	(%rdx,%rax), %ecx
	decq	%rcx
	movl	%ecx, (%rdx,%rax)
	retq
.Ltmp19:
	.size	c_expand_start_else, .Ltmp19-c_expand_start_else
	.cfi_endproc

	.globl	c_finish_else
	.align	16, 0x90
	.type	c_finish_else,@function
c_finish_else:                          # @c_finish_else
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 16
.Ltmp23:
	.cfi_offset %rbx, -16
	movl	if_stack_pointer(%rip), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	if_stack(%rip), %rdx
	sarq	%cl, %rax
	movq	$5, %rcx
	shlq	%cl, %rax
	movq	24(%rdx,%rax), %rbx
	movq	(%rbx), %rax
	movq	%rax, 48(%rbx)
	movq	$0, (%rbx)
	callq	current_stmt_tree
	movq	%rbx, (%rax)
	popq	%rbx
	retq
.Ltmp24:
	.size	c_finish_else, .Ltmp24-c_finish_else
	.cfi_endproc

	.globl	c_begin_if_stmt
	.align	16, 0x90
	.type	c_begin_if_stmt,@function
c_begin_if_stmt:                        # @c_begin_if_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$155, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%rax, %rax
	jmp	build_stmt  # TAILCALL
.Ltmp25:
	.size	c_begin_if_stmt, .Ltmp25-c_begin_if_stmt
	.cfi_endproc

	.globl	c_begin_while_stmt
	.align	16, 0x90
	.type	c_begin_while_stmt,@function
c_begin_while_stmt:                     # @c_begin_while_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$157, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	jmp	build_stmt  # TAILCALL
.Ltmp26:
	.size	c_begin_while_stmt, .Ltmp26-c_begin_while_stmt
	.cfi_endproc

	.globl	c_finish_while_stmt_cond
	.align	16, 0x90
	.type	c_finish_while_stmt_cond,@function
c_finish_while_stmt_cond:               # @c_finish_while_stmt_cond
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, 32(%rsi)
	retq
.Ltmp27:
	.size	c_finish_while_stmt_cond, .Ltmp27-c_finish_while_stmt_cond
	.cfi_endproc

	.globl	start_fname_decls
	.align	16, 0x90
	.type	start_fname_decls,@function
start_fname_decls:                      # @start_fname_decls
	.cfi_startproc
# BB#0:                                 # %for.body.lr.ph
	pushq	%r15
.Ltmp34:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp35:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp36:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp37:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 48
.Ltmp39:
	.cfi_offset %rbx, -48
.Ltmp40:
	.cfi_offset %r12, -40
.Ltmp41:
	.cfi_offset %r13, -32
.Ltmp42:
	.cfi_offset %r14, -24
.Ltmp43:
	.cfi_offset %r15, -16
	xorq	%rdi, %rdi
	movabsq	$fname_vars, %r13
	movabsq	$c_global_trees+232, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	xorq	%rbx, %rbx
	xorq	%r14, %r14
	.align	16, 0x90
.LBB8_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB8_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB8_1 Depth=1
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%r12, %rdi
	movq	%rax, %rsi
	movq	%r14, %rdx
	callq	tree_cons
	movq	(%r13), %rcx
	movq	%rax, %r14
	movq	$0, (%rcx)
.LBB8_3:                                # %for.inc
                                        #   in Loop: Header=BB8_1 Depth=1
	incq	%rbx
	movq	%rbx, %rdi
	andq	%r15, %rdi
	movq	$4, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	fname_vars(%rdx), %rax
	leaq	fname_vars(%rdx), %r13
	testq	%rax, %rax
	jne	.LBB8_1
# BB#4:                                 # %for.end
	movq	c_global_trees+240(%rip), %rdx
	movq	%r14, %rax
	orq	%rdx, %rax
	je	.LBB8_6
# BB#5:                                 # %if.then12
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	tree_cons
	movq	%rax, c_global_trees+240(%rip)
.LBB8_6:                                # %if.end14
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp44:
	.size	start_fname_decls, .Ltmp44-start_fname_decls
	.cfi_endproc

	.globl	finish_fname_decls
	.align	16, 0x90
	.type	finish_fname_decls,@function
finish_fname_decls:                     # @finish_fname_decls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp53:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp54:
	.cfi_def_cfa_offset 48
.Ltmp55:
	.cfi_offset %rbx, -40
.Ltmp56:
	.cfi_offset %r12, -32
.Ltmp57:
	.cfi_offset %r14, -24
.Ltmp58:
	.cfi_offset %r15, -16
	movq	c_global_trees+240(%rip), %r12
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB9_1
# BB#2:
	xorq	%rax, %rax
	movabsq	$0, %r14
	.align	16, 0x90
.LBB9_3:                                # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r12), %rdi
	testq	%rdi, %rdi
	movq	$1, %r15
	je	.LBB9_4
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB9_3 Depth=1
	movq	%rax, %rsi
	callq	chainon
	movq	(%r12), %r12
	xorq	%r15, %r15
	testq	%r12, %r12
	jne	.LBB9_3
	jmp	.LBB9_6
.LBB9_1:
	xorq	%r14, %r14
	xorq	%r15, %r15
	jmp	.LBB9_8
.LBB9_4:
	movq	%r12, %r14
.LBB9_6:                                # %for.end
	testq	%rax, %rax
	je	.LBB9_8
# BB#7:                                 # %if.then
	movq	current_function_decl(%rip), %rcx
	movq	168(%rcx), %rcx
	movq	(%rcx), %rsi
	movq	%rax, %rdi
	callq	chainon
	movq	%rax, %rcx
	movq	$153, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	build_stmt
	movl	16(%rax), %ecx
	orq	$16777216, %rcx         # imm = 0x1000000
	movl	%ecx, 16(%rax)
	movq	current_function_decl(%rip), %rcx
	movq	168(%rcx), %rcx
	movq	%rax, (%rcx)
.LBB9_8:                                # %for.body17.lr.ph
	movabsq	$c_global_trees+232, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_9:                                # %for.body17
                                        # =>This Inner Loop Header: Depth=1
	movq	$0, (%rcx)
	incq	%rbx
	movq	%rbx, %rdx
	andq	%rax, %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	fname_vars(%rdx), %rcx
	testq	%rcx, %rcx
	jne	.LBB9_9
# BB#10:                                # %for.end22
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB9_15
# BB#11:                                # %if.then24
	movq	24(%r14), %rax
	jmp	.LBB9_13
	.align	16, 0x90
.LBB9_12:                               # %for.body28
                                        #   in Loop: Header=BB9_13 Depth=1
	movq	32(%rax), %rcx
	movl	32(%rcx), %edx
	movq	24(%rax), %rsi
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	fname_vars(%rdx), %rcx
	movq	%rsi, (%rcx)
	movq	(%rax), %rax
.LBB9_13:                               # %for.body28
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB9_12
# BB#14:                                # %for.end42
	movq	(%r14), %rax
.LBB9_15:                               # %if.end45
	movq	%rax, c_global_trees+240(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp59:
	.size	finish_fname_decls, .Ltmp59-finish_fname_decls
	.cfi_endproc

	.globl	fname_as_string
	.align	16, 0x90
	.type	fname_as_string,@function
fname_as_string:                        # @fname_as_string
	.cfi_startproc
# BB#0:                                 # %entry
	movq	current_function_decl(%rip), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB10_4
# BB#1:                                 # %if.then
	testq	%rcx, %rcx
	je	.LBB10_2
# BB#3:                                 # %cond.true
	movq	decl_printable_name(%rip), %rax
	movq	$2, %rsi
	movq	%rcx, %rdi
	jmpq	*%rax  # TAILCALL
.LBB10_4:                               # %if.else
	movabsq	$.L.str2, %rax
	testq	%rcx, %rcx
	je	.LBB10_7
# BB#5:                                 # %land.lhs.true
	movq	72(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB10_7
# BB#6:                                 # %if.then5
	movq	32(%rcx), %rax
.LBB10_7:                               # %if.end9
	retq
.LBB10_2:
	movabsq	$.L.str1, %rax
	retq
.Ltmp60:
	.size	fname_as_string, .Ltmp60-fname_as_string
	.cfi_endproc

	.globl	fname_string
	.align	16, 0x90
	.type	fname_string,@function
fname_string:                           # @fname_string
	.cfi_startproc
# BB#0:                                 # %for.body.lr.ph
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rdi
	xorq	%rax, %rax
	.align	16, 0x90
.LBB11_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$4, %rcx
	shlq	%cl, %rdx
	movl	fname_vars+8(%rdx), %ecx
	cmpq	%rdi, %rcx
	je	.LBB11_3
# BB#1:                                 # %for.cond
                                        #   in Loop: Header=BB11_2 Depth=1
	incq	%rax
	movq	%rax, %rdx
	andq	%r8, %rdx
	movq	$4, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	cmpq	$0, fname_vars(%rsi)
	jne	.LBB11_2
.LBB11_3:                               # %for.end
	movq	current_function_decl(%rip), %rdi
	andq	%r8, %rax
	cmpq	$2, %rax
	jne	.LBB11_7
# BB#4:                                 # %if.then.i
	testq	%rdi, %rdi
	je	.LBB11_5
# BB#6:                                 # %cond.true.i
	movq	decl_printable_name(%rip), %rax
	movq	$2, %rsi
	jmpq	*%rax  # TAILCALL
.LBB11_7:                               # %if.else.i
	movabsq	$.L.str2, %rax
	testq	%rdi, %rdi
	je	.LBB11_10
# BB#8:                                 # %land.lhs.true.i
	movq	72(%rdi), %rcx
	testq	%rcx, %rcx
	je	.LBB11_10
# BB#9:                                 # %if.then5.i
	movq	32(%rcx), %rax
.LBB11_10:                              # %fname_as_string.exit
	retq
.LBB11_5:
	movabsq	$.L.str1, %rax
	retq
.Ltmp61:
	.size	fname_string, .Ltmp61-fname_string
	.cfi_endproc

	.globl	fname_decl
	.align	16, 0x90
	.type	fname_decl,@function
fname_decl:                             # @fname_decl
	.cfi_startproc
# BB#0:                                 # %for.body.lr.ph
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
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	xorq	%rbx, %rbx
	movabsq	$fname_vars, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rdi
	xorq	%r14, %r14
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movl	fname_vars+8(%rax), %eax
	cmpq	%rdi, %rax
	je	.LBB12_3
# BB#1:                                 # %for.cond
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%r14
	movq	%r14, %rbx
	andq	%r12, %rbx
	movq	$4, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	cmpq	$0, fname_vars(%rax)
	leaq	fname_vars(%rax), %r13
	jne	.LBB12_2
.LBB12_3:                               # %for.end
	movq	(%r13), %rax
	movq	(%rax), %r15
	testq	%r15, %r15
	jne	.LBB12_7
# BB#4:                                 # %if.then9
	callq	current_stmt_tree
	movq	(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rbx
	movl	fname_vars+12(%rbx), %esi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	*make_fname_decl(%rip)
	movq	%rax, %r15
	callq	current_stmt_tree
	movq	(%rax), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rcx, %rax
	je	.LBB12_6
# BB#5:                                 # %if.then16
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$0, (%rbx)
	callq	current_stmt_tree
	movq	%rbx, (%rax)
	movq	c_global_trees+240(%rip), %rdx
	movq	%r15, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	tree_cons
	movq	%rax, c_global_trees+240(%rip)
.LBB12_6:                               # %if.end22
	movq	(%r13), %rax
	movq	%r15, (%rax)
.LBB12_7:                               # %if.end26
	testq	%r12, %r14
	jne	.LBB12_10
# BB#8:                                 # %if.end26
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	jne	.LBB12_10
# BB#9:                                 # %if.then29
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	pedwarn_with_decl
.LBB12_10:                              # %if.end30
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	fname_decl, .Ltmp79-fname_decl
	.cfi_endproc

	.globl	combine_strings
	.align	16, 0x90
	.type	combine_strings,@function
combine_strings:                        # @combine_strings
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp88:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp89:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp91:
	.cfi_def_cfa_offset 112
.Ltmp92:
	.cfi_offset %rbx, -48
.Ltmp93:
	.cfi_offset %r12, -40
.Ltmp94:
	.cfi_offset %r13, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	c_global_trees(%rip), %rax
	movq	%rdi, %r14
	movl	60(%rax), %edx
	movl	flag_isoc99(%rip), %eax
	movq	$3, %rcx
	shrq	%cl, %rdx
	testq	%rax, %rax
	movq	$4095, %r8              # imm = 0xFFF
	jne	.LBB13_2
# BB#1:                                 # %select.mid
	movq	$509, %r8               # imm = 0x1FD
.LBB13_2:                               # %select.end
	movq	%rdx, %rax
	andq	$63, %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	cmpq	$0, (%r14)
	je	.LBB13_30
# BB#3:
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%r8, 32(%rsp)           # 8-byte Spill
	xorq	%r12, %r12
	movq	$1, %r13
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB13_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rcx
	movq	c_global_trees+96(%rip), %rdx
	movl	32(%rbx), %eax
	cmpq	%rdx, %rcx
	jne	.LBB13_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %r12
	addq	%rax, %r12
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB13_9
	.align	16, 0x90
.LBB13_6:                               # %if.else
                                        #   in Loop: Header=BB13_4 Depth=1
	movzbl	19(%rbx), %ecx
	leaq	-1(%r13,%rax), %r13
	testq	$1, %rcx
	je	.LBB13_9
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB13_4 Depth=1
	movl	in_system_header(%rip), %eax
	testq	%rsi, %rax
	jne	.LBB13_9
# BB#8:                                 # %if.then16
                                        #   in Loop: Header=BB13_4 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str4, %rdi
	movq	%rsi, %r15
	callq	warning
	movq	%r15, %rsi
.LBB13_9:                               # %for.inc
                                        #   in Loop: Header=BB13_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_4
# BB#10:                                # %for.end
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rsi, %rax
	movq	%rsi, %rbx
	je	.LBB13_12
# BB#11:                                # %if.then21
	movq	56(%rsp), %rax          # 8-byte Reload
	imulq	%rax, %r13
	addq	%r12, %r13
.LBB13_12:                              # %if.end23
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	xmalloc
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %rcx          # 8-byte Reload
	andq	%rbx, %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	movq	%rax, %r15
	.align	16, 0x90
.LBB13_13:                              # %for.body26
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_20 Depth 2
                                        #       Child Loop BB13_21 Depth 3
	movq	8(%r14), %rax
	movq	c_global_trees+96(%rip), %rcx
	movl	32(%r14), %r13d
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	jne	.LBB13_15
# BB#14:                                # %for.body26
                                        #   in Loop: Header=BB13_13 Depth=1
	movabsq	$1, %rax
.LBB13_15:                              # %for.body26
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	56(%rsp), %rcx          # 8-byte Reload
	je	.LBB13_17
# BB#16:                                # %for.body26
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	$1, %rcx
.LBB13_17:                              # %for.body26
                                        #   in Loop: Header=BB13_13 Depth=1
	subq	%rcx, %r13
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB13_18
# BB#51:                                # %if.then41
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	40(%r14), %rsi
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r15, %rdi
	movq	%r13, %rdx
	callq	memcpy
	addq	%r13, %r15
	jmp	.LBB13_23
	.align	16, 0x90
.LBB13_18:                              # %for.cond45.preheader
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB13_23
# BB#19:                                # %for.body48.lr.ph
                                        #   in Loop: Header=BB13_13 Depth=1
	xorq	%rax, %rax
	.align	16, 0x90
.LBB13_20:                              # %for.body48
                                        #   Parent Loop BB13_13 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB13_21 Depth 3
	movq	40(%r14), %rcx
	movzbl	(%rcx,%rax), %ecx
	movq	c_global_trees(%rip), %rdx
	movb	%cl, (%r15)
	movl	60(%rdx), %edx
	incq	%r15
	movq	$3, %rcx
	shrq	%cl, %rdx
	andq	$63, %rdx
	decq	%rdx
	testq	%rdx, %rdx
	movabsq	$0, %rdx
	jle	.LBB13_22
	.align	16, 0x90
.LBB13_21:                              # %for.body60
                                        #   Parent Loop BB13_13 Depth=1
                                        #     Parent Loop BB13_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movb	%r12b, (%r15)
	incq	%rdx
	movq	c_global_trees(%rip), %rsi
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	movl	60(%rsi), %esi
	sarq	%cl, %rdi
	incq	%r15
	movq	$3, %rcx
	shrq	%cl, %rsi
	andq	$63, %rsi
	decq	%rsi
	cmpq	%rsi, %rdi
	jl	.LBB13_21
.LBB13_22:                              # %for.inc64
                                        #   in Loop: Header=BB13_20 Depth=2
	incq	%rax
	movq	%rax, %rcx
	andq	%rbx, %rcx
	movq	%r13, %rdx
	andq	%rbx, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB13_20
.LBB13_23:                              # %for.inc68
                                        #   in Loop: Header=BB13_13 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB13_13
# BB#24:                                # %for.end71
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB13_31
# BB#25:                                # %for.cond75.preheader
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	movq	%rbx, %r12
	movq	16(%rsp), %r13          # 8-byte Reload
	je	.LBB13_32
# BB#26:                                # %for.body78.lr.ph
	andq	$63, %rdx
	cmpq	$1, %rdx
	ja	.LBB13_27
# BB#28:                                # %for.body78.lr.ph
	movq	$1, %rdx
	jmp	.LBB13_29
.LBB13_30:                              # %if.else86
	movq	8(%r14), %rax
	movq	c_global_trees+96(%rip), %rcx
	movl	32(%r14), %r13d
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jne	.LBB13_34
	jmp	.LBB13_33
.LBB13_31:                              # %if.else83
	movq	%rbx, %r12
	xorq	%rax, %rax
	movb	%al, (%r15)
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB13_32
.LBB13_27:
	decq	%rdx
	andq	%r12, %rdx
	incq	%rdx
.LBB13_29:                              # %for.body78.lr.ph
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	memset
.LBB13_32:                              # %if.end95
	movq	%r13, %rdi
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rsi
	callq	build_string
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	free
	xorq	%r15, %r15
	movq	40(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	movq	32(%rsp), %r8           # 8-byte Reload
	je	.LBB13_34
.LBB13_33:                              # %cond.true97
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r13, %rax
	cqto
	idivq	56(%rsp)                # 8-byte Folded Reload
	movq	$1, %r15
	movq	%rax, %r13
.LBB13_34:                              # %cond.end100
	movl	pedantic(%rip), %eax
	testq	%rax, %rax
	je	.LBB13_40
# BB#35:                                # %land.lhs.true103
	leaq	-1(%r13), %rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	jle	.LBB13_40
# BB#36:                                # %land.lhs.true103
	movl	c_language(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB13_40
# BB#37:                                # %if.then110
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	movq	$99, %rcx
	jne	.LBB13_39
# BB#38:                                # %select.mid6
	movq	$89, %rcx
.LBB13_39:                              # %select.end5
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	movq	%r8, %rdx
	callq	pedwarn
.LBB13_40:                              # %if.end114
	movl	flag_const_strings(%rip), %eax
	testq	%r15, %r15
	jne	.LBB13_41
# BB#42:                                # %if.end114
	movabsq	$integer_types, %rcx
	jmp	.LBB13_43
.LBB13_41:
	movabsq	$c_global_trees, %rcx
.LBB13_43:                              # %if.end114
	movq	(%rcx), %r15
	testq	%rax, %rax
	je	.LBB13_47
# BB#44:                                # %if.end114
	movl	flag_traditional(%rip), %ecx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB13_47
# BB#45:                                # %if.end114
	movl	flag_writable_strings(%rip), %ecx
	testq	%rax, %rcx
	jne	.LBB13_47
# BB#46:                                # %cond.end124
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	build_qualified_type
	movq	%rax, %r15
.LBB13_47:                              # %cond.end138
	decq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	build_int_2_wide
	movq	%rax, %rdi
	callq	build_index_type
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	build_array_type
	movq	%rax, 8(%r14)
	movl	16(%r14), %eax
	orq	$512, %rax              # imm = 0x200
	movl	%eax, 16(%r14)
	movl	flag_writable_strings(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB13_48
# BB#49:                                # %if.end147
	movabsq	$1, %rdx
	jmp	.LBB13_50
.LBB13_48:
	movabsq	$0, %rdx
.LBB13_50:                              # %if.end147
	movq	$12, %rcx
	shlq	%cl, %rdx
	movabsq	$4294701055, %rcx       # imm = 0xFFFBEFFF
	andq	%rcx, %rax
	orq	%rdx, %rax
	orq	$262144, %rax           # imm = 0x40000
	movl	%eax, 16(%r14)
	movq	%r14, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp97:
	.size	combine_strings, .Ltmp97-combine_strings
	.cfi_endproc

	.globl	constant_expression_warning
	.align	16, 0x90
	.type	constant_expression_warning,@function
constant_expression_warning:            # @constant_expression_warning
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-25, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	ja	.LBB14_3
# BB#1:                                 # %land.lhs.true
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB14_3
# BB#2:                                 # %land.lhs.true
	movl	pedantic(%rip), %eax
	testq	%rcx, %rax
	je	.LBB14_3
# BB#4:                                 # %if.then
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	jmp	pedwarn  # TAILCALL
.LBB14_3:                               # %if.end
	retq
.Ltmp98:
	.size	constant_expression_warning, .Ltmp98-constant_expression_warning
	.cfi_endproc

	.globl	overflow_warning
	.align	16, 0x90
	.type	overflow_warning,@function
overflow_warning:                       # @overflow_warning
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	je	.LBB15_4
# BB#1:                                 # %entry
	cmpq	$26, %rcx
	je	.LBB15_14
# BB#2:                                 # %entry
	cmpq	$27, %rcx
	jne	.LBB15_11
# BB#3:                                 # %land.lhs.true
	movq	32(%rdi), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	jne	.LBB15_9
.LBB15_4:                               # %land.lhs.true11
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB15_7
# BB#5:                                 # %if.then
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rcx, %rax
	movl	%eax, 16(%rdi)
	movl	skip_evaluation(%rip), %eax
	testq	%rax, %rax
	jne	.LBB15_16
# BB#6:                                 # %if.then20
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB15_7:                               # %if.else
	cmpq	$26, %rcx
	jne	.LBB15_8
.LBB15_14:                              # %land.lhs.true40
	testq	$524288, %rax           # imm = 0x80000
	jne	.LBB15_15
	jmp	.LBB15_11
.LBB15_8:                               # %if.else
	cmpq	$27, %rcx
	jne	.LBB15_11
.LBB15_9:                               # %land.lhs.true32
	movq	32(%rdi), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$26, %rcx
	jne	.LBB15_11
# BB#10:                                # %land.lhs.true32
	movq	%rax, %rcx
	andq	$524288, %rcx           # imm = 0x80000
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB15_11
.LBB15_15:                              # %if.then47
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rcx, %rax
	movl	%eax, 16(%rdi)
	movl	skip_evaluation(%rip), %eax
	testq	%rax, %rax
	jne	.LBB15_16
# BB#17:                                # %if.then53
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.LBB15_11:                              # %if.else55
	movq	%rax, %rcx
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$524316, %rcx           # imm = 0x8001C
	jne	.LBB15_16
# BB#12:                                # %if.then68
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rcx, %rax
	movl	%eax, 16(%rdi)
	movl	skip_evaluation(%rip), %eax
	testq	%rax, %rax
	je	.LBB15_13
.LBB15_16:                              # %if.end78
	retq
.LBB15_13:                              # %if.then74
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	jmp	warning  # TAILCALL
.Ltmp99:
	.size	overflow_warning, .Ltmp99-overflow_warning
	.cfi_endproc

	.globl	unsigned_conversion_warning
	.align	16, 0x90
	.type	unsigned_conversion_warning,@function
unsigned_conversion_warning:            # @unsigned_conversion_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp104:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp105:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp106:
	.cfi_def_cfa_offset 32
.Ltmp107:
	.cfi_offset %rbx, -32
.Ltmp108:
	.cfi_offset %r14, -24
.Ltmp109:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rdi, %r14
	cmpq	$25, %rax
	jne	.LBB16_10
# BB#1:                                 # %land.lhs.true
	movq	8(%r14), %rsi
	movl	16(%rsi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$6, %rcx
	jne	.LBB16_10
# BB#2:                                 # %land.lhs.true7
	testq	$8192, %rax             # imm = 0x2000
	je	.LBB16_10
# BB#3:                                 # %land.lhs.true7
	movl	skip_evaluation(%rip), %eax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB16_10
# BB#4:                                 # %land.lhs.true15
	movq	%rbx, %rdi
	callq	int_fits_type_p
	testq	%r15, %rax
	jne	.LBB16_10
# BB#5:                                 # %if.then
	movq	8(%r14), %rdi
	callq	signed_type
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	testq	%r15, %rax
	je	.LBB16_6
# BB#8:                                 # %if.else
	movl	warn_conversion(%rip), %eax
	testq	%rax, %rax
	je	.LBB16_10
# BB#9:                                 # %if.then26
	movabsq	$.L.str11, %rdi
	jmp	.LBB16_7
.LBB16_10:                              # %if.end28
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB16_6:                               # %if.then24
	movabsq	$.L.str10, %rdi
.LBB16_7:                               # %if.then24
	xorq	%rax, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	warning  # TAILCALL
.Ltmp110:
	.size	unsigned_conversion_warning, .Ltmp110-unsigned_conversion_warning
	.cfi_endproc

	.globl	signed_type
	.align	16, 0x90
	.type	signed_type,@function
signed_type:                            # @signed_type
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	128(%rax), %rcx
	movq	integer_types+16(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_2
# BB#1:                                 # %entry
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_2
# BB#3:                                 # %if.end
	movq	integer_types+48(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_4
# BB#5:                                 # %if.end6
	movq	integer_types+32(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_6
# BB#7:                                 # %if.end9
	movq	integer_types+64(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_8
# BB#9:                                 # %if.end12
	movq	integer_types+80(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_10
# BB#11:                                # %if.end15
	movq	c_global_trees+80(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_12
# BB#13:                                # %if.end18
	movq	global_trees+80(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_14
# BB#15:                                # %if.end21
	movq	global_trees+72(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_16
# BB#17:                                # %if.end24
	movq	global_trees+64(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_18
# BB#19:                                # %if.end27
	movq	global_trees+56(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_20
# BB#21:                                # %if.end30
	movq	global_trees+48(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB17_22
# BB#24:                                # %if.end33
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	jmp	signed_or_unsigned_type  # TAILCALL
.LBB17_2:                               # %if.then
	movq	integer_types+8(%rip), %rax
	retq
.LBB17_4:                               # %if.then5
	movq	integer_types+40(%rip), %rax
	retq
.LBB17_6:                               # %if.then8
	movq	integer_types+24(%rip), %rax
	retq
.LBB17_8:                               # %if.then11
	movq	integer_types+56(%rip), %rax
	retq
.LBB17_10:                              # %if.then14
	movq	integer_types+72(%rip), %rax
	retq
.LBB17_12:                              # %if.then17
	movq	c_global_trees+72(%rip), %rax
	retq
.LBB17_14:                              # %if.then20
	movq	global_trees+40(%rip), %rax
	retq
.LBB17_16:                              # %if.then23
	movq	global_trees+32(%rip), %rax
	retq
.LBB17_18:                              # %if.then26
	movq	global_trees+24(%rip), %rax
	retq
.LBB17_20:                              # %if.then29
	movq	global_trees+16(%rip), %rax
	retq
.LBB17_22:                              # %if.then32
	movq	global_trees+8(%rip), %rax
	retq
.Ltmp111:
	.size	signed_type, .Ltmp111-signed_type
	.cfi_endproc

	.globl	convert_and_check
	.align	16, 0x90
	.type	convert_and_check,@function
convert_and_check:                      # @convert_and_check
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp121:
	.cfi_def_cfa_offset 48
.Ltmp122:
	.cfi_offset %rbx, -40
.Ltmp123:
	.cfi_offset %r12, -32
.Ltmp124:
	.cfi_offset %r14, -24
.Ltmp125:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R15<kill>
	callq	convert
	movq	%rax, %r14
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$25, %rcx
	jne	.LBB18_13
# BB#1:                                 # %if.then
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB18_12
# BB#2:                                 # %if.then4
	movabsq	$4294443007, %rcx       # imm = 0xFFF7FFFF
	andq	%rax, %rcx
	movl	%ecx, 16(%r14)
	movl	16(%r15), %ecx
	andq	$262144, %rcx           # imm = 0x40000
	movabsq	$4294180863, %rdx       # imm = 0xFFF3FFFF
	andq	%rax, %rdx
	orq	%rcx, %rdx
	movl	%edx, 16(%r14)
	movq	8(%r15), %r8
	movl	16(%rbx), %eax
	movl	16(%r8), %edx
	movq	$13, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	andq	$1, %rdi
	movq	$13, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	cmpq	%rsi, %rdi
	jae	.LBB18_5
# BB#3:                                 # %if.then4
	andq	$255, %rdx
	cmpq	$6, %rdx
	jne	.LBB18_5
# BB#4:                                 # %land.lhs.true36
	movl	60(%rbx), %ecx
	movl	60(%r8), %edx
	xorq	%rcx, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	je	.LBB18_13
.LBB18_5:                               # %if.then47
	movl	pedantic(%rip), %ecx
	andq	$8192, %rax             # imm = 0x2000
	orq	%rcx, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB18_6
# BB#10:                                # %land.lhs.true59
	movl	skip_evaluation(%rip), %eax
	testq	%rax, %rax
	jne	.LBB18_13
	jmp	.LBB18_11
.LBB18_12:                              # %if.else
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	unsigned_conversion_warning
	jmp	.LBB18_13
.LBB18_6:                               # %lor.lhs.false55
	movq	%rbx, %rdi
	callq	unsigned_type
	movzbl	16(%r15), %ecx
	cmpq	$25, %rcx
	jne	.LBB18_8
# BB#7:                                 # %if.then.i
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	int_fits_type_p
	jmp	.LBB18_9
.LBB18_8:                               # %if.end.i
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	convert
	movl	16(%rax), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	notq	%rax
	andq	$1, %rax
.LBB18_9:                               # %constant_fits_type_p.exit
	movl	skip_evaluation(%rip), %ecx
	orq	%rax, %rcx
	testq	%r12, %rcx
	jne	.LBB18_13
.LBB18_11:                              # %if.then61
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB18_13:                              # %if.end64
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp126:
	.size	convert_and_check, .Ltmp126-convert_and_check
	.cfi_endproc

	.globl	unsigned_type
	.align	16, 0x90
	.type	unsigned_type,@function
unsigned_type:                          # @unsigned_type
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	128(%rax), %rcx
	movq	integer_types+8(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_2
# BB#1:                                 # %entry
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_2
# BB#3:                                 # %if.end
	movq	integer_types+40(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_4
# BB#5:                                 # %if.end6
	movq	integer_types+24(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_6
# BB#7:                                 # %if.end9
	movq	integer_types+56(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_8
# BB#9:                                 # %if.end12
	movq	integer_types+72(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_10
# BB#11:                                # %if.end15
	movq	c_global_trees+72(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_12
# BB#13:                                # %if.end18
	movq	global_trees+40(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_14
# BB#15:                                # %if.end21
	movq	global_trees+32(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_16
# BB#17:                                # %if.end24
	movq	global_trees+24(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_18
# BB#19:                                # %if.end27
	movq	global_trees+16(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_20
# BB#21:                                # %if.end30
	movq	global_trees+8(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB19_22
# BB#24:                                # %if.end33
	movq	$1, %rdi
	movq	%rax, %rsi
	jmp	signed_or_unsigned_type  # TAILCALL
.LBB19_2:                               # %if.then
	movq	integer_types+16(%rip), %rax
	retq
.LBB19_4:                               # %if.then5
	movq	integer_types+48(%rip), %rax
	retq
.LBB19_6:                               # %if.then8
	movq	integer_types+32(%rip), %rax
	retq
.LBB19_8:                               # %if.then11
	movq	integer_types+64(%rip), %rax
	retq
.LBB19_10:                              # %if.then14
	movq	integer_types+80(%rip), %rax
	retq
.LBB19_12:                              # %if.then17
	movq	c_global_trees+80(%rip), %rax
	retq
.LBB19_14:                              # %if.then20
	movq	global_trees+80(%rip), %rax
	retq
.LBB19_16:                              # %if.then23
	movq	global_trees+72(%rip), %rax
	retq
.LBB19_18:                              # %if.then26
	movq	global_trees+64(%rip), %rax
	retq
.LBB19_20:                              # %if.then29
	movq	global_trees+56(%rip), %rax
	retq
.LBB19_22:                              # %if.then32
	movq	global_trees+48(%rip), %rax
	retq
.Ltmp127:
	.size	unsigned_type, .Ltmp127-unsigned_type
	.cfi_endproc

	.globl	c_expand_expr_stmt
	.align	16, 0x90
	.type	c_expand_expr_stmt,@function
c_expand_expr_stmt:                     # @c_expand_expr_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp134:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp135:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp136:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp137:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp138:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp139:
	.cfi_def_cfa_offset 96
.Ltmp140:
	.cfi_offset %rbx, -48
.Ltmp141:
	.cfi_offset %r12, -40
.Ltmp142:
	.cfi_offset %r13, -32
.Ltmp143:
	.cfi_offset %r14, -24
.Ltmp144:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %rax
	movl	16(%rax), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$18, %rcx
	jne	.LBB20_4
# BB#1:                                 # %land.lhs.true
	movl	flag_isoc99(%rip), %eax
	testq	%rax, %rax
	jne	.LBB20_5
# BB#2:                                 # %lor.lhs.false
	movq	%r14, %rdi
	callq	lvalue_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB20_5
# BB#3:                                 # %lor.lhs.false.lor.lhs.false3_crit_edge
	movq	8(%r14), %rax
	movl	16(%rax), %eax
.LBB20_4:                               # %lor.lhs.false3
	andq	$255, %rax
	cmpq	$23, %rax
	jne	.LBB20_6
.LBB20_5:                               # %if.then
	movq	%r14, %rdi
	callq	default_conversion
	movq	%rax, %r14
.LBB20_6:                               # %if.end
	movl	warn_sequence_point(%rip), %eax
	testq	%rax, %rax
	je	.LBB20_38
# BB#7:                                 # %if.then13
	movq	$0, 40(%rsp)
	cmpq	$0, tlist_firstobj(%rip)
	movq	$0, 32(%rsp)
	movq	$0, warned_ids(%rip)
	movq	$0, save_expr_cache(%rip)
	jne	.LBB20_15
# BB#8:                                 # %if.then.i
	movabsq	$tlist_obstack, %rdi
	callq	gcc_obstack_init
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB20_10
# BB#9:                                 # %if.then3.i
	movabsq	$tlist_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB20_10:                              # %if.end.i
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB20_12
# BB#11:                                # %if.then10.i
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB20_12:                              # %if.end11.i
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rbx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB20_14
# BB#13:                                # %if.then31.i
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB20_14:                              # %if.end34.i
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rax, tlist_firstobj(%rip)
.LBB20_15:                              # %if.end38.i
	movq	%r14, 8(%rsp)           # 8-byte Spill
	leaq	40(%rsp), %rsi
	leaq	32(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	verify_tree
	movq	32(%rsp), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB20_33
# BB#16:
	movq	16(%rsp), %rax          # 8-byte Reload
	.align	16, 0x90
.LBB20_17:                              # %for.body.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_19 Depth 2
                                        #     Child Loop BB20_22 Depth 2
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	16(%rax), %r14
	testq	%r14, %r14
	je	.LBB20_32
# BB#18:                                # %if.then.i.i
                                        #   in Loop: Header=BB20_17 Depth=1
	movq	warned_ids(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r12
	testq	%rax, %rax
	movq	16(%rsp), %r15          # 8-byte Reload
	je	.LBB20_22
	.align	16, 0x90
.LBB20_19:                              # %for.body.i.i.i
                                        #   Parent Loop BB20_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	cmpq	%r12, %rcx
	je	.LBB20_32
# BB#20:                                # %for.cond.i.i.i
                                        #   in Loop: Header=BB20_19 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB20_19
# BB#21:                                #   in Loop: Header=BB20_17 Depth=1
	movq	16(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB20_22:                              # %while.body.i.i.i
                                        #   Parent Loop BB20_17 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r15), %rax
	cmpq	%r12, %rax
	jne	.LBB20_31
# BB#23:                                # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	16(%r15), %rax
	cmpq	%r14, %rax
	je	.LBB20_31
# BB#24:                                # %if.then10.i.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %r13
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB20_26
# BB#25:                                # %if.then.i80.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB20_26:                              # %if.end.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB20_28
# BB#27:                                # %if.then8.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB20_28:                              # %if.end9.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rbx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB20_30
# BB#29:                                # %if.then29.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB20_30:                              # %new_tlist.exit.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r13, (%rax)
	movq	%r12, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, warned_ids(%rip)
	movq	8(%r15), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str253, %rdi
	callq	warning
.LBB20_31:                              # %if.end12.i.i.i
                                        #   in Loop: Header=BB20_22 Depth=2
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB20_22
.LBB20_32:                              # %for.inc.i.i
                                        #   in Loop: Header=BB20_17 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB20_17
.LBB20_33:                              # %warn_for_collisions.exit.i
	movq	tlist_firstobj(%rip), %rsi
	movq	tlist_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB20_36
# BB#34:                                # %warn_for_collisions.exit.i
	movq	tlist_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB20_36
# BB#35:                                # %if.then48.i
	movq	%rsi, tlist_obstack+16(%rip)
	movq	%rsi, tlist_obstack+24(%rip)
	jmp	.LBB20_37
.LBB20_36:                              # %if.else.i
	movabsq	$tlist_obstack, %rdi
	callq	obstack_free
.LBB20_37:                              # %if.end14
	movq	8(%rsp), %r14           # 8-byte Reload
.LBB20_38:                              # %if.end14
	movq	8(%r14), %rbx
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	je	.LBB20_43
# BB#39:                                # %land.lhs.true18
	cmpq	$0, 32(%rbx)
	jne	.LBB20_43
# BB#40:                                # %lor.lhs.false23
	movzbl	16(%rbx), %eax
	cmpq	$5, %rax
	je	.LBB20_43
# BB#41:                                # %lor.lhs.false23
	cmpq	$18, %rax
	je	.LBB20_43
# BB#42:                                # %if.then39
	movabsq	$.L.str13, %rdi
	xorq	%rax, %rax
	callq	error
	movq	8(%r14), %rbx
.LBB20_43:                              # %if.end40
	callq	current_stmt_tree
	movq	%rbx, 8(%rax)
	movq	$152, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	build_stmt
	movq	%rax, %rdi
	callq	add_stmt
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp145:
	.size	c_expand_expr_stmt, .Ltmp145-c_expand_expr_stmt
	.cfi_endproc

	.globl	check_case_value
	.align	16, 0x90
	.type	check_case_value,@function
check_case_value:                       # @check_case_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp149:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp150:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp151:
	.cfi_def_cfa_offset 32
.Ltmp152:
	.cfi_offset %rbx, -24
.Ltmp153:
	.cfi_offset %r14, -16
	movq	%rdi, %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB21_19
# BB#1:                                 # %while.cond.preheader
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB21_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movl	16(%rdi), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	addq	$-114, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB21_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	32(%rdi), %rax
	cmpq	%rcx, %rax
	je	.LBB21_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	8(%rdi), %rsi
	movq	8(%rax), %rbx
	cmpq	%rbx, %rsi
	je	.LBB21_2
.LBB21_5:                               # %while.end
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB21_11
# BB#6:                                 # %if.then25
	callq	decl_constant_value
	movq	global_trees(%rip), %rcx
	.align	16, 0x90
.LBB21_7:                               # %while.cond26
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movzbl	16(%rdi), %eax
	addq	$-114, %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	ja	.LBB21_10
# BB#8:                                 # %land.lhs.true44
                                        #   in Loop: Header=BB21_7 Depth=1
	movq	32(%rdi), %rax
	cmpq	%rcx, %rax
	je	.LBB21_10
# BB#9:                                 # %land.rhs49
                                        #   in Loop: Header=BB21_7 Depth=1
	movq	8(%rdi), %rdx
	movq	8(%rax), %rsi
	cmpq	%rsi, %rdx
	je	.LBB21_7
.LBB21_10:                              # %while.end63
	callq	fold
	movq	%rax, %rdi
	movl	16(%rdi), %edx
	movq	global_trees(%rip), %rcx
.LBB21_11:                              # %if.end65
	andq	$255, %rdx
	cmpq	$25, %rdx
	je	.LBB21_14
# BB#12:                                # %if.end65
	cmpq	%rcx, %rdi
	je	.LBB21_14
# BB#13:                                # %if.then73
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rbx
	jmp	.LBB21_15
.LBB21_14:                              # %if.else
	callq	default_conversion
	movq	%rax, %rbx
.LBB21_15:                              # %if.end75
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	addq	$-25, %rcx
	andq	%r14, %rcx
	cmpq	$3, %rcx
	ja	.LBB21_19
# BB#16:                                # %land.lhs.true.i
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB21_19
# BB#17:                                # %land.lhs.true.i
	movl	pedantic(%rip), %eax
	testq	%r14, %rax
	je	.LBB21_19
# BB#18:                                # %if.then.i
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB21_19:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp154:
	.size	check_case_value, .Ltmp154-check_case_value
	.cfi_endproc

	.globl	type_for_size
	.align	16, 0x90
	.type	type_for_size,@function
type_for_size:                          # @type_for_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	integer_types+40(%rip), %rax
	movl	60(%rax), %edx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rdi, %rcx
	andq	%r8, %rcx
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rcx, %rdx
	jne	.LBB22_3
# BB#1:                                 # %if.then
	testq	%r8, %rsi
	je	.LBB22_31
# BB#2:
	movq	integer_types+48(%rip), %rax
	retq
.LBB22_3:                               # %if.end
	movq	integer_types+8(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	andq	%r8, %rdi
	cmpq	%rdi, %rcx
	jne	.LBB22_6
# BB#4:                                 # %if.then6
	testq	%r8, %rsi
	je	.LBB22_31
# BB#5:
	movq	integer_types+16(%rip), %rax
	retq
.LBB22_6:                               # %if.end12
	movq	integer_types+24(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jne	.LBB22_9
# BB#7:                                 # %if.then18
	testq	%r8, %rsi
	je	.LBB22_31
# BB#8:
	movq	integer_types+32(%rip), %rax
	retq
.LBB22_9:                               # %if.end24
	movq	integer_types+56(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jne	.LBB22_12
# BB#10:                                # %if.then30
	testq	%r8, %rsi
	je	.LBB22_31
# BB#11:
	movq	integer_types+64(%rip), %rax
	retq
.LBB22_12:                              # %if.end36
	movq	integer_types+72(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jne	.LBB22_15
# BB#13:                                # %if.then42
	testq	%r8, %rsi
	je	.LBB22_31
# BB#14:
	movq	integer_types+80(%rip), %rax
	retq
.LBB22_15:                              # %if.end48
	movq	c_global_trees+72(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jne	.LBB22_18
# BB#16:                                # %if.then54
	testq	%r8, %rsi
	je	.LBB22_31
# BB#17:
	movq	c_global_trees+80(%rip), %rax
	retq
.LBB22_18:                              # %if.end60
	movq	global_trees+8(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jae	.LBB22_19
# BB#21:                                # %if.end72
	movq	global_trees+16(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jae	.LBB22_22
# BB#24:                                # %if.end84
	movq	global_trees+24(%rip), %rax
	movl	60(%rax), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rdi, %rcx
	jae	.LBB22_25
# BB#27:                                # %if.end96
	movq	global_trees+32(%rip), %rdx
	movl	60(%rdx), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	jb	.LBB22_31
# BB#28:                                # %if.then102
	testq	%r8, %rsi
	je	.LBB22_30
# BB#29:
	movq	global_trees+72(%rip), %rdx
.LBB22_30:                              # %if.then102
	movq	%rdx, %rax
	jmp	.LBB22_31
.LBB22_19:                              # %if.then66
	testq	%r8, %rsi
	je	.LBB22_31
# BB#20:
	movq	global_trees+48(%rip), %rax
	retq
.LBB22_22:                              # %if.then78
	testq	%r8, %rsi
	je	.LBB22_31
# BB#23:
	movq	global_trees+56(%rip), %rax
	retq
.LBB22_25:                              # %if.then90
	testq	%r8, %rsi
	jne	.LBB22_26
.LBB22_31:                              # %return
	retq
.LBB22_26:
	movq	global_trees+64(%rip), %rax
	retq
.Ltmp155:
	.size	type_for_size, .Ltmp155-type_for_size
	.cfi_endproc

	.globl	type_for_mode
	.align	16, 0x90
	.type	type_for_mode,@function
type_for_mode:                          # @type_for_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp163:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp165:
	.cfi_def_cfa_offset 48
.Ltmp166:
	.cfi_offset %rbx, -40
.Ltmp167:
	.cfi_offset %r12, -32
.Ltmp168:
	.cfi_offset %r14, -24
.Ltmp169:
	.cfi_offset %r15, -16
	movq	integer_types+40(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %r12
	movl	60(%rax), %edx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r12, %rsi
	andq	%r15, %rsi
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rsi, %rdx
	jne	.LBB23_3
# BB#1:                                 # %if.then
	testq	%r15, %r14
	je	.LBB23_93
# BB#2:
	movq	integer_types+48(%rip), %rax
	jmp	.LBB23_93
.LBB23_3:                               # %if.end
	movq	integer_types+8(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	movq	%r12, %rbx
	andq	%r15, %rbx
	cmpq	%rbx, %rdx
	jne	.LBB23_6
# BB#4:                                 # %if.then8
	testq	%r15, %r14
	je	.LBB23_93
# BB#5:
	movq	integer_types+16(%rip), %rax
	jmp	.LBB23_93
.LBB23_6:                               # %if.end14
	movq	integer_types+24(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB23_9
# BB#7:                                 # %if.then21
	testq	%r15, %r14
	je	.LBB23_93
# BB#8:
	movq	integer_types+32(%rip), %rax
	jmp	.LBB23_93
.LBB23_9:                               # %if.end27
	movq	integer_types+56(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB23_12
# BB#10:                                # %if.then34
	testq	%r15, %r14
	je	.LBB23_93
# BB#11:
	movq	integer_types+64(%rip), %rax
	jmp	.LBB23_93
.LBB23_12:                              # %if.end40
	movq	integer_types+72(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB23_15
# BB#13:                                # %if.then47
	testq	%r15, %r14
	je	.LBB23_93
# BB#14:
	movq	integer_types+80(%rip), %rax
	jmp	.LBB23_93
.LBB23_15:                              # %if.end53
	movq	c_global_trees+72(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB23_18
# BB#16:                                # %if.then60
	testq	%r15, %r14
	je	.LBB23_93
# BB#17:
	movq	c_global_trees+80(%rip), %rax
	jmp	.LBB23_93
.LBB23_18:                              # %if.end66
	leaq	-2(%r12), %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jbe	.LBB23_19
# BB#36:                                # %if.end98
	movq	global_trees+40(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB23_39
# BB#37:                                # %if.then105
	testq	%r15, %r14
	je	.LBB23_93
# BB#38:
	movq	global_trees+80(%rip), %rax
	jmp	.LBB23_93
.LBB23_19:                              # %if.end66
	jmpq	*.LJTI23_0(,%rax,8)
.LBB23_20:                              # %if.then68
	testq	%r15, %r14
	jne	.LBB23_21
# BB#22:                                # %if.then68
	movabsq	$global_trees+8, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_39:                              # %if.end111
	movq	global_trees+192(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	je	.LBB23_93
# BB#40:                                # %if.end119
	movq	global_trees+200(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	je	.LBB23_93
# BB#41:                                # %if.end127
	movq	global_trees+208(%rip), %rax
	movl	60(%rax), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rbx, %rdx
	je	.LBB23_93
# BB#42:                                # %if.end135
	movq	integer_types(%rip), %rdi
	callq	build_pointer_type
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	%rbx, %rax
	jne	.LBB23_45
# BB#43:                                # %if.then142
	movq	integer_types(%rip), %rdi
	jmp	.LBB23_44
.LBB23_24:                              # %if.then76
	testq	%r15, %r14
	jne	.LBB23_25
# BB#26:                                # %if.then76
	movabsq	$global_trees+16, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_28:                              # %if.then84
	testq	%r15, %r14
	jne	.LBB23_29
# BB#30:                                # %if.then84
	movabsq	$global_trees+24, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_32:                              # %if.then92
	testq	%r15, %r14
	jne	.LBB23_33
# BB#34:                                # %if.then92
	movabsq	$global_trees+32, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_21:
	movabsq	$global_trees+48, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_25:
	movabsq	$global_trees+56, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_29:
	movabsq	$global_trees+64, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_33:
	movabsq	$global_trees+72, %rax
	movq	(%rax), %rax
.LBB23_93:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB23_45:                              # %if.end144
	movq	integer_types+40(%rip), %rdi
	callq	build_pointer_type
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	cmpq	%rbx, %rax
	jne	.LBB23_47
# BB#46:                                # %if.then152
	movq	integer_types+40(%rip), %rdi
.LBB23_44:                              # %if.then142
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build_pointer_type  # TAILCALL
.LBB23_47:                              # %if.end154
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$14, %rax
	jg	.LBB23_56
# BB#48:                                # %if.end154
	cmpq	$6, %rbx
	jne	.LBB23_59
	jmp	.LBB23_49
.LBB23_56:                              # %if.end154
	cmpq	$15, %rbx
	je	.LBB23_49
# BB#57:                                # %if.end154
	cmpq	$37, %rbx
	je	.LBB23_49
# BB#58:                                # %if.end154
	cmpq	$46, %rbx
	jne	.LBB23_59
.LBB23_49:                              # %if.end154.land.lhs.true174_crit_edge303
	movl	target_flags(%rip), %edx
	jmp	.LBB23_50
.LBB23_59:                              # %lor.lhs.false161
	movl	target_flags(%rip), %edx
	testq	$262144, %rdx           # imm = 0x40000
	je	.LBB23_51
# BB#60:                                # %land.lhs.true
	xorq	%rax, %rax
	cmpq	$44, %rbx
	ja	.LBB23_93
# BB#61:                                # %land.lhs.true
	movq	$1, %rsi
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	movabsq	$627065290800, %rcx     # imm = 0x9200010030
	testq	%rcx, %rdi
	je	.LBB23_62
.LBB23_50:                              # %land.lhs.true174
	testq	$327680, %rdx           # imm = 0x50000
	jne	.LBB23_64
.LBB23_51:                              # %cond.false178
	xorq	%rax, %rax
	cmpq	$44, %rbx
	ja	.LBB23_93
# BB#52:                                # %cond.false178
	movq	$1, %rsi
	movq	$1, %rdi
	movq	%rbx, %rcx
	shlq	%cl, %rdi
	movabsq	$627065225264, %rcx     # imm = 0x9200000030
	testq	%rcx, %rdi
	je	.LBB23_62
# BB#53:                                # %land.lhs.true188
	testq	$16384, %rdx            # imm = 0x4000
	jne	.LBB23_64
# BB#54:                                # %cond.false192
	cmpq	$15, %rbx
	je	.LBB23_63
# BB#55:                                # %cond.false192
	cmpq	$44, %rbx
	je	.LBB23_63
	jmp	.LBB23_93
.LBB23_62:                              # %land.lhs.true
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	movabsq	$17592186077184, %rcx   # imm = 0x100000008000
	testq	%rcx, %rsi
	je	.LBB23_93
.LBB23_63:                              # %land.lhs.true196
	xorq	%rax, %rax
	testq	$1048576, %rdx          # imm = 0x100000
	je	.LBB23_93
.LBB23_64:                              # %if.then201
	xorq	%rax, %rax
	addq	$-33, %r12
	andq	%r15, %r12
	cmpq	$17, %r12
	ja	.LBB23_93
# BB#65:                                # %if.then201
	jmpq	*.LJTI23_1(,%r12,8)
.LBB23_78:                              # %sw.bb219
	testq	%r15, %r14
	jne	.LBB23_79
# BB#80:                                # %sw.bb219
	movabsq	$global_trees+376, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_82:                              # %sw.bb225
	testq	%r15, %r14
	jne	.LBB23_83
# BB#84:                                # %sw.bb225
	movabsq	$global_trees+368, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_74:                              # %sw.bb213
	testq	%r15, %r14
	jne	.LBB23_75
# BB#76:                                # %sw.bb213
	movabsq	$global_trees+344, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_86:                              # %sw.bb231
	testq	%r15, %r14
	jne	.LBB23_87
# BB#88:                                # %sw.bb231
	movabsq	$global_trees+360, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_70:                              # %sw.bb207
	testq	%r15, %r14
	jne	.LBB23_71
# BB#72:                                # %sw.bb207
	movabsq	$global_trees+352, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_66:                              # %sw.bb
	testq	%r15, %r14
	jne	.LBB23_67
# BB#68:                                # %sw.bb
	movabsq	$global_trees+392, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_92:                              # %sw.bb239
	movq	global_trees+384(%rip), %rax
	jmp	.LBB23_93
.LBB23_91:                              # %sw.bb238
	movq	global_trees+328(%rip), %rax
	jmp	.LBB23_93
.LBB23_90:                              # %sw.bb237
	movq	global_trees+336(%rip), %rax
	jmp	.LBB23_93
.LBB23_79:
	movabsq	$global_trees+304, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_83:
	movabsq	$global_trees+296, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_75:
	movabsq	$global_trees+272, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_87:
	movabsq	$global_trees+288, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_71:
	movabsq	$global_trees+280, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.LBB23_67:
	movabsq	$global_trees+320, %rax
	movq	(%rax), %rax
	jmp	.LBB23_93
.Ltmp170:
	.size	type_for_mode, .Ltmp170-type_for_mode
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI23_0:
	.quad	.LBB23_20
	.quad	.LBB23_24
	.quad	.LBB23_28
	.quad	.LBB23_32
.LJTI23_1:
	.quad	.LBB23_78
	.quad	.LBB23_93
	.quad	.LBB23_93
	.quad	.LBB23_82
	.quad	.LBB23_74
	.quad	.LBB23_93
	.quad	.LBB23_86
	.quad	.LBB23_70
	.quad	.LBB23_93
	.quad	.LBB23_93
	.quad	.LBB23_66
	.quad	.LBB23_92
	.quad	.LBB23_93
	.quad	.LBB23_91
	.quad	.LBB23_93
	.quad	.LBB23_93
	.quad	.LBB23_93
	.quad	.LBB23_90

	.text
	.globl	signed_or_unsigned_type
	.align	16, 0x90
	.type	signed_or_unsigned_type,@function
signed_or_unsigned_type:                # @signed_or_unsigned_type
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rsi), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$12, %rcx
	ja	.LBB24_37
# BB#1:                                 # %entry
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB24_37
# BB#2:                                 # %lor.lhs.false18
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rdx
	je	.LBB24_37
# BB#3:                                 # %if.end
	movq	integer_types+8(%rip), %rdx
	movl	60(%rsi), %ecx
	movl	60(%rdx), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_6
# BB#4:                                 # %if.then31
	testq	%r8, %rdi
	je	.LBB24_36
# BB#5:
	movq	integer_types+16(%rip), %rdx
	jmp	.LBB24_36
.LBB24_6:                               # %if.end32
	movq	integer_types+40(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	andq	%r8, %rcx
	cmpq	%rax, %rcx
	jne	.LBB24_9
# BB#7:                                 # %if.then42
	testq	%r8, %rdi
	je	.LBB24_36
# BB#8:
	movq	integer_types+48(%rip), %rdx
	jmp	.LBB24_36
.LBB24_9:                               # %if.end48
	movq	integer_types+24(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_12
# BB#10:                                # %if.then58
	testq	%r8, %rdi
	je	.LBB24_36
# BB#11:
	movq	integer_types+32(%rip), %rdx
	jmp	.LBB24_36
.LBB24_12:                              # %if.end64
	movq	integer_types+56(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_15
# BB#13:                                # %if.then74
	testq	%r8, %rdi
	je	.LBB24_36
# BB#14:
	movq	integer_types+64(%rip), %rdx
	jmp	.LBB24_36
.LBB24_15:                              # %if.end80
	movq	integer_types+72(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_18
# BB#16:                                # %if.then90
	testq	%r8, %rdi
	je	.LBB24_36
# BB#17:
	movq	integer_types+80(%rip), %rdx
	jmp	.LBB24_36
.LBB24_18:                              # %if.end96
	movq	c_global_trees+72(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_21
# BB#19:                                # %if.then106
	testq	%r8, %rdi
	je	.LBB24_36
# BB#20:
	movq	c_global_trees+80(%rip), %rdx
	jmp	.LBB24_36
.LBB24_21:                              # %if.end112
	movq	global_trees+40(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_24
# BB#22:                                # %if.then122
	testq	%r8, %rdi
	je	.LBB24_36
# BB#23:
	movq	global_trees+80(%rip), %rdx
	jmp	.LBB24_36
.LBB24_24:                              # %if.end128
	movq	global_trees+32(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_27
# BB#25:                                # %if.then138
	testq	%r8, %rdi
	je	.LBB24_36
# BB#26:
	movq	global_trees+72(%rip), %rdx
	jmp	.LBB24_36
.LBB24_27:                              # %if.end144
	movq	global_trees+24(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_30
# BB#28:                                # %if.then154
	testq	%r8, %rdi
	je	.LBB24_36
# BB#29:
	movq	global_trees+64(%rip), %rdx
	jmp	.LBB24_36
.LBB24_30:                              # %if.end160
	movq	global_trees+16(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_33
# BB#31:                                # %if.then170
	testq	%r8, %rdi
	je	.LBB24_36
# BB#32:
	movq	global_trees+56(%rip), %rdx
	jmp	.LBB24_36
.LBB24_33:                              # %if.end176
	movq	global_trees+8(%rip), %rdx
	movl	60(%rdx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	jne	.LBB24_37
# BB#34:                                # %if.then186
	testq	%r8, %rdi
	je	.LBB24_36
# BB#35:
	movq	global_trees+48(%rip), %rdx
.LBB24_36:                              # %if.then186
	movq	%rdx, %rsi
.LBB24_37:                              # %return
	movq	%rsi, %rax
	retq
.Ltmp171:
	.size	signed_or_unsigned_type, .Ltmp171-signed_or_unsigned_type
	.cfi_endproc

	.globl	min_precision
	.align	16, 0x90
	.type	min_precision,@function
min_precision:                          # @min_precision
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp176:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp177:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp178:
	.cfi_def_cfa_offset 32
.Ltmp179:
	.cfi_offset %rbx, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB25_2
# BB#1:                                 # %if.then
	movq	8(%rbx), %rsi
	movq	$90, %rdi
	movq	%rbx, %rdx
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %rbx
.LBB25_2:                               # %if.end
	movq	%rbx, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	movq	$1, %rax
	jne	.LBB25_4
# BB#3:                                 # %if.else
	movq	%rbx, %rdi
	callq	tree_floor_log2
	incq	%rax
.LBB25_4:                               # %if.end6
	testq	%r15, %r14
	jne	.LBB25_5
# BB#6:                                 # %if.end6
	movabsq	$1, %rcx
	jmp	.LBB25_7
.LBB25_5:
	movabsq	$0, %rcx
.LBB25_7:                               # %if.end6
	addq	%rcx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp182:
	.size	min_precision, .Ltmp182-min_precision
	.cfi_endproc

	.globl	binary_op_error
	.align	16, 0x90
	.type	binary_op_error,@function
binary_op_error:                        # @binary_op_error
	.cfi_startproc
# BB#0:                                 # %entry
	addq	$-59, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$56, %rax
	ja	.LBB26_23
# BB#1:                                 # %entry
	movabsq	$.L.str16, %rsi
	jmpq	*.LJTI26_0(,%rax,8)
.LBB26_16:                              # %sw.bb15
	movabsq	$.L.str30, %rsi
	jmp	.LBB26_24
.LBB26_23:                              # %sw.default
	movabsq	$.L.str37, %rsi
.LBB26_24:                              # %sw.epilog
	movabsq	$.L.str38, %rdi
	xorq	%rax, %rax
	jmp	error  # TAILCALL
.LBB26_15:                              # %sw.bb14
	movabsq	$.L.str29, %rsi
	jmp	.LBB26_24
.LBB26_22:                              # %sw.bb21
	movabsq	$.L.str36, %rsi
	jmp	.LBB26_24
.LBB26_3:                               # %sw.bb2
	movabsq	$.L.str17, %rsi
	jmp	.LBB26_24
.LBB26_4:                               # %sw.bb3
	movabsq	$.L.str18, %rsi
	jmp	.LBB26_24
.LBB26_6:                               # %sw.bb5
	movabsq	$.L.str20, %rsi
	jmp	.LBB26_24
.LBB26_5:                               # %sw.bb4
	movabsq	$.L.str19, %rsi
	jmp	.LBB26_24
.LBB26_13:                              # %sw.bb12
	movabsq	$.L.str27, %rsi
	jmp	.LBB26_24
.LBB26_14:                              # %sw.bb13
	movabsq	$.L.str28, %rsi
	jmp	.LBB26_24
.LBB26_18:                              # %sw.bb17
	movabsq	$.L.str32, %rsi
	jmp	.LBB26_24
.LBB26_21:                              # %sw.bb20
	movabsq	$.L.str35, %rsi
	jmp	.LBB26_24
.LBB26_17:                              # %sw.bb16
	movabsq	$.L.str31, %rsi
	jmp	.LBB26_24
.LBB26_19:                              # %sw.bb18
	movabsq	$.L.str33, %rsi
	jmp	.LBB26_24
.LBB26_20:                              # %sw.bb19
	movabsq	$.L.str34, %rsi
	jmp	.LBB26_24
.LBB26_11:                              # %sw.bb10
	movabsq	$.L.str25, %rsi
	jmp	.LBB26_24
.LBB26_9:                               # %sw.bb8
	movabsq	$.L.str23, %rsi
	jmp	.LBB26_24
.LBB26_12:                              # %sw.bb11
	movabsq	$.L.str26, %rsi
	jmp	.LBB26_24
.LBB26_10:                              # %sw.bb9
	movabsq	$.L.str24, %rsi
	jmp	.LBB26_24
.LBB26_7:                               # %sw.bb6
	movabsq	$.L.str21, %rsi
	jmp	.LBB26_24
.LBB26_8:                               # %sw.bb7
	movabsq	$.L.str22, %rsi
	jmp	.LBB26_24
.LBB26_2:                               # %sw.bb
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	jmp	error  # TAILCALL
.Ltmp183:
	.size	binary_op_error, .Ltmp183-binary_op_error
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI26_0:
	.quad	.LBB26_24
	.quad	.LBB26_3
	.quad	.LBB26_4
	.quad	.LBB26_16
	.quad	.LBB26_23
	.quad	.LBB26_16
	.quad	.LBB26_23
	.quad	.LBB26_15
	.quad	.LBB26_23
	.quad	.LBB26_15
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_6
	.quad	.LBB26_5
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_13
	.quad	.LBB26_14
	.quad	.LBB26_22
	.quad	.LBB26_22
	.quad	.LBB26_18
	.quad	.LBB26_21
	.quad	.LBB26_17
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_19
	.quad	.LBB26_20
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_11
	.quad	.LBB26_9
	.quad	.LBB26_12
	.quad	.LBB26_10
	.quad	.LBB26_7
	.quad	.LBB26_8
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_23
	.quad	.LBB26_2

	.text
	.globl	shorten_compare
	.align	16, 0x90
	.type	shorten_compare,@function
shorten_compare:                        # @shorten_compare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp190:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp191:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp192:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp193:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp194:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp195:
	.cfi_def_cfa_offset 128
.Ltmp196:
	.cfi_offset %rbx, -48
.Ltmp197:
	.cfi_offset %r12, -40
.Ltmp198:
	.cfi_offset %r13, -32
.Ltmp199:
	.cfi_offset %r14, -24
.Ltmp200:
	.cfi_offset %r15, -16
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 48(%rsp)          # 8-byte Spill
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	(%rdi), %r12
	movq	(%rsi), %r13
	movl	(%rcx), %eax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	leaq	76(%rsp), %rsi
	movq	%r12, %rdi
	callq	get_narrower
	movq	%rax, %rbx
	leaq	72(%rsp), %rsi
	movq	%r13, %rdi
	callq	get_narrower
	movq	%rax, %r15
	cmpq	%rbx, %r12
	jne	.LBB27_3
# BB#1:                                 # %land.lhs.true
	movq	8(%r12), %rax
	movq	(%r14), %rcx
	cmpq	%rcx, %rax
	je	.LBB27_3
# BB#2:                                 # %if.then
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 76(%rsp)
.LBB27_3:                               # %if.end
	cmpq	%r15, %r13
	jne	.LBB27_6
# BB#4:                                 # %land.lhs.true8
	movq	8(%r13), %rax
	movq	(%r14), %rcx
	cmpq	%rcx, %rax
	je	.LBB27_6
# BB#5:                                 # %if.then12
	movl	16(%rax), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 72(%rsp)
.LBB27_6:                               # %if.end20
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%r14, 56(%rsp)          # 8-byte Spill
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	movabsq	$0, %r13
	movabsq	$1, %rax
	movq	%r13, %r14
	jne	.LBB27_8
# BB#7:                                 # %if.end20
	movq	%rax, %r14
.LBB27_8:                               # %if.end20
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$7, %rcx
	jne	.LBB27_10
# BB#9:                                 # %if.end20
	movq	%rax, %r13
.LBB27_10:                              # %if.end20
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB27_11
# BB#12:                                # %land.lhs.true40
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB27_13
# BB#14:                                # %land.lhs.true43
	movq	%r15, %rdi
	callq	real_zerop
	testq	%rbx, %rax
	je	.LBB27_15
.LBB27_13:
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	%r14, %rcx
	movq	%r13, %r14
	movq	32(%rsp), %r8           # 8-byte Reload
	movq	%r12, 64(%rsp)          # 8-byte Spill
	jmp	.LBB27_22
.LBB27_11:
	movq	64(%rsp), %r9           # 8-byte Reload
	movq	%r14, %rcx
	movq	%r13, %r14
	movq	%rbx, %r8
	movq	%r12, 64(%rsp)          # 8-byte Spill
	jmp	.LBB27_22
.LBB27_15:                              # %if.then46
	movl	76(%rsp), %eax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	%rdx, (%rcx)
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%r12, (%rcx)
	movl	72(%rsp), %ecx
	movl	%ecx, 76(%rsp)
	movl	%eax, 72(%rsp)
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %ecx
	leaq	-97(%rcx), %rax
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB27_21
# BB#16:                                # %if.then46
	jmpq	*.LJTI27_0(,%rax,8)
.LBB27_17:                              # %sw.bb
	movq	$99, %rcx
	jmp	.LBB27_21
.LBB27_19:                              # %sw.bb48
	movq	$100, %rcx
	jmp	.LBB27_21
.LBB27_18:                              # %sw.bb47
	movq	$97, %rcx
	jmp	.LBB27_21
.LBB27_20:                              # %sw.bb49
	movq	$98, %rcx
.LBB27_21:                              # %sw.epilog
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%ecx, (%rax)
	movq	%r12, %r9
	movq	%r13, %rcx
	movq	%r15, %r8
	movq	32(%rsp), %r15          # 8-byte Reload
.LBB27_22:                              # %if.end50
	movq	%r14, %rax
	orq	%rcx, %rax
	movq	56(%rsp), %r13          # 8-byte Reload
	jne	.LBB27_116
# BB#23:                                # %land.lhs.true54
	movzbl	16(%r15), %edx
	cmpq	$25, %rdx
	jne	.LBB27_116
# BB#24:                                # %land.lhs.true61
	movq	8(%r8), %rsi
	movq	(%r13), %rbx
	movl	60(%rsi), %edx
	movl	60(%rbx), %edi
	andq	$511, %rdx              # imm = 0x1FF
	andq	$511, %rdi              # imm = 0x1FF
	cmpq	%rdi, %rdx
	jae	.LBB27_116
# BB#25:                                # %if.then73
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movl	16(%rbx), %eax
	movl	76(%rsp), %r14d
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	signed_or_unsigned_type
	movzbl	16(%rax), %ecx
	cmpq	$10, %rcx
	jne	.LBB27_26
# BB#27:                                # %if.then88
	movl	60(%rax), %edi
	andq	$511, %rdi              # imm = 0x1FF
	movq	%r14, %rsi
	movq	%r14, %r15
	callq	type_for_size
	jmp	.LBB27_28
.LBB27_116:                             # %if.else481
	testq	%rcx, %rcx
	je	.LBB27_120
# BB#117:                               # %if.else481
	testq	%r14, %r14
	je	.LBB27_120
# BB#118:                               # %land.lhs.true485
	movq	8(%r8), %r12
	movq	8(%r15), %rdx
	movl	60(%r12), %esi
	movl	60(%rdx), %edx
	xorq	%rsi, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	je	.LBB27_119
.LBB27_120:                             # %if.else503
	movl	76(%rsp), %edx
	movl	72(%rsp), %esi
	cmpq	%rsi, %rdx
	jne	.LBB27_122
# BB#121:                               # %if.else503
	cmpq	%r14, %rcx
	jne	.LBB27_122
# BB#125:                               # %land.lhs.true509
	movq	8(%r8), %rdi
	movq	(%r13), %r12
	movl	60(%rdi), %edx
	movl	60(%r12), %ecx
	andq	$511, %rdx              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rdx
	jae	.LBB27_123
# BB#126:                               # %land.lhs.true522
	movq	%r8, %r14
	movq	8(%r15), %rsi
	movl	60(%rsi), %edx
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rcx, %rdx
	jae	.LBB27_123
# BB#127:                               # %if.then535
	movq	%r15, 32(%rsp)          # 8-byte Spill
	callq	common_type
	movl	76(%rsp), %r15d
	testq	%r15, %r15
	movq	$1, %rdi
	jne	.LBB27_129
# BB#128:                               # %lor.rhs542
	movq	(%r13), %rcx
	movzbl	17(%rcx), %edi
	andq	$32, %rdi
	movq	$5, %rcx
	shrq	%cl, %rdi
.LBB27_129:                             # %lor.end549
	movq	%rax, %rsi
	callq	signed_or_unsigned_type
	movq	%r14, %rbx
	movq	8(%rbx), %rsi
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	signed_or_unsigned_type
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	8(%rbx), %rsi
	movl	72(%rsp), %edi
	movq	%rax, %r14
	callq	signed_or_unsigned_type
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%r14, %r8
	movq	%rax, %rbx
	jmp	.LBB27_130
.LBB27_122:                             # %if.else503.if.else560_crit_edge
	movq	(%r13), %r12
.LBB27_123:                             # %if.else560
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB27_131
# BB#124:
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%r9, %rbx
	jmp	.LBB27_130
.LBB27_131:                             # %land.lhs.true564
	movq	%r9, %r15
	movq	%r15, %rdi
	callq	integer_zerop
	testq	%rbx, %rax
	je	.LBB27_132
# BB#133:                               # %land.lhs.true567
	movq	(%r13), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB27_132
# BB#134:                               # %if.then574
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	cmpq	$97, %rax
	jne	.LBB27_135
# BB#142:                               # %sw.bb596
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	movabsq	$c_global_trees+144, %r14
	je	.LBB27_143
# BB#144:                               # %sw.bb596
	movl	in_system_header(%rip), %eax
	testq	%rbx, %rax
	jne	.LBB27_143
# BB#145:                               # %land.lhs.true600
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rbx          # 8-byte Reload
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB27_148
# BB#146:                               # %land.lhs.true607
	movq	%r12, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movzbl	18(%rax), %eax
	testq	$8, %rax
	je	.LBB27_147
.LBB27_148:                             # %if.then616
	movq	%r12, %r15
	movabsq	$.L.str42, %rdi
	jmp	.LBB27_149
.LBB27_119:
	movq	%r15, %rbx
	jmp	.LBB27_130
.LBB27_135:                             # %if.then574
	cmpq	$100, %rax
	jne	.LBB27_132
# BB#136:                               # %sw.bb575
	movl	extra_warnings(%rip), %eax
	testq	%rax, %rax
	movabsq	$c_global_trees+136, %r14
	je	.LBB27_143
# BB#137:                               # %sw.bb575
	movl	in_system_header(%rip), %eax
	testq	%rbx, %rax
	jne	.LBB27_143
# BB#138:                               # %land.lhs.true579
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB27_141
# BB#139:                               # %land.lhs.true586
	movq	%r12, %rdi
	callq	signed_type
	movq	%rax, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	convert
	movzbl	18(%rax), %eax
	testq	$8, %rax
	je	.LBB27_140
.LBB27_141:                             # %if.then594
	movq	%r12, %r15
	movabsq	$.L.str41, %rdi
.LBB27_149:                             # %sw.epilog619
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB27_150
.LBB27_132:
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	%r15, %rbx
	jmp	.LBB27_130
.LBB27_26:
	movq	%r14, %r15
.LBB27_28:                              # %if.end94
	movq	104(%rax), %rcx
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	112(%rax), %r12
	movq	%rax, %r14
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB27_31
# BB#29:                                # %if.end94
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB27_31
# BB#30:                                # %if.then102
	movq	%rbx, %rdi
	callq	signed_type
	movq	%rax, %rbx
	movq	%rbx, (%r13)
.LBB27_31:                              # %if.end104
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	32(%rsp), %r9           # 8-byte Reload
	movq	8(%r9), %rax
	cmpq	%rbx, %rax
	je	.LBB27_33
# BB#32:                                # %if.then109
	movq	%rbx, %rdi
	movq	%r9, %rsi
	callq	convert
	movq	(%r13), %rbx
	movq	%rax, %r9
.LBB27_33:                              # %if.end111
	movq	%r12, %r14
	cmpq	%rbx, 16(%rsp)          # 8-byte Folded Reload
	movq	(%rsp), %r12            # 8-byte Reload
	je	.LBB27_35
# BB#34:                                # %if.then114
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r9, %rbx
	callq	convert
	movq	(%r13), %rdi
	movq	%rax, %r12
	movq	%r14, %rsi
	callq	convert
	movq	%rbx, %r9
	movq	%rax, %r14
.LBB27_35:                              # %if.end117
	movl	76(%rsp), %r11d
	movq	40(%r9), %rcx
	movq	40(%r12), %rsi
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB27_48
# BB#36:                                # %if.end117
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r11
	je	.LBB27_48
# BB#37:                                # %if.then121
	movq	$1, %rax
	cmpq	%rsi, %rcx
	movq	$1, %r10
	jb	.LBB27_40
# BB#38:                                # %lor.rhs
	xorq	%r10, %r10
	cmpq	%rsi, %rcx
	jne	.LBB27_40
# BB#39:                                # %land.rhs
	movq	32(%r9), %rdi
	movq	32(%r12), %rdx
	cmpq	%rdx, %rdi
	sbbq	%r10, %r10
	andq	$1, %r10
.LBB27_40:                              # %lor.end
	movq	40(%r14), %rdi
	cmpq	%rdi, %rcx
	jb	.LBB27_43
# BB#41:                                # %lor.rhs151
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	jne	.LBB27_43
# BB#42:                                # %land.rhs160
	movq	32(%r9), %r8
	movq	32(%r14), %rdx
	cmpq	%rdx, %r8
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB27_43:                              # %lor.end170
	movq	$1, %rdx
	cmpq	%rcx, %rsi
	movq	$1, %rbx
	jb	.LBB27_46
# BB#44:                                # %lor.rhs180
	xorq	%rbx, %rbx
	cmpq	%rcx, %rsi
	jne	.LBB27_46
# BB#45:                                # %land.rhs189
	movq	32(%r12), %r8
	movq	32(%r9), %rsi
	cmpq	%rsi, %r8
	sbbq	%rbx, %rbx
	andq	$1, %rbx
.LBB27_46:                              # %lor.end199
	cmpq	%rcx, %rdi
	jae	.LBB27_59
# BB#47:
	movq	24(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB27_61
.LBB27_48:                              # %if.else
	movq	$1, %rax
	cmpq	%rsi, %rcx
	movq	$1, %r10
	jl	.LBB27_51
# BB#49:                                # %lor.rhs238
	xorq	%r10, %r10
	cmpq	%rsi, %rcx
	jne	.LBB27_51
# BB#50:                                # %land.rhs247
	movq	32(%r9), %rdi
	movq	32(%r12), %rdx
	cmpq	%rdx, %rdi
	sbbq	%r10, %r10
	andq	$1, %r10
.LBB27_51:                              # %lor.end257
	movq	40(%r14), %rdi
	cmpq	%rdi, %rcx
	jl	.LBB27_54
# BB#52:                                # %lor.rhs267
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	jne	.LBB27_54
# BB#53:                                # %land.rhs276
	movq	32(%r9), %r8
	movq	32(%r14), %rdx
	cmpq	%rdx, %r8
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB27_54:                              # %lor.end286
	movq	$1, %rdx
	cmpq	%rcx, %rsi
	movq	$1, %rbx
	jl	.LBB27_57
# BB#55:                                # %lor.rhs296
	xorq	%rbx, %rbx
	cmpq	%rcx, %rsi
	jne	.LBB27_57
# BB#56:                                # %land.rhs305
	movq	32(%r12), %r8
	movq	32(%r9), %rsi
	cmpq	%rsi, %r8
	sbbq	%rbx, %rbx
	andq	$1, %rbx
.LBB27_57:                              # %lor.end315
	cmpq	%rcx, %rdi
	jge	.LBB27_59
# BB#58:
	movq	24(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB27_61
.LBB27_59:                              # %lor.rhs325
	xorq	%rdx, %rdx
	cmpq	%rcx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	jne	.LBB27_61
# BB#60:                                # %land.rhs334
	movq	32(%r14), %rcx
	movq	32(%r9), %rdx
	cmpq	%rdx, %rcx
	sbbq	%rdx, %rdx
	andq	$1, %rdx
.LBB27_61:                              # %if.end346
	xorq	%r12, %r12
	leaq	-97(%rsi), %rcx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$5, %rcx
	ja	.LBB27_85
# BB#62:                                # %if.end346
	jmpq	*.LJTI27_1(,%rcx,8)
.LBB27_66:                              # %if.then366
	testq	%rdx, %rdx
	je	.LBB27_68
# BB#67:                                # %if.then366
	movq	c_global_trees+136(%rip), %rdx
.LBB27_68:                              # %if.then366
	testq	%rbx, %rbx
	jne	.LBB27_70
# BB#69:
	movq	c_global_trees+144(%rip), %rdx
.LBB27_70:                              # %if.then366
	movq	%rdx, %r12
	jmp	.LBB27_85
.LBB27_143:
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r12, %r15
.LBB27_150:                             # %sw.epilog619
	movq	(%r14), %r12
	testq	%r12, %r12
	je	.LBB27_151
# BB#152:                               # %if.then622
	movq	64(%rsp), %rdx          # 8-byte Reload
	movzbl	17(%rdx), %eax
	testq	$1, %rax
	je	.LBB27_155
# BB#153:                               # %if.then629
	movq	8(%r12), %rsi
	movq	$47, %rdi
	xorq	%rax, %rax
	jmp	.LBB27_154
.LBB27_151:
	movq	64(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%r15, %r12
	jmp	.LBB27_130
.LBB27_76:                              # %if.then386
	testq	%rax, %rax
	je	.LBB27_77
# BB#78:                                # %if.then386
	xorq	%r12, %r12
	jmp	.LBB27_79
.LBB27_71:                              # %if.then376
	testq	%r10, %r10
	je	.LBB27_73
# BB#72:                                # %if.then376
	movq	c_global_trees+136(%rip), %r10
.LBB27_73:                              # %if.then376
	testq	%rax, %rax
	jne	.LBB27_75
# BB#74:
	movq	c_global_trees+144(%rip), %r10
.LBB27_75:                              # %if.then376
	movq	%r10, %r12
	jmp	.LBB27_85
.LBB27_80:                              # %if.then396
	testq	%rbx, %rbx
	je	.LBB27_81
# BB#82:                                # %if.then396
	xorq	%r12, %r12
	jmp	.LBB27_83
.LBB27_65:                              # %if.then357
	xorq	%r12, %r12
	orq	%rdx, %r10
	jne	.LBB27_84
	jmp	.LBB27_85
.LBB27_63:                              # %if.then349
	xorq	%r12, %r12
	orq	%rdx, %r10
	je	.LBB27_85
# BB#64:                                # %if.then349
	movq	c_global_trees+136(%rip), %r12
	jmp	.LBB27_85
.LBB27_147:
	movq	%r12, %r15
	jmp	.LBB27_150
.LBB27_77:
	movq	c_global_trees+136(%rip), %r12
.LBB27_79:                              # %if.then386
	testq	%r10, %r10
	jne	.LBB27_84
	jmp	.LBB27_85
.LBB27_81:
	movq	c_global_trees+136(%rip), %r12
.LBB27_83:                              # %if.then396
	testq	%rdx, %rdx
	je	.LBB27_85
.LBB27_84:
	movq	c_global_trees+144(%rip), %r12
.LBB27_85:                              # %if.end408
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB27_86
# BB#87:                                # %if.end408
	testq	%rdi, %r11
	jne	.LBB27_88
# BB#89:                                # %if.then412
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r9, 32(%rsp)           # 8-byte Spill
	testq	%r12, %r12
	je	.LBB27_90
# BB#91:                                # %if.then415
	leaq	-98(%rsi), %rax
	andq	%rdi, %rax
	movq	%r11, %r15
	cmpq	$2, %rax
	jae	.LBB27_92
# BB#95:                                # %sw.bb419
	movq	%rdi, %r14
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	112(%rax), %rax
	jmp	.LBB27_96
.LBB27_86:
	movq	%r9, 32(%rsp)           # 8-byte Spill
	jmp	.LBB27_99
.LBB27_88:
	movq	%r9, 32(%rsp)           # 8-byte Spill
	jmp	.LBB27_99
.LBB27_90:
	movq	%r11, %r15
	movq	%rdi, %r14
	jmp	.LBB27_97
.LBB27_92:                              # %if.then415
	andq	%rdi, %rsi
	movq	%rdi, %r14
	cmpq	$97, %rsi
	je	.LBB27_94
# BB#93:                                # %if.then415
	cmpq	$100, %rsi
	jne	.LBB27_98
.LBB27_94:                              # %sw.bb416
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rax
.LBB27_96:                              # %if.end424
	movq	%rax, 32(%rsp)          # 8-byte Spill
.LBB27_97:                              # %if.end424
	xorq	%r12, %r12
.LBB27_98:                              # %if.end424
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	unsigned_type
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%r14, %rdi
	movq	%r15, %r11
.LBB27_99:                              # %if.end426
	orq	%rax, %r11
	testq	%rdi, %r11
	movq	8(%rsp), %r8            # 8-byte Reload
	jne	.LBB27_105
# BB#100:                               # %land.lhs.true430
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	je	.LBB27_105
# BB#101:                               # %if.then437
	movq	c_global_trees+144(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB27_103
# BB#102:                               # %if.then440
	movq	%rdi, %r14
	movabsq	$.L.str39, %rdi
	xorq	%rax, %rax
	movq	%r8, %r15
	callq	warning
	movq	%r14, %rdi
	movq	%r15, %r8
.LBB27_103:                             # %if.end441
	movq	c_global_trees+136(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB27_105
# BB#104:                               # %if.then444
	movq	%rdi, %r14
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	movq	%r8, %r15
	callq	warning
	movq	%r14, %rdi
	movq	%r15, %r8
.LBB27_105:                             # %if.end446
	testq	%rbx, %rbx
	jne	.LBB27_112
# BB#106:                               # %if.end446
	movl	76(%rsp), %eax
	testq	%rdi, %rax
	je	.LBB27_112
# BB#107:                               # %land.lhs.true450
	movzbl	16(%r8), %eax
	cmpq	$25, %rax
	je	.LBB27_112
# BB#108:                               # %if.then457
	movq	c_global_trees+144(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB27_110
# BB#109:                               # %if.then460
	movabsq	$.L.str39, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	callq	warning
	movq	%rbx, %r8
.LBB27_110:                             # %if.end461
	movq	c_global_trees+136(%rip), %rax
	cmpq	%rax, %r12
	jne	.LBB27_112
# BB#111:                               # %if.then464
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	movq	%r8, %rbx
	callq	warning
	movq	%rbx, %r8
.LBB27_112:                             # %if.end466
	testq	%r12, %r12
	je	.LBB27_113
# BB#114:                               # %if.then469
	movzbl	17(%r8), %eax
	testq	$1, %rax
	je	.LBB27_155
# BB#115:                               # %if.then475
	movq	8(%r12), %rsi
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r8, %rdx
.LBB27_154:                             # %return
	movq	%r12, %rcx
	callq	build
	movq	%rax, %r12
	jmp	.LBB27_155
.LBB27_113:
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
.LBB27_130:                             # %if.end638
	movq	%r12, %rdi
	movq	%r8, %rsi
	callq	convert
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	c_global_trees+128(%rip), %rax
	movq	%rax, (%r13)
	xorq	%r12, %r12
.LBB27_155:                             # %return
	movq	%r12, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB27_140:
	movq	%r12, %r15
	jmp	.LBB27_150
.Ltmp201:
	.size	shorten_compare, .Ltmp201-shorten_compare
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI27_0:
	.quad	.LBB27_17
	.quad	.LBB27_19
	.quad	.LBB27_18
	.quad	.LBB27_20
.LJTI27_1:
	.quad	.LBB27_66
	.quad	.LBB27_76
	.quad	.LBB27_71
	.quad	.LBB27_80
	.quad	.LBB27_65
	.quad	.LBB27_63

	.text
	.globl	pointer_int_sum
	.align	16, 0x90
	.type	pointer_int_sum,@function
pointer_int_sum:                        # @pointer_int_sum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp208:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp209:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp210:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp211:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp212:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp213:
	.cfi_def_cfa_offset 64
.Ltmp214:
	.cfi_offset %rbx, -48
.Ltmp215:
	.cfi_offset %r12, -40
.Ltmp216:
	.cfi_offset %r13, -32
.Ltmp217:
	.cfi_offset %r14, -24
.Ltmp218:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	8(%r14), %r12
	movq	8(%r12), %rax
	movzbl	16(%rax), %ecx
	movq	%rdx, %rbx
	movq	%rdi, %r15
	cmpq	$13, %rcx
	jg	.LBB28_5
# BB#1:                                 # %entry
	cmpq	$5, %rcx
	jne	.LBB28_17
# BB#2:                                 # %if.then
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB28_4
# BB#3:                                 # %if.then5
	movabsq	$.L.str43, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB28_4:                               # %if.end
	movq	global_trees+96(%rip), %r13
	jmp	.LBB28_18
.LBB28_5:                               # %entry
	cmpq	$14, %rcx
	je	.LBB28_14
# BB#6:                                 # %entry
	cmpq	$16, %rcx
	jne	.LBB28_7
# BB#11:                                # %if.then27
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB28_13
# BB#12:                                # %if.then31
	movabsq	$.L.str45, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB28_13:                              # %if.end32
	movq	global_trees+96(%rip), %r13
	jmp	.LBB28_18
.LBB28_14:                              # %if.then41
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB28_16
# BB#15:                                # %if.then45
	movabsq	$.L.str46, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB28_16:                              # %if.end46
	movq	global_trees+96(%rip), %r13
	jmp	.LBB28_18
.LBB28_7:                               # %entry
	cmpq	$23, %rcx
	jne	.LBB28_17
# BB#8:                                 # %if.then13
	movl	pedantic(%rip), %eax
	movl	warn_pointer_arith(%rip), %ecx
	orq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB28_10
# BB#9:                                 # %if.then17
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	callq	pedwarn
.LBB28_10:                              # %if.end18
	movq	global_trees+96(%rip), %r13
	jmp	.LBB28_18
.LBB28_17:                              # %if.else47
	movq	%rax, %rdi
	callq	size_in_bytes
	movq	%rax, %r13
.LBB28_18:                              # %if.end53
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	leaq	-59(%rcx), %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rsi
	cmpq	$1, %rsi
	ja	.LBB28_31
# BB#19:                                # %if.end53
	andq	$512, %rdx              # imm = 0x200
	testq	%rax, %rdx
	jne	.LBB28_31
# BB#20:                                # %land.lhs.true69
	movq	40(%rbx), %rsi
	movzbl	17(%rsi), %edx
	testq	$2, %rdx
	je	.LBB28_31
# BB#21:                                # %land.lhs.true76
	movzbl	17(%r13), %edx
	testq	$2, %rdx
	je	.LBB28_31
# BB#22:                                # %land.lhs.true83
	movq	32(%rbx), %rdx
	movq	8(%rdx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$6, %rdx
	jne	.LBB28_31
# BB#23:                                # %land.lhs.true94
	movq	8(%rbx), %rdi
	movzbl	17(%rdi), %edx
	testq	$32, %rdx
	je	.LBB28_25
# BB#24:                                # %lor.lhs.false102
	movq	8(%r14), %rdx
	movl	60(%rdi), %r8d
	movl	60(%rdx), %edx
	xorq	%r8, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	jne	.LBB28_31
.LBB28_25:                              # %if.then115
	cmpq	$60, %rcx
	movq	%r15, %rcx
	jne	.LBB28_30
# BB#26:                                # %if.then123
	andq	%r15, %rax
	cmpq	$59, %rax
	jne	.LBB28_27
# BB#28:                                # %if.then123
	movabsq	$1, %rcx
	jmp	.LBB28_29
.LBB28_27:
	movabsq	$0, %rcx
.LBB28_29:                              # %if.then123
	addq	$59, %rcx
.LBB28_30:                              # %if.end125
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	callq	convert
	movq	$1, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	build_binary_op
	movq	32(%rbx), %rsi
	movq	%rax, %r14
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	convert
	movq	%rax, %rbx
.LBB28_31:                              # %if.end135
	movq	8(%rbx), %rax
	movq	sizetype_tab(%rip), %rcx
	movl	60(%rax), %edx
	movl	60(%rcx), %edi
	xorq	%rdi, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	je	.LBB28_33
# BB#32:                                # %if.end135.if.then161_crit_edge
	movl	16(%rcx), %esi
	jmp	.LBB28_34
.LBB28_33:                              # %lor.lhs.false147
	movl	16(%rax), %edx
	movl	16(%rcx), %esi
	xorq	%rsi, %rdx
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB28_35
.LBB28_34:                              # %if.then161
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	andq	$511, %rdi              # imm = 0x1FF
	callq	type_for_size
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	convert
	movq	%rax, %rbx
	movq	8(%rbx), %rax
.LBB28_35:                              # %if.end173
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	convert
	movq	$61, %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	build_binary_op
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r13
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	movq	%r13, %rcx
	callq	build
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	fold
	cmpq	%rbx, %rax
	jne	.LBB28_37
# BB#36:                                # %if.then182
	movl	16(%r14), %ecx
	movl	16(%r13), %edx
	movl	16(%rbx), %esi
	andq	%rcx, %rdx
	andq	$512, %rdx              # imm = 0x200
	movabsq	$4294966783, %rcx       # imm = 0xFFFFFDFF
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 16(%rbx)
.LBB28_37:                              # %if.end197
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp219:
	.size	pointer_int_sum, .Ltmp219-pointer_int_sum
	.cfi_endproc

	.globl	truthvalue_conversion
	.align	16, 0x90
	.type	truthvalue_conversion,@function
truthvalue_conversion:                  # @truthvalue_conversion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp227:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp228:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp229:
	.cfi_def_cfa_offset 48
.Ltmp230:
	.cfi_offset %rbx, -40
.Ltmp231:
	.cfi_offset %r12, -32
.Ltmp232:
	.cfi_offset %r14, -24
.Ltmp233:
	.cfi_offset %r15, -16
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB29_1
.LBB29_31:                              # %sw.bb64
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	32(%rbx), %rdi
	.align	16, 0x90
.LBB29_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$24, %rax
	jle	.LBB29_2
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB29_1 Depth=1
	cmpq	$47, %rax
	jle	.LBB29_4
# BB#8:                                 # %tailrecurse
                                        #   in Loop: Header=BB29_1 Depth=1
	cmpq	$75, %rax
	jle	.LBB29_9
# BB#15:                                # %tailrecurse
                                        #   in Loop: Header=BB29_1 Depth=1
	addq	$-76, %rax
	andq	%r15, %rax
	cmpq	$49, %rax
	ja	.LBB29_48
# BB#16:                                # %tailrecurse
                                        #   in Loop: Header=BB29_1 Depth=1
	jmpq	*.LJTI29_0(,%rax,8)
.LBB29_32:                              # %sw.bb69
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	40(%rbx), %r14
	movzbl	17(%r14), %eax
	testq	$1, %rax
	jne	.LBB29_33
# BB#34:                                # %if.else88
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	32(%rbx), %rdi
	jmp	.LBB29_1
.LBB29_36:                              # %sw.bb107
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$15, %rcx
	je	.LBB29_48
# BB#37:                                # %lor.lhs.false
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	32(%rbx), %rdi
	movq	8(%rdi), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$15, %rdx
	jne	.LBB29_38
	jmp	.LBB29_48
.LBB29_17:                              # %tailrecurse.sw.bb129_crit_edge
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	32(%rbx), %rdi
	movq	8(%rbx), %rax
	movq	8(%rdi), %rcx
.LBB29_38:                              # %sw.bb129
                                        #   in Loop: Header=BB29_1 Depth=1
	movl	60(%rax), %eax
	movl	60(%rcx), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rax
	jae	.LBB29_1
	jmp	.LBB29_48
.LBB29_2:                               # %tailrecurse
	testq	%rax, %rax
	jne	.LBB29_48
	jmp	.LBB29_52
.LBB29_4:                               # %tailrecurse
	cmpq	$25, %rax
	jne	.LBB29_5
# BB#18:                                # %sw.bb7
	movq	%rbx, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	jne	.LBB29_19
# BB#20:                                # %sw.bb7
	movabsq	$c_global_trees+136, %rax
	movq	(%rax), %rbx
	jmp	.LBB29_52
.LBB29_9:                               # %tailrecurse
	cmpq	$48, %rax
	je	.LBB29_45
# BB#10:                                # %tailrecurse
	cmpq	$51, %rax
	jne	.LBB29_11
# BB#35:                                # %sw.bb93
	movq	c_global_trees+128(%rip), %r14
	movq	32(%rbx), %r15
	movq	40(%rbx), %rdi
	callq	truthvalue_conversion
	movq	48(%rbx), %rdi
	movq	%rax, %r12
	callq	truthvalue_conversion
	movq	%rax, %rbx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	build
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fold  # TAILCALL
.LBB29_5:                               # %tailrecurse
	cmpq	$26, %rax
	jne	.LBB29_48
# BB#6:                                 # %sw.bb8
	movq	%rbx, %rdi
	callq	real_zerop
	testq	%r15, %rax
	jne	.LBB29_7
# BB#22:                                # %sw.bb8
	movabsq	$c_global_trees+136, %rax
	movq	(%rax), %rbx
	jmp	.LBB29_52
.LBB29_19:
	movabsq	$c_global_trees+144, %rax
	movq	(%rax), %rbx
	jmp	.LBB29_52
.LBB29_45:                              # %sw.bb213
	movl	warn_parentheses(%rip), %eax
	testq	%rax, %rax
	je	.LBB29_48
# BB#46:                                # %land.lhs.true215
	movl	24(%rbx), %eax
	cmpq	$48, %rax
	jne	.LBB29_48
# BB#47:                                # %if.then219
	movabsq	$.L.str47, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB29_48
.LBB29_11:                              # %tailrecurse
	cmpq	$60, %rax
	jne	.LBB29_48
# BB#12:                                # %sw.bb152
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	je	.LBB29_48
.LBB29_13:                              # %sw.bb163
	movq	32(%rbx), %r14
	movq	40(%rbx), %rdx
	movq	8(%r14), %rsi
	movq	8(%rdx), %rax
	cmpq	%rax, %rsi
	je	.LBB29_14
# BB#39:                                # %if.end184
	movq	$115, %rdi
	callq	build1
	movq	%rax, %rdi
	callq	fold
	movq	$102, %rdi
	movq	$1, %rcx
	movq	%r14, %rsi
	jmp	.LBB29_40
.LBB29_7:
	movabsq	$c_global_trees+144, %rax
	movq	(%rax), %rbx
	jmp	.LBB29_52
.LBB29_51:                              # %sw.bb
	movq	c_global_trees+128(%rip), %rax
	movq	%rax, 8(%rbx)
.LBB29_52:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB29_14:                              # %if.then176
	movq	$102, %rdi
	movq	$1, %rcx
	movq	%r14, %rsi
	jmp	.LBB29_41
.LBB29_42:                              # %sw.bb199
	movq	40(%rbx), %rdi
	callq	integer_onep
	testq	%r15, %rax
	je	.LBB29_48
# BB#43:                                # %land.lhs.true205
	movq	8(%rbx), %rax
	movq	c_global_trees+128(%rip), %rsi
	cmpq	%rsi, %rax
	je	.LBB29_48
# BB#44:                                # %if.then210
	movq	$115, %rdi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build1  # TAILCALL
.LBB29_24:                              # %sw.bb15
	movq	32(%rbx), %rdx
	movl	16(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB29_26
# BB#25:                                # %land.lhs.true
	movzbl	49(%rdx), %ecx
	testq	$1, %rcx
	je	.LBB29_26
.LBB29_48:                              # %sw.epilog
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$8, %rax
	jne	.LBB29_50
# BB#49:                                # %if.then229
	movq	%rbx, %rdi
	callq	save_expr
	movl	16(%rbx), %ebx
	movq	%rax, %r14
	movq	$7, %rcx
	shrq	%cl, %rbx
	andq	$2, %rbx
	orq	$92, %rbx
	movq	$127, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	build_unary_op
	movq	%rax, %rdi
	callq	truthvalue_conversion
	movq	%rax, %r15
	movq	$128, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	build_unary_op
	movq	%rax, %rdi
	callq	truthvalue_conversion
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r15, %rsi
.LBB29_40:                              # %if.end184
	movq	%rax, %rdx
	jmp	.LBB29_41
.LBB29_50:                              # %if.end243
	movq	global_trees+88(%rip), %rdx
	movq	$102, %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
.LBB29_41:                              # %if.end184
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build_binary_op  # TAILCALL
.LBB29_30:                              # %sw.bb44
	movq	40(%rbx), %rax
	movl	16(%rax), %r14d
	movq	32(%rbx), %rdi
	movq	$7, %rcx
	shrq	%cl, %r14
	andq	$2, %r14
	orq	$92, %r14
	callq	truthvalue_conversion
	movq	40(%rbx), %rdi
	movq	%rax, %rbx
	callq	truthvalue_conversion
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	jmp	.LBB29_40
.LBB29_33:                              # %if.then79
	movq	c_global_trees+128(%rip), %r15
	movq	32(%rbx), %rdi
	callq	truthvalue_conversion
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	jmp	.LBB29_28
.LBB29_26:                              # %if.end30
	testq	$256, %rax              # imm = 0x100
	je	.LBB29_29
# BB#27:                                # %if.then39
	movq	c_global_trees+128(%rip), %rsi
	movq	c_global_trees+136(%rip), %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
.LBB29_28:                              # %if.then39
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB29_29:                              # %if.else
	movq	c_global_trees+136(%rip), %rbx
	jmp	.LBB29_52
.Ltmp234:
	.size	truthvalue_conversion, .Ltmp234-truthvalue_conversion
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI29_0:
	.quad	.LBB29_31
	.quad	.LBB29_31
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_31
	.quad	.LBB29_31
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_32
	.quad	.LBB29_32
	.quad	.LBB29_48
	.quad	.LBB29_13
	.quad	.LBB29_42
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_51
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_36
	.quad	.LBB29_17
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_24
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_48
	.quad	.LBB29_30

	.text
	.globl	c_build_qualified_type
	.align	16, 0x90
	.type	c_build_qualified_type,@function
c_build_qualified_type:                 # @c_build_qualified_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp240:
	.cfi_def_cfa_offset 32
.Ltmp241:
	.cfi_offset %rbx, -24
.Ltmp242:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	$4, %r14
	je	.LBB30_4
# BB#1:                                 # %land.lhs.true
	movl	16(%rbx), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	jne	.LBB30_3
# BB#2:                                 # %lor.lhs.false6
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB30_4
.LBB30_3:                               # %if.then
	movabsq	$.L.str48, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	andq	%rax, %r14
.LBB30_4:                               # %if.end
	movzbl	16(%rbx), %eax
	cmpq	$18, %rax
	jne	.LBB30_6
# BB#5:                                 # %if.then20
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	c_build_qualified_type
	movq	24(%rbx), %rsi
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build_array_type  # TAILCALL
.LBB30_6:                               # %if.end25
	movq	%rbx, %rdi
	movq	%r14, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build_qualified_type  # TAILCALL
.Ltmp243:
	.size	c_build_qualified_type, .Ltmp243-c_build_qualified_type
	.cfi_endproc

	.globl	c_apply_type_quals_to_decl
	.align	16, 0x90
	.type	c_apply_type_quals_to_decl,@function
c_apply_type_quals_to_decl:             # @c_apply_type_quals_to_decl
	.cfi_startproc
# BB#0:                                 # %entry
	testq	$1, %rdi
	jne	.LBB31_3
# BB#1:                                 # %lor.lhs.false
	movq	8(%rsi), %rax
	testq	%rax, %rax
	je	.LBB31_4
# BB#2:                                 # %land.lhs.true
	movzbl	16(%rax), %eax
	cmpq	$15, %rax
	jne	.LBB31_4
.LBB31_3:                               # %if.then
	movl	16(%rsi), %eax
	orq	$4096, %rax             # imm = 0x1000
	movl	%eax, 16(%rsi)
.LBB31_4:                               # %if.end
	testq	$2, %rdi
	je	.LBB31_6
# BB#5:                                 # %if.then10
	movl	16(%rsi), %eax
	orq	$2304, %rax             # imm = 0x900
	movl	%eax, 16(%rsi)
.LBB31_6:                               # %if.end19
	testq	$4, %rdi
	je	.LBB31_12
# BB#7:                                 # %if.then22
	movq	8(%rsi), %rax
	testq	%rax, %rax
	je	.LBB31_13
# BB#8:                                 # %lor.lhs.false26
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB31_13
# BB#9:                                 # %lor.lhs.false42
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB31_10
.LBB31_13:                              # %if.then52
	movabsq	$.L.str48, %rdi
	xorq	%rax, %rax
	jmp	error  # TAILCALL
.LBB31_10:                              # %if.else
	movl	flag_strict_aliasing(%rip), %eax
	testq	%rax, %rax
	je	.LBB31_12
# BB#11:                                # %if.then54
	movq	$-2, 192(%rsi)
.LBB31_12:                              # %if.end58
	retq
.Ltmp244:
	.size	c_apply_type_quals_to_decl, .Ltmp244-c_apply_type_quals_to_decl
	.cfi_endproc

	.globl	c_common_get_alias_set
	.align	16, 0x90
	.type	c_common_get_alias_set,@function
c_common_get_alias_set:                 # @c_common_get_alias_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp247:
	.cfi_def_cfa_offset 16
.Ltmp248:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%rbx, %rcx
	jmp	.LBB32_1
	.align	16, 0x90
.LBB32_3:                               # %for.body.for.inc_crit_edge
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	32(%rcx), %rcx
.LBB32_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rcx), %eax
	cmpq	$39, %rax
	jne	.LBB32_2
# BB#16:                                # %land.lhs.true
                                        #   in Loop: Header=BB32_1 Depth=1
	movq	32(%rcx), %rcx
	movq	8(%rcx), %rax
	movzbl	16(%rax), %edx
	xorq	%rax, %rax
	cmpq	$21, %rdx
	jne	.LBB32_1
	jmp	.LBB32_17
	.align	16, 0x90
.LBB32_2:                               # %for.cond
                                        #   in Loop: Header=BB32_1 Depth=1
	cmpq	$43, %rax
	je	.LBB32_3
# BB#4:                                 # %for.end
	movl	16(%rbx), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %eax
	cmpq	$116, %rax
	jne	.LBB32_5
# BB#6:                                 # %if.end28
	movq	integer_types(%rip), %rsi
	xorq	%rax, %rax
	cmpq	%rbx, %rsi
	je	.LBB32_17
# BB#7:                                 # %if.end28
	movq	integer_types+8(%rip), %rsi
	cmpq	%rbx, %rsi
	je	.LBB32_17
# BB#8:                                 # %if.end28
	movq	integer_types+16(%rip), %rsi
	cmpq	%rbx, %rsi
	je	.LBB32_17
# BB#9:                                 # %if.end37
	andq	$8447, %rdx             # imm = 0x20FF
	cmpq	$8198, %rdx             # imm = 0x2006
	jne	.LBB32_13
# BB#10:                                # %if.then48
	movq	%rbx, %rdi
	callq	signed_type
	jmp	.LBB32_11
.LBB32_5:
	movq	$-1, %rax
	popq	%rbx
	retq
.LBB32_13:                              # %if.else
	orq	$2, %rcx
	cmpq	$15, %rcx
	jne	.LBB32_14
# BB#15:                                # %if.then67
	movq	%rbx, %rdi
	callq	build_type_no_quals
.LBB32_11:                              # %if.then48
	movq	%rax, %rcx
	cmpq	%rbx, %rcx
	movq	$-1, %rax
	je	.LBB32_17
# BB#12:                                # %if.then51
	movq	%rcx, %rdi
	popq	%rbx
	jmp	get_alias_set  # TAILCALL
.LBB32_17:                              # %return
	popq	%rbx
	retq
.LBB32_14:
	movq	$-1, %rax
	popq	%rbx
	retq
.Ltmp249:
	.size	c_common_get_alias_set, .Ltmp249-c_common_get_alias_set
	.cfi_endproc

	.globl	c_alignof
	.align	16, 0x90
	.type	c_alignof,@function
c_alignof:                              # @c_alignof
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp251:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %eax
	cmpq	$15, %rax
	jne	.LBB33_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movzbl	16(%rdi), %eax
.LBB33_2:                               # %if.end
	testq	%rax, %rax
	je	.LBB33_6
# BB#3:                                 # %if.end
	cmpq	$5, %rax
	jne	.LBB33_4
.LBB33_6:                               # %if.then12
	movq	global_trees+128(%rip), %rdx
	jmp	.LBB33_11
.LBB33_4:                               # %if.end
	cmpq	$23, %rax
	jne	.LBB33_7
# BB#5:                                 # %if.then9
	movq	$2, %rdi
	jmp	.LBB33_10
.LBB33_7:                               # %if.else13
	cmpq	$0, 32(%rdi)
	je	.LBB33_8
# BB#9:                                 # %if.else17
	movl	64(%rdi), %edi
	movq	$3, %rcx
	shrq	%cl, %rdi
.LBB33_10:                              # %if.end22
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, %rdx
	jmp	.LBB33_11
.LBB33_8:                               # %if.then16
	movabsq	$.L.str49, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+120(%rip), %rdx
.LBB33_11:                              # %if.end22
	movq	c_global_trees+32(%rip), %rsi
	movq	$115, %rdi
	callq	build1
	movq	%rax, %rdi
	popq	%rax
	jmp	fold  # TAILCALL
.Ltmp252:
	.size	c_alignof, .Ltmp252-c_alignof
	.cfi_endproc

	.globl	c_alignof_expr
	.align	16, 0x90
	.type	c_alignof_expr,@function
c_alignof_expr:                         # @c_alignof_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp254:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %eax
	cmpq	$41, %rax
	je	.LBB34_10
# BB#1:                                 # %entry
	cmpq	$39, %rax
	jne	.LBB34_2
# BB#5:                                 # %land.lhs.true
	movq	40(%rdi), %rax
	movzbl	53(%rax), %eax
	testq	$2, %rax
	je	.LBB34_8
# BB#6:                                 # %if.then14
	movabsq	$.L.str50, %rdi
	xorq	%rax, %rax
	callq	error
	movq	global_trees+128(%rip), %rdx
	jmp	.LBB34_7
.LBB34_10:                              # %if.then51
	movq	32(%rdi), %r8
	movzbl	16(%r8), %ecx
	cmpq	$115, %rcx
	jne	.LBB34_11
# BB#12:                                # %land.rhs.lr.ph
	movq	8(%r8), %rcx
	movq	8(%rcx), %rcx
	movl	64(%rcx), %eax
	movq	%r8, %rdx
	.align	16, 0x90
.LBB34_13:                              # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdx), %rdx
	movq	8(%rdx), %rcx
	movzbl	16(%rcx), %edi
	cmpq	$13, %rdi
	jne	.LBB34_14
# BB#15:                                # %while.body
                                        #   in Loop: Header=BB34_13 Depth=1
	movq	8(%rcx), %rcx
	movslq	64(%rcx), %rdi
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	movq	%rdx, %rcx
	jg	.LBB34_17
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB34_13 Depth=1
	movq	%r8, %rcx
.LBB34_17:                              # %while.body
                                        #   in Loop: Header=BB34_13 Depth=1
	jge	.LBB34_19
# BB#18:                                # %while.body
                                        #   in Loop: Header=BB34_13 Depth=1
	movq	%rax, %rdi
.LBB34_19:                              # %while.body
                                        #   in Loop: Header=BB34_13 Depth=1
	movzbl	16(%rdx), %eax
	cmpq	$115, %rax
	movq	%rdi, %rax
	movq	%rcx, %r8
	je	.LBB34_13
	jmp	.LBB34_20
.LBB34_2:                               # %entry
	cmpq	$34, %rax
	jne	.LBB34_21
# BB#3:                                 # %if.then
	movl	56(%rdi), %edi
	jmp	.LBB34_4
.LBB34_11:
	movq	%r8, %rcx
	jmp	.LBB34_20
.LBB34_8:                               # %land.lhs.true22
	movq	40(%rdi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB34_21
# BB#9:                                 # %if.then32
	movl	56(%rax), %edi
.LBB34_4:                               # %if.end100
	movq	$3, %rcx
	shrq	%cl, %rdi
	andq	$2097151, %rdi          # imm = 0x1FFFFF
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, %rdx
.LBB34_7:                               # %if.end100
	movq	c_global_trees+32(%rip), %rsi
	movq	$115, %rdi
	callq	build1
	movq	%rax, %rdi
	popq	%rax
	jmp	fold  # TAILCALL
.LBB34_21:                              # %if.else94
	movq	8(%rdi), %rdi
	popq	%rax
	jmp	c_alignof  # TAILCALL
.LBB34_14:
	movq	%r8, %rcx
.LBB34_20:                              # %while.end
	movq	8(%rcx), %rax
	movq	8(%rax), %rdi
	popq	%rax
	jmp	c_alignof  # TAILCALL
.Ltmp255:
	.size	c_alignof_expr, .Ltmp255-c_alignof_expr
	.cfi_endproc

	.globl	c_common_nodes_and_builtins
	.align	16, 0x90
	.type	c_common_nodes_and_builtins,@function
c_common_nodes_and_builtins:            # @c_common_nodes_and_builtins
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp262:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp263:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp264:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp265:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp266:
	.cfi_def_cfa_offset 48
	subq	$464, %rsp              # imm = 0x1D0
.Ltmp267:
	.cfi_def_cfa_offset 512
.Ltmp268:
	.cfi_offset %rbx, -48
.Ltmp269:
	.cfi_offset %r12, -40
.Ltmp270:
	.cfi_offset %r13, -32
.Ltmp271:
	.cfi_offset %r14, -24
.Ltmp272:
	.cfi_offset %r15, -16
	movq	integer_types+40(%rip), %rdx
	movq	$c_format_attribute_table, format_attribute_table(%rip)
	movq	$27, %rdi
	xorq	%rsi, %rsi
	callq	record_builtin_type
	movq	integer_types(%rip), %rdx
	movabsq	$.L.str51, %rsi
	movq	$28, %rdi
	callq	record_builtin_type
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB35_2
# BB#1:                                 # %if.then
	movq	integer_types+40(%rip), %rdx
	movq	$10, %rdi
	xorq	%rsi, %rsi
	callq	record_builtin_type
.LBB35_2:                               # %if.end
	movq	integer_types+56(%rip), %rdx
	movabsq	$.L.str52, %r12
	movq	$2, %rdi
	movq	%r12, %rsi
	callq	record_builtin_type
	movq	integer_types+48(%rip), %rdx
	movabsq	$.L.str53, %rbx
	movq	$1, %rdi
	movq	%rbx, %rsi
	callq	record_builtin_type
	movq	integer_types+64(%rip), %rdx
	movabsq	$.L.str54, %r15
	movq	$114, %rdi
	movq	%r15, %rsi
	callq	record_builtin_type
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB35_4
# BB#3:                                 # %if.then2
	movq	integer_types+64(%rip), %rdx
	movabsq	$.L.str55, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
.LBB35_4:                               # %if.end3
	movq	integer_types+72(%rip), %rdx
	movabsq	$.L.str56, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
	movq	integer_types+80(%rip), %rdx
	movabsq	$.L.str57, %r14
	movq	$114, %rdi
	movq	%r14, %rsi
	callq	record_builtin_type
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB35_6
# BB#5:                                 # %if.then5
	movq	integer_types+80(%rip), %rdx
	movabsq	$.L.str58, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
.LBB35_6:                               # %if.end6
	movq	integer_types+24(%rip), %rdx
	movabsq	$.L.str59, %rsi
	movq	$7, %rdi
	callq	record_builtin_type
	movq	integer_types+32(%rip), %rdx
	movabsq	$.L.str60, %r13
	movq	$114, %rdi
	movq	%r13, %rsi
	callq	record_builtin_type
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB35_8
# BB#7:                                 # %if.then8
	movq	integer_types+32(%rip), %rdx
	movabsq	$.L.str61, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
.LBB35_8:                               # %if.end9
	movq	integer_types+8(%rip), %rdx
	movabsq	$.L.str62, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
	movq	integer_types+16(%rip), %rdx
	movabsq	$.L.str63, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
	movq	global_trees+8(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+16(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+24(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+32(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str64, %rdi
	callq	get_identifier
	movq	global_trees+40(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+48(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+56(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+64(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+72(%rip), %rdx
	movq	$33, %rdi
	xorq	%rsi, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str65, %rdi
	callq	get_identifier
	movq	global_trees+80(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	$128, %rdi
	callq	make_signed_type
	movq	%rax, c_global_trees+72(%rip)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	$128, %rdi
	callq	make_unsigned_type
	movq	%rax, c_global_trees+80(%rip)
	movq	$33, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB35_10
# BB#9:                                 # %if.end9
	movq	%rbx, %r15
.LBB35_10:                              # %if.end9
	movq	%r15, %rdi
	callq	get_identifier
	movq	%rax, %rdi
	callq	identifier_global_value
	movq	8(%rax), %rbx
	movq	%rbx, c_global_trees+32(%rip)
	movq	%rbx, %rdi
	callq	signed_type
	movl	flag_traditional(%rip), %ecx
	movq	%rax, c_global_trees+40(%rip)
	testq	%rcx, %rcx
	je	.LBB35_12
# BB#11:                                # %if.then41
	movq	%rax, c_global_trees+32(%rip)
	movq	%rax, %rbx
.LBB35_12:                              # %if.end42
	movq	%rbx, %rdi
	callq	set_sizetype
	movl	flag_short_double(%rip), %edi
	callq	build_common_tree_nodes_2
	movq	global_trees+192(%rip), %rdx
	movq	$29, %rdi
	xorq	%rsi, %rsi
	callq	record_builtin_type
	movq	global_trees+200(%rip), %rdx
	movq	$30, %rdi
	xorq	%rsi, %rsi
	callq	record_builtin_type
	movq	global_trees+208(%rip), %rdx
	movabsq	$.L.str66, %rsi
	movq	$114, %rdi
	callq	record_builtin_type
	movabsq	$.L.str67, %rdi
	callq	get_identifier
	movq	global_trees+160(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str68, %rdi
	callq	get_identifier
	movq	global_trees+168(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str69, %rdi
	callq	get_identifier
	movq	global_trees+176(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str70, %rdi
	callq	get_identifier
	movq	global_trees+184(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+216(%rip), %rdx
	movq	$31, %rdi
	xorq	%rsi, %rsi
	callq	record_builtin_type
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	global_trees+216(%rip), %rcx
	movq	%rax, c_global_trees+248(%rip)
	movq	%rcx, 8(%rax)
	callq	build_void_list_node
	movq	%rax, global_trees+256(%rip)
	movq	$200, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, %rdi
	callq	build_index_type
	movq	integer_types(%rip), %rdi
	movq	%rax, %r15
	movq	%r15, %rsi
	callq	build_array_type
	movq	integer_types+40(%rip), %rdi
	movq	%rax, c_global_trees+88(%rip)
	movq	%r15, %rsi
	callq	build_array_type
	movq	integer_types(%rip), %rdi
	movq	%rax, c_global_trees+104(%rip)
	callq	build_pointer_type
	movq	integer_types(%rip), %rdi
	movq	%rax, c_global_trees+112(%rip)
	movq	$1, %rsi
	callq	build_qualified_type
	movq	%rax, %rdi
	callq	build_pointer_type
	movq	%rax, 248(%rsp)         # 8-byte Spill
	movl	flag_traditional(%rip), %ecx
	movq	%rax, c_global_trees+120(%rip)
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	je	.LBB35_14
# BB#13:                                # %if.end42
	movq	%rdx, %rax
.LBB35_14:                              # %if.end42
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	je	.LBB35_16
# BB#15:                                # %if.end42
	movq	%rdx, %rcx
.LBB35_16:                              # %if.end42
	andq	%rcx, %rax
	jne	.LBB35_17
# BB#18:                                # %if.end42
	movabsq	$global_trees+224, %rcx
	jmp	.LBB35_19
.LBB35_17:
	movabsq	$c_global_trees+112, %rcx
.LBB35_19:                              # %if.end42
	movq	(%rcx), %rcx
	movq	%rcx, 368(%rsp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB35_21
# BB#20:                                # %if.end42
	movq	global_trees+232(%rip), %rax
	movq	%rax, 248(%rsp)         # 8-byte Spill
.LBB35_21:                              # %if.end42
	callq	*targetm+304(%rip)
	movl	flag_short_wchar(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_23
# BB#22:                                # %select.mid
	movabsq	$.L.str71, %r13
.LBB35_23:                              # %select.end
	movq	%r13, %rdi
	callq	get_identifier
	movq	%rax, c_global_trees(%rip)
	movq	%rax, %rdi
	callq	identifier_global_value
	movq	8(%rax), %rbx
	movl	c_language(%rip), %eax
	movq	%rbx, c_global_trees(%rip)
	cmpq	$1, %rax
	jne	.LBB35_28
# BB#24:                                # %if.then84
	movl	60(%rbx), %edi
	movzbl	17(%rbx), %eax
	andq	$511, %rdi              # imm = 0x1FF
	testq	$32, %rax
	je	.LBB35_26
# BB#25:                                # %if.then89
	callq	make_unsigned_type
	jmp	.LBB35_27
.LBB35_28:                              # %if.else93
	movq	%rbx, %rdi
	callq	signed_type
	movq	%rax, c_global_trees+8(%rip)
	movq	%rbx, %rdi
	callq	unsigned_type
	movq	%rax, c_global_trees+16(%rip)
	jmp	.LBB35_29
.LBB35_26:                              # %if.else
	callq	make_signed_type
.LBB35_27:                              # %if.end92
	movq	%rax, c_global_trees(%rip)
	movabsq	$.L.str72, %rsi
	movq	$66, %rdi
	movq	%rax, %rdx
	callq	record_builtin_type
	movq	c_global_trees(%rip), %rbx
.LBB35_29:                              # %if.end96
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	build_array_type
	movq	%rax, c_global_trees+96(%rip)
	movabsq	$.L.str53, %rdi
	callq	get_identifier
	movq	%rax, %rdi
	callq	identifier_global_value
	movl	target_flags(%rip), %edx
	movq	8(%rax), %rax
	movq	%rax, c_global_trees+24(%rip)
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%r12, %rdi
	jne	.LBB35_31
# BB#30:                                # %if.end96
	movabsq	$.L.str56, %rdi
.LBB35_31:                              # %if.end96
	callq	get_identifier
	movq	%rax, %rdi
	callq	identifier_global_value
	movl	target_flags(%rip), %edx
	movq	8(%rax), %rax
	movq	%rax, c_global_trees+56(%rip)
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	je	.LBB35_33
# BB#32:
	movabsq	$.L.str54, %r14
.LBB35_33:                              # %if.end96
	movq	%r14, %rdi
	callq	get_identifier
	movq	%rax, %rdi
	callq	identifier_global_value
	movq	8(%rax), %rax
	movq	integer_types+40(%rip), %rdi
	movq	%rax, c_global_trees+64(%rip)
	xorq	%rsi, %rsi
	callq	build_function_type
	movl	target_flags(%rip), %edx
	movq	%rax, c_global_trees+176(%rip)
	andq	$33554432, %rdx         # imm = 0x2000000
	movq	$25, %rcx
	shrq	%cl, %rdx
	testq	%rdx, %rdx
	jne	.LBB35_35
# BB#34:                                # %if.end96
	movabsq	$.L.str71, %r12
.LBB35_35:                              # %if.end96
	movq	%r12, %rdi
	callq	get_identifier
	movq	%rax, %rdi
	callq	identifier_global_value
	movq	8(%rax), %rdi
	movq	%rdi, global_trees+240(%rip)
	callq	unsigned_type
	movq	%rax, c_global_trees+48(%rip)
	movabsq	$.L.str73, %rdi
	callq	get_identifier
	movq	global_trees+248(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str74, %rdi
	callq	get_identifier
	movq	global_trees+240(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movabsq	$.L.str75, %rdi
	callq	get_identifier
	movq	sizetype_tab(%rip), %rdx
	movq	$33, %rdi
	movq	%rax, %rsi
	callq	build_decl
	movq	%rax, %rdi
	callq	pushdecl
	movq	global_trees+248(%rip), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$18, %rax
	jne	.LBB35_37
# BB#36:                                # %if.then142
	movq	8(%rdi), %rdi
	callq	build_pointer_type
	movq	%rax, 240(%rsp)         # 8-byte Spill
	jmp	.LBB35_38
.LBB35_37:                              # %if.else146
	movq	%rdi, 240(%rsp)         # 8-byte Spill
	callq	build_reference_type
.LBB35_38:                              # %if.end148
	movq	%rax, 408(%rsp)         # 8-byte Spill
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rcx
	je	.LBB35_40
# BB#39:                                # %if.end148
	movq	%rdx, %rcx
.LBB35_40:                              # %if.end148
	movl	c_language(%rip), %esi
	cmpq	$1, %rsi
	je	.LBB35_42
# BB#41:                                # %if.end148
	movq	%rdx, %rax
.LBB35_42:                              # %if.end148
	movq	integer_types+40(%rip), %rdx
	movq	%rdx, 312(%rsp)         # 8-byte Spill
	testq	%rax, %rcx
	movq	%rdx, %rax
	jne	.LBB35_44
# BB#43:                                # %if.end148
	movq	sizetype_tab(%rip), %rax
.LBB35_44:                              # %if.end148
	movq	%rax, 400(%rsp)         # 8-byte Spill
	movq	global_trees+216(%rip), %r15
	movq	%r15, 392(%rsp)         # 8-byte Spill
	movq	integer_types+48(%rip), %r13
	movq	%r13, 416(%rsp)         # 8-byte Spill
	movq	integer_types+56(%rip), %r14
	movq	%r14, 224(%rsp)         # 8-byte Spill
	movq	integer_types+72(%rip), %r12
	movq	global_trees+192(%rip), %rax
	movq	%rax, 296(%rsp)         # 8-byte Spill
	movq	c_global_trees+56(%rip), %rax
	movq	%rax, 440(%rsp)         # 8-byte Spill
	movq	global_trees+200(%rip), %rax
	movq	%rax, 328(%rsp)         # 8-byte Spill
	movq	global_trees+208(%rip), %rax
	movq	%rax, 456(%rsp)         # 8-byte Spill
	movq	global_trees+168(%rip), %rax
	movq	%rax, 336(%rsp)         # 8-byte Spill
	movq	global_trees+176(%rip), %rax
	movq	%rax, 344(%rsp)         # 8-byte Spill
	movq	global_trees+184(%rip), %rax
	movq	%rax, 360(%rsp)         # 8-byte Spill
	movq	global_trees+224(%rip), %rbx
	movq	%rbx, 384(%rsp)         # 8-byte Spill
	movq	global_trees+232(%rip), %rax
	movq	%rax, 320(%rsp)         # 8-byte Spill
	movl	ptr_mode(%rip), %edi
	xorq	%rsi, %rsi
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	callq	type_for_mode
	movq	c_global_trees+32(%rip), %rcx
	movq	%rcx, 448(%rsp)         # 8-byte Spill
	movq	c_global_trees+112(%rip), %rcx
	movq	%rcx, 376(%rsp)         # 8-byte Spill
	movq	c_global_trees+120(%rip), %rcx
	movq	%rcx, 304(%rsp)         # 8-byte Spill
	movq	global_trees+256(%rip), %rsi
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	%r15, %rdi
	callq	build_function_type
	movq	global_trees+256(%rip), %rsi
	movq	%rax, 352(%rsp)         # 8-byte Spill
	movq	%rbx, %rdi
	callq	build_function_type
	movq	global_trees+256(%rip), %rsi
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 80(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 232(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 144(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	440(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 216(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	296(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r15
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 432(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	328(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r12
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 440(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	456(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 424(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	336(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 200(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	344(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r13
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 208(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	360(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 192(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 336(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 344(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	movq	456(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 328(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	movq	384(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 104(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	448(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	312(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r15
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 456(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 112(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	movq	392(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 256(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	304(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	400(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 152(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r13
	callq	tree_cons
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 280(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rbx
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 56(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	408(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 64(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r15, %rbx
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r15
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 96(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	376(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 360(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r15, %r14
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 168(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 296(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	448(%rsp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 288(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 416(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	384(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 272(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r14, %rdi
	movq	%r14, %r12
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 264(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r15
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	movq	392(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 32(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	240(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	408(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rbx, %r14
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	224(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 40(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	xorq	%rdi, %rdi
	movq	400(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	368(%rsp), %rsi         # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 128(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	448(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	376(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 376(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rbx
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 160(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r14, %rsi
	movq	%r14, %r15
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	320(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	384(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	368(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 240(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r12, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 184(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r15
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 176(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	400(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	248(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, 120(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rbx
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	392(%rsp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	global_trees+256(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rbx, %r12
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	448(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	320(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 248(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r13, %r15
	movq	%r15, %rdi
	callq	build_function_type
	movq	312(%rsp), %r13         # 8-byte Reload
	movq	%rax, 224(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	build_function_type
	movq	%rax, 368(%rsp)         # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	build_function_type
	movq	%rax, 88(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	408(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	movq	%r15, %rdi
	movq	%r15, %rbx
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r14, %rsi
	callq	tree_cons
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 392(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	304(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 400(%rsp)         # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 408(%rsp)         # 8-byte Spill
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	build_pointer_type
	movq	global_trees+256(%rip), %rdx
	movq	%rax, %rbx
	xorq	%rdi, %rdi
	movq	448(%rsp), %rsi         # 8-byte Reload
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	136(%rsp), %rbx         # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	build_function_type
	movq	%rax, 448(%rsp)         # 8-byte Spill
	movabsq	$.L.str76, %rdi
	movabsq	$.L.str76+10, %r8
	xorq	%rdx, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r14
	testq	%rcx, %rcx
	jne	.LBB35_51
# BB#45:                                # %land.lhs.true10.i
	movq	%rbx, %r13
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_49
# BB#46:
	movabsq	$.L.str76+10, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_48:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB35_51
# BB#47:                                # %for.cond.i.i
                                        #   in Loop: Header=BB35_48 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_48
.LBB35_49:                              # %land.lhs.true12.i
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	%r13, %rsi
	jne	.LBB35_51
# BB#50:                                # %if.then16.i
	movabsq	$.L.str76+10, %rdi
	xorq	%rdx, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movl	16(%rax), %ecx
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%rax)
.LBB35_51:                              # %builtin_function_2.exit
	testq	%r14, %r14
	jne	.LBB35_53
# BB#52:                                # %builtin_function_2.exit
	movq	16(%rsp), %r14          # 8-byte Reload
.LBB35_53:                              # %builtin_function_2.exit
	movq	%r14, built_in_decls(%rip)
	movabsq	$.L.str79, %rdi
	movq	$1, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	movq	232(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_61
# BB#54:                                # %land.lhs.true10.i1768
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_59
# BB#55:
	movabsq	$.L.str79+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_57:                              # %for.body.i.i1777
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_58
# BB#56:                                # %for.cond.i.i1772
                                        #   in Loop: Header=BB35_57 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_57
.LBB35_59:                              # %land.lhs.true12.i1779
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	232(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_61
# BB#60:                                # %if.then16.i1781
	movabsq	$.L.str79+10, %rdi
	movq	$1, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
	jmp	.LBB35_61
.LBB35_58:
	movq	232(%rsp), %rbx         # 8-byte Reload
.LBB35_61:                              # %builtin_function_2.exit1788
	testq	%r15, %r15
	jne	.LBB35_63
# BB#62:                                # %builtin_function_2.exit1788
	movq	%r14, %r15
.LBB35_63:                              # %builtin_function_2.exit1788
	movq	%r15, built_in_decls+8(%rip)
	movabsq	$.L.str80, %rdi
	movq	$2, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB35_70
# BB#64:                                # %land.lhs.true10.i1793
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_68
# BB#65:
	movabsq	$.L.str80+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_67:                              # %for.body.i.i1802
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_70
# BB#66:                                # %for.cond.i.i1797
                                        #   in Loop: Header=BB35_67 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_67
.LBB35_68:                              # %land.lhs.true12.i1804
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_70
# BB#69:                                # %if.then16.i1806
	movabsq	$.L.str80+10, %rdi
	movq	$2, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	232(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_70:                              # %builtin_function_2.exit1813
	testq	%r15, %r15
	jne	.LBB35_72
# BB#71:                                # %builtin_function_2.exit1813
	movq	%r14, %r15
.LBB35_72:                              # %builtin_function_2.exit1813
	movq	%r15, built_in_decls+16(%rip)
	movabsq	$.L.str81, %rdi
	movq	$3, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB35_79
# BB#73:                                # %land.lhs.true10.i1818
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_77
# BB#74:
	movabsq	$.L.str81+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_76:                              # %for.body.i.i1827
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_79
# BB#75:                                # %for.cond.i.i1822
                                        #   in Loop: Header=BB35_76 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_76
.LBB35_77:                              # %land.lhs.true12.i1829
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_79
# BB#78:                                # %if.then16.i1831
	movabsq	$.L.str81+10, %rdi
	movq	$3, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_79:                              # %builtin_function_2.exit1838
	testq	%r15, %r15
	jne	.LBB35_81
# BB#80:                                # %builtin_function_2.exit1838
	movq	%r14, %r15
.LBB35_81:                              # %builtin_function_2.exit1838
	movq	%r15, built_in_decls+24(%rip)
	movabsq	$.L.str82, %rdi
	movq	$4, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB35_88
# BB#82:                                # %land.lhs.true10.i1843
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_86
# BB#83:
	movabsq	$.L.str82+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_85:                              # %for.body.i.i1852
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_88
# BB#84:                                # %for.cond.i.i1847
                                        #   in Loop: Header=BB35_85 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_85
.LBB35_86:                              # %land.lhs.true12.i1854
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_88
# BB#87:                                # %if.then16.i1856
	movabsq	$.L.str82+10, %rdi
	movq	$4, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_88:                              # %builtin_function_2.exit1863
	testq	%r15, %r15
	jne	.LBB35_90
# BB#89:                                # %builtin_function_2.exit1863
	movq	%r14, %r15
.LBB35_90:                              # %builtin_function_2.exit1863
	movq	%r15, built_in_decls+32(%rip)
	movabsq	$.L.str83, %rdi
	movq	$5, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB35_97
# BB#91:                                # %land.lhs.true10.i1868
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_95
# BB#92:
	movabsq	$.L.str83+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_94:                              # %for.body.i.i1877
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_97
# BB#93:                                # %for.cond.i.i1872
                                        #   in Loop: Header=BB35_94 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_94
.LBB35_95:                              # %land.lhs.true12.i1879
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_97
# BB#96:                                # %if.then16.i1881
	movabsq	$.L.str83+10, %rdi
	movq	$5, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_97:                              # %builtin_function_2.exit1888
	testq	%r15, %r15
	movq	144(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_99
# BB#98:                                # %builtin_function_2.exit1888
	movq	%r14, %r15
.LBB35_99:                              # %builtin_function_2.exit1888
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+40(%rip)
	testq	%rax, %rax
	je	.LBB35_100
# BB#101:                               # %select.mid8
	xorq	%r8, %r8
	jmp	.LBB35_102
.LBB35_100:
	movabsq	$.L.str84+10, %r8
.LBB35_102:                             # %select.end7
	movabsq	$.L.str84, %rdi
	movq	$6, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_109
# BB#103:                               # %land.lhs.true10.i1893
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_107
# BB#104:
	movabsq	$.L.str84+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_106:                             # %for.body.i.i1902
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_109
# BB#105:                               # %for.cond.i.i1897
                                        #   in Loop: Header=BB35_106 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_106
.LBB35_107:                             # %land.lhs.true12.i1904
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_109
# BB#108:                               # %if.then16.i1906
	movabsq	$.L.str84+10, %rdi
	movq	$6, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	144(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_109:                             # %builtin_function_2.exit1913
	testq	%r15, %r15
	jne	.LBB35_111
# BB#110:                               # %builtin_function_2.exit1913
	movq	%r14, %r15
.LBB35_111:                             # %builtin_function_2.exit1913
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+48(%rip)
	testq	%rax, %rax
	movq	216(%rsp), %rsi         # 8-byte Reload
	je	.LBB35_112
# BB#113:                               # %select.mid11
	xorq	%r8, %r8
	jmp	.LBB35_114
.LBB35_112:
	movabsq	$.L.str85+10, %r8
.LBB35_114:                             # %select.end10
	movabsq	$.L.str85, %rdi
	movq	$7, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_121
# BB#115:                               # %land.lhs.true10.i1919
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_119
# BB#116:
	movabsq	$.L.str85+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_118:                             # %for.body.i.i1928
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_121
# BB#117:                               # %for.cond.i.i1923
                                        #   in Loop: Header=BB35_118 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_118
.LBB35_119:                             # %land.lhs.true12.i1930
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	216(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_121
# BB#120:                               # %if.then16.i1932
	movabsq	$.L.str85+10, %rdi
	movq	$7, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_121:                             # %builtin_function_2.exit1939
	testq	%r15, %r15
	jne	.LBB35_123
# BB#122:                               # %builtin_function_2.exit1939
	movq	%r14, %r15
.LBB35_123:                             # %builtin_function_2.exit1939
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+56(%rip)
	testq	%rax, %rax
	movq	208(%rsp), %rsi         # 8-byte Reload
	je	.LBB35_124
# BB#125:                               # %select.mid14
	xorq	%r8, %r8
	jmp	.LBB35_126
.LBB35_124:
	movabsq	$.L.str86+10, %r8
.LBB35_126:                             # %select.end13
	movabsq	$.L.str86, %rdi
	movq	$8, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_133
# BB#127:                               # %land.lhs.true10.i1945
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_131
# BB#128:
	movabsq	$.L.str86+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_130:                             # %for.body.i.i1954
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_133
# BB#129:                               # %for.cond.i.i1949
                                        #   in Loop: Header=BB35_130 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_130
.LBB35_131:                             # %land.lhs.true12.i1956
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	208(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_133
# BB#132:                               # %if.then16.i1958
	movabsq	$.L.str86+10, %rdi
	movq	$8, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_133:                             # %builtin_function_2.exit1965
	testq	%r15, %r15
	jne	.LBB35_135
# BB#134:                               # %builtin_function_2.exit1965
	movq	%r14, %r15
.LBB35_135:                             # %builtin_function_2.exit1965
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+64(%rip)
	testq	%rax, %rax
	movq	200(%rsp), %rsi         # 8-byte Reload
	je	.LBB35_136
# BB#137:                               # %select.mid17
	xorq	%r8, %r8
	jmp	.LBB35_138
.LBB35_136:
	movabsq	$.L.str87+10, %r8
.LBB35_138:                             # %select.end16
	movabsq	$.L.str87, %rdi
	movq	$9, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_145
# BB#139:                               # %land.lhs.true10.i1971
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_143
# BB#140:
	movabsq	$.L.str87+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_142:                             # %for.body.i.i1980
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_145
# BB#141:                               # %for.cond.i.i1975
                                        #   in Loop: Header=BB35_142 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_142
.LBB35_143:                             # %land.lhs.true12.i1982
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	200(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_145
# BB#144:                               # %if.then16.i1984
	movabsq	$.L.str87+10, %rdi
	movq	$9, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_145:                             # %builtin_function_2.exit1991
	testq	%r15, %r15
	jne	.LBB35_147
# BB#146:                               # %builtin_function_2.exit1991
	movq	%r14, %r15
.LBB35_147:                             # %builtin_function_2.exit1991
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+72(%rip)
	testq	%rax, %rax
	movq	192(%rsp), %rsi         # 8-byte Reload
	je	.LBB35_148
# BB#149:                               # %select.mid20
	xorq	%r8, %r8
	jmp	.LBB35_150
.LBB35_148:
	movabsq	$.L.str88+10, %r8
.LBB35_150:                             # %select.end19
	movabsq	$.L.str88, %rdi
	movq	$10, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_157
# BB#151:                               # %land.lhs.true10.i1997
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_155
# BB#152:
	movabsq	$.L.str88+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_154:                             # %for.body.i.i2006
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_157
# BB#153:                               # %for.cond.i.i2001
                                        #   in Loop: Header=BB35_154 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_154
.LBB35_155:                             # %land.lhs.true12.i2008
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	192(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_157
# BB#156:                               # %if.then16.i2010
	movabsq	$.L.str88+10, %rdi
	movq	$10, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_157:                             # %builtin_function_2.exit2017
	testq	%r15, %r15
	jne	.LBB35_159
# BB#158:                               # %builtin_function_2.exit2017
	movq	%r14, %r15
.LBB35_159:                             # %builtin_function_2.exit2017
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+80(%rip)
	testq	%rax, %rax
	je	.LBB35_160
# BB#161:                               # %select.mid23
	xorq	%r8, %r8
	jmp	.LBB35_162
.LBB35_160:
	movabsq	$.L.str89+10, %r8
.LBB35_162:                             # %select.end22
	movq	344(%rsp), %rsi         # 8-byte Reload
	movabsq	$.L.str89, %rdi
	movq	$11, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_169
# BB#163:                               # %land.lhs.true10.i2023
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_167
# BB#164:
	movabsq	$.L.str89+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_166:                             # %for.body.i.i2032
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_169
# BB#165:                               # %for.cond.i.i2027
                                        #   in Loop: Header=BB35_166 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_166
.LBB35_167:                             # %land.lhs.true12.i2034
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_169
# BB#168:                               # %if.then16.i2036
	movabsq	$.L.str89+10, %rdi
	movq	$11, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	344(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_169:                             # %builtin_function_2.exit2043
	testq	%r15, %r15
	jne	.LBB35_171
# BB#170:                               # %builtin_function_2.exit2043
	movq	%r14, %r15
.LBB35_171:                             # %builtin_function_2.exit2043
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+88(%rip)
	testq	%rax, %rax
	movq	336(%rsp), %rax         # 8-byte Reload
	je	.LBB35_172
# BB#173:                               # %select.mid26
	xorq	%r8, %r8
	jmp	.LBB35_174
.LBB35_172:
	movabsq	$.L.str90+10, %r8
.LBB35_174:                             # %select.end25
	movabsq	$.L.str90, %rdi
	movq	$12, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_181
# BB#175:                               # %land.lhs.true10.i2049
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_179
# BB#176:
	movabsq	$.L.str90+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_178:                             # %for.body.i.i2058
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_181
# BB#177:                               # %for.cond.i.i2053
                                        #   in Loop: Header=BB35_178 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_178
.LBB35_179:                             # %land.lhs.true12.i2060
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_181
# BB#180:                               # %if.then16.i2062
	movabsq	$.L.str90+10, %rdi
	movq	$12, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	336(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_181:                             # %builtin_function_2.exit2069
	testq	%r15, %r15
	jne	.LBB35_183
# BB#182:                               # %builtin_function_2.exit2069
	movq	%r14, %r15
.LBB35_183:                             # %builtin_function_2.exit2069
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+96(%rip)
	testq	%rax, %rax
	je	.LBB35_184
# BB#185:                               # %select.mid29
	xorq	%r8, %r8
	jmp	.LBB35_186
.LBB35_184:
	movabsq	$.L.str91+10, %r8
.LBB35_186:                             # %select.end28
	movq	328(%rsp), %rsi         # 8-byte Reload
	movabsq	$.L.str91, %rdi
	movq	$13, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_193
# BB#187:                               # %land.lhs.true10.i2075
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_191
# BB#188:
	movabsq	$.L.str91+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_190:                             # %for.body.i.i2084
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_193
# BB#189:                               # %for.cond.i.i2079
                                        #   in Loop: Header=BB35_190 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_190
.LBB35_191:                             # %land.lhs.true12.i2086
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_193
# BB#192:                               # %if.then16.i2088
	movabsq	$.L.str91+10, %rdi
	movq	$13, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	328(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_193:                             # %builtin_function_2.exit2095
	testq	%r15, %r15
	jne	.LBB35_195
# BB#194:                               # %builtin_function_2.exit2095
	movq	%r14, %r15
.LBB35_195:                             # %builtin_function_2.exit2095
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+104(%rip)
	testq	%rax, %rax
	je	.LBB35_196
# BB#197:                               # %select.mid32
	xorq	%r8, %r8
	jmp	.LBB35_198
.LBB35_196:
	movabsq	$.L.str92+10, %r8
.LBB35_198:                             # %select.end31
	movq	344(%rsp), %rsi         # 8-byte Reload
	movabsq	$.L.str92, %rdi
	movq	$14, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_205
# BB#199:                               # %land.lhs.true10.i2101
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_203
# BB#200:
	movabsq	$.L.str92+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_202:                             # %for.body.i.i2110
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_205
# BB#201:                               # %for.cond.i.i2105
                                        #   in Loop: Header=BB35_202 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_202
.LBB35_203:                             # %land.lhs.true12.i2112
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_205
# BB#204:                               # %if.then16.i2114
	movabsq	$.L.str92+10, %rdi
	movq	$14, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	344(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_205:                             # %builtin_function_2.exit2121
	testq	%r15, %r15
	jne	.LBB35_207
# BB#206:                               # %builtin_function_2.exit2121
	movq	%r14, %r15
.LBB35_207:                             # %builtin_function_2.exit2121
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+112(%rip)
	testq	%rax, %rax
	movq	336(%rsp), %rax         # 8-byte Reload
	je	.LBB35_208
# BB#209:                               # %select.mid35
	xorq	%r8, %r8
	jmp	.LBB35_210
.LBB35_208:
	movabsq	$.L.str93+10, %r8
.LBB35_210:                             # %select.end34
	movabsq	$.L.str93, %rdi
	movq	$15, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_217
# BB#211:                               # %land.lhs.true10.i2127
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_215
# BB#212:
	movabsq	$.L.str93+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_214:                             # %for.body.i.i2136
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_217
# BB#213:                               # %for.cond.i.i2131
                                        #   in Loop: Header=BB35_214 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_214
.LBB35_215:                             # %land.lhs.true12.i2138
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_217
# BB#216:                               # %if.then16.i2140
	movabsq	$.L.str93+10, %rdi
	movq	$15, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	336(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_217:                             # %builtin_function_2.exit2147
	testq	%r15, %r15
	jne	.LBB35_219
# BB#218:                               # %builtin_function_2.exit2147
	movq	%r14, %r15
.LBB35_219:                             # %builtin_function_2.exit2147
	movl	flag_isoc99(%rip), %eax
	movq	%r15, built_in_decls+120(%rip)
	testq	%rax, %rax
	je	.LBB35_220
# BB#221:                               # %select.mid38
	xorq	%r8, %r8
	jmp	.LBB35_222
.LBB35_220:
	movabsq	$.L.str94+10, %r8
.LBB35_222:                             # %select.end37
	movq	328(%rsp), %rsi         # 8-byte Reload
	movabsq	$.L.str94, %rdi
	movq	$16, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_229
# BB#223:                               # %land.lhs.true10.i2153
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_227
# BB#224:
	movabsq	$.L.str94+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_226:                             # %for.body.i.i2162
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_229
# BB#225:                               # %for.cond.i.i2157
                                        #   in Loop: Header=BB35_226 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_226
.LBB35_227:                             # %land.lhs.true12.i2164
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_229
# BB#228:                               # %if.then16.i2166
	movabsq	$.L.str94+10, %rdi
	movq	$16, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	328(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_229:                             # %builtin_function_2.exit2173
	testq	%r15, %r15
	jne	.LBB35_231
# BB#230:                               # %builtin_function_2.exit2173
	movq	%r14, %r15
.LBB35_231:                             # %builtin_function_2.exit2173
	movq	%r15, built_in_decls+128(%rip)
	movabsq	$.L.str95, %rdi
	movabsq	$.L.str95+10, %r8
	movq	$17, %rdx
	movq	$3, %rcx
	movq	128(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_238
# BB#232:                               # %land.lhs.true10.i2178
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_236
# BB#233:
	movabsq	$.L.str95+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_235:                             # %for.body.i.i2187
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_238
# BB#234:                               # %for.cond.i.i2182
                                        #   in Loop: Header=BB35_235 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_235
.LBB35_236:                             # %land.lhs.true12.i2189
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_238
# BB#237:                               # %if.then16.i2191
	movabsq	$.L.str95+10, %rdi
	movq	$17, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	224(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_238:                             # %builtin_function_2.exit2198
	testq	%r15, %r15
	jne	.LBB35_240
# BB#239:                               # %builtin_function_2.exit2198
	movq	%r14, %r15
.LBB35_240:                             # %builtin_function_2.exit2198
	movq	%r15, built_in_decls+136(%rip)
	movabsq	$.L.str96, %rdi
	movabsq	$.L.str96+10, %r8
	movq	$18, %rdx
	movq	$3, %rcx
	movq	120(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_247
# BB#241:                               # %land.lhs.true10.i2203
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_245
# BB#242:
	movabsq	$.L.str96+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_244:                             # %for.body.i.i2212
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_247
# BB#243:                               # %for.cond.i.i2207
                                        #   in Loop: Header=BB35_244 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_244
.LBB35_245:                             # %land.lhs.true12.i2214
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_247
# BB#246:                               # %if.then16.i2216
	movabsq	$.L.str96+10, %rdi
	movq	$18, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	368(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_247:                             # %builtin_function_2.exit2223
	testq	%r15, %r15
	jne	.LBB35_249
# BB#248:                               # %builtin_function_2.exit2223
	movq	%r14, %r15
.LBB35_249:                             # %builtin_function_2.exit2223
	movq	%r15, built_in_decls+144(%rip)
	movabsq	$.L.str97, %rdi
	movabsq	$.L.str97+10, %r8
	movq	$19, %rdx
	movq	$3, %rcx
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_256
# BB#250:                               # %land.lhs.true10.i2228
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_254
# BB#251:
	movabsq	$.L.str97+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_253:                             # %for.body.i.i2237
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_256
# BB#252:                               # %for.cond.i.i2232
                                        #   in Loop: Header=BB35_253 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_253
.LBB35_254:                             # %land.lhs.true12.i2239
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_256
# BB#255:                               # %if.then16.i2241
	movabsq	$.L.str97+10, %rdi
	movq	$19, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_256:                             # %builtin_function_2.exit2248
	testq	%r15, %r15
	jne	.LBB35_258
# BB#257:                               # %builtin_function_2.exit2248
	movq	%r14, %r15
.LBB35_258:                             # %builtin_function_2.exit2248
	movq	%r15, built_in_decls+152(%rip)
	movabsq	$.L.str98, %rdi
	movabsq	$.L.str98+10, %r8
	movq	$20, %rdx
	movq	$3, %rcx
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_265
# BB#259:                               # %land.lhs.true10.i2253
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_263
# BB#260:
	movabsq	$.L.str98+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_262:                             # %for.body.i.i2262
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_265
# BB#261:                               # %for.cond.i.i2257
                                        #   in Loop: Header=BB35_262 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_262
.LBB35_263:                             # %land.lhs.true12.i2264
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_265
# BB#264:                               # %if.then16.i2266
	movabsq	$.L.str98+10, %rdi
	movq	$20, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_265:                             # %builtin_function_2.exit2273
	testq	%r15, %r15
	movq	416(%rsp), %rax         # 8-byte Reload
	jne	.LBB35_267
# BB#266:                               # %builtin_function_2.exit2273
	movq	%r14, %r15
.LBB35_267:                             # %builtin_function_2.exit2273
	movq	%r15, built_in_decls+160(%rip)
	movabsq	$.L.str99, %rdi
	movabsq	$.L.str99+10, %r8
	movq	$21, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	movq	240(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_275
# BB#268:                               # %land.lhs.true10.i2278
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_273
# BB#269:
	movabsq	$.L.str99+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_271:                             # %for.body.i.i2287
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_272
# BB#270:                               # %for.cond.i.i2282
                                        #   in Loop: Header=BB35_271 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_271
.LBB35_273:                             # %land.lhs.true12.i2289
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	240(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_275
# BB#274:                               # %if.then16.i2291
	movabsq	$.L.str99+10, %rdi
	movq	$21, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
	jmp	.LBB35_275
.LBB35_272:
	movq	240(%rsp), %rbx         # 8-byte Reload
.LBB35_275:                             # %builtin_function_2.exit2298
	testq	%r15, %r15
	jne	.LBB35_277
# BB#276:                               # %builtin_function_2.exit2298
	movq	%r14, %r15
.LBB35_277:                             # %builtin_function_2.exit2298
	movq	%r15, built_in_decls+168(%rip)
	movabsq	$.L.str100, %rdi
	movabsq	$.L.str100+10, %r8
	movq	$22, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_283
# BB#278:                               # %land.lhs.true10.i2303
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_282
# BB#279:
	movabsq	$.L.str100+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_281:                             # %for.body.i.i2312
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_283
# BB#280:                               # %for.cond.i.i2307
                                        #   in Loop: Header=BB35_281 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_281
.LBB35_282:                             # %land.lhs.true12.i2314
	movabsq	$.L.str100+10, %rdi
	movq	$22, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	240(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_283:                             # %builtin_function_2.exit2320
	movq	184(%rsp), %rax         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_285
# BB#284:                               # %builtin_function_2.exit2320
	movq	%r14, %r15
.LBB35_285:                             # %builtin_function_2.exit2320
	movq	%r15, built_in_decls+176(%rip)
	movabsq	$.L.str101, %rdi
	movabsq	$.L.str101+10, %r8
	movq	$23, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_291
# BB#286:                               # %land.lhs.true10.i2325
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_290
# BB#287:
	movabsq	$.L.str101+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_289:                             # %for.body.i.i2334
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_291
# BB#288:                               # %for.cond.i.i2329
                                        #   in Loop: Header=BB35_289 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_289
.LBB35_290:                             # %land.lhs.true12.i2336
	movabsq	$.L.str101+10, %rdi
	movq	$23, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	184(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_291:                             # %builtin_function_2.exit2342
	movq	176(%rsp), %rax         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_293
# BB#292:                               # %builtin_function_2.exit2342
	movq	%r14, %r15
.LBB35_293:                             # %builtin_function_2.exit2342
	movq	%r15, built_in_decls+184(%rip)
	movabsq	$.L.str102, %rdi
	movabsq	$.L.str102+10, %r8
	movq	$24, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	movq	360(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_300
# BB#294:                               # %land.lhs.true10.i2347
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_298
# BB#295:
	movabsq	$.L.str102+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_297:                             # %for.body.i.i2356
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_299
# BB#296:                               # %for.cond.i.i2351
                                        #   in Loop: Header=BB35_297 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_297
.LBB35_298:                             # %land.lhs.true12.i2358
	movabsq	$.L.str102+10, %rdi
	movq	$24, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	176(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_299:                             # %builtin_function_2.exit2364
	movq	360(%rsp), %rbx         # 8-byte Reload
.LBB35_300:                             # %builtin_function_2.exit2364
	testq	%r15, %r15
	jne	.LBB35_302
# BB#301:                               # %builtin_function_2.exit2364
	movq	%r14, %r15
.LBB35_302:                             # %builtin_function_2.exit2364
	movq	%r15, built_in_decls+192(%rip)
	movabsq	$.L.str103, %rdi
	movabsq	$.L.str103+10, %r8
	movq	$25, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_309
# BB#303:                               # %land.lhs.true10.i2369
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_308
# BB#304:
	movabsq	$.L.str103+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_306:                             # %for.body.i.i2378
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_307
# BB#305:                               # %for.cond.i.i2373
                                        #   in Loop: Header=BB35_306 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_306
.LBB35_308:                             # %land.lhs.true12.i2380
	movabsq	$.L.str103+10, %rdi
	movq	$25, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	360(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, %r14
	jmp	.LBB35_309
.LBB35_307:
	movq	360(%rsp), %rbx         # 8-byte Reload
.LBB35_309:                             # %builtin_function_2.exit2386
	testq	%r15, %r15
	jne	.LBB35_311
# BB#310:                               # %builtin_function_2.exit2386
	movq	%r14, %r15
.LBB35_311:                             # %builtin_function_2.exit2386
	movq	%r15, built_in_decls+200(%rip)
	movabsq	$.L.str104, %rdi
	movabsq	$.L.str104+10, %r8
	movq	$26, %rdx
	movq	$3, %rcx
	movq	376(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_318
# BB#312:                               # %land.lhs.true10.i2391
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_316
# BB#313:
	movabsq	$.L.str104+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_315:                             # %for.body.i.i2400
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_317
# BB#314:                               # %for.cond.i.i2395
                                        #   in Loop: Header=BB35_315 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_315
.LBB35_316:                             # %land.lhs.true12.i2402
	movabsq	$.L.str104+10, %rdi
	movq	$26, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	376(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_317:                             # %builtin_function_2.exit2408
	movq	360(%rsp), %rbx         # 8-byte Reload
.LBB35_318:                             # %builtin_function_2.exit2408
	testq	%r15, %r15
	jne	.LBB35_320
# BB#319:                               # %builtin_function_2.exit2408
	movq	%r14, %r15
.LBB35_320:                             # %builtin_function_2.exit2408
	movq	%r15, built_in_decls+208(%rip)
	movabsq	$.L.str105, %rdi
	movabsq	$.L.str105+10, %r8
	movq	$27, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_326
# BB#321:                               # %land.lhs.true10.i2413
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_325
# BB#322:
	movabsq	$.L.str105+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_324:                             # %for.body.i.i2422
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_326
# BB#323:                               # %for.cond.i.i2417
                                        #   in Loop: Header=BB35_324 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_324
.LBB35_325:                             # %land.lhs.true12.i2424
	movabsq	$.L.str105+10, %rdi
	movq	$27, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	360(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_326:                             # %builtin_function_2.exit2430
	movq	376(%rsp), %rsi         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_328
# BB#327:                               # %builtin_function_2.exit2430
	movq	%r14, %r15
.LBB35_328:                             # %builtin_function_2.exit2430
	movq	%r15, built_in_decls+216(%rip)
	movabsq	$.L.str106, %rdi
	movabsq	$.L.str106+10, %r8
	movq	$28, %rdx
	movq	$3, %rcx
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_334
# BB#329:                               # %land.lhs.true10.i2435
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_333
# BB#330:
	movabsq	$.L.str106+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_332:                             # %for.body.i.i2444
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_334
# BB#331:                               # %for.cond.i.i2439
                                        #   in Loop: Header=BB35_332 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_332
.LBB35_333:                             # %land.lhs.true12.i2446
	movabsq	$.L.str106+10, %rdi
	movq	$28, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	376(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_334:                             # %builtin_function_2.exit2452
	movq	168(%rsp), %rax         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_336
# BB#335:                               # %builtin_function_2.exit2452
	movq	%r14, %r15
.LBB35_336:                             # %builtin_function_2.exit2452
	movq	%r15, built_in_decls+224(%rip)
	movabsq	$.L.str107, %rdi
	movabsq	$.L.str107+10, %r8
	movq	$29, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_342
# BB#337:                               # %land.lhs.true10.i2457
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_341
# BB#338:
	movabsq	$.L.str107+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_340:                             # %for.body.i.i2466
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_342
# BB#339:                               # %for.cond.i.i2461
                                        #   in Loop: Header=BB35_340 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_340
.LBB35_341:                             # %land.lhs.true12.i2468
	movabsq	$.L.str107+10, %rdi
	movq	$29, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	168(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_342:                             # %builtin_function_2.exit2474
	movq	160(%rsp), %rax         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_344
# BB#343:                               # %builtin_function_2.exit2474
	movq	%r14, %r15
.LBB35_344:                             # %builtin_function_2.exit2474
	movq	%r15, built_in_decls+232(%rip)
	movabsq	$.L.str108, %rdi
	movabsq	$.L.str108+10, %r8
	movq	$30, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_350
# BB#345:                               # %land.lhs.true10.i2479
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_349
# BB#346:
	movabsq	$.L.str108+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_348:                             # %for.body.i.i2488
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_350
# BB#347:                               # %for.cond.i.i2483
                                        #   in Loop: Header=BB35_348 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_348
.LBB35_349:                             # %land.lhs.true12.i2490
	movabsq	$.L.str108+10, %rdi
	movq	$30, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	160(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_350:                             # %builtin_function_2.exit2496
	movq	152(%rsp), %rax         # 8-byte Reload
	testq	%r15, %r15
	jne	.LBB35_352
# BB#351:                               # %builtin_function_2.exit2496
	movq	%r14, %r15
.LBB35_352:                             # %builtin_function_2.exit2496
	movq	%r15, built_in_decls+240(%rip)
	movabsq	$.L.str109, %rdi
	movabsq	$.L.str109+10, %r8
	movq	$31, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	movq	296(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_359
# BB#353:                               # %land.lhs.true10.i2501
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_357
# BB#354:
	movabsq	$.L.str109+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_356:                             # %for.body.i.i2510
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_358
# BB#355:                               # %for.cond.i.i2505
                                        #   in Loop: Header=BB35_356 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_356
.LBB35_357:                             # %land.lhs.true12.i2512
	movabsq	$.L.str109+10, %rdi
	movq	$31, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	152(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_358:                             # %builtin_function_2.exit2518
	movq	296(%rsp), %rbx         # 8-byte Reload
.LBB35_359:                             # %builtin_function_2.exit2518
	testq	%r15, %r15
	jne	.LBB35_361
# BB#360:                               # %builtin_function_2.exit2518
	movq	%r14, %r15
.LBB35_361:                             # %builtin_function_2.exit2518
	movq	%r15, built_in_decls+248(%rip)
	movabsq	$.L.str110, %rdi
	movabsq	$.L.str110+10, %r8
	movq	$32, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_368
# BB#362:                               # %land.lhs.true10.i2523
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_367
# BB#363:
	movabsq	$.L.str110+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_365:                             # %for.body.i.i2532
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_366
# BB#364:                               # %for.cond.i.i2527
                                        #   in Loop: Header=BB35_365 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_365
.LBB35_367:                             # %land.lhs.true12.i2534
	movabsq	$.L.str110+10, %rdi
	movq	$32, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	296(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, %r14
	jmp	.LBB35_368
.LBB35_366:
	movq	296(%rsp), %rbx         # 8-byte Reload
.LBB35_368:                             # %builtin_function_2.exit2540
	testq	%r15, %r15
	jne	.LBB35_370
# BB#369:                               # %builtin_function_2.exit2540
	movq	%r14, %r15
.LBB35_370:                             # %builtin_function_2.exit2540
	movq	%r15, built_in_decls+256(%rip)
	movabsq	$.L.str111, %rdi
	movabsq	$.L.str111+10, %r8
	movq	$33, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	movq	288(%rsp), %rbx         # 8-byte Reload
	jne	.LBB35_377
# BB#371:                               # %land.lhs.true10.i2545
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_375
# BB#372:
	movabsq	$.L.str111+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_374:                             # %for.body.i.i2554
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_376
# BB#373:                               # %for.cond.i.i2549
                                        #   in Loop: Header=BB35_374 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_374
.LBB35_375:                             # %land.lhs.true12.i2556
	movabsq	$.L.str111+10, %rdi
	movq	$33, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	296(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_376:                             # %builtin_function_2.exit2562
	movq	288(%rsp), %rbx         # 8-byte Reload
.LBB35_377:                             # %builtin_function_2.exit2562
	testq	%r15, %r15
	jne	.LBB35_379
# BB#378:                               # %builtin_function_2.exit2562
	movq	%r14, %r15
.LBB35_379:                             # %builtin_function_2.exit2562
	movq	%r15, built_in_decls+264(%rip)
	movabsq	$.L.str112, %rdi
	movabsq	$.L.str112+10, %r8
	movq	$34, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_386
# BB#380:                               # %land.lhs.true10.i2567
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_385
# BB#381:
	movabsq	$.L.str112+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_383:                             # %for.body.i.i2576
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_384
# BB#382:                               # %for.cond.i.i2571
                                        #   in Loop: Header=BB35_383 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_383
.LBB35_385:                             # %land.lhs.true12.i2578
	movabsq	$.L.str112+10, %rdi
	movq	$34, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	288(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, %r14
	jmp	.LBB35_386
.LBB35_384:
	movq	288(%rsp), %rbx         # 8-byte Reload
.LBB35_386:                             # %builtin_function_2.exit2584
	testq	%r15, %r15
	jne	.LBB35_388
# BB#387:                               # %builtin_function_2.exit2584
	movq	%r14, %r15
.LBB35_388:                             # %builtin_function_2.exit2584
	movq	%r15, built_in_decls+272(%rip)
	movabsq	$.L.str113, %rdi
	movabsq	$.L.str113+10, %r8
	movq	$35, %rdx
	movq	$3, %rcx
	movq	%rbx, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_394
# BB#389:                               # %land.lhs.true10.i2589
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_393
# BB#390:
	movabsq	$.L.str113+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_392:                             # %for.body.i.i2598
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_394
# BB#391:                               # %for.cond.i.i2593
                                        #   in Loop: Header=BB35_392 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_392
.LBB35_393:                             # %land.lhs.true12.i2600
	movabsq	$.L.str113+10, %rdi
	movq	$35, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	288(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_394:                             # %builtin_function_2.exit2606
	testq	%r15, %r15
	movq	416(%rsp), %rax         # 8-byte Reload
	jne	.LBB35_396
# BB#395:                               # %builtin_function_2.exit2606
	movq	%r14, %r15
.LBB35_396:                             # %builtin_function_2.exit2606
	movq	%r15, built_in_decls+280(%rip)
	movabsq	$.L.str114, %rdi
	movabsq	$.L.str114+10, %r8
	movq	$36, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_402
# BB#397:                               # %land.lhs.true10.i2611
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_401
# BB#398:
	movabsq	$.L.str114+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_400:                             # %for.body.i.i2620
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_402
# BB#399:                               # %for.cond.i.i2615
                                        #   in Loop: Header=BB35_400 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_400
.LBB35_401:                             # %land.lhs.true12.i2622
	movabsq	$.L.str114+10, %rdi
	movq	$36, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_402:                             # %builtin_function_2.exit2628
	testq	%r15, %r15
	jne	.LBB35_404
# BB#403:                               # %builtin_function_2.exit2628
	movq	%r14, %r15
.LBB35_404:                             # %builtin_function_2.exit2628
	movq	%r15, built_in_decls+288(%rip)
	movabsq	$.L.str115, %rdi
	movabsq	$.L.str115+10, %r8
	movq	$37, %rdx
	movq	$3, %rcx
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_410
# BB#405:                               # %land.lhs.true10.i2633
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_409
# BB#406:
	movabsq	$.L.str115+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_408:                             # %for.body.i.i2642
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_410
# BB#407:                               # %for.cond.i.i2637
                                        #   in Loop: Header=BB35_408 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_408
.LBB35_409:                             # %land.lhs.true12.i2644
	movabsq	$.L.str115+10, %rdi
	movq	$37, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	416(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_410:                             # %builtin_function_2.exit2650
	testq	%r15, %r15
	movq	440(%rsp), %rax         # 8-byte Reload
	jne	.LBB35_412
# BB#411:                               # %builtin_function_2.exit2650
	movq	%r14, %r15
.LBB35_412:                             # %builtin_function_2.exit2650
	movq	%r15, built_in_decls+296(%rip)
	movabsq	$.L.str116, %rdi
	movabsq	$.L.str116+10, %r8
	movq	$38, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_418
# BB#413:                               # %land.lhs.true10.i2655
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_417
# BB#414:
	movabsq	$.L.str116+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_416:                             # %for.body.i.i2664
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_418
# BB#415:                               # %for.cond.i.i2659
                                        #   in Loop: Header=BB35_416 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_416
.LBB35_417:                             # %land.lhs.true12.i2666
	movabsq	$.L.str116+10, %rdi
	movq	$38, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_418:                             # %builtin_function_2.exit2672
	testq	%r15, %r15
	jne	.LBB35_420
# BB#419:                               # %builtin_function_2.exit2672
	movq	%r14, %r15
.LBB35_420:                             # %builtin_function_2.exit2672
	movq	%r15, built_in_decls+304(%rip)
	movabsq	$.L.str117, %rdi
	movabsq	$.L.str117+10, %r8
	movq	$39, %rdx
	movq	$3, %rcx
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_426
# BB#421:                               # %land.lhs.true10.i2677
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_425
# BB#422:
	movabsq	$.L.str117+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_424:                             # %for.body.i.i2686
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_426
# BB#423:                               # %for.cond.i.i2681
                                        #   in Loop: Header=BB35_424 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_424
.LBB35_425:                             # %land.lhs.true12.i2688
	movabsq	$.L.str117+10, %rdi
	movq	$39, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_426:                             # %builtin_function_2.exit2694
	testq	%r15, %r15
	jne	.LBB35_428
# BB#427:                               # %builtin_function_2.exit2694
	movq	%r14, %r15
.LBB35_428:                             # %builtin_function_2.exit2694
	movq	%r15, built_in_decls+312(%rip)
	movabsq	$.L.str118, %rdi
	movabsq	$.L.str118+10, %r8
	movq	$40, %rdx
	movq	$3, %rcx
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_434
# BB#429:                               # %land.lhs.true10.i2699
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_433
# BB#430:
	movabsq	$.L.str118+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_432:                             # %for.body.i.i2708
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_434
# BB#431:                               # %for.cond.i.i2703
                                        #   in Loop: Header=BB35_432 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_432
.LBB35_433:                             # %land.lhs.true12.i2710
	movabsq	$.L.str118+10, %rdi
	movq	$40, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	440(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_434:                             # %builtin_function_2.exit2716
	testq	%r15, %r15
	movq	432(%rsp), %rax         # 8-byte Reload
	jne	.LBB35_436
# BB#435:                               # %builtin_function_2.exit2716
	movq	%r14, %r15
.LBB35_436:                             # %builtin_function_2.exit2716
	movq	%r15, built_in_decls+320(%rip)
	movabsq	$.L.str119, %rdi
	movabsq	$.L.str119+10, %r8
	movq	$41, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_442
# BB#437:                               # %land.lhs.true10.i2721
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_441
# BB#438:
	movabsq	$.L.str119+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_440:                             # %for.body.i.i2730
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_442
# BB#439:                               # %for.cond.i.i2725
                                        #   in Loop: Header=BB35_440 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_440
.LBB35_441:                             # %land.lhs.true12.i2732
	movabsq	$.L.str119+10, %rdi
	movq	$41, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_442:                             # %builtin_function_2.exit2738
	testq	%r15, %r15
	jne	.LBB35_444
# BB#443:                               # %builtin_function_2.exit2738
	movq	%r14, %r15
.LBB35_444:                             # %builtin_function_2.exit2738
	movq	%r15, built_in_decls+328(%rip)
	movabsq	$.L.str120, %rdi
	movabsq	$.L.str120+10, %r8
	movq	$42, %rdx
	movq	$3, %rcx
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_450
# BB#445:                               # %land.lhs.true10.i2743
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_449
# BB#446:
	movabsq	$.L.str120+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_448:                             # %for.body.i.i2752
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_450
# BB#447:                               # %for.cond.i.i2747
                                        #   in Loop: Header=BB35_448 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_448
.LBB35_449:                             # %land.lhs.true12.i2754
	movabsq	$.L.str120+10, %rdi
	movq	$42, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_450:                             # %builtin_function_2.exit2760
	testq	%r15, %r15
	jne	.LBB35_452
# BB#451:                               # %builtin_function_2.exit2760
	movq	%r14, %r15
.LBB35_452:                             # %builtin_function_2.exit2760
	movq	%r15, built_in_decls+336(%rip)
	movabsq	$.L.str121, %rdi
	movabsq	$.L.str121+10, %r8
	movq	$43, %rdx
	movq	$3, %rcx
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_458
# BB#453:                               # %land.lhs.true10.i2765
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_457
# BB#454:
	movabsq	$.L.str121+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_456:                             # %for.body.i.i2774
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_458
# BB#455:                               # %for.cond.i.i2769
                                        #   in Loop: Header=BB35_456 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_456
.LBB35_457:                             # %land.lhs.true12.i2776
	movabsq	$.L.str121+10, %rdi
	movq	$43, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	432(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_458:                             # %builtin_function_2.exit2782
	testq	%r15, %r15
	movq	424(%rsp), %rax         # 8-byte Reload
	jne	.LBB35_460
# BB#459:                               # %builtin_function_2.exit2782
	movq	%r14, %r15
.LBB35_460:                             # %builtin_function_2.exit2782
	movq	%r15, built_in_decls+344(%rip)
	movabsq	$.L.str122, %rdi
	movabsq	$.L.str122+10, %r8
	movq	$44, %rdx
	movq	$3, %rcx
	movq	%rax, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_466
# BB#461:                               # %land.lhs.true10.i2787
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_465
# BB#462:
	movabsq	$.L.str122+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_464:                             # %for.body.i.i2796
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_466
# BB#463:                               # %for.cond.i.i2791
                                        #   in Loop: Header=BB35_464 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_464
.LBB35_465:                             # %land.lhs.true12.i2798
	movabsq	$.L.str122+10, %rdi
	movq	$44, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_466:                             # %builtin_function_2.exit2804
	testq	%r15, %r15
	jne	.LBB35_468
# BB#467:                               # %builtin_function_2.exit2804
	movq	%r14, %r15
.LBB35_468:                             # %builtin_function_2.exit2804
	movq	%r15, built_in_decls+352(%rip)
	movabsq	$.L.str123, %rdi
	movabsq	$.L.str123+10, %r8
	movq	$45, %rdx
	movq	$3, %rcx
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_474
# BB#469:                               # %land.lhs.true10.i2809
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_473
# BB#470:
	movabsq	$.L.str123+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_472:                             # %for.body.i.i2818
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_474
# BB#471:                               # %for.cond.i.i2813
                                        #   in Loop: Header=BB35_472 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_472
.LBB35_473:                             # %land.lhs.true12.i2820
	movabsq	$.L.str123+10, %rdi
	movq	$45, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_474:                             # %builtin_function_2.exit2826
	testq	%r15, %r15
	jne	.LBB35_476
# BB#475:                               # %builtin_function_2.exit2826
	movq	%r14, %r15
.LBB35_476:                             # %builtin_function_2.exit2826
	movq	%r15, built_in_decls+360(%rip)
	movabsq	$.L.str124, %rdi
	movabsq	$.L.str124+10, %r8
	movq	$46, %rdx
	movq	$3, %rcx
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_482
# BB#477:                               # %land.lhs.true10.i2831
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_481
# BB#478:
	movabsq	$.L.str124+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_480:                             # %for.body.i.i2840
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_482
# BB#479:                               # %for.cond.i.i2835
                                        #   in Loop: Header=BB35_480 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_480
.LBB35_481:                             # %land.lhs.true12.i2842
	movabsq	$.L.str124+10, %rdi
	movq	$46, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	424(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_482:                             # %builtin_function_2.exit2848
	testq	%r15, %r15
	jne	.LBB35_484
# BB#483:                               # %builtin_function_2.exit2848
	movq	%r14, %r15
.LBB35_484:                             # %builtin_function_2.exit2848
	movq	%r15, built_in_decls+368(%rip)
	movabsq	$.L.str125, %rdi
	movq	$47, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+376(%rip)
	movabsq	$.L.str126, %rdi
	movq	$48, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	368(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+384(%rip)
	movabsq	$.L.str127, %rdi
	movq	$49, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%r12, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+392(%rip)
	movabsq	$.L.str128, %rdi
	movq	$50, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	456(%rsp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+400(%rip)
	movabsq	$.L.str129, %rdi
	movq	$51, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+408(%rip)
	movabsq	$.L.str130, %rdi
	movq	$52, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	104(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+416(%rip)
	movabsq	$.L.str131, %rdi
	movq	$53, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+424(%rip)
	movabsq	$.L.str132, %rdi
	movq	$54, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%r12, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+432(%rip)
	movabsq	$.L.str133, %rdi
	movq	$55, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%r12, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+440(%rip)
	movabsq	$.L.str134, %rdi
	movq	$56, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	448(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+448(%rip)
	movabsq	$.L.str135, %rdi
	movq	$57, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	256(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+456(%rip)
	movabsq	$.L.str136, %rdi
	movq	$58, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	112(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+464(%rip)
	movabsq	$.L.str137, %rdi
	movq	$59, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	96(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+472(%rip)
	movabsq	$.L.str138, %rdi
	movq	$60, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	352(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+480(%rip)
	movabsq	$.L.str139, %rdi
	movq	$61, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	392(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+488(%rip)
	movabsq	$.L.str140, %rdi
	movabsq	$.L.str140+10, %r8
	movq	$62, %rdx
	movq	$3, %rcx
	movq	%r15, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+496(%rip)
	movabsq	$.L.str141, %rdi
	movabsq	$.L.str141+10, %r8
	movq	$63, %rdx
	movq	$3, %rcx
	movq	280(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+504(%rip)
	movabsq	$.L.str142, %rdi
	movabsq	$.L.str142+10, %r8
	movq	$64, %rdx
	movq	$1, %rcx
	movq	400(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	testq	%rcx, %rcx
	jne	.LBB35_490
# BB#485:                               # %land.lhs.true10.i2853
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_489
# BB#486:
	movabsq	$.L.str142+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_488:                             # %for.body.i.i2862
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_490
# BB#487:                               # %for.cond.i.i2857
                                        #   in Loop: Header=BB35_488 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_488
.LBB35_489:                             # %land.lhs.true12.i2864
	movabsq	$.L.str142+10, %rdi
	movq	$64, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	400(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_490:                             # %builtin_function_2.exit2870
	testq	%r15, %r15
	movq	408(%rsp), %r12         # 8-byte Reload
	jne	.LBB35_492
# BB#491:                               # %builtin_function_2.exit2870
	movq	%r14, %r15
.LBB35_492:                             # %builtin_function_2.exit2870
	movq	%r15, built_in_decls+512(%rip)
	movabsq	$.L.str143, %rdi
	movabsq	$.L.str143+10, %r8
	movq	$65, %rdx
	movq	$3, %rcx
	movq	264(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+520(%rip)
	movabsq	$.L.str144, %rdi
	movabsq	$.L.str144+10, %r8
	movq	$66, %rdx
	movq	$3, %rcx
	movq	272(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_499
# BB#493:                               # %land.lhs.true10.i2875
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_497
# BB#494:
	movabsq	$.L.str144+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_496:                             # %for.body.i.i2884
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_498
# BB#495:                               # %for.cond.i.i2879
                                        #   in Loop: Header=BB35_496 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_496
.LBB35_497:                             # %land.lhs.true12.i2886
	movabsq	$.L.str144+10, %rdi
	movq	$66, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	368(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
.LBB35_498:                             # %builtin_function_2.exit2892
	movq	408(%rsp), %r12         # 8-byte Reload
.LBB35_499:                             # %builtin_function_2.exit2892
	testq	%r15, %r15
	jne	.LBB35_501
# BB#500:                               # %builtin_function_2.exit2892
	movq	%r14, %r15
.LBB35_501:                             # %builtin_function_2.exit2892
	movq	%r15, built_in_decls+528(%rip)
	movabsq	$.L.str145, %rdi
	movabsq	$.L.str145+10, %r8
	movq	$67, %rdx
	movq	$3, %rcx
	movq	248(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+536(%rip)
	movabsq	$.L.str146, %rdi
	movabsq	$.L.str146+10, %r8
	movq	$68, %rdx
	movq	$1, %rcx
	movq	%r12, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_508
# BB#502:                               # %land.lhs.true10.i2897
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_507
# BB#503:
	movabsq	$.L.str146+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_505:                             # %for.body.i.i2906
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_506
# BB#504:                               # %for.cond.i.i2901
                                        #   in Loop: Header=BB35_505 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_505
.LBB35_507:                             # %land.lhs.true12.i2908
	movabsq	$.L.str146+10, %rdi
	movq	$68, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	408(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rsi
	callq	builtin_function
	movq	%rax, %r14
	jmp	.LBB35_508
.LBB35_506:
	movq	408(%rsp), %r12         # 8-byte Reload
.LBB35_508:                             # %builtin_function_2.exit2914
	testq	%r15, %r15
	jne	.LBB35_510
# BB#509:                               # %builtin_function_2.exit2914
	movq	%r14, %r15
.LBB35_510:                             # %builtin_function_2.exit2914
	movq	%r15, built_in_decls+544(%rip)
	movabsq	$.L.str147, %rdi
	movabsq	$.L.str147+10, %r8
	movq	$69, %rdx
	movq	$3, %rcx
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+552(%rip)
	movabsq	$.L.str148, %rdi
	movabsq	$.L.str148+10, %r8
	movq	$70, %rdx
	movq	$3, %rcx
	movq	280(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+560(%rip)
	movabsq	$.L.str149, %rdi
	movabsq	$.L.str149+10, %r8
	movq	$71, %rdx
	movq	$1, %rcx
	movq	400(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_518
# BB#511:                               # %land.lhs.true10.i2919
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_516
# BB#512:
	movabsq	$.L.str149+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_514:                             # %for.body.i.i2928
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_515
# BB#513:                               # %for.cond.i.i2923
                                        #   in Loop: Header=BB35_514 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_514
.LBB35_516:                             # %land.lhs.true12.i2930
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	408(%rsp), %r12         # 8-byte Reload
	movq	400(%rsp), %rsi         # 8-byte Reload
	jne	.LBB35_518
# BB#517:                               # %if.then16.i2932
	movabsq	$.L.str149+10, %rdi
	movq	$71, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
	jmp	.LBB35_518
.LBB35_515:
	movq	408(%rsp), %r12         # 8-byte Reload
.LBB35_518:                             # %builtin_function_2.exit2939
	testq	%r15, %r15
	jne	.LBB35_520
# BB#519:                               # %builtin_function_2.exit2939
	movq	%r14, %r15
.LBB35_520:                             # %builtin_function_2.exit2939
	movq	%r15, built_in_decls+568(%rip)
	movabsq	$.L.str150, %rdi
	movabsq	$.L.str150+10, %r8
	movq	$72, %rdx
	movq	$3, %rcx
	movq	264(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+576(%rip)
	movabsq	$.L.str151, %rdi
	movabsq	$.L.str151+10, %r8
	movq	$73, %rdx
	movq	$3, %rcx
	movq	272(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_528
# BB#521:                               # %land.lhs.true10.i2944
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_526
# BB#522:
	movabsq	$.L.str151+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_524:                             # %for.body.i.i2953
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_525
# BB#523:                               # %for.cond.i.i2948
                                        #   in Loop: Header=BB35_524 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_524
.LBB35_526:                             # %land.lhs.true12.i2955
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	movq	408(%rsp), %r12         # 8-byte Reload
	jne	.LBB35_528
# BB#527:                               # %if.then16.i2957
	movabsq	$.L.str151+10, %rdi
	movq	$73, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	368(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
	jmp	.LBB35_528
.LBB35_525:
	movq	408(%rsp), %r12         # 8-byte Reload
.LBB35_528:                             # %builtin_function_2.exit2964
	testq	%r15, %r15
	jne	.LBB35_530
# BB#529:                               # %builtin_function_2.exit2964
	movq	%r14, %r15
.LBB35_530:                             # %builtin_function_2.exit2964
	movq	%r15, built_in_decls+584(%rip)
	movabsq	$.L.str152, %rdi
	movabsq	$.L.str152+10, %r8
	movq	$74, %rdx
	movq	$3, %rcx
	movq	248(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+592(%rip)
	movabsq	$.L.str153, %rdi
	movabsq	$.L.str153+10, %r8
	movq	$75, %rdx
	movq	$1, %rcx
	movq	%r12, %rsi
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, %r15
	xorq	%r14, %r14
	testq	%rcx, %rcx
	jne	.LBB35_537
# BB#531:                               # %land.lhs.true10.i2969
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_535
# BB#532:
	movabsq	$.L.str153+10, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_534:                             # %for.body.i.i2978
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB35_537
# BB#533:                               # %for.cond.i.i2973
                                        #   in Loop: Header=BB35_534 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_534
.LBB35_535:                             # %land.lhs.true12.i2980
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_537
# BB#536:                               # %if.then16.i2982
	movabsq	$.L.str153+10, %rdi
	movq	$75, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	408(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, %r14
	movl	16(%r14), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r14)
.LBB35_537:                             # %builtin_function_2.exit2989
	testq	%r15, %r15
	jne	.LBB35_539
# BB#538:                               # %builtin_function_2.exit2989
	movq	%r14, %r15
.LBB35_539:                             # %builtin_function_2.exit2989
	movq	%r15, built_in_decls+600(%rip)
	movabsq	$.L.str154, %rdi
	movq	$76, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	368(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+608(%rip)
	movabsq	$.L.str155, %rdi
	movq	$77, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+616(%rip)
	movabsq	$.L.str156, %rdi
	movq	$78, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+624(%rip)
	movabsq	$.L.str157, %rdi
	movq	$79, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+632(%rip)
	movabsq	$.L.str158, %rdi
	movq	$80, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+640(%rip)
	movabsq	$.L.str159, %rdi
	movq	$81, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+648(%rip)
	movabsq	$.L.str160, %rdi
	movq	$82, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	352(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+656(%rip)
	movabsq	$.L.str161, %rdi
	movq	$83, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	72(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+664(%rip)
	movabsq	$.L.str162, %rdi
	movq	$84, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+672(%rip)
	movabsq	$.L.str163, %rdi
	movq	$85, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	256(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+680(%rip)
	movabsq	$.L.str164, %rdi
	movq	$86, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	56(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+688(%rip)
	movabsq	$.L.str165, %rdi
	movq	$87, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+696(%rip)
	movabsq	$.L.str166, %rdi
	movq	$88, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+704(%rip)
	movabsq	$.L.str167, %rdi
	movq	$89, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	456(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+712(%rip)
	movabsq	$.L.str168, %rdi
	movq	$90, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+720(%rip)
	movabsq	$.L.str169, %rdi
	movq	$91, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+728(%rip)
	movabsq	$.L.str170, %rdi
	movq	$92, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rsi
	callq	builtin_function
	movq	%rax, built_in_decls+736(%rip)
	movabsq	$.L.str171, %rdi
	movq	$93, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	48(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movq	%rax, built_in_decls+744(%rip)
	movabsq	$.L.str172, %rdi
	movq	$94, %rdx
	movq	$3, %rcx
	xorq	%r8, %r8
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	builtin_function
	movl	flag_no_builtin(%rip), %ecx
	movq	%rax, built_in_decls+752(%rip)
	testq	%rcx, %rcx
	jne	.LBB35_573
# BB#540:                               # %land.lhs.true10.i2993
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_544
# BB#541:
	movabsq	$.L.str173, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_543:                             # %for.body.i.i3002
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB35_546
# BB#542:                               # %for.cond.i.i2997
                                        #   in Loop: Header=BB35_543 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_543
.LBB35_544:                             # %land.lhs.true12.i3004
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_546
# BB#545:                               # %builtin_function_2.exit3013
	movabsq	$.L.str173, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	builtin_function
	movl	16(%rax), %ecx
	orq	$10496, %rcx            # imm = 0x2900
	movl	%ecx, 16(%rax)
	movl	flag_no_builtin(%rip), %eax
	testq	%rax, %rax
	jne	.LBB35_573
.LBB35_546:                             # %land.lhs.true10.i3017
	movq	disabled_builtins(%rip), %rbx
	movl	flag_isoc99(%rip), %r14d
	testq	%rbx, %rbx
	je	.LBB35_552
# BB#547:
	movabsq	$.L.str174, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_549:                             # %for.body.i.i3026
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB35_550
# BB#548:                               # %for.cond.i.i3021
                                        #   in Loop: Header=BB35_549 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_549
.LBB35_552:                             # %land.lhs.true12.i3028
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r14
	jne	.LBB35_554
# BB#553:                               # %land.lhs.true12.i3028
	movl	flag_no_nonansi_builtin(%rip), %eax
	testq	%rbx, %rax
	je	.LBB35_554
.LBB35_550:                             # %builtin_function_2.exit3040.thread
	movl	c_language(%rip), %r14d
	cmpq	$1, %r14
	je	.LBB35_559
# BB#551:                               # %builtin_function_2.exit3040.thread
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 352(%rsp)         # 8-byte Spill
	jmp	.LBB35_559
.LBB35_554:                             # %if.then16.i3030
	movabsq	$.L.str174, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	builtin_function
	movl	16(%rax), %ecx
	testq	%rbx, %r14
	jne	.LBB35_556
# BB#555:                               # %if.then19.i
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%rax)
.LBB35_556:                             # %builtin_function_2.exit3040
	orq	$2304, %rcx             # imm = 0x900
	movl	%ecx, 16(%rax)
	movl	c_language(%rip), %r14d
	movl	flag_no_builtin(%rip), %eax
	cmpq	$1, %r14
	je	.LBB35_558
# BB#557:                               # %builtin_function_2.exit3040
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 352(%rsp)         # 8-byte Spill
.LBB35_558:                             # %builtin_function_2.exit3040
	testq	%rax, %rax
	jne	.LBB35_573
.LBB35_559:                             # %land.lhs.true10.i3044
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_565
# BB#560:
	movabsq	$.L.str175, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_562:                             # %for.body.i.i3053
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r15, %rdi
	callq	strcmp
	testq	%r12, %rax
	je	.LBB35_563
# BB#561:                               # %for.cond.i.i3048
                                        #   in Loop: Header=BB35_562 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_562
.LBB35_565:                             # %builtin_function_2.exit3065
	movabsq	$.L.str175, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	352(%rsp), %rsi         # 8-byte Reload
	callq	builtin_function
	movl	16(%rax), %ecx
	orq	$2304, %rcx             # imm = 0x900
	movl	%ecx, 16(%rax)
	movl	c_language(%rip), %ecx
	movl	flag_no_builtin(%rip), %eax
	cmpq	$1, %rcx
	je	.LBB35_567
# BB#566:                               # %select.mid82
	movq	224(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 8(%rsp)           # 8-byte Spill
.LBB35_567:                             # %select.end81
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB35_573
	jmp	.LBB35_568
.LBB35_563:                             # %builtin_function_2.exit3065.thread
	cmpq	$1, %r14
	je	.LBB35_568
# BB#564:                               # %builtin_function_2.exit3065.thread
	movq	224(%rsp), %rax         # 8-byte Reload
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB35_568:                             # %land.lhs.true10.i3069
	movq	disabled_builtins(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB35_572
# BB#569:
	movabsq	$.L.str176, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB35_571:                             # %for.body.i.i3078
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB35_573
# BB#570:                               # %for.cond.i.i3073
                                        #   in Loop: Header=BB35_571 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB35_571
.LBB35_572:                             # %land.lhs.true12.i3080
	movabsq	$.L.str176, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	builtin_function
	movl	16(%rax), %ecx
	orq	$2304, %rcx             # imm = 0x900
	movl	%ecx, 16(%rax)
.LBB35_573:                             # %builtin_function_2.exit3090
	movabsq	$.L.str177, %rdi
	callq	get_identifier
	movq	%rax, global_trees+400(%rip)
	movq	$simple_type_promotes_to, lang_type_promotes_to(%rip)
	addq	$464, %rsp              # imm = 0x1D0
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp273:
	.size	c_common_nodes_and_builtins, .Ltmp273-c_common_nodes_and_builtins
	.cfi_endproc

	.globl	simple_type_promotes_to
	.align	16, 0x90
	.type	simple_type_promotes_to,@function
simple_type_promotes_to:                # @simple_type_promotes_to
	.cfi_startproc
# BB#0:                                 # %entry
	movq	128(%rdi), %rdx
	movq	global_trees+192(%rip), %rax
	cmpq	%rax, %rdx
	je	.LBB36_1
# BB#2:                                 # %if.end
	movl	16(%rdi), %ecx
	movq	%rcx, %rsi
	andq	$255, %rsi
	cmpq	$11, %rsi
	je	.LBB36_11
# BB#3:                                 # %if.end
	xorq	%rax, %rax
	cmpq	$10, %rsi
	je	.LBB36_10
# BB#4:                                 # %if.end
	cmpq	$6, %rsi
	jne	.LBB36_16
# BB#5:                                 # %sw.bb.i
	movq	integer_types(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB36_11
# BB#6:                                 # %sw.bb.i
	movq	integer_types+8(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB36_11
# BB#7:                                 # %sw.bb.i
	movq	integer_types+16(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB36_11
# BB#8:                                 # %sw.bb.i
	movq	integer_types+24(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB36_11
# BB#9:                                 # %sw.bb.i
	movq	integer_types+32(%rip), %rsi
	cmpq	%rsi, %rdx
	je	.LBB36_11
.LBB36_10:                              # %c_promoting_integer_type_p.exit
	movq	integer_types+40(%rip), %rdx
	movl	60(%rdi), %esi
	movl	60(%rdx), %edx
	andq	$511, %rsi              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rsi
	jae	.LBB36_16
.LBB36_11:                              # %if.then2
	testq	$8192, %rcx             # imm = 0x2000
	je	.LBB36_12
# BB#13:                                # %land.lhs.true
	movl	flag_traditional(%rip), %eax
	testq	%rax, %rax
	jne	.LBB36_15
# BB#14:                                # %lor.lhs.false
	movq	integer_types+40(%rip), %rax
	movl	60(%rdi), %ecx
	movl	60(%rax), %edx
	xorq	%rcx, %rdx
	testq	$511, %rdx              # imm = 0x1FF
	jne	.LBB36_16
.LBB36_15:                              # %if.then12
	movq	integer_types+48(%rip), %rax
.LBB36_16:                              # %return
	retq
.LBB36_1:                               # %if.then
	movq	global_trees+200(%rip), %rax
	retq
.LBB36_12:                              # %if.then2.if.end13_crit_edge
	movq	integer_types+40(%rip), %rax
	retq
.Ltmp274:
	.size	simple_type_promotes_to, .Ltmp274-simple_type_promotes_to
	.cfi_endproc

	.globl	build_va_arg
	.align	16, 0x90
	.type	build_va_arg,@function
build_va_arg:                           # @build_va_arg
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	$133, %rdi
	movq	%rax, %rdx
	jmp	build1  # TAILCALL
.Ltmp275:
	.size	build_va_arg, .Ltmp275-build_va_arg
	.cfi_endproc

	.globl	disable_builtin_function
	.align	16, 0x90
	.type	disable_builtin_function,@function
disable_builtin_function:               # @disable_builtin_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp278:
	.cfi_def_cfa_offset 16
.Ltmp279:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$.L.str77, %rsi
	movq	$10, %rdx
                                        # kill: RDI<def> RBX<kill>
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB38_2
# BB#1:                                 # %if.end
	movq	$16, %rdi
	callq	xmalloc
	movq	disabled_builtins(%rip), %rcx
	movq	%rbx, (%rax)
	movq	%rcx, 8(%rax)
	movq	%rax, disabled_builtins(%rip)
	popq	%rbx
	retq
.LBB38_2:                               # %if.then
	movabsq	$.L.str178, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	popq	%rbx
	jmp	error  # TAILCALL
.Ltmp280:
	.size	disable_builtin_function, .Ltmp280-disable_builtin_function
	.cfi_endproc

	.globl	c_promoting_integer_type_p
	.align	16, 0x90
	.type	c_promoting_integer_type_p,@function
c_promoting_integer_type_p:             # @c_promoting_integer_type_p
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %ecx
	cmpq	$11, %rcx
	movq	$1, %rax
	je	.LBB39_10
# BB#1:                                 # %entry
	cmpq	$10, %rcx
	je	.LBB39_8
# BB#2:                                 # %entry
	cmpq	$6, %rcx
	jne	.LBB39_9
# BB#3:                                 # %sw.bb
	movq	128(%rdi), %rcx
	movq	integer_types(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_10
# BB#4:                                 # %sw.bb
	movq	integer_types+8(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_10
# BB#5:                                 # %sw.bb
	movq	integer_types+16(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_10
# BB#6:                                 # %sw.bb
	movq	integer_types+24(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_10
# BB#7:                                 # %sw.bb
	movq	integer_types+32(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB39_10
.LBB39_8:                               # %sw.bb24
	movq	integer_types+40(%rip), %rax
	movl	60(%rdi), %ecx
	movl	60(%rax), %eax
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rax              # imm = 0x1FF
	cmpq	%rax, %rcx
	sbbq	%rax, %rax
	andq	$1, %rax
	retq
.LBB39_9:                               # %sw.default
	xorq	%rax, %rax
.LBB39_10:                              # %return
	retq
.Ltmp281:
	.size	c_promoting_integer_type_p, .Ltmp281-c_promoting_integer_type_p
	.cfi_endproc

	.globl	self_promoting_args_p
	.align	16, 0x90
	.type	self_promoting_args_p,@function
self_promoting_args_p:                  # @self_promoting_args_p
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
	testq	%rdi, %rdi
	je	.LBB40_25
# BB#1:                                 # %for.body.lr.ph
	movq	global_trees+216(%rip), %rcx
	movq	global_trees+192(%rip), %r13
	movq	integer_types(%rip), %rax
	movq	%rax, -8(%rsp)          # 8-byte Spill
	movq	integer_types+8(%rip), %rax
	movq	%rax, -16(%rsp)         # 8-byte Spill
	movq	integer_types+16(%rip), %r10
	movq	integer_types+24(%rip), %r11
	movq	integer_types+32(%rip), %r14
	movq	integer_types+40(%rip), %r12
	movabsq	$0, %r9
	movabsq	$1, %r15
	.align	16, 0x90
.LBB40_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	(%rax), %rdi
	movq	32(%rax), %rbx
	testq	%rdi, %rdi
	movq	%r9, %rdx
	jne	.LBB40_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	%r15, %rdx
.LBB40_4:                               # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	%rcx, %rbx
	movq	%r9, %rsi
	je	.LBB40_6
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	%r15, %rsi
.LBB40_6:                               # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	xorq	%rax, %rax
	testq	%rdx, %rsi
	jne	.LBB40_26
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB40_26
# BB#8:                                 # %if.end4
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	128(%rbx), %rdx
	cmpq	%r13, %rdx
	je	.LBB40_26
# BB#9:                                 # %if.end8
                                        #   in Loop: Header=BB40_2 Depth=1
	movzbl	16(%rbx), %r8d
	cmpq	$11, %r8
	je	.LBB40_26
# BB#10:                                # %if.end8
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$10, %r8
	jne	.LBB40_11
# BB#20:                                # %sw.bb24.i
                                        #   in Loop: Header=BB40_2 Depth=1
	movl	60(%rbx), %eax
	movl	60(%r12), %edx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rax
	movq	%r9, %rax
	jb	.LBB40_22
# BB#21:                                # %sw.bb24.i
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	%r15, %rax
.LBB40_22:                              # %sw.bb24.i
                                        #   in Loop: Header=BB40_2 Depth=1
	jae	.LBB40_23
	jmp	.LBB40_26
	.align	16, 0x90
.LBB40_11:                              # %if.end8
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	$6, %r8
	jne	.LBB40_24
# BB#12:                                # %sw.bb.i
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	-8(%rsp), %rsi          # 8-byte Reload
	cmpq	%rsi, %rdx
	je	.LBB40_26
# BB#13:                                # %sw.bb.i
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	-16(%rsp), %rsi         # 8-byte Reload
	cmpq	%rsi, %rdx
	je	.LBB40_26
# BB#14:                                # %sw.bb.i
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	%r10, %rdx
	je	.LBB40_26
# BB#15:                                # %sw.bb.i
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	%r11, %rdx
	je	.LBB40_26
# BB#16:                                # %sw.bb.i
                                        #   in Loop: Header=BB40_2 Depth=1
	cmpq	%r14, %rdx
	je	.LBB40_26
# BB#17:                                # %c_promoting_integer_type_p.exit
                                        #   in Loop: Header=BB40_2 Depth=1
	movl	60(%rbx), %eax
	movl	60(%r12), %edx
	andq	$511, %rax              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	cmpq	%rdx, %rax
	movq	%r9, %rax
	jb	.LBB40_19
# BB#18:                                # %c_promoting_integer_type_p.exit
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	%r15, %rax
.LBB40_19:                              # %c_promoting_integer_type_p.exit
                                        #   in Loop: Header=BB40_2 Depth=1
	jb	.LBB40_26
.LBB40_23:                              # %sw.bb24.i
                                        #   in Loop: Header=BB40_2 Depth=1
	testq	%rdi, %rdi
	jne	.LBB40_2
	jmp	.LBB40_26
	.align	16, 0x90
.LBB40_24:                              # %for.inc
                                        #   in Loop: Header=BB40_2 Depth=1
	testq	%rdi, %rdi
	jne	.LBB40_2
.LBB40_25:
	movq	$1, %rax
.LBB40_26:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp298:
	.size	self_promoting_args_p, .Ltmp298-self_promoting_args_p
	.cfi_endproc

	.globl	strip_array_types
	.align	16, 0x90
	.type	strip_array_types,@function
strip_array_types:                      # @strip_array_types
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB41_2
	.align	16, 0x90
.LBB41_1:                               # %while.body
                                        #   in Loop: Header=BB41_2 Depth=1
	movq	8(%rdi), %rdi
.LBB41_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rdi), %eax
	cmpq	$18, %rax
	je	.LBB41_1
# BB#3:                                 # %while.end
	movq	%rdi, %rax
	retq
.Ltmp299:
	.size	strip_array_types, .Ltmp299-strip_array_types
	.cfi_endproc

	.globl	expand_tree_builtin
	.align	16, 0x90
	.type	expand_tree_builtin,@function
expand_tree_builtin:                    # @expand_tree_builtin
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
	movq	48(%rdi), %rcx
	andq	$1610612736, %rcx       # imm = 0x60000000
	xorq	%rax, %rax
	cmpq	$1610612736, %rcx       # imm = 0x60000000
	jne	.LBB42_16
# BB#1:                                 # %if.end
	movslq	56(%rdi), %rcx
	cmpq	$75, %rcx
	jg	.LBB42_4
# BB#2:                                 # %if.end
	decq	%rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$15, %rsi
	ja	.LBB42_16
# BB#3:                                 # %if.end
	jmpq	*.LJTI42_1(,%rsi,8)
.LBB42_6:                               # %sw.bb
	testq	%rdx, %rdx
	je	.LBB42_15
# BB#7:                                 # %if.end4
	movq	32(%rdx), %rsi
	movq	$80, %rdi
	jmp	.LBB42_8
.LBB42_4:                               # %if.end
	addq	$-76, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$5, %rcx
	ja	.LBB42_16
# BB#5:                                 # %if.end
	movq	$106, %r14
	jmpq	*.LJTI42_0(,%rcx,8)
.LBB42_18:                              # %sw.bb27
	movq	$105, %r14
	jmp	.LBB42_23
.LBB42_10:                              # %sw.bb5
	testq	%rdx, %rdx
	je	.LBB42_15
# BB#11:                                # %if.end8
	movq	32(%rdx), %rsi
	movq	$126, %rdi
	jmp	.LBB42_8
.LBB42_12:                              # %sw.bb12
	testq	%rdx, %rdx
	je	.LBB42_15
# BB#13:                                # %if.end15
	movq	32(%rdx), %rsi
	movq	$127, %rdi
	jmp	.LBB42_8
.LBB42_14:                              # %sw.bb19
	testq	%rdx, %rdx
	je	.LBB42_15
# BB#17:                                # %if.end22
	movq	32(%rdx), %rsi
	movq	$128, %rdi
.LBB42_8:                               # %if.end4
	xorq	%rdx, %rdx
.LBB42_9:                               # %if.end4
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	build_unary_op  # TAILCALL
.LBB42_15:                              # %if.then21
	movq	global_trees+88(%rip), %rax
	jmp	.LBB42_16
.LBB42_19:                              # %sw.bb28
	movq	$108, %r14
	jmp	.LBB42_23
.LBB42_20:                              # %sw.bb29
	movq	$107, %r14
	jmp	.LBB42_23
.LBB42_21:                              # %sw.bb30
	movq	$109, %r14
	jmp	.LBB42_23
.LBB42_22:                              # %sw.bb31
	movq	$103, %r14
.LBB42_23:                              # %unordered_cmp
	testq	%rsi, %rsi
	je	.LBB42_25
# BB#24:                                # %lor.lhs.false
	movq	(%rsi), %rax
	testq	%rax, %rax
	je	.LBB42_25
# BB#27:                                # %if.else
	cmpq	$0, (%rax)
	je	.LBB42_29
# BB#28:                                # %if.then41
	movq	72(%rdi), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str180, %rdi
	jmp	.LBB42_26
.LBB42_25:                              # %if.then34
	movq	72(%rdi), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str179, %rdi
.LBB42_26:                              # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees(%rip), %rax
.LBB42_16:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB42_29:                              # %if.end48
	movq	32(%rsi), %rsi
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	build_binary_op
	andq	%r14, %rbx
	cmpq	$103, %rbx
	je	.LBB42_16
# BB#30:                                # %if.then57
	movq	$96, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	jmp	.LBB42_9
.Ltmp308:
	.size	expand_tree_builtin, .Ltmp308-expand_tree_builtin
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI42_0:
	.quad	.LBB42_23
	.quad	.LBB42_18
	.quad	.LBB42_19
	.quad	.LBB42_20
	.quad	.LBB42_21
	.quad	.LBB42_22
.LJTI42_1:
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_6
	.quad	.LBB42_10
	.quad	.LBB42_10
	.quad	.LBB42_10
	.quad	.LBB42_12
	.quad	.LBB42_12
	.quad	.LBB42_12
	.quad	.LBB42_14
	.quad	.LBB42_14
	.quad	.LBB42_14

	.text
	.globl	statement_code_p
	.align	16, 0x90
	.type	statement_code_p,@function
statement_code_p:                       # @statement_code_p
	.cfi_startproc
# BB#0:                                 # %entry
	leaq	-152(%rdi), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$19, %rcx
	ja	.LBB43_2
# BB#1:                                 # %entry
	movq	$1, %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$655359, %rdx           # imm = 0x9FFFF
	jne	.LBB43_3
.LBB43_2:                               # %sw.default
	movq	lang_statement_code_p(%rip), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB43_3
# BB#4:                                 # %if.then
	jmpq	*%rcx  # TAILCALL
.LBB43_3:                               # %return
	retq
.Ltmp309:
	.size	statement_code_p, .Ltmp309-statement_code_p
	.cfi_endproc

	.globl	walk_stmt_tree
	.align	16, 0x90
	.type	walk_stmt_tree,@function
walk_stmt_tree:                         # @walk_stmt_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp316:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp317:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp318:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp319:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp320:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp321:
	.cfi_def_cfa_offset 64
.Ltmp322:
	.cfi_offset %rbx, -48
.Ltmp323:
	.cfi_offset %r12, -40
.Ltmp324:
	.cfi_offset %r13, -32
.Ltmp325:
	.cfi_offset %r14, -24
.Ltmp326:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	(%r13), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	testq	%rax, %rax
	je	.LBB44_1
# BB#2:                                 # %if.end
	movzbl	16(%rax), %edi
	leaq	-152(%rdi), %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	$19, %rcx
	ja	.LBB44_4
# BB#3:                                 # %if.end
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$655359, %rax           # imm = 0x9FFFF
	jne	.LBB44_8
.LBB44_4:                               # %sw.default.i
	movq	lang_statement_code_p(%rip), %rax
	testq	%rax, %rax
	je	.LBB44_5
# BB#6:                                 # %statement_code_p.exit
	callq	*%rax
	testq	%rbx, %rax
	je	.LBB44_7
.LBB44_8:                               # %if.end4
	movq	$1, %rax
	movl	%eax, 12(%rsp)
	leaq	12(%rsp), %rsi
	movq	%r13, %rdi
	movq	%r14, %rdx
	callq	*%r15
	testq	%rax, %rax
	jne	.LBB44_20
# BB#9:                                 # %if.end8
	movq	(%r13), %rax
	movzbl	16(%rax), %r12d
	leaq	-152(%r12), %rcx
	andq	%rbx, %rcx
	cmpq	$19, %rcx
	ja	.LBB44_11
# BB#10:                                # %if.end8
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$655359, %rax           # imm = 0x9FFFF
	jne	.LBB44_14
.LBB44_11:                              # %sw.default.i45
	movq	lang_statement_code_p(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB44_12
# BB#13:                                # %statement_code_p.exit49
	movq	%r12, %rdi
	callq	*%rcx
	testq	%rbx, %rax
	movabsq	$0, %rax
	je	.LBB44_20
.LBB44_14:                              # %if.end16
	movl	12(%rsp), %eax
	testq	%rax, %rax
	je	.LBB44_19
# BB#15:                                # %if.then18
	movslq	tree_code_length(,%r12,4), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rax, %rax
	jle	.LBB44_19
# BB#16:
	movq	$1, %rbx
	movq	$32, %r12
	.align	16, 0x90
.LBB44_18:                              # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r13), %rdi
	addq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	walk_stmt_tree
	testq	%rax, %rax
	jne	.LBB44_20
# BB#17:                                # %for.cond
                                        #   in Loop: Header=BB44_18 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	addq	$8, %r12
	movq	(%rsp), %rcx            # 8-byte Reload
	cmpq	%rcx, %rax
	jl	.LBB44_18
.LBB44_19:                              # %if.end25
	movq	(%r13), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	walk_stmt_tree
	jmp	.LBB44_20
.LBB44_1:
	xorq	%rax, %rax
	jmp	.LBB44_20
.LBB44_5:
	xorq	%rax, %rax
	jmp	.LBB44_20
.LBB44_7:
	xorq	%rax, %rax
	jmp	.LBB44_20
.LBB44_12:
	xorq	%rax, %rax
.LBB44_20:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp327:
	.size	walk_stmt_tree, .Ltmp327-walk_stmt_tree
	.cfi_endproc

	.globl	case_compare
	.align	16, 0x90
	.type	case_compare,@function
case_compare:                           # @case_compare
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB45_1
# BB#2:                                 # %if.else
	movq	$1, %rax
	testq	%rsi, %rsi
	je	.LBB45_3
# BB#4:                                 # %if.end6
	jmp	tree_int_cst_compare  # TAILCALL
.LBB45_1:                               # %if.then
	negq	%rsi
	sbbq	%rax, %rax
	retq
.LBB45_3:                               # %return
	retq
.Ltmp328:
	.size	case_compare, .Ltmp328-case_compare
	.cfi_endproc

	.globl	c_add_case_label
	.align	16, 0x90
	.type	c_add_case_label,@function
c_add_case_label:                       # @c_add_case_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp335:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp336:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp337:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp338:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp339:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp340:
	.cfi_def_cfa_offset 64
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
	movq	%rcx, %r12
	movq	%rdx, %r14
	movq	%rsi, %r13
	movq	%rdi, %rbx
	movq	$31, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	build_decl
	movq	current_function_decl(%rip), %rcx
	movq	%rcx, 80(%rax)
	movq	global_trees(%rip), %r15
	testq	%r13, %r13
	je	.LBB46_2
# BB#1:                                 # %entry
	cmpq	%r13, %r15
	je	.LBB46_2
# BB#4:                                 # %if.end5
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	%r14, %r14
	je	.LBB46_7
# BB#5:                                 # %land.lhs.true
	movq	8(%r14), %rax
	testq	%rax, %rax
	je	.LBB46_7
# BB#6:                                 # %land.lhs.true9
	movl	16(%rax), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	je	.LBB46_10
.LBB46_7:                               # %lor.lhs.false22
	xorq	%r15, %r15
	testq	%r12, %r12
	je	.LBB46_20
# BB#8:                                 # %land.lhs.true24
	movq	8(%r12), %rax
	testq	%rax, %rax
	je	.LBB46_11
# BB#9:                                 # %land.lhs.true28
	movl	16(%rax), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	jne	.LBB46_11
.LBB46_10:                              # %if.then44
	movabsq	$.L.str181, %rdi
	xorq	%rax, %rax
	callq	error
.LBB46_11:                              # %if.end45
	testq	%r12, %r12
	je	.LBB46_12
# BB#13:                                # %if.end45
	movabsq	$1, %r15
	jmp	.LBB46_14
.LBB46_2:                               # %if.then
	cmpq	$0, (%rbx)
	jne	.LBB46_55
# BB#3:                                 # %if.then2
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	jmp	.LBB46_28
.LBB46_12:
	movabsq	$0, %r15
.LBB46_14:                              # %if.end45
	je	.LBB46_20
# BB#15:                                # %if.end45
	movl	pedantic(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB46_20
# BB#16:                                # %if.then49
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB46_18
# BB#17:                                # %if.then51
	movabsq	$.L.str182, %rdi
	jmp	.LBB46_19
.LBB46_18:                              # %if.else
	movabsq	$.L.str183, %rdi
.LBB46_19:                              # %if.end53
	xorq	%rax, %rax
	callq	pedwarn
	movq	$1, %r15
.LBB46_20:                              # %if.end53
	movq	8(%r13), %rbx
	xorq	%r13, %r13
	testq	%r14, %r14
	je	.LBB46_22
# BB#21:                                # %if.then57
	movq	%r14, %rdi
	callq	check_case_value
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_and_check
	movq	%rax, %r13
.LBB46_22:                              # %if.end60
	testq	%r15, %r15
	je	.LBB46_24
# BB#23:                                # %if.then62
	movq	%r12, %rdi
	callq	check_case_value
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert_and_check
	movq	%rax, %r12
.LBB46_24:                              # %if.end65
	movq	global_trees(%rip), %r15
	cmpq	%r15, %r13
	je	.LBB46_26
# BB#25:                                # %if.end65
	cmpq	%r15, %r12
	je	.LBB46_26
# BB#29:                                # %if.end76
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB46_31
# BB#30:                                # %if.end76
	xorq	%r12, %r12
.LBB46_31:                              # %if.end76
	testq	%r13, %r13
	je	.LBB46_35
# BB#32:                                # %if.end76
	testq	%r12, %r12
	je	.LBB46_35
# BB#33:                                # %land.lhs.true84
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%rbx, %rax
	jne	.LBB46_35
# BB#34:                                # %if.then87
	movabsq	$.L.str184, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB46_35:                              # %if.end88
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	callq	splay_tree_lookup
	movq	%rax, %r15
	movq	%r13, %rax
	orq	%r12, %rax
	je	.LBB46_36
# BB#37:                                # %if.end88
	movabsq	$1, %rax
	jmp	.LBB46_38
.LBB46_26:                              # %if.then69
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	$0, (%rax)
	movq	(%rsp), %rdx            # 8-byte Reload
	jne	.LBB46_55
# BB#27:                                # %if.then72
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
.LBB46_28:                              # %return
	callq	build_case_label
	movq	%rax, %rdi
	callq	add_stmt
	movq	global_trees(%rip), %r15
	jmp	.LBB46_55
.LBB46_36:
	movabsq	$0, %rax
.LBB46_38:                              # %if.end88
	testq	%r15, %r15
	jne	.LBB46_46
# BB#39:                                # %if.end88
	testq	%rax, %rax
	je	.LBB46_46
# BB#40:                                # %if.then95
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	splay_tree_predecessor
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	splay_tree_successor
	movq	%rax, %r14
	testq	%r15, %r15
	je	.LBB46_43
# BB#41:                                # %land.lhs.true99
	movq	8(%r15), %rax
	movq	40(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB46_43
# BB#42:                                # %land.lhs.true101
	movq	%r13, %rsi
	callq	tree_int_cst_compare
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB46_46
.LBB46_43:                              # %if.else109
	testq	%r14, %r14
	je	.LBB46_54
# BB#44:                                # %if.else109
	testq	%r12, %r12
	je	.LBB46_54
# BB#45:                                # %land.lhs.true113
	movq	(%r14), %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_compare
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %r15
	jg	.LBB46_54
.LBB46_46:                              # %if.end119
	testq	%r15, %r15
	je	.LBB46_54
# BB#47:                                # %if.then121
	movq	8(%r15), %rax
	movq	48(%rax), %rbx
	testq	%r12, %r12
	je	.LBB46_49
# BB#48:                                # %if.then127
	movabsq	$.L.str185, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$.L.str186, %rsi
	jmp	.LBB46_52
.LBB46_49:                              # %if.else128
	testq	%r13, %r13
	je	.LBB46_51
# BB#50:                                # %if.then130
	movabsq	$.L.str187, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$.L.str188, %rsi
	jmp	.LBB46_52
.LBB46_51:                              # %if.else131
	movabsq	$.L.str189, %rdi
	xorq	%rax, %rax
	callq	error
	movabsq	$.L.str190, %rsi
.LBB46_52:                              # %if.end133
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	error_with_decl
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	$0, (%rax)
	jne	.LBB46_54
# BB#53:                                # %if.then136
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	build_case_label
	movq	%rax, %rdi
	callq	add_stmt
.LBB46_54:                              # %if.end140
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	(%rsp), %rdx            # 8-byte Reload
	callq	build_case_label
	movq	%rax, %rdi
	callq	add_stmt
	movq	%rax, %r15
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	splay_tree_insert
.LBB46_55:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp346:
	.size	c_add_case_label, .Ltmp346-c_add_case_label
	.cfi_endproc

	.globl	finish_label_address_expr
	.align	16, 0x90
	.type	finish_label_address_expr,@function
finish_label_address_expr:              # @finish_label_address_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp349:
	.cfi_def_cfa_offset 16
.Ltmp350:
	.cfi_offset %rbx, -16
	movl	pedantic(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB47_5
# BB#1:                                 # %if.then
	movl	c_language(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB47_3
# BB#2:                                 # %if.then1
	movabsq	$.L.str191, %rdi
	jmp	.LBB47_4
.LBB47_3:                               # %if.else
	movabsq	$.L.str192, %rdi
.LBB47_4:                               # %if.end2
	xorq	%rax, %rax
	callq	pedwarn
.LBB47_5:                               # %if.end2
	movq	%rbx, %rdi
	callq	lookup_label
	testq	%rax, %rax
	je	.LBB47_6
# BB#7:                                 # %if.else5
	movl	16(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movq	global_trees+224(%rip), %rsi
	movq	$121, %rdi
	movq	%rax, %rdx
	callq	build1
	movl	16(%rax), %ecx
	orq	$512, %rcx              # imm = 0x200
	movl	%ecx, 16(%rax)
	popq	%rbx
	retq
.LBB47_6:                               # %if.then4
	movq	global_trees+112(%rip), %rax
	popq	%rbx
	retq
.Ltmp351:
	.size	finish_label_address_expr, .Ltmp351-finish_label_address_expr
	.cfi_endproc

	.globl	mark_stmt_tree
	.align	16, 0x90
	.type	mark_stmt_tree,@function
mark_stmt_tree:                         # @mark_stmt_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp355:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp356:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp357:
	.cfi_def_cfa_offset 32
.Ltmp358:
	.cfi_offset %rbx, -24
.Ltmp359:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	(%rbx), %r14
	testq	%r14, %r14
	je	.LBB48_5
# BB#1:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_5
# BB#2:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB48_4
# BB#3:                                 # %if.then3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB48_4:                               # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB48_5:                               # %do.body9
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB48_10
# BB#6:                                 # %land.lhs.true12
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB48_10
# BB#7:                                 # %do.body16
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB48_9
# BB#8:                                 # %if.then20
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB48_9:                               # %if.end24
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB48_10:                              # %do.end32
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp360:
	.size	mark_stmt_tree, .Ltmp360-mark_stmt_tree
	.cfi_endproc

	.globl	c_mark_lang_decl
	.align	16, 0x90
	.type	c_mark_lang_decl,@function
c_mark_lang_decl:                       # @c_mark_lang_decl
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp361:
	.size	c_mark_lang_decl, .Ltmp361-c_mark_lang_decl
	.cfi_endproc

	.globl	mark_c_language_function
	.align	16, 0x90
	.type	mark_c_language_function,@function
mark_c_language_function:               # @mark_c_language_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp364:
	.cfi_def_cfa_offset 16
.Ltmp365:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB50_6
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	mark_stmt_tree
	movq	32(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB50_6
# BB#2:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB50_6
# BB#3:                                 # %do.body3
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB50_5
# BB#4:                                 # %if.then5
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB50_5:                               # %if.end8
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB50_6:                               # %do.end11
	popq	%rbx
	retq
.Ltmp366:
	.size	mark_c_language_function, .Ltmp366-mark_c_language_function
	.cfi_endproc

	.globl	c_expand_expr
	.align	16, 0x90
	.type	c_expand_expr,@function
c_expand_expr:                          # @c_expand_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp373:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp374:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp375:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp376:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp377:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp378:
	.cfi_def_cfa_offset 80
.Ltmp379:
	.cfi_offset %rbx, -48
.Ltmp380:
	.cfi_offset %r12, -40
.Ltmp381:
	.cfi_offset %r13, -32
.Ltmp382:
	.cfi_offset %r14, -24
.Ltmp383:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movzbl	16(%r13), %eax
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r14
	cmpq	$53, %rax
	je	.LBB51_21
# BB#1:                                 # %entry
	cmpq	$170, %rax
	jne	.LBB51_2
# BB#46:                                # %sw.bb126
	movq	32(%r13), %rax
	movq	32(%rax), %rbx
	movq	%rbx, %rdi
	callq	emit_local_var
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_expr  # TAILCALL
.LBB51_21:                              # %sw.bb77
	movq	32(%r13), %rax
	movzbl	16(%rax), %ecx
	cmpq	$121, %rcx
	jne	.LBB51_44
# BB#22:                                # %land.lhs.true86
	movq	32(%rax), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$30, %rax
	jne	.LBB51_44
# BB#23:                                # %land.lhs.true98
	movq	48(%rbx), %rax
	andq	$1610612736, %rax       # imm = 0x60000000
	cmpq	$536870912, %rax        # imm = 0x20000000
	jne	.LBB51_44
# BB#24:                                # %if.then123
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	40(%r13), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	const_int_rtx+512(%rip), %rax
	movl	56(%rbx), %r15d
	cmpq	%r14, %rax
	jne	.LBB51_25
# BB#26:                                # %if.then123
	movabsq	$1, %r8
	jmp	.LBB51_27
.LBB51_2:                               # %entry
	cmpq	$169, %rax
	jne	.LBB51_47
# BB#3:                                 # %sw.bb
	callq	push_temp_slots
	movzbl	19(%r13), %edi
	notq	%rdi
	andq	$1, %rdi
	callq	expand_start_stmt_expr
	movq	const_int_rtx+512(%rip), %rcx
	movq	%rax, %rbx
	cmpq	%r14, %rcx
	je	.LBB51_4
# BB#5:                                 # %land.lhs.true
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	32(%r13), %rax
	movzbl	16(%rax), %ecx
	cmpq	$153, %rcx
	jne	.LBB51_12
# BB#6:                                 # %land.lhs.true10
	movq	32(%rax), %rcx
	movzbl	16(%rcx), %eax
	cmpq	$166, %rax
	jne	.LBB51_12
# BB#7:                                 # %if.then
	movq	(%rcx), %rdx
	.align	16, 0x90
.LBB51_8:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %rsi
	movq	(%rsi), %rdx
	movq	%rcx, %rax
	testq	%rdx, %rdx
	movq	%rsi, %rcx
	jne	.LBB51_8
# BB#9:                                 # %while.end
	movzbl	16(%rsi), %ecx
	cmpq	$166, %rcx
	jne	.LBB51_12
# BB#10:                                # %land.lhs.true39
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	cmpq	$152, %rdx
	jne	.LBB51_12
# BB#11:                                # %if.then45
	orq	$1024, %rcx             # imm = 0x400
	movl	%ecx, 16(%rax)
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB51_12
.LBB51_25:
	movabsq	$0, %r8
.LBB51_27:                              # %if.then123
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	movq	%r12, %r10
	jne	.LBB51_31
# BB#28:                                # %land.lhs.true.i
	movq	72(%rbx), %rax
	movq	32(%rax), %rdi
	movabsq	$.L.str77, %rsi
	movq	$10, %rdx
	movq	%r10, 8(%rsp)           # 8-byte Spill
	movq	%r8, %r12
	callq	strncmp
	movq	%r12, %r8
	movq	8(%rsp), %r10           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB51_31
# BB#29:                                # %if.then.i
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r8, %rdx
	jmp	.LBB51_30
.LBB51_31:                              # %if.end.i
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$67, %rax
	jg	.LBB51_35
# BB#32:                                # %if.end.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	cmpq	$64, %r15
	jne	.LBB51_42
# BB#33:                                # %sw.bb.i
	xorq	%r9, %r9
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r10, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r15
                                        # kill: R8<def> R15<kill>
	callq	c_expand_builtin_printf
	jmp	.LBB51_34
.LBB51_35:                              # %if.end.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	cmpq	$68, %r15
	je	.LBB51_41
# BB#36:                                # %if.end.i
	cmpq	$71, %r15
	jne	.LBB51_37
# BB#39:                                # %sw.bb31.i
	movq	$1, %r9
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r10, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r15
                                        # kill: R8<def> R15<kill>
	callq	c_expand_builtin_printf
	jmp	.LBB51_40
.LBB51_4:
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
.LBB51_12:                              # %if.end49
	movq	32(%r13), %rdi
	callq	expand_stmt
	movq	%rbx, %rdi
	callq	expand_end_stmt_expr
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	movq	%r15, %rcx
	callq	expand_expr
	movq	%rax, %rbx
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB51_17
# BB#13:                                # %land.lhs.true56
	movl	(%rbx), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$66, %rcx
	jne	.LBB51_17
# BB#14:                                # %if.then60
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$3342336, %rax          # imm = 0x330000
	jne	.LBB51_15
# BB#16:                                # %if.else
	movq	%rbx, %rdi
	callq	preserve_temp_slots
	jmp	.LBB51_17
.LBB51_41:                              # %sw.bb36.i
	xorq	%r9, %r9
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r10, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r15
                                        # kill: R8<def> R15<kill>
	callq	c_expand_builtin_fprintf
.LBB51_34:                              # %sw.bb.i
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movabsq	$0, %r14
	jne	.LBB51_20
	jmp	.LBB51_43
.LBB51_37:                              # %if.end.i
	cmpq	$75, %r15
	jne	.LBB51_42
# BB#38:                                # %sw.bb41.i
	movq	$1, %r9
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r10, %rdx
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%r8, %r15
                                        # kill: R8<def> R15<kill>
	callq	c_expand_builtin_fprintf
.LBB51_40:                              # %sw.bb31.i
	movq	%rax, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	jne	.LBB51_20
	jmp	.LBB51_43
.LBB51_42:                              # %sw.default.i
	movq	72(%rbx), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str224, %rdi
	xorq	%rax, %rax
	movq	%r8, %r15
	callq	error
.LBB51_43:                              # %sw.epilog.i
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
.LBB51_30:                              # %if.then.i
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_call  # TAILCALL
.LBB51_15:                              # %if.then65
	movq	%rbx, %rdi
	callq	copy_to_reg
	movq	%rax, %rbx
.LBB51_17:                              # %if.end68
	movzbl	19(%r13), %eax
	testq	$1, %rax
	je	.LBB51_19
# BB#18:                                # %if.then75
	xorq	%rdi, %rdi
	callq	preserve_temp_slots
.LBB51_19:                              # %if.end76
	callq	pop_temp_slots
.LBB51_20:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB51_44:                              # %if.else125
	movabsq	$.L.str193, %rax
	movabsq	$.L__FUNCTION__.c_expand_expr, %rcx
	movq	$3632, %rsi             # imm = 0xE30
	jmp	.LBB51_45
.LBB51_47:                              # %sw.default
	movabsq	$.L.str193, %rax
	movabsq	$.L__FUNCTION__.c_expand_expr, %rcx
	movq	$3646, %rsi             # imm = 0xE3E
.LBB51_45:                              # %if.else125
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp384:
	.size	c_expand_expr, .Ltmp384-c_expand_expr
	.cfi_endproc

	.globl	c_safe_from_p
	.align	16, 0x90
	.type	c_safe_from_p,@function
c_safe_from_p:                          # @c_safe_from_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp390:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp391:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp392:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp393:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp394:
	.cfi_def_cfa_offset 48
.Ltmp395:
	.cfi_offset %rbx, -40
.Ltmp396:
	.cfi_offset %r12, -32
.Ltmp397:
	.cfi_offset %r14, -24
.Ltmp398:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movl	16(%r15), %eax
	movq	%rdi, %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$154, %rcx
	jne	.LBB52_5
# BB#1:                                 # %if.then
	movq	32(%r15), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$34, %rdx
	jne	.LBB52_5
# BB#2:                                 # %land.lhs.true
	movq	104(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB52_5
# BB#3:                                 # %land.lhs.true8
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	safe_from_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB52_10
# BB#4:                                 # %land.lhs.true8.if.end13_crit_edge
	movl	16(%r15), %eax
.LBB52_5:                               # %if.end13
	andq	$255, %rax
	leaq	-152(%rax), %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$19, %rcx
	ja	.LBB52_7
# BB#6:                                 # %if.end13
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$655359, %rdx           # imm = 0x9FFFF
	jne	.LBB52_9
.LBB52_7:                               # %sw.default.i
	movq	lang_statement_code_p(%rip), %rcx
	testq	%rcx, %rcx
	movq	$1, %rbx
	je	.LBB52_10
# BB#8:                                 # %statement_code_p.exit
	movq	%rax, %rdi
	callq	*%rcx
	testq	%r12, %rax
	je	.LBB52_10
.LBB52_9:                               # %land.lhs.true20
	movq	(%r15), %rsi
	testq	%rsi, %rsi
	movq	$1, %rbx
	je	.LBB52_10
# BB#11:                                # %if.then23
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	safe_from_p  # TAILCALL
.LBB52_10:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp399:
	.size	c_safe_from_p, .Ltmp399-c_safe_from_p
	.cfi_endproc

	.globl	c_unsafe_for_reeval
	.align	16, 0x90
	.type	c_unsafe_for_reeval,@function
c_unsafe_for_reeval:                    # @c_unsafe_for_reeval
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	addq	$-169, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	movq	$2, %rax
	jb	.LBB53_2
# BB#1:                                 # %entry
	movq	$-1, %rax
.LBB53_2:                               # %entry
	retq
.Ltmp400:
	.size	c_unsafe_for_reeval, .Ltmp400-c_unsafe_for_reeval
	.cfi_endproc

	.globl	c_staticp
	.align	16, 0x90
	.type	c_staticp,@function
c_staticp:                              # @c_staticp
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	cmpq	$170, %rax
	jne	.LBB54_2
# BB#1:                                 # %land.lhs.true
	movq	32(%rdi), %rax
	movq	32(%rax), %rax
	movzbl	18(%rax), %ecx
	movq	$1, %rax
	