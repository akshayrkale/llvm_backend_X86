	.file	"mytest/gcc/c-common.c.bc"
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
	c