	.file	"c-common.c.bc"
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
	testq	$4, %rcx
	jne	.LBB54_3
.LBB54_2:                               # %if.end
	xorq	%rax, %rax
.LBB54_3:                               # %return
	retq
.Ltmp401:
	.size	c_staticp, .Ltmp401-c_staticp
	.cfi_endproc

	.globl	add_c_tree_codes
	.align	16, 0x90
	.type	add_c_tree_codes,@function
add_c_tree_codes:                       # @add_c_tree_codes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp403:
	.cfi_def_cfa_offset 16
	movq	$101, %rax
	movb	%al, tree_code_type+171(%rip)
	movabsq	$7306357456645743973, %rax # imm = 0x6565656565656565
	movq	%rax, tree_code_type+163(%rip)
	movq	%rax, tree_code_type+155(%rip)
	movabsq	$7306357233304041592, %rax # imm = 0x6565653165317878
	movq	%rax, tree_code_type+147(%rip)
	movabsq	$c_tree_code_length, %rsi
	movabsq	$tree_code_length+588, %rdi
	movq	$100, %rdx
	callq	memcpy
	movabsq	$c_tree_code_name, %rsi
	movabsq	$tree_code_name+1176, %rdi
	movq	$200, %rdx
	callq	memcpy
	movq	$c_unsafe_for_reeval, lang_unsafe_for_reeval(%rip)
	popq	%rax
	retq
.Ltmp404:
	.size	add_c_tree_codes, .Ltmp404-add_c_tree_codes
	.cfi_endproc

	.globl	boolean_increment
	.align	16, 0x90
	.type	boolean_increment,@function
boolean_increment:                      # @boolean_increment
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp409:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp410:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp411:
	.cfi_def_cfa_offset 32
.Ltmp412:
	.cfi_offset %rbx, -32
.Ltmp413:
	.cfi_offset %r14, -24
.Ltmp414:
	.cfi_offset %r15, -16
	movl	c_language(%rip), %eax
	movq	%rdi, %rbx
	cmpq	$1, %rax
	jne	.LBB56_2
# BB#1:
	movq	c_global_trees+136(%rip), %r14
	jmp	.LBB56_3
.LBB56_2:                               # %select.mid
	movq	c_global_trees+160(%rip), %r14
.LBB56_3:                               # %select.end
	movq	%rsi, %rdi
	callq	stabilize_reference
	movq	%rax, %r15
	addq	$-129, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$3, %rax
	ja	.LBB56_9
# BB#4:                                 # %select.end
	jmpq	*.LJTI56_0(,%rax,8)
.LBB56_7:                               # %sw.bb13
	movq	8(%r15), %rbx
	movq	%r15, %rdi
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	jmp	.LBB56_11
.LBB56_10:                              # %sw.bb
	movq	8(%r15), %rsi
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%r14, %rcx
	jmp	.LBB56_11
.LBB56_8:                               # %sw.bb18
	movq	8(%r15), %rbx
	movq	%r15, %rdi
	callq	invert_truthvalue
	movq	%rax, %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%r15, %rdx
	jmp	.LBB56_6
.LBB56_5:                               # %sw.bb2
	movq	8(%r15), %rsi
	movq	$48, %rdi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%r14, %rcx
.LBB56_6:                               # %sw.epilog
	callq	build
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movq	8(%rbx), %rsi
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	8(%rbx), %rsi
	movq	%rax, %rcx
	movq	$47, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
.LBB56_11:                              # %sw.epilog
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB56_9:                               # %sw.default
	movabsq	$.L.str193, %rdi
	movabsq	$.L__FUNCTION__.boolean_increment, %rdx
	movq	$4080, %rsi             # imm = 0xFF0
	callq	fancy_abort
.Ltmp415:
	.size	boolean_increment, .Ltmp415-boolean_increment
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI56_0:
	.quad	.LBB56_7
	.quad	.LBB56_10
	.quad	.LBB56_8
	.quad	.LBB56_5

	.text
	.globl	c_common_init_options
	.align	16, 0x90
	.type	c_common_init_options,@function
c_common_init_options:                  # @c_common_init_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp418:
	.cfi_def_cfa_offset 16
.Ltmp419:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movl	%eax, c_language(%rip)
	je	.LBB57_5
# BB#1:                                 # %cond.false
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB57_2
# BB#3:                                 # %cond.false
	movabsq	$1, %rax
	jmp	.LBB57_4
.LBB57_2:
	movabsq	$0, %rax
.LBB57_4:                               # %cond.false
	xorq	$1, %rax
	leaq	5(%rax,%rax), %rdi
.LBB57_5:                               # %cond.end
	callq	cpp_create_reader
	movq	%rax, parse_in(%rip)
	movl	%ebx, flag_bounds_check(%rip)
	popq	%rbx
	retq
.Ltmp420:
	.size	c_common_init_options, .Ltmp420-c_common_init_options
	.cfi_endproc

	.globl	c_common_post_options
	.align	16, 0x90
	.type	c_common_post_options,@function
c_common_post_options:                  # @c_common_post_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp423:
	.cfi_def_cfa_offset 16
.Ltmp424:
	.cfi_offset %rbx, -16
	movq	parse_in(%rip), %rdi
	callq	cpp_post_options
	movl	flag_instrument_function_entry_exit(%rip), %eax
	movq	$1, %rcx
	movl	%ecx, flag_inline_trees(%rip)
	testq	%rax, %rax
	jne	.LBB58_5
# BB#1:                                 # %if.then
	movl	flag_no_inline(%rip), %eax
	testq	%rax, %rax
	jne	.LBB58_3
# BB#2:                                 # %if.then2
	movq	$1, %rax
	movl	%eax, flag_no_inline(%rip)
.LBB58_3:                               # %if.end
	movl	flag_inline_functions(%rip), %eax
	testq	%rax, %rax
	je	.LBB58_5
# BB#4:                                 # %if.then4
	movq	$2, %rax
	movl	%eax, flag_inline_trees(%rip)
	xorq	%rax, %rax
	movl	%eax, flag_inline_functions(%rip)
.LBB58_5:                               # %if.end6
	movl	flag_bounds_check(%rip), %eax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	cmpq	%rbx, %rax
	jne	.LBB58_7
# BB#6:                                 # %if.then7
	movl	flag_bounded_pointers(%rip), %eax
	movl	%eax, flag_bounds_check(%rip)
.LBB58_7:                               # %if.end8
	movl	warn_format_y2k(%rip), %ecx
	movl	warn_format(%rip), %eax
	testq	%rcx, %rcx
	je	.LBB58_10
# BB#8:                                 # %if.end8
	testq	%rbx, %rax
	jne	.LBB58_10
# BB#9:                                 # %if.then11
	movabsq	$.L.str194, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	warn_format(%rip), %eax
.LBB58_10:                              # %if.end12
	movl	warn_format_extra_args(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB58_13
# BB#11:                                # %if.end12
	testq	%rbx, %rax
	jne	.LBB58_13
# BB#12:                                # %if.then16
	movabsq	$.L.str195, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	warn_format(%rip), %eax
.LBB58_13:                              # %if.end17
	movl	warn_format_nonliteral(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB58_16
# BB#14:                                # %if.end17
	testq	%rbx, %rax
	jne	.LBB58_16
# BB#15:                                # %if.then21
	movabsq	$.L.str196, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	warn_format(%rip), %eax
.LBB58_16:                              # %if.end22
	movl	warn_format_security(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB58_19
# BB#17:                                # %if.end22
	testq	%rbx, %rax
	jne	.LBB58_19
# BB#18:                                # %if.then26
	movabsq	$.L.str197, %rdi
	xorq	%rax, %rax
	callq	warning
	movl	warn_format(%rip), %eax
.LBB58_19:                              # %if.end27
	movl	warn_missing_format_attribute(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB58_22
# BB#20:                                # %if.end27
	testq	%rbx, %rax
	jne	.LBB58_22
# BB#21:                                # %if.then31
	movabsq	$.L.str198, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB58_22:                              # %if.end32
	movq	parse_in(%rip), %rdi
	callq	cpp_errors
	movq	global_dc(%rip), %rcx
	movl	60(%rcx), %edx
	addq	%rax, %rdx
	movl	%edx, 60(%rcx)
	popq	%rbx
	retq
.Ltmp425:
	.size	c_common_post_options, .Ltmp425-c_common_post_options
	.cfi_endproc

	.globl	c_common_init
	.align	16, 0x90
	.type	c_common_init,@function
c_common_init:                          # @c_common_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp428:
	.cfi_def_cfa_offset 16
.Ltmp429:
	.cfi_offset %rbx, -16
	callq	init_c_lex
	movq	%rax, %rbx
	callq	init_pragma
	movzbl	c_attrs_initialized(%rip), %eax
	testq	%rax, %rax
	jne	.LBB59_2
# BB#1:                                 # %if.then
	callq	c_init_attributes
.LBB59_2:                               # %if.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp430:
	.size	c_common_init, .Ltmp430-c_common_init
	.cfi_endproc

	.align	16, 0x90
	.type	c_init_attributes,@function
c_init_attributes:                      # @c_init_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp437:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp438:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp439:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp440:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp441:
	.cfi_def_cfa_offset 48
.Ltmp442:
	.cfi_offset %rbx, -48
.Ltmp443:
	.cfi_offset %r12, -40
.Ltmp444:
	.cfi_offset %r13, -32
.Ltmp445:
	.cfi_offset %r14, -24
.Ltmp446:
	.cfi_offset %r15, -16
	movq	$0, built_in_attributes(%rip)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, built_in_attributes+8(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+16(%rip)
	movq	$1, %rdi
	xorq	%rsi, %rsi
	movq	$1, %r14
	callq	build_int_2_wide
	movq	%rax, built_in_attributes+24(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+32(%rip)
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, built_in_attributes+40(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+48(%rip)
	movq	$3, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, built_in_attributes+56(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+64(%rip)
	movq	$4, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, built_in_attributes+72(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	%rax, %rsi
	movq	%rdi, %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+80(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+24(%rip), %rsi
	movq	built_in_attributes+16(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+88(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+24(%rip), %rsi
	movq	built_in_attributes+48(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+96(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+40(%rip), %rsi
	movq	built_in_attributes+16(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+104(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+40(%rip), %rsi
	movq	built_in_attributes+64(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+112(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+56(%rip), %rsi
	movq	built_in_attributes+16(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+120(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+56(%rip), %rsi
	movq	built_in_attributes+80(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+128(%rip)
	movabsq	$.L.str201, %rbx
	movq	%rbx, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+136(%rip)
	movabsq	$.L.str202, %r13
	movq	%r13, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+144(%rip)
	movabsq	$.L.str203, %r12
	movq	%r12, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+152(%rip)
	movabsq	$.L.str204, %r15
	movq	%r15, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+160(%rip)
	movabsq	$.L.str205, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+168(%rip)
	movabsq	$.L.str206, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+176(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+88(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+184(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+192(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+96(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+200(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+208(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+104(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+216(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+224(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+112(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+232(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+240(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+120(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+248(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+256(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+136(%rip), %rsi
	movq	built_in_attributes+128(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+264(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+272(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+144(%rip), %rsi
	movq	built_in_attributes+88(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+280(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+288(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+144(%rip), %rsi
	movq	built_in_attributes+96(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+296(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+304(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+144(%rip), %rsi
	movq	built_in_attributes+104(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+312(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+320(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+144(%rip), %rsi
	movq	built_in_attributes+112(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+328(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+336(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+152(%rip), %rsi
	movq	built_in_attributes+120(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+344(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+352(%rip)
	movq	built_in_attributes(%rip), %rdi
	movq	built_in_attributes+160(%rip), %rsi
	movq	built_in_attributes+128(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+360(%rip)
	movq	built_in_attributes+168(%rip), %rdi
	movq	built_in_attributes(%rip), %rdx
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, built_in_attributes+368(%rip)
	movq	built_in_attributes+176(%rip), %rdi
	movq	built_in_attributes+32(%rip), %rsi
	movq	built_in_attributes(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+376(%rip)
	movq	built_in_attributes+176(%rip), %rdi
	movq	built_in_attributes+48(%rip), %rsi
	movq	built_in_attributes(%rip), %rdx
	callq	tree_cons
	movq	%rax, built_in_attributes+384(%rip)
	movabsq	$.L.str142, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+392(%rip)
	movabsq	$.L.str146, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+400(%rip)
	movabsq	$.L.str149, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+408(%rip)
	movabsq	$.L.str153, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+416(%rip)
	movq	%rbx, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+424(%rip)
	movabsq	$.L.str207, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+432(%rip)
	movabsq	$.L.str208, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+440(%rip)
	movq	%r13, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+448(%rip)
	movabsq	$.L.str209, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+456(%rip)
	movabsq	$.L.str210, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+464(%rip)
	movabsq	$.L.str211, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+472(%rip)
	movabsq	$.L.str212, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+480(%rip)
	movabsq	$.L.str213, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+488(%rip)
	movq	%r12, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+496(%rip)
	movabsq	$.L.str214, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+504(%rip)
	movabsq	$.L.str215, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+512(%rip)
	movabsq	$.L.str216, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+520(%rip)
	movabsq	$.L.str217, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+528(%rip)
	movabsq	$.L.str218, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+536(%rip)
	movabsq	$.L.str219, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+544(%rip)
	movabsq	$.L.str220, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+552(%rip)
	movabsq	$.L.str221, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+560(%rip)
	movq	%r15, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+568(%rip)
	movabsq	$.L.str222, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+576(%rip)
	movabsq	$.L.str223, %rdi
	callq	get_identifier
	movq	%rax, built_in_attributes+584(%rip)
	movq	$74, %rsi
	movabsq	$built_in_attributes, %rdi
	callq	ggc_add_tree_root
	movb	%r14b, c_attrs_initialized(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp447:
	.size	c_init_attributes, .Ltmp447-c_init_attributes
	.cfi_endproc

	.globl	c_common_finish
	.align	16, 0x90
	.type	c_common_finish,@function
c_common_finish:                        # @c_common_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp449:
	.cfi_def_cfa_offset 16
	movq	parse_in(%rip), %rdi
	callq	cpp_finish
	movq	parse_in(%rip), %rdi
	callq	cpp_errors
	movq	global_dc(%rip), %rcx
	movl	60(%rcx), %edx
	addq	%rax, %rdx
	movl	%edx, 60(%rcx)
	popq	%rax
	retq
.Ltmp450:
	.size	c_common_finish, .Ltmp450-c_common_finish
	.cfi_endproc

	.globl	c_common_insert_default_attributes
	.align	16, 0x90
	.type	c_common_insert_default_attributes,@function
c_common_insert_default_attributes:     # @c_common_insert_default_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp455:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp456:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp457:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp458:
	.cfi_def_cfa_offset 48
.Ltmp459:
	.cfi_offset %rbx, -32
.Ltmp460:
	.cfi_offset %r14, -24
.Ltmp461:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)
	movzbl	c_attrs_initialized(%rip), %eax
	movq	72(%rdi), %r15
	testq	%rax, %rax
	jne	.LBB62_2
# BB#1:                                 # %if.then
	callq	c_init_attributes
.LBB62_2:                               # %if.end
	movq	built_in_attributes+392(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB62_4
# BB#3:                                 # %if.then3
	movq	built_in_attributes+208(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
.LBB62_4:                               # %if.end4
	movq	built_in_attributes+400(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB62_6
# BB#5:                                 # %if.then6
	movq	built_in_attributes+240(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
.LBB62_6:                               # %if.end8
	movq	built_in_attributes+408(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB62_8
# BB#7:                                 # %if.then10
	movq	built_in_attributes+208(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
.LBB62_8:                               # %if.end12
	movq	built_in_attributes+416(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB62_10
# BB#9:                                 # %if.then14
	movq	built_in_attributes+240(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
.LBB62_10:                              # %if.end16
	movl	flag_hosted(%rip), %eax
	testq	%rax, %rax
	je	.LBB62_13
# BB#11:                                # %if.end16
	movq	built_in_attributes+424(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_13
# BB#12:                                # %if.then19
	movq	built_in_attributes+208(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_13:                              # %if.end21
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB62_16
# BB#14:                                # %if.end21
	movq	built_in_attributes+432(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_16
# BB#15:                                # %if.then25
	movq	built_in_attributes+240(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_16:                              # %if.end27
	testq	%r14, %rax
	je	.LBB62_19
# BB#17:                                # %if.end27
	movq	built_in_attributes+440(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_19
# BB#18:                                # %if.then31
	movq	built_in_attributes+240(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_19:                              # %if.end33
	testq	%r14, %rax
	je	.LBB62_22
# BB#20:                                # %if.end33
	movq	built_in_attributes+448(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_22
# BB#21:                                # %if.then37
	movq	built_in_attributes+304(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_22:                              # %if.end39
	testq	%r14, %rax
	je	.LBB62_25
# BB#23:                                # %if.end39
	movq	built_in_attributes+456(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_25
# BB#24:                                # %if.then43
	movq	built_in_attributes+336(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_25:                              # %if.end45
	testq	%r14, %rax
	je	.LBB62_28
# BB#26:                                # %if.end45
	movq	built_in_attributes+464(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_28
# BB#27:                                # %if.then49
	movq	built_in_attributes+336(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_28:                              # %if.end51
	testq	%r14, %rax
	je	.LBB62_31
# BB#29:                                # %if.end51
	movq	built_in_attributes+472(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_31
# BB#30:                                # %if.then55
	movq	built_in_attributes+192(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_31:                              # %if.end57
	testq	%r14, %rax
	je	.LBB62_34
# BB#32:                                # %if.end57
	movq	built_in_attributes+480(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_34
# BB#33:                                # %if.then61
	movq	built_in_attributes+224(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_34:                              # %if.end63
	testq	%r14, %rax
	je	.LBB62_37
# BB#35:                                # %if.end63
	movq	built_in_attributes+488(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_37
# BB#36:                                # %if.then67
	movq	built_in_attributes+224(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_37:                              # %if.end69
	testq	%r14, %rax
	je	.LBB62_40
# BB#38:                                # %if.end69
	movq	built_in_attributes+496(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_40
# BB#39:                                # %if.then73
	movq	built_in_attributes+352(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_40:                              # %if.end75
	xorq	%rbx, %rbx
	testq	%r14, %rax
	je	.LBB62_61
# BB#41:                                # %land.lhs.true77
	movl	flag_isoc99(%rip), %ecx
	movl	flag_noniso_default_format_attributes(%rip), %edx
	orq	%rcx, %rdx
	testq	%r14, %rdx
	je	.LBB62_44
# BB#42:                                # %land.lhs.true77
	movq	built_in_attributes+504(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_44
# BB#43:                                # %if.then82
	movq	built_in_attributes+272(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_44:                              # %if.end84
	testq	%r14, %rax
	je	.LBB62_61
# BB#45:                                # %land.lhs.true86
	movl	flag_isoc99(%rip), %ecx
	movl	flag_noniso_default_format_attributes(%rip), %edx
	orq	%rcx, %rdx
	testq	%r14, %rdx
	je	.LBB62_48
# BB#46:                                # %land.lhs.true86
	movq	built_in_attributes+512(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_48
# BB#47:                                # %if.then92
	movq	built_in_attributes+256(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_48:                              # %if.end94
	testq	%r14, %rax
	je	.LBB62_61
# BB#49:                                # %land.lhs.true96
	movl	flag_isoc99(%rip), %ecx
	movl	flag_noniso_default_format_attributes(%rip), %edx
	orq	%rcx, %rdx
	testq	%r14, %rdx
	je	.LBB62_52
# BB#50:                                # %land.lhs.true96
	movq	built_in_attributes+520(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_52
# BB#51:                                # %if.then102
	movq	built_in_attributes+288(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_52:                              # %if.end104
	testq	%r14, %rax
	je	.LBB62_61
# BB#53:                                # %land.lhs.true106
	movl	flag_isoc99(%rip), %ecx
	movl	flag_noniso_default_format_attributes(%rip), %edx
	orq	%rcx, %rdx
	testq	%r14, %rdx
	je	.LBB62_56
# BB#54:                                # %land.lhs.true106
	movq	built_in_attributes+528(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_56
# BB#55:                                # %if.then112
	movq	built_in_attributes+320(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %eax
.LBB62_56:                              # %if.end114
	testq	%r14, %rax
	je	.LBB62_61
# BB#57:                                # %land.lhs.true116
	movl	flag_isoc99(%rip), %ecx
	movl	flag_noniso_default_format_attributes(%rip), %edx
	orq	%rcx, %rdx
	testq	%r14, %rdx
	je	.LBB62_58
# BB#59:                                # %land.lhs.true116
	movq	built_in_attributes+536(%rip), %rcx
	cmpq	%rcx, %r15
	movq	%rax, %rbx
	jne	.LBB62_61
# BB#60:                                # %if.then122
	movq	built_in_attributes+320(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	jmp	.LBB62_61
.LBB62_58:
	movq	%rax, %rbx
.LBB62_61:                              # %if.end124
	movl	flag_noniso_default_format_attributes(%rip), %eax
	testq	%r14, %rbx
	je	.LBB62_65
# BB#62:                                # %if.end124
	testq	%r14, %rax
	je	.LBB62_65
# BB#63:                                # %if.end124
	movq	built_in_attributes+544(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_65
# BB#64:                                # %if.then130
	movq	built_in_attributes+376(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	movl	flag_noniso_default_format_attributes(%rip), %eax
.LBB62_65:                              # %if.end132
	testq	%r14, %rbx
	je	.LBB62_69
# BB#66:                                # %if.end132
	testq	%r14, %rax
	je	.LBB62_69
# BB#67:                                # %if.end132
	movq	built_in_attributes+552(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_69
# BB#68:                                # %if.then138
	movq	built_in_attributes+384(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	movl	flag_noniso_default_format_attributes(%rip), %eax
.LBB62_69:                              # %if.end140
	testq	%r14, %rbx
	je	.LBB62_73
# BB#70:                                # %if.end140
	testq	%r14, %rax
	je	.LBB62_73
# BB#71:                                # %if.end140
	movq	built_in_attributes+560(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_73
# BB#72:                                # %if.then146
	movq	built_in_attributes+384(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	movl	flag_noniso_default_format_attributes(%rip), %eax
.LBB62_73:                              # %if.end148
	testq	%r14, %rbx
	je	.LBB62_77
# BB#74:                                # %if.end148
	testq	%r14, %rax
	je	.LBB62_77
# BB#75:                                # %if.end148
	movq	built_in_attributes+568(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_77
# BB#76:                                # %if.then154
	movq	built_in_attributes+368(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	movl	flag_noniso_default_format_attributes(%rip), %eax
.LBB62_77:                              # %if.end156
	testq	%r14, %rbx
	je	.LBB62_81
# BB#78:                                # %if.end156
	testq	%r14, %rax
	je	.LBB62_81
# BB#79:                                # %if.end156
	movq	built_in_attributes+576(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB62_81
# BB#80:                                # %if.then162
	movq	built_in_attributes+208(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
	movl	flag_hosted(%rip), %ebx
	movl	flag_noniso_default_format_attributes(%rip), %eax
.LBB62_81:                              # %if.end164
	testq	%r14, %rbx
	je	.LBB62_85
# BB#82:                                # %if.end164
	testq	%r14, %rax
	je	.LBB62_85
# BB#83:                                # %if.end164
	movq	built_in_attributes+584(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB62_85
# BB#84:                                # %if.then170
	movq	built_in_attributes+240(%rip), %rsi
	leaq	8(%rsp), %rdi
	movq	$16, %rdx
	callq	decl_attributes
.LBB62_85:                              # %if.end172
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp462:
	.size	c_common_insert_default_attributes, .Ltmp462-c_common_insert_default_attributes
	.cfi_endproc

	.globl	shadow_warning
	.align	16, 0x90
	.type	shadow_warning,@function
shadow_warning:                         # @shadow_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp465:
	.cfi_def_cfa_offset 16
.Ltmp466:
	.cfi_offset %rbx, -16
	movq	32(%rsi), %rsi
	movq	%rdx, %rbx
	movq	%rdi, %rcx
	movabsq	$.L.str199, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rdx
	callq	warning
	movq	24(%rbx), %rdi
	movl	32(%rbx), %esi
	movabsq	$.L.str200, %rdx
	xorq	%rax, %rax
	popq	%rbx
	jmp	warning_with_file_and_line  # TAILCALL
.Ltmp467:
	.size	shadow_warning, .Ltmp467-shadow_warning
	.cfi_endproc

	.align	16, 0x90
	.type	c_expand_builtin_printf,@function
c_expand_builtin_printf:                # @c_expand_builtin_printf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp471:
	.cfi_def_cfa_offset 16
.Ltmp472:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp473:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
.Ltmp474:
	.cfi_offset %rbx, -56
.Ltmp475:
	.cfi_offset %r12, -48
.Ltmp476:
	.cfi_offset %r13, -40
.Ltmp477:
	.cfi_offset %r14, -32
.Ltmp478:
	.cfi_offset %r15, -24
	movq	%rcx, -56(%rbp)         # 8-byte Spill
	movq	%rdx, -64(%rbp)         # 8-byte Spill
	movq	%rdi, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r9
	jne	.LBB64_1
# BB#2:                                 # %entry
	movabsq	$built_in_decls+504, %rdx
	jmp	.LBB64_3
.LBB64_1:
	movabsq	$built_in_decls+560, %rdx
.LBB64_3:                               # %entry
	movq	%rsi, -80(%rbp)         # 8-byte Spill
	testq	%r9, %r9
	jne	.LBB64_4
# BB#5:                                 # %entry
	movabsq	$built_in_decls+496, %rax
	jmp	.LBB64_6
.LBB64_4:
	movabsq	$built_in_decls+552, %rax
.LBB64_6:                               # %entry
	movq	(%rax), %rbx
	testq	%r12, %r8
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rcx
	jne	.LBB64_8
# BB#7:                                 # %entry
	movq	%rsi, %rcx
.LBB64_8:                               # %entry
	movq	%r8, -72(%rbp)          # 8-byte Spill
	movq	(%rdx), %rdi
	testq	%rbx, %rbx
	movq	%rax, %rdx
	jne	.LBB64_10
# BB#9:                                 # %entry
	movq	%rsi, %rdx
.LBB64_10:                              # %entry
	movq	%rbx, -88(%rbp)         # 8-byte Spill
	testq	%rdi, %rdi
	jne	.LBB64_12
# BB#11:                                # %entry
	movq	%rsi, %rax
.LBB64_12:                              # %entry
	xorq	%r14, %r14
	testq	%r15, %r15
	je	.LBB64_40
# BB#13:                                # %entry
	movq	%rdi, -96(%rbp)         # 8-byte Spill
	orq	%rdx, %rcx
	orq	%rax, %rcx
	jne	.LBB64_40
# BB#14:                                # %lor.lhs.false10
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$13, %rax
	jne	.LBB64_40
# BB#15:                                # %if.end14
	xorq	%r14, %r14
	movl	%r14d, -44(%rbp)
	movq	$1, %rax
	movl	%eax, pedantic(%rip)
	movabsq	$.L.str205, %rdi
	callq	get_identifier
	movq	%rax, -104(%rbp)        # 8-byte Spill
	movabsq	$.L.str201, %rdi
	callq	get_identifier
	movq	global_trees+96(%rip), %r13
	movq	%rax, %rbx
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdx, %rdx
	movq	-104(%rbp), %rdi        # 8-byte Reload
	movq	%rax, %rsi
	callq	tree_cons
	leaq	-44(%rbp), %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	check_function_format
	movl	-44(%rbp), %eax
	movl	pedantic(%rip), %ecx
	movl	%ecx, pedantic(%rip)
	testq	%rax, %rax
	jne	.LBB64_40
# BB#16:                                # %while.cond.preheader
	movq	global_trees(%rip), %rdx
	movq	32(%r15), %rsi
	movq	%r12, %r13
	.align	16, 0x90
.LBB64_17:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rax
	movzbl	16(%rax), %ecx
	leaq	-114(%rcx), %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB64_20
# BB#18:                                # %land.lhs.true
                                        #   in Loop: Header=BB64_17 Depth=1
	movq	32(%rax), %rsi
	cmpq	%rdx, %rsi
	je	.LBB64_21
# BB#19:                                # %land.rhs
                                        #   in Loop: Header=BB64_17 Depth=1
	movq	8(%rax), %rdi
	movq	8(%rsi), %rbx
	movl	60(%rdi), %edi
	movl	60(%rbx), %ebx
	xorq	%rdi, %rbx
	testq	$65024, %rbx            # imm = 0xFE00
	je	.LBB64_17
	jmp	.LBB64_21
.LBB64_20:                              # %while.end
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB64_24
.LBB64_21:                              # %land.lhs.true58
	cmpq	$121, %rcx
	jne	.LBB64_22
# BB#23:                                # %if.then64
	movq	32(%rax), %rbx
	jmp	.LBB64_24
.LBB64_22:
	movq	%rax, %rbx
.LBB64_24:                              # %if.end68
	movzbl	16(%rbx), %eax
	cmpq	$29, %rax
	jne	.LBB64_40
# BB#25:                                # %if.end75
	movq	40(%rbx), %r12
	movabsq	$.L.str227, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB64_26
# BB#27:                                # %if.else
	movabsq	$.L.str226, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB64_28
# BB#29:                                # %if.else87
	movq	$37, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB64_40
# BB#30:                                # %if.end93
	movl	32(%rbx), %edx
	movq	$32, %rcx
	movq	%rdx, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$2, %rdx
	jne	.LBB64_32
# BB#31:                                # %if.then96
	movsbq	(%r12), %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	%rax, %rsi
	movq	-88(%rbp), %rdi         # 8-byte Reload
	jmp	.LBB64_36
.LBB64_26:                              # %if.then78
	movq	(%r15), %rsi
.LBB64_35:                              # %if.end135
	movq	-96(%rbp), %rdi         # 8-byte Reload
	jmp	.LBB64_36
.LBB64_28:                              # %if.then84
	movq	(%r15), %rsi
	movq	-88(%rbp), %rdi         # 8-byte Reload
.LBB64_36:                              # %if.end135
	callq	build_function_call
	movq	-72(%rbp), %rcx         # 8-byte Reload
	testq	%r13, %rcx
	jne	.LBB64_37
# BB#38:                                # %if.end135
	movq	-80(%rbp), %rsi         # 8-byte Reload
	jmp	.LBB64_39
.LBB64_37:
	movq	const_int_rtx+512(%rip), %rsi
.LBB64_39:                              # %if.end135
	movq	%rax, %rdi
	movq	-64(%rbp), %rdx         # 8-byte Reload
	movq	-56(%rbp), %rcx         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r14
.LBB64_40:                              # %return
	movq	%r14, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB64_32:                              # %if.else102
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$3, %rax
	jl	.LBB64_40
# BB#33:                                # %land.lhs.true107
	movzbl	-2(%r12,%r15), %eax
	cmpq	$10, %rax
	jne	.LBB64_40
# BB#34:                                # %if.then116
	addq	$-2, %r15
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rdx, %r14
	movq	%rbx, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	memcpy
	xorq	%rax, %rax
	movb	%al, (%r15,%rbx)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	build_string
	movq	%rax, %rdi
	callq	combine_strings
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	build_tree_list
	movq	%rax, %rsi
	jmp	.LBB64_35
.Ltmp479:
	.size	c_expand_builtin_printf, .Ltmp479-c_expand_builtin_printf
	.cfi_endproc

	.align	16, 0x90
	.type	c_expand_builtin_fprintf,@function
c_expand_builtin_fprintf:               # @c_expand_builtin_fprintf
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp486:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp487:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp488:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp489:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp490:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp491:
	.cfi_def_cfa_offset 128
.Ltmp492:
	.cfi_offset %rbx, -48
.Ltmp493:
	.cfi_offset %r12, -40
.Ltmp494:
	.cfi_offset %r13, -32
.Ltmp495:
	.cfi_offset %r14, -24
.Ltmp496:
	.cfi_offset %r15, -16
	movq	%rcx, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r9
	jne	.LBB65_1
# BB#2:                                 # %entry
	movabsq	$built_in_decls+528, %rdx
	jmp	.LBB65_3
.LBB65_1:
	movabsq	$built_in_decls+584, %rdx
.LBB65_3:                               # %entry
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	testq	%r9, %r9
	jne	.LBB65_4
# BB#5:                                 # %entry
	movabsq	$built_in_decls+520, %rax
	jmp	.LBB65_6
.LBB65_4:
	movabsq	$built_in_decls+576, %rax
.LBB65_6:                               # %entry
	movq	(%rax), %r14
	testq	%r13, %r8
	movabsq	$0, %rax
	movabsq	$1, %rsi
	movq	%rax, %rcx
	jne	.LBB65_8
# BB#7:                                 # %entry
	movq	%rsi, %rcx
.LBB65_8:                               # %entry
	movq	%r8, 48(%rsp)           # 8-byte Spill
	movq	(%rdx), %rdi
	testq	%r14, %r14
	movq	%rax, %rdx
	jne	.LBB65_10
# BB#9:                                 # %entry
	movq	%rsi, %rdx
.LBB65_10:                              # %entry
	testq	%rdi, %rdi
	jne	.LBB65_12
# BB#11:                                # %entry
	movq	%rsi, %rax
.LBB65_12:                              # %entry
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB65_38
# BB#13:                                # %entry
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	orq	%rdx, %rcx
	orq	%rax, %rcx
	jne	.LBB65_38
# BB#14:                                # %lor.lhs.false10
	movq	32(%rbx), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$13, %rax
	jne	.LBB65_38
# BB#15:                                # %lor.lhs.false13
	movq	(%rbx), %r12
	testq	%r12, %r12
	je	.LBB65_38
# BB#16:                                # %lor.lhs.false16
	movq	32(%r12), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$13, %rax
	jne	.LBB65_38
# BB#17:                                # %if.end29
	xorq	%r15, %r15
	movl	%r15d, 76(%rsp)
	movq	$1, %rax
	movl	%eax, pedantic(%rip)
	movabsq	$.L.str205, %rdi
	callq	get_identifier
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movabsq	$.L.str201, %rdi
	callq	get_identifier
	movq	global_trees+96(%rip), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdi, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	callq	tree_cons
	xorq	%rdx, %rdx
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	tree_cons
	leaq	76(%rsp), %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	check_function_format
	movl	76(%rsp), %eax
	movl	pedantic(%rip), %ecx
	movl	%ecx, pedantic(%rip)
	testq	%rax, %rax
	jne	.LBB65_38
# BB#18:                                # %if.end34
	movq	(%rbx), %rax
	movq	32(%rax), %r9
	movq	global_trees(%rip), %r8
	movq	%r9, %rsi
	.align	16, 0x90
.LBB65_19:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rax
	movzbl	16(%rax), %ecx
	leaq	-114(%rcx), %rsi
	andq	%r13, %rsi
	cmpq	$2, %rsi
	ja	.LBB65_22
# BB#20:                                # %land.lhs.true
                                        #   in Loop: Header=BB65_19 Depth=1
	movq	32(%rax), %rsi
	cmpq	%r8, %rsi
	je	.LBB65_23
# BB#21:                                # %land.rhs
                                        #   in Loop: Header=BB65_19 Depth=1
	movq	8(%rax), %rdi
	movq	8(%rsi), %rdx
	movl	60(%rdi), %edi
	movl	60(%rdx), %edx
	xorq	%rdi, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB65_19
	jmp	.LBB65_23
.LBB65_22:                              # %while.end
	xorq	%rdx, %rdx
	testq	%rax, %rax
	je	.LBB65_26
.LBB65_23:                              # %land.lhs.true77
	cmpq	$121, %rcx
	jne	.LBB65_24
# BB#25:                                # %if.then83
	movq	32(%rax), %rdx
	jmp	.LBB65_26
.LBB65_24:
	movq	%rax, %rdx
.LBB65_26:                              # %if.end87
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movzbl	16(%rdx), %eax
	cmpq	$29, %rax
	jne	.LBB65_38
# BB#27:                                # %if.end94
	movq	40(%rdx), %r12
	movabsq	$.L.str225, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB65_28
# BB#29:                                # %if.else
	movabsq	$.L.str226, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB65_30
# BB#31:                                # %if.else124
	movq	$37, %rsi
	movq	%r12, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB65_38
# BB#32:                                # %if.end130
	movq	32(%rbx), %rsi
	xorq	%rdi, %rdi
	callq	build_tree_list
	xorq	%rdi, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB65_33
.LBB65_28:                              # %if.then97
	movq	32(%rbx), %rsi
	xorq	%rdi, %rdi
	callq	build_tree_list
	movq	(%rbx), %rcx
	movq	(%rcx), %rcx
	movq	32(%rcx), %rsi
	xorq	%rdi, %rdi
.LBB65_33:                              # %if.end140
	movq	%rax, %rdx
	callq	tree_cons
	movq	32(%rsp), %rdi          # 8-byte Reload
	jmp	.LBB65_34
.LBB65_30:                              # %if.then112
	movq	32(%rbx), %rsi
	xorq	%rdi, %rdi
	callq	build_tree_list
	movq	(%rbx), %rcx
	movq	(%rcx), %rcx
	movq	32(%rcx), %rsi
	xorq	%rdi, %rdi
	movq	%rax, %rdx
	callq	tree_cons
	movq	%r14, %rdi
.LBB65_34:                              # %if.end140
	movq	%rax, %rsi
	callq	build_function_call
	movq	48(%rsp), %rcx          # 8-byte Reload
	testq	%r13, %rcx
	jne	.LBB65_35
# BB#36:                                # %if.end140
	movq	40(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB65_37
.LBB65_35:
	movq	const_int_rtx+512(%rip), %rsi
.LBB65_37:                              # %if.end140
	movq	%rax, %rdi
	movq	56(%rsp), %rdx          # 8-byte Reload
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r15
.LBB65_38:                              # %return
	movq	%r15, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp497:
	.size	c_expand_builtin_fprintf, .Ltmp497-c_expand_builtin_fprintf
	.cfi_endproc

	.align	16, 0x90
	.type	verify_tree,@function
verify_tree:                            # @verify_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp504:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp505:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp506:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp507:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp508:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp509:
	.cfi_def_cfa_offset 144
.Ltmp510:
	.cfi_offset %rbx, -48
.Ltmp511:
	.cfi_offset %r12, -40
.Ltmp512:
	.cfi_offset %r13, -32
.Ltmp513:
	.cfi_offset %r14, -24
.Ltmp514:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB66_525
# BB#1:                                 # %restart.preheader
	movl	16(%r13), %edi
	movq	%rdi, %rax
	andq	$254, %rax
	cmpq	$34, %rax
	je	.LBB66_9
# BB#2:
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
.LBB66_3:                               # %if.end4
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdi
	movzbl	tree_code_type(%rdi), %eax
	cmpq	$45, %rdi
	jg	.LBB66_16
# BB#4:                                 # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	cmpq	$2, %rdi
	jne	.LBB66_493
	jmp	.LBB66_5
	.align	16, 0x90
.LBB66_16:                              # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	cmpq	$90, %rdi
	jg	.LBB66_19
# BB#17:                                # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	leaq	-46(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$7, %rcx
	jbe	.LBB66_18
.LBB66_493:                             # %sw.epilog
                                        #   in Loop: Header=BB66_3 Depth=1
	movq	%r9, %rbx
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$49, %rcx
	jne	.LBB66_496
# BB#494:                               # %if.then127
                                        #   in Loop: Header=BB66_3 Depth=1
	movq	%rsi, %r14
	callq	first_rtl_op
	movq	%r14, %rsi
	movq	%rbx, %r9
	testq	%r9, %rax
	je	.LBB66_525
# BB#495:                               # %if.end132
                                        #   in Loop: Header=BB66_3 Depth=1
	movq	32(%r13), %r13
	movl	16(%r13), %edi
	movq	%rdi, %rax
	andq	$254, %rax
	xorq	%rbx, %rbx
	cmpq	$34, %rax
	jne	.LBB66_3
	jmp	.LBB66_9
	.align	16, 0x90
.LBB66_19:                              # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	leaq	-129(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$4, %rcx
	jb	.LBB66_273
# BB#20:                                # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	leaq	-91(%rdi), %rcx
	andq	%r9, %rcx
	cmpq	$2, %rcx
	jb	.LBB66_50
# BB#21:                                # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	cmpq	$118, %rdi
	jne	.LBB66_493
	jmp	.LBB66_22
.LBB66_18:                              # %if.end4
                                        #   in Loop: Header=BB66_3 Depth=1
	jmpq	*.LJTI66_0(,%rcx,8)
.LBB66_50:                              # %sw.bb5
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%r13), %rdi
	movq	%r13, 32(%rsp)          # 8-byte Spill
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	verify_tree
	movq	80(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_68
# BB#51:
	movq	40(%rsp), %rax          # 8-byte Reload
	.align	16, 0x90
.LBB66_52:                              # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_54 Depth 2
                                        #     Child Loop BB66_57 Depth 2
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	16(%rax), %r14
	testq	%r14, %r14
	je	.LBB66_67
# BB#53:                                # %if.then.i249
                                        #   in Loop: Header=BB66_52 Depth=1
	movq	warned_ids(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r15
	testq	%rax, %rax
	movq	40(%rsp), %r13          # 8-byte Reload
	je	.LBB66_57
	.align	16, 0x90
.LBB66_54:                              # %for.body.i.i
                                        #   Parent Loop BB66_52 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	cmpq	%r15, %rcx
	je	.LBB66_67
# BB#55:                                # %for.cond.i.i
                                        #   in Loop: Header=BB66_54 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_54
# BB#56:                                #   in Loop: Header=BB66_52 Depth=1
	movq	40(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB66_57:                              # %while.body.i.i
                                        #   Parent Loop BB66_52 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r13), %rax
	cmpq	%r15, %rax
	jne	.LBB66_66
# BB#58:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	16(%r13), %rax
	cmpq	%r14, %rax
	je	.LBB66_66
# BB#59:                                # %if.then10.i.i
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %r12
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_61
# BB#60:                                # %if.then.i256
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_61:                              # %if.end.i259
                                        #   in Loop: Header=BB66_57 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_63
# BB#62:                                # %if.then8.i262
                                        #   in Loop: Header=BB66_57 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_63:                              # %if.end9.i276
                                        #   in Loop: Header=BB66_57 Depth=2
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
	jle	.LBB66_65
# BB#64:                                # %if.then29.i277
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB66_65:                              # %new_tlist.exit281
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r12, (%rax)
	movq	%r15, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, warned_ids(%rip)
	movq	8(%r13), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str253, %rdi
	callq	warning
.LBB66_66:                              # %if.end12.i.i
                                        #   in Loop: Header=BB66_57 Depth=2
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB66_57
.LBB66_67:                              # %for.inc.i
                                        #   in Loop: Header=BB66_52 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_52
.LBB66_68:                              # %warn_for_collisions.exit
	movq	88(%rsp), %r13
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%r10, %rax
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB66_69:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB66_69
	jmp	.LBB66_86
.LBB66_9:                               # %if.then2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%r15), %r14
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_11
# BB#10:                                # %if.then.i
	movabsq	$tlist_obstack, %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_11:                              # %if.end.i
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_13
# BB#12:                                # %if.then8.i
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_13:                              # %if.end9.i
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_15
# BB#14:                                # %if.then29.i
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
.LBB66_15:                              # %new_tlist.exit
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r14, (%rax)
	movq	%r13, 8(%rax)
	movq	%rbx, 16(%rax)
	movq	%rax, (%r15)
	jmp	.LBB66_525
.LBB66_273:                             # %sw.bb20
	movq	32(%r13), %rdi
	movq	%r15, %rsi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	verify_tree
	jmp	.LBB66_525
.LBB66_85:                              # %new_tlist.exit321
                                        #   in Loop: Header=BB66_86 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r11, 16(%rax)
	movq	%rax, (%r14)
	movq	$0, (%rax)
	movq	%rax, %r14
.LBB66_86:                              # %while.cond1.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_70 Depth 2
                                        #       Child Loop BB66_74 Depth 3
	movq	%r13, %rdx
	.align	16, 0x90
.LBB66_70:                              # %while.cond1.i
                                        #   Parent Loop BB66_86 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_74 Depth 3
	testq	%rdx, %rdx
	je	.LBB66_87
# BB#71:                                # %while.body3.i
                                        #   in Loop: Header=BB66_70 Depth=2
	movq	(%r10), %rcx
	movq	(%rdx), %r13
	testq	%rcx, %rcx
	je	.LBB66_72
# BB#73:                                # %for.body.lr.ph.i
                                        #   in Loop: Header=BB66_70 Depth=2
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_74:                              # %for.body.i287
                                        #   Parent Loop BB66_86 Depth=1
                                        #     Parent Loop BB66_70 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_77
# BB#75:                                # %if.then.i289
                                        #   in Loop: Header=BB66_74 Depth=3
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_77
# BB#76:                                # %if.then9.i
                                        #   in Loop: Header=BB66_74 Depth=3
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_77:                              # %for.inc.i290
                                        #   in Loop: Header=BB66_74 Depth=3
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_74
# BB#78:                                # %for.end.i
                                        #   in Loop: Header=BB66_70 Depth=2
	testq	%r9, %rdx
	movq	%r13, %rdx
	jne	.LBB66_70
	jmp	.LBB66_79
.LBB66_72:                              # %while.body3.i.if.then15.i_crit_edge
                                        #   in Loop: Header=BB66_86 Depth=1
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_79:                              # %if.then15.i
                                        #   in Loop: Header=BB66_86 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r11
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_81
# BB#80:                                # %if.then.i296
                                        #   in Loop: Header=BB66_86 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r12
	movq	%r11, %r15
	callq	_obstack_newchunk
	movq	%r15, %r11
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%r12, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_81:                              # %if.end.i299
                                        #   in Loop: Header=BB66_86 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_83
# BB#82:                                # %if.then8.i302
                                        #   in Loop: Header=BB66_86 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_83:                              # %if.end9.i316
                                        #   in Loop: Header=BB66_86 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_85
# BB#84:                                # %if.then29.i317
                                        #   in Loop: Header=BB66_86 Depth=1
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_85
.LBB66_87:                              # %merge_tlist.exit
	movq	80(%rsp), %r13
	movq	%r10, %rax
	.align	16, 0x90
.LBB66_88:                              # %while.cond.i325
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB66_88
	jmp	.LBB66_105
.LBB66_104:                             # %new_tlist.exit392
                                        #   in Loop: Header=BB66_105 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r11, 16(%rax)
	movq	%rax, (%r14)
	movq	$0, (%rax)
	movq	%rax, %r14
.LBB66_105:                             # %while.cond1.outer.i360
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_89 Depth 2
                                        #       Child Loop BB66_93 Depth 3
	movq	%r13, %rdx
	.align	16, 0x90
.LBB66_89:                              # %while.cond1.i329
                                        #   Parent Loop BB66_105 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_93 Depth 3
	testq	%rdx, %rdx
	je	.LBB66_106
# BB#90:                                # %while.body3.i333
                                        #   in Loop: Header=BB66_89 Depth=2
	movq	(%r10), %rcx
	movq	(%rdx), %r13
	testq	%rcx, %rcx
	je	.LBB66_91
# BB#92:                                # %for.body.lr.ph.i337
                                        #   in Loop: Header=BB66_89 Depth=2
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_93:                              # %for.body.i342
                                        #   Parent Loop BB66_105 Depth=1
                                        #     Parent Loop BB66_89 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_96
# BB#94:                                # %if.then.i345
                                        #   in Loop: Header=BB66_93 Depth=3
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_96
# BB#95:                                # %if.then9.i346
                                        #   in Loop: Header=BB66_93 Depth=3
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_96:                              # %for.inc.i351
                                        #   in Loop: Header=BB66_93 Depth=3
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_93
# BB#97:                                # %for.end.i353
                                        #   in Loop: Header=BB66_89 Depth=2
	testq	%r9, %rdx
	movq	%r13, %rdx
	jne	.LBB66_89
	jmp	.LBB66_98
.LBB66_91:                              # %while.body3.i333.if.then15.i354_crit_edge
                                        #   in Loop: Header=BB66_105 Depth=1
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_98:                              # %if.then15.i354
                                        #   in Loop: Header=BB66_105 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r11
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_100
# BB#99:                                # %if.then.i367
                                        #   in Loop: Header=BB66_105 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r12
	movq	%r11, %r15
	callq	_obstack_newchunk
	movq	%r15, %r11
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%r12, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_100:                             # %if.end.i370
                                        #   in Loop: Header=BB66_105 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_102
# BB#101:                               # %if.then8.i373
                                        #   in Loop: Header=BB66_105 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_102:                             # %if.end9.i387
                                        #   in Loop: Header=BB66_105 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_104
# BB#103:                               # %if.then29.i388
                                        #   in Loop: Header=BB66_105 Depth=1
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_104
.LBB66_106:                             # %merge_tlist.exit361
	movq	%r9, %r15
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rdi
	leaq	64(%rsp), %rsi
	xorq	%rcx, %rcx
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r10, %rbx
	callq	verify_tree
	movq	%rbx, %r10
	movq	64(%rsp), %r12
	movq	%r10, %rax
	.align	16, 0x90
.LBB66_107:                             # %while.cond.i467
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB66_107
# BB#108:
	movq	%r15, %r9
	movq	%r12, %rdx
	jmp	.LBB66_109
.LBB66_124:                             # %new_tlist.exit534
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r11, 16(%rax)
	movq	%rax, (%r14)
	movq	$0, (%rax)
	movq	%rax, %r14
	movq	%r12, %rdx
	.align	16, 0x90
.LBB66_109:                             # %while.cond1.i471
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_113 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_525
# BB#110:                               # %while.body3.i475
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	(%r10), %rcx
	movq	(%rdx), %r12
	testq	%rcx, %rcx
	je	.LBB66_111
# BB#112:                               # %for.body.lr.ph.i479
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_113:                             # %for.body.i484
                                        #   Parent Loop BB66_109 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_116
# BB#114:                               # %if.then.i487
                                        #   in Loop: Header=BB66_113 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_116
# BB#115:                               # %if.then9.i488
                                        #   in Loop: Header=BB66_113 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_116:                             # %for.inc.i493
                                        #   in Loop: Header=BB66_113 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_113
# BB#117:                               # %for.end.i495
                                        #   in Loop: Header=BB66_109 Depth=1
	testq	%r9, %rdx
	movq	%r12, %rdx
	jne	.LBB66_109
	jmp	.LBB66_118
.LBB66_111:                             # %while.body3.i475.if.then15.i496_crit_edge
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_118:                             # %if.then15.i496
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r11
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_120
# BB#119:                               # %if.then.i509
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r15
	movq	%r11, %r13
	callq	_obstack_newchunk
	movq	%r13, %r11
	movq	24(%rsp), %r10          # 8-byte Reload
	movq	%r15, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_120:                             # %if.end.i512
                                        #   in Loop: Header=BB66_109 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_122
# BB#121:                               # %if.then8.i515
                                        #   in Loop: Header=BB66_109 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_122:                             # %if.end9.i529
                                        #   in Loop: Header=BB66_109 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_124
# BB#123:                               # %if.then29.i530
                                        #   in Loop: Header=BB66_109 Depth=1
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_124
.LBB66_496:                             # %if.end136
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$59, %rax
	jg	.LBB66_498
# BB#497:                               # %if.end136
	andq	%rbx, %rax
	cmpq	$50, %rax
	je	.LBB66_500
	jmp	.LBB66_525
.LBB66_498:                             # %if.end136
	addq	$-60, %rax
	andq	%rbx, %rax
	cmpq	$60, %rax
	ja	.LBB66_525
# BB#499:                               # %if.end136
	movq	$1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	movabsq	$1206967174036455425, %rax # imm = 0x10C0024000000001
	testq	%rax, %rdx
	je	.LBB66_525
.LBB66_500:                             # %sw.bb138
	callq	first_rtl_op
	movq	%rbx, %r9
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB66_525
# BB#501:                               # %for.body149.lr.ph
	xorq	%rdx, %rdx
	andq	%r9, 40(%rsp)           # 8-byte Folded Spill
	.align	16, 0x90
.LBB66_502:                             # %for.body149
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_503 Depth 2
                                        #     Child Loop BB66_505 Depth 2
                                        #       Child Loop BB66_509 Depth 3
                                        #     Child Loop BB66_523 Depth 2
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r15
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%r13,%rdx,8), %rdi
	xorq	%rcx, %rcx
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rbx
	movq	%rbx, %rdx
	callq	verify_tree
	movq	88(%rsp), %r14
	movq	%rbx, %rax
	.align	16, 0x90
.LBB66_503:                             # %while.cond.i396
                                        #   Parent Loop BB66_502 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r13
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB66_503
# BB#504:                               #   in Loop: Header=BB66_502 Depth=1
	movq	%r15, %r9
	movq	%r14, %rdx
	jmp	.LBB66_505
.LBB66_520:                             # %new_tlist.exit463
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%rax, (%r13)
	movq	$0, (%rax)
	movq	%rax, %r13
	movq	%r14, %rdx
	.align	16, 0x90
.LBB66_505:                             # %while.cond1.i400
                                        #   Parent Loop BB66_502 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_509 Depth 3
	testq	%rdx, %rdx
	je	.LBB66_522
# BB#506:                               # %while.body3.i404
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	80(%rsp), %rcx
	movq	(%rdx), %r14
	testq	%rcx, %rcx
	je	.LBB66_507
# BB#508:                               # %for.body.lr.ph.i408
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_509:                             # %for.body.i413
                                        #   Parent Loop BB66_502 Depth=1
                                        #     Parent Loop BB66_505 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_512
# BB#510:                               # %if.then.i416
                                        #   in Loop: Header=BB66_509 Depth=3
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_512
# BB#511:                               # %if.then9.i417
                                        #   in Loop: Header=BB66_509 Depth=3
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_512:                             # %for.inc.i422
                                        #   in Loop: Header=BB66_509 Depth=3
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_509
# BB#513:                               # %for.end.i424
                                        #   in Loop: Header=BB66_505 Depth=2
	testq	%r9, %rdx
	movq	%r14, %rdx
	jne	.LBB66_505
	jmp	.LBB66_514
.LBB66_507:                             # %while.body3.i404.if.then15.i425_crit_edge
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_514:                             # %if.then15.i425
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r12
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_516
# BB#515:                               # %if.then.i438
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r15
	callq	_obstack_newchunk
	movq	%r15, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_516:                             # %if.end.i441
                                        #   in Loop: Header=BB66_505 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_518
# BB#517:                               # %if.then8.i444
                                        #   in Loop: Header=BB66_505 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_518:                             # %if.end9.i458
                                        #   in Loop: Header=BB66_505 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_520
# BB#519:                               # %if.then29.i459
                                        #   in Loop: Header=BB66_505 Depth=2
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_520
	.align	16, 0x90
.LBB66_522:                             # %merge_tlist.exit432
                                        #   in Loop: Header=BB66_502 Depth=1
	movq	80(%rsp), %rax
	testq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB66_524
	.align	16, 0x90
.LBB66_523:                             # %while.body.i
                                        #   Parent Loop BB66_502 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rcx
	movq	(%rsi), %rdx
	testq	%rcx, %rcx
	movq	%rdx, (%rax)
	movq	%rax, (%rsi)
	movq	%rcx, %rax
	jne	.LBB66_523
.LBB66_524:                             # %add_tlist.exit
                                        #   in Loop: Header=BB66_502 Depth=1
	movq	56(%rsp), %rdx          # 8-byte Reload
	incq	%rdx
	movq	%rdx, %rax
	andq	%r9, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	32(%rsp), %r13          # 8-byte Reload
	jne	.LBB66_502
	jmp	.LBB66_525
.LBB66_446:                             # %add_tlist.exit1258
                                        #   in Loop: Header=BB66_5 Depth=1
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	(%r13), %r13
.LBB66_5:                               # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_7 Depth 2
                                        #     Child Loop BB66_427 Depth 2
                                        #       Child Loop BB66_431 Depth 3
                                        #     Child Loop BB66_445 Depth 2
	testq	%r13, %r13
	je	.LBB66_525
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB66_5 Depth=1
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	%r9, %r15
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%r13), %rdi
	xorq	%rcx, %rcx
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rbx
	movq	%rbx, %rdx
	callq	verify_tree
	movq	88(%rsp), %r14
	movq	%rbx, %rax
	.align	16, 0x90
.LBB66_7:                               # %while.cond.i1262
                                        #   Parent Loop BB66_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, %r13
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB66_7
# BB#8:                                 #   in Loop: Header=BB66_5 Depth=1
	movq	%r15, %r9
	movq	%r14, %rdx
	jmp	.LBB66_427
.LBB66_442:                             # %new_tlist.exit1329
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r12, 16(%rax)
	movq	%rax, (%r13)
	movq	$0, (%rax)
	movq	%rax, %r13
	movq	%r14, %rdx
	.align	16, 0x90
.LBB66_427:                             # %while.cond1.i1266
                                        #   Parent Loop BB66_5 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_431 Depth 3
	testq	%rdx, %rdx
	je	.LBB66_444
# BB#428:                               # %while.body3.i1270
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	80(%rsp), %rcx
	movq	(%rdx), %r14
	testq	%rcx, %rcx
	je	.LBB66_429
# BB#430:                               # %for.body.lr.ph.i1274
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB66_431:                             # %for.body.i1279
                                        #   Parent Loop BB66_5 Depth=1
                                        #     Parent Loop BB66_427 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_434
# BB#432:                               # %if.then.i1282
                                        #   in Loop: Header=BB66_431 Depth=3
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_434
# BB#433:                               # %if.then9.i1283
                                        #   in Loop: Header=BB66_431 Depth=3
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_434:                             # %for.inc.i1288
                                        #   in Loop: Header=BB66_431 Depth=3
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_431
# BB#435:                               # %for.end.i1290
                                        #   in Loop: Header=BB66_427 Depth=2
	testq	%r9, %rdx
	movq	%r14, %rdx
	jne	.LBB66_427
	jmp	.LBB66_436
.LBB66_429:                             # %while.body3.i1270.if.then15.i1291_crit_edge
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_436:                             # %if.then15.i1291
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r12
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_438
# BB#437:                               # %if.then.i1304
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r15
	callq	_obstack_newchunk
	movq	%r15, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_438:                             # %if.end.i1307
                                        #   in Loop: Header=BB66_427 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_440
# BB#439:                               # %if.then8.i1310
                                        #   in Loop: Header=BB66_427 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_440:                             # %if.end9.i1324
                                        #   in Loop: Header=BB66_427 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_442
# BB#441:                               # %if.then29.i1325
                                        #   in Loop: Header=BB66_427 Depth=2
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_442
.LBB66_444:                             # %merge_tlist.exit1298
                                        #   in Loop: Header=BB66_5 Depth=1
	movq	80(%rsp), %rax
	testq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB66_446
.LBB66_445:                             # %while.body.i1254
                                        #   Parent Loop BB66_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rax), %rcx
	movq	(%rsi), %rdx
	testq	%rcx, %rcx
	movq	%rdx, (%rax)
	movq	%rax, (%rsi)
	movq	%rcx, %rax
	jne	.LBB66_445
	jmp	.LBB66_446
.LBB66_22:                              # %for.cond.preheader
	movq	save_expr_cache(%rip), %r14
	jmp	.LBB66_23
.LBB66_447:                             # %for.cond
                                        #   in Loop: Header=BB66_23 Depth=1
	movq	(%r14), %r14
.LBB66_23:                              # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r14, %r14
	je	.LBB66_24
# BB#448:                               # %for.body
                                        #   in Loop: Header=BB66_23 Depth=1
	movq	24(%r14), %rax
	cmpq	%r13, %rax
	jne	.LBB66_447
# BB#449:
	movq	%r9, %rbx
	jmp	.LBB66_472
.LBB66_24:                              # %if.then67
	movq	%rsi, %r12
	movq	%r9, %rbx
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$31, %rcx
	jg	.LBB66_26
# BB#25:                                # %if.then70
	movabsq	$tlist_obstack, %rdi
	movq	$32, %rsi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rax
.LBB66_26:                              # %if.end71
	addq	$32, %rax
	movq	%rax, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r14
	cmpq	%r14, %rax
	jne	.LBB66_28
# BB#27:                                # %if.then79
	movzbl	tlist_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, tlist_obstack+80(%rip)
.LBB66_28:                              # %if.end82
	movslq	tlist_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB66_30
# BB#29:                                # %if.then102
	movq	%rax, tlist_obstack+24(%rip)
	movq	%rax, %rcx
.LBB66_30:                              # %if.end105
	movq	%rcx, tlist_obstack+16(%rip)
	movq	save_expr_cache(%rip), %rax
	movq	%rax, (%r14)
	movq	%r13, %rax
	movq	%rax, 24(%r14)
	movq	%r14, save_expr_cache(%rip)
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%rax), %rax
	leaq	88(%rsp), %rdx
	leaq	80(%rsp), %r8
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	callq	verify_tree
	movq	80(%rsp), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_470
# BB#31:
	movabsq	$.L.str253, %r13
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB66_32:                              # %for.body.i1188
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_34 Depth 2
                                        #     Child Loop BB66_37 Depth 2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_47
# BB#33:                                # %if.then.i1192
                                        #   in Loop: Header=BB66_32 Depth=1
	movq	warned_ids(%rip), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	je	.LBB66_37
.LBB66_34:                              # %for.body.i.i1200
                                        #   Parent Loop BB66_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	je	.LBB66_47
# BB#35:                                # %for.cond.i.i1196
                                        #   in Loop: Header=BB66_34 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_34
# BB#36:                                #   in Loop: Header=BB66_32 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB66_37:                              # %while.body.i.i1204
                                        #   Parent Loop BB66_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB66_46
# BB#38:                                # %land.lhs.true.i.i1207
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	16(%rax), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB66_46
# BB#39:                                # %if.then10.i.i1210
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %rdx
	movq	%rdx, (%rsp)            # 8-byte Spill
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_41
# BB#40:                                # %if.then.i1224
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rax
	movq	%rax, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_41:                              # %if.end.i1227
                                        #   in Loop: Header=BB66_37 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_43
# BB#42:                                # %if.then8.i1230
                                        #   in Loop: Header=BB66_37 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_43:                              # %if.end9.i1244
                                        #   in Loop: Header=BB66_37 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_45
# BB#44:                                # %if.then29.i1245
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_45:                              # %new_tlist.exit1249
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, (%r8)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r8)
	movq	$0, 16(%r8)
	movq	%r8, warned_ids(%rip)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	callq	warning
.LBB66_46:                              # %if.end12.i.i1213
                                        #   in Loop: Header=BB66_37 Depth=2
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_37
.LBB66_47:                              # %for.inc.i1216
                                        #   in Loop: Header=BB66_32 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_32
# BB#48:                                # %warn_for_collisions.exit1218
	movq	80(%rsp), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$0, 64(%rsp)
	je	.LBB66_471
# BB#49:
	movabsq	$tlist_obstack, %r13
.LBB66_451:                             # %while.body116
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_452 Depth 2
                                        #     Child Loop BB66_469 Depth 2
                                        #       Child Loop BB66_453 Depth 3
                                        #         Child Loop BB66_457 Depth 4
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 80(%rsp)
	leaq	64(%rsp), %rax
.LBB66_452:                             # %while.cond.i899
                                        #   Parent Loop BB66_451 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_452
	jmp	.LBB66_469
.LBB66_468:                             # %new_tlist.exit966
                                        #   in Loop: Header=BB66_469 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r8)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r8)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%r8, (%rax)
	movq	$0, (%r8)
	movq	%r8, 40(%rsp)           # 8-byte Spill
.LBB66_469:                             # %while.cond1.outer.i934
                                        #   Parent Loop BB66_451 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_453 Depth 3
                                        #         Child Loop BB66_457 Depth 4
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB66_453:                             # %while.cond1.i903
                                        #   Parent Loop BB66_451 Depth=1
                                        #     Parent Loop BB66_469 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB66_457 Depth 4
	testq	%rdx, %rdx
	je	.LBB66_450
# BB#454:                               # %while.body3.i907
                                        #   in Loop: Header=BB66_453 Depth=3
	movq	64(%rsp), %rcx
	movq	(%rdx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB66_455
# BB#456:                               # %for.body.lr.ph.i911
                                        #   in Loop: Header=BB66_453 Depth=3
	movq	8(%rdx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_457:                             # %for.body.i916
                                        #   Parent Loop BB66_451 Depth=1
                                        #     Parent Loop BB66_469 Depth=2
                                        #       Parent Loop BB66_453 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	8(%rcx), %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	cmpq	%rdi, %rsi
	jne	.LBB66_460
# BB#458:                               # %if.then.i919
                                        #   in Loop: Header=BB66_457 Depth=4
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_460
# BB#459:                               # %if.then9.i920
                                        #   in Loop: Header=BB66_457 Depth=4
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_460:                             # %for.inc.i925
                                        #   in Loop: Header=BB66_457 Depth=4
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_457
# BB#461:                               # %for.end.i927
                                        #   in Loop: Header=BB66_453 Depth=3
	testq	%rbx, %rdx
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB66_453
	jmp	.LBB66_462
.LBB66_455:                             # %while.body3.i907.if.then15.i928_crit_edge
                                        #   in Loop: Header=BB66_469 Depth=2
	movq	8(%rdx), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_462:                             # %if.then15.i928
                                        #   in Loop: Header=BB66_469 Depth=2
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_464
# BB#463:                               # %if.then.i941
                                        #   in Loop: Header=BB66_469 Depth=2
	movq	$24, %rsi
	movq	%r13, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_464:                             # %if.end.i944
                                        #   in Loop: Header=BB66_469 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_466
# BB#465:                               # %if.then8.i947
                                        #   in Loop: Header=BB66_469 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_466:                             # %if.end9.i961
                                        #   in Loop: Header=BB66_469 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_468
# BB#467:                               # %if.then29.i962
                                        #   in Loop: Header=BB66_469 Depth=2
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_468
.LBB66_450:                             # %while.cond114.loopexit
                                        #   in Loop: Header=BB66_451 Depth=1
	movq	80(%rsp), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_451
	jmp	.LBB66_471
.LBB66_274:                             # %sw.bb24
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$0, 64(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	40(%r13), %rdi
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	callq	verify_tree
	movq	32(%r13), %rdi
	leaq	64(%rsp), %rsi
	movq	%rsi, %rdx
	movq	%r13, %rcx
	callq	verify_tree
	movq	64(%rsp), %rax
	testq	%rax, %rax
	je	.LBB66_277
.LBB66_275:                             # %while.body.i1334
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rax), %rcx
	movq	(%rax), %r14
	cmpq	%r13, %rcx
	je	.LBB66_276
# BB#299:                               # %if.then5.i
                                        #   in Loop: Header=BB66_275 Depth=1
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	88(%rsp), %r15
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	8(%rax), %r12
	movq	16(%rax), %r13
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_301
# BB#300:                               # %if.then.i.i1345
                                        #   in Loop: Header=BB66_275 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_301:                             # %if.end.i.i1348
                                        #   in Loop: Header=BB66_275 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_303
# BB#302:                               # %if.then8.i.i1351
                                        #   in Loop: Header=BB66_275 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_303:                             # %if.end9.i.i1365
                                        #   in Loop: Header=BB66_275 Depth=1
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
	jle	.LBB66_305
# BB#304:                               # %if.then29.i.i1366
                                        #   in Loop: Header=BB66_275 Depth=1
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB66_305:                             # %new_tlist.exit.i1369
                                        #   in Loop: Header=BB66_275 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, (%rax)
	movq	%r13, 16(%rax)
	movq	%r12, 8(%rax)
	movq	%rax, 88(%rsp)
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	32(%rsp), %r13          # 8-byte Reload
.LBB66_276:                             # %while.cond.backedge.i
                                        #   in Loop: Header=BB66_275 Depth=1
	testq	%r14, %r14
	movq	%r14, %rax
	jne	.LBB66_275
.LBB66_277:                             # %add_tlist.exit1370
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	%r15, 48(%rsp)          # 8-byte Spill
	movq	88(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_295
# BB#278:
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB66_279:                             # %for.body.i1375
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_281 Depth 2
                                        #     Child Loop BB66_284 Depth 2
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	16(%rax), %r12
	testq	%r12, %r12
	je	.LBB66_294
# BB#280:                               # %if.then.i1379
                                        #   in Loop: Header=BB66_279 Depth=1
	movq	warned_ids(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r13
	testq	%rax, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB66_284
.LBB66_281:                             # %for.body.i.i1387
                                        #   Parent Loop BB66_279 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	cmpq	%r13, %rcx
	je	.LBB66_294
# BB#282:                               # %for.cond.i.i1383
                                        #   in Loop: Header=BB66_281 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_281
# BB#283:                               #   in Loop: Header=BB66_279 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
.LBB66_284:                             # %while.body.i.i1391
                                        #   Parent Loop BB66_279 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r14), %rax
	cmpq	%r13, %rax
	jne	.LBB66_293
# BB#285:                               # %land.lhs.true.i.i1394
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	16(%r14), %rax
	cmpq	%r12, %rax
	je	.LBB66_293
# BB#286:                               # %if.then10.i.i1397
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %r15
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_288
# BB#287:                               # %if.then.i1411
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_288:                             # %if.end.i1414
                                        #   in Loop: Header=BB66_284 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_290
# BB#289:                               # %if.then8.i1417
                                        #   in Loop: Header=BB66_284 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_290:                             # %if.end9.i1431
                                        #   in Loop: Header=BB66_284 Depth=2
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
	jle	.LBB66_292
# BB#291:                               # %if.then29.i1432
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB66_292:                             # %new_tlist.exit1436
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, (%rax)
	movq	%r13, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, warned_ids(%rip)
	movq	8(%r14), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str253, %rdi
	callq	warning
.LBB66_293:                             # %if.end12.i.i1400
                                        #   in Loop: Header=BB66_284 Depth=2
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB66_284
.LBB66_294:                             # %for.inc.i1403
                                        #   in Loop: Header=BB66_279 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_279
.LBB66_295:                             # %warn_for_collisions.exit1405
	movq	64(%rsp), %rax
	testq	%rax, %rax
	je	.LBB66_309
# BB#296:                               # %while.body.lr.ph.i1439
	cmpq	$0, 32(%rsp)            # 8-byte Folded Reload
	je	.LBB66_297
# BB#306:
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB66_307:                             # %lor.lhs.false.i1447
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rdx
	movq	(%rax), %rcx
	movq	%rdx, (%rax)
	movq	16(%rax), %rdx
	cmpq	%rsi, %rdx
	je	.LBB66_308
# BB#526:                               # %if.then5.i1450
                                        #   in Loop: Header=BB66_307 Depth=1
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%rax, (%rdx)
.LBB66_308:                             # %while.cond.backedge.i1449
                                        #   in Loop: Header=BB66_307 Depth=1
	testq	%rcx, %rcx
	movq	%rcx, %rax
	jne	.LBB66_307
	jmp	.LBB66_309
.LBB66_126:                             # %sw.bb10
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	$0, 72(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%r13), %rdi
	movq	%r13, 32(%rsp)          # 8-byte Spill
	leaq	88(%rsp), %rsi
	leaq	72(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	verify_tree
	movq	72(%rsp), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_144
# BB#127:
	movq	40(%rsp), %rax          # 8-byte Reload
.LBB66_128:                             # %for.body.i550
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_130 Depth 2
                                        #     Child Loop BB66_133 Depth 2
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	16(%rax), %r12
	testq	%r12, %r12
	je	.LBB66_143
# BB#129:                               # %if.then.i554
                                        #   in Loop: Header=BB66_128 Depth=1
	movq	warned_ids(%rip), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	movq	8(%rcx), %r13
	testq	%rax, %rax
	movq	40(%rsp), %r14          # 8-byte Reload
	je	.LBB66_133
.LBB66_130:                             # %for.body.i.i562
                                        #   Parent Loop BB66_128 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	cmpq	%r13, %rcx
	je	.LBB66_143
# BB#131:                               # %for.cond.i.i558
                                        #   in Loop: Header=BB66_130 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_130
# BB#132:                               #   in Loop: Header=BB66_128 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
.LBB66_133:                             # %while.body.i.i566
                                        #   Parent Loop BB66_128 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%r14), %rax
	cmpq	%r13, %rax
	jne	.LBB66_142
# BB#134:                               # %land.lhs.true.i.i569
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	16(%r14), %rax
	cmpq	%r12, %rax
	je	.LBB66_142
# BB#135:                               # %if.then10.i.i572
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %r15
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_137
# BB#136:                               # %if.then.i586
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_137:                             # %if.end.i589
                                        #   in Loop: Header=BB66_133 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_139
# BB#138:                               # %if.then8.i592
                                        #   in Loop: Header=BB66_133 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_139:                             # %if.end9.i606
                                        #   in Loop: Header=BB66_133 Depth=2
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
	jle	.LBB66_141
# BB#140:                               # %if.then29.i607
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	%rbx, tlist_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB66_141:                             # %new_tlist.exit611
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, (%rax)
	movq	%r13, 8(%rax)
	movq	$0, 16(%rax)
	movq	%rax, warned_ids(%rip)
	movq	8(%r14), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movabsq	$.L.str253, %rdi
	callq	warning
.LBB66_142:                             # %if.end12.i.i575
                                        #   in Loop: Header=BB66_133 Depth=2
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB66_133
.LBB66_143:                             # %for.inc.i578
                                        #   in Loop: Header=BB66_128 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_128
.LBB66_144:                             # %warn_for_collisions.exit580
	movq	88(%rsp), %r13
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	%r8, %rax
.LBB66_145:                             # %while.cond.i615
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB66_145
# BB#146:
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	%r13, %rdx
	jmp	.LBB66_147
.LBB66_381:                             # %sw.bb45
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%r9, %rbx
	movq	$0, 64(%rsp)
	movq	$0, 72(%rsp)
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	movq	32(%r13), %rdi
	leaq	88(%rsp), %rsi
	leaq	80(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	verify_tree
	movq	40(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB66_383
# BB#382:                               # %if.then53
	leaq	72(%rsp), %rsi
	leaq	64(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	verify_tree
.LBB66_383:                             # %if.end57
	movq	72(%rsp), %r12
	leaq	64(%rsp), %rax
	movq	%rbx, %r9
.LBB66_384:                             # %while.cond.i1784
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r14
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB66_384
	jmp	.LBB66_401
.LBB66_400:                             # %new_tlist.exit1851
                                        #   in Loop: Header=BB66_401 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%rax)
	movq	%r13, 16(%rax)
	movq	%rax, (%r14)
	movq	$0, (%rax)
	movq	%rax, %r14
.LBB66_401:                             # %while.cond1.outer.i1819
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_385 Depth 2
                                        #       Child Loop BB66_389 Depth 3
	movq	%r12, %rdx
.LBB66_385:                             # %while.cond1.i1788
                                        #   Parent Loop BB66_401 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB66_389 Depth 3
	testq	%rdx, %rdx
	je	.LBB66_402
# BB#386:                               # %while.body3.i1792
                                        #   in Loop: Header=BB66_385 Depth=2
	movq	64(%rsp), %rcx
	movq	(%rdx), %r12
	testq	%rcx, %rcx
	je	.LBB66_387
# BB#388:                               # %for.body.lr.ph.i1796
                                        #   in Loop: Header=BB66_385 Depth=2
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_389:                             # %for.body.i1801
                                        #   Parent Loop BB66_401 Depth=1
                                        #     Parent Loop BB66_385 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_392
# BB#390:                               # %if.then.i1804
                                        #   in Loop: Header=BB66_389 Depth=3
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_392
# BB#391:                               # %if.then9.i1805
                                        #   in Loop: Header=BB66_389 Depth=3
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_392:                             # %for.inc.i1810
                                        #   in Loop: Header=BB66_389 Depth=3
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_389
# BB#393:                               # %for.end.i1812
                                        #   in Loop: Header=BB66_385 Depth=2
	testq	%r9, %rdx
	movq	%r12, %rdx
	jne	.LBB66_385
	jmp	.LBB66_394
.LBB66_387:                             # %while.body3.i1792.if.then15.i1813_crit_edge
                                        #   in Loop: Header=BB66_401 Depth=1
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_394:                             # %if.then15.i1813
                                        #   in Loop: Header=BB66_401 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r13
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_396
# BB#395:                               # %if.then.i1826
                                        #   in Loop: Header=BB66_401 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rdi
	movq	%r9, %r15
	callq	_obstack_newchunk
	movq	%r15, %r9
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_396:                             # %if.end.i1829
                                        #   in Loop: Header=BB66_401 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB66_398
# BB#397:                               # %if.then8.i1832
                                        #   in Loop: Header=BB66_401 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_398:                             # %if.end9.i1846
                                        #   in Loop: Header=BB66_401 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %r8
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB66_400
# BB#399:                               # %if.then29.i1847
                                        #   in Loop: Header=BB66_401 Depth=1
	movq	%r8, tlist_obstack+24(%rip)
	movq	%r8, %rdx
	jmp	.LBB66_400
.LBB66_402:                             # %merge_tlist.exit1820
	movq	64(%rsp), %rax
	testq	%rax, %rax
	je	.LBB66_404
.LBB66_403:                             # %while.body.i1856
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	movq	88(%rsp), %rdx
	testq	%rcx, %rcx
	movq	%rdx, (%rax)
	movq	%rax, 88(%rsp)
	movq	%rcx, %rax
	jne	.LBB66_403
.LBB66_404:                             # %add_tlist.exit1862
	movq	80(%rsp), %rax
	testq	%rax, %rax
	je	.LBB66_406
.LBB66_405:                             # %while.body.i1867
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	(%rbx), %rax
	movq	88(%rsp), %rcx
	testq	%rax, %rax
	movq	%rcx, (%rbx)
	movq	%rbx, 88(%rsp)
	jne	.LBB66_405
	jmp	.LBB66_407
.LBB66_470:                             # %warn_for_collisions.exit1218.thread
	movq	$0, 64(%rsp)
.LBB66_471:                             # %while.end120
	movq	88(%rsp), %rax
	movq	%rax, 8(%r14)
	movq	64(%rsp), %rax
	movq	%rax, 16(%r14)
	movq	%r12, %rsi
.LBB66_472:                             # %if.end121
	movq	8(%r14), %r8
	movq	%rsi, %rdx
.LBB66_473:                             # %while.cond.i724
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r10
	movq	(%r10), %rdx
	testq	%rdx, %rdx
	jne	.LBB66_473
# BB#474:
	movq	%rbx, %r9
	jmp	.LBB66_475
.LBB66_297:
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB66_298:                             # %while.cond.backedge.i1449.us
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	movq	(%rsi), %rdx
	testq	%rcx, %rcx
	movq	%rdx, (%rax)
	movq	%rax, (%rsi)
	movq	%rcx, %rax
	jne	.LBB66_298
.LBB66_309:                             # %add_tlist.exit1451
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rbx
	movq	warned_ids(%rip), %rax
	movq	80(%rsp), %r14
	jmp	.LBB66_310
.LBB66_162:                             # %new_tlist.exit682
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%rbx, 8(%r8)
	movq	%r15, 16(%r8)
	movq	%r8, (%r14)
	movq	$0, (%r8)
	movq	%r8, %r14
	movq	%r12, %rdi
	movq	24(%rsp), %r8           # 8-byte Reload
	movq	%r13, %rdx
.LBB66_147:                             # %while.cond1.i619
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_151 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_164
# BB#148:                               # %while.body3.i623
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	(%r8), %rcx
	movq	(%rdx), %r13
	testq	%rcx, %rcx
	je	.LBB66_149
# BB#150:                               # %for.body.lr.ph.i627
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	8(%rdx), %rbx
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_151:                             # %for.body.i632
                                        #   Parent Loop BB66_147 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%rbx, %rsi
	jne	.LBB66_154
# BB#152:                               # %if.then.i635
                                        #   in Loop: Header=BB66_151 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_154
# BB#153:                               # %if.then9.i636
                                        #   in Loop: Header=BB66_151 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_154:                             # %for.inc.i641
                                        #   in Loop: Header=BB66_151 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_151
# BB#155:                               # %for.end.i643
                                        #   in Loop: Header=BB66_147 Depth=1
	testq	%rdi, %rdx
	movq	%r13, %rdx
	jne	.LBB66_147
	jmp	.LBB66_156
.LBB66_149:                             # %while.body3.i623.if.then15.i644_crit_edge
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	8(%rdx), %rbx
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_156:                             # %if.then15.i644
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	%rdi, %r12
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r15
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_158
# BB#157:                               # %if.then.i657
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	$24, %rsi
	movabsq	$tlist_obstack, %rax
	movq	%rax, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_158:                             # %if.end.i660
                                        #   in Loop: Header=BB66_147 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_160
# BB#159:                               # %if.then8.i663
                                        #   in Loop: Header=BB66_147 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_160:                             # %if.end9.i677
                                        #   in Loop: Header=BB66_147 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_162
# BB#161:                               # %if.then29.i678
                                        #   in Loop: Header=BB66_147 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_162
.LBB66_406:                             # %add_tlist.exit1873
	movq	88(%rsp), %rbx
	testq	%rbx, %rbx
	je	.LBB66_525
.LBB66_407:                             # %for.body.i1719.preheader
	movabsq	$tlist_obstack, %r14
	movabsq	$.L.str253, %r15
	movq	%rbx, %r12
.LBB66_408:                             # %for.body.i1719
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_410 Depth 2
                                        #     Child Loop BB66_413 Depth 2
	movq	16(%r12), %r13
	testq	%r13, %r13
	je	.LBB66_423
# BB#409:                               # %if.then.i1723
                                        #   in Loop: Header=BB66_408 Depth=1
	movq	warned_ids(%rip), %rax
	movq	8(%r12), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	je	.LBB66_413
.LBB66_410:                             # %for.body.i.i1731
                                        #   Parent Loop BB66_408 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	je	.LBB66_423
# BB#411:                               # %for.cond.i.i1727
                                        #   in Loop: Header=BB66_410 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_410
# BB#412:                               #   in Loop: Header=BB66_408 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
.LBB66_413:                             # %while.body.i.i1735
                                        #   Parent Loop BB66_408 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB66_422
# BB#414:                               # %land.lhs.true.i.i1738
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	cmpq	%r13, %rax
	je	.LBB66_422
# BB#415:                               # %if.then10.i.i1741
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_417
# BB#416:                               # %if.then.i1755
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	$24, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_417:                             # %if.end.i1758
                                        #   in Loop: Header=BB66_413 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_419
# BB#418:                               # %if.then8.i1761
                                        #   in Loop: Header=BB66_413 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_419:                             # %if.end9.i1775
                                        #   in Loop: Header=BB66_413 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_421
# BB#420:                               # %if.then29.i1776
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_421:                             # %new_tlist.exit1780
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r8)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r8)
	movq	$0, 16(%r8)
	movq	%r8, warned_ids(%rip)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rsi
	callq	warning
.LBB66_422:                             # %if.end12.i.i1744
                                        #   in Loop: Header=BB66_413 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_413
.LBB66_423:                             # %for.inc.i1747
                                        #   in Loop: Header=BB66_408 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB66_408
# BB#424:                               # %warn_for_collisions.exit1749
	movq	88(%rsp), %rax
	testq	%rax, %rax
	movq	24(%rsp), %rsi          # 8-byte Reload
	je	.LBB66_525
# BB#425:                               # %while.body.i1708.preheader
	movq	(%rsi), %rcx
.LBB66_426:                             # %while.body.i1708
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	movq	%rcx, (%rax)
	movq	%rax, (%rsi)
	movq	%rax, %rcx
	movq	%rdx, %rax
	jne	.LBB66_426
	jmp	.LBB66_525
.LBB66_483:                             # %if.then15.i753
                                        #   in Loop: Header=BB66_475 Depth=1
	movq	%rdx, (%r10)
	movq	$0, (%rdx)
	movq	%rdx, %r10
.LBB66_475:                             # %while.cond1.i728
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_478 Depth 2
	movq	%r8, %rdx
	testq	%rdx, %rdx
	je	.LBB66_484
# BB#476:                               # %while.body3.i732
                                        #   in Loop: Header=BB66_475 Depth=1
	movq	(%rsi), %rax
	movq	(%rdx), %r8
	testq	%rax, %rax
	je	.LBB66_483
# BB#477:                               # %for.body.lr.ph.i736
                                        #   in Loop: Header=BB66_475 Depth=1
	movq	8(%rdx), %rdi
	xorq	%rbx, %rbx
.LBB66_478:                             # %for.body.i741
                                        #   Parent Loop BB66_475 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB66_481
# BB#479:                               # %if.then.i744
                                        #   in Loop: Header=BB66_478 Depth=2
	cmpq	$0, 16(%rax)
	movq	$1, %rbx
	jne	.LBB66_481
# BB#480:                               # %if.then9.i745
                                        #   in Loop: Header=BB66_478 Depth=2
	movq	16(%rdx), %rcx
	movq	%rcx, 16(%rax)
.LBB66_481:                             # %for.inc.i750
                                        #   in Loop: Header=BB66_478 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_478
# BB#482:                               # %for.end.i752
                                        #   in Loop: Header=BB66_475 Depth=1
	testq	%r9, %rbx
	jne	.LBB66_475
	jmp	.LBB66_483
.LBB66_312:                             # %for.cond.i
                                        #   in Loop: Header=BB66_310 Depth=1
	movq	(%rax), %rax
.LBB66_310:                             # %add_tlist.exit1451
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB66_313
# BB#311:                               # %for.body.i1457
                                        #   in Loop: Header=BB66_310 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB66_312
	jmp	.LBB66_326
.LBB66_164:                             # %merge_tlist.exit651
	movq	72(%rsp), %rcx
	movq	%r8, %rdx
	movq	%r8, 24(%rsp)           # 8-byte Spill
.LBB66_165:                             # %while.cond.i686
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r8
	movq	(%r8), %rdx
	testq	%rdx, %rdx
	jne	.LBB66_165
	jmp	.LBB66_166
.LBB66_484:                             # %merge_tlist.exit758
	movq	16(%r14), %r14
	testq	%r14, %r14
	movq	%r15, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	je	.LBB66_525
# BB#485:                               # %while.body.i539.preheader
	movq	(%rax), %r15
	movabsq	$tlist_obstack, %rbx
.LBB66_486:                             # %while.body.i539
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rax
	movq	8(%rax), %r12
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	16(%rax), %r13
	movq	(%rax), %r14
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_488
# BB#487:                               # %if.then.i.i
                                        #   in Loop: Header=BB66_486 Depth=1
	movq	$24, %rsi
	movq	%rbx, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_488:                             # %if.end.i.i
                                        #   in Loop: Header=BB66_486 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_490
# BB#489:                               # %if.then8.i.i
                                        #   in Loop: Header=BB66_486 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_490:                             # %if.end9.i.i
                                        #   in Loop: Header=BB66_486 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_492
# BB#491:                               # %if.then29.i.i
                                        #   in Loop: Header=BB66_486 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_492:                             # %new_tlist.exit.i
                                        #   in Loop: Header=BB66_486 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, (%r8)
	movq	%r13, 16(%r8)
	movq	%r12, 8(%r8)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%r8, (%rax)
	testq	%r14, %r14
	movq	%r8, %r15
	jne	.LBB66_486
	jmp	.LBB66_525
.LBB66_313:                             # %while.cond.preheader.i
	testq	%r14, %r14
	je	.LBB66_326
# BB#314:
	movabsq	$tlist_obstack, %r15
	movabsq	$.L.str253, %r12
.LBB66_315:                             # %while.body.us.i
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r14), %rax
	cmpq	%rbx, %rax
	jne	.LBB66_325
# BB#316:                               # %land.lhs.true.us.i
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	16(%r14), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB66_325
# BB#317:                               # %land.lhs.true.us.i
                                        #   in Loop: Header=BB66_315 Depth=1
	testq	%rax, %rax
	je	.LBB66_325
# BB#318:                               # %if.then10.us.i
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %r13
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_320
# BB#319:                               # %if.then.i1535
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	$24, %rsi
	movq	%r15, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_320:                             # %if.end.i1538
                                        #   in Loop: Header=BB66_315 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_322
# BB#321:                               # %if.then8.i1541
                                        #   in Loop: Header=BB66_315 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_322:                             # %if.end9.i1555
                                        #   in Loop: Header=BB66_315 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_324
# BB#323:                               # %if.then29.i1556
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_324:                             # %new_tlist.exit1560
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r13, (%r8)
	movq	%rbx, 8(%r8)
	movq	$0, 16(%r8)
	movq	%r8, warned_ids(%rip)
	movq	8(%r14), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%rcx, %rsi
	callq	warning
.LBB66_325:                             # %if.end12.us.i
                                        #   in Loop: Header=BB66_315 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB66_315
.LBB66_326:                             # %warn_for_collisions_1.exit
	movq	88(%rsp), %r12
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB66_327:                             # %while.cond.i1461
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB66_327
# BB#328:
	movabsq	$tlist_obstack, %r13
	movq	%r12, %rdx
	jmp	.LBB66_329
.LBB66_174:                             # %if.then15.i715
                                        #   in Loop: Header=BB66_166 Depth=1
	movq	%rdx, (%r8)
	movq	$0, (%rdx)
	movq	%rdx, %r8
.LBB66_166:                             # %while.cond1.i690
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_169 Depth 2
	movq	%rcx, %rdx
	testq	%rdx, %rdx
	je	.LBB66_175
# BB#167:                               # %while.body3.i694
                                        #   in Loop: Header=BB66_166 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rsi
	movq	(%rdx), %rcx
	testq	%rsi, %rsi
	je	.LBB66_174
# BB#168:                               # %for.body.lr.ph.i698
                                        #   in Loop: Header=BB66_166 Depth=1
	movq	8(%rdx), %rdi
	xorq	%rbx, %rbx
.LBB66_169:                             # %for.body.i703
                                        #   Parent Loop BB66_166 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rsi), %rax
	cmpq	%rdi, %rax
	jne	.LBB66_172
# BB#170:                               # %if.then.i706
                                        #   in Loop: Header=BB66_169 Depth=2
	cmpq	$0, 16(%rsi)
	movq	$1, %rbx
	jne	.LBB66_172
# BB#171:                               # %if.then9.i707
                                        #   in Loop: Header=BB66_169 Depth=2
	movq	16(%rdx), %rax
	movq	%rax, 16(%rsi)
.LBB66_172:                             # %for.inc.i712
                                        #   in Loop: Header=BB66_169 Depth=2
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB66_169
# BB#173:                               # %for.end.i714
                                        #   in Loop: Header=BB66_166 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB66_166
	jmp	.LBB66_174
.LBB66_344:                             # %new_tlist.exit1528
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r14, 8(%r8)
	movq	%r15, 16(%r8)
	movq	%r8, (%rbx)
	movq	$0, (%r8)
	movq	%r8, %rbx
	movq	%r12, %rdx
.LBB66_329:                             # %while.cond1.i1465
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_333 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_346
# BB#330:                               # %while.body3.i1469
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rcx
	movq	(%rdx), %r12
	testq	%rcx, %rcx
	je	.LBB66_331
# BB#332:                               # %for.body.lr.ph.i1473
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	8(%rdx), %r14
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_333:                             # %for.body.i1478
                                        #   Parent Loop BB66_329 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%r14, %rsi
	jne	.LBB66_336
# BB#334:                               # %if.then.i1481
                                        #   in Loop: Header=BB66_333 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_336
# BB#335:                               # %if.then9.i1482
                                        #   in Loop: Header=BB66_333 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_336:                             # %for.inc.i1487
                                        #   in Loop: Header=BB66_333 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_333
# BB#337:                               # %for.end.i1489
                                        #   in Loop: Header=BB66_329 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	%r12, %rdx
	jne	.LBB66_329
	jmp	.LBB66_338
.LBB66_331:                             # %while.body3.i1469.if.then15.i1490_crit_edge
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	8(%rdx), %r14
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_338:                             # %if.then15.i1490
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r15
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_340
# BB#339:                               # %if.then.i1503
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	$24, %rsi
	movq	%r13, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_340:                             # %if.end.i1506
                                        #   in Loop: Header=BB66_329 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_342
# BB#341:                               # %if.then8.i1509
                                        #   in Loop: Header=BB66_329 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_342:                             # %if.end9.i1523
                                        #   in Loop: Header=BB66_329 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_344
# BB#343:                               # %if.then29.i1524
                                        #   in Loop: Header=BB66_329 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_344
.LBB66_175:                             # %merge_tlist.exit720
	movq	$0, 80(%rsp)
	movq	$0, 64(%rsp)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rax
	leaq	64(%rsp), %rdx
	leaq	80(%rsp), %rbx
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rbx, %rdx
	callq	verify_tree
	movq	80(%rsp), %r14
	testq	%r14, %r14
	je	.LBB66_193
# BB#176:
	movabsq	$tlist_obstack, %r13
	movq	%r14, %r15
.LBB66_177:                             # %for.body.i763
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_179 Depth 2
                                        #     Child Loop BB66_182 Depth 2
	movq	16(%r15), %r12
	testq	%r12, %r12
	je	.LBB66_192
# BB#178:                               # %if.then.i767
                                        #   in Loop: Header=BB66_177 Depth=1
	movq	warned_ids(%rip), %rax
	movq	8(%r15), %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%r14, 40(%rsp)          # 8-byte Spill
	je	.LBB66_182
.LBB66_179:                             # %for.body.i.i775
                                        #   Parent Loop BB66_177 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	movq	56(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	je	.LBB66_192
# BB#180:                               # %for.cond.i.i771
                                        #   in Loop: Header=BB66_179 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_179
# BB#181:                               #   in Loop: Header=BB66_177 Depth=1
	movq	%r14, 40(%rsp)          # 8-byte Spill
.LBB66_182:                             # %while.body.i.i779
                                        #   Parent Loop BB66_177 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB66_191
# BB#183:                               # %land.lhs.true.i.i782
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	cmpq	%r12, %rax
	je	.LBB66_191
# BB#184:                               # %if.then10.i.i785
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_186
# BB#185:                               # %if.then.i799
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	$24, %rsi
	movq	%r13, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_186:                             # %if.end.i802
                                        #   in Loop: Header=BB66_182 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_188
# BB#187:                               # %if.then8.i805
                                        #   in Loop: Header=BB66_182 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_188:                             # %if.end9.i819
                                        #   in Loop: Header=BB66_182 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_190
# BB#189:                               # %if.then29.i820
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_190:                             # %new_tlist.exit824
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r8)
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r8)
	movq	$0, 16(%r8)
	movq	%r8, warned_ids(%rip)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	movabsq	$.L.str253, %rdx
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	warning
.LBB66_191:                             # %if.end12.i.i788
                                        #   in Loop: Header=BB66_182 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_182
.LBB66_192:                             # %for.inc.i791
                                        #   in Loop: Header=BB66_177 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB66_177
.LBB66_193:                             # %warn_for_collisions.exit793
	movq	64(%rsp), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB66_194:                             # %while.cond.i828
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r13
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB66_194
# BB#195:
	movabsq	$tlist_obstack, %r12
	movq	56(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB66_196
.LBB66_346:                             # %merge_tlist.exit1497
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rbx
	movl	16(%rbx), %eax
	andq	$254, %rax
	cmpq	$34, %rax
	jne	.LBB66_372
# BB#347:                               # %if.then39
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_349
# BB#348:                               # %if.then.i1566
	movabsq	$tlist_obstack, %rax
	movq	$24, %rsi
	movq	%rax, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_349:                             # %if.end.i1569
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_351
# BB#350:                               # %if.then8.i1572
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_351:                             # %if.end9.i1586
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_353
# BB#352:                               # %if.then29.i1587
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_353:                             # %new_tlist.exit1591
	movq	%rdx, tlist_obstack+16(%rip)
	movq	$0, (%r8)
	movq	%rbx, 8(%r8)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rax, 16(%r8)
.LBB66_354:                             # %while.cond.i1595
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_354
# BB#355:                               # %while.cond1.outer.i1630.preheader
	movabsq	$tlist_obstack, %r14
	jmp	.LBB66_356
.LBB66_211:                             # %new_tlist.exit895
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r14, 8(%r8)
	movq	%r15, 16(%r8)
	movq	%r8, (%r13)
	movq	$0, (%r8)
	movq	%r8, %r13
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB66_196:                             # %while.cond1.i832
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_200 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_213
# BB#197:                               # %while.body3.i836
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rcx
	movq	(%rdx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB66_198
# BB#199:                               # %for.body.lr.ph.i840
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	8(%rdx), %r14
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_200:                             # %for.body.i845
                                        #   Parent Loop BB66_196 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%r14, %rsi
	jne	.LBB66_203
# BB#201:                               # %if.then.i848
                                        #   in Loop: Header=BB66_200 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_203
# BB#202:                               # %if.then9.i849
                                        #   in Loop: Header=BB66_200 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_203:                             # %for.inc.i854
                                        #   in Loop: Header=BB66_200 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_200
# BB#204:                               # %for.end.i856
                                        #   in Loop: Header=BB66_196 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB66_196
	jmp	.LBB66_205
.LBB66_198:                             # %while.body3.i836.if.then15.i857_crit_edge
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	8(%rdx), %r14
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_205:                             # %if.then15.i857
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r15
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_207
# BB#206:                               # %if.then.i870
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	$24, %rsi
	movq	%r12, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_207:                             # %if.end.i873
                                        #   in Loop: Header=BB66_196 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_209
# BB#208:                               # %if.then8.i876
                                        #   in Loop: Header=BB66_196 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_209:                             # %if.end9.i890
                                        #   in Loop: Header=BB66_196 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_211
# BB#210:                               # %if.then29.i891
                                        #   in Loop: Header=BB66_196 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_211
.LBB66_371:                             # %new_tlist.exit1662
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	%rcx, tlist_obstack+16(%rip)
	movq	%r12, 8(%r8)
	movq	%r13, 16(%r8)
	movq	%r8, (%rbx)
	movq	$0, (%r8)
	movq	%r8, %rbx
	movq	%r15, %r8
.LBB66_356:                             # %while.cond1.i1599
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_360 Depth 2
	testq	%r8, %r8
	je	.LBB66_372
# BB#357:                               # %while.body3.i1603
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	80(%rsp), %rdx
	movq	(%r8), %r15
	testq	%rdx, %rdx
	je	.LBB66_358
# BB#359:                               # %for.body.lr.ph.i1607
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	8(%r8), %r12
	movq	%r8, %rcx
	addq	$16, %rcx
	xorq	%rax, %rax
.LBB66_360:                             # %for.body.i1612
                                        #   Parent Loop BB66_356 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rdx), %rsi
	cmpq	%r12, %rsi
	jne	.LBB66_363
# BB#361:                               # %if.then.i1615
                                        #   in Loop: Header=BB66_360 Depth=2
	cmpq	$0, 16(%rdx)
	movq	$1, %rax
	jne	.LBB66_363
# BB#362:                               # %if.then9.i1616
                                        #   in Loop: Header=BB66_360 Depth=2
	movq	(%rcx), %rsi
	movq	%rsi, 16(%rdx)
.LBB66_363:                             # %for.inc.i1621
                                        #   in Loop: Header=BB66_360 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB66_360
# BB#364:                               # %for.end.i1623
                                        #   in Loop: Header=BB66_356 Depth=1
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	movq	%r15, %r8
	jne	.LBB66_356
	jmp	.LBB66_365
.LBB66_358:                             # %while.body3.i1603.if.then15.i1624_crit_edge
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	8(%r8), %r12
	addq	$16, %r8
	movq	%r8, %rcx
.LBB66_365:                             # %if.then15.i1624
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rdx
	movq	(%rcx), %r13
	subq	%rdx, %rax
	cmpq	$23, %rax
	jg	.LBB66_367
# BB#366:                               # %if.then.i1637
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	$24, %rsi
	movq	%r14, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rdx
.LBB66_367:                             # %if.end.i1640
                                        #   in Loop: Header=BB66_356 Depth=1
	addq	$24, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rdx
	jne	.LBB66_369
# BB#368:                               # %if.then8.i1643
                                        #   in Loop: Header=BB66_356 Depth=1
	movzbl	tlist_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, tlist_obstack+80(%rip)
.LBB66_369:                             # %if.end9.i1657
                                        #   in Loop: Header=BB66_356 Depth=1
	movslq	tlist_obstack+48(%rip), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rdx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_371
# BB#370:                               # %if.then29.i1658
                                        #   in Loop: Header=BB66_356 Depth=1
	movq	%rdx, tlist_obstack+24(%rip)
	movq	%rdx, %rcx
	jmp	.LBB66_371
.LBB66_213:                             # %merge_tlist.exit864
	movq	$0, 72(%rsp)
	movq	$0, 64(%rsp)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	48(%rax), %rax
	leaq	64(%rsp), %rdx
	leaq	72(%rsp), %r8
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%r8, %rdx
	callq	verify_tree
	movq	72(%rsp), %r14
	testq	%r14, %r14
	je	.LBB66_231
# BB#214:
	movabsq	$tlist_obstack, %r12
	movabsq	$.L.str253, %r13
	movq	%r14, %r15
.LBB66_215:                             # %for.body.i971
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_217 Depth 2
                                        #     Child Loop BB66_220 Depth 2
	movq	16(%r15), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB66_230
# BB#216:                               # %if.then.i975
                                        #   in Loop: Header=BB66_215 Depth=1
	movq	warned_ids(%rip), %rax
	movq	8(%r15), %rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	%r14, 32(%rsp)          # 8-byte Spill
	je	.LBB66_220
.LBB66_217:                             # %for.body.i.i983
                                        #   Parent Loop BB66_215 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rax), %rcx
	movq	40(%rsp), %rdx          # 8-byte Reload
	cmpq	%rdx, %rcx
	je	.LBB66_230
# BB#218:                               # %for.cond.i.i979
                                        #   in Loop: Header=BB66_217 Depth=2
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB66_217
# BB#219:                               #   in Loop: Header=BB66_215 Depth=1
	movq	%r14, 32(%rsp)          # 8-byte Spill
.LBB66_220:                             # %while.body.i.i987
                                        #   Parent Loop BB66_215 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB66_229
# BB#221:                               # %land.lhs.true.i.i990
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	16(%rax), %rax
	movq	56(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	je	.LBB66_229
# BB#222:                               # %if.then10.i.i993
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	tlist_obstack+32(%rip), %rax
	movq	tlist_obstack+24(%rip), %rcx
	movq	warned_ids(%rip), %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB66_224
# BB#223:                               # %if.then.i1007
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	$24, %rsi
	movq	%r12, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_224:                             # %if.end.i1010
                                        #   in Loop: Header=BB66_220 Depth=2
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_226
# BB#225:                               # %if.then8.i1013
                                        #   in Loop: Header=BB66_220 Depth=2
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_226:                             # %if.end9.i1027
                                        #   in Loop: Header=BB66_220 Depth=2
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_228
# BB#227:                               # %if.then29.i1028
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_228:                             # %new_tlist.exit1032
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	%rdx, tlist_obstack+16(%rip)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r8)
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, 8(%r8)
	movq	$0, 16(%r8)
	movq	%r8, warned_ids(%rip)
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	72(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rcx, %rsi
	callq	warning
.LBB66_229:                             # %if.end12.i.i996
                                        #   in Loop: Header=BB66_220 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB66_220
.LBB66_230:                             # %for.inc.i999
                                        #   in Loop: Header=BB66_215 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB66_215
.LBB66_231:                             # %warn_for_collisions.exit1001
	movq	64(%rsp), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
.LBB66_232:                             # %while.cond.i1036
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %r12
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB66_232
# BB#233:
	movabsq	$tlist_obstack, %r13
	movq	56(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB66_234
.LBB66_372:                             # %if.end44
	movq	80(%rsp), %r14
	testq	%r14, %r14
	movq	48(%rsp), %rax          # 8-byte Reload
	je	.LBB66_525
# BB#373:                               # %while.body.i1667.preheader
	movq	(%rax), %r15
	movabsq	$tlist_obstack, %rbx
.LBB66_374:                             # %while.body.i1667
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rax
	movq	8(%rax), %r12
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	16(%rax), %r13
	movq	(%rax), %r14
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_376
# BB#375:                               # %if.then.i.i1678
                                        #   in Loop: Header=BB66_374 Depth=1
	movq	$24, %rsi
	movq	%rbx, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_376:                             # %if.end.i.i1681
                                        #   in Loop: Header=BB66_374 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_378
# BB#377:                               # %if.then8.i.i1684
                                        #   in Loop: Header=BB66_374 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_378:                             # %if.end9.i.i1698
                                        #   in Loop: Header=BB66_374 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_380
# BB#379:                               # %if.then29.i.i1699
                                        #   in Loop: Header=BB66_374 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
.LBB66_380:                             # %new_tlist.exit.i1702
                                        #   in Loop: Header=BB66_374 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, (%r8)
	movq	%r13, 16(%r8)
	movq	%r12, 8(%r8)
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	%r8, (%rax)
	testq	%r14, %r14
	movq	%r8, %r15
	jne	.LBB66_374
	jmp	.LBB66_525
.LBB66_249:                             # %new_tlist.exit1103
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r14, 8(%r8)
	movq	%r15, 16(%r8)
	movq	%r8, (%r12)
	movq	$0, (%r8)
	movq	%r8, %r12
	movq	56(%rsp), %rdx          # 8-byte Reload
.LBB66_234:                             # %while.cond1.i1040
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_238 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_251
# BB#235:                               # %while.body3.i1044
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rcx
	movq	(%rdx), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	je	.LBB66_236
# BB#237:                               # %for.body.lr.ph.i1048
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	8(%rdx), %r14
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_238:                             # %for.body.i1053
                                        #   Parent Loop BB66_234 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%r14, %rsi
	jne	.LBB66_241
# BB#239:                               # %if.then.i1056
                                        #   in Loop: Header=BB66_238 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_241
# BB#240:                               # %if.then9.i1057
                                        #   in Loop: Header=BB66_238 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_241:                             # %for.inc.i1062
                                        #   in Loop: Header=BB66_238 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_238
# BB#242:                               # %for.end.i1064
                                        #   in Loop: Header=BB66_234 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	56(%rsp), %rdx          # 8-byte Reload
	jne	.LBB66_234
	jmp	.LBB66_243
.LBB66_236:                             # %while.body3.i1044.if.then15.i1065_crit_edge
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	8(%rdx), %r14
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_243:                             # %if.then15.i1065
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r15
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_245
# BB#244:                               # %if.then.i1078
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	$24, %rsi
	movq	%r13, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_245:                             # %if.end.i1081
                                        #   in Loop: Header=BB66_234 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_247
# BB#246:                               # %if.then8.i1084
                                        #   in Loop: Header=BB66_234 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_247:                             # %if.end9.i1098
                                        #   in Loop: Header=BB66_234 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_249
# BB#248:                               # %if.then29.i1099
                                        #   in Loop: Header=BB66_234 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_249
.LBB66_251:                             # %merge_tlist.exit1072
	movq	72(%rsp), %r12
.LBB66_252:                             # %while.cond.i1107
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %r14
	movq	(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB66_252
# BB#253:
	movabsq	$tlist_obstack, %rbx
	movq	%r12, %rdx
	jmp	.LBB66_254
.LBB66_269:                             # %new_tlist.exit1174
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	%rdx, tlist_obstack+16(%rip)
	movq	%r15, 8(%r8)
	movq	%r13, 16(%r8)
	movq	%r8, (%r14)
	movq	$0, (%r8)
	movq	%r8, %r14
	movq	%r12, %rdx
.LBB66_254:                             # %while.cond1.i1111
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB66_258 Depth 2
	testq	%rdx, %rdx
	je	.LBB66_271
# BB#255:                               # %while.body3.i1115
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	80(%rsp), %rcx
	movq	(%rdx), %r12
	testq	%rcx, %rcx
	je	.LBB66_256
# BB#257:                               # %for.body.lr.ph.i1119
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	8(%rdx), %r15
	movq	%rdx, %rax
	addq	$16, %rax
	xorq	%rdx, %rdx
.LBB66_258:                             # %for.body.i1124
                                        #   Parent Loop BB66_254 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	cmpq	%r15, %rsi
	jne	.LBB66_261
# BB#259:                               # %if.then.i1127
                                        #   in Loop: Header=BB66_258 Depth=2
	cmpq	$0, 16(%rcx)
	movq	$1, %rdx
	jne	.LBB66_261
# BB#260:                               # %if.then9.i1128
                                        #   in Loop: Header=BB66_258 Depth=2
	movq	(%rax), %rsi
	movq	%rsi, 16(%rcx)
.LBB66_261:                             # %for.inc.i1133
                                        #   in Loop: Header=BB66_258 Depth=2
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB66_258
# BB#262:                               # %for.end.i1135
                                        #   in Loop: Header=BB66_254 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	%r12, %rdx
	jne	.LBB66_254
	jmp	.LBB66_263
.LBB66_256:                             # %while.body3.i1115.if.then15.i1136_crit_edge
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	8(%rdx), %r15
	addq	$16, %rdx
	movq	%rdx, %rax
.LBB66_263:                             # %if.then15.i1136
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	tlist_obstack+32(%rip), %rdx
	movq	tlist_obstack+24(%rip), %rcx
	movq	(%rax), %r13
	subq	%rcx, %rdx
	cmpq	$23, %rdx
	jg	.LBB66_265
# BB#264:                               # %if.then.i1149
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	$24, %rsi
	movq	%rbx, %rdi
	callq	_obstack_newchunk
	movq	tlist_obstack+24(%rip), %rcx
.LBB66_265:                             # %if.end.i1152
                                        #   in Loop: Header=BB66_254 Depth=1
	addq	$24, %rcx
	movq	%rcx, tlist_obstack+24(%rip)
	movq	tlist_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB66_267
# BB#266:                               # %if.then8.i1155
                                        #   in Loop: Header=BB66_254 Depth=1
	movzbl	tlist_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, tlist_obstack+80(%rip)
.LBB66_267:                             # %if.end9.i1169
                                        #   in Loop: Header=BB66_254 Depth=1
	movslq	tlist_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, tlist_obstack+24(%rip)
	movq	tlist_obstack+32(%rip), %rcx
	movq	tlist_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rcx, %rax
	subq	%rsi, %rax
	cmpq	%rax, %rdi
	jle	.LBB66_269
# BB#268:                               # %if.then29.i1170
                                        #   in Loop: Header=BB66_254 Depth=1
	movq	%rcx, tlist_obstack+24(%rip)
	movq	%rcx, %rdx
	jmp	.LBB66_269
.LBB66_271:                             # %merge_tlist.exit1143
	movq	80(%rsp), %rax
	testq	%rax, %rax
	movq	48(%rsp), %rsi          # 8-byte Reload
	je	.LBB66_525
.LBB66_272:                             # %while.body.i1179
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rcx
	movq	(%rsi), %rdx
	testq	%rcx, %rcx
	movq	%rdx, (%rax)
	movq	%rax, (%rsi)
	movq	%rcx, %rax
	jne	.LBB66_272
.LBB66_525:                             # %sw.epilog156
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp515:
	.size	verify_tree, .Ltmp515-verify_tree
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI66_0:
	.quad	.LBB66_525
	.quad	.LBB66_50
	.quad	.LBB66_274
	.quad	.LBB66_493
	.quad	.LBB66_493
	.quad	.LBB66_126
	.quad	.LBB66_493
	.quad	.LBB66_381

	.type	warn_div_by_zero,@object # @warn_div_by_zero
	.data
	.globl	warn_div_by_zero
	.align	4
warn_div_by_zero:
	.long	1                       # 0x1
	.size	warn_div_by_zero, 4

	.type	c_global_trees,@object  # @c_global_trees
	.comm	c_global_trees,256,16
	.type	fname_vars,@object      # @fname_vars
	.section	.rodata,"a",@progbits
	.globl	fname_vars
	.align	16
fname_vars:
	.quad	c_global_trees+232
	.long	64                      # 0x40
	.long	0                       # 0x0
	.quad	c_global_trees+216
	.long	62                      # 0x3e
	.long	0                       # 0x0
	.quad	c_global_trees+224
	.long	63                      # 0x3f
	.long	1                       # 0x1
	.zero	16
	.size	fname_vars, 64

	.type	if_stack_space,@object  # @if_stack_space
	.local	if_stack_space
	.comm	if_stack_space,4,4
	.type	if_stack,@object        # @if_stack
	.local	if_stack
	.comm	if_stack,8,8
	.type	if_stack_pointer,@object # @if_stack_pointer
	.local	if_stack_pointer
	.comm	if_stack_pointer,4,4
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"suggest explicit braces to avoid ambiguous `else'"
	.size	.L.str, 50

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"top level"
	.size	.L.str1, 10

	.type	.L.str2,@object         # @.str2
.L.str2:
	.zero	1
	.size	.L.str2, 1

	.type	make_fname_decl,@object # @make_fname_decl
	.comm	make_fname_decl,8,8
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"`%s' is not defined outside of function scope"
	.size	.L.str3, 46

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"concatenation of string literals with __FUNCTION__ is deprecated"
	.size	.L.str4, 65

	.type	c_language,@object      # @c_language
	.comm	c_language,4,4
	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"string length `%d' is greater than the length `%d' ISO C%d compilers are required to support"
	.size	.L.str5, 93

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"overflow in constant expression"
	.size	.L.str6, 32

	.type	skip_evaluation,@object # @skip_evaluation
	.comm	skip_evaluation,4,4
	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"integer overflow in expression"
	.size	.L.str7, 31

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"floating point overflow in expression"
	.size	.L.str8, 38

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"vector overflow in expression"
	.size	.L.str9, 30

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"large integer implicitly truncated to unsigned type"
	.size	.L.str10, 52

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"negative integer implicitly converted to unsigned type"
	.size	.L.str11, 55

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"overflow in implicit constant conversion"
	.size	.L.str12, 41

	.type	warn_sequence_point,@object # @warn_sequence_point
	.comm	warn_sequence_point,4,4
	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"expression statement has incomplete type"
	.size	.L.str13, 41

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"case label does not reduce to an integer constant"
	.size	.L.str14, 50

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"invalid truth-value expression"
	.size	.L.str15, 31

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"+"
	.size	.L.str16, 2

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"-"
	.size	.L.str17, 2

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"*"
	.size	.L.str18, 2

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"max"
	.size	.L.str19, 4

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"min"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"=="
	.size	.L.str21, 3

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"!="
	.size	.L.str22, 3

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"<="
	.size	.L.str23, 3

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	">="
	.size	.L.str24, 3

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"<"
	.size	.L.str25, 2

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	">"
	.size	.L.str26, 2

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"<<"
	.size	.L.str27, 3

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	">>"
	.size	.L.str28, 3

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"%"
	.size	.L.str29, 2

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"/"
	.size	.L.str30, 2

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"&"
	.size	.L.str31, 2

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"|"
	.size	.L.str32, 2

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"&&"
	.size	.L.str33, 3

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"||"
	.size	.L.str34, 3

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"^"
	.size	.L.str35, 2

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"rotate"
	.size	.L.str36, 7

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"unknown"
	.size	.L.str37, 8

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"invalid operands to binary %s"
	.size	.L.str38, 30

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"comparison is always false due to limited range of data type"
	.size	.L.str39, 61

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"comparison is always true due to limited range of data type"
	.size	.L.str40, 60

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"comparison of unsigned expression >= 0 is always true"
	.size	.L.str41, 54

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"comparison of unsigned expression < 0 is always false"
	.size	.L.str42, 54

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"pointer of type `void *' used in arithmetic"
	.size	.L.str43, 44

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"pointer to a function used in arithmetic"
	.size	.L.str44, 41

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"pointer to member function used in arithmetic"
	.size	.L.str45, 46

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"pointer to a member used in arithmetic"
	.size	.L.str46, 39

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"suggest parentheses around assignment used as truth value"
	.size	.L.str47, 58

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"invalid use of `restrict'"
	.size	.L.str48, 26

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"__alignof__ applied to an incomplete type"
	.size	.L.str49, 42

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"`__alignof' applied to a bit-field"
	.size	.L.str50, 35

	.type	c_format_attribute_table,@object # @c_format_attribute_table
	.section	.rodata,"a",@progbits
	.align	16
c_format_attribute_table:
	.quad	.L.str205
	.long	3                       # 0x3
	.long	3                       # 0x3
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.zero	5
	.quad	handle_format_attribute
	.quad	.L.str206
	.long	1                       # 0x1
	.long	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.zero	5
	.quad	handle_format_arg_attribute
	.zero	32
	.size	c_format_attribute_table, 96

	.type	.L.str51,@object        # @.str51
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str51:
	.asciz	"char"
	.size	.L.str51, 5

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"long int"
	.size	.L.str52, 9

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"unsigned int"
	.size	.L.str53, 13

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"long unsigned int"
	.size	.L.str54, 18

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"unsigned long"
	.size	.L.str55, 14

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"long long int"
	.size	.L.str56, 14

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"long long unsigned int"
	.size	.L.str57, 23

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"long long unsigned"
	.size	.L.str58, 19

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"short int"
	.size	.L.str59, 10

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"short unsigned int"
	.size	.L.str60, 19

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"unsigned short"
	.size	.L.str61, 15

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"signed char"
	.size	.L.str62, 12

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"unsigned char"
	.size	.L.str63, 14

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"__int128_t"
	.size	.L.str64, 11

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"__uint128_t"
	.size	.L.str65, 12

	.type	flag_short_double,@object # @flag_short_double
	.comm	flag_short_double,4,4
	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"long double"
	.size	.L.str66, 12

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"complex int"
	.size	.L.str67, 12

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"complex float"
	.size	.L.str68, 14

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"complex double"
	.size	.L.str69, 15

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"complex long double"
	.size	.L.str70, 20

	.type	flag_short_wchar,@object # @flag_short_wchar
	.comm	flag_short_wchar,4,4
	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"int"
	.size	.L.str71, 4

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"wchar_t"
	.size	.L.str72, 8

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"__builtin_va_list"
	.size	.L.str73, 18

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"__builtin_ptrdiff_t"
	.size	.L.str74, 20

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"__builtin_size_t"
	.size	.L.str75, 17

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"__builtin_alloca"
	.size	.L.str76, 17

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"__builtin_"
	.size	.L.str77, 11

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"__builtin_abs"
	.size	.L.str79, 14

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"__builtin_labs"
	.size	.L.str80, 15

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"__builtin_fabs"
	.size	.L.str81, 15

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"__builtin_fabsf"
	.size	.L.str82, 16

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"__builtin_fabsl"
	.size	.L.str83, 16

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"__builtin_llabs"
	.size	.L.str84, 16

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"__builtin_imaxabs"
	.size	.L.str85, 18

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"__builtin_conj"
	.size	.L.str86, 15

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"__builtin_conjf"
	.size	.L.str87, 16

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"__builtin_conjl"
	.size	.L.str88, 16

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"__builtin_creal"
	.size	.L.str89, 16

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"__builtin_crealf"
	.size	.L.str90, 17

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"__builtin_creall"
	.size	.L.str91, 17

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"__builtin_cimag"
	.size	.L.str92, 16

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"__builtin_cimagf"
	.size	.L.str93, 17

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"__builtin_cimagl"
	.size	.L.str94, 17

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"__builtin_bzero"
	.size	.L.str95, 16

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"__builtin_bcmp"
	.size	.L.str96, 15

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"__builtin_ffs"
	.size	.L.str97, 14

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"__builtin_index"
	.size	.L.str98, 16

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"__builtin_rindex"
	.size	.L.str99, 17

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"__builtin_memcpy"
	.size	.L.str100, 17

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"__builtin_memcmp"
	.size	.L.str101, 17

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"__builtin_memset"
	.size	.L.str102, 17

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"__builtin_strcat"
	.size	.L.str103, 17

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"__builtin_strncat"
	.size	.L.str104, 18

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"__builtin_strcpy"
	.size	.L.str105, 17

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"__builtin_strncpy"
	.size	.L.str106, 18

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"__builtin_strcmp"
	.size	.L.str107, 17

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"__builtin_strncmp"
	.size	.L.str108, 18

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"__builtin_strlen"
	.size	.L.str109, 17

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"__builtin_strstr"
	.size	.L.str110, 17

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"__builtin_strpbrk"
	.size	.L.str111, 18

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"__builtin_strspn"
	.size	.L.str112, 17

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"__builtin_strcspn"
	.size	.L.str113, 18

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"__builtin_strchr"
	.size	.L.str114, 17

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"__builtin_strrchr"
	.size	.L.str115, 18

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"__builtin_sqrt"
	.size	.L.str116, 15

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"__builtin_sin"
	.size	.L.str117, 14

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"__builtin_cos"
	.size	.L.str118, 14

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"__builtin_sqrtf"
	.size	.L.str119, 16

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"__builtin_sinf"
	.size	.L.str120, 15

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"__builtin_cosf"
	.size	.L.str121, 15

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"__builtin_sqrtl"
	.size	.L.str122, 16

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"__builtin_sinl"
	.size	.L.str123, 15

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"__builtin_cosl"
	.size	.L.str124, 15

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"__builtin_saveregs"
	.size	.L.str125, 19

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"__builtin_classify_type"
	.size	.L.str126, 24

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"__builtin_next_arg"
	.size	.L.str127, 19

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"__builtin_args_info"
	.size	.L.str128, 20

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"__builtin_constant_p"
	.size	.L.str129, 21

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"__builtin_frame_address"
	.size	.L.str130, 24

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"__builtin_return_address"
	.size	.L.str131, 25

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"__builtin_aggregate_incoming_address"
	.size	.L.str132, 37

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"__builtin_apply_args"
	.size	.L.str133, 21

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"__builtin_apply"
	.size	.L.str134, 16

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"__builtin_return"
	.size	.L.str135, 17

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"__builtin_setjmp"
	.size	.L.str136, 17

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"__builtin_longjmp"
	.size	.L.str137, 18

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"__builtin_trap"
	.size	.L.str138, 15

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"__builtin_prefetch"
	.size	.L.str139, 19

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"__builtin_putchar"
	.size	.L.str140, 18

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"__builtin_puts"
	.size	.L.str141, 15

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"__builtin_printf"
	.size	.L.str142, 17

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"__builtin_fputc"
	.size	.L.str143, 16

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"__builtin_fputs"
	.size	.L.str144, 16

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"__builtin_fwrite"
	.size	.L.str145, 17

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"__builtin_fprintf"
	.size	.L.str146, 18

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"__builtin_putchar_unlocked"
	.size	.L.str147, 27

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"__builtin_puts_unlocked"
	.size	.L.str148, 24

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"__builtin_printf_unlocked"
	.size	.L.str149, 26

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"__builtin_fputc_unlocked"
	.size	.L.str150, 25

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"__builtin_fputs_unlocked"
	.size	.L.str151, 25

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"__builtin_fwrite_unlocked"
	.size	.L.str152, 26

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"__builtin_fprintf_unlocked"
	.size	.L.str153, 27

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"__builtin_isgreater"
	.size	.L.str154, 20

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"__builtin_isgreaterequal"
	.size	.L.str155, 25

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"__builtin_isless"
	.size	.L.str156, 17

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"__builtin_islessequal"
	.size	.L.str157, 22

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"__builtin_islessgreater"
	.size	.L.str158, 24

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"__builtin_isunordered"
	.size	.L.str159, 22

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"__builtin_unwind_init"
	.size	.L.str160, 22

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"__builtin_dwarf_cfa"
	.size	.L.str161, 20

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"__builtin_dwarf_fp_regnum"
	.size	.L.str162, 26

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"__builtin_init_dwarf_reg_size_table"
	.size	.L.str163, 36

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"__builtin_frob_return_addr"
	.size	.L.str164, 27

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"__builtin_extract_return_addr"
	.size	.L.str165, 30

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"__builtin_eh_return"
	.size	.L.str166, 20

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"__builtin_eh_return_data_regno"
	.size	.L.str167, 31

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"__builtin_varargs_start"
	.size	.L.str168, 24

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"__builtin_stdarg_start"
	.size	.L.str169, 23

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"__builtin_va_end"
	.size	.L.str170, 17

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"__builtin_va_copy"
	.size	.L.str171, 18

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"__builtin_expect"
	.size	.L.str172, 17

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"_exit"
	.size	.L.str173, 6

	.type	.L.str174,@object       # @.str174
.L.str174:
	.asciz	"_Exit"
	.size	.L.str174, 6

	.type	.L.str175,@object       # @.str175
.L.str175:
	.asciz	"abort"
	.size	.L.str175, 6

	.type	.L.str176,@object       # @.str176
.L.str176:
	.asciz	"exit"
	.size	.L.str176, 5

	.type	.L.str177,@object       # @.str177
.L.str177:
	.asciz	"main"
	.size	.L.str177, 5

	.type	.L.str178,@object       # @.str178
.L.str178:
	.asciz	"cannot disable built-in function `%s'"
	.size	.L.str178, 38

	.type	disabled_builtins,@object # @disabled_builtins
	.local	disabled_builtins
	.comm	disabled_builtins,8,8
	.type	.L.str179,@object       # @.str179
.L.str179:
	.asciz	"too few arguments to function `%s'"
	.size	.L.str179, 35

	.type	.L.str180,@object       # @.str180
.L.str180:
	.asciz	"too many arguments to function `%s'"
	.size	.L.str180, 36

	.type	lang_statement_code_p,@object # @lang_statement_code_p
	.comm	lang_statement_code_p,8,8
	.type	.L.str181,@object       # @.str181
.L.str181:
	.asciz	"pointers are not permitted as case values"
	.size	.L.str181, 42

	.type	.L.str182,@object       # @.str182
.L.str182:
	.asciz	"ISO C++ forbids range expressions in switch statements"
	.size	.L.str182, 55

	.type	.L.str183,@object       # @.str183
.L.str183:
	.asciz	"ISO C forbids range expressions in switch statements"
	.size	.L.str183, 53

	.type	.L.str184,@object       # @.str184
.L.str184:
	.asciz	"empty range specified"
	.size	.L.str184, 22

	.type	.L.str185,@object       # @.str185
.L.str185:
	.asciz	"duplicate (or overlapping) case value"
	.size	.L.str185, 38

	.type	.L.str186,@object       # @.str186
.L.str186:
	.asciz	"this is the first entry overlapping that value"
	.size	.L.str186, 47

	.type	.L.str187,@object       # @.str187
.L.str187:
	.asciz	"duplicate case value"
	.size	.L.str187, 21

	.type	.L.str188,@object       # @.str188
.L.str188:
	.asciz	"previously used here"
	.size	.L.str188, 21

	.type	.L.str189,@object       # @.str189
.L.str189:
	.asciz	"multiple default labels in one switch"
	.size	.L.str189, 38

	.type	.L.str190,@object       # @.str190
.L.str190:
	.asciz	"this is the first default label"
	.size	.L.str190, 32

	.type	.L.str191,@object       # @.str191
.L.str191:
	.asciz	"ISO C++ forbids taking the address of a label"
	.size	.L.str191, 46

	.type	.L.str192,@object       # @.str192
.L.str192:
	.asciz	"ISO C forbids taking the address of a label"
	.size	.L.str192, 44

	.type	.L.str193,@object       # @.str193
.L.str193:
	.asciz	"c-common.c"
	.size	.L.str193, 11

	.type	.L__FUNCTION__.c_expand_expr,@object # @__FUNCTION__.c_expand_expr
.L__FUNCTION__.c_expand_expr:
	.asciz	"c_expand_expr"
	.size	.L__FUNCTION__.c_expand_expr, 14

	.type	c_tree_code_type,@object # @c_tree_code_type
	.section	.rodata,"a",@progbits
	.align	16
c_tree_code_type:
	.ascii	"xx1e1eeeeeeeeeeeeeeeeeeee"
	.size	c_tree_code_type, 25

	.type	c_tree_code_length,@object # @c_tree_code_length
	.align	16
c_tree_code_length:
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	3                       # 0x3
	.long	4                       # 0x4
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	3                       # 0x3
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	5                       # 0x5
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	3                       # 0x3
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.size	c_tree_code_length, 100

	.type	c_tree_code_name,@object # @c_tree_code_name
	.align	16
c_tree_code_name:
	.quad	.L.str228
	.quad	.L.str229
	.quad	.L.str230
	.quad	.L.str231
	.quad	.L.str232
	.quad	.L.str233
	.quad	.L.str234
	.quad	.L.str235
	.quad	.L.str236
	.quad	.L.str237
	.quad	.L.str238
	.quad	.L.str239
	.quad	.L.str240
	.quad	.L.str241
	.quad	.L.str242
	.quad	.L.str243
	.quad	.L.str244
	.quad	.L.str245
	.quad	.L.str246
	.quad	.L.str247
	.quad	.L.str248
	.quad	.L.str249
	.quad	.L.str250
	.quad	.L.str251
	.quad	.L.str252
	.size	c_tree_code_name, 200

	.type	.L__FUNCTION__.boolean_increment,@object # @__FUNCTION__.boolean_increment
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.boolean_increment:
	.asciz	"boolean_increment"
	.size	.L__FUNCTION__.boolean_increment, 18

	.type	parse_in,@object        # @parse_in
	.comm	parse_in,8,8
	.type	.L.str194,@object       # @.str194
.L.str194:
	.asciz	"-Wformat-y2k ignored without -Wformat"
	.size	.L.str194, 38

	.type	.L.str195,@object       # @.str195
.L.str195:
	.asciz	"-Wformat-extra-args ignored without -Wformat"
	.size	.L.str195, 45

	.type	.L.str196,@object       # @.str196
.L.str196:
	.asciz	"-Wformat-nonliteral ignored without -Wformat"
	.size	.L.str196, 45

	.type	.L.str197,@object       # @.str197
.L.str197:
	.asciz	"-Wformat-security ignored without -Wformat"
	.size	.L.str197, 43

	.type	.L.str198,@object       # @.str198
.L.str198:
	.asciz	"-Wmissing-format-attribute ignored without -Wformat"
	.size	.L.str198, 52

	.type	c_attrs_initialized,@object # @c_attrs_initialized
	.local	c_attrs_initialized
	.comm	c_attrs_initialized,1,1
	.type	built_in_attributes,@object # @built_in_attributes
	.local	built_in_attributes
	.comm	built_in_attributes,592,16
	.type	.L.str199,@object       # @.str199
.L.str199:
	.asciz	"declaration of `%s' shadows %s"
	.size	.L.str199, 31

	.type	.L.str200,@object       # @.str200
.L.str200:
	.asciz	"shadowed declaration is here"
	.size	.L.str200, 29

	.type	flag_no_builtin,@object # @flag_no_builtin
	.comm	flag_no_builtin,4,4
	.type	flag_no_nonansi_builtin,@object # @flag_no_nonansi_builtin
	.comm	flag_no_nonansi_builtin,4,4
	.type	ridpointers,@object     # @ridpointers
	.comm	ridpointers,8,8
	.type	lang_expand_function_end,@object # @lang_expand_function_end
	.comm	lang_expand_function_end,8,8
	.type	.L.str201,@object       # @.str201
.L.str201:
	.asciz	"printf"
	.size	.L.str201, 7

	.type	.L.str202,@object       # @.str202
.L.str202:
	.asciz	"scanf"
	.size	.L.str202, 6

	.type	.L.str203,@object       # @.str203
.L.str203:
	.asciz	"strftime"
	.size	.L.str203, 9

	.type	.L.str204,@object       # @.str204
.L.str204:
	.asciz	"strfmon"
	.size	.L.str204, 8

	.type	.L.str205,@object       # @.str205
.L.str205:
	.asciz	"format"
	.size	.L.str205, 7

	.type	.L.str206,@object       # @.str206
.L.str206:
	.asciz	"format_arg"
	.size	.L.str206, 11

	.type	.L.str207,@object       # @.str207
.L.str207:
	.asciz	"fprintf"
	.size	.L.str207, 8

	.type	.L.str208,@object       # @.str208
.L.str208:
	.asciz	"sprintf"
	.size	.L.str208, 8

	.type	.L.str209,@object       # @.str209
.L.str209:
	.asciz	"fscanf"
	.size	.L.str209, 7

	.type	.L.str210,@object       # @.str210
.L.str210:
	.asciz	"sscanf"
	.size	.L.str210, 7

	.type	.L.str211,@object       # @.str211
.L.str211:
	.asciz	"vprintf"
	.size	.L.str211, 8

	.type	.L.str212,@object       # @.str212
.L.str212:
	.asciz	"vfprintf"
	.size	.L.str212, 9

	.type	.L.str213,@object       # @.str213
.L.str213:
	.asciz	"vsprintf"
	.size	.L.str213, 9

	.type	.L.str214,@object       # @.str214
.L.str214:
	.asciz	"snprintf"
	.size	.L.str214, 9

	.type	.L.str215,@object       # @.str215
.L.str215:
	.asciz	"vsnprintf"
	.size	.L.str215, 10

	.type	.L.str216,@object       # @.str216
.L.str216:
	.asciz	"vscanf"
	.size	.L.str216, 7

	.type	.L.str217,@object       # @.str217
.L.str217:
	.asciz	"vfscanf"
	.size	.L.str217, 8

	.type	.L.str218,@object       # @.str218
.L.str218:
	.asciz	"vsscanf"
	.size	.L.str218, 8

	.type	.L.str219,@object       # @.str219
.L.str219:
	.asciz	"gettext"
	.size	.L.str219, 8

	.type	.L.str220,@object       # @.str220
.L.str220:
	.asciz	"dgettext"
	.size	.L.str220, 9

	.type	.L.str221,@object       # @.str221
.L.str221:
	.asciz	"dcgettext"
	.size	.L.str221, 10

	.type	.L.str222,@object       # @.str222
.L.str222:
	.asciz	"printf_unlocked"
	.size	.L.str222, 16

	.type	.L.str223,@object       # @.str223
.L.str223:
	.asciz	"fprintf_unlocked"
	.size	.L.str223, 17

	.type	.L.str224,@object       # @.str224
.L.str224:
	.asciz	"built-in function `%s' not currently supported"
	.size	.L.str224, 47

	.type	.L.str225,@object       # @.str225
.L.str225:
	.asciz	"%s"
	.size	.L.str225, 3

	.type	.L.str226,@object       # @.str226
.L.str226:
	.asciz	"%c"
	.size	.L.str226, 3

	.type	.L.str227,@object       # @.str227
.L.str227:
	.asciz	"%s\n"
	.size	.L.str227, 4

	.type	.L.str228,@object       # @.str228
.L.str228:
	.asciz	"@@dummy"
	.size	.L.str228, 8

	.type	.L.str229,@object       # @.str229
.L.str229:
	.asciz	"srcloc"
	.size	.L.str229, 7

	.type	.L.str230,@object       # @.str230
.L.str230:
	.asciz	"sizeof_expr"
	.size	.L.str230, 12

	.type	.L.str231,@object       # @.str231
.L.str231:
	.asciz	"arrow_expr"
	.size	.L.str231, 11

	.type	.L.str232,@object       # @.str232
.L.str232:
	.asciz	"alignof_expr"
	.size	.L.str232, 13

	.type	.L.str233,@object       # @.str233
.L.str233:
	.asciz	"expr_stmt"
	.size	.L.str233, 10

	.type	.L.str234,@object       # @.str234
.L.str234:
	.asciz	"compound_stmt"
	.size	.L.str234, 14

	.type	.L.str235,@object       # @.str235
.L.str235:
	.asciz	"decl_stmt"
	.size	.L.str235, 10

	.type	.L.str236,@object       # @.str236
.L.str236:
	.asciz	"if_stmt"
	.size	.L.str236, 8

	.type	.L.str237,@object       # @.str237
.L.str237:
	.asciz	"for_stmt"
	.size	.L.str237, 9

	.type	.L.str238,@object       # @.str238
.L.str238:
	.asciz	"while_stmt"
	.size	.L.str238, 11

	.type	.L.str239,@object       # @.str239
.L.str239:
	.asciz	"do_stmt"
	.size	.L.str239, 8

	.type	.L.str240,@object       # @.str240
.L.str240:
	.asciz	"return_stmt"
	.size	.L.str240, 12

	.type	.L.str241,@object       # @.str241
.L.str241:
	.asciz	"break_stmt"
	.size	.L.str241, 11

	.type	.L.str242,@object       # @.str242
.L.str242:
	.asciz	"continue_stmt"
	.size	.L.str242, 14

	.type	.L.str243,@object       # @.str243
.L.str243:
	.asciz	"switch_stmt"
	.size	.L.str243, 12

	.type	.L.str244,@object       # @.str244
.L.str244:
	.asciz	"goto_stmt"
	.size	.L.str244, 10

	.type	.L.str245,@object       # @.str245
.L.str245:
	.asciz	"label_stmt"
	.size	.L.str245, 11

	.type	.L.str246,@object       # @.str246
.L.str246:
	.asciz	"asm_stmt"
	.size	.L.str246, 9

	.type	.L.str247,@object       # @.str247
.L.str247:
	.asciz	"scope_stmt"
	.size	.L.str247, 11

	.type	.L.str248,@object       # @.str248
.L.str248:
	.asciz	"file_stmt"
	.size	.L.str248, 10

	.type	.L.str249,@object       # @.str249
.L.str249:
	.asciz	"case_label"
	.size	.L.str249, 11

	.type	.L.str250,@object       # @.str250
.L.str250:
	.asciz	"stmt_expr"
	.size	.L.str250, 10

	.type	.L.str251,@object       # @.str251
.L.str251:
	.asciz	"compound_literal_expr"
	.size	.L.str251, 22

	.type	.L.str252,@object       # @.str252
.L.str252:
	.asciz	"cleanup_stmt"
	.size	.L.str252, 13

	.type	warned_ids,@object      # @warned_ids
	.local	warned_ids
	.comm	warned_ids,8,8
	.type	save_expr_cache,@object # @save_expr_cache
	.local	save_expr_cache
	.comm	save_expr_cache,8,8
	.type	tlist_firstobj,@object  # @tlist_firstobj
	.local	tlist_firstobj
	.comm	tlist_firstobj,8,8
	.type	tlist_obstack,@object   # @tlist_obstack
	.local	tlist_obstack
	.comm	tlist_obstack,88,8
	.type	.L.str253,@object       # @.str253
.L.str253:
	.asciz	"operation on `%s' may be undefined"
	.size	.L.str253, 35


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
