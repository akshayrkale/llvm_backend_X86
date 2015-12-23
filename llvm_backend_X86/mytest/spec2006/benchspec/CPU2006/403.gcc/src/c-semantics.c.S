	.file	"c-semantics.c.bc"
	.text
	.globl	begin_stmt_tree
	.align	16, 0x90
	.type	begin_stmt_tree,@function
begin_stmt_tree:                        # @begin_stmt_tree
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
	movq	c_global_trees+248(%rip), %rsi
	movq	%rdi, %r14
	movq	$152, %rdi
	xorq	%rax, %rax
	callq	build_nt
	movq	%rax, %rbx
	movq	%rbx, (%r14)
	callq	current_stmt_tree
	movq	%rbx, (%rax)
	callq	current_stmt_tree
	movq	$0, 8(%rax)
	movq	input_filename(%rip), %rbx
	callq	current_stmt_tree
	movq	%rbx, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	begin_stmt_tree, .Ltmp8-begin_stmt_tree
	.cfi_endproc

	.globl	add_stmt
	.align	16, 0x90
	.type	add_stmt,@function
add_stmt:                               # @add_stmt
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
	movq	input_filename(%rip), %rbx
	movq	%rdi, %r14
	callq	current_stmt_tree
	movq	16(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB1_3
# BB#1:                                 # %if.then
	movq	input_filename(%rip), %rbx
	callq	current_stmt_tree
	movq	16(%rax), %rsi
	movq	%rbx, %rdi
	callq	strcmp
	movq	input_filename(%rip), %r15
	movq	%rax, %rbx
	callq	current_stmt_tree
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	movq	%r15, 16(%rax)
	je	.LBB1_3
# BB#2:                                 # %if.then7
	movq	input_filename(%rip), %rdi
	callq	get_identifier
	movq	%rax, %rcx
	movq	$167, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	build_nt
	movq	%rax, %rdi
	callq	add_stmt
.LBB1_3:                                # %if.end11
	callq	current_stmt_tree
	movq	(%rax), %rax
	movq	%r14, (%rax)
	callq	current_stmt_tree
	movq	%r14, (%rax)
	callq	stmts_are_full_exprs_p
	movq	%rax, %rbx
	callq	current_stmt_tree
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	andq	$1, %rbx
	shlq	$25, %rbx
	movabsq	$4261412863, %rdx       # imm = 0xFDFFFFFF
	andq	%rcx, %rdx
	orq	%rbx, %rdx
	movl	%edx, 16(%rax)
	movq	current_function_decl(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_5
# BB#4:                                 # %if.then20
	incq	56(%rax)
.LBB1_5:                                # %if.end21
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp19:
	.size	add_stmt, .Ltmp19-add_stmt
	.cfi_endproc

	.globl	add_decl_stmt
	.align	16, 0x90
	.type	add_decl_stmt,@function
add_decl_stmt:                          # @add_decl_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rcx
	movq	$154, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	build_stmt
	movq	%rax, %rdi
	popq	%rax
	jmp	add_stmt  # TAILCALL
.Ltmp22:
	.size	add_decl_stmt, .Ltmp22-add_decl_stmt
	.cfi_endproc

	.globl	build_stmt
	.align	16, 0x90
	.type	build_stmt,@function
build_stmt:                             # @build_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 16
	subq	$80, %rsp
.Ltmp26:
	.cfi_def_cfa_offset 96
.Ltmp27:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	movq	%rsi, 8(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$8, 48(%rsp)
                                        # kill: RDI<def> RBX<kill>
	callq	make_node
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movslq	tree_code_length(,%rbx,4), %rdx
	movl	lineno(%rip), %ecx
	testq	%rdx, %rdx
	movl	%ecx, 24(%rax)
	jle	.LBB3_6
# BB#1:                                 # %for.body.lr.ph
	movq	$4, %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rdx
	.align	16, 0x90
.LBB3_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%rsp), %edi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$40, %rdi
	ja	.LBB3_4
# BB#3:                                 # %vaarg.in_reg
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	64(%rsp), %rcx
	addq	%rbx, %rcx
	addq	$8, %rdi
	movl	%edi, 48(%rsp)
	jmp	.LBB3_5
	.align	16, 0x90
.LBB3_4:                                # %vaarg.in_mem
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdi
	movq	%rdi, 56(%rsp)
.LBB3_5:                                # %vaarg.end
                                        #   in Loop: Header=BB3_2 Depth=1
	movq	(%rcx), %rcx
	movq	%rcx, (%rax,%rsi,8)
	leaq	-3(%rsi), %rcx
	andq	%r8, %rcx
	incq	%rsi
	cmpq	%rdx, %rcx
	jne	.LBB3_2
.LBB3_6:                                # %for.end
	addq	$80, %rsp
	popq	%rbx
	retq
.Ltmp28:
	.size	build_stmt, .Ltmp28-build_stmt
	.cfi_endproc

	.globl	add_scope_stmt
	.align	16, 0x90
	.type	add_scope_stmt,@function
add_scope_stmt:                         # @add_scope_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -48
.Ltmp41:
	.cfi_offset %r12, -40
.Ltmp42:
	.cfi_offset %r13, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r13
	callq	current_scope_stmt_stack
	movq	%rax, %r14
	movq	(%r14), %r15
	movq	$166, %rdi
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rax, %r12
	movl	16(%r12), %eax
	movq	%r13, %rcx
	andq	$1, %rcx
	shlq	$24, %rcx
	movabsq	$4009754623, %rdx       # imm = 0xEEFFFFFF
	andq	%rax, %rdx
	andq	$1, %rbx
	shlq	$28, %rbx
	orq	%rcx, %rbx
	orq	%rdx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	movl	%ebx, 16(%r12)
	je	.LBB4_2
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	tree_cons
	movq	%rax, %r15
	jmp	.LBB4_3
.LBB4_2:                                # %if.else
	movq	%r12, 32(%r15)
	movq	(%r15), %rax
.LBB4_3:                                # %if.end
	movq	%rax, (%r14)
	movq	%r12, %rdi
	callq	add_stmt
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp45:
	.size	add_scope_stmt, .Ltmp45-add_scope_stmt
	.cfi_endproc

	.globl	finish_stmt_tree
	.align	16, 0x90
	.type	finish_stmt_tree,@function
finish_stmt_tree:                       # @finish_stmt_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 16
.Ltmp49:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rax
	movq	(%rax), %rbx
	movq	%rbx, (%rdi)
	callq	current_stmt_tree
	movq	$0, (%rax)
	cmpq	$0, cfun(%rip)
	je	.LBB5_3
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB5_3
# BB#2:                                 # %if.then
	movl	lineno(%rip), %eax
	movl	%eax, 24(%rbx)
	movl	16(%rbx), %eax
	orq	$67108864, %rax         # imm = 0x4000000
	movl	%eax, 16(%rbx)
.LBB5_3:                                # %if.end
	popq	%rbx
	retq
.Ltmp50:
	.size	finish_stmt_tree, .Ltmp50-finish_stmt_tree
	.cfi_endproc

	.globl	expand_cond
	.align	16, 0x90
	.type	expand_cond,@function
expand_cond:                            # @expand_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp53:
	.cfi_def_cfa_offset 16
.Ltmp54:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB6_4
# BB#1:                                 # %land.lhs.true
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB6_2
# BB#3:                                 # %if.then
	movq	24(%rbx), %rdi
	callq	expand_stmt
	movq	32(%rbx), %rax
.LBB6_4:                                # %return
	popq	%rbx
	retq
.LBB6_2:
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp55:
	.size	expand_cond, .Ltmp55-expand_cond
	.cfi_endproc

	.globl	expand_stmt
	.align	16, 0x90
	.type	expand_stmt,@function
expand_stmt:                            # @expand_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp64:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp65:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp67:
	.cfi_def_cfa_offset 96
.Ltmp68:
	.cfi_offset %rbx, -48
.Ltmp69:
	.cfi_offset %r12, -40
.Ltmp70:
	.cfi_offset %r13, -32
.Ltmp71:
	.cfi_offset %r14, -24
.Ltmp72:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB7_70
# BB#1:                                 # %entry
	movq	global_trees(%rip), %rax
	cmpq	%r13, %rax
	je	.LBB7_70
	.align	16, 0x90
.LBB7_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	callq	stmts_are_full_exprs_p
	movl	16(%r13), %ebx
	movq	%rax, %r14
	testq	$67108864, %rbx         # imm = 0x4000000
	jne	.LBB7_4
# BB#3:                                 # %if.then.i144
                                        #   in Loop: Header=BB7_2 Depth=1
	movl	24(%r13), %eax
	movl	%eax, lineno(%rip)
	movl	16(%r13), %ebx
.LBB7_4:                                # %prep_stmt.exit
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	callq	current_stmt_tree
	movl	%ebx, 24(%rax)
	movl	16(%r13), %r15d
	movq	%r15, %rax
	andq	$255, %rax
	addq	$-152, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$19, %rax
	ja	.LBB7_64
# BB#5:                                 # %prep_stmt.exit
                                        #   in Loop: Header=BB7_2 Depth=1
	jmpq	*.LJTI7_0(,%rax,8)
.LBB7_9:                                # %sw.bb2
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	(%r13), %rax
	movq	32(%r13), %r12
	movq	$10, %rcx
	shrq	%cl, %r15
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB7_13
# BB#10:                                # %lor.rhs
                                        #   in Loop: Header=BB7_2 Depth=1
	movzbl	16(%rax), %ecx
	xorq	%rbx, %rbx
	cmpq	$166, %rcx
	jne	.LBB7_13
# BB#11:                                # %land.rhs18
                                        #   in Loop: Header=BB7_2 Depth=1
	cmpq	$0, (%rax)
	movabsq	$0, %rax
	movq	%rax, %rbx
	jne	.LBB7_13
# BB#12:                                # %land.rhs18
                                        #   in Loop: Header=BB7_2 Depth=1
	movabsq	$1, %rax
	movq	%rax, %rbx
.LBB7_13:                               # %lor.end
                                        #   in Loop: Header=BB7_2 Depth=1
	testq	%r12, %r12
	je	.LBB7_68
# BB#14:                                # %if.then.i129
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_16
# BB#15:                                # %if.then2.i130
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	expand_start_target_temps
.LBB7_16:                               # %if.end.i132
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%r12, %rax
	je	.LBB7_18
# BB#17:                                # %if.then4.i
                                        #   in Loop: Header=BB7_2 Depth=1
	andq	$1, %r15
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	expand_expr_stmt_value
.LBB7_18:                               # %if.end5.i
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_68
# BB#19:                                # %if.then8.i
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	expand_end_target_temps
	jmp	.LBB7_68
	.align	16, 0x90
.LBB7_64:                               # %sw.default
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	lang_expand_stmt(%rip), %rax
	testq	%rax, %rax
	je	.LBB7_66
# BB#65:                                # %if.then
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	*%rax
.LBB7_68:                               # %sw.epilog
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	current_stmt_tree
	movl	%r14d, 24(%rax)
	movq	(%r13), %r13
	testq	%r13, %r13
	je	.LBB7_70
# BB#69:                                # %sw.epilog
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r13
	jne	.LBB7_2
	jmp	.LBB7_70
.LBB7_25:                               # %sw.bb30
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %rdi
	callq	expand_stmt
	jmp	.LBB7_68
.LBB7_20:                               # %sw.bb25
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_decl_stmt
	jmp	.LBB7_68
.LBB7_24:                               # %sw.bb29
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_if_stmt
	jmp	.LBB7_68
.LBB7_21:                               # %sw.bb26
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_for_stmt
	jmp	.LBB7_68
.LBB7_22:                               # %sw.bb27
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_while_stmt
	jmp	.LBB7_68
.LBB7_23:                               # %sw.bb28
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_do_stmt
	jmp	.LBB7_68
.LBB7_6:                                # %sw.bb1
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %r15
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	testq	%r15, %r15
	jne	.LBB7_8
# BB#7:                                 # %if.then.i138
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	expand_null_return
	jmp	.LBB7_68
.LBB7_26:                               # %sw.bb31
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	expand_exit_something
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_68
# BB#27:                                # %if.then.i123
                                        #   in Loop: Header=BB7_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str1, %rdi
	callq	error
	jmp	.LBB7_68
.LBB7_28:                               # %sw.bb32
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	xorq	%rdi, %rdi
	callq	expand_continue_loop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_68
# BB#29:                                # %if.then.i118
                                        #   in Loop: Header=BB7_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str2, %rdi
	callq	error
	jmp	.LBB7_68
.LBB7_30:                               # %sw.bb33
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
	movq	32(%r13), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB7_34
# BB#31:                                # %land.lhs.true.i160
                                        #   in Loop: Header=BB7_2 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB7_32
# BB#33:                                # %if.then.i162
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rbx), %rdi
	callq	expand_stmt
	movq	32(%rbx), %r15
	jmp	.LBB7_34
.LBB7_42:                               # %sw.bb39
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %r15
	movl	16(%r15), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rcx
	jne	.LBB7_43
# BB#46:                                # %if.then6.i
                                        #   in Loop: Header=BB7_2 Depth=1
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%r15)
.LBB7_47:                               # %if.end10.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movzbl	16(%r15), %eax
	cmpq	$31, %rax
	jne	.LBB7_49
# BB#48:                                # %if.then16.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r15, %rdi
	callq	label_rtx
	movq	%r15, %rdi
	callq	expand_goto
	jmp	.LBB7_68
.LBB7_41:                               # %sw.bb35
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %rdi
	callq	expand_label
	jmp	.LBB7_68
.LBB7_50:                               # %sw.bb43
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %r12
	movq	40(%r13), %rbx
	movq	48(%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	56(%r13), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	64(%r13), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	andq	$16777216, %r15         # imm = 0x1000000
	testq	%r12, %r12
	jne	.LBB7_52
# BB#51:                                #   in Loop: Header=BB7_2 Depth=1
	xorq	%r12, %r12
	jmp	.LBB7_54
.LBB7_59:                               # %sw.bb63
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r13, %rdi
	callq	genrtl_scope_stmt
	jmp	.LBB7_68
.LBB7_67:                               # %sw.bb
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %rax
	movq	32(%rax), %rax
	movq	%rax, input_filename(%rip)
	jmp	.LBB7_68
.LBB7_37:                               # %sw.bb34
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	last_cleanup_this_contour
	testq	%rax, %rax
	je	.LBB7_40
# BB#38:                                # %if.then.i112
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	24(%rax), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	callq	warning_with_decl
	xorq	%rax, %rax
	movabsq	$.L.str5, %rdi
	callq	warning
	movzbl	genrtl_case_label.explained(%rip), %eax
	testq	%rax, %rax
	jne	.LBB7_40
# BB#39:                                # %if.then2.i113
                                        #   in Loop: Header=BB7_2 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str6, %rdi
	callq	warning
	movq	$1, %rax
	movb	%al, genrtl_case_label.explained(%rip)
.LBB7_40:                               # %genrtl_case_label.exit
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %rdi
	movq	40(%r13), %rsi
	movq	48(%r13), %rdx
	leaq	40(%rsp), %rcx
	callq	add_case_node
	jmp	.LBB7_68
.LBB7_60:                               # %sw.bb64
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB7_63
# BB#61:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB7_2 Depth=1
	cmpq	$0, 40(%rdi)
	je	.LBB7_68
# BB#62:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	8(%rdi), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB7_68
.LBB7_63:                               # %if.then.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	40(%r13), %rsi
	movq	$18, %rcx
	shrq	%cl, %r15
	andq	$1, %r15
	movq	%r15, %rdx
	callq	expand_decl_cleanup_eh
	jmp	.LBB7_68
.LBB7_8:                                # %if.else.i139
                                        #   in Loop: Header=BB7_2 Depth=1
	callq	expand_start_target_temps
	movq	%r15, %rdi
	callq	expand_return
	callq	expand_end_target_temps
	jmp	.LBB7_68
.LBB7_52:                               # %land.lhs.true.i101
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	ridpointers(%rip), %rax
	movq	72(%rax), %rax
	cmpq	%r12, %rax
	je	.LBB7_54
# BB#53:                                # %if.then.i102
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	32(%r12), %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movabsq	$.L.str7, %rdi
	callq	warning
.LBB7_54:                               # %if.end.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	jne	.LBB7_55
# BB#56:                                # %if.else.i
                                        #   in Loop: Header=BB7_2 Depth=1
	testq	%r12, %r12
	movabsq	$0, %r8
	je	.LBB7_58
# BB#57:                                # %if.else.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movabsq	$1, %r8
.LBB7_58:                               # %if.else.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %r9
	movl	lineno(%rip), %eax
	movq	%rax, (%rsp)
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	callq	c_expand_asm_operands
	jmp	.LBB7_68
.LBB7_49:                               # %if.else.i109
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%r15, %rdi
	callq	expand_computed_goto
	jmp	.LBB7_68
.LBB7_55:                               # %if.then2.i
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rbx, %rdi
	callq	expand_asm
	jmp	.LBB7_68
.LBB7_32:                               #   in Loop: Header=BB7_2 Depth=1
	movq	%rbx, %r15
.LBB7_34:                               # %expand_cond.exit
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r15
	jne	.LBB7_36
# BB#35:                                #   in Loop: Header=BB7_2 Depth=1
	movq	c_global_trees+144(%rip), %r15
.LBB7_36:                               # %select.end
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movq	8(%r15), %rdx
	movq	$1, %rdi
	movq	%r15, %rsi
	movabsq	$.L.str3, %rcx
	callq	expand_start_case
	movq	40(%r13), %rdi
	callq	expand_stmt
	movq	48(%r13), %rsi
	movq	%r15, %rdi
	callq	expand_end_case_type
	jmp	.LBB7_68
.LBB7_43:                               # %sw.bb39
                                        #   in Loop: Header=BB7_2 Depth=1
	cmpq	$1, %rcx
	jne	.LBB7_47
# BB#44:                                # %if.then.i107
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.genrtl_goto_stmt, %rcx
	movq	$304, %rsi              # imm = 0x130
	jmp	.LBB7_45
.LBB7_70:                               # %while.end
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_66:                               # %if.else
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_stmt, %rcx
	movq	$858, %rsi              # imm = 0x35A
.LBB7_45:                               # %if.then.i107
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp73:
	.size	expand_stmt, .Ltmp73-expand_stmt
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI7_0:
	.quad	.LBB7_9
	.quad	.LBB7_25
	.quad	.LBB7_20
	.quad	.LBB7_24
	.quad	.LBB7_21
	.quad	.LBB7_22
	.quad	.LBB7_23
	.quad	.LBB7_6
	.quad	.LBB7_26
	.quad	.LBB7_28
	.quad	.LBB7_30
	.quad	.LBB7_42
	.quad	.LBB7_41
	.quad	.LBB7_50
	.quad	.LBB7_59
	.quad	.LBB7_67
	.quad	.LBB7_37
	.quad	.LBB7_64
	.quad	.LBB7_64
	.quad	.LBB7_60

	.text
	.globl	make_rtl_for_local_static
	.align	16, 0x90
	.type	make_rtl_for_local_static,@function
make_rtl_for_local_static:              # @make_rtl_for_local_static
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 16
.Ltmp77:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	17(%rbx), %eax
	testq	$64, %rax
	je	.LBB8_1
# BB#9:                                 # %return
	popq	%rbx
	retq
.LBB8_1:                                # %if.end
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB8_3
# BB#2:                                 # %cond.false
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB8_3:                                # %cond.end
	movq	72(%rbx), %rcx
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	je	.LBB8_8
# BB#4:                                 # %land.lhs.true
	cmpq	$0, 144(%rbx)
	jne	.LBB8_8
# BB#5:                                 # %if.then8
	testq	%rax, %rax
	jne	.LBB8_7
# BB#6:                                 # %cond.false13
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB8_7:                                # %cond.end14
	movq	32(%rax), %rsi
.LBB8_8:                                # %if.end17
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	popq	%rbx
	jmp	rest_of_decl_compilation  # TAILCALL
.Ltmp78:
	.size	make_rtl_for_local_static, .Ltmp78-make_rtl_for_local_static
	.cfi_endproc

	.globl	emit_local_var
	.align	16, 0x90
	.type	emit_local_var,@function
emit_local_var:                         # @emit_local_var
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp82:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp84:
	.cfi_def_cfa_offset 32
.Ltmp85:
	.cfi_offset %rbx, -24
.Ltmp86:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	cmpq	$0, 144(%rbx)
	jne	.LBB9_6
# BB#1:                                 # %if.then
	movzbl	53(%rbx), %eax
	testq	$2, %rax
	je	.LBB9_5
# BB#2:                                 # %if.then3
	movq	120(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB9_4
# BB#3:                                 # %cond.false
	movq	%rbx, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%rbx), %rax
.LBB9_4:                                # %cond.end
	movq	32(%rax), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	rest_of_decl_compilation
	jmp	.LBB9_6
.LBB9_5:                                # %if.else
	movq	%rbx, %rdi
	callq	expand_decl
.LBB9_6:                                # %if.end8
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB9_8
# BB#7:                                 # %if.then10
	callq	expand_start_target_temps
.LBB9_8:                                # %if.end11
	movq	%rbx, %rdi
	callq	expand_decl_init
	callq	stmts_are_full_exprs_p
	addq	$8, %rsp
	testq	%r14, %rax
	je	.LBB9_9
# BB#10:                                # %if.then14
	popq	%rbx
	popq	%r14
	jmp	expand_end_target_temps  # TAILCALL
.LBB9_9:                                # %if.end15
	popq	%rbx
	popq	%r14
	retq
.Ltmp87:
	.size	emit_local_var, .Ltmp87-emit_local_var
	.cfi_endproc

	.globl	genrtl_do_pushlevel
	.align	16, 0x90
	.type	genrtl_do_pushlevel,@function
genrtl_do_pushlevel:                    # @genrtl_do_pushlevel
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp89:
	.cfi_def_cfa_offset 16
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	popq	%rax
	jmp	clear_last_expr  # TAILCALL
.Ltmp90:
	.size	genrtl_do_pushlevel, .Ltmp90-genrtl_do_pushlevel
	.cfi_endproc

	.globl	genrtl_goto_stmt
	.align	16, 0x90
	.type	genrtl_goto_stmt,@function
genrtl_goto_stmt:                       # @genrtl_goto_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp93:
	.cfi_def_cfa_offset 16
.Ltmp94:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$31, %rcx
	jne	.LBB11_1
# BB#3:                                 # %if.then6
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
.LBB11_4:                               # %if.end10
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB11_6
# BB#5:                                 # %if.then16
	movq	%rbx, %rdi
	callq	label_rtx
	movq	%rbx, %rdi
	popq	%rbx
	jmp	expand_goto  # TAILCALL
.LBB11_6:                               # %if.else
	movq	%rbx, %rdi
	popq	%rbx
	jmp	expand_computed_goto  # TAILCALL
.LBB11_1:                               # %entry
	cmpq	$1, %rcx
	jne	.LBB11_4
# BB#2:                                 # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.genrtl_goto_stmt, %rcx
	movq	$304, %rsi              # imm = 0x130
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp95:
	.size	genrtl_goto_stmt, .Ltmp95-genrtl_goto_stmt
	.cfi_endproc

	.globl	genrtl_expr_stmt
	.align	16, 0x90
	.type	genrtl_expr_stmt,@function
genrtl_expr_stmt:                       # @genrtl_expr_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp101:
	.cfi_def_cfa_offset 32
.Ltmp102:
	.cfi_offset %rbx, -24
.Ltmp103:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB12_6
# BB#1:                                 # %if.then.i
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB12_3
# BB#2:                                 # %if.then2.i
	callq	expand_start_target_temps
.LBB12_3:                               # %if.end.i
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB12_5
# BB#4:                                 # %if.then4.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	expand_expr_stmt_value
.LBB12_5:                               # %if.end5.i
	callq	stmts_are_full_exprs_p
	testq	%r14, %rax
	je	.LBB12_6
# BB#7:                                 # %if.then8.i
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_end_target_temps  # TAILCALL
.LBB12_6:                               # %genrtl_expr_stmt_value.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp104:
	.size	genrtl_expr_stmt, .Ltmp104-genrtl_expr_stmt
	.cfi_endproc

	.globl	genrtl_expr_stmt_value
	.align	16, 0x90
	.type	genrtl_expr_stmt_value,@function
genrtl_expr_stmt_value:                 # @genrtl_expr_stmt_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp112:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp113:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp114:
	.cfi_def_cfa_offset 48
.Ltmp115:
	.cfi_offset %rbx, -40
.Ltmp116:
	.cfi_offset %r12, -32
.Ltmp117:
	.cfi_offset %r14, -24
.Ltmp118:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB13_6
# BB#1:                                 # %if.then
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB13_3
# BB#2:                                 # %if.then2
	callq	expand_start_target_temps
.LBB13_3:                               # %if.end
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB13_5
# BB#4:                                 # %if.then4
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	expand_expr_stmt_value
.LBB13_5:                               # %if.end5
	callq	stmts_are_full_exprs_p
	testq	%r12, %rax
	je	.LBB13_6
# BB#7:                                 # %if.then8
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	expand_end_target_temps  # TAILCALL
.LBB13_6:                               # %if.end10
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp119:
	.size	genrtl_expr_stmt_value, .Ltmp119-genrtl_expr_stmt_value
	.cfi_endproc

	.globl	genrtl_decl_stmt
	.align	16, 0x90
	.type	genrtl_decl_stmt,@function
genrtl_decl_stmt:                       # @genrtl_decl_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp124:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp125:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 32
.Ltmp127:
	.cfi_offset %rbx, -32
.Ltmp128:
	.cfi_offset %r14, -24
.Ltmp129:
	.cfi_offset %r15, -16
	movq	input_filename(%rip), %rax
	movl	lineno(%rip), %esi
	movq	%rdi, %rbx
	movq	%rax, %rdi
	callq	emit_line_note
	movq	32(%rbx), %r14
	movl	16(%r14), %eax
	movq	%rax, %rcx
	andq	$262399, %rcx           # imm = 0x400FF
	cmpq	$262178, %rcx           # imm = 0x40022
	jne	.LBB14_1
# BB#5:                                 # %if.then28
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB14_16
# BB#6:                                 # %if.end.i
	movq	120(%r14), %rax
	testq	%rax, %rax
	jne	.LBB14_8
# BB#7:                                 # %cond.false.i
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %rax
.LBB14_8:                               # %cond.end.i
	movq	72(%r14), %rcx
	xorq	%rsi, %rsi
	cmpq	%rcx, %rax
	je	.LBB14_13
# BB#9:                                 # %land.lhs.true.i
	cmpq	$0, 144(%r14)
	jne	.LBB14_13
# BB#10:                                # %if.then8.i
	testq	%rax, %rax
	jne	.LBB14_12
# BB#11:                                # %cond.false13.i
	movq	%r14, %rdi
	callq	*lang_set_decl_assembler_name(%rip)
	movq	120(%r14), %rax
.LBB14_12:                              # %cond.end14.i
	movq	32(%rax), %rsi
.LBB14_13:                              # %if.end17.i
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	rest_of_decl_compilation  # TAILCALL
.LBB14_1:                               # %entry
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$34, %rcx
	jne	.LBB14_14
# BB#2:                                 # %land.lhs.true4
	movzbl	49(%r14), %ecx
	testq	$1, %rcx
	je	.LBB14_3
.LBB14_14:                              # %if.else29
	andq	$33554687, %rax         # imm = 0x20000FF
	cmpq	$33554463, %rax         # imm = 0x200001F
	jne	.LBB14_15
# BB#18:                                # %if.then41
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	declare_nonlocal_label  # TAILCALL
.LBB14_15:                              # %if.else42
	movq	lang_expand_decl_stmt(%rip), %rax
	testq	%rax, %rax
	je	.LBB14_16
# BB#19:                                # %if.then44
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmpq	*%rax  # TAILCALL
.LBB14_16:                              # %if.end48
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB14_3:                               # %if.then
	movq	8(%r14), %rdi
	callq	anon_aggr_type_p
	testq	%r15, %rax
	je	.LBB14_17
# BB#4:                                 # %if.else
	movq	88(%r14), %rdx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	expand_anon_union_decl  # TAILCALL
.LBB14_17:                              # %if.then13
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	emit_local_var  # TAILCALL
.Ltmp130:
	.size	genrtl_decl_stmt, .Ltmp130-genrtl_decl_stmt
	.cfi_endproc

	.globl	genrtl_if_stmt
	.align	16, 0x90
	.type	genrtl_if_stmt,@function
genrtl_if_stmt:                         # @genrtl_if_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 32
.Ltmp138:
	.cfi_offset %rbx, -32
.Ltmp139:
	.cfi_offset %r14, -24
.Ltmp140:
	.cfi_offset %r15, -16
	movq	input_filename(%rip), %rax
	movl	lineno(%rip), %esi
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	emit_line_note
	callq	clear_last_expr
	movq	32(%r14), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB15_4
# BB#1:                                 # %land.lhs.true.i
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB15_2
# BB#3:                                 # %if.then.i
	movq	24(%rbx), %rdi
	callq	expand_stmt
	movq	32(%rbx), %r15
	jmp	.LBB15_4
.LBB15_2:
	movq	%rbx, %r15
.LBB15_4:                               # %expand_cond.exit
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	expand_start_cond
	movq	40(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB15_6
# BB#5:                                 # %if.then
	callq	expand_stmt
.LBB15_6:                               # %if.end
	cmpq	$0, 48(%r14)
	je	.LBB15_8
# BB#7:                                 # %if.then12
	callq	expand_start_else
	movq	48(%r14), %rdi
	callq	expand_stmt
.LBB15_8:                               # %if.end16
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	expand_end_cond  # TAILCALL
.Ltmp141:
	.size	genrtl_if_stmt, .Ltmp141-genrtl_if_stmt
	.cfi_endproc

	.globl	genrtl_while_stmt
	.align	16, 0x90
	.type	genrtl_while_stmt,@function
genrtl_while_stmt:                      # @genrtl_while_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp146:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 32
.Ltmp149:
	.cfi_offset %rbx, -32
.Ltmp150:
	.cfi_offset %r14, -24
.Ltmp151:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	callq	emit_nop
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movq	$1, %rdi
	callq	expand_start_loop
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
	movq	32(%r14), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB16_4
# BB#1:                                 # %land.lhs.true.i
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB16_2
# BB#3:                                 # %if.then.i
	movq	24(%rbx), %rdi
	callq	expand_stmt
	movq	32(%rbx), %r15
	jmp	.LBB16_4
.LBB16_2:
	movq	%rbx, %r15
.LBB16_4:                               # %expand_cond.exit
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	callq	expand_exit_loop_top_cond
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
	movq	40(%r14), %rdi
	callq	expand_stmt
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	expand_end_loop  # TAILCALL
.Ltmp152:
	.size	genrtl_while_stmt, .Ltmp152-genrtl_while_stmt
	.cfi_endproc

	.globl	genrtl_do_stmt
	.align	16, 0x90
	.type	genrtl_do_stmt,@function
genrtl_do_stmt:                         # @genrtl_do_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp156:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp157:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp158:
	.cfi_def_cfa_offset 32
.Ltmp159:
	.cfi_offset %rbx, -24
.Ltmp160:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	32(%rbx), %r14
	movq	%r14, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_1
# BB#6:                                 # %if.then
	callq	expand_start_null_loop
	movq	40(%rbx), %rdi
	callq	expand_stmt
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_end_null_loop  # TAILCALL
.LBB17_1:                               # %if.else
	callq	emit_nop
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movq	$1, %rdi
	callq	expand_start_loop_continue_elsewhere
	movq	40(%rbx), %rdi
	callq	expand_stmt
	callq	expand_loop_continue_here
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB17_5
# BB#2:                                 # %land.lhs.true.i
	movzbl	16(%r14), %eax
	cmpq	$2, %rax
	jne	.LBB17_3
# BB#4:                                 # %if.then.i
	movq	24(%r14), %rdi
	callq	expand_stmt
	movq	32(%r14), %rbx
	jmp	.LBB17_5
.LBB17_3:
	movq	%r14, %rbx
.LBB17_5:                               # %expand_cond.exit
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	expand_exit_loop_if_false
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_end_loop  # TAILCALL
.Ltmp161:
	.size	genrtl_do_stmt, .Ltmp161-genrtl_do_stmt
	.cfi_endproc

	.globl	build_return_stmt
	.align	16, 0x90
	.type	build_return_stmt,@function
build_return_stmt:                      # @build_return_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rcx
	movq	$159, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	jmp	build_stmt  # TAILCALL
.Ltmp162:
	.size	build_return_stmt, .Ltmp162-build_return_stmt
	.cfi_endproc

	.globl	genrtl_return_stmt
	.align	16, 0x90
	.type	genrtl_return_stmt,@function
genrtl_return_stmt:                     # @genrtl_return_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 16
.Ltmp166:
	.cfi_offset %rbx, -16
	movq	32(%rdi), %rbx
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	testq	%rbx, %rbx
	je	.LBB19_1
# BB#2:                                 # %if.else
	callq	expand_start_target_temps
	movq	%rbx, %rdi
	callq	expand_return
	popq	%rbx
	jmp	expand_end_target_temps  # TAILCALL
.LBB19_1:                               # %if.then
	popq	%rbx
	jmp	expand_null_return  # TAILCALL
.Ltmp167:
	.size	genrtl_return_stmt, .Ltmp167-genrtl_return_stmt
	.cfi_endproc

	.globl	genrtl_for_stmt
	.align	16, 0x90
	.type	genrtl_for_stmt,@function
genrtl_for_stmt:                        # @genrtl_for_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp177:
	.cfi_def_cfa_offset 48
.Ltmp178:
	.cfi_offset %rbx, -40
.Ltmp179:
	.cfi_offset %r12, -32
.Ltmp180:
	.cfi_offset %r14, -24
.Ltmp181:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	19(%r14), %eax
	testq	$1, %rax
	je	.LBB20_2
# BB#1:                                 # %if.then
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
.LBB20_2:                               # %if.end
	movq	32(%r14), %rdi
	callq	expand_stmt
	callq	emit_nop
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movq	$1, %rdi
	callq	expand_start_loop_continue_elsewhere
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
	movq	40(%r14), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	je	.LBB20_6
# BB#3:                                 # %land.lhs.true.i
	movzbl	16(%rbx), %eax
	cmpq	$2, %rax
	jne	.LBB20_4
# BB#5:                                 # %if.then.i
	movq	24(%rbx), %rdi
	callq	expand_stmt
	movq	32(%rbx), %r12
	jmp	.LBB20_6
.LBB20_4:
	movq	%rbx, %r12
.LBB20_6:                               # %expand_cond.exit
	movq	input_filename(%rip), %rbx
	movl	lineno(%rip), %r15d
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_line_note
	testq	%r12, %r12
	je	.LBB20_8
# BB#7:                                 # %if.then8
	xorq	%rdi, %rdi
	movq	%r12, %rsi
	callq	expand_exit_loop_top_cond
.LBB20_8:                               # %if.end10
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	clear_last_expr
	movq	56(%r14), %rdi
	callq	expand_stmt
	movq	%rbx, input_filename(%rip)
	movl	%r15d, lineno(%rip)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_line_note
	callq	expand_loop_continue_here
	movq	48(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB20_15
# BB#9:                                 # %if.then.i.i
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	stmts_are_full_exprs_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	je	.LBB20_11
# BB#10:                                # %if.then2.i.i
	callq	expand_start_target_temps
.LBB20_11:                              # %if.end.i.i
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB20_13
# BB#12:                                # %if.then4.i.i
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	$1, %rdx
	movq	%rbx, %rdi
	callq	expand_expr_stmt_value
.LBB20_13:                              # %if.end5.i.i
	callq	stmts_are_full_exprs_p
	testq	%r14, %rax
	je	.LBB20_15
# BB#14:                                # %if.then8.i.i
	callq	expand_end_target_temps
.LBB20_15:                              # %if.end23
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	expand_end_loop  # TAILCALL
.Ltmp182:
	.size	genrtl_for_stmt, .Ltmp182-genrtl_for_stmt
	.cfi_endproc

	.globl	build_break_stmt
	.align	16, 0x90
	.type	build_break_stmt,@function
build_break_stmt:                       # @build_break_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$160, %rdi
	xorq	%rax, %rax
	jmp	build_stmt  # TAILCALL
.Ltmp183:
	.size	build_break_stmt, .Ltmp183-build_break_stmt
	.cfi_endproc

	.globl	genrtl_break_stmt
	.align	16, 0x90
	.type	genrtl_break_stmt,@function
genrtl_break_stmt:                      # @genrtl_break_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp185:
	.cfi_def_cfa_offset 16
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	callq	expand_exit_something
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB22_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB22_2:                               # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	popq	%rdx
	jmp	error  # TAILCALL
.Ltmp186:
	.size	genrtl_break_stmt, .Ltmp186-genrtl_break_stmt
	.cfi_endproc

	.globl	build_continue_stmt
	.align	16, 0x90
	.type	build_continue_stmt,@function
build_continue_stmt:                    # @build_continue_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$161, %rdi
	xorq	%rax, %rax
	jmp	build_stmt  # TAILCALL
.Ltmp187:
	.size	build_continue_stmt, .Ltmp187-build_continue_stmt
	.cfi_endproc

	.globl	genrtl_continue_stmt
	.align	16, 0x90
	.type	genrtl_continue_stmt,@function
genrtl_continue_stmt:                   # @genrtl_continue_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp189:
	.cfi_def_cfa_offset 16
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	xorq	%rdi, %rdi
	callq	expand_continue_loop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB24_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB24_2:                               # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	popq	%rdx
	jmp	error  # TAILCALL
.Ltmp190:
	.size	genrtl_continue_stmt, .Ltmp190-genrtl_continue_stmt
	.cfi_endproc

	.globl	genrtl_scope_stmt
	.align	16, 0x90
	.type	genrtl_scope_stmt,@function
genrtl_scope_stmt:                      # @genrtl_scope_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp196:
	.cfi_def_cfa_offset 32
.Ltmp197:
	.cfi_offset %rbx, -24
.Ltmp198:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %esi
	movq	32(%rbx), %r14
	testq	$134217728, %rsi        # imm = 0x8000000
	je	.LBB25_1
# BB#11:                                # %if.else23
	testq	%r14, %r14
	je	.LBB25_22
# BB#12:                                # %if.end38.thread
	movq	$24, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	xorq	$-97, %rsi
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%r14, 32(%rax)
	jmp	.LBB25_13
.LBB25_1:                               # %if.then
	testq	$16777216, %rsi         # imm = 0x1000000
	je	.LBB25_7
# BB#2:                                 # %if.then6
	testq	%r14, %r14
	jne	.LBB25_3
# BB#4:                                 # %if.then6
	movabsq	$1, %rdi
	jmp	.LBB25_5
.LBB25_7:                               # %if.then16
	testq	%r14, %r14
	je	.LBB25_8
# BB#9:                                 # %if.then16
	movabsq	$1, %rsi
	jmp	.LBB25_10
.LBB25_3:
	movabsq	$0, %rdi
.LBB25_5:                               # %if.then6
	addq	%rdi, %rdi
	movq	%r14, %rsi
	callq	expand_start_bindings_and_block
	jmp	.LBB25_6
.LBB25_8:
	movabsq	$0, %rsi
.LBB25_10:                              # %if.then16
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_end_bindings
.LBB25_6:                               # %if.end38
	testq	%r14, %r14
	je	.LBB25_22
.LBB25_13:                              # %land.lhs.true
	movzbl	19(%rbx), %eax
	testq	$1, %rax
	jne	.LBB25_22
# BB#14:                                # %if.then46
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB25_22
# BB#15:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB25_16:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	jne	.LBB25_21
# BB#17:                                # %land.lhs.true54
                                        #   in Loop: Header=BB25_16 Depth=1
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB25_21
# BB#18:                                # %land.lhs.true54
                                        #   in Loop: Header=BB25_16 Depth=1
	movq	80(%rbx), %rcx
	movq	current_function_decl(%rip), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB25_21
# BB#19:                                # %land.lhs.true54
                                        #   in Loop: Header=BB25_16 Depth=1
	andq	$1024, %rax             # imm = 0x400
	testq	%r14, %rax
	je	.LBB25_21
# BB#20:                                # %if.then69
                                        #   in Loop: Header=BB25_16 Depth=1
	callq	push_function_context
	movq	%rbx, %rdi
	callq	output_inline_function
	callq	pop_function_context
.LBB25_21:                              # %for.inc
                                        #   in Loop: Header=BB25_16 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB25_16
.LBB25_22:                              # %if.end72
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp199:
	.size	genrtl_scope_stmt, .Ltmp199-genrtl_scope_stmt
	.cfi_endproc

	.globl	genrtl_switch_stmt
	.align	16, 0x90
	.type	genrtl_switch_stmt,@function
genrtl_switch_stmt:                     # @genrtl_switch_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp204:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp205:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp206:
	.cfi_def_cfa_offset 32
.Ltmp207:
	.cfi_offset %rbx, -32
.Ltmp208:
	.cfi_offset %r14, -24
.Ltmp209:
	.cfi_offset %r15, -16
	movq	input_filename(%rip), %rax
	movl	lineno(%rip), %esi
	movq	%rdi, %r14
	movq	%rax, %rdi
	callq	emit_line_note
	callq	clear_last_expr
	movq	32(%r14), %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB26_4
# BB#1:                                 # %land.lhs.true.i
	movzbl	16(%r15), %eax
	cmpq	$2, %rax
	jne	.LBB26_2
# BB#3:                                 # %if.then.i
	movq	24(%r15), %rdi
	callq	expand_stmt
	movq	32(%r15), %rbx
	jmp	.LBB26_4
.LBB26_2:
	movq	%r15, %rbx
.LBB26_4:                               # %expand_cond.exit
	movq	global_trees(%rip), %rax
	cmpq	%rax, %rbx
	jne	.LBB26_6
# BB#5:
	movq	c_global_trees+144(%rip), %rbx
.LBB26_6:                               # %select.end
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movq	8(%rbx), %rdx
	movabsq	$.L.str3, %rcx
	movq	$1, %rdi
	movq	%rbx, %rsi
	callq	expand_start_case
	movq	40(%r14), %rdi
	callq	expand_stmt
	movq	48(%r14), %rsi
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	expand_end_case_type  # TAILCALL
.Ltmp210:
	.size	genrtl_switch_stmt, .Ltmp210-genrtl_switch_stmt
	.cfi_endproc

	.globl	build_case_label
	.align	16, 0x90
	.type	build_case_label,@function
build_case_label:                       # @build_case_label
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %r8
	movq	$168, %rdi
	xorq	%rax, %rax
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%r8, %rsi
	jmp	build_stmt  # TAILCALL
.Ltmp211:
	.size	build_case_label, .Ltmp211-build_case_label
	.cfi_endproc

	.globl	genrtl_case_label
	.align	16, 0x90
	.type	genrtl_case_label,@function
genrtl_case_label:                      # @genrtl_case_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp214:
	.cfi_def_cfa_offset 16
	subq	$16, %rsp
.Ltmp215:
	.cfi_def_cfa_offset 32
.Ltmp216:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	last_cleanup_this_contour
	testq	%rax, %rax
	je	.LBB28_3
# BB#1:                                 # %if.then
	movq	24(%rax), %rdi
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	callq	warning_with_decl
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	warning
	movzbl	genrtl_case_label.explained(%rip), %eax
	testq	%rax, %rax
	jne	.LBB28_3
# BB#2:                                 # %if.then2
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	callq	warning
	movq	$1, %rax
	movb	%al, genrtl_case_label.explained(%rip)
.LBB28_3:                               # %if.end3
	movq	32(%rbx), %rdi
	movq	40(%rbx), %rsi
	movq	48(%rbx), %rdx
	leaq	8(%rsp), %rcx
	callq	add_case_node
	addq	$16, %rsp
	popq	%rbx
	retq
.Ltmp217:
	.size	genrtl_case_label, .Ltmp217-genrtl_case_label
	.cfi_endproc

	.globl	genrtl_compound_stmt
	.align	16, 0x90
	.type	genrtl_compound_stmt,@function
genrtl_compound_stmt:                   # @genrtl_compound_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rdi
	jmp	expand_stmt  # TAILCALL
.Ltmp218:
	.size	genrtl_compound_stmt, .Ltmp218-genrtl_compound_stmt
	.cfi_endproc

	.globl	genrtl_asm_stmt
	.align	16, 0x90
	.type	genrtl_asm_stmt,@function
genrtl_asm_stmt:                        # @genrtl_asm_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp227:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp228:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp229:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp230:
	.cfi_def_cfa_offset 64
.Ltmp231:
	.cfi_offset %rbx, -48
.Ltmp232:
	.cfi_offset %r12, -40
.Ltmp233:
	.cfi_offset %r13, -32
.Ltmp234:
	.cfi_offset %r14, -24
.Ltmp235:
	.cfi_offset %r15, -16
	movq	%r9, %rbx
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r13
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB30_4
# BB#1:                                 # %land.lhs.true
	movq	ridpointers(%rip), %rax
	movq	72(%rax), %rax
	cmpq	%rdi, %rax
	je	.LBB30_2
# BB#3:                                 # %if.then
	movq	32(%rdi), %rsi
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	warning
	jmp	.LBB30_4
.LBB30_2:
	movq	%rdi, %r14
.LBB30_4:                               # %if.end
	movq	input_filename(%rip), %rdi
	movl	lineno(%rip), %esi
	callq	emit_line_note
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB30_5
# BB#9:                                 # %if.then2
	movq	%r13, %rdi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	expand_asm  # TAILCALL
.LBB30_5:                               # %if.else
	testq	%r14, %r14
	je	.LBB30_6
# BB#7:                                 # %if.else
	movabsq	$1, %r8
	jmp	.LBB30_8
.LBB30_6:
	movabsq	$0, %r8
.LBB30_8:                               # %if.else
	movq	input_filename(%rip), %r9
	movl	lineno(%rip), %eax
	movq	%rax, (%rsp)
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	c_expand_asm_operands
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp236:
	.size	genrtl_asm_stmt, .Ltmp236-genrtl_asm_stmt
	.cfi_endproc

	.globl	genrtl_decl_cleanup
	.align	16, 0x90
	.type	genrtl_decl_cleanup,@function
genrtl_decl_cleanup:                    # @genrtl_decl_cleanup
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	movq	32(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB31_4
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, 40(%rdi)
	je	.LBB31_3
# BB#2:                                 # %land.lhs.true
	movq	8(%rdi), %rcx
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %rcx
	je	.LBB31_3
.LBB31_4:                               # %if.then
	movl	16(%rax), %edx
	movq	40(%rax), %rsi
	movq	$18, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	jmp	expand_decl_cleanup_eh  # TAILCALL
.LBB31_3:                               # %if.end
	retq
.Ltmp237:
	.size	genrtl_decl_cleanup, .Ltmp237-genrtl_decl_cleanup
	.cfi_endproc

	.globl	prep_stmt
	.align	16, 0x90
	.type	prep_stmt,@function
prep_stmt:                              # @prep_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp240:
	.cfi_def_cfa_offset 16
.Ltmp241:
	.cfi_offset %rbx, -16
	movl	16(%rdi), %ebx
	testq	$67108864, %rbx         # imm = 0x4000000
	jne	.LBB32_2
# BB#1:                                 # %if.then
	movl	24(%rdi), %eax
	movl	%eax, lineno(%rip)
	movl	16(%rdi), %ebx
.LBB32_2:                               # %if.end
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	callq	current_stmt_tree
	movl	%ebx, 24(%rax)
	popq	%rbx
	retq
.Ltmp242:
	.size	prep_stmt, .Ltmp242-prep_stmt
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"c-semantics.c"
	.size	.L.str, 14

	.type	.L__FUNCTION__.genrtl_goto_stmt,@object # @__FUNCTION__.genrtl_goto_stmt
.L__FUNCTION__.genrtl_goto_stmt:
	.asciz	"genrtl_goto_stmt"
	.size	.L__FUNCTION__.genrtl_goto_stmt, 17

	.type	lang_expand_decl_stmt,@object # @lang_expand_decl_stmt
	.comm	lang_expand_decl_stmt,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"break statement not within loop or switch"
	.size	.L.str1, 42

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"continue statement not within a loop"
	.size	.L.str2, 37

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"switch statement"
	.size	.L.str3, 17

	.type	genrtl_case_label.explained,@object # @genrtl_case_label.explained
	.local	genrtl_case_label.explained
	.comm	genrtl_case_label.explained,1,1
	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"destructor needed for `%#D'"
	.size	.L.str4, 28

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"where case label appears here"
	.size	.L.str5, 30

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"(enclose actions of previous case statements requiring destructors in their own scope.)"
	.size	.L.str6, 88

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s qualifier ignored on asm"
	.size	.L.str7, 28

	.type	lang_expand_stmt,@object # @lang_expand_stmt
	.comm	lang_expand_stmt,8,8
	.type	.L__FUNCTION__.expand_stmt,@object # @__FUNCTION__.expand_stmt
.L__FUNCTION__.expand_stmt:
	.asciz	"expand_stmt"
	.size	.L__FUNCTION__.expand_stmt, 12


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
