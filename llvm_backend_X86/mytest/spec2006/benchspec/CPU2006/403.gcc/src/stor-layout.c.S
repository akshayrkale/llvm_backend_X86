	.file	"stor-layout.c.bc"
	.text
	.globl	internal_reference_types
	.align	16, 0x90
	.type	internal_reference_types,@function
internal_reference_types:               # @internal_reference_types
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movb	%al, reference_types_internal(%rip)
	retq
.Ltmp0:
	.size	internal_reference_types, .Ltmp0-internal_reference_types
	.cfi_endproc

	.globl	get_pending_sizes
	.align	16, 0x90
	.type	get_pending_sizes,@function
get_pending_sizes:                      # @get_pending_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	pending_sizes(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_3
# BB#1:
	movq	%rax, %rcx
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	current_function_decl(%rip), %rdx
	movq	32(%rcx), %rsi
	movq	%rdx, 40(%rsi)
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB1_2
.LBB1_3:                                # %for.end
	movq	$0, pending_sizes(%rip)
	retq
.Ltmp1:
	.size	get_pending_sizes, .Ltmp1-get_pending_sizes
	.cfi_endproc

	.globl	is_pending_size
	.align	16, 0x90
	.type	is_pending_size,@function
is_pending_size:                        # @is_pending_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	pending_sizes(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB2_1
	.align	16, 0x90
.LBB2_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rcx), %rax
	cmpq	%rdi, %rax
	movq	$1, %rax
	je	.LBB2_5
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB2_4 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB2_4
# BB#3:
	xorq	%rax, %rax
	retq
.LBB2_1:
	xorq	%rax, %rax
	retq
.LBB2_5:                                # %return
	retq
.Ltmp2:
	.size	is_pending_size, .Ltmp2-is_pending_size
	.cfi_endproc

	.globl	put_pending_size
	.align	16, 0x90
	.type	put_pending_size,@function
put_pending_size:                       # @put_pending_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_7:                                #   in Loop: Header=BB3_1 Depth=1
	addq	$32, %rax
	movq	(%rax), %rax
.LBB3_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %edx
	cmpq	$50, %rdx
	jne	.LBB3_2
# BB#6:                                 # %land.rhs
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	40(%rax), %rdx
	movzbl	17(%rdx), %edx
	testq	$2, %rdx
	jne	.LBB3_7
	jmp	.LBB3_3
	.align	16, 0x90
.LBB3_2:                                # %while.cond
                                        #   in Loop: Header=BB3_1 Depth=1
	cmpq	$49, %rdx
	je	.LBB3_7
.LBB3_3:                                # %while.end
	cmpq	$118, %rcx
	jne	.LBB3_5
# BB#4:                                 # %if.then
	movq	pending_sizes(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, pending_sizes(%rip)
.LBB3_5:                                # %if.end
	popq	%rax
	retq
.Ltmp5:
	.size	put_pending_size, .Ltmp5-put_pending_size
	.cfi_endproc

	.globl	put_pending_sizes
	.align	16, 0x90
	.type	put_pending_sizes,@function
put_pending_sizes:                      # @put_pending_sizes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp7:
	.cfi_def_cfa_offset 16
	cmpq	$0, pending_sizes(%rip)
	jne	.LBB4_2
# BB#1:                                 # %if.end
	movq	%rdi, pending_sizes(%rip)
	popq	%rax
	retq
.LBB4_2:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.put_pending_sizes, %rcx
	movq	$137, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp8:
	.size	put_pending_sizes, .Ltmp8-put_pending_sizes
	.cfi_endproc

	.globl	variable_size
	.align	16, 0x90
	.type	variable_size,@function
variable_size:                          # @variable_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp12:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp14:
	.cfi_def_cfa_offset 32
.Ltmp15:
	.cfi_offset %rbx, -24
.Ltmp16:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	jne	.LBB5_12
# BB#1:                                 # %lor.lhs.false
	callq	global_bindings_p
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_12
# BB#2:                                 # %lor.lhs.false1
	movq	%rbx, %rdi
	callq	contains_placeholder_p
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB5_12
# BB#3:                                 # %if.end
	movq	%rbx, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$118, %rcx
	jne	.LBB5_5
# BB#4:                                 # %if.then9
	orq	$16384, %rax            # imm = 0x4000
	movl	%eax, 16(%rbx)
.LBB5_5:                                # %if.end13
	callq	global_bindings_p
	testq	%r14, %rax
	je	.LBB5_10
# BB#6:                                 # %if.then16
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	je	.LBB5_9
# BB#7:                                 # %if.then23
	movabsq	$.L.str1, %rdi
	jmp	.LBB5_8
.LBB5_10:                               # %if.end25
	movl	immediate_size_expand(%rip), %eax
	testq	%rax, %rax
	je	.LBB5_13
# BB#11:                                # %if.then27
	movq	global_trees+88(%rip), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	jmp	.LBB5_12
.LBB5_9:                                # %if.else
	movabsq	$.L.str2, %rdi
.LBB5_8:                                # %return
	xorq	%rax, %rax
	callq	error
	movq	global_trees+128(%rip), %rbx
.LBB5_12:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB5_13:                               # %if.else30
	movq	cfun(%rip), %rax
	testq	%rax, %rax
	movq	%rbx, %rsi
	je	.LBB5_15
# BB#14:                                # %land.lhs.true
	movzbl	426(%rax), %eax
	testq	$8, %rax
	movq	%rbx, %rsi
	jne	.LBB5_12
	jmp	.LBB5_15
.LBB5_20:                               #   in Loop: Header=BB5_15 Depth=1
	addq	$32, %rsi
	movq	(%rsi), %rsi
.LBB5_15:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rsi), %eax
	movzbl	tree_code_type(%rax), %ecx
	cmpq	$50, %rcx
	jne	.LBB5_16
# BB#19:                                # %land.rhs.i
                                        #   in Loop: Header=BB5_15 Depth=1
	movq	40(%rsi), %rcx
	movzbl	17(%rcx), %ecx
	testq	$2, %rcx
	jne	.LBB5_20
	jmp	.LBB5_17
.LBB5_16:                               # %while.cond.i
                                        #   in Loop: Header=BB5_15 Depth=1
	cmpq	$49, %rcx
	je	.LBB5_20
.LBB5_17:                               # %while.end.i
	cmpq	$118, %rax
	jne	.LBB5_12
# BB#18:                                # %if.then.i
	movq	pending_sizes(%rip), %rdx
	xorq	%rdi, %rdi
	callq	tree_cons
	movq	%rax, pending_sizes(%rip)
	jmp	.LBB5_12
.Ltmp17:
	.size	variable_size, .Ltmp17-variable_size
	.cfi_endproc

	.globl	mode_for_size
	.align	16, 0x90
	.type	mode_for_size,@function
mode_for_size:                          # @mode_for_size
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rdx
	je	.LBB6_2
# BB#1:                                 # %land.lhs.true
	movzbl	mode_bitsize+10(%rip), %eax
	movq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	%rcx, %rax
	movq	$51, %rax
	jb	.LBB6_7
.LBB6_2:                                # %if.end
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	class_narrowest_mode(,%rsi,4), %eax
	testq	%rax, %rax
	je	.LBB6_6
# BB#3:
	andq	%r8, %rdi
	.align	16, 0x90
.LBB6_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rdi, %rcx
	je	.LBB6_7
# BB#5:                                 # %for.inc
                                        #   in Loop: Header=BB6_4 Depth=1
	movzbl	mode_wider_mode(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB6_4
.LBB6_6:
	movq	$51, %rax
.LBB6_7:                                # %return
	retq
.Ltmp18:
	.size	mode_for_size, .Ltmp18-mode_for_size
	.cfi_endproc

	.globl	mode_for_size_tree
	.align	16, 0x90
	.type	mode_for_size_tree,@function
mode_for_size_tree:                     # @mode_for_size_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 32
.Ltmp26:
	.cfi_offset %rbx, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$25, %rax
	jne	.LBB7_8
# BB#1:                                 # %lor.lhs.false
	movq	$1000, %rsi             # imm = 0x3E8
	movq	%rbx, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB7_8
# BB#2:                                 # %if.else
	movq	32(%rbx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %r14
	je	.LBB7_4
# BB#3:                                 # %land.lhs.true.i
	movzbl	mode_bitsize+10(%rip), %eax
	movq	%rdx, %rcx
	andq	%rsi, %rcx
	cmpq	%rcx, %rax
	jb	.LBB7_8
.LBB7_4:                                # %if.end.i
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movl	class_narrowest_mode(,%r15,4), %eax
	testq	%rax, %rax
	je	.LBB7_8
# BB#5:
	andq	%rsi, %rdx
	.align	16, 0x90
.LBB7_6:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB7_9
# BB#7:                                 # %for.inc.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movzbl	mode_wider_mode(%rsi), %eax
	testq	%rax, %rax
	jne	.LBB7_6
.LBB7_8:
	movq	$51, %rax
.LBB7_9:                                # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp29:
	.size	mode_for_size_tree, .Ltmp29-mode_for_size_tree
	.cfi_endproc

	.globl	smallest_mode_for_size
	.align	16, 0x90
	.type	smallest_mode_for_size,@function
smallest_mode_for_size:                 # @smallest_mode_for_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp31:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	class_narrowest_mode(,%rsi,4), %eax
	testq	%rax, %rax
	je	.LBB8_4
# BB#1:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rdi, %rcx
	jae	.LBB8_5
# BB#3:                                 # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	movzbl	mode_wider_mode(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB8_2
.LBB8_4:                                # %for.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.smallest_mode_for_size, %rcx
	movq	$259, %rsi              # imm = 0x103
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB8_5:                                # %if.then
	popq	%rdx
	retq
.Ltmp32:
	.size	smallest_mode_for_size, .Ltmp32-smallest_mode_for_size
	.cfi_endproc

	.globl	int_mode_for_mode
	.align	16, 0x90
	.type	int_mode_for_mode,@function
int_mode_for_mode:                      # @int_mode_for_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp34:
	.cfi_def_cfa_offset 16
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	mode_class(,%rax,4), %ecx
	cmpq	$8, %rcx
	ja	.LBB9_5
# BB#1:                                 # %entry
	movq	$1, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$484, %rsi              # imm = 0x1E4
	je	.LBB9_2
# BB#6:                                 # %sw.bb1
	movl	class_narrowest_mode+4(%rip), %edi
	testq	%rdi, %rdi
	je	.LBB9_10
# BB#7:
	movzbl	mode_bitsize(%rax,%rax), %eax
	andq	$65535, %rax            # imm = 0xFFFF
	.align	16, 0x90
.LBB9_8:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rax, %rcx
	je	.LBB9_11
# BB#9:                                 # %for.inc.i
                                        #   in Loop: Header=BB9_8 Depth=1
	movzbl	mode_wider_mode(%rdx), %edi
	testq	%rdi, %rdi
	jne	.LBB9_8
.LBB9_10:
	movq	$51, %rdi
	jmp	.LBB9_11
.LBB9_2:                                # %entry
	shlq	%cl, %rdx
	testq	$10, %rdx
	jne	.LBB9_11
# BB#3:                                 # %entry
	testq	%rcx, %rcx
	jne	.LBB9_5
# BB#4:                                 # %sw.bb4
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	cmpq	$51, %rdi
	movq	$51, %rdi
	jne	.LBB9_5
.LBB9_11:                               # %sw.epilog
	movq	%rdi, %rax
	popq	%rdx
	retq
.LBB9_5:                                # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.int_mode_for_mode, %rdx
	movq	$290, %rsi              # imm = 0x122
	callq	fancy_abort
.Ltmp35:
	.size	int_mode_for_mode, .Ltmp35-int_mode_for_mode
	.cfi_endproc

	.globl	round_up
	.align	16, 0x90
	.type	round_up,@function
round_up:                               # @round_up
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp41:
	.cfi_def_cfa_offset 32
.Ltmp42:
	.cfi_offset %rbx, -24
.Ltmp43:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	8(%r14), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %rbx
	movq	$63, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	size_binop  # TAILCALL
.Ltmp44:
	.size	round_up, .Ltmp44-round_up
	.cfi_endproc

	.globl	round_down
	.align	16, 0x90
	.type	round_down,@function
round_down:                             # @round_down
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 32
.Ltmp51:
	.cfi_offset %rbx, -24
.Ltmp52:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$32, %rcx
	movq	8(%r14), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	%rsi, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %rbx
	movq	$64, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	size_binop  # TAILCALL
.Ltmp53:
	.size	round_down, .Ltmp53-round_down
	.cfi_endproc

	.globl	layout_decl
	.align	16, 0x90
	.type	layout_decl,@function
layout_decl:                            # @layout_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 64
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	16(%r12), %eax
	movq	%rax, %r14
	andq	$255, %r14
	cmpq	$32, %r14
	je	.LBB12_77
# BB#1:                                 # %if.else
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	8(%r12), %r15
	movq	%rax, %r13
	andq	$254, %r13
	cmpq	$34, %r13
	je	.LBB12_5
# BB#2:                                 # %if.else
	cmpq	$36, %r14
	je	.LBB12_5
# BB#3:                                 # %if.else
	cmpq	$33, %r14
	je	.LBB12_5
# BB#4:                                 # %if.else
	cmpq	$37, %r14
	jne	.LBB12_78
.LBB12_5:                               # %if.end13
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %r15
	jne	.LBB12_7
# BB#6:
	movq	global_trees+216(%rip), %r15
.LBB12_7:                               # %select.end
	movl	16(%r15), %ecx
	andq	$8192, %rcx             # imm = 0x2000
	movabsq	$4294959103, %rdx       # imm = 0xFFFFDFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, 16(%r12)
	movq	48(%r12), %rax
	testq	$255, %rax
	jne	.LBB12_9
# BB#8:                                 # %if.then28
	movl	60(%r15), %edx
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	andq	$-256, %rax
	orq	%rdx, %rax
	movq	%rax, 48(%r12)
.LBB12_9:                               # %if.end40
	movq	40(%r12), %rsi
	testq	%rsi, %rsi
	je	.LBB12_10
# BB#11:                                # %if.else51
	movq	sizetype_tab(%rip), %rbx
	movq	global_trees+152(%rip), %rdx
	movq	$63, %rdi
	callq	size_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	jmp	.LBB12_12
.LBB12_10:                              # %if.then43
	movq	32(%r15), %rax
	movq	%rax, 40(%r12)
	movq	40(%r15), %rax
.LBB12_12:                              # %if.end57
	movq	%rax, 64(%r12)
	cmpq	$37, %r14
	jne	.LBB12_14
# BB#13:                                # %land.lhs.true59
	movq	48(%r12), %rdx
	testq	$4096, %rdx             # imm = 0x1000
	jne	.LBB12_22
.LBB12_14:                              # %land.lhs.true65
	movl	56(%r12), %ecx
	movq	%rcx, %rsi
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	je	.LBB12_15
# BB#16:                                # %lor.lhs.false
	cmpq	$37, %r14
	jne	.LBB12_18
# BB#17:                                # %land.lhs.true71
	movq	48(%r12), %rdx
	testq	$1024, %rdx             # imm = 0x400
	jne	.LBB12_22
.LBB12_18:                              # %land.lhs.true78
	movl	64(%r15), %edx
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rsi
	cmpq	%rsi, %rdx
	jbe	.LBB12_20
	jmp	.LBB12_19
.LBB12_15:                              # %land.lhs.true65.if.then86_crit_edge
	movl	64(%r15), %edx
.LBB12_19:                              # %if.then86
	andq	$16777215, %rdx         # imm = 0xFFFFFF
	movabsq	$4278190080, %rsi       # imm = 0xFF000000
	andq	%rsi, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 56(%r12)
	movabsq	$-34359738369, %rcx     # imm = 0xFFFFFFF7FFFFFFFF
	andq	%rcx, 48(%r12)
.LBB12_20:                              # %if.end99
	cmpq	$37, %r14
	jne	.LBB12_60
# BB#21:                                # %if.end99.if.then101_crit_edge
	movq	48(%r12), %rdx
.LBB12_22:                              # %if.then101
	movq	%rdx, %rsi
	andq	$4096, %rsi             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	je	.LBB12_24
# BB#23:                                # %if.then101
	movq	%r15, %rsi
.LBB12_24:                              # %if.then101
	movq	%rsi, 96(%r12)
	movl	maximum_field_alignment(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB12_28
# BB#25:                                # %if.then111
	movl	56(%r12), %eax
	movq	%rax, %rsi
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	cmpq	%rcx, %rsi
	movq	%rax, %rsi
	jb	.LBB12_27
# BB#26:                                # %if.then111
	movq	%rcx, %rsi
.LBB12_27:                              # %if.then111
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	movabsq	$4278190080, %rcx       # imm = 0xFF000000
	andq	%rcx, %rax
	orq	%rsi, %rax
	movl	%eax, 56(%r12)
	jmp	.LBB12_35
.LBB12_28:                              # %if.else134
	testq	$1024, %rdx             # imm = 0x400
	je	.LBB12_35
# BB#29:                                # %land.lhs.true142
	movabsq	$17179869184, %rcx      # imm = 0x400000000
	testq	%rcx, %rdx
	jne	.LBB12_32
# BB#30:                                # %land.lhs.true142
	testq	%rax, %rax
	je	.LBB12_32
# BB#31:                                # %lor.lhs.false153
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB12_35
.LBB12_32:                              # %if.then161
	movl	56(%r12), %eax
	movq	%rax, %rcx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	$8, %rcx
	jb	.LBB12_34
# BB#33:                                # %if.then161
	movq	$8, %rcx
.LBB12_34:                              # %if.then161
	movabsq	$4278190080, %rsi       # imm = 0xFF000000
	andq	%rsi, %rax
	orq	%rcx, %rax
	movl	%eax, 56(%r12)
	movabsq	$-34359738369, %rax     # imm = 0xFFFFFFF7FFFFFFFF
	andq	%rax, %rdx
	movq	%rdx, 48(%r12)
.LBB12_35:                              # %if.end190
	cmpq	$37, %r14
	jne	.LBB12_60
# BB#36:                                # %land.lhs.true192
	testq	$4096, %rdx             # imm = 0x1000
	je	.LBB12_53
# BB#37:                                # %land.lhs.true200
	movq	32(%r15), %rax
	testq	%rax, %rax
	je	.LBB12_52
# BB#38:                                # %land.lhs.true204
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB12_52
# BB#39:                                # %land.lhs.true212
	movl	60(%r15), %eax
	movq	$7, %rcx
	shrq	%cl, %rax
	andq	$508, %rax              # imm = 0x1FC
	movl	mode_class(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB12_52
# BB#40:                                # %if.then219
	movq	40(%r12), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB12_52
# BB#41:                                # %lor.lhs.false.i
	movq	$1000, %rsi             # imm = 0x3E8
	movq	%rbx, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB12_52
# BB#42:                                # %if.else.i
	movq	32(%rbx), %rax
	movzbl	mode_bitsize+10(%rip), %ecx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rax
	cmpq	%rax, %rcx
	jb	.LBB12_52
# BB#43:                                # %if.end.i.i
	movl	class_narrowest_mode+4(%rip), %ebx
	jmp	.LBB12_44
.LBB12_46:                              # %for.inc.i.i
                                        #   in Loop: Header=BB12_44 Depth=1
	movzbl	mode_wider_mode(%rdx), %ebx
.LBB12_44:                              # %if.end.i.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB12_52
# BB#45:                                # %for.body.i.i
                                        #   in Loop: Header=BB12_44 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rax, %rcx
	jne	.LBB12_46
# BB#47:                                # %mode_for_size_tree.exit
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	$51, %rax
	je	.LBB12_52
# BB#48:                                # %land.lhs.true224
	movq	%rbx, %rdi
	callq	get_mode_alignment
	andq	%r14, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	andq	%r14, %rcx
	cmpq	%rcx, %rax
	ja	.LBB12_52
# BB#49:                                # %if.then227
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movl	56(%r12), %edx
	movq	%rax, %rcx
	andq	%r14, %rcx
	movq	%rdx, %rax
	andq	$16777215, %rax         # imm = 0xFFFFFF
	cmpq	%rax, %rcx
	jbe	.LBB12_51
# BB#50:                                # %cond.true235
	movq	%rbx, %rdi
	callq	get_mode_alignment
	movl	56(%r12), %edx
.LBB12_51:                              # %cond.end243
	andq	$16777215, %rax         # imm = 0xFFFFFF
	movabsq	$4278190080, %rcx       # imm = 0xFF000000
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movl	%ecx, 56(%r12)
	movq	48(%r12), %rax
	andq	$255, %rbx
	andq	$-4352, %rax            # imm = 0xFFFFFFFFFFFFEF00
	orq	%rbx, %rax
	movq	%rax, 48(%r12)
.LBB12_52:                              # %if.end264.land.lhs.true266_crit_edge
	movq	48(%r12), %rdx
.LBB12_53:                              # %land.lhs.true266
	testq	$4096, %rdx             # imm = 0x1000
	je	.LBB12_60
# BB#54:                                # %land.lhs.true274
	movl	60(%r15), %eax
	andq	$65024, %rax            # imm = 0xFE00
	cmpq	$26112, %rax            # imm = 0x6600
	jne	.LBB12_60
# BB#55:                                # %land.lhs.true274
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$51, %rax
	jne	.LBB12_60
# BB#56:                                # %land.lhs.true288
	movl	64(%r15), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rsi           # 8-byte Reload
	andq	%rcx, %rsi
	cmpq	%rsi, %rax
	ja	.LBB12_60
# BB#57:                                # %land.lhs.true292
	movl	56(%r12), %ecx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	%rax, %rcx
	jb	.LBB12_60
# BB#58:                                # %land.lhs.true301
	cmpq	$0, 64(%r12)
	je	.LBB12_60
# BB#59:                                # %if.then305
	andq	$-4097, %rdx            # imm = 0xFFFFFFFFFFFFEFFF
	movq	%rdx, 48(%r12)
.LBB12_60:                              # %if.end310
	movq	40(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB12_63
# BB#61:                                # %land.lhs.true314
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	je	.LBB12_63
# BB#62:                                # %if.then322
	callq	variable_size
	movq	%rax, 40(%r12)
.LBB12_63:                              # %if.end328
	movq	64(%r12), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB12_67
# BB#64:                                # %land.lhs.true332
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB12_66
# BB#65:
	movq	%rdi, %rbx
	jmp	.LBB12_67
.LBB12_66:                              # %if.then340
	callq	variable_size
	movq	%rax, %rbx
	movq	%rbx, 64(%r12)
.LBB12_67:                              # %if.end346
	movl	warn_larger_than(%rip), %eax
	testq	%rax, %rax
	je	.LBB12_77
# BB#68:                                # %if.end346
	cmpq	$34, %r13
	jne	.LBB12_77
# BB#69:                                # %land.lhs.true352
	movzbl	49(%r12), %eax
	testq	$1, %rax
	jne	.LBB12_77
# BB#70:                                # %land.lhs.true352
	testq	%rbx, %rbx
	je	.LBB12_77
# BB#71:                                # %land.lhs.true364
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB12_77
# BB#72:                                # %land.lhs.true370
	movq	larger_than_size(%rip), %rsi
	movq	%rbx, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_77
# BB#73:                                # %if.then373
	movq	32(%rbx), %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r14, %rsi
	andq	%r15, %rsi
	movq	%rbx, %rdi
	callq	compare_tree_int
	testq	%r15, %rax
	je	.LBB12_74
# BB#76:                                # %if.else380
	movq	larger_than_size(%rip), %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	jmp	.LBB12_75
.LBB12_77:                              # %if.end383
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB12_74:                              # %if.then379
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r14, %rdx
.LBB12_75:                              # %if.then379
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	warning_with_decl  # TAILCALL
.LBB12_78:                              # %if.then12
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.layout_decl, %rcx
	movq	$346, %rsi              # imm = 0x15A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp71:
	.size	layout_decl, .Ltmp71-layout_decl
	.cfi_endproc

	.globl	set_lang_adjust_rli
	.align	16, 0x90
	.type	set_lang_adjust_rli,@function
set_lang_adjust_rli:                    # @set_lang_adjust_rli
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, lang_adjust_rli(%rip)
	retq
.Ltmp72:
	.size	set_lang_adjust_rli, .Ltmp72-set_lang_adjust_rli
	.cfi_endproc

	.globl	start_record_layout
	.align	16, 0x90
	.type	start_record_layout,@function
start_record_layout:                    # @start_record_layout
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp75:
	.cfi_def_cfa_offset 16
.Ltmp76:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$72, %rdi
	callq	xmalloc
	movq	%rbx, (%rax)
	movl	64(%rbx), %edx
	cmpq	$8, %rdx
	movq	$8, %rcx
	jb	.LBB14_2
# BB#1:                                 # %entry
	movq	%rdx, %rcx
.LBB14_2:                               # %entry
	cmpq	$128, %rcx
	movl	%ecx, 32(%rax)
	movl	%ecx, 40(%rax)
	movl	%ecx, 36(%rax)
	ja	.LBB14_4
# BB#3:                                 # %entry
	movq	$128, %rcx
.LBB14_4:                               # %entry
	movl	%ecx, 16(%rax)
	movq	global_trees+120(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	global_trees+136(%rip), %rcx
	movq	%rcx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	popq	%rbx
	retq
.Ltmp77:
	.size	start_record_layout, .Ltmp77-start_record_layout
	.cfi_endproc

	.globl	bit_from_pos
	.align	16, 0x90
	.type	bit_from_pos,@function
bit_from_pos:                           # @bit_from_pos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp80:
	.cfi_def_cfa_offset 16
.Ltmp81:
	.cfi_offset %rbx, -16
	movq	sizetype_tab+24(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %rcx
	movq	%rax, %rdi
	movq	%rcx, %rsi
	callq	convert
	movq	global_trees+152(%rip), %rdx
	movq	$61, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	size_binop  # TAILCALL
.Ltmp82:
	.size	bit_from_pos, .Ltmp82-bit_from_pos
	.cfi_endproc

	.globl	byte_from_pos
	.align	16, 0x90
	.type	byte_from_pos,@function
byte_from_pos:                          # @byte_from_pos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp88:
	.cfi_def_cfa_offset 32
.Ltmp89:
	.cfi_offset %rbx, -24
.Ltmp90:
	.cfi_offset %r14, -16
	movq	sizetype_tab(%rip), %rbx
	movq	global_trees+152(%rip), %rdx
	movq	%rdi, %r14
	movq	$62, %rdi
	callq	size_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	size_binop  # TAILCALL
.Ltmp91:
	.size	byte_from_pos, .Ltmp91-byte_from_pos
	.cfi_endproc

	.globl	pos_from_byte
	.align	16, 0x90
	.type	pos_from_byte,@function
pos_from_byte:                          # @pos_from_byte
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp98:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp99:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp100:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp101:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 48
.Ltmp103:
	.cfi_offset %rbx, -48
.Ltmp104:
	.cfi_offset %r12, -40
.Ltmp105:
	.cfi_offset %r13, -32
.Ltmp106:
	.cfi_offset %r14, -24
.Ltmp107:
	.cfi_offset %r15, -16
	movq	%rcx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	movabsq	$4294967288, %rbx       # imm = 0xFFFFFFF8
	movq	sizetype_tab(%rip), %r13
	andq	%rdx, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$64, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r13
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$61, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, (%r12)
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$68, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	global_trees+152(%rip), %rdx
	movq	$61, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	%rax, (%r14)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp108:
	.size	pos_from_byte, .Ltmp108-pos_from_byte
	.cfi_endproc

	.globl	pos_from_bit
	.align	16, 0x90
	.type	pos_from_bit,@function
pos_from_bit:                           # @pos_from_bit
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp115:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp116:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp117:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp118:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp119:
	.cfi_def_cfa_offset 48
.Ltmp120:
	.cfi_offset %rbx, -48
.Ltmp121:
	.cfi_offset %r12, -40
.Ltmp122:
	.cfi_offset %r13, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movq	sizetype_tab(%rip), %r13
	movq	%rcx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rdx, %rbx
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$64, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r13
	movq	$3, %rcx
	movq	%rbx, %rdi
	shrq	%cl, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	$61, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, (%r12)
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$68, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, (%r14)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	pos_from_bit, .Ltmp125-pos_from_bit
	.cfi_endproc

	.globl	normalize_offset
	.align	16, 0x90
	.type	normalize_offset,@function
normalize_offset:                       # @normalize_offset
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp135:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp136:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp137:
	.cfi_def_cfa_offset 64
.Ltmp138:
	.cfi_offset %rbx, -48
.Ltmp139:
	.cfi_offset %r12, -40
.Ltmp140:
	.cfi_offset %r13, -32
.Ltmp141:
	.cfi_offset %r14, -24
.Ltmp142:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	(%r14), %rax
	movq	%rdx, %rbx
	movq	%rdi, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%rbx, %r12
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_2
# BB#1:                                 # %if.then
	movq	(%r14), %r13
	movq	$3, %rsi
	movq	%r12, %rdi
	callq	size_int_wide
	movq	$64, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	(%r15), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	sizetype_tab(%rip), %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r13
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	andq	%rax, %rbx
	movq	$3, %rcx
	shrq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	$61, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	$59, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, (%r15)
	movq	(%r14), %rbx
	movq	$3, %rsi
	movq	%r12, %rdi
	callq	size_int_wide
	movq	$68, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, (%r14)
.LBB19_2:                               # %if.end
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp143:
	.size	normalize_offset, .Ltmp143-normalize_offset
	.cfi_endproc

	.globl	debug_rli
	.align	16, 0x90
	.type	debug_rli,@function
debug_rli:                              # @debug_rli
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp148:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp149:
	.cfi_def_cfa_offset 32
.Ltmp150:
	.cfi_offset %rbx, -24
.Ltmp151:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	stderr(%rip), %r14
	movq	(%rbx), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	print_node_brief
	movq	8(%rbx), %rdx
	movabsq	$.L.str6, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	print_node_brief
	movq	24(%rbx), %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	print_node_brief
	movl	16(%rbx), %r9d
	movl	40(%rbx), %r8d
	movl	36(%rbx), %ecx
	movl	32(%rbx), %edx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movl	64(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB20_1
.LBB20_2:                               # %if.end
	cmpq	$0, 56(%rbx)
	je	.LBB20_3
# BB#4:                                 # %if.then3
	movabsq	$.L.str10, %rdi
	movq	$17, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	56(%rbx), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	debug_tree  # TAILCALL
.LBB20_3:                               # %if.end6
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB20_1:                               # %if.then
	movabsq	$.L.str9, %rdi
	movq	$24, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB20_2
.Ltmp152:
	.size	debug_rli, .Ltmp152-debug_rli
	.cfi_endproc

	.globl	normalize_rli
	.align	16, 0x90
	.type	normalize_rli,@function
normalize_rli:                          # @normalize_rli
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %edx
	leaq	8(%rdi), %rax
	leaq	24(%rdi), %rsi
	movq	%rax, %rdi
	jmp	normalize_offset  # TAILCALL
.Ltmp153:
	.size	normalize_rli, .Ltmp153-normalize_rli
	.cfi_endproc

	.globl	rli_size_unit_so_far
	.align	16, 0x90
	.type	rli_size_unit_so_far,@function
rli_size_unit_so_far:                   # @rli_size_unit_so_far
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp157:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp158:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp159:
	.cfi_def_cfa_offset 32
.Ltmp160:
	.cfi_offset %rbx, -24
.Ltmp161:
	.cfi_offset %r14, -16
	movq	8(%rdi), %r14
	movq	24(%rdi), %rsi
	movq	sizetype_tab(%rip), %rbx
	movq	global_trees+152(%rip), %rdx
	movq	$62, %rdi
	callq	size_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	size_binop  # TAILCALL
.Ltmp162:
	.size	rli_size_unit_so_far, .Ltmp162-rli_size_unit_so_far
	.cfi_endproc

	.globl	rli_size_so_far
	.align	16, 0x90
	.type	rli_size_so_far,@function
rli_size_so_far:                        # @rli_size_so_far
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp165:
	.cfi_def_cfa_offset 16
.Ltmp166:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rsi
	movq	24(%rdi), %rbx
	movq	sizetype_tab+24(%rip), %rdi
	callq	convert
	movq	global_trees+152(%rip), %rdx
	movq	$61, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	$59, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	popq	%rbx
	jmp	size_binop  # TAILCALL
.Ltmp167:
	.size	rli_size_so_far, .Ltmp167-rli_size_so_far
	.cfi_endproc

	.globl	place_field
	.align	16, 0x90
	.type	place_field,@function
place_field:                            # @place_field
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp174:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp175:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp176:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp177:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp178:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp179:
	.cfi_def_cfa_offset 96
.Ltmp180:
	.cfi_offset %rbx, -48
.Ltmp181:
	.cfi_offset %r12, -40
.Ltmp182:
	.cfi_offset %r13, -32
.Ltmp183:
	.cfi_offset %r14, -24
.Ltmp184:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movzbl	16(%r13), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB24_160
# BB#1:                                 # %lor.lhs.false
	movq	8(%r13), %rbx
	movzbl	16(%rbx), %ecx
	testq	%rcx, %rcx
	je	.LBB24_160
# BB#2:                                 # %if.end
	cmpq	$37, %rax
	jne	.LBB24_3
# BB#5:                                 # %if.else21
	movq	(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$20, %rax
	jne	.LBB24_6
# BB#23:                                # %if.end30
	movq	24(%r14), %rdi
	leaq	24(%r14), %r12
	callq	integer_zerop
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB24_24
# BB#25:                                # %if.else37
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	8(%r14), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	movq	$128, %rsi
	jne	.LBB24_29
# BB#26:                                # %if.else41
	movq	8(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_28
# BB#27:                                # %if.then45
	movq	8(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	8(%r14), %rdi
	movq	%r15, %rbx
	movq	%rax, %r15
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rsi
	negq	%rsi
	andq	%r15, %rsi
	movq	%rbx, %r15
	movq	$3, %rcx
	shlq	%cl, %rsi
	jmp	.LBB24_29
.LBB24_3:                               # %if.end
	cmpq	$34, %rax
	jne	.LBB24_160
# BB#4:                                 # %if.then13
	movq	56(%r14), %rdx
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	movq	%rax, 56(%r14)
	jmp	.LBB24_160
.LBB24_6:                               # %if.then27
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	layout_decl
	movq	global_trees+120(%rip), %rax
	movq	%rax, 88(%r13)
	movq	global_trees+136(%rip), %rax
	movq	%rax, 160(%r13)
	movq	$128, %rdi
	callq	exact_log2_wide
	movq	%rax, %rcx
	movl	56(%r13), %eax
	movb	%cl, 59(%r13)
	movq	48(%r13), %rdx
	andq	$16777215, %rax         # imm = 0xFFFFFF
	movabsq	$34359738368, %rcx      # imm = 0x800000000
	testq	%rcx, %rdx
	jne	.LBB24_8
# BB#7:                                 # %if.then.i
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	x86_field_alignment
	movq	48(%r13), %rdx
.LBB24_8:                               # %if.end.i
	movq	(%r14), %rsi
	movq	$4, %rcx
	movl	60(%rsi), %edi
	shrq	%cl, %rdx
	movabsq	$2147483648, %r15       # imm = 0x80000000
	andq	%r15, %rdx
	orq	%rdx, %rdi
	movl	%edi, 60(%rsi)
	movl	32(%r14), %ebx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%r12, %rsi
	cmpq	%rsi, %rbx
	ja	.LBB24_10
# BB#9:                                 # %if.end.i
	movq	%rax, %rbx
.LBB24_10:                              # %if.end.i
	movl	%ebx, 32(%r14)
	movl	40(%r14), %edx
	cmpq	%rsi, %rdx
	ja	.LBB24_12
# BB#11:                                # %if.end.i
	movq	%rax, %rdx
.LBB24_12:                              # %if.end.i
	movl	%edx, 40(%r14)
	cmpq	$0, 96(%r13)
	je	.LBB24_20
# BB#13:                                # %if.then38.i
	movq	8(%r13), %rsi
	movslq	60(%rsi), %rdi
	movl	64(%rsi), %eax
	testq	%rdi, %rdi
	js	.LBB24_15
# BB#14:                                # %if.then48.i
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	x86_field_alignment
	movl	32(%r14), %ebx
	movl	40(%r14), %edx
	movq	8(%r13), %rsi
.LBB24_15:                              # %if.end50.i
	movq	%rax, %rdi
	andq	%r12, %rdi
	movq	%rbx, %rcx
	andq	%r12, %rcx
	cmpq	%rdi, %rcx
	ja	.LBB24_17
# BB#16:                                # %if.end50.i
	movq	%rax, %rbx
.LBB24_17:                              # %if.end50.i
	movl	%ebx, 32(%r14)
	andq	%rdx, %r12
	cmpq	%rdi, %r12
	ja	.LBB24_19
# BB#18:                                # %if.end50.i
	movq	%rax, %rdx
.LBB24_19:                              # %if.end50.i
	movl	%edx, 40(%r14)
	movq	(%r14), %rax
	movl	60(%rsi), %ecx
	movl	60(%rax), %edx
	andq	%r15, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 60(%rax)
.LBB24_20:                              # %if.end84.i
	movq	(%r14), %rax
	movzbl	16(%rax), %eax
	cmpq	$22, %rax
	jne	.LBB24_21
# BB#158:                               # %if.then100.i
	movq	sizetype_tab(%rip), %rsi
	movq	64(%r13), %rcx
	movq	104(%r13), %rdx
	movq	8(%r14), %r8
	movq	$51, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	jmp	.LBB24_159
.LBB24_21:                              # %if.end84.i
	cmpq	$21, %rax
	jne	.LBB24_160
# BB#22:                                # %if.then90.i
	movq	8(%r14), %rsi
	movq	64(%r13), %rdx
	movq	$79, %rdi
	callq	size_binop
.LBB24_159:                             # %if.end759
	movq	%rax, 8(%r14)
	jmp	.LBB24_160
.LBB24_24:                              # %if.then32
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	(%r12), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	(%r12), %rdi
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	%r15, %rbx
	movq	%rax, %r15
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rsi
	negq	%rsi
	andq	%r15, %rsi
	movq	%rbx, %r15
	jmp	.LBB24_29
.LBB24_28:                              # %if.else53
	movl	16(%r14), %esi
.LBB24_29:                              # %if.end56
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	movl	56(%r13), %ebx
	movq	48(%r13), %r12
	movq	%r13, %rdi
	callq	layout_decl
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	48(%r13), %rax
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB24_31
# BB#30:                                # %if.then69
	movl	56(%r13), %esi
	movq	%rax, %rdx
.LBB24_31:                              # %if.end81
	andq	$16777215, %rsi         # imm = 0xFFFFFF
	movq	$35, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	32(%rsp), %r12          # 8-byte Reload
	je	.LBB24_33
# BB#32:
	movq	%rsi, %rbx
	jmp	.LBB24_34
.LBB24_33:                              # %if.then83
	movq	%r13, %rdi
	callq	x86_field_alignment
	movq	%rax, %rbx
.LBB24_34:                              # %if.end85
	movq	(%r14), %rdi
	callq	*targetm+296(%rip)
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %r12
	je	.LBB24_48
# BB#35:                                # %if.end85
	xorq	$1, %rax
	jne	.LBB24_48
# BB#36:                                # %land.lhs.true91
	cmpq	$0, 96(%r13)
	je	.LBB24_48
# BB#37:                                # %land.lhs.true94
	movq	32(%r12), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	jne	.LBB24_48
# BB#38:                                # %land.lhs.true98
	movq	40(%r13), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_48
# BB#39:                                # %if.then103
	movq	48(%r14), %rax
	testq	%rax, %rax
	je	.LBB24_40
# BB#41:                                # %land.lhs.true105
	cmpq	$0, 96(%rax)
	je	.LBB24_40
# BB#42:                                # %land.lhs.true110
	movq	40(%rax), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_43
.LBB24_40:
	movq	$1, %rsi
	movq	40(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB24_86
.LBB24_48:                              # %if.else134
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB24_79
# BB#49:                                # %land.lhs.true137
	movq	(%r14), %rdi
	callq	*targetm+296(%rip)
	testq	%rax, %rax
	jne	.LBB24_79
# BB#50:                                # %land.lhs.true140
	cmpq	$0, 96(%r13)
	je	.LBB24_79
# BB#51:                                # %land.lhs.true144
	movq	32(%r12), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_52
.LBB24_79:                              # %if.else265
	movl	32(%r14), %eax
	movq	%rbx, %rsi
	movq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	ja	.LBB24_81
# BB#80:                                # %if.else265
	movq	%rsi, %rax
.LBB24_81:                              # %if.else265
	movl	%eax, 32(%r14)
	movl	36(%r14), %eax
	movl	64(%r12), %ecx
	cmpq	%rcx, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	ja	.LBB24_83
# BB#82:                                # %if.else265
	movq	%rcx, %rax
.LBB24_83:                              # %if.else265
	movl	%eax, 36(%r14)
	movl	56(%r13), %ecx
	movl	40(%r14), %eax
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	%rcx, %rax
	ja	.LBB24_85
# BB#84:                                # %if.else265
	movq	%rcx, %rax
.LBB24_85:                              # %if.else265
	movl	%eax, 40(%r14)
.LBB24_86:                              # %if.end308
	movl	warn_packed(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_92
# BB#87:                                # %land.lhs.true310
	movzbl	49(%r13), %eax
	testq	$4, %rax
	je	.LBB24_92
# BB#88:                                # %if.then318
	movl	64(%r12), %eax
	movq	%rdx, %rcx
	andq	%r15, %rcx
	cmpq	%rax, %rcx
	jbe	.LBB24_91
# BB#89:                                # %if.then323
	movq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jbe	.LBB24_92
# BB#90:                                # %if.then328
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%rdx, %rbx
	callq	warning_with_decl
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rbx, %rdx
	jmp	.LBB24_92
.LBB24_91:                              # %if.else330
	movq	$1, %rax
	movl	%eax, 64(%r14)
.LBB24_92:                              # %if.end332
	movq	%rsi, %rbx
	andq	%r15, %rbx
	andq	%r15, %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	cmpq	%rbx, %rdx
	jae	.LBB24_100
# BB#93:                                # %if.then335
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movl	warn_padded(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_95
# BB#94:                                # %if.then337
	movabsq	$.L.str12, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	warning_with_decl
.LBB24_95:                              # %if.end338
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movl	16(%r14), %eax
	cmpq	%rax, %rbx
	jae	.LBB24_97
# BB#96:                                # %if.then342
	movq	24(%r14), %r15
	movq	$32, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	8(%r15), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	size_int_type_wide
	movq	%rax, %r12
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	%rax, 24(%r14)
	movq	8(%r14), %rax
	jmp	.LBB24_98
.LBB24_97:                              # %if.else346
	movq	sizetype_tab(%rip), %r15
	movq	8(%r14), %r12
	movq	24(%r14), %rsi
	movq	global_trees+152(%rip), %rdx
	movq	$63, %rdi
	callq	size_binop
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, %r15
	movq	%r15, 8(%r14)
	movq	global_trees+136(%rip), %rax
	movq	%rax, 24(%r14)
	movabsq	$4294967288, %rdi       # imm = 0xFFFFFFF8
	movq	8(%r15), %rsi
	movq	8(%rsp), %rax           # 8-byte Reload
	andq	%rax, %rdi
	movq	$3, %rcx
	shrq	%cl, %rdi
	callq	size_int_type_wide
	movq	%rax, %r12
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	%rax, 8(%r14)
.LBB24_98:                              # %if.end357
	movzbl	17(%rax), %eax
	testq	$2, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	32(%rsp), %r12          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB24_100
# BB#99:                                # %if.then364
	movl	%eax, 16(%r14)
.LBB24_100:                             # %if.end367
	movq	(%r14), %rdi
	callq	*targetm+296(%rip)
	testq	%rax, %rax
	jne	.LBB24_115
# BB#101:                               # %land.lhs.true370
	movzbl	16(%r13), %eax
	cmpq	$37, %rax
	jne	.LBB24_115
# BB#102:                               # %land.lhs.true370
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB24_115
# BB#103:                               # %land.lhs.true380
	movq	48(%r13), %rax
	testq	$4096, %rax             # imm = 0x1000
	je	.LBB24_115
# BB#104:                               # %land.lhs.true387
	testq	$1024, %rax             # imm = 0x400
	jne	.LBB24_115
# BB#105:                               # %land.lhs.true387
	movl	maximum_field_alignment(%rip), %eax
	testq	%r15, %rax
	jne	.LBB24_115
# BB#106:                               # %land.lhs.true398
	movq	40(%r13), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	jne	.LBB24_115
# BB#107:                               # %land.lhs.true403
	movq	40(%r13), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_115
# BB#108:                               # %land.lhs.true408
	movq	8(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_115
# BB#109:                               # %land.lhs.true412
	movq	32(%r12), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_115
# BB#110:                               # %if.then417
	movq	%r12, %rbx
	movl	64(%rbx), %r12d
	movq	40(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	8(%r14), %rdi
	movq	%rax, %r15
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	24(%r14), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movslq	60(%rbx), %rcx
	movq	%rax, %rbx
	testq	%rcx, %rcx
	js	.LBB24_112
# BB#111:                               # %if.then434
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	x86_field_alignment
	movq	%rax, %r12
.LBB24_112:                             # %if.end436
	movq	8(%rsp), %rax           # 8-byte Reload
	leaq	(%rbx,%rax,8), %rcx
	movq	%r12, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	addq	%rcx, %r15
	leaq	-1(%rbx,%r15), %rax
	cqto
	idivq	%rbx
	movq	%rax, %r15
	movq	%rcx, %rax
	cqto
	idivq	%rbx
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	32(%rcx), %rdi
	subq	%rax, %r15
	movq	$1, %rsi
	callq	tree_low_cst
	cqto
	idivq	%rbx
	cmpq	%rax, %r15
	jle	.LBB24_114
# BB#113:                               # %if.then456
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %r15
	movq	$32, %rcx
	movq	8(%r15), %rsi
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r12, %rdi
	callq	size_int_type_wide
	movq	%rax, %r12
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	%rax, (%rbx)
.LBB24_114:                             # %if.end460
	movq	32(%rsp), %r12          # 8-byte Reload
	movl	60(%r12), %eax
	movq	$31, %rcx
	shrq	%cl, %rax
	orq	%rax, 16(%rsp)          # 8-byte Folded Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB24_115:                             # %if.end466
	movq	(%r14), %rdi
	callq	*targetm+296(%rip)
	testq	%rax, %rax
	je	.LBB24_143
# BB#116:                               # %land.lhs.true470
	movzbl	16(%r13), %eax
	cmpq	$37, %rax
	jne	.LBB24_143
# BB#117:                               # %land.lhs.true470
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB24_143
# BB#118:                               # %land.lhs.true480
	movzbl	49(%r13), %eax
	testq	$4, %rax
	jne	.LBB24_143
# BB#119:                               # %land.lhs.true488
	cmpq	$0, 48(%r14)
	je	.LBB24_143
# BB#120:                               # %land.lhs.true491
	movq	40(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB24_143
# BB#121:                               # %land.lhs.true495
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_143
# BB#122:                               # %land.lhs.true500
	movq	48(%r14), %rax
	movq	40(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB24_143
# BB#123:                               # %land.lhs.true505
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_143
# BB#124:                               # %land.lhs.true511
	movq	8(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_143
# BB#125:                               # %land.lhs.true515
	movq	32(%r12), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_143
# BB#126:                               # %land.lhs.true520
	movq	48(%r14), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_143
# BB#127:                               # %land.lhs.true528
	movq	48(%r14), %rax
	cmpq	$0, 96(%rax)
	je	.LBB24_129
# BB#128:                               # %land.lhs.true533
	movq	40(%rax), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_131
.LBB24_129:                             # %lor.lhs.false539
	cmpq	$0, 96(%r13)
	je	.LBB24_143
# BB#130:                               # %land.lhs.true543
	movq	40(%r13), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	jne	.LBB24_143
.LBB24_131:                             # %land.lhs.true548
	movq	48(%r14), %rax
	movq	8(%rax), %rax
	movq	32(%r12), %rcx
	movq	32(%rax), %rax
	movq	%rcx, %rdi
	movq	%rax, %rsi
	callq	simple_cst_equal
	testq	%r15, %rax
	je	.LBB24_134
# BB#132:                               # %lor.lhs.false558
	movq	48(%r14), %rax
	cmpq	$0, 96(%rax)
	je	.LBB24_143
# BB#133:                               # %land.lhs.true563
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_143
.LBB24_134:                             # %if.then569
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	48(%r14), %rax
	movl	64(%r12), %ebx
	testq	%rax, %rax
	je	.LBB24_139
# BB#135:                               # %land.lhs.true575
	cmpq	$0, 96(%rax)
	je	.LBB24_139
# BB#136:                               # %land.lhs.true580
	movq	40(%rax), %rax
	movq	%rax, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB24_139
# BB#137:                               # %if.then586
	movq	48(%r14), %rax
	movq	8(%rax), %rax
	movl	64(%rax), %eax
	movq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	ja	.LBB24_139
# BB#138:                               # %if.then586
	movq	%rax, %rbx
.LBB24_139:                             # %if.end603
	movl	maximum_field_alignment(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_142
# BB#140:                               # %if.then606
	movq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB24_142
# BB#141:                               # %if.then606
	movq	%rax, %rbx
.LBB24_142:                             # %if.end613
	movq	%r15, %r12
	movq	(%r12), %r15
	movq	$32, %rcx
	movq	8(%r15), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	size_int_type_wide
	movq	%rax, %rbx
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	%rax, (%r12)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %r15
.LBB24_143:                             # %if.end617
	movl	16(%r14), %edx
	leaq	8(%r14), %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	normalize_offset
	movq	8(%r14), %rax
	movq	%rax, 88(%r13)
	movq	24(%r14), %rax
	movq	%rax, 160(%r13)
	movl	16(%r14), %eax
	movq	%rax, %rdi
	negq	%rdi
	andq	%rax, %rdi
	andq	%r15, %rdi
	callq	exact_log2_wide
	movb	%al, 59(%r13)
	movq	(%r14), %rax
	movl	60(%rax), %edx
	movq	$31, %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	orq	%rdx, %rsi
	movl	%esi, 60(%rax)
	movq	160(%r13), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_144
# BB#145:                               # %if.else662
	movq	88(%r13), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	movq	$128, %rbx
	jne	.LBB24_150
# BB#146:                               # %if.else668
	movq	88(%r13), %rdi
	movq	$1, %rsi
	movq	$1, %rbx
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB24_148
# BB#147:                               # %if.then673
	movq	88(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	88(%r13), %rdi
	movq	%rax, %r12
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rbx
	negq	%rbx
	andq	%r12, %rbx
	movq	$3, %rcx
	jmp	.LBB24_149
.LBB24_144:                             # %if.then650
	movq	160(%r13), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	160(%r13), %rdi
	movq	%rax, %r12
	movq	$1, %rsi
	callq	tree_low_cst
	movq	%rax, %rbx
	negq	%rbx
	andq	%r12, %rbx
	jmp	.LBB24_150
.LBB24_148:                             # %if.else684
	movzbl	59(%r13), %ecx
.LBB24_149:                             # %if.end693
	shlq	%cl, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
.LBB24_150:                             # %if.end693
	andq	%rbx, %r15
	cmpq	%r15, 40(%rsp)          # 8-byte Folded Reload
	movq	24(%rsp), %r15          # 8-byte Reload
	je	.LBB24_152
# BB#151:                               # %if.then696
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	layout_decl
.LBB24_152:                             # %if.end697
	movq	%r13, 48(%r14)
	movq	40(%r13), %rdx
	testq	%rdx, %rdx
	je	.LBB24_160
# BB#153:                               # %if.else704
	movq	64(%r13), %rax
	movl	16(%rax), %eax
	andq	$262399, %rax           # imm = 0x400FF
	cmpq	$25, %rax
	jne	.LBB24_154
# BB#157:                               # %if.else752
	movq	24(%r14), %rsi
	movq	$59, %rdi
	callq	size_binop
	movq	%rax, 24(%r14)
	movl	16(%r14), %edx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	normalize_offset  # TAILCALL
.LBB24_154:                             # %if.then720
	movq	sizetype_tab(%rip), %r15
	movq	8(%r14), %r12
	movq	24(%r14), %rsi
	movq	global_trees+152(%rip), %rdx
	movq	$63, %rdi
	callq	size_binop
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, 8(%r14)
	movq	64(%r13), %rdx
	movq	$59, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	%rax, 8(%r14)
	movq	global_trees+136(%rip), %rax
	movq	%rax, 24(%r14)
	movl	56(%r13), %ecx
	movl	16(%r14), %eax
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	%rcx, %rax
	jb	.LBB24_156
# BB#155:                               # %if.then720
	movq	%rcx, %rax
.LBB24_156:                             # %if.then720
	movl	%eax, 16(%r14)
.LBB24_160:                             # %if.end759
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB24_52:                              # %if.then149
	movq	40(%r13), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_53
# BB#56:                                # %if.else164
	movzbl	49(%r13), %eax
	testq	$4, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rsi
	jne	.LBB24_58
# BB#57:                                # %if.then172
	movl	64(%r12), %esi
	jmp	.LBB24_58
.LBB24_53:                              # %if.then154
	movl	32(%r14), %eax
	movq	%rbx, %rsi
	movq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	ja	.LBB24_55
# BB#54:                                # %if.then154
	movq	%rsi, %rax
.LBB24_55:                              # %if.then154
	movl	%eax, 32(%r14)
.LBB24_58:                              # %if.end176
	cmpq	$0, 72(%r13)
	je	.LBB24_86
# BB#59:                                # %if.then180
	movslq	60(%r12), %rcx
	movl	64(%r12), %eax
	testq	%rcx, %rcx
	js	.LBB24_60
# BB#61:                                # %if.then188
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rbx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	x86_field_alignment
	jmp	.LBB24_62
.LBB24_60:
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rbx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
.LBB24_62:                              # %if.end190
	movl	maximum_field_alignment(%rip), %ecx
	testq	%rcx, %rcx
	je	.LBB24_66
# BB#63:                                # %if.then193
	movq	%rax, %rdx
	andq	%r15, %rdx
	cmpq	%rcx, %rdx
	jb	.LBB24_65
# BB#64:                                # %if.then193
	movq	%rcx, %rax
.LBB24_65:                              # %if.then193
	movq	%rbx, %r12
	jmp	.LBB24_70
.LBB24_66:                              # %if.else200
	movzbl	49(%r13), %ecx
	testq	$4, %rcx
	je	.LBB24_67
# BB#68:                                # %if.then208
	movq	%rax, %rcx
	andq	%r15, %rcx
	cmpq	$8, %rcx
	movq	%rbx, %r12
	jb	.LBB24_70
# BB#69:                                # %if.then208
	movq	$8, %rax
	jmp	.LBB24_70
.LBB24_43:                              # %if.then116
	movl	32(%r14), %eax
	movq	%rbx, %rsi
	movq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	ja	.LBB24_45
# BB#44:                                # %if.then116
	movq	%rsi, %rax
.LBB24_45:                              # %if.then116
	movl	%eax, 32(%r14)
	movl	36(%r14), %eax
	movl	64(%r12), %ecx
	cmpq	%rcx, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	ja	.LBB24_47
# BB#46:                                # %if.then116
	movq	%rcx, %rax
.LBB24_47:                              # %if.then116
	movl	%eax, 36(%r14)
	jmp	.LBB24_86
.LBB24_67:
	movq	%rbx, %r12
.LBB24_70:                              # %if.end216
	movl	32(%r14), %ecx
	movq	%rax, %rdx
	andq	%r15, %rdx
	cmpq	%rdx, %rcx
	movq	8(%rsp), %rsi           # 8-byte Reload
	ja	.LBB24_72
# BB#71:                                # %if.end216
	movq	%rax, %rcx
.LBB24_72:                              # %if.end216
	movl	%ecx, 32(%r14)
	movl	56(%r13), %ecx
	movl	40(%r14), %eax
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	cmpq	%rcx, %rax
	movq	40(%rsp), %rdx          # 8-byte Reload
	ja	.LBB24_74
# BB#73:                                # %if.end216
	movq	%rcx, %rax
.LBB24_74:                              # %if.end216
	movl	%eax, 40(%r14)
	movl	warn_packed(%rip), %eax
	testq	%rax, %rax
	je	.LBB24_78
# BB#75:                                # %if.then245
	movl	36(%r14), %eax
	movl	64(%r12), %ecx
	cmpq	%rcx, %rax
	ja	.LBB24_77
# BB#76:                                # %if.then245
	movq	%rcx, %rax
.LBB24_77:                              # %if.then245
	movl	%eax, 36(%r14)
.LBB24_78:                              # %if.end259
	movl	60(%r12), %eax
	movq	$31, %rcx
	shrq	%cl, %rax
	orq	%rax, 16(%rsp)          # 8-byte Folded Spill
	jmp	.LBB24_86
.Ltmp185:
	.size	place_field, .Ltmp185-place_field
	.cfi_endproc

	.globl	compute_record_mode
	.align	16, 0x90
	.type	compute_record_mode,@function
compute_record_mode:                    # @compute_record_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp192:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp193:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp194:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp195:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp196:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp197:
	.cfi_def_cfa_offset 64
.Ltmp198:
	.cfi_offset %rbx, -48
.Ltmp199:
	.cfi_offset %r12, -40
.Ltmp200:
	.cfi_offset %r13, -32
.Ltmp201:
	.cfi_offset %r14, -24
.Ltmp202:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movl	60(%r14), %eax
	movabsq	$4294902271, %rcx       # imm = 0xFFFF01FF
	andq	%rax, %rcx
	orq	$26112, %rcx            # imm = 0x6600
	movl	%ecx, 60(%r14)
	movq	32(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB25_25
# BB#1:                                 # %if.end
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB25_16
# BB#2:                                 # %for.body.lr.ph
	movq	$0, (%rsp)              # 8-byte Folded Spill
	movabsq	$4294967264, %r13       # imm = 0xFFFFFFE0
	.align	16, 0x90
.LBB25_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$37, %rax
	jne	.LBB25_13
# BB#4:                                 # %if.end9
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB25_25
# BB#5:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB25_3 Depth=1
	movl	60(%rax), %eax
	andq	$196096, %rax           # imm = 0x2FE00
	cmpq	$26112, %rax            # imm = 0x6600
	je	.LBB25_25
# BB#6:                                 # %lor.lhs.false31
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	%rbx, %rdi
	callq	bit_position
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB25_25
# BB#7:                                 # %lor.lhs.false35
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB25_25
# BB#8:                                 # %lor.lhs.false38
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	$1, %rsi
	callq	host_integerp
	testq	%r15, %rax
	je	.LBB25_25
# BB#9:                                 # %if.end44
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	%rbx, %rdi
	callq	int_bit_position
	movq	%r15, %r14
	movq	%rax, %r15
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%r13, %rsi
	xorq	%rdx, %rdx
                                        # kill: RAX<def> R15<kill>
	divq	%rsi
	movq	%rax, %r12
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movl	target_flags(%rip), %esi
	leaq	-1(%r15,%rax), %rax
	movq	%r14, %r15
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%r13, %rsi
	xorq	%rdx, %rdx
	divq	%rsi
	cmpq	%rax, %r12
	je	.LBB25_11
# BB#10:                                # %land.lhs.true57
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movl	target_flags(%rip), %esi
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%r13, %rsi
	cqto
	idivq	%rsi
	testq	%rdx, %rdx
	jne	.LBB25_25
.LBB25_11:                              # %if.end68
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	32(%r14), %rdi
	movq	40(%rbx), %rsi
	callq	simple_cst_equal
	testq	%r15, %rax
	je	.LBB25_13
# BB#12:                                # %if.then75
                                        #   in Loop: Header=BB25_3 Depth=1
	movzbl	48(%rbx), %eax
	movq	%rax, (%rsp)            # 8-byte Spill
.LBB25_13:                              # %for.inc
                                        #   in Loop: Header=BB25_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB25_3
# BB#14:                                # %for.end
	movzbl	16(%r14), %eax
	cmpq	$20, %rax
	movq	(%rsp), %rax            # 8-byte Reload
	jne	.LBB25_16
# BB#15:                                # %for.end
	testq	%r15, %rax
	jne	.LBB25_24
.LBB25_16:                              # %if.else
	movq	32(%r14), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB25_23
# BB#17:                                # %lor.lhs.false.i
	movq	$1000, %rsi             # imm = 0x3E8
	movq	%rbx, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB25_23
# BB#18:                                # %if.else.i
	movq	32(%rbx), %rdx
	movzbl	mode_bitsize+10(%rip), %eax
	andq	%r15, %rdx
	cmpq	%rdx, %rax
	jb	.LBB25_23
# BB#19:                                # %if.end.i.i
	movl	class_narrowest_mode+4(%rip), %eax
	jmp	.LBB25_20
.LBB25_22:                              # %for.inc.i.i
                                        #   in Loop: Header=BB25_20 Depth=1
	movzbl	mode_wider_mode(%rsi), %eax
.LBB25_20:                              # %if.end.i.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB25_23
# BB#21:                                # %for.body.i.i
                                        #   in Loop: Header=BB25_20 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB25_22
	jmp	.LBB25_24
.LBB25_23:
	movq	$51, %rax
.LBB25_24:                              # %mode_for_size_tree.exit
	movl	60(%r14), %ecx
	andq	$127, %rax
	shlq	$9, %rax
	andq	$-65025, %rcx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rax, %rcx
	movl	%ecx, 60(%r14)
.LBB25_25:                              # %if.end142
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp203:
	.size	compute_record_mode, .Ltmp203-compute_record_mode
	.cfi_endproc

	.globl	finish_record_layout
	.align	16, 0x90
	.type	finish_record_layout,@function
finish_record_layout:                   # @finish_record_layout
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp210:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp211:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp212:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp213:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp214:
	.cfi_def_cfa_offset 48
.Ltmp215:
	.cfi_offset %rbx, -48
.Ltmp216:
	.cfi_offset %r12, -40
.Ltmp217:
	.cfi_offset %r13, -32
.Ltmp218:
	.cfi_offset %r14, -24
.Ltmp219:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$8, %rax
	movl	%eax, 16(%rbx)
	leaq	8(%rbx), %rdi
	leaq	24(%rbx), %rsi
	movq	$8, %rdx
	callq	normalize_offset
	movq	(%rbx), %rax
	movl	64(%rax), %ecx
	movl	32(%rbx), %edx
	cmpq	%rdx, %rcx
	ja	.LBB26_2
# BB#1:                                 # %entry
	movq	%rdx, %rcx
.LBB26_2:                               # %entry
	movl	%ecx, 64(%rax)
	movq	8(%rbx), %rsi
	movq	24(%rbx), %r14
	movq	sizetype_tab+24(%rip), %rdi
	callq	convert
	movq	global_trees+152(%rip), %rdx
	movq	$61, %rdi
	movq	%rax, %rsi
	callq	size_binop
	movq	$59, %rdi
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	8(%rbx), %r15
	movq	24(%rbx), %rsi
	movq	sizetype_tab(%rip), %r12
	movq	global_trees+152(%rip), %rdx
	movq	%rax, %r14
	movq	$62, %rdi
	callq	size_binop
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	24(%rbx), %rdi
	movq	%rax, %r15
	callq	integer_zerop
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB26_4
# BB#3:                                 # %if.then.i
	movq	global_trees+128(%rip), %rdx
	movq	$59, %rdi
	movq	%r15, %rsi
	callq	size_binop
	movq	%rax, %r15
.LBB26_4:                               # %if.end.i
	movq	(%rbx), %rax
	movq	136(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB26_7
# BB#5:                                 # %land.lhs.true.i
	movslq	24(%rcx), %rdx
	cmpq	$7, %rdx
	jl	.LBB26_7
# BB#6:                                 # %if.then18.i
	movq	%r14, 80(%rcx)
	movq	(%rbx), %rax
	movq	136(%rax), %rax
	movq	%r15, 88(%rax)
	movq	(%rbx), %rax
.LBB26_7:                               # %if.end29.i
	movslq	64(%rax), %rdi
	movq	8(%r14), %rsi
	callq	size_int_type_wide
	movq	%rax, %r12
	movq	$63, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	(%rbx), %rcx
	movq	%rax, 32(%rcx)
	movl	64(%rcx), %edi
	movq	8(%r15), %rsi
	movq	$3, %rcx
	shrq	%cl, %rdi
	callq	size_int_type_wide
	movq	%rax, %r12
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r12, %rdx
	callq	size_binop
	movq	(%rbx), %rcx
	movq	%rax, 40(%rcx)
	movl	warn_padded(%rip), %eax
	testq	%rax, %rax
	je	.LBB26_11
# BB#8:                                 # %land.lhs.true43.i
	movzbl	17(%r14), %eax
	testq	$2, %rax
	je	.LBB26_11
# BB#9:                                 # %land.lhs.true45.i
	movq	32(%rcx), %rsi
	movq	%r14, %rdi
	callq	simple_cst_equal
	testq	%r13, %rax
	jne	.LBB26_11
# BB#10:                                # %if.then51.i
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB26_11:                              # %if.end52.i
	movl	warn_packed(%rip), %eax
	testq	%rax, %rax
	je	.LBB26_24
# BB#12:                                # %land.lhs.true54.i
	movq	(%rbx), %rax
	movzbl	16(%rax), %ecx
	cmpq	$20, %rcx
	jne	.LBB26_24
# BB#13:                                # %land.lhs.true60.i
	movzbl	62(%rax), %ecx
	testq	$16, %rcx
	je	.LBB26_24
# BB#14:                                # %land.lhs.true67.i
	movl	64(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB26_24
# BB#15:                                # %land.lhs.true69.i
	movzbl	17(%r14), %ecx
	testq	$2, %rcx
	je	.LBB26_24
# BB#16:                                # %if.then76.i
	movl	64(%rax), %edi
	movl	36(%rbx), %ecx
	cmpq	%rcx, %rdi
	ja	.LBB26_18
# BB#17:                                # %if.then76.i
	movq	%rcx, %rdi
.LBB26_18:                              # %if.then76.i
	movl	%edi, 36(%rbx)
	movq	32(%rax), %r14
	movq	$32, %rcx
	movq	8(%r14), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	size_int_type_wide
	movq	%rax, %r15
	movq	$63, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	(%rbx), %rcx
	movq	32(%rcx), %rsi
	movq	%rax, %rdi
	callq	simple_cst_equal
	testq	%r13, %rax
	je	.LBB26_24
# BB#19:                                # %if.then100.i
	movq	(%rbx), %rax
	movl	60(%rax), %ecx
	movabsq	$4293918719, %rdx       # imm = 0xFFEFFFFF
	andq	%rcx, %rdx
	movl	%edx, 60(%rax)
	movq	(%rbx), %rax
	movq	96(%rax), %rax
	testq	%rax, %rax
	je	.LBB26_23
# BB#20:                                # %if.then109.i
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB26_22
# BB#21:                                # %if.else.i
	movq	72(%rax), %rax
.LBB26_22:                              # %if.end130.i
	movq	32(%rax), %rsi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	warning
	jmp	.LBB26_24
.LBB26_23:                              # %if.else131.i
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	warning
.LBB26_24:                              # %finalize_record_size.exit
	movq	(%rbx), %rdi
	callq	compute_record_mode
	movq	(%rbx), %rdi
	callq	finalize_type_size
	movq	56(%rbx), %rax
	testq	%rax, %rax
	je	.LBB26_26
	.align	16, 0x90
.LBB26_25:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	layout_decl
	movq	56(%rbx), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, 56(%rbx)
	jne	.LBB26_25
.LBB26_26:                              # %while.end
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp220:
	.size	finish_record_layout, .Ltmp220-finish_record_layout
	.cfi_endproc

	.align	16, 0x90
	.type	finalize_type_size,@function
finalize_type_size:                     # @finalize_type_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp225:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp226:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp227:
	.cfi_def_cfa_offset 32
.Ltmp228:
	.cfi_offset %rbx, -32
.Ltmp229:
	.cfi_offset %r14, -24
.Ltmp230:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	60(%r14), %edi
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	je	.LBB27_5
# BB#1:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	cmpq	$51, %rax
	je	.LBB27_5
# BB#2:                                 # %land.lhs.true8
	movzbl	16(%r14), %ecx
	cmpq	$22, %rcx
	ja	.LBB27_4
# BB#3:                                 # %land.lhs.true8
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7602176, %rax          # imm = 0x740000
	jne	.LBB27_5
.LBB27_4:                               # %if.then
	callq	get_mode_alignment
	movl	%eax, 64(%r14)
	movl	60(%r14), %eax
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	movl	%eax, 60(%r14)
.LBB27_5:                               # %if.end
	movq	40(%r14), %rax
	testq	%rax, %rax
	jne	.LBB27_8
# BB#6:                                 # %land.lhs.true41
	movq	32(%r14), %rsi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB27_8
# BB#7:                                 # %if.then44
	movq	sizetype_tab(%rip), %rbx
	movq	global_trees+152(%rip), %rdx
	movq	$64, %rdi
	callq	size_binop
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, 40(%r14)
.LBB27_8:                               # %if.end51
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB27_12
# BB#9:                                 # %if.end70
	movslq	64(%r14), %rdi
	movq	8(%rbx), %rsi
	callq	size_int_type_wide
	movq	%rax, %r15
	movq	$63, %rdi
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	size_binop
	movq	%rax, 32(%r14)
	movq	40(%r14), %r15
	movl	64(%r14), %edi
	movq	8(%r15), %rsi
	movq	$3, %rcx
	shrq	%cl, %rdi
	callq	size_int_type_wide
	movq	%rax, %rbx
	movq	$63, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	$61, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	size_binop
	movq	%rax, 40(%r14)
	movq	32(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB27_12
# BB#10:                                # %land.lhs.true74
	movzbl	16(%rdi), %ecx
	cmpq	$25, %rcx
	je	.LBB27_12
# BB#11:                                # %if.then82
	callq	variable_size
	movq	%rax, 32(%r14)
	movq	40(%r14), %rax
.LBB27_12:                              # %if.end88
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB27_16
# BB#13:                                # %land.lhs.true92
	movzbl	16(%rax), %ecx
	cmpq	$25, %rcx
	jne	.LBB27_15
# BB#14:
	movq	%rax, %rcx
	jmp	.LBB27_16
.LBB27_15:                              # %if.then100
	movq	%rax, %rdi
	callq	variable_size
	movq	%rax, %rcx
	movq	%rcx, 40(%r14)
.LBB27_16:                              # %if.end106
	cmpq	$0, 120(%r14)
	movq	128(%r14), %rax
	jne	.LBB27_18
# BB#17:                                # %if.end106
	cmpq	%r14, %rax
	je	.LBB27_21
.LBB27_18:                              # %if.then110
	testq	%rax, %rax
	je	.LBB27_21
# BB#19:                                # %for.body.lr.ph
	movq	32(%r14), %rdx
	movl	60(%r14), %esi
	movl	64(%r14), %edi
	andq	$-2147418624, %rsi      # imm = 0xFFFFFFFF8000FE00
	.align	16, 0x90
.LBB27_20:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, 32(%rax)
	movq	%rcx, 40(%rax)
	movl	%edi, 64(%rax)
	movl	60(%rax), %ebx
	andq	$2147418623, %rbx       # imm = 0x7FFF01FF
	orq	%rsi, %rbx
	movl	%ebx, 60(%rax)
	movq	120(%rax), %rax
	testq	%rax, %rax
	jne	.LBB27_20
.LBB27_21:                              # %if.end153
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp231:
	.size	finalize_type_size, .Ltmp231-finalize_type_size
	.cfi_endproc

	.globl	layout_type
	.align	16, 0x90
	.type	layout_type,@function
layout_type:                            # @layout_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp240:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp241:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp242:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp243:
	.cfi_def_cfa_offset 64
.Ltmp244:
	.cfi_offset %rbx, -48
.Ltmp245:
	.cfi_offset %r12, -40
.Ltmp246:
	.cfi_offset %r13, -32
.Ltmp247:
	.cfi_offset %r14, -24
.Ltmp248:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB28_1
# BB#3:                                 # %if.end
	cmpq	$0, 32(%r13)
	jne	.LBB28_128
# BB#4:                                 # %if.end3
	movl	16(%r13), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	leaq	-5(%rcx), %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rdx
	cmpq	$19, %rdx
	ja	.LBB28_118
# BB#5:                                 # %if.end3
	jmpq	*.LJTI28_0(,%rdx,8)
.LBB28_89:                              # %sw.bb554
	movq	$72, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r13, (%r14)
	movl	64(%r13), %ecx
	cmpq	$8, %rcx
	movq	$8, %rax
	jb	.LBB28_91
# BB#90:                                # %sw.bb554
	movq	%rcx, %rax
.LBB28_91:                              # %sw.bb554
	cmpq	$128, %rax
	movl	%eax, 32(%r14)
	movl	%eax, 40(%r14)
	movl	%eax, 36(%r14)
	ja	.LBB28_93
# BB#92:                                # %sw.bb554
	movq	$128, %rax
.LBB28_93:                              # %sw.bb554
	movl	%eax, 16(%r14)
	movq	global_trees+120(%rip), %rax
	movq	%rax, 8(%r14)
	movq	global_trees+136(%rip), %rax
	movq	%rax, 24(%r14)
	xorq	%rax, %rax
	movl	%eax, 64(%r14)
	movq	$0, 56(%r14)
	movq	$0, 48(%r14)
	movzbl	16(%r13), %eax
	movq	24(%r13), %rbx
	cmpq	$22, %rax
	jne	.LBB28_96
# BB#94:                                # %if.then562
	movq	%rbx, %rdi
	callq	nreverse
	movq	%rax, %rbx
	movq	%rbx, 24(%r13)
	jmp	.LBB28_96
	.align	16, 0x90
.LBB28_95:                              # %for.body
                                        #   in Loop: Header=BB28_96 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	place_field
	movq	(%rbx), %rbx
.LBB28_96:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB28_95
# BB#97:                                # %for.end
	movzbl	16(%r13), %eax
	cmpq	$22, %rax
	jne	.LBB28_99
# BB#98:                                # %if.then579
	movq	24(%r13), %rdi
	callq	nreverse
	movq	%rax, 24(%r13)
.LBB28_99:                              # %if.end585
	movq	lang_adjust_rli(%rip), %rax
	testq	%rax, %rax
	je	.LBB28_101
# BB#100:                               # %if.then587
	movq	%r14, %rdi
	callq	*%rax
.LBB28_101:                             # %if.end588
	movq	%r14, %rdi
	callq	finish_record_layout
	jmp	.LBB28_121
.LBB28_48:                              # %sw.bb252
	movl	target_flags(%rip), %eax
	andq	$33554432, %rax         # imm = 0x2000000
	cmpq	$15, %rcx
	jne	.LBB28_50
# BB#49:                                # %sw.bb252
	movzbl	reference_types_internal(%rip), %ecx
	testq	$1, %rcx
	je	.LBB28_50
# BB#51:                                # %cond.end
	movq	%rax, %rdx
	andq	%r12, %rdx
	movq	$24, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	orq	$8, %rsi
	andq	$254, %rsi
	movzbl	mode_bitsize(%rsi), %ebx
	movq	$20, %rcx
	shrq	%cl, %rdx
	addq	$32, %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rbx
	jne	.LBB28_53
# BB#52:                                # %cond.true276
	movl	ptr_mode(%rip), %eax
	jmp	.LBB28_54
.LBB28_42:                              # %sw.bb222
	movl	target_flags(%rip), %edx
	movl	class_narrowest_mode+4(%rip), %eax
	movq	$19, %rcx
	shrq	%cl, %rdx
	testq	%rax, %rax
	je	.LBB28_46
# BB#43:
	andq	$64, %rdx
	addq	$64, %rdx
	andq	%r12, %rdx
	.align	16, 0x90
.LBB28_44:                              # %for.body.i967
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB28_47
# BB#45:                                # %for.inc.i971
                                        #   in Loop: Header=BB28_44 Depth=1
	movzbl	mode_wider_mode(%rsi), %eax
	testq	%rax, %rax
	jne	.LBB28_44
.LBB28_46:
	movq	$51, %rax
.LBB28_47:                              # %mode_for_size.exit973
	movl	60(%r13), %ecx
	andq	$127, %rax
	shlq	$9, %rax
	movabsq	$4294902271, %rdx       # imm = 0xFFFF01FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 60(%r13)
	movl	target_flags(%rip), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$64, %rax
	addq	$64, %rax
	movabsq	$4294967232, %rdi       # imm = 0xFFFFFFC0
	andq	%rax, %rdi
	movq	$3, %rsi
	callq	size_int_wide
	movq	%rax, 32(%r13)
	movl	target_flags(%rip), %edi
	movq	$19, %rcx
	shrq	%cl, %rdi
	andq	$64, %rdi
	addq	$64, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	jmp	.LBB28_19
.LBB28_118:                             # %sw.default
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.layout_type, %rdx
	movq	$1702, %rsi             # imm = 0x6A6
	callq	fancy_abort
.LBB28_119:                             # %sw.bb188
	movq	$1, %rax
	movl	%eax, 64(%r13)
	movl	60(%r13), %eax
	andq	$2147418623, %rax       # imm = 0x7FFF01FF
	jmp	.LBB28_120
.LBB28_20:                              # %sw.bb57
	movl	class_narrowest_mode+8(%rip), %edx
	movl	60(%r13), %eax
	testq	%rdx, %rdx
	je	.LBB28_24
# BB#21:
	movq	%rax, %rsi
	andq	$511, %rsi              # imm = 0x1FF
	andq	%r12, %rsi
.LBB28_22:                              # %for.body.i916
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	cmpq	%rsi, %rcx
	je	.LBB28_25
# BB#23:                                # %for.inc.i917
                                        #   in Loop: Header=BB28_22 Depth=1
	movzbl	mode_wider_mode(%rdi), %edx
	testq	%rdx, %rdx
	jne	.LBB28_22
.LBB28_24:
	movq	$51, %rdx
.LBB28_25:                              # %mode_for_size.exit
	andq	$127, %rdx
	movq	%rdx, %rcx
	shlq	$9, %rcx
	movabsq	$4294902271, %rsi       # imm = 0xFFFF01FF
	movzbl	mode_bitsize(%rdx,%rdx), %edi
	andq	%rsi, %rax
	orq	%rcx, %rax
	movl	%eax, 60(%r13)
	jmp	.LBB28_18
.LBB28_26:                              # %sw.bb92
	movq	8(%r13), %rcx
	movl	16(%rcx), %edx
	andq	$8192, %rdx             # imm = 0x2000
	movabsq	$4294959103, %rsi       # imm = 0xFFFFDFFF
	andq	%rsi, %rax
	orq	%rdx, %rax
	movl	%eax, 16(%r13)
	movl	60(%rcx), %eax
	movzbl	16(%rcx), %ecx
	addq	%rax, %rax
	andq	$1022, %rax             # imm = 0x3FE
	cmpq	$6, %rcx
	je	.LBB28_27
# BB#28:                                # %sw.bb92
	movabsq	$1, %rcx
	jmp	.LBB28_29
.LBB28_35:                              # %sw.bb151
	movq	8(%r13), %rcx
	movl	16(%rcx), %ecx
	andq	$8192, %rcx             # imm = 0x2000
	movabsq	$4294959103, %rdx       # imm = 0xFFFFDFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, 16(%r13)
	movl	60(%r13), %eax
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$254, %rax
	movzbl	mode_bitsize(%rax), %edi
	jmp	.LBB28_18
.LBB28_7:                               # %sw.bb4
	movl	60(%r13), %eax
	testq	$511, %rax              # imm = 0x1FF
	jne	.LBB28_9
# BB#8:                                 # %if.then9
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rcx, %rax
	orq	$1, %rax
	movl	%eax, 60(%r13)
.LBB28_9:                               # %sw.bb15
	movq	104(%r13), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$25, %rax
	jne	.LBB28_12
# BB#10:                                # %land.lhs.true
	callq	tree_int_cst_sgn
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB28_12
# BB#11:                                # %if.then25
	movl	16(%r13), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r13)
.LBB28_12:                              # %if.end30
	movl	class_narrowest_mode+4(%rip), %eax
	testq	%rax, %rax
	je	.LBB28_16
# BB#13:
	movl	60(%r13), %edx
	movq	%rdx, %rsi
	andq	$511, %rsi              # imm = 0x1FF
	andq	%r12, %rsi
	.align	16, 0x90
.LBB28_14:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	mode_bitsize(%rdi,%rdi), %ecx
	cmpq	%rsi, %rcx
	jae	.LBB28_17
# BB#15:                                # %for.inc.i
                                        #   in Loop: Header=BB28_14 Depth=1
	movzbl	mode_wider_mode(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB28_14
.LBB28_16:                              # %for.end.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.smallest_mode_for_size, %rcx
	movq	$259, %rsi              # imm = 0x103
	jmp	.LBB28_2
.LBB28_17:                              # %smallest_mode_for_size.exit
	andq	$127, %rax
	movq	%rax, %rcx
	shlq	$9, %rcx
	movabsq	$4294902271, %rsi       # imm = 0xFFFF01FF
	movzbl	mode_bitsize(%rax,%rax), %edi
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, 60(%r13)
	jmp	.LBB28_18
.LBB28_36:                              # %sw.bb197
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4294967264, %rdi       # imm = 0xFFFFFFE0
	andq	%rax, %rdi
	movq	$3, %rsi
	callq	size_int_wide
	movq	%rax, 32(%r13)
	movl	target_flags(%rip), %edi
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, 40(%r13)
	movl	target_flags(%rip), %edx
	movl	class_narrowest_mode+4(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rdx
	testq	%rax, %rax
	je	.LBB28_40
# BB#37:
	andq	$32, %rdx
	addq	$32, %rdx
	andq	%r12, %rdx
.LBB28_38:                              # %for.body.i953
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rdx, %rcx
	je	.LBB28_41
# BB#39:                                # %for.inc.i957
                                        #   in Loop: Header=BB28_38 Depth=1
	movzbl	mode_wider_mode(%rsi), %eax
	testq	%rax, %rax
	jne	.LBB28_38
.LBB28_40:
	movq	$51, %rax
.LBB28_41:                              # %mode_for_size.exit959
	movl	60(%r13), %ecx
	andq	$127, %rax
	shlq	$9, %rax
	movabsq	$4294902271, %rdx       # imm = 0xFFFF01FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 60(%r13)
	jmp	.LBB28_121
.LBB28_117:                             # %sw.bb679
	movq	$128, %rax
	movl	%eax, 64(%r13)
	movl	60(%r13), %eax
	andq	$2147418623, %rax       # imm = 0x7FFF01FF
	orq	$26112, %rax            # imm = 0x6600
.LBB28_120:                             # %sw.epilog
	movl	%eax, 60(%r13)
	jmp	.LBB28_121
.LBB28_55:                              # %sw.bb310
	movq	8(%r13), %r14
	movq	24(%r13), %rbx
	movq	%r14, %rdi
	callq	build_pointer_type
	testq	%rbx, %rbx
	je	.LBB28_73
# BB#56:                                # %land.lhs.true316
	movq	112(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB28_73
# BB#57:                                # %land.lhs.true319
	movq	104(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB28_73
# BB#58:                                # %land.lhs.true323
	cmpq	$0, 32(%r14)
	je	.LBB28_73
# BB#59:                                # %if.then327
	movq	global_trees+128(%rip), %r15
	movq	sizetype_tab(%rip), %rbx
	movq	8(%rcx), %rsi
	movq	$60, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	$59, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movzbl	62(%r13), %ecx
	movq	32(%r14), %rbx
	movq	%rax, %r15
	testq	$16, %rcx
	je	.LBB28_70
# BB#60:                                # %land.lhs.true345
	movzbl	16(%r14), %ecx
	cmpq	$12, %rcx
	ja	.LBB28_70
# BB#61:                                # %land.lhs.true345
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7232, %rax             # imm = 0x1C40
	je	.LBB28_70
# BB#62:                                # %land.lhs.true372
	movq	112(%r14), %rdi
	callq	integer_zerop
	testq	%r12, %rax
	jne	.LBB28_64
# BB#63:                                # %lor.lhs.false377
	movq	112(%r14), %rdi
	callq	integer_onep
	testq	%r12, %rax
	je	.LBB28_70
.LBB28_64:                              # %land.lhs.true382
	movq	104(%r14), %rdi
	movq	$1, %rsi
	callq	host_integerp
	testq	%r12, %rax
	je	.LBB28_70
# BB#65:                                # %if.then387
	movq	112(%r14), %rdi
	movq	$1, %rsi
	callq	tree_low_cst
	movq	104(%r14), %rdi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rsi
	callq	tree_low_cst
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rdx, %rcx
	subq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB28_66
# BB#67:                                # %if.then387
	movabsq	$1, %rax
	jmp	.LBB28_68
.LBB28_102:                             # %sw.bb589
	movq	24(%r13), %rcx
	movq	112(%rcx), %rax
	movzbl	16(%rax), %edx
	cmpq	$25, %rdx
	jne	.LBB28_104
# BB#103:                               # %lor.lhs.false600
	movq	104(%rcx), %rdx
	movzbl	16(%rdx), %ecx
	cmpq	$25, %rcx
	jne	.LBB28_104
# BB#105:                               # %if.else612
	movl	set_alignment(%rip), %r14d
	testq	%r14, %r14
	jne	.LBB28_107
# BB#106:                               # %cond.false615
	movl	target_flags(%rip), %r14d
	movq	$20, %rcx
	shrq	%cl, %r14
	andq	$32, %r14
	addq	$32, %r14
.LBB28_107:                             # %cond.end619
	movq	32(%rax), %r15
	movq	32(%rdx), %rax
	incq	%r15
	subq	%rax, %r15
	movq	$32, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%r14, %rsi
	andq	%r12, %rsi
	movq	%r12, %rcx
	leaq	-1(%r14,%r15), %r12
	movq	%r12, %rax
	andq	%rcx, %rax
	xorq	%rdx, %rdx
	divq	%rsi
	subq	%rdx, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdi, %rax
	jle	.LBB28_109
# BB#108:                               # %if.then641
	movl	60(%r13), %eax
	movabsq	$4294902271, %rcx       # imm = 0xFFFF01FF
	andq	%rax, %rcx
	orq	$26112, %rcx            # imm = 0x6600
	movl	%ecx, 60(%r13)
	jmp	.LBB28_116
.LBB28_50:                              # %cond.end.thread
	movabsq	$4293918720, %rcx       # imm = 0xFFF00000
	andq	%rcx, %rax
	movq	$20, %rcx
	shrq	%cl, %rax
	addq	$32, %rax
	movq	%rax, %rbx
	movl	ptr_mode(%rip), %eax
	jmp	.LBB28_54
.LBB28_53:                              # %cond.false277
	movabsq	$4127195136, %rcx       # imm = 0xF6000000
	andq	%rcx, %rax
	movq	$25, %rcx
	shrq	%cl, %rax
	orq	$4, %rax
.LBB28_54:                              # %cond.end281
	movl	60(%r13), %ecx
	andq	$127, %rax
	shlq	$9, %rax
	movabsq	$4294902271, %rdx       # imm = 0xFFFF01FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 60(%r13)
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	%rax, 32(%r13)
	movq	$60, %rcx
	movq	%rbx, %rdi
	shrq	%cl, %rdi
	andq	$7, %rdi
	addq	%rbx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$35, %rcx
	sarq	%cl, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, 40(%r13)
	movl	16(%r13), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%r13)
	movl	60(%r13), %eax
	andq	$511, %rbx              # imm = 0x1FF
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rax, %rcx
	orq	%rbx, %rcx
	movl	%ecx, 60(%r13)
	jmp	.LBB28_121
.LBB28_27:
	movabsq	$0, %rcx
.LBB28_29:                              # %sw.bb92
	movl	class_narrowest_mode+20(,%rcx,4), %edx
	testq	%rdx, %rdx
	je	.LBB28_33
# BB#30:
	andq	%r12, %rax
.LBB28_31:                              # %for.body.i924
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rax, %rcx
	je	.LBB28_34
# BB#32:                                # %for.inc.i928
                                        #   in Loop: Header=BB28_31 Depth=1
	movzbl	mode_wider_mode(%rsi), %edx
	testq	%rdx, %rdx
	jne	.LBB28_31
.LBB28_33:
	movq	$51, %rdx
.LBB28_34:                              # %mode_for_size.exit930
	movl	60(%r13), %eax
	andq	$127, %rdx
	movq	%rdx, %rcx
	shlq	$9, %rcx
	movabsq	$4294902271, %rsi       # imm = 0xFFFF01FF
	movzbl	mode_bitsize(%rdx,%rdx), %edi
	andq	%rax, %rsi
	orq	%rcx, %rsi
	movl	%esi, 60(%r13)
.LBB28_18:                              # %sw.epilog
	movq	$3, %rsi
	callq	size_int_wide
	movq	%rax, 32(%r13)
	movl	60(%r13), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_size(%rax), %edi
.LBB28_19:                              # %sw.epilog
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, 40(%r13)
.LBB28_121:                             # %sw.epilog
	movzbl	16(%r13), %eax
	addq	$-20, %rax
	andq	%r12, %rax
	cmpq	$3, %rax
	jb	.LBB28_123
# BB#122:                               # %if.then711
	movq	%r13, %rdi
	callq	finalize_type_size
.LBB28_123:                             # %if.end712
	movzbl	sizetype_set(%rip), %eax
	testq	%rax, %rax
	jne	.LBB28_125
# BB#124:                               # %if.then714
	movq	early_type_list(%rip), %rdx
	xorq	%rdi, %rdi
	movq	%r13, %rsi
	callq	tree_cons
	movq	%rax, early_type_list(%rip)
.LBB28_125:                             # %if.end716
	movzbl	16(%r13), %eax
	addq	$-18, %rax
	andq	%r12, %rax
	cmpq	$4, %rax
	ja	.LBB28_128
# BB#126:                               # %land.lhs.true751
	movq	152(%r13), %rax
	cmpq	$-1, %rax
	je	.LBB28_128
# BB#127:                               # %if.then755
	movq	$0, 152(%r13)
.LBB28_128:                             # %if.end758
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB28_109:                             # %if.else647
	movzbl	mode_bitsize+10(%rip), %eax
	cmpq	%rsi, %rax
	jb	.LBB28_114
# BB#110:                               # %if.end.i
	movl	class_narrowest_mode+4(%rip), %eax
	jmp	.LBB28_111
.LBB28_113:                             # %for.inc.i943
                                        #   in Loop: Header=BB28_111 Depth=1
	movzbl	mode_wider_mode(%rdx), %eax
.LBB28_111:                             # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB28_114
# BB#112:                               # %for.body.i939
                                        #   in Loop: Header=BB28_111 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
	cmpq	%rsi, %rcx
	jne	.LBB28_113
	jmp	.LBB28_115
.LBB28_114:
	movq	$51, %rax
.LBB28_115:                             # %mode_for_size.exit945
	movl	60(%r13), %ecx
	andq	$127, %rax
	shlq	$9, %rax
	movabsq	$4294902271, %rdx       # imm = 0xFFFF01FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 60(%r13)
.LBB28_116:                             # %if.end656
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	%rax, 32(%r13)
	movq	$60, %rcx
	shrq	%cl, %rbx
	andq	$7, %rbx
	addq	%r12, %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$35, %rcx
	sarq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	size_int_wide
	movq	%rax, 40(%r13)
	movl	%r14d, 64(%r13)
	movl	60(%r13), %eax
	andq	$511, %r15              # imm = 0x1FF
	andq	$2147483136, %rax       # imm = 0x7FFFFE00
	orq	%r15, %rax
	movl	%eax, 60(%r13)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB28_121
.LBB28_66:
	movabsq	$0, %rax
.LBB28_68:                              # %if.then387
	cmpq	$2, %rdx
	sbbq	%rcx, %rcx
	testq	%rax, %rcx
	je	.LBB28_70
# BB#69:
	movq	global_trees+96(%rip), %rbx
.LBB28_70:                              # %if.end404
	movq	sizetype_tab+24(%rip), %rdi
	movq	%r15, %rsi
	callq	convert
	movq	$61, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	size_binop
	movq	%rax, 32(%r13)
	cmpq	$0, 40(%r14)
	je	.LBB28_73
# BB#71:                                # %land.lhs.true413
	movq	%rbx, %rdi
	callq	integer_onep
	testq	%r12, %rax
	jne	.LBB28_73
# BB#72:                                # %if.then416
	movq	40(%r14), %rsi
	movq	$61, %rdi
	movq	%r15, %rdx
	callq	size_binop
	movq	%rax, 40(%r13)
.LBB28_73:                              # %if.end423
	movl	64(%r14), %eax
	cmpq	$8, %rax
	ja	.LBB28_75
# BB#74:                                # %if.end423
	movq	$8, %rax
.LBB28_75:                              # %if.end423
	movl	%eax, 64(%r13)
	movl	60(%r14), %eax
	movl	60(%r13), %ecx
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	andq	%rax, %rdx
	andq	$2147418623, %rcx       # imm = 0x7FFF01FF
	orq	%rdx, %rcx
	orq	$26112, %rcx            # imm = 0x6600
	movl	%ecx, 60(%r13)
	movq	32(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB28_121
# BB#76:                                # %land.lhs.true456
	movq	8(%r13), %rax
	movl	60(%rax), %ecx
	andq	$196096, %rcx           # imm = 0x2FE00
	cmpq	$26112, %rcx            # imm = 0x6600
	je	.LBB28_121
# BB#77:                                # %if.then474
	movq	32(%rax), %rsi
	callq	simple_cst_equal
	testq	%r12, %rax
	je	.LBB28_79
# BB#78:                                # %if.then483
	movq	8(%r13), %rax
	movl	60(%rax), %eax
	movl	60(%r13), %ecx
	andq	$65024, %rax            # imm = 0xFE00
	jmp	.LBB28_88
.LBB28_79:                              # %if.else
	movq	32(%r13), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB28_86
# BB#80:                                # %lor.lhs.false.i
	movq	$1000, %rsi             # imm = 0x3E8
	movq	%rbx, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB28_86
# BB#81:                                # %if.else.i
	movq	32(%rbx), %rdx
	movzbl	mode_bitsize+10(%rip), %eax
	andq	%r12, %rdx
	cmpq	%rdx, %rax
	jb	.LBB28_86
# BB#82:                                # %if.end.i.i
	movl	class_narrowest_mode+4(%rip), %eax
	jmp	.LBB28_83
.LBB28_85:                              # %for.inc.i.i
                                        #   in Loop: Header=BB28_83 Depth=1
	movzbl	mode_wider_mode(%rsi), %eax
.LBB28_83:                              # %if.end.i.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB28_86
# BB#84:                                # %for.body.i.i
                                        #   in Loop: Header=BB28_83 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB28_85
	jmp	.LBB28_87
.LBB28_86:
	movq	$51, %rax
.LBB28_87:                              # %mode_for_size_tree.exit
	movl	60(%r13), %ecx
	andq	$127, %rax
	shlq	$9, %rax
.LBB28_88:                              # %sw.epilog
	andq	$-65025, %rcx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rax, %rcx
	movl	%ecx, 60(%r13)
	jmp	.LBB28_121
.LBB28_1:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.layout_type, %rcx
	movq	$1422, %rsi             # imm = 0x58E
	jmp	.LBB28_2
.LBB28_6:                               # %sw.bb
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.layout_type, %rcx
	movq	$1433, %rsi             # imm = 0x599
	jmp	.LBB28_2
.LBB28_104:                             # %if.then611
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.layout_type, %rcx
	movq	$1666, %rsi             # imm = 0x682
.LBB28_2:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp249:
	.size	layout_type, .Ltmp249-layout_type
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI28_0:
	.quad	.LBB28_119
	.quad	.LBB28_9
	.quad	.LBB28_20
	.quad	.LBB28_26
	.quad	.LBB28_35
	.quad	.LBB28_9
	.quad	.LBB28_7
	.quad	.LBB28_9
	.quad	.LBB28_48
	.quad	.LBB28_36
	.quad	.LBB28_48
	.quad	.LBB28_42
	.quad	.LBB28_117
	.quad	.LBB28_55
	.quad	.LBB28_102
	.quad	.LBB28_89
	.quad	.LBB28_89
	.quad	.LBB28_89
	.quad	.LBB28_42
	.quad	.LBB28_6

	.text
	.globl	make_signed_type
	.align	16, 0x90
	.type	make_signed_type,@function
make_signed_type:                       # @make_signed_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp256:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp257:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp258:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp259:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp260:
	.cfi_def_cfa_offset 48
.Ltmp261:
	.cfi_offset %rbx, -48
.Ltmp262:
	.cfi_offset %r12, -40
.Ltmp263:
	.cfi_offset %r13, -32
.Ltmp264:
	.cfi_offset %r14, -24
.Ltmp265:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %r15
	movl	60(%r15), %eax
	andq	$511, %rbx              # imm = 0x1FF
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rax, %rcx
	orq	%rbx, %rcx
	cmpq	$128, %rbx
	movl	%ecx, 60(%r15)
	movq	$128, %r14
	ja	.LBB29_2
# BB#1:                                 # %entry
	movq	%rbx, %r14
.LBB29_2:                               # %entry
	leaq	-64(%r14), %rbx
	xorq	%rdi, %rdi
	testq	%rbx, %rbx
	jg	.LBB29_4
# BB#3:                                 # %cond.false.i
	leaq	-1(%r14), %rcx
	movq	$-1, %rdi
	shlq	%cl, %rdi
.LBB29_4:                               # %cond.end.i
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-65(%r14), %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %r12
	movq	$-1, %rsi
	movq	%r13, %rcx
	shlq	%cl, %rsi
	testq	%rax, %rax
	jg	.LBB29_6
# BB#5:                                 # %cond.end.i
	movq	$-1, %rsi
.LBB29_6:                               # %cond.end.i
	callq	build_int_2_wide
	testq	%rbx, %rbx
	movq	%rax, 104(%r15)
	jg	.LBB29_8
# BB#7:                                 # %cond.false20.i
	decq	%r14
	movq	$1, %r12
	movq	%r14, %rcx
	shlq	%cl, %r12
	decq	%r12
.LBB29_8:                               # %cond.end25.i
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB29_10
# BB#9:                                 # %cond.true30.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$1, %rsi
	movq	%r13, %rcx
	shlq	%cl, %rsi
	decq	%rsi
.LBB29_10:                              # %fixup_signed_type.exit
	movq	%r12, %rdi
	callq	build_int_2_wide
	movq	%rax, 112(%r15)
	movq	104(%r15), %rcx
	movq	%r15, 8(%rcx)
	movq	%r15, 8(%rax)
	movq	%r15, %rdi
	callq	layout_type
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp266:
	.size	make_signed_type, .Ltmp266-make_signed_type
	.cfi_endproc

	.globl	fixup_signed_type
	.align	16, 0x90
	.type	fixup_signed_type,@function
fixup_signed_type:                      # @fixup_signed_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp273:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp274:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp275:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp276:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp277:
	.cfi_def_cfa_offset 48
.Ltmp278:
	.cfi_offset %rbx, -48
.Ltmp279:
	.cfi_offset %r12, -40
.Ltmp280:
	.cfi_offset %r13, -32
.Ltmp281:
	.cfi_offset %r14, -24
.Ltmp282:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	60(%r15), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$128, %rax
	movq	$128, %r14
	ja	.LBB30_2
# BB#1:                                 # %entry
	movq	%rax, %r14
.LBB30_2:                               # %entry
	leaq	-64(%r14), %rbx
	xorq	%rdi, %rdi
	testq	%rbx, %rbx
	jg	.LBB30_4
# BB#3:                                 # %cond.false
	leaq	-1(%r14), %rcx
	movq	$-1, %rdi
	shlq	%cl, %rdi
.LBB30_4:                               # %cond.end
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	leaq	-65(%r14), %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %r12
	movq	$-1, %rsi
	movq	%r13, %rcx
	shlq	%cl, %rsi
	testq	%rax, %rax
	jg	.LBB30_6
# BB#5:                                 # %cond.end
	movq	$-1, %rsi
.LBB30_6:                               # %cond.end
	callq	build_int_2_wide
	testq	%rbx, %rbx
	movq	%rax, 104(%r15)
	jg	.LBB30_8
# BB#7:                                 # %cond.false20
	decq	%r14
	movq	$1, %r12
	movq	%r14, %rcx
	shlq	%cl, %r12
	decq	%r12
.LBB30_8:                               # %cond.end25
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jle	.LBB30_10
# BB#9:                                 # %cond.true30
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$1, %rsi
	movq	%r13, %rcx
	shlq	%cl, %rsi
	decq	%rsi
.LBB30_10:                              # %cond.end37
	movq	%r12, %rdi
	callq	build_int_2_wide
	movq	%rax, 112(%r15)
	movq	104(%r15), %rcx
	movq	%r15, 8(%rcx)
	movq	%r15, 8(%rax)
	movq	%r15, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	layout_type  # TAILCALL
.Ltmp283:
	.size	fixup_signed_type, .Ltmp283-fixup_signed_type
	.cfi_endproc

	.globl	make_unsigned_type
	.align	16, 0x90
	.type	make_unsigned_type,@function
make_unsigned_type:                     # @make_unsigned_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp289:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp290:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp291:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp292:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp293:
	.cfi_def_cfa_offset 48
.Ltmp294:
	.cfi_offset %rbx, -40
.Ltmp295:
	.cfi_offset %r12, -32
.Ltmp296:
	.cfi_offset %r14, -24
.Ltmp297:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %r12
	movl	60(%r12), %eax
	andq	$511, %rbx              # imm = 0x1FF
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rax, %rcx
	orq	%rbx, %rcx
	cmpq	$128, %rbx
	movl	%ecx, 60(%r12)
	movq	$128, %r15
	ja	.LBB31_2
# BB#1:                                 # %entry
	movq	%rbx, %r15
.LBB31_2:                               # %entry
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	build_int_2_wide
	movq	%rax, 104(%r12)
	movq	%r15, %rax
	addq	$-64, %rax
	js	.LBB31_3
# BB#4:                                 # %cond.end.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rdi
	testq	%rax, %rax
	jle	.LBB31_6
# BB#5:                                 # %cond.true8.i
	movq	$128, %rax
	subq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdi
	movq	$-1, %r14
	shrq	%cl, %r14
	jmp	.LBB31_6
.LBB31_3:                               # %cond.end.thread.i
	movq	$1, %rdi
	movq	%r15, %rcx
	shlq	%cl, %rdi
	decq	%rdi
.LBB31_6:                               # %fixup_unsigned_type.exit
	movq	%r14, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%r12)
	movq	104(%r12), %rcx
	movq	%r12, 8(%rcx)
	movq	%r12, 8(%rax)
	movq	%r12, %rdi
	callq	layout_type
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp298:
	.size	make_unsigned_type, .Ltmp298-make_unsigned_type
	.cfi_endproc

	.globl	fixup_unsigned_type
	.align	16, 0x90
	.type	fixup_unsigned_type,@function
fixup_unsigned_type:                    # @fixup_unsigned_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp303:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp304:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp305:
	.cfi_def_cfa_offset 32
.Ltmp306:
	.cfi_offset %rbx, -32
.Ltmp307:
	.cfi_offset %r14, -24
.Ltmp308:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	60(%rbx), %eax
	andq	$511, %rax              # imm = 0x1FF
	cmpq	$128, %rax
	movq	$128, %r15
	ja	.LBB32_2
# BB#1:                                 # %entry
	movq	%rax, %r15
.LBB32_2:                               # %entry
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	build_int_2_wide
	movq	%rax, 104(%rbx)
	movq	%r15, %rax
	addq	$-64, %rax
	js	.LBB32_3
# BB#4:                                 # %cond.end
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rdi
	testq	%rax, %rax
	jle	.LBB32_6
# BB#5:                                 # %cond.true8
	movq	$128, %rax
	subq	%r15, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdi
	movq	$-1, %r14
	shrq	%cl, %r14
	jmp	.LBB32_6
.LBB32_3:                               # %cond.end.thread
	movq	$1, %rdi
	movq	%r15, %rcx
	shlq	%cl, %rdi
	decq	%rdi
.LBB32_6:                               # %cond.end13
	movq	%r14, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rbx)
	movq	104(%rbx), %rcx
	movq	%rbx, 8(%rcx)
	movq	%rbx, 8(%rax)
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	layout_type  # TAILCALL
.Ltmp309:
	.size	fixup_unsigned_type, .Ltmp309-fixup_unsigned_type
	.cfi_endproc

	.globl	initialize_sizetypes
	.align	16, 0x90
	.type	initialize_sizetypes,@function
initialize_sizetypes:                   # @initialize_sizetypes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp313:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp314:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp315:
	.cfi_def_cfa_offset 32
.Ltmp316:
	.cfi_offset %rbx, -24
.Ltmp317:
	.cfi_offset %r14, -16
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%rbx, integer_types+40(%rip)
	movl	60(%rbx), %eax
	movabsq	$4294902271, %rcx       # imm = 0xFFFF01FF
	andq	%rax, %rcx
	orq	$2048, %rcx             # imm = 0x800
	movl	%ecx, 60(%rbx)
	movq	$4, %rdi
	callq	get_mode_alignment
	movl	%eax, 64(%rbx)
	movl	60(%rbx), %eax
	movzbl	mode_bitsize+8(%rip), %r14d
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	movl	%eax, 60(%rbx)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	build_int_2_wide
	movzbl	mode_size+4(%rip), %edi
	movq	%rax, 32(%rbx)
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, 40(%rbx)
	movl	16(%rbx), %eax
	orq	$8192, %rax             # imm = 0x2000
	movl	%eax, 16(%rbx)
	movl	60(%rbx), %eax
	andq	$511, %r14              # imm = 0x1FF
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rax, %rcx
	orq	%r14, %rcx
	movl	%ecx, 60(%rbx)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, 104(%rbx)
	movl	60(%rbx), %eax
	orq	$131072, %rax           # imm = 0x20000
	movl	%eax, 60(%rbx)
	movq	$1000, %rdi             # imm = 0x3E8
	xorq	%rsi, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rbx)
	movq	%rbx, sizetype_tab(%rip)
	movq	%rbx, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab+24(%rip)
	movq	$0, integer_types+40(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp318:
	.size	initialize_sizetypes, .Ltmp318-initialize_sizetypes
	.cfi_endproc

	.globl	set_sizetype
	.align	16, 0x90
	.type	set_sizetype,@function
set_sizetype:                           # @set_sizetype
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp325:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp326:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp327:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp328:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp329:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp330:
	.cfi_def_cfa_offset 80
.Ltmp331:
	.cfi_offset %rbx, -48
.Ltmp332:
	.cfi_offset %r12, -40
.Ltmp333:
	.cfi_offset %r13, -32
.Ltmp334:
	.cfi_offset %r14, -24
.Ltmp335:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	60(%rbx), %r14d
	andq	$511, %r14              # imm = 0x1FF
	leaq	4(%r14), %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r15, %rax
	andq	%rcx, %rax
	cmpq	$128, %rax
	jb	.LBB34_2
# BB#1:                                 # %entry
	movq	$128, %r15
.LBB34_2:                               # %entry
	movzbl	sizetype_set(%rip), %eax
	cmpq	$1, %rax
	je	.LBB34_3
# BB#5:                                 # %if.end
	movq	%rbx, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab(%rip)
	movq	%rbx, 24(%rax)
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	$6, %rdi
	callq	make_node
	movq	%rax, sizetype_tab+24(%rip)
	movq	96(%rbx), %rcx
	movq	%rcx, 96(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movl	60(%rax), %ecx
	movq	%r15, %rdx
	movq	%r15, 16(%rsp)          # 8-byte Spill
	andq	$511, %rdx              # imm = 0x1FF
	movabsq	$4294966784, %r15       # imm = 0xFFFFFE00
	andq	%r15, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+24(%rip), %r12
	movl	60(%r12), %ecx
	movzbl	17(%rbx), %eax
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	$128, %rcx
	movq	$128, %r13
	ja	.LBB34_7
# BB#6:                                 # %if.end
	movq	%rcx, %r13
.LBB34_7:                               # %if.end
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	testq	$32, %rax
	je	.LBB34_13
# BB#8:                                 # %if.then27
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	build_int_2_wide
	movq	%rax, 104(%r12)
	leaq	-64(%r13), %rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB34_9
# BB#10:                                # %cond.end.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rdi
	testq	%rax, %rax
	jle	.LBB34_12
# BB#11:                                # %cond.true8.i
	movq	$128, %rcx
	subq	%r13, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	movq	$-1, %rdi
	movq	$-1, %r14
	shrq	%cl, %r14
.LBB34_12:                              # %fixup_unsigned_type.exit
	movq	%r14, %rsi
	jmp	.LBB34_21
.LBB34_13:                              # %if.else
	leaq	-64(%r13), %r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rdi, %rdi
	testq	%rax, %rax
	jg	.LBB34_15
# BB#14:                                # %cond.false.i
	leaq	-1(%r13), %rcx
	movq	$-1, %rdi
	shlq	%cl, %rdi
.LBB34_15:                              # %cond.end.i126
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	-65(%r13), %r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rsi
	movq	%r14, %rcx
	shlq	%cl, %rsi
	testq	%rax, %rax
	jg	.LBB34_17
# BB#16:                                # %cond.end.i126
	movq	$-1, %rsi
.LBB34_17:                              # %cond.end.i126
	callq	build_int_2_wide
	testq	%r15, %r15
	movq	%rax, 104(%r12)
	movq	$-1, %rdi
	jg	.LBB34_19
# BB#18:                                # %cond.false20.i
	decq	%r13
	movq	$1, %rdi
	movq	%r13, %rcx
	shlq	%cl, %rdi
	decq	%rdi
.LBB34_19:                              # %cond.end25.i
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$4294966784, %r15       # imm = 0xFFFFFE00
	jle	.LBB34_21
# BB#20:                                # %cond.true30.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movq	$1, %rsi
	movq	%r14, %rcx
	shlq	%cl, %rsi
	decq	%rsi
	jmp	.LBB34_21
.LBB34_9:                               # %cond.end.thread.i
	movq	$1, %rdi
	movq	%r13, %rcx
	shlq	%cl, %rdi
	decq	%rdi
	movq	%r14, %rsi
.LBB34_21:                              # %fixup_signed_type.exit
	callq	build_int_2_wide
	movq	%rax, 112(%r12)
	movq	104(%r12), %rcx
	movq	%r12, 8(%rcx)
	movq	%r12, 8(%rax)
	movq	%r12, %rdi
	callq	layout_type
	movq	sizetype_tab+24(%rip), %rdi
	callq	layout_type
	movzbl	17(%rbx), %ecx
	movq	sizetype_tab(%rip), %rax
	testq	$32, %rcx
	movq	24(%rsp), %rbx          # 8-byte Reload
	je	.LBB34_23
# BB#22:                                # %if.then35
	movq	%rax, sizetype_tab+16(%rip)
	movq	sizetype_tab+24(%rip), %rax
	movq	%rax, sizetype_tab+40(%rip)
	movq	%rbx, %rdi
	callq	make_signed_type
	movq	%rax, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab+8(%rip)
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	make_signed_type
	movq	%rax, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab+32(%rip)
	jmp	.LBB34_36
.LBB34_23:                              # %if.else40
	movq	%rax, sizetype_tab+8(%rip)
	movq	sizetype_tab+24(%rip), %rax
	movq	%rax, sizetype_tab+32(%rip)
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %r13
	movl	60(%r13), %eax
	andq	%r15, %rax
	orq	%rbx, %rax
	movq	%rbx, %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rcx
	cmpq	$128, %rcx
	movl	%eax, 60(%r13)
	movq	$128, %r15
	ja	.LBB34_25
# BB#24:                                # %if.else40
	movq	%rbx, %r15
.LBB34_25:                              # %if.else40
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	build_int_2_wide
	movq	%rax, 104(%r13)
	movq	%r15, %rax
	addq	$-64, %rax
	js	.LBB34_26
# BB#27:                                # %cond.end.i.i
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rdi
	testq	%rax, %rax
	jle	.LBB34_29
# BB#28:                                # %cond.true8.i.i
	movq	$128, %rcx
	subq	%r15, %rcx
	andq	%r12, %rcx
	movq	$-1, %rdi
	movq	$-1, %r14
	shrq	%cl, %r14
	jmp	.LBB34_29
.LBB34_26:                              # %cond.end.thread.i.i
	movq	$1, %rdi
	movq	%r15, %rcx
	shlq	%cl, %rdi
	decq	%rdi
.LBB34_29:                              # %make_unsigned_type.exit
	movq	%r14, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%r13)
	movq	104(%r13), %rcx
	movq	%r13, 8(%rcx)
	movq	%r13, 8(%rax)
	movq	%r13, %rdi
	callq	layout_type
	movq	%r13, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab+16(%rip)
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %rbx
	movl	60(%rbx), %eax
	movabsq	$4294966784, %rcx       # imm = 0xFFFFFE00
	andq	%rcx, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	orq	%rdx, %rax
	movq	%rdx, %rcx
	andq	%r12, %rcx
	cmpq	$128, %rcx
	movl	%eax, 60(%rbx)
	movq	$128, %r15
	ja	.LBB34_31
# BB#30:                                # %make_unsigned_type.exit
	movq	%rdx, %r15
.LBB34_31:                              # %make_unsigned_type.exit
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	callq	build_int_2_wide
	movq	%rax, 104(%rbx)
	movq	%r15, %rax
	addq	$-64, %rax
	js	.LBB34_32
# BB#33:                                # %cond.end.i.i148
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$-1, %rdi
	testq	%rax, %rax
	jle	.LBB34_35
# BB#34:                                # %cond.true8.i.i152
	movq	$128, %rcx
	subq	%r15, %rcx
	andq	%r12, %rcx
	movq	$-1, %rdi
	movq	$-1, %r14
	shrq	%cl, %r14
	jmp	.LBB34_35
.LBB34_32:                              # %cond.end.thread.i.i146
	movq	$1, %rdi
	movq	%r15, %rcx
	shlq	%cl, %rdi
	decq	%rdi
.LBB34_35:                              # %make_unsigned_type.exit158
	movq	%r14, %rsi
	callq	build_int_2_wide
	movq	%rax, 112(%rbx)
	movq	104(%rbx), %rcx
	movq	%rbx, 8(%rcx)
	movq	%rbx, 8(%rax)
	movq	%rbx, %rdi
	callq	layout_type
	movq	%rbx, %rdi
	callq	copy_node
	movq	%rax, sizetype_tab+40(%rip)
.LBB34_36:                              # %if.end45
	movabsq	$.L.str13, %rdi
	callq	get_identifier
	movq	sizetype_tab+24(%rip), %rcx
	movq	%rax, 96(%rcx)
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab(%rip), %rax
	movq	$0, 80(%rax)
	movq	sizetype_tab+8(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+8(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab+8(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab+8(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab+8(%rip), %rax
	movq	$0, 80(%rax)
	movq	sizetype_tab+16(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+16(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab+16(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab+16(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab+16(%rip), %rax
	movq	$0, 80(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab+24(%rip), %rax
	movq	$0, 80(%rax)
	movq	sizetype_tab+32(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+32(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab+32(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab+32(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab+32(%rip), %rax
	movq	$0, 80(%rax)
	movq	sizetype_tab+40(%rip), %rax
	movl	60(%rax), %ecx
	orq	$131072, %rcx           # imm = 0x20000
	movl	%ecx, 60(%rax)
	movq	sizetype_tab+40(%rip), %rax
	movq	%rax, 128(%rax)
	movq	sizetype_tab+40(%rip), %rax
	movq	$0, 120(%rax)
	movq	sizetype_tab+40(%rip), %rax
	movq	$0, 72(%rax)
	movq	sizetype_tab+40(%rip), %rax
	movabsq	$sizetype_tab, %rdi
	movq	$0, 80(%rax)
	movq	$6, %rsi
	callq	ggc_add_tree_root
	movq	early_type_list(%rip), %rax
	jmp	.LBB34_37
	.align	16, 0x90
.LBB34_40:                              # %if.end80
                                        #   in Loop: Header=BB34_37 Depth=1
	movq	sizetype_tab+24(%rip), %rdx
	movq	32(%rcx), %rsi
	movq	%rdx, 8(%rsi)
	movq	sizetype_tab(%rip), %rdx
	movq	40(%rcx), %rcx
	movq	%rdx, 8(%rcx)
	movq	(%rax), %rax
.LBB34_37:                              # %if.end45
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB34_41
# BB#38:                                # %for.body73
                                        #   in Loop: Header=BB34_37 Depth=1
	movq	32(%rax), %rcx
	movzbl	16(%rcx), %edx
	cmpq	$6, %rdx
	je	.LBB34_40
# BB#39:                                # %if.then79
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.set_sizetype, %rcx
	movq	$1859, %rsi             # imm = 0x743
	jmp	.LBB34_4
.LBB34_41:                              # %for.end93
	movq	$0, early_type_list(%rip)
	movq	$1, %rax
	movb	%al, sizetype_set(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB34_3:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.set_sizetype, %rcx
	movq	$1806, %rsi             # imm = 0x70E
.LBB34_4:                               # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp336:
	.size	set_sizetype, .Ltmp336-set_sizetype
	.cfi_endproc

	.globl	get_best_mode
	.align	16, 0x90
	.type	get_best_mode,@function
get_best_mode:                          # @get_best_mode
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp340:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp341:
	.cfi_def_cfa_offset 24
.Ltmp342:
	.cfi_offset %rbx, -24
.Ltmp343:
	.cfi_offset %r14, -16
	movl	class_narrowest_mode+4(%rip), %r10d
	movq	%rcx, %r8
	movq	%rdx, %r9
	movq	%rsi, %r14
	xorq	%rax, %rax
	testq	%r10, %r10
	je	.LBB35_10
# BB#1:
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	andq	%r11, %r14
	.align	16, 0x90
.LBB35_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r10, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movzbl	mode_bitsize(%rsi,%rsi), %ebx
	xorq	%rdx, %rdx
	movq	%r14, %rax
	divq	%rbx
	addq	%rdi, %rdx
	andq	%r11, %rdx
	cmpq	%rbx, %rdx
	jbe	.LBB35_4
# BB#3:                                 # %for.inc
                                        #   in Loop: Header=BB35_2 Depth=1
	movzbl	mode_wider_mode(%rsi), %r10d
	xorq	%rax, %rax
	testq	%r10, %r10
	jne	.LBB35_2
	jmp	.LBB35_10
.LBB35_4:                               # %lor.lhs.false
	cmpq	$128, %rbx
	movq	%rbx, %rcx
	jb	.LBB35_6
# BB#5:                                 # %lor.lhs.false
	movq	$128, %rcx
.LBB35_6:                               # %lor.lhs.false
	andq	%r11, %r9
	xorq	%rax, %rax
	cmpq	%r9, %rcx
	ja	.LBB35_10
# BB#7:                                 # %lor.lhs.false12
	testq	%r11, %r8
	je	.LBB35_9
# BB#8:                                 # %land.lhs.true
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movzbl	mode_bitsize(%r8,%r8), %ecx
	cmpq	%rcx, %rbx
	ja	.LBB35_10
.LBB35_9:                               # %if.end21
	movq	%r10, %rax
.LBB35_10:                              # %return
	popq	%rbx
	popq	%r14
	retq
.Ltmp344:
	.size	get_best_mode, .Ltmp344-get_best_mode
	.cfi_endproc

	.globl	init_stor_layout_once
	.align	16, 0x90
	.type	init_stor_layout_once,@function
init_stor_layout_once:                  # @init_stor_layout_once
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$pending_sizes, %rdi
	movq	$1, %rsi
	jmp	ggc_add_tree_root  # TAILCALL
.Ltmp345:
	.size	init_stor_layout_once, .Ltmp345-init_stor_layout_once
	.cfi_endproc

	.type	set_alignment,@object   # @set_alignment
	.bss
	.globl	set_alignment
	.align	4
set_alignment:
	.long	0                       # 0x0
	.size	set_alignment, 4

	.type	reference_types_internal,@object # @reference_types_internal
	.local	reference_types_internal
	.comm	reference_types_internal,1,1
	.type	pending_sizes,@object   # @pending_sizes
	.local	pending_sizes
	.comm	pending_sizes,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"stor-layout.c"
	.size	.L.str, 14

	.type	.L__FUNCTION__.put_pending_sizes,@object # @__FUNCTION__.put_pending_sizes
.L__FUNCTION__.put_pending_sizes:
	.asciz	"put_pending_sizes"
	.size	.L__FUNCTION__.put_pending_sizes, 18

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"type size can't be explicitly evaluated"
	.size	.L.str1, 40

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"variable-size type declared outside of any function"
	.size	.L.str2, 52

	.type	immediate_size_expand,@object # @immediate_size_expand
	.comm	immediate_size_expand,4,4
	.type	.L__FUNCTION__.smallest_mode_for_size,@object # @__FUNCTION__.smallest_mode_for_size
.L__FUNCTION__.smallest_mode_for_size:
	.asciz	"smallest_mode_for_size"
	.size	.L__FUNCTION__.smallest_mode_for_size, 23

	.type	.L__FUNCTION__.int_mode_for_mode,@object # @__FUNCTION__.int_mode_for_mode
.L__FUNCTION__.int_mode_for_mode:
	.asciz	"int_mode_for_mode"
	.size	.L__FUNCTION__.int_mode_for_mode, 18

	.type	.L__FUNCTION__.layout_decl,@object # @__FUNCTION__.layout_decl
.L__FUNCTION__.layout_decl:
	.asciz	"layout_decl"
	.size	.L__FUNCTION__.layout_decl, 12

	.type	sizetype_tab,@object    # @sizetype_tab
	.comm	sizetype_tab,48,16
	.type	maximum_field_alignment,@object # @maximum_field_alignment
	.comm	maximum_field_alignment,4,4
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"size of `%s' is %d bytes"
	.size	.L.str3, 25

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"size of `%s' is larger than %d bytes"
	.size	.L.str4, 37

	.type	lang_adjust_rli,@object # @lang_adjust_rli
	.bss
	.globl	lang_adjust_rli
	.align	8
lang_adjust_rli:
	.quad	0
	.size	lang_adjust_rli, 8

	.type	.L.str5,@object         # @.str5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str5:
	.asciz	"type"
	.size	.L.str5, 5

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"\noffset"
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	" bitpos"
	.size	.L.str7, 8

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"\naligns: rec = %u, unpack = %u, unpad = %u, off = %u\n"
	.size	.L.str8, 54

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"packed may be necessary\n"
	.size	.L.str9, 25

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"pending statics:\n"
	.size	.L.str10, 18

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"packed attribute is unnecessary for `%s'"
	.size	.L.str11, 41

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"padding struct to align `%s'"
	.size	.L.str12, 29

	.type	.L__FUNCTION__.layout_type,@object # @__FUNCTION__.layout_type
.L__FUNCTION__.layout_type:
	.asciz	"layout_type"
	.size	.L__FUNCTION__.layout_type, 12

	.type	sizetype_set,@object    # @sizetype_set
	.local	sizetype_set
	.comm	sizetype_set,1,1
	.type	early_type_list,@object # @early_type_list
	.local	early_type_list
	.comm	early_type_list,8,8
	.type	.L__FUNCTION__.set_sizetype,@object # @__FUNCTION__.set_sizetype
.L__FUNCTION__.set_sizetype:
	.asciz	"set_sizetype"
	.size	.L__FUNCTION__.set_sizetype, 13

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"bit_size_type"
	.size	.L.str13, 14

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"padding struct size to alignment boundary"
	.size	.L.str14, 42

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"packed attribute is unnecessary"
	.size	.L.str15, 32


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
