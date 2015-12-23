	.file	"tree-inline.c.bc"
	.text
	.globl	tree_inlinable_function_p
	.align	16, 0x90
	.type	tree_inlinable_function_p,@function
tree_inlinable_function_p:              # @tree_inlinable_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rsi, %rsi
	jmp	inlinable_function_p  # TAILCALL
.Ltmp0:
	.size	tree_inlinable_function_p, .Ltmp0-tree_inlinable_function_p
	.cfi_endproc

	.align	16, 0x90
	.type	inlinable_function_p,@function
inlinable_function_p:                   # @inlinable_function_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp7:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp8:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp9:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp10:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp12:
	.cfi_def_cfa_offset 64
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)
	movq	48(%rdi), %rcx
	movq	%rsi, %r14
	movabsq	$68719476736, %r15      # imm = 0x1000000000
	xorq	%rax, %rax
	testq	%r15, %rcx
	jne	.LBB1_35
# BB#1:                                 # %if.end
	movl	flag_inline_trees(%rip), %eax
	movq	lang_hooks+168(%rip), %r13
	xorq	%r12, %r12
	testq	%rax, %rax
	je	.LBB1_6
# BB#2:                                 # %if.end
	andq	$2048, %rcx             # imm = 0x800
	je	.LBB1_6
# BB#3:                                 # %if.else10
	callq	*%r13
	movq	8(%rsp), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB1_5
# BB#4:                                 # %land.lhs.true
	movq	compiler_params(%rip), %rax
	movq	56(%rdi), %rcx
	addq	%rcx, %rcx
	movl	8(%rax), %eax
	leaq	(%rcx,%rcx,4), %rdx
	movq	$31, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	addq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$33, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jg	.LBB1_6
.LBB1_5:                                # %if.else15
	movq	$1, %r12
.LBB1_6:                                # %if.end18
	movq	%r12, %rax
	xorq	$1, %rax
	movq	48(%rdi), %rdx
	movq	$36, %rcx
	shlq	%cl, %rax
	andq	%r15, %rax
	movabsq	$-68719476737, %rcx     # imm = 0xFFFFFFEFFFFFFFFF
	andq	%rdx, %rcx
	orq	%rax, %rcx
	movq	%rcx, 48(%rdi)
	movq	8(%rsp), %rbx
	movq	%rbx, %rdi
	callq	*%r13
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	jne	.LBB1_11
# BB#7:                                 # %land.lhs.true26
	movq	56(%rbx), %rax
	xorq	%rcx, %rcx
	testq	%r14, %r14
	je	.LBB1_9
# BB#8:                                 # %cond.true
	movslq	56(%r14), %rcx
.LBB1_9:                                # %cond.end
	movq	compiler_params(%rip), %rdx
	movslq	8(%rdx), %rdx
	addq	%rax, %rcx
	addq	%rcx, %rcx
	leaq	(%rcx,%rcx,4), %rcx
	cmpq	%rdx, %rcx
	jle	.LBB1_11
# BB#10:                                # %land.lhs.true38
	addq	%rax, %rax
	leaq	(%rax,%rax,4), %rax
	movq	$61, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$3, %rsi
	addq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$34, %rcx
	sarq	%cl, %rsi
	xorq	%rcx, %rcx
	cmpq	%rsi, %rax
	jle	.LBB1_12
	jmp	.LBB1_16
.LBB1_11:                               # %if.end50
	xorq	%rcx, %rcx
.LBB1_12:                               # %if.end50
	testq	%r12, %r12
	je	.LBB1_16
# BB#13:                                # %land.lhs.true52
	leaq	8(%rsp), %rdi
	callq	*lang_hooks+160(%rip)
	testq	%r13, %rax
	je	.LBB1_15
# BB#14:                                # %land.lhs.true52
	xorq	%r12, %r12
.LBB1_15:                               # %land.lhs.true52
	movq	8(%rsp), %rbx
	movq	%r12, %rcx
.LBB1_16:                               # %if.end56
	xorq	%rax, %rax
	cmpq	$0, 168(%rbx)
	movabsq	$0, %r9
	je	.LBB1_18
# BB#17:                                # %select.mid
	movq	%rcx, %r9
.LBB1_18:                               # %select.end
	testq	%r13, %r9
	je	.LBB1_35
# BB#19:                                # %lor.lhs.false
	movq	48(%rbx), %rcx
	testq	%r15, %rcx
	jne	.LBB1_35
# BB#20:                                # %if.end70
	testq	%r14, %r14
	je	.LBB1_21
# BB#22:                                # %for.cond.preheader
	movq	(%r14), %rcx
	movq	8(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB1_27
# BB#23:                                # %for.body.preheader
	xorq	%rdi, %rdi
	.align	16, 0x90
.LBB1_25:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rcx,%rdi,8), %rdx
	cmpq	%rbx, %rdx
	je	.LBB1_26
# BB#24:                                # %for.cond
                                        #   in Loop: Header=BB1_25 Depth=1
	incq	%rdi
	cmpq	%rsi, %rdi
	jb	.LBB1_25
.LBB1_27:                               # %for.end
	movq	176(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB1_28
# BB#29:                                # %for.cond88.preheader
	movslq	24(%rsi), %rdi
	testq	%rdi, %rdi
	jle	.LBB1_30
# BB#33:                                # %for.body91.lr.ph
	movq	32(%rcx), %r8
	movq	$4, %rbx
	.align	16, 0x90
.LBB1_34:                               # %for.body91
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi,%rbx,8), %rcx
	cmpq	%r8, %rcx
	je	.LBB1_35
# BB#31:                                # %for.cond88
                                        #   in Loop: Header=BB1_34 Depth=1
	leaq	-3(%rbx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	cmpq	%rdi, %rdx
	jl	.LBB1_34
# BB#32:
	movq	%r9, %rax
	jmp	.LBB1_35
.LBB1_21:
	movq	%r9, %rax
	jmp	.LBB1_35
.LBB1_26:
	xorq	%rax, %rax
	jmp	.LBB1_35
.LBB1_28:
	movq	%r9, %rax
	jmp	.LBB1_35
.LBB1_30:
	movq	%r9, %rax
.LBB1_35:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp18:
	.size	inlinable_function_p, .Ltmp18-inlinable_function_p
	.cfi_endproc

	.globl	optimize_inline_calls
	.align	16, 0x90
	.type	optimize_inline_calls,@function
optimize_inline_calls:                  # @optimize_inline_calls
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp23:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp24:
	.cfi_def_cfa_offset 96
.Ltmp25:
	.cfi_offset %rbx, -24
.Ltmp26:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	$0, 64(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movabsq	$.L.str, %rdx
	movq	$32, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, (%rsp)
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_2
# BB#1:                                 # %if.then
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%rsp)
	movq	8(%rax), %rcx
.LBB2_2:                                # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
	movq	current_function_decl(%rip), %rcx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB2_6
# BB#3:                                 # %do.body12
	movq	(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rdx
	cmpq	%rsi, %rdx
	jb	.LBB2_5
# BB#4:                                 # %if.then18
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%rsp)
	movq	current_function_decl(%rip), %rcx
	movq	8(%rax), %rdx
.LBB2_5:                                # %if.end25
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rax)
	movq	%rcx, 32(%rax,%rdx,8)
	movq	current_function_decl(%rip), %rsi
.LBB2_6:                                # %if.end35
	leaq	(%rsp), %rbx
	movq	%rbx, %rdi
	callq	*lang_hooks+176(%rip)
	movabsq	$.L.str1, %rdx
	movq	$32, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, 40(%rsp)
	movabsq	$.L.str2, %rdx
	movq	$32, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, 48(%rsp)
	movq	(%rsp), %rax
	movq	8(%rax), %rax
	movq	htab_hash_pointer(%rip), %rsi
	movq	htab_eq_pointer(%rip), %rdx
	movl	%eax, 8(%rsp)
	movq	$37, %rdi
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, 64(%rsp)
	leaq	168(%r14), %rdi
	movabsq	$expand_call_inline, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	callq	walk_tree
	movq	64(%rsp), %rdi
	callq	htab_delete
	movq	(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB2_8
# BB#7:                                 # %if.then47
	callq	free
	movq	$0, (%rsp)
.LBB2_8:                                # %do.body53
	movq	40(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB2_10
# BB#9:                                 # %if.then56
	callq	free
	movq	$0, 40(%rsp)
.LBB2_10:                               # %do.end61
	cmpq	$0, 200(%r14)
	je	.LBB2_12
# BB#11:                                # %if.then64
	movq	48(%rsp), %rax
	movq	8(%rax), %rdi
	callq	make_tree_vec
	movq	48(%rsp), %rsi
	movq	%rax, %rbx
	movq	8(%rsi), %rdx
	leaq	32(%rbx), %rdi
	addq	$32, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	callq	memcpy
	movq	%rbx, 176(%r14)
.LBB2_12:                               # %do.body80
	movq	48(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB2_14
# BB#13:                                # %if.then83
	callq	free
	movq	$0, 48(%rsp)
.LBB2_14:                               # %do.end88
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp27:
	.size	optimize_inline_calls, .Ltmp27-optimize_inline_calls
	.cfi_endproc

	.globl	clone_body
	.align	16, 0x90
	.type	clone_body,@function
clone_body:                             # @clone_body
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp35:
	.cfi_def_cfa_offset 112
.Ltmp36:
	.cfi_offset %rbx, -32
.Ltmp37:
	.cfi_offset %r14, -24
.Ltmp38:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$0, 64(%rsp)
	movq	$0, 56(%rsp)
	movq	$0, 48(%rsp)
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	$0, (%rsp)
	movabsq	$.L.str, %rdx
	movq	$2, %rdi
	movq	$8, %rsi
	callq	varray_init
	movq	%rax, (%rsp)
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB3_2
# BB#1:                                 # %if.then
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%rsp)
	movq	8(%rax), %rcx
.LBB3_2:                                # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	movq	(%rsp), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB3_4
# BB#3:                                 # %if.then17
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%rsp)
	movq	8(%rax), %rcx
.LBB3_4:                                # %if.end24
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r15, 32(%rax,%rcx,8)
	movq	%r14, 24(%rsp)
	movq	$1, %rax
	movb	%al, 60(%rsp)
	movq	(%rsp), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rax
	movq	168(%rax), %rax
	movq	%rax, 72(%rsp)
	movabsq	$copy_body_r, %rsi
	leaq	72(%rsp), %rdi
	leaq	(%rsp), %rdx
	xorq	%rcx, %rcx
	callq	walk_tree
	movq	72(%rsp), %rax
	movq	168(%rbx), %rcx
	movq	%rax, (%rcx)
	movq	(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB3_6
# BB#5:                                 # %if.then37
	callq	free
	movq	$0, (%rsp)
.LBB3_6:                                # %do.end42
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp39:
	.size	clone_body, .Ltmp39-clone_body
	.cfi_endproc

	.globl	walk_tree
	.align	16, 0x90
	.type	walk_tree,@function
walk_tree:                              # @walk_tree
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp48:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp49:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp51:
	.cfi_def_cfa_offset 80
.Ltmp52:
	.cfi_offset %rbx, -48
.Ltmp53:
	.cfi_offset %r12, -40
.Ltmp54:
	.cfi_offset %r13, -32
.Ltmp55:
	.cfi_offset %r14, -24
.Ltmp56:
	.cfi_offset %r15, -16
	movq	(%rdi), %rax
	movq	%rdi, %rbx
	movq	%rcx, %r14
	movq	%r14, (%rsp)            # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB4_2
# BB#1:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_44:                               # %do.body168
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rbx
	addq	$8, %rbx
	jmp	.LBB4_13
.LBB4_45:                               # %do.body173
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	addq	$32, %rdi
.LBB4_46:                               # %do.body173
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
	.align	16, 0x90
.LBB4_12:                               # %do.body
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rbx
.LBB4_13:                               # %tail_recurse.backedge
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rsp), %r14            # 8-byte Reload
	movq	(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB4_2
	jmp	.LBB4_14
.LBB4_47:                               # %sw.bb183
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movslq	24(%rax), %r14
	testq	%r14, %r14
	je	.LBB4_48
# BB#49:                                # %while.cond.preheader
                                        #   in Loop: Header=BB4_2 Depth=1
	leaq	24(,%r14,8), %r15
	decq	%r14
	.align	16, 0x90
.LBB4_50:                               # %while.cond
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB4_52
# BB#51:                                # %do.body191
                                        #   in Loop: Header=BB4_50 Depth=2
	addq	%r15, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	addq	$-8, %r15
	decq	%r14
	testq	%rax, %rax
	je	.LBB4_50
	jmp	.LBB4_33
.LBB4_64:                               # %do.body268
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	addq	$104, %rdi
	jmp	.LBB4_65
.LBB4_67:                               # %do.body280
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	addq	$8, %rdi
.LBB4_65:                               # %do.body268
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#66:                                # %do.body275
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rbx
	addq	$112, %rbx
	jmp	.LBB4_13
.LBB4_62:                               # %do.body255
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	addq	$8, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#63:                                # %do.body263
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rbx
	addq	$24, %rbx
	jmp	.LBB4_13
.LBB4_53:                               # %do.body206
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	addq	$32, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
.LBB4_54:                               # %do.body216
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rbx
	addq	$40, %rbx
	jmp	.LBB4_13
.LBB4_52:                               # %do.body200
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$32, %rdi
	movq	%rdi, %rbx
	jmp	.LBB4_13
	.align	16, 0x90
.LBB4_2:                                # %if.end
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_32 Depth 2
                                        #     Child Loop BB4_50 Depth 2
	testq	%r14, %r14
	je	.LBB4_6
# BB#3:                                 # %if.then2
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	htab_find
	testq	%rax, %rax
	jne	.LBB4_4
# BB#5:                                 # %if.end5
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	callq	htab_find_slot
	movq	(%rbx), %rcx
	movq	%rcx, (%rax)
.LBB4_6:                                # %if.end7
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$1, %rax
	movl	%eax, 28(%rsp)
	movq	%rbx, %rdi
	leaq	28(%rsp), %rsi
	movq	16(%rsp), %rdx          # 8-byte Reload
	callq	*8(%rsp)                # 8-byte Folded Reload
	testq	%rax, %rax
	jne	.LBB4_33
# BB#7:                                 # %if.end11
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movl	28(%rsp), %ecx
	movzbl	16(%rax), %r13d
	testq	%rcx, %rcx
	jne	.LBB4_15
# BB#8:                                 # %if.then14
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdi
	callq	statement_code_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_12
# BB#9:                                 # %if.then14
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$2, %r13
	je	.LBB4_12
# BB#10:                                # %lor.lhs.false17
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rdi
	movq	lang_hooks+184(%rip), %rax
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_12
	jmp	.LBB4_11
	.align	16, 0x90
.LBB4_15:                               # %if.end23
                                        #   in Loop: Header=BB4_2 Depth=1
	movsbq	tree_code_type(%r13), %rcx
	cmpq	$113, %rcx
	jg	.LBB4_20
# BB#16:                                # %if.end23
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB4_40
# BB#17:                                # %if.end23
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$4503599627372547, %rsi # imm = 0x10000000000803
	testq	%rsi, %rdx
	jne	.LBB4_21
# BB#18:                                # %if.end23
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$51, %rcx
	jne	.LBB4_40
# BB#19:                                # %do.body156
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$8, %rax
	movq	%rax, %rbx
	jmp	.LBB4_13
	.align	16, 0x90
.LBB4_20:                               # %if.end23
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$-114, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	ja	.LBB4_40
.LBB4_21:                               # %if.then56
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdi
	callq	statement_code_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_24
# BB#22:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movzbl	19(%rax), %ecx
	testq	$4, %rcx
	jne	.LBB4_24
# BB#23:                                # %if.then63
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	24(%rax), %eax
	movl	%eax, lineno(%rip)
.LBB4_24:                               # %if.end64
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdi
	callq	first_rtl_op
	movq	%rax, %r14
	cmpq	$50, %r13
	jne	.LBB4_28
# BB#25:                                # %land.lhs.true68
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movq	40(%rax), %rcx
	movq	56(%rax), %rax
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	jne	.LBB4_27
# BB#26:                                # %land.lhs.true68
                                        #   in Loop: Header=BB4_2 Depth=1
	movabsq	$1, %rax
.LBB4_27:                               # %land.lhs.true68
                                        #   in Loop: Header=BB4_2 Depth=1
	subq	%rax, %r14
.LBB4_28:                               # %if.end77
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	$32, %r15
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$1, %r12
	testq	%rax, %rax
	jle	.LBB4_29
	.align	16, 0x90
.LBB4_32:                               # %do.body80
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rdi
	addq	%r15, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#31:                                # %for.cond
                                        #   in Loop: Header=BB4_32 Depth=2
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	addq	$8, %r15
	cmpq	%rdx, %rax
	jl	.LBB4_32
.LBB4_29:                               # %for.end
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%r13, %rdi
	callq	statement_code_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_30
# BB#34:                                # %if.then92
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$154, %r13
	jne	.LBB4_12
# BB#35:                                # %land.lhs.true95
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movq	32(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_12
# BB#36:                                # %land.lhs.true100
                                        #   in Loop: Header=BB4_2 Depth=1
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB4_12
# BB#37:                                # %do.body114
                                        #   in Loop: Header=BB4_2 Depth=1
	addq	$104, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#38:                                # %do.body123
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movq	32(%rax), %rdi
	addq	$40, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#39:                                # %do.body133
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	(%rbx), %rax
	movq	32(%rax), %rdi
	addq	$64, %rdi
	jmp	.LBB4_46
	.align	16, 0x90
.LBB4_40:                               # %if.end160
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	%rbx, %rdi
	leaq	28(%rsp), %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	(%rsp), %r8             # 8-byte Reload
	movq	lang_hooks+152(%rip), %rax
	callq	*%rax
	testq	%rax, %rax
	jne	.LBB4_33
# BB#41:                                # %if.end160
                                        #   in Loop: Header=BB4_2 Depth=1
	movl	28(%rsp), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB4_33
# BB#42:                                # %if.end166
                                        #   in Loop: Header=BB4_2 Depth=1
	cmpq	$46, %r13
	ja	.LBB4_68
# BB#43:                                # %if.end166
                                        #   in Loop: Header=BB4_2 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI4_0(,%r13,8)
.LBB4_55:                               # %do.body222
	movq	(%rbx), %rdi
	addq	$112, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
.LBB4_56:                               # %do.body230
	movq	(%rbx), %rdi
	addq	$8, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#57:                                # %do.end237
	movq	(%rbx), %rax
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB4_58
.LBB4_59:                               # %do.body242
                                        # =>This Inner Loop Header: Depth=1
	leaq	32(%rbx), %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	16(%rsp), %rdx          # 8-byte Reload
	movq	(%rsp), %rcx            # 8-byte Reload
	callq	walk_tree
	testq	%rax, %rax
	jne	.LBB4_33
# BB#60:                                # %for.inc250
                                        #   in Loop: Header=BB4_59 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_59
# BB#61:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_14:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_4:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_68:                               # %sw.default
	movabsq	$.L.str3, %rdi
	movabsq	$.L__FUNCTION__.walk_tree, %rdx
	movq	$1315, %rsi             # imm = 0x523
	callq	fancy_abort
.LBB4_11:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_30:
	xorq	%rax, %rax
	jmp	.LBB4_33
.LBB4_58:
	xorq	%rax, %rax
.LBB4_33:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_48:
	xorq	%rax, %rax
	jmp	.LBB4_33
.Ltmp57:
	.size	walk_tree, .Ltmp57-walk_tree
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI4_0:
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_45
	.quad	.LBB4_47
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_64
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_68
	.quad	.LBB4_44
	.quad	.LBB4_67
	.quad	.LBB4_44
	.quad	.LBB4_55
	.quad	.LBB4_68
	.quad	.LBB4_62
	.quad	.LBB4_68
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_68
	.quad	.LBB4_56
	.quad	.LBB4_68
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_53
	.quad	.LBB4_33
	.quad	.LBB4_33
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_68
	.quad	.LBB4_54

	.text
	.globl	walk_tree_without_duplicates
	.align	16, 0x90
	.type	walk_tree_without_duplicates,@function
walk_tree_without_duplicates:           # @walk_tree_without_duplicates
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp63:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp64:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp65:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp67:
	.cfi_def_cfa_offset 48
.Ltmp68:
	.cfi_offset %rbx, -40
.Ltmp69:
	.cfi_offset %r12, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	htab_hash_pointer(%rip), %r8
	movq	htab_eq_pointer(%rip), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$37, %rdi
	xorq	%rcx, %rcx
	movq	%r8, %rsi
	movq	%rax, %rdx
	callq	htab_create
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rcx
	callq	walk_tree
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	htab_delete
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	walk_tree_without_duplicates, .Ltmp72-walk_tree_without_duplicates
	.cfi_endproc

	.globl	copy_tree_r
	.align	16, 0x90
	.type	copy_tree_r,@function
copy_tree_r:                            # @copy_tree_r
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp82:
	.cfi_def_cfa_offset 48
.Ltmp83:
	.cfi_offset %rbx, -40
.Ltmp84:
	.cfi_offset %r12, -32
.Ltmp85:
	.cfi_offset %r14, -24
.Ltmp86:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	(%r14), %rdi
	movzbl	16(%rdi), %ebx
	movsbq	tree_code_type(%rbx), %rax
	movq	%rsi, %r15
	cmpq	$113, %rax
	jg	.LBB6_3
# BB#1:                                 # %entry
	leaq	-49(%rax), %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB6_4
# BB#2:                                 # %entry
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$5629499534215171, %rcx # imm = 0x14000000000803
	testq	%rcx, %rdx
	jne	.LBB6_9
	jmp	.LBB6_4
.LBB6_3:                                # %entry
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$114, %rcx
	je	.LBB6_9
.LBB6_4:                                # %lor.lhs.false32
	andq	$255, %rax
	cmpq	$115, %rax
	je	.LBB6_9
# BB#5:                                 # %lor.lhs.false32
	cmpq	$2, %rbx
	je	.LBB6_9
# BB#6:                                 # %lor.lhs.false32
	cmpq	$3, %rbx
	je	.LBB6_9
# BB#7:                                 # %lor.lhs.false44
	callq	*lang_hooks+184(%rip)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_16
# BB#8:                                 # %lor.lhs.false44.if.then_crit_edge
	movq	(%r14), %rdi
.LBB6_9:                                # %if.then
	movq	(%rdi), %r15
	callq	copy_node
	movq	%rax, (%r14)
	cmpq	$2, %rbx
	je	.LBB6_13
# BB#10:                                # %if.then
	cmpq	$35, %rbx
	je	.LBB6_13
# BB#11:                                # %lor.lhs.false53
	movq	%rax, %rdi
	callq	*lang_hooks+184(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB6_13
# BB#12:                                # %lor.lhs.false56
	movq	%rbx, %rdi
	callq	statement_code_p
	testq	%r12, %rax
	je	.LBB6_14
.LBB6_13:                               # %if.then59
	movq	(%r14), %rax
	movq	%r15, (%rax)
.LBB6_14:                               # %if.end
	movq	(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$166, %rcx
	jne	.LBB6_18
# BB#15:                                # %if.then68
	movq	$0, 32(%rax)
.LBB6_18:                               # %if.end78
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB6_16:                               # %if.else
	movzbl	tree_code_type(%rbx), %eax
	cmpq	$116, %rax
	jne	.LBB6_18
# BB#17:                                # %if.then76
	xorq	%rax, %rax
	movl	%eax, (%r15)
	jmp	.LBB6_18
.Ltmp87:
	.size	copy_tree_r, .Ltmp87-copy_tree_r
	.cfi_endproc

	.globl	remap_save_expr
	.align	16, 0x90
	.type	remap_save_expr,@function
remap_save_expr:                        # @remap_save_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp94:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp95:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp96:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp97:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp98:
	.cfi_def_cfa_offset 48
.Ltmp99:
	.cfi_offset %rbx, -48
.Ltmp100:
	.cfi_offset %r12, -40
.Ltmp101:
	.cfi_offset %r13, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	(%r15), %rax
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	splay_tree_lookup
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_1
# BB#2:                                 # %if.else
	xorq	%rax, %rax
	movl	%eax, (%r13)
	jmp	.LBB7_3
.LBB7_1:                                # %if.then
	movq	(%r15), %rdi
	callq	copy_node
	movq	%rax, %r13
	movq	%r12, 40(%r13)
	movq	$0, 48(%r13)
	movq	(%r15), %rsi
	movq	%r14, %rdi
	movq	%r13, %rdx
	callq	splay_tree_insert
	movq	global_trees(%rip), %rdx
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	splay_tree_insert
.LBB7_3:                                # %if.end
	movq	8(%rbx), %rax
	movq	%rax, (%r15)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	remap_save_expr, .Ltmp104-remap_save_expr
	.cfi_endproc

	.align	16, 0x90
	.type	copy_body_r,@function
copy_body_r:                            # @copy_body_r
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp111:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp112:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp113:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp116:
	.cfi_def_cfa_offset 64
.Ltmp117:
	.cfi_offset %rbx, -48
.Ltmp118:
	.cfi_offset %r12, -40
.Ltmp119:
	.cfi_offset %r13, -32
.Ltmp120:
	.cfi_offset %r14, -24
.Ltmp121:
	.cfi_offset %r15, -16
	movq	%rdx, %r13
	movq	(%r13), %rax
	movq	%rdi, %r14
	movq	(%r14), %rbx
	movq	8(%rax), %rcx
	movzbl	16(%rbx), %edx
	movq	24(%rax,%rcx,8), %r15
	movq	%rsi, %rax
	cmpq	$159, %rdx
	jne	.LBB8_5
# BB#1:                                 # %land.lhs.true
	movq	16(%r13), %rsi
	testq	%rsi, %rsi
	je	.LBB8_5
# BB#2:                                 # %if.then
	movq	$163, %rdi
	xorq	%rax, %rax
	callq	build_stmt
	movq	(%rbx), %rcx
	movq	%rax, %r15
	movq	%rcx, (%r15)
	movl	16(%r15), %eax
	orq	$16777216, %rax         # imm = 0x1000000
	movl	%eax, 16(%r15)
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB8_4
# BB#3:                                 # %if.then12
	movq	$152, %rdi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rax, (%r14)
	movl	16(%rax), %ecx
	orq	$33554432, %rcx         # imm = 0x2000000
	movl	%ecx, 16(%rax)
	movq	(%r14), %rax
	movq	%r15, (%rax)
	jmp	.LBB8_70
.LBB8_5:                                # %if.else23
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	lang_hooks+192(%rip), %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*%rax
	movq	(%r14), %rdx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB8_14
# BB#6:                                 # %if.then26
	movq	%rdx, %rdi
	movq	%r13, %rsi
	callq	remap_decl
	testq	%rax, %rax
	je	.LBB8_12
# BB#7:                                 # %while.cond.preheader
	movq	global_trees(%rip), %rcx
	.align	16, 0x90
.LBB8_8:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdx
	movzbl	16(%rdx), %eax
	addq	$-114, %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	ja	.LBB8_11
# BB#9:                                 # %land.lhs.true47
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	32(%rdx), %rax
	cmpq	%rcx, %rax
	je	.LBB8_11
# BB#10:                                # %land.rhs
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	8(%rdx), %rsi
	movq	8(%rax), %rdi
	cmpq	%rdi, %rsi
	je	.LBB8_8
.LBB8_11:                               # %while.end
	movq	%rdx, (%r14)
	jmp	.LBB8_70
.LBB8_14:                               # %if.else62
	movzbl	16(%rdx), %ebx
	cmpq	$119, %rbx
	je	.LBB8_21
# BB#15:                                # %if.else62
	cmpq	$166, %rbx
	jne	.LBB8_16
# BB#22:                                # %land.lhs.true86
	movl	32(%r13), %eax
	testq	%rax, %rax
	jne	.LBB8_37
# BB#23:                                # %if.then88
	movsbq	tree_code_type+166(%rip), %rcx
	movq	32(%rdx), %r15
	cmpq	$113, %rcx
	jg	.LBB8_26
# BB#24:                                # %if.then88
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB8_27
# BB#25:                                # %if.then88
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$5629499534215171, %rcx # imm = 0x14000000000803
	testq	%rcx, %rax
	jne	.LBB8_29
	jmp	.LBB8_27
.LBB8_16:                               # %if.else62
	cmpq	$118, %rbx
	jne	.LBB8_37
# BB#17:                                # %if.then68
	movq	(%r13), %rax
	movq	24(%r13), %r15
	movq	32(%rax), %r13
	movq	%r15, %rdi
	movq	%rdx, %rsi
	callq	splay_tree_lookup
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB8_18
# BB#19:                                # %if.else.i
	xorq	%rax, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%eax, (%rcx)
	movq	8(%rbx), %rax
	jmp	.LBB8_69
.LBB8_37:                               # %if.else89
	movsbq	tree_code_type(%rbx), %rax
	cmpq	$113, %rax
	jg	.LBB8_40
# BB#38:                                # %if.else89
	leaq	-49(%rax), %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB8_41
# BB#39:                                # %if.else89
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$5629499534215171, %rcx # imm = 0x14000000000803
	testq	%rcx, %rsi
	jne	.LBB8_46
	jmp	.LBB8_41
.LBB8_4:                                # %if.else
	movq	%r15, (%r14)
	jmp	.LBB8_70
.LBB8_40:                               # %if.else89
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$114, %rcx
	je	.LBB8_46
.LBB8_41:                               # %lor.lhs.false32.i
	andq	$255, %rax
	cmpq	$115, %rax
	je	.LBB8_46
# BB#42:                                # %lor.lhs.false32.i
	cmpq	$2, %rbx
	je	.LBB8_46
# BB#43:                                # %lor.lhs.false32.i
	cmpq	$3, %rbx
	je	.LBB8_46
# BB#44:                                # %lor.lhs.false44.i
	movq	%rdx, %rdi
	callq	*lang_hooks+184(%rip)
	testq	%r12, %rax
	je	.LBB8_53
# BB#45:                                # %lor.lhs.false44.if.then_crit_edge.i
	movq	(%r14), %rdx
.LBB8_46:                               # %if.then.i272
	movq	(%rdx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %rdi
	callq	copy_node
	movq	%rax, (%r14)
	cmpq	$2, %rbx
	je	.LBB8_50
# BB#47:                                # %if.then.i272
	cmpq	$35, %rbx
	je	.LBB8_50
# BB#48:                                # %lor.lhs.false53.i
	movq	%rax, %rdi
	callq	*lang_hooks+184(%rip)
	testq	%r12, %rax
	jne	.LBB8_50
# BB#49:                                # %lor.lhs.false56.i
	movq	%rbx, %rdi
	callq	statement_code_p
	testq	%r12, %rax
	je	.LBB8_51
.LBB8_50:                               # %if.then59.i
	movq	(%r14), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, (%rax)
.LBB8_51:                               # %if.end.i
	movq	(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$166, %rcx
	jne	.LBB8_55
# BB#52:                                # %if.then68.i
	movq	$0, 32(%rax)
.LBB8_55:                               # %copy_tree_r.exit
	movq	(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$48, %rcx
	jne	.LBB8_56
# BB#59:                                # %land.lhs.true117
	movq	32(%rax), %rdi
	movq	40(%rax), %rax
	cmpq	%rax, %rdi
	movq	lang_hooks+192(%rip), %rax
	jne	.LBB8_70
# BB#60:                                # %land.lhs.true125
	movq	%r15, %rsi
	callq	*%rax
	testq	%r12, %rax
	je	.LBB8_70
# BB#61:                                # %if.then131
	movq	(%r14), %rax
	movq	32(%rax), %rsi
	movq	24(%r13), %rdi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB8_70
# BB#62:                                # %if.then138
	movq	8(%rax), %rdi
	movq	global_trees(%rip), %r8
.LBB8_63:                               # %while.cond140
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movl	16(%rax), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	leaq	-114(%rcx), %rdi
	andq	%r12, %rdi
	cmpq	$2, %rdi
	ja	.LBB8_66
# BB#64:                                # %land.lhs.true158
                                        #   in Loop: Header=BB8_63 Depth=1
	movq	32(%rax), %rdi
	cmpq	%r8, %rdi
	je	.LBB8_66
# BB#65:                                # %land.rhs163
                                        #   in Loop: Header=BB8_63 Depth=1
	movq	8(%rax), %rbx
	movq	8(%rdi), %rsi
	cmpq	%rsi, %rbx
	je	.LBB8_63
.LBB8_66:                               # %while.end177
	testq	$512, %rdx              # imm = 0x200
	jne	.LBB8_69
# BB#67:                                # %lor.lhs.false182
	testq	$4096, %rdx             # imm = 0x1000
	je	.LBB8_70
# BB#68:                                # %land.lhs.true188
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB8_70
	jmp	.LBB8_69
.LBB8_56:                               # %copy_tree_r.exit
	cmpq	$50, %rcx
	jne	.LBB8_70
# BB#57:                                # %land.lhs.true96
	movq	56(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB8_70
# BB#58:                                # %if.then101
	movq	%rcx, 40(%rax)
	movq	(%r14), %rax
	movq	$0, 56(%rax)
	jmp	.LBB8_70
.LBB8_26:                               # %if.then88
	addq	$-114, %rcx
	andq	$255, %rcx
	cmpq	$2, %rcx
	jb	.LBB8_29
.LBB8_27:                               # %lor.lhs.false44.i.i
	movq	%rdx, %rdi
	callq	*lang_hooks+184(%rip)
	testq	%r12, %rax
	je	.LBB8_34
# BB#28:                                # %lor.lhs.false44.if.then_crit_edge.i.i
	movq	(%r14), %rdx
.LBB8_29:                               # %lor.lhs.false53.i.i
	movq	(%rdx), %rbx
	movq	%rdx, %rdi
	callq	copy_node
	movq	%rax, (%r14)
	movq	%rax, %rdi
	callq	*lang_hooks+184(%rip)
	testq	%r12, %rax
	jne	.LBB8_31
# BB#30:                                # %lor.lhs.false56.i.i
	movq	$166, %rdi
	callq	statement_code_p
	testq	%r12, %rax
	je	.LBB8_32
.LBB8_31:                               # %if.then59.i.i
	movq	(%r14), %rax
	movq	%rbx, (%rax)
.LBB8_32:                               # %if.end.i.i
	movq	(%r14), %rax
	movzbl	16(%rax), %ecx
	cmpq	$166, %rcx
	jne	.LBB8_36
# BB#33:                                # %if.then68.i.i
	movq	$0, 32(%rax)
	jmp	.LBB8_36
.LBB8_18:                               # %if.then.i
	movq	(%r14), %rdi
	callq	copy_node
	movq	%rax, %r12
	movq	%r13, 40(%r12)
	movq	$0, 48(%r12)
	movq	(%r14), %rsi
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	splay_tree_insert
	movq	global_trees(%rip), %rdx
	movq	%rax, %rbx
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	splay_tree_insert
	movq	8(%rbx), %rax
.LBB8_69:                               # %if.then196
	movq	%rax, (%r14)
	jmp	.LBB8_70
.LBB8_53:                               # %if.else.i274
	movzbl	tree_code_type(%rbx), %eax
	cmpq	$116, %rax
	jne	.LBB8_55
# BB#54:                                # %if.then76.i
	xorq	%rax, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%eax, (%rcx)
	jmp	.LBB8_55
.LBB8_34:                               # %if.else.i.i
	movzbl	tree_code_type+166(%rip), %eax
	cmpq	$116, %rax
	jne	.LBB8_36
# BB#35:                                # %if.then76.i.i
	xorq	%rax, %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movl	%eax, (%rcx)
.LBB8_36:                               # %copy_scope_stmt.exit
	movq	(%r14), %rax
	movq	%r15, 32(%rax)
	movq	(%r14), %rdi
	xorq	%rsi, %rsi
	movq	%r13, %rdx
	callq	remap_block
.LBB8_70:                               # %if.end205
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_12:                               # %if.then29
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.copy_body_r, %rcx
	movq	$381, %rsi              # imm = 0x17D
	jmp	.LBB8_13
.LBB8_21:                               # %if.then79
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.copy_body_r, %rcx
	movq	$396, %rsi              # imm = 0x18C
.LBB8_13:                               # %if.then29
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp122:
	.size	copy_body_r, .Ltmp122-copy_body_r
	.cfi_endproc

	.align	16, 0x90
	.type	remap_decl,@function
remap_decl:                             # @remap_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp129:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp130:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp131:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp132:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp133:
	.cfi_def_cfa_offset 48
.Ltmp134:
	.cfi_offset %rbx, -48
.Ltmp135:
	.cfi_offset %r12, -40
.Ltmp136:
	.cfi_offset %r13, -32
.Ltmp137:
	.cfi_offset %r14, -24
.Ltmp138:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	(%r12), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rbx
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	movq	%rbx, %rsi
	callq	*lang_hooks+192(%rip)
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rcx
	je	.LBB9_16
# BB#1:                                 # %if.end
	movq	24(%r12), %rdi
	movq	%r14, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	jne	.LBB9_15
# BB#2:                                 # %if.then4
	movq	(%r12), %rax
	movq	32(%rax), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	copy_decl_for_inlining
	movq	%rax, %r15
	movq	8(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_6
# BB#3:                                 # %land.lhs.true
	movzbl	16(%rdi), %eax
	cmpq	$18, %rax
	jne	.LBB9_6
# BB#4:                                 # %land.lhs.true14
	cmpq	$0, 24(%rdi)
	je	.LBB9_6
# BB#5:                                 # %if.then19
	callq	copy_node
	movq	%rax, 8(%r15)
	movq	24(%rax), %rdi
	callq	copy_node
	movq	8(%r15), %rcx
	movq	%rax, 24(%rcx)
	leaq	112(%rax), %rdi
	movabsq	$copy_body_r, %rsi
	xorq	%rcx, %rcx
	movq	%r12, %rdx
	callq	walk_tree
.LBB9_6:                                # %if.end40
	cmpq	$0, 72(%r15)
	jne	.LBB9_14
# BB#7:                                 # %land.lhs.true43
	movq	8(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB9_14
# BB#8:                                 # %land.lhs.true47
	callq	*lang_hooks+208(%rip)
	testq	%r13, %rax
	je	.LBB9_14
# BB#9:                                 # %if.then52
	movq	88(%r15), %rbx
	xorq	%r13, %r13
	jmp	.LBB9_10
.LBB9_12:                               # %if.end59
                                        #   in Loop: Header=BB9_10 Depth=1
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%r13, %rdx
	callq	tree_cons
	movq	(%rbx), %rbx
	movq	%rax, %r13
.LBB9_10:                               # %if.then52
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB9_13
# BB#11:                                # %for.body
                                        #   in Loop: Header=BB9_10 Depth=1
	movq	32(%rbx), %rdi
	movq	%r12, %rsi
	callq	remap_decl
	cmpq	$0, 24(%rbx)
	je	.LBB9_12
# BB#17:                                # %if.then58
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.remap_decl, %rcx
	movq	$176, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB9_13:                               # %for.end
	movq	%r13, %rdi
	callq	nreverse
	movq	%rax, 88(%r15)
.LBB9_14:                               # %if.end65
	movq	24(%r12), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	splay_tree_insert
.LBB9_15:                               # %if.end68
	movq	8(%rax), %rax
.LBB9_16:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp139:
	.size	remap_decl, .Ltmp139-remap_decl
	.cfi_endproc

	.align	16, 0x90
	.type	remap_block,@function
remap_block:                            # @remap_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp146:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp147:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp148:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp149:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp150:
	.cfi_def_cfa_offset 48
.Ltmp151:
	.cfi_offset %rbx, -48
.Ltmp152:
	.cfi_offset %r12, -40
.Ltmp153:
	.cfi_offset %r13, -32
.Ltmp154:
	.cfi_offset %r14, -24
.Ltmp155:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movl	32(%r12), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB10_2
# BB#1:                                 # %if.then
	movq	$0, 32(%r13)
.LBB10_21:                              # %if.end88
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB10_2:                               # %if.end
	movzbl	19(%r13), %eax
	movq	32(%r13), %r14
	testq	$1, %rax
	je	.LBB10_18
# BB#3:                                 # %land.lhs.true
	testq	%r14, %r14
	je	.LBB10_21
# BB#4:                                 # %if.then6
	movq	$4, %rdi
	callq	make_node
	movq	%rax, %r15
	movl	16(%r14), %eax
	andq	$65536, %rax            # imm = 0x10000
	movl	16(%r15), %ecx
	movabsq	$4294901759, %rdx       # imm = 0xFFFEFFFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r15)
	movq	%r14, 56(%r15)
	movq	%r15, 32(%r13)
	testq	%rbx, %rbx
	jne	.LBB10_7
# BB#5:                                 # %for.cond.preheader
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB10_6
.LBB10_7:                               # %for.body.lr.ph
	leaq	32(%r15), %r13
	.align	16, 0x90
.LBB10_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	remap_decl
	testq	%rax, %rax
	je	.LBB10_11
# BB#9:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB10_8 Depth=1
	movzbl	16(%rax), %ecx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB10_11
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB10_8 Depth=1
	movq	(%r13), %rcx
	movq	%rcx, (%rax)
	movq	%rax, (%r13)
.LBB10_11:                              # %for.inc
                                        #   in Loop: Header=BB10_8 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB10_8
	jmp	.LBB10_12
.LBB10_18:                              # %land.lhs.true70
	testq	%r14, %r14
	je	.LBB10_21
# BB#19:                                # %if.then75
	movq	24(%r12), %rdi
	movq	%r14, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB10_22
# BB#20:                                # %if.end83
	movq	8(%rax), %rax
	movq	%rax, 32(%r13)
	jmp	.LBB10_21
.LBB10_6:                               # %for.cond.preheader.for.end_crit_edge
	leaq	32(%r15), %r13
.LBB10_12:                              # %for.end
	movq	(%r13), %rdi
	callq	nreverse
	movq	%rax, (%r13)
	movzbl	60(%r12), %eax
	testq	%rax, %rax
	je	.LBB10_14
# BB#13:                                # %if.then47
	movq	%r15, %rdi
	callq	insert_block
	jmp	.LBB10_17
.LBB10_14:                              # %if.else48
	movq	(%r12), %rax
	movq	32(%rax), %rcx
	movq	104(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB10_16
# BB#15:
	addq	$104, %rcx
	movq	%rcx, %rax
.LBB10_16:                              # %if.else48
	movq	(%rax), %rcx
	movq	%rcx, (%r15)
	movq	%r15, (%rax)
.LBB10_17:                              # %if.end61
	movq	24(%r12), %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	splay_tree_insert  # TAILCALL
.LBB10_22:                              # %if.then82
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.remap_block, %rcx
	movq	$289, %rsi              # imm = 0x121
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp156:
	.size	remap_block, .Ltmp156-remap_block
	.cfi_endproc

	.align	16, 0x90
	.type	expand_call_inline,@function
expand_call_inline:                     # @expand_call_inline
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp163:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp164:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp165:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp166:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp168:
	.cfi_def_cfa_offset 144
.Ltmp169:
	.cfi_offset %rbx, -48
.Ltmp170:
	.cfi_offset %r12, -40
.Ltmp171:
	.cfi_offset %r13, -32
.Ltmp172:
	.cfi_offset %r14, -24
.Ltmp173:
	.cfi_offset %r15, -16
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movq	(%rdi), %r14
	movl	16(%r14), %eax
	movq	%rdx, %r15
	movq	%rsi, %rbx
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$50, %rcx
	jne	.LBB11_11
# BB#1:                                 # %if.then
	movq	$50, %rdi
	callq	first_rtl_op
	xorq	%rcx, %rcx
	movl	%ecx, (%rbx)
	movq	40(%r15), %rcx
	movq	(%rcx), %rsi
	movq	8(%rcx), %rdx
	movq	%rax, %r14
	cmpq	%rsi, %rdx
	jae	.LBB11_3
# BB#2:
	movq	%r15, %r12
	jmp	.LBB11_4
.LBB11_11:                              # %if.end28
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$116, %rcx
	jne	.LBB11_13
# BB#12:                                # %if.then37
	xorq	%rax, %rax
	movl	%eax, (%rbx)
	movl	16(%r14), %eax
.LBB11_13:                              # %if.end38
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	andq	$255, %rax
	cmpq	$53, %rax
	jne	.LBB11_86
# BB#14:                                # %if.end46
	movq	%r14, %rdi
	callq	get_callee_fndecl
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB11_86
# BB#15:                                # %if.end49
	cmpq	$0, 104(%rbx)
	jne	.LBB11_19
# BB#16:                                # %land.lhs.true
	movq	112(%rbx), %rax
	testq	%rax, %rax
	je	.LBB11_19
# BB#17:                                # %land.lhs.true53
	cmpq	$0, 168(%rax)
	je	.LBB11_19
# BB#18:                                # %select.mid
	movq	%rax, %rbx
.LBB11_19:                              # %if.end61
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	inlinable_function_p
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB11_86
# BB#20:                                # %if.end65
	movq	%rbx, %rdi
	callq	*lang_hooks+216(%rip)
	testq	%r12, %rax
	je	.LBB11_86
# BB#21:                                # %if.end69
	movq	24(%rbx), %rdi
	movl	32(%rbx), %esi
	callq	push_srcloc
	movq	8(%rbx), %rax
	movq	8(%rax), %rsi
	movq	$169, %rdi
	xorq	%rdx, %rdx
	callq	build1
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movl	16(%rax), %ecx
	orq	$16777216, %rcx         # imm = 0x1000000
	movl	%ecx, 16(%rax)
	movq	24(%r15), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movabsq	$splay_tree_compare_pointers, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	splay_tree_new
	movq	%rax, 24(%r15)
	movq	88(%rbx), %r13
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	40(%r14), %r12
	testq	%r13, %r13
	je	.LBB11_22
# BB#25:                                # %for.body.lr.ph.i
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	jmp	.LBB11_26
.LBB11_3:                               # %if.then3
	addq	%rsi, %rsi
	movq	%rcx, %rdi
	callq	varray_grow
	movq	%rax, %rcx
	movq	%rcx, 40(%r15)
	movq	%r15, %r12
	movq	8(%rcx), %rdx
.LBB11_4:                               # %if.end
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	leaq	1(%rdx), %rsi
	movq	%rsi, 8(%rcx)
	movq	%rax, 32(%rcx,%rdx,8)
	movq	$32, %r13
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB11_10
# BB#5:                                 # %for.body.lr.ph
	movq	$1, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r14
	.align	16, 0x90
.LBB11_6:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%rbx), %rax
	andq	%r15, %rax
	cmpq	$2, %rax
	jne	.LBB11_8
# BB#7:                                 # %if.then15
                                        #   in Loop: Header=BB11_6 Depth=1
	movl	32(%r12), %eax
	incq	%rax
	movl	%eax, 32(%r12)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	64(%r12), %rcx
	addq	%r13, %rdi
	movabsq	$expand_call_inline, %rsi
	movq	%r12, %rdx
	callq	walk_tree
	movl	32(%r12), %eax
	decq	%rax
	movl	%eax, 32(%r12)
	jmp	.LBB11_9
	.align	16, 0x90
.LBB11_8:                               # %for.inc.critedge
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	%r12, %rdx
	movq	64(%rdx), %rcx
	addq	%r13, %rdi
	movabsq	$expand_call_inline, %rsi
                                        # kill: R12<def> RDX<kill>
	callq	walk_tree
.LBB11_9:                               # %for.inc
                                        #   in Loop: Header=BB11_6 Depth=1
	movq	%rbx, %rax
	andq	%r15, %rax
	incq	%rbx
	addq	$8, %r13
	cmpq	%r14, %rax
	jne	.LBB11_6
.LBB11_10:                              # %for.end
	movq	40(%r12), %rax
	decq	8(%rax)
.LBB11_86:                              # %return
	xorq	%rax, %rax
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_39:                              # %if.then48.i
                                        #   in Loop: Header=BB11_26 Depth=1
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB11_41
.LBB11_40:                              # %if.then58.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	8(%rax), %rsi
	movq	$115, %rdi
	movq	%rax, %rdx
	callq	build1
.LBB11_41:                              # %if.end.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	24(%r15), %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	splay_tree_insert
	jmp	.LBB11_46
	.align	16, 0x90
.LBB11_26:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	movabsq	$0, %rsi
	je	.LBB11_28
# BB#27:                                # %cond.true.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	32(%r12), %rsi
.LBB11_28:                              # %cond.end.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	%r13, %rdi
	movq	48(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	movq	lang_hooks+232(%rip), %rax
	callq	*%rax
	movl	16(%r13), %ecx
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB11_29
# BB#31:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB11_26 Depth=1
	testq	$1024, %rcx             # imm = 0x400
	jne	.LBB11_29
# BB#32:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB11_26 Depth=1
	testq	%rax, %rax
	je	.LBB11_29
# BB#33:                                # %land.lhs.true11.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movl	16(%rax), %ecx
	testq	$256, %rcx              # imm = 0x100
	jne	.LBB11_29
# BB#34:                                # %if.then.i
                                        #   in Loop: Header=BB11_26 Depth=1
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	jne	.LBB11_36
# BB#35:                                # %cond.true21.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	%rax, %rdi
	callq	decl_constant_value
.LBB11_36:                              # %cond.end24.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	%rax, %rdi
	callq	fold
	movl	16(%rax), %ecx
	testq	$512, %rcx              # imm = 0x200
	jne	.LBB11_39
# BB#37:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB11_26 Depth=1
	testq	$4096, %rcx             # imm = 0x1000
	je	.LBB11_29
# BB#38:                                # %land.lhs.true38.i
                                        #   in Loop: Header=BB11_26 Depth=1
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$100, %rcx
	je	.LBB11_40
	.align	16, 0x90
.LBB11_29:                              #   in Loop: Header=BB11_26 Depth=1
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	(%r15), %rax
	movq	32(%rax), %rdx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	copy_decl_for_inlining
	movq	24(%r15), %rdi
	movq	%rax, %rbx
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_insert
	movq	$154, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	build_stmt
	movq	%rax, %r14
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r14)
	movq	8(%r13), %rax
	movzbl	62(%rax), %eax
	testq	$4, %rax
	jne	.LBB11_42
# BB#30:                                # %if.then77.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	movq	%rax, 104(%rbx)
	jmp	.LBB11_43
	.align	16, 0x90
.LBB11_42:                              # %if.else.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movl	16(%rbx), %eax
	movabsq	$4294963199, %rcx       # imm = 0xFFFFEFFF
	andq	%rcx, %rax
	movl	%eax, 16(%rbx)
	movq	8(%r13), %rsi
	movq	$49, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	56(%rsp), %rcx          # 8-byte Reload
	callq	build
	movq	%rax, %rcx
	movq	$152, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	build_stmt
	movq	%r14, (%rax)
	movq	%rax, %r14
.LBB11_43:                              # %if.end89.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	%rbx, %rdi
	callq	maybe_build_cleanup
	movq	%rax, %rcx
	testq	%rcx, %rcx
	jne	.LBB11_45
# BB#44:                                #   in Loop: Header=BB11_26 Depth=1
	movq	%r14, 64(%rsp)          # 8-byte Spill
	jmp	.LBB11_46
	.align	16, 0x90
.LBB11_45:                              # %if.then92.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	$171, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	build_stmt
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	%r14, (%rax)
.LBB11_46:                              # %for.inc.i
                                        #   in Loop: Header=BB11_26 Depth=1
	xorq	%rbx, %rbx
	testq	%r12, %r12
	je	.LBB11_48
# BB#47:                                # %cond.true98.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	(%r12), %rbx
.LBB11_48:                              # %cond.end102.i
                                        #   in Loop: Header=BB11_26 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%rbx, %r12
	jne	.LBB11_26
	jmp	.LBB11_23
.LBB11_22:
	movq	$0, 64(%rsp)            # 8-byte Folded Spill
	movq	%r12, %rbx
.LBB11_23:                              # %for.cond106.preheader.i
	testq	%rbx, %rbx
	movq	%r15, %r12
	je	.LBB11_24
# BB#49:
	movq	64(%rsp), %r14          # 8-byte Reload
	.align	16, 0x90
.LBB11_50:                              # %for.body108.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB11_53
# BB#51:                                # %lor.lhs.false114.i
                                        #   in Loop: Header=BB11_50 Depth=1
	movzbl	17(%rsi), %eax
	testq	$1, %rax
	je	.LBB11_53
# BB#52:                                # %if.end122.i
                                        #   in Loop: Header=BB11_50 Depth=1
	movq	$152, %rdi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%r14, (%rax)
	movq	%rax, %r14
.LBB11_53:                              # %for.inc126.i
                                        #   in Loop: Header=BB11_50 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB11_50
	jmp	.LBB11_54
.LBB11_24:
	movq	64(%rsp), %r14          # 8-byte Reload
.LBB11_54:                              # %initialize_inlined_parameters.exit
	movq	%r14, %rdi
	callq	nreverse
	movq	%rax, 80(%rsp)
	movq	64(%r12), %rcx
	movabsq	$expand_call_inline, %rsi
	leaq	80(%rsp), %rdi
	movq	%r12, %rdx
	callq	walk_tree
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rdi
	movq	80(%rsp), %rsi
	callq	chainon
	movq	%rax, 32(%rbx)
	movq	(%r12), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB11_56
# BB#55:                                # %if.then98
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, (%r12)
	movq	8(%rax), %rcx
.LBB11_56:                              # %if.end105
	movq	48(%rsp), %rbx          # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	cmpq	$0, 176(%rbx)
	jne	.LBB11_65
# BB#57:                                # %if.then116
	movq	48(%r12), %rax
	movq	8(%rax), %rdx
	addq	%rdx, %rdi
	movq	$32, %rcx
	movq	%rdi, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	js	.LBB11_62
# BB#58:                                # %for.body124.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	.align	16, 0x90
.LBB11_60:                              # %for.body124
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax,%rdi,8), %rcx
	cmpq	%rbx, %rcx
	je	.LBB11_61
# BB#59:                                # %for.cond121
                                        #   in Loop: Header=BB11_60 Depth=1
	movq	$32, %rcx
	leaq	-1(%rdi), %rsi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rsi, %rdi
	jg	.LBB11_60
	jmp	.LBB11_62
.LBB11_61:                              # %for.end136
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jns	.LBB11_65
.LBB11_62:                              # %do.body140
	movq	(%rax), %rsi
	cmpq	%rsi, %rdx
	jb	.LBB11_64
# BB#63:                                # %if.then147
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, 48(%r12)
	movq	8(%rax), %rdx
.LBB11_64:                              # %if.end154
	leaq	1(%rdx), %rcx
	movq	%rcx, 8(%rax)
	movq	%rbx, 32(%rax,%rdx,8)
.LBB11_65:                              # %if.end164
	movq	$31, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	build_decl
	movq	%rax, 16(%r12)
	movq	(%r12), %rcx
	movq	32(%rcx), %rcx
	movq	%rcx, 80(%rax)
	movq	104(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB11_87
# BB#66:                                # %lor.lhs.false
	movzbl	16(%rsi), %eax
	cmpq	$4, %rax
	jne	.LBB11_87
# BB#67:                                # %if.end184
	movq	32(%rsp), %rax          # 8-byte Reload
	leaq	32(%rax), %r14
	movq	$166, %rdi
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	build_stmt
	movq	%rax, %r13
	movl	16(%r13), %eax
	orq	$150994944, %rax        # imm = 0x9000000
	movl	%eax, 16(%r13)
	movq	88(%rbx), %rsi
	movq	%r13, %rdi
	movq	%r12, %rdx
	callq	remap_block
	movq	(%r14), %rax
	movq	%rax, (%r13)
	movq	%r13, (%r14)
	movq	32(%r13), %rax
	testq	%rax, %rax
	je	.LBB11_71
# BB#68:                                # %if.then210
	movq	112(%rbx), %rcx
	testq	%rcx, %rcx
	movq	%rbx, %rdx
	je	.LBB11_70
# BB#69:                                # %if.then210
	movq	%rcx, %rdx
.LBB11_70:                              # %if.then210
	movq	%rdx, 56(%rax)
	movq	(%r14), %r13
.LBB11_71:                              # %if.end220
	movq	(%r12), %rax
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rbx
	movq	96(%rbx), %r12
	testq	%r12, %r12
	movq	$1, %rcx
	movl	%ecx, 88(%rsp)
	je	.LBB11_72
# BB#73:                                # %lor.lhs.false.i454
	movq	8(%r12), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$5, %rcx
	jne	.LBB11_74
.LBB11_72:
	xorq	%r12, %r12
	movq	48(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB11_80
.LBB11_74:                              # %if.end.i457
	movq	32(%rax), %rdx
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	24(%r15), %rcx
	leaq	40(%r15), %r9
	leaq	88(%rsp), %r8
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	*lang_hooks+200(%rip)
	movq	24(%r15), %rdi
	movq	%rax, %r15
	movq	%r12, %rsi
	movq	%r15, %rdx
	callq	splay_tree_insert
	movq	8(%rbx), %rax
	movq	8(%r15), %rcx
	movq	8(%rax), %rsi
	cmpq	%rsi, %rcx
	je	.LBB11_75
# BB#76:                                # %if.else.i458
	movq	$115, %rdi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %rcx
	movq	$152, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	jmp	.LBB11_77
.LBB11_75:                              # %if.then17.i
	movq	$152, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
.LBB11_77:                              # %if.end25.i
	callq	build_stmt
	movq	%rax, %r12
	movl	16(%r12), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%r12)
	movl	88(%rsp), %eax
	testq	%rax, %rax
	movq	48(%rsp), %rbx          # 8-byte Reload
	je	.LBB11_78
# BB#79:                                # %if.then30.i
	movq	$154, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	build_stmt
	movq	%rax, %r15
	jmp	.LBB11_80
.LBB11_78:
	xorq	%r15, %r15
.LBB11_80:                              # %declare_return_variable.exit
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	chainon
	movq	%rax, (%r14)
	jmp	.LBB11_81
	.align	16, 0x90
.LBB11_82:                              # %while.cond.while.cond_crit_edge
                                        #   in Loop: Header=BB11_81 Depth=1
	movq	%rax, %r14
	movq	(%rax), %rax
.LBB11_81:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB11_82
# BB#83:                                # %while.end
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	24(%rax,%rcx,8), %rax
	movq	168(%rax), %rax
	movq	%rax, 88(%rsp)
	movabsq	$copy_body_r, %rsi
	leaq	88(%rsp), %rdi
	xorq	%rcx, %rcx
	movq	%r15, %rdx
	callq	walk_tree
	movq	88(%rsp), %rax
	movq	%rax, (%r14)
	movq	104(%rbx), %rsi
	movq	$166, %rdi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$134217728, %rax        # imm = 0x8000000
	movl	%eax, 16(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r15, %rdx
	callq	remap_block
	movq	32(%rsp), %r13          # 8-byte Reload
	movq	32(%r13), %rdi
	movq	%rbx, %rsi
	callq	chainon
	movq	%rax, %rbx
	movq	%rbx, 32(%r13)
	movq	16(%r15), %rsi
	movq	$164, %rdi
	xorq	%rax, %rax
	callq	build_stmt
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	chainon
	movq	%rax, 32(%r13)
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	chainon
	movq	%rax, 32(%r13)
	movq	24(%r15), %rdi
	callq	splay_tree_delete
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 24(%r15)
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movl	16(%r13), %ecx
	andq	$256, %rax              # imm = 0x100
	movabsq	$4294967039, %rdx       # imm = 0xFFFFFEFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r13)
	movq	72(%rsp), %r12          # 8-byte Reload
	movq	(%r12), %rax
	movq	(%rax), %rbx
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rsi
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	32(%rax), %edx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	build_expr_wfl
	movq	%rax, (%r12)
	movl	16(%rax), %ecx
	orq	$524288, %rcx           # imm = 0x80000
	movl	%ecx, 16(%rax)
	movq	(%r12), %rax
	movq	%rbx, (%rax)
	movq	%r15, %rbx
	callq	pop_srcloc
	movq	(%r12), %rax
	movq	48(%rsp), %r15          # 8-byte Reload
	movl	16(%rax), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%rax)
	movq	(%rbx), %rax
	movq	56(%r15), %rcx
	movq	32(%rax), %rax
	addq	%rcx, 56(%rax)
	movq	56(%r15), %rax
	movl	56(%rbx), %ecx
	addq	%rax, %rcx
	movl	%ecx, 56(%rbx)
	movq	64(%rbx), %rcx
	movabsq	$expand_call_inline, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	walk_tree
	movq	(%rbx), %rax
	movq	8(%rax), %rcx
	decq	%rcx
	movq	%rcx, 8(%rax)
	movl	8(%rbx), %eax
	cmpq	%rax, %rcx
	movabsq	$0, %rcx
	jne	.LBB11_85
# BB#84:                                # %if.then317
	movl	%ecx, 56(%rbx)
.LBB11_85:                              # %if.end319
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	%ecx, (%rax)
	movq	%r15, %rdi
	callq	*lang_hooks+224(%rip)
	jmp	.LBB11_86
.LBB11_87:                              # %if.then183
	movabsq	$.L.str3, %rax
	movabsq	$.L__FUNCTION__.expand_call_inline, %rcx
	movq	$900, %rsi              # imm = 0x384
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp174:
	.size	expand_call_inline, .Ltmp174-expand_call_inline
	.cfi_endproc

	.type	flag_inline_trees,@object # @flag_inline_trees
	.bss
	.globl	flag_inline_trees
	.align	4
flag_inline_trees:
	.long	0                       # 0x0
	.size	flag_inline_trees, 4

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"fns"
	.size	.L.str, 4

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"target_exprs"
	.size	.L.str1, 13

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"inlined_fns"
	.size	.L.str2, 12

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"tree-inline.c"
	.size	.L.str3, 14

	.type	.L__FUNCTION__.walk_tree,@object # @__FUNCTION__.walk_tree
.L__FUNCTION__.walk_tree:
	.asciz	"walk_tree"
	.size	.L__FUNCTION__.walk_tree, 10

	.type	.L__FUNCTION__.copy_body_r,@object # @__FUNCTION__.copy_body_r
.L__FUNCTION__.copy_body_r:
	.asciz	"copy_body_r"
	.size	.L__FUNCTION__.copy_body_r, 12

	.type	.L__FUNCTION__.remap_block,@object # @__FUNCTION__.remap_block
.L__FUNCTION__.remap_block:
	.asciz	"remap_block"
	.size	.L__FUNCTION__.remap_block, 12

	.type	.L__FUNCTION__.remap_decl,@object # @__FUNCTION__.remap_decl
.L__FUNCTION__.remap_decl:
	.asciz	"remap_decl"
	.size	.L__FUNCTION__.remap_decl, 11

	.type	.L__FUNCTION__.expand_call_inline,@object # @__FUNCTION__.expand_call_inline
.L__FUNCTION__.expand_call_inline:
	.asciz	"expand_call_inline"
	.size	.L__FUNCTION__.expand_call_inline, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
