	.file	"tree.c.bc"
	.text
	.globl	set_decl_assembler_name
	.align	16, 0x90
	.type	set_decl_assembler_name,@function
set_decl_assembler_name:                # @set_decl_assembler_name
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$30, %rcx
	je	.LBB0_5
# BB#1:                                 # %entry
	cmpq	$34, %rcx
	jne	.LBB0_6
# BB#2:                                 # %land.lhs.true
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB0_5
# BB#3:                                 # %lor.lhs.false9
	testq	$256, 48(%rdi)          # imm = 0x100
	jne	.LBB0_5
# BB#4:                                 # %lor.lhs.false9
	andq	$524288, %rax           # imm = 0x80000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB0_6
.LBB0_5:                                # %if.then
	movq	72(%rdi), %rax
	movq	%rax, 120(%rdi)
	popq	%rax
	retq
.LBB0_6:                                # %if.else
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.set_decl_assembler_name, %rcx
	movq	$216, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp2:
	.size	set_decl_assembler_name, .Ltmp2-set_decl_assembler_name
	.cfi_endproc

	.globl	init_obstacks
	.align	16, 0x90
	.type	init_obstacks,@function
init_obstacks:                          # @init_obstacks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movabsq	$permanent_obstack, %rdi
	callq	gcc_obstack_init
	movabsq	$type_hash_hash, %rsi
	movabsq	$type_hash_eq, %rdx
	movq	$1000, %rdi             # imm = 0x3E8
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, type_hash_table(%rip)
	movabsq	$type_hash_marked_p, %rsi
	movabsq	$type_hash_mark, %rdx
	movq	%rax, %rdi
	callq	ggc_add_deletable_htab
	movabsq	$global_trees, %rdi
	movq	$51, %rsi
	callq	ggc_add_tree_root
	movabsq	$integer_types, %rdi
	movq	$11, %rsi
	callq	ggc_add_tree_root
	movq	$set_decl_assembler_name, lang_set_decl_assembler_name(%rip)
	popq	%rax
	retq
.Ltmp5:
	.size	init_obstacks, .Ltmp5-init_obstacks
	.cfi_endproc

	.align	16, 0x90
	.type	type_hash_hash,@function
type_hash_hash:                         # @type_hash_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	retq
.Ltmp6:
	.size	type_hash_hash, .Ltmp6-type_hash_hash
	.cfi_endproc

	.align	16, 0x90
	.type	type_hash_eq,@function
type_hash_eq:                           # @type_hash_eq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp13:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp14:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp15:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp16:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 48
.Ltmp18:
	.cfi_offset %rbx, -48
.Ltmp19:
	.cfi_offset %r12, -40
.Ltmp20:
	.cfi_offset %r13, -32
.Ltmp21:
	.cfi_offset %r14, -24
.Ltmp22:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_32
# BB#1:                                 # %land.lhs.true
	movq	8(%rbx), %rax
	movq	8(%r14), %rcx
	movl	16(%rax), %edx
	movl	16(%rcx), %esi
	xorq	%rdx, %rsi
	testq	$255, %rsi
	jne	.LBB3_32
# BB#2:                                 # %land.lhs.true8
	movq	8(%rax), %rdx
	movq	8(%rcx), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB3_32
# BB#3:                                 # %land.lhs.true16
	movq	48(%rax), %r15
	movq	48(%rcx), %r12
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	attribute_list_contained
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB3_32
# BB#4:                                 # %land.rhs.i
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	attribute_list_contained
	testq	%r13, %rax
	je	.LBB3_32
# BB#5:                                 # %land.lhs.true22
	movq	8(%rbx), %rax
	movq	8(%r14), %rcx
	movl	64(%rax), %edx
	movl	64(%rcx), %esi
	cmpq	%rsi, %rdx
	jne	.LBB3_32
# BB#6:                                 # %land.lhs.true29
	movq	112(%rax), %rdx
	movq	112(%rcx), %rsi
	cmpq	%rsi, %rdx
	je	.LBB3_13
# BB#7:                                 # %if.end.i
	testq	%rdx, %rdx
	je	.LBB3_32
# BB#8:                                 # %if.end.i
	testq	%rsi, %rsi
	je	.LBB3_32
# BB#9:                                 # %if.end4.i
	movzbl	16(%rdx), %edi
	cmpq	$25, %rdi
	jne	.LBB3_32
# BB#10:                                # %land.lhs.true.i
	movzbl	16(%rsi), %edi
	cmpq	$25, %rdi
	jne	.LBB3_32
# BB#11:                                # %land.lhs.true11.i
	movq	32(%rdx), %rdi
	movq	32(%rsi), %rbx
	cmpq	%rbx, %rdi
	jne	.LBB3_32
# BB#12:                                # %land.lhs.true17.i
	movq	40(%rdx), %rdx
	movq	40(%rsi), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB3_32
.LBB3_13:                               # %land.lhs.true44
	movq	104(%rax), %rdx
	movq	104(%rcx), %rsi
	cmpq	%rsi, %rdx
	je	.LBB3_20
# BB#14:                                # %if.end.i132
	testq	%rdx, %rdx
	je	.LBB3_32
# BB#15:                                # %if.end.i132
	testq	%rsi, %rsi
	je	.LBB3_32
# BB#16:                                # %if.end4.i137
	movzbl	16(%rdx), %edi
	cmpq	$25, %rdi
	jne	.LBB3_32
# BB#17:                                # %land.lhs.true.i142
	movzbl	16(%rsi), %edi
	cmpq	$25, %rdi
	jne	.LBB3_32
# BB#18:                                # %land.lhs.true11.i148
	movq	32(%rdx), %rdi
	movq	32(%rsi), %rbx
	cmpq	%rbx, %rdi
	jne	.LBB3_32
# BB#19:                                # %land.lhs.true17.i152
	movq	40(%rdx), %rdx
	movq	40(%rsi), %rsi
	cmpq	%rsi, %rdx
	jne	.LBB3_32
.LBB3_20:                               # %land.lhs.true60
	movq	24(%rax), %r14
	movq	24(%rcx), %r15
	cmpq	%r15, %r14
	movq	$1, %rbx
	je	.LBB3_33
# BB#21:                                # %lor.lhs.false67
	testq	%r14, %r14
	je	.LBB3_32
# BB#22:                                # %land.lhs.true72
	movzbl	16(%r14), %eax
	cmpq	$2, %rax
	jne	.LBB3_32
# BB#23:                                # %land.lhs.true72
	testq	%r15, %r15
	je	.LBB3_32
# BB#24:                                # %land.lhs.true86
	movzbl	16(%r15), %eax
	cmpq	$2, %rax
	jne	.LBB3_32
.LBB3_25:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rax
	movq	32(%r15), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_32
# BB#26:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	24(%r14), %rdi
	movq	24(%r15), %rsi
	cmpq	%rsi, %rdi
	je	.LBB3_29
# BB#27:                                # %land.lhs.true.i158
                                        #   in Loop: Header=BB3_25 Depth=1
	callq	simple_cst_equal
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB3_32
# BB#28:                                # %land.lhs.true13.i
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	24(%r14), %rax
	movq	24(%r15), %rcx
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB3_32
.LBB3_29:                               # %for.inc.i
                                        #   in Loop: Header=BB3_25 Depth=1
	movq	(%r15), %r15
	movq	(%r14), %r14
	testq	%r15, %r15
	je	.LBB3_31
# BB#30:                                # %for.inc.i
                                        #   in Loop: Header=BB3_25 Depth=1
	testq	%r14, %r14
	jne	.LBB3_25
.LBB3_31:                               # %type_list_equal.exit
	cmpq	%r15, %r14
	je	.LBB3_33
.LBB3_32:                               # %if.end
	xorq	%rbx, %rbx
.LBB3_33:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp23:
	.size	type_hash_eq, .Ltmp23-type_hash_eq
	.cfi_endproc

	.align	16, 0x90
	.type	type_hash_marked_p,@function
type_hash_marked_p:                     # @type_hash_marked_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 16
.Ltmp27:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rbx
	movq	%rbx, %rdi
	callq	ggc_marked_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$1, %rax
	jne	.LBB4_4
# BB#1:                                 # %lor.rhs
	cmpq	$0, 88(%rbx)
	je	.LBB4_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %rax
.LBB4_4:                                # %lor.end
	popq	%rbx
	retq
.LBB4_2:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp28:
	.size	type_hash_marked_p, .Ltmp28-type_hash_marked_p
	.cfi_endproc

	.align	16, 0x90
	.type	type_hash_mark,@function
type_hash_mark:                         # @type_hash_mark
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp31:
	.cfi_def_cfa_offset 16
.Ltmp32:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB5_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	ggc_set_mark
.LBB5_2:                                # %do.body1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB5_7
# BB#3:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB5_7
# BB#4:                                 # %do.body5
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB5_6
# BB#5:                                 # %if.then7
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB5_6:                                # %if.end10
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB5_7:                                # %do.end14
	popq	%rbx
	retq
.Ltmp33:
	.size	type_hash_mark, .Ltmp33-type_hash_mark
	.cfi_endproc

	.globl	permalloc
	.align	16, 0x90
	.type	permalloc,@function
permalloc:                              # @permalloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp36:
	.cfi_def_cfa_offset 16
.Ltmp37:
	.cfi_offset %rbx, -16
	movq	%rdi, %rax
	movq	permanent_obstack+32(%rip), %rsi
	movq	permanent_obstack+24(%rip), %rdx
	subq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rsi
	jge	.LBB6_2
# BB#1:                                 # %if.then
	movabsq	$permanent_obstack, %rdi
	movq	%rax, %rsi
	callq	_obstack_newchunk
	movq	permanent_obstack+24(%rip), %rdx
.LBB6_2:                                # %if.end
	addq	%rbx, %rdx
	movq	%rdx, permanent_obstack+24(%rip)
	movq	permanent_obstack+16(%rip), %rax
	cmpq	%rax, %rdx
	jne	.LBB6_4
# BB#3:                                 # %if.then8
	movzbl	permanent_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, permanent_obstack+80(%rip)
.LBB6_4:                                # %if.end9
	movslq	permanent_obstack+48(%rip), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, permanent_obstack+24(%rip)
	movq	permanent_obstack+32(%rip), %rbx
	movq	permanent_obstack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB6_6
# BB#5:                                 # %if.then29
	movq	%rbx, permanent_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB6_6:                                # %if.end32
	movq	%rcx, permanent_obstack+16(%rip)
	popq	%rbx
	retq
.Ltmp38:
	.size	permalloc, .Ltmp38-permalloc
	.cfi_endproc

	.globl	perm_calloc
	.align	16, 0x90
	.type	perm_calloc,@function
perm_calloc:                            # @perm_calloc
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
	movq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	imulq	%rsi, %r15
	movq	permanent_obstack+32(%rip), %rdx
	movq	permanent_obstack+24(%rip), %rax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jge	.LBB7_2
# BB#1:                                 # %if.then
	movabsq	$permanent_obstack, %rdi
	movq	%r15, %rsi
	callq	_obstack_newchunk
	movq	permanent_obstack+24(%rip), %rax
.LBB7_2:                                # %if.end
	addq	%rbx, %rax
	movq	%rax, permanent_obstack+24(%rip)
	movq	permanent_obstack+16(%rip), %r14
	cmpq	%r14, %rax
	jne	.LBB7_4
# BB#3:                                 # %if.then10
	movzbl	permanent_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, permanent_obstack+80(%rip)
.LBB7_4:                                # %if.end11
	movslq	permanent_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, permanent_obstack+24(%rip)
	movq	permanent_obstack+32(%rip), %rax
	movq	permanent_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB7_6
# BB#5:                                 # %if.then31
	movq	%rax, permanent_obstack+24(%rip)
	movq	%rax, %rcx
.LBB7_6:                                # %if.end34
	movq	%rcx, permanent_obstack+16(%rip)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%r15, %rdx
	callq	memset
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp49:
	.size	perm_calloc, .Ltmp49-perm_calloc
	.cfi_endproc

	.globl	tree_size
	.align	16, 0x90
	.type	tree_size,@function
tree_size:                              # @tree_size
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp51:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %ecx
	movsbq	tree_code_type(%rcx), %rax
	cmpq	$59, %rax
	jg	.LBB8_3
# BB#1:                                 # %entry
	addq	$-49, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$2, %rdx
	jb	.LBB8_5
	jmp	.LBB8_2
.LBB8_3:                                # %entry
	cmpq	$97, %rax
	jg	.LBB8_6
# BB#4:                                 # %entry
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$60, %rax
	je	.LBB8_5
	jmp	.LBB8_2
.LBB8_6:                                # %entry
	cmpq	$113, %rax
	jg	.LBB8_9
# BB#7:                                 # %entry
	addq	$-98, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$3, %rdx
	ja	.LBB8_2
# BB#8:                                 # %entry
	movq	$208, %rax
	jmpq	*.LJTI8_0(,%rdx,8)
.LBB8_19:                               # %sw.bb3
	movq	$80, %rax
	jmp	.LBB8_20
.LBB8_9:                                # %entry
	leaq	-114(%rax), %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$2, %rsi
	jae	.LBB8_10
.LBB8_5:                                # %sw.bb4
	movl	tree_code_length(,%rcx,4), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	40(,%rax,8), %rax
	popq	%rdx
	retq
.LBB8_10:                               # %entry
	andq	%rdx, %rax
	cmpq	$120, %rax
	jne	.LBB8_11
# BB#17:                                # %sw.bb19
	movslq	tree_code_length(,%rcx,4), %rax
	leaq	24(,%rax,8), %rax
	cmpq	$3, %rcx
	jne	.LBB8_20
# BB#18:                                # %if.then27
	movl	24(%rdi), %edx
	decq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rax,%rdx,8), %rax
	popq	%rdx
	retq
.LBB8_11:                               # %entry
	cmpq	$116, %rax
	jne	.LBB8_2
# BB#12:                                # %sw.bb2
	movq	$168, %rax
	popq	%rdx
	retq
.LBB8_13:                               # %sw.bb8
	movq	$48, %rax
	cmpq	$25, %rcx
	jne	.LBB8_14
.LBB8_20:                               # %return
	popq	%rdx
	retq
.LBB8_14:                               # %sw.bb8
	cmpq	$26, %rcx
	jne	.LBB8_16
# BB#15:                                # %if.then12
	movq	$56, %rax
	popq	%rdx
	retq
.LBB8_2:                                # %sw.default
	movabsq	$.L.str147, %rdi
	movabsq	$.L__FUNCTION__.tree_size, %rdx
	movq	$316, %rsi              # imm = 0x13C
	callq	fancy_abort
.LBB8_16:                               # %if.else13
	movslq	tree_code_length(,%rcx,4), %rax
	leaq	24(,%rax,8), %rax
	popq	%rdx
	retq
.Ltmp52:
	.size	tree_size, .Ltmp52-tree_size
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_19
	.quad	.LBB8_13
	.quad	.LBB8_20
	.quad	.LBB8_5

	.text
	.globl	make_node
	.align	16, 0x90
	.type	make_node,@function
make_node:                              # @make_node
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
	subq	$16, %rsp
.Ltmp64:
	.cfi_def_cfa_offset 64
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
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	%rdi, %rax
	andq	%rsi, %rax
	cmpq	$3, %rax
	je	.LBB9_41
# BB#1:                                 # %if.end
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdi, %r14
	andq	$255, %r14
	movsbq	tree_code_type(%r14), %rax
	movsbq	tree_code_type(%rdx), %r13
	cmpq	$59, %rax
	jg	.LBB9_4
# BB#2:                                 # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	addq	$-49, %rax
	andq	%rsi, %rax
	cmpq	$2, %rax
	jb	.LBB9_6
	jmp	.LBB9_3
.LBB9_4:                                # %if.end
	cmpq	$97, %rax
	jg	.LBB9_7
# BB#5:                                 # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	andq	%rsi, %rax
	cmpq	$60, %rax
	je	.LBB9_6
	jmp	.LBB9_3
.LBB9_7:                                # %if.end
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	cmpq	$113, %rax
	jg	.LBB9_10
# BB#8:                                 # %if.end
	addq	$-98, %rax
	andq	%rsi, %rax
	cmpq	$3, %rax
	ja	.LBB9_3
# BB#9:                                 # %if.end
	movq	$208, %rbx
	jmpq	*.LJTI9_0(,%rax,8)
.LBB9_18:                               # %sw.bb3.i
	movq	$80, %rbx
	jmp	.LBB9_19
.LBB9_10:                               # %if.end
	leaq	-114(%rax), %rcx
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	jae	.LBB9_11
.LBB9_6:                                # %sw.bb4.i
	movl	tree_code_length(,%r14,4), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	40(,%rax,8), %rbx
.LBB9_19:                               # %tree_size.exit
	movq	%rsi, %r12
	movq	%rbx, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	memset
	movl	16(%r15), %eax
	andq	$-256, %rax
	orq	%r14, %rax
	movl	%eax, 16(%r15)
	cmpq	$114, %r13
	jg	.LBB9_27
# BB#20:                                # %tree_size.exit
	movq	%r12, %rdx
	andq	%rdx, %r13
	cmpq	$99, %r13
	je	.LBB9_38
# BB#21:                                # %tree_size.exit
	cmpq	$100, %r13
	jne	.LBB9_22
# BB#30:                                # %sw.bb16
	movq	8(%rsp), %rax           # 8-byte Reload
	andq	%rdx, %rax
	cmpq	$30, %rax
	je	.LBB9_32
# BB#31:                                # %if.then19
	movl	56(%r15), %eax
	movabsq	$4278190080, %rcx       # imm = 0xFF000000
	andq	%rax, %rcx
	orq	$1, %rcx
	movl	%ecx, 56(%r15)
.LBB9_32:                               # %if.end23
	movq	48(%r15), %rax
	movabsq	$-34359803905, %rcx     # imm = 0xFFFFFFF7FFFEFFFF
	leaq	65536(%rcx), %rdx
	andq	%rax, %rdx
	movq	%rdx, 48(%r15)
	movl	in_system_header(%rip), %edx
	andq	$1, %rdx
	shlq	$16, %rdx
	andq	%rcx, %rax
	orq	%rdx, %rax
	movq	%rax, 48(%r15)
	movl	lineno(%rip), %eax
	movl	%eax, 32(%r15)
	movq	input_filename(%rip), %rax
	testq	%rax, %rax
	jne	.LBB9_34
# BB#33:                                # %if.end23
	movabsq	$.L.str148, %rax
.LBB9_34:                               # %if.end23
	movq	%rax, 24(%r15)
	movl	next_decl_uid(%rip), %eax
	leaq	1(%rax), %rcx
	movl	%ecx, next_decl_uid(%rip)
	movl	%eax, 36(%r15)
	movq	$-1, 192(%r15)
	jmp	.LBB9_40
.LBB9_27:                               # %tree_size.exit
	andq	%r12, %r13
	cmpq	$116, %r13
	jne	.LBB9_28
# BB#35:                                # %sw.bb36
	movl	next_type_uid(%rip), %eax
	leaq	1(%rax), %rcx
	movl	%ecx, next_type_uid(%rip)
	movl	%eax, 56(%r15)
	movq	integer_types(%rip), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB9_37
# BB#36:                                # %cond.true41
	movl	64(%rcx), %eax
.LBB9_37:                               # %cond.end44
	movl	%eax, 64(%r15)
	movl	60(%r15), %eax
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	movl	%eax, 60(%r15)
	movq	%r15, 128(%r15)
	movq	$0, 48(%r15)
	movq	%r15, %rdi
	callq	*targetm+272(%rip)
	movq	$-1, 152(%r15)
	jmp	.LBB9_40
.LBB9_28:                               # %tree_size.exit
	cmpq	$115, %r13
	jne	.LBB9_40
# BB#29:                                # %sw.bb
	orq	$256, %rax              # imm = 0x100
	movl	%eax, 16(%r15)
	movq	global_trees+216(%rip), %rax
	movq	%rax, 8(%r15)
	jmp	.LBB9_40
.LBB9_38:                               # %sw.bb55
	orq	$512, %rax              # imm = 0x200
	jmp	.LBB9_39
.LBB9_22:                               # %tree_size.exit
	cmpq	$101, %r13
	movq	8(%rsp), %rsi           # 8-byte Reload
	jne	.LBB9_40
# BB#23:                                # %sw.bb60
	leaq	-129(%rsi), %rcx
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	jb	.LBB9_26
# BB#24:                                # %sw.bb60
	leaq	-48(%rsi), %rcx
	andq	%rdx, %rcx
	cmpq	$2, %rcx
	jb	.LBB9_26
# BB#25:                                # %sw.bb60
	andq	%rdx, %rsi
	cmpq	$120, %rsi
	jne	.LBB9_40
.LBB9_26:                               # %sw.bb61
	orq	$256, %rax              # imm = 0x100
.LBB9_39:                               # %sw.epilog67
	movl	%eax, 16(%r15)
.LBB9_40:                               # %sw.epilog67
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_11:                               # %if.end
	andq	%rsi, %rax
	cmpq	$120, %rax
	je	.LBB9_17
# BB#12:                                # %if.end
	cmpq	$116, %rax
	jne	.LBB9_3
# BB#13:                                # %sw.bb2.i
	movq	$168, %rbx
	jmp	.LBB9_19
.LBB9_14:                               # %sw.bb8.i
	movq	$48, %rbx
	cmpq	$25, %r14
	je	.LBB9_19
# BB#15:                                # %sw.bb8.i
	cmpq	$26, %r14
	jne	.LBB9_17
# BB#16:                                # %if.then12.i
	movq	$56, %rbx
	jmp	.LBB9_19
.LBB9_17:                               # %sw.bb19.i
	movslq	tree_code_length(,%r14,4), %rax
	leaq	24(,%rax,8), %rbx
	jmp	.LBB9_19
.LBB9_3:                                # %sw.default.i
	movabsq	$.L.str147, %rdi
	movabsq	$.L__FUNCTION__.tree_size, %rdx
	movq	$316, %rsi              # imm = 0x13C
	callq	fancy_abort
.LBB9_41:                               # %if.then
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.make_node, %rcx
	movq	$346, %rsi              # imm = 0x15A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp70:
	.size	make_node, .Ltmp70-make_node
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_18
	.quad	.LBB9_14
	.quad	.LBB9_19
	.quad	.LBB9_6

	.text
	.globl	make_lang_type
	.align	16, 0x90
	.type	make_lang_type,@function
make_lang_type:                         # @make_lang_type
	.cfi_startproc
# BB#0:                                 # %entry
	jmpq	*make_lang_type_fn(%rip)  # TAILCALL
.Ltmp71:
	.size	make_lang_type, .Ltmp71-make_lang_type
	.cfi_endproc

	.globl	copy_node
	.align	16, 0x90
	.type	copy_node,@function
copy_node:                              # @copy_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp77:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp78:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp79:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp80:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp81:
	.cfi_def_cfa_offset 48
.Ltmp82:
	.cfi_offset %rbx, -40
.Ltmp83:
	.cfi_offset %r12, -32
.Ltmp84:
	.cfi_offset %r14, -24
.Ltmp85:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	16(%r14), %r12d
	movsbq	tree_code_type(%r12), %rax
	cmpq	$59, %rax
	jg	.LBB11_3
# BB#1:                                 # %entry
	addq	$-49, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB11_5
	jmp	.LBB11_2
.LBB11_3:                               # %entry
	cmpq	$97, %rax
	jg	.LBB11_6
# BB#4:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$60, %rax
	je	.LBB11_5
	jmp	.LBB11_2
.LBB11_6:                               # %entry
	cmpq	$113, %rax
	jg	.LBB11_9
# BB#7:                                 # %entry
	addq	$-98, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$3, %rcx
	ja	.LBB11_2
# BB#8:                                 # %entry
	movq	$208, %r15
	jmpq	*.LJTI11_0(,%rcx,8)
.LBB11_19:                              # %sw.bb3.i
	movq	$80, %r15
	jmp	.LBB11_20
.LBB11_9:                               # %entry
	leaq	-114(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jae	.LBB11_10
.LBB11_5:                               # %sw.bb4.i
	movl	tree_code_length(,%r12,4), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	40(,%rax,8), %r15
.LBB11_20:                              # %tree_size.exit
	movq	%r15, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movq	$0, (%rbx)
	movl	16(%rbx), %eax
	movabsq	$4294950911, %rcx       # imm = 0xFFFFBFFF
	andq	%rax, %rcx
	movl	%ecx, 16(%rbx)
	movzbl	tree_code_type(%r12), %eax
	cmpq	$116, %rax
	jne	.LBB11_21
# BB#23:                                # %if.then13
	movl	next_type_uid(%rip), %eax
	leaq	1(%rax), %rcx
	movl	%ecx, next_type_uid(%rip)
	movl	%eax, 56(%rbx)
	movq	$0, 88(%rbx)
	xorq	%rax, %rax
	movl	%eax, 88(%rbx)
	jmp	.LBB11_24
.LBB11_21:                              # %tree_size.exit
	cmpq	$100, %rax
	jne	.LBB11_24
# BB#22:                                # %if.then
	movl	next_decl_uid(%rip), %eax
	leaq	1(%rax), %rcx
	movl	%ecx, next_decl_uid(%rip)
	movl	%eax, 36(%rbx)
.LBB11_24:                              # %if.end19
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB11_10:                              # %entry
	andq	%rcx, %rax
	cmpq	$120, %rax
	jne	.LBB11_11
# BB#17:                                # %sw.bb19.i
	movslq	tree_code_length(,%r12,4), %rax
	leaq	24(,%rax,8), %r15
	cmpq	$3, %r12
	jne	.LBB11_20
# BB#18:                                # %if.then27.i
	movl	24(%r14), %eax
	decq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r15,%rax,8), %r15
	jmp	.LBB11_20
.LBB11_11:                              # %entry
	cmpq	$116, %rax
	jne	.LBB11_2
# BB#12:                                # %sw.bb2.i
	movq	$168, %r15
	jmp	.LBB11_20
.LBB11_13:                              # %sw.bb8.i
	movq	$48, %r15
	cmpq	$25, %r12
	je	.LBB11_20
# BB#14:                                # %sw.bb8.i
	cmpq	$26, %r12
	jne	.LBB11_16
# BB#15:                                # %if.then12.i
	movq	$56, %r15
	jmp	.LBB11_20
.LBB11_2:                               # %sw.default.i
	movabsq	$.L.str147, %rdi
	movabsq	$.L__FUNCTION__.tree_size, %rdx
	movq	$316, %rsi              # imm = 0x13C
	callq	fancy_abort
.LBB11_16:                              # %if.else13.i
	movslq	tree_code_length(,%r12,4), %rax
	leaq	24(,%rax,8), %r15
	jmp	.LBB11_20
.Ltmp86:
	.size	copy_node, .Ltmp86-copy_node
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI11_0:
	.quad	.LBB11_19
	.quad	.LBB11_13
	.quad	.LBB11_20
	.quad	.LBB11_5

	.text
	.globl	copy_list
	.align	16, 0x90
	.type	copy_list,@function
copy_list:                              # @copy_list
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
	movq	%rdi, %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB12_4
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	copy_node
	movq	(%rbx), %rbx
	movq	%rax, %r14
	testq	%rbx, %rbx
	je	.LBB12_4
# BB#2:
	movq	%r14, %r15
	.align	16, 0x90
.LBB12_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	copy_node
	movq	%rax, (%r15)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rax, %r15
	jne	.LBB12_3
.LBB12_4:                               # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp97:
	.size	copy_list, .Ltmp97-copy_list
	.cfi_endproc

	.globl	build_int_2_wide
	.align	16, 0x90
	.type	build_int_2_wide,@function
build_int_2_wide:                       # @build_int_2_wide
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp103:
	.cfi_def_cfa_offset 32
.Ltmp104:
	.cfi_offset %rbx, -24
.Ltmp105:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$25, %rdi
	callq	make_node
	movq	%rbx, 32(%rax)
	movq	%r14, 40(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp106:
	.size	build_int_2_wide, .Ltmp106-build_int_2_wide
	.cfi_endproc

	.globl	build_vector
	.align	16, 0x90
	.type	build_vector,@function
build_vector:                           # @build_vector
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp111:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp112:
	.cfi_def_cfa_offset 32
.Ltmp113:
	.cfi_offset %rbx, -24
.Ltmp114:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	$28, %rdi
	callq	make_node
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	movq	%rbx, 32(%rax)
	movq	%r14, 8(%rax)
	je	.LBB14_1
# BB#2:
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB14_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rcx
	movq	%rsi, %rdi
	movq	%rdx, %r8
	movq	(%rbx), %rbx
	movl	16(%rcx), %edx
	movq	$18, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$19, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	andq	$1, %rsi
	orq	%rdi, %rsi
	orq	%r8, %rdx
	testq	%rbx, %rbx
	jne	.LBB14_3
# BB#4:                                 # %for.cond.for.end_crit_edge
	andq	$1, %rdx
	shlq	$19, %rdx
	andq	$1, %rsi
	shlq	$18, %rsi
	jmp	.LBB14_5
.LBB14_1:
	xorq	%rsi, %rsi
.LBB14_5:                               # %for.end
	movl	16(%rax), %ecx
	orq	%rsi, %rdx
	movabsq	$4294180863, %rsi       # imm = 0xFFF3FFFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 16(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp115:
	.size	build_vector, .Ltmp115-build_vector
	.cfi_endproc

	.globl	build_real
	.align	16, 0x90
	.type	build_real,@function
build_real:                             # @build_real
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 16
.Ltmp119:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	$26, %rdi
	callq	make_node
	movq	%rbx, 8(%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 48(%rax)
	movq	16(%rsp), %rcx
	movq	24(%rsp), %rdx
	movq	%rdx, 40(%rax)
	movq	%rcx, 32(%rax)
	movl	16(%rax), %ecx
	movabsq	$4294180863, %rdx       # imm = 0xFFF3FFFF
	andq	%rcx, %rdx
	movl	%edx, 16(%rax)
	popq	%rbx
	retq
.Ltmp120:
	.size	build_real, .Ltmp120-build_real
	.cfi_endproc

	.globl	real_value_from_int_cst
	.align	16, 0x90
	.type	real_value_from_int_cst,@function
real_value_from_int_cst:                # @real_value_from_int_cst
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp123:
	.cfi_def_cfa_offset 16
	subq	$32, %rsp
.Ltmp124:
	.cfi_def_cfa_offset 48
.Ltmp125:
	.cfi_offset %rbx, -16
	movq	%rsi, %rax
	movq	%rdi, %rbx
	movq	$0, 24(%rsp)
	movq	$0, 16(%rsp)
	movq	$0, 8(%rsp)
	movq	8(%rdx), %rcx
	movq	32(%rdx), %rsi
	movl	60(%rax), %eax
	movq	40(%rdx), %rdx
	movzbl	17(%rcx), %edi
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	testq	$32, %rdi
	je	.LBB16_1
# BB#2:                                 # %if.else
	leaq	8(%rsp), %rdi
	movq	%rax, %rcx
	callq	ereal_from_uint
	jmp	.LBB16_3
.LBB16_1:                               # %if.then
	leaq	8(%rsp), %rdi
	movq	%rax, %rcx
	callq	ereal_from_int
.LBB16_3:                               # %if.end
	movq	24(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	16(%rsp), %rdx
	movq	%rax, 16(%rbx)
	movq	%rdx, 8(%rbx)
	movq	%rcx, (%rbx)
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	retq
.Ltmp126:
	.size	real_value_from_int_cst, .Ltmp126-real_value_from_int_cst
	.cfi_endproc

	.globl	build_real_from_int_cst
	.align	16, 0x90
	.type	build_real_from_int_cst,@function
build_real_from_int_cst:                # @build_real_from_int_cst
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 40
	subq	$72, %rsp
.Ltmp136:
	.cfi_def_cfa_offset 112
.Ltmp137:
	.cfi_offset %rbx, -40
.Ltmp138:
	.cfi_offset %r12, -32
.Ltmp139:
	.cfi_offset %r14, -24
.Ltmp140:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movl	16(%r14), %ebx
	movq	%rdi, %r12
	movq	$26, %rdi
	callq	make_node
	movq	%rax, %r15
	movq	%r12, 8(%r15)
	movq	%r12, 8(%rsp)
	movq	%r14, 16(%rsp)
	movabsq	$build_real_from_int_cst_1, %rdi
	leaq	8(%rsp), %rsi
	callq	do_float_handler
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_2
# BB#1:                                 # %if.then
	movq	$19, %rcx
	shrq	%cl, %rbx
	movq	40(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	32(%rsp), %rdx
	movq	%rax, 64(%rsp)
	andq	$1, %rbx
	movq	%rdx, 56(%rsp)
	movq	%rcx, 48(%rsp)
	jmp	.LBB17_3
.LBB17_2:                               # %if.else
	movq	dconst0+16(%rip), %rax
	movq	dconst0+8(%rip), %rcx
	movq	dconst0(%rip), %rdx
	movq	%rax, 64(%rsp)
	movq	%rcx, 56(%rsp)
	movq	%rdx, 48(%rsp)
	movq	$1, %rbx
.LBB17_3:                               # %if.end
	movq	64(%rsp), %rax
	movq	%rax, 48(%r15)
	movq	48(%rsp), %rax
	movq	56(%rsp), %rcx
	movq	%rcx, 40(%r15)
	movq	%rax, 32(%r15)
	movl	16(%r15), %eax
	movq	$18, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movabsq	$4294180863, %rsi       # imm = 0xFFF3FFFF
	andq	%rax, %rsi
	movq	$19, %rcx
	shlq	%cl, %rbx
	orq	%rdx, %rbx
	orq	%rsi, %rbx
	movl	%ebx, 16(%r15)
	movq	%r15, %rax
	addq	$72, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp141:
	.size	build_real_from_int_cst, .Ltmp141-build_real_from_int_cst
	.cfi_endproc

	.align	16, 0x90
	.type	build_real_from_int_cst_1,@function
build_real_from_int_cst_1:              # @build_real_from_int_cst_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp144:
	.cfi_def_cfa_offset 16
	subq	$48, %rsp
.Ltmp145:
	.cfi_def_cfa_offset 64
.Ltmp146:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rax
	movq	8(%rbx), %rcx
	movq	$0, 40(%rsp)
	movq	$0, 32(%rsp)
	movq	$0, 24(%rsp)
	movq	8(%rcx), %rdi
	movq	32(%rcx), %rsi
	movl	60(%rax), %eax
	movq	40(%rcx), %rdx
	movzbl	17(%rdi), %edi
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	testq	$32, %rdi
	je	.LBB18_1
# BB#2:                                 # %if.else.i
	leaq	24(%rsp), %rdi
	movq	%rax, %rcx
	callq	ereal_from_uint
	jmp	.LBB18_3
.LBB18_1:                               # %if.then.i
	leaq	24(%rsp), %rdi
	movq	%rax, %rcx
	callq	ereal_from_int
.LBB18_3:                               # %real_value_from_int_cst.exit
	movq	40(%rsp), %rax
	movq	24(%rsp), %rcx
	movq	32(%rsp), %rdx
	movq	%rax, 16(%rsp)
	movq	%rdx, 8(%rsp)
	movq	%rcx, (%rsp)
	movq	16(%rsp), %rax
	movq	%rax, 32(%rbx)
	movq	(%rsp), %rax
	movq	8(%rsp), %rcx
	movq	%rcx, 24(%rbx)
	movq	%rax, 16(%rbx)
	addq	$48, %rsp
	popq	%rbx
	retq
.Ltmp147:
	.size	build_real_from_int_cst_1, .Ltmp147-build_real_from_int_cst_1
	.cfi_endproc

	.globl	build_string
	.align	16, 0x90
	.type	build_string,@function
build_string:                           # @build_string
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
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$29, %rdi
	callq	make_node
	movq	%rax, %rbx
	movl	%r15d, 32(%rbx)
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	ggc_alloc_string
	movq	%rax, 40(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp158:
	.size	build_string, .Ltmp158-build_string
	.cfi_endproc

	.globl	build_complex
	.align	16, 0x90
	.type	build_complex,@function
build_complex:                          # @build_complex
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp164:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp165:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp166:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp167:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp168:
	.cfi_def_cfa_offset 48
.Ltmp169:
	.cfi_offset %rbx, -40
.Ltmp170:
	.cfi_offset %r12, -32
.Ltmp171:
	.cfi_offset %r14, -24
.Ltmp172:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$27, %rdi
	callq	make_node
	movq	%rax, %rbx
	testq	%r12, %r12
	movq	%r15, 32(%rbx)
	movq	%r14, 40(%rbx)
	jne	.LBB20_2
# BB#1:                                 # %cond.false
	movq	8(%r15), %rdi
	callq	build_complex_type
	movq	%rax, %r12
.LBB20_2:                               # %cond.end
	movq	%r12, 8(%rbx)
	movl	16(%r15), %eax
	movl	16(%r14), %ecx
	movl	16(%rbx), %edx
	orq	%rax, %rcx
	andq	$524288, %rcx           # imm = 0x80000
	andq	$-524289, %rdx          # imm = 0xFFFFFFFFFFF7FFFF
	orq	%rcx, %rdx
	movl	%edx, 16(%rbx)
	movl	16(%r15), %eax
	movl	16(%r14), %ecx
	orq	%rax, %rcx
	andq	$262144, %rcx           # imm = 0x40000
	movabsq	$4294705151, %rax       # imm = 0xFFFBFFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	movl	%eax, 16(%rbx)
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp173:
	.size	build_complex, .Ltmp173-build_complex
	.cfi_endproc

	.globl	build_complex_type
	.align	16, 0x90
	.type	build_complex_type,@function
build_complex_type:                     # @build_complex_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp180:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp181:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp182:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp183:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp184:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp185:
	.cfi_def_cfa_offset 64
.Ltmp186:
	.cfi_offset %rbx, -48
.Ltmp187:
	.cfi_offset %r12, -40
.Ltmp188:
	.cfi_offset %r13, -32
.Ltmp189:
	.cfi_offset %r14, -24
.Ltmp190:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$8, %rdi
	callq	make_node
	movq	128(%r14), %rcx
	movq	%rax, %rbx
	movq	%rcx, 8(%rbx)
	movl	16(%r14), %eax
	movl	16(%rbx), %ecx
	movl	60(%r14), %edx
	movabsq	$4294961151, %rsi       # imm = 0xFFFFE7FF
	andq	%rcx, %rsi
	andq	$6144, %rax             # imm = 0x1800
	orq	%rsi, %rax
	movl	%eax, 16(%rbx)
	movl	60(%rbx), %eax
	andq	$2097152, %rdx          # imm = 0x200000
	movabsq	$4292870143, %rcx       # imm = 0xFFDFFFFF
	andq	%rax, %rcx
	orq	%rdx, %rcx
	movl	%ecx, 60(%rbx)
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB21_4
# BB#1:                                 # %if.end.i
	movq	%r14, %r12
	andq	$262143, %r12           # imm = 0x3FFFF
	movq	%rbx, %rdi
	callq	layout_type
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r12, %r13
	movq	%r13, (%rsp)
	movq	%rbx, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	%r12, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB21_3
# BB#2:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %r15
	testq	%r15, %r15
	jne	.LBB21_5
.LBB21_3:                               # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	movq	%r13, (%r15)
	movq	%rbx, 8(%r15)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	htab_find_slot_with_hash
	movq	%r15, (%rax)
.LBB21_4:                               # %type_hash_canon.exit
	movq	%rbx, %r15
.LBB21_5:                               # %type_hash_canon.exit
	cmpq	$0, 32(%r15)
	jne	.LBB21_7
# BB#6:                                 # %if.then
	movq	%r15, %rdi
	callq	layout_type
.LBB21_7:                               # %if.end
	movl	write_symbols(%rip), %eax
	cmpq	$4, %rax
	je	.LBB21_9
# BB#8:                                 # %if.end
	cmpq	$7, %rax
	jne	.LBB21_33
.LBB21_9:                               # %land.lhs.true
	cmpq	$0, 96(%r15)
	jne	.LBB21_33
# BB#10:                                # %if.then31
	movq	integer_types(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_11
# BB#12:                                # %if.else
	movq	integer_types+8(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_13
# BB#14:                                # %if.else39
	movq	integer_types+16(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_15
# BB#16:                                # %if.else43
	movq	integer_types+24(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_17
# BB#18:                                # %if.else47
	movq	integer_types+32(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_19
# BB#20:                                # %if.else51
	movq	integer_types+40(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_21
# BB#22:                                # %if.else55
	movq	integer_types+48(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_23
# BB#24:                                # %if.else59
	movq	integer_types+56(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_25
# BB#26:                                # %if.else63
	movq	integer_types+64(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_27
# BB#28:                                # %if.else67
	movq	integer_types+72(%rip), %rax
	cmpq	%r14, %rax
	je	.LBB21_29
# BB#30:                                # %if.else71
	movq	integer_types+80(%rip), %rax
	cmpq	%r14, %rax
	jne	.LBB21_33
# BB#31:
	movabsq	$.L.str161, %rdi
	jmp	.LBB21_32
.LBB21_11:
	movabsq	$.L.str151, %rdi
	jmp	.LBB21_32
.LBB21_13:
	movabsq	$.L.str152, %rdi
	jmp	.LBB21_32
.LBB21_15:
	movabsq	$.L.str153, %rdi
	jmp	.LBB21_32
.LBB21_17:
	movabsq	$.L.str154, %rdi
	jmp	.LBB21_32
.LBB21_19:
	movabsq	$.L.str155, %rdi
	jmp	.LBB21_32
.LBB21_21:
	movabsq	$.L.str156, %rdi
	jmp	.LBB21_32
.LBB21_23:
	movabsq	$.L.str157, %rdi
	jmp	.LBB21_32
.LBB21_25:
	movabsq	$.L.str158, %rdi
	jmp	.LBB21_32
.LBB21_27:
	movabsq	$.L.str159, %rdi
	jmp	.LBB21_32
.LBB21_29:
	movabsq	$.L.str160, %rdi
.LBB21_32:                              # %if.then89
	callq	get_identifier
	movq	%rax, 96(%r15)
.LBB21_33:                              # %if.end94
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp191:
	.size	build_complex_type, .Ltmp191-build_complex_type
	.cfi_endproc

	.globl	make_tree_vec
	.align	16, 0x90
	.type	make_tree_vec,@function
make_tree_vec:                          # @make_tree_vec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp197:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp198:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp199:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp200:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp201:
	.cfi_def_cfa_offset 48
.Ltmp202:
	.cfi_offset %rbx, -40
.Ltmp203:
	.cfi_offset %r12, -32
.Ltmp204:
	.cfi_offset %r14, -24
.Ltmp205:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$3, %r12
	movq	$3, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$137438953472, %rbx     # imm = 0x2000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	memset
	movb	%r12b, 16(%r15)
	movl	%r14d, 24(%r15)
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp206:
	.size	make_tree_vec, .Ltmp206-make_tree_vec
	.cfi_endproc

	.globl	integer_zerop
	.align	16, 0x90
	.type	integer_zerop,@function
integer_zerop:                          # @integer_zerop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp211:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp212:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp213:
	.cfi_def_cfa_offset 32
.Ltmp214:
	.cfi_offset %rbx, -32
.Ltmp215:
	.cfi_offset %r14, -24
.Ltmp216:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %r8
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB23_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	leaq	-114(%rax), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	ja	.LBB23_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r8, %rdi
	je	.LBB23_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB23_1 Depth=1
	movq	8(%rbx), %rsi
	movq	8(%rdi), %rcx
	movl	60(%rsi), %esi
	movl	60(%rcx), %ecx
	xorq	%rsi, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB23_1
.LBB23_4:                               # %while.end
	andq	$262399, %rdx           # imm = 0x400FF
	cmpq	$25, %rdx
	jne	.LBB23_7
# BB#5:                                 # %land.lhs.true41
	cmpq	$0, 32(%rbx)
	jne	.LBB23_7
# BB#6:                                 # %land.lhs.true44
	cmpq	$0, 40(%rbx)
	movq	$1, %r14
	je	.LBB23_12
.LBB23_7:                               # %lor.rhs
	xorq	%r14, %r14
	cmpq	$27, %rax
	jne	.LBB23_12
# BB#8:                                 # %land.lhs.true53
	movq	32(%rbx), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB23_12
# BB#9:                                 # %land.rhs55
	movq	40(%rbx), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB23_10
# BB#11:                                # %land.rhs55
	movabsq	$1, %r14
	jmp	.LBB23_12
.LBB23_10:
	movabsq	$0, %r14
.LBB23_12:                              # %lor.end
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp217:
	.size	integer_zerop, .Ltmp217-integer_zerop
	.cfi_endproc

	.globl	integer_onep
	.align	16, 0x90
	.type	integer_onep,@function
integer_onep:                           # @integer_onep
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp222:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp223:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp224:
	.cfi_def_cfa_offset 32
.Ltmp225:
	.cfi_offset %rbx, -32
.Ltmp226:
	.cfi_offset %r14, -24
.Ltmp227:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %r8
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB24_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	leaq	-114(%rax), %rsi
	andq	%r15, %rsi
	cmpq	$2, %rsi
	ja	.LBB24_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%r8, %rdi
	je	.LBB24_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB24_1 Depth=1
	movq	8(%rbx), %rsi
	movq	8(%rdi), %rcx
	movl	60(%rsi), %esi
	movl	60(%rcx), %ecx
	xorq	%rsi, %rcx
	testq	$65024, %rcx            # imm = 0xFE00
	je	.LBB24_1
.LBB24_4:                               # %while.end
	andq	$262399, %rdx           # imm = 0x400FF
	cmpq	$25, %rdx
	jne	.LBB24_7
# BB#5:                                 # %land.lhs.true41
	movq	32(%rbx), %rcx
	cmpq	$1, %rcx
	jne	.LBB24_7
# BB#6:                                 # %land.lhs.true44
	cmpq	$0, 40(%rbx)
	movq	$1, %r14
	je	.LBB24_12
.LBB24_7:                               # %lor.rhs
	xorq	%r14, %r14
	cmpq	$27, %rax
	jne	.LBB24_12
# BB#8:                                 # %land.lhs.true53
	movq	32(%rbx), %rdi
	callq	integer_onep
	testq	%r15, %rax
	je	.LBB24_12
# BB#9:                                 # %land.rhs55
	movq	40(%rbx), %rdi
	callq	integer_zerop
	testq	%r15, %rax
	je	.LBB24_10
# BB#11:                                # %land.rhs55
	movabsq	$1, %r14
	jmp	.LBB24_12
.LBB24_10:
	movabsq	$0, %r14
.LBB24_12:                              # %lor.end
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp228:
	.size	integer_onep, .Ltmp228-integer_onep
	.cfi_endproc

	.globl	integer_all_onesp
	.align	16, 0x90
	.type	integer_all_onesp,@function
integer_all_onesp:                      # @integer_all_onesp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp232:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp233:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp234:
	.cfi_def_cfa_offset 32
.Ltmp235:
	.cfi_offset %rbx, -24
.Ltmp236:
	.cfi_offset %r14, -16
	movq	global_trees(%rip), %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB25_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movzbl	16(%rbx), %ecx
	leaq	-114(%rcx), %rdx
	andq	%r14, %rdx
	cmpq	$2, %rdx
	ja	.LBB25_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rax, %rdi
	je	.LBB25_7
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB25_1 Depth=1
	movq	8(%rbx), %rcx
	movq	8(%rdi), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB25_1
	jmp	.LBB25_7
.LBB25_4:                               # %while.end
	cmpq	$27, %rcx
	jne	.LBB25_7
# BB#5:                                 # %land.lhs.true36
	movq	32(%rbx), %rdi
	callq	integer_all_onesp
	testq	%r14, %rax
	je	.LBB25_7
# BB#6:                                 # %land.lhs.true37
	movq	40(%rbx), %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB25_22
.LBB25_7:                               # %if.else
	movl	16(%rbx), %ecx
	andq	$262399, %rcx           # imm = 0x400FF
	xorq	%rax, %rax
	cmpq	$25, %rcx
	jne	.LBB25_22
# BB#8:                                 # %if.end53
	movq	8(%rbx), %rax
	movzbl	17(%rax), %ecx
	testq	$32, %rcx
	je	.LBB25_9
# BB#12:                                # %if.end69
	movl	60(%rax), %eax
	movq	$8, %rcx
	shrq	%cl, %rax
	andq	$254, %rax
	movzbl	mode_bitsize(%rax), %ecx
	cmpq	$64, %rcx
	jb	.LBB25_19
# BB#13:                                # %if.then80
	addq	$-64, %rcx
	cmpq	$65, %rcx
	jge	.LBB25_23
# BB#14:                                # %if.else84
	andq	%rcx, %r14
	movq	$-1, %rdx
	cmpq	$64, %r14
	je	.LBB25_16
# BB#15:                                # %if.else88
	movq	$1, %rdx
	shlq	%cl, %rdx
	decq	%rdx
.LBB25_16:                              # %if.end91
	movq	32(%rbx), %rcx
	xorq	%rax, %rax
	cmpq	$-1, %rcx
	jne	.LBB25_22
# BB#17:                                # %land.rhs97
	movq	40(%rbx), %rax
	cmpq	%rdx, %rax
	je	.LBB25_21
# BB#18:
	movabsq	$0, %rax
	jmp	.LBB25_22
.LBB25_9:                               # %if.then61
	movq	32(%rbx), %rcx
	xorq	%rax, %rax
	cmpq	$-1, %rcx
	jne	.LBB25_22
# BB#10:                                # %land.rhs64
	movq	40(%rbx), %rax
	cmpq	$-1, %rax
	je	.LBB25_21
# BB#11:
	movabsq	$0, %rax
	jmp	.LBB25_22
.LBB25_19:                              # %if.else105
	movq	32(%rbx), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	decq	%rdx
	cmpq	%rdx, %rax
	jne	.LBB25_20
.LBB25_21:                              # %if.else105
	movabsq	$1, %rax
	jmp	.LBB25_22
.LBB25_20:
	movabsq	$0, %rax
.LBB25_22:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB25_23:                              # %if.then83
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.integer_all_onesp, %rcx
	movq	$877, %rsi              # imm = 0x36D
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp237:
	.size	integer_all_onesp, .Ltmp237-integer_all_onesp
	.cfi_endproc

	.globl	integer_pow2p
	.align	16, 0x90
	.type	integer_pow2p,@function
integer_pow2p:                          # @integer_pow2p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp242:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 32
.Ltmp245:
	.cfi_offset %rbx, -32
.Ltmp246:
	.cfi_offset %r14, -24
.Ltmp247:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB26_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %r15d
	movq	%r15, %rcx
	andq	$255, %rcx
	leaq	-114(%rcx), %rdx
	andq	%r14, %rdx
	cmpq	$2, %rdx
	ja	.LBB26_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB26_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rax, %rdi
	je	.LBB26_7
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB26_1 Depth=1
	movq	8(%rbx), %rcx
	movq	8(%rdi), %rdx
	movl	60(%rcx), %ecx
	movl	60(%rdx), %edx
	xorq	%rcx, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB26_1
	jmp	.LBB26_7
.LBB26_4:                               # %while.end
	cmpq	$27, %rcx
	jne	.LBB26_7
# BB#5:                                 # %land.lhs.true36
	movq	32(%rbx), %rdi
	callq	integer_pow2p
	testq	%r14, %rax
	je	.LBB26_7
# BB#6:                                 # %land.lhs.true37
	movq	40(%rbx), %rdi
	callq	integer_zerop
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB26_28
.LBB26_7:                               # %if.end
	andq	$262399, %r15           # imm = 0x400FF
	xorq	%rax, %rax
	cmpq	$25, %r15
	jne	.LBB26_28
# BB#8:                                 # %if.end53
	movq	8(%rbx), %rcx
	movl	16(%rcx), %edx
	orq	$2, %rdx
	andq	$255, %rdx
	cmpq	$15, %rdx
	jne	.LBB26_10
# BB#9:                                 # %cond.true
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	jmp	.LBB26_11
.LBB26_10:                              # %cond.false
	movl	60(%rcx), %edx
	andq	$511, %rdx              # imm = 0x1FF
.LBB26_11:                              # %cond.end
	movq	32(%rbx), %rdi
	movq	40(%rbx), %rsi
	andq	%rdx, %r14
	cmpq	$128, %r14
	je	.LBB26_16
# BB#12:                                # %if.else
	cmpq	$65, %r14
	jb	.LBB26_14
# BB#13:                                # %if.then84
	addq	$-64, %rdx
	movq	$-1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	notq	%rbx
	andq	%rbx, %rsi
	jmp	.LBB26_16
.LBB26_14:                              # %if.else86
	xorq	%rsi, %rsi
	cmpq	$63, %r14
	ja	.LBB26_16
# BB#15:                                # %if.then88
	movq	$-1, %rbx
	movq	%rdx, %rcx
	shlq	%cl, %rbx
	notq	%rbx
	andq	%rbx, %rdi
.LBB26_16:                              # %if.end95
	movq	%rdi, %rcx
	orq	%rsi, %rcx
	je	.LBB26_28
# BB#17:                                # %if.end100
	testq	%rsi, %rsi
	je	.LBB26_18
# BB#24:                                # %lor.rhs
	xorq	%rax, %rax
	testq	%rdi, %rdi
	jne	.LBB26_28
	jmp	.LBB26_25
.LBB26_18:                              # %land.lhs.true102
	leaq	-1(%rdi), %rcx
	andq	%rdi, %rcx
	movabsq	$0, %rdx
	movabsq	$1, %rbx
	movq	%rdx, %rax
	jne	.LBB26_20
# BB#19:                                # %land.lhs.true102
	movq	%rbx, %rax
.LBB26_20:                              # %land.lhs.true102
	testq	%rdi, %rdi
	je	.LBB26_22
# BB#21:                                # %land.lhs.true102
	movq	%rbx, %rdx
.LBB26_22:                              # %land.lhs.true102
	testq	%rcx, %rcx
	je	.LBB26_28
# BB#23:                                # %land.lhs.true102
	testq	%rdx, %rdx
	jne	.LBB26_28
.LBB26_25:                              # %land.rhs107
	leaq	-1(%rsi), %rax
	testq	%rsi, %rax
	jne	.LBB26_26
# BB#27:                                # %land.rhs107
	movabsq	$1, %rax
	jmp	.LBB26_28
.LBB26_26:
	movabsq	$0, %rax
.LBB26_28:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp248:
	.size	integer_pow2p, .Ltmp248-integer_pow2p
	.cfi_endproc

	.globl	tree_log2
	.align	16, 0x90
	.type	tree_log2,@function
tree_log2:                              # @tree_log2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp250:
	.cfi_def_cfa_offset 16
	movq	global_trees(%rip), %r8
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	jmp	.LBB27_1
	.align	16, 0x90
.LBB27_7:                               # %if.then
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	32(%rdx), %rdi
.LBB27_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rdx
	movzbl	16(%rdx), %ecx
	leaq	-114(%rcx), %rdi
	andq	%rsi, %rdi
	cmpq	$2, %rdi
	ja	.LBB27_6
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	32(%rdx), %rdi
	cmpq	%r8, %rdi
	je	.LBB27_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB27_1 Depth=1
	movq	8(%rdx), %rcx
	movq	8(%rdi), %rax
	movl	60(%rcx), %ecx
	movl	60(%rax), %eax
	xorq	%rcx, %rax
	testq	$65024, %rax            # imm = 0xFE00
	je	.LBB27_1
	jmp	.LBB27_4
	.align	16, 0x90
.LBB27_6:                               # %while.end
                                        #   in Loop: Header=BB27_1 Depth=1
	cmpq	$27, %rcx
	je	.LBB27_7
.LBB27_4:                               # %if.end
	movq	8(%rdx), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB27_8
# BB#5:                                 # %cond.true
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	jmp	.LBB27_9
.LBB27_8:                               # %cond.false
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
.LBB27_9:                               # %cond.end
	movq	32(%rdx), %rdi
	movq	40(%rdx), %rdx
	andq	%rax, %rsi
	cmpq	$128, %rsi
	je	.LBB27_12
# BB#10:                                # %if.else
	cmpq	$65, %rsi
	jb	.LBB27_14
# BB#11:                                # %if.then65
	addq	$-64, %rax
	movq	$-1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rsi, %rdx
.LBB27_12:                              # %if.end76
	testq	%rdx, %rdx
	je	.LBB27_16
# BB#13:                                # %return
	movq	%rdx, %rdi
	callq	exact_log2_wide
	addq	$64, %rax
	popq	%rdx
	retq
.LBB27_14:                              # %if.else67
	cmpq	$63, %rsi
	ja	.LBB27_16
# BB#15:                                # %if.then69
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rdi
.LBB27_16:                              # %cond.false80
	popq	%rax
	jmp	exact_log2_wide  # TAILCALL
.Ltmp251:
	.size	tree_log2, .Ltmp251-tree_log2
	.cfi_endproc

	.globl	tree_floor_log2
	.align	16, 0x90
	.type	tree_floor_log2,@function
tree_floor_log2:                        # @tree_floor_log2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp253:
	.cfi_def_cfa_offset 16
	movq	global_trees(%rip), %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB28_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rdx
	movzbl	16(%rdx), %ecx
	leaq	-114(%rcx), %rdi
	andq	%r8, %rdi
	cmpq	$2, %rdi
	ja	.LBB28_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	32(%rdx), %rdi
	cmpq	%rax, %rdi
	je	.LBB28_5
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB28_1 Depth=1
	movq	8(%rdx), %rcx
	movq	8(%rdi), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %esi
	xorq	%rcx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB28_1
	jmp	.LBB28_5
.LBB28_4:                               # %while.end
	cmpq	$27, %rcx
	jne	.LBB28_5
# BB#16:                                # %if.then
	movq	32(%rdx), %rdi
	popq	%rax
	jmp	tree_log2  # TAILCALL
.LBB28_5:                               # %if.end
	movq	8(%rdx), %rax
	movl	16(%rax), %ecx
	orq	$2, %rcx
	andq	$255, %rcx
	cmpq	$15, %rcx
	jne	.LBB28_7
# BB#6:                                 # %cond.true
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	jmp	.LBB28_8
.LBB28_7:                               # %cond.false
	movl	60(%rax), %eax
	andq	$511, %rax              # imm = 0x1FF
.LBB28_8:                               # %cond.end
	movq	32(%rdx), %rdi
	movq	40(%rdx), %rdx
	testq	$383, %rax              # imm = 0x17F
	je	.LBB28_11
# BB#9:                                 # %if.else
	andq	%rax, %r8
	cmpq	$65, %r8
	jb	.LBB28_13
# BB#10:                                # %if.then67
	addq	$-64, %rax
	movq	$-1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	notq	%rsi
	andq	%rsi, %rdx
.LBB28_11:                              # %if.end78
	testq	%rdx, %rdx
	je	.LBB28_15
# BB#12:                                # %return
	movq	%rdx, %rdi
	callq	floor_log2_wide
	addq	$64, %rax
	popq	%rdx
	retq
.LBB28_13:                              # %if.else69
	cmpq	$63, %r8
	ja	.LBB28_15
# BB#14:                                # %if.then71
	movq	$-1, %rdx
	movq	%rax, %rcx
	shlq	%cl, %rdx
	notq	%rdx
	andq	%rdx, %rdi
.LBB28_15:                              # %cond.false82
	popq	%rax
	jmp	floor_log2_wide  # TAILCALL
.Ltmp254:
	.size	tree_floor_log2, .Ltmp254-tree_floor_log2
	.cfi_endproc

	.globl	real_zerop
	.align	16, 0x90
	.type	real_zerop,@function
real_zerop:                             # @real_zerop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp259:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp260:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp261:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp262:
	.cfi_def_cfa_offset 80
.Ltmp263:
	.cfi_offset %rbx, -32
.Ltmp264:
	.cfi_offset %r14, -24
.Ltmp265:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB29_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	ja	.LBB29_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB29_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB29_1 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%rdi), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB29_1
.LBB29_4:                               # %while.end
	movq	%rax, %rcx
	andq	$262399, %rcx           # imm = 0x400FF
	cmpq	$26, %rcx
	jne	.LBB29_7
# BB#5:                                 # %land.lhs.true41
	movq	dconst0+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst0+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst0(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	48(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r15, %rax
	movq	$1, %r14
	je	.LBB29_12
# BB#6:                                 # %land.lhs.true41.lor.rhs_crit_edge
	movl	16(%rbx), %eax
.LBB29_7:                               # %lor.rhs
	andq	$255, %rax
	xorq	%r14, %r14
	cmpq	$27, %rax
	jne	.LBB29_12
# BB#8:                                 # %land.lhs.true49
	movq	32(%rbx), %rdi
	callq	real_zerop
	testq	%r15, %rax
	je	.LBB29_12
# BB#9:                                 # %land.rhs52
	movq	40(%rbx), %rdi
	callq	real_zerop
	testq	%r15, %rax
	je	.LBB29_10
# BB#11:                                # %land.rhs52
	movabsq	$1, %r14
	jmp	.LBB29_12
.LBB29_10:
	movabsq	$0, %r14
.LBB29_12:                              # %lor.end
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp266:
	.size	real_zerop, .Ltmp266-real_zerop
	.cfi_endproc

	.globl	real_onep
	.align	16, 0x90
	.type	real_onep,@function
real_onep:                              # @real_onep
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp271:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp272:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp273:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp274:
	.cfi_def_cfa_offset 80
.Ltmp275:
	.cfi_offset %rbx, -32
.Ltmp276:
	.cfi_offset %r14, -24
.Ltmp277:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	ja	.LBB30_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB30_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB30_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB30_1 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%rdi), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB30_1
.LBB30_4:                               # %while.end
	movq	%rax, %rcx
	andq	$262399, %rcx           # imm = 0x400FF
	cmpq	$26, %rcx
	jne	.LBB30_7
# BB#5:                                 # %land.lhs.true41
	movq	dconst1+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst1+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst1(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	48(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r15, %rax
	movq	$1, %r14
	je	.LBB30_12
# BB#6:                                 # %land.lhs.true41.lor.rhs_crit_edge
	movl	16(%rbx), %eax
.LBB30_7:                               # %lor.rhs
	andq	$255, %rax
	xorq	%r14, %r14
	cmpq	$27, %rax
	jne	.LBB30_12
# BB#8:                                 # %land.lhs.true49
	movq	32(%rbx), %rdi
	callq	real_onep
	testq	%r15, %rax
	je	.LBB30_12
# BB#9:                                 # %land.rhs52
	movq	40(%rbx), %rdi
	callq	real_zerop
	testq	%r15, %rax
	je	.LBB30_10
# BB#11:                                # %land.rhs52
	movabsq	$1, %r14
	jmp	.LBB30_12
.LBB30_10:
	movabsq	$0, %r14
.LBB30_12:                              # %lor.end
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp278:
	.size	real_onep, .Ltmp278-real_onep
	.cfi_endproc

	.globl	real_twop
	.align	16, 0x90
	.type	real_twop,@function
real_twop:                              # @real_twop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp283:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp284:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp285:
	.cfi_def_cfa_offset 32
	subq	$48, %rsp
.Ltmp286:
	.cfi_def_cfa_offset 80
.Ltmp287:
	.cfi_offset %rbx, -32
.Ltmp288:
	.cfi_offset %r14, -24
.Ltmp289:
	.cfi_offset %r15, -16
	movq	global_trees(%rip), %rcx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB31_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	ja	.LBB31_4
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	32(%rbx), %rdi
	cmpq	%rcx, %rdi
	je	.LBB31_4
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB31_1 Depth=1
	movq	8(%rbx), %rdx
	movq	8(%rdi), %rsi
	movl	60(%rdx), %edx
	movl	60(%rsi), %esi
	xorq	%rdx, %rsi
	testq	$65024, %rsi            # imm = 0xFE00
	je	.LBB31_1
.LBB31_4:                               # %while.end
	movq	%rax, %rcx
	andq	$262399, %rcx           # imm = 0x400FF
	cmpq	$26, %rcx
	jne	.LBB31_7
# BB#5:                                 # %land.lhs.true41
	movq	dconst2+16(%rip), %rax
	movq	%rax, 40(%rsp)
	movq	dconst2+8(%rip), %rax
	movq	%rax, 32(%rsp)
	movq	dconst2(%rip), %rax
	movq	%rax, 24(%rsp)
	movq	48(%rbx), %rax
	movq	%rax, 16(%rsp)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, (%rsp)
	callq	ereal_cmp
	testq	%r15, %rax
	movq	$1, %r14
	je	.LBB31_12
# BB#6:                                 # %land.lhs.true41.lor.rhs_crit_edge
	movl	16(%rbx), %eax
.LBB31_7:                               # %lor.rhs
	andq	$255, %rax
	xorq	%r14, %r14
	cmpq	$27, %rax
	jne	.LBB31_12
# BB#8:                                 # %land.lhs.true49
	movq	32(%rbx), %rdi
	callq	real_twop
	testq	%r15, %rax
	je	.LBB31_12
# BB#9:                                 # %land.rhs52
	movq	40(%rbx), %rdi
	callq	real_zerop
	testq	%r15, %rax
	je	.LBB31_10
# BB#11:                                # %land.rhs52
	movabsq	$1, %r14
	jmp	.LBB31_12
.LBB31_10:
	movabsq	$0, %r14
.LBB31_12:                              # %lor.end
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp290:
	.size	real_twop, .Ltmp290-real_twop
	.cfi_endproc

	.globl	really_constant_p
	.align	16, 0x90
	.type	really_constant_p,@function
really_constant_p:                      # @really_constant_p
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB32_2
	.align	16, 0x90
.LBB32_1:                               # %while.body
                                        #   in Loop: Header=BB32_2 Depth=1
	movq	32(%rdi), %rdi
	movl	16(%rdi), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	addq	$-114, %rdx
.LBB32_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	%rcx, %rdx
	cmpq	$3, %rdx
	jb	.LBB32_1
# BB#3:                                 # %lor.rhs
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	retq
.Ltmp291:
	.size	really_constant_p, .Ltmp291-really_constant_p
	.cfi_endproc

	.globl	value_member
	.align	16, 0x90
	.type	value_member,@function
value_member:                           # @value_member
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB33_1
	.align	16, 0x90
.LBB33_5:                               # %if.end
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	(%rsi), %rsi
.LBB33_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB33_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB33_1 Depth=1
	movq	32(%rsi), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB33_5
# BB#3:
	movq	%rsi, %rax
.LBB33_4:                               # %return
	retq
.Ltmp292:
	.size	value_member, .Ltmp292-value_member
	.cfi_endproc

	.globl	purpose_member
	.align	16, 0x90
	.type	purpose_member,@function
purpose_member:                         # @purpose_member
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB34_1
	.align	16, 0x90
.LBB34_5:                               # %if.end
                                        #   in Loop: Header=BB34_1 Depth=1
	movq	(%rsi), %rsi
.LBB34_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB34_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB34_1 Depth=1
	movq	24(%rsi), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB34_5
# BB#3:
	movq	%rsi, %rax
.LBB34_4:                               # %return
	retq
.Ltmp293:
	.size	purpose_member, .Ltmp293-purpose_member
	.cfi_endproc

	.globl	binfo_member
	.align	16, 0x90
	.type	binfo_member,@function
binfo_member:                           # @binfo_member
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB35_1
	.align	16, 0x90
.LBB35_5:                               # %if.end
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	(%rsi), %rsi
.LBB35_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB35_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB35_1 Depth=1
	movq	8(%rsi), %rcx
	cmpq	%rdi, %rcx
	jne	.LBB35_5
# BB#3:
	movq	%rsi, %rax
.LBB35_4:                               # %return
	retq
.Ltmp294:
	.size	binfo_member, .Ltmp294-binfo_member
	.cfi_endproc

	.globl	chain_member
	.align	16, 0x90
	.type	chain_member,@function
chain_member:                           # @chain_member
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rsi, %rsi
	je	.LBB36_4
# BB#1:
	movq	$1, %rax
	.align	16, 0x90
.LBB36_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rdi, %rsi
	je	.LBB36_5
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB36_2 Depth=1
	movq	(%rsi), %rsi
	testq	%rsi, %rsi
	jne	.LBB36_2
.LBB36_4:
	xorq	%rax, %rax
.LBB36_5:                               # %return
	retq
.Ltmp295:
	.size	chain_member, .Ltmp295-chain_member
	.cfi_endproc

	.globl	chain_member_value
	.align	16, 0x90
	.type	chain_member_value,@function
chain_member_value:                     # @chain_member_value
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_3:                               # %if.end
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	(%rsi), %rsi
.LBB37_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB37_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	32(%rsi), %rax
	cmpq	%rdi, %rax
	movq	$1, %rax
	jne	.LBB37_3
	jmp	.LBB37_5
.LBB37_4:
	xorq	%rax, %rax
.LBB37_5:                               # %return
	retq
.Ltmp296:
	.size	chain_member_value, .Ltmp296-chain_member_value
	.cfi_endproc

	.globl	chain_member_purpose
	.align	16, 0x90
	.type	chain_member_purpose,@function
chain_member_purpose:                   # @chain_member_purpose
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB38_1
	.align	16, 0x90
.LBB38_3:                               # %if.end
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	(%rsi), %rsi
.LBB38_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rsi, %rsi
	je	.LBB38_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB38_1 Depth=1
	movq	24(%rsi), %rax
	cmpq	%rdi, %rax
	movq	$1, %rax
	jne	.LBB38_3
	jmp	.LBB38_5
.LBB38_4:
	xorq	%rax, %rax
.LBB38_5:                               # %return
	retq
.Ltmp297:
	.size	chain_member_purpose, .Ltmp297-chain_member_purpose
	.cfi_endproc

	.globl	list_length
	.align	16, 0x90
	.type	list_length,@function
list_length:                            # @list_length
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB39_2
	.align	16, 0x90
.LBB39_1:                               # %for.body
                                        #   in Loop: Header=BB39_2 Depth=1
	movq	(%rdi), %rdi
	incq	%rax
.LBB39_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	jne	.LBB39_1
# BB#3:                                 # %for.end
	retq
.Ltmp298:
	.size	list_length, .Ltmp298-list_length
	.cfi_endproc

	.globl	fields_length
	.align	16, 0x90
	.type	fields_length,@function
fields_length:                          # @fields_length
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB40_5
# BB#1:                                 # %for.body.lr.ph
	xorq	%rax, %rax
	movabsq	$0, %rdx
	movabsq	$1, %rsi
	.align	16, 0x90
.LBB40_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rcx), %edi
	cmpq	$37, %rdi
	movq	%rdx, %rdi
	jne	.LBB40_4
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	%rsi, %rdi
.LBB40_4:                               # %for.body
                                        #   in Loop: Header=BB40_2 Depth=1
	movq	(%rcx), %rcx
	addq	%rdi, %rax
	testq	%rcx, %rcx
	jne	.LBB40_2
.LBB40_5:                               # %for.end
	retq
.Ltmp299:
	.size	fields_length, .Ltmp299-fields_length
	.cfi_endproc

	.globl	chainon
	.align	16, 0x90
	.type	chainon,@function
chainon:                                # @chainon
	.cfi_startproc
# BB#0:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB41_1
# BB#2:
	movq	%rdi, %rcx
	.align	16, 0x90
.LBB41_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB41_3
# BB#4:                                 # %for.end
	movq	%rsi, (%rax)
	movq	%rdi, %rax
	retq
.LBB41_1:
	movq	%rsi, %rdi
	movq	%rdi, %rax
	retq
.Ltmp300:
	.size	chainon, .Ltmp300-chainon
	.cfi_endproc

	.globl	tree_last
	.align	16, 0x90
	.type	tree_last,@function
tree_last:                              # @tree_last
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	jmp	.LBB42_2
	.align	16, 0x90
.LBB42_1:                               # %while.cond
                                        #   in Loop: Header=BB42_2 Depth=1
	movq	%rdi, %rax
	movq	(%rax), %rdi
.LBB42_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rdi, %rdi
	jne	.LBB42_1
# BB#3:                                 # %if.end
	retq
.Ltmp301:
	.size	tree_last, .Ltmp301-tree_last
	.cfi_endproc

	.globl	nreverse
	.align	16, 0x90
	.type	nreverse,@function
nreverse:                               # @nreverse
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rcx, %rcx
	testq	%rdi, %rdi
	je	.LBB43_1
	.align	16, 0x90
.LBB43_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rax
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	movq	%rcx, (%rax)
	movq	%rax, %rcx
	jne	.LBB43_2
# BB#3:                                 # %for.end
	retq
.LBB43_1:
	xorq	%rax, %rax
	retq
.Ltmp302:
	.size	nreverse, .Ltmp302-nreverse
	.cfi_endproc

	.globl	listify
	.align	16, 0x90
	.type	listify,@function
listify:                                # @listify
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp308:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp309:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp310:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp311:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp312:
	.cfi_def_cfa_offset 48
.Ltmp313:
	.cfi_offset %rbx, -40
.Ltmp314:
	.cfi_offset %r12, -32
.Ltmp315:
	.cfi_offset %r14, -24
.Ltmp316:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	je	.LBB44_1
# BB#2:
	movq	$2, %r14
	xorq	%r12, %r12
	.align	16, 0x90
.LBB44_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%r14d, 16(%rax)
	movq	$0, (%rax)
	movq	$0, 24(%rax)
	movq	%rbx, 32(%rax)
	testq	%r12, %r12
	movq	%rax, %rcx
	je	.LBB44_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB44_3 Depth=1
	movq	%rax, (%r12)
	movq	%r15, %rcx
.LBB44_5:                               # %if.end
                                        #   in Loop: Header=BB44_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	movq	%rcx, %r15
	movq	%rax, %r12
	jne	.LBB44_3
	jmp	.LBB44_6
.LBB44_1:
	xorq	%rcx, %rcx
.LBB44_6:                               # %while.end
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp317:
	.size	listify, .Ltmp317-listify
	.cfi_endproc

	.globl	tree_cons
	.align	16, 0x90
	.type	tree_cons,@function
tree_cons:                              # @tree_cons
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp322:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp323:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp324:
	.cfi_def_cfa_offset 32
.Ltmp325:
	.cfi_offset %rbx, -32
.Ltmp326:
	.cfi_offset %r14, -24
.Ltmp327:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$2, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, (%rax)
	movq	%rbx, 24(%rax)
	movq	%r14, 32(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp328:
	.size	tree_cons, .Ltmp328-tree_cons
	.cfi_endproc

	.globl	build_tree_list
	.align	16, 0x90
	.type	build_tree_list,@function
build_tree_list:                        # @build_tree_list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp332:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp333:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp334:
	.cfi_def_cfa_offset 32
.Ltmp335:
	.cfi_offset %rbx, -24
.Ltmp336:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$2, %rdi
	callq	make_node
	movq	%rbx, 24(%rax)
	movq	%r14, 32(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp337:
	.size	build_tree_list, .Ltmp337-build_tree_list
	.cfi_endproc

	.globl	size_in_bytes
	.align	16, 0x90
	.type	size_in_bytes,@function
size_in_bytes:                          # @size_in_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp340:
	.cfi_def_cfa_offset 16
.Ltmp341:
	.cfi_offset %rbx, -16
	movq	global_trees(%rip), %rax
	cmpq	%rdi, %rax
	je	.LBB47_1
# BB#2:                                 # %if.end
	movq	128(%rdi), %rsi
	movq	40(%rsi), %rbx
	testq	%rbx, %rbx
	je	.LBB47_3
# BB#4:                                 # %if.end5
	movzbl	16(%rbx), %eax
	cmpq	$25, %rax
	jne	.LBB47_6
# BB#5:                                 # %if.then7
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	force_fit_type
	jmp	.LBB47_6
.LBB47_1:                               # %if.then
	movq	global_trees+88(%rip), %rbx
	jmp	.LBB47_6
.LBB47_3:                               # %if.then4
	xorq	%rdi, %rdi
	callq	incomplete_type_error
	movq	global_trees+120(%rip), %rbx
.LBB47_6:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp342:
	.size	size_in_bytes, .Ltmp342-size_in_bytes
	.cfi_endproc

	.globl	int_size_in_bytes
	.align	16, 0x90
	.type	int_size_in_bytes,@function
int_size_in_bytes:                      # @int_size_in_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	global_trees(%rip), %rcx
	xorq	%rax, %rax
	cmpq	%rdi, %rcx
	je	.LBB48_6
# BB#1:                                 # %if.end
	movq	128(%rdi), %rax
	movq	40(%rax), %rcx
	testq	%rcx, %rcx
	movq	$-1, %rax
	je	.LBB48_6
# BB#2:                                 # %lor.lhs.false
	movl	16(%rcx), %edx
	andq	$524543, %rdx           # imm = 0x800FF
	cmpq	$25, %rdx
	jne	.LBB48_6
# BB#3:                                 # %lor.lhs.false9
	cmpq	$0, 40(%rcx)
	jne	.LBB48_6
# BB#4:                                 # %lor.lhs.false12
	movq	32(%rcx), %rax
	cmpq	$-2, %rax
	jg	.LBB48_6
# BB#5:                                 # %lor.lhs.false12
	movq	$-1, %rax
.LBB48_6:                               # %return
	retq
.Ltmp343:
	.size	int_size_in_bytes, .Ltmp343-int_size_in_bytes
	.cfi_endproc

	.globl	bit_position
	.align	16, 0x90
	.type	bit_position,@function
bit_position:                           # @bit_position
	.cfi_startproc
# BB#0:                                 # %entry
	movq	88(%rdi), %rax
	movq	160(%rdi), %rsi
	movq	%rax, %rdi
	jmp	bit_from_pos  # TAILCALL
.Ltmp344:
	.size	bit_position, .Ltmp344-bit_position
	.cfi_endproc

	.globl	int_bit_position
	.align	16, 0x90
	.type	int_bit_position,@function
int_bit_position:                       # @int_bit_position
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp346:
	.cfi_def_cfa_offset 16
	movq	88(%rdi), %rax
	movq	160(%rdi), %rsi
	movq	%rax, %rdi
	callq	bit_from_pos
	movl	16(%rax), %ecx
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$25, %rcx
	jne	.LBB50_4
# BB#1:                                 # %land.rhs.i.i
	movq	40(%rax), %rcx
	cmpq	$-1, %rcx
	jne	.LBB50_2
# BB#5:                                 # %land.lhs.true16.i.i
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB50_4
# BB#6:                                 # %land.lhs.true21.i.i
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB50_7
	jmp	.LBB50_4
.LBB50_2:                               # %land.rhs.i.i
	testq	%rcx, %rcx
	jne	.LBB50_4
# BB#3:                                 # %land.lhs.true6.i.i
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	js	.LBB50_4
.LBB50_7:                               # %tree_low_cst.exit
	movq	%rcx, %rax
	popq	%rdx
	retq
.LBB50_4:                               # %if.else.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp347:
	.size	int_bit_position, .Ltmp347-int_bit_position
	.cfi_endproc

	.globl	tree_low_cst
	.align	16, 0x90
	.type	tree_low_cst,@function
tree_low_cst:                           # @tree_low_cst
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp349:
	.cfi_def_cfa_offset 16
	movl	16(%rdi), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$25, %rax
	jne	.LBB51_7
# BB#1:                                 # %land.rhs.i
	movq	40(%rdi), %rcx
	testq	%rcx, %rcx
	jne	.LBB51_3
# BB#2:                                 # %land.lhs.true6.i
	movq	32(%rdi), %rax
	testq	%rax, %rax
	jns	.LBB51_10
.LBB51_3:                               # %lor.lhs.false.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB51_4
# BB#8:                                 # %host_integerp.exit
	testq	%rcx, %rcx
	je	.LBB51_9
	jmp	.LBB51_7
.LBB51_4:                               # %land.lhs.true11.i
	cmpq	$-1, %rcx
	jne	.LBB51_7
# BB#5:                                 # %land.lhs.true16.i
	cmpq	$0, 32(%rdi)
	jns	.LBB51_7
# BB#6:                                 # %land.lhs.true21.i
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB51_7
.LBB51_9:                               # %host_integerp.exit.if.then_crit_edge
	movq	32(%rdi), %rax
.LBB51_10:                              # %if.then
	popq	%rdx
	retq
.LBB51_7:                               # %if.else
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp350:
	.size	tree_low_cst, .Ltmp350-tree_low_cst
	.cfi_endproc

	.globl	byte_position
	.align	16, 0x90
	.type	byte_position,@function
byte_position:                          # @byte_position
	.cfi_startproc
# BB#0:                                 # %entry
	movq	88(%rdi), %rax
	movq	160(%rdi), %rsi
	movq	%rax, %rdi
	jmp	byte_from_pos  # TAILCALL
.Ltmp351:
	.size	byte_position, .Ltmp351-byte_position
	.cfi_endproc

	.globl	int_byte_position
	.align	16, 0x90
	.type	int_byte_position,@function
int_byte_position:                      # @int_byte_position
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp353:
	.cfi_def_cfa_offset 16
	movq	88(%rdi), %rax
	movq	160(%rdi), %rsi
	movq	%rax, %rdi
	callq	byte_from_pos
	movl	16(%rax), %ecx
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$25, %rcx
	jne	.LBB53_4
# BB#1:                                 # %land.rhs.i.i
	movq	40(%rax), %rcx
	cmpq	$-1, %rcx
	jne	.LBB53_2
# BB#5:                                 # %land.lhs.true16.i.i
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	jns	.LBB53_4
# BB#6:                                 # %land.lhs.true21.i.i
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB53_7
	jmp	.LBB53_4
.LBB53_2:                               # %land.rhs.i.i
	testq	%rcx, %rcx
	jne	.LBB53_4
# BB#3:                                 # %land.lhs.true6.i.i
	movq	32(%rax), %rcx
	testq	%rcx, %rcx
	js	.LBB53_4
.LBB53_7:                               # %tree_low_cst.exit
	movq	%rcx, %rax
	popq	%rdx
	retq
.LBB53_4:                               # %if.else.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp354:
	.size	int_byte_position, .Ltmp354-int_byte_position
	.cfi_endproc

	.globl	expr_align
	.align	16, 0x90
	.type	expr_align,@function
expr_align:                             # @expr_align
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp359:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp360:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp361:
	.cfi_def_cfa_offset 32
.Ltmp362:
	.cfi_offset %rbx, -32
.Ltmp363:
	.cfi_offset %r14, -24
.Ltmp364:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB54_1
	.align	16, 0x90
.LBB54_9:                               # %sw.bb3
                                        #   in Loop: Header=BB54_1 Depth=1
	movq	32(%rbx), %rbx
.LBB54_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %ecx
	cmpq	$113, %rcx
	jg	.LBB54_4
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB54_1 Depth=1
	addq	$-30, %rcx
	andq	%r14, %rcx
	cmpq	$28, %rcx
	ja	.LBB54_14
# BB#3:                                 # %tailrecurse
                                        #   in Loop: Header=BB54_1 Depth=1
	movq	$16, %rax
	jmpq	*.LJTI54_0(,%rcx,8)
	.align	16, 0x90
.LBB54_4:                               # %tailrecurse
                                        #   in Loop: Header=BB54_1 Depth=1
	leaq	-118(%rcx), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jb	.LBB54_9
# BB#5:                                 # %tailrecurse
	addq	$-114, %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	ja	.LBB54_14
# BB#6:                                 # %sw.bb
	movq	32(%rbx), %rdi
	callq	expr_align
	movq	8(%rbx), %rcx
	movl	64(%rcx), %ecx
	andq	%rax, %r14
	cmpq	%rcx, %r14
	ja	.LBB54_8
# BB#7:                                 # %sw.bb
	movq	%rcx, %rax
	jmp	.LBB54_8
.LBB54_13:                              # %sw.bb22
	movl	56(%rbx), %eax
	andq	$16777215, %rax         # imm = 0xFFFFFF
	jne	.LBB54_8
.LBB54_14:                              # %sw.epilog
	movq	8(%rbx), %rax
	movl	64(%rax), %eax
.LBB54_8:                               # %sw.bb
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB54_10:                              # %sw.bb8
	movq	40(%rbx), %rdi
	callq	expr_align
	movq	48(%rbx), %rdi
	movq	%rax, %r15
	movq	%r15, %rbx
	andq	%r14, %rbx
	callq	expr_align
	andq	%rax, %r14
	cmpq	%r14, %rbx
	jb	.LBB54_12
# BB#11:                                # %sw.bb8
	movq	%rax, %r15
.LBB54_12:                              # %sw.bb8
	movq	%r15, %rax
	jmp	.LBB54_8
.Ltmp365:
	.size	expr_align, .Ltmp365-expr_align
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI54_0:
	.quad	.LBB54_8
	.quad	.LBB54_13
	.quad	.LBB54_13
	.quad	.LBB54_14
	.quad	.LBB54_13
	.quad	.LBB54_13
	.quad	.LBB54_13
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_9
	.quad	.LBB54_9
	.quad	.LBB54_9
	.quad	.LBB54_9
	.quad	.LBB54_10
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_14
	.quad	.LBB54_9
	.quad	.LBB54_9
	.quad	.LBB54_14
	.quad	.LBB54_9

	.text
	.globl	array_type_nelts
	.align	16, 0x90
	.type	array_type_nelts,@function
array_type_nelts:                       # @array_type_nelts
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp369:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp370:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp371:
	.cfi_def_cfa_offset 32
.Ltmp372:
	.cfi_offset %rbx, -24
.Ltmp373:
	.cfi_offset %r14, -16
	movq	24(%rdi), %rax
	testq	%rax, %rax
	je	.LBB55_1
# BB#2:                                 # %if.end
	movq	104(%rax), %r14
	movq	112(%rax), %rbx
	movq	%r14, %rdi
	callq	integer_zerop
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB55_3
# BB#4:                                 # %cond.false
	movq	8(%rbx), %rsi
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	fold  # TAILCALL
.LBB55_1:                               # %if.then
	movq	global_trees(%rip), %rbx
.LBB55_3:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp374:
	.size	array_type_nelts, .Ltmp374-array_type_nelts
	.cfi_endproc

	.globl	build
	.align	16, 0x90
	.type	build,@function
build:                                  # @build
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp378:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp379:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp380:
	.cfi_def_cfa_offset 96
.Ltmp381:
	.cfi_offset %rbx, -24
.Ltmp382:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
                                        # kill: RDI<def> RBX<kill>
	callq	make_node
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movl	tree_code_length(,%rdx,4), %r9d
	cmpq	$117, %rdx
	movq	%r14, 8(%rax)
	movq	$2, %r8
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jg	.LBB56_4
# BB#1:                                 # %entry
	andq	%rbx, %rcx
	cmpq	$54, %rcx
	jne	.LBB56_2
# BB#8:                                 # %sw.bb3.i
	movq	$3, %r8
	jmp	.LBB56_9
.LBB56_4:                               # %entry
	andq	%rbx, %rcx
	cmpq	$118, %rcx
	je	.LBB56_9
# BB#5:                                 # %entry
	cmpq	$120, %rcx
	je	.LBB56_7
# BB#6:                                 # %entry
	cmpq	$136, %rcx
	jne	.LBB56_3
.LBB56_7:                               # %sw.bb1.i
	xorq	%r8, %r8
	jmp	.LBB56_9
.LBB56_2:                               # %entry
	cmpq	$55, %rcx
	je	.LBB56_9
.LBB56_3:                               # %sw.default.i
	movq	%r9, %r8
.LBB56_9:                               # %first_rtl_op.exit
	movzbl	tree_code_type(%rdx), %esi
	leaq	-49(%rsi), %rcx
	andq	$255, %rcx
	xorq	%r10, %r10
	cmpq	$50, %rcx
	ja	.LBB56_11
# BB#10:                                # %switch.lookup
	movabsq	$1125899906844675, %r10 # imm = 0x4000000000803
	shrq	%cl, %r10
	andq	$1, %r10
.LBB56_11:                              # %lor.end
	movabsq	$4294967295, %r11       # imm = 0xFFFFFFFF
	movq	%r9, %r14
	andq	%r11, %r14
	cmpq	$1, %r14
	jne	.LBB56_12
# BB#36:                                # %if.then116
	movl	48(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$40, %rdx
	ja	.LBB56_38
# BB#37:                                # %vaarg.in_reg122
	movq	64(%rsp), %rcx
	addq	%rdi, %rcx
	addq	$8, %rdx
	movl	%edx, 48(%rsp)
	jmp	.LBB56_39
.LBB56_12:                              # %lor.end
	cmpq	$2, %r14
	jne	.LBB56_13
# BB#17:                                # %if.then
	movl	48(%rsp), %ebx
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$41, %rbx
	jae	.LBB56_18
# BB#20:                                # %vaarg.end
	movq	64(%rsp), %rcx
	leaq	8(%rbx), %rsi
	movl	%esi, 48(%rsp)
	movq	(%rcx,%rdx), %r9
	andq	%rsi, %r11
	cmpq	$40, %r11
	ja	.LBB56_19
# BB#21:                                # %vaarg.in_reg29
	movq	64(%rsp), %rdx
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdx, %rsi
	addq	$16, %rbx
	movl	%ebx, 48(%rsp)
	jmp	.LBB56_22
.LBB56_38:                              # %vaarg.in_mem124
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 56(%rsp)
.LBB56_39:                              # %vaarg.end128
	andq	$255, %rsi
	cmpq	$115, %rsi
	jne	.LBB56_49
# BB#40:                                # %if.end136
	movq	(%rcx), %rcx
	movq	%rcx, 32(%rax)
	jmp	.LBB56_48
.LBB56_13:                              # %for.cond.preheader
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	testq	%r9, %r9
	jle	.LBB56_48
# BB#14:                                # %for.body.lr.ph
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB56_15:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%rsp), %edx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$40, %rdx
	ja	.LBB56_41
# BB#16:                                # %vaarg.in_reg147
                                        #   in Loop: Header=BB56_15 Depth=1
	movq	64(%rsp), %rcx
	addq	%rdi, %rcx
	addq	$8, %rdx
	movl	%edx, 48(%rsp)
	jmp	.LBB56_42
	.align	16, 0x90
.LBB56_41:                              # %vaarg.in_mem149
                                        #   in Loop: Header=BB56_15 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 56(%rsp)
.LBB56_42:                              # %vaarg.end153
                                        #   in Loop: Header=BB56_15 Depth=1
	movq	(%rcx), %rdx
	testq	%rdx, %rdx
	movq	%rdx, 32(%rax,%rsi,8)
	je	.LBB56_47
# BB#43:                                # %vaarg.end153
                                        #   in Loop: Header=BB56_15 Depth=1
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r8, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rdi, %rbx
	jle	.LBB56_47
# BB#44:                                # %if.then163
                                        #   in Loop: Header=BB56_15 Depth=1
	movl	16(%rdx), %edi
	testq	$256, %rdi              # imm = 0x100
	je	.LBB56_46
# BB#45:                                # %if.then170
                                        #   in Loop: Header=BB56_15 Depth=1
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movl	16(%rdx), %edi
.LBB56_46:                              # %if.end176
                                        #   in Loop: Header=BB56_15 Depth=1
	movq	$54, %rcx
	shlq	%cl, %rdi
	movq	$63, %rcx
	sarq	%cl, %rdi
	andq	%rdi, %r10
.LBB56_47:                              # %for.inc
                                        #   in Loop: Header=BB56_15 Depth=1
	incq	%rsi
	movq	%rsi, %rcx
	andq	%r11, %rcx
	cmpq	%r14, %rcx
	jne	.LBB56_15
	jmp	.LBB56_48
.LBB56_18:                              # %vaarg.end.thread
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdx
	movq	%rdx, 56(%rsp)
	movq	(%rcx), %r9
.LBB56_19:                              # %vaarg.in_mem31
	movq	56(%rsp), %rsi
	leaq	8(%rsi), %rcx
	movq	%rcx, 56(%rsp)
.LBB56_22:                              # %vaarg.end35
	movq	(%rsi), %rdi
	movq	%r9, 32(%rax)
	movq	%rdi, 40(%rax)
	movl	16(%rax), %esi
	movq	%rsi, %rbx
	orq	$4096, %rbx             # imm = 0x1000
	testq	%r9, %r9
	movl	%ebx, 16(%rax)
	je	.LBB56_29
# BB#23:                                # %vaarg.end35
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB56_29
# BB#24:                                # %if.then44
	movl	16(%r9), %edx
	testq	$256, %rdx              # imm = 0x100
	je	.LBB56_26
# BB#25:                                # %if.then49
	orq	$4352, %rsi             # imm = 0x1100
	movl	%esi, 16(%rax)
	movl	16(%r9), %edx
	movq	%rsi, %rbx
.LBB56_26:                              # %if.end
	testq	$4096, %rdx             # imm = 0x1000
	jne	.LBB56_28
# BB#27:                                # %if.then61
	movabsq	$4294963199, %rcx       # imm = 0xFFFFEFFF
	andq	%rcx, %rbx
	movl	%ebx, 16(%rax)
	movl	16(%r9), %edx
.LBB56_28:                              # %if.end66
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	%rdx, %r10
.LBB56_29:                              # %if.end74
	testq	%rdi, %rdi
	je	.LBB56_48
# BB#30:                                # %if.end74
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	$2, %r8
	jl	.LBB56_48
# BB#31:                                # %if.then79
	movl	16(%rdi), %edx
	testq	$256, %rdx              # imm = 0x100
	je	.LBB56_33
# BB#32:                                # %if.then86
	orq	$256, %rbx              # imm = 0x100
	movl	%ebx, 16(%rax)
	movl	16(%rdi), %edx
.LBB56_33:                              # %if.end92
	testq	$4096, %rdx             # imm = 0x1000
	jne	.LBB56_35
# BB#34:                                # %if.then99
	movabsq	$4294963199, %rcx       # imm = 0xFFFFEFFF
	andq	%rcx, %rbx
	movl	%ebx, 16(%rax)
	movl	16(%rdi), %edx
.LBB56_35:                              # %if.end104
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	%rdx, %r10
.LBB56_48:                              # %if.end187
	movl	16(%rax), %ecx
	andq	$1, %r10
	shlq	$9, %r10
	movabsq	$4294966783, %rdx       # imm = 0xFFFFFDFF
	andq	%rcx, %rdx
	orq	%r10, %rdx
	movl	%edx, 16(%rax)
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB56_49:                              # %if.then135
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.build, %rcx
	movq	$2451, %rsi             # imm = 0x993
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp383:
	.size	build, .Ltmp383-build
	.cfi_endproc

	.globl	staticp
	.align	16, 0x90
	.type	staticp,@function
staticp:                                # @staticp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp386:
	.cfi_def_cfa_offset 16
.Ltmp387:
	.cfi_offset %rbx, -16
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%rax, %rax
	jmp	.LBB57_1
	.align	16, 0x90
.LBB57_29:                              # %if.then
                                        #   in Loop: Header=BB57_1 Depth=1
	movq	32(%rdi), %rdi
.LBB57_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rdi), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	leaq	-29(%rcx), %rsi
	andq	%rbx, %rsi
	cmpq	$17, %rsi
	ja	.LBB57_30
# BB#2:                                 # %tailrecurse
                                        #   in Loop: Header=BB57_1 Depth=1
	jmpq	*.LJTI57_0(,%rsi,8)
.LBB57_27:                              # %sw.bb56
                                        #   in Loop: Header=BB57_1 Depth=1
	movq	8(%rdi), %rdx
	movq	32(%rdx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	jne	.LBB57_30
# BB#28:                                # %land.lhs.true
                                        #   in Loop: Header=BB57_1 Depth=1
	movq	40(%rdi), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$25, %rdx
	je	.LBB57_29
.LBB57_30:                              # %sw.default
	cmpq	$147, %rcx
	jb	.LBB57_33
# BB#31:                                # %if.then81
	popq	%rbx
	jmpq	*lang_hooks+88(%rip)  # TAILCALL
.LBB57_32:                              # %return.loopexit
	movq	$1, %rax
	jmp	.LBB57_33
.LBB57_3:                               # %land.lhs.true.i
	cmpq	$0, 184(%rdi)
	je	.LBB57_5
# BB#4:                                 # %if.then13.i
	movq	8(%rdi), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	8(%rax), %rax
	addq	$128, %rax
	jmp	.LBB57_6
.LBB57_17:                              # %sw.bb9
	movq	48(%rdi), %rcx
	testq	$262144, %rdx           # imm = 0x40000
	jne	.LBB57_19
# BB#18:                                # %sw.bb9
	xorq	%rax, %rax
	movq	%rcx, %rdx
	andq	$256, %rdx              # imm = 0x100
	je	.LBB57_33
.LBB57_19:                              # %land.rhs22
	testq	$16777216, %rcx         # imm = 0x1000000
	jne	.LBB57_20
# BB#21:                                # %land.rhs22
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB57_23:                              # %sw.bb40
	movq	40(%rdi), %rax
	movzbl	49(%rax), %ecx
	xorq	%rax, %rax
	testq	$16, %rcx
	jne	.LBB57_33
# BB#24:                                # %land.rhs47
	movq	32(%rdi), %rdi
	callq	staticp
	testq	%rbx, %rax
	je	.LBB57_25
# BB#26:                                # %land.rhs47
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB57_22:                              # %sw.bb33
	movq	$18, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%rdx, %rax
	popq	%rbx
	retq
.LBB57_5:                               # %if.else19.i
	leaq	80(%rdi), %rax
	jmp	.LBB57_6
.LBB57_9:                               # %if.then35.i
                                        #   in Loop: Header=BB57_6 Depth=1
	addq	$48, %rax
.LBB57_6:                               # %while.cond.preheader.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB57_14
# BB#7:                                 # %land.rhs.i
                                        #   in Loop: Header=BB57_6 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	je	.LBB57_13
# BB#8:                                 # %land.rhs.i
                                        #   in Loop: Header=BB57_6 Depth=1
	cmpq	$4, %rcx
	je	.LBB57_9
# BB#10:                                # %if.else36.i
                                        #   in Loop: Header=BB57_6 Depth=1
	movzbl	tree_code_type(%rcx), %ecx
	cmpq	$116, %rcx
	jne	.LBB57_12
# BB#11:                                # %cond.true.i.i
                                        #   in Loop: Header=BB57_6 Depth=1
	addq	$144, %rax
	jmp	.LBB57_6
.LBB57_12:                              # %cond.false.i.i
                                        #   in Loop: Header=BB57_6 Depth=1
	addq	$80, %rax
	jmp	.LBB57_6
.LBB57_20:
	movabsq	$0, %rax
	popq	%rbx
	retq
.LBB57_25:
	movabsq	$0, %rax
	popq	%rbx
	retq
.LBB57_13:                              # %lor.lhs.false
	movzbl	49(%rdi), %ecx
	xorq	%rax, %rax
	testq	$4, %rcx
	je	.LBB57_33
.LBB57_14:                              # %land.rhs
	movzbl	51(%rdi), %eax
	testq	$1, %rax
	jne	.LBB57_15
# BB#16:                                # %land.rhs
	movabsq	$1, %rax
	popq	%rbx
	retq
.LBB57_33:                              # %return
	popq	%rbx
	retq
.LBB57_15:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp388:
	.size	staticp, .Ltmp388-staticp
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI57_0:
	.quad	.LBB57_32
	.quad	.LBB57_3
	.quad	.LBB57_32
	.quad	.LBB57_30
	.quad	.LBB57_30
	.quad	.LBB57_17
	.quad	.LBB57_30
	.quad	.LBB57_30
	.quad	.LBB57_30
	.quad	.LBB57_30
	.quad	.LBB57_23
	.quad	.LBB57_33
	.quad	.LBB57_30
	.quad	.LBB57_30
	.quad	.LBB57_27
	.quad	.LBB57_27
	.quad	.LBB57_30
	.quad	.LBB57_22

	.text
	.globl	decl_function_context
	.align	16, 0x90
	.type	decl_function_context,@function
decl_function_context:                  # @decl_function_context
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB58_11
# BB#1:                                 # %entry
	cmpq	$30, %rcx
	jne	.LBB58_2
# BB#4:                                 # %land.lhs.true
	cmpq	$0, 184(%rdi)
	je	.LBB58_6
# BB#5:                                 # %if.then13
	movq	8(%rdi), %rcx
	movq	24(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	8(%rcx), %rdi
	addq	$128, %rdi
	jmp	.LBB58_7
.LBB58_2:                               # %entry
	cmpq	$118, %rcx
	jne	.LBB58_6
# BB#3:                                 # %if.then6
	addq	$40, %rdi
	jmp	.LBB58_7
.LBB58_6:                               # %if.else19
	addq	$80, %rdi
.LBB58_7:                               # %while.cond.preheader
	movq	(%rdi), %rcx
	jmp	.LBB58_8
	.align	16, 0x90
.LBB58_13:                              # %if.then35
                                        #   in Loop: Header=BB58_8 Depth=1
	addq	$48, %rcx
	movq	(%rcx), %rcx
.LBB58_8:                               # %while.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB58_11
# BB#9:                                 # %land.rhs
                                        #   in Loop: Header=BB58_8 Depth=1
	movzbl	16(%rcx), %edx
	cmpq	$30, %rdx
	je	.LBB58_10
# BB#12:                                # %land.rhs
                                        #   in Loop: Header=BB58_8 Depth=1
	cmpq	$4, %rdx
	je	.LBB58_13
# BB#15:                                # %if.else36
                                        #   in Loop: Header=BB58_8 Depth=1
	movzbl	tree_code_type(%rdx), %edx
	cmpq	$116, %rdx
	jne	.LBB58_17
# BB#16:                                # %cond.true.i
                                        #   in Loop: Header=BB58_8 Depth=1
	addq	$144, %rcx
	movq	(%rcx), %rcx
	jmp	.LBB58_8
.LBB58_17:                              # %cond.false.i
                                        #   in Loop: Header=BB58_8 Depth=1
	addq	$80, %rcx
	movq	(%rcx), %rcx
	jmp	.LBB58_8
.LBB58_10:
	movq	%rcx, %rax
.LBB58_11:                              # %return
	retq
.Ltmp389:
	.size	decl_function_context, .Ltmp389-decl_function_context
	.cfi_endproc

	.globl	save_expr
	.align	16, 0x90
	.type	save_expr,@function
save_expr:                              # @save_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp394:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp395:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 32
.Ltmp397:
	.cfi_offset %rbx, -32
.Ltmp398:
	.cfi_offset %r14, -24
.Ltmp399:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	fold
	movq	%rax, %rbx
	jmp	.LBB59_1
	.align	16, 0x90
.LBB59_14:                              # %while.body
                                        #   in Loop: Header=BB59_1 Depth=1
	movq	32(%rbx), %rbx
.LBB59_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$116, %rax
	je	.LBB59_14
# BB#2:                                 # %for.cond.preheader
	movq	%rbx, %rax
	jmp	.LBB59_3
	.align	16, 0x90
.LBB59_13:                              #   in Loop: Header=BB59_3 Depth=1
	addq	$32, %rax
	movq	(%rax), %rax
.LBB59_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rax), %edx
	movq	%rdx, %rcx
	andq	$255, %rcx
	movzbl	tree_code_type(%rcx), %esi
	cmpq	$50, %rsi
	jne	.LBB59_4
# BB#12:                                # %land.rhs
                                        #   in Loop: Header=BB59_3 Depth=1
	movq	40(%rax), %rsi
	movzbl	17(%rsi), %esi
	testq	$2, %rsi
	jne	.LBB59_13
	jmp	.LBB59_5
	.align	16, 0x90
.LBB59_4:                               # %for.cond
                                        #   in Loop: Header=BB59_3 Depth=1
	cmpq	$49, %rsi
	je	.LBB59_13
.LBB59_5:                               # %for.end
	testq	$512, %rdx              # imm = 0x200
	jne	.LBB59_11
# BB#6:                                 # %for.end
	andq	$4352, %rdx             # imm = 0x1100
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rdx
	cmpq	$4096, %rdx             # imm = 0x1000
	je	.LBB59_11
# BB#7:                                 # %lor.lhs.false42
	testq	%rcx, %rcx
	je	.LBB59_11
# BB#8:                                 # %lor.lhs.false42
	cmpq	$118, %rcx
	je	.LBB59_11
# BB#9:                                 # %if.end
	movq	%rbx, %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	jne	.LBB59_11
# BB#10:                                # %if.end59
	movq	8(%r14), %rsi
	movq	current_function_decl(%rip), %rcx
	movq	$118, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$4352, %rax             # imm = 0x1100
	movl	%eax, 16(%rbx)
.LBB59_11:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp400:
	.size	save_expr, .Ltmp400-save_expr
	.cfi_endproc

	.globl	contains_placeholder_p
	.align	16, 0x90
	.type	contains_placeholder_p,@function
contains_placeholder_p:                 # @contains_placeholder_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp405:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp406:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp407:
	.cfi_def_cfa_offset 32
.Ltmp408:
	.cfi_offset %rbx, -32
.Ltmp409:
	.cfi_offset %r14, -24
.Ltmp410:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB60_6
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	$1, %rbx
.LBB60_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r14), %ecx
	movq	%rcx, %rax
	andq	$255, %rax
	cmpq	$57, %rax
	je	.LBB60_42
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$58, %rax
	je	.LBB60_6
# BB#4:                                 # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	movsbq	tree_code_type(%rax), %rdx
	cmpq	$59, %rdx
	jg	.LBB60_7
# BB#5:                                 # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	addq	$-49, %rdx
	andq	%r15, %rdx
	cmpq	$2, %rdx
	jb	.LBB60_9
	jmp	.LBB60_6
	.align	16, 0x90
.LBB60_7:                               # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$100, %rdx
	jg	.LBB60_12
# BB#8:                                 # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	andq	%r15, %rdx
	cmpq	$60, %rdx
	je	.LBB60_9
	jmp	.LBB60_6
.LBB60_12:                              # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	andq	%r15, %rdx
	cmpq	$101, %rdx
	je	.LBB60_9
# BB#13:                                # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$120, %rdx
	je	.LBB60_17
# BB#14:                                # %if.end6
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$114, %rdx
	je	.LBB60_15
	jmp	.LBB60_6
	.align	16, 0x90
.LBB60_9:                               # %sw.bb23
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$117, %rax
	jg	.LBB60_22
# BB#10:                                # %sw.bb23
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	%rax, %rcx
	addq	$-46, %rcx
	andq	%r15, %rcx
	cmpq	$7, %rcx
	ja	.LBB60_24
# BB#11:                                # %sw.bb23
                                        #   in Loop: Header=BB60_2 Depth=1
	jmpq	*.LJTI60_0(,%rcx,8)
.LBB60_26:                              # %sw.bb24
                                        #   in Loop: Header=BB60_2 Depth=1
	addq	$40, %r14
	jmp	.LBB60_16
.LBB60_22:                              # %sw.bb23
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$118, %rax
	je	.LBB60_31
# BB#23:                                # %sw.bb23
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$120, %rax
	je	.LBB60_6
.LBB60_24:                              # %sw.epilog
                                        #   in Loop: Header=BB60_2 Depth=1
	movl	tree_code_length(,%rax,4), %eax
	cmpq	$2, %rax
	je	.LBB60_38
# BB#25:                                # %sw.epilog
                                        #   in Loop: Header=BB60_2 Depth=1
	cmpq	$1, %rax
	jne	.LBB60_6
.LBB60_15:                              # %sw.bb
                                        #   in Loop: Header=BB60_2 Depth=1
	addq	$32, %r14
.LBB60_16:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB60_2 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB60_2
	jmp	.LBB60_6
.LBB60_31:                              # %sw.bb49
	testq	$8192, %rcx             # imm = 0x2000
	jne	.LBB60_6
# BB#32:                                # %lor.lhs.false54
	cmpq	$0, 48(%r14)
	je	.LBB60_33
.LBB60_6:                               # %return.loopexit
	xorq	%rbx, %rbx
.LBB60_42:                              # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB60_33:                              # %if.end61
	orq	$8192, %rcx             # imm = 0x2000
	movl	%ecx, 16(%r14)
	movq	32(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	movabsq	$0, %rbx
	je	.LBB60_42
# BB#34:                                # %if.then71
	movl	16(%r14), %ecx
	movabsq	$4294959103, %rdx       # imm = 0xFFFFDFFF
	andq	%rcx, %rdx
	movl	%edx, 16(%r14)
	movq	%rax, %rbx
	jmp	.LBB60_42
.LBB60_17:                              # %sw.bb9
	cmpq	$2, %rax
	movabsq	$0, %rbx
	jne	.LBB60_42
# BB#18:                                # %if.then12
	movq	32(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	movq	$1, %rbx
	jne	.LBB60_42
# BB#19:                                # %lor.rhs
	movq	(%r14), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB60_42
# BB#20:                                # %land.rhs
	callq	contains_placeholder_p
	testq	%r15, %rax
	jne	.LBB60_41
# BB#21:
	movabsq	$0, %rbx
	jmp	.LBB60_42
.LBB60_38:                              # %sw.bb97
	movq	32(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	movq	$1, %rbx
	jne	.LBB60_42
# BB#39:                                # %lor.rhs103
	movq	40(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	jne	.LBB60_41
# BB#40:
	movabsq	$0, %rbx
	jmp	.LBB60_42
.LBB60_27:                              # %sw.bb30
	movq	32(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	movq	$1, %rbx
	jne	.LBB60_42
# BB#28:                                # %lor.lhs.false
	movq	40(%r14), %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	jne	.LBB60_42
# BB#29:                                # %lor.rhs41
	movq	48(%r14), %rax
	movq	%rax, %rdi
	callq	contains_placeholder_p
	testq	%r15, %rax
	jne	.LBB60_41
# BB#30:
	movabsq	$0, %rbx
	jmp	.LBB60_42
.LBB60_35:                              # %sw.bb77
	movq	40(%r14), %rdi
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB60_42
# BB#36:                                # %land.rhs83
	callq	contains_placeholder_p
	testq	%r15, %rax
	je	.LBB60_37
.LBB60_41:                              # %lor.rhs103
	movabsq	$1, %rbx
	jmp	.LBB60_42
.LBB60_37:
	movabsq	$0, %rbx
	jmp	.LBB60_42
.Ltmp411:
	.size	contains_placeholder_p, .Ltmp411-contains_placeholder_p
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI60_0:
	.quad	.LBB60_6
	.quad	.LBB60_26
	.quad	.LBB60_24
	.quad	.LBB60_24
	.quad	.LBB60_24
	.quad	.LBB60_27
	.quad	.LBB60_24
	.quad	.LBB60_35

	.text
	.globl	unsave_expr
	.align	16, 0x90
	.type	unsave_expr,@function
unsave_expr:                            # @unsave_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp415:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp416:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp417:
	.cfi_def_cfa_offset 32
.Ltmp418:
	.cfi_offset %rbx, -24
.Ltmp419:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$119, %rax
	je	.LBB61_7
# BB#1:                                 # %land.lhs.true.i
	movq	8(%rbx), %r14
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$119, %rcx
	movl	%ecx, 16(%rax)
	movq	%r14, 8(%rax)
	xorq	%rdx, %rdx
	movl	%edx, 24(%rax)
	movq	%rbx, 32(%rax)
	movl	tree_code_length+476(%rip), %edx
	testq	%rdx, %rdx
	movq	$631, %rdx              # imm = 0x277
	je	.LBB61_3
# BB#2:                                 # %if.then.i
	movl	16(%rbx), %edx
	andq	$256, %rdx              # imm = 0x100
	orq	$119, %rdx
	movl	%edx, 16(%rax)
	movl	16(%rbx), %ecx
	andq	$4096, %rcx             # imm = 0x1000
	orq	%rdx, %rcx
	movl	%ecx, 16(%rax)
	movq	%rcx, %rdx
	orq	$512, %rdx              # imm = 0x200
.LBB61_3:                               # %if.end.i
	movzbl	tree_code_type+119(%rip), %esi
	cmpq	$49, %rsi
	jne	.LBB61_6
# BB#4:                                 # %land.lhs.true47.i
	movzbl	17(%rbx), %esi
	testq	$2, %rsi
	je	.LBB61_6
# BB#5:                                 # %if.then53.i
	movl	%edx, 16(%rax)
	movq	%rdx, %rcx
.LBB61_6:                               # %build1.exit
	movl	16(%rbx), %edx
	andq	$256, %rdx              # imm = 0x100
	movabsq	$4294967039, %rsi       # imm = 0xFFFFFEFF
	andq	%rcx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %rbx
.LBB61_7:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp420:
	.size	unsave_expr, .Ltmp420-unsave_expr
	.cfi_endproc

	.globl	build1
	.align	16, 0x90
	.type	build1,@function
build1:                                 # @build1
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
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%r15, %rdx
	andq	$255, %rdx
	xorq	%rcx, %rcx
	testq	%r14, %r14
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%edx, 16(%rax)
	movq	%rbx, 8(%rax)
	movl	%ecx, 24(%rax)
	movq	%r14, 32(%rax)
	je	.LBB62_8
# BB#1:                                 # %land.lhs.true
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$117, %rsi
	jg	.LBB62_3
# BB#2:                                 # %land.lhs.true
	leaq	-54(%r15), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB62_7
	jmp	.LBB62_6
.LBB62_3:                               # %land.lhs.true
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%r15, %rcx
	cmpq	$118, %rcx
	je	.LBB62_7
# BB#4:                                 # %land.lhs.true
	cmpq	$120, %rcx
	je	.LBB62_14
# BB#5:                                 # %land.lhs.true
	cmpq	$136, %rcx
	je	.LBB62_15
.LBB62_6:                               # %first_rtl_op.exit
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movl	tree_code_length(,%rsi,4), %ecx
	testq	%rcx, %rcx
	je	.LBB62_8
.LBB62_7:                               # %if.then
	movl	16(%r14), %ecx
	andq	$256, %rcx              # imm = 0x100
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$4096, %rcx             # imm = 0x1000
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB62_8:                               # %if.end
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$119, %rsi
	jg	.LBB62_12
# BB#9:                                 # %if.end
	leaq	-48(%r15), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB62_14
# BB#10:                                # %if.end
	andq	%r15, %rcx
	cmpq	$41, %rcx
	jne	.LBB62_15
# BB#11:                                # %sw.bb36
	movabsq	$4294963199, %rcx       # imm = 0xFFFFEFFF
	andq	%rcx, %rdx
	jmp	.LBB62_19
.LBB62_12:                              # %if.end
	leaq	-120(%r15), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	cmpq	$13, %rcx
	ja	.LBB62_15
# BB#13:                                # %if.end
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$15873, %rsi            # imm = 0x3E01
	je	.LBB62_15
.LBB62_14:                              # %sw.bb
	movabsq	$4294962943, %rcx       # imm = 0xFFFFEEFF
	andq	%rdx, %rcx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	jmp	.LBB62_20
.LBB62_15:                              # %sw.default
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movzbl	tree_code_type(%r15), %ecx
	cmpq	$49, %rcx
	jne	.LBB62_20
# BB#16:                                # %sw.default
	testq	%r14, %r14
	je	.LBB62_20
# BB#17:                                # %land.lhs.true47
	movzbl	17(%r14), %ecx
	testq	$2, %rcx
	je	.LBB62_20
# BB#18:                                # %if.then53
	orq	$512, %rdx              # imm = 0x200
.LBB62_19:                              # %sw.epilog
	movl	%edx, 16(%rax)
.LBB62_20:                              # %sw.epilog
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp431:
	.size	build1, .Ltmp431-build1
	.cfi_endproc

	.globl	first_rtl_op
	.align	16, 0x90
	.type	first_rtl_op,@function
first_rtl_op:                           # @first_rtl_op
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$2, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdi, %rcx
	cmpq	$117, %rdx
	jg	.LBB63_4
# BB#1:                                 # %entry
	cmpq	$54, %rcx
	jne	.LBB63_2
# BB#8:                                 # %sw.bb3
	movq	$3, %rax
	jmp	.LBB63_9
.LBB63_4:                               # %entry
	cmpq	$118, %rcx
	je	.LBB63_9
# BB#5:                                 # %entry
	cmpq	$120, %rcx
	je	.LBB63_7
# BB#6:                                 # %entry
	cmpq	$136, %rcx
	jne	.LBB63_3
.LBB63_7:                               # %sw.bb1
	xorq	%rax, %rax
	retq
.LBB63_2:                               # %entry
	cmpq	$55, %rcx
	jne	.LBB63_3
.LBB63_9:                               # %return
	retq
.LBB63_3:                               # %sw.default
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movl	tree_code_length(,%rdi,4), %eax
	retq
.Ltmp432:
	.size	first_rtl_op, .Ltmp432-first_rtl_op
	.cfi_endproc

	.globl	unsave_expr_1
	.align	16, 0x90
	.type	unsave_expr_1,@function
unsave_expr_1:                          # @unsave_expr_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp434:
	.cfi_def_cfa_offset 16
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$50, %rcx
	je	.LBB64_5
# BB#1:                                 # %entry
	cmpq	$120, %rcx
	jne	.LBB64_2
# BB#7:                                 # %sw.bb20
	cmpq	$0, 32(%rdi)
	je	.LBB64_10
# BB#8:                                 # %if.then24
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.unsave_expr_1, %rcx
	movq	$1712, %rsi             # imm = 0x6B0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB64_5:                               # %sw.bb4
	cmpq	$0, 40(%rdi)
	jne	.LBB64_10
# BB#6:                                 # %if.end10
	movq	56(%rdi), %rax
	movq	%rax, 40(%rdi)
	movq	$0, 56(%rdi)
	popq	%rax
	retq
.LBB64_2:                               # %entry
	cmpq	$118, %rcx
	jne	.LBB64_9
# BB#3:                                 # %sw.bb
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB64_10
# BB#4:                                 # %if.then
	movq	$0, 48(%rdi)
	popq	%rax
	retq
.LBB64_9:                               # %sw.default
	movq	lang_unsave_expr_now(%rip), %rax
	testq	%rax, %rax
	je	.LBB64_10
# BB#11:                                # %if.then27
	popq	%rdx
	jmpq	*%rax  # TAILCALL
.LBB64_10:                              # %sw.epilog
	popq	%rax
	retq
.Ltmp435:
	.size	unsave_expr_1, .Ltmp435-unsave_expr_1
	.cfi_endproc

	.globl	unsave_expr_now
	.align	16, 0x90
	.type	unsave_expr_now,@function
unsave_expr_now:                        # @unsave_expr_now
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp437:
	.cfi_def_cfa_offset 16
	movq	lang_unsave(%rip), %rax
	movq	%rdi, (%rsp)
	testq	%rax, %rax
	je	.LBB65_2
# BB#1:                                 # %if.then
	leaq	(%rsp), %rdi
	callq	*%rax
	jmp	.LBB65_3
.LBB65_2:                               # %if.else
	callq	unsave_expr_now_r
.LBB65_3:                               # %if.end
	movq	(%rsp), %rax
	popq	%rdx
	retq
.Ltmp438:
	.size	unsave_expr_now, .Ltmp438-unsave_expr_now
	.cfi_endproc

	.align	16, 0x90
	.type	unsave_expr_now_r,@function
unsave_expr_now_r:                      # @unsave_expr_now_r
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp445:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp446:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp447:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp448:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp449:
	.cfi_def_cfa_offset 48
.Ltmp450:
	.cfi_offset %rbx, -48
.Ltmp451:
	.cfi_offset %r12, -40
.Ltmp452:
	.cfi_offset %r13, -32
.Ltmp453:
	.cfi_offset %r14, -24
.Ltmp454:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	je	.LBB66_30
# BB#1:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movabsq	$54045394551701505, %r15 # imm = 0xC0020000000001
	movabsq	$72059518183276544, %r12 # imm = 0x10001C000000000
.LBB66_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r13), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$50, %rcx
	je	.LBB66_7
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$120, %rcx
	jne	.LBB66_4
# BB#9:                                 # %sw.bb20.i
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$0, 32(%r13)
	je	.LBB66_13
	jmp	.LBB66_10
	.align	16, 0x90
.LBB66_7:                               # %sw.bb4.i
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$0, 40(%r13)
	jne	.LBB66_13
# BB#8:                                 # %if.end10.i
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	56(%r13), %rcx
	movq	%rcx, 40(%r13)
	movq	$0, 56(%r13)
	jmp	.LBB66_13
	.align	16, 0x90
.LBB66_4:                               # %if.end
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$118, %rcx
	jne	.LBB66_11
# BB#5:                                 # %sw.bb.i
                                        #   in Loop: Header=BB66_2 Depth=1
	testq	$16384, %rax            # imm = 0x4000
	jne	.LBB66_13
# BB#6:                                 # %if.then.i
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	$0, 48(%r13)
	jmp	.LBB66_13
.LBB66_11:                              # %sw.default.i
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	lang_unsave_expr_now(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB66_13
# BB#12:                                # %if.then27.i
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	%r13, %rdi
	callq	*%rcx
	movl	16(%r13), %eax
	.align	16, 0x90
.LBB66_13:                              # %unsave_expr_1.exit
                                        #   in Loop: Header=BB66_2 Depth=1
	andq	$255, %rax
	movsbq	tree_code_type(%rax), %rcx
	cmpq	$59, %rcx
	jle	.LBB66_14
# BB#18:                                # %unsave_expr_1.exit
                                        #   in Loop: Header=BB66_2 Depth=1
	addq	$-60, %rcx
	andq	%r14, %rcx
	cmpq	$60, %rcx
	ja	.LBB66_32
# BB#19:                                # %unsave_expr_1.exit
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r15, %rdx
	jne	.LBB66_15
# BB#20:                                # %unsave_expr_1.exit
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%r12, %rdx
	jne	.LBB66_30
# BB#21:                                # %unsave_expr_1.exit
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$60, %rcx
	jne	.LBB66_32
# BB#22:                                # %sw.bb2
                                        #   in Loop: Header=BB66_2 Depth=1
	cmpq	$2, %rax
	jne	.LBB66_30
# BB#23:                                # %if.then5
                                        #   in Loop: Header=BB66_2 Depth=1
	movq	32(%r13), %rdi
	callq	unsave_expr_now_r
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB66_2
	jmp	.LBB66_30
.LBB66_14:                              # %unsave_expr_1.exit
	addq	$-49, %rcx
	andq	%r14, %rcx
	cmpq	$2, %rcx
	jae	.LBB66_32
.LBB66_15:                              # %sw.bb8
	movq	$2, %rbx
	cmpq	$117, %rax
	jg	.LBB66_24
# BB#16:                                # %sw.bb8
	cmpq	$54, %rax
	jne	.LBB66_17
# BB#31:                                # %sw.bb3.i
	movq	$3, %rbx
	jmp	.LBB66_28
.LBB66_24:                              # %sw.bb8
	cmpq	$118, %rax
	je	.LBB66_28
# BB#25:                                # %sw.bb8
	cmpq	$120, %rax
	je	.LBB66_30
# BB#26:                                # %sw.bb8
	cmpq	$136, %rax
	je	.LBB66_30
	jmp	.LBB66_27
.LBB66_17:                              # %sw.bb8
	cmpq	$55, %rax
	je	.LBB66_28
.LBB66_27:                              # %for.cond.preheader
	movslq	tree_code_length(,%rax,4), %rbx
	testq	%rbx, %rbx
	jle	.LBB66_30
.LBB66_28:                              # %for.body.lr.ph
	decq	%rbx
	.align	16, 0x90
.LBB66_29:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13,%rbx,8), %rdi
	callq	unsave_expr_now_r
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB66_29
.LBB66_30:                              # %sw.epilog
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB66_32:                              # %sw.default
	movabsq	$.L.str147, %rdi
	movabsq	$.L__FUNCTION__.unsave_expr_now_r, %rdx
	movq	$1768, %rsi             # imm = 0x6E8
	callq	fancy_abort
.LBB66_10:                              # %if.then24.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.unsave_expr_1, %rcx
	movq	$1712, %rsi             # imm = 0x6B0
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp455:
	.size	unsave_expr_now_r, .Ltmp455-unsave_expr_now_r
	.cfi_endproc

	.globl	unsafe_for_reeval
	.align	16, 0x90
	.type	unsafe_for_reeval,@function
unsafe_for_reeval:                      # @unsafe_for_reeval
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp462:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp463:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp464:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp465:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp466:
	.cfi_def_cfa_offset 48
.Ltmp467:
	.cfi_offset %rbx, -48
.Ltmp468:
	.cfi_offset %r12, -40
.Ltmp469:
	.cfi_offset %r13, -32
.Ltmp470:
	.cfi_offset %r14, -24
.Ltmp471:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	testq	%r13, %r13
	movq	$1, %rax
	je	.LBB67_28
# BB#1:                                 # %if.end
	movzbl	16(%r13), %r12d
	cmpq	$117, %r12
	movq	$2, %rcx
	jg	.LBB67_5
# BB#2:                                 # %if.end
	movq	$3, %r14
	cmpq	$54, %r12
	je	.LBB67_20
# BB#3:                                 # %if.end
	cmpq	$55, %r12
	je	.LBB67_9
	jmp	.LBB67_4
.LBB67_5:                               # %if.end
	cmpq	$118, %r12
	je	.LBB67_9
# BB#6:                                 # %if.end
	cmpq	$120, %r12
	je	.LBB67_8
# BB#7:                                 # %if.end
	cmpq	$136, %r12
	jne	.LBB67_4
.LBB67_8:                               # %sw.bb1.i
	xorq	%rcx, %rcx
	jmp	.LBB67_9
.LBB67_4:                               # %sw.default.i
	movl	tree_code_length(,%r12,4), %ecx
.LBB67_9:                               # %first_rtl_op.exit
	cmpq	$49, %r12
	jg	.LBB67_15
# BB#10:                                # %first_rtl_op.exit
	cmpq	$2, %r12
	movq	%rcx, %r14
	jne	.LBB67_20
# BB#11:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB67_12:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rdi
	callq	unsafe_for_reeval
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB67_14
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB67_12 Depth=1
	movq	%rbx, %rax
.LBB67_14:                              # %for.body
                                        #   in Loop: Header=BB67_12 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%rax, %rbx
	jne	.LBB67_12
	jmp	.LBB67_28
.LBB67_15:                              # %first_rtl_op.exit
	cmpq	$117, %r12
	jg	.LBB67_18
# BB#16:                                # %first_rtl_op.exit
	movq	$1, %r15
	cmpq	$50, %r12
	jne	.LBB67_25
# BB#17:
	movq	%rcx, %r14
	jmp	.LBB67_22
.LBB67_18:                              # %first_rtl_op.exit
	movq	$2, %rax
	cmpq	$118, %r12
	je	.LBB67_28
# BB#19:                                # %first_rtl_op.exit
	cmpq	$120, %r12
	movq	%rcx, %r14
	je	.LBB67_28
	jmp	.LBB67_20
.LBB67_25:                              # %first_rtl_op.exit
	cmpq	$53, %r12
	movq	%rcx, %r14
	jne	.LBB67_20
# BB#26:                                # %sw.bb9
	movq	40(%r13), %rdi
	callq	unsafe_for_reeval
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB67_28
# BB#27:                                # %sw.bb9
	movq	$1, %rax
	jmp	.LBB67_28
.LBB67_20:                              # %sw.default
	movq	lang_unsafe_for_reeval(%rip), %rcx
	xorq	%r15, %r15
	testq	%rcx, %rcx
	je	.LBB67_22
# BB#21:                                # %if.then19
	movq	%r13, %rdi
	callq	*%rcx
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB67_28
.LBB67_22:                              # %sw.epilog
	movsbq	tree_code_type(%r12), %rax
	cmpq	$59, %rax
	jg	.LBB67_29
# BB#23:                                # %sw.epilog
	addq	$-49, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB67_32
	jmp	.LBB67_24
.LBB67_29:                              # %sw.epilog
	addq	$-60, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$60, %rcx
	ja	.LBB67_24
# BB#30:                                # %sw.epilog
	xorq	%rax, %rax
	movq	$1, %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	movabsq	$1224981022790123520, %rdi # imm = 0x110001C000000000
	testq	%rdi, %rsi
	jne	.LBB67_28
# BB#31:                                # %sw.epilog
	shlq	%cl, %rdx
	movabsq	$54045394551701505, %rax # imm = 0xC0020000000001
	testq	%rax, %rdx
	je	.LBB67_24
.LBB67_32:                              # %for.cond28.preheader
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB67_33
# BB#34:                                # %for.body31.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	decq	%r14
	.align	16, 0x90
.LBB67_35:                              # %for.body31
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13,%r14,8), %rdi
	callq	unsafe_for_reeval
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%r15, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jge	.LBB67_37
# BB#36:                                # %for.body31
                                        #   in Loop: Header=BB67_35 Depth=1
	movq	%r15, %rax
.LBB67_37:                              # %for.body31
                                        #   in Loop: Header=BB67_35 Depth=1
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	decq	%r14
	testq	%rdx, %rdx
	movq	%rax, %r15
	jg	.LBB67_35
	jmp	.LBB67_28
.LBB67_24:                              # %sw.default45
	movq	$2, %rax
	jmp	.LBB67_28
.LBB67_33:
	movq	%r15, %rax
.LBB67_28:                              # %sw.bb9
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp472:
	.size	unsafe_for_reeval, .Ltmp472-unsafe_for_reeval
	.cfi_endproc

	.globl	has_cleanups
	.align	16, 0x90
	.type	has_cleanups,@function
has_cleanups:                           # @has_cleanups
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp479:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp480:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp481:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp482:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp483:
	.cfi_def_cfa_offset 48
.Ltmp484:
	.cfi_offset %rbx, -48
.Ltmp485:
	.cfi_offset %r12, -40
.Ltmp486:
	.cfi_offset %r13, -32
.Ltmp487:
	.cfi_offset %r14, -24
.Ltmp488:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	16(%r15), %ecx
	testq	$256, %rcx              # imm = 0x100
	je	.LBB68_27
# BB#1:                                 # %if.end
	andq	$255, %rcx
	movq	$1, %rax
	cmpq	$135, %rcx
	jg	.LBB68_8
# BB#2:                                 # %if.end
	leaq	-50(%rcx), %rdx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rdx
	cmpq	$6, %rdx
	ja	.LBB68_9
# BB#3:                                 # %if.end
	jmpq	*.LJTI68_0(,%rdx,8)
.LBB68_4:                               # %sw.bb5
	movq	40(%r15), %rbx
	jmp	.LBB68_6
	.align	16, 0x90
.LBB68_5:                               # %for.cond
                                        #   in Loop: Header=BB68_6 Depth=1
	movq	(%rbx), %rbx
.LBB68_6:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB68_27
# BB#7:                                 # %for.body
                                        #   in Loop: Header=BB68_6 Depth=1
	movq	32(%rbx), %rdi
	callq	has_cleanups
	testq	%r14, %rax
	je	.LBB68_5
	jmp	.LBB68_28
.LBB68_8:                               # %if.end
	cmpq	$136, %rcx
	je	.LBB68_28
.LBB68_9:                               # %sw.epilog
	cmpq	$146, %rcx
	jbe	.LBB68_11
# BB#10:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB68_28
.LBB68_11:                              # %if.end18
	movq	$2, %r14
	cmpq	$117, %rcx
	jg	.LBB68_14
# BB#12:                                # %if.end18
	cmpq	$54, %rcx
	jne	.LBB68_13
# BB#18:                                # %sw.bb3.i
	movq	$3, %r14
	jmp	.LBB68_19
.LBB68_14:                              # %if.end18
	cmpq	$118, %rcx
	je	.LBB68_19
# BB#15:                                # %if.end18
	cmpq	$120, %rcx
	je	.LBB68_27
# BB#16:                                # %if.end18
	cmpq	$136, %rcx
	je	.LBB68_27
	jmp	.LBB68_17
.LBB68_13:                              # %if.end18
	cmpq	$55, %rcx
	je	.LBB68_19
.LBB68_17:                              # %first_rtl_op.exit
	movslq	tree_code_length(,%rcx,4), %r14
	testq	%r14, %r14
	jle	.LBB68_27
.LBB68_19:                              # %for.body26.lr.ph
	movq	$4, %rbx
	movabsq	$4503599627372547, %r12 # imm = 0x10000000000803
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB68_20:                              # %for.body26
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r15,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB68_26
# BB#21:                                # %if.then31
                                        #   in Loop: Header=BB68_20 Depth=1
	movzbl	16(%rdi), %eax
	movsbq	tree_code_type(%rax), %rcx
	cmpq	$113, %rcx
	jg	.LBB68_24
# BB#22:                                # %if.then31
                                        #   in Loop: Header=BB68_20 Depth=1
	addq	$-49, %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB68_26
# BB#23:                                # %if.then31
                                        #   in Loop: Header=BB68_20 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%r12, %rax
	jne	.LBB68_25
	jmp	.LBB68_26
	.align	16, 0x90
.LBB68_24:                              # %if.then31
                                        #   in Loop: Header=BB68_20 Depth=1
	addq	$-114, %rcx
	andq	$255, %rcx
	cmpq	$1, %rcx
	ja	.LBB68_26
.LBB68_25:                              # %if.then58
                                        #   in Loop: Header=BB68_20 Depth=1
	callq	has_cleanups
	testq	%r13, %rax
	jne	.LBB68_28
.LBB68_26:                              # %for.inc69
                                        #   in Loop: Header=BB68_20 Depth=1
	leaq	-3(%rbx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	incq	%rbx
	cmpq	%r14, %rax
	jl	.LBB68_20
.LBB68_27:
	xorq	%rax, %rax
.LBB68_28:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp489:
	.size	has_cleanups, .Ltmp489-has_cleanups
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI68_0:
	.quad	.LBB68_28
	.quad	.LBB68_9
	.quad	.LBB68_9
	.quad	.LBB68_4
	.quad	.LBB68_9
	.quad	.LBB68_28
	.quad	.LBB68_27

	.text
	.globl	substitute_in_expr
	.align	16, 0x90
	.type	substitute_in_expr,@function
substitute_in_expr:                     # @substitute_in_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp496:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp497:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp498:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp499:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp500:
	.cfi_def_cfa_offset 48
.Ltmp501:
	.cfi_offset %rbx, -48
.Ltmp502:
	.cfi_offset %r12, -40
.Ltmp503:
	.cfi_offset %r13, -32
.Ltmp504:
	.cfi_offset %r14, -24
.Ltmp505:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	16(%r15), %ebx
	movsbq	tree_code_type(%rbx), %rax
	movq	%rdx, %r14
	movq	%rsi, %r13
	cmpq	$59, %rax
	jg	.LBB69_2
# BB#1:                                 # %entry
	addq	$-49, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	jb	.LBB69_8
	jmp	.LBB69_4
.LBB69_2:                               # %entry
	cmpq	$98, %rax
	jg	.LBB69_5
# BB#3:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$60, %rax
	jne	.LBB69_4
.LBB69_8:                               # %sw.bb26
	movl	tree_code_length(,%rbx,4), %eax
	cmpq	$3, %rax
	je	.LBB69_55
# BB#9:                                 # %sw.bb26
	cmpq	$2, %rax
	jne	.LBB69_10
# BB#48:                                # %sw.bb47
	cmpq	$120, %rbx
	je	.LBB69_23
# BB#49:                                # %sw.bb47
	cmpq	$46, %rbx
	je	.LBB69_50
# BB#51:                                # %if.end56
	movq	32(%r15), %rax
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	40(%r15), %rcx
	movq	%rax, %r12
	movq	%rcx, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rdx
	movq	%rax, %rcx
	cmpq	%rdx, %r12
	jne	.LBB69_53
# BB#52:                                # %land.lhs.true70
	movq	40(%r15), %rax
	cmpq	%rax, %rcx
	je	.LBB69_23
.LBB69_53:                              # %if.end77
	movq	8(%r15), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%r12, %rdx
	callq	build
	jmp	.LBB69_47
.LBB69_55:                              # %sw.bb82
	cmpq	$51, %rbx
	je	.LBB69_61
# BB#56:                                # %sw.bb82
	cmpq	$118, %rbx
	je	.LBB69_23
# BB#57:                                # %sw.bb82
	cmpq	$53, %rbx
	jne	.LBB69_60
# BB#58:                                # %if.then89
	movq	40(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	40(%r15), %rdx
	movq	%rax, %rcx
	cmpq	%rdx, %rcx
	je	.LBB69_23
# BB#59:                                # %if.end100
	movq	8(%r15), %rsi
	movq	32(%r15), %rdx
	movq	$53, %rdi
	xorq	%r8, %r8
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	build  # TAILCALL
.LBB69_10:                              # %sw.bb26
	cmpq	$1, %rax
	jne	.LBB69_66
# BB#11:                                # %sw.bb29
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB69_23
# BB#12:                                # %if.end39
	cmpq	$116, %rbx
	jne	.LBB69_26
# BB#13:
	movq	%r14, %r15
	jmp	.LBB69_23
.LBB69_5:                               # %entry
	cmpq	$113, %rax
	jg	.LBB69_14
# BB#6:                                 # %entry
	leaq	-99(%rax), %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB69_23
# BB#7:                                 # %entry
	andq	%rcx, %rax
	cmpq	$101, %rax
	je	.LBB69_8
	jmp	.LBB69_4
.LBB69_14:                              # %entry
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	cmpq	$114, %rax
	jne	.LBB69_15
# BB#67:                                # %sw.bb149
	leaq	-41(%rbx), %rax
	andq	%r12, %rax
	cmpq	$2, %rax
	jae	.LBB69_68
# BB#84:                                # %sw.bb247
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rcx
	movq	%rax, %r14
	cmpq	%rcx, %r14
	je	.LBB69_23
# BB#85:                                # %if.end258
	movq	8(%r15), %r13
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%ebx, 16(%rax)
	movq	%r13, 8(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 24(%rax)
	movq	%r14, 32(%rax)
	testq	%r14, %r14
	movq	%rbx, %rdx
	je	.LBB69_93
# BB#86:                                # %land.lhs.true.i
	cmpq	$117, %rbx
	jg	.LBB69_88
# BB#87:                                # %land.lhs.true.i
	leaq	-54(%rbx), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB69_92
	jmp	.LBB69_91
.LBB69_61:                              # %if.end113
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	40(%r15), %rdi
	movq	%rax, %r12
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	48(%r15), %rdi
	movq	%rax, %rbx
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rcx
	movq	%rax, %r8
	cmpq	%rcx, %r12
	movq	%rbx, %rcx
	jne	.LBB69_64
# BB#62:                                # %land.lhs.true131
	movq	40(%r15), %rax
	cmpq	%rax, %rcx
	jne	.LBB69_64
# BB#63:                                # %land.lhs.true137
	movq	48(%r15), %rax
	cmpq	%rax, %r8
	je	.LBB69_23
.LBB69_64:                              # %if.end144
	movq	8(%r15), %rsi
	movq	$51, %rdi
.LBB69_65:                              # %sw.epilog266
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	build
	jmp	.LBB69_47
.LBB69_26:                              # %if.end43
	movq	8(%r15), %r12
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%ebx, 16(%rax)
	movq	%r12, 8(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 24(%rax)
	movq	%r14, 32(%rax)
	testq	%r14, %r14
	movq	%rbx, %rdx
	je	.LBB69_34
# BB#27:                                # %land.lhs.true.i414
	cmpq	$117, %rbx
	jg	.LBB69_29
# BB#28:                                # %land.lhs.true.i414
	leaq	-54(%rbx), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB69_33
	jmp	.LBB69_32
.LBB69_15:                              # %entry
	cmpq	$120, %rax
	jne	.LBB69_4
# BB#16:                                # %sw.bb2
	cmpq	$57, %rbx
	je	.LBB69_23
# BB#17:                                # %sw.bb2
	cmpq	$2, %rbx
	jne	.LBB69_24
# BB#18:                                # %if.then6
	movq	(%r15), %rdi
	xorq	%r12, %r12
	testq	%rdi, %rdi
	je	.LBB69_20
# BB#19:                                # %cond.false
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	%rax, %r12
.LBB69_20:                              # %cond.end
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	(%r15), %rcx
	movq	%rax, %rbx
	cmpq	%rcx, %r12
	jne	.LBB69_22
# BB#21:                                # %land.lhs.true
	movq	32(%r15), %rax
	cmpq	%rax, %rbx
	je	.LBB69_23
.LBB69_22:                              # %if.end
	movq	24(%r15), %r14
	movq	$40, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	movq	$0, 16(%r15)
	movq	$0, 8(%r15)
	movq	$0, (%r15)
	movq	$2, %rax
	movl	%eax, 16(%r15)
	movq	%r12, (%r15)
	movq	%r14, 24(%r15)
	movq	%rbx, 32(%r15)
	jmp	.LBB69_23
.LBB69_68:                              # %sw.bb149
	cmpq	$40, %rbx
	jne	.LBB69_69
# BB#80:                                # %sw.bb211
	movq	32(%r15), %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	40(%r15), %rdi
	movq	%rax, %r12
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	48(%r15), %rdi
	movq	%rax, %rbx
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rcx
	movq	%rax, %r8
	cmpq	%rcx, %r12
	movq	%rbx, %rcx
	jne	.LBB69_83
# BB#81:                                # %land.lhs.true229
	movq	40(%r15), %rax
	cmpq	%rax, %rcx
	jne	.LBB69_83
# BB#82:                                # %land.lhs.true235
	movq	48(%r15), %rax
	cmpq	%rax, %r8
	je	.LBB69_23
.LBB69_83:                              # %if.end242
	movq	8(%r15), %rsi
	movq	$40, %rdi
	jmp	.LBB69_65
.LBB69_29:                              # %land.lhs.true.i414
	cmpq	$118, %rbx
	je	.LBB69_33
# BB#30:                                # %land.lhs.true.i414
	movq	$120, %rdx
	cmpq	$120, %rbx
	je	.LBB69_41
# BB#31:                                # %land.lhs.true.i414
	movq	$136, %rdx
	cmpq	$136, %rbx
	je	.LBB69_42
.LBB69_32:                              # %first_rtl_op.exit.i418
	movl	tree_code_length(,%rbx,4), %ecx
	testq	%rcx, %rcx
	movq	%rbx, %rdx
	je	.LBB69_34
.LBB69_33:                              # %if.then.i426
	movl	16(%r14), %ecx
	andq	$256, %rcx              # imm = 0x100
	orq	%rbx, %rcx
	movl	%ecx, 16(%rax)
	movl	16(%r14), %edx
	andq	$4096, %rdx             # imm = 0x1000
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB69_34:                              # %if.end.i428
	cmpq	$119, %rbx
	jg	.LBB69_38
# BB#35:                                # %if.end.i428
	leaq	-48(%rbx), %rcx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$2, %rsi
	jb	.LBB69_41
	jmp	.LBB69_36
.LBB69_38:                              # %if.end.i428
	leaq	-120(%rbx), %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rsi, %rcx
	jmp	.LBB69_39
.LBB69_69:                              # %sw.bb149
	cmpq	$39, %rbx
	jne	.LBB69_96
# BB#70:                                # %sw.bb150
	movq	32(%r15), %rdi
	movl	16(%rdi), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %eax
	cmpq	$114, %rax
	movq	%rdi, %rax
	jne	.LBB69_73
# BB#71:
	movq	%rdi, %rax
	.align	16, 0x90
.LBB69_72:                              # %for.inc
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rax
	movl	16(%rax), %ecx
	movq	%rcx, %rdx
	andq	$255, %rdx
	movzbl	tree_code_type(%rdx), %esi
	cmpq	$114, %rsi
	je	.LBB69_72
.LBB69_73:                              # %for.end
	cmpq	$57, %rdx
	jne	.LBB69_76
# BB#74:                                # %land.lhs.true172
	movq	40(%r15), %rdx
	cmpq	%r13, %rdx
	je	.LBB69_75
.LBB69_76:                              # %if.end179
	andq	$255, %rcx
	cmpq	$57, %rcx
	jne	.LBB69_78
# BB#77:                                # %land.lhs.true186
	cmpq	$0, 8(%rax)
	je	.LBB69_23
.LBB69_78:                              # %if.end192
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	substitute_in_expr
	movq	32(%r15), %rcx
	movq	%rax, %rdx
	cmpq	%rcx, %rdx
	je	.LBB69_23
# BB#79:                                # %if.end203
	movq	8(%r15), %rsi
	movq	40(%r15), %rcx
	movq	$39, %rdi
	xorq	%rax, %rax
	callq	build
	jmp	.LBB69_47
.LBB69_88:                              # %land.lhs.true.i
	cmpq	$118, %rbx
	je	.LBB69_92
# BB#89:                                # %land.lhs.true.i
	movq	$120, %rdx
	cmpq	$120, %rbx
	je	.LBB69_41
# BB#90:                                # %land.lhs.true.i
	movq	$136, %rdx
	cmpq	$136, %rbx
	je	.LBB69_42
.LBB69_91:                              # %first_rtl_op.exit.i
	movl	tree_code_length(,%rbx,4), %ecx
	testq	%rcx, %rcx
	movq	%rbx, %rdx
	je	.LBB69_93
.LBB69_92:                              # %if.then.i
	movl	16(%r14), %ecx
	andq	$256, %rcx              # imm = 0x100
	orq	%rbx, %rcx
	movl	%ecx, 16(%rax)
	movl	16(%r14), %edx
	andq	$4096, %rdx             # imm = 0x1000
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB69_93:                              # %if.end.i
	cmpq	$119, %rbx
	jg	.LBB69_95
# BB#94:                                # %if.end.i
	leaq	-48(%rbx), %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	jb	.LBB69_41
.LBB69_36:                              # %if.end.i428
	cmpq	$41, %rbx
	jne	.LBB69_42
# BB#37:                                # %sw.bb36.i434
	movabsq	$4294963199, %rcx       # imm = 0xFFFFEFFF
	andq	%rcx, %rdx
	jmp	.LBB69_46
.LBB69_95:                              # %if.end.i
	leaq	-120(%rbx), %rcx
	andq	%r12, %rcx
.LBB69_39:                              # %if.end.i428
	cmpq	$13, %rcx
	ja	.LBB69_42
# BB#40:                                # %if.end.i428
	movq	$1, %rsi
	shlq	%cl, %rsi
	testq	$15873, %rsi            # imm = 0x3E01
	je	.LBB69_42
.LBB69_41:                              # %sw.bb.i432
	movabsq	$4294962943, %rcx       # imm = 0xFFFFEEFF
	andq	%rdx, %rcx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	jmp	.LBB69_47
.LBB69_42:                              # %sw.default.i440
	movzbl	tree_code_type(%rbx), %ecx
	cmpq	$49, %rcx
	jne	.LBB69_47
# BB#43:                                # %sw.default.i440
	testq	%r14, %r14
	je	.LBB69_47
# BB#44:                                # %land.lhs.true47.i445
	movzbl	17(%r14), %ecx
	testq	$2, %rcx
	je	.LBB69_47
# BB#45:                                # %if.then53.i447
	orq	$512, %rdx              # imm = 0x200
.LBB69_46:                              # %build1.exit448
	movl	%edx, 16(%rax)
.LBB69_47:                              # %build1.exit448
	movq	%rax, %rdi
	callq	fold
	movl	16(%r15), %ecx
	movl	16(%rax), %edx
	andq	$4096, %rcx             # imm = 0x1000
	movabsq	$4294963199, %rsi       # imm = 0xFFFFEFFF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r15
.LBB69_23:                              # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB69_75:
	movq	%r14, %r15
	jmp	.LBB69_23
.LBB69_50:                              # %if.then54
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2094, %rsi             # imm = 0x82E
	jmp	.LBB69_25
.LBB69_60:                              # %if.then110
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2121, %rsi             # imm = 0x849
	jmp	.LBB69_25
.LBB69_66:                              # %sw.default
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2134, %rsi             # imm = 0x856
	jmp	.LBB69_25
.LBB69_4:                               # %sw.default265
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2193, %rsi             # imm = 0x891
	jmp	.LBB69_25
.LBB69_24:                              # %if.end25
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2069, %rsi             # imm = 0x815
	jmp	.LBB69_25
.LBB69_96:                              # %sw.default263
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.substitute_in_expr, %rcx
	movq	$2188, %rsi             # imm = 0x88C
.LBB69_25:                              # %if.end25
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp506:
	.size	substitute_in_expr, .Ltmp506-substitute_in_expr
	.cfi_endproc

	.globl	stabilize_reference
	.align	16, 0x90
	.type	stabilize_reference,@function
stabilize_reference:                    # @stabilize_reference
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp512:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp513:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp514:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp515:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp516:
	.cfi_def_cfa_offset 48
.Ltmp517:
	.cfi_offset %rbx, -40
.Ltmp518:
	.cfi_offset %r12, -32
.Ltmp519:
	.cfi_offset %r14, -24
.Ltmp520:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	16(%r14), %ebx
	cmpq	$38, %rbx
	jg	.LBB70_3
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	jne	.LBB70_33
# BB#2:                                 # %sw.bb63
	movq	global_trees(%rip), %r14
	jmp	.LBB70_33
.LBB70_3:                               # %entry
	cmpq	$113, %rbx
	jg	.LBB70_8
# BB#4:                                 # %entry
	cmpq	$71, %rbx
	jg	.LBB70_7
# BB#5:                                 # %entry
	addq	$-39, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$8, %rax
	ja	.LBB70_33
# BB#6:                                 # %entry
	jmpq	*.LJTI70_0(,%rax,8)
.LBB70_30:                              # %sw.bb10
	movq	32(%r14), %rdi
	callq	stabilize_reference
	movq	40(%r14), %rdx
	movq	%rax, %rcx
	movq	$39, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	jmp	.LBB70_31
.LBB70_8:                               # %entry
	cmpq	$120, %rbx
	je	.LBB70_18
# BB#9:                                 # %entry
	leaq	-114(%rbx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jbe	.LBB70_10
	jmp	.LBB70_33
.LBB70_7:                               # %entry
	leaq	-72(%rbx), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$5, %rcx
	jae	.LBB70_33
.LBB70_10:                              # %sw.bb2
	movq	32(%r14), %rdi
	callq	stabilize_reference
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
.LBB70_11:                              # %sw.epilog
	movq	%rcx, %rsi
	callq	build_nt
	jmp	.LBB70_32
.LBB70_18:                              # %sw.bb55
	movq	8(%r14), %r15
	movq	72(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB70_20
# BB#19:                                # %if.end.i144
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r15, 8(%rbx)
	movq	%rbx, 72(%r15)
	movq	%rbx, %rdi
	callq	layout_type
.LBB70_20:                              # %build_pointer_type.exit
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$121, %rcx
	movl	%ecx, 16(%rax)
	movq	%rbx, 8(%rax)
	xorq	%r12, %r12
	movl	%r12d, 24(%rax)
	movq	%r14, 32(%rax)
	testq	%r14, %r14
	je	.LBB70_26
# BB#21:                                # %land.lhs.true.i130
	movl	tree_code_length+484(%rip), %ecx
	testq	%rcx, %rcx
	movq	$633, %rcx              # imm = 0x279
	je	.LBB70_23
# BB#22:                                # %if.then.i139
	movl	16(%r14), %edx
	andq	$256, %rdx              # imm = 0x100
	orq	$121, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$4096, %rcx             # imm = 0x1000
	orq	%rdx, %rcx
	movl	%ecx, 16(%rax)
	orq	$512, %rcx              # imm = 0x200
.LBB70_23:                              # %if.end.i
	movzbl	tree_code_type+121(%rip), %edx
	cmpq	$49, %rdx
	jne	.LBB70_26
# BB#24:                                # %land.lhs.true47.i
	movzbl	17(%r14), %edx
	testq	$2, %rdx
	je	.LBB70_26
# BB#25:                                # %if.then53.i
	movl	%ecx, 16(%rax)
.LBB70_26:                              # %build1.exit141
	movq	%rax, %rdi
	callq	save_expr
	movq	%rax, %rbx
	movq	$40, %rdi
	callq	ggc_alloc
	testq	%rbx, %rbx
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$41, %rcx
	movl	%ecx, 16(%rax)
	movq	%r15, 8(%rax)
	movl	%r12d, 24(%rax)
	movq	%rbx, 32(%rax)
	je	.LBB70_29
# BB#27:                                # %build1.exit141
	movl	tree_code_length+164(%rip), %edx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rdx
	je	.LBB70_29
# BB#28:                                # %if.then.i
	movl	16(%rbx), %ecx
	andq	$256, %rcx              # imm = 0x100
	orq	$41, %rcx
	movl	%ecx, 16(%rax)
	movl	16(%rbx), %edx
	andq	$4096, %rdx             # imm = 0x1000
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
.LBB70_29:                              # %build1.exit
	movl	%ecx, 16(%rax)
	jmp	.LBB70_32
.LBB70_13:                              # %sw.bb19
	movq	32(%r14), %rdi
	callq	stabilize_reference
	movq	40(%r14), %rdi
	movq	%rax, %r15
	callq	stabilize_reference_1
	movq	48(%r14), %rdi
	movq	%rax, %rbx
	callq	stabilize_reference_1
	movq	%rax, %rcx
	movq	$40, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	build_nt
	jmp	.LBB70_32
.LBB70_12:                              # %sw.bb4
	movq	32(%r14), %rdi
	callq	stabilize_reference_1
	movq	%rax, %rcx
	movq	$41, %rdi
	xorq	%rax, %rax
	jmp	.LBB70_11
.LBB70_14:                              # %sw.bb33
	movq	32(%r14), %rdi
	callq	stabilize_reference
	movq	40(%r14), %rdi
	movq	%rax, %rbx
	callq	stabilize_reference_1
	movq	%rax, %rcx
	movq	$43, %rdi
	jmp	.LBB70_15
.LBB70_16:                              # %sw.bb43
	movq	32(%r14), %rdi
	callq	stabilize_reference
	movq	40(%r14), %rdi
	movq	%rax, %rbx
	callq	stabilize_reference_1
	movq	%rax, %rcx
	movq	$44, %rdi
.LBB70_15:                              # %sw.epilog
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
.LBB70_31:                              # %sw.epilog
	callq	build_nt
.LBB70_32:                              # %sw.epilog
	movq	8(%r14), %rcx
	movq	%rcx, 8(%rax)
	movl	16(%r14), %ecx
	movl	16(%rax), %edx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$-4097, %rdx            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$256, %rcx              # imm = 0x100
	andq	$-257, %rdx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$2048, %rcx             # imm = 0x800
	movabsq	$4294965247, %rsi       # imm = 0xFFFFF7FF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r14
.LBB70_33:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB70_17:                              # %sw.bb53
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	stabilize_reference_1  # TAILCALL
.Ltmp521:
	.size	stabilize_reference, .Ltmp521-stabilize_reference
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI70_0:
	.quad	.LBB70_30
	.quad	.LBB70_13
	.quad	.LBB70_12
	.quad	.LBB70_33
	.quad	.LBB70_14
	.quad	.LBB70_16
	.quad	.LBB70_33
	.quad	.LBB70_33
	.quad	.LBB70_17

	.text
	.globl	build_nt
	.align	16, 0x90
	.type	build_nt,@function
build_nt:                               # @build_nt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp524:
	.cfi_def_cfa_offset 16
	subq	$80, %rsp
.Ltmp525:
	.cfi_def_cfa_offset 96
.Ltmp526:
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
	testq	%rdx, %rdx
	jle	.LBB71_6
# BB#1:                                 # %for.body.lr.ph
	movq	$4, %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rdx
	.align	16, 0x90
.LBB71_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	48(%rsp), %edi
	movq	$32, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	$40, %rdi
	ja	.LBB71_4
# BB#3:                                 # %vaarg.in_reg
                                        #   in Loop: Header=BB71_2 Depth=1
	movq	64(%rsp), %rcx
	addq	%rbx, %rcx
	addq	$8, %rdi
	movl	%edi, 48(%rsp)
	jmp	.LBB71_5
	.align	16, 0x90
.LBB71_4:                               # %vaarg.in_mem
                                        #   in Loop: Header=BB71_2 Depth=1
	movq	56(%rsp), %rcx
	leaq	8(%rcx), %rdi
	movq	%rdi, 56(%rsp)
.LBB71_5:                               # %vaarg.end
                                        #   in Loop: Header=BB71_2 Depth=1
	movq	(%rcx), %rcx
	movq	%rcx, (%rax,%rsi,8)
	leaq	-3(%rsi), %rcx
	andq	%r8, %rcx
	incq	%rsi
	cmpq	%rdx, %rcx
	jne	.LBB71_2
.LBB71_6:                               # %for.end
	addq	$80, %rsp
	popq	%rbx
	retq
.Ltmp527:
	.size	build_nt, .Ltmp527-build_nt
	.cfi_endproc

	.globl	stabilize_reference_1
	.align	16, 0x90
	.type	stabilize_reference_1,@function
stabilize_reference_1:                  # @stabilize_reference_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp532:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp533:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp534:
	.cfi_def_cfa_offset 32
.Ltmp535:
	.cfi_offset %rbx, -32
.Ltmp536:
	.cfi_offset %r14, -24
.Ltmp537:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	16(%r14), %eax
	testq	$512, %rax              # imm = 0x200
	jne	.LBB72_15
# BB#1:                                 # %entry
	movq	%rax, %rbx
	andq	$255, %rbx
	cmpq	$118, %rbx
	je	.LBB72_15
# BB#2:                                 # %if.end
	movsbq	tree_code_type(%rbx), %rdx
	cmpq	$59, %rdx
	jg	.LBB72_7
# BB#3:                                 # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$49, %rdx
	jne	.LBB72_4
# BB#13:                                # %sw.bb47
	movq	32(%r14), %rdi
	callq	stabilize_reference_1
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rsi
	callq	build_nt
	jmp	.LBB72_14
.LBB72_7:                               # %if.end
	addq	$-60, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$60, %rcx
	ja	.LBB72_10
# BB#8:                                 # %if.end
	movq	$1, %rdx
	shlq	%cl, %rdx
	movabsq	$1279025867586011137, %rsi # imm = 0x11C0034000000001
	testq	%rsi, %rdx
	je	.LBB72_9
# BB#11:                                # %sw.bb
	testq	$256, %rax              # imm = 0x100
	jne	.LBB72_12
	jmp	.LBB72_15
.LBB72_4:                               # %if.end
	cmpq	$50, %rdx
	jne	.LBB72_10
# BB#5:                                 # %sw.bb13
	leaq	-62(%rbx), %rcx
	andq	%rax, %rcx
	cmpq	$7, %rcx
	ja	.LBB72_6
.LBB72_12:                              # %if.then10
	movq	%r14, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	save_expr  # TAILCALL
.LBB72_6:                               # %if.end39
	movq	32(%r14), %rdi
	callq	stabilize_reference_1
	movq	40(%r14), %rdi
	movq	%rax, %r15
	callq	stabilize_reference_1
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rcx, %rdx
	callq	build_nt
.LBB72_14:                              # %sw.epilog
	movq	8(%r14), %rcx
	movq	%rcx, 8(%rax)
	movl	16(%r14), %ecx
	movl	16(%rax), %edx
	andq	$4096, %rcx             # imm = 0x1000
	andq	$-4097, %rdx            # imm = 0xFFFFFFFFFFFFEFFF
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$256, %rcx              # imm = 0x100
	andq	$-257, %rdx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rcx, %rdx
	movl	%edx, 16(%rax)
	movl	16(%r14), %ecx
	andq	$2048, %rcx             # imm = 0x800
	movabsq	$4294965247, %rsi       # imm = 0xFFFFF7FF
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 16(%rax)
	movq	%rax, %r14
.LBB72_15:                              # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB72_9:                               # %if.end
	cmpq	$39, %rcx
	je	.LBB72_15
.LBB72_10:                              # %sw.default
	movabsq	$.L.str147, %rdi
	movabsq	$.L__FUNCTION__.stabilize_reference_1, %rdx
	movq	$2364, %rsi             # imm = 0x93C
	callq	fancy_abort
.Ltmp538:
	.size	stabilize_reference_1, .Ltmp538-stabilize_reference_1
	.cfi_endproc

	.globl	build_pointer_type
	.align	16, 0x90
	.type	build_pointer_type,@function
build_pointer_type:                     # @build_pointer_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp542:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp543:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp544:
	.cfi_def_cfa_offset 32
.Ltmp545:
	.cfi_offset %rbx, -24
.Ltmp546:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB73_2
# BB#1:                                 # %if.end
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r14, 8(%rbx)
	movq	%rbx, 72(%r14)
	movq	%rbx, %rdi
	callq	layout_type
.LBB73_2:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp547:
	.size	build_pointer_type, .Ltmp547-build_pointer_type
	.cfi_endproc

	.globl	build_decl
	.align	16, 0x90
	.type	build_decl,@function
build_decl:                             # @build_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp554:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp555:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp556:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp557:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp558:
	.cfi_def_cfa_offset 48
.Ltmp559:
	.cfi_offset %rbx, -48
.Ltmp560:
	.cfi_offset %r12, -40
.Ltmp561:
	.cfi_offset %r13, -32
.Ltmp562:
	.cfi_offset %r14, -24
.Ltmp563:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rbx, %r13
	andq	%rax, %r13
                                        # kill: RDI<def> RBX<kill>
	callq	make_node
	movq	%rax, %r14
	cmpq	$30, %r13
	movq	%r12, 72(%r14)
	movq	%r15, 8(%r14)
	je	.LBB74_3
# BB#1:                                 # %entry
	addq	$-34, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$2, %rbx
	ja	.LBB74_4
# BB#2:                                 # %if.then
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	layout_decl
	jmp	.LBB74_4
.LBB74_3:                               # %if.then7
	movq	$2, %rax
	movb	%al, 48(%r14)
.LBB74_4:                               # %if.end9
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp564:
	.size	build_decl, .Ltmp564-build_decl
	.cfi_endproc

	.globl	build_block
	.align	16, 0x90
	.type	build_block,@function
build_block:                            # @build_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp570:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp571:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp572:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp573:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp574:
	.cfi_def_cfa_offset 48
.Ltmp575:
	.cfi_offset %rbx, -40
.Ltmp576:
	.cfi_offset %r12, -32
.Ltmp577:
	.cfi_offset %r14, -24
.Ltmp578:
	.cfi_offset %r15, -16
	movq	%r8, %r14
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rdi, %rbx
	movq	$4, %rdi
	callq	make_node
	movq	%rbx, 32(%rax)
	movq	%r12, 40(%rax)
	movq	%r15, 48(%rax)
	movq	%r14, (%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp579:
	.size	build_block, .Ltmp579-build_block
	.cfi_endproc

	.globl	build_expr_wfl
	.align	16, 0x90
	.type	build_expr_wfl,@function
build_expr_wfl:                         # @build_expr_wfl
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
.Ltmp591:
	.cfi_offset %rbx, -48
.Ltmp592:
	.cfi_offset %r12, -40
.Ltmp593:
	.cfi_offset %r13, -32
.Ltmp594:
	.cfi_offset %r14, -24
.Ltmp595:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r13
	movq	%rsi, %r15
	movq	%rdi, %r14
	movq	$144, %rdi
	callq	make_node
	movq	%rax, %r12
	movq	%r14, 32(%r12)
	movq	$12, %rcx
	shlq	%cl, %r13
	andq	$4095, %rbx             # imm = 0xFFF
	orq	%r13, %rbx
	movl	%ebx, 24(%r12)
	movq	build_expr_wfl.last_file(%rip), %rax
	cmpq	%r15, %rax
	je	.LBB76_1
# BB#2:                                 # %if.then
	movq	%r15, build_expr_wfl.last_file(%rip)
	xorq	%rax, %rax
	testq	%r15, %r15
	je	.LBB76_4
# BB#3:                                 # %cond.true
	movq	%r15, %rdi
	callq	get_identifier
.LBB76_4:                               # %cond.end
	movq	%rax, build_expr_wfl.last_filenode(%rip)
	jmp	.LBB76_5
.LBB76_1:                               # %entry.if.end_crit_edge
	movq	build_expr_wfl.last_filenode(%rip), %rax
.LBB76_5:                               # %if.end
	movq	%rax, 40(%r12)
	testq	%r14, %r14
	je	.LBB76_7
# BB#6:                                 # %if.then7
	movl	16(%r14), %eax
	movl	16(%r12), %ecx
	andq	$256, %rax              # imm = 0x100
	movabsq	$4294967039, %rdx       # imm = 0xFFFFFEFF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r12)
	movq	8(%r14), %rax
	movq	%rax, 8(%r12)
.LBB76_7:                               # %if.end15
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp596:
	.size	build_expr_wfl, .Ltmp596-build_expr_wfl
	.cfi_endproc

	.globl	build_decl_attribute_variant
	.align	16, 0x90
	.type	build_decl_attribute_variant,@function
build_decl_attribute_variant:           # @build_decl_attribute_variant
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, 136(%rdi)
	movq	%rdi, %rax
	retq
.Ltmp597:
	.size	build_decl_attribute_variant, .Ltmp597-build_decl_attribute_variant
	.cfi_endproc

	.globl	build_type_attribute_variant
	.align	16, 0x90
	.type	build_type_attribute_variant,@function
build_type_attribute_variant:           # @build_type_attribute_variant
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
	subq	$16, %rsp
.Ltmp609:
	.cfi_def_cfa_offset 64
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
	movq	%rdi, %r14
	movq	48(%r14), %r15
	movq	%rsi, %rbx
	movq	%r15, %rdi
                                        # kill: RSI<def> RBX<kill>
	callq	attribute_list_contained
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB78_2
# BB#1:                                 # %land.rhs.i
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	attribute_list_contained
	testq	%r13, %rax
	jne	.LBB78_24
.LBB78_2:                               # %if.then
	movq	%r14, %rdi
	callq	copy_node
	movq	%rax, %r12
	movq	$0, 80(%r12)
	movq	$0, 72(%r12)
	movq	%rbx, 48(%r12)
	movq	%r12, 128(%r12)
	movq	$0, 120(%r12)
	movl	16(%r12), %eax
	movabsq	$4294961151, %rcx       # imm = 0xFFFFE7FF
	andq	%rax, %rcx
	movl	%ecx, 16(%r12)
	movl	60(%r12), %ecx
	movabsq	$4292870143, %rdx       # imm = 0xFFDFFFFF
	andq	%rcx, %rdx
	movl	%edx, 60(%r12)
	movq	8(%r12), %r15
	xorq	%rdx, %rdx
	testq	%rbx, %rbx
	je	.LBB78_5
	.align	16, 0x90
.LBB78_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movq	%rdx, %rdi
	movq	(%rbx), %rbx
	andq	$262143, %rsi           # imm = 0x3FFFF
	andq	%r13, %rdx
	addq	%rsi, %rdx
	testq	%rbx, %rbx
	jne	.LBB78_3
# BB#4:                                 # %for.cond.for.end_crit_edge.i
	addq	%rdi, %rsi
	andq	%r13, %rsi
	movq	%rsi, %rdx
.LBB78_5:                               # %attribute_hash_list.exit
	andq	$255, %rax
	andq	$262143, %r15           # imm = 0x3FFFF
	addq	%rax, %r15
	addq	%rdx, %r15
	cmpq	$17, %rax
	jg	.LBB78_17
# BB#6:                                 # %attribute_hash_list.exit
	cmpq	$6, %rax
	jne	.LBB78_7
# BB#21:                                # %sw.bb31
	movq	112(%r12), %rax
	jmp	.LBB78_20
.LBB78_17:                              # %attribute_hash_list.exit
	cmpq	$18, %rax
	je	.LBB78_19
# BB#18:                                # %attribute_hash_list.exit
	cmpq	$23, %rax
	jne	.LBB78_9
.LBB78_19:                              # %sw.bb24
	movq	24(%r12), %rax
.LBB78_20:                              # %sw.epilog
	andq	$262143, %rax           # imm = 0x3FFFF
	addq	%rax, %r15
	jmp	.LBB78_9
.LBB78_7:                               # %attribute_hash_list.exit
	cmpq	$7, %rax
	jne	.LBB78_9
# BB#8:                                 # %sw.bb37
	andq	$511, %rcx              # imm = 0x1FF
	addq	%rcx, %r15
.LBB78_9:                               # %sw.epilog
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB78_13
# BB#10:                                # %if.end.i
	movq	%r12, %rdi
	callq	layout_type
	movq	type_hash_table(%rip), %rdi
	andq	%r15, %r13
	movq	%r13, (%rsp)
	movq	%r12, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	%r15, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB78_12
# BB#11:                                # %type_hash_lookup.exit.i
	movq	8(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB78_14
.LBB78_12:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movq	%r13, (%rbx)
	movq	%r12, 8(%rbx)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
.LBB78_13:                              # %type_hash_canon.exit
	movq	%r12, %rdi
.LBB78_14:                              # %type_hash_canon.exit
	movl	16(%r14), %edx
	movq	$12, %rcx
	movq	%rdx, %r8
	shrq	%cl, %r8
	movq	$10, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movl	60(%r14), %eax
	movq	$19, %rcx
	shrq	%cl, %rax
	andq	$255, %rdx
	xorq	%rsi, %rsi
	cmpq	$20, %rdx
	jne	.LBB78_23
# BB#15:                                # %land.rhs
	cmpq	$0, 8(%r14)
	je	.LBB78_16
# BB#22:                                # %land.rhs
	movabsq	$1, %rsi
	jmp	.LBB78_23
.LBB78_16:
	movabsq	$0, %rsi
.LBB78_23:                              # %land.end
	andq	$1, %r8
	andq	$2, %rbx
	orq	%r8, %rbx
	andq	$4, %rax
	orq	%rbx, %rax
	movq	$3, %rcx
	shlq	%cl, %rsi
	orq	%rax, %rsi
	callq	build_qualified_type
	movq	%rax, %r14
.LBB78_24:                              # %if.end
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp615:
	.size	build_type_attribute_variant, .Ltmp615-build_type_attribute_variant
	.cfi_endproc

	.globl	attribute_list_equal
	.align	16, 0x90
	.type	attribute_list_equal,@function
attribute_list_equal:                   # @attribute_list_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp620:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp621:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp622:
	.cfi_def_cfa_offset 32
.Ltmp623:
	.cfi_offset %rbx, -32
.Ltmp624:
	.cfi_offset %r14, -24
.Ltmp625:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	attribute_list_contained
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rcx
	je	.LBB79_4
# BB#1:                                 # %land.rhs
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	attribute_list_contained
	testq	%r15, %rax
	je	.LBB79_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB79_4
.LBB79_2:
	movabsq	$0, %rax
.LBB79_4:                               # %land.end
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp626:
	.size	attribute_list_equal, .Ltmp626-attribute_list_equal
	.cfi_endproc

	.globl	attribute_hash_list
	.align	16, 0x90
	.type	attribute_hash_list,@function
attribute_hash_list:                    # @attribute_hash_list
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB80_3
# BB#1:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB80_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdi), %rdx
	movq	(%rdi), %rdi
	andq	$262143, %rdx           # imm = 0x3FFFF
	andq	%rcx, %rax
	addq	%rdx, %rax
	testq	%rdi, %rdi
	jne	.LBB80_2
.LBB80_3:                               # %for.end
	retq
.Ltmp627:
	.size	attribute_hash_list, .Ltmp627-attribute_hash_list
	.cfi_endproc

	.globl	type_hash_canon
	.align	16, 0x90
	.type	type_hash_canon,@function
type_hash_canon:                        # @type_hash_canon
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp633:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp634:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp635:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp636:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp637:
	.cfi_def_cfa_offset 64
.Ltmp638:
	.cfi_offset %rbx, -40
.Ltmp639:
	.cfi_offset %r12, -32
.Ltmp640:
	.cfi_offset %r14, -24
.Ltmp641:
	.cfi_offset %r15, -16
	movl	debug_no_type_hash(%rip), %eax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB81_4
# BB#1:                                 # %if.end
	movq	%rbx, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r14, %r12
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%r14, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB81_3
# BB#2:                                 # %type_hash_lookup.exit
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB81_5
.LBB81_3:                               # %if.else
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	movq	%r12, (%r15)
	movq	%rbx, 8(%r15)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	htab_find_slot_with_hash
	movq	%r15, (%rax)
.LBB81_4:                               # %return
	movq	%rbx, %rax
.LBB81_5:                               # %return
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp642:
	.size	type_hash_canon, .Ltmp642-type_hash_canon
	.cfi_endproc

	.globl	build_qualified_type
	.align	16, 0x90
	.type	build_qualified_type,@function
build_qualified_type:                   # @build_qualified_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp649:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp650:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp651:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp652:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp653:
	.cfi_def_cfa_offset 48
.Ltmp654:
	.cfi_offset %rbx, -48
.Ltmp655:
	.cfi_offset %r12, -40
.Ltmp656:
	.cfi_offset %r13, -32
.Ltmp657:
	.cfi_offset %r14, -24
.Ltmp658:
	.cfi_offset %r15, -16
	movq	128(%rdi), %r14
	movq	%rsi, %r15
	testq	%r14, %r14
	je	.LBB82_9
# BB#1:                                 # %for.body.lr.ph.i
	movabsq	$0, %r9
	movabsq	$1, %r8
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	%r15, %r11
	andq	%r10, %r11
	movq	%r14, %rax
	.align	16, 0x90
.LBB82_2:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rax), %edx
	movq	$12, %rcx
	movq	%rdx, %r12
	shrq	%cl, %r12
	movq	$10, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movl	60(%rax), %r13d
	movq	$19, %rcx
	shrq	%cl, %r13
	andq	$255, %rdx
	xorq	%rbx, %rbx
	cmpq	$20, %rdx
	jne	.LBB82_5
# BB#3:                                 # %land.rhs.i
                                        #   in Loop: Header=BB82_2 Depth=1
	cmpq	$0, 8(%rax)
	movq	%r9, %rbx
	je	.LBB82_5
# BB#4:                                 # %land.rhs.i
                                        #   in Loop: Header=BB82_2 Depth=1
	movq	%r8, %rbx
.LBB82_5:                               # %land.end.i
                                        #   in Loop: Header=BB82_2 Depth=1
	andq	$1, %r12
	andq	$2, %rsi
	orq	%r12, %rsi
	andq	$4, %r13
	orq	%rsi, %r13
	movq	$3, %rcx
	shlq	%cl, %rbx
	orq	%r13, %rbx
	andq	%r10, %rbx
	cmpq	%r11, %rbx
	jne	.LBB82_7
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB82_2 Depth=1
	movq	96(%rax), %rcx
	movq	96(%rdi), %rdx
	cmpq	%rdx, %rcx
	je	.LBB82_8
.LBB82_7:                               # %for.inc.i
                                        #   in Loop: Header=BB82_2 Depth=1
	movq	120(%rax), %rax
	testq	%rax, %rax
	jne	.LBB82_2
	jmp	.LBB82_9
.LBB82_8:                               # %get_qualified_type.exit
	testq	%rax, %rax
	jne	.LBB82_10
.LBB82_9:                               # %if.then
	callq	copy_node
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	120(%r14), %rcx
	movq	%rcx, 120(%rax)
	movq	%rax, 120(%r14)
	movl	16(%rax), %ecx
	movq	%r15, %rdx
	andq	$1, %rdx
	shlq	$12, %rdx
	movabsq	$4294961151, %rsi       # imm = 0xFFFFE7FF
	andq	%rcx, %rsi
	movq	%r15, %rcx
	andq	$2, %rcx
	shlq	$10, %rcx
	orq	%rdx, %rcx
	orq	%rsi, %rcx
	movl	%ecx, 16(%rax)
	movl	60(%rax), %ecx
	andq	$4, %r15
	shlq	$19, %r15
	movabsq	$4292870143, %rdx       # imm = 0xFFDFFFFF
	andq	%rcx, %rdx
	orq	%r15, %rdx
	movl	%edx, 60(%rax)
.LBB82_10:                              # %if.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp659:
	.size	build_qualified_type, .Ltmp659-build_qualified_type
	.cfi_endproc

	.globl	default_comp_type_attributes
	.align	16, 0x90
	.type	default_comp_type_attributes,@function
default_comp_type_attributes:           # @default_comp_type_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	retq
.Ltmp660:
	.size	default_comp_type_attributes, .Ltmp660-default_comp_type_attributes
	.cfi_endproc

	.globl	default_set_default_type_attributes
	.align	16, 0x90
	.type	default_set_default_type_attributes,@function
default_set_default_type_attributes:    # @default_set_default_type_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp661:
	.size	default_set_default_type_attributes, .Ltmp661-default_set_default_type_attributes
	.cfi_endproc

	.globl	default_insert_attributes
	.align	16, 0x90
	.type	default_insert_attributes,@function
default_insert_attributes:              # @default_insert_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	retq
.Ltmp662:
	.size	default_insert_attributes, .Ltmp662-default_insert_attributes
	.cfi_endproc

	.globl	default_function_attribute_inlinable_p
	.align	16, 0x90
	.type	default_function_attribute_inlinable_p,@function
default_function_attribute_inlinable_p: # @default_function_attribute_inlinable_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp663:
	.size	default_function_attribute_inlinable_p, .Ltmp663-default_function_attribute_inlinable_p
	.cfi_endproc

	.globl	default_ms_bitfield_layout_p
	.align	16, 0x90
	.type	default_ms_bitfield_layout_p,@function
default_ms_bitfield_layout_p:           # @default_ms_bitfield_layout_p
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	retq
.Ltmp664:
	.size	default_ms_bitfield_layout_p, .Ltmp664-default_ms_bitfield_layout_p
	.cfi_endproc

	.globl	is_attribute_p
	.align	16, 0x90
	.type	is_attribute_p,@function
is_attribute_p:                         # @is_attribute_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp671:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp672:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp673:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp674:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp675:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp676:
	.cfi_def_cfa_offset 64
.Ltmp677:
	.cfi_offset %rbx, -48
.Ltmp678:
	.cfi_offset %r12, -40
.Ltmp679:
	.cfi_offset %r13, -32
.Ltmp680:
	.cfi_offset %r14, -24
.Ltmp681:
	.cfi_offset %r15, -16
	movzbl	16(%rsi), %eax
	movq	%rdi, %r13
	xorq	%r14, %r14
	cmpq	$1, %rax
	jne	.LBB88_16
# BB#1:                                 # %if.end
	movq	32(%rsi), %rbx
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	strcmp
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	movq	$1, %r14
	je	.LBB88_16
# BB#2:                                 # %if.end3
	movq	%rbx, %rdi
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	callq	strlen
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	strlen
	movzbl	(%r13), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_8
# BB#3:                                 # %if.then13
	movzbl	1(%r13), %ecx
	cmpq	$95, %rcx
	movq	%r12, %rsi
	jne	.LBB88_17
# BB#4:                                 # %lor.lhs.false
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movabsq	$-8589934592, %rcx      # imm = 0xFFFFFFFE00000000
	movq	%rcx, %rdi
	addq	%rdx, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movzbl	(%r13,%rdi), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_17
# BB#5:                                 # %lor.lhs.false22
	movabsq	$-4294967296, %rcx      # imm = 0xFFFFFFFF00000000
	addq	%rcx, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%r13,%rdx), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_17
# BB#6:                                 # %if.end30
	movq	%r15, %rdx
	movq	%rdx, %rcx
	andq	%rsi, %rcx
	addq	$-4, %rax
	andq	%rsi, %rax
	cmpq	%rax, %rcx
	jne	.LBB88_15
# BB#7:                                 # %land.lhs.true
	addq	$2, %r13
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r13, %rdi
	movq	%rsi, %rbx
	movq	8(%rsp), %rsi           # 8-byte Reload
.LBB88_14:                              # %land.lhs.true67
	callq	strncmp
	testq	%rbx, %rax
	jne	.LBB88_15
	jmp	.LBB88_16
.LBB88_8:                               # %if.else
	movq	%r15, %rcx
	andq	%r12, %rcx
	leaq	4(%rax), %rdx
	andq	%r12, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB88_15
# BB#9:                                 # %land.lhs.true43
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rcx), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_15
# BB#10:                                # %land.lhs.true48
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	1(%rcx), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_15
# BB#11:                                # %land.lhs.true53
	movq	$32, %rcx
	shlq	%cl, %r15
	movabsq	$-8589934592, %rcx      # imm = 0xFFFFFFFE00000000
	movq	%rcx, %rdx
	addq	%r15, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rcx,%rdx), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_15
# BB#12:                                # %land.lhs.true60
	movabsq	$-4294967296, %rcx      # imm = 0xFFFFFFFF00000000
	addq	%rcx, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	8(%rsp), %rcx           # 8-byte Reload
	movzbl	(%rcx,%r15), %ecx
	cmpq	$95, %rcx
	jne	.LBB88_15
# BB#13:                                # %land.lhs.true67
	movq	8(%rsp), %rsi           # 8-byte Reload
	addq	$2, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r13, %rdi
	movq	%r12, %rbx
	movq	%rax, %rdx
	jmp	.LBB88_14
.LBB88_15:                              # %if.end75
	xorq	%r14, %r14
.LBB88_16:                              # %return
	movq	%r14, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB88_17:                              # %if.then29
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.is_attribute_p, %rcx
	movq	$2815, %rsi             # imm = 0xAFF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp682:
	.size	is_attribute_p, .Ltmp682-is_attribute_p
	.cfi_endproc

	.globl	lookup_attribute
	.align	16, 0x90
	.type	lookup_attribute,@function
lookup_attribute:                       # @lookup_attribute
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp688:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp689:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp690:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp691:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp692:
	.cfi_def_cfa_offset 48
.Ltmp693:
	.cfi_offset %rbx, -40
.Ltmp694:
	.cfi_offset %r12, -32
.Ltmp695:
	.cfi_offset %r14, -24
.Ltmp696:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r15
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB89_6
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB89_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$1, %rax
	jne	.LBB89_7
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB89_2 Depth=1
	movq	%r15, %rdi
	callq	is_attribute_p
	testq	%r12, %rax
	jne	.LBB89_4
# BB#5:                                 # %for.inc
                                        #   in Loop: Header=BB89_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB89_2
	jmp	.LBB89_6
.LBB89_4:
	movq	%rbx, %r14
.LBB89_6:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB89_7:                               # %if.then
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.lookup_attribute, %rcx
	movq	$2848, %rsi             # imm = 0xB20
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp697:
	.size	lookup_attribute, .Ltmp697-lookup_attribute
	.cfi_endproc

	.globl	merge_attributes
	.align	16, 0x90
	.type	merge_attributes,@function
merge_attributes:                       # @merge_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp704:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp705:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp706:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp707:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp708:
	.cfi_def_cfa_offset 48
.Ltmp709:
	.cfi_offset %rbx, -48
.Ltmp710:
	.cfi_offset %r12, -40
.Ltmp711:
	.cfi_offset %r13, -32
.Ltmp712:
	.cfi_offset %r14, -24
.Ltmp713:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	testq	%r15, %r15
	je	.LBB90_3
# BB#1:                                 # %if.else
	testq	%r14, %r14
	je	.LBB90_2
# BB#4:                                 # %land.lhs.true
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	attribute_list_contained
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB90_5
.LBB90_2:
	movq	%r15, %r14
.LBB90_3:                               # %if.end41
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB90_5:                               # %if.then2
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	attribute_list_contained
	testq	%r13, %rax
	jne	.LBB90_3
# BB#6:
	xorq	%rax, %rax
	movq	%r15, %rcx
	.align	16, 0x90
.LBB90_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rcx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB90_7
# BB#8:
	xorq	%rdx, %rdx
	movq	%r14, %rcx
	.align	16, 0x90
.LBB90_9:                               # %for.body.i70
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rcx), %rcx
	incq	%rdx
	testq	%rcx, %rcx
	jne	.LBB90_9
# BB#10:                                # %list_length.exit72
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	movq	%r15, %r12
	jl	.LBB90_12
# BB#11:                                # %list_length.exit72
	movq	%r14, %r12
.LBB90_12:                              # %list_length.exit72
	jl	.LBB90_14
# BB#13:                                # %list_length.exit72
	movq	%r15, %r14
	jmp	.LBB90_14
.LBB90_29:                              # %for.inc35
                                        #   in Loop: Header=BB90_14 Depth=1
	movq	(%r12), %r12
.LBB90_14:                              # %list_length.exit72
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB90_17 Depth 2
                                        #     Child Loop BB90_20 Depth 2
                                        #       Child Loop BB90_24 Depth 3
	testq	%r12, %r12
	je	.LBB90_3
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB90_14 Depth=1
	testq	%r14, %r14
	je	.LBB90_28
# BB#16:                                #   in Loop: Header=BB90_14 Depth=1
	movq	24(%r12), %rax
	movq	32(%rax), %r15
	movq	%r14, %rbx
	.align	16, 0x90
.LBB90_17:                              # %for.body.i83
                                        #   Parent Loop BB90_14 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$1, %rax
	jne	.LBB90_30
# BB#18:                                # %if.end.i87
                                        #   in Loop: Header=BB90_17 Depth=2
	movq	%r15, %rdi
	callq	is_attribute_p
	testq	%r13, %rax
	jne	.LBB90_20
# BB#19:                                # %for.inc.i90
                                        #   in Loop: Header=BB90_17 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB90_17
	jmp	.LBB90_28
.LBB90_23:                              #   in Loop: Header=BB90_20 Depth=2
	movq	24(%r12), %rax
	movq	32(%rax), %r15
	.align	16, 0x90
.LBB90_24:                              # %for.body.i73
                                        #   Parent Loop BB90_14 Depth=1
                                        #     Parent Loop BB90_20 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%rbx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$1, %rax
	jne	.LBB90_30
# BB#25:                                # %if.end.i
                                        #   in Loop: Header=BB90_24 Depth=3
	movq	%r15, %rdi
	callq	is_attribute_p
	testq	%r13, %rax
	jne	.LBB90_20
# BB#26:                                # %for.inc.i
                                        #   in Loop: Header=BB90_24 Depth=3
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB90_24
	jmp	.LBB90_28
.LBB90_20:                              # %for.cond13.preheader
                                        #   Parent Loop BB90_14 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB90_24 Depth 3
	testq	%rbx, %rbx
	je	.LBB90_28
# BB#21:                                # %for.body15
                                        #   in Loop: Header=BB90_20 Depth=2
	movq	32(%rbx), %rdi
	movq	32(%r12), %rsi
	callq	simple_cst_equal
	andq	%r13, %rax
	cmpq	$1, %rax
	je	.LBB90_27
# BB#22:                                # %for.inc
                                        #   in Loop: Header=BB90_20 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB90_23
	jmp	.LBB90_28
.LBB90_27:                              # %for.end
                                        #   in Loop: Header=BB90_14 Depth=1
	testq	%rbx, %rbx
	jne	.LBB90_29
.LBB90_28:                              # %if.then30
                                        #   in Loop: Header=BB90_14 Depth=1
	movq	%r12, %rdi
	callq	copy_node
	movq	%r14, (%rax)
	movq	%rax, %r14
	jmp	.LBB90_29
.LBB90_30:                              # %if.then.i84
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.lookup_attribute, %rcx
	movq	$2848, %rsi             # imm = 0xB20
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp714:
	.size	merge_attributes, .Ltmp714-merge_attributes
	.cfi_endproc

	.globl	attribute_list_contained
	.align	16, 0x90
	.type	attribute_list_contained,@function
attribute_list_contained:               # @attribute_list_contained
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp721:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp722:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp723:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp724:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp725:
	.cfi_def_cfa_offset 48
.Ltmp726:
	.cfi_offset %rbx, -48
.Ltmp727:
	.cfi_offset %r12, -40
.Ltmp728:
	.cfi_offset %r13, -32
.Ltmp729:
	.cfi_offset %r14, -24
.Ltmp730:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	cmpq	%rbx, %r14
	movq	$1, %rax
	je	.LBB91_34
# BB#1:                                 # %for.cond.preheader
	movq	%r14, %rsi
	testq	%r14, %r14
	je	.LBB91_6
.LBB91_2:                               # %for.cond.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB91_6
# BB#3:                                 # %land.lhs.true3
                                        #   in Loop: Header=BB91_2 Depth=1
	movq	24(%rsi), %rcx
	movq	24(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB91_7
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB91_2 Depth=1
	movq	32(%rsi), %rcx
	movq	32(%rbx), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB91_7
# BB#5:                                 # %for.inc
                                        #   in Loop: Header=BB91_2 Depth=1
	movq	(%rsi), %rsi
	movq	(%rbx), %rbx
	testq	%rsi, %rsi
	jne	.LBB91_2
.LBB91_6:                               # %for.end
	orq	%rbx, %rsi
	je	.LBB91_34
.LBB91_7:                               # %for.cond18.preheader
	testq	%rbx, %rbx
	je	.LBB91_34
# BB#8:                                 # %for.body20.lr.ph
	testq	%r14, %r14
	je	.LBB91_9
# BB#10:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB91_11:                              # %for.body.i.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB91_12 Depth 2
                                        #     Child Loop BB91_18 Depth 2
                                        #       Child Loop BB91_24 Depth 3
	movq	24(%rbx), %rax
	movq	32(%rax), %r12
	movq	%r14, %r15
	.align	16, 0x90
.LBB91_12:                              # %for.body.i
                                        #   Parent Loop BB91_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%r15), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$1, %rax
	jne	.LBB91_35
# BB#13:                                # %if.end.i
                                        #   in Loop: Header=BB91_12 Depth=2
	movq	%r12, %rdi
	callq	is_attribute_p
	testq	%r13, %rax
	jne	.LBB91_16
# BB#14:                                # %for.inc.i
                                        #   in Loop: Header=BB91_12 Depth=2
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB91_12
	jmp	.LBB91_15
.LBB91_16:                              # %for.cond23.preheader
                                        #   in Loop: Header=BB91_11 Depth=1
	testq	%r15, %r15
	je	.LBB91_17
.LBB91_18:                              # %for.body25
                                        #   Parent Loop BB91_11 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB91_24 Depth 3
	movq	32(%rbx), %rdi
	movq	32(%r15), %rsi
	callq	simple_cst_equal
	andq	%r13, %rax
	cmpq	$1, %rax
	je	.LBB91_28
# BB#19:                                # %for.inc34
                                        #   in Loop: Header=BB91_18 Depth=2
	movq	(%r15), %r15
	testq	%r15, %r15
	je	.LBB91_20
# BB#23:                                #   in Loop: Header=BB91_18 Depth=2
	movq	24(%rbx), %rax
	movq	32(%rax), %r12
	.align	16, 0x90
.LBB91_24:                              # %for.body.i89
                                        #   Parent Loop BB91_11 Depth=1
                                        #     Parent Loop BB91_18 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	24(%r15), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$1, %rax
	jne	.LBB91_35
# BB#25:                                # %if.end.i93
                                        #   in Loop: Header=BB91_24 Depth=3
	movq	%r12, %rdi
	callq	is_attribute_p
	testq	%r13, %rax
	jne	.LBB91_21
# BB#26:                                # %for.inc.i96
                                        #   in Loop: Header=BB91_24 Depth=3
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB91_24
	jmp	.LBB91_27
.LBB91_21:                              # %for.cond23.backedge
                                        #   in Loop: Header=BB91_18 Depth=2
	testq	%r15, %r15
	jne	.LBB91_18
	jmp	.LBB91_22
.LBB91_28:                              # %for.end43
                                        #   in Loop: Header=BB91_11 Depth=1
	testq	%r15, %r15
	je	.LBB91_29
# BB#30:                                # %if.end46
                                        #   in Loop: Header=BB91_11 Depth=1
	movq	32(%rbx), %rdi
	movq	32(%r15), %rsi
	callq	simple_cst_equal
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB91_31
# BB#32:                                # %for.inc55
                                        #   in Loop: Header=BB91_11 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB91_11
# BB#33:
	movq	$1, %rax
	jmp	.LBB91_34
.LBB91_9:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_15:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_27:
	xorq	%rax, %rax
.LBB91_34:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB91_20:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_17:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_22:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_29:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_31:
	xorq	%rax, %rax
	jmp	.LBB91_34
.LBB91_35:                              # %if.then.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.lookup_attribute, %rcx
	movq	$2848, %rsi             # imm = 0xB20
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp731:
	.size	attribute_list_contained, .Ltmp731-attribute_list_contained
	.cfi_endproc

	.globl	simple_cst_equal
	.align	16, 0x90
	.type	simple_cst_equal,@function
simple_cst_equal:                       # @simple_cst_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp738:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp739:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp740:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp741:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp742:
	.cfi_def_cfa_offset 48
.Ltmp743:
	.cfi_offset %rbx, -48
.Ltmp744:
	.cfi_offset %r12, -40
.Ltmp745:
	.cfi_offset %r13, -32
.Ltmp746:
	.cfi_offset %r14, -24
.Ltmp747:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	cmpq	%rbx, %r14
	je	.LBB92_61
# BB#1:
	movabsq	$0, %r12
	movabsq	$1, %r13
	jmp	.LBB92_2
	.align	16, 0x90
.LBB92_4:                               # %if.end
                                        #   Parent Loop BB92_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB92_5
# BB#6:                                 # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	movl	16(%rbx), %ecx
	movl	16(%r14), %eax
	movq	%rcx, %r15
	andq	$255, %r15
	andq	$254, %rcx
	cmpq	$114, %rcx
	movq	%r12, %rcx
	jne	.LBB92_8
# BB#7:                                 # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	movq	%r13, %rcx
.LBB92_8:                               # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	movq	%rax, %rdx
	andq	$254, %rdx
	cmpq	$116, %r15
	movq	%r12, %rsi
	jne	.LBB92_10
# BB#9:                                 # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	movq	%r13, %rsi
.LBB92_10:                              # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	orq	%rsi, %rcx
	cmpq	$114, %rdx
	je	.LBB92_12
# BB#11:                                # %if.end5
                                        #   in Loop: Header=BB92_4 Depth=2
	andq	$255, %rax
	cmpq	$116, %rax
	je	.LBB92_12
# BB#15:                                # %if.else29
                                        #   in Loop: Header=BB92_4 Depth=2
	testq	%rcx, %rcx
	je	.LBB92_17
# BB#16:                                # %if.then35
                                        #   in Loop: Header=BB92_4 Depth=2
	movq	32(%rbx), %rbx
	cmpq	%rbx, %r14
	jne	.LBB92_4
	jmp	.LBB92_61
	.align	16, 0x90
.LBB92_12:                              # %if.then15
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	32(%r14), %r14
	testq	%rcx, %rcx
	je	.LBB92_14
	jmp	.LBB92_13
	.align	16, 0x90
.LBB92_17:                              # %if.end41
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	%r15, %rax
	jne	.LBB92_18
# BB#19:                                # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$117, %r15
	jg	.LBB92_38
# BB#20:                                # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$45, %r15
	jg	.LBB92_23
# BB#21:                                # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	leaq	-25(%r15), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$14, %rcx
	ja	.LBB92_64
# BB#22:                                # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI92_1(,%rcx,8)
.LBB92_63:                              # %sw.bb203
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	40(%r14), %rax
	movq	40(%rbx), %rcx
	cmpq	%rcx, %rax
	movabsq	$0, %rax
	je	.LBB92_39
	jmp	.LBB92_62
.LBB92_38:                              # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$118, %r15
	jne	.LBB92_64
.LBB92_39:                              # %sw.bb88
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	32(%r14), %r14
.LBB92_13:                              # %if.then21
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	32(%rbx), %rbx
.LBB92_14:                              # %tailrecurse.outer.backedge
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	%rbx, %r14
	jne	.LBB92_2
	jmp	.LBB92_61
.LBB92_23:                              # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	leaq	-46(%r15), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$9, %rax
	ja	.LBB92_64
# BB#24:                                # %if.end44
                                        #   in Loop: Header=BB92_2 Depth=1
	jmpq	*.LJTI92_0(,%rax,8)
.LBB92_51:                              # %sw.bb115
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	32(%r14), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$34, %rax
	jne	.LBB92_55
# BB#52:                                # %land.lhs.true
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$0, 72(%rdi)
	jne	.LBB92_55
# BB#53:                                # %land.lhs.true130
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$0, 144(%rdi)
	jne	.LBB92_55
# BB#54:                                # %land.lhs.true130.if.end176_crit_edge
                                        #   in Loop: Header=BB92_2 Depth=1
	addq	$32, %rbx
	jmp	.LBB92_59
.LBB92_55:                              # %lor.lhs.false137
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	32(%rbx), %rsi
	movzbl	16(%rsi), %eax
	addq	$32, %rbx
	cmpq	$34, %rax
	jne	.LBB92_58
# BB#56:                                # %land.lhs.true147
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$0, 72(%rsi)
	jne	.LBB92_58
# BB#57:                                # %land.lhs.true155
                                        #   in Loop: Header=BB92_2 Depth=1
	cmpq	$0, 144(%rsi)
	je	.LBB92_59
.LBB92_58:                              # %if.end172
                                        #   in Loop: Header=BB92_2 Depth=1
	callq	simple_cst_equal
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB92_62
.LBB92_59:                              # %if.end176
                                        #   in Loop: Header=BB92_2 Depth=1
	movq	40(%r14), %r14
	movq	8(%rbx), %rbx
	jmp	.LBB92_14
	.align	16, 0x90
.LBB92_2:                               # %if.end.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB92_4 Depth 2
	testq	%r14, %r14
	jne	.LBB92_4
# BB#3:
	xorq	%rax, %rax
	jmp	.LBB92_62
.LBB92_5:
	xorq	%rax, %rax
	jmp	.LBB92_62
.LBB92_64:                              # %sw.epilog
	cmpq	$146, %r15
	jbe	.LBB92_66
# BB#65:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB92_62
.LBB92_18:
	xorq	%rax, %rax
	jmp	.LBB92_62
.LBB92_66:                              # %if.end225
	movsbq	tree_code_type(%r15), %rax
	cmpq	$113, %rax
	jg	.LBB92_70
# BB#67:                                # %if.end225
	addq	$-49, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$52, %rcx
	jbe	.LBB92_69
# BB#68:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB92_62
.LBB92_70:                              # %if.end225
	addq	$-114, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jbe	.LBB92_71
	jmp	.LBB92_62
.LBB92_69:                              # %if.end225
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rcx # imm = 0x10000000000803
	testq	%rcx, %rax
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	je	.LBB92_62
.LBB92_71:                              # %for.cond.preheader
	movslq	tree_code_length(,%r15,4), %rax
	testq	%rax, %rax
	movq	$1, %rax
	jle	.LBB92_62
# BB#72:
	movq	$4, %r12
.LBB92_74:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r14,%r12,8), %rdi
	movq	(%rbx,%r12,8), %rsi
	callq	simple_cst_equal
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB92_62
# BB#73:                                # %for.cond
                                        #   in Loop: Header=BB92_74 Depth=1
	leaq	-3(%r12), %rdx
	movslq	tree_code_length(,%r15,4), %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r12
	cmpq	%rsi, %rdx
	jl	.LBB92_74
	jmp	.LBB92_62
.LBB92_36:                              # %sw.bb77
	movq	40(%r14), %rax
	movq	40(%rbx), %rcx
	cmpq	%rcx, %rax
	movq	$1, %rax
	je	.LBB92_62
# BB#37:                                # %if.else87
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.simple_cst_equal, %rcx
	movq	$3603, %rsi             # imm = 0xE13
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB92_40:                              # %sw.bb96
	movq	32(%r14), %rdi
	movq	32(%rbx), %rsi
	callq	simple_cst_equal
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB92_62
# BB#41:                                # %if.end107
	movq	40(%rbx), %r15
	movq	40(%r14), %rbx
	testq	%r15, %r15
	je	.LBB92_48
# BB#42:                                # %if.end107
	testq	%rbx, %rbx
	je	.LBB92_48
# BB#43:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB92_44:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movq	32(%r15), %rsi
	callq	simple_cst_equal
	andq	%r14, %rax
	cmpq	$1, %rax
	jne	.LBB92_45
# BB#46:                                # %if.end.i
                                        #   in Loop: Header=BB92_44 Depth=1
	movq	(%r15), %r15
	movq	(%rbx), %rbx
	testq	%r15, %r15
	je	.LBB92_48
# BB#47:                                # %if.end.i
                                        #   in Loop: Header=BB92_44 Depth=1
	testq	%rbx, %rbx
	jne	.LBB92_44
.LBB92_48:                              # %while.end.i
	cmpq	%r15, %rbx
	jne	.LBB92_49
# BB#50:                                # %while.end.i
	movabsq	$1, %rax
	jmp	.LBB92_62
.LBB92_60:                              # %sw.bb184
	movq	32(%r14), %rdi
	movq	32(%rbx), %rsi
	callq	simple_cst_equal
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB92_62
.LBB92_61:                              # %sw.bb184
	movq	$1, %rax
.LBB92_62:                              # %sw.bb184
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB92_25:                              # %sw.bb
	movq	32(%r14), %rcx
	movq	32(%rbx), %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB92_62
# BB#26:                                # %land.rhs
	movq	40(%r14), %rax
	movq	40(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB92_27
# BB#28:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB92_62
.LBB92_29:                              # %sw.bb56
	addq	$32, %r14
	addq	$32, %rbx
	movq	$24, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB92_30
# BB#31:                                # %sw.bb56
	movabsq	$1, %rax
	jmp	.LBB92_62
.LBB92_32:                              # %sw.bb61
	movl	32(%r14), %esi
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movl	32(%rbx), %edi
	movq	$32, %rcx
	sarq	%cl, %rdx
	xorq	%rax, %rax
	cmpq	%rdi, %rsi
	jne	.LBB92_62
# BB#33:                                # %land.rhs65
	movq	40(%r14), %rdi
	movq	40(%rbx), %rsi
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB92_34
# BB#35:                                # %land.rhs65
	movabsq	$1, %rax
	jmp	.LBB92_62
.LBB92_30:
	movabsq	$0, %rax
	jmp	.LBB92_62
.LBB92_27:
	movabsq	$0, %rax
	jmp	.LBB92_62
.LBB92_34:
	movabsq	$0, %rax
	jmp	.LBB92_62
.LBB92_49:
	movabsq	$0, %rax
	jmp	.LBB92_62
.LBB92_45:
	xorq	%rax, %rax
	jmp	.LBB92_62
.Ltmp748:
	.size	simple_cst_equal, .Ltmp748-simple_cst_equal
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI92_0:
	.quad	.LBB92_36
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_51
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_40
	.quad	.LBB92_64
	.quad	.LBB92_60
.LJTI92_1:
	.quad	.LBB92_25
	.quad	.LBB92_29
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_32
	.quad	.LBB92_62
	.quad	.LBB92_64
	.quad	.LBB92_62
	.quad	.LBB92_64
	.quad	.LBB92_62
	.quad	.LBB92_62
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_64
	.quad	.LBB92_63

	.text
	.globl	merge_type_attributes
	.align	16, 0x90
	.type	merge_type_attributes,@function
merge_type_attributes:                  # @merge_type_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	48(%rdi), %rdi
	movq	48(%rsi), %rsi
	jmp	merge_attributes  # TAILCALL
.Ltmp749:
	.size	merge_type_attributes, .Ltmp749-merge_type_attributes
	.cfi_endproc

	.globl	merge_decl_attributes
	.align	16, 0x90
	.type	merge_decl_attributes,@function
merge_decl_attributes:                  # @merge_decl_attributes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	136(%rdi), %rdi
	movq	136(%rsi), %rsi
	jmp	merge_attributes  # TAILCALL
.Ltmp750:
	.size	merge_decl_attributes, .Ltmp750-merge_decl_attributes
	.cfi_endproc

	.globl	get_qualified_type
	.align	16, 0x90
	.type	get_qualified_type,@function
get_qualified_type:                     # @get_qualified_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp755:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp756:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp757:
	.cfi_def_cfa_offset 32
.Ltmp758:
	.cfi_offset %rbx, -32
.Ltmp759:
	.cfi_offset %r14, -24
.Ltmp760:
	.cfi_offset %r15, -16
	movq	128(%rdi), %r11
	xorq	%rax, %rax
	testq	%r11, %r11
	je	.LBB95_9
# BB#1:                                 # %for.body.lr.ph
	movabsq	$0, %r8
	movabsq	$1, %r9
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	andq	%r10, %rsi
	.align	16, 0x90
.LBB95_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r11), %eax
	movq	$12, %rcx
	movq	%rax, %r14
	shrq	%cl, %r14
	movq	$10, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movl	60(%r11), %r15d
	movq	$19, %rcx
	shrq	%cl, %r15
	andq	$255, %rax
	xorq	%rbx, %rbx
	cmpq	$20, %rax
	jne	.LBB95_5
# BB#3:                                 # %land.rhs
                                        #   in Loop: Header=BB95_2 Depth=1
	cmpq	$0, 8(%r11)
	movq	%r8, %rbx
	je	.LBB95_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB95_2 Depth=1
	movq	%r9, %rbx
.LBB95_5:                               # %land.end
                                        #   in Loop: Header=BB95_2 Depth=1
	andq	$1, %r14
	andq	$2, %rdx
	orq	%r14, %rdx
	andq	$4, %r15
	orq	%rdx, %r15
	movq	$3, %rcx
	shlq	%cl, %rbx
	orq	%r15, %rbx
	andq	%r10, %rbx
	cmpq	%rsi, %rbx
	jne	.LBB95_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB95_2 Depth=1
	movq	96(%r11), %rax
	movq	96(%rdi), %rcx
	cmpq	%rcx, %rax
	je	.LBB95_7
.LBB95_8:                               # %for.inc
                                        #   in Loop: Header=BB95_2 Depth=1
	movq	120(%r11), %r11
	xorq	%rax, %rax
	testq	%r11, %r11
	jne	.LBB95_2
	jmp	.LBB95_9
.LBB95_7:
	movq	%r11, %rax
.LBB95_9:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp761:
	.size	get_qualified_type, .Ltmp761-get_qualified_type
	.cfi_endproc

	.globl	build_type_copy
	.align	16, 0x90
	.type	build_type_copy,@function
build_type_copy:                        # @build_type_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp764:
	.cfi_def_cfa_offset 16
.Ltmp765:
	.cfi_offset %rbx, -16
	movq	128(%rdi), %rbx
	callq	copy_node
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	120(%rbx), %rcx
	movq	%rcx, 120(%rax)
	movq	%rax, 120(%rbx)
	popq	%rbx
	retq
.Ltmp766:
	.size	build_type_copy, .Ltmp766-build_type_copy
	.cfi_endproc

	.globl	type_hash_list
	.align	16, 0x90
	.type	type_hash_list,@function
type_hash_list:                         # @type_hash_list
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB97_3
# BB#1:
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB97_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rdi), %rdx
	movq	(%rdi), %rdi
	andq	$262143, %rdx           # imm = 0x3FFFF
	andq	%rcx, %rax
	addq	%rdx, %rax
	testq	%rdi, %rdi
	jne	.LBB97_2
.LBB97_3:                               # %for.end
	retq
.Ltmp767:
	.size	type_hash_list, .Ltmp767-type_hash_list
	.cfi_endproc

	.globl	type_hash_lookup
	.align	16, 0x90
	.type	type_hash_lookup,@function
type_hash_lookup:                       # @type_hash_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp771:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp772:
	.cfi_def_cfa_offset 24
	subq	$24, %rsp
.Ltmp773:
	.cfi_def_cfa_offset 48
.Ltmp774:
	.cfi_offset %rbx, -24
.Ltmp775:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	layout_type
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r14, %rax
	movq	%rax, 8(%rsp)
	movq	%rbx, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%r14, %rdx
	callq	htab_find_with_hash
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB98_2
# BB#1:                                 # %if.then
	movq	8(%rcx), %rax
.LBB98_2:                               # %return
	addq	$24, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp776:
	.size	type_hash_lookup, .Ltmp776-type_hash_lookup
	.cfi_endproc

	.globl	type_hash_add
	.align	16, 0x90
	.type	type_hash_add,@function
type_hash_add:                          # @type_hash_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp781:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp782:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp783:
	.cfi_def_cfa_offset 32
.Ltmp784:
	.cfi_offset %rbx, -32
.Ltmp785:
	.cfi_offset %r14, -24
.Ltmp786:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	movq	%rax, (%rbx)
	movq	%r14, 8(%rbx)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp787:
	.size	type_hash_add, .Ltmp787-type_hash_add
	.cfi_endproc

	.globl	mark_tree_hashtable
	.align	16, 0x90
	.type	mark_tree_hashtable,@function
mark_tree_hashtable:                    # @mark_tree_hashtable
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	movabsq	$mark_tree_hashtable_entry, %rsi
	xorq	%rdx, %rdx
	jmp	htab_traverse  # TAILCALL
.Ltmp788:
	.size	mark_tree_hashtable, .Ltmp788-mark_tree_hashtable
	.cfi_endproc

	.align	16, 0x90
	.type	mark_tree_hashtable_entry,@function
mark_tree_hashtable_entry:              # @mark_tree_hashtable_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp791:
	.cfi_def_cfa_offset 16
.Ltmp792:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB101_5
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB101_5
# BB#2:                                 # %do.body2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB101_4
# BB#3:                                 # %if.then4
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB101_4:                              # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB101_5:                              # %do.end10
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp793:
	.size	mark_tree_hashtable_entry, .Ltmp793-mark_tree_hashtable_entry
	.cfi_endproc

	.globl	type_list_equal
	.align	16, 0x90
	.type	type_list_equal,@function
type_list_equal:                        # @type_list_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp799:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp800:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp801:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp802:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp803:
	.cfi_def_cfa_offset 48
.Ltmp804:
	.cfi_offset %rbx, -40
.Ltmp805:
	.cfi_offset %r12, -32
.Ltmp806:
	.cfi_offset %r14, -24
.Ltmp807:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%r14, %r14
	je	.LBB102_9
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB102_9
# BB#2:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	.align	16, 0x90
.LBB102_3:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rax
	movq	32(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB102_12
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB102_3 Depth=1
	movq	24(%rbx), %rdi
	movq	24(%r14), %rsi
	cmpq	%rsi, %rdi
	je	.LBB102_7
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB102_3 Depth=1
	callq	simple_cst_equal
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB102_12
# BB#6:                                 # %land.lhs.true13
                                        #   in Loop: Header=BB102_3 Depth=1
	movq	24(%rbx), %rax
	movq	24(%r14), %rcx
	movq	8(%rax), %rax
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB102_12
.LBB102_7:                              # %for.inc
                                        #   in Loop: Header=BB102_3 Depth=1
	movq	(%r14), %r14
	movq	(%rbx), %rbx
	testq	%r14, %r14
	je	.LBB102_9
# BB#8:                                 # %for.inc
                                        #   in Loop: Header=BB102_3 Depth=1
	testq	%rbx, %rbx
	jne	.LBB102_3
.LBB102_9:                              # %for.end
	cmpq	%r14, %rbx
	jne	.LBB102_10
# BB#11:                                # %for.end
	movabsq	$1, %r15
	jmp	.LBB102_12
.LBB102_10:
	movabsq	$0, %r15
.LBB102_12:                             # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp808:
	.size	type_list_equal, .Ltmp808-type_list_equal
	.cfi_endproc

	.globl	type_num_arguments
	.align	16, 0x90
	.type	type_num_arguments,@function
type_num_arguments:                     # @type_num_arguments
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB103_4
# BB#1:                                 # %for.body.lr.ph
	xorq	%rax, %rax
	.align	16, 0x90
.LBB103_2:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rcx), %rdx
	movzbl	16(%rdx), %edx
	cmpq	$5, %rdx
	je	.LBB103_4
# BB#3:                                 # %if.else
                                        #   in Loop: Header=BB103_2 Depth=1
	movq	(%rcx), %rcx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB103_2
.LBB103_4:                              # %for.end
	retq
.Ltmp809:
	.size	type_num_arguments, .Ltmp809-type_num_arguments
	.cfi_endproc

	.globl	tree_int_cst_equal
	.align	16, 0x90
	.type	tree_int_cst_equal,@function
tree_int_cst_equal:                     # @tree_int_cst_equal
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rsi, %rdi
	movq	$1, %rax
	je	.LBB104_8
# BB#1:                                 # %if.end
	testq	%rdi, %rdi
	je	.LBB104_7
# BB#2:                                 # %if.end
	testq	%rsi, %rsi
	je	.LBB104_7
# BB#3:                                 # %if.end4
	movzbl	16(%rdi), %ecx
	cmpq	$25, %rcx
	jne	.LBB104_7
# BB#4:                                 # %land.lhs.true
	movzbl	16(%rsi), %ecx
	cmpq	$25, %rcx
	jne	.LBB104_7
# BB#5:                                 # %land.lhs.true11
	movq	32(%rdi), %rcx
	movq	32(%rsi), %rdx
	cmpq	%rdx, %rcx
	jne	.LBB104_7
# BB#6:                                 # %land.lhs.true17
	movq	40(%rdi), %rcx
	movq	40(%rsi), %rdx
	cmpq	%rdx, %rcx
	je	.LBB104_8
.LBB104_7:                              # %if.end25
	xorq	%rax, %rax
.LBB104_8:                              # %return
	retq
.Ltmp810:
	.size	tree_int_cst_equal, .Ltmp810-tree_int_cst_equal
	.cfi_endproc

	.globl	tree_int_cst_lt
	.align	16, 0x90
	.type	tree_int_cst_lt,@function
tree_int_cst_lt:                        # @tree_int_cst_lt
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	cmpq	%rsi, %rdi
	je	.LBB105_25
# BB#1:                                 # %if.end
	movq	8(%rdi), %rax
	movq	8(%rsi), %rcx
	movl	16(%rax), %r10d
	movl	16(%rcx), %r9d
	movq	$13, %rcx
	movq	%r10, %rdx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	$13, %rcx
	movq	%r9, %rax
	shrq	%cl, %rax
	andq	$1, %rax
	cmpq	%rax, %rdx
	jne	.LBB105_2
# BB#19:                                # %if.else18
	addq	$32, %rdi
	testq	%rdx, %rdx
	je	.LBB105_21
# BB#20:                                # %if.else18.if.end46_crit_edge
	addq	$32, %rsi
	jmp	.LBB105_16
.LBB105_2:                              # %if.then10
	cmpq	$0, 32(%rdi)
	leaq	32(%rdi), %rdi
	jne	.LBB105_4
# BB#3:                                 # %land.lhs.true.i
	xorq	%r8, %r8
	cmpq	$0, 8(%rdi)
	je	.LBB105_8
.LBB105_4:                              # %if.else.i
	testq	$8192, %r10             # imm = 0x2000
	movq	$1, %r8
	jne	.LBB105_8
# BB#5:                                 # %if.else7.i
	cmpq	$0, 8(%rdi)
	js	.LBB105_6
# BB#7:                                 # %select.mid
	movq	$1, %r8
	jmp	.LBB105_8
.LBB105_21:                             # %if.then26
	movq	8(%rdi), %rcx
	movq	40(%rsi), %rdx
	cmpq	%rdx, %rcx
	movq	$1, %rax
	jl	.LBB105_25
# BB#22:                                # %lor.rhs
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB105_25
# BB#23:                                # %land.rhs
	movq	(%rdi), %rax
	movq	32(%rsi), %rcx
	jmp	.LBB105_24
.LBB105_6:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
.LBB105_8:                              # %tree_int_cst_sgn.exit
	cmpq	$0, 32(%rsi)
	leaq	32(%rsi), %rsi
	jne	.LBB105_10
# BB#9:                                 # %land.lhs.true.i96
	xorq	%r10, %r10
	cmpq	$0, 8(%rsi)
	je	.LBB105_14
.LBB105_10:                             # %if.else.i102
	testq	$8192, %r9              # imm = 0x2000
	movq	$1, %r10
	jne	.LBB105_14
# BB#11:                                # %if.else7.i106
	cmpq	$0, 8(%rsi)
	js	.LBB105_12
# BB#13:                                # %select.mid2
	movq	$1, %r10
	jmp	.LBB105_14
.LBB105_12:
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
.LBB105_14:                             # %tree_int_cst_sgn.exit108
	movq	$32, %rcx
	movq	%r10, %r9
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	$32, %rcx
	movq	%r8, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rax
	cmpq	%r9, %rdx
	jl	.LBB105_25
# BB#15:                                # %if.else
	movq	$32, %rcx
	shlq	%cl, %r10
	movq	$32, %rcx
	sarq	%cl, %r10
	movq	$32, %rcx
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	cmpq	%r10, %r8
	movabsq	$0, %rax
	jg	.LBB105_25
.LBB105_16:                             # %if.end46
	movq	8(%rdi), %rcx
	movq	8(%rsi), %rdx
	cmpq	%rdx, %rcx
	movq	$1, %rax
	jb	.LBB105_25
# BB#17:                                # %lor.rhs54
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB105_25
# BB#18:                                # %land.rhs62
	movq	(%rdi), %rax
	movq	(%rsi), %rcx
.LBB105_24:                             # %return
	cmpq	%rcx, %rax
	sbbq	%rax, %rax
	andq	$1, %rax
.LBB105_25:                             # %return
	retq
.Ltmp811:
	.size	tree_int_cst_lt, .Ltmp811-tree_int_cst_lt
	.cfi_endproc

	.globl	tree_int_cst_sgn
	.align	16, 0x90
	.type	tree_int_cst_sgn,@function
tree_int_cst_sgn:                       # @tree_int_cst_sgn
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	$0, 32(%rdi)
	jne	.LBB106_2
# BB#1:                                 # %land.lhs.true
	xorq	%rax, %rax
	cmpq	$0, 40(%rdi)
	je	.LBB106_6
.LBB106_2:                              # %if.else
	movq	8(%rdi), %rax
	movzbl	17(%rax), %ecx
	movq	$1, %rax
	testq	$32, %rcx
	jne	.LBB106_6
# BB#3:                                 # %if.else7
	cmpq	$0, 40(%rdi)
	js	.LBB106_4
# BB#5:                                 # %select.mid
	movq	$1, %rax
.LBB106_6:                              # %return
	retq
.LBB106_4:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.Ltmp812:
	.size	tree_int_cst_sgn, .Ltmp812-tree_int_cst_sgn
	.cfi_endproc

	.globl	tree_int_cst_compare
	.align	16, 0x90
	.type	tree_int_cst_compare,@function
tree_int_cst_compare:                   # @tree_int_cst_compare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp816:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp817:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp818:
	.cfi_def_cfa_offset 32
.Ltmp819:
	.cfi_offset %rbx, -24
.Ltmp820:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	tree_int_cst_lt
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	jne	.LBB107_4
# BB#1:                                 # %if.else
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB107_2
# BB#3:                                 # %if.else
	movabsq	$1, %rax
	jmp	.LBB107_4
.LBB107_2:
	movabsq	$0, %rax
.LBB107_4:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp821:
	.size	tree_int_cst_compare, .Ltmp821-tree_int_cst_compare
	.cfi_endproc

	.globl	host_integerp
	.align	16, 0x90
	.type	host_integerp,@function
host_integerp:                          # @host_integerp
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$25, %rax
	jne	.LBB108_1
# BB#2:                                 # %land.rhs
	movq	40(%rdi), %rcx
	testq	%rcx, %rcx
	jne	.LBB108_3
# BB#4:                                 # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB108_5
.LBB108_1:
	xorq	%rax, %rax
	retq
.LBB108_3:
	movabsq	$0, %rdx
.LBB108_5:                              # %land.rhs
	jne	.LBB108_7
# BB#6:                                 # %land.lhs.true6
	cmpq	$0, 32(%rdi)
	movq	$1, %rax
	jns	.LBB108_14
.LBB108_7:                              # %lor.lhs.false
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	movq	%rdx, %rax
	jne	.LBB108_14
# BB#8:                                 # %land.lhs.true11
	cmpq	$-1, %rcx
	je	.LBB108_10
# BB#9:
	xorq	%rax, %rax
	retq
.LBB108_10:                             # %land.lhs.true16
	cmpq	$0, 32(%rdi)
	movabsq	$0, %rax
	jns	.LBB108_14
# BB#11:                                # %land.lhs.true21
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB108_12
# BB#13:                                # %land.lhs.true21
	movabsq	$1, %rax
.LBB108_14:                             # %land.end34
	retq
.LBB108_12:
	movabsq	$0, %rax
	retq
.Ltmp822:
	.size	host_integerp, .Ltmp822-host_integerp
	.cfi_endproc

	.globl	tree_int_cst_msb
	.align	16, 0x90
	.type	tree_int_cst_msb,@function
tree_int_cst_msb:                       # @tree_int_cst_msb
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp824:
	.cfi_def_cfa_offset 32
	movq	8(%rdi), %rax
	movl	60(%rax), %edx
	movq	32(%rdi), %rax
	movq	$8, %rcx
	shrq	%cl, %rdx
	andq	$254, %rdx
	movzbl	mode_bitsize(%rdx), %edx
	movq	40(%rdi), %rsi
	decq	%rdx
	leaq	8(%rsp), %r8
	leaq	16(%rsp), %r9
	movq	$128, %rcx
	movq	$0, (%rsp)
	movq	%rax, %rdi
	callq	rshift_double
	movq	8(%rsp), %rax
	andq	$1, %rax
	addq	$24, %rsp
	retq
.Ltmp825:
	.size	tree_int_cst_msb, .Ltmp825-tree_int_cst_msb
	.cfi_endproc

	.globl	simple_cst_list_equal
	.align	16, 0x90
	.type	simple_cst_list_equal,@function
simple_cst_list_equal:                  # @simple_cst_list_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp830:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp831:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp832:
	.cfi_def_cfa_offset 32
.Ltmp833:
	.cfi_offset %rbx, -32
.Ltmp834:
	.cfi_offset %r14, -24
.Ltmp835:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%r14, %r14
	je	.LBB110_6
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB110_6
# BB#2:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB110_3:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movq	32(%r14), %rsi
	callq	simple_cst_equal
	movq	%rax, %rcx
	andq	%r15, %rcx
	xorq	%rax, %rax
	cmpq	$1, %rcx
	jne	.LBB110_9
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB110_3 Depth=1
	movq	(%r14), %r14
	movq	(%rbx), %rbx
	testq	%r14, %r14
	je	.LBB110_6
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB110_3 Depth=1
	testq	%rbx, %rbx
	jne	.LBB110_3
.LBB110_6:                              # %while.end
	cmpq	%r14, %rbx
	jne	.LBB110_7
# BB#8:                                 # %while.end
	movabsq	$1, %rax
	jmp	.LBB110_9
.LBB110_7:
	movabsq	$0, %rax
.LBB110_9:                              # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp836:
	.size	simple_cst_list_equal, .Ltmp836-simple_cst_list_equal
	.cfi_endproc

	.globl	compare_tree_int
	.align	16, 0x90
	.type	compare_tree_int,@function
compare_tree_int:                       # @compare_tree_int
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rcx
	testq	%rcx, %rcx
	jne	.LBB111_2
# BB#1:                                 # %land.lhs.true.i
	cmpq	$0, 40(%rdi)
	je	.LBB111_6
.LBB111_2:                              # %if.else.i
	movq	8(%rdi), %rax
	movl	16(%rax), %eax
	movq	40(%rdi), %rdx
	testq	$8192, %rax             # imm = 0x2000
	jne	.LBB111_5
# BB#3:                                 # %if.else.i
	testq	%rdx, %rdx
	jns	.LBB111_5
# BB#4:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB111_5:                              # %if.else
	movq	$1, %rax
	testq	%rdx, %rdx
	jne	.LBB111_9
.LBB111_6:                              # %if.else4
	xorq	%rax, %rax
	cmpq	%rsi, %rcx
	je	.LBB111_9
# BB#7:                                 # %if.else9
	movq	$-1, %rax
	jb	.LBB111_9
# BB#8:                                 # %if.else9
	movq	$1, %rax
.LBB111_9:                              # %return
	retq
.Ltmp837:
	.size	compare_tree_int, .Ltmp837-compare_tree_int
	.cfi_endproc

	.globl	build_reference_type
	.align	16, 0x90
	.type	build_reference_type,@function
build_reference_type:                   # @build_reference_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp841:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp842:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp843:
	.cfi_def_cfa_offset 32
.Ltmp844:
	.cfi_offset %rbx, -24
.Ltmp845:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB112_2
# BB#1:                                 # %if.end
	movq	$15, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r14, 8(%rbx)
	movq	%rbx, 80(%r14)
	movq	%rbx, %rdi
	callq	layout_type
.LBB112_2:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp846:
	.size	build_reference_type, .Ltmp846-build_reference_type
	.cfi_endproc

	.globl	build_type_no_quals
	.align	16, 0x90
	.type	build_type_no_quals,@function
build_type_no_quals:                    # @build_type_no_quals
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp850:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp851:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp852:
	.cfi_def_cfa_offset 32
.Ltmp853:
	.cfi_offset %rbx, -24
.Ltmp854:
	.cfi_offset %r14, -16
	movzbl	16(%rdi), %eax
	cmpq	$15, %rax
	jne	.LBB113_1
# BB#5:                                 # %sw.bb3
	movq	8(%rdi), %rdi
	callq	build_type_no_quals
	movq	%rax, %r14
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB113_8
# BB#6:                                 # %if.end.i
	movq	$15, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r14, 8(%rbx)
	movq	%rbx, 80(%r14)
	jmp	.LBB113_4
.LBB113_1:                              # %entry
	cmpq	$13, %rax
	jne	.LBB113_7
# BB#2:                                 # %sw.bb
	movq	8(%rdi), %rdi
	callq	build_type_no_quals
	movq	%rax, %r14
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB113_8
# BB#3:                                 # %if.end.i14
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r14, 8(%rbx)
	movq	%rbx, 72(%r14)
.LBB113_4:                              # %return
	movq	%rbx, %rdi
	callq	layout_type
	jmp	.LBB113_8
.LBB113_7:                              # %sw.default
	movq	128(%rdi), %rbx
.LBB113_8:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp855:
	.size	build_type_no_quals, .Ltmp855-build_type_no_quals
	.cfi_endproc

	.globl	build_index_type
	.align	16, 0x90
	.type	build_index_type,@function
build_index_type:                       # @build_index_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp861:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp862:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp863:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp864:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp865:
	.cfi_def_cfa_offset 64
.Ltmp866:
	.cfi_offset %rbx, -40
.Ltmp867:
	.cfi_offset %r12, -32
.Ltmp868:
	.cfi_offset %r14, -24
.Ltmp869:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$6, %rdi
	callq	make_node
	movq	sizetype_tab(%rip), %rcx
	movq	%rax, %rbx
	movq	%rcx, 8(%rbx)
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %eax
	movl	60(%rbx), %ecx
	andq	$511, %rax              # imm = 0x1FF
	movabsq	$4294966784, %rdx       # imm = 0xFFFFFE00
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 60(%rbx)
	movq	global_trees+120(%rip), %rax
	movq	%rax, 104(%rbx)
	movq	sizetype_tab(%rip), %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, 112(%rbx)
	movq	sizetype_tab(%rip), %rax
	movl	60(%rax), %eax
	movl	60(%rbx), %ecx
	andq	$65024, %rax            # imm = 0xFE00
	andq	$-65025, %rcx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rax, %rcx
	movl	%ecx, 60(%rbx)
	movq	sizetype_tab(%rip), %rax
	movq	32(%rax), %rax
	movq	%rax, 32(%rbx)
	movq	sizetype_tab(%rip), %rax
	movq	40(%rax), %rax
	movq	%rax, 40(%rbx)
	movq	sizetype_tab(%rip), %rax
	movl	64(%rax), %edx
	movl	%edx, 64(%rbx)
	movl	60(%rax), %eax
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	andq	%rax, %rdx
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, 60(%rbx)
	movl	16(%r14), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$25, %rax
	jne	.LBB114_6
# BB#1:                                 # %land.rhs.i
	cmpq	$0, 40(%r14)
	jne	.LBB114_6
# BB#2:                                 # %land.lhs.true6.i.i
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB114_6
# BB#3:                                 # %if.end.i
	movq	32(%r14), %r14
	movq	%rbx, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r14, %r12
	movq	%r12, 8(%rsp)
	movq	%rbx, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%r14, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB114_5
# BB#4:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB114_7
.LBB114_5:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	movq	%r12, (%r15)
	movq	%rbx, 8(%r15)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	htab_find_slot_with_hash
	movq	%r15, (%rax)
.LBB114_6:                              # %return
	movq	%rbx, %rax
.LBB114_7:                              # %return
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp870:
	.size	build_index_type, .Ltmp870-build_index_type
	.cfi_endproc

	.globl	build_range_type
	.align	16, 0x90
	.type	build_range_type,@function
build_range_type:                       # @build_range_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp877:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp878:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp879:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp880:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp881:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp882:
	.cfi_def_cfa_offset 64
.Ltmp883:
	.cfi_offset %rbx, -48
.Ltmp884:
	.cfi_offset %r12, -40
.Ltmp885:
	.cfi_offset %r13, -32
.Ltmp886:
	.cfi_offset %r14, -24
.Ltmp887:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$6, %rdi
	callq	make_node
	movq	%rax, %r13
	testq	%rbx, %rbx
	movq	%rbx, 8(%r13)
	jne	.LBB115_2
# BB#1:
	movq	sizetype_tab(%rip), %rbx
.LBB115_2:                              # %entry
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	convert
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%r15, %r15
	movq	%rcx, 104(%r13)
	je	.LBB115_4
# BB#3:                                 # %cond.true
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	convert
.LBB115_4:                              # %cond.end
	movq	%rax, 112(%r13)
	movl	60(%rbx), %eax
	movl	60(%r13), %ecx
	andq	$511, %rax              # imm = 0x1FF
	andq	$-512, %rcx             # imm = 0xFFFFFFFFFFFFFE00
	orq	%rax, %rcx
	movl	%ecx, 60(%r13)
	movl	60(%rbx), %eax
	andq	$65024, %rax            # imm = 0xFE00
	andq	$-65025, %rcx           # imm = 0xFFFFFFFFFFFF01FF
	orq	%rax, %rcx
	movl	%ecx, 60(%r13)
	movq	32(%rbx), %rax
	movq	40(%rbx), %rdx
	movq	%rdx, 40(%r13)
	movq	%rax, 32(%r13)
	movl	64(%rbx), %eax
	movl	%eax, 64(%r13)
	movl	60(%rbx), %eax
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	andq	%rax, %rdx
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	orq	%rdx, %rcx
	movl	%ecx, 60(%r13)
	movl	16(%r14), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$25, %rax
	jne	.LBB115_32
# BB#5:                                 # %land.rhs.i
	movq	40(%r14), %rcx
	cmpq	$-1, %rcx
	jne	.LBB115_6
# BB#8:                                 # %land.lhs.true16.i
	movq	32(%r14), %rax
	testq	%rax, %rax
	jns	.LBB115_32
# BB#9:                                 # %land.lhs.true21.i
	movq	8(%r14), %rdx
	movzbl	17(%rdx), %edx
	testq	$32, %rdx
	jne	.LBB115_10
# BB#11:                                # %land.lhs.true21.i
	movabsq	$1, %rdx
	jmp	.LBB115_12
.LBB115_6:                              # %land.rhs.i
	testq	%rcx, %rcx
	jne	.LBB115_32
# BB#7:                                 # %land.lhs.true6.i
	movq	32(%r14), %rax
	testq	%rax, %rax
	movq	$1, %rdx
	jns	.LBB115_12
	jmp	.LBB115_32
.LBB115_10:
	movabsq	$0, %rdx
.LBB115_12:                             # %host_integerp.exit
	testq	%rdx, %rdx
	je	.LBB115_32
# BB#13:                                # %host_integerp.exit
	testq	%r15, %r15
	je	.LBB115_32
# BB#14:                                # %land.lhs.true42
	movl	16(%r15), %edx
	andq	$524543, %rdx           # imm = 0x800FF
	cmpq	$25, %rdx
	jne	.LBB115_32
# BB#15:                                # %land.rhs.i79
	movq	40(%r15), %rdx
	testq	%rdx, %rdx
	jne	.LBB115_16
# BB#22:                                # %host_integerp.exit95
	movq	32(%r15), %r12
	testq	%r12, %r12
	jns	.LBB115_23
	jmp	.LBB115_32
.LBB115_16:                             # %land.rhs.i79
	cmpq	$-1, %rdx
	jne	.LBB115_32
# BB#17:                                # %land.lhs.true16.i87
	movq	32(%r15), %r12
	testq	%r12, %r12
	jns	.LBB115_32
# BB#18:                                # %land.lhs.true21.i93
	movq	8(%r15), %rsi
	movzbl	17(%rsi), %esi
	testq	$32, %rsi
	jne	.LBB115_32
# BB#19:                                # %land.rhs.i.i
	cmpq	$-1, %rdx
	jne	.LBB115_21
# BB#20:                                # %land.lhs.true21.i.i
	movq	8(%r15), %rdx
	movzbl	17(%rdx), %edx
	testq	$32, %rdx
	jne	.LBB115_21
.LBB115_23:                             # %land.rhs.i.i102
	cmpq	$-1, %rcx
	jne	.LBB115_24
# BB#26:                                # %land.lhs.true16.i.i110
	testq	%rax, %rax
	jns	.LBB115_21
# BB#27:                                # %land.lhs.true21.i.i116
	movq	8(%r14), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	je	.LBB115_28
	jmp	.LBB115_21
.LBB115_24:                             # %land.rhs.i.i102
	testq	%rcx, %rcx
	jne	.LBB115_21
# BB#25:                                # %land.lhs.true6.i.i105
	testq	%rax, %rax
	js	.LBB115_21
.LBB115_28:                             # %tree_low_cst.exit121
	movl	debug_no_type_hash(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB115_32
# BB#29:                                # %if.end.i
	subq	%rax, %r12
	movq	%r13, %rdi
	callq	layout_type
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r12, %rbx
	movq	%rbx, (%rsp)
	movq	%r13, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	%r12, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB115_31
# BB#30:                                # %type_hash_lookup.exit.i
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB115_33
.LBB115_31:                             # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %r14
	movq	%rbx, (%r14)
	movq	%r13, 8(%r14)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	htab_find_slot_with_hash
	movq	%r14, (%rax)
.LBB115_32:                             # %return
	movq	%r13, %rax
.LBB115_33:                             # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB115_21:                             # %if.else.i96
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp888:
	.size	build_range_type, .Ltmp888-build_range_type
	.cfi_endproc

	.globl	build_index_2_type
	.align	16, 0x90
	.type	build_index_2_type,@function
build_index_2_type:                     # @build_index_2_type
	.cfi_startproc
# BB#0:                                 # %entry
	movq	sizetype_tab(%rip), %rax
	movq	%rsi, %rcx
	movq	%rdi, %rdx
	movq	%rax, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	jmp	build_range_type  # TAILCALL
.Ltmp889:
	.size	build_index_2_type, .Ltmp889-build_index_2_type
	.cfi_endproc

	.globl	index_type_equal
	.align	16, 0x90
	.type	index_type_equal,@function
index_type_equal:                       # @index_type_equal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp895:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp896:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp897:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp898:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp899:
	.cfi_def_cfa_offset 48
.Ltmp900:
	.cfi_offset %rbx, -40
.Ltmp901:
	.cfi_offset %r12, -32
.Ltmp902:
	.cfi_offset %r14, -24
.Ltmp903:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	movzbl	16(%r15), %ecx
	xorq	%r14, %r14
	cmpq	%rcx, %rax
	jne	.LBB117_8
# BB#1:                                 # %if.end
	cmpq	$6, %rax
	jne	.LBB117_7
# BB#2:                                 # %if.then10
	movl	60(%rbx), %eax
	movl	60(%r15), %ecx
	xorq	%rax, %rcx
	testq	$65535, %rcx            # imm = 0xFFFF
	jne	.LBB117_8
# BB#3:                                 # %lor.lhs.false27
	movq	32(%rbx), %rdi
	movq	32(%r15), %rsi
	callq	simple_cst_equal
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB117_8
# BB#4:                                 # %lor.lhs.false32
	movl	64(%rbx), %eax
	movl	64(%r15), %ecx
	cmpq	%rcx, %rax
	jne	.LBB117_8
# BB#5:                                 # %if.end38
	movq	104(%rbx), %rdi
	movq	104(%r15), %rsi
	callq	simple_cst_equal
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB117_7
# BB#6:                                 # %land.lhs.true
	movq	112(%rbx), %rdi
	movq	112(%r15), %rsi
	callq	simple_cst_equal
	andq	%r12, %rax
	cmpq	$1, %rax
	movq	$1, %r14
	je	.LBB117_8
.LBB117_7:                              # %if.end51
	xorq	%r14, %r14
.LBB117_8:                              # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp904:
	.size	index_type_equal, .Ltmp904-index_type_equal
	.cfi_endproc

	.globl	build_array_type
	.align	16, 0x90
	.type	build_array_type,@function
build_array_type:                       # @build_array_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp910:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp911:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp912:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp913:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp914:
	.cfi_def_cfa_offset 64
.Ltmp915:
	.cfi_offset %rbx, -40
.Ltmp916:
	.cfi_offset %r12, -32
.Ltmp917:
	.cfi_offset %r14, -24
.Ltmp918:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	movq	%rsi, %r14
	cmpq	$23, %rax
	jne	.LBB118_2
# BB#1:                                 # %if.then
	movabsq	$.L.str149, %rdi
	xorq	%rax, %rax
	callq	error
	movq	integer_types+40(%rip), %rbx
.LBB118_2:                              # %if.end
	cmpq	$0, 72(%rbx)
	jne	.LBB118_4
# BB#3:                                 # %if.end.i
	movq	$13, %rdi
	callq	make_node
	movq	%rbx, 8(%rax)
	movq	%rax, 72(%rbx)
	movq	%rax, %rdi
	callq	layout_type
.LBB118_4:                              # %build_pointer_type.exit
	movq	$18, %rdi
	callq	make_node
	movq	%rax, %r15
	movq	%rbx, 8(%r15)
	testq	%r14, %r14
	movq	%r14, 24(%r15)
	je	.LBB118_5
# BB#6:                                 # %if.end6
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB118_10
# BB#7:                                 # %if.end.i25
	andq	$262143, %rbx           # imm = 0x3FFFF
	andq	$262143, %r14           # imm = 0x3FFFF
	addq	%rbx, %r14
	movq	%r15, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r14, %r12
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%r14, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB118_9
# BB#8:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB118_11
.LBB118_9:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movq	%r12, (%rbx)
	movq	%r15, 8(%rbx)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
.LBB118_10:                             # %type_hash_canon.exit
	movq	%r15, %rbx
.LBB118_11:                             # %type_hash_canon.exit
	cmpq	$0, 32(%rbx)
	jne	.LBB118_13
# BB#12:                                # %if.then12
	movq	%rbx, %rdi
	callq	layout_type
	jmp	.LBB118_13
.LBB118_5:
	movq	%r15, %rbx
.LBB118_13:                             # %return
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp919:
	.size	build_array_type, .Ltmp919-build_array_type
	.cfi_endproc

	.globl	get_inner_array_type
	.align	16, 0x90
	.type	get_inner_array_type,@function
get_inner_array_type:                   # @get_inner_array_type
	.cfi_startproc
# BB#0:                                 # %entry
	.align	16, 0x90
.LBB119_1:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$18, %rax
	je	.LBB119_1
# BB#2:                                 # %while.end
	movq	%rdi, %rax
	retq
.Ltmp920:
	.size	get_inner_array_type, .Ltmp920-get_inner_array_type
	.cfi_endproc

	.globl	build_function_type
	.align	16, 0x90
	.type	build_function_type,@function
build_function_type:                    # @build_function_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp926:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp927:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp928:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp929:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp930:
	.cfi_def_cfa_offset 64
.Ltmp931:
	.cfi_offset %rbx, -40
.Ltmp932:
	.cfi_offset %r12, -32
.Ltmp933:
	.cfi_offset %r14, -24
.Ltmp934:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	16(%r15), %eax
	movq	%rsi, %r12
	cmpq	$23, %rax
	jne	.LBB120_2
# BB#1:                                 # %if.then
	movabsq	$.L.str150, %rdi
	xorq	%rax, %rax
	callq	error
	movq	integer_types+40(%rip), %r15
.LBB120_2:                              # %if.end
	movq	$23, %rdi
	callq	make_node
	movq	%rax, %r14
	movq	%r15, 8(%r14)
	xorq	%rbx, %rbx
	testq	%r12, %r12
	movq	%r12, 24(%r14)
	je	.LBB120_6
# BB#3:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB120_4:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r12), %rcx
	movq	%rbx, %rdx
	movq	(%r12), %r12
	andq	$262143, %rcx           # imm = 0x3FFFF
	andq	%rax, %rbx
	addq	%rcx, %rbx
	testq	%r12, %r12
	jne	.LBB120_4
# BB#5:                                 # %for.cond.for.end_crit_edge.i
	addq	%rdx, %rcx
	andq	%rax, %rcx
	movq	%rcx, %rbx
.LBB120_6:                              # %type_hash_list.exit
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB120_10
# BB#7:                                 # %if.end.i
	andq	$262143, %r15           # imm = 0x3FFFF
	addq	%r15, %rbx
	movq	%r14, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%rbx, %r12
	movq	%r12, 8(%rsp)
	movq	%r14, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%rbx, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB120_9
# BB#8:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %r15
	testq	%r15, %r15
	jne	.LBB120_11
.LBB120_9:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %r15
	movq	%r12, (%r15)
	movq	%r14, 8(%r15)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	htab_find_slot_with_hash
	movq	%r15, (%rax)
.LBB120_10:                             # %type_hash_canon.exit
	movq	%r14, %r15
.LBB120_11:                             # %type_hash_canon.exit
	cmpq	$0, 32(%r15)
	jne	.LBB120_13
# BB#12:                                # %if.then9
	movq	%r15, %rdi
	callq	layout_type
.LBB120_13:                             # %if.end10
	movq	%r15, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp935:
	.size	build_function_type, .Ltmp935-build_function_type
	.cfi_endproc

	.globl	build_method_type
	.align	16, 0x90
	.type	build_method_type,@function
build_method_type:                      # @build_method_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp942:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp943:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp944:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp945:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp946:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp947:
	.cfi_def_cfa_offset 64
.Ltmp948:
	.cfi_offset %rbx, -48
.Ltmp949:
	.cfi_offset %r12, -40
.Ltmp950:
	.cfi_offset %r13, -32
.Ltmp951:
	.cfi_offset %r14, -24
.Ltmp952:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r12
	movq	$16, %rdi
	callq	make_node
	movzbl	16(%r15), %ecx
	movq	%rax, %r14
	cmpq	$23, %rcx
	jne	.LBB121_11
# BB#1:                                 # %if.end
	movq	128(%r12), %rax
	movq	%rax, 112(%r14)
	movq	8(%r15), %rax
	movq	%rax, 8(%r14)
	movq	72(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB121_3
# BB#2:                                 # %if.end.i
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r12, 8(%rbx)
	movq	%rbx, 72(%r12)
	movq	%rbx, %rdi
	callq	layout_type
.LBB121_3:                              # %build_pointer_type.exit
	movq	24(%r15), %r13
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	$2, %rcx
	movl	%ecx, 16(%rax)
	movq	%r13, (%rax)
	movq	$0, 24(%rax)
	movq	%rbx, 32(%rax)
	movq	%rax, 24(%r14)
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB121_7
# BB#4:                                 # %if.end.i31
	andq	$262143, %r12           # imm = 0x3FFFF
	andq	$262143, %r15           # imm = 0x3FFFF
	addq	%r12, %r15
	movq	%r14, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r15, %r12
	movq	%r12, (%rsp)
	movq	%r14, 8(%rsp)
	leaq	(%rsp), %rsi
	movq	%r15, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB121_6
# BB#5:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB121_8
.LBB121_6:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movq	%r12, (%rbx)
	movq	%r14, 8(%rbx)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r15, %rdx
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
.LBB121_7:                              # %type_hash_canon.exit
	movq	%r14, %rbx
.LBB121_8:                              # %type_hash_canon.exit
	cmpq	$0, 32(%rbx)
	jne	.LBB121_10
# BB#9:                                 # %if.then17
	movq	%rbx, %rdi
	callq	layout_type
.LBB121_10:                             # %if.end18
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB121_11:                             # %if.then
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.build_method_type, %rcx
	movq	$4009, %rsi             # imm = 0xFA9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp953:
	.size	build_method_type, .Ltmp953-build_method_type
	.cfi_endproc

	.globl	build_offset_type
	.align	16, 0x90
	.type	build_offset_type,@function
build_offset_type:                      # @build_offset_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp959:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp960:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp961:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp962:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp963:
	.cfi_def_cfa_offset 64
.Ltmp964:
	.cfi_offset %rbx, -40
.Ltmp965:
	.cfi_offset %r12, -32
.Ltmp966:
	.cfi_offset %r14, -24
.Ltmp967:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$14, %rdi
	callq	make_node
	movq	128(%rbx), %rcx
	movq	%rax, %r15
	movq	%rcx, 112(%r15)
	movq	%r14, 8(%r15)
	movl	debug_no_type_hash(%rip), %eax
	testq	%rax, %rax
	jne	.LBB122_4
# BB#1:                                 # %if.end.i
	andq	$262143, %rbx           # imm = 0x3FFFF
	andq	$262143, %r14           # imm = 0x3FFFF
	addq	%rbx, %r14
	movq	%r15, %rdi
	callq	layout_type
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	type_hash_table(%rip), %rdi
	andq	%r14, %r12
	movq	%r12, 8(%rsp)
	movq	%r15, 16(%rsp)
	leaq	8(%rsp), %rsi
	movq	%r14, %rdx
	callq	htab_find_with_hash
	testq	%rax, %rax
	je	.LBB122_3
# BB#2:                                 # %type_hash_lookup.exit.i
	movq	8(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB122_5
.LBB122_3:                              # %if.else.i
	movq	$16, %rdi
	callq	ggc_alloc
	movq	%rax, %rbx
	movq	%r12, (%rbx)
	movq	%r15, 8(%rbx)
	movq	type_hash_table(%rip), %rdi
	movq	$1, %rcx
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	htab_find_slot_with_hash
	movq	%rbx, (%rax)
.LBB122_4:                              # %type_hash_canon.exit
	movq	%r15, %rbx
.LBB122_5:                              # %type_hash_canon.exit
	cmpq	$0, 32(%rbx)
	jne	.LBB122_7
# BB#6:                                 # %if.then
	movq	%rbx, %rdi
	callq	layout_type
.LBB122_7:                              # %if.end
	movq	%rbx, %rax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp968:
	.size	build_offset_type, .Ltmp968-build_offset_type
	.cfi_endproc

	.globl	get_unwidened
	.align	16, 0x90
	.type	get_unwidened,@function
get_unwidened:                          # @get_unwidened
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp975:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp976:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp977:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp978:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp979:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp980:
	.cfi_def_cfa_offset 64
.Ltmp981:
	.cfi_offset %rbx, -48
.Ltmp982:
	.cfi_offset %r12, -40
.Ltmp983:
	.cfi_offset %r13, -32
.Ltmp984:
	.cfi_offset %r14, -24
.Ltmp985:
	.cfi_offset %r15, -16
	movq	8(%rdi), %r8
	movq	%rsi, %r15
	testq	%r15, %r15
	movq	%r15, %rcx
	jne	.LBB123_2
# BB#1:                                 # %entry
	movq	%r8, %rcx
.LBB123_2:                              # %entry
	movl	60(%rcx), %r12d
	jne	.LBB123_3
# BB#4:                                 # %entry
	movabsq	$1, %rcx
	jmp	.LBB123_5
.LBB123_3:
	movabsq	$0, %rcx
.LBB123_5:                              # %entry
	andq	$511, %r12              # imm = 0x1FF
	xorq	%rdx, %rdx
	cmpq	%r15, %r8
	movq	%rdi, %rax
	je	.LBB123_9
# BB#6:                                 # %entry
	testq	%rcx, %rcx
	jne	.LBB123_9
# BB#7:                                 # %land.lhs.true5
	movl	60(%r8), %ecx
	andq	$511, %rcx              # imm = 0x1FF
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%r12, %rsi
	cmpq	%rcx, %rsi
	jbe	.LBB123_9
# BB#8:                                 # %land.rhs
	movzbl	17(%r8), %edx
	andq	$32, %rdx
	movq	$5, %rcx
	shrq	%cl, %rdx
.LBB123_9:                              # %land.end
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r12
	movq	%rax, %rbx
	jmp	.LBB123_10
	.align	16, 0x90
.LBB123_24:                             # %land.lhs.true61
                                        #   in Loop: Header=BB123_10 Depth=1
	movq	%r14, %rdx
.LBB123_10:                             # %while.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB123_11 Depth 2
	movq	%rdx, %r14
	.align	16, 0x90
.LBB123_11:                             # %while.cond
                                        #   Parent Loop BB123_10 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	16(%rbx), %ecx
	cmpq	$39, %rcx
	je	.LBB123_25
# BB#12:                                # %while.cond
                                        #   in Loop: Header=BB123_11 Depth=2
	cmpq	$115, %rcx
	jne	.LBB123_40
# BB#13:                                # %while.body
                                        #   in Loop: Header=BB123_11 Depth=2
	movq	8(%rbx), %rcx
	movq	32(%rbx), %rbx
	movq	8(%rbx), %rsi
	movl	60(%rcx), %ecx
	movl	60(%rsi), %edx
	andq	$511, %rcx              # imm = 0x1FF
	andq	$511, %rdx              # imm = 0x1FF
	movq	%rcx, %rdi
	subq	%rdx, %rdi
	jns	.LBB123_15
# BB#14:                                # %while.body
                                        #   in Loop: Header=BB123_11 Depth=2
	andq	%r13, %rcx
	cmpq	%rcx, %r12
	ja	.LBB123_40
.LBB123_15:                             # %if.end
                                        #   in Loop: Header=BB123_11 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	jle	.LBB123_11
# BB#16:                                # %if.then43
                                        #   in Loop: Header=BB123_10 Depth=1
	testq	%r13, %r14
	jne	.LBB123_17
# BB#19:                                # %lor.lhs.false55
                                        #   in Loop: Header=BB123_10 Depth=1
	movzbl	16(%rbx), %ecx
	xorq	%rdx, %rdx
	cmpq	$115, %rcx
	movq	%rbx, %rax
	jne	.LBB123_10
	jmp	.LBB123_20
.LBB123_17:                             # %lor.lhs.false
                                        #   in Loop: Header=BB123_10 Depth=1
	andq	%r13, %rdx
	cmpq	%rdx, %r12
	ja	.LBB123_20
# BB#18:                                # %lor.lhs.false
                                        #   in Loop: Header=BB123_10 Depth=1
	movq	%rbx, %rax
.LBB123_20:                             # %land.lhs.true61
                                        #   in Loop: Header=BB123_10 Depth=1
	movzbl	17(%rsi), %ecx
	andq	$32, %rcx
	je	.LBB123_22
# BB#21:                                # %land.lhs.true61
                                        #   in Loop: Header=BB123_10 Depth=1
	movq	$1, %r14
.LBB123_22:                             # %land.lhs.true61
                                        #   in Loop: Header=BB123_10 Depth=1
	testq	%rcx, %rcx
	je	.LBB123_24
# BB#23:                                # %land.lhs.true61
                                        #   in Loop: Header=BB123_10 Depth=1
	movq	%rbx, %rax
	jmp	.LBB123_24
.LBB123_25:                             # %land.lhs.true78
	movzbl	16(%r8), %ecx
	cmpq	$7, %rcx
	je	.LBB123_40
# BB#26:                                # %land.lhs.true84
	movq	40(%rbx), %rsi
	movq	40(%rsi), %rcx
	testq	%rcx, %rcx
	je	.LBB123_40
# BB#27:                                # %land.lhs.true89
	movl	16(%rcx), %edx
	andq	$524543, %rdx           # imm = 0x800FF
	cmpq	$25, %rdx
	jne	.LBB123_40
# BB#28:                                # %land.rhs.i
	cmpq	$0, 40(%rcx)
	jne	.LBB123_40
# BB#29:                                # %land.lhs.true6.i.i
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	32(%rcx), %rdi
	movl	16(%rsi), %esi
	movq	%rdi, %rax
	andq	%r13, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	callq	type_for_size
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	8(%rbx), %rcx
	movl	60(%rcx), %ecx
	movq	%rax, %rsi
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %rdx
	jae	.LBB123_30
# BB#31:                                # %land.lhs.true120
	testq	%r15, %r15
	movq	(%rsp), %rax            # 8-byte Reload
	jne	.LBB123_33
# BB#32:                                # %lor.lhs.false122
	movq	40(%rbx), %rcx
	movzbl	49(%rcx), %ecx
	testq	$16, %rcx
	jne	.LBB123_40
.LBB123_33:                             # %land.lhs.true131
	testq	%r13, %r14
	je	.LBB123_37
# BB#34:                                # %land.lhs.true131
	cmpq	%rdx, %r12
	jbe	.LBB123_37
# BB#35:                                # %lor.lhs.false136
	movq	40(%rbx), %rcx
	movl	16(%rcx), %edx
	testq	$8192, %rdx             # imm = 0x2000
	je	.LBB123_40
# BB#36:                                # %lor.lhs.false136
	testq	%rsi, %rsi
	jne	.LBB123_39
	jmp	.LBB123_40
.LBB123_30:
	movq	(%rsp), %rax            # 8-byte Reload
	jmp	.LBB123_40
.LBB123_37:                             # %land.lhs.true146
	testq	%rsi, %rsi
	je	.LBB123_40
# BB#38:                                # %land.lhs.true146.if.then149_crit_edge
	movq	40(%rbx), %rcx
.LBB123_39:                             # %if.then149
	movq	32(%rbx), %rdx
	movq	$39, %rdi
	xorq	%rax, %rax
	callq	build
	movl	16(%rbx), %edx
	movl	16(%rax), %ecx
	andq	$256, %rdx              # imm = 0x100
	andq	$-257, %rcx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rdx, %rcx
	movl	%ecx, 16(%rax)
	movl	16(%rbx), %esi
	andq	$2048, %rsi             # imm = 0x800
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rcx, %rdx
	orq	%rsi, %rdx
	movl	%edx, 16(%rax)
.LBB123_40:                             # %if.end177
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp986:
	.size	get_unwidened, .Ltmp986-get_unwidened
	.cfi_endproc

	.globl	get_narrower
	.align	16, 0x90
	.type	get_narrower,@function
get_narrower:                           # @get_narrower
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp993:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp994:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp995:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp996:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp997:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp998:
	.cfi_def_cfa_offset 64
.Ltmp999:
	.cfi_offset %rbx, -48
.Ltmp1000:
	.cfi_offset %r12, -40
.Ltmp1001:
	.cfi_offset %r13, -32
.Ltmp1002:
	.cfi_offset %r14, -24
.Ltmp1003:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movzbl	16(%r12), %eax
	movq	%rsi, %rdi
	cmpq	$115, %rax
	jne	.LBB124_1
# BB#2:                                 # %while.body.lr.ph.lr.ph
	movq	8(%r12), %rax
	movl	60(%rax), %edx
	movq	$1, %r14
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r12, %r13
.LBB124_3:                              # %while.body.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB124_4 Depth 2
	movq	%rdi, %r9
	movq	%r13, %r12
	.align	16, 0x90
.LBB124_4:                              # %while.body
                                        #   Parent Loop BB124_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%r12), %r13
	movq	8(%r13), %rdi
	movq	%rdx, %rax
	movl	60(%rdi), %edx
	andq	$511, %rax              # imm = 0x1FF
	movq	%rdx, %rcx
	andq	$511, %rcx              # imm = 0x1FF
	subq	%rcx, %rax
	js	.LBB124_5
# BB#6:                                 # %if.end
                                        #   in Loop: Header=BB124_4 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB124_10
# BB#7:                                 # %if.then16
                                        #   in Loop: Header=BB124_4 Depth=2
	movl	16(%rdi), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	testq	%r8, %r14
	jne	.LBB124_14
# BB#8:                                 # %if.else
                                        #   in Loop: Header=BB124_4 Depth=2
	movq	%rsi, %rax
	andq	%r8, %rax
	xorq	%r14, %r14
	cmpq	%rbx, %rax
	je	.LBB124_11
	jmp	.LBB124_9
	.align	16, 0x90
.LBB124_10:                             # %if.else34
                                        #   in Loop: Header=BB124_4 Depth=2
	testq	%r8, %r14
	jne	.LBB124_13
.LBB124_11:                             # %while.cond.backedge
                                        #   in Loop: Header=BB124_4 Depth=2
	movzbl	16(%r13), %eax
	xorq	%r14, %r14
	cmpq	$115, %rax
	movq	%r13, %r12
	je	.LBB124_4
	jmp	.LBB124_12
.LBB124_13:                             # %if.then36
                                        #   in Loop: Header=BB124_3 Depth=1
	movl	16(%rdi), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
.LBB124_14:                             # %while.cond.outer.backedge
                                        #   in Loop: Header=BB124_3 Depth=1
	movq	%r9, %rdi
	movzbl	16(%r13), %eax
	xorq	%r14, %r14
	cmpq	$115, %rax
	movq	%rbx, %rsi
	movq	%r13, %r12
	je	.LBB124_3
	jmp	.LBB124_15
.LBB124_1:
	movq	$1, %r14
	xorq	%rbx, %rbx
	movq	%r12, %r13
	jmp	.LBB124_15
.LBB124_5:
	movq	%rsi, %rbx
	movq	%r12, %r13
	movq	%r9, %rdi
	jmp	.LBB124_15
.LBB124_12:
	movq	%rsi, %rbx
	movq	%r13, %r12
	movq	%r9, %rdi
	jmp	.LBB124_15
.LBB124_9:
	movq	%rsi, %rbx
	movq	%r9, %rdi
.LBB124_15:                             # %while.end
	movl	16(%r13), %esi
	movq	%rsi, %rax
	andq	$255, %rax
	cmpq	$39, %rax
	jne	.LBB124_31
# BB#16:                                # %land.lhs.true
	movq	8(%r13), %rax
	movzbl	16(%rax), %eax
	cmpq	$7, %rax
	je	.LBB124_31
# BB#17:                                # %land.lhs.true58
	movq	40(%r13), %rax
	movq	40(%rax), %rax
	testq	%rax, %rax
	je	.LBB124_31
# BB#18:                                # %if.then63
	movl	16(%rax), %ecx
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$25, %rcx
	jne	.LBB124_22
# BB#19:                                # %land.rhs.i.i
	cmpq	$0, 40(%rax)
	jne	.LBB124_22
# BB#20:                                # %land.lhs.true6.i.i
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	32(%rax), %r15
	movq	$13, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	movq	%r15, %rdi
	callq	type_for_size
	movq	8(%r13), %rcx
	movl	60(%rcx), %ecx
	movq	%rax, %rsi
	andq	$511, %rcx              # imm = 0x1FF
	cmpq	%rcx, %r15
	jae	.LBB124_21
# BB#23:                                # %land.lhs.true85
	movq	40(%r13), %r8
	movzbl	49(%r8), %eax
	testq	$16, %rax
	je	.LBB124_25
# BB#24:
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB124_31
.LBB124_21:
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB124_31
.LBB124_25:                             # %land.lhs.true94
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	movq	8(%rsp), %rdi           # 8-byte Reload
	je	.LBB124_26
# BB#28:                                # %land.lhs.true106
	testq	%rsi, %rsi
	je	.LBB124_31
# BB#29:                                # %if.then111
	movl	16(%r8), %ebx
	movq	$13, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	jmp	.LBB124_30
.LBB124_26:                             # %lor.lhs.false
	movl	16(%r8), %edx
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	andq	%rbx, %rax
	cmpq	%rdx, %rax
	jne	.LBB124_31
# BB#27:                                # %lor.lhs.false
	testq	%rsi, %rsi
	je	.LBB124_31
.LBB124_30:                             # %if.end120
	movq	32(%r13), %rdx
	movq	%rdi, %r14
	movq	$39, %rdi
	xorq	%rax, %rax
	movq	%r8, %rcx
	callq	build
	movq	%r14, %rdi
	movq	%rax, %r12
	movl	16(%r13), %eax
	movl	16(%r12), %ecx
	andq	$256, %rax              # imm = 0x100
	andq	$-257, %rcx             # imm = 0xFFFFFFFFFFFFFEFF
	orq	%rax, %rcx
	movl	%ecx, 16(%r12)
	movl	16(%r13), %eax
	andq	$2048, %rax             # imm = 0x800
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%r12)
.LBB124_31:                             # %if.end148
	movl	%ebx, (%rdi)
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB124_22:                             # %if.else.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1004:
	.size	get_narrower, .Ltmp1004-get_narrower
	.cfi_endproc

	.globl	int_fits_type_p
	.align	16, 0x90
	.type	int_fits_type_p,@function
int_fits_type_p:                        # @int_fits_type_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp1007:
	.cfi_def_cfa_offset 16
.Ltmp1008:
	.cfi_offset %rbx, -16
	.align	16, 0x90
.LBB125_1:                              # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rbx
	movq	112(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB125_23
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB125_1 Depth=1
	movq	104(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB125_23
# BB#3:                                 # %land.lhs.true4
                                        #   in Loop: Header=BB125_1 Depth=1
	movzbl	16(%rsi), %eax
	cmpq	$25, %rax
	jne	.LBB125_23
# BB#4:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB125_1 Depth=1
	movzbl	16(%rcx), %eax
	cmpq	$25, %rax
	je	.LBB125_5
	.align	16, 0x90
.LBB125_23:                             # %if.else168
                                        #   in Loop: Header=BB125_1 Depth=1
	movzbl	16(%rbx), %eax
	cmpq	$6, %rax
	jne	.LBB125_25
# BB#24:                                # %land.lhs.true174
                                        #   in Loop: Header=BB125_1 Depth=1
	movq	8(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB125_1
.LBB125_25:                             # %if.else181
	callq	copy_node
	movq	%rbx, 8(%rax)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	force_fit_type
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB125_27
# BB#26:
	movabsq	$0, %rax
	popq	%rbx
	retq
.LBB125_5:                              # %if.then
	movzbl	17(%rbx), %eax
	movq	40(%rsi), %rbx
	movq	40(%rdi), %rdx
	testq	$32, %rax
	je	.LBB125_14
# BB#6:                                 # %if.then19
	xorq	%rax, %rax
	cmpq	%rdx, %rbx
	jb	.LBB125_28
# BB#7:                                 # %lor.lhs.false
	jne	.LBB125_9
# BB#8:                                 # %land.lhs.true36
	movq	32(%rsi), %rsi
	movq	32(%rdi), %rbx
	cmpq	%rbx, %rsi
	jb	.LBB125_28
.LBB125_9:                              # %land.lhs.true45
	movq	40(%rcx), %rsi
	cmpq	%rsi, %rdx
	jb	.LBB125_28
# BB#10:                                # %lor.lhs.false55
	jne	.LBB125_12
# BB#11:                                # %land.lhs.true65
	movq	32(%rdi), %rsi
	movq	32(%rcx), %rcx
	cmpq	%rcx, %rsi
	jb	.LBB125_28
.LBB125_12:                             # %land.rhs
	movq	$1, %rax
	testq	%rdx, %rdx
	jns	.LBB125_28
# BB#13:                                # %land.rhs79
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	andq	$32, %rax
	movq	$5, %rcx
	shrq	%cl, %rax
	popq	%rbx
	retq
.LBB125_14:                             # %if.else
	xorq	%rax, %rax
	cmpq	%rdx, %rbx
	jl	.LBB125_28
# BB#15:                                # %lor.lhs.false99
	jne	.LBB125_17
# BB#16:                                # %land.lhs.true109
	movq	32(%rsi), %rsi
	movq	32(%rdi), %rbx
	cmpq	%rbx, %rsi
	jb	.LBB125_28
.LBB125_17:                             # %land.lhs.true119
	movq	40(%rcx), %rsi
	cmpq	%rsi, %rdx
	jl	.LBB125_28
# BB#18:                                # %lor.lhs.false129
	jne	.LBB125_20
# BB#19:                                # %land.lhs.true139
	movq	32(%rdi), %rsi
	movq	32(%rcx), %rcx
	cmpq	%rcx, %rsi
	jb	.LBB125_28
.LBB125_20:                             # %land.rhs149
	movq	$1, %rax
	testq	%rdx, %rdx
	jns	.LBB125_28
# BB#21:                                # %land.rhs154
	movq	8(%rdi), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	jne	.LBB125_22
.LBB125_27:                             # %if.else181
	movabsq	$1, %rax
.LBB125_28:                             # %return
	popq	%rbx
	retq
.LBB125_22:
	movabsq	$0, %rax
	popq	%rbx
	retq
.Ltmp1009:
	.size	int_fits_type_p, .Ltmp1009-int_fits_type_p
	.cfi_endproc

	.globl	get_containing_scope
	.align	16, 0x90
	.type	get_containing_scope,@function
get_containing_scope:                   # @get_containing_scope
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$116, %rax
	jne	.LBB126_2
# BB#1:                                 # %cond.true
	addq	$144, %rdi
	movq	(%rdi), %rax
	retq
.LBB126_2:                              # %cond.false
	addq	$80, %rdi
	movq	(%rdi), %rax
	retq
.Ltmp1010:
	.size	get_containing_scope, .Ltmp1010-get_containing_scope
	.cfi_endproc

	.globl	decl_type_context
	.align	16, 0x90
	.type	decl_type_context,@function
decl_type_context:                      # @decl_type_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1012:
	.cfi_def_cfa_offset 16
	movq	80(%rdi), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB127_11
# BB#1:
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
.LBB127_2:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	16(%rcx), %esi
	cmpq	$19, %rsi
	jg	.LBB127_5
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB127_2 Depth=1
	cmpq	$4, %rsi
	jne	.LBB127_12
# BB#4:                                 # %if.then33
                                        #   in Loop: Header=BB127_2 Depth=1
	addq	$48, %rcx
	jmp	.LBB127_10
	.align	16, 0x90
.LBB127_5:                              # %while.body
                                        #   in Loop: Header=BB127_2 Depth=1
	leaq	-20(%rsi), %rdi
	andq	%rdx, %rdi
	cmpq	$3, %rdi
	jb	.LBB127_6
# BB#7:                                 # %while.body
                                        #   in Loop: Header=BB127_2 Depth=1
	cmpq	$30, %rsi
	je	.LBB127_9
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB127_2 Depth=1
	cmpq	$33, %rsi
	jne	.LBB127_12
.LBB127_9:                              # %if.then25
                                        #   in Loop: Header=BB127_2 Depth=1
	addq	$80, %rcx
.LBB127_10:                             # %while.cond.backedge
                                        #   in Loop: Header=BB127_2 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB127_2
.LBB127_11:                             # %return
	popq	%rdx
	retq
.LBB127_6:
	movq	%rcx, %rax
	popq	%rdx
	retq
.LBB127_12:                             # %if.else34
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.decl_type_context, %rcx
	movq	$4430, %rsi             # imm = 0x114E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1013:
	.size	decl_type_context, .Ltmp1013-decl_type_context
	.cfi_endproc

	.globl	get_callee_fndecl
	.align	16, 0x90
	.type	get_callee_fndecl,@function
get_callee_fndecl:                      # @get_callee_fndecl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1015:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %eax
	cmpq	$53, %rax
	jne	.LBB128_18
# BB#1:                                 # %while.cond.preheader
	movq	global_trees(%rip), %r9
	movq	32(%rdi), %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB128_2:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rdi
	movl	16(%rdi), %r10d
	movq	%r10, %rcx
	andq	$255, %rcx
	leaq	-114(%rcx), %rax
	andq	%r8, %rax
	cmpq	$2, %rax
	ja	.LBB128_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB128_2 Depth=1
	movq	32(%rdi), %rax
	cmpq	%r9, %rax
	je	.LBB128_5
# BB#4:                                 # %land.rhs
                                        #   in Loop: Header=BB128_2 Depth=1
	movq	8(%rdi), %rsi
	movq	8(%rax), %rdx
	movl	60(%rsi), %esi
	movl	60(%rdx), %edx
	xorq	%rsi, %rdx
	testq	$65024, %rdx            # imm = 0xFE00
	je	.LBB128_2
.LBB128_5:                              # %while.end
	movzbl	tree_code_type(%rcx), %eax
	cmpq	$100, %rax
	movabsq	$0, %r9
	movabsq	$1, %r8
	movq	%r9, %rsi
	jne	.LBB128_7
# BB#6:                                 # %while.end
	movq	%r8, %rsi
.LBB128_7:                              # %while.end
	cmpq	$30, %rcx
	je	.LBB128_9
# BB#8:                                 # %while.end
	movq	%r8, %r9
.LBB128_9:                              # %while.end
	movq	%r10, %rax
	andq	$4096, %rax             # imm = 0x1000
	movq	$12, %rcx
	shrq	%cl, %rax
	testq	$2048, %r10             # imm = 0x800
	jne	.LBB128_14
# BB#10:                                # %while.end
	andq	%r9, %rsi
	andq	%rax, %rsi
	je	.LBB128_14
# BB#11:                                # %land.lhs.true64
	movq	104(%rdi), %rax
	testq	%rax, %rax
	je	.LBB128_13
# BB#12:                                # %land.lhs.true64
	movq	%rax, %rdi
.LBB128_13:                             # %land.lhs.true64
	movl	16(%rdi), %r10d
.LBB128_14:                             # %if.end69
	andq	$255, %r10
	cmpq	$121, %r10
	jne	.LBB128_16
# BB#15:                                # %land.lhs.true76
	movq	32(%rdi), %rax
	movzbl	16(%rax), %ecx
	cmpq	$30, %rcx
	je	.LBB128_17
.LBB128_16:                             # %if.end90
	xorq	%rax, %rax
.LBB128_17:                             # %return
	popq	%rdx
	retq
.LBB128_18:                             # %if.then
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.get_callee_fndecl, %rcx
	movq	$4448, %rsi             # imm = 0x1160
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1016:
	.size	get_callee_fndecl, .Ltmp1016-get_callee_fndecl
	.cfi_endproc

	.globl	print_obstack_statistics
	.align	16, 0x90
	.type	print_obstack_statistics,@function
print_obstack_statistics:               # @print_obstack_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rsi), %rcx
	movq	%rdi, %rdx
	movq	24(%rsi), %r9
	movq	8(%rcx), %rax
	leaq	16(%rcx), %rcx
	subq	%rcx, %r9
	testq	%rax, %rax
	movq	$1, %r8
	je	.LBB129_2
	.align	16, 0x90
.LBB129_1:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	16(%rax), %rsi
	movq	$32, %rcx
	shlq	%cl, %r9
	movq	$32, %rcx
	sarq	%cl, %r9
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	incq	%r8
	subq	%rsi, %r9
	addq	%rcx, %r9
	testq	%rax, %rax
	jne	.LBB129_1
.LBB129_2:                              # %while.end
	movq	stderr(%rip), %rdi
	movabsq	$.L.str162, %rsi
	xorq	%rax, %rax
	movq	%r9, %rcx
	jmp	fprintf  # TAILCALL
.Ltmp1017:
	.size	print_obstack_statistics, .Ltmp1017-print_obstack_statistics
	.cfi_endproc

	.globl	dump_tree_statistics
	.align	16, 0x90
	.type	dump_tree_statistics,@function
dump_tree_statistics:                   # @dump_tree_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1023:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1024:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1025:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1026:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1027:
	.cfi_def_cfa_offset 48
.Ltmp1028:
	.cfi_offset %rbx, -40
.Ltmp1029:
	.cfi_offset %r12, -32
.Ltmp1030:
	.cfi_offset %r14, -24
.Ltmp1031:
	.cfi_offset %r15, -16
	movq	stderr(%rip), %r14
	movabsq	$.L.str163, %rdi
	movq	$25, %rsi
	movq	$1, %rdx
	movq	$1, %r15
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str164, %rdi
	movq	$25, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	permanent_obstack+8(%rip), %rcx
	movq	permanent_obstack+24(%rip), %rbx
	movq	8(%rcx), %rax
	leaq	16(%rcx), %rcx
	subq	%rcx, %rbx
	jmp	.LBB130_2
	.align	16, 0x90
.LBB130_1:                              # %while.body.i
                                        #   in Loop: Header=BB130_2 Depth=1
	leaq	16(%rax), %rdx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	(%rax), %rcx
	movq	8(%rax), %rax
	incq	%r15
	subq	%rdx, %rbx
	addq	%rcx, %rbx
.LBB130_2:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB130_1
# BB#3:                                 # %print_obstack_statistics.exit
	movabsq	$.L.str162, %rsi
	movabsq	$.L.str165, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rcx
	movq	%r15, %r8
	callq	fprintf
	movq	type_hash_table(%rip), %rdi
	callq	htab_size
	movq	type_hash_table(%rip), %rdi
	movq	%rax, %r15
	callq	htab_elements
	movq	type_hash_table(%rip), %rdi
	movq	%rax, %r12
	callq	htab_collisions
	movq	%rax, %rbx
	movabsq	$.L.str171, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	%rbx, %r8
	callq	fprintf
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmpq	*lang_hooks+104(%rip)  # TAILCALL
.Ltmp1032:
	.size	dump_tree_statistics, .Ltmp1032-dump_tree_statistics
	.cfi_endproc

	.globl	clean_symbol_name
	.align	16, 0x90
	.type	clean_symbol_name,@function
clean_symbol_name:                      # @clean_symbol_name
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB131_6
# BB#1:                                 # %for.body.preheader
	incq	%rdi
	movq	$95, %rcx
	.align	16, 0x90
.LBB131_2:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %edx
	testq	$140, %rdx
	jne	.LBB131_5
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB131_2 Depth=1
	cmpq	$46, %rax
	je	.LBB131_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB131_2 Depth=1
	movb	%cl, -1(%rdi)
.LBB131_5:                              # %for.inc
                                        #   in Loop: Header=BB131_2 Depth=1
	movzbl	(%rdi), %eax
	incq	%rdi
	testq	%rax, %rax
	jne	.LBB131_2
.LBB131_6:                              # %for.end
	retq
.Ltmp1033:
	.size	clean_symbol_name, .Ltmp1033-clean_symbol_name
	.cfi_endproc

	.globl	get_file_function_name_long
	.align	16, 0x90
	.type	get_file_function_name_long,@function
get_file_function_name_long:            # @get_file_function_name_long
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp1037:
	.cfi_def_cfa_offset 16
.Ltmp1038:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp1039:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$152, %rsp
.Ltmp1040:
	.cfi_offset %rbx, -56
.Ltmp1041:
	.cfi_offset %r12, -48
.Ltmp1042:
	.cfi_offset %r13, -40
.Ltmp1043:
	.cfi_offset %r14, -32
.Ltmp1044:
	.cfi_offset %r15, -24
	movq	first_global_object_name(%rip), %r12
	movq	%rdi, %r14
	testq	%r12, %r12
	jne	.LBB132_10
# BB#1:                                 # %if.else
	movq	weak_global_object_name(%rip), %r15
	movq	main_input_filename(%rip), %r13
	testq	%r15, %r15
	jne	.LBB132_3
# BB#2:
	movabsq	$.L.str166, %r15
.LBB132_3:                              # %if.else
	testq	%r13, %r13
	jne	.LBB132_5
# BB#4:
	movq	input_filename(%rip), %r13
.LBB132_5:                              # %if.else
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r13, %rdi
	callq	strlen
	leaq	22(%rbx,%rax), %rax
	andq	$-16, %rax
	movq	%rsp, %r12
	subq	%rax, %r12
	movq	%r12, %rsp
	movabsq	$.L.str167, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	%r15, %rdx
	movq	%r13, %rcx
	callq	sprintf
	movq	append_random_chars.value(%rip), %r15
	testq	%r15, %r15
	jne	.LBB132_9
# BB#6:                                 # %if.then.i
	movq	main_input_filename(%rip), %rdi
	leaq	-184(%rbp), %rsi
	callq	stat
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	$1, %r15
	js	.LBB132_8
# BB#7:                                 # %if.else.i
	movq	-184(%rbp), %rax
	movq	-176(%rbp), %rcx
	movq	-96(%rbp), %r15
	xorq	%rax, %rcx
	xorq	%rcx, %r15
.LBB132_8:                              # %if.end.i
	movq	%r15, append_random_chars.value(%rip)
	xorq	%rbx, %rbx
.LBB132_9:                              # %append_random_chars.exit
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r9
	movq	$1, %rcx
	movq	%r15, %rax
	shrq	%cl, %rax
	movabsq	$-8925843906633654007, %r8 # imm = 0x8421084210842109
	mulq	%r8
	movq	$4, %rcx
	movq	%rbx, %r10
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	imulq	$62, %rbx, %rax
	negq	%rax
	movzbl	append_random_chars.letters(%r15,%rax), %eax
	movb	%al, (%r12,%r9)
	movq	$5, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	mulq	%r8
	movq	$4, %rcx
	shrq	%cl, %rdx
	imulq	$62, %rdx, %rax
	negq	%rax
	movzbl	append_random_chars.letters(%rbx,%rax), %eax
	movb	%al, 1(%r12,%r9)
	movabsq	$2457006494729263899, %rcx # imm = 0x22190A63A12A5B1B
	movq	%r15, %rax
	mulq	%rcx
	movq	$9, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movq	$10, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	mulq	%r8
	movq	$4, %rcx
	shrq	%cl, %rdx
	imulq	$62, %rdx, %rax
	subq	%rax, %rbx
	movzbl	append_random_chars.letters(%rbx), %eax
	movb	%al, 2(%r12,%r9)
	movq	$3, %rcx
	movq	%r15, %rax
	shrq	%cl, %rax
	movabsq	$2536264768752788541, %rcx # imm = 0x23329F5E95D9243D
	mulq	%rcx
	movq	$12, %rcx
	movq	%rdx, %rbx
	shrq	%cl, %rbx
	movq	$13, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	mulq	%r8
	movq	$4, %rcx
	shrq	%cl, %rdx
	imulq	$62, %rdx, %rax
	subq	%rax, %rbx
	movabsq	$5236159522586402149, %rcx # imm = 0x48AA9357EB0A9D65
	movq	%r15, %rax
	mulq	%rcx
	movq	$22, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	movq	$23, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rax
	mulq	%r8
	movq	$4, %rcx
	shrq	%cl, %rdx
	imulq	$62, %rdx, %rax
	subq	%rax, %rdi
	movabsq	$2702533947141368851, %rcx # imm = 0x2581544E68CBAC13
	movq	%r15, %rax
	mulq	%rcx
	movq	$27, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	$28, %rcx
	shrq	%cl, %rdx
	movzbl	append_random_chars.letters(%rbx), %ebx
	movzbl	append_random_chars.letters(%rdi), %edi
	movq	%rdx, %rax
	mulq	%r8
	movq	$4, %rcx
	shrq	%cl, %rdx
	imulq	$62, %rdx, %rax
	subq	%rax, %rsi
	movzbl	append_random_chars.letters(%rsi), %eax
	movb	%bl, 3(%r12,%r9)
	movb	%dil, 4(%r12,%r9)
	movb	%al, 5(%r12,%r9)
	movb	%r10b, 6(%r12,%r9)
.LBB132_10:                             # %if.end9
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	strlen
	leaq	30(%rbx,%rax), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movabsq	$.L.str168, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	movq	%r12, %rcx
	callq	sprintf
	movq	first_global_object_name(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB132_17
# BB#11:                                # %if.then15
	movzbl	11(%rbx), %edx
	testq	%rdx, %rdx
	je	.LBB132_17
# BB#12:                                # %for.body.i.preheader
	leaq	12(%rbx), %rax
	movq	$95, %rcx
	.align	16, 0x90
.LBB132_13:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdx
	movzbl	_sch_istable(%rdx,%rdx), %esi
	testq	$140, %rsi
	jne	.LBB132_16
# BB#14:                                # %for.body.i
                                        #   in Loop: Header=BB132_13 Depth=1
	cmpq	$46, %rdx
	je	.LBB132_16
# BB#15:                                # %if.then.i31
                                        #   in Loop: Header=BB132_13 Depth=1
	movb	%cl, -1(%rax)
.LBB132_16:                             # %for.inc.i
                                        #   in Loop: Header=BB132_13 Depth=1
	movzbl	(%rax), %edx
	incq	%rax
	testq	%rdx, %rdx
	jne	.LBB132_13
.LBB132_17:                             # %if.end16
	movq	%rbx, %rdi
	callq	get_identifier
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp1045:
	.size	get_file_function_name_long, .Ltmp1045-get_file_function_name_long
	.cfi_endproc

	.globl	get_file_function_name
	.align	16, 0x90
	.type	get_file_function_name,@function
get_file_function_name:                 # @get_file_function_name
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1047:
	.cfi_def_cfa_offset 16
	movb	%dil, 6(%rsp)
	xorq	%rax, %rax
	movb	%al, 7(%rsp)
	leaq	6(%rsp), %rdi
	callq	get_file_function_name_long
	popq	%rdx
	retq
.Ltmp1048:
	.size	get_file_function_name, .Ltmp1048-get_file_function_name
	.cfi_endproc

	.globl	get_set_constructor_bits
	.align	16, 0x90
	.type	get_set_constructor_bits,@function
get_set_constructor_bits:               # @get_set_constructor_bits
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1055:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1056:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1057:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1058:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1059:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp1060:
	.cfi_def_cfa_offset 80
.Ltmp1061:
	.cfi_offset %rbx, -48
.Ltmp1062:
	.cfi_offset %r12, -40
.Ltmp1063:
	.cfi_offset %r13, -32
.Ltmp1064:
	.cfi_offset %r14, -24
.Ltmp1065:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	8(%r14), %rax
	movq	24(%rax), %rax
	movq	104(%rax), %rax
	movl	16(%rax), %ecx
	movq	%rdx, %rbx
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	andq	$524543, %rcx           # imm = 0x800FF
	cmpq	$25, %rcx
	jne	.LBB134_15
# BB#1:                                 # %land.rhs.i.i
	movq	40(%rax), %rcx
	cmpq	$-1, %rcx
	jne	.LBB134_2
# BB#13:                                # %land.lhs.true16.i.i
	movq	32(%rax), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jns	.LBB134_15
# BB#14:                                # %land.lhs.true21.i.i
	movq	8(%rax), %rax
	movzbl	17(%rax), %eax
	testq	$32, %rax
	je	.LBB134_4
	jmp	.LBB134_15
.LBB134_2:                              # %land.rhs.i.i
	testq	%rcx, %rcx
	jne	.LBB134_15
# BB#3:                                 # %land.lhs.true6.i.i
	movq	32(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	js	.LBB134_15
.LBB134_4:                              # %tree_low_cst.exit
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB134_6
# BB#5:                                 # %for.body.lr.ph
	leaq	-1(%rbx), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	incq	%rdx
	xorq	%rsi, %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	memset
.LBB134_6:                              # %for.end
	movq	%rbx, %rax
	movq	40(%r14), %rbx
	xorq	%r13, %r13
	testq	%rbx, %rbx
	je	.LBB134_52
# BB#7:                                 # %for.body6.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	xorq	%r13, %r13
	movq	$2, %r14
	jmp	.LBB134_8
.LBB134_27:                             # %land.rhs.i.i151
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rcx, %rcx
	jne	.LBB134_15
# BB#28:                                # %land.lhs.true6.i.i154
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rax, %rax
	js	.LBB134_15
.LBB134_33:                             # %tree_low_cst.exit170
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rdx, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	js	.LBB134_41
# BB#34:                                # %lor.lhs.false31
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	24(%rsp), %rsi          # 8-byte Reload
	subq	%rsi, %rax
	js	.LBB134_41
# BB#35:                                # %lor.lhs.false31
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	%rcx, %rdx
	jge	.LBB134_41
# BB#36:                                # %lor.lhs.false31
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	%rcx, %rax
	jge	.LBB134_41
# BB#37:                                # %for.cond42.preheader
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	%rax, %rdx
	jg	.LBB134_51
# BB#38:                                # %for.body45.lr.ph
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rdx), %rdi
	cmpq	%rax, %rdx
	jge	.LBB134_40
# BB#39:                                # %for.body45.lr.ph
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	%rax, %rdx
.LBB134_40:                             # %for.body45.lr.ph
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	addq	%rax, %rdx
	subq	%r8, %rdx
	movq	$1, %rsi
	callq	memset
	jmp	.LBB134_51
	.align	16, 0x90
.LBB134_8:                              # %for.body6
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %r12
	movl	16(%r12), %eax
	andq	$524543, %rax           # imm = 0x800FF
	cmpq	$25, %rax
	jne	.LBB134_30
# BB#9:                                 # %land.rhs.i
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	40(%r12), %rcx
	testq	%rcx, %rcx
	jne	.LBB134_10
# BB#17:                                # %host_integerp.exit
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	32(%r12), %rax
	testq	%rax, %rax
	js	.LBB134_30
	jmp	.LBB134_18
	.align	16, 0x90
.LBB134_10:                             # %land.rhs.i
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	$-1, %rcx
	jne	.LBB134_30
# BB#11:                                # %land.lhs.true16.i
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	32(%r12), %rax
	testq	%rax, %rax
	jns	.LBB134_30
# BB#12:                                # %land.lhs.true21.i
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%r12), %rdx
	movzbl	17(%rdx), %edx
	testq	$32, %rdx
	jne	.LBB134_30
.LBB134_18:                             # %lor.lhs.false
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	24(%rbx), %rdx
	testq	%rdx, %rdx
	jne	.LBB134_19
# BB#42:                                # %land.rhs.i.i104
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	$-1, %rcx
	jne	.LBB134_43
# BB#45:                                # %land.lhs.true16.i.i112
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rax, %rax
	jns	.LBB134_15
# BB#46:                                # %land.lhs.true21.i.i118
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%r12), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	je	.LBB134_47
	jmp	.LBB134_15
.LBB134_19:                             # %land.lhs.true
                                        #   in Loop: Header=BB134_8 Depth=1
	movl	16(%rdx), %esi
	andq	$524543, %rsi           # imm = 0x800FF
	cmpq	$25, %rsi
	jne	.LBB134_30
# BB#20:                                # %land.rhs.i129
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	40(%rdx), %rdi
	testq	%rdi, %rdi
	jne	.LBB134_21
# BB#29:                                # %host_integerp.exit145
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	32(%rdx), %r8
	testq	%r8, %r8
	jns	.LBB134_26
	jmp	.LBB134_30
.LBB134_43:                             # %land.rhs.i.i104
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rcx, %rcx
	jne	.LBB134_15
# BB#44:                                # %land.lhs.true6.i.i107
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rax, %rax
	js	.LBB134_15
.LBB134_47:                             # %tree_low_cst.exit123
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	subq	%rcx, %rax
	js	.LBB134_49
# BB#48:                                # %tree_low_cst.exit123
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	%rcx, %rax
	jge	.LBB134_49
# BB#50:                                # %if.end62
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	$1, %rdx
	movb	%dl, (%rcx,%rax)
	jmp	.LBB134_51
.LBB134_21:                             # %land.rhs.i129
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	$-1, %rdi
	jne	.LBB134_30
# BB#22:                                # %land.lhs.true16.i137
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	32(%rdx), %r8
	testq	%r8, %r8
	jns	.LBB134_30
# BB#23:                                # %land.lhs.true21.i143
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%rdx), %rsi
	movzbl	17(%rsi), %esi
	testq	$32, %rsi
	jne	.LBB134_30
# BB#24:                                # %land.rhs.i.i177
                                        #   in Loop: Header=BB134_8 Depth=1
	cmpq	$-1, %rdi
	jne	.LBB134_15
# BB#25:                                # %land.lhs.true21.i.i191
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%rdx), %rdx
	movzbl	17(%rdx), %edx
	testq	$32, %rdx
	jne	.LBB134_15
.LBB134_26:                             # %land.rhs.i.i151
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	%r8, %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	subq	%rsi, %rdx
	cmpq	$-1, %rcx
	jne	.LBB134_27
# BB#31:                                # %land.lhs.true16.i.i159
                                        #   in Loop: Header=BB134_8 Depth=1
	testq	%rax, %rax
	jns	.LBB134_15
# BB#32:                                # %land.lhs.true21.i.i165
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	8(%r12), %rcx
	movzbl	17(%rcx), %ecx
	testq	$32, %rcx
	je	.LBB134_33
	jmp	.LBB134_15
	.align	16, 0x90
.LBB134_30:                             # %if.then
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	24(%rbx), %r15
	movq	$40, %rdi
	callq	ggc_alloc
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movl	%r14d, 16(%rax)
	movq	%r13, (%rax)
	movq	%r15, 24(%rax)
	movq	%r12, 32(%rax)
	movq	%rax, %r13
.LBB134_51:                             # %for.inc66
                                        #   in Loop: Header=BB134_8 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB134_8
	jmp	.LBB134_52
.LBB134_49:                             # %if.then61
	movabsq	$.L.str169, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	error
.LBB134_52:                             # %return
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB134_41:                             # %if.then41
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.get_set_constructor_bits, %rcx
	movq	$4718, %rsi             # imm = 0x126E
	jmp	.LBB134_16
.LBB134_15:                             # %if.else.i
	movabsq	$.L.str147, %rax
	movabsq	$.L__FUNCTION__.tree_low_cst, %rcx
	movq	$3489, %rsi             # imm = 0xDA1
.LBB134_16:                             # %if.else.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp1066:
	.size	get_set_constructor_bits, .Ltmp1066-get_set_constructor_bits
	.cfi_endproc

	.globl	get_set_constructor_bytes
	.align	16, 0x90
	.type	get_set_constructor_bytes,@function
get_set_constructor_bytes:              # @get_set_constructor_bytes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp1070:
	.cfi_def_cfa_offset 16
.Ltmp1071:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp1072:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp1073:
	.cfi_offset %rbx, -56
.Ltmp1074:
	.cfi_offset %r12, -48
.Ltmp1075:
	.cfi_offset %r13, -40
.Ltmp1076:
	.cfi_offset %r14, -32
.Ltmp1077:
	.cfi_offset %r15, -24
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	$32, %rcx
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	movq	%r12, %r14
	shlq	%cl, %r14
	movq	$35, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %r15
	subq	%rax, %r15
	movq	%r15, %rsp
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	get_set_constructor_bits
	movq	%rax, -48(%rbp)         # 8-byte Spill
	testq	%rbx, %rbx
	jle	.LBB135_9
# BB#1:                                 # %for.body5.lr.ph
	decq	%r12
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%r12, %rdx
	incq	%rdx
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	callq	memset
	movq	$1, %rax
	.align	16, 0x90
.LBB135_2:                              # %for.body5
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r15,%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB135_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB135_2 Depth=1
	movzbl	(%r13), %edx
	movq	$1, %rsi
	movq	%rbx, %rcx
	shlq	%cl, %rsi
	orq	%rsi, %rdx
	movb	%dl, (%r13)
.LBB135_4:                              # %if.end
                                        #   in Loop: Header=BB135_2 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rbx
	xorq	%rdx, %rdx
	cmpq	$6, %rsi
	leaq	1(%r13), %rsi
	jg	.LBB135_6
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB135_2 Depth=1
	movq	%r13, %rsi
.LBB135_6:                              # %if.end
                                        #   in Loop: Header=BB135_2 Depth=1
	jg	.LBB135_8
# BB#7:                                 # %if.end
                                        #   in Loop: Header=BB135_2 Depth=1
	movq	%rbx, %rdx
.LBB135_8:                              # %if.end
                                        #   in Loop: Header=BB135_2 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	incq	%rax
	cmpq	%rbx, %rdi
	movq	%rdx, %rbx
	movq	%rsi, %r13
	jl	.LBB135_2
.LBB135_9:                              # %for.end17
	movq	-48(%rbp), %rax         # 8-byte Reload
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp1078:
	.size	get_set_constructor_bytes, .Ltmp1078-get_set_constructor_bytes
	.cfi_endproc

	.globl	build_common_tree_nodes
	.align	16, 0x90
	.type	build_common_tree_nodes,@function
build_common_tree_nodes:                # @build_common_tree_nodes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1085:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1086:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1087:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1088:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1089:
	.cfi_def_cfa_offset 48
.Ltmp1090:
	.cfi_offset %rbx, -48
.Ltmp1091:
	.cfi_offset %r12, -40
.Ltmp1092:
	.cfi_offset %r13, -32
.Ltmp1093:
	.cfi_offset %r14, -24
.Ltmp1094:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	callq	make_node
	movq	%rax, global_trees(%rip)
	movq	%rax, 8(%rax)
	callq	initialize_sizetypes
	movq	$8, %rdi
	callq	make_signed_type
	movq	%rax, integer_types+8(%rip)
	movq	$8, %rdi
	callq	make_unsigned_type
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	movq	%rax, integer_types+16(%rip)
	movq	$8, %rdi
	je	.LBB136_2
# BB#1:                                 # %cond.true
	callq	make_signed_type
	jmp	.LBB136_3
.LBB136_2:                              # %cond.false
	callq	make_unsigned_type
.LBB136_3:                              # %cond.end
	movq	%rax, integer_types(%rip)
	movq	$16, %rdi
	callq	make_signed_type
	movq	%rax, integer_types+24(%rip)
	movq	$16, %rdi
	callq	make_unsigned_type
	movq	%rax, integer_types+32(%rip)
	movq	$32, %rdi
	callq	make_signed_type
	movq	%rax, integer_types+40(%rip)
	movq	$32, %rdi
	callq	make_unsigned_type
	movl	target_flags(%rip), %edi
	movq	%rax, integer_types+48(%rip)
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	callq	make_signed_type
	movl	target_flags(%rip), %edi
	movq	%rax, integer_types+56(%rip)
	movq	$20, %rcx
	shrq	%cl, %rdi
	andq	$32, %rdi
	addq	$32, %rdi
	callq	make_unsigned_type
	movq	%rax, integer_types+64(%rip)
	movq	$64, %rdi
	callq	make_signed_type
	movq	%rax, integer_types+72(%rip)
	movq	$64, %rdi
	callq	make_unsigned_type
	movzbl	mode_bitsize+4(%rip), %r14d
	movq	%rax, integer_types+80(%rip)
	movq	%r14, %rdi
	callq	make_signed_type
	movzbl	mode_bitsize+6(%rip), %r15d
	movq	%rax, global_trees+8(%rip)
	movq	%r15, %rdi
	callq	make_signed_type
	movzbl	mode_bitsize+8(%rip), %r12d
	movq	%rax, global_trees+16(%rip)
	movq	%r12, %rdi
	callq	make_signed_type
	movzbl	mode_bitsize+10(%rip), %r13d
	movq	%rax, global_trees+24(%rip)
	movq	%r13, %rdi
	callq	make_signed_type
	movzbl	mode_bitsize+12(%rip), %ebx
	movq	%rax, global_trees+32(%rip)
	movq	%rbx, %rdi
	callq	make_signed_type
	movq	%rax, global_trees+40(%rip)
	movq	%r14, %rdi
	callq	make_unsigned_type
	movq	%rax, global_trees+48(%rip)
	movq	%r15, %rdi
	callq	make_unsigned_type
	movq	%rax, global_trees+56(%rip)
	movq	%r12, %rdi
	callq	make_unsigned_type
	movq	%rax, global_trees+64(%rip)
	movq	%r13, %rdi
	callq	make_unsigned_type
	movq	%rax, global_trees+72(%rip)
	movq	%rbx, %rdi
	callq	make_unsigned_type
	movq	%rax, global_trees+80(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp1095:
	.size	build_common_tree_nodes, .Ltmp1095-build_common_tree_nodes
	.cfi_endproc

	.globl	build_common_tree_nodes_2
	.align	16, 0x90
	.type	build_common_tree_nodes_2,@function
build_common_tree_nodes_2:              # @build_common_tree_nodes_2
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1102:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1103:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp1104:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp1105:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp1106:
	.cfi_def_cfa_offset 48
.Ltmp1107:
	.cfi_offset %rbx, -48
.Ltmp1108:
	.cfi_offset %r12, -40
.Ltmp1109:
	.cfi_offset %r13, -32
.Ltmp1110:
	.cfi_offset %r14, -24
.Ltmp1111:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$25, %rdi
	callq	make_node
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, global_trees+88(%rip)
	movq	$25, %rdi
	callq	make_node
	movq	$1, 32(%rax)
	movq	$0, 40(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, global_trees+96(%rip)
	movq	$25, %rdi
	callq	make_node
	movq	$-1, 40(%rax)
	movq	$-1, 32(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, global_trees+104(%rip)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, global_trees+120(%rip)
	movq	$1, %rdi
	xorq	%rsi, %rsi
	callq	size_int_wide
	movq	%rax, global_trees+128(%rip)
	xorq	%rdi, %rdi
	movq	$3, %rsi
	callq	size_int_wide
	movq	%rax, global_trees+136(%rip)
	movq	$1, %rdi
	movq	$3, %rsi
	callq	size_int_wide
	movq	%rax, global_trees+144(%rip)
	movq	$8, %rdi
	movq	$3, %rsi
	movq	$8, %rbx
	callq	size_int_wide
	movq	%rax, global_trees+152(%rip)
	movq	$5, %rdi
	callq	make_node
	movq	%rax, global_trees+216(%rip)
	movq	%rax, %rdi
	callq	layout_type
	movq	global_trees+216(%rip), %rax
	movl	%ebx, 64(%rax)
	movl	60(%rax), %ecx
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	movl	%ecx, 60(%rax)
	movq	$25, %rdi
	callq	make_node
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, global_trees+112(%rip)
	movq	global_trees+216(%rip), %r15
	movq	72(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB137_2
# BB#1:                                 # %if.end.i
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r15, 8(%rbx)
	movq	%rbx, 72(%r15)
	movq	%rbx, %rdi
	callq	layout_type
	movq	global_trees+112(%rip), %rax
.LBB137_2:                              # %build_pointer_type.exit
	movq	%rbx, 8(%rax)
	movq	global_trees+112(%rip), %rax
	movq	8(%rax), %rdi
	callq	layout_type
	movq	global_trees+216(%rip), %r15
	movq	72(%r15), %rbx
	testq	%rbx, %rbx
	jne	.LBB137_4
# BB#3:                                 # %if.end.i95
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %rbx
	movq	%r15, 8(%rbx)
	movq	%rbx, 72(%r15)
	movq	%rbx, %rdi
	callq	layout_type
	movq	global_trees+216(%rip), %r15
.LBB137_4:                              # %build_pointer_type.exit97
	movq	%rbx, global_trees+224(%rip)
	movq	128(%r15), %r12
	testq	%r12, %r12
	je	.LBB137_13
# BB#5:                                 # %for.body.lr.ph.i.i
	movabsq	$0, %r9
	movabsq	$1, %r8
	movabsq	$4294967295, %r10       # imm = 0xFFFFFFFF
	movq	%r12, %r13
	.align	16, 0x90
.LBB137_6:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r13), %eax
	movq	$12, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	movq	$10, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movl	60(%r13), %edi
	movq	$19, %rcx
	shrq	%cl, %rdi
	andq	$255, %rax
	xorq	%rsi, %rsi
	cmpq	$20, %rax
	jne	.LBB137_9
# BB#7:                                 # %land.rhs.i.i
                                        #   in Loop: Header=BB137_6 Depth=1
	cmpq	$0, 8(%r13)
	movq	%r9, %rsi
	je	.LBB137_9
# BB#8:                                 # %land.rhs.i.i
                                        #   in Loop: Header=BB137_6 Depth=1
	movq	%r8, %rsi
.LBB137_9:                              # %land.end.i.i
                                        #   in Loop: Header=BB137_6 Depth=1
	andq	$1, %rbx
	andq	$2, %rdx
	orq	%rbx, %rdx
	andq	$4, %rdi
	orq	%rdx, %rdi
	movq	$3, %rcx
	shlq	%cl, %rsi
	orq	%rdi, %rsi
	andq	%r10, %rsi
	cmpq	$1, %rsi
	jne	.LBB137_11
# BB#10:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB137_6 Depth=1
	movq	96(%r13), %rax
	movq	96(%r15), %rcx
	cmpq	%rcx, %rax
	je	.LBB137_12
.LBB137_11:                             # %for.inc.i.i
                                        #   in Loop: Header=BB137_6 Depth=1
	movq	120(%r13), %r13
	testq	%r13, %r13
	jne	.LBB137_6
	jmp	.LBB137_13
.LBB137_12:                             # %get_qualified_type.exit.i
	testq	%r13, %r13
	je	.LBB137_13
# BB#14:                                # %build_qualified_type.exit
	movq	72(%r13), %r15
	testq	%r15, %r15
	jne	.LBB137_16
	jmp	.LBB137_15
.LBB137_13:                             # %build_qualified_type.exit.thread
	movq	%r15, %rdi
	callq	copy_node
	movq	%rax, %r13
	movq	$0, 80(%r13)
	movq	$0, 72(%r13)
	movq	120(%r12), %rax
	movq	%rax, 120(%r13)
	movq	%r13, 120(%r12)
	movl	16(%r13), %eax
	movabsq	$4294961151, %rcx       # imm = 0xFFFFE7FF
	andq	%rax, %rcx
	orq	$4096, %rcx             # imm = 0x1000
	movl	%ecx, 16(%r13)
	movl	60(%r13), %eax
	movabsq	$4292870143, %rcx       # imm = 0xFFDFFFFF
	andq	%rax, %rcx
	movl	%ecx, 60(%r13)
.LBB137_15:                             # %if.end.i102
	movq	$13, %rdi
	callq	make_node
	movq	%rax, %r15
	movq	%r13, 8(%r15)
	movq	%r15, 72(%r13)
	movq	%r15, %rdi
	callq	layout_type
.LBB137_16:                             # %build_pointer_type.exit104
	movq	%r15, global_trees+232(%rip)
	movq	$7, %rdi
	callq	make_node
	movq	%rax, global_trees+192(%rip)
	movl	60(%rax), %ecx
	movabsq	$4294966784, %rbx       # imm = 0xFFFFFE00
	andq	%rbx, %rcx
	orq	$32, %rcx
	movl	%ecx, 60(%rax)
	movq	global_trees+192(%rip), %rdi
	callq	layout_type
	movq	$7, %rdi
	callq	make_node
	movq	%rax, global_trees+200(%rip)
	movl	60(%rax), %ecx
	andq	%rbx, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %r14
	movq	$64, %rdx
	je	.LBB137_18
# BB#17:                                # %build_pointer_type.exit104
	movq	$32, %rdx
.LBB137_18:                             # %build_pointer_type.exit104
	orq	%rdx, %rcx
	movl	%ecx, 60(%rax)
	movq	global_trees+200(%rip), %rdi
	callq	layout_type
	movq	$7, %rdi
	callq	make_node
	movq	%rax, global_trees+208(%rip)
	movl	target_flags(%rip), %edx
	movq	$19, %rcx
	movl	60(%rax), %esi
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$96, %rdx
	andq	%rbx, %rsi
	orq	%rdx, %rsi
	movl	%esi, 60(%rax)
	movq	global_trees+208(%rip), %rdi
	callq	layout_type
	movq	$8, %rdi
	callq	make_node
	movq	integer_types+40(%rip), %rcx
	movq	%rax, global_trees+160(%rip)
	movq	%rcx, 8(%rax)
	movq	global_trees+160(%rip), %rdi
	callq	layout_type
	movq	$8, %rdi
	callq	make_node
	movq	%rax, global_trees+168(%rip)
	movq	global_trees+192(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	global_trees+168(%rip), %rdi
	callq	layout_type
	movq	$8, %rdi
	callq	make_node
	movq	%rax, global_trees+176(%rip)
	movq	global_trees+200(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	global_trees+176(%rip), %rdi
	callq	layout_type
	movq	$8, %rdi
	callq	make_node
	movq	%rax, global_trees+184(%rip)
	movq	global_trees+208(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	global_trees+184(%rip), %rdi
	callq	layout_type
	callq	ix86_build_va_list
	movzbl	16(%rax), %ecx
	cmpq	$20, %rcx
	je	.LBB137_20
# BB#19:                                # %if.then56
	movq	128(%rax), %rbx
	movq	%rax, %rdi
	callq	copy_node
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	120(%rbx), %rcx
	movq	%rcx, 120(%rax)
	movq	%rax, 120(%rbx)
.LBB137_20:                             # %if.end58
	movq	%rax, global_trees+248(%rip)
	movq	global_trees+64(%rip), %rsi
	movq	$37, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+272(%rip)
	movq	global_trees+64(%rip), %rsi
	movq	$33, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+304(%rip)
	movq	global_trees+56(%rip), %rsi
	movq	$36, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+296(%rip)
	movq	global_trees+48(%rip), %rsi
	movq	$39, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+288(%rip)
	movq	global_trees+56(%rip), %rsi
	movq	$40, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+280(%rip)
	movq	global_trees+48(%rip), %rsi
	movq	$43, %rdi
	movq	$1, %rdx
	callq	make_vector
	movq	%rax, global_trees+320(%rip)
	movq	global_trees+192(%rip), %rsi
	movq	$50, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+336(%rip)
	movq	global_trees+192(%rip), %rsi
	movq	$46, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+328(%rip)
	movq	global_trees+24(%rip), %rsi
	movq	$37, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+344(%rip)
	movq	global_trees+24(%rip), %rsi
	movq	$33, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+376(%rip)
	movq	global_trees+16(%rip), %rsi
	movq	$36, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+368(%rip)
	movq	global_trees+8(%rip), %rsi
	movq	$39, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+360(%rip)
	movq	global_trees+16(%rip), %rsi
	movq	$40, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+352(%rip)
	movq	global_trees+192(%rip), %rsi
	movq	$44, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+384(%rip)
	movq	global_trees+8(%rip), %rsi
	movq	$43, %rdi
	xorq	%rdx, %rdx
	callq	make_vector
	movq	%rax, global_trees+392(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp1112:
	.size	build_common_tree_nodes_2, .Ltmp1112-build_common_tree_nodes_2
	.cfi_endproc

	.align	16, 0x90
	.type	make_vector,@function
make_vector:                            # @make_vector
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp1118:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp1119:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp1120:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp1121:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp1122:
	.cfi_def_cfa_offset 48
.Ltmp1123:
	.cfi_offset %rbx, -40
.Ltmp1124:
	.cfi_offset %r12, -32
.Ltmp1125:
	.cfi_offset %r14, -24
.Ltmp1126:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$9, %rdi
	callq	make_node
	movq	%rax, %r14
	movq	%r15, 8(%r14)
	movl	60(%r14), %eax
	andq	$127, %rbx
	shlq	$9, %rbx
	movabsq	$4294902271, %rcx       # imm = 0xFFFF01FF
	andq	%rax, %rcx
	orq	%rbx, %rcx
	movl	%ecx, 60(%r14)
	movl	16(%r15), %eax
	andq	$1, %r12
	shlq	$13, %r12
	movabsq	$4294959103, %rcx       # imm = 0xFFFFDFFF
	andq	%rax, %rcx
	orq	%r12, %rcx
	movl	%ecx, 16(%r15)
	movq	%r14, %rdi
	callq	layout_type
	movl	60(%r14), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movzbl	mode_unit_size(%rax), %ecx
	testq	%rcx, %rcx
	movq	$-1, %rbx
	je	.LBB138_2
# BB#1:                                 # %cond.false.i
	movzbl	mode_size(%rax), %eax
	xorq	%rdx, %rdx
	divq	%rcx
	movq	%rax, %rbx
	decq	%rbx
.LBB138_2:                              # %finish_vector_type.exit
	movq	$25, %rdi
	callq	make_node
	movq	%rbx, 32(%rax)
	movq	$0, 40(%rax)
	movq	integer_types+40(%rip), %rcx
	movq	%rcx, 8(%rax)
	movq	8(%r14), %rbx
	movq	%rax, %rdi
	callq	build_index_type
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	build_array_type
	movq	%rax, %r15
	movq	$20, %rdi
	callq	make_node
	movq	%rax, %rbx
	movabsq	$.L.str170, %rdi
	callq	get_identifier
	movq	%rax, %r12
	movq	$37, %rdi
	callq	make_node
	movq	%r12, 72(%rax)
	movq	%r15, 8(%rax)
	movq	%rax, 24(%rbx)
	movq	%rbx, 80(%rax)
	movq	%rbx, %rdi
	callq	layout_type
	movq	%rbx, 24(%r14)
	movl	56(%r14), %eax
	movl	%eax, 56(%rbx)
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp1127:
	.size	make_vector, .Ltmp1127-make_vector
	.cfi_endproc

	.type	tree_code_type,@object  # @tree_code_type
	.data
	.globl	tree_code_type
	.align	16
tree_code_type:
	.asciz	"xxxxbttttttttttttttttttttcccccdddddddddrrrrrrreeeeeeeeeeexe22222222222221111112211222222221eeeeee<<<<<<<<<<<<<2<121111eeeeeee2111eeeeeeeessssseeeee\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
	.size	tree_code_type, 256

	.type	tree_code_length,@object # @tree_code_length
	.globl	tree_code_length
	.align	16
tree_code_length:
	.long	0                       # 0x0
	.long	4294967295              # 0xffffffff
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	4                       # 0x4
	.long	3                       # 0x3
	.long	3                       # 0x3
	.long	2                       # 0x2
	.long	4                       # 0x4
	.long	3                       # 0x3
	.long	1                       # 0x1
	.long	0                       # 0x0
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	3                       # 0x3
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	1                       # 0x1
	.long	2                       # 0x2
	.long	2                       # 0x2
	.long	3                       # 0x3
	.long	2                       # 0x2
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.size	tree_code_length, 1024

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"error_mark"
	.size	.L.str, 11

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"identifier_node"
	.size	.L.str1, 16

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"tree_list"
	.size	.L.str2, 10

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"tree_vec"
	.size	.L.str3, 9

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"block"
	.size	.L.str4, 6

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"void_type"
	.size	.L.str5, 10

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"integer_type"
	.size	.L.str6, 13

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"real_type"
	.size	.L.str7, 10

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"complex_type"
	.size	.L.str8, 13

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"vector_type"
	.size	.L.str9, 12

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"enumeral_type"
	.size	.L.str10, 14

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"boolean_type"
	.size	.L.str11, 13

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"char_type"
	.size	.L.str12, 10

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"pointer_type"
	.size	.L.str13, 13

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"offset_type"
	.size	.L.str14, 12

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"reference_type"
	.size	.L.str15, 15

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"method_type"
	.size	.L.str16, 12

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"file_type"
	.size	.L.str17, 10

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"array_type"
	.size	.L.str18, 11

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"set_type"
	.size	.L.str19, 9

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"record_type"
	.size	.L.str20, 12

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"union_type"
	.size	.L.str21, 11

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"qual_union_type"
	.size	.L.str22, 16

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"function_type"
	.size	.L.str23, 14

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"lang_type"
	.size	.L.str24, 10

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"integer_cst"
	.size	.L.str25, 12

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"real_cst"
	.size	.L.str26, 9

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"complex_cst"
	.size	.L.str27, 12

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"vector_cst"
	.size	.L.str28, 11

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"string_cst"
	.size	.L.str29, 11

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"function_decl"
	.size	.L.str30, 14

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"label_decl"
	.size	.L.str31, 11

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"const_decl"
	.size	.L.str32, 11

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"type_decl"
	.size	.L.str33, 10

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"var_decl"
	.size	.L.str34, 9

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"parm_decl"
	.size	.L.str35, 10

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"result_decl"
	.size	.L.str36, 12

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"field_decl"
	.size	.L.str37, 11

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"namespace_decl"
	.size	.L.str38, 15

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"component_ref"
	.size	.L.str39, 14

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"bit_field_ref"
	.size	.L.str40, 14

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"indirect_ref"
	.size	.L.str41, 13

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"buffer_ref"
	.size	.L.str42, 11

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"array_ref"
	.size	.L.str43, 10

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"array_range_ref"
	.size	.L.str44, 16

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"vtable_ref"
	.size	.L.str45, 11

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"constructor"
	.size	.L.str46, 12

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"compound_expr"
	.size	.L.str47, 14

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"modify_expr"
	.size	.L.str48, 12

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"init_expr"
	.size	.L.str49, 10

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"target_expr"
	.size	.L.str50, 12

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"cond_expr"
	.size	.L.str51, 10

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"bind_expr"
	.size	.L.str52, 10

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"call_expr"
	.size	.L.str53, 10

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"method_call_expr"
	.size	.L.str54, 17

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"with_cleanup_expr"
	.size	.L.str55, 18

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"cleanup_point_expr"
	.size	.L.str56, 19

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"placeholder_expr"
	.size	.L.str57, 17

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"with_record_expr"
	.size	.L.str58, 17

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"plus_expr"
	.size	.L.str59, 10

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"minus_expr"
	.size	.L.str60, 11

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"mult_expr"
	.size	.L.str61, 10

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"trunc_div_expr"
	.size	.L.str62, 15

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"ceil_div_expr"
	.size	.L.str63, 14

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"floor_div_expr"
	.size	.L.str64, 15

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"round_div_expr"
	.size	.L.str65, 15

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"trunc_mod_expr"
	.size	.L.str66, 15

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"ceil_mod_expr"
	.size	.L.str67, 14

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"floor_mod_expr"
	.size	.L.str68, 15

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"round_mod_expr"
	.size	.L.str69, 15

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"rdiv_expr"
	.size	.L.str70, 10

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"exact_div_expr"
	.size	.L.str71, 15

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"fix_trunc_expr"
	.size	.L.str72, 15

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"fix_ceil_expr"
	.size	.L.str73, 14

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"fix_floor_expr"
	.size	.L.str74, 15

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"fix_round_expr"
	.size	.L.str75, 15

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"float_expr"
	.size	.L.str76, 11

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"negate_expr"
	.size	.L.str77, 12

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"min_expr"
	.size	.L.str78, 9

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"max_expr"
	.size	.L.str79, 9

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"abs_expr"
	.size	.L.str80, 9

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"ffs_expr"
	.size	.L.str81, 9

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"lshift_expr"
	.size	.L.str82, 12

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"rshift_expr"
	.size	.L.str83, 12

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"lrotate_expr"
	.size	.L.str84, 13

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"rrotate_expr"
	.size	.L.str85, 13

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"bit_ior_expr"
	.size	.L.str86, 13

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"bit_xor_expr"
	.size	.L.str87, 13

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"bit_and_expr"
	.size	.L.str88, 13

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"bit_andtc_expr"
	.size	.L.str89, 15

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"bit_not_expr"
	.size	.L.str90, 13

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"truth_andif_expr"
	.size	.L.str91, 17

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"truth_orif_expr"
	.size	.L.str92, 16

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"truth_and_expr"
	.size	.L.str93, 15

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"truth_or_expr"
	.size	.L.str94, 14

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"truth_xor_expr"
	.size	.L.str95, 15

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"truth_not_expr"
	.size	.L.str96, 15

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"lt_expr"
	.size	.L.str97, 8

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"le_expr"
	.size	.L.str98, 8

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"gt_expr"
	.size	.L.str99, 8

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"ge_expr"
	.size	.L.str100, 8

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"eq_expr"
	.size	.L.str101, 8

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"ne_expr"
	.size	.L.str102, 8

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"unordered_expr"
	.size	.L.str103, 15

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"ordered_expr"
	.size	.L.str104, 13

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"unlt_expr"
	.size	.L.str105, 10

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"unle_expr"
	.size	.L.str106, 10

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"ungt_expr"
	.size	.L.str107, 10

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"unge_expr"
	.size	.L.str108, 10

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"uneq_expr"
	.size	.L.str109, 10

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"in_expr"
	.size	.L.str110, 8

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"set_le_expr"
	.size	.L.str111, 12

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"card_expr"
	.size	.L.str112, 10

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"range_expr"
	.size	.L.str113, 11

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"convert_expr"
	.size	.L.str114, 13

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"nop_expr"
	.size	.L.str115, 9

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"non_lvalue_expr"
	.size	.L.str116, 16

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"view_convert_expr"
	.size	.L.str117, 18

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"save_expr"
	.size	.L.str118, 10

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"unsave_expr"
	.size	.L.str119, 12

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"rtl_expr"
	.size	.L.str120, 9

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"addr_expr"
	.size	.L.str121, 10

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"reference_expr"
	.size	.L.str122, 15

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"entry_value_expr"
	.size	.L.str123, 17

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"fdesc_expr"
	.size	.L.str124, 11

	.type	.L.str125,@object       # @.str125
.L.str125:
	.asciz	"complex_expr"
	.size	.L.str125, 13

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"conj_expr"
	.size	.L.str126, 10

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"realpart_expr"
	.size	.L.str127, 14

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"imagpart_expr"
	.size	.L.str128, 14

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"predecrement_expr"
	.size	.L.str129, 18

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"preincrement_expr"
	.size	.L.str130, 18

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"postdecrement_expr"
	.size	.L.str131, 19

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"postincrement_expr"
	.size	.L.str132, 19

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"va_arg_expr"
	.size	.L.str133, 12

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"try_catch_expr"
	.size	.L.str134, 15

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"try_finally"
	.size	.L.str135, 12

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"goto_subroutine"
	.size	.L.str136, 16

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"label_expr"
	.size	.L.str137, 11

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"goto_expr"
	.size	.L.str138, 10

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"return_expr"
	.size	.L.str139, 12

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"exit_expr"
	.size	.L.str140, 10

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"loop_expr"
	.size	.L.str141, 10

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"labeled_block_expr"
	.size	.L.str142, 19

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"exit_block_expr"
	.size	.L.str143, 16

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"expr_with_file_location"
	.size	.L.str144, 24

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"switch_expr"
	.size	.L.str145, 12

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"exc_ptr_expr"
	.size	.L.str146, 13

	.type	tree_code_name,@object  # @tree_code_name
	.data
	.globl	tree_code_name
	.align	16
tree_code_name:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.quad	.L.str36
	.quad	.L.str37
	.quad	.L.str38
	.quad	.L.str39
	.quad	.L.str40
	.quad	.L.str41
	.quad	.L.str42
	.quad	.L.str43
	.quad	.L.str44
	.quad	.L.str45
	.quad	.L.str46
	.quad	.L.str47
	.quad	.L.str48
	.quad	.L.str49
	.quad	.L.str50
	.quad	.L.str51
	.quad	.L.str52
	.quad	.L.str53
	.quad	.L.str54
	.quad	.L.str55
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.quad	.L.str62
	.quad	.L.str63
	.quad	.L.str64
	.quad	.L.str65
	.quad	.L.str66
	.quad	.L.str67
	.quad	.L.str68
	.quad	.L.str69
	.quad	.L.str70
	.quad	.L.str71
	.quad	.L.str72
	.quad	.L.str73
	.quad	.L.str74
	.quad	.L.str75
	.quad	.L.str76
	.quad	.L.str77
	.quad	.L.str78
	.quad	.L.str79
	.quad	.L.str80
	.quad	.L.str81
	.quad	.L.str82
	.quad	.L.str83
	.quad	.L.str84
	.quad	.L.str85
	.quad	.L.str86
	.quad	.L.str87
	.quad	.L.str88
	.quad	.L.str89
	.quad	.L.str90
	.quad	.L.str91
	.quad	.L.str92
	.quad	.L.str93
	.quad	.L.str94
	.quad	.L.str95
	.quad	.L.str96
	.quad	.L.str97
	.quad	.L.str98
	.quad	.L.str99
	.quad	.L.str100
	.quad	.L.str101
	.quad	.L.str102
	.quad	.L.str103
	.quad	.L.str104
	.quad	.L.str105
	.quad	.L.str106
	.quad	.L.str107
	.quad	.L.str108
	.quad	.L.str109
	.quad	.L.str110
	.quad	.L.str111
	.quad	.L.str112
	.quad	.L.str113
	.quad	.L.str114
	.quad	.L.str115
	.quad	.L.str116
	.quad	.L.str117
	.quad	.L.str118
	.quad	.L.str119
	.quad	.L.str120
	.quad	.L.str121
	.quad	.L.str122
	.quad	.L.str123
	.quad	.L.str124
	.quad	.L.str125
	.quad	.L.str126
	.quad	.L.str127
	.quad	.L.str128
	.quad	.L.str129
	.quad	.L.str130
	.quad	.L.str131
	.quad	.L.str132
	.quad	.L.str133
	.quad	.L.str134
	.quad	.L.str135
	.quad	.L.str136
	.quad	.L.str137
	.quad	.L.str138
	.quad	.L.str139
	.quad	.L.str140
	.quad	.L.str141
	.quad	.L.str142
	.quad	.L.str143
	.quad	.L.str144
	.quad	.L.str145
	.quad	.L.str146
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.quad	0
	.size	tree_code_name, 2048

	.type	.L.str147,@object       # @.str147
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str147:
	.asciz	"tree.c"
	.size	.L.str147, 7

	.type	.L__FUNCTION__.set_decl_assembler_name,@object # @__FUNCTION__.set_decl_assembler_name
.L__FUNCTION__.set_decl_assembler_name:
	.asciz	"set_decl_assembler_name"
	.size	.L__FUNCTION__.set_decl_assembler_name, 24

	.type	permanent_obstack,@object # @permanent_obstack
	.comm	permanent_obstack,88,8
	.type	type_hash_table,@object # @type_hash_table
	.comm	type_hash_table,8,8
	.type	global_trees,@object    # @global_trees
	.comm	global_trees,408,16
	.type	integer_types,@object   # @integer_types
	.comm	integer_types,88,16
	.type	lang_set_decl_assembler_name,@object # @lang_set_decl_assembler_name
	.comm	lang_set_decl_assembler_name,8,8
	.type	.L__FUNCTION__.tree_size,@object # @__FUNCTION__.tree_size
.L__FUNCTION__.tree_size:
	.asciz	"tree_size"
	.size	.L__FUNCTION__.tree_size, 10

	.type	.L__FUNCTION__.make_node,@object # @__FUNCTION__.make_node
.L__FUNCTION__.make_node:
	.asciz	"make_node"
	.size	.L__FUNCTION__.make_node, 10

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"<built-in>"
	.size	.L.str148, 11

	.type	next_decl_uid,@object   # @next_decl_uid
	.local	next_decl_uid
	.comm	next_decl_uid,4,4
	.type	next_type_uid,@object   # @next_type_uid
	.data
	.align	4
next_type_uid:
	.long	1                       # 0x1
	.size	next_type_uid, 4

	.type	make_lang_type_fn,@object # @make_lang_type_fn
	.globl	make_lang_type_fn
	.align	8
make_lang_type_fn:
	.quad	make_node
	.size	make_lang_type_fn, 8

	.type	.L__FUNCTION__.integer_all_onesp,@object # @__FUNCTION__.integer_all_onesp
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.integer_all_onesp:
	.asciz	"integer_all_onesp"
	.size	.L__FUNCTION__.integer_all_onesp, 18

	.type	.L__FUNCTION__.unsave_expr_1,@object # @__FUNCTION__.unsave_expr_1
.L__FUNCTION__.unsave_expr_1:
	.asciz	"unsave_expr_1"
	.size	.L__FUNCTION__.unsave_expr_1, 14

	.type	lang_unsave_expr_now,@object # @lang_unsave_expr_now
	.comm	lang_unsave_expr_now,8,8
	.type	lang_unsave,@object     # @lang_unsave
	.comm	lang_unsave,8,8
	.type	lang_unsafe_for_reeval,@object # @lang_unsafe_for_reeval
	.comm	lang_unsafe_for_reeval,8,8
	.type	.L__FUNCTION__.substitute_in_expr,@object # @__FUNCTION__.substitute_in_expr
.L__FUNCTION__.substitute_in_expr:
	.asciz	"substitute_in_expr"
	.size	.L__FUNCTION__.substitute_in_expr, 19

	.type	.L__FUNCTION__.stabilize_reference_1,@object # @__FUNCTION__.stabilize_reference_1
.L__FUNCTION__.stabilize_reference_1:
	.asciz	"stabilize_reference_1"
	.size	.L__FUNCTION__.stabilize_reference_1, 22

	.type	.L__FUNCTION__.build,@object # @__FUNCTION__.build
.L__FUNCTION__.build:
	.asciz	"build"
	.size	.L__FUNCTION__.build, 6

	.type	build_expr_wfl.last_file,@object # @build_expr_wfl.last_file
	.local	build_expr_wfl.last_file
	.comm	build_expr_wfl.last_file,8,8
	.type	build_expr_wfl.last_filenode,@object # @build_expr_wfl.last_filenode
	.local	build_expr_wfl.last_filenode
	.comm	build_expr_wfl.last_filenode,8,8
	.type	default_target_attribute_table,@object # @default_target_attribute_table
	.section	.rodata,"a",@progbits
	.globl	default_target_attribute_table
	.align	16
default_target_attribute_table:
	.zero	32
	.size	default_target_attribute_table, 32

	.type	.L__FUNCTION__.is_attribute_p,@object # @__FUNCTION__.is_attribute_p
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.is_attribute_p:
	.asciz	"is_attribute_p"
	.size	.L__FUNCTION__.is_attribute_p, 15

	.type	.L__FUNCTION__.lookup_attribute,@object # @__FUNCTION__.lookup_attribute
.L__FUNCTION__.lookup_attribute:
	.asciz	"lookup_attribute"
	.size	.L__FUNCTION__.lookup_attribute, 17

	.type	debug_no_type_hash,@object # @debug_no_type_hash
	.bss
	.globl	debug_no_type_hash
	.align	4
debug_no_type_hash:
	.long	0                       # 0x0
	.size	debug_no_type_hash, 4

	.type	.L__FUNCTION__.tree_low_cst,@object # @__FUNCTION__.tree_low_cst
	.section	.rodata.str1.1,"aMS",@progbits,1
.L__FUNCTION__.tree_low_cst:
	.asciz	"tree_low_cst"
	.size	.L__FUNCTION__.tree_low_cst, 13

	.type	.L__FUNCTION__.simple_cst_equal,@object # @__FUNCTION__.simple_cst_equal
.L__FUNCTION__.simple_cst_equal:
	.asciz	"simple_cst_equal"
	.size	.L__FUNCTION__.simple_cst_equal, 17

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"arrays of functions are not meaningful"
	.size	.L.str149, 39

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"function return type cannot be function"
	.size	.L.str150, 40

	.type	.L__FUNCTION__.build_method_type,@object # @__FUNCTION__.build_method_type
.L__FUNCTION__.build_method_type:
	.asciz	"build_method_type"
	.size	.L__FUNCTION__.build_method_type, 18

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"complex char"
	.size	.L.str151, 13

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"complex signed char"
	.size	.L.str152, 20

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"complex unsigned char"
	.size	.L.str153, 22

	.type	.L.str154,@object       # @.str154
.L.str154:
	.asciz	"complex short int"
	.size	.L.str154, 18

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"complex short unsigned int"
	.size	.L.str155, 27

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"complex int"
	.size	.L.str156, 12

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"complex unsigned int"
	.size	.L.str157, 21

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"complex long int"
	.size	.L.str158, 17

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"complex long unsigned int"
	.size	.L.str159, 26

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"complex long long int"
	.size	.L.str160, 22

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"complex long long unsigned int"
	.size	.L.str161, 31

	.type	.L__FUNCTION__.decl_type_context,@object # @__FUNCTION__.decl_type_context
.L__FUNCTION__.decl_type_context:
	.asciz	"decl_type_context"
	.size	.L__FUNCTION__.decl_type_context, 18

	.type	.L__FUNCTION__.get_callee_fndecl,@object # @__FUNCTION__.get_callee_fndecl
.L__FUNCTION__.get_callee_fndecl:
	.asciz	"get_callee_fndecl"
	.size	.L__FUNCTION__.get_callee_fndecl, 18

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"obstack %s: %u bytes, %d chunks\n"
	.size	.L.str162, 33

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"\n??? tree nodes created\n\n"
	.size	.L.str163, 26

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"(No per-node statistics)\n"
	.size	.L.str164, 26

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"permanent_obstack"
	.size	.L.str165, 18

	.type	.L.str166,@object       # @.str166
.L.str166:
	.zero	1
	.size	.L.str166, 1

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"%s%s"
	.size	.L.str167, 5

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"_GLOBAL__%s_%s"
	.size	.L.str168, 15

	.type	.L__FUNCTION__.get_set_constructor_bits,@object # @__FUNCTION__.get_set_constructor_bits
.L__FUNCTION__.get_set_constructor_bits:
	.asciz	"get_set_constructor_bits"
	.size	.L__FUNCTION__.get_set_constructor_bits, 25

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"invalid initializer for bit string"
	.size	.L.str169, 35

	.type	tree_node_counts,@object # @tree_node_counts
	.comm	tree_node_counts,56,16
	.type	tree_node_sizes,@object # @tree_node_sizes
	.comm	tree_node_sizes,56,16
	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"f"
	.size	.L.str170, 2

	.type	append_random_chars.letters,@object # @append_random_chars.letters
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
append_random_chars.letters:
	.asciz	"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
	.size	append_random_chars.letters, 63

	.type	append_random_chars.value,@object # @append_random_chars.value
	.local	append_random_chars.value
	.comm	append_random_chars.value,8,8
	.type	.L.str171,@object       # @.str171
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str171:
	.asciz	"Type hash: size %ld, %ld elements, %f collisions\n"
	.size	.L.str171, 50

	.type	.L__FUNCTION__.unsave_expr_now_r,@object # @__FUNCTION__.unsave_expr_now_r
.L__FUNCTION__.unsave_expr_now_r:
	.asciz	"unsave_expr_now_r"
	.size	.L__FUNCTION__.unsave_expr_now_r, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
