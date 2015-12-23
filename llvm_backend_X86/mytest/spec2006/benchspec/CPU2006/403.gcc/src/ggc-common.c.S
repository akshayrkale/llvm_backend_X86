	.file	"ggc-common.c.bc"
	.text
	.globl	ggc_add_root
	.align	16, 0x90
	.type	ggc_add_root,@function
ggc_add_root:                           # @ggc_add_root
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
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r12d, 16(%rax)
	movl	%r15d, 20(%rax)
	movq	%r14, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	ggc_add_root, .Ltmp14-ggc_add_root
	.cfi_endproc

	.globl	ggc_add_rtx_root
	.align	16, 0x90
	.type	ggc_add_rtx_root,@function
ggc_add_rtx_root:                       # @ggc_add_rtx_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 32
.Ltmp21:
	.cfi_offset %rbx, -24
.Ltmp22:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	$8, %rcx
	movl	%ecx, 20(%rax)
	movq	$ggc_mark_rtx_ptr, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp23:
	.size	ggc_add_rtx_root, .Ltmp23-ggc_add_rtx_root
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_rtx_ptr,@function
ggc_mark_rtx_ptr:                       # @ggc_mark_rtx_ptr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp29:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp30:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp31:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp32:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp33:
	.cfi_def_cfa_offset 48
.Ltmp34:
	.cfi_offset %rbx, -40
.Ltmp35:
	.cfi_offset %r12, -32
.Ltmp36:
	.cfi_offset %r14, -24
.Ltmp37:
	.cfi_offset %r15, -16
	movq	(%rdi), %r14
	testq	%r14, %r14
	je	.LBB2_11
# BB#1:                                 # %land.lhs.true
	movq	%r14, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB2_2
.LBB2_11:                               # %do.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB2_2:                                # %if.then
	movzbl	(%r14), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	ja	.LBB2_10
# BB#3:                                 # %sw.bb.i
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_10
# BB#4:
	xorq	%r12, %r12
	.align	16, 0x90
.LBB2_5:                                # %land.lhs.true.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB2_6
# BB#12:                                # %for.inc.i
                                        #   in Loop: Header=BB2_5 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_5
	jmp	.LBB2_7
.LBB2_6:
	movq	%rbx, %r12
.LBB2_7:                                # %for.cond7.preheader.i
	movq	24(%r14), %rbx
	jmp	.LBB2_9
	.align	16, 0x90
.LBB2_8:                                # %for.body.i
                                        #   in Loop: Header=BB2_9 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB2_9:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r12, %rbx
	jne	.LBB2_8
.LBB2_10:                               # %ggc_mark_rtx_children.exit
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children_1  # TAILCALL
.Ltmp38:
	.size	ggc_mark_rtx_ptr, .Ltmp38-ggc_mark_rtx_ptr
	.cfi_endproc

	.globl	ggc_add_tree_root
	.align	16, 0x90
	.type	ggc_add_tree_root,@function
ggc_add_tree_root:                      # @ggc_add_tree_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp44:
	.cfi_def_cfa_offset 32
.Ltmp45:
	.cfi_offset %rbx, -24
.Ltmp46:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	$8, %rcx
	movl	%ecx, 20(%rax)
	movq	$ggc_mark_tree_ptr, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp47:
	.size	ggc_add_tree_root, .Ltmp47-ggc_add_tree_root
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_tree_ptr,@function
ggc_mark_tree_ptr:                      # @ggc_mark_tree_ptr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 16
.Ltmp51:
	.cfi_offset %rbx, -16
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB4_5
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB4_5
# BB#2:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB4_4
# BB#3:                                 # %if.then3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB4_4:                                # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB4_5:                                # %do.end8
	popq	%rbx
	retq
.Ltmp52:
	.size	ggc_mark_tree_ptr, .Ltmp52-ggc_mark_tree_ptr
	.cfi_endproc

	.globl	ggc_add_rtx_varray_root
	.align	16, 0x90
	.type	ggc_add_rtx_varray_root,@function
ggc_add_rtx_varray_root:                # @ggc_add_rtx_varray_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp58:
	.cfi_def_cfa_offset 32
.Ltmp59:
	.cfi_offset %rbx, -24
.Ltmp60:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	$8, %rcx
	movl	%ecx, 20(%rax)
	movq	$ggc_mark_rtx_varray_ptr, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp61:
	.size	ggc_add_rtx_varray_root, .Ltmp61-ggc_add_rtx_varray_root
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_rtx_varray_ptr,@function
ggc_mark_rtx_varray_ptr:                # @ggc_mark_rtx_varray_ptr
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	jmp	ggc_mark_rtx_varray  # TAILCALL
.Ltmp62:
	.size	ggc_mark_rtx_varray_ptr, .Ltmp62-ggc_mark_rtx_varray_ptr
	.cfi_endproc

	.globl	ggc_add_tree_varray_root
	.align	16, 0x90
	.type	ggc_add_tree_varray_root,@function
ggc_add_tree_varray_root:               # @ggc_add_tree_varray_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp66:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp67:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp68:
	.cfi_def_cfa_offset 32
.Ltmp69:
	.cfi_offset %rbx, -24
.Ltmp70:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	$8, %rcx
	movl	%ecx, 20(%rax)
	movq	$ggc_mark_tree_varray_ptr, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp71:
	.size	ggc_add_tree_varray_root, .Ltmp71-ggc_add_tree_varray_root
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_tree_varray_ptr,@function
ggc_mark_tree_varray_ptr:               # @ggc_mark_tree_varray_ptr
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
	movq	(%rdi), %r14
	testq	%r14, %r14
	je	.LBB8_9
# BB#1:                                 # %if.then.i
	movq	(%r14), %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	addq	%r15, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB8_9
# BB#2:                                 # %do.body.lr.ph.i
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB8_3:                                # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14,%rbx,8), %r12
	testq	%r12, %r12
	je	.LBB8_8
# BB#4:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	%r12, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB8_8
# BB#5:                                 # %do.body6.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB8_7
# BB#6:                                 # %if.then10.i
                                        #   in Loop: Header=BB8_3 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB8_7:                                # %if.end.i
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
.LBB8_8:                                # %for.inc.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	jg	.LBB8_3
.LBB8_9:                                # %ggc_mark_tree_varray.exit
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp86:
	.size	ggc_mark_tree_varray_ptr, .Ltmp86-ggc_mark_tree_varray_ptr
	.cfi_endproc

	.globl	ggc_add_tree_hash_table_root
	.align	16, 0x90
	.type	ggc_add_tree_hash_table_root,@function
ggc_add_tree_hash_table_root:           # @ggc_add_tree_hash_table_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp90:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp91:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp92:
	.cfi_def_cfa_offset 32
.Ltmp93:
	.cfi_offset %rbx, -24
.Ltmp94:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	$32, %rdi
	callq	xmalloc
	movq	roots(%rip), %rcx
	movq	%rcx, (%rax)
	movq	%rbx, 8(%rax)
	movl	%r14d, 16(%rax)
	movq	$8, %rcx
	movl	%ecx, 20(%rax)
	movq	$ggc_mark_tree_hash_table_ptr, 24(%rax)
	movq	%rax, roots(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp95:
	.size	ggc_add_tree_hash_table_root, .Ltmp95-ggc_add_tree_hash_table_root
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_tree_hash_table_ptr,@function
ggc_mark_tree_hash_table_ptr:           # @ggc_mark_tree_hash_table_ptr
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	movabsq	$ggc_mark_tree_hash_table_entry, %rsi
	xorq	%rdx, %rdx
	jmp	hash_traverse  # TAILCALL
.Ltmp96:
	.size	ggc_mark_tree_hash_table_ptr, .Ltmp96-ggc_mark_tree_hash_table_ptr
	.cfi_endproc

	.globl	ggc_del_root
	.align	16, 0x90
	.type	ggc_del_root,@function
ggc_del_root:                           # @ggc_del_root
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp98:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	movabsq	$roots, %rdi
	.align	16, 0x90
.LBB11_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rcx
	movq	(%rcx), %rdi
	testq	%rdi, %rdi
	je	.LBB11_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB11_1 Depth=1
	movq	8(%rdi), %rdx
	cmpq	%rax, %rdx
	jne	.LBB11_1
# BB#3:                                 # %if.then
	movq	(%rdi), %rax
	movq	%rax, (%rcx)
	popq	%rax
	jmp	free  # TAILCALL
.LBB11_4:                               # %while.end
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.ggc_del_root, %rcx
	movq	$169, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp99:
	.size	ggc_del_root, .Ltmp99-ggc_del_root
	.cfi_endproc

	.globl	ggc_add_deletable_htab
	.align	16, 0x90
	.type	ggc_add_deletable_htab,@function
ggc_add_deletable_htab:                 # @ggc_add_deletable_htab
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
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movq	$32, %rdi
	callq	xmalloc
	movq	d_htab_roots(%rip), %rcx
	testq	%rbx, %rbx
	movq	%rcx, (%rax)
	movq	%r15, 8(%rax)
	jne	.LBB12_2
# BB#1:                                 # %entry
	movabsq	$ggc_marked_p, %rbx
.LBB12_2:                               # %entry
	movq	%rbx, 16(%rax)
	movq	%r14, 24(%rax)
	movq	%rax, d_htab_roots(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp110:
	.size	ggc_add_deletable_htab, .Ltmp110-ggc_add_deletable_htab
	.cfi_endproc

	.globl	ggc_mark_roots
	.align	16, 0x90
	.type	ggc_mark_roots,@function
ggc_mark_roots:                         # @ggc_mark_roots
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp117:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp118:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp119:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp120:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp121:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp122:
	.cfi_def_cfa_offset 64
.Ltmp123:
	.cfi_offset %rbx, -48
.Ltmp124:
	.cfi_offset %r12, -40
.Ltmp125:
	.cfi_offset %r13, -32
.Ltmp126:
	.cfi_offset %r14, -24
.Ltmp127:
	.cfi_offset %r15, -16
	movabsq	$.L.str1, %rdx
	movq	$4096, %rdi             # imm = 0x1000
	movq	$8, %rsi
	callq	varray_init
	movq	roots(%rip), %rcx
	movq	%rax, ggc_pending_trees(%rip)
	testq	%rcx, %rcx
	je	.LBB13_6
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB13_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_4 Depth 2
	movq	%rcx, (%rsp)            # 8-byte Spill
	movslq	16(%rcx), %r12
	testq	%r12, %r12
	jle	.LBB13_5
# BB#3:                                 # %for.body4.lr.ph
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movq	24(%rax), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	8(%rax), %rbx
	movslq	20(%rax), %r14
	xorq	%r15, %r15
	.align	16, 0x90
.LBB13_4:                               # %for.body4
                                        #   Parent Loop BB13_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	*8(%rsp)                # 8-byte Folded Reload
	incq	%r15
	movq	%r15, %rax
	andq	%r13, %rax
	addq	%r14, %rbx
	movq	%r12, %rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	jne	.LBB13_4
.LBB13_5:                               # %for.inc5
                                        #   in Loop: Header=BB13_2 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB13_2
.LBB13_6:                               # %for.end6
	callq	ggc_mark_trees
	movq	ggc_pending_trees(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_8
# BB#7:                                 # %if.then
	callq	free
	movq	$0, ggc_pending_trees(%rip)
.LBB13_8:                               # %do.end
	movabsq	$.L.str1, %rdx
	movq	$1024, %rdi             # imm = 0x400
	movq	$8, %rsi
	callq	varray_init
	movq	d_htab_roots(%rip), %rbx
	movq	%rax, ggc_pending_trees(%rip)
	testq	%rbx, %rbx
	je	.LBB13_11
# BB#9:
	movabsq	$ggc_htab_delete, %r14
	.align	16, 0x90
.LBB13_10:                              # %for.body10
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	htab_traverse
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB13_10
.LBB13_11:                              # %for.end13
	callq	ggc_mark_trees
	movq	ggc_pending_trees(%rip), %rdi
	testq	%rdi, %rdi
	je	.LBB13_13
# BB#12:                                # %if.then16
	callq	free
	movq	$0, ggc_pending_trees(%rip)
.LBB13_13:                              # %do.end18
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	ggc_mark_roots, .Ltmp128-ggc_mark_roots
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_trees,@function
ggc_mark_trees:                         # @ggc_mark_trees
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp137:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp138:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp140:
	.cfi_def_cfa_offset 64
.Ltmp141:
	.cfi_offset %rbx, -48
.Ltmp142:
	.cfi_offset %r12, -40
.Ltmp143:
	.cfi_offset %r13, -32
.Ltmp144:
	.cfi_offset %r14, -24
.Ltmp145:
	.cfi_offset %r15, -16
	movq	ggc_pending_trees(%rip), %rcx
	movq	8(%rcx), %rax
	testq	%rax, %rax
	je	.LBB14_68
# BB#1:
	leaq	8(%rcx), %rdx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_2:                               # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_56 Depth 2
                                        #     Child Loop BB14_61 Depth 2
                                        #     Child Loop BB14_141 Depth 2
                                        #     Child Loop BB14_146 Depth 2
                                        #     Child Loop BB14_152 Depth 2
                                        #     Child Loop BB14_157 Depth 2
                                        #     Child Loop BB14_267 Depth 2
                                        #     Child Loop BB14_272 Depth 2
                                        #     Child Loop BB14_73 Depth 2
                                        #       Child Loop BB14_78 Depth 3
                                        #       Child Loop BB14_277 Depth 3
                                        #     Child Loop BB14_20 Depth 2
	movq	24(%rcx,%rax,8), %r14
	decq	%rax
	movq	%rax, (%rdx)
	movq	ggc_stats(%rip), %rax
	movzbl	16(%r14), %r12d
	testq	%rax, %rax
	je	.LBB14_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB14_2 Depth=1
	movl	(%rax,%r12,4), %ecx
	incq	%rcx
	movl	%ecx, (%rax,%r12,4)
	movq	%r14, %rdi
	callq	ggc_get_size
	movq	ggc_stats(%rip), %rcx
	addq	%rax, 1024(%rcx,%r12,8)
.LBB14_4:                               # %do.body
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_9
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_9
# BB#6:                                 # %do.body12
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_8
# BB#7:                                 # %if.then15
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_8:                               # %if.end18
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_9:                               # %do.body26
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_14
# BB#10:                                # %land.lhs.true30
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_14
# BB#11:                                # %do.body34
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_13
# BB#12:                                # %if.then38
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_13:                              # %if.end42
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_14:                              # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$26, %r12
	jg	.LBB14_26
# BB#15:                                # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$1, %r12
	je	.LBB14_65
# BB#16:                                # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$2, %r12
	jne	.LBB14_17
# BB#30:                                # %do.body51
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_35
# BB#31:                                # %land.lhs.true54
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_35
# BB#32:                                # %do.body58
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_34
# BB#33:                                # %if.then62
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_34:                              # %if.end66
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_35:                              # %do.body75
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%r14), %rbx
.LBB14_36:                              # %do.body75
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_67
# BB#37:                                # %land.lhs.true79
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_67
# BB#38:                                # %do.body83
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_40
# BB#39:                                # %if.then87
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_40:                              # %if.end91
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB14_67
	.align	16, 0x90
.LBB14_26:                              # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$27, %r12
	je	.LBB14_41
# BB#27:                                # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %r13
	cmpq	$35, %r12
	jne	.LBB14_28
# BB#52:                                # %do.body183
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	160(%r14), %r15
	testq	%r15, %r15
	je	.LBB14_63
# BB#53:                                # %land.lhs.true185
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB14_63
# BB#54:                                # %if.then188
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, 8(%rsp)           # 8-byte Spill
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB14_62
# BB#55:                                # %sw.bb.i
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB14_62
	.align	16, 0x90
.LBB14_56:                              # %land.lhs.true.i
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB14_57
# BB#58:                                # %for.inc.i
                                        #   in Loop: Header=BB14_56 Depth=2
	movq	24(%rbx), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	jne	.LBB14_56
# BB#59:                                # %for.cond7.preheader.i
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB14_61
.LBB14_17:                              # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %r13
	cmpq	$3, %r12
	jne	.LBB14_63
# BB#18:                                # %sw.bb100
                                        #   in Loop: Header=BB14_2 Depth=1
	movslq	24(%r14), %rbx
	movq	%r14, %r12
	testq	%rbx, %rbx
	movq	%r13, %r15
	jle	.LBB14_67
# BB#19:                                # %do.body105.lr.ph
                                        #   in Loop: Header=BB14_2 Depth=1
	decq	%rbx
	.align	16, 0x90
.LBB14_20:                              # %do.body105
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	32(%r12,%rbx,8), %r14
	testq	%r14, %r14
	je	.LBB14_25
# BB#21:                                # %land.lhs.true111
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_25
# BB#22:                                # %do.body115
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_24
# BB#23:                                # %if.then119
                                        #   in Loop: Header=BB14_20 Depth=2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_24:                              # %if.end123
                                        #   in Loop: Header=BB14_20 Depth=2
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB14_25:                              # %while.cond101.backedge
                                        #   in Loop: Header=BB14_20 Depth=2
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB14_20
	jmp	.LBB14_67
.LBB14_41:                              # %do.body133
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_46
# BB#42:                                # %land.lhs.true136
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_46
# BB#43:                                # %do.body140
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_45
# BB#44:                                # %if.then144
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_45:                              # %if.end148
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_46:                              # %do.body157
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %r13
	movq	40(%r14), %rbx
	jmp	.LBB14_47
.LBB14_28:                              # %do.end50
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$37, %r12
	jne	.LBB14_63
# BB#29:                                # %do.body192
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	160(%r14), %rbx
.LBB14_47:                              # %do.body157
                                        #   in Loop: Header=BB14_2 Depth=1
	testq	%rbx, %rbx
	je	.LBB14_63
# BB#48:                                # %land.lhs.true161
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB14_63
# BB#49:                                # %do.body165
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_51
# BB#50:                                # %if.then169
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_51:                              # %if.end173
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB14_63
.LBB14_57:                              #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %r14
	movq	24(%r15), %rbx
	jmp	.LBB14_61
	.align	16, 0x90
.LBB14_60:                              # %for.body.i
                                        #   in Loop: Header=BB14_61 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB14_61:                              # %for.body.i
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB14_60
.LBB14_62:                              # %ggc_mark_rtx_children.exit
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	movq	8(%rsp), %r14           # 8-byte Reload
	.align	16, 0x90
.LBB14_63:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	movsbq	tree_code_type(%r12), %rax
	cmpq	$59, %rax
	jg	.LBB14_69
# BB#64:                                # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, 8(%rsp)           # 8-byte Spill
	addq	$-49, %rax
	movq	%r13, %r15
	andq	%r15, %rax
	cmpq	$2, %rax
	jb	.LBB14_71
	jmp	.LBB14_67
	.align	16, 0x90
.LBB14_69:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$97, %rax
	movq	%r13, %r15
	jg	.LBB14_80
# BB#70:                                # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, 8(%rsp)           # 8-byte Spill
	andq	%r15, %rax
	cmpq	$60, %rax
	jne	.LBB14_67
	jmp	.LBB14_71
.LBB14_80:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, 8(%rsp)           # 8-byte Spill
	cmpq	$113, %rax
	jg	.LBB14_83
# BB#81:                                # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	$-98, %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	ja	.LBB14_67
# BB#82:                                # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	jmpq	*.LJTI14_0(,%rax,8)
.LBB14_247:                             # %do.body1021
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_252
# BB#248:                               # %land.lhs.true1025
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_252
# BB#249:                               # %do.body1029
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_251
# BB#250:                               # %if.then1034
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_251:                             # %if.end1038
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_252:                             # %do.body1047
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_257
# BB#253:                               # %land.lhs.true1052
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_257
# BB#254:                               # %do.body1056
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_256
# BB#255:                               # %if.then1061
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_256:                             # %if.end1065
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_257:                             # %do.body1074
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	48(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_262
# BB#258:                               # %land.lhs.true1079
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_262
# BB#259:                               # %do.body1083
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_261
# BB#260:                               # %if.then1088
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_261:                             # %if.end1092
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_262:                             # %do.body1101
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	56(%r14), %rbx
	jmp	.LBB14_36
.LBB14_83:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	-114(%rax), %rcx
	andq	%r15, %rcx
	cmpq	$2, %rcx
	jae	.LBB14_84
	.align	16, 0x90
.LBB14_71:                              # %sw.bb1141
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movzbl	16(%rax), %edi
	movslq	tree_code_length(,%rdi,4), %r13
	callq	first_rtl_op
	movq	%rax, %rbx
	movq	%rbx, (%rsp)            # 8-byte Spill
	testq	%r13, %r13
	jle	.LBB14_67
# BB#72:                                # %while.body1158.lr.ph
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r13, %r14
	.align	16, 0x90
.LBB14_73:                              # %while.body1158
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB14_78 Depth 3
                                        #       Child Loop BB14_277 Depth 3
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	24(%rcx,%r14,8), %r12
	decq	%r13
	cmpq	%rdx, %rax
	leaq	-1(%r14), %r14
	jle	.LBB14_279
# BB#74:                                # %do.body1162
                                        #   in Loop: Header=BB14_73 Depth=2
	testq	%r12, %r12
	je	.LBB14_284
# BB#75:                                # %land.lhs.true1168
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	%r12, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_284
# BB#76:                                # %if.then1171
                                        #   in Loop: Header=BB14_73 Depth=2
	movzbl	(%r12), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	cmpq	$5, %rax
	ja	.LBB14_278
# BB#77:                                # %sw.bb.i1464
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	24(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB14_278
	.align	16, 0x90
.LBB14_78:                              # %land.lhs.true.i1472
                                        #   Parent Loop BB14_2 Depth=1
                                        #     Parent Loop BB14_73 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB14_79
# BB#274:                               # %for.inc.i1477
                                        #   in Loop: Header=BB14_78 Depth=3
	movq	24(%rbx), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	jne	.LBB14_78
# BB#275:                               # %for.cond7.preheader.i1468
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	24(%r12), %rbx
	jmp	.LBB14_277
	.align	16, 0x90
.LBB14_279:                             # %do.body1174
                                        #   in Loop: Header=BB14_73 Depth=2
	testq	%r12, %r12
	je	.LBB14_284
# BB#280:                               # %land.lhs.true1182
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	%r12, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_284
# BB#281:                               # %do.body1186
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_283
# BB#282:                               # %if.then1191
                                        #   in Loop: Header=BB14_73 Depth=2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_283:                             # %if.end1195
                                        #   in Loop: Header=BB14_73 Depth=2
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
	jmp	.LBB14_284
.LBB14_79:                              #   in Loop: Header=BB14_73 Depth=2
	movq	%rbx, %r15
	movq	24(%r12), %rbx
	jmp	.LBB14_277
	.align	16, 0x90
.LBB14_276:                             # %for.body.i1483
                                        #   in Loop: Header=BB14_277 Depth=3
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB14_277:                             # %for.body.i1483
                                        #   Parent Loop BB14_2 Depth=1
                                        #     Parent Loop BB14_73 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r15, %rbx
	jne	.LBB14_276
.LBB14_278:                             # %ggc_mark_rtx_children.exit1484
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	%r12, %rdi
	callq	ggc_mark_rtx_children_1
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	(%rsp), %rbx            # 8-byte Reload
	.align	16, 0x90
.LBB14_284:                             # %while.cond1154.backedge
                                        #   in Loop: Header=BB14_73 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB14_73
	jmp	.LBB14_67
.LBB14_84:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	andq	%r15, %rax
	cmpq	$116, %rax
	jne	.LBB14_85
# BB#182:                               # %do.body664
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	32(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_187
# BB#183:                               # %land.lhs.true670
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_187
# BB#184:                               # %do.body674
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_186
# BB#185:                               # %if.then679
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_186:                             # %if.end683
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_187:                             # %do.body692
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_192
# BB#188:                               # %land.lhs.true698
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_192
# BB#189:                               # %do.body702
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_191
# BB#190:                               # %if.then707
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_191:                             # %if.end711
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_192:                             # %do.body720
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	48(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_197
# BB#193:                               # %land.lhs.true726
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_197
# BB#194:                               # %do.body730
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_196
# BB#195:                               # %if.then735
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_196:                             # %if.end739
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_197:                             # %do.body748
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_202
# BB#198:                               # %land.lhs.true753
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_202
# BB#199:                               # %do.body757
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_201
# BB#200:                               # %if.then762
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_201:                             # %if.end766
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_202:                             # %do.body775
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_207
# BB#203:                               # %land.lhs.true780
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_207
# BB#204:                               # %do.body784
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_206
# BB#205:                               # %if.then789
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_206:                             # %if.end793
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_207:                             # %do.body802
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_212
# BB#208:                               # %land.lhs.true807
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_212
# BB#209:                               # %do.body811
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_211
# BB#210:                               # %if.then816
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_211:                             # %if.end820
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_212:                             # %do.body829
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	96(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_217
# BB#213:                               # %land.lhs.true835
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_217
# BB#214:                               # %do.body839
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_216
# BB#215:                               # %if.then844
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_216:                             # %if.end848
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_217:                             # %do.body857
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	104(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_222
# BB#218:                               # %land.lhs.true862
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_222
# BB#219:                               # %do.body866
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_221
# BB#220:                               # %if.then871
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_221:                             # %if.end875
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_222:                             # %do.body884
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	112(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_227
# BB#223:                               # %land.lhs.true889
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_227
# BB#224:                               # %do.body893
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_226
# BB#225:                               # %if.then898
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_226:                             # %if.end902
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_227:                             # %do.body911
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	120(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_232
# BB#228:                               # %land.lhs.true916
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_232
# BB#229:                               # %do.body920
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_231
# BB#230:                               # %if.then925
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_231:                             # %if.end929
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_232:                             # %do.body938
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	128(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_237
# BB#233:                               # %land.lhs.true943
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_237
# BB#234:                               # %do.body947
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_236
# BB#235:                               # %if.then952
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_236:                             # %if.end956
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_237:                             # %do.body965
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	136(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_242
# BB#238:                               # %land.lhs.true970
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_242
# BB#239:                               # %do.body974
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_241
# BB#240:                               # %if.then979
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_241:                             # %if.end983
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_242:                             # %do.body992
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	144(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_65
# BB#243:                               # %land.lhs.true998
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_65
# BB#244:                               # %do.body1002
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_246
# BB#245:                               # %if.then1007
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_246:                             # %if.end1011
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB14_65
.LBB14_85:                              # %sw.epilog
                                        #   in Loop: Header=BB14_2 Depth=1
	cmpq	$120, %rax
	jne	.LBB14_67
# BB#86:                                # %sw.bb1206
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	jmp	.LBB14_66
.LBB14_263:                             # %do.body1130
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	24(%rax), %r14
	testq	%r14, %r14
	je	.LBB14_67
# BB#264:                               # %land.lhs.true1135
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_67
# BB#265:                               # %if.then1138
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	(%r14), %eax
	addq	$-32, %rax
	andq	%r15, %rax
	movq	%r15, %r12
	cmpq	$5, %rax
	ja	.LBB14_273
# BB#266:                               # %sw.bb.i1435
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_273
.LBB14_267:                             # %land.lhs.true.i1443
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB14_268
# BB#269:                               # %for.inc.i1448
                                        #   in Loop: Header=BB14_267 Depth=2
	movq	24(%rbx), %rbx
	xorq	%r15, %r15
	testq	%rbx, %rbx
	jne	.LBB14_267
# BB#270:                               # %for.cond7.preheader.i1439
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r14), %rbx
	jmp	.LBB14_272
.LBB14_87:                              # %do.body223
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_92
# BB#88:                                # %land.lhs.true228
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_92
# BB#89:                                # %do.body232
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_91
# BB#90:                                # %if.then237
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_91:                              # %if.end241
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_92:                              # %do.body250
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_97
# BB#93:                                # %land.lhs.true255
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_97
# BB#94:                                # %do.body259
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_96
# BB#95:                                # %if.then264
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_96:                              # %if.end268
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_97:                              # %do.body277
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	72(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_102
# BB#98:                                # %land.lhs.true282
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_102
# BB#99:                                # %do.body286
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_101
# BB#100:                               # %if.then291
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_101:                             # %if.end295
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_102:                             # %do.body304
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	80(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_107
# BB#103:                               # %land.lhs.true309
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_107
# BB#104:                               # %do.body313
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_106
# BB#105:                               # %if.then318
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_106:                             # %if.end322
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_107:                             # %do.body331
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	88(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_112
# BB#108:                               # %land.lhs.true336
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_112
# BB#109:                               # %do.body340
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_111
# BB#110:                               # %if.then345
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_111:                             # %if.end349
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_112:                             # %do.body358
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	96(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_117
# BB#113:                               # %land.lhs.true363
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_117
# BB#114:                               # %do.body367
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_116
# BB#115:                               # %if.then372
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_116:                             # %if.end376
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_117:                             # %do.body385
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	104(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_122
# BB#118:                               # %land.lhs.true390
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_122
# BB#119:                               # %do.body394
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_121
# BB#120:                               # %if.then399
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_121:                             # %if.end403
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_122:                             # %do.body412
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	112(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_127
# BB#123:                               # %land.lhs.true417
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_127
# BB#124:                               # %do.body421
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_126
# BB#125:                               # %if.then426
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_126:                             # %if.end430
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_127:                             # %do.body439
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	128(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_132
# BB#128:                               # %land.lhs.true444
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_132
# BB#129:                               # %do.body448
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_131
# BB#130:                               # %if.then453
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_131:                             # %if.end457
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_132:                             # %do.body466
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	136(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_137
# BB#133:                               # %land.lhs.true471
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_137
# BB#134:                               # %do.body475
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_136
# BB#135:                               # %if.then480
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_136:                             # %if.end484
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_137:                             # %do.end492
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %r12
	movq	144(%r14), %r15
	testq	%r15, %r15
	je	.LBB14_148
# BB#138:                               # %land.lhs.true508
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB14_148
# BB#139:                               # %if.then511
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, %r13
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r12, %rax
	cmpq	$5, %rax
	ja	.LBB14_147
# BB#140:                               # %sw.bb.i1377
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB14_147
.LBB14_141:                             # %land.lhs.true.i1385
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB14_142
# BB#143:                               # %for.inc.i1390
                                        #   in Loop: Header=BB14_141 Depth=2
	movq	24(%rbx), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	jne	.LBB14_141
# BB#144:                               # %for.cond7.preheader.i1381
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB14_146
.LBB14_268:                             #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %r15
	movq	24(%r14), %rbx
	jmp	.LBB14_272
	.align	16, 0x90
.LBB14_271:                             # %for.body.i1454
                                        #   in Loop: Header=BB14_272 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB14_272:                             # %for.body.i1454
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r15, %rbx
	jne	.LBB14_271
.LBB14_273:                             # %ggc_mark_rtx_children.exit1455
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_mark_rtx_children_1
	movq	%r12, %r15
	jmp	.LBB14_67
.LBB14_142:                             #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %r14
	movq	24(%r15), %rbx
	jmp	.LBB14_146
	.align	16, 0x90
.LBB14_145:                             # %for.body.i1396
                                        #   in Loop: Header=BB14_146 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB14_146:                             # %for.body.i1396
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB14_145
.LBB14_147:                             # %ggc_mark_rtx_children.exit1397
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	movq	%r13, %r14
.LBB14_148:                             # %do.body515
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	152(%r14), %r15
	testq	%r15, %r15
	je	.LBB14_159
# BB#149:                               # %land.lhs.true520
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB14_159
# BB#150:                               # %if.then523
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, %r13
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r12, %rax
	cmpq	$5, %rax
	ja	.LBB14_158
# BB#151:                               # %sw.bb.i1406
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB14_158
.LBB14_152:                             # %land.lhs.true.i1414
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB14_153
# BB#154:                               # %for.inc.i1419
                                        #   in Loop: Header=BB14_152 Depth=2
	movq	24(%rbx), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	jne	.LBB14_152
# BB#155:                               # %for.cond7.preheader.i1410
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB14_157
.LBB14_153:                             #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %r14
	movq	24(%r15), %rbx
	jmp	.LBB14_157
	.align	16, 0x90
.LBB14_156:                             # %for.body.i1425
                                        #   in Loop: Header=BB14_157 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB14_157:                             # %for.body.i1425
                                        #   Parent Loop BB14_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB14_156
.LBB14_158:                             # %ggc_mark_rtx_children.exit1426
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	movq	%r13, %r14
.LBB14_159:                             # %do.body526
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	184(%r14), %rbx
	testq	%rbx, %rbx
	movq	%r12, %r15
	je	.LBB14_164
# BB#160:                               # %land.lhs.true531
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_164
# BB#161:                               # %do.body535
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_163
# BB#162:                               # %if.then540
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_163:                             # %if.end544
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_164:                             # %do.end552
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	120(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_169
# BB#165:                               # %land.lhs.true570
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_169
# BB#166:                               # %do.body574
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_168
# BB#167:                               # %if.then579
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_168:                             # %if.end583
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_169:                             # %if.end592
                                        #   in Loop: Header=BB14_2 Depth=1
	movzbl	16(%r14), %eax
	cmpq	$30, %rax
	jne	.LBB14_65
# BB#170:                               # %do.body600
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	168(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_175
# BB#171:                               # %land.lhs.true605
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_175
# BB#172:                               # %do.body609
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_174
# BB#173:                               # %if.then614
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_174:                             # %if.end618
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_175:                             # %do.body627
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	176(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB14_180
# BB#176:                               # %land.lhs.true632
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB14_180
# BB#177:                               # %do.body636
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB14_179
# BB#178:                               # %if.then641
                                        #   in Loop: Header=BB14_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB14_179:                             # %if.end645
                                        #   in Loop: Header=BB14_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB14_180:                             # %do.end653
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	160(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB14_65
# BB#181:                               # %if.then657
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	ggc_mark_struct_function
.LBB14_65:                              # %sw.bb219
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%r14, %rdi
.LBB14_66:                              # %while.cond.backedge
                                        #   in Loop: Header=BB14_2 Depth=1
	callq	lang_mark_tree
.LBB14_67:                              # %while.cond.backedge
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	ggc_pending_trees(%rip), %rcx
	movq	8(%rcx), %rax
	leaq	8(%rcx), %rdx
	testq	%rax, %rax
	jne	.LBB14_2
.LBB14_68:                              # %while.end1208
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp146:
	.size	ggc_mark_trees, .Ltmp146-ggc_mark_trees
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_247
	.quad	.LBB14_263
	.quad	.LBB14_87
	.quad	.LBB14_71

	.text
	.align	16, 0x90
	.type	ggc_htab_delete,@function
ggc_htab_delete:                        # @ggc_htab_delete
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp150:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp151:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp152:
	.cfi_def_cfa_offset 32
.Ltmp153:
	.cfi_offset %rbx, -24
.Ltmp154:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	(%r14), %rdi
	movq	%rsi, %rbx
	callq	*16(%rbx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB15_1
# BB#2:                                 # %if.else
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB15_4
# BB#3:                                 # %if.then2
	movq	(%r14), %rdi
	callq	*%rax
	jmp	.LBB15_4
.LBB15_1:                               # %if.then
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	htab_clear_slot
.LBB15_4:                               # %if.end4
	movq	$1, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp155:
	.size	ggc_htab_delete, .Ltmp155-ggc_htab_delete
	.cfi_endproc

	.globl	ggc_mark_rtx_children
	.align	16, 0x90
	.type	ggc_mark_rtx_children,@function
ggc_mark_rtx_children:                  # @ggc_mark_rtx_children
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
	movq	%rdi, %r14
	movzbl	(%r14), %eax
	addq	$-32, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	cmpq	$5, %rax
	ja	.LBB16_6
# BB#1:                                 # %sw.bb
	movq	24(%r14), %rbx
	xorq	%r15, %r15
	jmp	.LBB16_2
	.align	16, 0x90
.LBB16_9:                               # %for.inc
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	24(%rbx), %rbx
.LBB16_2:                               # %sw.bb
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB16_3
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	je	.LBB16_9
# BB#8:
	movq	%rbx, %r15
	movq	24(%r14), %rbx
	jmp	.LBB16_5
.LBB16_3:                               # %for.cond7.preheader
	movq	24(%r14), %rbx
	jmp	.LBB16_5
	.align	16, 0x90
.LBB16_4:                               # %for.body
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB16_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%r15, %rbx
	jne	.LBB16_4
.LBB16_6:                               # %sw.epilog
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	ggc_mark_rtx_children_1  # TAILCALL
.Ltmp170:
	.size	ggc_mark_rtx_children, .Ltmp170-ggc_mark_rtx_children
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_rtx_children_1,@function
ggc_mark_rtx_children_1:                # @ggc_mark_rtx_children_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp177:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp178:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp179:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp180:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp181:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp182:
	.cfi_def_cfa_offset 96
.Ltmp183:
	.cfi_offset %rbx, -48
.Ltmp184:
	.cfi_offset %r12, -40
.Ltmp185:
	.cfi_offset %r13, -32
.Ltmp186:
	.cfi_offset %r14, -24
.Ltmp187:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB17_1:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB17_69 Depth 2
                                        #     Child Loop BB17_75 Depth 2
                                        #     Child Loop BB17_81 Depth 2
                                        #     Child Loop BB17_87 Depth 2
                                        #     Child Loop BB17_33 Depth 2
                                        #     Child Loop BB17_91 Depth 2
                                        #     Child Loop BB17_96 Depth 2
                                        #     Child Loop BB17_102 Depth 2
                                        #     Child Loop BB17_58 Depth 2
                                        #     Child Loop BB17_64 Depth 2
                                        #     Child Loop BB17_46 Depth 2
                                        #     Child Loop BB17_52 Depth 2
                                        #     Child Loop BB17_17 Depth 2
                                        #       Child Loop BB17_113 Depth 3
                                        #       Child Loop BB17_119 Depth 3
                                        #       Child Loop BB17_121 Depth 3
                                        #         Child Loop BB17_127 Depth 4
                                        #         Child Loop BB17_133 Depth 4
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	ggc_stats(%rip), %rax
	movzbl	(%r12), %ebx
	testq	%rax, %rax
	je	.LBB17_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB17_1 Depth=1
	movl	3072(%rax,%rbx,4), %ecx
	incq	%rcx
	movl	%ecx, 3072(%rax,%rbx,4)
	movq	%r12, %rdi
	callq	ggc_get_size
	movq	ggc_stats(%rip), %rcx
	addq	%rax, 4096(%rcx,%rbx,8)
.LBB17_3:                               # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$54, %rbx
	jg	.LBB17_27
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$33, %rbx
	je	.LBB17_42
# BB#5:                                 # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$36, %rbx
	jne	.LBB17_6
# BB#54:                                # %do.body19
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	40(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_16
# BB#55:                                # %land.lhs.true25
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#56:                                # %if.then28
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_53
# BB#57:                                # %sw.bb.i225
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_58
	.align	16, 0x90
.LBB17_27:                              # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$65, %rbx
	jg	.LBB17_37
# BB#28:                                # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$55, %rbx
	jne	.LBB17_16
# BB#29:                                # %do.body78
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	8(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_16
# BB#30:                                # %land.lhs.true84
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#31:                                # %if.then87
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_53
# BB#32:                                # %sw.bb.i300
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_33
	.align	16, 0x90
.LBB17_37:                              # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$70, %rbx
	je	.LBB17_88
# BB#38:                                # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$67, %rbx
	jne	.LBB17_39
# BB#65:                                # %do.body32
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_77
# BB#66:                                # %land.lhs.true38
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_77
# BB#67:                                # %if.then41
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_76
# BB#68:                                # %sw.bb.i250
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_69
.LBB17_42:                              # %do.body9
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	64(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_16
# BB#43:                                # %land.lhs.true
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#44:                                # %if.then15
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_53
# BB#45:                                # %sw.bb.i
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_46
.LBB17_6:                               # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$37, %rbx
	jne	.LBB17_16
# BB#7:                                 # %sw.bb90
                                        #   in Loop: Header=BB17_1 Depth=1
	movl	40(%r12), %ecx
	addq	$98, %rcx
	andq	%r13, %rcx
	cmpq	$19, %rcx
	ja	.LBB17_16
# BB#8:                                 # %sw.bb90
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$753664, %rax           # imm = 0xB8000
	je	.LBB17_9
# BB#92:                                # %do.body94
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	32(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_16
# BB#93:                                # %land.lhs.true100
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#94:                                # %if.then103
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_53
# BB#95:                                # %sw.bb.i325
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_96
.LBB17_88:                              # %do.body57
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r12), %rbx
.LBB17_11:                              # %do.body107
                                        #   in Loop: Header=BB17_1 Depth=1
	testq	%rbx, %rbx
	je	.LBB17_16
# BB#12:                                # %land.lhs.true113
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#13:                                # %do.body117
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB17_15
# BB#14:                                # %if.then121
                                        #   in Loop: Header=BB17_1 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB17_15:                              # %if.end125
                                        #   in Loop: Header=BB17_1 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
	jmp	.LBB17_16
.LBB17_39:                              # %if.end
                                        #   in Loop: Header=BB17_1 Depth=1
	cmpq	$66, %rbx
	jne	.LBB17_16
# BB#40:                                # %do.body3
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB17_16
# BB#41:                                # %if.then5
                                        #   in Loop: Header=BB17_1 Depth=1
	callq	ggc_set_mark
	jmp	.LBB17_16
	.align	16, 0x90
.LBB17_62:                              # %for.inc.i235
                                        #   in Loop: Header=BB17_58 Depth=2
	movq	24(%rbx), %rbx
.LBB17_58:                              # %sw.bb.i225
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_61
# BB#59:                                # %land.lhs.true.i232
                                        #   in Loop: Header=BB17_58 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_62
# BB#60:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_61:                              # %for.cond7.i239.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_64
	.align	16, 0x90
.LBB17_63:                              # %for.body.i242
                                        #   in Loop: Header=BB17_64 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_64:                              # %for.body.i242
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_63
	jmp	.LBB17_53
	.align	16, 0x90
.LBB17_50:                              # %for.inc.i
                                        #   in Loop: Header=BB17_46 Depth=2
	movq	24(%rbx), %rbx
.LBB17_46:                              # %sw.bb.i
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_49
# BB#47:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB17_46 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_50
# BB#48:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_49:                              # %for.cond7.i.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_52
	.align	16, 0x90
.LBB17_51:                              # %for.body.i
                                        #   in Loop: Header=BB17_52 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_52:                              # %for.body.i
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_51
	jmp	.LBB17_53
.LBB17_89:                              # %for.inc.i310
                                        #   in Loop: Header=BB17_33 Depth=2
	movq	24(%rbx), %rbx
.LBB17_33:                              # %sw.bb.i300
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_36
# BB#34:                                # %land.lhs.true.i307
                                        #   in Loop: Header=BB17_33 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_89
# BB#35:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_36:                              # %for.cond7.i314.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_91
	.align	16, 0x90
.LBB17_90:                              # %for.body.i317
                                        #   in Loop: Header=BB17_91 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_91:                              # %for.body.i317
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_90
	jmp	.LBB17_53
.LBB17_9:                               # %sw.bb90
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$3, %rax
	je	.LBB17_16
# BB#10:                                # %do.body107
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	32(%r12), %rbx
	jmp	.LBB17_11
.LBB17_73:                              # %for.inc.i260
                                        #   in Loop: Header=BB17_69 Depth=2
	movq	24(%rbx), %rbx
.LBB17_69:                              # %sw.bb.i250
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_72
# BB#70:                                # %land.lhs.true.i257
                                        #   in Loop: Header=BB17_69 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_73
# BB#71:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_72:                              # %for.cond7.i264.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_75
	.align	16, 0x90
.LBB17_74:                              # %for.body.i267
                                        #   in Loop: Header=BB17_75 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_75:                              # %for.body.i267
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_74
.LBB17_76:                              # %ggc_mark_rtx_children.exit268
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
.LBB17_77:                              # %do.body44
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r12), %r15
	testq	%r15, %r15
	je	.LBB17_16
# BB#78:                                # %land.lhs.true50
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_16
# BB#79:                                # %if.then53
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_53
# BB#80:                                # %sw.bb.i275
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r14, %r14
	jmp	.LBB17_81
.LBB17_85:                              # %for.inc.i285
                                        #   in Loop: Header=BB17_81 Depth=2
	movq	24(%rbx), %rbx
.LBB17_81:                              # %sw.bb.i275
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_84
# BB#82:                                # %land.lhs.true.i282
                                        #   in Loop: Header=BB17_81 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_85
# BB#83:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_84:                              # %for.cond7.i289.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_87
	.align	16, 0x90
.LBB17_86:                              # %for.body.i292
                                        #   in Loop: Header=BB17_87 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_87:                              # %for.body.i292
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_86
	jmp	.LBB17_53
.LBB17_100:                             # %for.inc.i335
                                        #   in Loop: Header=BB17_96 Depth=2
	movq	24(%rbx), %rbx
.LBB17_96:                              # %sw.bb.i325
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB17_99
# BB#97:                                # %land.lhs.true.i332
                                        #   in Loop: Header=BB17_96 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_100
# BB#98:                                #   in Loop: Header=BB17_1 Depth=1
	movq	%rbx, %r14
.LBB17_99:                              # %for.cond7.i339.preheader
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB17_102
	.align	16, 0x90
.LBB17_101:                             # %for.body.i342
                                        #   in Loop: Header=BB17_102 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_102:                             # %for.body.i342
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r14, %rbx
	jne	.LBB17_101
.LBB17_53:                              # %ggc_mark_rtx_children.exit
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	.align	16, 0x90
.LBB17_16:                              # %sw.epilog135
                                        #   in Loop: Header=BB17_1 Depth=1
	movzbl	(%r12), %eax
	movq	rtx_format(,%rax,8), %rbx
	movzbl	(%rbx), %eax
	xorq	%r15, %r15
	testq	%rax, %rax
	movabsq	$0, %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	jne	.LBB17_17
	jmp	.LBB17_26
.LBB17_111:                             # %if.else
                                        #   in Loop: Header=BB17_17 Depth=2
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB17_120
# BB#112:                               # %sw.bb.i350
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	24(%r15), %rbx
	xorq	%r12, %r12
	jmp	.LBB17_113
	.align	16, 0x90
.LBB17_117:                             # %for.inc.i360
                                        #   in Loop: Header=BB17_113 Depth=3
	movq	24(%rbx), %rbx
.LBB17_113:                             # %sw.bb.i350
                                        #   Parent Loop BB17_1 Depth=1
                                        #     Parent Loop BB17_17 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB17_116
# BB#114:                               # %land.lhs.true.i357
                                        #   in Loop: Header=BB17_113 Depth=3
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	je	.LBB17_117
# BB#115:                               #   in Loop: Header=BB17_17 Depth=2
	movq	%rbx, %r12
.LBB17_116:                             # %for.cond7.i364.preheader
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	24(%r15), %rbx
	jmp	.LBB17_119
	.align	16, 0x90
.LBB17_118:                             # %for.body.i367
                                        #   in Loop: Header=BB17_119 Depth=3
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_119:                             # %for.body.i367
                                        #   Parent Loop BB17_1 Depth=1
                                        #     Parent Loop BB17_17 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	cmpq	%r12, %rbx
	jne	.LBB17_118
.LBB17_120:                             # %ggc_mark_rtx_children.exit368
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	movq	8(%rsp), %r12           # 8-byte Reload
	movq	%r14, %rbx
	jmp	.LBB17_23
	.align	16, 0x90
.LBB17_17:                              # %for.body
                                        #   Parent Loop BB17_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB17_113 Depth 3
                                        #       Child Loop BB17_119 Depth 3
                                        #       Child Loop BB17_121 Depth 3
                                        #         Child Loop BB17_127 Depth 4
                                        #         Child Loop BB17_133 Depth 4
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$85, %rax
	jg	.LBB17_103
# BB#18:                                # %for.body
                                        #   in Loop: Header=BB17_17 Depth=2
	andq	%r13, %rax
	cmpq	$69, %rax
	jne	.LBB17_24
	jmp	.LBB17_19
	.align	16, 0x90
.LBB17_103:                             # %for.body
                                        #   in Loop: Header=BB17_17 Depth=2
	andq	%r13, %rax
	cmpq	$86, %rax
	je	.LBB17_19
# BB#104:                               # %for.body
                                        #   in Loop: Header=BB17_17 Depth=2
	cmpq	$101, %rax
	je	.LBB17_106
# BB#105:                               # %for.body
                                        #   in Loop: Header=BB17_17 Depth=2
	cmpq	$117, %rax
	jne	.LBB17_24
.LBB17_106:                             # %sw.bb141
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	%rbx, %r14
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	8(%r12,%r15,8), %r15
	testq	%r15, %r15
	je	.LBB17_107
# BB#108:                               # %land.lhs.true148
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_107
# BB#109:                               # %if.then151
                                        #   in Loop: Header=BB17_17 Depth=2
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	jne	.LBB17_111
# BB#110:                               #   in Loop: Header=BB17_17 Depth=2
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	%r14, %rbx
	jmp	.LBB17_23
	.align	16, 0x90
.LBB17_19:                              # %do.body158
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	8(%r12,%r15,8), %r14
	movq	%r14, 40(%rsp)          # 8-byte Spill
	testq	%r14, %r14
	je	.LBB17_24
# BB#20:                                # %land.lhs.true164
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_24
# BB#21:                                # %if.then167
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movslq	(%r14), %r15
	testq	%r15, %r15
	jle	.LBB17_22
	.align	16, 0x90
.LBB17_121:                             # %do.body.i
                                        #   Parent Loop BB17_1 Depth=1
                                        #     Parent Loop BB17_17 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB17_127 Depth 4
                                        #         Child Loop BB17_133 Depth 4
	movq	(%r14,%r15,8), %r12
	testq	%r12, %r12
	je	.LBB17_122
# BB#124:                               # %land.lhs.true.i374
                                        #   in Loop: Header=BB17_121 Depth=3
	movq	%r12, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB17_122
# BB#125:                               # %if.then.i
                                        #   in Loop: Header=BB17_121 Depth=3
	movzbl	(%r12), %eax
	addq	$-32, %rax
	andq	%r13, %rax
	movq	%r13, %r14
	cmpq	$5, %rax
	ja	.LBB17_134
# BB#126:                               # %sw.bb.i381
                                        #   in Loop: Header=BB17_121 Depth=3
	movq	24(%r12), %rbx
	xorq	%r13, %r13
	jmp	.LBB17_127
	.align	16, 0x90
.LBB17_131:                             # %for.inc.i391
                                        #   in Loop: Header=BB17_127 Depth=4
	movq	24(%rbx), %rbx
.LBB17_127:                             # %sw.bb.i381
                                        #   Parent Loop BB17_1 Depth=1
                                        #     Parent Loop BB17_17 Depth=2
                                        #       Parent Loop BB17_121 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	testq	%rbx, %rbx
	je	.LBB17_130
# BB#128:                               # %land.lhs.true.i388
                                        #   in Loop: Header=BB17_127 Depth=4
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r14, %rax
	je	.LBB17_131
# BB#129:                               #   in Loop: Header=BB17_121 Depth=3
	movq	%rbx, %r13
.LBB17_130:                             # %for.cond7.i395.preheader
                                        #   in Loop: Header=BB17_121 Depth=3
	movq	24(%r12), %rbx
	jmp	.LBB17_133
	.align	16, 0x90
.LBB17_132:                             # %for.body.i398
                                        #   in Loop: Header=BB17_133 Depth=4
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB17_133:                             # %for.body.i398
                                        #   Parent Loop BB17_1 Depth=1
                                        #     Parent Loop BB17_17 Depth=2
                                        #       Parent Loop BB17_121 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	cmpq	%r13, %rbx
	jne	.LBB17_132
.LBB17_134:                             # %ggc_mark_rtx_children.exit399
                                        #   in Loop: Header=BB17_121 Depth=3
	movq	%r12, %rdi
	callq	ggc_mark_rtx_children_1
	movq	%r14, %r13
	movq	40(%rsp), %r14          # 8-byte Reload
.LBB17_122:                             # %while.cond.i.backedge
                                        #   in Loop: Header=BB17_121 Depth=3
	decq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB17_121
# BB#123:                               #   in Loop: Header=BB17_17 Depth=2
	movq	8(%rsp), %r12           # 8-byte Reload
.LBB17_22:                              #   in Loop: Header=BB17_17 Depth=2
	movq	24(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB17_23
.LBB17_107:                             #   in Loop: Header=BB17_17 Depth=2
	movq	%r14, %rbx
.LBB17_23:                              # %for.inc
                                        #   in Loop: Header=BB17_17 Depth=2
	movq	32(%rsp), %r15          # 8-byte Reload
.LBB17_24:                              # %for.inc
                                        #   in Loop: Header=BB17_17 Depth=2
	movzbl	1(%rbx), %eax
	incq	%r15
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB17_17
# BB#25:                                # %do.cond
                                        #   in Loop: Header=BB17_1 Depth=1
	movq	16(%rsp), %rax          # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %r12
	jne	.LBB17_1
.LBB17_26:                              # %do.end174
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp188:
	.size	ggc_mark_rtx_children_1, .Ltmp188-ggc_mark_rtx_children_1
	.cfi_endproc

	.globl	ggc_mark_rtvec_children
	.align	16, 0x90
	.type	ggc_mark_rtvec_children,@function
ggc_mark_rtvec_children:                # @ggc_mark_rtvec_children
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp195:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp196:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp197:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp198:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp199:
	.cfi_def_cfa_offset 48
.Ltmp200:
	.cfi_offset %rbx, -48
.Ltmp201:
	.cfi_offset %r12, -40
.Ltmp202:
	.cfi_offset %r13, -32
.Ltmp203:
	.cfi_offset %r14, -24
.Ltmp204:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movslq	(%r14), %r12
	testq	%r12, %r12
	jle	.LBB18_14
	.align	16, 0x90
.LBB18_1:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB18_5 Depth 2
                                        #     Child Loop BB18_11 Depth 2
	movq	(%r14,%r12,8), %r15
	testq	%r15, %r15
	je	.LBB18_13
# BB#2:                                 # %land.lhs.true
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB18_13
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB18_1 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$5, %rax
	ja	.LBB18_12
# BB#4:                                 # %sw.bb.i
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	24(%r15), %rbx
	xorq	%r13, %r13
	jmp	.LBB18_5
	.align	16, 0x90
.LBB18_9:                               # %for.inc.i
                                        #   in Loop: Header=BB18_5 Depth=2
	movq	24(%rbx), %rbx
.LBB18_5:                               # %sw.bb.i
                                        #   Parent Loop BB18_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB18_8
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB18_5 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB18_9
# BB#7:                                 #   in Loop: Header=BB18_1 Depth=1
	movq	%rbx, %r13
.LBB18_8:                               # %for.cond7.i.preheader
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB18_11
	.align	16, 0x90
.LBB18_10:                              # %for.body.i
                                        #   in Loop: Header=BB18_11 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB18_11:                              # %for.body.i
                                        #   Parent Loop BB18_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r13, %rbx
	jne	.LBB18_10
.LBB18_12:                              # %ggc_mark_rtx_children.exit
                                        #   in Loop: Header=BB18_1 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
.LBB18_13:                              # %while.cond.backedge
                                        #   in Loop: Header=BB18_1 Depth=1
	decq	%r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB18_1
.LBB18_14:                              # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp205:
	.size	ggc_mark_rtvec_children, .Ltmp205-ggc_mark_rtvec_children
	.cfi_endproc

	.globl	ggc_mark_rtx_varray
	.align	16, 0x90
	.type	ggc_mark_rtx_varray,@function
ggc_mark_rtx_varray:                    # @ggc_mark_rtx_varray
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp212:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp213:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp214:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp215:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp216:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp217:
	.cfi_def_cfa_offset 64
.Ltmp218:
	.cfi_offset %rbx, -48
.Ltmp219:
	.cfi_offset %r12, -40
.Ltmp220:
	.cfi_offset %r13, -32
.Ltmp221:
	.cfi_offset %r14, -24
.Ltmp222:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	%rbx, %rbx
	je	.LBB19_15
# BB#1:                                 # %if.then
	movq	(%rbx), %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	addq	%r14, %r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB19_15
# BB#2:                                 # %do.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	.align	16, 0x90
.LBB19_3:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_7 Depth 2
                                        #     Child Loop BB19_12 Depth 2
	movq	32(%rbx,%r13,8), %r15
	testq	%r15, %r15
	je	.LBB19_14
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r14, %rax
	jne	.LBB19_14
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB19_3 Depth=1
	movzbl	(%r15), %eax
	addq	$-32, %rax
	andq	%r14, %rax
	cmpq	$5, %rax
	ja	.LBB19_13
# BB#6:                                 # %sw.bb.i
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB19_13
	.align	16, 0x90
.LBB19_7:                               # %land.lhs.true.i
                                        #   Parent Loop BB19_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r14, %rax
	jne	.LBB19_8
# BB#9:                                 # %for.inc.i
                                        #   in Loop: Header=BB19_7 Depth=2
	movq	24(%rbx), %rbx
	xorq	%r12, %r12
	testq	%rbx, %rbx
	jne	.LBB19_7
# BB#10:                                # %for.cond7.preheader.i
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	24(%r15), %rbx
	jmp	.LBB19_12
.LBB19_8:                               #   in Loop: Header=BB19_3 Depth=1
	movq	%rbx, %r12
	movq	24(%r15), %rbx
	jmp	.LBB19_12
	.align	16, 0x90
.LBB19_11:                              # %for.body.i
                                        #   in Loop: Header=BB19_12 Depth=2
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children_1
	movq	24(%rbx), %rbx
.LBB19_12:                              # %for.body.i
                                        #   Parent Loop BB19_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%r12, %rbx
	jne	.LBB19_11
.LBB19_13:                              # %ggc_mark_rtx_children.exit
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children_1
	movq	8(%rsp), %rbx           # 8-byte Reload
.LBB19_14:                              # %for.inc
                                        #   in Loop: Header=BB19_3 Depth=1
	movq	$32, %rcx
	leaq	-1(%r13), %rax
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	testq	%r13, %r13
	movq	%rax, %r13
	jg	.LBB19_3
.LBB19_15:                              # %if.end6
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp223:
	.size	ggc_mark_rtx_varray, .Ltmp223-ggc_mark_rtx_varray
	.cfi_endproc

	.globl	ggc_mark_tree_varray
	.align	16, 0x90
	.type	ggc_mark_tree_varray,@function
ggc_mark_tree_varray:                   # @ggc_mark_tree_varray
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp229:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp230:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp231:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp233:
	.cfi_def_cfa_offset 48
.Ltmp234:
	.cfi_offset %rbx, -40
.Ltmp235:
	.cfi_offset %r12, -32
.Ltmp236:
	.cfi_offset %r14, -24
.Ltmp237:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB20_9
# BB#1:                                 # %if.then
	movq	(%r14), %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	addq	%r15, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB20_9
# BB#2:                                 # %do.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB20_3:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14,%rbx,8), %r12
	testq	%r12, %r12
	je	.LBB20_8
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	%r12, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB20_8
# BB#5:                                 # %do.body6
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB20_7
# BB#6:                                 # %if.then10
                                        #   in Loop: Header=BB20_3 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB20_7:                               # %if.end
                                        #   in Loop: Header=BB20_3 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r12, 32(%rax,%rcx,8)
.LBB20_8:                               # %for.inc
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	jg	.LBB20_3
.LBB20_9:                               # %if.end19
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp238:
	.size	ggc_mark_tree_varray, .Ltmp238-ggc_mark_tree_varray
	.cfi_endproc

	.globl	ggc_mark_tree_hash_table
	.align	16, 0x90
	.type	ggc_mark_tree_hash_table,@function
ggc_mark_tree_hash_table:               # @ggc_mark_tree_hash_table
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$ggc_mark_tree_hash_table_entry, %rsi
	xorq	%rdx, %rdx
	jmp	hash_traverse  # TAILCALL
.Ltmp239:
	.size	ggc_mark_tree_hash_table, .Ltmp239-ggc_mark_tree_hash_table
	.cfi_endproc

	.align	16, 0x90
	.type	ggc_mark_tree_hash_table_entry,@function
ggc_mark_tree_hash_table_entry:         # @ggc_mark_tree_hash_table_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp242:
	.cfi_def_cfa_offset 16
.Ltmp243:
	.cfi_offset %rbx, -16
	movq	8(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB22_5
# BB#1:                                 # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB22_5
# BB#2:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB22_4
# BB#3:                                 # %if.then3
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB22_4:                               # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB22_5:                               # %do.end8
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp244:
	.size	ggc_mark_tree_hash_table_entry, .Ltmp244-ggc_mark_tree_hash_table_entry
	.cfi_endproc

	.globl	ggc_alloc_cleared
	.align	16, 0x90
	.type	ggc_alloc_cleared,@function
ggc_alloc_cleared:                      # @ggc_alloc_cleared
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp248:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp249:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp250:
	.cfi_def_cfa_offset 32
.Ltmp251:
	.cfi_offset %rbx, -24
.Ltmp252:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	ggc_alloc
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	memset
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp253:
	.size	ggc_alloc_cleared, .Ltmp253-ggc_alloc_cleared
	.cfi_endproc

	.globl	ggc_print_common_statistics
	.align	16, 0x90
	.type	ggc_print_common_statistics,@function
ggc_print_common_statistics:            # @ggc_print_common_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp260:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp261:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp262:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp263:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp264:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp265:
	.cfi_def_cfa_offset 80
.Ltmp266:
	.cfi_offset %rbx, -48
.Ltmp267:
	.cfi_offset %r12, -40
.Ltmp268:
	.cfi_offset %r13, -32
.Ltmp269:
	.cfi_offset %r14, -24
.Ltmp270:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%rbx, ggc_stats(%rip)
	callq	ggc_collect
	movl	6160(%rbx), %eax
	movq	6144(%rbx), %rcx
	movq	$-1024, %rdx            # imm = 0xFFFFFFFFFFFFFC00
	.align	16, 0x90
.LBB24_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	1024(%rbx,%rdx), %esi
	addq	%rsi, %rax
	movl	%eax, 6160(%rbx)
	movq	3072(%rbx,%rdx,2), %rsi
	addq	%rsi, %rcx
	movq	%rcx, 6144(%rbx)
	addq	$4, %rdx
	jne	.LBB24_1
# BB#2:                                 # %for.cond4.preheader
	movl	6164(%rbx), %eax
	movq	6152(%rbx), %rcx
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB24_3:                               # %for.body6
                                        # =>This Inner Loop Header: Depth=1
	movl	3072(%rbx,%rdx,4), %esi
	addq	%rsi, %rax
	movl	%eax, 6164(%rbx)
	movq	4096(%rbx,%rdx,8), %rsi
	addq	%rsi, %rcx
	movq	%rcx, 6152(%rbx)
	incq	%rdx
	cmpq	$153, %rdx
	jne	.LBB24_3
# BB#4:                                 # %for.end15
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str3, %rdx
	movabsq	$.L.str4, %rcx
	movabsq	$.L.str5, %r8
	movabsq	$.L.str6, %r9
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	fprintf
	.align	16, 0x90
.LBB24_5:                               # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	ggc_stats(%rip), %rbx
	movl	(%rbx,%r13), %eax
	testq	%rax, %rax
	je	.LBB24_13
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB24_5 Depth=1
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	1024(%rbx,%r13,2), %rdi
	movq	tree_code_name(%r13,%r13), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	cmpq	$10240, %rdi            # imm = 0x2800
	movq	$32, %r14
	movq	%rdi, %r15
	jb	.LBB24_12
# BB#7:                                 # %cond.false
                                        #   in Loop: Header=BB24_5 Depth=1
	cmpq	$10485760, %rdi         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB24_9
# BB#8:                                 # %cond.false
                                        #   in Loop: Header=BB24_5 Depth=1
	movq	$20, %rcx
.LBB24_9:                               # %cond.false
                                        #   in Loop: Header=BB24_5 Depth=1
	movq	$107, %r14
	jb	.LBB24_11
# BB#10:                                # %cond.false
                                        #   in Loop: Header=BB24_5 Depth=1
	movq	$77, %r14
.LBB24_11:                              # %cond.false
                                        #   in Loop: Header=BB24_5 Depth=1
	movq	%rdi, %r15
	shrq	%cl, %r15
.LBB24_12:                              # %cond.end60
                                        #   in Loop: Header=BB24_5 Depth=1
	callq	int64_to_float64
	movq	%rax, %rdi
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	callq	float64_mul
	movq	6144(%rbx), %rdi
	movq	%rax, %r12
	callq	int64_to_float64
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str7, %rsi
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r15, %r8
	movq	%r14, %r9
	callq	fprintf
.LBB24_13:                              # %for.inc69
                                        #   in Loop: Header=BB24_5 Depth=1
	addq	$4, %r13
	cmpq	$1024, %r13             # imm = 0x400
	jne	.LBB24_5
# BB#14:                                # %for.end71
	movq	ggc_stats(%rip), %rax
	movq	6144(%rax), %r8
	movl	6160(%rax), %r10d
	cmpq	$10240, %r8             # imm = 0x2800
	movq	$32, %r9
	jb	.LBB24_20
# BB#15:                                # %cond.false78
	cmpq	$10485760, %r8          # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB24_17
# BB#16:                                # %cond.false78
	movq	$20, %rcx
.LBB24_17:                              # %cond.false78
	movq	$107, %r9
	jb	.LBB24_19
# BB#18:                                # %cond.false78
	movq	$77, %r9
.LBB24_19:                              # %cond.false78
	shrq	%cl, %r8
.LBB24_20:                              # %cond.end101
	movabsq	$.L.str8, %rsi
	movabsq	$.L.str9, %rdx
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r10, %rcx
	callq	fprintf
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str10, %rdx
	movabsq	$.L.str4, %rcx
	movabsq	$.L.str5, %r8
	movabsq	$.L.str6, %r9
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	.align	16, 0x90
.LBB24_21:                              # %for.body108
                                        # =>This Inner Loop Header: Depth=1
	movq	ggc_stats(%rip), %rbx
	movl	3072(%rbx,%r13), %eax
	testq	%rax, %rax
	je	.LBB24_29
# BB#22:                                # %if.then113
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	4096(%rbx,%r13,2), %rdi
	cmpq	$10240, %rdi            # imm = 0x2800
	movq	$32, %r12
	movq	%rdi, %r14
	jb	.LBB24_28
# BB#23:                                # %cond.false128
                                        #   in Loop: Header=BB24_21 Depth=1
	cmpq	$10485760, %rdi         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB24_25
# BB#24:                                # %cond.false128
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	$20, %rcx
.LBB24_25:                              # %cond.false128
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	$107, %r12
	jb	.LBB24_27
# BB#26:                                # %cond.false128
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	$77, %r12
.LBB24_27:                              # %cond.false128
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	%rdi, %r14
	shrq	%cl, %r14
.LBB24_28:                              # %cond.end161
                                        #   in Loop: Header=BB24_21 Depth=1
	movq	rtx_name(%r13,%r13), %r15
	callq	int64_to_float64
	movq	%rax, %rdi
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	callq	float64_mul
	movq	6152(%rbx), %rdi
	movq	%rax, %rbx
	callq	int64_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%rcx, (%rsp)
	movq	24(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str7, %rsi
	movq	%r15, %rdx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%r14, %r8
	movq	%r12, %r9
	callq	fprintf
.LBB24_29:                              # %for.inc173
                                        #   in Loop: Header=BB24_21 Depth=1
	addq	$4, %r13
	cmpq	$612, %r13              # imm = 0x264
	jne	.LBB24_21
# BB#30:                                # %for.end175
	movq	ggc_stats(%rip), %rax
	movq	6152(%rax), %r8
	movl	6164(%rax), %ebx
	cmpq	$10240, %r8             # imm = 0x2800
	movq	$32, %r9
	jb	.LBB24_36
# BB#31:                                # %cond.false182
	cmpq	$10485760, %r8          # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB24_33
# BB#32:                                # %cond.false182
	movq	$20, %rcx
.LBB24_33:                              # %cond.false182
	movq	$107, %r9
	jb	.LBB24_35
# BB#34:                                # %cond.false182
	movq	$77, %r9
.LBB24_35:                              # %cond.false182
	shrq	%cl, %r8
.LBB24_36:                              # %cond.end205
	movabsq	$.L.str8, %rsi
	movabsq	$.L.str9, %rdx
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rcx
	callq	fprintf
	movq	$0, ggc_stats(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp271:
	.size	ggc_print_common_statistics, .Ltmp271-ggc_print_common_statistics
	.cfi_endproc

	.type	roots,@object           # @roots
	.local	roots
	.comm	roots,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"ggc-common.c"
	.size	.L.str, 13

	.type	.L__FUNCTION__.ggc_del_root,@object # @__FUNCTION__.ggc_del_root
.L__FUNCTION__.ggc_del_root:
	.asciz	"ggc_del_root"
	.size	.L__FUNCTION__.ggc_del_root, 13

	.type	d_htab_roots,@object    # @d_htab_roots
	.local	d_htab_roots
	.comm	d_htab_roots,8,8
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"ggc_pending_trees"
	.size	.L.str1, 18

	.type	ggc_pending_trees,@object # @ggc_pending_trees
	.comm	ggc_pending_trees,8,8
	.type	ggc_stats,@object       # @ggc_stats
	.local	ggc_stats
	.comm	ggc_stats,8,8
	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"\n%-17s%10s %16s %10s\n"
	.size	.L.str2, 22

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Tree"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Number"
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Bytes"
	.size	.L.str5, 6

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"% Total"
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%-17s%10u%16ld%c %10.3f\n"
	.size	.L.str7, 25

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%-17s%10u%16ld%c\n"
	.size	.L.str8, 18

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Total"
	.size	.L.str9, 6

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"RTX"
	.size	.L.str10, 4

	.type	lang_mark_false_label_stack,@object # @lang_mark_false_label_stack
	.comm	lang_mark_false_label_stack,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
