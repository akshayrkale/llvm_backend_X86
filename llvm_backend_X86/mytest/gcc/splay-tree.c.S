	.file	"splay-tree.c.bc"
	.text
	.globl	splay_tree_new
	.align	16, 0x90
	.type	splay_tree_new,@function
splay_tree_new:                         # @splay_tree_new
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
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	$56, %rdi
	callq	xmalloc
	movq	$0, (%rax)
	movq	%rbx, 8(%rax)
	movq	%r15, 16(%rax)
	movq	%r14, 24(%rax)
	movq	$splay_tree_xmalloc_allocate, 32(%rax)
	movq	$splay_tree_xmalloc_deallocate, 40(%rax)
	movq	$0, 48(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	splay_tree_new, .Ltmp10-splay_tree_new
	.cfi_endproc

	.globl	splay_tree_new_with_allocator
	.align	16, 0x90
	.type	splay_tree_new_with_allocator,@function
splay_tree_new_with_allocator:          # @splay_tree_new_with_allocator
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp22:
	.cfi_def_cfa_offset 64
.Ltmp23:
	.cfi_offset %rbx, -48
.Ltmp24:
	.cfi_offset %r12, -40
.Ltmp25:
	.cfi_offset %r13, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%r9, %r15
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, %rbx
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%rdi, %r14
	movq	$56, %rdi
	movq	%r15, %rsi
	callq	*%rbx
	movq	$0, (%rax)
	movq	%r14, 8(%rax)
	movq	%r13, 16(%rax)
	movq	%r12, 24(%rax)
	movq	%rbx, 32(%rax)
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 40(%rax)
	movq	%r15, 48(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	splay_tree_new_with_allocator, .Ltmp28-splay_tree_new_with_allocator
	.cfi_endproc

	.align	16, 0x90
	.type	splay_tree_xmalloc_allocate,@function
splay_tree_xmalloc_allocate:            # @splay_tree_xmalloc_allocate
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	jmp	xmalloc  # TAILCALL
.Ltmp29:
	.size	splay_tree_xmalloc_allocate, .Ltmp29-splay_tree_xmalloc_allocate
	.cfi_endproc

	.align	16, 0x90
	.type	splay_tree_xmalloc_deallocate,@function
splay_tree_xmalloc_deallocate:          # @splay_tree_xmalloc_deallocate
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	free  # TAILCALL
.Ltmp30:
	.size	splay_tree_xmalloc_deallocate, .Ltmp30-splay_tree_xmalloc_deallocate
	.cfi_endproc

	.globl	splay_tree_delete
	.align	16, 0x90
	.type	splay_tree_delete,@function
splay_tree_delete:                      # @splay_tree_delete
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp33:
	.cfi_def_cfa_offset 16
.Ltmp34:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	(%rbx), %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	splay_tree_delete_helper
	movq	40(%rbx), %rax
	movq	48(%rbx), %rsi
	movq	%rbx, %rdi
	popq	%rbx
	jmpq	*%rax  # TAILCALL
.Ltmp35:
	.size	splay_tree_delete, .Ltmp35-splay_tree_delete
	.cfi_endproc

	.align	16, 0x90
	.type	splay_tree_delete_helper,@function
splay_tree_delete_helper:               # @splay_tree_delete_helper
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
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%r14, %r14
	je	.LBB5_6
# BB#1:                                 # %if.end
	movq	16(%r14), %rsi
	movq	%rbx, %rdi
	callq	splay_tree_delete_helper
	movq	24(%r14), %rsi
	movq	%rbx, %rdi
	callq	splay_tree_delete_helper
	movq	16(%rbx), %rax
	testq	%rax, %rax
	je	.LBB5_3
# BB#2:                                 # %if.then2
	movq	(%r14), %rdi
	callq	*%rax
.LBB5_3:                                # %if.end4
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB5_5
# BB#4:                                 # %if.then6
	movq	8(%r14), %rdi
	callq	*%rax
.LBB5_5:                                # %if.end8
	movq	40(%rbx), %rax
	movq	48(%rbx), %rsi
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmpq	*%rax  # TAILCALL
.LBB5_6:                                # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp44:
	.size	splay_tree_delete_helper, .Ltmp44-splay_tree_delete_helper
	.cfi_endproc

	.globl	splay_tree_insert
	.align	16, 0x90
	.type	splay_tree_insert,@function
splay_tree_insert:                      # @splay_tree_insert
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
	movq	%rdi, %rbx
	xorq	%r12, %r12
	cmpq	$0, (%rbx)
	movq	%rdx, %r14
	movq	%rsi, %r15
	je	.LBB6_7
# BB#1:                                 # %splay_tree_splay.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r12, %r12
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_splay_helper
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB6_7
# BB#2:                                 # %if.end
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	*8(%rbx)
	movq	(%rbx), %rdx
	movq	%rax, %r12
	testq	%rdx, %rdx
	je	.LBB6_7
# BB#3:                                 # %if.end
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB6_7
# BB#4:                                 # %if.then5
	movq	24(%rbx), %rax
	testq	%rax, %rax
	je	.LBB6_6
# BB#5:                                 # %if.then7
	movq	8(%rdx), %rdi
	callq	*%rax
	movq	(%rbx), %rdx
.LBB6_6:                                # %if.end11
	movq	%r14, 8(%rdx)
	jmp	.LBB6_13
.LBB6_7:                                # %if.else
	movq	48(%rbx), %rsi
	movq	$32, %rdi
	callq	*32(%rbx)
	movq	%rax, %rdx
	movq	%r15, (%rdx)
	movq	%r14, 8(%rdx)
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB6_8
# BB#9:                                 # %if.else20
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	testq	%r12, %r12
	js	.LBB6_10
# BB#11:                                # %if.else30
	movq	%rax, 24(%rdx)
	movq	16(%rax), %rcx
	movq	%rcx, 16(%rdx)
	movq	$0, 16(%rax)
	jmp	.LBB6_12
.LBB6_8:                                # %if.then19
	movq	$0, 24(%rdx)
	movq	$0, 16(%rdx)
	jmp	.LBB6_12
.LBB6_10:                               # %if.then22
	movq	%rax, 16(%rdx)
	movq	24(%rax), %rcx
	movq	%rcx, 24(%rdx)
	movq	$0, 24(%rax)
.LBB6_12:                               # %if.end39
	movq	%rdx, (%rbx)
.LBB6_13:                               # %if.end41
	movq	%rdx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp59:
	.size	splay_tree_insert, .Ltmp59-splay_tree_insert
	.cfi_endproc

	.globl	splay_tree_remove
	.align	16, 0x90
	.type	splay_tree_remove,@function
splay_tree_remove:                      # @splay_tree_remove
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp64:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 32
.Ltmp67:
	.cfi_offset %rbx, -32
.Ltmp68:
	.cfi_offset %r14, -24
.Ltmp69:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	cmpq	$0, (%r15)
	movq	%rsi, %r14
	je	.LBB7_10
# BB#1:                                 # %splay_tree_splay.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	splay_tree_splay_helper
	movq	(%r15), %rax
	testq	%rax, %rax
	je	.LBB7_10
# BB#2:                                 # %land.lhs.true
	movq	(%rax), %rdi
	movq	%r14, %rsi
	callq	*8(%r15)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_10
# BB#3:                                 # %if.then
	movq	(%r15), %rdi
	movq	24(%r15), %rax
	movq	16(%rdi), %rbx
	movq	24(%rdi), %r14
	testq	%rax, %rax
	je	.LBB7_5
# BB#4:                                 # %if.then8
	movq	8(%rdi), %rdi
	callq	*%rax
	movq	(%r15), %rdi
.LBB7_5:                                # %if.end
	movq	48(%r15), %rsi
	callq	*40(%r15)
	testq	%rbx, %rbx
	je	.LBB7_9
# BB#6:                                 # %if.then13
	movq	%rbx, (%r15)
	testq	%r14, %r14
	je	.LBB7_10
	.align	16, 0x90
.LBB7_7:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rax
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_7
# BB#8:                                 # %while.end
	movq	%r14, 24(%rax)
	jmp	.LBB7_10
.LBB7_9:                                # %if.else
	movq	%r14, (%r15)
.LBB7_10:                               # %if.end24
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	splay_tree_remove, .Ltmp70-splay_tree_remove
	.cfi_endproc

	.globl	splay_tree_lookup
	.align	16, 0x90
	.type	splay_tree_lookup,@function
splay_tree_lookup:                      # @splay_tree_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp77:
	.cfi_def_cfa_offset 32
.Ltmp78:
	.cfi_offset %rbx, -32
.Ltmp79:
	.cfi_offset %r14, -24
.Ltmp80:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	xorq	%r14, %r14
	cmpq	$0, (%rbx)
	movq	%rsi, %r15
	je	.LBB8_4
# BB#1:                                 # %splay_tree_splay.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	splay_tree_splay_helper
	movq	(%rbx), %rax
	testq	%rax, %rax
	je	.LBB8_4
# BB#2:                                 # %land.lhs.true
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	*8(%rbx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_4
# BB#3:                                 # %if.then
	movq	(%rbx), %r14
.LBB8_4:                                # %return
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp81:
	.size	splay_tree_lookup, .Ltmp81-splay_tree_lookup
	.cfi_endproc

	.globl	splay_tree_max
	.align	16, 0x90
	.type	splay_tree_max,@function
splay_tree_max:                         # @splay_tree_max
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	jmp	.LBB9_2
	.align	16, 0x90
.LBB9_1:                                # %while.cond
                                        #   in Loop: Header=BB9_2 Depth=1
	movq	%rcx, %rax
	movq	24(%rax), %rcx
.LBB9_2:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB9_1
# BB#3:                                 # %return
	retq
.Ltmp82:
	.size	splay_tree_max, .Ltmp82-splay_tree_max
	.cfi_endproc

	.globl	splay_tree_min
	.align	16, 0x90
	.type	splay_tree_min,@function
splay_tree_min:                         # @splay_tree_min
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rcx
	xorq	%rax, %rax
	jmp	.LBB10_2
	.align	16, 0x90
.LBB10_1:                               # %while.cond
                                        #   in Loop: Header=BB10_2 Depth=1
	movq	%rcx, %rax
	movq	16(%rax), %rcx
.LBB10_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	jne	.LBB10_1
# BB#3:                                 # %return
	retq
.Ltmp83:
	.size	splay_tree_min, .Ltmp83-splay_tree_min
	.cfi_endproc

	.globl	splay_tree_predecessor
	.align	16, 0x90
	.type	splay_tree_predecessor,@function
splay_tree_predecessor:                 # @splay_tree_predecessor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp88:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp89:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 32
.Ltmp91:
	.cfi_offset %rbx, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	cmpq	$0, (%r14)
	movq	%rsi, %r15
	je	.LBB11_6
# BB#1:                                 # %splay_tree_splay.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	splay_tree_splay_helper
	movq	(%r14), %rax
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	*8(%r14)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	(%r14), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB11_2
# BB#3:                                 # %if.end5
	movq	16(%rdx), %rax
	jmp	.LBB11_5
	.align	16, 0x90
.LBB11_4:                               # %while.cond
                                        #   in Loop: Header=BB11_5 Depth=1
	movq	%rax, %rbx
	movq	24(%rbx), %rax
.LBB11_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB11_4
	jmp	.LBB11_6
.LBB11_2:
	movq	%rdx, %rbx
.LBB11_6:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp94:
	.size	splay_tree_predecessor, .Ltmp94-splay_tree_predecessor
	.cfi_endproc

	.globl	splay_tree_successor
	.align	16, 0x90
	.type	splay_tree_successor,@function
splay_tree_successor:                   # @splay_tree_successor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp99:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp100:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 32
.Ltmp102:
	.cfi_offset %rbx, -32
.Ltmp103:
	.cfi_offset %r14, -24
.Ltmp104:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	cmpq	$0, (%r14)
	movq	%rsi, %r15
	je	.LBB12_6
# BB#1:                                 # %splay_tree_splay.exit
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	splay_tree_splay_helper
	movq	(%r14), %rax
	movq	(%rax), %rdi
	movq	%r15, %rsi
	callq	*8(%r14)
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	(%r14), %rdx
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB12_3
# BB#2:
	movq	%rdx, %rbx
	jmp	.LBB12_6
.LBB12_3:                               # %if.end5
	movq	24(%rdx), %rax
	jmp	.LBB12_5
	.align	16, 0x90
.LBB12_4:                               # %while.cond
                                        #   in Loop: Header=BB12_5 Depth=1
	movq	%rax, %rbx
	movq	16(%rbx), %rax
.LBB12_5:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB12_4
.LBB12_6:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp105:
	.size	splay_tree_successor, .Ltmp105-splay_tree_successor
	.cfi_endproc

	.globl	splay_tree_foreach
	.align	16, 0x90
	.type	splay_tree_foreach,@function
splay_tree_foreach:                     # @splay_tree_foreach
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rdi
	jmp	splay_tree_foreach_helper  # TAILCALL
.Ltmp106:
	.size	splay_tree_foreach, .Ltmp106-splay_tree_foreach
	.cfi_endproc

	.align	16, 0x90
	.type	splay_tree_foreach_helper,@function
splay_tree_foreach_helper:              # @splay_tree_foreach_helper
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp112:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp113:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp114:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp116:
	.cfi_def_cfa_offset 48
.Ltmp117:
	.cfi_offset %rbx, -40
.Ltmp118:
	.cfi_offset %r12, -32
.Ltmp119:
	.cfi_offset %r14, -24
.Ltmp120:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB14_5
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB14_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	splay_tree_foreach_helper
	testq	%r12, %rax
	jne	.LBB14_6
# BB#3:                                 # %if.end3
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	*%r15
	testq	%r12, %rax
	jne	.LBB14_6
# BB#4:                                 # %if.end7
                                        #   in Loop: Header=BB14_2 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB14_2
.LBB14_5:
	xorq	%rax, %rax
.LBB14_6:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp121:
	.size	splay_tree_foreach_helper, .Ltmp121-splay_tree_foreach_helper
	.cfi_endproc

	.globl	splay_tree_compare_ints
	.align	16, 0x90
	.type	splay_tree_compare_ints,@function
splay_tree_compare_ints:                # @splay_tree_compare_ints
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jge	.LBB15_2
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB15_2:                               # %if.else
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rsi, %rdi
	jle	.LBB15_3
# BB#4:                                 # %if.else
	movabsq	$1, %rax
	retq
.LBB15_3:
	movabsq	$0, %rax
	retq
.Ltmp122:
	.size	splay_tree_compare_ints, .Ltmp122-splay_tree_compare_ints
	.cfi_endproc

	.globl	splay_tree_compare_pointers
	.align	16, 0x90
	.type	splay_tree_compare_pointers,@function
splay_tree_compare_pointers:            # @splay_tree_compare_pointers
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rsi, %rdi
	jae	.LBB16_2
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	retq
.LBB16_2:                               # %if.else
	cmpq	%rdi, %rsi
	sbbq	%rax, %rax
	andq	$1, %rax
	retq
.Ltmp123:
	.size	splay_tree_compare_pointers, .Ltmp123-splay_tree_compare_pointers
	.cfi_endproc

	.align	16, 0x90
	.type	splay_tree_splay_helper,@function
splay_tree_splay_helper:                # @splay_tree_splay_helper
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp130:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp131:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp132:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp133:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp134:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp135:
	.cfi_def_cfa_offset 64
.Ltmp136:
	.cfi_offset %rbx, -48
.Ltmp137:
	.cfi_offset %r12, -40
.Ltmp138:
	.cfi_offset %r13, -32
.Ltmp139:
	.cfi_offset %r14, -24
.Ltmp140:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	(%r12), %rbx
	movq	%rcx, %r15
	movq	%rsi, %r13
	movq	%rdi, %r14
	testq	%rbx, %rbx
	je	.LBB17_1
# BB#2:                                 # %if.end
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	(%rbx), %rsi
	movq	%r13, %rdi
	callq	*8(%r14)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_7
# BB#3:                                 # %if.else
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB17_4
# BB#5:                                 # %if.else
	addq	$24, %rbx
	jmp	.LBB17_6
.LBB17_1:                               # %if.then
	movq	(%r15), %rbx
	jmp	.LBB17_24
.LBB17_4:
	addq	$16, %rbx
.LBB17_6:                               # %if.else
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	movq	%r12, %rcx
	movq	%r15, %r8
	callq	splay_tree_splay_helper
	movq	(%r12), %rcx
	movq	%rax, %rbx
	cmpq	%rbx, %rcx
	jne	.LBB17_24
.LBB17_7:                               # %if.end14
	testq	%r15, %r15
	je	.LBB17_24
# BB#8:                                 # %if.end17
	movq	(%r15), %rax
	movq	16(%rax), %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB17_9
# BB#12:                                # %if.end29
	cmpq	%rcx, %rbx
	jne	.LBB17_15
# BB#13:                                # %land.lhs.true
	movq	(%rdi), %rdx
	movq	16(%rdx), %rsi
	cmpq	%rsi, %rax
	je	.LBB17_14
.LBB17_15:                              # %if.else41
	movq	24(%rax), %rdx
	cmpq	%rdx, %rbx
	jne	.LBB17_19
# BB#16:                                # %land.lhs.true44
	movq	(%rdi), %rdx
	movq	24(%rdx), %rsi
	cmpq	%rsi, %rax
	je	.LBB17_17
.LBB17_19:                              # %if.end56
	cmpq	%rcx, %rbx
	je	.LBB17_20
# BB#21:                                # %if.else66
	movq	16(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	(%r15), %rax
	movq	%rax, 16(%rbx)
	movq	24(%rbx), %rax
	movq	(%rdi), %rcx
	movq	%rax, 16(%rcx)
	movq	(%rdi), %rax
	jmp	.LBB17_22
.LBB17_9:                               # %if.then19
	cmpq	%rcx, %rbx
	je	.LBB17_10
# BB#11:                                # %if.else25
	movq	16(%rbx), %rax
	movq	%rax, (%r12)
	movq	(%r15), %rax
	movq	%rax, 16(%rbx)
	movq	%rbx, (%r15)
	jmp	.LBB17_24
.LBB17_20:                              # %if.then59
	movq	24(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	(%r15), %rax
	movq	%rax, 24(%rbx)
	movq	16(%rbx), %rax
	movq	(%rdi), %rcx
	movq	%rax, 24(%rcx)
	movq	(%rdi), %rax
	movq	%rax, 16(%rbx)
	jmp	.LBB17_23
.LBB17_10:                              # %if.then22
	movq	24(%rbx), %rax
	movq	%rax, (%r12)
	movq	(%r15), %rax
	movq	%rax, 24(%rbx)
	movq	%rbx, (%r15)
	jmp	.LBB17_24
.LBB17_14:                              # %if.then34
	movq	24(%rax), %rcx
	movq	%rcx, 16(%rdx)
	movq	(%rdi), %rcx
	movq	%rcx, 24(%rax)
	movq	24(%rbx), %rcx
	movq	%rcx, 16(%rax)
.LBB17_22:                              # %return
	movq	%rax, 24(%rbx)
	jmp	.LBB17_23
.LBB17_17:                              # %if.then47
	movq	%rcx, 24(%rdx)
	movq	(%rdi), %rcx
	movq	%rcx, 16(%rax)
	movq	16(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	%rax, 16(%rbx)
.LBB17_23:                              # %return
	movq	%rbx, (%rdi)
.LBB17_24:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp141:
	.size	splay_tree_splay_helper, .Ltmp141-splay_tree_splay_helper
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
