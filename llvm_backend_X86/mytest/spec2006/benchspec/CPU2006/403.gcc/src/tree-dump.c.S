	.file	"tree-dump.c.bc"
	.text
	.globl	queue_and_dump_index
	.align	16, 0x90
	.type	queue_and_dump_index,@function
queue_and_dump_index:                   # @queue_and_dump_index
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp8:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp9:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp10:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%rdx, %r12
	movq	%rdi, %rbx
	testq	%r12, %r12
	je	.LBB0_15
# BB#1:                                 # %if.end
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	56(%rbx), %rdi
	movq	%r12, %rsi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB0_3
# BB#2:                                 # %if.then2
	movq	8(%rax), %rax
	movl	(%rax), %r15d
	jmp	.LBB0_10
.LBB0_3:                                # %if.else
	movl	20(%rbx), %r15d
	incq	%r15
	movl	%r15d, 20(%rbx)
	movq	48(%rbx), %r14
	testq	%r14, %r14
	je	.LBB0_5
# BB#4:                                 # %if.then.i
	movq	8(%r14), %rax
	movq	%rax, 48(%rbx)
	jmp	.LBB0_6
.LBB0_5:                                # %if.else.i
	movq	$16, %rdi
	callq	xmalloc
	movq	%rax, %r14
.LBB0_6:                                # %if.end.i
	movq	$8, %rdi
	callq	xmalloc
	movl	%r15d, (%rax)
	movzbl	4(%rax), %ecx
	andq	$1, %r13
	andq	$254, %rcx
	orq	%r13, %rcx
	movb	%cl, 4(%rax)
	movq	56(%rbx), %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	splay_tree_insert
	movq	%rax, (%r14)
	movq	$0, 8(%r14)
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.LBB0_7
# BB#8:                                 # %if.else10.i
	movq	%r14, 8(%rax)
	jmp	.LBB0_9
.LBB0_7:                                # %if.then9.i
	movq	%r14, 32(%rbx)
.LBB0_9:                                # %queue.exit
	movq	%r14, 40(%rbx)
.LBB0_10:                               # %if.end5
	movl	24(%rbx), %eax
	cmpq	$56, %rax
	jb	.LBB0_12
# BB#11:                                # %if.then.i15
	movq	(%rbx), %rdi
	movabsq	$.L.str105, %rsi
	movabsq	$.L.str104, %rcx
	movq	$25, %rdx
	xorq	%rax, %rax
	movq	$25, %r14
	callq	fprintf
	movl	%r14d, 24(%rbx)
	jmp	.LBB0_14
.LBB0_12:                               # %if.else.i16
	addq	$-25, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	movq	%r14, %rax
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r14
	je	.LBB0_14
# BB#13:                                # %if.then3.i
	movq	(%rbx), %rdi
	xorq	$15, %r14
	movabsq	$.L.str103, %rsi
	movabsq	$.L.str104, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movl	24(%rbx), %eax
	addq	%r14, %rax
	movl	%eax, 24(%rbx)
.LBB0_14:                               # %dump_maybe_newline.exit
	movq	(%rbx), %rdi
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	callq	fprintf
	movl	24(%rbx), %eax
	addq	$6, %rax
	movl	%eax, 24(%rbx)
	movq	(%rbx), %rdi
	movabsq	$.L.str106, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	callq	fprintf
	movl	24(%rbx), %eax
	addq	$8, %rax
	movl	%eax, 24(%rbx)
.LBB0_15:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	queue_and_dump_index, .Ltmp17-queue_and_dump_index
	.cfi_endproc

	.globl	queue_and_dump_type
	.align	16, 0x90
	.type	queue_and_dump_type,@function
queue_and_dump_type:                    # @queue_and_dump_type
	.cfi_startproc
# BB#0:                                 # %entry
	movq	8(%rsi), %rdx
	movabsq	$.L.str1, %rsi
	xorq	%rcx, %rcx
	jmp	queue_and_dump_index  # TAILCALL
.Ltmp18:
	.size	queue_and_dump_type, .Ltmp18-queue_and_dump_type
	.cfi_endproc

	.globl	dump_pointer
	.align	16, 0x90
	.type	dump_pointer,@function
dump_pointer:                           # @dump_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp28:
	.cfi_def_cfa_offset 48
.Ltmp29:
	.cfi_offset %rbx, -40
.Ltmp30:
	.cfi_offset %r12, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	24(%r12), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$56, %rax
	jb	.LBB2_2
# BB#1:                                 # %if.then.i
	movq	(%r12), %rdi
	movabsq	$.L.str105, %rsi
	movabsq	$.L.str104, %rcx
	movq	$25, %rdx
	xorq	%rax, %rax
	movq	$25, %rbx
	callq	fprintf
	movl	%ebx, 24(%r12)
	jmp	.LBB2_4
.LBB2_2:                                # %if.else.i
	addq	$-25, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	movq	%rbx, %rax
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB2_4
# BB#3:                                 # %if.then3.i
	movq	(%r12), %rdi
	xorq	$15, %rbx
	movabsq	$.L.str103, %rsi
	movabsq	$.L.str104, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	24(%r12), %eax
	addq	%rbx, %rax
	movl	%eax, 24(%r12)
.LBB2_4:                                # %dump_maybe_newline.exit
	movq	(%r12), %rdi
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	24(%r12), %eax
	addq	$15, %rax
	movl	%eax, 24(%r12)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp33:
	.size	dump_pointer, .Ltmp33-dump_pointer
	.cfi_endproc

	.globl	dump_int
	.align	16, 0x90
	.type	dump_int,@function
dump_int:                               # @dump_int
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp39:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp41:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp42:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp43:
	.cfi_def_cfa_offset 48
.Ltmp44:
	.cfi_offset %rbx, -40
.Ltmp45:
	.cfi_offset %r12, -32
.Ltmp46:
	.cfi_offset %r14, -24
.Ltmp47:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	24(%r12), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	cmpq	$56, %rax
	jb	.LBB3_2
# BB#1:                                 # %if.then.i
	movq	(%r12), %rdi
	movabsq	$.L.str105, %rsi
	movabsq	$.L.str104, %rcx
	movq	$25, %rdx
	xorq	%rax, %rax
	movq	$25, %rbx
	callq	fprintf
	movl	%ebx, 24(%r12)
	jmp	.LBB3_4
.LBB3_2:                                # %if.else.i
	addq	$-25, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	movq	%rbx, %rax
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB3_4
# BB#3:                                 # %if.then3.i
	movq	(%r12), %rdi
	xorq	$15, %rbx
	movabsq	$.L.str103, %rsi
	movabsq	$.L.str104, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	24(%r12), %eax
	addq	%rbx, %rax
	movl	%eax, 24(%r12)
.LBB3_4:                                # %dump_maybe_newline.exit
	movq	(%r12), %rdi
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	24(%r12), %eax
	addq	$14, %rax
	movl	%eax, 24(%r12)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp48:
	.size	dump_int, .Ltmp48-dump_int
	.cfi_endproc

	.globl	dump_string
	.align	16, 0x90
	.type	dump_string,@function
dump_string:                            # @dump_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp53:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp54:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp55:
	.cfi_def_cfa_offset 32
.Ltmp56:
	.cfi_offset %rbx, -32
.Ltmp57:
	.cfi_offset %r14, -24
.Ltmp58:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	24(%r15), %eax
	movq	%rsi, %r14
	cmpq	$56, %rax
	jb	.LBB4_2
# BB#1:                                 # %if.then.i
	movq	(%r15), %rdi
	movabsq	$.L.str105, %rsi
	movabsq	$.L.str104, %rcx
	movq	$25, %rdx
	xorq	%rax, %rax
	movq	$25, %rbx
	callq	fprintf
	movl	%ebx, 24(%r15)
	jmp	.LBB4_4
.LBB4_2:                                # %if.else.i
	addq	$-25, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	movq	%rbx, %rax
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB4_4
# BB#3:                                 # %if.then3.i
	movq	(%r15), %rdi
	xorq	$15, %rbx
	movabsq	$.L.str103, %rsi
	movabsq	$.L.str104, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	24(%r15), %eax
	addq	%rbx, %rax
	movl	%eax, 24(%r15)
.LBB4_4:                                # %dump_maybe_newline.exit
	movq	(%r15), %rdi
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	fprintf
	movq	%r14, %rdi
	callq	strlen
	cmpq	$14, %rax
	jb	.LBB4_6
# BB#5:                                 # %if.then
	movl	24(%r15), %ecx
	leaq	1(%rax,%rcx), %rax
	jmp	.LBB4_7
.LBB4_6:                                # %if.else
	movl	24(%r15), %eax
	addq	$14, %rax
.LBB4_7:                                # %if.end
	movl	%eax, 24(%r15)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp59:
	.size	dump_string, .Ltmp59-dump_string
	.cfi_endproc

	.globl	dump_stmt
	.align	16, 0x90
	.type	dump_stmt,@function
dump_stmt:                              # @dump_stmt
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
	movl	24(%r15), %eax
	movl	24(%rsi), %r14d
	cmpq	$56, %rax
	jb	.LBB5_2
# BB#1:                                 # %if.then.i.i
	movq	(%r15), %rdi
	movabsq	$.L.str105, %rsi
	movabsq	$.L.str104, %rcx
	movq	$25, %rdx
	xorq	%rax, %rax
	movq	$25, %rbx
	callq	fprintf
	movl	%ebx, 24(%r15)
	jmp	.LBB5_4
.LBB5_2:                                # %if.else.i.i
	addq	$-25, %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	movq	%rbx, %rax
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB5_4
# BB#3:                                 # %if.then3.i.i
	movq	(%r15), %rdi
	xorq	$15, %rbx
	movabsq	$.L.str103, %rsi
	movabsq	$.L.str104, %rcx
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	movl	24(%r15), %eax
	addq	%rbx, %rax
	movl	%eax, 24(%r15)
.LBB5_4:                                # %dump_int.exit
	movq	(%r15), %rdi
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	xorq	%rax, %rax
	movq	%r14, %rcx
	callq	fprintf
	movl	24(%r15), %eax
	addq	$14, %rax
	movl	%eax, 24(%r15)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp70:
	.size	dump_stmt, .Ltmp70-dump_stmt
	.cfi_endproc

	.globl	dump_next_stmt
	.align	16, 0x90
	.type	dump_next_stmt,@function
dump_next_stmt:                         # @dump_next_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rsi), %rdx
	movabsq	$.L.str6, %rsi
	xorq	%rcx, %rcx
	jmp	queue_and_dump_index  # TAILCALL
.Ltmp71:
	.size	dump_next_stmt, .Ltmp71-dump_next_stmt
	.cfi_endproc

	.globl	dump_flag
	.align	16, 0x90
	.type	dump_flag,@function
dump_flag:                              # @dump_flag
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %ecx
	andq	%rsi, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	testq	%rsi, %rcx
	je	.LBB7_4
# BB#1:                                 # %land.rhs
	movq	8(%rdi), %rax
	cmpq	%rdx, %rax
	je	.LBB7_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB7_4:                                # %land.end
	retq
.LBB7_2:
	movabsq	$0, %rax
	retq
.Ltmp72:
	.size	dump_flag, .Ltmp72-dump_flag
	.cfi_endproc

	.globl	dump_node
	.align	16, 0x90
	.type	dump_node,@function
dump_node:                              # @dump_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp84:
	.cfi_def_cfa_offset 192
.Ltmp85:
	.cfi_offset %rbx, -48
.Ltmp86:
	.cfi_offset %r12, -40
.Ltmp87:
	.cfi_offset %r13, -32
.Ltmp88:
	.cfi_offset %r14, -24
.Ltmp89:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%rdx, 48(%rsp)
	xorq	%rax, %rax
	movl	%eax, 68(%rsp)
	movl	%eax, 72(%rsp)
	movq	$0, 96(%rsp)
	movq	$0, 88(%rsp)
	movq	$0, 80(%rsp)
	movl	%esi, 64(%rsp)
	movq	%r14, 56(%rsp)
	movabsq	$splay_tree_compare_pointers, %rdi
	movabsq	$free, %rdx
	xorq	%rsi, %rsi
	callq	splay_tree_new
	movq	%rax, 104(%rsp)
	movl	68(%rsp), %r15d
	incq	%r15
	movl	%r15d, 68(%rsp)
	movq	96(%rsp), %rbx
	testq	%rbx, %rbx
	je	.LBB8_2
# BB#1:                                 # %if.then.i
	movq	8(%rbx), %rax
	movq	%rax, 96(%rsp)
	jmp	.LBB8_3
.LBB8_2:                                # %if.else.i
	movq	$16, %rdi
	callq	xmalloc
	movq	%rax, %rbx
.LBB8_3:                                # %if.end.i
	movq	$8, %rdi
	callq	xmalloc
	movl	%r15d, (%rax)
	movzbl	4(%rax), %ecx
	movq	104(%rsp), %rdi
	andq	$254, %rcx
	movb	%cl, 4(%rax)
	movq	%r14, %rsi
	movq	%rax, %rdx
	callq	splay_tree_insert
	movq	%rax, (%rbx)
	movq	$0, 8(%rbx)
	movq	88(%rsp), %rax
	testq	%rax, %rax
	je	.LBB8_4
# BB#5:                                 # %if.else10.i
	movq	%rbx, 8(%rax)
	movq	80(%rsp), %rax
	jmp	.LBB8_6
.LBB8_4:                                # %if.then9.i
	movq	%rbx, 80(%rsp)
	movq	%rbx, %rax
.LBB8_6:                                # %queue.exit
	movq	%rbx, 88(%rsp)
	testq	%rax, %rax
	je	.LBB8_369
# BB#7:                                 # %while.body.lr.ph
	leaq	48(%rsp), %r13
	movabsq	$.L.str104, %r14
	jmp	.LBB8_8
.LBB8_206:                              # %if.else.i.i358
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %r13
	andq	%rcx, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_209
# BB#207:                               # %if.then3.i.i361
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_208:                              # %dump_maybe_newline.exit.i366
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_209:                              # %dump_maybe_newline.exit.i366
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str76, %rdx
	jmp	.LBB8_210
.LBB8_200:                              # %if.else.i.i330
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %r13
	andq	%rcx, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_203
# BB#201:                               # %if.then3.i.i333
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_202:                              # %dump_maybe_newline.exit.i338
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_203:                              # %dump_maybe_newline.exit.i338
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str75, %rdx
.LBB8_210:                              # %if.end372.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	cmpq	$0, 200(%rbx)
	je	.LBB8_361
# BB#211:                               # %land.lhs.true375.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	64(%rsp), %eax
	testq	$2, %rax
	je	.LBB8_213
# BB#212:                               # %land.lhs.true375.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	56(%rsp), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB8_361
.LBB8_213:                              # %if.then378.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	168(%rax), %rdx
.LBB8_214:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str77, %rsi
	jmp	.LBB8_360
.LBB8_172:                              # %if.else.i.i246
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %r13
	andq	%rcx, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_175
# BB#173:                               # %if.then3.i.i249
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_174:                              # %dump_maybe_newline.exit.i254
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_175:                              # %dump_maybe_newline.exit.i254
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str69, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_176:                              # %if.end318.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$0, 88(%rbx)
	je	.LBB8_361
# BB#177:                               # %if.then321.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	bit_position
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str70, %rsi
	movq	%rax, %rdx
	jmp	.LBB8_360
.LBB8_224:                              # %if.else.i.i386
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_227
# BB#225:                               # %if.then3.i.i389
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_226:                              # %dump_maybe_newline.exit.i394
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_227:                              # %dump_maybe_newline.exit.i394
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str78, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_228:                              # %if.end388.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str44, %rsi
	callq	queue_and_dump_index
	movq	48(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str79, %rsi
	callq	queue_and_dump_index
	movq	56(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str80, %rsi
	callq	queue_and_dump_index
	movq	64(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str81, %rsi
.LBB8_267:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	queue_and_dump_index
	movq	(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str6, %rsi
	jmp	.LBB8_360
.LBB8_314:                              # %if.else.i.i442
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_317
# BB#315:                               # %if.then3.i.i445
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_316:                              # %dump_maybe_newline.exit.i450
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_317:                              # %dump_maybe_newline.exit.i450
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str92, %rdx
	jmp	.LBB8_318
.LBB8_308:                              # %if.else.i.i414
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_311
# BB#309:                               # %if.then3.i.i417
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_310:                              # %dump_maybe_newline.exit.i422
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_311:                              # %dump_maybe_newline.exit.i422
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str91, %rdx
.LBB8_318:                              # %if.end492.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	fprintf
	movl	72(%rsp), %r13d
	leaq	14(%r13), %rbx
	movl	%ebx, 72(%rsp)
	cmpq	$0, 32(%r15)
	jne	.LBB8_325
# BB#319:                               # %if.then498.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	cmpq	$56, %rbx
	jb	.LBB8_321
# BB#320:                               # %if.then.i.i465
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_323
.LBB8_321:                              # %if.else.i.i470
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %r13
	andq	%rax, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_324
# BB#322:                               # %if.then3.i.i473
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_323:                              # %dump_maybe_newline.exit.i478
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_324:                              # %dump_maybe_newline.exit.i478
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str93, %rdx
	callq	fprintf
	movl	72(%rsp), %ebx
	addq	$14, %rbx
	movl	%ebx, 72(%rsp)
.LBB8_325:                              # %if.end499.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	19(%r15), %eax
	testq	$8, %rax
	leaq	48(%rsp), %r12
	jne	.LBB8_332
# BB#326:                               # %if.then505.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_328
# BB#327:                               # %if.then.i.i493
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_330
.LBB8_328:                              # %if.else.i.i498
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_331
# BB#329:                               # %if.then3.i.i501
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_330:                              # %dump_maybe_newline.exit.i506
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_331:                              # %dump_maybe_newline.exit.i506
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str94, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_332:                              # %if.end506.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movabsq	$.L.str6, %rsi
	jmp	.LBB8_360
	.align	16, 0x90
.LBB8_8:                                # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_120 Depth 2
	movq	(%rax), %rcx
	movq	8(%rcx), %rbx
	movq	8(%rax), %rsi
	movq	(%rcx), %r15
	movl	(%rbx), %edx
	testq	%rsi, %rsi
	movq	%rsi, 80(%rsp)
	jne	.LBB8_10
# BB#9:                                 # %if.then.i17
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$0, 88(%rsp)
.LBB8_10:                               # %if.end.i21
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	96(%rsp), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, 96(%rsp)
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str106, %rsi
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$8, %rax
	movl	%eax, 72(%rsp)
	movzbl	4(%rbx), %eax
	testq	$1, %rax
	movabsq	$.L.str24, %rdx
	jne	.LBB8_12
# BB#11:                                # %if.else.i22
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	16(%r15), %eax
	movq	tree_code_name(,%rax,8), %rdx
.LBB8_12:                               # %if.end11.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str25, %rsi
	callq	fprintf
	movq	$25, %rax
	movl	%eax, 72(%rsp)
	movzbl	4(%rbx), %eax
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movl	16(%r15), %r12d
	testq	$1, %rax
	jne	.LBB8_13
# BB#28:                                # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	andq	$255, %r12
	movsbq	tree_code_type(%r12), %rbx
	cmpq	$115, %rbx
	jg	.LBB8_65
# BB#29:                                # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	leaq	-49(%rbx), %rcx
	andq	$255, %rcx
	cmpq	$52, %rcx
	ja	.LBB8_84
# BB#30:                                # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$4503599627372547, %rdx # imm = 0x10000000000803
	testq	%rdx, %rax
	je	.LBB8_31
# BB#43:                                # %if.then68.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str1, %rsi
	callq	queue_and_dump_index
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	cmpq	$100, %rbx
	jg	.LBB8_48
# BB#44:                                # %if.then68.i
                                        #   in Loop: Header=BB8_8 Depth=1
	andq	%rax, %rbx
	cmpq	$49, %rbx
	je	.LBB8_372
# BB#45:                                # %if.then68.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$50, %rbx
	je	.LBB8_47
# BB#46:                                # %if.then68.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$60, %rbx
	jne	.LBB8_49
.LBB8_47:                               # %sw.bb71.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	32(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str31, %rsi
	callq	queue_and_dump_index
	movq	40(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str32, %rsi
	jmp	.LBB8_83
	.align	16, 0x90
.LBB8_13:                               # %if.then23.i
                                        #   in Loop: Header=BB8_8 Depth=1
	testq	$524288, %r12           # imm = 0x80000
	jne	.LBB8_14
# BB#15:                                # %if.else29.i
                                        #   in Loop: Header=BB8_8 Depth=1
	testq	$2097152, %r12          # imm = 0x200000
	jne	.LBB8_16
# BB#17:                                # %if.else36.i
                                        #   in Loop: Header=BB8_8 Depth=1
	testq	$1048576, %r12          # imm = 0x100000
	movq	$25, %rbx
	je	.LBB8_20
# BB#18:                                # %dump_maybe_newline.exit.i86
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str28, %rdx
	jmp	.LBB8_19
	.align	16, 0x90
.LBB8_65:                               # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	andq	$255, %rbx
	cmpq	$116, %rbx
	jne	.LBB8_84
# BB#66:                                # %if.then163.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	lang_hooks+248(%rip), %rax
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_74
# BB#67:                                # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	testq	$1, %rax
	movq	$99, %rdx
	jne	.LBB8_69
# BB#68:                                # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$32, %rdx
.LBB8_69:                               # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rax, %rsi
	andq	$2, %rsi
	movq	$1, %rcx
	shrq	%cl, %rsi
	testq	%rsi, %rsi
	movq	$118, %rbx
	jne	.LBB8_71
# BB#70:                                # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$32, %rbx
.LBB8_71:                               # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	andq	$4, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	movq	$114, %r8
	jne	.LBB8_73
# BB#72:                                # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$32, %r8
.LBB8_73:                               # %if.then167.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str40, %rsi
	movq	%rbx, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_74:                               # %if.end179.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	96(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str34, %rsi
	callq	queue_and_dump_index
	movq	128(%r15), %rdx
	cmpq	%r15, %rdx
	movq	%r15, %rbx
	je	.LBB8_76
# BB#75:                                # %if.then185.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str41, %rsi
	callq	queue_and_dump_index
.LBB8_76:                               # %if.end188.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	32(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str42, %rsi
	callq	queue_and_dump_index
	movl	72(%rsp), %r15d
	movl	64(%rbx), %ebx
	cmpq	$56, %r15
	jb	.LBB8_78
# BB#77:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_80
	.align	16, 0x90
.LBB8_14:                               # %dump_maybe_newline.exit.i36
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str26, %rdx
	jmp	.LBB8_19
.LBB8_16:                               # %dump_maybe_newline.exit.i58
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str27, %rdx
.LBB8_19:                               # %if.end45.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	fprintf
	movl	72(%rsp), %ebx
	addq	$14, %rbx
	movl	%ebx, 72(%rsp)
.LBB8_20:                               # %if.end45.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movzbl	18(%r15), %eax
	testq	$4, %rax
	je	.LBB8_27
# BB#21:                                # %if.then51.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_23
# BB#22:                                # %if.then.i.i101
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_25
.LBB8_23:                               # %if.else.i.i106
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_26
# BB#24:                                # %if.then3.i.i109
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_25:                               # %dump_maybe_newline.exit.i114
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_26:                               # %dump_maybe_newline.exit.i114
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str29, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_27:                               # %if.end52.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	8(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str1, %rsi
	callq	queue_and_dump_index
	movq	64(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str30, %rsi
.LBB8_360:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	queue_and_dump_index
	jmp	.LBB8_361
.LBB8_78:                               # %if.else.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r15
	movq	%r15, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r15
	je	.LBB8_81
# BB#79:                                # %if.then3.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r15
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r15, %rax
.LBB8_80:                               # %dump_int.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_81:                               # %dump_int.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str43, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	jmp	.LBB8_84
.LBB8_31:                               # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$50, %rcx
	jne	.LBB8_32
# BB#82:                                # %if.then195.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str1, %rsi
	jmp	.LBB8_83
.LBB8_48:                               # %if.then68.i
                                        #   in Loop: Header=BB8_8 Depth=1
	andq	%rax, %rbx
	cmpq	$101, %rbx
	je	.LBB8_84
	jmp	.LBB8_49
.LBB8_32:                               # %if.end55.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$51, %rcx
	jne	.LBB8_84
# BB#33:                                # %if.then88.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	72(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB8_35
# BB#34:                                # %if.then90.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str34, %rsi
	callq	queue_and_dump_index
.LBB8_35:                               # %if.end93.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	120(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB8_38
# BB#36:                                # %cond.end.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	72(%rbx), %rax
	cmpq	%rax, %rdx
	je	.LBB8_38
# BB#37:                                # %cond.end114.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str35, %rsi
	callq	queue_and_dump_index
.LBB8_38:                               # %if.end117.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	8(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str1, %rsi
	callq	queue_and_dump_index
	movq	80(%rbx), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str36, %rsi
	callq	queue_and_dump_index
	movq	24(%rbx), %r13
	testq	%r13, %r13
	je	.LBB8_54
# BB#39:                                # %if.then121.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %r15
	movq	$47, %rsi
	movq	%r13, %rdi
	callq	strrchr
	testq	%rax, %rax
	je	.LBB8_41
# BB#40:                                # %if.then121.i
                                        #   in Loop: Header=BB8_8 Depth=1
	incq	%rax
	movq	%rax, %r13
.LBB8_41:                               # %if.then121.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	cmpq	$56, %rbx
	jb	.LBB8_50
# BB#42:                                # %if.then.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_52
.LBB8_372:                              # %sw.bb.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str31, %rsi
.LBB8_83:                               # %if.end199.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	queue_and_dump_index
	.align	16, 0x90
.LBB8_84:                               # %if.end199.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r13, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	lang_hooks+240(%rip), %rax
	callq	*%rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_361
# BB#85:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$90, %r12
	jg	.LBB8_88
# BB#86:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	leaq	-1(%r12), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$55, %rax
	ja	.LBB8_361
# BB#87:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	jmpq	*.LJTI8_1(,%rax,8)
.LBB8_159:                              # %sw.bb283.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movzbl	16(%r15), %eax
	movq	104(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	cmpq	$35, %rax
	jne	.LBB8_161
# BB#160:                               # %if.then290.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$.L.str67, %rsi
	jmp	.LBB8_162
.LBB8_88:                               # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$117, %r12
	jg	.LBB8_92
# BB#89:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	leaq	-91(%r12), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$2, %rax
	jb	.LBB8_350
# BB#90:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$96, %r12
	jne	.LBB8_361
	jmp	.LBB8_91
.LBB8_92:                               # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-118, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$53, %r12
	ja	.LBB8_361
# BB#93:                                # %if.end203.i
                                        #   in Loop: Header=BB8_8 Depth=1
	jmpq	*.LJTI8_0(,%r12,8)
.LBB8_350:                              # %sw.bb528.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str31, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str32, %rsi
	jmp	.LBB8_360
.LBB8_91:                               # %sw.bb524.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str31, %rsi
	jmp	.LBB8_360
.LBB8_121:                              # %sw.bb224.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %r13d
	movl	72(%rsp), %ebx
	andq	$511, %r13              # imm = 0x1FF
	cmpq	$56, %rbx
	jb	.LBB8_123
# BB#122:                               # %if.then.i.i1024.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_125
.LBB8_146:                              # %sw.bb266.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movzbl	16(%r15), %eax
	movl	72(%rsp), %ebx
	cmpq	$20, %rax
	jne	.LBB8_152
# BB#147:                               # %if.then273.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_149
# BB#148:                               # %if.then.i.i185
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	movl	%eax, 72(%rsp)
	leaq	48(%rsp), %r12
	jmp	.LBB8_151
.LBB8_161:                              # %if.else293.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$.L.str68, %rsi
.LBB8_162:                              # %if.end296.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str42, %rsi
	callq	queue_and_dump_index
	movl	56(%r15), %r13d
	movl	72(%rsp), %ebx
	andq	$16777215, %r13         # imm = 0xFFFFFF
	cmpq	$56, %rbx
	jb	.LBB8_164
# BB#163:                               # %if.then.i.i1060.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_166
.LBB8_164:                              # %if.else.i.i1065.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_167
# BB#165:                               # %if.then3.i.i1068.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_166:                              # %dump_int.exit1073.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_167:                              # %dump_int.exit1073.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str43, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %r13d
	leaq	14(%r13), %rax
	movl	%eax, 72(%rsp)
	movl	16(%r15), %ebx
	movq	%rbx, %rcx
	andq	$255, %rcx
	leaq	-34(%rcx), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$2, %rdx
	jae	.LBB8_168
# BB#178:                               # %if.then338.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	andq	%rsi, %rax
	cmpq	$56, %rax
	jb	.LBB8_180
# BB#179:                               # %if.then.i.i1078.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_182
.LBB8_50:                               # %if.else.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_53
# BB#51:                                # %if.then3.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_52:                               # %dump_maybe_newline.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_53:                               # %dump_maybe_newline.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	%r15, %rbx
	movl	32(%rbx), %ecx
	xorq	%rax, %rax
	movabsq	$.L.str37, %rsi
	movq	%r13, %rdx
	callq	fprintf
	movq	%r13, %rdi
	callq	strlen
	movl	72(%rsp), %ecx
	leaq	14(%rax,%rcx), %rax
	movl	%eax, 72(%rsp)
.LBB8_54:                               # %if.end141.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movzbl	50(%rbx), %eax
	testq	$64, %rax
	je	.LBB8_61
# BB#55:                                # %if.then148.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	cmpq	$56, %rbx
	jb	.LBB8_57
# BB#56:                                # %if.then.i.i129
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_59
.LBB8_57:                               # %if.else.i.i134
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_60
# BB#58:                                # %if.then3.i.i137
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_59:                               # %dump_maybe_newline.exit.i142
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_60:                               # %dump_maybe_newline.exit.i142
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str38, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_61:                               # %if.end149.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	leaq	48(%rsp), %r13
	je	.LBB8_84
# BB#62:                                # %land.lhs.true152.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	64(%rsp), %eax
	testq	$2, %rax
	je	.LBB8_64
# BB#63:                                # %land.lhs.true152.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	56(%rsp), %rax
	testq	%rax, %rax
	jne	.LBB8_84
.LBB8_64:                               # %if.then155.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	movabsq	$.L.str39, %rsi
	jmp	.LBB8_83
.LBB8_168:                              # %dump_int.exit1073.i
                                        #   in Loop: Header=BB8_8 Depth=1
	cmpq	$37, %rcx
	jne	.LBB8_361
# BB#169:                               # %if.then309.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movzbl	53(%rbx), %ecx
	testq	$2, %rcx
	je	.LBB8_176
# BB#170:                               # %if.then317.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_172
# BB#171:                               # %if.then.i.i241
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_174
.LBB8_180:                              # %if.else.i.i1083.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %r13
	andq	%rsi, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_183
# BB#181:                               # %if.then3.i.i1086.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_182:                              # %dump_int.exit1091.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_183:                              # %dump_int.exit1091.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str71, %rdx
	movq	%rbx, %rcx
	callq	fprintf
	movl	72(%rsp), %ebx
	leaq	14(%rbx), %rax
	movl	%eax, 72(%rsp)
	movzbl	49(%r15), %ecx
	testq	$4, %rcx
	je	.LBB8_361
# BB#184:                               # %if.then349.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_186
# BB#185:                               # %if.then.i.i269
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_188
.LBB8_260:                              # %sw.bb431.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_262
# BB#261:                               # %if.then.i.i.i1204.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_264
.LBB8_229:                              # %sw.bb401.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_231
# BB#230:                               # %if.then.i.i.i1108.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_233
.LBB8_292:                              # %sw.bb470.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_294
# BB#293:                               # %if.then.i.i.i1318.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_296
.LBB8_95:                               # %sw.bb204.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %r13
	cmpq	$56, %rbx
	jb	.LBB8_97
# BB#96:                                # %if.then.i.i970.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_99
.LBB8_112:                              # %sw.bb207.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	24(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str46, %rsi
	callq	queue_and_dump_index
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str47, %rsi
	callq	queue_and_dump_index
	movq	(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str39, %rsi
	jmp	.LBB8_360
.LBB8_113:                              # %sw.bb212.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_115
# BB#114:                               # %if.then.i.i1006.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_117
.LBB8_135:                              # %sw.bb242.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	60(%rax), %r13d
	movl	72(%rsp), %ebx
	andq	$511, %r13              # imm = 0x1FF
	cmpq	$56, %rbx
	jb	.LBB8_137
# BB#136:                               # %if.then.i.i1042.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_139
.LBB8_141:                              # %sw.bb247.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str54, %rsi
	jmp	.LBB8_360
.LBB8_142:                              # %sw.bb250.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str55, %rsi
	jmp	.LBB8_360
.LBB8_143:                              # %sw.bb253.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	112(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str56, %rsi
	callq	queue_and_dump_index
	jmp	.LBB8_144
.LBB8_145:                              # %sw.bb261.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	8(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str59, %rsi
	callq	queue_and_dump_index
	movq	24(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str60, %rsi
	jmp	.LBB8_360
.LBB8_94:                               # %if.end203.sw.bb256_crit_edge.i
                                        #   in Loop: Header=BB8_8 Depth=1
	leaq	48(%rsp), %rbx
	movq	40(%rsp), %r15          # 8-byte Reload
.LBB8_144:                              # %sw.bb256.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	8(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str57, %rsi
	callq	queue_and_dump_index
	movq	24(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str58, %rsi
	jmp	.LBB8_360
.LBB8_334:                              # %sw.bb507.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	40(%r15), %r13
	testq	%r13, %r13
	jne	.LBB8_335
# BB#341:                               # %if.end515thread-pre-split.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	jmp	.LBB8_342
.LBB8_348:                              # %sw.bb519.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	40(%r15), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str95, %rsi
	callq	fprintf
	movl	72(%rsp), %ebx
	movl	32(%r15), %r13d
	cmpq	$56, %rbx
	jae	.LBB8_104
# BB#349:                               # %if.else.i.i1415.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	jmp	.LBB8_106
.LBB8_190:                              # %sw.bb353.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	88(%rbx), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str73, %rsi
	callq	queue_and_dump_index
	movzbl	49(%rbx), %eax
	movl	72(%rsp), %r13d
	testq	$1, %rax
	je	.LBB8_197
# BB#191:                               # %if.then362.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_193
# BB#192:                               # %if.then.i.i297
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_195
.LBB8_158:                              # %sw.bb281.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str66, %rsi
	jmp	.LBB8_360
.LBB8_353:                              # %sw.bb552.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	40(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str59, %rsi
	jmp	.LBB8_360
.LBB8_358:                              # %sw.bb575.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str84, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str68, %r12
	movq	%r12, %rsi
	callq	queue_and_dump_index
	movq	48(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str94, %rsi
	callq	queue_and_dump_index
	movq	56(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	jmp	.LBB8_360
.LBB8_351:                              # %sw.bb535.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str31, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str32, %rsi
	callq	queue_and_dump_index
	movq	48(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str96, %rsi
	jmp	.LBB8_360
.LBB8_355:                              # %sw.bb560.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str99, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str77, %rsi
	jmp	.LBB8_360
.LBB8_352:                              # %sw.bb545.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str97, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str73, %rsi
	jmp	.LBB8_360
.LBB8_123:                              # %if.else.i.i1029.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_126
# BB#124:                               # %if.then3.i.i1032.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_125:                              # %dump_int.exit1037.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_126:                              # %dump_int.exit1037.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str49, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %ebx
	leaq	14(%rbx), %rax
	movl	%eax, 72(%rsp)
	movq	40(%rsp), %r15          # 8-byte Reload
	movzbl	17(%r15), %ecx
	testq	$32, %rcx
	je	.LBB8_133
# BB#127:                               # %if.then233.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_129
# BB#128:                               # %if.then.i.i157
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_131
.LBB8_152:                              # %if.else274.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_154
# BB#153:                               # %if.then.i.i213
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	movl	%eax, 72(%rsp)
	leaq	48(%rsp), %r12
	jmp	.LBB8_156
.LBB8_186:                              # %if.else.i.i274
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_189
# BB#187:                               # %if.then3.i.i277
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_188:                              # %dump_maybe_newline.exit.i282
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_189:                              # %dump_maybe_newline.exit.i282
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str72, %rdx
	callq	fprintf
	jmp	.LBB8_111
.LBB8_357:                              # %sw.bb571.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str86, %rsi
	jmp	.LBB8_360
.LBB8_356:                              # %sw.bb567.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	jmp	.LBB8_214
.LBB8_359:                              # %sw.bb588.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str85, %rsi
	jmp	.LBB8_360
.LBB8_242:                              # %sw.bb416.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_244
# BB#243:                               # %if.then.i.i.i1147.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_246
.LBB8_248:                              # %sw.bb420.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_250
# BB#249:                               # %if.then.i.i.i1166.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_252
.LBB8_280:                              # %sw.bb452.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_282
# BB#281:                               # %if.then.i.i.i1261.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_284
.LBB8_268:                              # %sw.bb435.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_270
# BB#269:                               # %if.then.i.i.i1223.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_272
.LBB8_254:                              # %sw.bb424.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_256
# BB#255:                               # %if.then.i.i.i1185.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_258
.LBB8_274:                              # %sw.bb448.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_276
# BB#275:                               # %if.then.i.i.i1242.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_278
.LBB8_286:                              # %sw.bb462.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_288
# BB#287:                               # %if.then.i.i.i1280.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_290
.LBB8_216:                              # %sw.bb381.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_218
# BB#217:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_220
.LBB8_300:                              # %sw.bb484.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_302
# BB#301:                               # %if.then.i.i.i1356.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_304
.LBB8_235:                              # %sw.bb402.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str82, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str83, %rsi
	jmp	.LBB8_267
.LBB8_354:                              # %sw.bb556.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str98, %rsi
	jmp	.LBB8_360
.LBB8_236:                              # %sw.bb409.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	movq	40(%rsp), %r15          # 8-byte Reload
	movl	24(%r15), %r13d
	cmpq	$56, %rbx
	jb	.LBB8_238
# BB#237:                               # %if.then.i.i.i1128.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_240
.LBB8_262:                              # %if.else.i.i.i1209.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_265
# BB#263:                               # %if.then3.i.i.i1212.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_264:                              # %dump_stmt.exit1216.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_265:                              # %dump_stmt.exit1216.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	jmp	.LBB8_266
.LBB8_231:                              # %if.else.i.i.i1113.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_234
# BB#232:                               # %if.then3.i.i.i1116.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_233:                              # %dump_stmt.exit1120.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_234:                              # %dump_stmt.exit1120.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str6, %rsi
	jmp	.LBB8_360
.LBB8_294:                              # %if.else.i.i.i1323.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_297
# BB#295:                               # %if.then3.i.i.i1326.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_296:                              # %dump_stmt.exit1330.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_297:                              # %dump_stmt.exit1330.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str86, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	jmp	.LBB8_298
.LBB8_149:                              # %if.else.i.i190
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	leaq	48(%rsp), %r12
	je	.LBB8_151
# BB#150:                               # %if.then3.i.i193
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
	movl	%eax, 72(%rsp)
.LBB8_151:                              # %dump_maybe_newline.exit.i198
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str61, %rdx
	jmp	.LBB8_157
.LBB8_97:                               # %if.else.i.i975.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_100
# BB#98:                                # %if.then3.i.i978.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_99:                               # %dump_maybe_newline.exit.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_100:                              # %dump_maybe_newline.exit.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str102, %rsi
	movabsq	$.L.str44, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movq	%r13, %rdi
	callq	strlen
	cmpq	$8, %rax
	jb	.LBB8_102
# BB#101:                               # %if.then.i982.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ecx
	leaq	7(%rax,%rcx), %rbx
	jmp	.LBB8_103
.LBB8_115:                              # %if.else.i.i1011.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_118
# BB#116:                               # %if.then3.i.i1014.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_117:                              # %dump_int.exit1019.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_118:                              # %dump_int.exit1019.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str45, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movslq	24(%r15), %rax
	testq	%rax, %rax
	jle	.LBB8_361
# BB#119:                               # %for.body.i.preheader
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB8_120:                              # %for.body.i
                                        #   Parent Loop BB8_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rax, %rax
	leaq	112(%rsp), %r12
	movq	%r12, %rdi
	movabsq	$.L.str48, %rsi
	movq	%rbx, %rdx
	callq	sprintf
	movq	40(%rsp), %r15          # 8-byte Reload
	movq	32(%r15,%rbx,8), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movq	%r12, %rsi
	callq	queue_and_dump_index
	incq	%rbx
	movslq	24(%r15), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB8_120
	jmp	.LBB8_361
.LBB8_137:                              # %if.else.i.i1047.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_140
# BB#138:                               # %if.then3.i.i1050.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_139:                              # %dump_int.exit1055.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_140:                              # %dump_int.exit1055.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str49, %rdx
	jmp	.LBB8_110
.LBB8_335:                              # %if.then510.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	cmpq	$56, %rbx
	jb	.LBB8_337
# BB#336:                               # %if.then.i.i1374.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_339
.LBB8_129:                              # %if.else.i.i162
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-11, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_132
# BB#130:                               # %if.then3.i.i165
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_131:                              # %dump_maybe_newline.exit.i170
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_132:                              # %dump_maybe_newline.exit.i170
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str50, %rdx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
.LBB8_133:                              # %if.end234.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	104(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str51, %rsi
	callq	queue_and_dump_index
	movq	112(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str52, %rsi
	callq	queue_and_dump_index
	cmpq	$10, %r12
	jne	.LBB8_361
# BB#134:                               # %if.then239.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rdi
	movabsq	$.L.str53, %rsi
	jmp	.LBB8_360
.LBB8_154:                              # %if.else.i.i218
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	leaq	48(%rsp), %r12
	je	.LBB8_156
# BB#155:                               # %if.then3.i.i221
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
	movl	%eax, 72(%rsp)
.LBB8_156:                              # %dump_maybe_newline.exit.i226
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str62, %rdx
.LBB8_157:                              # %if.end275.i
                                        #   in Loop: Header=BB8_8 Depth=1
	callq	fprintf
	movl	72(%rsp), %eax
	movq	%r15, 40(%rsp)          # 8-byte Spill
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	24(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movabsq	$.L.str63, %rsi
	callq	queue_and_dump_index
	movq	112(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movabsq	$.L.str64, %rsi
	callq	queue_and_dump_index
	movq	136(%r15), %rdx
	movq	$1, %rcx
	movq	%r12, %rdi
	movabsq	$.L.str65, %rsi
	jmp	.LBB8_360
.LBB8_102:                              # %if.else.i983.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	addq	$14, %rbx
.LBB8_103:                              # %dump_string_field.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%ebx, 72(%rsp)
	movl	24(%r15), %r13d
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_105
.LBB8_104:                              # %if.then.i.i988.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_108
.LBB8_105:                              # %if.else.i.i993.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
.LBB8_106:                              # %if.else.i.i993.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_109
# BB#107:                               # %if.then3.i.i996.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_108:                              # %dump_int.exit1001.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_109:                              # %dump_int.exit1001.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str45, %rdx
	jmp	.LBB8_110
.LBB8_244:                              # %if.else.i.i.i1152.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_247
# BB#245:                               # %if.then3.i.i.i1155.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_246:                              # %dump_stmt.exit1159.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_247:                              # %dump_stmt.exit1159.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	jmp	.LBB8_299
.LBB8_250:                              # %if.else.i.i.i1171.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_253
# BB#251:                               # %if.then3.i.i.i1174.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_252:                              # %dump_stmt.exit1178.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_253:                              # %dump_stmt.exit1178.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str84, %rsi
	jmp	.LBB8_267
.LBB8_282:                              # %if.else.i.i.i1266.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_285
# BB#283:                               # %if.then3.i.i.i1269.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_284:                              # %dump_stmt.exit1273.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_285:                              # %dump_stmt.exit1273.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str86, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str88, %rsi
	callq	queue_and_dump_index
	movq	48(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str89, %rsi
	jmp	.LBB8_267
.LBB8_270:                              # %if.else.i.i.i1228.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_273
# BB#271:                               # %if.then3.i.i.i1231.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_272:                              # %dump_stmt.exit1235.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_273:                              # %dump_stmt.exit1235.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str68, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str86, %rsi
	callq	queue_and_dump_index
	movq	48(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str85, %rsi
	callq	queue_and_dump_index
	movq	56(%r15), %rdx
.LBB8_298:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	xorq	%rcx, %rcx
.LBB8_299:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rdi
	movabsq	$.L.str77, %rsi
	jmp	.LBB8_267
.LBB8_256:                              # %if.else.i.i.i1190.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_259
# BB#257:                               # %if.then3.i.i.i1193.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_258:                              # %dump_stmt.exit1197.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_259:                              # %dump_stmt.exit1197.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str77, %rsi
	callq	queue_and_dump_index
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movabsq	$.L.str86, %rsi
	jmp	.LBB8_267
.LBB8_276:                              # %if.else.i.i.i1247.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_279
# BB#277:                               # %if.then3.i.i.i1250.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_278:                              # %dump_stmt.exit1254.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_279:                              # %dump_stmt.exit1254.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str87, %rsi
	jmp	.LBB8_267
.LBB8_288:                              # %if.else.i.i.i1285.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_291
# BB#289:                               # %if.then3.i.i.i1288.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_290:                              # %dump_stmt.exit1292.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_291:                              # %dump_stmt.exit1292.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str90, %rsi
	jmp	.LBB8_267
.LBB8_218:                              # %if.else.i.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_221
# BB#219:                               # %if.then3.i.i.i.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_220:                              # %dump_stmt.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_221:                              # %dump_stmt.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %ebx
	leaq	14(%rbx), %rax
	movl	%eax, 72(%rsp)
	cmpq	$0, 32(%r15)
	je	.LBB8_228
# BB#222:                               # %if.then387.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_224
# BB#223:                               # %if.then.i.i381
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_226
.LBB8_302:                              # %if.else.i.i.i1361.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_305
# BB#303:                               # %if.then3.i.i.i1364.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_304:                              # %dump_stmt.exit1368.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_305:                              # %dump_stmt.exit1368.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %ebx
	leaq	14(%rbx), %rax
	movl	%eax, 72(%rsp)
	movzbl	19(%r15), %ecx
	testq	$1, %rcx
	jne	.LBB8_306
# BB#312:                               # %if.else491.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_314
# BB#313:                               # %if.then.i.i437
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_316
.LBB8_238:                              # %if.else.i.i.i1133.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_241
# BB#239:                               # %if.then3.i.i.i1136.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_240:                              # %dump_stmt.exit1140.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_241:                              # %dump_stmt.exit1140.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str5, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	movq	32(%r15), %rdx
	xorq	%rcx, %rcx
	leaq	48(%rsp), %rbx
	movq	%rbx, %rdi
	movabsq	$.L.str84, %rsi
	callq	queue_and_dump_index
	movq	40(%r15), %rdx
	xorq	%rcx, %rcx
.LBB8_266:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%rbx, %rdi
	movabsq	$.L.str85, %rsi
	jmp	.LBB8_267
.LBB8_306:                              # %if.then490.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_308
# BB#307:                               # %if.then.i.i409
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_310
.LBB8_193:                              # %if.else.i.i302
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %r13
	andq	%rcx, %r13
	movq	%r13, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %r13
	je	.LBB8_196
# BB#194:                               # %if.then3.i.i305
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %r13
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%r13, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%r13, %rax
.LBB8_195:                              # %dump_maybe_newline.exit.i310
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_196:                              # %dump_maybe_newline.exit.i310
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str4, %rsi
	movabsq	$.L.str74, %rdx
	callq	fprintf
	movl	72(%rsp), %r13d
	addq	$14, %r13
	movl	%r13d, 72(%rsp)
.LBB8_197:                              # %if.end363.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	18(%rbx), %eax
	testq	$8, %rax
	jne	.LBB8_198
# BB#204:                               # %if.else371.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_206
# BB#205:                               # %if.then.i.i353
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_208
.LBB8_198:                              # %if.then370.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r13, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_200
# BB#199:                               # %if.then.i.i325
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_202
.LBB8_337:                              # %if.else.i.i1379.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_340
# BB#338:                               # %if.then3.i.i1382.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_339:                              # %dump_int.exit1387.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_340:                              # %dump_int.exit1387.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str83, %rdx
	movq	%r13, %rcx
	callq	fprintf
	movl	72(%rsp), %ebx
	addq	$14, %rbx
	movl	%ebx, 72(%rsp)
.LBB8_342:                              # %if.end515.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	32(%r15), %r13
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$56, %rax
	jb	.LBB8_344
# BB#343:                               # %if.then.i.i1392.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	jmp	.LBB8_346
.LBB8_344:                              # %if.else.i.i1397.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	andq	%rcx, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	je	.LBB8_347
# BB#345:                               # %if.then3.i.i1400.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
.LBB8_346:                              # %dump_int.exit1405.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	%eax, 72(%rsp)
.LBB8_347:                              # %dump_int.exit1405.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str3, %rsi
	movabsq	$.L.str82, %rdx
.LBB8_110:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	%r13, %rcx
	callq	fprintf
.LBB8_111:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %eax
	addq	$14, %rax
	movl	%eax, 72(%rsp)
	.align	16, 0x90
.LBB8_361:                              # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movzbl	64(%rsp), %eax
	testq	$1, %rax
	je	.LBB8_368
# BB#362:                               # %done.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	56(%rsp), %rax
	testq	%rax, %rax
	je	.LBB8_368
# BB#363:                               # %if.then596.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movl	72(%rsp), %ebx
	cmpq	$56, %rbx
	jb	.LBB8_365
# BB#364:                               # %if.then.i.i1436.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	movq	$25, %rdx
	xorq	%rax, %rax
	movabsq	$.L.str105, %rsi
	movq	%r14, %rcx
	callq	fprintf
	movq	$25, %rax
	movl	%eax, 72(%rsp)
	movq	40(%rsp), %r15          # 8-byte Reload
	jmp	.LBB8_367
	.align	16, 0x90
.LBB8_365:                              # %if.else.i.i1441.i
                                        #   in Loop: Header=BB8_8 Depth=1
	addq	$-25, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movq	%rbx, %rax
	movabsq	$-8608480567731124087, %rcx # imm = 0x8888888888888889
	mulq	%rcx
	movq	$3, %rcx
	shrq	%cl, %rdx
	leaq	(%rdx,%rdx,4), %rax
	leaq	(%rax,%rax,2), %rax
	subq	%rax, %rbx
	movq	40(%rsp), %r15          # 8-byte Reload
	je	.LBB8_367
# BB#366:                               # %if.then3.i.i1444.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	$15, %rbx
	xorq	%rax, %rax
	movabsq	$.L.str103, %rsi
	movq	%rbx, %rdx
	movq	%r14, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	%rbx, %rax
	movl	%eax, 72(%rsp)
.LBB8_367:                              # %dump_pointer.exit.i
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rdi
	xorq	%rax, %rax
	movabsq	$.L.str2, %rsi
	movabsq	$.L.str100, %rdx
	movq	%r15, %rcx
	callq	fprintf
	movl	72(%rsp), %eax
	addq	$15, %rax
	movl	%eax, 72(%rsp)
.LBB8_368:                              # %dequeue_and_dump.exit
                                        #   in Loop: Header=BB8_8 Depth=1
	movq	48(%rsp), %rsi
	movq	$10, %rdi
	callq	fputc
	movq	80(%rsp), %rax
	testq	%rax, %rax
	leaq	48(%rsp), %r13
	jne	.LBB8_8
.LBB8_369:                              # %while.end
	movq	96(%rsp), %rdi
	testq	%rdi, %rdi
	je	.LBB8_371
	.align	16, 0x90
.LBB8_370:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB8_370
.LBB8_371:                              # %for.end
	movq	104(%rsp), %rdi
	callq	splay_tree_delete
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_49:                               # %sw.default.i
	movabsq	$.L.str33, %rax
	movabsq	$.L__FUNCTION__.dequeue_and_dump, %rcx
	movq	$334, %rsi              # imm = 0x14E
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp90:
	.size	dump_node, .Ltmp90-dump_node
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_91
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_91
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_350
	.quad	.LBB8_350
	.quad	.LBB8_350
	.quad	.LBB8_350
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_357
	.quad	.LBB8_356
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_359
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_260
	.quad	.LBB8_242
	.quad	.LBB8_248
	.quad	.LBB8_280
	.quad	.LBB8_268
	.quad	.LBB8_292
	.quad	.LBB8_254
	.quad	.LBB8_260
	.quad	.LBB8_229
	.quad	.LBB8_229
	.quad	.LBB8_292
	.quad	.LBB8_274
	.quad	.LBB8_286
	.quad	.LBB8_216
	.quad	.LBB8_300
	.quad	.LBB8_361
	.quad	.LBB8_235
	.quad	.LBB8_354
	.quad	.LBB8_361
	.quad	.LBB8_236
.LJTI8_1:
	.quad	.LBB8_95
	.quad	.LBB8_112
	.quad	.LBB8_113
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_121
	.quad	.LBB8_135
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_121
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_141
	.quad	.LBB8_361
	.quad	.LBB8_142
	.quad	.LBB8_143
	.quad	.LBB8_361
	.quad	.LBB8_145
	.quad	.LBB8_361
	.quad	.LBB8_146
	.quad	.LBB8_146
	.quad	.LBB8_361
	.quad	.LBB8_94
	.quad	.LBB8_361
	.quad	.LBB8_334
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_348
	.quad	.LBB8_190
	.quad	.LBB8_361
	.quad	.LBB8_158
	.quad	.LBB8_361
	.quad	.LBB8_159
	.quad	.LBB8_159
	.quad	.LBB8_159
	.quad	.LBB8_159
	.quad	.LBB8_361
	.quad	.LBB8_350
	.quad	.LBB8_361
	.quad	.LBB8_91
	.quad	.LBB8_361
	.quad	.LBB8_350
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_353
	.quad	.LBB8_350
	.quad	.LBB8_350
	.quad	.LBB8_350
	.quad	.LBB8_358
	.quad	.LBB8_351
	.quad	.LBB8_355
	.quad	.LBB8_352
	.quad	.LBB8_361
	.quad	.LBB8_361
	.quad	.LBB8_91

	.text
	.globl	dump_begin
	.align	16, 0x90
	.type	dump_begin,@function
dump_begin:                             # @dump_begin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 48
.Ltmp102:
	.cfi_offset %rbx, -48
.Ltmp103:
	.cfi_offset %r12, -40
.Ltmp104:
	.cfi_offset %r13, -32
.Ltmp105:
	.cfi_offset %r14, -24
.Ltmp106:
	.cfi_offset %r15, -16
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	leaq	(%rax,%rax,2), %r12
	movl	dump_files+20(,%r12,8), %eax
	movq	%rsi, %r14
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB9_9
# BB#1:                                 # %if.end
	movq	dump_base_name(%rip), %rdi
	movq	dump_files(,%r12,8), %rsi
	leaq	dump_files+20(,%r12,8), %r13
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	callq	concat
	movslq	(%r13), %rcx
	movq	%rax, %r15
	testq	%rcx, %rcx
	js	.LBB9_2
# BB#3:                                 # %select.mid
	movabsq	$.L.str8, %rsi
	jmp	.LBB9_4
.LBB9_2:
	movabsq	$.L.str7, %rsi
.LBB9_4:                                # %select.end
	movq	%r15, %rdi
	callq	fopen
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB9_5
# BB#6:                                 # %if.else
	movq	$1, %rax
	movl	%eax, (%r13)
	jmp	.LBB9_7
.LBB9_5:                                # %if.then8
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	error
.LBB9_7:                                # %if.end12
	movq	%r15, %rdi
	callq	free
	testq	%r14, %r14
	je	.LBB9_9
# BB#8:                                 # %if.then14
	movl	dump_files+16(,%r12,8), %eax
	movl	%eax, (%r14)
.LBB9_9:                                # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	dump_begin, .Ltmp107-dump_begin
	.cfi_endproc

	.globl	dump_enabled_p
	.align	16, 0x90
	.type	dump_enabled_p,@function
dump_enabled_p:                         # @dump_enabled_p
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	leaq	(%rax,%rax,2), %rax
	movl	dump_files+20(,%rax,8), %eax
	retq
.Ltmp108:
	.size	dump_enabled_p, .Ltmp108-dump_enabled_p
	.cfi_endproc

	.globl	dump_flag_name
	.align	16, 0x90
	.type	dump_flag_name,@function
dump_flag_name:                         # @dump_flag_name
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	leaq	(%rax,%rax,2), %rax
	movq	dump_files+8(,%rax,8), %rax
	retq
.Ltmp109:
	.size	dump_flag_name, .Ltmp109-dump_flag_name
	.cfi_endproc

	.globl	dump_end
	.align	16, 0x90
	.type	dump_end,@function
dump_end:                               # @dump_end
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rdi
	jmp	fclose  # TAILCALL
.Ltmp110:
	.size	dump_end, .Ltmp110-dump_end
	.cfi_endproc

	.globl	dump_switch_p
	.align	16, 0x90
	.type	dump_switch_p,@function
dump_switch_p:                          # @dump_switch_p
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
	subq	$32, %rsp
.Ltmp122:
	.cfi_def_cfa_offset 80
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
	movq	%rdi, %r15
	movq	$1, %rbx
	movabsq	$dump_files+20, %r12
	.align	16, 0x90
.LBB13_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, 24(%rsp)          # 8-byte Spill
	movq	-12(%r12), %r13
	movq	%r13, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	strncmp
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	jne	.LBB13_17
# BB#2:                                 # %cond.end
                                        #   in Loop: Header=BB13_1 Depth=1
	addq	%r15, %r14
	jne	.LBB13_3
.LBB13_17:                              # %for.inc46
                                        #   in Loop: Header=BB13_1 Depth=1
	movq	%rbx, %rcx
	andq	%rdx, %rcx
	incq	%rbx
	addq	$24, %r12
	xorq	%rax, %rax
	cmpq	$5, %rcx
	jne	.LBB13_1
	jmp	.LBB13_18
.LBB13_3:                               # %while.cond.preheader
	movzbl	(%r14), %eax
	xorq	%rcx, %rcx
	testq	%rax, %rax
	movq	%r14, %r15
	je	.LBB13_16
	.align	16, 0x90
.LBB13_4:                               # %while.cond11.preheader.lr.ph.while.cond11.preheader.lr.ph.split_crit_edge
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB13_5 Depth 2
                                        #       Child Loop BB13_10 Depth 3
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%r15, %r13
	jmp	.LBB13_5
	.align	16, 0x90
.LBB13_6:                               # %while.cond11.while.cond11_crit_edge
                                        #   in Loop: Header=BB13_5 Depth=2
	incq	%r13
	movzbl	(%r13), %eax
.LBB13_5:                               # %while.cond11
                                        #   Parent Loop BB13_4 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB13_10 Depth 3
	andq	$255, %rax
	cmpq	$45, %rax
	je	.LBB13_6
# BB#7:                                 # %while.end
                                        #   in Loop: Header=BB13_5 Depth=2
	movq	$45, %rsi
	movq	%r13, %rdi
	callq	strchr
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB13_9
# BB#8:                                 # %if.then17
                                        #   in Loop: Header=BB13_5 Depth=2
	movq	%r13, %rdi
	callq	strlen
	movq	%rax, %r15
	addq	%r13, %r15
.LBB13_9:                               # %for.body23.lr.ph
                                        #   in Loop: Header=BB13_5 Depth=2
	movq	%r15, %rax
	subq	%r13, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rax, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
	movabsq	$dump_options+16, %r12
	movabsq	$.L.str11, %r14
	.align	16, 0x90
.LBB13_10:                              # %for.body23
                                        #   Parent Loop BB13_4 Depth=1
                                        #     Parent Loop BB13_5 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r14, %rdi
	callq	strlen
	cmpq	%rbx, %rax
	jne	.LBB13_13
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB13_10 Depth=3
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB13_12
.LBB13_13:                              # %for.inc
                                        #   in Loop: Header=BB13_10 Depth=3
	movq	(%r12), %r14
	addq	$16, %r12
	testq	%r14, %r14
	jne	.LBB13_10
# BB#14:                                # %for.end
                                        #   in Loop: Header=BB13_5 Depth=2
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	-12(%r12), %rcx
	xorq	%rax, %rax
	movabsq	$.L.str10, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	warning
	movzbl	(%r15), %eax
	testq	%rax, %rax
	movq	%r15, %r13
	jne	.LBB13_5
	jmp	.LBB13_15
	.align	16, 0x90
.LBB13_12:                              # %if.then33
                                        #   in Loop: Header=BB13_4 Depth=1
	movl	-8(%r12), %ecx
	movzbl	(%r15), %eax
	movq	8(%rsp), %rdx           # 8-byte Reload
	orq	%rcx, %rdx
	movq	%rdx, %rcx
	testq	%rax, %rax
	movq	24(%rsp), %r12          # 8-byte Reload
	jne	.LBB13_4
	jmp	.LBB13_16
.LBB13_15:
	movq	8(%rsp), %rcx           # 8-byte Reload
.LBB13_16:                              # %while.end39
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, (%r12)
	movl	%ecx, -4(%r12)
	movq	$1, %rax
.LBB13_18:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp128:
	.size	dump_switch_p, .Ltmp128-dump_switch_p
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"%-4s: "
	.size	.L.str, 7

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"type"
	.size	.L.str1, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%-4s: %-8lx "
	.size	.L.str2, 13

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"%-4s: %-7d "
	.size	.L.str3, 12

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%-13s "
	.size	.L.str4, 7

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"line"
	.size	.L.str5, 5

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"next"
	.size	.L.str6, 5

	.type	dump_files,@object      # @dump_files
	.data
	.align	16
dump_files:
	.quad	.L.str14
	.quad	.L.str15
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str16
	.quad	.L.str17
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str18
	.quad	.L.str19
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str20
	.quad	.L.str21
	.long	0                       # 0x0
	.long	0                       # 0x0
	.quad	.L.str22
	.quad	.L.str23
	.long	0                       # 0x0
	.long	0                       # 0x0
	.size	dump_files, 120

	.type	.L.str7,@object         # @.str7
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str7:
	.asciz	"w"
	.size	.L.str7, 2

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"a"
	.size	.L.str8, 2

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"could not open dump file `%s'"
	.size	.L.str9, 30

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"ignoring unknown option `%.*s' in `-f%s'"
	.size	.L.str10, 41

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"address"
	.size	.L.str11, 8

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"slim"
	.size	.L.str12, 5

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"all"
	.size	.L.str13, 4

	.type	dump_options,@object    # @dump_options
	.section	.rodata,"a",@progbits
	.align	16
dump_options:
	.quad	.L.str11
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str12
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str13
	.long	4294967295              # 0xffffffff
	.zero	4
	.quad	0
	.long	0                       # 0x0
	.zero	4
	.size	dump_options, 64

	.type	.L.str14,@object        # @.str14
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str14:
	.asciz	".tu"
	.size	.L.str14, 4

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"dump-translation-unit"
	.size	.L.str15, 22

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	".class"
	.size	.L.str16, 7

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"dump-class-hierarchy"
	.size	.L.str17, 21

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	".original"
	.size	.L.str18, 10

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"dump-tree-original"
	.size	.L.str19, 19

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	".optimized"
	.size	.L.str20, 11

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"dump-tree-optimized"
	.size	.L.str21, 20

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	".inlined"
	.size	.L.str22, 9

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"dump-tree-inlined"
	.size	.L.str23, 18

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"binfo"
	.size	.L.str24, 6

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"%-16s "
	.size	.L.str25, 7

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"pub"
	.size	.L.str26, 4

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"prot"
	.size	.L.str27, 5

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"priv"
	.size	.L.str28, 5

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"virt"
	.size	.L.str29, 5

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"base"
	.size	.L.str30, 5

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"op 0"
	.size	.L.str31, 5

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"op 1"
	.size	.L.str32, 5

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"tree-dump.c"
	.size	.L.str33, 12

	.type	.L__FUNCTION__.dequeue_and_dump,@object # @__FUNCTION__.dequeue_and_dump
.L__FUNCTION__.dequeue_and_dump:
	.asciz	"dequeue_and_dump"
	.size	.L__FUNCTION__.dequeue_and_dump, 17

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"name"
	.size	.L.str34, 5

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"mngl"
	.size	.L.str35, 5

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"scpe"
	.size	.L.str36, 5

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"srcp: %s:%-6d "
	.size	.L.str37, 15

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"artificial"
	.size	.L.str38, 11

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"chan"
	.size	.L.str39, 5

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"qual: %c%c%c     "
	.size	.L.str40, 18

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"unql"
	.size	.L.str41, 5

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"size"
	.size	.L.str42, 5

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"algn"
	.size	.L.str43, 5

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"strg"
	.size	.L.str44, 5

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"lngt"
	.size	.L.str45, 5

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"purp"
	.size	.L.str46, 5

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"valu"
	.size	.L.str47, 5

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"%u"
	.size	.L.str48, 3

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"prec"
	.size	.L.str49, 5

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"unsigned"
	.size	.L.str50, 9

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"min"
	.size	.L.str51, 4

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"max"
	.size	.L.str52, 4

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"csts"
	.size	.L.str53, 5

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"ptd"
	.size	.L.str54, 4

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"refd"
	.size	.L.str55, 5

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"clas"
	.size	.L.str56, 5

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"retn"
	.size	.L.str57, 5

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"prms"
	.size	.L.str58, 5

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"elts"
	.size	.L.str59, 5

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"domn"
	.size	.L.str60, 5

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"struct"
	.size	.L.str61, 7

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"union"
	.size	.L.str62, 6

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"flds"
	.size	.L.str63, 5

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"fncs"
	.size	.L.str64, 5

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"binf"
	.size	.L.str65, 5

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"cnst"
	.size	.L.str66, 5

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"argt"
	.size	.L.str67, 5

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"init"
	.size	.L.str68, 5

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"bitfield"
	.size	.L.str69, 9

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"bpos"
	.size	.L.str70, 5

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"used"
	.size	.L.str71, 5

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"register"
	.size	.L.str72, 9

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"args"
	.size	.L.str73, 5

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"undefined"
	.size	.L.str74, 10

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"extern"
	.size	.L.str75, 7

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"static"
	.size	.L.str76, 7

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"body"
	.size	.L.str77, 5

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"volatile"
	.size	.L.str78, 9

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"outs"
	.size	.L.str79, 5

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"ins"
	.size	.L.str80, 4

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"clbr"
	.size	.L.str81, 5

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"low"
	.size	.L.str82, 4

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"high"
	.size	.L.str83, 5

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"decl"
	.size	.L.str84, 5

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"expr"
	.size	.L.str85, 5

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"cond"
	.size	.L.str86, 5

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"dest"
	.size	.L.str87, 5

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"then"
	.size	.L.str88, 5

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"else"
	.size	.L.str89, 5

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"labl"
	.size	.L.str90, 5

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"begn"
	.size	.L.str91, 5

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"end"
	.size	.L.str92, 4

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"null"
	.size	.L.str93, 5

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"clnp"
	.size	.L.str94, 5

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"strg: %-7s "
	.size	.L.str95, 12

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"op 2"
	.size	.L.str96, 5

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"fn"
	.size	.L.str97, 3

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"stmt"
	.size	.L.str98, 5

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"vars"
	.size	.L.str99, 5

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"addr"
	.size	.L.str100, 5

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"%-4s: %-7s "
	.size	.L.str102, 12

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"%*s"
	.size	.L.str103, 4

	.type	.L.str104,@object       # @.str104
.L.str104:
	.zero	1
	.size	.L.str104, 1

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"\n%*s"
	.size	.L.str105, 5

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"@%-6u "
	.size	.L.str106, 7


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
