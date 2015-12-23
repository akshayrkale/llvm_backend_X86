	.file	"hash.c.bc"
	.text
	.globl	hash_table_init_n
	.align	16, 0x90
	.type	hash_table_init_n,@function
hash_table_init_n:                      # @hash_table_init_n
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
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
	movq	%r8, %r13
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	movq	$3, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	leaq	40(%rbx), %r15
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	xorq	%rdx, %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	_obstack_begin
	movq	64(%rbx), %rax
	movq	72(%rbx), %rdx
	subq	%rax, %rdx
	movq	$35, %rcx
	movq	%r13, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	cmpq	%r12, %rdx
	jge	.LBB0_2
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	_obstack_newchunk
	movq	64(%rbx), %rax
.LBB0_2:                                # %if.end
	addq	%r12, %rax
	movq	%rax, 64(%rbx)
	movq	56(%rbx), %rdi
	cmpq	%rdi, %rax
	jne	.LBB0_4
# BB#3:                                 # %if.then11
	movzbl	120(%rbx), %ecx
	orq	$2, %rcx
	movb	%cl, 120(%rbx)
.LBB0_4:                                # %if.end12
	movslq	88(%rbx), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, 64(%rbx)
	movq	72(%rbx), %r8
	movq	48(%rbx), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB0_6
# BB#5:                                 # %if.then32
	movq	%r8, 64(%rbx)
	movq	%r8, %rcx
.LBB0_6:                                # %if.end35
	movq	%rcx, 56(%rbx)
	movq	%rdi, (%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r14
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	memset
	movl	%r13d, 8(%rbx)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 16(%rbx)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%rax, 24(%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 32(%rbx)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	hash_table_init_n, .Ltmp17-hash_table_init_n
	.cfi_endproc

	.globl	hash_table_init
	.align	16, 0x90
	.type	hash_table_init,@function
hash_table_init:                        # @hash_table_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rbx
	leaq	40(%rbx), %r13
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	movq	$8072, %rsi             # imm = 0x1F88
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	callq	_obstack_begin
	movq	64(%rbx), %rax
	movq	72(%rbx), %rcx
	subq	%rax, %rcx
	cmpq	$8071, %rcx             # imm = 0x1F87
	jg	.LBB1_2
# BB#1:                                 # %if.then.i
	movq	$8072, %rsi             # imm = 0x1F88
	movq	%r13, %rdi
	callq	_obstack_newchunk
	movq	64(%rbx), %rax
.LBB1_2:                                # %if.end.i
	addq	$8072, %rax             # imm = 0x1F88
	movq	%rax, 64(%rbx)
	movq	56(%rbx), %rdi
	cmpq	%rdi, %rax
	jne	.LBB1_4
# BB#3:                                 # %if.then11.i
	movzbl	120(%rbx), %ecx
	orq	$2, %rcx
	movb	%cl, 120(%rbx)
.LBB1_4:                                # %if.end12.i
	movslq	88(%rbx), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, 64(%rbx)
	movq	72(%rbx), %r8
	movq	48(%rbx), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB1_6
# BB#5:                                 # %if.then32.i
	movq	%r8, 64(%rbx)
	movq	%r8, %rcx
.LBB1_6:                                # %hash_table_init_n.exit
	movq	%rcx, 56(%rbx)
	movq	%rdi, (%rbx)
	xorq	%rsi, %rsi
	movq	$8072, %rdx             # imm = 0x1F88
	callq	memset
	movq	$1009, %rax             # imm = 0x3F1
	movl	%eax, 8(%rbx)
	movq	%r12, 16(%rbx)
	movq	%r15, 24(%rbx)
	movq	%r14, 32(%rbx)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	hash_table_init, .Ltmp34-hash_table_init
	.cfi_endproc

	.globl	hash_table_free
	.align	16, 0x90
	.type	hash_table_free,@function
hash_table_free:                        # @hash_table_free
	.cfi_startproc
# BB#0:                                 # %entry
	addq	$40, %rdi
	xorq	%rsi, %rsi
	jmp	obstack_free  # TAILCALL
.Ltmp35:
	.size	hash_table_free, .Ltmp35-hash_table_free
	.cfi_endproc

	.globl	hash_lookup
	.align	16, 0x90
	.type	hash_lookup,@function
hash_lookup:                            # @hash_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp47:
	.cfi_def_cfa_offset 64
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	%r14, %rdi
	callq	*24(%r15)
	movl	8(%r15), %ecx
	movq	%rax, %r13
	xorq	%rdx, %rdx
                                        # kill: RAX<def> R13<kill>
	divq	%rcx
	movq	%rdx, %r12
	movq	(%r15), %rax
	movq	(%rax,%r12,8), %rbx
	jmp	.LBB3_1
	.align	16, 0x90
.LBB3_5:                                # %for.inc
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	(%rbx), %rbx
.LBB3_1:                                # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB3_6
# BB#2:                                 # %for.body
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	16(%rbx), %rax
	cmpq	%r13, %rax
	jne	.LBB3_5
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	8(%rbx), %rdi
	movq	%r14, %rsi
	callq	*32(%r15)
	testq	%rax, %rax
	je	.LBB3_5
# BB#4:
	movq	%rbx, %rax
	jmp	.LBB3_11
.LBB3_6:                                # %for.end
	movq	%r12, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%rax, %rcx
	movabsq	$0, %rax
	je	.LBB3_11
# BB#7:                                 # %if.end12
	xorq	%rdi, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	*16(%r15)
	movq	%rax, %r12
	xorq	%rax, %rax
	testq	%r12, %r12
	je	.LBB3_11
# BB#8:                                 # %if.end17
	movq	(%rsp), %rcx            # 8-byte Reload
	testq	%rcx, %rcx
	je	.LBB3_10
# BB#9:                                 # %if.then19
	leaq	40(%r15), %rdi
	movq	%r14, %rsi
	callq	*%rcx
	movq	%rax, %r14
.LBB3_10:                               # %if.end21
	movq	%r14, 8(%r12)
	movq	%r13, 16(%r12)
	movq	(%r15), %rax
	movq	(%rax,%rbx,8), %rcx
	movq	%rcx, (%r12)
	movq	%r12, (%rax,%rbx,8)
	movq	%r12, %rax
.LBB3_11:                               # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	hash_lookup, .Ltmp53-hash_lookup
	.cfi_endproc

	.globl	hash_newfunc
	.align	16, 0x90
	.type	hash_newfunc,@function
hash_newfunc:                           # @hash_newfunc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp56:
	.cfi_def_cfa_offset 16
.Ltmp57:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	testq	%rdi, %rdi
	jne	.LBB4_8
# BB#1:                                 # %if.then
	movq	64(%rbx), %rax
	movq	72(%rbx), %rcx
	subq	%rax, %rcx
	cmpq	$23, %rcx
	jg	.LBB4_3
# BB#2:                                 # %if.then.i
	leaq	40(%rbx), %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	64(%rbx), %rax
.LBB4_3:                                # %if.end.i
	addq	$24, %rax
	movq	%rax, 64(%rbx)
	movq	56(%rbx), %rdi
	cmpq	%rdi, %rax
	jne	.LBB4_5
# BB#4:                                 # %if.then8.i
	movzbl	120(%rbx), %ecx
	orq	$2, %rcx
	movb	%cl, 120(%rbx)
.LBB4_5:                                # %if.end9.i
	movslq	88(%rbx), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, 64(%rbx)
	movq	72(%rbx), %r8
	movq	48(%rbx), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB4_7
# BB#6:                                 # %if.then29.i
	movq	%r8, 64(%rbx)
	movq	%r8, %rcx
.LBB4_7:                                # %hash_allocate.exit
	movq	%rcx, 56(%rbx)
.LBB4_8:                                # %if.end
	movq	%rdi, %rax
	popq	%rbx
	retq
.Ltmp58:
	.size	hash_newfunc, .Ltmp58-hash_newfunc
	.cfi_endproc

	.globl	hash_allocate
	.align	16, 0x90
	.type	hash_allocate,@function
hash_allocate:                          # @hash_allocate
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp64:
	.cfi_def_cfa_offset 32
.Ltmp65:
	.cfi_offset %rbx, -24
.Ltmp66:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	64(%r14), %rdx
	movq	72(%r14), %rax
	subq	%rdx, %rax
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rax
	jge	.LBB5_2
# BB#1:                                 # %if.then
	leaq	40(%r14), %rdi
	callq	_obstack_newchunk
	movq	64(%r14), %rdx
.LBB5_2:                                # %if.end
	addq	%rbx, %rdx
	movq	%rdx, 64(%r14)
	movq	56(%r14), %rax
	cmpq	%rax, %rdx
	jne	.LBB5_4
# BB#3:                                 # %if.then8
	movzbl	120(%r14), %ecx
	orq	$2, %rcx
	movb	%cl, 120(%r14)
.LBB5_4:                                # %if.end9
	movslq	88(%r14), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, 64(%r14)
	movq	72(%r14), %rbx
	movq	48(%r14), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB5_6
# BB#5:                                 # %if.then29
	movq	%rbx, 64(%r14)
	movq	%rbx, %rcx
.LBB5_6:                                # %if.end32
	movq	%rcx, 56(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp67:
	.size	hash_allocate, .Ltmp67-hash_allocate
	.cfi_endproc

	.globl	hash_traverse
	.align	16, 0x90
	.type	hash_traverse,@function
hash_traverse:                          # @hash_traverse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp74:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp75:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp76:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp77:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp79:
	.cfi_def_cfa_offset 64
.Ltmp80:
	.cfi_offset %rbx, -48
.Ltmp81:
	.cfi_offset %r12, -40
.Ltmp82:
	.cfi_offset %r13, -32
.Ltmp83:
	.cfi_offset %r14, -24
.Ltmp84:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movl	8(%rdi), %eax
	movq	%rdx, %r15
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB6_7
# BB#1:                                 # %for.body.lr.ph
	xorq	%r13, %r13
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB6_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_4 Depth 2
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx), %rcx
	movq	%r13, %rdx
	andq	%r14, %rdx
	movq	(%rcx,%rdx,8), %rbx
	testq	%rbx, %rbx
	je	.LBB6_6
	.align	16, 0x90
.LBB6_4:                                # %for.body4
                                        #   Parent Loop BB6_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*%r12
	testq	%rax, %rax
	je	.LBB6_7
# BB#3:                                 # %for.cond2
                                        #   in Loop: Header=BB6_4 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_4
# BB#5:                                 # %for.cond2.for.inc5_crit_edge
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	8(%rax), %eax
.LBB6_6:                                # %for.inc5
                                        #   in Loop: Header=BB6_2 Depth=1
	incq	%r13
	movq	%r13, %rcx
	andq	%r14, %rcx
	movq	%rax, %rdx
	andq	%r14, %rdx
	cmpq	%rdx, %rcx
	jb	.LBB6_2
.LBB6_7:                                # %for.end6
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp85:
	.size	hash_traverse, .Ltmp85-hash_traverse
	.cfi_endproc

	.globl	string_hash
	.align	16, 0x90
	.type	string_hash,@function
string_hash:                            # @string_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	movabsq	$0, %rax
	je	.LBB7_4
# BB#1:                                 # %while.body.preheader
	incq	%rdi
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB7_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	imulq	$131073, %rcx, %rsi     # imm = 0x20001
	addq	%rax, %rsi
	movq	$2, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movzbl	(%rdi), %ecx
	xorq	%rsi, %rax
	incq	%rdx
	incq	%rdi
	testq	%rcx, %rcx
	jne	.LBB7_2
# BB#3:                                 # %while.cond.while.end_crit_edge
	imulq	$131073, %rdx, %rcx     # imm = 0x20001
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
.LBB7_4:                                # %while.end
	addq	%rax, %rdx
	movq	$2, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	xorq	%rdx, %rax
	retq
.Ltmp86:
	.size	string_hash, .Ltmp86-string_hash
	.cfi_endproc

	.globl	string_compare
	.align	16, 0x90
	.type	string_compare,@function
string_compare:                         # @string_compare
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp88:
	.cfi_def_cfa_offset 16
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	popq	%rdx
	retq
.LBB8_1:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp89:
	.size	string_compare, .Ltmp89-string_compare
	.cfi_endproc

	.globl	string_copy
	.align	16, 0x90
	.type	string_copy,@function
string_copy:                            # @string_copy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp95:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp96:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp97:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp98:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp99:
	.cfi_def_cfa_offset 48
.Ltmp100:
	.cfi_offset %rbx, -40
.Ltmp101:
	.cfi_offset %r12, -32
.Ltmp102:
	.cfi_offset %r14, -24
.Ltmp103:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	%r14, %rdi
	callq	strlen
	incq	%rax
	movq	24(%r12), %rdx
	movq	32(%r12), %rsi
	subq	%rdx, %rsi
	movq	$32, %rcx
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rsi
	jge	.LBB9_2
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	_obstack_newchunk
	movq	24(%r12), %rdx
.LBB9_2:                                # %if.end
	addq	%rbx, %rdx
	movq	%rdx, 24(%r12)
	movq	16(%r12), %r15
	cmpq	%r15, %rdx
	jne	.LBB9_4
# BB#3:                                 # %if.then9
	movzbl	80(%r12), %eax
	orq	$2, %rax
	movb	%al, 80(%r12)
.LBB9_4:                                # %if.end10
	movslq	48(%r12), %rax
	addq	%rax, %rdx
	notq	%rax
	andq	%rdx, %rax
	movq	%rax, 24(%r12)
	movq	32(%r12), %rcx
	movq	8(%r12), %rdx
	movq	%rax, %rsi
	subq	%rdx, %rsi
	movq	%rcx, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB9_6
# BB#5:                                 # %if.then31
	movq	%rcx, 24(%r12)
	movq	%rcx, %rax
.LBB9_6:                                # %if.end34
	movq	%rax, 16(%r12)
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	strcpy
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp104:
	.size	string_copy, .Ltmp104-string_copy
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
