	.file	"stringpool.c.bc"
	.text
	.globl	init_stringpool
	.align	16, 0x90
	.type	init_stringpool,@function
init_stringpool:                        # @init_stringpool
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	$14, %rdi
	callq	ht_create
	movq	%rax, ident_hash(%rip)
	movabsq	$ident_hash, %rbx
	movq	$alloc_node, 96(%rax)
	movabsq	$string_stack, %rdi
	callq	gcc_obstack_init
	movabsq	$mark_ident_hash, %rcx
	movq	$1, %rsi
	movq	$8, %rdx
	movq	%rbx, %rdi
	popq	%rbx
	jmp	ggc_add_root  # TAILCALL
.Ltmp4:
	.size	init_stringpool, .Ltmp4-init_stringpool
	.cfi_endproc

	.align	16, 0x90
	.type	alloc_node,@function
alloc_node:                             # @alloc_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp6:
	.cfi_def_cfa_offset 16
	movq	$1, %rdi
	callq	make_node
	addq	$24, %rax
	popq	%rdx
	retq
.Ltmp7:
	.size	alloc_node, .Ltmp7-alloc_node
	.cfi_endproc

	.align	16, 0x90
	.type	mark_ident_hash,@function
mark_ident_hash:                        # @mark_ident_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ident_hash(%rip), %rdi
	movabsq	$mark_ident, %rsi
	xorq	%rdx, %rdx
	jmp	ht_forall  # TAILCALL
.Ltmp8:
	.size	mark_ident_hash, .Ltmp8-mark_ident_hash
	.cfi_endproc

	.globl	ggc_alloc_string
	.align	16, 0x90
	.type	ggc_alloc_string,@function
ggc_alloc_string:                       # @ggc_alloc_string
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
	movq	%rdi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rsi, %rax
	andq	%rbx, %rax
	cmpq	%rbx, %rax
	jne	.LBB3_2
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %rsi
.LBB3_2:                                # %if.end
	testq	%rbx, %rsi
	jne	.LBB3_4
# BB#3:
	movabsq	$empty_string, %rax
	jmp	.LBB3_14
.LBB3_4:                                # %if.end
	andq	%rsi, %rbx
	cmpq	$1, %rbx
	jne	.LBB3_7
# BB#5:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	movq	$56, %rcx
	movzbl	_sch_istable(%rax,%rax), %edx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	testq	$4, %rdx
	je	.LBB3_7
# BB#6:                                 # %if.then11
	leaq	digit_vector-96(%rax,%rax), %rax
	jmp	.LBB3_14
.LBB3_7:                                # %if.end14
	movq	string_stack+24(%rip), %rdi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	string_stack+32(%rip), %rax
	sarq	%cl, %rbx
	leaq	1(%rbx), %r15
	leaq	1(%rdi,%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB3_9
# BB#8:                                 # %if.then20
	incq	%rsi
	movabsq	$string_stack, %rdi
	callq	_obstack_newchunk
	movq	string_stack+24(%rip), %rdi
.LBB3_9:                                # %if.end21
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	string_stack+24(%rip), %rax
	addq	%rax, %r15
	movq	%r15, string_stack+24(%rip)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rbx)
	movq	string_stack+16(%rip), %rax
	movq	string_stack+24(%rip), %rdx
	cmpq	%rax, %rdx
	jne	.LBB3_11
# BB#10:                                # %if.then31
	movzbl	string_stack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, string_stack+80(%rip)
.LBB3_11:                               # %if.end32
	movslq	string_stack+48(%rip), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, string_stack+24(%rip)
	movq	string_stack+32(%rip), %rbx
	movq	string_stack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB3_13
# BB#12:                                # %if.then51
	movq	%rbx, string_stack+24(%rip)
	movq	%rbx, %rcx
.LBB3_13:                               # %if.end54
	movq	%rcx, string_stack+16(%rip)
.LBB3_14:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp19:
	.size	ggc_alloc_string, .Ltmp19-ggc_alloc_string
	.cfi_endproc

	.globl	get_identifier
	.align	16, 0x90
	.type	get_identifier,@function
get_identifier:                         # @get_identifier
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp25:
	.cfi_def_cfa_offset 32
.Ltmp26:
	.cfi_offset %rbx, -24
.Ltmp27:
	.cfi_offset %r14, -16
	movq	ident_hash(%rip), %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	addq	$-24, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp28:
	.size	get_identifier, .Ltmp28-get_identifier
	.cfi_endproc

	.globl	get_identifier_with_length
	.align	16, 0x90
	.type	get_identifier_with_length,@function
get_identifier_with_length:             # @get_identifier_with_length
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp30:
	.cfi_def_cfa_offset 16
	movq	ident_hash(%rip), %rax
	movq	%rsi, %rdx
	movq	%rdi, %rsi
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	ht_lookup
	addq	$-24, %rax
	popq	%rdx
	retq
.Ltmp31:
	.size	get_identifier_with_length, .Ltmp31-get_identifier_with_length
	.cfi_endproc

	.globl	maybe_get_identifier
	.align	16, 0x90
	.type	maybe_get_identifier,@function
maybe_get_identifier:                   # @maybe_get_identifier
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 32
.Ltmp39:
	.cfi_offset %rbx, -32
.Ltmp40:
	.cfi_offset %r14, -24
.Ltmp41:
	.cfi_offset %r15, -16
	movq	ident_hash(%rip), %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	strlen
	xorq	%rcx, %rcx
	xorq	%r15, %r15
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	ht_lookup
	testq	%rax, %rax
	je	.LBB6_2
# BB#1:                                 # %if.then
	addq	$-24, %rax
	movq	%rax, %r15
.LBB6_2:                                # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp42:
	.size	maybe_get_identifier, .Ltmp42-maybe_get_identifier
	.cfi_endproc

	.globl	stringpool_statistics
	.align	16, 0x90
	.type	stringpool_statistics,@function
stringpool_statistics:                  # @stringpool_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	movq	ident_hash(%rip), %rdi
	jmp	ht_dump_statistics  # TAILCALL
.Ltmp43:
	.size	stringpool_statistics, .Ltmp43-stringpool_statistics
	.cfi_endproc

	.align	16, 0x90
	.type	mark_ident,@function
mark_ident:                             # @mark_ident
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 16
.Ltmp47:
	.cfi_offset %rbx, -16
	movq	%rsi, %rbx
	addq	$-24, %rbx
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_4
# BB#1:                                 # %do.body1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB8_3
# BB#2:                                 # %if.then2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB8_3:                                # %if.end
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB8_4:                                # %do.end7
	movq	$1, %rax
	popq	%rbx
	retq
.Ltmp48:
	.size	mark_ident, .Ltmp48-mark_ident
	.cfi_endproc

	.type	empty_string,@object    # @empty_string
	.section	.rodata,"a",@progbits
	.globl	empty_string
empty_string:
	.zero	1
	.size	empty_string, 1

	.type	digit_vector,@object    # @digit_vector
	.globl	digit_vector
	.align	16
digit_vector:
	.asciz	"0\0001\0002\0003\0004\0005\0006\0007\0008\0009"
	.size	digit_vector, 20

	.type	ident_hash,@object      # @ident_hash
	.comm	ident_hash,8,8
	.type	string_stack,@object    # @string_stack
	.local	string_stack
	.comm	string_stack,88,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
