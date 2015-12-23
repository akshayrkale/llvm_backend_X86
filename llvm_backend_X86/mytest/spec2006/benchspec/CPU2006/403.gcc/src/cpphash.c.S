	.file	"cpphash.c.bc"
	.text
	.globl	_cpp_init_hashtable
	.align	16, 0x90
	.type	_cpp_init_hashtable,@function
_cpp_init_hashtable:                    # @_cpp_init_hashtable
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
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%r14, %r14
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movq	$1, %rax
	movb	%al, 825(%rbx)
	movq	$13, %rdi
	callq	ht_create
	movq	%rax, %r14
	movq	$alloc_node, 96(%r14)
	leaq	400(%rbx), %rdi
	callq	gcc_obstack_init
.LBB0_2:                                # %if.end
	movq	%rbx, 112(%r14)
	movq	%r14, 640(%rbx)
	movq	%rbx, %rdi
	callq	_cpp_init_directives
	movq	%rbx, %rdi
	callq	_cpp_init_internal_pragmas
	movq	640(%rbx), %rdi
	movabsq	$.L.str, %rsi
	movq	$7, %rdx
	movq	$1, %rcx
	callq	ht_lookup
	movq	%rax, 784(%rbx)
	movq	640(%rbx), %rdi
	movabsq	$.L.str1, %rsi
	movq	$4, %rdx
	movq	$1, %rcx
	callq	ht_lookup
	movq	%rax, 792(%rbx)
	movq	640(%rbx), %rdi
	movabsq	$.L.str2, %rsi
	movq	$5, %rdx
	movq	$1, %rcx
	callq	ht_lookup
	movq	%rax, 800(%rbx)
	movq	640(%rbx), %rdi
	movabsq	$.L.str3, %rsi
	movq	$15, %rdx
	movq	$1, %rcx
	callq	ht_lookup
	movq	%rax, 808(%rbx)
	movq	640(%rbx), %rdi
	movabsq	$.L.str4, %rsi
	movq	$11, %rdx
	movq	$1, %rcx
	callq	ht_lookup
	movq	%rax, 816(%rbx)
	movzbl	21(%rax), %ecx
	orq	$8, %rcx
	movb	%cl, 21(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	_cpp_init_hashtable, .Ltmp8-_cpp_init_hashtable
	.cfi_endproc

	.align	16, 0x90
	.type	alloc_node,@function
alloc_node:                             # @alloc_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp11:
	.cfi_def_cfa_offset 16
.Ltmp12:
	.cfi_offset %rbx, -16
	movq	112(%rdi), %rbx
	movq	424(%rbx), %rcx
	movq	432(%rbx), %rax
	subq	%rcx, %rax
	cmpq	$31, %rax
	jg	.LBB1_2
# BB#1:                                 # %if.then
	leaq	400(%rbx), %rdi
	movq	$32, %rsi
	callq	_obstack_newchunk
	movq	424(%rbx), %rcx
.LBB1_2:                                # %if.end
	addq	$32, %rcx
	movq	%rcx, 424(%rbx)
	movq	416(%rbx), %rax
	cmpq	%rax, %rcx
	jne	.LBB1_4
# BB#3:                                 # %if.then8
	movzbl	480(%rbx), %edx
	orq	$2, %rdx
	movb	%dl, 480(%rbx)
.LBB1_4:                                # %if.end9
	movslq	448(%rbx), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, 424(%rbx)
	movq	432(%rbx), %r8
	movq	408(%rbx), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB1_6
# BB#5:                                 # %if.then29
	movq	%r8, 424(%rbx)
	movq	%r8, %rdx
.LBB1_6:                                # %if.end32
	movq	%rdx, 416(%rbx)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	popq	%rbx
	retq
.Ltmp13:
	.size	alloc_node, .Ltmp13-alloc_node
	.cfi_endproc

	.globl	cpp_lookup
	.align	16, 0x90
	.type	cpp_lookup,@function
cpp_lookup:                             # @cpp_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	movq	640(%rdi), %rdi
	movq	$1, %rcx
	jmp	ht_lookup  # TAILCALL
.Ltmp14:
	.size	cpp_lookup, .Ltmp14-cpp_lookup
	.cfi_endproc

	.globl	_cpp_destroy_hashtable
	.align	16, 0x90
	.type	_cpp_destroy_hashtable,@function
_cpp_destroy_hashtable:                 # @_cpp_destroy_hashtable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp17:
	.cfi_def_cfa_offset 16
.Ltmp18:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	825(%rbx), %eax
	testq	%rax, %rax
	je	.LBB3_1
# BB#2:                                 # %if.then
	movq	640(%rbx), %rdi
	callq	ht_destroy
	addq	$400, %rbx              # imm = 0x190
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	popq	%rbx
	jmp	obstack_free  # TAILCALL
.LBB3_1:                                # %if.end3
	popq	%rbx
	retq
.Ltmp19:
	.size	_cpp_destroy_hashtable, .Ltmp19-_cpp_destroy_hashtable
	.cfi_endproc

	.globl	cpp_defined
	.align	16, 0x90
	.type	cpp_defined,@function
cpp_defined:                            # @cpp_defined
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 16
.Ltmp23:
	.cfi_offset %rbx, -16
	movq	640(%rdi), %rdi
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	callq	ht_lookup
	testq	%rax, %rax
	je	.LBB4_4
# BB#1:                                 # %land.rhs
	movzbl	20(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB4_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB4_4
.LBB4_2:
	movabsq	$0, %rbx
.LBB4_4:                                # %land.end
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp24:
	.size	cpp_defined, .Ltmp24-cpp_defined
	.cfi_endproc

	.globl	cpp_forall_identifiers
	.align	16, 0x90
	.type	cpp_forall_identifiers,@function
cpp_forall_identifiers:                 # @cpp_forall_identifiers
	.cfi_startproc
# BB#0:                                 # %entry
	movq	640(%rdi), %rdi
	jmp	ht_forall  # TAILCALL
.Ltmp25:
	.size	cpp_forall_identifiers, .Ltmp25-cpp_forall_identifiers
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"defined"
	.size	.L.str, 8

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"true"
	.size	.L.str1, 5

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"false"
	.size	.L.str2, 6

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"__STRICT_ANSI__"
	.size	.L.str3, 16

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"__VA_ARGS__"
	.size	.L.str4, 12


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
