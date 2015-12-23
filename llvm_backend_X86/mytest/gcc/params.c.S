	.file	"params.c.bc"
	.text
	.globl	add_params
	.align	16, 0x90
	.type	add_params,@function
add_params:                             # @add_params
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
	movq	num_compiler_params(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	movq	compiler_params(%rip), %rdi
	addq	%rbx, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rsi
	callq	xrealloc
	movq	%rax, compiler_params(%rip)
	movq	num_compiler_params(%rip), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	leaq	(%rax,%rcx,8), %rdi
	movq	$3, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rdx
	movq	%r14, %rsi
	callq	memcpy
	addq	%rbx, num_compiler_params(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp8:
	.size	add_params, .Ltmp8-add_params
	.cfi_endproc

	.globl	set_param_value
	.align	16, 0x90
	.type	set_param_value,@function
set_param_value:                        # @set_param_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp15:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp16:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp17:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp20:
	.cfi_def_cfa_offset 64
.Ltmp21:
	.cfi_offset %rbx, -48
.Ltmp22:
	.cfi_offset %r12, -40
.Ltmp23:
	.cfi_offset %r13, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rsi, %rax
	andq	%r15, %rax
	cmpq	%r15, %rax
	je	.LBB1_3
# BB#1:                                 # %for.cond.preheader
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	num_compiler_params(%rip), %r13
	testq	%r13, %r13
	je	.LBB1_7
# BB#2:                                 # %for.body.lr.ph
	movq	compiler_params(%rip), %rbx
	addq	$8, %rbx
	xorq	%r14, %r14
	.align	16, 0x90
.LBB1_5:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rbx), %rdi
	movq	%r12, %rsi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB1_6
# BB#4:                                 # %for.cond
                                        #   in Loop: Header=BB1_5 Depth=1
	incq	%r14
	addq	$24, %rbx
	cmpq	%r13, %r14
	jb	.LBB1_5
.LBB1_7:                                # %for.end
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	error  # TAILCALL
.LBB1_6:                                # %return
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	%eax, (%rbx)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_3:                                # %if.then
	callq	abort
.Ltmp26:
	.size	set_param_value, .Ltmp26-set_param_value
	.cfi_endproc

	.type	compiler_params,@object # @compiler_params
	.comm	compiler_params,8,8
	.type	num_compiler_params,@object # @num_compiler_params
	.local	num_compiler_params
	.comm	num_compiler_params,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"invalid parameter `%s'"
	.size	.L.str, 23


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
