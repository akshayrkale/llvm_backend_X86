	.file	"alloca.c.bc"
	.text
	.globl	C_alloca
	.align	16, 0x90
	.type	C_alloca,@function
C_alloca:                               # @C_alloca
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
	movl	stack_dir(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then
	callq	find_stack_direction
.LBB0_2:                                # %if.end
	movq	last_alloca_header(%rip), %rdi
	xorq	%r15, %r15
	testq	%rdi, %rdi
	movabsq	$0, %rax
	je	.LBB0_12
# BB#3:
	leaq	7(%rsp), %r12
	.align	16, 0x90
.LBB0_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	stack_dir(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_6
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	8(%rdi), %rcx
	cmpq	%r12, %rcx
	ja	.LBB0_10
.LBB0_6:                                # %lor.lhs.false
                                        #   in Loop: Header=BB0_4 Depth=1
	testq	%rax, %rax
	jns	.LBB0_7
# BB#8:                                 # %land.lhs.true5
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	8(%rdi), %rax
	cmpq	%r12, %rax
	jae	.LBB0_9
.LBB0_10:                               # %if.then9
                                        #   in Loop: Header=BB0_4 Depth=1
	movq	(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB0_4
# BB#11:
	xorq	%rax, %rax
	jmp	.LBB0_12
.LBB0_7:
	movq	%rdi, %rax
	jmp	.LBB0_12
.LBB0_9:
	movq	%rdi, %rax
.LBB0_12:                               # %for.end
	movq	%rax, last_alloca_header(%rip)
	testq	%r14, %r14
	je	.LBB0_15
# BB#13:                                # %if.end14
	addq	$16, %r14
	movq	%r14, %rdi
	callq	xmalloc
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB0_16
# BB#14:                                # %if.end17
	movq	last_alloca_header(%rip), %rax
	movq	%rax, (%r15)
	leaq	7(%rsp), %rax
	movq	%rax, 8(%r15)
	movq	%r15, last_alloca_header(%rip)
	addq	$16, %r15
.LBB0_15:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB0_16:                               # %if.then16
	callq	abort
.Ltmp14:
	.size	C_alloca, .Ltmp14-C_alloca
	.cfi_endproc

	.align	16, 0x90
	.type	find_stack_direction,@function
find_stack_direction:                   # @find_stack_direction
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp16:
	.cfi_def_cfa_offset 16
	movq	find_stack_direction.addr(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_1
# BB#2:                                 # %if.else
	leaq	7(%rsp), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB1_4
# BB#3:                                 # %if.then2
	movq	$1, %rax
	jmp	.LBB1_5
.LBB1_1:                                # %if.then
	leaq	7(%rsp), %rax
	movq	%rax, find_stack_direction.addr(%rip)
	callq	find_stack_direction
	popq	%rax
	retq
.LBB1_4:                                # %if.else3
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB1_5:                                # %if.end4
	movl	%eax, stack_dir(%rip)
	popq	%rax
	retq
.Ltmp17:
	.size	find_stack_direction, .Ltmp17-find_stack_direction
	.cfi_endproc

	.type	stack_dir,@object       # @stack_dir
	.local	stack_dir
	.comm	stack_dir,4,4
	.type	last_alloca_header,@object # @last_alloca_header
	.local	last_alloca_header
	.comm	last_alloca_header,8,8
	.type	find_stack_direction.addr,@object # @find_stack_direction.addr
	.local	find_stack_direction.addr
	.comm	find_stack_direction.addr,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
