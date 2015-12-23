	.file	"cfg.c.bc"
	.text
	.globl	init_flow
	.align	16, 0x90
	.type	init_flow,@function
init_flow:                              # @init_flow
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	xorq	%rax, %rax
	movzbl	init_flow.initialized(%rip), %ecx
	movl	%eax, n_edges(%rip)
	movq	$0, first_deleted_edge(%rip)
	movq	$0, first_deleted_block(%rip)
	testq	%rcx, %rcx
	je	.LBB0_1
# BB#8:                                 # %if.else
	movq	flow_firstobj(%rip), %rsi
	movq	flow_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB0_11
# BB#9:                                 # %if.else
	movq	flow_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB0_11
# BB#10:                                # %if.then46
	movq	%rsi, flow_obstack+16(%rip)
	movq	%rsi, flow_obstack+24(%rip)
	jmp	.LBB0_12
.LBB0_1:                                # %if.then
	movabsq	$flow_obstack, %rdi
	callq	gcc_obstack_init
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB0_3
# BB#2:                                 # %if.then2
	movabsq	$flow_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rcx
.LBB0_3:                                # %if.end
	movq	flow_obstack+16(%rip), %r8
	cmpq	%r8, %rcx
	jne	.LBB0_5
# BB#4:                                 # %if.then9
	movzbl	flow_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, flow_obstack+80(%rip)
.LBB0_5:                                # %if.end10
	movslq	flow_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB0_7
# BB#6:                                 # %if.then30
	movq	%rax, flow_obstack+24(%rip)
	movq	%rax, %rdx
.LBB0_7:                                # %if.end33
	movq	%rdx, flow_obstack+16(%rip)
	movq	%r8, flow_firstobj(%rip)
	movq	$1, %rax
	movb	%al, init_flow.initialized(%rip)
	popq	%rax
	retq
.LBB0_11:                               # %if.else49
	movabsq	$flow_obstack, %rdi
	callq	obstack_free
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+24(%rip), %rsi
.LBB0_12:                               # %if.end50
	cmpq	%rsi, %rax
	jns	.LBB0_14
# BB#13:                                # %if.then65
	movabsq	$flow_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rsi
.LBB0_14:                               # %if.end66
	movq	flow_obstack+16(%rip), %r8
	cmpq	%r8, %rsi
	jne	.LBB0_16
# BB#15:                                # %if.then79
	movzbl	flow_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, flow_obstack+80(%rip)
.LBB0_16:                               # %if.end84
	movslq	flow_obstack+48(%rip), %rcx
	addq	%rcx, %rsi
	notq	%rcx
	andq	%rsi, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+8(%rip), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB0_18
# BB#17:                                # %if.then109
	movq	%rax, flow_obstack+24(%rip)
	movq	%rax, %rcx
.LBB0_18:                               # %if.end112
	movq	%rcx, flow_obstack+16(%rip)
	movq	%r8, flow_firstobj(%rip)
	popq	%rax
	retq
.Ltmp2:
	.size	init_flow, .Ltmp2-init_flow
	.cfi_endproc

	.globl	clear_edges
	.align	16, 0x90
	.type	clear_edges,@function
clear_edges:                            # @clear_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp4:
	.cfi_def_cfa_offset 16
	movslq	n_basic_blocks(%rip), %r8
	testq	%r8, %r8
	jle	.LBB1_6
# BB#1:
	xorq	%rax, %rax
	.align	16, 0x90
.LBB1_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_3 Depth 2
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rax,8), %rdx
	movq	40(%rdx), %rcx
	testq	%rcx, %rcx
	je	.LBB1_5
	.align	16, 0x90
.LBB1_3:                                # %while.body
                                        #   Parent Loop BB1_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	8(%rcx), %rsi
	movl	n_edges(%rip), %edi
	decq	%rdi
	movl	%edi, n_edges(%rip)
	movq	$0, 56(%rcx)
	movq	$0, 48(%rcx)
	movq	$0, 40(%rcx)
	movq	$0, 32(%rcx)
	movq	$0, 24(%rcx)
	movq	$0, 16(%rcx)
	movq	$0, 8(%rcx)
	movq	$0, (%rcx)
	movq	first_deleted_edge(%rip), %rdi
	testq	%rsi, %rsi
	movq	%rdi, 8(%rcx)
	movq	%rcx, first_deleted_edge(%rip)
	movq	%rsi, %rcx
	jne	.LBB1_3
# BB#4:                                 # %while.cond.while.end_crit_edge
                                        #   in Loop: Header=BB1_2 Depth=1
	movl	n_basic_blocks(%rip), %r8d
.LBB1_5:                                # %while.end
                                        #   in Loop: Header=BB1_2 Depth=1
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$32, %rcx
	movq	%r8, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdi
	movq	$0, 40(%rdx)
	movq	$0, 32(%rdx)
	jl	.LBB1_2
.LBB1_6:                                # %for.end
	movq	entry_exit_blocks+40(%rip), %rax
	testq	%rax, %rax
	je	.LBB1_8
	.align	16, 0x90
.LBB1_7:                                # %while.body6
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rax), %rcx
	movl	n_edges(%rip), %edx
	decq	%rdx
	movl	%edx, n_edges(%rip)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	first_deleted_edge(%rip), %rdx
	testq	%rcx, %rcx
	movq	%rdx, 8(%rax)
	movq	%rax, first_deleted_edge(%rip)
	movq	%rcx, %rax
	jne	.LBB1_7
.LBB1_8:                                # %while.end9
	movl	n_edges(%rip), %eax
	movq	$0, entry_exit_blocks+144(%rip)
	movq	$0, entry_exit_blocks+40(%rip)
	testq	%rax, %rax
	jne	.LBB1_10
# BB#9:                                 # %if.end
	popq	%rax
	retq
.LBB1_10:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.clear_edges, %rcx
	movq	$198, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp5:
	.size	clear_edges, .Ltmp5-clear_edges
	.cfi_endproc

	.globl	alloc_block
	.align	16, 0x90
	.type	alloc_block,@function
alloc_block:                            # @alloc_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp7:
	.cfi_def_cfa_offset 16
	movq	first_deleted_block(%rip), %rax
	testq	%rax, %rax
	je	.LBB2_2
# BB#1:                                 # %if.then
	movq	40(%rax), %rcx
	movq	%rcx, first_deleted_block(%rip)
	movq	$0, 40(%rax)
	popq	%rdx
	retq
.LBB2_2:                                # %if.else
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$111, %rax
	jg	.LBB2_4
# BB#3:                                 # %if.then3
	movabsq	$flow_obstack, %rdi
	movq	$112, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rcx
.LBB2_4:                                # %if.end
	addq	$112, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB2_6
# BB#5:                                 # %if.then10
	movzbl	flow_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, flow_obstack+80(%rip)
.LBB2_6:                                # %if.end11
	movslq	flow_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %r8
	movq	flow_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB2_8
# BB#7:                                 # %if.then31
	movq	%r8, flow_obstack+24(%rip)
	movq	%r8, %rdx
.LBB2_8:                                # %if.end34
	movq	%rdx, flow_obstack+16(%rip)
	movq	$0, 104(%rax)
	movq	$0, 96(%rax)
	movq	$0, 88(%rax)
	movq	$0, 80(%rax)
	movq	$0, 72(%rax)
	movq	$0, 64(%rax)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	popq	%rdx
	retq
.Ltmp8:
	.size	alloc_block, .Ltmp8-alloc_block
	.cfi_endproc

	.globl	expunge_block_nocompact
	.align	16, 0x90
	.type	expunge_block_nocompact,@function
expunge_block_nocompact:                # @expunge_block_nocompact
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, 104(%rdi)
	movq	$0, 96(%rdi)
	movq	$0, 88(%rdi)
	movq	$0, 80(%rdi)
	movq	$0, 72(%rdi)
	movq	$0, 64(%rdi)
	movq	$0, 56(%rdi)
	movq	$0, 48(%rdi)
	movq	$0, 40(%rdi)
	movq	$0, 32(%rdi)
	movq	$0, 24(%rdi)
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movl	%eax, 88(%rdi)
	movq	first_deleted_block(%rip), %rax
	movq	%rax, 40(%rdi)
	movq	%rdi, first_deleted_block(%rip)
	retq
.Ltmp9:
	.size	expunge_block_nocompact, .Ltmp9-expunge_block_nocompact
	.cfi_endproc

	.globl	expunge_block
	.align	16, 0x90
	.type	expunge_block,@function
expunge_block:                          # @expunge_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 16
.Ltmp13:
	.cfi_offset %rbx, -16
	movl	88(%rdi), %r10d
	movslq	n_basic_blocks(%rip), %r8
	leaq	1(%r10), %rdx
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%r8, %rsi
	jge	.LBB4_4
# BB#1:                                 # %for.body.lr.ph
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %r8
	.align	16, 0x90
.LBB4_2:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rsi,8), %r11
	movq	$32, %rcx
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r11, 32(%rax,%rbx,8)
	movl	%r10d, 88(%r11)
	incq	%rdx
	movq	%rdx, %rax
	andq	%r9, %rax
	cmpq	%r8, %rax
	movq	%rsi, %r10
	leaq	1(%rsi), %rsi
	jne	.LBB4_2
# BB#3:                                 # %for.cond.for.end_crit_edge
	movl	n_basic_blocks(%rip), %r8d
.LBB4_4:                                # %for.end
	decq	%r8
	movl	%r8d, n_basic_blocks(%rip)
	movq	basic_block_info(%rip), %rax
	decq	(%rax)
	movq	$0, 104(%rdi)
	movq	$0, 96(%rdi)
	movq	$0, 88(%rdi)
	movq	$0, 80(%rdi)
	movq	$0, 72(%rdi)
	movq	$0, 64(%rdi)
	movq	$0, 56(%rdi)
	movq	$0, 48(%rdi)
	movq	$0, 40(%rdi)
	movq	$0, 32(%rdi)
	movq	$0, 24(%rdi)
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	movl	%eax, 88(%rdi)
	movq	first_deleted_block(%rip), %rax
	movq	%rax, 40(%rdi)
	movq	%rdi, first_deleted_block(%rip)
	popq	%rbx
	retq
.Ltmp14:
	.size	expunge_block, .Ltmp14-expunge_block
	.cfi_endproc

	.globl	cached_make_edge
	.align	16, 0x90
	.type	cached_make_edge,@function
cached_make_edge:                       # @cached_make_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp23:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp24:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 48
.Ltmp26:
	.cfi_offset %rbx, -48
.Ltmp27:
	.cfi_offset %r12, -40
.Ltmp28:
	.cfi_offset %r13, -32
.Ltmp29:
	.cfi_offset %r14, -24
.Ltmp30:
	.cfi_offset %r15, -16
	movq	%rcx, %r12
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%r14, %r14
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %r13
	je	.LBB5_2
# BB#1:                                 # %entry
	movq	%rcx, %r13
.LBB5_2:                                # %entry
	movabsq	$entry_exit_blocks, %rdx
	cmpq	%rdx, %r15
	movq	%rax, %rdx
	je	.LBB5_4
# BB#3:                                 # %entry
	movq	%rcx, %rdx
.LBB5_4:                                # %entry
	andq	%rdx, %r13
	movabsq	$entry_exit_blocks+112, %rdx
	cmpq	%rdx, %rbx
	je	.LBB5_6
# BB#5:                                 # %entry
	movq	%rcx, %rax
.LBB5_6:                                # %entry
	andq	%rax, %r13
	je	.LBB5_9
# BB#7:                                 # %sw.default
	movslq	88(%r15), %rax
	movl	88(%rbx), %ecx
	movq	(%r14,%rax,8), %rax
	movq	%rcx, %rdx
	shrq	$6, %rdx
	movq	16(%rax,%rdx,8), %rax
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	%rdx, %rax
	je	.LBB5_11
# BB#8:                                 # %if.end
	xorq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r12
	je	.LBB5_25
.LBB5_9:                                # %sw.bb
	movq	40(%r15), %rax
	jmp	.LBB5_10
	.align	16, 0x90
.LBB5_13:                               # %for.cond
                                        #   in Loop: Header=BB5_10 Depth=1
	movq	8(%rax), %rax
.LBB5_10:                               # %sw.bb
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	je	.LBB5_11
# BB#14:                                # %for.body
                                        #   in Loop: Header=BB5_10 Depth=1
	movq	24(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB5_13
# BB#15:                                # %if.then12
	movl	48(%rax), %ecx
	orq	%r12, %rcx
	movl	%ecx, 48(%rax)
	xorq	%rax, %rax
	jmp	.LBB5_25
.LBB5_11:                               # %sw.epilog
	movq	first_deleted_edge(%rip), %rax
	testq	%rax, %rax
	je	.LBB5_16
# BB#12:                                # %if.then16
	movq	8(%rax), %rcx
	movq	%rcx, first_deleted_edge(%rip)
	jmp	.LBB5_23
.LBB5_16:                               # %if.else
	movq	flow_obstack+32(%rip), %rax
	movq	flow_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$63, %rax
	jg	.LBB5_18
# BB#17:                                # %if.then20
	movabsq	$flow_obstack, %rdi
	movq	$64, %rsi
	callq	_obstack_newchunk
	movq	flow_obstack+24(%rip), %rcx
.LBB5_18:                               # %if.end21
	addq	$64, %rcx
	movq	%rcx, flow_obstack+24(%rip)
	movq	flow_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB5_20
# BB#19:                                # %if.then28
	movzbl	flow_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, flow_obstack+80(%rip)
.LBB5_20:                               # %if.end29
	movslq	flow_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, flow_obstack+24(%rip)
	movq	flow_obstack+32(%rip), %r8
	movq	flow_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB5_22
# BB#21:                                # %if.then50
	movq	%r8, flow_obstack+24(%rip)
	movq	%r8, %rdx
.LBB5_22:                               # %if.end53
	movq	%rdx, flow_obstack+16(%rip)
	movq	$0, 56(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	$0, 16(%rax)
	movq	$0, 8(%rax)
	movq	$0, (%rax)
.LBB5_23:                               # %if.end57
	movl	n_edges(%rip), %ecx
	incq	%rcx
	movl	%ecx, n_edges(%rip)
	movq	40(%r15), %rcx
	movq	%rcx, 8(%rax)
	movq	32(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	%r15, 16(%rax)
	movq	%rbx, 24(%rax)
	movl	%r12d, 48(%rax)
	movq	%rax, 40(%r15)
	movq	%rax, 32(%rbx)
	testq	$1, %r13
	je	.LBB5_25
# BB#24:                                # %if.then66
	movslq	88(%r15), %rdx
	movl	88(%rbx), %ecx
	movq	(%r14,%rdx,8), %rdx
	movq	$1, %rsi
	shlq	%cl, %rsi
	shrq	$6, %rcx
	orq	%rsi, 16(%rdx,%rcx,8)
.LBB5_25:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp31:
	.size	cached_make_edge, .Ltmp31-cached_make_edge
	.cfi_endproc

	.globl	make_edge
	.align	16, 0x90
	.type	make_edge,@function
make_edge:                              # @make_edge
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	movq	%rdx, %rcx
	movq	%rsi, %rdx
	movq	%rax, %rsi
	jmp	cached_make_edge  # TAILCALL
.Ltmp32:
	.size	make_edge, .Ltmp32-make_edge
	.cfi_endproc

	.globl	make_single_succ_edge
	.align	16, 0x90
	.type	make_single_succ_edge,@function
make_single_succ_edge:                  # @make_single_succ_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp35:
	.cfi_def_cfa_offset 16
.Ltmp36:
	.cfi_offset %rbx, -16
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	cached_make_edge
	movq	$10000, %rcx            # imm = 0x2710
	movl	%ecx, 52(%rax)
	movq	96(%rbx), %rcx
	movq	%rcx, 56(%rax)
	popq	%rbx
	retq
.Ltmp37:
	.size	make_single_succ_edge, .Ltmp37-make_single_succ_edge
	.cfi_endproc

	.globl	remove_edge
	.align	16, 0x90
	.type	remove_edge,@function
remove_edge:                            # @remove_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp39:
	.cfi_def_cfa_offset 16
	movq	16(%rdi), %rax
	movq	24(%rdi), %r8
	addq	$40, %rax
	xorq	%rsi, %rsi
	movq	%rax, %rcx
	.align	16, 0x90
.LBB8_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movq	(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB8_3
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB8_1 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	%rdi, %rsi
	jne	.LBB8_1
.LBB8_3:                                # %for.end
	testq	%rsi, %rsi
	je	.LBB8_4
# BB#6:                                 # %if.end
	movq	8(%rdi), %rcx
	testq	%rdx, %rdx
	je	.LBB8_8
# BB#7:                                 # %if.then7
	movq	%rcx, 8(%rdx)
	jmp	.LBB8_9
.LBB8_8:                                # %if.else
	movq	%rcx, (%rax)
.LBB8_9:                                # %if.end12
	addq	$32, %r8
	xorq	%rax, %rax
	movq	%r8, %rdx
	.align	16, 0x90
.LBB8_10:                               # %for.cond13
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdx), %rdx
	movq	%rax, %rcx
	testq	%rdx, %rdx
	je	.LBB8_12
# BB#11:                                # %for.cond13
                                        #   in Loop: Header=BB8_10 Depth=1
	cmpq	%rdi, %rdx
	movq	%rdx, %rax
	jne	.LBB8_10
.LBB8_12:                               # %for.end20
	testq	%rdx, %rdx
	je	.LBB8_13
# BB#14:                                # %if.end23
	movq	(%rdi), %rax
	testq	%rcx, %rcx
	je	.LBB8_16
# BB#15:                                # %if.then25
	movq	%rax, (%rcx)
	jmp	.LBB8_17
.LBB8_16:                               # %if.else28
	movq	%rax, (%r8)
.LBB8_17:                               # %if.end31
	movl	n_edges(%rip), %eax
	decq	%rax
	movl	%eax, n_edges(%rip)
	movq	$0, 56(%rdi)
	movq	$0, 48(%rdi)
	movq	$0, 40(%rdi)
	movq	$0, 32(%rdi)
	movq	$0, 24(%rdi)
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movq	first_deleted_edge(%rip), %rax
	movq	%rax, 8(%rdi)
	movq	%rdi, first_deleted_edge(%rip)
	popq	%rax
	retq
.LBB8_4:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_edge, %rcx
	movq	$364, %rsi              # imm = 0x16C
	jmp	.LBB8_5
.LBB8_13:                               # %if.then22
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_edge, %rcx
	movq	$374, %rsi              # imm = 0x176
.LBB8_5:                                # %if.then
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp40:
	.size	remove_edge, .Ltmp40-remove_edge
	.cfi_endproc

	.globl	redirect_edge_succ
	.align	16, 0x90
	.type	redirect_edge_succ,@function
redirect_edge_succ:                     # @redirect_edge_succ
	.cfi_startproc
# BB#0:                                 # %entry
	movq	24(%rdi), %rax
	addq	$32, %rax
	.align	16, 0x90
.LBB9_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB9_1
# BB#2:                                 # %for.end
	movq	(%rax), %rax
	movq	%rax, (%rcx)
	movq	32(%rsi), %rax
	movq	%rax, (%rdi)
	movq	%rdi, 32(%rsi)
	movq	%rsi, 24(%rdi)
	retq
.Ltmp41:
	.size	redirect_edge_succ, .Ltmp41-redirect_edge_succ
	.cfi_endproc

	.globl	redirect_edge_succ_nodup
	.align	16, 0x90
	.type	redirect_edge_succ_nodup,@function
redirect_edge_succ_nodup:               # @redirect_edge_succ_nodup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp43:
	.cfi_def_cfa_offset 16
	movq	16(%rdi), %r9
	movq	40(%r9), %rax
	testq	%rax, %rax
	je	.LBB10_23
# BB#1:
	addq	$40, %r9
	.align	16, 0x90
.LBB10_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	cmpq	%rsi, %rcx
	jne	.LBB10_2
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB10_3 Depth=1
	cmpq	%rdi, %rax
	jne	.LBB10_5
.LBB10_2:                               # %for.cond
                                        #   in Loop: Header=BB10_3 Depth=1
	movq	8(%rax), %rax
	testq	%rax, %rax
	jne	.LBB10_3
.LBB10_23:                              # %if.else
	movq	24(%rdi), %rax
	addq	$32, %rax
	.align	16, 0x90
.LBB10_24:                              # %for.cond.i27
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	cmpq	%rdi, %rax
	jne	.LBB10_24
# BB#25:                                # %redirect_edge_succ.exit
	movq	(%rax), %rax
	movq	%rax, (%rcx)
	movq	32(%rsi), %rax
	movq	%rax, (%rdi)
	movq	%rdi, 32(%rsi)
	movq	%rsi, 24(%rdi)
	movq	%rdi, %rax
	popq	%rdx
	retq
.LBB10_5:                               # %if.then3
	movl	48(%rdi), %ecx
	movl	48(%rax), %edx
	orq	%rcx, %rdx
	movl	%edx, 48(%rax)
	movl	52(%rdi), %ecx
	movl	52(%rax), %edx
	addq	%rcx, %rdx
	movl	%edx, 52(%rax)
	movq	56(%rdi), %rcx
	addq	%rcx, 56(%rax)
	movq	24(%rdi), %r8
	xorq	%rsi, %rsi
	movq	%r9, %rcx
	.align	16, 0x90
.LBB10_6:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdx
	movq	(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB10_8
# BB#7:                                 # %for.cond.i
                                        #   in Loop: Header=BB10_6 Depth=1
	leaq	8(%rsi), %rcx
	cmpq	%rdi, %rsi
	jne	.LBB10_6
.LBB10_8:                               # %for.end.i
	testq	%rsi, %rsi
	je	.LBB10_9
# BB#11:                                # %if.end.i
	movq	8(%rdi), %rcx
	testq	%rdx, %rdx
	je	.LBB10_13
# BB#12:                                # %if.then7.i
	movq	%rcx, 8(%rdx)
	jmp	.LBB10_14
.LBB10_13:                              # %if.else.i
	movq	%rcx, (%r9)
.LBB10_14:                              # %if.end12.i
	addq	$32, %r8
	xorq	%rdx, %rdx
	movq	%r8, %rsi
	.align	16, 0x90
.LBB10_15:                              # %for.cond13.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rsi), %rsi
	movq	%rdx, %rcx
	testq	%rsi, %rsi
	je	.LBB10_17
# BB#16:                                # %for.cond13.i
                                        #   in Loop: Header=BB10_15 Depth=1
	cmpq	%rdi, %rsi
	movq	%rsi, %rdx
	jne	.LBB10_15
.LBB10_17:                              # %for.end20.i
	testq	%rsi, %rsi
	je	.LBB10_18
# BB#19:                                # %if.end23.i
	movq	(%rdi), %rdx
	testq	%rcx, %rcx
	je	.LBB10_21
# BB#20:                                # %if.then25.i
	movq	%rdx, (%rcx)
	jmp	.LBB10_22
.LBB10_21:                              # %if.else28.i
	movq	%rdx, (%r8)
.LBB10_22:                              # %remove_edge.exit
	movl	n_edges(%rip), %ecx
	decq	%rcx
	movl	%ecx, n_edges(%rip)
	movq	$0, 56(%rdi)
	movq	$0, 48(%rdi)
	movq	$0, 40(%rdi)
	movq	$0, 32(%rdi)
	movq	$0, 24(%rdi)
	movq	$0, 16(%rdi)
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movq	first_deleted_edge(%rip), %rcx
	movq	%rcx, 8(%rdi)
	movq	%rdi, first_deleted_edge(%rip)
	popq	%rdx
	retq
.LBB10_9:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_edge, %rcx
	movq	$364, %rsi              # imm = 0x16C
	jmp	.LBB10_10
.LBB10_18:                              # %if.then22.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.remove_edge, %rcx
	movq	$374, %rsi              # imm = 0x176
.LBB10_10:                              # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp44:
	.size	redirect_edge_succ_nodup, .Ltmp44-redirect_edge_succ_nodup
	.cfi_endproc

	.globl	redirect_edge_pred
	.align	16, 0x90
	.type	redirect_edge_pred,@function
redirect_edge_pred:                     # @redirect_edge_pred
	.cfi_startproc
# BB#0:                                 # %entry
	movq	16(%rdi), %rax
	addq	$40, %rax
	.align	16, 0x90
.LBB11_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	(%rcx), %rax
	cmpq	%rdi, %rax
	leaq	8(%rax), %rax
	jne	.LBB11_1
# BB#2:                                 # %for.end
	movq	(%rax), %rax
	movq	%rax, (%rcx)
	movq	40(%rsi), %rax
	movq	%rax, 8(%rdi)
	movq	%rdi, 40(%rsi)
	movq	%rsi, 16(%rdi)
	retq
.Ltmp45:
	.size	redirect_edge_pred, .Ltmp45-redirect_edge_pred
	.cfi_endproc

	.globl	dump_flow_info
	.align	16, 0x90
	.type	dump_flow_info,@function
dump_flow_info:                         # @dump_flow_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp52:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp53:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp54:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp55:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp56:
	.cfi_def_cfa_offset 48
.Ltmp57:
	.cfi_offset %rbx, -48
.Ltmp58:
	.cfi_offset %r12, -40
.Ltmp59:
	.cfi_offset %r13, -32
.Ltmp60:
	.cfi_offset %r14, -24
.Ltmp61:
	.cfi_offset %r15, -16
	movl	max_regno(%rip), %edx
	movq	%rdi, %r14
	movabsq	$.L.str26, %rsi
	xorq	%rax, %rax
                                        # kill: RDI<def> R14<kill>
	callq	fprintf
	movslq	max_regno(%rip), %rax
	cmpq	$54, %rax
	jl	.LBB12_31
# BB#1:
	movq	$53, %r15
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	reg_n_info(%rip), %rcx
	movq	32(%rcx,%r15,8), %rdx
	movl	16(%rdx), %ecx
	testq	%rcx, %rcx
	je	.LBB12_30
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	28(%rdx), %r8d
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str27, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movslq	36(%rax), %rdx
	testq	%rdx, %rdx
	js	.LBB12_5
# BB#4:                                 # %if.then16
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str28, %rsi
	callq	fprintf
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
.LBB12_5:                               # %if.end
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	12(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_9
# BB#6:                                 # %if.then28
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$1, %rdx
	movabsq	$.L.str30, %rcx
	je	.LBB12_8
# BB#7:                                 # %if.then28
                                        #   in Loop: Header=BB12_2 Depth=1
	movabsq	$.L.str31, %rcx
.LBB12_8:                               # %if.then28
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str29, %rsi
	callq	fprintf
.LBB12_9:                               # %if.end41
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rax
	movzbl	3(%rax), %eax
	testq	$8, %rax
	je	.LBB12_11
# BB#10:                                # %if.then45
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$10, %rsi
	movq	$1, %rdx
	movabsq	$.L.str32, %rdi
	movq	%r14, %rcx
	callq	fwrite
.LBB12_11:                              # %if.end47
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
	movl	24(%rax), %edx
	cmpq	$1, %rdx
	je	.LBB12_13
# BB#12:                                # %if.then53
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str33, %rsi
	callq	fprintf
	movq	reg_n_info(%rip), %rax
	movq	32(%rax,%r15,8), %rax
.LBB12_13:                              # %if.end60
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	32(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB12_17
# BB#14:                                # %if.end60
                                        #   in Loop: Header=BB12_2 Depth=1
	cmpq	$1, %rdx
	jne	.LBB12_16
# BB#15:                                # %if.then66
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$16, %rsi
	movq	$1, %rdx
	movabsq	$.L.str34, %rdi
	movq	%r14, %rcx
	callq	fwrite
	jmp	.LBB12_17
.LBB12_16:                              # %if.then74
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str35, %rsi
	callq	fprintf
.LBB12_17:                              # %if.end82
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rax
	movzbl	2(%rax), %eax
	movl	target_flags(%rip), %esi
	movzbl	mode_size(%rax), %edx
	movq	$23, %rcx
	shrq	%cl, %rsi
	andq	$4, %rsi
	addq	$4, %rsi
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rax, %rsi
	cmpq	%rsi, %rdx
	je	.LBB12_19
# BB#18:                                # %if.then96
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str36, %rsi
	callq	fprintf
.LBB12_19:                              # %if.end108
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r15, %rdi
	callq	reg_preferred_class
	movq	%rax, %r13
	movq	%r13, %r12
	andq	%rbx, %r12
	movq	%r15, %rdi
	callq	reg_alternate_class
	cmpq	$12, %r12
	jne	.LBB12_21
# BB#20:                                # %if.end108
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$24, %rcx
	je	.LBB12_27
.LBB12_21:                              # %if.then115
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$24, %rcx
	je	.LBB12_23
# BB#22:                                # %if.then115
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	%r13, %rcx
	andq	%rbx, %rcx
	cmpq	$24, %rcx
	je	.LBB12_23
# BB#24:                                # %if.else125
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	dump_flow_info.reg_class_names(,%r13,8), %rdx
	testq	%rbx, %rax
	jne	.LBB12_26
# BB#25:                                # %if.then128
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str38, %rsi
	callq	fprintf
	jmp	.LBB12_27
.LBB12_23:                              # %if.then121
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	dump_flow_info.reg_class_names(,%r13,8), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str37, %rsi
	callq	fprintf
	jmp	.LBB12_27
.LBB12_26:                              # %if.else132
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	dump_flow_info.reg_class_names(,%rax,8), %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str39, %rsi
	callq	fprintf
	.align	16, 0x90
.LBB12_27:                              # %if.end140
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	cfun(%rip), %rax
	movq	24(%rax), %rax
	movq	80(%rax), %rax
	movq	(%rax,%r15,8), %rax
	movslq	(%rax), %rax
	testq	%rax, %rax
	jns	.LBB12_29
# BB#28:                                # %if.then148
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$9, %rsi
	movq	$1, %rdx
	movabsq	$.L.str40, %rdi
	movq	%r14, %rcx
	callq	fwrite
.LBB12_29:                              # %if.end150
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	$2, %rsi
	movq	$1, %rdx
	movabsq	$.L.str41, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movl	max_regno(%rip), %eax
.LBB12_30:                              # %for.inc
                                        #   in Loop: Header=BB12_2 Depth=1
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB12_2
.LBB12_31:                              # %for.end
	movl	n_edges(%rip), %ecx
	movl	n_basic_blocks(%rip), %edx
	movabsq	$.L.str42, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rdx
	jle	.LBB12_39
	.align	16, 0x90
.LBB12_32:                              # %for.body157
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_34 Depth 2
                                        #     Child Loop BB12_37 Depth 2
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rdx,8), %r15
	movq	(%r15), %rax
	movq	8(%r15), %rcx
	movl	8(%rcx), %r8d
	movl	8(%rax), %ecx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str43, %rsi
	movq	%rdx, %r12
	callq	fprintf
	movl	92(%r15), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str44, %rsi
	callq	fprintf
	movq	96(%r15), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str45, %rsi
	callq	fprintf
	movl	104(%r15), %edx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str46, %rsi
	callq	fprintf
	movq	$14, %rsi
	movq	$1, %rdx
	movabsq	$.L.str47, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	32(%r15), %r13
	jmp	.LBB12_34
	.align	16, 0x90
.LBB12_33:                              # %for.body173
                                        #   in Loop: Header=BB12_34 Depth=2
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	dump_edge_info
	movq	(%r13), %r13
.LBB12_34:                              # %for.body173
                                        #   Parent Loop BB12_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r13, %r13
	jne	.LBB12_33
# BB#35:                                # %for.end175
                                        #   in Loop: Header=BB12_32 Depth=1
	movq	$13, %rsi
	movq	$1, %rdx
	movabsq	$.L.str48, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	40(%r15), %rbx
	jmp	.LBB12_37
	.align	16, 0x90
.LBB12_36:                              # %for.body179
                                        #   in Loop: Header=BB12_37 Depth=2
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	dump_edge_info
	movq	8(%rbx), %rbx
.LBB12_37:                              # %for.body179
                                        #   Parent Loop BB12_32 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	jne	.LBB12_36
# BB#38:                                # %for.end181
                                        #   in Loop: Header=BB12_32 Depth=1
	movq	$25, %rsi
	movq	$1, %rdx
	movabsq	$.L.str49, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	64(%r15), %rdi
	movq	%r14, %rsi
	callq	dump_regset
	movq	$23, %rsi
	movq	$1, %rdx
	movabsq	$.L.str50, %rdi
	movq	%r14, %rcx
	callq	fwrite
	movq	72(%r15), %rdi
	movq	%r14, %rsi
	callq	dump_regset
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	movq	%r12, %rdx
	incq	%rdx
	movslq	n_basic_blocks(%rip), %rax
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB12_32
.LBB12_39:                              # %for.end187
	movq	$10, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp62:
	.size	dump_flow_info, .Ltmp62-dump_flow_info
	.cfi_endproc

	.globl	dump_edge_info
	.align	16, 0x90
	.type	dump_edge_info,@function
dump_edge_info:                         # @dump_edge_info
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp69:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp70:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp71:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 48
.Ltmp74:
	.cfi_offset %rbx, -48
.Ltmp75:
	.cfi_offset %r12, -40
.Ltmp76:
	.cfi_offset %r13, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rdx
	je	.LBB13_1
# BB#2:                                 # %entry
	leaq	24(%r14), %rax
	jmp	.LBB13_3
.LBB13_1:
	leaq	16(%r14), %rax
.LBB13_3:                               # %entry
	movq	(%rax), %rax
	movabsq	$entry_exit_blocks, %rcx
	cmpq	%rcx, %rax
	je	.LBB13_4
# BB#6:                                 # %if.else
	movabsq	$entry_exit_blocks+112, %rcx
	cmpq	%rcx, %rax
	je	.LBB13_7
# BB#8:                                 # %if.else4
	movl	88(%rax), %edx
	movabsq	$.L.str53, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
	jmp	.LBB13_9
.LBB13_4:                               # %if.then
	movabsq	$.L.str51, %rdi
	movq	$6, %rsi
	jmp	.LBB13_5
.LBB13_7:                               # %if.then2
	movabsq	$.L.str52, %rdi
	movq	$5, %rsi
.LBB13_5:                               # %if.end6
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
.LBB13_9:                               # %if.end6
	movl	52(%r14), %edi
	testq	%rdi, %rdi
	je	.LBB13_11
# BB#10:                                # %if.then8
	callq	int32_to_float64
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movabsq	$4666723172467343360, %rsi # imm = 0x40C3880000000000
	movq	%rax, %rdi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str54, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rcx, %rdx
	callq	fprintf
.LBB13_11:                              # %if.end11
	cmpq	$0, 56(%r14)
	je	.LBB13_13
# BB#12:                                # %if.then13
	movabsq	$.L.str55, %rdi
	movq	$7, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	56(%r14), %rdx
	movabsq	$.L.str45, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	fprintf
.LBB13_13:                              # %if.end17
	movl	48(%r14), %r13d
	testq	%r13, %r13
	je	.LBB13_24
# BB#14:                                # %for.body.lr.ph
	movabsq	$.L.str62, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	xorq	%rbx, %rbx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB13_15:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %r14
	movq	%rbx, %rcx
	shlq	%cl, %r14
	movq	%r13, %rcx
	andq	%r14, %rcx
	testq	%r12, %rcx
	je	.LBB13_22
# BB#16:                                # %if.then25
                                        #   in Loop: Header=BB13_15 Depth=1
	notq	%r14
	testq	%r12, %rax
	je	.LBB13_18
# BB#17:                                # %if.then29
                                        #   in Loop: Header=BB13_15 Depth=1
	movq	$44, %rdi
	movq	%r15, %rsi
	callq	fputc
.LBB13_18:                              # %if.end31
                                        #   in Loop: Header=BB13_15 Depth=1
	andq	%r14, %r13
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$5, %rax
	jg	.LBB13_20
# BB#19:                                # %if.then34
                                        #   in Loop: Header=BB13_15 Depth=1
	movq	dump_edge_info.bitnames(,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	fputs
	jmp	.LBB13_21
	.align	16, 0x90
.LBB13_20:                              # %if.else36
                                        #   in Loop: Header=BB13_15 Depth=1
	xorq	%rax, %rax
	movq	%r15, %rdi
	movabsq	$.L.str63, %rsi
	movq	%rbx, %rdx
	callq	fprintf
.LBB13_21:                              # %for.inc
                                        #   in Loop: Header=BB13_15 Depth=1
	movq	$1, %rax
.LBB13_22:                              # %for.inc
                                        #   in Loop: Header=BB13_15 Depth=1
	incq	%rbx
	testq	%r12, %r13
	jne	.LBB13_15
# BB#23:                                # %for.end
	movq	$41, %rdi
	movq	%r15, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.LBB13_24:                              # %if.end41
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp79:
	.size	dump_edge_info, .Ltmp79-dump_edge_info
	.cfi_endproc

	.globl	debug_flow_info
	.align	16, 0x90
	.type	debug_flow_info,@function
debug_flow_info:                        # @debug_flow_info
	.cfi_startproc
# BB#0:                                 # %entry
	movq	stderr(%rip), %rdi
	jmp	dump_flow_info  # TAILCALL
.Ltmp80:
	.size	debug_flow_info, .Ltmp80-debug_flow_info
	.cfi_endproc

	.globl	alloc_aux_for_block
	.align	16, 0x90
	.type	alloc_aux_for_block,@function
alloc_aux_for_block:                    # @alloc_aux_for_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp84:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp85:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp86:
	.cfi_def_cfa_offset 32
.Ltmp87:
	.cfi_offset %rbx, -24
.Ltmp88:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	cmpq	$0, 80(%r14)
	jne	.LBB15_9
# BB#1:                                 # %entry
	movq	first_block_aux_obj(%rip), %rax
	testq	%rax, %rax
	je	.LBB15_9
# BB#2:                                 # %if.end
	movq	block_aux_obstack+32(%rip), %rdx
	movq	block_aux_obstack+24(%rip), %rax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jge	.LBB15_4
# BB#3:                                 # %if.then3
	movabsq	$block_aux_obstack, %rdi
	callq	_obstack_newchunk
	movq	block_aux_obstack+24(%rip), %rax
.LBB15_4:                               # %if.end4
	addq	%rbx, %rax
	movq	%rax, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB15_6
# BB#5:                                 # %if.then11
	movzbl	block_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, block_aux_obstack+80(%rip)
.LBB15_6:                               # %if.end12
	movslq	block_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+32(%rip), %r8
	movq	block_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB15_8
# BB#7:                                 # %if.then32
	movq	%r8, block_aux_obstack+24(%rip)
	movq	%r8, %rcx
.LBB15_8:                               # %if.end35
	movq	%rcx, block_aux_obstack+16(%rip)
	movq	%rdi, 80(%r14)
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB15_9:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alloc_aux_for_block, %rcx
	movq	$607, %rsi              # imm = 0x25F
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp89:
	.size	alloc_aux_for_block, .Ltmp89-alloc_aux_for_block
	.cfi_endproc

	.globl	alloc_aux_for_blocks
	.align	16, 0x90
	.type	alloc_aux_for_blocks,@function
alloc_aux_for_blocks:                   # @alloc_aux_for_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp96:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp97:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp98:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp99:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp100:
	.cfi_def_cfa_offset 48
.Ltmp101:
	.cfi_offset %rbx, -48
.Ltmp102:
	.cfi_offset %r12, -40
.Ltmp103:
	.cfi_offset %r13, -32
.Ltmp104:
	.cfi_offset %r14, -24
.Ltmp105:
	.cfi_offset %r15, -16
	movzbl	alloc_aux_for_blocks.initialized(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB16_1
# BB#14:                                # %if.else
	cmpq	$0, first_block_aux_obj(%rip)
	je	.LBB16_2
# BB#15:                                # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alloc_aux_for_blocks, %rcx
	movq	$629, %rsi              # imm = 0x275
	jmp	.LBB16_16
.LBB16_1:                               # %if.then
	movabsq	$block_aux_obstack, %rdi
	callq	gcc_obstack_init
	movq	$1, %rax
	movb	%al, alloc_aux_for_blocks.initialized(%rip)
.LBB16_2:                               # %if.end3
	movq	block_aux_obstack+32(%rip), %rax
	movq	block_aux_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB16_4
# BB#3:                                 # %if.then5
	movabsq	$block_aux_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	block_aux_obstack+24(%rip), %rcx
.LBB16_4:                               # %if.end6
	movq	block_aux_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB16_6
# BB#5:                                 # %if.then13
	movzbl	block_aux_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, block_aux_obstack+80(%rip)
.LBB16_6:                               # %if.end14
	movslq	block_aux_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+32(%rip), %rbx
	movq	block_aux_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB16_8
# BB#7:                                 # %if.then34
	movq	%rbx, block_aux_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB16_8:                               # %if.end37
	movq	%rdx, block_aux_obstack+16(%rip)
	movq	%rax, first_block_aux_obj(%rip)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	je	.LBB16_41
# BB#9:                                 # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rcx
	testq	%rcx, %rcx
	jle	.LBB16_24
# BB#10:                                # %for.body.lr.ph
	movq	$32, %rcx
	movq	%r14, %r12
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$4, %r15
	.align	16, 0x90
.LBB16_11:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%r15,8), %r13
	cmpq	$0, 80(%r13)
	jne	.LBB16_13
# BB#12:                                # %for.body
                                        #   in Loop: Header=BB16_11 Depth=1
	testq	%rax, %rax
	je	.LBB16_13
# BB#17:                                # %if.end.i
                                        #   in Loop: Header=BB16_11 Depth=1
	movq	block_aux_obstack+32(%rip), %rcx
	movq	block_aux_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	%r12, %rcx
	jge	.LBB16_19
# BB#18:                                # %if.then3.i
                                        #   in Loop: Header=BB16_11 Depth=1
	movabsq	$block_aux_obstack, %rdi
	movq	%r14, %rsi
	callq	_obstack_newchunk
	movq	block_aux_obstack+24(%rip), %rax
.LBB16_19:                              # %if.end4.i
                                        #   in Loop: Header=BB16_11 Depth=1
	addq	%r12, %rax
	movq	%rax, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB16_21
# BB#20:                                # %if.then11.i
                                        #   in Loop: Header=BB16_11 Depth=1
	movzbl	block_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, block_aux_obstack+80(%rip)
.LBB16_21:                              # %if.end12.i
                                        #   in Loop: Header=BB16_11 Depth=1
	movslq	block_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+32(%rip), %rbx
	movq	block_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB16_23
# BB#22:                                # %if.then32.i
                                        #   in Loop: Header=BB16_11 Depth=1
	movq	%rbx, block_aux_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB16_23:                              # %alloc_aux_for_block.exit
                                        #   in Loop: Header=BB16_11 Depth=1
	movq	%rcx, block_aux_obstack+16(%rip)
	movq	%rdi, 80(%r13)
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	callq	memset
	movslq	n_basic_blocks(%rip), %rdx
	leaq	-3(%r15), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	first_block_aux_obj(%rip), %rax
	sarq	%cl, %rsi
	incq	%r15
	cmpq	%rdx, %rsi
	jl	.LBB16_11
.LBB16_24:                              # %for.end
	cmpq	$0, entry_exit_blocks+80(%rip)
	jne	.LBB16_13
# BB#25:                                # %for.end
	testq	%rax, %rax
	je	.LBB16_13
# BB#26:                                # %if.end.i82
	movq	block_aux_obstack+32(%rip), %rdx
	movq	block_aux_obstack+24(%rip), %rax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	%r15, %rdx
	jge	.LBB16_28
# BB#27:                                # %if.then3.i84
	movabsq	$block_aux_obstack, %rdi
	movq	%r14, %rsi
	callq	_obstack_newchunk
	movq	block_aux_obstack+24(%rip), %rax
.LBB16_28:                              # %if.end4.i87
	addq	%r15, %rax
	movq	%rax, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB16_30
# BB#29:                                # %if.then11.i90
	movzbl	block_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, block_aux_obstack+80(%rip)
.LBB16_30:                              # %if.end12.i104
	movslq	block_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+32(%rip), %rbx
	movq	block_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB16_32
# BB#31:                                # %if.then32.i105
	movq	%rbx, block_aux_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB16_32:                              # %alloc_aux_for_block.exit106
	movq	%rcx, block_aux_obstack+16(%rip)
	movq	%rdi, entry_exit_blocks+80(%rip)
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	memset
	cmpq	$0, entry_exit_blocks+192(%rip)
	jne	.LBB16_13
# BB#33:                                # %alloc_aux_for_block.exit106
	movq	first_block_aux_obj(%rip), %rax
	testq	%rax, %rax
	je	.LBB16_13
# BB#34:                                # %if.end.i116
	movq	block_aux_obstack+32(%rip), %rcx
	movq	block_aux_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	%r15, %rcx
	jge	.LBB16_36
# BB#35:                                # %if.then3.i118
	movabsq	$block_aux_obstack, %rdi
	movq	%r14, %rsi
	callq	_obstack_newchunk
	movq	block_aux_obstack+24(%rip), %rax
.LBB16_36:                              # %if.end4.i121
	addq	%r15, %rax
	movq	%rax, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB16_38
# BB#37:                                # %if.then11.i124
	movzbl	block_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, block_aux_obstack+80(%rip)
.LBB16_38:                              # %if.end12.i138
	movslq	block_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, block_aux_obstack+24(%rip)
	movq	block_aux_obstack+32(%rip), %rbx
	movq	block_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB16_40
# BB#39:                                # %if.then32.i139
	movq	%rbx, block_aux_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB16_40:                              # %alloc_aux_for_block.exit140
	movq	%rcx, block_aux_obstack+16(%rip)
	movq	%rdi, entry_exit_blocks+192(%rip)
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	memset
.LBB16_41:                              # %if.end46
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB16_13:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alloc_aux_for_block, %rcx
	movq	$607, %rsi              # imm = 0x25F
.LBB16_16:                              # %if.then2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp106:
	.size	alloc_aux_for_blocks, .Ltmp106-alloc_aux_for_blocks
	.cfi_endproc

	.globl	clear_aux_for_blocks
	.align	16, 0x90
	.type	clear_aux_for_blocks,@function
clear_aux_for_blocks:                   # @clear_aux_for_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	jle	.LBB17_3
# BB#1:
	movq	$4, %rdx
	.align	16, 0x90
.LBB17_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	(%rcx,%rdx,8), %rcx
	movq	$0, 80(%rcx)
	leaq	-3(%rdx), %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	incq	%rdx
	cmpq	%rax, %rsi
	jl	.LBB17_2
.LBB17_3:                               # %for.end
	movq	$0, entry_exit_blocks+80(%rip)
	movq	$0, entry_exit_blocks+192(%rip)
	retq
.Ltmp107:
	.size	clear_aux_for_blocks, .Ltmp107-clear_aux_for_blocks
	.cfi_endproc

	.globl	free_aux_for_blocks
	.align	16, 0x90
	.type	free_aux_for_blocks,@function
free_aux_for_blocks:                    # @free_aux_for_blocks
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp109:
	.cfi_def_cfa_offset 16
	movq	first_block_aux_obj(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB18_9
# BB#1:                                 # %if.end
	movq	block_aux_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB18_4
# BB#2:                                 # %if.end
	movq	block_aux_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB18_4
# BB#3:                                 # %if.then2
	movq	%rsi, block_aux_obstack+16(%rip)
	movq	%rsi, block_aux_obstack+24(%rip)
	jmp	.LBB18_5
.LBB18_4:                               # %if.else
	movabsq	$block_aux_obstack, %rdi
	callq	obstack_free
.LBB18_5:                               # %if.end3
	movslq	n_basic_blocks(%rip), %rax
	movq	$0, first_block_aux_obj(%rip)
	testq	%rax, %rax
	jle	.LBB18_8
# BB#6:
	movq	$4, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	.align	16, 0x90
.LBB18_7:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rsi
	movq	(%rsi,%rcx,8), %rsi
	movq	$0, 80(%rsi)
	leaq	-3(%rcx), %rsi
	andq	%rdx, %rsi
	incq	%rcx
	cmpq	%rax, %rsi
	jne	.LBB18_7
.LBB18_8:                               # %clear_aux_for_blocks.exit
	movq	$0, entry_exit_blocks+80(%rip)
	movq	$0, entry_exit_blocks+192(%rip)
	popq	%rax
	retq
.LBB18_9:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.free_aux_for_blocks, %rcx
	movq	$664, %rsi              # imm = 0x298
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp110:
	.size	free_aux_for_blocks, .Ltmp110-free_aux_for_blocks
	.cfi_endproc

	.globl	alloc_aux_for_edge
	.align	16, 0x90
	.type	alloc_aux_for_edge,@function
alloc_aux_for_edge:                     # @alloc_aux_for_edge
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp116:
	.cfi_def_cfa_offset 32
.Ltmp117:
	.cfi_offset %rbx, -24
.Ltmp118:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	cmpq	$0, 40(%r14)
	jne	.LBB19_9
# BB#1:                                 # %entry
	movq	first_edge_aux_obj(%rip), %rax
	testq	%rax, %rax
	je	.LBB19_9
# BB#2:                                 # %if.end
	movq	edge_aux_obstack+32(%rip), %rdx
	movq	edge_aux_obstack+24(%rip), %rax
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	cmpq	%rbx, %rdx
	jge	.LBB19_4
# BB#3:                                 # %if.then3
	movabsq	$edge_aux_obstack, %rdi
	callq	_obstack_newchunk
	movq	edge_aux_obstack+24(%rip), %rax
.LBB19_4:                               # %if.end4
	addq	%rbx, %rax
	movq	%rax, edge_aux_obstack+24(%rip)
	movq	edge_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB19_6
# BB#5:                                 # %if.then11
	movzbl	edge_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, edge_aux_obstack+80(%rip)
.LBB19_6:                               # %if.end12
	movslq	edge_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, edge_aux_obstack+24(%rip)
	movq	edge_aux_obstack+32(%rip), %r8
	movq	edge_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%r8, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB19_8
# BB#7:                                 # %if.then32
	movq	%r8, edge_aux_obstack+24(%rip)
	movq	%r8, %rcx
.LBB19_8:                               # %if.end35
	movq	%rcx, edge_aux_obstack+16(%rip)
	movq	%rdi, 40(%r14)
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	memset
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB19_9:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alloc_aux_for_edge, %rcx
	movq	$681, %rsi              # imm = 0x2A9
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp119:
	.size	alloc_aux_for_edge, .Ltmp119-alloc_aux_for_edge
	.cfi_endproc

	.globl	alloc_aux_for_edges
	.align	16, 0x90
	.type	alloc_aux_for_edges,@function
alloc_aux_for_edges:                    # @alloc_aux_for_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp126:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp128:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp129:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp130:
	.cfi_def_cfa_offset 48
.Ltmp131:
	.cfi_offset %rbx, -48
.Ltmp132:
	.cfi_offset %r12, -40
.Ltmp133:
	.cfi_offset %r13, -32
.Ltmp134:
	.cfi_offset %r14, -24
.Ltmp135:
	.cfi_offset %r15, -16
	movzbl	alloc_aux_for_edges.initialized(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB20_1
# BB#2:                                 # %if.else
	cmpq	$0, first_edge_aux_obj(%rip)
	je	.LBB20_3
# BB#27:                                # %if.then2
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.alloc_aux_for_edges, %rcx
	movq	$703, %rsi              # imm = 0x2BF
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB20_1:                               # %if.then
	movabsq	$edge_aux_obstack, %rdi
	callq	gcc_obstack_init
	movq	$1, %rax
	movb	%al, alloc_aux_for_edges.initialized(%rip)
.LBB20_3:                               # %if.end3
	movq	edge_aux_obstack+32(%rip), %rax
	movq	edge_aux_obstack+24(%rip), %rcx
	cmpq	%rcx, %rax
	jns	.LBB20_5
# BB#4:                                 # %if.then5
	movabsq	$edge_aux_obstack, %rdi
	xorq	%rsi, %rsi
	callq	_obstack_newchunk
	movq	edge_aux_obstack+24(%rip), %rcx
.LBB20_5:                               # %if.end6
	movq	edge_aux_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB20_7
# BB#6:                                 # %if.then13
	movzbl	edge_aux_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, edge_aux_obstack+80(%rip)
.LBB20_7:                               # %if.end14
	movslq	edge_aux_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, edge_aux_obstack+24(%rip)
	movq	edge_aux_obstack+32(%rip), %rbx
	movq	edge_aux_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB20_9
# BB#8:                                 # %if.then34
	movq	%rbx, edge_aux_obstack+24(%rip)
	movq	%rbx, %rdx
.LBB20_9:                               # %if.end37
	movq	%rdx, edge_aux_obstack+16(%rip)
	movq	%rax, first_edge_aux_obj(%rip)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB20_26
# BB#10:                                # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	js	.LBB20_26
# BB#11:                                # %for.body.lr.ph
	movq	$32, %rcx
	movq	%r14, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$-1, %r12
	.align	16, 0x90
.LBB20_12:                              # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_15 Depth 2
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movabsq	$entry_exit_blocks, %rcx
	js	.LBB20_14
# BB#13:                                # %if.then50
                                        #   in Loop: Header=BB20_12 Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%r12,8), %rcx
.LBB20_14:                              # %if.end53
                                        #   in Loop: Header=BB20_12 Depth=1
	movq	40(%rcx), %r13
	testq	%r13, %r13
	je	.LBB20_25
	.align	16, 0x90
.LBB20_15:                              # %for.body56
                                        #   Parent Loop BB20_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	$0, 40(%r13)
	jne	.LBB20_28
# BB#16:                                # %for.body56
                                        #   in Loop: Header=BB20_15 Depth=2
	movq	first_edge_aux_obj(%rip), %rax
	testq	%rax, %rax
	je	.LBB20_28
# BB#17:                                # %if.end.i
                                        #   in Loop: Header=BB20_15 Depth=2
	movq	edge_aux_obstack+32(%rip), %rcx
	movq	edge_aux_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	%r15, %rcx
	jge	.LBB20_19
# BB#18:                                # %if.then3.i
                                        #   in Loop: Header=BB20_15 Depth=2
	movabsq	$edge_aux_obstack, %rdi
	movq	%r14, %rsi
	callq	_obstack_newchunk
	movq	edge_aux_obstack+24(%rip), %rax
.LBB20_19:                              # %if.end4.i
                                        #   in Loop: Header=BB20_15 Depth=2
	addq	%r15, %rax
	movq	%rax, edge_aux_obstack+24(%rip)
	movq	edge_aux_obstack+16(%rip), %rdi
	cmpq	%rdi, %rax
	jne	.LBB20_21
# BB#20:                                # %if.then11.i
                                        #   in Loop: Header=BB20_15 Depth=2
	movzbl	edge_aux_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, edge_aux_obstack+80(%rip)
.LBB20_21:                              # %if.end12.i
                                        #   in Loop: Header=BB20_15 Depth=2
	movslq	edge_aux_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, edge_aux_obstack+24(%rip)
	movq	edge_aux_obstack+32(%rip), %rbx
	movq	edge_aux_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rbx, %rax
	subq	%rdx, %rax
	cmpq	%rax, %rsi
	jle	.LBB20_23
# BB#22:                                # %if.then32.i
                                        #   in Loop: Header=BB20_15 Depth=2
	movq	%rbx, edge_aux_obstack+24(%rip)
	movq	%rbx, %rcx
.LBB20_23:                              # %alloc_aux_for_edge.exit
                                        #   in Loop: Header=BB20_15 Depth=2
	movq	%rcx, edge_aux_obstack+16(%rip)
	movq	%rdi, 40(%r13)
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	callq	memset
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB20_15
# BB#24:                                # %for.cond54.for.inc57_crit_edge
                                        #   in Loop: Header=BB20_12 Depth=1
	movl	n_basic_blocks(%rip), %eax
.LBB20_25:                              # %for.inc57
                                        #   in Loop: Header=BB20_12 Depth=1
	incq	%r12
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rsi, %rdx
	jl	.LBB20_12
.LBB20_26:                              # %if.end59
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_28:                              # %if.then.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.alloc_aux_for_edge, %rdx
	movq	$681, %rsi              # imm = 0x2A9
	callq	fancy_abort
.Ltmp136:
	.size	alloc_aux_for_edges, .Ltmp136-alloc_aux_for_edges
	.cfi_endproc

	.globl	clear_aux_for_edges
	.align	16, 0x90
	.type	clear_aux_for_edges,@function
clear_aux_for_edges:                    # @clear_aux_for_edges
	.cfi_startproc
# BB#0:                                 # %entry
	movslq	n_basic_blocks(%rip), %rax
	testq	%rax, %rax
	js	.LBB21_8
# BB#1:
	movq	$-1, %rdx
	movabsq	$entry_exit_blocks, %rsi
	.align	16, 0x90
.LBB21_2:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB21_6 Depth 2
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	testq	%rdi, %rdi
	movq	%rsi, %rcx
	js	.LBB21_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rdx,8), %rcx
.LBB21_4:                               # %if.end
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	40(%rcx), %rcx
	jmp	.LBB21_6
	.align	16, 0x90
.LBB21_5:                               # %for.body4
                                        #   in Loop: Header=BB21_6 Depth=2
	movq	$0, 40(%rcx)
	movq	8(%rcx), %rcx
.LBB21_6:                               # %for.body4
                                        #   Parent Loop BB21_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	jne	.LBB21_5
# BB#7:                                 # %for.inc5
                                        #   in Loop: Header=BB21_2 Depth=1
	incq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdi
	jl	.LBB21_2
.LBB21_8:                               # %for.end6
	retq
.Ltmp137:
	.size	clear_aux_for_edges, .Ltmp137-clear_aux_for_edges
	.cfi_endproc

	.globl	free_aux_for_edges
	.align	16, 0x90
	.type	free_aux_for_edges,@function
free_aux_for_edges:                     # @free_aux_for_edges
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp139:
	.cfi_def_cfa_offset 16
	movq	first_edge_aux_obj(%rip), %rsi
	testq	%rsi, %rsi
	je	.LBB22_14
# BB#1:                                 # %if.end
	movq	edge_aux_obstack+8(%rip), %rax
	cmpq	%rax, %rsi
	jbe	.LBB22_4
# BB#2:                                 # %if.end
	movq	edge_aux_obstack+32(%rip), %rax
	cmpq	%rax, %rsi
	jae	.LBB22_4
# BB#3:                                 # %if.then2
	movq	%rsi, edge_aux_obstack+16(%rip)
	movq	%rsi, edge_aux_obstack+24(%rip)
	jmp	.LBB22_5
.LBB22_4:                               # %if.else
	movabsq	$edge_aux_obstack, %rdi
	callq	obstack_free
.LBB22_5:                               # %if.end3
	movslq	n_basic_blocks(%rip), %rax
	movq	$0, first_edge_aux_obj(%rip)
	testq	%rax, %rax
	js	.LBB22_13
# BB#6:
	movq	$-1, %rsi
	movabsq	$entry_exit_blocks, %r8
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	.align	16, 0x90
.LBB22_7:                               # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB22_11 Depth 2
	movq	$32, %rcx
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	%r8, %rcx
	js	.LBB22_9
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB22_7 Depth=1
	movq	basic_block_info(%rip), %rcx
	movq	32(%rcx,%rsi,8), %rcx
.LBB22_9:                               # %if.end.i
                                        #   in Loop: Header=BB22_7 Depth=1
	movq	40(%rcx), %rcx
	jmp	.LBB22_11
	.align	16, 0x90
.LBB22_10:                              # %for.body4.i
                                        #   in Loop: Header=BB22_11 Depth=2
	movq	$0, 40(%rcx)
	movq	8(%rcx), %rcx
.LBB22_11:                              # %for.body4.i
                                        #   Parent Loop BB22_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rcx, %rcx
	jne	.LBB22_10
# BB#12:                                # %for.inc5.i
                                        #   in Loop: Header=BB22_7 Depth=1
	incq	%rsi
	movq	%rsi, %rcx
	andq	%rdi, %rcx
	cmpq	%rax, %rcx
	jne	.LBB22_7
.LBB22_13:                              # %clear_aux_for_edges.exit
	popq	%rax
	retq
.LBB22_14:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.free_aux_for_edges, %rcx
	movq	$754, %rsi              # imm = 0x2F2
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp140:
	.size	free_aux_for_edges, .Ltmp140-free_aux_for_edges
	.cfi_endproc

	.type	entry_exit_blocks,@object # @entry_exit_blocks
	.data
	.globl	entry_exit_blocks
	.align	16
entry_exit_blocks:
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
	.long	4294967295              # 0xffffffff
	.long	0                       # 0x0
	.quad	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
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
	.long	4294967294              # 0xfffffffe
	.long	0                       # 0x0
	.quad	0                       # 0x0
	.long	0                       # 0x0
	.long	0                       # 0x0
	.size	entry_exit_blocks, 224

	.type	init_flow.initialized,@object # @init_flow.initialized
	.local	init_flow.initialized
	.comm	init_flow.initialized,1,1
	.type	first_deleted_edge,@object # @first_deleted_edge
	.comm	first_deleted_edge,8,8
	.type	first_deleted_block,@object # @first_deleted_block
	.local	first_deleted_block
	.comm	first_deleted_block,8,8
	.type	n_edges,@object         # @n_edges
	.comm	n_edges,4,4
	.type	flow_obstack,@object    # @flow_obstack
	.comm	flow_obstack,88,8
	.type	flow_firstobj,@object   # @flow_firstobj
	.local	flow_firstobj
	.comm	flow_firstobj,8,8
	.type	n_basic_blocks,@object  # @n_basic_blocks
	.comm	n_basic_blocks,4,4
	.type	basic_block_info,@object # @basic_block_info
	.comm	basic_block_info,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"cfg.c"
	.size	.L.str, 6

	.type	.L__FUNCTION__.clear_edges,@object # @__FUNCTION__.clear_edges
.L__FUNCTION__.clear_edges:
	.asciz	"clear_edges"
	.size	.L__FUNCTION__.clear_edges, 12

	.type	.L__FUNCTION__.remove_edge,@object # @__FUNCTION__.remove_edge
.L__FUNCTION__.remove_edge:
	.asciz	"remove_edge"
	.size	.L__FUNCTION__.remove_edge, 12

	.type	dump_flow_info.reg_class_names,@object # @dump_flow_info.reg_class_names
	.section	.rodata,"a",@progbits
	.align	16
dump_flow_info.reg_class_names:
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
	.size	dump_flow_info.reg_class_names, 200

	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"NO_REGS"
	.size	.L.str1, 8

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"AREG"
	.size	.L.str2, 5

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"DREG"
	.size	.L.str3, 5

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"CREG"
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"BREG"
	.size	.L.str5, 5

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"SIREG"
	.size	.L.str6, 6

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"DIREG"
	.size	.L.str7, 6

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"AD_REGS"
	.size	.L.str8, 8

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Q_REGS"
	.size	.L.str9, 7

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"NON_Q_REGS"
	.size	.L.str10, 11

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"INDEX_REGS"
	.size	.L.str11, 11

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"LEGACY_REGS"
	.size	.L.str12, 12

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"GENERAL_REGS"
	.size	.L.str13, 13

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"FP_TOP_REG"
	.size	.L.str14, 11

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"FP_SECOND_REG"
	.size	.L.str15, 14

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"FLOAT_REGS"
	.size	.L.str16, 11

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"SSE_REGS"
	.size	.L.str17, 9

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"MMX_REGS"
	.size	.L.str18, 9

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"FP_TOP_SSE_REGS"
	.size	.L.str19, 16

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"FP_SECOND_SSE_REGS"
	.size	.L.str20, 19

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"FLOAT_SSE_REGS"
	.size	.L.str21, 15

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"FLOAT_INT_REGS"
	.size	.L.str22, 15

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"INT_SSE_REGS"
	.size	.L.str23, 13

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"FLOAT_INT_SSE_REGS"
	.size	.L.str24, 19

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"ALL_REGS"
	.size	.L.str25, 9

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"%d registers.\n"
	.size	.L.str26, 15

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"\nRegister %d used %d times across %d insns"
	.size	.L.str27, 43

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	" in block %d"
	.size	.L.str28, 13

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"; set %d time%s"
	.size	.L.str29, 16

	.type	.L.str30,@object        # @.str30
.L.str30:
	.zero	1
	.size	.L.str30, 1

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"s"
	.size	.L.str31, 2

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"; user var"
	.size	.L.str32, 11

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"; dies in %d places"
	.size	.L.str33, 20

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"; crosses 1 call"
	.size	.L.str34, 17

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"; crosses %d calls"
	.size	.L.str35, 19

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"; %d bytes"
	.size	.L.str36, 11

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"; pref %s"
	.size	.L.str37, 10

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"; %s or none"
	.size	.L.str38, 13

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"; pref %s, else %s"
	.size	.L.str39, 19

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"; pointer"
	.size	.L.str40, 10

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	".\n"
	.size	.L.str41, 3

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"\n%d basic blocks, %d edges.\n"
	.size	.L.str42, 29

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"\nBasic block %d: first insn %d, last %d, "
	.size	.L.str43, 42

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"loop_depth %d, count "
	.size	.L.str44, 22

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"%lld"
	.size	.L.str45, 5

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	", freq %i.\n"
	.size	.L.str46, 12

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"Predecessors: "
	.size	.L.str47, 15

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"\nSuccessors: "
	.size	.L.str48, 14

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"\nRegisters live at start:"
	.size	.L.str49, 26

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"\nRegisters live at end:"
	.size	.L.str50, 24

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	" ENTRY"
	.size	.L.str51, 7

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	" EXIT"
	.size	.L.str52, 6

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	" %d"
	.size	.L.str53, 4

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	" [%.1f%%] "
	.size	.L.str54, 11

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	" count:"
	.size	.L.str55, 8

	.type	dump_edge_info.bitnames,@object # @dump_edge_info.bitnames
	.section	.rodata,"a",@progbits
	.align	16
dump_edge_info.bitnames:
	.quad	.L.str56
	.quad	.L.str57
	.quad	.L.str58
	.quad	.L.str59
	.quad	.L.str60
	.quad	.L.str61
	.size	dump_edge_info.bitnames, 48

	.type	.L.str56,@object        # @.str56
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str56:
	.asciz	"fallthru"
	.size	.L.str56, 9

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"ab"
	.size	.L.str57, 3

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"abcall"
	.size	.L.str58, 7

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"eh"
	.size	.L.str59, 3

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"fake"
	.size	.L.str60, 5

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"dfs_back"
	.size	.L.str61, 9

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	" ("
	.size	.L.str62, 3

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"%d"
	.size	.L.str63, 3

	.type	first_block_aux_obj,@object # @first_block_aux_obj
	.local	first_block_aux_obj
	.comm	first_block_aux_obj,8,8
	.type	.L__FUNCTION__.alloc_aux_for_block,@object # @__FUNCTION__.alloc_aux_for_block
.L__FUNCTION__.alloc_aux_for_block:
	.asciz	"alloc_aux_for_block"
	.size	.L__FUNCTION__.alloc_aux_for_block, 20

	.type	block_aux_obstack,@object # @block_aux_obstack
	.local	block_aux_obstack
	.comm	block_aux_obstack,88,8
	.type	alloc_aux_for_blocks.initialized,@object # @alloc_aux_for_blocks.initialized
	.local	alloc_aux_for_blocks.initialized
	.comm	alloc_aux_for_blocks.initialized,1,1
	.type	.L__FUNCTION__.alloc_aux_for_blocks,@object # @__FUNCTION__.alloc_aux_for_blocks
.L__FUNCTION__.alloc_aux_for_blocks:
	.asciz	"alloc_aux_for_blocks"
	.size	.L__FUNCTION__.alloc_aux_for_blocks, 21

	.type	.L__FUNCTION__.free_aux_for_blocks,@object # @__FUNCTION__.free_aux_for_blocks
.L__FUNCTION__.free_aux_for_blocks:
	.asciz	"free_aux_for_blocks"
	.size	.L__FUNCTION__.free_aux_for_blocks, 20

	.type	first_edge_aux_obj,@object # @first_edge_aux_obj
	.local	first_edge_aux_obj
	.comm	first_edge_aux_obj,8,8
	.type	.L__FUNCTION__.alloc_aux_for_edge,@object # @__FUNCTION__.alloc_aux_for_edge
.L__FUNCTION__.alloc_aux_for_edge:
	.asciz	"alloc_aux_for_edge"
	.size	.L__FUNCTION__.alloc_aux_for_edge, 19

	.type	edge_aux_obstack,@object # @edge_aux_obstack
	.local	edge_aux_obstack
	.comm	edge_aux_obstack,88,8
	.type	alloc_aux_for_edges.initialized,@object # @alloc_aux_for_edges.initialized
	.local	alloc_aux_for_edges.initialized
	.comm	alloc_aux_for_edges.initialized,1,1
	.type	.L__FUNCTION__.alloc_aux_for_edges,@object # @__FUNCTION__.alloc_aux_for_edges
.L__FUNCTION__.alloc_aux_for_edges:
	.asciz	"alloc_aux_for_edges"
	.size	.L__FUNCTION__.alloc_aux_for_edges, 20

	.type	.L__FUNCTION__.free_aux_for_edges,@object # @__FUNCTION__.free_aux_for_edges
.L__FUNCTION__.free_aux_for_edges:
	.asciz	"free_aux_for_edges"
	.size	.L__FUNCTION__.free_aux_for_edges, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
