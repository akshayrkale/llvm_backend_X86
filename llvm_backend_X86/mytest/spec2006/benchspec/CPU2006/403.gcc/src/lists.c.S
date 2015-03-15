	.file	"lists.c.bc"
	.text
	.globl	alloc_INSN_LIST
	.align	16, 0x90
	.type	alloc_INSN_LIST,@function
alloc_INSN_LIST:                        # @alloc_INSN_LIST
	.cfi_startproc
# BB#0:                                 # %entry
	movq	unused_insn_list(%rip), %rax
	movq	%rsi, %rcx
	movq	%rdi, %rdx
	testq	%rax, %rax
	je	.LBB0_2
# BB#1:                                 # %if.end
	movq	16(%rax), %rsi
	movq	%rsi, unused_insn_list(%rip)
	movq	%rdx, 8(%rax)
	movq	%rcx, 16(%rax)
	movl	(%rax), %ecx
	movabsq	$4278255615, %rdx       # imm = 0xFF00FFFF
	andq	%rcx, %rdx
	movl	%edx, (%rax)
	retq
.LBB0_2:                                # %if.else
	movq	$4, %rdi
	xorq	%rsi, %rsi
	jmp	gen_rtx_fmt_ue  # TAILCALL
.Ltmp0:
	.size	alloc_INSN_LIST, .Ltmp0-alloc_INSN_LIST
	.cfi_endproc

	.globl	alloc_EXPR_LIST
	.align	16, 0x90
	.type	alloc_EXPR_LIST,@function
alloc_EXPR_LIST:                        # @alloc_EXPR_LIST
	.cfi_startproc
# BB#0:                                 # %entry
	movq	unused_expr_list(%rip), %rax
	movq	%rdx, %r8
	movq	%rsi, %rdx
	movq	%rdi, %rcx
	testq	%rax, %rax
	je	.LBB1_2
# BB#1:                                 # %if.end
	movq	16(%rax), %rsi
	movq	%rsi, unused_expr_list(%rip)
	movq	%rdx, 8(%rax)
	movq	%r8, 16(%rax)
	movb	%cl, 2(%rax)
	retq
.LBB1_2:                                # %if.else
	movq	$3, %rdi
	movq	%rcx, %rsi
	movq	%r8, %rcx
	jmp	gen_rtx_fmt_ee  # TAILCALL
.Ltmp1:
	.size	alloc_EXPR_LIST, .Ltmp1-alloc_EXPR_LIST
	.cfi_endproc

	.globl	init_EXPR_INSN_LIST_cache
	.align	16, 0x90
	.type	init_EXPR_INSN_LIST_cache,@function
init_EXPR_INSN_LIST_cache:              # @init_EXPR_INSN_LIST_cache
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$unused_expr_list, %rdi
	movabsq	$zap_lists, %rcx
	movq	$1, %rsi
	movq	$1, %rdx
	jmp	ggc_add_root  # TAILCALL
.Ltmp2:
	.size	init_EXPR_INSN_LIST_cache, .Ltmp2-init_EXPR_INSN_LIST_cache
	.cfi_endproc

	.align	16, 0x90
	.type	zap_lists,@function
zap_lists:                              # @zap_lists
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, unused_expr_list(%rip)
	movq	$0, unused_insn_list(%rip)
	retq
.Ltmp3:
	.size	zap_lists, .Ltmp3-zap_lists
	.cfi_endproc

	.globl	free_EXPR_LIST_list
	.align	16, 0x90
	.type	free_EXPR_LIST_list,@function
free_EXPR_LIST_list:                    # @free_EXPR_LIST_list
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB4_3
	.align	16, 0x90
.LBB4_1:                                # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	16(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB4_1
# BB#2:                                 # %free_list.exit
	movq	unused_expr_list(%rip), %rax
	movq	%rax, 16(%rcx)
	movq	(%rdi), %rax
	movq	%rax, unused_expr_list(%rip)
	movq	$0, (%rdi)
.LBB4_3:                                # %return
	retq
.Ltmp4:
	.size	free_EXPR_LIST_list, .Ltmp4-free_EXPR_LIST_list
	.cfi_endproc

	.globl	free_INSN_LIST_list
	.align	16, 0x90
	.type	free_INSN_LIST_list,@function
free_INSN_LIST_list:                    # @free_INSN_LIST_list
	.cfi_startproc
# BB#0:                                 # %entry
	movq	(%rdi), %rax
	testq	%rax, %rax
	je	.LBB5_3
	.align	16, 0x90
.LBB5_1:                                # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rcx
	movq	16(%rcx), %rax
	testq	%rax, %rax
	jne	.LBB5_1
# BB#2:                                 # %free_list.exit
	movq	unused_insn_list(%rip), %rax
	movq	%rax, 16(%rcx)
	movq	(%rdi), %rax
	movq	%rax, unused_insn_list(%rip)
	movq	$0, (%rdi)
.LBB5_3:                                # %return
	retq
.Ltmp5:
	.size	free_INSN_LIST_list, .Ltmp5-free_INSN_LIST_list
	.cfi_endproc

	.globl	free_EXPR_LIST_node
	.align	16, 0x90
	.type	free_EXPR_LIST_node,@function
free_EXPR_LIST_node:                    # @free_EXPR_LIST_node
	.cfi_startproc
# BB#0:                                 # %entry
	movq	unused_expr_list(%rip), %rax
	movq	%rax, 16(%rdi)
	movq	%rdi, unused_expr_list(%rip)
	retq
.Ltmp6:
	.size	free_EXPR_LIST_node, .Ltmp6-free_EXPR_LIST_node
	.cfi_endproc

	.globl	free_INSN_LIST_node
	.align	16, 0x90
	.type	free_INSN_LIST_node,@function
free_INSN_LIST_node:                    # @free_INSN_LIST_node
	.cfi_startproc
# BB#0:                                 # %entry
	movq	unused_insn_list(%rip), %rax
	movq	%rax, 16(%rdi)
	movq	%rdi, unused_insn_list(%rip)
	retq
.Ltmp7:
	.size	free_INSN_LIST_node, .Ltmp7-free_INSN_LIST_node
	.cfi_endproc

	.type	unused_insn_list,@object # @unused_insn_list
	.local	unused_insn_list
	.comm	unused_insn_list,8,8
	.type	unused_expr_list,@object # @unused_expr_list
	.local	unused_expr_list
	.comm	unused_expr_list,8,8

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
