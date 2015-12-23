	.file	"stmt.c.bc"
	.text
	.globl	using_eh_for_cleanups
	.align	16, 0x90
	.type	using_eh_for_cleanups,@function
using_eh_for_cleanups:                  # @using_eh_for_cleanups
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$1, %rax
	movb	%al, using_eh_for_cleanups_p(%rip)
	retq
.Ltmp0:
	.size	using_eh_for_cleanups, .Ltmp0-using_eh_for_cleanups
	.cfi_endproc

	.globl	free_stmt_status
	.align	16, 0x90
	.type	free_stmt_status,@function
free_stmt_status:                       # @free_stmt_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB1_2
# BB#1:                                 # %if.then
	callq	free
.LBB1_2:                                # %if.end
	movq	$0, 8(%rbx)
	popq	%rbx
	retq
.Ltmp5:
	.size	free_stmt_status, .Ltmp5-free_stmt_status
	.cfi_endproc

	.globl	mark_stmt_status
	.align	16, 0x90
	.type	mark_stmt_status,@function
mark_stmt_status:                       # @mark_stmt_status
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp12:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp13:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp14:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp15:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp16:
	.cfi_def_cfa_offset 48
.Ltmp17:
	.cfi_offset %rbx, -48
.Ltmp18:
	.cfi_offset %r12, -40
.Ltmp19:
	.cfi_offset %r13, -32
.Ltmp20:
	.cfi_offset %r14, -24
.Ltmp21:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	testq	%r14, %r14
	je	.LBB2_124
# BB#1:                                 # %if.end
	movq	(%r14), %r12
	testq	%r12, %r12
	je	.LBB2_34
# BB#2:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_3:                                # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_23 Depth 2
	movq	24(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_6
# BB#4:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_6
# BB#5:                                 # %if.then.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_6:                                # %do.body2.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	40(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_9
# BB#7:                                 # %land.lhs.true5.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_9
# BB#8:                                 # %if.then8.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_9:                                # %do.body11.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	48(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_12
# BB#10:                                # %land.lhs.true16.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_12
# BB#11:                                # %if.then19.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_12:                               # %do.body22.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	64(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_17
# BB#13:                                # %land.lhs.true26.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_17
# BB#14:                                # %do.body30.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_16
# BB#15:                                # %if.then32.i
                                        #   in Loop: Header=BB2_3 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_16:                               # %if.end35.i
                                        #   in Loop: Header=BB2_3 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_17:                               # %do.body41.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	72(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_22
# BB#18:                                # %land.lhs.true46.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_22
# BB#19:                                # %do.body50.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_21
# BB#20:                                # %if.then54.i
                                        #   in Loop: Header=BB2_3 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_21:                               # %if.end58.i
                                        #   in Loop: Header=BB2_3 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_22:                               # %do.end66.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	80(%r12), %rbx
	jmp	.LBB2_23
	.align	16, 0x90
.LBB2_29:                               # %for.inc.i
                                        #   in Loop: Header=BB2_23 Depth=2
	movq	(%rbx), %rbx
.LBB2_23:                               # %do.end66.i
                                        #   Parent Loop BB2_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB2_30
# BB#24:                                # %if.then72.i
                                        #   in Loop: Header=BB2_23 Depth=2
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movq	8(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_29
# BB#25:                                # %land.lhs.true79.i
                                        #   in Loop: Header=BB2_23 Depth=2
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_29
# BB#26:                                # %do.body83.i
                                        #   in Loop: Header=BB2_23 Depth=2
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_28
# BB#27:                                # %if.then87.i
                                        #   in Loop: Header=BB2_23 Depth=2
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_28:                               # %if.end91.i
                                        #   in Loop: Header=BB2_23 Depth=2
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r15, 32(%rax,%rcx,8)
	jmp	.LBB2_29
	.align	16, 0x90
.LBB2_30:                               # %do.body100.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	104(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB2_33
# BB#31:                                # %land.lhs.true105.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r13, %rax
	jne	.LBB2_33
# BB#32:                                # %if.then108.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_33:                               # %do.end110.i
                                        #   in Loop: Header=BB2_3 Depth=1
	movq	8(%r12), %r12
	testq	%r12, %r12
	jne	.LBB2_3
.LBB2_34:                               # %mark_block_nesting.exit
	movq	16(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_46
# BB#35:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_36:                               # %do.body.i38
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_39
# BB#37:                                # %land.lhs.true.i41
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_39
# BB#38:                                # %if.then.i42
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_39:                               # %do.body2.i44
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	32(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_42
# BB#40:                                # %land.lhs.true5.i47
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_42
# BB#41:                                # %if.then8.i48
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_42:                               # %do.body11.i50
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	40(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_45
# BB#43:                                # %land.lhs.true16.i53
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_45
# BB#44:                                # %if.then19.i54
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_45:                               # %do.end21.i
                                        #   in Loop: Header=BB2_36 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_36
.LBB2_46:                               # %mark_cond_nesting.exit
	movq	24(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_64
# BB#47:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_48:                               # %do.body.i59
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_51
# BB#49:                                # %land.lhs.true.i62
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_51
# BB#50:                                # %if.then.i63
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_51:                               # %do.body2.i66
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	32(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_54
# BB#52:                                # %land.lhs.true5.i69
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_54
# BB#53:                                # %if.then8.i70
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_54:                               # %do.body11.i72
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	40(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_57
# BB#55:                                # %land.lhs.true16.i75
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_57
# BB#56:                                # %if.then19.i76
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_57:                               # %do.body22.i77
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	48(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_60
# BB#58:                                # %land.lhs.true27.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_60
# BB#59:                                # %if.then30.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_60:                               # %do.body33.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	56(%rbx), %r15
	testq	%r15, %r15
	je	.LBB2_63
# BB#61:                                # %land.lhs.true38.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_63
# BB#62:                                # %if.then41.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	%r15, %rdi
	callq	ggc_mark_rtx_children
.LBB2_63:                               # %do.end43.i
                                        #   in Loop: Header=BB2_48 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB2_48
.LBB2_64:                               # %mark_loop_nesting.exit
	movq	32(%r14), %r15
	testq	%r15, %r15
	je	.LBB2_88
# BB#65:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_66:                               # %do.body.i82
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_69
# BB#67:                                # %land.lhs.true.i85
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_69
# BB#68:                                # %if.then.i86
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_69:                               # %do.body2.i89
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_72
# BB#70:                                # %land.lhs.true5.i92
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_72
# BB#71:                                # %if.then8.i93
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_72:                               # %do.body11.i94
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_77
# BB#73:                                # %land.lhs.true15.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_77
# BB#74:                                # %do.body19.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_76
# BB#75:                                # %if.then21.i
                                        #   in Loop: Header=BB2_66 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_76:                               # %if.end24.i
                                        #   in Loop: Header=BB2_66 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_77:                               # %do.body30.i101
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	56(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_82
# BB#78:                                # %land.lhs.true35.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_82
# BB#79:                                # %do.body39.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_81
# BB#80:                                # %if.then43.i
                                        #   in Loop: Header=BB2_66 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_81:                               # %if.end47.i
                                        #   in Loop: Header=BB2_66 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_82:                               # %do.body56.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	64(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_87
# BB#83:                                # %land.lhs.true61.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_87
# BB#84:                                # %do.body65.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_86
# BB#85:                                # %if.then69.i
                                        #   in Loop: Header=BB2_66 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_86:                               # %if.end73.i
                                        #   in Loop: Header=BB2_66 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_87:                               # %do.end81.i
                                        #   in Loop: Header=BB2_66 Depth=1
	movq	40(%r15), %rdi
	callq	mark_case_node
	movq	8(%r15), %r15
	testq	%r15, %r15
	jne	.LBB2_66
.LBB2_88:                               # %mark_case_nesting.exit
	movq	56(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_97
# BB#89:                                # %land.lhs.true
	movq	%rbx, %rdi
	callq	ggc_set_mark
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB2_93
# BB#90:                                # %do.body3
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_92
# BB#91:                                # %if.then5
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_92:                               # %if.end8
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%rbx, 32(%rax,%rcx,8)
.LBB2_93:                               # %do.end11
	cmpq	$0, 56(%r14)
	je	.LBB2_97
# BB#94:                                # %do.body15
	movq	64(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB2_97
# BB#95:                                # %land.lhs.true17
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB2_97
# BB#96:                                # %if.then20
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_97:                               # %if.end23
	movq	96(%r14), %r15
	testq	%r15, %r15
	je	.LBB2_124
# BB#98:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_99:                               # %if.then.i106
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rdi
	callq	ggc_set_mark
	movq	8(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_102
# BB#100:                               # %land.lhs.true.i107
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_102
# BB#101:                               # %if.then5.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_102:                              # %do.body8.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	16(%r15), %r14
	testq	%r14, %r14
	je	.LBB2_107
# BB#103:                               # %land.lhs.true10.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_107
# BB#104:                               # %do.body14.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_106
# BB#105:                               # %if.then16.i
                                        #   in Loop: Header=BB2_99 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_106:                              # %if.end19.i
                                        #   in Loop: Header=BB2_99 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB2_107:                              # %do.body24.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	24(%r15), %r14
	testq	%r14, %r14
	je	.LBB2_112
# BB#108:                               # %land.lhs.true27.i119
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_112
# BB#109:                               # %do.body31.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_111
# BB#110:                               # %if.then35.i
                                        #   in Loop: Header=BB2_99 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_111:                              # %if.end39.i
                                        #   in Loop: Header=BB2_99 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB2_112:                              # %do.body48.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	32(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_115
# BB#113:                               # %land.lhs.true51.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_115
# BB#114:                               # %if.then54.i122
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_115:                              # %do.body57.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	48(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB2_118
# BB#116:                               # %land.lhs.true60.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_118
# BB#117:                               # %if.then63.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%rbx, %rdi
	callq	ggc_mark_rtx_children
.LBB2_118:                              # %do.body66.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	56(%r15), %r14
	testq	%r14, %r14
	je	.LBB2_123
# BB#119:                               # %land.lhs.true69.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r12, %rax
	jne	.LBB2_123
# BB#120:                               # %do.body73.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB2_122
# BB#121:                               # %if.then77.i
                                        #   in Loop: Header=BB2_99 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB2_122:                              # %if.end81.i
                                        #   in Loop: Header=BB2_99 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB2_123:                              # %do.end89.i
                                        #   in Loop: Header=BB2_99 Depth=1
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB2_99
.LBB2_124:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp22:
	.size	mark_stmt_status, .Ltmp22-mark_stmt_status
	.cfi_endproc

	.globl	init_stmt
	.align	16, 0x90
	.type	init_stmt,@function
init_stmt:                              # @init_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$stmt_obstack, %rdi
	jmp	gcc_obstack_init  # TAILCALL
.Ltmp23:
	.size	init_stmt, .Ltmp23-init_stmt
	.cfi_endproc

	.globl	init_stmt_for_function
	.align	16, 0x90
	.type	init_stmt_for_function,@function
init_stmt_for_function:                 # @init_stmt_for_function
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp25:
	.cfi_def_cfa_offset 16
	movq	$104, %rdi
	callq	xmalloc
	movq	cfun(%rip), %rcx
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 40(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 52(%rax)
	movq	$0, 96(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	%ecx, 72(%rax)
	movq	$0, 56(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 64(%rax)
	popq	%rax
	retq
.Ltmp26:
	.size	init_stmt_for_function, .Ltmp26-init_stmt_for_function
	.cfi_endproc

	.globl	in_control_zone_p
	.align	16, 0x90
	.type	in_control_zone_p,@function
in_control_zone_p:                      # @in_control_zone_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rcx
	cmpq	$0, 16(%rcx)
	movq	$1, %rax
	jne	.LBB5_5
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, 24(%rcx)
	jne	.LBB5_5
# BB#2:                                 # %lor.rhs
	cmpq	$0, 32(%rcx)
	je	.LBB5_3
# BB#4:                                 # %lor.rhs
	movabsq	$1, %rax
.LBB5_5:                                # %lor.end
	retq
.LBB5_3:
	movabsq	$0, %rax
	retq
.Ltmp27:
	.size	in_control_zone_p, .Ltmp27-in_control_zone_p
	.cfi_endproc

	.globl	set_file_and_line_for_stmt
	.align	16, 0x90
	.type	set_file_and_line_for_stmt,@function
set_file_and_line_for_stmt:             # @set_file_and_line_for_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB6_2
# BB#1:                                 # %if.then
	movq	%rdi, 80(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	%esi, 88(%rax)
.LBB6_2:                                # %if.end
	retq
.Ltmp28:
	.size	set_file_and_line_for_stmt, .Ltmp28-set_file_and_line_for_stmt
	.cfi_endproc

	.globl	emit_nop
	.align	16, 0x90
	.type	emit_nop,@function
emit_nop:                               # @emit_nop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp30:
	.cfi_def_cfa_offset 16
	callq	get_last_insn
	movl	optimize(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB7_4
# BB#1:                                 # %land.lhs.true
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	je	.LBB7_5
# BB#2:                                 # %land.lhs.true
	cmpq	$37, %rcx
	jne	.LBB7_4
# BB#3:                                 # %land.lhs.true4
	movq	%rax, %rdi
	callq	prev_real_insn
	testq	%rax, %rax
	je	.LBB7_5
.LBB7_4:                                # %if.end
	popq	%rax
	retq
.LBB7_5:                                # %if.then
	callq	gen_nop
	movq	%rax, %rdi
	popq	%rax
	jmp	emit_insn  # TAILCALL
.Ltmp31:
	.size	emit_nop, .Ltmp31-emit_nop
	.cfi_endproc

	.globl	label_rtx
	.align	16, 0x90
	.type	label_rtx,@function
label_rtx:                              # @label_rtx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp34:
	.cfi_def_cfa_offset 16
.Ltmp35:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB8_5
# BB#1:                                 # %if.end
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB8_4
# BB#2:                                 # %if.end5
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB8_4
# BB#3:                                 # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB8_4:                                # %cond.end
	popq	%rbx
	retq
.LBB8_5:                                # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp36:
	.size	label_rtx, .Ltmp36-label_rtx
	.cfi_endproc

	.globl	emit_jump
	.align	16, 0x90
	.type	emit_jump,@function
emit_jump:                              # @emit_jump
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 16
.Ltmp40:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	popq	%rbx
	jmp	emit_barrier  # TAILCALL
.Ltmp41:
	.size	emit_jump, .Ltmp41-emit_jump
	.cfi_endproc

	.globl	expand_computed_goto
	.align	16, 0x90
	.type	expand_computed_goto,@function
expand_computed_goto:                   # @expand_computed_goto
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp44:
	.cfi_def_cfa_offset 16
.Ltmp45:
	.cfi_offset %rbx, -16
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, %rbx
	callq	emit_queue
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
	callq	emit_indirect_jump
	movq	cfun(%rip), %rax
	movl	424(%rax), %ecx
	orq	$2048, %rcx             # imm = 0x800
	movl	%ecx, 424(%rax)
	popq	%rbx
	retq
.Ltmp46:
	.size	expand_computed_goto, .Ltmp46-expand_computed_goto
	.cfi_endproc

	.globl	expand_label
	.align	16, 0x90
	.type	expand_label,@function
expand_label:                           # @expand_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp52:
	.cfi_def_cfa_offset 32
.Ltmp53:
	.cfi_offset %rbx, -24
.Ltmp54:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	callq	do_pending_stack_adjust
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB11_11
# BB#1:                                 # %if.end.i
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB11_4
# BB#2:                                 # %if.end5.i
	callq	gen_label_rtx
	movq	%rax, %rdi
	testq	%rdi, %rdi
	movq	%rdi, 144(%rbx)
	jne	.LBB11_4
# BB#3:                                 # %cond.false.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB11_4:                               # %label_rtx.exit
	callq	emit_label
	movq	72(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB11_8
# BB#5:                                 # %if.then
	movq	144(%rbx), %rax
	movq	32(%rcx), %r14
	testq	%rax, %rax
	jne	.LBB11_7
# BB#6:                                 # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB11_7:                               # %cond.end
	movq	%r14, 56(%rax)
.LBB11_8:                               # %if.end
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	cmpq	$0, 8(%rax)
	je	.LBB11_10
# BB#9:                                 # %if.then10
	movq	$16, %rdi
	callq	ggc_alloc
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	80(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 80(%rcx)
	movq	%rbx, 8(%rax)
.LBB11_10:                              # %if.end20
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB11_11:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp55:
	.size	expand_label, .Ltmp55-expand_label
	.cfi_endproc

	.globl	declare_nonlocal_label
	.align	16, 0x90
	.type	declare_nonlocal_label,@function
declare_nonlocal_label:                 # @declare_nonlocal_label
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp59:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp61:
	.cfi_def_cfa_offset 32
.Ltmp62:
	.cfi_offset %rbx, -24
.Ltmp63:
	.cfi_offset %r14, -16
	movl	target_flags(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movzbl	mode_size+4(%rax), %esi
	movq	%rdi, %rbx
	orq	$4, %rax
	xorq	%rdx, %rdx
	movq	%rax, %rdi
	callq	assign_stack_local
	movq	cfun(%rip), %rcx
	movq	160(%rcx), %rdx
	movq	%rax, %r14
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	%rax, 160(%rcx)
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB12_7
# BB#1:                                 # %if.end.i
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB12_4
# BB#2:                                 # %if.end5.i
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB12_4
# BB#3:                                 # %cond.false.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB12_4:                               # %label_rtx.exit
	movl	(%rax), %ecx
	orq	$268435456, %rcx        # imm = 0x10000000
	movl	%ecx, (%rax)
	movq	cfun(%rip), %rsi
	movq	168(%rsi), %rcx
	testq	%rcx, %rcx
	jne	.LBB12_6
# BB#5:                                 # %if.then
	movq	240(%rsi), %rax
	movq	16(%rax), %rdx
	addq	$184, %rsi
	movq	$2, %rdi
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movq	168(%rax), %rcx
.LBB12_6:                               # %if.end
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_ee
	movq	cfun(%rip), %rcx
	movq	%rax, 168(%rcx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB12_7:                               # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp64:
	.size	declare_nonlocal_label, .Ltmp64-declare_nonlocal_label
	.cfi_endproc

	.globl	expand_goto
	.align	16, 0x90
	.type	expand_goto,@function
expand_goto:                            # @expand_goto
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp70:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp71:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp72:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp73:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp74:
	.cfi_def_cfa_offset 48
.Ltmp75:
	.cfi_offset %rbx, -40
.Ltmp76:
	.cfi_offset %r12, -32
.Ltmp77:
	.cfi_offset %r14, -24
.Ltmp78:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
	callq	decl_function_context
	testq	%rax, %rax
	je	.LBB13_17
# BB#1:                                 # %entry
	movq	current_function_decl(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB13_17
# BB#2:                                 # %if.then
	movq	%rax, %rdi
	callq	find_function_data
	movl	target_flags(%rip), %r15d
	movzbl	16(%rbx), %edx
	movq	%rax, %r14
	movq	$25, %rcx
	shrq	%cl, %r15
	cmpq	$31, %rdx
	jne	.LBB13_23
# BB#3:                                 # %if.end.i
	movq	144(%rbx), %rdx
	testq	%rdx, %rdx
	jne	.LBB13_6
# BB#4:                                 # %if.end5.i
	callq	gen_label_rtx
	movq	%rax, %rdx
	testq	%rdx, %rdx
	movq	%rdx, 144(%rbx)
	jne	.LBB13_6
# BB#5:                                 # %cond.false.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdx
.LBB13_6:                               # %label_rtx.exit
	andq	$1, %r15
	orq	$4, %r15
	movq	$67, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_fmt_u00
	movq	160(%r14), %rdx
	movq	32(%rdx), %rsi
	movq	168(%r14), %rcx
	jmp	.LBB13_8
	.align	16, 0x90
.LBB13_7:                               # %for.body
                                        #   in Loop: Header=BB13_8 Depth=1
	movq	(%rdx), %rdx
	movq	32(%rdx), %rsi
	movq	16(%rcx), %rcx
.LBB13_8:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rbx, %rsi
	jne	.LBB13_7
# BB#9:                                 # %for.end
	movl	424(%r14), %edx
	movq	8(%rcx), %r12
	orq	$256, %rdx              # imm = 0x100
	movl	%edx, 424(%r14)
	movq	cfun(%rip), %rcx
	movl	424(%rcx), %edx
	orq	$512, %rdx              # imm = 0x200
	movl	%edx, 424(%rcx)
	movl	(%rax), %ecx
	orq	$134217728, %rcx        # imm = 0x8000000
	movl	%ecx, (%rax)
	movq	%rbx, %rdi
	callq	lookup_static_chain
	movq	%rax, %rdi
	callq	copy_to_reg
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	copy_rtx
	movq	global_rtl+56(%rip), %rsi
	movq	%rax, %rdi
	movq	%r15, %rdx
	callq	replace_rtx
	movq	%rax, %rdi
	callq	copy_to_reg
	movq	184(%r14), %rdi
	movq	%rax, %r14
	xorq	%rbx, %rbx
	testq	%rdi, %rdi
	je	.LBB13_11
# BB#10:                                # %if.then21
	callq	copy_rtx
	movq	global_rtl+56(%rip), %rsi
	movq	%rax, %rdi
	movq	%r15, %rdx
	callq	replace_rtx
	movq	%rax, %rbx
.LBB13_11:                              # %if.end
	movq	global_rtl+32(%rip), %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	$2, %rdi
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	emit_stack_restore
	movq	global_rtl+32(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	global_rtl+16(%rip), %rdx
	movq	$48, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_e
	movq	%rax, %rdi
	callq	emit_insn
	movq	%r14, %rdi
	callq	emit_indirect_jump
	callq	get_last_insn
	movq	%rax, %rbx
	jmp	.LBB13_12
.LBB13_17:                              # %if.else55
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB13_23
# BB#18:                                # %if.end.i87
	movq	144(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB13_21
# BB#19:                                # %if.end5.i90
	callq	gen_label_rtx
	movq	%rax, %rsi
	testq	%rsi, %rsi
	movq	%rsi, 144(%rbx)
	jne	.LBB13_21
# BB#20:                                # %cond.false.i91
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rsi
.LBB13_21:                              # %label_rtx.exit93
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	expand_goto_internal  # TAILCALL
	.align	16, 0x90
.LBB13_22:                              # %for.inc50
                                        #   in Loop: Header=BB13_12 Depth=1
	movq	16(%rbx), %rbx
.LBB13_12:                              # %if.end
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB13_16
# BB#13:                                # %for.body32
                                        #   in Loop: Header=BB13_12 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$34, %rax
	je	.LBB13_16
# BB#14:                                # %for.body32
                                        #   in Loop: Header=BB13_12 Depth=1
	cmpq	$33, %rax
	jne	.LBB13_22
# BB#15:                                # %if.then36
	movq	const_int_rtx+512(%rip), %rsi
	movq	56(%rbx), %rdx
	movq	$27, %rdi
	callq	alloc_EXPR_LIST
	movq	%rax, 56(%rbx)
.LBB13_16:                              # %if.end57
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB13_23:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp79:
	.size	expand_goto, .Ltmp79-expand_goto
	.cfi_endproc

	.align	16, 0x90
	.type	expand_goto_internal,@function
expand_goto_internal:                   # @expand_goto_internal
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp86:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp87:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp88:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp89:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp90:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp91:
	.cfi_def_cfa_offset 64
.Ltmp92:
	.cfi_offset %rbx, -48
.Ltmp93:
	.cfi_offset %r12, -40
.Ltmp94:
	.cfi_offset %r13, -32
.Ltmp95:
	.cfi_offset %r14, -24
.Ltmp96:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movzbl	(%r13), %eax
	movq	%rdi, %r14
	cmpq	$36, %rax
	jne	.LBB14_61
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	cmpq	$0, 16(%r13)
	movq	8(%rax), %rax
	je	.LBB14_18
# BB#2:                                 # %if.then2
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB14_15
# BB#3:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	.align	16, 0x90
.LBB14_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	48(%rbx), %rax
	movslq	8(%rax), %rax
	movslq	8(%r13), %rcx
	cmpq	%rcx, %rax
	jl	.LBB14_10
# BB#5:                                 # %if.end11
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	40(%rbx), %rax
	movq	64(%rbx), %rdi
	testq	%rax, %rax
	je	.LBB14_7
# BB#6:                                 # %if.end11
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	%rax, %r15
.LBB14_7:                               # %if.end11
                                        #   in Loop: Header=BB14_4 Depth=1
	testq	%rdi, %rdi
	je	.LBB14_9
# BB#8:                                 # %if.then24
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	$1, %rsi
	movq	$1, %rdx
	callq	expand_cleanups
	callq	do_pending_stack_adjust
.LBB14_9:                               # %for.inc
                                        #   in Loop: Header=BB14_4 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB14_4
.LBB14_10:                              # %for.end
	testq	%r15, %r15
	je	.LBB14_15
# BB#11:                                # %if.then30
	callq	clear_pending_stack_adjust
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	200(%rax), %rax
	cmpq	%r13, %rax
	jne	.LBB14_14
# BB#12:                                # %land.lhs.true
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$23, %rcx
	jne	.LBB14_14
# BB#13:                                # %land.lhs.true36
	movzbl	62(%rax), %eax
	testq	$2, %rax
	jne	.LBB14_15
.LBB14_14:                              # %if.else
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	emit_stack_restore
.LBB14_15:                              # %if.end45
	testq	%r14, %r14
	je	.LBB14_60
# BB#16:                                # %land.lhs.true47
	movzbl	49(%r14), %eax
	testq	$16, %rax
	je	.LBB14_60
# BB#17:                                # %if.then52
	movq	72(%r14), %rax
	movq	32(%rax), %rsi
	movabsq	$.L.str30, %rdi
	xorq	%rax, %rax
	callq	error
	jmp	.LBB14_60
.LBB14_18:                              # %if.else55
	movq	%rdx, %rbx
	movq	16(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB14_21
# BB#19:                                # %land.lhs.true.i
	movq	32(%rcx), %rdx
	cmpq	%r13, %rdx
	je	.LBB14_26
# BB#20:                                # %lor.lhs.false.i
	movq	40(%rcx), %rdx
	cmpq	%r13, %rdx
	je	.LBB14_25
.LBB14_21:                              # %if.else.i
	movq	24(%rax), %rcx
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB14_35
# BB#22:                                # %land.lhs.true12.i
	movq	32(%rcx), %rsi
	cmpq	%r13, %rsi
	je	.LBB14_26
# BB#23:                                # %lor.lhs.false17.i
	movq	40(%rcx), %rsi
	cmpq	%r13, %rsi
	je	.LBB14_25
# BB#24:                                # %lor.lhs.false23.i
	movq	56(%rcx), %rsi
	cmpq	%r13, %rsi
	jne	.LBB14_35
.LBB14_25:                              # %if.end33.i
	xorq	%rdx, %rdx
	testq	%rcx, %rcx
	je	.LBB14_35
.LBB14_26:                              # %if.then35.i
	movq	(%rax), %rsi
	movq	%rcx, %rdi
	.align	16, 0x90
.LBB14_27:                              # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rdi
	testq	%rdi, %rdi
	je	.LBB14_29
# BB#28:                                # %while.cond.i
                                        #   in Loop: Header=BB14_27 Depth=1
	cmpq	%rsi, %rdi
	jne	.LBB14_27
.LBB14_29:                              # %while.end.i
	movq	$1, %rdx
	testq	%rdi, %rdi
	jne	.LBB14_57
# BB#30:                                # %if.end42.i
	movq	8(%rsi), %rdx
	jmp	.LBB14_31
.LBB14_34:                              # %for.inc.i
                                        #   in Loop: Header=BB14_31 Depth=1
	movq	(%rsi), %rsi
.LBB14_31:                              # %if.end42.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	%rcx, %rsi
	je	.LBB14_35
# BB#32:                                # %for.body.i
                                        #   in Loop: Header=BB14_31 Depth=1
	cmpq	%rdx, %rsi
	jne	.LBB14_34
# BB#33:                                # %if.then49.i
                                        #   in Loop: Header=BB14_31 Depth=1
	movq	8(%rsi), %rdx
	jmp	.LBB14_34
.LBB14_35:                              # %if.end53.i
	movq	(%rax), %r15
	cmpq	%rdx, %r15
	je	.LBB14_36
	.align	16, 0x90
.LBB14_39:                              # %for.body58.i
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, 40(%r15)
	jne	.LBB14_41
# BB#40:                                # %lor.lhs.false62.i
                                        #   in Loop: Header=BB14_39 Depth=1
	cmpq	$0, 64(%r15)
	jne	.LBB14_41
# BB#37:                                # %for.cond56.i
                                        #   in Loop: Header=BB14_39 Depth=1
	movq	8(%r15), %r15
	cmpq	%rdx, %r15
	jne	.LBB14_39
# BB#38:
	movq	%rdx, %r15
	jmp	.LBB14_54
.LBB14_41:                              # %if.then72.i
	movq	$64, %rdi
	callq	ggc_alloc
	movq	%rax, %r12
	testq	%rbx, %rbx
	je	.LBB14_43
# BB#42:                                # %if.end75.i
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r14, 16(%r12)
	movq	%r13, 32(%r12)
	jmp	.LBB14_44
.LBB14_36:
	movq	%rdx, %r15
	jmp	.LBB14_54
.LBB14_43:                              # %cond.false.i
	callq	do_pending_stack_adjust
	movq	%r14, 16(%r12)
	movq	%r13, 32(%r12)
	callq	get_last_insn
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB14_44:                              # %cond.end.i
	movq	$4, %rdi
	callq	make_node
	movq	%rax, %rbx
	movl	16(%rbx), %eax
	orq	$65536, %rax            # imm = 0x10000
	movl	%eax, 16(%rbx)
	movq	cfun(%rip), %rax
	movzbl	426(%rax), %eax
	testq	$4, %rax
	je	.LBB14_45
# BB#46:                                # %if.else85.i
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, (%rbx)
	movq	current_function_decl(%rip), %rax
	movq	104(%rax), %rax
	movq	%rbx, (%rax)
	jmp	.LBB14_47
.LBB14_45:                              # %if.then84.i
	movq	%rbx, %rdi
	callq	insert_block
.LBB14_47:                              # %if.end93.i
	callq	start_sequence
	movabsq	$4294967198, %rsi       # imm = 0xFFFFFF9E
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rcx
	movzbl	426(%rcx), %ecx
	testq	$4, %rcx
	je	.LBB14_49
# BB#48:                                # %if.then100.i
	movq	%rbx, 32(%rax)
.LBB14_49:                              # %if.end101.i
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, 8(%r12)
	movabsq	$4294967199, %rsi       # imm = 0xFFFFFF9F
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rcx
	movzbl	426(%rcx), %ecx
	testq	$4, %rcx
	je	.LBB14_51
# BB#50:                                # %if.then109.i
	movq	%rbx, 32(%rax)
.LBB14_51:                              # %if.end113.i
	movq	%rbx, 24(%r12)
	callq	end_sequence
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	emit_insns_after
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	52(%rax), %eax
	movl	%eax, 40(%r12)
	movq	$0, 48(%r12)
	movq	72(%r15), %rdx
	movq	64(%r15), %rsi
	movq	%rdx, %rax
	orq	%rsi, %rax
	movabsq	$0, %rax
	je	.LBB14_53
# BB#52:                                # %cond.true125.i
	xorq	%rdi, %rdi
	callq	tree_cons
.LBB14_53:                              # %cond.end134.i
	movq	%rax, 56(%r12)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	96(%rax), %rax
	movq	%rax, (%r12)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%r12, 96(%rax)
.LBB14_54:                              # %if.end140.i
	testq	%r15, %r15
	jne	.LBB14_55
# BB#56:                                # %if.end140.i
	movabsq	$1, %rdx
	jmp	.LBB14_57
.LBB14_55:
	movabsq	$0, %rdx
.LBB14_57:                              # %expand_fixup.exit
	testq	%rdx, %rdx
	je	.LBB14_60
# BB#58:                                # %expand_fixup.exit
	testq	%r14, %r14
	je	.LBB14_60
# BB#59:                                # %if.then59
	movl	16(%r14), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%r14)
.LBB14_60:                              # %if.end65
	callq	do_pending_stack_adjust
	movq	%r13, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_barrier  # TAILCALL
.LBB14_61:                              # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_goto_internal, %rcx
	movq	$881, %rsi              # imm = 0x371
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp97:
	.size	expand_goto_internal, .Ltmp97-expand_goto_internal
	.cfi_endproc

	.globl	expand_fixups
	.align	16, 0x90
	.type	expand_fixups,@function
expand_fixups:                          # @expand_fixups
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r8, %r8
	movq	%rax, %rcx
	jmp	fixup_gotos  # TAILCALL
.Ltmp98:
	.size	expand_fixups, .Ltmp98-expand_fixups
	.cfi_endproc

	.align	16, 0x90
	.type	fixup_gotos,@function
fixup_gotos:                            # @fixup_gotos
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp105:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp106:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp107:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp108:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp110:
	.cfi_def_cfa_offset 64
.Ltmp111:
	.cfi_offset %rbx, -48
.Ltmp112:
	.cfi_offset %r12, -40
.Ltmp113:
	.cfi_offset %r13, -32
.Ltmp114:
	.cfi_offset %r14, -24
.Ltmp115:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	96(%rax), %rax
	movq	%rcx, %r15
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB16_40
# BB#1:                                 # %for.body.lr.ph
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r8
	movq	8(%rsp), %rcx           # 8-byte Reload
	orq	%rcx, %r8
	orq	%rdx, %r8
	je	.LBB16_2
# BB#3:                                 # %for.body.lr.ph
	movabsq	$1, %r12
	jmp	.LBB16_4
.LBB16_2:
	movabsq	$0, %r12
.LBB16_4:                               # %for.body.lr.ph
	xorq	%r14, %r14
	xorq	$1, %r12
	.align	16, 0x90
.LBB16_5:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_16 Depth 2
	movq	%r14, %rcx
	movq	%rax, %r14
	movq	8(%r14), %rax
	testq	%rax, %rax
	jne	.LBB16_8
# BB#6:                                 # %if.then
                                        #   in Loop: Header=BB16_5 Depth=1
	testq	%rcx, %rcx
	je	.LBB16_27
# BB#7:                                 # %if.then2
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	(%r14), %rax
	movq	%rax, (%rcx)
	jmp	.LBB16_27
	.align	16, 0x90
.LBB16_8:                               # %if.else
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	32(%r14), %rcx
	cmpq	$0, 16(%rcx)
	je	.LBB16_27
# BB#9:                                 # %if.then5
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	16(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB16_15
# BB#10:                                # %if.then5
                                        #   in Loop: Header=BB16_5 Depth=1
	testq	%r12, %r12
	jne	.LBB16_15
# BB#11:                                # %land.lhs.true11
                                        #   in Loop: Header=BB16_5 Depth=1
	movslq	8(%r15), %rdx
	movslq	8(%rcx), %rcx
	cmpq	%rcx, %rdx
	jge	.LBB16_15
# BB#12:                                # %land.lhs.true19
                                        #   in Loop: Header=BB16_5 Depth=1
	movslq	8(%rax), %rax
	cmpq	%rax, %rdx
	jle	.LBB16_15
# BB#13:                                # %land.lhs.true28
                                        #   in Loop: Header=BB16_5 Depth=1
	movzbl	49(%rdi), %eax
	testq	$4, %rax
	jne	.LBB16_15
# BB#14:                                # %if.then31
                                        #   in Loop: Header=BB16_5 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str17, %rsi
	callq	error_with_decl
	movq	16(%r14), %rax
	orq	$1024, 48(%rax)         # imm = 0x400
	.align	16, 0x90
.LBB16_15:                              # %if.end38
                                        #   in Loop: Header=BB16_5 Depth=1
	callq	start_sequence
	xorq	%rdi, %rdi
	callq	pushlevel
	movq	24(%r14), %rdi
	callq	set_block
	movq	56(%r14), %rbx
	jmp	.LBB16_16
	.align	16, 0x90
.LBB16_20:                              # %for.inc
                                        #   in Loop: Header=BB16_16 Depth=2
	movq	(%rbx), %rbx
.LBB16_16:                              # %if.end38
                                        #   Parent Loop BB16_5 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB16_21
# BB#17:                                # %for.body44
                                        #   in Loop: Header=BB16_16 Depth=2
	movzbl	17(%rbx), %eax
	testq	$4, %rax
	je	.LBB16_20
# BB#18:                                # %land.lhs.true49
                                        #   in Loop: Header=BB16_16 Depth=2
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB16_20
# BB#19:                                # %if.then51
                                        #   in Loop: Header=BB16_16 Depth=2
	movq	$1, %rsi
	movq	$1, %rdx
	callq	expand_cleanups
	callq	do_pending_stack_adjust
	jmp	.LBB16_20
	.align	16, 0x90
.LBB16_21:                              # %if.end56
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	48(%r14), %rsi
	testq	%rsi, %rsi
	je	.LBB16_26
# BB#22:                                # %land.lhs.true59
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	cfun(%rip), %rax
	movq	32(%r14), %rcx
	movq	200(%rax), %rax
	cmpq	%rax, %rcx
	jne	.LBB16_25
# BB#23:                                # %land.lhs.true62
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %ecx
	cmpq	$23, %rcx
	jne	.LBB16_25
# BB#24:                                # %land.lhs.true68
                                        #   in Loop: Header=BB16_5 Depth=1
	movzbl	62(%rax), %eax
	testq	$2, %rax
	jne	.LBB16_26
.LBB16_25:                              # %if.then76
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	8(%r14), %rdx
	xorq	%rdi, %rdi
	callq	emit_stack_restore
.LBB16_26:                              # %if.end79
                                        #   in Loop: Header=BB16_5 Depth=1
	callq	get_insns
	movq	%rax, %rbx
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	callq	end_sequence
	movq	8(%r14), %rsi
	movq	%rbx, %rdi
	callq	emit_insns_after
	movq	$0, 8(%r14)
.LBB16_27:                              # %for.inc86
                                        #   in Loop: Header=BB16_5 Depth=1
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB16_5
# BB#28:                                # %for.end88
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	96(%rax), %r15
	jmp	.LBB16_29
	.align	16, 0x90
.LBB16_39:                              # %for.inc144
                                        #   in Loop: Header=BB16_29 Depth=1
	movq	(%r15), %r15
.LBB16_29:                              # %for.end88
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_35 Depth 2
	testq	%r15, %r15
	je	.LBB16_40
# BB#30:                                # %for.body93
                                        #   in Loop: Header=BB16_29 Depth=1
	cmpq	$0, 8(%r15)
	je	.LBB16_39
# BB#31:                                # %land.lhs.true96
                                        #   in Loop: Header=BB16_29 Depth=1
	movq	32(%r15), %rax
	cmpq	$0, 16(%rax)
	jne	.LBB16_39
# BB#32:                                # %land.lhs.true96
                                        #   in Loop: Header=BB16_29 Depth=1
	testq	%r13, %r13
	je	.LBB16_39
# BB#33:                                # %land.lhs.true104
                                        #   in Loop: Header=BB16_29 Depth=1
	movslq	32(%r13), %rax
	movslq	40(%r15), %rcx
	cmpq	%rcx, %rax
	jg	.LBB16_39
# BB#34:                                # %if.then107
                                        #   in Loop: Header=BB16_29 Depth=1
	movq	56(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB16_37
	.align	16, 0x90
.LBB16_35:                              # %for.body113
                                        #   Parent Loop BB16_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rbx), %rax
	movq	72(%r13), %rcx
	cmpq	%rcx, %rax
	jne	.LBB16_36
# BB#41:                                # %if.then119
                                        #   in Loop: Header=BB16_35 Depth=2
	callq	start_sequence
	xorq	%rdi, %rdi
	callq	pushlevel
	movq	24(%r15), %rdi
	callq	set_block
	movq	32(%rbx), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	expand_cleanups
	callq	do_pending_stack_adjust
	callq	get_insns
	movq	%rax, %r14
	movq	$1, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	poplevel
	callq	end_sequence
	testq	%r14, %r14
	je	.LBB16_43
# BB#42:                                # %if.then126
                                        #   in Loop: Header=BB16_35 Depth=2
	movq	8(%r15), %rsi
	movq	%r14, %rdi
	callq	emit_insns_after
	movq	%rax, 8(%r15)
.LBB16_43:                              # %if.end130
                                        #   in Loop: Header=BB16_35 Depth=2
	movq	(%rbx), %rax
	movq	%rax, 56(%r15)
	movq	(%rbx), %rax
.LBB16_36:                              # %for.cond111.backedge
                                        #   in Loop: Header=BB16_35 Depth=2
	testq	%rax, %rax
	movq	%rax, %rbx
	jne	.LBB16_35
.LBB16_37:                              # %for.end138
                                        #   in Loop: Header=BB16_29 Depth=1
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB16_39
# BB#38:                                # %if.then140
                                        #   in Loop: Header=BB16_29 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 48(%r15)
	jmp	.LBB16_39
.LBB16_40:                              # %for.end146
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp116:
	.size	fixup_gotos, .Ltmp116-fixup_gotos
	.cfi_endproc

	.globl	expand_asm
	.align	16, 0x90
	.type	expand_asm,@function
expand_asm:                             # @expand_asm
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp118:
	.cfi_def_cfa_offset 16
	movzbl	16(%rdi), %eax
	cmpq	$121, %rax
	jne	.LBB17_2
# BB#1:                                 # %if.then
	movq	32(%rdi), %rdi
.LBB17_2:                               # %if.end
	movq	40(%rdi), %rdx
	movq	$40, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_s
	movq	%rax, %rdi
	callq	emit_insn
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	popq	%rax
	retq
.Ltmp119:
	.size	expand_asm, .Ltmp119-expand_asm
	.cfi_endproc

	.globl	parse_output_constraint
	.align	16, 0x90
	.type	parse_output_constraint,@function
parse_output_constraint:                # @parse_output_constraint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp123:
	.cfi_def_cfa_offset 16
.Ltmp124:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp125:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp126:
	.cfi_offset %rbx, -56
.Ltmp127:
	.cfi_offset %r12, -48
.Ltmp128:
	.cfi_offset %r13, -40
.Ltmp129:
	.cfi_offset %r14, -32
.Ltmp130:
	.cfi_offset %r15, -24
	movq	%r9, %r14
	movq	%r8, %r13
	movq	%rcx, -56(%rbp)         # 8-byte Spill
	movq	%rdx, -80(%rbp)         # 8-byte Spill
	movq	%rsi, -48(%rbp)         # 8-byte Spill
	movq	%rdi, %rbx
	movq	(%rbx), %r15
	xorq	%rax, %rax
	movb	%al, (%r13)
	movb	%al, (%r14)
	movq	$61, %rsi
	movq	%r15, %rdi
	callq	strchr
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB18_4
# BB#1:                                 # %if.end
	movq	$43, %rsi
	movq	%r15, %rdi
	callq	strchr
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB18_2
.LBB18_4:                               # %if.end4
	movq	%rbx, -88(%rbp)         # 8-byte Spill
	movzbl	(%r12), %ecx
	movq	16(%rbp), %rax
	cmpq	$43, %rcx
	jne	.LBB18_5
# BB#6:                                 # %if.end4
	movabsq	$1, %rcx
	jmp	.LBB18_7
.LBB18_5:
	movabsq	$0, %rcx
.LBB18_7:                               # %if.end4
	movq	%r13, -72(%rbp)         # 8-byte Spill
	movq	%r14, -64(%rbp)         # 8-byte Spill
	cmpq	%r15, %r12
	movb	%cl, (%rax)
	jne	.LBB18_9
# BB#8:                                 # %if.end4
	testq	%rax, %rax
	je	.LBB18_12
.LBB18_9:                               # %if.then9
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%r12, %r14
	subq	%r15, %r14
	je	.LBB18_11
# BB#10:                                # %if.then13
	movsbq	(%r12), %rsi
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	movq	-48(%rbp), %rdx         # 8-byte Reload
	callq	warning
.LBB18_11:                              # %if.end15
	movq	%rsp, %r12
	leaq	16(%rbx), %r13
	andq	$-16, %r13
	movq	%rbx, -96(%rbp)         # 8-byte Spill
	movq	%r12, %rbx
	subq	%r13, %rbx
	movq	%rbx, %rsp
	negq	%r13
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movzbl	(%rbx), %eax
	movb	%al, (%rbx,%r14)
	movq	$61, %rax
	movb	%al, (%r12,%r13)
	movq	%rbx, %rdi
	movq	-96(%rbp), %rsi         # 8-byte Reload
	callq	ggc_alloc_string
	movq	%rax, %r15
	movq	-88(%rbp), %rax         # 8-byte Reload
	movq	%r15, (%rax)
.LBB18_12:                              # %for.cond.preheader
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movq	-72(%rbp), %r10         # 8-byte Reload
	movq	-56(%rbp), %r11         # 8-byte Reload
	movq	-48(%rbp), %r14         # 8-byte Reload
	movzbl	1(%r15), %edi
	testq	%rdi, %rdi
	movq	$1, %rbx
	je	.LBB18_40
# BB#13:                                # %for.body.lr.ph
	incq	%r14
	movq	-80(%rbp), %rax         # 8-byte Reload
	addq	%rax, %r11
	addq	$2, %r15
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$1, %r9
	movabsq	$64424968201, %r8       # imm = 0xF00070009
	andq	%rdx, %r11
	andq	%rdx, %r14
	movq	$1, %rbx
	.align	16, 0x90
.LBB18_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$56, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	leaq	-33(%rax), %rcx
	andq	%rdx, %rcx
	cmpq	$82, %rcx
	ja	.LBB18_24
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB18_14 Depth=1
	jmpq	*.LJTI18_0(,%rcx,8)
.LBB18_19:                              # %sw.bb31
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%r10)
	jmp	.LBB18_39
	.align	16, 0x90
.LBB18_24:                              # %sw.default
                                        #   in Loop: Header=BB18_14 Depth=1
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$136, %rax
	je	.LBB18_39
# BB#25:                                # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	movq	$56, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	cmpq	$112, %rax
	jg	.LBB18_31
# BB#26:                                # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	addq	$-65, %rdi
	andq	$255, %rdi
	cmpq	$37, %rdi
	ja	.LBB18_36
# BB#27:                                # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	movq	$1, %rax
	movq	%rdi, %rcx
	shlq	%cl, %rax
	testq	%r8, %rax
	jne	.LBB18_34
# BB#28:                                # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	cmpq	$24, %rdi
	jne	.LBB18_29
# BB#38:                                # %cond.end188
                                        #   in Loop: Header=BB18_14 Depth=1
	movl	target_flags(%rip), %eax
	testq	$262144, %rax           # imm = 0x40000
	movb	%r9b, (%rsi)
	je	.LBB18_37
	jmp	.LBB18_39
.LBB18_31:                              # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	addq	$-113, %rdi
	andq	$255, %rdi
	cmpq	$8, %rdi
	ja	.LBB18_36
# BB#32:                                # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	jmpq	*.LJTI18_1(,%rdi,8)
.LBB18_35:                              # %cond.true121
                                        #   in Loop: Header=BB18_14 Depth=1
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	jne	.LBB18_34
	jmp	.LBB18_36
.LBB18_21:                              # %sw.bb34
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%r10)
	jmp	.LBB18_39
.LBB18_22:                              # %sw.bb35
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%rsi)
	movb	%r9b, (%r10)
	jmp	.LBB18_39
.LBB18_23:                              # %sw.bb36
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%rsi)
	jmp	.LBB18_39
.LBB18_17:                              # %sw.bb24
                                        #   in Loop: Header=BB18_14 Depth=1
	cmpq	%r11, %r14
	jne	.LBB18_39
	jmp	.LBB18_18
.LBB18_33:                              # %cond.true139
                                        #   in Loop: Header=BB18_14 Depth=1
	movzbl	target_flags+1(%rip), %eax
	testq	$64, %rax
	je	.LBB18_36
	jmp	.LBB18_34
.LBB18_29:                              # %if.end43
                                        #   in Loop: Header=BB18_14 Depth=1
	cmpq	$37, %rdi
	jne	.LBB18_36
.LBB18_30:                              # %cond.true67
                                        #   in Loop: Header=BB18_14 Depth=1
	movzbl	target_flags(%rip), %eax
	testq	$33, %rax
	jne	.LBB18_34
.LBB18_36:                              # %cond.end188.thread256
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%rsi)
.LBB18_37:                              # %if.else
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%r10)
	jmp	.LBB18_39
.LBB18_34:                              # %cond.end188.thread
                                        #   in Loop: Header=BB18_14 Depth=1
	movb	%r9b, (%rsi)
	.align	16, 0x90
.LBB18_39:                              # %for.cond.backedge
                                        #   in Loop: Header=BB18_14 Depth=1
	movzbl	(%r15), %edi
	incq	%r15
	testq	%rdi, %rdi
	jne	.LBB18_14
	jmp	.LBB18_40
.LBB18_2:                               # %if.then3
	movabsq	$.L.str1, %rdi
	jmp	.LBB18_3
.LBB18_20:                              # %sw.bb33
	movabsq	$.L.str5, %rdi
.LBB18_3:                               # %return
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	error
.LBB18_40:                              # %return
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB18_16:                              # %sw.bb
	movabsq	$.L.str3, %rdi
	jmp	.LBB18_3
.LBB18_18:                              # %if.then29
	movabsq	$.L.str4, %rdi
	jmp	.LBB18_3
.Ltmp131:
	.size	parse_output_constraint, .Ltmp131-parse_output_constraint
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI18_0:
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_17
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_39
	.quad	.LBB18_16
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_20
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_21
	.quad	.LBB18_16
	.quad	.LBB18_21
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_19
	.quad	.LBB18_24
	.quad	.LBB18_22
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_20
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_22
	.quad	.LBB18_24
	.quad	.LBB18_39
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_24
	.quad	.LBB18_19
	.quad	.LBB18_39
	.quad	.LBB18_19
	.quad	.LBB18_23
	.quad	.LBB18_24
	.quad	.LBB18_23
	.quad	.LBB18_39
.LJTI18_1:
	.quad	.LBB18_34
	.quad	.LBB18_34
	.quad	.LBB18_36
	.quad	.LBB18_30
	.quad	.LBB18_30
	.quad	.LBB18_36
	.quad	.LBB18_36
	.quad	.LBB18_35
	.quad	.LBB18_33

	.text
	.globl	expand_asm_operands
	.align	16, 0x90
	.type	expand_asm_operands,@function
expand_asm_operands:                    # @expand_asm_operands
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp135:
	.cfi_def_cfa_offset 16
.Ltmp136:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp137:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$232, %rsp
.Ltmp138:
	.cfi_offset %rbx, -56
.Ltmp139:
	.cfi_offset %r12, -48
.Ltmp140:
	.cfi_offset %r13, -40
.Ltmp141:
	.cfi_offset %r14, -32
.Ltmp142:
	.cfi_offset %r15, -24
	movq	%r9, -192(%rbp)         # 8-byte Spill
	movq	%r8, -200(%rbp)         # 8-byte Spill
	movq	%rcx, -144(%rbp)        # 8-byte Spill
	movq	%rdx, %r14
	movq	%r14, -104(%rbp)        # 8-byte Spill
	movq	%rsi, %rbx
	movq	%rdi, -168(%rbp)        # 8-byte Spill
	movq	%r14, %rdi
	callq	list_length
	movq	%rax, %r13
	movq	%r13, -136(%rbp)        # 8-byte Spill
	movq	%rbx, %rdi
	callq	list_length
	movq	%rax, %r12
	movq	$32, %rcx
                                        # kill: RAX<def> R12<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rcx
	andq	$-16, %rcx
	movq	%rsp, %r15
	subq	%rcx, %r15
	movq	%r15, %rsp
	leaq	15(,%rax,4), %rax
	andq	$-16, %rax
	movq	%rsp, %rdx
	subq	%rax, %rdx
	movq	%rdx, -152(%rbp)        # 8-byte Spill
	movq	%rdx, %rsp
                                        # kill: RDX<def> RSP<kill>
	subq	%rcx, %rdx
	movq	%rdx, -128(%rbp)        # 8-byte Spill
	movq	%rdx, %rsp
	movq	%rsp, %rcx
	subq	%rax, %rcx
	movq	%rcx, -160(%rbp)        # 8-byte Spill
	movq	%rcx, %rsp
	movq	%r12, %rax
	addq	%r13, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	15(,%rax,8), %rax
	andq	$-16, %rax
	movq	%rsp, %rcx
	subq	%rax, %rcx
	movq	%rcx, -112(%rbp)        # 8-byte Spill
	movq	%rcx, %rsp
	movq	%rbx, %rax
	movq	%rbx, %r13
	orq	%r14, %rax
	je	.LBB19_25
# BB#1:                                 # %if.then.i
	testq	%r13, %r13
	movq	%r13, %rax
	jne	.LBB19_3
# BB#2:                                 # %if.then.i
	movq	-104(%rbp), %rax        # 8-byte Reload
.LBB19_3:                               # %if.then.i
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %rcx
	movzbl	(%rcx), %esi
	xorq	%rax, %rax
	testq	%rsi, %rsi
	je	.LBB19_9
# BB#4:                                 # %while.body.i.i.preheader
	incq	%rcx
	xorq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$1, %rdi
	.align	16, 0x90
.LBB19_5:                               # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rsi
	cmpq	$44, %rsi
	movq	%rbx, %rdx
	jne	.LBB19_7
# BB#6:                                 # %while.body.i.i
                                        #   in Loop: Header=BB19_5 Depth=1
	movq	%rdi, %rdx
.LBB19_7:                               # %while.body.i.i
                                        #   in Loop: Header=BB19_5 Depth=1
	movzbl	(%rcx), %esi
	addq	%rdx, %rax
	incq	%rcx
	testq	%rsi, %rsi
	jne	.LBB19_5
# BB#8:                                 # %n_occurrences.exit.i
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$30, %rdx
	jl	.LBB19_9
# BB#18:                                # %if.then6.i
	movabsq	$.L.str25, %rdi
	jmp	.LBB19_19
.LBB19_9:                               # %while.cond.preheader.i
	movq	$1, %rbx
	testq	%r13, %r13
	je	.LBB19_31
# BB#10:
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rax
	movabsq	$0, %rdi
	movabsq	$1, %r9
	movq	-104(%rbp), %r10        # 8-byte Reload
	movq	%r13, %r11
	.align	16, 0x90
.LBB19_11:                              # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_13 Depth 2
	movq	24(%r11), %rcx
	movq	32(%rcx), %rcx
	movq	40(%rcx), %rcx
	movzbl	(%rcx), %ebx
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	je	.LBB19_16
# BB#12:                                # %while.body.i43.i.preheader
                                        #   in Loop: Header=BB19_11 Depth=1
	incq	%rcx
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB19_13:                              # %while.body.i43.i
                                        #   Parent Loop BB19_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	$255, %rbx
	cmpq	$44, %rbx
	movq	%rdi, %rdx
	jne	.LBB19_15
# BB#14:                                # %while.body.i43.i
                                        #   in Loop: Header=BB19_13 Depth=2
	movq	%r9, %rdx
.LBB19_15:                              # %while.body.i43.i
                                        #   in Loop: Header=BB19_13 Depth=2
	movzbl	(%rcx), %ebx
	addq	%rdx, %rsi
	incq	%rcx
	testq	%rbx, %rbx
	jne	.LBB19_13
.LBB19_16:                              # %n_occurrences.exit45.i
                                        #   in Loop: Header=BB19_11 Depth=1
	andq	%r8, %rsi
	cmpq	%rax, %rsi
	jne	.LBB19_17
# BB#20:                                # %if.end18.i
                                        #   in Loop: Header=BB19_11 Depth=1
	movq	(%r11), %rcx
	testq	%rcx, %rcx
	movq	%r10, %r11
	je	.LBB19_22
# BB#21:                                # %if.end18.i
                                        #   in Loop: Header=BB19_11 Depth=1
	movq	%rcx, %r11
.LBB19_22:                              # %if.end18.i
                                        #   in Loop: Header=BB19_11 Depth=1
	je	.LBB19_24
# BB#23:                                # %if.end18.i
                                        #   in Loop: Header=BB19_11 Depth=1
	movq	%r10, %rcx
.LBB19_24:                              # %if.end18.i
                                        #   in Loop: Header=BB19_11 Depth=1
	testq	%r11, %r11
	movq	%rcx, %r10
	jne	.LBB19_11
.LBB19_25:                              # %if.end13
	movq	$1, %rbx
	testq	%r13, %r13
	je	.LBB19_31
# BB#26:
	movq	%r13, %rcx
	xorq	%rbx, %rbx
.LBB19_27:                              # %for.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_28 Depth 2
	movq	24(%rcx), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rcx, %rdx
	je	.LBB19_30
	.align	16, 0x90
.LBB19_28:                              # %for.cond4.i
                                        #   Parent Loop BB19_27 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB19_30
# BB#29:                                # %for.body6.i
                                        #   in Loop: Header=BB19_28 Depth=2
	movq	24(%rdx), %rsi
	movq	24(%rsi), %rsi
	cmpq	%rsi, %rax
	jne	.LBB19_28
	jmp	.LBB19_55
	.align	16, 0x90
.LBB19_30:                              # %for.inc15.i
                                        #   in Loop: Header=BB19_27 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_27
.LBB19_31:                              # %for.cond19.preheader.i
	cmpq	$0, -104(%rbp)          # 8-byte Folded Reload
	je	.LBB19_39
# BB#32:
	movq	-104(%rbp), %rcx        # 8-byte Reload
.LBB19_33:                              # %for.body21.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_34 Depth 2
                                        #     Child Loop BB19_36 Depth 2
	movq	24(%rcx), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	movq	%rcx, %rdx
	je	.LBB19_38
	.align	16, 0x90
.LBB19_34:                              # %for.cond32.i
                                        #   Parent Loop BB19_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	je	.LBB19_35
# BB#54:                                # %for.body34.i
                                        #   in Loop: Header=BB19_34 Depth=2
	movq	24(%rdx), %rsi
	movq	24(%rsi), %rsi
	cmpq	%rsi, %rax
	jne	.LBB19_34
	jmp	.LBB19_55
.LBB19_35:                              # %for.cond46.preheader.i
                                        #   in Loop: Header=BB19_33 Depth=1
	testq	%rbx, %rbx
	movq	%r13, %rdx
	jne	.LBB19_38
	.align	16, 0x90
.LBB19_36:                              # %for.body48.i
                                        #   Parent Loop BB19_33 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdx), %rsi
	movq	24(%rsi), %rsi
	cmpq	%rsi, %rax
	je	.LBB19_55
# BB#37:                                # %for.inc56.i
                                        #   in Loop: Header=BB19_36 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB19_36
	.align	16, 0x90
.LBB19_38:                              # %for.inc60.i
                                        #   in Loop: Header=BB19_33 Depth=1
	movq	(%rcx), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_33
.LBB19_39:                              # %if.end16
	movq	-168(%rbp), %rax        # 8-byte Reload
	movq	40(%rax), %rdi
	callq	xstrdup
	movq	%rax, %r14
	movq	$37, %rsi
	movq	%r14, %rdi
	jmp	.LBB19_40
.LBB19_55:                              # %check_unique_operand_names.exit
	movq	32(%rax), %rsi
	movabsq	$.L.str24, %rdi
.LBB19_97:                              # %return
	xorq	%rax, %rax
	callq	error
	jmp	.LBB19_204
	.align	16, 0x90
.LBB19_46:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB19_40 Depth=1
	movq	$37, %rsi
.LBB19_40:                              # %if.end16
                                        # =>This Inner Loop Header: Depth=1
	callq	strchr
	testq	%rax, %rax
	je	.LBB19_47
# BB#41:                                # %while.body.i1020
                                        #   in Loop: Header=BB19_40 Depth=1
	movzbl	1(%rax), %ecx
	leaq	1(%rax), %rdi
	cmpq	$91, %rcx
	je	.LBB19_45
# BB#42:                                # %if.else.i
                                        #   in Loop: Header=BB19_40 Depth=1
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	testq	$136, %rcx
	je	.LBB19_46
# BB#43:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB19_40 Depth=1
	movzbl	2(%rax), %ecx
	cmpq	$91, %rcx
	jne	.LBB19_46
# BB#44:                                #   in Loop: Header=BB19_40 Depth=1
	addq	$2, %rax
	movq	%rax, %rdi
.LBB19_45:                              # %if.end18.i1022
                                        #   in Loop: Header=BB19_40 Depth=1
	movq	%r13, %rsi
	movq	-104(%rbp), %rdx        # 8-byte Reload
	callq	resolve_operand_name_1
	movq	%rax, %rdi
	jmp	.LBB19_46
.LBB19_47:                              # %while.end.i
	movq	%r15, -184(%rbp)        # 8-byte Spill
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	build_string
	movq	%rax, -216(%rbp)        # 8-byte Spill
	movq	%r14, %rdi
	callq	free
	testq	%rbx, %rbx
	movq	-112(%rbp), %r14        # 8-byte Reload
	jne	.LBB19_50
# BB#48:
	movq	%r13, %rax
	movq	-112(%rbp), %r14        # 8-byte Reload
	.align	16, 0x90
.LBB19_49:                              # %for.body.i1027
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	32(%rcx), %rcx
	movq	40(%rcx), %rcx
	movq	%rcx, (%r14)
	movq	(%rax), %rax
	addq	$8, %r14
	testq	%rax, %rax
	jne	.LBB19_49
.LBB19_50:                              # %for.cond27.preheader.i
	movq	%r12, -120(%rbp)        # 8-byte Spill
	cmpq	$0, -104(%rbp)          # 8-byte Folded Reload
	je	.LBB19_61
# BB#51:
	movq	-104(%rbp), %r12        # 8-byte Reload
	movq	-104(%rbp), %rbx        # 8-byte Reload
	.align	16, 0x90
.LBB19_52:                              # %for.body29.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_58 Depth 2
	movq	24(%r12), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %r15
	movq	$91, %rsi
	movq	%r15, %rdi
	callq	strchr
	testq	%rax, %rax
	jne	.LBB19_56
# BB#53:                                # %if.then39.i
                                        #   in Loop: Header=BB19_52 Depth=1
	movq	%r15, (%r14)
	jmp	.LBB19_60
	.align	16, 0x90
.LBB19_56:                              # %if.else40.i
                                        #   in Loop: Header=BB19_52 Depth=1
	movq	%r15, %rdi
	callq	xstrdup
	movq	%rax, %r15
	movq	$91, %rsi
	movq	%r15, %rdi
	jmp	.LBB19_58
	.align	16, 0x90
.LBB19_57:                              # %while.body46.i
                                        #   in Loop: Header=BB19_58 Depth=2
	movq	%rax, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	resolve_operand_name_1
	movq	$91, %rsi
	movq	%rax, %rdi
.LBB19_58:                              # %while.body46.i
                                        #   Parent Loop BB19_52 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	callq	strchr
	testq	%rax, %rax
	jne	.LBB19_57
# BB#59:                                # %while.end48.i
                                        #   in Loop: Header=BB19_52 Depth=1
	movq	%r15, %rdi
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	callq	ggc_alloc_string
	movq	%rax, (%r14)
	movq	%r15, %rdi
	callq	free
.LBB19_60:                              # %for.inc51.i
                                        #   in Loop: Header=BB19_52 Depth=1
	movq	(%r12), %r12
	addq	$8, %r14
	testq	%r12, %r12
	jne	.LBB19_52
.LBB19_61:                              # %resolve_operand_names.exit
	movabsq	$.L.str6, %rsi
	movq	$5, %rdi
	callq	build_string
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	-144(%rbp), %rdx        # 8-byte Reload
	callq	tree_cons
	movq	%rax, %rbx
	movabsq	$.L.str7, %rsi
	movq	$4, %rdi
	callq	build_string
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, %rbx
	movabsq	$.L.str8, %rsi
	movq	$7, %rdi
	callq	build_string
	xorq	%rdi, %rdi
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	tree_cons
	movq	%rax, -224(%rbp)        # 8-byte Spill
	testq	%rax, %rax
	movabsq	$0, %rax
	movq	%rax, -144(%rbp)        # 8-byte Spill
	je	.LBB19_69
# BB#62:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movabsq	$4294967292, %r12       # imm = 0xFFFFFFFC
	movq	$0, -144(%rbp)          # 8-byte Folded Spill
	movq	-224(%rbp), %r14        # 8-byte Reload
	.align	16, 0x90
.LBB19_63:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rax
	movq	40(%rax), %rbx
	movq	%rbx, %rdi
	callq	decode_reg_name
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB19_65
# BB#64:                                # %for.body
                                        #   in Loop: Header=BB19_63 Depth=1
	andq	%r15, %rax
	cmpq	%r12, %rax
	je	.LBB19_65
# BB#66:                                # %if.else
                                        #   in Loop: Header=BB19_63 Depth=1
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	cmpq	%rcx, %rax
	jne	.LBB19_68
# BB#67:                                # %if.then33
                                        #   in Loop: Header=BB19_63 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str9, %rdi
	movq	%rbx, %rsi
	callq	error
	jmp	.LBB19_68
	.align	16, 0x90
.LBB19_65:                              # %if.then30
                                        #   in Loop: Header=BB19_63 Depth=1
	incq	-144(%rbp)              # 8-byte Folded Spill
.LBB19_68:                              # %for.inc
                                        #   in Loop: Header=BB19_63 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB19_63
.LBB19_69:                              # %for.end
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	testq	%r13, %r13
	je	.LBB19_70
# BB#71:
	movq	$0, -168(%rbp)          # 8-byte Folded Spill
	movq	%r13, %r14
	movq	%r13, -232(%rbp)        # 8-byte Spill
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB19_72:                              # %for.body38
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %r15
	movq	8(%r15), %r12
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r12
	je	.LBB19_204
# BB#73:                                # %if.end46
                                        #   in Loop: Header=BB19_72 Depth=1
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rbx,8), %rax
	movq	%rax, -48(%rbp)
	subq	$16, %rsp
	leaq	-49(%rbp), %rax
	movq	%rax, (%rsp)
	leaq	-48(%rbp), %rdi
	movq	%rbx, %rsi
	movq	-136(%rbp), %rdx        # 8-byte Reload
	movq	-120(%rbp), %rcx        # 8-byte Reload
	leaq	-51(%rbp), %r8
	leaq	-50(%rbp), %r9
	callq	parse_output_constraint
	addq	$16, %rsp
	testq	%rax, %rax
	je	.LBB19_204
# BB#74:                                # %if.end49
                                        #   in Loop: Header=BB19_72 Depth=1
	movzbl	-50(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB19_84
# BB#75:                                # %land.lhs.true
                                        #   in Loop: Header=BB19_72 Depth=1
	movzbl	-51(%rbp), %eax
	movzbl	-49(%rbp), %ecx
	orq	%rax, %rcx
	testq	$255, %rcx
	jne	.LBB19_83
# BB#76:                                # %lor.lhs.false56
                                        #   in Loop: Header=BB19_72 Depth=1
	movzbl	16(%r15), %eax
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB19_84
# BB#77:                                # %land.lhs.true63
                                        #   in Loop: Header=BB19_72 Depth=1
	movq	144(%r15), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_79
# BB#78:                                # %cond.false
                                        #   in Loop: Header=BB19_72 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %rcx
.LBB19_79:                              # %cond.end
                                        #   in Loop: Header=BB19_72 Depth=1
	movl	(%rcx), %eax
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB19_84
# BB#80:                                # %land.lhs.true73
                                        #   in Loop: Header=BB19_72 Depth=1
	testq	%rcx, %rcx
	jne	.LBB19_82
# BB#81:                                # %cond.false80
                                        #   in Loop: Header=BB19_72 Depth=1
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %rax
	movl	(%rax), %eax
.LBB19_82:                              # %cond.end83
                                        #   in Loop: Header=BB19_72 Depth=1
	movq	$16, %rcx
	movl	60(%r12), %edx
	shrq	%cl, %rax
	andq	$255, %rax
	movq	$9, %rcx
	shrq	%cl, %rdx
	andq	$127, %rdx
	cmpq	%rdx, %rax
	je	.LBB19_84
	.align	16, 0x90
.LBB19_83:                              # %if.then93
                                        #   in Loop: Header=BB19_72 Depth=1
	movq	%r15, %rdi
	callq	mark_addressable
.LBB19_84:                              # %if.end95
                                        #   in Loop: Header=BB19_72 Depth=1
	movzbl	-49(%rbp), %eax
	movq	(%r14), %r14
	addq	%rax, -168(%rbp)        # 8-byte Folded Spill
	incq	%rbx
	testq	%r14, %r14
	jne	.LBB19_72
	jmp	.LBB19_85
.LBB19_17:                              # %if.then17.i
	movabsq	$.L.str26, %rdi
.LBB19_19:                              # %return
	xorq	%rax, %rax
	callq	error
.LBB19_204:                             # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB19_70:
	movq	%r13, -232(%rbp)        # 8-byte Spill
	movq	$0, -168(%rbp)          # 8-byte Folded Spill
.LBB19_85:                              # %for.end104
	movq	-136(%rbp), %rax        # 8-byte Reload
	movq	-168(%rbp), %rcx        # 8-byte Reload
	leaq	(%rcx,%rax), %rdx
	movq	-120(%rbp), %rax        # 8-byte Reload
	leaq	(%rdx,%rax), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$30, %rax
	jle	.LBB19_86
# BB#96:                                # %if.then109
	movabsq	$.L.str10, %rdi
	movq	$30, %rsi
	jmp	.LBB19_97
.LBB19_86:                              # %for.cond111.preheader
	movq	%rdx, -176(%rbp)        # 8-byte Spill
	cmpq	$0, -104(%rbp)          # 8-byte Folded Reload
	je	.LBB19_94
# BB#87:                                # %for.body113.preheader
	movq	$32, %rcx
	movq	-120(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	-112(%rbp), %rcx        # 8-byte Reload
	leaq	(%rcx,%rax,8), %r12
	xorq	%rbx, %rbx
	leaq	-52(%rbp), %r14
	leaq	-64(%rbp), %r15
	movq	-104(%rbp), %r13        # 8-byte Reload
	.align	16, 0x90
.LBB19_88:                              # %for.body113
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movq	global_trees(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB19_204
# BB#89:                                # %if.end124
                                        #   in Loop: Header=BB19_88 Depth=1
	movq	(%r12,%rbx,8), %rax
	movq	%rax, -64(%rbp)
	subq	$16, %rsp
	movq	%r14, 8(%rsp)
	leaq	-53(%rbp), %rax
	movq	%rax, (%rsp)
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	-176(%rbp), %rdx        # 8-byte Reload
	movq	-120(%rbp), %rcx        # 8-byte Reload
	movq	-168(%rbp), %r8         # 8-byte Reload
	movq	-112(%rbp), %r9         # 8-byte Reload
	callq	parse_input_constraint
	addq	$16, %rsp
	testq	%rax, %rax
	je	.LBB19_204
# BB#90:                                # %if.end130
                                        #   in Loop: Header=BB19_88 Depth=1
	movzbl	-52(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB19_93
# BB#91:                                # %if.end130
                                        #   in Loop: Header=BB19_88 Depth=1
	movzbl	-53(%rbp), %eax
	testq	$255, %rax
	je	.LBB19_93
# BB#92:                                # %if.then135
                                        #   in Loop: Header=BB19_88 Depth=1
	movq	32(%r13), %rdi
	callq	mark_addressable
.LBB19_93:                              # %for.inc140
                                        #   in Loop: Header=BB19_88 Depth=1
	movq	(%r13), %r13
	incq	%rbx
	testq	%r13, %r13
	jne	.LBB19_88
.LBB19_94:                              # %for.cond145.preheader
	xorq	%r15, %r15
	movq	-232(%rbp), %rax        # 8-byte Reload
	testq	%rax, %rax
	je	.LBB19_95
# BB#98:
	movq	-112(%rbp), %rdi        # 8-byte Reload
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	movq	%rax, %r14
	movq	-120(%rbp), %r13        # 8-byte Reload
	movq	-176(%rbp), %rbx        # 8-byte Reload
	.align	16, 0x90
.LBB19_99:                              # %for.body147
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, -168(%rbp)        # 8-byte Spill
	movq	32(%r14), %r12
	movq	8(%r12), %rax
	movq	%rax, -208(%rbp)        # 8-byte Spill
	subq	$16, %rsp
	leaq	-65(%rbp), %rax
	movq	%rax, (%rsp)
	movq	%r15, %rsi
	movq	%rbx, %rdx
	movq	%r13, %rcx
	leaq	-67(%rbp), %r8
	leaq	-66(%rbp), %r9
	callq	parse_output_constraint
	addq	$16, %rsp
	testq	%rax, %rax
	je	.LBB19_100
# BB#102:                               # %if.end161
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rax, %rax
	movl	%eax, generating_concat_p(%rip)
	movq	-128(%rbp), %rax        # 8-byte Reload
	movq	$0, (%rax,%r15,8)
	movzbl	16(%r12), %eax
	movzbl	-67(%rbp), %r13d
	cmpq	$41, %rax
	jne	.LBB19_104
# BB#103:                               # %if.end161
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	$255, %r13
	jne	.LBB19_118
.LBB19_104:                             # %lor.lhs.false173
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	tree_code_type(%rax), %eax
	cmpq	$100, %rax
	jne	.LBB19_116
# BB#105:                               # %land.lhs.true183
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	144(%r12), %rax
	testq	$255, %r13
	jne	.LBB19_109
# BB#106:                               # %lor.lhs.false186
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	%rax, %rax
	jne	.LBB19_108
# BB#107:                               # %cond.false193
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB19_108:                             # %cond.end196
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	(%rax), %ecx
	cmpq	$61, %rcx
	jne	.LBB19_116
.LBB19_109:                             # %land.lhs.true202
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	%rax, %rax
	jne	.LBB19_111
# BB#110:                               # %cond.false209
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
.LBB19_111:                             # %cond.end212
                                        #   in Loop: Header=BB19_99 Depth=1
	movl	(%rax), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB19_118
# BB#112:                               # %land.lhs.true218
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	%rax, %rax
	jne	.LBB19_114
# BB#113:                               # %cond.false225
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %rax
	movl	(%rax), %edx
.LBB19_114:                             # %cond.end228
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	$16, %rcx
	movq	-208(%rbp), %rax        # 8-byte Reload
	movl	60(%rax), %eax
	movzbl	-66(%rbp), %esi
	shrq	%cl, %rdx
	movq	$9, %rcx
	shrq	%cl, %rax
	testq	%rsi, %rsi
	je	.LBB19_118
# BB#115:                               # %cond.end228
                                        #   in Loop: Header=BB19_99 Depth=1
	andq	$255, %rdx
	andq	$127, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rax, %rdx
	jne	.LBB19_117
	jmp	.LBB19_118
.LBB19_116:                             # %lor.lhs.false240
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	-66(%rbp), %eax
	testq	%rax, %rax
	je	.LBB19_118
.LBB19_117:                             # %lor.lhs.false240
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	-65(%rbp), %eax
	testq	$255, %rax
	jne	.LBB19_118
# BB#130:                               # %if.else297
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	-208(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rdi
	callq	assign_temp
	movq	-184(%rbp), %rcx        # 8-byte Reload
	movq	%rax, (%rcx,%r15,8)
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	make_tree
	movq	%rax, 32(%r14)
	jmp	.LBB19_131
	.align	16, 0x90
.LBB19_118:                             # %if.then245
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$4, %rcx
	movq	%r12, %rdi
	callq	expand_expr
	movzbl	-66(%rbp), %ecx
	movq	%rax, %rbx
	movq	-184(%rbp), %r12        # 8-byte Reload
	movq	%rbx, (%r12,%r15,8)
	testq	%rcx, %rcx
	jne	.LBB19_121
# BB#119:                               # %land.lhs.true250
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$66, %rax
	je	.LBB19_121
# BB#120:                               # %if.then257
                                        #   in Loop: Header=BB19_99 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str11, %rdi
	movq	%r15, %rsi
	callq	error
.LBB19_121:                             # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	$255, %r13
	movabsq	$0, %rax
	jne	.LBB19_123
# BB#122:                               # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	movabsq	$1, %rax
.LBB19_123:                             # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$66, %rcx
	movabsq	$0, %rdx
	jne	.LBB19_125
# BB#124:                               # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	movabsq	$1, %rdx
.LBB19_125:                             # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	testq	%rdx, %rax
	jne	.LBB19_127
# BB#126:                               # %if.end258
                                        #   in Loop: Header=BB19_99 Depth=1
	cmpq	$65, %rcx
	jne	.LBB19_131
.LBB19_127:                             # %if.then274
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	$1, %rsi
	movq	%rbx, %rdi
	callq	protect_from_queue
	movq	%rax, %r13
	movq	-128(%rbp), %rax        # 8-byte Reload
	movq	%r13, (%rax,%r15,8)
	movzbl	2(%rbx), %edi
	callq	gen_reg_rtx
	movzbl	-65(%rbp), %ecx
	movq	%rax, (%r12,%r15,8)
	testq	%rcx, %rcx
	jne	.LBB19_129
# BB#128:                               # %if.end306.thread
                                        #   in Loop: Header=BB19_99 Depth=1
	movl	generating_concat_p(%rip), %eax
	movl	%eax, generating_concat_p(%rip)
	movq	-120(%rbp), %r13        # 8-byte Reload
	jmp	.LBB19_133
.LBB19_129:                             # %if.then289
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	%rax, %rdi
	movq	%r13, %rsi
	callq	emit_move_insn
.LBB19_131:                             # %if.end306
                                        #   in Loop: Header=BB19_99 Depth=1
	movzbl	-65(%rbp), %eax
	testq	%rax, %rax
	movl	generating_concat_p(%rip), %eax
	movl	%eax, generating_concat_p(%rip)
	movq	-120(%rbp), %r13        # 8-byte Reload
	je	.LBB19_133
# BB#132:                               # %if.then308
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	-208(%rbp), %rax        # 8-byte Reload
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movq	$32, %rcx
	movq	-136(%rbp), %rsi        # 8-byte Reload
	movq	%rsi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	-160(%rbp), %rcx        # 8-byte Reload
	movl	%eax, (%rcx,%rdx,4)
	incq	%rsi
	movq	%rsi, -136(%rbp)        # 8-byte Spill
	movq	-152(%rbp), %rax        # 8-byte Reload
	movl	%r15d, (%rax,%rdx,4)
.LBB19_133:                             # %for.inc320
                                        #   in Loop: Header=BB19_99 Depth=1
	movq	-176(%rbp), %rbx        # 8-byte Reload
	movq	-168(%rbp), %rdi        # 8-byte Reload
	movq	(%r14), %r14
	incq	%r15
	addq	$8, %rdi
	testq	%r14, %r14
	jne	.LBB19_99
	jmp	.LBB19_134
.LBB19_95:
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	movq	-120(%rbp), %r13        # 8-byte Reload
	movq	-176(%rbp), %rbx        # 8-byte Reload
.LBB19_134:                             # %for.end324
	movq	%rbx, %rdi
	callq	rtvec_alloc
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	rtvec_alloc
	movq	%rax, %r9
	xorq	%rsi, %rsi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	movq	-184(%rbp), %rax        # 8-byte Reload
	je	.LBB19_136
# BB#135:                               # %cond.false330
	movq	(%rax), %rax
	movzbl	2(%rax), %esi
.LBB19_136:                             # %cond.end335
	movq	%rbx, -176(%rbp)        # 8-byte Spill
	movq	-216(%rbp), %rax        # 8-byte Reload
	movq	40(%rax), %rdx
	subq	$32, %rsp
	movq	16(%rbp), %rax
	movq	%rax, 16(%rsp)
	movq	-192(%rbp), %rax        # 8-byte Reload
	movq	%rax, 8(%rsp)
	movabsq	$empty_string, %rcx
	movq	$41, %rdi
	xorq	%r8, %r8
	movq	%r9, (%rsp)
	movq	%r9, -240(%rbp)         # 8-byte Spill
	movq	%r14, %r9
	callq	gen_rtx_fmt_ssiEEsi
	addq	$32, %rsp
	movq	%rax, -168(%rbp)        # 8-byte Spill
	movl	(%rax), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r13
	movq	%r13, -120(%rbp)        # 8-byte Spill
	movq	$134217728, %rcx        # imm = 0x8000000
	movq	-136(%rbp), %r15        # 8-byte Reload
	je	.LBB19_138
# BB#137:                               # %cond.end335
	movq	-200(%rbp), %rcx        # 8-byte Reload
	andq	$1, %rcx
	shlq	$27, %rcx
.LBB19_138:                             # %cond.end335
	movq	%rcx, -248(%rbp)        # 8-byte Spill
	movq	%r14, -208(%rbp)        # 8-byte Spill
	movabsq	$4160749567, %rdx       # imm = 0xF7FFFFFF
	andq	%rdx, %rax
	orq	%rcx, %rax
	cmpq	$0, -104(%rbp)          # 8-byte Folded Reload
	movq	-168(%rbp), %rcx        # 8-byte Reload
	movl	%eax, (%rcx)
	je	.LBB19_168
# BB#139:                               # %for.body344.lr.ph
	movq	$32, %rcx
	movq	-120(%rbp), %rdx        # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, -256(%rbp)        # 8-byte Spill
	movq	-112(%rbp), %rax        # 8-byte Reload
	leaq	(%rax,%rdx,8), %rax
	movq	%rax, -200(%rbp)        # 8-byte Spill
	xorq	%r14, %r14
	.align	16, 0x90
.LBB19_140:                             # %for.body344
                                        # =>This Inner Loop Header: Depth=1
	movq	-200(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%r14,8), %rbx
	movq	%rbx, -80(%rbp)
	subq	$16, %rsp
	leaq	-68(%rbp), %rax
	movq	%rax, 8(%rsp)
	leaq	-69(%rbp), %rax
	movq	%rax, (%rsp)
	leaq	-80(%rbp), %rdi
	movq	%r14, %rsi
	movq	-176(%rbp), %rdx        # 8-byte Reload
	movq	-120(%rbp), %rcx        # 8-byte Reload
	movq	%r15, %r8
	movq	-112(%rbp), %r9         # 8-byte Reload
	callq	parse_input_constraint
	addq	$16, %rsp
	testq	%rax, %rax
	je	.LBB19_141
# BB#142:                               # %if.end355
                                        #   in Loop: Header=BB19_140 Depth=1
	xorq	%rax, %rax
	movl	%eax, generating_concat_p(%rip)
	movq	-104(%rbp), %rax        # 8-byte Reload
	movq	32(%rax), %rdi
	movq	8(%rdi), %r13
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, %r15
	movl	(%r15), %edi
	movq	%rdi, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$65, %rax
	jne	.LBB19_144
# BB#143:                               # %if.then365
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdi
	andq	$255, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
.LBB19_144:                             # %if.end370
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	-80(%rbp), %rsi
	movq	%r15, %rdi
	callq	asm_operand_ok
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB19_145
# BB#146:                               # %if.then374
                                        #   in Loop: Header=BB19_140 Depth=1
	movzbl	-68(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB19_147
# BB#148:                               # %if.else383
                                        #   in Loop: Header=BB19_140 Depth=1
	movzbl	-69(%rbp), %eax
	movq	-256(%rbp), %rcx        # 8-byte Reload
	leaq	(%rcx,%r14), %rsi
	testq	%rax, %rax
	jne	.LBB19_150
# BB#149:                               # %if.then385
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	%r13, %r12
	xorq	%rax, %rax
	movabsq	$.L.str12, %rdi
.LBB19_166:                             # %if.end509
                                        #   in Loop: Header=BB19_140 Depth=1
	callq	warning
	jmp	.LBB19_167
	.align	16, 0x90
.LBB19_145:                             #   in Loop: Header=BB19_140 Depth=1
	movq	%r13, %r12
	jmp	.LBB19_167
.LBB19_147:                             # %if.then376
                                        #   in Loop: Header=BB19_140 Depth=1
	movl	60(%r13), %edi
	movq	%r13, %r12
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	%r15, %rsi
	callq	force_reg
	movq	%rax, %r15
	jmp	.LBB19_167
.LBB19_150:                             # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	%rsi, -264(%rbp)        # 8-byte Spill
	movl	(%r15), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$133, %rcx
	jg	.LBB19_157
# BB#151:                               # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	addq	$-54, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$16, %rcx
	ja	.LBB19_164
# BB#152:                               # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24599, %rdx            # imm = 0x6017
	jne	.LBB19_159
# BB#153:                               # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$68224, %rdx            # imm = 0x10A80
	je	.LBB19_154
# BB#160:                               # %if.then454
                                        #   in Loop: Header=BB19_140 Depth=1
	movl	16(%r13), %edi
	movq	$10, %rcx
	movq	%rdi, %rax
	shrq	%cl, %rax
	movl	60(%r13), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rdi
	xorq	%rdx, %rdx
	cmpq	$20, %rdi
	jne	.LBB19_163
# BB#161:                               # %land.rhs
                                        #   in Loop: Header=BB19_140 Depth=1
	cmpq	$0, 8(%r13)
	movabsq	$0, %rdx
	je	.LBB19_163
# BB#162:                               # %land.rhs
                                        #   in Loop: Header=BB19_140 Depth=1
	movabsq	$1, %rdx
.LBB19_163:                             # %land.end
                                        #   in Loop: Header=BB19_140 Depth=1
	andq	$2, %rax
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rsi
	orq	%rdx, %rsi
	orq	$1, %rsi
	movq	%r13, %rdi
	movq	%r13, %r12
	callq	build_qualified_type
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
	movq	%r13, %r15
	jmp	.LBB19_167
.LBB19_157:                             # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	cmpq	$134, %rcx
	je	.LBB19_159
# BB#158:                               # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	cmpq	$140, %rcx
	jne	.LBB19_164
.LBB19_159:                             # %if.then427
                                        #   in Loop: Header=BB19_140 Depth=1
	movl	60(%r13), %edi
	movq	%r13, %r12
	movq	$9, %rcx
	shrq	%cl, %rdi
	andq	$127, %rdi
	movq	%r15, %rsi
	callq	force_const_mem
	movq	%rax, %r15
	jmp	.LBB19_167
.LBB19_154:                             # %if.else387
                                        #   in Loop: Header=BB19_140 Depth=1
	cmpq	$12, %rcx
	jne	.LBB19_164
# BB#155:                               # %land.lhs.true491
                                        #   in Loop: Header=BB19_140 Depth=1
	testq	$134217728, %rax        # imm = 0x8000000
	jne	.LBB19_156
.LBB19_164:                             # %if.else497
                                        #   in Loop: Header=BB19_140 Depth=1
	movq	%r13, %r12
	movq	%r15, %rdi
	callq	queued_subexp_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_167
# BB#165:                               # %if.else501
                                        #   in Loop: Header=BB19_140 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str12, %rdi
	movq	-264(%rbp), %rsi        # 8-byte Reload
	jmp	.LBB19_166
.LBB19_156:                             #   in Loop: Header=BB19_140 Depth=1
	movq	%r13, %r12
	.align	16, 0x90
.LBB19_167:                             # %if.end509
                                        #   in Loop: Header=BB19_140 Depth=1
	movl	generating_concat_p(%rip), %eax
	movl	%eax, generating_concat_p(%rip)
	movq	-168(%rbp), %r13        # 8-byte Reload
	movq	32(%r13), %rax
	movq	%r15, 8(%rax,%r14,8)
	movl	60(%r12), %esi
	movq	$9, %rcx
	shrq	%cl, %rsi
	andq	$127, %rsi
	movq	$40, %rdi
	movq	%rbx, %rdx
	callq	gen_rtx_fmt_s
	movq	40(%r13), %rcx
	movq	%rax, 8(%rcx,%r14,8)
	movq	-104(%rbp), %rax        # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, -104(%rbp)        # 8-byte Spill
	incq	%r14
	testq	%rax, %rax
	movq	-136(%rbp), %r15        # 8-byte Reload
	jne	.LBB19_140
.LBB19_168:                             # %for.end532
	xorq	%rax, %rax
	movl	%eax, generating_concat_p(%rip)
	movq	-176(%rbp), %r12        # 8-byte Reload
	subq	%r15, %r12
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r13, %rsi
	movq	-168(%rbp), %r13        # 8-byte Reload
	jle	.LBB19_171
# BB#169:                               # %for.body536.lr.ph
	movq	$1, %rbx
	movq	%r12, %rax
	andq	%rsi, %rax
	movq	%rax, -104(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB19_170:                             # %for.body536
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rax
	movq	(%rax,%rbx,8), %rdi
	movq	%rsi, %r14
	xorq	%rsi, %rsi
	callq	protect_from_queue
	movq	%r14, %rsi
	movq	32(%r13), %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	%rbx, %rax
	andq	%rsi, %rax
	incq	%rbx
	movq	-104(%rbp), %rcx        # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB19_170
.LBB19_171:                             # %for.cond553.preheader
	movq	$32, %rcx
	movq	-120(%rbp), %rdx        # 8-byte Reload
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	-184(%rbp), %r13        # 8-byte Reload
	jle	.LBB19_174
# BB#172:
	movq	$1, %rbx
	andq	%rsi, %rdx
	movq	%rdx, -104(%rbp)        # 8-byte Spill
	.align	16, 0x90
.LBB19_173:                             # %for.body556
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r13,%rbx,8), %rdi
	movq	%rsi, %r14
	movq	$1, %rsi
	callq	protect_from_queue
	movq	%r14, %rsi
	movq	%rax, -8(%r13,%rbx,8)
	movq	%rbx, %rax
	andq	%rsi, %rax
	incq	%rbx
	movq	-104(%rbp), %rcx        # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB19_173
.LBB19_174:                             # %for.cond565.preheader
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%rsi, %rdx
	movq	%r13, %rsi
	movq	-152(%rbp), %rax        # 8-byte Reload
	movq	-168(%rbp), %r13        # 8-byte Reload
	jle	.LBB19_177
# BB#175:                               # %for.body568.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$1, %rbx
	leaq	-96(%rbp), %r14
	.align	16, 0x90
.LBB19_176:                             # %for.body568
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdx, %r15
	movslq	-4(%rax,%rbx,4), %rdx
	movq	32(%r13), %rax
	movq	(%rsi,%rdx,8), %rcx
	addq	%r12, %rax
	movq	%rcx, (%rax,%rbx,8)
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str13, %rsi
	callq	sprintf
	movq	-160(%rbp), %rax        # 8-byte Reload
	movl	-4(%rax,%rbx,4), %eax
	movq	%rax, -104(%rbp)        # 8-byte Spill
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	ggc_alloc_string
	movq	$40, %rdi
	movq	-104(%rbp), %rsi        # 8-byte Reload
	movq	%rax, %rdx
	callq	gen_rtx_fmt_s
	movq	-136(%rbp), %rdi        # 8-byte Reload
	movq	%r15, %rdx
	movq	-184(%rbp), %rsi        # 8-byte Reload
	movq	40(%r13), %rcx
	addq	%r12, %rcx
	movq	%rax, (%rcx,%rbx,8)
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	%rdi, %rcx
	andq	%rdx, %rcx
	incq	%rbx
	cmpq	%rcx, %rax
	movq	-152(%rbp), %rax        # 8-byte Reload
	jne	.LBB19_176
.LBB19_177:                             # %for.end597
	movq	%r13, -168(%rbp)        # 8-byte Spill
	movq	%rsi, %r12
	movl	generating_concat_p(%rip), %eax
	movl	%eax, generating_concat_p(%rip)
	movq	-120(%rbp), %r15        # 8-byte Reload
	andq	%rdx, %r15
	cmpq	$1, %r15
	movq	-144(%rbp), %rdi        # 8-byte Reload
	jne	.LBB19_180
# BB#178:                               # %for.end597
	testq	%rdx, %rdi
	jne	.LBB19_180
# BB#179:                               # %if.then603
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax), %rax
	movq	-168(%rbp), %rcx        # 8-byte Reload
	movq	%rax, 16(%rcx)
	movq	(%r12), %rdx
	movq	$47, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_ee
	movq	%rax, %rdi
	callq	emit_insn
	jmp	.LBB19_198
.LBB19_180:                             # %if.else610
	movq	%rdi, %rax
	movq	-120(%rbp), %rcx        # 8-byte Reload
	orq	%rcx, %rax
	testq	%rdx, %rax
	je	.LBB19_181
# BB#182:                               # %if.else618
	testq	%rdx, %rcx
	movq	$1, %rax
	je	.LBB19_184
# BB#183:                               # %if.else618
	movq	-120(%rbp), %rax        # 8-byte Reload
.LBB19_184:                             # %if.else618
	movq	%r15, -104(%rbp)        # 8-byte Spill
	addq	%rax, %rdi
	callq	rtvec_alloc
	movq	$39, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
	movq	%rax, %r13
	cmpq	$0, -232(%rbp)          # 8-byte Folded Reload
	je	.LBB19_188
# BB#185:                               # %for.body628.lr.ph
	movq	-232(%rbp), %r14        # 8-byte Reload
	movabsq	$4160749567, %r12       # imm = 0xF7FFFFFF
.LBB19_186:                             # %for.body628
                                        # =>This Inner Loop Header: Depth=1
	movq	-184(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rbx,8), %r15
	movq	-216(%rbp), %rax        # 8-byte Reload
	movq	40(%rax), %rdx
	movq	-112(%rbp), %rax        # 8-byte Reload
	movq	(%rax,%rbx,8), %rcx
	movzbl	2(%r15), %esi
	subq	$32, %rsp
	movq	16(%rbp), %rax
	movq	%rax, 16(%rsp)
	movq	-192(%rbp), %rax        # 8-byte Reload
	movq	%rax, 8(%rsp)
	movq	$41, %rdi
	movq	-240(%rbp), %rax        # 8-byte Reload
	movq	%rax, (%rsp)
	movq	%rbx, %r8
	movq	-208(%rbp), %r9         # 8-byte Reload
	callq	gen_rtx_fmt_ssiEEsi
	addq	$32, %rsp
	movq	$47, %rdi
	xorq	%rsi, %rsi
	movq	%r15, %rdx
	movq	%rax, %rcx
	callq	gen_rtx_fmt_ee
	movq	8(%r13), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	8(%r13), %rax
	movq	8(%rax,%rbx,8), %rax
	movq	16(%rax), %rax
	movl	(%rax), %ecx
	andq	%r12, %rcx
	movq	-248(%rbp), %rdx        # 8-byte Reload
	orq	%rdx, %rcx
	movl	%ecx, (%rax)
	movq	(%r14), %r14
	incq	%rbx
	testq	%r14, %r14
	jne	.LBB19_186
# BB#187:                               # %for.end665
	movq	-184(%rbp), %r12        # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB19_189
.LBB19_188:                             # %if.then668
	movq	8(%r13), %rax
	movq	-168(%rbp), %rcx        # 8-byte Reload
	movq	%rcx, 8(%rax)
	movq	$1, %rbx
.LBB19_189:                             # %for.cond677.preheader
	movq	-224(%rbp), %r14        # 8-byte Reload
	testq	%r14, %r14
	je	.LBB19_197
# BB#190:                               # %for.body679.lr.ph
	movabsq	$4294967292, %r15       # imm = 0xFFFFFFFC
.LBB19_191:                             # %for.body679
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r14), %rax
	movq	40(%rax), %rdi
	callq	decode_reg_name
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jns	.LBB19_194
# BB#192:                               # %if.then689
                                        #   in Loop: Header=BB19_191 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%r15, %rax
	jne	.LBB19_196
# BB#193:                               # %if.then696
                                        #   in Loop: Header=BB19_191 Depth=1
	movq	$62, %rdi
	xorq	%rsi, %rsi
	callq	gen_rtx_fmt_0
	movq	$51, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	leaq	1(%rbx), %rdx
	movq	$32, %rcx
	jmp	.LBB19_195
.LBB19_194:                             # %if.end708
                                        #   in Loop: Header=BB19_191 Depth=1
	movq	$2, %rdi
	movq	%rax, %rsi
	callq	gen_rtx_REG
	movq	$49, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_e
	movq	$32, %rcx
	leaq	1(%rbx), %rdx
.LBB19_195:                             # %for.inc718
                                        #   in Loop: Header=BB19_191 Depth=1
	movq	8(%r13), %rsi
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rax, 8(%rsi,%rbx,8)
	movq	%rdx, %rbx
.LBB19_196:                             # %for.inc718
                                        #   in Loop: Header=BB19_191 Depth=1
	movq	(%r14), %r14
	testq	%r14, %r14
	jne	.LBB19_191
.LBB19_197:                             # %for.end721
	movq	%r13, %rdi
	callq	emit_insn
	movq	-104(%rbp), %r15        # 8-byte Reload
	jmp	.LBB19_198
.LBB19_181:                             # %if.then616
	movq	-168(%rbp), %rdi        # 8-byte Reload
	callq	emit_insn
.LBB19_198:                             # %for.cond725.preheader
	movq	$32, %rcx
	movq	-120(%rbp), %rax        # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB19_203
# BB#199:
	movq	$1, %rbx
	.align	16, 0x90
.LBB19_200:                             # %for.body728
                                        # =>This Inner Loop Header: Depth=1
	movq	-128(%rbp), %rax        # 8-byte Reload
	movq	-8(%rax,%rbx,8), %rdi
	testq	%rdi, %rdi
	je	.LBB19_202
# BB#201:                               # %if.then732
                                        #   in Loop: Header=BB19_200 Depth=1
	movq	-8(%r12,%rbx,8), %rsi
	callq	emit_move_insn
.LBB19_202:                             # %for.inc739
                                        #   in Loop: Header=BB19_200 Depth=1
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%rbx
	cmpq	%r15, %rax
	jne	.LBB19_200
.LBB19_203:                             # %for.end741
	callq	free_temp_slots
	jmp	.LBB19_204
.LBB19_100:                             # %if.then160
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_asm_operands, %rcx
	movq	$1726, %rsi             # imm = 0x6BE
	jmp	.LBB19_101
.LBB19_141:                             # %if.then354
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_asm_operands, %rcx
	movq	$1800, %rsi             # imm = 0x708
.LBB19_101:                             # %if.then160
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp143:
	.size	expand_asm_operands, .Ltmp143-expand_asm_operands
	.cfi_endproc

	.align	16, 0x90
	.type	parse_input_constraint,@function
parse_input_constraint:                 # @parse_input_constraint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp150:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp151:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp152:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp153:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp155:
	.cfi_def_cfa_offset 112
.Ltmp156:
	.cfi_offset %rbx, -48
.Ltmp157:
	.cfi_offset %r12, -40
.Ltmp158:
	.cfi_offset %r13, -32
.Ltmp159:
	.cfi_offset %r14, -24
.Ltmp160:
	.cfi_offset %r15, -16
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movq	120(%rsp), %r14
	movq	112(%rsp), %rbx
	movq	(%rdi), %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	movq	%r9, 8(%rsp)            # 8-byte Spill
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	callq	strlen
	movq	%r14, %rdx
	movq	%rax, %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	movb	%r15b, (%rbx)
	movb	%r15b, (%rdx)
	movq	$1, %r13
	je	.LBB20_36
# BB#1:                                 # %for.body.lr.ph
	movq	%r12, %rsi
	incq	%rsi
	movq	40(%rsp), %r12          # 8-byte Reload
	movq	24(%rsp), %rax          # 8-byte Reload
	subq	%rax, %r12
	movq	$32, %rcx
	movq	48(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	movq	$1, %rbx
	andq	%rdi, %r12
	movq	%r12, 40(%rsp)          # 8-byte Spill
	andq	%rdi, %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	32(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB20_2:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movsbq	(%r12,%r15), %rcx
	leaq	-33(%rcx), %rax
	andq	%rdi, %rax
	cmpq	$82, %rax
	ja	.LBB20_20
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB20_2 Depth=1
	jmpq	*.LJTI20_0(,%rax,8)
.LBB20_11:                              # %sw.bb14
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rdi, %r13
	movq	%rdx, %rbx
	leaq	(%r12,%r15), %rdi
	movq	$10, %rdx
	leaq	56(%rsp), %rsi
	callq	strtoul
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jae	.LBB20_12
# BB#13:                                # %if.end20
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	56(%rsp), %rcx
	movzbl	(%rcx), %edx
	testq	%rdx, %rdx
	jne	.LBB20_18
# BB#14:                                # %land.lhs.true24
                                        #   in Loop: Header=BB20_2 Depth=1
	testq	%r15, %r15
	je	.LBB20_17
# BB#15:                                # %land.lhs.true24
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$1, %r15
	jne	.LBB20_18
# BB#16:                                # %land.lhs.true29
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	(%r12), %edx
	cmpq	$37, %rdx
	jne	.LBB20_18
.LBB20_17:                              # %if.then34
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx,%rax,8), %r12
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	%r12, (%rax)
	movq	%r12, %rdi
	callq	strlen
	movq	%rax, %r14
	xorq	%r15, %r15
	movq	%rbx, %rdx
	movq	%r13, %rdi
	movq	$1, %rbx
	jmp	.LBB20_35
	.align	16, 0x90
.LBB20_20:                              # %sw.default
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rcx, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$136, %rax
	je	.LBB20_21
# BB#22:                                # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$112, %rcx
	jg	.LBB20_28
# BB#23:                                # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	addq	$-65, %rcx
	andq	$255, %rcx
	cmpq	$37, %rcx
	ja	.LBB20_33
# BB#24:                                # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$64424968201, %rsi      # imm = 0xF00070009
	testq	%rsi, %rax
	jne	.LBB20_31
# BB#25:                                # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$24, %rcx
	jne	.LBB20_26
# BB#37:                                # %cond.end210
                                        #   in Loop: Header=BB20_2 Depth=1
	movl	target_flags(%rip), %eax
	testq	$262144, %rax           # imm = 0x40000
	movb	%bl, (%rdx)
	je	.LBB20_34
	jmp	.LBB20_35
	.align	16, 0x90
.LBB20_28:                              # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	addq	$-113, %rcx
	andq	$255, %rcx
	cmpq	$8, %rcx
	ja	.LBB20_33
# BB#29:                                # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	jmpq	*.LJTI20_1(,%rcx,8)
.LBB20_32:                              # %cond.true138
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	target_flags+2(%rip), %eax
	testq	$5, %rax
	jne	.LBB20_31
	jmp	.LBB20_33
.LBB20_18:                              # %if.else
                                        #   in Loop: Header=BB20_2 Depth=1
	subq	%r12, %rcx
	movq	%rcx, %r15
	movq	%rbx, %rdx
	movq	%r13, %rdi
	movq	$1, %rbx
.LBB20_19:                              # %sw.bb38
                                        #   in Loop: Header=BB20_2 Depth=1
	movb	%bl, (%rdx)
	jmp	.LBB20_35
.LBB20_4:                               # %sw.bb
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jne	.LBB20_35
	jmp	.LBB20_5
.LBB20_30:                              # %cond.true158
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	target_flags+1(%rip), %eax
	testq	$64, %rax
	je	.LBB20_33
	jmp	.LBB20_31
.LBB20_7:                               # %sw.bb5
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jne	.LBB20_35
# BB#8:                                 # %sw.bb5
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, 24(%rsp)          # 8-byte Folded Reload
	jne	.LBB20_35
	jmp	.LBB20_9
.LBB20_26:                              # %if.end48
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	$37, %rcx
	jne	.LBB20_33
.LBB20_27:                              # %cond.true77
                                        #   in Loop: Header=BB20_2 Depth=1
	movzbl	target_flags(%rip), %eax
	testq	$33, %rax
	jne	.LBB20_31
	.align	16, 0x90
.LBB20_33:                              # %sw.bb39
                                        #   in Loop: Header=BB20_2 Depth=1
	movb	%bl, (%rdx)
.LBB20_34:                              # %if.else215
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	112(%rsp), %rax
	movb	%bl, (%rax)
	jmp	.LBB20_35
.LBB20_31:                              # %cond.end210.thread
                                        #   in Loop: Header=BB20_2 Depth=1
	movb	%bl, (%rdx)
	.align	16, 0x90
.LBB20_35:                              # %for.inc
                                        #   in Loop: Header=BB20_2 Depth=1
	incq	%r15
	cmpq	%r14, %r15
	movq	$1, %r13
	jb	.LBB20_2
	jmp	.LBB20_36
.LBB20_21:                              # %if.then45
	movabsq	$.L.str29, %rdi
.LBB20_6:                               # %return
	xorq	%rax, %rax
	xorq	%r13, %r13
	movq	%rcx, %rsi
	callq	error
	jmp	.LBB20_36
.LBB20_12:                              # %if.then19
	movabsq	$.L.str28, %rdi
.LBB20_10:                              # %return
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	error
.LBB20_36:                              # %return
	movq	%r13, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB20_5:                               # %if.then
	movabsq	$.L.str27, %rdi
	jmp	.LBB20_6
.LBB20_9:                               # %if.then10
	movabsq	$.L.str4, %rdi
	jmp	.LBB20_10
.Ltmp161:
	.size	parse_input_constraint, .Ltmp161-parse_input_constraint
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI20_0:
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_7
	.quad	.LBB20_4
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_35
	.quad	.LBB20_4
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_11
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_35
	.quad	.LBB20_4
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_34
	.quad	.LBB20_20
	.quad	.LBB20_33
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_33
	.quad	.LBB20_20
	.quad	.LBB20_35
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_20
	.quad	.LBB20_34
	.quad	.LBB20_35
	.quad	.LBB20_34
	.quad	.LBB20_19
	.quad	.LBB20_20
	.quad	.LBB20_19
	.quad	.LBB20_35
.LJTI20_1:
	.quad	.LBB20_31
	.quad	.LBB20_31
	.quad	.LBB20_33
	.quad	.LBB20_27
	.quad	.LBB20_27
	.quad	.LBB20_33
	.quad	.LBB20_33
	.quad	.LBB20_32
	.quad	.LBB20_30

	.text
	.globl	expand_expr_stmt
	.align	16, 0x90
	.type	expand_expr_stmt,@function
expand_expr_stmt:                       # @expand_expr_stmt
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	movq	$1, %rdx
	jmp	expand_expr_stmt_value  # TAILCALL
.Ltmp162:
	.size	expand_expr_stmt, .Ltmp162-expand_expr_stmt
	.cfi_endproc

	.globl	expand_expr_stmt_value
	.align	16, 0x90
	.type	expand_expr_stmt_value,@function
expand_expr_stmt_value:                 # @expand_expr_stmt_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp169:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp170:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp171:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp172:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp173:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp174:
	.cfi_def_cfa_offset 64
.Ltmp175:
	.cfi_offset %rbx, -48
.Ltmp176:
	.cfi_offset %r12, -40
.Ltmp177:
	.cfi_offset %r13, -32
.Ltmp178:
	.cfi_offset %r14, -24
.Ltmp179:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r14, %rax
	andq	%r12, %rax
	cmpq	%r12, %rax
	jne	.LBB22_4
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	72(%rax), %eax
	testq	%rax, %rax
	je	.LBB22_2
# BB#3:                                 # %if.then
	movabsq	$1, %r14
	jmp	.LBB22_4
.LBB22_2:
	movabsq	$0, %r14
.LBB22_4:                               # %if.end
	testq	%r12, %r14
	je	.LBB22_5
# BB#18:                                # %land.lhs.true41
	movq	8(%r15), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$23, %rax
	jne	.LBB22_20
# BB#19:                                # %if.then50
	callq	build_pointer_type
	movq	$121, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	build1
	movq	%rax, %r15
	jmp	.LBB22_20
.LBB22_5:                               # %land.lhs.true
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	72(%rax), %ecx
	testq	%rcx, %rcx
	movabsq	$0, %rcx
	movabsq	$1, %rdi
	movq	%rcx, %rsi
	je	.LBB22_7
# BB#6:                                 # %land.lhs.true
	movq	%rdi, %rsi
.LBB22_7:                               # %land.lhs.true
	testq	%r12, %rdx
	je	.LBB22_9
# BB#8:                                 # %land.lhs.true
	movq	%rdi, %rcx
.LBB22_9:                               # %land.lhs.true
	testq	%rcx, %rsi
	jne	.LBB22_20
# BB#10:                                # %land.lhs.true
	movq	global_trees(%rip), %rcx
	cmpq	%r15, %rcx
	je	.LBB22_20
# BB#11:                                # %if.then10
	movl	16(%r15), %ecx
	testq	$256, %rcx              # imm = 0x100
	je	.LBB22_12
# BB#16:                                # %if.else
	movl	warn_unused_value(%rip), %eax
	testq	%rax, %rax
	je	.LBB22_20
# BB#17:                                # %if.then36
	movq	%r15, %rdi
	callq	warn_if_unused_value
	jmp	.LBB22_20
.LBB22_12:                              # %if.then12
	movl	extra_warnings(%rip), %edx
	movl	warn_unused_value(%rip), %esi
	orq	%rdx, %rsi
	testq	%r12, %rsi
	je	.LBB22_20
# BB#13:                                # %land.lhs.true16
	andq	$255, %rcx
	cmpq	$114, %rcx
	jne	.LBB22_15
# BB#14:                                # %land.lhs.true22
	movq	8(%r15), %rcx
	movzbl	16(%rcx), %ecx
	cmpq	$5, %rcx
	je	.LBB22_20
.LBB22_15:                              # %if.then31
	movq	80(%rax), %rdi
	movl	88(%rax), %esi
	movabsq	$.L.str14, %rdx
	xorq	%rax, %rax
	callq	warning_with_file_and_line
.LBB22_20:                              # %if.end55
	xorq	%rbx, %rbx
	testq	%r12, %r14
	movq	%r12, %r13
	movabsq	$0, %rsi
	jne	.LBB22_22
# BB#21:                                # %if.end55
	movq	const_int_rtx+512(%rip), %rsi
.LBB22_22:                              # %if.end55
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	movq	8(%r15), %rcx
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB22_30
# BB#23:                                # %land.lhs.true61
	movzbl	(%r12), %eax
	cmpq	$66, %rax
	jne	.LBB22_29
# BB#24:                                # %land.lhs.true66
	movzbl	17(%r15), %eax
	testq	$8, %rax
	je	.LBB22_29
# BB#25:                                # %if.then72
	movq	8(%rsp), %rax           # 8-byte Reload
	movl	60(%rax), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	je	.LBB22_29
# BB#26:                                # %if.then72
	andq	%r13, %rax
	cmpq	$51, %rax
	jne	.LBB22_27
# BB#28:                                # %if.else90
	callq	gen_label_rtx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	32(%rcx), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	$103, %rdx
	movq	$51, %r8
	xorq	%r9, %r9
	movq	%rbx, (%rsp)
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%rax, %rcx
	callq	emit_cmp_and_jump_insns
	movq	%rbx, %rdi
	callq	emit_label
.LBB22_29:                              # %if.end97
	movq	%r12, %rbx
.LBB22_30:                              # %if.end97
	movq	%rbx, %rdi
	callq	preserve_temp_slots
	callq	free_temp_slots
	testq	%r13, %r14
	je	.LBB22_32
# BB#31:                                # %if.then99
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 64(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, 56(%rax)
.LBB22_32:                              # %if.end102
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_queue  # TAILCALL
.LBB22_27:                              # %if.then88
	movq	%r12, %rdi
	callq	copy_to_reg
	movq	%rax, %rbx
	jmp	.LBB22_30
.Ltmp180:
	.size	expand_expr_stmt_value, .Ltmp180-expand_expr_stmt_value
	.cfi_endproc

	.globl	warn_if_unused_value
	.align	16, 0x90
	.type	warn_if_unused_value,@function
warn_if_unused_value:                   # @warn_if_unused_value
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp184:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp185:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp186:
	.cfi_def_cfa_offset 32
.Ltmp187:
	.cfi_offset %rbx, -24
.Ltmp188:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %edx
	testq	$65536, %rdx            # imm = 0x10000
	je	.LBB23_2
# BB#1:
	xorq	%rax, %rax
.LBB23_42:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB23_2:
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB23_3:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB23_4
# BB#5:                                 # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	%rdx, %rsi
	andq	$255, %rsi
	cmpq	$90, %rsi
	jg	.LBB23_8
# BB#6:                                 # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	leaq	-41(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$14, %rcx
	ja	.LBB23_30
# BB#7:                                 # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	xorq	%rax, %rax
	jmpq	*.LJTI23_0(,%rcx,8)
.LBB23_29:                              # %sw.bb95
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	32(%rbx), %rbx
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$15, %rax
	je	.LBB23_16
	jmp	.LBB23_30
	.align	16, 0x90
.LBB23_8:                               # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	cmpq	$113, %rsi
	jg	.LBB23_10
# BB#9:                                 # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	leaq	-91(%rsi), %rax
	andq	%r14, %rax
	cmpq	$2, %rax
	jb	.LBB23_15
	jmp	.LBB23_30
	.align	16, 0x90
.LBB23_10:                              # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	leaq	-114(%rsi), %rcx
	andq	%r14, %rcx
	cmpq	$26, %rcx
	ja	.LBB23_30
# BB#11:                                # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$68649024, %rax         # imm = 0x4178040
	jne	.LBB23_12
# BB#13:                                # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$7, %rax
	jne	.LBB23_23
# BB#14:                                # %if.end6
                                        #   in Loop: Header=BB23_3 Depth=1
	cmpq	$4, %rcx
	jne	.LBB23_30
.LBB23_15:                              # %sw.bb11
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	40(%rbx), %rbx
	jmp	.LBB23_16
.LBB23_18:                              # %sw.bb23
                                        #   in Loop: Header=BB23_3 Depth=1
	testq	$262144, %rdx           # imm = 0x40000
	jne	.LBB23_19
# BB#20:                                # %if.end30
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	32(%rbx), %rdi
	callq	warn_if_unused_value
	testq	%r14, %rax
	movq	$1, %rax
	jne	.LBB23_42
# BB#21:                                # %if.end37
                                        #   in Loop: Header=BB23_3 Depth=1
	movq	40(%rbx), %rbx
	movzbl	17(%rbx), %eax
	testq	$2, %rax
	jne	.LBB23_22
	.align	16, 0x90
.LBB23_16:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB23_3 Depth=1
	movl	16(%rbx), %edx
	testq	$65536, %rdx            # imm = 0x10000
	je	.LBB23_3
# BB#17:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_30:                              # %sw.default
	movzbl	tree_code_type(%rsi), %eax
	cmpq	$100, %rax
	je	.LBB23_33
# BB#31:                                # %sw.default
	cmpq	$101, %rax
	je	.LBB23_36
# BB#32:                                # %sw.default
	cmpq	$114, %rax
	jne	.LBB23_39
.LBB23_33:                              # %land.lhs.true
	testq	$2048, %rdx             # imm = 0x800
	je	.LBB23_35
# BB#34:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_12:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_4:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_23:                              # %sw.bb52
	testq	$262144, %rdx           # imm = 0x40000
	je	.LBB23_25
# BB#24:
	xorq	%rax, %rax
	jmp	.LBB23_42
	.align	16, 0x90
.LBB23_25:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rbx
	movl	16(%rbx), %ecx
	movq	%rcx, %rax
	andq	$254, %rax
	cmpq	$114, %rax
	je	.LBB23_25
# BB#26:                                # %while.end
	andq	$255, %rcx
	cmpq	$53, %rcx
	ja	.LBB23_39
# BB#27:                                # %while.end
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$9851624184872960, %rcx # imm = 0x23000000000000
	testq	%rcx, %rax
	je	.LBB23_39
# BB#28:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_35:                              # %if.end135
	cmpq	$101, %rax
	jne	.LBB23_39
.LBB23_36:                              # %land.lhs.true145
	movl	tree_code_length(,%rsi,4), %eax
	testq	%rax, %rax
	je	.LBB23_37
# BB#38:                                # %land.lhs.true145
	andq	$256, %rdx              # imm = 0x100
	testq	%r14, %rdx
	jmp	.LBB23_40
.LBB23_39:                              # %maybe_warn
	testq	$256, %rdx              # imm = 0x100
.LBB23_40:                              # %maybe_warn
	movabsq	$0, %rax
	jne	.LBB23_42
# BB#41:                                # %if.end162
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	80(%rax), %rdi
	movl	88(%rax), %esi
	movabsq	$.L.str15, %rdx
	xorq	%rax, %rax
	callq	warning_with_file_and_line
	movq	$1, %rax
	jmp	.LBB23_42
.LBB23_37:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_19:
	xorq	%rax, %rax
	jmp	.LBB23_42
.LBB23_22:
	xorq	%rax, %rax
	jmp	.LBB23_42
.Ltmp189:
	.size	warn_if_unused_value, .Ltmp189-warn_if_unused_value
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI23_0:
	.quad	.LBB23_29
	.quad	.LBB23_30
	.quad	.LBB23_30
	.quad	.LBB23_30
	.quad	.LBB23_30
	.quad	.LBB23_30
	.quad	.LBB23_18
	.quad	.LBB23_42
	.quad	.LBB23_42
	.quad	.LBB23_42
	.quad	.LBB23_30
	.quad	.LBB23_15
	.quad	.LBB23_42
	.quad	.LBB23_42
	.quad	.LBB23_42

	.text
	.globl	clear_last_expr
	.align	16, 0x90
	.type	clear_last_expr,@function
clear_last_expr:                        # @clear_last_expr
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	retq
.Ltmp190:
	.size	clear_last_expr, .Ltmp190-clear_last_expr
	.cfi_endproc

	.globl	expand_start_stmt_expr
	.align	16, 0x90
	.type	expand_start_stmt_expr,@function
expand_start_stmt_expr:                 # @expand_start_stmt_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp194:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp195:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp196:
	.cfi_def_cfa_offset 32
.Ltmp197:
	.cfi_offset %rbx, -24
.Ltmp198:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	$120, %rdi
	callq	make_node
	movq	%rax, %r14
	callq	do_pending_stack_adjust
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB25_2
# BB#1:                                 # %if.then
	movq	%r14, %rdi
	callq	start_sequence_for_rtl_expr
	jmp	.LBB25_3
.LBB25_2:                               # %if.else
	callq	start_sequence
.LBB25_3:                               # %if.end
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
	movl	4(%rcx), %edx
	incq	%rdx
	movl	%edx, 4(%rcx)
	movq	8(%rax), %rax
	movl	72(%rax), %ecx
	incq	%rcx
	movl	%ecx, 72(%rax)
	movq	$0, 64(%rax)
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp199:
	.size	expand_start_stmt_expr, .Ltmp199-expand_start_stmt_expr
	.cfi_endproc

	.globl	expand_end_stmt_expr
	.align	16, 0x90
	.type	expand_end_stmt_expr,@function
expand_end_stmt_expr:                   # @expand_end_stmt_expr
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp202:
	.cfi_def_cfa_offset 16
.Ltmp203:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movq	16(%rax), %rcx
	movl	4(%rcx), %edx
	decq	%rdx
	movl	%edx, 4(%rcx)
	movq	8(%rax), %rcx
	movq	64(%rcx), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB26_2
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, 56(%rcx)
	je	.LBB26_2
# BB#3:                                 # %if.else
	movzbl	(%rax), %edx
	cmpq	$133, %rdx
	jg	.LBB26_6
# BB#4:                                 # %if.else
	addq	$-54, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$14, %rcx
	ja	.LBB26_8
# BB#5:                                 # %if.else
	movq	$1, %rdx
	shlq	%cl, %rdx
	testq	$24727, %rdx            # imm = 0x6097
	jne	.LBB26_9
	jmp	.LBB26_8
.LBB26_2:                               # %if.then
	movq	const_int_rtx+512(%rip), %rax
	movq	%rax, 64(%rcx)
	movq	cfun(%rip), %rax
	movq	global_trees+216(%rip), %rcx
	movq	8(%rax), %rax
	movq	%rcx, 56(%rax)
	jmp	.LBB26_9
.LBB26_6:                               # %if.else
	cmpq	$134, %rdx
	je	.LBB26_9
# BB#7:                                 # %if.else
	cmpq	$140, %rdx
	je	.LBB26_9
.LBB26_8:                               # %if.then56
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	callq	protect_from_queue
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 64(%rcx)
.LBB26_9:                               # %if.end61
	callq	emit_queue
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	56(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	64(%rax), %rax
	movq	%rax, 40(%rbx)
	callq	get_insns
	movq	%rax, 32(%rbx)
	movq	cfun(%rip), %rax
	movq	224(%rax), %rdx
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	callq	tree_cons
	movq	cfun(%rip), %rcx
	movq	%rax, 224(%rcx)
	callq	end_sequence
	movl	16(%rbx), %eax
	orq	$256, %rax              # imm = 0x100
	movl	%eax, 16(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	64(%rax), %rdi
	callq	volatile_refs_p
	movl	16(%rbx), %ecx
	andq	$1, %rax
	shlq	$11, %rax
	movabsq	$4294965247, %rdx       # imm = 0xFFFFF7FF
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 16(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	72(%rax), %ecx
	decq	%rcx
	movl	%ecx, 72(%rax)
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp204:
	.size	expand_end_stmt_expr, .Ltmp204-expand_end_stmt_expr
	.cfi_endproc

	.globl	expand_start_cond
	.align	16, 0x90
	.type	expand_start_cond,@function
expand_start_cond:                      # @expand_start_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp209:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp210:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp211:
	.cfi_def_cfa_offset 32
.Ltmp212:
	.cfi_offset %rbx, -32
.Ltmp213:
	.cfi_offset %r14, -24
.Ltmp214:
	.cfi_offset %r15, -16
	movq	stmt_obstack+32(%rip), %rcx
	movq	stmt_obstack+24(%rip), %rax
	movq	%rsi, %r15
	movq	%rdi, %r14
	subq	%rax, %rcx
	cmpq	$119, %rcx
	jg	.LBB27_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rax
.LBB27_2:                               # %if.end
	addq	$120, %rax
	movq	%rax, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB27_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, stmt_obstack+80(%rip)
.LBB27_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB27_6
# BB#5:                                 # %if.then29
	movq	%rax, stmt_obstack+24(%rip)
	movq	%rax, %rcx
.LBB27_6:                               # %if.end32
	movq	cfun(%rip), %rax
	movq	%rcx, stmt_obstack+16(%rip)
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 16(%rbx)
	callq	gen_label_rtx
	movq	%rax, %rcx
	movq	%rcx, 40(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB27_7
# BB#8:                                 # %cond.true
	callq	gen_label_rtx
	movq	40(%rbx), %rcx
	jmp	.LBB27_9
.LBB27_7:
	xorq	%rax, %rax
.LBB27_9:                               # %cond.end
	movq	%rax, 24(%rbx)
	movq	%rax, 32(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	xorq	%rdx, %rdx
	movq	%rbx, 40(%rax)
	movq	%r14, %rdi
	movq	%rcx, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	do_jump  # TAILCALL
.Ltmp215:
	.size	expand_start_cond, .Ltmp215-expand_start_cond
	.cfi_endproc

	.globl	expand_start_elseif
	.align	16, 0x90
	.type	expand_start_elseif,@function
expand_start_elseif:                    # @expand_start_elseif
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp219:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp220:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp221:
	.cfi_def_cfa_offset 32
.Ltmp222:
	.cfi_offset %rbx, -24
.Ltmp223:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
	movq	%rdi, %r14
	testq	%rbx, %rbx
	jne	.LBB28_2
# BB#1:                                 # %if.then
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
.LBB28_2:                               # %if.end
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rdi
	callq	emit_label
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	do_jump  # TAILCALL
.Ltmp224:
	.size	expand_start_elseif, .Ltmp224-expand_start_elseif
	.cfi_endproc

	.globl	expand_start_else
	.align	16, 0x90
	.type	expand_start_else,@function
expand_start_else:                      # @expand_start_else
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp227:
	.cfi_def_cfa_offset 16
.Ltmp228:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB29_2
# BB#1:                                 # %if.then
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	32(%rax), %rbx
.LBB29_2:                               # %if.end
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	$0, 40(%rax)
	popq	%rbx
	retq
.Ltmp229:
	.size	expand_start_else, .Ltmp229-expand_start_else
	.cfi_endproc

	.globl	expand_elseif
	.align	16, 0x90
	.type	expand_elseif,@function
expand_elseif:                          # @expand_elseif
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp232:
	.cfi_def_cfa_offset 16
.Ltmp233:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	16(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rax
	movq	40(%rax), %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	popq	%rbx
	jmp	do_jump  # TAILCALL
.Ltmp234:
	.size	expand_elseif, .Ltmp234-expand_elseif
	.cfi_endproc

	.globl	expand_end_cond
	.align	16, 0x90
	.type	expand_end_cond,@function
expand_end_cond:                        # @expand_end_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp239:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp240:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp241:
	.cfi_def_cfa_offset 32
.Ltmp242:
	.cfi_offset %rbx, -32
.Ltmp243:
	.cfi_offset %r14, -24
.Ltmp244:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %rbx
	callq	do_pending_stack_adjust
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB31_2
# BB#1:                                 # %if.then
	callq	emit_label
.LBB31_2:                               # %if.end
	movq	32(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB31_4
# BB#3:                                 # %if.then7
	callq	emit_label
.LBB31_4:                               # %do.body
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	16(%rax), %r15
	movabsq	$stmt_obstack, %r14
	.align	16, 0x90
.LBB31_5:                               # %do.body15
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB31_7
# BB#6:                                 # %if.then18
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB31_7:                               # %if.end23
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB31_9
# BB#8:                                 # %if.then27
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB31_9:                               # %if.end33
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB31_11
# BB#10:                                # %if.then36
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB31_11:                              # %if.end42
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB31_13
# BB#12:                                # %if.then45
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB31_13:                              # %if.end51
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB31_15
# BB#14:                                # %if.then54
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB31_15:                              # %if.end60
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB31_18
# BB#16:                                # %if.end60
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB31_18
# BB#17:                                # %if.then68
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB31_19
	.align	16, 0x90
.LBB31_18:                              # %if.else
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB31_19:                              # %do.cond
                                        #   in Loop: Header=BB31_5 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%r15, %rbx
	jne	.LBB31_5
# BB#20:                                # %do.end72
	movq	$0, 56(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp245:
	.size	expand_end_cond, .Ltmp245-expand_end_cond
	.cfi_endproc

	.globl	expand_start_loop
	.align	16, 0x90
	.type	expand_start_loop,@function
expand_start_loop:                      # @expand_start_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp249:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp250:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp251:
	.cfi_def_cfa_offset 32
.Ltmp252:
	.cfi_offset %rbx, -24
.Ltmp253:
	.cfi_offset %r14, -16
	movq	stmt_obstack+32(%rip), %rcx
	movq	stmt_obstack+24(%rip), %rax
	movq	%rdi, %r14
	subq	%rax, %rcx
	cmpq	$119, %rcx
	jg	.LBB32_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rax
.LBB32_2:                               # %if.end
	addq	$120, %rax
	movq	%rax, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB32_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, stmt_obstack+80(%rip)
.LBB32_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB32_6
# BB#5:                                 # %if.then29
	movq	%rax, stmt_obstack+24(%rip)
	movq	%rax, %rcx
.LBB32_6:                               # %if.end32
	movq	cfun(%rip), %rax
	movq	%rcx, stmt_obstack+16(%rip)
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 16(%rbx)
	callq	gen_label_rtx
	movq	%rax, 32(%rbx)
	callq	gen_label_rtx
	movq	%rax, 40(%rbx)
	movq	$0, 48(%rbx)
	movq	32(%rbx), %rcx
	movq	%rcx, 56(%rbx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	je	.LBB32_8
# BB#7:                                 # %if.end32
	movq	%rax, %r14
.LBB32_8:                               # %if.end32
	movq	%r14, 24(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 40(%rax)
	callq	do_pending_stack_adjust
	callq	emit_queue
	movabsq	$4294967200, %rsi       # imm = 0xFFFFFFA0
	xorq	%rdi, %rdi
	callq	emit_note
	movq	32(%rbx), %rdi
	callq	emit_label
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp254:
	.size	expand_start_loop, .Ltmp254-expand_start_loop
	.cfi_endproc

	.globl	expand_start_loop_continue_elsewhere
	.align	16, 0x90
	.type	expand_start_loop_continue_elsewhere,@function
expand_start_loop_continue_elsewhere:   # @expand_start_loop_continue_elsewhere
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp257:
	.cfi_def_cfa_offset 16
.Ltmp258:
	.cfi_offset %rbx, -16
	callq	expand_start_loop
	movq	%rax, %rbx
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	24(%rcx), %rcx
	movq	%rax, 56(%rcx)
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp259:
	.size	expand_start_loop_continue_elsewhere, .Ltmp259-expand_start_loop_continue_elsewhere
	.cfi_endproc

	.globl	expand_start_null_loop
	.align	16, 0x90
	.type	expand_start_null_loop,@function
expand_start_null_loop:                 # @expand_start_null_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 16
.Ltmp263:
	.cfi_offset %rbx, -16
	movq	stmt_obstack+32(%rip), %rcx
	movq	stmt_obstack+24(%rip), %rax
	subq	%rax, %rcx
	cmpq	$119, %rcx
	jg	.LBB34_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rax
.LBB34_2:                               # %if.end
	addq	$120, %rax
	movq	%rax, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB34_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, stmt_obstack+80(%rip)
.LBB34_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB34_6
# BB#5:                                 # %if.then29
	movq	%rax, stmt_obstack+24(%rip)
	movq	%rax, %rcx
.LBB34_6:                               # %if.end32
	movq	cfun(%rip), %rax
	movq	%rcx, stmt_obstack+16(%rip)
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 16(%rbx)
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, 32(%rbx)
	callq	gen_label_rtx
	movq	%rax, 40(%rbx)
	movq	$0, 48(%rbx)
	movq	%rax, 56(%rbx)
	movq	%rax, 24(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 40(%rax)
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp264:
	.size	expand_start_null_loop, .Ltmp264-expand_start_null_loop
	.cfi_endproc

	.globl	expand_loop_continue_here
	.align	16, 0x90
	.type	expand_loop_continue_here,@function
expand_loop_continue_here:              # @expand_loop_continue_here
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp266:
	.cfi_def_cfa_offset 16
	callq	do_pending_stack_adjust
	movabsq	$4294967202, %rsi       # imm = 0xFFFFFFA2
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	56(%rax), %rdi
	popq	%rax
	jmp	emit_label  # TAILCALL
.Ltmp267:
	.size	expand_loop_continue_here, .Ltmp267-expand_loop_continue_here
	.cfi_endproc

	.globl	expand_end_loop
	.align	16, 0x90
	.type	expand_end_loop,@function
expand_end_loop:                        # @expand_end_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp274:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp275:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp276:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp277:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp278:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp279:
	.cfi_def_cfa_offset 64
.Ltmp280:
	.cfi_offset %rbx, -48
.Ltmp281:
	.cfi_offset %r12, -40
.Ltmp282:
	.cfi_offset %r13, -32
.Ltmp283:
	.cfi_offset %r14, -24
.Ltmp284:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %r14
	movq	56(%rax), %rax
	cmpq	%rax, %r14
	jne	.LBB36_2
# BB#1:                                 # %if.then
	movabsq	$4294967202, %rdi       # imm = 0xFFFFFFA2
	movq	%r14, %rsi
	callq	emit_note_before
.LBB36_2:                               # %if.end
	callq	do_pending_stack_adjust
	xorq	%rax, %rax
	testq	%r14, %r14
	movabsq	$0, %r15
	je	.LBB36_41
# BB#3:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r14, %rbx
	xorq	%r12, %r12
	.align	16, 0x90
.LBB36_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	cmpq	$37, %rcx
	jne	.LBB36_13
# BB#5:                                 # %if.then7
                                        #   in Loop: Header=BB36_4 Depth=1
	movl	40(%rbx), %ecx
	addq	$98, %rcx
	andq	%r13, %rcx
	cmpq	$13, %rcx
	ja	.LBB36_13
# BB#6:                                 # %if.then7
                                        #   in Loop: Header=BB36_4 Depth=1
	movq	%r14, %r15
	jmpq	*.LJTI36_0(,%rcx,8)
.LBB36_11:                              # %if.then35
                                        #   in Loop: Header=BB36_4 Depth=1
	incq	%r12
	jmp	.LBB36_13
.LBB36_12:                              # %if.then42
                                        #   in Loop: Header=BB36_4 Depth=1
	decq	%r12
	jmp	.LBB36_13
.LBB36_7:                               # %if.then20
                                        #   in Loop: Header=BB36_4 Depth=1
	incq	%rax
	jmp	.LBB36_13
.LBB36_8:                               # %if.then26
                                        #   in Loop: Header=BB36_4 Depth=1
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB36_10
# BB#9:                                 #   in Loop: Header=BB36_4 Depth=1
	decq	%rax
	.align	16, 0x90
.LBB36_13:                              # %for.inc
                                        #   in Loop: Header=BB36_4 Depth=1
	movq	24(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB36_4
# BB#14:
	movq	%r14, %r15
.LBB36_41:                              # %if.end124
	callq	do_pending_stack_adjust
	movq	%r15, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movabsq	$4294967201, %rsi       # imm = 0xFFFFFFA1
	xorq	%rdi, %rdi
	callq	emit_note
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	40(%rax), %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %r15
	movabsq	$stmt_obstack, %r14
	movq	%r15, %rcx
	jmp	.LBB36_42
	.align	16, 0x90
.LBB36_57:                              # %do.cond.do.body133_crit_edge
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	24(%rax), %rcx
.LBB36_42:                              # %do.body133
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB36_44
# BB#43:                                # %if.then138
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB36_44:                              # %if.end143
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB36_46
# BB#45:                                # %if.then146
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB36_46:                              # %if.end152
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB36_48
# BB#47:                                # %if.then155
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB36_48:                              # %if.end161
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB36_50
# BB#49:                                # %if.then164
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB36_50:                              # %if.end170
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB36_52
# BB#51:                                # %if.then173
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB36_52:                              # %if.end179
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB36_55
# BB#53:                                # %if.end179
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB36_55
# BB#54:                                # %if.then188
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB36_56
	.align	16, 0x90
.LBB36_55:                              # %if.else189
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB36_56:                              # %do.cond
                                        #   in Loop: Header=BB36_42 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%r15, %rbx
	jne	.LBB36_57
# BB#58:                                # %do.end193
	movq	$0, 56(%rax)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB36_15:                              # %for.end
	testq	%rbx, %rbx
	je	.LBB36_16
# BB#17:                                # %for.end
	movl	optimize(%rip), %edx
	testq	%r13, %rdx
	je	.LBB36_18
# BB#19:                                # %for.end
	testq	%r13, %rax
	jne	.LBB36_20
# BB#21:                                # %land.lhs.true57
	testq	%r13, %r12
	je	.LBB36_24
# BB#22:                                # %land.lhs.true57
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jge	.LBB36_24
# BB#23:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_20:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_24:                              # %land.lhs.true60
	cmpq	$0, 24(%rbx)
	je	.LBB36_25
# BB#26:                                # %land.lhs.true65
	callq	get_last_insn
	movq	%rax, %rdi
	callq	any_condjump_p
	testq	%r13, %rax
	je	.LBB36_28
# BB#27:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_16:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_18:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_25:
	movq	%r14, %r15
	jmp	.LBB36_41
.LBB36_28:                              # %if.then69
	callq	gen_label_rtx
	movq	16(%r14), %rcx
	movzbl	(%rcx), %edx
	movq	%rax, %r15
	cmpq	$37, %rdx
	movq	%r14, %rax
	jne	.LBB36_32
# BB#29:                                # %land.lhs.true77
	movl	40(%rcx), %eax
	movabsq	$4294967202, %rdx       # imm = 0xFFFFFFA2
	cmpq	%rdx, %rax
	je	.LBB36_31
# BB#30:                                # %select.mid
	movq	%r14, %rcx
.LBB36_31:                              # %select.end
	movq	%rcx, %rax
.LBB36_32:                              # %if.end89
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_label_before
	testq	%r13, %r12
	je	.LBB36_59
# BB#33:                                # %for.cond95.preheader
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%rax, %rax
	movq	%rax, %rcx
	je	.LBB36_40
.LBB36_34:                              # %for.body97
                                        # =>This Inner Loop Header: Depth=1
	xorq	%r12, %r12
	cmpq	%rbx, %rcx
	je	.LBB36_36
# BB#35:                                # %cond.false
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	24(%rcx), %r12
.LBB36_36:                              # %cond.end
                                        #   in Loop: Header=BB36_34 Depth=1
	movzbl	(%rcx), %eax
	cmpq	$37, %rax
	jne	.LBB36_38
# BB#37:                                # %land.lhs.true105
                                        #   in Loop: Header=BB36_34 Depth=1
	movl	40(%rcx), %eax
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rdx, %rax
	movabsq	$4294967198, %rdx       # imm = 0xFFFFFF9E
	cmpq	%rdx, %rax
	je	.LBB36_39
.LBB36_38:                              # %if.end116
                                        #   in Loop: Header=BB36_34 Depth=1
	movq	%rcx, %r13
	callq	get_last_insn
	movq	%r13, %rdi
	movq	%r13, %rsi
	movq	%rax, %rdx
	callq	reorder_insns
.LBB36_39:                              # %for.cond95.backedge
                                        #   in Loop: Header=BB36_34 Depth=1
	testq	%r12, %r12
	movq	%r12, %rcx
	jne	.LBB36_34
	jmp	.LBB36_40
.LBB36_59:                              # %if.then92
	callq	get_last_insn
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	reorder_insns
.LBB36_40:                              # %if.end120
	movq	%r14, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	emit_jump_insn_before
	movq	%r15, %rdi
	callq	emit_barrier_before
	jmp	.LBB36_41
.LBB36_10:                              # %if.then28
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_end_loop, %rcx
	movq	$2733, %rsi             # imm = 0xAAD
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp285:
	.size	expand_end_loop, .Ltmp285-expand_end_loop
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI36_0:
	.quad	.LBB36_11
	.quad	.LBB36_12
	.quad	.LBB36_41
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_15
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_13
	.quad	.LBB36_7
	.quad	.LBB36_8

	.text
	.globl	expand_end_null_loop
	.align	16, 0x90
	.type	expand_end_null_loop,@function
expand_end_null_loop:                   # @expand_end_null_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp290:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp291:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp292:
	.cfi_def_cfa_offset 32
.Ltmp293:
	.cfi_offset %rbx, -32
.Ltmp294:
	.cfi_offset %r14, -24
.Ltmp295:
	.cfi_offset %r15, -16
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	40(%rax), %rdi
	callq	emit_label
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %r15
	movabsq	$stmt_obstack, %r14
	movq	%r15, %rcx
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_16:                              # %do.cond.do.body3_crit_edge
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	24(%rax), %rcx
.LBB37_1:                               # %do.body3
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB37_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rcx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB37_3:                               # %if.end
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB37_5
# BB#4:                                 # %if.then13
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB37_5:                               # %if.end19
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB37_7
# BB#6:                                 # %if.then22
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB37_7:                               # %if.end28
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB37_9
# BB#8:                                 # %if.then31
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB37_9:                               # %if.end37
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB37_11
# BB#10:                                # %if.then40
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB37_11:                              # %if.end46
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB37_14
# BB#12:                                # %if.end46
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB37_14
# BB#13:                                # %if.then54
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB37_15
	.align	16, 0x90
.LBB37_14:                              # %if.else
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB37_15:                              # %do.cond
                                        #   in Loop: Header=BB37_1 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	cmpq	%r15, %rbx
	jne	.LBB37_16
# BB#17:                                # %do.end58
	movq	$0, 56(%rax)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp296:
	.size	expand_end_null_loop, .Ltmp296-expand_end_null_loop
	.cfi_endproc

	.globl	expand_continue_loop
	.align	16, 0x90
	.type	expand_continue_loop,@function
expand_continue_loop:                   # @expand_continue_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp298:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	testq	%rdi, %rdi
	movq	$0, 56(%rax)
	jne	.LBB38_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB38_3
.LBB38_2:                               # %if.end4
	movq	56(%rdi), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_goto_internal
	movq	$1, %rax
.LBB38_3:                               # %return
	popq	%rdx
	retq
.Ltmp299:
	.size	expand_continue_loop, .Ltmp299-expand_continue_loop
	.cfi_endproc

	.globl	expand_exit_loop
	.align	16, 0x90
	.type	expand_exit_loop,@function
expand_exit_loop:                       # @expand_exit_loop
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp301:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	testq	%rdi, %rdi
	movq	$0, 56(%rax)
	jne	.LBB39_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rdi
	xorq	%rax, %rax
	testq	%rdi, %rdi
	je	.LBB39_3
.LBB39_2:                               # %if.end4
	movq	40(%rdi), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_goto_internal
	movq	$1, %rax
.LBB39_3:                               # %return
	popq	%rdx
	retq
.Ltmp302:
	.size	expand_exit_loop, .Ltmp302-expand_exit_loop
	.cfi_endproc

	.globl	expand_exit_loop_if_false
	.align	16, 0x90
	.type	expand_exit_loop_if_false,@function
expand_exit_loop_if_false:              # @expand_exit_loop_if_false
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp307:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp308:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp309:
	.cfi_def_cfa_offset 32
.Ltmp310:
	.cfi_offset %rbx, -32
.Ltmp311:
	.cfi_offset %r14, -24
.Ltmp312:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %r14
	testq	%rbx, %rbx
	movq	$0, 56(%rcx)
	jne	.LBB40_2
# BB#1:                                 # %if.end
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB40_5
.LBB40_2:                               # %if.end4
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	do_jump
	callq	get_last_insn
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB40_4
# BB#3:                                 # %if.then7
	movq	%rax, 48(%rbx)
.LBB40_4:                               # %if.end8
	movq	40(%rbx), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_goto_internal
	movq	%r14, %rdi
	callq	emit_label
	movq	$1, %rax
.LBB40_5:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp313:
	.size	expand_exit_loop_if_false, .Ltmp313-expand_exit_loop_if_false
	.cfi_endproc

	.globl	expand_exit_loop_top_cond
	.align	16, 0x90
	.type	expand_exit_loop_top_cond,@function
expand_exit_loop_top_cond:              # @expand_exit_loop_top_cond
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp318:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp319:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp320:
	.cfi_def_cfa_offset 32
.Ltmp321:
	.cfi_offset %rbx, -32
.Ltmp322:
	.cfi_offset %r14, -24
.Ltmp323:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %rbx
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, %r14
	testq	%rbx, %rbx
	movq	$0, 56(%rcx)
	jne	.LBB41_2
# BB#1:                                 # %if.end.i
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rbx
	xorq	%rax, %rax
	testq	%rbx, %rbx
	je	.LBB41_5
.LBB41_2:                               # %if.end4.i
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	do_jump
	callq	get_last_insn
	movzbl	(%rax), %ecx
	cmpq	$36, %rcx
	jne	.LBB41_4
# BB#3:                                 # %if.then7.i
	movq	%rax, 48(%rbx)
.LBB41_4:                               # %if.end
	movq	40(%rbx), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_goto_internal
	movq	%r14, %rdi
	callq	emit_label
	movabsq	$4294967204, %rsi       # imm = 0xFFFFFFA4
	xorq	%rdi, %rdi
	callq	emit_note
	movq	$1, %rax
.LBB41_5:                               # %return
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp324:
	.size	expand_exit_loop_top_cond, .Ltmp324-expand_exit_loop_top_cond
	.cfi_endproc

	.globl	stmt_loop_nest_empty
	.align	16, 0x90
	.type	stmt_loop_nest_empty,@function
stmt_loop_nest_empty:                   # @stmt_loop_nest_empty
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rcx
	testq	%rcx, %rcx
	movq	$1, %rax
	je	.LBB42_4
# BB#1:                                 # %lor.rhs
	cmpq	$0, 24(%rcx)
	jne	.LBB42_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %rax
.LBB42_4:                               # %lor.end
	retq
.LBB42_2:
	movabsq	$0, %rax
	retq
.Ltmp325:
	.size	stmt_loop_nest_empty, .Ltmp325-stmt_loop_nest_empty
	.cfi_endproc

	.globl	preserve_subexpressions_p
	.align	16, 0x90
	.type	preserve_subexpressions_p,@function
preserve_subexpressions_p:              # @preserve_subexpressions_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp327:
	.cfi_def_cfa_offset 16
	movl	flag_expensive_optimizations(%rip), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB43_9
# BB#1:                                 # %if.end
	movl	optimize(%rip), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB43_9
# BB#2:                                 # %if.end
	movq	cfun(%rip), %rcx
	testq	%rcx, %rcx
	je	.LBB43_9
# BB#3:                                 # %lor.lhs.false2
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB43_9
# BB#4:                                 # %lor.lhs.false4
	cmpq	$0, 24(%rcx)
	je	.LBB43_9
# BB#5:                                 # %if.end8
	callq	get_last_insn_anywhere
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB43_9
# BB#6:                                 # %land.rhs
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	24(%rax), %rax
	movq	32(%rax), %rax
	movl	8(%rcx), %edx
	movl	8(%rax), %eax
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movl	n_non_fixed_regs(%rip), %eax
	sarq	%cl, %rdx
	leaq	(%rax,%rax,2), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rdx
	jge	.LBB43_7
# BB#8:                                 # %land.rhs
	movabsq	$1, %rax
.LBB43_9:                               # %return
	popq	%rdx
	retq
.LBB43_7:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp328:
	.size	preserve_subexpressions_p, .Ltmp328-preserve_subexpressions_p
	.cfi_endproc

	.globl	expand_exit_something
	.align	16, 0x90
	.type	expand_exit_something,@function
expand_exit_something:                  # @expand_exit_something
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp330:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rcx
	xorq	%rax, %rax
	jmp	.LBB44_2
	.align	16, 0x90
.LBB44_1:                               # %for.cond
                                        #   in Loop: Header=BB44_2 Depth=1
	movq	(%rcx), %rcx
.LBB44_2:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB44_5
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB44_2 Depth=1
	movq	24(%rcx), %rsi
	testq	%rsi, %rsi
	je	.LBB44_1
# BB#4:                                 # %if.then
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	expand_goto_internal
	movq	$1, %rax
.LBB44_5:                               # %return
	popq	%rdx
	retq
.Ltmp331:
	.size	expand_exit_something, .Ltmp331-expand_exit_something
	.cfi_endproc

	.globl	expand_null_return
	.align	16, 0x90
	.type	expand_null_return,@function
expand_null_return:                     # @expand_null_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp335:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp336:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp337:
	.cfi_def_cfa_offset 32
.Ltmp338:
	.cfi_offset %rbx, -24
.Ltmp339:
	.cfi_offset %r14, -16
	callq	get_last_insn
	movq	%rax, %r14
	callq	clobber_return_register
	movq	cfun(%rip), %rax
	movq	192(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB45_2
# BB#1:                                 # %cond.false.i
	movq	200(%rax), %rbx
.LBB45_2:                               # %cond.end.i
	callq	clear_pending_stack_adjust
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	testq	%rbx, %rbx
	jne	.LBB45_4
# BB#3:                                 # %if.then.i
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	%rbx, 200(%rcx)
.LBB45_4:                               # %expand_null_return_1.exit
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	expand_goto_internal  # TAILCALL
.Ltmp340:
	.size	expand_null_return, .Ltmp340-expand_null_return
	.cfi_endproc

	.globl	expand_return
	.align	16, 0x90
	.type	expand_return,@function
expand_return:                          # @expand_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp344:
	.cfi_def_cfa_offset 16
.Ltmp345:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp346:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$72, %rsp
.Ltmp347:
	.cfi_offset %rbx, -56
.Ltmp348:
	.cfi_offset %r12, -48
.Ltmp349:
	.cfi_offset %r13, -40
.Ltmp350:
	.cfi_offset %r14, -32
.Ltmp351:
	.cfi_offset %r15, -24
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movzbl	16(%rax), %eax
	movq	%rdi, %rbx
	cmpq	$5, %rax
	jne	.LBB46_7
# BB#1:                                 # %if.then
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	callq	emit_queue
	jmp	.LBB46_2
.LBB46_7:                               # %if.end
	movq	global_trees(%rip), %rax
	cmpq	%rbx, %rax
	je	.LBB46_2
# BB#8:                                 # %if.else
	movzbl	16(%rbx), %eax
	cmpq	$36, %rax
	movq	%rbx, %r15
	je	.LBB46_14
# BB#9:                                 # %if.else
	addq	$-48, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$1, %rcx
	ja	.LBB46_12
# BB#10:                                # %land.lhs.true
	movq	32(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$36, %rax
	jne	.LBB46_12
# BB#11:                                # %if.then28
	movq	40(%rbx), %r15
	jmp	.LBB46_14
.LBB46_12:                              # %if.else32
	movq	8(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	movq	%rbx, %r15
	je	.LBB46_14
# BB#13:                                # %if.else32
	xorq	%r15, %r15
.LBB46_14:                              # %if.end45
	callq	get_last_insn
	movl	optimize(%rip), %eax
	testq	%rax, %rax
	je	.LBB46_25
# BB#15:                                # %if.end45
	testq	%r15, %r15
	je	.LBB46_25
# BB#16:                                # %land.lhs.true49
	movq	cfun(%rip), %rax
	cmpq	$0, 264(%rax)
	jne	.LBB46_25
# BB#17:                                # %land.lhs.true51
	movzbl	16(%r15), %eax
	cmpq	$51, %rax
	jne	.LBB46_25
# BB#18:                                # %land.lhs.true57
	movq	40(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$53, %rax
	je	.LBB46_20
# BB#19:                                # %lor.lhs.false66
	movq	48(%r15), %rax
	movzbl	16(%rax), %eax
	cmpq	$53, %rax
	jne	.LBB46_25
.LBB46_20:                              # %if.then75
	callq	gen_label_rtx
	movq	32(%r15), %rdi
	movq	%rax, %rbx
	xorq	%rdx, %rdx
	movq	%rbx, %rsi
	callq	do_jump
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB46_22
# BB#21:                                # %if.then.i
	movl	100(%rax), %ecx
	incq	%rcx
	movl	%ecx, 100(%rax)
.LBB46_22:                              # %start_cleanup_deferral.exit
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rcx
	movq	96(%rax), %rdx
	movq	8(%rcx), %rsi
	movq	40(%r15), %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	expand_return
	movq	%rbx, %rdi
	callq	emit_label
	movq	current_function_decl(%rip), %rax
	movq	8(%rax), %rcx
	movq	96(%rax), %rdx
	movq	8(%rcx), %rsi
	movq	48(%r15), %rcx
	movq	$48, %rdi
	xorq	%rax, %rax
	callq	build
	movl	16(%rax), %ecx
	orq	$256, %rcx              # imm = 0x100
	movl	%ecx, 16(%rax)
	movq	%rax, %rdi
	callq	expand_return
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB46_24
# BB#23:                                # %if.then.i456
	movl	100(%rax), %ecx
	decq	%rcx
	movl	%ecx, 100(%rax)
.LBB46_24:                              # %if.end353
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB46_25:                              # %if.end107
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rdi
	movq	144(%rdi), %r14
	testq	%r14, %r14
	jne	.LBB46_27
# BB#26:                                # %cond.false
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rdi
	movq	144(%rdi), %r14
.LBB46_27:                              # %cond.end
	testq	%r15, %r15
	je	.LBB46_62
# BB#28:                                # %land.lhs.true123
	movq	8(%r15), %rax
	movl	60(%rax), %ecx
	andq	$65024, %rcx            # imm = 0xFE00
	cmpq	$26112, %rcx            # imm = 0x6600
	jne	.LBB46_54
# BB#29:                                # %land.lhs.true130
	movzbl	(%r14), %ecx
	cmpq	$61, %rcx
	jne	.LBB46_54
# BB#30:                                # %if.then134
	movq	%rax, %rdi
	callq	int_size_in_bytes
	movl	target_flags(%rip), %esi
	movq	%rax, -88(%rbp)         # 8-byte Spill
	andq	$33554432, %rsi         # imm = 0x2000000
	movq	$23, %rcx
	movq	%rsi, %rdi
	shrq	%cl, %rdi
	addq	$4, %rdi
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rdi
	leaq	-1(%rax,%rdi), %rax
	xorq	%rdx, %rdx
	divq	%rdi
	movq	8(%r15), %rcx
	movl	64(%rcx), %r13d
	movq	$20, %rcx
	shrq	%cl, %rsi
	addq	$32, %rsi
	movabsq	$4294967264, %rbx       # imm = 0xFFFFFFE0
	movq	%rsi, %rcx
	andq	%rbx, %rcx
	cmpq	%rcx, %r13
	jb	.LBB46_32
# BB#31:                                # %if.then134
	movq	%rsi, %r13
.LBB46_32:                              # %if.then134
	movq	$32, %rcx
	movq	%rax, %rdx
	movq	%rax, -96(%rbp)         # 8-byte Spill
	shlq	%cl, %rdx
	movq	$29, %rcx
	sarq	%cl, %rdx
	addq	$15, %rdx
	andq	$-16, %rdx
	movq	%rsp, %r12
	subq	%rdx, %r12
	movq	%r12, %rsp
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	callq	expand_expr
	movq	%rax, -80(%rbp)         # 8-byte Spill
	movq	-88(%rbp), %rax         # 8-byte Reload
	testq	%rax, %rax
	je	.LBB46_2
# BB#33:                                # %for.cond.preheader
	movq	%r12, -72(%rbp)         # 8-byte Spill
	movq	%r14, -104(%rbp)        # 8-byte Spill
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	testq	%rax, %rax
	je	.LBB46_41
# BB#34:                                # %for.body.lr.ph
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	$0, -48(%rbp)           # 8-byte Folded Spill
	movq	$0, -56(%rbp)           # 8-byte Folded Spill
	xorq	%r12, %r12
	.align	16, 0x90
.LBB46_35:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	target_flags(%rip), %edx
	movq	$20, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	andq	$32, %rax
	addq	$32, %rax
	andq	%rbx, %rax
	decq	%rax
	testq	%r12, %rax
	je	.LBB46_37
# BB#36:                                # %for.body
                                        #   in Loop: Header=BB46_35 Depth=1
	testq	%r12, %r12
	jne	.LBB46_38
.LBB46_37:                              # %if.then182
                                        #   in Loop: Header=BB46_35 Depth=1
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movl	target_flags(%rip), %esi
	movq	%rax, %rdi
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	$20, %rcx
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%rbx, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rsi
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	%rdi, (%rcx,%rax,8)
	movzbl	2(%rdi), %eax
	movq	const_tiny_rtx(,%rax,8), %rsi
	callq	emit_move_insn
	movl	target_flags(%rip), %edx
.LBB46_38:                              # %if.end195
                                        #   in Loop: Header=BB46_35 Depth=1
	movq	$20, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	andq	$32, %rsi
	addq	$32, %rsi
	andq	%rbx, %rsi
	leaq	-1(%rsi), %rax
	testq	%r12, %rax
	jne	.LBB46_40
# BB#39:                                # %if.then203
                                        #   in Loop: Header=BB46_35 Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rsi
	movq	$51, %rdx
	movq	-80(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	operand_subword_force
	movl	target_flags(%rip), %edx
	movq	%rax, -56(%rbp)         # 8-byte Spill
.LBB46_40:                              # %if.end211
                                        #   in Loop: Header=BB46_35 Depth=1
	movq	$20, %rcx
	shrq	%cl, %rdx
	andq	$32, %rdx
	addq	$32, %rdx
	movl	word_mode(%rip), %r15d
	andq	%rbx, %rdx
	leaq	-1(%rdx), %r14
	andq	%r12, %r14
	subq	$16, %rsp
	movq	%r15, (%rsp)
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%rdx, 8(%rsp)
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %r9
	callq	extract_bit_field
	addq	$16, %rsp
	movl	target_flags(%rip), %r9d
	movq	$20, %rcx
	shrq	%cl, %r9
	andq	$32, %r9
	addq	$32, %r9
	andq	%rbx, %r9
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%r15, %rcx
	movq	%rax, %r8
	callq	store_bit_field
	addq	%r13, %r12
	movq	-64(%rbp), %rax         # 8-byte Reload
	cmpq	%rax, %r12
	jb	.LBB46_35
.LBB46_41:                              # %for.end
	movl	class_narrowest_mode+4(%rip), %r13d
	testq	%r13, %r13
	movq	-104(%rbp), %rdx        # 8-byte Reload
	movl	target_flags(%rip), %r15d
	movq	-88(%rbp), %rsi         # 8-byte Reload
	je	.LBB46_44
	.align	16, 0x90
.LBB46_42:                              # %for.body241
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	mode_size(%rax), %ecx
	cmpq	%rsi, %rcx
	jae	.LBB46_45
# BB#43:                                # %for.inc249
                                        #   in Loop: Header=BB46_42 Depth=1
	movzbl	mode_wider_mode(%rax), %r13d
	testq	%r13, %r13
	jne	.LBB46_42
.LBB46_44:                              # %if.then256
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_return, %rcx
	movq	$3202, %rsi             # imm = 0xC82
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.LBB46_2:                               # %if.then
	callq	get_last_insn
	movq	%rax, %r14
	callq	clobber_return_register
	movq	cfun(%rip), %rax
	movq	192(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB46_4
# BB#3:                                 # %cond.false.i.i
	movq	200(%rax), %rbx
.LBB46_4:                               # %cond.end.i.i
	callq	clear_pending_stack_adjust
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	testq	%rbx, %rbx
	jne	.LBB46_6
# BB#5:                                 # %if.then.i.i
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	%rbx, 200(%rcx)
.LBB46_6:                               # %expand_null_return.exit
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	expand_goto_internal  # TAILCALL
.LBB46_54:                              # %land.lhs.true291
	movzbl	16(%rax), %eax
	cmpq	$5, %rax
	je	.LBB46_62
# BB#55:                                # %land.lhs.true300
	movzbl	(%r14), %eax
	cmpq	$39, %rax
	je	.LBB46_57
# BB#56:                                # %land.lhs.true300
	cmpq	$61, %rax
	jne	.LBB46_62
.LBB46_57:                              # %if.then310
	movq	8(%rdi), %rdi
	movl	16(%rdi), %eax
	movq	$10, %rcx
	movq	%rax, %rbx
	shrq	%cl, %rbx
	movl	60(%rdi), %esi
	movq	$19, %rcx
	shrq	%cl, %rsi
	andq	$255, %rax
	xorq	%rdx, %rdx
	cmpq	$20, %rax
	jne	.LBB46_61
# BB#58:                                # %land.rhs
	cmpq	$0, 8(%rdi)
	je	.LBB46_59
# BB#60:                                # %land.rhs
	movabsq	$1, %rdx
	jmp	.LBB46_61
.LBB46_62:                              # %if.else350
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	expand_expr
	callq	emit_queue
	movq	%r14, %rdi
	jmp	.LBB46_53
.LBB46_45:                              # %if.end257
	movb	%r13b, 2(%rdx)
	movslq	word_mode(%rip), %rdi
	movzbl	mode_size(%rdi), %eax
	cmpq	%rax, %rcx
	jb	.LBB46_47
# BB#46:                                # %select.mid
	movq	%r13, %rdi
.LBB46_47:                              # %select.end
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	-96(%rbp), %rbx         # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	callq	gen_reg_rtx
	movq	%rax, -56(%rbp)         # 8-byte Spill
	callq	emit_queue
	testq	%rbx, %rbx
	movq	-72(%rbp), %r12         # 8-byte Reload
	jle	.LBB46_50
# BB#48:                                # %for.body276.lr.ph
	movq	$23, %rcx
	shrq	%cl, %r15
	andq	$4, %r15
	addq	$4, %r15
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	andq	%rax, %r15
	movq	-88(%rbp), %rax         # 8-byte Reload
	leaq	-1(%rax,%r15), %rax
	xorq	%rbx, %rbx
	xorq	%rdx, %rdx
	divq	%r15
	movq	%rax, %r15
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r15
	.align	16, 0x90
.LBB46_49:                              # %for.body276
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	-56(%rbp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	-48(%rbp), %rcx         # 8-byte Reload
	callq	operand_subword
	movq	(%r12,%rbx,8), %rsi
	movq	%rax, %rdi
	callq	emit_move_insn
	incq	%rbx
	movq	%rbx, %rax
	andq	%r14, %rax
	cmpq	%r15, %rax
	jne	.LBB46_49
.LBB46_50:                              # %for.end282
	movq	-56(%rbp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	-48(%rbp), %rcx         # 8-byte Reload
	andq	%rax, %rcx
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jne	.LBB46_52
# BB#51:
	movq	%rsi, %rdi
	jmp	.LBB46_53
.LBB46_52:                              # %if.then285
	movq	%r13, %rdi
	callq	gen_lowpart
	movq	%rax, %rdi
	jmp	.LBB46_53
.LBB46_59:
	movabsq	$0, %rdx
.LBB46_61:                              # %land.end
	andq	$2, %rbx
	andq	$4, %rsi
	movq	$3, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rsi
	orq	%rdx, %rsi
	orq	$1, %rsi
	callq	build_qualified_type
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rax, %rdi
	callq	assign_temp
	movzbl	2(%rax), %edx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	expand_expr
	movq	%rax, %rdi
	callq	force_not_mem
	movq	%rax, %rbx
	callq	emit_queue
	movq	%rbx, %rdi
.LBB46_53:                              # %if.end287
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	jmp	expand_value_return  # TAILCALL
.Ltmp352:
	.size	expand_return, .Ltmp352-expand_return
	.cfi_endproc

	.globl	start_cleanup_deferral
	.align	16, 0x90
	.type	start_cleanup_deferral,@function
start_cleanup_deferral:                 # @start_cleanup_deferral
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB47_2
# BB#1:                                 # %if.then
	movl	100(%rax), %ecx
	incq	%rcx
	movl	%ecx, 100(%rax)
.LBB47_2:                               # %if.end
	retq
.Ltmp353:
	.size	start_cleanup_deferral, .Ltmp353-start_cleanup_deferral
	.cfi_endproc

	.globl	end_cleanup_deferral
	.align	16, 0x90
	.type	end_cleanup_deferral,@function
end_cleanup_deferral:                   # @end_cleanup_deferral
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB48_2
# BB#1:                                 # %if.then
	movl	100(%rax), %ecx
	decq	%rcx
	movl	%ecx, 100(%rax)
.LBB48_2:                               # %if.end
	retq
.Ltmp354:
	.size	end_cleanup_deferral, .Ltmp354-end_cleanup_deferral
	.cfi_endproc

	.align	16, 0x90
	.type	expand_value_return,@function
expand_value_return:                    # @expand_value_return
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp359:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp360:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp361:
	.cfi_def_cfa_offset 32
.Ltmp362:
	.cfi_offset %rbx, -32
.Ltmp363:
	.cfi_offset %r14, -24
.Ltmp364:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	callq	get_last_insn
	movq	current_function_decl(%rip), %rcx
	movq	96(%rcx), %rdi
	movq	144(%rdi), %rbx
	movq	%rax, %r14
	testq	%rbx, %rbx
	jne	.LBB49_2
# BB#1:                                 # %cond.false
	xorq	%rsi, %rsi
	callq	make_decl_rtl
	movq	current_function_decl(%rip), %rax
	movq	96(%rax), %rdi
	movq	144(%rdi), %rbx
.LBB49_2:                               # %cond.end
	cmpq	%r15, %rbx
	je	.LBB49_6
# BB#3:                                 # %if.then
	movzbl	(%rbx), %eax
	cmpq	$39, %rax
	jne	.LBB49_5
# BB#4:                                 # %if.then16
	movq	8(%rdi), %rdi
	callq	int_size_in_bytes
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	emit_group_load
	jmp	.LBB49_6
.LBB49_5:                               # %if.else
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	emit_move_insn
.LBB49_6:                               # %if.end19
	movq	cfun(%rip), %rax
	movq	192(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB49_8
# BB#7:                                 # %cond.false.i
	movq	200(%rax), %rbx
.LBB49_8:                               # %cond.end.i
	callq	clear_pending_stack_adjust
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	$0, 56(%rax)
	testq	%rbx, %rbx
	jne	.LBB49_10
# BB#9:                                 # %if.then.i
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, %rbx
	movq	%rbx, 200(%rcx)
.LBB49_10:                              # %expand_null_return_1.exit
	xorq	%rdi, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	expand_goto_internal  # TAILCALL
.Ltmp365:
	.size	expand_value_return, .Ltmp365-expand_value_return
	.cfi_endproc

	.globl	drop_through_at_end_p
	.align	16, 0x90
	.type	drop_through_at_end_p,@function
drop_through_at_end_p:                  # @drop_through_at_end_p
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp367:
	.cfi_def_cfa_offset 16
	callq	get_last_insn
	movq	%rax, %rcx
	xorq	%rax, %rax
	jmp	.LBB50_1
	.align	16, 0x90
.LBB50_3:                               # %while.body
                                        #   in Loop: Header=BB50_1 Depth=1
	movq	16(%rcx), %rcx
.LBB50_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB50_7
# BB#2:                                 # %land.rhs
                                        #   in Loop: Header=BB50_1 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$37, %rdx
	je	.LBB50_3
# BB#4:                                 # %land.rhs2
	cmpq	$35, %rdx
	je	.LBB50_5
# BB#6:                                 # %land.rhs2
	movabsq	$1, %rax
.LBB50_7:                               # %land.end6
	popq	%rdx
	retq
.LBB50_5:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp368:
	.size	drop_through_at_end_p, .Ltmp368-drop_through_at_end_p
	.cfi_endproc

	.globl	optimize_tail_recursion
	.align	16, 0x90
	.type	optimize_tail_recursion,@function
optimize_tail_recursion:                # @optimize_tail_recursion
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp371:
	.cfi_def_cfa_offset 16
.Ltmp372:
	.cfi_offset %rbx, -16
	movq	current_function_decl(%rip), %rax
	movq	88(%rax), %rax
	movq	%rsi, %rbx
	movq	%rax, %rsi
	callq	tail_recursion_args
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rcx
	je	.LBB51_4
# BB#1:                                 # %if.then
	movq	cfun(%rip), %rax
	cmpq	$0, 232(%rax)
	jne	.LBB51_3
# BB#2:                                 # %if.then2
	callq	gen_label_rtx
	movq	cfun(%rip), %rcx
	movq	%rax, 232(%rcx)
	movq	cfun(%rip), %rax
	movq	232(%rax), %rdi
	movq	240(%rax), %rsi
	callq	emit_label_after
.LBB51_3:                               # %if.end
	callq	emit_queue
	movq	cfun(%rip), %rax
	movq	232(%rax), %rsi
	xorq	%rdi, %rdi
	movq	%rbx, %rdx
	callq	expand_goto_internal
	callq	emit_barrier
	movq	$1, %rax
.LBB51_4:                               # %return
	popq	%rbx
	retq
.Ltmp373:
	.size	optimize_tail_recursion, .Ltmp373-optimize_tail_recursion
	.cfi_endproc

	.align	16, 0x90
	.type	tail_recursion_args,@function
tail_recursion_args:                    # @tail_recursion_args
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp377:
	.cfi_def_cfa_offset 16
.Ltmp378:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp379:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp380:
	.cfi_offset %rbx, -56
.Ltmp381:
	.cfi_offset %r12, -48
.Ltmp382:
	.cfi_offset %r13, -40
.Ltmp383:
	.cfi_offset %r14, -32
.Ltmp384:
	.cfi_offset %r15, -24
	testq	%rsi, %rsi
	movabsq	$0, %rdx
	movabsq	$1, %rcx
	movq	%rdx, %r8
	je	.LBB52_2
# BB#1:                                 # %entry
	movq	%rcx, %r8
.LBB52_2:                               # %entry
	xorq	%r15, %r15
	testq	%rdi, %rdi
	je	.LBB52_4
# BB#3:                                 # %entry
	movq	%rcx, %rdx
.LBB52_4:                               # %entry
	je	.LBB52_22
# BB#5:                                 # %entry
	testq	%rsi, %rsi
	je	.LBB52_22
# BB#6:
	movabsq	$0, %r14
	movabsq	$1, %r12
	movq	%rsi, %rbx
	movq	%rsi, -56(%rbp)         # 8-byte Spill
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	movq	%rdi, %r13
	.align	16, 0x90
.LBB52_7:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rax
	movq	8(%rax), %rax
	movq	8(%rbx), %rcx
	movq	128(%rax), %rax
	movq	128(%rcx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB52_8
# BB#9:                                 # %if.end
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB52_11
# BB#10:                                # %cond.false
                                        #   in Loop: Header=BB52_7 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB52_11:                              # %cond.end
                                        #   in Loop: Header=BB52_7 Depth=1
	movzbl	(%rax), %eax
	cmpq	$61, %rax
	jne	.LBB52_12
# BB#13:                                # %lor.lhs.false
                                        #   in Loop: Header=BB52_7 Depth=1
	movzbl	48(%rbx), %eax
	cmpq	$51, %rax
	je	.LBB52_14
# BB#15:                                # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	(%rbx), %rbx
	movq	(%r13), %r13
	testq	%rbx, %rbx
	movq	%r14, %r8
	je	.LBB52_17
# BB#16:                                # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	%r12, %r8
.LBB52_17:                              # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	incq	%r15
	testq	%r13, %r13
	movq	%r14, %rdx
	je	.LBB52_19
# BB#18:                                # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	movq	%r12, %rdx
.LBB52_19:                              # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	je	.LBB52_21
# BB#20:                                # %for.inc
                                        #   in Loop: Header=BB52_7 Depth=1
	testq	%rbx, %rbx
	jne	.LBB52_7
.LBB52_21:                              # %for.cond.for.end_crit_edge
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	-56(%rbp), %rsi         # 8-byte Reload
	movq	-64(%rbp), %rdi         # 8-byte Reload
.LBB52_22:                              # %for.end
	movq	%rsi, -56(%rbp)         # 8-byte Spill
	xorq	%rax, %rax
	testq	%rdx, %rdx
	jne	.LBB52_49
# BB#23:                                # %for.end
	testq	%r8, %r8
	jne	.LBB52_49
# BB#24:                                # %if.end26
	movq	%rsp, %rcx
	leaq	15(,%r15,8), %rax
	andq	$-16, %rax
	subq	%rax, %rcx
	movq	%rcx, -48(%rbp)         # 8-byte Spill
	movq	%rcx, %rsp
	testq	%rdi, %rdi
	movq	%rdi, -64(%rbp)         # 8-byte Spill
	je	.LBB52_35
# BB#25:
	movq	-48(%rbp), %r14         # 8-byte Reload
	movq	-64(%rbp), %r15         # 8-byte Reload
	movq	%r15, %rbx
	.align	16, 0x90
.LBB52_26:                              # %for.body29
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, (%r14)
	movq	(%rbx), %rbx
	addq	$8, %r14
	testq	%rbx, %rbx
	jne	.LBB52_26
# BB#27:                                # %for.cond37.preheader
	testq	%r15, %r15
	je	.LBB52_35
# BB#28:
	xorq	%r14, %r14
	movq	-64(%rbp), %r12         # 8-byte Reload
	.align	16, 0x90
.LBB52_29:                              # %for.cond40.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB52_31 Depth 2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB52_43
# BB#30:                                # %for.body43.lr.ph
                                        #   in Loop: Header=BB52_29 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	movq	(%rax,%r14,8), %r15
	xorq	%r13, %r13
	movq	-56(%rbp), %rbx         # 8-byte Reload
	.align	16, 0x90
.LBB52_31:                              # %for.body43
                                        #   Parent Loop BB52_29 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB52_33
# BB#32:                                # %cond.false50
                                        #   in Loop: Header=BB52_31 Depth=2
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB52_33:                              # %cond.end53
                                        #   in Loop: Header=BB52_31 Depth=2
	movq	%r15, %rsi
	callq	reg_mentioned_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB52_42
# BB#34:                                # %for.inc61
                                        #   in Loop: Header=BB52_31 Depth=2
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rbx), %rbx
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB52_31
	jmp	.LBB52_43
	.align	16, 0x90
.LBB52_42:                              # %if.then67
                                        #   in Loop: Header=BB52_29 Depth=1
	movq	%r15, %rdi
	callq	copy_to_reg
	movq	-48(%rbp), %rcx         # 8-byte Reload
	movq	%rax, (%rcx,%r14,8)
.LBB52_43:                              # %for.inc74
                                        #   in Loop: Header=BB52_29 Depth=1
	movq	(%r12), %r12
	incq	%r14
	testq	%r12, %r12
	jne	.LBB52_29
.LBB52_35:                              # %for.cond79.preheader
	movq	-56(%rbp), %rbx         # 8-byte Reload
	testq	%rbx, %rbx
	movq	-64(%rbp), %r15         # 8-byte Reload
	movq	-48(%rbp), %r12         # 8-byte Reload
	je	.LBB52_48
	.align	16, 0x90
.LBB52_36:                              # %for.body81
                                        # =>This Inner Loop Header: Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB52_38
# BB#37:                                # %cond.false88
                                        #   in Loop: Header=BB52_36 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB52_38:                              # %cond.end91
                                        #   in Loop: Header=BB52_36 Depth=1
	movq	(%r12), %r14
	movl	(%rdi), %eax
	movl	(%r14), %ecx
	xorq	%rax, %rcx
	testq	$16711680, %rcx         # imm = 0xFF0000
	jne	.LBB52_44
# BB#39:                                # %if.then102
                                        #   in Loop: Header=BB52_36 Depth=1
	testq	%rdi, %rdi
	jne	.LBB52_41
# BB#40:                                # %cond.false109
                                        #   in Loop: Header=BB52_36 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB52_41:                              # %cond.end112
                                        #   in Loop: Header=BB52_36 Depth=1
	movq	%r14, %rsi
	callq	emit_move_insn
	jmp	.LBB52_47
	.align	16, 0x90
.LBB52_44:                              # %if.else
                                        #   in Loop: Header=BB52_36 Depth=1
	testq	%rdi, %rdi
	jne	.LBB52_46
# BB#45:                                # %cond.false123
                                        #   in Loop: Header=BB52_36 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB52_46:                              # %cond.end126
                                        #   in Loop: Header=BB52_36 Depth=1
	movq	32(%r15), %rax
	movq	8(%rax), %rax
	movl	16(%rax), %edx
	movq	$13, %rcx
	shrq	%cl, %rdx
	andq	$1, %rdx
	movq	%r14, %rsi
	callq	convert_move
.LBB52_47:                              # %for.inc139
                                        #   in Loop: Header=BB52_36 Depth=1
	movq	(%rbx), %rbx
	movq	(%r15), %r15
	addq	$8, %r12
	testq	%rbx, %rbx
	jne	.LBB52_36
.LBB52_48:                              # %for.end145
	callq	free_temp_slots
	movq	$1, %rax
	jmp	.LBB52_49
.LBB52_8:
	xorq	%rax, %rax
	jmp	.LBB52_49
.LBB52_12:
	xorq	%rax, %rax
	jmp	.LBB52_49
.LBB52_14:
	xorq	%rax, %rax
.LBB52_49:                              # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp385:
	.size	tail_recursion_args, .Ltmp385-tail_recursion_args
	.cfi_endproc

	.globl	expand_start_bindings_and_block
	.align	16, 0x90
	.type	expand_start_bindings_and_block,@function
expand_start_bindings_and_block:        # @expand_start_bindings_and_block
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp391:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp392:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp393:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp394:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp395:
	.cfi_def_cfa_offset 48
.Ltmp396:
	.cfi_offset %rbx, -40
.Ltmp397:
	.cfi_offset %r12, -32
.Ltmp398:
	.cfi_offset %r14, -24
.Ltmp399:
	.cfi_offset %r15, -16
	movq	stmt_obstack+32(%rip), %rcx
	movq	stmt_obstack+24(%rip), %rax
	movq	%rsi, %r14
	movq	%rdi, %r15
	subq	%rax, %rcx
	cmpq	$119, %rcx
	jg	.LBB53_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rax
.LBB53_2:                               # %if.end
	addq	$120, %rax
	movq	%rax, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB53_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, stmt_obstack+80(%rip)
.LBB53_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB53_6
# BB#5:                                 # %if.then29
	movq	%rax, stmt_obstack+24(%rip)
	movq	%rax, %rcx
.LBB53_6:                               # %if.end32
	movq	%rcx, stmt_obstack+16(%rip)
	movq	%r15, %rax
	andq	$2, %rax
	je	.LBB53_8
# BB#7:                                 # %if.end32
	testq	%r14, %r14
	jne	.LBB53_18
.LBB53_8:                               # %if.end47
	andq	$1, %r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB53_9
# BB#10:                                # %if.else
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, %r12
	jmp	.LBB53_11
.LBB53_9:                               # %if.then49
	movabsq	$4294967198, %rsi       # imm = 0xFFFFFF9E
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%rax, %r12
	movq	%r14, 32(%r12)
.LBB53_11:                              # %if.end51
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 16(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 64(%rbx)
	movq	$0, 88(%rbx)
	movq	cfun(%rip), %rax
	leaq	64(%rbx), %r14
	movl	336(%rax), %eax
	movl	%eax, 96(%rbx)
	xorq	%rax, %rax
	movl	%eax, 100(%rbx)
	movq	%r12, 104(%rbx)
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	%r14, 112(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB53_14
# BB#12:                                # %land.lhs.true76
	movq	64(%rax), %rsi
	movq	72(%rax), %rdx
	movq	%rsi, %rax
	orq	%rdx, %rax
	jne	.LBB53_13
.LBB53_14:                              # %if.else106
	movq	$0, 72(%rbx)
	jmp	.LBB53_15
.LBB53_13:                              # %if.then91
	xorq	%rdi, %rdi
	callq	tree_cons
	movq	%rax, 72(%rbx)
.LBB53_15:                              # %if.end110
	movq	$0, 80(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	8(%rax), %rax
	movq	%rax, 56(%rbx)
	movq	%r12, 48(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rcx
	movl	52(%rcx), %edx
	incq	%rdx
	xorq	%rax, %rax
	testq	%r15, %r15
	movl	%edx, 52(%rcx)
	movl	%edx, 32(%rbx)
	je	.LBB53_17
# BB#16:                                # %cond.true
	callq	gen_label_rtx
.LBB53_17:                              # %cond.end
	movq	%rax, 24(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 40(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	push_temp_slots  # TAILCALL
.LBB53_18:                              # %if.then46
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_start_bindings_and_block, %rcx
	movq	$3395, %rsi             # imm = 0xD43
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp400:
	.size	expand_start_bindings_and_block, .Ltmp400-expand_start_bindings_and_block
	.cfi_endproc

	.globl	expand_start_target_temps
	.align	16, 0x90
	.type	expand_start_target_temps,@function
expand_start_target_temps:              # @expand_start_target_temps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp402:
	.cfi_def_cfa_offset 16
	callq	push_temp_slots
	movq	$2, %rdi
	xorq	%rsi, %rsi
	callq	expand_start_bindings_and_block
	movq	cfun(%rip), %rax
	movl	328(%rax), %ecx
	movl	%ecx, 336(%rax)
	popq	%rax
	retq
.Ltmp403:
	.size	expand_start_target_temps, .Ltmp403-expand_start_target_temps
	.cfi_endproc

	.globl	expand_end_target_temps
	.align	16, 0x90
	.type	expand_end_target_temps,@function
expand_end_target_temps:                # @expand_end_target_temps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp405:
	.cfi_def_cfa_offset 16
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	expand_end_bindings
	popq	%rax
	jmp	pop_temp_slots  # TAILCALL
.Ltmp406:
	.size	expand_end_target_temps, .Ltmp406-expand_end_target_temps
	.cfi_endproc

	.globl	expand_end_bindings
	.align	16, 0x90
	.type	expand_end_bindings,@function
expand_end_bindings:                    # @expand_end_bindings
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp413:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp414:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp415:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp416:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp417:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp418:
	.cfi_def_cfa_offset 96
.Ltmp419:
	.cfi_offset %rbx, -48
.Ltmp420:
	.cfi_offset %r12, -40
.Ltmp421:
	.cfi_offset %r13, -32
.Ltmp422:
	.cfi_offset %r14, -24
.Ltmp423:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	warn_unused_variable(%rip), %ecx
	movq	(%rax), %r12
	movq	%rdx, %r13
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %rbx
	testq	%rcx, %rcx
	je	.LBB56_9
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB56_9
# BB#2:
	movabsq	$.L.str16, %r14
	.align	16, 0x90
.LBB56_3:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	$65791, %rax            # imm = 0x100FF
	cmpq	$34, %rax
	jne	.LBB56_8
# BB#4:                                 # %land.lhs.true6.i
                                        #   in Loop: Header=BB56_3 Depth=1
	movq	48(%rbx), %rax
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB56_8
# BB#5:                                 # %land.lhs.true12.i
                                        #   in Loop: Header=BB56_3 Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB56_8
# BB#6:                                 # %land.lhs.true12.i
                                        #   in Loop: Header=BB56_3 Depth=1
	andq	$4194304, %rax          # imm = 0x400000
	jne	.LBB56_8
# BB#7:                                 # %if.then22.i
                                        #   in Loop: Header=BB56_3 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB56_8:                               # %for.inc.i
                                        #   in Loop: Header=BB56_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB56_3
.LBB56_9:                               # %warn_about_unused_variables.exit
	cmpq	$0, 24(%r12)
	je	.LBB56_11
# BB#10:                                # %if.then
	callq	do_pending_stack_adjust
	movq	24(%r12), %rdi
	callq	emit_label
.LBB56_11:                              # %if.end
	movq	cfun(%rip), %rax
	movl	152(%rax), %ecx
	movl	88(%r12), %edx
	cmpq	%rdx, %rcx
	je	.LBB56_35
# BB#12:                                # %land.lhs.true
	cmpq	$0, 160(%rax)
	je	.LBB56_35
# BB#13:                                # %land.lhs.true3
	cmpq	$0, 8(%r12)
	je	.LBB56_14
# BB#15:                                # %cond.false
	cmpq	$0, 64(%r12)
	jne	.LBB56_17
# BB#16:                                # %lor.lhs.false
	cmpq	$0, 40(%r12)
	jne	.LBB56_17
	jmp	.LBB56_35
.LBB56_14:                              # %cond.true
	movzbl	425(%rax), %eax
	testq	$1, %rax
	je	.LBB56_35
.LBB56_17:                              # %if.then12
	callq	gen_label_rtx
	cmpq	$0, 8(%r12)
	movq	%rax, (%rsp)            # 8-byte Spill
	je	.LBB56_21
# BB#18:                                # %if.then.i
	movq	cfun(%rip), %rax
	movq	168(%rax), %rbx
	jmp	.LBB56_20
	.align	16, 0x90
.LBB56_19:                              # %for.body.i231
                                        #   in Loop: Header=BB56_20 Depth=1
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	8(%rbx), %rdi
	movq	%rax, %r14
	movq	%r14, %rsi
	callq	emit_move_insn
	callq	start_sequence
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r14
	callq	end_sequence
	movq	48(%r12), %rsi
	movq	%r14, %rdi
	callq	emit_insns_before
	movq	16(%rbx), %rbx
.LBB56_20:                              # %for.body.i231
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	jne	.LBB56_19
.LBB56_21:                              # %if.end.i
	movq	%r13, 16(%rsp)          # 8-byte Spill
	callq	do_pending_stack_adjust
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	cfun(%rip), %r14
	movq	160(%r14), %r13
	xorq	%rcx, %rcx
	testq	%r13, %r13
	je	.LBB56_34
# BB#22:                                # %for.body16.lr.ph.i
	movq	%r12, 24(%rsp)          # 8-byte Spill
	addq	$168, %r14
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB56_23:                              # %for.body16.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rax
	movzbl	49(%rax), %eax
	movq	(%r14), %r14
	testq	$16, %rax
	jne	.LBB56_25
# BB#24:                                # %if.then18.i
                                        #   in Loop: Header=BB56_23 Depth=1
	movq	8(%r14), %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	48(%rax), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	callq	gen_label_rtx
	movq	%rax, %r12
	movl	(%r12), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r12)
	callq	start_sequence
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	movq	%r12, %rdx
	callq	gen_rtx_fmt_u00
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	movq	%rbx, %rdi
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	emit_insns_before
	movq	%r12, %rdi
	callq	emit_label
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r12, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	global_rtl+56(%rip), %rdi
	movq	global_rtl+32(%rip), %rsi
	movq	%rax, 40(%rsp)          # 8-byte Spill
	callq	emit_move_insn
	movq	32(%r13), %rdi
	callq	expand_goto
.LBB56_25:                              # %for.inc30.i
                                        #   in Loop: Header=BB56_23 Depth=1
	movq	(%r13), %r13
	addq	$16, %r14
	testq	%r13, %r13
	jne	.LBB56_23
# BB#26:                                # %for.end34.i
	movq	cfun(%rip), %rbx
	movq	160(%rbx), %r13
	testq	%r13, %r13
	je	.LBB56_33
# BB#27:                                # %for.body39.lr.ph.i
	addq	$168, %rbx
	xorq	%rax, %rax
	.align	16, 0x90
.LBB56_28:                              # %for.body39.i
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rcx
	movzbl	49(%rcx), %ecx
	movq	(%rbx), %rbx
	testq	$16, %rcx
	je	.LBB56_30
# BB#29:                                # %if.then49.i
                                        #   in Loop: Header=BB56_28 Depth=1
	movq	8(%rbx), %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	48(%rax), %r12
	callq	gen_label_rtx
	movq	%rax, %r14
	movl	(%r14), %eax
	orq	$268435456, %rax        # imm = 0x10000000
	movl	%eax, (%r14)
	callq	start_sequence
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r15
	callq	end_sequence
	movq	%r15, %rdi
	movq	%r12, %rsi
	callq	emit_insns_before
	movq	%r14, %rdi
	callq	emit_label
	movq	$3, %rdi
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	gen_rtx_fmt_ee
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$1, %rax
.LBB56_30:                              # %for.inc60.i
                                        #   in Loop: Header=BB56_28 Depth=1
	movq	(%r13), %r13
	addq	$16, %rbx
	testq	%r13, %r13
	jne	.LBB56_28
# BB#31:                                # %for.end66.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB56_33
# BB#32:                                # %if.then68.i
	movq	global_rtl+56(%rip), %rdi
	movq	global_rtl+32(%rip), %rsi
	callq	emit_move_insn
	callq	expand_builtin_trap
.LBB56_33:                              # %expand_nl_goto_receivers.exit
	movq	24(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
.LBB56_34:                              # %expand_nl_goto_receivers.exit
	movq	cfun(%rip), %rax
	movq	%rcx, 176(%rax)
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	emit_label
	movq	16(%rsp), %r13          # 8-byte Reload
.LBB56_35:                              # %if.end13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB56_38
# BB#36:                                # %lor.lhs.false15
	cmpq	$0, 40(%r12)
	je	.LBB56_37
.LBB56_38:                              # %if.then20
	movq	80(%r12), %rbx
	testq	%rbx, %rbx
	je	.LBB56_43
# BB#39:
	movabsq	$.L.str17, %r14
	.align	16, 0x90
.LBB56_40:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rax
	orq	$4096, 48(%rax)         # imm = 0x1000
	movq	8(%rbx), %rdi
	movzbl	17(%rdi), %eax
	testq	$4, %rax
	je	.LBB56_42
# BB#41:                                # %if.then31
                                        #   in Loop: Header=BB56_40 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	error_with_decl
.LBB56_42:                              # %for.inc
                                        #   in Loop: Header=BB56_40 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB56_40
.LBB56_43:                              # %if.end35
	cmpq	$0, 40(%r12)
	leaq	40(%r12), %r15
	jne	.LBB56_45
	jmp	.LBB56_44
.LBB56_37:                              # %if.end35.thread
	leaq	40(%r12), %r15
.LBB56_44:                              # %lor.lhs.false40
	cmpq	$0, 64(%r12)
	je	.LBB56_61
.LBB56_45:                              # %if.then45
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	72(%rax), %ecx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	56(%rax), %r13
	movq	64(%rax), %r14
	xorq	%rcx, %rcx
	movl	%ecx, 72(%rax)
	callq	get_last_insn
	movzbl	(%rax), %ecx
	cmpq	$37, %rcx
	jne	.LBB56_47
# BB#46:                                # %if.then55
	movq	%rax, %rdi
	callq	prev_nonnote_insn
.LBB56_47:                              # %if.end57
	movq	$1, %rbx
	testq	%rax, %rax
	je	.LBB56_51
# BB#48:                                # %lor.rhs
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB56_49
# BB#50:                                # %lor.rhs
	movabsq	$1, %rbx
	jmp	.LBB56_51
.LBB56_49:
	movabsq	$0, %rbx
.LBB56_51:                              # %lor.end
	movq	64(%r12), %rdi
	xorq	%rsi, %rsi
	movq	%rbx, %rdx
	callq	expand_cleanups
	testq	%rbx, %rbx
	je	.LBB56_53
# BB#52:                                # %if.then66
	callq	do_pending_stack_adjust
.LBB56_53:                              # %if.end67
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	%ecx, 72(%rax)
	movq	%r14, 64(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%r13, 56(%rax)
	testq	%rbx, %rbx
	je	.LBB56_60
# BB#54:                                # %land.lhs.true75
	movq	(%r15), %rsi
	testq	%rsi, %rsi
	je	.LBB56_60
# BB#55:                                # %if.then80
	cmpq	$0, 8(%r12)
	jne	.LBB56_56
# BB#57:                                # %if.then80
	movabsq	$1, %rdi
	jmp	.LBB56_58
.LBB56_56:
	movabsq	$0, %rdi
.LBB56_58:                              # %if.then80
	xorq	%rdx, %rdx
	callq	emit_stack_restore
	movq	cfun(%rip), %rsi
	cmpq	$0, 168(%rsi)
	je	.LBB56_60
# BB#59:                                # %if.then87
	addq	$184, %rsi
	movq	$2, %rdi
	xorq	%rdx, %rdx
	callq	emit_stack_save
.LBB56_60:                              # %if.end89
	movq	(%r15), %rsi
	movq	48(%r12), %rcx
	movq	64(%r12), %rdx
	movq	%r12, %rdi
	movq	16(%rsp), %r8           # 8-byte Reload
	callq	fixup_gotos
.LBB56_61:                              # %if.end98
	movq	8(%rsp), %rax           # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB56_63
# BB#62:                                # %if.then100
	movabsq	$4294967199, %rsi       # imm = 0xFFFFFF9F
	xorq	%rdi, %rdi
	callq	emit_note
	movq	48(%r12), %rcx
	movq	32(%rcx), %rcx
	movq	%rcx, 32(%rax)
	jmp	.LBB56_64
.LBB56_63:                              # %if.else
	movq	48(%r12), %rax
	movabsq	$4294967197, %rcx       # imm = 0xFFFFFF9D
	movl	%ecx, 40(%rax)
.LBB56_64:                              # %if.end113
	movl	96(%r12), %eax
	movq	cfun(%rip), %rcx
	movl	%eax, 336(%rcx)
	movq	56(%r12), %rax
	movq	8(%rcx), %rcx
	movq	%rax, 8(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %r15
	movabsq	$stmt_obstack, %r14
	jmp	.LBB56_65
	.align	16, 0x90
.LBB56_80:                              # %do.cond.do.body121_crit_edge
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_65:                              # %do.body121
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB56_67
# BB#66:                                # %if.then125
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_67:                              # %if.end131
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB56_69
# BB#68:                                # %if.then134
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_69:                              # %if.end140
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB56_71
# BB#70:                                # %if.then144
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_71:                              # %if.end150
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB56_73
# BB#72:                                # %if.then154
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_73:                              # %if.end160
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB56_75
# BB#74:                                # %if.then163
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB56_75:                              # %if.end169
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB56_78
# BB#76:                                # %if.end169
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB56_78
# BB#77:                                # %if.then178
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB56_79
	.align	16, 0x90
.LBB56_78:                              # %if.else179
                                        #   in Loop: Header=BB56_65 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB56_79:                              # %do.cond
                                        #   in Loop: Header=BB56_65 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB56_80
# BB#81:                                # %do.end183
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	pop_temp_slots  # TAILCALL
.Ltmp424:
	.size	expand_end_bindings, .Ltmp424-expand_end_bindings
	.cfi_endproc

	.globl	is_body_block
	.align	16, 0x90
	.type	is_body_block,@function
is_body_block:                          # @is_body_block
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	16(%rdi), %eax
	cmpq	$4, %rax
	jne	.LBB57_5
# BB#1:                                 # %if.then
	movq	48(%rdi), %rax
	testq	%rax, %rax
	je	.LBB57_5
# BB#2:                                 # %land.lhs.true
	movzbl	16(%rax), %ecx
	cmpq	$4, %rcx
	jne	.LBB57_5
# BB#3:                                 # %if.then6
	movq	48(%rax), %rax
	testq	%rax, %rax
	je	.LBB57_5
# BB#4:                                 # %land.lhs.true10
	movzbl	16(%rax), %eax
	cmpq	$30, %rax
	movq	$1, %rax
	je	.LBB57_6
.LBB57_5:                               # %if.end18
	xorq	%rax, %rax
.LBB57_6:                               # %return
	retq
.Ltmp425:
	.size	is_body_block, .Ltmp425-is_body_block
	.cfi_endproc

	.globl	conditional_context
	.align	16, 0x90
	.type	conditional_context,@function
conditional_context:                    # @conditional_context
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB58_4
# BB#1:                                 # %land.rhs
	movl	100(%rcx), %eax
	testq	%rax, %rax
	je	.LBB58_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB58_4:                               # %land.end
	retq
.LBB58_2:
	movabsq	$0, %rax
	retq
.Ltmp426:
	.size	conditional_context, .Ltmp426-conditional_context
	.cfi_endproc

	.globl	current_nesting_level
	.align	16, 0x90
	.type	current_nesting_level,@function
current_nesting_level:                  # @current_nesting_level
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB59_2
# BB#1:                                 # %cond.true
	movq	8(%rcx), %rax
	movq	(%rax), %rax
.LBB59_2:                               # %cond.end
	retq
.Ltmp427:
	.size	current_nesting_level, .Ltmp427-current_nesting_level
	.cfi_endproc

	.globl	warn_about_unused_variables
	.align	16, 0x90
	.type	warn_about_unused_variables,@function
warn_about_unused_variables:            # @warn_about_unused_variables
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp431:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp432:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp433:
	.cfi_def_cfa_offset 32
.Ltmp434:
	.cfi_offset %rbx, -24
.Ltmp435:
	.cfi_offset %r14, -16
	movl	warn_unused_variable(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB60_9
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	je	.LBB60_9
# BB#2:
	movabsq	$.L.str16, %r14
	.align	16, 0x90
.LBB60_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %eax
	andq	$65791, %rax            # imm = 0x100FF
	cmpq	$34, %rax
	jne	.LBB60_8
# BB#4:                                 # %land.lhs.true6
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	48(%rbx), %rax
	testq	$65536, %rax            # imm = 0x10000
	jne	.LBB60_8
# BB#5:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB60_3 Depth=1
	cmpq	$0, 72(%rbx)
	je	.LBB60_8
# BB#6:                                 # %land.lhs.true12
                                        #   in Loop: Header=BB60_3 Depth=1
	andq	$4194304, %rax          # imm = 0x400000
	jne	.LBB60_8
# BB#7:                                 # %if.then22
                                        #   in Loop: Header=BB60_3 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	warning_with_decl
	.align	16, 0x90
.LBB60_8:                               # %for.inc
                                        #   in Loop: Header=BB60_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB60_3
.LBB60_9:                               # %if.end24
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp436:
	.size	warn_about_unused_variables, .Ltmp436-warn_about_unused_variables
	.cfi_endproc

	.align	16, 0x90
	.type	expand_cleanups,@function
expand_cleanups:                        # @expand_cleanups
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp442:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp443:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp444:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp445:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp446:
	.cfi_def_cfa_offset 48
.Ltmp447:
	.cfi_offset %rbx, -40
.Ltmp448:
	.cfi_offset %r12, -32
.Ltmp449:
	.cfi_offset %r14, -24
.Ltmp450:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB61_22
# BB#1:                                 # %if.then.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %r15
	je	.LBB61_6
# BB#2:                                 # %if.then.lr.ph.if.then.lr.ph.split_crit_edge
	testq	%r12, %r14
	je	.LBB61_3
	.align	16, 0x90
.LBB61_14:                              # %if.then
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB61_16
# BB#15:                                # %if.then5
                                        #   in Loop: Header=BB61_14 Depth=1
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	expand_cleanups
	jmp	.LBB61_21
	.align	16, 0x90
.LBB61_16:                              # %land.lhs.true14
                                        #   in Loop: Header=BB61_14 Depth=1
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	jne	.LBB61_21
# BB#17:                                # %if.then19
                                        #   in Loop: Header=BB61_14 Depth=1
	movzbl	using_eh_for_cleanups_p(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB61_19
# BB#18:                                # %if.then23
                                        #   in Loop: Header=BB61_14 Depth=1
	callq	expand_eh_region_start
	movq	32(%rbx), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	32(%rbx), %rdi
	callq	expand_eh_region_end_fixup
	jmp	.LBB61_20
.LBB61_19:                              # %if.else28
                                        #   in Loop: Header=BB61_14 Depth=1
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
.LBB61_20:                              # %if.end32
                                        #   in Loop: Header=BB61_14 Depth=1
	callq	free_temp_slots
.LBB61_21:                              # %for.inc
                                        #   in Loop: Header=BB61_14 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB61_14
	jmp	.LBB61_22
	.align	16, 0x90
.LBB61_6:                               # %if.then.us
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB61_7
# BB#12:                                # %if.then5.us
                                        #   in Loop: Header=BB61_6 Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rdx
	callq	expand_cleanups
	jmp	.LBB61_13
	.align	16, 0x90
.LBB61_7:                               # %if.else.us
                                        #   in Loop: Header=BB61_6 Depth=1
	movzbl	using_eh_for_cleanups_p(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB61_9
# BB#8:                                 # %if.then10.us
                                        #   in Loop: Header=BB61_6 Depth=1
	callq	expand_eh_region_end_cleanup
.LBB61_9:                               # %if.end.us
                                        #   in Loop: Header=BB61_6 Depth=1
	testq	%r12, %r14
	je	.LBB61_13
# BB#10:                                # %land.lhs.true14.us
                                        #   in Loop: Header=BB61_6 Depth=1
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	jne	.LBB61_13
# BB#11:                                # %if.end32.us
                                        #   in Loop: Header=BB61_6 Depth=1
	movq	32(%rbx), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	callq	free_temp_slots
	.align	16, 0x90
.LBB61_13:                              # %for.inc.us
                                        #   in Loop: Header=BB61_6 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB61_6
	jmp	.LBB61_22
	.align	16, 0x90
.LBB61_3:                               # %if.then.us4
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movzbl	16(%rdi), %eax
	cmpq	$2, %rax
	jne	.LBB61_5
# BB#4:                                 # %if.then5.us27
                                        #   in Loop: Header=BB61_3 Depth=1
	xorq	%rdx, %rdx
	movq	%r15, %rsi
	callq	expand_cleanups
.LBB61_5:                               # %for.inc.us28
                                        #   in Loop: Header=BB61_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB61_3
.LBB61_22:                              # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp451:
	.size	expand_cleanups, .Ltmp451-expand_cleanups
	.cfi_endproc

	.globl	save_stack_pointer
	.align	16, 0x90
	.type	save_stack_pointer,@function
save_stack_pointer:                     # @save_stack_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp454:
	.cfi_def_cfa_offset 16
.Ltmp455:
	.cfi_offset %rbx, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rbx
	cmpq	$0, 40(%rbx)
	jne	.LBB62_5
# BB#1:                                 # %if.then
	cmpq	$0, 8(%rbx)
	leaq	40(%rbx), %rsi
	jne	.LBB62_2
# BB#3:                                 # %if.then
	movabsq	$1, %rdi
	jmp	.LBB62_4
.LBB62_2:
	movabsq	$0, %rdi
.LBB62_4:                               # %if.then
	movq	48(%rbx), %rdx
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 8(%rax)
.LBB62_5:                               # %if.end
	popq	%rbx
	retq
.Ltmp456:
	.size	save_stack_pointer, .Ltmp456-save_stack_pointer
	.cfi_endproc

	.globl	expand_decl
	.align	16, 0x90
	.type	expand_decl,@function
expand_decl:                            # @expand_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp462:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp463:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp464:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp465:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp466:
	.cfi_def_cfa_offset 48
.Ltmp467:
	.cfi_offset %rbx, -40
.Ltmp468:
	.cfi_offset %r12, -32
.Ltmp469:
	.cfi_offset %r14, -24
.Ltmp470:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movl	16(%rbx), %eax
	movq	8(%rbx), %r14
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$34, %rcx
	jne	.LBB63_1
# BB#3:                                 # %if.end28
	testq	$262144, %rax           # imm = 0x40000
	jne	.LBB63_67
# BB#4:                                 # %lor.lhs.false
	movq	48(%rbx), %rcx
	testq	$256, %rcx              # imm = 0x100
	jne	.LBB63_67
# BB#5:                                 # %if.end39
	movq	global_trees(%rip), %rdx
	cmpq	%rdx, %r14
	je	.LBB63_6
# BB#7:                                 # %if.else
	cmpq	$0, 40(%rbx)
	je	.LBB63_8
# BB#12:                                # %if.else58
	movq	%rcx, %rsi
	andq	$255, %rsi
	cmpq	$51, %rsi
	je	.LBB63_16
# BB#13:                                # %land.lhs.true
	movl	flag_float_store(%rip), %edx
	testq	%rdx, %rdx
	je	.LBB63_23
# BB#14:                                # %land.lhs.true66
	movzbl	16(%r14), %edx
	cmpq	$7, %rdx
	je	.LBB63_16
# BB#15:                                # %land.lhs.true66
	andq	$2048, %rax             # imm = 0x800
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB63_24
	jmp	.LBB63_16
.LBB63_1:                               # %entry
	cmpq	$32, %rcx
	jne	.LBB63_67
# BB#2:                                 # %if.then
	movl	60(%r14), %eax
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	movb	%al, 48(%rbx)
	movl	64(%r14), %eax
	movl	56(%rbx), %ecx
	andq	$16777215, %rax         # imm = 0xFFFFFF
	movabsq	$4278190080, %rdx       # imm = 0xFF000000
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 56(%rbx)
	movq	32(%r14), %rax
	movq	%rax, 40(%rbx)
	movq	40(%r14), %rax
	movq	%rax, 64(%rbx)
.LBB63_67:                              # %if.end426
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB63_6:                               # %if.then41
	movq	const_int_rtx+512(%rip), %rsi
	movq	$51, %rdi
	callq	gen_rtx_MEM
	movq	%rax, 144(%rbx)
	jmp	.LBB63_67
.LBB63_8:                               # %if.then46
	cmpq	$0, 104(%rbx)
	je	.LBB63_9
# BB#10:                                # %if.else51
	movl	target_flags(%rip), %edi
	movq	$25, %rcx
	shrq	%cl, %rdi
	andq	$1, %rdi
	orq	$4, %rdi
	callq	gen_reg_rtx
	movq	$51, %rdi
	movq	%rax, %rsi
	jmp	.LBB63_11
.LBB63_9:                               # %if.then49
	movq	const_int_rtx+512(%rip), %rsi
	movq	$51, %rdi
.LBB63_11:                              # %if.end55
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	set_mem_attributes
	movq	%r14, 144(%rbx)
	jmp	.LBB63_67
.LBB63_23:                              # %land.lhs.true72
	testq	$2048, %rax             # imm = 0x800
	jne	.LBB63_16
.LBB63_24:                              # %land.lhs.true78
	testq	$1024, %rcx             # imm = 0x400
	jne	.LBB63_26
# BB#25:                                # %land.lhs.true78
	movl	optimize(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB63_26
.LBB63_16:                              # %if.else243
	movq	64(%rbx), %rdi
	movzbl	16(%rdi), %eax
	movabsq	$-34359738369, %r15     # imm = 0xFFFFFFF7FFFFFFFF
	cmpq	$25, %rax
	jne	.LBB63_58
# BB#17:                                # %land.lhs.true251
	movl	flag_stack_check(%rip), %eax
	testq	%rax, %rax
	je	.LBB63_19
# BB#18:                                # %land.lhs.true253
	movl	target_flags(%rip), %edx
	movq	$23, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
	movq	$4092, %rax             # imm = 0xFFC
	subq	%rdx, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
	movabsq	$2951479051793528259, %rcx # imm = 0x28F5C28F5C28F5C3
	mulq	%rcx
	movq	$2, %rcx
	shrq	%cl, %rdx
	movq	%rdx, %rsi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB63_19
.LBB63_58:                              # %if.else375
	callq	do_pending_stack_adjust
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %r12
	cmpq	$0, 40(%r12)
	jne	.LBB63_63
# BB#59:                                # %if.then.i
	cmpq	$0, 8(%r12)
	leaq	40(%r12), %rsi
	jne	.LBB63_60
# BB#61:                                # %if.then.i
	movabsq	$1, %rdi
	jmp	.LBB63_62
.LBB63_19:                              # %if.then262
	movq	144(%rbx), %rax
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB63_51
# BB#20:                                # %cond.end278
	movzbl	(%rax), %ecx
	cmpq	$66, %rcx
	jne	.LBB63_22
# BB#21:                                # %cond.end294
	movq	8(%rax), %r14
	movzbl	(%r14), %eax
	cmpq	$61, %rax
	jne	.LBB63_22
.LBB63_51:                              # %if.end319
	movq	48(%rbx), %rax
	movq	%rax, %rdx
	andq	$255, %rdx
	cmpq	$51, %rdx
	movq	$128, %rcx
	je	.LBB63_53
# BB#52:                                # %cond.false328
	movzbl	mode_bitsize(%rdx,%rdx), %ecx
.LBB63_53:                              # %cond.end337
	movl	56(%rbx), %edx
	movabsq	$4278190080, %rsi       # imm = 0xFF000000
	andq	%rdx, %rsi
	orq	%rcx, %rsi
	movl	%esi, 56(%rbx)
	andq	%r15, %rax
	movq	%rax, 48(%rbx)
	movq	$1, %rsi
	movq	$1, %rdx
	movq	$1, %rcx
	movq	%rbx, %rdi
	callq	assign_temp
	movq	%rax, %r15
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	set_mem_attributes
	movq	%r15, 144(%rbx)
	testq	%r14, %r14
	je	.LBB63_67
# BB#54:                                # %if.then353
	testq	%r15, %r15
	jne	.LBB63_56
# BB#55:                                # %cond.false360
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r15
.LBB63_56:                              # %cond.end363
	movq	8(%r15), %rdi
	movq	%r14, %rsi
	callq	force_operand
	cmpq	%r14, %rax
	je	.LBB63_67
# BB#57:                                # %if.then371
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	emit_move_insn
	jmp	.LBB63_67
.LBB63_60:
	movabsq	$0, %rdi
.LBB63_62:                              # %if.then.i
	movq	48(%r12), %rdx
	callq	emit_stack_save
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%r12, 8(%rax)
.LBB63_63:                              # %save_stack_pointer.exit
	movzbl	16(%r14), %eax
	cmpq	$18, %rax
	jne	.LBB63_66
# BB#64:                                # %land.lhs.true384
	movq	24(%r14), %rax
	testq	%rax, %rax
	je	.LBB63_66
# BB#65:                                # %if.then387
	movq	112(%rax), %rdi
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
.LBB63_66:                              # %if.end392
	movq	64(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%rax, %r14
	callq	free_temp_slots
	movq	8(%rbx), %rax
	movl	64(%rax), %edx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	allocate_dynamic_stack_space
	movzbl	48(%rbx), %edi
	movq	%rax, %rsi
	callq	gen_rtx_MEM
	movq	%rax, %r14
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	set_mem_attributes
	movq	%r14, 144(%rbx)
	movl	target_flags(%rip), %eax
	movq	$20, %rcx
	shrq	%cl, %rax
	movl	56(%rbx), %ecx
	andq	$32, %rax
	addq	$32, %rax
	movabsq	$4278190080, %rdx       # imm = 0xFF000000
	andq	%rcx, %rdx
	orq	%rax, %rdx
	movl	%edx, 56(%rbx)
	andq	%r15, 48(%rbx)
	jmp	.LBB63_67
.LBB63_26:                              # %if.then87
	movl	16(%r14), %eax
	movq	$13, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	movl	%eax, 4(%rsp)
	leaq	4(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	callq	promote_mode
	movq	%rax, %rdi
	callq	gen_reg_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB63_28
# BB#27:                                # %cond.false
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB63_28:                              # %cond.end
	movl	(%rax), %ecx
	movq	%rcx, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	$61, %rdx
	jne	.LBB63_32
# BB#29:                                # %if.then112
	testq	%rax, %rax
	jne	.LBB63_31
# BB#30:                                # %cond.false119
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB63_31:                              # %cond.end122
	movq	cfun(%rip), %rcx
	jmp	.LBB63_40
.LBB63_32:                              # %if.else125
	testq	%rax, %rax
	jne	.LBB63_34
# BB#33:                                # %cond.false132
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
	movl	(%rax), %ecx
.LBB63_34:                              # %cond.end135
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$65, %rcx
	jne	.LBB63_41
# BB#35:                                # %if.then140
	testq	%rax, %rax
	jne	.LBB63_37
# BB#36:                                # %cond.false147
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB63_37:                              # %cond.end150
	movq	cfun(%rip), %rcx
	movq	8(%rax), %rax
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%rbx, (%rcx,%rax,8)
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB63_39
# BB#38:                                # %cond.false167
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB63_39:                              # %cond.end170
	movq	cfun(%rip), %rcx
	movq	16(%rax), %rax
.LBB63_40:                              # %if.end183
	movq	24(%rcx), %rcx
	movl	8(%rax), %eax
	movq	72(%rcx), %rcx
	movq	%rbx, (%rcx,%rax,8)
.LBB63_41:                              # %if.end183
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB63_43
# BB#42:                                # %cond.false190
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB63_43:                              # %cond.end193
	callq	mark_user_reg
	movl	16(%r14), %eax
	orq	$2, %rax
	andq	$255, %rax
	cmpq	$15, %rax
	jne	.LBB63_47
# BB#44:                                # %if.then206
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB63_46
# BB#45:                                # %cond.false213
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB63_46:                              # %cond.end216
	movq	8(%rbx), %rax
	movq	8(%rax), %rax
	movl	64(%rax), %esi
	callq	mark_reg_pointer
.LBB63_47:                              # %if.end224
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB63_49
# BB#48:                                # %cond.false231
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB63_49:                              # %cond.end234
	movq	%rbx, %rsi
	callq	maybe_set_unchanging
	movzbl	17(%rbx), %eax
	testq	$4, %rax
	je	.LBB63_67
# BB#50:                                # %if.then241
	movq	%rbx, %rdi
	callq	put_var_into_stack
	jmp	.LBB63_67
.LBB63_22:                              # %if.then303
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_decl, %rcx
	movq	$3969, %rsi             # imm = 0xF81
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp471:
	.size	expand_decl, .Ltmp471-expand_decl
	.cfi_endproc

	.globl	expand_decl_init
	.align	16, 0x90
	.type	expand_decl_init,@function
expand_decl_init:                       # @expand_decl_init
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp475:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp476:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp477:
	.cfi_def_cfa_offset 32
.Ltmp478:
	.cfi_offset %rbx, -24
.Ltmp479:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movl	16(%r14), %ebx
	movq	%rbx, %rax
	andq	$255, %rax
	cmpq	$32, %rax
	je	.LBB64_14
# BB#1:                                 # %entry
	movq	%rbx, %rax
	andq	$262144, %rax           # imm = 0x40000
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB64_14
# BB#2:                                 # %if.end
	movq	104(%r14), %rax
	movq	global_trees(%rip), %rcx
	andq	$65536, %rbx            # imm = 0x10000
	cmpq	%rcx, %rax
	je	.LBB64_3
# BB#8:                                 # %if.else
	testq	%rax, %rax
	je	.LBB64_13
# BB#9:                                 # %land.lhs.true
	movzbl	16(%rax), %eax
	cmpq	$2, %rax
	je	.LBB64_13
# BB#10:                                # %if.then41
	movq	24(%r14), %rdi
	movl	32(%r14), %esi
	callq	emit_line_note
	movq	104(%r14), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	jmp	.LBB64_11
.LBB64_14:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB64_3:                               # %if.then10
	movq	8(%r14), %rdi
	movl	16(%rdi), %eax
	movq	%rax, %rcx
	andq	$254, %rcx
	cmpq	$6, %rcx
	je	.LBB64_7
# BB#4:                                 # %if.then10
	andq	$255, %rax
	cmpq	$10, %rax
	je	.LBB64_7
# BB#5:                                 # %if.then10
	cmpq	$13, %rax
	je	.LBB64_7
# BB#6:                                 # %if.then10
	cmpq	$15, %rax
	jne	.LBB64_12
.LBB64_7:                               # %if.then26
	movq	global_trees+88(%rip), %rsi
	callq	convert
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rax, %rsi
.LBB64_11:                              # %if.end49
	callq	expand_assignment
.LBB64_12:                              # %if.end49
	callq	emit_queue
.LBB64_13:                              # %if.end49
	movl	16(%r14), %eax
	movabsq	$4294901759, %rcx       # imm = 0xFFFEFFFF
	andq	%rax, %rcx
	orq	%rbx, %rcx
	movl	%ecx, 16(%r14)
	xorq	%rdi, %rdi
	callq	preserve_temp_slots
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	free_temp_slots  # TAILCALL
.Ltmp480:
	.size	expand_decl_init, .Ltmp480-expand_decl_init
	.cfi_endproc

	.globl	expand_decl_cleanup
	.align	16, 0x90
	.type	expand_decl_cleanup,@function
expand_decl_cleanup:                    # @expand_decl_cleanup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp487:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp488:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp489:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp490:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp491:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp492:
	.cfi_def_cfa_offset 64
.Ltmp493:
	.cfi_offset %rbx, -48
.Ltmp494:
	.cfi_offset %r12, -40
.Ltmp495:
	.cfi_offset %r13, -32
.Ltmp496:
	.cfi_offset %r14, -24
.Ltmp497:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	%rsi, %rbx
	movq	%rdi, %r14
	xorq	%r15, %r15
	testq	%rax, %rax
	je	.LBB65_16
# BB#1:                                 # %lor.lhs.false
	movq	8(%rax), %rax
	movq	(%rax), %r12
	testq	%r12, %r12
	je	.LBB65_16
# BB#2:                                 # %if.end
	movq	$1, %r15
	testq	%rbx, %rbx
	je	.LBB65_16
# BB#3:                                 # %conditional_context.exit
	movl	100(%r12), %ecx
	leaq	64(%r12), %rax
	testq	%rcx, %rcx
	je	.LBB65_4
# BB#5:                                 # %if.then7
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movl	word_mode(%rip), %edi
	callq	gen_reg_rtx
	movq	%rax, %r13
	movq	%r13, (%rsp)            # 8-byte Spill
	callq	start_sequence
	movq	const_int_rtx+512(%rip), %rsi
	movq	%r13, %rdi
	callq	emit_move_insn
	callq	get_insns
	movq	%rax, %r13
	callq	end_sequence
	movq	104(%r12), %rsi
	movq	%r13, %rdi
	callq	emit_insns_after
	movq	%rax, 104(%r12)
	movq	const_int_rtx+520(%rip), %rsi
	movq	(%rsp), %r13            # 8-byte Reload
	movq	%r13, %rdi
	callq	emit_move_insn
	movl	word_mode(%rip), %edi
	movq	$1, %rsi
	callq	type_for_mode
	movq	$34, %rdi
	xorq	%rsi, %rsi
	movq	%rax, %rdx
	callq	build_decl
	movq	%r13, 144(%rax)
	movq	global_trees+216(%rip), %r13
	movq	%rax, %rdi
	callq	truthvalue_conversion
	movq	global_trees+88(%rip), %r8
	movq	%rax, %rcx
	movq	$51, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	movq	%rcx, %rdx
	movq	%rbx, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	112(%r12), %r13
	movq	%rax, %rbx
	jmp	.LBB65_6
.LBB65_4:
	movq	%rax, %r13
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB65_6:                               # %if.end26
	movq	%rbx, %rdi
	callq	unsave_expr
	movq	(%r13), %rdx
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	tree_cons
	movq	%rax, %rbx
	movl	100(%r12), %r14d
	testq	%r14, %r14
	movq	%rbx, (%r13)
	je	.LBB65_7
# BB#8:                                 # %if.then34
	callq	start_sequence
	jmp	.LBB65_9
.LBB65_7:                               # %if.then30
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%r12, 8(%rax)
.LBB65_9:                               # %if.end35
	movzbl	using_eh_for_cleanups_p(%rip), %eax
	testq	%rax, %rax
	je	.LBB65_10
# BB#11:                                # %if.else
	callq	expand_eh_region_start
	jmp	.LBB65_12
.LBB65_10:                              # %if.then37
	movl	16(%rbx), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%rbx)
.LBB65_12:                              # %if.end38
	testq	%r14, %r14
	je	.LBB65_15
# BB#13:                                # %if.then40
	callq	get_insns
	movq	%rax, %rbx
	callq	end_sequence
	testq	%rbx, %rbx
	je	.LBB65_16
# BB#14:                                # %if.then43
	movq	104(%r12), %rsi
	movq	%rbx, %rdi
	callq	emit_insns_after
	movq	%rax, 104(%r12)
	jmp	.LBB65_16
.LBB65_15:                              # %if.else52
	callq	get_last_insn
	movq	%rax, 104(%r12)
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 112(%r12)
.LBB65_16:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp498:
	.size	expand_decl_cleanup, .Ltmp498-expand_decl_cleanup
	.cfi_endproc

	.globl	expand_decl_cleanup_eh
	.align	16, 0x90
	.type	expand_decl_cleanup_eh,@function
expand_decl_cleanup_eh:                 # @expand_decl_cleanup_eh
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp502:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp503:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp504:
	.cfi_def_cfa_offset 32
.Ltmp505:
	.cfi_offset %rbx, -24
.Ltmp506:
	.cfi_offset %r14, -16
	movq	%rdx, %r14
	movq	%rsi, %rbx
                                        # kill: RSI<def> RBX<kill>
	callq	expand_decl_cleanup
	testq	%rbx, %rbx
	je	.LBB66_3
# BB#1:                                 # %entry
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB66_3
# BB#2:                                 # %if.then
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	(%rcx), %rcx
	movq	64(%rcx), %rcx
	movl	16(%rcx), %edx
	andq	$1, %r14
	shlq	$18, %r14
	movabsq	$4294705151, %rsi       # imm = 0xFFFBFFFF
	andq	%rdx, %rsi
	orq	%r14, %rsi
	movl	%esi, 16(%rcx)
.LBB66_3:                               # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp507:
	.size	expand_decl_cleanup_eh, .Ltmp507-expand_decl_cleanup_eh
	.cfi_endproc

	.globl	expand_anon_union_decl
	.align	16, 0x90
	.type	expand_anon_union_decl,@function
expand_anon_union_decl:                 # @expand_anon_union_decl
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp514:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp515:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp516:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp517:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp518:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp519:
	.cfi_def_cfa_offset 80
.Ltmp520:
	.cfi_offset %rbx, -48
.Ltmp521:
	.cfi_offset %r12, -40
.Ltmp522:
	.cfi_offset %r13, -32
.Ltmp523:
	.cfi_offset %r14, -24
.Ltmp524:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	%rdx, %r13
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%rax, %rax
	je	.LBB67_2
# BB#1:                                 # %cond.false
	movq	8(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB67_2:                               # %cond.end
	testq	%r13, %r13
	je	.LBB67_7
# BB#3:
	movq	%r13, %rax
	.align	16, 0x90
.LBB67_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rax), %rcx
	movzbl	17(%rcx), %ecx
	testq	$4, %rcx
	jne	.LBB67_5
# BB#6:                                 # %for.inc
                                        #   in Loop: Header=BB67_4 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB67_4
	jmp	.LBB67_7
.LBB67_5:                               # %if.then
	movl	16(%r15), %eax
	orq	$1024, %rax             # imm = 0x400
	movl	%eax, 16(%r15)
.LBB67_7:                               # %for.end
	movq	%r15, %rdi
	callq	expand_decl
	movq	%r15, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	expand_decl_cleanup
	movq	144(%r15), %r12
	testq	%r12, %r12
	jne	.LBB67_9
# BB#8:                                 # %cond.false12
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	make_decl_rtl
	movq	144(%r15), %r12
.LBB67_9:                               # %cond.end15
	testq	%r13, %r13
	je	.LBB67_29
# BB#10:                                # %for.body19.lr.ph
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB67_11:                              # %for.body19
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r13), %rbx
	movq	8(%rbx), %rax
	movl	60(%rax), %eax
	movq	24(%r13), %rsi
	movzbl	18(%rbx), %edx
	movq	$9, %rcx
	shrq	%cl, %rax
	andq	$127, %rax
	testq	$1, %rdx
	je	.LBB67_13
# BB#12:                                # %if.then34
                                        #   in Loop: Header=BB67_11 Depth=1
	movl	16(%r15), %ecx
	orq	$65536, %rcx            # imm = 0x10000
	movl	%ecx, 16(%r15)
.LBB67_13:                              # %if.end40
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movl	56(%r15), %ecx
	movl	56(%rbx), %edx
	andq	$16777215, %rcx         # imm = 0xFFFFFF
	movabsq	$4278190080, %rsi       # imm = 0xFF000000
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movl	%edx, 56(%rbx)
	movq	48(%r15), %rcx
	movq	48(%rbx), %rdx
	movabsq	$34359738368, %rsi      # imm = 0x800000000
	andq	%rsi, %rcx
	movabsq	$-34359738369, %rsi     # imm = 0xFFFFFFF7FFFFFFFF
	andq	%rsi, %rdx
	orq	%rcx, %rdx
	movq	%rdx, 48(%rbx)
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$51, %rcx
	jne	.LBB67_16
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB67_11 Depth=1
	movzbl	48(%r15), %eax
	cmpq	$51, %rax
	movq	$51, %rax
	je	.LBB67_16
# BB#15:                                # %if.then67
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	40(%rbx), %rdi
	movq	$1, %rsi
	movq	$1, %rdx
	callq	mode_for_size_tree
	movb	%al, 48(%rbx)
.LBB67_16:                              # %if.end77
                                        #   in Loop: Header=BB67_11 Depth=1
	movl	(%r12), %edx
	movq	%rdx, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$61, %rcx
	jne	.LBB67_17
# BB#20:                                # %if.then97
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB67_22
# BB#21:                                # %if.then102
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	%r12, 144(%rbx)
	jmp	.LBB67_26
	.align	16, 0x90
.LBB67_17:                              # %if.end77
                                        #   in Loop: Header=BB67_11 Depth=1
	cmpq	$66, %rcx
	jne	.LBB67_23
# BB#18:                                # %if.then81
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	$16, %rcx
	shrq	%cl, %rdx
	andq	$255, %rdx
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB67_24
# BB#19:                                # %if.then86
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	%r12, 144(%rbx)
	jmp	.LBB67_26
	.align	16, 0x90
.LBB67_22:                              # %if.else105
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	%rax, %rdi
	movq	%r12, %rsi
	callq	gen_lowpart_SUBREG
	jmp	.LBB67_25
	.align	16, 0x90
.LBB67_24:                              # %if.else
                                        #   in Loop: Header=BB67_11 Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	adjust_address_1
.LBB67_25:                              # %if.end112
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	%rax, 144(%rbx)
.LBB67_26:                              # %if.end112
                                        #   in Loop: Header=BB67_11 Depth=1
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB67_28
# BB#27:                                # %if.then114
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	64(%r14), %rdx
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	tree_cons
	movq	%rax, 64(%r14)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB67_28:                              # %for.inc120
                                        #   in Loop: Header=BB67_11 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB67_11
.LBB67_29:                              # %for.end123
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB67_23:                              # %if.else110
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_anon_union_decl, %rcx
	movq	$4259, %rsi             # imm = 0x10A3
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp525:
	.size	expand_anon_union_decl, .Ltmp525-expand_anon_union_decl
	.cfi_endproc

	.globl	move_cleanups_up
	.align	16, 0x90
	.type	move_cleanups_up,@function
move_cleanups_up:                       # @move_cleanups_up
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp529:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp530:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp531:
	.cfi_def_cfa_offset 32
.Ltmp532:
	.cfi_offset %rbx, -24
.Ltmp533:
	.cfi_offset %r14, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rbx
	movq	8(%rbx), %r14
	movq	64(%rbx), %rdi
	movq	64(%r14), %rsi
	callq	chainon
	movq	%rax, 64(%r14)
	movq	$0, 64(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp534:
	.size	move_cleanups_up, .Ltmp534-move_cleanups_up
	.cfi_endproc

	.globl	last_cleanup_this_contour
	.align	16, 0x90
	.type	last_cleanup_this_contour,@function
last_cleanup_this_contour:              # @last_cleanup_this_contour
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB69_2
# BB#1:                                 # %if.end
	movq	64(%rcx), %rax
.LBB69_2:                               # %return
	retq
.Ltmp535:
	.size	last_cleanup_this_contour, .Ltmp535-last_cleanup_this_contour
	.cfi_endproc

	.globl	any_pending_cleanups
	.align	16, 0x90
	.type	any_pending_cleanups,@function
any_pending_cleanups:                   # @any_pending_cleanups
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	testq	%rax, %rax
	je	.LBB70_1
# BB#2:                                 # %lor.lhs.false
	movq	8(%rax), %rax
	testq	%rax, %rax
	je	.LBB70_3
# BB#4:                                 # %lor.lhs.false2
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB70_5
# BB#6:                                 # %if.end
	movq	64(%rcx), %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rdi
	je	.LBB70_8
# BB#7:                                 # %land.lhs.true
	movq	$1, %rax
	testq	%rdx, %rdx
	jne	.LBB70_14
	jmp	.LBB70_9
.LBB70_1:
	xorq	%rax, %rax
	retq
.LBB70_3:
	xorq	%rax, %rax
	retq
.LBB70_5:
	xorq	%rax, %rax
	retq
.LBB70_8:                               # %if.end10
	testq	%rdx, %rdx
	jne	.LBB70_11
.LBB70_9:                               # %land.lhs.true17
	cmpq	$0, 72(%rcx)
	je	.LBB70_10
	.align	16, 0x90
.LBB70_11:                              # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB70_12
# BB#13:                                # %for.body
                                        #   in Loop: Header=BB70_11 Depth=1
	cmpq	$0, 64(%rcx)
	movq	$1, %rax
	je	.LBB70_11
.LBB70_14:                              # %return
	retq
.LBB70_12:
	xorq	%rax, %rax
	retq
.LBB70_10:
	xorq	%rax, %rax
	retq
.Ltmp536:
	.size	any_pending_cleanups, .Ltmp536-any_pending_cleanups
	.cfi_endproc

	.globl	expand_start_case
	.align	16, 0x90
	.type	expand_start_case,@function
expand_start_case:                      # @expand_start_case
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp543:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp544:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp545:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp546:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp547:
	.cfi_def_cfa_offset 48
.Ltmp548:
	.cfi_offset %rbx, -48
.Ltmp549:
	.cfi_offset %r12, -40
.Ltmp550:
	.cfi_offset %r13, -32
.Ltmp551:
	.cfi_offset %r14, -24
.Ltmp552:
	.cfi_offset %r15, -16
	movq	stmt_obstack+32(%rip), %rbx
	movq	stmt_obstack+24(%rip), %rax
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %r13
	subq	%rax, %rbx
	cmpq	$119, %rbx
	jg	.LBB71_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rax
.LBB71_2:                               # %if.end
	addq	$120, %rax
	movq	%rax, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rbx
	cmpq	%rbx, %rax
	jne	.LBB71_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %ecx
	orq	$2, %rcx
	movb	%cl, stmt_obstack+80(%rip)
.LBB71_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rcx
	addq	%rcx, %rax
	notq	%rcx
	andq	%rax, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+8(%rip), %rdx
	movq	%rcx, %rsi
	subq	%rdx, %rsi
	movq	%rax, %rdi
	subq	%rdx, %rdi
	cmpq	%rdi, %rsi
	jle	.LBB71_6
# BB#5:                                 # %if.then29
	movq	%rax, stmt_obstack+24(%rip)
	movq	%rax, %rcx
.LBB71_6:                               # %if.end32
	movq	cfun(%rip), %rax
	movq	%rcx, stmt_obstack+16(%rip)
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	%rax, 8(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	40(%rax), %rax
	movq	%rax, (%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movl	48(%rax), %ecx
	incq	%rcx
	movl	%ecx, 48(%rax)
	movl	%ecx, 16(%rbx)
	xorq	%rax, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r13
	je	.LBB71_8
# BB#7:                                 # %cond.true
	callq	gen_label_rtx
.LBB71_8:                               # %cond.end
	movq	%rax, 24(%rbx)
	movq	$0, 40(%rbx)
	movq	%r12, 56(%rbx)
	movq	%r15, 64(%rbx)
	movq	$0, 48(%rbx)
	movq	%r14, 72(%rbx)
	callq	force_line_numbers
	movl	%eax, 80(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	%rbx, 40(%rax)
	callq	do_pending_stack_adjust
	callq	get_last_insn
	movzbl	(%rax), %eax
	cmpq	$37, %rax
	je	.LBB71_10
# BB#9:                                 # %if.then59
	movabsq	$4294967197, %rsi       # imm = 0xFFFFFF9D
	xorq	%rdi, %rdi
	callq	emit_note
.LBB71_10:                              # %if.end61
	callq	get_last_insn
	movq	%rax, 32(%rbx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB71_12
# BB#11:                                # %if.then.i
	movl	100(%rax), %ecx
	incq	%rcx
	movl	%ecx, 100(%rax)
.LBB71_12:                              # %start_cleanup_deferral.exit
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp553:
	.size	expand_start_case, .Ltmp553-expand_start_case
	.cfi_endproc

	.globl	expand_start_case_dummy
	.align	16, 0x90
	.type	expand_start_case_dummy,@function
expand_start_case_dummy:                # @expand_start_case_dummy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp555:
	.cfi_def_cfa_offset 16
	movq	stmt_obstack+32(%rip), %rax
	movq	stmt_obstack+24(%rip), %rcx
	subq	%rcx, %rax
	cmpq	$119, %rax
	jg	.LBB72_2
# BB#1:                                 # %if.then
	movabsq	$stmt_obstack, %rdi
	movq	$120, %rsi
	callq	_obstack_newchunk
	movq	stmt_obstack+24(%rip), %rcx
.LBB72_2:                               # %if.end
	addq	$120, %rcx
	movq	%rcx, stmt_obstack+24(%rip)
	movq	stmt_obstack+16(%rip), %rax
	cmpq	%rax, %rcx
	jne	.LBB72_4
# BB#3:                                 # %if.then8
	movzbl	stmt_obstack+80(%rip), %edx
	orq	$2, %rdx
	movb	%dl, stmt_obstack+80(%rip)
.LBB72_4:                               # %if.end9
	movslq	stmt_obstack+48(%rip), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, stmt_obstack+24(%rip)
	movq	stmt_obstack+32(%rip), %r8
	movq	stmt_obstack+8(%rip), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%r8, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB72_6
# BB#5:                                 # %if.then29
	movq	%r8, stmt_obstack+24(%rip)
	movq	%r8, %rdx
.LBB72_6:                               # %if.end32
	movq	cfun(%rip), %rcx
	movq	%rdx, stmt_obstack+16(%rip)
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	40(%rcx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movl	48(%rcx), %edx
	incq	%rdx
	movl	%edx, 48(%rcx)
	movl	%edx, 16(%rax)
	movq	$0, 64(%rax)
	movq	$0, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	$0, 24(%rax)
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 32(%rcx)
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	%rax, 40(%rcx)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB72_8
# BB#7:                                 # %if.then.i
	movl	100(%rax), %ecx
	incq	%rcx
	movl	%ecx, 100(%rax)
.LBB72_8:                               # %start_cleanup_deferral.exit
	popq	%rax
	retq
.Ltmp556:
	.size	expand_start_case_dummy, .Ltmp556-expand_start_case_dummy
	.cfi_endproc

	.globl	expand_end_case_dummy
	.align	16, 0x90
	.type	expand_end_case_dummy,@function
expand_end_case_dummy:                  # @expand_end_case_dummy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp561:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp562:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp563:
	.cfi_def_cfa_offset 32
.Ltmp564:
	.cfi_offset %rbx, -32
.Ltmp565:
	.cfi_offset %r14, -24
.Ltmp566:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	je	.LBB73_2
# BB#1:                                 # %if.then.i
	movl	100(%rcx), %edx
	decq	%rdx
	movl	%edx, 100(%rcx)
.LBB73_2:                               # %end_cleanup_deferral.exit
	movq	32(%rax), %r15
	movabsq	$stmt_obstack, %r14
	jmp	.LBB73_3
	.align	16, 0x90
.LBB73_18:                              # %do.cond.do.body1_crit_edge
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_3:                               # %do.body1
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB73_5
# BB#4:                                 # %if.then
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_5:                               # %if.end
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB73_7
# BB#6:                                 # %if.then10
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_7:                               # %if.end16
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB73_9
# BB#8:                                 # %if.then19
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_9:                               # %if.end25
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB73_11
# BB#10:                                # %if.then28
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_11:                              # %if.end34
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB73_13
# BB#12:                                # %if.then38
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB73_13:                              # %if.end44
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB73_16
# BB#14:                                # %if.end44
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB73_16
# BB#15:                                # %if.then52
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB73_17
	.align	16, 0x90
.LBB73_16:                              # %if.else
                                        #   in Loop: Header=BB73_3 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB73_17:                              # %do.cond
                                        #   in Loop: Header=BB73_3 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB73_18
# BB#19:                                # %do.end56
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp567:
	.size	expand_end_case_dummy, .Ltmp567-expand_end_case_dummy
	.cfi_endproc

	.globl	case_index_expr_type
	.align	16, 0x90
	.type	case_index_expr_type,@function
case_index_expr_type:                   # @case_index_expr_type
	.cfi_startproc
# BB#0:                                 # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB74_2
# BB#1:                                 # %if.then
	movq	56(%rcx), %rax
	movq	8(%rax), %rax
.LBB74_2:                               # %return
	retq
.Ltmp568:
	.size	case_index_expr_type, .Ltmp568-case_index_expr_type
	.cfi_endproc

	.globl	pushcase
	.align	16, 0x90
	.type	pushcase,@function
pushcase:                               # @pushcase
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp575:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp576:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp577:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp578:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp579:
	.cfi_def_cfa_offset 48
.Ltmp580:
	.cfi_offset %rbx, -48
.Ltmp581:
	.cfi_offset %r12, -40
.Ltmp582:
	.cfi_offset %r13, -32
.Ltmp583:
	.cfi_offset %r14, -24
.Ltmp584:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rbx
	movq	32(%rbx), %rax
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %rdx
	movq	%rdi, %rcx
	testq	%rax, %rax
	movq	$1, %r13
	je	.LBB75_11
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 32(%rax)
	je	.LBB75_11
# BB#2:                                 # %if.end
	movq	8(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB75_4
# BB#3:                                 # %land.lhs.true6
	movslq	16(%rsi), %rsi
	movslq	16(%rax), %rdi
	cmpq	%rdi, %rsi
	movq	$5, %r13
	jg	.LBB75_11
.LBB75_4:                               # %if.end13
	movq	56(%rax), %rsi
	movq	8(%rsi), %r12
	movq	global_trees(%rip), %rsi
	xorq	%r13, %r13
	cmpq	%rsi, %r12
	je	.LBB75_11
# BB#5:                                 # %if.end25
	testq	%rcx, %rcx
	je	.LBB75_6
# BB#8:                                 # %if.end28
	movq	64(%rax), %rdi
	movq	%rcx, %rsi
	callq	*%rdx
	movq	%rax, %rbx
	callq	check_seenlabel
	xorq	%rdi, %rdi
	testq	%rbx, %rbx
	je	.LBB75_7
# BB#9:                                 # %land.lhs.true30
	movzbl	18(%rbx), %eax
	movq	$3, %r13
	testq	$4, %rax
	jne	.LBB75_11
# BB#10:                                # %lor.lhs.false
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	int_fits_type_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	%rbx, %rdi
	jne	.LBB75_7
.LBB75_11:                              # %return
	movq	%r13, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB75_6:                               # %if.end28.thread
	callq	check_seenlabel
	xorq	%rdi, %rdi
.LBB75_7:                               # %if.end36
	movq	%rdi, %rsi
	movq	%r15, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_case_node  # TAILCALL
.Ltmp585:
	.size	pushcase, .Ltmp585-pushcase
	.cfi_endproc

	.align	16, 0x90
	.type	check_seenlabel,@function
check_seenlabel:                        # @check_seenlabel
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp587:
	.cfi_def_cfa_offset 16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movslq	80(%rax), %rdi
	testq	%rdi, %rdi
	js	.LBB76_12
# BB#1:                                 # %if.then
	callq	restore_line_number_status
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 80(%rax)
	movq	32(%rax), %rcx
	jmp	.LBB76_2
	.align	16, 0x90
.LBB76_11:                              # %for.inc
                                        #   in Loop: Header=BB76_2 Depth=1
	movq	24(%rcx), %rcx
.LBB76_2:                               # %if.then
                                        # =>This Inner Loop Header: Depth=1
	testq	%rcx, %rcx
	je	.LBB76_12
# BB#3:                                 # %for.body
                                        #   in Loop: Header=BB76_2 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$36, %rdx
	je	.LBB76_12
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB76_2 Depth=1
	cmpq	$37, %rdx
	je	.LBB76_11
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB76_2 Depth=1
	cmpq	$32, %rdx
	jne	.LBB76_7
# BB#6:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB76_2 Depth=1
	movq	32(%rcx), %rdx
	movzbl	(%rdx), %edx
	cmpq	$48, %rdx
	je	.LBB76_11
.LBB76_7:                               # %do.body.preheader
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB76_12
# BB#8:                                 # %land.rhs
                                        #   in Loop: Header=BB76_7 Depth=1
	movzbl	(%rcx), %edx
	cmpq	$37, %rdx
	jne	.LBB76_7
# BB#9:                                 # %lor.rhs
                                        #   in Loop: Header=BB76_7 Depth=1
	movslq	40(%rcx), %rsi
	testq	%rsi, %rsi
	js	.LBB76_7
# BB#10:                                # %if.then38
	movq	32(%rcx), %rdi
	movq	72(%rax), %rcx
	movabsq	$.L.str21, %rdx
	xorq	%rax, %rax
	popq	%r8
	jmp	warning_with_file_and_line  # TAILCALL
.LBB76_12:                              # %if.end53
	popq	%rax
	retq
.Ltmp588:
	.size	check_seenlabel, .Ltmp588-check_seenlabel
	.cfi_endproc

	.globl	add_case_node
	.align	16, 0x90
	.type	add_case_node,@function
add_case_node:                          # @add_case_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp595:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp596:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp597:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp598:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp599:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp600:
	.cfi_def_cfa_offset 64
.Ltmp601:
	.cfi_offset %rbx, -48
.Ltmp602:
	.cfi_offset %r12, -40
.Ltmp603:
	.cfi_offset %r13, -32
.Ltmp604:
	.cfi_offset %r14, -24
.Ltmp605:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	testq	%rsi, %rsi
	movq	%r12, %r13
	je	.LBB77_2
# BB#1:                                 # %entry
	movq	%rsi, %r13
.LBB77_2:                               # %entry
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %r14
	movq	%r13, %rax
	orq	%r12, %rax
	je	.LBB77_3
# BB#6:                                 # %if.end16
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	40(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB77_7
# BB#8:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	.align	16, 0x90
.LBB77_9:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rax, %rbx
	movq	24(%rbx), %rsi
	movq	%r13, %rdi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	jne	.LBB77_10
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB77_9 Depth=1
	movq	32(%rbx), %rdi
	movq	%r12, %rsi
	callq	tree_int_cst_lt
	testq	%r15, %rax
	je	.LBB77_52
# BB#12:                                # %if.then28
                                        #   in Loop: Header=BB77_9 Depth=1
	leaq	8(%rbx), %r14
	jmp	.LBB77_13
	.align	16, 0x90
.LBB77_10:                              # %if.then24
                                        #   in Loop: Header=BB77_9 Depth=1
	movq	%rbx, %r14
.LBB77_13:                              # %while.cond.backedge
                                        #   in Loop: Header=BB77_9 Depth=1
	movq	(%r14), %rax
	testq	%rax, %rax
	jne	.LBB77_9
	jmp	.LBB77_14
.LBB77_3:                               # %if.then3
	movq	48(%r14), %rax
	testq	%rax, %rax
	jne	.LBB77_4
# BB#5:                                 # %if.end10
	movq	%rdx, 48(%r14)
	movq	%rdx, %rdi
	callq	expand_label
	xorq	%rax, %rax
	jmp	.LBB77_51
.LBB77_7:
	addq	$40, %r14
.LBB77_14:                              # %while.end
	movq	$56, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r12, 24(%r15)
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB77_16
# BB#15:                                # %if.then36
	movq	24(%r15), %rax
	movq	%rax, 32(%r15)
	jmp	.LBB77_17
.LBB77_16:                              # %if.else39
	movq	%r13, 32(%r15)
.LBB77_17:                              # %if.end41
	movq	8(%rsp), %rdi           # 8-byte Reload
	movq	%rdi, 40(%r15)
	callq	expand_label
	movq	%r15, (%r14)
	movq	%rbx, 16(%r15)
	xorq	%rax, %rax
	testq	%rbx, %rbx
	movl	%eax, 48(%r15)
	movq	$0, 8(%r15)
	movq	$0, (%r15)
	je	.LBB77_51
	.align	16, 0x90
.LBB77_18:                              # %while.body47
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdx
	movq	(%rdx), %rcx
	movl	48(%rdx), %esi
	cmpq	%rcx, %r15
	jne	.LBB77_36
# BB#19:                                # %if.then50
                                        #   in Loop: Header=BB77_18 Depth=1
	testq	%r12, %rsi
	movq	%r12, %rcx
	je	.LBB77_50
	jmp	.LBB77_20
	.align	16, 0x90
.LBB77_36:                              # %if.else136
                                        #   in Loop: Header=BB77_18 Depth=1
	movq	$1, %rcx
	testq	%r12, %rsi
	jne	.LBB77_37
.LBB77_50:                              # %if.end230
                                        #   in Loop: Header=BB77_18 Depth=1
	movl	%ecx, 48(%rdx)
	movq	16(%rdx), %rbx
	testq	%rbx, %rbx
	movq	%rdx, %r15
	jne	.LBB77_18
	jmp	.LBB77_51
.LBB77_52:                              # %if.else29
	movq	40(%rbx), %rax
	movq	(%rsp), %rcx            # 8-byte Reload
.LBB77_4:                               # %if.then4
	movq	%rax, (%rcx)
	movq	$2, %rax
.LBB77_51:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB77_20:                              # %if.else55
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jns	.LBB77_49
# BB#21:                                # %if.then57
	movslq	48(%r15), %rax
	movq	8(%r15), %rsi
	testq	%rax, %rax
	js	.LBB77_22
# BB#28:                                # %if.else89
	movq	8(%rsi), %rax
	testq	%rax, %rax
	movq	%rax, (%rdx)
	je	.LBB77_30
# BB#29:                                # %if.then94
	movq	%rdx, 16(%rax)
.LBB77_30:                              # %if.end96
	movq	%rdx, 8(%rsi)
	movq	(%rsi), %rax
	testq	%rax, %rax
	movq	%rax, 8(%r15)
	je	.LBB77_32
# BB#31:                                # %if.then101
	movq	%r15, 16(%rax)
.LBB77_32:                              # %if.end103
	movq	%r15, (%rsi)
	movl	48(%rsi), %ebx
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$31, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movl	%eax, 48(%rdx)
	movq	$31, %rcx
	shrq	%cl, %rdi
	subq	%rbx, %rdi
	movl	%edi, 48(%r15)
	jmp	.LBB77_33
.LBB77_37:                              # %if.else142
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	testq	%rsi, %rsi
	jle	.LBB77_49
# BB#38:                                # %if.then145
	movslq	48(%r15), %rax
	movq	(%r15), %rsi
	testq	%rax, %rax
	jle	.LBB77_43
# BB#39:                                # %if.then149
	movq	%rsi, 8(%rdx)
	testq	%rsi, %rsi
	je	.LBB77_41
# BB#40:                                # %if.then153
	movq	%rdx, 16(%rsi)
.LBB77_41:                              # %if.end155
	movq	%rdx, (%r15)
	jmp	.LBB77_25
.LBB77_49:                              # %if.else133
	xorq	%rax, %rax
	movl	%eax, 48(%rdx)
	jmp	.LBB77_51
.LBB77_43:                              # %if.else179
	movq	(%rsi), %rax
	testq	%rax, %rax
	movq	%rax, 8(%rdx)
	je	.LBB77_45
# BB#44:                                # %if.then186
	movq	%rdx, 16(%rax)
.LBB77_45:                              # %if.end188
	movq	%rdx, (%rsi)
	movq	8(%rsi), %rax
	testq	%rax, %rax
	movq	%rax, (%r15)
	je	.LBB77_47
# BB#46:                                # %if.then193
	movq	%r15, 16(%rax)
.LBB77_47:                              # %if.end195
	movq	%r15, 8(%rsi)
	movl	48(%rsi), %ebx
	movq	$32, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	$31, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movl	%eax, 48(%r15)
	movq	$31, %rcx
	shrq	%cl, %rdi
	subq	%rbx, %rdi
	movl	%edi, 48(%rdx)
.LBB77_33:                              # %if.end103
	xorq	%rax, %rax
	movl	%eax, 48(%rsi)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%rsi, 16(%rdx)
	movq	%rsi, 16(%r15)
	movq	%rcx, 16(%rsi)
	je	.LBB77_48
# BB#34:                                # %if.then116
	movq	(%rcx), %rax
	cmpq	%rdx, %rax
	je	.LBB77_35
# BB#54:                                # %if.else122
	movq	%rsi, 8(%rcx)
	xorq	%rax, %rax
	jmp	.LBB77_51
.LBB77_48:                              # %if.else125
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rsi, 40(%rcx)
	jmp	.LBB77_51
.LBB77_22:                              # %if.then60
	movq	%rsi, (%rdx)
	testq	%rsi, %rsi
	je	.LBB77_24
# BB#23:                                # %if.then64
	movq	%rdx, 16(%rsi)
.LBB77_24:                              # %if.end66
	movq	%rdx, 8(%r15)
.LBB77_25:                              # %if.end66
	xorq	%rax, %rax
	movl	%eax, 48(%rdx)
	movl	%eax, 48(%r15)
	movq	16(%rdx), %rcx
	testq	%rcx, %rcx
	movq	%r15, 16(%rdx)
	movq	%rcx, 16(%r15)
	je	.LBB77_42
# BB#26:                                # %if.then74
	movq	(%rcx), %rax
	cmpq	%rdx, %rax
	je	.LBB77_27
# BB#53:                                # %if.else79
	movq	%r15, 8(%rcx)
	xorq	%rax, %rax
	jmp	.LBB77_51
.LBB77_42:                              # %if.else82
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%r15, 40(%rcx)
	jmp	.LBB77_51
.LBB77_27:                              # %if.then77
	movq	%r15, (%rcx)
	xorq	%rax, %rax
	jmp	.LBB77_51
.LBB77_35:                              # %if.then120
	movq	%rsi, (%rcx)
	xorq	%rax, %rax
	jmp	.LBB77_51
.Ltmp606:
	.size	add_case_node, .Ltmp606-add_case_node
	.cfi_endproc

	.globl	pushcase_range
	.align	16, 0x90
	.type	pushcase_range,@function
pushcase_range:                         # @pushcase_range
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp613:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp614:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp615:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp616:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp617:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp618:
	.cfi_def_cfa_offset 80
.Ltmp619:
	.cfi_offset %rbx, -48
.Ltmp620:
	.cfi_offset %r12, -40
.Ltmp621:
	.cfi_offset %r13, -32
.Ltmp622:
	.cfi_offset %r14, -24
.Ltmp623:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rbx
	movq	32(%rbx), %rax
	movq	%rdx, %r13
	movq	%rsi, %r12
	movq	%rdi, %r15
	testq	%rax, %rax
	movq	$1, %r14
	je	.LBB78_15
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 32(%rax)
	je	.LBB78_15
# BB#2:                                 # %if.end
	movq	8(%rbx), %rdx
	testq	%rdx, %rdx
	je	.LBB78_4
# BB#3:                                 # %land.lhs.true6
	movslq	16(%rdx), %rsi
	movslq	16(%rax), %rdx
	cmpq	%rdx, %rsi
	movq	$5, %r14
	jg	.LBB78_15
.LBB78_4:                               # %if.end13
	movq	56(%rax), %rdx
	movq	8(%rdx), %rsi
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	global_trees(%rip), %rdx
	xorq	%r14, %r14
	cmpq	%rdx, %rsi
	je	.LBB78_15
# BB#5:                                 # %if.end25
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%r8, 16(%rsp)           # 8-byte Spill
	movq	64(%rax), %rbx
	callq	check_seenlabel
	testq	%r15, %r15
	jne	.LBB78_7
# BB#6:                                 # %if.then27
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %r15
.LBB78_7:                               # %if.end29
	testq	%r12, %r12
	jne	.LBB78_9
# BB#8:                                 # %if.end33
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	112(%rax), %r12
	testq	%r12, %r12
	je	.LBB78_10
.LBB78_9:                               # %land.lhs.true35
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$4, %r14
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	jne	.LBB78_15
	jmp	.LBB78_11
.LBB78_10:                              # %if.then40
	movq	112(%rbx), %r12
	movq	%rbx, (%rsp)            # 8-byte Spill
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB78_11:                              # %if.end43
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	(%rsp), %rbx            # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	*%r13
	movq	%rax, %r15
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	*%r13
	movzbl	18(%r15), %ecx
	movq	%rax, %rbx
	movq	$3, %r14
	testq	$4, %rcx
	jne	.LBB78_15
# BB#12:                                # %lor.lhs.false
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	int_fits_type_p
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	je	.LBB78_15
# BB#13:                                # %if.end51
	movzbl	18(%rbx), %eax
	testq	$4, %rax
	jne	.LBB78_15
# BB#14:                                # %lor.lhs.false58
	movq	%rbx, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	int_fits_type_p
	testq	%r13, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	8(%rsp), %rdx           # 8-byte Reload
	je	.LBB78_15
# BB#16:                                # %if.end62
	movq	%r15, %rdi
	movq	%rbx, %rsi
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	add_case_node  # TAILCALL
.LBB78_15:                              # %return
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp624:
	.size	pushcase_range, .Ltmp624-pushcase_range
	.cfi_endproc

	.globl	all_cases_count
	.align	16, 0x90
	.type	all_cases_count,@function
all_cases_count:                        # @all_cases_count
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp631:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp632:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp633:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp634:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp635:
	.cfi_def_cfa_offset 48
.Ltmp636:
	.cfi_offset %rbx, -48
.Ltmp637:
	.cfi_offset %r12, -40
.Ltmp638:
	.cfi_offset %r13, -32
.Ltmp639:
	.cfi_offset %r14, -24
.Ltmp640:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rax, %rax
	movl	%eax, (%r14)
	movzbl	16(%r15), %eax
	cmpq	$10, %rax
	je	.LBB79_9
# BB#1:                                 # %entry
	movq	$2, %r12
	cmpq	$11, %rax
	je	.LBB79_21
# BB#2:                                 # %entry
	cmpq	$12, %rax
	jne	.LBB79_4
# BB#3:                                 # %sw.bb1
	movq	$256, %r12              # imm = 0x100
	jmp	.LBB79_21
.LBB79_9:                               # %sw.bb18
	movq	104(%r15), %rdi
	xorq	%rsi, %rsi
	callq	host_integerp
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movq	$-1, %r12
	je	.LBB79_21
# BB#10:                                # %lor.lhs.false
	movq	112(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB79_21
# BB#11:                                # %lor.lhs.false26
	xorq	%rsi, %rsi
	callq	host_integerp
	testq	%rbx, %rax
	je	.LBB79_21
# BB#12:                                # %if.end32
	movq	104(%r15), %rdi
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movq	24(%r15), %rbx
	movq	%rax, %r15
	testq	%rbx, %rbx
	movabsq	$0, %r12
	je	.LBB79_21
# BB#13:                                # %for.body.lr.ph
	xorq	%r12, %r12
	movq	%r15, %rax
	.align	16, 0x90
.LBB79_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%rbx), %rdi
	movq	%rax, %r13
	xorq	%rsi, %rsi
	callq	tree_low_cst
	movl	(%r14), %ecx
	cmpq	$2, %rcx
	je	.LBB79_16
# BB#15:                                # %for.body
                                        #   in Loop: Header=BB79_14 Depth=1
	cmpq	%r13, %rax
	jle	.LBB79_16
# BB#17:                                # %if.else43
                                        #   in Loop: Header=BB79_14 Depth=1
	leaq	(%r15,%r12), %rcx
	cmpq	%rax, %rcx
	je	.LBB79_20
# BB#18:                                # %if.then45
                                        #   in Loop: Header=BB79_14 Depth=1
	movq	$1, %rcx
	jmp	.LBB79_19
	.align	16, 0x90
.LBB79_16:                              # %if.then42
                                        #   in Loop: Header=BB79_14 Depth=1
	movq	$2, %rcx
.LBB79_19:                              # %if.end47
                                        #   in Loop: Header=BB79_14 Depth=1
	movl	%ecx, (%r14)
.LBB79_20:                              # %if.end47
                                        #   in Loop: Header=BB79_14 Depth=1
	movq	(%rbx), %rbx
	incq	%r12
	testq	%rbx, %rbx
	jne	.LBB79_14
	jmp	.LBB79_21
.LBB79_4:                               # %sw.bb2
	movq	112(%r15), %rdx
	testq	%rdx, %rdx
	movq	$-1, %r12
	je	.LBB79_21
# BB#5:                                 # %land.lhs.true
	movq	104(%r15), %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB79_21
# BB#6:                                 # %land.lhs.true10
	movq	global_trees+88(%rip), %rsi
	movq	%r15, %rdi
	callq	convert
	movq	%rax, %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB79_21
# BB#7:                                 # %land.lhs.true15
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	host_integerp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB79_21
# BB#8:                                 # %if.then
	movq	$1, %rsi
	movq	%r14, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	tree_low_cst  # TAILCALL
.LBB79_21:                              # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp641:
	.size	all_cases_count, .Ltmp641-all_cases_count
	.cfi_endproc

	.globl	mark_seen_cases
	.align	16, 0x90
	.type	mark_seen_cases,@function
mark_seen_cases:                        # @mark_seen_cases
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp648:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp649:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp650:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp651:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp652:
	.cfi_def_cfa_offset 48
	subq	$96, %rsp
.Ltmp653:
	.cfi_def_cfa_offset 144
.Ltmp654:
	.cfi_offset %rbx, -48
.Ltmp655:
	.cfi_offset %r12, -40
.Ltmp656:
	.cfi_offset %r13, -32
.Ltmp657:
	.cfi_offset %r14, -24
.Ltmp658:
	.cfi_offset %r15, -16
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %r14
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movq	%rbx, 72(%rsp)          # 8-byte Spill
	movq	$25, %rdi
	callq	make_node
	movq	%r14, %rdx
	movq	%rax, %r14
	testq	%rdx, %rdx
	movq	%rbx, 8(%r14)
	je	.LBB80_34
# BB#1:                                 # %if.else
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$2, %rax
	jne	.LBB80_10
# BB#2:                                 # %if.then2
	movq	24(%rdx), %rax
	movq	8(%rax), %rax
	movq	%rax, 8(%r14)
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %r12
	testq	%r12, %r12
	je	.LBB80_34
# BB#3:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	.align	16, 0x90
.LBB80_4:                               # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB80_5 Depth 2
	movq	32(%r12), %rax
	movq	32(%rax), %rcx
	movq	40(%rax), %rax
	movq	%rax, 40(%r14)
	movq	%rcx, 32(%r14)
	movq	%rdx, %rbx
	.align	16, 0x90
.LBB80_5:                               # %do.body
                                        #   Parent Loop BB80_4 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rsi
	movq	%r14, %rdi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	jne	.LBB80_8
# BB#6:                                 # %if.else25
                                        #   in Loop: Header=BB80_5 Depth=2
	movq	32(%rbx), %rdi
	movq	%r14, %rsi
	callq	tree_int_cst_lt
	testq	%r13, %rax
	je	.LBB80_35
# BB#7:                                 # %if.then29
                                        #   in Loop: Header=BB80_5 Depth=2
	addq	$8, %rbx
.LBB80_8:                               # %do.cond
                                        #   in Loop: Header=BB80_5 Depth=2
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB80_5
	jmp	.LBB80_9
	.align	16, 0x90
.LBB80_35:                              # %if.else30
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	%r15, %rcx
	andq	$7, %rcx
	movq	$1, %rax
	shlq	%cl, %rax
	movq	$3, %rcx
	movq	%r15, %rdx
	shrq	%cl, %rdx
	andq	$536870911, %rdx        # imm = 0x1FFFFFFF
	movq	16(%rsp), %rsi          # 8-byte Reload
	movzbl	(%rsi,%rdx), %ecx
	orq	%rax, %rcx
	movb	%cl, (%rsi,%rdx)
.LBB80_9:                               # %for.inc
                                        #   in Loop: Header=BB80_4 Depth=1
	movq	(%r12), %r12
	incq	%r15
	testq	%r12, %r12
	movq	64(%rsp), %rdx          # 8-byte Reload
	jne	.LBB80_4
	jmp	.LBB80_34
.LBB80_10:                              # %if.else37
	movq	%r13, %r12
	cmpq	$0, (%rdx)
	je	.LBB80_12
# BB#11:                                # %if.then40
	xorq	%rsi, %rsi
	movq	%rdx, %rdi
	callq	case_tree2list
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rax, %rdx
	movq	%rdx, 40(%rcx)
.LBB80_12:                              # %for.cond48.preheader
	testq	%rdx, %rdx
	je	.LBB80_34
# BB#13:                                # %for.body50.lr.ph
	leaq	32(%r14), %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	leaq	40(%r14), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	leaq	88(%rsp), %rbx
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB80_14:                              # %for.body50
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB80_15 Depth 2
                                        #       Child Loop BB80_19 Depth 3
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	24(%rdx), %rax
	movq	32(%rax), %rcx
	movq	40(%rax), %rax
	movq	%rax, 40(%r14)
	movq	%rcx, 32(%r14)
	movq	32(%rdx), %rdi
	jmp	.LBB80_15
	.align	16, 0x90
.LBB80_32:                              # %if.end136
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	32(%r14), %rdi
	movq	40(%r14), %rsi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	%r15, %r8
	movq	48(%rsp), %r9           # 8-byte Reload
	callq	add_double
	movq	64(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdi
.LBB80_15:                              # %for.body50
                                        #   Parent Loop BB80_14 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB80_19 Depth 3
	movq	%r14, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB80_33
# BB#16:                                # %while.body
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r12, %rax
	leaq	80(%rsp), %r9
	je	.LBB80_25
# BB#17:                                # %land.lhs.true
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 24(%rax)
	jne	.LBB80_18
.LBB80_25:                              # %if.else99
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rax
	testq	%rax, %rax
	jne	.LBB80_26
# BB#27:                                # %if.else110
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	$0, 80(%rsp)
	movq	$0, 88(%rsp)
	xorq	%rsi, %rsi
	xorq	%rdi, %rdi
	jmp	.LBB80_28
.LBB80_26:                              # %if.then102
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	32(%rax), %rdi
	movq	40(%rax), %rsi
	movq	%rbx, %rdx
	movq	%r9, %rcx
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%r9, %r13
	callq	neg_double
	movq	%r13, %r9
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	88(%rsp), %rdi
	movq	80(%rsp), %rsi
.LBB80_28:                              # %if.end111
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	(%r15), %rdx
	movq	8(%r15), %rcx
	movq	%rbx, %r8
                                        # kill: RBX<def> R8<kill>
	callq	add_double
.LBB80_29:                              # %if.end119
                                        #   in Loop: Header=BB80_15 Depth=2
	cmpq	$0, 80(%rsp)
	jne	.LBB80_32
# BB#30:                                # %land.lhs.true122
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	88(%rsp), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jae	.LBB80_32
# BB#31:                                # %if.then125
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	%rax, %rcx
	andq	$7, %rcx
	movq	$1, %rdx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	andq	$536870911, %rax        # imm = 0x1FFFFFFF
	movq	16(%rsp), %rsi          # 8-byte Reload
	movzbl	(%rsi,%rax), %ecx
	orq	%rdx, %rcx
	movb	%cl, (%rsi,%rax)
	jmp	.LBB80_32
.LBB80_18:                              # %if.then75
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 88(%rsp)
	movq	$0, 80(%rsp)
	movq	%r13, %r15
	.align	16, 0x90
.LBB80_19:                              # %for.cond76
                                        #   Parent Loop BB80_14 Depth=1
                                        #     Parent Loop BB80_15 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r15, %r15
	jne	.LBB80_21
# BB#20:                                # %if.then79
                                        #   in Loop: Header=BB80_19 Depth=3
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %r15
	movq	$0, 88(%rsp)
.LBB80_21:                              # %if.end82
                                        #   in Loop: Header=BB80_19 Depth=3
	movq	32(%r15), %rsi
	movq	%r14, %rdi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB80_22
# BB#23:                                # %if.end90
                                        #   in Loop: Header=BB80_19 Depth=3
	incq	88(%rsp)
	movq	(%r15), %r15
	cmpq	%r13, %r15
	jne	.LBB80_19
# BB#24:                                # %if.then96
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	$-1, 88(%rsp)
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB80_29
.LBB80_22:                              # %if.then87
                                        #   in Loop: Header=BB80_15 Depth=2
	movq	88(%rsp), %rax
	movq	(%r15), %r13
	incq	%rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB80_29
	.align	16, 0x90
.LBB80_33:                              # %for.inc150
                                        #   in Loop: Header=BB80_14 Depth=1
	movq	64(%rsp), %rdx          # 8-byte Reload
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB80_14
.LBB80_34:                              # %if.end154
	addq	$96, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp659:
	.size	mark_seen_cases, .Ltmp659-mark_seen_cases
	.cfi_endproc

	.align	16, 0x90
	.type	case_tree2list,@function
case_tree2list:                         # @case_tree2list
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp662:
	.cfi_def_cfa_offset 16
.Ltmp663:
	.cfi_offset %rbx, -16
	jmp	.LBB81_1
	.align	16, 0x90
.LBB81_4:                               # %if.then6
                                        #   in Loop: Header=BB81_1 Depth=1
	movq	$0, (%rbx)
	movq	%rbx, %rsi
.LBB81_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movq	%rdi, %rbx
	movq	8(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB81_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB81_1 Depth=1
	callq	case_tree2list
	movq	%rax, %rsi
.LBB81_3:                               # %if.end
                                        #   in Loop: Header=BB81_1 Depth=1
	movq	%rsi, 8(%rbx)
	movq	(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB81_4
# BB#5:                                 # %return
	movq	%rbx, %rax
	popq	%rbx
	retq
.Ltmp664:
	.size	case_tree2list, .Ltmp664-case_tree2list
	.cfi_endproc

	.globl	check_for_full_enumeration_handling
	.align	16, 0x90
	.type	check_for_full_enumeration_handling,@function
check_for_full_enumeration_handling:    # @check_for_full_enumeration_handling
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp671:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp672:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp673:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp674:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp675:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp676:
	.cfi_def_cfa_offset 64
.Ltmp677:
	.cfi_offset %rbx, -48
.Ltmp678:
	.cfi_offset %r12, -40
.Ltmp679:
	.cfi_offset %r13, -32
.Ltmp680:
	.cfi_offset %r14, -24
.Ltmp681:
	.cfi_offset %r15, -16
	movl	warn_switch(%rip), %eax
	movq	%rdi, (%rsp)            # 8-byte Spill
	xorq	%rcx, %rcx
	movl	%ecx, 12(%rsp)
	testq	%rax, %rax
	je	.LBB82_36
# BB#1:                                 # %if.end
	leaq	12(%rsp), %rsi
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	all_cases_count
	movq	%rax, %r14
	leaq	-1(%r14), %rax
	cmpq	$599998, %rax           # imm = 0x927BE
	ja	.LBB82_10
# BB#2:                                 # %land.lhs.true2
	leaq	8(%r14), %rax
	movq	$63, %rcx
	sarq	%cl, %rax
	movq	$61, %rcx
	shrq	%cl, %rax
	leaq	8(%r14,%rax), %rdi
	movq	$3, %rcx
	sarq	%cl, %rdi
	movq	$1, %rsi
	callq	calloc
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB82_10
# BB#3:                                 # %if.then5
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	24(%rdi), %rbx
	movl	12(%rsp), %ecx
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	mark_seen_cases
	testq	%rbx, %rbx
	je	.LBB82_9
# BB#4:                                 # %for.body.lr.ph
	movq	$1, %r12
	movabsq	$.L.str18, %r13
	.align	16, 0x90
.LBB82_5:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	-1(%r12), %rax
	movq	$3, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	andq	$536870911, %rdx        # imm = 0x1FFFFFFF
	movzbl	(%r15,%rdx), %edx
	andq	$7, %rax
	movq	$1, %rsi
	movq	%rax, %rcx
	shlq	%cl, %rsi
	testq	%rsi, %rdx
	jne	.LBB82_7
# BB#6:                                 # %if.then14
                                        #   in Loop: Header=BB82_5 Depth=1
	movq	24(%rbx), %rax
	movq	32(%rax), %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	warning
.LBB82_7:                               # %for.inc
                                        #   in Loop: Header=BB82_5 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB82_9
# BB#8:                                 # %for.inc
                                        #   in Loop: Header=BB82_5 Depth=1
	cmpq	%r14, %r12
	leaq	1(%r12), %r12
	jl	.LBB82_5
.LBB82_9:                               # %for.end
	movq	%r15, %rdi
	callq	free
.LBB82_10:                              # %if.end17
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB82_13
# BB#11:                                # %land.lhs.true19
	cmpq	$0, (%rdi)
	je	.LBB82_13
# BB#12:                                # %if.then26
	xorq	%rsi, %rsi
	callq	case_tree2list
	movq	cfun(%rip), %rcx
	movq	8(%rcx), %rcx
	movq	32(%rcx), %rcx
	movq	%rax, 40(%rcx)
.LBB82_13:                              # %if.end38
	movl	warn_switch(%rip), %eax
	testq	%rax, %rax
	je	.LBB82_36
# BB#14:                                # %if.then40
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %r13
	testq	%r13, %r13
	je	.LBB82_36
# BB#15:                                # %for.body48.lr.ph
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB82_16:                              # %for.body48
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB82_17 Depth 2
                                        #     Child Loop BB82_27 Depth 2
	movq	(%rsp), %rax            # 8-byte Reload
	movq	24(%rax), %r15
	leaq	24(%r13), %r14
	jmp	.LBB82_17
	.align	16, 0x90
.LBB82_19:                              # %for.inc59
                                        #   in Loop: Header=BB82_17 Depth=2
	movq	(%r15), %r15
.LBB82_17:                              # %for.body48
                                        #   Parent Loop BB82_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB82_20
# BB#18:                                # %land.rhs53
                                        #   in Loop: Header=BB82_17 Depth=2
	movq	(%r14), %rdi
	movq	32(%r15), %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	je	.LBB82_19
	jmp	.LBB82_25
	.align	16, 0x90
.LBB82_20:                              # %if.then64
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	(%r14), %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	96(%rax), %rax
	movq	32(%rcx), %rsi
	testq	%rax, %rax
	jne	.LBB82_22
# BB#21:                                # %if.then68
                                        #   in Loop: Header=BB82_16 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str19, %rdi
	callq	warning
	jmp	.LBB82_25
	.align	16, 0x90
.LBB82_22:                              # %if.else
                                        #   in Loop: Header=BB82_16 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB82_24
# BB#23:                                # %cond.false
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	72(%rax), %rax
.LBB82_24:                              # %cond.end
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str20, %rdi
	callq	warning
.LBB82_25:                              # %if.end90
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	24(%r13), %rdi
	movq	32(%r13), %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB82_35
# BB#26:                                # %if.then94
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	(%rsp), %rax            # 8-byte Reload
	movq	24(%rax), %rbx
	jmp	.LBB82_27
	.align	16, 0x90
.LBB82_29:                              # %for.inc108
                                        #   in Loop: Header=BB82_27 Depth=2
	movq	(%rbx), %rbx
.LBB82_27:                              # %if.then94
                                        #   Parent Loop BB82_16 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%rbx, %rbx
	je	.LBB82_30
# BB#28:                                # %land.rhs99
                                        #   in Loop: Header=BB82_27 Depth=2
	movq	32(%r13), %rdi
	movq	32(%rbx), %rsi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	je	.LBB82_29
	jmp	.LBB82_35
	.align	16, 0x90
.LBB82_30:                              # %if.then113
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	32(%r13), %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	96(%rax), %rax
	movq	32(%rcx), %rsi
	testq	%rax, %rax
	jne	.LBB82_32
# BB#31:                                # %if.then118
                                        #   in Loop: Header=BB82_16 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str19, %rdi
	callq	warning
	jmp	.LBB82_35
	.align	16, 0x90
.LBB82_32:                              # %if.else123
                                        #   in Loop: Header=BB82_16 Depth=1
	movzbl	16(%rax), %ecx
	cmpq	$1, %rcx
	je	.LBB82_34
# BB#33:                                # %cond.false139
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	72(%rax), %rax
.LBB82_34:                              # %cond.end144
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	32(%rax), %rdx
	xorq	%rax, %rax
	movabsq	$.L.str20, %rdi
	callq	warning
.LBB82_35:                              # %for.inc152
                                        #   in Loop: Header=BB82_16 Depth=1
	movq	8(%r13), %r13
	testq	%r13, %r13
	jne	.LBB82_16
.LBB82_36:                              # %if.end154
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp682:
	.size	check_for_full_enumeration_handling, .Ltmp682-check_for_full_enumeration_handling
	.cfi_endproc

	.globl	expand_end_case_type
	.align	16, 0x90
	.type	expand_end_case_type,@function
expand_end_case_type:                   # @expand_end_case_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp686:
	.cfi_def_cfa_offset 16
.Ltmp687:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp688:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$104, %rsp
.Ltmp689:
	.cfi_offset %rbx, -56
.Ltmp690:
	.cfi_offset %r12, -48
.Ltmp691:
	.cfi_offset %r13, -40
.Ltmp692:
	.cfi_offset %r14, -32
.Ltmp693:
	.cfi_offset %r15, -24
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	%rdi, %r13
	testq	%rax, %rax
	je	.LBB83_185
# BB#1:                                 # %if.end
	movq	%rax, %rbx
	movq	%rsi, %r12
	callq	gen_label_rtx
	movq	56(%rbx), %rcx
	movq	%rcx, -72(%rbp)         # 8-byte Spill
	movq	%rbx, %r15
	movq	8(%rcx), %r14
	movl	16(%r14), %edx
	movq	%rax, -120(%rbp)        # 8-byte Spill
	movq	%r12, %rax
	movq	$13, %rcx
	shrq	%cl, %rdx
	movq	%rdx, -96(%rbp)         # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB83_3
# BB#2:                                 # %if.then4
	movq	8(%r13), %rax
.LBB83_3:                               # %if.end7
	movq	%rax, -88(%rbp)         # 8-byte Spill
	callq	do_pending_stack_adjust
	callq	check_seenlabel
	movq	global_trees(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB83_163
# BB#4:                                 # %if.then9
	movq	%r15, %r12
	movq	48(%r12), %rbx
	testq	%rbx, %rbx
	movq	-72(%rbp), %rcx         # 8-byte Reload
	jne	.LBB83_9
# BB#5:                                 # %land.lhs.true
	movq	-88(%rbp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$10, %rax
	jne	.LBB83_8
# BB#6:                                 # %land.lhs.true17
	movzbl	16(%rcx), %eax
	cmpq	$25, %rax
	je	.LBB83_8
# BB#7:                                 # %if.end24
	movq	-88(%rbp), %rdi         # 8-byte Reload
	callq	check_for_full_enumeration_handling
	movq	48(%r12), %rbx
	testq	%rbx, %rbx
	jne	.LBB83_9
.LBB83_8:                               # %if.then29
	movq	$31, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	build_decl
	movq	%rax, 48(%r12)
	movq	%rax, %rdi
	callq	expand_label
	movq	48(%r12), %rbx
.LBB83_9:                               # %if.end37
	movq	%r14, -64(%rbp)         # 8-byte Spill
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB83_10
# BB#12:                                # %if.end.i
	movq	144(%rbx), %r13
	testq	%r13, %r13
	jne	.LBB83_15
# BB#13:                                # %if.end5.i
	callq	gen_label_rtx
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%r13, 144(%rbx)
	jne	.LBB83_15
# BB#14:                                # %cond.false.i
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r13
.LBB83_15:                              # %label_rtx.exit
	callq	get_last_insn
	movq	%rax, -48(%rbp)
	movq	40(%r12), %r15
	leaq	40(%r12), %rax
	movq	%rax, -80(%rbp)         # 8-byte Spill
	xorq	%r14, %r14
	testq	%r15, %r15
	movq	$1, %rbx
	je	.LBB83_16
# BB#17:                                # %land.lhs.true46
	cmpq	$0, (%r15)
	je	.LBB83_19
# BB#18:                                # %if.end59
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%r15, %rdi
	callq	case_tree2list
	movq	%rax, %r15
	testq	%r15, %r15
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	%r15, (%rax)
	je	.LBB83_16
.LBB83_19:                              # %while.body.i.preheader
	movq	%r13, -112(%rbp)        # 8-byte Spill
	movq	%r12, -104(%rbp)        # 8-byte Spill
.LBB83_20:                              # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB83_25 Depth 2
	movq	%r15, %r12
	movq	40(%r12), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB83_10
# BB#21:                                # %if.end.i.i
                                        #   in Loop: Header=BB83_20 Depth=1
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB83_24
# BB#22:                                # %if.end5.i.i
                                        #   in Loop: Header=BB83_20 Depth=1
	callq	gen_label_rtx
	movq	%rax, %rdi
	testq	%rdi, %rdi
	movq	%rdi, 144(%rbx)
	jne	.LBB83_24
# BB#23:                                # %cond.false.i.i
                                        #   in Loop: Header=BB83_20 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB83_24:                              # %label_rtx.exit.i
                                        #   in Loop: Header=BB83_20 Depth=1
	callq	next_real_insn
	movq	8(%r12), %r15
	movq	%rax, %r14
	jmp	.LBB83_25
	.align	16, 0x90
.LBB83_38:                              # %while.body43.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	32(%r15), %rax
	movq	%rax, 32(%r12)
	movq	8(%r15), %r15
.LBB83_25:                              # %label_rtx.exit.i
                                        #   Parent Loop BB83_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	%r15, %r15
	je	.LBB83_39
# BB#26:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	40(%r15), %rbx
	movzbl	16(%rbx), %eax
	cmpq	$31, %rax
	jne	.LBB83_10
# BB#27:                                # %if.end.i78.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	144(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB83_30
# BB#28:                                # %if.end5.i81.i
                                        #   in Loop: Header=BB83_25 Depth=2
	callq	gen_label_rtx
	movq	%rax, %rdi
	testq	%rdi, %rdi
	movq	%rdi, 144(%rbx)
	jne	.LBB83_30
# BB#29:                                # %cond.false.i82.i
                                        #   in Loop: Header=BB83_25 Depth=2
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rdi
.LBB83_30:                              # %label_rtx.exit84.i
                                        #   in Loop: Header=BB83_25 Depth=2
	callq	next_real_insn
	movq	%rax, %rbx
	cmpq	%r14, %rbx
	je	.LBB83_36
# BB#31:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB83_25 Depth=2
	testq	%r14, %r14
	je	.LBB83_40
# BB#32:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB83_25 Depth=2
	testq	%rbx, %rbx
	je	.LBB83_40
# BB#33:                                # %land.lhs.true10.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	%r14, %rdi
	callq	simplejump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB83_40
# BB#34:                                # %land.lhs.true13.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	%rbx, %rdi
	callq	simplejump_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB83_40
# BB#35:                                # %land.lhs.true16.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	32(%r14), %rax
	movq	32(%rbx), %rcx
	movq	16(%rax), %rdi
	movq	16(%rcx), %rsi
	callq	rtx_equal_p
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB83_40
.LBB83_36:                              # %land.lhs.true28.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	32(%r12), %rdx
	movq	24(%r15), %rbx
	movq	8(%rdx), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB83_40
# BB#37:                                # %land.rhs.i
                                        #   in Loop: Header=BB83_25 Depth=2
	movq	32(%r12), %rbx
	movq	8(%rbx), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	tree_int_cst_lt
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB83_38
.LBB83_40:                              # %while.end.i
                                        #   in Loop: Header=BB83_20 Depth=1
	movq	%r15, 8(%r12)
	testq	%r15, %r15
	jne	.LBB83_20
	jmp	.LBB83_41
.LBB83_163:                             # %if.else424
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%r15, %r12
	je	.LBB83_165
# BB#164:                               # %if.then.i724
	movl	100(%rax), %ecx
	decq	%rcx
	movl	%ecx, 100(%rax)
	jmp	.LBB83_165
.LBB83_16:
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	movq	$0, -128(%rbp)          # 8-byte Folded Spill
	xorq	%r15, %r15
	jmp	.LBB83_66
.LBB83_39:                              # %while.end.thread.i
	movq	$0, 8(%r12)
.LBB83_41:                              # %group_case_nodes.exit
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	je	.LBB83_42
# BB#44:                                # %for.body.lr.ph
	xorq	%r15, %r15
	xorq	%r12, %r12
	xorq	%r14, %r14
	.align	16, 0x90
.LBB83_45:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movzbl	16(%rsi), %eax
	cmpq	$25, %rax
	jne	.LBB83_46
# BB#47:                                # %if.end73
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	32(%rbx), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB83_48
# BB#49:                                # %if.end80
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	-64(%rbp), %r13         # 8-byte Reload
	movq	%r13, %rdi
	callq	convert
	movq	%rax, 24(%rbx)
	movq	32(%rbx), %rsi
	movq	%r13, %rdi
	callq	convert
	movq	%rax, 32(%rbx)
	movq	24(%rbx), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r14
	movq	%rax, %rcx
	movq	%rdi, %r13
	je	.LBB83_60
# BB#50:                                # %if.else
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	40(%rdi), %rcx
	movq	40(%r15), %rdx
	cmpq	%rdx, %rcx
	jl	.LBB83_53
# BB#51:                                # %lor.lhs.false
                                        #   in Loop: Header=BB83_45 Depth=1
	jne	.LBB83_54
# BB#52:                                # %land.lhs.true106
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	32(%rdi), %rcx
	movq	32(%r15), %rdx
	cmpq	%rdx, %rcx
	jae	.LBB83_54
	.align	16, 0x90
.LBB83_53:                              # %if.then115
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	%rdi, %r15
.LBB83_54:                              # %if.end117
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	40(%r12), %rcx
	movq	40(%rax), %rdx
	cmpq	%rdx, %rcx
	jl	.LBB83_58
# BB#55:                                # %lor.lhs.false126
                                        #   in Loop: Header=BB83_45 Depth=1
	jne	.LBB83_56
# BB#57:                                # %land.lhs.true135
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	32(%r12), %rcx
	movq	32(%rax), %rdx
	cmpq	%rdx, %rcx
	jae	.LBB83_56
	.align	16, 0x90
.LBB83_58:                              # %if.then144
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	%rax, %rcx
	jmp	.LBB83_59
.LBB83_56:                              #   in Loop: Header=BB83_45 Depth=1
	movq	%r12, %rcx
.LBB83_59:                              # %if.end147
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	%r15, %r13
.LBB83_60:                              # %if.end147
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	%rcx, %r12
	movq	%rax, %rsi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movabsq	$0, %rax
	jne	.LBB83_62
# BB#61:                                # %if.end147
                                        #   in Loop: Header=BB83_45 Depth=1
	movabsq	$1, %rax
.LBB83_62:                              # %if.end147
                                        #   in Loop: Header=BB83_45 Depth=1
	movq	8(%rbx), %rbx
	leaq	1(%rax,%r14), %r14
	testq	%rbx, %rbx
	movq	%r13, %r15
	movq	%r12, %rdx
                                        # kill: R12<def> RDX<kill>
	jne	.LBB83_45
# BB#63:                                # %for.end
	xorq	%r15, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	je	.LBB83_64
# BB#65:                                # %if.then156
	movq	$60, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movq	%rdx, -136(%rbp)        # 8-byte Spill
	movq	%r13, %rcx
	movq	%r13, -128(%rbp)        # 8-byte Spill
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	%rax, %r15
	movq	-104(%rbp), %r12        # 8-byte Reload
	movq	-112(%rbp), %r13        # 8-byte Reload
	jmp	.LBB83_66
.LBB83_42:
	movq	$0, -136(%rbp)          # 8-byte Folded Spill
	movq	$0, -128(%rbp)          # 8-byte Folded Spill
	xorq	%r15, %r15
	jmp	.LBB83_43
.LBB83_64:
	movq	%rdx, -136(%rbp)        # 8-byte Spill
	movq	%r13, -128(%rbp)        # 8-byte Spill
	xorq	%r14, %r14
.LBB83_43:                              # %if.end159
	movq	-104(%rbp), %r12        # 8-byte Reload
	movq	-112(%rbp), %r13        # 8-byte Reload
	movq	$1, %rbx
.LBB83_66:                              # %if.end159
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB83_68
# BB#67:                                # %if.then.i662
	movl	100(%rax), %ecx
	decq	%rcx
	movl	%ecx, 100(%rax)
.LBB83_68:                              # %end_cleanup_deferral.exit
	testq	%rbx, %rbx
	je	.LBB83_70
# BB#69:                                # %if.then161
	movq	const_int_rtx+512(%rip), %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	-72(%rbp), %rdi         # 8-byte Reload
	callq	expand_expr
	callq	emit_queue
	callq	do_pending_stack_adjust
	movq	%r13, %rdi
	jmp	.LBB83_103
.LBB83_70:                              # %if.else163
	movq	%r12, -104(%rbp)        # 8-byte Spill
	callq	case_values_threshold
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	movq	%r14, %rcx
	andq	%r12, %rcx
	cmpq	%rax, %rcx
	jb	.LBB83_76
# BB#71:                                # %lor.lhs.false166
	addq	%r14, %r14
	leaq	(%r14,%r14,4), %rax
	movabsq	$4294967294, %rsi       # imm = 0xFFFFFFFE
	andq	%rax, %rsi
	movq	%r15, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB83_76
# BB#72:                                # %lor.lhs.false170
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB83_76
# BB#73:                                # %lor.lhs.false174
	movq	-72(%rbp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	je	.LBB83_76
# BB#74:                                # %lor.lhs.false174
	cmpq	$47, %rax
	jne	.LBB83_128
# BB#75:                                # %land.lhs.true188
	movq	-72(%rbp), %rax         # 8-byte Reload
	movq	40(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$25, %rax
	jne	.LBB83_128
.LBB83_76:                              # %if.then195
	movq	-96(%rbp), %r15         # 8-byte Reload
	andq	$1, %r15
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	-72(%rbp), %rdi         # 8-byte Reload
	callq	expand_expr
	movq	%rax, %r14
	movzbl	2(%r14), %esi
	movl	mode_class(,%rsi,4), %eax
	cmpq	$1, %rax
	jne	.LBB83_82
# BB#77:                                # %land.lhs.true203
	movq	$74, %rdi
	callq	have_insn_for
	testq	%r12, %rax
	jne	.LBB83_82
# BB#78:                                # %if.then209
	movzbl	2(%r14), %ebx
	jmp	.LBB83_79
.LBB83_186:                             # %for.inc222
                                        #   in Loop: Header=BB83_79 Depth=1
	andq	%r12, %rbx
	movzbl	mode_wider_mode(%rbx), %ebx
.LBB83_79:                              # %if.then209
                                        # =>This Inner Loop Header: Depth=1
	testq	%rbx, %rbx
	je	.LBB83_82
# BB#80:                                # %for.body216
                                        #   in Loop: Header=BB83_79 Depth=1
	movq	$74, %rdi
	movq	%rbx, %rsi
	callq	have_insn_for
	testq	%r12, %rax
	je	.LBB83_186
# BB#81:                                # %if.then219
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	convert_to_mode
	movq	%rax, %r14
.LBB83_82:                              # %if.end227
	callq	emit_queue
	callq	do_pending_stack_adjust
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	protect_from_queue
	movq	%rax, %r14
	movl	(%r14), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%r15, %rbx
	cmpq	$66, %rcx
	jne	.LBB83_83
# BB#84:                                # %if.then233
	movq	%r13, -112(%rbp)        # 8-byte Spill
	movq	%r14, %rdi
	callq	copy_to_reg
	movq	%rax, %r14
	movl	(%r14), %eax
	jmp	.LBB83_85
.LBB83_83:
	movq	%r13, -112(%rbp)        # 8-byte Spill
.LBB83_85:                              # %if.end235
	movq	-72(%rbp), %r15         # 8-byte Reload
	movl	16(%r15), %ecx
	andq	$65535, %rax            # imm = 0xFFFF
	andq	$255, %rcx
	cmpq	$54, %rax
	je	.LBB83_87
# BB#86:                                # %if.end235
	cmpq	$25, %rcx
	je	.LBB83_87
# BB#104:                               # %if.else289
	movq	-88(%rbp), %rax         # 8-byte Reload
	movzbl	16(%rax), %eax
	xorq	%rcx, %rcx
	cmpq	$10, %rax
	je	.LBB83_127
# BB#105:                               # %land.rhs
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %r13
	movq	32(%r13), %rax
	movq	global_trees+104(%rip), %rcx
	movq	%rcx, -72(%rbp)         # 8-byte Spill
	movq	8(%rax), %r15
	movq	$127, %rdi
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	build_int_2_wide
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	convert
	movzbl	cost_table_initialized(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB83_115
# BB#106:                               # %if.then.i690
	movq	$1, %rcx
	movb	%cl, cost_table_initialized(%rip)
	movq	$65535, %r8             # imm = 0xFFFF
	movq	$8, %rdx
	movq	$16, %rsi
	.align	16, 0x90
.LBB83_107:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	andq	%r12, %rcx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	testq	$140, %rcx
	jne	.LBB83_108
# BB#109:                               # %if.else.i
                                        #   in Loop: Header=BB83_107 Depth=1
	testq	$32, %rcx
	jne	.LBB83_110
# BB#111:                               # %if.else17.i
                                        #   in Loop: Header=BB83_107 Depth=1
	leaq	1(%rbx), %rdi
	testq	$2, %rcx
	je	.LBB83_113
# BB#112:                               # %if.then24.i
                                        #   in Loop: Header=BB83_107 Depth=1
	movw	%r8w, cost_table_+2(%rbx,%rbx)
	jmp	.LBB83_113
	.align	16, 0x90
.LBB83_108:                             # %if.then4.i
                                        #   in Loop: Header=BB83_107 Depth=1
	movw	%si, cost_table_+2(%rbx,%rbx)
	leaq	1(%rbx), %rdi
	jmp	.LBB83_113
.LBB83_110:                             # %if.then13.i
                                        #   in Loop: Header=BB83_107 Depth=1
	movw	%dx, cost_table_+2(%rbx,%rbx)
	leaq	1(%rbx), %rdi
.LBB83_113:                             # %for.inc.i
                                        #   in Loop: Header=BB83_107 Depth=1
	cmpq	$128, %rdi
	movq	%rdi, %rbx
	jne	.LBB83_107
# BB#114:                               # %for.cond31.preheader.i
	movq	$8, %rcx
	movw	%cx, cost_table_+66(%rip)
	movq	$4, %rcx
	movw	%cx, cost_table_+20(%rip)
	movw	%cx, cost_table_+2(%rip)
	movq	$2, %rcx
	movw	%cx, cost_table_+22(%rip)
	movq	$1, %rcx
	movw	%cx, cost_table_+26(%rip)
	movw	%cx, cost_table_+24(%rip)
	movw	%cx, cost_table_+18(%rip)
	testq	%r13, %r13
	je	.LBB83_127
.LBB83_115:                             # %for.body33.lr.ph.i
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	40(%rcx), %r8
.LBB83_116:                             # %for.body33.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB83_124 Depth 2
	movq	24(%r13), %rsi
	movq	40(%rsi), %rdi
	xorq	%rcx, %rcx
	cmpq	%r8, %rdi
	jl	.LBB83_127
# BB#117:                               # %lor.lhs.false.i694
                                        #   in Loop: Header=BB83_116 Depth=1
	jne	.LBB83_119
# BB#118:                               # %land.lhs.true.i695
                                        #   in Loop: Header=BB83_116 Depth=1
	movq	32(%rsi), %rdi
	movq	-72(%rbp), %rdx         # 8-byte Reload
	movq	32(%rdx), %rbx
	cmpq	%rbx, %rdi
	jb	.LBB83_127
.LBB83_119:                             # %lor.lhs.false59.i
                                        #   in Loop: Header=BB83_116 Depth=1
	movq	32(%r13), %rdi
	movq	40(%rax), %rbx
	movq	40(%rdi), %rdx
	cmpq	%rdx, %rbx
	jl	.LBB83_127
# BB#120:                               # %lor.lhs.false69.i
                                        #   in Loop: Header=BB83_116 Depth=1
	jne	.LBB83_121
# BB#122:                               # %land.lhs.true79.i
                                        #   in Loop: Header=BB83_116 Depth=1
	movq	32(%rax), %rdx
	movq	32(%rdi), %rdi
	cmpq	%rdi, %rdx
	jae	.LBB83_123
	jmp	.LBB83_127
.LBB83_121:                             # %lor.lhs.false69.if.end90_crit_edge.i
                                        #   in Loop: Header=BB83_116 Depth=1
	movq	32(%rdi), %rdi
.LBB83_123:                             # %if.end90.i
                                        #   in Loop: Header=BB83_116 Depth=1
	movslq	32(%rsi), %rsi
	.align	16, 0x90
.LBB83_124:                             # %for.cond96.i
                                        #   Parent Loop BB83_116 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	cmpq	%rdi, %rsi
	jg	.LBB83_126
# BB#125:                               # %for.body104.i
                                        #   in Loop: Header=BB83_124 Depth=2
	movswq	cost_table_+2(%rsi,%rsi), %rdx
	leaq	1(%rsi), %rsi
	testq	%rdx, %rdx
	jns	.LBB83_124
	jmp	.LBB83_127
.LBB83_126:                             # %for.inc116.i
                                        #   in Loop: Header=BB83_116 Depth=1
	movq	8(%r13), %r13
	testq	%r13, %r13
	movq	$1, %rcx
	jne	.LBB83_116
.LBB83_127:                             # %land.end
	movl	%ecx, use_cost_table(%rip)
	xorq	%rsi, %rsi
	movq	-80(%rbp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	balance_case_nodes
	movq	(%rbx), %rsi
	movq	%r14, %rdi
	movq	-112(%rbp), %rbx        # 8-byte Reload
	movq	%rbx, %rdx
	movq	-64(%rbp), %rcx         # 8-byte Reload
	callq	emit_case_nodes
	callq	get_last_insn
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	movq	-104(%rbp), %r12        # 8-byte Reload
	jne	.LBB83_102
	jmp	.LBB83_160
.LBB83_87:                              # %if.then247
	cmpq	$25, %rcx
	je	.LBB83_93
# BB#88:                                # %if.then254
	movq	8(%r14), %rdi
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rcx
	movq	%rax, %rsi
	jne	.LBB83_90
# BB#89:                                # %if.then254
	movq	%rcx, %rsi
.LBB83_90:                              # %if.then254
	testq	%rdi, %rdi
	movq	-64(%rbp), %rbx         # 8-byte Reload
	jns	.LBB83_92
# BB#91:                                # %if.then254
	movq	%rcx, %rax
.LBB83_92:                              # %if.then254
	andq	%rax, %rsi
	movq	$63, %rcx
	shlq	%cl, %rsi
	movq	$63, %rcx
	sarq	%cl, %rsi
	callq	build_int_2_wide
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	convert
	movq	%rax, %r15
.LBB83_93:                              # %if.end265
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	movq	-104(%rbp), %r13        # 8-byte Reload
	movq	-112(%rbp), %r14        # 8-byte Reload
	je	.LBB83_97
	.align	16, 0x90
.LBB83_94:                              # %for.body271
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movq	%r15, %rdi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	jne	.LBB83_96
# BB#95:                                # %land.lhs.true275
                                        #   in Loop: Header=BB83_94 Depth=1
	movq	32(%rbx), %rdi
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	je	.LBB83_98
.LBB83_96:                              # %for.inc281
                                        #   in Loop: Header=BB83_94 Depth=1
	movq	8(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB83_94
.LBB83_97:                              # %if.else287
	callq	do_pending_stack_adjust
	movq	%r14, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	movq	%r13, %r12
	jmp	.LBB83_160
.LBB83_98:                              # %if.then285
	movq	40(%rbx), %r14
	movzbl	16(%r14), %eax
	cmpq	$31, %rax
	jne	.LBB83_10
# BB#99:                                # %if.end.i673
	movq	144(%r14), %rbx
	testq	%rbx, %rbx
	movq	%r13, %r12
	jne	.LBB83_102
# BB#100:                               # %if.end5.i676
	callq	gen_label_rtx
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 144(%r14)
	jne	.LBB83_102
# BB#101:                               # %cond.false.i677
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rbx
.LBB83_102:                             # %label_rtx.exit679
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
.LBB83_103:                             # %if.end415
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
.LBB83_159:                             # %if.end412
	callq	emit_barrier
.LBB83_160:                             # %if.end415
	movq	-48(%rbp), %rax
	movq	24(%rax), %rax
	movq	%rax, -48(%rbp)
	callq	get_last_insn
	movq	%rax, -56(%rbp)
	leaq	-48(%rbp), %rdi
	leaq	-56(%rbp), %rsi
	callq	squeeze_notes
	testq	%rax, %rax
	jne	.LBB83_161
# BB#162:                               # %if.end421
	movq	-48(%rbp), %rdi
	movq	-56(%rbp), %rsi
	movq	32(%r12), %rdx
	callq	reorder_insns
.LBB83_165:                             # %if.end425
	movq	24(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB83_167
# BB#166:                               # %if.then427
	callq	emit_label
.LBB83_167:                             # %if.end430
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %rax
	movq	40(%rax), %rdi
	callq	free_case_nodes
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
	movq	32(%rax), %r15
	movabsq	$stmt_obstack, %r14
	jmp	.LBB83_168
	.align	16, 0x90
.LBB83_183:                             # %do.cond.do.body438_crit_edge
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_168:                             # %do.body438
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rcx
	movq	40(%rax), %rbx
	cmpq	%rbx, %rcx
	jne	.LBB83_170
# BB#169:                               # %if.then443
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 24(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_170:                             # %if.end448
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	16(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB83_172
# BB#171:                               # %if.then452
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 16(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_172:                             # %if.end458
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB83_174
# BB#173:                               # %if.then462
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, (%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_174:                             # %if.end468
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB83_176
# BB#175:                               # %if.then472
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 8(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_176:                             # %if.end478
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	32(%rax), %rcx
	cmpq	%rbx, %rcx
	jne	.LBB83_178
# BB#177:                               # %if.then483
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	8(%rbx), %rcx
	movq	%rcx, 32(%rax)
	movq	cfun(%rip), %rax
	movq	8(%rax), %rax
.LBB83_178:                             # %if.end489
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	40(%rax), %rcx
	movl	16(%rcx), %ecx
	decq	%rcx
	movl	%ecx, 48(%rax)
	movq	(%rbx), %rcx
	movq	%rcx, 40(%rax)
	movq	stmt_obstack+8(%rip), %rax
	cmpq	%rax, %rbx
	jbe	.LBB83_181
# BB#179:                               # %if.end489
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	stmt_obstack+32(%rip), %rax
	cmpq	%rax, %rbx
	jae	.LBB83_181
# BB#180:                               # %if.then500
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	%rbx, stmt_obstack+16(%rip)
	movq	%rbx, stmt_obstack+24(%rip)
	jmp	.LBB83_182
	.align	16, 0x90
.LBB83_181:                             # %if.else501
                                        #   in Loop: Header=BB83_168 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	obstack_free
.LBB83_182:                             # %do.cond
                                        #   in Loop: Header=BB83_168 Depth=1
	cmpq	%r15, %rbx
	jne	.LBB83_183
# BB#184:                               # %do.end506
	callq	free_temp_slots
.LBB83_185:                             # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB83_128:                             # %if.else308
	movq	-64(%rbp), %rdi         # 8-byte Reload
	movq	-72(%rbp), %rsi         # 8-byte Reload
	movq	-128(%rbp), %rdx        # 8-byte Reload
	movq	%r15, %rcx
	movq	-120(%rbp), %r8         # 8-byte Reload
	movq	%r13, %r9
	callq	try_casesi
	testq	%r12, %rax
	je	.LBB83_139
# BB#129:
	movq	%r13, -112(%rbp)        # 8-byte Spill
	jmp	.LBB83_130
.LBB83_139:                             # %if.then311
	movl	optimize_size(%rip), %eax
	movq	-104(%rbp), %rcx        # 8-byte Reload
	movq	64(%rcx), %rbx
	testq	%rax, %rax
	jne	.LBB83_140
# BB#141:                               # %land.lhs.true315
	xorq	%rsi, %rsi
	movq	-128(%rbp), %r14        # 8-byte Reload
	movq	%r14, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB83_140
# BB#142:                               # %land.lhs.true319
	movq	$3, %rsi
	movq	%r14, %rdi
	callq	compare_tree_int
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	global_trees+88(%rip), %rdx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB83_144
# BB#143:                               # %land.lhs.true319
	movq	%r15, -136(%rbp)        # 8-byte Spill
.LBB83_144:                             # %land.lhs.true319
	js	.LBB83_146
# BB#145:                               # %land.lhs.true319
	movq	%r14, %rdx
.LBB83_146:                             # %land.lhs.true319
	movq	%rdx, -128(%rbp)        # 8-byte Spill
	movq	-120(%rbp), %r8         # 8-byte Reload
	movq	-136(%rbp), %rax        # 8-byte Reload
	jmp	.LBB83_147
.LBB83_140:
	movq	%r15, %rax
	movq	-120(%rbp), %r8         # 8-byte Reload
.LBB83_147:                             # %if.end324
	movq	%r8, -120(%rbp)         # 8-byte Spill
	movq	%rbx, %rdi
	movq	%rbx, -64(%rbp)         # 8-byte Spill
	movq	-72(%rbp), %rsi         # 8-byte Reload
	movq	-128(%rbp), %rdx        # 8-byte Reload
	movq	%rax, %rbx
	movq	%rbx, %rcx
	movq	%r13, %r9
	movq	%r13, -112(%rbp)        # 8-byte Spill
	callq	try_tablejump
	testq	%r12, %rax
	movq	%rbx, %r15
	je	.LBB83_148
.LBB83_130:                             # %if.end329
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	tree_low_cst
	incq	%rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$29, %rcx
	sarq	%cl, %rdx
	leaq	15(%rdx), %rax
	andq	$-16, %rax
	movq	%rsp, %rdi
	subq	%rax, %rdi
	movq	%rdi, -72(%rbp)         # 8-byte Spill
	movq	%rdi, %rsp
	xorq	%rsi, %rsi
	callq	memset
	movq	-80(%rbp), %rax         # 8-byte Reload
	movq	(%rax), %r14
	jmp	.LBB83_131
.LBB83_156:                             # %for.inc364
                                        #   in Loop: Header=BB83_131 Depth=1
	movq	8(%r14), %r14
.LBB83_131:                             # %if.end329
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB83_151 Depth 2
	testq	%r14, %r14
	je	.LBB83_132
# BB#149:                               # %for.body341
                                        #   in Loop: Header=BB83_131 Depth=1
	movq	24(%r14), %rdx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	-64(%rbp), %r15         # 8-byte Reload
	movq	%r15, %rsi
	movq	-128(%rbp), %r12        # 8-byte Reload
	movq	%r12, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	32(%r14), %rdx
	movq	%rax, %rbx
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	movq	%r12, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	$1, %rsi
	movq	%rax, %rdi
	callq	tree_low_cst
	movq	%rax, %r12
	cmpq	%r12, %rbx
	jg	.LBB83_156
# BB#150:                               # %for.body354.lr.ph
                                        #   in Loop: Header=BB83_131 Depth=1
	decq	%rbx
	.align	16, 0x90
.LBB83_151:                             # %for.body354
                                        #   Parent Loop BB83_131 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	40(%r14), %r13
	movl	target_flags(%rip), %r15d
	movzbl	16(%r13), %eax
	movq	$25, %rcx
	shrq	%cl, %r15
	cmpq	$31, %rax
	jne	.LBB83_10
# BB#152:                               # %if.end.i712
                                        #   in Loop: Header=BB83_151 Depth=2
	movq	144(%r13), %rdx
	testq	%rdx, %rdx
	jne	.LBB83_155
# BB#153:                               # %if.end5.i715
                                        #   in Loop: Header=BB83_151 Depth=2
	callq	gen_label_rtx
	movq	%rax, %rdx
	testq	%rdx, %rdx
	movq	%rdx, 144(%r13)
	jne	.LBB83_155
# BB#154:                               # %cond.false.i716
                                        #   in Loop: Header=BB83_151 Depth=2
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	make_decl_rtl
	movq	144(%r13), %rdx
.LBB83_155:                             # %label_rtx.exit718
                                        #   in Loop: Header=BB83_151 Depth=2
	andq	$1, %r15
	orq	$4, %r15
	movq	$67, %rdi
	movq	%r15, %rsi
	callq	gen_rtx_fmt_u00
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	%rax, 8(%rcx,%rbx,8)
	incq	%rbx
	cmpq	%r12, %rbx
	jl	.LBB83_151
	jmp	.LBB83_156
.LBB83_132:                             # %for.cond367.preheader
	movq	$32, %rcx
	movq	-88(%rbp), %r13         # 8-byte Reload
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	-112(%rbp), %r15        # 8-byte Reload
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jle	.LBB83_137
# BB#133:                               # %for.body370.lr.ph
	movq	$1, %rbx
	movq	%r13, %r14
	andq	%r12, %r14
.LBB83_134:                             # %for.body370
                                        # =>This Inner Loop Header: Depth=1
	movq	-72(%rbp), %rax         # 8-byte Reload
	cmpq	$0, -8(%rax,%rbx,8)
	jne	.LBB83_136
# BB#135:                               # %if.then375
                                        #   in Loop: Header=BB83_134 Depth=1
	movl	target_flags(%rip), %esi
	movq	$25, %rcx
	shrq	%cl, %rsi
	andq	$1, %rsi
	orq	$4, %rsi
	movq	$67, %rdi
	movq	%r15, %rdx
	callq	gen_rtx_fmt_u00
	movq	-72(%rbp), %rcx         # 8-byte Reload
	movq	%rax, -8(%rcx,%rbx,8)
.LBB83_136:                             # %for.inc383
                                        #   in Loop: Header=BB83_134 Depth=1
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	cmpq	%r14, %rax
	jne	.LBB83_134
.LBB83_137:                             # %for.end385
	movq	-120(%rbp), %r14        # 8-byte Reload
	movq	%r14, %rdi
	callq	emit_label
	movl	target_flags(%rip), %ebx
	movl	flag_pic(%rip), %eax
	movq	$25, %rcx
	shrq	%cl, %rbx
	andq	$1, %rbx
	orq	$4, %rbx
	testq	%rax, %rax
	je	.LBB83_157
# BB#138:                               # %lor.end393
	movq	$67, %rdi
	movq	%rbx, %rsi
	movq	%r14, %rdx
	callq	gen_rtx_fmt_u00
	movq	%rax, %rbx
	movq	%r13, %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	callq	gen_rtvec_v
	movq	const_int_rtx+512(%rip), %r8
	movq	$45, %rdi
	movq	$4, %rsi
	movq	%rbx, %rdx
	movq	%rax, %rcx
	movq	%r8, %r9
	callq	gen_rtx_fmt_eEee0
	jmp	.LBB83_158
.LBB83_157:                             # %if.else402
	movq	%r13, %rdi
	movq	-72(%rbp), %rsi         # 8-byte Reload
	callq	gen_rtvec_v
	movq	$44, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	gen_rtx_fmt_E
.LBB83_158:                             # %if.end412
	movq	%rax, %rdi
	callq	emit_jump_insn
	movq	-104(%rbp), %r12        # 8-byte Reload
	jmp	.LBB83_159
.LBB83_46:                              # %if.then72
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_end_case_type, %rcx
	movq	$5336, %rsi             # imm = 0x14D8
	jmp	.LBB83_11
.LBB83_48:                              # %if.then79
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_end_case_type, %rcx
	movq	$5338, %rsi             # imm = 0x14DA
	jmp	.LBB83_11
.LBB83_161:                             # %if.then420
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_end_case_type, %rcx
	movq	$5547, %rsi             # imm = 0x15AB
	jmp	.LBB83_11
.LBB83_148:                             # %if.then327
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.expand_end_case_type, %rcx
	movq	$5490, %rsi             # imm = 0x1572
	jmp	.LBB83_11
.LBB83_10:                              # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
.LBB83_11:                              # %if.then.i
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp694:
	.size	expand_end_case_type, .Ltmp694-expand_end_case_type
	.cfi_endproc

	.align	16, 0x90
	.type	balance_case_nodes,@function
balance_case_nodes:                     # @balance_case_nodes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp701:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp702:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp703:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp704:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp705:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp706:
	.cfi_def_cfa_offset 64
.Ltmp707:
	.cfi_offset %rbx, -48
.Ltmp708:
	.cfi_offset %r12, -40
.Ltmp709:
	.cfi_offset %r13, -32
.Ltmp710:
	.cfi_offset %r14, -24
.Ltmp711:
	.cfi_offset %r15, -16
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB84_31
# BB#1:
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB84_2:                               # %while.cond.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB84_3 Depth 2
                                        #     Child Loop BB84_16 Depth 2
                                        #     Child Loop BB84_29 Depth 2
	movq	%rsi, (%rsp)            # 8-byte Spill
	xorq	%r14, %r14
	testq	%rbx, %rbx
	movabsq	$0, %rax
	movabsq	$0, %r15
	je	.LBB84_11
	.align	16, 0x90
.LBB84_3:                               # %while.body
                                        #   Parent Loop BB84_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rdi
	movq	32(%rbx), %rsi
	movq	%rax, %r13
	callq	tree_int_cst_equal
	testq	%r12, %rax
	jne	.LBB84_6
# BB#4:                                 # %if.then3
                                        #   in Loop: Header=BB84_3 Depth=2
	movl	use_cost_table(%rip), %eax
	incq	%r14
	testq	%rax, %rax
	je	.LBB84_8
# BB#5:                                 # %if.end10.thread
                                        #   in Loop: Header=BB84_3 Depth=2
	movq	32(%rbx), %rax
	movq	32(%rax), %rax
	movswq	cost_table_+2(%rax,%rax), %rax
	addq	%rax, %r15
	jmp	.LBB84_7
	.align	16, 0x90
.LBB84_6:                               # %if.end10
                                        #   in Loop: Header=BB84_3 Depth=2
	movl	use_cost_table(%rip), %eax
	testq	%rax, %rax
	je	.LBB84_8
.LBB84_7:                               # %if.then12
                                        #   in Loop: Header=BB84_3 Depth=2
	movq	24(%rbx), %rax
	movq	32(%rax), %rax
	movswq	cost_table_+2(%rax,%rax), %rax
	addq	%rax, %r15
.LBB84_8:                               # %if.end21
                                        #   in Loop: Header=BB84_3 Depth=2
	movq	8(%rbx), %rbx
	leaq	1(%r13), %rax
	testq	%rbx, %rbx
	jne	.LBB84_3
# BB#9:                                 # %while.end
                                        #   in Loop: Header=BB84_2 Depth=1
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	(%rcx), %rsi
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$2, %rdx
	jl	.LBB84_10
# BB#14:                                # %if.then24
                                        #   in Loop: Header=BB84_2 Depth=1
	movl	use_cost_table(%rip), %ecx
	testq	%rcx, %rcx
	jne	.LBB84_15
# BB#24:                                # %if.else
                                        #   in Loop: Header=BB84_2 Depth=1
	andq	%r12, %rax
	cmpq	$3, %rax
	jne	.LBB84_26
# BB#25:                                # %if.then71
                                        #   in Loop: Header=BB84_2 Depth=1
	leaq	8(%rsi), %r15
	movq	%rsi, %r13
	jmp	.LBB84_30
	.align	16, 0x90
.LBB84_15:                              # %if.then26
                                        #   in Loop: Header=BB84_2 Depth=1
	leaq	1(%r15), %rax
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rax
	movq	$31, %rcx
	shrq	%cl, %rax
	leaq	1(%r15,%rax), %rbx
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$33, %rcx
	sarq	%cl, %rbx
	xorq	%r14, %r14
	movq	%rsi, %rax
	movq	%rsi, %r13
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB84_16
	.align	16, 0x90
.LBB84_19:                              # %if.end54
                                        #   in Loop: Header=BB84_16 Depth=2
	movq	8(%rdx), %rax
	addq	$8, %rdx
	incq	%r14
	movq	%rdx, %r15
.LBB84_16:                              # %while.body29
                                        #   Parent Loop BB84_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rax), %rdi
	movq	32(%rax), %rsi
	callq	tree_int_cst_equal
	movq	(%r15), %rdx
	testq	%r12, %rax
	jne	.LBB84_18
# BB#17:                                # %if.then34
                                        #   in Loop: Header=BB84_16 Depth=2
	movq	32(%rdx), %rax
	movq	32(%rax), %rax
	movswq	cost_table_+2(%rax,%rax), %rax
	subq	%rax, %rbx
.LBB84_18:                              # %if.end42
                                        #   in Loop: Header=BB84_16 Depth=2
	movq	24(%rdx), %rax
	movq	32(%rax), %rax
	movswq	cost_table_+2(%rax,%rax), %rax
	subq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB84_19
# BB#20:                                # %while.end57
                                        #   in Loop: Header=BB84_2 Depth=1
	testq	%r12, %r14
	jne	.LBB84_30
	jmp	.LBB84_21
	.align	16, 0x90
.LBB84_26:                              # %if.else73
                                        #   in Loop: Header=BB84_2 Depth=1
	leaq	(%r13,%r14), %rax
	leaq	2(%r13,%r14), %rdx
	movabsq	$2147483648, %rcx       # imm = 0x80000000
	andq	%rcx, %rdx
	movq	$31, %rcx
	shrq	%cl, %rdx
	leaq	2(%rdx,%rax), %rbx
	movq	$32, %rcx
	movq	24(%rsi), %rdi
	movq	%rsi, %rax
	movq	32(%rax), %rsi
	movq	%rax, %r13
	shlq	%cl, %rbx
	movq	$33, %rcx
	sarq	%cl, %rbx
	callq	tree_int_cst_equal
	testq	%r12, %rax
	movabsq	$0, %rax
	jne	.LBB84_28
# BB#27:                                # %if.else73
                                        #   in Loop: Header=BB84_2 Depth=1
	movabsq	$1, %rax
.LBB84_28:                              # %if.else73
                                        #   in Loop: Header=BB84_2 Depth=1
	subq	%rax, %rbx
	cmpq	$2, %rbx
	movq	8(%rsp), %r15           # 8-byte Reload
	jl	.LBB84_30
	.align	16, 0x90
.LBB84_29:                              # %if.end89
                                        #   Parent Loop BB84_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	(%r15), %r15
	movq	8(%r15), %rax
	movq	24(%rax), %rdi
	movq	32(%rax), %rsi
	addq	$8, %r15
	callq	tree_int_cst_equal
	andq	%r12, %rax
	cmpq	$1, %rax
	sbbq	%rax, %rax
	leaq	-1(%rbx,%rax), %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jg	.LBB84_29
	.align	16, 0x90
.LBB84_30:                              # %if.end93
                                        #   in Loop: Header=BB84_2 Depth=1
	movq	(%r15), %r14
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%r14, (%rax)
	movq	$0, (%r15)
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, 16(%r14)
	movq	%r13, (%r14)
	movq	%r14, %rdi
	movq	%r14, %rsi
	callq	balance_case_nodes
	movq	8(%r14), %rbx
	testq	%rbx, %rbx
	movq	%r14, %rsi
	leaq	8(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jne	.LBB84_2
	jmp	.LBB84_31
.LBB84_10:
	movq	%rsi, %r14
.LBB84_11:                              # %if.else98
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, 16(%r14)
	movq	8(%r14), %rax
	jmp	.LBB84_12
	.align	16, 0x90
.LBB84_13:                              # %for.body103
                                        #   in Loop: Header=BB84_12 Depth=1
	movq	%rax, %rcx
	movq	%r14, 16(%rcx)
	movq	8(%rcx), %rax
	movq	%rcx, %r14
.LBB84_12:                              # %if.else98
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB84_13
	jmp	.LBB84_31
.LBB84_21:                              # %if.then60
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rbx
	movq	(%rsp), %rax            # 8-byte Reload
	movq	%rax, 16(%rbx)
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	balance_case_nodes
	movq	8(%rbx), %rax
	jmp	.LBB84_23
	.align	16, 0x90
.LBB84_22:                              # %for.body
                                        #   in Loop: Header=BB84_23 Depth=1
	movq	%rax, %rcx
	movq	%rbx, 16(%rcx)
	movq	8(%rcx), %rax
	movq	%rcx, %rbx
.LBB84_23:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	testq	%rax, %rax
	jne	.LBB84_22
.LBB84_31:                              # %if.end110
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp712:
	.size	balance_case_nodes, .Ltmp712-balance_case_nodes
	.cfi_endproc

	.align	16, 0x90
	.type	emit_case_nodes,@function
emit_case_nodes:                        # @emit_case_nodes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp719:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp720:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp721:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp722:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp723:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp724:
	.cfi_def_cfa_offset 128
.Ltmp725:
	.cfi_offset %rbx, -48
.Ltmp726:
	.cfi_offset %r12, -40
.Ltmp727:
	.cfi_offset %r13, -32
.Ltmp728:
	.cfi_offset %r14, -24
.Ltmp729:
	.cfi_offset %r15, -16
	movq	%rcx, %r13
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r15
	movq	%rdi, 72(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB85_1
	.align	16, 0x90
.LBB85_103:                             # %tailrecurse.backedge
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	(%rax), %r15
.LBB85_1:                               # %tailrecurse
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%r13), %r12d
	movq	$13, %rcx
	movq	72(%rsp), %rax          # 8-byte Reload
	movzbl	2(%rax), %eax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movl	60(%r13), %ebx
	movq	104(%r13), %rsi
	shrq	%cl, %r12
	movq	$9, %rcx
	shrq	%cl, %rbx
	movq	%r15, %rdi
	callq	node_has_low_bound
	testq	%r14, %rax
	je	.LBB85_9
# BB#2:                                 # %land.rhs.i
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	112(%r13), %rsi
	movq	%r15, %rdi
	callq	node_has_high_bound
	testq	%r14, %rax
	jne	.LBB85_3
.LBB85_9:                               # %if.else
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%r15), %rdi
	movq	32(%r15), %rsi
	andq	$1, %r12
	andq	$127, %rbx
	callq	tree_int_cst_equal
	testq	%r14, %rax
	jne	.LBB85_10
# BB#80:                                # %if.else131
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r12, 64(%rsp)          # 8-byte Spill
	movq	8(%r15), %r13
	movq	(%r15), %rbx
	testq	%r13, %r13
	jne	.LBB85_81
# BB#112:                               # %land.lhs.true189
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r15, %rdi
	movq	56(%rsp), %r14          # 8-byte Reload
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	112(%rax), %rsi
	movq	%rdi, %r13
	callq	node_has_high_bound
	movq	%rax, %r12
	testq	%rbx, %rbx
	movq	64(%rsp), %rbx          # 8-byte Reload
	je	.LBB85_120
# BB#113:                               # %if.then192
                                        #   in Loop: Header=BB85_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB85_115
# BB#114:                               # %if.then195
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	convert_modes
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	emit_cmp_and_jump_insns
.LBB85_115:                             # %if.end199
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%r13), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r14, %rdi
	movq	%r14, %r15
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%rbx, %r12
	callq	convert_modes
	movq	40(%r13), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#116:                               # %if.end.i615
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_119
# BB#117:                               # %if.end5.i618
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_119
# BB#118:                               # %cond.false.i619
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_119:                             # %label_rtx.exit622
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$104, %rdx
.LBB85_30:                              # %tailrecurse.backedge
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %r8
	movq	%r12, %r9
	callq	emit_cmp_and_jump_insns
	movq	%r13, %rax
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	(%rax), %r15
	jmp	.LBB85_1
	.align	16, 0x90
.LBB85_10:                              # %if.then11
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %r14
	movq	%rbx, %rsi
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%rax, %rdx
	movq	%r12, %rcx
	movq	%r12, 64(%rsp)          # 8-byte Spill
	callq	convert_modes
	movq	40(%r15), %r12
	movzbl	16(%r12), %ecx
	movq	%rax, %rbx
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#11:                                # %if.end.i503
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%r12), %r13
	testq	%r13, %r13
	jne	.LBB85_14
# BB#12:                                # %if.end5.i506
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	movq	%rax, %r13
	testq	%r13, %r13
	movq	%r13, 144(%r12)
	jne	.LBB85_14
# BB#13:                                # %cond.false.i507
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	make_decl_rtl
	movq	144(%r12), %r13
.LBB85_14:                              # %label_rtx.exit509
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB85_18
# BB#15:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB85_1 Depth=1
	movzbl	(%rbx), %eax
	cmpq	$54, %rax
	jne	.LBB85_18
# BB#16:                                # %if.then.i513
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB85_21
# BB#17:                                # %if.then8.i
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	do_pending_stack_adjust
	movq	%r13, %rdi
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
	jmp	.LBB85_21
	.align	16, 0x90
.LBB85_18:                              # %if.else.i
                                        #   in Loop: Header=BB85_1 Depth=1
	testq	$16711680, %r8          # imm = 0xFF0000
	jne	.LBB85_20
# BB#19:                                # %cond.true.i
                                        #   in Loop: Header=BB85_1 Depth=1
	movl	(%rbx), %r8d
.LBB85_20:                              # %cond.end.i
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	$103, %rdx
	xorq	%rcx, %rcx
	movq	%r13, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	emit_cmp_and_jump_insns
.LBB85_21:                              # %do_jump_if_equal.exit
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	8(%r15), %rbx
	movq	(%r15), %rax
	testq	%rbx, %rbx
	jne	.LBB85_22
# BB#65:                                # %land.lhs.true92
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r15, %rbx
	testq	%rax, %rax
	movq	%r14, %r12
	movq	40(%rsp), %r13          # 8-byte Reload
	je	.LBB85_64
# BB#66:                                # %if.then95
                                        #   in Loop: Header=BB85_1 Depth=1
	cmpq	$0, (%rax)
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jne	.LBB85_69
# BB#67:                                # %lor.lhs.false99
                                        #   in Loop: Header=BB85_1 Depth=1
	cmpq	$0, 8(%rax)
	jne	.LBB85_69
# BB#68:                                # %lor.lhs.false103
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%rax), %rdi
	movq	32(%rax), %rsi
	callq	tree_int_cst_equal
	testq	%r14, %rax
	jne	.LBB85_71
.LBB85_69:                              # %if.then110
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	112(%r13), %rsi
	movq	%rbx, %rdi
	callq	node_has_high_bound
	testq	%r14, %rax
	movq	%rbx, %rax
	jne	.LBB85_103
# BB#70:                                # %if.then113
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r14, %r15
	movq	64(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	callq	convert_modes
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r12, %r8
	movq	%r14, %r9
	movq	%r15, %r14
	callq	emit_cmp_and_jump_insns
	movq	%rbx, %rax
	movq	(%rax), %r15
	jmp	.LBB85_1
	.align	16, 0x90
.LBB85_81:                              # %land.lhs.true134
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rsi
	testq	%rbx, %rbx
	leaq	8(%r15), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	jne	.LBB85_82
# BB#104:                               # %if.then172
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r15, %rdi
	callq	node_has_low_bound
	testq	%r14, %rax
	movq	%r15, %rbx
	movq	%r14, %r15
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB85_106
# BB#105:                               # %if.then175
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	convert_modes
	movq	$107, %rdx
	xorq	%rcx, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r12, %r8
	movq	%r13, %r9
	callq	emit_cmp_and_jump_insns
.LBB85_106:                             # %if.end179
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	convert_modes
	movq	40(%rbx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#107:                               # %if.end.i766
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_110
# BB#108:                               # %if.end5.i769
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_110
# BB#109:                               # %cond.false.i770
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_110:                             # %label_rtx.exit773
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$106, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %r8
.LBB85_111:                             # %tailrecurse.backedge
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r13, %r9
	callq	emit_cmp_and_jump_insns
	movq	%r15, %r14
	movq	40(%rsp), %r13          # 8-byte Reload
	jmp	.LBB85_102
	.align	16, 0x90
.LBB85_22:                              # %land.lhs.true
                                        #   in Loop: Header=BB85_1 Depth=1
	leaq	8(%r15), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	%r15, %r12
	testq	%rax, %rax
	movq	%r14, %r15
	jne	.LBB85_23
# BB#46:                                # %if.then57
                                        #   in Loop: Header=BB85_1 Depth=1
	cmpq	$0, 8(%rbx)
	movq	64(%rsp), %r13          # 8-byte Reload
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jne	.LBB85_49
# BB#47:                                # %lor.lhs.false
                                        #   in Loop: Header=BB85_1 Depth=1
	cmpq	$0, (%rbx)
	jne	.LBB85_49
# BB#48:                                # %lor.lhs.false64
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	24(%rbx), %rdi
	movq	32(%rbx), %rsi
	callq	tree_int_cst_equal
	testq	%r14, %rax
	jne	.LBB85_52
.LBB85_49:                              # %if.then71
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rsi
	movq	%r12, %rbx
	movq	%rbx, %rdi
	callq	node_has_low_bound
	testq	%r14, %rax
	jne	.LBB85_50
# BB#51:                                # %if.then74
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r15, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	convert_modes
	movq	$107, %rdx
	xorq	%rcx, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r15, %r8
	movq	%r13, %r9
	callq	emit_cmp_and_jump_insns
	movq	40(%rsp), %r13          # 8-byte Reload
	jmp	.LBB85_102
.LBB85_82:                              # %if.then137
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r13, %rdi
	callq	node_has_low_bound
	testq	%r14, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	je	.LBB85_89
# BB#83:                                # %land.rhs.i711
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	112(%rax), %rsi
	movq	%r13, %rdi
	callq	node_has_high_bound
	testq	%r14, %rax
	jne	.LBB85_84
.LBB85_89:                              # %if.else149
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$31, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	build_decl
	movq	32(%r15), %rdi
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#90:                                # %if.end.i736
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r15, %r13
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_93
# BB#91:                                # %if.end5.i739
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_93
# BB#92:                                # %cond.false.i740
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_93:                              # %label_rtx.exit743
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	jmp	.LBB85_94
.LBB85_23:                              # %if.then18
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	40(%rsp), %r14          # 8-byte Reload
	movq	104(%r14), %rsi
	movq	%rbx, %rdi
	callq	node_has_low_bound
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	je	.LBB85_31
# BB#24:                                # %land.rhs.i522
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	112(%r14), %rsi
	movq	%rbx, %rdi
	callq	node_has_high_bound
	testq	%r15, %rax
	jne	.LBB85_25
.LBB85_31:                              # %if.else30
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	(%r12), %rbx
	movq	104(%r14), %rsi
	movq	%rbx, %rdi
	callq	node_has_low_bound
	testq	%r15, %rax
	je	.LBB85_38
# BB#32:                                # %land.rhs.i551
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	112(%r14), %rsi
	movq	%rbx, %rdi
	callq	node_has_high_bound
	testq	%r15, %rax
	jne	.LBB85_33
.LBB85_38:                              # %if.else42
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$31, %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	build_decl
	movq	32(%r12), %rdi
	movq	%rax, %r13
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movzbl	16(%r13), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#39:                                # %if.end.i591
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%r13), %rax
	testq	%rax, %rax
	jne	.LBB85_42
# BB#40:                                # %if.end5.i594
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%r13)
	jne	.LBB85_42
# BB#41:                                # %cond.false.i595
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	make_decl_rtl
	movq	144(%r13), %rax
.LBB85_42:                              # %label_rtx.exit598
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	56(%rsp), %r8           # 8-byte Reload
	movq	64(%rsp), %r9           # 8-byte Reload
	callq	emit_cmp_and_jump_insns
	movq	(%r12), %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	callq	emit_case_nodes
	callq	get_last_insn
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB85_44
# BB#43:                                # %if.then.i606
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	do_pending_stack_adjust
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
.LBB85_44:                              # %emit_jump_if_reachable.exit
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r13, %rdi
	callq	expand_label
	movq	%r15, %r14
	movq	40(%rsp), %r13          # 8-byte Reload
	jmp	.LBB85_102
.LBB85_50:                              #   in Loop: Header=BB85_1 Depth=1
	movq	40(%rsp), %r13          # 8-byte Reload
	jmp	.LBB85_102
.LBB85_84:                              # %if.then142
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %rcx          # 8-byte Reload
	callq	convert_modes
	movq	8(%r15), %rcx
	movq	40(%rcx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#85:                                # %if.end.i721
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_88
# BB#86:                                # %if.end5.i724
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_88
# BB#87:                                # %cond.false.i725
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_88:                              # %label_rtx.exit728
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r15, %r13
	movq	%rax, (%rsp)
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
.LBB85_94:                              # %if.end155
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %r8
	movq	64(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %r9
	callq	emit_cmp_and_jump_insns
	movq	%r13, %r14
	movq	24(%r14), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	%r12, %r13
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	movq	%rbx, %r12
	callq	convert_modes
	movq	40(%r14), %rbx
	movq	%r14, %r15
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#95:                                # %if.end.i751
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_98
# BB#96:                                # %if.end5.i754
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_98
# BB#97:                                # %cond.false.i755
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_98:                              # %label_rtx.exit758
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$104, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	movq	%r14, %rsi
	movq	%r13, %r8
	movq	%r12, %r9
	callq	emit_cmp_and_jump_insns
	movq	(%r15), %rsi
	movq	%rbx, %rdi
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	40(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rcx
	callq	emit_case_nodes
	movq	16(%rsp), %rbx          # 8-byte Reload
	testq	%rbx, %rbx
	je	.LBB85_64
# BB#99:                                # %if.then163
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	get_last_insn
	movzbl	(%rax), %eax
	cmpq	$35, %rax
	je	.LBB85_101
# BB#100:                               # %if.then.i781
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	do_pending_stack_adjust
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	callq	emit_barrier
.LBB85_101:                             # %emit_jump_if_reachable.exit783
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%rbx, %rdi
	callq	expand_label
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
.LBB85_102:                             # %tailrecurse.backedge
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%rsp), %rax          # 8-byte Reload
	jmp	.LBB85_103
.LBB85_25:                              # %if.then22
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	%r12, %rbx
	movq	32(%rbx), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, %r15
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rcx
	callq	convert_modes
	movq	8(%rbx), %rcx
	movq	%rbx, %r13
	movq	40(%rcx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#26:                                # %if.end.i532
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_29
# BB#27:                                # %if.end5.i535
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_29
# BB#28:                                # %cond.false.i536
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_29:                              # %label_rtx.exit539
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$105, %rdx
	jmp	.LBB85_30
.LBB85_33:                              # %if.then34
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	32(%r12), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	56(%rsp), %rdi          # 8-byte Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rcx
	callq	convert_modes
	movq	(%r12), %rcx
	movq	40(%rcx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#34:                                # %if.end.i561
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	144(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB85_37
# BB#35:                                # %if.end5.i564
                                        #   in Loop: Header=BB85_1 Depth=1
	callq	gen_label_rtx
	testq	%rax, %rax
	movq	%rax, 144(%rbx)
	jne	.LBB85_37
# BB#36:                                # %cond.false.i565
                                        #   in Loop: Header=BB85_1 Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %rax
.LBB85_37:                              # %label_rtx.exit568
                                        #   in Loop: Header=BB85_1 Depth=1
	movq	$107, %rdx
	xorq	%rcx, %rcx
	movq	%rax, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	56(%rsp), %r8           # 8-byte Reload
	jmp	.LBB85_111
.LBB85_120:                             # %if.else206
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	104(%rax), %rsi
	movq	%r13, %rdi
	callq	node_has_low_bound
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %r12
	jne	.LBB85_124
# BB#121:                               # %if.else206
	testq	%r15, %rax
	je	.LBB85_124
# BB#122:                               # %if.then212
	movq	%r13, %r15
	movq	32(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	convert_modes
	movq	$105, %rdx
	jmp	.LBB85_123
.LBB85_124:                             # %if.else216
	testq	%r15, %rax
	jne	.LBB85_127
# BB#125:                               # %if.else216
	testq	%r15, %r12
	je	.LBB85_127
# BB#126:                               # %if.then220
	movq	%r13, %r15
	movq	24(%r15), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r14, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%rbx, %rcx
	callq	convert_modes
	movq	$107, %rdx
.LBB85_123:                             # %if.end244
	xorq	%rcx, %rcx
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r14, %r8
	movq	%rbx, %r9
	callq	emit_cmp_and_jump_insns
	jmp	.LBB85_3
.LBB85_127:                             # %if.else224
	orq	%r12, %rax
	testq	%r15, %rax
	movq	%r13, %r15
	jne	.LBB85_3
# BB#128:                               # %if.then228
	movq	%r14, %r12
	movq	%r12, %rdi
	movq	%rbx, %r13
	movq	%r13, %rsi
	callq	type_for_mode
	movq	24(%r15), %rdx
	movq	%rax, %rbx
	movq	$114, %rdi
	movq	%rbx, %rsi
	callq	build1
	movq	32(%r15), %rdx
	movq	%rax, %r14
	movq	$114, %rdi
	movq	%rbx, %rsi
	callq	build1
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r12, %rdx
	callq	expand_expr
	movq	$76, %rsi
	xorq	%r8, %r8
	movq	$2, (%rsp)
	movq	%r12, %rdi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	%rax, %rcx
	movq	%r13, %r9
	callq	expand_simple_binop
	movq	%rax, %r13
	movq	$60, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	%r14, %rcx
	callq	build
	movq	%rax, %rdi
	callq	fold
	xorq	%rsi, %rsi
	xorq	%rcx, %rcx
	movq	%rax, %rdi
	movq	%r12, %rdx
	callq	expand_expr
	movq	$105, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r9
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, (%rsp)
	movq	%r13, %rdi
	movq	%rax, %rsi
	movq	%r12, %r8
	callq	emit_cmp_and_jump_insns
.LBB85_3:                               # %if.then
	movq	40(%r15), %r14
	movzbl	16(%r14), %eax
	cmpq	$31, %rax
	jne	.LBB85_130
# BB#4:                                 # %if.end.i
	movq	144(%r14), %rbx
	testq	%rbx, %rbx
	jne	.LBB85_7
# BB#5:                                 # %if.end5.i
	callq	gen_label_rtx
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 144(%r14)
	jne	.LBB85_7
# BB#6:                                 # %cond.false.i
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	make_decl_rtl
	movq	144(%r14), %rbx
.LBB85_7:                               # %label_rtx.exit
	callq	do_pending_stack_adjust
	movq	%rbx, %rdi
.LBB85_8:                               # %label_rtx.exit
	callq	gen_jump
	movq	%rax, %rdi
	callq	emit_jump_insn
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	emit_barrier  # TAILCALL
.LBB85_71:                              # %if.else119
	movq	(%rbx), %rax
	movq	24(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	64(%rsp), %r12          # 8-byte Reload
	movq	%r12, %rcx
	callq	convert_modes
	movq	(%rbx), %rcx
	movq	40(%rcx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#72:                                # %if.end.i670
	movq	144(%rbx), %r15
	testq	%r15, %r15
	jne	.LBB85_75
# BB#73:                                # %if.end5.i673
	callq	gen_label_rtx
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%r15, 144(%rbx)
	jne	.LBB85_75
# BB#74:                                # %cond.false.i674
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r15
.LBB85_75:                              # %label_rtx.exit677
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB85_77
# BB#76:                                # %land.lhs.true.i684
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	je	.LBB85_58
.LBB85_77:                              # %if.else.i695
	testq	$16711680, %r8          # imm = 0xFF0000
	jne	.LBB85_79
# BB#78:                                # %cond.true.i697
	movl	(%r14), %r8d
.LBB85_79:                              # %cond.end.i701
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	$103, %rdx
	xorq	%rcx, %rcx
	movq	%r15, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r12, %r9
	jmp	.LBB85_63
.LBB85_52:                              # %if.else80
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	(%rbx), %rax
	movq	24(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	expand_expr
	movq	%r15, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
	movq	%r13, %rcx
	callq	convert_modes
	movq	(%rbx), %rcx
	movq	40(%rcx), %rbx
	movzbl	16(%rbx), %ecx
	movq	%rax, %r14
	cmpq	$31, %rcx
	jne	.LBB85_130
# BB#53:                                # %if.end.i630
	movq	144(%rbx), %r15
	testq	%r15, %r15
	jne	.LBB85_56
# BB#54:                                # %if.end5.i633
	callq	gen_label_rtx
	movq	%rax, %r15
	testq	%r15, %r15
	movq	%r15, 144(%rbx)
	jne	.LBB85_56
# BB#55:                                # %cond.false.i634
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	make_decl_rtl
	movq	144(%rbx), %r15
.LBB85_56:                              # %label_rtx.exit637
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %r8d
	movq	%r8, %rax
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$54, %rax
	jne	.LBB85_60
# BB#57:                                # %land.lhs.true.i644
	movzbl	(%r14), %eax
	cmpq	$54, %rax
	jne	.LBB85_60
.LBB85_58:                              # %if.then.i648
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %rax
	movq	8(%r14), %rcx
	cmpq	%rcx, %rax
	jne	.LBB85_64
# BB#59:                                # %if.then8.i652
	callq	do_pending_stack_adjust
	movq	%r15, %rdi
	jmp	.LBB85_8
.LBB85_60:                              # %if.else.i655
	testq	$16711680, %r8          # imm = 0xFF0000
	jne	.LBB85_62
# BB#61:                                # %cond.true.i657
	movl	(%r14), %r8d
.LBB85_62:                              # %cond.end.i661
	movq	$16, %rcx
	shrq	%cl, %r8
	andq	$255, %r8
	movq	$103, %rdx
	xorq	%rcx, %rcx
	movq	%r15, (%rsp)
	movq	72(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%r13, %r9
.LBB85_63:                              # %cond.end.i661
	callq	emit_cmp_and_jump_insns
.LBB85_64:                              # %if.end251
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB85_130:                             # %if.then.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.label_rtx, %rcx
	movq	$683, %rsi              # imm = 0x2AB
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp730:
	.size	emit_case_nodes, .Ltmp730-emit_case_nodes
	.cfi_endproc

	.align	16, 0x90
	.type	free_case_nodes,@function
free_case_nodes:                        # @free_case_nodes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp733:
	.cfi_def_cfa_offset 16
.Ltmp734:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB86_1
# BB#2:                                 # %if.then
	movq	(%rbx), %rdi
	callq	free_case_nodes
	movq	8(%rbx), %rdi
	callq	free_case_nodes
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.LBB86_1:                               # %if.end
	popq	%rbx
	retq
.Ltmp735:
	.size	free_case_nodes, .Ltmp735-free_case_nodes
	.cfi_endproc

	.align	16, 0x90
	.type	node_has_low_bound,@function
node_has_low_bound:                     # @node_has_low_bound
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp741:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp742:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp743:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp744:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp745:
	.cfi_def_cfa_offset 48
.Ltmp746:
	.cfi_offset %rbx, -40
.Ltmp747:
	.cfi_offset %r12, -32
.Ltmp748:
	.cfi_offset %r14, -24
.Ltmp749:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	24(%rbx), %rdi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB87_5
# BB#1:                                 # %if.end
	xorq	%r14, %r14
	cmpq	$0, (%rbx)
	jne	.LBB87_6
# BB#2:                                 # %if.end3
	movq	24(%rbx), %rdx
	movq	8(%rdx), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$60, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	24(%rbx), %rsi
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	je	.LBB87_6
	.align	16, 0x90
.LBB87_3:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB87_6
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB87_3 Depth=1
	movq	32(%rbx), %rsi
	movq	%r15, %rdi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	je	.LBB87_3
.LBB87_5:
	movq	$1, %r14
.LBB87_6:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp750:
	.size	node_has_low_bound, .Ltmp750-node_has_low_bound
	.cfi_endproc

	.align	16, 0x90
	.type	node_has_high_bound,@function
node_has_high_bound:                    # @node_has_high_bound
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp756:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp757:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp758:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp759:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp760:
	.cfi_def_cfa_offset 48
.Ltmp761:
	.cfi_offset %rbx, -40
.Ltmp762:
	.cfi_offset %r12, -32
.Ltmp763:
	.cfi_offset %r14, -24
.Ltmp764:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rsi, %rsi
	je	.LBB88_6
# BB#1:                                 # %if.end
	movq	32(%rbx), %rdi
	callq	tree_int_cst_equal
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	jne	.LBB88_6
# BB#2:                                 # %if.end4
	xorq	%r14, %r14
	cmpq	$0, 8(%rbx)
	jne	.LBB88_7
# BB#3:                                 # %if.end7
	movq	32(%rbx), %rdx
	movq	8(%rdx), %rsi
	movq	global_trees+96(%rip), %rcx
	movq	$59, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	build
	movq	%rax, %rdi
	callq	fold
	movq	32(%rbx), %rdi
	movq	%rax, %r15
	movq	%r15, %rsi
	callq	tree_int_cst_lt
	testq	%r12, %rax
	je	.LBB88_7
	.align	16, 0x90
.LBB88_4:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rbx), %rbx
	testq	%rbx, %rbx
	je	.LBB88_7
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB88_4 Depth=1
	movq	24(%rbx), %rsi
	movq	%r15, %rdi
	callq	tree_int_cst_equal
	testq	%r12, %rax
	je	.LBB88_4
.LBB88_6:
	movq	$1, %r14
.LBB88_7:                               # %return
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp765:
	.size	node_has_high_bound, .Ltmp765-node_has_high_bound
	.cfi_endproc

	.align	16, 0x90
	.type	resolve_operand_name_1,@function
resolve_operand_name_1:                 # @resolve_operand_name_1
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp772:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp773:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp774:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp775:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp776:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp777:
	.cfi_def_cfa_offset 80
.Ltmp778:
	.cfi_offset %rbx, -48
.Ltmp779:
	.cfi_offset %r12, -40
.Ltmp780:
	.cfi_offset %r13, -32
.Ltmp781:
	.cfi_offset %r14, -24
.Ltmp782:
	.cfi_offset %r15, -16
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movq	$93, %rsi
                                        # kill: RDI<def> R15<kill>
	callq	strchr
	testq	%rax, %rax
	je	.LBB89_1
# BB#2:                                 # %if.end
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %r13
	subq	%r15, %r13
	movq	%r15, 16(%rsp)          # 8-byte Spill
	decq	%r13
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB89_9
# BB#3:                                 # %for.body.lr.ph
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB89_4:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r14), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB89_8
# BB#5:                                 # %if.then6
                                        #   in Loop: Header=BB89_4 Depth=1
	movq	32(%rax), %r15
	movq	%r15, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	%r13, %rdx
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB89_8
# BB#6:                                 # %land.lhs.true
                                        #   in Loop: Header=BB89_4 Depth=1
	movzbl	(%r15,%r13), %eax
	testq	%rax, %rax
	je	.LBB89_7
.LBB89_8:                               # %for.inc
                                        #   in Loop: Header=BB89_4 Depth=1
	movq	(%r14), %r14
	incq	%rbx
	testq	%r14, %r14
	jne	.LBB89_4
.LBB89_9:                               # %for.cond14.preheader
	movq	16(%rsp), %rax          # 8-byte Reload
	leaq	1(%rax), %r14
	jmp	.LBB89_10
	.align	16, 0x90
.LBB89_15:                              # %for.inc40
                                        #   in Loop: Header=BB89_10 Depth=1
	movq	(%r12), %r12
	incq	%rbx
.LBB89_10:                              # %for.cond14.preheader
                                        # =>This Inner Loop Header: Depth=1
	testq	%r12, %r12
	je	.LBB89_16
# BB#11:                                # %for.body16
                                        #   in Loop: Header=BB89_10 Depth=1
	movq	24(%r12), %rax
	movq	24(%rax), %rax
	testq	%rax, %rax
	je	.LBB89_15
# BB#12:                                # %if.then23
                                        #   in Loop: Header=BB89_10 Depth=1
	movq	32(%rax), %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r13, %rdx
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB89_15
# BB#13:                                # %land.lhs.true32
                                        #   in Loop: Header=BB89_10 Depth=1
	movzbl	(%r15,%r13), %eax
	testq	%rax, %rax
	jne	.LBB89_15
# BB#14:
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB89_17
.LBB89_16:                              # %for.end44
	xorq	%rbx, %rbx
	movq	8(%rsp), %r15           # 8-byte Reload
	movb	%bl, (%r15)
	movabsq	$.L.str23, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	error
.LBB89_17:                              # %found
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	16(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	sprintf
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %rbx
	addq	%r14, %rbx
	cmpq	%r15, %rbx
	ja	.LBB89_20
# BB#18:                                # %if.end51
	incq	%r15
	movq	%r15, %rdi
	callq	strlen
	leaq	1(%rax), %rdx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	memmove
	jmp	.LBB89_19
.LBB89_1:                               # %if.then
	movabsq	$.L.str22, %rdi
	xorq	%rax, %rax
	callq	error
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %rbx
	addq	%r15, %rbx
.LBB89_19:                              # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB89_7:
	movq	8(%rsp), %r15           # 8-byte Reload
	jmp	.LBB89_17
.LBB89_20:                              # %if.then50
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.resolve_operand_name_1, %rcx
	movq	$2184, %rsi             # imm = 0x888
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp783:
	.size	resolve_operand_name_1, .Ltmp783-resolve_operand_name_1
	.cfi_endproc

	.align	16, 0x90
	.type	mark_case_node,@function
mark_case_node:                         # @mark_case_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp788:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp789:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp790:
	.cfi_def_cfa_offset 32
.Ltmp791:
	.cfi_offset %rbx, -32
.Ltmp792:
	.cfi_offset %r14, -24
.Ltmp793:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB90_18
# BB#1:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB90_2:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB90_7
# BB#3:                                 # %land.lhs.true
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB90_7
# BB#4:                                 # %do.body3
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB90_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB90_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB90_6:                               # %if.end
                                        #   in Loop: Header=BB90_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB90_7:                               # %do.body11
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	32(%rbx), %r14
	testq	%r14, %r14
	je	.LBB90_12
# BB#8:                                 # %land.lhs.true14
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB90_12
# BB#9:                                 # %do.body18
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB90_11
# BB#10:                                # %if.then22
                                        #   in Loop: Header=BB90_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB90_11:                              # %if.end26
                                        #   in Loop: Header=BB90_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB90_12:                              # %do.body35
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	40(%rbx), %r14
	testq	%r14, %r14
	je	.LBB90_17
# BB#13:                                # %land.lhs.true38
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	%r14, %rdi
	callq	ggc_set_mark
	testq	%r15, %rax
	jne	.LBB90_17
# BB#14:                                # %do.body42
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	ggc_pending_trees(%rip), %rax
	movq	(%rax), %rsi
	movq	8(%rax), %rcx
	cmpq	%rsi, %rcx
	jb	.LBB90_16
# BB#15:                                # %if.then46
                                        #   in Loop: Header=BB90_2 Depth=1
	addq	%rsi, %rsi
	movq	%rax, %rdi
	callq	varray_grow
	movq	%rax, ggc_pending_trees(%rip)
	movq	8(%rax), %rcx
.LBB90_16:                              # %if.end50
                                        #   in Loop: Header=BB90_2 Depth=1
	leaq	1(%rcx), %rdx
	movq	%rdx, 8(%rax)
	movq	%r14, 32(%rax,%rcx,8)
.LBB90_17:                              # %do.end58
                                        #   in Loop: Header=BB90_2 Depth=1
	movq	8(%rbx), %rdi
	callq	mark_case_node
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB90_2
.LBB90_18:                              # %if.end59
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp794:
	.size	mark_case_node, .Ltmp794-mark_case_node
	.cfi_endproc

	.type	using_eh_for_cleanups_p,@object # @using_eh_for_cleanups_p
	.local	using_eh_for_cleanups_p
	.comm	using_eh_for_cleanups_p,1,1
	.type	stmt_obstack,@object    # @stmt_obstack
	.comm	stmt_obstack,88,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"stmt.c"
	.size	.L.str, 7

	.type	.L__FUNCTION__.label_rtx,@object # @__FUNCTION__.label_rtx
.L__FUNCTION__.label_rtx:
	.asciz	"label_rtx"
	.size	.L__FUNCTION__.label_rtx, 10

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"output operand constraint lacks `='"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"output constraint `%c' for operand %d is not at the beginning"
	.size	.L.str2, 62

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"operand constraint contains incorrectly positioned '+' or '='"
	.size	.L.str3, 62

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"`%%' constraint used with last operand"
	.size	.L.str4, 39

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"matching constraint not valid in output operand"
	.size	.L.str5, 48

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"flags"
	.size	.L.str6, 6

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"fpsr"
	.size	.L.str7, 5

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"dirflag"
	.size	.L.str8, 8

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"unknown register name `%s' in `asm'"
	.size	.L.str9, 36

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"more than %d operands in `asm'"
	.size	.L.str10, 31

	.type	.L__FUNCTION__.expand_asm_operands,@object # @__FUNCTION__.expand_asm_operands
.L__FUNCTION__.expand_asm_operands:
	.asciz	"expand_asm_operands"
	.size	.L__FUNCTION__.expand_asm_operands, 20

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"output number %d not directly addressable"
	.size	.L.str11, 42

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"asm operand %d probably doesn't match constraints"
	.size	.L.str12, 50

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"%d"
	.size	.L.str13, 3

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"statement with no effect"
	.size	.L.str14, 25

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"value computed is not used"
	.size	.L.str15, 27

	.type	.L__FUNCTION__.expand_end_loop,@object # @__FUNCTION__.expand_end_loop
.L__FUNCTION__.expand_end_loop:
	.asciz	"expand_end_loop"
	.size	.L__FUNCTION__.expand_end_loop, 16

	.type	.L__FUNCTION__.expand_return,@object # @__FUNCTION__.expand_return
.L__FUNCTION__.expand_return:
	.asciz	"expand_return"
	.size	.L__FUNCTION__.expand_return, 14

	.type	.L__FUNCTION__.expand_start_bindings_and_block,@object # @__FUNCTION__.expand_start_bindings_and_block
.L__FUNCTION__.expand_start_bindings_and_block:
	.asciz	"expand_start_bindings_and_block"
	.size	.L__FUNCTION__.expand_start_bindings_and_block, 32

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"unused variable `%s'"
	.size	.L.str16, 21

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"label `%s' used before containing binding contour"
	.size	.L.str17, 50

	.type	.L__FUNCTION__.expand_decl,@object # @__FUNCTION__.expand_decl
.L__FUNCTION__.expand_decl:
	.asciz	"expand_decl"
	.size	.L__FUNCTION__.expand_decl, 12

	.type	.L__FUNCTION__.expand_anon_union_decl,@object # @__FUNCTION__.expand_anon_union_decl
.L__FUNCTION__.expand_anon_union_decl:
	.asciz	"expand_anon_union_decl"
	.size	.L__FUNCTION__.expand_anon_union_decl, 23

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"enumeration value `%s' not handled in switch"
	.size	.L.str18, 45

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"case value `%ld' not in enumerated type"
	.size	.L.str19, 40

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"case value `%ld' not in enumerated type `%s'"
	.size	.L.str20, 45

	.type	.L__FUNCTION__.expand_end_case_type,@object # @__FUNCTION__.expand_end_case_type
.L__FUNCTION__.expand_end_case_type:
	.asciz	"expand_end_case_type"
	.size	.L__FUNCTION__.expand_end_case_type, 21

	.type	use_cost_table,@object  # @use_cost_table
	.local	use_cost_table
	.comm	use_cost_table,4,4
	.type	cost_table_,@object     # @cost_table_
	.local	cost_table_
	.comm	cost_table_,258,16
	.type	cost_table_initialized,@object # @cost_table_initialized
	.local	cost_table_initialized
	.comm	cost_table_initialized,1,1
	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"unreachable code at beginning of %s"
	.size	.L.str21, 36

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"missing close brace for named operand"
	.size	.L.str22, 38

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"undefined named operand '%s'"
	.size	.L.str23, 29

	.type	.L__FUNCTION__.resolve_operand_name_1,@object # @__FUNCTION__.resolve_operand_name_1
.L__FUNCTION__.resolve_operand_name_1:
	.asciz	"resolve_operand_name_1"
	.size	.L__FUNCTION__.resolve_operand_name_1, 23

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"duplicate asm operand name '%s'"
	.size	.L.str24, 32

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"too many alternatives in `asm'"
	.size	.L.str25, 31

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"operand constraints for `asm' differ in number of alternatives"
	.size	.L.str26, 63

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"input operand constraint contains `%c'"
	.size	.L.str27, 39

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"matching constraint references invalid operand number"
	.size	.L.str28, 54

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"invalid punctuation `%c' in constraint"
	.size	.L.str29, 39

	.type	.L__FUNCTION__.expand_goto_internal,@object # @__FUNCTION__.expand_goto_internal
.L__FUNCTION__.expand_goto_internal:
	.asciz	"expand_goto_internal"
	.size	.L__FUNCTION__.expand_goto_internal, 21

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"jump to `%s' invalidly jumps into binding contour"
	.size	.L.str30, 50


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
