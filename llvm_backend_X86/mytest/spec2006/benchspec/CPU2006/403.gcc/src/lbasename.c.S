	.file	"lbasename.c.bc"
	.text
	.globl	lbasename
	.align	16, 0x90
	.type	lbasename,@function
lbasename:                              # @lbasename
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	.LBB0_1
.LBB0_4:                                #   in Loop: Header=BB0_1 Depth=1
	movq	%rax, %rdi
	.align	16, 0x90
.LBB0_1:                                # %for.cond.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_2 Depth 2
	leaq	1(%rdi), %rax
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_5:                                # %for.cond.for.inc_crit_edge
                                        #   in Loop: Header=BB0_2 Depth=2
	incq	%rax
.LBB0_2:                                # %for.cond
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	-1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB0_6
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB0_2 Depth=2
	cmpq	$47, %rcx
	jne	.LBB0_5
	jmp	.LBB0_4
.LBB0_6:                                # %for.end
	movq	%rdi, %rax
	retq
.Ltmp0:
	.size	lbasename, .Ltmp0-lbasename
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
