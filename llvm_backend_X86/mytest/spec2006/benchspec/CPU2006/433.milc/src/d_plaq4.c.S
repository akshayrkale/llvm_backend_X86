	.file	"d_plaq4.c.bc"
	.text
	.globl	d_plaquette
	.align	16, 0x90
	.type	d_plaquette,@function
d_plaquette:                            # @d_plaquette
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
	subq	$256, %rsp              # imm = 0x100
.Ltmp11:
	.cfi_def_cfa_offset 304
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
	movslq	sites_on_node(%rip), %rax
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	$0, 96(%rsp)
	movq	$0, 104(%rsp)
	movq	$144, %rsi
	movq	%rax, %rdi
	callq	calloc
	movq	%rax, 40(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	movq	$1, %r12
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$1, %rdi
	movq	$1, %r12
	callq	terminate
	movq	$1, 64(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB0_2:                                # %for.body9.lr.ph
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
                                        #       Child Loop BB0_5 Depth 3
                                        #       Child Loop BB0_9 Depth 3
                                        #       Child Loop BB0_8 Depth 3
	movq	%r12, 56(%rsp)          # 8-byte Spill
	leaq	(%r12,%r12,8), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$4, %rcx
	shlq	%cl, %rax
	addq	$112, %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r12, %rcx
	andq	%rax, %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	leaq	112(%rsp), %r15
	.align	16, 0x90
.LBB0_3:                                # %for.body9
                                        #   Parent Loop BB0_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB0_5 Depth 3
                                        #       Child Loop BB0_9 Depth 3
                                        #       Child Loop BB0_8 Depth 3
	movq	%rbx, 80(%rsp)          # 8-byte Spill
	leaq	(%rbx,%rbx,8), %r14
	movq	gen_pt(%rip), %r8
	movq	$4, %rcx
	movq	%r14, %rdi
	shlq	%cl, %rdi
	addq	$112, %rdi
	movq	$144, %rsi
	movq	$3, %rcx
	movq	%r12, %rdx
	callq	start_gather
	movq	gen_pt+8(%rip), %r8
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$144, %rsi
	movq	$3, %rcx
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	start_gather
	movslq	sites_on_node(%rip), %rcx
	movq	%rax, 88(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB0_6
# BB#4:                                 # %for.body27.lr.ph
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	lattice(%rip), %rax
	movq	$4, %rcx
	shlq	%cl, %r14
	leaq	112(%rax,%r14), %r12
	movq	$4, %rcx
	movq	32(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	leaq	112(%rax,%rdx), %rbx
	movq	$1, %r14
	movq	40(%rsp), %r13          # 8-byte Reload
	.align	16, 0x90
.LBB0_5:                                # %for.body27
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	mult_su3_an
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r14
	addq	$144, %r13
	addq	$1808, %r12             # imm = 0x710
	addq	$1808, %rbx             # imm = 0x710
	cmpq	%rax, %rdx
	jl	.LBB0_5
.LBB0_6:                                # %for.end
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	72(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	wait_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB0_10
# BB#7:                                 # %for.body39.lr.ph
                                        #   in Loop: Header=BB0_3 Depth=2
	cmpq	$3, 24(%rsp)            # 8-byte Folded Reload
	movq	$1, %rbx
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, %r13
	movq	%rax, %r12
	jne	.LBB0_9
	.align	16, 0x90
.LBB0_8:                                # %for.inc55.us
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rsi
	movq	%r13, %rdi
	movq	%r15, %rdx
	callq	mult_su3_nn
	movq	gen_pt+8(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	realtrace_su3
	movq	96(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, 96(%rsp)
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$144, %r13
	cmpq	%rax, %rdx
	jl	.LBB0_8
	jmp	.LBB0_10
	.align	16, 0x90
.LBB0_9:                                # %for.inc55
                                        #   Parent Loop BB0_2 Depth=1
                                        #     Parent Loop BB0_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	mult_su3_nn
	movq	gen_pt+8(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	realtrace_su3
	movq	104(%rsp), %rsi
	movq	%rax, %rdi
	callq	float64_add
	movq	%rax, 104(%rsp)
	movslq	sites_on_node(%rip), %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%rbx
	addq	$144, %r12
	cmpq	%rax, %rdx
	jl	.LBB0_9
.LBB0_10:                               # %for.end58
                                        #   in Loop: Header=BB0_3 Depth=2
	movq	%r14, %rdi
	callq	cleanup_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	80(%rsp), %rbx          # 8-byte Reload
	incq	%rbx
	movq	%rbx, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rdx, %rax
	movq	64(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB0_3
# BB#11:                                # %for.inc62
                                        #   in Loop: Header=BB0_2 Depth=1
	incq	%r12
	incq	64(%rsp)                # 8-byte Folded Spill
	cmpq	$4, %r12
	jne	.LBB0_2
# BB#12:                                # %for.end64
	leaq	104(%rsp), %rdi
	callq	g_doublesum
	leaq	96(%rsp), %rdi
	callq	g_doublesum
	movl	nx(%rip), %eax
	movl	ny(%rip), %ecx
	movl	nz(%rip), %edx
	movl	nt(%rip), %esi
	imulq	%rax, %rcx
	imulq	%rcx, %rdx
	movq	104(%rsp), %r14
	imulq	%rdx, %rsi
	leaq	(%rsi,%rsi,2), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	callq	int32_to_float64
	movq	%rax, %rbx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, (%rcx)
	movq	96(%rsp), %rdi
	movq	%rbx, %rsi
	callq	float64_div
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rax, (%rcx)
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	free
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	d_plaquette, .Ltmp17-d_plaquette
	.cfi_endproc

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"plaquette: can't malloc su3mat"
	.size	.Lstr, 31


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
