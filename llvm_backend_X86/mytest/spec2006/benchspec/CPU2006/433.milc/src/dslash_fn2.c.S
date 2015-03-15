	.file	"dslash_fn2.c.bc"
	.text
	.globl	cleanup_gathers
	.align	16, 0x90
	.type	cleanup_gathers,@function
cleanup_gathers:                        # @cleanup_gathers
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
	movq	%rdi, %rbx
	movq	(%rbx), %rdi
	movq	%rsi, %r14
	callq	cleanup_gather
	movq	56(%rbx), %rdi
	callq	cleanup_gather
	movq	(%r14), %rdi
	callq	cleanup_gather
	movq	56(%r14), %rdi
	callq	cleanup_gather
	movq	8(%rbx), %rdi
	callq	cleanup_gather
	movq	48(%rbx), %rdi
	callq	cleanup_gather
	movq	8(%r14), %rdi
	callq	cleanup_gather
	movq	48(%r14), %rdi
	callq	cleanup_gather
	movq	16(%rbx), %rdi
	callq	cleanup_gather
	movq	40(%rbx), %rdi
	callq	cleanup_gather
	movq	16(%r14), %rdi
	callq	cleanup_gather
	movq	40(%r14), %rdi
	callq	cleanup_gather
	movq	24(%rbx), %rdi
	callq	cleanup_gather
	movq	32(%rbx), %rdi
	callq	cleanup_gather
	movq	24(%r14), %rdi
	callq	cleanup_gather
	movq	32(%r14), %rdi
	callq	cleanup_gather
	movq	64(%rbx), %rdi
	callq	cleanup_gather
	movq	120(%rbx), %rdi
	callq	cleanup_gather
	movq	64(%r14), %rdi
	callq	cleanup_gather
	movq	120(%r14), %rdi
	callq	cleanup_gather
	movq	72(%rbx), %rdi
	callq	cleanup_gather
	movq	112(%rbx), %rdi
	callq	cleanup_gather
	movq	72(%r14), %rdi
	callq	cleanup_gather
	movq	112(%r14), %rdi
	callq	cleanup_gather
	movq	80(%rbx), %rdi
	callq	cleanup_gather
	movq	104(%rbx), %rdi
	callq	cleanup_gather
	movq	80(%r14), %rdi
	callq	cleanup_gather
	movq	104(%r14), %rdi
	callq	cleanup_gather
	movq	88(%rbx), %rdi
	callq	cleanup_gather
	movq	96(%rbx), %rdi
	callq	cleanup_gather
	movq	88(%r14), %rdi
	callq	cleanup_gather
	movq	96(%r14), %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cleanup_gather  # TAILCALL
.Ltmp8:
	.size	cleanup_gathers, .Ltmp8-cleanup_gathers
	.cfi_endproc

	.globl	cleanup_dslash_temps
	.align	16, 0x90
	.type	cleanup_dslash_temps,@function
cleanup_dslash_temps:                   # @cleanup_dslash_temps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp10:
	.cfi_def_cfa_offset 16
	movzbl	temp_not_allocated(%rip), %eax
	cmpq	$1, %rax
	jne	.LBB1_2
# BB#1:                                 # %for.cond.preheader
	movq	temp(%rip), %rdi
	callq	free
	movq	temp+8(%rip), %rdi
	callq	free
	movq	temp+16(%rip), %rdi
	callq	free
	movq	temp+24(%rip), %rdi
	callq	free
	movq	temp+32(%rip), %rdi
	callq	free
	movq	temp+40(%rip), %rdi
	callq	free
	movq	temp+48(%rip), %rdi
	callq	free
	movq	temp+56(%rip), %rdi
	callq	free
	movq	temp+64(%rip), %rdi
	callq	free
.LBB1_2:                                # %if.end
	xorq	%rax, %rax
	movb	%al, temp_not_allocated(%rip)
	popq	%rax
	retq
.Ltmp11:
	.size	cleanup_dslash_temps, .Ltmp11-cleanup_dslash_temps
	.cfi_endproc

	.globl	dslash_fn
	.align	16, 0x90
	.type	dslash_fn,@function
dslash_fn:                              # @dslash_fn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp18:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp19:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp20:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp21:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp22:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp23:
	.cfi_def_cfa_offset 192
.Ltmp24:
	.cfi_offset %rbx, -48
.Ltmp25:
	.cfi_offset %r12, -40
.Ltmp26:
	.cfi_offset %r13, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movl	valid_longlinks(%rip), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB2_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	callq	load_longlinks
.LBB2_2:                                # %if.end
	movq	%r15, 136(%rsp)         # 8-byte Spill
	movl	valid_fatlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB2_4
# BB#3:                                 # %if.then2
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB2_4:                                # %if.end3
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jne	.LBB2_5
# BB#7:                                 # %sw.bb5
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$3, %r13
	jmp	.LBB2_8
.LBB2_5:                                # %if.end3
	movq	$1, %r13
	cmpq	$1, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jne	.LBB2_8
# BB#6:                                 # %sw.bb4
	movq	$2, %r13
.LBB2_8:                                # %sw.epilog
	movq	gen_pt(%rip), %r8
	movq	$48, %rsi
	xorq	%rdx, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+64(%rip), %r8
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$8, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+8(%rip), %r8
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$1, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+72(%rip), %r8
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$9, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+16(%rip), %r8
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$2, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+80(%rip), %r8
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$10, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+24(%rip), %r8
	movq	%rax, 96(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$3, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+88(%rip), %r8
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$11, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rcx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	callq	start_gather
	movl	even_sites_on_node(%rip), %r12d
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	%r15, %rsi
	andq	%rsi, %r13
	cmpq	$2, %r13
	movq	%r12, %rdi
	je	.LBB2_10
# BB#9:                                 # %sw.epilog
	movl	sites_on_node(%rip), %edi
.LBB2_10:                               # %sw.epilog
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %r13
	je	.LBB2_12
# BB#11:                                # %sw.epilog
	xorq	%r12, %r12
.LBB2_12:                               # %sw.epilog
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB2_15
# BB#13:                                # %for.body25.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$3200, %rdx, %rcx       # imm = 0xC80
	leaq	2480(%rax,%rcx), %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	%rsi, %rdi
	movq	%rdi, 128(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB2_14:                               # %for.body25
                                        # =>This Inner Loop Header: Depth=1
	leaq	-2480(%rbx,%r14), %r13
	leaq	-1216(%rbx), %rdi
	movq	%r14, %r15
	movq	%rsi, %r14
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	mult_adj_su3_mat_vec_4dir
	leaq	192(%rbx), %rdx
	leaq	-1792(%rbx), %rdi
	movq	%r13, %rsi
	callq	mult_adj_su3_mat_vec_4dir
	movq	128(%rsp), %rcx         # 8-byte Reload
	movq	%r14, %rsi
	movq	%r15, %r14
	incq	%r12
	movq	%r12, %rax
	andq	%rsi, %rax
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rcx, %rax
	jne	.LBB2_14
.LBB2_15:                               # %for.cond34.preheader
	movq	gen_pt+56(%rip), %r8
	movq	$2480, %rdi             # imm = 0x9B0
	movq	$48, %rsi
	movq	$7, %rdx
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+48(%rip), %r8
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$2528, %rdi             # imm = 0x9E0
	movq	$48, %rsi
	movq	$6, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+40(%rip), %r8
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$2576, %rdi             # imm = 0xA10
	movq	$48, %rsi
	movq	$5, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+32(%rip), %r8
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	$2624, %rdi             # imm = 0xA40
	movq	$48, %rsi
	movq	$4, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+120(%rip), %r8
	movq	%rax, %r13
	movq	%r13, 24(%rsp)          # 8-byte Spill
	movq	$2672, %rdi             # imm = 0xA70
	movq	$48, %rsi
	movq	$15, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+112(%rip), %r8
	movq	%rax, %r14
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	$2720, %rdi             # imm = 0xAA0
	movq	$48, %rsi
	movq	$14, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+104(%rip), %r8
	movq	%rax, %r15
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	$2768, %rdi             # imm = 0xAD0
	movq	$48, %rsi
	movq	$13, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	gen_pt+96(%rip), %r8
	movq	%rax, %r12
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	$2816, %rdi             # imm = 0xB00
	movq	$48, %rsi
	movq	$12, %rdx
	movq	%rbx, %rcx
	callq	start_gather
	movq	%rax, %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	%r13, %rdi
	callq	wait_gather
	movq	%r14, %rdi
	callq	wait_gather
	movq	%r15, %rdi
	callq	wait_gather
	movq	%r12, %rdi
	callq	wait_gather
	movq	%rbx, %rdi
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r15d
	movq	40(%rsp), %rdx          # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r15, %rax
	je	.LBB2_17
# BB#16:                                # %for.cond34.preheader
	movl	sites_on_node(%rip), %eax
.LBB2_17:                               # %for.cond34.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	je	.LBB2_19
# BB#18:                                # %for.cond34.preheader
	xorq	%r15, %r15
.LBB2_19:                               # %for.cond34.preheader
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	136(%rsp), %rdx         # 8-byte Reload
	movq	%rsi, %r14
	jge	.LBB2_22
# BB#20:                                # %for.body127.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	movq	$3, %rcx
	movq	%r15, %r12
	shlq	%cl, %r12
	imulq	$3200, %r15, %rcx       # imm = 0xC80
	leaq	2864(%rax,%rcx), %rbx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 136(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB2_21:                               # %for.body127
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rdx
	movq	gen_pt+8(%rip), %rcx
	movq	gen_pt+16(%rip), %rdi
	movq	gen_pt+24(%rip), %rax
	movq	(%rdx,%r12), %rsi
	movq	(%rcx,%r12), %rdx
	movq	(%rdi,%r12), %rcx
	movq	(%rax,%r12), %r8
	movq	136(%rsp), %rax         # 8-byte Reload
	leaq	-2864(%rbx,%rax), %r13
	leaq	-1600(%rbx), %rdi
	movq	%r13, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+64(%rip), %rdx
	movq	gen_pt+72(%rip), %rcx
	movq	gen_pt+80(%rip), %rdi
	movq	gen_pt+88(%rip), %rax
	movq	(%rdx,%r12), %rsi
	movq	(%rcx,%r12), %rdx
	movq	(%rdi,%r12), %rcx
	movq	(%rax,%r12), %r8
	leaq	-2176(%rbx), %rdi
	movq	%rbx, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+56(%rip), %rdx
	movq	gen_pt+48(%rip), %rcx
	movq	gen_pt+40(%rip), %rdi
	movq	gen_pt+32(%rip), %rax
	movq	(%rdx,%r12), %rsi
	movq	(%rcx,%r12), %rdx
	movq	(%rdi,%r12), %rcx
	movq	(%rax,%r12), %r8
	movq	%r13, %rdi
	callq	sub_four_su3_vecs
	movq	gen_pt+120(%rip), %rdx
	movq	gen_pt+112(%rip), %rcx
	movq	gen_pt+104(%rip), %rdi
	movq	gen_pt+96(%rip), %rax
	movq	(%rdx,%r12), %rsi
	movq	(%rcx,%r12), %rdx
	movq	(%rdi,%r12), %rcx
	movq	(%rax,%r12), %r8
	movq	%rbx, %rdi
	callq	sub_four_su3_vecs
	movq	%r13, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	add_su3_vector
	incq	%r15
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %r12
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB2_21
.LBB2_22:                               # %for.cond178.preheader
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	cleanup_gather
	movq	96(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	cleanup_gather
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	32(%rsp), %rdi          # 8-byte Reload
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cleanup_gather  # TAILCALL
.Ltmp29:
	.size	dslash_fn, .Ltmp29-dslash_fn
	.cfi_endproc

	.globl	dslash_fn_special
	.align	16, 0x90
	.type	dslash_fn_special,@function
dslash_fn_special:                      # @dslash_fn_special
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp38:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp41:
	.cfi_def_cfa_offset 96
.Ltmp42:
	.cfi_offset %rbx, -48
.Ltmp43:
	.cfi_offset %r12, -40
.Ltmp44:
	.cfi_offset %r13, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movl	valid_longlinks(%rip), %eax
	movq	%r8, %r13
	movq	%rcx, %r15
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %rbx
	testq	%rax, %rax
	jne	.LBB3_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	callq	load_longlinks
.LBB3_2:                                # %if.end
	movq	%r14, 40(%rsp)          # 8-byte Spill
	movl	valid_fatlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB3_4
# BB#3:                                 # %if.then2
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB3_4:                                # %if.end3
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r12, %rax
	andq	%rcx, %rax
	cmpq	$3, %rax
	jne	.LBB3_5
# BB#7:                                 # %sw.bb5
	movq	$3, %r14
	jmp	.LBB3_8
.LBB3_5:                                # %if.end3
	movq	$1, %r14
	cmpq	$1, %rax
	jne	.LBB3_8
# BB#6:                                 # %sw.bb4
	movq	$2, %r14
.LBB3_8:                                # %sw.epilog
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	gen_pt(%rip), %r8
	andq	%rcx, %r13
	cmpq	$1, %r13
	jne	.LBB3_9
# BB#31:                                # %for.inc31.us.3
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	$48, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, (%r15)
	movq	gen_pt+8(%rip), %r8
	movq	$48, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 8(%r15)
	movq	gen_pt+16(%rip), %r8
	movq	$48, %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 16(%r15)
	movq	gen_pt+24(%rip), %r8
	movq	$48, %rsi
	movq	$3, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 24(%r15)
	movq	gen_pt+64(%rip), %r8
	movq	$48, %rsi
	movq	$8, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 64(%r15)
	movq	gen_pt+72(%rip), %r8
	movq	$48, %rsi
	movq	$9, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 72(%r15)
	movq	gen_pt+80(%rip), %r8
	movq	$48, %rsi
	movq	$10, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 80(%r15)
	movq	gen_pt+88(%rip), %r8
	movq	$48, %rsi
	movq	$11, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	start_gather
	movq	%rax, 88(%r15)
	movq	%r15, 24(%rsp)          # 8-byte Spill
	jmp	.LBB3_10
.LBB3_9:                                # %for.inc31.3
	movq	%r13, (%rsp)            # 8-byte Spill
	movq	(%r15), %r9
	movq	$48, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+8(%rip), %r8
	movq	8(%r15), %r9
	movq	$48, %rsi
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+16(%rip), %r8
	movq	16(%r15), %r9
	movq	$48, %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+24(%rip), %r8
	movq	24(%r15), %r9
	movq	$48, %rsi
	movq	$3, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+64(%rip), %r8
	movq	64(%r15), %r9
	movq	$48, %rsi
	movq	$8, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+72(%rip), %r8
	movq	72(%r15), %r9
	movq	$48, %rsi
	movq	$9, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+80(%rip), %r8
	movq	80(%r15), %r9
	movq	$48, %rsi
	movq	$10, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
	movq	gen_pt+88(%rip), %r8
	movq	88(%r15), %r9
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$11, %rdx
	movq	%rbx, %rdi
	movq	%r12, %rcx
	callq	restart_gather
.LBB3_10:                               # %for.end33
	movl	even_sites_on_node(%rip), %r15d
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %r14
	cmpq	$2, %r14
	movq	%r15, %rdi
	je	.LBB3_12
# BB#11:                                # %for.end33
	movl	sites_on_node(%rip), %edi
.LBB3_12:                               # %for.end33
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %r14
	je	.LBB3_14
# BB#13:                                # %for.end33
	xorq	%r15, %r15
.LBB3_14:                               # %for.end33
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB3_17
# BB#15:                                # %for.body44.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	imulq	$3200, %rdx, %rcx       # imm = 0xC80
	leaq	2480(%rax,%rcx), %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	%rsi, %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_16:                               # %for.body44
                                        # =>This Inner Loop Header: Depth=1
	leaq	-2480(%r12,%rbx), %r14
	leaq	-1216(%r12), %rdi
	movq	%rbx, %r13
	movq	%rsi, %rbx
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	mult_adj_su3_mat_vec_4dir
	leaq	192(%r12), %rdx
	leaq	-1792(%r12), %rdi
	movq	%r14, %rsi
	callq	mult_adj_su3_mat_vec_4dir
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%r13, %rbx
	incq	%r15
	movq	%r15, %rax
	andq	%rsi, %rax
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rcx, %rax
	jne	.LBB3_16
.LBB3_17:                               # %for.cond53.preheader
	movq	gen_pt+56(%rip), %r8
	movq	(%rsp), %r15            # 8-byte Reload
	cmpq	$1, %r15
	jne	.LBB3_18
# BB#28:                                # %for.inc88.us.3
	movq	$2480, %rdi             # imm = 0x9B0
	movq	$48, %rsi
	movq	$7, %rdx
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r14, %rcx
	callq	start_gather
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rax, 56(%rbx)
	movq	gen_pt+48(%rip), %r8
	movq	$2528, %rdi             # imm = 0x9E0
	movq	$48, %rsi
	movq	$6, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 48(%rbx)
	movq	gen_pt+40(%rip), %r8
	movq	$2576, %rdi             # imm = 0xA10
	movq	$48, %rsi
	movq	$5, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 40(%rbx)
	movq	gen_pt+32(%rip), %r8
	movq	$2624, %rdi             # imm = 0xA40
	movq	$48, %rsi
	movq	$4, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 32(%rbx)
	jmp	.LBB3_29
.LBB3_18:                               # %for.inc88.3
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %r9
	movq	$2480, %rdi             # imm = 0x9B0
	movq	$48, %rsi
	movq	$7, %rdx
	movq	8(%rsp), %r14           # 8-byte Reload
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+48(%rip), %r8
	movq	48(%rbx), %r9
	movq	$2528, %rdi             # imm = 0x9E0
	movq	$48, %rsi
	movq	$6, %rdx
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+40(%rip), %r8
	movq	40(%rbx), %r9
	movq	$2576, %rdi             # imm = 0xA10
	movq	$48, %rsi
	movq	$5, %rdx
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+32(%rip), %r8
	movq	32(%rbx), %r9
	movq	$2624, %rdi             # imm = 0xA40
	movq	$48, %rsi
	movq	$4, %rdx
	movq	%r14, %rcx
	callq	restart_gather
.LBB3_29:                               # %for.cond91.preheader
	movq	gen_pt+120(%rip), %r8
	cmpq	$1, %r15
	jne	.LBB3_30
# BB#19:                                # %for.inc135.us.3
	movq	$2672, %rdi             # imm = 0xA70
	movq	$48, %rsi
	movq	$15, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 120(%rbx)
	movq	gen_pt+112(%rip), %r8
	movq	$2720, %rdi             # imm = 0xAA0
	movq	$48, %rsi
	movq	$14, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 112(%rbx)
	movq	gen_pt+104(%rip), %r8
	movq	$2768, %rdi             # imm = 0xAD0
	movq	$48, %rsi
	movq	$13, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 104(%rbx)
	movq	gen_pt+96(%rip), %r8
	movq	$2816, %rdi             # imm = 0xB00
	movq	$48, %rsi
	movq	$12, %rdx
	movq	%r14, %rcx
	callq	start_gather
	movq	%rax, 96(%rbx)
	jmp	.LBB3_20
.LBB3_30:                               # %for.inc135.3
	movq	120(%rbx), %r9
	movq	$2672, %rdi             # imm = 0xA70
	movq	$48, %rsi
	movq	$15, %rdx
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+112(%rip), %r8
	movq	112(%rbx), %r9
	movq	$2720, %rdi             # imm = 0xAA0
	movq	$48, %rsi
	movq	$14, %rdx
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+104(%rip), %r8
	movq	104(%rbx), %r9
	movq	$2768, %rdi             # imm = 0xAD0
	movq	$48, %rsi
	movq	$13, %rdx
	movq	%r14, %rcx
	callq	restart_gather
	movq	gen_pt+96(%rip), %r8
	movq	96(%rbx), %r9
	movq	$2816, %rdi             # imm = 0xB00
	movq	$48, %rsi
	movq	$12, %rdx
	movq	%r14, %rcx
	callq	restart_gather
.LBB3_20:                               # %for.cond138.preheader
	movq	(%rbx), %rdi
	callq	wait_gather
	movq	8(%rbx), %rdi
	callq	wait_gather
	movq	16(%rbx), %rdi
	callq	wait_gather
	movq	24(%rbx), %rdi
	callq	wait_gather
	movq	64(%rbx), %rdi
	callq	wait_gather
	movq	72(%rbx), %rdi
	callq	wait_gather
	movq	80(%rbx), %rdi
	callq	wait_gather
	movq	88(%rbx), %rdi
	callq	wait_gather
	movq	56(%rbx), %rdi
	callq	wait_gather
	movq	48(%rbx), %rdi
	callq	wait_gather
	movq	40(%rbx), %rdi
	callq	wait_gather
	movq	32(%rbx), %rdi
	callq	wait_gather
	movq	120(%rbx), %rdi
	callq	wait_gather
	movq	112(%rbx), %rdi
	callq	wait_gather
	movq	104(%rbx), %rdi
	callq	wait_gather
	movq	96(%rbx), %rdi
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r14d
	movq	16(%rsp), %rdx          # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r14, %rax
	je	.LBB3_22
# BB#21:                                # %for.cond138.preheader
	movl	sites_on_node(%rip), %eax
.LBB3_22:                               # %for.cond138.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	je	.LBB3_24
# BB#23:                                # %for.cond138.preheader
	xorq	%r14, %r14
.LBB3_24:                               # %for.cond138.preheader
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	%rsi, %r12
	jge	.LBB3_27
# BB#25:                                # %for.body194.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	imulq	$3200, %r14, %rcx       # imm = 0xC80
	leaq	2864(%rax,%rcx), %r13
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rdx, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_26:                               # %for.body194
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rdx
	movq	gen_pt+8(%rip), %rcx
	movq	gen_pt+16(%rip), %rdi
	movq	gen_pt+24(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	40(%rsp), %rax          # 8-byte Reload
	leaq	-2864(%r13,%rax), %r15
	leaq	-1600(%r13), %rdi
	movq	%r15, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+64(%rip), %rdx
	movq	gen_pt+72(%rip), %rcx
	movq	gen_pt+80(%rip), %rdi
	movq	gen_pt+88(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	leaq	-2176(%r13), %rdi
	movq	%r13, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+56(%rip), %rdx
	movq	gen_pt+48(%rip), %rcx
	movq	gen_pt+40(%rip), %rdi
	movq	gen_pt+32(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r15, %rdi
	callq	sub_four_su3_vecs
	movq	gen_pt+120(%rip), %rdx
	movq	gen_pt+112(%rip), %rcx
	movq	gen_pt+104(%rip), %rdi
	movq	gen_pt+96(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r13, %rdi
	callq	sub_four_su3_vecs
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	add_su3_vector
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %rbx
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB3_26
.LBB3_27:                               # %for.end244
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp47:
	.size	dslash_fn_special, .Ltmp47-dslash_fn_special
	.cfi_endproc

	.globl	dslash_fn_on_temp
	.align	16, 0x90
	.type	dslash_fn_on_temp,@function
dslash_fn_on_temp:                      # @dslash_fn_on_temp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp56:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp57:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp58:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp59:
	.cfi_def_cfa_offset 288
.Ltmp60:
	.cfi_offset %rbx, -48
.Ltmp61:
	.cfi_offset %r12, -40
.Ltmp62:
	.cfi_offset %r13, -32
.Ltmp63:
	.cfi_offset %r14, -24
.Ltmp64:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %rbx
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r13
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 160(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 152(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 144(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 136(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 128(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 112(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, 104(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movl	valid_longlinks(%rip), %ecx
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jne	.LBB4_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	callq	load_longlinks
.LBB4_2:                                # %if.end
	movl	valid_fatlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB4_4
# BB#3:                                 # %if.then8
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB4_4:                                # %if.end9
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%r12, %rax
	andq	%r15, %rax
	cmpq	$3, %rax
	jne	.LBB4_5
# BB#7:                                 # %sw.bb11
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$3, %rbx
	jmp	.LBB4_8
.LBB4_5:                                # %if.end9
	movq	$1, %rbx
	cmpq	$1, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jne	.LBB4_8
# BB#6:                                 # %sw.bb10
	movq	$2, %rbx
.LBB4_8:                                # %sw.epilog
	movq	gen_pt(%rip), %r8
	movq	$48, %rsi
	xorq	%rdx, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+64(%rip), %r8
	movq	%rax, 72(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$8, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+8(%rip), %r8
	movq	%rax, 88(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+72(%rip), %r8
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$9, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+16(%rip), %r8
	movq	%rax, 80(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+80(%rip), %r8
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$10, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+24(%rip), %r8
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$3, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+88(%rip), %r8
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$48, %rsi
	movq	$11, %rdx
	movq	%r13, %rdi
	movq	%r12, %rcx
	callq	start_gather_from_temp
	movl	even_sites_on_node(%rip), %edx
	movq	%rax, 48(%rsp)          # 8-byte Spill
	andq	%r15, %rbx
	cmpq	$2, %rbx
	movq	%rdx, %rax
	je	.LBB4_10
# BB#9:                                 # %sw.epilog
	movl	sites_on_node(%rip), %eax
.LBB4_10:                               # %sw.epilog
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	movq	%rsi, %rdi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rbx
	je	.LBB4_12
# BB#11:                                # %sw.epilog
	xorq	%rdx, %rdx
.LBB4_12:                               # %sw.epilog
	movq	$32, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	movq	%r13, 232(%rsp)         # 8-byte Spill
	jge	.LBB4_15
# BB#13:                                # %for.body44.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$4, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %rcx
	movq	104(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 224(%rsp)         # 8-byte Spill
	movq	120(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 216(%rsp)         # 8-byte Spill
	movq	136(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 208(%rsp)         # 8-byte Spill
	movq	152(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 200(%rsp)         # 8-byte Spill
	movq	112(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 192(%rsp)         # 8-byte Spill
	movq	128(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 184(%rsp)         # 8-byte Spill
	movq	144(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rsi         # 8-byte Reload
	leaq	(%rsi,%rcx), %rsi
	movq	%rsi, 168(%rsp)         # 8-byte Spill
	addq	%rcx, 232(%rsp)         # 8-byte Folded Spill
	imulq	$3200, %rdx, %rcx       # imm = 0xC80
	leaq	1(%rdx), %r15
	leaq	688(%rax,%rcx), %rbx
	xorq	%r13, %r13
	.align	16, 0x90
.LBB4_14:                               # %for.body44
                                        # =>This Inner Loop Header: Depth=1
	movq	192(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %r9
	movq	184(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %r8
	movq	176(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %rcx
	movq	168(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %rdx
	movq	232(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %r12
	movq	%rdi, %r14
	leaq	576(%rbx), %rdi
	movq	%r12, %rsi
	callq	mult_adj_su3_mat_4vec
	movq	224(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %r9
	movq	216(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %r8
	movq	208(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %rcx
	movq	200(%rsp), %rax         # 8-byte Reload
	leaq	(%rax,%r13), %rdx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	mult_adj_su3_mat_4vec
	movq	%r14, %rdi
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	addq	$48, %r13
	addq	$3200, %rbx             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB4_14
.LBB4_15:                               # %for.cond77.preheader
	movq	gen_pt+56(%rip), %r8
	movq	$48, %rsi
	movq	$7, %rdx
	movq	160(%rsp), %rdi         # 8-byte Reload
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+48(%rip), %r8
	movq	%rax, 224(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$6, %rdx
	movq	144(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+40(%rip), %r8
	movq	%rax, 216(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$5, %rdx
	movq	128(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+32(%rip), %r8
	movq	%rax, 208(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$4, %rdx
	movq	112(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+120(%rip), %r8
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$15, %rdx
	movq	152(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+112(%rip), %r8
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$14, %rdx
	movq	136(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+104(%rip), %r8
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$13, %rdx
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	gen_pt+96(%rip), %r8
	movq	%rax, 176(%rsp)         # 8-byte Spill
	movq	$48, %rsi
	movq	$12, %rdx
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rcx
	callq	start_gather_from_temp
	movq	%rax, 168(%rsp)         # 8-byte Spill
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r13d
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r13, %rax
	je	.LBB4_17
# BB#16:                                # %for.cond77.preheader
	movl	sites_on_node(%rip), %eax
.LBB4_17:                               # %for.cond77.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	je	.LBB4_19
# BB#18:                                # %for.cond77.preheader
	xorq	%r13, %r13
.LBB4_19:                               # %for.cond77.preheader
	movq	$32, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%rsi, %rax
	movq	%rax, 232(%rsp)         # 8-byte Spill
	jge	.LBB4_22
# BB#20:                                # %for.body141.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$4, %rcx
	movq	%r13, %rdx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rbx
	movq	96(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rbx), %r12
	movq	$3, %rcx
	movq	%r13, %r15
	shlq	%cl, %r15
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rbx
	imulq	$3200, %r13, %rcx       # imm = 0xC80
	leaq	688(%rax,%rcx), %r14
	.align	16, 0x90
.LBB4_21:                               # %for.body141
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rdx
	movq	gen_pt+8(%rip), %rcx
	movq	gen_pt+16(%rip), %rdi
	movq	gen_pt+24(%rip), %rax
	movq	(%rdx,%r15), %rsi
	movq	(%rcx,%r15), %rdx
	movq	(%rdi,%r15), %rcx
	movq	(%rax,%r15), %r8
	leaq	576(%r14), %rdi
	movq	%rbx, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+64(%rip), %rdx
	movq	gen_pt+72(%rip), %rcx
	movq	gen_pt+80(%rip), %rdi
	movq	gen_pt+88(%rip), %rax
	movq	(%rdx,%r15), %rsi
	movq	(%rcx,%r15), %rdx
	movq	(%rdi,%r15), %rcx
	movq	(%rax,%r15), %r8
	movq	%r14, %rdi
	movq	%r12, %r9
	callq	mult_su3_mat_vec_sum_4dir
	movq	232(%rsp), %rdx         # 8-byte Reload
	incq	%r13
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$48, %r12
	addq	$8, %r15
	addq	$48, %rbx
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB4_21
.LBB4_22:                               # %for.cond170.preheader
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	200(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	192(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	176(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r14d
	movq	24(%rsp), %rdx          # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r14, %r15
	je	.LBB4_24
# BB#23:                                # %for.cond170.preheader
	movl	sites_on_node(%rip), %r15d
.LBB4_24:                               # %for.cond170.preheader
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	movq	16(%rsp), %r13          # 8-byte Reload
	je	.LBB4_26
# BB#25:                                # %for.cond170.preheader
	xorq	%r14, %r14
.LBB4_26:                               # %for.cond170.preheader
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jge	.LBB4_29
# BB#27:                                # %for.body208.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	movq	96(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rax), %r12
	addq	%rax, %r13
	.align	16, 0x90
.LBB4_28:                               # %for.body208
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt+56(%rip), %rdx
	movq	gen_pt+48(%rip), %rcx
	movq	gen_pt+40(%rip), %rdi
	movq	gen_pt+32(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r13, %rdi
	callq	sub_four_su3_vecs
	movq	gen_pt+120(%rip), %rdx
	movq	gen_pt+112(%rip), %rcx
	movq	gen_pt+104(%rip), %rdi
	movq	gen_pt+96(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r12, %rdi
	callq	sub_four_su3_vecs
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	add_su3_vector
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$8, %rbx
	addq	$48, %r12
	addq	$48, %r13
	cmpq	%rdx, %rax
	jl	.LBB4_28
.LBB4_29:                               # %for.cond239.preheader
	movq	72(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	224(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	56(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	216(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	208(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	32(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	200(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	88(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	192(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	80(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	184(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	176(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	48(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	cleanup_gather
	movq	160(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	152(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	144(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	136(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	128(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	112(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	104(%rsp), %rdi         # 8-byte Reload
	callq	free
	movq	96(%rsp), %rdi          # 8-byte Reload
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp65:
	.size	dslash_fn_on_temp, .Ltmp65-dslash_fn_on_temp
	.cfi_endproc

	.globl	dslash_fn_on_temp_special
	.align	16, 0x90
	.type	dslash_fn_on_temp_special,@function
dslash_fn_on_temp_special:              # @dslash_fn_on_temp_special
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp72:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp73:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp74:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp75:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp76:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp77:
	.cfi_def_cfa_offset 96
.Ltmp78:
	.cfi_offset %rbx, -48
.Ltmp79:
	.cfi_offset %r12, -40
.Ltmp80:
	.cfi_offset %r13, -32
.Ltmp81:
	.cfi_offset %r14, -24
.Ltmp82:
	.cfi_offset %r15, -16
	movzbl	temp_not_allocated(%rip), %eax
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rcx, %r15
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	movq	%rsi, %r14
	movq	%rdi, %r12
	testq	%rax, %rax
	jne	.LBB5_2
# BB#1:                                 # %for.cond.preheader
	movslq	sites_on_node(%rip), %rbx
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+32(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+8(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+40(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+16(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+48(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+24(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+56(%rip)
	movq	$48, %rsi
	movq	%rbx, %rdi
	callq	calloc
	movq	%rax, temp+64(%rip)
	movq	$1, %rax
	movb	%al, temp_not_allocated(%rip)
.LBB5_2:                                # %if.end
	movq	%r14, (%rsp)            # 8-byte Spill
	movl	valid_longlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB5_4
# BB#3:                                 # %if.then8
	xorq	%rax, %rax
	callq	load_longlinks
.LBB5_4:                                # %if.end9
	movl	valid_fatlinks(%rip), %eax
	testq	%rax, %rax
	jne	.LBB5_6
# BB#5:                                 # %if.then11
	xorq	%rax, %rax
	callq	load_fatlinks
.LBB5_6:                                # %if.end12
	movq	%r15, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rcx          # 8-byte Reload
	andq	%rdx, %rcx
	cmpq	$3, %rcx
	jne	.LBB5_7
# BB#9:                                 # %sw.bb14
	movq	$3, %r14
	movq	%rax, %r13
	jmp	.LBB5_10
.LBB5_7:                                # %if.end12
	movq	$1, %r14
	cmpq	$1, %rcx
	movq	%rax, %r13
	jne	.LBB5_10
# BB#8:                                 # %sw.bb13
	movq	$2, %r14
.LBB5_10:                               # %sw.epilog
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	$8, %rbx
	andq	%rdx, 40(%rsp)          # 8-byte Folded Spill
	.align	16, 0x90
.LBB5_11:                               # %for.body18
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt-64(,%rbx,8), %r8
	leaq	-8(%rbx), %rdx
	cmpq	$1, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB5_13
# BB#12:                                # %if.then21
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	$48, %rsi
	movq	%r12, %rdi
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	start_gather_from_temp
	movq	%rax, -64(%r13,%rbx,8)
	movq	gen_pt(,%rbx,8), %r8
	movq	$48, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	start_gather_from_temp
	movq	%rax, (%r13,%rbx,8)
	jmp	.LBB5_14
	.align	16, 0x90
.LBB5_13:                               # %if.else
                                        #   in Loop: Header=BB5_11 Depth=1
	movq	-64(%r13,%rbx,8), %r9
	movq	$48, %rsi
	movq	%r12, %rdi
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rcx
	callq	restart_gather_from_temp
	movq	gen_pt(,%rbx,8), %r8
	movq	(%r13,%rbx,8), %r9
	movq	$48, %rsi
	movq	%r12, %rdi
	movq	%rbx, %rdx
	movq	%r15, %rcx
	callq	restart_gather_from_temp
.LBB5_14:                               # %for.inc47
                                        #   in Loop: Header=BB5_11 Depth=1
	incq	%rbx
	cmpq	$12, %rbx
	jne	.LBB5_11
# BB#15:                                # %for.end49
	movq	%r13, 16(%rsp)          # 8-byte Spill
	movl	even_sites_on_node(%rip), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r14
	cmpq	$2, %r14
	movq	%rax, %rdx
	je	.LBB5_17
# BB#16:                                # %for.end49
	movl	sites_on_node(%rip), %edx
.LBB5_17:                               # %for.end49
	movq	$32, %rcx
	movq	%rdx, %rsi
                                        # kill: RDX<def> RSI<kill>
	movq	%rsi, %rdi
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$1, %r14
	je	.LBB5_19
# BB#18:                                # %for.end49
	xorq	%rax, %rax
.LBB5_19:                               # %for.end49
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rdx, %rsi
	movq	%rdi, %r10
	movq	%r12, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	jge	.LBB5_22
# BB#20:                                # %for.body63.lr.ph
	movq	lattice(%rip), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$4, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	leaq	(%rsi,%rsi,2), %r15
	imulq	$3200, %rax, %rcx       # imm = 0xC80
	leaq	1(%rax), %r13
	leaq	688(%rdx,%rcx), %r14
	.align	16, 0x90
.LBB5_21:                               # %for.body63
                                        # =>This Inner Loop Header: Depth=1
	movq	temp(%rip), %rdx
	movq	temp+8(%rip), %rcx
	movq	temp+16(%rip), %r8
	movq	temp+24(%rip), %r9
	addq	%r15, %r9
	addq	%r15, %r8
	addq	%r15, %rcx
	addq	%r15, %rdx
	leaq	(%rdi,%r15), %rbx
	movq	%r10, %r12
	leaq	576(%r14), %rdi
	movq	%rbx, %rsi
	callq	mult_adj_su3_mat_4vec
	movq	temp+32(%rip), %rdx
	movq	temp+40(%rip), %rcx
	movq	temp+48(%rip), %r8
	movq	temp+56(%rip), %r9
	addq	%r15, %r9
	addq	%r15, %r8
	addq	%r15, %rcx
	addq	%r15, %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	mult_adj_su3_mat_4vec
	movq	%r12, %r10
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r13
	addq	$48, %r15
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB5_21
.LBB5_22:                               # %for.cond88.preheader
	movq	temp(%rip), %rdi
	movq	gen_pt+56(%rip), %r8
	cmpq	$1, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB5_23
# BB#40:                                # %for.inc115.us.3
	movq	$48, %rsi
	movq	$7, %rdx
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	%rax, 56(%rbx)
	movq	temp+8(%rip), %rdi
	movq	gen_pt+48(%rip), %r8
	movq	$48, %rsi
	movq	$6, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 48(%rbx)
	movq	temp+16(%rip), %rdi
	movq	gen_pt+40(%rip), %r8
	movq	$48, %rsi
	movq	$5, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 40(%rbx)
	movq	temp+24(%rip), %rdi
	movq	gen_pt+32(%rip), %r8
	movq	$48, %rsi
	movq	$4, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 32(%rbx)
	jmp	.LBB5_41
.LBB5_23:                               # %for.inc115.3
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %r9
	movq	$48, %rsi
	movq	$7, %rdx
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+8(%rip), %rdi
	movq	gen_pt+48(%rip), %r8
	movq	48(%rbx), %r9
	movq	$48, %rsi
	movq	$6, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+16(%rip), %rdi
	movq	gen_pt+40(%rip), %r8
	movq	40(%rbx), %r9
	movq	$48, %rsi
	movq	$5, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+24(%rip), %rdi
	movq	gen_pt+32(%rip), %r8
	movq	32(%rbx), %r9
	movq	$48, %rsi
	movq	$4, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
.LBB5_41:                               # %for.cond118.preheader
	movq	temp+32(%rip), %rdi
	movq	gen_pt+120(%rip), %r8
	cmpq	$1, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB5_24
# BB#42:                                # %for.inc150.us.3
	movq	$48, %rsi
	movq	$15, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 120(%rbx)
	movq	temp+40(%rip), %rdi
	movq	gen_pt+112(%rip), %r8
	movq	$48, %rsi
	movq	$14, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 112(%rbx)
	movq	temp+48(%rip), %rdi
	movq	gen_pt+104(%rip), %r8
	movq	$48, %rsi
	movq	$13, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 104(%rbx)
	movq	temp+56(%rip), %rdi
	movq	gen_pt+96(%rip), %r8
	movq	$48, %rsi
	movq	$12, %rdx
	movq	%r14, %rcx
	callq	start_gather_from_temp
	movq	%rax, 96(%rbx)
	jmp	.LBB5_25
.LBB5_24:                               # %for.inc150.3
	movq	120(%rbx), %r9
	movq	$48, %rsi
	movq	$15, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+40(%rip), %rdi
	movq	gen_pt+112(%rip), %r8
	movq	112(%rbx), %r9
	movq	$48, %rsi
	movq	$14, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+48(%rip), %rdi
	movq	gen_pt+104(%rip), %r8
	movq	104(%rbx), %r9
	movq	$48, %rsi
	movq	$13, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
	movq	temp+56(%rip), %rdi
	movq	gen_pt+96(%rip), %r8
	movq	96(%rbx), %r9
	movq	$48, %rsi
	movq	$12, %rdx
	movq	%r14, %rcx
	callq	restart_gather_from_temp
.LBB5_25:                               # %for.cond153.preheader
	movq	(%rbx), %rdi
	callq	wait_gather
	movq	64(%rbx), %rdi
	callq	wait_gather
	movq	8(%rbx), %rdi
	callq	wait_gather
	movq	72(%rbx), %rdi
	callq	wait_gather
	movq	16(%rbx), %rdi
	callq	wait_gather
	movq	80(%rbx), %rdi
	callq	wait_gather
	movq	24(%rbx), %rdi
	callq	wait_gather
	movq	88(%rbx), %rdi
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r14d
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r14, %rax
	je	.LBB5_27
# BB#26:                                # %for.cond153.preheader
	movl	sites_on_node(%rip), %eax
.LBB5_27:                               # %for.cond153.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	je	.LBB5_29
# BB#28:                                # %for.cond153.preheader
	xorq	%r14, %r14
.LBB5_29:                               # %for.cond153.preheader
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	(%rsp), %r9             # 8-byte Reload
	movq	%rsi, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jge	.LBB5_32
# BB#30:                                # %for.body183.lr.ph
	movq	lattice(%rip), %rax
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$4, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %r15
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	imulq	$3200, %r14, %rcx       # imm = 0xC80
	leaq	688(%rax,%rcx), %r13
	.align	16, 0x90
.LBB5_31:                               # %for.body183
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rdx
	movq	gen_pt+8(%rip), %rcx
	movq	gen_pt+16(%rip), %rdi
	movq	gen_pt+24(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r9, %r12
	leaq	(%r12,%r15), %r9
	leaq	576(%r13), %rdi
	callq	mult_su3_mat_vec_sum_4dir
	movq	gen_pt+64(%rip), %rax
	movq	(%rax,%rbx), %rsi
	movq	gen_pt+72(%rip), %rax
	movq	gen_pt+80(%rip), %rcx
	movq	gen_pt+88(%rip), %rdi
	movq	(%rax,%rbx), %rdx
	movq	(%rcx,%rbx), %rcx
	movq	temp+64(%rip), %r9
	movq	(%rdi,%rbx), %r8
	addq	%r15, %r9
	movq	%r13, %rdi
	callq	mult_su3_mat_vec_sum_4dir
	movq	%r12, %r9
	movq	40(%rsp), %rdx          # 8-byte Reload
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$48, %r15
	addq	$8, %rbx
	addq	$3200, %r13             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB5_31
.LBB5_32:                               # %for.cond212.preheader
	movq	%r9, %r12
	movq	16(%rsp), %rbx          # 8-byte Reload
	movq	56(%rbx), %rdi
	callq	wait_gather
	movq	48(%rbx), %rdi
	callq	wait_gather
	movq	40(%rbx), %rdi
	callq	wait_gather
	movq	32(%rbx), %rdi
	callq	wait_gather
	movq	120(%rbx), %rdi
	callq	wait_gather
	movq	112(%rbx), %rdi
	callq	wait_gather
	movq	104(%rbx), %rdi
	callq	wait_gather
	movq	96(%rbx), %rdi
	callq	wait_gather
	movl	even_sites_on_node(%rip), %r14d
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	$2, %rdx
	movq	%r14, %rax
	je	.LBB5_34
# BB#33:                                # %for.cond212.preheader
	movl	sites_on_node(%rip), %eax
.LBB5_34:                               # %for.cond212.preheader
	movq	$32, %rcx
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rdx
	je	.LBB5_36
# BB#35:                                # %for.cond212.preheader
	xorq	%r14, %r14
.LBB5_36:                               # %for.cond212.preheader
	movq	$32, %rcx
	movq	%r14, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	movq	%rsi, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	jge	.LBB5_39
# BB#37:                                # %for.body250.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	$4, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r13
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	.align	16, 0x90
.LBB5_38:                               # %for.body250
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt+56(%rip), %rdx
	movq	gen_pt+48(%rip), %rcx
	movq	gen_pt+40(%rip), %rdi
	movq	gen_pt+32(%rip), %rax
	movq	(%rdx,%rbx), %rsi
	movq	(%rcx,%rbx), %rdx
	movq	(%rdi,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	movq	%r12, %r15
	leaq	(%r15,%r13), %r12
	movq	%r12, %rdi
	callq	sub_four_su3_vecs
	movq	temp+64(%rip), %rdi
	movq	gen_pt+120(%rip), %rsi
	movq	gen_pt+112(%rip), %rdx
	movq	gen_pt+104(%rip), %rcx
	movq	gen_pt+96(%rip), %rax
	movq	(%rsi,%rbx), %rsi
	movq	(%rdx,%rbx), %rdx
	movq	(%rcx,%rbx), %rcx
	movq	(%rax,%rbx), %r8
	addq	%r13, %rdi
	callq	sub_four_su3_vecs
	movq	temp+64(%rip), %rsi
	addq	%r13, %rsi
	movq	%r12, %rdi
	movq	%r12, %rdx
	movq	%r15, %r12
	callq	add_su3_vector
	movq	40(%rsp), %rdx          # 8-byte Reload
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$48, %r13
	addq	$8, %rbx
	cmpq	%rdx, %rax
	jl	.LBB5_38
.LBB5_39:                               # %for.end280
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp83:
	.size	dslash_fn_on_temp_special, .Ltmp83-dslash_fn_on_temp_special
	.cfi_endproc

	.type	temp_not_allocated,@object # @temp_not_allocated
	.local	temp_not_allocated
	.comm	temp_not_allocated,1,1
	.type	temp,@object            # @temp
	.local	temp
	.comm	temp,72,16

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
