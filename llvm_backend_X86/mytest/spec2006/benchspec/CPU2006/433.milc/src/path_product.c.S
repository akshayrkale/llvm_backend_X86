	.file	"path_product.c.bc"
	.text
	.globl	path_product
	.align	16, 0x90
	.type	path_product,@function
path_product:                           # @path_product
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
	subq	$48, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 96
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
	movslq	sites_on_node(%rip), %r14
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %r13
	movq	%r13, 32(%rsp)          # 8-byte Spill
	movq	$144, %rsi
	movq	%r14, %rdi
	callq	calloc
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$144, %rsi
	movq	%r14, %rdi
	callq	calloc
	movslq	(%r13), %rsi
	movq	%rax, 8(%rsp)           # 8-byte Spill
	cmpq	$3, %rsi
	jg	.LBB0_4
# BB#1:                                 # %if.then
	leaq	(%rsi,%rsi,8), %rdi
	movq	$4, %rcx
	shlq	%cl, %rdi
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rsi, %rdx
	addq	$112, %rdi
	movq	$144, %rsi
	movq	$3, %rcx
	callq	start_gather
	jmp	.LBB0_2
.LBB0_4:                                # %if.else
	testq	%r14, %r14
                                        # implicit-def: RAX
	jle	.LBB0_2
# BB#5:                                 # %for.body.lr.ph
	movq	lattice(%rip), %r15
	addq	$2912, %r15             # imm = 0xB60
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r14
	jmp	.LBB0_6
	.align	16, 0x90
.LBB0_7:                                # %for.body.for.body_crit_edge
                                        #   in Loop: Header=BB0_6 Depth=1
	movl	(%r13), %esi
	incq	%rbx
	addq	$3200, %r15             # imm = 0xC80
.LBB0_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$7, %rax
	subq	%rsi, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,8), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	-2800(%r15,%rax), %rdi
	movq	%r15, %rsi
	callq	su3_adjoint
	movq	%rbx, %rax
	andq	%r12, %rax
	cmpq	%r14, %rax
                                        # implicit-def: RAX
	jne	.LBB0_7
.LBB0_2:                                # %for.cond18.preheader
	movq	%rax, %rdx
	movq	$32, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jle	.LBB0_3
# BB#8:
	movq	$1, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	%rdx, %r14
	.align	16, 0x90
.LBB0_9:                                # %for.body21
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_43 Depth 2
                                        #     Child Loop BB0_48 Depth 2
                                        #     Child Loop BB0_39 Depth 2
                                        #     Child Loop BB0_34 Depth 2
                                        #     Child Loop BB0_23 Depth 2
                                        #     Child Loop BB0_28 Depth 2
                                        #     Child Loop BB0_19 Depth 2
                                        #     Child Loop BB0_14 Depth 2
	movq	%rbx, %rsi
	movabsq	$2147483648, %rax       # imm = 0x80000000
	andq	%rax, %rsi
	movq	$31, %rcx
	shrq	%cl, %rsi
	addq	%rbx, %rsi
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %rsi
	movq	%rbx, %rcx
	movl	(%r13,%rbx,4), %eax
	movl	-4(%r13,%rbx,4), %edx
	subq	%rsi, %rcx
	andq	%r15, %rcx
	cmpq	$1, %rcx
	jne	.LBB0_30
# BB#10:                                # %if.then24
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rsi
	jg	.LBB0_21
# BB#11:                                # %if.then29
                                        #   in Loop: Header=BB0_9 Depth=1
	cmpq	$3, %rdx
	jg	.LBB0_17
# BB#12:                                # %if.then35
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rdi
	movq	%r14, 24(%rsp)          # 8-byte Spill
	callq	wait_gather
	movslq	sites_on_node(%rip), %rsi
	testq	%rsi, %rsi
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%r13, %r14
	movq	%r15, %rdx
	jle	.LBB0_15
# BB#13:                                # %for.body40.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %r12
	addq	$112, %r12
	movq	$1, %rbx
	movq	8(%rsp), %r13           # 8-byte Reload
	.align	16, 0x90
.LBB0_14:                               # %for.body40
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	movslq	(%r14,%rcx,4), %rcx
	movq	-8(%rax,%rbx,8), %rdi
	movq	%rsi, %r15
	leaq	(%rcx,%rcx,8), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	addq	%r12, %rsi
	movq	%r13, %rdx
	callq	mult_su3_nn
	movq	%r15, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	%rsi, %rcx
	andq	%rdx, %rcx
	incq	%rbx
	addq	$144, %r13
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rcx, %rax
	jne	.LBB0_14
.LBB0_15:                               # %for.end53
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rdx, %r15
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	40(%rsp), %r13          # 8-byte Reload
	jmp	.LBB0_16
	.align	16, 0x90
.LBB0_30:                               # %if.else134
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	$3, %rsi
	jg	.LBB0_41
# BB#31:                                # %if.then139
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jg	.LBB0_37
# BB#32:                                # %if.then145
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rdi
	movq	%r14, 24(%rsp)          # 8-byte Spill
	callq	wait_gather
	movslq	sites_on_node(%rip), %rdx
	testq	%rdx, %rdx
	movq	%r15, %rsi
	movq	%r13, %rax
	movq	40(%rsp), %r15          # 8-byte Reload
	jle	.LBB0_35
# BB#33:                                # %for.body150.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %r12
	addq	$2912, %r12             # imm = 0xB60
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_34:                               # %for.body150
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movslq	(%rcx,%r15,4), %rcx
	movq	-8(%rax,%rbx,8), %rdi
	leaq	(%rcx,%rcx,8), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	%rsi, %r13
	leaq	-2800(%r12,%rax), %rsi
	movq	%rdx, %r14
	movq	%r12, %rdx
	callq	mult_su3_nn
	movq	%r13, %rsi
	movq	%r14, %rdx
	movq	%rbx, %rax
	andq	%rsi, %rax
	movq	%rdx, %rcx
	andq	%rsi, %rcx
	incq	%rbx
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rcx, %rax
	movq	32(%rsp), %rax          # 8-byte Reload
	jne	.LBB0_34
.LBB0_35:                               # %for.end162
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rsi, %r15
	movq	%rax, %r12
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	jmp	.LBB0_36
	.align	16, 0x90
.LBB0_21:                               # %if.else77
                                        #   in Loop: Header=BB0_9 Depth=1
	cmpq	$3, %rdx
	jg	.LBB0_25
# BB#22:                                # %if.then83
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, %r13
	movq	%r14, %rdi
	movq	%r14, 24(%rsp)          # 8-byte Spill
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	movq	$1, %rbx
	movq	(%rsp), %r12            # 8-byte Reload
	jle	.LBB0_24
	.align	16, 0x90
.LBB0_23:                               # %for.body88
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r12, %rsi
	callq	su3mat_copy
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%r14, %rcx
	andq	%r15, %rcx
	incq	%rbx
	addq	$144, %r12
	cmpq	%rcx, %rax
	jne	.LBB0_23
.LBB0_24:                               # %for.end96
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	(%rax,%r13,4), %eax
	movq	%r13, %rbx
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$144, %rsi
	movq	$3, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	start_gather_from_temp
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_41:                               # %if.else193
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$3, %rdx
	jg	.LBB0_45
# BB#42:                                # %if.then199
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r14, %rdi
	movq	%r14, 24(%rsp)          # 8-byte Spill
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	movq	$1, %rbx
	movq	(%rsp), %r12            # 8-byte Reload
	jle	.LBB0_44
	.align	16, 0x90
.LBB0_43:                               # %for.body204
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r12, %rsi
	callq	su3mat_copy
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%r14, %rcx
	andq	%r15, %rcx
	incq	%rbx
	addq	$144, %r12
	cmpq	%rcx, %rax
	jne	.LBB0_43
.LBB0_44:                               # %for.end212
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cleanup_gather
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%r13,%rax,4), %eax
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$144, %rsi
	movq	$3, %rcx
	movq	(%rsp), %rdi            # 8-byte Reload
	jmp	.LBB0_46
.LBB0_17:                               # %if.else54
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, %r13
	movslq	sites_on_node(%rip), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB0_16
# BB#18:                                # %for.body59.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %rbx
	addq	$2912, %rbx             # imm = 0xB60
	movq	$1, %r12
	movq	8(%rsp), %r14           # 8-byte Reload
	jmp	.LBB0_19
	.align	16, 0x90
.LBB0_20:                               # %for.body59.for.body59_crit_edge
                                        #   in Loop: Header=BB0_19 Depth=2
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	(%rax,%r13,4), %eax
	incq	%r12
	addq	$144, %r14
	addq	$3200, %rbx             # imm = 0xC80
.LBB0_19:                               # %for.body59
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,8), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	-2800(%rbx,%rax), %rsi
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	mult_su3_nn
	movq	%r12, %rax
	andq	%r15, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_20
.LBB0_16:                               # %if.end72
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r13, %rbx
	movq	32(%rsp), %r13          # 8-byte Reload
	movl	(%r13,%rbx,4), %eax
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$144, %rsi
	movq	$3, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	start_gather_from_temp
	movq	%rax, %r14
	jmp	.LBB0_51
.LBB0_37:                               # %if.else163
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r13, %r12
	movslq	sites_on_node(%rip), %rcx
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	testq	%rcx, %rcx
	jle	.LBB0_36
# BB#38:                                # %for.body168.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %rbx
	addq	$2912, %rbx             # imm = 0xB60
	movq	$1, %r13
	movq	8(%rsp), %r14           # 8-byte Reload
	jmp	.LBB0_39
	.align	16, 0x90
.LBB0_40:                               # %for.body168.for.body168_crit_edge
                                        #   in Loop: Header=BB0_39 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%r12,%rax,4), %eax
	incq	%r13
	addq	$144, %r14
	addq	$3200, %rbx             # imm = 0xC80
.LBB0_39:                               # %for.body168
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,8), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	-2800(%rbx,%rax), %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	mult_su3_nn
	movq	%r13, %rax
	andq	%r15, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_40
.LBB0_36:                               # %if.end181
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%r12, %r13
	movq	40(%rsp), %rbx          # 8-byte Reload
	movl	(%r13,%rbx,4), %eax
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$2912, %rdi             # imm = 0xB60
	movq	$144, %rsi
	movq	$3, %rcx
	callq	start_gather
	movq	%rax, %r14
	jmp	.LBB0_51
.LBB0_25:                               # %if.else101
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$2912, %rdi             # imm = 0xB60
	movq	$144, %rsi
	movq	$3, %rcx
	callq	start_gather
.LBB0_26:                               # %if.end113
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	jle	.LBB0_29
# BB#27:                                # %for.body118.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %r12
	addq	$112, %r12
	movq	$1, %r13
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB0_28:                               # %for.body118
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%r13,8), %rdi
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	40(%rsp), %rcx          # 8-byte Reload
	movl	(%rax,%rcx,4), %eax
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,8), %rsi
	movq	$4, %rcx
	shlq	%cl, %rsi
	addq	%r12, %rsi
	movq	%rbx, %rdx
	callq	mult_su3_na
	movq	%r13, %rax
	andq	%r15, %rax
	movq	%r14, %rcx
	andq	%r15, %rcx
	incq	%r13
	addq	$144, %rbx
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rcx, %rax
	jne	.LBB0_28
.LBB0_29:                               # %for.end132
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	cleanup_gather
	movq	32(%rsp), %r13          # 8-byte Reload
	jmp	.LBB0_50
.LBB0_45:                               # %if.else217
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	gen_pt(%rip), %r8
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$144, %rsi
	movq	$3, %rcx
	movq	8(%rsp), %rdi           # 8-byte Reload
.LBB0_46:                               # %if.end222
                                        #   in Loop: Header=BB0_9 Depth=1
	callq	start_gather_from_temp
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	jle	.LBB0_49
# BB#47:                                # %for.body227.lr.ph
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	lattice(%rip), %r12
	addq	$2912, %r12             # imm = 0xB60
	movq	$1, %rbx
	.align	16, 0x90
.LBB0_48:                               # %for.body227
                                        #   Parent Loop BB0_9 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	(%r13,%rax,4), %eax
	movq	$7, %rdx
	subq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	leaq	(%rdx,%rdx,8), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	-2800(%r12,%rax), %rsi
	movq	%r12, %rdx
	callq	mult_su3_na
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	%r14, %rcx
	andq	%r15, %rcx
	incq	%rbx
	addq	$3200, %r12             # imm = 0xC80
	cmpq	%rcx, %rax
	jne	.LBB0_48
.LBB0_49:                               # %for.end240
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdi
	callq	cleanup_gather
.LBB0_50:                               # %for.inc243
                                        #   in Loop: Header=BB0_9 Depth=1
	movq	40(%rsp), %rbx          # 8-byte Reload
.LBB0_51:                               # %for.inc243
                                        #   in Loop: Header=BB0_9 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%r15, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%r15, %rcx
	cmpq	%rcx, %rax
	jne	.LBB0_9
	jmp	.LBB0_52
.LBB0_3:
	movq	%rdx, %r14
.LBB0_52:                               # %for.end245
	movq	16(%rsp), %rdx          # 8-byte Reload
	leaq	-1(%rdx), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	(%r13,%rax,4), %eax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	$1, %rdx
	je	.LBB0_53
# BB#61:                                # %if.else282
	cmpq	$3, %rax
	jg	.LBB0_68
# BB#62:                                # %if.then288
	movq	%r14, %rdi
	movq	%r14, %r13
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	jle	.LBB0_65
# BB#63:
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r14
	movq	(%rsp), %r15            # 8-byte Reload
	.align	16, 0x90
.LBB0_64:                               # %for.body293
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	su3mat_copy
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	addq	$144, %r15
	cmpq	%r14, %rax
	jne	.LBB0_64
.LBB0_65:                               # %for.end301
	movq	%r13, %rdi
	callq	cleanup_gather
	movslq	sites_on_node(%rip), %r15
	testq	%r15, %r15
	jle	.LBB0_68
# BB#66:                                # %for.body306.lr.ph
	movq	lattice(%rip), %r14
	addq	$2912, %r14             # imm = 0xB60
	movq	$1, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r15
	movq	(%rsp), %rbx            # 8-byte Reload
	.align	16, 0x90
.LBB0_67:                               # %for.body306
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	su3mat_copy
	movq	%r12, %rax
	andq	%r13, %rax
	incq	%r12
	addq	$144, %rbx
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%r15, %rax
	jne	.LBB0_67
	jmp	.LBB0_68
.LBB0_53:                               # %if.then249
	cmpq	$3, %rax
	jg	.LBB0_58
# BB#54:                                # %if.then255
	movq	%r14, %rdi
	movq	%r14, %r13
	callq	wait_gather
	movslq	sites_on_node(%rip), %r14
	testq	%r14, %r14
	jle	.LBB0_57
# BB#55:                                # %for.body260.lr.ph
	movq	lattice(%rip), %r15
	addq	$2912, %r15             # imm = 0xB60
	movq	$1, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	andq	%r12, %r14
	.align	16, 0x90
.LBB0_56:                               # %for.body260
                                        # =>This Inner Loop Header: Depth=1
	movq	gen_pt(%rip), %rax
	movq	-8(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	callq	su3mat_copy
	movq	%rbx, %rax
	andq	%r12, %rax
	incq	%rbx
	addq	$3200, %r15             # imm = 0xC80
	cmpq	%r14, %rax
	jne	.LBB0_56
.LBB0_57:                               # %for.end267
	movq	%r13, %rdi
	callq	cleanup_gather
	jmp	.LBB0_68
.LBB0_58:                               # %if.else268
	movslq	sites_on_node(%rip), %r15
	testq	%r15, %r15
	jle	.LBB0_68
# BB#59:                                # %for.body273.lr.ph
	movq	lattice(%rip), %r14
	addq	$2912, %r14             # imm = 0xB60
	movq	$1, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %r15
	movq	8(%rsp), %rbx           # 8-byte Reload
	.align	16, 0x90
.LBB0_60:                               # %for.body273
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	su3mat_copy
	movq	%r12, %rax
	andq	%r13, %rax
	incq	%r12
	addq	$144, %rbx
	addq	$3200, %r14             # imm = 0xC80
	cmpq	%r15, %rax
	jne	.LBB0_60
.LBB0_68:                               # %if.end316
	movq	(%rsp), %rdi            # 8-byte Reload
	callq	free
	movq	8(%rsp), %rdi           # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.Ltmp17:
	.size	path_product, .Ltmp17-path_product
	.cfi_endproc


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
