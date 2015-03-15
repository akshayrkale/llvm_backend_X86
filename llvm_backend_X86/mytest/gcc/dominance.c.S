	.file	"dominance.c.bc"
	.text
	.globl	calculate_dominance_info
	.align	16, 0x90
	.type	calculate_dominance_info,@function
calculate_dominance_info:               # @calculate_dominance_info
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
	subq	$176, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 224
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
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	jne	.LBB0_2
# BB#1:                                 # %entry
	testq	%rdi, %rdi
	je	.LBB0_101
.LBB0_2:                                # %if.end
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movl	n_basic_blocks(%rip), %r12d
	addq	$2, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%r12, %r14
	andq	%rbx, %r14
	movq	$4, %rsi
	movq	%r14, %rdi
	callq	xcalloc
	movq	%rax, 80(%rsp)
	movq	$2, %rcx
	shlq	%cl, %r14
	movq	%r14, %rdi
	callq	xmalloc
	movq	%rbx, %rsi
	movq	%r12, %rbx
	andq	%rsi, %rbx
	movq	%rax, 96(%rsp)
	je	.LBB0_102
# BB#3:
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_4:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, (%rax,%rcx,4)
	incq	%rcx
	movq	%rcx, %rdx
	andq	%rsi, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB0_4
# BB#5:                                 # %for.body26.i.preheader
	movq	%r14, %rdi
	movq	%rsi, %r15
	callq	xmalloc
	movq	%rax, 88(%rsp)
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB0_6:                                # %for.body26.i
                                        # =>This Inner Loop Header: Depth=1
	movl	%ecx, (%rax,%rcx,4)
	incq	%rcx
	movq	%rcx, %rdx
	andq	%r15, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB0_6
	jmp	.LBB0_7
.LBB0_102:                              # %if.else18.thread.i
	movq	%r14, %rdi
	movq	%rsi, %r15
	callq	xmalloc
	movq	%rax, 88(%rsp)
.LBB0_7:                                # %do.body35.i
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 120(%rsp)
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 104(%rsp)
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 112(%rsp)
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 128(%rsp)
	movq	%r14, %rdi
	callq	xmalloc
	testq	%r15, %r12
	movq	%rax, 136(%rsp)
	je	.LBB0_10
# BB#8:
	movq	$1, %rcx
	movq	$1, %rdx
	.align	16, 0x90
.LBB0_9:                                # %for.body63.i
                                        # =>This Inner Loop Header: Depth=1
	movl	%edx, -4(%rax,%rcx,4)
	movq	%rcx, %rsi
	andq	%r15, %rsi
	incq	%rcx
	cmpq	%rbx, %rsi
	jne	.LBB0_9
.LBB0_10:                               # %init_dom_info.exit
	movq	$4, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movl	n_basic_blocks(%rip), %edi
	movq	%rax, 144(%rsp)
	incq	%rdi
	andq	%r15, %rdi
	movq	$4, %rsi
	callq	xcalloc
	movq	%rax, 152(%rsp)
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	xcalloc
	movq	%rax, 160(%rsp)
	movq	$1, %rcx
	movl	%ecx, 168(%rsp)
	xorq	%rdx, %rdx
	movl	%edx, 172(%rsp)
	movq	56(%rsp), %rbx          # 8-byte Reload
	andq	%r15, %rbx
	jne	.LBB0_11
# BB#12:                                # %init_dom_info.exit
	movabsq	$entry_exit_blocks, %rsi
	jmp	.LBB0_13
.LBB0_11:
	movabsq	$entry_exit_blocks+112, %rsi
.LBB0_13:                               # %init_dom_info.exit
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movslq	n_basic_blocks(%rip), %rdx
	movq	152(%rsp), %rdi
	movl	%ecx, (%rdi,%rdx,4)
	movl	168(%rsp), %ecx
	movq	%rsi, (%rax,%rcx,8)
	incq	%rcx
	movl	%ecx, 168(%rsp)
	leaq	80(%rsp), %rdi
	movq	%rsi, %r12
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	calc_dfs_tree_nonrec
	testq	%rbx, %rbx
	je	.LBB0_18
# BB#14:                                # %if.then.i
	movslq	n_basic_blocks(%rip), %rbx
	testq	%rbx, %rbx
	jle	.LBB0_18
# BB#15:                                # %for.body.lr.ph.i
	decq	%rbx
	leaq	80(%rsp), %r14
	.align	16, 0x90
.LBB0_16:                               # %for.body.i35
                                        # =>This Inner Loop Header: Depth=1
	movq	basic_block_info(%rip), %rax
	movq	32(%rax,%rbx,8), %rsi
	movslq	88(%rsi), %rax
	movq	152(%rsp), %rcx
	movl	(%rcx,%rax,4), %edx
	testq	%rdx, %rdx
	jne	.LBB0_17
# BB#103:                               # %if.end.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movl	168(%rsp), %edx
	movl	%edx, (%rcx,%rax,4)
	movl	168(%rsp), %eax
	movq	160(%rsp), %rcx
	movq	%rsi, (%rcx,%rax,8)
	incq	%rax
	movl	%eax, 168(%rsp)
	movq	%r14, %rdi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	calc_dfs_tree_nonrec
.LBB0_17:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB0_16 Depth=1
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB0_16
.LBB0_18:                               # %if.end24.i
	movl	168(%rsp), %ecx
	movl	n_basic_blocks(%rip), %eax
	decq	%rcx
	movq	%rcx, %r13
	movq	%r15, %r9
	andq	%r9, %r13
	movl	%ecx, 172(%rsp)
	incq	%rax
	andq	%r9, %rax
	cmpq	%rax, %r13
	jne	.LBB0_104
# BB#19:                                # %calc_dfs_tree.exit
	cmpq	$2, %r13
	movq	%r12, %r8
	jb	.LBB0_78
	.align	16, 0x90
.LBB0_20:                               # %while.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_40 Depth 2
                                        #     Child Loop BB0_25 Depth 2
                                        #     Child Loop BB0_57 Depth 2
                                        #     Child Loop BB0_64 Depth 2
                                        #     Child Loop BB0_66 Depth 2
	movq	%r13, 64(%rsp)          # 8-byte Spill
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	80(%rsp), %rcx
	movq	160(%rsp), %rax
	movq	(%rax,%r13,8), %rax
	movl	(%rcx,%r13,4), %ecx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r9, %rcx
	jne	.LBB0_21
# BB#22:                                # %while.body.i
                                        #   in Loop: Header=BB0_20 Depth=1
	addq	$32, %rax
	jmp	.LBB0_23
	.align	16, 0x90
.LBB0_21:                               #   in Loop: Header=BB0_20 Depth=1
	addq	$40, %rax
.LBB0_23:                               # %while.body.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	movq	%r13, %r14
	je	.LBB0_55
# BB#24:                                # %for.body.lr.ph.i47
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	56(%rsp), %rax          # 8-byte Reload
	testq	%r9, %rax
	movq	%r13, %r12
	movq	%r13, %r15
	je	.LBB0_40
	.align	16, 0x90
.LBB0_25:                               # %if.end11.us.i
                                        #   Parent Loop BB0_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rax
	movq	8(%rbx), %rbx
	cmpq	%r8, %rax
	jne	.LBB0_26
# BB#27:                                # %if.then13.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movslq	n_basic_blocks(%rip), %rax
	jmp	.LBB0_28
	.align	16, 0x90
.LBB0_26:                               # %if.else16.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movslq	88(%rax), %rax
.LBB0_28:                               # %if.end20.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	152(%rsp), %rcx
	leaq	(%rcx,%rax,4), %rax
	movl	(%rax), %r14d
	movq	%r13, %rax
	andq	%r9, %rax
	cmpq	%rax, %r14
	jbe	.LBB0_37
# BB#29:                                # %if.then22.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	128(%rsp), %rax
	movl	(%rax,%r14,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_30
# BB#35:                                # %if.then.i.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	96(%rsp), %rcx
	movq	88(%rsp), %rax
	movl	(%rcx,%r14,4), %ecx
	jmp	.LBB0_36
	.align	16, 0x90
.LBB0_30:                               # %if.end.i.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movl	(%rax,%rcx,4), %eax
	testq	%rax, %rax
	je	.LBB0_32
# BB#31:                                # %if.then7.i.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	leaq	80(%rsp), %rdi
	movq	%r14, %rsi
	movq	%r9, %r15
	movq	%r8, %r13
	callq	compress
	movq	%r13, %r8
	movq	%r15, %r9
	movq	128(%rsp), %rax
	movl	(%rax,%r14,4), %ecx
.LBB0_32:                               # %if.end11.i.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	96(%rsp), %rdx
	andq	%r9, %rcx
	movq	88(%rsp), %rax
	movl	(%rdx,%rcx,4), %ecx
	movl	(%rdx,%r14,4), %edx
	movl	(%rax,%rcx,4), %esi
	movl	(%rax,%rdx,4), %edi
	cmpq	%rdi, %rsi
	jb	.LBB0_34
# BB#33:                                # %select.mid
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	%rdx, %rcx
.LBB0_34:                               # %select.end
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	64(%rsp), %r13          # 8-byte Reload
.LBB0_36:                               # %eval.exit.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	andq	%r9, %rcx
	movl	(%rax,%rcx,4), %r14d
.LBB0_37:                               # %if.end25.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	%r12, %rax
	andq	%r9, %rax
	movq	%r14, %rcx
	andq	%r9, %rcx
	cmpq	%rax, %rcx
	jb	.LBB0_39
# BB#38:                                # %if.end25.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	movq	%r12, %r14
.LBB0_39:                               # %if.end25.us.i
                                        #   in Loop: Header=BB0_25 Depth=2
	testq	%rbx, %rbx
	movq	%r14, %r12
	jne	.LBB0_25
	jmp	.LBB0_55
	.align	16, 0x90
.LBB0_40:                               # %if.end11.i
                                        #   Parent Loop BB0_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rbx), %rax
	movq	(%rbx), %rbx
	cmpq	%r8, %rax
	jne	.LBB0_42
# BB#41:                                # %if.then13.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movslq	n_basic_blocks(%rip), %rax
	jmp	.LBB0_43
	.align	16, 0x90
.LBB0_42:                               # %if.else16.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movslq	88(%rax), %rax
.LBB0_43:                               # %if.end20.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	152(%rsp), %rcx
	leaq	(%rcx,%rax,4), %rax
	movl	(%rax), %r14d
	movq	%r13, %rax
	andq	%r9, %rax
	cmpq	%rax, %r14
	jbe	.LBB0_52
# BB#44:                                # %if.then22.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	128(%rsp), %rax
	movl	(%rax,%r14,4), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_46
# BB#45:                                # %if.then.i.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	96(%rsp), %rcx
	movq	88(%rsp), %rax
	movl	(%rcx,%r14,4), %ecx
	jmp	.LBB0_51
	.align	16, 0x90
.LBB0_46:                               # %if.end.i.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movl	(%rax,%rcx,4), %eax
	testq	%rax, %rax
	je	.LBB0_48
# BB#47:                                # %if.then7.i.i
                                        #   in Loop: Header=BB0_40 Depth=2
	leaq	80(%rsp), %rdi
	movq	%r14, %rsi
	movq	%r9, %r12
	movq	%r8, %r13
	callq	compress
	movq	%r13, %r8
	movq	%r12, %r9
	movq	128(%rsp), %rax
	movl	(%rax,%r14,4), %ecx
.LBB0_48:                               # %if.end11.i.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	96(%rsp), %rdx
	andq	%r9, %rcx
	movq	88(%rsp), %rax
	movl	(%rdx,%rcx,4), %ecx
	movl	(%rdx,%r14,4), %edx
	movl	(%rax,%rcx,4), %esi
	movl	(%rax,%rdx,4), %edi
	cmpq	%rdi, %rsi
	jb	.LBB0_50
# BB#49:                                # %select.mid88
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	%rdx, %rcx
.LBB0_50:                               # %select.end87
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	64(%rsp), %r13          # 8-byte Reload
.LBB0_51:                               # %eval.exit.i
                                        #   in Loop: Header=BB0_40 Depth=2
	andq	%r9, %rcx
	movl	(%rax,%rcx,4), %r14d
.LBB0_52:                               # %if.end25.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	%r15, %rax
	andq	%r9, %rax
	movq	%r14, %rcx
	andq	%r9, %rcx
	cmpq	%rax, %rcx
	jb	.LBB0_54
# BB#53:                                # %if.end25.i
                                        #   in Loop: Header=BB0_40 Depth=2
	movq	%r15, %r14
.LBB0_54:                               # %if.end25.i
                                        #   in Loop: Header=BB0_40 Depth=2
	testq	%rbx, %rbx
	movq	%r14, %r15
	jne	.LBB0_40
.LBB0_55:                               # %for.end.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	88(%rsp), %r11
	movl	%r14d, (%r11,%r13,4)
	movq	144(%rsp), %r15
	movq	96(%rsp), %r10
	movl	(%r15,%r13,4), %ebx
	movl	(%r10,%r13,4), %eax
	movl	(%r10,%rbx,4), %ecx
	movl	(%r11,%rax,4), %esi
	movl	(%r11,%rcx,4), %ecx
	movq	136(%rsp), %rdi
	cmpq	%rcx, %rsi
	movq	%r13, %r8
	movq	%r13, %rsi
	jae	.LBB0_60
# BB#56:                                # %while.body.i.lr.ph.i.preheader
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	128(%rsp), %rax
	movq	%rax, 72(%rsp)          # 8-byte Spill
	leaq	(%r15,%r13,4), %r12
	movq	%r13, %r8
	movq	%r13, %rsi
	.align	16, 0x90
.LBB0_57:                               # %while.body.i.i
                                        #   Parent Loop BB0_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	(%r15,%rbx,4), %edx
	movl	(%rdi,%r8,4), %eax
	movl	(%rdi,%rdx,4), %edx
	movl	(%rdi,%rbx,4), %ecx
	addq	%rax, %rdx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rdx
	andq	$2147483647, %rcx       # imm = 0x7FFFFFFF
	shlq	$1, %rcx
	cmpq	%rcx, %rdx
	jb	.LBB0_59
# BB#58:                                # %if.then.i167.i
                                        #   in Loop: Header=BB0_57 Depth=2
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	%esi, (%rax,%rbx,4)
	movl	(%r12), %eax
	movl	(%r15,%rax,4), %ebx
	movl	%ebx, (%r12)
	movl	(%r10,%r13,4), %eax
	movl	(%r10,%rbx,4), %ecx
	movl	(%r11,%rax,4), %edx
	movl	(%r11,%rcx,4), %ecx
	cmpq	%rcx, %rdx
	jb	.LBB0_57
	jmp	.LBB0_60
.LBB0_59:                               # %if.else.i.i
                                        #   in Loop: Header=BB0_57 Depth=2
	movl	%eax, (%rdi,%rbx,4)
	movl	(%r12), %esi
	movq	72(%rsp), %rax          # 8-byte Reload
	movl	%esi, (%rax,%r8,4)
	movl	(%r15,%rsi,4), %ebx
	movl	(%r10,%r13,4), %eax
	movl	(%r10,%rbx,4), %ecx
	movl	(%r11,%rax,4), %edx
	movl	(%r11,%rcx,4), %ecx
	leaq	(%r15,%rsi,4), %r12
	cmpq	%rcx, %rdx
	movq	%rsi, %r8
	jb	.LBB0_57
	.align	16, 0x90
.LBB0_60:                               # %while.end.i.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	%eax, (%r10,%r8,4)
	movl	(%rdi,%r13,4), %eax
	movq	48(%rsp), %rdx          # 8-byte Reload
	movl	(%rdi,%rdx,4), %ecx
	addq	%rax, %rcx
	movl	%ecx, (%rdi,%rdx,4)
	movl	(%rdi,%r13,4), %eax
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	andq	%r9, %rcx
	andq	$2147483647, %rax       # imm = 0x7FFFFFFF
	shlq	$1, %rax
	cmpq	%rax, %rcx
	leaq	80(%rsp), %r12
	movq	%r13, %rdi
	jae	.LBB0_62
# BB#61:                                # %if.then79.i.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movl	(%r15,%rdx,4), %eax
	movl	%esi, (%r15,%rdx,4)
	movq	%rax, %rsi
.LBB0_62:                               # %while.cond87.preheader.i.i
                                        #   in Loop: Header=BB0_20 Depth=1
	testq	%r9, %rsi
	je	.LBB0_65
# BB#63:                                # %while.body88.lr.ph.i.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	128(%rsp), %rax
	.align	16, 0x90
.LBB0_64:                               # %while.body88.i.i
                                        #   Parent Loop BB0_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%r9, %rsi
	movl	%edx, (%rax,%rsi,4)
	movl	(%r15,%rsi,4), %esi
	testq	%rsi, %rsi
	jne	.LBB0_64
.LBB0_65:                               # %link_roots.exit.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	104(%rsp), %rax
	andq	%r9, %r14
	movq	112(%rsp), %r8
	movl	(%rax,%r14,4), %ecx
	movl	%ecx, (%r8,%rdi,4)
	movl	%edi, (%rax,%r14,4)
	movq	%rdi, %r15
	movl	(%rax,%rdx,4), %esi
	testq	%rsi, %rsi
	je	.LBB0_77
	.align	16, 0x90
.LBB0_66:                               # %for.body44.i
                                        #   Parent Loop BB0_20 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	128(%rsp), %rcx
	movq	%rsi, %rbx
	andq	%r9, %rbx
	movl	(%rcx,%rbx,4), %eax
	testq	%rax, %rax
	jne	.LBB0_68
# BB#67:                                # %if.then.i175.i
                                        #   in Loop: Header=BB0_66 Depth=2
	movl	(%r10,%rbx,4), %ecx
	jmp	.LBB0_73
	.align	16, 0x90
.LBB0_68:                               # %if.end.i179.i
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%rdx, %r13
	movl	(%rcx,%rax,4), %ecx
	testq	%rcx, %rcx
	je	.LBB0_70
# BB#69:                                # %if.then7.i181.i
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%r12, %rdi
	movq	%r9, %r14
	callq	compress
	movq	%r14, %r9
	movq	128(%rsp), %rax
	movl	(%rax,%rbx,4), %eax
	movq	88(%rsp), %r11
	movq	96(%rsp), %r10
	movq	112(%rsp), %r8
.LBB0_70:                               # %if.end11.i194.i
                                        #   in Loop: Header=BB0_66 Depth=2
	andq	%r9, %rax
	movl	(%r10,%rax,4), %ecx
	movl	(%r10,%rbx,4), %eax
	movl	(%r11,%rcx,4), %esi
	movl	(%r11,%rax,4), %edi
	cmpq	%rdi, %rsi
	jb	.LBB0_72
# BB#71:                                # %select.mid145
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%rax, %rcx
.LBB0_72:                               # %select.end144
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%r13, %rdx
.LBB0_73:                               # %eval.exit196.i
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%rcx, %rax
	andq	%r9, %rax
	movl	(%r11,%rax,4), %esi
	movl	(%r11,%rbx,4), %edi
	movq	120(%rsp), %rax
	cmpq	%rdi, %rsi
	jb	.LBB0_75
# BB#74:                                # %select.mid150
                                        #   in Loop: Header=BB0_66 Depth=2
	movq	%rdx, %rcx
.LBB0_75:                               # %select.end149
                                        #   in Loop: Header=BB0_66 Depth=2
	movl	%ecx, (%rax,%rbx,4)
	movl	(%r8,%rbx,4), %esi
	testq	%rsi, %rsi
	jne	.LBB0_66
# BB#76:                                # %for.cond42.for.end65_crit_edge.i
                                        #   in Loop: Header=BB0_20 Depth=1
	movq	104(%rsp), %rax
.LBB0_77:                               # %for.end65.i
                                        #   in Loop: Header=BB0_20 Depth=1
	xorq	%rcx, %rcx
	movl	%ecx, (%rax,%rdx,4)
	movq	40(%rsp), %rcx          # 8-byte Reload
	decq	%rcx
	movq	%rcx, %rax
	andq	%r9, %rax
	movq	%r15, %r13
	decq	%r13
	cmpq	$1, %rax
	movq	32(%rsp), %r8           # 8-byte Reload
	ja	.LBB0_20
.LBB0_78:                               # %while.end.i
	movq	120(%rsp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 4(%rax)
	movl	172(%rsp), %ecx
	cmpq	$2, %rcx
	jb	.LBB0_83
# BB#79:                                # %for.body74.lr.ph.i
	movq	88(%rsp), %r8
	movq	$2, %rsi
	.align	16, 0x90
.LBB0_80:                               # %for.body74.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	andq	%r9, %rdi
	movl	(%rax,%rdi,4), %ebx
	movl	(%r8,%rdi,4), %edx
	cmpq	%rdx, %rbx
	je	.LBB0_82
# BB#81:                                # %if.then82.i
                                        #   in Loop: Header=BB0_80 Depth=1
	movl	(%rax,%rbx,4), %ecx
	movl	%ecx, (%rax,%rdi,4)
	movl	172(%rsp), %ecx
.LBB0_82:                               # %for.inc93.i
                                        #   in Loop: Header=BB0_80 Depth=1
	incq	%rsi
	movq	%rsi, %rdx
	andq	%r9, %rdx
	movq	%rcx, %rdi
	andq	%r9, %rdi
	cmpq	%rdi, %rdx
	jbe	.LBB0_80
.LBB0_83:                               # %calc_idoms.exit
	movq	%r9, %r11
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	je	.LBB0_89
# BB#84:                                # %for.cond.preheader
	movslq	n_basic_blocks(%rip), %rdi
	testq	%rdi, %rdi
	jle	.LBB0_89
# BB#85:                                # %for.body.lr.ph
	movq	basic_block_info(%rip), %r9
	movq	152(%rsp), %r10
	movq	160(%rsp), %r8
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB0_86:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	32(%r9,%rsi,8), %rcx
	movslq	88(%rcx), %rcx
	movl	(%r10,%rcx,4), %ecx
	movl	(%rax,%rcx,4), %ecx
	cmpq	$2, %rcx
	jb	.LBB0_88
# BB#87:                                # %if.then9
                                        #   in Loop: Header=BB0_86 Depth=1
	movq	(%r8,%rcx,8), %rcx
	movl	88(%rcx), %ecx
	movq	16(%rsp), %rdx          # 8-byte Reload
	movl	%ecx, (%rdx,%rsi,4)
	movl	n_basic_blocks(%rip), %edi
.LBB0_88:                               # %for.inc
                                        #   in Loop: Header=BB0_86 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rbx
	jl	.LBB0_86
.LBB0_89:                               # %if.end16
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB0_100
# BB#90:                                # %if.then18
	movl	n_basic_blocks(%rip), %esi
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r11, %rbx
	callq	sbitmap_vector_zero
	movq	%rbx, %r8
	movl	172(%rsp), %eax
	testq	%rax, %rax
	je	.LBB0_100
# BB#91:
	movslq	n_basic_blocks(%rip), %rcx
	movq	152(%rsp), %rdx
	movl	(%rdx,%rcx,4), %r14d
	movq	$1, %rbx
	andq	%r8, %r14
	.align	16, 0x90
.LBB0_92:                               # %for.body.i59
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rcx
	andq	%r8, %rcx
	cmpq	%r14, %rcx
	je	.LBB0_99
# BB#93:                                # %if.end.i64
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	160(%rsp), %rax
	movq	120(%rsp), %rdx
	movq	(%rax,%rcx,8), %rsi
	movl	(%rdx,%rcx,4), %ecx
	movslq	88(%rsi), %r15
	testq	%rcx, %rcx
	je	.LBB0_96
# BB#94:                                # %if.end.i64
                                        #   in Loop: Header=BB0_92 Depth=1
	cmpq	%r14, %rcx
	je	.LBB0_96
# BB#95:                                # %if.then10.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	(%rax,%rcx,8), %rax
	movslq	88(%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx,%r15,8), %rdi
	movq	(%rcx,%rax,8), %rsi
	leaq	(%rcx,%r15,8), %r13
	movq	%r8, %r12
	callq	sbitmap_copy
	jmp	.LBB0_98
	.align	16, 0x90
.LBB0_96:                               # %if.else.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	%r8, %r12
	testq	%rcx, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%rax,%r15,8), %r13
	jne	.LBB0_98
# BB#97:                                # %if.then26.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	(%r13), %rdi
	callq	sbitmap_ones
.LBB0_98:                               # %if.end30.i
                                        #   in Loop: Header=BB0_92 Depth=1
	movq	%r12, %r8
	movq	%r15, %rax
	movabsq	$4294967232, %rcx       # imm = 0xFFFFFFC0
	andq	%rcx, %rax
	movq	$1, %rdx
	movq	(%r13), %rsi
	movq	%r15, %rcx
	shlq	%cl, %rdx
	movq	$3, %rcx
	shrq	%cl, %rax
	orq	%rdx, 16(%rsi,%rax)
	movl	172(%rsp), %eax
.LBB0_99:                               # %for.inc.i
                                        #   in Loop: Header=BB0_92 Depth=1
	incq	%rbx
	movq	%rbx, %rcx
	andq	%r8, %rcx
	movq	%rax, %rdx
	andq	%r8, %rdx
	cmpq	%rdx, %rcx
	jbe	.LBB0_92
.LBB0_100:                              # %if.end19
	movq	80(%rsp), %rdi
	callq	free
	movq	96(%rsp), %rdi
	callq	free
	movq	88(%rsp), %rdi
	callq	free
	movq	120(%rsp), %rdi
	callq	free
	movq	104(%rsp), %rdi
	callq	free
	movq	112(%rsp), %rdi
	callq	free
	movq	128(%rsp), %rdi
	callq	free
	movq	136(%rsp), %rdi
	callq	free
	movq	144(%rsp), %rdi
	callq	free
	movq	152(%rsp), %rdi
	callq	free
	movq	160(%rsp), %rdi
	callq	free
.LBB0_101:                              # %cleanup
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_104:                              # %if.then29.i
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.calc_dfs_tree, %rcx
	movq	$344, %rsi              # imm = 0x158
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp17:
	.size	calculate_dominance_info, .Ltmp17-calculate_dominance_info
	.cfi_endproc

	.align	16, 0x90
	.type	compress,@function
compress:                               # @compress
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 32
.Ltmp25:
	.cfi_offset %rbx, -32
.Ltmp26:
	.cfi_offset %r14, -24
.Ltmp27:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	48(%r15), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rsi, %rbx
	movl	(%rax,%rbx,4), %r14d
	movl	(%rax,%r14,4), %eax
	testq	%rax, %rax
	je	.LBB1_4
# BB#1:                                 # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	compress
	movq	16(%r15), %rax
	movq	8(%r15), %rdx
	movl	(%rax,%r14,4), %ecx
	movl	(%rax,%rbx,4), %esi
	movl	(%rdx,%rcx,4), %edi
	movl	(%rdx,%rsi,4), %edx
	cmpq	%rdx, %rdi
	jae	.LBB1_3
# BB#2:                                 # %if.then14
	movl	%ecx, (%rax,%rbx,4)
.LBB1_3:                                # %if.end
	movq	48(%r15), %rax
	movl	(%rax,%r14,4), %ecx
	movl	%ecx, (%rax,%rbx,4)
.LBB1_4:                                # %if.end27
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp28:
	.size	compress, .Ltmp28-compress
	.cfi_endproc

	.align	16, 0x90
	.type	calc_dfs_tree_nonrec,@function
calc_dfs_tree_nonrec:                   # @calc_dfs_tree_nonrec
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp37:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp38:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp39:
	.cfi_def_cfa_offset 48
.Ltmp40:
	.cfi_offset %rbx, -48
.Ltmp41:
	.cfi_offset %r12, -40
.Ltmp42:
	.cfi_offset %r13, -32
.Ltmp43:
	.cfi_offset %r14, -24
.Ltmp44:
	.cfi_offset %r15, -16
	movl	n_basic_blocks(%rip), %eax
	addq	$3, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r13
	movq	%rax, %rdi
	callq	xmalloc
	leaq	40(%r15), %rdx
	addq	$32, %r15
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r14, %rcx
	andq	%r9, %rcx
	jne	.LBB2_2
# BB#1:                                 # %entry
	movq	%rdx, %r15
.LBB2_2:                                # %entry
	movabsq	$entry_exit_blocks+112, %rdx
	movabsq	$entry_exit_blocks, %rsi
	movq	%rdx, %r8
	jne	.LBB2_4
# BB#3:                                 # %entry
	movq	%rsi, %r8
.LBB2_4:                                # %entry
	testq	%rcx, %rcx
	jne	.LBB2_6
# BB#5:                                 # %entry
	movq	%rdx, %rsi
.LBB2_6:                                # %entry
	movq	(%r15), %rdx
	xorq	%r12, %r12
	jmp	.LBB2_7
	.align	16, 0x90
.LBB2_25:                               # %if.else61
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	8(%rcx), %rdx
.LBB2_7:                                # %while.cond1.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_13 Depth 2
                                        #     Child Loop BB2_9 Depth 2
	testq	%rdx, %rdx
	je	.LBB2_22
# BB#8:                                 # %while.body3.lr.ph
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	%r9, %r14
	je	.LBB2_13
	.align	16, 0x90
.LBB2_9:                                # %if.then5.us
                                        #   Parent Loop BB2_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	16(%rdx), %rcx
	cmpq	%rsi, %rcx
	je	.LBB2_11
# BB#10:                                # %lor.lhs.false.us
                                        #   in Loop: Header=BB2_9 Depth=2
	movslq	88(%rcx), %rdi
	movq	72(%r13), %rbx
	movl	(%rbx,%rdi,4), %edi
	testq	%rdi, %rdi
	je	.LBB2_12
.LBB2_11:                               # %while.cond1.backedge.us
                                        #   in Loop: Header=BB2_9 Depth=2
	movq	(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB2_9
	jmp	.LBB2_22
	.align	16, 0x90
.LBB2_13:                               # %if.else11
                                        #   Parent Loop BB2_7 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rdx), %rcx
	cmpq	%rsi, %rcx
	je	.LBB2_21
# BB#14:                                # %lor.lhs.false15
                                        #   in Loop: Header=BB2_13 Depth=2
	movslq	88(%rcx), %rdi
	movq	72(%r13), %rbx
	movl	(%rbx,%rdi,4), %edi
	testq	%rdi, %rdi
	je	.LBB2_15
.LBB2_21:                               # %while.cond1.backedge
                                        #   in Loop: Header=BB2_13 Depth=2
	movq	8(%rdx), %rdx
	testq	%rdx, %rdx
	jne	.LBB2_13
	.align	16, 0x90
.LBB2_22:                               # %while.end
                                        #   in Loop: Header=BB2_7 Depth=1
	testq	%r9, %r12
	je	.LBB2_26
# BB#23:                                # %if.end55
                                        #   in Loop: Header=BB2_7 Depth=1
	decq	%r12
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	(%rax,%rdx,8), %rcx
	testq	%r9, %r14
	je	.LBB2_25
# BB#24:                                # %if.then59
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%rcx), %rdx
	jmp	.LBB2_7
.LBB2_12:                               # %if.end9
                                        #   in Loop: Header=BB2_7 Depth=1
	leaq	24(%rdx), %r11
	movq	%rdx, %r10
	leaq	32(%rcx), %rdx
	jmp	.LBB2_16
.LBB2_15:                               # %if.end22
                                        #   in Loop: Header=BB2_7 Depth=1
	leaq	16(%rdx), %r11
	movq	%rdx, %r10
	leaq	40(%rcx), %rdx
.LBB2_16:                               # %if.end25
                                        #   in Loop: Header=BB2_7 Depth=1
	cmpq	%r8, %rcx
	je	.LBB2_27
# BB#17:                                # %if.end29
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	(%r11), %r11
	movq	(%rdx), %rdx
	cmpq	%r8, %r11
	jne	.LBB2_18
# BB#19:                                # %if.else37
                                        #   in Loop: Header=BB2_7 Depth=1
	movslq	n_basic_blocks(%rip), %rdi
	jmp	.LBB2_20
.LBB2_18:                               # %if.then32
                                        #   in Loop: Header=BB2_7 Depth=1
	movslq	88(%r11), %rdi
.LBB2_20:                               # %if.end41
                                        #   in Loop: Header=BB2_7 Depth=1
	movq	72(%r13), %r11
	leaq	(%r11,%rdi,4), %r15
	movl	88(%r13), %edi
	movl	(%r15), %r15d
	leaq	1(%rdi), %rbx
	movl	%ebx, 88(%r13)
	movslq	88(%rcx), %rbx
	movl	%edi, (%r11,%rbx,4)
	movq	80(%r13), %rbx
	movq	%rcx, (%rbx,%rdi,8)
	movq	(%r13), %rcx
	movl	%r15d, (%rcx,%rdi,4)
	movq	$32, %rcx
	leaq	1(%r12), %rdi
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r10, (%rax,%r12,8)
	movq	%rdi, %r12
	jmp	.LBB2_7
.LBB2_26:                               # %while.end64
	movq	%rax, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB2_27:                               # %if.then28
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.calc_dfs_tree_nonrec, %rcx
	movq	$266, %rsi              # imm = 0x10A
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp45:
	.size	calc_dfs_tree_nonrec, .Ltmp45-calc_dfs_tree_nonrec
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"dominance.c"
	.size	.L.str, 12

	.type	.L__FUNCTION__.calc_dfs_tree,@object # @__FUNCTION__.calc_dfs_tree
.L__FUNCTION__.calc_dfs_tree:
	.asciz	"calc_dfs_tree"
	.size	.L__FUNCTION__.calc_dfs_tree, 14

	.type	.L__FUNCTION__.calc_dfs_tree_nonrec,@object # @__FUNCTION__.calc_dfs_tree_nonrec
.L__FUNCTION__.calc_dfs_tree_nonrec:
	.asciz	"calc_dfs_tree_nonrec"
	.size	.L__FUNCTION__.calc_dfs_tree_nonrec, 21


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
