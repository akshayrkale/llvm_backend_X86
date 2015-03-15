	.file	"cppinit.c.bc"
	.text
	.globl	cpp_create_reader
	.align	16, 0x90
	.type	cpp_create_reader,@function
cpp_create_reader:                      # @cpp_create_reader
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp4:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp5:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp6:
	.cfi_def_cfa_offset 32
.Ltmp7:
	.cfi_offset %rbx, -32
.Ltmp8:
	.cfi_offset %r14, -24
.Ltmp9:
	.cfi_offset %r15, -16
	movzbl	init_library.initialized(%rip), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then.i
	movq	$1, %rax
	movb	%al, init_library.initialized(%rip)
.LBB0_2:                                # %init_library.exit
	movq	$1, %rdi
	movq	$832, %rsi              # imm = 0x340
	movq	$1, %r15
	callq	xcalloc
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movl	%r14d, 736(%rbx)
	movzbl	lang_defaults(,%rax,8), %ecx
	movb	%cl, 769(%rbx)
	movzbl	lang_defaults+1(,%rax,8), %ecx
	movb	%cl, 744(%rbx)
	movzbl	lang_defaults+2(,%rax,8), %ecx
	movb	%cl, 742(%rbx)
	movzbl	lang_defaults+3(,%rax,8), %ecx
	movb	%cl, 748(%rbx)
	movzbl	lang_defaults+4(,%rax,8), %ecx
	movb	%cl, 746(%rbx)
	movzbl	lang_defaults+5(,%rax,8), %ecx
	movb	%cl, 767(%rbx)
	movzbl	lang_defaults+6(,%rax,8), %ecx
	movb	%cl, 743(%rbx)
	movzbl	lang_defaults+7(,%rax,8), %eax
	movb	%al, 747(%rbx)
	movb	%r15b, 760(%rbx)
	movb	%r15b, 745(%rbx)
	movb	%r15b, 776(%rbx)
	movq	$8, %rax
	movl	%eax, 664(%rbx)
	movb	%r15b, 777(%rbx)
	movb	%r15b, 741(%rbx)
	movq	$1, %rdi
	movq	$112, %rsi
	callq	xcalloc
	movq	%rax, 672(%rbx)
	callq	deps_init
	movq	%rax, 392(%rbx)
	leaq	24(%rbx), %rdi
	callq	init_line_maps
	movl	%r15d, 64(%rbx)
	movzbl	745(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB0_3
# BB#4:                                 # %init_library.exit
	movabsq	$1, %rax
	jmp	.LBB0_5
.LBB0_3:
	movabsq	$0, %rax
.LBB0_5:                                # %init_library.exit
	movb	%al, 11(%rbx)
	movq	$67, %rax
	movb	%al, 302(%rbx)
	movq	$66, %rcx
	movb	%cl, 350(%rbx)
	movq	$0, 352(%rbx)
	movb	%al, 374(%rbx)
	xorq	%rax, %rax
	movb	%al, 375(%rbx)
	leaq	200(%rbx), %r14
	movq	$250, %rsi
	movq	%r14, %rdi
	callq	_cpp_init_tokenrun
	movq	%r14, 232(%rbx)
	movq	216(%rbx), %rax
	movq	%rax, 192(%rbx)
	leaq	96(%rbx), %rax
	movq	%rax, 152(%rbx)
	movq	$0, 136(%rbx)
	movq	$0, 104(%rbx)
	movq	$0, 96(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	_cpp_get_buff
	movq	%rax, 72(%rbx)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	_cpp_get_buff
	movq	%rax, 80(%rbx)
	leaq	488(%rbx), %rdi
	callq	gcc_obstack_init
	movq	%rbx, %rdi
	callq	_cpp_init_includes
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	cpp_create_reader, .Ltmp10-cpp_create_reader
	.cfi_endproc

	.globl	cpp_destroy
	.align	16, 0x90
	.type	cpp_destroy,@function
cpp_destroy:                            # @cpp_destroy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp16:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp17:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp18:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 48
.Ltmp21:
	.cfi_offset %rbx, -40
.Ltmp22:
	.cfi_offset %r12, -32
.Ltmp23:
	.cfi_offset %r14, -24
.Ltmp24:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	jmp	.LBB1_2
	.align	16, 0x90
.LBB1_1:                                # %while.body
                                        #   in Loop: Header=BB1_2 Depth=1
	movq	%r14, %rdi
	callq	_cpp_pop_buffer
.LBB1_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%r14)
	jne	.LBB1_1
# BB#3:                                 # %while.end
	movq	264(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB1_5
# BB#4:                                 # %if.then
	callq	free
	movq	$0, 264(%r14)
	xorq	%rax, %rax
	movl	%eax, 272(%r14)
.LBB1_5:                                # %for.body.lr.ph
	movq	392(%r14), %rdi
	callq	deps_free
	leaq	488(%r14), %rdi
	xorq	%rsi, %rsi
	callq	obstack_free
	movq	%r14, %rdi
	callq	_cpp_destroy_hashtable
	movq	%r14, %rdi
	callq	_cpp_cleanup_includes
	movq	72(%r14), %rdi
	callq	_cpp_free_buff
	movq	80(%r14), %rdi
	callq	_cpp_free_buff
	movq	88(%r14), %rdi
	callq	_cpp_free_buff
	leaq	200(%r14), %r15
	movq	%r15, %rbx
	.align	16, 0x90
.LBB1_6:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %r12
	movq	16(%rbx), %rdi
	callq	free
	cmpq	%r15, %rbx
	je	.LBB1_7
# BB#13:                                # %if.then10
                                        #   in Loop: Header=BB1_6 Depth=1
	movq	%rbx, %rdi
	callq	free
.LBB1_7:                                # %for.cond.backedge
                                        #   in Loop: Header=BB1_6 Depth=1
	testq	%r12, %r12
	movq	%r12, %rbx
	jne	.LBB1_6
# BB#8:                                 # %for.end
	movq	688(%r14), %rbx
	testq	%rbx, %rbx
	je	.LBB1_10
	.align	16, 0x90
.LBB1_9:                                # %for.body14
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %r15
	movq	8(%rbx), %rdi
	callq	free
	movq	%rbx, %rdi
	callq	free
	testq	%r15, %r15
	movq	%r15, %rbx
	jne	.LBB1_9
.LBB1_10:                               # %for.end17
	movq	96(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB1_12
	.align	16, 0x90
.LBB1_11:                               # %for.body21
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB1_11
.LBB1_12:                               # %for.end24
	leaq	24(%r14), %rdi
	callq	free_line_maps
	movl	248(%r14), %ebx
	movq	%r14, %rdi
	callq	free
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp25:
	.size	cpp_destroy, .Ltmp25-cpp_destroy
	.cfi_endproc

	.globl	cpp_read_main_file
	.align	16, 0x90
	.type	cpp_read_main_file,@function
cpp_read_main_file:                     # @cpp_read_main_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp32:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp33:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp34:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp35:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp36:
	.cfi_def_cfa_offset 48
.Ltmp37:
	.cfi_offset %rbx, -48
.Ltmp38:
	.cfi_offset %r12, -40
.Ltmp39:
	.cfi_offset %r13, -32
.Ltmp40:
	.cfi_offset %r14, -24
.Ltmp41:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r12
                                        # kill: RDI<def> R12<kill>
	movq	%rdx, %rsi
	callq	_cpp_init_hashtable
	movzbl	772(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_2
# BB#1:                                 # %if.then
	movq	%r12, %rdi
	callq	init_standard_includes
.LBB2_2:                                # %if.end
	movq	672(%r12), %rax
	movq	16(%rax), %r15
	movq	32(%rax), %rbx
	movq	48(%rax), %rcx
	movq	64(%rax), %rdx
	testq	%rcx, %rcx
	je	.LBB2_3
# BB#4:                                 # %if.then.i
	movq	56(%rax), %rsi
	movq	%rdx, (%rsi)
	jmp	.LBB2_5
.LBB2_3:
	movq	%rdx, %rcx
.LBB2_5:                                # %if.end.i
	testq	%rbx, %rbx
	je	.LBB2_6
# BB#7:                                 # %if.then3.i
	movq	40(%rax), %rax
	movq	%rcx, (%rax)
	jmp	.LBB2_8
.LBB2_6:
	movq	%rcx, %rbx
.LBB2_8:                                # %if.end6.i
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	remove_dup_dirs
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	remove_dup_dirs
	movq	%rax, %r13
	testq	%r15, %r15
	je	.LBB2_9
# BB#10:                                # %if.then9.i
	movq	%rbx, (%r13)
	testq	%rbx, %rbx
	je	.LBB2_11
# BB#12:                                # %land.lhs.true.i
	movq	24(%r13), %rax
	movq	24(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_13
# BB#14:                                # %land.lhs.true13.i
	movq	32(%r13), %rax
	movq	32(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB2_15
# BB#16:                                # %if.then16.i
	movzbl	740(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_17
.LBB2_18:                               # %remove_dup_dir.exit.i
	movq	(%rbx), %rax
	movq	%rax, (%r13)
	movq	8(%rbx), %rdi
	callq	free
	movq	%rbx, %rdi
	callq	free
	jmp	.LBB2_19
.LBB2_9:
	movq	%rbx, %r13
	movq	%rbx, %r15
	jmp	.LBB2_19
.LBB2_11:
	xorq	%r13, %r13
	jmp	.LBB2_19
.LBB2_13:
	movq	%rbx, %r13
	jmp	.LBB2_19
.LBB2_15:
	movq	%rbx, %r13
.LBB2_19:                               # %merge_include_chains.exit
	movq	%r15, 688(%r12)
	movq	%r13, 696(%r12)
	movzbl	740(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_20
.LBB2_26:                               # %if.end12
	movzbl	749(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_28
# BB#27:                                # %if.then15
	movq	392(%r12), %rdi
	movq	%r14, %rsi
	callq	deps_add_default_target
.LBB2_28:                               # %if.end16
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	_cpp_read_file
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB2_38
# BB#29:                                # %if.end19
	movzbl	753(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_30
# BB#31:                                # %if.end19
	movabsq	$1, %rax
	jmp	.LBB2_32
.LBB2_30:
	movabsq	$0, %rax
.LBB2_32:                               # %if.end19
	movb	%al, 48(%r12)
	movzbl	771(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_37
# BB#33:                                # %if.then24
	movq	%r12, %rdi
	callq	_cpp_lex_direct
	movq	%rax, %rbx
	movzbl	6(%rbx), %eax
	cmpq	$40, %rax
	jne	.LBB2_36
# BB#34:                                # %if.then.i46
	movq	%r12, %rdi
	callq	_cpp_lex_direct
	movq	%rax, %r14
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	_cpp_backup_tokens
	movzbl	6(%r14), %eax
	cmpq	$57, %rax
	jne	.LBB2_36
# BB#35:                                # %if.then6.i
	movzbl	7(%rbx), %esi
	andq	$1, %rsi
	movq	%r12, %rdi
	callq	_cpp_handle_directive
	jmp	.LBB2_37
.LBB2_36:                               # %if.end8.i
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	_cpp_backup_tokens
.LBB2_37:                               # %if.end25
	movq	56(%r12), %rax
	movq	(%rax), %rax
.LBB2_38:                               # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_20:                               # %if.then3
	movq	stderr(%rip), %rbx
	movabsq	$.L.str, %rdi
	movq	$35, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	movq	688(%r12), %r15
	testq	%r15, %r15
	je	.LBB2_25
# BB#21:
	movabsq	$.L.str2, %r13
	.align	16, 0x90
.LBB2_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	696(%r12), %rax
	cmpq	%rax, %r15
	jne	.LBB2_24
# BB#23:                                # %if.then7
                                        #   in Loop: Header=BB2_22 Depth=1
	movq	$35, %rsi
	movq	$1, %rdx
	movabsq	$.L.str1, %rdi
	movq	%rbx, %rcx
	callq	fwrite
.LBB2_24:                               # %if.end9
                                        #   in Loop: Header=BB2_22 Depth=1
	movq	8(%r15), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	fprintf
	movq	(%r15), %r15
	testq	%r15, %r15
	jne	.LBB2_22
.LBB2_25:                               # %for.end
	movabsq	$.L.str3, %rdi
	movq	$20, %rsi
	movq	$1, %rdx
	movq	%rbx, %rcx
	callq	fwrite
	jmp	.LBB2_26
.LBB2_17:                               # %if.then.i.i
	movq	stderr(%rip), %rdi
	movq	8(%rbx), %rdx
	movabsq	$.L.str165, %rsi
	xorq	%rax, %rax
	callq	fprintf
	jmp	.LBB2_18
.Ltmp42:
	.size	cpp_read_main_file, .Ltmp42-cpp_read_main_file
	.cfi_endproc

	.align	16, 0x90
	.type	init_standard_includes,@function
init_standard_includes:                 # @init_standard_includes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp46:
	.cfi_def_cfa_offset 16
.Ltmp47:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp48:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp49:
	.cfi_offset %rbx, -56
.Ltmp50:
	.cfi_offset %r12, -48
.Ltmp51:
	.cfi_offset %r13, -40
.Ltmp52:
	.cfi_offset %r14, -32
.Ltmp53:
	.cfi_offset %r15, -24
	movq	%rdi, -48(%rbp)         # 8-byte Spill
	movq	712(%rdi), %rax
	movq	%rax, -64(%rbp)         # 8-byte Spill
	movabsq	$.L.str169, %rdi
	callq	getenv
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB3_11
# BB#1:                                 # %land.lhs.true
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB3_11
# BB#2:
	movq	%r14, %rbx
	.align	16, 0x90
.LBB3_3:                                # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_4 Depth 2
	xorq	%r15, %r15
	jmp	.LBB3_4
	.align	16, 0x90
.LBB3_6:                                # %while.body.i
                                        #   in Loop: Header=BB3_4 Depth=2
	incq	%r15
.LBB3_4:                                # %while.cond.i
                                        #   Parent Loop BB3_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx,%r15), %eax
	testq	%rax, %rax
	je	.LBB3_7
# BB#5:                                 # %while.cond.i
                                        #   in Loop: Header=BB3_4 Depth=2
	cmpq	$58, %rax
	jne	.LBB3_6
	.align	16, 0x90
.LBB3_7:                                # %while.end.i
                                        #   in Loop: Header=BB3_3 Depth=1
	testq	%r15, %r15
	jne	.LBB3_9
# BB#8:                                 # %if.then.i
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	$2, %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	$46, %rax
	movw	%ax, (%r12)
	jmp	.LBB3_10
	.align	16, 0x90
.LBB3_9:                                # %if.else.i
                                        #   in Loop: Header=BB3_3 Depth=1
	leaq	(%rbx,%r15), %r13
	subq	%rbx, %r13
	leaq	1(%r13), %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	memcpy
	xorq	%rax, %rax
	movb	%al, (%r12,%r13)
.LBB3_10:                               # %if.end.i
                                        #   in Loop: Header=BB3_3 Depth=1
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%r12, %rsi
	callq	append_include_chain
	movzbl	(%rbx,%r15), %eax
	testq	%rax, %rax
	leaq	1(%rbx,%r15), %rbx
	jne	.LBB3_3
.LBB3_11:                               # %if.end
	movq	-48(%rbp), %rcx         # 8-byte Reload
	movzbl	744(%rcx), %eax
	movzbl	742(%rcx), %ecx
	leaq	(%rcx,%rax,2), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	ja	.LBB3_18
# BB#12:                                # %if.end
	jmpq	*.LJTI3_0(,%rax,8)
.LBB3_13:                               # %do.body7
	movabsq	$.L.str170, %rdi
	jmp	.LBB3_17
.LBB3_14:                               # %do.body11
	movabsq	$.L.str171, %rdi
	jmp	.LBB3_17
.LBB3_15:                               # %do.body15
	movabsq	$.L.str172, %rdi
	jmp	.LBB3_17
.LBB3_16:                               # %do.body19
	movabsq	$.L.str173, %rdi
.LBB3_17:                               # %sw.epilog
	callq	getenv
	movq	%rax, %r14
.LBB3_18:                               # %sw.epilog
	testq	%r14, %r14
	je	.LBB3_29
# BB#19:                                # %land.lhs.true24
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB3_29
# BB#20:
	xorq	%r13, %r13
	.align	16, 0x90
.LBB3_21:                               # %do.body.i144
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_22 Depth 2
	xorq	%rbx, %rbx
	jmp	.LBB3_22
	.align	16, 0x90
.LBB3_24:                               # %while.body.i148
                                        #   in Loop: Header=BB3_22 Depth=2
	incq	%rbx
.LBB3_22:                               # %while.cond.i146
                                        #   Parent Loop BB3_21 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r14,%rbx), %eax
	testq	%rax, %rax
	je	.LBB3_25
# BB#23:                                # %while.cond.i146
                                        #   in Loop: Header=BB3_22 Depth=2
	cmpq	$58, %rax
	jne	.LBB3_24
	.align	16, 0x90
.LBB3_25:                               # %while.end.i150
                                        #   in Loop: Header=BB3_21 Depth=1
	testq	%rbx, %rbx
	jne	.LBB3_27
# BB#26:                                # %if.then.i153
                                        #   in Loop: Header=BB3_21 Depth=1
	movq	$2, %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	$46, %rax
	movw	%ax, (%r15)
	jmp	.LBB3_28
	.align	16, 0x90
.LBB3_27:                               # %if.else.i160
                                        #   in Loop: Header=BB3_21 Depth=1
	leaq	(%r14,%rbx), %r12
	subq	%r14, %r12
	leaq	1(%r12), %rdi
	callq	xmalloc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	memcpy
	movb	%r13b, (%r15,%r12)
.LBB3_28:                               # %if.end.i164
                                        #   in Loop: Header=BB3_21 Depth=1
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%r15, %rsi
	callq	append_include_chain
	movzbl	(%r14,%rbx), %eax
	testq	%rax, %rax
	leaq	1(%r14,%rbx), %r14
	jne	.LBB3_21
.LBB3_29:                               # %if.end29
	cmpq	$0, -64(%rbp)           # 8-byte Folded Reload
	je	.LBB3_31
# BB#30:                                # %if.end29
	movq	cpp_GCC_INCLUDE_DIR_len(%rip), %r12
	testq	%r12, %r12
	je	.LBB3_31
# BB#40:                                # %if.then33
	movabsq	$4294967296, %rax       # imm = 0x100000000
	movq	$32, %rcx
	shlq	%cl, %r12
	leaq	(%r12,%rax), %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	addq	$15, %rax
	andq	$-16, %rax
	movq	%rsp, %r13
	subq	%rax, %r13
	movq	%r13, -72(%rbp)         # 8-byte Spill
	movq	%r13, %rsp
	movq	-64(%rbp), %rdi         # 8-byte Reload
	callq	strlen
	movq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	%r12, -56(%rbp)         # 8-byte Spill
	movabsq	$cpp_GCC_INCLUDE_DIR, %rsi
	movq	%r13, %rdi
	movq	%r12, %rdx
	callq	memcpy
	movq	cpp_include_defaults(%rip), %r15
	xorq	%rax, %rax
	movb	%al, (%r12,%r13)
	testq	%r15, %r15
	je	.LBB3_39
# BB#41:                                # %for.body.lr.ph
	movq	%r14, %rax
	movq	cpp_GCC_INCLUDE_DIR_len(%rip), %rcx
	subq	%rcx, %rax
	movq	%rax, -88(%rbp)         # 8-byte Spill
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, -80(%rbp)         # 8-byte Spill
	movabsq	$cpp_include_defaults+24, %r12
	movq	%r15, %r14
	.align	16, 0x90
.LBB3_42:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movl	-8(%r12), %eax
	testq	%rax, %rax
	je	.LBB3_45
# BB#43:                                # %lor.lhs.false
                                        #   in Loop: Header=BB3_42 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	movzbl	742(%rax), %eax
	testq	%rax, %rax
	je	.LBB3_47
# BB#44:                                # %land.lhs.true47
                                        #   in Loop: Header=BB3_42 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	movzbl	773(%rax), %eax
	testq	%rax, %rax
	jne	.LBB3_47
	.align	16, 0x90
.LBB3_45:                               # %if.then50
                                        #   in Loop: Header=BB3_42 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	movq	-56(%rbp), %rdx         # 8-byte Reload
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB3_47
# BB#46:                                # %if.then55
                                        #   in Loop: Header=BB3_42 Depth=1
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %r14
	movq	-88(%rbp), %rax         # 8-byte Reload
	leaq	(%rax,%r14), %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movabsq	$4294967296, %rax       # imm = 0x100000000
	addq	%rax, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	movq	%rax, %rbx
	movq	-64(%rbp), %rsi         # 8-byte Reload
	movq	-80(%rbp), %r13         # 8-byte Reload
	movq	%r13, %rdx
	callq	memcpy
	leaq	(%rbx,%r13), %rdi
	movq	-24(%r12), %rsi
	movq	-56(%rbp), %rax         # 8-byte Reload
	addq	%rax, %rsi
	movq	cpp_GCC_INCLUDE_DIR_len(%rip), %rax
	subq	%rax, %r14
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$4294967296, %rax       # imm = 0x100000000
	leaq	(%r14,%rax), %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	callq	memcpy
	movl	-4(%r12), %ecx
	movq	$1, %rdx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	movq	-72(%rbp), %r13         # 8-byte Reload
	callq	append_include_chain
.LBB3_47:                               # %for.inc
                                        #   in Loop: Header=BB3_42 Depth=1
	movq	(%r12), %r14
	addq	$24, %r12
	testq	%r14, %r14
	jne	.LBB3_42
	jmp	.LBB3_32
.LBB3_31:                               # %if.end29.for.cond73.preheader_crit_edge
	movq	cpp_include_defaults(%rip), %r15
.LBB3_32:                               # %for.cond73.preheader
	testq	%r15, %r15
	je	.LBB3_39
# BB#33:                                # %for.body76.lr.ph
	movabsq	$cpp_include_defaults+24, %rbx
	.align	16, 0x90
.LBB3_34:                               # %for.body76
                                        # =>This Inner Loop Header: Depth=1
	movl	-8(%rbx), %eax
	testq	%rax, %rax
	je	.LBB3_37
# BB#35:                                # %lor.lhs.false79
                                        #   in Loop: Header=BB3_34 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	movzbl	742(%rax), %eax
	testq	%rax, %rax
	je	.LBB3_38
# BB#36:                                # %land.lhs.true84
                                        #   in Loop: Header=BB3_34 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	movzbl	773(%rax), %eax
	testq	%rax, %rax
	jne	.LBB3_38
	.align	16, 0x90
.LBB3_37:                               # %if.then88
                                        #   in Loop: Header=BB3_34 Depth=1
	movq	-16(%rbx), %rsi
	movq	%r15, %rdi
	callq	update_path
	movl	-4(%rbx), %ecx
	movq	$1, %rdx
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rax, %rsi
	callq	append_include_chain
.LBB3_38:                               # %for.inc94
                                        #   in Loop: Header=BB3_34 Depth=1
	movq	(%rbx), %r15
	addq	$24, %rbx
	testq	%r15, %r15
	jne	.LBB3_34
.LBB3_39:                               # %for.end96
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp54:
	.size	init_standard_includes, .Ltmp54-init_standard_includes
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI3_0:
	.quad	.LBB3_13
	.quad	.LBB3_14
	.quad	.LBB3_15
	.quad	.LBB3_16

	.text
	.globl	cpp_finish_options
	.align	16, 0x90
	.type	cpp_finish_options,@function
cpp_finish_options:                     # @cpp_finish_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp61:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp62:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp63:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp64:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp65:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp66:
	.cfi_def_cfa_offset 80
.Ltmp67:
	.cfi_offset %rbx, -48
.Ltmp68:
	.cfi_offset %r12, -40
.Ltmp69:
	.cfi_offset %r13, -32
.Ltmp70:
	.cfi_offset %r14, -24
.Ltmp71:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	742(%r14), %eax
	testq	%rax, %rax
	je	.LBB4_4
# BB#1:                                 # %land.lhs.true
	movzbl	777(%r14), %eax
	testq	%rax, %rax
	je	.LBB4_4
# BB#2:
	movabsq	$operator_array, %rbx
	movabsq	$operator_array+176, %r15
	.align	16, 0x90
.LBB4_3:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	movl	8(%rbx), %edx
	movq	%r14, %rdi
	callq	cpp_lookup
	movzbl	21(%rax), %ecx
	orq	$1, %rcx
	movb	%cl, 21(%rax)
	movl	12(%rbx), %ecx
	movl	%ecx, 24(%rax)
	addq	$16, %rbx
	cmpq	%r15, %rbx
	jb	.LBB4_3
.LBB4_4:                                # %if.end
	movzbl	771(%r14), %eax
	testq	%rax, %rax
	je	.LBB4_6
# BB#5:                                 # %if.end.if.end18_crit_edge
	movq	672(%r14), %rax
	jmp	.LBB4_14
.LBB4_6:                                # %if.then6
	movabsq	$.L.str4, %rdx
	movq	$2, %rsi
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	_cpp_do_file_change
	movq	%r14, %rdi
	callq	init_builtins
	movabsq	$.L.str5, %rdx
	movq	$2, %rsi
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	_cpp_do_file_change
	movq	672(%r14), %rax
	movq	(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB4_9
	.align	16, 0x90
.LBB4_7:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movq	%r14, %rdi
	callq	*16(%rbx)
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_7
# BB#8:                                 # %for.cond.while.cond.preheader_crit_edge
	movq	672(%r14), %rax
.LBB4_9:                                # %while.cond.preheader
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB4_14
# BB#10:
	movq	$61, %r12
	leaq	8(%rsp), %r15
	movq	$1, %r13
	.align	16, 0x90
.LBB4_11:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movb	%r12b, 14(%rsp)
	movq	%rdi, 24(%rsp)
	callq	strlen
	movl	%eax, 16(%rsp)
	movl	64(%r14), %eax
	incq	%rax
	movl	%eax, 64(%r14)
	movq	$3, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	_cpp_execute_include
	testq	%rax, %rax
	je	.LBB4_13
# BB#12:                                # %if.then12
                                        #   in Loop: Header=BB4_11 Depth=1
	movq	(%r14), %rax
	movb	%r13b, 83(%rax)
	movq	%r14, %rdi
	callq	cpp_scan_nooutput
.LBB4_13:                               # %if.end13
                                        #   in Loop: Header=BB4_11 Depth=1
	movq	(%rbx), %rax
	movq	672(%r14), %rcx
	movq	%rax, 80(%rcx)
	movq	%rbx, %rdi
	callq	free
	movq	672(%r14), %rax
	movq	80(%rax), %rbx
	testq	%rbx, %rbx
	jne	.LBB4_11
.LBB4_14:                               # %if.end18
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB4_16
	.align	16, 0x90
.LBB4_15:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rbx
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB4_15
.LBB4_16:                               # %free_chain.exit
	movq	%r14, %rdi
	callq	_cpp_push_next_buffer
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	cpp_finish_options, .Ltmp72-cpp_finish_options
	.cfi_endproc

	.align	16, 0x90
	.type	init_builtins,@function
init_builtins:                          # @init_builtins
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp76:
	.cfi_def_cfa_offset 16
.Ltmp77:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp78:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp79:
	.cfi_offset %rbx, -56
.Ltmp80:
	.cfi_offset %r12, -48
.Ltmp81:
	.cfi_offset %r13, -40
.Ltmp82:
	.cfi_offset %r14, -32
.Ltmp83:
	.cfi_offset %r15, -24
	movq	%rdi, %r13
	leaq	728(%r13), %rax
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movabsq	$builtin_array, %r12
	movabsq	$builtin_array+336, %r14
	.align	16, 0x90
.LBB5_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	18(%r12), %eax
	testq	$8, %rax
	jne	.LBB5_2
# BB#3:                                 # %if.else
                                        #   in Loop: Header=BB5_1 Depth=1
	testq	$1, %rax
	jne	.LBB5_4
# BB#5:                                 # %if.else18
                                        #   in Loop: Header=BB5_1 Depth=1
	testq	$2, %rax
	je	.LBB5_6
# BB#7:                                 # %if.else18
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	-48(%rbp), %rax         # 8-byte Reload
	jmp	.LBB5_8
	.align	16, 0x90
.LBB5_2:                                # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%r12), %rsi
	movzbl	20(%r12), %edx
	movq	%r13, %rdi
	callq	cpp_lookup
	movq	$1, %rcx
	movb	%cl, 20(%rax)
	movzbl	21(%rax), %ecx
	orq	$20, %rcx
	movb	%cl, 21(%rax)
	movzbl	16(%r12), %ecx
	movl	%ecx, 24(%rax)
	jmp	.LBB5_10
	.align	16, 0x90
.LBB5_4:                                # %if.then11
                                        #   in Loop: Header=BB5_1 Depth=1
	movzbl	20(%r12), %ebx
	movq	version_string(%rip), %r15
	movq	%r15, %rdi
	callq	strlen
	leaq	20(%rbx,%rax), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	(%r12), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str127, %rsi
	jmp	.LBB5_9
.LBB5_6:                                #   in Loop: Header=BB5_1 Depth=1
	leaq	8(%r12), %rax
.LBB5_8:                                # %if.else18
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	(%rax), %r15
	movzbl	20(%r12), %ebx
	movq	%r15, %rdi
	callq	strlen
	leaq	18(%rax,%rbx), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	(%r12), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str128, %rsi
.LBB5_9:                                # %if.end33
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%r15, %rcx
	callq	sprintf
	movq	%r13, %rdi
	movq	%rbx, %rsi
	callq	_cpp_define_builtin
.LBB5_10:                               # %for.inc
                                        #   in Loop: Header=BB5_1 Depth=1
	addq	$24, %r12
	cmpq	%r14, %r12
	jb	.LBB5_1
# BB#11:                                # %for.end
	movzbl	742(%r13), %eax
	testq	%rax, %rax
	je	.LBB5_13
# BB#12:                                # %if.then37
	movabsq	$.L.str129, %rsi
	movq	%r13, %rdi
	callq	_cpp_define_builtin
	movabsq	$.L.str130, %rsi
	movq	%r13, %rdi
	callq	_cpp_define_builtin
.LBB5_13:                               # %if.end38
	movzbl	744(%r13), %eax
	testq	%rax, %rax
	je	.LBB5_15
# BB#14:                                # %if.then41
	movabsq	$.L.str131, %rsi
	movq	%r13, %rdi
	callq	_cpp_define_builtin
.LBB5_15:                               # %if.end42
	movl	736(%r13), %eax
	cmpq	$3, %rax
	jne	.LBB5_17
# BB#16:                                # %if.then46
	movabsq	$.L.str132, %rsi
	jmp	.LBB5_19
.LBB5_17:                               # %if.else47
	movzbl	769(%r13), %eax
	testq	%rax, %rax
	je	.LBB5_20
# BB#18:                                # %if.then50
	movabsq	$.L.str133, %rsi
.LBB5_19:                               # %if.end52
	movq	%r13, %rdi
	callq	_cpp_define_builtin
.LBB5_20:                               # %if.end52
	movzbl	741(%r13), %eax
	testq	%rax, %rax
	jne	.LBB5_22
# BB#21:                                # %if.then57
	movabsq	$.L.str134, %rsi
	movq	%r13, %rdi
	callq	_cpp_define_builtin
.LBB5_22:                               # %if.end58
	movl	736(%r13), %eax
	cmpq	$9, %rax
	je	.LBB5_25
# BB#23:                                # %if.end58
	addq	$-2, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$2, %rcx
	ja	.LBB5_27
# BB#24:                                # %if.then72
	movabsq	$.L.str135, %rsi
	jmp	.LBB5_26
.LBB5_25:                               # %if.then78
	movabsq	$.L.str136, %rsi
.LBB5_26:                               # %if.end80
	movq	%r13, %rdi
	callq	_cpp_define_builtin
.LBB5_27:                               # %if.end80
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp84:
	.size	init_builtins, .Ltmp84-init_builtins
	.cfi_endproc

	.globl	_cpp_push_next_buffer
	.align	16, 0x90
	.type	_cpp_push_next_buffer,@function
_cpp_push_next_buffer:                  # @_cpp_push_next_buffer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp91:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp93:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp94:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp95:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp96:
	.cfi_def_cfa_offset 80
.Ltmp97:
	.cfi_offset %rbx, -48
.Ltmp98:
	.cfi_offset %r12, -40
.Ltmp99:
	.cfi_offset %r13, -32
.Ltmp100:
	.cfi_offset %r14, -24
.Ltmp101:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	672(%r12), %rdi
	xorq	%r14, %r14
	testq	%rdi, %rdi
	je	.LBB6_10
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 80(%rdi)
	jne	.LBB6_10
# BB#2:                                 # %while.cond.preheader
	xorq	%r14, %r14
	movq	$61, %r13
	leaq	8(%rsp), %r15
	jmp	.LBB6_3
	.align	16, 0x90
.LBB6_7:                                # %if.end11.while.body_crit_edge
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	672(%r12), %rdi
.LBB6_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	96(%rdi), %rbx
	testq	%rbx, %rbx
	je	.LBB6_8
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB6_3 Depth=1
	movzbl	771(%r12), %eax
	testq	%rax, %rax
	jne	.LBB6_6
# BB#5:                                 # %if.then10
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	8(%rbx), %rdi
	movb	%r13b, 14(%rsp)
	movq	%rdi, 24(%rsp)
	callq	strlen
	movl	%eax, 16(%rsp)
	movl	64(%r12), %eax
	incq	%rax
	movl	%eax, 64(%r12)
	movq	$3, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	_cpp_execute_include
	movq	672(%r12), %rdi
	movq	%rax, %r14
.LBB6_6:                                # %if.end11
                                        #   in Loop: Header=BB6_3 Depth=1
	movq	(%rbx), %rax
	movq	%rax, 96(%rdi)
	movq	%rbx, %rdi
	callq	free
	testq	$1, %r14
	je	.LBB6_7
	jmp	.LBB6_10
.LBB6_8:                                # %if.then16
	callq	free
	movq	$0, 672(%r12)
	movzbl	771(%r12), %eax
	testq	%rax, %rax
	jne	.LBB6_10
# BB#9:                                 # %if.then24
	movq	24(%r12), %rax
	movq	(%rax), %rdx
	movq	$2, %rsi
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r12, %rdi
	callq	_cpp_do_file_change
.LBB6_10:                               # %if.end27
	andq	$1, %r14
	movq	%r14, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp102:
	.size	_cpp_push_next_buffer, .Ltmp102-_cpp_push_next_buffer
	.cfi_endproc

	.globl	cpp_finish
	.align	16, 0x90
	.type	cpp_finish,@function
cpp_finish:                             # @cpp_finish
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp107:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp108:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp109:
	.cfi_def_cfa_offset 32
.Ltmp110:
	.cfi_offset %rbx, -32
.Ltmp111:
	.cfi_offset %r14, -24
.Ltmp112:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	jmp	.LBB7_2
	.align	16, 0x90
.LBB7_1:                                # %while.body
                                        #   in Loop: Header=BB7_2 Depth=1
	movq	%rbx, %rdi
	callq	_cpp_pop_buffer
.LBB7_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	cmpq	$0, (%rbx)
	jne	.LBB7_1
# BB#3:                                 # %while.end
	movzbl	749(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_18
# BB#4:                                 # %land.lhs.true
	movl	248(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB7_18
# BB#5:                                 # %if.then
	movq	680(%rbx), %rdi
	movzbl	(%rdi), %eax
	testq	%rax, %rax
	je	.LBB7_6
# BB#13:                                # %if.else.i
	movzbl	752(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_15
# BB#14:
	movabsq	$.L.str125, %rsi
	jmp	.LBB7_16
.LBB7_6:                                # %if.then.i
	movq	stdout(%rip), %r14
	jmp	.LBB7_7
.LBB7_15:                               # %select.mid
	movabsq	$.L.str124, %rsi
.LBB7_16:                               # %select.end
	callq	fopen
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB7_17
.LBB7_7:                                # %if.end11.i
	movq	392(%rbx), %rdi
	movq	$72, %rdx
	movq	%r14, %rsi
	callq	deps_write
	movzbl	750(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_9
# BB#8:                                 # %if.then14.i
	movq	392(%rbx), %rdi
	movq	%r14, %rsi
	callq	deps_phony_targets
.LBB7_9:                                # %if.end16.i
	movq	stdout(%rip), %rax
	cmpq	%rax, %r14
	je	.LBB7_18
# BB#10:                                # %if.then19.i
	movq	%r14, %rdi
	callq	ferror
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rax
	jne	.LBB7_12
# BB#11:                                # %lor.lhs.false.i
	movq	%r14, %rdi
	callq	fclose
	testq	%r15, %rax
	je	.LBB7_18
.LBB7_12:                               # %if.then25.i
	movabsq	$.L.str126, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_fatal
	jmp	.LBB7_18
.LBB7_17:                               # %if.then8.i
	movq	680(%rbx), %rsi
	movq	%rbx, %rdi
	callq	cpp_notice_from_errno
.LBB7_18:                               # %if.end
	movzbl	753(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_19
# BB#20:                                # %if.then5
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	_cpp_report_missing_guards  # TAILCALL
.LBB7_19:                               # %if.end6
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp113:
	.size	cpp_finish, .Ltmp113-cpp_finish
	.cfi_endproc

	.globl	cpp_handle_option
	.align	16, 0x90
	.type	cpp_handle_option,@function
cpp_handle_option:                      # @cpp_handle_option
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp120:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp121:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp122:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp123:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp124:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp125:
	.cfi_def_cfa_offset 112
.Ltmp126:
	.cfi_offset %rbx, -48
.Ltmp127:
	.cfi_offset %r12, -40
.Ltmp128:
	.cfi_offset %r13, -32
.Ltmp129:
	.cfi_offset %r14, -24
.Ltmp130:
	.cfi_offset %r15, -16
	movq	(%rdx), %rbx
	movzbl	(%rbx), %eax
	cmpq	$45, %rax
	jne	.LBB8_7
# BB#1:                                 # %lor.lhs.false
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB8_7
# BB#2:
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	movq	672(%rdi), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	leaq	1(%rbx), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	$68, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	jmp	.LBB8_3
.LBB8_7:                                # %if.then
	cmpq	$0, 648(%rdi)
	je	.LBB8_8
# BB#9:                                 # %if.else
	cmpq	$0, 656(%rdi)
	je	.LBB8_10
# BB#11:                                # %if.else25
	movq	progname(%rip), %rdx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	cpp_fatal
	jmp	.LBB8_199
	.align	16, 0x90
.LBB8_6:                                # %if.then.i
                                        #   in Loop: Header=BB8_3 Depth=1
	leaq	1(%r12), %r15
	movq	%r14, %rdx
.LBB8_3:                                # %while.cond.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_4 Depth 2
	movq	%rbx, %r12
	.align	16, 0x90
.LBB8_4:                                # %while.cond.i
                                        #   Parent Loop BB8_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r12, %rbx
	movq	%r15, %rax
	andq	%rdx, %rax
	movq	%rbx, %rcx
	andq	%rdx, %rcx
	xorq	%r13, %r13
	cmpq	%rax, %rcx
	jbe	.LBB8_200
# BB#5:                                 # %while.body.i
                                        #   in Loop: Header=BB8_4 Depth=2
	movq	%rdx, %r14
	leaq	(%rbx,%r15), %r12
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %r12
	movq	$1, %rcx
	shrq	%cl, %r12
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	cl_options+16(%rax), %r13
	movq	cl_options(%rax), %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rdx
	callq	memcmp
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB8_6
# BB#12:                                # %if.else.i
                                        #   in Loop: Header=BB8_4 Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movq	%r14, %rdx
	js	.LBB8_4
# BB#13:                                # %if.else8.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	1(%r13,%rax), %eax
	testq	%rax, %rax
	je	.LBB8_23
# BB#14:                                # %if.end.i
                                        #   in Loop: Header=BB8_3 Depth=1
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	cmpq	$0, cl_options+8(%rax)
	leaq	1(%r12), %r15
	je	.LBB8_3
# BB#15:                                # %for.cond.preheader.i
	movq	%r15, %rax
	andq	%rdx, %rax
	cmpq	$67, %rax
	ja	.LBB8_23
.LBB8_16:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r15, %rbx
	andq	%rdx, %rbx
	movq	$5, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	cl_options+16(%rax), %r13
	movq	cl_options(%rax), %rsi
	movq	48(%rsp), %rdi          # 8-byte Reload
	movq	%rdx, %r14
	movq	%r13, %rdx
	callq	memcmp
	movq	%r14, %rdx
	testq	%rdx, %rax
	jne	.LBB8_22
# BB#17:                                # %if.end28.i
                                        #   in Loop: Header=BB8_16 Depth=1
	movq	40(%rsp), %rax          # 8-byte Reload
	movzbl	1(%r13,%rax), %eax
	testq	%rax, %rax
	je	.LBB8_18
# BB#19:                                # %if.end34.i
                                        #   in Loop: Header=BB8_16 Depth=1
	movq	$5, %rcx
	shlq	%cl, %rbx
	cmpq	$0, cl_options+8(%rbx)
	je	.LBB8_21
# BB#20:                                # %select.mid
                                        #   in Loop: Header=BB8_16 Depth=1
	movq	%r15, %r12
.LBB8_21:                               # %select.end
                                        #   in Loop: Header=BB8_16 Depth=1
	incq	%r15
	movq	%r15, %rax
	andq	%rdx, %rax
	cmpq	$68, %rax
	jb	.LBB8_16
	jmp	.LBB8_22
.LBB8_8:                                # %if.then12
	movq	%rbx, 648(%rdi)
	xorq	%r13, %r13
	jmp	.LBB8_199
.LBB8_10:                               # %if.then20
	movq	%rbx, 656(%rdi)
	xorq	%r13, %r13
.LBB8_199:                              # %if.end465
	incq	%r13
.LBB8_200:                              # %return
	movq	%r13, %rax
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_18:
	movq	%r15, %r12
.LBB8_22:                               # %parse_option.exit
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	movabsq	$0, %r13
	js	.LBB8_200
.LBB8_23:                               # %if.end34
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	$5, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	cl_options+8(%rax), %rsi
	movl	cl_options+24(%rax), %r13d
	xorq	%r15, %r15
	testq	%rsi, %rsi
	je	.LBB8_24
# BB#27:                                # %if.then40
	movq	$5, %rcx
	shlq	%cl, %r12
	movq	cl_options+16(%r12), %rax
	movq	40(%rsp), %rbx          # 8-byte Reload
	movzbl	1(%rax,%rbx), %ecx
	leaq	1(%rax,%rbx), %r12
	testq	%rcx, %rcx
	jne	.LBB8_28
# BB#29:                                # %if.then40
	movq	%r13, %rax
	andq	%rdx, %rax
	cmpq	$21, %rax
	je	.LBB8_30
# BB#31:                                # %if.then52
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	8(%rax), %r14
	testq	%r14, %r14
	movq	$1, %r15
	movq	%r14, %r12
	jne	.LBB8_25
# BB#32:                                # %if.then56
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rdx
	callq	cpp_fatal
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB8_200
.LBB8_24:
	movq	40(%rsp), %r14          # 8-byte Reload
	xorq	%r12, %r12
	jmp	.LBB8_25
.LBB8_28:
	movq	%rbx, %r14
	jmp	.LBB8_25
.LBB8_30:
	movq	%rbx, %r14
.LBB8_25:                               # %if.end64
	andq	%rdx, %r13
	cmpq	$67, %r13
	jbe	.LBB8_33
# BB#26:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_33:                               # %if.end64
	movq	%rdx, %rbx
	jmpq	*.LJTI8_0(,%r13,8)
.LBB8_93:                               # %sw.bb158
	movq	$4, %rax
	movq	32(%rsp), %rsi          # 8-byte Reload
	movl	%eax, 736(%rsi)
	movq	$1, %rax
	movb	%al, 769(%rsi)
	xorq	%rcx, %rcx
	movb	%cl, 744(%rsi)
	movq	$256, %rdx              # imm = 0x100
	movw	%dx, 742(%rsi)
	movb	%al, 748(%rsi)
	movq	$257, %rax              # imm = 0x101
	movw	%ax, 746(%rsi)
	movb	%cl, 767(%rsi)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_92:                               # %sw.bb157
	movq	$2, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movb	%al, 744(%rdx)
	movw	%ax, 742(%rdx)
	movb	%al, 748(%rdx)
	movq	$1, %rcx
	jmp	.LBB8_82
.LBB8_58:                               # %sw.bb110
	movq	stdout(%rip), %r14
	movabsq	$.L.str32, %rdi
	movq	$458, %rsi              # imm = 0x1CA
	movq	$1, %rdx
	movq	$1, %rbx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str33, %rdi
	movq	$503, %rsi              # imm = 0x1F7
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str34, %rdi
	movq	$331, %rsi              # imm = 0x14B
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str35, %rdi
	movq	$298, %rsi              # imm = 0x12A
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str36, %rdi
	movq	$498, %rsi              # imm = 0x1F2
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str37, %rdi
	movq	$405, %rsi              # imm = 0x195
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str38, %rdi
	movq	$382, %rsi              # imm = 0x17E
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str39, %rdi
	movq	$398, %rsi              # imm = 0x18E
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str40, %rdi
	movq	$155, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str41, %rdi
	movq	$369, %rsi              # imm = 0x171
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str42, %rdi
	movq	$417, %rsi              # imm = 0x1A1
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movabsq	$.L.str43, %rdi
	movq	$445, %rsi              # imm = 0x1BD
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	32(%rsp), %rax          # 8-byte Reload
	movb	%bl, 778(%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_118:                              # %sw.bb202
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	392(%rax), %rdi
	cmpq	$17, %r13
	jne	.LBB8_119
# BB#120:                               # %sw.bb202
	movabsq	$1, %rdx
	jmp	.LBB8_121
.LBB8_141:                              # %sw.bb250
	movq	$24, %rdi
	callq	xmalloc
	movq	%r12, 8(%rax)
	movq	$0, (%rax)
	cmpq	$36, %r13
	jne	.LBB8_145
# BB#142:                               # %do.body
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	$0, 96(%rcx)
	je	.LBB8_143
# BB#144:                               # %if.else259
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	104(%rdx), %rcx
	addq	$104, %rdx
	jmp	.LBB8_73
.LBB8_148:                              # %sw.bb274
	movq	%r12, %rdi
	callq	strlen
	movq	32(%rsp), %rbx          # 8-byte Reload
	cmpq	$0, 712(%rbx)
	movq	%rax, %r14
	je	.LBB8_150
# BB#149:                               # %if.then281
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movl	720(%rbx), %r12d
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	sarq	%cl, %rax
	leaq	1(%rax,%r12), %rdi
	callq	xmalloc
	movq	712(%rbx), %rsi
	movq	%rax, %rbx
	movq	%rbx, 40(%rsp)          # 8-byte Spill
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	memcpy
	addq	%rbx, %r12
	movabsq	$4294967296, %rax       # imm = 0x100000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r12, %rdi
	movq	48(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB8_152
.LBB8_88:                               # %sw.bb148
	xorq	%rax, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	movb	%al, 769(%rdx)
	movb	%al, 744(%rdx)
	movq	$256, %rax              # imm = 0x100
	movw	%ax, 742(%rdx)
	movq	$1, %rcx
	movb	%cl, 748(%rdx)
	movw	%ax, 746(%rdx)
	movb	%cl, 767(%rdx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_89:                               # %sw.bb155
	movq	$1, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	movb	%al, 769(%rdx)
	xorq	%rcx, %rcx
	movb	%cl, 744(%rdx)
	jmp	.LBB8_90
.LBB8_66:                               # %sw.bb127
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 767(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_77:                               # %sw.bb138
	movq	$257, %rax              # imm = 0x101
	movq	32(%rsp), %rcx          # 8-byte Reload
	movw	%ax, 742(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_59:                               # %sw.bb112
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 778(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_60:                               # %sw.bb115
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 778(%rcx)
	jmp	.LBB8_63
.LBB8_125:                              # %sw.bb215
	movzbl	(%r12), %eax
	movq	%r12, %rbx
	cmpq	$45, %rax
	jne	.LBB8_131
# BB#126:                               # %if.then220
	movzbl	1(%rbx), %eax
	testq	%rax, %rax
	je	.LBB8_127
# BB#130:                               # %if.else227
	incq	%rbx
	movq	$24, %rdi
	callq	xmalloc
	movq	%rbx, 8(%rax)
	movq	$0, (%rax)
	movq	$cpp_unassert, 16(%rax)
	jmp	.LBB8_69
.LBB8_64:                               # %sw.bb123
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 745(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_68:                               # %sw.bb131
	movq	$24, %rdi
	callq	xmalloc
	movq	%r12, 8(%rax)
	movq	$0, (%rax)
	movq	$cpp_define, 16(%rax)
	jmp	.LBB8_69
.LBB8_67:                               # %sw.bb129
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 753(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_133:                              # %sw.bb232
	movabsq	$.L.str10, %rsi
	movq	%r12, %rdi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_134
# BB#137:                               # %if.else245
	movq	%r12, %rdi
	callq	xstrdup
	xorq	%rdx, %rdx
	jmp	.LBB8_138
.LBB8_113:                              # %sw.bb190
	movq	$2, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 749(%rcx)
	movq	$1, %rax
	jmp	.LBB8_115
.LBB8_122:                              # %sw.bb205
	movq	$2, %rax
	jmp	.LBB8_123
.LBB8_116:                              # %sw.bb198
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%r12, 680(%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_112:                              # %sw.bb188
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 751(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_114:                              # %sw.bb193
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 749(%rcx)
.LBB8_115:                              # %if.end465
	movb	%al, 763(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_124:                              # %sw.bb210
	movq	$1, %rax
.LBB8_123:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 749(%rcx)
	movq	%r12, 680(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_117:                              # %sw.bb200
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 750(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_65:                               # %sw.bb125
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 765(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_132:                              # %sw.bb231
	movq	$24, %rdi
	callq	xmalloc
	movq	%r12, 8(%rax)
	movq	$0, (%rax)
	movq	$cpp_undef, 16(%rax)
	jmp	.LBB8_69
.LBB8_159:                              # %sw.bb312
	movabsq	$.L.str12, %rsi
	movq	%r14, %rdi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_160
# BB#161:                               # %if.else320
	movabsq	$.L.str13, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_162
# BB#164:                               # %if.else327
	movabsq	$.L.str14, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_165
# BB#167:                               # %if.else335
	movabsq	$.L.str15, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_168
# BB#170:                               # %if.else343
	movabsq	$.L.str16, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_168
# BB#171:                               # %if.else351
	movabsq	$.L.str17, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_172
# BB#174:                               # %if.else358
	movabsq	$.L.str18, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_175
# BB#177:                               # %if.else365
	movabsq	$.L.str19, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_178
# BB#180:                               # %if.else372
	movabsq	$.L.str20, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_181
# BB#183:                               # %if.else379
	movabsq	$.L.str21, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_184
# BB#185:                               # %if.else387
	movabsq	$.L.str22, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_186
# BB#187:                               # %if.else395
	movabsq	$.L.str23, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_188
# BB#189:                               # %if.else403
	movabsq	$.L.str24, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_188
# BB#190:                               # %if.else411
	movabsq	$.L.str25, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_191
# BB#192:                               # %if.else419
	movabsq	$.L.str26, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_193
# BB#194:                               # %if.else427
	movabsq	$.L.str27, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_195
# BB#196:                               # %if.else435
	movabsq	$.L.str28, %rax
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_197
# BB#198:                               # %if.else443
	movq	(%rsp), %rax            # 8-byte Reload
	testq	%rbx, %rax
	movq	%r15, %r13
	jne	.LBB8_199
	jmp	.LBB8_200
.LBB8_34:                               # %while.cond.preheader
	movzbl	(%r12), %edi
	testq	%rdi, %rdi
	je	.LBB8_35
# BB#100:                               # %while.body.lr.ph
	incq	%r12
	movq	$3, %rax
	movq	$1, %rdx
	movq	$2, %rsi
.LBB8_101:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$56, %rcx
	shlq	%cl, %rdi
	movq	$56, %rcx
	sarq	%cl, %rdi
	cmpq	$72, %rdi
	jg	.LBB8_104
# BB#102:                               # %while.body
                                        #   in Loop: Header=BB8_101 Depth=1
	andq	%rbx, %rdi
	cmpq	$68, %rdi
	jne	.LBB8_107
# BB#103:                               # %sw.bb183
                                        #   in Loop: Header=BB8_101 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 774(%rcx)
	jmp	.LBB8_107
.LBB8_104:                              # %while.body
                                        #   in Loop: Header=BB8_101 Depth=1
	andq	%rbx, %rdi
	cmpq	$73, %rdi
	je	.LBB8_111
# BB#105:                               # %while.body
                                        #   in Loop: Header=BB8_101 Depth=1
	cmpq	$77, %rdi
	jne	.LBB8_106
# BB#109:                               # %sw.bb178
                                        #   in Loop: Header=BB8_101 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%dl, 774(%rcx)
	jmp	.LBB8_107
.LBB8_111:                              # %sw.bb186
                                        #   in Loop: Header=BB8_101 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%dl, 775(%rcx)
	jmp	.LBB8_107
.LBB8_106:                              # %while.body
                                        #   in Loop: Header=BB8_101 Depth=1
	cmpq	$78, %rdi
	jne	.LBB8_107
# BB#110:                               # %sw.bb180
                                        #   in Loop: Header=BB8_101 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%sil, 774(%rcx)
.LBB8_107:                              # %while.cond.backedge
                                        #   in Loop: Header=BB8_101 Depth=1
	movq	%r12, %rcx
	movzbl	(%rcx), %edi
	incq	%rcx
	movq	%rcx, %r12
	testq	%rdi, %rdi
	jne	.LBB8_101
# BB#108:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_36:                               # %sw.bb
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	$.L.str7, 728(%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_37:                               # %sw.bb66
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	$.L.str8, 728(%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_38:                               # %sw.bb69
	xorq	%rax, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 777(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_41:                               # %sw.bb73
	xorq	%rax, %rax
	jmp	.LBB8_40
.LBB8_44:                               # %sw.bb78
	xorq	%rax, %rax
	jmp	.LBB8_43
.LBB8_39:                               # %sw.bb71
	movq	$1, %rax
.LBB8_40:                               # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 771(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_42:                               # %sw.bb76
	movq	$1, %rax
.LBB8_43:                               # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 776(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_45:                               # %sw.bb81
	movq	$1, %rax
	jmp	.LBB8_46
.LBB8_48:                               # %sw.bb86
	movzbl	(%r12), %eax
	movq	%r12, %rbx
	testq	%rax, %rax
	je	.LBB8_49
# BB#50:                                # %if.then91
	leaq	56(%rsp), %rax
	movq	$10, %rdx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	strtol
	movq	56(%rsp), %rcx
	movzbl	(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB8_51
# BB#52:                                # %if.then91
	testq	%rax, %rax
	jle	.LBB8_53
# BB#54:                                # %if.then91
	cmpq	$100, %rax
	jg	.LBB8_55
# BB#56:                                # %if.then102
	movq	32(%rsp), %rcx          # 8-byte Reload
	movl	%eax, 664(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_47:                               # %sw.bb83
	xorq	%rax, %rax
.LBB8_46:                               # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 741(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_158:                              # %sw.bb310
	movq	%r12, %rdi
	callq	xstrdup
	movq	$2, %rdx
	jmp	.LBB8_138
.LBB8_79:                               # %sw.bb143
	movq	32(%rsp), %r14          # 8-byte Reload
	movq	%r12, 712(%r14)
	movq	%r12, %rdi
	callq	strlen
	movl	%eax, 720(%r14)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_140:                              # %sw.bb248
	movq	%r12, %rdi
	callq	xstrdup
	movq	$1, %rdx
.LBB8_138:                              # %if.end465
	xorq	%rcx, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	jmp	.LBB8_139
.LBB8_86:                               # %sw.bb152
	movq	$9, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movb	%al, 744(%rdx)
	movq	$256, %rcx              # imm = 0x100
	movw	%cx, 742(%rdx)
	movq	$1, %rcx
	movb	%cl, 748(%rdx)
	movw	%ax, 746(%rdx)
	jmp	.LBB8_83
.LBB8_80:                               # %sw.bb149
	movq	$5, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movb	%al, 744(%rdx)
	movq	$257, %rax              # imm = 0x101
	movw	%ax, 742(%rdx)
	movq	$1, %rax
	jmp	.LBB8_81
.LBB8_84:                               # %sw.bb150
	movq	$7, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movq	$1, %rax
	movb	%al, 744(%rdx)
.LBB8_90:                               # %if.end465
	movq	$256, %rcx              # imm = 0x100
	movw	%cx, 742(%rdx)
	movb	%al, 748(%rdx)
	jmp	.LBB8_82
.LBB8_85:                               # %sw.bb151
	movq	$8, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movq	$1, %rax
	movb	%al, 744(%rdx)
	movq	$257, %rcx              # imm = 0x101
	movw	%cx, 742(%rdx)
.LBB8_81:                               # %if.end465
	movb	%al, 748(%rdx)
	movq	$256, %rcx              # imm = 0x100
	jmp	.LBB8_82
.LBB8_94:                               # %sw.bb159
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 772(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_95:                               # %sw.bb161
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 773(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_96:                               # %sw.bb163
	movq	%r12, %rbx
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	$0, 656(%rax)
	je	.LBB8_97
# BB#98:                                # %if.else171
	movabsq	$.L.str9, %rcx
	jmp	.LBB8_99
.LBB8_74:                               # %sw.bb132
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 754(%rcx)
.LBB8_75:                               # %sw.bb134
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 770(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_78:                               # %sw.bb141
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 764(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_87:                               # %sw.bb153
	movq	$6, %rax
	movq	32(%rsp), %rsi          # 8-byte Reload
	movl	%eax, 736(%rsi)
	xorq	%rax, %rax
	movb	%al, 769(%rsi)
	movb	%al, 744(%rsi)
	movq	$257, %rcx              # imm = 0x101
	movw	%cx, 742(%rsi)
	movq	$1, %rdx
	movb	%dl, 748(%rsi)
	movw	%cx, 746(%rsi)
	movb	%al, 767(%rsi)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_91:                               # %sw.bb156
	movq	$3, %rax
	movq	32(%rsp), %rdx          # 8-byte Reload
	movl	%eax, 736(%rdx)
	xorq	%rax, %rax
	movb	%al, 769(%rdx)
	movb	%al, 744(%rdx)
	movw	%ax, 742(%rdx)
	movb	%al, 748(%rdx)
	movq	$257, %rcx              # imm = 0x101
.LBB8_82:                               # %if.end465
	movw	%cx, 746(%rdx)
.LBB8_83:                               # %if.end465
	movb	%al, 767(%rdx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_76:                               # %sw.bb136
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 746(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_61:                               # %sw.bb118
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 740(%rcx)
	jmp	.LBB8_63
.LBB8_62:                               # %sw.bb121
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
.LBB8_63:                               # %if.end465
	movb	%al, 824(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_57:                               # %sw.bb108
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 755(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_119:
	movabsq	$0, %rdx
.LBB8_121:                              # %sw.bb202
	movq	%r15, %r13
	movq	%r12, %rsi
	callq	deps_add_target
	jmp	.LBB8_199
.LBB8_145:                              # %do.body264
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	$0, 80(%rcx)
	je	.LBB8_146
# BB#147:                               # %if.else268
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	88(%rdx), %rcx
	addq	$88, %rdx
	jmp	.LBB8_73
.LBB8_150:                              # %if.else294
	movq	cpp_GCC_INCLUDE_DIR_len(%rip), %rbx
	testq	%rbx, %rbx
	je	.LBB8_153
# BB#151:                               # %if.then296
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	movq	%r14, %rax
	sarq	%cl, %rax
	leaq	1(%rax,%rbx), %rax
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$cpp_GCC_INCLUDE_DIR, %rax
	movq	40(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rbx
	movabsq	$4294967296, %rax       # imm = 0x100000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%rbx, %rdi
	movq	%r12, %rsi
.LBB8_152:                              # %if.end307
	movq	%r14, %rdx
	callq	memcpy
	jmp	.LBB8_154
.LBB8_143:                              # %if.then257
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, 96(%rcx)
	addq	$104, %rcx
	jmp	.LBB8_71
.LBB8_131:                              # %if.else229
	movq	$24, %rdi
	callq	xmalloc
	movq	%rbx, 8(%rax)
	movq	$0, (%rax)
	movq	$cpp_assert, 16(%rax)
.LBB8_69:                               # %sw.bb131
	movq	8(%rsp), %rcx           # 8-byte Reload
	cmpq	$0, (%rcx)
	je	.LBB8_70
# BB#72:                                # %if.else.i690
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	8(%rdx), %rcx
	addq	$8, %rdx
.LBB8_73:                               # %if.end465
	movq	%rax, (%rcx)
	movq	%rax, (%rdx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_127:                              # %if.then225
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB8_129
.LBB8_128:                              # %while.body.i783
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rbx
	movq	%rax, %rdi
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rax
	jne	.LBB8_128
.LBB8_129:                              # %free_chain.exit
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	$0, 8(%rax)
	movq	$0, (%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_134:                              # %if.then235
	movq	32(%rsp), %rax          # 8-byte Reload
	movzbl	766(%rax), %eax
	testq	%rax, %rax
	je	.LBB8_135
# BB#136:                               # %if.else243
	movabsq	$.L.str11, %rcx
.LBB8_99:                               # %return
	xorq	%rax, %rax
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rcx, %rsi
	callq	cpp_fatal
	movq	16(%rsp), %r13          # 8-byte Reload
	jmp	.LBB8_200
.LBB8_160:                              # %if.then317
	movq	$257, %rax              # imm = 0x101
	movq	32(%rsp), %rcx          # 8-byte Reload
	movw	%ax, 758(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_162:                              # %if.then325
	movq	$1, %rax
	jmp	.LBB8_163
.LBB8_165:                              # %if.then332
	movq	$1, %rax
	jmp	.LBB8_166
.LBB8_168:                              # %if.then340
	movq	$1, %rax
	jmp	.LBB8_169
.LBB8_172:                              # %if.then356
	movq	$1, %rax
	jmp	.LBB8_173
.LBB8_175:                              # %if.then363
	movq	$1, %rax
	jmp	.LBB8_176
.LBB8_178:                              # %if.then370
	movq	$1, %rax
	jmp	.LBB8_179
.LBB8_181:                              # %if.then377
	movq	$1, %rax
	jmp	.LBB8_182
.LBB8_184:                              # %if.then384
	xorq	%rax, %rax
.LBB8_163:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 761(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_186:                              # %if.then392
	xorq	%rax, %rax
.LBB8_166:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 759(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_188:                              # %if.then400
	xorq	%rax, %rax
.LBB8_169:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 758(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_191:                              # %if.then416
	xorq	%rax, %rax
.LBB8_173:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 768(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_193:                              # %if.then424
	xorq	%rax, %rax
.LBB8_176:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 760(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_195:                              # %if.then432
	xorq	%rax, %rax
.LBB8_179:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 762(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_197:                              # %if.then440
	xorq	%rax, %rax
.LBB8_182:                              # %if.end465
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 756(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_35:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_49:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_51:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_53:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_55:
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_97:                               # %if.then168
	movq	32(%rsp), %rax          # 8-byte Reload
	movq	%rbx, 656(%rax)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_146:                              # %if.then266
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, 80(%rcx)
	addq	$88, %rcx
	jmp	.LBB8_71
.LBB8_153:                              # %if.else304
	movq	%r12, %rdi
	callq	xstrdup
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB8_154:                              # %if.end307
	cmpq	$39, %r13
	jne	.LBB8_155
# BB#156:                               # %if.end307
	movabsq	$1, %rax
	jmp	.LBB8_157
.LBB8_70:                               # %if.then.i689
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rax, (%rcx)
	addq	$8, %rcx
.LBB8_71:                               # %if.end465
	movq	%rax, (%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_135:                              # %if.then238
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	32(%rdx), %rax
	movq	40(%rdx), %rcx
	movq	%rcx, 24(%rdx)
	movq	%rax, 16(%rdx)
	movq	$0, 40(%rdx)
	movq	$0, 32(%rdx)
	movq	$1, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movb	%al, 766(%rcx)
	movq	%r15, %r13
	jmp	.LBB8_199
.LBB8_155:
	movabsq	$0, %rax
.LBB8_157:                              # %if.end307
	xorq	%rcx, %rcx
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rax, %rdx
.LBB8_139:                              # %if.end465
	callq	append_include_chain
	movq	%r15, %r13
	jmp	.LBB8_199
.Ltmp131:
	.size	cpp_handle_option, .Ltmp131-cpp_handle_option
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_66
	.quad	.LBB8_77
	.quad	.LBB8_58
	.quad	.LBB8_59
	.quad	.LBB8_60
	.quad	.LBB8_125
	.quad	.LBB8_64
	.quad	.LBB8_68
	.quad	.LBB8_67
	.quad	.LBB8_133
	.quad	.LBB8_113
	.quad	.LBB8_122
	.quad	.LBB8_116
	.quad	.LBB8_112
	.quad	.LBB8_114
	.quad	.LBB8_124
	.quad	.LBB8_117
	.quad	.LBB8_118
	.quad	.LBB8_118
	.quad	.LBB8_65
	.quad	.LBB8_132
	.quad	.LBB8_159
	.quad	.LBB8_34
	.quad	.LBB8_36
	.quad	.LBB8_37
	.quad	.LBB8_38
	.quad	.LBB8_41
	.quad	.LBB8_44
	.quad	.LBB8_39
	.quad	.LBB8_42
	.quad	.LBB8_45
	.quad	.LBB8_48
	.quad	.LBB8_47
	.quad	.LBB8_58
	.quad	.LBB8_158
	.quad	.LBB8_141
	.quad	.LBB8_141
	.quad	.LBB8_79
	.quad	.LBB8_140
	.quad	.LBB8_148
	.quad	.LBB8_148
	.quad	.LBB8_86
	.quad	.LBB8_88
	.quad	.LBB8_80
	.quad	.LBB8_92
	.quad	.LBB8_84
	.quad	.LBB8_85
	.quad	.LBB8_94
	.quad	.LBB8_95
	.quad	.LBB8_96
	.quad	.LBB8_75
	.quad	.LBB8_74
	.quad	.LBB8_78
	.quad	.LBB8_87
	.quad	.LBB8_92
	.quad	.LBB8_93
	.quad	.LBB8_93
	.quad	.LBB8_88
	.quad	.LBB8_89
	.quad	.LBB8_89
	.quad	.LBB8_92
	.quad	.LBB8_91
	.quad	.LBB8_93
	.quad	.LBB8_93
	.quad	.LBB8_76
	.quad	.LBB8_61
	.quad	.LBB8_62
	.quad	.LBB8_57

	.text
	.align	16, 0x90
	.type	append_include_chain,@function
append_include_chain:                   # @append_include_chain
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp138:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp139:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp140:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp141:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp142:
	.cfi_def_cfa_offset 48
	subq	$160, %rsp
.Ltmp143:
	.cfi_def_cfa_offset 208
.Ltmp144:
	.cfi_offset %rbx, -48
.Ltmp145:
	.cfi_offset %r12, -40
.Ltmp146:
	.cfi_offset %r13, -32
.Ltmp147:
	.cfi_offset %r14, -24
.Ltmp148:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r15
	movzbl	(%rbx), %eax
	movq	672(%r15), %r13
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %r14
	testq	%rax, %rax
	jne	.LBB9_2
# BB#1:                                 # %if.then
	movq	%rbx, %rdi
	callq	free
	movabsq	$.L.str46, %rdi
	callq	xstrdup
	movq	%rax, %rbx
.LBB9_2:                                # %if.end
	movq	%rbx, %rdi
	callq	_cpp_simplify_pathname
	leaq	16(%rsp), %rsi
	movq	%rbx, %rdi
	callq	stat
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB9_8
# BB#3:                                 # %if.then4
	callq	__errno_location
	movl	(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB9_4
# BB#5:                                 # %if.else
	movzbl	740(%r15), %eax
	testq	%rax, %rax
	je	.LBB9_7
# BB#6:                                 # %if.then11
	movq	stderr(%rip), %rdi
	movabsq	$.L.str47, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdx
	callq	fprintf
	jmp	.LBB9_7
.LBB9_8:                                # %if.end15
	movq	%r13, (%rsp)            # 8-byte Spill
	movl	40(%rsp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB9_9
# BB#10:                                # %if.end19
	movq	%rbx, %rdi
	callq	strlen
	movl	288(%r15), %ecx
	movq	%rax, %r13
                                        # kill: RAX<def> R13<kill>
	andq	%r12, %rax
	cmpq	%rcx, %rax
	jbe	.LBB9_12
# BB#11:                                # %if.then24
	movl	%r13d, 288(%r15)
.LBB9_12:                               # %if.end26
	movq	$56, %rdi
	callq	xmalloc
	movq	%rbx, 8(%rax)
	movl	%r13d, 16(%rax)
	movq	24(%rsp), %rcx
	movq	%rcx, 24(%rax)
	movq	16(%rsp), %rcx
	movq	%rcx, 32(%rax)
	leaq	-1(%r14), %rcx
	andq	%r12, %rcx
	cmpq	$1, %rcx
	ja	.LBB9_17
# BB#13:                                # %if.then33
	movq	8(%rsp), %rcx           # 8-byte Reload
	testq	%r12, %rcx
	je	.LBB9_14
# BB#15:                                # %if.then33
	movabsq	$1, %rcx
	jmp	.LBB9_16
.LBB9_4:                                # %if.then8
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	cpp_notice_from_errno
	jmp	.LBB9_7
.LBB9_9:                                # %if.then18
	movabsq	$.L.str48, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%rbx, %rdx
	callq	cpp_notice
.LBB9_7:                                # %if.end14
	movq	%rbx, %rdi
	callq	free
.LBB9_32:                               # %cleanup
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_17:                               # %if.else35
	xorq	%rcx, %rcx
	movl	%ecx, 40(%rax)
	movq	(%rsp), %rdx            # 8-byte Reload
	jmp	.LBB9_18
.LBB9_14:
	movabsq	$0, %rcx
.LBB9_16:                               # %if.then33
	movq	(%rsp), %rdx            # 8-byte Reload
	xorq	$1, %rcx
	incq	%rcx
	movl	%ecx, 40(%rax)
.LBB9_18:                               # %if.end37
	movq	$0, 48(%rax)
	movq	$0, (%rax)
	movq	%r14, %rcx
	andq	%r12, %rcx
	cmpq	$2, %rcx
	je	.LBB9_27
# BB#19:                                # %if.end37
	cmpq	$1, %rcx
	jne	.LBB9_20
# BB#24:                                # %do.body46
	cmpq	$0, 48(%rdx)
	je	.LBB9_25
# BB#26:                                # %if.else50
	movq	56(%rdx), %rcx
	addq	$56, %rdx
	jmp	.LBB9_30
.LBB9_27:                               # %do.body57
	cmpq	$0, 64(%rdx)
	je	.LBB9_28
# BB#29:                                # %if.else61
	movq	72(%rdx), %rcx
	addq	$72, %rdx
	jmp	.LBB9_30
.LBB9_20:                               # %if.end37
	testq	%r12, %r14
	jne	.LBB9_32
# BB#21:                                # %do.body
	cmpq	$0, 32(%rdx)
	je	.LBB9_22
# BB#23:                                # %if.else41
	movq	40(%rdx), %rcx
	addq	$40, %rdx
.LBB9_30:                               # %if.end63
	movq	%rax, (%rcx)
	jmp	.LBB9_31
.LBB9_25:                               # %if.then48
	movq	%rax, 48(%rdx)
	addq	$56, %rdx
	jmp	.LBB9_31
.LBB9_28:                               # %if.then59
	movq	%rax, 64(%rdx)
	addq	$72, %rdx
	jmp	.LBB9_31
.LBB9_22:                               # %if.then39
	movq	%rax, 32(%rdx)
	addq	$40, %rdx
.LBB9_31:                               # %if.end63
	movq	%rax, (%rdx)
	jmp	.LBB9_32
.Ltmp149:
	.size	append_include_chain, .Ltmp149-append_include_chain
	.cfi_endproc

	.globl	cpp_handle_options
	.align	16, 0x90
	.type	cpp_handle_options,@function
cpp_handle_options:                     # @cpp_handle_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp156:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp157:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp158:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp159:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp160:
	.cfi_def_cfa_offset 48
.Ltmp161:
	.cfi_offset %rbx, -48
.Ltmp162:
	.cfi_offset %r12, -40
.Ltmp163:
	.cfi_offset %r13, -32
.Ltmp164:
	.cfi_offset %r14, -24
.Ltmp165:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r12
	movq	%rdi, %r15
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jle	.LBB10_4
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB10_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rsi
	subq	%rbx, %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%r14,%rax,8), %rdx
	movq	$1, %rcx
	movq	%r15, %rdi
	callq	cpp_handle_option
	testq	%r13, %rax
	je	.LBB10_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB10_3 Depth=1
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB10_3
.LBB10_4:                               # %for.end
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp166:
	.size	cpp_handle_options, .Ltmp166-cpp_handle_options
	.cfi_endproc

	.globl	cpp_post_options
	.align	16, 0x90
	.type	cpp_post_options,@function
cpp_post_options:                       # @cpp_post_options
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp173:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp175:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp176:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp177:
	.cfi_def_cfa_offset 48
.Ltmp178:
	.cfi_offset %rbx, -48
.Ltmp179:
	.cfi_offset %r12, -40
.Ltmp180:
	.cfi_offset %r13, -32
.Ltmp181:
	.cfi_offset %r14, -24
.Ltmp182:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movzbl	824(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB11_1
.LBB11_2:                               # %if.end
	movq	648(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB11_4
# BB#3:                                 # %lor.lhs.false
	movabsq	$.L.str10, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB11_5
.LBB11_4:                               # %if.then7
	movq	$.L.str8, 648(%rbx)
.LBB11_5:                               # %if.end10
	movq	656(%rbx), %r14
	testq	%r14, %r14
	je	.LBB11_7
# BB#6:                                 # %lor.lhs.false13
	movabsq	$.L.str10, %rsi
	movq	%r14, %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB11_8
.LBB11_7:                               # %if.then18
	movq	$.L.str8, 656(%rbx)
	movabsq	$.L.str8, %r14
.LBB11_8:                               # %if.end21
	movzbl	742(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_10
# BB#9:                                 # %if.then24
	xorq	%rax, %rax
	movb	%al, 761(%rbx)
.LBB11_10:                              # %if.end26
	cmpq	$0, 728(%rbx)
	jne	.LBB11_12
# BB#11:                                # %if.then29
	movq	$.L.str8, 728(%rbx)
.LBB11_12:                              # %if.end32
	movzbl	771(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_14
# BB#13:                                # %if.then35
	movq	$1, %rax
	movb	%al, 15(%rbx)
.LBB11_14:                              # %if.end36
	movzbl	774(%rbx), %eax
	cmpq	$1, %rax
	jne	.LBB11_15
# BB#35:                                # %if.then46
	movq	$1, %rax
	movb	%al, 763(%rbx)
	jmp	.LBB11_17
.LBB11_15:                              # %if.end42
	movzbl	763(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_18
# BB#16:                                # %if.then52
	xorq	%rax, %rax
	movb	%al, 774(%rbx)
.LBB11_17:                              # %if.end55
	xorq	%rax, %rax
	movb	%al, 775(%rbx)
.LBB11_18:                              # %if.end57
	movzbl	749(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_19
# BB#32:                                # %if.else32.i
	cmpq	$0, 680(%rbx)
	jne	.LBB11_34
# BB#33:                                # %if.then37.i
	movq	%r14, 680(%rbx)
	jmp	.LBB11_34
.LBB11_19:                              # %if.then.i
	movabsq	$.L.str44, %rdi
	callq	getenv
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB11_21
# BB#20:
	movq	$1, %rax
	jmp	.LBB11_23
.LBB11_21:                              # %if.else.i
	movabsq	$.L.str45, %rdi
	callq	getenv
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB11_28
# BB#22:
	movq	$2, %rax
.LBB11_23:                              # %if.end11.i
	movb	%al, 749(%rbx)
	movq	$32, %rsi
	movq	%r14, %rdi
	callq	strchr
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB11_25
# BB#24:                                # %if.then14.i
	movq	392(%rbx), %rdi
	leaq	1(%r15), %rsi
	xorq	%rdx, %rdx
	xorq	%r13, %r13
	callq	deps_add_target
	subq	%r14, %r15
	leaq	1(%r15), %rdi
	callq	xmalloc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movb	%r13b, (%r12,%r15)
	movq	%r12, %r14
.LBB11_25:                              # %if.end23.i
	cmpq	$0, 680(%rbx)
	jne	.LBB11_27
# BB#26:                                # %if.then27.i
	movq	%r14, 680(%rbx)
.LBB11_27:                              # %init_dependency_output.exit
	movq	$1, %rax
	movb	%al, 752(%rbx)
	movzbl	749(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB11_34
.LBB11_28:                              # %land.lhs.true
	movzbl	751(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB11_31
# BB#29:                                # %lor.lhs.false65
	cmpq	$0, 680(%rbx)
	jne	.LBB11_31
# BB#30:                                # %lor.lhs.false68
	movzbl	750(%rbx), %eax
	testq	%rax, %rax
	je	.LBB11_34
.LBB11_31:                              # %if.then72
	movabsq	$.L.str31, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	cpp_fatal  # TAILCALL
.LBB11_34:                              # %if.end73
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_1:                               # %if.then
	movq	stderr(%rip), %r14
	movq	version_string(%rip), %rdx
	movabsq	$.L.str29, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movabsq	$.L.str30, %rdi
	movq	$19, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	callq	fwrite
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	fputc
	jmp	.LBB11_2
.Ltmp183:
	.size	cpp_post_options, .Ltmp183-cpp_post_options
	.cfi_endproc

	.align	16, 0x90
	.type	remove_dup_dirs,@function
remove_dup_dirs:                        # @remove_dup_dirs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp190:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp191:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp192:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp193:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp194:
	.cfi_def_cfa_offset 48
.Ltmp195:
	.cfi_offset %rbx, -48
.Ltmp196:
	.cfi_offset %r12, -40
.Ltmp197:
	.cfi_offset %r13, -32
.Ltmp198:
	.cfi_offset %r14, -24
.Ltmp199:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB12_17
# BB#1:                                 # %for.cond1.preheader.lr.ph
	xorq	%r12, %r12
	movq	%r15, %r13
	jmp	.LBB12_2
.LBB12_12:                              # %if.then.i
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdx
	xorq	%rax, %rax
	movq	stderr(%rip), %rdi
	movabsq	$.L.str165, %rsi
	callq	fprintf
	jmp	.LBB12_13
	.align	16, 0x90
.LBB12_2:                               # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB12_4 Depth 2
	cmpq	%r15, %r13
	je	.LBB12_15
# BB#3:                                 # %for.body2.lr.ph
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	24(%r13), %rax
	movq	%r15, %rbx
	.align	16, 0x90
.LBB12_4:                               # %for.body2
                                        #   Parent Loop BB12_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rcx
	cmpq	%rcx, %rax
	jne	.LBB12_14
# BB#5:                                 # %land.lhs.true
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	32(%r13), %rcx
	movq	32(%rbx), %rdx
	cmpq	%rdx, %rcx
	je	.LBB12_6
.LBB12_14:                              # %for.inc
                                        #   in Loop: Header=BB12_4 Depth=2
	movq	(%rbx), %rbx
	cmpq	%r13, %rbx
	jne	.LBB12_4
.LBB12_15:                              #   in Loop: Header=BB12_2 Depth=1
	movq	%r13, %r12
	jmp	.LBB12_16
.LBB12_6:                               # %if.then
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	40(%r13), %eax
	testq	%rax, %rax
	je	.LBB12_11
# BB#7:                                 # %land.lhs.true8
                                        #   in Loop: Header=BB12_2 Depth=1
	movl	40(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB12_11
# BB#8:                                 # %if.then11
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	8(%r13), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str166, %rsi
	callq	cpp_warning
	movq	8(%r13), %rdi
	movq	8(%rbx), %r13
	movq	%r13, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB12_9
# BB#10:                                # %if.else
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str168, %rsi
	callq	cpp_warning
	jmp	.LBB12_11
.LBB12_9:                               # %if.then15
                                        #   in Loop: Header=BB12_2 Depth=1
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str167, %rsi
	movq	%r13, %rdx
	callq	cpp_warning
.LBB12_11:                              # %if.end17
                                        #   in Loop: Header=BB12_2 Depth=1
	movzbl	740(%r14), %eax
	movq	(%r12), %r13
	testq	%rax, %rax
	jne	.LBB12_12
.LBB12_13:                              # %remove_dup_dir.exit
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	(%r13), %rax
	movq	%rax, (%r12)
	movq	8(%r13), %rdi
	callq	free
	movq	%r13, %rdi
	callq	free
	.align	16, 0x90
.LBB12_16:                              # %for.end
                                        #   in Loop: Header=BB12_2 Depth=1
	movq	(%r12), %r13
	testq	%r13, %r13
	jne	.LBB12_2
.LBB12_17:                              # %for.end22
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp200:
	.size	remove_dup_dirs, .Ltmp200-remove_dup_dirs
	.cfi_endproc

	.type	_cpp_trigraph_map,@object # @_cpp_trigraph_map
	.section	.rodata,"a",@progbits
	.globl	_cpp_trigraph_map
	.align	16
_cpp_trigraph_map:
	.asciz	"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000|\000\000\000\000\000^[]\000\000\000~\000\\\000\000\000\000\000\000\000\000\000\000\000\000{#}\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
	.size	_cpp_trigraph_map, 256

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"#include \"...\" search starts here:\n"
	.size	.L.str, 36

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"#include <...> search starts here:\n"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" %s\n"
	.size	.L.str2, 5

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"End of search list.\n"
	.size	.L.str3, 21

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"<built-in>"
	.size	.L.str4, 11

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"<command line>"
	.size	.L.str5, 15

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"too many filenames. Type %s --help for usage info"
	.size	.L.str6, 50

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"_"
	.size	.L.str7, 2

	.type	.L.str8,@object         # @.str8
.L.str8:
	.zero	1
	.size	.L.str8, 1

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"output filename specified twice"
	.size	.L.str9, 32

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"-"
	.size	.L.str10, 2

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"-I- specified twice"
	.size	.L.str11, 20

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"-Wall"
	.size	.L.str12, 6

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"-Wtraditional"
	.size	.L.str13, 14

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"-Wtrigraphs"
	.size	.L.str14, 12

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"-Wcomment"
	.size	.L.str15, 10

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"-Wcomments"
	.size	.L.str16, 11

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"-Wundef"
	.size	.L.str17, 8

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"-Wimport"
	.size	.L.str18, 9

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"-Werror"
	.size	.L.str19, 8

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"-Wsystem-headers"
	.size	.L.str20, 17

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"-Wno-traditional"
	.size	.L.str21, 17

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"-Wno-trigraphs"
	.size	.L.str22, 15

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"-Wno-comment"
	.size	.L.str23, 13

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"-Wno-comments"
	.size	.L.str24, 14

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"-Wno-undef"
	.size	.L.str25, 11

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"-Wno-import"
	.size	.L.str26, 12

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"-Wno-error"
	.size	.L.str27, 11

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"-Wno-system-headers"
	.size	.L.str28, 20

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"GNU CPP version %s (cpplib)"
	.size	.L.str29, 28

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	" (x86-64 Linux/ELF)"
	.size	.L.str30, 20

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"you must additionally specify either -M or -MM"
	.size	.L.str31, 47

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Switches:\n  -include <file>           Include the contents of <file> before other files\n  -imacros <file>           Accept definition of macros in <file>\n  -iprefix <path>           Specify <path> as a prefix for next two options\n  -iwithprefix <dir>        Add <dir> to the end of the system include path\n  -iwithprefixbefore <dir>  Add <dir> to the end of the main include path\n  -isystem <dir>            Add <dir> to the start of the system include path\n"
	.size	.L.str32, 459

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"  -idirafter <dir>          Add <dir> to the end of the system include path\n  -I <dir>                  Add <dir> to the end of the main include path\n  -I-                       Fine-grained include path control; see info docs\n  -nostdinc                 Do not search system include directories\n                             (dirs specified with -isystem will still be used)\n  -nostdinc++               Do not search system include directories for C++\n  -o <file>                 Put output into <file>\n"
	.size	.L.str33, 504

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"  -pedantic                 Issue all warnings demanded by strict ISO C\n  -pedantic-errors          Issue -pedantic warnings as errors instead\n  -trigraphs                Support ISO C trigraphs\n  -lang-c                   Assume that the input sources are in C\n  -lang-c89                 Assume that the input sources are in C89\n"
	.size	.L.str34, 332

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"  -lang-c++                 Assume that the input sources are in C++\n  -lang-objc                Assume that the input sources are in ObjectiveC\n  -lang-objc++              Assume that the input sources are in ObjectiveC++\n  -lang-asm                 Assume that the input sources are in assembler\n"
	.size	.L.str35, 299

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"  -std=<std name>           Specify the conformance standard; one of:\n                            gnu89, gnu99, c89, c99, iso9899:1990,\n                            iso9899:199409, iso9899:1999\n  -+                        Allow parsing of C++ style features\n  -w                        Inhibit warning messages\n  -Wtrigraphs               Warn if trigraphs are encountered\n  -Wno-trigraphs            Do not warn about trigraphs\n  -Wcomment{s}              Warn if one comment starts inside another\n"
	.size	.L.str36, 499

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"  -Wno-comment{s}           Do not warn about comments\n  -Wtraditional             Warn about features not present in traditional C\n  -Wno-traditional          Do not warn about traditional C\n  -Wundef                   Warn if an undefined macro is used by #if\n  -Wno-undef                Do not warn about testing undefined macros\n  -Wimport                  Warn about the use of the #import directive\n"
	.size	.L.str37, 406

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"  -Wno-import               Do not warn about the use of #import\n  -Werror                   Treat all warnings as errors\n  -Wno-error                Do not treat warnings as errors\n  -Wsystem-headers          Do not suppress warnings from system headers\n  -Wno-system-headers       Suppress warnings from system headers\n  -Wall                     Enable all preprocessor warnings\n"
	.size	.L.str38, 383

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"  -M                        Generate make dependencies\n  -MM                       As -M, but ignore system header files\n  -MD                       Generate make dependencies and compile\n  -MMD                      As -MD, but ignore system header files\n  -MF <file>                Write dependency output to the given file\n  -MG                       Treat missing header file as generated files\n"
	.size	.L.str39, 399

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"  -MP\t\t\t    Generate phony targets for all headers\n  -MQ <target>              Add a MAKE-quoted target\n  -MT <target>              Add an unquoted target\n"
	.size	.L.str40, 156

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"  -D<macro>                 Define a <macro> with string '1' as its value\n  -D<macro>=<val>           Define a <macro> with <val> as its value\n  -A<question>=<answer>     Assert the <answer> to <question>\n  -A-<question>=<answer>    Disable the <answer> to <question>\n  -U<macro>                 Undefine <macro> \n  -v                        Display the version number\n"
	.size	.L.str41, 370

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"  -H                        Print the name of header files as they are used\n  -C                        Do not discard comments\n  -dM                       Display a list of macro definitions active at end\n  -dD                       Preserve macro definitions in output\n  -dN                       As -dD except that only the names are preserved\n  -dI                       Include #include directives in the output\n"
	.size	.L.str42, 418

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"  -fpreprocessed            Treat the input file as already preprocessed\n  -ftabstop=<number>        Distance between tab stops for column reporting\n  -P                        Do not generate #line directives\n  -$                        Do not allow '$' in identifiers\n  -remap                    Remap file names when including files\n  --version                 Display version information\n  -h or --help              Display this information\n"
	.size	.L.str43, 446

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"DEPENDENCIES_OUTPUT"
	.size	.L.str44, 20

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"SUNPRO_DEPENDENCIES"
	.size	.L.str45, 20

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"."
	.size	.L.str46, 2

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"ignoring nonexistent directory \"%s\"\n"
	.size	.L.str47, 37

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"%s: Not a directory"
	.size	.L.str48, 20

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"$"
	.size	.L.str49, 2

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"+"
	.size	.L.str50, 2

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"-help"
	.size	.L.str51, 6

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"-target-help"
	.size	.L.str52, 13

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"-version"
	.size	.L.str53, 9

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"A"
	.size	.L.str54, 2

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"assertion missing after %s"
	.size	.L.str55, 27

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"C"
	.size	.L.str56, 2

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"D"
	.size	.L.str57, 2

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"macro name missing after %s"
	.size	.L.str58, 28

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"H"
	.size	.L.str59, 2

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"I"
	.size	.L.str60, 2

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"directory name missing after %s"
	.size	.L.str61, 32

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"M"
	.size	.L.str62, 2

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"MD"
	.size	.L.str63, 3

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"file name missing after %s"
	.size	.L.str64, 27

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"MF"
	.size	.L.str65, 3

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"MG"
	.size	.L.str66, 3

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"MM"
	.size	.L.str67, 3

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"MMD"
	.size	.L.str68, 4

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"MP"
	.size	.L.str69, 3

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"MQ"
	.size	.L.str70, 3

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"target missing after %s"
	.size	.L.str71, 24

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"MT"
	.size	.L.str72, 3

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"P"
	.size	.L.str73, 2

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"U"
	.size	.L.str74, 2

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"W"
	.size	.L.str75, 2

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"argument missing after %s"
	.size	.L.str76, 26

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"d"
	.size	.L.str77, 2

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"fleading-underscore"
	.size	.L.str78, 20

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"fno-leading-underscore"
	.size	.L.str79, 23

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"fno-operator-names"
	.size	.L.str80, 19

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"fno-preprocessed"
	.size	.L.str81, 17

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"fno-show-column"
	.size	.L.str82, 16

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"fpreprocessed"
	.size	.L.str83, 14

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"fshow-column"
	.size	.L.str84, 13

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"fsigned-char"
	.size	.L.str85, 13

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"ftabstop="
	.size	.L.str86, 10

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"number missing after %s"
	.size	.L.str87, 24

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"funsigned-char"
	.size	.L.str88, 15

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"h"
	.size	.L.str89, 2

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"idirafter"
	.size	.L.str90, 10

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"imacros"
	.size	.L.str91, 8

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"include"
	.size	.L.str92, 8

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"iprefix"
	.size	.L.str93, 8

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"path name missing after %s"
	.size	.L.str94, 27

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"isystem"
	.size	.L.str95, 8

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"iwithprefix"
	.size	.L.str96, 12

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"iwithprefixbefore"
	.size	.L.str97, 18

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"lang-asm"
	.size	.L.str98, 9

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"lang-c"
	.size	.L.str99, 7

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"lang-c++"
	.size	.L.str100, 9

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"lang-c89"
	.size	.L.str101, 9

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"lang-objc"
	.size	.L.str102, 10

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"lang-objc++"
	.size	.L.str103, 12

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"nostdinc"
	.size	.L.str104, 9

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"nostdinc++"
	.size	.L.str105, 11

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"o"
	.size	.L.str106, 2

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"pedantic"
	.size	.L.str107, 9

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"pedantic-errors"
	.size	.L.str108, 16

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"remap"
	.size	.L.str109, 6

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"std=c++98"
	.size	.L.str110, 10

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"std=c89"
	.size	.L.str111, 8

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"std=c99"
	.size	.L.str112, 8

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"std=c9x"
	.size	.L.str113, 8

	.type	.L.str114,@object       # @.str114
.L.str114:
	.asciz	"std=gnu89"
	.size	.L.str114, 10

	.type	.L.str115,@object       # @.str115
.L.str115:
	.asciz	"std=gnu99"
	.size	.L.str115, 10

	.type	.L.str116,@object       # @.str116
.L.str116:
	.asciz	"std=gnu9x"
	.size	.L.str116, 10

	.type	.L.str117,@object       # @.str117
.L.str117:
	.asciz	"std=iso9899:1990"
	.size	.L.str117, 17

	.type	.L.str118,@object       # @.str118
.L.str118:
	.asciz	"std=iso9899:199409"
	.size	.L.str118, 19

	.type	.L.str119,@object       # @.str119
.L.str119:
	.asciz	"std=iso9899:1999"
	.size	.L.str119, 17

	.type	.L.str120,@object       # @.str120
.L.str120:
	.asciz	"std=iso9899:199x"
	.size	.L.str120, 17

	.type	.L.str121,@object       # @.str121
.L.str121:
	.asciz	"trigraphs"
	.size	.L.str121, 10

	.type	.L.str122,@object       # @.str122
.L.str122:
	.asciz	"v"
	.size	.L.str122, 2

	.type	.L.str123,@object       # @.str123
.L.str123:
	.asciz	"version"
	.size	.L.str123, 8

	.type	.L.str124,@object       # @.str124
.L.str124:
	.asciz	"w"
	.size	.L.str124, 2

	.type	cl_options,@object      # @cl_options
	.section	.rodata,"a",@progbits
	.align	16
cl_options:
	.quad	.L.str49
	.quad	0
	.quad	1                       # 0x1
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str50
	.quad	0
	.quad	1                       # 0x1
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str51
	.quad	0
	.quad	5                       # 0x5
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str52
	.quad	0
	.quad	12                      # 0xc
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str53
	.quad	0
	.quad	8                       # 0x8
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str54
	.quad	.L.str55
	.quad	1                       # 0x1
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str56
	.quad	0
	.quad	1                       # 0x1
	.long	6                       # 0x6
	.zero	4
	.quad	.L.str57
	.quad	.L.str58
	.quad	1                       # 0x1
	.long	7                       # 0x7
	.zero	4
	.quad	.L.str59
	.quad	0
	.quad	1                       # 0x1
	.long	8                       # 0x8
	.zero	4
	.quad	.L.str60
	.quad	.L.str61
	.quad	1                       # 0x1
	.long	9                       # 0x9
	.zero	4
	.quad	.L.str62
	.quad	0
	.quad	1                       # 0x1
	.long	10                      # 0xa
	.zero	4
	.quad	.L.str63
	.quad	.L.str64
	.quad	2                       # 0x2
	.long	11                      # 0xb
	.zero	4
	.quad	.L.str65
	.quad	.L.str64
	.quad	2                       # 0x2
	.long	12                      # 0xc
	.zero	4
	.quad	.L.str66
	.quad	0
	.quad	2                       # 0x2
	.long	13                      # 0xd
	.zero	4
	.quad	.L.str67
	.quad	0
	.quad	2                       # 0x2
	.long	14                      # 0xe
	.zero	4
	.quad	.L.str68
	.quad	.L.str64
	.quad	3                       # 0x3
	.long	15                      # 0xf
	.zero	4
	.quad	.L.str69
	.quad	0
	.quad	2                       # 0x2
	.long	16                      # 0x10
	.zero	4
	.quad	.L.str70
	.quad	.L.str71
	.quad	2                       # 0x2
	.long	17                      # 0x11
	.zero	4
	.quad	.L.str72
	.quad	.L.str71
	.quad	2                       # 0x2
	.long	18                      # 0x12
	.zero	4
	.quad	.L.str73
	.quad	0
	.quad	1                       # 0x1
	.long	19                      # 0x13
	.zero	4
	.quad	.L.str74
	.quad	.L.str58
	.quad	1                       # 0x1
	.long	20                      # 0x14
	.zero	4
	.quad	.L.str75
	.quad	.L.str76
	.quad	1                       # 0x1
	.long	21                      # 0x15
	.zero	4
	.quad	.L.str77
	.quad	.L.str76
	.quad	1                       # 0x1
	.long	22                      # 0x16
	.zero	4
	.quad	.L.str78
	.quad	0
	.quad	19                      # 0x13
	.long	23                      # 0x17
	.zero	4
	.quad	.L.str79
	.quad	0
	.quad	22                      # 0x16
	.long	24                      # 0x18
	.zero	4
	.quad	.L.str80
	.quad	0
	.quad	18                      # 0x12
	.long	25                      # 0x19
	.zero	4
	.quad	.L.str81
	.quad	0
	.quad	16                      # 0x10
	.long	26                      # 0x1a
	.zero	4
	.quad	.L.str82
	.quad	0
	.quad	15                      # 0xf
	.long	27                      # 0x1b
	.zero	4
	.quad	.L.str83
	.quad	0
	.quad	13                      # 0xd
	.long	28                      # 0x1c
	.zero	4
	.quad	.L.str84
	.quad	0
	.quad	12                      # 0xc
	.long	29                      # 0x1d
	.zero	4
	.quad	.L.str85
	.quad	0
	.quad	12                      # 0xc
	.long	30                      # 0x1e
	.zero	4
	.quad	.L.str86
	.quad	.L.str87
	.quad	9                       # 0x9
	.long	31                      # 0x1f
	.zero	4
	.quad	.L.str88
	.quad	0
	.quad	14                      # 0xe
	.long	32                      # 0x20
	.zero	4
	.quad	.L.str89
	.quad	0
	.quad	1                       # 0x1
	.long	33                      # 0x21
	.zero	4
	.quad	.L.str90
	.quad	.L.str61
	.quad	9                       # 0x9
	.long	34                      # 0x22
	.zero	4
	.quad	.L.str91
	.quad	.L.str64
	.quad	7                       # 0x7
	.long	35                      # 0x23
	.zero	4
	.quad	.L.str92
	.quad	.L.str64
	.quad	7                       # 0x7
	.long	36                      # 0x24
	.zero	4
	.quad	.L.str93
	.quad	.L.str94
	.quad	7                       # 0x7
	.long	37                      # 0x25
	.zero	4
	.quad	.L.str95
	.quad	.L.str61
	.quad	7                       # 0x7
	.long	38                      # 0x26
	.zero	4
	.quad	.L.str96
	.quad	.L.str61
	.quad	11                      # 0xb
	.long	39                      # 0x27
	.zero	4
	.quad	.L.str97
	.quad	.L.str61
	.quad	17                      # 0x11
	.long	40                      # 0x28
	.zero	4
	.quad	.L.str98
	.quad	0
	.quad	8                       # 0x8
	.long	41                      # 0x29
	.zero	4
	.quad	.L.str99
	.quad	0
	.quad	6                       # 0x6
	.long	42                      # 0x2a
	.zero	4
	.quad	.L.str100
	.quad	0
	.quad	8                       # 0x8
	.long	43                      # 0x2b
	.zero	4
	.quad	.L.str101
	.quad	0
	.quad	8                       # 0x8
	.long	44                      # 0x2c
	.zero	4
	.quad	.L.str102
	.quad	0
	.quad	9                       # 0x9
	.long	45                      # 0x2d
	.zero	4
	.quad	.L.str103
	.quad	0
	.quad	11                      # 0xb
	.long	46                      # 0x2e
	.zero	4
	.quad	.L.str104
	.quad	0
	.quad	8                       # 0x8
	.long	47                      # 0x2f
	.zero	4
	.quad	.L.str105
	.quad	0
	.quad	10                      # 0xa
	.long	48                      # 0x30
	.zero	4
	.quad	.L.str106
	.quad	.L.str64
	.quad	1                       # 0x1
	.long	49                      # 0x31
	.zero	4
	.quad	.L.str107
	.quad	0
	.quad	8                       # 0x8
	.long	50                      # 0x32
	.zero	4
	.quad	.L.str108
	.quad	0
	.quad	15                      # 0xf
	.long	51                      # 0x33
	.zero	4
	.quad	.L.str109
	.quad	0
	.quad	5                       # 0x5
	.long	52                      # 0x34
	.zero	4
	.quad	.L.str110
	.quad	0
	.quad	9                       # 0x9
	.long	53                      # 0x35
	.zero	4
	.quad	.L.str111
	.quad	0
	.quad	7                       # 0x7
	.long	54                      # 0x36
	.zero	4
	.quad	.L.str112
	.quad	0
	.quad	7                       # 0x7
	.long	55                      # 0x37
	.zero	4
	.quad	.L.str113
	.quad	0
	.quad	7                       # 0x7
	.long	56                      # 0x38
	.zero	4
	.quad	.L.str114
	.quad	0
	.quad	9                       # 0x9
	.long	57                      # 0x39
	.zero	4
	.quad	.L.str115
	.quad	0
	.quad	9                       # 0x9
	.long	58                      # 0x3a
	.zero	4
	.quad	.L.str116
	.quad	0
	.quad	9                       # 0x9
	.long	59                      # 0x3b
	.zero	4
	.quad	.L.str117
	.quad	0
	.quad	16                      # 0x10
	.long	60                      # 0x3c
	.zero	4
	.quad	.L.str118
	.quad	0
	.quad	18                      # 0x12
	.long	61                      # 0x3d
	.zero	4
	.quad	.L.str119
	.quad	0
	.quad	16                      # 0x10
	.long	62                      # 0x3e
	.zero	4
	.quad	.L.str120
	.quad	0
	.quad	16                      # 0x10
	.long	63                      # 0x3f
	.zero	4
	.quad	.L.str121
	.quad	0
	.quad	9                       # 0x9
	.long	64                      # 0x40
	.zero	4
	.quad	.L.str122
	.quad	0
	.quad	1                       # 0x1
	.long	65                      # 0x41
	.zero	4
	.quad	.L.str123
	.quad	0
	.quad	7                       # 0x7
	.long	66                      # 0x42
	.zero	4
	.quad	.L.str124
	.quad	0
	.quad	1                       # 0x1
	.long	67                      # 0x43
	.zero	4
	.size	cl_options, 2176

	.type	.L.str125,@object       # @.str125
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str125:
	.asciz	"a"
	.size	.L.str125, 2

	.type	.L.str126,@object       # @.str126
.L.str126:
	.asciz	"I/O error on output"
	.size	.L.str126, 20

	.type	.L.str127,@object       # @.str127
.L.str127:
	.asciz	"%s \"%s\"\n"
	.size	.L.str127, 9

	.type	.L.str128,@object       # @.str128
.L.str128:
	.asciz	"%s %s\n"
	.size	.L.str128, 7

	.type	.L.str129,@object       # @.str129
.L.str129:
	.asciz	"__cplusplus 1"
	.size	.L.str129, 14

	.type	.L.str130,@object       # @.str130
.L.str130:
	.asciz	"__GXX_WEAK__ 1"
	.size	.L.str130, 15

	.type	.L.str131,@object       # @.str131
.L.str131:
	.asciz	"__OBJC__ 1"
	.size	.L.str131, 11

	.type	.L.str132,@object       # @.str132
.L.str132:
	.asciz	"__STDC_VERSION__ 199409L"
	.size	.L.str132, 25

	.type	.L.str133,@object       # @.str133
.L.str133:
	.asciz	"__STDC_VERSION__ 199901L"
	.size	.L.str133, 25

	.type	.L.str134,@object       # @.str134
.L.str134:
	.asciz	"__CHAR_UNSIGNED__ 1"
	.size	.L.str134, 20

	.type	.L.str135,@object       # @.str135
.L.str135:
	.asciz	"__STRICT_ANSI__ 1"
	.size	.L.str135, 18

	.type	.L.str136,@object       # @.str136
.L.str136:
	.asciz	"__ASSEMBLER__ 1"
	.size	.L.str136, 16

	.type	.L.str137,@object       # @.str137
.L.str137:
	.asciz	"__TIME__"
	.size	.L.str137, 9

	.type	.L.str138,@object       # @.str138
.L.str138:
	.asciz	"__DATE__"
	.size	.L.str138, 9

	.type	.L.str139,@object       # @.str139
.L.str139:
	.asciz	"__FILE__"
	.size	.L.str139, 9

	.type	.L.str140,@object       # @.str140
.L.str140:
	.asciz	"__BASE_FILE__"
	.size	.L.str140, 14

	.type	.L.str141,@object       # @.str141
.L.str141:
	.asciz	"__LINE__"
	.size	.L.str141, 9

	.type	.L.str142,@object       # @.str142
.L.str142:
	.asciz	"__INCLUDE_LEVEL__"
	.size	.L.str142, 18

	.type	.L.str143,@object       # @.str143
.L.str143:
	.asciz	"_Pragma"
	.size	.L.str143, 8

	.type	.L.str144,@object       # @.str144
.L.str144:
	.asciz	"__VERSION__"
	.size	.L.str144, 12

	.type	.L.str145,@object       # @.str145
.L.str145:
	.asciz	"__USER_LABEL_PREFIX__"
	.size	.L.str145, 22

	.type	.L.str146,@object       # @.str146
.L.str146:
	.asciz	"__REGISTER_PREFIX__"
	.size	.L.str146, 20

	.type	.L.str147,@object       # @.str147
.L.str147:
	.asciz	"__HAVE_BUILTIN_SETJMP__"
	.size	.L.str147, 24

	.type	.L.str148,@object       # @.str148
.L.str148:
	.asciz	"1"
	.size	.L.str148, 2

	.type	.L.str149,@object       # @.str149
.L.str149:
	.asciz	"__WCHAR_TYPE__"
	.size	.L.str149, 15

	.type	.L.str150,@object       # @.str150
.L.str150:
	.asciz	"int"
	.size	.L.str150, 4

	.type	.L.str151,@object       # @.str151
.L.str151:
	.asciz	"__WINT_TYPE__"
	.size	.L.str151, 14

	.type	.L.str152,@object       # @.str152
.L.str152:
	.asciz	"unsigned int"
	.size	.L.str152, 13

	.type	.L.str153,@object       # @.str153
.L.str153:
	.asciz	"__STDC__"
	.size	.L.str153, 9

	.type	builtin_array,@object   # @builtin_array
	.section	.rodata,"a",@progbits
	.align	16
builtin_array:
	.quad	.L.str137
	.quad	0
	.byte	5                       # 0x5
	.zero	1
	.short	8                       # 0x8
	.short	8                       # 0x8
	.zero	2
	.quad	.L.str138
	.quad	0
	.byte	1                       # 0x1
	.zero	1
	.short	8                       # 0x8
	.short	8                       # 0x8
	.zero	2
	.quad	.L.str139
	.quad	0
	.byte	2                       # 0x2
	.zero	1
	.short	8                       # 0x8
	.short	8                       # 0x8
	.zero	2
	.quad	.L.str140
	.quad	0
	.byte	3                       # 0x3
	.zero	1
	.short	8                       # 0x8
	.short	13                      # 0xd
	.zero	2
	.quad	.L.str141
	.quad	0
	.byte	0                       # 0x0
	.zero	1
	.short	8                       # 0x8
	.short	8                       # 0x8
	.zero	2
	.quad	.L.str142
	.quad	0
	.byte	4                       # 0x4
	.zero	1
	.short	8                       # 0x8
	.short	17                      # 0x11
	.zero	2
	.quad	.L.str143
	.quad	0
	.byte	7                       # 0x7
	.zero	1
	.short	8                       # 0x8
	.short	7                       # 0x7
	.zero	2
	.quad	.L.str144
	.quad	0
	.byte	0                       # 0x0
	.zero	1
	.short	1                       # 0x1
	.short	11                      # 0xb
	.zero	2
	.quad	.L.str145
	.quad	0
	.byte	0                       # 0x0
	.zero	1
	.short	2                       # 0x2
	.short	21                      # 0x15
	.zero	2
	.quad	.L.str146
	.quad	.L.str8
	.byte	0                       # 0x0
	.zero	1
	.short	0                       # 0x0
	.short	19                      # 0x13
	.zero	2
	.quad	.L.str147
	.quad	.L.str148
	.byte	0                       # 0x0
	.zero	1
	.short	0                       # 0x0
	.short	23                      # 0x17
	.zero	2
	.quad	.L.str149
	.quad	.L.str150
	.byte	0                       # 0x0
	.zero	1
	.short	0                       # 0x0
	.short	14                      # 0xe
	.zero	2
	.quad	.L.str151
	.quad	.L.str152
	.byte	0                       # 0x0
	.zero	1
	.short	0                       # 0x0
	.short	13                      # 0xd
	.zero	2
	.quad	.L.str153
	.quad	.L.str148
	.byte	0                       # 0x0
	.zero	1
	.short	0                       # 0x0
	.short	8                       # 0x8
	.zero	2
	.size	builtin_array, 336

	.type	operator_array,@object  # @operator_array
	.align	16
operator_array:
	.quad	.L.str154
	.long	3                       # 0x3
	.long	17                      # 0x11
	.quad	.L.str155
	.long	6                       # 0x6
	.long	33                      # 0x21
	.quad	.L.str156
	.long	6                       # 0x6
	.long	9                       # 0x9
	.quad	.L.str157
	.long	5                       # 0x5
	.long	10                      # 0xa
	.quad	.L.str158
	.long	5                       # 0x5
	.long	16                      # 0x10
	.quad	.L.str159
	.long	3                       # 0x3
	.long	1                       # 0x1
	.quad	.L.str160
	.long	6                       # 0x6
	.long	25                      # 0x19
	.quad	.L.str161
	.long	2                       # 0x2
	.long	18                      # 0x12
	.quad	.L.str162
	.long	5                       # 0x5
	.long	34                      # 0x22
	.quad	.L.str163
	.long	3                       # 0x3
	.long	11                      # 0xb
	.quad	.L.str164
	.long	6                       # 0x6
	.long	35                      # 0x23
	.size	operator_array, 176

	.type	.L.str154,@object       # @.str154
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str154:
	.asciz	"and"
	.size	.L.str154, 4

	.type	.L.str155,@object       # @.str155
.L.str155:
	.asciz	"and_eq"
	.size	.L.str155, 7

	.type	.L.str156,@object       # @.str156
.L.str156:
	.asciz	"bitand"
	.size	.L.str156, 7

	.type	.L.str157,@object       # @.str157
.L.str157:
	.asciz	"bitor"
	.size	.L.str157, 6

	.type	.L.str158,@object       # @.str158
.L.str158:
	.asciz	"compl"
	.size	.L.str158, 6

	.type	.L.str159,@object       # @.str159
.L.str159:
	.asciz	"not"
	.size	.L.str159, 4

	.type	.L.str160,@object       # @.str160
.L.str160:
	.asciz	"not_eq"
	.size	.L.str160, 7

	.type	.L.str161,@object       # @.str161
.L.str161:
	.asciz	"or"
	.size	.L.str161, 3

	.type	.L.str162,@object       # @.str162
.L.str162:
	.asciz	"or_eq"
	.size	.L.str162, 6

	.type	.L.str163,@object       # @.str163
.L.str163:
	.asciz	"xor"
	.size	.L.str163, 4

	.type	.L.str164,@object       # @.str164
.L.str164:
	.asciz	"xor_eq"
	.size	.L.str164, 7

	.type	.L.str165,@object       # @.str165
.L.str165:
	.asciz	"ignoring duplicate directory \"%s\"\n"
	.size	.L.str165, 35

	.type	.L.str166,@object       # @.str166
.L.str166:
	.asciz	"changing search order for system directory \"%s\""
	.size	.L.str166, 48

	.type	.L.str167,@object       # @.str167
.L.str167:
	.asciz	"  as it is the same as non-system directory \"%s\""
	.size	.L.str167, 49

	.type	.L.str168,@object       # @.str168
.L.str168:
	.asciz	"  as it has already been specified as a non-system directory"
	.size	.L.str168, 61

	.type	.L.str169,@object       # @.str169
.L.str169:
	.asciz	"CPATH"
	.size	.L.str169, 6

	.type	.L.str170,@object       # @.str170
.L.str170:
	.asciz	"C_INCLUDE_PATH"
	.size	.L.str170, 15

	.type	.L.str171,@object       # @.str171
.L.str171:
	.asciz	"CPLUS_INCLUDE_PATH"
	.size	.L.str171, 19

	.type	.L.str172,@object       # @.str172
.L.str172:
	.asciz	"OBJC_INCLUDE_PATH"
	.size	.L.str172, 18

	.type	.L.str173,@object       # @.str173
.L.str173:
	.asciz	"OBJCPLUS_INCLUDE_PATH"
	.size	.L.str173, 22

	.type	lang_defaults,@object   # @lang_defaults
	.section	.rodata,"a",@progbits
	.align	16
lang_defaults:
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.byte	0                       # 0x0
	.byte	1                       # 0x1
	.byte	0                       # 0x0
	.size	lang_defaults, 80

	.type	init_library.initialized,@object # @init_library.initialized
	.local	init_library.initialized
	.comm	init_library.initialized,1,1

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
