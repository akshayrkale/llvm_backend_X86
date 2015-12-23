	.file	"cppmacro.c.bc"
	.text
	.globl	cpp_quote_string
	.align	16, 0x90
	.type	cpp_quote_string,@function
cpp_quote_string:                       # @cpp_quote_string
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
.Ltmp11:
	.cfi_offset %rbx, -48
.Ltmp12:
	.cfi_offset %r12, -40
.Ltmp13:
	.cfi_offset %r13, -32
.Ltmp14:
	.cfi_offset %r14, -24
.Ltmp15:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r14
	je	.LBB0_8
# BB#1:
	movq	$92, %r12
	jmp	.LBB0_2
	.align	16, 0x90
.LBB0_5:                                # %if.else
                                        #   in Loop: Header=BB0_2 Depth=1
	movzbl	_sch_istable(%rdx,%rdx), %eax
	testq	$16, %rax
	jne	.LBB0_6
# BB#9:                                 # %if.else13
                                        #   in Loop: Header=BB0_2 Depth=1
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str, %rsi
	callq	sprintf
	addq	$4, %rbx
	jmp	.LBB0_7
.LBB0_6:                                # %if.then11
                                        #   in Loop: Header=BB0_2 Depth=1
	movb	%dl, (%rbx)
	incq	%rbx
	jmp	.LBB0_7
	.align	16, 0x90
.LBB0_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r15), %edx
	decq	%r14
	incq	%r15
	cmpq	$34, %rdx
	je	.LBB0_4
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB0_2 Depth=1
	cmpq	$92, %rdx
	jne	.LBB0_5
.LBB0_4:                                # %if.then
                                        #   in Loop: Header=BB0_2 Depth=1
	movb	%r12b, (%rbx)
	movb	%dl, 1(%rbx)
	addq	$2, %rbx
.LBB0_7:                                # %while.cond.backedge
                                        #   in Loop: Header=BB0_2 Depth=1
	testq	%r13, %r14
	jne	.LBB0_2
.LBB0_8:                                # %while.end
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp16:
	.size	cpp_quote_string, .Ltmp16-cpp_quote_string
	.cfi_endproc

	.globl	_cpp_pop_context
	.align	16, 0x90
	.type	_cpp_pop_context,@function
_cpp_pop_context:                       # @_cpp_pop_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp20:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp21:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp22:
	.cfi_def_cfa_offset 32
.Ltmp23:
	.cfi_offset %rbx, -24
.Ltmp24:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	152(%r14), %rbx
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.LBB1_2
# BB#1:                                 # %if.then
	movzbl	21(%rax), %ecx
	andq	$223, %rcx
	movb	%cl, 21(%rax)
.LBB1_2:                                # %if.end
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB1_4
# BB#3:                                 # %if.then5
	movq	%r14, %rdi
	callq	_cpp_release_buff
.LBB1_4:                                # %if.end7
	movq	8(%rbx), %rax
	movq	%rax, 152(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp25:
	.size	_cpp_pop_context, .Ltmp25-_cpp_pop_context
	.cfi_endproc

	.globl	cpp_get_token
	.align	16, 0x90
	.type	cpp_get_token,@function
cpp_get_token:                          # @cpp_get_token
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
	subq	$224, %rsp
.Ltmp37:
	.cfi_def_cfa_offset 272
.Ltmp38:
	.cfi_offset %rbx, -48
.Ltmp39:
	.cfi_offset %r12, -40
.Ltmp40:
	.cfi_offset %r13, -32
.Ltmp41:
	.cfi_offset %r14, -24
.Ltmp42:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	%r12, 168(%rsp)         # 8-byte Spill
	leaq	344(%r12), %rax
	movq	%rax, 88(%rsp)          # 8-byte Spill
	leaq	80(%r12), %rax
	movq	%rax, 144(%rsp)         # 8-byte Spill
	leaq	368(%r12), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	leaq	296(%r12), %rax
	movq	%rax, 56(%rsp)          # 8-byte Spill
	leaq	320(%r12), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB2_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_27 Depth 2
                                        #     Child Loop BB2_189 Depth 2
                                        #     Child Loop BB2_192 Depth 2
                                        #     Child Loop BB2_11 Depth 2
                                        #       Child Loop BB2_52 Depth 3
                                        #     Child Loop BB2_71 Depth 2
                                        #       Child Loop BB2_78 Depth 3
                                        #         Child Loop BB2_97 Depth 4
                                        #     Child Loop BB2_128 Depth 2
                                        #     Child Loop BB2_171 Depth 2
	movq	152(%r12), %rbx
	movq	8(%rbx), %rax
	testq	%rax, %rax
	jne	.LBB2_21
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r12, %rdi
	callq	_cpp_lex_token
	movq	%rax, %r14
	jmp	.LBB2_3
	.align	16, 0x90
.LBB2_21:                               # %if.else
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	16(%rbx), %r14
	movq	24(%rbx), %rcx
	cmpq	%rcx, %r14
	jne	.LBB2_22
# BB#46:                                # %if.else18
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	40(%rbx), %rcx
	testq	%rcx, %rcx
	je	.LBB2_48
# BB#47:                                # %if.then.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	21(%rcx), %edx
	andq	$223, %rdx
	movb	%dl, 21(%rcx)
.LBB2_48:                               # %if.end.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB2_50
# BB#49:                                # %if.then5.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r12, %rdi
	callq	_cpp_release_buff
	movq	8(%rbx), %rax
.LBB2_50:                               # %_cpp_pop_context.exit
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	movzbl	8(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_1
	jmp	.LBB2_51
	.align	16, 0x90
.LBB2_22:                               # %if.then3
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	48(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB2_23
# BB#24:                                # %if.else8
                                        #   in Loop: Header=BB2_1 Depth=1
	leaq	8(%r14), %rax
	movq	%rax, 16(%rbx)
	movq	(%r14), %r14
	jmp	.LBB2_25
.LBB2_23:                               # %if.then5
                                        #   in Loop: Header=BB2_1 Depth=1
	leaq	24(%r14), %rax
	movq	%rax, 16(%rbx)
.LBB2_25:                               # %if.end
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	7(%r14), %eax
	testq	$8, %rax
	jne	.LBB2_26
	.align	16, 0x90
.LBB2_3:                                # %if.end25
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movzbl	6(%r14), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	cmpq	$56, %rax
	jne	.LBB2_222
# BB#4:                                 # %if.end29
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%r14), %rdi
	movzbl	20(%rdi), %eax
	movq	%rax, %rsi
	andq	$255, %rsi
	movq	$8, %rcx
	shrq	%cl, %rax
	cmpq	$1, %rsi
	jne	.LBB2_222
# BB#5:                                 # %if.end29
                                        #   in Loop: Header=BB2_1 Depth=1
	andq	$8192, %rdx             # imm = 0x2000
	testq	$65535, %rdx            # imm = 0xFFFF
	jne	.LBB2_222
# BB#6:                                 # %if.end41
                                        #   in Loop: Header=BB2_1 Depth=1
	testq	$32, %rax
	jne	.LBB2_220
# BB#7:                                 # %if.then46
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	15(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_222
# BB#8:                                 # %land.lhs.true
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rcx, %rcx
	movb	%cl, 184(%r12)
	movb	%cl, 10(%r12)
	testq	$4, %rax
	jne	.LBB2_182
# BB#9:                                 # %if.then.i121
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	24(%rdi), %rdx
	movzbl	26(%rdx), %ecx
	testq	$1, %rcx
	je	.LBB2_178
# BB#10:                                # %if.then3.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rdx, 80(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movq	$1, %rcx
	movb	%cl, 15(%r12)
	movl	244(%r12), %eax
	incq	%rax
	movl	%eax, 244(%r12)
	movb	%cl, 16(%r12)
	xorq	%rax, %rax
	jmp	.LBB2_11
	.align	16, 0x90
.LBB2_52:                               # %for.cond.i117.i
                                        #   Parent Loop BB2_1 Depth=1
                                        #     Parent Loop BB2_11 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB2_15
# BB#53:                                # %for.cond.i117.i
                                        #   in Loop: Header=BB2_52 Depth=3
	cmpq	$22, %rcx
	je	.LBB2_57
# BB#54:                                # %for.cond.i117.i
                                        #   in Loop: Header=BB2_52 Depth=3
	cmpq	$66, %rcx
	jne	.LBB2_16
# BB#55:                                # %lor.lhs.false.i122.i
                                        #   in Loop: Header=BB2_52 Depth=3
	movzbl	7(%rbx), %ecx
	testq	$1, %rcx
	jne	.LBB2_52
# BB#56:                                # %land.lhs.true.i123.i
                                        #   in Loop: Header=BB2_52 Depth=3
	cmpq	$0, 8(%rax)
	jne	.LBB2_52
	.align	16, 0x90
.LBB2_11:                               # %for.cond.i117.i.outer
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_52 Depth 3
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB2_52
# BB#12:                                # %for.cond.i117.i.us
                                        #   in Loop: Header=BB2_11 Depth=2
	movq	%r12, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$66, %rcx
	je	.LBB2_11
# BB#13:                                # %for.cond.i117.i.us
                                        #   in Loop: Header=BB2_1 Depth=1
	cmpq	$22, %rcx
	jne	.LBB2_14
.LBB2_57:                               # %if.then11.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$2, %rax
	movb	%al, 16(%r12)
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	collect_args
	movq	%rax, %rsi
	jmp	.LBB2_65
	.align	16, 0x90
.LBB2_182:                              # %if.end42.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	24(%rdi), %eax
	cmpq	$7, %rax
	ja	.LBB2_184
# BB#183:                               # %if.end42.i
                                        #   in Loop: Header=BB2_1 Depth=1
	jmpq	*.LJTI2_0(,%rax,8)
.LBB2_211:                              # %sw.bb29.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rdi, 96(%rsp)          # 8-byte Spill
	movzbl	302(%r12), %ecx
	cmpq	$67, %rcx
	jne	.LBB2_213
# BB#212:                               # %if.then35.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rdi, %rdi
	callq	time
	movq	%rax, 216(%rsp)
	leaq	216(%rsp), %rdi
	callq	localtime
	movq	%rax, %rbx
	movq	$12, %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, %rsi
	movq	%rsi, 312(%r12)
	movq	$11, %rax
	movl	%eax, 304(%r12)
	movq	$61, %rax
	movb	%al, 302(%r12)
	movabsq	$0, %rax
	movb	%al, 303(%r12)
	movq	%rax, %r15
	movslq	16(%rbx), %rax
	movq	monthnames(,%rax,8), %rdx
	movl	20(%rbx), %r8d
	movl	12(%rbx), %ecx
	addq	$1900, %r8              # imm = 0x76C
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movabsq	$.L.str18, %rsi
	callq	sprintf
	movq	$9, %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, %rsi
	movq	%rsi, 336(%r12)
	movq	$8, %rax
	movl	%eax, 328(%r12)
	movq	$61, %rax
	movb	%al, 326(%r12)
	movb	%r15b, 327(%r12)
	movl	(%rbx), %r8d
	movl	4(%rbx), %ecx
	movl	8(%rbx), %edx
	xorq	%rax, %rax
	movq	%rsi, %rdi
	movabsq	$.L.str19, %rsi
	callq	sprintf
	movq	96(%rsp), %rax          # 8-byte Reload
	movl	24(%rax), %eax
.LBB2_213:                              # %if.end73.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	andq	%r13, %rax
	cmpq	$1, %rax
	movq	56(%rsp), %r15          # 8-byte Reload
	je	.LBB2_215
# BB#214:                               # %if.end73.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	40(%rsp), %r15          # 8-byte Reload
.LBB2_215:                              # %if.end73.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	jmp	.LBB2_200
.LBB2_26:                               # %if.then12
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 208(%rsp)
	movq	152(%r12), %rbx
	.align	16, 0x90
.LBB2_27:                               # %do.body.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	48(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB2_28
# BB#29:                                # %if.else.i
                                        #   in Loop: Header=BB2_27 Depth=2
	movq	16(%rbx), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 16(%rbx)
	movq	(%rax), %r15
	jmp	.LBB2_30
	.align	16, 0x90
.LBB2_28:                               # %if.then.i111
                                        #   in Loop: Header=BB2_27 Depth=2
	movq	16(%rbx), %r15
	leaq	24(%r15), %rax
	movq	%rax, 16(%rbx)
.LBB2_30:                               # %if.end.i113
                                        #   in Loop: Header=BB2_27 Depth=2
	movzbl	6(%r15), %eax
	cmpq	$66, %rax
	je	.LBB2_223
# BB#31:                                # %if.end5.i
                                        #   in Loop: Header=BB2_27 Depth=2
	movq	%r12, %rdi
	leaq	208(%rsp), %rsi
	movq	%r15, %rdx
	callq	paste_tokens
	testq	%rax, %rax
	je	.LBB2_32
# BB#42:                                # %do.cond.i
                                        #   in Loop: Header=BB2_27 Depth=2
	movzbl	7(%r15), %eax
	testq	$8, %rax
	jne	.LBB2_27
	jmp	.LBB2_43
.LBB2_32:                               # %if.then6.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB2_37
# BB#33:                                # %if.then.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	240(%r12), %eax
	incq	%rax
	movl	%eax, 240(%r12)
	movq	232(%r12), %rcx
	movq	192(%r12), %rax
	movq	16(%rcx), %rdx
	addq	$-24, %rax
	cmpq	%rdx, %rax
	jne	.LBB2_36
# BB#34:                                # %land.lhs.true.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB2_36
# BB#35:                                # %if.then6.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rcx, 232(%r12)
	movq	24(%rcx), %rax
.LBB2_36:                               # %while.cond.backedge.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 192(%r12)
	jmp	.LBB2_40
.LBB2_185:                              # %sw.bb.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	56(%r12), %rcx
	cmpq	$3, %rax
	jne	.LBB2_186
# BB#187:                               # %while.cond.preheader.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movslq	16(%rcx), %rax
	testq	%rax, %rax
	js	.LBB2_191
# BB#188:                               # %while.body.lr.ph.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	168(%rsp), %rcx         # 8-byte Reload
	movq	24(%rcx), %rdx
	.align	16, 0x90
.LBB2_189:                              # %while.body.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rcx
	movslq	16(%rdx,%rcx,8), %rax
	testq	%rax, %rax
	jns	.LBB2_189
# BB#190:                               # %while.cond.if.end.loopexit_crit_edge.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	leaq	(%rdx,%rcx,8), %rcx
	jmp	.LBB2_191
.LBB2_37:                               # %if.else.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_38
# BB#39:                                # %if.else20.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-8, 16(%rax)
	jmp	.LBB2_40
.LBB2_204:                              # %sw.bb14.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	56(%r12), %rax
	movq	192(%r12), %rcx
	movl	-24(%rcx), %ecx
	movl	8(%rax), %ebx
	movl	12(%rax), %eax
	addq	%rcx, %rbx
	subq	%rax, %rbx
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movq	%rax, %r15
	movq	$21, %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, %r14
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str32, %rsi
	movq	%rbx, %rdx
	callq	sprintf
	movq	$57, %rax
	movb	%al, 6(%r15)
	movq	%r14, 16(%r15)
	movq	%r14, %rdi
	jmp	.LBB2_210
.LBB2_203:                              # %sw.bb11.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movl	44(%r12), %r14d
	decq	%r14
	jmp	.LBB2_209
.LBB2_205:                              # %sw.bb21.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	56(%r12), %rax
	testq	%rax, %rax
	movq	$1, %r14
	je	.LBB2_209
# BB#206:                               # %land.lhs.true.i.i144
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	21(%rax), %eax
	testq	%rax, %rax
	je	.LBB2_209
# BB#207:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	808(%r12), %rax
	movzbl	20(%rax), %eax
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB2_209
# BB#208:                               # %lor.rhs.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movabsq	$1, %r14
.LBB2_209:                              # %lor.end.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movq	%rax, %r15
	movq	$21, %rsi
	movq	%r12, %rdi
	callq	_cpp_unaligned_alloc
	movq	%rax, %rbx
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str32, %rsi
	movq	%r14, %rdx
	callq	sprintf
	movq	$57, %rax
	movb	%al, 6(%r15)
	movq	%rbx, 16(%r15)
	movq	%rbx, %rdi
.LBB2_210:                              # %sw.epilog.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	callq	strlen
	movl	%eax, 8(%r15)
	xorq	%rax, %rax
	movb	%al, 7(%r15)
	jmp	.LBB2_200
.LBB2_216:                              # %sw.bb82.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	8(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_222
# BB#217:                               # %if.end85.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r12, %rdi
	callq	_cpp_do__Pragma
	jmp	.LBB2_218
.LBB2_186:                              #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
.LBB2_191:                              # %if.end.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	(%rcx), %r15
	movq	%r15, %rdi
	callq	strlen
	movq	%rax, %r12
                                        # kill: RAX<def> R12<kill>
	andq	$1073741823, %rax       # imm = 0x3FFFFFFF
	leaq	1(,%rax,4), %rsi
	movq	168(%rsp), %rdi         # 8-byte Reload
	callq	_cpp_unaligned_alloc
	movq	%rax, %r14
	movq	%r13, %rcx
	testq	%rcx, %r12
	movq	%r14, %rbx
	movq	$92, %rax
	je	.LBB2_199
	.align	16, 0x90
.LBB2_192:                              # %while.body.i.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r15), %edx
	decq	%r12
	incq	%r15
	cmpq	$34, %rdx
	je	.LBB2_194
# BB#193:                               # %while.body.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	cmpq	$92, %rdx
	jne	.LBB2_195
.LBB2_194:                              # %if.then.i.i.i141
                                        #   in Loop: Header=BB2_192 Depth=2
	movb	%al, (%rbx)
	movb	%dl, 1(%rbx)
	addq	$2, %rbx
	jmp	.LBB2_198
	.align	16, 0x90
.LBB2_195:                              # %if.else.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	movzbl	_sch_istable(%rdx,%rdx), %eax
	testq	$16, %rax
	jne	.LBB2_196
# BB#225:                               # %if.else13.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str, %rsi
	movq	%rcx, %r13
	callq	sprintf
	movq	%r13, %rcx
	addq	$4, %rbx
	jmp	.LBB2_197
.LBB2_196:                              # %if.then11.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	movb	%dl, (%rbx)
	incq	%rbx
.LBB2_197:                              # %while.cond.backedge.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	movq	$92, %rax
.LBB2_198:                              # %while.cond.backedge.i.i.i
                                        #   in Loop: Header=BB2_192 Depth=2
	testq	%rcx, %r12
	jne	.LBB2_192
.LBB2_199:                              # %cpp_quote_string.exit.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	subq	%r14, %rbx
	movq	168(%rsp), %r12         # 8-byte Reload
	movq	%r12, %rdi
	movq	%rcx, %r15
	callq	_cpp_temp_token
	movq	%r15, %r13
	movq	%rax, %r15
	movq	%rbx, %rax
	andq	%r13, %rax
	xorq	%rcx, %rcx
	movb	%cl, (%r14,%rax)
	movq	$61, %rax
	movb	%al, 6(%r15)
	movl	%ebx, 8(%r15)
	movq	%r14, 16(%r15)
	movb	%cl, 7(%r15)
.LBB2_200:                              # %sw.epilog.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	120(%rsp), %r14         # 8-byte Reload
	jne	.LBB2_202
# BB#201:                               # %if.then.i.i.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$56, %rdi
	callq	xmalloc
	movq	152(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB2_202:                              # %push_token_context.exit.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	%r15, 16(%rax)
	addq	$24, %r15
	movq	%r15, 24(%rax)
	jmp	.LBB2_218
.LBB2_38:                               # %if.then17.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-24, 16(%rax)
.LBB2_40:                               # %_cpp_backup_tokens.exit.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	736(%r12), %eax
	cmpq	$9, %rax
	je	.LBB2_43
# BB#41:                                # %if.then8.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	208(%rsp), %rsi
	movq	%r12, %rdi
	callq	cpp_token_as_text
	movq	%r14, %rbx
	movq	%rax, %r14
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str39, %rsi
	movq	%r14, %rdx
	movq	%rbx, %r14
	callq	cpp_warning
.LBB2_43:                               # %do.end.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	movq	(%rax), %rax
	movq	208(%rsp), %rbx
	testq	%rax, %rax
	jne	.LBB2_45
# BB#44:                                # %if.then.i.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$56, %rdi
	callq	xmalloc
	movq	152(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB2_45:                               # %paste_all_tokens.exit
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	%rbx, 16(%rax)
	addq	$24, %rbx
	movq	%rbx, 24(%rax)
	jmp	.LBB2_218
.LBB2_14:                               # %for.cond.i117.i.us
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rbx, %rbx
	cmpq	$67, %rcx
	jne	.LBB2_16
.LBB2_15:                               # %lor.lhs.false19.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rsi, %rsi
	movq	48(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jne	.LBB2_65
.LBB2_16:                               # %if.then22.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB2_58
# BB#17:                                # %if.then.i.i128.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movl	240(%r12), %eax
	incq	%rax
	movl	%eax, 240(%r12)
	movq	232(%r12), %rcx
	movq	192(%r12), %rax
	movq	16(%rcx), %rdx
	addq	$-24, %rax
	cmpq	%rdx, %rax
	jne	.LBB2_20
# BB#18:                                # %land.lhs.true.i.i132.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB2_20
# BB#19:                                # %if.then6.i.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rcx, 232(%r12)
	movq	24(%rcx), %rax
.LBB2_20:                               # %while.cond.backedge.i.i135.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 192(%r12)
	jmp	.LBB2_61
.LBB2_58:                               # %if.else.i.i138.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	jne	.LBB2_59
# BB#60:                                # %if.else20.i.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-8, 16(%rax)
	jmp	.LBB2_61
.LBB2_59:                               # %if.then17.i.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	addq	$-24, 16(%rax)
.LBB2_61:                               # %_cpp_backup_tokens.exit.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rsi, %rsi
	testq	%rbx, %rbx
	je	.LBB2_65
# BB#62:                                # %if.then24.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	jne	.LBB2_64
# BB#63:                                # %if.then.i.i.i147.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$56, %rdi
	callq	xmalloc
	xorq	%rsi, %rsi
	movq	152(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB2_64:                               # %push_token_context.exit.i153.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	%rbx, 16(%rax)
	addq	$24, %rbx
	movq	%rbx, 24(%rax)
.LBB2_65:                               # %funlike_invocation_p.exit.i
                                        #   in Loop: Header=BB2_1 Depth=1
	xorq	%rax, %rax
	movb	%al, 16(%r12)
	movl	244(%r12), %eax
	decq	%rax
	movl	%eax, 244(%r12)
	movzbl	15(%r12), %eax
	decq	%rax
	movb	%al, 15(%r12)
	testq	%rsi, %rsi
	je	.LBB2_66
# BB#69:                                # %if.end24.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r15, %r10
	movq	%r10, 96(%rsp)          # 8-byte Spill
	movq	24(%r10), %rcx
	movzbl	24(%rcx), %eax
	testq	%rax, %rax
	je	.LBB2_177
# BB#70:                                # %if.then30.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	8(%rsi), %rax
	movq	%rax, 184(%rsp)         # 8-byte Spill
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movl	20(%rcx), %r9d
	movq	8(%rcx), %rdx
	xorq	%rsi, %rsi
	testq	%r9, %r9
	leaq	(%r9,%r9,2), %rax
	leaq	(%rdx,%rax,8), %r8
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	%r13, %rdi
	movq	%rcx, %r12
	movq	%r12, 112(%rsp)         # 8-byte Spill
	je	.LBB2_126
	.align	16, 0x90
.LBB2_71:                               # %for.body.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB2_78 Depth 3
                                        #         Child Loop BB2_97 Depth 4
	movzbl	6(%rdx), %eax
	movq	%rax, %rcx
	andq	$255, %rcx
	cmpq	$65, %rcx
	jne	.LBB2_125
# BB#72:                                # %if.then.i.i125
                                        #   in Loop: Header=BB2_71 Depth=2
	movl	8(%rdx), %r13d
	movq	$8, %rcx
	shrq	%cl, %rax
	decq	%r13
	andq	%rdi, %r13
	movq	$5, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	184(%rsp), %rsi         # 8-byte Reload
	addq	%rsi, %rbx
	movq	%rbx, 160(%rsp)         # 8-byte Spill
	testq	$4, %rax
	jne	.LBB2_73
# BB#117:                               # %if.else.i.i126
                                        #   in Loop: Header=BB2_71 Depth=2
	testq	$8, %rax
	jne	.LBB2_120
# BB#118:                               # %lor.lhs.false.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	8(%r12), %rax
	cmpq	%rax, %rdx
	jbe	.LBB2_121
# BB#119:                               # %land.lhs.true.i99.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movzbl	-17(%rdx), %eax
	testq	$8, %rax
	jne	.LBB2_120
.LBB2_121:                              # %if.else25.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	$5, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	184(%rsp), %rcx         # 8-byte Reload
	cmpq	$0, 8(%rcx,%rax)
	jne	.LBB2_123
# BB#122:                               # %if.then27.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	160(%rsp), %rsi         # 8-byte Reload
	movq	%r8, %rbx
	movq	%r9, %r12
	movq	%rdx, %r15
	callq	expand_arg
	movq	%r15, %rdx
	movq	%r12, %r9
	movq	%rbx, %r8
	movq	96(%rsp), %r10          # 8-byte Reload
	movq	112(%rsp), %r12         # 8-byte Reload
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
.LBB2_123:                              # %if.end28.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r13
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	28(%rax,%r13), %eax
	jmp	.LBB2_124
	.align	16, 0x90
.LBB2_73:                               # %if.then5.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	addq	$2, %r9
	movq	$5, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	cmpq	$0, 16(%rsi,%rbx)
	jne	.LBB2_74
# BB#75:                                # %if.then7.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	%rdx, 64(%rsp)          # 8-byte Spill
	movq	144(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$5, %rcx
	shlq	%cl, %r13
	movl	24(%rsi,%r13), %ecx
	movq	16(%rax), %rdx
	testq	%rcx, %rcx
	movq	168(%rsp), %r14         # 8-byte Reload
	jne	.LBB2_77
# BB#76:                                #   in Loop: Header=BB2_71 Depth=2
	movq	%r9, 72(%rsp)           # 8-byte Spill
	movq	%rdi, %r15
	jmp	.LBB2_116
.LBB2_120:                              # %if.then21.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	$5, %rcx
	shlq	%cl, %r13
	movq	184(%rsp), %rax         # 8-byte Reload
	movl	24(%rax,%r13), %eax
.LBB2_124:                              # %for.inc.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	leaq	1(%r9,%rax), %r9
	jmp	.LBB2_125
.LBB2_74:                               #   in Loop: Header=BB2_71 Depth=2
	movq	112(%rsp), %r12         # 8-byte Reload
	jmp	.LBB2_125
.LBB2_77:                               # %for.body.lr.ph.i.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	%rdx, 200(%rsp)         # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movq	%r9, 72(%rsp)           # 8-byte Spill
	leaq	24(%rsi,%r13), %rax
	movq	%rax, 136(%rsp)         # 8-byte Spill
	xorq	%rbx, %rbx
	xorq	%rdx, %rdx
	xorq	%rsi, %rsi
	jmp	.LBB2_78
.LBB2_82:                               #   in Loop: Header=BB2_78 Depth=3
	movq	%r12, %rdi
	movq	176(%rsp), %rsi         # 8-byte Reload
	jmp	.LBB2_112
	.align	16, 0x90
.LBB2_78:                               # %for.body.i.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        #     Parent Loop BB2_71 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB2_97 Depth 4
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	movq	160(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	movq	%rdi, %r12
	movq	(%rax,%rcx,8), %rdi
	movzbl	6(%rdi), %ecx
	addq	$-58, %rcx
	andq	$255, %rcx
	cmpq	$8, %rcx
	ja	.LBB2_84
# BB#79:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	$1, %rsi
	movq	$1, %rax
	shlq	%cl, %rax
	testq	$27, %rax
	jne	.LBB2_85
# BB#80:                                # %for.body.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	cmpq	$8, %rcx
	jne	.LBB2_84
# BB#81:                                # %if.then.i.i89.i
                                        #   in Loop: Header=BB2_78 Depth=3
	cmpq	$0, 176(%rsp)           # 8-byte Folded Reload
	jne	.LBB2_82
# BB#83:                                # %if.then3.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	8(%rdi), %rsi
	movq	%r12, %rdi
	jmp	.LBB2_112
	.align	16, 0x90
.LBB2_84:                               # %lor.rhs.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	xorq	%rsi, %rsi
.LBB2_85:                               # %lor.end.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%rbx, 192(%rsp)         # 8-byte Spill
	movq	%rdx, 152(%rsp)         # 8-byte Spill
	movq	%r14, %r15
	movq	%rdi, %r13
	movq	%rsi, %rbx
	callq	cpp_token_len
	movq	%rbx, %r8
	movq	%r12, %rbx
	andq	%rbx, %rax
	movq	$2, %rcx
	movq	%rax, %r12
	shlq	%cl, %r12
	testq	%r8, %r8
	jne	.LBB2_87
# BB#86:                                # %lor.end.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%rax, %r12
.LBB2_87:                               # %lor.end.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	144(%rsp), %rsi         # 8-byte Reload
	movq	(%rsi), %rax
	movq	24(%rax), %rcx
	movq	16(%rax), %rbx
	addq	$2, %r12
	movq	200(%rsp), %rdx         # 8-byte Reload
	subq	%rdx, %rcx
	cmpq	%r12, %rcx
	jae	.LBB2_89
# BB#88:                                # %if.then28.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	subq	%rbx, %rdx
	movq	%r15, %rbx
	movq	%rbx, %rdi
	movq	%rdx, %r15
	movq	%r12, %rdx
	movq	%r8, %r14
	callq	_cpp_extend_buff
	movq	%r14, %r8
	movq	%r15, %rdx
	movq	80(%rbx), %rax
	movq	%rbx, %r15
	movq	16(%rax), %rbx
	addq	%rbx, %rdx
.LBB2_89:                               # %if.end37.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%r13, %rcx
	cmpq	%rbx, %rdx
	je	.LBB2_95
# BB#90:                                # %if.then42.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	176(%rsp), %rsi         # 8-byte Reload
	testq	%rsi, %rsi
	movq	%rcx, %rax
	je	.LBB2_92
# BB#91:                                # %if.then42.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%rsi, %rax
.LBB2_92:                               # %if.then42.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movzbl	7(%rax), %eax
	testq	$1, %rax
	je	.LBB2_94
# BB#93:                                # %if.then49.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	$32, %rax
	movb	%al, (%rdx)
	incq	%rdx
.LBB2_94:                               # %if.end51.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%rdx, %rbx
.LBB2_95:                               # %if.end51.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	testq	%r8, %r8
	je	.LBB2_105
# BB#96:                                # %if.then53.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%r15, %r13
	movq	%r13, %rdi
	movq	%rcx, %r15
	movq	%r15, 128(%rsp)         # 8-byte Spill
	movq	%r12, %rsi
	callq	_cpp_get_buff
	movq	%rax, 200(%rsp)         # 8-byte Spill
	movq	16(%rax), %r12
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	cpp_spell_token
	movq	%rax, %r13
	subq	%r12, %r13
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %r13
	movq	$92, %rax
	je	.LBB2_104
	.align	16, 0x90
.LBB2_97:                               # %while.body.i.i.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        #     Parent Loop BB2_71 Depth=2
                                        #       Parent Loop BB2_78 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movzbl	(%r12), %edx
	decq	%r13
	incq	%r12
	cmpq	$34, %rdx
	je	.LBB2_99
# BB#98:                                # %while.body.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	cmpq	$92, %rdx
	jne	.LBB2_100
.LBB2_99:                               # %if.then.i.i.i95.i
                                        #   in Loop: Header=BB2_97 Depth=4
	movb	%al, (%rbx)
	movb	%dl, 1(%rbx)
	addq	$2, %rbx
	jmp	.LBB2_103
	.align	16, 0x90
.LBB2_100:                              # %if.else.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	movzbl	_sch_istable(%rdx,%rdx), %eax
	testq	$16, %rax
	jne	.LBB2_101
# BB#224:                               # %if.else13.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movabsq	$.L.str, %rsi
	movq	%rcx, %r15
	callq	sprintf
	movq	%r15, %rcx
	addq	$4, %rbx
	jmp	.LBB2_102
.LBB2_101:                              # %if.then11.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	movb	%dl, (%rbx)
	incq	%rbx
.LBB2_102:                              # %while.cond.backedge.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	movq	$92, %rax
.LBB2_103:                              # %while.cond.backedge.i.i.i.i
                                        #   in Loop: Header=BB2_97 Depth=4
	testq	%rcx, %r13
	jne	.LBB2_97
.LBB2_104:                              # %cpp_quote_string.exit.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	168(%rsp), %r14         # 8-byte Reload
	movq	%r14, %rdi
	movq	200(%rsp), %rsi         # 8-byte Reload
	callq	_cpp_release_buff
	movq	%rbx, 200(%rsp)         # 8-byte Spill
	movq	128(%rsp), %r15         # 8-byte Reload
	jmp	.LBB2_106
	.align	16, 0x90
.LBB2_105:                              # %if.else.i.i97.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	%r15, %r14
	movq	%r14, %rdi
	movq	%rcx, %r15
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	cpp_spell_token
	movq	%rax, 200(%rsp)         # 8-byte Spill
.LBB2_106:                              # %if.end63.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movzbl	6(%r15), %eax
	xorq	%rsi, %rsi
	cmpq	$60, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rdx, %rdi
	movq	192(%rsp), %rbx         # 8-byte Reload
	jne	.LBB2_107
# BB#108:                               # %land.lhs.true.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movzbl	8(%r15), %eax
	cmpq	$92, %rax
	jne	.LBB2_110
# BB#109:                               #   in Loop: Header=BB2_78 Depth=3
	movq	152(%rsp), %rdx         # 8-byte Reload
	incq	%rdx
	jmp	.LBB2_111
	.align	16, 0x90
.LBB2_107:                              #   in Loop: Header=BB2_78 Depth=3
	xorq	%rdx, %rdx
	jmp	.LBB2_112
	.align	16, 0x90
.LBB2_110:                              # %select.mid
                                        #   in Loop: Header=BB2_78 Depth=3
	xorq	%rdx, %rdx
.LBB2_111:                              # %select.end
                                        #   in Loop: Header=BB2_78 Depth=3
	xorq	%rsi, %rsi
.LBB2_112:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB2_78 Depth=3
	movq	136(%rsp), %rax         # 8-byte Reload
	movl	(%rax), %eax
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rdi, %rcx
	cmpq	%rax, %rcx
	jb	.LBB2_78
# BB#113:                               # %for.end.i.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	%rdi, %r15
	testq	$1, %rdx
	jne	.LBB2_115
# BB#114:                               #   in Loop: Header=BB2_71 Depth=2
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	200(%rsp), %rdx         # 8-byte Reload
	jmp	.LBB2_116
.LBB2_115:                              # %if.then79.i.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	xorq	%rax, %rax
	movq	%r14, %rdi
	movabsq	$.L.str33, %rsi
	callq	cpp_warning
	movq	200(%rsp), %rdx         # 8-byte Reload
	decq	%rdx
	movq	184(%rsp), %rsi         # 8-byte Reload
	movq	24(%rsp), %rbx          # 8-byte Reload
.LBB2_116:                              # %stringify_arg.exit.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	movq	%rsi, 184(%rsp)         # 8-byte Spill
	movq	80(%r14), %rax
	movq	%r14, %rdi
	movq	16(%rax), %r12
	leaq	16(%rsi,%rbx), %r14
	movq	%rdx, %rbx
	subq	%r12, %rbx
	leaq	1(%rdx), %rcx
	movq	%rcx, 16(%rax)
	movq	%rdx, %r13
	callq	_cpp_temp_token
	movq	%rbx, %rcx
	movq	%r15, %rdi
	andq	%rdi, %rcx
	subq	%rbx, %rcx
	xorq	%rsi, %rsi
	movb	%sil, (%r13,%rcx)
	movq	$61, %rcx
	movb	%cl, 6(%rax)
	movl	%ebx, 8(%rax)
	movq	%r12, 16(%rax)
	movb	%sil, 7(%rax)
	movq	%rax, (%r14)
	movq	120(%rsp), %r14         # 8-byte Reload
	movq	96(%rsp), %r10          # 8-byte Reload
	movq	112(%rsp), %r12         # 8-byte Reload
	movq	104(%rsp), %r8          # 8-byte Reload
	movq	72(%rsp), %r9           # 8-byte Reload
	movq	64(%rsp), %rdx          # 8-byte Reload
	.align	16, 0x90
.LBB2_125:                              # %for.inc.i.i
                                        #   in Loop: Header=BB2_71 Depth=2
	addq	$24, %rdx
	cmpq	%r8, %rdx
	movq	%r9, %rsi
	jb	.LBB2_71
.LBB2_126:                              # %for.end.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r8, 104(%rsp)          # 8-byte Spill
	movq	%r12, 112(%rsp)         # 8-byte Spill
	movq	%r10, 96(%rsp)          # 8-byte Spill
	movq	%r14, 120(%rsp)         # 8-byte Spill
	andq	%rdi, %rsi
	movq	$3, %rcx
	shlq	%cl, %rsi
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%r8, %rbx
	callq	_cpp_get_buff
	movq	%rbx, %rcx
	movq	%rax, 192(%rsp)         # 8-byte Spill
	movq	8(%rax), %r14
	movq	%r14, 200(%rsp)         # 8-byte Spill
	movq	8(%r12), %rbx
	cmpq	%rcx, %rbx
	jae	.LBB2_170
# BB#127:                               # %for.body40.i.i.lr.ph
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	200(%rsp), %r12         # 8-byte Reload
	jmp	.LBB2_128
.LBB2_136:                              # %if.then79.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	24(%r9,%rdx), %r15d
	movq	(%rsi), %rsi
	movq	200(%rsp), %rcx         # 8-byte Reload
	cmpq	%rcx, %r12
	jne	.LBB2_138
# BB#137:                               #   in Loop: Header=BB2_128 Depth=2
	xorq	%r13, %r13
	movq	%rcx, %r12
	jmp	.LBB2_151
.LBB2_138:                              # %if.then84.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	-8(%r12), %rcx
	movzbl	6(%rcx), %edx
	leaq	-8(%r12), %rcx
	cmpq	$21, %rdx
	jne	.LBB2_146
# BB#139:                               # %land.lhs.true91.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	112(%rsp), %rdx         # 8-byte Reload
	movzbl	26(%rdx), %edx
	testq	$2, %rdx
	je	.LBB2_146
# BB#140:                               # %land.lhs.true95.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	112(%rsp), %rdx         # 8-byte Reload
	movzbl	24(%rdx), %edx
	andq	%r8, %rax
	cmpq	%rdx, %rax
	jne	.LBB2_146
# BB#141:                               # %if.then101.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	xorq	%r13, %r13
	movq	%r15, %rax
	andq	%r8, %rax
	je	.LBB2_143
# BB#142:                               # %if.then101.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%rcx, %r13
.LBB2_143:                              # %if.then101.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	testq	%rax, %rax
	je	.LBB2_145
# BB#144:                               # %if.then101.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%r12, %rcx
.LBB2_145:                              # %if.then101.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%rcx, %r12
	jmp	.LBB2_151
.LBB2_146:                              # %if.else109.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	testq	%r8, %r15
	je	.LBB2_148
# BB#147:                               # %if.else109.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	xorq	%rcx, %rcx
.LBB2_148:                              # %if.else109.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%rcx, %r13
	jmp	.LBB2_151
	.align	16, 0x90
.LBB2_128:                              # %for.body40.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	6(%rbx), %edi
	movq	%rdi, %rax
	andq	$255, %rax
	cmpq	$65, %rax
	jne	.LBB2_129
# BB#130:                               # %if.end49.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movl	8(%rbx), %eax
	movq	$8, %rcx
	shrq	%cl, %rdi
	leaq	-1(%rax), %rdx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rdx
	movq	$5, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	testq	$4, %rdi
	movq	184(%rsp), %r9          # 8-byte Reload
	jne	.LBB2_131
# BB#132:                               # %if.else61.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	addq	%r9, %rsi
	testq	$8, %rdi
	jne	.LBB2_133
# BB#134:                               # %if.else69.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	112(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx), %rcx
	cmpq	%rcx, %rbx
	je	.LBB2_149
# BB#135:                               # %land.lhs.true73.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movzbl	-17(%rbx), %ecx
	testq	$8, %rcx
	jne	.LBB2_136
.LBB2_149:                              # %if.else117.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	28(%r9,%rdx), %r15d
	movq	8(%r9,%rdx), %rsi
	jmp	.LBB2_150
	.align	16, 0x90
.LBB2_129:                              # %if.then47.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%rbx, (%r12)
	addq	$8, %r12
	movq	%r12, %r14
	jmp	.LBB2_169
	.align	16, 0x90
.LBB2_131:                              # %if.then59.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	$5, %rcx
	shlq	%cl, %rdx
	leaq	16(%r9,%rdx), %rsi
	xorq	%r13, %r13
	movq	$1, %r15
	jmp	.LBB2_151
.LBB2_133:                              # %if.then66.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	24(%r9,%rdx), %r15d
	movq	(%rsi), %rsi
.LBB2_150:                              # %if.end122.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	xorq	%r13, %r13
.LBB2_151:                              # %if.end122.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	168(%rsp), %rax         # 8-byte Reload
	movzbl	8(%rax), %eax
	testq	%rax, %rax
	jne	.LBB2_156
# BB#152:                               # %land.lhs.true124.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	112(%rsp), %rax         # 8-byte Reload
	movq	8(%rax), %rax
	cmpq	%rax, %rbx
	je	.LBB2_156
# BB#153:                               # %land.lhs.true128.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%r9, %r14
	movzbl	-17(%rbx), %eax
	testq	$8, %rax
	jne	.LBB2_155
# BB#154:                               # %if.then134.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	168(%rsp), %rdi         # 8-byte Reload
	movq	%rsi, 176(%rsp)         # 8-byte Spill
	callq	_cpp_temp_token
	movq	176(%rsp), %rsi         # 8-byte Reload
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	$66, %rcx
	movb	%cl, 6(%rax)
	movq	%rbx, 8(%rax)
	xorq	%rcx, %rcx
	movb	%cl, 7(%rax)
	movq	%rax, (%r12)
	addq	$8, %r12
.LBB2_155:                              # %if.end137.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%r14, %r9
.LBB2_156:                              # %if.end137.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	%r9, 184(%rsp)          # 8-byte Spill
	testq	%r8, %r15
	jne	.LBB2_158
# BB#157:                               #   in Loop: Header=BB2_128 Depth=2
	movq	%r12, %r14
	jmp	.LBB2_160
	.align	16, 0x90
.LBB2_158:                              # %if.then139.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	andq	%r8, %r15
	movq	$3, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	%r12, %rdi
	callq	memcpy
	movzbl	7(%rbx), %eax
	leaq	(%r12,%r15,8), %r14
	testq	$8, %rax
	je	.LBB2_160
# BB#159:                               # %if.then148.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	leaq	-8(%r12,%r15,8), %r13
.LBB2_160:                              # %if.end151.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	168(%rsp), %rdi         # 8-byte Reload
	movzbl	8(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB2_163
# BB#161:                               # %land.lhs.true155.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movzbl	7(%rbx), %eax
	testq	$8, %rax
	jne	.LBB2_163
# BB#162:                               # %if.then160.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movq	88(%rsp), %rax          # 8-byte Reload
	movq	%rax, (%r14)
	addq	$8, %r14
.LBB2_163:                              # %if.end162.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	testq	%r13, %r13
	jne	.LBB2_165
# BB#164:                               #   in Loop: Header=BB2_128 Depth=2
	movq	104(%rsp), %rcx         # 8-byte Reload
	jmp	.LBB2_169
	.align	16, 0x90
.LBB2_165:                              # %if.then164.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	callq	_cpp_temp_token
	movq	(%r13), %rcx
	movzbl	6(%rcx), %ecx
	movb	%cl, 6(%rax)
	movq	(%r13), %rcx
	movq	8(%rcx), %rdx
	movq	16(%rcx), %rcx
	movq	%rcx, 16(%rax)
	movq	%rdx, 8(%rax)
	movq	(%r13), %rcx
	movzbl	7(%rbx), %edx
	movzbl	7(%rcx), %ecx
	testq	$8, %rdx
	jne	.LBB2_166
# BB#167:                               # %if.else182.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	andq	$247, %rcx
	jmp	.LBB2_168
.LBB2_166:                              # %if.then177.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	orq	$8, %rcx
.LBB2_168:                              # %if.end188.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	movb	%cl, 7(%rax)
	movq	104(%rsp), %rcx         # 8-byte Reload
	movq	%rax, (%r13)
.LBB2_169:                              # %for.inc190.i.i
                                        #   in Loop: Header=BB2_128 Depth=2
	addq	$24, %rbx
	cmpq	%rcx, %rbx
	movq	%r14, %r12
	jb	.LBB2_128
.LBB2_170:                              # %for.cond193.i.i.preheader
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	112(%rsp), %rax         # 8-byte Reload
	movzbl	24(%rax), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	movq	168(%rsp), %r12         # 8-byte Reload
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB2_174
	.align	16, 0x90
.LBB2_171:                              # %for.body198.i.i
                                        #   Parent Loop BB2_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdx
	andq	%rsi, %rdx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movq	184(%rsp), %rcx         # 8-byte Reload
	movq	8(%rcx,%rdx), %rdi
	testq	%rdi, %rdi
	je	.LBB2_173
# BB#172:                               # %if.then203.i.i
                                        #   in Loop: Header=BB2_171 Depth=2
	movq	%rsi, %r15
	callq	free
	movq	%r15, %rsi
	movq	112(%rsp), %rax         # 8-byte Reload
	movzbl	24(%rax), %eax
.LBB2_173:                              # %for.inc208.i.i
                                        #   in Loop: Header=BB2_171 Depth=2
	incq	%rbx
	movq	%rbx, %rcx
	andq	%rsi, %rcx
	movq	%rax, %rdx
	andq	$65535, %rdx            # imm = 0xFFFF
	cmpq	%rdx, %rcx
	jb	.LBB2_171
.LBB2_174:                              # %for.end209.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	152(%r12), %rax
	movq	(%rax), %rax
	movq	200(%rsp), %rdx         # 8-byte Reload
	subq	%rdx, %r14
	movq	$3, %rcx
	shrq	%cl, %r14
	testq	%rax, %rax
	jne	.LBB2_176
# BB#175:                               # %if.then.i.i308.i.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	$56, %rdi
	movq	%rdx, %r15
	movq	%rsi, %rbx
	callq	xmalloc
	movq	%rbx, %rsi
	movq	%r15, %rdx
	movq	152(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB2_176:                              # %replace_args.exit.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	xorq	%rcx, %rcx
	movb	%cl, 48(%rax)
	movq	96(%rsp), %r10          # 8-byte Reload
	movq	%r10, 40(%rax)
	movq	192(%rsp), %rcx         # 8-byte Reload
	movq	%rcx, 32(%rax)
	movq	%rdx, 16(%rax)
	andq	%rsi, %r14
	movq	%rsi, %r13
	leaq	(%rdx,%r14,8), %rcx
	movq	%rcx, 24(%rax)
	movq	120(%rsp), %r14         # 8-byte Reload
	movq	32(%rsp), %rsi          # 8-byte Reload
.LBB2_177:                              # %if.end31.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r10, %rbx
	movq	%r12, %rdi
	callq	_cpp_release_buff
	movq	%rbx, %rdi
	movzbl	21(%rdi), %eax
	movq	80(%rsp), %rdx          # 8-byte Reload
.LBB2_178:                              # %if.end32.i
                                        #   in Loop: Header=BB2_1 Depth=1
	orq	$32, %rax
	movb	%al, 21(%rdi)
	movzbl	24(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB2_218
# BB#179:                               # %if.then40.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%r14, 120(%rsp)         # 8-byte Spill
	movq	152(%r12), %rax
	movq	8(%rdx), %rbx
	movq	(%rax), %rax
	movl	20(%rdx), %edx
	testq	%rax, %rax
	jne	.LBB2_181
# BB#180:                               # %if.then.i.i73.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rdi, %r14
	movq	$56, %rdi
	movq	%rdx, %r15
	callq	xmalloc
	movq	%r15, %rdx
	movq	%r14, %rdi
	movq	152(%r12), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB2_181:                              # %push_token_context.exit.i
                                        #   in Loop: Header=BB2_1 Depth=1
	movq	%rax, 152(%r12)
	movq	$1, %rcx
	movb	%cl, 48(%rax)
	movq	%rdi, 40(%rax)
	movq	$0, 32(%rax)
	movq	%rbx, 16(%rax)
	leaq	(%rdx,%rdx,2), %rcx
	leaq	(%rbx,%rcx,8), %rcx
	movq	%rcx, 24(%rax)
	movq	120(%rsp), %r14         # 8-byte Reload
.LBB2_218:                              # %if.then51
                                        #   in Loop: Header=BB2_1 Depth=1
	movzbl	8(%r12), %eax
	testq	%rax, %rax
	jne	.LBB2_1
# BB#219:                               # %if.end15
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movq	$66, %rcx
	movb	%cl, 6(%rax)
	movq	%r14, 8(%rax)
	xorq	%rcx, %rcx
	movb	%cl, 7(%rax)
	jmp	.LBB2_221
.LBB2_184:                              # %sw.default.i.i
	movq	8(%rdi), %rdx
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_ice
	jmp	.LBB2_222
.LBB2_220:                              # %if.else59
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movzbl	6(%r14), %ecx
	movb	%cl, 6(%rax)
	movzbl	7(%r14), %ecx
	leaq	8(%r14), %rdx
	orq	$32, %rcx
	movb	%cl, 7(%rax)
	movq	(%rdx), %rcx
	movq	8(%rdx), %rdx
	movq	%rdx, 16(%rax)
	movq	%rcx, 8(%rax)
.LBB2_221:                              # %return
	movq	%rax, %r14
	jmp	.LBB2_222
.LBB2_51:
	movq	88(%rsp), %r14          # 8-byte Reload
	jmp	.LBB2_222
.LBB2_66:                               # %if.then14.i
	movzbl	761(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_222
# BB#67:                                # %land.lhs.true.i
	movq	24(%r15), %rax
	movzbl	26(%rax), %eax
	testq	$4, %rax
	jne	.LBB2_222
# BB#68:                                # %if.then23.i
	movq	8(%r15), %rdx
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_warning
.LBB2_222:                              # %return
	movq	%r14, %rax
	addq	$224, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_223:                              # %if.then4.i
	callq	abort
.Ltmp43:
	.size	cpp_get_token, .Ltmp43-cpp_get_token
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI2_0:
	.quad	.LBB2_204
	.quad	.LBB2_211
	.quad	.LBB2_185
	.quad	.LBB2_185
	.quad	.LBB2_203
	.quad	.LBB2_211
	.quad	.LBB2_205
	.quad	.LBB2_216

	.text
	.globl	cpp_sys_macro_p
	.align	16, 0x90
	.type	cpp_sys_macro_p,@function
cpp_sys_macro_p:                        # @cpp_sys_macro_p
	.cfi_startproc
# BB#0:                                 # %entry
	movq	152(%rdi), %rax
	movq	40(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB3_3
# BB#1:                                 # %land.lhs.true
	movq	24(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB3_3
# BB#2:                                 # %land.rhs
	movzbl	26(%rcx), %eax
	andq	$4, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
.LBB3_3:                                # %land.end
	retq
.Ltmp44:
	.size	cpp_sys_macro_p, .Ltmp44-cpp_sys_macro_p
	.cfi_endproc

	.globl	cpp_scan_nooutput
	.align	16, 0x90
	.type	cpp_scan_nooutput,@function
cpp_scan_nooutput:                      # @cpp_scan_nooutput
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp47:
	.cfi_def_cfa_offset 16
.Ltmp48:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	.align	16, 0x90
.LBB4_1:                                # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	jne	.LBB4_1
# BB#2:                                 # %while.end
	popq	%rbx
	retq
.Ltmp49:
	.size	cpp_scan_nooutput, .Ltmp49-cpp_scan_nooutput
	.cfi_endproc

	.globl	_cpp_backup_tokens
	.align	16, 0x90
	.type	_cpp_backup_tokens,@function
_cpp_backup_tokens:                     # @_cpp_backup_tokens
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp51:
	.cfi_def_cfa_offset 16
	movq	152(%rdi), %rax
	cmpq	$0, 8(%rax)
	je	.LBB5_1
# BB#8:                                 # %if.else
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rsi
	cmpq	$1, %rsi
	jne	.LBB5_13
# BB#9:                                 # %if.end14
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB5_11
# BB#10:                                # %if.then17
	addq	$-24, 16(%rax)
	popq	%rax
	retq
.LBB5_1:                                # %if.then
	movl	240(%rdi), %eax
	addq	%rsi, %rax
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	testq	%r8, %rsi
	movl	%eax, 240(%rdi)
	je	.LBB5_12
# BB#2:                                 # %while.body.lr.ph
	movq	192(%rdi), %rcx
	movq	232(%rdi), %rax
	.align	16, 0x90
.LBB5_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	16(%rax), %rdx
	addq	$-24, %rcx
	cmpq	%rdx, %rcx
	jne	.LBB5_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	8(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB5_6
# BB#5:                                 # %if.then6
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	%rdx, 232(%rdi)
	movq	24(%rdx), %rcx
	movq	%rdx, %rax
.LBB5_6:                                # %while.cond.backedge
                                        #   in Loop: Header=BB5_3 Depth=1
	decq	%rsi
	testq	%r8, %rsi
	jne	.LBB5_3
# BB#7:                                 # %while.cond.if.end25.loopexit_crit_edge
	movq	%rcx, 192(%rdi)
	popq	%rax
	retq
.LBB5_11:                               # %if.else20
	addq	$-8, 16(%rax)
.LBB5_12:                               # %if.end25
	popq	%rax
	retq
.LBB5_13:                               # %if.then13
	callq	abort
.Ltmp52:
	.size	_cpp_backup_tokens, .Ltmp52-_cpp_backup_tokens
	.cfi_endproc

	.globl	_cpp_free_definition
	.align	16, 0x90
	.type	_cpp_free_definition,@function
_cpp_free_definition:                   # @_cpp_free_definition
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rax, %rax
	movb	%al, 20(%rdi)
	movzbl	21(%rdi), %eax
	andq	$219, %rax
	movb	%al, 21(%rdi)
	retq
.Ltmp53:
	.size	_cpp_free_definition, .Ltmp53-_cpp_free_definition
	.cfi_endproc

	.globl	_cpp_create_definition
	.align	16, 0x90
	.type	_cpp_create_definition,@function
_cpp_create_definition:                 # @_cpp_create_definition
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp65:
	.cfi_def_cfa_offset 80
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r14
	movq	$32, %rsi
                                        # kill: RDI<def> R14<kill>
	callq	_cpp_aligned_alloc
	movq	%rax, %r13
	movl	68(%r14), %eax
	movl	%eax, 16(%r13)
	movq	$0, (%r13)
	xorq	%rax, %rax
	movw	%ax, 24(%r13)
	movzbl	26(%r13), %ecx
	movl	%eax, 20(%r13)
	andq	$252, %rcx
	movb	%cl, 26(%r13)
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movq	%rax, %r12
	movzbl	6(%r12), %eax
	movq	%rax, %rdx
	andq	$255, %rdx
	movq	$8, %rcx
	shrq	%cl, %rax
	cmpq	$67, %rdx
	je	.LBB7_42
# BB#1:                                 # %entry
	cmpq	$22, %rdx
	jne	.LBB7_40
# BB#2:                                 # %land.lhs.true
	testq	$1, %rax
	je	.LBB7_3
.LBB7_40:                               # %land.lhs.true21
	testq	$1, %rax
	jne	.LBB7_42
# BB#41:                                # %if.then26
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_pedwarn
.LBB7_42:                               # %if.end28
	movzbl	26(%r13), %eax
	movq	192(%r14), %r15
	testq	$1, %rax
	je	.LBB7_44
# BB#43:                                # %if.then34
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	lex_expansion_token
	jmp	.LBB7_47
.LBB7_44:                               # %if.else36
	movq	72(%r14), %rax
	movq	16(%rax), %rdx
	movq	24(%rax), %rsi
	movl	20(%r13), %eax
	subq	%rdx, %rsi
	leaq	1(%rax), %rdi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	leaq	(%rdi,%rdi,2), %rcx
	cmpq	%rcx, %rsi
	jae	.LBB7_46
# BB#45:                                # %if.then.i325
	leaq	72(%r14), %rsi
	movq	$24, %rdx
	movq	%r14, %rdi
	callq	_cpp_extend_buff
	movq	72(%r14), %rcx
	movl	20(%r13), %eax
	movq	16(%rcx), %rdx
.LBB7_46:                               # %alloc_expansion_token.exit
	leaq	1(%rax), %rcx
	movl	%ecx, 20(%r13)
	movq	16(%r12), %rcx
	andq	%rax, %rbx
	leaq	(%rbx,%rbx,2), %rsi
	movq	%rcx, 16(%rdx,%rsi,8)
	movq	(%r12), %rcx
	movq	8(%r12), %rdi
	leaq	(%rdx,%rsi,8), %rax
	movq	%rdi, 8(%rdx,%rsi,8)
	movq	%rcx, (%rdx,%rsi,8)
.LBB7_47:                               # %for.cond.preheader
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB7_48
	.align	16, 0x90
.LBB7_63:                               # %if.end122
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	lex_expansion_token
.LBB7_48:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	20(%r13), %edx
	cmpq	$2, %rdx
	jb	.LBB7_53
# BB#49:                                # %land.lhs.true42
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	-18(%rax), %esi
	movq	%rsi, %rdi
	andq	$255, %rdi
	movq	$8, %rcx
	shrq	%cl, %rsi
	cmpq	$40, %rdi
	jne	.LBB7_53
# BB#50:                                # %land.lhs.true49
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	26(%r13), %ecx
	testq	$1, %rcx
	je	.LBB7_53
# BB#51:                                # %if.then55
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	6(%rax), %edi
	movq	%rdi, %rcx
	andq	$255, %rcx
	cmpq	$65, %rcx
	jne	.LBB7_59
# BB#52:                                # %if.then61
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	$8, %rcx
	shrq	%cl, %rdi
	andq	$250, %rdi
	andq	$1, %rsi
	orq	%rdi, %rsi
	orq	$4, %rsi
	movb	%sil, 7(%rax)
	movq	16(%rax), %rcx
	leaq	-24(%rax), %rdx
	movq	%rcx, 16(%rdx)
	movq	(%rax), %rcx
	movq	8(%rax), %rsi
	movq	%rsi, 8(%rdx)
	movq	%rcx, (%rdx)
	movl	20(%r13), %edx
	decq	%rdx
	movl	%edx, 20(%r13)
	jmp	.LBB7_53
.LBB7_59:                               # %if.else80
                                        #   in Loop: Header=BB7_48 Depth=1
	movl	736(%r14), %ecx
	cmpq	$9, %rcx
	jne	.LBB7_60
	.align	16, 0x90
.LBB7_53:                               # %if.end86
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	6(%rax), %eax
	cmpq	$67, %rax
	je	.LBB7_64
# BB#54:                                # %if.end86
                                        #   in Loop: Header=BB7_48 Depth=1
	cmpq	$41, %rax
	jne	.LBB7_63
# BB#55:                                # %if.then99
                                        #   in Loop: Header=BB7_48 Depth=1
	decq	%rdx
	testq	%rbx, %rdx
	movl	%edx, 20(%r13)
	je	.LBB7_58
# BB#56:                                # %if.end106
                                        #   in Loop: Header=BB7_48 Depth=1
	movq	%r14, %rdi
	movq	%r13, %rsi
	callq	lex_expansion_token
	movl	20(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB7_58
# BB#57:                                # %lor.lhs.false
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	je	.LBB7_58
# BB#62:                                # %if.end116
                                        #   in Loop: Header=BB7_48 Depth=1
	movzbl	-17(%rax), %ecx
	orq	$8, %rcx
	movb	%cl, -17(%rax)
	jmp	.LBB7_63
.LBB7_64:                               # %for.end
	movq	%r15, 16(%rsp)          # 8-byte Spill
	movq	72(%r14), %rax
	movq	16(%rax), %rsi
	movq	%rsi, 8(%r13)
	decq	%rdx
	testq	%rbx, %rdx
	movl	%edx, 20(%r13)
	movq	24(%rsp), %r9           # 8-byte Reload
	je	.LBB7_65
# BB#66:                                # %if.end137
	movzbl	7(%rsi), %ecx
	andq	$254, %rcx
	movb	%cl, 7(%rsi)
	movq	%rdx, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	leaq	(%rcx,%rcx,2), %rcx
	leaq	(%rsi,%rcx,8), %rcx
	movq	%rcx, 16(%rax)
	jne	.LBB7_71
# BB#67:                                # %land.lhs.true147
	movzbl	26(%r13), %eax
	testq	$1, %rax
	jne	.LBB7_71
# BB#68:                                # %land.lhs.true153
	movzbl	6(%rsi), %eax
	cmpq	$56, %rax
	jne	.LBB7_71
# BB#69:                                # %land.lhs.true161
	movq	8(%rsi), %rax
	cmpq	%r9, %rax
	jne	.LBB7_71
# BB#70:                                # %if.then167
	movzbl	21(%r9), %eax
	orq	$32, %rax
	movb	%al, 21(%r9)
	jmp	.LBB7_71
.LBB7_58:                               # %if.then115
	movabsq	$.L.str3, %rsi
.LBB7_61:                               # %cleanup1
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r14, %rdi
	callq	cpp_error
	jmp	.LBB7_95
.LBB7_65:                               # %if.end137.thread
	movq	%rdx, %rcx
	andq	%rbx, %rcx
	leaq	(%rcx,%rcx,2), %rcx
	leaq	(%rsi,%rcx,8), %rcx
	movq	%rcx, 16(%rax)
.LBB7_71:                               # %if.end172
	movq	56(%r14), %rax
	movzbl	21(%rax), %eax
	testq	%rax, %rax
	je	.LBB7_72
# BB#73:                                # %if.end172
	movabsq	$1, %rax
	jmp	.LBB7_74
.LBB7_72:
	movabsq	$0, %rax
.LBB7_74:                               # %if.end172
	movzbl	26(%r13), %edi
	movq	$2, %rcx
	shlq	%cl, %rax
	movq	%rdi, %rcx
	andq	$251, %rcx
	orq	%rax, %rcx
	movb	%cl, 26(%r13)
	movzbl	20(%r9), %eax
	movq	$8, %rcx
	movq	%rax, %r15
	shrq	%cl, %r15
	testq	$255, %rax
	je	.LBB7_92
# BB#75:                                # %if.then184
	testq	$16, %r15
	jne	.LBB7_89
# BB#76:                                # %if.end.i330
	movq	24(%r9), %r8
	movl	20(%r8), %eax
	movq	%rdx, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB7_89
# BB#77:                                # %lor.lhs.false.i331
	movzbl	24(%r8), %eax
	movzbl	24(%r13), %ecx
	cmpq	%rcx, %rax
	jne	.LBB7_89
# BB#78:                                # %lor.lhs.false8.i
	movzbl	26(%r8), %ecx
	xorq	%rcx, %rdi
	testq	$3, %rdi
	jne	.LBB7_89
# BB#79:                                # %for.cond.preheader.i
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	je	.LBB7_85
# BB#80:                                # %for.body.lr.ph.i
	xorq	%r12, %r12
	jmp	.LBB7_81
.LBB7_83:                               # %for.cond.i333.for.body.i_crit_edge
                                        #   in Loop: Header=BB7_81 Depth=1
	movq	8(%r13), %rsi
.LBB7_81:                               # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rcx, %rbx
	andq	%rbx, %rax
	movq	8(%r8), %rdi
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	addq	%rax, %rdi
	addq	%rax, %rsi
	movq	%r8, %r15
	callq	_cpp_equiv_tokens
	movq	%r15, %r8
	testq	%rbx, %rax
	movq	24(%rsp), %r9           # 8-byte Reload
	je	.LBB7_89
# BB#82:                                # %for.cond.i333
                                        #   in Loop: Header=BB7_81 Depth=1
	movl	20(%r8), %eax
	incq	%r12
	movq	%r12, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	%rax, %rcx
	jb	.LBB7_83
# BB#84:                                # %for.cond.for.cond37.preheader_crit_edge.i
	movzbl	24(%r8), %eax
.LBB7_85:                               # %for.cond37.preheader.i
	testq	$65535, %rax            # imm = 0xFFFF
	je	.LBB7_91
# BB#86:                                # %for.body42.lr.ph.i
	movq	(%r8), %r8
	movq	(%r13), %rdx
	andq	$65535, %rax            # imm = 0xFFFF
	xorq	%rsi, %rsi
.LBB7_88:                               # %for.body42.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	movq	(%r8,%rdi,8), %rcx
	movq	(%rdx,%rdi,8), %rdi
	cmpq	%rdi, %rcx
	jne	.LBB7_89
# BB#87:                                # %for.cond37.i
                                        #   in Loop: Header=BB7_88 Depth=1
	incq	%rsi
	movq	%rsi, %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	cmpq	%rax, %rdi
	jb	.LBB7_88
	jmp	.LBB7_91
.LBB7_89:                               # %if.then187
	movq	8(%r9), %r8
	movl	68(%r14), %esi
	movabsq	$.L.str4, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r9, %r15
	callq	cpp_pedwarn_with_line
	movq	%r15, %r9
	movzbl	20(%r9), %eax
	andq	$1279, %rax             # imm = 0x4FF
	cmpq	$1, %rax
	jne	.LBB7_91
# BB#90:                                # %if.then199
	movq	24(%r9), %rax
	movl	16(%rax), %esi
	movabsq	$.L.str5, %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r9, %r15
	callq	cpp_pedwarn_with_line
	movq	%r15, %r9
.LBB7_91:                               # %if.end203
	xorq	%rax, %rax
	movb	%al, 20(%r9)
	movzbl	21(%r9), %r15d
	andq	$-37, %r15
	movb	%r15b, 21(%r9)
.LBB7_92:                               # %if.end204
	movq	$1, %r12
	movb	%r12b, 20(%r9)
	movq	%r13, 24(%r9)
	movq	8(%r9), %rdi
	movabsq	$.L.str6, %rsi
	movq	$7, %rdx
	movq	%r9, %rbx
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_94
# BB#93:                                # %if.then212
	orq	$16, %r15
	movb	%r15b, 21(%rbx)
.LBB7_94:                               # %cleanup1
	movq	16(%rsp), %r15          # 8-byte Reload
.LBB7_95:                               # %cleanup1
	movq	192(%r14), %rax
	movzbl	-18(%rax), %eax
	movb	%al, -18(%r15)
	movq	%r15, 192(%r14)
.LBB7_96:                               # %cleanup2
	xorq	%rax, %rax
	movb	%al, 13(%r14)
	movzbl	24(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB7_99
# BB#97:                                # %for.body.lr.ph
	movq	(%r13), %rdx
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB7_98:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	decq	%rcx
	movq	%rcx, %rdi
	andq	%rsi, %rdi
	movq	(%rdx,%rdi,8), %rdi
	movw	%ax, 16(%rdi)
	jne	.LBB7_98
.LBB7_99:                               # %for.end236
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_3:                                # %if.then
	leaq	72(%r14), %r15
	xorq	%rbx, %rbx
	jmp	.LBB7_4
.LBB7_23:                               # %save_parameter.exit.thread.i
                                        #   in Loop: Header=BB7_4 Depth=1
	leaq	1(%rcx), %rdx
	movw	%dx, 24(%r13)
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rbx, (%rax,%rcx,8)
	movw	%dx, 16(%rbx)
	movq	$1, %rbx
.LBB7_4:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %ecx
	cmpq	$66, %rcx
	jg	.LBB7_16
# BB#5:                                 # %for.cond.i
                                        #   in Loop: Header=BB7_4 Depth=1
	cmpq	$46, %rcx
	jg	.LBB7_11
# BB#6:                                 # %for.cond.i
                                        #   in Loop: Header=BB7_4 Depth=1
	cmpq	$21, %rcx
	jne	.LBB7_7
# BB#25:                                # %sw.bb11.i
                                        #   in Loop: Header=BB7_4 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	movabsq	$0, %rbx
	jne	.LBB7_4
	jmp	.LBB7_26
.LBB7_11:                               # %for.cond.i
                                        #   in Loop: Header=BB7_4 Depth=1
	cmpq	$56, %rcx
	jne	.LBB7_12
# BB#18:                                # %sw.bb.i
                                        #   in Loop: Header=BB7_4 Depth=1
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rbx
	jne	.LBB7_19
# BB#20:                                # %if.end.i
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	8(%rax), %rbx
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB7_24
# BB#21:                                # %if.end.i.i
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	(%r15), %rcx
	movq	16(%rcx), %rax
	movq	24(%rcx), %rdx
	movzbl	24(%r13), %ecx
	subq	%rax, %rdx
	leaq	8(,%rcx,8), %rsi
	cmpq	%rsi, %rdx
	jae	.LBB7_23
# BB#22:                                # %if.then4.i.i
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	$8, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	_cpp_extend_buff
	movq	72(%r14), %rax
	movzbl	24(%r13), %ecx
	movq	16(%rax), %rax
	jmp	.LBB7_23
.LBB7_16:                               # %for.cond.i
	cmpq	$67, %rcx
	je	.LBB7_37
	jmp	.LBB7_17
.LBB7_60:                               # %if.then83
	movabsq	$.L.str2, %rsi
	jmp	.LBB7_61
.LBB7_19:                               # %if.then.i
	movabsq	$.L.str10, %rsi
	jmp	.LBB7_38
.LBB7_24:                               # %save_parameter.exit.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str15, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error
	jmp	.LBB7_39
.LBB7_7:                                # %for.cond.i
	cmpq	$23, %rcx
	jne	.LBB7_17
# BB#8:                                 # %sw.bb6.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	jne	.LBB7_10
# BB#9:                                 # %lor.lhs.false.i
	movzbl	24(%r13), %eax
	testq	%rax, %rax
	je	.LBB7_10
.LBB7_26:                               # %if.then13.i
	movabsq	$.L.str11, %rsi
	jmp	.LBB7_38
.LBB7_12:                               # %for.cond.i
	cmpq	$47, %rcx
	jne	.LBB7_17
# BB#13:                                # %sw.bb15.i
	movzbl	26(%r13), %eax
	orq	$2, %rax
	movb	%al, 26(%r13)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rbx
	je	.LBB7_14
# BB#33:                                # %if.else.i
	movzbl	770(%r14), %eax
	testq	%rax, %rax
	je	.LBB7_36
# BB#34:                                # %if.then29.i
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	jmp	.LBB7_35
.LBB7_17:                               # %sw.default.i
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	cpp_token_as_text
	movq	%rax, %rcx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB7_39
.LBB7_14:                               # %if.then18.i
	movq	816(%r14), %rbx
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	je	.LBB7_27
# BB#15:                                # %if.then.i67.i
	movq	8(%rbx), %rcx
	movabsq	$.L.str15, %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rdx, %rsi
	movq	%rcx, %rdx
	callq	cpp_error
	jmp	.LBB7_30
.LBB7_27:                               # %if.end.i79.i
	movq	(%r15), %rcx
	movq	16(%rcx), %rax
	movq	24(%rcx), %rdx
	movzbl	24(%r13), %ecx
	subq	%rax, %rdx
	leaq	8(,%rcx,8), %rsi
	cmpq	%rsi, %rdx
	jae	.LBB7_29
# BB#28:                                # %if.then4.i84.i
	movq	$8, %rdx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	_cpp_extend_buff
	movq	72(%r14), %rax
	movzbl	24(%r13), %ecx
	movq	16(%rax), %rax
.LBB7_29:                               # %if.end6.i88.i
	leaq	1(%rcx), %rdx
	movw	%dx, 24(%r13)
	andq	$65535, %rcx            # imm = 0xFFFF
	movq	%rbx, (%rax,%rcx,8)
	movw	%dx, 16(%rbx)
.LBB7_30:                               # %save_parameter.exit90.i
	movq	$1, %rax
	movb	%al, 13(%r14)
	movzbl	769(%r14), %eax
	testq	%rax, %rax
	jne	.LBB7_36
# BB#31:                                # %land.lhs.true.i
	movzbl	770(%r14), %eax
	testq	%rax, %rax
	je	.LBB7_36
# BB#32:                                # %if.then24.i
	movabsq	$.L.str12, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rsi
.LBB7_35:                               # %if.end31.i
	callq	cpp_pedwarn
.LBB7_36:                               # %if.end31.i
	movq	%r14, %rdi
	callq	_cpp_lex_token
	movzbl	6(%rax), %eax
	cmpq	$23, %rax
	jne	.LBB7_37
.LBB7_10:                               # %if.end
	movq	(%r15), %rax
	movq	16(%rax), %rcx
	movq	%rcx, (%r13)
	movzbl	24(%r13), %edx
	leaq	(%rcx,%rdx,8), %rcx
	movq	%rcx, 16(%rax)
	movzbl	26(%r13), %eax
	orq	$1, %rax
	movb	%al, 26(%r13)
	jmp	.LBB7_42
.LBB7_37:                               # %sw.bb40.i
	movabsq	$.L.str14, %rsi
.LBB7_38:                               # %parse_params.exit.thread
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error
.LBB7_39:                               # %parse_params.exit.thread
	movq	(%r15), %rax
	movq	16(%rax), %rax
	movq	%rax, (%r13)
	xorq	%r12, %r12
	jmp	.LBB7_96
.Ltmp71:
	.size	_cpp_create_definition, .Ltmp71-_cpp_create_definition
	.cfi_endproc

	.align	16, 0x90
	.type	lex_expansion_token,@function
lex_expansion_token:                    # @lex_expansion_token
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 96
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%r14, 16(%rsp)          # 8-byte Spill
	movq	72(%r14), %rcx
	movq	%rsi, %rbx
	movq	16(%rcx), %rax
	movq	24(%rcx), %rsi
	movl	20(%rbx), %edx
	subq	%rax, %rsi
	leaq	1(%rdx), %rdi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdi
	movq	$3, %rcx
	shlq	%cl, %rdi
	leaq	(%rdi,%rdi,2), %rcx
	cmpq	%rcx, %rsi
	jae	.LBB8_2
# BB#1:                                 # %if.then.i47
	leaq	72(%r14), %rsi
	movq	$24, %rdx
	movq	%r14, %rdi
	callq	_cpp_extend_buff
	movq	72(%r14), %rax
	movl	20(%rbx), %edx
	movq	16(%rax), %rax
.LBB8_2:                                # %alloc_expansion_token.exit
	leaq	1(%rdx), %rcx
	movl	%ecx, 20(%rbx)
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	leaq	(%rdx,%rdx,2), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, 192(%r14)
	movq	%r14, %rdi
	callq	_cpp_lex_direct
	movzbl	6(%rax), %esi
	cmpq	$56, %rsi
	jne	.LBB8_5
# BB#3:                                 # %land.lhs.true
	movq	8(%rax), %rcx
	movzbl	16(%rcx), %edx
	testq	%rdx, %rdx
	je	.LBB8_5
# BB#4:                                 # %if.then
	movq	$65, %rdx
	movb	%dl, 6(%rax)
	movzbl	16(%rcx), %ecx
	movl	%ecx, 8(%rax)
	movq	%rax, (%rsp)            # 8-byte Spill
	jmp	.LBB8_26
.LBB8_5:                                # %if.else
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movzbl	761(%r14), %ecx
	testq	%rcx, %rcx
	je	.LBB8_26
# BB#6:                                 # %land.lhs.true10
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	24(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB8_26
# BB#7:                                 # %land.lhs.true14
	cmpq	$58, %rsi
	je	.LBB8_9
# BB#8:                                 # %land.lhs.true14
	cmpq	$61, %rsi
	jne	.LBB8_26
.LBB8_9:                                # %if.then25
	movq	(%rsp), %rax            # 8-byte Reload
	movl	8(%rax), %edx
	testq	%rdx, %rdx
	je	.LBB8_26
# BB#10:                                # %while.cond.preheader.lr.ph.i
	movq	(%rsp), %rax            # 8-byte Reload
	movq	16(%rax), %rbx
	addq	%rbx, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB8_11:                               # %while.cond.preheader.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_12 Depth 2
                                        #     Child Loop BB8_16 Depth 2
                                        #     Child Loop BB8_22 Depth 2
	cmpq	%rdx, %rbx
	movq	%rbx, %r13
	jae	.LBB8_15
	.align	16, 0x90
.LBB8_12:                               # %land.rhs.i
                                        #   Parent Loop BB8_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%r13), %eax
	movzbl	_sch_istable+1(%rax,%rax), %ecx
	testq	$2, %rcx
	jne	.LBB8_15
# BB#13:                                # %lor.rhs.i
                                        #   in Loop: Header=BB8_12 Depth=2
	cmpq	$36, %rax
	jne	.LBB8_27
# BB#14:                                # %land.rhs9.i
                                        #   in Loop: Header=BB8_12 Depth=2
	movzbl	767(%r14), %eax
	testq	%rax, %rax
	jne	.LBB8_15
.LBB8_27:                               # %while.body.i
                                        #   in Loop: Header=BB8_12 Depth=2
	incq	%r13
	cmpq	%rdx, %r13
	jb	.LBB8_12
	.align	16, 0x90
.LBB8_15:                               # %while.cond13.preheader.i
                                        #   in Loop: Header=BB8_11 Depth=1
	cmpq	%rdx, %r13
	movq	%r13, %rbx
	jae	.LBB8_20
	.align	16, 0x90
.LBB8_16:                               # %land.rhs16.i
                                        #   Parent Loop BB8_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movzbl	(%rbx), %eax
	movzbl	_sch_istable(%rax,%rax), %ecx
	testq	$516, %rcx              # imm = 0x204
	jne	.LBB8_19
# BB#17:                                # %lor.rhs24.i
                                        #   in Loop: Header=BB8_16 Depth=2
	cmpq	$36, %rax
	jne	.LBB8_20
# BB#18:                                # %land.rhs28.i
                                        #   in Loop: Header=BB8_16 Depth=2
	movzbl	767(%r14), %eax
	testq	%rax, %rax
	je	.LBB8_20
.LBB8_19:                               # %while.body36.i
                                        #   in Loop: Header=BB8_16 Depth=2
	incq	%rbx
	cmpq	%rdx, %rbx
	jb	.LBB8_16
.LBB8_20:                               # %while.end38.i
                                        #   in Loop: Header=BB8_11 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	24(%rax), %esi
	movq	%rsi, 40(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	je	.LBB8_25
# BB#21:                                # %for.body44.lr.ph.i
                                        #   in Loop: Header=BB8_11 Depth=1
	movq	%rbx, %rdx
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	subq	%r13, %rdx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	movq	$1, %r14
	.align	16, 0x90
.LBB8_22:                               # %for.body44.i
                                        #   Parent Loop BB8_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	-8(%rdi,%r14,8), %rax
	movl	(%rax), %ecx
	cmpq	%rdx, %rcx
	jne	.LBB8_28
# BB#23:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB8_22 Depth=2
	movq	8(%rax), %r15
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rdx, %r12
	callq	memcmp
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rdx
	movq	40(%rsp), %rsi          # 8-byte Reload
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_24
.LBB8_28:                               # %for.inc.i
                                        #   in Loop: Header=BB8_22 Depth=2
	movq	%r14, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r14
	cmpq	%rsi, %rax
	jb	.LBB8_22
	jmp	.LBB8_25
.LBB8_24:                               # %if.then.i
                                        #   in Loop: Header=BB8_11 Depth=1
	xorq	%rax, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str8, %rsi
	movq	%r15, %rdx
	callq	cpp_warning
	.align	16, 0x90
.LBB8_25:                               # %for.cond.backedge.i
                                        #   in Loop: Header=BB8_11 Depth=1
	movq	8(%rsp), %rdx           # 8-byte Reload
	cmpq	%rdx, %rbx
	movq	16(%rsp), %r14          # 8-byte Reload
	jb	.LBB8_11
.LBB8_26:                               # %if.end27
	movq	(%rsp), %rax            # 8-byte Reload
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	lex_expansion_token, .Ltmp89-lex_expansion_token
	.cfi_endproc

	.globl	cpp_macro_definition
	.align	16, 0x90
	.type	cpp_macro_definition,@function
cpp_macro_definition:                   # @cpp_macro_definition
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
	subq	$32, %rsp
.Ltmp101:
	.cfi_def_cfa_offset 80
.Ltmp102:
	.cfi_offset %rbx, -48
.Ltmp103:
	.cfi_offset %r12, -40
.Ltmp104:
	.cfi_offset %r13, -32
.Ltmp105:
	.cfi_offset %r14, -24
.Ltmp106:
	.cfi_offset %r15, -16
	movzbl	20(%rsi), %edx
	movq	%rdx, %rax
	andq	$1279, %rax             # imm = 0x4FF
	cmpq	$1, %rax
	jne	.LBB9_1
# BB#2:                                 # %if.end
	movq	24(%rsi), %r8
	movzbl	26(%r8), %eax
	movl	(%rsi), %r15d
	testq	$1, %rax
	je	.LBB9_3
# BB#8:                                 # %if.then9
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	movzbl	24(%r8), %eax
	addq	$6, %r15
	testq	%rax, %rax
	je	.LBB9_4
# BB#9:                                 # %for.body.lr.ph
	movq	(%r8), %rcx
	movq	$1, %rdx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_10:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%rcx,%rdx,8), %rdi
	movl	(%rdi), %edi
	leaq	1(%r15,%rdi), %r15
	movq	%rdx, %rdi
	andq	%rbx, %rdi
	incq	%rdx
	cmpq	%rax, %rdi
	jb	.LBB9_10
	jmp	.LBB9_4
.LBB9_1:                                # %if.then
	andq	$255, %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	cpp_ice
	jmp	.LBB9_42
.LBB9_3:
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	addq	$2, %r15
.LBB9_4:                                # %for.cond19.preheader
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movl	20(%r8), %r9d
	testq	%r9, %r9
	je	.LBB9_16
# BB#5:                                 # %for.body22.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB9_6:                                # %for.body22
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r8), %r13
	movq	%rbx, %rax
	andq	%rdx, %rax
	leaq	(%rax,%rax,2), %r12
	movzbl	6(%r13,%r12,8), %eax
	cmpq	$65, %rax
	jne	.LBB9_11
# BB#7:                                 # %if.then30
                                        #   in Loop: Header=BB9_6 Depth=1
	movl	8(%r13,%r12,8), %eax
	movq	(%r8), %rcx
	decq	%rax
	andq	%rdx, %rax
	movq	(%rcx,%rax,8), %rax
	movl	(%rax), %eax
	jmp	.LBB9_12
	.align	16, 0x90
.LBB9_11:                               # %if.else
                                        #   in Loop: Header=BB9_6 Depth=1
	leaq	(%r13,%r12,8), %rdi
	movq	%r8, %r14
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	%rdx, %r15
	callq	cpp_token_len
	movq	%r15, %rdx
	movq	24(%rsp), %r15          # 8-byte Reload
	movq	%r14, %r8
	movl	20(%r8), %r9d
.LBB9_12:                               # %if.end38
                                        #   in Loop: Header=BB9_6 Depth=1
	movzbl	7(%r13,%r12,8), %edi
	addq	%r15, %rax
	movq	$2, %rcx
	movq	%rdi, %rsi
	shrq	%cl, %rsi
	andq	$1, %rsi
	testq	$8, %rdi
	je	.LBB9_13
# BB#14:                                # %if.end38
                                        #   in Loop: Header=BB9_6 Depth=1
	leaq	3(%rsi,%rax), %r15
	jmp	.LBB9_15
	.align	16, 0x90
.LBB9_13:                               #   in Loop: Header=BB9_6 Depth=1
	addq	%rsi, %rax
	movq	%rax, %r15
.LBB9_15:                               # %if.end38
                                        #   in Loop: Header=BB9_6 Depth=1
	incq	%rbx
	movq	%rbx, %rax
	andq	%rdx, %rax
	movq	%r9, %rcx
	andq	%rdx, %rcx
	cmpq	%rcx, %rax
	jb	.LBB9_6
.LBB9_16:                               # %for.end55
	movq	%r8, %r14
	movq	16(%rsp), %rbx          # 8-byte Reload
	movl	272(%rbx), %eax
	movq	264(%rbx), %r12
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%r15, %rsi
	andq	%rcx, %rsi
	cmpq	%rax, %rsi
	jbe	.LBB9_18
# BB#17:                                # %if.then58
	movq	%r12, %rdi
	callq	xrealloc
	movq	%rax, %r12
	movq	%r12, 264(%rbx)
	movl	%r15d, 272(%rbx)
.LBB9_18:                               # %if.end63
	movq	8(%rsp), %rbx           # 8-byte Reload
	movq	8(%rbx), %rsi
	movl	(%rbx), %edx
	movq	%r12, %rdi
	callq	memcpy
	movq	%r14, %r8
	movzbl	26(%r8), %eax
	movl	(%rbx), %ecx
	testq	$1, %rax
	je	.LBB9_19
# BB#20:                                # %if.then76
	movq	$40, %rax
	movb	%al, (%r12,%rcx)
	movzbl	24(%r8), %eax
	leaq	1(%r12,%rcx), %r12
	testq	%rax, %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	je	.LBB9_28
# BB#21:                                # %for.body82.lr.ph
	xorq	%r14, %r14
	.align	16, 0x90
.LBB9_22:                               # %for.body82
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r8), %rcx
	movq	%r14, %rdx
	andq	%rsi, %rdx
	movq	(%rcx,%rdx,8), %rbx
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	816(%rcx), %rcx
	cmpq	%rcx, %rbx
	je	.LBB9_24
# BB#23:                                # %if.then88
                                        #   in Loop: Header=BB9_22 Depth=1
	movq	%rsi, %r13
	movq	8(%rbx), %rsi
	movl	(%rbx), %edx
	movq	%r12, %rdi
	movq	%r8, %r15
	callq	memcpy
	movq	%r13, %rsi
	movq	%r15, %r8
	movl	(%rbx), %ecx
	movzbl	24(%r8), %eax
	addq	%rcx, %r12
.LBB9_24:                               # %if.end98
                                        #   in Loop: Header=BB9_22 Depth=1
	incq	%r14
	movq	%r14, %rcx
	andq	%rsi, %rcx
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	%rax, %rcx
	jae	.LBB9_26
# BB#25:                                # %if.then104
                                        #   in Loop: Header=BB9_22 Depth=1
	movq	$44, %rax
	movb	%al, (%r12)
	incq	%r12
	jmp	.LBB9_27
	.align	16, 0x90
.LBB9_26:                               # %if.else106
                                        #   in Loop: Header=BB9_22 Depth=1
	movzbl	26(%r8), %eax
	testq	$2, %rax
	je	.LBB9_27
# BB#43:                                # %if.then111
                                        #   in Loop: Header=BB9_22 Depth=1
	movq	$11822, %rax            # imm = 0x2E2E
	movw	%ax, (%r12)
	movq	$46, %rax
	movb	%al, 2(%r12)
	addq	$3, %r12
.LBB9_27:                               # %for.cond77.backedge
                                        #   in Loop: Header=BB9_22 Depth=1
	movzbl	24(%r8), %eax
	cmpq	%rax, %rcx
	jb	.LBB9_22
.LBB9_28:                               # %for.end119
	movq	$41, %rax
	movb	%al, (%r12)
	incq	%r12
	jmp	.LBB9_29
.LBB9_19:
	addq	%rcx, %r12
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
.LBB9_29:                               # %if.end121
	movq	$32, %rax
	movb	%al, (%r12)
	movl	20(%r8), %eax
	incq	%r12
	testq	%rax, %rax
	je	.LBB9_41
# BB#30:                                # %for.body130.lr.ph
	xorq	%r14, %r14
	.align	16, 0x90
.LBB9_31:                               # %for.body130
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r8), %rax
	movq	%r14, %rcx
	andq	%rsi, %rcx
	leaq	(%rcx,%rcx,2), %rcx
	movzbl	7(%rax,%rcx,8), %edx
	leaq	7(%rax,%rcx,8), %rdi
	testq	$1, %rdx
	je	.LBB9_33
# BB#32:                                # %if.then139
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	$32, %rdx
	movb	%dl, (%r12)
	movzbl	(%rdi), %edx
	incq	%r12
.LBB9_33:                               # %if.end141
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%r8, %rbx
	testq	$4, %rdx
	je	.LBB9_35
# BB#34:                                # %if.then146
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	$35, %rdx
	movb	%dl, (%r12)
	incq	%r12
.LBB9_35:                               # %if.end148
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	%rsi, %r15
	movzbl	6(%rax,%rcx,8), %edx
	cmpq	$65, %rdx
	jne	.LBB9_37
# BB#36:                                # %if.then154
                                        #   in Loop: Header=BB9_31 Depth=1
	movl	8(%rax,%rcx,8), %eax
	movq	(%rbx), %rcx
	decq	%rax
	andq	%r15, %rax
	movq	(%rcx,%rax,8), %rax
	movq	8(%rax), %rsi
	movl	(%rax), %edx
	movq	%r12, %rdi
	movq	%rdx, %r13
	callq	memcpy
	addq	%r13, %r12
	jmp	.LBB9_38
	.align	16, 0x90
.LBB9_37:                               # %if.else174
                                        #   in Loop: Header=BB9_31 Depth=1
	leaq	(%rax,%rcx,8), %rsi
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rdx
	callq	cpp_spell_token
	movq	%rax, %r12
.LBB9_38:                               # %if.end176
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	(%rax), %eax
	testq	$8, %rax
	je	.LBB9_40
# BB#39:                                # %if.then181
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	$8992, %rax             # imm = 0x2320
	movw	%ax, (%r12)
	movq	$35, %rax
	movb	%al, 2(%r12)
	addq	$3, %r12
.LBB9_40:                               # %for.inc186
                                        #   in Loop: Header=BB9_31 Depth=1
	movq	%rbx, %r8
	movq	%r15, %rsi
	movl	20(%r8), %eax
	incq	%r14
	movq	%r14, %rcx
	andq	%rsi, %rcx
	cmpq	%rax, %rcx
	jb	.LBB9_31
.LBB9_41:                               # %if.end189
	xorq	%rax, %rax
	movb	%al, (%r12)
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	264(%rax), %rbx
.LBB9_42:                               # %return
	movq	%rbx, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	cpp_macro_definition, .Ltmp107-cpp_macro_definition
	.cfi_endproc

	.align	16, 0x90
	.type	expand_arg,@function
expand_arg:                             # @expand_arg
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp114:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp115:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp116:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp117:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 48
.Ltmp119:
	.cfi_offset %rbx, -48
.Ltmp120:
	.cfi_offset %r12, -40
.Ltmp121:
	.cfi_offset %r13, -32
.Ltmp122:
	.cfi_offset %r14, -24
.Ltmp123:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movl	24(%r12), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	je	.LBB10_13
# BB#1:                                 # %if.end
	movq	$2048, %rdi             # imm = 0x800
	callq	xmalloc
	movq	%rax, 8(%r12)
	movq	152(%r14), %rax
	movl	24(%r12), %ebx
	movq	(%rax), %rax
	movq	(%r12), %r13
	incq	%rbx
	testq	%rax, %rax
	jne	.LBB10_3
# BB#2:                                 # %if.then.i.i
	movq	$56, %rdi
	callq	xmalloc
	movq	152(%r14), %rcx
	movq	%rcx, 8(%rax)
	movq	$0, (%rax)
	movq	%rax, (%rcx)
.LBB10_3:                               # %push_ptoken_context.exit
	movq	%rax, 152(%r14)
	xorq	%rcx, %rcx
	movb	%cl, 48(%rax)
	movq	$0, 40(%rax)
	movq	$0, 32(%rax)
	movq	%r13, 16(%rax)
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rbx
	leaq	(%r13,%rbx,8), %rcx
	movq	%rcx, 24(%rax)
	movl	28(%r12), %ecx
	movq	$256, %rbx              # imm = 0x100
	jmp	.LBB10_4
	.align	16, 0x90
.LBB10_7:                               # %if.end17
                                        #   in Loop: Header=BB10_4 Depth=1
	movl	28(%r12), %edx
	leaq	1(%rdx), %rcx
	movl	%ecx, 28(%r12)
	movq	8(%r12), %rsi
	movq	%rax, (%rsi,%rdx,8)
.LBB10_4:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	incq	%rcx
	andq	%r15, %rcx
	movq	%rbx, %rax
	andq	%r15, %rax
	cmpq	%rax, %rcx
	jb	.LBB10_6
# BB#5:                                 # %if.then5
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	8(%r12), %rdi
	movq	%rbx, %rsi
	andq	$2147483647, %rsi       # imm = 0x7FFFFFFF
	leaq	(%rbx,%rbx), %rbx
	shlq	$4, %rsi
	callq	xrealloc
	movq	%rax, 8(%r12)
.LBB10_6:                               # %if.end12
                                        #   in Loop: Header=BB10_4 Depth=1
	movq	%r14, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %ecx
	cmpq	$67, %rcx
	jne	.LBB10_7
# BB#8:                                 # %for.end
	movq	152(%r14), %rbx
	movq	40(%rbx), %rax
	testq	%rax, %rax
	je	.LBB10_10
# BB#9:                                 # %if.then.i
	movzbl	21(%rax), %ecx
	andq	$223, %rcx
	movb	%cl, 21(%rax)
.LBB10_10:                              # %if.end.i
	movq	32(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB10_12
# BB#11:                                # %if.then5.i
	movq	%r14, %rdi
	callq	_cpp_release_buff
.LBB10_12:                              # %_cpp_pop_context.exit
	movq	8(%rbx), %rax
	movq	%rax, 152(%r14)
.LBB10_13:                              # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp124:
	.size	expand_arg, .Ltmp124-expand_arg
	.cfi_endproc

	.align	16, 0x90
	.type	collect_args,@function
collect_args:                           # @collect_args
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp131:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp132:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp133:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp134:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp135:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp136:
	.cfi_def_cfa_offset 112
.Ltmp137:
	.cfi_offset %rbx, -48
.Ltmp138:
	.cfi_offset %r12, -40
.Ltmp139:
	.cfi_offset %r13, -32
.Ltmp140:
	.cfi_offset %r14, -24
.Ltmp141:
	.cfi_offset %r15, -16
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	24(%rsi), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movzbl	24(%rax), %eax
	movq	%rdi, %r13
	testq	%rax, %rax
	movq	$1, %rbx
	je	.LBB11_2
# BB#1:                                 # %entry
	movq	%rax, %rbx
.LBB11_2:                               # %entry
	imulq	$432, %rbx, %rsi        # imm = 0x1B0
	movq	%r13, %rdi
	callq	_cpp_get_buff
	movq	%rax, %r15
	movq	%r15, 24(%rsp)          # 8-byte Spill
	movq	8(%r15), %r14
	movq	$5, %rcx
	shlq	%cl, %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	memset
	xorq	%rax, %rax
	addq	%r14, %rbx
	movq	%rbx, 16(%r15)
	leaq	368(%r13), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB11_3
	.align	16, 0x90
.LBB11_32:                              # %do.cond.do.body_crit_edge
                                        #   in Loop: Header=BB11_3 Depth=1
	movq	16(%r15), %rbx
	movq	48(%rsp), %rax          # 8-byte Reload
.LBB11_3:                               # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB11_4 Depth 2
                                        #     Child Loop BB11_21 Depth 2
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	1(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	%rbx, (%r14)
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	xorq	%rcx, %rcx
	jmp	.LBB11_4
	.align	16, 0x90
.LBB11_24:                              # %if.end81
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	(%r14), %rdx
	leaq	1(%rbx), %rcx
	andq	%r12, %rbx
	movq	%rax, (%rdx,%rbx,8)
.LBB11_4:                               # %for.cond
                                        #   Parent Loop BB11_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rcx, %rbx
	movq	(%r14), %rax
	movq	24(%r15), %rcx
	leaq	2(%rbx), %rdx
	andq	%r12, %rdx
	leaq	(%rax,%rdx,8), %rax
	cmpq	%rcx, %rax
	jbe	.LBB11_6
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	$8000, %rdx             # imm = 0x1F40
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	_cpp_append_extend_buff
	movq	%rax, %r15
	movq	16(%r15), %rax
	movq	%rax, (%r14)
.LBB11_6:                               # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	%r13, %rdi
	callq	cpp_get_token
	movzbl	6(%rax), %edx
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$65, %rsi
	jg	.LBB11_15
# BB#7:                                 # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	$56, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$56, %rcx
	sarq	%cl, %rsi
	cmpq	$39, %rsi
	jg	.LBB11_13
# BB#8:                                 # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	andq	$255, %rdx
	cmpq	$21, %rdx
	je	.LBB11_18
# BB#9:                                 # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	cmpq	$22, %rdx
	jne	.LBB11_10
# BB#23:                                # %if.then30
                                        #   in Loop: Header=BB11_4 Depth=2
	incq	56(%rsp)                # 8-byte Folded Spill
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_15:                              # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	andq	$255, %rdx
	cmpq	$67, %rdx
	je	.LBB11_21
# BB#16:                                # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	cmpq	$66, %rdx
	jne	.LBB11_24
# BB#17:                                # %if.then19
                                        #   in Loop: Header=BB11_4 Depth=2
	xorq	%rcx, %rcx
	testq	%r12, %rbx
	je	.LBB11_4
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_13:                              # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$40, %rcx
	jne	.LBB11_24
# BB#14:                                # %land.lhs.true73
                                        #   in Loop: Header=BB11_4 Depth=2
	testq	$16384, %rdx            # imm = 0x4000
	je	.LBB11_24
	jmp	.LBB11_21
.LBB11_18:                              # %if.then49
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	jne	.LBB11_24
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	40(%rsp), %rdx          # 8-byte Reload
	movzbl	26(%rdx), %ecx
	testq	$2, %rcx
	je	.LBB11_21
# BB#20:                                # %land.lhs.true55
                                        #   in Loop: Header=BB11_4 Depth=2
	movzbl	24(%rdx), %ecx
	movq	48(%rsp), %rdx          # 8-byte Reload
	andq	%r12, %rdx
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	cmpq	%rcx, %rdx
	je	.LBB11_24
	jmp	.LBB11_21
.LBB11_10:                              # %if.end15
                                        #   in Loop: Header=BB11_4 Depth=2
	cmpq	$23, %rdx
	jne	.LBB11_24
# BB#11:                                # %if.then38
                                        #   in Loop: Header=BB11_4 Depth=2
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r12, %rcx
	je	.LBB11_21
# BB#12:                                #   in Loop: Header=BB11_4 Depth=2
	decq	%rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	jmp	.LBB11_24
	.align	16, 0x90
.LBB11_21:                              # %while.cond
                                        #   Parent Loop BB11_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rcx
	testq	%r12, %rcx
	je	.LBB11_22
# BB#25:                                # %land.rhs
                                        #   in Loop: Header=BB11_21 Depth=2
	movq	(%r14), %rdx
	leaq	-1(%rcx), %rbx
	movq	%rbx, %rsi
	andq	%r12, %rsi
	movq	(%rdx,%rsi,8), %rsi
	movzbl	6(%rsi), %esi
	cmpq	$66, %rsi
	je	.LBB11_21
	jmp	.LBB11_26
	.align	16, 0x90
.LBB11_22:                              # %while.cond.while.end_crit_edge
                                        #   in Loop: Header=BB11_3 Depth=1
	movq	(%r14), %rdx
	xorq	%rcx, %rcx
.LBB11_26:                              # %while.end
                                        #   in Loop: Header=BB11_3 Depth=1
	movl	%ecx, 24(%r14)
	movq	%rcx, %rsi
	andq	%r12, %rsi
	movq	32(%rsp), %rdi          # 8-byte Reload
	movq	%rdi, (%rdx,%rsi,8)
	movq	40(%rsp), %rdx          # 8-byte Reload
	movzbl	24(%rdx), %r8d
	movq	48(%rsp), %rdx          # 8-byte Reload
	andq	%r12, %rdx
	cmpq	%r8, %rdx
	ja	.LBB11_29
# BB#27:                                # %if.then104
                                        #   in Loop: Header=BB11_3 Depth=1
	movq	(%r14), %rsi
	incq	%rcx
	andq	%r12, %rcx
	leaq	(%rsi,%rcx,8), %rcx
	movq	%rcx, 16(%r15)
	cmpq	%r8, %rdx
	je	.LBB11_29
# BB#28:                                # %if.then104
                                        #   in Loop: Header=BB11_3 Depth=1
	addq	$32, %r14
.LBB11_29:                              # %do.cond
                                        #   in Loop: Header=BB11_3 Depth=1
	movzbl	6(%rax), %eax
	cmpq	$23, %rax
	je	.LBB11_44
# BB#30:                                # %do.cond
                                        #   in Loop: Header=BB11_3 Depth=1
	cmpq	$40, %rax
	je	.LBB11_35
# BB#31:                                # %do.cond
                                        #   in Loop: Header=BB11_3 Depth=1
	cmpq	$67, %rax
	jne	.LBB11_32
# BB#33:                                # %if.else153
	movq	152(%r13), %rax
	cmpq	$0, 8(%rax)
	jne	.LBB11_40
# BB#34:                                # %lor.rhs
	movzbl	8(%r13), %eax
	testq	%rax, %rax
	jne	.LBB11_36
	jmp	.LBB11_43
.LBB11_44:                              # %if.else161
	cmpq	%r8, %rdx
	jae	.LBB11_53
# BB#45:                                # %if.then166
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	$2, %rax
	andq	%r12, %rax
	cmpq	%r8, %rax
	movq	40(%rsp), %rax          # 8-byte Reload
	jne	.LBB11_52
# BB#46:                                # %land.lhs.true172
	movzbl	26(%rax), %eax
	testq	$2, %rax
	je	.LBB11_52
# BB#47:                                # %if.then179
	movzbl	770(%r13), %ecx
	testq	%rcx, %rcx
	je	.LBB11_48
# BB#49:                                # %if.then179
	andq	$4, %rax
	testq	$255, %rax
	jne	.LBB11_50
# BB#51:                                # %if.then188
	movabsq	$.L.str36, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	cpp_pedwarn
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_61
.LBB11_35:                              # %if.then159
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	cpp_error
	movq	152(%r13), %rax
	cmpq	$0, 8(%rax)
	je	.LBB11_36
.LBB11_40:                              # %if.else.i
	movzbl	48(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB11_42
# BB#41:                                # %if.then17.i
	addq	$-24, 16(%rax)
	jmp	.LBB11_43
.LBB11_53:                              # %if.else196
	movq	16(%rsp), %rax          # 8-byte Reload
	jbe	.LBB11_54
# BB#55:                                # %if.then201
	testq	%r12, %rax
	jne	.LBB11_58
# BB#56:                                # %lor.lhs.false204
	movl	24(%r14), %eax
	testq	%rax, %rax
	je	.LBB11_57
.LBB11_58:                              # %if.then207
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rdx
	movabsq	$.L.str38, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	48(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB11_59
.LBB11_36:                              # %if.then.i
	movl	240(%r13), %eax
	incq	%rax
	movl	%eax, 240(%r13)
	movq	232(%r13), %rcx
	movq	192(%r13), %rax
	movq	16(%rcx), %rdx
	addq	$-24, %rax
	cmpq	%rdx, %rax
	jne	.LBB11_39
# BB#37:                                # %land.lhs.true.i
	movq	8(%rcx), %rcx
	testq	%rcx, %rcx
	je	.LBB11_39
# BB#38:                                # %if.then6.i
	movq	%rcx, 232(%r13)
	movq	24(%rcx), %rax
.LBB11_39:                              # %while.cond.backedge.i
	movq	%rax, 192(%r13)
	jmp	.LBB11_43
.LBB11_52:                              # %if.else190
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rdx
	movabsq	$.L.str37, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	%r8, %rcx
	movq	48(%rsp), %r8           # 8-byte Reload
.LBB11_59:                              # %if.end218
	callq	cpp_error
	jmp	.LBB11_60
.LBB11_54:
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_61
.LBB11_42:                              # %if.else20.i
	addq	$-8, 16(%rax)
.LBB11_43:                              # %if.end160
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	8(%rax), %rdx
	movabsq	$.L.str35, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	cpp_error
.LBB11_60:                              # %if.end218
	movq	%r13, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	_cpp_release_buff
	xorq	%rax, %rax
.LBB11_61:                              # %return
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB11_48:
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_61
.LBB11_50:
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_61
.LBB11_57:
	movq	24(%rsp), %rax          # 8-byte Reload
	jmp	.LBB11_61
.Ltmp142:
	.size	collect_args, .Ltmp142-collect_args
	.cfi_endproc

	.align	16, 0x90
	.type	paste_tokens,@function
paste_tokens:                           # @paste_tokens
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp146:
	.cfi_def_cfa_offset 16
.Ltmp147:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp148:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp149:
	.cfi_offset %rbx, -56
.Ltmp150:
	.cfi_offset %r12, -48
.Ltmp151:
	.cfi_offset %r13, -40
.Ltmp152:
	.cfi_offset %r14, -32
.Ltmp153:
	.cfi_offset %r15, -24
	movq	%rsi, %r14
	movq	(%r14), %r13
	movq	%rdx, %r15
	movq	%rdi, %r12
	movq	%r13, %rdi
	callq	cpp_token_len
	movq	%rax, %rbx
	movq	%r15, %rdi
	callq	cpp_token_len
	leaq	1(%rbx,%rax), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	addq	$15, %rcx
	andq	$-16, %rcx
	movq	%rsp, %rbx
	subq	%rcx, %rbx
	movq	%rbx, %rsp
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%rbx, %rdx
	callq	cpp_spell_token
	movzbl	6(%r13), %ecx
	cmpq	$7, %rcx
	jne	.LBB12_3
# BB#1:                                 # %land.lhs.true
	movzbl	6(%r15), %ecx
	andq	$254, %rcx
	cmpq	$6, %rcx
	jne	.LBB12_3
# BB#2:                                 # %if.then
	movq	$32, %rcx
	movb	%cl, (%rax)
	incq	%rax
.LBB12_3:                               # %if.end
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	cpp_spell_token
	xorq	%r15, %r15
	movb	%r15b, (%rax)
	subq	%rbx, %rax
	movq	$1, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	cpp_push_buffer
	movq	192(%r12), %rax
	movzbl	-20(%rax), %eax
	movq	(%r12), %rcx
	decq	%rax
	movl	%eax, 64(%rcx)
	movb	%r15b, 68(%rcx)
	movq	%r12, %rdi
	callq	_cpp_temp_token
	movq	%rax, 192(%r12)
	movq	%r12, %rdi
	callq	_cpp_lex_direct
	movq	%rax, (%r14)
	movq	(%r12), %rax
	movq	(%rax), %rcx
	movq	16(%rax), %rax
	cmpq	%rax, %rcx
	jne	.LBB12_4
# BB#5:                                 # %if.end
	movabsq	$1, %rbx
	jmp	.LBB12_6
.LBB12_4:
	movabsq	$0, %rbx
.LBB12_6:                               # %if.end
	movq	%r12, %rdi
	callq	_cpp_pop_buffer
	movq	%rbx, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp154:
	.size	paste_tokens, .Ltmp154-paste_tokens
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\\%03o"
	.size	.L.str, 6

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"ISO C requires whitespace after the macro name"
	.size	.L.str1, 47

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"'#' is not followed by a macro parameter"
	.size	.L.str2, 41

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"'##' cannot appear at either end of a macro expansion"
	.size	.L.str3, 54

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\"%s\" redefined"
	.size	.L.str4, 15

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"this is the location of the previous definition"
	.size	.L.str5, 48

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"__STDC_"
	.size	.L.str6, 8

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"invalid hash type %d in cpp_macro_definition"
	.size	.L.str7, 45

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"macro argument \"%s\" would be stringified with -traditional"
	.size	.L.str8, 59

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"\"%s\" may not appear in macro parameter list"
	.size	.L.str9, 44

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"macro parameters must be comma-separated"
	.size	.L.str10, 41

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"parameter name missing"
	.size	.L.str11, 23

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"anonymous variadic macros were introduced in C99"
	.size	.L.str12, 49

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"ISO C does not permit named variadic macros"
	.size	.L.str13, 44

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"missing ')' in macro parameter list"
	.size	.L.str14, 36

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"duplicate macro parameter \"%s\""
	.size	.L.str15, 31

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"function-like macro \"%s\" must be used with arguments in traditional C"
	.size	.L.str16, 70

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"invalid built-in macro \"%s\""
	.size	.L.str17, 28

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"%s %2d %4d"
	.size	.L.str18, 11

	.type	monthnames,@object      # @monthnames
	.section	.rodata,"a",@progbits
	.align	16
monthnames:
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.size	monthnames, 96

	.type	.L.str19,@object        # @.str19
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str19:
	.asciz	"%02d:%02d:%02d"
	.size	.L.str19, 15

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"Jan"
	.size	.L.str20, 4

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Feb"
	.size	.L.str21, 4

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Mar"
	.size	.L.str22, 4

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Apr"
	.size	.L.str23, 4

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"May"
	.size	.L.str24, 4

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Jun"
	.size	.L.str25, 4

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Jul"
	.size	.L.str26, 4

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Aug"
	.size	.L.str27, 4

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Sep"
	.size	.L.str28, 4

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Oct"
	.size	.L.str29, 4

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Nov"
	.size	.L.str30, 4

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"Dec"
	.size	.L.str31, 4

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"%u"
	.size	.L.str32, 3

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"invalid string literal, ignoring final '\\'"
	.size	.L.str33, 43

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"directives may not be used inside a macro argument"
	.size	.L.str34, 51

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"unterminated argument list invoking macro \"%s\""
	.size	.L.str35, 47

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"ISO C99 requires rest arguments to be used"
	.size	.L.str36, 43

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"macro \"%s\" requires %u arguments, but only %u given"
	.size	.L.str37, 52

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"macro \"%s\" passed %u arguments, but takes just %u"
	.size	.L.str38, 50

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"pasting \"%s\" and \"%s\" does not give a valid preprocessing token"
	.size	.L.str39, 64


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
