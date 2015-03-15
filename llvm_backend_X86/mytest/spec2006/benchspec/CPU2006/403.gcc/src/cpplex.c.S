	.file	"cpplex.c.bc"
	.text
	.globl	cpp_ideq
	.align	16, 0x90
	.type	cpp_ideq,@function
cpp_ideq:                               # @cpp_ideq
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp1:
	.cfi_def_cfa_offset 16
	movzbl	6(%rdi), %ecx
	xorq	%rax, %rax
	cmpq	$56, %rcx
	jne	.LBB0_4
# BB#1:                                 # %if.end
	movq	8(%rdi), %rax
	movq	8(%rax), %rdi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB0_2
# BB#3:                                 # %if.end
	movabsq	$1, %rax
.LBB0_4:                                # %return
	popq	%rdx
	retq
.LBB0_2:
	movabsq	$0, %rax
	popq	%rdx
	retq
.Ltmp2:
	.size	cpp_ideq, .Ltmp2-cpp_ideq
	.cfi_endproc

	.globl	_cpp_init_tokenrun
	.align	16, 0x90
	.type	_cpp_init_tokenrun,@function
_cpp_init_tokenrun:                     # @_cpp_init_tokenrun
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp8:
	.cfi_def_cfa_offset 32
.Ltmp9:
	.cfi_offset %rbx, -24
.Ltmp10:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	movq	$3, %rcx
	shlq	%cl, %rax
	leaq	(%rax,%rax,2), %r14
	movq	%r14, %rdi
	callq	xmalloc
	movq	%rax, 16(%rbx)
	addq	%r14, %rax
	movq	%rax, 24(%rbx)
	movq	$0, (%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp11:
	.size	_cpp_init_tokenrun, .Ltmp11-_cpp_init_tokenrun
	.cfi_endproc

	.globl	_cpp_temp_token
	.align	16, 0x90
	.type	_cpp_temp_token,@function
_cpp_temp_token:                        # @_cpp_temp_token
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp19:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp20:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp21:
	.cfi_def_cfa_offset 48
.Ltmp22:
	.cfi_offset %rbx, -40
.Ltmp23:
	.cfi_offset %r12, -32
.Ltmp24:
	.cfi_offset %r14, -24
.Ltmp25:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	232(%r15), %r12
	movq	192(%r15), %rbx
	movq	24(%r12), %rax
	cmpq	%rax, %rbx
	movq	%rbx, %rax
	jne	.LBB2_4
# BB#1:                                 # %if.then
	movq	(%r12), %rax
	testq	%rax, %rax
	jne	.LBB2_3
# BB#2:                                 # %if.then.i
	movq	$32, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, (%r12)
	movq	%r12, 8(%r14)
	movq	$6000, %rdi             # imm = 0x1770
	callq	xmalloc
	movq	%rax, 16(%r14)
	addq	$6000, %rax             # imm = 0x1770
	movq	%rax, 24(%r14)
	movq	$0, (%r14)
	movq	(%r12), %rax
.LBB2_3:                                # %next_tokenrun.exit
	movq	%rax, 232(%r15)
	movq	16(%rax), %rax
	movq	%rax, 192(%r15)
.LBB2_4:                                # %if.end
	leaq	24(%rax), %rcx
	movq	%rcx, 192(%r15)
	movl	-24(%rbx), %ecx
	movl	%ecx, (%rax)
	movzbl	-20(%rbx), %ecx
	movw	%cx, 4(%rax)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp26:
	.size	_cpp_temp_token, .Ltmp26-_cpp_temp_token
	.cfi_endproc

	.globl	_cpp_lex_token
	.align	16, 0x90
	.type	_cpp_lex_token,@function
_cpp_lex_token:                         # @_cpp_lex_token
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp33:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp34:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp35:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp36:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 48
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
	movq	%rdi, %rbx
	xorq	%r15, %r15
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB3_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	232(%rbx), %r13
	movq	192(%rbx), %r14
	movq	24(%r13), %rax
	cmpq	%rax, %r14
	jne	.LBB3_5
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	(%r13), %rax
	testq	%rax, %rax
	jne	.LBB3_4
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	$32, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, (%r13)
	movq	%r13, 8(%r14)
	movq	$6000, %rdi             # imm = 0x1770
	callq	xmalloc
	movq	%rax, 16(%r14)
	addq	$6000, %rax             # imm = 0x1770
	movq	%rax, 24(%r14)
	movq	$0, (%r14)
	movq	(%r13), %rax
.LBB3_4:                                # %next_tokenrun.exit
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%rax, 232(%rbx)
	movq	16(%rax), %r14
	movq	%r14, 192(%rbx)
.LBB3_5:                                # %if.end
                                        #   in Loop: Header=BB3_1 Depth=1
	movl	240(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB3_6
# BB#7:                                 # %if.else
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	%rbx, %rdi
	callq	_cpp_lex_direct
	movq	%rax, %r14
	jmp	.LBB3_8
	.align	16, 0x90
.LBB3_6:                                # %if.then5
                                        #   in Loop: Header=BB3_1 Depth=1
	decq	%rax
	movl	%eax, 240(%rbx)
	leaq	24(%r14), %rax
	movq	%rax, 192(%rbx)
.LBB3_8:                                # %if.end9
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	7(%r14), %esi
	testq	$64, %rsi
	je	.LBB3_15
# BB#9:                                 # %if.then11
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	6(%r14), %eax
	cmpq	$40, %rax
	jne	.LBB3_12
# BB#10:                                # %land.lhs.true
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	16(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB3_12
# BB#11:                                # %land.lhs.true15
                                        #   in Loop: Header=BB3_1 Depth=1
	andq	$1, %rsi
	movq	%rbx, %rdi
	callq	_cpp_handle_directive
	testq	%r12, %rax
	jne	.LBB3_1
	.align	16, 0x90
.LBB3_12:                               # %if.end22
                                        #   in Loop: Header=BB3_1 Depth=1
	movq	584(%rbx), %rax
	testq	%rax, %rax
	je	.LBB3_15
# BB#13:                                # %land.lhs.true24
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	9(%rbx), %ecx
	testq	%rcx, %rcx
	jne	.LBB3_15
# BB#14:                                # %if.then27
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	16(%rbx), %edx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	*%rax
	.align	16, 0x90
.LBB3_15:                               # %if.end34
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	8(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB3_18
# BB#16:                                # %if.end38
                                        #   in Loop: Header=BB3_1 Depth=1
	movb	%r15b, 184(%rbx)
	movzbl	9(%rbx), %eax
	testq	%rax, %rax
	je	.LBB3_18
# BB#17:                                # %lor.lhs.false
                                        #   in Loop: Header=BB3_1 Depth=1
	movzbl	6(%r14), %eax
	cmpq	$67, %rax
	jne	.LBB3_1
.LBB3_18:                               # %for.end
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp43:
	.size	_cpp_lex_token, .Ltmp43-_cpp_lex_token
	.cfi_endproc

	.globl	_cpp_lex_direct
	.align	16, 0x90
	.type	_cpp_lex_direct,@function
_cpp_lex_direct:                        # @_cpp_lex_direct
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp50:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp51:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp52:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp53:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp54:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp55:
	.cfi_def_cfa_offset 96
.Ltmp56:
	.cfi_offset %rbx, -48
.Ltmp57:
	.cfi_offset %r12, -40
.Ltmp58:
	.cfi_offset %r13, -32
.Ltmp59:
	.cfi_offset %r14, -24
.Ltmp60:
	.cfi_offset %r15, -16
	movq	%rdi, %r10
	movq	%r10, 32(%rsp)          # 8-byte Spill
	movq	192(%r10), %r15
	leaq	24(%r15), %rax
	movq	%rax, 192(%r10)
	leaq	200(%r10), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	jmp	.LBB4_1
.LBB4_40:                               # %if.then53
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rax, 232(%r10)
	movq	216(%r10), %r15
	leaq	24(%r15), %rax
	movq	%rax, 192(%r10)
	.align	16, 0x90
.LBB4_1:                                # %fresh_line.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_2 Depth 2
                                        #       Child Loop BB4_3 Depth 3
                                        #         Child Loop BB4_4 Depth 4
                                        #           Child Loop BB4_5 Depth 5
                                        #           Child Loop BB4_8 Depth 5
                                        #         Child Loop BB4_118 Depth 4
                                        #         Child Loop BB4_146 Depth 4
	movq	%r15, 40(%rsp)          # 8-byte Spill
.LBB4_2:                                # %fresh_line
                                        #   Parent Loop BB4_1 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_3 Depth 3
                                        #         Child Loop BB4_4 Depth 4
                                        #           Child Loop BB4_5 Depth 5
                                        #           Child Loop BB4_8 Depth 5
                                        #         Child Loop BB4_118 Depth 4
                                        #         Child Loop BB4_146 Depth 4
	movq	(%r10), %r12
	movzbl	68(%r12), %eax
	movb	%al, 7(%r15)
	movb	%dl, 68(%r12)
	jmp	.LBB4_3
.LBB4_42:                               # %if.then66
                                        #   in Loop: Header=BB4_3 Depth=3
	decq	(%r12)
	.align	16, 0x90
.LBB4_3:                                # %update_tokens_line
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB4_4 Depth 4
                                        #           Child Loop BB4_5 Depth 5
                                        #           Child Loop BB4_8 Depth 5
                                        #         Child Loop BB4_118 Depth 4
                                        #         Child Loop BB4_146 Depth 4
	movl	64(%r10), %eax
	movl	%eax, (%r15)
	jmp	.LBB4_4
.LBB4_22:                               # %skip_whitespace.exit
                                        #   in Loop: Header=BB4_4 Depth=4
	movq	%rax, (%rbx)
	movq	40(%rsp), %r15          # 8-byte Reload
	.align	16, 0x90
.LBB4_4:                                # %skipped_white
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        #       Parent Loop BB4_3 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB4_5 Depth 5
                                        #           Child Loop BB4_8 Depth 5
	movq	(%r12), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, (%r12)
	movq	24(%r12), %rsi
	movl	64(%r12), %edi
	movzbl	(%rax), %edx
	subq	%rsi, %rcx
	addq	%rcx, %rdi
	movw	%di, 4(%r15)
.LBB4_5:                                # %trigraph
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        #       Parent Loop BB4_3 Depth=3
                                        #         Parent Loop BB4_4 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movq	%rdx, %rax
	andq	%r13, %rax
	cmpq	$126, %rax
	ja	.LBB4_349
# BB#6:                                 # %trigraph
                                        #   in Loop: Header=BB4_5 Depth=5
	jmpq	*.LJTI4_0(,%rax,8)
.LBB4_41:                               # %sw.bb59
                                        #   in Loop: Header=BB4_5 Depth=5
	movl	64(%r10), %r14d
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
	movl	64(%r10), %ecx
	cmpq	%rcx, %r14
	jne	.LBB4_42
# BB#43:                                # %if.end69
                                        #   in Loop: Header=BB4_5 Depth=5
	movq	$92, %rdx
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	$92, %rcx
	je	.LBB4_349
# BB#44:                                # %if.end69
                                        #   in Loop: Header=BB4_5 Depth=5
	cmpq	$63, %rcx
	movq	%rax, %rdx
	jne	.LBB4_5
	jmp	.LBB4_45
.LBB4_7:                                # %sw.bb
                                        #   in Loop: Header=BB4_4 Depth=4
	movzbl	7(%r15), %eax
	orq	$1, %rax
	movb	%al, 7(%r15)
	movq	(%r10), %rbx
	xorq	%r15, %r15
	.align	16, 0x90
.LBB4_8:                                # %do.body.i
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        #       Parent Loop BB4_3 Depth=3
                                        #         Parent Loop BB4_4 Depth=4
                                        # =>        This Inner Loop Header: Depth=5
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rdx
	je	.LBB4_12
# BB#9:                                 # %do.body.i
                                        #   in Loop: Header=BB4_8 Depth=5
	andq	%r14, %rdx
	cmpq	$32, %rdx
	je	.LBB4_21
# BB#10:                                # %do.body.i
                                        #   in Loop: Header=BB4_8 Depth=5
	cmpq	$9, %rdx
	jne	.LBB4_15
# BB#11:                                # %if.then3.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movq	(%r10), %rcx
	movl	664(%r10), %esi
	movq	(%rcx), %rax
	movq	24(%rcx), %rdx
	movl	64(%rcx), %edi
	addq	%r14, %rax
	subq	%rdx, %rax
	addq	%rdi, %rax
	andq	%r14, %rax
	xorq	%rdx, %rdx
	divq	%rsi
	leaq	-1(%rsi,%rdi), %rax
	subq	%rdx, %rax
	movl	%eax, 64(%rcx)
	jmp	.LBB4_21
	.align	16, 0x90
.LBB4_12:                               # %if.then6.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movq	(%rbx), %rax
	movq	16(%rbx), %rcx
	decq	%rax
	cmpq	%rcx, %rax
	je	.LBB4_23
# BB#13:                                # %if.end.i
                                        #   in Loop: Header=BB4_8 Depth=5
	testq	%r14, %r15
	jne	.LBB4_21
# BB#14:                                # %if.then9.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movabsq	$.L.str110, %rsi
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r10, %r14
	callq	cpp_warning
	movq	%r14, %r10
	movq	$1, %r15
	jmp	.LBB4_21
.LBB4_15:                               # %if.else11.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movzbl	8(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_21
# BB#16:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movzbl	770(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_21
# BB#17:                                # %if.then15.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movq	(%rbx), %rax
	movq	24(%rbx), %rcx
	movl	64(%rbx), %r9d
	subq	%rcx, %rax
	addq	%rax, %r9
	cmpq	$12, %rdx
	je	.LBB4_18
# BB#19:                                # %if.then15.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movabsq	$.L.str113, %r8
	jmp	.LBB4_20
.LBB4_18:                               #   in Loop: Header=BB4_8 Depth=5
	movabsq	$.L.str112, %r8
.LBB4_20:                               # %if.then15.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movl	64(%r10), %esi
	movabsq	$.L.str111, %rcx
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r9, %rdx
	movq	%r10, %r14
	callq	cpp_pedwarn_with_line
	movq	%r14, %r10
	.align	16, 0x90
.LBB4_21:                               # %if.end24.i
                                        #   in Loop: Header=BB4_8 Depth=5
	movq	(%rbx), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, (%rbx)
	movzbl	(%rax), %edx
	movzbl	_sch_istable+1(%rdx,%rdx), %ecx
	testq	$8, %rcx
	jne	.LBB4_8
	jmp	.LBB4_22
.LBB4_109:                              # %sw.bb119
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	(%r12), %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_111
# BB#110:                               # %sw.bb119
                                        #   in Loop: Header=BB4_3 Depth=3
	cmpq	$92, %rax
	jne	.LBB4_112
.LBB4_111:                              # %if.then.i1008
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_112:                              # %get_effective_char.exit1011
                                        #   in Loop: Header=BB4_3 Depth=3
	andq	%r13, %rax
	cmpq	$61, %rax
	je	.LBB4_158
# BB#113:                               # %get_effective_char.exit1011
                                        #   in Loop: Header=BB4_3 Depth=3
	cmpq	$47, %rax
	jne	.LBB4_114
# BB#138:                               # %land.lhs.true132
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	743(%r10), %eax
	testq	%rax, %rax
	jne	.LBB4_141
# BB#139:                               # %lor.lhs.false135
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	56(%r10), %rax
	testq	%rax, %rax
	je	.LBB4_159
# BB#140:                               # %land.lhs.true137
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	21(%rax), %eax
	testq	%rax, %rax
	je	.LBB4_159
.LBB4_141:                              # %if.then141
                                        #   in Loop: Header=BB4_3 Depth=3
	movl	736(%r10), %eax
	testq	%rax, %rax
	jne	.LBB4_145
# BB#142:                               # %land.lhs.true145
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	770(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_145
# BB#143:                               # %land.lhs.true149
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	80(%r12), %eax
	testq	%rax, %rax
	jne	.LBB4_145
# BB#144:                               # %if.then151
                                        #   in Loop: Header=BB4_3 Depth=3
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	cpp_pedwarn
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	cpp_pedwarn
	movq	%rbx, %r10
	movq	$1, %rax
	movb	%al, 80(%r12)
.LBB4_145:                              # %if.end153
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	(%r10), %rbx
	movl	64(%r10), %eax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$1, %rax
	movb	%al, 12(%r10)
.LBB4_146:                              # %do.body.i1060
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        #       Parent Loop BB4_3 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	(%rbx), %rax
	movq	16(%rbx), %rcx
	cmpq	%rcx, %rax
	je	.LBB4_152
# BB#147:                               # %if.end.i1063
                                        #   in Loop: Header=BB4_146 Depth=4
	leaq	1(%rax), %rcx
	movq	%rcx, (%rbx)
	movzbl	(%rax), %eax
	cmpq	$63, %rax
	je	.LBB4_149
# BB#148:                               # %if.end.i1063
                                        #   in Loop: Header=BB4_146 Depth=4
	cmpq	$92, %rax
	jne	.LBB4_150
.LBB4_149:                              # %if.then7.i
                                        #   in Loop: Header=BB4_146 Depth=4
	movq	%r10, %rdi
	movq	%r10, %r14
	callq	skip_escaped_newlines
	movq	%r14, %r10
.LBB4_150:                              # %do.cond.i
                                        #   in Loop: Header=BB4_146 Depth=4
	andq	$255, %rax
	movzbl	_sch_istable+1(%rax,%rax), %eax
	testq	$4, %rax
	je	.LBB4_146
# BB#151:                               # %do.end.i1068
                                        #   in Loop: Header=BB4_3 Depth=3
	decq	(%rbx)
.LBB4_152:                              # %skip_line_comment.exit
                                        #   in Loop: Header=BB4_3 Depth=3
	xorq	%rax, %rax
	movb	%al, 12(%r10)
	movl	64(%r10), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	16(%rsp), %rdx          # 8-byte Reload
	andq	%rcx, %rdx
	cmpq	%rax, %rdx
	je	.LBB4_156
# BB#153:                               # %land.lhs.true156
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	758(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_156
# BB#154:                               # %if.then160
                                        #   in Loop: Header=BB4_3 Depth=3
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	cpp_warning
	jmp	.LBB4_155
.LBB4_114:                              # %get_effective_char.exit1011
                                        #   in Loop: Header=BB4_3 Depth=3
	cmpq	$42, %rax
	jne	.LBB4_159
# BB#115:                               # %if.then124
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	(%r10), %r14
	movq	$1, %rax
	movb	%al, 12(%r10)
	movq	(%r14), %rdx
	movq	16(%r14), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB4_117
# BB#116:                               # %skip_block_comment.exit.thread
                                        #   in Loop: Header=BB4_3 Depth=3
	xorq	%rax, %rax
	movb	%al, 12(%r10)
	jmp	.LBB4_135
.LBB4_117:                              # %while.body.lr.ph.i
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	%r13, %rax
.LBB4_118:                              # %while.body.i
                                        #   Parent Loop BB4_1 Depth=1
                                        #     Parent Loop BB4_2 Depth=2
                                        #       Parent Loop BB4_3 Depth=3
                                        # =>      This Inner Loop Header: Depth=4
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	1(%rdx), %rcx
	movq	%rcx, (%r14)
	movzbl	(%rdx), %eax
	cmpq	$63, %rax
	je	.LBB4_120
# BB#119:                               # %while.body.i
                                        #   in Loop: Header=BB4_118 Depth=4
	cmpq	$92, %rax
	jne	.LBB4_121
.LBB4_120:                              # %if.then.i1021
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_121:                              # %if.end.i1023
                                        #   in Loop: Header=BB4_118 Depth=4
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	$47, %rdx
	jne	.LBB4_128
# BB#122:                               # %if.then9.i1024
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	16(%rsp), %rax          # 8-byte Reload
	andq	%rbx, %rax
	cmpq	$42, %rax
	je	.LBB4_123
# BB#124:                               # %if.end13.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movzbl	758(%r10), %eax
	testq	%rax, %rax
	movq	$47, %rax
	je	.LBB4_132
# BB#125:                               # %land.lhs.true.i1026
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	(%r14), %rdi
	movzbl	(%rdi), %ecx
	cmpq	$42, %rcx
	jne	.LBB4_132
# BB#126:                               # %land.lhs.true19.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movzbl	1(%rdi), %ecx
	cmpq	$47, %rcx
	je	.LBB4_132
# BB#127:                               # %if.then25.i1031
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	24(%r14), %rcx
	movl	64(%r14), %edx
	movl	64(%r10), %esi
	subq	%rcx, %rdi
	addq	%rdi, %rdx
	movabsq	$.L.str98, %rcx
	xorq	%rax, %rax
	movq	%r10, %rdi
	callq	cpp_warning_with_line
	movq	$47, %rax
	movq	32(%rsp), %r10          # 8-byte Reload
	jmp	.LBB4_132
.LBB4_128:                              # %if.else.i1032
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	_sch_istable+1(%rcx,%rcx), %ecx
	testq	$4, %rcx
	jne	.LBB4_129
# BB#136:                               # %if.else35.i
                                        #   in Loop: Header=BB4_118 Depth=4
	cmpq	$9, %rdx
	jne	.LBB4_132
# BB#137:                               # %if.then38.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	(%r10), %rcx
	movl	664(%r10), %esi
	movq	(%rcx), %rax
	movq	24(%rcx), %rdx
	movl	64(%rcx), %edi
	addq	%rbx, %rax
	subq	%rdx, %rax
	addq	%rdi, %rax
	andq	%rbx, %rax
	xorq	%rdx, %rdx
	divq	%rsi
	leaq	-1(%rsi,%rdi), %rax
	subq	%rdx, %rax
	movl	%eax, 64(%rcx)
	movq	$9, %rax
	jmp	.LBB4_132
.LBB4_129:                              # %if.then34.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	(%r10), %rdi
	movq	(%rdi), %rcx
	movzbl	-1(%rcx), %edx
	movzbl	(%rcx), %esi
	addq	%rdx, %rsi
	andq	%rbx, %rsi
	cmpq	$23, %rsi
	jne	.LBB4_131
# BB#130:                               # %if.then.i.i1038
                                        #   in Loop: Header=BB4_118 Depth=4
	incq	%rcx
	movq	%rcx, (%rdi)
.LBB4_131:                              # %handle_newline.exit.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	%rcx, 24(%rdi)
	xorq	%rcx, %rcx
	movl	%ecx, 64(%rdi)
	movl	64(%r10), %ecx
	incq	%rcx
	movl	%ecx, 64(%r10)
.LBB4_132:                              # %while.cond.backedge.i
                                        #   in Loop: Header=BB4_118 Depth=4
	movq	(%r14), %rdx
	movq	16(%r14), %rcx
	cmpq	%rcx, %rdx
	jne	.LBB4_118
	jmp	.LBB4_133
.LBB4_123:                              #   in Loop: Header=BB4_3 Depth=3
	movq	$47, %rax
	movq	$42, 16(%rsp)           # 8-byte Folded Spill
.LBB4_133:                              # %skip_block_comment.exit
                                        #   in Loop: Header=BB4_3 Depth=3
	xorq	%rcx, %rcx
	movb	%cl, 12(%r10)
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%rbx, %rcx
	cmpq	$42, %rcx
	jne	.LBB4_135
# BB#134:                               # %skip_block_comment.exit
                                        #   in Loop: Header=BB4_3 Depth=3
	andq	%rbx, %rax
	cmpq	$47, %rax
	je	.LBB4_156
.LBB4_135:                              # %if.then127
                                        #   in Loop: Header=BB4_3 Depth=3
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	cpp_error
.LBB4_155:                              # %if.end171
                                        #   in Loop: Header=BB4_3 Depth=3
	movq	%rbx, %r10
.LBB4_156:                              # %if.end171
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	11(%r10), %eax
	testq	%rax, %rax
	jne	.LBB4_160
# BB#157:                               # %if.then174
                                        #   in Loop: Header=BB4_3 Depth=3
	movzbl	7(%r15), %eax
	orq	$1, %rax
	movb	%al, 7(%r15)
	jmp	.LBB4_3
.LBB4_34:                               # %sw.bb35
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movzbl	-1(%rcx), %edx
	movzbl	(%rcx), %esi
	addq	%rdx, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	cmpq	$23, %rdx
	jne	.LBB4_36
# BB#35:                                # %if.then.i961
                                        #   in Loop: Header=BB4_2 Depth=2
	incq	%rcx
	movq	%rcx, (%rax)
.LBB4_36:                               # %handle_newline.exit967
                                        #   in Loop: Header=BB4_2 Depth=2
	xorq	%rdx, %rdx
	movq	%rcx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 64(%rax)
	movl	64(%r10), %eax
	incq	%rax
	movl	%eax, 64(%r10)
	movq	$64, %rax
	movb	%al, 68(%r12)
	movzbl	8(%r10), %eax
	testq	%rax, %rax
	jne	.LBB4_33
# BB#37:                                # %if.then40
                                        #   in Loop: Header=BB4_2 Depth=2
	movzbl	16(%r10), %eax
	cmpq	$2, %rax
	jne	.LBB4_39
# BB#38:                                # %if.then46
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	$65, %rax
	movb	%al, 68(%r12)
.LBB4_39:                               # %if.end51
                                        #   in Loop: Header=BB4_2 Depth=2
	movl	244(%r10), %eax
	testq	%rax, %rax
	jne	.LBB4_2
	jmp	.LBB4_40
.LBB4_23:                               # %if.end
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	(%r12), %rax
	decq	%rax
	movq	%rax, (%r12)
	movq	$64, %rcx
	movb	%cl, 68(%r12)
	movzbl	16(%r10), %ecx
	testq	%rcx, %rcx
	movq	40(%rsp), %r15          # 8-byte Reload
	jne	.LBB4_33
# BB#24:                                # %land.lhs.true
                                        #   in Loop: Header=BB4_2 Depth=2
	movzbl	8(%r10), %ecx
	testq	%rcx, %rcx
	jne	.LBB4_33
# BB#25:                                # %if.then17
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	24(%r12), %rcx
	cmpq	%rcx, %rax
	je	.LBB4_31
# BB#26:                                # %if.then21
                                        #   in Loop: Header=BB4_2 Depth=2
	movzbl	81(%r12), %eax
	testq	%rax, %rax
	jne	.LBB4_28
# BB#27:                                # %if.then23
                                        #   in Loop: Header=BB4_2 Depth=2
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	cpp_pedwarn
	movq	%rbx, %r10
.LBB4_28:                               # %if.end24
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movzbl	-1(%rcx), %edx
	movzbl	(%rcx), %esi
	addq	%rdx, %rsi
	andq	%r14, %rsi
	cmpq	$23, %rsi
	jne	.LBB4_30
# BB#29:                                # %if.then.i
                                        #   in Loop: Header=BB4_2 Depth=2
	incq	%rcx
	movq	%rcx, (%rax)
.LBB4_30:                               # %handle_newline.exit
                                        #   in Loop: Header=BB4_2 Depth=2
	movq	%rcx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 64(%rax)
	movl	64(%r10), %eax
	incq	%rax
	movl	%eax, 64(%r10)
.LBB4_31:                               # %if.end25
                                        #   in Loop: Header=BB4_2 Depth=2
	cmpq	$0, 32(%r12)
	je	.LBB4_33
# BB#32:                                # %if.then27
                                        #   in Loop: Header=BB4_2 Depth=2
	movzbl	83(%r12), %r14d
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	_cpp_pop_buffer
	xorq	%rdx, %rdx
	movq	%rbx, %r10
	testq	%r14, %r14
	je	.LBB4_2
.LBB4_33:                               # %if.end34
	movq	$67, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_46:                               # %sw.bb79
	movq	$57, %rax
	movb	%al, 6(%r15)
	leaq	8(%r15), %rsi
	xorq	%rcx, %rcx
	movq	%r10, %rdi
	callq	parse_number
	jmp	.LBB4_350
.LBB4_104:                              # %sw.bb113
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdx, %rax
	cmpq	$34, %rax
	jne	.LBB4_105
# BB#106:                               # %sw.bb113
	movabsq	$1, %rax
	jmp	.LBB4_107
.LBB4_323:                              # %do.body595
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_325
# BB#324:                               # %do.body595
	cmpq	$92, %rax
	jne	.LBB4_326
.LBB4_325:                              # %if.then.i1283
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_326:                              # %get_effective_char.exit1286
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$61, %rcx
	jne	.LBB4_328
# BB#327:                               # %if.then599
	movq	$25, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_335:                              # %do.body625
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_337
# BB#336:                               # %do.body625
	cmpq	$92, %rax
	jne	.LBB4_338
.LBB4_337:                              # %if.then.i972
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_338:                              # %get_effective_char.exit
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$35, %rcx
	je	.LBB4_238
# BB#339:                               # %do.body632
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$40, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_348:                              # %sw.bb659
	movzbl	767(%r10), %eax
	testq	%rax, %rax
	movq	$36, %rdx
	jne	.LBB4_57
.LBB4_349:                              # %sw.default
	movq	$60, %rax
	movb	%al, 6(%r15)
	movb	%dl, 8(%r15)
	jmp	.LBB4_350
.LBB4_223:                              # %sw.bb325
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_225
# BB#224:                               # %sw.bb325
	cmpq	$92, %rax
	jne	.LBB4_226
.LBB4_225:                              # %if.then.i1153
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_226:                              # %get_effective_char.exit1156
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_228
# BB#227:                               # %if.then329
	movq	$32, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_284:                              # %sw.bb493
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_286
# BB#285:                               # %sw.bb493
	cmpq	$92, %rax
	jne	.LBB4_287
.LBB4_286:                              # %if.then.i1233
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_287:                              # %get_effective_char.exit1236
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$61, %rcx
	jne	.LBB4_288
# BB#290:                               # %if.then502
	movq	$33, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_342:                              # %sw.bb643
	movq	$22, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_343:                              # %sw.bb645
	movq	$23, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_311:                              # %do.body565
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_313
# BB#312:                               # %do.body565
	cmpq	$92, %rax
	jne	.LBB4_314
.LBB4_313:                              # %if.then.i1263
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_314:                              # %get_effective_char.exit1266
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$61, %rcx
	jne	.LBB4_316
# BB#315:                               # %if.then569
	movq	$30, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_260:                              # %sw.bb433
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_262
# BB#261:                               # %sw.bb433
	cmpq	$92, %rax
	jne	.LBB4_263
.LBB4_262:                              # %if.then.i1203
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_263:                              # %get_effective_char.exit1206
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	$61, %rcx
	jne	.LBB4_264
# BB#266:                               # %if.then442
	movq	$28, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_341:                              # %sw.bb641
	movq	$21, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_268:                              # %sw.bb452
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_270
# BB#269:                               # %sw.bb452
	cmpq	$92, %rax
	jne	.LBB4_271
.LBB4_270:                              # %if.then.i1213
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_271:                              # %get_effective_char.exit1216
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$45, %rax
	je	.LBB4_281
# BB#272:                               # %get_effective_char.exit1216
	cmpq	$61, %rax
	jne	.LBB4_273
# BB#282:                               # %if.then482
	movq	$29, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_245:                              # %sw.bb389
	movq	$51, %rax
	movb	%al, 6(%r15)
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_247
# BB#246:                               # %sw.bb389
	cmpq	$92, %rax
	jne	.LBB4_248
.LBB4_247:                              # %if.then.i1183
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_248:                              # %get_effective_char.exit1186
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r14, %rcx
	cmpq	$46, %rcx
	jne	.LBB4_255
# BB#249:                               # %if.then394
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	(%r12), %rbx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_251
# BB#250:                               # %if.then394
	cmpq	$92, %rax
	jne	.LBB4_252
.LBB4_251:                              # %if.then.i1193
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_252:                              # %get_effective_char.exit1196
	andq	%r14, %rax
	cmpq	$46, %rax
	jne	.LBB4_254
# BB#253:                               # %if.then400
	movq	$47, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_300:                              # %sw.bb531
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_302
# BB#301:                               # %sw.bb531
	cmpq	$92, %rax
	jne	.LBB4_303
.LBB4_302:                              # %if.then.i1253
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_303:                              # %get_effective_char.exit1256
	andq	%r13, %rax
	cmpq	$62, %rax
	jne	.LBB4_304
# BB#307:                               # %land.lhs.true545
	movzbl	747(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_310
# BB#308:                               # %if.then550
	movzbl	7(%r15), %eax
	orq	$2, %rax
	movb	%al, 7(%r15)
.LBB4_309:                              # %sw.bb649
	movq	$43, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_346:                              # %sw.bb655
	movq	$46, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_172:                              # %sw.bb180
	movzbl	10(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_174
# BB#173:                               # %if.then183
	movq	$63, %rax
	movb	%al, 6(%r15)
	movq	$62, %rdx
	jmp	.LBB4_108
.LBB4_317:                              # %do.body580
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_319
# BB#318:                               # %do.body580
	cmpq	$92, %rax
	jne	.LBB4_320
.LBB4_319:                              # %if.then.i1273
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_320:                              # %get_effective_char.exit1276
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_322
# BB#321:                               # %if.then584
	movq	$24, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_202:                              # %sw.bb269
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_204
# BB#203:                               # %sw.bb269
	cmpq	$92, %rax
	jne	.LBB4_205
.LBB4_204:                              # %if.then.i1123
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_205:                              # %get_effective_char.exit1126
	andq	%r13, %rax
	cmpq	$63, %rax
	je	.LBB4_215
# BB#206:                               # %get_effective_char.exit1126
	cmpq	$62, %rax
	jne	.LBB4_207
# BB#209:                               # %do.body279
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_211
# BB#210:                               # %do.body279
	cmpq	$92, %rax
	jne	.LBB4_212
.LBB4_211:                              # %if.then.i1133
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_212:                              # %get_effective_char.exit1136
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_214
# BB#213:                               # %if.then283
	movq	$36, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_347:                              # %sw.bb657
	movq	$55, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_47:                               # %sw.bb81
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	(%r12), %r14
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_49
# BB#48:                                # %sw.bb81
	cmpq	$92, %rax
	jne	.LBB4_50
.LBB4_49:                               # %if.then.i980
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_50:                               # %get_effective_char.exit983
	andq	%rax, %r13
	cmpq	$34, %r13
	je	.LBB4_52
# BB#51:                                # %get_effective_char.exit983
	cmpq	$39, %r13
	jne	.LBB4_56
.LBB4_52:                               # %if.then88
	cmpq	$34, %r13
	jne	.LBB4_53
# BB#54:                                # %if.then88
	movabsq	$1, %rcx
	jmp	.LBB4_55
.LBB4_344:                              # %sw.bb647
	movq	$42, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_329:                              # %do.body610
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_331
# BB#330:                               # %do.body610
	cmpq	$92, %rax
	jne	.LBB4_332
.LBB4_331:                              # %if.then.i1293
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_332:                              # %get_effective_char.exit1296
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_334
# BB#333:                               # %if.then614
	movq	$35, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_345:                              # %sw.bb651
	movq	$44, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_292:                              # %sw.bb512
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_294
# BB#293:                               # %sw.bb512
	cmpq	$92, %rax
	jne	.LBB4_295
.LBB4_294:                              # %if.then.i1243
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_295:                              # %get_effective_char.exit1246
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_296
# BB#298:                               # %if.then521
	movq	$34, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_340:                              # %sw.bb639
	movq	$16, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_105:
	movabsq	$0, %rax
.LBB4_107:                              # %sw.bb113
	leaq	58(%rax,%rax,2), %rax
	movb	%al, 6(%r15)
.LBB4_108:                              # %sw.epilog
	movq	%r10, %rdi
	movq	%r15, %rsi
	callq	parse_string
	jmp	.LBB4_350
.LBB4_45:                               # %if.then72
	movq	$19, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_328:                              # %do.body602
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$1, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_228:                              # %if.else331
	movzbl	747(%r10), %ecx
	testq	%rcx, %rcx
	je	.LBB4_240
# BB#229:                               # %if.else331
	cmpq	$58, %rax
	jne	.LBB4_240
# BB#230:                               # %if.then339
	movzbl	7(%r15), %eax
	orq	$2, %rax
	movb	%al, 7(%r15)
	movq	$40, %rax
	movb	%al, 6(%r15)
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_232
# BB#231:                               # %if.then339
	cmpq	$92, %rax
	jne	.LBB4_233
.LBB4_232:                              # %if.then.i1163
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_233:                              # %get_effective_char.exit1166
	andq	%r13, %rax
	cmpq	$37, %rax
	jne	.LBB4_280
# BB#234:                               # %if.then348
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	(%r12), %r14
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_236
# BB#235:                               # %if.then348
	cmpq	$92, %rax
	jne	.LBB4_237
.LBB4_236:                              # %if.then.i1173
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_237:                              # %get_effective_char.exit1176
	andq	%r13, %rax
	cmpq	$58, %rax
	jne	.LBB4_239
.LBB4_238:                              # %if.then354
	movq	$41, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_288:                              # %get_effective_char.exit1236
	cmpq	$38, %rcx
	jne	.LBB4_291
# BB#289:                               # %if.then497
	movq	$17, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_316:                              # %do.body572
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$6, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_264:                              # %get_effective_char.exit1206
	cmpq	$43, %rcx
	jne	.LBB4_267
# BB#265:                               # %if.then437
	movq	$48, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_255:                              # %if.else406
	movq	%rax, %rdx
	andq	$255, %rdx
	movzbl	_sch_istable(%rdx,%rdx), %edx
	testq	$4, %rdx
	je	.LBB4_257
# BB#256:                               # %if.then411
	movq	$57, %rcx
	movb	%cl, 6(%r15)
	leaq	8(%r15), %rsi
	movq	$1, %rcx
	movq	%r10, %rdi
	movq	%rax, %rdx
	callq	parse_number
	jmp	.LBB4_350
.LBB4_304:                              # %get_effective_char.exit1256
	cmpq	$58, %rax
	jne	.LBB4_310
# BB#305:                               # %land.lhs.true535
	movzbl	742(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_310
# BB#306:                               # %if.then540
	movq	$52, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_310:                              # %do.body557
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$20, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_322:                              # %do.body587
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	xorq	%rax, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_334:                              # %do.body617
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$11, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_296:                              # %get_effective_char.exit1246
	cmpq	$124, %rax
	jne	.LBB4_299
# BB#297:                               # %if.then516
	movq	$18, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_174:                              # %if.end185
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_176
# BB#175:                               # %if.end185
	cmpq	$92, %rax
	jne	.LBB4_177
.LBB4_176:                              # %if.then.i1093
	movq	%r10, %rdi
	movq	%r10, %rbx
	callq	skip_escaped_newlines
	movq	%rbx, %r10
.LBB4_177:                              # %get_effective_char.exit1096
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$57, %rdx
	jg	.LBB4_181
# BB#178:                               # %get_effective_char.exit1096
	andq	%r13, %rax
	cmpq	$37, %rax
	jne	.LBB4_201
# BB#179:                               # %land.lhs.true247
	movzbl	747(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_201
# BB#180:                               # %if.then252
	movq	$44, %rax
.LBB4_200:                              # %sw.epilog
	movb	%al, 6(%r15)
	movzbl	7(%r15), %eax
	orq	$2, %rax
	movb	%al, 7(%r15)
	jmp	.LBB4_350
.LBB4_53:
	movabsq	$0, %rcx
.LBB4_55:                               # %if.then88
	leaq	59(%rcx,%rcx,2), %rcx
	movb	%cl, 6(%r15)
	movq	%r10, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	callq	parse_string
	jmp	.LBB4_350
.LBB4_281:                              # %if.then477
	movq	$49, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_273:                              # %get_effective_char.exit1216
	cmpq	$62, %rax
	jne	.LBB4_283
# BB#274:                               # %if.then456
	movq	$50, %rax
	movb	%al, 6(%r15)
	movzbl	742(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_350
# BB#275:                               # %if.then461
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_277
# BB#276:                               # %if.then461
	cmpq	$92, %rax
	jne	.LBB4_278
.LBB4_277:                              # %if.then.i1223
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_278:                              # %get_effective_char.exit1226
	andq	%rbx, %rax
	cmpq	$42, %rax
	jne	.LBB4_280
# BB#279:                               # %if.then465
	movq	$53, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_215:                              # %land.lhs.true296
	movzbl	742(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_222
# BB#216:                               # %do.body302
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_218
# BB#217:                               # %do.body302
	cmpq	$92, %rax
	jne	.LBB4_219
.LBB4_218:                              # %if.then.i1143
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_219:                              # %get_effective_char.exit1146
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_221
# BB#220:                               # %if.then306
	movq	$39, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_207:                              # %get_effective_char.exit1126
	cmpq	$61, %rax
	jne	.LBB4_222
# BB#208:                               # %if.then273
	movq	$26, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_222:                              # %do.body317
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$2, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_254:                              # %if.else402
	decq	%rbx
	movq	%rbx, (%r12)
	jmp	.LBB4_350
.LBB4_56:                               # %if.end92
	movq	%r14, (%r12)
.LBB4_57:                               # %sw.bb94
	movq	$56, %rax
	movb	%al, 6(%r15)
	movq	%r15, 40(%rsp)          # 8-byte Spill
	movq	(%r10), %r12
	movq	(%r12), %r15
	movq	%r15, %rbx
	.align	16, 0x90
.LBB4_58:                               # %while.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rbx), %ecx
	movzbl	_sch_istable(%rcx,%rcx), %eax
	incq	%rbx
	testq	$516, %rax              # imm = 0x204
	jne	.LBB4_58
# BB#59:                                # %while.end.i
	leaq	-1(%rbx), %r13
	addq	$-36, %rcx
	andq	$255, %rcx
	cmpq	$56, %rcx
	ja	.LBB4_93
# BB#60:                                # %while.end.i
	movq	$1, %rax
	shlq	%cl, %rax
	movabsq	$72057594172145665, %rcx # imm = 0x100000008000001
	testq	%rcx, %rax
	je	.LBB4_93
# BB#61:                                # %if.then.i991
	movq	640(%r10), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r10, 32(%rsp)          # 8-byte Spill
	decq	%r15
	subq	%r15, %r13
	movq	24(%rax), %rdi
	movq	32(%rax), %rax
	movq	$32, %rcx
	movq	%r13, %r14
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	leaq	(%rdi,%r14), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB4_63
# BB#62:                                # %if.then.i.i
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	callq	_obstack_newchunk
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	24(%rax), %rdi
.LBB4_63:                               # %if.end.i.i
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	memcpy
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	%r14, 24(%rax)
	movq	%rax, %r15
	movzbl	-1(%rbx), %r14d
	movq	%rbx, (%r12)
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	32(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB4_64
	.align	16, 0x90
.LBB4_73:                               # %if.end26.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	(%r12), %rcx
	leaq	1(%rcx), %rdx
	movq	%rdx, (%r12)
	movzbl	(%rcx), %r14d
	addq	%rax, 24(%rsp)          # 8-byte Folded Spill
	movq	%rbx, %rdx
.LBB4_64:                               # %while.cond.outer.i.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_65 Depth 2
	andq	$255, %r14
	movzbl	_sch_istable(%r14,%r14), %eax
	jmp	.LBB4_65
	.align	16, 0x90
.LBB4_78:                               # %while.cond.backedge.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	movq	%r14, %rax
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
.LBB4_65:                               # %while.cond.outer.i.i
                                        #   Parent Loop BB4_64 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	testq	$516, %rax              # imm = 0x204
	jne	.LBB4_69
# BB#66:                                # %lor.rhs.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	andq	%r13, %r14
	cmpq	$36, %r14
	je	.LBB4_67
# BB#74:                                # %while.end.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	movq	(%r12), %rax
	decq	%rax
	movq	%rax, 8(%r12)
	cmpq	$63, %r14
	je	.LBB4_76
# BB#75:                                # %while.end.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	cmpq	$92, %r14
	jne	.LBB4_81
.LBB4_76:                               # %if.end44.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	movq	%rdx, %rdi
	movq	%rdx, %rbx
	callq	skip_escaped_newlines
	movq	%rax, %r14
                                        # kill: RAX<def> R14<kill>
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$516, %rax              # imm = 0x204
	jne	.LBB4_77
# BB#79:                                # %lor.rhs51.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	andq	%r13, %r14
	cmpq	$36, %r14
	movq	%rbx, %rdx
	jne	.LBB4_81
# BB#80:                                # %land.rhs54.i.i
                                        #   in Loop: Header=BB4_65 Depth=2
	movzbl	767(%rdx), %eax
	testq	%rax, %rax
	movq	$36, %r14
	jne	.LBB4_78
	jmp	.LBB4_81
	.align	16, 0x90
.LBB4_77:                               #   in Loop: Header=BB4_65 Depth=2
	movq	%rbx, %rdx
	jmp	.LBB4_78
.LBB4_67:                               # %land.rhs.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movzbl	767(%rdx), %eax
	testq	%rax, %rax
	movq	$36, %r14
	je	.LBB4_68
	.align	16, 0x90
.LBB4_69:                               # %while.body.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	%rdx, %rbx
	movq	24(%r15), %rax
	movq	32(%r15), %rcx
	leaq	1(%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB4_71
# BB#70:                                # %if.then25.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	_obstack_newchunk
	movq	24(%r15), %rax
.LBB4_71:                               # %if.end26.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movq	%r14, %rcx
	andq	%r13, %rcx
	leaq	1(%rax), %rdx
	cmpq	$36, %rcx
	movq	%rdx, 24(%r15)
	movb	%r14b, (%rax)
	movabsq	$0, %rax
	jne	.LBB4_73
# BB#72:                                # %if.end26.i.i
                                        #   in Loop: Header=BB4_64 Depth=1
	movabsq	$1, %rax
	jmp	.LBB4_73
.LBB4_93:                               # %if.else.i
	movq	640(%r10), %rdi
	decq	%r15
	movq	%r13, %rdx
	subq	%r15, %rdx
	movq	$1, %rcx
	movq	%r15, %rsi
	movq	%r10, %rbx
	callq	ht_lookup
	movq	%rbx, %rdi
	movq	(%rdi), %rcx
	movq	%rax, %rbx
	movq	%r13, (%rcx)
	jmp	.LBB4_94
.LBB4_81:                               # %do.body61.loopexit.i.i
	movq	8(%r12), %rax
.LBB4_82:                               # %do.body61.i.i
	movq	%rax, (%r12)
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%r13, %rax
	je	.LBB4_86
# BB#83:                                # %land.lhs.true67.i.i
	movzbl	770(%rdx), %eax
	testq	%rax, %rax
	je	.LBB4_86
# BB#84:                                # %land.lhs.true71.i.i
	movzbl	9(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB4_86
# BB#85:                                # %if.then73.i.i
	movabsq	$.L.str101, %rsi
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rdx, %rbx
	callq	cpp_pedwarn
	movq	%rbx, %rdx
.LBB4_86:                               # %if.end74.i.i
	movq	%r15, %rbx
	movq	24(%rbx), %r15
	movq	32(%rbx), %rax
	movq	16(%rbx), %r14
	leaq	1(%r15), %rcx
	cmpq	%rax, %rcx
	movq	%r15, %rax
	jbe	.LBB4_88
# BB#87:                                # %if.then88.i.i
	movq	$1, %rsi
	movq	%rbx, %rdi
	movq	%rdx, %r12
	callq	_obstack_newchunk
	movq	%r12, %rdx
	movq	24(%rbx), %rax
.LBB4_88:                               # %if.end89.i.i
	leaq	1(%rax), %rcx
	movq	%rcx, 24(%rbx)
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	16(%rbx), %rsi
	movq	24(%rbx), %rcx
	movq	640(%rdx), %rdi
	cmpq	%rsi, %rcx
	jne	.LBB4_90
# BB#89:                                # %if.then99.i.i
	movzbl	80(%rbx), %eax
	orq	$2, %rax
	movb	%al, 80(%rbx)
.LBB4_90:                               # %if.end100.i.i
	movq	%rdx, %r12
	movslq	48(%rbx), %rax
	addq	%rax, %rcx
	notq	%rax
	andq	%rcx, %rax
	movq	%rax, 24(%rbx)
	movq	32(%rbx), %r8
	movq	8(%rbx), %r9
	subq	%r14, %r15
	movq	%rax, %rdx
	subq	%r9, %rdx
	movq	%r8, %rcx
	subq	%r9, %rcx
	cmpq	%rcx, %rdx
	jle	.LBB4_92
# BB#91:                                # %if.then121.i.i
	movq	%r8, 24(%rbx)
	movq	%r8, %rax
.LBB4_92:                               # %parse_identifier_slow.exit.i
	movq	%rax, 16(%rbx)
	movq	$2, %rcx
	movq	%r15, %rdx
	callq	ht_lookup
	movq	%rax, %rbx
	movq	%r12, %rdi
.LBB4_94:                               # %if.end.i998
	movzbl	21(%rbx), %eax
	testq	$8, %rax
	movq	40(%rsp), %r15          # 8-byte Reload
	je	.LBB4_102
# BB#95:                                # %land.rhs.i
	movzbl	9(%rdi), %ecx
	testq	%rcx, %rcx
	je	.LBB4_96
.LBB4_102:                              # %parse_identifier.exit
	movq	%rbx, 8(%r15)
	movzbl	21(%rbx), %eax
	testq	$1, %rax
	je	.LBB4_350
# BB#103:                               # %if.then103
	movzbl	7(%r15), %eax
	orq	$16, %rax
	movb	%al, 7(%r15)
	movzbl	24(%rbx), %eax
	movb	%al, 6(%r15)
.LBB4_350:                              # %sw.epilog
	movq	%r15, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_96:                               # %if.then25.i
	testq	$2, %rax
	je	.LBB4_99
# BB#97:                                # %land.lhs.true.i1001
	movzbl	14(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB4_99
# BB#98:                                # %if.then32.i
	movq	8(%rbx), %rdx
	movabsq	$.L.str99, %rsi
	xorq	%rax, %rax
	movq	%rdi, %r14
	callq	cpp_error
	movq	%r14, %rdi
.LBB4_99:                               # %if.end33.i
	movq	816(%rdi), %rax
	cmpq	%rax, %rbx
	jne	.LBB4_102
# BB#100:                               # %land.lhs.true36.i
	movzbl	13(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB4_102
# BB#101:                               # %if.then39.i
	movabsq	$.L.str100, %rsi
	xorq	%rax, %rax
	callq	cpp_pedwarn
	jmp	.LBB4_102
.LBB4_68:                               # %while.end.thread.i.i
	movq	(%r12), %rax
	decq	%rax
	movq	%rax, 8(%r12)
	jmp	.LBB4_82
.LBB4_240:                              # %if.else366
	testq	%rcx, %rcx
	je	.LBB4_244
# BB#241:                               # %if.else366
	cmpq	$62, %rax
	jne	.LBB4_244
# BB#242:                               # %if.then374
	movzbl	7(%r15), %eax
	orq	$2, %rax
	movb	%al, 7(%r15)
.LBB4_243:                              # %sw.bb653
	movq	$45, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_291:                              # %do.body505
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$9, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_267:                              # %do.body445
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$4, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_299:                              # %do.body524
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$10, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_257:                              # %if.else415
	cmpq	$42, %rcx
	jne	.LBB4_280
# BB#258:                               # %land.lhs.true418
	movzbl	742(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_280
# BB#259:                               # %if.then423
	movq	$54, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_280:                              # %do.body361
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	jmp	.LBB4_350
.LBB4_181:                              # %get_effective_char.exit1096
	addq	$-58, %rax
	andq	%r13, %rax
	cmpq	$5, %rax
	ja	.LBB4_201
# BB#182:                               # %get_effective_char.exit1096
	jmpq	*.LJTI4_1(,%rax,8)
.LBB4_198:                              # %land.lhs.true234
	movzbl	747(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_201
# BB#199:                               # %if.then238
	movq	$42, %rax
	jmp	.LBB4_200
.LBB4_214:                              # %do.body286
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$12, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_283:                              # %do.body485
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$5, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_244:                              # %do.body381
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$8, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_221:                              # %do.body309
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$15, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_239:                              # %if.else356
	decq	%r14
	movq	%r14, (%r12)
	jmp	.LBB4_350
.LBB4_160:                              # %if.end179
	movq	%r15, %r13
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movzbl	-1(%rcx), %edx
	movq	80(%r10), %rax
	movq	$1, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	subq	%rdi, %rsi
	movzbl	_sch_istable(%rdx,%rdx), %ebx
	addq	%rcx, %rsi
	movq	$53, %rcx
	shlq	%cl, %rbx
	movq	$63, %rcx
	sarq	%cl, %rbx
	movq	16(%rax), %rdi
	movq	24(%rax), %rcx
	addq	%rsi, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	%rbx, %r15
	andq	%r14, %r15
	subq	%rdi, %rcx
	cmpq	%r15, %rcx
	jae	.LBB4_171
# BB#161:                               # %if.then.i.i1084
	movq	88(%r10), %rax
	movq	%r10, %r12
	testq	%rax, %rax
	je	.LBB4_166
# BB#162:                               # %if.end.lr.ph.i.i.i
	leaq	88(%r12), %rdx
	leaq	(%r15,%r15,2), %rsi
	movq	$1, %rcx
	shrq	%cl, %rsi
	addq	$8000, %rsi             # imm = 0x1F40
.LBB4_163:                              # %if.end.i.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rdi
	movq	8(%rax), %r8
	movq	(%rax), %rcx
	subq	%r8, %rdi
	cmpq	%r15, %rdi
	jb	.LBB4_165
# BB#164:                               # %if.end.i.i.i
                                        #   in Loop: Header=BB4_163 Depth=1
	cmpq	%rsi, %rdi
	jbe	.LBB4_169
.LBB4_165:                              # %for.inc.i.i.i
                                        #   in Loop: Header=BB4_163 Depth=1
	movq	%rax, %rdx
	testq	%rcx, %rcx
	movq	%rcx, %rax
	jne	.LBB4_163
.LBB4_166:                              # %if.then.i.i.i
	cmpq	$8000, %r15             # imm = 0x1F40
	movq	$8000, %rax             # imm = 0x1F40
	jb	.LBB4_168
# BB#167:                               # %if.then.i.i.i
	leaq	7(%r15), %rax
	movabsq	$8589934584, %rcx       # imm = 0x1FFFFFFF8
	andq	%rax, %rcx
	movq	%rcx, %rax
.LBB4_168:                              # %if.then.i.i.i
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	32(%rax), %rax
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, %rdi
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	(%rdi,%rax), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rdi, 8(%rdi,%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rdi, 16(%rdi,%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 24(%rdi,%rcx)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	$0, (%rdi,%rcx)
	jmp	.LBB4_170
.LBB4_158:                              # %if.then165
	movq	$31, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_159:                              # %do.body
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$7, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_184:                              # %do.body195
	movq	(%r10), %rax
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_186
# BB#185:                               # %do.body195
	cmpq	$92, %rax
	jne	.LBB4_187
.LBB4_186:                              # %if.then.i1103
	movq	%r10, %rdi
	callq	skip_escaped_newlines
.LBB4_187:                              # %get_effective_char.exit1106
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_189
# BB#188:                               # %if.then199
	movq	$37, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_183:                              # %if.then189
	movq	$27, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_190:                              # %land.lhs.true212
	movzbl	742(%r10), %eax
	testq	%rax, %rax
	je	.LBB4_201
# BB#191:                               # %do.body217
	movq	%r15, %rbx
	movq	(%r10), %rax
	movq	%r10, %rsi
	movq	(%rax), %rcx
	movq	%rcx, 8(%rax)
	leaq	1(%rcx), %rdx
	movq	%rdx, (%rax)
	movzbl	(%rcx), %eax
	cmpq	$63, %rax
	je	.LBB4_193
# BB#192:                               # %do.body217
	cmpq	$92, %rax
	jne	.LBB4_194
.LBB4_193:                              # %if.then.i1113
	movq	%rsi, %rdi
	callq	skip_escaped_newlines
.LBB4_194:                              # %get_effective_char.exit1116
	andq	%r13, %rax
	cmpq	$61, %rax
	jne	.LBB4_197
# BB#195:                               # %if.then221
	movq	$38, %rax
	jmp	.LBB4_196
.LBB4_201:                              # %do.body259
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$3, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_169:                              # %for.end.i.i.i
	movq	%rcx, (%rdx)
	movq	$0, (%rax)
	movq	8(%rax), %rdi
	movq	%rdi, 16(%rax)
.LBB4_170:                              # %_cpp_get_buff.exit.i.i
	movq	%r12, %rdx
	movq	80(%rdx), %rcx
	movq	%rcx, (%rax)
	movq	%rax, 80(%rdx)
.LBB4_171:                              # %save_comment.exit
	addq	%rdi, %r15
	movq	%r15, 16(%rax)
	movq	$64, %rax
	movq	%r13, %r15
	movb	%al, 6(%r15)
	movl	%ebx, 8(%r15)
	movq	%rdi, 16(%r15)
	movq	$47, %rax
	movb	%al, (%rdi)
	incq	%rdi
	decq	%rbx
	andq	%r14, %rbx
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	%rbx, %rdx
	callq	memcpy
	jmp	.LBB4_350
.LBB4_189:                              # %do.body202
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$13, %rax
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.LBB4_197:                              # %do.body224
	movq	8(%r12), %rax
	movq	%rax, (%r12)
	movq	$14, %rax
.LBB4_196:                              # %sw.epilog
	movq	%rbx, %r15
	movb	%al, 6(%r15)
	jmp	.LBB4_350
.Ltmp61:
	.size	_cpp_lex_direct, .Ltmp61-_cpp_lex_direct
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI4_0:
	.quad	.LBB4_7
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_7
	.quad	.LBB4_34
	.quad	.LBB4_7
	.quad	.LBB4_7
	.quad	.LBB4_34
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_349
	.quad	.LBB4_7
	.quad	.LBB4_323
	.quad	.LBB4_104
	.quad	.LBB4_335
	.quad	.LBB4_348
	.quad	.LBB4_223
	.quad	.LBB4_284
	.quad	.LBB4_104
	.quad	.LBB4_342
	.quad	.LBB4_343
	.quad	.LBB4_311
	.quad	.LBB4_260
	.quad	.LBB4_341
	.quad	.LBB4_268
	.quad	.LBB4_245
	.quad	.LBB4_109
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_46
	.quad	.LBB4_300
	.quad	.LBB4_346
	.quad	.LBB4_172
	.quad	.LBB4_317
	.quad	.LBB4_202
	.quad	.LBB4_41
	.quad	.LBB4_347
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_47
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_344
	.quad	.LBB4_41
	.quad	.LBB4_309
	.quad	.LBB4_329
	.quad	.LBB4_57
	.quad	.LBB4_349
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_57
	.quad	.LBB4_345
	.quad	.LBB4_292
	.quad	.LBB4_243
	.quad	.LBB4_340
.LJTI4_1:
	.quad	.LBB4_198
	.quad	.LBB4_201
	.quad	.LBB4_184
	.quad	.LBB4_183
	.quad	.LBB4_201
	.quad	.LBB4_190

	.text
	.align	16, 0x90
	.type	skip_escaped_newlines,@function
skip_escaped_newlines:                  # @skip_escaped_newlines
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp70:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp71:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp72:
	.cfi_def_cfa_offset 48
.Ltmp73:
	.cfi_offset %rbx, -48
.Ltmp74:
	.cfi_offset %r12, -40
.Ltmp75:
	.cfi_offset %r13, -32
.Ltmp76:
	.cfi_offset %r14, -24
.Ltmp77:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	(%r12), %r14
	movq	(%r14), %rax
	movzbl	81(%r14), %ecx
	movzbl	-1(%rax), %ebx
	testq	%rcx, %rcx
	jne	.LBB5_27
# BB#1:                                 # %do.body.preheader
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r15, %r15
	movq	%r14, %rcx
	.align	16, 0x90
.LBB5_2:                                # %do.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_14 Depth 2
	movq	%rbx, %rdx
	andq	%r13, %rdx
	cmpq	$63, %rdx
	jne	.LBB5_13
# BB#3:                                 # %if.then3
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	(%rax), %edx
	cmpq	$63, %rdx
	movq	$63, %rbx
	jne	.LBB5_27
# BB#4:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	(%rcx), %rdx
	movzbl	1(%rdx), %r8d
	movzbl	_cpp_trigraph_map(%r8), %r9d
	testq	%r9, %r9
	je	.LBB5_27
# BB#5:                                 # %if.end.i
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	759(%r12), %edi
	movzbl	746(%r12), %esi
	testq	%rdi, %rdi
	je	.LBB5_11
# BB#6:                                 # %land.lhs.true.i
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	12(%r12), %edi
	testq	%rdi, %rdi
	jne	.LBB5_11
# BB#7:                                 # %if.then8.i
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	$255, %rsi
	je	.LBB5_9
# BB#8:                                 # %trigraph_p.exit.thread107
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	24(%rcx), %rax
	movl	64(%rcx), %ecx
	movl	64(%r12), %esi
	addq	%r13, %rdx
	subq	%rax, %rdx
	addq	%rcx, %rdx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str108, %rcx
	callq	cpp_warning_with_line
	movq	(%r14), %rax
	jmp	.LBB5_12
	.align	16, 0x90
.LBB5_11:                               # %trigraph_p.exit
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	$255, %rsi
	je	.LBB5_27
.LBB5_12:                               # %if.end
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	1(%rax), %ecx
	movzbl	_cpp_trigraph_map(%rcx), %ebx
	addq	$2, %rax
	cmpq	$92, %rbx
	movq	%rax, (%r14)
	jne	.LBB5_27
.LBB5_13:                               # %if.end19
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	16(%r14), %rdx
	xorq	%rcx, %rcx
	cmpq	%rdx, %rax
	je	.LBB5_27
	.align	16, 0x90
.LBB5_14:                               # %do.body26
                                        #   Parent Loop BB5_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	leaq	1(%rax,%rcx), %rdi
	movq	%rdi, (%r14)
	movzbl	(%rax,%rcx), %esi
	movzbl	_sch_istable(%rsi,%rsi), %esi
	incq	%rcx
	testq	$2048, %rsi             # imm = 0x800
	je	.LBB5_16
# BB#15:                                # %do.body26
                                        #   in Loop: Header=BB5_14 Depth=2
	cmpq	%rdx, %rdi
	jb	.LBB5_14
.LBB5_16:                               # %do.end
                                        #   in Loop: Header=BB5_2 Depth=1
	testq	$1024, %rsi             # imm = 0x400
	je	.LBB5_17
# BB#18:                                # %if.end46
                                        #   in Loop: Header=BB5_2 Depth=1
	cmpq	$1, %rcx
	je	.LBB5_21
# BB#19:                                # %land.lhs.true
                                        #   in Loop: Header=BB5_2 Depth=1
	movzbl	12(%r12), %eax
	testq	%rax, %rax
	jne	.LBB5_21
# BB#20:                                # %if.then52
                                        #   in Loop: Header=BB5_2 Depth=1
	xorq	%rax, %rax
	movq	%r12, %rdi
	movabsq	$.L.str106, %rsi
	callq	cpp_warning
.LBB5_21:                               # %if.end53
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	(%r12), %rcx
	movq	(%rcx), %rax
	movzbl	-1(%rax), %edx
	movzbl	(%rax), %esi
	addq	%rdx, %rsi
	andq	%r13, %rsi
	cmpq	$23, %rsi
	jne	.LBB5_23
# BB#22:                                # %if.then.i
                                        #   in Loop: Header=BB5_2 Depth=1
	incq	%rax
	movq	%rax, (%rcx)
.LBB5_23:                               # %handle_newline.exit
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	%rax, 24(%rcx)
	movl	%r15d, 64(%rcx)
	movl	64(%r12), %eax
	incq	%rax
	movl	%eax, 64(%r12)
	movq	(%r14), %rdx
	movq	%rdx, 8(%r14)
	movq	16(%r14), %rax
	cmpq	%rax, %rdx
	je	.LBB5_24
# BB#25:                                # %do.cond64
                                        #   in Loop: Header=BB5_2 Depth=1
	leaq	1(%rdx), %rax
	movq	%rax, (%r14)
	movzbl	(%rdx), %ebx
	cmpq	$63, %rbx
	je	.LBB5_2
# BB#26:                                # %do.cond64
                                        #   in Loop: Header=BB5_2 Depth=1
	cmpq	$92, %rbx
	je	.LBB5_2
	jmp	.LBB5_27
.LBB5_17:                               # %if.then44
	movq	%rax, (%r14)
	jmp	.LBB5_27
.LBB5_24:                               # %do.cond64.thread
	movabsq	$.L.str107, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdi
	callq	cpp_pedwarn
	movq	%r13, %rbx
.LBB5_27:                               # %if.end70
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB5_9:                                # %if.else.i
	movq	72(%rcx), %rax
	cmpq	%rax, %rdx
	je	.LBB5_27
# BB#10:                                # %if.then19.i
	movq	%rdx, 72(%rcx)
	movq	24(%rcx), %rax
	movl	64(%rcx), %ecx
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movl	64(%r12), %esi
	addq	%rdx, %r9
	subq	%rax, %r9
	addq	%rcx, %r9
	movabsq	$.L.str109, %rcx
	xorq	%rax, %rax
	movq	%r12, %rdi
	movq	%r9, %rdx
	callq	cpp_warning_with_line
	jmp	.LBB5_27
.Ltmp78:
	.size	skip_escaped_newlines, .Ltmp78-skip_escaped_newlines
	.cfi_endproc

	.align	16, 0x90
	.type	parse_number,@function
parse_number:                           # @parse_number
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp85:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp86:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp87:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp88:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp89:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp90:
	.cfi_def_cfa_offset 80
.Ltmp91:
	.cfi_offset %rbx, -48
.Ltmp92:
	.cfi_offset %r12, -40
.Ltmp93:
	.cfi_offset %r13, -32
.Ltmp94:
	.cfi_offset %r14, -24
.Ltmp95:
	.cfi_offset %r15, -16
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	80(%rdi), %r14
	movq	(%rdi), %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	16(%r14), %r15
	movq	24(%r14), %r13
	movq	%rdx, %r12
	movq	%rsi, (%rsp)            # 8-byte Spill
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rcx
	je	.LBB6_11
# BB#1:                                 # %if.then
	cmpq	%r13, %r15
	jne	.LBB6_10
# BB#2:                                 # %if.then4
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	88(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB6_7
# BB#3:
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	88(%rax), %rax
	.align	16, 0x90
.LBB6_4:                                # %if.end.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rdx
	movq	8(%rbx), %rsi
	movq	(%rbx), %rcx
	subq	%rsi, %rdx
	je	.LBB6_6
# BB#5:                                 # %if.end.i.i
                                        #   in Loop: Header=BB6_4 Depth=1
	cmpq	$8002, %rdx             # imm = 0x1F42
	jb	.LBB6_8
.LBB6_6:                                # %for.inc.i.i
                                        #   in Loop: Header=BB6_4 Depth=1
	movq	%rbx, %rax
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.LBB6_4
.LBB6_7:                                # %if.then.i.i
	movq	$8032, %rdi             # imm = 0x1F60
	callq	xmalloc
	movq	%rax, %rdi
	leaq	8000(%rdi), %rbx
	movq	%rdi, 8008(%rdi)
	movq	%rdi, 8016(%rdi)
	movq	%rbx, 8024(%rdi)
	movq	$0, 8000(%rdi)
	jmp	.LBB6_9
.LBB6_8:                                # %for.end.i.i
	movq	%rcx, (%rax)
	movq	$0, (%rbx)
	movq	8(%rbx), %rdi
	movq	%rdi, 16(%rbx)
.LBB6_9:                                # %_cpp_extend_buff.exit
	movq	16(%r14), %rsi
	movq	24(%r14), %rdx
	subq	%rsi, %rdx
	callq	memcpy
	movq	%r14, (%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rbx, 80(%rax)
	movq	16(%rbx), %r15
	movq	24(%rbx), %r13
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB6_10:                               # %if.end
	movq	$46, %rax
	movb	%al, (%r15)
	incq	%r15
.LBB6_11:                               # %do.body11.preheader
	movq	24(%rsp), %rax          # 8-byte Reload
	leaq	88(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	.align	16, 0x90
.LBB6_12:                               # %do.body11
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB6_16 Depth 2
	movq	%r13, %rax
	subq	%r15, %rax
	cmpq	$1, %rax
	ja	.LBB6_13
# BB#14:                                # %if.then13
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	80(%rcx), %r14
	movq	16(%r14), %rax
	movq	24(%r14), %r8
	movq	88(%rcx), %rbx
	subq	%rax, %r15
	subq	%rax, %r8
	leaq	2(%r8,%r8), %rdx
	testq	%rbx, %rbx
	je	.LBB6_19
# BB#15:                                # %if.end.lr.ph.i.i204
                                        #   in Loop: Header=BB6_12 Depth=1
	leaq	(%rdx,%rdx,2), %r9
	movq	$1, %rcx
	shrq	%cl, %r9
	addq	$8000, %r9              # imm = 0x1F40
	movq	8(%rsp), %rcx           # 8-byte Reload
	.align	16, 0x90
.LBB6_16:                               # %if.end.i.i230
                                        #   Parent Loop BB6_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rax
	movq	8(%rbx), %rsi
	movq	(%rbx), %rdi
	subq	%rsi, %rax
	cmpq	%rdx, %rax
	jb	.LBB6_18
# BB#17:                                # %if.end.i.i230
                                        #   in Loop: Header=BB6_16 Depth=2
	cmpq	%r9, %rax
	jbe	.LBB6_22
.LBB6_18:                               # %for.inc.i.i232
                                        #   in Loop: Header=BB6_16 Depth=2
	movq	%rbx, %rcx
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.LBB6_16
.LBB6_19:                               # %if.then.i.i219
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$8000, %rdx             # imm = 0x1F40
	movq	$8000, %r13             # imm = 0x1F40
	jb	.LBB6_21
# BB#20:                                # %if.then.i.i219
                                        #   in Loop: Header=BB6_12 Depth=1
	addq	%r8, %r8
	addq	$9, %r8
	andq	$-8, %r8
	movq	%r8, %r13
.LBB6_21:                               # %if.then.i.i219
                                        #   in Loop: Header=BB6_12 Depth=1
	leaq	32(%r13), %rdi
	callq	xmalloc
	movq	%rax, %rdi
	leaq	(%rdi,%r13), %rbx
	movq	%rdi, 8(%rdi,%r13)
	movq	%rdi, 16(%rdi,%r13)
	movq	%rbx, 24(%rdi,%r13)
	movq	$0, (%rdi,%r13)
	jmp	.LBB6_23
	.align	16, 0x90
.LBB6_13:                               #   in Loop: Header=BB6_12 Depth=1
	movq	%r15, %r14
	jmp	.LBB6_24
.LBB6_22:                               # %for.end.i.i234
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	%rdi, (%rcx)
	movq	$0, (%rbx)
	movq	8(%rbx), %rdi
	movq	%rdi, 16(%rbx)
.LBB6_23:                               # %_cpp_extend_buff.exit240
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	16(%r14), %rsi
	movq	24(%r14), %rdx
	subq	%rsi, %rdx
	callq	memcpy
	movq	%r14, (%rbx)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	%rbx, 80(%rax)
	movq	16(%rbx), %r14
	movq	24(%rbx), %r13
	addq	%r15, %r14
	movq	16(%rsp), %rbx          # 8-byte Reload
.LBB6_24:                               # %if.end24
                                        #   in Loop: Header=BB6_12 Depth=1
	movb	%r12b, (%r14)
	movq	(%rbx), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, (%rbx)
	movzbl	(%rax), %r12d
	movzbl	_sch_istable(%r12,%r12), %ecx
	leaq	1(%r14), %r15
	testq	$516, %rcx              # imm = 0x204
	jne	.LBB6_12
# BB#25:                                # %if.end24
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$46, %r12
	je	.LBB6_12
# BB#26:                                # %lor.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$43, %r12
	je	.LBB6_28
# BB#27:                                # %lor.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$45, %r12
	jne	.LBB6_34
.LBB6_28:                               # %land.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	movsbq	(%r14), %rcx
	cmpq	$79, %rcx
	jg	.LBB6_30
# BB#29:                                # %land.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	andq	$255, %rcx
	cmpq	$69, %rcx
	je	.LBB6_12
	jmp	.LBB6_34
.LBB6_30:                               # %land.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	andq	$255, %rcx
	cmpq	$101, %rcx
	je	.LBB6_12
# BB#31:                                # %land.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$80, %rcx
	je	.LBB6_33
# BB#32:                                # %land.rhs
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$112, %rcx
	jne	.LBB6_34
.LBB6_33:                               # %land.rhs58
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movzbl	748(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB6_12
	.align	16, 0x90
.LBB6_34:                               # %do.end
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	%rax, 8(%rbx)
	cmpq	$63, %r12
	je	.LBB6_36
# BB#35:                                # %do.end
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$92, %r12
	jne	.LBB6_46
.LBB6_36:                               # %if.end70
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	skip_escaped_newlines
	movq	%rax, %r12
                                        # kill: RAX<def> R12<kill>
	andq	$255, %rax
	movzbl	_sch_istable(%rax,%rax), %eax
	testq	$516, %rax              # imm = 0x204
	jne	.LBB6_12
# BB#37:                                # %if.end70
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$46, %rax
	je	.LBB6_12
# BB#38:                                # %lor.rhs81
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$43, %rax
	je	.LBB6_40
# BB#39:                                # %lor.rhs81
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$45, %rax
	jne	.LBB6_46
.LBB6_40:                               # %land.rhs87
                                        #   in Loop: Header=BB6_12 Depth=1
	movsbq	(%r14), %rax
	cmpq	$79, %rax
	jg	.LBB6_42
# BB#41:                                # %land.rhs87
                                        #   in Loop: Header=BB6_12 Depth=1
	andq	$255, %rax
	cmpq	$69, %rax
	je	.LBB6_12
	jmp	.LBB6_46
.LBB6_42:                               # %land.rhs87
                                        #   in Loop: Header=BB6_12 Depth=1
	andq	$255, %rax
	cmpq	$80, %rax
	je	.LBB6_45
# BB#43:                                # %land.rhs87
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$101, %rax
	je	.LBB6_12
# BB#44:                                # %land.rhs87
                                        #   in Loop: Header=BB6_12 Depth=1
	cmpq	$112, %rax
	jne	.LBB6_46
.LBB6_45:                               # %land.rhs107
                                        #   in Loop: Header=BB6_12 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	748(%rax), %eax
	testq	%rax, %rax
	jne	.LBB6_12
.LBB6_46:                               # %do.body117
	movq	8(%rbx), %rax
	movq	%rax, (%rbx)
	xorq	%rax, %rax
	movb	%al, 1(%r14)
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	80(%rax), %rax
	movq	16(%rax), %rcx
	movq	(%rsp), %rdx            # 8-byte Reload
	movq	%rcx, 8(%rdx)
	subq	%rcx, %r15
	movl	%r15d, (%rdx)
	addq	$2, %r14
	movq	%r14, 16(%rax)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp96:
	.size	parse_number, .Ltmp96-parse_number
	.cfi_endproc

	.align	16, 0x90
	.type	parse_string,@function
parse_string:                           # @parse_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp103:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp104:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp105:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp106:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp107:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp108:
	.cfi_def_cfa_offset 112
.Ltmp109:
	.cfi_offset %rbx, -48
.Ltmp110:
	.cfi_offset %r12, -40
.Ltmp111:
	.cfi_offset %r13, -32
.Ltmp112:
	.cfi_offset %r14, -24
.Ltmp113:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	80(%r13), %rax
	movq	%rdx, 56(%rsp)          # 8-byte Spill
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movq	(%r13), %rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	movq	16(%rax), %r14
	movq	24(%rax), %r15
	leaq	88(%r13), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$0, 24(%rsp)            # 8-byte Folded Spill
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_44:                               # %if.end74
                                        #   in Loop: Header=BB7_1 Depth=1
	movb	%r12b, (%r14)
	incq	%r14
.LBB7_1:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB7_4 Depth 2
                                        #     Child Loop BB7_18 Depth 2
	cmpq	%r14, %r15
	jne	.LBB7_12
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	80(%r13), %r14
	movq	16(%r14), %rax
	movq	24(%r14), %r8
	movq	88(%r13), %rbx
	subq	%rax, %r15
	subq	%rax, %r8
	leaq	2(%r8,%r8), %rdx
	testq	%rbx, %rbx
	je	.LBB7_7
# BB#3:                                 # %if.end.lr.ph.i.i
                                        #   in Loop: Header=BB7_1 Depth=1
	leaq	(%rdx,%rdx,2), %r9
	movq	$1, %rcx
	shrq	%cl, %r9
	addq	$8000, %r9              # imm = 0x1F40
	movq	32(%rsp), %rcx          # 8-byte Reload
	.align	16, 0x90
.LBB7_4:                                # %if.end.i.i
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	24(%rbx), %rax
	movq	8(%rbx), %rsi
	movq	(%rbx), %rdi
	subq	%rsi, %rax
	cmpq	%rdx, %rax
	jb	.LBB7_6
# BB#5:                                 # %if.end.i.i
                                        #   in Loop: Header=BB7_4 Depth=2
	cmpq	%r9, %rax
	jbe	.LBB7_10
.LBB7_6:                                # %for.inc.i.i
                                        #   in Loop: Header=BB7_4 Depth=2
	movq	%rbx, %rcx
	testq	%rdi, %rdi
	movq	%rdi, %rbx
	jne	.LBB7_4
.LBB7_7:                                # %if.then.i.i
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	$8000, %rdx             # imm = 0x1F40
	movq	$8000, %r12             # imm = 0x1F40
	jb	.LBB7_9
# BB#8:                                 # %if.then.i.i
                                        #   in Loop: Header=BB7_1 Depth=1
	addq	%r8, %r8
	addq	$9, %r8
	andq	$-8, %r8
	movq	%r8, %r12
.LBB7_9:                                # %if.then.i.i
                                        #   in Loop: Header=BB7_1 Depth=1
	leaq	32(%r12), %rdi
	callq	xmalloc
	movq	%rax, %rdi
	leaq	(%rdi,%r12), %rbx
	movq	%rdi, 8(%rdi,%r12)
	movq	%rdi, 16(%rdi,%r12)
	movq	%rbx, 24(%rdi,%r12)
	movq	$0, (%rdi,%r12)
	jmp	.LBB7_11
.LBB7_10:                               # %for.end.i.i
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	%rdi, (%rcx)
	movq	$0, (%rbx)
	movq	8(%rbx), %rdi
	movq	%rdi, 16(%rbx)
.LBB7_11:                               # %_cpp_extend_buff.exit
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	16(%r14), %rsi
	movq	24(%r14), %rdx
	subq	%rsi, %rdx
	callq	memcpy
	movq	%r14, (%rbx)
	movq	%rbx, 80(%r13)
	movq	16(%rbx), %r14
	movq	24(%rbx), %rax
	addq	%r15, %r14
	movq	%rax, %r15
.LBB7_12:                               # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	48(%rsp), %rdx          # 8-byte Reload
	movq	(%rdx), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, (%rdx)
	movzbl	(%rax), %r12d
	cmpq	$63, %r12
	je	.LBB7_14
# BB#13:                                # %if.end
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	$92, %r12
	jne	.LBB7_15
.LBB7_14:                               # %if.then19
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	%r13, %rdi
	callq	skip_escaped_newlines
	movq	%rax, %r12
.LBB7_15:                               # %if.end20
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	56(%rsp), %rbx          # 8-byte Reload
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%rcx, %rbx
	movq	%r12, %rax
	andq	%rcx, %rax
	cmpq	%rbx, %rax
	jne	.LBB7_21
# BB#16:                                # %if.then23
                                        #   in Loop: Header=BB7_1 Depth=1
	movzbl	10(%r13), %eax
	testq	%rax, %rax
	jne	.LBB7_25
# BB#17:                                # %if.end.i
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	80(%r13), %rax
	movq	16(%rax), %rax
	movq	%r14, %rdx
	.align	16, 0x90
.LBB7_18:                               # %for.cond.i
                                        #   Parent Loop BB7_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rdx, %rcx
	cmpq	%rax, %rcx
	jbe	.LBB7_20
# BB#19:                                # %land.rhs.i
                                        #   in Loop: Header=BB7_18 Depth=2
	movzbl	-1(%rcx), %esi
	leaq	-1(%rcx), %rdx
	cmpq	$92, %rsi
	je	.LBB7_18
.LBB7_20:                               # %unescaped_terminator_p.exit
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	%r14, %rax
	subq	%rcx, %rax
	testq	$1, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB7_44
	jmp	.LBB7_25
	.align	16, 0x90
.LBB7_21:                               # %if.else
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	%r12, %rax
	andq	$255, %rax
	movzbl	_sch_istable+1(%rax,%rax), %eax
	testq	$4, %rax
	jne	.LBB7_22
# BB#38:                                # %if.else57
                                        #   in Loop: Header=BB7_1 Depth=1
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB7_44
# BB#39:                                # %if.then60
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	48(%rsp), %rcx          # 8-byte Reload
	movq	(%rcx), %rax
	movq	16(%rcx), %rcx
	decq	%rax
	cmpq	%rcx, %rax
	je	.LBB7_40
# BB#42:                                # %if.end68
                                        #   in Loop: Header=BB7_1 Depth=1
	xorq	%r12, %r12
	testq	$1, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB7_44
# BB#43:                                # %if.then70
                                        #   in Loop: Header=BB7_1 Depth=1
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r13, %rdi
	movabsq	$.L.str103, %rsi
	callq	cpp_warning
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
	jmp	.LBB7_44
	.align	16, 0x90
.LBB7_22:                               # %if.then30
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	736(%r13), %eax
	cmpq	$9, %rax
	jne	.LBB7_26
# BB#23:                                # %if.then30
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	$62, %rbx
	jne	.LBB7_24
.LBB7_26:                               # %if.end38
                                        #   in Loop: Header=BB7_1 Depth=1
	cmpq	$34, %rbx
	jne	.LBB7_45
# BB#27:                                # %lor.lhs.false41
                                        #   in Loop: Header=BB7_1 Depth=1
	movzbl	10(%r13), %eax
	testq	%rax, %rax
	jne	.LBB7_28
# BB#31:                                # %if.end47
                                        #   in Loop: Header=BB7_1 Depth=1
	testq	$1, 40(%rsp)            # 8-byte Folded Reload
	jne	.LBB7_33
# BB#32:                                # %if.then49
                                        #   in Loop: Header=BB7_1 Depth=1
	xorq	%rax, %rax
	movq	%r13, %rdi
	movabsq	$.L.str102, %rsi
	callq	cpp_pedwarn
	movq	$1, 40(%rsp)            # 8-byte Folded Spill
.LBB7_33:                               # %if.end50
                                        #   in Loop: Header=BB7_1 Depth=1
	movl	252(%r13), %eax
	testq	%rax, %rax
	jne	.LBB7_35
# BB#34:                                # %if.then53
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	movl	(%rcx), %eax
	movl	%eax, 252(%r13)
	movzbl	4(%rcx), %eax
	movl	%eax, 256(%r13)
.LBB7_35:                               # %if.end56
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	(%r13), %rax
	movq	(%rax), %rcx
	movzbl	-1(%rcx), %edx
	movzbl	(%rcx), %esi
	addq	%rdx, %rsi
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rdx, %rsi
	cmpq	$23, %rsi
	jne	.LBB7_37
# BB#36:                                # %if.then.i160
                                        #   in Loop: Header=BB7_1 Depth=1
	incq	%rcx
	movq	%rcx, (%rax)
.LBB7_37:                               # %handle_newline.exit
                                        #   in Loop: Header=BB7_1 Depth=1
	movq	%rcx, 24(%rax)
	xorq	%rcx, %rcx
	movl	%ecx, 64(%rax)
	movl	64(%r13), %eax
	incq	%rax
	movl	%eax, 64(%r13)
	movq	$10, %r12
	jmp	.LBB7_44
.LBB7_45:                               # %if.then44
	movabsq	$.L.str104, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	cpp_error
	jmp	.LBB7_24
.LBB7_28:                               # %land.lhs.true.i147
	movabsq	$.L.str104, %rsi
	movq	$34, %rdx
	xorq	%rax, %rax
	movq	%r13, %rdi
	callq	cpp_error
	jmp	.LBB7_41
.LBB7_40:                               # %if.then65
	movabsq	$.L.str104, %rsi
	xorq	%rax, %rax
	movq	%r13, %rdi
	movq	56(%rsp), %rdx          # 8-byte Reload
	callq	cpp_error
	cmpq	$34, %rbx
	jne	.LBB7_24
.LBB7_41:                               # %land.lhs.true.i
	movl	252(%r13), %esi
	testq	%rsi, %rsi
	je	.LBB7_24
# BB#29:                                # %land.lhs.true1.i150
	movl	64(%r13), %eax
	cmpq	%rax, %rsi
	je	.LBB7_24
# BB#30:                                # %if.then.i152
	movl	256(%r13), %edx
	movabsq	$.L.str105, %rcx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	callq	cpp_error_with_line
	movl	%ebx, 252(%r13)
.LBB7_24:                               # %if.then35
	movq	48(%rsp), %rax          # 8-byte Reload
	decq	(%rax)
.LBB7_25:                               # %for.end
	xorq	%rax, %rax
	movb	%al, (%r14)
	movq	80(%r13), %rax
	movq	16(%rax), %rcx
	movq	16(%rsp), %rsi          # 8-byte Reload
	movq	%rcx, 16(%rsi)
	movq	%r14, %rdx
	subq	%rcx, %rdx
	movl	%edx, 8(%rsi)
	incq	%r14
	movq	%r14, 16(%rax)
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp114:
	.size	parse_string, .Ltmp114-parse_string
	.cfi_endproc

	.globl	cpp_token_len
	.align	16, 0x90
	.type	cpp_token_len,@function
cpp_token_len:                          # @cpp_token_len
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	6(%rdi), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	token_spellings(%rax), %ecx
	cmpq	$2, %rcx
	je	.LBB8_3
# BB#1:                                 # %entry
	xorq	%rax, %rax
	addq	$-3, %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$1, %rdx
	ja	.LBB8_4
# BB#2:                                 # %sw.bb
	movl	8(%rdi), %eax
	addq	$5, %rax
	retq
.LBB8_3:                                # %sw.bb2
	movq	8(%rdi), %rax
	movl	(%rax), %eax
.LBB8_4:                                # %sw.epilog
	addq	$5, %rax
	retq
.Ltmp115:
	.size	cpp_token_len, .Ltmp115-cpp_token_len
	.cfi_endproc

	.globl	cpp_spell_token
	.align	16, 0x90
	.type	cpp_spell_token,@function
cpp_spell_token:                        # @cpp_spell_token
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp120:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp121:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp122:
	.cfi_def_cfa_offset 32
.Ltmp123:
	.cfi_offset %rbx, -32
.Ltmp124:
	.cfi_offset %r14, -24
.Ltmp125:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	6(%r14), %r8d
	movq	%r8, %rsi
	andq	$255, %rsi
	movq	$4, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movl	token_spellings(%rax), %ecx
	movq	%rdx, %rbx
	cmpq	$5, %rcx
	ja	.LBB9_22
# BB#1:                                 # %entry
	jmpq	*.LJTI9_0(,%rcx,8)
.LBB9_2:                                # %sw.bb
	movq	$8, %rcx
	shrq	%cl, %r8
	testq	$2, %r8
	je	.LBB9_4
# BB#3:                                 # %if.then
	leaq	digraph_spellings-320(,%rsi,8), %rax
	jmp	.LBB9_6
.LBB9_9:                                # %sw.bb21
	movzbl	8(%r14), %eax
	movb	%al, (%rbx)
	incq	%rbx
	jmp	.LBB9_22
.LBB9_11:                               # %sw.bb34
	movq	16(%r14), %rsi
	movl	8(%r14), %edx
	movq	%rbx, %rdi
	callq	memcpy
	movl	8(%r14), %eax
	addq	%rax, %rbx
	jmp	.LBB9_22
.LBB9_12:                               # %sw.bb46
	movq	%rsi, %rax
	addq	$-58, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$5, %rdx
	ja	.LBB9_16
# BB#13:                                # %sw.bb46
	movq	$34, %r15
	movq	$34, %rcx
	jmpq	*.LJTI9_1(,%rdx,8)
.LBB9_14:                               # %sw.bb52
	movq	$39, %r15
	movq	$39, %rcx
	jmp	.LBB9_19
.LBB9_20:                               # %sw.bb82
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	token_spellings+8(%rsi), %rdx
	movabsq	$.L.str6, %rsi
	jmp	.LBB9_21
.LBB9_16:                               # %sw.default
	movq	$4, %rcx
	shlq	%cl, %rsi
	movq	token_spellings+8(%rsi), %rdx
	movabsq	$.L.str5, %rsi
.LBB9_21:                               # %return
	xorq	%rax, %rax
	callq	cpp_ice
	jmp	.LBB9_22
.LBB9_4:                                # %if.else
	testq	$16, %r8
	je	.LBB9_5
.LBB9_10:                               # %sw.bb24
	movq	8(%r14), %rax
	movq	8(%rax), %rsi
	movl	(%rax), %edx
	movq	%rbx, %rdi
	callq	memcpy
	movq	8(%r14), %rax
	movl	(%rax), %eax
	addq	%rax, %rbx
	jmp	.LBB9_22
.LBB9_5:                                # %if.else11
	movq	$4, %rcx
	shlq	%cl, %rsi
	leaq	token_spellings+8(%rsi), %rax
.LBB9_6:                                # %while.cond.preheader
	movq	(%rax), %rax
	movzbl	(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB9_22
# BB#7:                                 # %while.body.preheader
	incq	%rax
	.align	16, 0x90
.LBB9_8:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movb	%cl, (%rbx)
	movzbl	(%rax), %ecx
	incq	%rbx
	incq	%rax
	testq	%rcx, %rcx
	jne	.LBB9_8
	jmp	.LBB9_22
.LBB9_17:                               # %sw.bb53
	movq	$39, %r15
.LBB9_18:                               # %if.then62
	movq	$76, %rax
	movb	%al, (%rbx)
	incq	%rbx
	movq	%r15, %rcx
	jmp	.LBB9_19
.LBB9_15:                               # %sw.bb54
	movq	$60, %rcx
	movq	$62, %r15
.LBB9_19:                               # %if.end65
	movb	%cl, (%rbx)
	movq	16(%r14), %rsi
	movl	8(%r14), %edx
	leaq	1(%rbx), %rdi
	callq	memcpy
	movl	8(%r14), %eax
	leaq	2(%rax,%rbx), %rcx
	movb	%r15b, 1(%rax,%rbx)
	movq	%rcx, %rbx
.LBB9_22:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp126:
	.size	cpp_spell_token, .Ltmp126-cpp_spell_token
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI9_0:
	.quad	.LBB9_2
	.quad	.LBB9_9
	.quad	.LBB9_10
	.quad	.LBB9_11
	.quad	.LBB9_12
	.quad	.LBB9_20
.LJTI9_1:
	.quad	.LBB9_14
	.quad	.LBB9_17
	.quad	.LBB9_16
	.quad	.LBB9_19
	.quad	.LBB9_18
	.quad	.LBB9_15

	.text
	.globl	cpp_token_as_text
	.align	16, 0x90
	.type	cpp_token_as_text,@function
cpp_token_as_text:                      # @cpp_token_as_text
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp133:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp134:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp135:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp136:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 48
.Ltmp138:
	.cfi_offset %rbx, -48
.Ltmp139:
	.cfi_offset %r12, -40
.Ltmp140:
	.cfi_offset %r13, -32
.Ltmp141:
	.cfi_offset %r14, -24
.Ltmp142:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	6(%r14), %eax
	movq	$4, %rcx
	shlq	%cl, %rax
	movl	token_spellings(%rax), %eax
	movq	%rdi, %r15
	cmpq	$2, %rax
	je	.LBB10_3
# BB#1:                                 # %entry
	xorq	%rcx, %rcx
	addq	$-3, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	$1, %rdx
	ja	.LBB10_4
# BB#2:                                 # %sw.bb.i
	movl	8(%r14), %ecx
	jmp	.LBB10_4
.LBB10_3:                               # %sw.bb2.i
	movq	8(%r14), %rax
	movl	(%rax), %ecx
.LBB10_4:                               # %cpp_token_len.exit
	movq	80(%r15), %rax
	movq	16(%rax), %r12
	movq	24(%rax), %rdx
	addq	$5, %rcx
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rcx, %rbx
	subq	%r12, %rdx
	cmpq	%rbx, %rdx
	jae	.LBB10_15
# BB#5:                                 # %if.then.i
	movq	88(%r15), %rax
	testq	%rax, %rax
	je	.LBB10_10
# BB#6:                                 # %if.end.lr.ph.i.i
	leaq	88(%r15), %rdx
	leaq	(%rbx,%rbx,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB10_7:                               # %if.end.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rdi
	movq	8(%rax), %rsi
	movq	(%rax), %rcx
	subq	%rsi, %rdi
	cmpq	%rbx, %rdi
	jb	.LBB10_9
# BB#8:                                 # %if.end.i.i
                                        #   in Loop: Header=BB10_7 Depth=1
	cmpq	%r8, %rdi
	jbe	.LBB10_13
.LBB10_9:                               # %for.inc.i.i
                                        #   in Loop: Header=BB10_7 Depth=1
	movq	%rax, %rdx
	testq	%rcx, %rcx
	movq	%rcx, %rax
	jne	.LBB10_7
.LBB10_10:                              # %if.then.i.i
	cmpq	$8000, %rbx             # imm = 0x1F40
	movq	$8000, %r13             # imm = 0x1F40
	jb	.LBB10_12
# BB#11:                                # %if.then.i.i
	leaq	7(%rbx), %rax
	movabsq	$8589934584, %r13       # imm = 0x1FFFFFFF8
	andq	%rax, %r13
.LBB10_12:                              # %if.then.i.i
	leaq	32(%r13), %rdi
	callq	xmalloc
	movq	%rax, %r12
	leaq	(%r12,%r13), %rax
	movq	%r12, 8(%r12,%r13)
	movq	%r12, 16(%r12,%r13)
	movq	%rax, 24(%r12,%r13)
	movq	$0, (%r12,%r13)
	jmp	.LBB10_14
.LBB10_13:                              # %for.end.i.i
	movq	%rcx, (%rdx)
	movq	$0, (%rax)
	movq	8(%rax), %r12
	movq	%r12, 16(%rax)
.LBB10_14:                              # %_cpp_get_buff.exit.i
	movq	80(%r15), %rcx
	movq	%rcx, (%rax)
	movq	%rax, 80(%r15)
.LBB10_15:                              # %_cpp_unaligned_alloc.exit
	addq	%r12, %rbx
	movq	%rbx, 16(%rax)
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	cpp_spell_token
	xorq	%rcx, %rcx
	movb	%cl, (%rax)
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp143:
	.size	cpp_token_as_text, .Ltmp143-cpp_token_as_text
	.cfi_endproc

	.globl	_cpp_unaligned_alloc
	.align	16, 0x90
	.type	_cpp_unaligned_alloc,@function
_cpp_unaligned_alloc:                   # @_cpp_unaligned_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp148:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp149:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp150:
	.cfi_def_cfa_offset 32
.Ltmp151:
	.cfi_offset %rbx, -32
.Ltmp152:
	.cfi_offset %r14, -24
.Ltmp153:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	80(%r14), %rdx
	movq	16(%rdx), %rax
	movq	24(%rdx), %rcx
	movq	%rsi, %rbx
	subq	%rax, %rcx
	cmpq	%rbx, %rcx
	jae	.LBB11_11
# BB#1:                                 # %if.then
	movq	88(%r14), %rdx
	testq	%rdx, %rdx
	je	.LBB11_6
# BB#2:                                 # %if.end.lr.ph.i
	leaq	88(%r14), %rax
	leaq	(%rbx,%rbx,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB11_3:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdx), %rdi
	movq	8(%rdx), %rsi
	movq	(%rdx), %rcx
	subq	%rsi, %rdi
	cmpq	%rbx, %rdi
	jb	.LBB11_5
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB11_3 Depth=1
	cmpq	%r8, %rdi
	jbe	.LBB11_9
.LBB11_5:                               # %for.inc.i
                                        #   in Loop: Header=BB11_3 Depth=1
	movq	%rdx, %rax
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	jne	.LBB11_3
.LBB11_6:                               # %if.then.i
	cmpq	$8000, %rbx             # imm = 0x1F40
	movq	$8000, %r15             # imm = 0x1F40
	jb	.LBB11_8
# BB#7:                                 # %if.then.i
	leaq	7(%rbx), %r15
	andq	$-8, %r15
.LBB11_8:                               # %if.then.i
	leaq	32(%r15), %rdi
	callq	xmalloc
	leaq	(%rax,%r15), %rdx
	movq	%rax, 8(%rax,%r15)
	movq	%rax, 16(%rax,%r15)
	movq	%rdx, 24(%rax,%r15)
	movq	$0, (%rax,%r15)
	jmp	.LBB11_10
.LBB11_9:                               # %for.end.i
	movq	%rcx, (%rax)
	movq	$0, (%rdx)
	movq	8(%rdx), %rax
	movq	%rax, 16(%rdx)
.LBB11_10:                              # %_cpp_get_buff.exit
	movq	80(%r14), %rcx
	movq	%rcx, (%rdx)
	movq	%rdx, 80(%r14)
.LBB11_11:                              # %if.end
	addq	%rax, %rbx
	movq	%rbx, 16(%rdx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp154:
	.size	_cpp_unaligned_alloc, .Ltmp154-_cpp_unaligned_alloc
	.cfi_endproc

	.globl	cpp_type2name
	.align	16, 0x90
	.type	cpp_type2name,@function
cpp_type2name:                          # @cpp_type2name
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	token_spellings+8(%rax), %rax
	retq
.Ltmp155:
	.size	cpp_type2name, .Ltmp155-cpp_type2name
	.cfi_endproc

	.globl	cpp_output_token
	.align	16, 0x90
	.type	cpp_output_token,@function
cpp_output_token:                       # @cpp_output_token
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp161:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp162:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp163:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp165:
	.cfi_def_cfa_offset 48
.Ltmp166:
	.cfi_offset %rbx, -40
.Ltmp167:
	.cfi_offset %r12, -32
.Ltmp168:
	.cfi_offset %r14, -24
.Ltmp169:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movzbl	6(%r14), %edx
	movq	%rdx, %rax
	andq	$255, %rax
	movq	$4, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movl	token_spellings(%rdi), %ecx
	movq	%rsi, %r12
	cmpq	$4, %rcx
	ja	.LBB13_8
# BB#1:                                 # %entry
	jmpq	*.LJTI13_0(,%rcx,8)
.LBB13_2:                               # %sw.bb
	movq	$8, %rcx
	shrq	%cl, %rdx
	testq	$2, %rdx
	je	.LBB13_4
# BB#3:                                 # %if.then
	leaq	digraph_spellings-320(,%rax,8), %rax
	jmp	.LBB13_6
.LBB13_9:                               # %sw.bb21
	movzbl	8(%r14), %edi
	jmp	.LBB13_10
.LBB13_12:                              # %sw.bb32
	movq	16(%r14), %rdi
	movl	8(%r14), %edx
	jmp	.LBB13_13
.LBB13_14:                              # %sw.bb40
	movq	%rax, %rdx
	addq	$-58, %rdx
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rdx, %rcx
	cmpq	$5, %rcx
	ja	.LBB13_18
# BB#15:                                # %sw.bb40
	movq	$34, %r15
	movq	$34, %rdi
	jmpq	*.LJTI13_1(,%rcx,8)
.LBB13_16:                              # %sw.bb46
	movq	$39, %r15
	movq	$39, %rdi
	jmp	.LBB13_21
.LBB13_18:                              # %sw.default
	movq	stderr(%rip), %rdi
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	token_spellings+8(%rax), %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB13_4:                               # %if.else
	testq	$16, %rdx
	je	.LBB13_5
.LBB13_11:                              # %sw.bb25
	movq	8(%r14), %rax
	movq	8(%rax), %rdi
	movl	(%rax), %edx
.LBB13_13:                              # %sw.bb32
	movq	$1, %rsi
	movq	%r12, %rcx
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB13_5:                               # %if.else11
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	token_spellings+8(%rax), %rax
.LBB13_6:                               # %if.end17
	movq	(%rax), %rbx
	movzbl	(%rbx), %edi
	incq	%rbx
	.align	16, 0x90
.LBB13_7:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rdi
	movq	%r12, %rsi
	callq	putc
	movzbl	(%rbx), %edi
	incq	%rbx
	testq	%rdi, %rdi
	jne	.LBB13_7
.LBB13_8:                               # %sw.epilog70
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB13_19:                              # %sw.bb47
	movq	$39, %r15
.LBB13_20:                              # %if.then57
	movq	$76, %rdi
	movq	%r12, %rsi
	callq	putc
	movq	%r15, %rdi
	jmp	.LBB13_21
.LBB13_17:                              # %sw.bb48
	movq	$60, %rdi
	movq	$62, %r15
.LBB13_21:                              # %if.end59
	movq	%r12, %rsi
	callq	putc
	movq	16(%r14), %rdi
	movl	8(%r14), %edx
	movq	$1, %rsi
	movq	%r12, %rcx
	callq	fwrite
	movq	%r15, %rdi
.LBB13_10:                              # %sw.bb21
	movq	%r12, %rsi
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp170:
	.size	cpp_output_token, .Ltmp170-cpp_output_token
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI13_0:
	.quad	.LBB13_2
	.quad	.LBB13_9
	.quad	.LBB13_11
	.quad	.LBB13_12
	.quad	.LBB13_14
.LJTI13_1:
	.quad	.LBB13_16
	.quad	.LBB13_19
	.quad	.LBB13_18
	.quad	.LBB13_21
	.quad	.LBB13_20
	.quad	.LBB13_17

	.text
	.globl	_cpp_equiv_tokens
	.align	16, 0x90
	.type	_cpp_equiv_tokens,@function
_cpp_equiv_tokens:                      # @_cpp_equiv_tokens
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp174:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp175:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp176:
	.cfi_def_cfa_offset 32
.Ltmp177:
	.cfi_offset %rbx, -24
.Ltmp178:
	.cfi_offset %r14, -16
	movzbl	6(%rdi), %edx
	movzbl	6(%rsi), %ebx
	movq	%rdx, %r8
	andq	$255, %r8
	movq	%rbx, %rcx
	andq	$255, %rcx
	xorq	%rax, %rax
	cmpq	%rcx, %r8
	jne	.LBB14_15
# BB#1:                                 # %land.lhs.true
	movq	$8, %rcx
	shrq	%cl, %rbx
	movq	$8, %rcx
	shrq	%cl, %rdx
	cmpq	%rbx, %rdx
	jne	.LBB14_15
# BB#2:                                 # %if.then
	movq	$4, %rcx
	movq	%r8, %rax
	shlq	%cl, %rax
	movl	token_spellings(%rax), %ecx
	movq	$1, %rax
	decq	%rcx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %rcx
	cmpq	$4, %rcx
	ja	.LBB14_15
# BB#3:                                 # %if.then
	jmpq	*.LJTI14_0(,%rcx,8)
.LBB14_11:                              # %sw.bb35
	movl	8(%rdi), %edx
	movl	8(%rsi), %ecx
	xorq	%rax, %rax
	cmpq	%rcx, %rdx
	jne	.LBB14_15
# BB#12:                                # %land.rhs
	movq	16(%rdi), %rdi
	movq	16(%rsi), %rsi
	callq	memcmp
	testq	%r14, %rax
	je	.LBB14_14
# BB#13:
	movabsq	$0, %rax
	jmp	.LBB14_15
.LBB14_4:                               # %sw.bb11
	movzbl	8(%rdi), %eax
	movzbl	8(%rsi), %ecx
	cmpq	%rcx, %rax
	je	.LBB14_14
# BB#5:
	movabsq	$0, %rax
	jmp	.LBB14_15
.LBB14_9:                               # %sw.bb29
	movq	8(%rdi), %rax
	movq	8(%rsi), %rcx
	cmpq	%rcx, %rax
	je	.LBB14_14
# BB#10:
	movabsq	$0, %rax
	jmp	.LBB14_15
.LBB14_6:                               # %sw.bb18
	cmpq	$65, %r8
	movq	$1, %rax
	jne	.LBB14_15
# BB#7:                                 # %lor.rhs
	movl	8(%rdi), %eax
	movl	8(%rsi), %ecx
	cmpq	%rcx, %rax
	jne	.LBB14_8
.LBB14_14:                              # %land.rhs
	movabsq	$1, %rax
.LBB14_15:                              # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB14_8:
	movabsq	$0, %rax
	jmp	.LBB14_15
.Ltmp179:
	.size	_cpp_equiv_tokens, .Ltmp179-_cpp_equiv_tokens
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI14_0:
	.quad	.LBB14_4
	.quad	.LBB14_9
	.quad	.LBB14_11
	.quad	.LBB14_11
	.quad	.LBB14_6

	.text
	.globl	cpp_avoid_paste
	.align	16, 0x90
	.type	cpp_avoid_paste,@function
cpp_avoid_paste:                        # @cpp_avoid_paste
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	6(%rsi), %r10d
	movzbl	6(%rdx), %r8d
	testq	$4096, %r10             # imm = 0x1000
	movq	$56, %r9
	je	.LBB15_1
# BB#2:                                 # %entry
	movq	$56, %r10
	jmp	.LBB15_3
.LBB15_1:
	andq	$255, %r10
.LBB15_3:                               # %entry
	movq	$8, %rcx
	movq	%r8, %rax
	shrq	%cl, %rax
	testq	$16, %rax
	jne	.LBB15_5
# BB#4:
	andq	$255, %r8
	movq	%r8, %r9
.LBB15_5:                               # %entry
	testq	$2, %rax
	je	.LBB15_7
# BB#6:                                 # %if.then14
	leaq	-40(%r9), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	digraph_spellings(,%rax,8), %rax
	jmp	.LBB15_9
.LBB15_7:                               # %if.else
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	movq	%r9, %rax
	andq	%r8, %rax
	cmpq	$55, %rax
	ja	.LBB15_11
# BB#8:                                 # %if.then20
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r9, %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	leaq	token_spellings+8(%rax), %rax
.LBB15_9:                               # %if.end26
	movq	(%rax), %rax
	movzbl	(%rax), %r8d
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r10, %rax
	cmpq	$15, %rax
	ja	.LBB15_11
# BB#10:                                # %if.end26
	movq	$1, %rax
	cmpq	$61, %r8
	je	.LBB15_81
.LBB15_11:                              # %if.end32
	xorq	%rax, %rax
	movq	$32, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	cmpq	$19, %r11
	jg	.LBB15_14
# BB#12:                                # %if.end32
	addq	$-2, %r10
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r10
	cmpq	$8, %r10
	ja	.LBB15_81
# BB#13:                                # %if.end32
	jmpq	*.LJTI15_1(,%r10,8)
.LBB15_25:                              # %sw.bb
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	andq	%rax, %r8
	cmpq	$62, %r8
	jne	.LBB15_26
# BB#27:                                # %sw.bb
	movabsq	$1, %rax
	retq
.LBB15_14:                              # %if.end32
	movq	$32, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	cmpq	$39, %r11
	jg	.LBB15_18
# BB#15:                                # %if.end32
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r10
	cmpq	$20, %r10
	jne	.LBB15_81
# BB#16:                                # %sw.bb83
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	andq	%rax, %r8
	cmpq	$58, %r8
	je	.LBB15_47
# BB#17:
	movabsq	$0, %rax
	retq
.LBB15_18:                              # %if.end32
	movq	$32, %rcx
	movq	%r10, %r11
	shlq	%cl, %r11
	movq	$32, %rcx
	sarq	%cl, %r11
	cmpq	$49, %r11
	jg	.LBB15_23
# BB#19:                                # %if.end32
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r10
	cmpq	$40, %r10
	jne	.LBB15_81
# BB#20:                                # %sw.bb105
	cmpq	$35, %r8
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB15_22
# BB#21:                                # %sw.bb105
	movq	%rdx, %rax
.LBB15_22:                              # %sw.bb105
	cmpq	$37, %r8
	je	.LBB15_79
	jmp	.LBB15_80
.LBB15_23:                              # %if.end32
	addq	$-50, %r10
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %r10
	cmpq	$10, %r10
	ja	.LBB15_81
# BB#24:                                # %if.end32
	jmpq	*.LJTI15_0(,%r10,8)
.LBB15_48:                              # %sw.bb91
	cmpq	$42, %r8
	jne	.LBB15_49
# BB#50:                                # %sw.bb91
	movabsq	$1, %rax
	retq
.LBB15_28:                              # %sw.bb37
	addq	$-37, %r8
	andq	%r8, %rcx
	cmpq	$26, %rcx
	ja	.LBB15_81
# BB#29:                                # %switch.lookup
	andq	$134217727, %r8         # imm = 0x7FFFFFF
	movq	$77594625, %rax         # imm = 0x4A00001
	movq	%r8, %rcx
	shrq	%cl, %rax
	andq	$1, %rax
	retq
.LBB15_30:                              # %sw.bb50
	cmpq	$43, %r8
	jne	.LBB15_31
# BB#32:                                # %sw.bb50
	movabsq	$1, %rax
	retq
.LBB15_33:                              # %sw.bb53
	cmpq	$45, %r8
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB15_35
# BB#34:                                # %sw.bb53
	movq	%rdx, %rax
.LBB15_35:                              # %sw.bb53
	cmpq	$62, %r8
	je	.LBB15_79
	jmp	.LBB15_80
.LBB15_36:                              # %sw.bb61
	cmpq	$47, %r8
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB15_38
# BB#37:                                # %sw.bb61
	movq	%rdx, %rax
.LBB15_38:                              # %sw.bb61
	cmpq	$42, %r8
	je	.LBB15_79
	jmp	.LBB15_80
.LBB15_39:                              # %sw.bb69
	movabsq	$4294967291, %rax       # imm = 0xFFFFFFFB
	andq	%rax, %r8
	cmpq	$58, %r8
	jne	.LBB15_40
# BB#41:                                # %sw.bb69
	movabsq	$1, %rax
	retq
.LBB15_42:                              # %sw.bb77
	cmpq	$38, %r8
	jne	.LBB15_43
# BB#44:                                # %sw.bb77
	movabsq	$1, %rax
	retq
.LBB15_45:                              # %sw.bb80
	cmpq	$124, %r8
	jne	.LBB15_46
.LBB15_47:                              # %sw.bb83
	movabsq	$1, %rax
	retq
.LBB15_26:
	movabsq	$0, %rax
	retq
.LBB15_31:
	movabsq	$0, %rax
	retq
.LBB15_40:
	movabsq	$0, %rax
	retq
.LBB15_43:
	movabsq	$0, %rax
	retq
.LBB15_46:
	movabsq	$0, %rax
	retq
.LBB15_51:                              # %sw.bb94
	movq	$1, %rax
	cmpq	$37, %r8
	je	.LBB15_81
# BB#52:                                # %sw.bb94
	cmpq	$46, %r8
	je	.LBB15_81
# BB#53:                                # %lor.rhs100
	andq	%rcx, %r9
	cmpq	$57, %r9
	jne	.LBB15_54
# BB#55:                                # %lor.rhs100
	movabsq	$1, %rax
	retq
.LBB15_56:                              # %sw.bb113
	movq	%r9, %r10
	andq	%rcx, %r10
	cmpq	$57, %r10
	jne	.LBB15_63
# BB#57:                                # %land.lhs.true116
	movl	8(%rdx), %r8d
	testq	%r8, %r8
	movq	$1, %rax
	je	.LBB15_81
# BB#58:                                # %for.body.lr.ph.i
	movq	16(%rdx), %r9
	movq	$1, %rsi
.LBB15_59:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	-1(%r9,%rsi), %eax
	movzbl	_sch_istable(%rax,%rax), %edx
	testq	$516, %rdx              # imm = 0x204
	jne	.LBB15_62
# BB#60:                                # %lor.lhs.false.i
                                        #   in Loop: Header=BB15_59 Depth=1
	cmpq	$36, %rax
	jne	.LBB15_64
# BB#61:                                # %land.lhs.true.i
                                        #   in Loop: Header=BB15_59 Depth=1
	movzbl	767(%rdi), %eax
	testq	%rax, %rax
	je	.LBB15_64
.LBB15_62:                              # %for.inc.i
                                        #   in Loop: Header=BB15_59 Depth=1
	movq	%rsi, %rax
	andq	%rcx, %rax
	incq	%rsi
	cmpq	%r8, %rax
	movq	$1, %rax
	jb	.LBB15_59
	jmp	.LBB15_81
.LBB15_67:                              # %sw.bb129
	andq	$254, %r9
	cmpq	$56, %r9
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB15_69
# BB#68:                                # %sw.bb129
	movq	%rdx, %rax
.LBB15_69:                              # %sw.bb129
	cmpq	$46, %r8
	movq	%rcx, %rsi
	jne	.LBB15_71
# BB#70:                                # %sw.bb129
	movq	%rdx, %rsi
.LBB15_71:                              # %sw.bb129
	orq	%rsi, %rax
	cmpq	$43, %r8
	movq	%rcx, %rsi
	jne	.LBB15_73
# BB#72:                                # %sw.bb129
	movq	%rdx, %rsi
.LBB15_73:                              # %sw.bb129
	orq	%rsi, %rax
	cmpq	$45, %r8
	je	.LBB15_79
	jmp	.LBB15_80
.LBB15_74:                              # %sw.bb146
	movzbl	744(%rdi), %edx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB15_81
# BB#75:                                # %land.lhs.true149
	movzbl	8(%rsi), %edx
	cmpq	$64, %rdx
	jne	.LBB15_81
# BB#76:                                # %land.rhs
	andq	%rcx, %r9
	cmpq	$56, %r9
	movabsq	$0, %rcx
	movabsq	$1, %rdx
	movq	%rcx, %rax
	jne	.LBB15_78
# BB#77:                                # %land.rhs
	movq	%rdx, %rax
.LBB15_78:                              # %land.rhs
	cmpq	$61, %r9
	jne	.LBB15_80
.LBB15_79:                              # %land.rhs
	movq	%rdx, %rcx
.LBB15_80:                              # %land.rhs
	orq	%rcx, %rax
.LBB15_81:                              # %return
	retq
.LBB15_49:
	movabsq	$0, %rax
	retq
.LBB15_63:                              # %lor.lhs.false118
	andq	$253, %r9
	cmpq	$56, %r9
	movq	$1, %rax
	je	.LBB15_81
.LBB15_64:                              # %lor.rhs124
	cmpq	$61, %r10
	jne	.LBB15_65
# BB#66:                                # %lor.rhs124
	movabsq	$1, %rax
	retq
.LBB15_65:
	movabsq	$0, %rax
	retq
.LBB15_54:
	movabsq	$0, %rax
	retq
.Ltmp180:
	.size	cpp_avoid_paste, .Ltmp180-cpp_avoid_paste
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI15_0:
	.quad	.LBB15_48
	.quad	.LBB15_51
	.quad	.LBB15_81
	.quad	.LBB15_81
	.quad	.LBB15_81
	.quad	.LBB15_81
	.quad	.LBB15_56
	.quad	.LBB15_67
	.quad	.LBB15_81
	.quad	.LBB15_81
	.quad	.LBB15_74
.LJTI15_1:
	.quad	.LBB15_25
	.quad	.LBB15_28
	.quad	.LBB15_30
	.quad	.LBB15_33
	.quad	.LBB15_81
	.quad	.LBB15_36
	.quad	.LBB15_39
	.quad	.LBB15_42
	.quad	.LBB15_45

	.text
	.globl	cpp_output_line
	.align	16, 0x90
	.type	cpp_output_line,@function
cpp_output_line:                        # @cpp_output_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp185:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp186:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp187:
	.cfi_def_cfa_offset 32
.Ltmp188:
	.cfi_offset %rbx, -32
.Ltmp189:
	.cfi_offset %r14, -24
.Ltmp190:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
                                        # kill: RDI<def> R14<kill>
	callq	cpp_get_token
	movq	%rax, %rbx
	jmp	.LBB16_1
	.align	16, 0x90
.LBB16_3:                               # %if.then
                                        #   in Loop: Header=BB16_1 Depth=1
	movq	$32, %rdi
	movq	%r15, %rsi
	callq	putc
.LBB16_1:                               # %entry
                                        # =>This Inner Loop Header: Depth=1
	movzbl	6(%rbx), %eax
	cmpq	$67, %rax
	je	.LBB16_4
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB16_1 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	cpp_output_token
	movq	%r14, %rdi
	callq	cpp_get_token
	movq	%rax, %rbx
	movzbl	7(%rbx), %eax
	testq	$1, %rax
	je	.LBB16_1
	jmp	.LBB16_3
.LBB16_4:                               # %while.end
	movq	$10, %rdi
	movq	%r15, %rsi
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	putc  # TAILCALL
.Ltmp191:
	.size	cpp_output_line, .Ltmp191-cpp_output_line
	.cfi_endproc

	.globl	cpp_parse_escape
	.align	16, 0x90
	.type	cpp_parse_escape,@function
cpp_parse_escape:                       # @cpp_parse_escape
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp198:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp199:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp200:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp201:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp202:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp203:
	.cfi_def_cfa_offset 80
.Ltmp204:
	.cfi_offset %rbx, -48
.Ltmp205:
	.cfi_offset %r12, -40
.Ltmp206:
	.cfi_offset %r13, -32
.Ltmp207:
	.cfi_offset %r14, -24
.Ltmp208:
	.cfi_offset %r15, -16
	movq	(%rsi), %r13
	movzbl	(%r13), %r12d
	movq	%r8, %r15
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, %r14
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	incq	%r13
	cmpq	$84, %r12
	jg	.LBB17_9
# BB#1:                                 # %entry
	cmpq	$47, %r12
	jg	.LBB17_4
# BB#2:                                 # %entry
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	leaq	-34(%r12), %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$6, %rax
	ja	.LBB17_77
# BB#3:                                 # %entry
	jmpq	*.LJTI17_2(,%rax,8)
.LBB17_9:                               # %entry
	cmpq	$90, %r12
	jg	.LBB17_17
# BB#10:                                # %entry
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	cmpq	$85, %r12
	jne	.LBB17_77
	jmp	.LBB17_11
.LBB17_4:                               # %entry
	leaq	-48(%r12), %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%rdx, %rcx
	cmpq	$8, %rcx
	jae	.LBB17_5
# BB#22:                                # %while.cond57.preheader
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	cmpq	%r14, %r13
	jae	.LBB17_27
# BB#23:
	movq	$1, %rsi
	.align	16, 0x90
.LBB17_24:                              # %while.body62
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %ecx
	movq	%rcx, %rdi
	andq	$248, %rdi
	cmpq	$48, %rdi
	jne	.LBB17_27
# BB#25:                                # %if.end69
                                        #   in Loop: Header=BB17_24 Depth=1
	incq	%r13
	cmpq	%r14, %r13
	leaq	-48(%rcx,%rax,8), %rax
	jae	.LBB17_27
# BB#26:                                # %if.end69
                                        #   in Loop: Header=BB17_24 Depth=1
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rdi
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	$3, %rdi
	jl	.LBB17_24
.LBB17_27:                              # %while.end74
	andq	%rax, %rdx
	movq	%rdx, %r12
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %r12
	cmpq	%r12, %rdx
	jne	.LBB17_75
# BB#28:
	movq	%rax, %r12
	jmp	.LBB17_81
.LBB17_17:                              # %entry
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	cmpq	$109, %r12
	jg	.LBB17_20
# BB#18:                                # %entry
	leaq	-91(%r12), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rax
	cmpq	$11, %rax
	ja	.LBB17_77
# BB#19:                                # %entry
	jmpq	*.LJTI17_1(,%rax,8)
.LBB17_35:                              # %sw.bb9
	movq	24(%rsp), %rdi          # 8-byte Reload
	movzbl	761(%rdi), %eax
	testq	%rax, %rax
	je	.LBB17_37
# BB#36:                                # %if.then
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	callq	cpp_warning
.LBB17_37:                              # %if.end
	testq	%rbx, %r15
	movq	$7, %r12
	je	.LBB17_81
# BB#38:                                # %if.end
	movq	$97, %r12
	jmp	.LBB17_81
.LBB17_20:                              # %entry
	leaq	-110(%r12), %rax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$13, %rax
	ja	.LBB17_77
# BB#21:                                # %entry
	jmpq	*.LJTI17_0(,%rax,8)
.LBB17_31:                              # %sw.bb3
	movq	$10, %r12
	jmp	.LBB17_81
.LBB17_11:                              # %sw.bb20
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	742(%rax), %eax
	testq	%rax, %rax
	jne	.LBB17_13
# BB#12:                                # %lor.lhs.false.i
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	769(%rax), %eax
	testq	%rax, %rax
	je	.LBB17_77
.LBB17_13:                              # %if.end.i
	movq	24(%rsp), %r15          # 8-byte Reload
	movzbl	761(%r15), %eax
	testq	%rax, %rax
	je	.LBB17_15
# BB#14:                                # %if.then6.i
	movabsq	$.L.str20, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r12, %rdx
	callq	cpp_warning
.LBB17_15:                              # %if.end7.i
	cmpq	$117, %r12
	jne	.LBB17_16
# BB#40:                                # %if.end7.i
	movabsq	$1, %rax
	jmp	.LBB17_41
.LBB17_75:                              # %if.then80
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cpp_pedwarn
	jmp	.LBB17_81
.LBB17_76:                              # %sw.epilog
	movq	24(%rsp), %rax          # 8-byte Reload
	movzbl	770(%rax), %eax
	testq	%rax, %rax
	jne	.LBB17_77
	jmp	.LBB17_81
.LBB17_16:
	movabsq	$0, %rax
.LBB17_41:                              # %if.end7.i
	xorq	$1, %rax
	leaq	4(,%rax,4), %rbx
	movq	%r14, %rax
	subq	%r13, %rax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jae	.LBB17_42
# BB#84:                                # %if.end21.thread.i
	movabsq	$.L.str21, %rsi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	cpp_error
	movq	%rbx, %rax
	movq	%r14, %r13
	jmp	.LBB17_50
.LBB17_42:
	xorq	%r12, %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB17_43:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %edx
	movzbl	_sch_istable+1(%rdx,%rdx), %eax
	testq	$1, %rax
	je	.LBB17_46
# BB#44:                                # %if.then18.i
                                        #   in Loop: Header=BB17_43 Depth=1
	movzbl	_hex_value(%rdx), %eax
	cmpq	$99, %rax
	je	.LBB17_85
# BB#45:                                # %hex_digit_value.exit.i
                                        #   in Loop: Header=BB17_43 Depth=1
	movq	$4, %rcx
	shlq	%cl, %r12
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	addq	%rax, %r12
	decq	%rbx
	incq	%r13
	xorq	%rax, %rax
	testq	%r14, %rbx
	jne	.LBB17_43
	jmp	.LBB17_47
.LBB17_5:                               # %entry
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	cmpq	$63, %r12
	je	.LBB17_81
# BB#6:                                 # %entry
	cmpq	$69, %r12
	jne	.LBB17_77
.LBB17_7:                               # %sw.bb14
	movq	24(%rsp), %rdi          # 8-byte Reload
	movzbl	770(%rdi), %eax
	testq	%rax, %rax
	je	.LBB17_8
# BB#39:                                # %if.then18
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r12, %rdx
	callq	cpp_pedwarn
	movq	$27, %r12
	jmp	.LBB17_81
.LBB17_77:                              # %if.then86
	movzbl	_sch_istable(%r12,%r12), %eax
	testq	$172, %rax
	je	.LBB17_79
# BB#78:                                # %if.then93
	movabsq	$.L.str14, %rsi
	jmp	.LBB17_80
.LBB17_79:                              # %if.else
	movabsq	$.L.str15, %rsi
.LBB17_80:                              # %if.end95
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r12, %rdx
	callq	cpp_pedwarn
.LBB17_81:                              # %if.end95
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%r12, %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	jbe	.LBB17_83
# BB#82:                                # %if.then99
	movabsq	$.L.str16, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cpp_pedwarn
.LBB17_83:                              # %if.end100
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%r13, (%rax)
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB17_46:                              # %if.else19.i
	movabsq	$.L.str22, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	callq	cpp_error
	movq	%rbx, %rax
.LBB17_47:                              # %if.end21.i
	andq	%r12, %r14
	cmpq	$160, %r14
	jb	.LBB17_50
# BB#48:                                # %if.end21.i
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	js	.LBB17_50
# BB#49:                                # %if.end21.i
	movabsq	$4294965248, %rcx       # imm = 0xFFFFF800
	andq	%r12, %rcx
	cmpq	$55296, %rcx            # imm = 0xD800
	jne	.LBB17_81
.LBB17_50:                              # %if.else33.i
	movabsq	$4294967295, %r9        # imm = 0xFFFFFFFF
	movq	%r12, %rbx
	andq	%r9, %rbx
	cmpq	$36, %rbx
	movabsq	$0, %rcx
	movabsq	$1, %r8
	movq	%rcx, %rsi
	je	.LBB17_52
# BB#51:                                # %if.else33.i
	movq	%r8, %rsi
.LBB17_52:                              # %if.else33.i
	cmpq	$64, %rbx
	movq	%rcx, %rdx
	je	.LBB17_54
# BB#53:                                # %if.else33.i
	movq	%r8, %rdx
.LBB17_54:                              # %if.else33.i
	cmpq	$96, %rbx
	movq	24(%rsp), %rdi          # 8-byte Reload
	je	.LBB17_56
# BB#55:                                # %if.else33.i
	movq	%r8, %rcx
.LBB17_56:                              # %if.else33.i
	testq	%r9, %rax
	jne	.LBB17_81
# BB#57:                                # %if.else33.i
	andq	%rdx, %rsi
	andq	%rcx, %rsi
	je	.LBB17_81
# BB#58:                                # %if.then46.i
	movabsq	$.L.str23, %rsi
	xorq	%rax, %rax
	callq	cpp_error
	jmp	.LBB17_81
.LBB17_29:                              # %sw.bb1
	movq	$8, %r12
	jmp	.LBB17_81
.LBB17_30:                              # %sw.bb2
	movq	$12, %r12
	jmp	.LBB17_81
.LBB17_8:
	movq	$27, %r12
	jmp	.LBB17_81
.LBB17_32:                              # %sw.bb4
	movq	$13, %r12
	jmp	.LBB17_81
.LBB17_33:                              # %sw.bb5
	movq	$9, %r12
	jmp	.LBB17_81
.LBB17_34:                              # %sw.bb6
	movq	$11, %r12
	jmp	.LBB17_81
.LBB17_59:                              # %sw.bb21
	movq	24(%rsp), %rdi          # 8-byte Reload
	movzbl	761(%rdi), %eax
	testq	%rax, %rax
	je	.LBB17_61
# BB#60:                                # %if.then25
	movq	%rsi, %rbx
	movabsq	$.L.str10, %rsi
	xorq	%rax, %rax
	callq	cpp_warning
	movq	%rbx, %rsi
.LBB17_61:                              # %if.end26
	movq	$120, %r12
	testq	%rsi, %r15
	jne	.LBB17_81
# BB#62:                                # %while.cond.preheader
	xorq	%rax, %rax
	cmpq	%r14, %r13
	jae	.LBB17_63
# BB#64:
	xorq	%r15, %r15
	xorq	%r12, %r12
.LBB17_65:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%r13), %ecx
	movzbl	_sch_istable+1(%rcx,%rcx), %edx
	testq	$1, %rdx
	je	.LBB17_68
# BB#66:                                # %if.end35
                                        #   in Loop: Header=BB17_65 Depth=1
	movzbl	_hex_value(%rcx), %eax
	cmpq	$99, %rax
	je	.LBB17_85
# BB#67:                                # %hex_digit_value.exit
                                        #   in Loop: Header=BB17_65 Depth=1
	movq	%r12, %rcx
	andq	$-268435456, %rcx       # imm = 0xFFFFFFFFF0000000
	orq	%rcx, %r15
	movq	$4, %rcx
	shlq	%cl, %r12
	incq	%r13
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	addq	%rax, %r12
	cmpq	%r14, %r13
	movq	$1, %rax
	jb	.LBB17_65
	jmp	.LBB17_70
.LBB17_63:
	xorq	%r12, %r12
	xorq	%r15, %r15
	jmp	.LBB17_69
.LBB17_68:                              # %while.end
	testq	%rsi, %rax
	jne	.LBB17_70
.LBB17_69:                              # %if.then40
	movq	%rsi, %rbx
	movabsq	$.L.str11, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cpp_error
	movq	%rbx, %rsi
.LBB17_70:                              # %if.end41
	movq	%r12, %rax
	andq	%rsi, %rax
	movq	%rax, %rbx
	movq	16(%rsp), %rcx          # 8-byte Reload
	andq	%rcx, %rbx
	cmpq	%rbx, %rax
	je	.LBB17_71
# BB#72:                                # %if.end41
	movabsq	$1, %rax
	jmp	.LBB17_73
.LBB17_71:
	movabsq	$0, %rax
.LBB17_73:                              # %if.end41
	orq	%r15, %rax
	testq	%rsi, %rax
	je	.LBB17_81
# BB#74:                                # %if.then49
	movabsq	$.L.str12, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	cpp_pedwarn
	movq	%rbx, %r12
	jmp	.LBB17_81
.LBB17_85:                              # %if.else.i
	callq	abort
.Ltmp209:
	.size	cpp_parse_escape, .Ltmp209-cpp_parse_escape
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI17_0:
	.quad	.LBB17_31
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_32
	.quad	.LBB17_77
	.quad	.LBB17_33
	.quad	.LBB17_11
	.quad	.LBB17_34
	.quad	.LBB17_77
	.quad	.LBB17_59
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_76
.LJTI17_1:
	.quad	.LBB17_76
	.quad	.LBB17_81
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_35
	.quad	.LBB17_29
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_7
	.quad	.LBB17_30
.LJTI17_2:
	.quad	.LBB17_81
	.quad	.LBB17_77
	.quad	.LBB17_77
	.quad	.LBB17_76
	.quad	.LBB17_77
	.quad	.LBB17_81
	.quad	.LBB17_76

	.text
	.globl	cpp_interpret_charconst
	.align	16, 0x90
	.type	cpp_interpret_charconst,@function
cpp_interpret_charconst:                # @cpp_interpret_charconst
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp216:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp217:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp218:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp219:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp220:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp221:
	.cfi_def_cfa_offset 128
.Ltmp222:
	.cfi_offset %rbx, -48
.Ltmp223:
	.cfi_offset %r12, -40
.Ltmp224:
	.cfi_offset %r13, -32
.Ltmp225:
	.cfi_offset %r14, -24
.Ltmp226:
	.cfi_offset %r15, -16
	movq	16(%rsi), %rax
	movq	%rax, 72(%rsp)
	movl	8(%rsi), %r15d
	movzbl	6(%rsi), %esi
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	addq	%rax, %r15
	cmpq	$58, %rsi
	jne	.LBB18_1
# BB#2:                                 # %if.then
	movzbl	741(%rdi), %eax
	testq	%rax, %rax
	jne	.LBB18_3
# BB#4:                                 # %if.then
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movabsq	$1, %rax
	jmp	.LBB18_5
.LBB18_1:
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	jmp	.LBB18_6
.LBB18_3:
	movq	%rdi, 64(%rsp)          # 8-byte Spill
	movabsq	$0, %rax
.LBB18_5:                               # %if.then
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$8, %rcx
.LBB18_6:                               # %if.then8
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rcx, %rsi
	andq	%r13, %rsi
	movq	%rsi, 32(%rsp)          # 8-byte Spill
	movq	$1, %r12
	shlq	%cl, %r12
	decq	%r12
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	$64, %rax
	xorq	%rdx, %rdx
	divq	%rsi
	movq	%rax, %rbx
	xorq	%r14, %r14
	jmp	.LBB18_7
.LBB18_11:                              # %if.then23
                                        #   in Loop: Header=BB18_7 Depth=1
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	48(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	andq	%r12, %rax
	andq	%r13, %rax
	orq	%rax, %rdx
	movq	%rdx, 48(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB18_7:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	72(%rsp), %rax
	cmpq	%r15, %rax
	jae	.LBB18_12
# BB#8:                                 # %while.body
                                        #   in Loop: Header=BB18_7 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 72(%rsp)
	movzbl	(%rax), %eax
	cmpq	$92, %rax
	jne	.LBB18_10
# BB#9:                                 # %if.then16
                                        #   in Loop: Header=BB18_7 Depth=1
	movq	64(%rsp), %rdi          # 8-byte Reload
	leaq	72(%rsp), %rsi
	movq	%r15, %rdx
	movq	%r12, %rcx
	movq	56(%rsp), %r8           # 8-byte Reload
	callq	cpp_parse_escape
.LBB18_10:                              # %if.end17
                                        #   in Loop: Header=BB18_7 Depth=1
	incq	%r14
	movq	%r14, %rcx
	andq	%r13, %rcx
	movq	%rbx, %rdx
	andq	%r13, %rdx
	cmpq	%rdx, %rcx
	ja	.LBB18_7
	jmp	.LBB18_11
.LBB18_12:                              # %while.end
	testq	%r13, %r14
	je	.LBB18_13
# BB#14:                                # %if.else34
	movq	%rbx, %rcx
	andq	%r13, %rcx
	movq	%r14, %rax
	andq	%r13, %rax
	cmpq	%rcx, %rax
	jbe	.LBB18_16
# BB#15:                                # %if.then37
	movabsq	$.L.str18, %rsi
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	cpp_warning
	jmp	.LBB18_21
.LBB18_13:                              # %if.end46.thread
	movabsq	$.L.str17, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	cpp_error
	movq	48(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB18_27
.LBB18_16:                              # %if.else38
	movq	56(%rsp), %rcx          # 8-byte Reload
	testq	%r13, %rcx
	jne	.LBB18_20
# BB#17:                                # %if.else38
	cmpq	$2, %rax
	jb	.LBB18_20
# BB#18:                                # %if.else38
	movq	8(%rsp), %rax           # 8-byte Reload
	testq	%r13, %rax
	je	.LBB18_20
# BB#19:                                # %if.then43
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	movq	64(%rsp), %rdi          # 8-byte Reload
	callq	cpp_warning
.LBB18_20:                              # %if.end46
	movq	%r14, %rbx
.LBB18_21:                              # %if.end46
	movq	24(%rsp), %rdi          # 8-byte Reload
	testq	%r13, %rbx
	je	.LBB18_22
# BB#23:                                # %if.then48
	imulq	%rbx, %rdi
	movq	$64, %rcx
	subq	%rdi, %rcx
	andq	%r13, %rcx
	movq	$-1, %rax
	shrq	%cl, %rax
	cmpq	$0, 16(%rsp)            # 8-byte Folded Reload
	movq	48(%rsp), %rsi          # 8-byte Reload
	jne	.LBB18_25
# BB#24:                                # %lor.lhs.false
	decq	%rdi
	movq	$1, %rdx
	movq	%rdi, %rcx
	shlq	%cl, %rdx
	testq	%rsi, %rdx
	je	.LBB18_25
# BB#26:                                # %if.else61
	notq	%rax
	orq	%rax, %rsi
	jmp	.LBB18_27
.LBB18_25:                              # %if.then59
	andq	%rax, %rsi
	jmp	.LBB18_27
.LBB18_22:
	xorq	%rbx, %rbx
	movq	48(%rsp), %rsi          # 8-byte Reload
.LBB18_27:                              # %if.end64
	movq	40(%rsp), %rax          # 8-byte Reload
	movl	%ebx, (%rax)
	movq	%rsi, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp227:
	.size	cpp_interpret_charconst, .Ltmp227-cpp_interpret_charconst
	.cfi_endproc

	.globl	_cpp_release_buff
	.align	16, 0x90
	.type	_cpp_release_buff,@function
_cpp_release_buff:                      # @_cpp_release_buff
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rsi, %rcx
	.align	16, 0x90
.LBB19_1:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, %rax
	movq	(%rax), %rcx
	testq	%rcx, %rcx
	jne	.LBB19_1
# BB#2:                                 # %while.end
	movq	88(%rdi), %rcx
	movq	%rcx, (%rax)
	movq	%rsi, 88(%rdi)
	retq
.Ltmp228:
	.size	_cpp_release_buff, .Ltmp228-_cpp_release_buff
	.cfi_endproc

	.globl	_cpp_get_buff
	.align	16, 0x90
	.type	_cpp_get_buff,@function
_cpp_get_buff:                          # @_cpp_get_buff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp231:
	.cfi_def_cfa_offset 16
.Ltmp232:
	.cfi_offset %rbx, -16
	movq	88(%rdi), %rax
	testq	%rax, %rax
	je	.LBB20_5
# BB#1:                                 # %if.end.lr.ph
	addq	$88, %rdi
	leaq	(%rsi,%rsi,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB20_2:                               # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rax), %rbx
	movq	8(%rax), %rdx
	movq	(%rax), %rcx
	subq	%rdx, %rbx
	cmpq	%rsi, %rbx
	jb	.LBB20_4
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB20_2 Depth=1
	cmpq	%r8, %rbx
	jbe	.LBB20_8
.LBB20_4:                               # %for.inc
                                        #   in Loop: Header=BB20_2 Depth=1
	movq	%rax, %rdi
	testq	%rcx, %rcx
	movq	%rcx, %rax
	jne	.LBB20_2
.LBB20_5:                               # %if.then
	cmpq	$8000, %rsi             # imm = 0x1F40
	movq	$8000, %rbx             # imm = 0x1F40
	jb	.LBB20_7
# BB#6:                                 # %if.then
	addq	$7, %rsi
	andq	$-8, %rsi
	movq	%rsi, %rbx
.LBB20_7:                               # %if.then
	leaq	32(%rbx), %rdi
	callq	xmalloc
	movq	%rax, %rcx
	leaq	(%rcx,%rbx), %rax
	movq	%rcx, 8(%rcx,%rbx)
	movq	%rcx, 16(%rcx,%rbx)
	movq	%rax, 24(%rcx,%rbx)
	movq	$0, (%rcx,%rbx)
	popq	%rbx
	retq
.LBB20_8:                               # %for.end
	movq	%rcx, (%rdi)
	movq	$0, (%rax)
	movq	8(%rax), %rcx
	movq	%rcx, 16(%rax)
	popq	%rbx
	retq
.Ltmp233:
	.size	_cpp_get_buff, .Ltmp233-_cpp_get_buff
	.cfi_endproc

	.globl	_cpp_append_extend_buff
	.align	16, 0x90
	.type	_cpp_append_extend_buff,@function
_cpp_append_extend_buff:                # @_cpp_append_extend_buff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp238:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp239:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp240:
	.cfi_def_cfa_offset 32
.Ltmp241:
	.cfi_offset %rbx, -32
.Ltmp242:
	.cfi_offset %r14, -24
.Ltmp243:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	16(%r14), %rax
	movq	24(%r14), %rcx
	movq	88(%rdi), %rbx
	subq	%rax, %rcx
	leaq	(%rdx,%rcx,2), %rax
	testq	%rbx, %rbx
	je	.LBB21_5
# BB#1:                                 # %if.end.lr.ph.i
	addq	$88, %rdi
	leaq	(%rax,%rax,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB21_2:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movq	8(%rbx), %rdx
	movq	(%rbx), %rcx
	subq	%rdx, %rsi
	cmpq	%rax, %rsi
	jb	.LBB21_4
# BB#3:                                 # %if.end.i
                                        #   in Loop: Header=BB21_2 Depth=1
	cmpq	%r8, %rsi
	jbe	.LBB21_8
.LBB21_4:                               # %for.inc.i
                                        #   in Loop: Header=BB21_2 Depth=1
	movq	%rbx, %rdi
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.LBB21_2
.LBB21_5:                               # %if.then.i
	cmpq	$8000, %rax             # imm = 0x1F40
	movq	$8000, %r15             # imm = 0x1F40
	jb	.LBB21_7
# BB#6:                                 # %if.then.i
	addq	$7, %rax
	andq	$-8, %rax
	movq	%rax, %r15
.LBB21_7:                               # %if.then.i
	leaq	32(%r15), %rdi
	callq	xmalloc
	movq	%rax, %rdi
	leaq	(%rdi,%r15), %rbx
	movq	%rdi, 8(%rdi,%r15)
	movq	%rdi, 16(%rdi,%r15)
	movq	%rbx, 24(%rdi,%r15)
	movq	$0, (%rdi,%r15)
	jmp	.LBB21_9
.LBB21_8:                               # %for.end.i
	movq	%rcx, (%rdi)
	movq	$0, (%rbx)
	movq	8(%rbx), %rdi
	movq	%rdi, 16(%rbx)
.LBB21_9:                               # %_cpp_get_buff.exit
	movq	%rbx, (%r14)
	movq	16(%r14), %rsi
	movq	24(%r14), %rdx
	subq	%rsi, %rdx
	callq	memcpy
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp244:
	.size	_cpp_append_extend_buff, .Ltmp244-_cpp_append_extend_buff
	.cfi_endproc

	.globl	_cpp_extend_buff
	.align	16, 0x90
	.type	_cpp_extend_buff,@function
_cpp_extend_buff:                       # @_cpp_extend_buff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp250:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp251:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp252:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp253:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp254:
	.cfi_def_cfa_offset 48
.Ltmp255:
	.cfi_offset %rbx, -40
.Ltmp256:
	.cfi_offset %r12, -32
.Ltmp257:
	.cfi_offset %r14, -24
.Ltmp258:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	(%r14), %r15
	movq	16(%r15), %rax
	movq	24(%r15), %rcx
	movq	88(%rdi), %rbx
	subq	%rax, %rcx
	leaq	(%rdx,%rcx,2), %rax
	testq	%rbx, %rbx
	je	.LBB22_5
# BB#1:                                 # %if.end.lr.ph.i
	addq	$88, %rdi
	leaq	(%rax,%rax,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB22_2:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rbx), %rsi
	movq	8(%rbx), %rdx
	movq	(%rbx), %rcx
	subq	%rdx, %rsi
	cmpq	%rax, %rsi
	jb	.LBB22_4
# BB#3:                                 # %if.end.i
                                        #   in Loop: Header=BB22_2 Depth=1
	cmpq	%r8, %rsi
	jbe	.LBB22_8
.LBB22_4:                               # %for.inc.i
                                        #   in Loop: Header=BB22_2 Depth=1
	movq	%rbx, %rdi
	testq	%rcx, %rcx
	movq	%rcx, %rbx
	jne	.LBB22_2
.LBB22_5:                               # %if.then.i
	cmpq	$8000, %rax             # imm = 0x1F40
	movq	$8000, %r12             # imm = 0x1F40
	jb	.LBB22_7
# BB#6:                                 # %if.then.i
	addq	$7, %rax
	andq	$-8, %rax
	movq	%rax, %r12
.LBB22_7:                               # %if.then.i
	leaq	32(%r12), %rdi
	callq	xmalloc
	movq	%rax, %rdi
	leaq	(%rdi,%r12), %rbx
	movq	%rdi, 8(%rdi,%r12)
	movq	%rdi, 16(%rdi,%r12)
	movq	%rbx, 24(%rdi,%r12)
	movq	$0, (%rdi,%r12)
	jmp	.LBB22_9
.LBB22_8:                               # %for.end.i
	movq	%rcx, (%rdi)
	movq	$0, (%rbx)
	movq	8(%rbx), %rdi
	movq	%rdi, 16(%rbx)
.LBB22_9:                               # %_cpp_get_buff.exit
	movq	16(%r15), %rsi
	movq	24(%r15), %rdx
	subq	%rsi, %rdx
	callq	memcpy
	movq	%r15, (%rbx)
	movq	%rbx, (%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp259:
	.size	_cpp_extend_buff, .Ltmp259-_cpp_extend_buff
	.cfi_endproc

	.globl	_cpp_free_buff
	.align	16, 0x90
	.type	_cpp_free_buff,@function
_cpp_free_buff:                         # @_cpp_free_buff
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp262:
	.cfi_def_cfa_offset 16
.Ltmp263:
	.cfi_offset %rbx, -16
	testq	%rdi, %rdi
	je	.LBB23_2
	.align	16, 0x90
.LBB23_1:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rdi), %rbx
	movq	8(%rdi), %rdi
	callq	free
	testq	%rbx, %rbx
	movq	%rbx, %rdi
	jne	.LBB23_1
.LBB23_2:                               # %for.end
	popq	%rbx
	retq
.Ltmp264:
	.size	_cpp_free_buff, .Ltmp264-_cpp_free_buff
	.cfi_endproc

	.globl	_cpp_aligned_alloc
	.align	16, 0x90
	.type	_cpp_aligned_alloc,@function
_cpp_aligned_alloc:                     # @_cpp_aligned_alloc
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp269:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp270:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp271:
	.cfi_def_cfa_offset 32
.Ltmp272:
	.cfi_offset %rbx, -32
.Ltmp273:
	.cfi_offset %r14, -24
.Ltmp274:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	72(%r14), %rdx
	movq	16(%rdx), %rax
	movq	24(%rdx), %rcx
	movq	%rsi, %rbx
	subq	%rax, %rcx
	cmpq	%rbx, %rcx
	jae	.LBB24_11
# BB#1:                                 # %if.then
	movq	88(%r14), %rdx
	testq	%rdx, %rdx
	je	.LBB24_6
# BB#2:                                 # %if.end.lr.ph.i
	leaq	88(%r14), %rax
	leaq	(%rbx,%rbx,2), %r8
	movq	$1, %rcx
	shrq	%cl, %r8
	addq	$8000, %r8              # imm = 0x1F40
	.align	16, 0x90
.LBB24_3:                               # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%rdx), %rdi
	movq	8(%rdx), %rsi
	movq	(%rdx), %rcx
	subq	%rsi, %rdi
	cmpq	%rbx, %rdi
	jb	.LBB24_5
# BB#4:                                 # %if.end.i
                                        #   in Loop: Header=BB24_3 Depth=1
	cmpq	%r8, %rdi
	jbe	.LBB24_9
.LBB24_5:                               # %for.inc.i
                                        #   in Loop: Header=BB24_3 Depth=1
	movq	%rdx, %rax
	testq	%rcx, %rcx
	movq	%rcx, %rdx
	jne	.LBB24_3
.LBB24_6:                               # %if.then.i
	cmpq	$8000, %rbx             # imm = 0x1F40
	movq	$8000, %r15             # imm = 0x1F40
	jb	.LBB24_8
# BB#7:                                 # %if.then.i
	leaq	7(%rbx), %r15
	andq	$-8, %r15
.LBB24_8:                               # %if.then.i
	leaq	32(%r15), %rdi
	callq	xmalloc
	leaq	(%rax,%r15), %rdx
	movq	%rax, 8(%rax,%r15)
	movq	%rax, 16(%rax,%r15)
	movq	%rdx, 24(%rax,%r15)
	movq	$0, (%rax,%r15)
	jmp	.LBB24_10
.LBB24_9:                               # %for.end.i
	movq	%rcx, (%rax)
	movq	$0, (%rdx)
	movq	8(%rdx), %rax
	movq	%rax, 16(%rdx)
.LBB24_10:                              # %_cpp_get_buff.exit
	movq	72(%r14), %rcx
	movq	%rcx, (%rdx)
	movq	%rdx, 72(%r14)
.LBB24_11:                              # %if.end
	addq	%rax, %rbx
	movq	%rbx, 16(%rdx)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp275:
	.size	_cpp_aligned_alloc, .Ltmp275-_cpp_aligned_alloc
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"no newline at end of file"
	.size	.L.str, 26

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"unterminated comment"
	.size	.L.str1, 21

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"C++ style comments are not allowed in ISO C89"
	.size	.L.str2, 46

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"(this will be reported only once per input file)"
	.size	.L.str3, 49

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"multi-line comment"
	.size	.L.str4, 19

	.type	token_spellings,@object # @token_spellings
	.section	.rodata,"a",@progbits
	.align	16
token_spellings:
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str30
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str31
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str32
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str33
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str34
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str35
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str36
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str37
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str38
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str39
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str40
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str41
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str42
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str43
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str44
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str45
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str46
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str47
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str48
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str49
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str50
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str51
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str52
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str53
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str54
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str55
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str56
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str57
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str58
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str59
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str60
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str61
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str62
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str63
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str64
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str65
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str66
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str67
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str68
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str69
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str70
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str71
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str72
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str73
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str74
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str75
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str76
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str77
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str78
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str79
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str80
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str81
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str82
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str83
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str84
	.long	0                       # 0x0
	.zero	4
	.quad	.L.str85
	.long	2                       # 0x2
	.zero	4
	.quad	.L.str86
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str87
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str88
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str89
	.long	1                       # 0x1
	.zero	4
	.quad	.L.str90
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str91
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str92
	.long	4                       # 0x4
	.zero	4
	.quad	.L.str93
	.long	3                       # 0x3
	.zero	4
	.quad	.L.str94
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str95
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str96
	.long	5                       # 0x5
	.zero	4
	.quad	.L.str97
	.size	token_spellings, 1088

	.type	digraph_spellings,@object # @digraph_spellings
	.align	16
digraph_spellings:
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.size	digraph_spellings, 48

	.type	.L.str5,@object         # @.str5
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str5:
	.asciz	"unknown string token %s\n"
	.size	.L.str5, 25

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"unspellable token %s"
	.size	.L.str6, 21

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"impossible STRING token %s\n"
	.size	.L.str7, 28

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"the meaning of '\\a' varies with -traditional"
	.size	.L.str8, 45

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"non-ISO-standard escape sequence, '\\%c'"
	.size	.L.str9, 40

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"the meaning of '\\x' varies with -traditional"
	.size	.L.str10, 45

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"\\x used with no following hex digits"
	.size	.L.str11, 37

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"hex escape sequence out of range"
	.size	.L.str12, 33

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"octal escape sequence out of range"
	.size	.L.str13, 35

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"unknown escape sequence '\\%c'"
	.size	.L.str14, 30

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"unknown escape sequence: '\\%03o'"
	.size	.L.str15, 33

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"escape sequence out of range for character"
	.size	.L.str16, 43

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"empty character constant"
	.size	.L.str17, 25

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"character constant too long"
	.size	.L.str18, 28

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"multi-character character constant"
	.size	.L.str19, 35

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"the meaning of '\\%c' varies with -traditional"
	.size	.L.str20, 46

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"incomplete universal-character-name"
	.size	.L.str21, 36

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"non-hex digit '%c' in universal-character-name"
	.size	.L.str22, 47

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"universal-character-name out of range"
	.size	.L.str23, 38

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"%:"
	.size	.L.str24, 3

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"%:%:"
	.size	.L.str25, 5

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"<:"
	.size	.L.str26, 3

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	":>"
	.size	.L.str27, 3

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"<%"
	.size	.L.str28, 3

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"%>"
	.size	.L.str29, 3

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"="
	.size	.L.str30, 2

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"!"
	.size	.L.str31, 2

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	">"
	.size	.L.str32, 2

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"<"
	.size	.L.str33, 2

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"+"
	.size	.L.str34, 2

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"-"
	.size	.L.str35, 2

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"*"
	.size	.L.str36, 2

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"/"
	.size	.L.str37, 2

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"%"
	.size	.L.str38, 2

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"&"
	.size	.L.str39, 2

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"|"
	.size	.L.str40, 2

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"^"
	.size	.L.str41, 2

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	">>"
	.size	.L.str42, 3

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"<<"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"<?"
	.size	.L.str44, 3

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	">?"
	.size	.L.str45, 3

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"~"
	.size	.L.str46, 2

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"&&"
	.size	.L.str47, 3

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"||"
	.size	.L.str48, 3

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"?"
	.size	.L.str49, 2

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	":"
	.size	.L.str50, 2

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	","
	.size	.L.str51, 2

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"("
	.size	.L.str52, 2

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	")"
	.size	.L.str53, 2

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"=="
	.size	.L.str54, 3

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"!="
	.size	.L.str55, 3

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	">="
	.size	.L.str56, 3

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"<="
	.size	.L.str57, 3

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"+="
	.size	.L.str58, 3

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"-="
	.size	.L.str59, 3

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"*="
	.size	.L.str60, 3

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"/="
	.size	.L.str61, 3

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"%="
	.size	.L.str62, 3

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"&="
	.size	.L.str63, 3

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"|="
	.size	.L.str64, 3

	.type	.L.str65,@object        # @.str65
.L.str65:
	.asciz	"^="
	.size	.L.str65, 3

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	">>="
	.size	.L.str66, 4

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"<<="
	.size	.L.str67, 4

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"<?="
	.size	.L.str68, 4

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	">?="
	.size	.L.str69, 4

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"#"
	.size	.L.str70, 2

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"##"
	.size	.L.str71, 3

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"["
	.size	.L.str72, 2

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"]"
	.size	.L.str73, 2

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"{"
	.size	.L.str74, 2

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"}"
	.size	.L.str75, 2

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	";"
	.size	.L.str76, 2

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"..."
	.size	.L.str77, 4

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"++"
	.size	.L.str78, 3

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"--"
	.size	.L.str79, 3

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"->"
	.size	.L.str80, 3

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"."
	.size	.L.str81, 2

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"::"
	.size	.L.str82, 3

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"->*"
	.size	.L.str83, 4

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	".*"
	.size	.L.str84, 3

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"@"
	.size	.L.str85, 2

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"CPP_NAME"
	.size	.L.str86, 9

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"CPP_NUMBER"
	.size	.L.str87, 11

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"CPP_CHAR"
	.size	.L.str88, 9

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"CPP_WCHAR"
	.size	.L.str89, 10

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"CPP_OTHER"
	.size	.L.str90, 10

	.type	.L.str91,@object        # @.str91
.L.str91:
	.asciz	"CPP_STRING"
	.size	.L.str91, 11

	.type	.L.str92,@object        # @.str92
.L.str92:
	.asciz	"CPP_WSTRING"
	.size	.L.str92, 12

	.type	.L.str93,@object        # @.str93
.L.str93:
	.asciz	"CPP_HEADER_NAME"
	.size	.L.str93, 16

	.type	.L.str94,@object        # @.str94
.L.str94:
	.asciz	"CPP_COMMENT"
	.size	.L.str94, 12

	.type	.L.str95,@object        # @.str95
.L.str95:
	.asciz	"CPP_MACRO_ARG"
	.size	.L.str95, 14

	.type	.L.str96,@object        # @.str96
.L.str96:
	.asciz	"CPP_PADDING"
	.size	.L.str96, 12

	.type	.L.str97,@object        # @.str97
.L.str97:
	.asciz	"CPP_EOF"
	.size	.L.str97, 8

	.type	.L.str98,@object        # @.str98
.L.str98:
	.asciz	"\"/*\" within comment"
	.size	.L.str98, 20

	.type	.L.str99,@object        # @.str99
.L.str99:
	.asciz	"attempt to use poisoned \"%s\""
	.size	.L.str99, 29

	.type	.L.str100,@object       # @.str100
.L.str100:
	.asciz	"__VA_ARGS__ can only appear in the expansion of a C99 variadic macro"
	.size	.L.str100, 69

	.type	.L.str101,@object       # @.str101
.L.str101:
	.asciz	"'$' character(s) in identifier"
	.size	.L.str101, 31

	.type	.L.str102,@object       # @.str102
.L.str102:
	.asciz	"multi-line string literals are deprecated"
	.size	.L.str102, 42

	.type	.L.str103,@object       # @.str103
.L.str103:
	.asciz	"null character(s) preserved in literal"
	.size	.L.str103, 39

	.type	.L.str104,@object       # @.str104
.L.str104:
	.asciz	"missing terminating %c character"
	.size	.L.str104, 33

	.type	.L.str105,@object       # @.str105
.L.str105:
	.asciz	"possible start of unterminated string literal"
	.size	.L.str105, 46

	.type	.L.str106,@object       # @.str106
.L.str106:
	.asciz	"backslash and newline separated by space"
	.size	.L.str106, 41

	.type	.L.str107,@object       # @.str107
.L.str107:
	.asciz	"backslash-newline at end of file"
	.size	.L.str107, 33

	.type	.L.str108,@object       # @.str108
.L.str108:
	.asciz	"trigraph ??%c converted to %c"
	.size	.L.str108, 30

	.type	.L.str109,@object       # @.str109
.L.str109:
	.asciz	"trigraph ??%c ignored"
	.size	.L.str109, 22

	.type	.L.str110,@object       # @.str110
.L.str110:
	.asciz	"null character(s) ignored"
	.size	.L.str110, 26

	.type	.L.str111,@object       # @.str111
.L.str111:
	.asciz	"%s in preprocessing directive"
	.size	.L.str111, 30

	.type	.L.str112,@object       # @.str112
.L.str112:
	.asciz	"form feed"
	.size	.L.str112, 10

	.type	.L.str113,@object       # @.str113
.L.str113:
	.asciz	"vertical tab"
	.size	.L.str113, 13


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
