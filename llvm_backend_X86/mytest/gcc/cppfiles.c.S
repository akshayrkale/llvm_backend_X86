	.file	"cppfiles.c.bc"
	.text
	.globl	_cpp_init_includes
	.align	16, 0x90
	.type	_cpp_init_includes,@function
_cpp_init_includes:                     # @_cpp_init_includes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$strcmp, %rdi
	movabsq	$free, %rsi
	movabsq	$destroy_node, %rdx
	callq	splay_tree_new
	movq	%rax, 280(%rbx)
	popq	%rbx
	retq
.Ltmp4:
	.size	_cpp_init_includes, .Ltmp4-_cpp_init_includes
	.cfi_endproc

	.align	16, 0x90
	.type	destroy_node,@function
destroy_node:                           # @destroy_node
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 16
.Ltmp8:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	testq	%rbx, %rbx
	je	.LBB1_4
# BB#1:                                 # %if.then
	movq	24(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB1_3
# BB#2:                                 # %if.then.i
	callq	free
	movq	$0, 24(%rbx)
.LBB1_3:                                # %purge_cache.exit
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.LBB1_4:                                # %if.end
	popq	%rbx
	retq
.Ltmp9:
	.size	destroy_node, .Ltmp9-destroy_node
	.cfi_endproc

	.globl	_cpp_cleanup_includes
	.align	16, 0x90
	.type	_cpp_cleanup_includes,@function
_cpp_cleanup_includes:                  # @_cpp_cleanup_includes
	.cfi_startproc
# BB#0:                                 # %entry
	movq	280(%rdi), %rdi
	jmp	splay_tree_delete  # TAILCALL
.Ltmp10:
	.size	_cpp_cleanup_includes, .Ltmp10-_cpp_cleanup_includes
	.cfi_endproc

	.globl	_cpp_never_reread
	.align	16, 0x90
	.type	_cpp_never_reread,@function
_cpp_never_reread:                      # @_cpp_never_reread
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$-1, 8(%rdi)
	retq
.Ltmp11:
	.size	_cpp_never_reread, .Ltmp11-_cpp_never_reread
	.cfi_endproc

	.globl	_cpp_fake_include
	.align	16, 0x90
	.type	_cpp_fake_include,@function
_cpp_fake_include:                      # @_cpp_fake_include
	.cfi_startproc
# BB#0:                                 # %entry
	jmp	find_or_create_entry  # TAILCALL
.Ltmp12:
	.size	_cpp_fake_include, .Ltmp12-_cpp_fake_include
	.cfi_endproc

	.align	16, 0x90
	.type	find_or_create_entry,@function
find_or_create_entry:                   # @find_or_create_entry
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp17:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 32
.Ltmp20:
	.cfi_offset %rbx, -32
.Ltmp21:
	.cfi_offset %r14, -24
.Ltmp22:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%rsi, %rdi
	callq	xstrdup
	movq	%rax, %r15
	movq	%r15, %rdi
	callq	_cpp_simplify_pathname
	movq	280(%r14), %rdi
	movq	%r15, %rsi
	callq	splay_tree_lookup
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB5_2
# BB#1:                                 # %if.end
	movq	%r15, %rdi
	callq	free
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB5_2:                                # %if.else
	movq	$1, %rdi
	movq	$192, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	%r15, (%rbx)
	callq	__errno_location
	movl	(%rax), %eax
	movl	%eax, 180(%rbx)
	movq	280(%r14), %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	splay_tree_insert  # TAILCALL
.Ltmp23:
	.size	find_or_create_entry, .Ltmp23-find_or_create_entry
	.cfi_endproc

	.globl	cpp_included
	.align	16, 0x90
	.type	cpp_included,@function
cpp_included:                           # @cpp_included
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp27:
	.cfi_def_cfa_offset 16
.Ltmp28:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp29:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp30:
	.cfi_offset %rbx, -56
.Ltmp31:
	.cfi_offset %r12, -48
.Ltmp32:
	.cfi_offset %r13, -40
.Ltmp33:
	.cfi_offset %r14, -32
.Ltmp34:
	.cfi_offset %r15, -24
	movq	%rsi, %r14
	movzbl	(%r14), %eax
	movq	%rdi, %r15
	cmpq	$36, %rax
	je	.LBB6_2
# BB#1:                                 # %entry
	cmpq	$47, %rax
	jne	.LBB6_6
.LBB6_2:                                # %if.then
	movq	280(%r15), %rdi
	movq	%r14, %rsi
	callq	splay_tree_lookup
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB6_14
# BB#3:                                 # %land.rhs
	cmpq	$0, 8(%rcx)
	je	.LBB6_4
# BB#5:                                 # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB6_14
.LBB6_6:                                # %if.end
	movq	%r14, %rdi
	callq	strlen
	movl	288(%r15), %ecx
	leaq	17(%rax,%rcx), %rax
	andq	$-16, %rax
	movq	%rsp, %r12
	subq	%rax, %r12
	movq	%r12, %rsp
	movq	688(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB6_13
# BB#7:                                 # %for.body.lr.ph
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_8:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rsi
	movl	16(%rbx), %edx
	movq	%r12, %rdi
	callq	memcpy
	movl	16(%rbx), %eax
	movq	$47, %rcx
	movb	%cl, (%r12,%rax)
	movl	16(%rbx), %edi
	incq	%rdi
	andq	%r13, %rdi
	addq	%r12, %rdi
	movq	%r14, %rsi
	callq	strcpy
	movzbl	764(%r15), %eax
	testq	%rax, %rax
	movq	%r12, %rsi
	je	.LBB6_10
# BB#9:                                 # %if.then22
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	remap_filename
	movq	%rax, %rsi
.LBB6_10:                               # %if.end24
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	280(%r15), %rdi
	callq	splay_tree_lookup
	testq	%rax, %rax
	je	.LBB6_12
# BB#11:                                # %land.lhs.true
                                        #   in Loop: Header=BB6_8 Depth=1
	cmpq	$0, 8(%rax)
	movq	$1, %rax
	jne	.LBB6_14
.LBB6_12:                               # %for.inc
                                        #   in Loop: Header=BB6_8 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_8
.LBB6_13:
	xorq	%rax, %rax
	jmp	.LBB6_14
.LBB6_4:
	movabsq	$0, %rax
.LBB6_14:                               # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp35:
	.size	cpp_included, .Ltmp35-cpp_included
	.cfi_endproc

	.align	16, 0x90
	.type	remap_filename,@function
remap_filename:                         # @remap_filename
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp39:
	.cfi_def_cfa_offset 16
.Ltmp40:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp41:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp42:
	.cfi_offset %rbx, -56
.Ltmp43:
	.cfi_offset %r12, -48
.Ltmp44:
	.cfi_offset %r13, -40
.Ltmp45:
	.cfi_offset %r14, -32
.Ltmp46:
	.cfi_offset %r15, -24
	movq	%rdx, %r12
	movq	48(%r12), %rbx
	movq	%rsi, %r15
	movq	%rdi, %r14
	testq	%rbx, %rbx
	jne	.LBB7_2
# BB#1:                                 # %if.then
	movl	16(%r12), %r13d
	leaq	1(%r13), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	addq	$15, %rcx
	andq	$-16, %rcx
	movq	%rsp, %rbx
	subq	%rcx, %rbx
	movq	%rbx, %rsp
	movq	8(%r12), %rsi
	movq	%rbx, %rdi
	movq	%r13, %rdx
	callq	memcpy
	xorq	%rax, %rax
	movb	%al, (%r13,%rbx)
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	read_name_map
	movq	%rax, %rbx
	testq	%rbx, %rbx
	movq	%rbx, 48(%r12)
	je	.LBB7_14
.LBB7_2:                                # %for.body.lr.ph
	movq	%r14, -48(%rbp)         # 8-byte Spill
	movl	16(%r12), %eax
	leaq	1(%rax,%r15), %r12
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB7_4:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r12, %rsi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB7_5
# BB#3:                                 # %for.cond
                                        #   in Loop: Header=BB7_4 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_4
# BB#6:                                 # %for.end
	movq	$47, %rsi
	movq	%r15, %rdi
	callq	strrchr
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB7_14
# BB#7:                                 # %if.end21
	movq	%r13, %r12
	subq	%r15, %r12
	jne	.LBB7_9
# BB#8:                                 # %if.then23
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	-48(%rbp), %rdi         # 8-byte Reload
	callq	cpp_ice
.LBB7_9:                                # %if.end24
	leaq	16(%r12), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	memcpy
	xorq	%rax, %rax
	movb	%al, (%r12,%rbx)
	movq	-48(%rbp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	read_name_map
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB7_14
# BB#10:
	incq	%r13
	.align	16, 0x90
.LBB7_11:                               # %for.body37
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	movq	%r13, %rsi
	callq	strcmp
	testq	%r14, %rax
	je	.LBB7_12
# BB#13:                                # %for.inc44
                                        #   in Loop: Header=BB7_11 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_11
	jmp	.LBB7_14
.LBB7_5:                                # %if.then16
	movq	16(%rbx), %r15
.LBB7_14:                               # %return
	movq	%r15, %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB7_12:                               # %if.then41
	movq	16(%rbx), %r15
	jmp	.LBB7_14
.Ltmp47:
	.size	remap_filename, .Ltmp47-remap_filename
	.cfi_endproc

	.globl	cpp_make_system_header
	.align	16, 0x90
	.type	cpp_make_system_header,@function
cpp_make_system_header:                 # @cpp_make_system_header
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%r8, %r8
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %rsi
	je	.LBB8_5
# BB#1:                                 # %if.then
	testq	%rax, %rdx
	je	.LBB8_2
# BB#3:                                 # %if.then
	movabsq	$1, %r8
	jmp	.LBB8_4
.LBB8_2:
	movabsq	$0, %r8
.LBB8_4:                                # %if.then
	incq	%r8
.LBB8_5:                                # %if.end
	movq	56(%rdi), %rax
	movl	64(%rdi), %r9d
	movl	8(%rax), %ecx
	movl	12(%rax), %esi
	movq	(%rax), %rdx
	addq	%r9, %rcx
	subq	%rsi, %rcx
	movq	$2, %rsi
	jmp	_cpp_do_file_change  # TAILCALL
.Ltmp48:
	.size	cpp_make_system_header, .Ltmp48-cpp_make_system_header
	.cfi_endproc

	.globl	_cpp_report_missing_guards
	.align	16, 0x90
	.type	_cpp_report_missing_guards,@function
_cpp_report_missing_guards:             # @_cpp_report_missing_guards
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp50:
	.cfi_def_cfa_offset 16
	xorq	%rax, %rax
	movl	%eax, 4(%rsp)
	movq	280(%rdi), %rdi
	movabsq	$report_missing_guard, %rsi
	leaq	4(%rsp), %rdx
	callq	splay_tree_foreach
	popq	%rax
	retq
.Ltmp51:
	.size	_cpp_report_missing_guards, .Ltmp51-_cpp_report_missing_guards
	.cfi_endproc

	.align	16, 0x90
	.type	report_missing_guard,@function
report_missing_guard:                   # @report_missing_guard
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp57:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp58:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp59:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp60:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp61:
	.cfi_def_cfa_offset 48
.Ltmp62:
	.cfi_offset %rbx, -40
.Ltmp63:
	.cfi_offset %r12, -32
.Ltmp64:
	.cfi_offset %r14, -24
.Ltmp65:
	.cfi_offset %r15, -16
	movq	8(%rdi), %rbx
	movq	%rsi, %r15
	testq	%rbx, %rbx
	je	.LBB10_6
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 8(%rbx)
	jne	.LBB10_6
# BB#2:                                 # %land.lhs.true1
	movzbl	184(%rbx), %eax
	cmpq	$1, %rax
	je	.LBB10_3
.LBB10_6:                               # %if.end9
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB10_3:                               # %if.then
	movl	(%r15), %eax
	movq	stderr(%rip), %r14
	testq	%rax, %rax
	jne	.LBB10_5
# BB#4:                                 # %if.then6
	movabsq	$.L.str11, %rdi
	movq	$43, %rsi
	movq	$1, %rdx
	movq	$1, %r12
	movq	%r14, %rcx
	callq	fwrite
	movl	%r12d, (%r15)
.LBB10_5:                               # %if.end
	movq	(%rbx), %rdi
	movq	%r14, %rsi
	callq	fputs
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB10_6
.Ltmp66:
	.size	report_missing_guard, .Ltmp66-report_missing_guard
	.cfi_endproc

	.globl	_cpp_execute_include
	.align	16, 0x90
	.type	_cpp_execute_include,@function
_cpp_execute_include:                   # @_cpp_execute_include
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp73:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp75:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp76:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp77:
	.cfi_def_cfa_offset 48
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
	movq	%rdx, %r12
	movq	%rsi, %r13
	movq	%rdi, %r15
                                        # kill: RDI<def> R15<kill>
                                        # kill: RSI<def> R13<kill>
                                        # kill: RDX<def> R12<kill>
	callq	find_include_file
	movq	%rax, %rbx
	xorq	%r14, %r14
	cmpq	$-1, %rbx
	je	.LBB11_8
# BB#1:                                 # %entry
	testq	%rbx, %rbx
	jne	.LBB11_6
# BB#2:                                 # %if.then
	movzbl	6(%r13), %eax
	movq	16(%r13), %rsi
	cmpq	$63, %rax
	jne	.LBB11_3
# BB#4:                                 # %if.then
	movabsq	$1, %rdx
	jmp	.LBB11_5
.LBB11_6:                               # %if.then5
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	stack_include_file
	movq	%rax, %r14
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r12
	cmpq	$2, %r12
	jne	.LBB11_8
# BB#7:                                 # %if.then9
	movq	$-1, 8(%rbx)
	jmp	.LBB11_8
.LBB11_3:
	movabsq	$0, %rdx
.LBB11_5:                               # %if.then
	movq	%r15, %rdi
	callq	handle_missing_header
.LBB11_8:                               # %if.end11
	movq	%r14, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp83:
	.size	_cpp_execute_include, .Ltmp83-_cpp_execute_include
	.cfi_endproc

	.align	16, 0x90
	.type	find_include_file,@function
find_include_file:                      # @find_include_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp87:
	.cfi_def_cfa_offset 16
.Ltmp88:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp89:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp90:
	.cfi_offset %rbx, -56
.Ltmp91:
	.cfi_offset %r12, -48
.Ltmp92:
	.cfi_offset %r13, -40
.Ltmp93:
	.cfi_offset %r14, -32
.Ltmp94:
	.cfi_offset %r15, -24
	movq	16(%rsi), %r14
	movzbl	(%r14), %eax
	movq	%rdi, %r15
	cmpq	$36, %rax
	je	.LBB12_2
# BB#1:                                 # %entry
	cmpq	$47, %rax
	jne	.LBB12_3
.LBB12_2:                               # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	open_file
.LBB12_32:                              # %return
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB12_3:                               # %if.end
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %rdx
	cmpq	$1, %rdx
	jne	.LBB12_5
# BB#4:                                 # %land.lhs.true
	movq	(%r15), %rax
	movq	48(%rax), %rax
	movq	16(%rax), %rax
	testq	%rax, %rax
	jne	.LBB12_10
.LBB12_5:                               # %if.else
	movzbl	6(%rsi), %eax
	cmpq	$63, %rax
	jne	.LBB12_7
# BB#6:                                 # %if.then15
	leaq	696(%r15), %rax
	jmp	.LBB12_10
.LBB12_7:                               # %if.else16
	movq	(%r15), %r13
	cmpq	$3, %rdx
	je	.LBB12_18
# BB#8:                                 # %if.end.i
	movzbl	766(%r15), %eax
	testq	%rax, %rax
	je	.LBB12_12
# BB#9:                                 # %if.then2.i
	leaq	688(%r15), %rax
.LBB12_10:                              # %if.end19
	movq	(%rax), %r13
	testq	%r13, %r13
	jne	.LBB12_23
# BB#11:                                # %if.then22
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r14, %rdx
	callq	cpp_error
	movq	$-1, %rax
	jmp	.LBB12_32
.LBB12_12:                              # %if.end4.i
	movzbl	82(%r13), %eax
	testq	%rax, %rax
	jne	.LBB12_22
# BB#13:                                # %if.then6.i
	movq	$1, %rax
	movb	%al, 82(%r13)
	movq	48(%r13), %rax
	movq	(%rax), %rdi
	callq	lbasename
	movq	48(%r13), %rcx
	movq	(%rcx), %rcx
	subq	%rcx, %rax
	testq	%rbx, %rax
	je	.LBB12_18
# BB#14:                                # %if.then11.i
	movq	%rax, %rdx
	andq	%rbx, %rdx
	cmpq	$1, %rdx
	movq	%rcx, 96(%r13)
	jbe	.LBB12_15
# BB#16:                                # %if.then11.i
	movabsq	$1, %rcx
	subq	%rcx, %rax
	jmp	.LBB12_19
.LBB12_18:                              # %use_cwd.i
	movq	$.L.str10, 96(%r13)
	movq	$1, %rax
.LBB12_19:                              # %if.end21.i
	movl	288(%r15), %ecx
	andq	%rax, %rbx
	cmpq	%rcx, %rbx
	jbe	.LBB12_21
# BB#20:                                # %if.then24.i
	movl	%eax, 288(%r15)
.LBB12_21:                              # %if.end26.i
	movl	%eax, 104(%r13)
	movq	688(%r15), %rax
	movq	%rax, 88(%r13)
	movq	56(%r15), %rax
	movzbl	21(%rax), %eax
	movl	%eax, 128(%r13)
.LBB12_22:                              # %if.end19.thread
	addq	$88, %r13
.LBB12_23:                              # %if.end23
	movq	%r14, %rdi
	callq	strlen
	movl	288(%r15), %ecx
	leaq	17(%rax,%rcx), %rax
	andq	$-16, %rax
	movq	%rsp, %rbx
	subq	%rax, %rbx
	movq	%rbx, %rsp
	xorq	%rax, %rax
	testq	%r13, %r13
	je	.LBB12_32
	.align	16, 0x90
.LBB12_24:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movslq	16(%r13), %r12
	movq	8(%r13), %rsi
	movq	%rbx, %rdi
	movq	%r12, %rdx
	callq	memcpy
	leaq	-1(%r12), %rdi
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movzbl	(%rbx,%rax), %eax
	cmpq	$47, %rax
	je	.LBB12_26
# BB#25:                                # %select.mid
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	%r12, %rdi
.LBB12_26:                              # %select.end
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	$32, %rcx
	movq	%rdi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$47, %rcx
	movb	%cl, (%rbx,%rax)
	incq	%rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	addq	%rbx, %rdi
	movq	%r14, %rsi
	callq	strcpy
	movzbl	764(%r15), %eax
	testq	%rax, %rax
	movq	%rbx, %rsi
	je	.LBB12_28
# BB#27:                                # %if.then45
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r13, %rdx
	callq	remap_filename
	movq	%rax, %rsi
.LBB12_28:                              # %if.end48
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	%r15, %rdi
	callq	open_file
	testq	%rax, %rax
	jne	.LBB12_29
# BB#30:                                # %for.inc
                                        #   in Loop: Header=BB12_24 Depth=1
	movq	(%r13), %r13
	testq	%r13, %r13
	jne	.LBB12_24
# BB#31:
	xorq	%rax, %rax
	jmp	.LBB12_32
.LBB12_29:                              # %if.then51
	movq	%r13, 16(%rax)
	jmp	.LBB12_32
.LBB12_15:
	movabsq	$0, %rcx
	subq	%rcx, %rax
	jmp	.LBB12_19
.Ltmp95:
	.size	find_include_file, .Ltmp95-find_include_file
	.cfi_endproc

	.align	16, 0x90
	.type	handle_missing_header,@function
handle_missing_header:                  # @handle_missing_header
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp99:
	.cfi_def_cfa_offset 16
.Ltmp100:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp101:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	pushq	%rax
.Ltmp102:
	.cfi_offset %rbx, -56
.Ltmp103:
	.cfi_offset %r12, -48
.Ltmp104:
	.cfi_offset %r13, -40
.Ltmp105:
	.cfi_offset %r14, -32
.Ltmp106:
	.cfi_offset %r15, -24
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movzbl	749(%rbx), %eax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	$1, %rsi
	jne	.LBB13_4
# BB#1:                                 # %lor.rhs
	movq	56(%rbx), %rsi
	movzbl	21(%rsi), %esi
	testq	%rsi, %rsi
	je	.LBB13_2
# BB#3:                                 # %lor.rhs
	movabsq	$1, %rsi
	jmp	.LBB13_4
.LBB13_2:
	movabsq	$0, %rsi
.LBB13_4:                               # %lor.end
	movzbl	751(%rbx), %edi
	testq	%rdi, %rdi
	je	.LBB13_15
# BB#5:                                 # %lor.end
	cmpq	%rsi, %rax
	jbe	.LBB13_15
# BB#6:                                 # %if.then
	testq	%rcx, %rdx
	je	.LBB13_9
# BB#7:                                 # %lor.lhs.false
	movzbl	(%r14), %eax
	cmpq	$36, %rax
	je	.LBB13_9
# BB#8:                                 # %lor.lhs.false
	cmpq	$47, %rax
	jne	.LBB13_10
.LBB13_9:                               # %if.then17
	movq	392(%rbx), %rdi
	movq	%r14, %rsi
	callq	deps_add_dep
	jmp	.LBB13_19
.LBB13_15:                              # %if.else34
	testq	%rax, %rax
	je	.LBB13_18
# BB#16:                                # %if.else34
	cmpq	%rsi, %rax
	ja	.LBB13_18
# BB#17:                                # %if.then41
	callq	__errno_location
	movl	(%rax), %edi
	callq	xstrerror
	movq	%rax, %rcx
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	cpp_warning
	jmp	.LBB13_19
.LBB13_18:                              # %if.else44
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	cpp_error_from_errno
	jmp	.LBB13_19
.LBB13_10:                              # %if.else
	movq	696(%rbx), %r15
	movq	%r14, %rdi
	callq	strlen
	movq	%r15, %rsi
	testq	%rsi, %rsi
	je	.LBB13_11
# BB#12:                                # %if.end
	movl	16(%rsi), %r13d
	movq	$32, %rcx
	movq	%r13, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	2(%rax,%r13), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$15, %rdx
	andq	$-16, %rdx
	movq	%rsp, %r12
	subq	%rdx, %r12
	movq	%r12, %rsp
	xorq	%rdi, %rdi
	testq	%r13, %r13
	je	.LBB13_14
# BB#13:                                # %if.then26
	movq	%rax, -48(%rbp)         # 8-byte Spill
	movq	8(%rsi), %rsi
	movq	%r12, %rdi
	movq	%r15, %rdx
	callq	memcpy
	movq	-48(%rbp), %rax         # 8-byte Reload
	incq	%r13
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	$47, %rcx
	movb	%cl, (%r12,%r15)
	movq	%r13, %rdi
	jmp	.LBB13_14
.LBB13_11:                              # %if.end.thread
	movq	$32, %rcx
	movq	%rax, %rsi
	shlq	%cl, %rsi
	movabsq	$8589934592, %rdx       # imm = 0x200000000
	addq	%rsi, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	addq	$15, %rdx
	andq	$-16, %rdx
	movq	%rsp, %r12
	subq	%rdx, %r12
	movq	%r12, %rsp
	xorq	%rdi, %rdi
.LBB13_14:                              # %if.end29
	addq	%r12, %rdi
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$4294967296, %rdx       # imm = 0x100000000
	addq	%rax, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r14, %rsi
	callq	memcpy
	movq	392(%rbx), %rdi
	movq	%r12, %rsi
	callq	deps_add_dep
.LBB13_19:                              # %if.end46
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp107:
	.size	handle_missing_header, .Ltmp107-handle_missing_header
	.cfi_endproc

	.align	16, 0x90
	.type	stack_include_file,@function
stack_include_file:                     # @stack_include_file
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
	subq	$16, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 64
.Ltmp120:
	.cfi_offset %rbx, -48
.Ltmp121:
	.cfi_offset %r12, -40
.Ltmp122:
	.cfi_offset %r13, -32
.Ltmp123:
	.cfi_offset %r14, -24
.Ltmp124:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movq	8(%r13), %rax
	movq	%rdi, %r14
	xorq	%rbx, %rbx
	cmpq	$-1, %rax
	je	.LBB14_48
# BB#1:                                 # %entry
	testq	%rax, %rax
	je	.LBB14_3
# BB#2:                                 # %lor.lhs.false
	movzbl	20(%rax), %eax
	cmpq	$1, %rax
	je	.LBB14_48
.LBB14_3:                               # %if.end
	movq	56(%r14), %rax
	xorq	%rsi, %rsi
	testq	%rax, %rax
	movabsq	$0, %rdi
	je	.LBB14_5
# BB#4:                                 # %cond.true
	movzbl	21(%rax), %edi
.LBB14_5:                               # %cond.end
	movq	16(%r13), %rdx
	testq	%rdx, %rdx
	je	.LBB14_7
# BB#6:                                 # %cond.true8
	movl	40(%rdx), %esi
.LBB14_7:                               # %cond.end12
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	xorq	%r15, %r15
	cmpq	%rsi, %rdi
	jle	.LBB14_10
# BB#8:                                 # %cond.true16
	testq	%rax, %rax
	je	.LBB14_12
# BB#9:                                 # %cond.true19
	movzbl	21(%rax), %r15d
	jmp	.LBB14_12
.LBB14_10:                              # %cond.false26
	testq	%rdx, %rdx
	je	.LBB14_12
# BB#11:                                # %cond.true29
	movl	40(%rdx), %r15d
.LBB14_12:                              # %cond.end35
	movzbl	749(%r14), %eax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jle	.LBB14_15
# BB#13:                                # %land.lhs.true40
	movzbl	184(%r13), %eax
	testq	%rax, %rax
	jne	.LBB14_15
# BB#14:                                # %if.then42
	movq	392(%r14), %rdi
	movq	(%r13), %rsi
	callq	deps_add_dep
.LBB14_15:                              # %if.end43
	cmpq	$0, 24(%r13)
	jne	.LBB14_31
# BB#16:                                # %if.then45
	movl	56(%r13), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$24576, %rax            # imm = 0x6000
	jne	.LBB14_17
# BB#36:                                # %if.then24.i
	movq	(%r13), %rdx
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_error
	jmp	.LBB14_47
.LBB14_17:                              # %if.then45
	movq	%r15, 8(%rsp)           # 8-byte Spill
	cmpq	$32768, %rax            # imm = 0x8000
	jne	.LBB14_37
# BB#18:                                # %if.then.i
	movq	80(%r13), %r15
	xorq	%rax, %rax
	movb	%al, 188(%r13)
	leaq	1(%r15), %rdi
	callq	xmalloc
	movq	%rax, %r12
	testq	%r15, %r15
	jle	.LBB14_19
# BB#22:                                # %while.body.lr.ph.i
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB14_23:                              # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movl	176(%r13), %edi
	leaq	(%r12,%rbx), %rsi
	movq	%r15, %rdx
	subq	%rbx, %rdx
	callq	read
	testq	%rax, %rax
	js	.LBB14_46
# BB#24:                                # %if.end10.i
                                        #   in Loop: Header=BB14_23 Depth=1
	testq	%rax, %rax
	je	.LBB14_25
# BB#20:                                # %while.cond.i
                                        #   in Loop: Header=BB14_23 Depth=1
	addq	%rax, %rbx
	cmpq	%rbx, %r15
	jg	.LBB14_23
# BB#21:
	movq	%r15, %rbx
	jmp	.LBB14_26
.LBB14_37:                              # %if.else26.i
	movq	$8193, %rdi             # imm = 0x2001
	callq	xmalloc
	movq	%rax, %r12
	xorq	%r15, %r15
	movq	$8192, %rax             # imm = 0x2000
	jmp	.LBB14_38
.LBB14_41:                              # %if.then38.i
                                        #   in Loop: Header=BB14_38 Depth=1
	leaq	(%r15,%r15), %rbx
	leaq	1(%r15,%r15), %rsi
	movq	%r12, %rdi
	callq	xrealloc
	movq	%rax, %r12
	movq	%rbx, %rax
.LBB14_38:                              # %while.cond29.outer.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB14_39 Depth 2
	movq	%r15, %rbx
	movq	%rax, %r15
	.align	16, 0x90
.LBB14_39:                              # %while.cond29.i
                                        #   Parent Loop BB14_38 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	176(%r13), %edi
	leaq	(%r12,%rbx), %rsi
	movq	%r15, %rdx
	subq	%rbx, %rdx
	callq	read
	testq	%rax, %rax
	jle	.LBB14_42
# BB#40:                                # %while.body35.i
                                        #   in Loop: Header=BB14_39 Depth=2
	addq	%rax, %rbx
	cmpq	%r15, %rbx
	jne	.LBB14_39
	jmp	.LBB14_41
.LBB14_42:                              # %while.end42.i
	testq	%rax, %rax
	js	.LBB14_46
# BB#43:                                # %if.end45.i
	leaq	1(%rbx), %rsi
	cmpq	%r15, %rsi
	jge	.LBB14_45
# BB#44:                                # %if.then48.i
	movq	%r12, %rdi
	callq	xrealloc
	movq	%rax, %r12
.LBB14_45:                              # %if.end51.i
	xorq	%rax, %rax
	movb	%al, (%r12,%rbx)
	movq	%rbx, 80(%r13)
	jmp	.LBB14_27
.LBB14_46:                              # %perror_fail.i
	movq	(%r13), %rsi
	movq	%r14, %rdi
	callq	cpp_error_from_errno
.LBB14_47:                              # %if.then47
	movq	$-1, 8(%r13)
	xorq	%rbx, %rbx
	jmp	.LBB14_48
.LBB14_19:
	movq	%r15, %rbx
	jmp	.LBB14_26
.LBB14_25:                              # %if.then12.i
	movq	(%r13), %rdx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	cpp_warning
	leaq	1(%rbx), %rsi
	movq	%r12, %rdi
	callq	xrealloc
	movq	%rax, %r12
	movq	%rbx, 80(%r13)
.LBB14_26:                              # %while.end.i
	xorq	%rax, %rax
	movb	%al, (%r12,%rbx)
.LBB14_27:                              # %if.end48
	movq	%r12, 24(%r13)
	movl	56(%r13), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$32768, %rax            # imm = 0x8000
	jne	.LBB14_30
# BB#28:                                # %land.lhs.true51
	cmpq	$0, 80(%r13)
	jne	.LBB14_30
# BB#29:                                # %if.then55
	movq	$-1, 8(%r13)
.LBB14_30:                              # %if.end56
	movl	176(%r13), %edi
	callq	close
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 176(%r13)
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB14_31:                              # %if.end59
	cmpq	$0, (%r14)
	je	.LBB14_33
# BB#32:                                # %if.then62
	movzbl	184(%r13), %eax
	incq	%rax
	movw	%ax, 184(%r13)
.LBB14_33:                              # %if.end65
	movq	24(%r13), %rsi
	movq	80(%r13), %rdx
	movzbl	771(%r14), %ecx
	xorq	%r8, %r8
	movq	%r14, %rdi
	callq	cpp_push_buffer
	movq	%r13, 48(%rax)
	movzbl	186(%r13), %eax
	incq	%rax
	movw	%ax, 186(%r13)
	movq	$1, %rbx
	movb	%bl, 184(%r14)
	movq	$0, 168(%r14)
	movq	(%r13), %rdx
	movzbl	(%rdx), %eax
	testq	%rax, %rax
	jne	.LBB14_35
# BB#34:
	movabsq	$.L.str4, %rdx
.LBB14_35:                              # %select.end
	xorq	%rsi, %rsi
	movq	$1, %rcx
	movq	%r14, %rdi
	movq	%r15, %r8
	callq	_cpp_do_file_change
.LBB14_48:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp125:
	.size	stack_include_file, .Ltmp125-stack_include_file
	.cfi_endproc

	.globl	_cpp_compare_file_date
	.align	16, 0x90
	.type	_cpp_compare_file_date,@function
_cpp_compare_file_date:                 # @_cpp_compare_file_date
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp129:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp130:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp131:
	.cfi_def_cfa_offset 32
.Ltmp132:
	.cfi_offset %rbx, -24
.Ltmp133:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	xorq	%rdx, %rdx
                                        # kill: RDI<def> R14<kill>
	callq	find_include_file
	movq	%rax, %rbx
	leaq	1(%rbx), %rax
	cmpq	$2, %rax
	jae	.LBB15_2
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB15_7
.LBB15_2:                               # %if.end
	movslq	176(%rbx), %rdi
	testq	%rdi, %rdi
	jle	.LBB15_4
# BB#3:                                 # %if.then3
	callq	close
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 176(%rbx)
.LBB15_4:                               # %if.end7
	movq	(%r14), %rax
	movq	48(%rax), %rax
	movq	120(%rbx), %rcx
	movq	120(%rax), %rax
	cmpq	%rax, %rcx
	jle	.LBB15_5
# BB#6:                                 # %if.end7
	movabsq	$1, %rax
	jmp	.LBB15_7
.LBB15_5:
	movabsq	$0, %rax
.LBB15_7:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp134:
	.size	_cpp_compare_file_date, .Ltmp134-_cpp_compare_file_date
	.cfi_endproc

	.globl	_cpp_read_file
	.align	16, 0x90
	.type	_cpp_read_file,@function
_cpp_read_file:                         # @_cpp_read_file
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp138:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp139:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp140:
	.cfi_def_cfa_offset 32
.Ltmp141:
	.cfi_offset %rbx, -24
.Ltmp142:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
                                        # kill: RDI<def> RBX<kill>
                                        # kill: RSI<def> R14<kill>
	callq	open_file
	movq	%rbx, %rdi
	testq	%rax, %rax
	je	.LBB16_1
# BB#2:                                 # %if.end
	movq	%rax, %rsi
	callq	stack_include_file
	jmp	.LBB16_3
.LBB16_1:                               # %if.then
	movq	%r14, %rsi
	callq	cpp_error_from_errno
	xorq	%rax, %rax
.LBB16_3:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp143:
	.size	_cpp_read_file, .Ltmp143-_cpp_read_file
	.cfi_endproc

	.align	16, 0x90
	.type	open_file,@function
open_file:                              # @open_file
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
	movq	%rsi, %r14
                                        # kill: RSI<def> R14<kill>
	callq	find_or_create_entry
	movq	8(%rax), %r15
	movl	180(%r15), %ebx
	testq	%rbx, %rbx
	je	.LBB17_2
# BB#1:                                 # %if.then
	callq	__errno_location
	movl	%ebx, (%rax)
.LBB17_13:                              # %return
	xorq	%r15, %r15
	jmp	.LBB17_14
.LBB17_2:                               # %if.end
	movq	8(%r15), %rax
	cmpq	$-1, %rax
	je	.LBB17_14
# BB#3:                                 # %if.end
	testq	%rax, %rax
	je	.LBB17_5
# BB#4:                                 # %lor.lhs.false
	movzbl	20(%rax), %eax
	cmpq	$1, %rax
	je	.LBB17_14
.LBB17_5:                               # %if.end8
	cmpq	$0, 24(%r15)
	jne	.LBB17_14
# BB#6:                                 # %if.end11
	movzbl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB17_7
# BB#8:                                 # %if.end17
	movq	(%r15), %rdi
	movq	$256, %rsi              # imm = 0x100
	movq	$438, %rdx              # imm = 0x1B6
	xorq	%rax, %rax
	callq	open
	movq	%rax, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdi, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	movl	%edi, 176(%r15)
	jne	.LBB17_9
	jmp	.LBB17_12
.LBB17_7:                               # %if.end17.thread
	xorq	%rdi, %rdi
	movl	%edi, 176(%r15)
.LBB17_9:                               # %land.lhs.true21
	leaq	32(%r15), %rsi
	callq	fstat
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB17_12
# BB#10:                                # %if.then26
	movl	56(%r15), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	jne	.LBB17_14
# BB#11:                                # %if.end31
	callq	__errno_location
	movq	$2, %rcx
	movl	%ecx, (%rax)
	movl	176(%r15), %edi
	callq	close
	movl	%r14d, 176(%r15)
.LBB17_12:                              # %if.end36
	callq	__errno_location
	movl	(%rax), %eax
	movl	%eax, 180(%r15)
	jmp	.LBB17_13
.LBB17_14:                              # %return
	movq	%r15, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp154:
	.size	open_file, .Ltmp154-open_file
	.cfi_endproc

	.globl	_cpp_pop_file_buffer
	.align	16, 0x90
	.type	_cpp_pop_file_buffer,@function
_cpp_pop_file_buffer:                   # @_cpp_pop_file_buffer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp159:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp160:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp161:
	.cfi_def_cfa_offset 32
.Ltmp162:
	.cfi_offset %rbx, -32
.Ltmp163:
	.cfi_offset %r14, -24
.Ltmp164:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movzbl	184(%r15), %eax
	movq	%rsi, %rbx
	testq	%rax, %rax
	je	.LBB18_3
# BB#1:                                 # %land.lhs.true
	cmpq	$0, 8(%rbx)
	jne	.LBB18_3
# BB#2:                                 # %if.then
	movq	168(%r15), %rax
	movq	%rax, 8(%rbx)
.LBB18_3:                               # %if.end
	xorq	%r14, %r14
	movb	%r14b, 184(%r15)
	movzbl	186(%rbx), %eax
	decq	%rax
	testq	$65535, %rax            # imm = 0xFFFF
	movw	%ax, 186(%rbx)
	jne	.LBB18_9
# BB#4:                                 # %land.lhs.true6
	movq	8(%rbx), %rax
	cmpq	$-1, %rax
	je	.LBB18_7
# BB#5:                                 # %land.lhs.true6
	testq	%rax, %rax
	je	.LBB18_9
# BB#6:                                 # %lor.lhs.false
	movzbl	20(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB18_9
.LBB18_7:                               # %if.then16
	movq	24(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB18_9
# BB#8:                                 # %if.then.i
	callq	free
	movq	$0, 24(%rbx)
.LBB18_9:                               # %if.end17
	cmpq	$0, (%r15)
	je	.LBB18_12
# BB#10:                                # %if.then19
	movq	$1, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	xorq	%r14, %r14
	movq	%r15, %rdi
	callq	_cpp_do_file_change
	movq	(%r15), %rax
	cmpq	$0, 32(%rax)
	jne	.LBB18_12
# BB#11:                                # %if.then22
	movq	%r15, %rdi
	callq	_cpp_push_next_buffer
	movq	%rax, %r14
.LBB18_12:                              # %if.end24
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp165:
	.size	_cpp_pop_file_buffer, .Ltmp165-_cpp_pop_file_buffer
	.cfi_endproc

	.globl	_cpp_simplify_pathname
	.align	16, 0x90
	.type	_cpp_simplify_pathname,@function
_cpp_simplify_pathname:                 # @_cpp_simplify_pathname
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp172:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp173:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp174:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp175:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp176:
	.cfi_def_cfa_offset 48
	subq	$176, %rsp
.Ltmp177:
	.cfi_def_cfa_offset 224
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
	callq	__errno_location
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
	movzbl	(%rbx), %eax
	testq	%rax, %rax
	je	.LBB19_46
# BB#1:                                 # %entry
	cmpq	$47, %rax
	movq	%rbx, %r15
	movq	%rbx, %rdx
	jne	.LBB19_7
# BB#2:                                 # %if.then5
	movzbl	1(%rbx), %eax
	leaq	1(%rbx), %rdx
	cmpq	$47, %rax
	movq	$1, %rcx
	jne	.LBB19_3
# BB#4:                                 # %if.then10
	movzbl	2(%rbx), %eax
	leaq	2(%rbx), %r15
	cmpq	$47, %rax
	jne	.LBB19_5
	.align	16, 0x90
.LBB19_6:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movzbl	1(%r15), %eax
	incq	%r15
	cmpq	$47, %rax
	je	.LBB19_6
	jmp	.LBB19_7
.LBB19_3:
	movq	%rdx, %r15
	jmp	.LBB19_7
.LBB19_5:
	movq	%r15, %rdx
.LBB19_7:                               # %if.end23
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	xorq	%rsi, %rsi
	movq	%rdx, %r12
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB19_8
	.align	16, 0x90
.LBB19_74:                              # %while.end112
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	(%rsp), %rcx            # 8-byte Reload
.LBB19_8:                               # %for.cond.outer.outer
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB19_49 Depth 2
                                        #       Child Loop BB19_69 Depth 3
                                        #     Child Loop BB19_9 Depth 2
                                        #       Child Loop BB19_10 Depth 3
                                        #       Child Loop BB19_24 Depth 3
                                        #     Child Loop BB19_40 Depth 2
	testq	%rcx, %rcx
	je	.LBB19_49
	jmp	.LBB19_9
	.align	16, 0x90
.LBB19_52:                              # %while.body28
                                        #   in Loop: Header=BB19_49 Depth=2
	incq	%r15
.LBB19_49:                              # %while.cond24
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_69 Depth 3
	movzbl	(%r15), %eax
	xorq	%r14, %r14
	cmpq	$46, %rax
	je	.LBB19_53
# BB#50:                                # %while.cond24
                                        #   in Loop: Header=BB19_49 Depth=2
	testq	%rax, %rax
	je	.LBB19_43
# BB#51:                                # %while.cond24
                                        #   in Loop: Header=BB19_49 Depth=2
	cmpq	$47, %rax
	je	.LBB19_52
	jmp	.LBB19_61
	.align	16, 0x90
.LBB19_53:                              # %if.then39
                                        #   in Loop: Header=BB19_49 Depth=2
	movzbl	1(%r15), %eax
	cmpq	$46, %rax
	je	.LBB19_57
# BB#54:                                # %if.then39
                                        #   in Loop: Header=BB19_49 Depth=2
	testq	%rax, %rax
	je	.LBB19_43
# BB#55:                                # %if.then39
                                        #   in Loop: Header=BB19_49 Depth=2
	cmpq	$47, %rax
	jne	.LBB19_61
# BB#56:                                # %if.then49
                                        #   in Loop: Header=BB19_49 Depth=2
	addq	$2, %r15
	jmp	.LBB19_49
	.align	16, 0x90
.LBB19_57:                              # %land.lhs.true
                                        #   in Loop: Header=BB19_49 Depth=2
	movzbl	2(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_59
# BB#58:                                # %land.lhs.true
                                        #   in Loop: Header=BB19_49 Depth=2
	cmpq	$47, %rax
	jne	.LBB19_61
.LBB19_59:                              # %if.end69
                                        #   in Loop: Header=BB19_49 Depth=2
	movq	$1, %r14
	cmpq	%r12, 24(%rsp)          # 8-byte Folded Reload
	je	.LBB19_60
# BB#64:                                # %land.lhs.true72
                                        #   in Loop: Header=BB19_49 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	jne	.LBB19_61
# BB#65:                                # %if.then76
                                        #   in Loop: Header=BB19_49 Depth=2
	movb	%sil, (%r12)
	movq	%rbx, %rdi
	xorq	%r13, %r13
	leaq	32(%rsp), %rsi
	callq	stat
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_66
# BB#67:                                # %if.then.i
                                        #   in Loop: Header=BB19_49 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%r13d, (%rax)
	movl	56(%rsp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	movabsq	$0, %rsi
	jne	.LBB19_61
# BB#68:                                # %while.cond80.preheader
                                        #   in Loop: Header=BB19_49 Depth=2
	addq	$2, %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jbe	.LBB19_49
	.align	16, 0x90
.LBB19_69:                              # %land.rhs
                                        #   Parent Loop BB19_8 Depth=1
                                        #     Parent Loop BB19_49 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %ecx
	leaq	-1(%r12), %rax
	cmpq	$47, %rcx
	je	.LBB19_49
# BB#70:                                # %while.cond80
                                        #   in Loop: Header=BB19_69 Depth=3
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rax, %r12
	ja	.LBB19_69
# BB#71:                                #   in Loop: Header=BB19_49 Depth=2
	movq	%rax, %r12
	jmp	.LBB19_49
.LBB19_66:                              #   in Loop: Header=BB19_8 Depth=1
	xorq	%rsi, %rsi
	jmp	.LBB19_61
.LBB19_26:                              #   in Loop: Header=BB19_9 Depth=2
	movq	%rax, %r12
	.align	16, 0x90
.LBB19_9:                               # %for.cond.outer.us
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB19_10 Depth 3
                                        #       Child Loop BB19_24 Depth 3
	cmpq	%r12, 8(%rsp)           # 8-byte Folded Reload
	jne	.LBB19_10
	jmp	.LBB19_40
	.align	16, 0x90
.LBB19_27:                              # %while.body28.us186
                                        #   in Loop: Header=BB19_10 Depth=3
	incq	%r15
.LBB19_10:                              # %while.cond24.us176
                                        #   Parent Loop BB19_8 Depth=1
                                        #     Parent Loop BB19_9 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_43
# BB#11:                                # %while.cond24.us176
                                        #   in Loop: Header=BB19_10 Depth=3
	cmpq	$47, %rax
	je	.LBB19_27
# BB#12:                                # %while.cond24.us176
                                        #   in Loop: Header=BB19_10 Depth=3
	xorq	%r14, %r14
	cmpq	$46, %rax
	jne	.LBB19_61
# BB#13:                                # %if.then39.us178
                                        #   in Loop: Header=BB19_10 Depth=3
	movzbl	1(%r15), %eax
	cmpq	$47, %rax
	jne	.LBB19_14
# BB#28:                                # %for.cond.backedge.us190
                                        #   in Loop: Header=BB19_10 Depth=3
	addq	$2, %r15
	jmp	.LBB19_10
.LBB19_14:                              # %if.then39.us178
                                        #   in Loop: Header=BB19_9 Depth=2
	testq	%rax, %rax
	je	.LBB19_43
# BB#15:                                # %if.then39.us178
                                        #   in Loop: Header=BB19_9 Depth=2
	cmpq	$46, %rax
	jne	.LBB19_61
# BB#16:                                # %land.lhs.true.us181
                                        #   in Loop: Header=BB19_9 Depth=2
	movzbl	2(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_18
# BB#17:                                # %land.lhs.true.us181
                                        #   in Loop: Header=BB19_9 Depth=2
	cmpq	$47, %rax
	jne	.LBB19_61
.LBB19_18:                              # %if.end69.us
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	$1, %r14
	cmpq	%r12, 24(%rsp)          # 8-byte Folded Reload
	je	.LBB19_60
# BB#19:                                # %land.lhs.true72.us
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	(%rax), %eax
	testq	%rax, %rax
	jne	.LBB19_61
# BB#20:                                # %if.then76.us
                                        #   in Loop: Header=BB19_9 Depth=2
	movb	%sil, (%r12)
	movq	%rbx, %rdi
	xorq	%r13, %r13
	leaq	32(%rsp), %rsi
	callq	stat
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB19_21
# BB#22:                                # %if.then.i.us
                                        #   in Loop: Header=BB19_9 Depth=2
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%r13d, (%rax)
	movl	56(%rsp), %eax
	andq	$61440, %rax            # imm = 0xF000
	cmpq	$16384, %rax            # imm = 0x4000
	movabsq	$0, %rsi
	jne	.LBB19_61
# BB#23:                                # %while.cond80.preheader.us
                                        #   in Loop: Header=BB19_9 Depth=2
	addq	$2, %r15
	movq	24(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r12
	jbe	.LBB19_9
.LBB19_24:                              # %land.rhs.us
                                        #   Parent Loop BB19_8 Depth=1
                                        #     Parent Loop BB19_9 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	movzbl	(%r12), %ecx
	leaq	-1(%r12), %rax
	cmpq	$47, %rcx
	je	.LBB19_9
# BB#25:                                # %while.cond80.us
                                        #   in Loop: Header=BB19_24 Depth=3
	movq	24(%rsp), %rcx          # 8-byte Reload
	cmpq	%rcx, %rax
	movq	%rax, %r12
	ja	.LBB19_24
	jmp	.LBB19_26
.LBB19_39:                              # %while.cond24.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	movq	%rax, %r15
.LBB19_40:                              # %while.cond24.us.us
                                        #   Parent Loop BB19_8 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%r15, %rax
	movzbl	(%rax), %ecx
	xorq	%r14, %r14
	cmpq	$46, %rcx
	je	.LBB19_29
# BB#41:                                # %while.cond24.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	testq	%rcx, %rcx
	je	.LBB19_42
# BB#47:                                # %while.cond24.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	cmpq	$47, %rcx
	jne	.LBB19_32
# BB#48:                                # %while.body28.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	incq	%rax
	jmp	.LBB19_39
.LBB19_29:                              # %if.then39.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	movzbl	1(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB19_42
# BB#30:                                # %if.then39.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	cmpq	$47, %rcx
	movabsq	$0, %rsi
	jne	.LBB19_31
# BB#38:                                # %if.then49.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	addq	$2, %rax
	jmp	.LBB19_39
.LBB19_31:                              # %if.then39.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	cmpq	$46, %rcx
	jne	.LBB19_32
# BB#36:                                # %land.lhs.true.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	movzbl	2(%rax), %ecx
	leaq	2(%rax), %r15
	testq	%rcx, %rcx
	je	.LBB19_40
# BB#37:                                # %land.lhs.true.us.us
                                        #   in Loop: Header=BB19_40 Depth=2
	cmpq	$47, %rcx
	je	.LBB19_40
.LBB19_32:                              #   in Loop: Header=BB19_8 Depth=1
	movq	%rax, %r15
	movq	8(%rsp), %r12           # 8-byte Reload
	jmp	.LBB19_33
.LBB19_60:                              #   in Loop: Header=BB19_8 Depth=1
	movq	24(%rsp), %r12          # 8-byte Reload
	.align	16, 0x90
.LBB19_61:                              # %if.end94
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	8(%rsp), %rax           # 8-byte Reload
	cmpq	%rax, %r12
	jbe	.LBB19_33
# BB#62:                                # %if.then97
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	$47, %rax
	jmp	.LBB19_63
.LBB19_21:                              #   in Loop: Header=BB19_8 Depth=1
	xorq	%rsi, %rsi
	jmp	.LBB19_61
	.align	16, 0x90
.LBB19_33:                              # %while.cond100
                                        #   in Loop: Header=BB19_8 Depth=1
	movzbl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB19_72
# BB#34:                                # %while.cond100
                                        #   in Loop: Header=BB19_8 Depth=1
	cmpq	$47, %rax
	je	.LBB19_72
# BB#35:                                # %while.body109
                                        #   in Loop: Header=BB19_8 Depth=1
	incq	%r15
.LBB19_63:                              # %while.cond100
                                        #   in Loop: Header=BB19_8 Depth=1
	movb	%al, (%r12)
	incq	%r12
	jmp	.LBB19_33
	.align	16, 0x90
.LBB19_72:                              # %while.end112
                                        #   in Loop: Header=BB19_8 Depth=1
	testq	%r14, %r14
	je	.LBB19_74
# BB#73:                                # %while.end112
                                        #   in Loop: Header=BB19_8 Depth=1
	movq	%r12, 24(%rsp)          # 8-byte Spill
	jmp	.LBB19_74
.LBB19_42:
	movq	8(%rsp), %r12           # 8-byte Reload
.LBB19_43:                              # %for.end
	cmpq	%rbx, %r12
	jne	.LBB19_45
# BB#44:                                # %if.then118
	leaq	1(%rbx), %r12
	movq	$46, %rax
	movb	%al, (%rbx)
.LBB19_45:                              # %if.end120
	xorq	%rax, %rax
	movb	%al, (%r12)
.LBB19_46:                              # %return
	movq	%rbx, %rax
	addq	$176, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp183:
	.size	_cpp_simplify_pathname, .Ltmp183-_cpp_simplify_pathname
	.cfi_endproc

	.align	16, 0x90
	.type	read_name_map,@function
read_name_map:                          # @read_name_map
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp187:
	.cfi_def_cfa_offset 16
.Ltmp188:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp189:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$56, %rsp
.Ltmp190:
	.cfi_offset %rbx, -56
.Ltmp191:
	.cfi_offset %r12, -48
.Ltmp192:
	.cfi_offset %r13, -40
.Ltmp193:
	.cfi_offset %r14, -32
.Ltmp194:
	.cfi_offset %r15, -24
	movq	%rdi, %r14
	movq	704(%r14), %r12
	movq	%rsi, %r15
	testq	%r12, %r12
	je	.LBB20_6
# BB#1:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB20_3:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%r12), %rdi
	movq	%r15, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB20_4
# BB#2:                                 # %for.cond
                                        #   in Loop: Header=BB20_3 Depth=1
	movq	(%r12), %r12
	testq	%r12, %r12
	jne	.LBB20_3
.LBB20_6:                               # %for.end
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %r13
	movq	%r15, %rdi
	callq	xstrdup
	movq	%rax, 8(%r13)
	movq	$0, 16(%r13)
	movq	%r15, %rdi
	callq	strlen
	movq	%rsp, %rbx
	addq	$27, %rax
	andq	$-16, %rax
	subq	%rax, %rbx
	movq	%rbx, %rsp
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movzbl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB20_8
# BB#7:                                 # %if.then10
	movq	%rbx, %rdi
	callq	strlen
	movq	$47, %rcx
	movw	%cx, (%rbx,%rax)
.LBB20_8:                               # %if.end12
	leaq	16(%r13), %r12
	movq	%rbx, %rdi
	callq	strlen
	xorq	%rcx, %rcx
	movb	%cl, 10(%rbx,%rax)
	movq	$25443, %rcx            # imm = 0x6363
	movw	%cx, 8(%rbx,%rax)
	movabsq	$7435005814636635496, %rcx # imm = 0x672E726564616568
	movq	%rcx, (%rbx,%rax)
	movabsq	$.L.str3, %rsi
	movq	%rbx, %rdi
	callq	fopen
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB20_14
# BB#9:                                 # %if.then16
	movq	%r13, -96(%rbp)         # 8-byte Spill
	movq	%r14, -88(%rbp)         # 8-byte Spill
	movq	%r15, %rdi
	movq	%r15, -56(%rbp)         # 8-byte Spill
	callq	strlen
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	getc
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	%r13, %rcx
	je	.LBB20_13
# BB#10:                                # %while.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	movq	%r14, -64(%rbp)         # 8-byte Spill
	leaq	2(%r14), %rcx
	movq	%rcx, -72(%rbp)         # 8-byte Spill
	leaq	1(%r14), %rcx
	movq	%rcx, -80(%rbp)         # 8-byte Spill
	.align	16, 0x90
.LBB20_11:                              # %while.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB20_16 Depth 2
                                        #     Child Loop BB20_24 Depth 2
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	_sch_istable+1(%rcx,%rcx), %ecx
	testq	$12, %rcx
	jne	.LBB20_12
# BB#15:                                # %if.end24
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	read_filename_string
	movq	%rax, -48(%rbp)         # 8-byte Spill
	.align	16, 0x90
.LBB20_16:                              # %while.cond26
                                        #   Parent Loop BB20_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	getc
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	%r13, %rcx
	je	.LBB20_17
# BB#18:                                # %land.rhs
                                        #   in Loop: Header=BB20_16 Depth=2
	movq	%rax, %rcx
	andq	$255, %rcx
	movzbl	_sch_istable(%rcx,%rcx), %ecx
	testq	$1, %rcx
	jne	.LBB20_16
	jmp	.LBB20_19
	.align	16, 0x90
.LBB20_17:                              #   in Loop: Header=BB20_11 Depth=1
	movq	%r13, %rax
.LBB20_19:                              # %while.end
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	read_filename_string
	movq	%rax, %r15
	movq	$24, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	-48(%rbp), %rax         # 8-byte Reload
	movq	%rax, 8(%r14)
	movzbl	(%r15), %eax
	cmpq	$36, %rax
	je	.LBB20_21
# BB#20:                                # %while.end
                                        #   in Loop: Header=BB20_11 Depth=1
	cmpq	$47, %rax
	jne	.LBB20_22
.LBB20_21:                              # %if.then47
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	%r15, 16(%r14)
	jmp	.LBB20_23
.LBB20_22:                              # %if.else
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	%r15, %rdi
	callq	strlen
	movq	-72(%rbp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	movq	%rax, %rdi
	callq	xmalloc
	movq	%rax, 16(%r14)
	movq	%rax, %rdi
	movq	-56(%rbp), %rsi         # 8-byte Reload
	callq	strcpy
	movq	16(%r14), %rax
	movq	-64(%rbp), %rcx         # 8-byte Reload
	movq	$47, %rdx
	movb	%dl, (%rax,%rcx)
	movq	16(%r14), %rdi
	movq	-80(%rbp), %rax         # 8-byte Reload
	addq	%rax, %rdi
	movq	%r15, %rsi
	callq	strcpy
	movq	%r15, %rdi
	callq	free
.LBB20_23:                              # %if.end62
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	(%r12), %rax
	movq	%rax, (%r14)
	movq	%r14, (%r12)
.LBB20_24:                              # %while.cond65
                                        #   Parent Loop BB20_11 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, %rdi
	callq	getc
	andq	%r13, %rax
	cmpq	%r13, %rax
	je	.LBB20_12
# BB#25:                                # %while.cond65
                                        #   in Loop: Header=BB20_24 Depth=2
	cmpq	$10, %rax
	jne	.LBB20_24
	.align	16, 0x90
.LBB20_12:                              # %while.cond.backedge
                                        #   in Loop: Header=BB20_11 Depth=1
	movq	%rbx, %rdi
	callq	getc
	movq	%rax, %rcx
	andq	%r13, %rcx
	cmpq	%r13, %rcx
	jne	.LBB20_11
.LBB20_13:                              # %while.end75
	movq	%rbx, %rdi
	callq	fclose
	movq	-88(%rbp), %r14         # 8-byte Reload
	movq	-96(%rbp), %r13         # 8-byte Reload
.LBB20_14:                              # %if.end77
	movq	704(%r14), %rax
	movq	%rax, (%r13)
	movq	%r13, 704(%r14)
	jmp	.LBB20_5
.LBB20_4:                               # %if.then
	addq	$16, %r12
.LBB20_5:                               # %return
	movq	(%r12), %rax
	leaq	-40(%rbp), %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp195:
	.size	read_name_map, .Ltmp195-read_name_map
	.cfi_endproc

	.align	16, 0x90
	.type	read_filename_string,@function
read_filename_string:                   # @read_filename_string
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp202:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp203:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp204:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp205:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp206:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp207:
	.cfi_def_cfa_offset 64
.Ltmp208:
	.cfi_offset %rbx, -48
.Ltmp209:
	.cfi_offset %r12, -40
.Ltmp210:
	.cfi_offset %r13, -32
.Ltmp211:
	.cfi_offset %r14, -24
.Ltmp212:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	%rdi, %r12
	movq	$21, %rdi
	callq	xmalloc
	movq	%r12, %rcx
	andq	$255, %rcx
	movzbl	_sch_istable+1(%rcx,%rcx), %ecx
	testq	$12, %rcx
	je	.LBB21_2
# BB#1:
	movq	%rax, %r14
	movq	%rax, %r15
	jmp	.LBB21_12
.LBB21_2:                               # %if.then
	movb	%r12b, (%rax)
	leaq	1(%rax), %r14
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rbx, %rdi
	callq	getc
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%r12, %rcx
	cmpq	%r12, %rcx
	je	.LBB21_11
# BB#3:
	movq	$20, %rdx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%rax, %r12
	.align	16, 0x90
.LBB21_4:                               # %land.rhs
                                        # =>This Inner Loop Header: Depth=1
	movq	%r12, %rax
	andq	$255, %rax
	movzbl	_sch_istable+1(%rax,%rax), %eax
	testq	$12, %rax
	movq	(%rsp), %rdi            # 8-byte Reload
	jne	.LBB21_5
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	%rbx, %r15
	movq	%r14, %rbx
	subq	%rdi, %rbx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rax, %rbx
	jne	.LBB21_7
# BB#8:                                 # %if.then15
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	$33, %rcx
	movq	%rdx, %rsi
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	addq	%rdx, %rdx
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	orq	$1, %rsi
	callq	xrealloc
	movq	%rax, (%rsp)            # 8-byte Spill
	addq	%rax, %rbx
	movq	%rbx, %r14
	jmp	.LBB21_9
	.align	16, 0x90
.LBB21_7:                               #   in Loop: Header=BB21_4 Depth=1
	movq	%rdx, 8(%rsp)           # 8-byte Spill
.LBB21_9:                               # %if.end
                                        #   in Loop: Header=BB21_4 Depth=1
	movq	%r15, %rbx
	movb	%r12b, (%r14)
	incq	%r14
	movq	%rbx, %rdi
	callq	getc
	movq	%rax, %r12
                                        # kill: RAX<def> R12<kill>
	andq	%r13, %rax
	cmpq	%r13, %rax
	movq	8(%rsp), %rdx           # 8-byte Reload
	jne	.LBB21_4
# BB#10:
	movq	%r13, %r12
.LBB21_11:                              # %if.end21
	movq	(%rsp), %r15            # 8-byte Reload
	jmp	.LBB21_12
.LBB21_5:
	movq	%rdi, %r15
.LBB21_12:                              # %if.end21
	xorq	%rax, %rax
	movb	%al, (%r14)
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	ungetc
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp213:
	.size	read_filename_string, .Ltmp213-read_filename_string
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"absolute file name in remap_filename"
	.size	.L.str, 37

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"header.gcc"
	.size	.L.str2, 11

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"r"
	.size	.L.str3, 2

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"<stdin>"
	.size	.L.str4, 8

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"%s is shorter than expected"
	.size	.L.str6, 28

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s is a block device"
	.size	.L.str7, 21

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"%s: %s"
	.size	.L.str8, 7

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"no include path in which to find %s"
	.size	.L.str9, 36

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"."
	.size	.L.str10, 2

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Multiple include guards may be useful for:\n"
	.size	.L.str11, 44


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
