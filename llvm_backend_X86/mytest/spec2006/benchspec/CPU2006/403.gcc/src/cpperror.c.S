	.file	"cpperror.c.bc"
	.text
	.globl	_cpp_begin_message
	.align	16, 0x90
	.type	_cpp_begin_message,@function
_cpp_begin_message:                     # @_cpp_begin_message
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp7:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp8:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp9:
	.cfi_def_cfa_offset 48
.Ltmp10:
	.cfi_offset %rbx, -40
.Ltmp11:
	.cfi_offset %r12, -32
.Ltmp12:
	.cfi_offset %r14, -24
.Ltmp13:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rdi, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$5, %rax
	ja	.LBB0_12
# BB#1:                                 # %entry
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_2:                                # %sw.bb
	movq	56(%r12), %rcx
	testq	%rcx, %rcx
	je	.LBB0_5
# BB#3:                                 # %land.lhs.true
	movzbl	21(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB0_5
# BB#4:                                 # %land.lhs.true3
	movzbl	756(%r12), %ecx
	xorq	%rbx, %rbx
	testq	%rcx, %rcx
	je	.LBB0_17
.LBB0_5:                                # %sw.bb5
	movzbl	762(%r12), %ecx
	testq	%rcx, %rcx
	jne	.LBB0_8
# BB#6:                                 # %lor.lhs.false
	cmpq	$2, %rax
	jne	.LBB0_15
# BB#7:                                 # %land.lhs.true10
	movzbl	754(%r12), %eax
	testq	%rax, %rax
	je	.LBB0_15
.LBB0_8:                                # %if.then14
	movzbl	757(%r12), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB0_17
# BB#9:                                 # %if.end18
	movl	248(%r12), %eax
	cmpq	$999, %rax              # imm = 0x3E7
	ja	.LBB0_12
# BB#10:                                # %if.then21
	incq	%rax
	jmp	.LBB0_11
.LBB0_13:                               # %sw.bb42
	movq	$1000, %rax             # imm = 0x3E8
.LBB0_11:                               # %return
	movl	%eax, 248(%r12)
.LBB0_12:                               # %if.end49.critedge70
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	print_location
	movq	$1, %rbx
.LBB0_17:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB0_15:                               # %if.else
	movzbl	755(%r12), %eax
	xorq	%rbx, %rbx
	testq	%rax, %rax
	jne	.LBB0_17
# BB#16:                                # %sw.epilog
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	print_location
	movq	stderr(%rip), %rcx
	movabsq	$.L.str1, %rdi
	movq	$9, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	callq	fwrite
	jmp	.LBB0_17
.LBB0_14:                               # %sw.bb44
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	movq	$1, %rbx
	callq	fwrite
	movq	$1000, %rax             # imm = 0x3E8
	movl	%eax, 248(%r12)
	movq	%r12, %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
	callq	print_location
	jmp	.LBB0_17
.Ltmp14:
	.size	_cpp_begin_message, .Ltmp14-_cpp_begin_message
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_2
	.quad	.LBB0_5
	.quad	.LBB0_2
	.quad	.LBB0_8
	.quad	.LBB0_13
	.quad	.LBB0_14

	.text
	.align	16, 0x90
	.type	print_location,@function
print_location:                         # @print_location
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp21:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp22:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp23:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp24:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp25:
	.cfi_def_cfa_offset 48
.Ltmp26:
	.cfi_offset %rbx, -48
.Ltmp27:
	.cfi_offset %r12, -40
.Ltmp28:
	.cfi_offset %r13, -32
.Ltmp29:
	.cfi_offset %r14, -24
.Ltmp30:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	cmpq	$0, (%rbx)
	movq	%rdx, %r14
	movq	%rsi, %r12
	jne	.LBB1_1
# BB#11:                                # %if.then
	movq	stderr(%rip), %rdi
	movq	progname(%rip), %rdx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB1_1:                                # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r12
	jne	.LBB1_3
# BB#2:                                 # %if.then2
	movq	192(%rbx), %rax
	movl	-24(%rax), %r12d
	movzbl	-20(%rax), %r14d
.LBB1_3:                                # %if.end
	leaq	24(%rbx), %r13
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	lookup_line
	movq	%rax, %r15
	movq	%r13, %rdi
	movq	%r15, %rsi
	callq	print_containing_files
	movl	8(%r15), %ecx
	addq	%r12, %rcx
	movq	%rcx, %rdx
	movl	12(%r15), %eax
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rdx
	testq	%rsi, %r14
	movq	$1, %r8
	je	.LBB1_5
# BB#4:                                 # %if.end
	movq	%r14, %r8
.LBB1_5:                                # %if.end
	cmpq	%rax, %rdx
	jne	.LBB1_7
# BB#6:                                 # %if.then15
	movq	stderr(%rip), %rbx
	movq	(%r15), %rdx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	jmp	.LBB1_10
.LBB1_7:                                # %if.else17
	movzbl	776(%rbx), %esi
	movq	stderr(%rip), %rbx
	movq	(%r15), %rdx
	subq	%rax, %rcx
	testq	%rsi, %rsi
	jne	.LBB1_9
# BB#8:                                 # %if.then21
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
	jmp	.LBB1_10
.LBB1_9:                                # %if.else24
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	fprintf
.LBB1_10:                               # %if.end28
	movq	$32, %rdi
	movq	%rbx, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fputc  # TAILCALL
.Ltmp31:
	.size	print_location, .Ltmp31-print_location
	.cfi_endproc

	.globl	cpp_ice
	.align	16, 0x90
	.type	cpp_ice,@function
cpp_ice:                                # @cpp_ice
	.cfi_startproc
# BB#0:                                 # %if.end
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp38:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp39:
	.cfi_def_cfa_offset 112
.Ltmp40:
	.cfi_offset %rbx, -32
.Ltmp41:
	.cfi_offset %r14, -24
.Ltmp42:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	112(%rsp), %rax
	movq	stderr(%rip), %r15
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movabsq	$.L.str, %rdi
	movq	$16, %rsi
	movq	$1, %rdx
	movq	%r15, %rcx
	callq	fwrite
	movq	$1000, %rax             # imm = 0x3E8
	movl	%eax, 248(%rbx)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%rbx, %rdi
	callq	print_location
	leaq	48(%rsp), %rdx
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r15, %rsi
	callq	putc
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp43:
	.size	cpp_ice, .Ltmp43-cpp_ice
	.cfi_endproc

	.globl	cpp_fatal
	.align	16, 0x90
	.type	cpp_fatal,@function
cpp_fatal:                              # @cpp_fatal
	.cfi_startproc
# BB#0:                                 # %if.end
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp49:
	.cfi_def_cfa_offset 96
.Ltmp50:
	.cfi_offset %rbx, -24
.Ltmp51:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movq	$1000, %rax             # imm = 0x3E8
	movl	%eax, 248(%rdi)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	print_location
	movq	stderr(%rip), %rbx
	leaq	48(%rsp), %rdx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	putc
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp52:
	.size	cpp_fatal, .Ltmp52-cpp_fatal
	.cfi_endproc

	.globl	cpp_error
	.align	16, 0x90
	.type	cpp_error,@function
cpp_error:                              # @cpp_error
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp56:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp58:
	.cfi_def_cfa_offset 96
.Ltmp59:
	.cfi_offset %rbx, -24
.Ltmp60:
	.cfi_offset %r14, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movzbl	757(%rdi), %eax
	movq	%rsi, %rbx
	testq	%rax, %rax
	je	.LBB4_1
.LBB4_4:                                # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB4_1:                                # %if.end34.i
	movl	248(%rdi), %eax
	cmpq	$999, %rax              # imm = 0x3E7
	ja	.LBB4_3
# BB#2:                                 # %if.then38.i
	incq	%rax
	movl	%eax, 248(%rdi)
.LBB4_3:                                # %if.end49.critedge68.i
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	print_location
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB4_4
.Ltmp61:
	.size	cpp_error, .Ltmp61-cpp_error
	.cfi_endproc

	.globl	cpp_error_with_line
	.align	16, 0x90
	.type	cpp_error_with_line,@function
cpp_error_with_line:                    # @cpp_error_with_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp65:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp67:
	.cfi_def_cfa_offset 96
.Ltmp68:
	.cfi_offset %rbx, -24
.Ltmp69:
	.cfi_offset %r14, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$32, 48(%rsp)
	movzbl	757(%rdi), %eax
	movq	%rcx, %rbx
	testq	%rax, %rax
	je	.LBB5_1
.LBB5_4:                                # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB5_1:                                # %if.end34.i
	movl	248(%rdi), %eax
	cmpq	$999, %rax              # imm = 0x3E7
	ja	.LBB5_3
# BB#2:                                 # %if.then38.i
	incq	%rax
	movl	%eax, 248(%rdi)
.LBB5_3:                                # %if.end49.critedge68.i
	callq	print_location
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB5_4
.Ltmp70:
	.size	cpp_error_with_line, .Ltmp70-cpp_error_with_line
	.cfi_endproc

	.globl	cpp_error_from_errno
	.align	16, 0x90
	.type	cpp_error_from_errno,@function
cpp_error_from_errno:                   # @cpp_error_from_errno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp74:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp75:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp76:
	.cfi_def_cfa_offset 32
.Ltmp77:
	.cfi_offset %rbx, -24
.Ltmp78:
	.cfi_offset %r14, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	callq	__errno_location
	movl	(%rax), %edi
	callq	xstrerror
	movq	%rax, %rcx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_error  # TAILCALL
.Ltmp79:
	.size	cpp_error_from_errno, .Ltmp79-cpp_error_from_errno
	.cfi_endproc

	.globl	cpp_warning
	.align	16, 0x90
	.type	cpp_warning,@function
cpp_warning:                            # @cpp_warning
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp83:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp84:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp85:
	.cfi_def_cfa_offset 96
.Ltmp86:
	.cfi_offset %rbx, -24
.Ltmp87:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB7_1
.LBB7_2:                                # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB7_1:                                # %do.body
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB7_2
.Ltmp88:
	.size	cpp_warning, .Ltmp88-cpp_warning
	.cfi_endproc

	.globl	cpp_warning_with_line
	.align	16, 0x90
	.type	cpp_warning_with_line,@function
cpp_warning_with_line:                  # @cpp_warning_with_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp92:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp93:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp94:
	.cfi_def_cfa_offset 96
.Ltmp95:
	.cfi_offset %rbx, -24
.Ltmp96:
	.cfi_offset %r14, -16
	movq	%rcx, %rbx
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	leaq	(%rsp), %rdx
	movq	%rdx, 64(%rsp)
	leaq	96(%rsp), %rdx
	movq	%rdx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$32, 48(%rsp)
	xorq	%rsi, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_1
.LBB8_2:                                # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB8_1:                                # %do.body
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB8_2
.Ltmp97:
	.size	cpp_warning_with_line, .Ltmp97-cpp_warning_with_line
	.cfi_endproc

	.globl	cpp_pedwarn
	.align	16, 0x90
	.type	cpp_pedwarn,@function
cpp_pedwarn:                            # @cpp_pedwarn
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp102:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp103:
	.cfi_def_cfa_offset 96
.Ltmp104:
	.cfi_offset %rbx, -24
.Ltmp105:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movq	$2, %rsi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB9_1
.LBB9_2:                                # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB9_1:                                # %do.body
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB9_2
.Ltmp106:
	.size	cpp_pedwarn, .Ltmp106-cpp_pedwarn
	.cfi_endproc

	.globl	cpp_pedwarn_with_line
	.align	16, 0x90
	.type	cpp_pedwarn_with_line,@function
cpp_pedwarn_with_line:                  # @cpp_pedwarn_with_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp110:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp111:
	.cfi_def_cfa_offset 24
	subq	$72, %rsp
.Ltmp112:
	.cfi_def_cfa_offset 96
.Ltmp113:
	.cfi_offset %rbx, -24
.Ltmp114:
	.cfi_offset %r14, -16
	movq	%rcx, %rbx
	movq	%rdx, %rax
	movq	%rsi, %rcx
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	leaq	(%rsp), %rdx
	movq	%rdx, 64(%rsp)
	leaq	96(%rsp), %rdx
	movq	%rdx, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$32, 48(%rsp)
	movq	$2, %rsi
	movq	%rcx, %rdx
	movq	%rax, %rcx
	callq	_cpp_begin_message
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB10_1
.LBB10_2:                               # %if.end
	addq	$72, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB10_1:                               # %do.body
	movq	stderr(%rip), %r14
	leaq	48(%rsp), %rdx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	vfprintf
	movq	$10, %rdi
	movq	%r14, %rsi
	callq	putc
	jmp	.LBB10_2
.Ltmp115:
	.size	cpp_pedwarn_with_line, .Ltmp115-cpp_pedwarn_with_line
	.cfi_endproc

	.globl	cpp_notice
	.align	16, 0x90
	.type	cpp_notice,@function
cpp_notice:                             # @cpp_notice
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp118:
	.cfi_def_cfa_offset 16
	subq	$80, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 96
.Ltmp120:
	.cfi_offset %rbx, -16
	movq	%r9, 40(%rsp)
	movq	%r8, 32(%rsp)
	movq	%rcx, 24(%rsp)
	movq	%rdx, 16(%rsp)
	leaq	(%rsp), %rax
	movq	%rax, 64(%rsp)
	leaq	96(%rsp), %rax
	movq	%rax, 56(%rsp)
	movq	$48, 52(%rsp)
	movq	$16, 48(%rsp)
	movl	248(%rdi), %eax
	cmpq	$999, %rax              # imm = 0x3E7
	ja	.LBB11_2
# BB#1:                                 # %if.then
	incq	%rax
	movl	%eax, 248(%rdi)
.LBB11_2:                               # %do.body
	movq	stderr(%rip), %rbx
	leaq	48(%rsp), %rdx
	movq	%rbx, %rdi
	callq	vfprintf
	movq	$10, %rdi
	movq	%rbx, %rsi
	callq	putc
	addq	$80, %rsp
	popq	%rbx
	retq
.Ltmp121:
	.size	cpp_notice, .Ltmp121-cpp_notice
	.cfi_endproc

	.globl	cpp_notice_from_errno
	.align	16, 0x90
	.type	cpp_notice_from_errno,@function
cpp_notice_from_errno:                  # @cpp_notice_from_errno
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp125:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp126:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp127:
	.cfi_def_cfa_offset 32
.Ltmp128:
	.cfi_offset %rbx, -24
.Ltmp129:
	.cfi_offset %r14, -16
	movq	%rsi, %rbx
	movzbl	(%rbx), %eax
	movq	%rdi, %r14
	testq	%rax, %rax
	jne	.LBB12_2
# BB#1:
	movabsq	$.L.str3, %rbx
.LBB12_2:                               # %select.end
	callq	__errno_location
	movl	(%rax), %edi
	callq	xstrerror
	movq	%rax, %rcx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	cpp_notice  # TAILCALL
.Ltmp130:
	.size	cpp_notice_from_errno, .Ltmp130-cpp_notice_from_errno
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"internal error: "
	.size	.L.str, 17

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"warning: "
	.size	.L.str1, 10

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"%s: %s"
	.size	.L.str2, 7

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"stdout"
	.size	.L.str3, 7

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"%s: "
	.size	.L.str4, 5

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"%s:"
	.size	.L.str5, 4

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"%s:%u:"
	.size	.L.str6, 7

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s:%u:%u:"
	.size	.L.str7, 10


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
