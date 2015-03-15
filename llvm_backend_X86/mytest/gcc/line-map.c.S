	.file	"line-map.c.bc"
	.text
	.globl	init_line_maps
	.align	16, 0x90
	.type	init_line_maps,@function
init_line_maps:                         # @init_line_maps
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$0, 8(%rdi)
	movq	$0, (%rdi)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 16(%rdi)
	xorq	%rax, %rax
	movb	%al, 24(%rdi)
	movl	%eax, 20(%rdi)
	retq
.Ltmp0:
	.size	init_line_maps, .Ltmp0-init_line_maps
	.cfi_endproc

	.globl	free_line_maps
	.align	16, 0x90
	.type	free_line_maps,@function
free_line_maps:                         # @free_line_maps
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp6:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp7:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp8:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp9:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp10:
	.cfi_def_cfa_offset 48
.Ltmp11:
	.cfi_offset %rbx, -40
.Ltmp12:
	.cfi_offset %r12, -32
.Ltmp13:
	.cfi_offset %r14, -24
.Ltmp14:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB1_5
# BB#1:                                 # %if.then
	movl	12(%r12), %eax
	decq	%rax
	leaq	(%rax,%rax,2), %rax
	movslq	16(%rdi,%rax,8), %rcx
	testq	%rcx, %rcx
	jns	.LBB1_2
.LBB1_4:                                # %for.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	free  # TAILCALL
.LBB1_5:                                # %if.end
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB1_2:                                # %for.body.lr.ph
	movq	stderr(%rip), %r14
	leaq	16(%rdi,%rax,8), %rbx
	leaq	(%rdi,%rax,8), %rax
	movabsq	$.L.str, %r15
	.align	16, 0x90
.LBB1_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movslq	(%rbx), %rax
	movq	(%r12), %rdi
	leaq	(%rax,%rax,2), %rcx
	movslq	16(%rdi,%rcx,8), %rdx
	leaq	(%rdi,%rcx,8), %rax
	leaq	16(%rdi,%rcx,8), %rbx
	testq	%rdx, %rdx
	jns	.LBB1_3
	jmp	.LBB1_4
.Ltmp15:
	.size	free_line_maps, .Ltmp15-free_line_maps
	.cfi_endproc

	.globl	add_line_map
	.align	16, 0x90
	.type	add_line_map,@function
add_line_map:                           # @add_line_map
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp24:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp25:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp26:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp27:
	.cfi_def_cfa_offset 80
.Ltmp28:
	.cfi_offset %rbx, -48
.Ltmp29:
	.cfi_offset %r12, -40
.Ltmp30:
	.cfi_offset %r13, -32
.Ltmp31:
	.cfi_offset %r14, -24
.Ltmp32:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movl	12(%r12), %r13d
	movq	%rsi, %r10
	testq	%r13, %r13
	je	.LBB2_2
# BB#1:                                 # %land.lhs.true
	leaq	-1(%r13), %rax
	movq	(%r12), %rsi
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %rax
	leaq	(%rax,%rax,2), %rax
	movl	12(%rsi,%rax,8), %eax
	andq	%rcx, %rdi
	cmpq	%rdi, %rax
	ja	.LBB2_28
.LBB2_2:                                # %if.end
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%r9, 16(%rsp)           # 8-byte Spill
	movq	%rcx, (%rsp)            # 8-byte Spill
	movl	8(%r12), %eax
	cmpq	%rax, %r13
	jne	.LBB2_3
# BB#4:                                 # %if.then5
	movq	%r8, 24(%rsp)           # 8-byte Spill
	leaq	256(%r13,%r13), %rax
	movl	%eax, 8(%r12)
	movq	(%r12), %rdi
	movabsq	$4294967294, %rdx       # imm = 0xFFFFFFFE
	andq	%rax, %rdx
	movq	$3, %rcx
	shlq	%cl, %rdx
	leaq	(%rdx,%rdx,2), %rsi
	movq	%r10, %rbx
	callq	xrealloc
	movq	%rbx, %r10
	movq	%rax, %r15
	movq	%r15, (%r12)
	movl	12(%r12), %r13d
	jmp	.LBB2_5
.LBB2_3:                                # %if.end.if.end12_crit_edge
	movq	%r8, 24(%rsp)           # 8-byte Spill
	movq	(%r12), %r15
.LBB2_5:                                # %if.end12
	leaq	1(%r13), %rax
	movl	%eax, 12(%r12)
	movl	20(%r12), %eax
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%rdi, %r13
	xorq	%r14, %r14
	testq	%rax, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB2_15
# BB#6:                                 # %if.else
	movq	%r10, %rax
	andq	%rdi, %rax
	cmpq	$1, %rax
	jne	.LBB2_7
# BB#8:                                 # %if.then22
	leaq	-1(%r13), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	16(%r15,%rax,8), %rcx
	testq	%rcx, %rcx
	js	.LBB2_9
# BB#12:                                # %if.else27
	leaq	(%rcx,%rcx,2), %rax
	leaq	(%r15,%rax,8), %rbx
	movq	$1, %r14
	testq	%rdx, %rdx
	je	.LBB2_11
# BB#13:                                # %if.end37
	movq	(%rbx), %rdi
	movq	%rdx, %rsi
	callq	strcmp
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	testq	%rdi, %rax
	movq	$1, %r14
	jne	.LBB2_10
# BB#14:
	movq	24(%rsp), %rdx          # 8-byte Reload
	jmp	.LBB2_15
.LBB2_7:
	movq	%r10, %r14
	jmp	.LBB2_15
.LBB2_9:                                # %if.end37.thread
	leaq	(%r15,%rax,8), %rbx
	movq	$2, %r14
.LBB2_10:                               # %if.then39
	movq	stderr(%rip), %rdi
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	fprintf
.LBB2_11:                               # %if.then46
	movq	(%rbx), %rdx
	movl	36(%rbx), %eax
	movl	8(%rbx), %esi
	movl	12(%rbx), %ecx
	movzbl	21(%rbx), %edi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	addq	%rax, %rsi
	subq	%rcx, %rsi
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
.LBB2_15:                               # %if.end60
	leaq	(%r13,%r13,2), %rax
	leaq	(%r15,%rax,8), %r13
	movq	%r14, %rcx
	andq	%rdi, %rcx
	cmpq	$2, %rcx
	movb	%r14b, 20(%r15,%rax,8)
	movq	8(%rsp), %rsi           # 8-byte Reload
	movb	%sil, 21(%r15,%rax,8)
	movq	(%rsp), %rsi            # 8-byte Reload
	movl	%esi, 12(%r15,%rax,8)
	movq	%rdx, (%r13)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movl	%esi, 8(%r15,%rax,8)
	je	.LBB2_24
# BB#16:                                # %if.end60
	cmpq	$1, %rcx
	jne	.LBB2_17
# BB#25:                                # %if.then88
	movl	20(%r12), %ecx
	decq	%rcx
	movl	%ecx, 20(%r12)
	movslq	-8(%r15,%rax,8), %rcx
	movq	(%r12), %rdx
	leaq	(%rcx,%rcx,2), %rcx
	movl	16(%rdx,%rcx,8), %ecx
	jmp	.LBB2_26
.LBB2_24:                               # %if.then81
	movl	-8(%r15,%rax,8), %ecx
.LBB2_26:                               # %if.end99
	movl	%ecx, 16(%r15,%rax,8)
	jmp	.LBB2_27
.LBB2_17:                               # %if.end60
	testq	%rdi, %r14
	jne	.LBB2_27
# BB#18:                                # %if.then69
	movl	20(%r12), %ebx
	leaq	1(%rbx), %rcx
	movl	%ecx, 20(%r12)
	movl	12(%r12), %ecx
	addq	$-2, %rcx
	movl	%ecx, 16(%r15,%rax,8)
	movzbl	24(%r12), %eax
	testq	%rax, %rax
	je	.LBB2_27
# BB#19:                                # %if.then76
	movq	stderr(%rip), %r14
	testq	%rdi, %rbx
	je	.LBB2_23
# BB#20:
	movq	%rdi, %r15
	.align	16, 0x90
.LBB2_21:                               # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$46, %rdi
	movq	%r14, %rsi
	callq	putc
	decq	%rbx
	testq	%r15, %rbx
	jne	.LBB2_21
# BB#22:                                # %trace_include.exit.loopexit
	movq	(%r13), %rdx
.LBB2_23:                               # %trace_include.exit
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
.LBB2_27:                               # %if.end99
	movq	%r13, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_28:                               # %if.then
	callq	abort
.Ltmp33:
	.size	add_line_map, .Ltmp33-add_line_map
	.cfi_endproc

	.globl	lookup_line
	.align	16, 0x90
	.type	lookup_line,@function
lookup_line:                            # @lookup_line
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp35:
	.cfi_def_cfa_offset 16
	movl	12(%rdi), %edx
	testq	%rdx, %rdx
	je	.LBB3_10
# BB#1:                                 # %while.cond.preheader
	movq	(%rdi), %r10
	xorq	%rdi, %rdi
	cmpq	$1, %rdx
	jbe	.LBB3_9
# BB#2:
	movabsq	$4294967294, %r9        # imm = 0xFFFFFFFE
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %rsi
	.align	16, 0x90
.LBB3_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	leaq	(%rdx,%rdi), %rax
	andq	%r9, %rax
	movq	$1, %rcx
	shrq	%cl, %rax
	leaq	(%rax,%rax,2), %rcx
	movl	12(%r10,%rcx,8), %ecx
	cmpq	%rsi, %rcx
	ja	.LBB3_5
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rax, %rdi
.LBB3_5:                                # %while.body
                                        #   in Loop: Header=BB3_3 Depth=1
	ja	.LBB3_7
# BB#6:                                 # %while.body
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rdx, %rax
.LBB3_7:                                # %while.body
                                        #   in Loop: Header=BB3_3 Depth=1
	movq	%rax, %rcx
	subq	%rdi, %rcx
	andq	%r8, %rcx
	cmpq	$1, %rcx
	movq	%rax, %rdx
	ja	.LBB3_3
# BB#8:                                 # %while.cond.while.end_crit_edge
	andq	%r8, %rdi
.LBB3_9:                                # %while.end
	leaq	(%rdi,%rdi,2), %rax
	leaq	(%r10,%rax,8), %rax
	popq	%rdx
	retq
.LBB3_10:                               # %if.then
	callq	abort
.Ltmp36:
	.size	lookup_line, .Ltmp36-lookup_line
	.cfi_endproc

	.globl	print_containing_files
	.align	16, 0x90
	.type	print_containing_files,@function
print_containing_files:                 # @print_containing_files
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp43:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp44:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp45:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp46:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp47:
	.cfi_def_cfa_offset 48
.Ltmp48:
	.cfi_offset %rbx, -48
.Ltmp49:
	.cfi_offset %r12, -40
.Ltmp50:
	.cfi_offset %r13, -32
.Ltmp51:
	.cfi_offset %r14, -24
.Ltmp52:
	.cfi_offset %r15, -16
	movslq	16(%rsi), %rax
	movq	%rdi, %rbx
	testq	%rax, %rax
	js	.LBB4_6
# BB#1:                                 # %lor.lhs.false
	movl	16(%rbx), %ecx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rax, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB4_2
.LBB4_6:                                # %return
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_2:                                # %if.end
	movl	%eax, 16(%rbx)
	movq	(%rbx), %r15
	movq	stderr(%rip), %r14
	leaq	(%rax,%rax,2), %r12
	movl	36(%r15,%r12,8), %eax
	movl	8(%r15,%r12,8), %ecx
	movl	12(%r15,%r12,8), %esi
	movq	(%r15,%r12,8), %rdx
	leaq	-1(%rax,%rcx), %rcx
	subq	%rsi, %rcx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	fprintf
	movslq	16(%r15,%r12,8), %rax
	testq	%rax, %rax
	jns	.LBB4_3
.LBB4_5:                                # %while.end
	movabsq	$.L.str4, %rdi
	movq	$2, %rsi
	movq	$1, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fwrite  # TAILCALL
.LBB4_3:
	movabsq	$.L.str3, %r15
.LBB4_4:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	(%rbx), %r12
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %r13
	movl	36(%r12,%r13,8), %eax
	movl	8(%r12,%r13,8), %ecx
	movl	12(%r12,%r13,8), %esi
	movq	(%r12,%r13,8), %rdx
	leaq	-1(%rax,%rcx), %rcx
	subq	%rsi, %rcx
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	fprintf
	movslq	16(%r12,%r13,8), %rax
	testq	%rax, %rax
	jns	.LBB4_4
	jmp	.LBB4_5
.Ltmp53:
	.size	print_containing_files, .Ltmp53-print_containing_files
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"line-map.c: file \"%s\" entered but not left\n"
	.size	.L.str, 44

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"line-map.c: file \"%s\" left but not entered\n"
	.size	.L.str1, 44

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"In file included from %s:%u"
	.size	.L.str2, 28

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	",\n                 from %s:%u"
	.size	.L.str3, 30

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	":\n"
	.size	.L.str4, 3

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	" %s\n"
	.size	.L.str5, 5


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
