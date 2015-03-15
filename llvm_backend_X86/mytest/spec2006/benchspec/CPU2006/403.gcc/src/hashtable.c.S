	.file	"hashtable.c.bc"
	.text
	.globl	gcc_obstack_init
	.align	16, 0x90
	.type	gcc_obstack_init,@function
gcc_obstack_init:                       # @gcc_obstack_init
	.cfi_startproc
# BB#0:                                 # %entry
	movabsq	$xmalloc, %rcx
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	jmp	_obstack_begin  # TAILCALL
.Ltmp0:
	.size	gcc_obstack_init, .Ltmp0-gcc_obstack_init
	.cfi_endproc

	.globl	ht_create
	.align	16, 0x90
	.type	ht_create,@function
ht_create:                              # @ht_create
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp5:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp6:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp7:
	.cfi_def_cfa_offset 32
.Ltmp8:
	.cfi_offset %rbx, -32
.Ltmp9:
	.cfi_offset %r14, -24
.Ltmp10:
	.cfi_offset %r15, -16
	movq	$1, %r14
	movq	%rdi, %rcx
	shlq	%cl, %r14
	movq	$128, %rdi
	callq	xmalloc
	movq	%rax, %rbx
	movabsq	$xmalloc, %rcx
	movq	$0, 120(%rbx)
	movq	$0, 112(%rbx)
	movq	$0, 104(%rbx)
	movq	$0, 96(%rbx)
	movq	$0, 88(%rbx)
	movq	$0, 80(%rbx)
	movq	$0, 72(%rbx)
	movq	$0, 64(%rbx)
	movq	$0, 56(%rbx)
	movq	$0, 48(%rbx)
	movq	$0, 40(%rbx)
	movq	$0, 32(%rbx)
	movq	$0, 24(%rbx)
	movq	$0, 16(%rbx)
	movq	$0, 8(%rbx)
	movq	$0, (%rbx)
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	xorq	%r15, %r15
	movq	%rbx, %rdi
	callq	_obstack_begin
	movl	%r15d, 48(%rbx)
	movabsq	$4294967295, %rdi       # imm = 0xFFFFFFFF
	andq	%r14, %rdi
	movq	$8, %rsi
	callq	xcalloc
	movq	%rax, 88(%rbx)
	movl	%r14d, 104(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp11:
	.size	ht_create, .Ltmp11-ht_create
	.cfi_endproc

	.globl	ht_destroy
	.align	16, 0x90
	.type	ht_destroy,@function
ht_destroy:                             # @ht_destroy
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp14:
	.cfi_def_cfa_offset 16
.Ltmp15:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	xorq	%rsi, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	obstack_free
	movq	88(%rbx), %rdi
	callq	free
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.Ltmp16:
	.size	ht_destroy, .Ltmp16-ht_destroy
	.cfi_endproc

	.globl	ht_lookup
	.align	16, 0x90
	.type	ht_lookup,@function
ht_lookup:                              # @ht_lookup
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp23:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp24:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp25:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp26:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp27:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp28:
	.cfi_def_cfa_offset 96
.Ltmp29:
	.cfi_offset %rbx, -48
.Ltmp30:
	.cfi_offset %r12, -40
.Ltmp31:
	.cfi_offset %r13, -32
.Ltmp32:
	.cfi_offset %r14, -24
.Ltmp33:
	.cfi_offset %r15, -16
	movq	%rdi, %r8
	xorq	%rax, %rax
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rdx
	je	.LBB3_1
# BB#2:
	movq	%rcx, 8(%rsp)           # 8-byte Spill
	movq	%rdx, %rcx
	movq	%rsi, %rbx
	.align	16, 0x90
.LBB3_3:                                # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	decq	%rcx
	movzbl	(%rbx), %edi
	imulq	$67, %rax, %rax
	testq	%r12, %rcx
	leaq	1(%rbx), %rbx
	leaq	-113(%rax,%rdi), %rax
	jne	.LBB3_3
	jmp	.LBB3_4
.LBB3_1:
	movq	%rcx, 8(%rsp)           # 8-byte Spill
.LBB3_4:                                # %calc_hash.exit
	movl	120(%r8), %ecx
	addq	%rdx, %rax
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movl	104(%r8), %edi
	decq	%rdi
	movq	%rdi, 32(%rsp)          # 8-byte Spill
	incq	%rcx
	movl	%ecx, 120(%r8)
	movq	%rax, %r15
	movq	88(%r8), %rbx
	andq	%rdi, %r15
	movq	%r15, %r14
	andq	%r12, %r14
	movq	(%rbx,%r14,8), %rcx
	testq	%rcx, %rcx
	je	.LBB3_14
# BB#5:                                 # %if.end.lr.ph
	imulq	$17, %rax, %r9
	andq	%rdi, %r9
	orq	$1, %r9
	movq	16(%rsp), %rdx          # 8-byte Reload
	andq	%r12, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB3_6:                                # %if.end
                                        # =>This Inner Loop Header: Depth=1
	movl	(%rcx), %eax
	cmpq	%rdx, %rax
	jne	.LBB3_13
# BB#7:                                 # %land.lhs.true
                                        #   in Loop: Header=BB3_6 Depth=1
	movq	8(%rcx), %rdi
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	%r8, %r14
	movq	%rsi, %r13
	movq	%r9, %r12
	callq	memcmp
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%r12, %r9
	movq	32(%rsp), %rdi          # 8-byte Reload
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%r13, %rsi
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	%r14, %r8
	testq	%r12, %rax
	je	.LBB3_8
.LBB3_13:                               # %if.end25
                                        #   in Loop: Header=BB3_6 Depth=1
	movl	124(%r8), %eax
	addq	%r9, %r15
	incq	%rax
	andq	%rdi, %r15
	movl	%eax, 124(%r8)
	movq	%r15, %r14
	andq	%r12, %r14
	movq	(%rbx,%r14,8), %rcx
	testq	%rcx, %rcx
	jne	.LBB3_6
.LBB3_14:                               # %for.end
	movq	%rsi, %r13
	xorq	%r10, %r10
	movq	8(%rsp), %rbx           # 8-byte Reload
	testq	%r12, %rbx
	je	.LBB3_35
# BB#15:                                # %if.end33
	movq	%r8, %rdi
	movq	%r8, %r15
	callq	*96(%r15)
	movq	%r15, %r9
	movq	%rax, %r10
	movq	88(%r9), %rax
	movq	%r10, (%rax,%r14,8)
	movq	16(%rsp), %rsi          # 8-byte Reload
	movl	%esi, (%r10)
	andq	%r12, %rbx
	cmpq	$1, %rbx
	jne	.LBB3_23
# BB#16:                                # %if.then41
	movq	%r10, 40(%rsp)          # 8-byte Spill
	movq	24(%r9), %rdi
	movq	32(%r9), %rax
	movq	$32, %rcx
	movq	%rsi, %r15
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	leaq	1(%r15), %r14
	leaq	1(%rdi,%r15), %rcx
	cmpq	%rax, %rcx
	movq	%r13, %rax
	jbe	.LBB3_18
# BB#17:                                # %if.then49
	leaq	1(%rsi), %rsi
	movq	%r9, %rdi
	movq	%r9, %rbx
	movq	%rax, %r13
	callq	_obstack_newchunk
	movq	%r13, %rax
	movq	%rbx, %r9
	movq	24(%r9), %rdi
.LBB3_18:                               # %if.end51
	movq	%rax, %rsi
	movq	%r15, %rdx
	movq	%r9, %rbx
	callq	memcpy
	movq	%rbx, %r9
	movq	24(%r9), %rax
	addq	%rax, %r14
	movq	%r14, 24(%r9)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%r15)
	movq	16(%r9), %r8
	movq	24(%r9), %rdx
	cmpq	%r8, %rdx
	jne	.LBB3_20
# BB#19:                                # %if.then64
	movzbl	80(%r9), %ecx
	orq	$2, %rcx
	movb	%cl, 80(%r9)
.LBB3_20:                               # %if.end65
	movslq	48(%r9), %rcx
	addq	%rcx, %rdx
	notq	%rcx
	andq	%rdx, %rcx
	movq	%rcx, 24(%r9)
	movq	32(%r9), %rax
	movq	8(%r9), %rsi
	movq	%rcx, %rdi
	subq	%rsi, %rdi
	movq	%rax, %rdx
	subq	%rsi, %rdx
	cmpq	%rdx, %rdi
	jle	.LBB3_22
# BB#21:                                # %if.then85
	movq	%rax, 24(%r9)
	movq	%rax, %rcx
.LBB3_22:                               # %if.end88
	movq	%rcx, 16(%r9)
	movq	40(%rsp), %r10          # 8-byte Reload
	movq	%r8, 8(%r10)
	jmp	.LBB3_24
.LBB3_23:                               # %if.else93
	movq	%r13, 8(%r10)
.LBB3_24:                               # %if.end95
	movl	108(%r9), %eax
	incq	%rax
	movl	%eax, 108(%r9)
	movl	104(%r9), %r14d
	andq	$1073741823, %rax       # imm = 0x3FFFFFFF
	shlq	$2, %rax
	leaq	(%r14,%r14,2), %rcx
	andq	%r12, %rcx
	cmpq	%rcx, %rax
	jb	.LBB3_35
# BB#25:                                # %if.then102
	movq	%r10, 40(%rsp)          # 8-byte Spill
	leaq	(%r14,%r14), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movabsq	$4294967294, %rdi       # imm = 0xFFFFFFFE
	andq	%rax, %rdi
	movq	$8, %rsi
	movq	%r9, %rbx
	callq	xcalloc
	movq	88(%rbx), %rdi
	movl	104(%rbx), %ecx
	movq	%rbx, %r13
	movq	%rax, %r15
	leaq	-1(%r14,%r14), %rax
	leaq	(%rdi,%rcx,8), %r8
	movq	%rdi, %r11
	.align	16, 0x90
.LBB3_26:                               # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB3_29 Depth 2
                                        #     Child Loop BB3_31 Depth 2
	movq	(%r11), %r10
	testq	%r10, %r10
	je	.LBB3_33
# BB#27:                                # %if.then.i
                                        #   in Loop: Header=BB3_26 Depth=1
	movl	(%r10), %r9d
	xorq	%rcx, %rcx
	testq	%r9, %r9
	je	.LBB3_30
# BB#28:                                # %while.body.i.preheader.i
                                        #   in Loop: Header=BB3_26 Depth=1
	movq	8(%r10), %rsi
	xorq	%rcx, %rcx
	movq	%r9, %rdx
	.align	16, 0x90
.LBB3_29:                               # %while.body.i.i
                                        #   Parent Loop BB3_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	decq	%rdx
	movzbl	(%rsi), %ebx
	imulq	$67, %rcx, %rcx
	testq	%r12, %rdx
	leaq	1(%rsi), %rsi
	leaq	-113(%rcx,%rbx), %rcx
	jne	.LBB3_29
.LBB3_30:                               # %calc_hash.exit.i
                                        #   in Loop: Header=BB3_26 Depth=1
	addq	%r9, %rcx
	imulq	$17, %rcx, %rdx
	andq	%rax, %rdx
	orq	$1, %rdx
	.align	16, 0x90
.LBB3_31:                               # %for.cond.i
                                        #   Parent Loop BB3_26 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	andq	%rax, %rcx
	movq	%rcx, %rsi
	andq	%r12, %rsi
	addq	%rdx, %rcx
	cmpq	$0, (%r15,%rsi,8)
	jne	.LBB3_31
# BB#32:                                # %if.then6.i
                                        #   in Loop: Header=BB3_26 Depth=1
	movq	%r10, (%r15,%rsi,8)
.LBB3_33:                               # %do.cond.i
                                        #   in Loop: Header=BB3_26 Depth=1
	addq	$8, %r11
	cmpq	%r8, %r11
	jb	.LBB3_26
# BB#34:                                # %ht_expand.exit
	callq	free
	movq	%r15, 88(%r13)
	movq	32(%rsp), %rax          # 8-byte Reload
	movl	%eax, 104(%r13)
	movq	40(%rsp), %r10          # 8-byte Reload
	jmp	.LBB3_35
.LBB3_8:                                # %if.then13
	movq	8(%rsp), %rax           # 8-byte Reload
	andq	%r12, %rax
	cmpq	$2, %rax
	jne	.LBB3_35
# BB#9:                                 # %if.then16
	movq	8(%r8), %rax
	cmpq	%rsi, %rax
	jae	.LBB3_12
# BB#10:                                # %land.lhs.true19
	movq	32(%r8), %rax
	cmpq	%rsi, %rax
	jbe	.LBB3_12
# BB#11:                                # %if.then22
	movq	%rsi, 16(%r8)
	movq	%rsi, 24(%r8)
	jmp	.LBB3_35
.LBB3_12:                               # %if.else
	movq	%r8, %rdi
	movq	%r10, %rbx
	callq	obstack_free
	movq	%rbx, %r10
.LBB3_35:                               # %return
	movq	%r10, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp34:
	.size	ht_lookup, .Ltmp34-ht_lookup
	.cfi_endproc

	.globl	ht_forall
	.align	16, 0x90
	.type	ht_forall,@function
ht_forall:                              # @ht_forall
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp41:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp42:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp43:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp44:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp45:
	.cfi_def_cfa_offset 48
.Ltmp46:
	.cfi_offset %rbx, -48
.Ltmp47:
	.cfi_offset %r12, -40
.Ltmp48:
	.cfi_offset %r13, -32
.Ltmp49:
	.cfi_offset %r14, -24
.Ltmp50:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	88(%r12), %rbx
	movl	104(%r12), %eax
	movq	%rdx, %r14
	movq	%rsi, %r15
	leaq	(%rbx,%rax,8), %r13
	.align	16, 0x90
.LBB4_1:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rsi
	testq	%rsi, %rsi
	je	.LBB4_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB4_1 Depth=1
	movq	112(%r12), %rdi
	movq	%r14, %rdx
	callq	*%r15
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB4_4
.LBB4_3:                                # %do.cond
                                        #   in Loop: Header=BB4_1 Depth=1
	addq	$8, %rbx
	cmpq	%r13, %rbx
	jb	.LBB4_1
.LBB4_4:                                # %do.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp51:
	.size	ht_forall, .Ltmp51-ht_forall
	.cfi_endproc

	.globl	ht_dump_statistics
	.align	16, 0x90
	.type	ht_dump_statistics,@function
ht_dump_statistics:                     # @ht_dump_statistics
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp58:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp59:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp60:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp61:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp62:
	.cfi_def_cfa_offset 48
	subq	$64, %rsp
.Ltmp63:
	.cfi_def_cfa_offset 112
.Ltmp64:
	.cfi_offset %rbx, -48
.Ltmp65:
	.cfi_offset %r12, -40
.Ltmp66:
	.cfi_offset %r13, -32
.Ltmp67:
	.cfi_offset %r14, -24
.Ltmp68:
	.cfi_offset %r15, -16
	movq	88(%rdi), %rax
	movl	104(%rdi), %ecx
	movq	%rdi, %r8
	leaq	(%rax,%rcx,8), %r9
	xorq	%rcx, %rcx
	xorq	%rbx, %rbx
	xorq	%r12, %r12
	xorq	%r14, %r14
	.align	16, 0x90
.LBB5_1:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rax), %rdx
	testq	%rdx, %rdx
	je	.LBB5_5
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movl	(%rdx), %edi
	movq	%rdi, %rdx
	imulq	%rdx, %rdx
	cmpq	%rbx, %rdi
	movq	%rdi, %rsi
	ja	.LBB5_4
# BB#3:                                 # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	movq	%rbx, %rsi
.LBB5_4:                                # %if.then
                                        #   in Loop: Header=BB5_1 Depth=1
	addq	%rdi, %r12
	addq	%rdx, %rcx
	incq	%r14
	movq	%rsi, %rbx
.LBB5_5:                                # %do.cond
                                        #   in Loop: Header=BB5_1 Depth=1
	addq	$8, %rax
	cmpq	%r9, %rax
	jb	.LBB5_1
# BB#6:                                 # %do.end
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	%r8, %r13
	movq	%r13, 56(%rsp)          # 8-byte Spill
	movl	108(%r13), %r15d
	movq	%r13, %rdi
	callq	_obstack_memory_used
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movl	104(%r13), %edx
	movq	%rdx, (%rsp)            # 8-byte Spill
	sarq	%cl, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	stderr(%rip), %rdi
	movq	$3, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rdi, %rbx
	movq	%r15, %rdx
	callq	fprintf
	movq	%r14, %rdi
	callq	int64_to_float64
	movabsq	$4636737291354636288, %rsi # imm = 0x4059000000000000
	movq	%rax, %rdi
	callq	float64_mul
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	int32_to_float64
	movq	%rax, 48(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	fprintf
	movl	104(%r13), %edx
	movabsq	$.L.str2, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdi, %r14
	callq	fprintf
	cmpq	$10240, %r12            # imm = 0x2800
	movq	%r12, %rdx
	movq	$32, %rbx
	jb	.LBB5_12
# BB#7:                                 # %cond.false
	cmpq	$10485760, %r12         # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB5_9
# BB#8:                                 # %cond.false
	movq	$20, %rcx
.LBB5_9:                                # %cond.false
	movq	$107, %rbx
	jb	.LBB5_11
# BB#10:                                # %cond.false
	movq	$77, %rbx
.LBB5_11:                               # %cond.false
	movq	%r12, %rdx
	shrq	%cl, %rdx
.LBB5_12:                               # %cond.end37
	movq	32(%rsp), %r8           # 8-byte Reload
	subq	%r12, %r8
	cmpq	$10240, %r8             # imm = 0x2800
	jae	.LBB5_14
# BB#13:
	movq	$32, %r9
	jmp	.LBB5_19
.LBB5_14:                               # %cond.false42
	cmpq	$10485760, %r8          # imm = 0xA00000
	movq	$10, %rcx
	jb	.LBB5_16
# BB#15:                                # %cond.false42
	movq	$20, %rcx
.LBB5_16:                               # %cond.false42
	movq	$107, %r9
	jb	.LBB5_18
# BB#17:                                # %cond.false42
	movq	$77, %r9
.LBB5_18:                               # %cond.false42
	shrq	%cl, %r8
.LBB5_19:                               # %cond.end60
	movabsq	$.L.str3, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rcx
	callq	fprintf
	movq	$32, %r8
	movq	40(%rsp), %rdx          # 8-byte Reload
	cmpq	$10240, %rdx            # imm = 0x2800
	jb	.LBB5_25
# BB#20:                                # %cond.false66
	cmpq	$10485760, %rdx         # imm = 0xA00000
	movq	$7, %rcx
	jb	.LBB5_22
# BB#21:                                # %cond.false66
	movq	$17, %rcx
.LBB5_22:                               # %cond.false66
	movq	$107, %r8
	jb	.LBB5_24
# BB#23:                                # %cond.false66
	movq	$77, %r8
.LBB5_24:                               # %cond.false66
	movq	(%rsp), %rax            # 8-byte Reload
	shrq	%cl, %rax
	movq	%rax, %rdx
.LBB5_25:                               # %cond.end84
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r8, %rcx
	callq	fprintf
	movq	%r12, %rdi
	callq	int64_to_float64
	movq	%rax, %rdi
	movq	48(%rsp), %r15          # 8-byte Reload
	movq	%r15, %rsi
	callq	float64_div
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	int64_to_float64
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_div
	movq	56(%rsp), %rbx          # 8-byte Reload
	movl	124(%rbx), %edi
	movq	%rax, %r12
	callq	int32_to_float64
	movl	120(%rbx), %edi
	movq	%rax, %r13
	callq	int32_to_float64
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str5, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movl	120(%rbx), %edi
	callq	int32_to_float64
	movq	%r15, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	movabsq	$.L.str6, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rcx, %rdx
	callq	fprintf
	movq	%r12, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	float64_sub
	movq	%rax, %r15
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r12
	movabsq	$1, %r13
	movq	%r12, %rbx
	je	.LBB5_27
# BB#26:                                # %cond.end84
	movq	%r13, %rbx
.LBB5_27:                               # %cond.end84
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_29
# BB#28:                                # %cond.end84
	movq	%r13, %r12
.LBB5_29:                               # %cond.end84
	orq	%r12, %rbx
	je	.LBB5_34
# BB#30:                                # %if.end.i
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r15, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB5_33
# BB#31:
	movabsq	$4547007122018943789, %r13 # imm = 0x3F1A36E2EB1C432D
	movq	%r15, %rbx
	.align	16, 0x90
.LBB5_32:                               # %do.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	float64_sub
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r13, %rsi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB5_32
.LBB5_33:                               # %approx_sqrt.exit
	movabsq	$.L.str7, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	32(%rsp), %rdx          # 8-byte Reload
	movq	%rbx, %rcx
	callq	fprintf
	movabsq	$.L.str8, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	8(%rsp), %rdx           # 8-byte Reload
	addq	$64, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	fprintf  # TAILCALL
.LBB5_34:                               # %if.then.i
	callq	abort
.Ltmp69:
	.size	ht_dump_statistics, .Ltmp69-ht_dump_statistics
	.cfi_endproc

	.globl	approx_sqrt
	.align	16, 0x90
	.type	approx_sqrt,@function
approx_sqrt:                            # @approx_sqrt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp75:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp76:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp77:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp78:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp79:
	.cfi_def_cfa_offset 48
.Ltmp80:
	.cfi_offset %rbx, -40
.Ltmp81:
	.cfi_offset %r12, -32
.Ltmp82:
	.cfi_offset %r14, -24
.Ltmp83:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	xorq	%rsi, %rsi
                                        # kill: RDI<def> R14<kill>
	callq	__unorddf2
	testq	%rax, %rax
	movabsq	$0, %r15
	movabsq	$1, %r12
	movq	%r15, %rbx
	je	.LBB6_2
# BB#1:                                 # %entry
	movq	%r12, %rbx
.LBB6_2:                                # %entry
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	__gedf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB6_4
# BB#3:                                 # %entry
	movq	%r12, %r15
.LBB6_4:                                # %entry
	orq	%r15, %rbx
	je	.LBB6_9
# BB#5:                                 # %if.end
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	__eqdf2
	testq	%rax, %rax
	je	.LBB6_8
# BB#6:
	movabsq	$4547007122018943789, %r15 # imm = 0x3F1A36E2EB1C432D
	movq	%r14, %rbx
	.align	16, 0x90
.LBB6_7:                                # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_sub
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	float64_add
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %r12
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	float64_sub
	movq	%rax, %rbx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	__gtdf2
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB6_7
.LBB6_8:                                # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB6_9:                                # %if.then
	callq	abort
.Ltmp84:
	.size	approx_sqrt, .Ltmp84-approx_sqrt
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\nString pool\nentries\t\t%lu\n"
	.size	.L.str, 27

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"identifiers\t%lu (%.2f%%)\n"
	.size	.L.str1, 26

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"slots\t\t%lu\n"
	.size	.L.str2, 12

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"bytes\t\t%lu%c (%lu%c overhead)\n"
	.size	.L.str3, 31

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"table size\t%lu%c\n"
	.size	.L.str4, 18

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"coll/search\t%.4f\n"
	.size	.L.str5, 18

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"ins/search\t%.4f\n"
	.size	.L.str6, 17

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"avg. entry\t%.2f bytes (+/- %.2f)\n"
	.size	.L.str7, 34

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"longest entry\t%lu\n"
	.size	.L.str8, 19


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
