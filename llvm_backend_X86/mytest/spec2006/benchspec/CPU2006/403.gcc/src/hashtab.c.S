	.file	"hashtab.c.bc"
	.text
	.align	16, 0x90
	.type	hash_pointer,@function
hash_pointer:                           # @hash_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	movq	$3, %rcx
	shrq	%cl, %rdi
	movq	%rdi, %rax
	retq
.Ltmp0:
	.size	hash_pointer, .Ltmp0-hash_pointer
	.cfi_endproc

	.align	16, 0x90
	.type	eq_pointer,@function
eq_pointer:                             # @eq_pointer
	.cfi_startproc
# BB#0:                                 # %entry
	cmpq	%rsi, %rdi
	jne	.LBB1_1
# BB#2:                                 # %entry
	movabsq	$1, %rax
	retq
.LBB1_1:
	movabsq	$0, %rax
	retq
.Ltmp1:
	.size	eq_pointer, .Ltmp1-eq_pointer
	.cfi_endproc

	.globl	htab_create
	.align	16, 0x90
	.type	htab_create,@function
htab_create:                            # @htab_create
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp8:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp9:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp10:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp11:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp12:
	.cfi_def_cfa_offset 48
.Ltmp13:
	.cfi_offset %rbx, -48
.Ltmp14:
	.cfi_offset %r12, -40
.Ltmp15:
	.cfi_offset %r13, -32
.Ltmp16:
	.cfi_offset %r14, -24
.Ltmp17:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rdx, %r15
	movq	%rsi, %r12
	movq	%rdi, %rdx
	movabsq	$higher_prime_number.primes+240, %rsi
	movabsq	$higher_prime_number.primes, %rax
	.align	16, 0x90
.LBB2_2:                                # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	subq	%rax, %rdi
	movq	$3, %rcx
	movq	%rdi, %rbx
	sarq	%cl, %rbx
	movq	$63, %rcx
	shrq	%cl, %rdi
	addq	%rbx, %rdi
	movq	$1, %rcx
	sarq	%cl, %rdi
	movq	(%rax,%rdi,8), %rcx
	cmpq	%rdx, %rcx
	jae	.LBB2_1
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB2_2 Depth=1
	leaq	8(%rax,%rdi,8), %rax
	cmpq	%rsi, %rax
	jne	.LBB2_2
	jmp	.LBB2_4
.LBB2_1:                                # %while.cond.outer.loopexit.i
                                        #   in Loop: Header=BB2_2 Depth=1
	leaq	(%rax,%rdi,8), %rsi
	incq	%rbx
	cmpq	$3, %rbx
	jae	.LBB2_2
	jmp	.LBB2_5
.LBB2_4:
	movq	%rsi, %rax
.LBB2_5:                                # %while.end.i
	movq	(%rax), %r13
	cmpq	%rdx, %r13
	jb	.LBB2_7
# BB#6:                                 # %higher_prime_number.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	xcalloc
	movq	%rax, %rbx
	movq	$8, %rsi
	movq	%r13, %rdi
	callq	xcalloc
	movq	%rax, 24(%rbx)
	movq	%r13, 32(%rbx)
	movq	%r12, (%rbx)
	movq	%r15, 8(%rbx)
	movq	%r14, 16(%rbx)
	xorq	%rax, %rax
	movl	%eax, 64(%rbx)
	movq	%rbx, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB2_7:                                # %if.then4.i
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	callq	abort
.Ltmp18:
	.size	htab_create, .Ltmp18-htab_create
	.cfi_endproc

	.globl	htab_try_create
	.align	16, 0x90
	.type	htab_try_create,@function
htab_try_create:                        # @htab_try_create
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp25:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp26:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp27:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp28:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp29:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp30:
	.cfi_def_cfa_offset 64
.Ltmp31:
	.cfi_offset %rbx, -48
.Ltmp32:
	.cfi_offset %r12, -40
.Ltmp33:
	.cfi_offset %r13, -32
.Ltmp34:
	.cfi_offset %r14, -24
.Ltmp35:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%rdi, %r9
	movabsq	$higher_prime_number.primes+240, %r10
	movabsq	$higher_prime_number.primes, %rax
	.align	16, 0x90
.LBB3_2:                                # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%r10, %rdi
	subq	%rax, %rdi
	movq	$3, %rcx
	movq	%rdi, %rbx
	sarq	%cl, %rbx
	movq	$63, %rcx
	shrq	%cl, %rdi
	addq	%rbx, %rdi
	movq	$1, %rcx
	sarq	%cl, %rdi
	movq	(%rax,%rdi,8), %rcx
	cmpq	%r9, %rcx
	jae	.LBB3_1
# BB#3:                                 # %if.then.i
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	8(%rax,%rdi,8), %rax
	cmpq	%r10, %rax
	jne	.LBB3_2
	jmp	.LBB3_4
.LBB3_1:                                # %while.cond.outer.loopexit.i
                                        #   in Loop: Header=BB3_2 Depth=1
	leaq	(%rax,%rdi,8), %r10
	incq	%rbx
	cmpq	$3, %rbx
	jae	.LBB3_2
	jmp	.LBB3_5
.LBB3_4:
	movq	%r10, %rax
.LBB3_5:                                # %while.end.i
	movq	%rsi, %r15
	movq	%rdx, %r12
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	(%rax), %rbx
	cmpq	%r9, %rbx
	jb	.LBB3_11
# BB#6:                                 # %higher_prime_number.exit
	movq	$1, %rdi
	movq	$72, %rsi
	callq	calloc
	movq	%rax, %r14
	xorq	%r13, %r13
	testq	%r14, %r14
	je	.LBB3_10
# BB#7:                                 # %if.end
	movq	$8, %rsi
	movq	%rbx, %rdi
	callq	calloc
	testq	%rax, %rax
	movq	%rax, 24(%r14)
	je	.LBB3_8
# BB#9:                                 # %if.end6
	movq	%rbx, 32(%r14)
	movq	%r15, (%r14)
	movq	%r12, 8(%r14)
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, 16(%r14)
	movq	$1, %rax
	movl	%eax, 64(%r14)
	movq	%r14, %r13
	jmp	.LBB3_10
.LBB3_8:                                # %if.then5
	movq	%r14, %rdi
	callq	free
.LBB3_10:                               # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB3_11:                               # %if.then4.i
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%r9, %rdx
	callq	fprintf
	callq	abort
.Ltmp36:
	.size	htab_try_create, .Ltmp36-htab_try_create
	.cfi_endproc

	.globl	htab_delete
	.align	16, 0x90
	.type	htab_delete,@function
htab_delete:                            # @htab_delete
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp40:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp41:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp42:
	.cfi_def_cfa_offset 32
.Ltmp43:
	.cfi_offset %rbx, -24
.Ltmp44:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	cmpq	$0, 16(%r14)
	je	.LBB4_6
# BB#1:                                 # %if.then
	movq	32(%r14), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB4_6
# BB#2:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB4_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r14), %rax
	movq	(%rax,%rbx,8), %rdi
	cmpq	$2, %rdi
	jb	.LBB4_5
# BB#4:                                 # %if.then9
                                        #   in Loop: Header=BB4_3 Depth=1
	callq	*16(%r14)
.LBB4_5:                                # %for.inc
                                        #   in Loop: Header=BB4_3 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	jg	.LBB4_3
.LBB4_6:                                # %if.end14
	movq	24(%r14), %rdi
	callq	free
	movq	%r14, %rdi
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	jmp	free  # TAILCALL
.Ltmp45:
	.size	htab_delete, .Ltmp45-htab_delete
	.cfi_endproc

	.globl	htab_empty
	.align	16, 0x90
	.type	htab_empty,@function
htab_empty:                             # @htab_empty
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp49:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp50:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp51:
	.cfi_def_cfa_offset 32
.Ltmp52:
	.cfi_offset %rbx, -24
.Ltmp53:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	cmpq	$0, 16(%r14)
	je	.LBB5_6
# BB#1:                                 # %if.then
	movq	32(%r14), %rax
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	addq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	js	.LBB5_6
# BB#2:                                 # %for.body.lr.ph
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	.align	16, 0x90
.LBB5_3:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	24(%r14), %rax
	movq	(%rax,%rbx,8), %rdi
	cmpq	$2, %rdi
	jb	.LBB5_5
# BB#4:                                 # %if.then9
                                        #   in Loop: Header=BB5_3 Depth=1
	callq	*16(%r14)
.LBB5_5:                                # %for.inc
                                        #   in Loop: Header=BB5_3 Depth=1
	movq	$32, %rcx
	leaq	-1(%rbx), %rax
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	testq	%rbx, %rbx
	movq	%rax, %rbx
	jg	.LBB5_3
.LBB5_6:                                # %if.end14
	movq	32(%r14), %rdx
	movq	24(%r14), %rdi
	movq	$3, %rcx
	shlq	%cl, %rdx
	xorq	%rsi, %rsi
	callq	memset
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp54:
	.size	htab_empty, .Ltmp54-htab_empty
	.cfi_endproc

	.globl	htab_find_with_hash
	.align	16, 0x90
	.type	htab_find_with_hash,@function
htab_find_with_hash:                    # @htab_find_with_hash
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
	movl	56(%r14), %eax
	movq	%rdx, %r13
	incq	%rax
	movl	%eax, 56(%r14)
	movq	24(%r14), %rcx
	movq	32(%r14), %rdi
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %r13
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rdi
	movq	%rdx, %rbx
	movq	(%rcx,%rbx,8), %r12
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	testq	%r12, %r12
	je	.LBB6_11
# BB#1:                                 # %entry
	movq	%rdi, 16(%rsp)          # 8-byte Spill
	cmpq	$1, %r12
	jne	.LBB6_3
# BB#2:                                 # %entry.if.end_crit_edge
	leaq	8(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB6_5
.LBB6_3:                                # %land.lhs.true
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	*8(%r14)
	testq	%r15, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB6_11
# BB#4:
	leaq	8(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB6_5:                                # %if.end
	leaq	-2(%rsi), %rcx
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rcx
	movq	%rdx, %r13
	incq	%r13
	.align	16, 0x90
.LBB6_6:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	60(%r14), %eax
	incq	%rax
	addq	%r13, %rbx
	movl	%eax, 60(%r14)
	movq	%rbx, %rax
	andq	%r15, %rax
	subq	%rsi, %rax
	jb	.LBB6_8
# BB#7:                                 # %if.then15
                                        #   in Loop: Header=BB6_6 Depth=1
	movq	%rax, %rbx
.LBB6_8:                                # %if.end19
                                        #   in Loop: Header=BB6_6 Depth=1
	movq	24(%r14), %rax
	movq	%rbx, %rcx
	andq	%r15, %rcx
	movq	(%rax,%rcx,8), %r12
	cmpq	$1, %r12
	je	.LBB6_6
# BB#9:                                 # %if.end19
                                        #   in Loop: Header=BB6_6 Depth=1
	testq	%r12, %r12
	je	.LBB6_11
# BB#10:                                # %land.lhs.true28
                                        #   in Loop: Header=BB6_6 Depth=1
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	callq	*(%rax)
	testq	%r15, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	je	.LBB6_6
.LBB6_11:                               # %return
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	htab_find_with_hash, .Ltmp72-htab_find_with_hash
	.cfi_endproc

	.globl	htab_find
	.align	16, 0x90
	.type	htab_find,@function
htab_find:                              # @htab_find
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp79:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp80:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp81:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp84:
	.cfi_def_cfa_offset 80
.Ltmp85:
	.cfi_offset %rbx, -48
.Ltmp86:
	.cfi_offset %r12, -40
.Ltmp87:
	.cfi_offset %r13, -32
.Ltmp88:
	.cfi_offset %r14, -24
.Ltmp89:
	.cfi_offset %r15, -16
	movq	%rsi, 24(%rsp)          # 8-byte Spill
	movq	%rdi, %r15
	movq	%rsi, %rdi
	callq	*(%r15)
	movl	56(%r15), %ecx
	movq	%rax, %r13
	incq	%rcx
	movl	%ecx, 56(%r15)
	movq	24(%r15), %rcx
	movq	32(%r15), %rsi
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	andq	%r14, %r13
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rsi
	movq	%rdx, %rbx
	movq	(%rcx,%rbx,8), %r12
	testq	%r12, %r12
	je	.LBB7_11
# BB#1:                                 # %entry
	movq	%rsi, 16(%rsp)          # 8-byte Spill
	cmpq	$1, %r12
	jne	.LBB7_3
# BB#2:                                 # %entry.if.end_crit_edge.i
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	16(%rsp), %rsi          # 8-byte Reload
	jmp	.LBB7_5
.LBB7_3:                                # %land.lhs.true.i
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	*8(%r15)
	testq	%r14, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	jne	.LBB7_11
# BB#4:
	leaq	8(%r15), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
.LBB7_5:                                # %if.end.i
	leaq	-2(%rsi), %rcx
	xorq	%rdx, %rdx
	movq	%r13, %rax
	divq	%rcx
	movq	%rdx, %r13
	incq	%r13
	.align	16, 0x90
.LBB7_6:                                # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movl	60(%r15), %eax
	incq	%rax
	addq	%r13, %rbx
	movl	%eax, 60(%r15)
	movq	%rbx, %rax
	andq	%r14, %rax
	subq	%rsi, %rax
	jb	.LBB7_8
# BB#7:                                 # %if.then15.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%rax, %rbx
.LBB7_8:                                # %if.end19.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	24(%r15), %rax
	movq	%rbx, %rcx
	andq	%r14, %rcx
	movq	(%rax,%rcx,8), %r12
	cmpq	$1, %r12
	je	.LBB7_6
# BB#9:                                 # %if.end19.i
                                        #   in Loop: Header=BB7_6 Depth=1
	testq	%r12, %r12
	je	.LBB7_11
# BB#10:                                # %land.lhs.true28.i
                                        #   in Loop: Header=BB7_6 Depth=1
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	callq	*(%rax)
	testq	%r14, %rax
	movq	16(%rsp), %rsi          # 8-byte Reload
	je	.LBB7_6
.LBB7_11:                               # %htab_find_with_hash.exit
	movq	%r12, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp90:
	.size	htab_find, .Ltmp90-htab_find
	.cfi_endproc

	.globl	htab_find_slot_with_hash
	.align	16, 0x90
	.type	htab_find_slot_with_hash,@function
htab_find_slot_with_hash:               # @htab_find_slot_with_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp97:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp98:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp99:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp100:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp101:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp102:
	.cfi_def_cfa_offset 96
.Ltmp103:
	.cfi_offset %rbx, -48
.Ltmp104:
	.cfi_offset %r12, -40
.Ltmp105:
	.cfi_offset %r13, -32
.Ltmp106:
	.cfi_offset %r14, -24
.Ltmp107:
	.cfi_offset %r15, -16
	movq	%rcx, %r8
	movq	%rdx, %r9
	movq	%rsi, %r15
	movq	%rdi, %r14
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	%r8, %rax
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB8_18
# BB#1:                                 # %land.lhs.true
	movq	40(%r14), %r12
	movq	32(%r14), %rdx
	leaq	(%rdx,%rdx,2), %rax
	movq	$2, %rcx
	movq	%r12, %rsi
	shlq	%cl, %rsi
	cmpq	%rsi, %rax
	ja	.LBB8_18
# BB#2:                                 # %land.lhs.true5
	movq	%r15, 32(%rsp)          # 8-byte Spill
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	24(%r14), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	(%rax,%rdx,8), %r15
	addq	%rdx, %rdx
	movabsq	$higher_prime_number.primes+240, %rsi
	movabsq	$higher_prime_number.primes, %rax
	.align	16, 0x90
.LBB8_4:                                # %while.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	%rsi, %rdi
	subq	%rax, %rdi
	movq	$3, %rcx
	movq	%rdi, %rbx
	sarq	%cl, %rbx
	movq	$63, %rcx
	shrq	%cl, %rdi
	addq	%rbx, %rdi
	movq	$1, %rcx
	sarq	%cl, %rdi
	movq	(%rax,%rdi,8), %rcx
	cmpq	%rdx, %rcx
	jae	.LBB8_3
# BB#5:                                 # %if.then.i.i
                                        #   in Loop: Header=BB8_4 Depth=1
	leaq	8(%rax,%rdi,8), %rax
	cmpq	%rsi, %rax
	jne	.LBB8_4
	jmp	.LBB8_6
.LBB8_3:                                # %while.cond.outer.loopexit.i.i
                                        #   in Loop: Header=BB8_4 Depth=1
	leaq	(%rax,%rdi,8), %rsi
	incq	%rbx
	cmpq	$3, %rbx
	jae	.LBB8_4
	jmp	.LBB8_7
.LBB8_6:
	movq	%rsi, %rax
.LBB8_7:                                # %while.end.i.i
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	(%rax), %rdi
	cmpq	%rdx, %rdi
	jb	.LBB8_49
# BB#8:                                 # %higher_prime_number.exit.i
	movq	%rdi, 32(%r14)
	movl	64(%r14), %eax
	movq	$8, %rsi
	testq	%rax, %rax
	je	.LBB8_10
# BB#9:                                 # %if.then.i
	callq	calloc
	xorq	%rsi, %rsi
	testq	%rax, %rax
	jne	.LBB8_11
	jmp	.LBB8_48
.LBB8_10:                               # %if.else.i
	callq	xcalloc
	movq	40(%r14), %r12
.LBB8_11:                               # %if.end10.i
	movq	%rax, 24(%r14)
	movq	48(%r14), %rax
	subq	%rax, %r12
	movq	%r12, 40(%r14)
	movq	$0, 48(%r14)
	movq	16(%rsp), %rbx          # 8-byte Reload
	jmp	.LBB8_12
.LBB8_49:                               # %if.then4.i.i
	movq	stderr(%rip), %rcx
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	jmp	.LBB8_50
.LBB8_23:                               # %if.then14.i
                                        #   in Loop: Header=BB8_12 Depth=1
	movq	%r13, %r10
	cmpq	$1, %rax
	je	.LBB8_50
# BB#24:                                # %if.end7.i.i
                                        #   in Loop: Header=BB8_12 Depth=1
	leaq	-2(%r8), %r9
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%r9
	movq	%rdx, %rax
	incq	%rax
.LBB8_25:                               # %for.cond.i.i
                                        #   Parent Loop BB8_12 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	addq	%rax, %rcx
	movq	%rcx, %rdx
	andq	%r10, %rdx
	subq	%r8, %rdx
	jb	.LBB8_27
# BB#26:                                # %if.then15.i.i
                                        #   in Loop: Header=BB8_25 Depth=2
	movq	%rdx, %rcx
.LBB8_27:                               # %if.end19.i.i
                                        #   in Loop: Header=BB8_25 Depth=2
	movq	%rcx, %rdx
	andq	%r10, %rdx
	movq	(%rdi,%rdx,8), %rsi
	testq	%rsi, %rsi
	je	.LBB8_28
# BB#29:                                # %if.end19.i.i
                                        #   in Loop: Header=BB8_25 Depth=2
	cmpq	$1, %rsi
	jne	.LBB8_25
	jmp	.LBB8_30
.LBB8_28:                               #   in Loop: Header=BB8_12 Depth=1
	leaq	(%rdi,%rdx,8), %rax
	movq	%r10, %r13
	jmp	.LBB8_15
	.align	16, 0x90
.LBB8_12:                               # %do.body.i
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_25 Depth 2
	movq	(%rbx), %r12
	cmpq	$2, %r12
	jb	.LBB8_16
# BB#13:                                # %if.then14.i
                                        #   in Loop: Header=BB8_12 Depth=1
	movq	%r12, %rdi
	callq	*(%r14)
	movq	%rax, %rsi
	movq	32(%r14), %r8
	movq	24(%r14), %rdi
	andq	%r13, %rsi
	xorq	%rdx, %rdx
	movq	%rsi, %rax
	divq	%r8
	movq	%rdx, %rcx
	movq	(%rdi,%rcx,8), %rax
	testq	%rax, %rax
	jne	.LBB8_23
# BB#14:                                #   in Loop: Header=BB8_12 Depth=1
	leaq	(%rdi,%rcx,8), %rax
.LBB8_15:                               # %find_empty_slot_for_expand.exit.i
                                        #   in Loop: Header=BB8_12 Depth=1
	movq	%r12, (%rax)
.LBB8_16:                               # %if.end17.i
                                        #   in Loop: Header=BB8_12 Depth=1
	addq	$8, %rbx
	cmpq	%r15, %rbx
	jb	.LBB8_12
# BB#17:                                # %htab_expand.exit
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	free
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	24(%rsp), %r9           # 8-byte Reload
.LBB8_18:                               # %if.end
	movq	32(%r14), %r12
	andq	%r13, %r9
	movq	%r13, %rbx
	xorq	%rdx, %rdx
	movq	%r9, %rax
	divq	%r12
	movq	%rdx, %r13
	movl	56(%r14), %eax
	incq	%rax
	movl	%eax, 56(%r14)
	movq	24(%r14), %rax
	movq	(%rax,%r13,8), %rdi
	xorq	%rsi, %rsi
	testq	%rdi, %rdi
	jne	.LBB8_31
# BB#19:
	xorq	%rcx, %rcx
	jmp	.LBB8_20
.LBB8_31:                               # %if.end
	movq	%r8, 40(%rsp)           # 8-byte Spill
	cmpq	$1, %rdi
	jne	.LBB8_33
# BB#32:                                # %if.end.if.end26_crit_edge
	leaq	(%rax,%r13,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	8(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB8_35
.LBB8_33:                               # %if.else18
	movq	%r9, 24(%rsp)           # 8-byte Spill
	movq	%r15, %rsi
	callq	*8(%r14)
	testq	%rbx, %rax
	je	.LBB8_34
# BB#51:                                # %if.then20
	movq	24(%r14), %rax
	jmp	.LBB8_47
.LBB8_34:
	movq	$0, 16(%rsp)            # 8-byte Folded Spill
	leaq	8(%r14), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	24(%rsp), %r9           # 8-byte Reload
	xorq	%rsi, %rsi
.LBB8_35:                               # %if.end26
	movq	%r15, 32(%rsp)          # 8-byte Spill
	leaq	-2(%r12), %rcx
	xorq	%rdx, %rdx
	movq	%r9, %rax
	divq	%rcx
	incq	%rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB8_36
.LBB8_43:                               # %if.then49
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	24(%rsp), %rdx          # 8-byte Reload
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	xorq	%rsi, %rsi
	.align	16, 0x90
.LBB8_36:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	60(%r14), %eax
	incq	%rax
	addq	%rdx, %r13
	movl	%eax, 60(%r14)
	movq	%r13, %rax
	andq	%rbx, %rax
	subq	%r12, %rax
	jb	.LBB8_38
# BB#37:                                # %if.then35
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	%rax, %r13
.LBB8_38:                               # %if.end39
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	24(%r14), %rax
	movq	%r13, %r15
	andq	%rbx, %r15
	movq	(%rax,%r15,8), %rdi
	testq	%rdi, %rdi
	je	.LBB8_39
# BB#40:                                # %if.end39
                                        #   in Loop: Header=BB8_36 Depth=1
	cmpq	$1, %rdi
	je	.LBB8_41
# BB#44:                                # %if.else56
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	movq	8(%rsp), %rax           # 8-byte Reload
	callq	*(%rax)
	xorq	%rsi, %rsi
	testq	%rbx, %rax
	movq	24(%rsp), %rdx          # 8-byte Reload
	je	.LBB8_36
	jmp	.LBB8_45
.LBB8_41:                               # %if.then49
                                        #   in Loop: Header=BB8_36 Depth=1
	movq	16(%rsp), %rcx          # 8-byte Reload
	testq	%rcx, %rcx
	jne	.LBB8_43
# BB#42:                                #   in Loop: Header=BB8_36 Depth=1
	leaq	(%rax,%r15,8), %rcx
	jmp	.LBB8_43
.LBB8_39:
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	16(%rsp), %rcx          # 8-byte Reload
.LBB8_20:                               # %empty_entry
	testq	%rbx, %r8
	je	.LBB8_48
# BB#21:                                # %if.end70
	incq	40(%r14)
	testq	%rcx, %rcx
	je	.LBB8_46
# BB#22:                                # %if.then74
	movq	$0, (%rcx)
	movq	%rcx, %rsi
	jmp	.LBB8_48
.LBB8_46:                               # %if.end75
	andq	%rbx, %r13
.LBB8_47:                               # %return
	leaq	(%rax,%r13,8), %rsi
.LBB8_48:                               # %return
	movq	%rsi, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_45:                               # %if.then60
	movq	24(%r14), %rax
	leaq	(%rax,%r15,8), %rsi
	jmp	.LBB8_48
.LBB8_50:                               # %if.then6.i.i
	callq	abort
.LBB8_30:                               # %if.then29.i.i
	callq	abort
.Ltmp108:
	.size	htab_find_slot_with_hash, .Ltmp108-htab_find_slot_with_hash
	.cfi_endproc

	.globl	htab_find_slot
	.align	16, 0x90
	.type	htab_find_slot,@function
htab_find_slot:                         # @htab_find_slot
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp113:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp114:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp115:
	.cfi_def_cfa_offset 32
.Ltmp116:
	.cfi_offset %rbx, -32
.Ltmp117:
	.cfi_offset %r14, -24
.Ltmp118:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %rbx
	movq	%r15, %rdi
	callq	*(%rbx)
	movq	%rbx, %rdi
	movq	%r15, %rsi
	movq	%rax, %rdx
	movq	%r14, %rcx
	popq	%rbx
	popq	%r14
	popq	%r15
	jmp	htab_find_slot_with_hash  # TAILCALL
.Ltmp119:
	.size	htab_find_slot, .Ltmp119-htab_find_slot
	.cfi_endproc

	.globl	htab_remove_elt
	.align	16, 0x90
	.type	htab_remove_elt,@function
htab_remove_elt:                        # @htab_remove_elt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp126:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp128:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp129:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp130:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp131:
	.cfi_def_cfa_offset 64
.Ltmp132:
	.cfi_offset %rbx, -48
.Ltmp133:
	.cfi_offset %r12, -40
.Ltmp134:
	.cfi_offset %r13, -32
.Ltmp135:
	.cfi_offset %r14, -24
.Ltmp136:
	.cfi_offset %r15, -16
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r15
	movq	%rsi, %rdi
	callq	*(%r15)
	movq	%rax, %r12
	movq	32(%r15), %rsi
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %r12
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rsi
	movq	%rdx, %r14
	movl	56(%r15), %eax
	incq	%rax
	movl	%eax, 56(%r15)
	movq	24(%r15), %rax
	movq	(%rax,%r14,8), %rdi
	xorq	%r13, %r13
	testq	%rdi, %rdi
	je	.LBB10_12
# BB#1:                                 # %entry
	cmpq	$1, %rdi
	jne	.LBB10_3
# BB#2:                                 # %if.end.if.end26_crit_edge.i
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rbx, %rcx
	jmp	.LBB10_5
.LBB10_3:                               # %if.else18.i
	movq	%rsi, %r13
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	*8(%r15)
	movq	%rbx, %rcx
	testq	%rcx, %rax
	je	.LBB10_4
# BB#17:                                # %if.then20.i
	movq	24(%r15), %rax
	leaq	(%rax,%r14,8), %r13
	jmp	.LBB10_12
.LBB10_4:
	leaq	8(%r15), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%r13, %rsi
.LBB10_5:                               # %if.end26.i
	leaq	-2(%rsi), %rdi
	xorq	%rdx, %rdx
	movq	%r12, %rax
	divq	%rdi
	movq	%rdx, %r12
	incq	%r12
	.align	16, 0x90
.LBB10_6:                               # %for.cond.i
                                        # =>This Inner Loop Header: Depth=1
	movl	60(%r15), %eax
	incq	%rax
	addq	%r12, %r14
	movl	%eax, 60(%r15)
	movq	%r14, %rax
	andq	%rcx, %rax
	subq	%rsi, %rax
	jb	.LBB10_8
# BB#7:                                 # %if.then35.i
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%rax, %r14
.LBB10_8:                               # %if.end39.i
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	24(%r15), %rax
	movq	%r14, %rbx
	andq	%rcx, %rbx
	movq	(%rax,%rbx,8), %rdi
	xorq	%r13, %r13
	cmpq	$1, %rdi
	je	.LBB10_6
# BB#9:                                 # %if.end39.i
                                        #   in Loop: Header=BB10_6 Depth=1
	testq	%rdi, %rdi
	je	.LBB10_12
# BB#10:                                # %if.else56.i
                                        #   in Loop: Header=BB10_6 Depth=1
	movq	%rsi, %r13
	movq	8(%rsp), %rsi           # 8-byte Reload
	movq	(%rsp), %rax            # 8-byte Reload
	callq	*(%rax)
	movq	%r13, %rsi
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB10_6
# BB#11:                                # %if.then60.i
	movq	24(%r15), %rax
	leaq	(%rax,%rbx,8), %r13
.LBB10_12:                              # %htab_find_slot_with_hash.exit
	movq	(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB10_16
# BB#13:                                # %if.end
	movq	16(%r15), %rax
	testq	%rax, %rax
	je	.LBB10_15
# BB#14:                                # %if.then1
	callq	*%rax
.LBB10_15:                              # %if.end3
	movq	$1, (%r13)
	incq	48(%r15)
.LBB10_16:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp137:
	.size	htab_remove_elt, .Ltmp137-htab_remove_elt
	.cfi_endproc

	.globl	htab_clear_slot
	.align	16, 0x90
	.type	htab_clear_slot,@function
htab_clear_slot:                        # @htab_clear_slot
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp141:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp142:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp143:
	.cfi_def_cfa_offset 32
.Ltmp144:
	.cfi_offset %rbx, -24
.Ltmp145:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	24(%r14), %rax
	movq	%rsi, %rbx
	cmpq	%rbx, %rax
	ja	.LBB11_6
# BB#1:                                 # %lor.lhs.false
	movq	32(%r14), %rcx
	leaq	(%rax,%rcx,8), %rax
	cmpq	%rbx, %rax
	jbe	.LBB11_6
# BB#2:                                 # %lor.lhs.false3
	movq	(%rbx), %rdi
	cmpq	$1, %rdi
	jbe	.LBB11_6
# BB#3:                                 # %if.end
	movq	16(%r14), %rax
	testq	%rax, %rax
	je	.LBB11_5
# BB#4:                                 # %if.then7
	callq	*%rax
.LBB11_5:                               # %if.end9
	movq	$1, (%rbx)
	incq	48(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.LBB11_6:                               # %if.then
	callq	abort
.Ltmp146:
	.size	htab_clear_slot, .Ltmp146-htab_clear_slot
	.cfi_endproc

	.globl	htab_traverse
	.align	16, 0x90
	.type	htab_traverse,@function
htab_traverse:                          # @htab_traverse
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp153:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp154:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp155:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp156:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp157:
	.cfi_def_cfa_offset 48
.Ltmp158:
	.cfi_offset %rbx, -48
.Ltmp159:
	.cfi_offset %r12, -40
.Ltmp160:
	.cfi_offset %r13, -32
.Ltmp161:
	.cfi_offset %r14, -24
.Ltmp162:
	.cfi_offset %r15, -16
	movq	24(%rdi), %rbx
	movq	32(%rdi), %rax
	movq	%rdx, %r14
	movq	%rsi, %r15
	leaq	(%rbx,%rax,8), %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB12_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	(%rbx), %rax
	cmpq	$2, %rax
	jb	.LBB12_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB12_1 Depth=1
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	*%r15
	testq	%r13, %rax
	je	.LBB12_4
.LBB12_3:                               # %do.cond
                                        #   in Loop: Header=BB12_1 Depth=1
	addq	$8, %rbx
	cmpq	%r12, %rbx
	jb	.LBB12_1
.LBB12_4:                               # %do.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp163:
	.size	htab_traverse, .Ltmp163-htab_traverse
	.cfi_endproc

	.globl	htab_size
	.align	16, 0x90
	.type	htab_size,@function
htab_size:                              # @htab_size
	.cfi_startproc
# BB#0:                                 # %entry
	movq	32(%rdi), %rax
	retq
.Ltmp164:
	.size	htab_size, .Ltmp164-htab_size
	.cfi_endproc

	.globl	htab_elements
	.align	16, 0x90
	.type	htab_elements,@function
htab_elements:                          # @htab_elements
	.cfi_startproc
# BB#0:                                 # %entry
	movq	40(%rdi), %rax
	movq	48(%rdi), %rcx
	subq	%rcx, %rax
	retq
.Ltmp165:
	.size	htab_elements, .Ltmp165-htab_elements
	.cfi_endproc

	.globl	htab_collisions
	.align	16, 0x90
	.type	htab_collisions,@function
htab_collisions:                        # @htab_collisions
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp169:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp170:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp171:
	.cfi_def_cfa_offset 32
.Ltmp172:
	.cfi_offset %rbx, -24
.Ltmp173:
	.cfi_offset %r14, -16
	movl	56(%rdi), %r14d
	xorq	%rax, %rax
	testq	%r14, %r14
	je	.LBB15_2
# BB#1:                                 # %if.end
	movl	60(%rdi), %edi
	callq	int32_to_float64
	movq	%rax, %rbx
	movq	%r14, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
.LBB15_2:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp174:
	.size	htab_collisions, .Ltmp174-htab_collisions
	.cfi_endproc

	.globl	htab_hash_string
	.align	16, 0x90
	.type	htab_hash_string,@function
htab_hash_string:                       # @htab_hash_string
	.cfi_startproc
# BB#0:                                 # %entry
	movzbl	(%rdi), %ecx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB16_3
# BB#1:                                 # %while.body.preheader
	incq	%rdi
	xorq	%rax, %rax
	.align	16, 0x90
.LBB16_2:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	andq	$255, %rcx
	imulq	$67, %rax, %rax
	leaq	-113(%rax,%rcx), %rax
	movzbl	(%rdi), %ecx
	incq	%rdi
	testq	%rcx, %rcx
	jne	.LBB16_2
.LBB16_3:                               # %while.end
	retq
.Ltmp175:
	.size	htab_hash_string, .Ltmp175-htab_hash_string
	.cfi_endproc

	.type	htab_hash_pointer,@object # @htab_hash_pointer
	.data
	.globl	htab_hash_pointer
	.align	8
htab_hash_pointer:
	.quad	hash_pointer
	.size	htab_hash_pointer, 8

	.type	htab_eq_pointer,@object # @htab_eq_pointer
	.globl	htab_eq_pointer
	.align	8
htab_eq_pointer:
	.quad	eq_pointer
	.size	htab_eq_pointer, 8

	.type	higher_prime_number.primes,@object # @higher_prime_number.primes
	.section	.rodata,"a",@progbits
	.align	16
higher_prime_number.primes:
	.quad	7                       # 0x7
	.quad	13                      # 0xd
	.quad	31                      # 0x1f
	.quad	61                      # 0x3d
	.quad	127                     # 0x7f
	.quad	251                     # 0xfb
	.quad	509                     # 0x1fd
	.quad	1021                    # 0x3fd
	.quad	2039                    # 0x7f7
	.quad	4093                    # 0xffd
	.quad	8191                    # 0x1fff
	.quad	16381                   # 0x3ffd
	.quad	32749                   # 0x7fed
	.quad	65521                   # 0xfff1
	.quad	131071                  # 0x1ffff
	.quad	262139                  # 0x3fffb
	.quad	524287                  # 0x7ffff
	.quad	1048573                 # 0xffffd
	.quad	2097143                 # 0x1ffff7
	.quad	4194301                 # 0x3ffffd
	.quad	8388593                 # 0x7ffff1
	.quad	16777213                # 0xfffffd
	.quad	33554393                # 0x1ffffd9
	.quad	67108859                # 0x3fffffb
	.quad	134217689               # 0x7ffffd9
	.quad	268435399               # 0xfffffc7
	.quad	536870909               # 0x1ffffffd
	.quad	1073741789              # 0x3fffffdd
	.quad	2147483647              # 0x7fffffff
	.quad	4294967291              # 0xfffffffb
	.size	higher_prime_number.primes, 240

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Cannot find prime bigger than %lu\n"
	.size	.L.str, 35


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
