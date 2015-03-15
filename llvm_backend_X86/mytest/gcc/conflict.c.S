	.file	"conflict.c.bc"
	.text
	.globl	conflict_graph_new
	.align	16, 0x90
	.type	conflict_graph_new,@function
conflict_graph_new:                     # @conflict_graph_new
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
	movq	%rdi, %rbx
	movq	$112, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movabsq	$xmalloc, %r15
	movl	%ebx, 8(%r14)
	movabsq	$arc_hash, %rsi
	movabsq	$arc_eq, %rdx
	movq	$64, %rdi
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, (%r14)
	leaq	24(%r14), %rdi
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r15, %rcx
	callq	_obstack_begin
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, 16(%r14)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	memset
	movq	%r14, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp10:
	.size	conflict_graph_new, .Ltmp10-conflict_graph_new
	.cfi_endproc

	.align	16, 0x90
	.type	arc_hash,@function
arc_hash:                               # @arc_hash
	.cfi_startproc
# BB#0:                                 # %entry
	movl	20(%rdi), %eax
	leaq	-1(%rax), %rdx
	imulq	%rax, %rdx
	movabsq	$2147483648, %rsi       # imm = 0x80000000
	andq	%rdx, %rsi
	movq	$31, %rcx
	shrq	%cl, %rsi
	addq	%rdx, %rsi
	movq	$32, %rcx
	movl	16(%rdi), %eax
	shlq	%cl, %rsi
	movq	$33, %rcx
	sarq	%cl, %rsi
	addq	%rsi, %rax
	retq
.Ltmp11:
	.size	arc_hash, .Ltmp11-arc_hash
	.cfi_endproc

	.align	16, 0x90
	.type	arc_eq,@function
arc_eq:                                 # @arc_eq
	.cfi_startproc
# BB#0:                                 # %entry
	movl	16(%rdi), %ecx
	movl	16(%rsi), %edx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB2_4
# BB#1:                                 # %land.rhs
	movl	20(%rdi), %eax
	movl	20(%rsi), %ecx
	cmpq	%rcx, %rax
	jne	.LBB2_2
# BB#3:                                 # %land.rhs
	movabsq	$1, %rax
.LBB2_4:                                # %land.end
	retq
.LBB2_2:
	movabsq	$0, %rax
	retq
.Ltmp12:
	.size	arc_eq, .Ltmp12-arc_eq
	.cfi_endproc

	.globl	conflict_graph_delete
	.align	16, 0x90
	.type	conflict_graph_delete,@function
conflict_graph_delete:                  # @conflict_graph_delete
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp15:
	.cfi_def_cfa_offset 16
.Ltmp16:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	leaq	24(%rbx), %rdi
	xorq	%rsi, %rsi
	callq	obstack_free
	movq	(%rbx), %rdi
	callq	htab_delete
	movq	16(%rbx), %rdi
	callq	free
	movq	%rbx, %rdi
	popq	%rbx
	jmp	free  # TAILCALL
.Ltmp17:
	.size	conflict_graph_delete, .Ltmp17-conflict_graph_delete
	.cfi_endproc

	.globl	conflict_graph_add
	.align	16, 0x90
	.type	conflict_graph_add,@function
conflict_graph_add:                     # @conflict_graph_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp24:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp25:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp26:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp27:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp29:
	.cfi_def_cfa_offset 80
.Ltmp30:
	.cfi_offset %rbx, -48
.Ltmp31:
	.cfi_offset %r12, -40
.Ltmp32:
	.cfi_offset %r13, -32
.Ltmp33:
	.cfi_offset %r14, -24
.Ltmp34:
	.cfi_offset %r15, -16
	movq	%rsi, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdx, %r8
	andq	%rax, %r8
	andq	%r13, %rax
	movq	$32, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$32, %rcx
                                        # kill: RSI<def> R13<kill>
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rsi
	movq	%rdi, %r14
	movq	%r13, %r12
	jle	.LBB4_2
# BB#1:                                 # %entry
	movq	%rdx, %r12
.LBB4_2:                                # %entry
	jge	.LBB4_4
# BB#3:                                 # %entry
	movq	%rdx, %r13
.LBB4_4:                                # %entry
	cmpq	%r8, %rax
	je	.LBB4_14
# BB#5:                                 # %if.end
	movl	%r12d, 24(%rsp)
	movl	%r13d, 28(%rsp)
	movq	(%r14), %rdi
	leaq	8(%rsp), %rsi
	movq	$1, %rdx
	callq	htab_find_slot
	movq	%rax, %r15
	xorq	%rax, %rax
	cmpq	$0, (%r15)
	jne	.LBB4_13
# BB#6:                                 # %if.end11
	movq	48(%r14), %rcx
	movq	56(%r14), %rax
	subq	%rcx, %rax
	cmpq	$23, %rax
	jg	.LBB4_8
# BB#7:                                 # %if.then14
	leaq	24(%r14), %rdi
	movq	$24, %rsi
	callq	_obstack_newchunk
	movq	48(%r14), %rcx
.LBB4_8:                                # %if.end15
	addq	$24, %rcx
	movq	%rcx, 48(%r14)
	movq	40(%r14), %rax
	cmpq	%rax, %rcx
	jne	.LBB4_10
# BB#9:                                 # %if.then22
	movzbl	104(%r14), %edx
	orq	$2, %rdx
	movb	%dl, 104(%r14)
.LBB4_10:                               # %if.end23
	movslq	72(%r14), %rdx
	addq	%rdx, %rcx
	notq	%rdx
	andq	%rcx, %rdx
	movq	%rdx, 48(%r14)
	movq	56(%r14), %rbx
	movq	32(%r14), %rsi
	movq	%rdx, %rdi
	subq	%rsi, %rdi
	movq	%rbx, %rcx
	subq	%rsi, %rcx
	cmpq	%rcx, %rdi
	jle	.LBB4_12
# BB#11:                                # %if.then43
	movq	%rbx, 48(%r14)
	movq	%rbx, %rdx
.LBB4_12:                               # %if.end46
	movq	%rdx, 40(%r14)
	movl	%r12d, 16(%rax)
	movl	%r13d, 20(%rax)
	movq	$32, %rcx
	movq	16(%r14), %rdx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	movq	(%rdx,%r12,8), %rcx
	movq	%rcx, (%rax)
	movq	%rax, (%rdx,%r12,8)
	movq	$32, %rcx
	movq	16(%r14), %rdx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	(%rdx,%r13,8), %rcx
	movq	%rcx, 8(%rax)
	movq	%rax, (%rdx,%r13,8)
	movq	%rax, (%r15)
	movq	$1, %rax
.LBB4_13:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_14:                               # %if.then
	movabsq	$.L.str, %rax
	movabsq	$.L__FUNCTION__.conflict_graph_add, %rcx
	movq	$207, %rsi
	movq	%rax, %rdi
	movq	%rcx, %rdx
	callq	fancy_abort
.Ltmp35:
	.size	conflict_graph_add, .Ltmp35-conflict_graph_add
	.cfi_endproc

	.globl	conflict_graph_conflict_p
	.align	16, 0x90
	.type	conflict_graph_conflict_p,@function
conflict_graph_conflict_p:              # @conflict_graph_conflict_p
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$24, %rsp
.Ltmp37:
	.cfi_def_cfa_offset 32
	movq	$32, %rcx
	movq	%rdx, %r8
	shlq	%cl, %r8
	movq	$32, %rcx
	sarq	%cl, %r8
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r8, %rax
	movq	%rsi, %rax
	jle	.LBB5_2
# BB#1:                                 # %entry
	movq	%rdx, %rax
.LBB5_2:                                # %entry
	movl	%eax, 16(%rsp)
	jge	.LBB5_4
# BB#3:                                 # %entry
	movq	%rdx, %rsi
.LBB5_4:                                # %entry
	movl	%esi, 20(%rsp)
	movq	(%rdi), %rdi
	leaq	(%rsp), %rsi
	callq	htab_find
	testq	%rax, %rax
	je	.LBB5_5
# BB#6:                                 # %entry
	movabsq	$1, %rax
	addq	$24, %rsp
	retq
.LBB5_5:
	movabsq	$0, %rax
	addq	$24, %rsp
	retq
.Ltmp38:
	.size	conflict_graph_conflict_p, .Ltmp38-conflict_graph_conflict_p
	.cfi_endproc

	.globl	conflict_graph_enum
	.align	16, 0x90
	.type	conflict_graph_enum,@function
conflict_graph_enum:                    # @conflict_graph_enum
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp45:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp46:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp47:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp48:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp49:
	.cfi_def_cfa_offset 48
.Ltmp50:
	.cfi_offset %rbx, -48
.Ltmp51:
	.cfi_offset %r12, -40
.Ltmp52:
	.cfi_offset %r13, -32
.Ltmp53:
	.cfi_offset %r14, -24
.Ltmp54:
	.cfi_offset %r15, -16
	movq	%rcx, %r14
	movq	%rsi, %r15
	movq	$32, %rcx
	movq	%r15, %rax
	movq	16(%rdi), %rsi
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	(%rsi,%rax,8), %rbx
	movq	%rdx, %r12
	testq	%rbx, %rbx
	je	.LBB6_6
# BB#1:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB6_2:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	20(%rbx), %esi
	movl	16(%rbx), %edi
	movq	%r14, %rdx
	callq	*%r12
	testq	%r13, %rax
	jne	.LBB6_6
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB6_2 Depth=1
	movslq	20(%rbx), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB6_5
# BB#4:                                 # %select.mid
                                        #   in Loop: Header=BB6_2 Depth=1
	addq	$8, %rbx
.LBB6_5:                                # %select.end
                                        #   in Loop: Header=BB6_2 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB6_2
.LBB6_6:                                # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp55:
	.size	conflict_graph_enum, .Ltmp55-conflict_graph_enum
	.cfi_endproc

	.globl	conflict_graph_merge_regs
	.align	16, 0x90
	.type	conflict_graph_merge_regs,@function
conflict_graph_merge_regs:              # @conflict_graph_merge_regs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp62:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp63:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp64:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp65:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp66:
	.cfi_def_cfa_offset 48
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
	movq	%rdx, %r12
	movq	%rsi, %r14
	movq	%rdi, %r15
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
                                        # kill: RDX<def> R12<kill>
	andq	%r13, %rdx
                                        # kill: RSI<def> R14<kill>
	andq	%r13, %rsi
	movq	$32, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rsi
	je	.LBB7_8
# BB#1:                                 # %entry
	movq	16(%r15), %rcx
	movq	(%rcx,%rax,8), %rbx
	testq	%rbx, %rbx
	je	.LBB7_8
# BB#2:
	andq	%r12, %r13
	.align	16, 0x90
.LBB7_3:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movl	16(%rbx), %edx
	cmpq	%r13, %rdx
	jne	.LBB7_5
# BB#4:                                 # %if.then3
                                        #   in Loop: Header=BB7_3 Depth=1
	movl	20(%rbx), %edx
.LBB7_5:                                # %if.end4
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	conflict_graph_add
	movslq	20(%rbx), %rax
	movq	$32, %rcx
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB7_7
# BB#6:                                 # %select.mid
                                        #   in Loop: Header=BB7_3 Depth=1
	addq	$8, %rbx
.LBB7_7:                                # %select.end
                                        #   in Loop: Header=BB7_3 Depth=1
	movq	(%rbx), %rbx
	testq	%rbx, %rbx
	jne	.LBB7_3
.LBB7_8:                                # %while.end
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp72:
	.size	conflict_graph_merge_regs, .Ltmp72-conflict_graph_merge_regs
	.cfi_endproc

	.globl	conflict_graph_print
	.align	16, 0x90
	.type	conflict_graph_print,@function
conflict_graph_print:                   # @conflict_graph_print
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
	movq	%rsi, %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	%rdi, %rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movabsq	$.L.str1, %rdi
	movq	$11, %rsi
	movq	$1, %rdx
	movq	%rax, %rcx
	callq	fwrite
	movslq	8(%rbx), %rax
	testq	%rax, %rax
	jle	.LBB8_14
# BB#1:                                 # %for.body.lr.ph
	xorq	%r15, %r15
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB8_2:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB8_3 Depth 2
	movq	16(%rbx), %rax
	movq	(%rax,%r15,8), %r13
	testq	%r13, %r13
	movabsq	$0, %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %rbx
	je	.LBB8_13
	.align	16, 0x90
.LBB8_3:                                # %while.body.i
                                        #   Parent Loop BB8_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movl	16(%r13), %r12d
	movl	20(%r13), %r14d
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	jne	.LBB8_5
# BB#4:                                 # %if.then.i
                                        #   in Loop: Header=BB8_3 Depth=2
	xorq	%rax, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str2, %rsi
	movq	%r15, %rdx
	callq	fprintf
	movq	$1, 24(%rsp)            # 8-byte Folded Spill
.LBB8_5:                                # %if.end.i14
                                        #   in Loop: Header=BB8_3 Depth=2
	movq	%r15, %rax
	andq	%rbx, %rax
	movq	%r12, %rcx
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB8_7
# BB#6:                                 #   in Loop: Header=BB8_3 Depth=2
	movq	%r14, %r12
	jmp	.LBB8_8
	.align	16, 0x90
.LBB8_7:                                # %if.else.i
                                        #   in Loop: Header=BB8_3 Depth=2
	andq	%rbx, %r14
	cmpq	%r14, %rax
	jne	.LBB8_15
.LBB8_8:                                # %if.end.i
                                        #   in Loop: Header=BB8_3 Depth=2
	movq	%rbx, %r14
	movq	$32, %rcx
	movq	%r15, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rax, %rax
	movq	16(%rsp), %rdi          # 8-byte Reload
	movabsq	$.L.str3, %rsi
	movq	%r12, %rdx
	callq	fprintf
	movslq	20(%r13), %rax
	cmpq	%rbx, %rax
	jg	.LBB8_10
# BB#9:                                 # %select.mid
                                        #   in Loop: Header=BB8_3 Depth=2
	addq	$8, %r13
.LBB8_10:                               # %select.end
                                        #   in Loop: Header=BB8_3 Depth=2
	movq	(%r13), %r13
	testq	%r13, %r13
	movq	%r14, %rbx
	jne	.LBB8_3
# BB#11:                                # %conflict_graph_enum.exit
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	24(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB8_13
# BB#12:                                # %if.then
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	$10, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	fputc
.LBB8_13:                               # %for.inc
                                        #   in Loop: Header=BB8_2 Depth=1
	movq	%rbx, %rsi
	incq	%r15
	movq	8(%rsp), %rbx           # 8-byte Reload
	movslq	8(%rbx), %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB8_2
.LBB8_14:                               # %for.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_15:                               # %if.else10.i
	movabsq	$.L.str, %rdi
	movabsq	$.L__FUNCTION__.print_conflict, %rdx
	movq	$356, %rsi              # imm = 0x164
	callq	fancy_abort
.Ltmp90:
	.size	conflict_graph_print, .Ltmp90-conflict_graph_print
	.cfi_endproc

	.globl	conflict_graph_compute
	.align	16, 0x90
	.type	conflict_graph_compute,@function
conflict_graph_compute:                 # @conflict_graph_compute
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
	subq	$160, %rsp
.Ltmp102:
	.cfi_def_cfa_offset 208
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
	movq	%rsi, %r15
	movq	%r15, 88(%rsp)          # 8-byte Spill
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	callq	max_reg_num
	movq	%rax, %rbx
	movq	$112, %rdi
	callq	xmalloc
	movq	%rax, %r14
	movq	%r14, 96(%rsp)          # 8-byte Spill
	movabsq	$xmalloc, %r12
	movl	%ebx, 8(%r14)
	movabsq	$arc_hash, %rsi
	movabsq	$arc_eq, %rdx
	movq	$64, %rdi
	xorq	%rcx, %rcx
	callq	htab_create
	movq	%rax, (%r14)
	leaq	24(%r14), %rdi
	movabsq	$free, %r8
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	movq	%r12, %rcx
	callq	_obstack_begin
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	%rbx, %rdi
	callq	xmalloc
	movq	%rax, 16(%r14)
	xorq	%rsi, %rsi
	movq	%rax, %rdi
	movq	%rbx, %rdx
	callq	memset
	leaq	136(%rsp), %rdi
	callq	bitmap_initialize
	leaq	112(%rsp), %rdi
	callq	bitmap_initialize
	movslq	n_basic_blocks(%rip), %rdx
	testq	%rdx, %rdx
	jle	.LBB9_42
# BB#1:
	leaq	136(%rsp), %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB9_3:                                # %for.body
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB9_41 Depth 2
                                        #       Child Loop BB9_8 Depth 3
                                        #         Child Loop BB9_9 Depth 4
                                        #           Child Loop BB9_11 Depth 5
                                        #             Child Loop BB9_16 Depth 6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
                                        #       Child Loop BB9_35 Depth 3
	movq	%rdx, (%rsp)            # 8-byte Spill
	movq	basic_block_info(%rip), %rax
	movq	24(%rax,%rdx,8), %rbx
	movq	72(%rbx), %rsi
	movq	%r12, %rdi
	callq	bitmap_copy
	xorq	%rcx, %rcx
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	24(%rsp), %rdx          # 8-byte Reload
	callq	bitmap_operation
	movq	(%rbx), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	8(%rbx), %rcx
	jmp	.LBB9_41
	.align	16, 0x90
.LBB9_40:                               # %for.inc176
                                        #   in Loop: Header=BB9_41 Depth=2
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	16(%rcx), %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
.LBB9_41:                               # %for.inc176
                                        #   Parent Loop BB9_3 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB9_8 Depth 3
                                        #         Child Loop BB9_9 Depth 4
                                        #           Child Loop BB9_11 Depth 5
                                        #             Child Loop BB9_16 Depth 6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
                                        #       Child Loop BB9_35 Depth 3
	cmpq	%rax, %rcx
	je	.LBB9_2
# BB#4:                                 # %for.body10
                                        #   in Loop: Header=BB9_41 Depth=2
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movzbl	(%rcx), %eax
	movzbl	rtx_class(%rax), %eax
	cmpq	$105, %rax
	jne	.LBB9_40
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB9_41 Depth=2
	leaq	112(%rsp), %rbx
	movq	%rbx, %rdi
	callq	bitmap_clear
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	32(%rax), %rdi
	movabsq	$mark_reg, %rsi
	movq	%rbx, %rdx
	callq	note_stores
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rdx
	callq	bitmap_operation
	movq	$1, %rcx
	movq	%r12, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	bitmap_operation
	movq	112(%rsp), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	testq	%rax, %rax
	je	.LBB9_34
# BB#6:                                 # %for.cond32.preheader.lr.ph
                                        #   in Loop: Header=BB9_41 Depth=2
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	testq	%rax, %rax
	movq	$53, %rdx
	je	.LBB9_8
# BB#7:                                 # %select.mid
                                        #   in Loop: Header=BB9_41 Depth=2
	xorq	%rdx, %rdx
	.align	16, 0x90
.LBB9_8:                                # %for.cond32.preheader
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        # =>    This Loop Header: Depth=3
                                        #         Child Loop BB9_9 Depth 4
                                        #           Child Loop BB9_11 Depth 5
                                        #             Child Loop BB9_16 Depth 6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
	xorq	%rcx, %rcx
	movq	%r13, %r8
	.align	16, 0x90
.LBB9_9:                                # %for.body35
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        # =>      This Loop Header: Depth=4
                                        #           Child Loop BB9_11 Depth 5
                                        #             Child Loop BB9_16 Depth 6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	24(%rax,%rcx,8), %rsi
	testq	%rsi, %rsi
	movq	%r15, %rdi
	je	.LBB9_32
# BB#10:                                # %for.body44.preheader
                                        #   in Loop: Header=BB9_9 Depth=4
	movq	$6, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	shlq	%cl, %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
	movq	%rdx, %rcx
	andq	%r8, %rcx
	.align	16, 0x90
.LBB9_11:                               # %for.body44
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        #         Parent Loop BB9_9 Depth=4
                                        # =>        This Loop Header: Depth=5
                                        #             Child Loop BB9_16 Depth 6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
	movq	$1, %rax
	shlq	%cl, %rax
	testq	%rsi, %rax
	je	.LBB9_31
# BB#12:                                # %if.then47
                                        #   in Loop: Header=BB9_11 Depth=5
	movq	%rax, 56(%rsp)          # 8-byte Spill
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	movq	%rdx, 72(%rsp)          # 8-byte Spill
	movq	136(%rsp), %r9
	testq	%r9, %r9
	je	.LBB9_30
# BB#13:                                # %for.cond81.preheader.lr.ph
                                        #   in Loop: Header=BB9_11 Depth=5
	movq	48(%rsp), %rax          # 8-byte Reload
	movl	16(%rax), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movl	16(%r9), %ecx
	movq	40(%rsp), %rdx          # 8-byte Reload
	movq	80(%rsp), %rsi          # 8-byte Reload
	leaq	(%rsi,%rdx), %rdx
	addq	%rdx, %rax
	testq	%rcx, %rcx
	movq	$53, %r12
	je	.LBB9_15
# BB#14:                                # %select.mid7
                                        #   in Loop: Header=BB9_11 Depth=5
	xorq	%r12, %r12
.LBB9_15:                               # %select.end6
                                        #   in Loop: Header=BB9_11 Depth=5
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	leaq	(%rax,%rax,2), %rax
	leaq	8(%rdi,%rax,8), %rax
	movq	%rax, 104(%rsp)         # 8-byte Spill
	.align	16, 0x90
.LBB9_16:                               # %for.cond81.preheader
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        #         Parent Loop BB9_9 Depth=4
                                        #           Parent Loop BB9_11 Depth=5
                                        # =>          This Loop Header: Depth=6
                                        #               Child Loop BB9_18 Depth 7
                                        #               Child Loop BB9_24 Depth 7
	movq	24(%r9), %r13
	testq	%r13, %r13
	je	.LBB9_23
# BB#17:                                # %for.body95.preheader
                                        #   in Loop: Header=BB9_16 Depth=6
	movq	%r12, %rbx
	andq	%r8, %rbx
	.align	16, 0x90
.LBB9_18:                               # %for.body95
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        #         Parent Loop BB9_9 Depth=4
                                        #           Parent Loop BB9_11 Depth=5
                                        #             Parent Loop BB9_16 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movq	$1, %rax
	movq	%rbx, %rcx
	shlq	%cl, %rax
	testq	%r13, %rax
	je	.LBB9_22
# BB#19:                                # %if.then102
                                        #   in Loop: Header=BB9_18 Depth=7
	movl	16(%r9), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	addq	%rbx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %esi
	leaq	(%rdx,%rdx,2), %rcx
	movl	8(%rdi,%rcx,8), %edx
	notq	%rax
	andq	%rax, %r13
	cmpq	%rdx, %rsi
	je	.LBB9_21
# BB#20:                                # %if.then119
                                        #   in Loop: Header=BB9_18 Depth=7
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %r14
	movq	%r9, %r15
	callq	conflict_graph_add
	movq	%r15, %r9
	movq	%r14, %r8
	movq	88(%rsp), %rdi          # 8-byte Reload
.LBB9_21:                               # %if.end121
                                        #   in Loop: Header=BB9_18 Depth=7
	testq	%r13, %r13
	je	.LBB9_23
.LBB9_22:                               # %for.inc
                                        #   in Loop: Header=BB9_18 Depth=7
	incq	%r12
	movq	%r12, %rax
	andq	%r8, %rax
	incq	%rbx
	cmpq	$64, %rax
	jb	.LBB9_18
.LBB9_23:                               # %if.end127
                                        #   in Loop: Header=BB9_16 Depth=6
	movq	32(%r9), %r13
	xorq	%r12, %r12
	testq	%r13, %r13
	movabsq	$0, %rbx
	je	.LBB9_29
	.align	16, 0x90
.LBB9_24:                               # %for.body95.1
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        #       Parent Loop BB9_8 Depth=3
                                        #         Parent Loop BB9_9 Depth=4
                                        #           Parent Loop BB9_11 Depth=5
                                        #             Parent Loop BB9_16 Depth=6
                                        # =>            This Inner Loop Header: Depth=7
	movq	$1, %rax
	movq	%r12, %rcx
	shlq	%cl, %rax
	testq	%r13, %rax
	je	.LBB9_28
# BB#25:                                # %if.then102.1
                                        #   in Loop: Header=BB9_24 Depth=7
	movl	16(%r9), %edx
	movq	$7, %rcx
	shlq	%cl, %rdx
	leaq	64(%r12,%rdx), %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	104(%rsp), %rcx         # 8-byte Reload
	movl	(%rcx), %esi
	leaq	(%rdx,%rdx,2), %rcx
	movl	8(%rdi,%rcx,8), %edx
	notq	%rax
	andq	%rax, %r13
	cmpq	%rdx, %rsi
	je	.LBB9_27
# BB#26:                                # %if.then119.1
                                        #   in Loop: Header=BB9_24 Depth=7
	movq	96(%rsp), %rdi          # 8-byte Reload
	movq	%r8, %r14
	movq	%r9, %r15
	callq	conflict_graph_add
	movq	%r15, %r9
	movq	%r14, %r8
	movq	88(%rsp), %rdi          # 8-byte Reload
.LBB9_27:                               # %if.end121.1
                                        #   in Loop: Header=BB9_24 Depth=7
	testq	%r13, %r13
	je	.LBB9_29
.LBB9_28:                               # %for.inc.1
                                        #   in Loop: Header=BB9_24 Depth=7
	incq	%rbx
	movq	%rbx, %rax
	andq	%r8, %rax
	incq	%r12
	cmpq	$64, %rax
	jb	.LBB9_24
.LBB9_29:                               # %if.end127.1
                                        #   in Loop: Header=BB9_16 Depth=6
	movq	(%r9), %r9
	xorq	%r12, %r12
	testq	%r9, %r9
	jne	.LBB9_16
.LBB9_30:                               # %do.end
                                        #   in Loop: Header=BB9_11 Depth=5
	movq	56(%rsp), %rax          # 8-byte Reload
	notq	%rax
	movq	64(%rsp), %rsi          # 8-byte Reload
	andq	%rax, %rsi
	movq	72(%rsp), %rdx          # 8-byte Reload
	movq	80(%rsp), %rcx          # 8-byte Reload
	je	.LBB9_32
.LBB9_31:                               # %for.inc139
                                        #   in Loop: Header=BB9_11 Depth=5
	incq	%rdx
	movq	%rdx, %rax
	andq	%r8, %rax
	incq	%rcx
	cmpq	$64, %rax
	jb	.LBB9_11
.LBB9_32:                               # %if.end142
                                        #   in Loop: Header=BB9_9 Depth=4
	movq	%rdi, %r15
	movq	32(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	xorq	%rdx, %rdx
	cmpq	$2, %rcx
	jne	.LBB9_9
# BB#33:                                # %for.end145
                                        #   in Loop: Header=BB9_8 Depth=3
	movq	%r8, %r13
	movq	48(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	%rax, 48(%rsp)          # 8-byte Spill
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movq	24(%rsp), %r14          # 8-byte Reload
	leaq	136(%rsp), %r12
	jne	.LBB9_8
	.align	16, 0x90
.LBB9_34:                               # %do.end149
                                        #   in Loop: Header=BB9_41 Depth=2
	movq	%r14, 24(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rax          # 8-byte Reload
	movq	56(%rax), %rbx
	jmp	.LBB9_35
	.align	16, 0x90
.LBB9_39:                               # %for.inc170
                                        #   in Loop: Header=BB9_35 Depth=3
	movq	16(%rbx), %rbx
.LBB9_35:                               # %do.end149
                                        #   Parent Loop BB9_3 Depth=1
                                        #     Parent Loop BB9_41 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%rbx, %rbx
	je	.LBB9_40
# BB#36:                                # %for.body154
                                        #   in Loop: Header=BB9_35 Depth=3
	movl	(%rbx), %eax
	andq	$16711680, %rax         # imm = 0xFF0000
	cmpq	$65536, %rax            # imm = 0x10000
	jne	.LBB9_39
# BB#37:                                # %if.then159
                                        #   in Loop: Header=BB9_35 Depth=3
	movq	8(%rbx), %rax
	movl	8(%rax), %r14d
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	callq	bitmap_bit_p
	testq	%r13, %rax
	je	.LBB9_39
# BB#38:                                # %if.then167
                                        #   in Loop: Header=BB9_35 Depth=3
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	bitmap_set_bit
	jmp	.LBB9_39
.LBB9_2:                                # %for.cond.loopexit
                                        #   in Loop: Header=BB9_3 Depth=1
	movq	(%rsp), %rdx            # 8-byte Reload
	decq	%rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jg	.LBB9_3
.LBB9_42:                               # %if.then184
	leaq	136(%rsp), %rdi
	callq	bitmap_clear
	leaq	112(%rsp), %rdi
	callq	bitmap_clear
	movq	96(%rsp), %rax          # 8-byte Reload
	addq	$160, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp108:
	.size	conflict_graph_compute, .Ltmp108-conflict_graph_compute
	.cfi_endproc

	.align	16, 0x90
	.type	mark_reg,@function
mark_reg:                               # @mark_reg
	.cfi_startproc
# BB#0:                                 # %entry
	movl	(%rdi), %eax
	movq	%rax, %rcx
	andq	$65535, %rcx            # imm = 0xFFFF
	cmpq	$63, %rcx
	jne	.LBB10_2
# BB#1:                                 # %if.then
	movq	8(%rdi), %rdi
	movl	(%rdi), %eax
.LBB10_2:                               # %if.end
	andq	$65535, %rax            # imm = 0xFFFF
	cmpq	$61, %rax
	jne	.LBB10_3
# BB#4:                                 # %if.end5
	movl	8(%rdi), %esi
	movq	%rdx, %rdi
	jmp	bitmap_set_bit  # TAILCALL
.LBB10_3:                               # %return
	retq
.Ltmp109:
	.size	mark_reg, .Ltmp109-mark_reg
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"conflict.c"
	.size	.L.str, 11

	.type	.L__FUNCTION__.conflict_graph_add,@object # @__FUNCTION__.conflict_graph_add
.L__FUNCTION__.conflict_graph_add:
	.asciz	"conflict_graph_add"
	.size	.L__FUNCTION__.conflict_graph_add, 19

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Conflicts:\n"
	.size	.L.str1, 12

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" %d:"
	.size	.L.str2, 5

	.type	.L__FUNCTION__.print_conflict,@object # @__FUNCTION__.print_conflict
.L__FUNCTION__.print_conflict:
	.asciz	"print_conflict"
	.size	.L__FUNCTION__.print_conflict, 15

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	" %d"
	.size	.L.str3, 4


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
