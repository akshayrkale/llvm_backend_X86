	.file	"MD5.c.bc"
	.text
	.globl	XS_Digest__MD5_new
	.align	16, 0x90
	.type	XS_Digest__MD5_new,@function
XS_Digest__MD5_new:                     # @XS_Digest__MD5_new
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r12
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r12,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	sarq	%cl, %r12
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB0_2:                                # %if.end
	movq	8(%rax,%r12,8), %rdi
	movq	16(%rdi), %rax
	leaq	1(%r12), %r15
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB0_3
# BB#7:                                 # %if.then.i
	movq	(%rdi), %rax
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB0_10
# BB#8:                                 # %cond.true.i
	movq	(%rax), %rax
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB0_10
# BB#9:                                 # %land.lhs.true.i
	movq	(%rbx), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB0_11
.LBB0_10:                               # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	Perl_croak
	jmp	.LBB0_11
.LBB0_3:                                # %if.then7
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB0_5
# BB#4:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, (%rsp)
	movq	(%rax), %r14
	jmp	.LBB0_6
.LBB0_5:                                # %cond.false
	leaq	(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %r14
.LBB0_6:                                # %cond.end
	movq	$184, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
	movq	$200003165, (%rbx)      # imm = 0xBEBCE5D
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r12,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r12,8), %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_sv_setref_pv
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%r12,8), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	orq	$8388608, 16(%rax)      # imm = 0x800000
.LBB0_11:                               # %if.end24
	movq	$1732584193, 8(%rbx)    # imm = 0x67452301
	movabsq	$4023233417, %rax       # imm = 0xEFCDAB89
	movq	%rax, 16(%rbx)
	movabsq	$2562383102, %rax       # imm = 0x98BADCFE
	movq	%rax, 24(%rbx)
	movq	$271733878, 32(%rbx)    # imm = 0x10325476
	movq	$0, 48(%rbx)
	movq	$0, 40(%rbx)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	XS_Digest__MD5_new, .Ltmp14-XS_Digest__MD5_new
	.cfi_endproc

	.globl	XS_Digest__MD5_clone
	.align	16, 0x90
	.type	XS_Digest__MD5_clone,@function
XS_Digest__MD5_clone:                   # @XS_Digest__MD5_clone
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
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB1_2:                                # %if.end
	movq	8(%rax,%rbx,8), %r15
	movzbl	18(%r15), %eax
	leaq	1(%rbx), %r13
	testq	$8, %rax
	je	.LBB1_6
# BB#3:                                 # %if.then.i
	movq	(%r15), %rax
	movq	(%rax), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB1_6
# BB#4:                                 # %cond.true.i
	movq	(%rdi), %rax
	movq	24(%rax), %r14
	testq	%r14, %r14
	je	.LBB1_6
# BB#5:                                 # %land.lhs.true.i
	movq	(%r14), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB1_7
.LBB1_6:                                # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	Perl_croak
	movq	(%r15), %rax
	movq	(%rax), %rdi
.LBB1_7:                                # %get_md5_ctx.exit
	movq	$1, %rsi
	callq	Perl_sv_reftype
	movq	%rax, %r15
	movq	$184, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r12
	callq	Perl_sv_newmortal
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	Perl_sv_setref_pv
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	orq	$8388608, 16(%rax)      # imm = 0x800000
	movq	$184, %rdx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	memcpy
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp31:
	.size	XS_Digest__MD5_clone, .Ltmp31-XS_Digest__MD5_clone
	.cfi_endproc

	.globl	XS_Digest__MD5_DESTROY
	.align	16, 0x90
	.type	XS_Digest__MD5_DESTROY,@function
XS_Digest__MD5_DESTROY:                 # @XS_Digest__MD5_DESTROY
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp35:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp36:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp37:
	.cfi_def_cfa_offset 32
.Ltmp38:
	.cfi_offset %rbx, -24
.Ltmp39:
	.cfi_offset %r14, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB2_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB2_6
# BB#3:                                 # %if.then.i
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB2_6
# BB#4:                                 # %cond.true.i
	movq	(%rax), %rax
	movq	24(%rax), %r14
	testq	%r14, %r14
	je	.LBB2_6
# BB#5:                                 # %land.lhs.true.i
	movq	(%r14), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB2_7
.LBB2_6:                                # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	Perl_croak
.LBB2_7:                                # %get_md5_ctx.exit
	movq	%r14, %rdi
	callq	Perl_safesysfree
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp40:
	.size	XS_Digest__MD5_DESTROY, .Ltmp40-XS_Digest__MD5_DESTROY
	.cfi_endproc

	.globl	XS_Digest__MD5_add
	.align	16, 0x90
	.type	XS_Digest__MD5_add,@function
XS_Digest__MD5_add:                     # @XS_Digest__MD5_add
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp47:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp48:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp49:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp50:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp51:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp52:
	.cfi_def_cfa_offset 96
.Ltmp53:
	.cfi_offset %rbx, -48
.Ltmp54:
	.cfi_offset %r12, -40
.Ltmp55:
	.cfi_offset %r13, -32
.Ltmp56:
	.cfi_offset %r14, -24
.Ltmp57:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_sp(%rip), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rdx
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rdx, %r15
	movq	$3, %rcx
	movq	%r15, %rdx
	sarq	%cl, %rdx
	movq	%rdx, 32(%rsp)          # 8-byte Spill
	cmpq	$7, %r15
	jg	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB3_2:                                # %if.end
	movq	8(%rax,%rbx,8), %rax
	movzbl	18(%rax), %ecx
	incq	%rbx
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	$8, %rcx
	je	.LBB3_6
# BB#3:                                 # %if.then.i
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB3_6
# BB#4:                                 # %cond.true.i
	movq	(%rax), %rax
	movq	24(%rax), %r12
	testq	%r12, %r12
	je	.LBB3_6
# BB#5:                                 # %land.lhs.true.i
	movq	(%r12), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB3_7
.LBB3_6:                                # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	Perl_croak
.LBB3_7:                                # %get_md5_ctx.exit
	cmpq	$9, %r15
	jl	.LBB3_25
# BB#8:                                 # %for.body.lr.ph
	leaq	56(%r12), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	leaq	16(,%r14,8), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	$1, %r14
	.align	16, 0x90
.LBB3_9:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movq	-8(%rax,%r14,8), %rdi
	movq	16(%rdi), %rax
	andq	$537133056, %rax        # imm = 0x20040000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB3_11
# BB#10:                                # %cond.true
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 40(%rsp)
	movq	(%rax), %r13
	jmp	.LBB3_12
	.align	16, 0x90
.LBB3_11:                               # %cond.false
                                        #   in Loop: Header=BB3_9 Depth=1
	leaq	40(%rsp), %rsi
	callq	Perl_sv_2pvbyte
	movq	40(%rsp), %rdx
	movq	%rax, %r13
.LBB3_12:                               # %cond.end
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	40(%r12), %rcx
	movq	%rcx, %rax
	andq	$63, %rax
	addq	%rdx, %rcx
	movq	%rcx, 40(%r12)
	jae	.LBB3_14
# BB#13:                                # %if.then.i41
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	48(%r12)
.LBB3_14:                               # %if.end.i
                                        #   in Loop: Header=BB3_9 Depth=1
	testq	%rax, %rax
	jne	.LBB3_16
# BB#15:                                #   in Loop: Header=BB3_9 Depth=1
	movq	%rdx, %rbx
	jmp	.LBB3_19
	.align	16, 0x90
.LBB3_16:                               # %if.then3.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$64, %r15
	subq	%rax, %r15
	movq	%rdx, %rbx
	subq	%r15, %rbx
	leaq	56(%r12,%rax), %rdi
	jae	.LBB3_18
# BB#17:                                # %if.then5.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	%r13, %rsi
	jmp	.LBB3_23
	.align	16, 0x90
.LBB3_18:                               # %if.end6.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	memcpy
	movq	$1, %rdx
	movq	%r12, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	MD5Transform
	addq	%r15, %r13
.LBB3_19:                               # %if.end15.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$6, %rcx
	movq	%rbx, %r15
	shrq	%cl, %r15
	testq	%r15, %r15
	je	.LBB3_21
# BB#20:                                # %if.then17.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	%r12, %rdi
	movq	%r13, %rsi
	movq	%r15, %rdx
	callq	MD5Transform
.LBB3_21:                               # %if.end18.i
                                        #   in Loop: Header=BB3_9 Depth=1
	andq	$63, %rbx
	je	.LBB3_24
# BB#22:                                # %if.then21.i
                                        #   in Loop: Header=BB3_9 Depth=1
	movq	$6, %rcx
	shlq	%cl, %r15
	addq	%r15, %r13
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rsi
	movq	%rbx, %rdx
.LBB3_23:                               # %MD5Update.exit
                                        #   in Loop: Header=BB3_9 Depth=1
	callq	memcpy
.LBB3_24:                               # %MD5Update.exit
                                        #   in Loop: Header=BB3_9 Depth=1
	incq	%r14
	movq	32(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r14
	jl	.LBB3_9
.LBB3_25:                               # %for.end
	movq	PL_stack_base(%rip), %rax
	movq	8(%rsp), %rcx           # 8-byte Reload
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp58:
	.size	XS_Digest__MD5_add, .Ltmp58-XS_Digest__MD5_add
	.cfi_endproc

	.globl	XS_Digest__MD5_addfile
	.align	16, 0x90
	.type	XS_Digest__MD5_addfile,@function
XS_Digest__MD5_addfile:                 # @XS_Digest__MD5_addfile
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp65:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp66:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp67:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp68:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp69:
	.cfi_def_cfa_offset 48
	subq	$4128, %rsp             # imm = 0x1020
.Ltmp70:
	.cfi_def_cfa_offset 4176
.Ltmp71:
	.cfi_offset %rbx, -48
.Ltmp72:
	.cfi_offset %r12, -40
.Ltmp73:
	.cfi_offset %r13, -32
.Ltmp74:
	.cfi_offset %r14, -24
.Ltmp75:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rsi
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rsi, %rdx
	cmpq	$16, %rdx
	je	.LBB4_2
# BB#1:                                 # %if.then
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB4_2:                                # %if.end
	movq	8(%rax,%rbx,8), %r14
	movq	16(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %rbx
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movzbl	18(%r14), %ecx
	movq	56(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	testq	$8, %rcx
	je	.LBB4_6
# BB#3:                                 # %if.then.i
	movq	(%r14), %rax
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB4_6
# BB#4:                                 # %cond.true.i
	movq	(%rax), %rax
	movq	24(%rax), %r13
	testq	%r13, %r13
	je	.LBB4_6
# BB#5:                                 # %land.lhs.true.i
	movq	(%r13), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB4_7
.LBB4_6:                                # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%r13, %r13
	callq	Perl_croak
.LBB4_7:                                # %get_md5_ctx.exit
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB4_39
# BB#8:                                 # %if.then9
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	40(%r13), %rax
	andq	$63, %rax
	je	.LBB4_22
# BB#9:                                 # %if.then11
	movq	$64, %rdx
	subq	%rax, %rdx
	leaq	32(%rsp), %r15
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_PerlIO_read
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movq	8(%rsp), %rbx           # 8-byte Reload
	jle	.LBB4_41
# BB#10:                                # %if.then15
	movq	40(%r13), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rcx
	andq	$63, %rcx
	addq	%rax, %rdx
	movq	%rdx, 40(%r13)
	jae	.LBB4_12
# BB#11:                                # %if.then.i54
	incq	48(%r13)
.LBB4_12:                               # %if.end.i
	testq	%rcx, %rcx
	je	.LBB4_13
# BB#14:                                # %if.then3.i
	movq	$64, %rbx
	subq	%rcx, %rbx
	movq	%rax, %r12
	subq	%rbx, %r12
	leaq	56(%r13,%rcx), %rdi
	jae	.LBB4_16
# BB#15:                                # %if.then5.i
	leaq	32(%rsp), %rsi
	movq	%rax, %rdx
	jmp	.LBB4_21
.LBB4_39:                               # %if.else34
	movabsq	$.L.str6, %rdi
	jmp	.LBB4_40
.LBB4_13:
	movq	%rax, %r12
	jmp	.LBB4_17
.LBB4_16:                               # %if.end6.i
	leaq	56(%r13), %r14
	leaq	32(%rsp), %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r14, %rsi
	callq	MD5Transform
	leaq	32(%rsp,%rbx), %r15
.LBB4_17:                               # %if.end15.i
	movq	$6, %rcx
	movq	%r12, %rbx
	shrq	%cl, %rbx
	testq	%rbx, %rbx
	je	.LBB4_19
# BB#18:                                # %if.then17.i
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	MD5Transform
.LBB4_19:                               # %if.end18.i
	andq	$63, %r12
	je	.LBB4_22
# BB#20:                                # %if.then21.i
	leaq	56(%r13), %rdi
	movq	$6, %rcx
	shlq	%cl, %rbx
	addq	%rbx, %r15
	movq	%r15, %rsi
	movq	%r12, %rdx
.LBB4_21:                               # %while.cond.preheader
	callq	memcpy
.LBB4_22:                               # %while.cond.preheader
	leaq	32(%rsp), %r15
	movq	$4096, %rdx             # imm = 0x1000
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_PerlIO_read
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jle	.LBB4_32
# BB#23:                                # %while.body.lr.ph
	leaq	56(%r13), %rcx
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB4_24:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%r13), %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rdx, %rcx
	andq	$63, %rcx
	addq	%rax, %rdx
	movq	%rdx, 40(%r13)
	jae	.LBB4_26
# BB#25:                                # %if.then.i62
                                        #   in Loop: Header=BB4_24 Depth=1
	incq	48(%r13)
.LBB4_26:                               # %if.end.i64
                                        #   in Loop: Header=BB4_24 Depth=1
	testq	%rcx, %rcx
	jne	.LBB4_28
# BB#27:                                #   in Loop: Header=BB4_24 Depth=1
	movq	%rax, %rbx
	movq	%r15, %r14
	jmp	.LBB4_35
	.align	16, 0x90
.LBB4_28:                               # %if.then3.i68
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	$64, %r14
	subq	%rcx, %r14
	movq	%rax, %rbx
	subq	%r14, %rbx
	leaq	56(%r13,%rcx), %rdi
	movq	%r15, %rsi
	jae	.LBB4_34
# BB#29:                                # %if.then5.i69
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	%rax, %rdx
	jmp	.LBB4_30
	.align	16, 0x90
.LBB4_34:                               # %if.end6.i73
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	%r14, %rdx
	callq	memcpy
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	MD5Transform
	leaq	32(%rsp,%r14), %r14
.LBB4_35:                               # %if.end15.i78
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	$6, %rcx
	movq	%rbx, %r12
	shrq	%cl, %r12
	testq	%r12, %r12
	je	.LBB4_37
# BB#36:                                # %if.then17.i79
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	%r13, %rdi
	movq	%r14, %rsi
	movq	%r12, %rdx
	callq	MD5Transform
.LBB4_37:                               # %if.end18.i82
                                        #   in Loop: Header=BB4_24 Depth=1
	andq	$63, %rbx
	je	.LBB4_31
# BB#38:                                # %if.then21.i86
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	$6, %rcx
	shlq	%cl, %r12
	addq	%r12, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r14, %rsi
	movq	%rbx, %rdx
.LBB4_30:                               # %while.cond.backedge
                                        #   in Loop: Header=BB4_24 Depth=1
	callq	memcpy
.LBB4_31:                               # %while.cond.backedge
                                        #   in Loop: Header=BB4_24 Depth=1
	movq	$4096, %rdx             # imm = 0x1000
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%r15, %rsi
	callq	Perl_PerlIO_read
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	jg	.LBB4_24
.LBB4_32:                               # %while.end
	movq	24(%rsp), %rdi          # 8-byte Reload
	callq	Perl_PerlIO_error
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	8(%rsp), %rbx           # 8-byte Reload
	je	.LBB4_41
# BB#33:                                # %if.then32
	movabsq	$.L.str5, %rdi
.LBB4_40:                               # %cleanup
	xorq	%rax, %rax
	callq	Perl_croak
.LBB4_41:                               # %cleanup
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$4128, %rsp             # imm = 0x1020
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp76:
	.size	XS_Digest__MD5_addfile, .Ltmp76-XS_Digest__MD5_addfile
	.cfi_endproc

	.globl	XS_Digest__MD5_digest
	.align	16, 0x90
	.type	XS_Digest__MD5_digest,@function
XS_Digest__MD5_digest:                  # @XS_Digest__MD5_digest
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp83:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp84:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp85:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp86:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp87:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp88:
	.cfi_def_cfa_offset 64
.Ltmp89:
	.cfi_offset %rbx, -48
.Ltmp90:
	.cfi_offset %r12, -40
.Ltmp91:
	.cfi_offset %r13, -32
.Ltmp92:
	.cfi_offset %r14, -24
.Ltmp93:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rcx
	movq	PL_stack_sp(%rip), %rsi
	movq	PL_stack_base(%rip), %rax
	leaq	-8(%rcx), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rcx), %r12
	leaq	(%rax,%r12,8), %rbx
	movq	(%rdi), %rdx
	movq	$3, %rcx
	shlq	%cl, %r12
	movq	$3, %rcx
	movq	88(%rdx), %r14
	sarq	%cl, %r12
	subq	%rbx, %rsi
	cmpq	$8, %rsi
	je	.LBB5_2
# BB#1:                                 # %if.then
	movq	96(%rdx), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB5_2:                                # %if.end
	movq	8(%rax,%r12,8), %rax
	movzbl	18(%rax), %ecx
	leaq	1(%r12), %r13
	testq	$8, %rcx
	je	.LBB5_6
# BB#3:                                 # %if.then.i
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	18(%rax), %ecx
	testq	$1, %rcx
	je	.LBB5_6
# BB#4:                                 # %cond.true.i
	movq	(%rax), %rax
	movq	24(%rax), %rbx
	testq	%rbx, %rbx
	je	.LBB5_6
# BB#5:                                 # %land.lhs.true.i
	movq	(%rbx), %rax
	cmpq	$200003165, %rax        # imm = 0xBEBCE5D
	je	.LBB5_7
.LBB5_6:                                # %if.end12.i
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	Perl_croak
.LBB5_7:                                # %get_md5_ctx.exit
	leaq	(%rsp), %r15
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	MD5Final
	movq	$1732584193, 8(%rbx)    # imm = 0x67452301
	movabsq	$4023233417, %rax       # imm = 0xEFCDAB89
	movq	%rax, 16(%rbx)
	movabsq	$2562383102, %rax       # imm = 0x98BADCFE
	movq	%rax, 24(%rbx)
	movq	$271733878, 32(%rbx)    # imm = 0x10325476
	movq	$0, 48(%rbx)
	movq	$0, 40(%rbx)
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	make_mortal_sv
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%r12,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp94:
	.size	XS_Digest__MD5_digest, .Ltmp94-XS_Digest__MD5_digest
	.cfi_endproc

	.align	16, 0x90
	.type	MD5Final,@function
MD5Final:                               # @MD5Final
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp100:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp101:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp102:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp103:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp104:
	.cfi_def_cfa_offset 48
.Ltmp105:
	.cfi_offset %rbx, -40
.Ltmp106:
	.cfi_offset %r12, -32
.Ltmp107:
	.cfi_offset %r14, -24
.Ltmp108:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	40(%r12), %rax
	movq	%rdi, %r14
	andq	$63, %rax
	cmpq	$56, %rax
	movq	$56, %rbx
	jb	.LBB6_2
# BB#1:                                 # %entry
	movq	$120, %rbx
.LBB6_2:                                # %entry
	movq	%rbx, %rdx
	subq	%rax, %rdx
	leaq	56(%r12), %r15
	leaq	56(%r12,%rax), %rdi
	movabsq	$PADDING, %rsi
	callq	memcpy
	movq	40(%r12), %rdx
	movq	48(%r12), %rsi
	movq	$3, %rcx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$3, %rcx
	shlq	%cl, %rsi
	movq	$29, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	orq	%rsi, %rax
	movq	%rbx, %rsi
	orq	$1, %rsi
	movb	%dil, 56(%r12,%rbx)
	movq	$5, %rcx
	movq	%rdx, %rdi
	shrq	%cl, %rdi
	movq	%rbx, %r8
	orq	$2, %r8
	movb	%dil, 56(%r12,%rsi)
	movq	$13, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	movq	%rbx, %rdi
	orq	$3, %rdi
	movb	%sil, 56(%r12,%r8)
	movq	$21, %rcx
	shrq	%cl, %rdx
	movb	%dl, 56(%r12,%rdi)
	movq	%rbx, %rdx
	orq	$4, %rdx
	movq	%rbx, %rsi
	orq	$5, %rsi
	movb	%al, 56(%r12,%rdx)
	movq	$8, %rcx
	movq	%rax, %rdi
	shrq	%cl, %rdi
	movq	%rbx, %r8
	orq	$6, %r8
	movb	%dil, 56(%r12,%rsi)
	movq	$16, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	orq	$7, %rbx
	movb	%sil, 56(%r12,%r8)
	movq	$24, %rcx
	shrq	%cl, %rax
	movb	%al, 56(%r12,%rbx)
	addq	$4, %rdx
	movq	$6, %rcx
	shrq	%cl, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	MD5Transform
	movq	8(%r12), %rax
	movb	%al, (%r14)
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 1(%r14)
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 2(%r14)
	movq	$24, %rcx
	shrq	%cl, %rax
	movb	%al, 3(%r14)
	movq	16(%r12), %rax
	movb	%al, 4(%r14)
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 5(%r14)
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 6(%r14)
	movq	$24, %rcx
	shrq	%cl, %rax
	movb	%al, 7(%r14)
	movq	24(%r12), %rax
	movb	%al, 8(%r14)
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 9(%r14)
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 10(%r14)
	movq	$24, %rcx
	shrq	%cl, %rax
	movb	%al, 11(%r14)
	movq	32(%r12), %rax
	movb	%al, 12(%r14)
	movq	$8, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 13(%r14)
	movq	$16, %rcx
	movq	%rax, %rdx
	shrq	%cl, %rdx
	movb	%dl, 14(%r14)
	movq	$24, %rcx
	shrq	%cl, %rax
	movb	%al, 15(%r14)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp109:
	.size	MD5Final, .Ltmp109-MD5Final
	.cfi_endproc

	.align	16, 0x90
	.type	make_mortal_sv,@function
make_mortal_sv:                         # @make_mortal_sv
	.cfi_startproc
# BB#0:                                 # %entry
	subq	$40, %rsp
.Ltmp111:
	.cfi_def_cfa_offset 48
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rsi
	movq	$16, %rax
	je	.LBB7_12
# BB#1:                                 # %entry
	andq	%rsi, %rcx
	cmpq	$2, %rcx
	jne	.LBB7_2
# BB#8:                                 # %sw.bb2
	movzbl	(%rdi), %esi
	movq	$2, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movzbl	.L.str29(%rax), %eax
	movb	%al, (%rsp)
	leaq	1(%rsp), %r9
	leaq	16(%rdi), %r8
	incq	%rdi
	.align	16, 0x90
.LBB7_9:                                # %if.end.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$4, %rcx
	shlq	%cl, %rsi
	andq	$48, %rsi
	movzbl	(%rdi), %edx
	movq	$4, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	orq	%rsi, %rax
	movzbl	1(%rdi), %r10d
	movq	$2, %rcx
	shlq	%cl, %rdx
	movq	$6, %rcx
	movq	%r10, %rsi
	shrq	%cl, %rsi
	movzbl	.L.str29(%rax), %eax
	andq	$60, %rdx
	orq	%rdx, %rsi
	movzbl	.L.str29(%rsi), %ecx
	andq	$63, %r10
	movzbl	.L.str29(%r10), %edx
	movb	%al, (%r9)
	movb	%cl, 1(%r9)
	movb	%dl, 2(%r9)
	movzbl	2(%rdi), %esi
	movq	$2, %rcx
	movq	%rsi, %rax
	shrq	%cl, %rax
	movzbl	.L.str29(%rax), %eax
	movb	%al, 3(%r9)
	addq	$3, %rdi
	addq	$4, %r9
	cmpq	%r8, %rdi
	jne	.LBB7_9
# BB#10:                                # %base64_16.exit
	leaq	(%rsp), %rdi
	movq	$4, %rcx
	shlq	%cl, %rsi
	andq	$48, %rsi
	movzbl	.L.str29(%rsi), %eax
	movb	%al, 21(%rsp)
	xorq	%rax, %rax
	movb	%al, 22(%rsp)
	movq	$22, %rax
	jmp	.LBB7_12
.LBB7_2:                                # %entry
	cmpq	$1, %rcx
	jne	.LBB7_11
# BB#3:                                 # %sw.bb1
	leaq	16(%rdi), %r10
	leaq	1(%rdi), %rcx
	cmpq	%rcx, %r10
	movq	%r10, %r9
	ja	.LBB7_5
# BB#4:                                 # %sw.bb1
	movq	%rcx, %r9
.LBB7_5:                                # %sw.bb1
	leaq	(%rsp), %rsi
	leaq	(%rdi,%rdi), %r8
	.align	16, 0x90
.LBB7_6:                                # %while.body.i
                                        # =>This Inner Loop Header: Depth=1
	movzbl	(%rdi), %edx
	movq	$4, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movzbl	.L.str30(%rax), %eax
	andq	$15, %rdx
	movzbl	.L.str30(%rdx), %ecx
	movb	%al, (%rsi)
	movb	%cl, 1(%rsi)
	incq	%rdi
	addq	$2, %rsi
	cmpq	%r10, %rdi
	jb	.LBB7_6
# BB#7:                                 # %hex_16.exit
	leaq	(%rsp), %rdi
	addq	%r9, %r9
	subq	%r8, %r9
	xorq	%rax, %rax
	movb	%al, (%rsp,%r9)
	movq	$32, %rax
	jmp	.LBB7_12
.LBB7_11:                               # %sw.default
	movabsq	$.L.str28, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
                                        # implicit-def: RDI
                                        # implicit-def: RAX
.LBB7_12:                               # %sw.epilog
	movq	%rax, %rsi
	callq	Perl_newSVpv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	addq	$40, %rsp
	retq
.Ltmp112:
	.size	make_mortal_sv, .Ltmp112-make_mortal_sv
	.cfi_endproc

	.globl	XS_Digest__MD5_md5
	.align	16, 0x90
	.type	XS_Digest__MD5_md5,@function
XS_Digest__MD5_md5:                     # @XS_Digest__MD5_md5
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp119:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp120:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp121:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp122:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp123:
	.cfi_def_cfa_offset 48
	subq	$240, %rsp
.Ltmp124:
	.cfi_def_cfa_offset 288
.Ltmp125:
	.cfi_offset %rbx, -48
.Ltmp126:
	.cfi_offset %r12, -40
.Ltmp127:
	.cfi_offset %r13, -32
.Ltmp128:
	.cfi_offset %r14, -24
.Ltmp129:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_sp(%rip), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rdx
	movq	$3, %rcx
	movq	%r14, %rbx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	subq	%rdx, %r15
	movq	$3, %rcx
	movq	%r15, %rdx
	sarq	%cl, %rdx
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	(%rdi), %rcx
	movq	88(%rcx), %r12
	movq	$1732584193, 64(%rsp)   # imm = 0x67452301
	movabsq	$4023233417, %rcx       # imm = 0xEFCDAB89
	movq	%rcx, 72(%rsp)
	movabsq	$2562383102, %rcx       # imm = 0x98BADCFE
	movq	%rcx, 80(%rsp)
	movzbl	PL_dowarn(%rip), %ecx
	movq	$271733878, 88(%rsp)    # imm = 0x10325476
	movq	$0, 104(%rsp)
	movq	$0, 96(%rsp)
	testq	$1, %rcx
	je	.LBB8_20
# BB#1:                                 # %if.then
	cmpq	$1, 24(%rsp)            # 8-byte Folded Reload
	jne	.LBB8_7
# BB#2:                                 # %if.then7
	movq	8(%rax,%rbx,8), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB8_20
# BB#3:                                 # %if.then11
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	17(%rax), %ecx
	testq	$16, %rcx
	je	.LBB8_6
# BB#4:                                 # %land.lhs.true
	movq	(%rax), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	80(%rax), %rdi
	movabsq	$.L.str8, %rsi
	callq	strcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB8_5
.LBB8_6:                                # %if.else
	movabsq	$.L.str10, %rdx
	jmp	.LBB8_14
.LBB8_7:                                # %if.else23
	cmpq	$9, %r15
	jl	.LBB8_20
# BB#8:                                 # %if.then26
	movq	8(%rax,%rbx,8), %rdi
	movq	16(%rdi), %rax
	andq	$537133056, %rax        # imm = 0x20040000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB8_10
# BB#9:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 48(%rsp)
	movq	(%rax), %rax
	jmp	.LBB8_11
.LBB8_10:                               # %cond.false
	leaq	48(%rsp), %rsi
	callq	Perl_sv_2pvbyte
	movq	48(%rsp), %rcx
.LBB8_11:                               # %cond.end
	cmpq	$11, %rcx
	jne	.LBB8_20
# BB#12:                                # %land.lhs.true44
	movabsq	$.L.str8, %rdi
	movq	$11, %rdx
	movq	%rax, %rsi
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB8_20
# BB#13:
	movabsq	$.L.str11, %rdx
	jmp	.LBB8_14
.LBB8_5:
	movabsq	$.L.str9, %rdx
.LBB8_14:                               # %if.then52
	testq	%r12, %r12
	je	.LBB8_15
# BB#16:                                # %cond.false56
	cmpq	$1, %r12
	je	.LBB8_17
# BB#18:                                # %cond.false56
	movabsq	$.L.str14, %rsi
	jmp	.LBB8_19
.LBB8_15:
	movabsq	$.L.str12, %rsi
	jmp	.LBB8_19
.LBB8_17:
	movabsq	$.L.str13, %rsi
.LBB8_19:                               # %cond.end60
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	Perl_warn
.LBB8_20:                               # %for.cond.preheader
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	testq	%r15, %r15
	jle	.LBB8_38
# BB#21:                                # %for.body.lr.ph
	leaq	8(,%r14,8), %rax
	movq	%rax, 16(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	.align	16, 0x90
.LBB8_22:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rax
	movq	(%rax,%r13,8), %rdi
	movq	16(%rdi), %rax
	andq	$537133056, %rax        # imm = 0x20040000
	cmpq	$262144, %rax           # imm = 0x40000
	jne	.LBB8_24
# BB#23:                                # %cond.true74
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rdx
	movq	%rdx, 48(%rsp)
	movq	(%rax), %r15
	jmp	.LBB8_25
	.align	16, 0x90
.LBB8_24:                               # %cond.false85
                                        #   in Loop: Header=BB8_22 Depth=1
	leaq	48(%rsp), %rsi
	callq	Perl_sv_2pvbyte
	movq	48(%rsp), %rdx
	movq	%rax, %r15
.LBB8_25:                               # %cond.end90
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	96(%rsp), %rcx
	movq	%rcx, %rax
	andq	$63, %rax
	addq	%rdx, %rcx
	movq	%rcx, 96(%rsp)
	jae	.LBB8_27
# BB#26:                                # %if.then.i
                                        #   in Loop: Header=BB8_22 Depth=1
	incq	104(%rsp)
.LBB8_27:                               # %if.end.i
                                        #   in Loop: Header=BB8_22 Depth=1
	testq	%rax, %rax
	jne	.LBB8_29
# BB#28:                                #   in Loop: Header=BB8_22 Depth=1
	movq	%rdx, %r14
	jmp	.LBB8_32
	.align	16, 0x90
.LBB8_29:                               # %if.then3.i
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	$64, %rbx
	subq	%rax, %rbx
	movq	%rdx, %r14
	subq	%rbx, %r14
	leaq	112(%rsp,%rax), %rdi
	jae	.LBB8_31
# BB#30:                                # %if.then5.i
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	%r15, %rsi
	jmp	.LBB8_36
	.align	16, 0x90
.LBB8_31:                               # %if.end6.i
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	memcpy
	movq	$1, %rdx
	leaq	56(%rsp), %rdi
	leaq	112(%rsp), %rsi
	callq	MD5Transform
	addq	%rbx, %r15
.LBB8_32:                               # %if.end15.i
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	$6, %rcx
	movq	%r14, %r12
	shrq	%cl, %r12
	testq	%r12, %r12
	je	.LBB8_34
# BB#33:                                # %if.then17.i
                                        #   in Loop: Header=BB8_22 Depth=1
	leaq	56(%rsp), %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	MD5Transform
.LBB8_34:                               # %if.end18.i
                                        #   in Loop: Header=BB8_22 Depth=1
	andq	$63, %r14
	je	.LBB8_37
# BB#35:                                # %if.then21.i
                                        #   in Loop: Header=BB8_22 Depth=1
	movq	$6, %rcx
	shlq	%cl, %r12
	addq	%r12, %r15
	leaq	112(%rsp), %rdi
	movq	%r15, %rsi
	movq	%r14, %rdx
.LBB8_36:                               # %MD5Update.exit
                                        #   in Loop: Header=BB8_22 Depth=1
	callq	memcpy
.LBB8_37:                               # %MD5Update.exit
                                        #   in Loop: Header=BB8_22 Depth=1
	incq	%r13
	movq	24(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jl	.LBB8_22
.LBB8_38:                               # %for.end
	leaq	32(%rsp), %rbx
	leaq	56(%rsp), %rsi
	movq	%rbx, %rdi
	callq	MD5Final
	movq	%rbx, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	make_mortal_sv
	movq	PL_stack_base(%rip), %rcx
	movq	8(%rsp), %rdx           # 8-byte Reload
	movq	%rax, 8(%rcx,%rdx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	1(%rdx), %rcx
	leaq	(%rax,%rcx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$240, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp130:
	.size	XS_Digest__MD5_md5, .Ltmp130-XS_Digest__MD5_md5
	.cfi_endproc

	.globl	boot_Digest__MD5
	.align	16, 0x90
	.type	boot_Digest__MD5,@function
boot_Digest__MD5:                       # @boot_Digest__MD5
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp135:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 32
.Ltmp138:
	.cfi_offset %rbx, -32
.Ltmp139:
	.cfi_offset %r14, -24
.Ltmp140:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movabsq	$.L.str17, %rdi
	movabsq	$XS_Digest__MD5_new, %rsi
	movabsq	$.L.str16, %rbx
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str18, %rdi
	movabsq	$XS_Digest__MD5_clone, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str19, %rdi
	movabsq	$XS_Digest__MD5_DESTROY, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str20, %rdi
	movabsq	$XS_Digest__MD5_add, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str21, %rdi
	movabsq	$XS_Digest__MD5_addfile, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movabsq	$.L.str22, %rdi
	movabsq	$XS_Digest__MD5_digest, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$1, 88(%rax)
	movabsq	$.L.str23, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$0, 88(%rax)
	movabsq	$.L.str24, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$2, 88(%rax)
	movabsq	$.L.str25, %rdi
	movabsq	$XS_Digest__MD5_md5, %r14
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$0, 88(%rax)
	movabsq	$.L.str26, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$2, 88(%rax)
	movabsq	$.L.str27, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$1, 88(%rax)
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r15,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp141:
	.size	boot_Digest__MD5, .Ltmp141-boot_Digest__MD5
	.cfi_endproc

	.align	16, 0x90
	.type	MD5Transform,@function
MD5Transform:                           # @MD5Transform
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp148:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp149:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp150:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp151:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp152:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp153:
	.cfi_def_cfa_offset 96
.Ltmp154:
	.cfi_offset %rbx, -48
.Ltmp155:
	.cfi_offset %r12, -40
.Ltmp156:
	.cfi_offset %r13, -32
.Ltmp157:
	.cfi_offset %r14, -24
.Ltmp158:
	.cfi_offset %r15, -16
	movq	%rdi, -128(%rsp)        # 8-byte Spill
	movq	8(%rdi), %rbx
	movq	16(%rdi), %r8
	movq	24(%rdi), %r9
	movq	32(%rdi), %rax
	.align	16, 0x90
.LBB10_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	movq	%r8, 40(%rsp)           # 8-byte Spill
	movq	%rdx, -72(%rsp)         # 8-byte Spill
	movq	%rbx, -80(%rsp)         # 8-byte Spill
	movq	%r9, -96(%rsp)          # 8-byte Spill
	movq	%rax, -88(%rsp)         # 8-byte Spill
	movq	%r9, %r14
	xorq	%rax, %r14
	andq	%r8, %r14
	xorq	%rax, %r14
	movq	%rax, %r8
	movzbl	(%rsi), %eax
	movzbl	1(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	2(%rsi), %eax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movzbl	3(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movq	%rdx, -8(%rsp)          # 8-byte Spill
	addq	%rbx, %r14
	addq	%rdx, %r14
	movabsq	$3614090360, %rax       # imm = 0xD76AA478
	addq	%rax, %r14
	movq	$7, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$25, %rcx
	shrq	%cl, %r14
	orq	%rax, %r14
	movq	40(%rsp), %rax          # 8-byte Reload
	addq	%rax, %r14
	movq	40(%rsp), %rdx          # 8-byte Reload
	xorq	%r9, %rdx
	andq	%r14, %rdx
	xorq	%r9, %rdx
	movq	%r9, %r10
	movzbl	4(%rsi), %eax
	movzbl	5(%rsi), %ebx
	movq	$8, %rcx
	shlq	%cl, %rbx
	orq	%rax, %rbx
	movzbl	6(%rsi), %eax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rbx, %rax
	movzbl	7(%rsi), %ebx
	movq	$24, %rcx
	shlq	%cl, %rbx
	orq	%rax, %rbx
	movq	%rbx, 32(%rsp)          # 8-byte Spill
	leaq	(%r8,%rbx), %r9
	addq	%rdx, %r9
	movabsq	$3905402710, %rax       # imm = 0xE8C7B756
	addq	%rax, %r9
	movq	$12, %rcx
	movq	%r9, %rdx
	shlq	%cl, %rdx
	movq	$20, %rcx
	shrq	%cl, %r9
	orq	%rdx, %r9
	addq	%r14, %r9
	movq	%r14, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	xorq	%rax, %rdx
	andq	%r9, %rdx
	movq	40(%rsp), %rax          # 8-byte Reload
	xorq	%rax, %rdx
	movzbl	8(%rsi), %r8d
	movzbl	9(%rsi), %ebx
	movq	$8, %rcx
	shlq	%cl, %rbx
	orq	%r8, %rbx
	movzbl	10(%rsi), %eax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rbx, %rax
	movzbl	11(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rax, %rdi
	movq	%rdi, -24(%rsp)         # 8-byte Spill
	leaq	(%r10,%rdi), %rax
	leaq	606105819(%rdx,%rax), %r10
	movq	$17, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$15, %rcx
	shrq	%cl, %r10
	orq	%rax, %r10
	addq	%r9, %r10
	movq	%r9, %rax
	xorq	%r14, %rax
	andq	%r10, %rax
	xorq	%r14, %rax
	movzbl	12(%rsi), %edx
	movzbl	13(%rsi), %ebx
	movq	$8, %rcx
	shlq	%cl, %rbx
	orq	%rdx, %rbx
	movzbl	14(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rbx, %rdx
	movzbl	15(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, (%rsp)            # 8-byte Spill
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rdi), %r12
	addq	%rax, %r12
	movabsq	$3250441966, %rax       # imm = 0xC1BDCEEE
	addq	%rax, %r12
	movq	$22, %rcx
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$10, %rcx
	shrq	%cl, %r12
	orq	%rax, %r12
	addq	%r10, %r12
	movzbl	16(%rsi), %eax
	movzbl	17(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	18(%rsi), %eax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdx, %rax
	movzbl	19(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	movq	%r10, %rcx
	xorq	%r9, %rcx
	andq	%r12, %rcx
	xorq	%r9, %rcx
	orq	%rax, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	addq	%rdx, %r14
	addq	%rcx, %r14
	movabsq	$4118548399, %rax       # imm = 0xF57C0FAF
	addq	%rax, %r14
	movq	$7, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$25, %rcx
	shrq	%cl, %r14
	orq	%rax, %r14
	movzbl	21(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	20(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	22(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	23(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	addq	%r12, %r14
	orq	%rdx, %rdi
	movq	%rdi, 8(%rsp)           # 8-byte Spill
	movq	%r12, %rax
	xorq	%r10, %rax
	andq	%r14, %rax
	xorq	%r10, %rax
	addq	%rdi, %r9
	leaq	1200080426(%rax,%r9), %r11
	movq	$12, %rcx
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$20, %rcx
	shrq	%cl, %r11
	orq	%rax, %r11
	movzbl	25(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	24(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	26(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	27(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, 24(%rsp)          # 8-byte Spill
	movq	%r14, %rax
	addq	%r14, %r11
	xorq	%r12, %rax
	andq	%r11, %rax
	xorq	%r12, %rax
	addq	%rdi, %r10
	addq	%rax, %r10
	movq	$17, %rcx
	movabsq	$2821735955, %rax       # imm = 0xA8304613
	addq	%rax, %r10
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$15, %rcx
	shrq	%cl, %r10
	orq	%rax, %r10
	movzbl	29(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	28(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	30(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	31(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, -104(%rsp)        # 8-byte Spill
	movq	%r11, %rax
	addq	%r11, %r10
	xorq	%r14, %rax
	andq	%r10, %rax
	xorq	%r14, %rax
	addq	%rdi, %r12
	addq	%rax, %r12
	movq	$22, %rcx
	movabsq	$4249261313, %rax       # imm = 0xFD469501
	addq	%rax, %r12
	movq	%r12, %rax
	shlq	%cl, %rax
	movq	$10, %rcx
	shrq	%cl, %r12
	orq	%rax, %r12
	movzbl	33(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	32(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	34(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	35(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, -56(%rsp)         # 8-byte Spill
	movq	%r10, %rax
	addq	%r10, %r12
	xorq	%r11, %rax
	andq	%r12, %rax
	xorq	%r11, %rax
	addq	%rdi, %r14
	leaq	1770035416(%rax,%r14), %rax
	movq	$7, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$25, %rcx
	shrq	%cl, %rax
	orq	%rdx, %rax
	movzbl	37(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	36(%rsi), %ecx
	orq	%rcx, %rdx
	movzbl	38(%rsi), %edi
	movq	$16, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movzbl	39(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	movq	%rdx, -32(%rsp)         # 8-byte Spill
	movq	%r12, %rcx
	addq	%r12, %rax
	xorq	%r10, %rcx
	andq	%rax, %rcx
	xorq	%r10, %rcx
	addq	%rdx, %r11
	movq	%rdx, %r14
	addq	%rcx, %r11
	movq	$12, %rcx
	movabsq	$2336552879, %rdx       # imm = 0x8B44F7AF
	addq	%rdx, %r11
	movq	%r11, %rdx
	shlq	%cl, %rdx
	movq	$20, %rcx
	shrq	%cl, %r11
	orq	%rdx, %r11
	movzbl	41(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	40(%rsi), %ecx
	orq	%rcx, %rdx
	movzbl	42(%rsi), %edi
	movq	$16, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movzbl	43(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	movq	%rdx, -48(%rsp)         # 8-byte Spill
	movq	%rax, %rcx
	addq	%rax, %r11
	xorq	%r12, %rcx
	andq	%r11, %rcx
	xorq	%r12, %rcx
	addq	%rdx, %r10
	movq	%rdx, %r15
	addq	%rcx, %r10
	movq	$17, %rcx
	movabsq	$4294925233, %rdx       # imm = 0xFFFF5BB1
	addq	%rdx, %r10
	movq	%r10, %rdx
	shlq	%cl, %rdx
	movq	$15, %rcx
	shrq	%cl, %r10
	orq	%rdx, %r10
	movzbl	45(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	44(%rsi), %ecx
	orq	%rcx, %rdx
	movzbl	46(%rsi), %edi
	movq	$16, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movzbl	47(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	movq	%rdx, -16(%rsp)         # 8-byte Spill
	movq	%r11, %rcx
	addq	%r11, %r10
	xorq	%rax, %rcx
	andq	%r10, %rcx
	xorq	%rax, %rcx
	addq	%rdx, %r12
	movq	%rdx, %r8
	addq	%rcx, %r12
	movq	$22, %rcx
	movabsq	$2304563134, %rdx       # imm = 0x895CD7BE
	addq	%rdx, %r12
	movq	%r12, %rdx
	shlq	%cl, %rdx
	movq	$10, %rcx
	shrq	%cl, %r12
	orq	%rdx, %r12
	movzbl	49(%rsi), %edx
	movq	$8, %rcx
	shlq	%cl, %rdx
	movzbl	48(%rsi), %ecx
	orq	%rcx, %rdx
	movzbl	50(%rsi), %edi
	movq	$16, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movzbl	51(%rsi), %edx
	movq	$24, %rcx
	shlq	%cl, %rdx
	orq	%rdi, %rdx
	movq	%rdx, -40(%rsp)         # 8-byte Spill
	movq	%r10, %rcx
	addq	%r10, %r12
	xorq	%r11, %rcx
	andq	%r12, %rcx
	xorq	%r11, %rcx
	addq	%rdx, %rax
	leaq	1804603682(%rcx,%rax), %r13
	movq	$7, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$25, %rcx
	shrq	%cl, %r13
	orq	%rax, %r13
	movzbl	53(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	52(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	54(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	55(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, -64(%rsp)         # 8-byte Spill
	movq	%r12, %rax
	addq	%r12, %r13
	xorq	%r10, %rax
	andq	%r13, %rax
	xorq	%r10, %rax
	addq	%rdi, %r11
	addq	%rax, %r11
	movq	$12, %rcx
	movabsq	$4254626195, %rax       # imm = 0xFD987193
	addq	%rax, %r11
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$20, %rcx
	shrq	%cl, %r11
	orq	%rax, %r11
	movzbl	57(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	56(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	58(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	59(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, -120(%rsp)        # 8-byte Spill
	movq	%r13, %rax
	addq	%r13, %r11
	xorq	%r12, %rax
	andq	%r11, %rax
	xorq	%r12, %rax
	addq	%rdi, %r10
	movq	%rdi, %r9
	addq	%rax, %r10
	movq	$17, %rcx
	movabsq	$2792965006, %rax       # imm = 0xA679438E
	addq	%rax, %r10
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$15, %rcx
	shrq	%cl, %r10
	orq	%rax, %r10
	movzbl	61(%rsi), %eax
	movq	$8, %rcx
	shlq	%cl, %rax
	movzbl	60(%rsi), %ecx
	orq	%rcx, %rax
	movzbl	62(%rsi), %edx
	movq	$16, %rcx
	shlq	%cl, %rdx
	orq	%rax, %rdx
	movzbl	63(%rsi), %edi
	movq	$24, %rcx
	shlq	%cl, %rdi
	orq	%rdx, %rdi
	movq	%rdi, -112(%rsp)        # 8-byte Spill
	movq	%r11, %rax
	addq	%r11, %r10
	xorq	%r13, %rax
	andq	%r10, %rax
	xorq	%r13, %rax
	addq	%rdi, %r12
	movq	%rdi, %rbx
	leaq	1236535329(%rax,%r12), %rdx
	movq	$22, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$10, %rcx
	shrq	%cl, %rdx
	orq	%rax, %rdx
	addq	%r10, %rdx
	movq	%rdx, %rax
	xorq	%r10, %rax
	andq	%r11, %rax
	xorq	%r10, %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %r13
	addq	%rax, %r13
	movq	$5, %rcx
	movabsq	$4129170786, %rax       # imm = 0xF61E2562
	addq	%rax, %r13
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$27, %rcx
	shrq	%cl, %r13
	orq	%rax, %r13
	addq	%rdx, %r13
	movq	%r13, %rax
	xorq	%rdx, %rax
	andq	%r10, %rax
	xorq	%rdx, %rax
	movq	24(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %r11
	addq	%rax, %r11
	movq	$9, %rcx
	movabsq	$3225465664, %rax       # imm = 0xC040B340
	addq	%rax, %r11
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$23, %rcx
	shrq	%cl, %r11
	orq	%rax, %r11
	addq	%r13, %r11
	movq	%r11, %rax
	xorq	%r13, %rax
	andq	%rdx, %rax
	xorq	%r13, %rax
	addq	%r8, %r10
	leaq	643717713(%rax,%r10), %r12
	movq	$14, %rcx
	movq	%r12, %rdi
	shlq	%cl, %rdi
	movq	$18, %rcx
	shrq	%cl, %r12
	orq	%rdi, %r12
	addq	%r11, %r12
	movq	%r12, %rcx
	xorq	%r11, %rcx
	andq	%r13, %rcx
	xorq	%r11, %rcx
	movq	-8(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	$20, %rcx
	movabsq	$3921069994, %rdi       # imm = 0xE9B6C7AA
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$12, %rcx
	shrq	%cl, %rdx
	orq	%rdi, %rdx
	addq	%r12, %rdx
	movq	%rdx, %rcx
	xorq	%r12, %rcx
	andq	%r11, %rcx
	xorq	%r12, %rcx
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	%rax, %r13
	addq	%rcx, %r13
	movq	$5, %rcx
	movabsq	$3593408605, %rdi       # imm = 0xD62F105D
	addq	%rdi, %r13
	movq	%r13, %rdi
	shlq	%cl, %rdi
	movq	$27, %rcx
	shrq	%cl, %r13
	orq	%rdi, %r13
	addq	%rdx, %r13
	movq	%r13, %rcx
	xorq	%rdx, %rcx
	andq	%r12, %rcx
	xorq	%rdx, %rcx
	addq	%r15, %r11
	leaq	38016083(%rcx,%r11), %rdi
	movq	$9, %rcx
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$23, %rcx
	shrq	%cl, %rdi
	orq	%r10, %rdi
	addq	%r13, %rdi
	movq	%rdi, %rcx
	xorq	%r13, %rcx
	andq	%rdx, %rcx
	xorq	%r13, %rcx
	addq	%rbx, %r12
	addq	%rcx, %r12
	movq	$14, %rcx
	movabsq	$3634488961, %rbx       # imm = 0xD8A1E681
	addq	%rbx, %r12
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$18, %rcx
	shrq	%cl, %r12
	orq	%r10, %r12
	addq	%rdi, %r12
	movq	%r12, %rcx
	xorq	%rdi, %rcx
	andq	%r13, %rcx
	xorq	%rdi, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	$20, %rcx
	movabsq	$3889429448, %rbx       # imm = 0xE7D3FBC8
	addq	%rbx, %rdx
	movq	%rdx, %r10
	shlq	%cl, %r10
	movq	$12, %rcx
	shrq	%cl, %rdx
	orq	%r10, %rdx
	addq	%r12, %rdx
	movq	%rdx, %rcx
	xorq	%r12, %rcx
	andq	%rdi, %rcx
	xorq	%r12, %rcx
	addq	%r14, %r13
	leaq	568446438(%rcx,%r13), %rbx
	movq	$5, %rcx
	movq	%rbx, %r10
	shlq	%cl, %r10
	movq	$27, %rcx
	shrq	%cl, %rbx
	orq	%r10, %rbx
	addq	%rdx, %rbx
	movq	%rbx, %rcx
	xorq	%rdx, %rcx
	andq	%r12, %rcx
	xorq	%rdx, %rcx
	addq	%r9, %rdi
	addq	%rcx, %rdi
	movq	$9, %rcx
	movabsq	$3275163606, %rax       # imm = 0xC33707D6
	addq	%rax, %rdi
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$23, %rcx
	shrq	%cl, %rdi
	orq	%r10, %rdi
	addq	%rbx, %rdi
	movq	%rdi, %rcx
	xorq	%rbx, %rcx
	andq	%rdx, %rcx
	xorq	%rbx, %rcx
	movq	(%rsp), %rax            # 8-byte Reload
	addq	%rax, %r12
	addq	%rcx, %r12
	movq	$14, %rcx
	movabsq	$4107603335, %rax       # imm = 0xF4D50D87
	addq	%rax, %r12
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$18, %rcx
	shrq	%cl, %r12
	orq	%r10, %r12
	addq	%rdi, %r12
	movq	%r12, %rcx
	xorq	%rdi, %rcx
	andq	%rbx, %rcx
	xorq	%rdi, %rcx
	movq	-56(%rsp), %r14         # 8-byte Reload
	addq	%r14, %rdx
	leaq	1163531501(%rcx,%rdx), %rdx
	movq	$20, %rcx
	movq	%rdx, %r10
	shlq	%cl, %r10
	movq	$12, %rcx
	shrq	%cl, %rdx
	orq	%r10, %rdx
	addq	%r12, %rdx
	movq	%rdx, %rcx
	xorq	%r12, %rcx
	andq	%rdi, %rcx
	xorq	%r12, %rcx
	movq	-64(%rsp), %r15         # 8-byte Reload
	addq	%r15, %rbx
	addq	%rcx, %rbx
	movq	$5, %rcx
	movabsq	$2850285829, %rax       # imm = 0xA9E3E905
	addq	%rax, %rbx
	movq	%rbx, %r10
	shlq	%cl, %r10
	movq	$27, %rcx
	shrq	%cl, %rbx
	orq	%r10, %rbx
	addq	%rdx, %rbx
	movq	%rbx, %rcx
	xorq	%rdx, %rcx
	andq	%r12, %rcx
	xorq	%rdx, %rcx
	movq	-24(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rdi
	addq	%rcx, %rdi
	movq	$9, %rcx
	movabsq	$4243563512, %rax       # imm = 0xFCEFA3F8
	addq	%rax, %rdi
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$23, %rcx
	shrq	%cl, %rdi
	orq	%r10, %rdi
	addq	%rbx, %rdi
	movq	%rdi, %rcx
	xorq	%rbx, %rcx
	andq	%rdx, %rcx
	xorq	%rbx, %rcx
	movq	-104(%rsp), %r8         # 8-byte Reload
	addq	%r8, %r12
	leaq	1735328473(%rcx,%r12), %r12
	movq	$14, %rcx
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$18, %rcx
	shrq	%cl, %r12
	orq	%r10, %r12
	addq	%rdi, %r12
	movq	%r12, %r10
	xorq	%rdi, %r10
	movq	%r10, %rcx
	andq	%rbx, %rcx
	xorq	%rdi, %rcx
	movq	-40(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	$20, %rcx
	movabsq	$2368359562, %rax       # imm = 0x8D2A4C8A
	addq	%rax, %rdx
	movq	%rdx, %r11
	shlq	%cl, %r11
	movq	$12, %rcx
	shrq	%cl, %rdx
	orq	%r11, %rdx
	addq	%r12, %rdx
	xorq	%rdx, %r10
	movq	8(%rsp), %rax           # 8-byte Reload
	addq	%rax, %rbx
	addq	%r10, %rbx
	movq	$4, %rcx
	movabsq	$4294588738, %rax       # imm = 0xFFFA3942
	addq	%rax, %rbx
	movq	%rbx, %r10
	shlq	%cl, %r10
	movq	$28, %rcx
	shrq	%cl, %rbx
	orq	%r10, %rbx
	movq	%rdx, %rcx
	addq	%rdx, %rbx
	xorq	%r12, %rcx
	xorq	%rbx, %rcx
	addq	%r14, %rdi
	addq	%rcx, %rdi
	movq	$11, %rcx
	movabsq	$2272392833, %rax       # imm = 0x8771F681
	addq	%rax, %rdi
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$21, %rcx
	shrq	%cl, %rdi
	orq	%r10, %rdi
	movq	%rbx, %rcx
	addq	%rbx, %rdi
	xorq	%rdx, %rcx
	xorq	%rdi, %rcx
	movq	-16(%rsp), %rax         # 8-byte Reload
	addq	%rax, %r12
	leaq	1839030562(%rcx,%r12), %r12
	movq	$16, %rcx
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$16, %rcx
	shrq	%cl, %r12
	orq	%r10, %r12
	movq	%rdi, %rcx
	addq	%rdi, %r12
	xorq	%rbx, %rcx
	xorq	%r12, %rcx
	addq	%r9, %rdx
	addq	%rcx, %rdx
	movq	$23, %rcx
	movabsq	$4259657740, %rax       # imm = 0xFDE5380C
	addq	%rax, %rdx
	movq	%rdx, %r10
	shlq	%cl, %r10
	movq	$9, %rcx
	shrq	%cl, %rdx
	orq	%r10, %rdx
	movq	%r12, %rcx
	addq	%r12, %rdx
	xorq	%rdi, %rcx
	xorq	%rdx, %rcx
	movq	32(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rbx
	addq	%rcx, %rbx
	movq	$4, %rcx
	movabsq	$2763975236, %rax       # imm = 0xA4BEEA44
	addq	%rax, %rbx
	movq	%rbx, %r10
	shlq	%cl, %r10
	movq	$28, %rcx
	shrq	%cl, %rbx
	orq	%r10, %rbx
	movq	%rdx, %rcx
	addq	%rdx, %rbx
	xorq	%r12, %rcx
	xorq	%rbx, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdi
	leaq	1272893353(%rcx,%rdi), %rdi
	movq	$11, %rcx
	movq	%rdi, %r10
	shlq	%cl, %r10
	movq	$21, %rcx
	shrq	%cl, %rdi
	orq	%r10, %rdi
	movq	%rbx, %rcx
	addq	%rbx, %rdi
	xorq	%rdx, %rcx
	xorq	%rdi, %rcx
	movq	%r8, %r14
	addq	%r14, %r12
	addq	%rcx, %r12
	movq	$16, %rcx
	movabsq	$4139469664, %rax       # imm = 0xF6BB4B60
	addq	%rax, %r12
	movq	%r12, %r10
	shlq	%cl, %r10
	movq	$16, %rcx
	shrq	%cl, %r12
	orq	%r10, %r12
	movq	%rdi, %rcx
	addq	%rdi, %r12
	xorq	%rbx, %rcx
	xorq	%r12, %rcx
	movq	-48(%rsp), %rax         # 8-byte Reload
	addq	%rax, %rdx
	addq	%rcx, %rdx
	movq	$23, %rcx
	movabsq	$3200236656, %rax       # imm = 0xBEBFBC70
	addq	%rax, %rdx
	movq	%rdx, %r10
	shlq	%cl, %r10
	movq	$9, %rcx
	shrq	%cl, %rdx
	orq	%r10, %rdx
	movq	%r12, %rcx
	addq	%r12, %rdx
	xorq	%rdi, %rcx
	xorq	%rdx, %rcx
	addq	%r15, %rbx
	leaq	681279174(%rcx,%rbx), %r11
	movq	$4, %rcx
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movq	$28, %rcx
	shrq	%cl, %r11
	orq	%rbx, %r11
	movq	%rdx, %rcx
	addq	%rdx, %r11
	xorq	%r12, %rcx
	xorq	%r11, %rcx
	movq	-8(%rsp), %r8           # 8-byte Reload
	addq	%r8, %rdi
	addq	%rcx, %rdi
	movq	$11, %rcx
	movabsq	$3936430074, %rax       # imm = 0xEAA127FA
	addq	%rax, %rdi
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$21, %rcx
	shrq	%cl, %rdi
	orq	%rbx, %rdi
	movq	%r11, %rcx
	addq	%r11, %rdi
	xorq	%rdx, %rcx
	xorq	%rdi, %rcx
	movq	(%rsp), %r15            # 8-byte Reload
	addq	%r15, %r12
	addq	%rcx, %r12
	movq	$16, %rcx
	movabsq	$3572445317, %rax       # imm = 0xD4EF3085
	addq	%rax, %r12
	movq	%r12, %rbx
	shlq	%cl, %rbx
	movq	$16, %rcx
	shrq	%cl, %r12
	orq	%rbx, %r12
	movq	%rdi, %rcx
	addq	%rdi, %r12
	xorq	%r11, %rcx
	xorq	%r12, %rcx
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	%rax, %rdx
	leaq	76029189(%rcx,%rdx), %rdx
	movq	$23, %rcx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$9, %rcx
	shrq	%cl, %rdx
	orq	%rbx, %rdx
	movq	%r12, %rcx
	addq	%r12, %rdx
	xorq	%rdi, %rcx
	xorq	%rdx, %rcx
	movq	-32(%rsp), %rax         # 8-byte Reload
	addq	%rax, %r11
	addq	%rcx, %r11
	movq	$4, %rcx
	movabsq	$3654602809, %rax       # imm = 0xD9D4D039
	addq	%rax, %r11
	movq	%r11, %rbx
	shlq	%cl, %rbx
	movq	$28, %rcx
	shrq	%cl, %r11
	orq	%rbx, %r11
	movq	%rdx, %rcx
	addq	%rdx, %r11
	xorq	%r12, %rcx
	xorq	%r11, %rcx
	movq	-40(%rsp), %r9          # 8-byte Reload
	addq	%r9, %rdi
	addq	%rcx, %rdi
	movq	$11, %rcx
	movabsq	$3873151461, %rax       # imm = 0xE6DB99E5
	addq	%rax, %rdi
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$21, %rcx
	shrq	%cl, %rdi
	orq	%rbx, %rdi
	movq	%r11, %rcx
	addq	%r11, %rdi
	xorq	%rdx, %rcx
	xorq	%rdi, %rcx
	movq	-112(%rsp), %r13        # 8-byte Reload
	addq	%r13, %r12
	leaq	530742520(%rcx,%r12), %r10
	movq	$16, %rcx
	movq	%r10, %rax
	shlq	%cl, %rax
	movq	$16, %rcx
	shrq	%cl, %r10
	orq	%rax, %r10
	movq	%rdi, %rax
	addq	%rdi, %r10
	xorq	%r11, %rax
	xorq	%r10, %rax
	movq	-24(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rdx
	movq	%rcx, %r12
	addq	%rax, %rdx
	movq	$23, %rcx
	movabsq	$3299628645, %rax       # imm = 0xC4AC5665
	addq	%rax, %rdx
	movq	%rdx, %rax
	shlq	%cl, %rax
	movq	$9, %rcx
	shrq	%cl, %rdx
	orq	%rax, %rdx
	addq	%r8, %r11
	movq	%rdi, %rax
	addq	%r10, %rdx
	notq	%rax
	orq	%rdx, %rax
	xorq	%r10, %rax
	addq	%rax, %r11
	movq	$6, %rcx
	movabsq	$4096336452, %rax       # imm = 0xF4292244
	addq	%rax, %r11
	movq	%r11, %rax
	shlq	%cl, %rax
	movq	$26, %rcx
	shrq	%cl, %r11
	orq	%rax, %r11
	addq	%r14, %rdi
	movq	%r10, %rax
	addq	%rdx, %r11
	notq	%rax
	orq	%r11, %rax
	xorq	%rdx, %rax
	leaq	1126891415(%rax,%rdi), %rax
	movq	$10, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$22, %rcx
	shrq	%cl, %rax
	orq	%rdi, %rax
	movq	-120(%rsp), %rcx        # 8-byte Reload
	addq	%rcx, %r10
	movq	%rdx, %rcx
	addq	%r11, %rax
	notq	%rcx
	orq	%rax, %rcx
	xorq	%r11, %rcx
	addq	%rcx, %r10
	movq	$15, %rcx
	movabsq	$2878612391, %rdi       # imm = 0xAB9423A7
	addq	%rdi, %r10
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$17, %rcx
	shrq	%cl, %r10
	orq	%rdi, %r10
	movq	8(%rsp), %rcx           # 8-byte Reload
	addq	%rcx, %rdx
	movq	%r11, %rcx
	addq	%rax, %r10
	notq	%rcx
	orq	%r10, %rcx
	xorq	%rax, %rcx
	addq	%rcx, %rdx
	movq	$21, %rcx
	movabsq	$4237533241, %rdi       # imm = 0xFC93A039
	addq	%rdi, %rdx
	movq	%rdx, %rdi
	shlq	%cl, %rdi
	movq	$11, %rcx
	shrq	%cl, %rdx
	orq	%rdi, %rdx
	addq	%r9, %r11
	movq	%rax, %rcx
	addq	%r10, %rdx
	notq	%rcx
	orq	%rdx, %rcx
	xorq	%r10, %rcx
	leaq	1700485571(%rcx,%r11), %rdi
	movq	$6, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movq	$26, %rcx
	shrq	%cl, %rdi
	orq	%rbx, %rdi
	addq	%r15, %rax
	movq	%r10, %rcx
	addq	%rdx, %rdi
	notq	%rcx
	orq	%rdi, %rcx
	xorq	%rdx, %rcx
	addq	%rcx, %rax
	movq	$10, %rcx
	movabsq	$2399980690, %rbx       # imm = 0x8F0CCC92
	addq	%rbx, %rax
	movq	%rax, %rbx
	shlq	%cl, %rbx
	movq	$22, %rcx
	shrq	%cl, %rax
	orq	%rbx, %rax
	movq	-48(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %r10
	movq	%rdx, %rcx
	addq	%rdi, %rax
	notq	%rcx
	orq	%rax, %rcx
	xorq	%rdi, %rcx
	addq	%rcx, %r10
	movq	$15, %rcx
	movabsq	$4293915773, %rbx       # imm = 0xFFEFF47D
	addq	%rbx, %r10
	movq	%r10, %rbx
	shlq	%cl, %rbx
	movq	$17, %rcx
	shrq	%cl, %r10
	orq	%rbx, %r10
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rdx
	movq	%rdi, %rcx
	addq	%rax, %r10
	notq	%rcx
	orq	%r10, %rcx
	xorq	%rax, %rcx
	addq	%rcx, %rdx
	movq	$21, %rcx
	movabsq	$2240044497, %rbx       # imm = 0x85845DD1
	addq	%rbx, %rdx
	movq	%rdx, %rbx
	shlq	%cl, %rbx
	movq	$11, %rcx
	shrq	%cl, %rdx
	orq	%rbx, %rdx
	movq	-56(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rdi
	movq	%rax, %rcx
	addq	%r10, %rdx
	notq	%rcx
	orq	%rdx, %rcx
	xorq	%r10, %rcx
	leaq	1873313359(%rcx,%rdi), %rbx
	movq	$6, %rcx
	movq	%rbx, %rdi
	shlq	%cl, %rdi
	movq	$26, %rcx
	shrq	%cl, %rbx
	orq	%rdi, %rbx
	addq	%rdx, %rbx
	addq	%r13, %rax
	movq	%r10, %rcx
	notq	%rcx
	orq	%rbx, %rcx
	xorq	%rdx, %rcx
	addq	%rcx, %rax
	movabsq	$4264355552, %rcx       # imm = 0xFE2CE6E0
	addq	%rcx, %rax
	movq	$10, %rcx
	movq	%rax, %rdi
	shlq	%cl, %rdi
	movq	$22, %rcx
	shrq	%cl, %rax
	orq	%rdi, %rax
	addq	%rbx, %rax
	movq	%rdx, %rcx
	notq	%rcx
	orq	%rax, %rcx
	xorq	%rbx, %rcx
	movq	24(%rsp), %rdi          # 8-byte Reload
	addq	%rdi, %r10
	addq	%rcx, %r10
	movabsq	$2734768916, %rcx       # imm = 0xA3014314
	addq	%rcx, %r10
	movq	$15, %rcx
	movq	%r10, %rdi
	shlq	%cl, %rdi
	movq	$17, %rcx
	shrq	%cl, %r10
	orq	%rdi, %r10
	addq	%rax, %r10
	movq	%rbx, %rcx
	notq	%rcx
	orq	%r10, %rcx
	xorq	%rax, %rcx
	movq	-64(%rsp), %rdi         # 8-byte Reload
	addq	%rdi, %rdx
	leaq	1309151649(%rcx,%rdx), %rdi
	movq	$21, %rcx
	movq	%rdi, %rdx
	shlq	%cl, %rdx
	movq	$11, %rcx
	shrq	%cl, %rdi
	orq	%rdx, %rdi
	addq	%r10, %rdi
	movq	%rax, %rcx
	notq	%rcx
	orq	%rdi, %rcx
	xorq	%r10, %rcx
	movq	16(%rsp), %rdx          # 8-byte Reload
	addq	%rdx, %rbx
	addq	%rcx, %rbx
	movabsq	$4149444226, %rcx       # imm = 0xF7537E82
	addq	%rcx, %rbx
	movq	$6, %rcx
	movq	%rbx, %rdx
	shlq	%cl, %rdx
	movq	$26, %rcx
	shrq	%cl, %rbx
	orq	%rdx, %rbx
	addq	%rdi, %rbx
	movq	%r10, %rcx
	notq	%rcx
	orq	%rbx, %rcx
	xorq	%rdi, %rcx
	movq	-16(%rsp), %rdx         # 8-byte Reload
	addq	%rdx, %rax
	addq	%rcx, %rax
	movabsq	$3174756917, %rcx       # imm = 0xBD3AF235
	addq	%rcx, %rax
	movq	$10, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$22, %rcx
	shrq	%cl, %rax
	orq	%rdx, %rax
	addq	%rbx, %rax
	movq	%rdi, %rcx
	notq	%rcx
	orq	%rax, %rcx
	xorq	%rbx, %rcx
	addq	%r12, %r10
	leaq	718787259(%rcx,%r10), %r9
	movq	$15, %rcx
	movq	%r9, %r8
	shlq	%cl, %r8
	movq	$17, %rcx
	shrq	%cl, %r9
	orq	%r8, %r9
	addq	%rax, %r9
	movq	%rbx, %rcx
	notq	%rcx
	orq	%r9, %rcx
	xorq	%rax, %rcx
	movq	-32(%rsp), %rdx         # 8-byte Reload
	addq	%rdx, %rdi
	addq	%rcx, %rdi
	movabsq	$3951481745, %rcx       # imm = 0xEB86D391
	addq	%rcx, %rdi
	movq	$21, %rcx
	movq	%rdi, %r8
	shlq	%cl, %r8
	movq	$11, %rcx
	shrq	%cl, %rdi
	movq	-72(%rsp), %rdx         # 8-byte Reload
	orq	%r8, %rdi
	movq	40(%rsp), %r8           # 8-byte Reload
	movq	-80(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rbx
	addq	%r9, %r8
	addq	%rdi, %r8
	movq	-96(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %r9
	movq	-88(%rsp), %rcx         # 8-byte Reload
	addq	%rcx, %rax
	addq	$64, %rsi
	decq	%rdx
	jne	.LBB10_1
# BB#2:                                 # %do.end
	movq	-128(%rsp), %rcx        # 8-byte Reload
	movq	%rbx, 8(%rcx)
	movq	%r8, 16(%rcx)
	movq	%r9, 24(%rcx)
	movq	%rax, 32(%rcx)
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp159:
	.size	MD5Transform, .Ltmp159-MD5Transform
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Digest::MD5::new(xclass)"
	.size	.L.str, 32

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Usage: Digest::MD5::clone(self)"
	.size	.L.str1, 32

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Usage: Digest::MD5::DESTROY(context)"
	.size	.L.str2, 37

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Usage: Digest::MD5::add(self, ...)"
	.size	.L.str3, 35

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Usage: Digest::MD5::addfile(self, fh)"
	.size	.L.str4, 38

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Reading from filehandle failed"
	.size	.L.str5, 31

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"No filehandle passed"
	.size	.L.str6, 21

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Usage: %s(context)"
	.size	.L.str7, 19

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Digest::MD5"
	.size	.L.str8, 12

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"probably called as method"
	.size	.L.str9, 26

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"called with reference argument"
	.size	.L.str10, 31

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"probably called as class method"
	.size	.L.str11, 32

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"md5"
	.size	.L.str12, 4

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"md5_hex"
	.size	.L.str13, 8

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"md5_base64"
	.size	.L.str14, 11

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"&Digest::MD5::%s function %s"
	.size	.L.str15, 29

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"MD5.c"
	.size	.L.str16, 6

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"Digest::MD5::new"
	.size	.L.str17, 17

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Digest::MD5::clone"
	.size	.L.str18, 19

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Digest::MD5::DESTROY"
	.size	.L.str19, 21

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"Digest::MD5::add"
	.size	.L.str20, 17

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Digest::MD5::addfile"
	.size	.L.str21, 21

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Digest::MD5::hexdigest"
	.size	.L.str22, 23

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Digest::MD5::digest"
	.size	.L.str23, 20

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Digest::MD5::b64digest"
	.size	.L.str24, 23

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Digest::MD5::md5"
	.size	.L.str25, 17

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Digest::MD5::md5_base64"
	.size	.L.str26, 24

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Digest::MD5::md5_hex"
	.size	.L.str27, 21

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Bad convertion type (%d)"
	.size	.L.str28, 25

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
	.size	.L.str29, 65

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"0123456789abcdef"
	.size	.L.str30, 17

	.type	PADDING,@object         # @PADDING
	.section	.rodata,"a",@progbits
	.align	16
PADDING:
	.asciz	"\200\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000"
	.size	PADDING, 64

	.type	.L.str31,@object        # @.str31
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str31:
	.asciz	"Not a reference to a Digest::MD5 object"
	.size	.L.str31, 40


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
