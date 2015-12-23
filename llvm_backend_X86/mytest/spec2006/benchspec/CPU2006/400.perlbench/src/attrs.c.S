	.file	"attrs.c.bc"
	.text
	.globl	XS_attrs_import
	.align	16, 0x90
	.type	XS_attrs_import,@function
XS_attrs_import:                        # @XS_attrs_import
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
	subq	$32, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 80
.Ltmp12:
	.cfi_offset %rbx, -48
.Ltmp13:
	.cfi_offset %r12, -40
.Ltmp14:
	.cfi_offset %r13, -32
.Ltmp15:
	.cfi_offset %r14, -24
.Ltmp16:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	%rdi, %r12
	movq	PL_stack_sp(%rip), %r14
	movq	PL_stack_base(%rip), %rcx
	leaq	-8(%rax), %rdx
	movq	%rdx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	(%r12), %rax
	leaq	(%rcx,%r15,8), %rcx
	movq	%r14, %r13
	subq	%rcx, %r13
	movq	$3, %rcx
	movq	%r13, %rdx
	movq	88(%rax), %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	sarq	%cl, %rdx
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	%r13, %rbx
	andq	$-8, %rbx
	cmpq	$7, %r13
	jg	.LBB0_2
# BB#1:                                 # %if.then
	movq	96(%rax), %rax
	movq	(%rax), %rax
	movq	64(%rax), %rsi
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB0_2:                                # %if.end
	movq	PL_compcv(%rip), %rax
	subq	%rbx, %r14
	movq	%r14, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	je	.LBB0_4
# BB#3:                                 # %lor.lhs.false
	movq	(%rax), %rax
	movq	128(%rax), %r14
	xorq	%r12, %r12
	testq	%r14, %r14
	jne	.LBB0_5
.LBB0_4:                                # %if.then10
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	%r12, %r14
.LBB0_5:                                # %if.end11
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	je	.LBB0_9
# BB#6:                                 # %land.lhs.true
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$48, %rax
	je	.LBB0_9
# BB#7:                                 # %land.lhs.true15
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	cmpq	$24, %rax
	je	.LBB0_11
# BB#8:                                 # %lor.lhs.false18
	movq	PL_curcop(%rip), %rax
	movq	88(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	testq	$16, %rax
	jne	.LBB0_11
.LBB0_9:                                # %lor.lhs.false22
	movq	PL_curcop(%rip), %rax
	cmpq	$0, 88(%rax)
	jne	.LBB0_12
# BB#10:                                # %land.lhs.true26
	movzbl	PL_dowarn(%rip), %eax
	testq	$1, %rax
	je	.LBB0_12
.LBB0_11:                               # %if.then30
	movabsq	$.L.str2, %rsi
	movq	$2, %rdi
	xorq	%rax, %rax
	callq	Perl_warner
.LBB0_12:                               # %for.cond.preheader
	cmpq	$9, %r13
	jl	.LBB0_30
# BB#13:                                # %for.body.lr.ph
	cmpq	$0, (%rsp)              # 8-byte Folded Reload
	je	.LBB0_17
# BB#14:                                # %for.body.preheader
	leaq	16(,%r15,8), %r15
	movq	$1, %r13
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB0_15:                               # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	addq	%r15, %rax
	movq	-8(%rax,%r13,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB0_16
# BB#25:                                # %cond.false
                                        #   in Loop: Header=BB0_15 Depth=1
	movq	$2, %rdx
	leaq	24(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
	jmp	.LBB0_26
	.align	16, 0x90
.LBB0_16:                               # %cond.true
                                        #   in Loop: Header=BB0_15 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rsp)
	movq	(%rax), %rbx
.LBB0_26:                               # %cond.end
                                        #   in Loop: Header=BB0_15 Depth=1
	movq	$6, %rdx
	movq	%rbx, %rdi
	movabsq	$.L.str6, %rsi
	callq	strncmp
	testq	%r12, %rax
	movq	$65471, %rax            # imm = 0xFFBF
	je	.LBB0_29
# BB#27:                                # %if.else.i
                                        #   in Loop: Header=BB0_15 Depth=1
	movq	$6, %rdx
	movq	%rbx, %rdi
	movabsq	$.L.str7, %rsi
	callq	strncmp
	testq	%r12, %rax
	movq	$65407, %rax            # imm = 0xFF7F
	je	.LBB0_29
# BB#28:                                # %if.then55
                                        #   in Loop: Header=BB0_15 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str3, %rdi
	movq	%rbx, %rsi
	callq	Perl_croak
	movq	$65535, %rax            # imm = 0xFFFF
.LBB0_29:                               # %for.inc
                                        #   in Loop: Header=BB0_15 Depth=1
	movq	(%r14), %rcx
	movzbl	136(%rcx), %edx
	andq	%rax, %rdx
	movw	%dx, 136(%rcx)
	incq	%r13
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jl	.LBB0_15
	jmp	.LBB0_30
.LBB0_17:                               # %for.body.us.preheader
	leaq	16(,%r15,8), %r15
	movq	$1, %r13
	.align	16, 0x90
.LBB0_18:                               # %for.body.us
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_base(%rip), %rax
	addq	%r15, %rax
	movq	-8(%rax,%r13,8), %rdi
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	jne	.LBB0_19
# BB#20:                                # %cond.false.us
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$2, %rdx
	leaq	24(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	%rax, %rbx
	jmp	.LBB0_21
	.align	16, 0x90
.LBB0_19:                               # %cond.true.us
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 24(%rsp)
	movq	(%rax), %rbx
.LBB0_21:                               # %cond.end.us
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$6, %rdx
	movq	%rbx, %rdi
	movabsq	$.L.str6, %rsi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$64, %r12
	je	.LBB0_24
# BB#22:                                # %if.else.i.us
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	$6, %rdx
	movq	%rbx, %rdi
	movabsq	$.L.str7, %rsi
	callq	strncmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	movq	$128, %r12
	je	.LBB0_24
# BB#23:                                # %if.then55.us
                                        #   in Loop: Header=BB0_18 Depth=1
	xorq	%rax, %rax
	xorq	%r12, %r12
	movabsq	$.L.str3, %rdi
	movq	%rbx, %rsi
	callq	Perl_croak
.LBB0_24:                               # %for.inc.us
                                        #   in Loop: Header=BB0_18 Depth=1
	movq	(%r14), %rax
	movzbl	136(%rax), %ecx
	orq	%r12, %rcx
	movw	%cx, 136(%rax)
	incq	%r13
	movq	16(%rsp), %rax          # 8-byte Reload
	cmpq	%rax, %r13
	jl	.LBB0_18
.LBB0_30:                               # %for.end
	movq	8(%rsp), %rax           # 8-byte Reload
	movq	%rax, PL_stack_sp(%rip)
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	XS_attrs_import, .Ltmp17-XS_attrs_import
	.cfi_endproc

	.globl	XS_attrs_get
	.align	16, 0x90
	.type	XS_attrs_get,@function
XS_attrs_get:                           # @XS_attrs_get
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp22:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp23:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp24:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp25:
	.cfi_def_cfa_offset 48
.Ltmp26:
	.cfi_offset %rbx, -32
.Ltmp27:
	.cfi_offset %r14, -24
.Ltmp28:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r15
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r14
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r14
	movq	$3, %rcx
	sarq	%cl, %r14
	movq	%r15, %rbx
	subq	%rdx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$1, %rbx
	je	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB1_2:                                # %if.end
	movq	8(%rax,%r14,8), %rdi
	movq	16(%rdi), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	subq	%rbx, %r15
	testq	$524288, %rax           # imm = 0x80000
	je	.LBB1_4
# BB#3:                                 # %if.then7
	movq	(%rdi), %rax
	movq	(%rax), %r14
	movzbl	16(%r14), %eax
	cmpq	$12, %rax
	je	.LBB1_8
	jmp	.LBB1_9
.LBB1_4:                                # %if.else
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB1_6
# BB#5:                                 # %cond.true
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 8(%rsp)
	movq	(%rax), %rdi
	jmp	.LBB1_7
.LBB1_6:                                # %cond.false
	leaq	8(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
	movq	%rax, %rdi
.LBB1_7:                                # %cond.end
	xorq	%rsi, %rsi
	callq	Perl_get_cv
	movq	%rax, %r14
.LBB1_8:                                # %if.end19
	testq	%r14, %r14
	jne	.LBB1_10
.LBB1_9:                                # %if.then21
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	xorq	%r14, %r14
	callq	Perl_croak
.LBB1_10:                               # %if.end22
	movq	(%r14), %rax
	movzbl	136(%rax), %eax
	testq	$64, %rax
	je	.LBB1_14
# BB#11:                                # %if.then26
	movq	PL_stack_max(%rip), %rax
	subq	%r15, %rax
	cmpq	$7, %rax
	jg	.LBB1_13
# BB#12:                                # %if.then33
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB1_13:                               # %if.end35
	movabsq	$.L.str6, %rdi
	movq	$6, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r15)
	movq	(%r14), %rax
	movzbl	136(%rax), %eax
	addq	$8, %r15
.LBB1_14:                               # %if.end39
	testq	$128, %rax
	je	.LBB1_18
# BB#15:                                # %if.then45
	movq	PL_stack_max(%rip), %rax
	subq	%r15, %rax
	cmpq	$7, %rax
	jg	.LBB1_17
# BB#16:                                # %if.then52
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB1_17:                               # %if.end54
	movabsq	$.L.str7, %rdi
	movq	$6, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r15)
	addq	$8, %r15
.LBB1_18:                               # %if.end59
	movq	%r15, PL_stack_sp(%rip)
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp29:
	.size	XS_attrs_get, .Ltmp29-XS_attrs_get
	.cfi_endproc

	.globl	boot_attrs
	.align	16, 0x90
	.type	boot_attrs,@function
boot_attrs:                             # @boot_attrs
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp34:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp35:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp36:
	.cfi_def_cfa_offset 32
.Ltmp37:
	.cfi_offset %rbx, -32
.Ltmp38:
	.cfi_offset %r14, -24
.Ltmp39:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movabsq	$.L.str9, %rdi
	movabsq	$XS_attrs_import, %r14
	movabsq	$.L.str8, %r15
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$1, 88(%rax)
	movabsq	$.L.str10, %rdi
	movq	%r14, %rsi
	movq	%r15, %rdx
	callq	Perl_newXS
	movq	(%rax), %rax
	movq	$0, 88(%rax)
	movabsq	$.L.str11, %rdi
	movabsq	$XS_attrs_get, %rsi
	movq	%r15, %rdx
	callq	Perl_newXS
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%rbx,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp40:
	.size	boot_attrs, .Ltmp40-boot_attrs
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: %s(Class, ...)"
	.size	.L.str, 22

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"can't set attributes outside a subroutine scope"
	.size	.L.str1, 48

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"pragma \"attrs\" is deprecated, use \"sub NAME : ATTRS\" instead"
	.size	.L.str2, 61

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"invalid attribute name %s"
	.size	.L.str3, 26

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Usage: attrs::get(sub)"
	.size	.L.str4, 23

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"invalid subroutine reference or name"
	.size	.L.str5, 37

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"method"
	.size	.L.str6, 7

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"locked"
	.size	.L.str7, 7

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"attrs.c"
	.size	.L.str8, 8

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"attrs::unimport"
	.size	.L.str9, 16

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"attrs::import"
	.size	.L.str10, 14

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"attrs::get"
	.size	.L.str11, 11


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
