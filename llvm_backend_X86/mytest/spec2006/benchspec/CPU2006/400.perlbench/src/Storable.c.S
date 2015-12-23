	.file	"Storable.c.bc"
	.text
	.globl	is_storing
	.align	16, 0x90
	.type	is_storing,@function
is_storing:                             # @is_storing
	.cfi_startproc
# BB#0:                                 # %entry
	movq	Context_ptr(%rip), %rcx
	movl	(%rcx), %edx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB0_2
# BB#1:                                 # %land.rhs
	movzbl	4(%rcx), %eax
	andq	$1, %rax
.LBB0_2:                                # %land.end
	retq
.Ltmp0:
	.size	is_storing, .Ltmp0-is_storing
	.cfi_endproc

	.globl	is_retrieving
	.align	16, 0x90
	.type	is_retrieving,@function
is_retrieving:                          # @is_retrieving
	.cfi_startproc
# BB#0:                                 # %entry
	movq	Context_ptr(%rip), %rcx
	movl	(%rcx), %edx
	xorq	%rax, %rax
	testq	%rdx, %rdx
	je	.LBB1_2
# BB#1:                                 # %land.rhs
	movzbl	4(%rcx), %eax
	andq	$2, %rax
	movq	$1, %rcx
	shrq	%cl, %rax
.LBB1_2:                                # %land.end
	retq
.Ltmp1:
	.size	is_retrieving, .Ltmp1-is_retrieving
	.cfi_endproc

	.globl	last_op_in_netorder
	.align	16, 0x90
	.type	last_op_in_netorder,@function
last_op_in_netorder:                    # @last_op_in_netorder
	.cfi_startproc
# BB#0:                                 # %entry
	movq	Context_ptr(%rip), %rax
	movl	80(%rax), %eax
	retq
.Ltmp2:
	.size	last_op_in_netorder, .Ltmp2-last_op_in_netorder
	.cfi_endproc

	.globl	pstore
	.align	16, 0x90
	.type	pstore,@function
pstore:                                 # @pstore
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	jmp	do_store  # TAILCALL
.Ltmp3:
	.size	pstore, .Ltmp3-pstore
	.cfi_endproc

	.align	16, 0x90
	.type	do_store,@function
do_store:                               # @do_store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp10:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp11:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp12:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp13:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp14:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp15:
	.cfi_def_cfa_offset 80
.Ltmp16:
	.cfi_offset %rbx, -48
.Ltmp17:
	.cfi_offset %r12, -40
.Ltmp18:
	.cfi_offset %r13, -32
.Ltmp19:
	.cfi_offset %r14, -24
.Ltmp20:
	.cfi_offset %r15, -16
	movq	Context_ptr(%rip), %r15
	movl	112(%r15), %eax
	movq	%r8, 8(%rsp)            # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	%rsi, %r12
	movq	%rdi, %rbx
	testq	%rax, %rax
	je	.LBB4_8
# BB#1:                                 # %if.then
	movl	116(%r15), %eax
	testq	%rax, %rax
	je	.LBB4_3
# BB#2:                                 # %if.then.i
	xorq	%rax, %rax
	movl	%eax, 116(%r15)
	movq	208(%r15), %rax
	movq	%rax, 176(%r15)
	movq	200(%r15), %rax
	movq	%rax, 168(%r15)
	movq	184(%r15), %rax
	movq	192(%r15), %rcx
	movq	%rcx, 160(%r15)
	movq	%rax, 152(%r15)
.LBB4_3:                                # %if.end.i
	movl	4(%r15), %eax
	testq	$2, %rax
	je	.LBB4_5
# BB#4:                                 # %if.then3.i
	movq	%r15, %rdi
	callq	clean_retrieve_context
	jmp	.LBB4_8
.LBB4_5:                                # %if.else.i
	testq	$1, %rax
	je	.LBB4_7
# BB#6:                                 # %if.then7.i
	movq	%r15, %rdi
	callq	clean_store_context
	jmp	.LBB4_8
.LBB4_7:                                # %if.else8.i
	xorq	%rcx, %rcx
	movl	%ecx, (%r15)
	movl	%ecx, 112(%r15)
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rax
	movl	%eax, 4(%r15)
.LBB4_8:                                # %if.end
	movl	(%r15), %eax
	testq	%rax, %rax
	je	.LBB4_10
# BB#9:                                 # %if.then3
	movq	$255, %rdi
	callq	Perl_newSV
	movq	%rax, %r14
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r13
	movabsq	$.L.str29, %rdi
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_bless
	movq	(%r14), %rax
	movq	%rbx, %r14
	movq	(%rax), %rbx
	xorq	%rsi, %rsi
	movq	$240, %rdx
	movq	%rbx, %rdi
	callq	memset
	movq	%r13, 248(%rbx)
	movq	248(%r15), %rax
	movq	%rax, 240(%rbx)
	movq	%rbx, Context_ptr(%rip)
	movq	%rbx, %r15
	movq	%r14, %rbx
.LBB4_10:                               # %if.end4
	movq	$1, %rax
	movl	%eax, (%r15)
	movzbl	18(%r12), %ecx
	testq	$8, %rcx
	jne	.LBB4_12
# BB#11:                                # %if.then7
	movl	%eax, 112(%r15)
	movabsq	$.L.str64, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB4_12:                               # %if.end9
	movq	(%r12), %rax
	movq	(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	testq	%rbx, %rbx
	jne	.LBB4_17
# BB#13:                                # %if.then11
	movq	%rbx, %r14
	movq	152(%r15), %rax
	testq	%rax, %rax
	je	.LBB4_15
# BB#14:                                # %if.then11.if.end18_crit_edge
	movq	160(%r15), %rbx
	jmp	.LBB4_16
.LBB4_15:                               # %if.then13
	movq	$8192, %rdi             # imm = 0x2000
	movq	$8192, %rbx             # imm = 0x2000
	callq	Perl_safesysmalloc
	movq	%rax, 152(%r15)
	movq	$8192, 160(%r15)        # imm = 0x2000
.LBB4_16:                               # %if.end18
	movq	%rax, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	movq	%r14, %rbx
.LBB4_17:                               # %if.end27
	movq	24(%rsp), %rcx          # 8-byte Reload
	orq	$1, %rcx
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%eax, 80(%r15)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 88(%r15)
	movl	%eax, 92(%r15)
	movq	$0, 96(%r15)
	movl	%eax, 104(%r15)
	movq	$-1, 72(%r15)
	movq	$-1, 64(%r15)
	movq	%rbx, 216(%r15)
	movl	%ecx, 4(%r15)
	movq	$1, %rax
	movl	%eax, (%r15)
	callq	Perl_newHV
	movq	%rax, 8(%r15)
	andq	$-536870913, 16(%rax)   # imm = 0xFFFFFFFFDFFFFFFF
	movq	(%rax), %rax
	movq	$4095, 16(%rax)         # imm = 0xFFF
	callq	Perl_newHV
	movq	%rax, 40(%r15)
	movq	(%rax), %rax
	movq	$4095, 16(%rax)         # imm = 0xFFF
	callq	Perl_newHV
	movq	%rax, 56(%r15)
	callq	Perl_newAV
	movq	%rax, 16(%r15)
	movl	80(%r15), %eax
	testq	%rax, %rax
	jne	.LBB4_18
# BB#19:                                # %if.end27
	movabsq	$1, %rax
	jmp	.LBB4_20
.LBB4_18:
	movabsq	$0, %rax
.LBB4_20:                               # %if.end27
	leaq	6(%rax,%rax,8), %r14
	je	.LBB4_21
# BB#22:                                # %if.end27
	movabsq	$magic_write.network_file_header, %r12
	jmp	.LBB4_23
.LBB4_21:
	movabsq	$magic_write.file_header, %r12
.LBB4_23:                               # %if.end27
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB4_24
# BB#27:                                # %if.else42.i
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	Perl_PerlIO_write
	movq	%rax, %rcx
	xorq	%rax, %rax
	cmpq	%r14, %rcx
	jne	.LBB4_37
	jmp	.LBB4_28
.LBB4_24:                               # %if.then6.i
	movq	168(%r15), %r13
	movq	176(%r15), %rax
	addq	$4, %r12
	leaq	-4(%r13,%r14), %rcx
	addq	$-4, %r14
	cmpq	%rax, %rcx
	jbe	.LBB4_26
# BB#25:                                # %if.then9.i
	movq	152(%r15), %rdi
	movq	160(%r15), %rax
	addq	%r14, %rax
	subq	%rdi, %r13
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$35180077121536, %rbx   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %r13
	movq	%r13, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
.LBB4_26:                               # %if.end35.i
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%r14, %rdx
	callq	memcpy
	addq	%r14, 168(%r15)
.LBB4_28:                               # %if.end30
	movq	%r15, %rdi
	movq	(%rsp), %rsi            # 8-byte Reload
	callq	store
	cmpq	$0, 216(%r15)
	movq	%rax, %r14
	movq	8(%rsp), %rbx           # 8-byte Reload
	jne	.LBB4_31
# BB#29:                                # %if.end30
	testq	%rbx, %rbx
	je	.LBB4_31
# BB#30:                                # %if.then34
	movq	Context_ptr(%rip), %rax
	movq	152(%rax), %rdi
	movq	168(%rax), %rsi
	subq	%rdi, %rsi
	callq	Perl_newSVpv
	movq	%rax, (%rbx)
.LBB4_31:                               # %if.end36
	movq	%r15, %rdi
	callq	clean_store_context
	movq	240(%r15), %rax
	testq	%rax, %rax
	je	.LBB4_34
# BB#32:                                # %land.lhs.true38
	movzbl	4(%r15), %ecx
	testq	$4, %rcx
	jne	.LBB4_34
# BB#33:                                # %free_context.exit
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	248(%r15), %rdi
	callq	Perl_sv_free
	movq	%rbx, Context_ptr(%rip)
.LBB4_34:                               # %if.end43
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r14
	jne	.LBB4_35
# BB#36:                                # %if.end43
	movabsq	$1, %rax
	jmp	.LBB4_37
.LBB4_35:
	movabsq	$0, %rax
.LBB4_37:                               # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp21:
	.size	do_store, .Ltmp21-do_store
	.cfi_endproc

	.globl	net_pstore
	.align	16, 0x90
	.type	net_pstore,@function
net_pstore:                             # @net_pstore
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	jmp	do_store  # TAILCALL
.Ltmp22:
	.size	net_pstore, .Ltmp22-net_pstore
	.cfi_endproc

	.globl	mstore
	.align	16, 0x90
	.type	mstore,@function
mstore:                                 # @mstore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp24:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%rax, %rsi
	callq	do_store
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB6_1
# BB#2:                                 # %entry
	movq	(%rsp), %rax
	popq	%rdx
	retq
.LBB6_1:
	movabsq	$PL_sv_undef, %rax
	popq	%rdx
	retq
.Ltmp25:
	.size	mstore, .Ltmp25-mstore
	.cfi_endproc

	.globl	net_mstore
	.align	16, 0x90
	.type	net_mstore,@function
net_mstore:                             # @net_mstore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp27:
	.cfi_def_cfa_offset 16
	movq	%rdi, %rax
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	movq	%rax, %rsi
	callq	do_store
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB7_1
# BB#2:                                 # %entry
	movq	(%rsp), %rax
	popq	%rdx
	retq
.LBB7_1:
	movabsq	$PL_sv_undef, %rax
	popq	%rdx
	retq
.Ltmp28:
	.size	net_mstore, .Ltmp28-net_mstore
	.cfi_endproc

	.globl	pretrieve
	.align	16, 0x90
	.type	pretrieve,@function
pretrieve:                              # @pretrieve
	.cfi_startproc
# BB#0:                                 # %entry
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	jmp	do_retrieve  # TAILCALL
.Ltmp29:
	.size	pretrieve, .Ltmp29-pretrieve
	.cfi_endproc

	.align	16, 0x90
	.type	do_retrieve,@function
do_retrieve:                            # @do_retrieve
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp38:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp39:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp40:
	.cfi_def_cfa_offset 48
	subq	$320, %rsp              # imm = 0x140
.Ltmp41:
	.cfi_def_cfa_offset 368
.Ltmp42:
	.cfi_offset %rbx, -48
.Ltmp43:
	.cfi_offset %r12, -40
.Ltmp44:
	.cfi_offset %r13, -32
.Ltmp45:
	.cfi_offset %r14, -24
.Ltmp46:
	.cfi_offset %r15, -16
	movq	Context_ptr(%rip), %r14
	movl	112(%r14), %eax
	movq	%rdx, %rbx
	movq	%rsi, %r13
	movq	%rdi, %r15
	testq	%rax, %rax
	je	.LBB9_8
# BB#1:                                 # %if.then
	movl	116(%r14), %eax
	testq	%rax, %rax
	je	.LBB9_3
# BB#2:                                 # %if.then.i
	xorq	%rax, %rax
	movl	%eax, 116(%r14)
	movq	208(%r14), %rax
	movq	%rax, 176(%r14)
	movq	200(%r14), %rax
	movq	%rax, 168(%r14)
	movq	184(%r14), %rax
	movq	192(%r14), %rcx
	movq	%rcx, 160(%r14)
	movq	%rax, 152(%r14)
.LBB9_3:                                # %if.end.i
	movl	4(%r14), %eax
	testq	$2, %rax
	je	.LBB9_5
# BB#4:                                 # %if.then3.i
	movq	%r14, %rdi
	callq	clean_retrieve_context
	jmp	.LBB9_8
.LBB9_5:                                # %if.else.i
	testq	$1, %rax
	je	.LBB9_7
# BB#6:                                 # %if.then7.i
	movq	%r14, %rdi
	callq	clean_store_context
	jmp	.LBB9_8
.LBB9_7:                                # %if.else8.i
	xorq	%rcx, %rcx
	movl	%ecx, (%r14)
	movl	%ecx, 112(%r14)
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rcx, %rax
	movl	%eax, 4(%r14)
.LBB9_8:                                # %if.end
	movl	(%r14), %eax
	testq	%rax, %rax
	je	.LBB9_10
# BB#9:                                 # %if.then3
	movq	$255, %rdi
	callq	Perl_newSV
	movq	%rax, %r12
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	%r13, %r15
	movq	%rax, %r13
	movabsq	$.L.str29, %rdi
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%r13, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_bless
	movq	(%r12), %rax
	movq	%rbx, %r12
	movq	(%rax), %rbx
	xorq	%rsi, %rsi
	movq	$240, %rdx
	movq	%rbx, %rdi
	callq	memset
	movq	%r13, 248(%rbx)
	movq	%r15, %r13
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	248(%r14), %rax
	movq	%rax, 240(%rbx)
	movq	%rbx, Context_ptr(%rip)
	movq	%rbx, %r14
	movq	%r12, %rbx
.LBB9_10:                               # %if.end4
	movq	$1, %rax
	movl	%eax, (%r14)
	cmpq	$0, 120(%r14)
	jne	.LBB9_12
# BB#11:                                # %if.then7
	movq	$128, %rdi
	callq	Perl_safesysmalloc
	movq	%rax, 120(%r14)
	movq	$128, 128(%r14)
.LBB9_12:                               # %if.end12
	testq	%r15, %r15
	jne	.LBB9_30
# BB#13:                                # %if.end12
	testq	%r13, %r13
	je	.LBB9_30
# BB#14:                                # %if.then15
	movq	16(%r13), %rax
	testq	$536870912, %rax        # imm = 0x20000000
	je	.LBB9_24
# BB#15:                                # %if.then17
	movq	%r15, 8(%rsp)           # 8-byte Spill
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB9_17
# BB#16:                                # %cond.true
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	(%r13), %rax
	movq	%r13, %r15
	movq	8(%rax), %rcx
	movq	%rcx, 40(%rsp)
	movq	(%rax), %r13
	jmp	.LBB9_18
.LBB9_17:                               # %cond.false
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	leaq	40(%rsp), %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	movq	%r13, %r15
	callq	Perl_sv_2pv_flags
	movq	40(%rsp), %rcx
	movq	%rax, %r13
.LBB9_18:                               # %cond.end
	incq	%rcx
	movq	%rcx, 32(%rsp)
	movq	$1, %rbx
	movb	%bl, 31(%rsp)
	leaq	32(%rsp), %rsi
	leaq	31(%rsp), %rdx
	movq	%r13, %rdi
	callq	Perl_bytes_from_utf8
	movzbl	31(%rsp), %ecx
	movq	%rax, %r12
	testq	%rcx, %rcx
	je	.LBB9_20
# BB#19:                                # %if.then24
	movl	%ebx, 112(%r14)
	movabsq	$.L.str30, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_20:                               # %if.end26
	cmpq	%r13, %r12
	movq	%r15, %r13
	movq	8(%rsp), %r15           # 8-byte Reload
	movq	16(%rsp), %rbx          # 8-byte Reload
	je	.LBB9_24
# BB#21:                                # %if.then28
	callq	Perl_sv_newmortal
	movq	%rax, %r13
	movq	16(%r13), %rax
	testq	$252, %rax
	jne	.LBB9_23
# BB#22:                                # %lor.rhs
	movq	$4, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_upgrade
	movq	16(%r13), %rax
.LBB9_23:                               # %lor.end
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r13)
	movq	(%r13), %rax
	movq	%r12, (%rax)
	movq	32(%rsp), %rcx
	movq	%rcx, 16(%rax)
	movq	32(%rsp), %rcx
	decq	%rcx
	movq	%rcx, 8(%rax)
.LBB9_24:                               # %if.end43
	movq	$1, %rax
	movl	%eax, 116(%r14)
	movq	176(%r14), %rcx
	movq	%rcx, 208(%r14)
	movq	168(%r14), %rcx
	movq	%rcx, 200(%r14)
	movq	152(%r14), %rcx
	movq	160(%r14), %rdx
	movq	%rdx, 192(%r14)
	movq	%rcx, 184(%r14)
	movq	16(%r13), %rcx
	testq	$67108864, %rcx         # imm = 0x4000000
	jne	.LBB9_26
# BB#25:                                # %if.then47
	movl	%eax, 112(%r14)
	movabsq	$.L.str31, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	16(%r13), %rcx
.LBB9_26:                               # %if.end49
	testq	$262144, %rcx           # imm = 0x40000
	je	.LBB9_28
# BB#27:                                # %cond.true54
	movq	(%r13), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 160(%r14)
	movq	(%rax), %rax
	jmp	.LBB9_29
.LBB9_28:                               # %cond.false61
	leaq	160(%r14), %rsi
	movq	$2, %rdx
	movq	%r13, %rdi
	callq	Perl_sv_2pv_flags
	movq	160(%r14), %rcx
.LBB9_29:                               # %cond.end65
	movq	%rax, 152(%r14)
	movq	%rax, 168(%r14)
	addq	%rax, %rcx
	movq	%rcx, 176(%r14)
.LBB9_30:                               # %if.end75
	orq	$2, %rbx
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	testq	%r15, %r15
	movq	%r15, 216(%r14)
	je	.LBB9_41
# BB#31:                                # %if.else11.i
	movq	%r13, 8(%rsp)           # 8-byte Spill
	leaq	48(%rsp), %rsi
	movq	$5, %rdx
	movq	%r15, %rdi
	callq	Perl_PerlIO_read
	cmpq	$5, %rax
	jne	.LBB9_105
# BB#32:                                # %if.end16.i
	movl	48(%rsp), %eax
	cmpq	$812938096, %rax        # imm = 0x30747370
	je	.LBB9_33
# BB#34:                                # %if.then22.i
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB9_35
# BB#37:                                # %if.else43.i
	leaq	53(%rsp), %rsi
	movq	$6, %rdx
	callq	Perl_PerlIO_read
	cmpq	$6, %rax
	jne	.LBB9_105
	jmp	.LBB9_38
.LBB9_41:                               # %if.then64.i
	movq	%r13, 8(%rsp)           # 8-byte Spill
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB9_105
# BB#42:                                # %if.then71.i
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	jmp	.LBB9_43
.LBB9_33:
	leaq	52(%rsp), %rax
	jmp	.LBB9_43
.LBB9_35:                               # %if.then25.i
	movq	168(%r14), %rax
	movq	176(%r14), %rdx
	leaq	6(%rax), %rcx
	cmpq	%rdx, %rcx
	ja	.LBB9_105
# BB#36:                                # %if.then32.i
	movzbl	4(%rax), %edx
	movw	%dx, 57(%rsp)
	movl	(%rax), %eax
	movl	%eax, 53(%rsp)
	movq	%rcx, 168(%r14)
.LBB9_38:                               # %if.end52.i
	movabsq	$old_magicstr, %rsi
	leaq	48(%rsp), %rdi
	movq	$10, %rdx
	callq	memcmp
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB9_40
# BB#39:                                # %if.then56.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str41, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_40:                               # %if.end57.i
	leaq	58(%rsp), %rax
.LBB9_43:                               # %if.end85.i
	movzbl	(%rax), %ebx
	movq	$1, %rcx
	movq	%rbx, %r12
	shrq	%cl, %r12
	testq	%r12, %r12
	jne	.LBB9_44
# BB#45:                                # %if.end85.i
	movabsq	$sv_old_retrieve, %rax
	jmp	.LBB9_46
.LBB9_44:
	movabsq	$sv_retrieve, %rax
.LBB9_46:                               # %if.end85.i
	cmpq	$3, %rbx
	movq	%rax, 232(%r14)
	jbe	.LBB9_47
# BB#48:                                # %if.then89.i
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB9_49
# BB#51:                                # %if.else106.i
	callq	PerlIO_getc
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r13, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB9_52
	jmp	.LBB9_105
.LBB9_47:                               # %lor.lhs.false.thread.i
	movl	%r12d, 224(%r14)
	xorq	%r13, %r13
	movl	%r13d, 228(%r14)
	jmp	.LBB9_79
.LBB9_49:                               # %if.then92.i
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB9_105
# BB#50:                                # %if.then99.i
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %r13d
.LBB9_52:                               # %if.end114.i
	movl	%r12d, 224(%r14)
	movl	%r13d, 228(%r14)
	cmpq	$5, %rbx
	ja	.LBB9_78
# BB#53:                                # %lor.lhs.false.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%r12, %rax
	andq	%rdx, %rax
	cmpq	$2, %rax
	jne	.LBB9_79
# BB#54:                                # %lor.lhs.false.i
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$7, %rax
	jl	.LBB9_79
# BB#55:                                # %if.then124.i
	movslq	108(%r14), %rax
	testq	%rax, %rax
	jns	.LBB9_77
# BB#56:                                # %if.then127.i
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB9_73
# BB#57:                                # %cond.false.i
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	movq	16(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	$1, %rsi
	movabsq	$.L.str37, %rdi
	callq	Perl_get_sv
	testq	$262144, (%rsp)         # 8-byte Folded Reload
                                        # imm = 0x40000
	je	.LBB9_64
# BB#58:                                # %cond.true132.i
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB9_73
# BB#59:                                # %land.rhs.i
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB9_73
# BB#60:                                # %lor.rhs.i
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB9_73
# BB#61:                                # %land.rhs139.i
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB9_62
# BB#63:                                # %land.rhs139.i
	movabsq	$1, %rax
	jmp	.LBB9_73
.LBB9_64:                               # %cond.false145.i
	movq	16(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, (%rsp)          # 8-byte Folded Reload
                                        # imm = 0x10000
	je	.LBB9_68
# BB#65:                                # %cond.true150.i
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB9_66
# BB#67:                                # %cond.true150.i
	movabsq	$1, %rax
	jmp	.LBB9_73
.LBB9_68:                               # %cond.false155.i
	movq	16(%rax), %rax
	movq	%rax, (%rsp)            # 8-byte Spill
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, (%rsp)         # 8-byte Folded Reload
                                        # imm = 0x20000
	je	.LBB9_72
# BB#69:                                # %cond.true160.i
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB9_70
# BB#71:                                # %cond.true160.i
	movabsq	$1, %rax
	jmp	.LBB9_73
.LBB9_66:
	movabsq	$0, %rax
	jmp	.LBB9_73
.LBB9_62:
	movabsq	$0, %rax
	jmp	.LBB9_73
.LBB9_72:                               # %cond.false165.i
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB9_73
.LBB9_70:
	movabsq	$0, %rax
.LBB9_73:                               # %cond.end174.i
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB9_74
# BB#75:                                # %cond.end174.i
	movabsq	$1, %rax
	jmp	.LBB9_76
.LBB9_74:
	movabsq	$0, %rax
.LBB9_76:                               # %cond.end174.i
	movl	%eax, 108(%r14)
.LBB9_77:                               # %if.end179.i
	andq	%rdx, %rax
	cmpq	$1, %rax
	je	.LBB9_79
.LBB9_78:                               # %if.then187.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str42, %rdi
	movq	$2, %rcx
	movq	$6, %r8
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	Perl_croak
.LBB9_79:                               # %if.end190.i
	movq	%rbx, %rax
	andq	$1, %rax
	movl	%eax, 80(%r14)
	jne	.LBB9_109
# BB#80:                                # %if.end194.i
	cmpq	$3, %rbx
	movabsq	$0, %rax
	movabsq	$1, %rdx
	movq	%rax, %rsi
	jbe	.LBB9_82
# BB#81:                                # %if.end194.i
	movq	%rdx, %rsi
.LBB9_82:                               # %if.end194.i
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	cmpq	$1, %r13
	jle	.LBB9_84
# BB#83:                                # %if.end194.i
	movq	%rdx, %rax
.LBB9_84:                               # %if.end194.i
	movq	216(%r14), %rdi
	andq	%rax, %rsi
	movq	%rsi, (%rsp)            # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB9_85
# BB#87:                                # %if.else217.i
	callq	PerlIO_getc
	movq	%rax, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r12, %rcx
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	je	.LBB9_105
# BB#88:                                # %if.end224.i
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	movq	(%rsp), %rax            # 8-byte Reload
	leaq	3(%rax,%r12), %r13
	je	.LBB9_89
# BB#92:                                # %if.else246.i
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	48(%rsp), %rsi
	movq	%r13, %rdx
	callq	Perl_PerlIO_read
	cmpq	%r13, %rax
	jne	.LBB9_105
	jmp	.LBB9_93
.LBB9_85:                               # %if.then203.i
	movq	168(%r14), %rcx
	movq	176(%r14), %rax
	cmpq	%rax, %rcx
	jae	.LBB9_105
# BB#86:                                # %if.end224.thread.i
	leaq	1(%rcx), %rsi
	movq	%rsi, 168(%r14)
	movzbl	(%rcx), %r12d
	movq	(%rsp), %rcx            # 8-byte Reload
	leaq	3(%rcx,%r12), %r13
	jmp	.LBB9_90
.LBB9_89:                               # %if.end224.i.if.then228.i_crit_edge
	movq	168(%r14), %rsi
	movq	176(%r14), %rax
.LBB9_90:                               # %if.then228.i
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	leaq	(%rsi,%r13), %rbx
	cmpq	%rax, %rbx
	jbe	.LBB9_91
.LBB9_105:                              # %if.then78
	movq	$1, %rax
	movl	%eax, 112(%r14)
	cmpq	$0, 216(%r14)
	je	.LBB9_107
# BB#106:
	movabsq	$.L.str33, %rsi
	jmp	.LBB9_108
.LBB9_107:                              # %select.mid
	movabsq	$.L.str34, %rsi
.LBB9_108:                              # %select.end
	movabsq	$.L.str32, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_109:                              # %if.end83
	movq	$1, %rbx
	testq	%r15, %r15
	jne	.LBB9_116
# BB#110:                               # %cond.false86
	movq	8(%rsp), %rdi           # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB9_115
# BB#111:                               # %cond.true88
	movzbl	17(%rdi), %eax
	xorq	%rbx, %rbx
	testq	$224, %rax
	je	.LBB9_116
# BB#112:                               # %land.rhs
	callq	Perl_sv_tainted
	testq	$255, %rax
	je	.LBB9_113
# BB#114:                               # %land.rhs
	movabsq	$1, %rbx
	jmp	.LBB9_116
.LBB9_115:                              # %cond.false95
	movl	84(%r14), %ebx
	jmp	.LBB9_116
.LBB9_113:
	movabsq	$0, %rbx
.LBB9_116:                              # %cond.end98
	callq	Perl_newHV
	movq	%rax, 56(%r14)
	movq	232(%r14), %rcx
	movabsq	$sv_old_retrieve, %rdx
	xorq	%rax, %rax
	cmpq	%rdx, %rcx
	jne	.LBB9_118
# BB#117:                               # %cond.true.i
	callq	Perl_newHV
.LBB9_118:                              # %init_retrieve_context.exit
	movq	%rax, 8(%r14)
	callq	Perl_newAV
	movq	%rax, 24(%r14)
	movq	$-1, 32(%r14)
	callq	Perl_newAV
	movq	%rax, 48(%r14)
	movq	$0, 72(%r14)
	movq	$0, 64(%r14)
	movq	16(%rsp), %rax          # 8-byte Reload
	movl	%eax, 4(%r14)
	movl	%ebx, 84(%r14)
	movq	$1, %rax
	movl	%eax, (%r14)
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movl	%r13d, 108(%r14)
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r12
	testq	%r15, %r15
	jne	.LBB9_121
# BB#119:                               # %init_retrieve_context.exit
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	je	.LBB9_121
# BB#120:                               # %if.then104
	xorq	%rax, %rax
	movl	%eax, 116(%r14)
	movq	208(%r14), %rax
	movq	%rax, 176(%r14)
	movq	200(%r14), %rax
	movq	%rax, 168(%r14)
	movq	184(%r14), %rax
	movq	192(%r14), %rcx
	movq	%rcx, 160(%r14)
	movq	%rax, 152(%r14)
.LBB9_121:                              # %if.end108
	movq	8(%r14), %r15
	movq	%r14, %rdi
	callq	clean_retrieve_context
	movq	240(%r14), %rax
	testq	%rax, %rax
	je	.LBB9_123
# BB#122:                               # %free_context.exit
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rbx
	movq	248(%r14), %rdi
	callq	Perl_sv_free
	movq	%rbx, Context_ptr(%rip)
.LBB9_123:                              # %if.end113
	testq	%r12, %r12
	je	.LBB9_124
# BB#125:                               # %if.end116
	testq	%r15, %r15
	je	.LBB9_129
# BB#126:                               # %if.then118
	movq	%r12, %rdi
	callq	sv_type
	testq	%r13, %rax
	jne	.LBB9_129
# BB#127:                               # %land.lhs.true123
	movq	(%r12), %rax
	movq	(%rax), %rax
	testq	%rax, %rax
	je	.LBB9_129
# BB#128:                               # %land.lhs.true126
	movzbl	17(%rax), %eax
	testq	$16, %rax
	jne	.LBB9_135
.LBB9_129:                              # %if.end132
	movzbl	17(%r12), %eax
	testq	$16, %rax
	je	.LBB9_134
# BB#130:                               # %if.then136
	movq	(%r12), %rax
	movq	48(%rax), %rbx
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r12
	testq	%rbx, %rbx
	je	.LBB9_135
# BB#131:                               # %if.then136
	movq	PL_amagic_generation(%rip), %rax
	testq	%rax, %rax
	je	.LBB9_135
# BB#132:                               # %land.lhs.true145
	movq	%rbx, %rdi
	callq	Perl_Gv_AMupdate
	testq	$255, %rax
	je	.LBB9_135
# BB#133:                               # %if.then149
	orq	$268435456, 16(%r12)    # imm = 0x10000000
	jmp	.LBB9_135
.LBB9_124:
	movabsq	$PL_sv_undef, %r12
	jmp	.LBB9_135
.LBB9_134:                              # %if.end153
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r12
.LBB9_135:                              # %return
	movq	%r12, %rax
	addq	$320, %rsp              # imm = 0x140
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB9_91:                               # %if.then236.i
	leaq	48(%rsp), %rdi
	movq	%r13, %rdx
	callq	memcpy
	movq	%rbx, 168(%r14)
.LBB9_93:                               # %if.end256.i
	movq	$32, %rcx
	movq	%r12, %r13
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	%rbx, %r12
	cmpq	$4, %r12
	jne	.LBB9_95
# BB#94:                                # %lor.lhs.false260.i
	movabsq	$byteorderstr, %rsi
	leaq	48(%rsp), %rdi
	movq	%r13, %rdx
	callq	memcmp
	testq	%rbx, %rax
	je	.LBB9_96
.LBB9_95:                               # %if.then265.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str43, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_96:                               # %if.end267.i
	movzbl	48(%rsp,%r13), %eax
	cmpq	$4, %rax
	je	.LBB9_98
# BB#97:                                # %if.then276.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_98:                               # %if.end278.i
	movzbl	49(%rsp,%r13), %eax
	cmpq	$8, %rax
	movq	(%rsp), %rbx            # 8-byte Reload
	je	.LBB9_100
# BB#99:                                # %if.then284.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str45, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_100:                              # %if.end286.i
	movzbl	50(%rsp,%r13), %eax
	cmpq	$8, %rax
	je	.LBB9_102
# BB#101:                               # %if.then291.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str46, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB9_102:                              # %if.end293.i
	testq	%rbx, %rbx
	je	.LBB9_109
# BB#103:                               # %if.then295.i
	leaq	48(%rsp), %rax
	movzbl	3(%r13,%rax), %eax
	cmpq	$8, %rax
	je	.LBB9_109
# BB#104:                               # %if.then301.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str47, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	jmp	.LBB9_109
.Ltmp47:
	.size	do_retrieve, .Ltmp47-do_retrieve
	.cfi_endproc

	.globl	mretrieve
	.align	16, 0x90
	.type	mretrieve,@function
mretrieve:                              # @mretrieve
	.cfi_startproc
# BB#0:                                 # %entry
	movq	%rdi, %rax
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	jmp	do_retrieve  # TAILCALL
.Ltmp48:
	.size	mretrieve, .Ltmp48-mretrieve
	.cfi_endproc

	.globl	dclone
	.align	16, 0x90
	.type	dclone,@function
dclone:                                 # @dclone
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp54:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp55:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp56:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp57:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp58:
	.cfi_def_cfa_offset 48
.Ltmp59:
	.cfi_offset %rbx, -40
.Ltmp60:
	.cfi_offset %r12, -32
.Ltmp61:
	.cfi_offset %r14, -24
.Ltmp62:
	.cfi_offset %r15, -16
	movq	Context_ptr(%rip), %rax
	movl	112(%rax), %ecx
	movq	%rdi, %r14
	testq	%rcx, %rcx
	je	.LBB11_8
# BB#1:                                 # %if.then
	movl	116(%rax), %ecx
	testq	%rcx, %rcx
	je	.LBB11_3
# BB#2:                                 # %if.then.i
	xorq	%rcx, %rcx
	movl	%ecx, 116(%rax)
	movq	208(%rax), %rcx
	movq	%rcx, 176(%rax)
	movq	200(%rax), %rcx
	movq	%rcx, 168(%rax)
	movq	184(%rax), %rcx
	movq	192(%rax), %rdx
	movq	%rdx, 160(%rax)
	movq	%rcx, 152(%rax)
.LBB11_3:                               # %if.end.i
	movl	4(%rax), %ecx
	testq	$2, %rcx
	je	.LBB11_5
# BB#4:                                 # %if.then3.i
	movq	%rax, %rdi
	callq	clean_retrieve_context
	jmp	.LBB11_8
.LBB11_5:                               # %if.else.i
	testq	$1, %rcx
	je	.LBB11_7
# BB#6:                                 # %if.then7.i
	movq	%rax, %rdi
	callq	clean_store_context
	jmp	.LBB11_8
.LBB11_7:                               # %if.else8.i
	xorq	%rdx, %rdx
	movl	%edx, (%rax)
	movl	%edx, 112(%rax)
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	andq	%rdx, %rcx
	movl	%ecx, 4(%rax)
.LBB11_8:                               # %if.end
	xorq	%rdi, %rdi
	movq	$4, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rsi
	callq	do_store
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	testq	%r12, %rax
	je	.LBB11_19
# BB#9:                                 # %if.end3
	movq	Context_ptr(%rip), %r15
	movq	152(%r15), %rax
	movq	168(%r15), %rbx
	subq	%rax, %rbx
	testq	%rax, %rax
	jne	.LBB11_11
# BB#10:                                # %if.then9
	movq	$8192, %rdi             # imm = 0x2000
	callq	Perl_safesysmalloc
	movq	%rax, 152(%r15)
	movq	$8192, 160(%r15)        # imm = 0x2000
.LBB11_11:                              # %if.end14
	testq	%r12, %rbx
	movq	%rax, 168(%r15)
	je	.LBB11_13
# BB#12:                                # %if.then20
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rbx, %rax
	jmp	.LBB11_14
.LBB11_19:                              # %return
	movabsq	$PL_sv_undef, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.LBB11_13:                              # %if.else
	movq	160(%r15), %rcx
	addq	%rcx, %rax
.LBB11_14:                              # %if.end31
	movq	%rax, 176(%r15)
	movzbl	17(%r14), %ecx
	xorq	%rax, %rax
	testq	$224, %rcx
	je	.LBB11_18
# BB#15:                                # %land.rhs
	movq	%r14, %rdi
	callq	Perl_sv_tainted
	testq	$255, %rax
	je	.LBB11_16
# BB#17:                                # %land.rhs
	movabsq	$1, %rax
	jmp	.LBB11_18
.LBB11_16:
	movabsq	$0, %rax
.LBB11_18:                              # %land.end
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	movq	$4, %rdx
	movl	%eax, 84(%r15)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	jmp	do_retrieve  # TAILCALL
.Ltmp63:
	.size	dclone, .Ltmp63-dclone
	.cfi_endproc

	.globl	XS_Storable__Cxt_DESTROY
	.align	16, 0x90
	.type	XS_Storable__Cxt_DESTROY,@function
XS_Storable__Cxt_DESTROY:               # @XS_Storable__Cxt_DESTROY
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp68:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp69:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp70:
	.cfi_def_cfa_offset 32
.Ltmp71:
	.cfi_offset %rbx, -32
.Ltmp72:
	.cfi_offset %r14, -24
.Ltmp73:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %r14
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rdx
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	movq	%r14, %rbx
	subq	%rdx, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	$1, %rbx
	je	.LBB12_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB12_2:                               # %if.end
	movq	8(%rax,%r15,8), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	(%rax), %r15
	movq	120(%r15), %rdi
	movq	$3, %rcx
	shlq	%cl, %rbx
	testq	%rdi, %rdi
	je	.LBB12_4
# BB#3:                                 # %if.then8
	callq	Perl_safesysfree
.LBB12_4:                               # %if.end11
	movl	116(%r15), %eax
	subq	%rbx, %r14
	testq	%rax, %rax
	jne	.LBB12_7
# BB#5:                                 # %land.lhs.true
	movq	152(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB12_9
# BB#6:                                 # %if.end18
	callq	Perl_safesysfree
	movl	116(%r15), %eax
	testq	%rax, %rax
	je	.LBB12_9
.LBB12_7:                               # %land.lhs.true21
	movq	184(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB12_9
# BB#8:                                 # %if.then24
	callq	Perl_safesysfree
.LBB12_9:                               # %if.end27
	movq	%r14, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp74:
	.size	XS_Storable__Cxt_DESTROY, .Ltmp74-XS_Storable__Cxt_DESTROY
	.cfi_endproc

	.globl	XS_Storable_init_perinterp
	.align	16, 0x90
	.type	XS_Storable_init_perinterp,@function
XS_Storable_init_perinterp:             # @XS_Storable_init_perinterp
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp80:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp81:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp82:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp83:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp84:
	.cfi_def_cfa_offset 48
.Ltmp85:
	.cfi_offset %rbx, -40
.Ltmp86:
	.cfi_offset %r12, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r15
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	$3, %rcx
	shlq	%cl, %r15
	movq	$3, %rcx
	sarq	%cl, %r15
	cmpq	%rax, %rdx
	je	.LBB13_2
# BB#1:                                 # %if.then
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB13_2:                               # %if.end
	movq	$255, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r14
	movabsq	$.L.str29, %rdi
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	(%rax), %rbx
	xorq	%rsi, %rsi
	movq	$248, %rdx
	xorq	%r12, %r12
	movq	%rbx, %rdi
	callq	memset
	movq	%r14, 248(%rbx)
	movq	%rbx, Context_ptr(%rip)
	movl	%r12d, 80(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 88(%rbx)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp89:
	.size	XS_Storable_init_perinterp, .Ltmp89-XS_Storable_init_perinterp
	.cfi_endproc

	.globl	XS_Storable_pstore
	.align	16, 0x90
	.type	XS_Storable_pstore,@function
XS_Storable_pstore:                     # @XS_Storable_pstore
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
.Ltmp101:
	.cfi_offset %rbx, -48
.Ltmp102:
	.cfi_offset %r12, -40
.Ltmp103:
	.cfi_offset %r13, -32
.Ltmp104:
	.cfi_offset %r14, -24
.Ltmp105:
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
	je	.LBB14_2
# BB#1:                                 # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB14_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r13
	callq	Perl_sv_2io
	movq	(%rax), %rcx
	movq	PL_op(%rip), %rax
	movq	PL_stack_base(%rip), %rdx
	movq	64(%rcx), %r14
	movzbl	37(%rax), %ecx
	movq	16(%rdx,%rbx,8), %r15
	testq	$32, %rcx
	je	.LBB14_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r12
	jmp	.LBB14_5
.LBB14_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r12
.LBB14_5:                               # %cond.end
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	do_store
	movq	PL_stack_base(%rip), %r14
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setiv
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB14_7
# BB#6:                                 # %if.then16
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB14_7:                               # %if.end18
	movq	%r12, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp106:
	.size	XS_Storable_pstore, .Ltmp106-XS_Storable_pstore
	.cfi_endproc

	.globl	XS_Storable_net_pstore
	.align	16, 0x90
	.type	XS_Storable_net_pstore,@function
XS_Storable_net_pstore:                 # @XS_Storable_net_pstore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp113:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp114:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp115:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp116:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp117:
	.cfi_def_cfa_offset 48
.Ltmp118:
	.cfi_offset %rbx, -48
.Ltmp119:
	.cfi_offset %r12, -40
.Ltmp120:
	.cfi_offset %r13, -32
.Ltmp121:
	.cfi_offset %r14, -24
.Ltmp122:
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
	je	.LBB15_2
# BB#1:                                 # %if.then
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB15_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	leaq	1(%rbx), %r13
	callq	Perl_sv_2io
	movq	(%rax), %rcx
	movq	PL_op(%rip), %rax
	movq	PL_stack_base(%rip), %rdx
	movq	64(%rcx), %r14
	movzbl	37(%rax), %ecx
	movq	16(%rdx,%rbx,8), %r15
	testq	$32, %rcx
	je	.LBB15_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r12
	jmp	.LBB15_5
.LBB15_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r12
.LBB15_5:                               # %cond.end
	xorq	%rdx, %rdx
	movq	$1, %rcx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	do_store
	movq	PL_stack_base(%rip), %r14
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%r12, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_setiv
	movzbl	17(%r12), %eax
	testq	$64, %rax
	je	.LBB15_7
# BB#6:                                 # %if.then16
	movq	%r12, %rdi
	callq	Perl_mg_set
.LBB15_7:                               # %if.end18
	movq	%r12, 8(%r14,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp123:
	.size	XS_Storable_net_pstore, .Ltmp123-XS_Storable_net_pstore
	.cfi_endproc

	.globl	XS_Storable_mstore
	.align	16, 0x90
	.type	XS_Storable_mstore,@function
XS_Storable_mstore:                     # @XS_Storable_mstore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp127:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp128:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp129:
	.cfi_def_cfa_offset 32
.Ltmp130:
	.cfi_offset %rbx, -24
.Ltmp131:
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
	je	.LBB16_2
# BB#1:                                 # %if.then
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB16_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rsi
	leaq	1(%rbx), %r14
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	callq	do_store
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB16_3
# BB#4:                                 # %if.end
	movq	(%rsp), %rax
	jmp	.LBB16_5
.LBB16_3:
	movabsq	$PL_sv_undef, %rax
.LBB16_5:                               # %if.end
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp132:
	.size	XS_Storable_mstore, .Ltmp132-XS_Storable_mstore
	.cfi_endproc

	.globl	XS_Storable_net_mstore
	.align	16, 0x90
	.type	XS_Storable_net_mstore,@function
XS_Storable_net_mstore:                 # @XS_Storable_net_mstore
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp136:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp137:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp138:
	.cfi_def_cfa_offset 32
.Ltmp139:
	.cfi_offset %rbx, -24
.Ltmp140:
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
	je	.LBB17_2
# BB#1:                                 # %if.then
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB17_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rsi
	leaq	1(%rbx), %r14
	leaq	(%rsp), %r8
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	movq	$1, %rcx
	callq	do_store
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB17_3
# BB#4:                                 # %if.end
	movq	(%rsp), %rax
	jmp	.LBB17_5
.LBB17_3:
	movabsq	$PL_sv_undef, %rax
.LBB17_5:                               # %if.end
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp141:
	.size	XS_Storable_net_mstore, .Ltmp141-XS_Storable_net_mstore
	.cfi_endproc

	.globl	XS_Storable_pretrieve
	.align	16, 0x90
	.type	XS_Storable_pretrieve,@function
XS_Storable_pretrieve:                  # @XS_Storable_pretrieve
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp145:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp146:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp147:
	.cfi_def_cfa_offset 32
.Ltmp148:
	.cfi_offset %rbx, -24
.Ltmp149:
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
	leaq	1(%rbx), %r14
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB18_2
# BB#1:                                 # %if.then
	movabsq	$.L.str6, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB18_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2io
	movq	(%rax), %rax
	movq	56(%rax), %rdi
	xorq	%rsi, %rsi
	xorq	%rdx, %rdx
	callq	do_retrieve
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp150:
	.size	XS_Storable_pretrieve, .Ltmp150-XS_Storable_pretrieve
	.cfi_endproc

	.globl	XS_Storable_mretrieve
	.align	16, 0x90
	.type	XS_Storable_mretrieve,@function
XS_Storable_mretrieve:                  # @XS_Storable_mretrieve
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp154:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp155:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp156:
	.cfi_def_cfa_offset 32
.Ltmp157:
	.cfi_offset %rbx, -24
.Ltmp158:
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
	leaq	1(%rbx), %r14
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB19_2
# BB#1:                                 # %if.then
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB19_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rsi
	xorq	%rdi, %rdi
	xorq	%rdx, %rdx
	callq	do_retrieve
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp159:
	.size	XS_Storable_mretrieve, .Ltmp159-XS_Storable_mretrieve
	.cfi_endproc

	.globl	XS_Storable_dclone
	.align	16, 0x90
	.type	XS_Storable_dclone,@function
XS_Storable_dclone:                     # @XS_Storable_dclone
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp163:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp164:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp165:
	.cfi_def_cfa_offset 32
.Ltmp166:
	.cfi_offset %rbx, -24
.Ltmp167:
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
	leaq	1(%rbx), %r14
	subq	%rsi, %rdx
	cmpq	$8, %rdx
	je	.LBB20_2
# BB#1:                                 # %if.then
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	PL_stack_base(%rip), %rax
.LBB20_2:                               # %if.end
	movq	8(%rax,%rbx,8), %rdi
	callq	dclone
	movq	PL_stack_base(%rip), %rcx
	movq	%rax, 8(%rcx,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	movq	8(%rax,%rbx,8), %rdi
	callq	Perl_sv_2mortal
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r14,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp168:
	.size	XS_Storable_dclone, .Ltmp168-XS_Storable_dclone
	.cfi_endproc

	.globl	XS_Storable_last_op_in_netorder
	.align	16, 0x90
	.type	XS_Storable_last_op_in_netorder,@function
XS_Storable_last_op_in_netorder:        # @XS_Storable_last_op_in_netorder
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp174:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp175:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp176:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp177:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp178:
	.cfi_def_cfa_offset 48
.Ltmp179:
	.cfi_offset %rbx, -40
.Ltmp180:
	.cfi_offset %r12, -32
.Ltmp181:
	.cfi_offset %r14, -24
.Ltmp182:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rdx
	je	.LBB21_2
# BB#1:                                 # %if.then
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB21_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	leaq	1(%rbx), %r15
	testq	$32, %rcx
	je	.LBB21_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB21_5
.LBB21_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB21_5:                               # %cond.end
	movq	Context_ptr(%rip), %rax
	movq	PL_stack_base(%rip), %r12
	movslq	80(%rax), %rsi
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB21_7
# BB#6:                                 # %if.then11
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB21_7:                               # %if.end13
	movq	%r14, 8(%r12,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp183:
	.size	XS_Storable_last_op_in_netorder, .Ltmp183-XS_Storable_last_op_in_netorder
	.cfi_endproc

	.globl	XS_Storable_is_storing
	.align	16, 0x90
	.type	XS_Storable_is_storing,@function
XS_Storable_is_storing:                 # @XS_Storable_is_storing
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp189:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp190:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp191:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp192:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp193:
	.cfi_def_cfa_offset 48
.Ltmp194:
	.cfi_offset %rbx, -40
.Ltmp195:
	.cfi_offset %r12, -32
.Ltmp196:
	.cfi_offset %r14, -24
.Ltmp197:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rdx
	je	.LBB22_2
# BB#1:                                 # %if.then
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB22_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	leaq	1(%rbx), %r15
	testq	$32, %rcx
	je	.LBB22_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB22_5
.LBB22_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB22_5:                               # %cond.end
	movq	Context_ptr(%rip), %rax
	movl	(%rax), %ecx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB22_7
# BB#6:                                 # %land.rhs.i
	movzbl	4(%rax), %esi
	andq	$1, %rsi
.LBB22_7:                               # %is_storing.exit
	movq	PL_stack_base(%rip), %r12
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB22_9
# BB#8:                                 # %if.then11
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB22_9:                               # %if.end13
	movq	%r14, 8(%r12,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp198:
	.size	XS_Storable_is_storing, .Ltmp198-XS_Storable_is_storing
	.cfi_endproc

	.globl	XS_Storable_is_retrieving
	.align	16, 0x90
	.type	XS_Storable_is_retrieving,@function
XS_Storable_is_retrieving:              # @XS_Storable_is_retrieving
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp204:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp205:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp206:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp207:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp208:
	.cfi_def_cfa_offset 48
.Ltmp209:
	.cfi_offset %rbx, -40
.Ltmp210:
	.cfi_offset %r12, -32
.Ltmp211:
	.cfi_offset %r14, -24
.Ltmp212:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_stack_sp(%rip), %rdx
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %rbx
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%rbx,8), %rax
	movq	$3, %rcx
	shlq	%cl, %rbx
	movq	$3, %rcx
	sarq	%cl, %rbx
	cmpq	%rax, %rdx
	je	.LBB23_2
# BB#1:                                 # %if.then
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB23_2:                               # %if.end
	movq	PL_op(%rip), %rax
	movzbl	37(%rax), %ecx
	leaq	1(%rbx), %r15
	testq	$32, %rcx
	je	.LBB23_4
# BB#3:                                 # %cond.true
	movq	24(%rax), %rax
	movq	PL_curpad(%rip), %rcx
	movq	(%rcx,%rax,8), %r14
	jmp	.LBB23_5
.LBB23_4:                               # %cond.false
	callq	Perl_sv_newmortal
	movq	%rax, %r14
.LBB23_5:                               # %cond.end
	movq	Context_ptr(%rip), %rax
	movl	(%rax), %ecx
	xorq	%rsi, %rsi
	testq	%rcx, %rcx
	je	.LBB23_7
# BB#6:                                 # %land.rhs.i
	movzbl	4(%rax), %esi
	andq	$2, %rsi
	movq	$1, %rcx
	shrq	%cl, %rsi
.LBB23_7:                               # %is_retrieving.exit
	movq	PL_stack_base(%rip), %r12
	movq	%r14, %rdi
	callq	Perl_sv_setiv
	movzbl	17(%r14), %eax
	testq	$64, %rax
	je	.LBB23_9
# BB#8:                                 # %if.then11
	movq	%r14, %rdi
	callq	Perl_mg_set
.LBB23_9:                               # %if.end13
	movq	%r14, 8(%r12,%rbx,8)
	movq	PL_stack_base(%rip), %rax
	leaq	(%rax,%r15,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp213:
	.size	XS_Storable_is_retrieving, .Ltmp213-XS_Storable_is_retrieving
	.cfi_endproc

	.globl	boot_Storable
	.align	16, 0x90
	.type	boot_Storable,@function
boot_Storable:                          # @boot_Storable
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp220:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp221:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp222:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp223:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp224:
	.cfi_def_cfa_offset 48
.Ltmp225:
	.cfi_offset %rbx, -48
.Ltmp226:
	.cfi_offset %r12, -40
.Ltmp227:
	.cfi_offset %r13, -32
.Ltmp228:
	.cfi_offset %r14, -24
.Ltmp229:
	.cfi_offset %r15, -16
	movq	PL_markstack_ptr(%rip), %rax
	leaq	-8(%rax), %rcx
	movq	%rcx, PL_markstack_ptr(%rip)
	movq	(%rax), %r13
	movabsq	$.L.str13, %rdi
	movabsq	$XS_Storable__Cxt_DESTROY, %rsi
	movabsq	$.L.str12, %r12
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str14, %rdi
	movabsq	$XS_Storable_init_perinterp, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str15, %r14
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str16, %rdi
	movabsq	$XS_Storable_pstore, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str17, %rbx
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str18, %rdi
	movabsq	$XS_Storable_net_pstore, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str19, %rdi
	movabsq	$XS_Storable_mstore, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movabsq	$.L.str20, %r15
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str21, %rdi
	movabsq	$XS_Storable_net_mstore, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str22, %rdi
	movabsq	$XS_Storable_pretrieve, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str23, %rdi
	movabsq	$XS_Storable_mretrieve, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str24, %rdi
	movabsq	$XS_Storable_dclone, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str25, %rdi
	movabsq	$XS_Storable_last_op_in_netorder, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str26, %rdi
	movabsq	$XS_Storable_is_storing, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movabsq	$.L.str27, %rdi
	movabsq	$XS_Storable_is_retrieving, %rsi
	movq	%r12, %rdx
	callq	Perl_newXS
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_setpv
	movq	$255, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r14
	movabsq	$.L.str29, %rdi
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%r14, %rdi
	movq	%rax, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	(%rax), %rbx
	xorq	%rsi, %rsi
	movq	$248, %rdx
	xorq	%r15, %r15
	movq	%rbx, %rdi
	callq	memset
	movq	%r14, 248(%rbx)
	movq	%rbx, Context_ptr(%rip)
	movl	%r15d, 80(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 88(%rbx)
	movabsq	$.L.str28, %rdi
	movq	$2, %rsi
	movq	$4, %rdx
	callq	Perl_gv_fetchpv
	movq	PL_stack_base(%rip), %rax
	movq	$PL_sv_yes, 8(%rax,%r13,8)
	movq	PL_stack_base(%rip), %rax
	leaq	8(%rax,%r13,8), %rax
	movq	%rax, PL_stack_sp(%rip)
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp230:
	.size	boot_Storable, .Ltmp230-boot_Storable
	.cfi_endproc

	.align	16, 0x90
	.type	clean_retrieve_context,@function
clean_retrieve_context:                 # @clean_retrieve_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp234:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp235:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp236:
	.cfi_def_cfa_offset 32
.Ltmp237:
	.cfi_offset %rbx, -24
.Ltmp238:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	24(%rbx), %r14
	testq	%r14, %r14
	je	.LBB25_2
# BB#1:                                 # %if.then
	movq	$0, 24(%rbx)
	movq	%r14, %rdi
	callq	Perl_av_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB25_2:                               # %if.end
	movq	$-1, 32(%rbx)
	movq	48(%rbx), %r14
	testq	%r14, %r14
	je	.LBB25_4
# BB#3:                                 # %if.then5
	movq	$0, 48(%rbx)
	movq	%r14, %rdi
	callq	Perl_av_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB25_4:                               # %if.end9
	movq	56(%rbx), %r14
	testq	%r14, %r14
	je	.LBB25_6
# BB#5:                                 # %if.then11
	movq	$0, 56(%rbx)
	movq	%r14, %rdi
	callq	Perl_hv_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB25_6:                               # %if.end15
	movq	8(%rbx), %r14
	testq	%r14, %r14
	je	.LBB25_8
# BB#7:                                 # %if.then17
	movq	$0, 8(%rbx)
	movq	%r14, %rdi
	callq	Perl_hv_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB25_8:                               # %if.end21
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 108(%rbx)
	xorq	%rax, %rax
	movl	%eax, (%rbx)
	movl	%eax, 112(%rbx)
	movl	4(%rbx), %eax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	movl	%ecx, 4(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp239:
	.size	clean_retrieve_context, .Ltmp239-clean_retrieve_context
	.cfi_endproc

	.align	16, 0x90
	.type	clean_store_context,@function
clean_store_context:                    # @clean_store_context
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp243:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp244:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp245:
	.cfi_def_cfa_offset 32
.Ltmp246:
	.cfi_offset %rbx, -24
.Ltmp247:
	.cfi_offset %r14, -16
	movq	%rdi, %rbx
	movq	8(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB26_4
# BB#1:                                 # %if.then
	callq	Perl_hv_iterinit
	jmp	.LBB26_3
	.align	16, 0x90
.LBB26_2:                               # %while.body
                                        #   in Loop: Header=BB26_3 Depth=1
	movq	$PL_sv_undef, 16(%rax)
.LBB26_3:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	movq	8(%rbx), %rdi
	callq	Perl_hv_iternext
	testq	%rax, %rax
	jne	.LBB26_2
.LBB26_4:                               # %if.end
	movq	40(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB26_8
# BB#5:                                 # %if.then6
	callq	Perl_hv_iterinit
	jmp	.LBB26_7
	.align	16, 0x90
.LBB26_6:                               # %while.body13
                                        #   in Loop: Header=BB26_7 Depth=1
	movq	$PL_sv_undef, 16(%rax)
.LBB26_7:                               # %while.body13
                                        # =>This Inner Loop Header: Depth=1
	movq	40(%rbx), %rdi
	callq	Perl_hv_iternext
	testq	%rax, %rax
	jne	.LBB26_6
.LBB26_8:                               # %if.end16
	movq	8(%rbx), %r14
	testq	%r14, %r14
	je	.LBB26_10
# BB#9:                                 # %if.then19
	movq	$0, 8(%rbx)
	movq	%r14, %rdi
	callq	Perl_hv_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB26_10:                              # %if.end23
	movq	40(%rbx), %r14
	testq	%r14, %r14
	je	.LBB26_12
# BB#11:                                # %if.then26
	movq	$0, 40(%rbx)
	movq	%r14, %rdi
	callq	Perl_hv_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB26_12:                              # %if.end30
	movq	56(%rbx), %r14
	testq	%r14, %r14
	je	.LBB26_14
# BB#13:                                # %if.then32
	movq	$0, 56(%rbx)
	movq	%r14, %rdi
	callq	Perl_hv_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB26_14:                              # %if.end36
	movq	16(%rbx), %r14
	testq	%r14, %r14
	je	.LBB26_16
# BB#15:                                # %if.then38
	movq	$0, 16(%rbx)
	movq	%r14, %rdi
	callq	Perl_av_undef
	movq	%r14, %rdi
	callq	Perl_sv_free
.LBB26_16:                              # %if.end42
	movq	$-1, 88(%rbx)
	movq	96(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB26_18
# BB#17:                                # %if.then44
	callq	Perl_sv_free
.LBB26_18:                              # %if.end46
	movq	$0, 96(%rbx)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movl	%eax, 104(%rbx)
	xorq	%rax, %rax
	movl	%eax, (%rbx)
	movl	%eax, 112(%rbx)
	movl	4(%rbx), %eax
	movabsq	$4294967292, %rcx       # imm = 0xFFFFFFFC
	andq	%rax, %rcx
	movl	%ecx, 4(%rbx)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp248:
	.size	clean_store_context, .Ltmp248-clean_store_context
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve,@function
retrieve:                               # @retrieve
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp255:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp256:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp257:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp258:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp259:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp260:
	.cfi_def_cfa_offset 96
.Ltmp261:
	.cfi_offset %rbx, -48
.Ltmp262:
	.cfi_offset %r12, -40
.Ltmp263:
	.cfi_offset %r13, -32
.Ltmp264:
	.cfi_offset %r14, -24
.Ltmp265:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	cmpq	$0, 8(%r12)
	movq	%rsi, %r14
	je	.LBB27_27
# BB#1:                                 # %if.then
	movl	80(%r12), %eax
	movq	216(%r12), %rdi
	testq	%rax, %rax
	je	.LBB27_7
# BB#2:                                 # %if.then2
	testq	%rdi, %rdi
	je	.LBB27_3
# BB#5:                                 # %if.else12
	leaq	32(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB27_103
# BB#6:                                 # %if.else12.if.end17_crit_edge
	movq	32(%rsp), %rax
	movq	%rax, 40(%rsp)
	jmp	.LBB27_11
.LBB27_27:                              # %if.end89
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB27_28
# BB#30:                                # %if.else106
	callq	PerlIO_getc
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rbx
	cmpq	%rax, %rcx
	jne	.LBB27_31
	jmp	.LBB27_103
.LBB27_7:                               # %if.else18
	testq	%rdi, %rdi
	je	.LBB27_8
# BB#10:                                # %if.else36
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB27_103
	jmp	.LBB27_11
.LBB27_3:                               # %if.then4
	movq	168(%r12), %rax
	movq	176(%r12), %rdx
	leaq	8(%rax), %rcx
	xorq	%rbx, %rbx
	cmpq	%rdx, %rcx
	ja	.LBB27_103
# BB#4:                                 # %if.then6
	movq	(%rax), %rax
	movq	%rax, 32(%rsp)
	movq	%rcx, 168(%r12)
	movq	%rax, 40(%rsp)
	jmp	.LBB27_11
.LBB27_28:                              # %if.then92
	movq	168(%r12), %rax
	movq	176(%r12), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	jae	.LBB27_103
# BB#29:                                # %if.then99
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r12)
	movzbl	(%rax), %r15d
.LBB27_31:                              # %if.end113
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r15
	je	.LBB27_32
# BB#41:                                # %if.else171
	movq	$32, %rcx
	movq	%r15, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$27, %rax
	jl	.LBB27_67
# BB#42:                                # %land.lhs.true
	movslq	228(%r12), %rax
	cmpq	$7, %rax
	jl	.LBB27_67
# BB#43:                                # %if.then176
	movslq	108(%r12), %rax
	testq	%rax, %rax
	jns	.LBB27_65
# BB#44:                                # %if.then179
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	movq	%rax, %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	je	.LBB27_61
# BB#45:                                # %cond.false183
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	movq	%rdi, %r13
	callq	Perl_get_sv
	movq	16(%rax), %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	$1, %rsi
	movq	%r13, %rdi
	callq	Perl_get_sv
	testq	$262144, 8(%rsp)        # 8-byte Folded Reload
                                        # imm = 0x40000
	je	.LBB27_52
# BB#46:                                # %cond.true188
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB27_61
# BB#47:                                # %land.rhs192
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB27_61
# BB#48:                                # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB27_61
# BB#49:                                # %land.rhs197
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB27_50
# BB#51:                                # %land.rhs197
	movabsq	$1, %rax
	jmp	.LBB27_61
.LBB27_8:                               # %if.then21
	movq	168(%r12), %rax
	movq	176(%r12), %rdx
	leaq	8(%rax), %rcx
	xorq	%rbx, %rbx
	cmpq	%rdx, %rcx
	ja	.LBB27_103
# BB#9:                                 # %if.then28
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)
	movq	%rcx, 168(%r12)
.LBB27_11:                              # %if.end43
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB27_12
# BB#14:                                # %if.else57
	callq	PerlIO_getc
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	xorq	%rbx, %rbx
	cmpq	%rax, %rcx
	jne	.LBB27_15
	jmp	.LBB27_103
.LBB27_12:                              # %if.then46
	movq	168(%r12), %rax
	movq	176(%r12), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	jae	.LBB27_103
# BB#13:                                # %if.then52
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r12)
	movzbl	(%rax), %r15d
.LBB27_15:                              # %if.end64
	movq	8(%r12), %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r15
	je	.LBB27_16
# BB#26:                                # %if.end82
	movq	64(%r12), %rdi
	callq	Perl_newSViv
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	xorq	%r8, %r8
	xorq	%rbx, %rbx
	movq	%r13, %rdi
	movq	%rax, %rcx
	callq	Perl_hv_store
	testq	%rax, %rax
	jne	.LBB27_67
	jmp	.LBB27_103
.LBB27_32:                              # %if.then116
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB27_33
# BB#37:                                # %if.else149
	leaq	24(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB27_103
	jmp	.LBB27_38
.LBB27_16:                              # %if.then67
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	movq	%r13, %rdi
	callq	Perl_hv_fetch
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB27_18
# BB#17:                                # %if.then71
	movq	$1, %rax
	movl	%eax, 112(%r12)
	movq	40(%rsp), %rsi
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB27_18:                              # %if.end72
	movq	(%rbx), %rdi
	movzbl	18(%rdi), %eax
	testq	$1, %rax
	je	.LBB27_20
# BB#19:                                # %cond.true
	movq	(%rdi), %rax
	movq	24(%rax), %r14
	jmp	.LBB27_21
.LBB27_33:                              # %if.then120
	movq	168(%r12), %rcx
	movq	176(%r12), %rdx
	leaq	4(%rcx), %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rax
	ja	.LBB27_103
# BB#34:                                # %if.then128
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB27_36
# BB#35:                                # %if.then136
	movslq	(%rcx), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 168(%r12)
	jmp	.LBB27_38
.LBB27_20:                              # %cond.false
	callq	Perl_sv_2iv
	movq	%rax, %r14
.LBB27_21:                              # %cond.end
	movq	24(%r12), %rdi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%r14, %rsi
	callq	Perl_av_fetch
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB27_23
# BB#22:                                # %if.then77
	movq	$1, %rax
	movl	%eax, 112(%r12)
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	Perl_croak
.LBB27_23:                              # %if.end79
	movq	(%r15), %rax
	jmp	.LBB27_24
.LBB27_36:                              # %if.else140
	movl	(%rcx), %ecx
	movl	%ecx, 24(%rsp)
	movq	%rax, 168(%r12)
.LBB27_38:                              # %if.end156
	movq	24(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	%rax, 24(%rsp)
	movq	24(%r12), %rdi
	xorq	%rdx, %rdx
	xorq	%rbx, %rbx
	movq	%rax, %rsi
	callq	Perl_av_fetch
	movq	%rax, %r14
	testq	%r14, %r14
	jne	.LBB27_40
# BB#39:                                # %if.then161
	movq	$1, %rax
	movl	%eax, 112(%r12)
	movq	24(%rsp), %rsi
	movabsq	$.L.str36, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB27_40:                              # %if.end163
	movq	(%r14), %rax
.LBB27_24:                              # %if.end79
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	je	.LBB27_103
# BB#25:                                # %land.rhs
	incq	8(%rax)
	movq	%rax, %rbx
	jmp	.LBB27_103
.LBB27_52:                              # %cond.false206
	movq	16(%rax), %r13
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, %r13            # imm = 0x10000
	je	.LBB27_56
# BB#53:                                # %cond.true211
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB27_54
# BB#55:                                # %cond.true211
	movabsq	$1, %rax
	jmp	.LBB27_61
.LBB27_56:                              # %cond.false217
	movq	16(%rax), %r13
	movabsq	$.L.str37, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, %r13           # imm = 0x20000
	je	.LBB27_60
# BB#57:                                # %cond.true222
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB27_58
# BB#59:                                # %cond.true222
	movabsq	$1, %rax
	jmp	.LBB27_61
.LBB27_54:
	movabsq	$0, %rax
	jmp	.LBB27_61
.LBB27_50:
	movabsq	$0, %rax
	jmp	.LBB27_61
.LBB27_60:                              # %cond.false227
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB27_61
.LBB27_58:
	movabsq	$0, %rax
.LBB27_61:                              # %cond.end237
	testq	%rbx, %rax
	je	.LBB27_62
# BB#63:                                # %cond.end237
	movabsq	$1, %rax
	jmp	.LBB27_64
.LBB27_62:
	movabsq	$0, %rax
.LBB27_64:                              # %cond.end237
	movl	%eax, 108(%r12)
.LBB27_65:                              # %if.end242
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB27_67
# BB#66:                                # %if.then246
	movq	$1, %rax
	movl	%eax, 112(%r12)
	movl	228(%r12), %edx
	movl	224(%r12), %esi
	movabsq	$.L.str38, %rdi
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	movq	$26, (%rsp)
	movq	%r15, %rcx
	callq	Perl_croak
.LBB27_67:                              # %first_time
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	cmpq	$27, %r15
	jle	.LBB27_69
# BB#68:                                # %first_time
	movq	$27, %r15
.LBB27_69:                              # %first_time
	movq	232(%r12), %rcx
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	*(%rcx,%r15,8)
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB27_103
# BB#70:                                # %if.end261
	movslq	224(%r12), %rax
	cmpq	$1, %rax
	jg	.LBB27_71
# BB#72:                                # %while.cond.preheader
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB27_73
	.align	16, 0x90
.LBB27_100:                             # %if.end408
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	120(%r12), %rax
	xorq	%rdx, %rdx
	movb	%dl, (%rax,%rcx)
	movq	120(%r12), %rdi
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%rax, %r13
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r13, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB27_73:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB27_74
# BB#75:                                # %cond.false271
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	168(%r12), %rax
	movq	176(%r12), %rcx
	cmpq	%rcx, %rax
	jae	.LBB27_102
# BB#76:                                # %cond.false279
                                        #   in Loop: Header=BB27_73 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r12)
	movsbq	(%rax), %rax
	jmp	.LBB27_77
	.align	16, 0x90
.LBB27_74:                              # %cond.true268
                                        #   in Loop: Header=BB27_73 Depth=1
	callq	PerlIO_getc
.LBB27_77:                              # %cond.end286
                                        #   in Loop: Header=BB27_73 Depth=1
	andq	%r15, %rax
	cmpq	$66, %rax
	je	.LBB27_84
# BB#78:                                # %cond.end286
                                        #   in Loop: Header=BB27_73 Depth=1
	cmpq	$88, %rax
	je	.LBB27_79
# BB#80:                                # %cond.end286
                                        #   in Loop: Header=BB27_73 Depth=1
	cmpq	$98, %rax
	jne	.LBB27_102
# BB#81:                                # %sw.bb
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB27_101
# BB#82:                                # %if.then292
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	168(%r12), %rax
	movq	176(%r12), %rcx
	cmpq	%rcx, %rax
	jae	.LBB27_102
# BB#83:                                # %if.then299
                                        #   in Loop: Header=BB27_73 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r12)
	movzbl	(%rax), %eax
	jmp	.LBB27_92
	.align	16, 0x90
.LBB27_84:                              # %sw.bb317
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB27_89
# BB#85:                                # %if.then320
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	168(%r12), %rcx
	movq	176(%r12), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	ja	.LBB27_102
# BB#86:                                # %if.then328
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB27_88
# BB#87:                                # %if.then336
                                        #   in Loop: Header=BB27_73 Depth=1
	movslq	(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	movq	%rax, 168(%r12)
	jmp	.LBB27_90
.LBB27_89:                              # %if.else349
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	$8, %rdx
	leaq	16(%rsp), %rsi
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	je	.LBB27_90
	jmp	.LBB27_102
.LBB27_101:                             # %if.else307
                                        #   in Loop: Header=BB27_73 Depth=1
	callq	PerlIO_getc
	movq	%rax, %rdx
	andq	%r15, %rdx
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%r15, %rdx
	movq	%rax, 16(%rsp)
	jne	.LBB27_93
	jmp	.LBB27_102
.LBB27_88:                              # %if.else340
                                        #   in Loop: Header=BB27_73 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, 16(%rsp)
	movq	%rax, 168(%r12)
.LBB27_90:                              # %if.end356
                                        #   in Loop: Header=BB27_73 Depth=1
	movl	80(%r12), %ecx
	movq	16(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB27_93
# BB#91:                                # %if.then359
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB27_92:                              # %sw.epilog
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	%rax, 16(%rsp)
.LBB27_93:                              # %sw.epilog
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	128(%r12), %rcx
	cmpq	%rcx, %rax
	jb	.LBB27_95
# BB#94:                                # %if.then367
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	120(%r12), %rdi
	incq	%rax
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 120(%r12)
	movq	16(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 128(%r12)
.LBB27_95:                              # %if.end375
                                        #   in Loop: Header=BB27_73 Depth=1
	xorq	%rcx, %rcx
	testq	%rax, %rax
	je	.LBB27_100
# BB#96:                                # %if.then377
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	jne	.LBB27_99
# BB#97:                                # %if.then380
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	168(%r12), %rsi
	movq	176(%r12), %rcx
	leaq	(%rsi,%rax), %rdx
	cmpq	%rcx, %rdx
	ja	.LBB27_102
# BB#98:                                # %if.then388
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	120(%r12), %rdi
	movq	%rax, %rdx
	callq	memcpy
	movq	16(%rsp), %rcx
	addq	%rcx, 168(%r12)
	jmp	.LBB27_100
.LBB27_99:                              # %if.else398
                                        #   in Loop: Header=BB27_73 Depth=1
	movq	120(%r12), %rsi
	movq	%rax, %rdx
	callq	Perl_PerlIO_read
	movq	16(%rsp), %rdx
	movq	%rax, %rcx
	cmpq	%rdx, %rcx
	je	.LBB27_100
.LBB27_102:                             # %return.loopexit
	xorq	%rbx, %rbx
	jmp	.LBB27_103
.LBB27_71:
	movq	%r14, %rbx
.LBB27_103:                             # %return
	movq	%rbx, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB27_79:
	movq	%r14, %rbx
	jmp	.LBB27_103
.Ltmp266:
	.size	retrieve, .Ltmp266-retrieve
	.cfi_endproc

	.align	16, 0x90
	.type	sv_type,@function
sv_type:                                # @sv_type
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp269:
	.cfi_def_cfa_offset 16
.Ltmp270:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movq	16(%rbx), %rdx
	movq	%rdx, %rcx
	andq	$255, %rcx
	cmpq	$12, %rcx
	jbe	.LBB28_1
# BB#16:                                # %sw.epilog
	movq	$7, %rax
	jmp	.LBB28_17
.LBB28_1:                               # %entry
	movq	$1, %rax
	jmpq	*.LJTI28_0(,%rcx,8)
.LBB28_2:                               # %sw.bb4
	testq	$32768, %rdx            # imm = 0x8000
	je	.LBB28_5
# BB#3:                                 # %land.lhs.true
	movq	$112, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	movq	$5, %rax
	jne	.LBB28_17
# BB#4:                                 # %land.lhs.true.sw.bb9_crit_edge
	movq	16(%rbx), %rdx
.LBB28_5:                               # %sw.bb9
	testq	$32768, %rdx            # imm = 0x8000
	je	.LBB28_8
# BB#6:                                 # %land.lhs.true13
	movq	$113, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	movq	$4, %rax
	jne	.LBB28_17
# BB#7:                                 # %land.lhs.true13.if.end17_crit_edge
	movq	16(%rbx), %rdx
.LBB28_8:                               # %sw.bb1
	movq	$19, %rcx
	shrq	%cl, %rdx
	notq	%rdx
	andq	$1, %rdx
	movq	%rdx, %rax
	popq	%rbx
	retq
.LBB28_9:                               # %sw.bb22
	testq	$32768, %rdx            # imm = 0x8000
	je	.LBB28_11
# BB#10:                                # %land.lhs.true26
	movq	$80, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	movq	$4, %rax
	jne	.LBB28_17
.LBB28_11:                              # %if.end30
	movq	$2, %rax
	popq	%rbx
	retq
.LBB28_12:                              # %sw.bb31
	testq	$32768, %rdx            # imm = 0x8000
	je	.LBB28_14
# BB#13:                                # %land.lhs.true35
	movq	$80, %rsi
	movq	%rbx, %rdi
	callq	Perl_mg_find
	testq	%rax, %rax
	movq	$4, %rax
	je	.LBB28_14
.LBB28_17:                              # %return
	popq	%rbx
	retq
.LBB28_15:                              # %sw.bb40
	movq	$6, %rax
	popq	%rbx
	retq
.LBB28_14:                              # %if.end39
	movq	$3, %rax
	popq	%rbx
	retq
.Ltmp271:
	.size	sv_type, .Ltmp271-sv_type
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI28_0:
	.quad	.LBB28_17
	.quad	.LBB28_17
	.quad	.LBB28_17
	.quad	.LBB28_8
	.quad	.LBB28_8
	.quad	.LBB28_8
	.quad	.LBB28_8
	.quad	.LBB28_2
	.quad	.LBB28_5
	.quad	.LBB28_2
	.quad	.LBB28_9
	.quad	.LBB28_12
	.quad	.LBB28_15

	.text
	.align	16, 0x90
	.type	retrieve_lscalar,@function
retrieve_lscalar:                       # @retrieve_lscalar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp278:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp279:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp280:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp281:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp282:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp283:
	.cfi_def_cfa_offset 64
.Ltmp284:
	.cfi_offset %rbx, -48
.Ltmp285:
	.cfi_offset %r12, -40
.Ltmp286:
	.cfi_offset %r13, -32
.Ltmp287:
	.cfi_offset %r14, -24
.Ltmp288:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	movq	%rsi, %r12
	testq	%rdi, %rdi
	je	.LBB29_1
# BB#5:                                 # %if.else18
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r15, %r15
	cmpq	$8, %rax
	jne	.LBB29_22
	jmp	.LBB29_6
.LBB29_1:                               # %if.then
	movq	168(%rbx), %rcx
	movq	176(%rbx), %rdx
	leaq	4(%rcx), %rax
	xorq	%r15, %r15
	cmpq	%rdx, %rax
	ja	.LBB29_22
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB29_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, 168(%rbx)
	jmp	.LBB29_6
.LBB29_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 8(%rsp)
	movq	%rax, 168(%rbx)
.LBB29_6:                               # %if.end24
	movl	80(%rbx), %eax
	movq	8(%rsp), %rdi
	testq	%rax, %rax
	je	.LBB29_8
# BB#7:                                 # %if.then26
	callq	Perl_my_ntohl
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 8(%rsp)
.LBB29_8:                               # %if.end30
	callq	Perl_newSV
	movq	%rax, %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB29_22
# BB#9:                                 # %if.end34
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB29_22
# BB#10:                                # %if.end42
	testq	%r12, %r12
	je	.LBB29_12
# BB#11:                                # %if.then44
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r12
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_bless
	movq	(%r13), %rax
	movq	$0, (%rax)
	movq	%r13, %rdi
	callq	Perl_sv_free
.LBB29_12:                              # %if.end48
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB29_13
# BB#20:                                # %if.else68
	movq	(%r14), %rax
	movq	(%rax), %rsi
	movq	8(%rsp), %rdx
	callq	Perl_PerlIO_read
	movq	8(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB29_15
	jmp	.LBB29_21
.LBB29_13:                              # %if.then51
	movq	168(%rbx), %rsi
	movq	8(%rsp), %rdx
	movq	176(%rbx), %rax
	leaq	(%rsi,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB29_14
.LBB29_21:                              # %if.else66
	movq	%r14, %rdi
	callq	Perl_sv_free
	jmp	.LBB29_22
.LBB29_14:                              # %if.then59
	movq	(%r14), %rax
	movq	(%rax), %rdi
	callq	memcpy
	movq	8(%rsp), %rax
	addq	%rax, 168(%rbx)
.LBB29_15:                              # %if.end77
	movq	(%r14), %rcx
	movq	%rax, 8(%rcx)
	movq	(%rcx), %rcx
	xorq	%rdx, %rdx
	movb	%dl, (%rcx,%rax)
	movq	16(%r14), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%r14)
	movl	84(%rbx), %eax
	testq	%rax, %rax
	je	.LBB29_19
# BB#16:                                # %if.end77
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB29_19
# BB#17:                                # %if.end77
	movzbl	PL_tainted(%rip), %eax
	testq	$255, %rax
	je	.LBB29_19
# BB#18:                                # %if.then93
	movq	%r14, %rdi
	callq	Perl_sv_taint
.LBB29_19:                              # %return
	movq	%r14, %r15
.LBB29_22:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp289:
	.size	retrieve_lscalar, .Ltmp289-retrieve_lscalar
	.cfi_endproc

	.align	16, 0x90
	.type	old_retrieve_array,@function
old_retrieve_array:                     # @old_retrieve_array
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp296:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp297:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp298:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp299:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp300:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp301:
	.cfi_def_cfa_offset 64
.Ltmp302:
	.cfi_offset %rbx, -48
.Ltmp303:
	.cfi_offset %r12, -40
.Ltmp304:
	.cfi_offset %r13, -32
.Ltmp305:
	.cfi_offset %r14, -24
.Ltmp306:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB30_1
# BB#5:                                 # %if.else18
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r13, %r13
	cmpq	$8, %rax
	jne	.LBB30_31
	jmp	.LBB30_6
.LBB30_1:                               # %if.then
	movq	168(%r15), %rcx
	movq	176(%r15), %rdx
	leaq	4(%rcx), %rax
	xorq	%r13, %r13
	cmpq	%rdx, %rax
	ja	.LBB30_31
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB30_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, 168(%r15)
	jmp	.LBB30_6
.LBB30_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 8(%rsp)
	movq	%rax, 168(%r15)
.LBB30_6:                               # %if.end24
	movl	80(%r15), %eax
	testq	%rax, %rax
	je	.LBB30_8
# BB#7:                                 # %if.then26
	movq	8(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)
.LBB30_8:                               # %if.end30
	callq	Perl_newAV
	movq	%rax, %r14
	xorq	%r13, %r13
	testq	%r14, %r14
	je	.LBB30_31
# BB#9:                                 # %if.end34
	movq	64(%r15), %rsi
	movq	24(%r15), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r15)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB30_31
# BB#10:                                # %if.end42
	movq	8(%rsp), %rsi
	testq	%rsi, %rsi
	movq	%r14, %r13
	je	.LBB30_31
# BB#11:                                # %if.then44
	movq	%r14, %rdi
	callq	Perl_av_extend
	cmpq	$0, 8(%rsp)
	movq	%r14, %r13
	jle	.LBB30_31
# BB#12:                                # %for.body.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB30_13:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	jne	.LBB30_16
# BB#14:                                # %if.then51
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	168(%r15), %rax
	movq	176(%r15), %rcx
	xorq	%r13, %r13
	cmpq	%rcx, %rax
	jae	.LBB30_31
# BB#15:                                # %if.then58
                                        #   in Loop: Header=BB30_13 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r15)
	movzbl	(%rax), %eax
	jmp	.LBB30_17
	.align	16, 0x90
.LBB30_16:                              # %if.else64
                                        #   in Loop: Header=BB30_13 Depth=1
	callq	PerlIO_getc
	movq	%rax, %rcx
	andq	%r12, %rcx
	xorq	%r13, %r13
	cmpq	%r12, %rcx
	je	.LBB30_31
.LBB30_17:                              # %if.end71
                                        #   in Loop: Header=BB30_13 Depth=1
	andq	%r12, %rax
	cmpq	$73, %rax
	je	.LBB30_30
# BB#18:                                # %if.end71
                                        #   in Loop: Header=BB30_13 Depth=1
	cmpq	$105, %rax
	je	.LBB30_28
# BB#19:                                # %if.then78
                                        #   in Loop: Header=BB30_13 Depth=1
	movl	224, %edx
	movl	228, %ecx
	cmpq	$2, %rdx
	jne	.LBB30_24
# BB#20:                                #   in Loop: Header=BB30_13 Depth=1
	movq	%rcx, %r8
	jmp	.LBB30_21
.LBB30_24:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	%rcx, %rax
	andq	%r12, %rax
	movq	$6, %r8
	cmpq	$6, %rax
	jne	.LBB30_25
.LBB30_21:                              # %if.else.i
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB30_23
# BB#22:                                # %select.mid16
                                        #   in Loop: Header=BB30_13 Depth=1
	movabsq	$.L.str34, %rsi
.LBB30_23:                              # %select.end15
                                        #   in Loop: Header=BB30_13 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str40, %rdi
	movq	%r8, %rcx
	callq	Perl_croak
	jmp	.LBB30_28
.LBB30_25:                              # %if.then.i
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB30_27
# BB#26:                                # %select.mid
                                        #   in Loop: Header=BB30_13 Depth=1
	movabsq	$.L.str34, %rsi
.LBB30_27:                              # %select.end
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	movabsq	$.L.str39, %rdi
	callq	Perl_croak
	.align	16, 0x90
.LBB30_28:                              # %if.end80
                                        #   in Loop: Header=BB30_13 Depth=1
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%r15, %rdi
	callq	retrieve
	testq	%rax, %rax
	je	.LBB30_31
# BB#29:                                # %if.end84
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB30_31
.LBB30_30:                              # %for.inc
                                        #   in Loop: Header=BB30_13 Depth=1
	movq	8(%rsp), %rax
	incq	%rbx
	cmpq	%rax, %rbx
	movq	%r14, %r13
	jl	.LBB30_13
.LBB30_31:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp307:
	.size	old_retrieve_array, .Ltmp307-old_retrieve_array
	.cfi_endproc

	.align	16, 0x90
	.type	old_retrieve_hash,@function
old_retrieve_hash:                      # @old_retrieve_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp314:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp315:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp316:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp317:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp318:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp319:
	.cfi_def_cfa_offset 80
.Ltmp320:
	.cfi_offset %rbx, -48
.Ltmp321:
	.cfi_offset %r12, -40
.Ltmp322:
	.cfi_offset %r13, -32
.Ltmp323:
	.cfi_offset %r14, -24
.Ltmp324:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB31_1
# BB#5:                                 # %if.else18
	leaq	24(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r15, %r15
	cmpq	$8, %rax
	jne	.LBB31_64
	jmp	.LBB31_6
.LBB31_1:                               # %if.then
	movq	168(%rbx), %rcx
	movq	176(%rbx), %rdx
	leaq	4(%rcx), %rax
	xorq	%r15, %r15
	cmpq	%rdx, %rax
	ja	.LBB31_64
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB31_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, 24(%rsp)
	movq	%rax, 168(%rbx)
	jmp	.LBB31_6
.LBB31_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 24(%rsp)
	movq	%rax, 168(%rbx)
.LBB31_6:                               # %if.end24
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB31_8
# BB#7:                                 # %if.then26
	movq	24(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 24(%rsp)
.LBB31_8:                               # %if.end30
	callq	Perl_newHV
	movq	%rax, %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB31_64
# BB#9:                                 # %if.end34
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB31_64
# BB#10:                                # %if.end42
	movq	24(%rsp), %rsi
	testq	%rsi, %rsi
	movq	%r14, %r15
	je	.LBB31_64
# BB#11:                                # %if.end46
	movq	%r14, %rdi
	callq	Perl_hv_ksplit
	cmpq	$0, 24(%rsp)
	movq	%r14, %r15
	jle	.LBB31_64
# BB#12:                                # %for.body.lr.ph
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB31_14:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB31_17
# BB#15:                                # %if.then51
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	168(%rbx), %rax
	movq	176(%rbx), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jae	.LBB31_64
# BB#16:                                # %if.then58
                                        #   in Loop: Header=BB31_14 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%rbx)
	movzbl	(%rax), %eax
	jmp	.LBB31_18
	.align	16, 0x90
.LBB31_17:                              # %if.else64
                                        #   in Loop: Header=BB31_14 Depth=1
	callq	PerlIO_getc
	movq	%rax, %rcx
	andq	%r13, %rcx
	xorq	%r15, %r15
	cmpq	%r13, %rcx
	je	.LBB31_64
.LBB31_18:                              # %if.end71
                                        #   in Loop: Header=BB31_14 Depth=1
	andq	%r13, %rax
	cmpq	$118, %rax
	jne	.LBB31_19
# BB#24:                                # %if.then89
                                        #   in Loop: Header=BB31_14 Depth=1
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%rbx, %rdi
	callq	retrieve
	movq	%rax, 8(%rsp)           # 8-byte Spill
	testq	%rax, %rax
	jne	.LBB31_34
	jmp	.LBB31_64
	.align	16, 0x90
.LBB31_19:                              # %if.end71
                                        #   in Loop: Header=BB31_14 Depth=1
	cmpq	$86, %rax
	jne	.LBB31_25
# BB#20:                                # %if.then74
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	old_retrieve_hash.sv_h_undef(%rip), %rax
	testq	%rax, %rax
	jne	.LBB31_21
# BB#22:                                # %if.end78
                                        #   in Loop: Header=BB31_14 Depth=1
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_newSVsv
	movq	$0, 8(%rsp)             # 8-byte Folded Spill
	testq	%rax, %rax
	movq	%rax, old_retrieve_hash.sv_h_undef(%rip)
	movq	%rax, PL_Sv(%rip)
	je	.LBB31_34
	jmp	.LBB31_23
.LBB31_25:                              # %if.else94
                                        #   in Loop: Header=BB31_14 Depth=1
	movl	224, %edx
	movl	228, %ecx
	cmpq	$2, %rdx
	jne	.LBB31_30
# BB#26:                                #   in Loop: Header=BB31_14 Depth=1
	movq	%rcx, %r8
	jmp	.LBB31_27
.LBB31_21:                              # %if.end78.thread
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	%rax, PL_Sv(%rip)
.LBB31_23:                              # %land.rhs80
                                        #   in Loop: Header=BB31_14 Depth=1
	incq	8(%rax)
	movq	%rax, 8(%rsp)           # 8-byte Spill
	jmp	.LBB31_34
.LBB31_30:                              # %land.lhs.true.i
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	$6, %r8
	cmpq	$6, %rax
	jne	.LBB31_31
.LBB31_27:                              # %if.else.i
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB31_29
# BB#28:                                # %select.mid16
                                        #   in Loop: Header=BB31_14 Depth=1
	movabsq	$.L.str34, %rsi
.LBB31_29:                              # %select.end15
                                        #   in Loop: Header=BB31_14 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str40, %rdi
	movq	%r8, %rcx
	callq	Perl_croak
	jmp	.LBB31_34
.LBB31_31:                              # %if.then.i
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB31_33
# BB#32:                                # %select.mid
                                        #   in Loop: Header=BB31_14 Depth=1
	movabsq	$.L.str34, %rsi
.LBB31_33:                              # %select.end
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	movabsq	$.L.str39, %rdi
	callq	Perl_croak
	.align	16, 0x90
.LBB31_34:                              # %if.end97
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB31_37
# BB#35:                                # %if.then100
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	168(%rbx), %rax
	movq	176(%rbx), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jae	.LBB31_64
# BB#36:                                # %if.then107
                                        #   in Loop: Header=BB31_14 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%rbx)
	movzbl	(%rax), %eax
	jmp	.LBB31_38
	.align	16, 0x90
.LBB31_37:                              # %if.else114
                                        #   in Loop: Header=BB31_14 Depth=1
	callq	PerlIO_getc
	movq	%rax, %rcx
	andq	%r13, %rcx
	xorq	%r15, %r15
	cmpq	%r13, %rcx
	je	.LBB31_64
.LBB31_38:                              # %if.end121
                                        #   in Loop: Header=BB31_14 Depth=1
	andq	%r13, %rax
	cmpq	$107, %rax
	je	.LBB31_48
# BB#39:                                # %if.then124
                                        #   in Loop: Header=BB31_14 Depth=1
	movl	224, %edx
	movl	228, %ecx
	cmpq	$2, %rdx
	jne	.LBB31_44
# BB#40:                                #   in Loop: Header=BB31_14 Depth=1
	movq	%rcx, %r8
	jmp	.LBB31_41
.LBB31_44:                              # %land.lhs.true.i280
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	%rcx, %rax
	andq	%r13, %rax
	movq	$6, %r8
	cmpq	$6, %rax
	jne	.LBB31_45
.LBB31_41:                              # %if.else.i286
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB31_43
# BB#42:                                # %select.mid32
                                        #   in Loop: Header=BB31_14 Depth=1
	movabsq	$.L.str34, %rsi
.LBB31_43:                              # %select.end31
                                        #   in Loop: Header=BB31_14 Depth=1
	xorq	%rax, %rax
	movabsq	$.L.str40, %rdi
	movq	%r8, %rcx
	callq	Perl_croak
	jmp	.LBB31_48
.LBB31_45:                              # %if.then.i283
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$1, %rax
	movl	%eax, 112
	cmpq	$0, 216
	movabsq	$.L.str33, %rsi
	jne	.LBB31_47
# BB#46:                                # %select.mid30
                                        #   in Loop: Header=BB31_14 Depth=1
	movabsq	$.L.str34, %rsi
.LBB31_47:                              # %select.end29
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	movabsq	$.L.str39, %rdi
	callq	Perl_croak
	.align	16, 0x90
.LBB31_48:                              # %if.end126
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB31_53
# BB#49:                                # %if.then129
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	168(%rbx), %rcx
	movq	176(%rbx), %rdx
	leaq	4(%rcx), %rax
	xorq	%r15, %r15
	cmpq	%rdx, %rax
	ja	.LBB31_64
# BB#50:                                # %if.then137
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB31_52
# BB#51:                                # %if.then145
                                        #   in Loop: Header=BB31_14 Depth=1
	movslq	(%rcx), %rcx
	movq	%rcx, 16(%rsp)
	movq	%rax, 168(%rbx)
	jmp	.LBB31_54
	.align	16, 0x90
.LBB31_53:                              # %if.else158
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	$8, %rdx
	leaq	16(%rsp), %rsi
	callq	Perl_PerlIO_read
	xorq	%r15, %r15
	cmpq	$8, %rax
	je	.LBB31_54
	jmp	.LBB31_64
.LBB31_52:                              # %if.else149
                                        #   in Loop: Header=BB31_14 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, 16(%rsp)
	movq	%rax, 168(%rbx)
.LBB31_54:                              # %if.end165
                                        #   in Loop: Header=BB31_14 Depth=1
	movl	80(%rbx), %ecx
	movq	16(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB31_56
# BB#55:                                # %if.then168
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 16(%rsp)
.LBB31_56:                              # %if.end172
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	128(%rbx), %rcx
	cmpq	%rcx, %rax
	jb	.LBB31_58
# BB#57:                                # %if.then175
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	120(%rbx), %rdi
	incq	%rax
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 120(%rbx)
	movq	16(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 128(%rbx)
.LBB31_58:                              # %if.end183
                                        #   in Loop: Header=BB31_14 Depth=1
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB31_63
# BB#59:                                # %if.then185
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB31_62
# BB#60:                                # %if.then188
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	168(%rbx), %rsi
	movq	176(%rbx), %rcx
	leaq	(%rsi,%rax), %rdx
	xorq	%r15, %r15
	cmpq	%rcx, %rdx
	ja	.LBB31_64
# BB#61:                                # %if.then196
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	120(%rbx), %rdi
	movq	%rax, %rdx
	callq	memcpy
	xorq	%rdx, %rdx
	movq	16(%rsp), %rcx
	addq	%rcx, 168(%rbx)
	jmp	.LBB31_63
.LBB31_62:                              # %if.else206
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	120(%rbx), %rsi
	movq	%rax, %rdx
	callq	Perl_PerlIO_read
	movq	16(%rsp), %rdx
	movq	%rax, %rcx
	xorq	%r15, %r15
	cmpq	%rdx, %rcx
	movabsq	$0, %rdx
	jne	.LBB31_64
.LBB31_63:                              # %if.end216
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	120(%rbx), %rax
	movb	%dl, (%rax,%rcx)
	movq	120(%rbx), %rsi
	movq	16(%rsp), %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	8(%rsp), %rcx           # 8-byte Reload
	callq	Perl_hv_store
	testq	%rax, %rax
	movabsq	$0, %r15
	je	.LBB31_64
# BB#13:                                # %for.cond
                                        #   in Loop: Header=BB31_14 Depth=1
	movq	24(%rsp), %rax
	incq	%r12
	cmpq	%rax, %r12
	movq	%r14, %r15
	jl	.LBB31_14
.LBB31_64:                              # %return
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp325:
	.size	old_retrieve_hash, .Ltmp325-old_retrieve_hash
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_ref,@function
retrieve_ref:                           # @retrieve_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp332:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp333:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp334:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp335:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp336:
	.cfi_def_cfa_offset 48
.Ltmp337:
	.cfi_offset %rbx, -48
.Ltmp338:
	.cfi_offset %r12, -40
.Ltmp339:
	.cfi_offset %r13, -32
.Ltmp340:
	.cfi_offset %r14, -24
.Ltmp341:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %r15
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	callq	Perl_newSV
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB32_8
# BB#1:                                 # %if.end
	movq	64(%r15), %rsi
	movq	24(%r15), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r15)
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB32_8
# BB#2:                                 # %if.end6
	testq	%r14, %r14
	je	.LBB32_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r12
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r13
	movq	%r13, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_bless
	movq	(%r13), %rax
	movq	$0, (%rax)
	movq	%r13, %rdi
	callq	Perl_sv_free
.LBB32_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	retrieve
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB32_8
# BB#5:                                 # %if.end16
	testq	%r14, %r14
	jne	.LBB32_7
# BB#6:                                 # %if.else
	movq	$3, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
.LBB32_7:                               # %if.end20
	movq	(%rbx), %rax
	movq	%r15, (%rax)
	orq	$524288, 16(%rbx)       # imm = 0x80000
	movq	%rbx, %r12
.LBB32_8:                               # %return
	movq	%r12, %rax
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp342:
	.size	retrieve_ref, .Ltmp342-retrieve_ref
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_undef,@function
retrieve_undef:                         # @retrieve_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp348:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp349:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp350:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp351:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp352:
	.cfi_def_cfa_offset 48
.Ltmp353:
	.cfi_offset %rbx, -40
.Ltmp354:
	.cfi_offset %r12, -32
.Ltmp355:
	.cfi_offset %r14, -24
.Ltmp356:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%rdi, %rbx
	xorq	%rdi, %rdi
	xorq	%r15, %r15
	callq	Perl_newSV
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB33_5
# BB#1:                                 # %if.end
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r12, PL_Sv(%rip)
	incq	8(%r12)
	movq	%r12, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB33_5
# BB#2:                                 # %if.end6
	testq	%r14, %r14
	je	.LBB33_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB33_4:                               # %return
	movq	%r12, %r15
.LBB33_5:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp357:
	.size	retrieve_undef, .Ltmp357-retrieve_undef
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_integer,@function
retrieve_integer:                       # @retrieve_integer
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp363:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp364:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp365:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp366:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp367:
	.cfi_def_cfa_offset 48
.Ltmp368:
	.cfi_offset %rbx, -40
.Ltmp369:
	.cfi_offset %r12, -32
.Ltmp370:
	.cfi_offset %r14, -24
.Ltmp371:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB34_1
# BB#3:                                 # %if.else8
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r12, %r12
	cmpq	$8, %rax
	jne	.LBB34_10
# BB#4:                                 # %if.else8.if.end13_crit_edge
	movq	(%rsp), %rdi
	jmp	.LBB34_5
.LBB34_1:                               # %if.then
	movq	168(%rbx), %rax
	movq	176(%rbx), %rdx
	leaq	8(%rax), %rcx
	xorq	%r12, %r12
	cmpq	%rdx, %rcx
	ja	.LBB34_10
# BB#2:                                 # %if.then2
	movq	(%rax), %rdi
	movq	%rdi, (%rsp)
	movq	%rcx, 168(%rbx)
.LBB34_5:                               # %if.end13
	callq	Perl_newSViv
	movq	%rax, %r15
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB34_10
# BB#6:                                 # %if.end17
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB34_10
# BB#7:                                 # %if.end24
	testq	%r14, %r14
	je	.LBB34_9
# BB#8:                                 # %if.then26
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB34_9:                               # %return
	movq	%r15, %r12
.LBB34_10:                              # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp372:
	.size	retrieve_integer, .Ltmp372-retrieve_integer
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_double,@function
retrieve_double:                        # @retrieve_double
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp378:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp379:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp380:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp381:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp382:
	.cfi_def_cfa_offset 48
.Ltmp383:
	.cfi_offset %rbx, -40
.Ltmp384:
	.cfi_offset %r12, -32
.Ltmp385:
	.cfi_offset %r14, -24
.Ltmp386:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB35_1
# BB#3:                                 # %if.else8
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r12, %r12
	cmpq	$8, %rax
	jne	.LBB35_10
# BB#4:                                 # %if.else8.if.end13_crit_edge
	movq	(%rsp), %rdi
	jmp	.LBB35_5
.LBB35_1:                               # %if.then
	movq	168(%rbx), %rax
	movq	176(%rbx), %rdx
	leaq	8(%rax), %rcx
	xorq	%r12, %r12
	cmpq	%rdx, %rcx
	ja	.LBB35_10
# BB#2:                                 # %if.then2
	movq	(%rax), %rdi
	movq	%rdi, (%rsp)
	movq	%rcx, 168(%rbx)
.LBB35_5:                               # %if.end13
	callq	Perl_newSVnv
	movq	%rax, %r15
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB35_10
# BB#6:                                 # %if.end17
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB35_10
# BB#7:                                 # %if.end24
	testq	%r14, %r14
	je	.LBB35_9
# BB#8:                                 # %if.then26
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB35_9:                               # %return
	movq	%r15, %r12
.LBB35_10:                              # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp387:
	.size	retrieve_double, .Ltmp387-retrieve_double
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_byte,@function
retrieve_byte:                          # @retrieve_byte
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp393:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp394:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp395:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp396:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp397:
	.cfi_def_cfa_offset 48
.Ltmp398:
	.cfi_offset %rbx, -40
.Ltmp399:
	.cfi_offset %r12, -32
.Ltmp400:
	.cfi_offset %r14, -24
.Ltmp401:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB36_1
# BB#3:                                 # %if.else5
	callq	PerlIO_getc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	xorq	%r12, %r12
	cmpq	%rcx, %rdx
	jne	.LBB36_4
	jmp	.LBB36_9
.LBB36_1:                               # %if.then
	movq	168(%rbx), %rax
	movq	176(%rbx), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	jae	.LBB36_9
# BB#2:                                 # %if.then2
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%rbx)
	movzbl	(%rax), %eax
.LBB36_4:                               # %if.end11
	addq	$128, %rax
	movq	$56, %rcx
	shlq	%cl, %rax
	movq	$56, %rcx
	sarq	%cl, %rax
	movq	%rax, %rdi
	callq	Perl_newSViv
	movq	%rax, %r15
	xorq	%r12, %r12
	testq	%r15, %r15
	je	.LBB36_9
# BB#5:                                 # %if.end19
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%rbx)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB36_9
# BB#6:                                 # %if.end27
	testq	%r14, %r14
	je	.LBB36_8
# BB#7:                                 # %if.then29
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB36_8:                               # %return
	movq	%r15, %r12
.LBB36_9:                               # %return
	movq	%r12, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp402:
	.size	retrieve_byte, .Ltmp402-retrieve_byte
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_netint,@function
retrieve_netint:                        # @retrieve_netint
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp408:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp409:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp410:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp411:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp412:
	.cfi_def_cfa_offset 48
.Ltmp413:
	.cfi_offset %rbx, -40
.Ltmp414:
	.cfi_offset %r12, -32
.Ltmp415:
	.cfi_offset %r14, -24
.Ltmp416:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	216(%r12), %rdi
	movq	%rsi, %r14
	testq	%rdi, %rdi
	je	.LBB37_1
# BB#5:                                 # %if.else18
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB37_11
	jmp	.LBB37_6
.LBB37_1:                               # %if.then
	movq	168(%r12), %rcx
	movq	176(%r12), %rdx
	leaq	4(%rcx), %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rax
	ja	.LBB37_11
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB37_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, (%rsp)
	movq	%rax, 168(%r12)
	jmp	.LBB37_6
.LBB37_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, (%rsp)
	movq	%rax, 168(%r12)
.LBB37_6:                               # %if.end24
	movq	(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, %rdi
	callq	Perl_newSViv
	movq	%rax, %r15
	xorq	%rbx, %rbx
	testq	%r15, %r15
	je	.LBB37_11
# BB#7:                                 # %if.end31
	movq	64(%r12), %rsi
	movq	24(%r12), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r12)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB37_11
# BB#8:                                 # %if.end39
	testq	%r14, %r14
	je	.LBB37_10
# BB#9:                                 # %if.then41
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB37_10:                              # %return
	movq	%r15, %rbx
.LBB37_11:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp417:
	.size	retrieve_netint, .Ltmp417-retrieve_netint
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_scalar,@function
retrieve_scalar:                        # @retrieve_scalar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp424:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp425:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp426:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp427:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp428:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp429:
	.cfi_def_cfa_offset 64
.Ltmp430:
	.cfi_offset %rbx, -48
.Ltmp431:
	.cfi_offset %r12, -40
.Ltmp432:
	.cfi_offset %r13, -32
.Ltmp433:
	.cfi_offset %r14, -24
.Ltmp434:
	.cfi_offset %r15, -16
	movq	%rdi, %r12
	movq	216(%r12), %rdi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB38_1
# BB#3:                                 # %if.else5
	callq	PerlIO_getc
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r13, %rcx
	andq	%rax, %rcx
	xorq	%r15, %r15
	cmpq	%rax, %rcx
	jne	.LBB38_4
	jmp	.LBB38_25
.LBB38_1:                               # %if.then
	movq	168(%r12), %rax
	movq	176(%r12), %rcx
	xorq	%r15, %r15
	cmpq	%rcx, %rax
	jae	.LBB38_25
# BB#2:                                 # %if.then2
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r12)
	movzbl	(%rax), %r13d
.LBB38_4:                               # %if.end11
	movq	$32, %rcx
	movq	%r13, %rbx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%rbx, %rdi
	callq	Perl_newSV
	movq	%rax, %r14
	xorq	%r15, %r15
	testq	%r14, %r14
	je	.LBB38_25
# BB#5:                                 # %if.end16
	movq	64(%r12), %rsi
	movq	24(%r12), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r12)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB38_25
# BB#6:                                 # %if.end24
	movq	8(%rsp), %rdi           # 8-byte Reload
	testq	%rdi, %rdi
	je	.LBB38_8
# BB#7:                                 # %if.then26
	movq	$1, %rsi
	callq	Perl_gv_stashpv
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, (%rsp)            # 8-byte Spill
	movq	%rax, %rdi
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	Perl_sv_bless
	movq	(%rsp), %rdi            # 8-byte Reload
	movq	(%rdi), %rax
	movq	$0, (%rax)
	callq	Perl_sv_free
.LBB38_8:                               # %if.end30
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	je	.LBB38_9
# BB#14:                                # %if.else47
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB38_15
# BB#23:                                # %if.else70
	movq	(%r14), %rax
	movq	(%rax), %rsi
	movq	%rbx, %rdx
	callq	Perl_PerlIO_read
	cmpq	%rbx, %rax
	je	.LBB38_17
	jmp	.LBB38_24
.LBB38_9:                               # %if.then33
	movzbl	16(%r14), %eax
	cmpq	$4, %rax
	ja	.LBB38_11
# BB#10:                                # %if.then36
	movq	$4, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_upgrade
.LBB38_11:                              # %if.end38
	movq	(%r14), %rax
	cmpq	$0, 16(%rax)
	jne	.LBB38_13
# BB#12:                                # %cond.true
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_grow
	movq	(%r14), %rax
.LBB38_13:                              # %cond.end
	movq	(%rax), %rcx
	movq	8(%rax), %rdx
	leaq	16(%r14), %rax
	xorq	%rsi, %rsi
	movb	%sil, (%rcx,%rdx)
	jmp	.LBB38_18
.LBB38_15:                              # %if.then50
	movq	168(%r12), %rsi
	movq	176(%r12), %rax
	leaq	(%rsi,%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB38_16
.LBB38_24:                              # %if.else68
	movq	%r14, %rdi
	callq	Perl_sv_free
	jmp	.LBB38_25
.LBB38_16:                              # %if.then58
	movq	(%r14), %rax
	movq	(%rax), %rdi
	movq	%rbx, %rdx
	callq	memcpy
	addq	%rbx, 168(%r12)
.LBB38_17:                              # %if.end81
	movq	(%r14), %rax
	movq	%rbx, 8(%rax)
	movq	(%rax), %rax
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%rbx)
	leaq	16(%r14), %rax
.LBB38_18:                              # %if.end90
	movq	(%rax), %rcx
	andq	$1223753727, %rcx       # imm = 0x48F0FFFF
	orq	$67371008, %rcx         # imm = 0x4040000
	movq	%rcx, (%rax)
	movl	84(%r12), %eax
	testq	%rax, %rax
	je	.LBB38_22
# BB#19:                                # %if.end90
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB38_22
# BB#20:                                # %if.end90
	movzbl	PL_tainted(%rip), %eax
	testq	$255, %rax
	je	.LBB38_22
# BB#21:                                # %if.then101
	movq	%r14, %rdi
	callq	Perl_sv_taint
.LBB38_22:                              # %return
	movq	%r14, %r15
.LBB38_25:                              # %return
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp435:
	.size	retrieve_scalar, .Ltmp435-retrieve_scalar
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_tied_array,@function
retrieve_tied_array:                    # @retrieve_tied_array
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp441:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp442:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp443:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp444:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp445:
	.cfi_def_cfa_offset 48
.Ltmp446:
	.cfi_offset %rbx, -40
.Ltmp447:
	.cfi_offset %r12, -32
.Ltmp448:
	.cfi_offset %r14, -24
.Ltmp449:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	xorq	%r15, %r15
	callq	Perl_newSV
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB39_6
# BB#1:                                 # %if.end
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB39_6
# BB#2:                                 # %if.end6
	testq	%r12, %r12
	je	.LBB39_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%r12), %rax
	movq	$0, (%rax)
	movq	%r12, %rdi
	callq	Perl_sv_free
.LBB39_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB39_6
# BB#5:                                 # %if.end16
	movq	$10, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	(%rbx), %rax
	movzbl	72(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 72(%rax)
	movq	$80, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_magic
	movq	%r14, %rdi
	callq	Perl_sv_free
	movq	%rbx, %r15
.LBB39_6:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp450:
	.size	retrieve_tied_array, .Ltmp450-retrieve_tied_array
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_tied_hash,@function
retrieve_tied_hash:                     # @retrieve_tied_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp456:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp457:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp458:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp459:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp460:
	.cfi_def_cfa_offset 48
.Ltmp461:
	.cfi_offset %rbx, -40
.Ltmp462:
	.cfi_offset %r12, -32
.Ltmp463:
	.cfi_offset %r14, -24
.Ltmp464:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	xorq	%r15, %r15
	callq	Perl_newSV
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB40_6
# BB#1:                                 # %if.end
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB40_6
# BB#2:                                 # %if.end6
	testq	%r12, %r12
	je	.LBB40_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%r12), %rax
	movq	$0, (%rax)
	movq	%r12, %rdi
	callq	Perl_sv_free
.LBB40_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%r15, %r15
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB40_6
# BB#5:                                 # %if.end16
	movq	$11, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_upgrade
	movq	$80, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_magic
	movq	%r14, %rdi
	callq	Perl_sv_free
	movq	%rbx, %r15
.LBB40_6:                               # %return
	movq	%r15, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp465:
	.size	retrieve_tied_hash, .Ltmp465-retrieve_tied_hash
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_tied_scalar,@function
retrieve_tied_scalar:                   # @retrieve_tied_scalar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp471:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp472:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp473:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp474:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp475:
	.cfi_def_cfa_offset 48
.Ltmp476:
	.cfi_offset %rbx, -40
.Ltmp477:
	.cfi_offset %r12, -32
.Ltmp478:
	.cfi_offset %r14, -24
.Ltmp479:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	callq	Perl_newSV
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB41_10
# BB#1:                                 # %if.end
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB41_10
# BB#2:                                 # %if.end6
	testq	%r12, %r12
	je	.LBB41_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB41_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	retrieve
	testq	%rax, %rax
	je	.LBB41_10
# BB#5:                                 # %if.else
	movzbl	16(%rax), %ebx
	testq	%rbx, %rbx
	je	.LBB41_7
# BB#6:                                 # %if.else
	movq	%rax, %rbx
.LBB41_7:                               # %if.else
	movq	$7, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
	movq	$113, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%rbx, %rsi
	callq	Perl_sv_magic
	testq	%rbx, %rbx
	je	.LBB41_9
# BB#8:                                 # %if.then22
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB41_9:                               # %return
	movq	%r15, %rbx
.LBB41_10:                              # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp480:
	.size	retrieve_tied_scalar, .Ltmp480-retrieve_tied_scalar
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_other,@function
retrieve_other:                         # @retrieve_other
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp482:
	.cfi_def_cfa_offset 16
	movl	224(%rdi), %edx
	movl	228(%rdi), %ecx
	cmpq	$2, %rdx
	jne	.LBB42_4
# BB#1:
	movq	%rcx, %r8
	jmp	.LBB42_2
.LBB42_4:                               # %land.lhs.true
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	$6, %r8
	cmpq	$6, %rax
	jne	.LBB42_5
.LBB42_2:                               # %if.else
	movq	$1, %rax
	movl	%eax, 112(%rdi)
	cmpq	$0, 216(%rdi)
	je	.LBB42_9
# BB#3:
	movabsq	$.L.str33, %rsi
	jmp	.LBB42_10
.LBB42_9:                               # %select.mid2
	movabsq	$.L.str34, %rsi
.LBB42_10:                              # %select.end1
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	movq	%r8, %rcx
	callq	Perl_croak
	jmp	.LBB42_11
.LBB42_5:                               # %if.then
	movq	$1, %rax
	movl	%eax, 112(%rdi)
	cmpq	$0, 216(%rdi)
	je	.LBB42_7
# BB#6:
	movabsq	$.L.str33, %rsi
	jmp	.LBB42_8
.LBB42_7:                               # %select.mid
	movabsq	$.L.str34, %rsi
.LBB42_8:                               # %select.end
	movabsq	$.L.str39, %rdi
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	callq	Perl_croak
.LBB42_11:                              # %if.end
	xorq	%rax, %rax
	popq	%rdx
	retq
.Ltmp483:
	.size	retrieve_other, .Ltmp483-retrieve_other
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_array,@function
retrieve_array:                         # @retrieve_array
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp490:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp491:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp492:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp493:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp494:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp495:
	.cfi_def_cfa_offset 64
.Ltmp496:
	.cfi_offset %rbx, -48
.Ltmp497:
	.cfi_offset %r12, -40
.Ltmp498:
	.cfi_offset %r13, -32
.Ltmp499:
	.cfi_offset %r14, -24
.Ltmp500:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	216(%r14), %rdi
	movq	%rsi, %r15
	testq	%rdi, %rdi
	je	.LBB43_1
# BB#5:                                 # %if.else18
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r13, %r13
	cmpq	$8, %rax
	jne	.LBB43_18
	jmp	.LBB43_6
.LBB43_1:                               # %if.then
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	xorq	%r13, %r13
	cmpq	%rdx, %rax
	ja	.LBB43_18
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB43_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, 168(%r14)
	jmp	.LBB43_6
.LBB43_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 8(%rsp)
	movq	%rax, 168(%r14)
.LBB43_6:                               # %if.end24
	movl	80(%r14), %eax
	testq	%rax, %rax
	je	.LBB43_8
# BB#7:                                 # %if.then26
	movq	8(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)
.LBB43_8:                               # %if.end30
	callq	Perl_newAV
	movq	%rax, %r12
	xorq	%r13, %r13
	testq	%r12, %r12
	je	.LBB43_18
# BB#9:                                 # %if.end34
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%r12, PL_Sv(%rip)
	incq	8(%r12)
	movq	%r12, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB43_18
# BB#10:                                # %if.end42
	testq	%r15, %r15
	je	.LBB43_12
# BB#11:                                # %if.then44
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB43_12:                              # %if.end48
	movq	8(%rsp), %rsi
	testq	%rsi, %rsi
	movq	%r12, %r13
	je	.LBB43_18
# BB#13:                                # %if.then50
	movq	%r12, %rdi
	callq	Perl_av_extend
	cmpq	$0, 8(%rsp)
	movq	%r12, %r13
	jle	.LBB43_18
# BB#14:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB43_16:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	xorq	%r13, %r13
	movq	%r14, %rdi
	callq	retrieve
	testq	%rax, %rax
	je	.LBB43_18
# BB#17:                                # %if.end58
                                        #   in Loop: Header=BB43_16 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB43_18
# BB#15:                                # %for.cond
                                        #   in Loop: Header=BB43_16 Depth=1
	movq	8(%rsp), %rax
	incq	%rbx
	cmpq	%rax, %rbx
	movq	%r12, %r13
	jl	.LBB43_16
.LBB43_18:                              # %return
	movq	%r13, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp501:
	.size	retrieve_array, .Ltmp501-retrieve_array
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_hash,@function
retrieve_hash:                          # @retrieve_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp508:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp509:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp510:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp511:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp512:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp513:
	.cfi_def_cfa_offset 64
.Ltmp514:
	.cfi_offset %rbx, -48
.Ltmp515:
	.cfi_offset %r12, -40
.Ltmp516:
	.cfi_offset %r13, -32
.Ltmp517:
	.cfi_offset %r14, -24
.Ltmp518:
	.cfi_offset %r15, -16
	movq	%rdi, %r13
	movq	216(%r13), %rdi
	movq	%rsi, %r15
	testq	%rdi, %rdi
	je	.LBB44_1
# BB#5:                                 # %if.else18
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB44_37
	jmp	.LBB44_6
.LBB44_1:                               # %if.then
	movq	168(%r13), %rcx
	movq	176(%r13), %rdx
	leaq	4(%rcx), %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rax
	ja	.LBB44_37
# BB#2:                                 # %if.then2
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB44_4
# BB#3:                                 # %if.then8
	movslq	(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, 168(%r13)
	jmp	.LBB44_6
.LBB44_4:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 8(%rsp)
	movq	%rax, 168(%r13)
.LBB44_6:                               # %if.end24
	movl	80(%r13), %eax
	testq	%rax, %rax
	je	.LBB44_8
# BB#7:                                 # %if.then26
	movq	8(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)
.LBB44_8:                               # %if.end30
	callq	Perl_newHV
	movq	%rax, %r14
	xorq	%rbx, %rbx
	testq	%r14, %r14
	je	.LBB44_37
# BB#9:                                 # %if.end34
	movq	64(%r13), %rsi
	movq	24(%r13), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r13)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB44_37
# BB#10:                                # %if.end42
	testq	%r15, %r15
	je	.LBB44_12
# BB#11:                                # %if.then44
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB44_12:                              # %if.end48
	movq	8(%rsp), %rsi
	testq	%rsi, %rsi
	movq	%r14, %rbx
	je	.LBB44_37
# BB#13:                                # %if.end52
	movq	%r14, %rdi
	callq	Perl_hv_ksplit
	cmpq	$0, 8(%rsp)
	movq	%r14, %rbx
	jle	.LBB44_37
# BB#14:                                # %for.body.lr.ph
	xorq	%r15, %r15
	.align	16, 0x90
.LBB44_16:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%r13, %rdi
	callq	retrieve
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB44_17
# BB#18:                                # %if.end58
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	jne	.LBB44_23
# BB#19:                                # %if.then61
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	168(%r13), %rcx
	movq	176(%r13), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	ja	.LBB44_20
# BB#21:                                # %if.then69
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB44_25
# BB#22:                                # %if.then77
                                        #   in Loop: Header=BB44_16 Depth=1
	movslq	(%rcx), %rcx
	movq	%rcx, (%rsp)
	jmp	.LBB44_26
	.align	16, 0x90
.LBB44_23:                              # %if.else90
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	$8, %rdx
	leaq	(%rsp), %rsi
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	je	.LBB44_27
	jmp	.LBB44_24
.LBB44_25:                              # %if.else81
                                        #   in Loop: Header=BB44_16 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, (%rsp)
.LBB44_26:                              # %if.end97
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	%rax, 168(%r13)
.LBB44_27:                              # %if.end97
                                        #   in Loop: Header=BB44_16 Depth=1
	movl	80(%r13), %ecx
	movq	(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB44_29
# BB#28:                                # %if.then100
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsp)
.LBB44_29:                              # %if.end104
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	128(%r13), %rcx
	cmpq	%rcx, %rax
	jb	.LBB44_31
# BB#30:                                # %if.then107
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	120(%r13), %rdi
	incq	%rax
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 120(%r13)
	movq	(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 128(%r13)
.LBB44_31:                              # %if.end115
                                        #   in Loop: Header=BB44_16 Depth=1
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB44_36
# BB#32:                                # %if.then117
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	jne	.LBB44_35
# BB#33:                                # %if.then120
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	168(%r13), %rsi
	movq	176(%r13), %rcx
	leaq	(%rsi,%rax), %rdx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rdx
	ja	.LBB44_37
# BB#34:                                # %if.then128
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	120(%r13), %rdi
	movq	%rax, %rdx
	callq	memcpy
	xorq	%rdx, %rdx
	movq	(%rsp), %rcx
	addq	%rcx, 168(%r13)
	jmp	.LBB44_36
.LBB44_35:                              # %if.else138
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	120(%r13), %rsi
	movq	%rax, %rdx
	callq	Perl_PerlIO_read
	movq	(%rsp), %rdx
	movq	%rax, %rcx
	xorq	%rbx, %rbx
	cmpq	%rdx, %rcx
	movabsq	$0, %rdx
	jne	.LBB44_37
.LBB44_36:                              # %if.end148
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	120(%r13), %rax
	movb	%dl, (%rax,%rcx)
	movq	120(%r13), %rsi
	movq	(%rsp), %rdx
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%r12, %rcx
	callq	Perl_hv_store
	testq	%rax, %rax
	movabsq	$0, %rbx
	je	.LBB44_37
# BB#15:                                # %for.cond
                                        #   in Loop: Header=BB44_16 Depth=1
	movq	8(%rsp), %rax
	incq	%r15
	cmpq	%rax, %r15
	movq	%r14, %rbx
	jl	.LBB44_16
	jmp	.LBB44_37
.LBB44_17:
	xorq	%rbx, %rbx
	jmp	.LBB44_37
.LBB44_20:
	xorq	%rbx, %rbx
	jmp	.LBB44_37
.LBB44_24:
	xorq	%rbx, %rbx
.LBB44_37:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp519:
	.size	retrieve_hash, .Ltmp519-retrieve_hash
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_sv_undef,@function
retrieve_sv_undef:                      # @retrieve_sv_undef
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp524:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp525:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp526:
	.cfi_def_cfa_offset 32
.Ltmp527:
	.cfi_offset %rbx, -32
.Ltmp528:
	.cfi_offset %r14, -24
.Ltmp529:
	.cfi_offset %r15, -16
	movq	32(%rdi), %rcx
	movq	64(%rdi), %rax
	movq	%rsi, %r14
	cmpq	$-1, %rcx
	jne	.LBB45_2
# BB#1:                                 # %if.then
	movq	%rax, 32(%rdi)
.LBB45_2:                               # %if.end3
	movq	24(%rdi), %rcx
	leaq	1(%rax), %rdx
	movq	%rdx, 64(%rdi)
	movabsq	$PL_sv_undef, %r15
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	Perl_av_store
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB45_5
# BB#3:                                 # %if.end7
	testq	%r14, %r14
	movq	%r15, %rbx
	je	.LBB45_5
# BB#4:                                 # %if.then9
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movabsq	$PL_sv_undef, %rbx
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%r15), %rax
	movq	$0, (%rax)
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB45_5:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp530:
	.size	retrieve_sv_undef, .Ltmp530-retrieve_sv_undef
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_sv_yes,@function
retrieve_sv_yes:                        # @retrieve_sv_yes
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp535:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp536:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp537:
	.cfi_def_cfa_offset 32
.Ltmp538:
	.cfi_offset %rbx, -32
.Ltmp539:
	.cfi_offset %r14, -24
.Ltmp540:
	.cfi_offset %r15, -16
	movq	64(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rsi, %r14
	leaq	1(%rax), %rdx
	movq	%rdx, 64(%rdi)
	movabsq	$PL_sv_yes, %r15
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	Perl_av_store
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB46_3
# BB#1:                                 # %if.end2
	testq	%r14, %r14
	movq	%r15, %rbx
	je	.LBB46_3
# BB#2:                                 # %if.then4
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movabsq	$PL_sv_yes, %rbx
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%r15), %rax
	movq	$0, (%rax)
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB46_3:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp541:
	.size	retrieve_sv_yes, .Ltmp541-retrieve_sv_yes
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_sv_no,@function
retrieve_sv_no:                         # @retrieve_sv_no
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp546:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp547:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp548:
	.cfi_def_cfa_offset 32
.Ltmp549:
	.cfi_offset %rbx, -32
.Ltmp550:
	.cfi_offset %r14, -24
.Ltmp551:
	.cfi_offset %r15, -16
	movq	64(%rdi), %rax
	movq	24(%rdi), %rcx
	movq	%rsi, %r14
	leaq	1(%rax), %rdx
	movq	%rdx, 64(%rdi)
	movabsq	$PL_sv_no, %r15
	movq	%rcx, %rdi
	movq	%rax, %rsi
	movq	%r15, %rdx
	callq	Perl_av_store
	xorq	%rbx, %rbx
	testq	%rax, %rax
	je	.LBB47_3
# BB#1:                                 # %if.end2
	testq	%r14, %r14
	movq	%r15, %rbx
	je	.LBB47_3
# BB#2:                                 # %if.then4
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movabsq	$PL_sv_no, %rbx
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%r15), %rax
	movq	$0, (%rax)
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB47_3:                               # %return
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp552:
	.size	retrieve_sv_no, .Ltmp552-retrieve_sv_no
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_blessed,@function
retrieve_blessed:                       # @retrieve_blessed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp559:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp560:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp561:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp562:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp563:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp564:
	.cfi_def_cfa_offset 192
.Ltmp565:
	.cfi_offset %rbx, -48
.Ltmp566:
	.cfi_offset %r12, -40
.Ltmp567:
	.cfi_offset %r13, -32
.Ltmp568:
	.cfi_offset %r14, -24
.Ltmp569:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB48_1
# BB#3:                                 # %if.else6
	callq	PerlIO_getc
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rsi
	movq	%rax, 136(%rsp)
	jne	.LBB48_4
	jmp	.LBB48_22
.LBB48_1:                               # %if.then
	movq	168(%r15), %rax
	movq	176(%r15), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	jae	.LBB48_22
# BB#2:                                 # %if.then2
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r15)
	movzbl	(%rax), %eax
	movq	%rax, 136(%rsp)
.LBB48_4:                               # %if.end14
	testq	$128, %rax
	je	.LBB48_5
# BB#6:                                 # %if.then16
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB48_7
# BB#11:                                # %if.else47
	leaq	136(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rbx, %rbx
	cmpq	$8, %rax
	jne	.LBB48_22
	jmp	.LBB48_12
.LBB48_5:
	leaq	(%rsp), %r14
	jmp	.LBB48_15
.LBB48_7:                               # %if.then19
	movq	168(%r15), %rcx
	movq	176(%r15), %rdx
	leaq	4(%rcx), %rax
	xorq	%rbx, %rbx
	cmpq	%rdx, %rax
	ja	.LBB48_22
# BB#8:                                 # %if.then26
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB48_10
# BB#9:                                 # %if.then34
	movslq	(%rcx), %rcx
	movq	%rcx, 136(%rsp)
	movq	%rax, 168(%r15)
	jmp	.LBB48_12
.LBB48_10:                              # %if.else38
	movl	(%rcx), %ecx
	movl	%ecx, 136(%rsp)
	movq	%rax, 168(%r15)
.LBB48_12:                              # %if.end54
	movl	80(%r15), %eax
	movq	136(%rsp), %rdi
	testq	%rax, %rax
	je	.LBB48_14
# BB#13:                                # %if.then56
	callq	Perl_my_ntohl
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	movq	%rdi, 136(%rsp)
.LBB48_14:                              # %if.end60
	incq	%rdi
	callq	Perl_safesysmalloc
	movq	%rax, %r14
.LBB48_15:                              # %if.end62
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB48_16
# BB#18:                                # %if.else81
	movq	136(%rsp), %rdx
	movq	%r14, %rsi
	callq	Perl_PerlIO_read
	movq	136(%rsp), %rcx
	xorq	%rbx, %rbx
	cmpq	%rcx, %rax
	jne	.LBB48_22
	jmp	.LBB48_19
.LBB48_16:                              # %if.then65
	movq	168(%r15), %rsi
	movq	136(%rsp), %rdx
	movq	176(%r15), %rax
	leaq	(%rsi,%rdx), %rcx
	xorq	%rbx, %rbx
	cmpq	%rax, %rcx
	ja	.LBB48_22
# BB#17:                                # %if.then73
	movq	%r14, %rdi
	callq	memcpy
	movq	136(%rsp), %rax
	addq	%rax, 168(%r15)
.LBB48_19:                              # %if.end88
	xorq	%rbx, %rbx
	movb	%bl, (%r14,%rax)
	movq	72(%r15), %r12
	movq	48(%r15), %r13
	leaq	1(%r12), %rax
	movq	%rax, 72(%r15)
	movq	136(%rsp), %rsi
	movq	%r14, %rdi
	callq	Perl_newSVpvn
	movq	%r13, %rdi
	movq	%r12, %rsi
	movq	%rax, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB48_22
# BB#20:                                # %if.end93
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	retrieve
	movq	%rax, %rbx
	leaq	(%rsp), %rax
	cmpq	%rax, %r14
	je	.LBB48_22
# BB#21:                                # %if.then98
	movq	%r14, %rdi
	callq	Perl_safesysfree
.LBB48_22:                              # %cleanup
	movq	%rbx, %rax
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp570:
	.size	retrieve_blessed, .Ltmp570-retrieve_blessed
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_idx_blessed,@function
retrieve_idx_blessed:                   # @retrieve_idx_blessed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp574:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp575:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp576:
	.cfi_def_cfa_offset 32
.Ltmp577:
	.cfi_offset %rbx, -24
.Ltmp578:
	.cfi_offset %r14, -16
	movq	%rdi, %r14
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB49_1
# BB#3:                                 # %if.else6
	callq	PerlIO_getc
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%rcx, %rcx
	cmpq	%rdx, %rsi
	movq	%rax, (%rsp)
	jne	.LBB49_4
	jmp	.LBB49_16
.LBB49_1:                               # %if.then
	movq	168(%r14), %rax
	movq	176(%r14), %rdx
	xorq	%rcx, %rcx
	cmpq	%rdx, %rax
	jae	.LBB49_16
# BB#2:                                 # %if.then2
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %eax
	movq	%rax, (%rsp)
.LBB49_4:                               # %if.end14
	testq	$128, %rax
	je	.LBB49_13
# BB#5:                                 # %if.then16
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB49_6
# BB#10:                                # %if.else47
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%rcx, %rcx
	cmpq	$8, %rax
	jne	.LBB49_16
	jmp	.LBB49_11
.LBB49_6:                               # %if.then19
	movq	168(%r14), %rdx
	movq	176(%r14), %rsi
	leaq	4(%rdx), %rax
	xorq	%rcx, %rcx
	cmpq	%rsi, %rax
	ja	.LBB49_16
# BB#7:                                 # %if.then26
	movq	%rdx, %rcx
	andq	$-4, %rcx
	cmpq	%rcx, %rdx
	jne	.LBB49_9
# BB#8:                                 # %if.then34
	movslq	(%rdx), %rcx
	movq	%rcx, (%rsp)
	movq	%rax, 168(%r14)
	jmp	.LBB49_11
.LBB49_9:                               # %if.else38
	movl	(%rdx), %ecx
	movl	%ecx, (%rsp)
	movq	%rax, 168(%r14)
.LBB49_11:                              # %if.end54
	movl	80(%r14), %ecx
	movq	(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB49_13
# BB#12:                                # %if.then56
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, (%rsp)
.LBB49_13:                              # %if.end61
	movq	48(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	Perl_av_fetch
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB49_15
# BB#14:                                # %if.then64
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movq	(%rsp), %rsi
	movabsq	$.L.str60, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB49_15:                              # %if.end65
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	(%rax), %rsi
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %rcx
.LBB49_16:                              # %return
	movq	%rcx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp579:
	.size	retrieve_idx_blessed, .Ltmp579-retrieve_idx_blessed
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_hook,@function
retrieve_hook:                          # @retrieve_hook
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp586:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp587:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp588:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp589:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp590:
	.cfi_def_cfa_offset 48
	subq	$256, %rsp              # imm = 0x100
.Ltmp591:
	.cfi_def_cfa_offset 304
.Ltmp592:
	.cfi_offset %rbx, -48
.Ltmp593:
	.cfi_offset %r12, -40
.Ltmp594:
	.cfi_offset %r13, -32
.Ltmp595:
	.cfi_offset %r14, -24
.Ltmp596:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	$0, 96(%rsp)
	movl	4(%r14), %r13d
	movq	216(%r14), %rdi
	andq	$4, %r13
	testq	%rdi, %rdi
	je	.LBB50_1
# BB#3:                                 # %if.else5
	callq	PerlIO_getc
	movq	%rax, %r15
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%r15, %rcx
	andq	%rax, %rcx
	xorq	%r12, %r12
	cmpq	%rax, %rcx
	jne	.LBB50_4
	jmp	.LBB50_210
.LBB50_1:                               # %if.then
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	jae	.LBB50_210
# BB#2:                                 # %if.then2
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %r15d
.LBB50_4:                               # %if.end11
	movq	%r15, %rax
	andq	$3, %rax
	cmpq	$3, %rax
	jbe	.LBB50_5
# BB#30:                                # %sw.default50
	movl	224(%r14), %edx
	movl	228(%r14), %ecx
	cmpq	$2, %rdx
	jne	.LBB50_34
# BB#31:
	movq	%rcx, %rbx
	jmp	.LBB50_32
.LBB50_5:                               # %if.end11
	jmpq	*.LJTI50_0(,%rax,8)
.LBB50_39:                              # %sw.bb
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	callq	Perl_newSV
	movq	%rax, %rbx
	jmp	.LBB50_40
.LBB50_34:                              # %land.lhs.true.i825
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	movq	$6, %rbx
	cmpq	$6, %rax
	jne	.LBB50_35
.LBB50_32:                              # %if.else.i835
	movq	$1, %rax
	movl	%eax, 112(%r14)
	cmpq	$0, 216(%r14)
	je	.LBB50_37
# BB#33:
	movabsq	$.L.str33, %rsi
	jmp	.LBB50_38
.LBB50_37:                              # %select.mid33
	movabsq	$.L.str34, %rsi
.LBB50_38:                              # %select.end32
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%rbx, %rcx
	callq	Perl_croak
	jmp	.LBB50_210
.LBB50_6:                               # %sw.bb14
	callq	Perl_newAV
	jmp	.LBB50_7
.LBB50_8:                               # %sw.bb16
	callq	Perl_newHV
.LBB50_7:                               # %sw.epilog52
	movq	%rax, %rbx
	xorq	%r12, %r12
.LBB50_40:                              # %sw.epilog52
	xorq	%rax, %rax
.LBB50_41:                              # %sw.epilog52
	testq	%rbx, %rbx
	je	.LBB50_42
# BB#43:                                # %if.end55
	movq	%rax, 64(%rsp)          # 8-byte Spill
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB50_44
# BB#45:                                # %while.cond.preheader
	movq	%r12, 48(%rsp)          # 8-byte Spill
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	testq	$64, %r15
	je	.LBB50_53
# BB#46:                                # %while.body.lr.ph
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	xorq	%r12, %r12
	.align	16, 0x90
.LBB50_47:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	retrieve
	testq	%rax, %rax
	je	.LBB50_210
# BB#48:                                # %if.end69
                                        #   in Loop: Header=BB50_47 Depth=1
	movq	%rax, %rdi
	callq	Perl_sv_free
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB50_51
# BB#49:                                # %if.then72
                                        #   in Loop: Header=BB50_47 Depth=1
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB50_210
# BB#50:                                # %if.then79
                                        #   in Loop: Header=BB50_47 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %r15d
	jmp	.LBB50_52
	.align	16, 0x90
.LBB50_51:                              # %if.else86
                                        #   in Loop: Header=BB50_47 Depth=1
	callq	PerlIO_getc
	movq	%rax, %r15
                                        # kill: RAX<def> R15<kill>
	andq	%rbx, %rax
	cmpq	%rbx, %rax
	je	.LBB50_210
.LBB50_52:                              # %while.cond.backedge
                                        #   in Loop: Header=BB50_47 Depth=1
	testq	$64, %r15
	jne	.LBB50_47
.LBB50_53:                              # %while.end
	movq	216(%r14), %rdi
	movq	%r15, %rax
	andq	$4, %rax
	movq	%r15, %r12
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	andq	$32, %r12
	je	.LBB50_76
# BB#54:                                # %if.then96
	testq	%rbx, %rax
	je	.LBB50_66
# BB#55:                                # %if.then99
	testq	%rdi, %rdi
	je	.LBB50_56
# BB#60:                                # %if.else130
	leaq	88(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	je	.LBB50_64
# BB#61:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_42:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_9:                               # %sw.bb18
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB50_10
# BB#12:                                # %if.else35
	callq	PerlIO_getc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	xorq	%r12, %r12
	cmpq	%rcx, %rdx
	jne	.LBB50_13
	jmp	.LBB50_210
.LBB50_35:                              # %if.then.i830
	movq	$1, %rax
	movl	%eax, 112(%r14)
	cmpq	$0, 216(%r14)
	je	.LBB50_26
# BB#36:
	movabsq	$.L.str33, %rsi
	jmp	.LBB50_27
.LBB50_44:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_10:                              # %if.then21
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	jae	.LBB50_210
# BB#11:                                # %if.then28
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %eax
.LBB50_13:                              # %if.end42
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	andq	%rsi, %rax
	cmpq	$6, %rax
	je	.LBB50_18
# BB#14:                                # %if.end42
	cmpq	$5, %rax
	jne	.LBB50_15
# BB#17:                                # %sw.bb45
	callq	Perl_newAV
	movq	%rax, %rbx
	movq	$5, %rax
	movq	$80, %r12
	jmp	.LBB50_41
.LBB50_76:                              # %if.else177
	testq	%rbx, %rax
	je	.LBB50_88
# BB#77:                                # %if.then180
	testq	%rdi, %rdi
	je	.LBB50_78
# BB#82:                                # %if.else212
	leaq	248(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	je	.LBB50_86
# BB#83:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_18:                              # %sw.bb47
	callq	Perl_newHV
	movq	%rax, %rbx
	movq	$6, %rax
	movq	$80, %r12
	jmp	.LBB50_41
.LBB50_15:                              # %if.end42
	cmpq	$4, %rax
	jne	.LBB50_19
# BB#16:                                # %sw.bb43
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rbx
	movq	$4, %rax
	movq	$113, %r12
	jmp	.LBB50_41
.LBB50_66:                              # %if.else144
	testq	%rdi, %rdi
	je	.LBB50_67
# BB#69:                                # %if.else162
	callq	PerlIO_getc
	movq	%rax, %rsi
                                        # kill: RAX<def> RSI<kill>
	andq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rbx, %rax
	movq	%rsi, 88(%rsp)
	jne	.LBB50_73
# BB#70:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_56:                              # %if.then102
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	jbe	.LBB50_58
# BB#57:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_88:                              # %if.else227
	testq	%rdi, %rdi
	je	.LBB50_89
# BB#91:                                # %if.else245
	callq	PerlIO_getc
	movq	%rax, %rdi
                                        # kill: RAX<def> RDI<kill>
	andq	%rbx, %rax
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rbx, %rax
	movq	%rdi, 248(%rsp)
	jne	.LBB50_95
# BB#92:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_19:                              # %sw.default
	movl	224(%r14), %edx
	movl	228(%r14), %ecx
	cmpq	$2, %rdx
	jne	.LBB50_23
# BB#20:
	movq	%rcx, %r8
	jmp	.LBB50_21
.LBB50_78:                              # %if.then183
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	jbe	.LBB50_80
# BB#79:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_67:                              # %if.then147
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB50_68
# BB#71:                                # %if.then154
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %esi
	jmp	.LBB50_72
.LBB50_58:                              # %if.then109
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB50_62
# BB#59:                                # %if.then117
	movslq	(%rcx), %rcx
	movq	%rcx, 88(%rsp)
	jmp	.LBB50_63
.LBB50_23:                              # %land.lhs.true.i
	andq	%rcx, %rsi
	movq	$6, %r8
	cmpq	$6, %rsi
	jne	.LBB50_24
.LBB50_21:                              # %if.else.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	cmpq	$0, 216(%r14)
	je	.LBB50_28
# BB#22:
	movabsq	$.L.str33, %rsi
	jmp	.LBB50_29
.LBB50_89:                              # %if.then230
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	jae	.LBB50_90
# BB#93:                                # %if.then237
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %edi
	jmp	.LBB50_94
.LBB50_80:                              # %if.then191
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB50_84
# BB#81:                                # %if.then199
	movslq	(%rcx), %rcx
	movq	%rcx, 248(%rsp)
	jmp	.LBB50_85
.LBB50_68:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_62:                              # %if.else121
	movl	(%rcx), %ecx
	movl	%ecx, 88(%rsp)
.LBB50_63:                              # %if.end137
	movq	%rax, 168(%r14)
.LBB50_64:                              # %if.end137
	movl	80(%r14), %eax
	movq	88(%rsp), %rsi
	testq	%rax, %rax
	je	.LBB50_73
# BB#65:                                # %if.then139
	movq	%rsi, %rdi
	callq	Perl_my_ntohl
	movq	%rax, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
.LBB50_72:                              # %if.end172
	movq	%rsi, 88(%rsp)
.LBB50_73:                              # %if.end172
	movq	48(%r14), %rdi
	xorq	%rdx, %rdx
	callq	Perl_av_fetch
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB50_75
# BB#74:                                # %if.then175
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movq	88(%rsp), %rsi
	movabsq	$.L.str60, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB50_75:                              # %if.end176
	movq	%r12, 32(%rsp)          # 8-byte Spill
	movq	(%rbx), %rax
	movq	(%rax), %rax
	movq	(%rax), %rax
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB50_105:                             # %if.end294
	movq	216(%r14), %rdi
	testq	$8, %r15
	je	.LBB50_117
# BB#106:                               # %if.then297
	testq	%rdi, %rdi
	je	.LBB50_107
# BB#111:                               # %if.else329
	leaq	104(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	je	.LBB50_115
# BB#112:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_117:                             # %if.else344
	testq	%rdi, %rdi
	je	.LBB50_118
# BB#120:                               # %if.else362
	callq	PerlIO_getc
	movq	%rax, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rdi, %rdx
	andq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	cmpq	%rax, %rdx
	movq	%rdi, 104(%rsp)
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB50_124
# BB#121:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_107:                             # %if.then300
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	jbe	.LBB50_109
# BB#108:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_118:                             # %if.then347
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	cmpq	%rcx, %rax
	movq	56(%rsp), %r12          # 8-byte Reload
	jae	.LBB50_119
# BB#122:                               # %if.then354
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %edi
	jmp	.LBB50_123
.LBB50_109:                             # %if.then308
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	movq	56(%rsp), %r12          # 8-byte Reload
	jne	.LBB50_113
# BB#110:                               # %if.then316
	movslq	(%rcx), %rcx
	movq	%rcx, 104(%rsp)
	jmp	.LBB50_114
.LBB50_28:                              # %select.mid23
	movabsq	$.L.str34, %rsi
.LBB50_29:                              # %select.end22
	movabsq	$.L.str40, %rdi
	xorq	%rax, %rax
	xorq	%r12, %r12
	movq	%r8, %rcx
	callq	Perl_croak
	jmp	.LBB50_210
.LBB50_24:                              # %if.then.i
	movq	$1, %rax
	movl	%eax, 112(%r14)
	cmpq	$0, 216(%r14)
	je	.LBB50_26
# BB#25:
	movabsq	$.L.str33, %rsi
	jmp	.LBB50_27
.LBB50_26:                              # %select.mid
	movabsq	$.L.str34, %rsi
.LBB50_27:                              # %select.end
	movabsq	$.L.str39, %rdi
	movq	$2, %r8
	movq	$6, %r9
	xorq	%rax, %rax
	xorq	%r12, %r12
	callq	Perl_croak
.LBB50_210:                             # %cleanup
	movq	%r12, %rax
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB50_90:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_119:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_84:                              # %if.else203
	movl	(%rcx), %ecx
	movl	%ecx, 248(%rsp)
.LBB50_85:                              # %if.end219
	movq	%rax, 168(%r14)
.LBB50_86:                              # %if.end219
	movl	80(%r14), %eax
	movq	248(%rsp), %rdi
	testq	%rax, %rax
	je	.LBB50_95
# BB#87:                                # %if.then222
	callq	Perl_my_ntohl
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
.LBB50_94:                              # %if.end255
	movq	%rdi, 248(%rsp)
.LBB50_95:                              # %if.end255
	cmpq	$128, %rdi
	jl	.LBB50_96
# BB#97:                                # %if.then258
	incq	%rdi
	callq	Perl_safesysmalloc
	movq	%rax, %rbx
	jmp	.LBB50_98
.LBB50_96:
	leaq	112(%rsp), %rbx
.LBB50_98:                              # %if.end260
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB50_99
# BB#101:                               # %if.else279
	movq	248(%rsp), %rdx
	movq	%rbx, %rsi
	callq	Perl_PerlIO_read
	movq	248(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB50_104
# BB#102:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_99:                              # %if.then263
	movq	168(%r14), %rsi
	movq	248(%rsp), %rdx
	movq	176(%r14), %rax
	leaq	(%rsi,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB50_103
# BB#100:
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_103:                             # %if.then271
	movq	%rbx, %rdi
	callq	memcpy
	movq	248(%rsp), %rax
	addq	%rax, 168(%r14)
.LBB50_104:                             # %if.end286
	movq	%r12, 32(%rsp)          # 8-byte Spill
	xorq	%r12, %r12
	movb	%r12b, (%rbx,%rax)
	movq	%rbx, %rdi
	movq	%rdi, 40(%rsp)          # 8-byte Spill
	movq	72(%r14), %rbx
	movq	48(%r14), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	1(%rbx), %rax
	movq	%rax, 72(%r14)
	movq	248(%rsp), %rsi
	callq	Perl_newSVpvn
	movq	24(%rsp), %rdi          # 8-byte Reload
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	jne	.LBB50_105
	jmp	.LBB50_210
.LBB50_113:                             # %if.else320
	movl	(%rcx), %ecx
	movl	%ecx, 104(%rsp)
.LBB50_114:                             # %if.end336
	movq	%rax, 168(%r14)
.LBB50_115:                             # %if.end336
	movl	80(%r14), %eax
	movq	104(%rsp), %rdi
	testq	%rax, %rax
	je	.LBB50_124
# BB#116:                               # %if.then339
	callq	Perl_my_ntohl
	movq	%rax, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
.LBB50_123:                             # %if.end372
	movq	%rdi, 104(%rsp)
.LBB50_124:                             # %if.end372
	callq	Perl_newSV
	movq	104(%rsp), %rdx
	movq	%rax, %rbx
	testq	%rdx, %rdx
	je	.LBB50_129
# BB#125:                               # %if.then375
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB50_126
# BB#139:                               # %if.else396
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	callq	Perl_PerlIO_read
	movq	104(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB50_128
	jmp	.LBB50_140
.LBB50_126:                             # %if.then378
	movq	168(%r14), %rsi
	movq	176(%r14), %rax
	leaq	(%rsi,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB50_127
.LBB50_140:                             # %if.else394
	movq	%rbx, %rdi
	callq	Perl_sv_free
	xorq	%r12, %r12
	jmp	.LBB50_210
.LBB50_127:                             # %if.then386
	movq	(%rbx), %rax
	movq	(%rax), %rdi
	callq	memcpy
	movq	104(%rsp), %rax
	addq	%rax, 168(%r14)
.LBB50_128:                             # %if.end405
	movq	(%rbx), %rcx
	movq	%rax, 8(%rcx)
	movq	(%rcx), %rcx
	xorq	%rdx, %rdx
	movb	%dl, (%rcx,%rax)
.LBB50_129:                             # %if.end412
	movq	16(%rbx), %rax
	andq	$1223753727, %rax       # imm = 0x48F0FFFF
	orq	$67371008, %rax         # imm = 0x4040000
	movq	%rax, 16(%rbx)
	movl	84(%r14), %eax
	testq	%rax, %rax
	je	.LBB50_133
# BB#130:                               # %if.end412
	movzbl	PL_tainting(%rip), %eax
	testq	$255, %rax
	je	.LBB50_133
# BB#131:                               # %if.end412
	movzbl	PL_tainted(%rip), %eax
	testq	$255, %rax
	je	.LBB50_133
# BB#132:                               # %if.then422
	movq	%rbx, %rdi
	callq	Perl_sv_taint
.LBB50_133:                             # %if.end426
	xorq	%rdx, %rdx
	testq	$128, %r15
	movabsq	$0, %rcx
	je	.LBB50_167
# BB#134:                               # %if.then429
	movq	216(%r14), %rdi
	testq	$16, %r15
	je	.LBB50_145
# BB#135:                               # %if.then432
	movq	%rbx, %r15
	movq	%r12, %rbx
	testq	%rdi, %rdi
	je	.LBB50_136
# BB#142:                               # %if.else464
	leaq	96(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	xorq	%r12, %r12
	cmpq	$8, %rax
	jne	.LBB50_210
	jmp	.LBB50_143
.LBB50_145:                             # %if.else479
	movq	%rbx, %r15
	movq	%r12, %rbx
	testq	%rdi, %rdi
	je	.LBB50_146
# BB#148:                               # %if.else497
	callq	PerlIO_getc
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rax, %rsi
	andq	%rdx, %rsi
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	xorq	%r12, %r12
	cmpq	%rdx, %rsi
	movq	%rax, 96(%rsp)
	jne	.LBB50_149
	jmp	.LBB50_210
.LBB50_136:                             # %if.then435
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	xorq	%r12, %r12
	cmpq	%rdx, %rax
	ja	.LBB50_210
# BB#137:                               # %if.then443
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB50_141
# BB#138:                               # %if.then451
	movslq	(%rcx), %rcx
	movq	%rcx, 96(%rsp)
	movq	%rax, 168(%r14)
	jmp	.LBB50_143
.LBB50_146:                             # %if.then482
	movq	168(%r14), %rax
	movq	176(%r14), %rcx
	xorq	%r12, %r12
	cmpq	%rcx, %rax
	jae	.LBB50_210
# BB#147:                               # %if.then489
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r14)
	movzbl	(%rax), %eax
	movq	%rax, 96(%rsp)
	jmp	.LBB50_149
.LBB50_141:                             # %if.else455
	movl	(%rcx), %ecx
	movl	%ecx, 96(%rsp)
	movq	%rax, 168(%r14)
.LBB50_143:                             # %if.end471
	movl	80(%r14), %ecx
	movq	96(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB50_149
# BB#144:                               # %if.then474
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 96(%rsp)
.LBB50_149:                             # %if.end507
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rcx
	movq	%rbx, %r12
	movq	%r15, %rbx
	je	.LBB50_167
# BB#150:                               # %if.end514
	callq	Perl_newAV
	movq	96(%rsp), %rsi
	movq	%rax, %r15
	incq	%rsi
	movq	%r15, %rdi
	callq	Perl_av_extend
	movq	96(%rsp), %rcx
	movq	(%r15), %rax
	xorq	%rdx, %rdx
	testq	%r15, %r15
	movq	%rcx, 8(%rax)
	movabsq	$0, %rcx
	je	.LBB50_167
# BB#151:                               # %if.then516
	cmpq	$0, 96(%rsp)
	movq	$1, %rdx
	jle	.LBB50_152
# BB#153:                               # %for.body.lr.ph
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
.LBB50_154:                             # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB50_159
# BB#155:                               # %if.then528
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	168(%r14), %rcx
	movq	176(%r14), %rdx
	leaq	4(%rcx), %rax
	xorq	%r12, %r12
	cmpq	%rdx, %rax
	ja	.LBB50_210
# BB#156:                               # %if.then536
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB50_158
# BB#157:                               # %if.then544
                                        #   in Loop: Header=BB50_154 Depth=1
	movslq	(%rcx), %rcx
	movq	%rcx, 80(%rsp)
	movq	%rax, 168(%r14)
	jmp	.LBB50_160
.LBB50_159:                             # %if.else557
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	$8, %rdx
	leaq	80(%rsp), %rsi
	callq	Perl_PerlIO_read
	xorq	%r12, %r12
	cmpq	$8, %rax
	je	.LBB50_160
	jmp	.LBB50_210
.LBB50_158:                             # %if.else548
                                        #   in Loop: Header=BB50_154 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, 80(%rsp)
	movq	%rax, 168(%r14)
.LBB50_160:                             # %if.end564
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	80(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	%rax, 80(%rsp)
	movq	24(%r14), %rdi
	xorq	%rdx, %rdx
	movq	%rax, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	jne	.LBB50_163
# BB#161:                               # %if.then569
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	80(%rsp), %rcx
	movq	32(%r14), %rax
	cmpq	%rax, %rcx
	jne	.LBB50_211
# BB#162:                               # %if.then572
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	$PL_sv_undef, 72(%rsp)
	leaq	72(%rsp), %rax
.LBB50_163:                             # %if.end576
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	(%rax), %rax
	testq	%rax, %rax
	movq	%rax, 72(%rsp)
	movq	%rax, PL_Sv(%rip)
	je	.LBB50_165
# BB#164:                               # %land.rhs578
                                        #   in Loop: Header=BB50_154 Depth=1
	incq	8(%rax)
.LBB50_165:                             # %land.end582
                                        #   in Loop: Header=BB50_154 Depth=1
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 8(%rcx,%r15,8)
	movq	96(%rsp), %rax
	incq	%r15
	cmpq	%rax, %r15
	jl	.LBB50_154
# BB#166:
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	56(%rsp), %r12          # 8-byte Reload
	movq	$1, %rdx
	jmp	.LBB50_167
.LBB50_152:
	movq	%r15, %rcx
.LBB50_167:                             # %if.end586
	movq	%rdx, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	%rbx, 16(%rsp)          # 8-byte Spill
	movq	$1, %rsi
	movq	40(%rsp), %rdi          # 8-byte Reload
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%r12, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	movq	%r12, %r15
	callq	Perl_sv_free
	movq	(%r15), %rax
	movq	56(%r14), %rdi
	movq	48(%rax), %rsi
	movabsq	$.L.str61, %rdx
	callq	pkg_can
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB50_170
# BB#168:                               # %if.then597
	movabsq	$.L.str58, %rdi
	movq	$8, %rsi
	callq	Perl_newSVpvn
	movq	%r15, %r12
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	Perl_sv_catpv
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	Perl_eval_sv
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	(%r12), %rax
	movq	48(%rax), %rax
	movq	(%rax), %rax
	movq	56(%r14), %r15
	movq	80(%rax), %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	$2, %rcx
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %r15
	movq	%rax, %rdx
	callq	Perl_hv_delete
	movq	(%r15), %rax
	movq	56(%r14), %rdi
	movq	48(%rax), %rsi
	movabsq	$.L.str61, %rdx
	callq	pkg_can
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB50_170
# BB#169:                               # %if.then609
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str62, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	40(%rsp), %rsi          # 8-byte Reload
	movq	%rsi, %rdx
	callq	Perl_croak
.LBB50_170:                             # %if.end612
	cmpq	$0, 8(%rsp)             # 8-byte Folded Reload
	movq	24(%rsp), %rdx          # 8-byte Reload
	jne	.LBB50_172
# BB#171:                               # %if.then614
	callq	Perl_newAV
	movq	%rax, %r12
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_av_extend
	movq	%r12, %rdx
	movq	(%rdx), %rax
	movq	$0, 8(%rax)
.LBB50_172:                             # %if.end618
	movq	%rbx, 8(%rsp)           # 8-byte Spill
	movq	16(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, PL_Sv(%rip)
	testq	%rcx, %rcx
	je	.LBB50_174
# BB#173:                               # %land.rhs620
	incq	8(%rcx)
.LBB50_174:                             # %land.end624
	movq	(%rdx), %rax
	movq	%rdx, %rbx
	movq	(%rax), %rax
	movq	%rcx, (%rax)
	movq	%r15, %rdi
	callq	Perl_newRV
	movq	PL_stack_sp(%rip), %r15
	movq	%rax, %r12
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB50_176
# BB#175:                               # %if.then.i839
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB50_176:                             # %if.end.i
	movq	PL_stack_base(%rip), %rcx
	movq	%r15, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%r15, %rax
	cmpq	$7, %rax
	jg	.LBB50_178
# BB#177:                               # %if.then6.i
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB50_178:                             # %if.end7.i
	movq	%r12, 8(%r15)
	movq	PL_stack_max(%rip), %rax
	addq	$8, %r15
	subq	%r15, %rax
	cmpq	$7, %rax
	jg	.LBB50_180
# BB#179:                               # %if.then15.i
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB50_180:                             # %if.end17.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	%r13, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r15)
	addq	$8, %r15
	movq	%rbx, %rdx
	testq	%rdx, %rdx
	je	.LBB50_181
# BB#182:                               # %if.then22.i
	movq	(%rdx), %rax
	movq	8(%rax), %rbx
	movq	PL_stack_max(%rip), %rcx
	movq	(%rax), %r13
	incq	%rbx
	subq	%r15, %rcx
	cmpq	$7, %rcx
	jg	.LBB50_183
# BB#184:                               # %if.then34.i
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
	jmp	.LBB50_185
.LBB50_181:
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
	jmp	.LBB50_190
.LBB50_183:
	movq	%r12, (%rsp)            # 8-byte Spill
	movq	%rdx, 24(%rsp)          # 8-byte Spill
.LBB50_185:                             # %if.end36.i
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movq	(%r13), %rdx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	%rdx, 8(%r15)
	leaq	8(%r15), %r15
	jl	.LBB50_190
# BB#186:                               # %for.body.lr.ph.i
	movq	$2, %r12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rbx
.LBB50_187:                             # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	PL_stack_max(%rip), %rax
	subq	%r15, %rax
	cmpq	$7, %rax
	jg	.LBB50_189
# BB#188:                               # %if.then47.i
                                        #   in Loop: Header=BB50_187 Depth=1
	movq	$1, %rdx
	movq	%r15, %rdi
	movq	%r15, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r15
.LBB50_189:                             # %if.end49.i
                                        #   in Loop: Header=BB50_187 Depth=1
	movq	-8(%r13,%r12,8), %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r15)
	addq	$8, %r15
	movq	%r12, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	incq	%r12
	cmpq	%rbx, %rax
	jne	.LBB50_187
.LBB50_190:                             # %if.end55.i
	movq	%r15, PL_stack_sp(%rip)
	movq	$2, %rsi
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	Perl_call_sv
	movq	PL_stack_sp(%rip), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	testq	%rdx, %rax
	je	.LBB50_191
# BB#192:                               # %if.then59.i
	movq	(%rcx), %rax
	addq	$-8, %rcx
	testq	%rax, %rax
	movq	%rax, PL_Sv(%rip)
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	(%rsp), %rbx            # 8-byte Reload
	je	.LBB50_194
# BB#193:                               # %land.rhs.i
	incq	8(%rax)
	jmp	.LBB50_194
.LBB50_191:
	movq	56(%rsp), %r13          # 8-byte Reload
	movq	32(%rsp), %r15          # 8-byte Reload
	movq	16(%rsp), %r12          # 8-byte Reload
	movq	(%rsp), %rbx            # 8-byte Reload
.LBB50_194:                             # %if.end64.i
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rdx
	movq	%rcx, PL_stack_sp(%rip)
	cmpq	%rdx, %rax
	jle	.LBB50_196
# BB#195:                               # %if.then67.i
	callq	Perl_free_tmps
.LBB50_196:                             # %scalar_call.exit
	callq	Perl_pop_scope
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_av_undef
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %r15
	movq	48(%rsp), %r12          # 8-byte Reload
	movq	40(%rsp), %rdi          # 8-byte Reload
	jne	.LBB50_199
# BB#197:                               # %scalar_call.exit
	leaq	112(%rsp), %rax
	cmpq	%rax, %rdi
	je	.LBB50_199
# BB#198:                               # %if.then636
	callq	Perl_safesysfree
.LBB50_199:                             # %if.end637
	movq	64(%rsp), %rax          # 8-byte Reload
	testq	%rbx, %rax
	je	.LBB50_209
# BB#200:                               # %if.end640
	movq	%rbx, %r15
	movq	%rax, %rbx
	xorq	%rsi, %rsi
	movq	%r14, %rdi
	callq	retrieve
	movq	%rbx, %rsi
	movq	%r15, %rcx
	movq	%rax, %r15
	andq	%rsi, %rcx
	cmpq	$6, %rcx
	je	.LBB50_206
# BB#201:                               # %if.end640
	cmpq	$5, %rcx
	jne	.LBB50_202
# BB#205:                               # %sw.bb644
	movq	$10, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_upgrade
	movq	(%r13), %rax
	movzbl	72(%rax), %ecx
	andq	$254, %rcx
	movb	%cl, 72(%rax)
	jmp	.LBB50_208
.LBB50_206:                             # %sw.bb650
	movq	$11, %rsi
	jmp	.LBB50_204
.LBB50_202:                             # %if.end640
	cmpq	$4, %rcx
	jne	.LBB50_207
# BB#203:                               # %sw.bb642
	movq	$7, %rsi
.LBB50_204:                             # %sw.epilog654
	movq	%r13, %rdi
	callq	Perl_sv_upgrade
	jmp	.LBB50_208
.LBB50_207:                             # %sw.default652
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str63, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB50_208:                             # %sw.epilog654
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	movq	%r13, %rdi
	movq	%r15, %rsi
	movq	%r12, %rdx
	callq	Perl_sv_magic
	movq	%r15, %rdi
	callq	Perl_sv_free
.LBB50_209:                             # %cleanup
	movq	%r13, %r12
	jmp	.LBB50_210
.LBB50_211:                             # %if.else573
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str36, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	Perl_croak
.Ltmp597:
	.size	retrieve_hook, .Ltmp597-retrieve_hook
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI50_0:
	.quad	.LBB50_39
	.quad	.LBB50_6
	.quad	.LBB50_8
	.quad	.LBB50_9

	.text
	.align	16, 0x90
	.type	retrieve_overloaded,@function
retrieve_overloaded:                    # @retrieve_overloaded
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp604:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp605:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp606:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp607:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp608:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp609:
	.cfi_def_cfa_offset 64
.Ltmp610:
	.cfi_offset %rbx, -48
.Ltmp611:
	.cfi_offset %r12, -40
.Ltmp612:
	.cfi_offset %r13, -32
.Ltmp613:
	.cfi_offset %r14, -24
.Ltmp614:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	callq	Perl_newSV
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB51_14
# BB#1:                                 # %if.end
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%r13, PL_Sv(%rip)
	incq	8(%r13)
	movq	%r13, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB51_14
# BB#2:                                 # %if.end6
	testq	%r15, %r15
	je	.LBB51_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%r13, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB51_4:                               # %if.end13
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB51_14
# BB#5:                                 # %if.end17
	movq	$3, %rsi
	movq	%r13, %rdi
	callq	Perl_sv_upgrade
	movq	(%r13), %rax
	movq	%r15, (%rax)
	orq	$524288, 16(%r13)       # imm = 0x80000
	movzbl	16(%r15), %eax
	testq	%rax, %rax
	je	.LBB51_7
# BB#6:                                 # %cond.end
	movq	(%r15), %rax
	movq	48(%rax), %r12
	testq	%r12, %r12
	jne	.LBB51_8
.LBB51_7:                               # %if.then25
	movq	$1, %rax
	movl	%eax, 112(%r14)
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r15, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$.L.str57, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r15, %rdx
	callq	Perl_croak
.LBB51_8:                               # %if.end27
	cmpq	$0, PL_amagic_generation(%rip)
	je	.LBB51_10
# BB#9:                                 # %land.lhs.true
	movq	%r12, %rdi
	callq	Perl_Gv_AMupdate
	testq	$255, %rax
	jne	.LBB51_13
.LBB51_10:                              # %if.then31
	movabsq	$.L.str58, %rdi
	movq	$8, %rsi
	callq	Perl_newSVpvn
	movq	(%r12), %rcx
	movq	80(%rcx), %rsi
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rbx
	movq	%rbx, %rdi
	callq	Perl_sv_catpv
	movq	$2, %rsi
	movq	%rbx, %rdi
	callq	Perl_eval_sv
	movq	%rbx, %rdi
	callq	Perl_sv_free
	cmpq	$0, PL_amagic_generation(%rip)
	je	.LBB51_12
# BB#11:                                # %land.lhs.true36
	movq	%r12, %rdi
	callq	Perl_Gv_AMupdate
	testq	$255, %rax
	jne	.LBB51_13
.LBB51_12:                              # %if.then40
	movq	$1, %rax
	movl	%eax, 112(%r14)
	xorq	%rsi, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$.L.str59, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r15, %rdx
	movq	8(%rsp), %rcx           # 8-byte Reload
	movq	%rcx, %r8
	callq	Perl_croak
.LBB51_13:                              # %if.end44
	orq	$268435456, 16(%r13)    # imm = 0x10000000
	movq	%r13, %rbx
.LBB51_14:                              # %return
	movq	%rbx, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp615:
	.size	retrieve_overloaded, .Ltmp615-retrieve_overloaded
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_tied_key,@function
retrieve_tied_key:                      # @retrieve_tied_key
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp621:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp622:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp623:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp624:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp625:
	.cfi_def_cfa_offset 48
.Ltmp626:
	.cfi_offset %rbx, -40
.Ltmp627:
	.cfi_offset %r12, -32
.Ltmp628:
	.cfi_offset %r14, -24
.Ltmp629:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%rdi, %r14
	xorq	%rdi, %rdi
	xorq	%rbx, %rbx
	callq	Perl_newSV
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB52_7
# BB#1:                                 # %if.end
	movq	64(%r14), %rsi
	movq	24(%r14), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r14)
	movq	%r15, PL_Sv(%rip)
	incq	8(%r15)
	movq	%r15, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB52_7
# BB#2:                                 # %if.end6
	testq	%r12, %r12
	je	.LBB52_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r12
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r12, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB52_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r12
	testq	%r12, %r12
	je	.LBB52_7
# BB#5:                                 # %if.end16
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	movq	%r14, %rdi
	callq	retrieve
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB52_7
# BB#6:                                 # %if.end20
	movq	$7, %rsi
	movq	%r15, %rdi
	callq	Perl_sv_upgrade
	movq	$112, %rdx
	movq	$-2, %r8
	movq	%r15, %rdi
	movq	%r12, %rsi
	movq	%r14, %rcx
	callq	Perl_sv_magic
	movq	%r14, %rdi
	callq	Perl_sv_free
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	%r15, %rbx
.LBB52_7:                               # %return
	movq	%rbx, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp630:
	.size	retrieve_tied_key, .Ltmp630-retrieve_tied_key
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_tied_idx,@function
retrieve_tied_idx:                      # @retrieve_tied_idx
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp637:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp638:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp639:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp640:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp641:
	.cfi_def_cfa_offset 48
	subq	$16, %rsp
.Ltmp642:
	.cfi_def_cfa_offset 64
.Ltmp643:
	.cfi_offset %rbx, -48
.Ltmp644:
	.cfi_offset %r12, -40
.Ltmp645:
	.cfi_offset %r13, -32
.Ltmp646:
	.cfi_offset %r14, -24
.Ltmp647:
	.cfi_offset %r15, -16
	movq	%rsi, %r15
	movq	%rdi, %r13
	xorq	%rdi, %rdi
	xorq	%r12, %r12
	callq	Perl_newSV
	movq	%rax, %r14
	testq	%r14, %r14
	je	.LBB53_14
# BB#1:                                 # %if.end
	movq	64(%r13), %rsi
	movq	24(%r13), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r13)
	movq	%r14, PL_Sv(%rip)
	incq	8(%r14)
	movq	%r14, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB53_14
# BB#2:                                 # %if.end6
	testq	%r15, %r15
	je	.LBB53_4
# BB#3:                                 # %if.then8
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r15
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %rbx
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_bless
	movq	(%rbx), %rax
	movq	$0, (%rax)
	movq	%rbx, %rdi
	callq	Perl_sv_free
.LBB53_4:                               # %if.end12
	xorq	%rsi, %rsi
	xorq	%r12, %r12
	movq	%r13, %rdi
	callq	retrieve
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB53_14
# BB#5:                                 # %if.end16
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB53_6
# BB#10:                                # %if.else38
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	jne	.LBB53_14
	jmp	.LBB53_11
.LBB53_6:                               # %if.then18
	movq	168(%r13), %rcx
	movq	176(%r13), %rdx
	leaq	4(%rcx), %rax
	cmpq	%rdx, %rax
	ja	.LBB53_14
# BB#7:                                 # %if.then21
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB53_9
# BB#8:                                 # %if.then27
	movslq	(%rcx), %rcx
	movq	%rcx, 8(%rsp)
	movq	%rax, 168(%r13)
	jmp	.LBB53_11
.LBB53_9:                               # %if.else
	movl	(%rcx), %ecx
	movl	%ecx, 8(%rsp)
	movq	%rax, 168(%r13)
.LBB53_11:                              # %if.end45
	movl	80(%r13), %eax
	testq	%rax, %rax
	je	.LBB53_13
# BB#12:                                # %if.then47
	movq	8(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 8(%rsp)
.LBB53_13:                              # %if.end51
	movq	$7, %rsi
	movq	%r14, %rdi
	callq	Perl_sv_upgrade
	movq	8(%rsp), %r8
	movq	$112, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_sv_magic
	movq	%r15, %rdi
	callq	Perl_sv_free
	movq	%r14, %r12
.LBB53_14:                              # %return
	movq	%r12, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp648:
	.size	retrieve_tied_idx, .Ltmp648-retrieve_tied_idx
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_utf8str,@function
retrieve_utf8str:                       # @retrieve_utf8str
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp650:
	.cfi_def_cfa_offset 16
	callq	retrieve_scalar
	testq	%rax, %rax
	je	.LBB54_2
# BB#1:                                 # %if.then
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB54_2:                               # %if.end
	popq	%rdx
	retq
.Ltmp651:
	.size	retrieve_utf8str, .Ltmp651-retrieve_utf8str
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_lutf8str,@function
retrieve_lutf8str:                      # @retrieve_lutf8str
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp653:
	.cfi_def_cfa_offset 16
	callq	retrieve_lscalar
	testq	%rax, %rax
	je	.LBB55_2
# BB#1:                                 # %if.then
	orq	$536870912, 16(%rax)    # imm = 0x20000000
.LBB55_2:                               # %if.end
	popq	%rdx
	retq
.Ltmp654:
	.size	retrieve_lutf8str, .Ltmp654-retrieve_lutf8str
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_flag_hash,@function
retrieve_flag_hash:                     # @retrieve_flag_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp661:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp662:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp663:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp664:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp665:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp666:
	.cfi_def_cfa_offset 96
.Ltmp667:
	.cfi_offset %rbx, -48
.Ltmp668:
	.cfi_offset %r12, -40
.Ltmp669:
	.cfi_offset %r13, -32
.Ltmp670:
	.cfi_offset %r14, -24
.Ltmp671:
	.cfi_offset %r15, -16
	movq	%rdi, %rbx
	movq	216(%rbx), %rdi
	movq	%rsi, %r12
	testq	%rdi, %rdi
	je	.LBB56_1
# BB#3:                                 # %if.else5
	callq	PerlIO_getc
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	%rax, %rcx
	andq	%rdx, %rcx
	xorq	%r14, %r14
	cmpq	%rdx, %rcx
	je	.LBB56_56
# BB#4:                                 # %if.end11
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB56_5
# BB#9:                                 # %if.else41
	movq	%rax, 24(%rsp)          # 8-byte Spill
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_read
	cmpq	$8, %rax
	jne	.LBB56_56
	jmp	.LBB56_10
.LBB56_1:                               # %if.then
	movq	168(%rbx), %rax
	movq	176(%rbx), %rcx
	xorq	%r14, %r14
	cmpq	%rcx, %rax
	jae	.LBB56_56
# BB#2:                                 # %if.end11.thread
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%rbx)
	movzbl	(%rax), %eax
.LBB56_5:
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	168(%rbx), %rcx
	movq	176(%rbx), %rdx
	leaq	4(%rcx), %rax
	xorq	%r14, %r14
	cmpq	%rdx, %rax
	ja	.LBB56_56
# BB#6:                                 # %if.then21
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB56_8
# BB#7:                                 # %if.then28
	movslq	(%rcx), %rcx
	movq	%rcx, 40(%rsp)
	movq	%rax, 168(%rbx)
	jmp	.LBB56_10
.LBB56_8:                               # %if.else32
	movl	(%rcx), %ecx
	movl	%ecx, 40(%rsp)
	movq	%rax, 168(%rbx)
.LBB56_10:                              # %if.end48
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB56_12
# BB#11:                                # %if.then50
	movq	40(%rsp), %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 40(%rsp)
.LBB56_12:                              # %if.end54
	callq	Perl_newHV
	xorq	%r14, %r14
	testq	%rax, %rax
	je	.LBB56_56
# BB#13:                                # %if.end58
	movq	64(%rbx), %rsi
	movq	24(%rbx), %rdi
	leaq	1(%rsi), %rcx
	movq	%rcx, 64(%rbx)
	movq	%rax, PL_Sv(%rip)
	incq	8(%rax)
	movq	%rax, %rdx
	movq	%rax, %r15
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB56_56
# BB#14:                                # %if.end66
	testq	%r12, %r12
	je	.LBB56_16
# BB#15:                                # %if.then68
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_gv_stashpv
	movq	%rax, %r14
	movq	%r15, %rdi
	callq	Perl_newRV_noinc
	movq	%rax, %r12
	movq	%r12, %rdi
	movq	%r14, %rsi
	callq	Perl_sv_bless
	movq	(%r12), %rax
	movq	$0, (%rax)
	movq	%r12, %rdi
	callq	Perl_sv_free
.LBB56_16:                              # %if.end72
	movq	40(%rsp), %rsi
	testq	%rsi, %rsi
	movq	%r15, %r14
	je	.LBB56_56
# BB#17:                                # %if.end76
	movq	%r15, %rdi
	callq	Perl_hv_ksplit
	cmpq	$0, 40(%rsp)
	jle	.LBB56_18
# BB#19:                                # %for.body.lr.ph
	movq	%r15, 16(%rsp)          # 8-byte Spill
	andq	$1, 24(%rsp)            # 8-byte Folded Spill
	xorq	%r15, %r15
	.align	16, 0x90
.LBB56_20:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	retrieve
	movq	%rax, %r13
	testq	%r13, %r13
	je	.LBB56_21
# BB#22:                                # %if.end82
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB56_25
# BB#23:                                # %if.then85
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	168(%rbx), %rax
	movq	176(%rbx), %rcx
	cmpq	%rcx, %rax
	jae	.LBB56_24
# BB#27:                                # %if.then92
                                        #   in Loop: Header=BB56_20 Depth=1
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%rbx)
	movzbl	(%rax), %r12d
	jmp	.LBB56_28
.LBB56_25:                              # %if.else99
                                        #   in Loop: Header=BB56_20 Depth=1
	callq	PerlIO_getc
	movq	%rax, %r12
                                        # kill: RAX<def> R12<kill>
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	je	.LBB56_26
.LBB56_28:                              # %if.end106
                                        #   in Loop: Header=BB56_20 Depth=1
	cmpq	$0, 24(%rsp)            # 8-byte Folded Reload
	je	.LBB56_31
# BB#29:                                # %if.end106
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	%r12, %rax
	andq	$4, %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB56_31
# BB#30:                                # %if.then111
                                        #   in Loop: Header=BB56_20 Depth=1
	orq	$8388608, 16(%r13)      # imm = 0x800000
.LBB56_31:                              # %if.end112
                                        #   in Loop: Header=BB56_20 Depth=1
	testq	$8, %r12
	jne	.LBB56_32
# BB#34:                                # %if.else124
                                        #   in Loop: Header=BB56_20 Depth=1
	xorq	%rax, %rax
	testq	$16, %r12
	je	.LBB56_36
# BB#35:                                # %if.then127
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	%r13, %rdi
	callq	Perl_sv_free
	movq	$512, %rax              # imm = 0x200
	movabsq	$PL_sv_placeholder, %r13
.LBB56_36:                              # %if.end129
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	216(%rbx), %rdi
	andq	$3, %r12
	orq	%rax, %r12
	testq	%rdi, %rdi
	jne	.LBB56_41
# BB#37:                                # %if.then142
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	168(%rbx), %rcx
	movq	176(%rbx), %rdx
	leaq	4(%rcx), %rax
	xorq	%r14, %r14
	cmpq	%rdx, %rax
	ja	.LBB56_56
# BB#38:                                # %if.then150
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	%rcx, %rdx
	andq	$-4, %rdx
	cmpq	%rdx, %rcx
	jne	.LBB56_40
# BB#39:                                # %if.then158
                                        #   in Loop: Header=BB56_20 Depth=1
	movslq	(%rcx), %rcx
	movq	%rcx, 32(%rsp)
	movq	%rax, 168(%rbx)
	jmp	.LBB56_42
.LBB56_32:                              # %if.then115
                                        #   in Loop: Header=BB56_20 Depth=1
	xorq	%rsi, %rsi
	xorq	%r14, %r14
	movq	%rbx, %rdi
	callq	retrieve
	testq	%rax, %rax
	je	.LBB56_56
# BB#33:                                # %if.end119
                                        #   in Loop: Header=BB56_20 Depth=1
	xorq	%rcx, %rcx
	xorq	%r14, %r14
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%rax, %rsi
	movq	%r13, %rdx
	callq	Perl_hv_store_ent
	testq	%rax, %rax
	jne	.LBB56_52
	jmp	.LBB56_56
.LBB56_41:                              # %if.else171
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	$8, %rdx
	leaq	32(%rsp), %rsi
	callq	Perl_PerlIO_read
	xorq	%r14, %r14
	cmpq	$8, %rax
	je	.LBB56_42
	jmp	.LBB56_56
.LBB56_40:                              # %if.else162
                                        #   in Loop: Header=BB56_20 Depth=1
	movl	(%rcx), %ecx
	movl	%ecx, 32(%rsp)
	movq	%rax, 168(%rbx)
.LBB56_42:                              # %if.end178
                                        #   in Loop: Header=BB56_20 Depth=1
	movl	80(%rbx), %ecx
	movq	32(%rsp), %rax
	testq	%rcx, %rcx
	je	.LBB56_44
# BB#43:                                # %if.then181
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	%rax, %rdi
	callq	Perl_my_ntohl
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	%rax, 32(%rsp)
.LBB56_44:                              # %if.end185
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	128(%rbx), %rcx
	cmpq	%rcx, %rax
	jb	.LBB56_46
# BB#45:                                # %if.then188
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	120(%rbx), %rdi
	incq	%rax
	movq	%rax, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 120(%rbx)
	movq	32(%rsp), %rax
	leaq	1(%rax), %rcx
	movq	%rcx, 128(%rbx)
.LBB56_46:                              # %if.end196
                                        #   in Loop: Header=BB56_20 Depth=1
	xorq	%rdx, %rdx
	testq	%rax, %rax
	movabsq	$0, %rcx
	je	.LBB56_51
# BB#47:                                # %if.then198
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB56_50
# BB#48:                                # %if.then201
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	168(%rbx), %rsi
	movq	176(%rbx), %rcx
	leaq	(%rsi,%rax), %rdx
	xorq	%r14, %r14
	cmpq	%rcx, %rdx
	ja	.LBB56_56
# BB#49:                                # %if.then209
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	120(%rbx), %rdi
	movq	%rax, %rdx
	callq	memcpy
	xorq	%rdx, %rdx
	movq	32(%rsp), %rcx
	addq	%rcx, 168(%rbx)
	jmp	.LBB56_51
.LBB56_50:                              # %if.else219
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	120(%rbx), %rsi
	movq	%rax, %rdx
	callq	Perl_PerlIO_read
	movq	32(%rsp), %rdx
	movq	%rax, %rcx
	xorq	%r14, %r14
	cmpq	%rdx, %rcx
	movabsq	$0, %rdx
	jne	.LBB56_56
.LBB56_51:                              # %if.end229
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	120(%rbx), %rax
	movb	%dl, (%rax,%rcx)
	movq	120(%rbx), %rsi
	movq	32(%rsp), %rdx
	xorq	%r8, %r8
	movq	16(%rsp), %rdi          # 8-byte Reload
	movq	%r13, %rcx
	movq	%r12, %r9
	callq	Perl_hv_store_flags
	testq	%rax, %rax
	movabsq	$0, %r14
	je	.LBB56_56
.LBB56_52:                              # %for.inc
                                        #   in Loop: Header=BB56_20 Depth=1
	movq	40(%rsp), %rax
	incq	%r15
	cmpq	%rax, %r15
	jl	.LBB56_20
# BB#53:
	movq	24(%rsp), %rax          # 8-byte Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	jmp	.LBB56_54
.LBB56_18:                              # %if.end76.for.end_crit_edge
	movq	24(%rsp), %rax          # 8-byte Reload
	andq	$1, %rax
.LBB56_54:                              # %for.end
	testq	%rax, %rax
	movq	%r15, %r14
	je	.LBB56_56
# BB#55:                                # %if.then243
	orq	$8388608, 16(%r15)      # imm = 0x800000
	movq	%r15, %r14
	jmp	.LBB56_56
.LBB56_21:
	xorq	%r14, %r14
	jmp	.LBB56_56
.LBB56_24:
	xorq	%r14, %r14
	jmp	.LBB56_56
.LBB56_26:
	xorq	%r14, %r14
.LBB56_56:                              # %return
	movq	%r14, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp672:
	.size	retrieve_flag_hash, .Ltmp672-retrieve_flag_hash
	.cfi_endproc

	.align	16, 0x90
	.type	retrieve_code,@function
retrieve_code:                          # @retrieve_code
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp679:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp680:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp681:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp682:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp683:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp684:
	.cfi_def_cfa_offset 96
.Ltmp685:
	.cfi_offset %rbx, -48
.Ltmp686:
	.cfi_offset %r12, -40
.Ltmp687:
	.cfi_offset %r13, -32
.Ltmp688:
	.cfi_offset %r14, -24
.Ltmp689:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movq	PL_stack_sp(%rip), %r13
	movq	64(%r15), %r14
	movq	%rsi, %r12
	xorq	%rdi, %rdi
	callq	Perl_newSViv
	movq	%rax, %rbx
	testq	%rbx, %rbx
	je	.LBB57_1
# BB#2:                                 # %if.end
	movq	%r13, 40(%rsp)          # 8-byte Spill
	movq	64(%r15), %rsi
	movq	24(%r15), %rdi
	leaq	1(%rsi), %rax
	movq	%rax, 64(%r15)
	movq	%rbx, PL_Sv(%rip)
	incq	8(%rbx)
	movq	%rbx, %rdx
	callq	Perl_av_store
	testq	%rax, %rax
	je	.LBB57_3
# BB#4:                                 # %if.end9
	movq	%r14, 32(%rsp)          # 8-byte Spill
	movq	%r12, %r14
	testq	%r14, %r14
	je	.LBB57_6
# BB#5:                                 # %if.then11
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_gv_stashpv
	movq	%r14, %r12
	movq	%rax, %r14
	movq	%rbx, %rdi
	callq	Perl_newRV_noinc
	movq	%r15, %r13
	movq	%rax, %r15
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%r12, %r14
	callq	Perl_sv_bless
	movq	(%r15), %rax
	movq	$0, (%rax)
	movq	%r15, %rdi
	movq	%r13, %r15
	callq	Perl_sv_free
.LBB57_6:                               # %if.end15
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	movabsq	$0, %r12
	je	.LBB57_7
# BB#9:                                 # %if.else26
	callq	PerlIO_getc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rcx, %rdx
	andq	%rax, %rdx
	cmpq	%rax, %rdx
	jne	.LBB57_10
	jmp	.LBB57_105
.LBB57_1:
	xorq	%r12, %r12
	jmp	.LBB57_105
.LBB57_3:
	xorq	%r12, %r12
	jmp	.LBB57_105
.LBB57_7:                               # %if.then17
	movq	168(%r15), %rax
	movq	176(%r15), %rcx
	cmpq	%rcx, %rax
	jae	.LBB57_105
# BB#8:                                 # %if.then21
	leaq	1(%rax), %rcx
	movq	%rcx, 168(%r15)
	movzbl	(%rax), %ecx
.LBB57_10:                              # %if.end33
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	movq	%rcx, %rax
	andq	%r12, %rax
	cmpq	$1, %rax
	jne	.LBB57_11
# BB#13:                                # %sw.bb35
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	retrieve_lscalar
	movq	%rax, %rbx
	jmp	.LBB57_15
.LBB57_11:                              # %if.end33
	cmpq	$10, %rax
	jne	.LBB57_14
# BB#12:                                # %sw.bb
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	retrieve_scalar
	movq	%rax, %rbx
	jmp	.LBB57_15
.LBB57_14:                              # %sw.default
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str48, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	Perl_croak
                                        # implicit-def: RBX
.LBB57_15:                              # %sw.epilog
	movabsq	$.L.str49, %rdi
	movq	$4, %rsi
	callq	Perl_newSVpvn
	movzbl	18(%rbx), %ecx
	movq	%rax, %r13
	testq	$4, %rcx
	je	.LBB57_17
# BB#16:                                # %cond.true
	movq	(%rbx), %rax
	movq	(%rax), %rsi
	jmp	.LBB57_18
.LBB57_17:                              # %cond.false
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rsi
.LBB57_18:                              # %cond.end
	movq	%r13, %rdi
	callq	Perl_sv_catpv
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movq	96(%r15), %rdi
	testq	%rdi, %rdi
	jne	.LBB57_22
# BB#19:                                # %if.then44
	movabsq	$.L.str50, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	%rax, %rax
	movq	%rax, 96(%r15)
	movq	%rax, PL_Sv(%rip)
	je	.LBB57_21
# BB#20:                                # %land.rhs49
	incq	8(%rax)
.LBB57_21:                              # %if.end55
	movq	96(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB57_46
.LBB57_22:                              # %cond.false59
	movq	16(%rdi), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB57_41
# BB#23:                                # %cond.true64
	movq	(%rdi), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB57_46
# BB#24:                                # %land.lhs.true
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB57_27
# BB#25:                                # %lor.lhs.false
	testq	%rcx, %rcx
	je	.LBB57_46
# BB#26:                                # %land.lhs.true72
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB57_46
	jmp	.LBB57_27
.LBB57_41:                              # %cond.false79
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB57_43
# BB#42:                                # %cond.true84
	movq	(%rdi), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB57_27
	jmp	.LBB57_46
.LBB57_43:                              # %cond.false89
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB57_45
# BB#44:                                # %cond.true94
	movq	(%rdi), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	jne	.LBB57_27
	jmp	.LBB57_46
.LBB57_45:                              # %cond.false99
	callq	Perl_sv_2bool
	testq	$255, %rax
	jne	.LBB57_27
.LBB57_46:                              # %if.then104
	movl	88(%r15), %eax
	testq	%rax, %rax
	je	.LBB57_70
# BB#47:                                # %lor.lhs.false107
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB57_71
# BB#48:                                # %land.lhs.true111
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	%rax, %rax
	je	.LBB57_49
# BB#50:                                # %cond.false115
	movabsq	$.L.str51, %r12
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_get_sv
	movq	16(%rax), %rbx
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_get_sv
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB57_57
# BB#51:                                # %cond.true120
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB57_66
# BB#52:                                # %land.rhs124
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB57_66
# BB#53:                                # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB57_66
# BB#54:                                # %land.rhs130
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB57_55
# BB#56:                                # %land.rhs130
	movabsq	$1, %rax
	jmp	.LBB57_66
.LBB57_49:                              # %cond.end172.thread
	xorq	%rax, %rax
	movl	%eax, 88(%r15)
	jmp	.LBB57_70
.LBB57_57:                              # %cond.false140
	movq	16(%rax), %rbx
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, %rbx            # imm = 0x10000
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	je	.LBB57_61
# BB#58:                                # %cond.true145
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB57_59
# BB#60:                                # %cond.true145
	movabsq	$1, %rax
	jmp	.LBB57_66
.LBB57_61:                              # %cond.false151
	movq	16(%rax), %rbx
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, %rbx           # imm = 0x20000
	je	.LBB57_65
# BB#62:                                # %cond.true156
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB57_63
# BB#64:                                # %cond.true156
	movabsq	$1, %rax
	jmp	.LBB57_66
.LBB57_59:
	movabsq	$0, %rax
	jmp	.LBB57_66
.LBB57_55:
	movabsq	$0, %rax
	jmp	.LBB57_66
.LBB57_65:                              # %cond.false162
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB57_66
.LBB57_63:
	movabsq	$0, %rax
.LBB57_66:                              # %cond.end172
	andq	%r12, %rax
	je	.LBB57_67
# BB#68:                                # %cond.end172
	movabsq	$1, %rcx
	jmp	.LBB57_69
.LBB57_67:
	movabsq	$0, %rcx
.LBB57_69:                              # %cond.end172
	testq	%rax, %rax
	movl	%ecx, 88(%r15)
	je	.LBB57_70
.LBB57_71:                              # %if.else180
	movq	%r13, %rdi
	callq	Perl_newSVsv
	movq	%rax, %r12
	movq	$32, %rcx
	movq	32(%rsp), %rsi          # 8-byte Reload
	shlq	%cl, %rsi
	movq	$32, %rcx
	movq	24(%r15), %rdi
	sarq	%cl, %rsi
	testq	%r12, %r12
	movq	%r12, PL_Sv(%rip)
	je	.LBB57_104
# BB#72:                                # %land.rhs185
	incq	8(%r12)
	jmp	.LBB57_104
.LBB57_70:                              # %if.then178
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str52, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB57_27:                              # %if.end193
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rax
	movq	%rax, PL_tmps_floor(%rip)
	movq	96(%r15), %rax
	movzbl	18(%rax), %ecx
	testq	$8, %rcx
	je	.LBB57_86
# BB#28:                                # %land.lhs.true198
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	16(%rax), %eax
	cmpq	$12, %rax
	jne	.LBB57_86
# BB#29:                                # %if.then206
	movabsq	$.L.str53, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	movabsq	$.L.str15, %rsi
	movq	%rax, %rdi
	movq	%rax, %r14
	callq	Perl_sv_setpv
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rcx
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rax
	jne	.LBB57_31
# BB#30:                                # %if.then211
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB57_31:                              # %if.end212
	movq	PL_stack_base(%rip), %rcx
	movq	40(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB57_33
# BB#32:                                # %if.then219
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB57_33:                              # %if.end221
	movq	%r13, %rdi
	movq	%r13, 40(%rsp)          # 8-byte Spill
	callq	Perl_newSVsv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	96(%r15), %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_sv
	movq	PL_stack_sp(%rip), %r13
	andq	%r12, %rax
	cmpq	$1, %rax
	je	.LBB57_35
# BB#34:                                # %if.then231
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str54, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB57_35:                              # %if.end233
	movq	(%r13), %r12
	movq	%r14, %rdx
	testq	%rdx, %rdx
	movq	32(%rsp), %rbx          # 8-byte Reload
	movq	24(%rsp), %r14          # 8-byte Reload
	je	.LBB57_85
# BB#36:                                # %cond.false237
	movq	16(%rdx), %rax
	testq	$262144, %rax           # imm = 0x40000
	je	.LBB57_73
# BB#37:                                # %cond.true241
	movq	(%rdx), %rax
	testq	%rax, %rax
	movq	%rax, PL_Xpv(%rip)
	je	.LBB57_85
# BB#38:                                # %land.lhs.true244
	movq	8(%rax), %rcx
	cmpq	$1, %rcx
	ja	.LBB57_78
# BB#39:                                # %lor.lhs.false248
	testq	%rcx, %rcx
	je	.LBB57_85
# BB#40:                                # %land.lhs.true251
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	jne	.LBB57_78
	jmp	.LBB57_85
.LBB57_86:                              # %if.else305
	movzbl	18(%r13), %eax
	testq	$4, %rax
	movq	32(%rsp), %rbx          # 8-byte Reload
	je	.LBB57_88
# BB#87:                                # %cond.true310
	movq	(%r13), %rax
	movq	(%rax), %rdi
	jmp	.LBB57_89
.LBB57_88:                              # %cond.false313
	movq	%r13, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rdi
.LBB57_89:                              # %cond.end315
	movq	24(%rsp), %r14          # 8-byte Reload
	movq	$1, %rsi
	callq	Perl_eval_pv
	movq	%rax, %r12
	jmp	.LBB57_90
.LBB57_73:                              # %cond.false258
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB57_75
# BB#74:                                # %cond.true262
	movq	(%rdx), %rax
	cmpq	$0, 24(%rax)
	jne	.LBB57_78
	jmp	.LBB57_85
.LBB57_75:                              # %cond.false267
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB57_77
# BB#76:                                # %cond.true271
	movq	(%rdx), %rax
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	movq	16(%rsp), %rdx          # 8-byte Reload
	testq	%rax, %rax
	jne	.LBB57_78
	jmp	.LBB57_85
.LBB57_77:                              # %cond.false276
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	16(%rsp), %rdi          # 8-byte Reload
	callq	Perl_sv_2bool
	movq	16(%rsp), %rdx          # 8-byte Reload
	testq	$255, %rax
	je	.LBB57_85
.LBB57_78:                              # %if.then280
	movq	%rdx, 16(%rsp)          # 8-byte Spill
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movq	%r15, 8(%rsp)           # 8-byte Spill
	movq	40(%rsp), %rdi          # 8-byte Reload
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB57_80
# BB#79:                                # %cond.true286
	movq	(%rdi), %rax
	movq	(%rax), %r15
	jmp	.LBB57_81
.LBB57_80:                              # %cond.false289
	callq	Perl_sv_2pv_nolen
	movq	%rax, %r15
.LBB57_81:                              # %cond.end291
	movq	16(%rsp), %rdi          # 8-byte Reload
	movzbl	18(%rdi), %eax
	testq	$4, %rax
	je	.LBB57_83
# BB#82:                                # %cond.true297
	movq	(%rdi), %rax
	movq	(%rax), %rdx
	jmp	.LBB57_84
.LBB57_83:                              # %cond.false300
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rdx
.LBB57_84:                              # %cond.end302
	movabsq	$.L.str55, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	Perl_croak
	movq	8(%rsp), %r15           # 8-byte Reload
.LBB57_85:                              # %if.end304
	addq	$-8, %r13
	movq	%r13, PL_stack_sp(%rip)
	movq	40(%rsp), %r13          # 8-byte Reload
.LBB57_90:                              # %if.end318
	testq	%r12, %r12
	je	.LBB57_93
# BB#91:                                # %land.lhs.true320
	movzbl	18(%r12), %eax
	testq	$8, %rax
	je	.LBB57_93
# BB#92:                                # %land.lhs.true324
	movq	(%r12), %rax
	movq	(%rax), %r12
	movzbl	16(%r12), %eax
	cmpq	$12, %rax
	je	.LBB57_97
.LBB57_93:                              # %if.else334
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movzbl	18(%r13), %eax
	testq	$4, %rax
	je	.LBB57_95
# BB#94:                                # %cond.true340
	movq	(%r13), %rax
	movq	(%rax), %rsi
	jmp	.LBB57_96
.LBB57_95:                              # %cond.false343
	movq	%r13, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rsi
.LBB57_96:                              # %cond.end345
	movabsq	$.L.str56, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
	movq	%r14, %r12
.LBB57_97:                              # %if.end347
	movq	%r12, PL_Sv(%rip)
	testq	%r12, %r12
	je	.LBB57_99
# BB#98:                                # %land.rhs349
	incq	8(%r12)
.LBB57_99:                              # %land.end353
	movq	%r13, %rdi
	callq	Perl_sv_free
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB57_101
# BB#100:                               # %if.then357
	callq	Perl_free_tmps
.LBB57_101:                             # %if.end358
	callq	Perl_pop_scope
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	movq	24(%r15), %rdi
	sarq	%cl, %rbx
	testq	%r12, %r12
	movq	%r12, PL_Sv(%rip)
	je	.LBB57_103
# BB#102:                               # %land.rhs362
	incq	8(%r12)
.LBB57_103:                             # %land.end366
	movq	%rbx, %rsi
.LBB57_104:                             # %return
	movq	%r12, %rdx
	callq	Perl_av_store
.LBB57_105:                             # %return
	movq	%r12, %rax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp690:
	.size	retrieve_code, .Ltmp690-retrieve_code
	.cfi_endproc

	.align	16, 0x90
	.type	pkg_can,@function
pkg_can:                                # @pkg_can
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp696:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp697:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp698:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp699:
	.cfi_def_cfa_offset 40
	pushq	%rax
.Ltmp700:
	.cfi_def_cfa_offset 48
.Ltmp701:
	.cfi_offset %rbx, -40
.Ltmp702:
	.cfi_offset %r12, -32
.Ltmp703:
	.cfi_offset %r14, -24
.Ltmp704:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	(%r12), %rax
	movq	80(%rax), %rbx
	movq	%rdx, %r15
	movq	%rdi, %r14
	movq	%rbx, %rdi
	callq	strlen
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB58_3
# BB#1:                                 # %if.then
	movq	(%rax), %rcx
	movzbl	18(%rcx), %eax
	andq	$1807, %rax             # imm = 0x70F
	je	.LBB58_9
# BB#2:                                 # %if.then
	movq	%rcx, %rax
	jmp	.LBB58_9
.LBB58_3:                               # %if.end
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%r15, %rsi
	callq	Perl_gv_fetchmethod_autoload
	testq	%rax, %rax
	je	.LBB58_6
# BB#4:                                 # %land.lhs.true.i
	movzbl	16(%rax), %ecx
	cmpq	$13, %rcx
	jne	.LBB58_6
# BB#5:                                 # %if.then.i
	movq	(%rax), %rax
	movq	56(%rax), %rax
	movq	56(%rax), %rdi
	callq	Perl_newRV
	jmp	.LBB58_7
.LBB58_6:                               # %if.else.i
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_newSVsv
.LBB58_7:                               # %pkg_fetchmeth.exit
	movq	%rax, %r15
	movq	(%r12), %rax
	movq	80(%rax), %rbx
	movq	%rbx, %rdi
	callq	strlen
	xorq	%r8, %r8
	movq	%r14, %rdi
	movq	%rbx, %rsi
	movq	%rax, %rdx
	movq	%r15, %rcx
	callq	Perl_hv_store
	movzbl	18(%r15), %eax
	andq	$1807, %rax             # imm = 0x70F
	je	.LBB58_9
# BB#8:                                 # %pkg_fetchmeth.exit
	movq	%r15, %rax
.LBB58_9:                               # %return
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp705:
	.size	pkg_can, .Ltmp705-pkg_can
	.cfi_endproc

	.align	16, 0x90
	.type	store,@function
store:                                  # @store
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp710:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp711:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp712:
	.cfi_def_cfa_offset 32
	subq	$16, %rsp
.Ltmp713:
	.cfi_def_cfa_offset 48
.Ltmp714:
	.cfi_offset %rbx, -32
.Ltmp715:
	.cfi_offset %r14, -24
.Ltmp716:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movq	%rsi, 8(%rsp)
	movq	8(%r14), %rbx
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	movq	%rbx, %rdi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB59_18
# BB#1:                                 # %if.then
	movq	8(%rsp), %rcx
	movabsq	$PL_sv_undef, %rsi
	cmpq	%rsi, %rcx
	je	.LBB59_2
# BB#3:                                 # %if.end
	movq	(%rax), %rdi
	callq	Perl_my_htonl
	movq	%rax, (%rsp)
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB59_4
# BB#7:                                 # %if.else40
	xorq	%rsi, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB59_8
	jmp	.LBB59_24
.LBB59_18:                              # %if.end128
	movq	64(%r14), %rcx
	incq	%rcx
	movq	%rcx, 64(%r14)
	leaq	8(%rsp), %rsi
	movq	$8, %rdx
	xorq	%r8, %r8
	movq	%rbx, %rdi
	callq	Perl_hv_store
	testq	%rax, %rax
	je	.LBB59_19
# BB#20:                                # %if.end135
	movq	8(%rsp), %rdi
	callq	sv_type
	movq	8(%rsp), %rsi
	movq	%rax, %rdx
	jmp	.LBB59_21
.LBB59_2:                               # %if.then2
	incq	64(%r14)
	movq	$1, %rdx
.LBB59_21:                              # %undef_special_case
	movzbl	17(%rsi), %eax
	testq	$16, %rax
	je	.LBB59_23
# BB#22:                                # %if.then139
	movq	(%rsi), %rax
	movq	48(%rax), %rcx
	movq	%r14, %rdi
	callq	store_blessed
	jmp	.LBB59_24
.LBB59_23:                              # %if.else142
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%r14, %rdi
	callq	*sv_store(,%rdx,8)
	jmp	.LBB59_24
.LBB59_4:                               # %if.then5
	movq	168(%r14), %r15
	movq	176(%r14), %rax
	cmpq	%rax, %r15
	jae	.LBB59_6
# BB#5:                                 # %if.then8
	leaq	1(%r15), %rax
	movq	%rax, 168(%r14)
	xorq	%rax, %rax
	movb	%al, (%r15)
	jmp	.LBB59_8
.LBB59_19:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB59_24
.LBB59_6:                               # %if.else
	movq	152(%r14), %rdi
	movq	160(%r14), %rax
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$35184372088832, %rbx   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%rbx, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r14)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r14)
	xorq	%rcx, %rcx
	movb	%cl, (%rax,%r15)
.LBB59_8:                               # %if.end47
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB59_9
# BB#16:                                # %if.else120
	leaq	(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB59_15
# BB#17:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB59_24
.LBB59_9:                               # %if.then50
	movq	168(%r14), %rbx
	movq	176(%r14), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB59_11
# BB#10:                                # %if.then58
	movq	152(%r14), %rdi
	movq	160(%r14), %rax
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %rax
	movabsq	$35197256990720, %r15   # imm = 0x200300000000
	addq	%rax, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r14)
	addq	%rax, %r15
	movq	%r15, 176(%r14)
.LBB59_11:                              # %if.end100
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB59_13
# BB#12:                                # %if.then108
	movq	(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB59_14
.LBB59_13:                              # %if.else112
	movl	(%rsp), %eax
	movl	%eax, (%rbx)
	movq	168(%r14), %rbx
.LBB59_14:                              # %if.end115
	addq	$4, %rbx
	movq	%rbx, 168(%r14)
.LBB59_15:                              # %if.end127
	xorq	%rax, %rax
.LBB59_24:                              # %return
	addq	$16, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp717:
	.size	store, .Ltmp717-store
	.cfi_endproc

	.align	16, 0x90
	.type	store_blessed,@function
store_blessed:                          # @store_blessed
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp724:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp725:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp726:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp727:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp728:
	.cfi_def_cfa_offset 48
	subq	$224, %rsp
.Ltmp729:
	.cfi_def_cfa_offset 272
.Ltmp730:
	.cfi_offset %rbx, -48
.Ltmp731:
	.cfi_offset %r12, -40
.Ltmp732:
	.cfi_offset %r13, -32
.Ltmp733:
	.cfi_offset %r14, -24
.Ltmp734:
	.cfi_offset %r15, -16
	movq	%rcx, %rbx
	movq	%rdx, %r15
	movq	%rsi, %r14
	movq	%rdi, %r12
	movq	56(%r12), %rdi
	movabsq	$.L.str82, %rdx
	movq	%rbx, %rsi
	callq	pkg_can
	testq	%rax, %rax
	je	.LBB60_255
# BB#1:                                 # %if.then
	movq	%rax, 120(%rsp)         # 8-byte Spill
	movq	%rbx, 104(%rsp)         # 8-byte Spill
	movl	4(%r12), %r13d
	andq	$4, %r13
	leaq	-1(%r15), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	$3, %rax
	jbe	.LBB60_2
# BB#10:                                # %sw.default7.i
	movq	$1, %rax
	movl	%eax, 112(%r12)
	movq	%r12, 40(%rsp)          # 8-byte Spill
	movabsq	$.L.str84, %rdi
	xorq	%rax, %rax
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	movq	%r15, %rsi
	movq	%r15, 96(%rsp)          # 8-byte Spill
	callq	Perl_croak
	xorq	%rcx, %rcx
                                        # implicit-def: RAX
	movq	%rax, 72(%rsp)          # 8-byte Spill
	jmp	.LBB60_11
.LBB60_255:                             # %if.end
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movq	(%rbx), %rax
	movq	80(%rax), %r14
	movq	%r14, %rdi
	callq	strlen
	movq	%rax, %rbx
	movq	%rbx, 144(%rsp)
	movq	40(%r12), %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB60_256
# BB#261:                               # %if.then6
	movq	(%rax), %rax
	movq	%rax, 136(%rsp)
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB60_262
# BB#265:                               # %if.else44
	movq	$18, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_266
	jmp	.LBB60_338
.LBB60_2:                               # %if.then
	movq	%r15, 96(%rsp)          # 8-byte Spill
	movq	%r12, 40(%rsp)          # 8-byte Spill
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%rcx, %rcx
	movq	$0, 72(%rsp)            # 8-byte Folded Spill
	jmpq	*.LJTI60_0(,%rax,8)
.LBB60_3:                               # %sw.bb1.i
	movq	$1, 72(%rsp)            # 8-byte Folded Spill
	jmp	.LBB60_4
.LBB60_256:                             # %if.end.i
	movq	72(%r12), %rcx
	incq	%rcx
	movq	%rcx, 72(%r12)
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r14, %rsi
	movq	%rbx, %rdx
	callq	Perl_hv_store
	testq	%rax, %rax
	movq	%r12, %rbx
	jne	.LBB60_258
# BB#257:                               # %if.then7.i
	movq	$1, %rax
	movl	%eax, 112(%rbx)
	movabsq	$.L.str83, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB60_258:                             # %if.else362
	movq	72(%rbx), %rax
	movq	%rax, 136(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_259
# BB#296:                               # %if.else422
	movq	$17, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_297
	jmp	.LBB60_338
.LBB60_262:                             # %if.then8
	movq	168(%r12), %r14
	movq	176(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB60_264
# BB#263:                               # %if.then11
	leaq	1(%r14), %rax
	movq	%rax, 168(%r12)
	movq	$18, %rax
	movb	%al, (%r14)
	jmp	.LBB60_266
.LBB60_5:                               # %sw.bb2.i
	movq	$2, 72(%rsp)            # 8-byte Folded Spill
.LBB60_4:                               # %sw.epilog8.i
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
	xorq	%rcx, %rcx
	jmp	.LBB60_11
.LBB60_6:                               # %sw.bb3.i
	movzbl	16(%r14), %eax
	cmpq	$11, %rax
	movq	$3, 72(%rsp)            # 8-byte Folded Spill
	movq	$80, %rcx
	movq	$6, 48(%rsp)            # 8-byte Folded Spill
	je	.LBB60_11
# BB#7:                                 # %sw.bb3.i
	cmpq	$10, %rax
	jne	.LBB60_9
# BB#8:                                 # %sw.bb6.i
	movq	$5, 48(%rsp)            # 8-byte Folded Spill
	jmp	.LBB60_11
.LBB60_259:                             # %if.then365
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_295
# BB#260:                               # %if.then372
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	$17, %rax
	movb	%al, (%r15)
	jmp	.LBB60_297
.LBB60_264:                             # %if.else
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r12)
	movq	$18, %rcx
	movb	%cl, (%rax,%r14)
.LBB60_266:                             # %if.end51
	movq	136(%rsp), %rbx
	cmpq	$127, %rbx
	jg	.LBB60_271
# BB#267:                               # %if.then54
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB60_268
# BB#270:                               # %if.else116
	andq	$255, %rbx
	movq	%rbx, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	movq	%r12, %rbx
	jne	.LBB60_336
	jmp	.LBB60_338
.LBB60_271:                             # %if.else125
	movq	%r12, %rbx
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_272
# BB#275:                               # %if.else186
	movq	$128, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_276
	jmp	.LBB60_338
.LBB60_268:                             # %if.then59
	movq	168(%r12), %r15
	movq	176(%r12), %rax
	cmpq	%rax, %r15
	jae	.LBB60_334
# BB#269:                               # %if.then66
	leaq	1(%r15), %rax
	movq	%rax, 168(%r12)
	movb	%bl, (%r15)
	jmp	.LBB60_335
.LBB60_272:                             # %if.then129
	movq	168(%rbx), %r14
	movq	176(%rbx), %rax
	cmpq	%rax, %r14
	jae	.LBB60_274
# BB#273:                               # %if.then136
	leaq	1(%r14), %rax
	movq	%rax, 168(%rbx)
	movq	$128, %rax
	movb	%al, (%r14)
	jmp	.LBB60_276
.LBB60_295:                             # %if.else376
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	$17, %rcx
	movb	%cl, (%rax,%r15)
.LBB60_297:                             # %if.end429
	movq	%rbx, %rax
	movq	144(%rsp), %rbx
	cmpq	$127, %rbx
	jg	.LBB60_302
# BB#298:                               # %if.then432
	movq	%rax, %r13
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB60_299
# BB#301:                               # %if.else494
	andq	$255, %rbx
	movq	%rbx, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	movq	%r13, %rbx
	jne	.LBB60_328
	jmp	.LBB60_338
.LBB60_302:                             # %if.else503
	movq	%rax, %rbx
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_303
# BB#306:                               # %if.else565
	movq	$128, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_307
	jmp	.LBB60_338
.LBB60_299:                             # %if.then437
	movq	168(%r13), %r12
	movq	176(%r13), %rax
	cmpq	%rax, %r12
	jae	.LBB60_326
# BB#300:                               # %if.then444
	leaq	1(%r12), %rax
	movq	%rax, 168(%r13)
	movb	%bl, (%r12)
	jmp	.LBB60_327
.LBB60_303:                             # %if.then508
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_305
# BB#304:                               # %if.then515
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	$128, %rax
	movb	%al, (%r15)
	jmp	.LBB60_307
.LBB60_9:                               # %sw.default.i
	movq	$113, %rcx
	movq	$4, 48(%rsp)            # 8-byte Folded Spill
.LBB60_11:                              # %sw.epilog8.i
	movq	%rcx, 24(%rsp)          # 8-byte Spill
	movq	104(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	80(%rax), %rdi
	movq	%rdi, 80(%rsp)          # 8-byte Spill
	callq	strlen
	movq	%rax, 216(%rsp)
	movq	%r14, %rdi
	callq	Perl_newRV_noinc
	movq	PL_stack_sp(%rip), %rbx
	movq	%rax, %r15
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB60_13
# BB#12:                                # %if.then.i.i
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB60_13:                              # %if.end.i.i
	movq	%r14, 64(%rsp)          # 8-byte Spill
	movq	PL_stack_base(%rip), %rcx
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB60_15
# BB#14:                                # %if.then6.i.i
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB60_15:                              # %if.end7.i.i
	movq	%r15, 8(%rbx)
	movq	PL_stack_max(%rip), %rax
	addq	$8, %rbx
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB60_17
# BB#16:                                # %if.then15.i.i
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB60_17:                              # %if.end17.i.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %r13
	movq	%r13, %rdi
	callq	Perl_newSViv
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movq	$1, %rsi
	movq	120(%rsp), %rdi         # 8-byte Reload
	callq	Perl_call_sv
	movq	%rax, %rbx
	movq	$32, %rcx
	movq	%rbx, %r12
	movq	PL_stack_sp(%rip), %r14
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	callq	Perl_newAV
	movq	%rax, 120(%rsp)         # 8-byte Spill
	testq	%r12, %r12
	jle	.LBB60_23
# BB#18:                                # %for.body.lr.ph.i.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	leaq	(%rbx,%rax), %rax
	movabsq	$-4294967296, %r13      # imm = 0xFFFFFFFF00000000
	orq	%rax, %r13
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	decq	%rbx
	movq	%r14, %r12
	.align	16, 0x90
.LBB60_19:                              # %for.body.i.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12), %rdx
	testq	%rdx, %rdx
	movq	%rdx, PL_Sv(%rip)
	je	.LBB60_21
# BB#20:                                # %land.rhs.i.i
                                        #   in Loop: Header=BB60_19 Depth=1
	incq	8(%rdx)
.LBB60_21:                              # %land.end.i.i
                                        #   in Loop: Header=BB60_19 Depth=1
	addq	$-8, %r12
	movq	120(%rsp), %rdi         # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_av_store
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	decq	%rbx
	testq	%rax, %rax
	jg	.LBB60_19
# BB#22:                                # %for.cond.for.end_crit_edge.i.i
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	xorq	%rax, %r13
	leaq	(%r14,%r13,8), %r14
.LBB60_23:                              # %for.end.i.i
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	movq	%r14, PL_stack_sp(%rip)
	cmpq	%rcx, %rax
	movq	40(%rsp), %rbx          # 8-byte Reload
	jle	.LBB60_25
# BB#24:                                # %if.then34.i.i
	callq	Perl_free_tmps
.LBB60_25:                              # %array_call.exit.i
	callq	Perl_pop_scope
	movq	(%r15), %rax
	movq	$0, (%rax)
	movq	%r15, %rdi
	callq	Perl_sv_free
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	8(%rax), %rdx
	incq	%rdx
	movq	%rdx, 112(%rsp)         # 8-byte Spill
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rdx
	movq	104(%rsp), %r13         # 8-byte Reload
	je	.LBB60_26
# BB#32:                                # %if.end23.i
	movq	(%rax), %r12
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	(%r12), %rdi
	movzbl	18(%rdi), %eax
	movq	72(%rsp), %rcx          # 8-byte Reload
	orq	$64, %rcx
	movq	%rcx, 56(%rsp)          # 8-byte Spill
	testq	$4, %rax
	je	.LBB60_34
# BB#33:                                # %cond.true.i
	movq	(%rdi), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 208(%rsp)
	movq	(%rax), %rax
	jmp	.LBB60_35
.LBB60_26:                              # %if.then.i1053
	movq	40(%rbx), %rdi
	movq	216(%rsp), %rdx
	xorq	%rcx, %rcx
	movq	80(%rsp), %r14          # 8-byte Reload
	movq	%r14, %rsi
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB60_31
# BB#27:                                # %if.then16.i
	movq	$1, %rax
	movl	%eax, 112(%rbx)
	movl	4(%rbx), %eax
	andq	$4, %rax
	movq	$2, %rcx
	shrq	%cl, %rax
	testq	%rax, %rax
	jne	.LBB60_28
# BB#29:                                # %if.then16.i
	movabsq	$.L.str87, %rsi
	jmp	.LBB60_30
.LBB60_34:                              # %cond.false.i
	leaq	208(%rsp), %rsi
	movq	$2, %rdx
	callq	Perl_sv_2pv_flags
.LBB60_35:                              # %cond.end.i
	movq	%rax, 16(%rsp)          # 8-byte Spill
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	cmpq	$2, %rax
	jl	.LBB60_70
# BB#36:                                # %for.body.i.lr.ph
	movq	48(%rsp), %rax          # 8-byte Reload
	andq	$255, %rax
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	56(%rsp), %rax          # 8-byte Reload
	andq	$255, %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	movq	$0, 96(%rsp)            # 8-byte Folded Spill
	movq	$1, %r14
	.align	16, 0x90
.LBB60_37:                              # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	(%r12,%r14,8), %r12
	movzbl	18(%r12), %eax
	movq	16(%rbx), %rcx
	movq	%rcx, 104(%rsp)         # 8-byte Spill
	testq	$8, %rax
	jne	.LBB60_39
# BB#38:                                # %if.then41.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	$1, %rax
	movl	%eax, 112(%rbx)
	xorq	%rax, %rax
	movabsq	$.L.str88, %rdi
	movq	%r14, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	Perl_croak
.LBB60_39:                              # %if.end43.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	(%r12), %rax
	movq	(%rax), %rax
	movq	%rax, 192(%rsp)
	movq	8(%rbx), %rdi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	leaq	192(%rsp), %rsi
	callq	Perl_hv_fetch
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB60_40
# BB#41:                                # %if.end49.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	216(%rbx), %rdi
	movq	96(%rsp), %rax          # 8-byte Reload
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %rax
	leaq	1(%rax), %rax
	jne	.LBB60_76
# BB#42:                                # %if.then51.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB60_43
# BB#45:                                # %if.else92.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	$19, %rsi
	callq	PerlIO_putc
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB60_48
	jmp	.LBB60_46
	.align	16, 0x90
.LBB60_40:                              #   in Loop: Header=BB60_37 Depth=1
	movq	%rbx, %r13
	jmp	.LBB60_69
	.align	16, 0x90
.LBB60_76:                              # %if.else240.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	%rax, 96(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB60_77
# BB#80:                                # %if.else300.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	PerlIO_putc
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB60_63
	jmp	.LBB60_81
.LBB60_43:                              # %if.then53.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_47
# BB#44:                                # %if.then57.i
                                        #   in Loop: Header=BB60_37 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	$19, %rax
	movb	%al, (%r15)
	jmp	.LBB60_48
.LBB60_77:                              # %if.then243.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_79
# BB#78:                                # %if.then250.i
                                        #   in Loop: Header=BB60_37 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	56(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r15)
	jmp	.LBB60_62
.LBB60_47:                              # %if.else.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	152(%rbx), %rdi
	movq	%rbx, %r13
	movq	160(%r13), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%rbx, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r13)
	movq	%r13, %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	$19, %rcx
	movb	%cl, (%rax,%r15)
.LBB60_48:                              # %if.end99.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB60_51
# BB#49:                                # %if.then102.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_53
# BB#50:                                # %if.then109.i
                                        #   in Loop: Header=BB60_37 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	56(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r15)
	jmp	.LBB60_54
.LBB60_51:                              # %if.else159.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	32(%rsp), %rsi          # 8-byte Reload
	callq	PerlIO_putc
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB60_55
	jmp	.LBB60_52
.LBB60_79:                              # %if.else254.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	152(%rbx), %rdi
	movq	%rbx, %r13
	movq	160(%r13), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%rbx, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r13)
	movq	%r13, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	56(%rsp), %rcx          # 8-byte Reload
	jmp	.LBB60_61
.LBB60_53:                              # %if.else113.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	152(%rbx), %rdi
	movq	%rbx, %r13
	movq	160(%r13), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%rbx, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r13)
	movq	%r13, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	56(%rsp), %rcx          # 8-byte Reload
	movb	%cl, (%rax,%r15)
.LBB60_54:                              # %if.end167.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB60_55:                              # %if.end167.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%r13, %rax
	cmpq	$3, %rax
	jne	.LBB60_63
# BB#56:                                # %if.then170.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	jne	.LBB60_74
# BB#57:                                # %if.then173.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_60
# BB#58:                                # %if.then180.i
                                        #   in Loop: Header=BB60_37 Depth=1
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	48(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r15)
	jmp	.LBB60_62
.LBB60_74:                              # %if.else230.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	callq	PerlIO_putc
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB60_63
	jmp	.LBB60_75
.LBB60_60:                              # %if.else184.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	152(%rbx), %rdi
	movq	%rbx, %r13
	movq	160(%r13), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%rbx, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r13)
	movq	%r13, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	48(%rsp), %rcx          # 8-byte Reload
.LBB60_61:                              # %if.end309.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movb	%cl, (%rax,%r15)
.LBB60_62:                              # %if.end309.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB60_63:                              # %if.end309.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	192(%rsp), %rsi
	movq	%rbx, %rdi
	callq	store
	movq	%rax, %r15
	testq	%r13, %r15
	jne	.LBB60_338
# BB#64:                                # %if.end313.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	8(%rbx), %rdi
	movq	$8, %rdx
	xorq	%rcx, %rcx
	leaq	192(%rsp), %rsi
	callq	Perl_hv_fetch
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB60_66
# BB#65:                                # %if.then317.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	$1, %rax
	movl	%eax, 112(%rbx)
	xorq	%rax, %rax
	movabsq	$.L.str89, %rdi
	movq	%r14, %rsi
	movq	80(%rsp), %rdx          # 8-byte Reload
	callq	Perl_croak
.LBB60_66:                              # %if.end319.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	%rbx, %r13
	movq	104(%rsp), %rdi         # 8-byte Reload
	movq	(%rdi), %rax
	movq	8(%rax), %rsi
	movq	192(%rsp), %rdx
	incq	%rsi
	testq	%rdx, %rdx
	movq	%rdx, PL_Sv(%rip)
	je	.LBB60_68
# BB#67:                                # %land.rhs.i
                                        #   in Loop: Header=BB60_37 Depth=1
	incq	8(%rdx)
.LBB60_68:                              # %land.end.i
                                        #   in Loop: Header=BB60_37 Depth=1
	callq	Perl_av_store
.LBB60_69:                              # %sv_seen.i
                                        #   in Loop: Header=BB60_37 Depth=1
	movq	$32, %rcx
	movq	112(%rsp), %rbx         # 8-byte Reload
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movq	%r12, %rdi
	callq	Perl_sv_free
	movq	(%r15), %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	movq	%rax, (%r12,%r14,8)
	incq	%r14
	movq	$32, %rcx
	movq	%r14, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rbx, %rax
	movq	%r13, %rbx
	jl	.LBB60_37
.LBB60_70:                              # %for.end.i
	movslq	216(%rsp), %r14
	movq	40(%rbx), %r15
	xorq	%rcx, %rcx
	movq	%r15, %rdi
	movq	80(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	Perl_hv_fetch
	testq	%rax, %rax
	je	.LBB60_71
# BB#82:                                # %known_class.exit.i
	movq	(%rax), %rax
	jmp	.LBB60_83
.LBB60_71:                              # %if.end.i2478.i
	movq	72(%rbx), %rcx
	incq	%rcx
	movq	%rcx, 72(%rbx)
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%r13, %rsi
	movq	%r14, %rdx
	callq	Perl_hv_store
	testq	%rax, %rax
	jne	.LBB60_73
# BB#72:                                # %if.then7.i.i
	movq	$1, %rax
	movl	%eax, 112(%rbx)
	movabsq	$.L.str83, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB60_73:                              # %if.then333.i
	movq	72(%rbx), %rax
	movq	%rax, 200(%rsp)
	movq	$-1, %rax
.LBB60_83:                              # %if.end335.i
	movq	%rax, 200(%rsp)
	cmpq	$-1, %rax
	je	.LBB60_84
# BB#85:                                # %if.end335.i
	movq	%rax, %rcx
	jmp	.LBB60_86
.LBB60_84:
	movq	216(%rsp), %rcx
.LBB60_86:                              # %if.end335.i
	cmpq	$255, %rcx
	jg	.LBB60_87
# BB#88:                                # %if.end335.i
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	72(%rsp), %r14          # 8-byte Reload
	jmp	.LBB60_89
.LBB60_87:
	movq	%r12, 88(%rsp)          # 8-byte Spill
	movq	72(%rsp), %r14          # 8-byte Reload
	orq	$4, %r14
.LBB60_89:                              # %if.end335.i
	cmpq	$-1, %rax
	je	.LBB60_91
# BB#90:                                # %if.end335.i
	orq	$32, %r14
.LBB60_91:                              # %if.end335.i
	movq	208(%rsp), %rax
	cmpq	$255, %rax
	jbe	.LBB60_93
# BB#92:
	orq	$8, %r14
.LBB60_93:                              # %select.end
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$1, %rax
	jle	.LBB60_95
# BB#94:
	orq	$-128, %r14
.LBB60_95:                              # %select.end
	cmpq	$256, %rax              # imm = 0x100
	jle	.LBB60_97
# BB#96:
	orq	$16, %r14
.LBB60_97:                              # %select.end
	movq	216(%rbx), %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	96(%rsp), %rcx          # 8-byte Reload
	testq	%rax, %rcx
	je	.LBB60_98
# BB#113:                               # %if.else587.i
	testq	%rdi, %rdi
	je	.LBB60_114
# BB#117:                               # %if.else647.i
	movq	%r14, %rsi
	andq	$255, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB60_120
# BB#118:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_98:                              # %if.then379.i
	testq	%rdi, %rdi
	je	.LBB60_99
# BB#102:                               # %if.else439.i
	movq	$19, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_103
	jmp	.LBB60_338
.LBB60_28:
	movabsq	$.L.str86, %rsi
.LBB60_30:                              # %if.then16.i
	movabsq	$.L.str85, %rdi
	xorq	%rax, %rax
	movq	%r14, %rdx
	callq	Perl_croak
.LBB60_31:                              # %if.end.i1054
	movq	(%r13), %rax
	movq	56(%rbx), %r15
	movq	%rbx, %r14
	movq	80(%rax), %rbx
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %r12
	movabsq	$PL_sv_undef, %rdi
	callq	Perl_newSVsv
	xorq	%r8, %r8
	movq	%r15, %rdi
	movq	%rbx, %rsi
	movq	%r12, %rdx
	movq	%rax, %rcx
	callq	Perl_hv_store
	movq	%r14, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	movq	96(%rsp), %rdx          # 8-byte Reload
	movq	%r13, %rcx
	callq	store_blessed
	jmp	.LBB60_337
.LBB60_114:                             # %if.then590.i
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_116
# BB#115:                               # %if.then597.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movb	%r14b, (%r15)
	jmp	.LBB60_120
.LBB60_99:                              # %if.then382.i
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_101
# BB#100:                               # %if.then389.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	$19, %rax
	movb	%al, (%r15)
	jmp	.LBB60_103
.LBB60_116:                             # %if.else601.i
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movb	%r14b, (%rax,%r15)
	jmp	.LBB60_120
.LBB60_101:                             # %if.else393.i
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	$19, %rcx
	movb	%cl, (%rax,%r15)
.LBB60_103:                             # %if.end446.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_104
# BB#107:                               # %if.else506.i
	movq	%r14, %rsi
	andq	$255, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_108
	jmp	.LBB60_338
.LBB60_104:                             # %if.then449.i
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_106
# BB#105:                               # %if.then456.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movb	%r14b, (%r15)
	jmp	.LBB60_108
.LBB60_334:                             # %if.else70
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r14
	movq	%r14, 176(%r12)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r12)
	movb	%bl, (%rax,%r15)
.LBB60_335:                             # %if.end810
	movq	%r12, %rbx
	jmp	.LBB60_336
.LBB60_274:                             # %if.else140
	movq	152(%rbx), %rdi
	movq	%rbx, %r15
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	movq	%r15, %rbx
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%rbx)
	movq	$128, %rcx
	movb	%cl, (%rax,%r14)
.LBB60_276:                             # %if.end194
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_287
# BB#277:                               # %if.then196
	movq	136(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 132(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_278
# BB#285:                               # %if.else271
	leaq	132(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_336
# BB#286:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_287:                             # %if.else279
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_288
# BB#293:                               # %if.else352
	leaq	136(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_336
# BB#294:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_278:                             # %if.then202
	movq	%rbx, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_280
# BB#279:                               # %if.then210
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_280:                             # %if.end252
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_282
# BB#281:                               # %if.then260
	movl	132(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_284
.LBB60_106:                             # %if.else460.i
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movb	%r14b, (%rax,%r15)
.LBB60_108:                             # %if.end514.i
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%r15, %rax
	cmpq	$3, %rax
	jne	.LBB60_120
# BB#109:                               # %if.then517.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_110
# BB#112:                               # %if.else577.i
	movq	48(%rsp), %rsi          # 8-byte Reload
	andq	$255, %rsi
	callq	PerlIO_putc
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_120
	jmp	.LBB60_338
.LBB60_46:
	movq	%r13, %r15
	jmp	.LBB60_338
.LBB60_81:
	movq	%r13, %r15
	jmp	.LBB60_338
.LBB60_52:
	movq	%r13, %r15
	jmp	.LBB60_338
.LBB60_110:                             # %if.then520.i
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_119
# BB#111:                               # %if.then527.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movq	48(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r15)
	jmp	.LBB60_120
.LBB60_326:                             # %if.else448
	movq	152(%r13), %rdi
	movq	160(%r13), %r15
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %r15
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%r15, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %r15
	movq	%r15, 176(%r13)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r13)
	movb	%bl, (%rax,%r12)
.LBB60_327:                             # %if.end742
	movq	%r13, %rbx
	jmp	.LBB60_328
.LBB60_305:                             # %if.else519
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	$128, %rcx
	movb	%cl, (%rax,%r15)
.LBB60_307:                             # %if.end573
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_318
# BB#308:                               # %if.then576
	movq	144(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 128(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_309
# BB#316:                               # %if.else652
	leaq	128(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_328
# BB#317:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_318:                             # %if.else660
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_319
# BB#324:                               # %if.else733
	leaq	144(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_328
# BB#325:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_288:                             # %if.then282
	movq	%rbx, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_290
# BB#289:                               # %if.then290
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_290:                             # %if.end332
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_292
# BB#291:                               # %if.then340
	movq	136(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_284
.LBB60_309:                             # %if.then583
	movq	%rbx, %r12
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_311
# BB#310:                               # %if.then591
	movq	152(%r12), %rdi
	movq	160(%r12), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r15, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r15
	movq	%r15, 176(%r12)
.LBB60_311:                             # %if.end633
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_313
# BB#312:                               # %if.then641
	movl	128(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_315
.LBB60_282:                             # %if.else263
	movl	132(%rsp), %eax
	jmp	.LBB60_283
.LBB60_75:
	movq	%r13, %r15
	jmp	.LBB60_338
.LBB60_119:                             # %if.else531.i
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movq	48(%rsp), %rcx          # 8-byte Reload
	movb	%cl, (%rax,%r15)
.LBB60_120:                             # %if.end656.i
	movq	%r14, %r12
	andq	$255, %r12
	movq	%r14, %rax
	andq	$4, %rax
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	$32, %r14
	je	.LBB60_144
# BB#121:                               # %if.then660.i
	testq	%r15, %rax
	je	.LBB60_139
# BB#122:                               # %if.then664.i
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_132
# BB#123:                               # %if.then666.i
	movq	200(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 188(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_124
# BB#131:                               # %if.else741.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	leaq	188(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_173
	jmp	.LBB60_338
.LBB60_144:                             # %if.else903.i
	testq	%r15, %rax
	je	.LBB60_162
# BB#145:                               # %if.then907.i
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_155
# BB#146:                               # %if.then910.i
	movq	216(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 184(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_147
# BB#154:                               # %if.else986.i
	leaq	184(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_167
	jmp	.LBB60_338
.LBB60_139:                             # %if.else831.i
	movq	216(%rbx), %rdi
	movq	200(%rsp), %r14
	testq	%rdi, %rdi
	je	.LBB60_140
# BB#143:                               # %if.else893.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	andq	$255, %r14
	movq	%r14, %rsi
	callq	PerlIO_putc
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_173
	jmp	.LBB60_338
.LBB60_132:                             # %if.else749.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_133
# BB#138:                               # %if.else822.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	leaq	200(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_173
	jmp	.LBB60_338
.LBB60_162:                             # %if.else1076.i
	movq	216(%rbx), %rdi
	movq	216(%rsp), %r14
	testq	%rdi, %rdi
	je	.LBB60_163
# BB#165:                               # %if.else1138.i
	andq	$255, %r14
	movq	%r14, %rsi
	callq	PerlIO_putc
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_167
	jmp	.LBB60_338
.LBB60_124:                             # %if.then672.i
	movq	%rbx, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_126
# BB#125:                               # %if.then680.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_126:                             # %if.end722.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_128
# BB#127:                               # %if.then730.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movl	188(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_130
.LBB60_155:                             # %if.else994.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_156
# BB#161:                               # %if.else1067.i
	leaq	216(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_167
	jmp	.LBB60_338
.LBB60_140:                             # %if.then836.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	cmpq	%rax, %r15
	jae	.LBB60_142
# BB#141:                               # %if.then843.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%rbx)
	movb	%r14b, (%r15)
	jmp	.LBB60_173
.LBB60_147:                             # %if.then917.i
	movq	%rbx, %r13
	movq	168(%r13), %rbx
	movq	176(%r13), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_149
# BB#148:                               # %if.then925.i
	movq	152(%r13), %rdi
	movq	160(%r13), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%r14, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r13)
	addq	%rax, %r14
	movq	%r14, 176(%r13)
.LBB60_149:                             # %if.end967.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_151
# BB#150:                               # %if.then975.i
	movl	184(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_153
.LBB60_133:                             # %if.then752.i
	movq	%rbx, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_135
# BB#134:                               # %if.then760.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_135:                             # %if.end802.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_137
# BB#136:                               # %if.then810.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movq	200(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_130
.LBB60_163:                             # %if.then1081.i
	movq	168(%rbx), %r13
	movq	176(%rbx), %rax
	cmpq	%rax, %r13
	jae	.LBB60_166
# BB#164:                               # %if.then1088.i
	leaq	1(%r13), %rax
	movq	%rax, 168(%rbx)
	movb	%r14b, (%r13)
	jmp	.LBB60_167
.LBB60_128:                             # %if.else733.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movl	188(%rsp), %eax
	jmp	.LBB60_129
.LBB60_142:                             # %if.else847.i
	movq	152(%rbx), %rdi
	movq	%rbx, %r13
	movq	160(%r13), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%rbx, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r13)
	movq	%r13, %rbx
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%rbx)
	movb	%r14b, (%rax,%r15)
	jmp	.LBB60_173
.LBB60_156:                             # %if.then997.i
	movq	%rbx, %r13
	movq	168(%r13), %rbx
	movq	176(%r13), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_158
# BB#157:                               # %if.then1005.i
	movq	152(%r13), %rdi
	movq	160(%r13), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%r14, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r13)
	addq	%rax, %r14
	movq	%r14, 176(%r13)
.LBB60_158:                             # %if.end1047.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_160
# BB#159:                               # %if.then1055.i
	movq	216(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_153
.LBB60_319:                             # %if.then663
	movq	%rbx, %r12
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_321
# BB#320:                               # %if.then671
	movq	152(%r12), %rdi
	movq	160(%r12), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r15, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r15
	movq	%r15, 176(%r12)
.LBB60_321:                             # %if.end713
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_323
# BB#322:                               # %if.then721
	movq	144(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_315
.LBB60_151:                             # %if.else978.i
	movl	184(%rsp), %eax
	jmp	.LBB60_152
.LBB60_137:                             # %if.else814.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movl	200(%rsp), %eax
.LBB60_129:                             # %if.end736.i
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB60_130:                             # %if.end736.i
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
	movq	%r15, %rbx
	jmp	.LBB60_173
.LBB60_166:                             # %if.else1092.i
	movq	152(%rbx), %rdi
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	160(%rax), %rbx
	subq	%rdi, %r13
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rax, 152(%rcx)
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, 160(%rcx)
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %rbx
	movq	40(%rsp), %rcx          # 8-byte Reload
	movq	%rbx, 176(%rcx)
	movq	40(%rsp), %rbx          # 8-byte Reload
	leaq	1(%rax,%r13), %rcx
	movq	%rcx, 168(%rbx)
	movb	%r14b, (%rax,%r13)
	jmp	.LBB60_167
.LBB60_292:                             # %if.else344
	movl	136(%rsp), %eax
.LBB60_283:                             # %if.end266
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB60_284:                             # %if.end266
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
	movq	%r15, %rbx
	jmp	.LBB60_336
.LBB60_313:                             # %if.else644
	movl	128(%rsp), %eax
	jmp	.LBB60_314
.LBB60_160:                             # %if.else1059.i
	movl	216(%rsp), %eax
.LBB60_152:                             # %if.end981.i
	movl	%eax, (%rbx)
	movq	168(%r13), %rbx
.LBB60_153:                             # %if.end981.i
	addq	$4, %rbx
	movq	%rbx, 168(%r13)
	movq	%r13, %rbx
.LBB60_167:                             # %if.end1147.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_168
# BB#172:                               # %if.else1207.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movq	216(%rsp), %rdx
	movq	80(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	216(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB60_338
	jmp	.LBB60_173
.LBB60_168:                             # %if.then1150.i
	movq	168(%rbx), %r14
	movq	216(%rsp), %rdx
	movq	176(%rbx), %rax
	leaq	(%r14,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_169
# BB#170:                               # %if.then1158.i
	movq	%r12, 104(%rsp)         # 8-byte Spill
	movq	152(%rbx), %rdi
	movq	%rbx, %r15
	movq	160(%r15), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %r14
	movq	%r14, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	movq	%r15, %rbx
	movq	216(%rsp), %rdx
	jmp	.LBB60_171
.LBB60_169:
	movq	%r12, 104(%rsp)         # 8-byte Spill
.LBB60_171:                             # %if.end1200.i
	movq	80(%rsp), %rsi          # 8-byte Reload
	movq	%r14, %rdi
	callq	memcpy
	movq	216(%rsp), %rax
	addq	%rax, 168(%rbx)
.LBB60_173:                             # %if.end1215.i
	movq	208(%rsp), %r12
	testq	$8, 104(%rsp)           # 8-byte Folded Reload
	je	.LBB60_193
# BB#174:                               # %if.then1219.i
	movq	%r12, 176(%rsp)
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_185
# BB#175:                               # %if.then1223.i
	movq	%r12, %rdi
	callq	Perl_my_htonl
	movl	%eax, 172(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %r13
	je	.LBB60_176
# BB#183:                               # %if.else1299.i
	leaq	172(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_198
# BB#184:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_193:                             # %if.else1389.i
	movq	%rbx, %r13
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB60_194
# BB#197:                               # %if.else1452.i
	andq	$255, %r12
	movq	%r12, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB60_198
	jmp	.LBB60_338
.LBB60_185:                             # %if.else1307.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %r13
	je	.LBB60_186
# BB#191:                               # %if.else1380.i
	leaq	176(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_198
# BB#192:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_176:                             # %if.then1230.i
	movq	%r13, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_178
# BB#177:                               # %if.then1238.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_178:                             # %if.end1280.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_180
# BB#179:                               # %if.then1288.i
	movl	172(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_182
.LBB60_194:                             # %if.then1395.i
	movq	168(%r13), %r15
	movq	176(%r13), %rax
	cmpq	%rax, %r15
	jae	.LBB60_196
# BB#195:                               # %if.then1402.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%r13)
	movb	%r12b, (%r15)
	jmp	.LBB60_198
.LBB60_186:                             # %if.then1310.i
	movq	%r13, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_188
# BB#187:                               # %if.then1318.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_188:                             # %if.end1360.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_190
# BB#189:                               # %if.then1368.i
	movq	176(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_182
.LBB60_180:                             # %if.else1291.i
	movl	172(%rsp), %eax
	jmp	.LBB60_181
.LBB60_196:                             # %if.else1406.i
	movq	152(%r13), %rdi
	movq	160(%r13), %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%r14, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r14
	movq	%r14, 176(%r13)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r13)
	movb	%r12b, (%rax,%r15)
	jmp	.LBB60_198
.LBB60_190:                             # %if.else1372.i
	movl	176(%rsp), %eax
.LBB60_181:                             # %if.end1294.i
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB60_182:                             # %if.end1294.i
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
	movq	%r15, %r13
.LBB60_198:                             # %if.end1461.i
	movq	208(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB60_203
# BB#199:                               # %if.then1463.i
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	je	.LBB60_200
# BB#211:                               # %if.else1523.i
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	208(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB60_203
# BB#212:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_200:                             # %if.then1466.i
	movq	168(%r13), %r14
	movq	176(%r13), %rax
	leaq	(%r14,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_202
# BB#201:                               # %if.then1474.i
	movq	152(%r13), %rdi
	movq	%r13, %r15
	movq	160(%r15), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %r14
	movq	%r14, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	movq	%r15, %r13
	movq	208(%rsp), %rdx
.LBB60_202:                             # %if.end1516.i
	movq	%r14, %rdi
	movq	16(%rsp), %rsi          # 8-byte Reload
	callq	memcpy
	movq	208(%rsp), %rax
	addq	%rax, 168(%r13)
.LBB60_203:                             # %if.end1531.i
	movq	104(%rsp), %rcx         # 8-byte Reload
	testq	$128, %rcx
	je	.LBB60_241
# BB#204:                               # %if.then1535.i
	movq	%r13, %rbx
	movq	112(%rsp), %rax         # 8-byte Reload
	leaq	-1(%rax), %r13
	movl	%r13d, 168(%rsp)
	testq	$16, %rcx
	je	.LBB60_226
# BB#205:                               # %if.then1540.i
	movl	80(%rbx), %eax
	testq	%rax, %rax
	je	.LBB60_218
# BB#206:                               # %if.then1543.i
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	movq	%r13, %rdi
	callq	Perl_my_htonl
	movl	%eax, 164(%rsp)
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %r13
	je	.LBB60_207
# BB#216:                               # %if.else1620.i
	leaq	164(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_231
# BB#217:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_226:                             # %if.else1709.i
	movq	%rbx, %r12
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB60_227
# BB#230:                               # %if.else1772.i
	andq	$255, %r13
	movq	%r13, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	andq	%r15, %rax
	cmpq	%r15, %rax
	movq	%r12, %r13
	jne	.LBB60_231
	jmp	.LBB60_338
.LBB60_218:                             # %if.else1628.i
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	movq	%rbx, %r13
	je	.LBB60_219
# BB#224:                               # %if.else1700.i
	leaq	168(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB60_231
# BB#225:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_207:                             # %if.then1551.i
	movq	%r13, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_209
# BB#208:                               # %if.then1559.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_209:                             # %if.end1601.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_213
# BB#210:                               # %if.then1609.i
	movl	164(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_215
.LBB60_227:                             # %if.then1715.i
	movq	168(%r12), %r15
	movq	176(%r12), %rax
	cmpq	%rax, %r15
	jae	.LBB60_229
# BB#228:                               # %if.then1722.i
	leaq	1(%r15), %rax
	movq	%rax, 168(%r12)
	movb	%r13b, (%r15)
	movq	%r12, %r13
	jmp	.LBB60_231
.LBB60_219:                             # %if.then1631.i
	movq	%r13, %r15
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_221
# BB#220:                               # %if.then1639.i
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB60_221:                             # %if.end1681.i
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB60_223
# BB#222:                               # %if.then1689.i
	movl	168(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB60_215
.LBB60_213:                             # %if.else1612.i
	movl	164(%rsp), %eax
	jmp	.LBB60_214
.LBB60_229:                             # %if.else1726.i
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r14
	movq	%r14, 176(%r12)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r12)
	movb	%r13b, (%rax,%r15)
	movq	%r12, %r13
	jmp	.LBB60_231
.LBB60_323:                             # %if.else725
	movl	144(%rsp), %eax
.LBB60_314:                             # %if.end647
	movl	%eax, (%rbx)
	movq	168(%r12), %rbx
.LBB60_315:                             # %if.end647
	addq	$4, %rbx
	movq	%rbx, 168(%r12)
	movq	%r12, %rbx
.LBB60_328:                             # %if.end742
	movq	216(%rbx), %rdi
	testq	%rdi, %rdi
	je	.LBB60_329
# BB#332:                               # %if.else802
	movq	144(%rsp), %rdx
	movq	%r14, %rsi
	callq	Perl_PerlIO_write
	movq	144(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB60_336
# BB#333:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_329:                             # %if.then745
	movq	168(%rbx), %r15
	movq	144(%rsp), %rdx
	movq	176(%rbx), %rax
	leaq	(%r15,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB60_331
# BB#330:                               # %if.then753
	movq	152(%rbx), %rdi
	movq	%rbx, %r12
	movq	160(%r12), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%r12)
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	%r12, %rbx
	movq	144(%rsp), %rdx
.LBB60_331:                             # %if.end795
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	memcpy
	movq	144(%rsp), %rax
	addq	%rax, 168(%rbx)
.LBB60_336:                             # %if.end810
	movq	$32, %rcx
	movq	96(%rsp), %rdx          # 8-byte Reload
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	%rbx, %rdi
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	*sv_store(,%rdx,8)
.LBB60_337:                             # %return
	movq	%rax, %r15
.LBB60_338:                             # %return
	movq	%r15, %rax
	addq	$224, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB60_223:                             # %if.else1692.i
	movl	168(%rsp), %eax
.LBB60_214:                             # %if.end1615.i
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB60_215:                             # %if.end1615.i
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
	movq	%r15, %r13
.LBB60_231:                             # %for.cond1782.i.preheader
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	movq	88(%rsp), %r12          # 8-byte Reload
	jl	.LBB60_243
# BB#232:                               # %for.body1785.i.lr.ph
	movq	$2, %r15
	.align	16, 0x90
.LBB60_233:                             # %for.body1785.i
                                        # =>This Inner Loop Header: Depth=1
	movq	-8(%r12,%r15,8), %rdi
	callq	Perl_my_htonl
	movq	%rax, 152(%rsp)
	movq	216(%r13), %rdi
	testq	%rdi, %rdi
	jne	.LBB60_247
# BB#234:                               # %if.then1792.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	168(%r13), %rbx
	movq	176(%r13), %rax
	leaq	4(%rbx), %rcx
	movq	%r12, %r14
	cmpq	%rax, %rcx
	jbe	.LBB60_236
# BB#235:                               # %if.then1800.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	152(%r13), %rdi
	movq	160(%r13), %r12
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r13)
	movq	%r12, 160(%r13)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r13)
	addq	%rax, %r12
	movq	%r12, 176(%r13)
.LBB60_236:                             # %if.end1842.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r14, %r12
	jne	.LBB60_238
# BB#237:                               # %if.then1850.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	152(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB60_239
.LBB60_247:                             # %if.else1862.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	$8, %rdx
	leaq	152(%rsp), %rsi
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB60_240
	jmp	.LBB60_248
.LBB60_238:                             # %if.else1854.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movl	152(%rsp), %eax
	movl	%eax, (%rbx)
	movq	168(%r13), %rbx
.LBB60_239:                             # %if.end1857.i
                                        #   in Loop: Header=BB60_233 Depth=1
	addq	$4, %rbx
	movq	%rbx, 168(%r13)
.LBB60_240:                             # %for.inc1870.i
                                        #   in Loop: Header=BB60_233 Depth=1
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$32, %rcx
	movq	%r15, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	incq	%r15
	cmpq	%rax, %rdx
	jl	.LBB60_233
.LBB60_241:                             # %if.end1873.i
	movq	$32, %rcx
	movq	112(%rsp), %rax         # 8-byte Reload
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	$2, %rax
	jl	.LBB60_243
# BB#242:                               # %if.then1876.i
	movq	120(%rsp), %rax         # 8-byte Reload
	movq	(%rax), %rax
	movq	$0, 8(%rax)
.LBB60_243:                             # %if.end1879.i
	movq	120(%rsp), %rbx         # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_av_undef
	movq	%rbx, %rdi
	callq	Perl_sv_free
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	movq	72(%rsp), %rax          # 8-byte Reload
	andq	%r14, %rax
	cmpq	$3, %rax
	jne	.LBB60_254
# BB#244:                               # %if.then1882.i
	movq	%r13, %r15
	movq	64(%rsp), %r13          # 8-byte Reload
	movq	%r13, %rdi
	movq	24(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rsi
	callq	Perl_mg_find
	movq	%rax, %r12
	testq	%r12, %r12
	jne	.LBB60_253
# BB#245:                               # %if.then1887.i
	movzbl	16(%r13), %eax
	cmpq	$11, %rax
	movq	$1, %rcx
	movl	%ecx, 112(%r15)
	jne	.LBB60_249
# BB#246:
	movabsq	$.L.str77, %rdx
	jmp	.LBB60_252
.LBB60_248:
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	jmp	.LBB60_338
.LBB60_249:                             # %cond.false1897.i
	cmpq	$10, %rax
	je	.LBB60_250
# BB#251:                               # %cond.false1897.i
	movabsq	$.L.str79, %rdx
	jmp	.LBB60_252
.LBB60_250:
	movabsq	$.L.str78, %rdx
.LBB60_252:                             # %cond.false1897.i
	movq	%rbx, %rsi
	movabsq	$.L.str90, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB60_253:                             # %if.end1903.i
	movq	24(%r12), %rsi
	movq	%r15, %rdi
	callq	store
	movq	%rax, %r15
	testq	%r14, %r15
	jne	.LBB60_338
.LBB60_254:                             # %if.end1908.i
	xorq	%r15, %r15
	jmp	.LBB60_338
.Ltmp735:
	.size	store_blessed, .Ltmp735-store_blessed
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI60_0:
	.quad	.LBB60_11
	.quad	.LBB60_3
	.quad	.LBB60_5
	.quad	.LBB60_6

	.text
	.align	16, 0x90
	.type	store_ref,@function
store_ref:                              # @store_ref
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp742:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp743:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp744:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp745:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp746:
	.cfi_def_cfa_offset 48
.Ltmp747:
	.cfi_offset %rbx, -48
.Ltmp748:
	.cfi_offset %r12, -40
.Ltmp749:
	.cfi_offset %r13, -32
.Ltmp750:
	.cfi_offset %r14, -24
.Ltmp751:
	.cfi_offset %r15, -16
	movq	(%rsi), %rax
	movq	(%rax), %r14
	movzbl	17(%r14), %eax
	movq	%rdi, %r12
	movabsq	$35184372088832, %r13   # imm = 0x200000000000
	testq	$16, %rax
	je	.LBB61_10
# BB#1:                                 # %if.then
	movq	(%r14), %rax
	movq	48(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB61_10
# BB#2:                                 # %if.then
	movq	PL_amagic_generation(%rip), %rax
	testq	%rax, %rax
	je	.LBB61_10
# BB#3:                                 # %land.lhs.true4
	callq	Perl_Gv_AMupdate
	testq	$255, %rax
	je	.LBB61_10
# BB#4:                                 # %if.then6
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB61_5
# BB#9:                                 # %if.else44
	movq	$20, %rsi
	jmp	.LBB61_14
.LBB61_10:                              # %if.else52
	movq	216(%r12), %rdi
	testq	%rdi, %rdi
	je	.LBB61_11
# BB#13:                                # %if.else181
	movq	$4, %rsi
.LBB61_14:                              # %if.else181
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB61_8
# BB#15:                                # %return
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB61_11:                              # %if.then55
	movq	168(%r12), %r15
	movq	176(%r12), %rax
	cmpq	%rax, %r15
	jae	.LBB61_16
# BB#12:                                # %if.then62
	leaq	1(%r15), %rax
	movq	%rax, 168(%r12)
	movq	$4, %rax
	movb	%al, (%r15)
	jmp	.LBB61_8
.LBB61_16:                              # %if.else66
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%r13, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r12)
	movq	$4, %rcx
	movb	%cl, (%rax,%r15)
	jmp	.LBB61_8
.LBB61_5:                               # %if.then8
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	cmpq	%rax, %rbx
	jae	.LBB61_7
# BB#6:                                 # %if.then11
	leaq	1(%rbx), %rax
	movq	%rax, 168(%r12)
	movq	$20, %rax
	movb	%al, (%rbx)
	jmp	.LBB61_8
.LBB61_7:                               # %if.else
	movq	152(%r12), %rdi
	movq	160(%r12), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r13, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r15, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %r15
	movq	%r15, 176(%r12)
	leaq	1(%rax,%rbx), %rcx
	movq	%rcx, 168(%r12)
	movq	$20, %rcx
	movb	%cl, (%rax,%rbx)
.LBB61_8:                               # %if.end189
	movq	%r12, %rdi
	movq	%r14, %rsi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	store  # TAILCALL
.Ltmp752:
	.size	store_ref, .Ltmp752-store_ref
	.cfi_endproc

	.align	16, 0x90
	.type	store_scalar,@function
store_scalar:                           # @store_scalar
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp759:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp760:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp761:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp762:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp763:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp764:
	.cfi_def_cfa_offset 128
.Ltmp765:
	.cfi_offset %rbx, -48
.Ltmp766:
	.cfi_offset %r12, -40
.Ltmp767:
	.cfi_offset %r13, -32
.Ltmp768:
	.cfi_offset %r14, -24
.Ltmp769:
	.cfi_offset %r15, -16
	movq	%rsi, %rbx
	movq	16(%rbx), %r14
	movq	%rdi, %r15
	movabsq	$35184372088832, %rdx   # imm = 0x200000000000
	testq	$118423552, %r14        # imm = 0x70F0000
	je	.LBB62_1
# BB#13:                                # %if.end114
	movabsq	$35197256990720, %r13   # imm = 0x200300000000
	movq	%r14, %rax
	andq	$8781824, %rax          # imm = 0x860000
	cmpq	$8781824, %rax          # imm = 0x860000
	jne	.LBB62_28
# BB#14:                                # %if.then118
	movabsq	$PL_sv_yes, %rax
	cmpq	%rax, %rbx
	je	.LBB62_15
# BB#20:                                # %if.else189
	movabsq	$PL_sv_no, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_26
# BB#21:                                # %if.then192
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_22
# BB#25:                                # %if.else252
	movq	$16, %rsi
	jmp	.LBB62_12
.LBB62_1:                               # %if.then
	movq	216(%r15), %rdi
	movabsq	$PL_sv_undef, %rax
	cmpq	%rax, %rbx
	je	.LBB62_2
# BB#7:                                 # %if.else45
	testq	%rdi, %rdi
	je	.LBB62_8
# BB#11:                                # %if.else105
	movq	$5, %rsi
	jmp	.LBB62_12
.LBB62_28:                              # %if.else269
	testq	$262144, %r14           # imm = 0x40000
	jne	.LBB62_98
# BB#29:                                # %if.else273
	movabsq	$35214436859904, %r12   # imm = 0x200700000000
	testq	$65536, %r14            # imm = 0x10000
	jne	.LBB62_30
# BB#75:                                # %if.else734
	testq	$131072, %r14           # imm = 0x20000
	je	.LBB62_97
# BB#76:                                # %if.then737
	testq	$16777216, %r14         # imm = 0x1000000
	movq	%r14, %rax
	jne	.LBB62_79
# BB#77:                                # %if.then737
	movq	%r14, %rax
	andq	$393216, %rax           # imm = 0x60000
	movq	%r14, %rax
	je	.LBB62_79
# BB#78:                                # %cond.false756
	movq	%rbx, %rdi
	callq	Perl_sv_2iv
	movq	16(%rbx), %rax
.LBB62_79:                              # %if.end760
	movabsq	$2147549184, %rcx       # imm = 0x80010000
	andq	%rax, %rcx
	cmpq	$65536, %rcx            # imm = 0x10000
	jne	.LBB62_82
# BB#80:                                # %if.then765
	testq	$65536, %rax            # imm = 0x10000
	je	.LBB62_81
.LBB62_30:                              # %cond.true280
	movq	(%rbx), %rax
	movq	24(%rax), %rdi
.LBB62_31:                              # %integer
	movq	%rdi, 72(%rsp)
	movabsq	$2147483648, %rax       # imm = 0x80000000
	testq	%rax, %r14
	je	.LBB62_37
# BB#32:                                # %land.lhs.true
	movzbl	18(%rbx), %eax
	testq	$1, %rax
	je	.LBB62_34
# BB#33:                                # %cond.true291
	movq	(%rbx), %rax
	movq	24(%rax), %rax
	jmp	.LBB62_35
.LBB62_2:                               # %if.then1
	testq	%rdi, %rdi
	je	.LBB62_3
# BB#6:                                 # %if.else37
	movq	$14, %rsi
	jmp	.LBB62_12
.LBB62_15:                              # %if.then121
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_16
# BB#19:                                # %if.else181
	movq	$15, %rsi
.LBB62_12:                              # %if.else105
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_191
	jmp	.LBB62_192
.LBB62_8:                               # %if.then48
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_10
# BB#9:                                 # %if.then55
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$5, %rax
	movb	%al, (%r14)
	jmp	.LBB62_191
.LBB62_3:                               # %if.then3
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_5
# BB#4:                                 # %if.then6
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$14, %rax
	movb	%al, (%r14)
	jmp	.LBB62_191
.LBB62_16:                              # %if.then124
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_18
# BB#17:                                # %if.then131
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$15, %rax
	movb	%al, (%r14)
	jmp	.LBB62_191
.LBB62_10:                              # %if.else59
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$5, %rcx
	movb	%cl, (%rax,%r14)
	jmp	.LBB62_191
.LBB62_22:                              # %if.then195
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_24
# BB#23:                                # %if.then202
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$16, %rax
	movb	%al, (%r14)
	jmp	.LBB62_191
.LBB62_5:                               # %if.else
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$14, %rcx
	movb	%cl, (%rax,%r14)
	jmp	.LBB62_191
.LBB62_18:                              # %if.else135
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$15, %rcx
	movb	%cl, (%rax,%r14)
	jmp	.LBB62_191
.LBB62_34:                              # %cond.false293
	movq	%rbx, %rdi
	callq	Perl_sv_2uv
.LBB62_35:                              # %cond.end295
	cmpq	$2147483647, %rax       # imm = 0x7FFFFFFF
	ja	.LBB62_98
# BB#36:                                # %cond.end295.if.end300_crit_edge
	movq	72(%rsp), %rdi
.LBB62_37:                              # %if.end300
	leaq	128(%rdi), %r14
	cmpq	$255, %r14
	ja	.LBB62_48
# BB#38:                                # %if.then306
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_39
# BB#42:                                # %if.else369
	movq	$8, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_43
	jmp	.LBB62_192
.LBB62_48:                              # %if.else445
	movl	80(%r15), %eax
	testq	%rax, %rax
	je	.LBB62_63
# BB#49:                                # %if.then447
	callq	Perl_my_htonl
	movq	%rax, 56(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_50
# BB#53:                                # %if.else509
	movq	$9, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_54
	jmp	.LBB62_192
.LBB62_97:                              # %if.else924
	testq	$117440512, %r14        # imm = 0x7000000
	jne	.LBB62_98
# BB#190:                               # %if.else1976
	movq	$1, %rax
	movl	%eax, 112(%r15)
	xorq	%rsi, %rsi
	movq	%rbx, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$.L.str81, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%rbx, %rdx
	callq	Perl_croak
	jmp	.LBB62_191
.LBB62_24:                              # %if.else206
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$16, %rcx
	movb	%cl, (%rax,%r14)
	jmp	.LBB62_191
.LBB62_82:                              # %if.end776
	testq	$131072, %rax           # imm = 0x20000
	je	.LBB62_84
# BB#83:                                # %cond.true780
	movq	(%rbx), %rax
	movq	32(%rax), %rax
	jmp	.LBB62_85
.LBB62_39:                              # %if.then312
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	cmpq	%rax, %r12
	jae	.LBB62_41
# BB#40:                                # %if.then319
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$8, %rax
	movb	%al, (%r12)
	jmp	.LBB62_43
.LBB62_63:                              # %if.else597
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_64
# BB#67:                                # %if.else657
	movq	$6, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_68
	jmp	.LBB62_192
.LBB62_50:                              # %if.then452
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_52
# BB#51:                                # %if.then459
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$9, %rax
	movb	%al, (%r14)
	jmp	.LBB62_54
.LBB62_41:                              # %if.else323
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$8, %rcx
	movb	%cl, (%rax,%r12)
.LBB62_43:                              # %if.end376
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_44
# BB#47:                                # %if.else436
	andq	$255, %r14
	movq	%r14, %rsi
	jmp	.LBB62_12
.LBB62_44:                              # %if.then379
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	cmpq	%rax, %r12
	jae	.LBB62_46
# BB#45:                                # %if.then386
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movb	%r14b, (%r12)
	jmp	.LBB62_191
.LBB62_81:                              # %cond.false772
	movq	%rbx, %rdi
	callq	Perl_sv_2iv
	movq	%rax, %rdi
	jmp	.LBB62_31
.LBB62_84:                              # %cond.false782
	movq	%rbx, %rdi
	callq	Perl_sv_2nv
.LBB62_85:                              # %cond.end784
	movq	%rax, 48(%rsp)
	movl	80(%r15), %eax
	testq	%rax, %rax
	je	.LBB62_86
.LBB62_98:                              # %string_readlen
	movq	16(%rbx), %r14
.LBB62_26:                              # %if.else260
	testq	$262144, %r14           # imm = 0x40000
	je	.LBB62_99
# BB#27:                                # %cond.true
	movq	(%rbx), %rax
	movq	8(%rax), %r12
	movq	%r12, 64(%rsp)
	movq	(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	jmp	.LBB62_100
.LBB62_99:                              # %cond.false938
	leaq	64(%rsp), %rsi
	movq	$2, %rdx
	movq	%rbx, %rdi
	callq	Perl_sv_2pv_flags
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	64(%rsp), %r12
	movq	16(%rbx), %r14
.LBB62_100:                             # %string
	movq	%r12, 40(%rsp)
	testq	$536870912, %r14        # imm = 0x20000000
	je	.LBB62_148
# BB#101:                               # %if.then945
	cmpq	$255, %r12
	jg	.LBB62_120
# BB#102:                               # %if.then948
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_103
# BB#106:                               # %if.else1010
	movq	$23, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_107
	jmp	.LBB62_192
.LBB62_148:                             # %if.else1459
	cmpq	$255, %r12
	jg	.LBB62_163
# BB#149:                               # %if.then1462
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_150
# BB#153:                               # %if.else1525
	movq	$10, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_154
	jmp	.LBB62_192
.LBB62_120:                             # %if.else1156
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_121
# BB#124:                               # %if.else1216
	movq	$24, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_125
	jmp	.LBB62_192
.LBB62_163:                             # %if.else1671
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_164
# BB#167:                               # %if.else1731
	movq	$1, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_168
	jmp	.LBB62_192
.LBB62_103:                             # %if.then953
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_105
# BB#104:                               # %if.then960
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$23, %rax
	movb	%al, (%r14)
	jmp	.LBB62_107
.LBB62_121:                             # %if.then1159
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_123
# BB#122:                               # %if.then1166
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$24, %rax
	movb	%al, (%r14)
	jmp	.LBB62_125
.LBB62_150:                             # %if.then1468
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_152
# BB#151:                               # %if.then1475
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$10, %rax
	movb	%al, (%r14)
	jmp	.LBB62_154
.LBB62_164:                             # %if.then1674
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_166
# BB#165:                               # %if.then1681
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$1, %rax
	movb	%al, (%r14)
	jmp	.LBB62_168
.LBB62_105:                             # %if.else964
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$23, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_107:                             # %if.end1017
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_108
# BB#111:                               # %if.else1077
	andq	$255, %r12
	movq	%r12, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_112
	jmp	.LBB62_192
.LBB62_108:                             # %if.then1020
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_110
# BB#109:                               # %if.then1027
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movb	%r12b, (%r14)
	jmp	.LBB62_112
.LBB62_123:                             # %if.else1170
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$24, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_125:                             # %if.end1223
	movl	80(%r15), %eax
	testq	%rax, %rax
	je	.LBB62_138
# BB#126:                               # %if.then1226
	movq	40(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 36(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_127
# BB#136:                               # %if.else1301
	leaq	36(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB62_134
# BB#137:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_138:                             # %if.else1309
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_139
# BB#144:                               # %if.else1382
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB62_134
# BB#145:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_127:                             # %if.then1232
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_129
# BB#128:                               # %if.then1240
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB62_129:                             # %if.end1282
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_131
# BB#130:                               # %if.then1290
	movl	36(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB62_133
.LBB62_110:                             # %if.else1031
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movb	%r12b, (%rax,%r14)
.LBB62_112:                             # %if.end1085
	movq	40(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB62_191
# BB#113:                               # %if.then1087
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_114
# BB#118:                               # %if.else1147
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	40(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB62_191
# BB#119:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_152:                             # %if.else1479
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$10, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_154:                             # %if.end1532
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_155
# BB#158:                               # %if.else1592
	andq	$255, %r12
	movq	%r12, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_159
	jmp	.LBB62_192
.LBB62_155:                             # %if.then1535
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_157
# BB#156:                               # %if.then1542
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movb	%r12b, (%r14)
	jmp	.LBB62_159
.LBB62_166:                             # %if.else1685
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$1, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_168:                             # %if.end1738
	movl	80(%r15), %eax
	testq	%rax, %rax
	je	.LBB62_182
# BB#169:                               # %if.then1741
	movq	40(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 32(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_170
# BB#180:                               # %if.else1817
	leaq	32(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB62_177
# BB#181:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_182:                             # %if.else1825
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_183
# BB#188:                               # %if.else1898
	leaq	40(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB62_177
# BB#189:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_139:                             # %if.then1312
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_141
# BB#140:                               # %if.then1320
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB62_141:                             # %if.end1362
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_143
# BB#142:                               # %if.then1370
	movq	40(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB62_133
.LBB62_170:                             # %if.then1748
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_172
# BB#171:                               # %if.then1756
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB62_172:                             # %if.end1798
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_174
# BB#173:                               # %if.then1806
	movl	32(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB62_176
.LBB62_131:                             # %if.else1293
	movl	36(%rsp), %eax
	jmp	.LBB62_132
.LBB62_157:                             # %if.else1546
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movb	%r12b, (%rax,%r14)
.LBB62_159:                             # %if.end1600
	movq	40(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB62_191
# BB#160:                               # %if.then1602
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_114
# BB#161:                               # %if.else1662
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	40(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB62_191
# BB#162:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_114:                             # %if.then1090
	movq	168(%r15), %r12
	jmp	.LBB62_115
.LBB62_183:                             # %if.then1828
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_185
# BB#184:                               # %if.then1836
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB62_185:                             # %if.end1878
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_187
# BB#186:                               # %if.then1886
	movq	40(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB62_176
.LBB62_143:                             # %if.else1374
	movl	40(%rsp), %eax
.LBB62_132:                             # %if.end1296
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB62_133:                             # %if.end1296
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
.LBB62_134:                             # %if.end1390
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_135
# BB#146:                               # %if.else1450
	movq	40(%rsp), %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	40(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB62_191
# BB#147:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_174:                             # %if.else1809
	movl	32(%rsp), %eax
	jmp	.LBB62_175
.LBB62_86:                              # %if.end789
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_87
# BB#90:                                # %if.else849
	movq	$7, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB62_91
	jmp	.LBB62_192
.LBB62_46:                              # %if.else390
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movb	%r14b, (%rax,%r12)
	jmp	.LBB62_191
.LBB62_64:                              # %if.then600
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_66
# BB#65:                                # %if.then607
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$6, %rax
	movb	%al, (%r14)
	jmp	.LBB62_68
.LBB62_187:                             # %if.else1890
	movl	40(%rsp), %eax
.LBB62_175:                             # %if.end1812
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB62_176:                             # %if.end1812
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
.LBB62_177:                             # %if.end1906
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_135
# BB#178:                               # %if.else1966
	movq	40(%rsp), %rdx
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	40(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB62_191
# BB#179:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_135:                             # %if.then1393
	movq	168(%r15), %r12
	movq	40(%rsp), %rdx
.LBB62_115:                             # %if.then1090
	movq	176(%r15), %rax
	leaq	(%r12,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_117
# BB#116:                               # %if.then1098
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %r12
	movq	%r12, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	movq	40(%rsp), %rdx
.LBB62_117:                             # %if.end1140
	movq	%r12, %rdi
	movq	24(%rsp), %rsi          # 8-byte Reload
	callq	memcpy
	movq	40(%rsp), %rax
	addq	%rax, 168(%r15)
.LBB62_191:                             # %if.end1982
	xorq	%rax, %rax
.LBB62_192:                             # %return
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB62_52:                              # %if.else463
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$9, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_54:                              # %if.end516
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_55
# BB#61:                                # %if.else589
	leaq	56(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB62_191
# BB#62:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_55:                              # %if.then519
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_57
# BB#56:                                # %if.then527
	movq	152(%r15), %rdi
	movq	160(%r15), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	addq	%r13, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r14, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r14
	movq	%r14, 176(%r15)
.LBB62_57:                              # %if.end569
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB62_59
# BB#58:                                # %if.then577
	movq	56(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB62_60
.LBB62_59:                              # %if.else581
	movl	56(%rsp), %eax
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB62_60:                              # %if.end584
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
	jmp	.LBB62_191
.LBB62_66:                              # %if.else611
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$6, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_68:                              # %if.end664
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_69
# BB#73:                                # %if.else724
	leaq	72(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB62_191
# BB#74:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_69:                              # %if.then667
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	leaq	8(%r14), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_71
# BB#70:                                # %if.then675
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%r12, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %r14
	movq	%r14, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
.LBB62_71:                              # %if.end717
	movq	72(%rsp), %rax
	jmp	.LBB62_72
.LBB62_87:                              # %if.then792
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB62_89
# BB#88:                                # %if.then799
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$7, %rax
	movb	%al, (%r14)
	jmp	.LBB62_91
.LBB62_89:                              # %if.else803
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$7, %rcx
	movb	%cl, (%rax,%r14)
.LBB62_91:                              # %if.end856
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB62_92
# BB#95:                                # %if.else916
	leaq	48(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB62_191
# BB#96:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB62_192
.LBB62_92:                              # %if.then859
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	leaq	8(%r14), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB62_94
# BB#93:                                # %if.then867
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%r12, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %r14
	movq	%r14, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
.LBB62_94:                              # %if.end909
	movq	48(%rsp), %rax
.LBB62_72:                              # %if.end1982
	movq	%rax, (%r14)
	addq	$8, 168(%r15)
	jmp	.LBB62_191
.Ltmp770:
	.size	store_scalar, .Ltmp770-store_scalar
	.cfi_endproc

	.align	16, 0x90
	.type	store_array,@function
store_array:                            # @store_array
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp777:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp778:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp779:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp780:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp781:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp782:
	.cfi_def_cfa_offset 80
.Ltmp783:
	.cfi_offset %rbx, -48
.Ltmp784:
	.cfi_offset %r12, -40
.Ltmp785:
	.cfi_offset %r13, -32
.Ltmp786:
	.cfi_offset %r14, -24
.Ltmp787:
	.cfi_offset %r15, -16
	movq	%rsi, %r12
	movq	%r12, 8(%rsp)           # 8-byte Spill
	movq	%rdi, %r15
	movq	%r12, %rdi
	callq	Perl_av_len
	incq	%rax
	movq	%rax, 24(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB63_1
# BB#4:                                 # %if.else34
	movq	$2, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB63_5
	jmp	.LBB63_35
.LBB63_1:                               # %if.then
	movq	%r12, %r14
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	cmpq	%rax, %r12
	jae	.LBB63_3
# BB#2:                                 # %if.then2
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$2, %rax
	movb	%al, (%r12)
	movq	%r14, %r12
	jmp	.LBB63_5
.LBB63_3:                               # %if.else
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$2, %rcx
	movb	%cl, (%rax,%r12)
	movq	%r14, %r12
.LBB63_5:                               # %if.end41
	movl	80(%r15), %eax
	movabsq	$35197256990720, %r13   # imm = 0x200300000000
	testq	%rax, %rax
	je	.LBB63_22
# BB#6:                                 # %if.then43
	movq	24(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 20(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB63_7
# BB#20:                                # %if.else118
	leaq	20(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB63_14
# BB#21:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB63_35
.LBB63_22:                              # %if.else126
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB63_23
# BB#28:                                # %if.else199
	leaq	24(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB63_14
# BB#29:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB63_35
.LBB63_7:                               # %if.then49
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	movq	%r12, %r14
	cmpq	%rax, %rcx
	jbe	.LBB63_9
# BB#8:                                 # %if.then57
	movq	152(%r15), %rdi
	movq	160(%r15), %r12
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	addq	%r13, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r12, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r12
	movq	%r12, 176(%r15)
.LBB63_9:                               # %if.end99
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r14, %r12
	jne	.LBB63_11
# BB#10:                                # %if.then107
	movl	20(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB63_13
.LBB63_23:                              # %if.then129
	movq	%r12, %r14
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB63_25
# BB#24:                                # %if.then137
	movq	152(%r15), %rdi
	movq	160(%r15), %r12
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	addq	%r13, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r12, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r12
	movq	%r12, 176(%r15)
.LBB63_25:                              # %if.end179
	movq	%r14, %r12
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB63_27
# BB#26:                                # %if.then187
	movq	24(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB63_13
.LBB63_11:                              # %if.else110
	movl	20(%rsp), %eax
	jmp	.LBB63_12
.LBB63_27:                              # %if.else191
	movl	24(%rsp), %eax
.LBB63_12:                              # %if.end113
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB63_13:                              # %if.end113
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
.LBB63_14:                              # %for.cond.preheader
	xorq	%rax, %rax
	cmpq	$0, 24(%rsp)
	jle	.LBB63_35
# BB#15:                                # %for.body.lr.ph
	xorq	%rbx, %rbx
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB63_16:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	xorq	%rdx, %rdx
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	Perl_av_fetch
	testq	%rax, %rax
	jne	.LBB63_33
# BB#17:                                # %if.then212
                                        #   in Loop: Header=BB63_16 Depth=1
	incq	64(%r15)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	jne	.LBB63_31
# BB#18:                                # %if.then215
                                        #   in Loop: Header=BB63_16 Depth=1
	movq	%r14, %r13
	movq	168(%r15), %r14
	movq	176(%r15), %rax
	cmpq	%rax, %r14
	jae	.LBB63_30
# BB#19:                                # %if.then222
                                        #   in Loop: Header=BB63_16 Depth=1
	leaq	1(%r14), %rax
	movq	%rax, 168(%r15)
	movq	$14, %rax
	movb	%al, (%r14)
	movq	%r13, %r14
	jmp	.LBB63_34
	.align	16, 0x90
.LBB63_33:                              # %if.end280
                                        #   in Loop: Header=BB63_16 Depth=1
	movq	(%rax), %rsi
	movq	%r15, %rdi
	callq	store
	testq	%r14, %rax
	je	.LBB63_34
	jmp	.LBB63_35
	.align	16, 0x90
.LBB63_31:                              # %if.else272
                                        #   in Loop: Header=BB63_16 Depth=1
	movq	$14, %rsi
	callq	PerlIO_putc
	andq	%r14, %rax
	cmpq	%r14, %rax
	jne	.LBB63_34
	jmp	.LBB63_32
.LBB63_30:                              # %if.else226
                                        #   in Loop: Header=BB63_16 Depth=1
	movq	152(%r15), %rdi
	movq	160(%r15), %r12
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %r12
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r12, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %r12
	movq	%r12, 176(%r15)
	movq	8(%rsp), %r12           # 8-byte Reload
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r15)
	movq	$14, %rcx
	movb	%cl, (%rax,%r14)
	movq	%r13, %r14
	.align	16, 0x90
.LBB63_34:                              # %for.inc
                                        #   in Loop: Header=BB63_16 Depth=1
	movq	24(%rsp), %rcx
	incq	%rbx
	xorq	%rax, %rax
	cmpq	%rcx, %rbx
	jl	.LBB63_16
	jmp	.LBB63_35
.LBB63_32:
	movq	%r14, %rax
.LBB63_35:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp788:
	.size	store_array, .Ltmp788-store_array
	.cfi_endproc

	.align	16, 0x90
	.type	store_hash,@function
store_hash:                             # @store_hash
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp795:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp796:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp797:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp798:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp799:
	.cfi_def_cfa_offset 48
	subq	$144, %rsp
.Ltmp800:
	.cfi_def_cfa_offset 192
.Ltmp801:
	.cfi_offset %rbx, -48
.Ltmp802:
	.cfi_offset %r12, -40
.Ltmp803:
	.cfi_offset %r13, -32
.Ltmp804:
	.cfi_offset %r14, -24
.Ltmp805:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	(%r14), %rax
	movq	24(%rax), %rax
	movq	%rax, 136(%rsp)
	movq	16(%r14), %rax
	movq	%rdi, %r12
	testq	$8388608, %rax          # imm = 0x800000
	je	.LBB64_3
# BB#1:                                 # %lor.end.thread
	movq	$23, %rcx
	movq	216(%r12), %rdi
	shrq	%cl, %rax
	andq	$1, %rax
	leaq	216(%r12), %rcx
	movq	%rcx, 80(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	movq	%rax, 64(%rsp)          # 8-byte Spill
	jne	.LBB64_2
	jmp	.LBB64_5
.LBB64_3:                               # %lor.end
	movabsq	$2147483648, %rdx       # imm = 0x80000000
	movq	%rax, %rbx
	andq	%rdx, %rbx
	movq	$31, %rcx
	shrq	%cl, %rbx
	movq	$23, %rcx
	movq	%rax, %rsi
	shrq	%cl, %rsi
	movq	216(%r12), %rdi
	andq	$1, %rsi
	movq	%rsi, 64(%rsp)          # 8-byte Spill
	andq	%rdx, %rax
	leaq	216(%r12), %rax
	movq	%rax, 80(%rsp)          # 8-byte Spill
	je	.LBB64_15
# BB#4:                                 # %if.then10
	testq	%rdi, %rdi
	je	.LBB64_5
.LBB64_2:                               # %if.else49
	movq	$25, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB64_9
	jmp	.LBB64_162
.LBB64_5:                               # %if.then12
	movq	%r14, %r15
	movq	168(%r12), %r14
	movq	176(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB64_7
# BB#6:                                 # %if.then15
	leaq	1(%r14), %rax
	movq	%rax, 168(%r12)
	movq	$25, %rax
	movb	%al, (%r14)
	jmp	.LBB64_8
.LBB64_7:                               # %if.else18
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r12)
	movq	$25, %rcx
	movb	%cl, (%rax,%r14)
.LBB64_8:                               # %if.end56
	movq	%r15, %r14
.LBB64_9:                               # %if.end56
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB64_10
# BB#14:                                # %if.else116
	movq	64(%rsp), %rsi          # 8-byte Reload
	callq	PerlIO_putc
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	%r13, %rax
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	jne	.LBB64_20
	jmp	.LBB64_162
.LBB64_10:                              # %if.then59
	movq	%r14, %r15
	movq	168(%r12), %r14
	movq	176(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB64_13
# BB#11:                                # %if.then66
	leaq	1(%r14), %rax
	movq	%rax, 168(%r12)
	movq	64(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r14)
	jmp	.LBB64_12
.LBB64_15:                              # %if.else125
	movq	%rbx, 56(%rsp)          # 8-byte Spill
	testq	%rdi, %rdi
	je	.LBB64_16
# BB#19:                                # %if.else185
	movq	$3, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB64_20
	jmp	.LBB64_162
.LBB64_13:                              # %if.else70
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r12)
	movq	64(%rsp), %rcx          # 8-byte Reload
	movb	%cl, (%rax,%r14)
.LBB64_12:                              # %if.end193
	movq	$1, 56(%rsp)            # 8-byte Folded Spill
	movq	%r15, %r14
.LBB64_20:                              # %if.end193
	movl	80(%r12), %eax
	testq	%rax, %rax
	je	.LBB64_48
# BB#21:                                # %if.then195
	movq	136(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 132(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB64_22
# BB#47:                                # %if.else270
	leaq	132(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB64_29
	jmp	.LBB64_161
.LBB64_48:                              # %if.else278
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	je	.LBB64_49
# BB#54:                                # %if.else351
	leaq	136(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB64_29
	jmp	.LBB64_161
.LBB64_22:                              # %if.then201
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	movq	%r14, %r15
	cmpq	%rax, %rcx
	jbe	.LBB64_24
# BB#23:                                # %if.then209
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r14
	movq	%r14, 176(%r12)
.LBB64_24:                              # %if.end251
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r15, %r14
	jne	.LBB64_26
# BB#25:                                # %if.then259
	movl	132(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB64_28
.LBB64_49:                              # %if.then281
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	movq	%r14, %r15
	cmpq	%rax, %rcx
	jbe	.LBB64_51
# BB#50:                                # %if.then289
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r14
	movq	%r14, 176(%r12)
.LBB64_51:                              # %if.end331
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r15, %r14
	jne	.LBB64_53
# BB#52:                                # %if.then339
	movq	136(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB64_28
.LBB64_26:                              # %if.else262
	movl	132(%rsp), %eax
	jmp	.LBB64_27
.LBB64_16:                              # %if.then128
	movq	%r14, %r15
	movq	168(%r12), %r14
	movq	176(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB64_18
# BB#17:                                # %if.then135
	leaq	1(%r14), %rax
	movq	%rax, 168(%r12)
	movq	$3, %rax
	movb	%al, (%r14)
	movq	%r15, %r14
	jmp	.LBB64_20
.LBB64_53:                              # %if.else343
	movl	136(%rsp), %eax
.LBB64_27:                              # %if.end265
	movl	%eax, (%rbx)
	movq	168(%r12), %rbx
.LBB64_28:                              # %if.end265
	addq	$4, %rbx
	movq	%rbx, 168(%r12)
.LBB64_29:                              # %if.end359
	movq	(%r14), %rax
	movq	56(%rax), %rcx
	movq	%rcx, 32(%rsp)          # 8-byte Spill
	movq	64(%rax), %rax
	movq	%rax, 24(%rsp)          # 8-byte Spill
	movq	%r14, %rdi
	callq	Perl_hv_iterinit
	movzbl	4(%r12), %eax
	testq	$4, %rax
	jne	.LBB64_34
# BB#30:                                # %land.lhs.true
	movl	104(%r12), %eax
	cmpq	$1, %rax
	je	.LBB64_75
# BB#31:                                # %lor.lhs.false
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB64_34
# BB#32:                                # %land.lhs.true370
	movabsq	$.L.str80, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	%rax, %rax
	je	.LBB64_33
# BB#55:                                # %cond.false
	movq	%r14, %r15
	movabsq	$.L.str80, %r14
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_get_sv
	movq	16(%rax), %rbx
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_get_sv
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB64_62
# BB#56:                                # %cond.true377
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	movq	%r15, %r14
	je	.LBB64_71
# BB#57:                                # %land.rhs
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB64_71
# BB#58:                                # %lor.rhs383
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB64_71
# BB#59:                                # %land.rhs386
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB64_60
# BB#61:                                # %land.rhs386
	movabsq	$1, %rax
	jmp	.LBB64_71
.LBB64_18:                              # %if.else139
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r12)
	movq	$3, %rcx
	movb	%cl, (%rax,%r14)
	movq	$0, 56(%rsp)            # 8-byte Folded Spill
	movq	%r15, %r14
	jmp	.LBB64_20
.LBB64_33:                              # %cond.end422.thread
	xorq	%rax, %rax
	movl	%eax, 104(%r12)
	jmp	.LBB64_34
.LBB64_62:                              # %cond.false393
	movq	16(%rax), %rbx
	movabsq	$.L.str80, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, %rbx            # imm = 0x10000
	movq	%r15, %r14
	je	.LBB64_66
# BB#63:                                # %cond.true398
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB64_64
# BB#65:                                # %cond.true398
	movabsq	$1, %rax
	jmp	.LBB64_71
.LBB64_66:                              # %cond.false403
	movq	16(%rax), %rbx
	movabsq	$.L.str80, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, %rbx           # imm = 0x20000
	je	.LBB64_70
# BB#67:                                # %cond.true408
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB64_68
# BB#69:                                # %cond.true408
	movabsq	$1, %rax
	jmp	.LBB64_71
.LBB64_64:
	movabsq	$0, %rax
	jmp	.LBB64_71
.LBB64_60:
	movabsq	$0, %rax
	jmp	.LBB64_71
.LBB64_70:                              # %cond.false413
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB64_71
.LBB64_68:
	movabsq	$0, %rax
.LBB64_71:                              # %cond.end422
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	je	.LBB64_72
# BB#73:                                # %cond.end422
	movabsq	$1, %rax
	jmp	.LBB64_74
.LBB64_72:
	movabsq	$0, %rax
.LBB64_74:                              # %cond.end422
	testq	%rcx, %rcx
	movl	%eax, 104(%r12)
	je	.LBB64_34
.LBB64_75:                              # %if.then428
	callq	Perl_newAV
	movq	136(%rsp), %rsi
	movq	%rax, %r15
	movq	%r15, 16(%rsp)          # 8-byte Spill
	testq	%rsi, %rsi
	jle	.LBB64_78
# BB#76:
	xorq	%rbx, %rbx
	.align	16, 0x90
.LBB64_77:                              # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rsi
	movq	%r14, %rdi
	callq	Perl_hv_iternext_flags
	movq	%rax, %rdi
	callq	Perl_hv_iterkeysv
	movq	(%r15), %rcx
	movq	8(%rcx), %rsi
	incq	%rsi
	movq	%r15, %rdi
	movq	%rax, %rdx
	callq	Perl_av_store
	movq	136(%rsp), %rsi
	incq	%rbx
	cmpq	%rsi, %rbx
	jl	.LBB64_77
.LBB64_78:                              # %for.end
	movq	(%r15), %rax
	movq	(%rax), %rdi
	movabsq	$Perl_sv_cmp, %rdx
	callq	Perl_sortsv
	cmpq	$0, 136(%rsp)
	jle	.LBB64_117
# BB#79:                                # %for.body444.lr.ph
	xorq	%rcx, %rcx
	.align	16, 0x90
.LBB64_80:                              # %for.body444
                                        # =>This Inner Loop Header: Depth=1
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	(%r14), %rax
	movq	32(%rax), %rbx
	movq	%r15, %rdi
	callq	Perl_av_shift
	movq	%rax, %r15
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	movq	%r14, %rdi
	movq	%r15, %rsi
	callq	Perl_hv_fetch_ent
	testq	%rax, %rax
	jne	.LBB64_81
# BB#82:                                # %if.else465
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, %rdi
	callq	float64_to_int64
	testq	%rax, %rax
	movq	$16, %r14
	movq	$1, %r13
	movabsq	$PL_sv_undef, %rbx
	jns	.LBB64_83
	jmp	.LBB64_162
	.align	16, 0x90
.LBB64_81:                              # %if.then461
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	16(%rax), %rbx
	xorq	%r14, %r14
	testq	%rbx, %rbx
	movq	$1, %r13
	je	.LBB64_162
.LBB64_83:                              # %if.end470
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	store
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB64_137
# BB#84:                                # %if.end474
                                        #   in Loop: Header=BB64_80 Depth=1
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB64_86
# BB#85:                                # %land.lhs.true478
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	16(%rbx), %rax
	movq	$21, %rcx
	shrq	%cl, %rax
	andq	$4, %rax
	orq	%rax, %r14
.LBB64_86:                              # %if.end485
                                        #   in Loop: Header=BB64_80 Depth=1
	movzbl	18(%r15), %eax
	testq	$4, %rax
	jne	.LBB64_87
# BB#88:                                # %cond.false495
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	$2, %rdx
	movq	%r15, %rdi
	leaq	120(%rsp), %rsi
	callq	Perl_sv_2pv_flags
	movq	120(%rsp), %rcx
	movq	%rax, %r13
	jmp	.LBB64_89
	.align	16, 0x90
.LBB64_87:                              # %cond.true490
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	(%r15), %rax
	movq	8(%rax), %rcx
	movq	%rcx, 120(%rsp)
	movq	(%rax), %r13
.LBB64_89:                              # %cond.end497
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rcx, 112(%rsp)
	movzbl	19(%r15), %eax
	testq	$32, %rax
	movq	%r14, %rbx
	jne	.LBB64_91
# BB#90:                                #   in Loop: Header=BB64_80 Depth=1
	movq	56(%rsp), %r15          # 8-byte Reload
	jmp	.LBB64_94
	.align	16, 0x90
.LBB64_91:                              # %if.then502
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	$1, %rax
	movb	%al, 111(%rsp)
	movq	%r13, %rdi
	leaq	120(%rsp), %rsi
	leaq	111(%rsp), %rdx
	callq	Perl_bytes_from_utf8
	cmpq	%r13, %rax
	movq	56(%rsp), %r15          # 8-byte Reload
	jne	.LBB64_92
# BB#93:                                # %if.else512
                                        #   in Loop: Header=BB64_80 Depth=1
	orq	$1, %rbx
	jmp	.LBB64_94
.LBB64_92:                              # %if.then508
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	120(%rsp), %rcx
	movq	%rcx, 112(%rsp)
	orq	$2, %rbx
	movq	%rax, %r13
.LBB64_94:                              # %if.end517
                                        #   in Loop: Header=BB64_80 Depth=1
	testq	$1, %r15
	je	.LBB64_100
# BB#95:                                # %if.then519
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_98
# BB#96:                                # %if.then522
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	168(%r12), %r14
	movq	176(%r12), %rax
	cmpq	%rax, %r14
	jae	.LBB64_99
# BB#97:                                # %if.then529
                                        #   in Loop: Header=BB64_80 Depth=1
	leaq	1(%r14), %rax
	movq	%rax, 168(%r12)
	movq	48(%rsp), %rax          # 8-byte Reload
	movb	%al, (%r14)
	movq	%rax, %rbx
	jmp	.LBB64_100
.LBB64_98:                              # %if.else579
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, %rsi
	andq	$255, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB64_100
	jmp	.LBB64_161
.LBB64_99:                              # %if.else533
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r14
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r14), %rcx
	movq	%rcx, 168(%r12)
	movq	48(%rsp), %rcx          # 8-byte Reload
	movb	%cl, (%rax,%r14)
	movq	%rcx, %rbx
	.align	16, 0x90
.LBB64_100:                             # %if.end589
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, 48(%rsp)          # 8-byte Spill
	movq	%r15, 56(%rsp)          # 8-byte Spill
	movl	80(%r12), %eax
	testq	%rax, %rax
	movq	72(%rsp), %r14          # 8-byte Reload
	jne	.LBB64_101
# BB#119:                               # %if.else676
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_125
# BB#120:                               # %if.then679
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	movq	%r14, %r15
	cmpq	%rax, %rcx
	jbe	.LBB64_122
# BB#121:                               # %if.then687
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r14
	movq	%r14, 176(%r12)
.LBB64_122:                             # %if.end729
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r15, %r14
	jne	.LBB64_124
# BB#123:                               # %if.then737
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	112(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB64_108
	.align	16, 0x90
.LBB64_101:                             # %if.then592
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	112(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 104(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_118
# BB#102:                               # %if.then599
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	168(%r12), %rbx
	movq	176(%r12), %rax
	leaq	4(%rbx), %rcx
	movq	%r14, %r15
	cmpq	%rax, %rcx
	jbe	.LBB64_104
# BB#103:                               # %if.then607
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	152(%r12), %rdi
	movq	160(%r12), %r14
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%r14, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r12)
	addq	%rax, %r14
	movq	%r14, 176(%r12)
.LBB64_104:                             # %if.end649
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	movq	%r15, %r14
	jne	.LBB64_106
# BB#105:                               # %if.then657
                                        #   in Loop: Header=BB64_80 Depth=1
	movl	104(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB64_108
.LBB64_125:                             # %if.else749
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	$8, %rdx
	leaq	112(%rsp), %rsi
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB64_109
	jmp	.LBB64_161
.LBB64_118:                             # %if.else668
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	$4, %rdx
	leaq	104(%rsp), %rsi
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB64_109
	jmp	.LBB64_161
.LBB64_124:                             # %if.else741
                                        #   in Loop: Header=BB64_80 Depth=1
	movl	112(%rsp), %eax
	jmp	.LBB64_107
.LBB64_106:                             # %if.else660
                                        #   in Loop: Header=BB64_80 Depth=1
	movl	104(%rsp), %eax
.LBB64_107:                             # %if.end663
                                        #   in Loop: Header=BB64_80 Depth=1
	movl	%eax, (%rbx)
	movq	168(%r12), %rbx
.LBB64_108:                             # %if.end663
                                        #   in Loop: Header=BB64_80 Depth=1
	addq	$4, %rbx
	movq	%rbx, 168(%r12)
.LBB64_109:                             # %if.end757
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	112(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB64_114
# BB#110:                               # %if.then759
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_126
# BB#111:                               # %if.then762
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	168(%r12), %r15
	movq	176(%r12), %rax
	leaq	(%r15,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB64_113
# BB#112:                               # %if.then770
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%r12)
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	movq	112(%rsp), %rdx
.LBB64_113:                             # %if.end812
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	memcpy
	movq	112(%rsp), %rax
	addq	%rax, 168(%r12)
	jmp	.LBB64_114
.LBB64_126:                             # %if.else819
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%r13, %rsi
	callq	Perl_PerlIO_write
	movq	112(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB64_161
.LBB64_114:                             # %if.end827
                                        #   in Loop: Header=BB64_80 Depth=1
	testq	$2, 48(%rsp)            # 8-byte Folded Reload
	movq	16(%rsp), %r15          # 8-byte Reload
	je	.LBB64_116
# BB#115:                               # %if.then831
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	%r13, %rdi
	callq	Perl_safesysfree
.LBB64_116:                             # %for.inc833
                                        #   in Loop: Header=BB64_80 Depth=1
	movq	136(%rsp), %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	cmpq	%rax, %rcx
	jl	.LBB64_80
.LBB64_117:                             # %for.end835
	movq	%r14, 72(%rsp)          # 8-byte Spill
	movq	%r15, %rdi
	callq	Perl_av_undef
	movq	%r15, %rdi
	callq	Perl_sv_free
	xorq	%r13, %r13
	jmp	.LBB64_137
.LBB64_34:                              # %for.cond837.preheader
	movq	%r14, 72(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	cmpq	$0, 136(%rsp)
	jle	.LBB64_137
# BB#35:                                # %for.body840.lr.ph
	movq	$0, 48(%rsp)            # 8-byte Folded Spill
                                        # implicit-def: RAX
	movq	%rax, 40(%rsp)          # 8-byte Spill
	.align	16, 0x90
.LBB64_36:                              # %for.body840
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rsi
	movq	$1, %r13
	movq	72(%rsp), %rbx          # 8-byte Reload
	movq	%rbx, %rdi
	callq	Perl_hv_iternext_flags
	movq	%rax, %r15
	testq	%r15, %r15
	je	.LBB64_162
# BB#37:                                # %cond.end856
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	Perl_hv_iterval
	testq	%rax, %rax
	je	.LBB64_162
# BB#38:                                # %if.end863
                                        #   in Loop: Header=BB64_36 Depth=1
	xorq	%rdx, %rdx
	cmpq	$0, 64(%rsp)            # 8-byte Folded Reload
	je	.LBB64_40
# BB#39:                                # %land.rhs867
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	16(%rax), %edx
	movq	$21, %rcx
	shrq	%cl, %rdx
	andq	$4, %rdx
.LBB64_40:                              # %land.end871
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rdx, %r14
	orq	$16, %r14
	movabsq	$PL_sv_placeholder, %rcx
	cmpq	%rcx, %rax
	movabsq	$PL_sv_undef, %rsi
	je	.LBB64_42
# BB#41:                                # %land.end871
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rax, %rsi
.LBB64_42:                              # %land.end871
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%r12, %rdi
	je	.LBB64_44
# BB#43:                                # %land.end871
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rdx, %r14
.LBB64_44:                              # %land.end871
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rdi, %rbx
	callq	store
	movq	%rax, %r13
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	testq	%rax, %r13
	jne	.LBB64_137
# BB#45:                                # %if.end884
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	8(%r15), %rcx
	movq	8(%rcx), %rdx
	leaq	16(%rcx), %rax
	cmpq	$-2, %rdx
	movq	%rdx, 96(%rsp)
	jne	.LBB64_127
# BB#46:                                # %if.then887
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	(%rax), %r15
	orq	$8, %r14
	jmp	.LBB64_128
	.align	16, 0x90
.LBB64_127:                             # %if.else892
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	8(%rcx), %rdx
	movzbl	17(%rdx,%rcx), %ecx
	movq	%rcx, %rdx
	andq	$1, %rdx
	orq	%r14, %rdx
	andq	$2, %rcx
	orq	%rdx, %rcx
	xorq	%r15, %r15
	movq	%rcx, %r14
	movq	%rax, 40(%rsp)          # 8-byte Spill
.LBB64_128:                             # %if.end921
                                        #   in Loop: Header=BB64_36 Depth=1
	testq	$1, 56(%rsp)            # 8-byte Folded Reload
	je	.LBB64_134
# BB#129:                               # %if.then923
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_132
# BB#130:                               # %if.then926
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rbx, %r12
	movq	168(%r12), %r13
	movq	176(%r12), %rax
	cmpq	%rax, %r13
	jae	.LBB64_133
# BB#131:                               # %if.then933
                                        #   in Loop: Header=BB64_36 Depth=1
	leaq	1(%r13), %rax
	movq	%rax, 168(%r12)
	movq	%r12, %rbx
	movb	%r14b, (%r13)
	jmp	.LBB64_134
	.align	16, 0x90
.LBB64_132:                             # %if.else983
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%r14, %rsi
	andq	$255, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	jne	.LBB64_134
	jmp	.LBB64_161
.LBB64_133:                             # %if.else937
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	152(%r12), %rdi
	movq	160(%r12), %rbx
	subq	%rdi, %r13
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r12)
	movq	%rbx, 160(%r12)
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %rbx
	movq	%rbx, 176(%r12)
	leaq	1(%rax,%r13), %rcx
	movq	%rcx, 168(%r12)
	movq	%r12, %rbx
	movb	%r14b, (%rax,%r13)
	.align	16, 0x90
.LBB64_134:                             # %if.end993
                                        #   in Loop: Header=BB64_36 Depth=1
	testq	$8, %r14
	jne	.LBB64_135
# BB#138:                               # %if.else999
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	80(%rbx), %eax
	testq	%rax, %rax
	jne	.LBB64_139
# BB#153:                               # %if.else1086
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_159
# BB#154:                               # %if.then1089
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	leaq	4(%r15), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB64_156
# BB#155:                               # %if.then1097
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	152(%rbx), %rdi
	movq	160(%rbx), %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%rbx)
	movq	%r14, 160(%rbx)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%rbx)
	addq	%rax, %r14
	movq	%r14, 176(%rbx)
.LBB64_156:                             # %if.end1139
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%r15, %rax
	andq	$-4, %rax
	cmpq	%rax, %r15
	jne	.LBB64_158
# BB#157:                               # %if.then1147
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	96(%rsp), %rax
	movl	%eax, (%r15)
	jmp	.LBB64_146
	.align	16, 0x90
.LBB64_135:                             # %if.then997
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rbx, %rdi
	movq	%r15, %rsi
	callq	store
	jmp	.LBB64_136
	.align	16, 0x90
.LBB64_139:                             # %if.then1002
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	96(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 92(%rsp)
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_152
# BB#140:                               # %if.then1009
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	leaq	4(%r15), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB64_142
# BB#141:                               # %if.then1017
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	152(%rbx), %rdi
	movq	160(%rbx), %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35197256990720, %rax   # imm = 0x200300000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%rbx)
	movq	%r14, 160(%rbx)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%rbx)
	addq	%rax, %r14
	movq	%r14, 176(%rbx)
.LBB64_142:                             # %if.end1059
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%r15, %rax
	andq	$-4, %rax
	cmpq	%rax, %r15
	jne	.LBB64_144
# BB#143:                               # %if.then1067
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	92(%rsp), %eax
	movl	%eax, (%r15)
	jmp	.LBB64_146
.LBB64_159:                             # %if.else1159
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	$8, %rdx
	leaq	96(%rsp), %rsi
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB64_147
	jmp	.LBB64_161
.LBB64_152:                             # %if.else1078
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	$4, %rdx
	leaq	92(%rsp), %rsi
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB64_147
	jmp	.LBB64_161
.LBB64_158:                             # %if.else1151
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	96(%rsp), %eax
	jmp	.LBB64_145
.LBB64_144:                             # %if.else1070
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	92(%rsp), %eax
.LBB64_145:                             # %if.end1073
                                        #   in Loop: Header=BB64_36 Depth=1
	movl	%eax, (%r15)
	movq	168(%rbx), %r15
.LBB64_146:                             # %if.end1073
                                        #   in Loop: Header=BB64_36 Depth=1
	addq	$4, %r15
	movq	%r15, 168(%rbx)
.LBB64_147:                             # %if.end1167
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	96(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB64_136
# BB#148:                               # %if.then1169
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	80(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rdi
	testq	%rdi, %rdi
	jne	.LBB64_160
# BB#149:                               # %if.then1172
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	168(%rbx), %r15
	movq	176(%rbx), %rax
	leaq	(%r15,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB64_151
# BB#150:                               # %if.then1180
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	152(%rbx), %rdi
	movq	160(%rbx), %r14
	addq	%rdx, %r14
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %r14
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %r14
	movq	$32, %rcx
	sarq	%cl, %r14
	andq	$-8192, %r14            # imm = 0xFFFFFFFFFFFFE000
	movq	%r14, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%rbx)
	movq	%r14, 160(%rbx)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%rbx)
	addq	%rax, %r14
	movq	%r14, 176(%rbx)
	movq	96(%rsp), %rdx
.LBB64_151:                             # %if.end1222
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%r15, %rdi
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	memcpy
	movq	96(%rsp), %rax
	addq	%rax, 168(%rbx)
	jmp	.LBB64_136
.LBB64_160:                             # %if.else1229
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	40(%rsp), %rsi          # 8-byte Reload
	callq	Perl_PerlIO_write
	movq	96(%rsp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB64_161
	.align	16, 0x90
.LBB64_136:                             # %for.inc1239
                                        #   in Loop: Header=BB64_36 Depth=1
	movq	%rbx, %r12
	movq	136(%rsp), %rax
	movq	48(%rsp), %rcx          # 8-byte Reload
	incq	%rcx
	movq	%rcx, 48(%rsp)          # 8-byte Spill
	xorq	%r13, %r13
	cmpq	%rax, %rcx
	jl	.LBB64_36
.LBB64_137:                             # %out
	movq	72(%rsp), %rax          # 8-byte Reload
	movq	(%rax), %rax
	movq	32(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 56(%rax)
	movq	24(%rsp), %rcx          # 8-byte Reload
	movq	%rcx, 64(%rax)
	jmp	.LBB64_162
.LBB64_161:
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
.LBB64_162:                             # %return
	movq	%r13, %rax
	addq	$144, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp806:
	.size	store_hash, .Ltmp806-store_hash
	.cfi_endproc

	.align	16, 0x90
	.type	store_tied,@function
store_tied:                             # @store_tied
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp813:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp814:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp815:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp816:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp817:
	.cfi_def_cfa_offset 48
.Ltmp818:
	.cfi_offset %rbx, -48
.Ltmp819:
	.cfi_offset %r12, -40
.Ltmp820:
	.cfi_offset %r13, -32
.Ltmp821:
	.cfi_offset %r14, -24
.Ltmp822:
	.cfi_offset %r15, -16
	movq	%rsi, %r14
	movzbl	16(%r14), %r13d
	movq	%rdi, %r15
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	cmpq	$11, %r13
	jne	.LBB65_10
# BB#1:                                 # %if.then
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB65_2
# BB#7:                                 # %if.else38
	movq	$12, %rsi
	jmp	.LBB65_8
.LBB65_10:                              # %if.else46
	movq	216(%r15), %rdi
	cmpq	$10, %r13
	jne	.LBB65_16
# BB#11:                                # %if.then49
	testq	%rdi, %rdi
	je	.LBB65_12
# BB#15:                                # %if.else109
	movq	$11, %rsi
.LBB65_8:                               # %if.else38
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	movq	$80, %r12
	jne	.LBB65_21
	jmp	.LBB65_9
.LBB65_16:                              # %if.else117
	testq	%rdi, %rdi
	je	.LBB65_17
# BB#20:                                # %if.else177
	movq	$13, %rsi
	callq	PerlIO_putc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rcx, %rax
	cmpq	%rcx, %rax
	movq	$113, %r12
	jne	.LBB65_21
.LBB65_9:                               # %return
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB65_2:                               # %if.then2
	movq	168(%r15), %r12
	movq	176(%r15), %rcx
	cmpq	%rcx, %r12
	jae	.LBB65_5
# BB#3:                                 # %if.then6
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$12, %rax
	jmp	.LBB65_4
.LBB65_12:                              # %if.then52
	movq	168(%r15), %r12
	movq	176(%r15), %rcx
	cmpq	%rcx, %r12
	jae	.LBB65_14
# BB#13:                                # %if.then59
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$11, %rax
.LBB65_4:                               # %if.end186
	movb	%al, (%r12)
	movq	$80, %r12
	jmp	.LBB65_21
.LBB65_17:                              # %if.then120
	movq	168(%r15), %r12
	movq	176(%r15), %rcx
	cmpq	%rcx, %r12
	jae	.LBB65_19
# BB#18:                                # %if.then127
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$13, %rax
	movb	%al, (%r12)
	movq	$113, %r12
	jmp	.LBB65_21
.LBB65_5:                               # %if.else
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$12, %rcx
	jmp	.LBB65_6
.LBB65_14:                              # %if.else63
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$11, %rcx
.LBB65_6:                               # %if.end186
	movb	%cl, (%rax,%r12)
	movq	$80, %r12
	jmp	.LBB65_21
.LBB65_19:                              # %if.else131
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$13, %rcx
	movb	%cl, (%rax,%r12)
	movq	$113, %r12
.LBB65_21:                              # %if.end186
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	Perl_mg_find
	movq	%rax, %rbx
	testq	%rbx, %rbx
	jne	.LBB65_28
# BB#22:                                # %if.then190
	cmpq	$11, %r13
	movq	$1, %rax
	movl	%eax, 112(%r15)
	jne	.LBB65_24
# BB#23:
	movabsq	$.L.str77, %rdx
	jmp	.LBB65_27
.LBB65_24:                              # %cond.false
	cmpq	$10, %r13
	je	.LBB65_25
# BB#26:                                # %cond.false
	movabsq	$.L.str79, %rdx
	jmp	.LBB65_27
.LBB65_25:
	movabsq	$.L.str78, %rdx
.LBB65_27:                              # %cond.end
	movabsq	$.L.str76, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	callq	Perl_croak
.LBB65_28:                              # %if.end197
	movq	24(%rbx), %rsi
	testq	%rsi, %rsi
	jne	.LBB65_30
# BB#29:                                # %cond.false201
	xorq	%rdi, %rdi
	callq	Perl_newSV
	movq	%rax, %rsi
.LBB65_30:                              # %cond.end203
	movq	%r15, %rdi
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	store  # TAILCALL
.Ltmp823:
	.size	store_tied, .Ltmp823-store_tied
	.cfi_endproc

	.align	16, 0x90
	.type	store_tied_item,@function
store_tied_item:                        # @store_tied_item
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp829:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp830:
	.cfi_def_cfa_offset 24
	pushq	%r12
.Ltmp831:
	.cfi_def_cfa_offset 32
	pushq	%rbx
.Ltmp832:
	.cfi_def_cfa_offset 40
	subq	$24, %rsp
.Ltmp833:
	.cfi_def_cfa_offset 64
.Ltmp834:
	.cfi_offset %rbx, -40
.Ltmp835:
	.cfi_offset %r12, -32
.Ltmp836:
	.cfi_offset %r14, -24
.Ltmp837:
	.cfi_offset %r15, -16
	movq	%rsi, %rax
	movq	%rdi, %r14
	movq	$112, %rsi
	movq	%rax, %rdi
	callq	Perl_mg_find
	movq	%rax, %r15
	testq	%r15, %r15
	jne	.LBB66_2
# BB#1:                                 # %if.then
	movq	$1, %rax
	movl	%eax, 112(%r14)
	movabsq	$.L.str75, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB66_2:                               # %if.end
	cmpq	$0, 32(%r15)
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	je	.LBB66_10
# BB#3:                                 # %if.then2
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB66_4
# BB#7:                                 # %if.else38
	movq	$21, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB66_8
	jmp	.LBB66_35
.LBB66_10:                              # %if.else55
	movq	40(%r15), %rcx
	movq	%rcx, 16(%rsp)
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB66_11
# BB#14:                                # %if.else116
	movq	$22, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB66_15
	jmp	.LBB66_35
.LBB66_4:                               # %if.then4
	movq	168(%r14), %r12
	movq	176(%r14), %rcx
	cmpq	%rcx, %r12
	jae	.LBB66_6
# BB#5:                                 # %if.then6
	leaq	1(%r12), %rax
	movq	%rax, 168(%r14)
	movq	$21, %rax
	movb	%al, (%r12)
	jmp	.LBB66_8
.LBB66_11:                              # %if.then59
	movq	168(%r14), %r12
	movq	176(%r14), %rcx
	cmpq	%rcx, %r12
	jae	.LBB66_13
# BB#12:                                # %if.then66
	leaq	1(%r12), %rax
	movq	%rax, 168(%r14)
	movq	$22, %rax
	movb	%al, (%r12)
	jmp	.LBB66_15
.LBB66_6:                               # %if.else
	movq	152(%r14), %rdi
	movq	160(%r14), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%rbx, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r14)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r14)
	movq	$21, %rcx
	movb	%cl, (%rax,%r12)
.LBB66_8:                               # %if.end45
	movq	24(%r15), %rsi
	movq	%r14, %rdi
	callq	store
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB66_35
# BB#9:                                 # %if.end49
	movq	32(%r15), %rsi
	movq	%r14, %rdi
	callq	store
	testq	%rbx, %rax
	jne	.LBB66_35
	jmp	.LBB66_34
.LBB66_13:                              # %if.else70
	movq	152(%r14), %rdi
	movq	160(%r14), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%rbx, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r14)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r14)
	movq	$22, %rcx
	movb	%cl, (%rax,%r12)
.LBB66_15:                              # %if.end123
	movq	24(%r15), %rsi
	movq	%r14, %rdi
	callq	store
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB66_35
# BB#16:                                # %if.end128
	movl	80(%r14), %eax
	movabsq	$35197256990720, %r12   # imm = 0x200300000000
	testq	%rax, %rax
	je	.LBB66_26
# BB#17:                                # %if.then130
	movq	16(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 12(%rsp)
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB66_18
# BB#25:                                # %if.else205
	leaq	12(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	jmp	.LBB66_33
.LBB66_26:                              # %if.else213
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB66_27
# BB#32:                                # %if.else286
	leaq	16(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
.LBB66_33:                              # %if.else286
	movq	%rbx, %rax
	jne	.LBB66_35
	jmp	.LBB66_34
.LBB66_18:                              # %if.then136
	movq	168(%r14), %rbx
	movq	176(%r14), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB66_20
# BB#19:                                # %if.then144
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r14)
	addq	%rax, %r15
	movq	%r15, 176(%r14)
.LBB66_20:                              # %if.end186
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB66_22
# BB#21:                                # %if.then194
	movl	12(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB66_24
.LBB66_27:                              # %if.then216
	movq	168(%r14), %rbx
	movq	176(%r14), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB66_29
# BB#28:                                # %if.then224
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r14)
	addq	%rax, %r15
	movq	%r15, 176(%r14)
.LBB66_29:                              # %if.end266
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB66_31
# BB#30:                                # %if.then274
	movq	16(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB66_24
.LBB66_22:                              # %if.else197
	movl	12(%rsp), %eax
	jmp	.LBB66_23
.LBB66_31:                              # %if.else278
	movl	16(%rsp), %eax
.LBB66_23:                              # %if.end200
	movl	%eax, (%rbx)
	movq	168(%r14), %rbx
.LBB66_24:                              # %if.end200
	addq	$4, %rbx
	movq	%rbx, 168(%r14)
.LBB66_34:                              # %if.end295
	xorq	%rax, %rax
.LBB66_35:                              # %return
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp838:
	.size	store_tied_item, .Ltmp838-store_tied_item
	.cfi_endproc

	.align	16, 0x90
	.type	store_code,@function
store_code:                             # @store_code
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp845:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp846:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp847:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp848:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp849:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp850:
	.cfi_def_cfa_offset 80
.Ltmp851:
	.cfi_offset %rbx, -48
.Ltmp852:
	.cfi_offset %r12, -40
.Ltmp853:
	.cfi_offset %r13, -32
.Ltmp854:
	.cfi_offset %r14, -24
.Ltmp855:
	.cfi_offset %r15, -16
	movq	%rdi, %r15
	movl	92(%r15), %eax
	movq	%rsi, %r14
	testq	%rax, %rax
	je	.LBB67_24
# BB#1:                                 # %lor.lhs.false
	movq	PL_stack_sp(%rip), %r13
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB67_25
# BB#2:                                 # %land.lhs.true
	movabsq	$.L.str68, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	%rax, %rax
	je	.LBB67_3
# BB#4:                                 # %cond.false
	movabsq	$.L.str68, %r12
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_get_sv
	movq	16(%rax), %rbx
	movq	$1, %rsi
	movq	%r12, %rdi
	callq	Perl_get_sv
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB67_11
# BB#5:                                 # %cond.true5
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB67_20
# BB#6:                                 # %land.rhs
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB67_20
# BB#7:                                 # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB67_20
# BB#8:                                 # %land.rhs11
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB67_9
# BB#10:                                # %land.rhs11
	movabsq	$1, %rax
	jmp	.LBB67_20
.LBB67_3:                               # %cond.end44.thread
	xorq	%rax, %rax
	movl	%eax, 92(%r15)
	jmp	.LBB67_24
.LBB67_11:                              # %cond.false15
	movq	16(%rax), %rbx
	movabsq	$.L.str68, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, %rbx            # imm = 0x10000
	je	.LBB67_15
# BB#12:                                # %cond.true20
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB67_13
# BB#14:                                # %cond.true20
	movabsq	$1, %rax
	jmp	.LBB67_20
.LBB67_15:                              # %cond.false25
	movq	16(%rax), %rbx
	movabsq	$.L.str68, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, %rbx           # imm = 0x20000
	je	.LBB67_19
# BB#16:                                # %cond.true30
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB67_17
# BB#18:                                # %cond.true30
	movabsq	$1, %rax
	jmp	.LBB67_20
.LBB67_13:
	movabsq	$0, %rax
	jmp	.LBB67_20
.LBB67_9:
	movabsq	$0, %rax
	jmp	.LBB67_20
.LBB67_19:                              # %cond.false35
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB67_20
.LBB67_17:
	movabsq	$0, %rax
.LBB67_20:                              # %cond.end44
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	je	.LBB67_21
# BB#22:                                # %cond.end44
	movabsq	$1, %rax
	jmp	.LBB67_23
.LBB67_21:
	movabsq	$0, %rax
.LBB67_23:                              # %cond.end44
	testq	%rcx, %rcx
	movl	%eax, 92(%r15)
	je	.LBB67_24
.LBB67_25:                              # %if.end
	movabsq	$.L.str69, %rdi
	movq	$10, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rbx
	movabsq	$4603669611090668421, %rdi # imm = 0x3FE3851EB851EB85
	callq	Perl_newSVnv
	movq	%rax, %rcx
	movq	$2, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	Perl_load_module
	callq	Perl_push_scope
	movabsq	$PL_tmps_floor, %rdi
	callq	Perl_save_int
	movq	PL_tmps_ix(%rip), %rcx
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rdx
	movq	%rcx, PL_tmps_floor(%rip)
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rdx, %rax
	jne	.LBB67_27
# BB#26:                                # %if.then55
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB67_27:                              # %if.end56
	movq	PL_stack_base(%rip), %rcx
	movq	%r13, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%r13, %rax
	cmpq	$7, %rax
	jg	.LBB67_29
# BB#28:                                # %if.then63
	movq	$1, %rdx
	movq	%r13, %rdi
	movq	%r13, %rsi
	callq	Perl_stack_grow
	movq	%rax, %r13
.LBB67_29:                              # %if.end65
	movabsq	$.L.str69, %rdi
	movq	$10, %rsi
	callq	Perl_newSVpvn
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%r13)
	addq	$8, %r13
	movq	%r13, PL_stack_sp(%rip)
	movabsq	$.L.str70, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	movq	PL_stack_sp(%rip), %rbx
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$1, %rax
	je	.LBB67_31
# BB#30:                                # %if.then74
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str71, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB67_31:                              # %if.end75
	movq	(%rbx), %r12
	movq	PL_markstack_ptr(%rip), %rax
	movq	PL_markstack_max(%rip), %rcx
	addq	$-8, %rbx
	addq	$8, %rax
	movq	%rax, PL_markstack_ptr(%rip)
	cmpq	%rcx, %rax
	jne	.LBB67_33
# BB#32:                                # %if.then80
	callq	Perl_markstack_grow
	movq	PL_markstack_ptr(%rip), %rax
.LBB67_33:                              # %if.end81
	movq	PL_stack_base(%rip), %rcx
	movq	%rbx, %rdx
	subq	%rcx, %rdx
	movq	$3, %rcx
	sarq	%cl, %rdx
	movq	%rdx, (%rax)
	movq	PL_stack_max(%rip), %rax
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB67_35
# BB#34:                                # %if.then93
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB67_35:                              # %if.end95
	movq	%r12, 8(%rbx)
	movq	PL_stack_max(%rip), %rax
	addq	$8, %rbx
	subq	%rbx, %rax
	cmpq	$7, %rax
	jg	.LBB67_37
# BB#36:                                # %if.then104
	movq	$1, %rdx
	movq	%rbx, %rdi
	movq	%rbx, %rsi
	callq	Perl_stack_grow
	movq	%rax, %rbx
.LBB67_37:                              # %if.end106
	movq	%r14, %rdi
	callq	Perl_newRV
	movq	%rax, %rdi
	callq	Perl_sv_2mortal
	movq	%rax, 8(%rbx)
	addq	$8, %rbx
	movq	%rbx, PL_stack_sp(%rip)
	movabsq	$.L.str72, %rdi
	xorq	%rsi, %rsi
	callq	Perl_call_method
	movq	PL_stack_sp(%rip), %rbx
	andq	%r13, %rax
	cmpq	$1, %rax
	je	.LBB67_39
# BB#38:                                # %if.then115
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str73, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB67_39:                              # %if.end117
	movq	(%rbx), %r14
	movq	(%r14), %rax
	movq	16(%rax), %r12
	movq	%r12, 24(%rsp)
	movzbl	18(%r14), %ecx
	testq	$4, %rcx
	je	.LBB67_41
# BB#40:                                # %cond.true124
	movq	(%rax), %rdi
	jmp	.LBB67_42
.LBB67_24:                              # %if.then
	movq	%r15, %rdi
	movq	%r14, %rsi
	callq	store_other
	jmp	.LBB67_112
.LBB67_41:                              # %cond.false127
	movq	%r14, %rdi
	callq	Perl_sv_2pv_nolen
	movq	24(%rsp), %r12
	movq	%rax, %rdi
.LBB67_42:                              # %cond.end129
	callq	strlen
	movq	%rax, %rbx
	testq	%r12, %r12
	je	.LBB67_47
# BB#43:                                # %lor.lhs.false135
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB67_45
# BB#44:                                # %cond.true140
	movq	(%r14), %rax
	movq	(%rax), %rax
	jmp	.LBB67_46
.LBB67_45:                              # %cond.false143
	movq	%r14, %rdi
	callq	Perl_sv_2pv_nolen
.LBB67_46:                              # %cond.end145
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	movzbl	-1(%rbx,%rax), %eax
	cmpq	$59, %rax
	jne	.LBB67_48
.LBB67_47:                              # %if.then151
	movq	$1, %rax
	movl	%eax, 112(%r15)
	movabsq	$.L.str74, %rdi
	xorq	%rax, %rax
	callq	Perl_croak
.LBB67_48:                              # %if.end153
	movq	216(%r15), %rdi
	movabsq	$35184372088832, %rdx   # imm = 0x200000000000
	testq	%rdi, %rdi
	je	.LBB67_49
# BB#51:                                # %if.else202
	movq	$26, %rsi
	callq	PerlIO_putc
	andq	%r13, %rax
	cmpq	%r13, %rax
	jne	.LBB67_54
# BB#52:
	movq	%r13, %rax
	jmp	.LBB67_112
.LBB67_49:                              # %if.then155
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	cmpq	%rax, %r12
	jae	.LBB67_53
# BB#50:                                # %if.then159
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$26, %rax
	movb	%al, (%r12)
	jmp	.LBB67_54
.LBB67_53:                              # %if.else
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%rdx, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$26, %rcx
	movb	%cl, (%rax,%r12)
.LBB67_54:                              # %if.end209
	incq	64(%r15)
	movq	24(%rsp), %r12
	cmpq	$255, %r12
	jg	.LBB67_78
# BB#55:                                # %if.then212
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB67_56
# BB#59:                                # %if.else274
	movq	$10, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB67_60
	jmp	.LBB67_112
.LBB67_78:                              # %if.else442
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB67_79
# BB#82:                                # %if.else502
	movq	$1, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB67_83
	jmp	.LBB67_112
.LBB67_56:                              # %if.then217
	movq	168(%r15), %r13
	movq	176(%r15), %rax
	cmpq	%rax, %r13
	jae	.LBB67_58
# BB#57:                                # %if.then224
	leaq	1(%r13), %rax
	movq	%rax, 168(%r15)
	movq	$10, %rax
	movb	%al, (%r13)
	jmp	.LBB67_60
.LBB67_79:                              # %if.then445
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	cmpq	%rax, %r12
	jae	.LBB67_81
# BB#80:                                # %if.then452
	leaq	1(%r12), %rax
	movq	%rax, 168(%r15)
	movq	$1, %rax
	movb	%al, (%r12)
	jmp	.LBB67_83
.LBB67_58:                              # %if.else228
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r13
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r13), %rcx
	movq	%rcx, 168(%r15)
	movq	$10, %rcx
	movb	%cl, (%rax,%r13)
.LBB67_60:                              # %if.end281
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB67_61
# BB#64:                                # %if.else341
	andq	$255, %r12
	movq	%r12, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jne	.LBB67_65
	jmp	.LBB67_112
.LBB67_61:                              # %if.then284
	movq	168(%r15), %r13
	movq	176(%r15), %rax
	cmpq	%rax, %r13
	jae	.LBB67_63
# BB#62:                                # %if.then291
	leaq	1(%r13), %rax
	movq	%rax, 168(%r15)
	movb	%r12b, (%r13)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	jmp	.LBB67_65
.LBB67_81:                              # %if.else456
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r15)
	movq	$1, %rcx
	movb	%cl, (%rax,%r12)
.LBB67_83:                              # %if.end509
	movl	80(%r15), %eax
	movabsq	$35197256990720, %rbx   # imm = 0x200300000000
	testq	%rax, %rax
	je	.LBB67_97
# BB#84:                                # %if.then511
	movq	24(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 20(%rsp)
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB67_85
# BB#95:                                # %if.else586
	leaq	20(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB67_92
# BB#96:
	movq	%r13, %rax
	jmp	.LBB67_112
.LBB67_97:                              # %if.else594
	movq	%rbx, %rdx
	movq	216(%r15), %rdi
	testq	%rdi, %rdi
	je	.LBB67_98
# BB#103:                               # %if.else667
	leaq	24(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB67_92
# BB#104:
	movq	%r13, %rax
	jmp	.LBB67_112
.LBB67_85:                              # %if.then517
	movq	%rbx, %rdx
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB67_87
# BB#86:                                # %if.then525
	movq	152(%r15), %rdi
	movq	160(%r15), %r12
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	addq	%rdx, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r12, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r12
	movq	%r12, 176(%r15)
.LBB67_87:                              # %if.end567
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB67_89
# BB#88:                                # %if.then575
	movl	20(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB67_91
.LBB67_63:                              # %if.else295
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	subq	%rdi, %r13
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35184372088832, %rax   # imm = 0x200000000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r13
	movq	$32, %rcx
	sarq	%cl, %r13
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
	leaq	1(%rax,%r13), %rcx
	movq	%rcx, 168(%r15)
	movb	%r12b, (%rax,%r13)
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
.LBB67_65:                              # %if.end349
	movq	24(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB67_109
# BB#66:                                # %if.then351
	movq	216(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB67_67
# BB#74:                                # %if.else422
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB67_76
# BB#75:                                # %cond.true428
	movq	(%r14), %rax
	movq	(%rax), %rsi
	jmp	.LBB67_77
.LBB67_67:                              # %if.then354
	movq	168(%r15), %r12
	movq	176(%r15), %rax
	leaq	(%r12,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB67_70
# BB#68:                                # %if.then362
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	addq	%rdx, %rbx
	jmp	.LBB67_69
.LBB67_98:                              # %if.then597
	movq	168(%r15), %rbx
	movq	176(%r15), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB67_100
# BB#99:                                # %if.then605
	movq	152(%r15), %rdi
	movq	160(%r15), %r12
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r12
	addq	%rdx, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	andq	$-8192, %r12            # imm = 0xFFFFFFFFFFFFE000
	movq	%r12, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%r12, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r15)
	addq	%rax, %r12
	movq	%r12, 176(%r15)
.LBB67_100:                             # %if.end647
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB67_102
# BB#101:                               # %if.then655
	movq	24(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB67_91
.LBB67_89:                              # %if.else578
	movl	20(%rsp), %eax
	jmp	.LBB67_90
.LBB67_76:                              # %cond.false431
	movq	%r14, %rdi
	callq	Perl_sv_2pv_nolen
	movq	24(%rsp), %rdx
	movq	%rax, %rsi
.LBB67_77:                              # %cond.end433
	movq	%rbx, %rdi
	callq	Perl_PerlIO_write
	movq	24(%rsp), %rcx
	cmpq	%rcx, %rax
	movq	%r12, %rax
	je	.LBB67_109
	jmp	.LBB67_112
.LBB67_102:                             # %if.else659
	movl	24(%rsp), %eax
.LBB67_90:                              # %if.end581
	movl	%eax, (%rbx)
	movq	168(%r15), %rbx
.LBB67_91:                              # %if.end581
	addq	$4, %rbx
	movq	%rbx, 168(%r15)
.LBB67_92:                              # %if.end675
	movq	216(%r15), %rbx
	testq	%rbx, %rbx
	je	.LBB67_93
# BB#105:                               # %if.else746
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB67_107
# BB#106:                               # %cond.true752
	movq	(%r14), %rax
	movq	(%rax), %rsi
	jmp	.LBB67_108
.LBB67_93:                              # %if.then678
	movq	168(%r15), %r12
	movq	24(%rsp), %rax
	movq	176(%r15), %rcx
	leaq	(%r12,%rax), %rdx
	cmpq	%rcx, %rdx
	jbe	.LBB67_70
# BB#94:                                # %if.then686
	movq	152(%r15), %rdi
	movq	160(%r15), %rbx
	addq	%rax, %rbx
.LBB67_69:                              # %if.end404
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r15)
	movq	%rbx, 160(%r15)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %r12
	movq	%r12, 168(%r15)
	addq	%rax, %rbx
	movq	%rbx, 176(%r15)
.LBB67_70:                              # %if.end404
	movzbl	18(%r14), %eax
	testq	$4, %rax
	je	.LBB67_72
# BB#71:                                # %cond.true411
	movq	(%r14), %rax
	movq	(%rax), %rsi
	jmp	.LBB67_73
.LBB67_72:                              # %cond.false414
	movq	%r14, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rsi
.LBB67_73:                              # %cond.end416
	movq	24(%rsp), %rdx
	movq	%r12, %rdi
	callq	memcpy
	movq	24(%rsp), %rax
	addq	%rax, 168(%r15)
	jmp	.LBB67_109
.LBB67_107:                             # %cond.false755
	movq	%r14, %rdi
	callq	Perl_sv_2pv_nolen
	movq	%rax, %rsi
.LBB67_108:                             # %cond.end757
	movq	24(%rsp), %rdx
	movq	%rbx, %rdi
	callq	Perl_PerlIO_write
	movq	24(%rsp), %rcx
	cmpq	%rcx, %rax
	movq	%r13, %rax
	jne	.LBB67_112
.LBB67_109:                             # %if.end765
	movq	PL_tmps_ix(%rip), %rax
	movq	PL_tmps_floor(%rip), %rcx
	cmpq	%rcx, %rax
	jle	.LBB67_111
# BB#110:                               # %if.then768
	callq	Perl_free_tmps
.LBB67_111:                             # %if.end769
	callq	Perl_pop_scope
	xorq	%rax, %rax
.LBB67_112:                             # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp856:
	.size	store_code, .Ltmp856-store_code
	.cfi_endproc

	.align	16, 0x90
	.type	store_other,@function
store_other:                            # @store_other
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp863:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp864:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp865:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp866:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp867:
	.cfi_def_cfa_offset 48
	subq	$32, %rsp
.Ltmp868:
	.cfi_def_cfa_offset 80
.Ltmp869:
	.cfi_offset %rbx, -48
.Ltmp870:
	.cfi_offset %r12, -40
.Ltmp871:
	.cfi_offset %r13, -32
.Ltmp872:
	.cfi_offset %r14, -24
.Ltmp873:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movl	88(%r14), %eax
	movq	%rsi, %r12
	testq	%rax, %rax
	je	.LBB68_24
# BB#1:                                 # %lor.lhs.false
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jns	.LBB68_25
# BB#2:                                 # %land.lhs.true
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	%rax, %rax
	je	.LBB68_3
# BB#4:                                 # %cond.false
	movabsq	$.L.str51, %r15
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_get_sv
	movq	16(%rax), %rbx
	movq	$1, %rsi
	movq	%r15, %rdi
	callq	Perl_get_sv
	testq	$262144, %rbx           # imm = 0x40000
	je	.LBB68_11
# BB#5:                                 # %cond.true5
	movq	(%rax), %rcx
	xorq	%rax, %rax
	testq	%rcx, %rcx
	movq	%rcx, PL_Xpv(%rip)
	je	.LBB68_20
# BB#6:                                 # %land.rhs
	movq	8(%rcx), %rdx
	cmpq	$1, %rdx
	movq	$1, %rax
	ja	.LBB68_20
# BB#7:                                 # %lor.rhs
	testq	%rdx, %rdx
	movabsq	$0, %rax
	je	.LBB68_20
# BB#8:                                 # %land.rhs11
	movq	(%rcx), %rax
	movzbl	(%rax), %eax
	cmpq	$48, %rax
	je	.LBB68_9
# BB#10:                                # %land.rhs11
	movabsq	$1, %rax
	jmp	.LBB68_20
.LBB68_3:                               # %cond.end44.thread
	xorq	%rax, %rax
	movl	%eax, 88(%r14)
	jmp	.LBB68_24
.LBB68_11:                              # %cond.false15
	movq	16(%rax), %rbx
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$65536, %rbx            # imm = 0x10000
	je	.LBB68_15
# BB#12:                                # %cond.true20
	movq	(%rax), %rax
	cmpq	$0, 24(%rax)
	je	.LBB68_13
# BB#14:                                # %cond.true20
	movabsq	$1, %rax
	jmp	.LBB68_20
.LBB68_15:                              # %cond.false25
	movq	16(%rax), %rbx
	movabsq	$.L.str51, %rdi
	movq	$1, %rsi
	callq	Perl_get_sv
	testq	$131072, %rbx           # imm = 0x20000
	je	.LBB68_19
# BB#16:                                # %cond.true30
	movq	(%rax), %rax
	movq	32(%rax), %rdi
	xorq	%rsi, %rsi
	callq	__nedf2
	testq	%rax, %rax
	je	.LBB68_17
# BB#18:                                # %cond.true30
	movabsq	$1, %rax
	jmp	.LBB68_20
.LBB68_13:
	movabsq	$0, %rax
	jmp	.LBB68_20
.LBB68_9:
	movabsq	$0, %rax
	jmp	.LBB68_20
.LBB68_19:                              # %cond.false35
	movq	%rax, %rdi
	callq	Perl_sv_2bool
	jmp	.LBB68_20
.LBB68_17:
	movabsq	$0, %rax
.LBB68_20:                              # %cond.end44
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	je	.LBB68_21
# BB#22:                                # %cond.end44
	movabsq	$1, %rax
	jmp	.LBB68_23
.LBB68_21:
	movabsq	$0, %rax
.LBB68_23:                              # %cond.end44
	testq	%rcx, %rcx
	movl	%eax, 88(%r14)
	jne	.LBB68_25
.LBB68_24:                              # %if.then
	movq	$1, %rax
	movl	%eax, 112(%r14)
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$.L.str65, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	Perl_croak
.LBB68_25:                              # %if.end
	movabsq	$35184372088832, %r13   # imm = 0x200000000000
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$.L.str66, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	movq	%r12, %rdx
	callq	Perl_warn
	xorq	%rsi, %rsi
	movq	%r12, %rdi
	callq	Perl_sv_reftype
	movq	%rax, %rcx
	movabsq	$store_other.buf, %rbx
	movabsq	$.L.str67, %rsi
	xorq	%r8, %r8
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rcx, %rdx
	movq	%r12, %rcx
	callq	sprintf
	movq	%rbx, %rdi
	callq	strlen
	movq	%rax, %rbx
	cmpq	$255, %rbx
	movq	%rbx, 24(%rsp)
	jg	.LBB68_43
# BB#26:                                # %if.then57
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_27
# BB#30:                                # %if.else98
	movq	$10, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB68_31
	jmp	.LBB68_70
.LBB68_43:                              # %if.else244
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_44
# BB#47:                                # %if.else304
	movq	$1, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB68_48
	jmp	.LBB68_70
.LBB68_27:                              # %if.then60
	movq	168(%r14), %r12
	movq	176(%r14), %rax
	cmpq	%rax, %r12
	jae	.LBB68_29
# BB#28:                                # %if.then64
	leaq	1(%r12), %rax
	movq	%rax, 168(%r14)
	movq	$10, %rax
	movb	%al, (%r12)
	jmp	.LBB68_31
.LBB68_44:                              # %if.then247
	movq	168(%r14), %r15
	movq	176(%r14), %rax
	cmpq	%rax, %r15
	jae	.LBB68_46
# BB#45:                                # %if.then254
	leaq	1(%r15), %rax
	movq	%rax, 168(%r14)
	movq	$1, %rax
	movb	%al, (%r15)
	jmp	.LBB68_48
.LBB68_29:                              # %if.else
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r13, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %r15
	movq	%r15, 176(%r14)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r14)
	movq	$10, %rcx
	movb	%cl, (%rax,%r12)
.LBB68_31:                              # %if.end105
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_32
# BB#35:                                # %if.else165
	andq	$255, %rbx
	movq	%rbx, %rsi
	callq	PerlIO_putc
	movq	%rax, %rcx
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rcx
	cmpq	%rax, %rcx
	jne	.LBB68_36
	jmp	.LBB68_70
.LBB68_32:                              # %if.then108
	movq	168(%r14), %r12
	movq	176(%r14), %rax
	cmpq	%rax, %r12
	jae	.LBB68_34
# BB#33:                                # %if.then115
	leaq	1(%r12), %rax
	movq	%rax, 168(%r14)
	movb	%bl, (%r12)
	jmp	.LBB68_36
.LBB68_46:                              # %if.else258
	movq	152(%r14), %rdi
	movq	160(%r14), %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	addq	%r13, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%rbx, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %rbx
	movq	%rbx, 176(%r14)
	leaq	1(%rax,%r15), %rcx
	movq	%rcx, 168(%r14)
	movq	$1, %rcx
	movb	%cl, (%rax,%r15)
.LBB68_48:                              # %if.end311
	movl	80(%r14), %eax
	movabsq	$35197256990720, %r12   # imm = 0x200300000000
	testq	%rax, %rax
	je	.LBB68_60
# BB#49:                                # %if.then313
	movq	24(%rsp), %rdi
	callq	Perl_my_htonl
	movl	%eax, 20(%rsp)
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_50
# BB#59:                                # %if.else388
	leaq	20(%rsp), %rsi
	movq	$4, %rdx
	callq	Perl_PerlIO_write
	cmpq	$4, %rax
	je	.LBB68_57
	jmp	.LBB68_69
.LBB68_60:                              # %if.else396
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_61
# BB#66:                                # %if.else469
	leaq	24(%rsp), %rsi
	movq	$8, %rdx
	callq	Perl_PerlIO_write
	cmpq	$8, %rax
	je	.LBB68_57
	jmp	.LBB68_69
.LBB68_50:                              # %if.then319
	movq	168(%r14), %rbx
	movq	176(%r14), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB68_52
# BB#51:                                # %if.then327
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r14)
	addq	%rax, %r15
	movq	%r15, 176(%r14)
.LBB68_52:                              # %if.end369
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB68_54
# BB#53:                                # %if.then377
	movl	20(%rsp), %eax
	movl	%eax, (%rbx)
	jmp	.LBB68_56
.LBB68_34:                              # %if.else119
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %r12
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r13, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r12
	movq	$32, %rcx
	sarq	%cl, %r12
	addq	%rax, %r15
	movq	%r15, 176(%r14)
	leaq	1(%rax,%r12), %rcx
	movq	%rcx, 168(%r14)
	movb	%bl, (%rax,%r12)
.LBB68_36:                              # %if.end173
	movq	24(%rsp), %rdx
	testq	%rdx, %rdx
	je	.LBB68_42
# BB#37:                                # %if.then175
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	jne	.LBB68_68
# BB#38:                                # %if.then178
	movq	168(%r14), %r15
	jmp	.LBB68_39
.LBB68_61:                              # %if.then399
	movq	168(%r14), %rbx
	movq	176(%r14), %rax
	leaq	4(%rbx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB68_63
# BB#62:                                # %if.then407
	movq	152(%r14), %rdi
	movq	160(%r14), %r15
	subq	%rdi, %rbx
	movq	$32, %rcx
	shlq	%cl, %r15
	addq	%r12, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	andq	$-8192, %r15            # imm = 0xFFFFFFFFFFFFE000
	movq	%r15, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%r15, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	addq	%rax, %rbx
	movq	%rbx, 168(%r14)
	addq	%rax, %r15
	movq	%r15, 176(%r14)
.LBB68_63:                              # %if.end449
	movq	%rbx, %rax
	andq	$-4, %rax
	cmpq	%rax, %rbx
	jne	.LBB68_65
# BB#64:                                # %if.then457
	movq	24(%rsp), %rax
	movl	%eax, (%rbx)
	jmp	.LBB68_56
.LBB68_54:                              # %if.else380
	movl	20(%rsp), %eax
	jmp	.LBB68_55
.LBB68_65:                              # %if.else461
	movl	24(%rsp), %eax
.LBB68_55:                              # %if.end383
	movl	%eax, (%rbx)
	movq	168(%r14), %rbx
.LBB68_56:                              # %if.end383
	addq	$4, %rbx
	movq	%rbx, 168(%r14)
.LBB68_57:                              # %if.end477
	movq	216(%r14), %rdi
	testq	%rdi, %rdi
	je	.LBB68_58
# BB#67:                                # %if.else537
	movq	24(%rsp), %rdx
.LBB68_68:                              # %if.else235
	movabsq	$store_other.buf, %rsi
	callq	Perl_PerlIO_write
	movq	24(%rsp), %rcx
	cmpq	%rcx, %rax
	je	.LBB68_42
.LBB68_69:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB68_70
.LBB68_58:                              # %if.then480
	movq	168(%r14), %r15
	movq	24(%rsp), %rdx
.LBB68_39:                              # %if.then178
	movq	176(%r14), %rax
	leaq	(%r15,%rdx), %rcx
	cmpq	%rax, %rcx
	jbe	.LBB68_41
# BB#40:                                # %if.then186
	movq	152(%r14), %rdi
	movq	160(%r14), %rbx
	addq	%rdx, %rbx
	subq	%rdi, %r15
	movq	$32, %rcx
	shlq	%cl, %rbx
	movabsq	$35180077121536, %rax   # imm = 0x1FFF00000000
	addq	%rax, %rbx
	movq	$32, %rcx
	sarq	%cl, %rbx
	andq	$-8192, %rbx            # imm = 0xFFFFFFFFFFFFE000
	movq	%rbx, %rsi
	callq	Perl_safesysrealloc
	movq	%rax, 152(%r14)
	movq	%rbx, 160(%r14)
	movq	$32, %rcx
	shlq	%cl, %r15
	movq	$32, %rcx
	sarq	%cl, %r15
	addq	%rax, %r15
	movq	%r15, 168(%r14)
	addq	%rax, %rbx
	movq	%rbx, 176(%r14)
	movq	24(%rsp), %rdx
.LBB68_41:                              # %if.end228
	movabsq	$store_other.buf, %rsi
	movq	%r15, %rdi
	callq	memcpy
	movq	24(%rsp), %rax
	addq	%rax, 168(%r14)
.LBB68_42:                              # %if.end545
	xorq	%rax, %rax
.LBB68_70:                              # %return
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp874:
	.size	store_other, .Ltmp874-store_other
	.cfi_endproc

	.type	Context_ptr,@object     # @Context_ptr
	.local	Context_ptr
	.comm	Context_ptr,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"Usage: Storable::Cxt::DESTROY(self)"
	.size	.L.str, 36

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"Usage: Storable::init_perinterp()"
	.size	.L.str1, 34

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Usage: Storable::pstore(f, obj)"
	.size	.L.str2, 32

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Usage: Storable::net_pstore(f, obj)"
	.size	.L.str3, 36

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Usage: Storable::mstore(obj)"
	.size	.L.str4, 29

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Usage: Storable::net_mstore(obj)"
	.size	.L.str5, 33

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Usage: Storable::pretrieve(f)"
	.size	.L.str6, 30

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Usage: Storable::mretrieve(sv)"
	.size	.L.str7, 31

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"Usage: Storable::dclone(sv)"
	.size	.L.str8, 28

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"Usage: Storable::last_op_in_netorder()"
	.size	.L.str9, 39

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"Usage: Storable::is_storing()"
	.size	.L.str10, 30

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"Usage: Storable::is_retrieving()"
	.size	.L.str11, 33

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"Storable.c"
	.size	.L.str12, 11

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"Storable::Cxt::DESTROY"
	.size	.L.str13, 23

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"Storable::init_perinterp"
	.size	.L.str14, 25

	.type	.L.str15,@object        # @.str15
.L.str15:
	.zero	1
	.size	.L.str15, 1

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"Storable::pstore"
	.size	.L.str16, 17

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"$$"
	.size	.L.str17, 3

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"Storable::net_pstore"
	.size	.L.str18, 21

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"Storable::mstore"
	.size	.L.str19, 17

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"$"
	.size	.L.str20, 2

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"Storable::net_mstore"
	.size	.L.str21, 21

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"Storable::pretrieve"
	.size	.L.str22, 20

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"Storable::mretrieve"
	.size	.L.str23, 20

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"Storable::dclone"
	.size	.L.str24, 17

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"Storable::last_op_in_netorder"
	.size	.L.str25, 30

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"Storable::is_storing"
	.size	.L.str26, 21

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"Storable::is_retrieving"
	.size	.L.str27, 24

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"Storable::drop_utf8"
	.size	.L.str28, 20

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"Storable::Cxt"
	.size	.L.str29, 14

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"Frozen string corrupt - contains characters outside 0-255"
	.size	.L.str30, 58

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"Not a scalar string"
	.size	.L.str31, 20

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"Magic number checking on storable %s failed"
	.size	.L.str32, 44

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"file"
	.size	.L.str33, 5

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"string"
	.size	.L.str34, 7

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"Old tag 0x%lx should have been mapped already"
	.size	.L.str35, 46

	.type	.L.str36,@object        # @.str36
.L.str36:
	.asciz	"Object #%ld should have been retrieved already"
	.size	.L.str36, 47

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"Storable::accept_future_minor"
	.size	.L.str37, 30

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"Storable binary image v%d.%d contains data of type %d. This Storable is v%d.%d and can only handle data types up to %d"
	.size	.L.str38, 119

	.type	sv_old_retrieve,@object # @sv_old_retrieve
	.data
	.align	16
sv_old_retrieve:
	.quad	0
	.quad	retrieve_lscalar
	.quad	old_retrieve_array
	.quad	old_retrieve_hash
	.quad	retrieve_ref
	.quad	retrieve_undef
	.quad	retrieve_integer
	.quad	retrieve_double
	.quad	retrieve_byte
	.quad	retrieve_netint
	.quad	retrieve_scalar
	.quad	retrieve_tied_array
	.quad	retrieve_tied_hash
	.quad	retrieve_tied_scalar
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.quad	retrieve_other
	.size	sv_old_retrieve, 224

	.type	.L.str39,@object        # @.str39
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str39:
	.asciz	"Corrupted storable %s (binary v%d.%d), current is v%d.%d"
	.size	.L.str39, 57

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"Corrupted storable %s (binary v%d.%d)"
	.size	.L.str40, 38

	.type	old_retrieve_hash.sv_h_undef,@object # @old_retrieve_hash.sv_h_undef
	.local	old_retrieve_hash.sv_h_undef
	.comm	old_retrieve_hash.sv_h_undef,8,8
	.type	magicstr,@object        # @magicstr
	.section	.rodata,"a",@progbits
magicstr:
	.asciz	"pst0"
	.size	magicstr, 5

	.type	old_magicstr,@object    # @old_magicstr
old_magicstr:
	.asciz	"perl-store"
	.size	old_magicstr, 11

	.type	.L.str41,@object        # @.str41
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str41:
	.asciz	"File is not a perl storable"
	.size	.L.str41, 28

	.type	sv_retrieve,@object     # @sv_retrieve
	.data
	.align	16
sv_retrieve:
	.quad	0
	.quad	retrieve_lscalar
	.quad	retrieve_array
	.quad	retrieve_hash
	.quad	retrieve_ref
	.quad	retrieve_undef
	.quad	retrieve_integer
	.quad	retrieve_double
	.quad	retrieve_byte
	.quad	retrieve_netint
	.quad	retrieve_scalar
	.quad	retrieve_tied_array
	.quad	retrieve_tied_hash
	.quad	retrieve_tied_scalar
	.quad	retrieve_sv_undef
	.quad	retrieve_sv_yes
	.quad	retrieve_sv_no
	.quad	retrieve_blessed
	.quad	retrieve_idx_blessed
	.quad	retrieve_hook
	.quad	retrieve_overloaded
	.quad	retrieve_tied_key
	.quad	retrieve_tied_idx
	.quad	retrieve_utf8str
	.quad	retrieve_lutf8str
	.quad	retrieve_flag_hash
	.quad	retrieve_code
	.quad	retrieve_other
	.size	sv_retrieve, 224

	.type	.L.str42,@object        # @.str42
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str42:
	.asciz	"Storable binary image v%d.%d more recent than I am (v%d.%d)"
	.size	.L.str42, 60

	.type	byteorderstr,@object    # @byteorderstr
	.section	.rodata,"a",@progbits
byteorderstr:
	.asciz	"1234"
	.size	byteorderstr, 5

	.type	.L.str43,@object        # @.str43
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str43:
	.asciz	"Byte order is not compatible"
	.size	.L.str43, 29

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"Integer size is not compatible"
	.size	.L.str44, 31

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"Long integer size is not compatible"
	.size	.L.str45, 36

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"Pointer size is not compatible"
	.size	.L.str46, 31

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"Double size is not compatible"
	.size	.L.str47, 30

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"Unexpected type %d in retrieve_code\n"
	.size	.L.str48, 37

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"sub "
	.size	.L.str49, 5

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"Storable::Eval"
	.size	.L.str50, 15

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"Storable::forgive_me"
	.size	.L.str51, 21

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"Can't eval, please set $Storable::Eval to a true value"
	.size	.L.str52, 55

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"@"
	.size	.L.str53, 2

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"Unexpected return value from $Storable::Eval callback\n"
	.size	.L.str54, 55

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"code %s caused an error: %s"
	.size	.L.str55, 28

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"code %s did not evaluate to a subroutine reference\n"
	.size	.L.str56, 52

	.type	.L.str57,@object        # @.str57
.L.str57:
	.asciz	"Cannot restore overloading on %s(0x%lx) (package <unknown>)"
	.size	.L.str57, 60

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"require "
	.size	.L.str58, 9

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"Cannot restore overloading on %s(0x%lx) (package %s) (even after a \"require %s;\")"
	.size	.L.str59, 82

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"Class name #%ld should have been seen already"
	.size	.L.str60, 46

	.type	.L.str61,@object        # @.str61
.L.str61:
	.asciz	"STORABLE_thaw"
	.size	.L.str61, 14

	.type	.L.str62,@object        # @.str62
.L.str62:
	.asciz	"No STORABLE_thaw defined for objects of class %s (even after a \"require %s;\")"
	.size	.L.str62, 78

	.type	.L.str63,@object        # @.str63
.L.str63:
	.asciz	"Forgot to deal with extra type %d"
	.size	.L.str63, 34

	.type	.L.str64,@object        # @.str64
.L.str64:
	.asciz	"Not a reference"
	.size	.L.str64, 16

	.type	sv_store,@object        # @sv_store
	.section	.rodata,"a",@progbits
	.align	16
sv_store:
	.quad	store_ref
	.quad	store_scalar
	.quad	store_array
	.quad	store_hash
	.quad	store_tied
	.quad	store_tied_item
	.quad	store_code
	.quad	store_other
	.size	sv_store, 64

	.type	store_other.buf,@object # @store_other.buf
	.local	store_other.buf
	.comm	store_other.buf,80,16
	.type	.L.str65,@object        # @.str65
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str65:
	.asciz	"Can't store %s items"
	.size	.L.str65, 21

	.type	.L.str66,@object        # @.str66
.L.str66:
	.asciz	"Can't store item %s(0x%lx)"
	.size	.L.str66, 27

	.type	.L.str67,@object        # @.str67
.L.str67:
	.asciz	"You lost %s(0x%lx)%c"
	.size	.L.str67, 21

	.type	.L.str68,@object        # @.str68
.L.str68:
	.asciz	"Storable::Deparse"
	.size	.L.str68, 18

	.type	.L.str69,@object        # @.str69
.L.str69:
	.asciz	"B::Deparse"
	.size	.L.str69, 11

	.type	.L.str70,@object        # @.str70
.L.str70:
	.asciz	"new"
	.size	.L.str70, 4

	.type	.L.str71,@object        # @.str71
.L.str71:
	.asciz	"Unexpected return value from B::Deparse::new\n"
	.size	.L.str71, 46

	.type	.L.str72,@object        # @.str72
.L.str72:
	.asciz	"coderef2text"
	.size	.L.str72, 13

	.type	.L.str73,@object        # @.str73
.L.str73:
	.asciz	"Unexpected return value from B::Deparse::coderef2text\n"
	.size	.L.str73, 55

	.type	.L.str74,@object        # @.str74
.L.str74:
	.asciz	"The result of B::Deparse::coderef2text was empty - maybe you're trying to serialize an XS function?\n"
	.size	.L.str74, 101

	.type	.L.str75,@object        # @.str75
.L.str75:
	.asciz	"No magic 'p' found while storing reference to tied item"
	.size	.L.str75, 56

	.type	.L.str76,@object        # @.str76
.L.str76:
	.asciz	"No magic '%c' found while storing tied %s"
	.size	.L.str76, 42

	.type	.L.str77,@object        # @.str77
.L.str77:
	.asciz	"hash"
	.size	.L.str77, 5

	.type	.L.str78,@object        # @.str78
.L.str78:
	.asciz	"array"
	.size	.L.str78, 6

	.type	.L.str79,@object        # @.str79
.L.str79:
	.asciz	"scalar"
	.size	.L.str79, 7

	.type	.L.str80,@object        # @.str80
.L.str80:
	.asciz	"Storable::canonical"
	.size	.L.str80, 20

	.type	.L.str81,@object        # @.str81
.L.str81:
	.asciz	"Can't determine type of %s(0x%lx)"
	.size	.L.str81, 34

	.type	.L.str82,@object        # @.str82
.L.str82:
	.asciz	"STORABLE_freeze"
	.size	.L.str82, 16

	.type	.L.str83,@object        # @.str83
.L.str83:
	.asciz	"Unable to record new classname"
	.size	.L.str83, 31

	.type	.L.str84,@object        # @.str84
.L.str84:
	.asciz	"Unexpected object type (%d) in store_hook()"
	.size	.L.str84, 44

	.type	.L.str85,@object        # @.str85
.L.str85:
	.asciz	"Too late to ignore hooks for %s class \"%s\""
	.size	.L.str85, 43

	.type	.L.str86,@object        # @.str86
.L.str86:
	.asciz	"cloning"
	.size	.L.str86, 8

	.type	.L.str87,@object        # @.str87
.L.str87:
	.asciz	"storing"
	.size	.L.str87, 8

	.type	.L.str88,@object        # @.str88
.L.str88:
	.asciz	"Item #%d returned by STORABLE_freeze for %s is not a reference"
	.size	.L.str88, 63

	.type	.L.str89,@object        # @.str89
.L.str89:
	.asciz	"Could not serialize item #%d from hook in %s"
	.size	.L.str89, 45

	.type	.L.str90,@object        # @.str90
.L.str90:
	.asciz	"No magic '%c' found while storing ref to tied %s with hook"
	.size	.L.str90, 59

	.type	magic_write.network_file_header,@object # @magic_write.network_file_header
	.section	.rodata,"a",@progbits
magic_write.network_file_header:
	.ascii	"pst0\005\006"
	.size	magic_write.network_file_header, 6

	.type	magic_write.file_header,@object # @magic_write.file_header
magic_write.file_header:
	.ascii	"pst0\004\006\0041234\004\b\b\b"
	.size	magic_write.file_header, 15


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
