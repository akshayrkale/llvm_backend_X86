	.file	"io_helpers.c.bc"
	.text
	.globl	save_lattice
	.align	16, 0x90
	.type	save_lattice,@function
save_lattice:                           # @save_lattice
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
	subq	$16, %rsp
.Ltmp11:
	.cfi_def_cfa_offset 64
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
	movq	%rsi, %r12
	movq	%rdi, %rbx
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	dclock
	movq	%rax, %r14
	leaq	-20(%rbx), %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$11, %rax
	ja	.LBB0_8
# BB#1:                                 # %entry
	jmpq	*.LJTI0_0(,%rax,8)
.LBB0_2:                                # %sw.bb1
	movq	%r12, %rdi
	callq	save_ascii
	movq	%rax, %r15
	jmp	.LBB0_9
.LBB0_8:                                # %sw.default
	movabsq	$.Lstr, %rdi
	callq	puts
	movq	$1, %rdi
	callq	terminate
                                        # implicit-def: R15
.LBB0_9:                                # %sw.epilog
	xorq	%rax, %rax
	callq	dclock
	andq	%r13, %rbx
	cmpq	$20, %rbx
	je	.LBB0_12
# BB#10:                                # %sw.epilog
	movl	this_node(%rip), %ecx
	testq	%r13, %rcx
	jne	.LBB0_12
# BB#11:                                # %if.then16
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_sub
	movq	%rax, %rcx
	movabsq	$.L.str1, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
.LBB0_12:                               # %if.end18
	leaq	8(%rsp), %rdi
	leaq	(%rsp), %rsi
	callq	d_plaquette
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_14
# BB#13:                                # %if.then20
	movq	(%rsp), %rdx
	movq	8(%rsp), %rsi
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	printf
.LBB0_14:                               # %if.end22
	movq	%r15, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB0_3:                                # %sw.bb3
	movq	%r12, %rdi
	callq	save_serial
	movq	%rax, %r15
	jmp	.LBB0_9
.LBB0_5:                                # %sw.bb7
	movq	%r12, %rdi
	callq	save_checkpoint
	movq	%rax, %r15
	jmp	.LBB0_9
.LBB0_4:                                # %sw.bb5
	movq	%r12, %rdi
	callq	save_parallel
	movq	%rax, %r15
	jmp	.LBB0_9
.LBB0_6:                                # %sw.bb9
	movq	%r12, %rdi
	callq	save_serial_archive
	movq	%rax, %r15
	jmp	.LBB0_9
.LBB0_7:                                # %sw.bb11
	movq	%r12, %rdi
	callq	save_parallel_archive
	movq	%rax, %r15
	jmp	.LBB0_9
.Ltmp17:
	.size	save_lattice, .Ltmp17-save_lattice
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI0_0:
	.quad	.LBB0_9
	.quad	.LBB0_2
	.quad	.LBB0_3
	.quad	.LBB0_5
	.quad	.LBB0_8
	.quad	.LBB0_8
	.quad	.LBB0_8
	.quad	.LBB0_8
	.quad	.LBB0_4
	.quad	.LBB0_8
	.quad	.LBB0_6
	.quad	.LBB0_7

	.text
	.globl	reload_lattice
	.align	16, 0x90
	.type	reload_lattice,@function
reload_lattice:                         # @reload_lattice
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
	movq	%rsi, %r12
	movq	%rdi, %rbx
	xorq	%rax, %rax
	xorq	%r15, %r15
	callq	dclock
	movq	%rax, %r14
	movq	%rbx, %rax
	addq	$-10, %rax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	andq	%r13, %rax
	cmpq	$9, %rax
	ja	.LBB1_6
# BB#1:                                 # %entry
	jmpq	*.LJTI1_0(,%rax,8)
.LBB1_2:                                # %sw.bb1
	callq	coldlat
	jmp	.LBB1_9
.LBB1_6:                                # %sw.default
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_8
# BB#7:                                 # %if.then
	movabsq	$.L.str3, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
.LBB1_8:                                # %if.end
	movq	$1, %rdi
	callq	terminate
                                        # implicit-def: R15
.LBB1_9:                                # %sw.epilog
	xorq	%rax, %rax
	callq	dclock
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rbx, %rcx
	cmpq	$10, %rcx
	je	.LBB1_12
# BB#10:                                # %sw.epilog
	movl	this_node(%rip), %ecx
	testq	%r13, %rcx
	jne	.LBB1_12
# BB#11:                                # %if.then14
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_sub
	movq	%rax, %rcx
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	movq	%rcx, %rsi
	callq	printf
.LBB1_12:                               # %if.end17
	leaq	24(%rsp), %rdi
	leaq	16(%rsp), %rsi
	callq	d_plaquette
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_14
# BB#13:                                # %if.then19
	movq	16(%rsp), %rdx
	movq	24(%rsp), %rsi
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	printf
	movq	stdout(%rip), %rdi
	callq	fflush
.LBB1_14:                               # %if.end22
	xorq	%rax, %rax
	callq	dclock
	callq	check_unitarity
	movq	%rax, 8(%rsp)
	leaq	8(%rsp), %rdi
	callq	g_doublemax
	xorq	%rax, %rax
	callq	dclock
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB1_16
# BB#15:                                # %if.then29
	movq	8(%rsp), %rsi
	movabsq	$.L.str5, %rdi
	xorq	%rax, %rax
	callq	printf
.LBB1_16:                               # %if.end31
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	%r15, %rax
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB1_3:                                # %sw.bb2
	movq	%r12, %rdi
	callq	restore_ascii
	movq	%rax, %r15
	jmp	.LBB1_9
.LBB1_4:                                # %sw.bb4
	movq	%r12, %rdi
	callq	restore_serial
	movq	%rax, %r15
	jmp	.LBB1_9
.LBB1_5:                                # %sw.bb6
	movq	%r12, %rdi
	callq	restore_parallel
	movq	%rax, %r15
	jmp	.LBB1_9
.Ltmp35:
	.size	reload_lattice, .Ltmp35-reload_lattice
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI1_0:
	.quad	.LBB1_9
	.quad	.LBB1_2
	.quad	.LBB1_3
	.quad	.LBB1_4
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_6
	.quad	.LBB1_5

	.text
	.globl	ask_starting_lattice
	.align	16, 0x90
	.type	ask_starting_lattice,@function
ask_starting_lattice:                   # @ask_starting_lattice
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp42:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp43:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp44:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp45:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp46:
	.cfi_def_cfa_offset 48
	subq	$256, %rsp              # imm = 0x100
.Ltmp47:
	.cfi_def_cfa_offset 304
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
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r12
	je	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.Lstr65, %rdi
	callq	puts
.LBB2_2:                                # %if.end
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	scanf
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB2_3
# BB#5:                                 # %if.end5
	movabsq	$.L.str9, %rdi
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str10, %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB2_6
# BB#8:                                 # %if.else
	movabsq	$.L.str12, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB2_9
# BB#10:                                # %if.else18
	movabsq	$.L.str13, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB2_11
# BB#12:                                # %if.else23
	movabsq	$.L.str14, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB2_13
# BB#14:                                # %if.else28
	movabsq	$.L.str15, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB2_15
# BB#24:                                # %if.else33
	movabsq	$.L.str16, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	printf
	movq	$1, %rbx
	jmp	.LBB2_23
.LBB2_3:                                # %if.then3
	movabsq	$.Lstr64, %rdi
	jmp	.LBB2_4
.LBB2_6:                                # %if.then11
	movq	$11, %rax
	jmp	.LBB2_7
.LBB2_9:                                # %if.then16
	movq	$10, %rax
.LBB2_7:                                # %if.end40
	movl	%eax, (%r15)
	movq	$10, %rdi
	callq	putchar
	jmp	.LBB2_17
.LBB2_11:                               # %if.then22
	movq	$12, %rax
	jmp	.LBB2_16
.LBB2_13:                               # %if.then27
	movq	$13, %rax
	jmp	.LBB2_16
.LBB2_15:                               # %if.then32
	movq	$19, %rax
.LBB2_16:                               # %if.end40
	movl	%eax, (%r15)
.LBB2_17:                               # %if.end40
	movl	(%r15), %eax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	andq	%rax, %rcx
	xorq	%rbx, %rbx
	cmpq	$10, %rcx
	je	.LBB2_23
# BB#18:                                # %if.then43
	testq	%r13, %r12
	je	.LBB2_20
# BB#19:                                # %if.then45
	movabsq	$.Lstr63, %rdi
	callq	puts
.LBB2_20:                               # %if.end47
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rsi
	callq	scanf
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB2_21
# BB#22:                                # %if.end52
	movq	%r14, %rdi
	callq	puts
	jmp	.LBB2_23
.LBB2_21:                               # %if.then50
	movabsq	$.Lstr62, %rdi
.LBB2_4:                                # %cleanup
	callq	puts
	movq	$1, %rbx
.LBB2_23:                               # %cleanup
	movq	%rbx, %rax
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp53:
	.size	ask_starting_lattice, .Ltmp53-ask_starting_lattice
	.cfi_endproc

	.globl	ask_ending_lattice
	.align	16, 0x90
	.type	ask_ending_lattice,@function
ask_ending_lattice:                     # @ask_ending_lattice
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp60:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp61:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp62:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp63:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp64:
	.cfi_def_cfa_offset 48
	subq	$256, %rsp              # imm = 0x100
.Ltmp65:
	.cfi_def_cfa_offset 304
.Ltmp66:
	.cfi_offset %rbx, -48
.Ltmp67:
	.cfi_offset %r12, -40
.Ltmp68:
	.cfi_offset %r13, -32
.Ltmp69:
	.cfi_offset %r14, -24
.Ltmp70:
	.cfi_offset %r15, -16
	movq	%rdx, %r14
	movq	%rsi, %r15
	movq	%rdi, %r12
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	testq	%r13, %r12
	je	.LBB3_2
# BB#1:                                 # %if.then
	movabsq	$.Lstr69, %rdi
	callq	puts
.LBB3_2:                                # %if.end
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	scanf
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB3_3
# BB#5:                                 # %if.end5
	movabsq	$.L.str9, %rdi
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str22, %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_6
# BB#7:                                 # %if.else
	movabsq	$.L.str23, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_8
# BB#9:                                 # %if.else16
	movabsq	$.L.str24, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_10
# BB#11:                                # %if.else21
	movabsq	$.L.str25, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_12
# BB#13:                                # %if.else26
	movabsq	$.L.str26, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_14
# BB#15:                                # %if.else31
	movabsq	$.L.str27, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_16
# BB#17:                                # %if.else36
	movabsq	$.L.str28, %rdi
	leaq	(%rsp), %rsi
	callq	strcmp
	testq	%r13, %rax
	je	.LBB3_19
# BB#18:                                # %if.else42
	movabsq	$.L.str29, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	printf
	movq	$1, %rbx
	jmp	.LBB3_25
.LBB3_3:                                # %if.then3
	movabsq	$.Lstr68, %rdi
	jmp	.LBB3_4
.LBB3_6:                                # %if.then11
	movq	$21, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_8:                                # %if.then15
	movq	$22, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_10:                               # %if.then20
	movq	$28, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_12:                               # %if.then25
	movq	$23, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_14:                               # %if.then30
	movq	$30, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_16:                               # %if.then35
	movq	$31, %rax
	movl	%eax, (%r15)
	jmp	.LBB3_20
.LBB3_19:                               # %if.end51
	movq	$20, %rax
	movl	%eax, (%r15)
	movq	$10, %rdi
	callq	putchar
	movl	(%r15), %eax
	xorq	%rbx, %rbx
	cmpq	$20, %rax
	je	.LBB3_25
.LBB3_20:                               # %if.then53
	testq	%r13, %r12
	je	.LBB3_22
# BB#21:                                # %if.then55
	movabsq	$.Lstr67, %rdi
	callq	puts
.LBB3_22:                               # %if.end57
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%r14, %rsi
	callq	scanf
	andq	%r13, %rax
	cmpq	$1, %rax
	jne	.LBB3_23
# BB#24:                                # %if.end62
	movq	%r14, %rdi
	callq	puts
	jmp	.LBB3_25
.LBB3_23:                               # %if.then60
	movabsq	$.Lstr66, %rdi
.LBB3_4:                                # %cleanup
	callq	puts
	movq	$1, %rbx
.LBB3_25:                               # %cleanup
	movq	%rbx, %rax
	addq	$256, %rsp              # imm = 0x100
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp71:
	.size	ask_ending_lattice, .Ltmp71-ask_ending_lattice
	.cfi_endproc

	.globl	coldlat
	.align	16, 0x90
	.type	coldlat,@function
coldlat:                                # @coldlat
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp78:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp79:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp80:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp81:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp82:
	.cfi_def_cfa_offset 48
	subq	$48, %rsp
.Ltmp83:
	.cfi_def_cfa_offset 96
.Ltmp84:
	.cfi_offset %rbx, -48
.Ltmp85:
	.cfi_offset %r12, -40
.Ltmp86:
	.cfi_offset %r13, -32
.Ltmp87:
	.cfi_offset %r14, -24
.Ltmp88:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB4_12
# BB#1:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %rax
	movq	%rax, 32(%rsp)          # 8-byte Spill
	leaq	152(%rax), %rcx
	xorq	%rsi, %rsi
	movabsq	$4294967295, %r12       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB4_2:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB4_3 Depth 2
                                        #       Child Loop BB4_4 Depth 3
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %rax
	movq	$0, 40(%rsp)            # 8-byte Folded Spill
	.align	16, 0x90
.LBB4_3:                                # %for.cond4.preheader
                                        #   Parent Loop BB4_2 Depth=1
                                        # =>  This Loop Header: Depth=2
                                        #       Child Loop BB4_4 Depth 3
	movq	%rax, 24(%rsp)          # 8-byte Spill
	xorq	%r15, %r15
	movq	%rax, %r14
	.align	16, 0x90
.LBB4_4:                                # %for.cond7.preheader
                                        #   Parent Loop BB4_2 Depth=1
                                        #     Parent Loop BB4_3 Depth=2
                                        # =>    This Inner Loop Header: Depth=3
	testq	%r12, %r15
	jne	.LBB4_8
# BB#5:                                 # %for.inc.thread
                                        #   in Loop: Header=BB4_4 Depth=3
	xorq	%rsi, %rsi
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	callq	cmplx
	movq	%rax, -40(%r14)
	movq	%rdx, -32(%r14)
	leaq	(%r15,%r15,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rcx,8), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rdx
	leaq	128(%rax,%rdx), %r13
	jmp	.LBB4_6
	.align	16, 0x90
.LBB4_8:                                # %for.inc
                                        #   in Loop: Header=BB4_4 Depth=3
	movq	%r15, %rbx
	andq	%r12, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	cmpq	$1, %rbx
	movq	%rax, -40(%r14)
	movq	%rdx, -32(%r14)
	jne	.LBB4_9
# BB#14:                                # %for.inc.1.thread
                                        #   in Loop: Header=BB4_4 Depth=3
	xorq	%rsi, %rsi
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	callq	cmplx
	movq	%rax, -24(%r14)
	movq	%rdx, -16(%r14)
	leaq	(%r15,%r15,2), %rax
	movq	$4, %rcx
	shlq	%cl, %rax
	movq	40(%rsp), %rcx          # 8-byte Reload
	leaq	(%rcx,%rcx,8), %rdx
	movq	$4, %rcx
	shlq	%cl, %rdx
	movq	32(%rsp), %rcx          # 8-byte Reload
	addq	%rcx, %rdx
	leaq	144(%rax,%rdx), %rbx
	jmp	.LBB4_15
	.align	16, 0x90
.LBB4_9:                                #   in Loop: Header=BB4_4 Depth=3
	leaq	-24(%r14), %r13
.LBB4_6:                                # %for.inc.1
                                        #   in Loop: Header=BB4_4 Depth=3
	movq	%r15, %rbx
	andq	%r12, %rbx
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	cmpq	$2, %rbx
	movq	%rax, (%r13)
	movq	%rdx, -16(%r14)
	jne	.LBB4_7
# BB#16:                                # %if.else.2
                                        #   in Loop: Header=BB4_4 Depth=3
	xorq	%rsi, %rsi
	movabsq	$4607182418800017408, %rdi # imm = 0x3FF0000000000000
	callq	cmplx
	movq	%rax, -8(%r14)
	jmp	.LBB4_17
	.align	16, 0x90
.LBB4_7:                                #   in Loop: Header=BB4_4 Depth=3
	leaq	-8(%r14), %rbx
.LBB4_15:                               # %if.then.2
                                        #   in Loop: Header=BB4_4 Depth=3
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, (%rbx)
.LBB4_17:                               # %for.inc.2
                                        #   in Loop: Header=BB4_4 Depth=3
	movq	%rdx, (%r14)
	incq	%r15
	addq	$48, %r14
	cmpq	$3, %r15
	jne	.LBB4_4
# BB#10:                                # %for.inc28
                                        #   in Loop: Header=BB4_3 Depth=2
	movq	40(%rsp), %rax          # 8-byte Reload
	movq	%rax, %rcx
	incq	%rcx
	movq	%rcx, 40(%rsp)          # 8-byte Spill
	movq	24(%rsp), %rax          # 8-byte Reload
	addq	$144, %rax
	cmpq	$4, %rcx
	jne	.LBB4_3
# BB#11:                                # %for.inc31
                                        #   in Loop: Header=BB4_2 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	sites_on_node(%rip), %rdx
	sarq	%cl, %rax
	addq	$3200, 32(%rsp)         # 8-byte Folded Spill
                                        # imm = 0xC80
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	$3200, %rcx             # imm = 0xC80
	cmpq	%rdx, %rax
	jl	.LBB4_2
.LBB4_12:                               # %for.end33
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	je	.LBB4_18
# BB#13:                                # %if.end37
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB4_18:                               # %if.then35
	movabsq	$.Lstr70, %rdi
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	jmp	puts  # TAILCALL
.Ltmp89:
	.size	coldlat, .Ltmp89-coldlat
	.cfi_endproc

	.globl	funnylat
	.align	16, 0x90
	.type	funnylat,@function
funnylat:                               # @funnylat
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
	subq	$32, %rsp
.Ltmp101:
	.cfi_def_cfa_offset 80
.Ltmp102:
	.cfi_offset %rbx, -48
.Ltmp103:
	.cfi_offset %r12, -40
.Ltmp104:
	.cfi_offset %r13, -32
.Ltmp105:
	.cfi_offset %r14, -24
.Ltmp106:
	.cfi_offset %r15, -16
	movslq	sites_on_node(%rip), %rax
	testq	%rax, %rax
	jle	.LBB5_5
# BB#1:                                 # %for.cond1.preheader.lr.ph
	movq	lattice(%rip), %rdi
	leaq	248(%rdi), %rcx
	xorq	%rsi, %rsi
	movabsq	$4294967292, %rdx       # imm = 0xFFFFFFFC
	.align	16, 0x90
.LBB5_2:                                # %for.cond1.preheader
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB5_3 Depth 2
	movq	%rsi, 8(%rsp)           # 8-byte Spill
	movq	%rcx, 16(%rsp)          # 8-byte Spill
	movq	%rcx, %r13
	xorq	%rbx, %rbx
	movq	%rdx, %r14
	movq	%rdi, %r12
	.align	16, 0x90
.LBB5_3:                                # %for.cond4.preheader
                                        #   Parent Loop BB5_2 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movq	%rbx, 24(%rsp)          # 8-byte Spill
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -136(%r13)
	movq	%rdx, -128(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -120(%r13)
	movq	%rdx, -112(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -104(%r13)
	movq	%rdx, -96(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -88(%r13)
	movq	%rdx, -80(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -72(%r13)
	movq	%rdx, -64(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -56(%r13)
	movq	%rdx, -48(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -40(%r13)
	movq	%rdx, -32(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -24(%r13)
	movq	%rdx, -16(%r13)
	xorq	%rdi, %rdi
	xorq	%rsi, %rsi
	callq	cmplx
	movq	%rax, -8(%r13)
	movq	%rbx, %rdi
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	andq	%rax, %rdi
	callq	int32_to_float64
	movq	%rax, %rbx
	movq	%rbx, -136(%r13)
	movswq	(%r12), %rax
	addq	%rax, %rax
	leaq	(%rax,%rax,4), %rdi
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	movq	%rax, %r15
	andq	%r15, %rdi
	callq	int32_to_float64
	movq	%rax, -72(%r13)
	movswq	2(%r12), %rax
	imulq	$100, %rax, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movq	%rax, -8(%r13)
	movq	%rbx, -128(%r13)
	movq	24(%rsp), %rbx          # 8-byte Reload
	movswq	4(%r12), %rax
	addq	%rax, %rax
	leaq	(%rax,%rax,4), %rdi
	andq	%r15, %rdi
	callq	int32_to_float64
	movq	%rax, -64(%r13)
	movswq	6(%r12), %rax
	imulq	$100, %rax, %rdi
	andq	%r14, %rdi
	callq	int32_to_float64
	movq	%rax, (%r13)
	incq	%rbx
	addq	$144, %r13
	cmpq	$4, %rbx
	jne	.LBB5_3
# BB#4:                                 # %for.inc72
                                        #   in Loop: Header=BB5_2 Depth=1
	movq	8(%rsp), %rsi           # 8-byte Reload
	incq	%rsi
	movq	$32, %rcx
	movq	%rsi, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	sites_on_node(%rip), %rdx
	sarq	%cl, %rax
	addq	$3200, %r12             # imm = 0xC80
	movq	16(%rsp), %rcx          # 8-byte Reload
	addq	$3200, %rcx             # imm = 0xC80
	cmpq	%rdx, %rax
	movq	%r14, %rdx
	movq	%r12, %rdi
	jl	.LBB5_2
.LBB5_5:                                # %for.end74
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp107:
	.size	funnylat, .Ltmp107-funnylat
	.cfi_endproc

	.globl	get_f
	.align	16, 0x90
	.type	get_f,@function
get_f:                                  # @get_f
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
	subq	$80, %rsp
.Ltmp119:
	.cfi_def_cfa_offset 128
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
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdi
	je	.LBB6_5
# BB#1:                                 # %while.body.preheader
	movabsq	$.L.str33, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str7, %rdi
	movq	%rbx, %r13
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	scanf
	movabsq	$.L.str34, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	sscanf
	andq	%r15, %rax
	cmpq	$1, %rax
	movq	%r14, %rbx
	je	.LBB6_4
# BB#2:
	leaq	(%rsp), %r14
	movabsq	$.L.str34, %r12
	.align	16, 0x90
.LBB6_3:                                # %if.else
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$.Lstr73, %rdi
	callq	puts
	xorq	%rax, %rax
	movabsq	$.L.str33, %rdi
	movq	%r13, %rsi
	callq	printf
	xorq	%rax, %rax
	movabsq	$.L.str7, %rdi
	movq	%r14, %rsi
	callq	scanf
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	sscanf
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB6_3
.LBB6_4:                                # %if.then5
	movq	(%rbx), %rdx
	movabsq	$.L.str35, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	printf
	xorq	%rbx, %rbx
	jmp	.LBB6_17
.LBB6_5:                                # %if.else8
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	scanf
	testq	%r15, %rax
	jne	.LBB6_6
# BB#9:                                 # %if.then16
	movabsq	$.L.str38, %rdi
	jmp	.LBB6_8
.LBB6_6:                                # %if.else8
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB6_10
# BB#7:                                 # %if.then12
	movabsq	$.L.str37, %rdi
.LBB6_8:                                # %cleanup
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movq	$1, %rbx
.LBB6_17:                               # %cleanup
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB6_10:                               # %if.else18
	leaq	(%rsp), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB6_12
# BB#11:                                # %if.then22
	movabsq	$.L.str39, %rdi
	leaq	(%rsp), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movq	$1, %rbx
	jmp	.LBB6_17
.LBB6_12:                               # %if.end27
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str34, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	scanf
	testq	%r15, %rax
	jne	.LBB6_13
# BB#15:                                # %if.then35
	movabsq	$.L.str41, %rdi
	jmp	.LBB6_8
.LBB6_13:                               # %if.end27
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB6_16
# BB#14:                                # %if.then31
	movabsq	$.L.str40, %rdi
	jmp	.LBB6_8
.LBB6_16:                               # %if.end38
	movq	(%r14), %rsi
	movabsq	$.L.str42, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	printf
	jmp	.LBB6_17
.Ltmp125:
	.size	get_f, .Ltmp125-get_f
	.cfi_endproc

	.globl	get_i
	.align	16, 0x90
	.type	get_i,@function
get_i:                                  # @get_i
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp132:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp133:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp134:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp135:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp136:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp137:
	.cfi_def_cfa_offset 128
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
	movq	%rdx, %r14
	movq	%rsi, %rbx
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	testq	%r15, %rdi
	je	.LBB7_5
# BB#1:                                 # %while.body.preheader
	movabsq	$.L.str33, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str7, %rdi
	movq	%rbx, %r13
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	scanf
	movabsq	$.L.str43, %rsi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%r14, %rdx
	callq	sscanf
	andq	%r15, %rax
	cmpq	$1, %rax
	movq	%r14, %rbx
	je	.LBB7_4
# BB#2:
	leaq	(%rsp), %r14
	movabsq	$.L.str43, %r12
	.align	16, 0x90
.LBB7_3:                                # %if.else
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$.Lstr73, %rdi
	callq	puts
	xorq	%rax, %rax
	movabsq	$.L.str33, %rdi
	movq	%r13, %rsi
	callq	printf
	xorq	%rax, %rax
	movabsq	$.L.str7, %rdi
	movq	%r14, %rsi
	callq	scanf
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	movq	%rbx, %rdx
	callq	sscanf
	andq	%r15, %rax
	cmpq	$1, %rax
	jne	.LBB7_3
.LBB7_4:                                # %if.then5
	movl	(%rbx), %edx
	movabsq	$.L.str44, %rdi
	xorq	%rax, %rax
	movq	%r13, %rsi
	callq	printf
	xorq	%rbx, %rbx
	jmp	.LBB7_17
.LBB7_5:                                # %if.else8
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	scanf
	testq	%r15, %rax
	jne	.LBB7_6
# BB#9:                                 # %if.then16
	movabsq	$.L.str46, %rdi
	jmp	.LBB7_8
.LBB7_6:                                # %if.else8
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB7_10
# BB#7:                                 # %if.then12
	movabsq	$.L.str45, %rdi
.LBB7_8:                                # %cleanup
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movq	$1, %rbx
.LBB7_17:                               # %cleanup
	movq	%rbx, %rax
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB7_10:                               # %if.else18
	leaq	(%rsp), %rdi
	movq	%rbx, %rsi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB7_12
# BB#11:                                # %if.then22
	movabsq	$.L.str47, %rdi
	leaq	(%rsp), %rdx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movq	$1, %rbx
	jmp	.LBB7_17
.LBB7_12:                               # %if.end27
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	printf
	movabsq	$.L.str43, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	scanf
	testq	%r15, %rax
	jne	.LBB7_13
# BB#15:                                # %if.then35
	movabsq	$.L.str49, %rdi
	jmp	.LBB7_8
.LBB7_13:                               # %if.end27
	andq	%r15, %rax
	cmpq	%r15, %rax
	jne	.LBB7_16
# BB#14:                                # %if.then31
	movabsq	$.L.str48, %rdi
	jmp	.LBB7_8
.LBB7_16:                               # %if.end38
	movl	(%r14), %esi
	movabsq	$.L.str50, %rdi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	printf
	jmp	.LBB7_17
.Ltmp143:
	.size	get_i, .Ltmp143-get_i
	.cfi_endproc

	.globl	get_s
	.align	16, 0x90
	.type	get_s,@function
get_s:                                  # @get_s
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp150:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp151:
	.cfi_def_cfa_offset 24
	pushq	%r13
.Ltmp152:
	.cfi_def_cfa_offset 32
	pushq	%r12
.Ltmp153:
	.cfi_def_cfa_offset 40
	pushq	%rbx
.Ltmp154:
	.cfi_def_cfa_offset 48
	subq	$80, %rsp
.Ltmp155:
	.cfi_def_cfa_offset 128
.Ltmp156:
	.cfi_offset %rbx, -48
.Ltmp157:
	.cfi_offset %r12, -40
.Ltmp158:
	.cfi_offset %r13, -32
.Ltmp159:
	.cfi_offset %r14, -24
.Ltmp160:
	.cfi_offset %r15, -16
	movq	%rdx, %r15
	movq	%rsi, %r14
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rdi
	je	.LBB8_5
# BB#1:                                 # %while.body.preheader
	movabsq	$.L.str33, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	printf
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	scanf
	andq	%rbx, %rax
	cmpq	$1, %rax
	movq	%r14, %r12
	movq	%r15, %r13
	je	.LBB8_4
# BB#2:
	movabsq	$.L.str33, %r14
	movabsq	$.L.str7, %r15
	.align	16, 0x90
.LBB8_3:                                # %if.else
                                        # =>This Inner Loop Header: Depth=1
	movabsq	$.Lstr73, %rdi
	callq	puts
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%r12, %rsi
	callq	printf
	xorq	%rax, %rax
	movq	%r15, %rdi
	movq	%r13, %rsi
	callq	scanf
	andq	%rbx, %rax
	cmpq	$1, %rax
	jne	.LBB8_3
.LBB8_4:                                # %if.then3
	movabsq	$.L.str51, %rdi
	xorq	%rax, %rax
	movq	%r12, %rsi
	movq	%r13, %rdx
	callq	printf
.LBB8_17:                               # %cleanup
	xorq	%rax, %rax
	jmp	.LBB8_18
.LBB8_5:                                # %if.else6
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rsi
	xorq	%rax, %rax
	callq	scanf
	testq	%rbx, %rax
	jne	.LBB8_6
# BB#9:                                 # %if.then13
	movabsq	$.L.str53, %rdi
	jmp	.LBB8_8
.LBB8_6:                                # %if.else6
	andq	%rbx, %rax
	cmpq	%rbx, %rax
	jne	.LBB8_10
# BB#7:                                 # %if.then9
	movabsq	$.L.str52, %rdi
.LBB8_8:                                # %cleanup
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	printf
	movq	$1, %rax
.LBB8_18:                               # %cleanup
	addq	$80, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.LBB8_10:                               # %if.else15
	leaq	(%rsp), %rdi
	movq	%r14, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB8_12
# BB#11:                                # %if.then19
	movabsq	$.L.str54, %rdi
	leaq	(%rsp), %rdx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	printf
	movq	$1, %rax
	jmp	.LBB8_18
.LBB8_12:                               # %if.end24
	movabsq	$.L.str9, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	printf
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	scanf
	testq	%rbx, %rax
	jne	.LBB8_13
# BB#15:                                # %if.then32
	movabsq	$.L.str56, %rdi
	jmp	.LBB8_8
.LBB8_13:                               # %if.end24
	andq	%rbx, %rax
	cmpq	%rbx, %rax
	jne	.LBB8_16
# BB#14:                                # %if.then28
	movabsq	$.L.str55, %rdi
	jmp	.LBB8_8
.LBB8_16:                               # %if.end35
	movq	%r15, %rdi
	callq	puts
	jmp	.LBB8_17
.Ltmp161:
	.size	get_s, .Ltmp161-get_s
	.cfi_endproc

	.globl	get_prompt
	.align	16, 0x90
	.type	get_prompt,@function
get_prompt:                             # @get_prompt
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp166:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp167:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp168:
	.cfi_def_cfa_offset 32
	subq	$80, %rsp
.Ltmp169:
	.cfi_def_cfa_offset 112
.Ltmp170:
	.cfi_offset %rbx, -32
.Ltmp171:
	.cfi_offset %r14, -24
.Ltmp172:
	.cfi_offset %r15, -16
	movq	%rdi, %r14
	movabsq	$4294967295, %r15       # imm = 0xFFFFFFFF
	movl	%r15d, (%r14)
	movabsq	$.Lstr74, %rdi
	callq	puts
	movabsq	$.L.str7, %rdi
	leaq	(%rsp), %rbx
	xorq	%rax, %rax
	movq	%rbx, %rsi
	callq	scanf
	movabsq	$.L.str58, %rsi
	movq	%rbx, %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB9_1
# BB#4:                                 # %if.else
	movabsq	$.L.str59, %rsi
	leaq	(%rsp), %rdi
	callq	strcmp
	testq	%r15, %rax
	je	.LBB9_5
# BB#6:                                 # %if.else9
	movabsq	$.L.str60, %rsi
	leaq	(%rsp), %rdi
	callq	strcmp
	testq	%r15, %rax
	jne	.LBB9_2
# BB#7:                                 # %if.then13
	movq	$1, %rax
	movl	%eax, (%r14)
	xorq	%rax, %rax
	jmp	.LBB9_8
.LBB9_1:                                # %if.then
	movabsq	$.L.str43, %rdi
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	scanf
.LBB9_2:                                # %if.end15
	movl	(%r14), %ecx
	xorq	%rax, %rax
	cmpq	$2, %rcx
	jb	.LBB9_8
# BB#3:                                 # %if.else19
	movabsq	$.Lstr75, %rdi
	callq	puts
	movq	$1, %rax
	jmp	.LBB9_8
.LBB9_5:                                # %if.then8
	xorq	%rax, %rax
	movl	%eax, (%r14)
.LBB9_8:                                # %cleanup
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.Ltmp173:
	.size	get_prompt, .Ltmp173-get_prompt
	.cfi_endproc

	.type	.L.str1,@object         # @.str1
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str1:
	.asciz	"Time to save = %e\n"
	.size	.L.str1, 19

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"CHECK PLAQ: %e %e\n"
	.size	.L.str2, 19

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"reload_lattice: Bad startflag %d\n"
	.size	.L.str3, 34

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Time to reload gauge configuration = %e\n"
	.size	.L.str4, 41

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Unitarity checked.  Max deviation %.2e\n"
	.size	.L.str5, 40

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"%s"
	.size	.L.str7, 3

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"%s "
	.size	.L.str9, 4

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"fresh"
	.size	.L.str10, 6

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"continue"
	.size	.L.str12, 9

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"reload_ascii"
	.size	.L.str13, 13

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"reload_serial"
	.size	.L.str14, 14

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"reload_parallel"
	.size	.L.str15, 16

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"ask_starting_lattice: ERROR IN INPUT: lattice_command %s is invalid\n"
	.size	.L.str16, 69

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"save_ascii"
	.size	.L.str22, 11

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"save_serial"
	.size	.L.str23, 12

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"save_parallel"
	.size	.L.str24, 14

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"save_checkpoint"
	.size	.L.str25, 16

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"save_serial_archive"
	.size	.L.str26, 20

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"save_parallel_archive"
	.size	.L.str27, 22

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"forget"
	.size	.L.str28, 7

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"ask_ending_lattice: ERROR IN INPUT: %s is not a save lattice command\n"
	.size	.L.str29, 70

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"enter %s "
	.size	.L.str33, 10

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"%lf"
	.size	.L.str34, 4

	.type	.L.str35,@object        # @.str35
.L.str35:
	.asciz	"%s %g\n"
	.size	.L.str35, 7

	.type	.L.str37,@object        # @.str37
.L.str37:
	.asciz	"get_f: EOF on STDIN while expecting %s.\n"
	.size	.L.str37, 41

	.type	.L.str38,@object        # @.str38
.L.str38:
	.asciz	"get_f: Format error looking for %s\n"
	.size	.L.str38, 36

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"get_f: ERROR IN INPUT: expected %s but found %s\n"
	.size	.L.str39, 49

	.type	.L.str40,@object        # @.str40
.L.str40:
	.asciz	"\nget_f: Expecting value for %s but found EOF.\n"
	.size	.L.str40, 47

	.type	.L.str41,@object        # @.str41
.L.str41:
	.asciz	"\nget_f: Format error reading value for %s\n"
	.size	.L.str41, 43

	.type	.L.str42,@object        # @.str42
.L.str42:
	.asciz	"%g\n"
	.size	.L.str42, 4

	.type	.L.str43,@object        # @.str43
.L.str43:
	.asciz	"%d"
	.size	.L.str43, 3

	.type	.L.str44,@object        # @.str44
.L.str44:
	.asciz	"%s %d\n"
	.size	.L.str44, 7

	.type	.L.str45,@object        # @.str45
.L.str45:
	.asciz	"get_i: EOF on STDIN while expecting %s.\n"
	.size	.L.str45, 41

	.type	.L.str46,@object        # @.str46
.L.str46:
	.asciz	"get_i: Format error looking for %s\n"
	.size	.L.str46, 36

	.type	.L.str47,@object        # @.str47
.L.str47:
	.asciz	"get_i: ERROR IN INPUT: expected %s but found %s\n"
	.size	.L.str47, 49

	.type	.L.str48,@object        # @.str48
.L.str48:
	.asciz	"\nget_i: Expecting value for %s but found EOF.\n"
	.size	.L.str48, 47

	.type	.L.str49,@object        # @.str49
.L.str49:
	.asciz	"\nget_i: Format error reading value for %s\n"
	.size	.L.str49, 43

	.type	.L.str50,@object        # @.str50
.L.str50:
	.asciz	"%d\n"
	.size	.L.str50, 4

	.type	.L.str51,@object        # @.str51
.L.str51:
	.asciz	"%s %s\n"
	.size	.L.str51, 7

	.type	.L.str52,@object        # @.str52
.L.str52:
	.asciz	"get_s: EOF on STDIN while expecting %s.\n"
	.size	.L.str52, 41

	.type	.L.str53,@object        # @.str53
.L.str53:
	.asciz	"get_s: Format error looking for %s\n"
	.size	.L.str53, 36

	.type	.L.str54,@object        # @.str54
.L.str54:
	.asciz	"get_s: ERROR IN INPUT: expected %s but found %s\n"
	.size	.L.str54, 49

	.type	.L.str55,@object        # @.str55
.L.str55:
	.asciz	"\nget_s: Expecting value for %s but found EOF.\n"
	.size	.L.str55, 47

	.type	.L.str56,@object        # @.str56
.L.str56:
	.asciz	"\nget_s: Format error reading value for %s\n"
	.size	.L.str56, 43

	.type	.L.str58,@object        # @.str58
.L.str58:
	.asciz	"prompt"
	.size	.L.str58, 7

	.type	.L.str59,@object        # @.str59
.L.str59:
	.asciz	"0"
	.size	.L.str59, 2

	.type	.L.str60,@object        # @.str60
.L.str60:
	.asciz	"1"
	.size	.L.str60, 2

	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"save_lattice: ERROR: unknown type for saving lattice"
	.size	.Lstr, 53

	.type	.Lstr62,@object         # @str62
	.align	16
.Lstr62:
	.asciz	"ask_starting_lattice: ERROR IN INPUT: file name read"
	.size	.Lstr62, 53

	.type	.Lstr63,@object         # @str63
	.align	16
.Lstr63:
	.asciz	"enter name of file containing lattice"
	.size	.Lstr63, 38

	.type	.Lstr64,@object         # @str64
	.align	16
.Lstr64:
	.asciz	"ask_starting_lattice: ERROR IN INPUT: starting lattice command"
	.size	.Lstr64, 63

	.type	.Lstr65,@object         # @str65
	.align	16
.Lstr65:
	.asciz	"enter 'continue', 'fresh', 'reload_ascii', 'reload_serial', or 'reload_parallel'"
	.size	.Lstr65, 81

	.type	.Lstr66,@object         # @str66
	.align	16
.Lstr66:
	.asciz	"ask_ending_lattice: ERROR IN INPUT: save filename"
	.size	.Lstr66, 50

	.type	.Lstr67,@object         # @str67
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr67:
	.asciz	"enter filename"
	.size	.Lstr67, 15

	.type	.Lstr68,@object         # @str68
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr68:
	.asciz	"ask_ending_lattice: ERROR IN INPUT: ending lattice command"
	.size	.Lstr68, 59

	.type	.Lstr69,@object         # @str69
	.align	16
.Lstr69:
	.asciz	"'forget' lattice at end,  'save_ascii', 'save_serial', 'save_parallel', 'save_checkpoint', 'save_serial_archive', or 'save_parallel_archive'"
	.size	.Lstr69, 141

	.type	.Lstr70,@object         # @str70
	.align	16
.Lstr70:
	.asciz	"unit gauge configuration loaded"
	.size	.Lstr70, 32

	.type	.Lstr73,@object         # @str73
	.align	16
.Lstr73:
	.asciz	"Data format error."
	.size	.Lstr73, 19

	.type	.Lstr74,@object         # @str74
	.align	16
.Lstr74:
	.asciz	"type 0 for no prompts  or 1 for prompts"
	.size	.Lstr74, 40

	.type	.Lstr75,@object         # @str75
	.align	16
.Lstr75:
	.asciz	"get_prompt: ERROR IN INPUT: initial prompt"
	.size	.Lstr75, 43


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
