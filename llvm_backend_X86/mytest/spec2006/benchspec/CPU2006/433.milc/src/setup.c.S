	.file	"setup.c.bc"
	.text
	.globl	setup
	.align	16, 0x90
	.type	setup,@function
setup:                                  # @setup
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
	callq	initial_set
	movl	volume(%rip), %ebx
	movl	iseed(%rip), %r15d
	movq	%rax, %r14
	xorq	%rax, %rax
	callq	mynode
	addq	%rax, %rbx
	movabsq	$node_prn, %rdi
	movq	%r15, %rsi
	movq	%rbx, %rdx
	callq	initialize_prn
	callq	setup_layout
	xorq	%rax, %rax
	callq	make_lattice
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_2
# BB#1:                                 # %if.then
	movabsq	$.Lstr30, %rdi
	callq	puts
.LBB0_2:                                # %if.end
	movq	stdout(%rip), %r15
	movq	%r15, %rdi
	callq	fflush
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	make_nn_gathers
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_4
# BB#3:                                 # %if.then5
	movabsq	$.Lstr29, %rdi
	callq	puts
.LBB0_4:                                # %if.end7
	movq	%r15, %rdi
	callq	fflush
	movl	%ebx, 4(%rsp)
	movabsq	$third_neighbor, %r12
	leaq	4(%rsp), %rbx
	.align	16, 0x90
.LBB0_5:                                # %for.body.i
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r12, %rdi
	movq	%rbx, %rsi
	callq	make_gather
	movl	4(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	movl	%eax, 4(%rsp)
	jl	.LBB0_5
# BB#6:                                 # %make_3n_gathers.exit
	movq	$8, %rdi
	callq	sort_eight_neighborlists
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_8
# BB#7:                                 # %if.then10
	movabsq	$.Lstr28, %rdi
	callq	puts
.LBB0_8:                                # %if.end12
	movq	%r15, %rdi
	callq	fflush
	xorq	%rax, %rax
	callq	phaseset
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_10
# BB#9:                                 # %if.then15
	movabsq	$.Lstr, %rdi
	callq	puts
.LBB0_10:                               # %if.end17
	movq	%r15, %rdi
	callq	fflush
	movq	%r14, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	retq
.Ltmp14:
	.size	setup, .Ltmp14-setup
	.cfi_endproc

	.globl	initial_set
	.align	16, 0x90
	.type	initial_set,@function
initial_set:                            # @initial_set
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp18:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp19:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp20:
	.cfi_def_cfa_offset 32
.Ltmp21:
	.cfi_offset %rbx, -24
.Ltmp22:
	.cfi_offset %r14, -16
	xorq	%rax, %rax
	callq	mynode
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB1_12
# BB#1:                                 # %if.then
	movabsq	$.Lstr31, %rdi
	callq	puts
	movabsq	$.Lstr32, %rdi
	callq	puts
	movabsq	$.Lstr33, %rdi
	callq	puts
	xorq	%rax, %rax
	callq	machine_type
	movq	%rax, %rbx
	xorq	%rax, %rax
	callq	numnodes
	movq	%rax, %rcx
	movabsq	$.L.str7, %rdi
	xorq	%rax, %rax
	movq	%rbx, %rsi
	movq	%rcx, %rdx
	callq	printf
	movabsq	$.Lstr34, %rdi
	callq	puts
	leaq	4(%rsp), %rdi
	callq	get_prompt
	testq	%r14, %rax
	jne	.LBB1_8
# BB#2:                                 # %if.end
	movl	4(%rsp), %edi
	movabsq	$.L.str9, %rsi
	movabsq	$par_buf+24, %rdx
	callq	get_i
	testq	%r14, %rax
	jne	.LBB1_8
# BB#3:                                 # %if.end16
	movl	4(%rsp), %edi
	movabsq	$.L.str10, %rsi
	movabsq	$par_buf+4, %rdx
	callq	get_i
	testq	%r14, %rax
	jne	.LBB1_8
# BB#4:                                 # %if.end21
	movl	4(%rsp), %edi
	movabsq	$.L.str11, %rsi
	movabsq	$par_buf+8, %rdx
	callq	get_i
	testq	%r14, %rax
	jne	.LBB1_8
# BB#5:                                 # %if.end26
	movl	4(%rsp), %edi
	movabsq	$.L.str12, %rsi
	movabsq	$par_buf+12, %rdx
	callq	get_i
	testq	%r14, %rax
	jne	.LBB1_8
# BB#6:                                 # %if.end31
	movl	4(%rsp), %edi
	movabsq	$.L.str13, %rsi
	movabsq	$par_buf+16, %rdx
	callq	get_i
	testq	%r14, %rax
	jne	.LBB1_8
# BB#7:                                 # %if.then33
	movl	4(%rsp), %edi
	movabsq	$.L.str14, %rsi
	movabsq	$par_buf+20, %rdx
	callq	get_i
.LBB1_8:                                # %if.end36
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB1_10
# BB#9:                                 # %if.then38
	movq	$1, %rax
	jmp	.LBB1_11
.LBB1_10:                               # %if.else
	xorq	%rax, %rax
.LBB1_11:                               # %if.end40
	movl	%eax, par_buf(%rip)
.LBB1_12:                               # %if.end40
	movabsq	$par_buf, %rdi
	movq	$640, %rsi              # imm = 0x280
	callq	broadcast_bytes
	movq	par_buf(%rip), %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	testq	%r14, %rdx
	je	.LBB1_14
# BB#13:                                # %if.then42
	xorq	%rdi, %rdi
	callq	normal_exit
	movl	par_buf+4(%rip), %eax
.LBB1_14:                               # %if.end43
	movq	par_buf+8(%rip), %rdx
	movl	%eax, nx(%rip)
	movl	%edx, ny(%rip)
	movq	$32, %rcx
	shrq	%cl, %rdx
	movl	%edx, nz(%rip)
	movq	par_buf+16(%rip), %rax
	movl	%eax, nt(%rip)
	movl	par_buf+24(%rip), %edx
	movq	$32, %rcx
	shrq	%cl, %rax
	movl	%eax, iseed(%rip)
	movl	%edx, nflavors(%rip)
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	callq	mynode
	movl	%eax, this_node(%rip)
	xorq	%rax, %rax
	callq	numnodes
	movl	%eax, number_of_nodes(%rip)
	movl	nx(%rip), %eax
	movl	ny(%rip), %ecx
	imulq	%rax, %rcx
	movl	nz(%rip), %edx
	movl	nt(%rip), %esi
	movl	4(%rsp), %eax
	imulq	%rcx, %rdx
	imulq	%rdx, %rsi
	movl	%esi, volume(%rip)
	movl	%ebx, total_iters(%rip)
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp23:
	.size	initial_set, .Ltmp23-initial_set
	.cfi_endproc

	.globl	readin
	.align	16, 0x90
	.type	readin,@function
readin:                                 # @readin
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp27:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp28:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp29:
	.cfi_def_cfa_offset 32
.Ltmp30:
	.cfi_offset %rbx, -24
.Ltmp31:
	.cfi_offset %r14, -16
	movl	this_node(%rip), %eax
	movq	%rdi, %rbx
	testq	%rax, %rax
	jne	.LBB2_18
# BB#1:                                 # %if.then
	movabsq	$.Lstr35, %rdi
	callq	puts
	movabsq	$.L.str16, %rsi
	movabsq	$par_buf+28, %rdx
	movq	%rbx, %rdi
	callq	get_i
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	testq	%r14, %rax
	jne	.LBB2_14
# BB#2:                                 # %if.end8
	movabsq	$.L.str17, %rsi
	movabsq	$par_buf+32, %rdx
	movq	%rbx, %rdi
	callq	get_i
	testq	%r14, %rax
	jne	.LBB2_14
# BB#3:                                 # %if.end13
	movabsq	$.L.str18, %rsi
	movabsq	$par_buf+40, %rdx
	movq	%rbx, %rdi
	callq	get_i
	testq	%r14, %rax
	jne	.LBB2_14
# BB#4:                                 # %if.end18
	movabsq	$.L.str19, %rsi
	movabsq	$par_buf+48, %rdx
	movq	%rbx, %rdi
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#5:                                 # %if.end23
	movabsq	$.L.str20, %rsi
	movabsq	$par_buf+56, %rdx
	movq	%rbx, %rdi
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#6:                                 # %if.end28
	movabsq	$.L.str21, %rsi
	movabsq	$par_buf+64, %rdx
	movq	%rbx, %rdi
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#7:                                 # %if.end33
	movabsq	$.L.str22, %rsi
	movabsq	$par_buf+96, %rdx
	movq	%rbx, %rdi
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#8:                                 # %if.end38
	movabsq	$.L.str23, %rsi
	movabsq	$par_buf+36, %rdx
	movq	%rbx, %rdi
	callq	get_i
	testq	%r14, %rax
	jne	.LBB2_14
# BB#9:                                 # %if.end43
	movabsq	$.L.str24, %rsi
	movabsq	$par_buf+72, %rdx
	movq	%rbx, %rdi
	callq	get_i
	testq	%r14, %rax
	jne	.LBB2_14
# BB#10:                                # %if.end48
	movabsq	$.L.str25, %rax
	leaq	(%rsp), %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#11:                                # %if.end56
	movq	(%rsp), %rax
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, par_buf+80(%rip)
	movabsq	$.L.str26, %rax
	leaq	(%rsp), %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	get_f
	testq	%r14, %rax
	jne	.LBB2_14
# BB#12:                                # %if.end65
	movq	(%rsp), %rax
	movq	%rax, %rdi
	movq	%rax, %rsi
	callq	float64_mul
	movq	%rax, par_buf+88(%rip)
	movabsq	$par_buf+116, %rax
	movabsq	$par_buf+124, %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	ask_starting_lattice
	testq	%r14, %rax
	jne	.LBB2_14
# BB#13:                                # %if.then67
	movabsq	$par_buf+120, %rax
	movabsq	$par_buf+380, %rcx
	movq	%rbx, %rdi
	movq	%rax, %rsi
	movq	%rcx, %rdx
	callq	ask_ending_lattice
.LBB2_14:                               # %if.end70
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB2_16
# BB#15:                                # %if.then72
	movq	$1, %rax
	jmp	.LBB2_17
.LBB2_16:                               # %if.else
	xorq	%rax, %rax
.LBB2_17:                               # %if.end74
	movl	%eax, par_buf(%rip)
.LBB2_18:                               # %if.end74
	movabsq	$par_buf, %rdi
	movq	$640, %rsi              # imm = 0x280
	callq	broadcast_bytes
	movl	par_buf(%rip), %eax
	testq	%rax, %rax
	je	.LBB2_20
# BB#19:                                # %if.then76
	xorq	%rdi, %rdi
	callq	normal_exit
.LBB2_20:                               # %if.end77
	movl	par_buf+28(%rip), %eax
	movq	par_buf+32(%rip), %rdx
	movl	%eax, warms(%rip)
	movl	%edx, trajecs(%rip)
	movq	$32, %rcx
	shrq	%cl, %rdx
	movl	%edx, steps(%rip)
	movl	par_buf+40(%rip), %eax
	movl	%eax, propinterval(%rip)
	movl	par_buf+72(%rip), %eax
	movl	%eax, niter(%rip)
	movq	par_buf+80(%rip), %rax
	movq	%rax, rsqmin(%rip)
	movq	par_buf+88(%rip), %rax
	movq	%rax, rsqprop(%rip)
	movq	par_buf+96(%rip), %rax
	movq	%rax, epsilon(%rip)
	movq	par_buf+48(%rip), %rax
	movq	%rax, beta(%rip)
	movq	par_buf+56(%rip), %rax
	movq	par_buf+64(%rip), %rcx
	movl	par_buf+116(%rip), %edx
	movl	par_buf+120(%rip), %esi
	movq	%rax, mass(%rip)
	movq	%rcx, u0(%rip)
	movl	%edx, startflag(%rip)
	movl	%esi, saveflag(%rip)
	movabsq	$startfile, %rdi
	movabsq	$par_buf+124, %rsi
	callq	strcpy
	movabsq	$savefile, %rdi
	movabsq	$par_buf+380, %rsi
	callq	strcpy
	movl	startflag(%rip), %edi
	cmpq	$10, %rdi
	jne	.LBB2_22
# BB#21:                                # %if.then81
	xorq	%rdi, %rdi
	callq	rephase
	movl	startflag(%rip), %edi
.LBB2_22:                               # %if.end82
	movabsq	$startfile, %rsi
	callq	reload_lattice
	movq	%rax, startlat_p(%rip)
	xorq	%rax, %rax
	movl	%eax, valid_longlinks(%rip)
	movl	%eax, valid_fatlinks(%rip)
	movl	%eax, phases_in(%rip)
	movq	$1, %rdi
	callq	rephase
	xorq	%rax, %rax
	callq	make_loop_table
	xorq	%rax, %rax
	callq	make_path_table
	xorq	%rax, %rax
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp32:
	.size	readin, .Ltmp32-readin
	.cfi_endproc

	.globl	make_3n_gathers
	.align	16, 0x90
	.type	make_3n_gathers,@function
make_3n_gathers:                        # @make_3n_gathers
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r14
.Ltmp36:
	.cfi_def_cfa_offset 16
	pushq	%rbx
.Ltmp37:
	.cfi_def_cfa_offset 24
	pushq	%rax
.Ltmp38:
	.cfi_def_cfa_offset 32
.Ltmp39:
	.cfi_offset %rbx, -24
.Ltmp40:
	.cfi_offset %r14, -16
	xorq	%rax, %rax
	movl	%eax, 4(%rsp)
	movabsq	$third_neighbor, %r14
	leaq	4(%rsp), %rbx
	.align	16, 0x90
.LBB3_1:                                # %for.body
                                        # =>This Inner Loop Header: Depth=1
	movq	$1, %rdx
	xorq	%rcx, %rcx
	movq	$1, %r8
	movq	%r14, %rdi
	movq	%rbx, %rsi
	callq	make_gather
	movl	4(%rsp), %eax
	incq	%rax
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	$4, %rdx
	movl	%eax, 4(%rsp)
	jl	.LBB3_1
# BB#2:                                 # %for.end
	movq	$8, %rdi
	callq	sort_eight_neighborlists
	addq	$8, %rsp
	popq	%rbx
	popq	%r14
	retq
.Ltmp41:
	.size	make_3n_gathers, .Ltmp41-make_3n_gathers
	.cfi_endproc

	.globl	third_neighbor
	.align	16, 0x90
	.type	third_neighbor,@function
third_neighbor:                         # @third_neighbor
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%r15
.Ltmp46:
	.cfi_def_cfa_offset 16
	pushq	%r14
.Ltmp47:
	.cfi_def_cfa_offset 24
	pushq	%rbx
.Ltmp48:
	.cfi_def_cfa_offset 32
.Ltmp49:
	.cfi_offset %rbx, -32
.Ltmp50:
	.cfi_offset %r14, -24
.Ltmp51:
	.cfi_offset %r15, -16
	movq	%rcx, %rax
	movq	56(%rsp), %r10
	movq	48(%rsp), %r11
	movq	40(%rsp), %r14
	movq	32(%rsp), %r15
	movl	(%r8), %ebx
	movabsq	$4294967295, %r8        # imm = 0xFFFFFFFF
	andq	%r8, %r9
	cmpq	$1, %r9
	je	.LBB4_2
# BB#1:                                 # %entry
	movq	$7, %rcx
	subq	%rbx, %rcx
	movq	%rcx, %rbx
.LBB4_2:                                # %entry
	movl	%edi, (%r15)
	movl	%esi, (%r14)
	movl	%edx, (%r11)
	movl	%eax, (%r10)
	andq	%r8, %rbx
	cmpq	$7, %rbx
	ja	.LBB4_17
# BB#3:                                 # %entry
	jmpq	*.LJTI4_0(,%rbx,8)
.LBB4_4:                                # %sw.bb
	addq	$3, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	movslq	nx(%rip), %rsi
	sarq	%cl, %rdi
	movq	%rdi, %rax
	jmp	.LBB4_6
.LBB4_8:                                # %sw.bb5
	addq	$3, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	movslq	ny(%rip), %rdi
	sarq	%cl, %rsi
	movq	%rsi, %rax
	jmp	.LBB4_10
.LBB4_11:                               # %sw.bb13
	addq	$3, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	movslq	nz(%rip), %rsi
	sarq	%cl, %rdx
	movq	%rdx, %rax
	jmp	.LBB4_13
.LBB4_14:                               # %sw.bb21
	addq	$3, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	nt(%rip), %rsi
	jmp	.LBB4_15
.LBB4_16:                               # %sw.bb24
	movslq	nt(%rip), %rsi
	leaq	-3(%rax,%rsi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
.LBB4_15:                               # %sw.epilog
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	movl	%edx, (%r10)
	jmp	.LBB4_7
.LBB4_12:                               # %sw.bb16
	movslq	nz(%rip), %rsi
	leaq	-3(%rdx,%rsi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB4_13:                               # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r11)
	jmp	.LBB4_7
.LBB4_9:                                # %sw.bb8
	movslq	ny(%rip), %rdi
	leaq	-3(%rsi,%rdi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB4_10:                               # %sw.epilog
	cqto
	idivq	%rdi
	movl	%edx, (%r14)
	jmp	.LBB4_7
.LBB4_5:                                # %sw.bb1
	movslq	nx(%rip), %rsi
	leaq	-3(%rdi,%rsi,4), %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
.LBB4_6:                                # %sw.epilog
	cqto
	idivq	%rsi
	movl	%edx, (%r15)
.LBB4_7:                                # %sw.epilog
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB4_17:                               # %sw.default
	movabsq	$.Lstr36, %rdi
	callq	puts
	movq	$1, %rdi
	callq	exit
.Ltmp52:
	.size	third_neighbor, .Ltmp52-third_neighbor
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI4_0:
	.quad	.LBB4_4
	.quad	.LBB4_8
	.quad	.LBB4_11
	.quad	.LBB4_14
	.quad	.LBB4_16
	.quad	.LBB4_12
	.quad	.LBB4_9
	.quad	.LBB4_5

	.type	.L.str7,@object         # @.str7
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str7:
	.asciz	"Machine = %s, with %d nodes\n"
	.size	.L.str7, 29

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"nflavors"
	.size	.L.str9, 9

	.type	par_buf,@object         # @par_buf
	.comm	par_buf,640,8
	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"nx"
	.size	.L.str10, 3

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"ny"
	.size	.L.str11, 3

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"nz"
	.size	.L.str12, 3

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"nt"
	.size	.L.str13, 3

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"iseed"
	.size	.L.str14, 6

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"warms"
	.size	.L.str16, 6

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"trajecs"
	.size	.L.str17, 8

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"traj_between_meas"
	.size	.L.str18, 18

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"beta"
	.size	.L.str19, 5

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"mass"
	.size	.L.str20, 5

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"u0"
	.size	.L.str21, 3

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"microcanonical_time_step"
	.size	.L.str22, 25

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"steps_per_trajectory"
	.size	.L.str23, 21

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"max_cg_iterations"
	.size	.L.str24, 18

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"error_per_site"
	.size	.L.str25, 15

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"error_for_propagator"
	.size	.L.str26, 21

	.type	gf,@object              # @gf
	.comm	gf,8,8
	.type	.Lstr,@object           # @str
.Lstr:
	.asciz	"Finished setup"
	.size	.Lstr, 15

	.type	.Lstr28,@object         # @str28
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr28:
	.asciz	"Made 3nn gathers"
	.size	.Lstr28, 17

	.type	.Lstr29,@object         # @str29
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr29:
	.asciz	"Made nn gathers"
	.size	.Lstr29, 16

	.type	.Lstr30,@object         # @str30
.Lstr30:
	.asciz	"Made lattice"
	.size	.Lstr30, 13

	.type	.Lstr31,@object         # @str31
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr31:
	.asciz	"SU3 with improved KS action"
	.size	.Lstr31, 28

	.type	.Lstr32,@object         # @str32
	.align	16
.Lstr32:
	.asciz	"Microcanonical simulation with refreshing"
	.size	.Lstr32, 42

	.type	.Lstr33,@object         # @str33
	.section	.rodata.str1.1,"aMS",@progbits,1
.Lstr33:
	.asciz	"MIMD version 6"
	.size	.Lstr33, 15

	.type	.Lstr34,@object         # @str34
.Lstr34:
	.asciz	"R algorithm"
	.size	.Lstr34, 12

	.type	.Lstr35,@object         # @str35
.Lstr35:
	.asciz	"\n"
	.size	.Lstr35, 2

	.type	.Lstr36,@object         # @str36
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr36:
	.asciz	"third_neighb: bad direction"
	.size	.Lstr36, 28


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
