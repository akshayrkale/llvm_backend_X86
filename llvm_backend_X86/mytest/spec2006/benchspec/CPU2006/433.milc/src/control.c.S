	.file	"control.c.bc"
	.text
	.globl	main
	.align	16, 0x90
	.type	main,@function
main:                                   # @main
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
	callq	initialize_machine
	xorq	%rax, %rax
	callq	g_sync
	xorq	%rax, %rax
	callq	setup
	movq	%rax, 8(%rsp)           # 8-byte Spill
	movq	%rax, %rdi
	callq	readin
	movabsq	$4294967295, %r14       # imm = 0xFFFFFFFF
	jmp	.LBB0_1
	.align	16, 0x90
.LBB0_19:                               # %while.cond.backedge
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	8(%rsp), %rdi           # 8-byte Reload
	callq	readin
.LBB0_1:                                # %entry
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB0_3 Depth 2
                                        #     Child Loop BB0_7 Depth 2
	testq	%r14, %rax
	jne	.LBB0_20
# BB#2:                                 # %while.body
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	callq	dclock
	movslq	warms(%rip), %rax
	testq	%rax, %rax
	movabsq	$0, %rbx
	jle	.LBB0_4
	.align	16, 0x90
.LBB0_3:                                # %for.body
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rax, %rax
	callq	update
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	movslq	warms(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_3
.LBB0_4:                                # %for.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_6
# BB#5:                                 # %if.then
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$.Lstr40, %rdi
	callq	puts
.LBB0_6:                                # %if.end
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	stdout(%rip), %rdi
	callq	fflush
	movslq	trajecs(%rip), %rax
	xorq	%r13, %r13
	testq	%rax, %rax
	movabsq	$0, %rbx
	movabsq	$0, %r15
	jle	.LBB0_10
	.align	16, 0x90
.LBB0_7:                                # %for.body10
                                        #   Parent Loop BB0_1 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	xorq	%rax, %rax
	callq	update
	movq	%rax, %r12
	movslq	propinterval(%rip), %rsi
	movq	$32, %rcx
	movq	%rbx, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	andq	%r14, %rdx
	decq	%rsi
	andq	%r14, %rsi
	cmpq	%rsi, %rdx
	jne	.LBB0_9
# BB#8:                                 # %if.then14
                                        #   in Loop: Header=BB0_7 Depth=2
	xorq	%rdi, %rdi
	callq	rephase
	callq	g_measure
	movq	$1, %rdi
	callq	rephase
	movq	mass(%rip), %rdx
	movq	$1040, %rdi             # imm = 0x410
	movq	$1184, %rsi             # imm = 0x4A0
	callq	f_meas_imp
	addq	%r12, %r15
	incq	%r13
	movq	stdout(%rip), %rdi
	callq	fflush
.LBB0_9:                                # %for.inc25
                                        #   in Loop: Header=BB0_7 Depth=2
	incq	%rbx
	movq	$32, %rcx
	movq	%rbx, %rax
	movslq	trajecs(%rip), %rdx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	cmpq	%rdx, %rax
	jl	.LBB0_7
.LBB0_10:                               # %for.end27
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_12
# BB#11:                                # %if.then30
                                        #   in Loop: Header=BB0_1 Depth=1
	movabsq	$.Lstr, %rdi
	callq	puts
.LBB0_12:                               # %if.end32
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	stdout(%rip), %rdi
	callq	fflush
	movq	$32, %rcx
	movq	%r13, %rax
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	testq	%rax, %rax
	jle	.LBB0_15
# BB#13:                                # %if.end32
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	this_node(%rip), %eax
	testq	%r14, %rax
	jne	.LBB0_15
# BB#14:                                # %if.then39
                                        #   in Loop: Header=BB0_1 Depth=1
	andq	%r14, %r15
	movq	%r15, %rdi
	callq	int32_to_float64
	movq	%rax, %rbx
	andq	%r14, %r13
	movq	%r13, %rdi
	callq	int32_to_float64
	movq	%rbx, %rdi
	movq	%rax, %rsi
	callq	float64_div
	movq	%rax, %rcx
	xorq	%rax, %rax
	movabsq	$.L.str38, %rdi
	movq	%rcx, %rsi
	callq	printf
.LBB0_15:                               # %if.end44
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rax, %rax
	callq	dclock
	movl	this_node(%rip), %eax
	testq	%rax, %rax
	jne	.LBB0_17
# BB#16:                                # %if.then49
                                        #   in Loop: Header=BB0_1 Depth=1
	movl	total_iters(%rip), %esi
	xorq	%rax, %rax
	movabsq	$.L.str39, %rdi
	callq	printf
.LBB0_17:                               # %if.end51
                                        #   in Loop: Header=BB0_1 Depth=1
	movq	stdout(%rip), %rdi
	callq	fflush
	movl	saveflag(%rip), %eax
	cmpq	$20, %rax
	je	.LBB0_19
# BB#18:                                # %if.then55
                                        #   in Loop: Header=BB0_1 Depth=1
	xorq	%rdi, %rdi
	callq	rephase
	movl	saveflag(%rip), %edi
	movabsq	$savefile, %rsi
	callq	save_lattice
	movq	$1, %rdi
	callq	rephase
	jmp	.LBB0_19
.LBB0_20:                               # %while.end
	xorq	%rax, %rax
	addq	$16, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	retq
.Ltmp17:
	.size	main, .Ltmp17-main
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"magic_number"
	.size	.L.str, 13

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"time_stamp"
	.size	.L.str1, 11

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"checksums"
	.size	.L.str2, 10

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"nx"
	.size	.L.str3, 3

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"ny"
	.size	.L.str4, 3

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"nz"
	.size	.L.str5, 3

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"nt"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"action.description"
	.size	.L.str7, 19

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"gauge.description"
	.size	.L.str8, 18

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"gauge.beta11"
	.size	.L.str9, 13

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"gauge.beta12"
	.size	.L.str10, 13

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"gauge.tadpole.u0"
	.size	.L.str11, 17

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"gauge.nloops"
	.size	.L.str12, 13

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"gauge.nreps"
	.size	.L.str13, 12

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"gauge.previous.filename"
	.size	.L.str14, 24

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"gauge.previous.time_stamp"
	.size	.L.str15, 26

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"gauge.previous.checksums"
	.size	.L.str16, 25

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"gauge.fix.description"
	.size	.L.str17, 22

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"gauge.fix.tolerance"
	.size	.L.str18, 20

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"gauge.smear.description"
	.size	.L.str19, 24

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"gauge.smear.steps"
	.size	.L.str20, 18

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"gauge.smear.factor"
	.size	.L.str21, 19

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"quark.description"
	.size	.L.str22, 18

	.type	.L.str23,@object        # @.str23
.L.str23:
	.asciz	"quark.flavors"
	.size	.L.str23, 14

	.type	.L.str24,@object        # @.str24
.L.str24:
	.asciz	"quark.flavors1"
	.size	.L.str24, 15

	.type	.L.str25,@object        # @.str25
.L.str25:
	.asciz	"quark.flavors2"
	.size	.L.str25, 15

	.type	.L.str26,@object        # @.str26
.L.str26:
	.asciz	"quark.mass"
	.size	.L.str26, 11

	.type	.L.str27,@object        # @.str27
.L.str27:
	.asciz	"quark.mass1"
	.size	.L.str27, 12

	.type	.L.str28,@object        # @.str28
.L.str28:
	.asciz	"quark.mass2"
	.size	.L.str28, 12

	.type	.L.str29,@object        # @.str29
.L.str29:
	.asciz	"quark.kappa"
	.size	.L.str29, 12

	.type	.L.str30,@object        # @.str30
.L.str30:
	.asciz	"quark.link.c1"
	.size	.L.str30, 14

	.type	.L.str31,@object        # @.str31
.L.str31:
	.asciz	"quark.link.c3"
	.size	.L.str31, 14

	.type	.L.str32,@object        # @.str32
.L.str32:
	.asciz	"quark.staple.w3"
	.size	.L.str32, 16

	.type	.L.str33,@object        # @.str33
.L.str33:
	.asciz	"quark.clover.c0"
	.size	.L.str33, 16

	.type	.L.str34,@object        # @.str34
.L.str34:
	.asciz	"quark.clover.u0"
	.size	.L.str34, 16

	.type	.L.str35,@object        # @.str35
.L.str35:
	.zero	1
	.size	.L.str35, 1

	.type	gauge_info_keyword,@object # @gauge_info_keyword
	.data
	.globl	gauge_info_keyword
	.align	16
gauge_info_keyword:
	.quad	.L.str
	.quad	.L.str1
	.quad	.L.str2
	.quad	.L.str3
	.quad	.L.str4
	.quad	.L.str5
	.quad	.L.str6
	.quad	.L.str7
	.quad	.L.str8
	.quad	.L.str9
	.quad	.L.str10
	.quad	.L.str11
	.quad	.L.str12
	.quad	.L.str13
	.quad	.L.str14
	.quad	.L.str15
	.quad	.L.str16
	.quad	.L.str17
	.quad	.L.str18
	.quad	.L.str19
	.quad	.L.str20
	.quad	.L.str21
	.quad	.L.str22
	.quad	.L.str23
	.quad	.L.str24
	.quad	.L.str25
	.quad	.L.str26
	.quad	.L.str27
	.quad	.L.str28
	.quad	.L.str29
	.quad	.L.str30
	.quad	.L.str31
	.quad	.L.str32
	.quad	.L.str33
	.quad	.L.str34
	.quad	.L.str35
	.size	gauge_info_keyword, 288

	.type	warms,@object           # @warms
	.comm	warms,4,4
	.type	this_node,@object       # @this_node
	.comm	this_node,4,4
	.type	trajecs,@object         # @trajecs
	.comm	trajecs,4,4
	.type	propinterval,@object    # @propinterval
	.comm	propinterval,4,4
	.type	lattice,@object         # @lattice
	.comm	lattice,8,8
	.type	mass,@object            # @mass
	.comm	mass,8,8
	.type	.L.str38,@object        # @.str38
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str38:
	.asciz	"average cg iters for step= %e\n"
	.size	.L.str38, 31

	.type	.L.str39,@object        # @.str39
.L.str39:
	.asciz	"total_iters = %d\n"
	.size	.L.str39, 18

	.type	total_iters,@object     # @total_iters
	.comm	total_iters,4,4
	.type	saveflag,@object        # @saveflag
	.comm	saveflag,4,4
	.type	savefile,@object        # @savefile
	.comm	savefile,256,16
	.type	ensemble_id,@object     # @ensemble_id
	.comm	ensemble_id,256,16
	.type	sequence_number,@object # @sequence_number
	.comm	sequence_number,4,4
	.type	nx,@object              # @nx
	.comm	nx,4,4
	.type	ny,@object              # @ny
	.comm	ny,4,4
	.type	nz,@object              # @nz
	.comm	nz,4,4
	.type	nt,@object              # @nt
	.comm	nt,4,4
	.type	volume,@object          # @volume
	.comm	volume,4,4
	.type	iseed,@object           # @iseed
	.comm	iseed,4,4
	.type	steps,@object           # @steps
	.comm	steps,4,4
	.type	niter,@object           # @niter
	.comm	niter,4,4
	.type	nflavors,@object        # @nflavors
	.comm	nflavors,4,4
	.type	epsilon,@object         # @epsilon
	.comm	epsilon,8,8
	.type	beta,@object            # @beta
	.comm	beta,8,8
	.type	u0,@object              # @u0
	.comm	u0,8,8
	.type	rsqmin,@object          # @rsqmin
	.comm	rsqmin,8,8
	.type	rsqprop,@object         # @rsqprop
	.comm	rsqprop,8,8
	.type	startflag,@object       # @startflag
	.comm	startflag,4,4
	.type	startfile,@object       # @startfile
	.comm	startfile,256,16
	.type	phases_in,@object       # @phases_in
	.comm	phases_in,4,4
	.type	source_start,@object    # @source_start
	.comm	source_start,4,4
	.type	source_inc,@object      # @source_inc
	.comm	source_inc,4,4
	.type	n_sources,@object       # @n_sources
	.comm	n_sources,4,4
	.type	sites_on_node,@object   # @sites_on_node
	.comm	sites_on_node,4,4
	.type	even_sites_on_node,@object # @even_sites_on_node
	.comm	even_sites_on_node,4,4
	.type	odd_sites_on_node,@object # @odd_sites_on_node
	.comm	odd_sites_on_node,4,4
	.type	number_of_nodes,@object # @number_of_nodes
	.comm	number_of_nodes,4,4
	.type	valid_longlinks,@object # @valid_longlinks
	.comm	valid_longlinks,4,4
	.type	valid_fatlinks,@object  # @valid_fatlinks
	.comm	valid_fatlinks,4,4
	.type	startlat_p,@object      # @startlat_p
	.comm	startlat_p,8,8
	.type	node_prn,@object        # @node_prn
	.comm	node_prn,88,8
	.type	gen_pt,@object          # @gen_pt
	.comm	gen_pt,128,16
	.type	start_lat_hdr,@object   # @start_lat_hdr
	.comm	start_lat_hdr,92,4
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"RUNNING COMPLETED"
	.size	.Lstr, 18

	.type	.Lstr40,@object         # @str40
	.align	16
.Lstr40:
	.asciz	"WARMUPS COMPLETED"
	.size	.Lstr40, 18


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
