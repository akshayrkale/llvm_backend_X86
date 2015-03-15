	.file	"annexb.c.bc"
	.text
	.globl	WriteAnnexbNALU
	.align	16, 0x90
	.type	WriteAnnexbNALU,@function
WriteAnnexbNALU:                        # @WriteAnnexbNALU
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
	testq	%rbx, %rbx
	jne	.LBB0_2
# BB#1:                                 # %lor.rhs
	movabsq	$.L.str, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.WriteAnnexbNALU, %rcx
	movq	$38, %rdx
	callq	__assert_fail
.LBB0_2:                                # %lor.end
	movl	20(%rbx), %eax
	testq	%rax, %rax
	je	.LBB0_4
# BB#3:                                 # %lor.rhs2
	movabsq	$.L.str2, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.WriteAnnexbNALU, %rcx
	movq	$39, %rdx
	callq	__assert_fail
.LBB0_4:                                # %lor.end3
	cmpq	$0, f(%rip)
	jne	.LBB0_6
# BB#5:                                 # %lor.rhs6
	movabsq	$.L.str3, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.WriteAnnexbNALU, %rcx
	movq	$40, %rdx
	callq	__assert_fail
.LBB0_6:                                # %lor.end7
	movl	(%rbx), %eax
	leaq	-3(%rax), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	andq	%rcx, %rdx
	cmpq	$2, %rdx
	jb	.LBB0_8
# BB#7:                                 # %lor.rhs12
	movabsq	$.L.str4, %rdi
	movabsq	$.L.str1, %rsi
	movabsq	$.L__func__.WriteAnnexbNALU, %rcx
	movq	$41, %rdx
	callq	__assert_fail
	movl	(%rbx), %eax
.LBB0_8:                                # %lor.end13
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	$24, %r15
	cmpq	$4, %rax
	jl	.LBB0_10
# BB#9:                                 # %if.then
	movq	f(%rip), %rsi
	xorq	%rdi, %rdi
	callq	putc
	movq	$32, %r15
.LBB0_10:                               # %if.end
	movq	f(%rip), %rsi
	xorq	%rdi, %rdi
	callq	putc
	movq	f(%rip), %rsi
	xorq	%rdi, %rdi
	callq	putc
	movq	f(%rip), %rsi
	movq	$1, %rdi
	callq	putc
	movl	20(%rbx), %eax
	movq	$7, %rcx
	shlq	%cl, %rax
	movl	16(%rbx), %edx
	movq	$5, %rcx
	shlq	%cl, %rdx
	movl	12(%rbx), %ecx
	movq	24(%rbx), %rsi
	orq	%rax, %rdx
	orq	%rdx, %rcx
	movb	%cl, (%rsi)
	movl	4(%rbx), %r14d
	movq	24(%rbx), %rdi
	movq	f(%rip), %rcx
	movq	$1, %rsi
	movq	%r14, %rdx
	callq	fwrite
	movl	4(%rbx), %ecx
	cmpq	%rax, %r14
	jne	.LBB0_12
# BB#11:                                # %if.end33
	movq	f(%rip), %rdi
	leaq	(%r15,%rcx,8), %rbx
	callq	fflush
	movq	%rbx, %rax
	popq	%rbx
	popq	%r14
	popq	%r15
	retq
.LBB0_12:                               # %if.then30
	movabsq	$.L.str5, %rdx
	xorq	%rax, %rax
	movq	%rdx, %rdi
	movq	%rcx, %rsi
	callq	printf
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rdi
	callq	exit
.Ltmp10:
	.size	WriteAnnexbNALU, .Ltmp10-WriteAnnexbNALU
	.cfi_endproc

	.globl	OpenAnnexbFile
	.align	16, 0x90
	.type	OpenAnnexbFile,@function
OpenAnnexbFile:                         # @OpenAnnexbFile
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbx
.Ltmp13:
	.cfi_def_cfa_offset 16
.Ltmp14:
	.cfi_offset %rbx, -16
	movq	%rdi, %rbx
	movabsq	$.L.str6, %rsi
                                        # kill: RDI<def> RBX<kill>
	callq	fopen
	testq	%rax, %rax
	movq	%rax, f(%rip)
	je	.LBB1_2
# BB#1:                                 # %if.end
	popq	%rbx
	retq
.LBB1_2:                                # %if.then
	movabsq	$.L.str7, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rbx, %rsi
	callq	printf
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rdi
	callq	exit
.Ltmp15:
	.size	OpenAnnexbFile, .Ltmp15-OpenAnnexbFile
	.cfi_endproc

	.globl	CloseAnnexbFile
	.align	16, 0x90
	.type	CloseAnnexbFile,@function
CloseAnnexbFile:                        # @CloseAnnexbFile
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rax
.Ltmp17:
	.cfi_def_cfa_offset 16
	movq	f(%rip), %rdi
	callq	fclose
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	jne	.LBB2_2
# BB#1:                                 # %if.end
	popq	%rax
	retq
.LBB2_2:                                # %if.then
	movabsq	$.Lstr, %rax
	movq	%rax, %rdi
	callq	puts
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	%rax, %rdi
	callq	exit
.Ltmp18:
	.size	CloseAnnexbFile, .Ltmp18-CloseAnnexbFile
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"n != NULL"
	.size	.L.str, 10

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"annexb.c"
	.size	.L.str1, 9

	.type	.L__func__.WriteAnnexbNALU,@object # @__func__.WriteAnnexbNALU
.L__func__.WriteAnnexbNALU:
	.asciz	"WriteAnnexbNALU"
	.size	.L__func__.WriteAnnexbNALU, 16

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"n->forbidden_bit == 0"
	.size	.L.str2, 22

	.type	f,@object               # @f
	.local	f
	.comm	f,8,8
	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"f != NULL"
	.size	.L.str3, 10

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"n->startcodeprefix_len == 3 || n->startcodeprefix_len == 4"
	.size	.L.str4, 59

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Fatal: cannot write %d bytes to bitstream file, exit (-1)\n"
	.size	.L.str5, 59

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"wb"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Fatal: cannot open Annex B bytestream file '%s', exit (-1)\n"
	.size	.L.str7, 60

	.type	top_pic,@object         # @top_pic
	.comm	top_pic,8,8
	.type	bottom_pic,@object      # @bottom_pic
	.comm	bottom_pic,8,8
	.type	frame_pic,@object       # @frame_pic
	.comm	frame_pic,8,8
	.type	frame_pic2,@object      # @frame_pic2
	.comm	frame_pic2,8,8
	.type	frame_pic3,@object      # @frame_pic3
	.comm	frame_pic3,8,8
	.type	imgY_org,@object        # @imgY_org
	.comm	imgY_org,8,8
	.type	imgUV_org,@object       # @imgUV_org
	.comm	imgUV_org,8,8
	.type	img4Y_tmp,@object       # @img4Y_tmp
	.comm	img4Y_tmp,8,8
	.type	log2_max_frame_num_minus4,@object # @log2_max_frame_num_minus4
	.comm	log2_max_frame_num_minus4,4,4
	.type	log2_max_pic_order_cnt_lsb_minus4,@object # @log2_max_pic_order_cnt_lsb_minus4
	.comm	log2_max_pic_order_cnt_lsb_minus4,4,4
	.type	me_tot_time,@object     # @me_tot_time
	.comm	me_tot_time,4,4
	.type	me_time,@object         # @me_time
	.comm	me_time,4,4
	.type	active_pps,@object      # @active_pps
	.comm	active_pps,8,8
	.type	active_sps,@object      # @active_sps
	.comm	active_sps,8,8
	.type	mb_adaptive,@object     # @mb_adaptive
	.comm	mb_adaptive,4,4
	.type	MBPairIsField,@object   # @MBPairIsField
	.comm	MBPairIsField,4,4
	.type	wp_weight,@object       # @wp_weight
	.comm	wp_weight,8,8
	.type	wp_offset,@object       # @wp_offset
	.comm	wp_offset,8,8
	.type	wbp_weight,@object      # @wbp_weight
	.comm	wbp_weight,8,8
	.type	luma_log_weight_denom,@object # @luma_log_weight_denom
	.comm	luma_log_weight_denom,4,4
	.type	chroma_log_weight_denom,@object # @chroma_log_weight_denom
	.comm	chroma_log_weight_denom,4,4
	.type	wp_luma_round,@object   # @wp_luma_round
	.comm	wp_luma_round,4,4
	.type	wp_chroma_round,@object # @wp_chroma_round
	.comm	wp_chroma_round,4,4
	.type	imgY_org_top,@object    # @imgY_org_top
	.comm	imgY_org_top,8,8
	.type	imgY_org_bot,@object    # @imgY_org_bot
	.comm	imgY_org_bot,8,8
	.type	imgUV_org_top,@object   # @imgUV_org_top
	.comm	imgUV_org_top,8,8
	.type	imgUV_org_bot,@object   # @imgUV_org_bot
	.comm	imgUV_org_bot,8,8
	.type	imgY_org_frm,@object    # @imgY_org_frm
	.comm	imgY_org_frm,8,8
	.type	imgUV_org_frm,@object   # @imgUV_org_frm
	.comm	imgUV_org_frm,8,8
	.type	imgY_com,@object        # @imgY_com
	.comm	imgY_com,8,8
	.type	imgUV_com,@object       # @imgUV_com
	.comm	imgUV_com,8,8
	.type	direct_ref_idx,@object  # @direct_ref_idx
	.comm	direct_ref_idx,8,8
	.type	direct_pdir,@object     # @direct_pdir
	.comm	direct_pdir,8,8
	.type	pixel_map,@object       # @pixel_map
	.comm	pixel_map,8,8
	.type	refresh_map,@object     # @refresh_map
	.comm	refresh_map,8,8
	.type	intras,@object          # @intras
	.comm	intras,4,4
	.type	Iframe_ctr,@object      # @Iframe_ctr
	.comm	Iframe_ctr,4,4
	.type	Pframe_ctr,@object      # @Pframe_ctr
	.comm	Pframe_ctr,4,4
	.type	Bframe_ctr,@object      # @Bframe_ctr
	.comm	Bframe_ctr,4,4
	.type	frame_no,@object        # @frame_no
	.comm	frame_no,4,4
	.type	nextP_tr_fld,@object    # @nextP_tr_fld
	.comm	nextP_tr_fld,4,4
	.type	nextP_tr_frm,@object    # @nextP_tr_frm
	.comm	nextP_tr_frm,4,4
	.type	tot_time,@object        # @tot_time
	.comm	tot_time,4,4
	.type	errortext,@object       # @errortext
	.comm	errortext,300,16
	.type	resTrans_R,@object      # @resTrans_R
	.comm	resTrans_R,1024,16
	.type	resTrans_G,@object      # @resTrans_G
	.comm	resTrans_G,1024,16
	.type	resTrans_B,@object      # @resTrans_B
	.comm	resTrans_B,1024,16
	.type	rec_resR,@object        # @rec_resR
	.comm	rec_resR,1024,16
	.type	rec_resG,@object        # @rec_resG
	.comm	rec_resG,1024,16
	.type	rec_resB,@object        # @rec_resB
	.comm	rec_resB,1024,16
	.type	mprRGB,@object          # @mprRGB
	.comm	mprRGB,3072,16
	.type	dc_level,@object        # @dc_level
	.comm	dc_level,128,16
	.type	dc_level_temp,@object   # @dc_level_temp
	.comm	dc_level_temp,128,16
	.type	cbp_chroma_block,@object # @cbp_chroma_block
	.comm	cbp_chroma_block,128,16
	.type	cbp_chroma_block_temp,@object # @cbp_chroma_block_temp
	.comm	cbp_chroma_block_temp,128,16
	.type	b8_ipredmode8x8,@object # @b8_ipredmode8x8
	.comm	b8_ipredmode8x8,64,16
	.type	b8_intra_pred_modes8x8,@object # @b8_intra_pred_modes8x8
	.comm	b8_intra_pred_modes8x8,64,16
	.type	gop_structure,@object   # @gop_structure
	.comm	gop_structure,8,8
	.type	rdopt,@object           # @rdopt
	.comm	rdopt,8,8
	.type	rddata_top_frame_mb,@object # @rddata_top_frame_mb
	.comm	rddata_top_frame_mb,3360,8
	.type	rddata_bot_frame_mb,@object # @rddata_bot_frame_mb
	.comm	rddata_bot_frame_mb,3360,8
	.type	rddata_top_field_mb,@object # @rddata_top_field_mb
	.comm	rddata_top_field_mb,3360,8
	.type	rddata_bot_field_mb,@object # @rddata_bot_field_mb
	.comm	rddata_bot_field_mb,3360,8
	.type	p_stat,@object          # @p_stat
	.comm	p_stat,8,8
	.type	p_log,@object           # @p_log
	.comm	p_log,8,8
	.type	p_trace,@object         # @p_trace
	.comm	p_trace,8,8
	.type	p_in,@object            # @p_in
	.comm	p_in,4,4
	.type	p_dec,@object           # @p_dec
	.comm	p_dec,4,4
	.type	glob_remapping_of_pic_nums_idc_l0,@object # @glob_remapping_of_pic_nums_idc_l0
	.comm	glob_remapping_of_pic_nums_idc_l0,80,16
	.type	glob_abs_diff_pic_num_minus1_l0,@object # @glob_abs_diff_pic_num_minus1_l0
	.comm	glob_abs_diff_pic_num_minus1_l0,80,16
	.type	glob_long_term_pic_idx_l0,@object # @glob_long_term_pic_idx_l0
	.comm	glob_long_term_pic_idx_l0,80,16
	.type	glob_remapping_of_pic_nums_idc_l1,@object # @glob_remapping_of_pic_nums_idc_l1
	.comm	glob_remapping_of_pic_nums_idc_l1,80,16
	.type	glob_abs_diff_pic_num_minus1_l1,@object # @glob_abs_diff_pic_num_minus1_l1
	.comm	glob_abs_diff_pic_num_minus1_l1,80,16
	.type	glob_long_term_pic_idx_l1,@object # @glob_long_term_pic_idx_l1
	.comm	glob_long_term_pic_idx_l1,80,16
	.type	Bytes_After_Header,@object # @Bytes_After_Header
	.comm	Bytes_After_Header,4,4
	.type	rpc_bytes_to_go,@object # @rpc_bytes_to_go
	.comm	rpc_bytes_to_go,4,4
	.type	rpc_bits_to_go,@object  # @rpc_bits_to_go
	.comm	rpc_bits_to_go,4,4
	.type	.Lstr,@object           # @str
	.section	.rodata.str1.16,"aMS",@progbits,1
	.align	16
.Lstr:
	.asciz	"Fatal: cannot close Annex B bytestream file, exit (-1)"
	.size	.Lstr, 55


	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
