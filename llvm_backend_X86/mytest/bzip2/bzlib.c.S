	.file	"bzlib.c.bc"
	.section	.debug_info,"",@progbits
.Lsection_info:
	.section	.debug_abbrev,"",@progbits
.Lsection_abbrev:
	.section	.debug_line,"",@progbits
.Lsection_line:
	.section	.debug_loc,"",@progbits
	.section	.debug_pubnames,"",@progbits
	.section	.debug_pubtypes,"",@progbits
	.section	.debug_str,"MS",@progbits,1
.Linfo_string:
	.section	.debug_ranges,"",@progbits
.Ldebug_range:
	.section	.debug_loc,"",@progbits
.Lsection_debug_loc:
	.file	1 "bzlib.c"
	.file	2 "./bzlib_private.h"
	.text
	.globl	BZ2_bz__AssertH__fail
	.align	16, 0x90
	.type	BZ2_bz__AssertH__fail,@function
BZ2_bz__AssertH__fail:                  # @BZ2_bz__AssertH__fail
.Lfunc_begin0:
	.loc	1 88 0                  # bzlib.c:88:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp3:
	.cfi_def_cfa_offset 16
.Ltmp4:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp5:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$16, %rsp
.Ltmp6:
	.cfi_offset %rbx, -32
.Ltmp7:
	.cfi_offset %r14, -24
	movl	%edi, -20(%rbp)
	.loc	1 89 0 prologue_end     # bzlib.c:89:0
.Ltmp8:
	movq	stderr(%rip), %r14
	movl	-20(%rbp), %ebx
	.loc	1 106 0                 # bzlib.c:106:0
	callq	BZ2_bzlibVersion
	movq	%rax, %rcx
	.loc	1 89 0                  # bzlib.c:89:0
	movabsq	$.L.str, %rsi
	xorq	%rax, %rax
	movq	%r14, %rdi
	movq	%rbx, %rdx
	callq	fprintf
	.loc	1 109 0                 # bzlib.c:109:0
.Ltmp9:
	movl	-20(%rbp), %eax
	cmpq	$1007, %rax             # imm = 0x3EF
	jne	.LBB0_2
# BB#1:                                 # %if.then
	.loc	1 110 0                 # bzlib.c:110:0
.Ltmp10:
	movq	stderr(%rip), %rdi
	movabsq	$.L.str1, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp11:
.LBB0_2:                                # %if.end
	movq	$3, %rdi
	.loc	1 138 0                 # bzlib.c:138:0
	callq	exit
.Ltmp12:
.Ltmp13:
	.size	BZ2_bz__AssertH__fail, .Ltmp13-BZ2_bz__AssertH__fail
.Lfunc_end0:
	.cfi_endproc

	.globl	BZ2_bzlibVersion
	.align	16, 0x90
	.type	BZ2_bzlibVersion,@function
BZ2_bzlibVersion:                       # @BZ2_bzlibVersion
.Lfunc_begin1:
	.loc	1 1440 0                # bzlib.c:1440:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp16:
	.cfi_def_cfa_offset 16
.Ltmp17:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp18:
	.cfi_def_cfa_register %rbp
	.loc	1 1441 0 prologue_end   # bzlib.c:1441:0
.Ltmp19:
	movabsq	$.L.str5, %rax
	popq	%rbp
	retq
.Ltmp20:
.Ltmp21:
	.size	BZ2_bzlibVersion, .Ltmp21-BZ2_bzlibVersion
.Lfunc_end1:
	.cfi_endproc

	.globl	BZ2_bzCompressInit
	.align	16, 0x90
	.type	BZ2_bzCompressInit,@function
BZ2_bzCompressInit:                     # @BZ2_bzCompressInit
.Lfunc_begin2:
	.loc	1 210 0                 # bzlib.c:210:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp25:
	.cfi_def_cfa_offset 16
.Ltmp26:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp27:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
.Ltmp28:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movl	%esi, -28(%rbp)
	movl	%edx, -32(%rbp)
	movl	%ecx, -36(%rbp)
	.loc	1 214 0 prologue_end    # bzlib.c:214:0
.Ltmp29:
	callq	bz_config_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB2_1
.Ltmp30:
# BB#2:                                 # %if.end
	.loc	1 216 0                 # bzlib.c:216:0
	cmpq	$0, -24(%rbp)
	je	.LBB2_7
# BB#3:                                 # %lor.lhs.false
	movslq	-28(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB2_7
# BB#4:                                 # %lor.lhs.false2
	movslq	-28(%rbp), %rax
	cmpq	$9, %rax
	jg	.LBB2_7
# BB#5:                                 # %lor.lhs.false4
	movslq	-36(%rbp), %rax
	testq	%rax, %rax
	js	.LBB2_7
# BB#6:                                 # %lor.lhs.false6
	movslq	-36(%rbp), %rax
	cmpq	$251, %rax
	jl	.LBB2_8
.LBB2_7:                                # %if.then8
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	.loc	1 219 0                 # bzlib.c:219:0
	movl	%eax, -12(%rbp)
	jmp	.LBB2_28
.Ltmp31:
.LBB2_1:                                # %if.then
	movabsq	$4294967287, %rax       # imm = 0xFFFFFFF7
	.loc	1 214 0                 # bzlib.c:214:0
.Ltmp32:
	movl	%eax, -12(%rbp)
.Ltmp33:
.LBB2_28:                               # %return
	.loc	1 268 0                 # bzlib.c:268:0
	movl	-12(%rbp), %eax
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB2_8:                                # %if.end9
	.loc	1 221 0                 # bzlib.c:221:0
.Ltmp34:
	movl	-36(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB2_10
# BB#9:                                 # %if.then11
	movq	$30, %rax
	movl	%eax, -36(%rbp)
.Ltmp35:
.LBB2_10:                               # %if.end12
	.loc	1 222 0                 # bzlib.c:222:0
	movq	-24(%rbp), %rax
	cmpq	$0, 56(%rax)
	jne	.LBB2_12
# BB#11:                                # %if.then14
	movq	-24(%rbp), %rax
	movq	$default_bzalloc, 56(%rax)
.Ltmp36:
.LBB2_12:                               # %if.end16
	.loc	1 223 0                 # bzlib.c:223:0
	movq	-24(%rbp), %rax
	cmpq	$0, 64(%rax)
	jne	.LBB2_14
# BB#13:                                # %if.then18
	movq	-24(%rbp), %rax
	movq	$default_bzfree, 64(%rax)
.Ltmp37:
.LBB2_14:                               # %if.end20
	.loc	1 225 0                 # bzlib.c:225:0
	movq	-24(%rbp), %rax
	movq	72(%rax), %rdi
	movq	$55768, %rsi            # imm = 0xD9D8
	movq	$1, %rdx
	callq	*56(%rax)
	testq	%rax, %rax
	movq	%rax, -48(%rbp)
	je	.LBB2_15
# BB#16:                                # %if.end25
	.loc	1 227 0                 # bzlib.c:227:0
	movq	-24(%rbp), %rax
	movq	-48(%rbp), %rcx
	movq	%rax, (%rcx)
	.loc	1 229 0                 # bzlib.c:229:0
	movq	-48(%rbp), %rax
	movq	$0, 24(%rax)
	.loc	1 230 0                 # bzlib.c:230:0
	movq	-48(%rbp), %rax
	movq	$0, 32(%rax)
	.loc	1 231 0                 # bzlib.c:231:0
	movq	-48(%rbp), %rax
	movq	$0, 40(%rax)
	.loc	1 233 0                 # bzlib.c:233:0
	movl	-28(%rbp), %eax
	.loc	1 234 0                 # bzlib.c:234:0
	movq	-24(%rbp), %rbx
	.loc	1 233 0                 # bzlib.c:233:0
	imulq	$100000, %rax, %rax     # imm = 0x186A0
	movl	%eax, -40(%rbp)
	.loc	1 234 0                 # bzlib.c:234:0
	movslq	-40(%rbp), %rsi
	movq	72(%rbx), %rdi
	movq	$2, %rcx
	shlq	%cl, %rsi
	movq	$1, %rdx
	callq	*56(%rbx)
	movq	-48(%rbp), %rcx
	movq	%rax, 24(%rcx)
	.loc	1 235 0                 # bzlib.c:235:0
	movq	-24(%rbp), %rax
	movl	-40(%rbp), %esi
	movq	72(%rax), %rdi
	addq	$34, %rsi
	movq	$32, %rcx
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	movq	$2, %rcx
	shlq	%cl, %rsi
	movq	$1, %rdx
	callq	*56(%rax)
	movq	-48(%rbp), %rcx
	movq	%rax, 32(%rcx)
	.loc	1 236 0                 # bzlib.c:236:0
	movq	-24(%rbp), %rax
	movq	72(%rax), %rdi
	movq	$262148, %rsi           # imm = 0x40004
	movq	$1, %rdx
	callq	*56(%rax)
	movq	-48(%rbp), %rcx
	movq	%rax, 40(%rcx)
	.loc	1 238 0                 # bzlib.c:238:0
.Ltmp38:
	movq	-48(%rbp), %rax
	cmpq	$0, 24(%rax)
	je	.LBB2_19
# BB#17:                                # %lor.lhs.false47
	movq	-48(%rbp), %rax
	cmpq	$0, 32(%rax)
	je	.LBB2_19
# BB#18:                                # %lor.lhs.false51
	movq	-48(%rbp), %rax
	cmpq	$0, 40(%rax)
	je	.LBB2_19
.Ltmp39:
# BB#27:                                # %if.end86
	.loc	1 246 0                 # bzlib.c:246:0
	movq	-48(%rbp), %rax
	xorq	%rbx, %rbx
	movl	%ebx, 660(%rax)
	.loc	1 247 0                 # bzlib.c:247:0
	movq	-48(%rbp), %rax
	movq	$2, %rcx
	movl	%ecx, 12(%rax)
	.loc	1 248 0                 # bzlib.c:248:0
	movq	-48(%rbp), %rax
	movl	%ecx, 8(%rax)
	.loc	1 249 0                 # bzlib.c:249:0
	movq	-48(%rbp), %rax
	movl	%ebx, 652(%rax)
	.loc	1 250 0                 # bzlib.c:250:0
	movl	-28(%rbp), %eax
	movq	-48(%rbp), %rcx
	movl	%eax, 664(%rcx)
	.loc	1 251 0                 # bzlib.c:251:0
	movl	-28(%rbp), %eax
	movq	-48(%rbp), %rcx
	imulq	$100000, %rax, %rax     # imm = 0x186A0
	addq	$-19, %rax
	movl	%eax, 112(%rcx)
	.loc	1 252 0                 # bzlib.c:252:0
	movl	-32(%rbp), %eax
	movq	-48(%rbp), %rcx
	movl	%eax, 656(%rcx)
	.loc	1 253 0                 # bzlib.c:253:0
	movl	-36(%rbp), %eax
	movq	-48(%rbp), %rcx
	movl	%eax, 88(%rcx)
	.loc	1 255 0                 # bzlib.c:255:0
	movq	-48(%rbp), %rax
	movq	32(%rax), %rcx
	movq	%rcx, 64(%rax)
	.loc	1 256 0                 # bzlib.c:256:0
	movq	-48(%rbp), %rax
	movq	24(%rax), %rcx
	movq	%rcx, 72(%rax)
	.loc	1 257 0                 # bzlib.c:257:0
	movq	-48(%rbp), %rax
	movq	$0, 80(%rax)
	.loc	1 258 0                 # bzlib.c:258:0
	movq	-48(%rbp), %rax
	movq	24(%rax), %rcx
	movq	%rcx, 56(%rax)
	.loc	1 260 0                 # bzlib.c:260:0
	movq	-48(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, 48(%rcx)
	.loc	1 261 0                 # bzlib.c:261:0
	movq	-24(%rbp), %rax
	movl	%ebx, 12(%rax)
	.loc	1 262 0                 # bzlib.c:262:0
	movq	-24(%rbp), %rax
	movl	%ebx, 16(%rax)
	.loc	1 263 0                 # bzlib.c:263:0
	movq	-24(%rbp), %rax
	movl	%ebx, 36(%rax)
	.loc	1 264 0                 # bzlib.c:264:0
	movq	-24(%rbp), %rax
	movl	%ebx, 40(%rax)
	.loc	1 265 0                 # bzlib.c:265:0
	movq	-48(%rbp), %rdi
	callq	init_RL
	.loc	1 266 0                 # bzlib.c:266:0
	movq	-48(%rbp), %rdi
	callq	prepare_new_block
	.loc	1 267 0                 # bzlib.c:267:0
	movl	%ebx, -12(%rbp)
	jmp	.LBB2_28
.LBB2_19:                               # %if.then55
	.loc	1 239 0                 # bzlib.c:239:0
.Ltmp40:
	movq	-48(%rbp), %rax
	cmpq	$0, 24(%rax)
	je	.LBB2_21
# BB#20:                                # %if.then59
	movq	-24(%rbp), %rax
	movq	-48(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	24(%rcx), %rsi
	callq	*64(%rax)
.Ltmp41:
.LBB2_21:                               # %if.end63
	.loc	1 240 0                 # bzlib.c:240:0
	movq	-48(%rbp), %rax
	cmpq	$0, 32(%rax)
	je	.LBB2_23
# BB#22:                                # %if.then67
	movq	-24(%rbp), %rax
	movq	-48(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	32(%rcx), %rsi
	callq	*64(%rax)
.Ltmp42:
.LBB2_23:                               # %if.end71
	.loc	1 241 0                 # bzlib.c:241:0
	movq	-48(%rbp), %rax
	cmpq	$0, 40(%rax)
	je	.LBB2_25
# BB#24:                                # %if.then75
	movq	-24(%rbp), %rax
	movq	-48(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	40(%rcx), %rsi
	callq	*64(%rax)
.Ltmp43:
.LBB2_25:                               # %if.end79
	.loc	1 242 0                 # bzlib.c:242:0
	cmpq	$0, -48(%rbp)
	je	.LBB2_15
# BB#26:                                # %if.then82
	movq	-24(%rbp), %rax
	movq	72(%rax), %rdi
	movq	-48(%rbp), %rsi
	callq	*64(%rax)
.Ltmp44:
.LBB2_15:                               # %if.then24
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	.loc	1 226 0                 # bzlib.c:226:0
.Ltmp45:
	movl	%eax, -12(%rbp)
	jmp	.LBB2_28
.Ltmp46:
.Ltmp47:
	.size	BZ2_bzCompressInit, .Ltmp47-BZ2_bzCompressInit
.Lfunc_end2:
	.file	3 "./bzlib.h"
	.cfi_endproc

	.align	16, 0x90
	.type	bz_config_ok,@function
bz_config_ok:                           # @bz_config_ok
.Lfunc_begin3:
	.loc	1 149 0                 # bzlib.c:149:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp50:
	.cfi_def_cfa_offset 16
.Ltmp51:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp52:
	.cfi_def_cfa_register %rbp
	movq	$1, %rax
	.loc	1 153 0 prologue_end    # bzlib.c:153:0
.Ltmp53:
	popq	%rbp
	retq
.Ltmp54:
.Ltmp55:
	.size	bz_config_ok, .Ltmp55-bz_config_ok
.Lfunc_end3:
	.cfi_endproc

	.align	16, 0x90
	.type	default_bzalloc,@function
default_bzalloc:                        # @default_bzalloc
.Lfunc_begin4:
	.loc	1 160 0                 # bzlib.c:160:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp58:
	.cfi_def_cfa_offset 16
.Ltmp59:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp60:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	movl	%esi, -12(%rbp)
	.loc	1 161 0 prologue_end    # bzlib.c:161:0
.Ltmp61:
	movl	-12(%rbp), %edi
	movl	%edx, -16(%rbp)
	imulq	%rdx, %rdi
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	malloc
	movq	%rax, -24(%rbp)
	.loc	1 162 0                 # bzlib.c:162:0
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp62:
.Ltmp63:
	.size	default_bzalloc, .Ltmp63-default_bzalloc
.Lfunc_end4:
	.cfi_endproc

	.align	16, 0x90
	.type	default_bzfree,@function
default_bzfree:                         # @default_bzfree
.Lfunc_begin5:
	.loc	1 167 0                 # bzlib.c:167:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp66:
	.cfi_def_cfa_offset 16
.Ltmp67:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp68:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	testq	%rsi, %rsi
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	je	.LBB5_2
# BB#1:                                 # %if.then
	.loc	1 168 0 prologue_end    # bzlib.c:168:0
.Ltmp69:
	movq	-16(%rbp), %rdi
	callq	free
.Ltmp70:
.LBB5_2:                                # %if.end
	.loc	1 169 0                 # bzlib.c:169:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp71:
.Ltmp72:
	.size	default_bzfree, .Ltmp72-default_bzfree
.Lfunc_end5:
	.cfi_endproc

	.align	16, 0x90
	.type	init_RL,@function
init_RL:                                # @init_RL
.Lfunc_begin6:
	.loc	1 189 0                 # bzlib.c:189:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp75:
	.cfi_def_cfa_offset 16
.Ltmp76:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp77:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	$256, %rax              # imm = 0x100
	.loc	1 190 0 prologue_end    # bzlib.c:190:0
.Ltmp78:
	movl	%eax, 92(%rdi)
	.loc	1 191 0                 # bzlib.c:191:0
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 96(%rax)
	.loc	1 192 0                 # bzlib.c:192:0
	popq	%rbp
	retq
.Ltmp79:
.Ltmp80:
	.size	init_RL, .Ltmp80-init_RL
.Lfunc_end6:
	.cfi_endproc

	.align	16, 0x90
	.type	prepare_new_block,@function
prepare_new_block:                      # @prepare_new_block
.Lfunc_begin7:
	.loc	1 175 0                 # bzlib.c:175:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp83:
	.cfi_def_cfa_offset 16
.Ltmp84:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp85:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 177 0 prologue_end    # bzlib.c:177:0
.Ltmp86:
	movl	%eax, 108(%rdi)
	.loc	1 178 0                 # bzlib.c:178:0
	movq	-8(%rbp), %rcx
	movl	%eax, 116(%rcx)
	.loc	1 179 0                 # bzlib.c:179:0
	movq	-8(%rbp), %rcx
	movl	%eax, 120(%rcx)
	.loc	1 180 0                 # bzlib.c:180:0
.Ltmp87:
	movq	-8(%rbp), %rcx
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movl	%edx, 648(%rcx)
.Ltmp88:
	.loc	1 181 0                 # bzlib.c:181:0
	movl	%eax, -12(%rbp)
	jmp	.LBB7_1
	.align	16, 0x90
.LBB7_2:                                # %for.body
                                        #   in Loop: Header=BB7_1 Depth=1
	movslq	-12(%rbp), %rcx
	movq	-8(%rbp), %rdx
	movb	%al, 128(%rdx,%rcx)
	movl	-12(%rbp), %ecx
	incq	%rcx
	movl	%ecx, -12(%rbp)
.LBB7_1:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-12(%rbp), %rcx
	cmpq	$255, %rcx
	jle	.LBB7_2
.Ltmp89:
# BB#3:                                 # %for.end
	.loc	1 182 0                 # bzlib.c:182:0
	movq	-8(%rbp), %rax
	movl	660(%rax), %ecx
	incq	%rcx
	movl	%ecx, 660(%rax)
	.loc	1 183 0                 # bzlib.c:183:0
	popq	%rbp
	retq
.Ltmp90:
.Ltmp91:
	.size	prepare_new_block, .Ltmp91-prepare_new_block
.Lfunc_end7:
	.cfi_endproc

	.globl	BZ2_bzCompress
	.align	16, 0x90
	.type	BZ2_bzCompress,@function
BZ2_bzCompress:                         # @BZ2_bzCompress
.Lfunc_begin8:
	.loc	1 465 0                 # bzlib.c:465:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp95:
	.cfi_def_cfa_offset 16
.Ltmp96:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp97:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
.Ltmp98:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	.loc	1 468 0 prologue_end    # bzlib.c:468:0
.Ltmp99:
	cmpq	$0, -24(%rbp)
	movl	%esi, -28(%rbp)
	je	.LBB8_1
.Ltmp100:
# BB#2:                                 # %if.end
	.loc	1 469 0                 # bzlib.c:469:0
	movq	-24(%rbp), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%rax, -40(%rbp)
	je	.LBB8_1
# BB#3:                                 # %if.end3
	.loc	1 471 0                 # bzlib.c:471:0
.Ltmp101:
	movq	-40(%rbp), %rax
	movq	(%rax), %rax
	movq	-24(%rbp), %rcx
	cmpq	%rcx, %rax
	jne	.LBB8_1
.Ltmp102:
# BB#4:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	$3, %rax
	movq	$4, %rcx
	jmp	.LBB8_5
	.align	16, 0x90
.LBB8_12:                               # %if.then13
                                        #   in Loop: Header=BB8_5 Depth=1
	.loc	1 486 0                 # bzlib.c:486:0
.Ltmp103:
	movq	-24(%rbp), %rdx
	movl	8(%rdx), %edx
	movq	-40(%rbp), %rsi
	movl	%edx, 16(%rsi)
	.loc	1 487 0                 # bzlib.c:487:0
	movq	-40(%rbp), %rdx
	movl	%eax, 8(%rdx)
.Ltmp104:
.LBB8_5:                                # %preswitch
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 474 0                 # bzlib.c:474:0
	movq	-40(%rbp), %rdx
	movl	8(%rdx), %edx
	decq	%rdx
	andq	%rbx, %rdx
	cmpq	$3, %rdx
	ja	.LBB8_35
# BB#6:                                 # %preswitch
                                        #   in Loop: Header=BB8_5 Depth=1
	jmpq	*.LJTI8_0(,%rdx,8)
.LBB8_8:                                # %sw.bb8
                                        #   in Loop: Header=BB8_5 Depth=1
	.loc	1 480 0                 # bzlib.c:480:0
.Ltmp105:
	movl	-28(%rbp), %edx
	testq	%rdx, %rdx
	je	.LBB8_9
# BB#11:                                # %if.else
                                        #   in Loop: Header=BB8_5 Depth=1
	.loc	1 485 0                 # bzlib.c:485:0
.Ltmp106:
	movl	-28(%rbp), %edx
	cmpq	$1, %rdx
	je	.LBB8_12
# BB#13:                                # %if.else15
                                        #   in Loop: Header=BB8_5 Depth=1
	.loc	1 491 0                 # bzlib.c:491:0
.Ltmp107:
	movl	-28(%rbp), %edx
	cmpq	$2, %rdx
	jne	.LBB8_1
# BB#14:                                # %if.then18
                                        #   in Loop: Header=BB8_5 Depth=1
	.loc	1 492 0                 # bzlib.c:492:0
.Ltmp108:
	movq	-24(%rbp), %rdx
	movl	8(%rdx), %edx
	movq	-40(%rbp), %rsi
	movl	%edx, 16(%rsi)
	.loc	1 493 0                 # bzlib.c:493:0
	movq	-40(%rbp), %rdx
	movl	%ecx, 8(%rdx)
	jmp	.LBB8_5
.Ltmp109:
.LBB8_1:                                # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB8_36:                               # %return
	.loc	1 520 0                 # bzlib.c:520:0
	movl	%eax, -12(%rbp)
.LBB8_37:                               # %return
	.loc	1 521 0                 # bzlib.c:521:0
	movl	-12(%rbp), %eax
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB8_35:                               # %sw.epilog
	xorq	%rax, %rax
	jmp	.LBB8_36
.LBB8_9:                                # %if.then10
	.loc	1 481 0                 # bzlib.c:481:0
.Ltmp110:
	movq	-24(%rbp), %rdi
	callq	handle_compress
	testq	%rax, %rax
	movb	%al, -29(%rbp)
	movq	$1, %rax
	jne	.LBB8_36
.Ltmp111:
# BB#10:                                # %if.then10
	movq	$-2, %rax
	jmp	.LBB8_36
.LBB8_7:                                # %sw.bb
	.loc	1 477 0                 # bzlib.c:477:0
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.LBB8_15:                               # %sw.bb23
	.loc	1 500 0                 # bzlib.c:500:0
.Ltmp112:
	movl	-28(%rbp), %eax
	cmpq	$1, %rax
	je	.LBB8_17
# BB#16:                                # %if.then26
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.Ltmp113:
.LBB8_24:                               # %sw.bb47
	.loc	1 510 0                 # bzlib.c:510:0
	movl	-28(%rbp), %eax
	cmpq	$2, %rax
	je	.LBB8_26
# BB#25:                                # %if.then50
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.Ltmp114:
.LBB8_17:                               # %if.end27
	.loc	1 501 0                 # bzlib.c:501:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movl	16(%rax), %eax
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB8_19
# BB#18:                                # %if.then33
	.loc	1 502 0                 # bzlib.c:502:0
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.Ltmp115:
.LBB8_26:                               # %if.end51
	.loc	1 511 0                 # bzlib.c:511:0
	movq	-40(%rbp), %rax
	movq	(%rax), %rcx
	movl	16(%rax), %eax
	movl	8(%rcx), %ecx
	cmpq	%rcx, %rax
	je	.LBB8_28
# BB#27:                                # %if.then57
	.loc	1 512 0                 # bzlib.c:512:0
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.Ltmp116:
.LBB8_19:                               # %if.end34
	.loc	1 503 0                 # bzlib.c:503:0
	movq	-24(%rbp), %rdi
	callq	handle_compress
	.loc	1 504 0                 # bzlib.c:504:0
.Ltmp117:
	movq	-40(%rbp), %rcx
.Ltmp118:
	.loc	1 503 0                 # bzlib.c:503:0
	movb	%al, -29(%rbp)
	.loc	1 504 0                 # bzlib.c:504:0
.Ltmp119:
	movl	16(%rcx), %eax
	testq	%rax, %rax
	jne	.LBB8_22
# BB#20:                                # %lor.lhs.false
	movq	-40(%rbp), %rdi
	callq	isempty_RL
	testq	%rax, %rax
	je	.LBB8_22
# BB#21:                                # %lor.lhs.false41
	movq	-40(%rbp), %rax
	movslq	120(%rax), %rcx
	movslq	116(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB8_23
.Ltmp120:
.LBB8_22:                               # %if.then44
	movq	$2, %rax
	jmp	.LBB8_36
.LBB8_28:                               # %if.end58
	.loc	1 513 0                 # bzlib.c:513:0
	movq	-24(%rbp), %rdi
	callq	handle_compress
	testq	%rax, %rax
	movb	%al, -29(%rbp)
	je	.LBB8_29
# BB#30:                                # %if.end62
	.loc	1 515 0                 # bzlib.c:515:0
.Ltmp121:
	movq	-40(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB8_33
# BB#31:                                # %lor.lhs.false66
	movq	-40(%rbp), %rdi
	callq	isempty_RL
	testq	%rax, %rax
	je	.LBB8_33
# BB#32:                                # %lor.lhs.false69
	movq	-40(%rbp), %rax
	movslq	120(%rax), %rcx
	movslq	116(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB8_34
.Ltmp122:
.LBB8_33:                               # %if.then74
	movq	$3, %rax
	jmp	.LBB8_36
.LBB8_29:                               # %if.then61
	.loc	1 514 0                 # bzlib.c:514:0
.Ltmp123:
	movl	%ebx, -12(%rbp)
	jmp	.LBB8_37
.Ltmp124:
.LBB8_23:                               # %if.end45
	.loc	1 506 0                 # bzlib.c:506:0
	movq	-40(%rbp), %rax
	movq	$2, %rcx
	movl	%ecx, 8(%rax)
	movq	$1, %rax
	jmp	.LBB8_36
.LBB8_34:                               # %if.end75
	.loc	1 517 0                 # bzlib.c:517:0
	movq	-40(%rbp), %rax
	movq	$1, %rcx
	movl	%ecx, 8(%rax)
	movq	$4, %rax
	jmp	.LBB8_36
.Ltmp125:
.Ltmp126:
	.size	BZ2_bzCompress, .Ltmp126-BZ2_bzCompress
.Lfunc_end8:
	.cfi_endproc
	.section	.rodata,"a",@progbits
	.align	8
.LJTI8_0:
	.quad	.LBB8_7
	.quad	.LBB8_8
	.quad	.LBB8_15
	.quad	.LBB8_24

	.text
	.align	16, 0x90
	.type	handle_compress,@function
handle_compress:                        # @handle_compress
.Lfunc_begin9:
	.loc	1 419 0                 # bzlib.c:419:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp130:
	.cfi_def_cfa_offset 16
.Ltmp131:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp132:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$32, %rsp
.Ltmp133:
	.cfi_offset %rbx, -48
.Ltmp134:
	.cfi_offset %r12, -40
.Ltmp135:
	.cfi_offset %r14, -32
.Ltmp136:
	.cfi_offset %r15, -24
	movq	%rdi, -40(%rbp)
	xorq	%rax, %rax
	.loc	1 422 0 prologue_end    # bzlib.c:422:0
.Ltmp137:
	movq	-40(%rbp), %rcx
	.loc	1 420 0                 # bzlib.c:420:0
	movb	%al, -41(%rbp)
	.loc	1 421 0                 # bzlib.c:421:0
	movb	%al, -42(%rbp)
	.loc	1 422 0                 # bzlib.c:422:0
	movq	48(%rcx), %rax
	movq	%rax, -56(%rbp)
	movq	$2, %rbx
	.loc	1 443 0                 # bzlib.c:443:0
.Ltmp138:
	movabsq	$0, %r14
	movabsq	$1, %r15
	movq	$1, %r12
	jmp	.LBB9_1
	.align	16, 0x90
.LBB9_14:                               # %if.then47
                                        #   in Loop: Header=BB9_1 Depth=1
	callq	BZ2_compressBlock
	.loc	1 444 0                 # bzlib.c:444:0
	movq	-56(%rbp), %rax
	movl	%r12d, 12(%rax)
.Ltmp139:
.LBB9_1:                                # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 426 0                 # bzlib.c:426:0
	movq	-56(%rbp), %rax
	movl	12(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB9_9
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 427 0                 # bzlib.c:427:0
.Ltmp140:
	movq	-56(%rbp), %rdi
	callq	copy_output_until_stop
	movzbl	-42(%rbp), %ecx
	.loc	1 428 0                 # bzlib.c:428:0
.Ltmp141:
	movq	-56(%rbp), %rdx
.Ltmp142:
	.loc	1 427 0                 # bzlib.c:427:0
	orq	%rax, %rcx
	movb	%cl, -42(%rbp)
	.loc	1 428 0                 # bzlib.c:428:0
.Ltmp143:
	movslq	120(%rdx), %rax
	movslq	116(%rdx), %rcx
	cmpq	%rcx, %rax
	jl	.LBB9_18
.Ltmp144:
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 429 0                 # bzlib.c:429:0
	movq	-56(%rbp), %rax
	movl	8(%rax), %eax
	cmpq	$4, %rax
	jne	.LBB9_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	-56(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB9_6
# BB#5:                                 # %land.lhs.true11
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 431 0                 # bzlib.c:431:0
	movq	-56(%rbp), %rdi
	callq	isempty_RL
	testq	%rax, %rax
	jne	.LBB9_18
.Ltmp145:
.LBB9_6:                                # %if.end15
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 432 0                 # bzlib.c:432:0
	movq	-56(%rbp), %rdi
	callq	prepare_new_block
	.loc	1 433 0                 # bzlib.c:433:0
	movq	-56(%rbp), %rax
	movl	%ebx, 12(%rax)
	.loc	1 434 0                 # bzlib.c:434:0
.Ltmp146:
	movq	-56(%rbp), %rax
	movl	8(%rax), %eax
	cmpq	$3, %rax
	jne	.LBB9_9
# BB#7:                                 # %land.lhs.true20
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	-56(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB9_9
# BB#8:                                 # %land.lhs.true24
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 436 0                 # bzlib.c:436:0
	movq	-56(%rbp), %rdi
	callq	isempty_RL
	testq	%rax, %rax
	jne	.LBB9_18
.Ltmp147:
	.align	16, 0x90
.LBB9_9:                                # %if.end30
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 439 0                 # bzlib.c:439:0
	movq	-56(%rbp), %rax
	movl	12(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB9_1
# BB#10:                                # %if.then34
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 440 0                 # bzlib.c:440:0
.Ltmp148:
	movq	-56(%rbp), %rdi
	callq	copy_input_until_stop
	movzbl	-41(%rbp), %ecx
	.loc	1 441 0                 # bzlib.c:441:0
.Ltmp149:
	movq	-56(%rbp), %rdx
.Ltmp150:
	.loc	1 440 0                 # bzlib.c:440:0
	orq	%rax, %rcx
	movb	%cl, -41(%rbp)
	.loc	1 441 0                 # bzlib.c:441:0
.Ltmp151:
	movl	8(%rdx), %eax
	cmpq	$2, %rax
	je	.LBB9_15
# BB#11:                                # %land.lhs.true43
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	-56(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB9_15
# BB#12:                                # %if.then47
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 442 0                 # bzlib.c:442:0
.Ltmp152:
	movq	-56(%rbp), %rdi
	callq	flush_RL
	.loc	1 443 0                 # bzlib.c:443:0
	movq	-56(%rbp), %rdi
	movl	8(%rdi), %eax
	cmpq	$4, %rax
	movq	%r14, %rsi
	jne	.LBB9_14
# BB#13:                                # %if.then47
                                        #   in Loop: Header=BB9_1 Depth=1
	movq	%r15, %rsi
	jmp	.LBB9_14
.Ltmp153:
	.align	16, 0x90
.LBB9_15:                               # %if.else
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 447 0                 # bzlib.c:447:0
	movq	-56(%rbp), %rax
	movslq	108(%rax), %rcx
	movslq	112(%rax), %rax
	cmpq	%rax, %rcx
	jl	.LBB9_17
# BB#16:                                # %if.then55
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 448 0                 # bzlib.c:448:0
.Ltmp154:
	movq	-56(%rbp), %rdi
	xorq	%rsi, %rsi
	jmp	.LBB9_14
.Ltmp155:
.LBB9_17:                               # %if.else57
                                        #   in Loop: Header=BB9_1 Depth=1
	.loc	1 452 0                 # bzlib.c:452:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	jne	.LBB9_1
.Ltmp156:
.LBB9_18:                               # %while.end
	.loc	1 459 0                 # bzlib.c:459:0
	movzbl	-41(%rbp), %eax
	testq	%rax, %rax
	movq	$1, %rax
	jne	.LBB9_22
# BB#19:                                # %lor.rhs
	movzbl	-42(%rbp), %eax
	testq	%rax, %rax
	je	.LBB9_20
# BB#21:                                # %lor.rhs
	movabsq	$1, %rax
	jmp	.LBB9_22
.LBB9_20:
	movabsq	$0, %rax
.LBB9_22:                               # %lor.end
	addq	$32, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp157:
.Ltmp158:
	.size	handle_compress, .Ltmp158-handle_compress
.Lfunc_end9:
	.cfi_endproc

	.align	16, 0x90
	.type	isempty_RL,@function
isempty_RL:                             # @isempty_RL
.Lfunc_begin10:
	.loc	1 197 0                 # bzlib.c:197:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp161:
	.cfi_def_cfa_offset 16
.Ltmp162:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp163:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -16(%rbp)
	.loc	1 198 0 prologue_end    # bzlib.c:198:0
.Ltmp164:
	movl	92(%rdi), %eax
	cmpq	$255, %rax
	ja	.LBB10_3
# BB#1:                                 # %land.lhs.true
	movq	-16(%rbp), %rax
	movslq	96(%rax), %rax
	testq	%rax, %rax
	jle	.LBB10_3
# BB#2:                                 # %if.then
	xorq	%rax, %rax
	jmp	.LBB10_4
.LBB10_3:                               # %if.else
	movq	$1, %rax
.LBB10_4:                               # %return
	.loc	1 200 0                 # bzlib.c:200:0
	movb	%al, -1(%rbp)
.Ltmp165:
	.loc	1 201 0                 # bzlib.c:201:0
	movzbl	-1(%rbp), %eax
	popq	%rbp
	retq
.Ltmp166:
.Ltmp167:
	.size	isempty_RL, .Ltmp167-isempty_RL
.Lfunc_end10:
	.cfi_endproc

	.globl	BZ2_bzCompressEnd
	.align	16, 0x90
	.type	BZ2_bzCompressEnd,@function
BZ2_bzCompressEnd:                      # @BZ2_bzCompressEnd
.Lfunc_begin11:
	.loc	1 526 0                 # bzlib.c:526:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp170:
	.cfi_def_cfa_offset 16
.Ltmp171:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp172:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	testq	%rdi, %rdi
	movq	%rdi, -16(%rbp)
	je	.LBB11_1
# BB#2:                                 # %if.end
	.loc	1 529 0 prologue_end    # bzlib.c:529:0
.Ltmp173:
	movq	-16(%rbp), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%rax, -24(%rbp)
	je	.LBB11_1
# BB#3:                                 # %if.end3
	.loc	1 531 0                 # bzlib.c:531:0
.Ltmp174:
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	-16(%rbp), %rcx
	cmpq	%rcx, %rax
	je	.LBB11_4
.Ltmp175:
.LBB11_1:                               # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB11_11:                              # %return
	.loc	1 540 0                 # bzlib.c:540:0
	movl	%eax, -4(%rbp)
	.loc	1 541 0                 # bzlib.c:541:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB11_4:                               # %if.end7
	.loc	1 533 0                 # bzlib.c:533:0
.Ltmp176:
	movq	-24(%rbp), %rax
	cmpq	$0, 24(%rax)
	je	.LBB11_6
# BB#5:                                 # %if.then9
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	24(%rcx), %rsi
	callq	*64(%rax)
.Ltmp177:
.LBB11_6:                               # %if.end11
	.loc	1 534 0                 # bzlib.c:534:0
	movq	-24(%rbp), %rax
	cmpq	$0, 32(%rax)
	je	.LBB11_8
# BB#7:                                 # %if.then13
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	32(%rcx), %rsi
	callq	*64(%rax)
.Ltmp178:
.LBB11_8:                               # %if.end17
	.loc	1 535 0                 # bzlib.c:535:0
	movq	-24(%rbp), %rax
	cmpq	$0, 40(%rax)
	je	.LBB11_10
# BB#9:                                 # %if.then19
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	40(%rcx), %rsi
	callq	*64(%rax)
.Ltmp179:
.LBB11_10:                              # %if.end23
	.loc	1 536 0                 # bzlib.c:536:0
	movq	-16(%rbp), %rax
	movq	48(%rax), %rsi
	movq	72(%rax), %rdi
	callq	*64(%rax)
	.loc	1 538 0                 # bzlib.c:538:0
	movq	-16(%rbp), %rax
	movq	$0, 48(%rax)
	xorq	%rax, %rax
	jmp	.LBB11_11
.Ltmp180:
.Ltmp181:
	.size	BZ2_bzCompressEnd, .Ltmp181-BZ2_bzCompressEnd
.Lfunc_end11:
	.cfi_endproc

	.globl	BZ2_bzDecompressInit
	.align	16, 0x90
	.type	BZ2_bzDecompressInit,@function
BZ2_bzDecompressInit:                   # @BZ2_bzDecompressInit
.Lfunc_begin12:
	.loc	1 553 0                 # bzlib.c:553:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp184:
	.cfi_def_cfa_offset 16
.Ltmp185:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp186:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movl	%esi, -20(%rbp)
	movl	%edx, -24(%rbp)
	.loc	1 556 0 prologue_end    # bzlib.c:556:0
.Ltmp187:
	callq	bz_config_ok
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	testq	%rcx, %rax
	je	.LBB12_1
.Ltmp188:
# BB#2:                                 # %if.end
	.loc	1 558 0                 # bzlib.c:558:0
	cmpq	$0, -16(%rbp)
	je	.LBB12_3
.Ltmp189:
# BB#4:                                 # %if.end2
	.loc	1 559 0                 # bzlib.c:559:0
	movl	-24(%rbp), %eax
	testq	%rax, %rax
	je	.LBB12_6
# BB#5:                                 # %land.lhs.true
	movl	-24(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB12_3
.Ltmp190:
.LBB12_6:                               # %if.end6
	.loc	1 560 0                 # bzlib.c:560:0
	movslq	-20(%rbp), %rax
	testq	%rax, %rax
	js	.LBB12_3
# BB#7:                                 # %lor.lhs.false
	movslq	-20(%rbp), %rax
	cmpq	$5, %rax
	jl	.LBB12_8
.Ltmp191:
.LBB12_3:                               # %if.then1
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
	.loc	1 558 0                 # bzlib.c:558:0
.Ltmp192:
	movl	%eax, -4(%rbp)
	jmp	.LBB12_15
.Ltmp193:
.LBB12_1:                               # %if.then
	movabsq	$4294967287, %rax       # imm = 0xFFFFFFF7
	.loc	1 556 0                 # bzlib.c:556:0
.Ltmp194:
	movl	%eax, -4(%rbp)
.Ltmp195:
.LBB12_15:                              # %return
	.loc	1 585 0                 # bzlib.c:585:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB12_8:                               # %if.end10
	.loc	1 562 0                 # bzlib.c:562:0
.Ltmp196:
	movq	-16(%rbp), %rax
	cmpq	$0, 56(%rax)
	jne	.LBB12_10
# BB#9:                                 # %if.then12
	movq	-16(%rbp), %rax
	movq	$default_bzalloc, 56(%rax)
.Ltmp197:
.LBB12_10:                              # %if.end14
	.loc	1 563 0                 # bzlib.c:563:0
	movq	-16(%rbp), %rax
	cmpq	$0, 64(%rax)
	jne	.LBB12_12
# BB#11:                                # %if.then16
	movq	-16(%rbp), %rax
	movq	$default_bzfree, 64(%rax)
.Ltmp198:
.LBB12_12:                              # %if.end18
	.loc	1 565 0                 # bzlib.c:565:0
	movq	-16(%rbp), %rax
	movq	72(%rax), %rdi
	movq	$64144, %rsi            # imm = 0xFA90
	movq	$1, %rdx
	callq	*56(%rax)
	testq	%rax, %rax
	movq	%rax, -32(%rbp)
	je	.LBB12_13
# BB#14:                                # %if.end23
	.loc	1 567 0                 # bzlib.c:567:0
	movq	-16(%rbp), %rax
	movq	-32(%rbp), %rcx
	movq	%rax, (%rcx)
	.loc	1 568 0                 # bzlib.c:568:0
	movq	-32(%rbp), %rax
	movq	-16(%rbp), %rcx
	movq	%rax, 48(%rcx)
	.loc	1 569 0                 # bzlib.c:569:0
	movq	-32(%rbp), %rax
	movq	$10, %rcx
	movl	%ecx, 8(%rax)
	.loc	1 570 0                 # bzlib.c:570:0
	movq	-32(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 36(%rax)
	.loc	1 571 0                 # bzlib.c:571:0
	movq	-32(%rbp), %rax
	movl	%ecx, 32(%rax)
	.loc	1 572 0                 # bzlib.c:572:0
	movq	-32(%rbp), %rax
	movl	%ecx, 3188(%rax)
	.loc	1 573 0                 # bzlib.c:573:0
	movq	-16(%rbp), %rax
	movl	%ecx, 12(%rax)
	.loc	1 574 0                 # bzlib.c:574:0
	movq	-16(%rbp), %rax
	movl	%ecx, 16(%rax)
	.loc	1 575 0                 # bzlib.c:575:0
	movq	-16(%rbp), %rax
	movl	%ecx, 36(%rax)
	.loc	1 576 0                 # bzlib.c:576:0
	movq	-16(%rbp), %rax
	movl	%ecx, 40(%rax)
	.loc	1 577 0                 # bzlib.c:577:0
	movzbl	-24(%rbp), %eax
	movq	-32(%rbp), %rdx
	movb	%al, 44(%rdx)
	.loc	1 578 0                 # bzlib.c:578:0
	movq	-32(%rbp), %rax
	movq	$0, 3168(%rax)
	.loc	1 579 0                 # bzlib.c:579:0
	movq	-32(%rbp), %rax
	movq	$0, 3160(%rax)
	.loc	1 580 0                 # bzlib.c:580:0
	movq	-32(%rbp), %rax
	movq	$0, 3152(%rax)
	.loc	1 581 0                 # bzlib.c:581:0
	movq	-32(%rbp), %rax
	movl	%ecx, 48(%rax)
	.loc	1 582 0                 # bzlib.c:582:0
	movl	-20(%rbp), %eax
	movq	-32(%rbp), %rdx
	movl	%eax, 52(%rdx)
	.loc	1 584 0                 # bzlib.c:584:0
	movl	%ecx, -4(%rbp)
	jmp	.LBB12_15
.LBB12_13:                              # %if.then22
	movabsq	$4294967293, %rax       # imm = 0xFFFFFFFD
	.loc	1 566 0                 # bzlib.c:566:0
.Ltmp199:
	movl	%eax, -4(%rbp)
	jmp	.LBB12_15
.Ltmp200:
.Ltmp201:
	.size	BZ2_bzDecompressInit, .Ltmp201-BZ2_bzDecompressInit
.Lfunc_end12:
	.cfi_endproc

	.globl	BZ2_indexIntoF
	.align	16, 0x90
	.type	BZ2_indexIntoF,@function
BZ2_indexIntoF:                         # @BZ2_indexIntoF
.Lfunc_begin13:
	.loc	1 744 0                 # bzlib.c:744:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp204:
	.cfi_def_cfa_offset 16
.Ltmp205:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp206:
	.cfi_def_cfa_register %rbp
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	xorq	%rax, %rax
	.loc	1 746 0 prologue_end    # bzlib.c:746:0
.Ltmp207:
	movl	%eax, -20(%rbp)
	movq	$256, %rax              # imm = 0x100
	.loc	1 747 0                 # bzlib.c:747:0
	movl	%eax, -24(%rbp)
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB13_1:                               # %do.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 749 0                 # bzlib.c:749:0
.Ltmp208:
	movl	-20(%rbp), %ecx
	movl	-24(%rbp), %edx
	addq	%rcx, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	movq	$1, %rcx
	shrq	%cl, %rdx
	movl	%edx, -28(%rbp)
	.loc	1 750 0                 # bzlib.c:750:0
.Ltmp209:
	movslq	-28(%rbp), %rcx
	movq	-16(%rbp), %rdx
	movslq	-4(%rbp), %rsi
	movslq	(%rdx,%rcx,4), %rcx
	cmpq	%rcx, %rsi
	jl	.LBB13_3
# BB#2:                                 # %if.then
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	-28(%rbp), %ecx
	movl	%ecx, -20(%rbp)
	jmp	.LBB13_4
	.align	16, 0x90
.LBB13_3:                               # %if.else
                                        #   in Loop: Header=BB13_1 Depth=1
	movl	-28(%rbp), %ecx
	movl	%ecx, -24(%rbp)
.Ltmp210:
.LBB13_4:                               # %do.cond
                                        #   in Loop: Header=BB13_1 Depth=1
	.loc	1 751 0                 # bzlib.c:751:0
	movl	-24(%rbp), %ecx
	movl	-20(%rbp), %edx
	subq	%rdx, %rcx
	andq	%rax, %rcx
	cmpq	$1, %rcx
	jne	.LBB13_1
.Ltmp211:
# BB#5:                                 # %do.end
	.loc	1 753 0                 # bzlib.c:753:0
	movl	-20(%rbp), %eax
	popq	%rbp
	retq
.Ltmp212:
.Ltmp213:
	.size	BZ2_indexIntoF, .Ltmp213-BZ2_indexIntoF
.Lfunc_end13:
	.cfi_endproc

	.globl	BZ2_bzDecompress
	.align	16, 0x90
	.type	BZ2_bzDecompress,@function
BZ2_bzDecompress:                       # @BZ2_bzDecompress
.Lfunc_begin14:
	.loc	1 865 0                 # bzlib.c:865:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp217:
	.cfi_def_cfa_offset 16
.Ltmp218:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp219:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r12
	pushq	%rbx
	subq	$48, %rsp
.Ltmp220:
	.cfi_offset %rbx, -48
.Ltmp221:
	.cfi_offset %r12, -40
.Ltmp222:
	.cfi_offset %r14, -32
.Ltmp223:
	.cfi_offset %r15, -24
	testq	%rdi, %rdi
	movq	%rdi, -48(%rbp)
	je	.LBB14_1
# BB#2:                                 # %if.end
	.loc	1 869 0 prologue_end    # bzlib.c:869:0
.Ltmp224:
	movq	-48(%rbp), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%rax, -64(%rbp)
	je	.LBB14_1
# BB#3:                                 # %if.end3
	.loc	1 871 0                 # bzlib.c:871:0
.Ltmp225:
	movq	-64(%rbp), %rax
	movq	(%rax), %rax
	movq	-48(%rbp), %rcx
	cmpq	%rcx, %rax
	je	.LBB14_4
.Ltmp226:
.LBB14_1:                               # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB14_28:                              # %return
	.loc	1 907 0                 # bzlib.c:907:0
.Ltmp227:
	movl	%eax, -36(%rbp)
.Ltmp228:
.LBB14_29:                              # %return
	.loc	1 914 0                 # bzlib.c:914:0
	movl	-36(%rbp), %eax
	addq	$48, %rsp
	popq	%rbx
	popq	%r12
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.LBB14_4:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.loc	1 883 0                 # bzlib.c:883:0
.Ltmp229:
	movabsq	$.L.str2, %r14
.Ltmp230:
	.loc	1 885 0                 # bzlib.c:885:0
	movabsq	$.L.str3, %r15
	movq	$14, %r12
.Ltmp231:
	.align	16, 0x90
.LBB14_5:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 874 0                 # bzlib.c:874:0
	movq	-64(%rbp), %rax
	movl	8(%rax), %eax
	cmpq	$1, %rax
	je	.LBB14_6
.Ltmp232:
# BB#7:                                 # %if.end11
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 875 0                 # bzlib.c:875:0
	movq	-64(%rbp), %rax
	movl	8(%rax), %eax
	cmpq	$2, %rax
	jne	.LBB14_21
# BB#8:                                 # %if.then14
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 876 0                 # bzlib.c:876:0
.Ltmp233:
	movq	-64(%rbp), %rax
	movzbl	44(%rax), %eax
	testq	%rax, %rax
	je	.LBB14_10
# BB#9:                                 # %if.then15
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 877 0                 # bzlib.c:877:0
	movq	-64(%rbp), %rdi
	callq	unRLE_obuf_to_output_SMALL
	jmp	.LBB14_11
	.align	16, 0x90
.LBB14_10:                              # %if.else
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 878 0                 # bzlib.c:878:0
	movq	-64(%rbp), %rdi
	callq	unRLE_obuf_to_output_FAST
.LBB14_11:                              # %if.end17
                                        #   in Loop: Header=BB14_5 Depth=1
	movb	%al, -49(%rbp)
.Ltmp234:
	.loc	1 879 0                 # bzlib.c:879:0
	movzbl	-49(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB14_12
.Ltmp235:
# BB#13:                                # %if.end20
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 880 0                 # bzlib.c:880:0
	movq	-64(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%rbx, %rcx
	cmpq	%rcx, %rax
	jne	.LBB14_30
# BB#14:                                # %land.lhs.true
                                        #   in Loop: Header=BB14_5 Depth=1
	movq	-64(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB14_30
# BB#15:                                # %if.then23
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 881 0                 # bzlib.c:881:0
.Ltmp236:
	movq	-64(%rbp), %rax
	movl	3184(%rax), %ecx
	notq	%rcx
	movl	%ecx, 3184(%rax)
.Ltmp237:
	.loc	1 882 0                 # bzlib.c:882:0
	movq	-64(%rbp), %rax
	movslq	52(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB14_17
# BB#16:                                # %if.then26
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 883 0                 # bzlib.c:883:0
	movq	-64(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	3184(%rax), %ecx
	movl	3176(%rax), %edx
	xorq	%rax, %rax
	movq	%r14, %rsi
	callq	fprintf
.Ltmp238:
.LBB14_17:                              # %if.end29
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 885 0                 # bzlib.c:885:0
	movq	-64(%rbp), %rax
	movslq	52(%rax), %rax
	cmpq	$2, %rax
	jl	.LBB14_19
# BB#18:                                # %if.then32
                                        #   in Loop: Header=BB14_5 Depth=1
	movq	stderr(%rip), %rdi
	xorq	%rax, %rax
	movq	%r15, %rsi
	callq	fprintf
.Ltmp239:
.LBB14_19:                              # %if.end34
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 886 0                 # bzlib.c:886:0
	movq	-64(%rbp), %rax
	movl	3184(%rax), %ecx
	movl	3176(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB14_12
.Ltmp240:
# BB#20:                                # %if.end39
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 888 0                 # bzlib.c:888:0
	movq	-64(%rbp), %rax
	movl	3188(%rax), %edx
	movq	$31, %rcx
	movq	%rdx, %rsi
	shrq	%cl, %rsi
	leaq	(%rdx,%rdx), %rcx
	orq	%rsi, %rcx
	movl	%ecx, 3188(%rax)
	.loc	1 891 0                 # bzlib.c:891:0
	movq	-64(%rbp), %rax
	movl	3184(%rax), %ecx
	movl	3188(%rax), %edx
	xorq	%rcx, %rdx
	movl	%edx, 3188(%rax)
	.loc	1 892 0                 # bzlib.c:892:0
	movq	-64(%rbp), %rax
	movl	%r12d, 8(%rax)
.Ltmp241:
.LBB14_21:                              # %if.end47
                                        #   in Loop: Header=BB14_5 Depth=1
	.loc	1 897 0                 # bzlib.c:897:0
	movq	-64(%rbp), %rax
	movslq	8(%rax), %rax
	cmpq	$10, %rax
	jl	.LBB14_5
# BB#22:                                # %if.then50
                                        #   in Loop: Header=BB14_5 Depth=1
.Ltmp242:
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 898 0                 # bzlib.c:898:0
	movq	-64(%rbp), %rdi
	callq	BZ2_decompress
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$4, %rcx
	movl	%eax, -68(%rbp)
	je	.LBB14_23
# BB#26:                                # %if.end65
                                        #   in Loop: Header=BB14_5 Depth=1
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 907 0                 # bzlib.c:907:0
.Ltmp243:
	movq	-64(%rbp), %rax
	movl	8(%rax), %eax
	cmpq	$2, %rax
	je	.LBB14_5
	jmp	.LBB14_27
.Ltmp244:
.LBB14_6:                               # %if.then10
	.loc	1 874 0                 # bzlib.c:874:0
	movl	%ebx, -36(%rbp)
	jmp	.LBB14_29
.Ltmp245:
.LBB14_30:                              # %if.else45
	xorq	%rax, %rax
	jmp	.LBB14_28
.LBB14_23:                              # %if.then53
.Ltmp246:
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 900 0                 # bzlib.c:900:0
	movq	-64(%rbp), %rax
	movslq	52(%rax), %rax
	cmpq	$3, %rax
	jl	.LBB14_25
# BB#24:                                # %if.then56
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 901 0                 # bzlib.c:901:0
	movq	-64(%rbp), %rax
	movq	stderr(%rip), %rdi
	movl	3188(%rax), %ecx
	movl	3180(%rax), %edx
	movabsq	$.L.str4, %rsi
	xorq	%rax, %rax
	callq	fprintf
.Ltmp247:
.LBB14_25:                              # %if.end59
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 903 0                 # bzlib.c:903:0
	movq	-64(%rbp), %rax
	movl	3188(%rax), %ecx
	movl	3180(%rax), %eax
	cmpq	%rax, %rcx
	je	.LBB14_27
.Ltmp248:
.LBB14_12:                              # %if.then19
	movabsq	$4294967292, %rax       # imm = 0xFFFFFFFC
	jmp	.LBB14_28
.LBB14_27:                              # %if.then68
.Ltmp249:
	#DEBUG_VALUE: r <- [RBP+-68]
	.loc	1 907 0                 # bzlib.c:907:0
	movl	-68(%rbp), %eax
	jmp	.LBB14_28
.Ltmp250:
.Ltmp251:
	.size	BZ2_bzDecompress, .Ltmp251-BZ2_bzDecompress
.Lfunc_end14:
	.cfi_endproc

	.align	16, 0x90
	.type	unRLE_obuf_to_output_SMALL,@function
unRLE_obuf_to_output_SMALL:             # @unRLE_obuf_to_output_SMALL
.Lfunc_begin15:
	.loc	1 763 0                 # bzlib.c:763:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp255:
	.cfi_def_cfa_offset 16
.Ltmp256:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp257:
	.cfi_def_cfa_register %rbp
	pushq	%r15
	pushq	%r14
	pushq	%r13
	pushq	%r12
	pushq	%rbx
	subq	$24, %rsp
.Ltmp258:
	.cfi_offset %rbx, -56
.Ltmp259:
	.cfi_offset %r12, -48
.Ltmp260:
	.cfi_offset %r13, -40
.Ltmp261:
	.cfi_offset %r14, -32
.Ltmp262:
	.cfi_offset %r15, -24
	movq	%rdi, -56(%rbp)
	.loc	1 766 0 prologue_end    # bzlib.c:766:0
.Ltmp263:
	movzbl	20(%rdi), %eax
	movabsq	$4294967295, %r13       # imm = 0xFFFFFFFF
	movq	$1, %r14
	movq	$2, %r15
	testq	%rax, %rax
	je	.LBB15_44
# BB#1:
	xorq	%r12, %r12
	jmp	.LBB15_2
	.align	16, 0x90
.LBB15_6:                               # %if.then23
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 779 0                 # bzlib.c:779:0
.Ltmp264:
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
.Ltmp265:
.LBB15_2:                               # %while.body2
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 771 0                 # bzlib.c:771:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	je	.LBB15_3
.Ltmp266:
# BB#4:                                 # %if.end
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 772 0                 # bzlib.c:772:0
	movq	-56(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_5
.Ltmp267:
# BB#7:                                 # %while.end
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 783 0                 # bzlib.c:783:0
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_3
.Ltmp268:
# BB#8:                                 # %if.end30
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 786 0                 # bzlib.c:786:0
	movq	-56(%rbp), %rax
	movl	64080(%rax), %edx
	movslq	1092(%rax), %rax
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB15_9
.Ltmp269:
# BB#11:                                # %if.end37
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 789 0                 # bzlib.c:789:0
	movq	-56(%rbp), %rax
	movl	%r14d, 16(%rax)
	.loc	1 790 0                 # bzlib.c:790:0
	movq	-56(%rbp), %rax
	movzbl	64(%rax), %ecx
	movb	%cl, 12(%rax)
	.loc	1 791 0                 # bzlib.c:791:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	.loc	1 791 0                 # bzlib.c:791:0
.Ltmp270:
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_14
# BB#12:                                # %if.then59
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 791 0                 # bzlib.c:791:0
.Ltmp271:
	movq	-56(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-56(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 791 0                 # bzlib.c:791:0
.Ltmp272:
	movq	-56(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB15_14
# BB#13:                                # %if.then68
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rax
	movl	%r12d, 28(%rax)
.Ltmp273:
.LBB15_14:                              # %if.end71
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 791 0                 # bzlib.c:791:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	.loc	1 792 0                 # bzlib.c:792:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	je	.LBB15_16
# BB#15:                                # %select.mid
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rax, %rax
.LBB15_16:                              # %select.end
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	-57(%rbp), %ecx
	movq	-56(%rbp), %rdx
	xorq	%rax, %rcx
	movb	%cl, -57(%rbp)
	movl	1092(%rdx), %eax
	incq	%rax
	movl	%eax, 1092(%rdx)
	.loc	1 793 0                 # bzlib.c:793:0
.Ltmp274:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_2
.Ltmp275:
# BB#17:                                # %if.end88
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 794 0                 # bzlib.c:794:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB15_18
.Ltmp276:
# BB#19:                                # %if.end96
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 796 0                 # bzlib.c:796:0
	movq	-56(%rbp), %rax
	movl	%r15d, 16(%rax)
	.loc	1 797 0                 # bzlib.c:797:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	.loc	1 797 0                 # bzlib.c:797:0
.Ltmp277:
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_22
# BB#20:                                # %if.then125
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 797 0                 # bzlib.c:797:0
.Ltmp278:
	movq	-56(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-56(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 797 0                 # bzlib.c:797:0
.Ltmp279:
	movq	-56(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB15_22
# BB#21:                                # %if.then135
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rax
	movl	%r12d, 28(%rax)
.Ltmp280:
.LBB15_22:                              # %if.end138
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 797 0                 # bzlib.c:797:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	.loc	1 798 0                 # bzlib.c:798:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	je	.LBB15_24
# BB#23:                                # %select.mid11
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rax, %rax
.LBB15_24:                              # %select.end10
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	-57(%rbp), %ecx
	movq	-56(%rbp), %rdx
	xorq	%rax, %rcx
	movb	%cl, -57(%rbp)
	movl	1092(%rdx), %eax
	incq	%rax
	movl	%eax, 1092(%rdx)
	.loc	1 799 0                 # bzlib.c:799:0
.Ltmp281:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_2
.Ltmp282:
# BB#25:                                # %if.end156
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 800 0                 # bzlib.c:800:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB15_18
.Ltmp283:
# BB#26:                                # %if.end164
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 802 0                 # bzlib.c:802:0
	movq	-56(%rbp), %rax
	movq	$3, %rcx
	movl	%ecx, 16(%rax)
	.loc	1 803 0                 # bzlib.c:803:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	.loc	1 803 0                 # bzlib.c:803:0
.Ltmp284:
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_29
# BB#27:                                # %if.then193
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 803 0                 # bzlib.c:803:0
.Ltmp285:
	movq	-56(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-56(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 803 0                 # bzlib.c:803:0
.Ltmp286:
	movq	-56(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB15_29
# BB#28:                                # %if.then203
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rax
	movl	%r12d, 28(%rax)
.Ltmp287:
.LBB15_29:                              # %if.end206
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 803 0                 # bzlib.c:803:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	.loc	1 804 0                 # bzlib.c:804:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	je	.LBB15_31
# BB#30:                                # %select.mid14
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rax, %rax
.LBB15_31:                              # %select.end13
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	-57(%rbp), %ecx
	movq	-56(%rbp), %rdx
	xorq	%rax, %rcx
	movb	%cl, -57(%rbp)
	movl	1092(%rdx), %eax
	incq	%rax
	movl	%eax, 1092(%rdx)
	.loc	1 805 0                 # bzlib.c:805:0
.Ltmp288:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_2
.Ltmp289:
# BB#32:                                # %if.end224
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 806 0                 # bzlib.c:806:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	je	.LBB15_33
.Ltmp290:
.LBB15_18:                              # %if.then93
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 794 0                 # bzlib.c:794:0
	movzbl	-57(%rbp), %eax
	movq	-56(%rbp), %rcx
	movl	%eax, 64(%rcx)
	jmp	.LBB15_2
.Ltmp291:
	.align	16, 0x90
.LBB15_5:                               # %if.end6
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 773 0                 # bzlib.c:773:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movzbl	12(%rax), %eax
	movq	24(%rcx), %rcx
	movb	%al, (%rcx)
	.loc	1 774 0                 # bzlib.c:774:0
.Ltmp292:
	movq	-56(%rbp), %rax
	movl	3184(%rax), %edx
	movq	$8, %rcx
	movq	%rdx, %rsi
	movzbl	12(%rax), %edi
	shlq	%cl, %rsi
	movq	$24, %rcx
	shrq	%cl, %rdx
	xorq	%rdx, %rdi
	movl	BZ2_crc32Table(,%rdi,4), %ecx
	xorq	%rsi, %rcx
	movl	%ecx, 3184(%rax)
.Ltmp293:
	.loc	1 775 0                 # bzlib.c:775:0
	movq	-56(%rbp), %rax
	movl	16(%rax), %ecx
	decq	%rcx
	movl	%ecx, 16(%rax)
	.loc	1 776 0                 # bzlib.c:776:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	incq	24(%rax)
	.loc	1 777 0                 # bzlib.c:777:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	32(%rax), %ecx
	decq	%rcx
	movl	%ecx, 32(%rax)
	.loc	1 778 0                 # bzlib.c:778:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %ecx
	incq	%rcx
	movl	%ecx, 36(%rax)
	.loc	1 779 0                 # bzlib.c:779:0
.Ltmp294:
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_2
	jmp	.LBB15_6
.Ltmp295:
.LBB15_33:                              # %if.end232
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 808 0                 # bzlib.c:808:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	.loc	1 808 0                 # bzlib.c:808:0
.Ltmp296:
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_36
# BB#34:                                # %if.then260
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 808 0                 # bzlib.c:808:0
.Ltmp297:
	movq	-56(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-56(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 808 0                 # bzlib.c:808:0
.Ltmp298:
	movq	-56(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB15_36
# BB#35:                                # %if.then270
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rax
	movl	%r12d, 28(%rax)
.Ltmp299:
.LBB15_36:                              # %if.end273
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 808 0                 # bzlib.c:808:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	.loc	1 809 0                 # bzlib.c:809:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	je	.LBB15_38
# BB#37:                                # %select.mid17
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rax, %rax
.LBB15_38:                              # %select.end16
                                        #   in Loop: Header=BB15_2 Depth=1
	movzbl	-57(%rbp), %ecx
	movq	-56(%rbp), %rdx
	xorq	%rax, %rcx
	movb	%cl, -57(%rbp)
	movl	1092(%rdx), %eax
	incq	%rax
	movl	%eax, 1092(%rdx)
	.loc	1 810 0                 # bzlib.c:810:0
	movzbl	-57(%rbp), %eax
	movq	-56(%rbp), %rcx
	addq	$4, %rax
	movl	%eax, 16(%rcx)
	.loc	1 811 0                 # bzlib.c:811:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rcx
	movl	%eax, 64(%rcx)
	movq	-56(%rbp), %rdx
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	.loc	1 811 0                 # bzlib.c:811:0
.Ltmp300:
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_41
# BB#39:                                # %if.then315
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 811 0                 # bzlib.c:811:0
.Ltmp301:
	movq	-56(%rbp), %rax
	movslq	28(%rax), %rcx
	movl	BZ2_rNums(,%rcx,4), %ecx
	movl	%ecx, 24(%rax)
	movq	-56(%rbp), %rax
	movl	28(%rax), %ecx
	incq	%rcx
	movl	%ecx, 28(%rax)
	.loc	1 811 0                 # bzlib.c:811:0
.Ltmp302:
	movq	-56(%rbp), %rax
	movl	28(%rax), %eax
	cmpq	$512, %rax              # imm = 0x200
	jne	.LBB15_41
# BB#40:                                # %if.then325
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rax
	movl	%r12d, 28(%rax)
.Ltmp303:
.LBB15_41:                              # %if.end328
                                        #   in Loop: Header=BB15_2 Depth=1
	.loc	1 811 0                 # bzlib.c:811:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %ecx
	decq	%rcx
	movl	%ecx, 24(%rax)
	.loc	1 812 0                 # bzlib.c:812:0
	movq	-56(%rbp), %rax
	movl	24(%rax), %eax
	cmpq	$1, %rax
	movq	$1, %rax
	je	.LBB15_43
# BB#42:                                # %select.mid19
                                        #   in Loop: Header=BB15_2 Depth=1
	xorq	%rax, %rax
.LBB15_43:                              # %select.end18
                                        #   in Loop: Header=BB15_2 Depth=1
	movq	-56(%rbp), %rcx
	movl	64(%rcx), %edx
	xorq	%rax, %rdx
	movl	%edx, 64(%rcx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	jmp	.LBB15_2
.Ltmp304:
.LBB15_44:
	movq	$3, %r12
	jmp	.LBB15_45
	.align	16, 0x90
.LBB15_48:                              # %if.then382
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 828 0                 # bzlib.c:828:0
.Ltmp305:
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
.Ltmp306:
.LBB15_45:                              # %while.body341
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 820 0                 # bzlib.c:820:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	32(%rax), %eax
	testq	%rax, %rax
	je	.LBB15_3
.Ltmp307:
# BB#46:                                # %if.end347
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 821 0                 # bzlib.c:821:0
	movq	-56(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_47
.Ltmp308:
# BB#49:                                # %while.end387
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 832 0                 # bzlib.c:832:0
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_3
.Ltmp309:
# BB#50:                                # %if.end394
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 835 0                 # bzlib.c:835:0
	movq	-56(%rbp), %rax
	movl	64080(%rax), %edx
	movslq	1092(%rax), %rax
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rax
	jg	.LBB15_9
.Ltmp310:
# BB#51:                                # %if.end401
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 838 0                 # bzlib.c:838:0
	movq	-56(%rbp), %rax
	movl	%r14d, 16(%rax)
	.loc	1 839 0                 # bzlib.c:839:0
	movq	-56(%rbp), %rax
	movzbl	64(%rax), %ecx
	movb	%cl, 12(%rax)
	.loc	1 840 0                 # bzlib.c:840:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 841 0                 # bzlib.c:841:0
.Ltmp311:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_45
.Ltmp312:
# BB#52:                                # %if.end438
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 842 0                 # bzlib.c:842:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB15_53
.Ltmp313:
# BB#54:                                # %if.end446
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 844 0                 # bzlib.c:844:0
	movq	-56(%rbp), %rax
	movl	%r15d, 16(%rax)
	.loc	1 845 0                 # bzlib.c:845:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 846 0                 # bzlib.c:846:0
.Ltmp314:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_45
.Ltmp315:
# BB#55:                                # %if.end480
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 847 0                 # bzlib.c:847:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB15_53
.Ltmp316:
# BB#56:                                # %if.end488
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 849 0                 # bzlib.c:849:0
	movq	-56(%rbp), %rax
	movl	%r12d, 16(%rax)
	.loc	1 850 0                 # bzlib.c:850:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 851 0                 # bzlib.c:851:0
.Ltmp317:
	movq	-56(%rbp), %rax
	movl	64080(%rax), %ecx
	movl	1092(%rax), %eax
	incq	%rcx
	andq	%r13, %rcx
	cmpq	%rcx, %rax
	je	.LBB15_45
.Ltmp318:
# BB#57:                                # %if.end522
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 852 0                 # bzlib.c:852:0
	movq	-56(%rbp), %rax
	movzbl	-57(%rbp), %ecx
	movl	64(%rax), %eax
	cmpq	%rax, %rcx
	je	.LBB15_58
.Ltmp319:
.LBB15_53:                              # %if.then443
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 842 0                 # bzlib.c:842:0
	movzbl	-57(%rbp), %eax
	movq	-56(%rbp), %rcx
	movl	%eax, 64(%rcx)
	jmp	.LBB15_45
.Ltmp320:
	.align	16, 0x90
.LBB15_47:                              # %if.end352
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 822 0                 # bzlib.c:822:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rcx
	movzbl	12(%rax), %eax
	movq	24(%rcx), %rcx
	movb	%al, (%rcx)
	.loc	1 823 0                 # bzlib.c:823:0
.Ltmp321:
	movq	-56(%rbp), %rax
	movl	3184(%rax), %edx
	movq	$8, %rcx
	movq	%rdx, %rsi
	movzbl	12(%rax), %edi
	shlq	%cl, %rsi
	movq	$24, %rcx
	shrq	%cl, %rdx
	xorq	%rdx, %rdi
	movl	BZ2_crc32Table(,%rdi,4), %ecx
	xorq	%rsi, %rcx
	movl	%ecx, 3184(%rax)
.Ltmp322:
	.loc	1 824 0                 # bzlib.c:824:0
	movq	-56(%rbp), %rax
	movl	16(%rax), %ecx
	decq	%rcx
	movl	%ecx, 16(%rax)
	.loc	1 825 0                 # bzlib.c:825:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	incq	24(%rax)
	.loc	1 826 0                 # bzlib.c:826:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	32(%rax), %ecx
	decq	%rcx
	movl	%ecx, 32(%rax)
	.loc	1 827 0                 # bzlib.c:827:0
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %ecx
	incq	%rcx
	movl	%ecx, 36(%rax)
	.loc	1 828 0                 # bzlib.c:828:0
.Ltmp323:
	movq	-56(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %eax
	testq	%rax, %rax
	jne	.LBB15_45
	jmp	.LBB15_48
.Ltmp324:
.LBB15_58:                              # %if.end530
                                        #   in Loop: Header=BB15_45 Depth=1
	.loc	1 854 0                 # bzlib.c:854:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rdx
	movb	%al, -57(%rbp)
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	.loc	1 855 0                 # bzlib.c:855:0
	movzbl	-57(%rbp), %eax
	movq	-56(%rbp), %rcx
	addq	$4, %rax
	movl	%eax, 16(%rcx)
	.loc	1 856 0                 # bzlib.c:856:0
	movq	-56(%rbp), %rsi
	movl	60(%rsi), %edi
	addq	$1096, %rsi             # imm = 0x448
	callq	BZ2_indexIntoF
	movq	-56(%rbp), %rcx
	movl	%eax, 64(%rcx)
	movq	-56(%rbp), %rdx
	movl	60(%rdx), %ebx
	movq	3160(%rdx), %rcx
	movq	3168(%rdx), %rsi
	movzbl	(%rcx,%rbx,2), %edi
	movq	$1, %rcx
	movq	%rbx, %rax
	shrq	%cl, %rax
	movzbl	(%rsi,%rax), %eax
	movq	$2, %rcx
	shlq	%cl, %rbx
	andq	$4, %rbx
	movq	%rbx, %rcx
	shrq	%cl, %rax
	andq	$15, %rax
	movq	$16, %rcx
	shlq	%cl, %rax
	orq	%rdi, %rax
	movl	%eax, 60(%rdx)
	movq	-56(%rbp), %rax
	movl	1092(%rax), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rax)
	jmp	.LBB15_45
.Ltmp325:
.LBB15_3:                               # %if.then3
	xorq	%rax, %rax
	jmp	.LBB15_10
.LBB15_9:                               # %if.then36
	movq	$1, %rax
.LBB15_10:                              # %return
	.loc	1 787 0                 # bzlib.c:787:0
.Ltmp326:
	movb	%al, -41(%rbp)
.Ltmp327:
	.loc	1 860 0                 # bzlib.c:860:0
	movzbl	-41(%rbp), %eax
	addq	$24, %rsp
	popq	%rbx
	popq	%r12
	popq	%r13
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp328:
.Ltmp329:
	.size	unRLE_obuf_to_output_SMALL, .Ltmp329-unRLE_obuf_to_output_SMALL
.Lfunc_end15:
	.cfi_endproc

	.align	16, 0x90
	.type	unRLE_obuf_to_output_FAST,@function
unRLE_obuf_to_output_FAST:              # @unRLE_obuf_to_output_FAST
.Lfunc_begin16:
	.loc	1 594 0                 # bzlib.c:594:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp333:
	.cfi_def_cfa_offset 16
.Ltmp334:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp335:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
.Ltmp336:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	.loc	1 597 0 prologue_end    # bzlib.c:597:0
.Ltmp337:
	movzbl	20(%rdi), %eax
	testq	%rax, %rax
	je	.LBB16_42
# BB#1:
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	movq	$1, %r11
	xorq	%r10, %r10
	movq	$2, %r9
	movq	$3, %r8
	jmp	.LBB16_2
	.align	16, 0x90
.LBB16_5:                               # %if.then23
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 610 0                 # bzlib.c:610:0
.Ltmp338:
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	40(%rcx), %edx
	incq	%rdx
	movl	%edx, 40(%rcx)
.Ltmp339:
.LBB16_2:                               # %while.body2
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 602 0                 # bzlib.c:602:0
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	32(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB16_57
.Ltmp340:
# BB#3:                                 # %if.end
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 603 0                 # bzlib.c:603:0
	movq	-24(%rbp), %rcx
	movl	16(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_4
.Ltmp341:
# BB#6:                                 # %while.end
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 614 0                 # bzlib.c:614:0
	movq	-24(%rbp), %rcx
	movl	64080(%rcx), %edx
	movl	1092(%rcx), %ecx
	incq	%rdx
	andq	%rax, %rdx
	cmpq	%rdx, %rcx
	je	.LBB16_57
.Ltmp342:
# BB#7:                                 # %if.end30
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 617 0                 # bzlib.c:617:0
	movq	-24(%rbp), %rcx
	movl	64080(%rcx), %edx
	movslq	1092(%rcx), %rsi
	incq	%rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rdx, %rsi
	jg	.LBB16_8
.Ltmp343:
# BB#9:                                 # %if.end37
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 620 0                 # bzlib.c:620:0
	movq	-24(%rbp), %rcx
	movl	%r11d, 16(%rcx)
	.loc	1 621 0                 # bzlib.c:621:0
	movq	-24(%rbp), %rcx
	movzbl	64(%rcx), %edx
	movb	%dl, 12(%rcx)
	.loc	1 622 0                 # bzlib.c:622:0
	movq	-24(%rbp), %rcx
	movl	60(%rcx), %edx
	movq	3152(%rcx), %rsi
	movl	(%rsi,%rdx,4), %edx
	movl	%edx, 60(%rcx)
	movq	-24(%rbp), %rcx
	movzbl	60(%rcx), %ecx
	movq	-24(%rbp), %rdx
	movb	%cl, -25(%rbp)
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, 60(%rdx)
	.loc	1 622 0                 # bzlib.c:622:0
.Ltmp344:
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_12
# BB#10:                                # %if.then50
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 622 0                 # bzlib.c:622:0
.Ltmp345:
	movq	-24(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 622 0                 # bzlib.c:622:0
.Ltmp346:
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB16_12
# BB#11:                                # %if.then59
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rcx
	movl	%r10d, 28(%rcx)
.Ltmp347:
.LBB16_12:                              # %if.end62
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 622 0                 # bzlib.c:622:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	.loc	1 623 0                 # bzlib.c:623:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	movq	$1, %rcx
	je	.LBB16_14
# BB#13:                                # %select.mid
                                        #   in Loop: Header=BB16_2 Depth=1
	xorq	%rcx, %rcx
.LBB16_14:                              # %select.end
                                        #   in Loop: Header=BB16_2 Depth=1
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rsi
	xorq	%rcx, %rdx
	movb	%dl, -25(%rbp)
	movl	1092(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rsi)
	.loc	1 624 0                 # bzlib.c:624:0
.Ltmp348:
	movq	-24(%rbp), %rcx
	movl	64080(%rcx), %edx
	movl	1092(%rcx), %ecx
	incq	%rdx
	andq	%rax, %rdx
	cmpq	%rdx, %rcx
	je	.LBB16_2
.Ltmp349:
# BB#15:                                # %if.end79
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 625 0                 # bzlib.c:625:0
	movq	-24(%rbp), %rcx
	movzbl	-25(%rbp), %edx
	movl	64(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB16_16
.Ltmp350:
# BB#17:                                # %if.end87
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 627 0                 # bzlib.c:627:0
	movq	-24(%rbp), %rcx
	movl	%r9d, 16(%rcx)
	.loc	1 628 0                 # bzlib.c:628:0
	movq	-24(%rbp), %rcx
	movl	60(%rcx), %edx
	movq	3152(%rcx), %rsi
	movl	(%rsi,%rdx,4), %edx
	movl	%edx, 60(%rcx)
	movq	-24(%rbp), %rcx
	movzbl	60(%rcx), %ecx
	movq	-24(%rbp), %rdx
	movb	%cl, -25(%rbp)
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, 60(%rdx)
	.loc	1 628 0                 # bzlib.c:628:0
.Ltmp351:
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_20
# BB#18:                                # %if.then102
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 628 0                 # bzlib.c:628:0
.Ltmp352:
	movq	-24(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 628 0                 # bzlib.c:628:0
.Ltmp353:
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB16_20
# BB#19:                                # %if.then112
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rcx
	movl	%r10d, 28(%rcx)
.Ltmp354:
.LBB16_20:                              # %if.end115
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 628 0                 # bzlib.c:628:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	.loc	1 629 0                 # bzlib.c:629:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	movq	$1, %rcx
	je	.LBB16_22
# BB#21:                                # %select.mid9
                                        #   in Loop: Header=BB16_2 Depth=1
	xorq	%rcx, %rcx
.LBB16_22:                              # %select.end8
                                        #   in Loop: Header=BB16_2 Depth=1
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rsi
	xorq	%rcx, %rdx
	movb	%dl, -25(%rbp)
	movl	1092(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rsi)
	.loc	1 630 0                 # bzlib.c:630:0
.Ltmp355:
	movq	-24(%rbp), %rcx
	movl	64080(%rcx), %edx
	movl	1092(%rcx), %ecx
	incq	%rdx
	andq	%rax, %rdx
	cmpq	%rdx, %rcx
	je	.LBB16_2
.Ltmp356:
# BB#23:                                # %if.end133
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 631 0                 # bzlib.c:631:0
	movq	-24(%rbp), %rcx
	movzbl	-25(%rbp), %edx
	movl	64(%rcx), %ecx
	cmpq	%rcx, %rdx
	jne	.LBB16_16
.Ltmp357:
# BB#24:                                # %if.end141
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 633 0                 # bzlib.c:633:0
	movq	-24(%rbp), %rcx
	movl	%r8d, 16(%rcx)
	.loc	1 634 0                 # bzlib.c:634:0
	movq	-24(%rbp), %rcx
	movl	60(%rcx), %edx
	movq	3152(%rcx), %rsi
	movl	(%rsi,%rdx,4), %edx
	movl	%edx, 60(%rcx)
	movq	-24(%rbp), %rcx
	movzbl	60(%rcx), %ecx
	movq	-24(%rbp), %rdx
	movb	%cl, -25(%rbp)
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, 60(%rdx)
	.loc	1 634 0                 # bzlib.c:634:0
.Ltmp358:
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_27
# BB#25:                                # %if.then156
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 634 0                 # bzlib.c:634:0
.Ltmp359:
	movq	-24(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 634 0                 # bzlib.c:634:0
.Ltmp360:
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB16_27
# BB#26:                                # %if.then166
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rcx
	movl	%r10d, 28(%rcx)
.Ltmp361:
.LBB16_27:                              # %if.end169
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 634 0                 # bzlib.c:634:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	.loc	1 635 0                 # bzlib.c:635:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	movq	$1, %rcx
	je	.LBB16_29
# BB#28:                                # %select.mid11
                                        #   in Loop: Header=BB16_2 Depth=1
	xorq	%rcx, %rcx
.LBB16_29:                              # %select.end10
                                        #   in Loop: Header=BB16_2 Depth=1
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rsi
	xorq	%rcx, %rdx
	movb	%dl, -25(%rbp)
	movl	1092(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rsi)
	.loc	1 636 0                 # bzlib.c:636:0
.Ltmp362:
	movq	-24(%rbp), %rcx
	movl	64080(%rcx), %edx
	movl	1092(%rcx), %ecx
	incq	%rdx
	andq	%rax, %rdx
	cmpq	%rdx, %rcx
	je	.LBB16_2
.Ltmp363:
# BB#30:                                # %if.end187
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 637 0                 # bzlib.c:637:0
	movq	-24(%rbp), %rcx
	movzbl	-25(%rbp), %edx
	movl	64(%rcx), %ecx
	cmpq	%rcx, %rdx
	je	.LBB16_31
.Ltmp364:
.LBB16_16:                              # %if.then84
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 625 0                 # bzlib.c:625:0
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rdx
	movl	%ecx, 64(%rdx)
	jmp	.LBB16_2
.Ltmp365:
	.align	16, 0x90
.LBB16_4:                               # %if.end6
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 604 0                 # bzlib.c:604:0
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rdi
	movzbl	12(%rcx), %ecx
	movq	24(%rdi), %rdi
	movb	%cl, (%rdi)
	.loc	1 605 0                 # bzlib.c:605:0
.Ltmp366:
	movq	-24(%rbp), %rdi
	movl	3184(%rdi), %esi
	movq	$8, %rcx
	movq	%rsi, %rdx
	movzbl	12(%rdi), %ebx
	shlq	%cl, %rdx
	movq	$24, %rcx
	shrq	%cl, %rsi
	xorq	%rsi, %rbx
	movl	BZ2_crc32Table(,%rbx,4), %ecx
	xorq	%rdx, %rcx
	movl	%ecx, 3184(%rdi)
.Ltmp367:
	.loc	1 606 0                 # bzlib.c:606:0
	movq	-24(%rbp), %rcx
	movl	16(%rcx), %edx
	decq	%rdx
	movl	%edx, 16(%rcx)
	.loc	1 607 0                 # bzlib.c:607:0
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	incq	24(%rcx)
	.loc	1 608 0                 # bzlib.c:608:0
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	32(%rcx), %edx
	decq	%rdx
	movl	%edx, 32(%rcx)
	.loc	1 609 0                 # bzlib.c:609:0
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	36(%rcx), %edx
	incq	%rdx
	movl	%edx, 36(%rcx)
	.loc	1 610 0                 # bzlib.c:610:0
.Ltmp368:
	movq	-24(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	36(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_2
	jmp	.LBB16_5
.Ltmp369:
.LBB16_31:                              # %if.end195
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 639 0                 # bzlib.c:639:0
	movq	-24(%rbp), %rcx
	movl	60(%rcx), %edx
	movq	3152(%rcx), %rsi
	movl	(%rsi,%rdx,4), %edx
	movl	%edx, 60(%rcx)
	movq	-24(%rbp), %rcx
	movzbl	60(%rcx), %ecx
	movq	-24(%rbp), %rdx
	movb	%cl, -25(%rbp)
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, 60(%rdx)
	.loc	1 639 0                 # bzlib.c:639:0
.Ltmp370:
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_34
# BB#32:                                # %if.then209
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 639 0                 # bzlib.c:639:0
.Ltmp371:
	movq	-24(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 639 0                 # bzlib.c:639:0
.Ltmp372:
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB16_34
# BB#33:                                # %if.then219
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rcx
	movl	%r10d, 28(%rcx)
.Ltmp373:
.LBB16_34:                              # %if.end222
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 639 0                 # bzlib.c:639:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	.loc	1 640 0                 # bzlib.c:640:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	movq	$1, %rcx
	je	.LBB16_36
# BB#35:                                # %select.mid13
                                        #   in Loop: Header=BB16_2 Depth=1
	xorq	%rcx, %rcx
.LBB16_36:                              # %select.end12
                                        #   in Loop: Header=BB16_2 Depth=1
	movzbl	-25(%rbp), %edx
	movq	-24(%rbp), %rsi
	xorq	%rcx, %rdx
	movb	%dl, -25(%rbp)
	movl	1092(%rsi), %ecx
	incq	%rcx
	movl	%ecx, 1092(%rsi)
	.loc	1 641 0                 # bzlib.c:641:0
	movzbl	-25(%rbp), %ecx
	movq	-24(%rbp), %rdx
	addq	$4, %rcx
	movl	%ecx, 16(%rdx)
	.loc	1 642 0                 # bzlib.c:642:0
	movq	-24(%rbp), %rcx
	movl	60(%rcx), %edx
	movq	3152(%rcx), %rsi
	movl	(%rsi,%rdx,4), %edx
	movl	%edx, 60(%rcx)
	movq	-24(%rbp), %rcx
	movzbl	60(%rcx), %edx
	movl	%edx, 64(%rcx)
	movq	-24(%rbp), %rdx
	movl	60(%rdx), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, 60(%rdx)
	.loc	1 642 0                 # bzlib.c:642:0
.Ltmp374:
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB16_39
# BB#37:                                # %if.then252
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 642 0                 # bzlib.c:642:0
.Ltmp375:
	movq	-24(%rbp), %rcx
	movslq	28(%rcx), %rdx
	movl	BZ2_rNums(,%rdx,4), %edx
	movl	%edx, 24(%rcx)
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %edx
	incq	%rdx
	movl	%edx, 28(%rcx)
	.loc	1 642 0                 # bzlib.c:642:0
.Ltmp376:
	movq	-24(%rbp), %rcx
	movl	28(%rcx), %ecx
	cmpq	$512, %rcx              # imm = 0x200
	jne	.LBB16_39
# BB#38:                                # %if.then262
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rcx
	movl	%r10d, 28(%rcx)
.Ltmp377:
.LBB16_39:                              # %if.end265
                                        #   in Loop: Header=BB16_2 Depth=1
	.loc	1 642 0                 # bzlib.c:642:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %edx
	decq	%rdx
	movl	%edx, 24(%rcx)
	.loc	1 643 0                 # bzlib.c:643:0
	movq	-24(%rbp), %rcx
	movl	24(%rcx), %ecx
	cmpq	$1, %rcx
	movq	$1, %rcx
	je	.LBB16_41
# BB#40:                                # %select.mid15
                                        #   in Loop: Header=BB16_2 Depth=1
	xorq	%rcx, %rcx
.LBB16_41:                              # %select.end14
                                        #   in Loop: Header=BB16_2 Depth=1
	movq	-24(%rbp), %rdx
	movl	64(%rdx), %esi
	xorq	%rcx, %rsi
	movl	%esi, 64(%rdx)
	movq	-24(%rbp), %rcx
	movl	1092(%rcx), %edx
	incq	%rdx
	movl	%edx, 1092(%rcx)
	jmp	.LBB16_2
.Ltmp378:
.LBB16_42:                              # %if.else
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	.loc	1 649 0                 # bzlib.c:649:0
	movq	-24(%rbp), %rax
	movl	3184(%rax), %eax
	.loc	1 650 0                 # bzlib.c:650:0
	movq	-24(%rbp), %rcx
	.loc	1 649 0                 # bzlib.c:649:0
	movl	%eax, -32(%rbp)
.Ltmp379:
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	.loc	1 650 0                 # bzlib.c:650:0
	movzbl	12(%rcx), %eax
	.loc	1 651 0                 # bzlib.c:651:0
	movq	-24(%rbp), %rcx
	.loc	1 650 0                 # bzlib.c:650:0
	movb	%al, -33(%rbp)
.Ltmp380:
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	.loc	1 651 0                 # bzlib.c:651:0
	movl	16(%rcx), %eax
	.loc	1 652 0                 # bzlib.c:652:0
	movq	-24(%rbp), %rcx
	.loc	1 651 0                 # bzlib.c:651:0
	movl	%eax, -40(%rbp)
.Ltmp381:
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	.loc	1 652 0                 # bzlib.c:652:0
	movl	1092(%rcx), %eax
	.loc	1 653 0                 # bzlib.c:653:0
	movq	-24(%rbp), %rcx
	.loc	1 652 0                 # bzlib.c:652:0
	movl	%eax, -44(%rbp)
.Ltmp382:
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	.loc	1 653 0                 # bzlib.c:653:0
	movl	64(%rcx), %eax
	.loc	1 654 0                 # bzlib.c:654:0
	movq	-24(%rbp), %rcx
	.loc	1 653 0                 # bzlib.c:653:0
	movl	%eax, -48(%rbp)
.Ltmp383:
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	.loc	1 654 0                 # bzlib.c:654:0
	movq	3152(%rcx), %rax
	.loc	1 655 0                 # bzlib.c:655:0
	movq	-24(%rbp), %rcx
	.loc	1 654 0                 # bzlib.c:654:0
	movq	%rax, -56(%rbp)
.Ltmp384:
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	.loc	1 655 0                 # bzlib.c:655:0
	movl	60(%rcx), %eax
	.loc	1 656 0                 # bzlib.c:656:0
	movq	-24(%rbp), %rcx
	.loc	1 655 0                 # bzlib.c:655:0
	movl	%eax, -60(%rbp)
.Ltmp385:
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	.loc	1 656 0                 # bzlib.c:656:0
	movq	(%rcx), %rax
	movq	24(%rax), %rax
	.loc	1 657 0                 # bzlib.c:657:0
	movq	-24(%rbp), %rcx
	.loc	1 656 0                 # bzlib.c:656:0
	movq	%rax, -72(%rbp)
.Ltmp386:
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	.loc	1 657 0                 # bzlib.c:657:0
	movq	(%rcx), %rax
	movl	32(%rax), %eax
	.loc	1 661 0                 # bzlib.c:661:0
	movq	-24(%rbp), %rcx
	.loc	1 657 0                 # bzlib.c:657:0
	movl	%eax, -76(%rbp)
.Ltmp387:
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	.loc	1 660 0                 # bzlib.c:660:0
	movl	%eax, -80(%rbp)
.Ltmp388:
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	.loc	1 661 0                 # bzlib.c:661:0
	movl	64080(%rcx), %eax
	incq	%rax
	movl	%eax, -84(%rbp)
.Ltmp389:
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	movq	$2, %rax
	movabsq	$4294967295, %rdx       # imm = 0xFFFFFFFF
	movq	$3, %r8
	jmp	.LBB16_43
.LBB16_64:                              # %if.then366
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 707 0                 # bzlib.c:707:0
.Ltmp390:
	movzbl	-25(%rbp), %ecx
	movl	%ecx, -48(%rbp)
.Ltmp391:
	.align	16, 0x90
.LBB16_43:                              # %while.body289
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB16_44 Depth 2
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 667 0                 # bzlib.c:667:0
	movslq	-40(%rbp), %rcx
	testq	%rcx, %rcx
	jle	.LBB16_50
	jmp	.LBB16_44
	.align	16, 0x90
.LBB16_46:                              # %if.end302
                                        #   in Loop: Header=BB16_44 Depth=2
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 671 0                 # bzlib.c:671:0
.Ltmp392:
	movzbl	-33(%rbp), %ecx
	movq	-72(%rbp), %rdi
	movb	%cl, (%rdi)
	.loc	1 672 0                 # bzlib.c:672:0
.Ltmp393:
	movl	-32(%rbp), %edi
	movq	$8, %rcx
	movq	%rdi, %rbx
	shlq	%cl, %rbx
	movzbl	-33(%rbp), %esi
	movq	$24, %rcx
	shrq	%cl, %rdi
	xorq	%rdi, %rsi
	movl	BZ2_crc32Table(,%rsi,4), %ecx
.Ltmp394:
	.loc	1 673 0                 # bzlib.c:673:0
	movl	-40(%rbp), %esi
	.loc	1 672 0                 # bzlib.c:672:0
.Ltmp395:
	xorq	%rbx, %rcx
	movl	%ecx, -32(%rbp)
.Ltmp396:
	.loc	1 673 0                 # bzlib.c:673:0
	decq	%rsi
	movl	%esi, -40(%rbp)
	.loc	1 674 0                 # bzlib.c:674:0
	incq	-72(%rbp)
	.loc	1 675 0                 # bzlib.c:675:0
	movl	-76(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -76(%rbp)
.LBB16_44:                              # %while.body294
                                        #   Parent Loop BB16_43 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 669 0                 # bzlib.c:669:0
.Ltmp397:
	movl	-76(%rbp), %ecx
	testq	%rcx, %rcx
	je	.LBB16_54
.Ltmp398:
# BB#45:                                # %if.end298
                                        #   in Loop: Header=BB16_44 Depth=2
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 670 0                 # bzlib.c:670:0
	movl	-40(%rbp), %ecx
	cmpq	$1, %rcx
	jne	.LBB16_46
.Ltmp399:
.LBB16_47:                              # %s_state_out_len_eq_one
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 679 0                 # bzlib.c:679:0
	movl	-76(%rbp), %ecx
	testq	%rcx, %rcx
	je	.LBB16_48
.Ltmp400:
# BB#49:                                # %if.end317
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 682 0                 # bzlib.c:682:0
	movzbl	-33(%rbp), %ecx
	movq	-72(%rbp), %rsi
	movb	%cl, (%rsi)
	.loc	1 683 0                 # bzlib.c:683:0
.Ltmp401:
	movl	-32(%rbp), %esi
	movq	$8, %rcx
	movq	%rsi, %rdi
	movzbl	-33(%rbp), %ebx
	shlq	%cl, %rdi
	movq	$24, %rcx
	shrq	%cl, %rsi
	xorq	%rsi, %rbx
	movl	BZ2_crc32Table(,%rbx,4), %ecx
	xorq	%rdi, %rcx
	movl	%ecx, -32(%rbp)
.Ltmp402:
	.loc	1 684 0                 # bzlib.c:684:0
	incq	-72(%rbp)
	.loc	1 685 0                 # bzlib.c:685:0
	movl	-76(%rbp), %ecx
	decq	%rcx
	movl	%ecx, -76(%rbp)
.Ltmp403:
.LBB16_50:                              # %if.end327
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 689 0                 # bzlib.c:689:0
	movslq	-44(%rbp), %rcx
	movslq	-84(%rbp), %rsi
	cmpq	%rsi, %rcx
	jg	.LBB16_8
.Ltmp404:
# BB#51:                                # %if.end331
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 693 0                 # bzlib.c:693:0
	movl	-44(%rbp), %ecx
	movl	-84(%rbp), %esi
	cmpq	%rsi, %rcx
	je	.LBB16_52
.Ltmp405:
# BB#59:                                # %if.end335
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 696 0                 # bzlib.c:696:0
	movzbl	-48(%rbp), %ecx
	.loc	1 697 0                 # bzlib.c:697:0
	movl	-60(%rbp), %esi
	movq	-56(%rbp), %rdi
	.loc	1 696 0                 # bzlib.c:696:0
	movb	%cl, -33(%rbp)
	.loc	1 697 0                 # bzlib.c:697:0
	movl	(%rdi,%rsi,4), %ecx
	movl	%ecx, -60(%rbp)
	movzbl	-60(%rbp), %ecx
	movb	%cl, -25(%rbp)
	movl	-60(%rbp), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	-44(%rbp), %ecx
	.loc	1 698 0                 # bzlib.c:698:0
.Ltmp406:
	movzbl	-25(%rbp), %edi
	movl	-48(%rbp), %ebx
.Ltmp407:
	.loc	1 697 0                 # bzlib.c:697:0
	movl	%esi, -60(%rbp)
	incq	%rcx
	movl	%ecx, -44(%rbp)
	.loc	1 698 0                 # bzlib.c:698:0
.Ltmp408:
	cmpq	%rbx, %rdi
	je	.LBB16_61
# BB#60:                                # %if.then346
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 699 0                 # bzlib.c:699:0
.Ltmp409:
	movzbl	-25(%rbp), %ecx
	movl	%ecx, -48(%rbp)
	jmp	.LBB16_47
.Ltmp410:
	.align	16, 0x90
.LBB16_61:                              # %if.end348
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 701 0                 # bzlib.c:701:0
	movl	-44(%rbp), %ecx
	movl	-84(%rbp), %esi
	cmpq	%rsi, %rcx
	je	.LBB16_47
.Ltmp411:
# BB#62:                                # %if.end352
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 705 0                 # bzlib.c:705:0
	movl	-60(%rbp), %ecx
	movq	-56(%rbp), %rsi
	.loc	1 704 0                 # bzlib.c:704:0
	movl	%eax, -40(%rbp)
	.loc	1 705 0                 # bzlib.c:705:0
	movl	(%rsi,%rcx,4), %ecx
	movl	%ecx, -60(%rbp)
	movzbl	-60(%rbp), %ecx
	movb	%cl, -25(%rbp)
	movl	-60(%rbp), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	-44(%rbp), %ecx
	movl	%esi, -60(%rbp)
	.loc	1 706 0                 # bzlib.c:706:0
.Ltmp412:
	movl	-84(%rbp), %esi
.Ltmp413:
	.loc	1 705 0                 # bzlib.c:705:0
	incq	%rcx
	movl	%ecx, -44(%rbp)
	andq	%rdx, %rcx
	cmpq	%rsi, %rcx
	je	.LBB16_43
# BB#63:                                # %if.end362
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 707 0                 # bzlib.c:707:0
.Ltmp414:
	movzbl	-25(%rbp), %ecx
	movl	-48(%rbp), %esi
	cmpq	%rsi, %rcx
	jne	.LBB16_64
.Ltmp415:
# BB#65:                                # %if.end368
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 710 0                 # bzlib.c:710:0
	movl	-60(%rbp), %ecx
	movq	-56(%rbp), %rsi
	.loc	1 709 0                 # bzlib.c:709:0
	movl	%r8d, -40(%rbp)
	.loc	1 710 0                 # bzlib.c:710:0
	movl	(%rsi,%rcx,4), %ecx
	movl	%ecx, -60(%rbp)
	movzbl	-60(%rbp), %ecx
	movb	%cl, -25(%rbp)
	movl	-60(%rbp), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	-44(%rbp), %ecx
	movl	%esi, -60(%rbp)
	.loc	1 711 0                 # bzlib.c:711:0
.Ltmp416:
	movl	-84(%rbp), %esi
.Ltmp417:
	.loc	1 710 0                 # bzlib.c:710:0
	incq	%rcx
	movl	%ecx, -44(%rbp)
	andq	%rdx, %rcx
	cmpq	%rsi, %rcx
	je	.LBB16_43
# BB#66:                                # %if.end378
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 712 0                 # bzlib.c:712:0
.Ltmp418:
	movzbl	-25(%rbp), %ecx
	movl	-48(%rbp), %esi
	cmpq	%rsi, %rcx
	jne	.LBB16_64
.Ltmp419:
# BB#67:                                # %if.end384
                                        #   in Loop: Header=BB16_43 Depth=1
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 714 0                 # bzlib.c:714:0
	movl	-60(%rbp), %ecx
	movq	-56(%rbp), %rsi
	movl	(%rsi,%rcx,4), %ecx
	movl	%ecx, -60(%rbp)
	movzbl	-60(%rbp), %ecx
	movb	%cl, -25(%rbp)
	movl	-60(%rbp), %esi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, -60(%rbp)
	movl	-44(%rbp), %ecx
	incq	%rcx
	.loc	1 715 0                 # bzlib.c:715:0
	movzbl	-25(%rbp), %esi
	.loc	1 716 0                 # bzlib.c:716:0
	movl	-60(%rbp), %edi
	movq	-56(%rbp), %rbx
	.loc	1 714 0                 # bzlib.c:714:0
	movl	%ecx, -44(%rbp)
	.loc	1 715 0                 # bzlib.c:715:0
	addq	$4, %rsi
	movl	%esi, -40(%rbp)
	.loc	1 716 0                 # bzlib.c:716:0
	movl	(%rbx,%rdi,4), %ecx
	movl	%ecx, -60(%rbp)
	movzbl	-60(%rbp), %ecx
	movl	%ecx, -48(%rbp)
	movl	-60(%rbp), %esi
	movl	-44(%rbp), %edi
	movq	$8, %rcx
	shrq	%cl, %rsi
	movl	%esi, -60(%rbp)
	incq	%rdi
	movl	%edi, -44(%rbp)
	jmp	.LBB16_43
.Ltmp420:
.LBB16_8:                               # %if.then36
	movq	$1, %rax
	jmp	.LBB16_58
.LBB16_52:                              # %if.then334
.Ltmp421:
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	xorq	%rax, %rax
	jmp	.LBB16_53
.LBB16_48:                              # %if.then316
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	movq	$1, %rax
.Ltmp422:
.LBB16_53:                              # %return_notr
	.loc	1 694 0                 # bzlib.c:694:0
	movl	%eax, -40(%rbp)
.Ltmp423:
.LBB16_54:                              # %return_notr
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 720 0                 # bzlib.c:720:0
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %eax
	.loc	1 721 0                 # bzlib.c:721:0
	movq	-24(%rbp), %rcx
	.loc	1 720 0                 # bzlib.c:720:0
	movl	%eax, -88(%rbp)
	.loc	1 721 0                 # bzlib.c:721:0
	movq	(%rcx), %rax
	movl	-80(%rbp), %ecx
	movl	-76(%rbp), %edx
	movl	36(%rax), %esi
	subq	%rdx, %rcx
	addq	%rcx, %rsi
	movl	%esi, 36(%rax)
	.loc	1 722 0                 # bzlib.c:722:0
.Ltmp424:
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movl	36(%rax), %eax
	movl	-88(%rbp), %ecx
	cmpq	%rcx, %rax
	jae	.LBB16_56
# BB#55:                                # %if.then409
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 723 0                 # bzlib.c:723:0
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movl	40(%rax), %ecx
	incq	%rcx
	movl	%ecx, 40(%rax)
.Ltmp425:
.LBB16_56:                              # %if.end425
	#DEBUG_VALUE: c_calculatedBlockCRC <- [RBP+-32]
	#DEBUG_VALUE: c_state_out_ch <- [RBP+-33]
	#DEBUG_VALUE: c_state_out_len <- [RBP+-40]
	#DEBUG_VALUE: c_nblock_used <- [RBP+-44]
	#DEBUG_VALUE: c_k0 <- [RBP+-48]
	#DEBUG_VALUE: c_tt <- [RBP+-56]
	#DEBUG_VALUE: c_tPos <- [RBP+-60]
	#DEBUG_VALUE: cs_next_out <- [RBP+-72]
	#DEBUG_VALUE: cs_avail_out <- [RBP+-76]
	#DEBUG_VALUE: avail_out_INIT <- [RBP+-80]
	#DEBUG_VALUE: s_save_nblockPP <- [RBP+-84]
	#DEBUG_VALUE: total_out_lo32_old <- [RBP+-88]
	.loc	1 726 0                 # bzlib.c:726:0
	movl	-32(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 3184(%rcx)
	.loc	1 727 0                 # bzlib.c:727:0
	movzbl	-33(%rbp), %eax
	movq	-24(%rbp), %rcx
	movb	%al, 12(%rcx)
	.loc	1 728 0                 # bzlib.c:728:0
	movl	-40(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 16(%rcx)
	.loc	1 729 0                 # bzlib.c:729:0
	movl	-44(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 1092(%rcx)
	.loc	1 730 0                 # bzlib.c:730:0
	movl	-48(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 64(%rcx)
	.loc	1 731 0                 # bzlib.c:731:0
	movq	-56(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, 3152(%rcx)
	.loc	1 732 0                 # bzlib.c:732:0
	movl	-60(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, 60(%rcx)
	.loc	1 733 0                 # bzlib.c:733:0
	movq	-24(%rbp), %rax
	movq	-72(%rbp), %rcx
	movq	(%rax), %rax
	movq	%rcx, 24(%rax)
	.loc	1 734 0                 # bzlib.c:734:0
	movq	-24(%rbp), %rax
	movl	-76(%rbp), %ecx
	movq	(%rax), %rax
	movl	%ecx, 32(%rax)
.Ltmp426:
.LBB16_57:                              # %return
	xorq	%rax, %rax
.LBB16_58:                              # %return
	.loc	1 737 0                 # bzlib.c:737:0
	movb	%al, -9(%rbp)
	.loc	1 738 0                 # bzlib.c:738:0
	movzbl	-9(%rbp), %eax
	popq	%rbx
	popq	%rbp
	retq
.Ltmp427:
.Ltmp428:
	.size	unRLE_obuf_to_output_FAST, .Ltmp428-unRLE_obuf_to_output_FAST
.Lfunc_end16:
	.cfi_endproc

	.globl	BZ2_bzDecompressEnd
	.align	16, 0x90
	.type	BZ2_bzDecompressEnd,@function
BZ2_bzDecompressEnd:                    # @BZ2_bzDecompressEnd
.Lfunc_begin17:
	.loc	1 919 0                 # bzlib.c:919:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp431:
	.cfi_def_cfa_offset 16
.Ltmp432:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp433:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	testq	%rdi, %rdi
	movq	%rdi, -16(%rbp)
	je	.LBB17_1
# BB#2:                                 # %if.end
	.loc	1 922 0 prologue_end    # bzlib.c:922:0
.Ltmp434:
	movq	-16(%rbp), %rax
	movq	48(%rax), %rax
	testq	%rax, %rax
	movq	%rax, -24(%rbp)
	je	.LBB17_1
# BB#3:                                 # %if.end3
	.loc	1 924 0                 # bzlib.c:924:0
.Ltmp435:
	movq	-24(%rbp), %rax
	movq	(%rax), %rax
	movq	-16(%rbp), %rcx
	cmpq	%rcx, %rax
	je	.LBB17_4
.Ltmp436:
.LBB17_1:                               # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB17_11:                              # %return
	.loc	1 933 0                 # bzlib.c:933:0
	movl	%eax, -4(%rbp)
	.loc	1 934 0                 # bzlib.c:934:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB17_4:                               # %if.end7
	.loc	1 926 0                 # bzlib.c:926:0
.Ltmp437:
	movq	-24(%rbp), %rax
	cmpq	$0, 3152(%rax)
	je	.LBB17_6
# BB#5:                                 # %if.then9
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	3152(%rcx), %rsi
	callq	*64(%rax)
.Ltmp438:
.LBB17_6:                               # %if.end11
	.loc	1 927 0                 # bzlib.c:927:0
	movq	-24(%rbp), %rax
	cmpq	$0, 3160(%rax)
	je	.LBB17_8
# BB#7:                                 # %if.then13
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	3160(%rcx), %rsi
	callq	*64(%rax)
.Ltmp439:
.LBB17_8:                               # %if.end17
	.loc	1 928 0                 # bzlib.c:928:0
	movq	-24(%rbp), %rax
	cmpq	$0, 3168(%rax)
	je	.LBB17_10
# BB#9:                                 # %if.then19
	movq	-16(%rbp), %rax
	movq	-24(%rbp), %rcx
	movq	72(%rax), %rdi
	movq	3168(%rcx), %rsi
	callq	*64(%rax)
.Ltmp440:
.LBB17_10:                              # %if.end23
	.loc	1 930 0                 # bzlib.c:930:0
	movq	-16(%rbp), %rax
	movq	48(%rax), %rsi
	movq	72(%rax), %rdi
	callq	*64(%rax)
	.loc	1 931 0                 # bzlib.c:931:0
	movq	-16(%rbp), %rax
	movq	$0, 48(%rax)
	xorq	%rax, %rax
	jmp	.LBB17_11
.Ltmp441:
.Ltmp442:
	.size	BZ2_bzDecompressEnd, .Ltmp442-BZ2_bzDecompressEnd
.Lfunc_end17:
	.cfi_endproc

	.globl	BZ2_bzWriteOpen
	.align	16, 0x90
	.type	BZ2_bzWriteOpen,@function
BZ2_bzWriteOpen:                        # @BZ2_bzWriteOpen
.Lfunc_begin18:
	.loc	1 990 0                 # bzlib.c:990:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp446:
	.cfi_def_cfa_offset 16
.Ltmp447:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp448:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$56, %rsp
.Ltmp449:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	.loc	1 994 0 prologue_end    # bzlib.c:994:0
.Ltmp450:
	cmpq	$0, -24(%rbp)
	movl	%ecx, -40(%rbp)
	movl	%r8d, -44(%rbp)
.Ltmp451:
	.loc	1 992 0                 # bzlib.c:992:0
	movq	$0, -56(%rbp)
	je	.LBB18_2
# BB#1:                                 # %if.then
	.loc	1 994 0                 # bzlib.c:994:0
.Ltmp452:
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp453:
.LBB18_2:                               # %if.end
	.loc	1 994 0                 # bzlib.c:994:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_4
# BB#3:                                 # %if.then2
	movq	-56(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp454:
.LBB18_4:                               # %if.end3
	.loc	1 996 0                 # bzlib.c:996:0
	cmpq	$0, -32(%rbp)
	je	.LBB18_11
# BB#5:                                 # %lor.lhs.false
	movslq	-36(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB18_11
# BB#6:                                 # %lor.lhs.false6
	movslq	-36(%rbp), %rax
	cmpq	$9, %rax
	jg	.LBB18_11
# BB#7:                                 # %lor.lhs.false8
	movslq	-44(%rbp), %rax
	testq	%rax, %rax
	js	.LBB18_11
# BB#8:                                 # %lor.lhs.false10
	movslq	-44(%rbp), %rax
	cmpq	$250, %rax
	jg	.LBB18_11
# BB#9:                                 # %lor.lhs.false12
	movslq	-40(%rbp), %rax
	testq	%rax, %rax
	js	.LBB18_11
# BB#10:                                # %lor.lhs.false14
	movslq	-40(%rbp), %rax
	cmpq	$5, %rax
	jl	.LBB18_16
.LBB18_11:                              # %if.then16
	.loc	1 1000 0                # bzlib.c:1000:0
.Ltmp455:
	cmpq	$0, -24(%rbp)
	je	.LBB18_13
# BB#12:                                # %if.then18
	movq	-24(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, (%rax)
.Ltmp456:
.LBB18_13:                              # %if.end19
	.loc	1 1000 0                # bzlib.c:1000:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_15
# BB#14:                                # %if.then21
	movq	-56(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, 5096(%rax)
.Ltmp457:
.LBB18_15:                              # %if.end23
	.loc	1 1000 0                # bzlib.c:1000:0
	movq	$0, -16(%rbp)
.Ltmp458:
.LBB18_41:                              # %return
	.loc	1 1027 0                # bzlib.c:1027:0
	movq	-16(%rbp), %rax
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB18_16:                              # %if.end24
	.loc	1 1002 0                # bzlib.c:1002:0
.Ltmp459:
	movq	-32(%rbp), %rdi
	callq	ferror
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB18_22
# BB#17:                                # %if.then25
	.loc	1 1003 0                # bzlib.c:1003:0
.Ltmp460:
	cmpq	$0, -24(%rbp)
	je	.LBB18_19
# BB#18:                                # %if.then27
	movq	-24(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, (%rax)
.Ltmp461:
.LBB18_19:                              # %if.end28
	.loc	1 1003 0                # bzlib.c:1003:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_21
# BB#20:                                # %if.then30
	movq	-56(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, 5096(%rax)
.Ltmp462:
.LBB18_21:                              # %if.end32
	.loc	1 1003 0                # bzlib.c:1003:0
	movq	$0, -16(%rbp)
	jmp	.LBB18_41
.Ltmp463:
.LBB18_22:                              # %if.end33
	movq	$5104, %rdi             # imm = 0x13F0
	.loc	1 1005 0                # bzlib.c:1005:0
	callq	malloc
	testq	%rax, %rax
	movq	%rax, -56(%rbp)
	je	.LBB18_23
# BB#28:                                # %if.end44
	.loc	1 1009 0                # bzlib.c:1009:0
.Ltmp464:
	cmpq	$0, -24(%rbp)
	je	.LBB18_30
# BB#29:                                # %if.then46
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp465:
.LBB18_30:                              # %if.end47
	.loc	1 1009 0                # bzlib.c:1009:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_32
# BB#31:                                # %if.then49
	movq	-56(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp466:
.LBB18_32:                              # %if.end51
	.loc	1 1010 0                # bzlib.c:1010:0
	movq	-56(%rbp), %rax
	xorq	%rcx, %rcx
	movb	%cl, 5100(%rax)
	.loc	1 1011 0                # bzlib.c:1011:0
	movq	-56(%rbp), %rax
	movl	%ecx, 5008(%rax)
	.loc	1 1012 0                # bzlib.c:1012:0
	movq	-32(%rbp), %rax
	movq	-56(%rbp), %rcx
	movq	%rax, (%rcx)
	.loc	1 1013 0                # bzlib.c:1013:0
	movq	-56(%rbp), %rax
	movq	$1, %rcx
	movb	%cl, 5012(%rax)
	.loc	1 1014 0                # bzlib.c:1014:0
	movq	-56(%rbp), %rax
	movq	$0, 5072(%rax)
	.loc	1 1015 0                # bzlib.c:1015:0
	movq	-56(%rbp), %rax
	movq	$0, 5080(%rax)
	.loc	1 1016 0                # bzlib.c:1016:0
	movq	-56(%rbp), %rax
	movq	$0, 5088(%rax)
	.loc	1 1018 0                # bzlib.c:1018:0
.Ltmp467:
	movl	-44(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB18_34
# BB#33:                                # %if.then55
	movq	$30, %rax
	movl	%eax, -44(%rbp)
.Ltmp468:
.LBB18_34:                              # %if.end56
	.loc	1 1019 0                # bzlib.c:1019:0
	movq	-56(%rbp), %rdi
	movl	-44(%rbp), %ecx
	movl	-40(%rbp), %edx
	movl	-36(%rbp), %esi
	addq	$5016, %rdi             # imm = 0x1398
	callq	BZ2_bzCompressInit
	testq	%rbx, %rax
	movl	%eax, -48(%rbp)
	.loc	1 1021 0                # bzlib.c:1021:0
.Ltmp469:
	je	.LBB18_40
# BB#35:                                # %if.then60
	.loc	1 1022 0                # bzlib.c:1022:0
.Ltmp470:
	cmpq	$0, -24(%rbp)
	je	.LBB18_37
# BB#36:                                # %if.then62
	movl	-48(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp471:
.LBB18_37:                              # %if.end63
	.loc	1 1022 0                # bzlib.c:1022:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_39
# BB#38:                                # %if.then65
	movl	-48(%rbp), %eax
	movq	-56(%rbp), %rcx
	movl	%eax, 5096(%rcx)
.Ltmp472:
.LBB18_39:                              # %if.end67
	.loc	1 1022 0                # bzlib.c:1022:0
	movq	-56(%rbp), %rdi
	callq	free
	movq	$0, -16(%rbp)
	jmp	.LBB18_41
.Ltmp473:
.LBB18_23:                              # %if.then36
	.loc	1 1007 0                # bzlib.c:1007:0
	cmpq	$0, -24(%rbp)
	je	.LBB18_25
# BB#24:                                # %if.then38
	movq	-24(%rbp), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	movl	%ecx, (%rax)
.Ltmp474:
.LBB18_25:                              # %if.end39
	.loc	1 1007 0                # bzlib.c:1007:0
	cmpq	$0, -56(%rbp)
	je	.LBB18_27
# BB#26:                                # %if.then41
	movq	-56(%rbp), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	movl	%ecx, 5096(%rax)
.Ltmp475:
.LBB18_27:                              # %if.end43
	.loc	1 1007 0                # bzlib.c:1007:0
	movq	$0, -16(%rbp)
	jmp	.LBB18_41
.Ltmp476:
.LBB18_40:                              # %if.end68
	.loc	1 1024 0                # bzlib.c:1024:0
	movq	-56(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5024(%rax)
	.loc	1 1025 0                # bzlib.c:1025:0
	movq	-56(%rbp), %rax
	movq	$1, %rcx
	movb	%cl, 5100(%rax)
	.loc	1 1026 0                # bzlib.c:1026:0
	movq	-56(%rbp), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB18_41
.Ltmp477:
.Ltmp478:
	.size	BZ2_bzWriteOpen, .Ltmp478-BZ2_bzWriteOpen
.Lfunc_end18:
	.file	4 "../musl-1.0.4/include/bits/alltypes.h"
	.cfi_endproc

	.globl	BZ2_bzWrite
	.align	16, 0x90
	.type	BZ2_bzWrite,@function
BZ2_bzWrite:                            # @BZ2_bzWrite
.Lfunc_begin19:
	.loc	1 1037 0                # bzlib.c:1037:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp482:
	.cfi_def_cfa_offset 16
.Ltmp483:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp484:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$48, %rsp
.Ltmp485:
	.cfi_offset %rbx, -32
.Ltmp486:
	.cfi_offset %r14, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	.loc	1 1039 0 prologue_end   # bzlib.c:1039:0
.Ltmp487:
	movq	-32(%rbp), %rax
	.loc	1 1041 0                # bzlib.c:1041:0
.Ltmp488:
	cmpq	$0, -24(%rbp)
	movq	%rdx, -40(%rbp)
	movl	%ecx, -44(%rbp)
.Ltmp489:
	.loc	1 1039 0                # bzlib.c:1039:0
	movq	%rax, -64(%rbp)
	je	.LBB19_2
# BB#1:                                 # %if.then
	.loc	1 1041 0                # bzlib.c:1041:0
.Ltmp490:
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp491:
.LBB19_2:                               # %if.end
	.loc	1 1041 0                # bzlib.c:1041:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_4
# BB#3:                                 # %if.then2
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp492:
.LBB19_4:                               # %if.end3
	.loc	1 1042 0                # bzlib.c:1042:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_7
# BB#5:                                 # %lor.lhs.false
	cmpq	$0, -40(%rbp)
	je	.LBB19_7
# BB#6:                                 # %lor.lhs.false6
	movslq	-44(%rbp), %rax
	testq	%rax, %rax
	js	.LBB19_7
.Ltmp493:
# BB#11:                                # %if.end16
	.loc	1 1044 0                # bzlib.c:1044:0
	movq	-64(%rbp), %rax
	movzbl	5012(%rax), %eax
	testq	%rax, %rax
	je	.LBB19_12
.Ltmp494:
# BB#16:                                # %if.end25
	.loc	1 1046 0                # bzlib.c:1046:0
	movq	-64(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB19_17
.Ltmp495:
# BB#21:                                # %if.end35
	.loc	1 1049 0                # bzlib.c:1049:0
	movl	-44(%rbp), %eax
	testq	%rax, %rax
	je	.LBB19_32
.Ltmp496:
# BB#22:                                # %if.end45
	.loc	1 1052 0                # bzlib.c:1052:0
	movl	-44(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5024(%rcx)
	.loc	1 1053 0                # bzlib.c:1053:0
	movq	-40(%rbp), %rax
	movq	-64(%rbp), %rcx
	movq	%rax, 5016(%rcx)
	movq	$5000, %r14             # imm = 0x1388
	.align	16, 0x90
.LBB19_23:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1056 0                # bzlib.c:1056:0
.Ltmp497:
	movq	-64(%rbp), %rax
	movl	%r14d, 5048(%rax)
	.loc	1 1057 0                # bzlib.c:1057:0
	movq	-64(%rbp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 5040(%rax)
	.loc	1 1058 0                # bzlib.c:1058:0
	movq	-64(%rbp), %rdi
	addq	$5016, %rdi             # imm = 0x1398
	xorq	%rsi, %rsi
	callq	BZ2_bzCompress
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$1, %rcx
	movl	%eax, -56(%rbp)
	jne	.LBB19_24
# BB#28:                                # %if.end61
                                        #   in Loop: Header=BB19_23 Depth=1
	.loc	1 1062 0                # bzlib.c:1062:0
.Ltmp498:
	movq	-64(%rbp), %rax
	movl	5048(%rax), %eax
	cmpq	$4999, %rax             # imm = 0x1387
	ja	.LBB19_31
# BB#29:                                # %if.then65
                                        #   in Loop: Header=BB19_23 Depth=1
	.loc	1 1063 0                # bzlib.c:1063:0
.Ltmp499:
	movq	-64(%rbp), %rax
	movl	5048(%rax), %eax
	movq	$5000, %rcx             # imm = 0x1388
	.loc	1 1064 0                # bzlib.c:1064:0
	movq	-64(%rbp), %rdi
	.loc	1 1063 0                # bzlib.c:1063:0
	subq	%rax, %rcx
	movl	%ecx, -48(%rbp)
	.loc	1 1064 0                # bzlib.c:1064:0
	movslq	-48(%rbp), %rdx
	movq	(%rdi), %rcx
	addq	$8, %rdi
	movq	$1, %rsi
	callq	fwrite
	.loc	1 1066 0                # bzlib.c:1066:0
.Ltmp500:
	movl	-48(%rbp), %ecx
.Ltmp501:
	.loc	1 1064 0                # bzlib.c:1064:0
	movl	%eax, -52(%rbp)
	andq	%rbx, %rax
	.loc	1 1066 0                # bzlib.c:1066:0
.Ltmp502:
	cmpq	%rax, %rcx
	jne	.LBB19_17
# BB#30:                                # %lor.lhs.false75
                                        #   in Loop: Header=BB19_23 Depth=1
	movq	-64(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB19_17
.Ltmp503:
.LBB19_31:                              # %if.end90
                                        #   in Loop: Header=BB19_23 Depth=1
	.loc	1 1070 0                # bzlib.c:1070:0
	movq	-64(%rbp), %rax
	movl	5024(%rax), %eax
	testq	%rax, %rax
	jne	.LBB19_23
.LBB19_32:                              # %if.then95
	.loc	1 1071 0                # bzlib.c:1071:0
.Ltmp504:
	cmpq	$0, -24(%rbp)
	je	.LBB19_34
# BB#33:                                # %if.then98
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp505:
.LBB19_34:                              # %if.end99
	.loc	1 1071 0                # bzlib.c:1071:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_37
# BB#35:                                # %if.then102
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	jmp	.LBB19_36
.Ltmp506:
.LBB19_7:                               # %if.then8
	.loc	1 1043 0                # bzlib.c:1043:0
	cmpq	$0, -24(%rbp)
	je	.LBB19_9
# BB#8:                                 # %if.then10
	movq	-24(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, (%rax)
.Ltmp507:
.LBB19_9:                               # %if.end11
	.loc	1 1043 0                # bzlib.c:1043:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_37
# BB#10:                                # %if.then13
	movq	-64(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
.Ltmp508:
.LBB19_36:                              # %return
	.loc	1 1071 0                # bzlib.c:1071:0
	movl	%ecx, 5096(%rax)
.Ltmp509:
.LBB19_37:                              # %return
	.loc	1 1073 0                # bzlib.c:1073:0
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.LBB19_17:                              # %if.then27
	.loc	1 1047 0                # bzlib.c:1047:0
.Ltmp510:
	cmpq	$0, -24(%rbp)
	je	.LBB19_19
# BB#18:                                # %if.then29
	movq	-24(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, (%rax)
.Ltmp511:
.LBB19_19:                              # %if.end30
	.loc	1 1047 0                # bzlib.c:1047:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_37
# BB#20:                                # %if.then32
	movq	-64(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	jmp	.LBB19_36
.Ltmp512:
.LBB19_12:                              # %if.then17
	.loc	1 1045 0                # bzlib.c:1045:0
	cmpq	$0, -24(%rbp)
	je	.LBB19_14
# BB#13:                                # %if.then19
	movq	-24(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax)
.Ltmp513:
.LBB19_14:                              # %if.end20
	.loc	1 1045 0                # bzlib.c:1045:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_37
# BB#15:                                # %if.then22
	movq	-64(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB19_36
.Ltmp514:
.LBB19_24:                              # %if.then53
	.loc	1 1060 0                # bzlib.c:1060:0
	cmpq	$0, -24(%rbp)
	je	.LBB19_26
# BB#25:                                # %if.then55
	movl	-56(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp515:
.LBB19_26:                              # %if.end56
	.loc	1 1060 0                # bzlib.c:1060:0
	cmpq	$0, -64(%rbp)
	je	.LBB19_37
# BB#27:                                # %if.then58
	movl	-56(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5096(%rcx)
	jmp	.LBB19_37
.Ltmp516:
.Ltmp517:
	.size	BZ2_bzWrite, .Ltmp517-BZ2_bzWrite
.Lfunc_end19:
	.cfi_endproc

	.globl	BZ2_bzWriteClose
	.align	16, 0x90
	.type	BZ2_bzWriteClose,@function
BZ2_bzWriteClose:                       # @BZ2_bzWriteClose
.Lfunc_begin20:
	.loc	1 1083 0                # bzlib.c:1083:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp520:
	.cfi_def_cfa_offset 16
.Ltmp521:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp522:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	movl	%edx, -20(%rbp)
	movq	%rcx, -32(%rbp)
	.loc	1 1084 0 prologue_end   # bzlib.c:1084:0
.Ltmp523:
	movq	-8(%rbp), %rdi
	movq	-16(%rbp), %rsi
	movq	-32(%rbp), %rcx
	movl	-20(%rbp), %edx
	movq	%r8, %rax
	movq	%rax, -40(%rbp)
	xorq	%r8, %r8
	movq	$0, (%rsp)
	movq	%rax, %r9
	callq	BZ2_bzWriteClose64
	.loc	1 1086 0                # bzlib.c:1086:0
	addq	$48, %rsp
	popq	%rbp
	retq
.Ltmp524:
.Ltmp525:
	.size	BZ2_bzWriteClose, .Ltmp525-BZ2_bzWriteClose
.Lfunc_end20:
	.cfi_endproc

	.globl	BZ2_bzWriteClose64
	.align	16, 0x90
	.type	BZ2_bzWriteClose64,@function
BZ2_bzWriteClose64:                     # @BZ2_bzWriteClose64
.Lfunc_begin21:
	.loc	1 1097 0                # bzlib.c:1097:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp529:
	.cfi_def_cfa_offset 16
.Ltmp530:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp531:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$80, %rsp
.Ltmp532:
	.cfi_offset %rbx, -32
.Ltmp533:
	.cfi_offset %r14, -24
	movq	16(%rbp), %rax
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movq	%rcx, -48(%rbp)
	movq	%r8, -56(%rbp)
	.loc	1 1099 0 prologue_end   # bzlib.c:1099:0
.Ltmp534:
	movq	-32(%rbp), %rcx
	movq	%r9, -64(%rbp)
	movq	%rax, -72(%rbp)
	testq	%rcx, %rcx
	movq	%rcx, -96(%rbp)
	je	.LBB21_1
# BB#5:                                 # %if.end6
	.loc	1 1103 0                # bzlib.c:1103:0
.Ltmp535:
	movq	-96(%rbp), %rax
	movzbl	5012(%rax), %eax
	testq	%rax, %rax
	je	.LBB21_6
.Ltmp536:
# BB#10:                                # %if.end15
	.loc	1 1105 0                # bzlib.c:1105:0
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	jne	.LBB21_11
.Ltmp537:
# BB#15:                                # %if.end25
	.loc	1 1108 0                # bzlib.c:1108:0
	cmpq	$0, -48(%rbp)
	je	.LBB21_17
# BB#16:                                # %if.then27
	movq	-48(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp538:
.LBB21_17:                              # %if.end28
	.loc	1 1109 0                # bzlib.c:1109:0
	cmpq	$0, -56(%rbp)
	je	.LBB21_19
# BB#18:                                # %if.then30
	movq	-56(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp539:
.LBB21_19:                              # %if.end31
	.loc	1 1110 0                # bzlib.c:1110:0
	cmpq	$0, -64(%rbp)
	je	.LBB21_21
# BB#20:                                # %if.then33
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp540:
.LBB21_21:                              # %if.end34
	.loc	1 1111 0                # bzlib.c:1111:0
	cmpq	$0, -72(%rbp)
	je	.LBB21_23
# BB#22:                                # %if.then36
	movq	-72(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp541:
.LBB21_23:                              # %if.end37
	.loc	1 1113 0                # bzlib.c:1113:0
	movl	-36(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB21_36
# BB#24:                                # %land.lhs.true
	movq	-96(%rbp), %rax
	movl	5096(%rax), %eax
	testq	%rax, %rax
	jne	.LBB21_36
# BB#25:
	movq	$5000, %r14             # imm = 0x1388
	.align	16, 0x90
.LBB21_26:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1115 0                # bzlib.c:1115:0
.Ltmp542:
	movq	-96(%rbp), %rax
	movl	%r14d, 5048(%rax)
	.loc	1 1116 0                # bzlib.c:1116:0
	movq	-96(%rbp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 5040(%rax)
	.loc	1 1117 0                # bzlib.c:1117:0
	movq	-96(%rbp), %rdi
	addq	$5016, %rdi             # imm = 0x1398
	movq	$2, %rsi
	callq	BZ2_bzCompress
	movq	%rax, %rcx
	andq	%rbx, %rcx
	cmpq	$3, %rcx
	movl	%eax, -84(%rbp)
	je	.LBB21_32
# BB#27:                                # %land.lhs.true46
                                        #   in Loop: Header=BB21_26 Depth=1
	.loc	1 1118 0                # bzlib.c:1118:0
.Ltmp543:
	movl	-84(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB21_28
.Ltmp544:
.LBB21_32:                              # %if.end56
                                        #   in Loop: Header=BB21_26 Depth=1
	.loc	1 1121 0                # bzlib.c:1121:0
	movq	-96(%rbp), %rax
	movl	5048(%rax), %eax
	cmpq	$4999, %rax             # imm = 0x1387
	ja	.LBB21_35
# BB#33:                                # %if.then60
                                        #   in Loop: Header=BB21_26 Depth=1
	.loc	1 1122 0                # bzlib.c:1122:0
.Ltmp545:
	movq	-96(%rbp), %rax
	movl	5048(%rax), %eax
	movq	$5000, %rcx             # imm = 0x1388
	.loc	1 1123 0                # bzlib.c:1123:0
	movq	-96(%rbp), %rdi
	.loc	1 1122 0                # bzlib.c:1122:0
	subq	%rax, %rcx
	movl	%ecx, -76(%rbp)
	.loc	1 1123 0                # bzlib.c:1123:0
	movslq	-76(%rbp), %rdx
	movq	(%rdi), %rcx
	addq	$8, %rdi
	movq	$1, %rsi
	callq	fwrite
	.loc	1 1125 0                # bzlib.c:1125:0
.Ltmp546:
	movl	-76(%rbp), %ecx
.Ltmp547:
	.loc	1 1123 0                # bzlib.c:1123:0
	movl	%eax, -80(%rbp)
	andq	%rbx, %rax
	.loc	1 1125 0                # bzlib.c:1125:0
.Ltmp548:
	cmpq	%rax, %rcx
	jne	.LBB21_11
# BB#34:                                # %lor.lhs.false
                                        #   in Loop: Header=BB21_26 Depth=1
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB21_11
.Ltmp549:
.LBB21_35:                              # %if.end84
                                        #   in Loop: Header=BB21_26 Depth=1
	.loc	1 1129 0                # bzlib.c:1129:0
	movl	-84(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB21_26
.Ltmp550:
.LBB21_36:                              # %if.end89
	.loc	1 1133 0                # bzlib.c:1133:0
	movl	-36(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB21_39
# BB#37:                                # %land.lhs.true91
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB21_39
# BB#38:                                # %if.then95
	.loc	1 1134 0                # bzlib.c:1134:0
.Ltmp551:
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	callq	fflush
	.loc	1 1135 0                # bzlib.c:1135:0
.Ltmp552:
	movq	-96(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	testq	%rbx, %rax
	je	.LBB21_39
.Ltmp553:
.LBB21_11:                              # %if.then17
	.loc	1 1106 0                # bzlib.c:1106:0
	cmpq	$0, -24(%rbp)
	je	.LBB21_13
# BB#12:                                # %if.then19
	movq	-24(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, (%rax)
.Ltmp554:
.LBB21_13:                              # %if.end20
	.loc	1 1106 0                # bzlib.c:1106:0
	cmpq	$0, -96(%rbp)
	je	.LBB21_52
# BB#14:                                # %if.then22
	movq	-96(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, 5096(%rax)
	jmp	.LBB21_52
.Ltmp555:
.LBB21_1:                               # %if.then
	.loc	1 1102 0                # bzlib.c:1102:0
	cmpq	$0, -24(%rbp)
	je	.LBB21_3
# BB#2:                                 # %if.then2
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp556:
.LBB21_3:                               # %if.end
	.loc	1 1102 0                # bzlib.c:1102:0
	cmpq	$0, -96(%rbp)
	je	.LBB21_52
# BB#4:                                 # %if.then4
	movq	-96(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
	jmp	.LBB21_52
.Ltmp557:
.LBB21_6:                               # %if.then7
	.loc	1 1104 0                # bzlib.c:1104:0
	cmpq	$0, -24(%rbp)
	je	.LBB21_8
# BB#7:                                 # %if.then9
	movq	-24(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax)
.Ltmp558:
.LBB21_8:                               # %if.end10
	.loc	1 1104 0                # bzlib.c:1104:0
	cmpq	$0, -96(%rbp)
	je	.LBB21_52
# BB#9:                                 # %if.then12
	movq	-96(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 5096(%rax)
	jmp	.LBB21_52
.Ltmp559:
.LBB21_39:                              # %if.end112
	.loc	1 1139 0                # bzlib.c:1139:0
	cmpq	$0, -48(%rbp)
	je	.LBB21_41
# BB#40:                                # %if.then115
	.loc	1 1140 0                # bzlib.c:1140:0
	movq	-96(%rbp), %rax
	movl	5028(%rax), %eax
	movq	-48(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp560:
.LBB21_41:                              # %if.end117
	.loc	1 1141 0                # bzlib.c:1141:0
	cmpq	$0, -56(%rbp)
	je	.LBB21_43
# BB#42:                                # %if.then120
	.loc	1 1142 0                # bzlib.c:1142:0
	movq	-96(%rbp), %rax
	movl	5032(%rax), %eax
	movq	-56(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp561:
.LBB21_43:                              # %if.end122
	.loc	1 1143 0                # bzlib.c:1143:0
	cmpq	$0, -64(%rbp)
	je	.LBB21_45
# BB#44:                                # %if.then125
	.loc	1 1144 0                # bzlib.c:1144:0
	movq	-96(%rbp), %rax
	movl	5052(%rax), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp562:
.LBB21_45:                              # %if.end127
	.loc	1 1145 0                # bzlib.c:1145:0
	cmpq	$0, -72(%rbp)
	je	.LBB21_47
# BB#46:                                # %if.then130
	.loc	1 1146 0                # bzlib.c:1146:0
	movq	-96(%rbp), %rax
	movl	5056(%rax), %eax
	movq	-72(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp563:
.LBB21_47:                              # %if.end132
	.loc	1 1148 0                # bzlib.c:1148:0
	cmpq	$0, -24(%rbp)
	je	.LBB21_49
# BB#48:                                # %if.then135
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp564:
.LBB21_49:                              # %if.end136
	.loc	1 1148 0                # bzlib.c:1148:0
	cmpq	$0, -96(%rbp)
	je	.LBB21_51
# BB#50:                                # %if.then139
	movq	-96(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp565:
.LBB21_51:                              # %if.end141
	.loc	1 1149 0                # bzlib.c:1149:0
	movq	-96(%rbp), %rdi
	addq	$5016, %rdi             # imm = 0x1398
	callq	BZ2_bzCompressEnd
	.loc	1 1150 0                # bzlib.c:1150:0
	movq	-96(%rbp), %rdi
	callq	free
.LBB21_52:                              # %return
	.loc	1 1151 0                # bzlib.c:1151:0
	addq	$80, %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.LBB21_28:                              # %if.then48
	.loc	1 1119 0                # bzlib.c:1119:0
.Ltmp566:
	cmpq	$0, -24(%rbp)
	je	.LBB21_30
# BB#29:                                # %if.then50
	movl	-84(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp567:
.LBB21_30:                              # %if.end51
	.loc	1 1119 0                # bzlib.c:1119:0
	cmpq	$0, -96(%rbp)
	je	.LBB21_52
# BB#31:                                # %if.then53
	movl	-84(%rbp), %eax
	movq	-96(%rbp), %rcx
	movl	%eax, 5096(%rcx)
	jmp	.LBB21_52
.Ltmp568:
.Ltmp569:
	.size	BZ2_bzWriteClose64, .Ltmp569-BZ2_bzWriteClose64
.Lfunc_end21:
	.cfi_endproc

	.globl	BZ2_bzReadOpen
	.align	16, 0x90
	.type	BZ2_bzReadOpen,@function
BZ2_bzReadOpen:                         # @BZ2_bzReadOpen
.Lfunc_begin22:
	.loc	1 1166 0                # bzlib.c:1166:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp573:
	.cfi_def_cfa_offset 16
.Ltmp574:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp575:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$72, %rsp
.Ltmp576:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movl	%edx, -36(%rbp)
	movl	%ecx, -40(%rbp)
	.loc	1 1170 0 prologue_end   # bzlib.c:1170:0
.Ltmp577:
	cmpq	$0, -24(%rbp)
	movq	%r8, -48(%rbp)
	movl	%r9d, -52(%rbp)
.Ltmp578:
	.loc	1 1167 0                # bzlib.c:1167:0
	movq	$0, -64(%rbp)
	je	.LBB22_2
# BB#1:                                 # %if.then
	.loc	1 1170 0                # bzlib.c:1170:0
.Ltmp579:
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp580:
.LBB22_2:                               # %if.end
	.loc	1 1170 0                # bzlib.c:1170:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_4
# BB#3:                                 # %if.then2
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp581:
.LBB22_4:                               # %if.end3
	.loc	1 1172 0                # bzlib.c:1172:0
	cmpq	$0, -32(%rbp)
	je	.LBB22_14
# BB#5:                                 # %lor.lhs.false
	movl	-40(%rbp), %eax
	testq	%rax, %rax
	je	.LBB22_7
# BB#6:                                 # %land.lhs.true
	movl	-40(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB22_14
.LBB22_7:                               # %lor.lhs.false7
	movslq	-36(%rbp), %rax
	testq	%rax, %rax
	js	.LBB22_14
# BB#8:                                 # %lor.lhs.false9
	movslq	-36(%rbp), %rax
	cmpq	$4, %rax
	jg	.LBB22_14
# BB#9:                                 # %lor.lhs.false11
	cmpq	$0, -48(%rbp)
	jne	.LBB22_11
# BB#10:                                # %land.lhs.true13
	movl	-52(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB22_14
.LBB22_11:                              # %lor.lhs.false15
	cmpq	$0, -48(%rbp)
	je	.LBB22_19
# BB#12:                                # %land.lhs.true17
	movslq	-52(%rbp), %rax
	testq	%rax, %rax
	js	.LBB22_14
# BB#13:                                # %lor.lhs.false19
	movslq	-52(%rbp), %rax
	cmpq	$5001, %rax             # imm = 0x1389
	jl	.LBB22_19
.LBB22_14:                              # %if.then21
	.loc	1 1177 0                # bzlib.c:1177:0
.Ltmp582:
	cmpq	$0, -24(%rbp)
	je	.LBB22_16
# BB#15:                                # %if.then23
	movq	-24(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, (%rax)
.Ltmp583:
.LBB22_16:                              # %if.end24
	.loc	1 1177 0                # bzlib.c:1177:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_18
# BB#17:                                # %if.then26
	movq	-64(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, 5096(%rax)
.Ltmp584:
.LBB22_18:                              # %if.end28
	.loc	1 1177 0                # bzlib.c:1177:0
	movq	$0, -16(%rbp)
.Ltmp585:
.LBB22_45:                              # %return
	.loc	1 1211 0                # bzlib.c:1211:0
	movq	-16(%rbp), %rax
	addq	$72, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB22_19:                              # %if.end29
	.loc	1 1179 0                # bzlib.c:1179:0
.Ltmp586:
	movq	-32(%rbp), %rdi
	callq	ferror
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	je	.LBB22_25
# BB#20:                                # %if.then30
	.loc	1 1180 0                # bzlib.c:1180:0
.Ltmp587:
	cmpq	$0, -24(%rbp)
	je	.LBB22_22
# BB#21:                                # %if.then32
	movq	-24(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, (%rax)
.Ltmp588:
.LBB22_22:                              # %if.end33
	.loc	1 1180 0                # bzlib.c:1180:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_24
# BB#23:                                # %if.then35
	movq	-64(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, 5096(%rax)
.Ltmp589:
.LBB22_24:                              # %if.end37
	.loc	1 1180 0                # bzlib.c:1180:0
	movq	$0, -16(%rbp)
	jmp	.LBB22_45
.Ltmp590:
.LBB22_25:                              # %if.end38
	movq	$5104, %rdi             # imm = 0x13F0
	.loc	1 1182 0                # bzlib.c:1182:0
	callq	malloc
	testq	%rax, %rax
	movq	%rax, -64(%rbp)
	je	.LBB22_26
# BB#31:                                # %if.end49
	.loc	1 1186 0                # bzlib.c:1186:0
.Ltmp591:
	cmpq	$0, -24(%rbp)
	je	.LBB22_33
# BB#32:                                # %if.then51
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp592:
.LBB22_33:                              # %if.end52
	.loc	1 1186 0                # bzlib.c:1186:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_35
# BB#34:                                # %if.then54
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp593:
.LBB22_35:                              # %if.end56
	.loc	1 1188 0                # bzlib.c:1188:0
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movb	%cl, 5100(%rax)
	.loc	1 1189 0                # bzlib.c:1189:0
	movq	-32(%rbp), %rax
	movq	-64(%rbp), %rdx
	movq	%rax, (%rdx)
	.loc	1 1190 0                # bzlib.c:1190:0
	movq	-64(%rbp), %rax
	movl	%ecx, 5008(%rax)
	.loc	1 1191 0                # bzlib.c:1191:0
	movq	-64(%rbp), %rax
	movb	%cl, 5012(%rax)
	.loc	1 1192 0                # bzlib.c:1192:0
	movq	-64(%rbp), %rax
	movq	$0, 5072(%rax)
	.loc	1 1193 0                # bzlib.c:1193:0
	movq	-64(%rbp), %rax
	movq	$0, 5080(%rax)
	.loc	1 1194 0                # bzlib.c:1194:0
	movq	-64(%rbp), %rax
	movq	$0, 5088(%rax)
	jmp	.LBB22_36
	.align	16, 0x90
.LBB22_37:                              # %while.body
                                        #   in Loop: Header=BB22_36 Depth=1
	.loc	1 1197 0                # bzlib.c:1197:0
.Ltmp594:
	movq	-48(%rbp), %rax
	movq	-64(%rbp), %rcx
	movzbl	(%rax), %eax
	movslq	5008(%rcx), %rdx
	movb	%al, 8(%rcx,%rdx)
	movq	-64(%rbp), %rax
	movl	5008(%rax), %ecx
	incq	%rcx
	movl	%ecx, 5008(%rax)
	.loc	1 1198 0                # bzlib.c:1198:0
	incq	-48(%rbp)
	.loc	1 1199 0                # bzlib.c:1199:0
	movl	-52(%rbp), %eax
	decq	%rax
	movl	%eax, -52(%rbp)
.Ltmp595:
.LBB22_36:                              # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1196 0                # bzlib.c:1196:0
	movslq	-52(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB22_37
# BB#38:                                # %while.end
	.loc	1 1202 0                # bzlib.c:1202:0
	movq	-64(%rbp), %rdi
	movl	-40(%rbp), %edx
	movl	-36(%rbp), %esi
	addq	$5016, %rdi             # imm = 0x1398
	callq	BZ2_bzDecompressInit
	testq	%rbx, %rax
	movl	%eax, -68(%rbp)
	.loc	1 1203 0                # bzlib.c:1203:0
.Ltmp596:
	je	.LBB22_44
# BB#39:                                # %if.then65
	.loc	1 1204 0                # bzlib.c:1204:0
.Ltmp597:
	cmpq	$0, -24(%rbp)
	je	.LBB22_41
# BB#40:                                # %if.then67
	movl	-68(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp598:
.LBB22_41:                              # %if.end68
	.loc	1 1204 0                # bzlib.c:1204:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_43
# BB#42:                                # %if.then70
	movl	-68(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5096(%rcx)
.Ltmp599:
.LBB22_43:                              # %if.end72
	.loc	1 1204 0                # bzlib.c:1204:0
	movq	-64(%rbp), %rdi
	callq	free
	movq	$0, -16(%rbp)
	jmp	.LBB22_45
.Ltmp600:
.LBB22_26:                              # %if.then41
	.loc	1 1184 0                # bzlib.c:1184:0
	cmpq	$0, -24(%rbp)
	je	.LBB22_28
# BB#27:                                # %if.then43
	movq	-24(%rbp), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	movl	%ecx, (%rax)
.Ltmp601:
.LBB22_28:                              # %if.end44
	.loc	1 1184 0                # bzlib.c:1184:0
	cmpq	$0, -64(%rbp)
	je	.LBB22_30
# BB#29:                                # %if.then46
	movq	-64(%rbp), %rax
	movabsq	$4294967293, %rcx       # imm = 0xFFFFFFFD
	movl	%ecx, 5096(%rax)
.Ltmp602:
.LBB22_30:                              # %if.end48
	.loc	1 1184 0                # bzlib.c:1184:0
	movq	$0, -16(%rbp)
	jmp	.LBB22_45
.Ltmp603:
.LBB22_44:                              # %if.end73
	.loc	1 1206 0                # bzlib.c:1206:0
	movq	-64(%rbp), %rax
	movl	5008(%rax), %ecx
	movl	%ecx, 5024(%rax)
	.loc	1 1207 0                # bzlib.c:1207:0
	movq	-64(%rbp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 5016(%rax)
	.loc	1 1209 0                # bzlib.c:1209:0
	movq	-64(%rbp), %rax
	movq	$1, %rcx
	movb	%cl, 5100(%rax)
	.loc	1 1210 0                # bzlib.c:1210:0
	movq	-64(%rbp), %rax
	movq	%rax, -16(%rbp)
	jmp	.LBB22_45
.Ltmp604:
.Ltmp605:
	.size	BZ2_bzReadOpen, .Ltmp605-BZ2_bzReadOpen
.Lfunc_end22:
	.cfi_endproc

	.globl	BZ2_bzReadClose
	.align	16, 0x90
	.type	BZ2_bzReadClose,@function
BZ2_bzReadClose:                        # @BZ2_bzReadClose
.Lfunc_begin23:
	.loc	1 1216 0                # bzlib.c:1216:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp608:
	.cfi_def_cfa_offset 16
.Ltmp609:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp610:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 1219 0 prologue_end   # bzlib.c:1219:0
.Ltmp611:
	cmpq	$0, -8(%rbp)
	movq	%rsi, -16(%rbp)
.Ltmp612:
	.loc	1 1217 0                # bzlib.c:1217:0
	movq	%rsi, -24(%rbp)
	je	.LBB23_2
# BB#1:                                 # %if.then
	.loc	1 1219 0                # bzlib.c:1219:0
.Ltmp613:
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp614:
.LBB23_2:                               # %if.end
	.loc	1 1219 0                # bzlib.c:1219:0
	cmpq	$0, -24(%rbp)
	je	.LBB23_4
# BB#3:                                 # %if.then2
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp615:
.LBB23_4:                               # %if.end3
	.loc	1 1220 0                # bzlib.c:1220:0
	cmpq	$0, -24(%rbp)
	je	.LBB23_5
.Ltmp616:
# BB#9:                                 # %if.end13
	.loc	1 1223 0                # bzlib.c:1223:0
	movq	-24(%rbp), %rax
	movzbl	5012(%rax), %eax
	testq	%rax, %rax
	je	.LBB23_14
# BB#10:                                # %if.then14
	.loc	1 1224 0                # bzlib.c:1224:0
.Ltmp617:
	cmpq	$0, -8(%rbp)
	je	.LBB23_12
# BB#11:                                # %if.then16
	movq	-8(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax)
.Ltmp618:
.LBB23_12:                              # %if.end17
	.loc	1 1224 0                # bzlib.c:1224:0
	cmpq	$0, -24(%rbp)
	je	.LBB23_17
# BB#13:                                # %if.then19
	movq	-24(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 5096(%rax)
	jmp	.LBB23_17
.Ltmp619:
.LBB23_5:                               # %if.then5
	.loc	1 1221 0                # bzlib.c:1221:0
	cmpq	$0, -8(%rbp)
	je	.LBB23_7
# BB#6:                                 # %if.then7
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp620:
.LBB23_7:                               # %if.end8
	.loc	1 1221 0                # bzlib.c:1221:0
	cmpq	$0, -24(%rbp)
	je	.LBB23_17
# BB#8:                                 # %if.then10
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
	jmp	.LBB23_17
.Ltmp621:
.LBB23_14:                              # %if.end22
	.loc	1 1226 0                # bzlib.c:1226:0
	movq	-24(%rbp), %rax
	movzbl	5100(%rax), %eax
	testq	%rax, %rax
	je	.LBB23_16
# BB#15:                                # %if.then24
	.loc	1 1227 0                # bzlib.c:1227:0
	movq	-24(%rbp), %rdi
	addq	$5016, %rdi             # imm = 0x1398
	callq	BZ2_bzDecompressEnd
.Ltmp622:
.LBB23_16:                              # %if.end25
	.loc	1 1228 0                # bzlib.c:1228:0
	movq	-24(%rbp), %rdi
	callq	free
.LBB23_17:                              # %return
	.loc	1 1229 0                # bzlib.c:1229:0
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp623:
.Ltmp624:
	.size	BZ2_bzReadClose, .Ltmp624-BZ2_bzReadClose
.Lfunc_end23:
	.cfi_endproc

	.globl	BZ2_bzRead
	.align	16, 0x90
	.type	BZ2_bzRead,@function
BZ2_bzRead:                             # @BZ2_bzRead
.Lfunc_begin24:
	.loc	1 1238 0                # bzlib.c:1238:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp628:
	.cfi_def_cfa_offset 16
.Ltmp629:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp630:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$56, %rsp
.Ltmp631:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	.loc	1 1240 0 prologue_end   # bzlib.c:1240:0
.Ltmp632:
	movq	-32(%rbp), %rax
	.loc	1 1242 0                # bzlib.c:1242:0
.Ltmp633:
	cmpq	$0, -24(%rbp)
	movq	%rdx, -40(%rbp)
	movl	%ecx, -44(%rbp)
.Ltmp634:
	.loc	1 1240 0                # bzlib.c:1240:0
	movq	%rax, -64(%rbp)
	je	.LBB24_2
# BB#1:                                 # %if.then
	.loc	1 1242 0                # bzlib.c:1242:0
.Ltmp635:
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp636:
.LBB24_2:                               # %if.end
	.loc	1 1242 0                # bzlib.c:1242:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_4
# BB#3:                                 # %if.then2
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp637:
.LBB24_4:                               # %if.end3
	.loc	1 1244 0                # bzlib.c:1244:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_7
# BB#5:                                 # %lor.lhs.false
	cmpq	$0, -40(%rbp)
	je	.LBB24_7
# BB#6:                                 # %lor.lhs.false6
	movslq	-44(%rbp), %rax
	testq	%rax, %rax
	js	.LBB24_7
.Ltmp638:
# BB#13:                                # %if.end16
	.loc	1 1247 0                # bzlib.c:1247:0
	movq	-64(%rbp), %rax
	movzbl	5012(%rax), %eax
	testq	%rax, %rax
	je	.LBB24_18
# BB#14:                                # %if.then17
	.loc	1 1248 0                # bzlib.c:1248:0
.Ltmp639:
	cmpq	$0, -24(%rbp)
	je	.LBB24_16
# BB#15:                                # %if.then19
	movq	-24(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax)
.Ltmp640:
.LBB24_16:                              # %if.end20
	.loc	1 1248 0                # bzlib.c:1248:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#17:                                # %if.then22
	movq	-64(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	jmp	.LBB24_11
.Ltmp641:
.LBB24_7:                               # %if.then8
	.loc	1 1245 0                # bzlib.c:1245:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_9
# BB#8:                                 # %if.then10
	movq	-24(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, (%rax)
.Ltmp642:
.LBB24_9:                               # %if.end11
	.loc	1 1245 0                # bzlib.c:1245:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#10:                                # %if.then13
	movq	-64(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
.LBB24_11:                              # %if.end15
	movl	%ecx, 5096(%rax)
.Ltmp643:
.LBB24_12:                              # %if.end15
	xorq	%rax, %rax
.LBB24_59:                              # %return
	.loc	1 1284 0                # bzlib.c:1284:0
.Ltmp644:
	movl	%eax, -12(%rbp)
.Ltmp645:
.LBB24_60:                              # %return
	.loc	1 1289 0                # bzlib.c:1289:0
	movl	-12(%rbp), %eax
	addq	$56, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB24_18:                              # %if.end25
	.loc	1 1250 0                # bzlib.c:1250:0
.Ltmp646:
	movl	-44(%rbp), %eax
	testq	%rax, %rax
	je	.LBB24_19
.Ltmp647:
# BB#23:                                # %if.end35
	.loc	1 1253 0                # bzlib.c:1253:0
	movl	-44(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5048(%rcx)
	.loc	1 1254 0                # bzlib.c:1254:0
	movq	-40(%rbp), %rax
	movq	-64(%rbp), %rcx
	movq	%rax, 5040(%rcx)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	.align	16, 0x90
.LBB24_24:                              # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1258 0                # bzlib.c:1258:0
.Ltmp648:
	movq	-64(%rbp), %rax
	movq	(%rax), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB24_25
.Ltmp649:
# BB#29:                                # %if.end46
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1261 0                # bzlib.c:1261:0
	movq	-64(%rbp), %rax
	movl	5024(%rax), %eax
	testq	%rax, %rax
	jne	.LBB24_33
# BB#30:                                # %land.lhs.true
                                        #   in Loop: Header=BB24_24 Depth=1
	movq	-64(%rbp), %rax
	movq	(%rax), %rdi
	callq	myfeof
	testq	%rax, %rax
	jne	.LBB24_33
# BB#31:                                # %if.then52
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1262 0                # bzlib.c:1262:0
.Ltmp650:
	movq	-64(%rbp), %rdi
	movq	(%rdi), %rcx
	addq	$8, %rdi
	movq	$1, %rsi
	movq	$5000, %rdx             # imm = 0x1388
	callq	fread
	.loc	1 1264 0                # bzlib.c:1264:0
.Ltmp651:
	movq	-64(%rbp), %rcx
.Ltmp652:
	.loc	1 1262 0                # bzlib.c:1262:0
	movl	%eax, -48(%rbp)
	.loc	1 1264 0                # bzlib.c:1264:0
.Ltmp653:
	movq	(%rcx), %rdi
	callq	ferror
	testq	%rbx, %rax
	jne	.LBB24_25
.Ltmp654:
# BB#32:                                # %if.end69
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1266 0                # bzlib.c:1266:0
	movl	-48(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5008(%rcx)
	.loc	1 1267 0                # bzlib.c:1267:0
	movq	-64(%rbp), %rax
	movl	5008(%rax), %ecx
	movl	%ecx, 5024(%rax)
	.loc	1 1268 0                # bzlib.c:1268:0
	movq	-64(%rbp), %rax
	leaq	8(%rax), %rcx
	movq	%rcx, 5016(%rax)
.Ltmp655:
.LBB24_33:                              # %if.end76
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1271 0                # bzlib.c:1271:0
	movq	-64(%rbp), %rdi
	addq	$5016, %rdi             # imm = 0x1398
	callq	BZ2_bzDecompress
	testq	%rbx, %rax
	movl	%eax, -52(%rbp)
	je	.LBB24_39
# BB#34:                                # %land.lhs.true81
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1273 0                # bzlib.c:1273:0
.Ltmp656:
	movl	-52(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB24_35
.Ltmp657:
.LBB24_39:                              # %if.end94
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1276 0                # bzlib.c:1276:0
	movl	-52(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB24_47
# BB#40:                                # %land.lhs.true97
                                        #   in Loop: Header=BB24_24 Depth=1
	movq	-64(%rbp), %rax
	movq	(%rax), %rdi
	callq	myfeof
	testq	%rax, %rax
	je	.LBB24_47
# BB#41:                                # %land.lhs.true102
                                        #   in Loop: Header=BB24_24 Depth=1
	movq	-64(%rbp), %rax
	movl	5024(%rax), %eax
	testq	%rax, %rax
	jne	.LBB24_47
# BB#42:                                # %land.lhs.true107
                                        #   in Loop: Header=BB24_24 Depth=1
	movq	-64(%rbp), %rax
	movl	5048(%rax), %eax
	testq	%rax, %rax
	jne	.LBB24_43
.Ltmp658:
	.align	16, 0x90
.LBB24_47:                              # %if.end122
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1280 0                # bzlib.c:1280:0
	movl	-52(%rbp), %eax
	cmpq	$4, %rax
	je	.LBB24_48
.Ltmp659:
# BB#53:                                # %if.end137
                                        #   in Loop: Header=BB24_24 Depth=1
	.loc	1 1283 0                # bzlib.c:1283:0
	movq	-64(%rbp), %rax
	movl	5048(%rax), %eax
	testq	%rax, %rax
	jne	.LBB24_24
# BB#54:                                # %if.then142
	.loc	1 1284 0                # bzlib.c:1284:0
.Ltmp660:
	cmpq	$0, -24(%rbp)
	je	.LBB24_56
# BB#55:                                # %if.then145
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp661:
.LBB24_56:                              # %if.end146
	.loc	1 1284 0                # bzlib.c:1284:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_58
# BB#57:                                # %if.then149
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp662:
.LBB24_58:                              # %if.end151
	.loc	1 1284 0                # bzlib.c:1284:0
	movl	-44(%rbp), %eax
	jmp	.LBB24_59
.Ltmp663:
.LBB24_19:                              # %if.then27
	.loc	1 1251 0                # bzlib.c:1251:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_21
# BB#20:                                # %if.then29
	movq	-24(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp664:
.LBB24_21:                              # %if.end30
	.loc	1 1251 0                # bzlib.c:1251:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#22:                                # %if.then32
	movq	-64(%rbp), %rax
	xorq	%rcx, %rcx
	jmp	.LBB24_11
.Ltmp665:
.LBB24_25:                              # %if.then38
	.loc	1 1259 0                # bzlib.c:1259:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_27
# BB#26:                                # %if.then40
	movq	-24(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	movl	%ecx, (%rax)
.Ltmp666:
.LBB24_27:                              # %if.end41
	.loc	1 1259 0                # bzlib.c:1259:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#28:                                # %if.then43
	movq	-64(%rbp), %rax
	movabsq	$4294967290, %rcx       # imm = 0xFFFFFFFA
	jmp	.LBB24_11
.Ltmp667:
.LBB24_48:                              # %if.then125
	.loc	1 1281 0                # bzlib.c:1281:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_50
# BB#49:                                # %if.then128
	movq	-24(%rbp), %rax
	movq	$4, %rcx
	movl	%ecx, (%rax)
.Ltmp668:
.LBB24_50:                              # %if.end129
	.loc	1 1281 0                # bzlib.c:1281:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_52
# BB#51:                                # %if.then132
	movq	-64(%rbp), %rax
	movq	$4, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp669:
.LBB24_52:                              # %if.end134
	.loc	1 1282 0                # bzlib.c:1282:0
	movq	-64(%rbp), %rax
	movl	-44(%rbp), %ecx
	movl	5048(%rax), %eax
	subq	%rax, %rcx
	movl	%ecx, -12(%rbp)
	jmp	.LBB24_60
.Ltmp670:
.LBB24_35:                              # %if.then84
	.loc	1 1274 0                # bzlib.c:1274:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_37
# BB#36:                                # %if.then87
	movl	-52(%rbp), %eax
	movq	-24(%rbp), %rcx
	movl	%eax, (%rcx)
.Ltmp671:
.LBB24_37:                              # %if.end88
	.loc	1 1274 0                # bzlib.c:1274:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#38:                                # %if.then91
	movl	-52(%rbp), %eax
	movq	-64(%rbp), %rcx
	movl	%eax, 5096(%rcx)
	xorq	%rax, %rax
	jmp	.LBB24_59
.Ltmp672:
.LBB24_43:                              # %if.then112
	.loc	1 1278 0                # bzlib.c:1278:0
	cmpq	$0, -24(%rbp)
	je	.LBB24_45
# BB#44:                                # %if.then115
	movq	-24(%rbp), %rax
	movabsq	$4294967289, %rcx       # imm = 0xFFFFFFF9
	movl	%ecx, (%rax)
.Ltmp673:
.LBB24_45:                              # %if.end116
	.loc	1 1278 0                # bzlib.c:1278:0
	cmpq	$0, -64(%rbp)
	je	.LBB24_12
# BB#46:                                # %if.then119
	movq	-64(%rbp), %rax
	movabsq	$4294967289, %rcx       # imm = 0xFFFFFFF9
	jmp	.LBB24_11
.Ltmp674:
.Ltmp675:
	.size	BZ2_bzRead, .Ltmp675-BZ2_bzRead
.Lfunc_end24:
	.cfi_endproc

	.align	16, 0x90
	.type	myfeof,@function
myfeof:                                 # @myfeof
.Lfunc_begin25:
	.loc	1 971 0                 # bzlib.c:971:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp678:
	.cfi_def_cfa_offset 16
.Ltmp679:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp680:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 972 0 prologue_end    # bzlib.c:972:0
.Ltmp681:
	callq	fgetc
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movq	%rax, %rdx
	andq	%rcx, %rdx
	cmpq	%rcx, %rdx
	movl	%eax, -20(%rbp)
	je	.LBB25_1
# BB#2:                                 # %if.end
	.loc	1 974 0                 # bzlib.c:974:0
	movq	-16(%rbp), %rsi
	movl	-20(%rbp), %edi
	callq	ungetc
	xorq	%rax, %rax
	jmp	.LBB25_3
.LBB25_1:                               # %if.then
	movq	$1, %rax
.LBB25_3:                               # %return
	.loc	1 975 0                 # bzlib.c:975:0
	movb	%al, -1(%rbp)
	.loc	1 976 0                 # bzlib.c:976:0
	movzbl	-1(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp682:
.Ltmp683:
	.size	myfeof, .Ltmp683-myfeof
.Lfunc_end25:
	.cfi_endproc

	.globl	BZ2_bzReadGetUnused
	.align	16, 0x90
	.type	BZ2_bzReadGetUnused,@function
BZ2_bzReadGetUnused:                    # @BZ2_bzReadGetUnused
.Lfunc_begin26:
	.loc	1 1298 0                # bzlib.c:1298:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp686:
	.cfi_def_cfa_offset 16
.Ltmp687:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp688:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	.loc	1 1299 0 prologue_end   # bzlib.c:1299:0
.Ltmp689:
	movq	-16(%rbp), %rax
	movq	%rdx, -24(%rbp)
	movq	%rcx, -32(%rbp)
	testq	%rax, %rax
	movq	%rax, -40(%rbp)
	je	.LBB26_1
# BB#5:                                 # %if.end6
	.loc	1 1302 0                # bzlib.c:1302:0
.Ltmp690:
	movq	-40(%rbp), %rax
	movl	5096(%rax), %eax
	cmpq	$4, %rax
	je	.LBB26_10
# BB#6:                                 # %if.then9
	.loc	1 1303 0                # bzlib.c:1303:0
.Ltmp691:
	cmpq	$0, -8(%rbp)
	je	.LBB26_8
# BB#7:                                 # %if.then11
	movq	-8(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, (%rax)
.Ltmp692:
.LBB26_8:                               # %if.end12
	.loc	1 1303 0                # bzlib.c:1303:0
	cmpq	$0, -40(%rbp)
	je	.LBB26_17
# BB#9:                                 # %if.then14
	movq	-40(%rbp), %rax
	movabsq	$4294967295, %rcx       # imm = 0xFFFFFFFF
	movl	%ecx, 5096(%rax)
.Ltmp693:
	.loc	1 1310 0                # bzlib.c:1310:0
	popq	%rbp
	retq
.LBB26_10:                              # %if.end17
	.loc	1 1304 0                # bzlib.c:1304:0
.Ltmp694:
	cmpq	$0, -24(%rbp)
	je	.LBB26_1
# BB#11:                                # %lor.lhs.false
	cmpq	$0, -32(%rbp)
	je	.LBB26_1
.Ltmp695:
# BB#12:                                # %if.end28
	.loc	1 1307 0                # bzlib.c:1307:0
	cmpq	$0, -8(%rbp)
	je	.LBB26_14
# BB#13:                                # %if.then30
	movq	-8(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, (%rax)
.Ltmp696:
.LBB26_14:                              # %if.end31
	.loc	1 1307 0                # bzlib.c:1307:0
	cmpq	$0, -40(%rbp)
	je	.LBB26_16
# BB#15:                                # %if.then33
	movq	-40(%rbp), %rax
	xorq	%rcx, %rcx
	movl	%ecx, 5096(%rax)
.Ltmp697:
.LBB26_16:                              # %if.end35
	.loc	1 1308 0                # bzlib.c:1308:0
	movq	-40(%rbp), %rax
	movl	5024(%rax), %eax
	movq	-32(%rbp), %rcx
	movl	%eax, (%rcx)
	.loc	1 1309 0                # bzlib.c:1309:0
	movq	-40(%rbp), %rax
	movq	5016(%rax), %rax
	movq	-24(%rbp), %rcx
	movq	%rax, (%rcx)
	jmp	.LBB26_17
.LBB26_1:                               # %if.then
	.loc	1 1301 0                # bzlib.c:1301:0
.Ltmp698:
	cmpq	$0, -8(%rbp)
	je	.LBB26_3
# BB#2:                                 # %if.then2
	movq	-8(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, (%rax)
.Ltmp699:
.LBB26_3:                               # %if.end
	.loc	1 1301 0                # bzlib.c:1301:0
	cmpq	$0, -40(%rbp)
	je	.LBB26_17
# BB#4:                                 # %if.then4
	movq	-40(%rbp), %rax
	movabsq	$4294967294, %rcx       # imm = 0xFFFFFFFE
	movl	%ecx, 5096(%rax)
.Ltmp700:
	.loc	1 1310 0                # bzlib.c:1310:0
	popq	%rbp
	retq
.LBB26_17:                              # %return
	popq	%rbp
	retq
.Ltmp701:
.Ltmp702:
	.size	BZ2_bzReadGetUnused, .Ltmp702-BZ2_bzReadGetUnused
.Lfunc_end26:
	.cfi_endproc

	.globl	BZ2_bzBuffToBuffCompress
	.align	16, 0x90
	.type	BZ2_bzBuffToBuffCompress,@function
BZ2_bzBuffToBuffCompress:               # @BZ2_bzBuffToBuffCompress
.Lfunc_begin27:
	.loc	1 1327 0                # bzlib.c:1327:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp706:
	.cfi_def_cfa_offset 16
.Ltmp707:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp708:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$136, %rsp
.Ltmp709:
	.cfi_offset %rbx, -24
	movq	16(%rbp), %rax
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	movl	%ecx, -44(%rbp)
	.loc	1 1331 0 prologue_end   # bzlib.c:1331:0
.Ltmp710:
	cmpq	$0, -24(%rbp)
	movl	%r8d, -48(%rbp)
	movl	%r9d, -52(%rbp)
	movl	%eax, -56(%rbp)
	je	.LBB27_9
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, -32(%rbp)
	je	.LBB27_9
# BB#2:                                 # %lor.lhs.false2
	cmpq	$0, -40(%rbp)
	je	.LBB27_9
# BB#3:                                 # %lor.lhs.false4
	movslq	-48(%rbp), %rax
	testq	%rax, %rax
	jle	.LBB27_9
# BB#4:                                 # %lor.lhs.false6
	movslq	-48(%rbp), %rax
	cmpq	$9, %rax
	jg	.LBB27_9
# BB#5:                                 # %lor.lhs.false8
	movslq	-52(%rbp), %rax
	testq	%rax, %rax
	js	.LBB27_9
# BB#6:                                 # %lor.lhs.false10
	movslq	-52(%rbp), %rax
	cmpq	$4, %rax
	jg	.LBB27_9
# BB#7:                                 # %lor.lhs.false12
	movslq	-56(%rbp), %rax
	testq	%rax, %rax
	js	.LBB27_9
# BB#8:                                 # %lor.lhs.false14
	movslq	-56(%rbp), %rax
	cmpq	$251, %rax
	jl	.LBB27_10
.Ltmp711:
.LBB27_9:                               # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB27_19:                              # %return
	.loc	1 1366 0                # bzlib.c:1366:0
	movl	%eax, -12(%rbp)
	.loc	1 1367 0                # bzlib.c:1367:0
	movl	-12(%rbp), %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB27_10:                              # %if.end
	.loc	1 1338 0                # bzlib.c:1338:0
.Ltmp712:
	movl	-56(%rbp), %eax
	testq	%rax, %rax
	jne	.LBB27_12
# BB#11:                                # %if.then17
	movq	$30, %rax
	movl	%eax, -56(%rbp)
.Ltmp713:
.LBB27_12:                              # %if.end18
	.loc	1 1342 0                # bzlib.c:1342:0
	movl	-56(%rbp), %ecx
	movl	-52(%rbp), %edx
	movl	-48(%rbp), %esi
	.loc	1 1339 0                # bzlib.c:1339:0
	movq	$0, -80(%rbp)
	.loc	1 1340 0                # bzlib.c:1340:0
	movq	$0, -72(%rbp)
	.loc	1 1341 0                # bzlib.c:1341:0
	movq	$0, -64(%rbp)
	leaq	-136(%rbp), %rdi
	.loc	1 1342 0                # bzlib.c:1342:0
	callq	BZ2_bzCompressInit
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movl	%eax, -140(%rbp)
	jne	.LBB27_18
# BB#13:                                # %if.end21
	.loc	1 1346 0                # bzlib.c:1346:0
	movq	-40(%rbp), %rax
	.loc	1 1347 0                # bzlib.c:1347:0
	movq	-24(%rbp), %rcx
	.loc	1 1348 0                # bzlib.c:1348:0
	movl	-44(%rbp), %edx
	.loc	1 1349 0                # bzlib.c:1349:0
	movq	-32(%rbp), %rsi
	.loc	1 1346 0                # bzlib.c:1346:0
	movq	%rax, -136(%rbp)
	.loc	1 1347 0                # bzlib.c:1347:0
	movq	%rcx, -112(%rbp)
	.loc	1 1348 0                # bzlib.c:1348:0
	movl	%edx, -128(%rbp)
	.loc	1 1349 0                # bzlib.c:1349:0
	movl	(%rsi), %eax
	movl	%eax, -104(%rbp)
	leaq	-136(%rbp), %rdi
	movq	$2, %rsi
	.loc	1 1351 0                # bzlib.c:1351:0
	callq	BZ2_bzCompress
	andq	%rax, %rbx
	cmpq	$3, %rbx
	movl	%eax, -140(%rbp)
	jne	.LBB27_14
# BB#16:                                # %output_overflow
	leaq	-136(%rbp), %rdi
	.loc	1 1361 0                # bzlib.c:1361:0
	callq	BZ2_bzCompressEnd
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	jmp	.LBB27_19
.LBB27_14:                              # %if.end25
	.loc	1 1353 0                # bzlib.c:1353:0
.Ltmp714:
	movl	-140(%rbp), %eax
	cmpq	$4, %rax
	je	.LBB27_15
.Ltmp715:
# BB#17:                                # %errhandler
	leaq	-136(%rbp), %rdi
	.loc	1 1365 0                # bzlib.c:1365:0
	callq	BZ2_bzCompressEnd
.LBB27_18:                              # %return
	.loc	1 1366 0                # bzlib.c:1366:0
	movl	-140(%rbp), %eax
	jmp	.LBB27_19
.LBB27_15:                              # %if.end28
	.loc	1 1356 0                # bzlib.c:1356:0
	movq	-32(%rbp), %rax
	movl	-104(%rbp), %ecx
	movl	(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, (%rax)
	leaq	-136(%rbp), %rdi
	.loc	1 1357 0                # bzlib.c:1357:0
	callq	BZ2_bzCompressEnd
	xorq	%rax, %rax
	jmp	.LBB27_19
.Ltmp716:
.Ltmp717:
	.size	BZ2_bzBuffToBuffCompress, .Ltmp717-BZ2_bzBuffToBuffCompress
.Lfunc_end27:
	.cfi_endproc

	.globl	BZ2_bzBuffToBuffDecompress
	.align	16, 0x90
	.type	BZ2_bzBuffToBuffDecompress,@function
BZ2_bzBuffToBuffDecompress:             # @BZ2_bzBuffToBuffDecompress
.Lfunc_begin28:
	.loc	1 1378 0                # bzlib.c:1378:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp721:
	.cfi_def_cfa_offset 16
.Ltmp722:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp723:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$136, %rsp
.Ltmp724:
	.cfi_offset %rbx, -24
	movq	%rdi, -24(%rbp)
	movq	%rsi, -32(%rbp)
	movq	%rdx, -40(%rbp)
	.loc	1 1382 0 prologue_end   # bzlib.c:1382:0
.Ltmp725:
	cmpq	$0, -24(%rbp)
	movl	%ecx, -44(%rbp)
	movl	%r8d, -48(%rbp)
	movl	%r9d, -52(%rbp)
	je	.LBB28_7
# BB#1:                                 # %lor.lhs.false
	cmpq	$0, -32(%rbp)
	je	.LBB28_7
# BB#2:                                 # %lor.lhs.false2
	cmpq	$0, -40(%rbp)
	je	.LBB28_7
# BB#3:                                 # %lor.lhs.false4
	movl	-48(%rbp), %eax
	testq	%rax, %rax
	je	.LBB28_5
# BB#4:                                 # %land.lhs.true
	movl	-48(%rbp), %eax
	cmpq	$1, %rax
	jne	.LBB28_7
.LBB28_5:                               # %lor.lhs.false7
	movslq	-52(%rbp), %rax
	testq	%rax, %rax
	js	.LBB28_7
# BB#6:                                 # %lor.lhs.false9
	movslq	-52(%rbp), %rax
	cmpq	$5, %rax
	jl	.LBB28_8
.Ltmp726:
.LBB28_7:                               # %if.then
	movabsq	$4294967294, %rax       # imm = 0xFFFFFFFE
.LBB28_17:                              # %return
	.loc	1 1419 0                # bzlib.c:1419:0
	movl	%eax, -12(%rbp)
	.loc	1 1420 0                # bzlib.c:1420:0
	movl	-12(%rbp), %eax
	addq	$136, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB28_8:                               # %if.end
	.loc	1 1391 0                # bzlib.c:1391:0
	movl	-48(%rbp), %edx
	movl	-52(%rbp), %esi
	.loc	1 1388 0                # bzlib.c:1388:0
	movq	$0, -80(%rbp)
	.loc	1 1389 0                # bzlib.c:1389:0
	movq	$0, -72(%rbp)
	.loc	1 1390 0                # bzlib.c:1390:0
	movq	$0, -64(%rbp)
	leaq	-136(%rbp), %rdi
	.loc	1 1391 0                # bzlib.c:1391:0
	callq	BZ2_bzDecompressInit
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	testq	%rbx, %rax
	movl	%eax, -140(%rbp)
	jne	.LBB28_16
# BB#9:                                 # %if.end13
	.loc	1 1394 0                # bzlib.c:1394:0
	movq	-40(%rbp), %rax
	.loc	1 1395 0                # bzlib.c:1395:0
	movq	-24(%rbp), %rcx
	.loc	1 1396 0                # bzlib.c:1396:0
	movl	-44(%rbp), %edx
	.loc	1 1397 0                # bzlib.c:1397:0
	movq	-32(%rbp), %rsi
	.loc	1 1394 0                # bzlib.c:1394:0
	movq	%rax, -136(%rbp)
	.loc	1 1395 0                # bzlib.c:1395:0
	movq	%rcx, -112(%rbp)
	.loc	1 1396 0                # bzlib.c:1396:0
	movl	%edx, -128(%rbp)
	.loc	1 1397 0                # bzlib.c:1397:0
	movl	(%rsi), %eax
	movl	%eax, -104(%rbp)
	leaq	-136(%rbp), %rdi
	.loc	1 1399 0                # bzlib.c:1399:0
	callq	BZ2_bzDecompress
	testq	%rbx, %rax
	movl	%eax, -140(%rbp)
	.loc	1 1400 0                # bzlib.c:1400:0
.Ltmp727:
	je	.LBB28_12
.Ltmp728:
# BB#10:                                # %if.end17
	.loc	1 1401 0                # bzlib.c:1401:0
	movl	-140(%rbp), %eax
	cmpq	$4, %rax
	je	.LBB28_11
.Ltmp729:
# BB#15:                                # %errhandler
	leaq	-136(%rbp), %rdi
	.loc	1 1418 0                # bzlib.c:1418:0
	callq	BZ2_bzDecompressEnd
.LBB28_16:                              # %return
	.loc	1 1419 0                # bzlib.c:1419:0
	movl	-140(%rbp), %eax
	jmp	.LBB28_17
.LBB28_12:                              # %output_overflow_or_eof
	.loc	1 1409 0                # bzlib.c:1409:0
.Ltmp730:
	movl	-104(%rbp), %eax
	leaq	-136(%rbp), %rdi
	testq	%rax, %rax
	je	.LBB28_14
# BB#13:                                # %if.then25
	.loc	1 1410 0                # bzlib.c:1410:0
.Ltmp731:
	callq	BZ2_bzDecompressEnd
	movabsq	$4294967289, %rax       # imm = 0xFFFFFFF9
	jmp	.LBB28_17
.Ltmp732:
.LBB28_11:                              # %if.end20
	.loc	1 1404 0                # bzlib.c:1404:0
	movq	-32(%rbp), %rax
	movl	-104(%rbp), %ecx
	movl	(%rax), %edx
	subq	%rcx, %rdx
	movl	%edx, (%rax)
	leaq	-136(%rbp), %rdi
	.loc	1 1405 0                # bzlib.c:1405:0
	callq	BZ2_bzDecompressEnd
	xorq	%rax, %rax
	jmp	.LBB28_17
.LBB28_14:                              # %if.else
	.loc	1 1413 0                # bzlib.c:1413:0
.Ltmp733:
	callq	BZ2_bzDecompressEnd
	movabsq	$4294967288, %rax       # imm = 0xFFFFFFF8
	jmp	.LBB28_17
.Ltmp734:
.Ltmp735:
	.size	BZ2_bzBuffToBuffDecompress, .Ltmp735-BZ2_bzBuffToBuffDecompress
.Lfunc_end28:
	.cfi_endproc

	.globl	BZ2_bzopen
	.align	16, 0x90
	.type	BZ2_bzopen,@function
BZ2_bzopen:                             # @BZ2_bzopen
.Lfunc_begin29:
	.loc	1 1554 0                # bzlib.c:1554:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp738:
	.cfi_def_cfa_offset 16
.Ltmp739:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp740:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 1555 0 prologue_end   # bzlib.c:1555:0
.Ltmp741:
	movq	-8(%rbp), %rdi
	movq	%rsi, %rax
	movq	%rax, -16(%rbp)
	movabsq	$4294967295, %rsi       # imm = 0xFFFFFFFF
	xorq	%rcx, %rcx
	movq	%rax, %rdx
	callq	bzopen_or_bzdopen
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp742:
.Ltmp743:
	.size	BZ2_bzopen, .Ltmp743-BZ2_bzopen
.Lfunc_end29:
	.cfi_endproc

	.align	16, 0x90
	.type	bzopen_or_bzdopen,@function
bzopen_or_bzdopen:                      # @bzopen_or_bzdopen
.Lfunc_begin30:
	.loc	1 1463 0                # bzlib.c:1463:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp747:
	.cfi_def_cfa_offset 16
.Ltmp748:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp749:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$5104, %rsp             # imm = 0x13F0
.Ltmp750:
	.cfi_offset %rbx, -32
.Ltmp751:
	.cfi_offset %r14, -24
	movq	%rdi, -32(%rbp)
	movl	%esi, -36(%rbp)
	movq	%rdx, -48(%rbp)
	movl	%ecx, -52(%rbp)
	movq	$9, %rax
	.loc	1 1466 0 prologue_end   # bzlib.c:1466:0
.Ltmp752:
	movl	%eax, -5060(%rbp)
	xorq	%rax, %rax
	.loc	1 1467 0                # bzlib.c:1467:0
	movl	%eax, -5064(%rbp)
	.loc	1 1468 0                # bzlib.c:1468:0
	movw	%ax, -5072(%rbp)
	movq	$0, -5080(%rbp)
	.loc	1 1472 0                # bzlib.c:1472:0
	movq	$0, -5088(%rbp)
	.loc	1 1474 0                # bzlib.c:1474:0
	movq	$0, -5096(%rbp)
	.loc	1 1475 0                # bzlib.c:1475:0
	movl	%eax, -5100(%rbp)
	movq	$30, %rcx
	.loc	1 1480 0                # bzlib.c:1480:0
.Ltmp753:
	cmpq	$0, -48(%rbp)
.Ltmp754:
	.loc	1 1476 0                # bzlib.c:1476:0
	movl	%ecx, -5104(%rbp)
	.loc	1 1477 0                # bzlib.c:1477:0
	movl	%eax, -5108(%rbp)
	.loc	1 1478 0                # bzlib.c:1478:0
	movl	%eax, -5112(%rbp)
	je	.LBB30_39
# BB#1:
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	movq	$1, %rcx
	jmp	.LBB30_2
	.align	16, 0x90
.LBB30_8:                               # %sw.bb2
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1488 0                # bzlib.c:1488:0
.Ltmp755:
	movl	%ecx, -5108(%rbp)
.Ltmp756:
	.loc	1 1494 0                # bzlib.c:1494:0
	incq	-48(%rbp)
.Ltmp757:
.LBB30_2:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 1481 0                # bzlib.c:1481:0
	movq	-48(%rbp), %rdx
	movzbl	(%rdx), %edx
	testq	%rdx, %rdx
	je	.LBB30_12
# BB#3:                                 # %while.body
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1482 0                # bzlib.c:1482:0
.Ltmp758:
	movq	-48(%rbp), %rdx
	movsbq	(%rdx), %rdx
	andq	%rbx, %rdx
	cmpq	$119, %rdx
	je	.LBB30_7
# BB#4:                                 # %while.body
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	$115, %rdx
	je	.LBB30_8
# BB#5:                                 # %while.body
                                        #   in Loop: Header=BB30_2 Depth=1
	cmpq	$114, %rdx
	jne	.LBB30_9
# BB#6:                                 # %sw.bb
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1484 0                # bzlib.c:1484:0
.Ltmp759:
	movl	%eax, -5064(%rbp)
.Ltmp760:
	.loc	1 1494 0                # bzlib.c:1494:0
	incq	-48(%rbp)
	jmp	.LBB30_2
	.align	16, 0x90
.LBB30_7:                               # %sw.bb1
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1486 0                # bzlib.c:1486:0
.Ltmp761:
	movl	%ecx, -5064(%rbp)
.Ltmp762:
	.loc	1 1494 0                # bzlib.c:1494:0
	incq	-48(%rbp)
	jmp	.LBB30_2
.LBB30_9:                               # %sw.default
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1490 0                # bzlib.c:1490:0
.Ltmp763:
	movq	-48(%rbp), %rdx
	movsbq	(%rdx), %rdx
	addq	$-48, %rdx
	andq	%rbx, %rdx
	cmpq	$9, %rdx
	ja	.LBB30_11
# BB#10:                                # %if.then6
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1491 0                # bzlib.c:1491:0
.Ltmp764:
	movq	-48(%rbp), %rdx
	movsbq	(%rdx), %rdx
	addq	$-48, %rdx
	movl	%edx, -5060(%rbp)
.Ltmp765:
.LBB30_11:                              # %sw.epilog
                                        #   in Loop: Header=BB30_2 Depth=1
	.loc	1 1494 0                # bzlib.c:1494:0
	incq	-48(%rbp)
	jmp	.LBB30_2
.Ltmp766:
.LBB30_12:                              # %while.end
	.loc	1 1496 0                # bzlib.c:1496:0
	movl	-5064(%rbp), %eax
	testq	%rax, %rax
	je	.LBB30_14
# BB#13:
	movabsq	$.L.str17, %rsi
	jmp	.LBB30_15
.LBB30_39:                              # %if.then
	.loc	1 1480 0                # bzlib.c:1480:0
.Ltmp767:
	movq	$0, -24(%rbp)
	jmp	.LBB30_38
.Ltmp768:
.LBB30_14:                              # %select.mid
	movabsq	$.L.str18, %rsi
.LBB30_15:                              # %select.end
	leaq	-5080(%rbp), %r14
	.loc	1 1496 0                # bzlib.c:1496:0
	movq	%r14, %rdi
	callq	strcat
	.loc	1 1497 0                # bzlib.c:1497:0
	movabsq	$.L.str19, %rsi
	movq	%r14, %rdi
	callq	strcat
	.loc	1 1499 0                # bzlib.c:1499:0
.Ltmp769:
	movl	-52(%rbp), %eax
	testq	%rax, %rax
	je	.LBB30_16
# BB#22:                                # %if.else27
	.loc	1 1517 0                # bzlib.c:1517:0
.Ltmp770:
	movl	-36(%rbp), %edi
	leaq	-5080(%rbp), %rsi
	callq	fdopen
	jmp	.LBB30_23
.Ltmp771:
.LBB30_16:                              # %if.then15
	.loc	1 1500 0                # bzlib.c:1500:0
	cmpq	$0, -32(%rbp)
	je	.LBB30_18
# BB#17:                                # %lor.lhs.false
	movq	-32(%rbp), %rdi
	movabsq	$.L.str20, %rsi
	callq	strcmp
	testq	%rbx, %rax
	je	.LBB30_18
# BB#21:                                # %if.else
	.loc	1 1508 0                # bzlib.c:1508:0
.Ltmp772:
	movq	-32(%rbp), %rdi
	leaq	-5080(%rbp), %rsi
	callq	fopen
	jmp	.LBB30_23
.Ltmp773:
.LBB30_18:                              # %if.then21
	.loc	1 1504 0                # bzlib.c:1504:0
	movl	-5064(%rbp), %eax
	testq	%rax, %rax
	je	.LBB30_20
# BB#19:                                # %cond.true
	movq	stdout(%rip), %rax
	jmp	.LBB30_23
.LBB30_20:                              # %cond.false
	movq	stdin(%rip), %rax
.Ltmp774:
.LBB30_23:                              # %if.end30
	.loc	1 1517 0                # bzlib.c:1517:0
	movq	%rax, -5088(%rbp)
.Ltmp775:
	.loc	1 1521 0                # bzlib.c:1521:0
	cmpq	$0, -5088(%rbp)
	je	.LBB30_24
.Ltmp776:
# BB#25:                                # %if.end34
	.loc	1 1523 0                # bzlib.c:1523:0
	movl	-5064(%rbp), %eax
	testq	%rax, %rax
	je	.LBB30_31
# BB#26:                                # %if.then36
	.loc	1 1525 0                # bzlib.c:1525:0
.Ltmp777:
	movslq	-5060(%rbp), %rax
	testq	%rax, %rax
	jg	.LBB30_28
# BB#27:                                # %if.then39
	movq	$1, %rax
	movl	%eax, -5060(%rbp)
.Ltmp778:
.LBB30_28:                              # %if.end40
	.loc	1 1526 0                # bzlib.c:1526:0
	movslq	-5060(%rbp), %rax
	cmpq	$10, %rax
	jl	.LBB30_30
# BB#29:                                # %if.then43
	movq	$9, %rax
	movl	%eax, -5060(%rbp)
.Ltmp779:
.LBB30_30:                              # %if.end44
	.loc	1 1527 0                # bzlib.c:1527:0
	movq	-5088(%rbp), %rsi
	movl	-5104(%rbp), %r8d
	movl	-5100(%rbp), %ecx
	movl	-5060(%rbp), %edx
	leaq	-56(%rbp), %rdi
	callq	BZ2_bzWriteOpen
	jmp	.LBB30_32
.Ltmp780:
.LBB30_24:                              # %if.then33
	.loc	1 1521 0                # bzlib.c:1521:0
	movq	$0, -24(%rbp)
	jmp	.LBB30_38
.Ltmp781:
.LBB30_31:                              # %if.else46
	.loc	1 1530 0                # bzlib.c:1530:0
	movq	-5088(%rbp), %rsi
	movl	-5112(%rbp), %r9d
	movl	-5108(%rbp), %ecx
	movl	-5100(%rbp), %edx
	leaq	-56(%rbp), %rdi
	leaq	-5056(%rbp), %r8
	callq	BZ2_bzReadOpen
.LBB30_32:                              # %if.end49
	movq	%rax, -5096(%rbp)
.Ltmp782:
	.loc	1 1533 0                # bzlib.c:1533:0
	cmpq	$0, -5096(%rbp)
	je	.LBB30_33
.Ltmp783:
# BB#37:                                # %if.end60
	.loc	1 1541 0                # bzlib.c:1541:0
	movq	-5096(%rbp), %rax
	movq	%rax, -24(%rbp)
	jmp	.LBB30_38
.LBB30_33:                              # %if.then52
	.loc	1 1537 0                # bzlib.c:1537:0
.Ltmp784:
	movq	-5088(%rbp), %rax
	movq	stdin(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_36
# BB#34:                                # %land.lhs.true
	movq	-5088(%rbp), %rax
	movq	stdout(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB30_36
# BB#35:                                # %if.then57
	movq	-5088(%rbp), %rdi
	callq	fclose
.Ltmp785:
.LBB30_36:                              # %if.end59
	.loc	1 1539 0                # bzlib.c:1539:0
	movq	$0, -24(%rbp)
.Ltmp786:
.LBB30_38:                              # %return
	.loc	1 1542 0                # bzlib.c:1542:0
	movq	-24(%rbp), %rax
	addq	$5104, %rsp             # imm = 0x13F0
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.Ltmp787:
.Ltmp788:
	.size	bzopen_or_bzdopen, .Ltmp788-bzopen_or_bzdopen
.Lfunc_end30:
	.cfi_endproc

	.globl	BZ2_bzdopen
	.align	16, 0x90
	.type	BZ2_bzdopen,@function
BZ2_bzdopen:                            # @BZ2_bzdopen
.Lfunc_begin31:
	.loc	1 1563 0                # bzlib.c:1563:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp791:
	.cfi_def_cfa_offset 16
.Ltmp792:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp793:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movl	%edi, -4(%rbp)
	.loc	1 1564 0 prologue_end   # bzlib.c:1564:0
.Ltmp794:
	movl	-4(%rbp), %eax
	movq	%rsi, %rdx
	movq	%rdx, -16(%rbp)
	xorq	%rdi, %rdi
	movq	$1, %rcx
	movq	%rax, %rsi
	callq	bzopen_or_bzdopen
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp795:
.Ltmp796:
	.size	BZ2_bzdopen, .Ltmp796-BZ2_bzdopen
.Lfunc_end31:
	.cfi_endproc

	.globl	BZ2_bzread
	.align	16, 0x90
	.type	BZ2_bzread,@function
BZ2_bzread:                             # @BZ2_bzread
.Lfunc_begin32:
	.loc	1 1570 0                # bzlib.c:1570:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp799:
	.cfi_def_cfa_offset 16
.Ltmp800:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp801:
	.cfi_def_cfa_register %rbp
	subq	$48, %rsp
	movq	%rdi, -16(%rbp)
	.loc	1 1572 0 prologue_end   # bzlib.c:1572:0
.Ltmp802:
	movq	-16(%rbp), %rax
	movq	%rsi, -24(%rbp)
	movl	%edx, -28(%rbp)
	movl	5096(%rax), %eax
	cmpq	$4, %rax
	jne	.LBB32_2
.Ltmp803:
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	jmp	.LBB32_6
.LBB32_2:                               # %if.end
	.loc	1 1573 0                # bzlib.c:1573:0
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %ecx
	leaq	-32(%rbp), %rdi
	callq	BZ2_bzRead
	.loc	1 1574 0                # bzlib.c:1574:0
.Ltmp804:
	movl	-32(%rbp), %ecx
.Ltmp805:
	.loc	1 1573 0                # bzlib.c:1573:0
	movl	%eax, -36(%rbp)
	.loc	1 1574 0                # bzlib.c:1574:0
.Ltmp806:
	testq	%rcx, %rcx
	je	.LBB32_4
# BB#3:                                 # %lor.lhs.false
	movl	-32(%rbp), %eax
	cmpq	$4, %rax
	jne	.LBB32_5
.LBB32_4:                               # %if.then3
	.loc	1 1575 0                # bzlib.c:1575:0
.Ltmp807:
	movl	-36(%rbp), %eax
	jmp	.LBB32_6
.Ltmp808:
.LBB32_5:                               # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
.LBB32_6:                               # %return
	.loc	1 1577 0                # bzlib.c:1577:0
.Ltmp809:
	movl	%eax, -4(%rbp)
.Ltmp810:
	.loc	1 1579 0                # bzlib.c:1579:0
	movl	-4(%rbp), %eax
	addq	$48, %rsp
	popq	%rbp
	retq
.Ltmp811:
.Ltmp812:
	.size	BZ2_bzread, .Ltmp812-BZ2_bzread
.Lfunc_end32:
	.cfi_endproc

	.globl	BZ2_bzwrite
	.align	16, 0x90
	.type	BZ2_bzwrite,@function
BZ2_bzwrite:                            # @BZ2_bzwrite
.Lfunc_begin33:
	.loc	1 1584 0                # bzlib.c:1584:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp815:
	.cfi_def_cfa_offset 16
.Ltmp816:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp817:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movq	%rsi, -24(%rbp)
	movl	%edx, -28(%rbp)
	.loc	1 1587 0 prologue_end   # bzlib.c:1587:0
.Ltmp818:
	movq	-16(%rbp), %rsi
	movq	-24(%rbp), %rdx
	movl	-28(%rbp), %ecx
	leaq	-32(%rbp), %rdi
	callq	BZ2_bzWrite
	.loc	1 1588 0                # bzlib.c:1588:0
.Ltmp819:
	movl	-32(%rbp), %eax
	testq	%rax, %rax
	je	.LBB33_1
# BB#2:                                 # %if.else
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB33_3
.LBB33_1:                               # %if.then
	.loc	1 1589 0                # bzlib.c:1589:0
.Ltmp820:
	movl	-28(%rbp), %eax
.Ltmp821:
.LBB33_3:                               # %return
	.loc	1 1591 0                # bzlib.c:1591:0
	movl	%eax, -4(%rbp)
.Ltmp822:
	.loc	1 1593 0                # bzlib.c:1593:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp823:
.Ltmp824:
	.size	BZ2_bzwrite, .Ltmp824-BZ2_bzwrite
.Lfunc_end33:
	.cfi_endproc

	.globl	BZ2_bzflush
	.align	16, 0x90
	.type	BZ2_bzflush,@function
BZ2_bzflush:                            # @BZ2_bzflush
.Lfunc_begin34:
	.loc	1 1598 0                # bzlib.c:1598:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp827:
	.cfi_def_cfa_offset 16
.Ltmp828:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp829:
	.cfi_def_cfa_register %rbp
	xorq	%rax, %rax
	movq	%rdi, -8(%rbp)
	.loc	1 1600 0 prologue_end   # bzlib.c:1600:0
.Ltmp830:
	popq	%rbp
	retq
.Ltmp831:
.Ltmp832:
	.size	BZ2_bzflush, .Ltmp832-BZ2_bzflush
.Lfunc_end34:
	.cfi_endproc

	.globl	BZ2_bzclose
	.align	16, 0x90
	.type	BZ2_bzclose,@function
BZ2_bzclose:                            # @BZ2_bzclose
.Lfunc_begin35:
	.loc	1 1606 0                # bzlib.c:1606:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp835:
	.cfi_def_cfa_offset 16
.Ltmp836:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp837:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 1611 0 prologue_end   # bzlib.c:1611:0
.Ltmp838:
	movq	(%rdi), %rax
	.loc	1 1614 0                # bzlib.c:1614:0
.Ltmp839:
	cmpq	$0, -8(%rbp)
.Ltmp840:
	.loc	1 1611 0                # bzlib.c:1611:0
	movq	%rax, -24(%rbp)
	je	.LBB35_8
# BB#1:                                 # %if.end
	.loc	1 1615 0                # bzlib.c:1615:0
.Ltmp841:
	movq	-8(%rbp), %rax
	movzbl	5012(%rax), %eax
	testq	%rax, %rax
	je	.LBB35_4
# BB#2:                                 # %if.then1
	.loc	1 1616 0                # bzlib.c:1616:0
.Ltmp842:
	movq	-8(%rbp), %rsi
	leaq	-12(%rbp), %rdi
	xorq	%rdx, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	callq	BZ2_bzWriteClose
	.loc	1 1617 0                # bzlib.c:1617:0
.Ltmp843:
	movl	-12(%rbp), %eax
	testq	%rax, %rax
	je	.LBB35_5
# BB#3:                                 # %if.then3
	.loc	1 1618 0                # bzlib.c:1618:0
.Ltmp844:
	movq	-8(%rbp), %rsi
	xorq	%rdi, %rdi
	movq	$1, %rdx
	xorq	%rcx, %rcx
	xorq	%r8, %r8
	callq	BZ2_bzWriteClose
	jmp	.LBB35_5
.Ltmp845:
.LBB35_4:                               # %if.else
	.loc	1 1621 0                # bzlib.c:1621:0
	movq	-8(%rbp), %rsi
	leaq	-12(%rbp), %rdi
	callq	BZ2_bzReadClose
.Ltmp846:
.LBB35_5:                               # %if.end5
	.loc	1 1626 0                # bzlib.c:1626:0
	movq	-24(%rbp), %rax
	movq	stdin(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB35_8
# BB#6:                                 # %land.lhs.true
	movq	-24(%rbp), %rax
	movq	stdout(%rip), %rcx
	cmpq	%rcx, %rax
	je	.LBB35_8
# BB#7:                                 # %if.then8
	.loc	1 1628 0                # bzlib.c:1628:0
.Ltmp847:
	movq	-24(%rbp), %rdi
	callq	fclose
.Ltmp848:
.LBB35_8:                               # %if.end9
	.loc	1 1630 0                # bzlib.c:1630:0
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp849:
.Ltmp850:
	.size	BZ2_bzclose, .Ltmp850-BZ2_bzclose
.Lfunc_end35:
	.cfi_endproc

	.globl	BZ2_bzerror
	.align	16, 0x90
	.type	BZ2_bzerror,@function
BZ2_bzerror:                            # @BZ2_bzerror
.Lfunc_begin36:
	.loc	1 1658 0                # bzlib.c:1658:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp853:
	.cfi_def_cfa_offset 16
.Ltmp854:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp855:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	.loc	1 1659 0 prologue_end   # bzlib.c:1659:0
.Ltmp856:
	movq	-8(%rbp), %rax
	movq	%rsi, -16(%rbp)
	movslq	5096(%rax), %rax
	testq	%rax, %rax
	movl	%eax, -20(%rbp)
	jle	.LBB36_2
# BB#1:                                 # %if.then
	xorq	%rax, %rax
	.loc	1 1661 0                # bzlib.c:1661:0
.Ltmp857:
	movl	%eax, -20(%rbp)
.Ltmp858:
.LBB36_2:                               # %if.end
	.loc	1 1662 0                # bzlib.c:1662:0
	movl	-20(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, (%rcx)
	.loc	1 1663 0                # bzlib.c:1663:0
	movl	-20(%rbp), %eax
	negq	%rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	sarq	%cl, %rax
	movq	bzerrorstrings(,%rax,8), %rax
	popq	%rbp
	retq
.Ltmp859:
.Ltmp860:
	.size	BZ2_bzerror, .Ltmp860-BZ2_bzerror
.Lfunc_end36:
	.cfi_endproc

	.align	16, 0x90
	.type	copy_output_until_stop,@function
copy_output_until_stop:                 # @copy_output_until_stop
.Lfunc_begin37:
	.loc	1 392 0                 # bzlib.c:392:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp863:
	.cfi_def_cfa_offset 16
.Ltmp864:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp865:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	xorq	%rax, %rax
	.loc	1 393 0 prologue_end    # bzlib.c:393:0
.Ltmp866:
	movb	%al, -9(%rbp)
	movq	$1, %rax
	jmp	.LBB37_1
	.align	16, 0x90
.LBB37_4:                               # %if.then16
                                        #   in Loop: Header=BB37_1 Depth=1
	.loc	1 409 0                 # bzlib.c:409:0
.Ltmp867:
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	40(%rcx), %edx
	incq	%rdx
	movl	%edx, 40(%rcx)
.Ltmp868:
.LBB37_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 398 0                 # bzlib.c:398:0
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	32(%rcx), %ecx
	testq	%rcx, %rcx
	je	.LBB37_5
.Ltmp869:
# BB#2:                                 # %if.end
                                        #   in Loop: Header=BB37_1 Depth=1
	.loc	1 401 0                 # bzlib.c:401:0
	movq	-8(%rbp), %rcx
	movslq	120(%rcx), %rdx
	movslq	116(%rcx), %rcx
	cmpq	%rcx, %rdx
	jge	.LBB37_5
.Ltmp870:
# BB#3:                                 # %if.end3
                                        #   in Loop: Header=BB37_1 Depth=1
	.loc	1 404 0                 # bzlib.c:404:0
	movq	-8(%rbp), %rcx
	.loc	1 403 0                 # bzlib.c:403:0
	movb	%al, -9(%rbp)
	.loc	1 404 0                 # bzlib.c:404:0
	movslq	120(%rcx), %rdx
	movq	(%rcx), %rsi
	movq	80(%rcx), %rcx
	movzbl	(%rcx,%rdx), %ecx
	movq	24(%rsi), %rdx
	movb	%cl, (%rdx)
	.loc	1 405 0                 # bzlib.c:405:0
	movq	-8(%rbp), %rcx
	movl	120(%rcx), %edx
	incq	%rdx
	movl	%edx, 120(%rcx)
	.loc	1 406 0                 # bzlib.c:406:0
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	32(%rcx), %edx
	decq	%rdx
	movl	%edx, 32(%rcx)
	.loc	1 407 0                 # bzlib.c:407:0
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	incq	24(%rcx)
	.loc	1 408 0                 # bzlib.c:408:0
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	36(%rcx), %edx
	incq	%rdx
	movl	%edx, 36(%rcx)
	.loc	1 409 0                 # bzlib.c:409:0
.Ltmp871:
	movq	-8(%rbp), %rcx
	movq	(%rcx), %rcx
	movl	36(%rcx), %ecx
	testq	%rcx, %rcx
	jne	.LBB37_1
	jmp	.LBB37_4
.Ltmp872:
.LBB37_5:                               # %while.end
	.loc	1 412 0                 # bzlib.c:412:0
	movzbl	-9(%rbp), %eax
	popq	%rbp
	retq
.Ltmp873:
.Ltmp874:
	.size	copy_output_until_stop, .Ltmp874-copy_output_until_stop
.Lfunc_end37:
	.cfi_endproc

	.align	16, 0x90
	.type	copy_input_until_stop,@function
copy_input_until_stop:                  # @copy_input_until_stop
.Lfunc_begin38:
	.loc	1 347 0                 # bzlib.c:347:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp878:
	.cfi_def_cfa_offset 16
.Ltmp879:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp880:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	subq	$40, %rsp
.Ltmp881:
	.cfi_offset %rbx, -24
	movq	%rdi, -16(%rbp)
	.loc	1 350 0 prologue_end    # bzlib.c:350:0
.Ltmp882:
	movq	-16(%rbp), %rax
	xorq	%rcx, %rcx
.Ltmp883:
	.loc	1 348 0                 # bzlib.c:348:0
	movb	%cl, -17(%rbp)
	.loc	1 350 0                 # bzlib.c:350:0
.Ltmp884:
	movl	8(%rax), %eax
	movq	$1, %rbx
	cmpq	$2, %rax
	jne	.LBB38_14
	jmp	.LBB38_1
	.align	16, 0x90
.LBB38_28:                              # %if.end151
                                        #   in Loop: Header=BB38_14 Depth=1
	.loc	1 382 0                 # bzlib.c:382:0
.Ltmp885:
	movq	-16(%rbp), %rax
	movl	16(%rax), %ecx
	decq	%rcx
	movl	%ecx, 16(%rax)
.LBB38_14:                              # %while.body60
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 371 0                 # bzlib.c:371:0
.Ltmp886:
	movq	-16(%rbp), %rax
	movslq	108(%rax), %rcx
	movslq	112(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB38_29
.Ltmp887:
# BB#15:                                # %if.end66
                                        #   in Loop: Header=BB38_14 Depth=1
	.loc	1 373 0                 # bzlib.c:373:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB38_29
.Ltmp888:
# BB#16:                                # %if.end72
                                        #   in Loop: Header=BB38_14 Depth=1
	.loc	1 375 0                 # bzlib.c:375:0
	movq	-16(%rbp), %rax
	movl	16(%rax), %eax
	testq	%rax, %rax
	je	.LBB38_29
.Ltmp889:
# BB#17:                                # %if.end76
                                        #   in Loop: Header=BB38_14 Depth=1
	.loc	1 377 0                 # bzlib.c:377:0
	movq	-16(%rbp), %rax
.Ltmp890:
	.loc	1 376 0                 # bzlib.c:376:0
	movb	%bl, -17(%rbp)
.Ltmp891:
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp892:
	movq	-16(%rbp), %rcx
.Ltmp893:
	.loc	1 377 0                 # bzlib.c:377:0
	movl	%eax, -32(%rbp)
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp894:
	movl	92(%rcx), %ecx
.Ltmp895:
	.loc	1 377 0                 # bzlib.c:377:0
	cmpq	%rcx, %rax
	je	.LBB38_20
# BB#18:                                # %land.lhs.true84
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp896:
	movq	-16(%rbp), %rax
	movl	96(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB38_20
# BB#19:                                # %if.then88
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
.Ltmp897:
	#DEBUG_VALUE: ch <- [RBP+-33]
	.loc	1 377 0                 # bzlib.c:377:0
	movq	-16(%rbp), %rax
	movzbl	92(%rax), %eax
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp898:
	movq	-16(%rbp), %rdx
.Ltmp899:
	.loc	1 377 0                 # bzlib.c:377:0
	movb	%al, -33(%rbp)
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp900:
	movl	648(%rdx), %eax
	movq	$8, %rcx
	movq	%rax, %rsi
	movzbl	-33(%rbp), %edi
	shlq	%cl, %rsi
	movq	$24, %rcx
	shrq	%cl, %rax
	xorq	%rax, %rdi
	movl	BZ2_crc32Table(,%rdi,4), %eax
	xorq	%rsi, %rax
	movl	%eax, 648(%rdx)
.Ltmp901:
	.loc	1 377 0                 # bzlib.c:377:0
	movq	-16(%rbp), %rax
	movl	92(%rax), %ecx
	movb	%bl, 128(%rax,%rcx)
	movq	-16(%rbp), %rax
	movzbl	-33(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-16(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	movl	-32(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 92(%rcx)
	jmp	.LBB38_26
.Ltmp902:
	.align	16, 0x90
.LBB38_20:                              # %if.else113
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
	movq	-16(%rbp), %rax
	movl	-32(%rbp), %ecx
	movl	92(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB38_22
# BB#21:                                # %lor.lhs.false117
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	movq	-16(%rbp), %rax
	movl	96(%rax), %eax
	cmpq	$255, %rax
	jne	.LBB38_25
.LBB38_22:                              # %if.then121
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
.Ltmp903:
	movq	-16(%rbp), %rax
	movl	92(%rax), %eax
	cmpq	$255, %rax
	ja	.LBB38_24
# BB#23:                                # %if.then125
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	movq	-16(%rbp), %rdi
	callq	add_pair_to_block
.Ltmp904:
.LBB38_24:                              # %if.end126
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
	movl	-32(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 92(%rcx)
	movq	-16(%rbp), %rax
	movl	%ebx, 96(%rax)
	jmp	.LBB38_26
.Ltmp905:
.LBB38_25:                              # %if.else129
                                        #   in Loop: Header=BB38_14 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-32]
	.loc	1 377 0                 # bzlib.c:377:0
	movq	-16(%rbp), %rax
	movl	96(%rax), %ecx
	incq	%rcx
	movl	%ecx, 96(%rax)
.Ltmp906:
	.align	16, 0x90
.LBB38_26:                              # %if.end133
                                        #   in Loop: Header=BB38_14 Depth=1
	.loc	1 378 0                 # bzlib.c:378:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	.loc	1 379 0                 # bzlib.c:379:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	.loc	1 380 0                 # bzlib.c:380:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 381 0                 # bzlib.c:381:0
.Ltmp907:
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB38_28
# BB#27:                                # %if.then147
                                        #   in Loop: Header=BB38_14 Depth=1
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
	jmp	.LBB38_28
.Ltmp908:
	.align	16, 0x90
.LBB38_13:                              # %if.then55
                                        #   in Loop: Header=BB38_1 Depth=1
	.loc	1 363 0                 # bzlib.c:363:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	16(%rax), %ecx
	incq	%rcx
	movl	%ecx, 16(%rax)
.Ltmp909:
.LBB38_1:                               # %while.body
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 355 0                 # bzlib.c:355:0
	movq	-16(%rbp), %rax
	movslq	108(%rax), %rcx
	movslq	112(%rax), %rax
	cmpq	%rax, %rcx
	jge	.LBB38_29
.Ltmp910:
# BB#2:                                 # %if.end
                                        #   in Loop: Header=BB38_1 Depth=1
	.loc	1 357 0                 # bzlib.c:357:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %eax
	testq	%rax, %rax
	je	.LBB38_29
.Ltmp911:
# BB#3:                                 # %if.end5
                                        #   in Loop: Header=BB38_1 Depth=1
	.loc	1 359 0                 # bzlib.c:359:0
	movq	-16(%rbp), %rax
.Ltmp912:
	.loc	1 358 0                 # bzlib.c:358:0
	movb	%bl, -17(%rbp)
.Ltmp913:
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
	movq	(%rax), %rax
	movq	(%rax), %rax
	movzbl	(%rax), %eax
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp914:
	movq	-16(%rbp), %rcx
.Ltmp915:
	.loc	1 359 0                 # bzlib.c:359:0
	movl	%eax, -24(%rbp)
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp916:
	movl	92(%rcx), %ecx
.Ltmp917:
	.loc	1 359 0                 # bzlib.c:359:0
	cmpq	%rcx, %rax
	je	.LBB38_6
# BB#4:                                 # %land.lhs.true
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp918:
	movq	-16(%rbp), %rax
	movl	96(%rax), %eax
	cmpq	$1, %rax
	jne	.LBB38_6
# BB#5:                                 # %if.then11
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
.Ltmp919:
	#DEBUG_VALUE: ch <- [RBP+-25]
	.loc	1 359 0                 # bzlib.c:359:0
	movq	-16(%rbp), %rax
	movzbl	92(%rax), %eax
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp920:
	movq	-16(%rbp), %rdx
.Ltmp921:
	.loc	1 359 0                 # bzlib.c:359:0
	movb	%al, -25(%rbp)
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp922:
	movl	648(%rdx), %eax
	movq	$8, %rcx
	movq	%rax, %rsi
	movzbl	-25(%rbp), %edi
	shlq	%cl, %rsi
	movq	$24, %rcx
	shrq	%cl, %rax
	xorq	%rax, %rdi
	movl	BZ2_crc32Table(,%rdi,4), %eax
	xorq	%rsi, %rax
	movl	%eax, 648(%rdx)
.Ltmp923:
	.loc	1 359 0                 # bzlib.c:359:0
	movq	-16(%rbp), %rax
	movl	92(%rax), %ecx
	movb	%bl, 128(%rax,%rcx)
	movq	-16(%rbp), %rax
	movzbl	-25(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-16(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	movl	-24(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 92(%rcx)
	jmp	.LBB38_12
.Ltmp924:
	.align	16, 0x90
.LBB38_6:                               # %if.else
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
	movq	-16(%rbp), %rax
	movl	-24(%rbp), %ecx
	movl	92(%rax), %eax
	cmpq	%rax, %rcx
	jne	.LBB38_8
# BB#7:                                 # %lor.lhs.false
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	movq	-16(%rbp), %rax
	movl	96(%rax), %eax
	cmpq	$255, %rax
	jne	.LBB38_11
.LBB38_8:                               # %if.then32
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
.Ltmp925:
	movq	-16(%rbp), %rax
	movl	92(%rax), %eax
	cmpq	$255, %rax
	ja	.LBB38_10
# BB#9:                                 # %if.then36
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	movq	-16(%rbp), %rdi
	callq	add_pair_to_block
.Ltmp926:
.LBB38_10:                              # %if.end37
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
	movl	-24(%rbp), %eax
	movq	-16(%rbp), %rcx
	movl	%eax, 92(%rcx)
	movq	-16(%rbp), %rax
	movl	%ebx, 96(%rax)
	jmp	.LBB38_12
.Ltmp927:
.LBB38_11:                              # %if.else40
                                        #   in Loop: Header=BB38_1 Depth=1
	#DEBUG_VALUE: zchh <- [RBP+-24]
	.loc	1 359 0                 # bzlib.c:359:0
	movq	-16(%rbp), %rax
	movl	96(%rax), %ecx
	incq	%rcx
	movl	%ecx, 96(%rax)
.Ltmp928:
	.align	16, 0x90
.LBB38_12:                              # %if.end44
                                        #   in Loop: Header=BB38_1 Depth=1
	.loc	1 360 0                 # bzlib.c:360:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	incq	(%rax)
	.loc	1 361 0                 # bzlib.c:361:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	8(%rax), %ecx
	decq	%rcx
	movl	%ecx, 8(%rax)
	.loc	1 362 0                 # bzlib.c:362:0
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %ecx
	incq	%rcx
	movl	%ecx, 12(%rax)
	.loc	1 363 0                 # bzlib.c:363:0
.Ltmp929:
	movq	-16(%rbp), %rax
	movq	(%rax), %rax
	movl	12(%rax), %eax
	testq	%rax, %rax
	jne	.LBB38_1
	jmp	.LBB38_13
.Ltmp930:
.LBB38_29:                              # %if.end155
	.loc	1 385 0                 # bzlib.c:385:0
	movzbl	-17(%rbp), %eax
	addq	$40, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp931:
.Ltmp932:
	.size	copy_input_until_stop, .Ltmp932-copy_input_until_stop
.Lfunc_end38:
	.cfi_endproc

	.align	16, 0x90
	.type	flush_RL,@function
flush_RL:                               # @flush_RL
.Lfunc_begin39:
	.loc	1 310 0                 # bzlib.c:310:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp935:
	.cfi_def_cfa_offset 16
.Ltmp936:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp937:
	.cfi_def_cfa_register %rbp
	subq	$16, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 311 0 prologue_end    # bzlib.c:311:0
.Ltmp938:
	movl	92(%rdi), %eax
	cmpq	$255, %rax
	ja	.LBB39_2
# BB#1:                                 # %if.then
	movq	-8(%rbp), %rdi
	callq	add_pair_to_block
.Ltmp939:
.LBB39_2:                               # %if.end
	.loc	1 312 0                 # bzlib.c:312:0
	movq	-8(%rbp), %rdi
	callq	init_RL
	.loc	1 313 0                 # bzlib.c:313:0
	addq	$16, %rsp
	popq	%rbp
	retq
.Ltmp940:
.Ltmp941:
	.size	flush_RL, .Ltmp941-flush_RL
.Lfunc_end39:
	.cfi_endproc

	.align	16, 0x90
	.type	add_pair_to_block,@function
add_pair_to_block:                      # @add_pair_to_block
.Lfunc_begin40:
	.loc	1 274 0                 # bzlib.c:274:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp944:
	.cfi_def_cfa_offset 16
.Ltmp945:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp946:
	.cfi_def_cfa_register %rbp
	movq	%rdi, -8(%rbp)
	.loc	1 276 0 prologue_end    # bzlib.c:276:0
.Ltmp947:
	movzbl	92(%rdi), %eax
	movb	%al, -13(%rbp)
	xorq	%rax, %rax
	jmp	.LBB40_1
	.align	16, 0x90
.LBB40_2:                               # %for.body
                                        #   in Loop: Header=BB40_1 Depth=1
	.loc	1 278 0                 # bzlib.c:278:0
.Ltmp948:
	movq	-8(%rbp), %rax
	movl	648(%rax), %edx
	movq	$8, %rcx
	movq	%rdx, %rsi
	movzbl	-13(%rbp), %edi
	shlq	%cl, %rsi
	movq	$24, %rcx
	shrq	%cl, %rdx
	xorq	%rdx, %rdi
	movl	BZ2_crc32Table(,%rdi,4), %ecx
	xorq	%rsi, %rcx
	movl	%ecx, 648(%rax)
.Ltmp949:
	.loc	1 277 0                 # bzlib.c:277:0
	movl	-12(%rbp), %eax
	incq	%rax
.LBB40_1:                               # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movl	%eax, -12(%rbp)
	movq	-8(%rbp), %rax
	movslq	-12(%rbp), %rcx
	movslq	96(%rax), %rax
	cmpq	%rax, %rcx
	jl	.LBB40_2
.Ltmp950:
# BB#3:                                 # %for.end
	.loc	1 280 0                 # bzlib.c:280:0
	movq	-8(%rbp), %rax
	movl	92(%rax), %ecx
	movq	$1, %rdx
	movb	%dl, 128(%rax,%rcx)
	.loc	1 281 0                 # bzlib.c:281:0
	movq	-8(%rbp), %rax
	movl	96(%rax), %eax
	cmpq	$3, %rax
	je	.LBB40_7
# BB#4:                                 # %for.end
	cmpq	$2, %rax
	je	.LBB40_8
# BB#5:                                 # %for.end
	cmpq	$1, %rax
	je	.LBB40_6
# BB#9:                                 # %sw.default
	.loc	1 295 0                 # bzlib.c:295:0
.Ltmp951:
	movq	-8(%rbp), %rax
	movslq	96(%rax), %rcx
	movq	$1, %rdx
	movb	%dl, 124(%rcx,%rax)
	.loc	1 296 0                 # bzlib.c:296:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	.loc	1 297 0                 # bzlib.c:297:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	.loc	1 298 0                 # bzlib.c:298:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	.loc	1 299 0                 # bzlib.c:299:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
	.loc	1 300 0                 # bzlib.c:300:0
	movq	-8(%rbp), %rax
	movl	96(%rax), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	addq	$-4, %rcx
	jmp	.LBB40_10
.LBB40_7:                               # %sw.bb27
	.loc	1 290 0                 # bzlib.c:290:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
.LBB40_8:                               # %sw.bb14
	.loc	1 286 0                 # bzlib.c:286:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
	movb	%cl, (%rax,%rdx)
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
.LBB40_6:                               # %sw.bb
	.loc	1 283 0                 # bzlib.c:283:0
	movq	-8(%rbp), %rax
	movzbl	-13(%rbp), %ecx
	movslq	108(%rax), %rdx
	movq	64(%rax), %rax
.LBB40_10:                              # %sw.epilog
	.loc	1 300 0                 # bzlib.c:300:0
	movb	%cl, (%rax,%rdx)
	.loc	1 301 0                 # bzlib.c:301:0
	movq	-8(%rbp), %rax
	movl	108(%rax), %ecx
	incq	%rcx
	movl	%ecx, 108(%rax)
.Ltmp952:
	.loc	1 304 0                 # bzlib.c:304:0
	popq	%rbp
	retq
.Ltmp953:
.Ltmp954:
	.size	add_pair_to_block, .Ltmp954-add_pair_to_block
.Lfunc_end40:
	.cfi_endproc

	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"\n\nbzip2/libbzip2: internal error number %d.\nThis is a bug in bzip2/libbzip2, %s.\nPlease report it to me at: jseward@bzip.org.  If this happened\nwhen you were using some program which uses libbzip2 as a\ncomponent, you should also report this bug to the author(s)\nof that program.  Please make an effort to report this bug;\ntimely and accurate bug reports eventually lead to higher\nquality software.  Thanks.  Julian Seward, 15 February 2005.\n\n"
	.size	.L.str, 443

	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"\n*** A special note about internal error number 1007 ***\n\nExperience suggests that a common cause of i.e. 1007\nis unreliable memory or other hardware.  The 1007 assertion\njust happens to cross-check the results of huge numbers of\nmemory reads/writes, and so acts (unintendedly) as a stress\ntest of your memory system.\n\nI suggest the following: try compressing the file again,\npossibly monitoring progress in detail with the -vv flag.\n\n* If the error cannot be reproduced, and/or happens at different\n  points in compression, you may have a flaky memory system.\n  Try a memory-test program.  I have used Memtest86\n  (www.memtest86.com).  At the time of writing it is free (GPLd).\n  Memtest86 tests memory much more thorougly than your BIOSs\n  power-on test, and may find failures that the BIOS doesn't.\n\n* If the error can be repeatably reproduced, this is a bug in\n  bzip2, and I would very much like to hear about it.  Please\n  let me know, and, ideally, save a copy of the file causing the\n  problem -- without which I will be unable to investigate it.\n\n"
	.size	.L.str1, 1057

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	" {0x%08x, 0x%08x}"
	.size	.L.str2, 18

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"]"
	.size	.L.str3, 2

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"\n    combined CRCs: stored = 0x%08x, computed = 0x%08x"
	.size	.L.str4, 55

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"1.0.3, 15-Feb-2005"
	.size	.L.str5, 19

	.type	bzerrorstrings,@object  # @bzerrorstrings
	.data
	.align	16
bzerrorstrings:
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
	.quad	.L.str16
	.quad	.L.str16
	.quad	.L.str16
	.quad	.L.str16
	.quad	.L.str16
	.size	bzerrorstrings, 128

	.type	.L.str6,@object         # @.str6
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str6:
	.asciz	"OK"
	.size	.L.str6, 3

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"SEQUENCE_ERROR"
	.size	.L.str7, 15

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"PARAM_ERROR"
	.size	.L.str8, 12

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"MEM_ERROR"
	.size	.L.str9, 10

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"DATA_ERROR"
	.size	.L.str10, 11

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"DATA_ERROR_MAGIC"
	.size	.L.str11, 17

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"IO_ERROR"
	.size	.L.str12, 9

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"UNEXPECTED_EOF"
	.size	.L.str13, 15

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"OUTBUFF_FULL"
	.size	.L.str14, 13

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"CONFIG_ERROR"
	.size	.L.str15, 13

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"???"
	.size	.L.str16, 4

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"w"
	.size	.L.str17, 2

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"r"
	.size	.L.str18, 2

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"b"
	.size	.L.str19, 2

	.type	.L.str20,@object        # @.str20
.L.str20:
	.zero	1
	.size	.L.str20, 1

	.data
.Ldebug_end0:
	.text
.Ldebug_end1:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"bzlib.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"bzerrorstrings"
.Linfo_string4:
	.asciz	"char"
.Linfo_string5:
	.asciz	"int"
.Linfo_string6:
	.asciz	"BZ2_bz__AssertH__fail"
.Linfo_string7:
	.asciz	"BZ2_bzCompressInit"
.Linfo_string8:
	.asciz	"BZ2_bzCompress"
.Linfo_string9:
	.asciz	"BZ2_bzCompressEnd"
.Linfo_string10:
	.asciz	"BZ2_bzDecompressInit"
.Linfo_string11:
	.asciz	"BZ2_indexIntoF"
.Linfo_string12:
	.asciz	"Int32"
.Linfo_string13:
	.asciz	"BZ2_bzDecompress"
.Linfo_string14:
	.asciz	"BZ2_bzDecompressEnd"
.Linfo_string15:
	.asciz	"BZ2_bzWriteOpen"
.Linfo_string16:
	.asciz	"BZ2_bzWrite"
.Linfo_string17:
	.asciz	"BZ2_bzWriteClose"
.Linfo_string18:
	.asciz	"BZ2_bzWriteClose64"
.Linfo_string19:
	.asciz	"BZ2_bzReadOpen"
.Linfo_string20:
	.asciz	"BZ2_bzReadClose"
.Linfo_string21:
	.asciz	"BZ2_bzRead"
.Linfo_string22:
	.asciz	"BZ2_bzReadGetUnused"
.Linfo_string23:
	.asciz	"BZ2_bzBuffToBuffCompress"
.Linfo_string24:
	.asciz	"BZ2_bzBuffToBuffDecompress"
.Linfo_string25:
	.asciz	"BZ2_bzlibVersion"
.Linfo_string26:
	.asciz	"BZ2_bzopen"
.Linfo_string27:
	.asciz	"BZ2_bzdopen"
.Linfo_string28:
	.asciz	"BZ2_bzread"
.Linfo_string29:
	.asciz	"BZ2_bzwrite"
.Linfo_string30:
	.asciz	"BZ2_bzflush"
.Linfo_string31:
	.asciz	"BZ2_bzclose"
.Linfo_string32:
	.asciz	"BZ2_bzerror"
.Linfo_string33:
	.asciz	"bzopen_or_bzdopen"
.Linfo_string34:
	.asciz	"myfeof"
.Linfo_string35:
	.asciz	"unsigned char"
.Linfo_string36:
	.asciz	"Bool"
.Linfo_string37:
	.asciz	"unRLE_obuf_to_output_FAST"
.Linfo_string38:
	.asciz	"unRLE_obuf_to_output_SMALL"
.Linfo_string39:
	.asciz	"isempty_RL"
.Linfo_string40:
	.asciz	"handle_compress"
.Linfo_string41:
	.asciz	"flush_RL"
.Linfo_string42:
	.asciz	"add_pair_to_block"
.Linfo_string43:
	.asciz	"copy_input_until_stop"
.Linfo_string44:
	.asciz	"copy_output_until_stop"
.Linfo_string45:
	.asciz	"prepare_new_block"
.Linfo_string46:
	.asciz	"init_RL"
.Linfo_string47:
	.asciz	"default_bzfree"
.Linfo_string48:
	.asciz	"default_bzalloc"
.Linfo_string49:
	.asciz	"bz_config_ok"
.Linfo_string50:
	.asciz	"errcode"
.Linfo_string51:
	.asciz	"strm"
.Linfo_string52:
	.asciz	"next_in"
.Linfo_string53:
	.asciz	"avail_in"
.Linfo_string54:
	.asciz	"unsigned int"
.Linfo_string55:
	.asciz	"total_in_lo32"
.Linfo_string56:
	.asciz	"total_in_hi32"
.Linfo_string57:
	.asciz	"next_out"
.Linfo_string58:
	.asciz	"avail_out"
.Linfo_string59:
	.asciz	"total_out_lo32"
.Linfo_string60:
	.asciz	"total_out_hi32"
.Linfo_string61:
	.asciz	"state"
.Linfo_string62:
	.asciz	"bzalloc"
.Linfo_string63:
	.asciz	"bzfree"
.Linfo_string64:
	.asciz	"opaque"
.Linfo_string65:
	.asciz	"bz_stream"
.Linfo_string66:
	.asciz	"blockSize100k"
.Linfo_string67:
	.asciz	"verbosity"
.Linfo_string68:
	.asciz	"workFactor"
.Linfo_string69:
	.asciz	"n"
.Linfo_string70:
	.asciz	"s"
.Linfo_string71:
	.asciz	"mode"
.Linfo_string72:
	.asciz	"avail_in_expect"
.Linfo_string73:
	.asciz	"UInt32"
.Linfo_string74:
	.asciz	"arr1"
.Linfo_string75:
	.asciz	"arr2"
.Linfo_string76:
	.asciz	"ftab"
.Linfo_string77:
	.asciz	"origPtr"
.Linfo_string78:
	.asciz	"ptr"
.Linfo_string79:
	.asciz	"block"
.Linfo_string80:
	.asciz	"UChar"
.Linfo_string81:
	.asciz	"mtfv"
.Linfo_string82:
	.asciz	"unsigned short"
.Linfo_string83:
	.asciz	"UInt16"
.Linfo_string84:
	.asciz	"zbits"
.Linfo_string85:
	.asciz	"state_in_ch"
.Linfo_string86:
	.asciz	"state_in_len"
.Linfo_string87:
	.asciz	"rNToGo"
.Linfo_string88:
	.asciz	"rTPos"
.Linfo_string89:
	.asciz	"nblock"
.Linfo_string90:
	.asciz	"nblockMAX"
.Linfo_string91:
	.asciz	"numZ"
.Linfo_string92:
	.asciz	"state_out_pos"
.Linfo_string93:
	.asciz	"nInUse"
.Linfo_string94:
	.asciz	"inUse"
.Linfo_string95:
	.asciz	"unseqToSeq"
.Linfo_string96:
	.asciz	"bsBuff"
.Linfo_string97:
	.asciz	"bsLive"
.Linfo_string98:
	.asciz	"blockCRC"
.Linfo_string99:
	.asciz	"combinedCRC"
.Linfo_string100:
	.asciz	"blockNo"
.Linfo_string101:
	.asciz	"nMTF"
.Linfo_string102:
	.asciz	"mtfFreq"
.Linfo_string103:
	.asciz	"selector"
.Linfo_string104:
	.asciz	"selectorMtf"
.Linfo_string105:
	.asciz	"len"
.Linfo_string106:
	.asciz	"code"
.Linfo_string107:
	.asciz	"rfreq"
.Linfo_string108:
	.asciz	"len_pack"
.Linfo_string109:
	.asciz	"EState"
.Linfo_string110:
	.asciz	"items"
.Linfo_string111:
	.asciz	"size"
.Linfo_string112:
	.asciz	"v"
.Linfo_string113:
	.asciz	"addr"
.Linfo_string114:
	.asciz	"i"
.Linfo_string115:
	.asciz	"action"
.Linfo_string116:
	.asciz	"progress"
.Linfo_string117:
	.asciz	"progress_in"
.Linfo_string118:
	.asciz	"progress_out"
.Linfo_string119:
	.asciz	"small"
.Linfo_string120:
	.asciz	"state_out_ch"
.Linfo_string121:
	.asciz	"state_out_len"
.Linfo_string122:
	.asciz	"blockRandomised"
.Linfo_string123:
	.asciz	"smallDecompress"
.Linfo_string124:
	.asciz	"currBlockNo"
.Linfo_string125:
	.asciz	"tPos"
.Linfo_string126:
	.asciz	"k0"
.Linfo_string127:
	.asciz	"unzftab"
.Linfo_string128:
	.asciz	"nblock_used"
.Linfo_string129:
	.asciz	"cftab"
.Linfo_string130:
	.asciz	"cftabCopy"
.Linfo_string131:
	.asciz	"tt"
.Linfo_string132:
	.asciz	"ll16"
.Linfo_string133:
	.asciz	"ll4"
.Linfo_string134:
	.asciz	"storedBlockCRC"
.Linfo_string135:
	.asciz	"storedCombinedCRC"
.Linfo_string136:
	.asciz	"calculatedBlockCRC"
.Linfo_string137:
	.asciz	"calculatedCombinedCRC"
.Linfo_string138:
	.asciz	"inUse16"
.Linfo_string139:
	.asciz	"seqToUnseq"
.Linfo_string140:
	.asciz	"mtfa"
.Linfo_string141:
	.asciz	"mtfbase"
.Linfo_string142:
	.asciz	"limit"
.Linfo_string143:
	.asciz	"base"
.Linfo_string144:
	.asciz	"perm"
.Linfo_string145:
	.asciz	"minLens"
.Linfo_string146:
	.asciz	"save_i"
.Linfo_string147:
	.asciz	"save_j"
.Linfo_string148:
	.asciz	"save_t"
.Linfo_string149:
	.asciz	"save_alphaSize"
.Linfo_string150:
	.asciz	"save_nGroups"
.Linfo_string151:
	.asciz	"save_nSelectors"
.Linfo_string152:
	.asciz	"save_EOB"
.Linfo_string153:
	.asciz	"save_groupNo"
.Linfo_string154:
	.asciz	"save_groupPos"
.Linfo_string155:
	.asciz	"save_nextSym"
.Linfo_string156:
	.asciz	"save_nblockMAX"
.Linfo_string157:
	.asciz	"save_nblock"
.Linfo_string158:
	.asciz	"save_es"
.Linfo_string159:
	.asciz	"save_N"
.Linfo_string160:
	.asciz	"save_curr"
.Linfo_string161:
	.asciz	"save_zt"
.Linfo_string162:
	.asciz	"save_zn"
.Linfo_string163:
	.asciz	"save_zvec"
.Linfo_string164:
	.asciz	"save_zj"
.Linfo_string165:
	.asciz	"save_gSel"
.Linfo_string166:
	.asciz	"save_gMinlen"
.Linfo_string167:
	.asciz	"save_gLimit"
.Linfo_string168:
	.asciz	"save_gBase"
.Linfo_string169:
	.asciz	"save_gPerm"
.Linfo_string170:
	.asciz	"DState"
.Linfo_string171:
	.asciz	"indx"
.Linfo_string172:
	.asciz	"nb"
.Linfo_string173:
	.asciz	"na"
.Linfo_string174:
	.asciz	"mid"
.Linfo_string175:
	.asciz	"corrupt"
.Linfo_string176:
	.asciz	"r"
.Linfo_string177:
	.asciz	"k1"
.Linfo_string178:
	.asciz	"c_calculatedBlockCRC"
.Linfo_string179:
	.asciz	"c_state_out_ch"
.Linfo_string180:
	.asciz	"c_state_out_len"
.Linfo_string181:
	.asciz	"c_nblock_used"
.Linfo_string182:
	.asciz	"c_k0"
.Linfo_string183:
	.asciz	"c_tt"
.Linfo_string184:
	.asciz	"c_tPos"
.Linfo_string185:
	.asciz	"cs_next_out"
.Linfo_string186:
	.asciz	"cs_avail_out"
.Linfo_string187:
	.asciz	"avail_out_INIT"
.Linfo_string188:
	.asciz	"s_save_nblockPP"
.Linfo_string189:
	.asciz	"total_out_lo32_old"
.Linfo_string190:
	.asciz	"bzerror"
.Linfo_string191:
	.asciz	"f"
.Linfo_string192:
	.asciz	"_IO_FILE"
.Linfo_string193:
	.asciz	"FILE"
.Linfo_string194:
	.asciz	"ret"
.Linfo_string195:
	.asciz	"bzf"
.Linfo_string196:
	.asciz	"handle"
.Linfo_string197:
	.asciz	"buf"
.Linfo_string198:
	.asciz	"Char"
.Linfo_string199:
	.asciz	"bufN"
.Linfo_string200:
	.asciz	"writing"
.Linfo_string201:
	.asciz	"lastErr"
.Linfo_string202:
	.asciz	"initialisedOk"
.Linfo_string203:
	.asciz	"bzFile"
.Linfo_string204:
	.asciz	"b"
.Linfo_string205:
	.asciz	"n2"
.Linfo_string206:
	.asciz	"abandon"
.Linfo_string207:
	.asciz	"nbytes_in"
.Linfo_string208:
	.asciz	"nbytes_out"
.Linfo_string209:
	.asciz	"nbytes_in_lo32"
.Linfo_string210:
	.asciz	"nbytes_in_hi32"
.Linfo_string211:
	.asciz	"nbytes_out_lo32"
.Linfo_string212:
	.asciz	"nbytes_out_hi32"
.Linfo_string213:
	.asciz	"unused"
.Linfo_string214:
	.asciz	"nUnused"
.Linfo_string215:
	.asciz	"c"
.Linfo_string216:
	.asciz	"dest"
.Linfo_string217:
	.asciz	"destLen"
.Linfo_string218:
	.asciz	"source"
.Linfo_string219:
	.asciz	"sourceLen"
.Linfo_string220:
	.asciz	"path"
.Linfo_string221:
	.asciz	"fd"
.Linfo_string222:
	.asciz	"open_mode"
.Linfo_string223:
	.asciz	"bzerr"
.Linfo_string224:
	.asciz	"mode2"
.Linfo_string225:
	.asciz	"fp"
.Linfo_string226:
	.asciz	"bzfp"
.Linfo_string227:
	.asciz	"smallMode"
.Linfo_string228:
	.asciz	"nread"
.Linfo_string229:
	.asciz	"errnum"
.Linfo_string230:
	.asciz	"err"
.Linfo_string231:
	.asciz	"zchh"
.Linfo_string232:
	.asciz	"ch"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	6481                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x194a DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x16 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	60                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	1637                    # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	bzerrorstrings
	.byte	3                       # Abbrev [3] 0x3c:0xc DW_TAG_array_type
	.long	72                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x41:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x48:0x5 DW_TAG_pointer_type
	.long	77                      # DW_AT_type
	.byte	6                       # Abbrev [6] 0x4d:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	7                       # Abbrev [7] 0x54:0x7 DW_TAG_base_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	8                       # Abbrev [8] 0x5b:0x29 DW_TAG_subprogram
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x75:0xe DW_TAG_formal_parameter
	.long	.Linfo_string50         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	10                      # Abbrev [10] 0x84:0x73 DW_TAG_subprogram
	.long	.Linfo_string7          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0xa2:0xe DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	9                       # Abbrev [9] 0xb0:0xe DW_TAG_formal_parameter
	.long	.Linfo_string66         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	9                       # Abbrev [9] 0xbe:0xe DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	208                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	9                       # Abbrev [9] 0xcc:0xe DW_TAG_formal_parameter
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	209                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	11                      # Abbrev [11] 0xda:0xe DW_TAG_variable
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	11                      # Abbrev [11] 0xe8:0xe DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0xf7:0x7 DW_TAG_base_type
	.long	.Linfo_string5          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	12                      # Abbrev [12] 0xfe:0x5c DW_TAG_subprogram
	.long	.Linfo_string8          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	464                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.quad	.Lfunc_end8             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x11d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	464                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x12c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string115        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	464                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	14                      # Abbrev [14] 0x13b:0xf DW_TAG_variable
	.long	.Linfo_string116        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	466                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	99
	.byte	14                      # Abbrev [14] 0x14a:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	467                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x15a:0x3e DW_TAG_subprogram
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	525                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.quad	.Lfunc_end11            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x179:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	525                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x188:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	527                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x198:0x5c DW_TAG_subprogram
	.long	.Linfo_string10         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	549                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.quad	.Lfunc_end12            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x1b7:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	550                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	13                      # Abbrev [13] 0x1c6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	551                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	13                      # Abbrev [13] 0x1d5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	552                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	14                      # Abbrev [14] 0x1e4:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	554                     # DW_AT_decl_line
	.long	5225                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x1f4:0x6b DW_TAG_subprogram
	.long	.Linfo_string11         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	607                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin13          # DW_AT_low_pc
	.quad	.Lfunc_end13            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x213:0xf DW_TAG_formal_parameter
	.long	.Linfo_string171        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	13                      # Abbrev [13] 0x222:0xf DW_TAG_formal_parameter
	.long	.Linfo_string129        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	743                     # DW_AT_decl_line
	.long	6266                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x231:0xf DW_TAG_variable
	.long	.Linfo_string172        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	745                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	14                      # Abbrev [14] 0x240:0xf DW_TAG_variable
	.long	.Linfo_string173        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	745                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	14                      # Abbrev [14] 0x24f:0xf DW_TAG_variable
	.long	.Linfo_string174        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	745                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x25f:0xb DW_TAG_typedef
	.long	247                     # DW_AT_type
	.long	.Linfo_string12         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	84                      # DW_AT_decl_line
	.byte	12                      # Abbrev [12] 0x26a:0x6f DW_TAG_subprogram
	.long	.Linfo_string13         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	864                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin14          # DW_AT_low_pc
	.quad	.Lfunc_end14            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x289:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	864                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	14                      # Abbrev [14] 0x298:0xf DW_TAG_variable
	.long	.Linfo_string175        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	866                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	79
	.byte	14                      # Abbrev [14] 0x2a7:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	867                     # DW_AT_decl_line
	.long	5225                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	16                      # Abbrev [16] 0x2b6:0x22 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	16                      # Abbrev [16] 0x2bb:0x1c DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	16                      # Abbrev [16] 0x2c0:0x16 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	14                      # Abbrev [14] 0x2c5:0x10 DW_TAG_variable
	.long	.Linfo_string176        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	898                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x2d9:0x3e DW_TAG_subprogram
	.long	.Linfo_string14         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	918                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin17          # DW_AT_low_pc
	.quad	.Lfunc_end17            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x2f8:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	918                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x307:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	920                     # DW_AT_decl_line
	.long	5225                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x317:0x89 DW_TAG_subprogram
	.long	.Linfo_string15         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	980                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin18          # DW_AT_low_pc
	.quad	.Lfunc_end18            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x336:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	981                     # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x345:0xf DW_TAG_formal_parameter
	.long	.Linfo_string191        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	985                     # DW_AT_decl_line
	.long	6276                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x354:0xf DW_TAG_formal_parameter
	.long	.Linfo_string66         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	987                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	13                      # Abbrev [13] 0x363:0xf DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	988                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x372:0xf DW_TAG_formal_parameter
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	989                     # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	14                      # Abbrev [14] 0x381:0xf DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	991                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	14                      # Abbrev [14] 0x390:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	992                     # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	0                       # End Of Children Mark
	.byte	17                      # Abbrev [17] 0x3a0:0x1 DW_TAG_pointer_type
	.byte	18                      # Abbrev [18] 0x3a1:0x94 DW_TAG_subprogram
	.long	.Linfo_string16         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1032                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin19          # DW_AT_low_pc
	.quad	.Lfunc_end19            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x3bc:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1033                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x3cb:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1034                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x3da:0xf DW_TAG_formal_parameter
	.long	.Linfo_string197        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1035                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x3e9:0xf DW_TAG_formal_parameter
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1036                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	14                      # Abbrev [14] 0x3f8:0xf DW_TAG_variable
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1038                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	14                      # Abbrev [14] 0x407:0xf DW_TAG_variable
	.long	.Linfo_string205        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1038                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	14                      # Abbrev [14] 0x416:0xf DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1038                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	14                      # Abbrev [14] 0x425:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1039                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x435:0x67 DW_TAG_subprogram
	.long	.Linfo_string17         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1077                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin20          # DW_AT_low_pc
	.quad	.Lfunc_end20            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x450:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1078                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	13                      # Abbrev [13] 0x45f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1079                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	13                      # Abbrev [13] 0x46e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string206        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1080                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	13                      # Abbrev [13] 0x47d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string207        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1081                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x48c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string208        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1082                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x49c:0xc6 DW_TAG_subprogram
	.long	.Linfo_string18         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1089                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin21          # DW_AT_low_pc
	.quad	.Lfunc_end21            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x4b7:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1090                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x4c6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1091                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x4d5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string206        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1092                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	13                      # Abbrev [13] 0x4e4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string209        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1093                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	13                      # Abbrev [13] 0x4f3:0xf DW_TAG_formal_parameter
	.long	.Linfo_string210        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1094                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	13                      # Abbrev [13] 0x502:0xf DW_TAG_formal_parameter
	.long	.Linfo_string211        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1095                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	13                      # Abbrev [13] 0x511:0x10 DW_TAG_formal_parameter
	.long	.Linfo_string212        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1096                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	14                      # Abbrev [14] 0x521:0x10 DW_TAG_variable
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1098                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	14                      # Abbrev [14] 0x531:0x10 DW_TAG_variable
	.long	.Linfo_string205        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1098                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	14                      # Abbrev [14] 0x541:0x10 DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1098                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	14                      # Abbrev [14] 0x551:0x10 DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1099                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240\177"
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x562:0x99 DW_TAG_subprogram
	.long	.Linfo_string19         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1155                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin22          # DW_AT_low_pc
	.quad	.Lfunc_end22            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x581:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1156                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x590:0xf DW_TAG_formal_parameter
	.long	.Linfo_string191        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1160                    # DW_AT_decl_line
	.long	6276                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x59f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1162                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	13                      # Abbrev [13] 0x5ae:0xf DW_TAG_formal_parameter
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1163                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x5bd:0xf DW_TAG_formal_parameter
	.long	.Linfo_string213        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1164                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	13                      # Abbrev [13] 0x5cc:0xf DW_TAG_formal_parameter
	.long	.Linfo_string214        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1165                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	14                      # Abbrev [14] 0x5db:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1167                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	14                      # Abbrev [14] 0x5ea:0x10 DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1168                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274\177"
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x5fb:0x49 DW_TAG_subprogram
	.long	.Linfo_string20         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1215                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin23          # DW_AT_low_pc
	.quad	.Lfunc_end23            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x616:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1215                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	13                      # Abbrev [13] 0x625:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1215                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x634:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1217                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x644:0x89 DW_TAG_subprogram
	.long	.Linfo_string21         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1233                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin24          # DW_AT_low_pc
	.quad	.Lfunc_end24            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x663:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1234                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x672:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1235                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x681:0xf DW_TAG_formal_parameter
	.long	.Linfo_string197        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1236                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x690:0xf DW_TAG_formal_parameter
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1237                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	14                      # Abbrev [14] 0x69f:0xf DW_TAG_variable
	.long	.Linfo_string69         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1239                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	14                      # Abbrev [14] 0x6ae:0xf DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1239                    # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	14                      # Abbrev [14] 0x6bd:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1240                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	64
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0x6cd:0x67 DW_TAG_subprogram
	.long	.Linfo_string22         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1293                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin26          # DW_AT_low_pc
	.quad	.Lfunc_end26            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x6e8:0xf DW_TAG_formal_parameter
	.long	.Linfo_string190        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1294                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	13                      # Abbrev [13] 0x6f7:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1295                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	13                      # Abbrev [13] 0x706:0xf DW_TAG_formal_parameter
	.long	.Linfo_string213        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1296                    # DW_AT_decl_line
	.long	6454                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x715:0xf DW_TAG_formal_parameter
	.long	.Linfo_string214        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1297                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	14                      # Abbrev [14] 0x724:0xf DW_TAG_variable
	.long	.Linfo_string195        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1299                    # DW_AT_decl_line
	.long	6298                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x734:0xa9 DW_TAG_subprogram
	.long	.Linfo_string23         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1319                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin27          # DW_AT_low_pc
	.quad	.Lfunc_end27            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x753:0xf DW_TAG_formal_parameter
	.long	.Linfo_string216        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1320                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x762:0xf DW_TAG_formal_parameter
	.long	.Linfo_string217        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1321                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x771:0xf DW_TAG_formal_parameter
	.long	.Linfo_string218        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1322                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x780:0xf DW_TAG_formal_parameter
	.long	.Linfo_string219        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1323                    # DW_AT_decl_line
	.long	4440                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	13                      # Abbrev [13] 0x78f:0xf DW_TAG_formal_parameter
	.long	.Linfo_string66         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1324                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	13                      # Abbrev [13] 0x79e:0xf DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1325                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	13                      # Abbrev [13] 0x7ad:0xf DW_TAG_formal_parameter
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1326                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	14                      # Abbrev [14] 0x7bc:0x10 DW_TAG_variable
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1328                    # DW_AT_decl_line
	.long	4268                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370~"
	.byte	14                      # Abbrev [14] 0x7cc:0x10 DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1329                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364~"
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x7dd:0x9a DW_TAG_subprogram
	.long	.Linfo_string24         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1371                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin28          # DW_AT_low_pc
	.quad	.Lfunc_end28            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x7fc:0xf DW_TAG_formal_parameter
	.long	.Linfo_string216        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1372                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x80b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string217        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1373                    # DW_AT_decl_line
	.long	6449                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0x81a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string218        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1374                    # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	13                      # Abbrev [13] 0x829:0xf DW_TAG_formal_parameter
	.long	.Linfo_string219        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1375                    # DW_AT_decl_line
	.long	4440                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	13                      # Abbrev [13] 0x838:0xf DW_TAG_formal_parameter
	.long	.Linfo_string119        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1376                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	13                      # Abbrev [13] 0x847:0xf DW_TAG_formal_parameter
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1377                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	14                      # Abbrev [14] 0x856:0x10 DW_TAG_variable
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1379                    # DW_AT_decl_line
	.long	4268                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\370~"
	.byte	14                      # Abbrev [14] 0x866:0x10 DW_TAG_variable
	.long	.Linfo_string194        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1380                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\364~"
	.byte	0                       # End Of Children Mark
	.byte	19                      # Abbrev [19] 0x877:0x1f DW_TAG_subprogram
	.long	.Linfo_string25         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1439                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2198                    # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	5                       # Abbrev [5] 0x896:0x5 DW_TAG_pointer_type
	.long	2203                    # DW_AT_type
	.byte	20                      # Abbrev [20] 0x89b:0x5 DW_TAG_const_type
	.long	77                      # DW_AT_type
	.byte	12                      # Abbrev [12] 0x8a0:0x3e DW_TAG_subprogram
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1551                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin29          # DW_AT_low_pc
	.quad	.Lfunc_end29            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x8bf:0xf DW_TAG_formal_parameter
	.long	.Linfo_string220        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1552                    # DW_AT_decl_line
	.long	2198                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	13                      # Abbrev [13] 0x8ce:0xf DW_TAG_formal_parameter
	.long	.Linfo_string71         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1553                    # DW_AT_decl_line
	.long	2198                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x8de:0x3e DW_TAG_subprogram
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1560                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin31          # DW_AT_low_pc
	.quad	.Lfunc_end31            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x8fd:0xf DW_TAG_formal_parameter
	.long	.Linfo_string221        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1561                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	13                      # Abbrev [13] 0x90c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string71         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1562                    # DW_AT_decl_line
	.long	2198                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x91c:0x6b DW_TAG_subprogram
	.long	.Linfo_string28         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1569                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin32          # DW_AT_low_pc
	.quad	.Lfunc_end32            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x93b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1569                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	13                      # Abbrev [13] 0x94a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string197        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1569                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x959:0xf DW_TAG_formal_parameter
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1569                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	14                      # Abbrev [14] 0x968:0xf DW_TAG_variable
	.long	.Linfo_string223        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1571                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	14                      # Abbrev [14] 0x977:0xf DW_TAG_variable
	.long	.Linfo_string228        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1571                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x987:0x5c DW_TAG_subprogram
	.long	.Linfo_string29         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1583                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin33          # DW_AT_low_pc
	.quad	.Lfunc_end33            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0x9a6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1583                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	13                      # Abbrev [13] 0x9b5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string197        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1583                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	13                      # Abbrev [13] 0x9c4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string105        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1583                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	14                      # Abbrev [14] 0x9d3:0xf DW_TAG_variable
	.long	.Linfo_string223        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1585                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0x9e3:0x2f DW_TAG_subprogram
	.long	.Linfo_string30         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1597                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin34          # DW_AT_low_pc
	.quad	.Lfunc_end34            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xa02:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1597                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	18                      # Abbrev [18] 0xa12:0x49 DW_TAG_subprogram
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1605                    # DW_AT_decl_line
                                        # DW_AT_prototyped
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin35          # DW_AT_low_pc
	.quad	.Lfunc_end35            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xa2d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1605                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	14                      # Abbrev [14] 0xa3c:0xf DW_TAG_variable
	.long	.Linfo_string223        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1607                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	14                      # Abbrev [14] 0xa4b:0xf DW_TAG_variable
	.long	.Linfo_string225        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1611                    # DW_AT_decl_line
	.long	6276                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	12                      # Abbrev [12] 0xa5b:0x4d DW_TAG_subprogram
	.long	.Linfo_string32         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1657                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	2198                    # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin36          # DW_AT_low_pc
	.quad	.Lfunc_end36            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xa7a:0xf DW_TAG_formal_parameter
	.long	.Linfo_string204        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1657                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	13                      # Abbrev [13] 0xa89:0xf DW_TAG_formal_parameter
	.long	.Linfo_string229        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1657                    # DW_AT_decl_line
	.long	6271                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0xa98:0xf DW_TAG_variable
	.long	.Linfo_string230        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1659                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xaa8:0x10b DW_TAG_subprogram
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1458                    # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin30          # DW_AT_low_pc
	.quad	.Lfunc_end30            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xac7:0xf DW_TAG_formal_parameter
	.long	.Linfo_string220        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1459                    # DW_AT_decl_line
	.long	2198                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	13                      # Abbrev [13] 0xad6:0xf DW_TAG_formal_parameter
	.long	.Linfo_string221        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1460                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	13                      # Abbrev [13] 0xae5:0xf DW_TAG_formal_parameter
	.long	.Linfo_string71         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1461                    # DW_AT_decl_line
	.long	2198                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	13                      # Abbrev [13] 0xaf4:0xf DW_TAG_formal_parameter
	.long	.Linfo_string222        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1462                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	14                      # Abbrev [14] 0xb03:0xf DW_TAG_variable
	.long	.Linfo_string223        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1464                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	14                      # Abbrev [14] 0xb12:0x10 DW_TAG_variable
	.long	.Linfo_string213        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1465                    # DW_AT_decl_line
	.long	6459                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\300X"
	.byte	14                      # Abbrev [14] 0xb22:0x10 DW_TAG_variable
	.long	.Linfo_string66         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1466                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\274X"
	.byte	14                      # Abbrev [14] 0xb32:0x10 DW_TAG_variable
	.long	.Linfo_string200        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1467                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270X"
	.byte	14                      # Abbrev [14] 0xb42:0x10 DW_TAG_variable
	.long	.Linfo_string224        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1468                    # DW_AT_decl_line
	.long	6472                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250X"
	.byte	14                      # Abbrev [14] 0xb52:0x10 DW_TAG_variable
	.long	.Linfo_string225        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1472                    # DW_AT_decl_line
	.long	6276                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\240X"
	.byte	14                      # Abbrev [14] 0xb62:0x10 DW_TAG_variable
	.long	.Linfo_string226        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1474                    # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\230X"
	.byte	14                      # Abbrev [14] 0xb72:0x10 DW_TAG_variable
	.long	.Linfo_string67         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1475                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\224X"
	.byte	14                      # Abbrev [14] 0xb82:0x10 DW_TAG_variable
	.long	.Linfo_string68         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1476                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\220X"
	.byte	14                      # Abbrev [14] 0xb92:0x10 DW_TAG_variable
	.long	.Linfo_string227        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1477                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\214X"
	.byte	14                      # Abbrev [14] 0xba2:0x10 DW_TAG_variable
	.long	.Linfo_string214        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	1478                    # DW_AT_decl_line
	.long	247                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\210X"
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xbb3:0x3e DW_TAG_subprogram
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	969                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin25          # DW_AT_low_pc
	.quad	.Lfunc_end25            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xbd2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string191        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	969                     # DW_AT_decl_line
	.long	6276                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0xbe1:0xf DW_TAG_variable
	.long	.Linfo_string215        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	972                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0xbf1:0xb DW_TAG_typedef
	.long	3068                    # DW_AT_type
	.long	.Linfo_string36         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	82                      # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0xbfc:0x7 DW_TAG_base_type
	.long	.Linfo_string35         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	21                      # Abbrev [21] 0xc03:0x11b DW_TAG_subprogram
	.long	.Linfo_string37         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	593                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin16          # DW_AT_low_pc
	.quad	.Lfunc_end16            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xc22:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	593                     # DW_AT_decl_line
	.long	5225                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	14                      # Abbrev [14] 0xc31:0xf DW_TAG_variable
	.long	.Linfo_string177        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	595                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	103
	.byte	22                      # Abbrev [22] 0xc40:0xdd DW_TAG_lexical_block
	.quad	.Ltmp337                # DW_AT_low_pc
	.quad	.Ltmp426                # DW_AT_high_pc
	.byte	22                      # Abbrev [22] 0xc51:0xcb DW_TAG_lexical_block
	.quad	.Ltmp378                # DW_AT_low_pc
	.quad	.Ltmp426                # DW_AT_high_pc
	.byte	14                      # Abbrev [14] 0xc62:0xf DW_TAG_variable
	.long	.Linfo_string178        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	649                     # DW_AT_decl_line
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	14                      # Abbrev [14] 0xc71:0xf DW_TAG_variable
	.long	.Linfo_string179        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	650                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	95
	.byte	14                      # Abbrev [14] 0xc80:0xf DW_TAG_variable
	.long	.Linfo_string180        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	651                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	14                      # Abbrev [14] 0xc8f:0xf DW_TAG_variable
	.long	.Linfo_string181        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	652                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	14                      # Abbrev [14] 0xc9e:0xf DW_TAG_variable
	.long	.Linfo_string182        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	653                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	14                      # Abbrev [14] 0xcad:0xf DW_TAG_variable
	.long	.Linfo_string183        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	654                     # DW_AT_decl_line
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	14                      # Abbrev [14] 0xcbc:0xf DW_TAG_variable
	.long	.Linfo_string184        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	655                     # DW_AT_decl_line
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	68
	.byte	14                      # Abbrev [14] 0xccb:0x10 DW_TAG_variable
	.long	.Linfo_string185        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	656                     # DW_AT_decl_line
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\270\177"
	.byte	14                      # Abbrev [14] 0xcdb:0x10 DW_TAG_variable
	.long	.Linfo_string186        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	657                     # DW_AT_decl_line
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\264\177"
	.byte	14                      # Abbrev [14] 0xceb:0x10 DW_TAG_variable
	.long	.Linfo_string187        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	660                     # DW_AT_decl_line
	.long	5063                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\260\177"
	.byte	14                      # Abbrev [14] 0xcfb:0x10 DW_TAG_variable
	.long	.Linfo_string188        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	661                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\254\177"
	.byte	14                      # Abbrev [14] 0xd0b:0x10 DW_TAG_variable
	.long	.Linfo_string189        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	662                     # DW_AT_decl_line
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_location
	.byte	145
	.ascii	"\250\177"
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xd1e:0x3e DW_TAG_subprogram
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	762                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin15          # DW_AT_low_pc
	.quad	.Lfunc_end15            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xd3d:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	762                     # DW_AT_decl_line
	.long	5225                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	14                      # Abbrev [14] 0xd4c:0xf DW_TAG_variable
	.long	.Linfo_string177        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	764                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	71
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0xd5c:0x2d DW_TAG_subprogram
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	196                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.quad	.Lfunc_end10            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0xd7a:0xe DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	196                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xd89:0x5c DW_TAG_subprogram
	.long	.Linfo_string40         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	418                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.quad	.Lfunc_end9             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xda8:0xf DW_TAG_formal_parameter
	.long	.Linfo_string51         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	418                     # DW_AT_decl_line
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	14                      # Abbrev [14] 0xdb7:0xf DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	420                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	87
	.byte	14                      # Abbrev [14] 0xdc6:0xf DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	421                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	86
	.byte	14                      # Abbrev [14] 0xdd5:0xf DW_TAG_variable
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	422                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	72
	.byte	0                       # End Of Children Mark
	.byte	24                      # Abbrev [24] 0xde5:0x2b DW_TAG_subprogram
	.long	.Linfo_string41         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	309                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin39          # DW_AT_low_pc
	.quad	.Lfunc_end39            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xe00:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	309                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	24                      # Abbrev [24] 0xe10:0x49 DW_TAG_subprogram
	.long	.Linfo_string42         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin40          # DW_AT_low_pc
	.quad	.Lfunc_end40            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xe2b:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	14                      # Abbrev [14] 0xe3a:0xf DW_TAG_variable
	.long	.Linfo_string114        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	14                      # Abbrev [14] 0xe49:0xf DW_TAG_variable
	.long	.Linfo_string232        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	115
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xe59:0x104 DW_TAG_subprogram
	.long	.Linfo_string43         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	346                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin38          # DW_AT_low_pc
	.quad	.Lfunc_end38            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xe78:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	346                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0xe87:0xf DW_TAG_variable
	.long	.Linfo_string117        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	348                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	111
	.byte	16                      # Abbrev [16] 0xe96:0xc6 DW_TAG_lexical_block
	.long	.Ldebug_ranges7         # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0xe9b:0x60 DW_TAG_lexical_block
	.quad	.Ltmp885                # DW_AT_low_pc
	.quad	.Ltmp908                # DW_AT_high_pc
	.byte	22                      # Abbrev [22] 0xeac:0x4e DW_TAG_lexical_block
	.quad	.Ltmp885                # DW_AT_low_pc
	.quad	.Ltmp908                # DW_AT_high_pc
	.byte	16                      # Abbrev [16] 0xebd:0x3c DW_TAG_lexical_block
	.long	.Ldebug_ranges4         # DW_AT_ranges
	.byte	14                      # Abbrev [14] 0xec2:0xf DW_TAG_variable
	.long	.Linfo_string231        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	377                     # DW_AT_decl_line
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	16                      # Abbrev [16] 0xed1:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges3         # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0xed6:0x21 DW_TAG_lexical_block
	.quad	.Ltmp897                # DW_AT_low_pc
	.quad	.Ltmp902                # DW_AT_high_pc
	.byte	14                      # Abbrev [14] 0xee7:0xf DW_TAG_variable
	.long	.Linfo_string232        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	377                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	95
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	22                      # Abbrev [22] 0xefb:0x60 DW_TAG_lexical_block
	.quad	.Ltmp908                # DW_AT_low_pc
	.quad	.Ltmp930                # DW_AT_high_pc
	.byte	22                      # Abbrev [22] 0xf0c:0x4e DW_TAG_lexical_block
	.quad	.Ltmp908                # DW_AT_low_pc
	.quad	.Ltmp930                # DW_AT_high_pc
	.byte	16                      # Abbrev [16] 0xf1d:0x3c DW_TAG_lexical_block
	.long	.Ldebug_ranges6         # DW_AT_ranges
	.byte	14                      # Abbrev [14] 0xf22:0xf DW_TAG_variable
	.long	.Linfo_string231        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	359                     # DW_AT_decl_line
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	16                      # Abbrev [16] 0xf31:0x27 DW_TAG_lexical_block
	.long	.Ldebug_ranges5         # DW_AT_ranges
	.byte	22                      # Abbrev [22] 0xf36:0x21 DW_TAG_lexical_block
	.quad	.Ltmp919                # DW_AT_low_pc
	.quad	.Ltmp924                # DW_AT_high_pc
	.byte	14                      # Abbrev [14] 0xf47:0xf DW_TAG_variable
	.long	.Linfo_string232        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	359                     # DW_AT_decl_line
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	103
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	21                      # Abbrev [21] 0xf5d:0x3e DW_TAG_subprogram
	.long	.Linfo_string44         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	391                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin37          # DW_AT_low_pc
	.quad	.Lfunc_end37            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	13                      # Abbrev [13] 0xf7c:0xf DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	391                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	14                      # Abbrev [14] 0xf8b:0xf DW_TAG_variable
	.long	.Linfo_string118        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	393                     # DW_AT_decl_line
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	119
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xf9b:0x37 DW_TAG_subprogram
	.long	.Linfo_string45         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	174                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.quad	.Lfunc_end7             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0xfb5:0xe DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	174                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0xfc3:0xe DW_TAG_variable
	.long	.Linfo_string114        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	176                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xfd2:0x29 DW_TAG_subprogram
	.long	.Linfo_string46         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	188                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.quad	.Lfunc_end6             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0xfec:0xe DW_TAG_formal_parameter
	.long	.Linfo_string70         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	188                     # DW_AT_decl_line
	.long	4490                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	25                      # Abbrev [25] 0xffb:0x37 DW_TAG_subprogram
	.long	.Linfo_string47         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	166                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.quad	.Lfunc_end5             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x1015:0xe DW_TAG_formal_parameter
	.long	.Linfo_string64         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	166                     # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	9                       # Abbrev [9] 0x1023:0xe DW_TAG_formal_parameter
	.long	.Linfo_string113        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	166                     # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	0                       # End Of Children Mark
	.byte	23                      # Abbrev [23] 0x1032:0x57 DW_TAG_subprogram
	.long	.Linfo_string48         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	159                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	928                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.quad	.Lfunc_end4             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	9                       # Abbrev [9] 0x1050:0xe DW_TAG_formal_parameter
	.long	.Linfo_string64         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	159                     # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	9                       # Abbrev [9] 0x105e:0xe DW_TAG_formal_parameter
	.long	.Linfo_string110        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	159                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	9                       # Abbrev [9] 0x106c:0xe DW_TAG_formal_parameter
	.long	.Linfo_string111        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	159                     # DW_AT_decl_line
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	11                      # Abbrev [11] 0x107a:0xe DW_TAG_variable
	.long	.Linfo_string112        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	161                     # DW_AT_decl_line
	.long	928                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	26                      # Abbrev [26] 0x1089:0x1e DW_TAG_subprogram
	.long	.Linfo_string49         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	148                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	247                     # DW_AT_type
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.quad	.Lfunc_end3             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	5                       # Abbrev [5] 0x10a7:0x5 DW_TAG_pointer_type
	.long	4268                    # DW_AT_type
	.byte	15                      # Abbrev [15] 0x10ac:0xb DW_TAG_typedef
	.long	4279                    # DW_AT_type
	.long	.Linfo_string65         # DW_AT_name
	.byte	3                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.byte	27                      # Abbrev [27] 0x10b7:0xa1 DW_TAG_structure_type
	.byte	80                      # DW_AT_byte_size
	.byte	3                       # DW_AT_decl_file
	.byte	204                     # DW_AT_decl_line
	.byte	28                      # Abbrev [28] 0x10bb:0xd DW_TAG_member
	.long	.Linfo_string52         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	205                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x10c8:0xd DW_TAG_member
	.long	.Linfo_string53         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x10d5:0xd DW_TAG_member
	.long	.Linfo_string55         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x10e2:0xd DW_TAG_member
	.long	.Linfo_string56         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	208                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x10ef:0xd DW_TAG_member
	.long	.Linfo_string57         # DW_AT_name
	.long	72                      # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	210                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x10fc:0xd DW_TAG_member
	.long	.Linfo_string58         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	211                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1109:0xd DW_TAG_member
	.long	.Linfo_string59         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	212                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1116:0xd DW_TAG_member
	.long	.Linfo_string60         # DW_AT_name
	.long	4440                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	213                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1123:0xd DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	928                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	215                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1130:0xd DW_TAG_member
	.long	.Linfo_string62         # DW_AT_name
	.long	4447                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	217                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x113d:0xd DW_TAG_member
	.long	.Linfo_string63         # DW_AT_name
	.long	4473                    # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	218                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x114a:0xd DW_TAG_member
	.long	.Linfo_string64         # DW_AT_name
	.long	928                     # DW_AT_type
	.byte	3                       # DW_AT_decl_file
	.byte	219                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x1158:0x7 DW_TAG_base_type
	.long	.Linfo_string54         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # Abbrev [5] 0x115f:0x5 DW_TAG_pointer_type
	.long	4452                    # DW_AT_type
	.byte	29                      # Abbrev [29] 0x1164:0x15 DW_TAG_subroutine_type
	.long	928                     # DW_AT_type
                                        # DW_AT_prototyped
	.byte	30                      # Abbrev [30] 0x1169:0x5 DW_TAG_formal_parameter
	.long	928                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0x116e:0x5 DW_TAG_formal_parameter
	.long	247                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0x1173:0x5 DW_TAG_formal_parameter
	.long	247                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x1179:0x5 DW_TAG_pointer_type
	.long	4478                    # DW_AT_type
	.byte	31                      # Abbrev [31] 0x117e:0xc DW_TAG_subroutine_type
                                        # DW_AT_prototyped
	.byte	30                      # Abbrev [30] 0x117f:0x5 DW_TAG_formal_parameter
	.long	928                     # DW_AT_type
	.byte	30                      # Abbrev [30] 0x1184:0x5 DW_TAG_formal_parameter
	.long	928                     # DW_AT_type
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x118a:0x5 DW_TAG_pointer_type
	.long	4495                    # DW_AT_type
	.byte	32                      # Abbrev [32] 0x118f:0xc DW_TAG_typedef
	.long	4507                    # DW_AT_type
	.long	.Linfo_string109        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	300                     # DW_AT_decl_line
	.byte	33                      # Abbrev [33] 0x119b:0x22c DW_TAG_structure_type
	.short	55768                   # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.byte	231                     # DW_AT_decl_line
	.byte	28                      # Abbrev [28] 0x11a0:0xd DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	233                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11ad:0xd DW_TAG_member
	.long	.Linfo_string71         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	237                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11ba:0xd DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	238                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11c7:0xd DW_TAG_member
	.long	.Linfo_string72         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	241                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11d4:0xd DW_TAG_member
	.long	.Linfo_string74         # DW_AT_name
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	244                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11e1:0xd DW_TAG_member
	.long	.Linfo_string75         # DW_AT_name
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	245                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11ee:0xd DW_TAG_member
	.long	.Linfo_string76         # DW_AT_name
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	246                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x11fb:0xd DW_TAG_member
	.long	.Linfo_string77         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	247                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1208:0xd DW_TAG_member
	.long	.Linfo_string78         # DW_AT_name
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	250                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1215:0xd DW_TAG_member
	.long	.Linfo_string79         # DW_AT_name
	.long	5079                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	251                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x1222:0xd DW_TAG_member
	.long	.Linfo_string81         # DW_AT_name
	.long	5095                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	252                     # DW_AT_decl_line
	.byte	72                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	28                      # Abbrev [28] 0x122f:0xd DW_TAG_member
	.long	.Linfo_string84         # DW_AT_name
	.long	5079                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.byte	253                     # DW_AT_decl_line
	.byte	80                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x123c:0xe DW_TAG_member
	.long	.Linfo_string68         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	256                     # DW_AT_decl_line
	.byte	88                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x124a:0xe DW_TAG_member
	.long	.Linfo_string85         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	259                     # DW_AT_decl_line
	.byte	92                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1258:0xe DW_TAG_member
	.long	.Linfo_string86         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	260                     # DW_AT_decl_line
	.byte	96                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1266:0xe DW_TAG_member
	.long	.Linfo_string87         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	100                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1274:0xe DW_TAG_member
	.long	.Linfo_string88         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.byte	104                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1282:0xe DW_TAG_member
	.long	.Linfo_string89         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	264                     # DW_AT_decl_line
	.byte	108                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1290:0xe DW_TAG_member
	.long	.Linfo_string90         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	265                     # DW_AT_decl_line
	.byte	112                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x129e:0xe DW_TAG_member
	.long	.Linfo_string91         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	266                     # DW_AT_decl_line
	.byte	116                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x12ac:0xe DW_TAG_member
	.long	.Linfo_string92         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	267                     # DW_AT_decl_line
	.byte	120                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x12ba:0xe DW_TAG_member
	.long	.Linfo_string93         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	270                     # DW_AT_decl_line
	.byte	124                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x12c8:0xe DW_TAG_member
	.long	.Linfo_string94         # DW_AT_name
	.long	5118                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	271                     # DW_AT_decl_line
	.byte	128                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x12d6:0xf DW_TAG_member
	.long	.Linfo_string95         # DW_AT_name
	.long	5130                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	272                     # DW_AT_decl_line
	.short	384                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x12e5:0xf DW_TAG_member
	.long	.Linfo_string96         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	275                     # DW_AT_decl_line
	.short	640                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x12f4:0xf DW_TAG_member
	.long	.Linfo_string97         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	276                     # DW_AT_decl_line
	.short	644                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1303:0xf DW_TAG_member
	.long	.Linfo_string98         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	279                     # DW_AT_decl_line
	.short	648                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1312:0xf DW_TAG_member
	.long	.Linfo_string99         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	280                     # DW_AT_decl_line
	.short	652                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1321:0xf DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	283                     # DW_AT_decl_line
	.short	656                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1330:0xf DW_TAG_member
	.long	.Linfo_string100        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	284                     # DW_AT_decl_line
	.short	660                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x133f:0xf DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	285                     # DW_AT_decl_line
	.short	664                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x134e:0xf DW_TAG_member
	.long	.Linfo_string101        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	288                     # DW_AT_decl_line
	.short	668                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x135d:0xf DW_TAG_member
	.long	.Linfo_string102        # DW_AT_name
	.long	5142                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	289                     # DW_AT_decl_line
	.short	672                     # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x136c:0xf DW_TAG_member
	.long	.Linfo_string103        # DW_AT_name
	.long	5155                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	290                     # DW_AT_decl_line
	.short	1704                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x137b:0xf DW_TAG_member
	.long	.Linfo_string104        # DW_AT_name
	.long	5155                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	291                     # DW_AT_decl_line
	.short	19706                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x138a:0xf DW_TAG_member
	.long	.Linfo_string105        # DW_AT_name
	.long	5168                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	293                     # DW_AT_decl_line
	.short	37708                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1399:0xf DW_TAG_member
	.long	.Linfo_string106        # DW_AT_name
	.long	5187                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	294                     # DW_AT_decl_line
	.short	39256                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x13a8:0xf DW_TAG_member
	.long	.Linfo_string107        # DW_AT_name
	.long	5187                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	295                     # DW_AT_decl_line
	.short	45448                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x13b7:0xf DW_TAG_member
	.long	.Linfo_string108        # DW_AT_name
	.long	5206                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	297                     # DW_AT_decl_line
	.short	51640                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x13c7:0xb DW_TAG_typedef
	.long	4440                    # DW_AT_type
	.long	.Linfo_string73         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	85                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x13d2:0x5 DW_TAG_pointer_type
	.long	5063                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x13d7:0x5 DW_TAG_pointer_type
	.long	5084                    # DW_AT_type
	.byte	15                      # Abbrev [15] 0x13dc:0xb DW_TAG_typedef
	.long	3068                    # DW_AT_type
	.long	.Linfo_string80         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	83                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x13e7:0x5 DW_TAG_pointer_type
	.long	5100                    # DW_AT_type
	.byte	15                      # Abbrev [15] 0x13ec:0xb DW_TAG_typedef
	.long	5111                    # DW_AT_type
	.long	.Linfo_string83         # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	87                      # DW_AT_decl_line
	.byte	6                       # Abbrev [6] 0x13f7:0x7 DW_TAG_base_type
	.long	.Linfo_string82         # DW_AT_name
	.byte	7                       # DW_AT_encoding
	.byte	2                       # DW_AT_byte_size
	.byte	3                       # Abbrev [3] 0x13fe:0xc DW_TAG_array_type
	.long	3057                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1403:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x140a:0xc DW_TAG_array_type
	.long	5084                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x140f:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1416:0xd DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	36                      # Abbrev [36] 0x141b:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1423:0xd DW_TAG_array_type
	.long	5084                    # DW_AT_type
	.byte	36                      # Abbrev [36] 0x1428:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	18001                   # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1430:0x13 DW_TAG_array_type
	.long	5084                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1435:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	36                      # Abbrev [36] 0x143b:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1443:0x13 DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1448:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	36                      # Abbrev [36] 0x144e:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1456:0x13 DW_TAG_array_type
	.long	5063                    # DW_AT_type
	.byte	36                      # Abbrev [36] 0x145b:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	257                     # DW_AT_upper_bound
	.byte	4                       # Abbrev [4] 0x1462:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	3                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x1469:0x5 DW_TAG_pointer_type
	.long	5230                    # DW_AT_type
	.byte	32                      # Abbrev [32] 0x146e:0xc DW_TAG_typedef
	.long	5242                    # DW_AT_type
	.long	.Linfo_string170        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.short	472                     # DW_AT_decl_line
	.byte	37                      # Abbrev [37] 0x147a:0x3b6 DW_TAG_structure_type
	.short	64144                   # DW_AT_byte_size
	.byte	2                       # DW_AT_decl_file
	.short	382                     # DW_AT_decl_line
	.byte	34                      # Abbrev [34] 0x1480:0xe DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	4263                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	384                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x148e:0xe DW_TAG_member
	.long	.Linfo_string61         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	387                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x149c:0xe DW_TAG_member
	.long	.Linfo_string120        # DW_AT_name
	.long	5084                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	390                     # DW_AT_decl_line
	.byte	12                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14aa:0xe DW_TAG_member
	.long	.Linfo_string121        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	391                     # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14b8:0xe DW_TAG_member
	.long	.Linfo_string122        # DW_AT_name
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	392                     # DW_AT_decl_line
	.byte	20                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14c6:0xe DW_TAG_member
	.long	.Linfo_string87         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	393                     # DW_AT_decl_line
	.byte	24                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14d4:0xe DW_TAG_member
	.long	.Linfo_string88         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	393                     # DW_AT_decl_line
	.byte	28                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14e2:0xe DW_TAG_member
	.long	.Linfo_string96         # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	396                     # DW_AT_decl_line
	.byte	32                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14f0:0xe DW_TAG_member
	.long	.Linfo_string97         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	397                     # DW_AT_decl_line
	.byte	36                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x14fe:0xe DW_TAG_member
	.long	.Linfo_string66         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	400                     # DW_AT_decl_line
	.byte	40                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x150c:0xe DW_TAG_member
	.long	.Linfo_string123        # DW_AT_name
	.long	3057                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	401                     # DW_AT_decl_line
	.byte	44                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x151a:0xe DW_TAG_member
	.long	.Linfo_string124        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	402                     # DW_AT_decl_line
	.byte	48                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1528:0xe DW_TAG_member
	.long	.Linfo_string67         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	403                     # DW_AT_decl_line
	.byte	52                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1536:0xe DW_TAG_member
	.long	.Linfo_string77         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	406                     # DW_AT_decl_line
	.byte	56                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1544:0xe DW_TAG_member
	.long	.Linfo_string125        # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	407                     # DW_AT_decl_line
	.byte	60                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1552:0xe DW_TAG_member
	.long	.Linfo_string126        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	408                     # DW_AT_decl_line
	.byte	64                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x1560:0xe DW_TAG_member
	.long	.Linfo_string127        # DW_AT_name
	.long	6192                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	409                     # DW_AT_decl_line
	.byte	68                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x156e:0xf DW_TAG_member
	.long	.Linfo_string128        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	410                     # DW_AT_decl_line
	.short	1092                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x157d:0xf DW_TAG_member
	.long	.Linfo_string129        # DW_AT_name
	.long	6204                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	411                     # DW_AT_decl_line
	.short	1096                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x158c:0xf DW_TAG_member
	.long	.Linfo_string130        # DW_AT_name
	.long	6204                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	412                     # DW_AT_decl_line
	.short	2124                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x159b:0xf DW_TAG_member
	.long	.Linfo_string131        # DW_AT_name
	.long	5074                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	415                     # DW_AT_decl_line
	.short	3152                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15aa:0xf DW_TAG_member
	.long	.Linfo_string132        # DW_AT_name
	.long	5095                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	418                     # DW_AT_decl_line
	.short	3160                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15b9:0xf DW_TAG_member
	.long	.Linfo_string133        # DW_AT_name
	.long	5079                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	419                     # DW_AT_decl_line
	.short	3168                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15c8:0xf DW_TAG_member
	.long	.Linfo_string134        # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	422                     # DW_AT_decl_line
	.short	3176                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15d7:0xf DW_TAG_member
	.long	.Linfo_string135        # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	423                     # DW_AT_decl_line
	.short	3180                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15e6:0xf DW_TAG_member
	.long	.Linfo_string136        # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	424                     # DW_AT_decl_line
	.short	3184                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x15f5:0xf DW_TAG_member
	.long	.Linfo_string137        # DW_AT_name
	.long	5063                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	425                     # DW_AT_decl_line
	.short	3188                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1604:0xf DW_TAG_member
	.long	.Linfo_string93         # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	428                     # DW_AT_decl_line
	.short	3192                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1613:0xf DW_TAG_member
	.long	.Linfo_string94         # DW_AT_name
	.long	5118                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	429                     # DW_AT_decl_line
	.short	3196                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1622:0xf DW_TAG_member
	.long	.Linfo_string138        # DW_AT_name
	.long	6217                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	430                     # DW_AT_decl_line
	.short	3452                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1631:0xf DW_TAG_member
	.long	.Linfo_string139        # DW_AT_name
	.long	5130                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	431                     # DW_AT_decl_line
	.short	3468                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1640:0xf DW_TAG_member
	.long	.Linfo_string140        # DW_AT_name
	.long	6229                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	434                     # DW_AT_decl_line
	.short	3724                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x164f:0xf DW_TAG_member
	.long	.Linfo_string141        # DW_AT_name
	.long	6242                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	435                     # DW_AT_decl_line
	.short	7820                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x165e:0xf DW_TAG_member
	.long	.Linfo_string103        # DW_AT_name
	.long	5155                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	436                     # DW_AT_decl_line
	.short	7884                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x166d:0xf DW_TAG_member
	.long	.Linfo_string104        # DW_AT_name
	.long	5155                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	437                     # DW_AT_decl_line
	.short	25886                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x167c:0xf DW_TAG_member
	.long	.Linfo_string105        # DW_AT_name
	.long	5168                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	438                     # DW_AT_decl_line
	.short	43888                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x168b:0xf DW_TAG_member
	.long	.Linfo_string142        # DW_AT_name
	.long	5187                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	440                     # DW_AT_decl_line
	.short	45436                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x169a:0xf DW_TAG_member
	.long	.Linfo_string143        # DW_AT_name
	.long	5187                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	441                     # DW_AT_decl_line
	.short	51628                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16a9:0xf DW_TAG_member
	.long	.Linfo_string144        # DW_AT_name
	.long	5187                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	442                     # DW_AT_decl_line
	.short	57820                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16b8:0xf DW_TAG_member
	.long	.Linfo_string145        # DW_AT_name
	.long	6254                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	443                     # DW_AT_decl_line
	.short	64012                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16c7:0xf DW_TAG_member
	.long	.Linfo_string146        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	446                     # DW_AT_decl_line
	.short	64036                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16d6:0xf DW_TAG_member
	.long	.Linfo_string147        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	447                     # DW_AT_decl_line
	.short	64040                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16e5:0xf DW_TAG_member
	.long	.Linfo_string148        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	448                     # DW_AT_decl_line
	.short	64044                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x16f4:0xf DW_TAG_member
	.long	.Linfo_string149        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	449                     # DW_AT_decl_line
	.short	64048                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1703:0xf DW_TAG_member
	.long	.Linfo_string150        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	450                     # DW_AT_decl_line
	.short	64052                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1712:0xf DW_TAG_member
	.long	.Linfo_string151        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	451                     # DW_AT_decl_line
	.short	64056                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1721:0xf DW_TAG_member
	.long	.Linfo_string152        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	452                     # DW_AT_decl_line
	.short	64060                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1730:0xf DW_TAG_member
	.long	.Linfo_string153        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	453                     # DW_AT_decl_line
	.short	64064                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x173f:0xf DW_TAG_member
	.long	.Linfo_string154        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	454                     # DW_AT_decl_line
	.short	64068                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x174e:0xf DW_TAG_member
	.long	.Linfo_string155        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	455                     # DW_AT_decl_line
	.short	64072                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x175d:0xf DW_TAG_member
	.long	.Linfo_string156        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	456                     # DW_AT_decl_line
	.short	64076                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x176c:0xf DW_TAG_member
	.long	.Linfo_string157        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	457                     # DW_AT_decl_line
	.short	64080                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x177b:0xf DW_TAG_member
	.long	.Linfo_string158        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	458                     # DW_AT_decl_line
	.short	64084                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x178a:0xf DW_TAG_member
	.long	.Linfo_string159        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	459                     # DW_AT_decl_line
	.short	64088                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1799:0xf DW_TAG_member
	.long	.Linfo_string160        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	460                     # DW_AT_decl_line
	.short	64092                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17a8:0xf DW_TAG_member
	.long	.Linfo_string161        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	461                     # DW_AT_decl_line
	.short	64096                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17b7:0xf DW_TAG_member
	.long	.Linfo_string162        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	462                     # DW_AT_decl_line
	.short	64100                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17c6:0xf DW_TAG_member
	.long	.Linfo_string163        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	463                     # DW_AT_decl_line
	.short	64104                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17d5:0xf DW_TAG_member
	.long	.Linfo_string164        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	464                     # DW_AT_decl_line
	.short	64108                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17e4:0xf DW_TAG_member
	.long	.Linfo_string165        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	465                     # DW_AT_decl_line
	.short	64112                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x17f3:0xf DW_TAG_member
	.long	.Linfo_string166        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	466                     # DW_AT_decl_line
	.short	64116                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1802:0xf DW_TAG_member
	.long	.Linfo_string167        # DW_AT_name
	.long	6266                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	467                     # DW_AT_decl_line
	.short	64120                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1811:0xf DW_TAG_member
	.long	.Linfo_string168        # DW_AT_name
	.long	6266                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	468                     # DW_AT_decl_line
	.short	64128                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1820:0xf DW_TAG_member
	.long	.Linfo_string169        # DW_AT_name
	.long	6266                    # DW_AT_type
	.byte	2                       # DW_AT_decl_file
	.short	469                     # DW_AT_decl_line
	.short	64136                   # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1830:0xc DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1835:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	255                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x183c:0xd DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	36                      # Abbrev [36] 0x1841:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	256                     # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1849:0xc DW_TAG_array_type
	.long	3057                    # DW_AT_type
	.byte	4                       # Abbrev [4] 0x184e:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1855:0xd DW_TAG_array_type
	.long	5084                    # DW_AT_type
	.byte	36                      # Abbrev [36] 0x185a:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	4095                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1862:0xc DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1867:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	15                      # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x186e:0xc DW_TAG_array_type
	.long	607                     # DW_AT_type
	.byte	4                       # Abbrev [4] 0x1873:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	5                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	5                       # Abbrev [5] 0x187a:0x5 DW_TAG_pointer_type
	.long	607                     # DW_AT_type
	.byte	5                       # Abbrev [5] 0x187f:0x5 DW_TAG_pointer_type
	.long	247                     # DW_AT_type
	.byte	5                       # Abbrev [5] 0x1884:0x5 DW_TAG_pointer_type
	.long	6281                    # DW_AT_type
	.byte	32                      # Abbrev [32] 0x1889:0xc DW_TAG_typedef
	.long	6293                    # DW_AT_type
	.long	.Linfo_string193        # DW_AT_name
	.byte	4                       # DW_AT_decl_file
	.short	340                     # DW_AT_decl_line
	.byte	38                      # Abbrev [38] 0x1895:0x5 DW_TAG_structure_type
	.long	.Linfo_string192        # DW_AT_name
                                        # DW_AT_declaration
	.byte	5                       # Abbrev [5] 0x189a:0x5 DW_TAG_pointer_type
	.long	6303                    # DW_AT_type
	.byte	32                      # Abbrev [32] 0x189f:0xc DW_TAG_typedef
	.long	6315                    # DW_AT_type
	.long	.Linfo_string203        # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	962                     # DW_AT_decl_line
	.byte	37                      # Abbrev [37] 0x18ab:0x6e DW_TAG_structure_type
	.short	5104                    # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.short	949                     # DW_AT_decl_line
	.byte	34                      # Abbrev [34] 0x18b1:0xe DW_TAG_member
	.long	.Linfo_string196        # DW_AT_name
	.long	6276                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	953                     # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	34                      # Abbrev [34] 0x18bf:0xe DW_TAG_member
	.long	.Linfo_string197        # DW_AT_name
	.long	6425                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	955                     # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x18cd:0xf DW_TAG_member
	.long	.Linfo_string199        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	956                     # DW_AT_decl_line
	.short	5008                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x18dc:0xf DW_TAG_member
	.long	.Linfo_string200        # DW_AT_name
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	957                     # DW_AT_decl_line
	.short	5012                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x18eb:0xf DW_TAG_member
	.long	.Linfo_string51         # DW_AT_name
	.long	4268                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	958                     # DW_AT_decl_line
	.short	5016                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x18fa:0xf DW_TAG_member
	.long	.Linfo_string201        # DW_AT_name
	.long	607                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	959                     # DW_AT_decl_line
	.short	5096                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	35                      # Abbrev [35] 0x1909:0xf DW_TAG_member
	.long	.Linfo_string202        # DW_AT_name
	.long	3057                    # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.short	960                     # DW_AT_decl_line
	.short	5100                    # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1919:0xd DW_TAG_array_type
	.long	6438                    # DW_AT_type
	.byte	36                      # Abbrev [36] 0x191e:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	4999                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x1926:0xb DW_TAG_typedef
	.long	77                      # DW_AT_type
	.long	.Linfo_string198        # DW_AT_name
	.byte	2                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.byte	5                       # Abbrev [5] 0x1931:0x5 DW_TAG_pointer_type
	.long	4440                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x1936:0x5 DW_TAG_pointer_type
	.long	928                     # DW_AT_type
	.byte	3                       # Abbrev [3] 0x193b:0xd DW_TAG_array_type
	.long	77                      # DW_AT_type
	.byte	36                      # Abbrev [36] 0x1940:0x7 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.short	4999                    # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x1948:0xc DW_TAG_array_type
	.long	77                      # DW_AT_type
	.byte	4                       # Abbrev [4] 0x194d:0x6 DW_TAG_subrange_type
	.long	84                      # DW_AT_type
	.byte	9                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
.L.debug_info_end0:
	.section	.debug_abbrev,"",@progbits
	.byte	1                       # Abbreviation Code
	.byte	17                      # DW_TAG_compile_unit
	.byte	1                       # DW_CHILDREN_yes
	.byte	37                      # DW_AT_producer
	.byte	14                      # DW_FORM_strp
	.byte	19                      # DW_AT_language
	.byte	5                       # DW_FORM_data2
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	16                      # DW_AT_stmt_list
	.byte	23                      # DW_FORM_sec_offset
	.byte	27                      # DW_AT_comp_dir
	.byte	14                      # DW_FORM_strp
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	2                       # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	4                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
	.byte	36                      # DW_TAG_base_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	62                      # DW_AT_encoding
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	8                       # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	10                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	11                      # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	12                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	14                      # Abbreviation Code
	.byte	52                      # DW_TAG_variable
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	15                      # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	16                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	17                      # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	18                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	19                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	20                      # Abbreviation Code
	.byte	38                      # DW_TAG_const_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	21                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	22                      # Abbreviation Code
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	23                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	24                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	25                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	26                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	17                      # DW_AT_low_pc
	.byte	1                       # DW_FORM_addr
	.byte	18                      # DW_AT_high_pc
	.byte	1                       # DW_FORM_addr
	.byte	64                      # DW_AT_frame_base
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	27                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	28                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	29                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	30                      # Abbreviation Code
	.byte	5                       # DW_TAG_formal_parameter
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	31                      # Abbreviation Code
	.byte	21                      # DW_TAG_subroutine_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	39                      # DW_AT_prototyped
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	32                      # Abbreviation Code
	.byte	22                      # DW_TAG_typedef
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	33                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	5                       # DW_FORM_data2
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	34                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	11                      # DW_FORM_data1
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	35                      # Abbreviation Code
	.byte	13                      # DW_TAG_member
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	56                      # DW_AT_data_member_location
	.byte	5                       # DW_FORM_data2
	.byte	50                      # DW_AT_accessibility
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	36                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	37                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	11                      # DW_AT_byte_size
	.byte	5                       # DW_FORM_data2
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	5                       # DW_FORM_data2
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	38                      # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	0                       # DW_CHILDREN_no
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	60                      # DW_AT_declaration
	.byte	25                      # DW_FORM_flag_present
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp227
	.quad	.Ltmp228
	.quad	.Ltmp242
	.quad	.Ltmp244
	.quad	.Ltmp246
	.quad	.Ltmp250
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp227
	.quad	.Ltmp228
	.quad	.Ltmp241
	.quad	.Ltmp244
	.quad	.Ltmp246
	.quad	.Ltmp250
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp227
	.quad	.Ltmp228
	.quad	.Ltmp229
	.quad	.Ltmp250
	.quad	0
	.quad	0
.Ldebug_ranges3:
	.quad	.Ltmp892
	.quad	.Ltmp893
	.quad	.Ltmp894
	.quad	.Ltmp895
	.quad	.Ltmp896
	.quad	.Ltmp906
	.quad	0
	.quad	0
.Ldebug_ranges4:
	.quad	.Ltmp889
	.quad	.Ltmp890
	.quad	.Ltmp891
	.quad	.Ltmp906
	.quad	0
	.quad	0
.Ldebug_ranges5:
	.quad	.Ltmp914
	.quad	.Ltmp915
	.quad	.Ltmp916
	.quad	.Ltmp917
	.quad	.Ltmp918
	.quad	.Ltmp928
	.quad	0
	.quad	0
.Ldebug_ranges6:
	.quad	.Ltmp911
	.quad	.Ltmp912
	.quad	.Ltmp913
	.quad	.Ltmp928
	.quad	0
	.quad	0
.Ldebug_ranges7:
	.quad	.Ltmp882
	.quad	.Ltmp883
	.quad	.Ltmp884
	.quad	.Ltmp930
	.quad	0
	.quad	0
	.section	.debug_pubnames,"",@progbits
.Lset0 = .Lpubnames_end0-.Lpubnames_begin0 # Length of Public Names Info
	.long	.Lset0
.Lpubnames_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset1 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset1
	.long	3557                    # DIE offset
	.asciz	"flush_RL"              # External Name
	.long	1844                    # DIE offset
	.asciz	"BZ2_bzBuffToBuffCompress" # External Name
	.long	3995                    # DIE offset
	.asciz	"prepare_new_block"     # External Name
	.long	4091                    # DIE offset
	.asciz	"default_bzfree"        # External Name
	.long	2332                    # DIE offset
	.asciz	"BZ2_bzread"            # External Name
	.long	791                     # DIE offset
	.asciz	"BZ2_bzWriteOpen"       # External Name
	.long	2013                    # DIE offset
	.asciz	"BZ2_bzBuffToBuffDecompress" # External Name
	.long	132                     # DIE offset
	.asciz	"BZ2_bzCompressInit"    # External Name
	.long	1077                    # DIE offset
	.asciz	"BZ2_bzWriteClose"      # External Name
	.long	2167                    # DIE offset
	.asciz	"BZ2_bzlibVersion"      # External Name
	.long	2995                    # DIE offset
	.asciz	"myfeof"                # External Name
	.long	408                     # DIE offset
	.asciz	"BZ2_bzDecompressInit"  # External Name
	.long	2439                    # DIE offset
	.asciz	"BZ2_bzwrite"           # External Name
	.long	729                     # DIE offset
	.asciz	"BZ2_bzDecompressEnd"   # External Name
	.long	3465                    # DIE offset
	.asciz	"handle_compress"       # External Name
	.long	1741                    # DIE offset
	.asciz	"BZ2_bzReadGetUnused"   # External Name
	.long	91                      # DIE offset
	.asciz	"BZ2_bz__AssertH__fail" # External Name
	.long	1531                    # DIE offset
	.asciz	"BZ2_bzReadClose"       # External Name
	.long	2578                    # DIE offset
	.asciz	"BZ2_bzclose"           # External Name
	.long	2728                    # DIE offset
	.asciz	"bzopen_or_bzdopen"     # External Name
	.long	3600                    # DIE offset
	.asciz	"add_pair_to_block"     # External Name
	.long	500                     # DIE offset
	.asciz	"BZ2_indexIntoF"        # External Name
	.long	1604                    # DIE offset
	.asciz	"BZ2_bzRead"            # External Name
	.long	3420                    # DIE offset
	.asciz	"isempty_RL"            # External Name
	.long	4233                    # DIE offset
	.asciz	"bz_config_ok"          # External Name
	.long	2531                    # DIE offset
	.asciz	"BZ2_bzflush"           # External Name
	.long	346                     # DIE offset
	.asciz	"BZ2_bzCompressEnd"     # External Name
	.long	4146                    # DIE offset
	.asciz	"default_bzalloc"       # External Name
	.long	3075                    # DIE offset
	.asciz	"unRLE_obuf_to_output_FAST" # External Name
	.long	4050                    # DIE offset
	.asciz	"init_RL"               # External Name
	.long	3933                    # DIE offset
	.asciz	"copy_output_until_stop" # External Name
	.long	2651                    # DIE offset
	.asciz	"BZ2_bzerror"           # External Name
	.long	1180                    # DIE offset
	.asciz	"BZ2_bzWriteClose64"    # External Name
	.long	929                     # DIE offset
	.asciz	"BZ2_bzWrite"           # External Name
	.long	254                     # DIE offset
	.asciz	"BZ2_bzCompress"        # External Name
	.long	1378                    # DIE offset
	.asciz	"BZ2_bzReadOpen"        # External Name
	.long	3673                    # DIE offset
	.asciz	"copy_input_until_stop" # External Name
	.long	2208                    # DIE offset
	.asciz	"BZ2_bzopen"            # External Name
	.long	3358                    # DIE offset
	.asciz	"unRLE_obuf_to_output_SMALL" # External Name
	.long	618                     # DIE offset
	.asciz	"BZ2_bzDecompress"      # External Name
	.long	2270                    # DIE offset
	.asciz	"BZ2_bzdopen"           # External Name
	.long	0                       # End Mark
.Lpubnames_end0:
	.section	.debug_pubtypes,"",@progbits
.Lset2 = .Lpubtypes_end0-.Lpubtypes_begin0 # Length of Public Types Info
	.long	.Lset2
.Lpubtypes_begin0:
	.short	2                       # DWARF Version
	.long	.L.debug_info_begin0    # Offset of Compilation Unit Info
.Lset3 = .L.debug_info_end0-.L.debug_info_begin0 # Compilation Unit Length
	.long	.Lset3
	.long	6281                    # DIE offset
	.asciz	"FILE"                  # External Name
	.long	5063                    # DIE offset
	.asciz	"UInt32"                # External Name
	.long	4495                    # DIE offset
	.asciz	"EState"                # External Name
	.long	4268                    # DIE offset
	.asciz	"bz_stream"             # External Name
	.long	4440                    # DIE offset
	.asciz	"unsigned int"          # External Name
	.long	5100                    # DIE offset
	.asciz	"UInt16"                # External Name
	.long	247                     # DIE offset
	.asciz	"int"                   # External Name
	.long	3057                    # DIE offset
	.asciz	"Bool"                  # External Name
	.long	5111                    # DIE offset
	.asciz	"unsigned short"        # External Name
	.long	5230                    # DIE offset
	.asciz	"DState"                # External Name
	.long	607                     # DIE offset
	.asciz	"Int32"                 # External Name
	.long	5084                    # DIE offset
	.asciz	"UChar"                 # External Name
	.long	3068                    # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	6303                    # DIE offset
	.asciz	"bzFile"                # External Name
	.long	77                      # DIE offset
	.asciz	"char"                  # External Name
	.long	6438                    # DIE offset
	.asciz	"Char"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
