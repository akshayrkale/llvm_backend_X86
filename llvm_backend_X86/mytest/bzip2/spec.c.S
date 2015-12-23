	.file	"spec.c.bc"
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
	.file	1 "spec.c"
	.text
	.globl	ran
	.align	16, 0x90
	.type	ran,@function
ran:                                    # @ran
.Lfunc_begin0:
	.loc	1 63 0                  # spec.c:63:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp2:
	.cfi_def_cfa_offset 16
.Ltmp3:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp4:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movabsq	$4730756183288445817, %rsi # imm = 0x41A705AF1FE3FB79
	.loc	1 68 0 prologue_end     # spec.c:68:0
.Ltmp5:
	movq	%rsi, %rax
	imulq	seedi(%rip)
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$15, %rcx
	sarq	%cl, %rdx
	.loc	1 69 0                  # spec.c:69:0
	movq	seedi(%rip), %rdi
	.loc	1 68 0                  # spec.c:68:0
	addq	%rax, %rdx
	movq	%rdx, -16(%rbp)
	.loc	1 69 0                  # spec.c:69:0
	movq	%rdi, %rax
	imulq	%rsi
	movq	$63, %rcx
	movq	%rdx, %rax
	shrq	%cl, %rax
	movq	$15, %rcx
	sarq	%cl, %rdx
	addq	%rax, %rdx
	imulq	$127773, %rdx, %rax     # imm = 0x1F31D
	subq	%rax, %rdi
	.loc	1 70 0                  # spec.c:70:0
	movq	-16(%rbp), %rax
	.loc	1 69 0                  # spec.c:69:0
	movq	%rdi, -8(%rbp)
	.loc	1 70 0                  # spec.c:70:0
	imulq	$16807, %rdi, %rcx      # imm = 0x41A7
	imulq	$2836, %rax, %rax       # imm = 0xB14
	subq	%rax, %rcx
	testq	%rcx, %rcx
	movq	%rcx, -24(%rbp)
	.loc	1 71 0                  # spec.c:71:0
.Ltmp6:
	jle	.LBB0_2
# BB#1:                                 # %if.then
	.loc	1 72 0                  # spec.c:72:0
.Ltmp7:
	movq	-24(%rbp), %rax
	jmp	.LBB0_3
.Ltmp8:
.LBB0_2:                                # %if.else
	.loc	1 74 0                  # spec.c:74:0
	movq	-24(%rbp), %rax
	addq	$2147483647, %rax       # imm = 0x7FFFFFFF
.LBB0_3:                                # %if.end
	movq	%rax, seedi(%rip)
.Ltmp9:
	.loc	1 76 0                  # spec.c:76:0
	movq	seedi(%rip), %rdi
	callq	int64_to_float64
	movabsq	$4746794007244308480, %rsi # imm = 0x41DFFFFFFFC00000
	movq	%rax, %rdi
	callq	float64_div
	addq	$32, %rsp
	popq	%rbp
	retq
.Ltmp10:
.Ltmp11:
	.size	ran, .Ltmp11-ran
.Lfunc_end0:
	.cfi_endproc

	.globl	spec_init
	.align	16, 0x90
	.type	spec_init,@function
spec_init:                              # @spec_init
.Lfunc_begin1:
	.loc	1 80 0                  # spec.c:80:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp15:
	.cfi_def_cfa_offset 16
.Ltmp16:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp17:
	.cfi_def_cfa_register %rbp
	.loc	1 82 0 prologue_end     # spec.c:82:0
.Ltmp18:
	pushq	%rbx
	subq	$24, %rsp
.Ltmp19:
	.cfi_offset %rbx, -24
	movslq	dbglvl(%rip), %rax
	cmpq	$4, %rax
	jl	.LBB1_2
# BB#1:                                 # %if.then
	movabsq	$.L.str, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp20:
.LBB1_2:                                # %if.end
	xorq	%rbx, %rbx
	.loc	1 87 0                  # spec.c:87:0
.Ltmp21:
	movl	%ebx, -12(%rbp)
	jmp	.LBB1_3
	.align	16, 0x90
.LBB1_8:                                # %for.inc29
                                        #   in Loop: Header=BB1_3 Depth=1
	movl	-12(%rbp), %eax
	incq	%rax
	movl	%eax, -12(%rbp)
.LBB1_3:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB1_6 Depth 2
	movslq	-12(%rbp), %rax
	cmpq	$2, %rax
	jg	.LBB1_9
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB1_3 Depth=1
.Ltmp22:
	#DEBUG_VALUE: limit <- [RBP+-20]
	.loc	1 88 0                  # spec.c:88:0
	movslq	-12(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	.loc	1 89 0                  # spec.c:89:0
	movslq	-12(%rbp), %rcx
	.loc	1 88 0                  # spec.c:88:0
	movl	spec_fd(,%rax,8), %eax
	movl	%eax, -20(%rbp)
	.loc	1 89 0                  # spec.c:89:0
	leaq	(%rcx,%rcx,2), %rax
	movq	$0, spec_fd+16(,%rax,8)
	movq	$0, spec_fd+8(,%rax,8)
	movq	$0, spec_fd(,%rax,8)
	.loc	1 90 0                  # spec.c:90:0
	movl	-20(%rbp), %eax
	movslq	-12(%rbp), %rcx
	leaq	(%rcx,%rcx,2), %rcx
	.loc	1 91 0                  # spec.c:91:0
	movl	-20(%rbp), %edi
	.loc	1 90 0                  # spec.c:90:0
	movl	%eax, spec_fd(,%rcx,8)
	.loc	1 91 0                  # spec.c:91:0
	addq	$1048576, %rdi          # imm = 0x100000
	movq	$32, %rcx
	shlq	%cl, %rdi
	movq	$32, %rcx
	sarq	%cl, %rdi
	callq	malloc
	movslq	-12(%rbp), %rcx
	.loc	1 92 0                  # spec.c:92:0
.Ltmp23:
	movslq	-12(%rbp), %rdx
.Ltmp24:
	.loc	1 91 0                  # spec.c:91:0
	leaq	(%rcx,%rcx,2), %rcx
	movq	%rax, spec_fd+16(,%rcx,8)
	.loc	1 92 0                  # spec.c:92:0
.Ltmp25:
	leaq	(%rdx,%rdx,2), %rax
	cmpq	$0, spec_fd+16(,%rax,8)
	je	.LBB1_10
.Ltmp26:
# BB#5:                                 # %if.end18
                                        #   in Loop: Header=BB1_3 Depth=1
	#DEBUG_VALUE: limit <- [RBP+-20]
	.loc	1 96 0                  # spec.c:96:0
	movl	%ebx, -16(%rbp)
	jmp	.LBB1_6
	.align	16, 0x90
.LBB1_7:                                # %for.inc
                                        #   in Loop: Header=BB1_6 Depth=2
	#DEBUG_VALUE: limit <- [RBP+-20]
	.loc	1 97 0                  # spec.c:97:0
.Ltmp27:
	movslq	-12(%rbp), %rax
	movslq	-16(%rbp), %rcx
	leaq	(%rax,%rax,2), %rax
	movq	spec_fd+16(,%rax,8), %rax
	movb	%bl, (%rax,%rcx)
.Ltmp28:
	.loc	1 96 0                  # spec.c:96:0
	movl	-16(%rbp), %eax
	addq	$1024, %rax             # imm = 0x400
	movl	%eax, -16(%rbp)
.LBB1_6:                                # %for.cond19
                                        #   Parent Loop BB1_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	#DEBUG_VALUE: limit <- [RBP+-20]
	movslq	-16(%rbp), %rax
	movslq	-20(%rbp), %rcx
	cmpq	%rcx, %rax
	jl	.LBB1_7
	jmp	.LBB1_8
.Ltmp29:
.LBB1_9:                                # %for.end30
	xorq	%rax, %rax
	.loc	1 100 0                 # spec.c:100:0
	addq	$24, %rsp
	popq	%rbx
	popq	%rbp
	retq
.LBB1_10:                               # %if.then16
.Ltmp30:
	#DEBUG_VALUE: limit <- [RBP+-20]
	.loc	1 93 0                  # spec.c:93:0
	movabsq	$.L.str1, %rcx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	printf
	xorq	%rdi, %rdi
	.loc	1 94 0                  # spec.c:94:0
	callq	exit
.Ltmp31:
.Ltmp32:
	.size	spec_init, .Ltmp32-spec_init
.Lfunc_end1:
	.cfi_endproc

	.globl	spec_random_load
	.align	16, 0x90
	.type	spec_random_load,@function
spec_random_load:                       # @spec_random_load
.Lfunc_begin2:
	.loc	1 103 0                 # spec.c:103:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp36:
	.cfi_def_cfa_offset 16
.Ltmp37:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp38:
	.cfi_def_cfa_register %rbp
	.loc	1 113 0 prologue_end    # spec.c:113:0
.Ltmp39:
	pushq	%r15
	pushq	%r14
	pushq	%rbx
	subq	$4194328, %rsp          # imm = 0x400018
.Ltmp40:
	.cfi_offset %rbx, -40
.Ltmp41:
	.cfi_offset %r14, -32
.Ltmp42:
	.cfi_offset %r15, -24
	movslq	dbglvl(%rip), %rax
	movl	%edi, -28(%rbp)
	cmpq	$5, %rax
	jl	.LBB2_2
# BB#1:                                 # %if.then
	movabsq	$.L.str2, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp43:
.LBB2_2:                                # %if.end
	xorq	%r15, %r15
	.loc	1 114 0                 # spec.c:114:0
.Ltmp44:
	movl	%r15d, -32(%rbp)
	.loc	1 115 0                 # spec.c:115:0
.Ltmp45:
	movabsq	$.L.str3, %r14
	movabsq	$4643211215818981376, %rbx # imm = 0x4070000000000000
	jmp	.LBB2_3
.Ltmp46:
	.align	16, 0x90
.LBB2_9:                                # %for.inc13
                                        #   in Loop: Header=BB2_3 Depth=1
	.loc	1 114 0                 # spec.c:114:0
	movl	-32(%rbp), %eax
	incq	%rax
	movl	%eax, -32(%rbp)
.LBB2_3:                                # %for.cond
                                        # =>This Loop Header: Depth=1
                                        #     Child Loop BB2_7 Depth 2
	movslq	-32(%rbp), %rax
	cmpq	$31, %rax
	jg	.LBB2_10
# BB#4:                                 # %for.body
                                        #   in Loop: Header=BB2_3 Depth=1
	.loc	1 115 0                 # spec.c:115:0
.Ltmp47:
	movslq	dbglvl(%rip), %rax
	cmpq	$6, %rax
	jl	.LBB2_6
# BB#5:                                 # %if.then3
                                        #   in Loop: Header=BB2_3 Depth=1
	movl	-32(%rbp), %esi
	xorq	%rax, %rax
	movq	%r14, %rdi
	callq	printf
.Ltmp48:
.LBB2_6:                                # %if.end5
                                        #   in Loop: Header=BB2_3 Depth=1
	.loc	1 116 0                 # spec.c:116:0
	movl	%r15d, -36(%rbp)
	jmp	.LBB2_7
	.align	16, 0x90
.LBB2_8:                                # %for.inc
                                        #   in Loop: Header=BB2_7 Depth=2
	.loc	1 117 0                 # spec.c:117:0
.Ltmp49:
	callq	ran
	movq	%rax, %rdi
	movq	%rbx, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movslq	-32(%rbp), %rdx
	movq	$17, %rcx
	shlq	%cl, %rdx
.Ltmp50:
	.loc	1 116 0                 # spec.c:116:0
	movl	-36(%rbp), %ecx
	.loc	1 117 0                 # spec.c:117:0
.Ltmp51:
	movslq	-36(%rbp), %rsi
	leaq	-4194352(%rbp,%rdx), %rdx
	movb	%al, (%rsi,%rdx)
.Ltmp52:
	.loc	1 116 0                 # spec.c:116:0
	incq	%rcx
	movl	%ecx, -36(%rbp)
.LBB2_7:                                # %for.cond6
                                        #   Parent Loop BB2_3 Depth=1
                                        # =>  This Inner Loop Header: Depth=2
	movslq	-36(%rbp), %rax
	cmpq	$131071, %rax           # imm = 0x1FFFF
	jle	.LBB2_8
	jmp	.LBB2_9
.Ltmp53:
.LBB2_10:                               # %for.end15
	.loc	1 121 0                 # spec.c:121:0
	movslq	dbglvl(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB2_12
# BB#11:                                # %if.then18
	movabsq	$.L.str4, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp54:
.LBB2_12:                               # %if.end20
	xorq	%rax, %rax
	.loc	1 123 0                 # spec.c:123:0
.Ltmp55:
	movl	%eax, -32(%rbp)
	movabsq	$4629700416936869888, %r14 # imm = 0x4040000000000000
	jmp	.LBB2_13
	.align	16, 0x90
.LBB2_14:                               # %for.inc34
                                        #   in Loop: Header=BB2_13 Depth=1
	.loc	1 124 0                 # spec.c:124:0
.Ltmp56:
	movslq	-28(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movq	spec_fd+16(,%rax,8), %rax
	movslq	-32(%rbp), %rbx
	addq	%rax, %rbx
	callq	ran
	movq	%rax, %rdi
	movq	%r14, %rsi
	callq	float64_mul
	movq	%rax, %rdi
	callq	float64_to_int64
	movq	$17, %rcx
	shlq	%cl, %rax
	leaq	-4194352(%rbp,%rax), %rsi
	movq	$131072, %rdx           # imm = 0x20000
	movq	%rbx, %rdi
	callq	memcpy
.Ltmp57:
	.loc	1 123 0                 # spec.c:123:0
	movl	-32(%rbp), %eax
	addq	$131072, %rax           # imm = 0x20000
	movl	%eax, -32(%rbp)
.LBB2_13:                               # %for.cond21
                                        # =>This Inner Loop Header: Depth=1
	movslq	-28(%rbp), %rax
	movslq	-32(%rbp), %rcx
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd(,%rax,8), %rax
	cmpq	%rax, %rcx
	jl	.LBB2_14
.Ltmp58:
# BB#15:                                # %for.end35
	.loc	1 128 0                 # spec.c:128:0
	movslq	-28(%rbp), %rax
	leaq	(%rax,%rax,2), %rcx
	movq	$1048576, %rdx          # imm = 0x100000
	xorq	%rax, %rax
	movl	%edx, spec_fd+4(,%rcx,8)
	.loc	1 129 0                 # spec.c:129:0
	addq	$4194328, %rsp          # imm = 0x400018
	popq	%rbx
	popq	%r14
	popq	%r15
	popq	%rbp
	retq
.Ltmp59:
.Ltmp60:
	.size	spec_random_load, .Ltmp60-spec_random_load
.Lfunc_end2:
	.cfi_endproc

	.globl	spec_load
	.align	16, 0x90
	.type	spec_load,@function
spec_load:                              # @spec_load
.Lfunc_begin3:
	.loc	1 132 0                 # spec.c:132:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp64:
	.cfi_def_cfa_offset 16
.Ltmp65:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp66:
	.cfi_def_cfa_register %rbp
	pushq	%r14
	pushq	%rbx
	subq	$48, %rsp
.Ltmp67:
	.cfi_offset %rbx, -32
.Ltmp68:
	.cfi_offset %r14, -24
	movq	%rsi, -32(%rbp)
	.loc	1 138 0 prologue_end    # spec.c:138:0
.Ltmp69:
	movq	-32(%rbp), %rcx
	movl	%edi, -20(%rbp)
	movl	%edx, -36(%rbp)
	xorq	%rsi, %rsi
	xorq	%rax, %rax
	xorq	%rbx, %rbx
	movq	%rcx, %rdi
	callq	open
	movq	$32, %rcx
	movq	%rax, %rdx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	testq	%rdx, %rdx
	movl	%eax, -40(%rbp)
	js	.LBB3_1
# BB#3:                                 # %if.end
	.loc	1 143 0                 # spec.c:143:0
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	-20(%rbp), %rcx
	movl	%ebx, spec_fd+4(,%rax,8)
	leaq	(%rcx,%rcx,2), %rax
	movl	%ebx, spec_fd+8(,%rax,8)
	.loc	1 144 0                 # spec.c:144:0
.Ltmp70:
	movl	%ebx, -48(%rbp)
	movabsq	$4294967295, %rbx       # imm = 0xFFFFFFFF
	jmp	.LBB3_4
	.align	16, 0x90
.LBB3_8:                                # %for.inc
                                        #   in Loop: Header=BB3_4 Depth=1
	.loc	1 151 0                 # spec.c:151:0
.Ltmp71:
	movslq	-20(%rbp), %rax
	movl	-44(%rbp), %ecx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %edx
.Ltmp72:
	.loc	1 144 0                 # spec.c:144:0
	movl	-44(%rbp), %esi
	movl	-48(%rbp), %edi
	.loc	1 151 0                 # spec.c:151:0
.Ltmp73:
	addq	%rcx, %rdx
	movl	%edx, spec_fd+4(,%rax,8)
.Ltmp74:
	.loc	1 144 0                 # spec.c:144:0
	addq	%rsi, %rdi
	movl	%edi, -48(%rbp)
.LBB3_4:                                # %for.cond
                                        # =>This Inner Loop Header: Depth=1
	movslq	-48(%rbp), %rax
	movslq	-36(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB3_9
# BB#5:                                 # %for.body
                                        #   in Loop: Header=BB3_4 Depth=1
	.loc	1 145 0                 # spec.c:145:0
.Ltmp75:
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movq	spec_fd+16(,%rax,8), %rax
	movslq	-48(%rbp), %rsi
	movl	-40(%rbp), %edi
	addq	%rax, %rsi
	movq	$131072, %rdx           # imm = 0x20000
	callq	read
	testq	%rbx, %rax
	movl	%eax, -44(%rbp)
	je	.LBB3_9
# BB#6:                                 # %if.end13
                                        #   in Loop: Header=BB3_4 Depth=1
	.loc	1 147 0                 # spec.c:147:0
.Ltmp76:
	movslq	-44(%rbp), %rax
	testq	%rax, %rax
	jns	.LBB3_8
# BB#7:                                 # %if.then16
	.loc	1 148 0                 # spec.c:148:0
.Ltmp77:
	movq	stderr(%rip), %rbx
	movq	-32(%rbp), %r14
	callq	__errno_location
	movl	(%rax), %eax
	movq	%rax, %rdi
	callq	strerror
	movq	%rax, %rcx
	movabsq	$.L.str6, %rdx
	jmp	.LBB3_2
.Ltmp78:
.LBB3_9:                                # %for.end
	.loc	1 153 0                 # spec.c:153:0
	movl	-40(%rbp), %edi
	callq	close
	.loc	1 157 0                 # spec.c:157:0
.Ltmp79:
	movabsq	$.L.str7, %rbx
	jmp	.LBB3_10
.Ltmp80:
	.align	16, 0x90
.LBB3_15:                               # %if.end48
                                        #   in Loop: Header=BB3_10 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-52]
	.loc	1 158 0                 # spec.c:158:0
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movq	spec_fd+16(,%rax,8), %rsi
	movslq	spec_fd+4(,%rax,8), %rdi
	movslq	-52(%rbp), %rdx
	addq	%rsi, %rdi
	callq	memcpy
	.loc	1 159 0                 # spec.c:159:0
	movslq	-20(%rbp), %rax
	movl	-52(%rbp), %ecx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %edx
	addq	%rcx, %rdx
	movl	%edx, spec_fd+4(,%rax,8)
.Ltmp81:
.LBB3_10:                               # %while.cond
                                        # =>This Inner Loop Header: Depth=1
	.loc	1 154 0                 # spec.c:154:0
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+4(,%rax,8), %rax
	movslq	-36(%rbp), %rcx
	cmpq	%rcx, %rax
	jge	.LBB3_16
# BB#11:                                # %while.body
                                        #   in Loop: Header=BB3_10 Depth=1
.Ltmp82:
	#DEBUG_VALUE: tmp <- [RBP+-52]
	.loc	1 155 0                 # spec.c:155:0
	movslq	-20(%rbp), %rax
	movl	-36(%rbp), %edx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %eax
	.loc	1 156 0                 # spec.c:156:0
.Ltmp83:
	movslq	-20(%rbp), %rsi
.Ltmp84:
	.loc	1 155 0                 # spec.c:155:0
	subq	%rax, %rdx
	movq	$32, %rcx
	movq	%rdx, %rax
	shlq	%cl, %rax
	.loc	1 156 0                 # spec.c:156:0
.Ltmp85:
	leaq	(%rsi,%rsi,2), %rcx
	movslq	spec_fd+4(,%rcx,8), %rsi
	movq	$32, %rcx
.Ltmp86:
	.loc	1 155 0                 # spec.c:155:0
	sarq	%cl, %rax
	movl	%edx, -52(%rbp)
	cmpq	%rsi, %rax
	jle	.LBB3_13
# BB#12:                                # %if.then39
                                        #   in Loop: Header=BB3_10 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-52]
	.loc	1 156 0                 # spec.c:156:0
.Ltmp87:
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %eax
	movl	%eax, -52(%rbp)
.Ltmp88:
.LBB3_13:                               # %if.end43
                                        #   in Loop: Header=BB3_10 Depth=1
	#DEBUG_VALUE: tmp <- [RBP+-52]
	.loc	1 157 0                 # spec.c:157:0
	movslq	dbglvl(%rip), %rax
	cmpq	$4, %rax
	jl	.LBB3_15
# BB#14:                                # %if.then46
                                        #   in Loop: Header=BB3_10 Depth=1
.Ltmp89:
	#DEBUG_VALUE: tmp <- [RBP+-52]
	movl	-52(%rbp), %esi
	xorq	%rax, %rax
	movq	%rbx, %rdi
	callq	printf
	jmp	.LBB3_15
.Ltmp90:
.LBB3_16:                               # %while.end
	xorq	%rax, %rax
	.loc	1 161 0                 # spec.c:161:0
	addq	$48, %rsp
	popq	%rbx
	popq	%r14
	popq	%rbp
	retq
.LBB3_1:                                # %if.then
	.loc	1 140 0                 # spec.c:140:0
.Ltmp91:
	movq	stderr(%rip), %rbx
	movq	-32(%rbp), %r14
	callq	__errno_location
	movl	(%rax), %eax
	movq	%rax, %rdi
	callq	strerror
	movq	%rax, %rcx
	movabsq	$.L.str5, %rdx
.LBB3_2:                                # %if.then
	xorq	%rax, %rax
	movq	%rbx, %rdi
	movq	%rdx, %rsi
	movq	%r14, %rdx
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 141 0                 # spec.c:141:0
	callq	exit
.Ltmp92:
.Ltmp93:
	.size	spec_load, .Ltmp93-spec_load
.Lfunc_end3:
	.cfi_endproc

	.globl	spec_read
	.align	16, 0x90
	.type	spec_read,@function
spec_read:                              # @spec_read
.Lfunc_begin4:
	.loc	1 164 0                 # spec.c:164:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp96:
	.cfi_def_cfa_offset 16
.Ltmp97:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp98:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movl	%edi, -8(%rbp)
	movq	%rsi, -16(%rbp)
	.loc	1 166 0 prologue_end    # spec.c:166:0
.Ltmp99:
	movslq	dbglvl(%rip), %rax
	movl	%edx, -20(%rbp)
	xorq	%rcx, %rcx
.Ltmp100:
	.loc	1 165 0                 # spec.c:165:0
	movl	%ecx, -24(%rbp)
	.loc	1 166 0                 # spec.c:166:0
.Ltmp101:
	cmpq	$5, %rax
	jl	.LBB4_2
# BB#1:                                 # %if.then
	movq	-16(%rbp), %rdx
	movl	-20(%rbp), %ecx
	movl	-8(%rbp), %esi
	movabsq	$.L.str8, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp102:
.LBB4_2:                                # %if.end
	.loc	1 167 0                 # spec.c:167:0
	movslq	-8(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB4_14
.Ltmp103:
# BB#3:                                 # %if.end4
	.loc	1 171 0                 # spec.c:171:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movslq	spec_fd+4(,%rax,8), %rax
	cmpq	%rax, %rcx
	jl	.LBB4_7
# BB#4:                                 # %if.then8
	.loc	1 172 0                 # spec.c:172:0
.Ltmp104:
	movslq	dbglvl(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB4_6
# BB#5:                                 # %if.then10
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp105:
.LBB4_6:                                # %if.end12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB4_13
.LBB4_7:                                # %if.end13
	.loc	1 175 0                 # spec.c:175:0
.Ltmp106:
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %ecx
	movl	-20(%rbp), %edx
	addq	%rcx, %rdx
	movq	$32, %rcx
	movslq	spec_fd+4(,%rax,8), %rax
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	cmpq	%rax, %rdx
	jl	.LBB4_9
# BB#8:                                 # %if.then21
	.loc	1 176 0                 # spec.c:176:0
.Ltmp107:
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %ecx
	movl	spec_fd+8(,%rax,8), %eax
	subq	%rax, %rcx
	movl	%ecx, -24(%rbp)
	jmp	.LBB4_10
.Ltmp108:
.LBB4_9:                                # %if.else
	.loc	1 178 0                 # spec.c:178:0
	movl	-20(%rbp), %eax
	movl	%eax, -24(%rbp)
.Ltmp109:
.LBB4_10:                               # %if.end28
	.loc	1 180 0                 # spec.c:180:0
	movslq	-8(%rbp), %rax
	movq	-16(%rbp), %rdi
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movq	spec_fd+16(,%rax,8), %rsi
	movslq	-24(%rbp), %rdx
	addq	%rcx, %rsi
	callq	memcpy
	.loc	1 181 0                 # spec.c:181:0
	movslq	-8(%rbp), %rax
	movl	-24(%rbp), %ecx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %edx
	.loc	1 182 0                 # spec.c:182:0
.Ltmp110:
	movslq	dbglvl(%rip), %rsi
.Ltmp111:
	.loc	1 181 0                 # spec.c:181:0
	addq	%rcx, %rdx
	movl	%edx, spec_fd+8(,%rax,8)
	.loc	1 182 0                 # spec.c:182:0
.Ltmp112:
	cmpq	$5, %rsi
	jl	.LBB4_12
# BB#11:                                # %if.then43
	movl	-24(%rbp), %esi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp113:
.LBB4_12:                               # %if.end45
	.loc	1 183 0                 # spec.c:183:0
	movl	-24(%rbp), %eax
.LBB4_13:                               # %return
	movl	%eax, -4(%rbp)
	.loc	1 184 0                 # spec.c:184:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB4_14:                               # %if.then2
	.loc	1 168 0                 # spec.c:168:0
.Ltmp114:
	movq	stderr(%rip), %rcx
	movl	-8(%rbp), %edx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 169 0                 # spec.c:169:0
	callq	exit
.Ltmp115:
.Ltmp116:
	.size	spec_read, .Ltmp116-spec_read
.Lfunc_end4:
	.cfi_endproc

	.globl	spec_fread
	.align	16, 0x90
	.type	spec_fread,@function
spec_fread:                             # @spec_fread
.Lfunc_begin5:
	.loc	1 185 0                 # spec.c:185:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp119:
	.cfi_def_cfa_offset 16
.Ltmp120:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp121:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -16(%rbp)
	movl	%esi, -20(%rbp)
	movl	%edx, -24(%rbp)
	.loc	1 187 0 prologue_end    # spec.c:187:0
.Ltmp122:
	movslq	dbglvl(%rip), %rax
	movl	%ecx, -28(%rbp)
	xorq	%rcx, %rcx
.Ltmp123:
	.loc	1 186 0                 # spec.c:186:0
	movl	%ecx, -32(%rbp)
	.loc	1 187 0                 # spec.c:187:0
.Ltmp124:
	cmpq	$5, %rax
	jl	.LBB5_2
# BB#1:                                 # %if.then
	movq	-16(%rbp), %rsi
	movl	-28(%rbp), %r8d
	movl	-24(%rbp), %ecx
	movl	-20(%rbp), %edx
	movabsq	$.L.str12, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp125:
.LBB5_2:                                # %if.end
	.loc	1 188 0                 # spec.c:188:0
	movslq	-28(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB5_14
.Ltmp126:
# BB#3:                                 # %if.end4
	.loc	1 192 0                 # spec.c:192:0
	movslq	-28(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movslq	spec_fd+4(,%rax,8), %rax
	cmpq	%rax, %rcx
	jl	.LBB5_7
# BB#4:                                 # %if.then8
	.loc	1 193 0                 # spec.c:193:0
.Ltmp127:
	movslq	dbglvl(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB5_6
# BB#5:                                 # %if.then10
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp128:
.LBB5_6:                                # %if.end12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB5_13
.LBB5_7:                                # %if.end13
	.loc	1 196 0                 # spec.c:196:0
.Ltmp129:
	movslq	-28(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %ecx
	movl	-20(%rbp), %edx
	movl	-24(%rbp), %esi
	imulq	%rdx, %rsi
	addq	%rcx, %rsi
	movq	$32, %rcx
	movslq	spec_fd+4(,%rax,8), %rax
	shlq	%cl, %rsi
	movq	$32, %rcx
	sarq	%cl, %rsi
	cmpq	%rax, %rsi
	jl	.LBB5_9
# BB#8:                                 # %if.then21
	.loc	1 197 0                 # spec.c:197:0
.Ltmp130:
	movslq	-28(%rbp), %rax
	leaq	(%rax,%rax,2), %rcx
	movl	spec_fd+4(,%rcx,8), %eax
	movl	spec_fd+8(,%rcx,8), %ecx
	subq	%rcx, %rax
	movq	$32, %rcx
	shlq	%cl, %rax
	movq	$32, %rcx
	movslq	-20(%rbp), %rsi
	sarq	%cl, %rax
	cqto
	idivq	%rsi
	jmp	.LBB5_10
.Ltmp131:
.LBB5_9:                                # %if.else
	.loc	1 199 0                 # spec.c:199:0
	movl	-24(%rbp), %eax
.LBB5_10:                               # %if.end28
	movl	%eax, -32(%rbp)
.Ltmp132:
	.loc	1 201 0                 # spec.c:201:0
	movslq	-28(%rbp), %rax
	movq	-16(%rbp), %rdi
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movq	spec_fd+16(,%rax,8), %rsi
	movslq	-32(%rbp), %rdx
	addq	%rcx, %rsi
	callq	memcpy
	.loc	1 202 0                 # spec.c:202:0
	movslq	-28(%rbp), %rax
	movl	-32(%rbp), %ecx
	movl	-20(%rbp), %edx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %esi
	.loc	1 203 0                 # spec.c:203:0
.Ltmp133:
	movslq	dbglvl(%rip), %rdi
.Ltmp134:
	.loc	1 202 0                 # spec.c:202:0
	imulq	%rcx, %rdx
	addq	%rdx, %rsi
	movl	%esi, spec_fd+8(,%rax,8)
	.loc	1 203 0                 # spec.c:203:0
.Ltmp135:
	cmpq	$5, %rdi
	jl	.LBB5_12
# BB#11:                                # %if.then44
	movl	-32(%rbp), %eax
	movl	-20(%rbp), %esi
	imulq	%rax, %rsi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp136:
.LBB5_12:                               # %if.end47
	.loc	1 204 0                 # spec.c:204:0
	movl	-32(%rbp), %eax
.LBB5_13:                               # %return
	movl	%eax, -4(%rbp)
	.loc	1 205 0                 # spec.c:205:0
	movl	-4(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB5_14:                               # %if.then2
	.loc	1 189 0                 # spec.c:189:0
.Ltmp137:
	movq	stderr(%rip), %rcx
	movl	-28(%rbp), %edx
	movabsq	$.L.str13, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 190 0                 # spec.c:190:0
	callq	exit
.Ltmp138:
.Ltmp139:
	.size	spec_fread, .Ltmp139-spec_fread
.Lfunc_end5:
	.cfi_endproc

	.globl	spec_getc
	.align	16, 0x90
	.type	spec_getc,@function
spec_getc:                              # @spec_getc
.Lfunc_begin6:
	.loc	1 206 0                 # spec.c:206:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp142:
	.cfi_def_cfa_offset 16
.Ltmp143:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp144:
	.cfi_def_cfa_register %rbp
	.loc	1 208 0 prologue_end    # spec.c:208:0
.Ltmp145:
	subq	$16, %rsp
	movslq	dbglvl(%rip), %rax
	movl	%edi, -8(%rbp)
	xorq	%rcx, %rcx
.Ltmp146:
	.loc	1 207 0                 # spec.c:207:0
	movl	%ecx, -12(%rbp)
	.loc	1 208 0                 # spec.c:208:0
.Ltmp147:
	cmpq	$5, %rax
	jl	.LBB6_2
# BB#1:                                 # %if.then
	movl	-8(%rbp), %esi
	movabsq	$.L.str14, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp148:
.LBB6_2:                                # %if.end
	.loc	1 209 0                 # spec.c:209:0
	movslq	-8(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB6_11
.Ltmp149:
# BB#3:                                 # %if.end4
	.loc	1 213 0                 # spec.c:213:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movslq	spec_fd+4(,%rax,8), %rax
	cmpq	%rax, %rcx
	jl	.LBB6_7
# BB#4:                                 # %if.then8
	.loc	1 214 0                 # spec.c:214:0
.Ltmp150:
	movslq	dbglvl(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB6_6
# BB#5:                                 # %if.then10
	movabsq	$.L.str10, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp151:
.LBB6_6:                                # %if.end12
	movabsq	$4294967295, %rax       # imm = 0xFFFFFFFF
	jmp	.LBB6_10
.LBB6_7:                                # %if.end13
	.loc	1 217 0                 # spec.c:217:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movslq	-8(%rbp), %rdx
	leaq	1(%rcx), %rsi
	movl	%esi, spec_fd+8(,%rax,8)
	leaq	(%rdx,%rdx,2), %rax
	movq	spec_fd+16(,%rax,8), %rax
	movzbl	(%rax,%rcx), %eax
	.loc	1 218 0                 # spec.c:218:0
.Ltmp152:
	movslq	dbglvl(%rip), %rcx
.Ltmp153:
	.loc	1 217 0                 # spec.c:217:0
	movl	%eax, -12(%rbp)
	.loc	1 218 0                 # spec.c:218:0
.Ltmp154:
	cmpq	$5, %rcx
	jl	.LBB6_9
# BB#8:                                 # %if.then23
	movl	-12(%rbp), %esi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp155:
.LBB6_9:                                # %if.end25
	.loc	1 219 0                 # spec.c:219:0
	movl	-12(%rbp), %eax
.LBB6_10:                               # %return
	movl	%eax, -4(%rbp)
	.loc	1 220 0                 # spec.c:220:0
	movl	-4(%rbp), %eax
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB6_11:                               # %if.then2
	.loc	1 210 0                 # spec.c:210:0
.Ltmp156:
	movq	stderr(%rip), %rcx
	movl	-8(%rbp), %edx
	movabsq	$.L.str9, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 211 0                 # spec.c:211:0
	callq	exit
.Ltmp157:
.Ltmp158:
	.size	spec_getc, .Ltmp158-spec_getc
.Lfunc_end6:
	.cfi_endproc

	.globl	spec_ungetc
	.align	16, 0x90
	.type	spec_ungetc,@function
spec_ungetc:                            # @spec_ungetc
.Lfunc_begin7:
	.loc	1 221 0                 # spec.c:221:0
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
	subq	$16, %rsp
	movb	%dil, -1(%rbp)
	.loc	1 223 0 prologue_end    # spec.c:223:0
.Ltmp164:
	movslq	dbglvl(%rip), %rax
	movl	%esi, -8(%rbp)
	xorq	%rcx, %rcx
.Ltmp165:
	.loc	1 222 0                 # spec.c:222:0
	movl	%ecx, -12(%rbp)
	.loc	1 223 0                 # spec.c:223:0
.Ltmp166:
	cmpq	$5, %rax
	jl	.LBB7_2
# BB#1:                                 # %if.then
	movl	-8(%rbp), %esi
	movabsq	$.L.str15, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp167:
.LBB7_2:                                # %if.end
	.loc	1 224 0                 # spec.c:224:0
	movslq	-8(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB7_3
.Ltmp168:
# BB#5:                                 # %if.end4
	.loc	1 228 0                 # spec.c:228:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rax
	testq	%rax, %rax
	jle	.LBB7_6
.Ltmp169:
# BB#7:                                 # %if.end11
	.loc	1 232 0                 # spec.c:232:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %edx
	decq	%rdx
	movslq	-8(%rbp), %rsi
	movl	%edx, spec_fd+8(,%rax,8)
	movq	$32, %rcx
	shlq	%cl, %rdx
	leaq	(%rsi,%rsi,2), %rax
	movq	spec_fd+16(,%rax,8), %rax
	movq	$32, %rcx
	sarq	%cl, %rdx
	movzbl	(%rax,%rdx), %eax
	movzbl	-1(%rbp), %ecx
	cmpq	%rcx, %rax
	jne	.LBB7_11
.Ltmp170:
# BB#8:                                 # %if.end24
	.loc	1 236 0                 # spec.c:236:0
	movslq	dbglvl(%rip), %rax
	cmpq	$5, %rax
	jl	.LBB7_10
# BB#9:                                 # %if.then27
	movl	-12(%rbp), %esi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp171:
.LBB7_10:                               # %if.end29
	.loc	1 237 0                 # spec.c:237:0
	movzbl	-1(%rbp), %eax
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB7_3:                                # %if.then2
	.loc	1 225 0                 # spec.c:225:0
.Ltmp172:
	movq	stderr(%rip), %rcx
	movl	-8(%rbp), %edx
	movabsq	$.L.str9, %rsi
	jmp	.LBB7_4
.Ltmp173:
.LBB7_6:                                # %if.then6
	.loc	1 229 0                 # spec.c:229:0
	movslq	-8(%rbp), %rax
	movq	stderr(%rip), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+8(,%rax,8), %edx
	movabsq	$.L.str16, %rsi
.Ltmp174:
.LBB7_4:                                # %if.then2
	xorq	%rax, %rax
	.loc	1 225 0                 # spec.c:225:0
.Ltmp175:
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 226 0                 # spec.c:226:0
	callq	exit
.Ltmp176:
.LBB7_11:                               # %if.then22
	.loc	1 233 0                 # spec.c:233:0
	movq	stderr(%rip), %rcx
	movabsq	$.L.str17, %rdx
	xorq	%rax, %rax
	movq	%rcx, %rdi
	movq	%rdx, %rsi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 234 0                 # spec.c:234:0
	callq	exit
.Ltmp177:
.Ltmp178:
	.size	spec_ungetc, .Ltmp178-spec_ungetc
.Lfunc_end7:
	.cfi_endproc

	.globl	spec_rewind
	.align	16, 0x90
	.type	spec_rewind,@function
spec_rewind:                            # @spec_rewind
.Lfunc_begin8:
	.loc	1 239 0                 # spec.c:239:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp181:
	.cfi_def_cfa_offset 16
.Ltmp182:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp183:
	.cfi_def_cfa_register %rbp
	movl	%edi, -4(%rbp)
	.loc	1 240 0 prologue_end    # spec.c:240:0
.Ltmp184:
	movslq	-4(%rbp), %rax
	leaq	(%rax,%rax,2), %rcx
	xorq	%rdx, %rdx
	xorq	%rax, %rax
	movl	%edx, spec_fd+8(,%rcx,8)
	.loc	1 241 0                 # spec.c:241:0
	popq	%rbp
	retq
.Ltmp185:
.Ltmp186:
	.size	spec_rewind, .Ltmp186-spec_rewind
.Lfunc_end8:
	.cfi_endproc

	.globl	spec_reset
	.align	16, 0x90
	.type	spec_reset,@function
spec_reset:                             # @spec_reset
.Lfunc_begin9:
	.loc	1 243 0                 # spec.c:243:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp190:
	.cfi_def_cfa_offset 16
.Ltmp191:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp192:
	.cfi_def_cfa_register %rbp
	pushq	%rbx
	pushq	%rax
.Ltmp193:
	.cfi_offset %rbx, -24
	movl	%edi, -12(%rbp)
	.loc	1 244 0 prologue_end    # spec.c:244:0
.Ltmp194:
	movslq	-12(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movq	spec_fd+16(,%rax,8), %rdi
	movslq	spec_fd+4(,%rax,8), %rdx
	xorq	%rsi, %rsi
	xorq	%rbx, %rbx
	callq	memset
	.loc	1 245 0                 # spec.c:245:0
	movslq	-12(%rbp), %rax
	movslq	-12(%rbp), %rcx
	leaq	(%rax,%rax,2), %rax
	movl	%ebx, spec_fd+4(,%rax,8)
	leaq	(%rcx,%rcx,2), %rcx
	xorq	%rax, %rax
	movl	%ebx, spec_fd+8(,%rcx,8)
	.loc	1 246 0                 # spec.c:246:0
	addq	$8, %rsp
	popq	%rbx
	popq	%rbp
	retq
.Ltmp195:
.Ltmp196:
	.size	spec_reset, .Ltmp196-spec_reset
.Lfunc_end9:
	.cfi_endproc

	.globl	spec_write
	.align	16, 0x90
	.type	spec_write,@function
spec_write:                             # @spec_write
.Lfunc_begin10:
	.loc	1 249 0                 # spec.c:249:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp199:
	.cfi_def_cfa_offset 16
.Ltmp200:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp201:
	.cfi_def_cfa_register %rbp
	.loc	1 250 0 prologue_end    # spec.c:250:0
.Ltmp202:
	subq	$32, %rsp
	movslq	dbglvl(%rip), %rax
	movl	%edi, -4(%rbp)
	movq	%rsi, -16(%rbp)
	movl	%edx, -20(%rbp)
	cmpq	$5, %rax
	jl	.LBB10_2
# BB#1:                                 # %if.then
	movq	-16(%rbp), %rdx
	movl	-20(%rbp), %ecx
	movl	-4(%rbp), %esi
	movabsq	$.L.str18, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp203:
.LBB10_2:                               # %if.end
	.loc	1 251 0                 # spec.c:251:0
	movslq	-4(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB10_6
.Ltmp204:
# BB#3:                                 # %if.end4
	.loc	1 255 0                 # spec.c:255:0
	movslq	-4(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movq	spec_fd+16(,%rax,8), %rdi
	movq	-16(%rbp), %rsi
	movslq	-20(%rbp), %rdx
	addq	%rcx, %rdi
	callq	memcpy
	.loc	1 256 0                 # spec.c:256:0
	movslq	-4(%rbp), %rax
	movl	-20(%rbp), %ecx
	leaq	(%rax,%rax,2), %rax
	movl	spec_fd+4(,%rax,8), %edx
	.loc	1 257 0                 # spec.c:257:0
	movslq	-4(%rbp), %rsi
	.loc	1 256 0                 # spec.c:256:0
	addq	%rcx, %rdx
	movl	%edx, spec_fd+4(,%rax,8)
	.loc	1 257 0                 # spec.c:257:0
	movl	-20(%rbp), %eax
	leaq	(%rsi,%rsi,2), %rcx
	movl	spec_fd+8(,%rcx,8), %edx
	.loc	1 258 0                 # spec.c:258:0
.Ltmp205:
	movslq	dbglvl(%rip), %rsi
.Ltmp206:
	.loc	1 257 0                 # spec.c:257:0
	addq	%rax, %rdx
	movl	%edx, spec_fd+8(,%rcx,8)
	.loc	1 258 0                 # spec.c:258:0
.Ltmp207:
	cmpq	$5, %rsi
	jl	.LBB10_5
# BB#4:                                 # %if.then18
	movl	-20(%rbp), %esi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp208:
.LBB10_5:                               # %if.end20
	.loc	1 259 0                 # spec.c:259:0
	movl	-20(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB10_6:                               # %if.then2
	.loc	1 252 0                 # spec.c:252:0
.Ltmp209:
	movq	stderr(%rip), %rcx
	movl	-4(%rbp), %edx
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 253 0                 # spec.c:253:0
	callq	exit
.Ltmp210:
.Ltmp211:
	.size	spec_write, .Ltmp211-spec_write
.Lfunc_end10:
	.cfi_endproc

	.globl	spec_fwrite
	.align	16, 0x90
	.type	spec_fwrite,@function
spec_fwrite:                            # @spec_fwrite
.Lfunc_begin11:
	.loc	1 261 0                 # spec.c:261:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp214:
	.cfi_def_cfa_offset 16
.Ltmp215:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp216:
	.cfi_def_cfa_register %rbp
	subq	$32, %rsp
	movq	%rdi, -8(%rbp)
	.loc	1 262 0 prologue_end    # spec.c:262:0
.Ltmp217:
	movslq	dbglvl(%rip), %rax
	movl	%esi, -12(%rbp)
	movl	%edx, -16(%rbp)
	movl	%ecx, -20(%rbp)
	cmpq	$5, %rax
	jl	.LBB11_2
# BB#1:                                 # %if.then
	movq	-8(%rbp), %rsi
	movl	-20(%rbp), %r8d
	movl	-16(%rbp), %ecx
	movl	-12(%rbp), %edx
	movabsq	$.L.str20, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp218:
.LBB11_2:                               # %if.end
	.loc	1 263 0                 # spec.c:263:0
	movslq	-20(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB11_6
.Ltmp219:
# BB#3:                                 # %if.end4
	.loc	1 267 0                 # spec.c:267:0
	movslq	-20(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movq	spec_fd+16(,%rax,8), %rdi
	addq	%rcx, %rdi
	movl	-12(%rbp), %eax
	movl	-16(%rbp), %edx
	movq	-8(%rbp), %rsi
	imulq	%rax, %rdx
	movq	$32, %rcx
	shlq	%cl, %rdx
	movq	$32, %rcx
	sarq	%cl, %rdx
	callq	memcpy
	.loc	1 268 0                 # spec.c:268:0
	movl	-12(%rbp), %eax
	movslq	-20(%rbp), %rcx
	movl	-16(%rbp), %edx
	imulq	%rax, %rdx
	leaq	(%rcx,%rcx,2), %rax
	movl	spec_fd+4(,%rax,8), %ecx
	addq	%rdx, %rcx
	.loc	1 269 0                 # spec.c:269:0
	movslq	-20(%rbp), %rdx
	.loc	1 268 0                 # spec.c:268:0
	movl	%ecx, spec_fd+4(,%rax,8)
	.loc	1 269 0                 # spec.c:269:0
	movl	-12(%rbp), %eax
	movl	-16(%rbp), %ecx
	leaq	(%rdx,%rdx,2), %rdx
	movl	spec_fd+8(,%rdx,8), %esi
	.loc	1 270 0                 # spec.c:270:0
.Ltmp220:
	movslq	dbglvl(%rip), %rdi
.Ltmp221:
	.loc	1 269 0                 # spec.c:269:0
	imulq	%rax, %rcx
	addq	%rcx, %rsi
	movl	%esi, spec_fd+8(,%rdx,8)
	.loc	1 270 0                 # spec.c:270:0
.Ltmp222:
	cmpq	$5, %rdi
	jl	.LBB11_5
# BB#4:                                 # %if.then20
	movl	-16(%rbp), %esi
	movabsq	$.L.str11, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp223:
.LBB11_5:                               # %if.end22
	.loc	1 271 0                 # spec.c:271:0
	movl	-16(%rbp), %eax
	addq	$32, %rsp
	popq	%rbp
	retq
.LBB11_6:                               # %if.then2
	.loc	1 264 0                 # spec.c:264:0
.Ltmp224:
	movq	stderr(%rip), %rcx
	movl	-20(%rbp), %edx
	movabsq	$.L.str21, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 265 0                 # spec.c:265:0
	callq	exit
.Ltmp225:
.Ltmp226:
	.size	spec_fwrite, .Ltmp226-spec_fwrite
.Lfunc_end11:
	.cfi_endproc

	.globl	spec_putc
	.align	16, 0x90
	.type	spec_putc,@function
spec_putc:                              # @spec_putc
.Lfunc_begin12:
	.loc	1 273 0                 # spec.c:273:0
	.cfi_startproc
# BB#0:                                 # %entry
	pushq	%rbp
.Ltmp229:
	.cfi_def_cfa_offset 16
.Ltmp230:
	.cfi_offset %rbp, -16
	movq	%rsp, %rbp
.Ltmp231:
	.cfi_def_cfa_register %rbp
	.loc	1 274 0 prologue_end    # spec.c:274:0
.Ltmp232:
	subq	$16, %rsp
	movslq	dbglvl(%rip), %rax
	movb	%dil, -1(%rbp)
	movl	%esi, -8(%rbp)
	cmpq	$5, %rax
	jl	.LBB12_2
# BB#1:                                 # %if.then
	movzbl	-1(%rbp), %esi
	movl	-8(%rbp), %edx
	movabsq	$.L.str22, %rdi
	xorq	%rax, %rax
	callq	printf
.Ltmp233:
.LBB12_2:                               # %if.end
	.loc	1 275 0                 # spec.c:275:0
	movslq	-8(%rbp), %rax
	cmpq	$4, %rax
	jge	.LBB12_4
.Ltmp234:
# BB#3:                                 # %if.end5
	.loc	1 279 0                 # spec.c:279:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rax
	movslq	spec_fd+8(,%rax,8), %rcx
	movslq	-8(%rbp), %rdx
	leaq	1(%rcx), %rsi
	movl	%esi, spec_fd+8(,%rax,8)
	movzbl	-1(%rbp), %eax
	leaq	(%rdx,%rdx,2), %rdx
	movq	spec_fd+16(,%rdx,8), %rdx
	movb	%al, (%rdx,%rcx)
	.loc	1 280 0                 # spec.c:280:0
	movslq	-8(%rbp), %rax
	leaq	(%rax,%rax,2), %rcx
	movl	spec_fd+4(,%rcx,8), %edx
	.loc	1 281 0                 # spec.c:281:0
	movzbl	-1(%rbp), %eax
	.loc	1 280 0                 # spec.c:280:0
	incq	%rdx
	movl	%edx, spec_fd+4(,%rcx,8)
	.loc	1 281 0                 # spec.c:281:0
	addq	$16, %rsp
	popq	%rbp
	retq
.LBB12_4:                               # %if.then3
	.loc	1 276 0                 # spec.c:276:0
.Ltmp235:
	movq	stderr(%rip), %rcx
	movl	-8(%rbp), %edx
	movabsq	$.L.str19, %rsi
	xorq	%rax, %rax
	movq	%rcx, %rdi
	callq	fprintf
	xorq	%rdi, %rdi
	.loc	1 277 0                 # spec.c:277:0
	callq	exit
.Ltmp236:
.Ltmp237:
	.size	spec_putc, .Ltmp237-spec_putc
.Lfunc_end12:
	.cfi_endproc

	.type	dbglvl,@object          # @dbglvl
	.data
	.globl	dbglvl
	.align	4
dbglvl:
	.long	4                       # 0x4
	.size	dbglvl, 4

	.type	seedi,@object           # @seedi
	.comm	seedi,8,8
	.type	.L.str,@object          # @.str
	.section	.rodata.str1.1,"aMS",@progbits,1
.L.str:
	.asciz	"spec_init\n"
	.size	.L.str, 11

	.type	spec_fd,@object         # @spec_fd
	.comm	spec_fd,72,16
	.type	.L.str1,@object         # @.str1
.L.str1:
	.asciz	"spec_init: Error mallocing memory!\n"
	.size	.L.str1, 36

	.type	.L.str2,@object         # @.str2
.L.str2:
	.asciz	"Creating Chunks\n"
	.size	.L.str2, 17

	.type	.L.str3,@object         # @.str3
.L.str3:
	.asciz	"Creating Chunk %d\n"
	.size	.L.str3, 19

	.type	.L.str4,@object         # @.str4
.L.str4:
	.asciz	"Filling input file\n"
	.size	.L.str4, 20

	.type	.L.str5,@object         # @.str5
.L.str5:
	.asciz	"Can't open file %s: %s\n"
	.size	.L.str5, 24

	.type	.L.str6,@object         # @.str6
.L.str6:
	.asciz	"Error reading from %s: %s\n"
	.size	.L.str6, 27

	.type	.L.str7,@object         # @.str7
.L.str7:
	.asciz	"Duplicating %d bytes\n"
	.size	.L.str7, 22

	.type	.L.str8,@object         # @.str8
.L.str8:
	.asciz	"spec_read: %d, %p, %d = "
	.size	.L.str8, 25

	.type	.L.str9,@object         # @.str9
.L.str9:
	.asciz	"spec_read: fd=%d, > MAX_SPEC_FD!\n"
	.size	.L.str9, 34

	.type	.L.str10,@object        # @.str10
.L.str10:
	.asciz	"EOF\n"
	.size	.L.str10, 5

	.type	.L.str11,@object        # @.str11
.L.str11:
	.asciz	"%d\n"
	.size	.L.str11, 4

	.type	.L.str12,@object        # @.str12
.L.str12:
	.asciz	"spec_fread: %p, (%d x %d) fd %d ="
	.size	.L.str12, 34

	.type	.L.str13,@object        # @.str13
.L.str13:
	.asciz	"spec_fread: fd=%d, > MAX_SPEC_FD!\n"
	.size	.L.str13, 35

	.type	.L.str14,@object        # @.str14
.L.str14:
	.asciz	"spec_getc: %d = "
	.size	.L.str14, 17

	.type	.L.str15,@object        # @.str15
.L.str15:
	.asciz	"spec_ungetc: %d = "
	.size	.L.str15, 19

	.type	.L.str16,@object        # @.str16
.L.str16:
	.asciz	"spec_ungetc: pos %d <= 0\n"
	.size	.L.str16, 26

	.type	.L.str17,@object        # @.str17
.L.str17:
	.asciz	"spec_ungetc: can't unget something that wasn't what was in the buffer!\n"
	.size	.L.str17, 72

	.type	.L.str18,@object        # @.str18
.L.str18:
	.asciz	"spec_write: %d, %p, %d = "
	.size	.L.str18, 26

	.type	.L.str19,@object        # @.str19
.L.str19:
	.asciz	"spec_write: fd=%d, > MAX_SPEC_FD!\n"
	.size	.L.str19, 35

	.type	.L.str20,@object        # @.str20
.L.str20:
	.asciz	"spec_fwrite: %p, %d, %d, %d = "
	.size	.L.str20, 31

	.type	.L.str21,@object        # @.str21
.L.str21:
	.asciz	"spec_fwrite: fd=%d, > MAX_SPEC_FD!\n"
	.size	.L.str21, 36

	.type	.L.str22,@object        # @.str22
.L.str22:
	.asciz	"spec_putc: %d, %d = "
	.size	.L.str22, 21

	.data
.Ldebug_end1:
	.text
.Ldebug_end2:
	.section	.debug_str,"MS",@progbits,1
.Linfo_string0:
	.asciz	"clang version 3.5 (trunk)"
.Linfo_string1:
	.asciz	"spec.c"
.Linfo_string2:
	.asciz	"/var/services/homes/akale/llvm523/llvm_backend/mytest/bzip2"
.Linfo_string3:
	.asciz	"dbglvl"
.Linfo_string4:
	.asciz	"int"
.Linfo_string5:
	.asciz	"spec_fd"
.Linfo_string6:
	.asciz	"limit"
.Linfo_string7:
	.asciz	"len"
.Linfo_string8:
	.asciz	"pos"
.Linfo_string9:
	.asciz	"buf"
.Linfo_string10:
	.asciz	"unsigned char"
.Linfo_string11:
	.asciz	"spec_fd_t"
.Linfo_string12:
	.asciz	"seedi"
.Linfo_string13:
	.asciz	"long int"
.Linfo_string14:
	.asciz	"ran"
.Linfo_string15:
	.asciz	"double"
.Linfo_string16:
	.asciz	"spec_init"
.Linfo_string17:
	.asciz	"spec_random_load"
.Linfo_string18:
	.asciz	"spec_load"
.Linfo_string19:
	.asciz	"spec_read"
.Linfo_string20:
	.asciz	"spec_fread"
.Linfo_string21:
	.asciz	"spec_getc"
.Linfo_string22:
	.asciz	"spec_ungetc"
.Linfo_string23:
	.asciz	"spec_rewind"
.Linfo_string24:
	.asciz	"spec_reset"
.Linfo_string25:
	.asciz	"spec_write"
.Linfo_string26:
	.asciz	"spec_fwrite"
.Linfo_string27:
	.asciz	"spec_putc"
.Linfo_string28:
	.asciz	"lo"
.Linfo_string29:
	.asciz	"hi"
.Linfo_string30:
	.asciz	"test"
.Linfo_string31:
	.asciz	"i"
.Linfo_string32:
	.asciz	"j"
.Linfo_string33:
	.asciz	"fd"
.Linfo_string34:
	.asciz	"random_text"
.Linfo_string35:
	.asciz	"char"
.Linfo_string36:
	.asciz	"num"
.Linfo_string37:
	.asciz	"filename"
.Linfo_string38:
	.asciz	"size"
.Linfo_string39:
	.asciz	"rc"
.Linfo_string40:
	.asciz	"tmp"
.Linfo_string41:
	.asciz	"ch"
	.section	.debug_info,"",@progbits
.L.debug_info_begin0:
	.long	1264                    # Length of Unit
	.short	4                       # DWARF version number
	.long	.Lsection_abbrev        # Offset Into Abbrev. Section
	.byte	8                       # Address Size (in bytes)
	.byte	1                       # Abbrev [1] 0xb:0x4e9 DW_TAG_compile_unit
	.long	.Linfo_string0          # DW_AT_producer
	.short	12                      # DW_AT_language
	.long	.Linfo_string1          # DW_AT_name
	.long	.Lsection_line          # DW_AT_stmt_list
	.long	.Linfo_string2          # DW_AT_comp_dir
	.quad	0                       # DW_AT_low_pc
	.byte	2                       # Abbrev [2] 0x26:0x15 DW_TAG_variable
	.long	.Linfo_string3          # DW_AT_name
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.byte	28                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	dbglvl
	.byte	3                       # Abbrev [3] 0x3b:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	4                       # DW_AT_byte_size
	.byte	2                       # Abbrev [2] 0x42:0x15 DW_TAG_variable
	.long	.Linfo_string5          # DW_AT_name
	.long	87                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.byte	50                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	spec_fd
	.byte	4                       # Abbrev [4] 0x57:0xc DW_TAG_array_type
	.long	99                      # DW_AT_type
	.byte	5                       # Abbrev [5] 0x5c:0x6 DW_TAG_subrange_type
	.long	172                     # DW_AT_type
	.byte	2                       # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	6                       # Abbrev [6] 0x63:0x3d DW_TAG_structure_type
	.long	.Linfo_string11         # DW_AT_name
	.byte	24                      # DW_AT_byte_size
	.byte	1                       # DW_AT_decl_file
	.byte	45                      # DW_AT_decl_line
	.byte	7                       # Abbrev [7] 0x6b:0xd DW_TAG_member
	.long	.Linfo_string6          # DW_AT_name
	.long	59                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	46                      # DW_AT_decl_line
	.byte	0                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	7                       # Abbrev [7] 0x78:0xd DW_TAG_member
	.long	.Linfo_string7          # DW_AT_name
	.long	59                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	47                      # DW_AT_decl_line
	.byte	4                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	7                       # Abbrev [7] 0x85:0xd DW_TAG_member
	.long	.Linfo_string8          # DW_AT_name
	.long	59                      # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	48                      # DW_AT_decl_line
	.byte	8                       # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	7                       # Abbrev [7] 0x92:0xd DW_TAG_member
	.long	.Linfo_string9          # DW_AT_name
	.long	160                     # DW_AT_type
	.byte	1                       # DW_AT_decl_file
	.byte	49                      # DW_AT_decl_line
	.byte	16                      # DW_AT_data_member_location
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.byte	0                       # End Of Children Mark
	.byte	8                       # Abbrev [8] 0xa0:0x5 DW_TAG_pointer_type
	.long	165                     # DW_AT_type
	.byte	3                       # Abbrev [3] 0xa5:0x7 DW_TAG_base_type
	.long	.Linfo_string10         # DW_AT_name
	.byte	8                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	9                       # Abbrev [9] 0xac:0x7 DW_TAG_base_type
	.long	.Linfo_string4          # DW_AT_name
	.byte	4                       # DW_AT_byte_size
	.byte	5                       # DW_AT_encoding
	.byte	2                       # Abbrev [2] 0xb3:0x15 DW_TAG_variable
	.long	.Linfo_string12         # DW_AT_name
	.long	200                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_decl_file
	.byte	52                      # DW_AT_decl_line
	.byte	9                       # DW_AT_location
	.byte	3
	.quad	seedi
	.byte	3                       # Abbrev [3] 0xc8:0x7 DW_TAG_base_type
	.long	.Linfo_string13         # DW_AT_name
	.byte	5                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	10                      # Abbrev [10] 0xcf:0x49 DW_TAG_subprogram
	.long	.Linfo_string14         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	53                      # DW_AT_decl_line
	.long	280                     # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin0           # DW_AT_low_pc
	.quad	.Lfunc_end0             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	11                      # Abbrev [11] 0xed:0xe DW_TAG_variable
	.long	.Linfo_string28         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	64                      # DW_AT_decl_line
	.long	200                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0xfb:0xe DW_TAG_variable
	.long	.Linfo_string29         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	65                      # DW_AT_decl_line
	.long	200                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	11                      # Abbrev [11] 0x109:0xe DW_TAG_variable
	.long	.Linfo_string30         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	66                      # DW_AT_decl_line
	.long	200                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x118:0x7 DW_TAG_base_type
	.long	.Linfo_string15         # DW_AT_name
	.byte	4                       # DW_AT_encoding
	.byte	8                       # DW_AT_byte_size
	.byte	10                      # Abbrev [10] 0x11f:0x55 DW_TAG_subprogram
	.long	.Linfo_string16         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	80                      # DW_AT_decl_line
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin1           # DW_AT_low_pc
	.quad	.Lfunc_end1             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	11                      # Abbrev [11] 0x13d:0xe DW_TAG_variable
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	11                      # Abbrev [11] 0x14b:0xe DW_TAG_variable
	.long	.Linfo_string32         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	81                      # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	12                      # Abbrev [12] 0x159:0x1a DW_TAG_lexical_block
	.long	.Ldebug_ranges1         # DW_AT_ranges
	.byte	12                      # Abbrev [12] 0x15e:0x14 DW_TAG_lexical_block
	.long	.Ldebug_ranges0         # DW_AT_ranges
	.byte	11                      # Abbrev [11] 0x163:0xe DW_TAG_variable
	.long	.Linfo_string6          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	88                      # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x174:0x5a DW_TAG_subprogram
	.long	.Linfo_string17         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	103                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin2           # DW_AT_low_pc
	.quad	.Lfunc_end2             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x192:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	103                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	11                      # Abbrev [11] 0x1a0:0xe DW_TAG_variable
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	11                      # Abbrev [11] 0x1ae:0xe DW_TAG_variable
	.long	.Linfo_string32         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	110                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	11                      # Abbrev [11] 0x1bc:0x11 DW_TAG_variable
	.long	.Linfo_string34         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	111                     # DW_AT_decl_line
	.long	1234                    # DW_AT_type
	.byte	5                       # DW_AT_location
	.byte	145
	.ascii	"\320\377\377}"
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x1ce:0x87 DW_TAG_subprogram
	.long	.Linfo_string18         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	132                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin3           # DW_AT_low_pc
	.quad	.Lfunc_end3             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x1ec:0xe DW_TAG_formal_parameter
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	132                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	14                      # Abbrev [14] 0x1fa:0xe DW_TAG_formal_parameter
	.long	.Linfo_string37         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	132                     # DW_AT_decl_line
	.long	1262                    # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	14                      # Abbrev [14] 0x208:0xe DW_TAG_formal_parameter
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	132                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	92
	.byte	11                      # Abbrev [11] 0x216:0xe DW_TAG_variable
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	88
	.byte	11                      # Abbrev [11] 0x224:0xe DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	84
	.byte	11                      # Abbrev [11] 0x232:0xe DW_TAG_variable
	.long	.Linfo_string31         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	134                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	80
	.byte	12                      # Abbrev [12] 0x240:0x14 DW_TAG_lexical_block
	.long	.Ldebug_ranges2         # DW_AT_ranges
	.byte	11                      # Abbrev [11] 0x245:0xe DW_TAG_variable
	.long	.Linfo_string40         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	155                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	76
	.byte	0                       # End Of Children Mark
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x255:0x57 DW_TAG_subprogram
	.long	.Linfo_string19         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin4           # DW_AT_low_pc
	.quad	.Lfunc_end4             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x273:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	14                      # Abbrev [14] 0x281:0xe DW_TAG_formal_parameter
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	160                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x28f:0xe DW_TAG_formal_parameter
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	164                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	11                      # Abbrev [11] 0x29d:0xe DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	165                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x2ac:0x65 DW_TAG_subprogram
	.long	.Linfo_string20         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	185                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin5           # DW_AT_low_pc
	.quad	.Lfunc_end5             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x2ca:0xe DW_TAG_formal_parameter
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	185                     # DW_AT_decl_line
	.long	160                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x2d8:0xe DW_TAG_formal_parameter
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	185                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	14                      # Abbrev [14] 0x2e6:0xe DW_TAG_formal_parameter
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	185                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	104
	.byte	14                      # Abbrev [14] 0x2f4:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	185                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	100
	.byte	11                      # Abbrev [11] 0x302:0xe DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	186                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	96
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x311:0x3b DW_TAG_subprogram
	.long	.Linfo_string21         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin6           # DW_AT_low_pc
	.quad	.Lfunc_end6             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x32f:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	206                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0x33d:0xe DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	207                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x34c:0x49 DW_TAG_subprogram
	.long	.Linfo_string22         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin7           # DW_AT_low_pc
	.quad	.Lfunc_end7             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x36a:0xe DW_TAG_formal_parameter
	.long	.Linfo_string41         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.long	165                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.byte	14                      # Abbrev [14] 0x378:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	221                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	11                      # Abbrev [11] 0x386:0xe DW_TAG_variable
	.long	.Linfo_string39         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	222                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x395:0x2d DW_TAG_subprogram
	.long	.Linfo_string23         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	239                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin8           # DW_AT_low_pc
	.quad	.Lfunc_end8             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x3b3:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	239                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x3c2:0x2d DW_TAG_subprogram
	.long	.Linfo_string24         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	243                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin9           # DW_AT_low_pc
	.quad	.Lfunc_end9             # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x3e0:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	243                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	0                       # End Of Children Mark
	.byte	13                      # Abbrev [13] 0x3ef:0x49 DW_TAG_subprogram
	.long	.Linfo_string25         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	249                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin10          # DW_AT_low_pc
	.quad	.Lfunc_end10            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	14                      # Abbrev [14] 0x40d:0xe DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	249                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	124
	.byte	14                      # Abbrev [14] 0x41b:0xe DW_TAG_formal_parameter
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	249                     # DW_AT_decl_line
	.long	160                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	14                      # Abbrev [14] 0x429:0xe DW_TAG_formal_parameter
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.byte	249                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x438:0x5c DW_TAG_subprogram
	.long	.Linfo_string26         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin11          # DW_AT_low_pc
	.quad	.Lfunc_end11            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x457:0xf DW_TAG_formal_parameter
	.long	.Linfo_string9          # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	160                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	16                      # Abbrev [16] 0x466:0xf DW_TAG_formal_parameter
	.long	.Linfo_string38         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	116
	.byte	16                      # Abbrev [16] 0x475:0xf DW_TAG_formal_parameter
	.long	.Linfo_string36         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	112
	.byte	16                      # Abbrev [16] 0x484:0xf DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	261                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	108
	.byte	0                       # End Of Children Mark
	.byte	15                      # Abbrev [15] 0x494:0x3e DW_TAG_subprogram
	.long	.Linfo_string27         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
                                        # DW_AT_prototyped
	.long	59                      # DW_AT_type
                                        # DW_AT_external
	.byte	1                       # DW_AT_accessibility
                                        # DW_ACCESS_public
	.quad	.Lfunc_begin12          # DW_AT_low_pc
	.quad	.Lfunc_end12            # DW_AT_high_pc
	.byte	1                       # DW_AT_frame_base
	.byte	86
	.byte	16                      # Abbrev [16] 0x4b3:0xf DW_TAG_formal_parameter
	.long	.Linfo_string41         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
	.long	165                     # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	127
	.byte	16                      # Abbrev [16] 0x4c2:0xf DW_TAG_formal_parameter
	.long	.Linfo_string33         # DW_AT_name
	.byte	1                       # DW_AT_decl_file
	.short	273                     # DW_AT_decl_line
	.long	59                      # DW_AT_type
	.byte	2                       # DW_AT_location
	.byte	145
	.byte	120
	.byte	0                       # End Of Children Mark
	.byte	4                       # Abbrev [4] 0x4d2:0x15 DW_TAG_array_type
	.long	1255                    # DW_AT_type
	.byte	5                       # Abbrev [5] 0x4d7:0x6 DW_TAG_subrange_type
	.long	172                     # DW_AT_type
	.byte	31                      # DW_AT_upper_bound
	.byte	17                      # Abbrev [17] 0x4dd:0x9 DW_TAG_subrange_type
	.long	172                     # DW_AT_type
	.long	131071                  # DW_AT_upper_bound
	.byte	0                       # End Of Children Mark
	.byte	3                       # Abbrev [3] 0x4e7:0x7 DW_TAG_base_type
	.long	.Linfo_string35         # DW_AT_name
	.byte	6                       # DW_AT_encoding
	.byte	1                       # DW_AT_byte_size
	.byte	8                       # Abbrev [8] 0x4ee:0x5 DW_TAG_pointer_type
	.long	1255                    # DW_AT_type
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
	.byte	63                      # DW_AT_external
	.byte	25                      # DW_FORM_flag_present
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	2                       # DW_AT_location
	.byte	24                      # DW_FORM_exprloc
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	3                       # Abbreviation Code
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
	.byte	4                       # Abbreviation Code
	.byte	1                       # DW_TAG_array_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	5                       # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	6                       # Abbreviation Code
	.byte	19                      # DW_TAG_structure_type
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	11                      # DW_AT_byte_size
	.byte	11                      # DW_FORM_data1
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	7                       # Abbreviation Code
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
	.byte	8                       # Abbreviation Code
	.byte	15                      # DW_TAG_pointer_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	9                       # Abbreviation Code
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
	.byte	10                      # Abbreviation Code
	.byte	46                      # DW_TAG_subprogram
	.byte	1                       # DW_CHILDREN_yes
	.byte	3                       # DW_AT_name
	.byte	14                      # DW_FORM_strp
	.byte	58                      # DW_AT_decl_file
	.byte	11                      # DW_FORM_data1
	.byte	59                      # DW_AT_decl_line
	.byte	11                      # DW_FORM_data1
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
	.byte	11                      # DW_TAG_lexical_block
	.byte	1                       # DW_CHILDREN_yes
	.byte	85                      # DW_AT_ranges
	.byte	23                      # DW_FORM_sec_offset
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	13                      # Abbreviation Code
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
	.byte	14                      # Abbreviation Code
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
	.byte	15                      # Abbreviation Code
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
	.byte	16                      # Abbreviation Code
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
	.byte	17                      # Abbreviation Code
	.byte	33                      # DW_TAG_subrange_type
	.byte	0                       # DW_CHILDREN_no
	.byte	73                      # DW_AT_type
	.byte	19                      # DW_FORM_ref4
	.byte	47                      # DW_AT_upper_bound
	.byte	6                       # DW_FORM_data4
	.byte	0                       # EOM(1)
	.byte	0                       # EOM(2)
	.byte	0                       # EOM(3)
	.section	.debug_ranges,"",@progbits
.Lgnu_ranges0:
.Ldebug_ranges0:
	.quad	.Ltmp22
	.quad	.Ltmp29
	.quad	.Ltmp30
	.quad	.Ltmp31
	.quad	0
	.quad	0
.Ldebug_ranges1:
	.quad	.Ltmp21
	.quad	.Ltmp29
	.quad	.Ltmp30
	.quad	.Ltmp31
	.quad	0
	.quad	0
.Ldebug_ranges2:
	.quad	.Ltmp79
	.quad	.Ltmp81
	.quad	.Ltmp82
	.quad	.Ltmp90
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
	.long	207                     # DIE offset
	.asciz	"ran"                   # External Name
	.long	597                     # DIE offset
	.asciz	"spec_read"             # External Name
	.long	1172                    # DIE offset
	.asciz	"spec_putc"             # External Name
	.long	462                     # DIE offset
	.asciz	"spec_load"             # External Name
	.long	179                     # DIE offset
	.asciz	"seedi"                 # External Name
	.long	684                     # DIE offset
	.asciz	"spec_fread"            # External Name
	.long	785                     # DIE offset
	.asciz	"spec_getc"             # External Name
	.long	962                     # DIE offset
	.asciz	"spec_reset"            # External Name
	.long	372                     # DIE offset
	.asciz	"spec_random_load"      # External Name
	.long	844                     # DIE offset
	.asciz	"spec_ungetc"           # External Name
	.long	917                     # DIE offset
	.asciz	"spec_rewind"           # External Name
	.long	66                      # DIE offset
	.asciz	"spec_fd"               # External Name
	.long	1007                    # DIE offset
	.asciz	"spec_write"            # External Name
	.long	38                      # DIE offset
	.asciz	"dbglvl"                # External Name
	.long	1080                    # DIE offset
	.asciz	"spec_fwrite"           # External Name
	.long	287                     # DIE offset
	.asciz	"spec_init"             # External Name
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
	.long	280                     # DIE offset
	.asciz	"double"                # External Name
	.long	99                      # DIE offset
	.asciz	"spec_fd_t"             # External Name
	.long	59                      # DIE offset
	.asciz	"int"                   # External Name
	.long	165                     # DIE offset
	.asciz	"unsigned char"         # External Name
	.long	200                     # DIE offset
	.asciz	"long int"              # External Name
	.long	1255                    # DIE offset
	.asciz	"char"                  # External Name
	.long	0                       # End Mark
.Lpubtypes_end0:

	.ident	"clang version 3.5 (trunk)"
	.section	".note.GNU-stack","",@progbits
